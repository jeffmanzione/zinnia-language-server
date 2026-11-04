import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';
import {SemanticAnalyzer} from '../semantics/analyzer';
import {parseModule} from '../semantics/parser';
import {generateSemanticTokens} from '../semantics/semantic';
import {Module} from '../semantics/statements';

// Parses [text], which must have no syntax errors.
function parse(text: string): Module {
  const result = parseModule(text);
  assert.deepEqual(result.problems, []);
  return result.module;
}

// An analyzer that knows no other modules, so tests don't load the standard
// library.
const STUB_ANALYZER = {
  searchBuiltinForId: () => undefined,
  lookupDocInfoFromModuleName: () => undefined,
  isStandardLibraryFile: () => false,
} as unknown as SemanticAnalyzer;

describe('parser', () => {
  it('parses a small program', () => {
    const module = parse([
      'import io',
      'class Point {',
      '  new(field x, field y) {}',
      '  method norm2() x * x + y * y',
      '}',
      'function origin() Point(0, 0)',
      'io.println(origin().norm2())',
      '',
    ].join('\n'));
    assert.deepEqual(
        module.statements.map(s => s.kind),
        ['ImportStat', 'ClassStat', 'FunctionStat', 'PostfixChainExpr']);
  });

  it('parses deeply nested expressions quickly', () => {
    const depth = 40;
    const text = 'x = ' + 'f('.repeat(depth) + '1' + ')'.repeat(depth) + '\n' +
        'y = ' + '('.repeat(depth) + '1' + ')'.repeat(depth) + '\n';
    const start = Date.now();
    parse(text);
    // Without memoization this took minutes; it should take milliseconds.
    assert.ok(Date.now() - start < 1000, `took ${Date.now() - start} ms`);
  });

  it('parses strings that end in an escaped backslash', () => {
    const module = parse('a = \'\\\\\'\nb = i\'it\\\'s\'\n');
    assert.equal(module.statements.length, 2);
  });

  it('parses import ... as with a module name', () => {
    const module = parse('import math as m\n');
    assert.equal(module.statements[0].kind, 'ImportAsStat');
  });

  it('parses return followed by a parenthesized expression', () => {
    parse('function f(a) {\n  return (a > 1) and (a < 5)\n}\n');
    parse('function g(a) {\n  return (a + 1) * 2\n}\n');
  });

  it('binds is more tightly than or', () => {
    const module = parse('x = a is Array or a is Tuple\n');
    const assign = module.statements[0] as any;
    assert.equal(assign.kind, 'AssignBaseExpr');
    assert.equal(assign.rhs.kind, 'OrChainExpr');
    assert.deepEqual(
        assign.rhs.exprs.map((e: any) => e.kind), ['IsExpr', 'IsExpr']);
  });

  it('parses const and the length operator', () => {
    const module = parse('const n = |items| + 1\n');
    const assign = module.statements[0] as any;
    assert.equal(assign.lhs.kind, 'IdentifierExpr');
    assert.equal(assign.lhs.constTok.text, 'const');
    assert.equal(assign.rhs.exprs[0].kind, 'LengthExpr');
  });

  it('treats def as an ordinary name', () => {
    const module = parse('def = 1\n');
    assert.equal((module.statements[0] as any).lhs.token.text, 'def');
  });

  it('ends a line comment before the new line', () => {
    // If the comment swallowed the new line, `f` and `(2)` would become a call.
    const module = parse('x = f ; comment\n(2)\n; Last line, no new line.');
    assert.deepEqual(
        module.statements.map(s => s.kind), ['AssignBaseExpr', 'ParensExpr']);
  });
});

describe('error recovery', () => {
  // Returns a summary of each statement and the rows of the problems.
  function recover(text: string): [string[], number[]] {
    const result = parseModule(text);
    const statements = result.module.statements.map((s: any) => {
      if (s.kind === 'AssignBaseExpr') {
        return s.lhs.token.text + '=';
      }
      if (s.kind === 'FunctionStat' || s.kind === 'ClassStat') {
        return s.name.token.text;
      }
      return s.kind;
    });
    return [statements, result.problems.map(p => p.row ?? -1)];
  }

  it('skips a bad statement and keeps the rest', () => {
    assert.deepEqual(
        recover('a = 1\nb = 2 ) 3\nc = 3\n'), [['a=', 'b=', 'c='], [1]]);
  });

  it('keeps a function whose body has an error', () => {
    const [statements, rows] =
        recover('function f() {\n  a = 1\n  b = = 2\n  c = 3\n}\nfunction g() 4\n');
    assert.deepEqual(statements, ['f', 'g']);
    assert.deepEqual(rows, [2]);
  });

  it('keeps a class whose method has an error', () => {
    const result = parseModule(
        'class A {\n  method m() {\n    x = )\n  }\n  method n() 1\n}\nb = 2\n');
    const cls = result.module.statements[0] as any;
    assert.equal(cls.kind, 'ClassStat');
    assert.deepEqual(cls.stats.map((m: any) => m.name.token.text), ['m', 'n']);
    assert.equal(result.module.statements.length, 2);
    assert.deepEqual(
        result.problems,
        [{message: 'Unexpected \')\'; expected an expression.', row: 2, col: 8}]);
  });

  it('closes an unclosed block at the next top-level definition', () => {
    const result =
        parseModule('function f() {\n  a = 1\nfunction g() 2\n');
    assert.deepEqual(
        result.module.statements.map((s: any) => s.name.token.text), ['f', 'g']);
    assert.deepEqual(result.problems, [{
      message: '\'{\' is never closed; expected \'}\'.',
      row: 0,
      col: 13
    }]);
  });

  it('reports an unclosed parenthesis at the end of the file', () => {
    assert.deepEqual(
        parseModule('x = 1\ny = (2\n').problems,
        [{message: 'Unexpected end of file; expected \',\' or \')\'.'}]);
    assert.deepEqual(
        parseModule('x = 1\ny = (2,\n').problems,
        [{message: 'Unexpected end of file; expected an expression.'}]);
  });

  it('reports unknown characters and unterminated strings', () => {
    assert.deepEqual(
        parseModule('a = 1\nb = 2 $$ 3\n').problems,
        [{message: 'Unexpected character \'$\'.', row: 1, col: 6}]);
    assert.deepEqual(
        parseModule('a = \'abc\nb = 2\n').problems,
        [{message: 'Unterminated string.', row: 0, col: 4}]);
  });
});

describe('semantic tokens', () => {
  it('links tokens to their identifier declarations', () => {
    const module = parse('x = 1\ny = x + 2\n');
    const [, , tokens] =
        generateSemanticTokens('/test.zn', STUB_ANALYZER, module);
    const uses = tokens.filter(t => t.text === 'x');
    assert.equal(uses.length, 2);
    assert.ok(uses[1].identifier !== undefined);
    const declaration = uses[1].identifier!.tokens[0].token;
    assert.equal(declaration.pos.rowBegin, 1);
    assert.equal(declaration.pos.columnBegin, 1);
  });

  it('marks const variables readonly', () => {
    const module = parse('const limit = 3\n');
    const [, , tokens] =
        generateSemanticTokens('/test.zn', STUB_ANALYZER, module);
    assert.ok(tokens.some(t => t.text === 'const' && t.type === 'keyword'));
    const limit = tokens.find(t => t.text === 'limit')!;
    assert.ok(limit.modifiers.includes('readonly'));
  });
});

describe('SemanticAnalyzer', () => {
  it('reports syntax errors and analyzes the rest of the file', async () => {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => []);
    const info = await analyzer.parseDocument({
      textDocument: {uri: 'file:///analyzer-test.zn'},
      text: 'x = 1\ny = = 2\nz = x\n'
    });
    assert.equal(info.problems.length, 1);
    assert.equal(info.problems[0].row, 1);
    assert.ok(info.tokens.some(t => t.text === 'z'));
    const uses = info.tokens.filter(t => t.text === 'x');
    assert.equal(uses.length, 2);
  });
});
