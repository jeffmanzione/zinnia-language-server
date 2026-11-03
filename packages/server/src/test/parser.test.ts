import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';
import {expectEOF, expectSingleResult} from 'typescript-parsec';

import {SemanticAnalyzer} from '../semantics/analyzer';
import {MODULE} from '../semantics/parser';
import {generateSemanticTokens} from '../semantics/semantic';
import {Module} from '../semantics/statements';
import {zinniaTokenizer} from '../semantics/tokenizer';

function parse(text: string): Module {
  return expectSingleResult(expectEOF(MODULE.parse(zinniaTokenizer.parse(text))));
}

// An analyzer that knows no other modules, so tests don't load the standard
// library.
const STUB_ANALYZER = {
  searchBuiltinForId: () => undefined,
  lookupModule: async () => null,
  lookupDocInfoFromModuleName: () => undefined,
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
  it('reports parse problems and keeps the last good analysis', async () => {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => []);
    const uri = 'file:///analyzer-test.zn';
    const good =
        await analyzer.parseDocument({textDocument: {uri: uri}, text: 'x = 1\n'});
    assert.deepEqual(good.problems, []);
    const bad = await analyzer.parseDocument(
        {textDocument: {uri: uri}, text: 'x = (1,\n'});
    assert.equal(bad.problems.length, 1);
    assert.ok(bad.tokens.some(t => t.text === 'x'));
  });
});
