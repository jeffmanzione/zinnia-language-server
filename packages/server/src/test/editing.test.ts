import * as assert from 'node:assert/strict';
import {before, describe, it} from 'node:test';

import {Diagnostic, DiagnosticSeverity} from 'vscode-languageserver/node';

import {editDistance, quickFixes, similarNames} from '../codeActions';
import {foldingRanges} from '../folding';
import {formatText} from '../format';
import {SemanticAnalyzer, SemanticDocInfo} from '../semantics/analyzer';

const OPTIONS = {tabSize: 2, insertSpaces: true};

function format(...lines: string[]): string[] {
  return formatText(lines.join('\n'), OPTIONS).split('\n');
}

describe('formatting', () => {
  it('indents blocks by their braces', () => {
    assert.deepEqual(
        format('class A {', 'method m() {', '      return 1', '}', '}'),
        ['class A {', '  method m() {', '    return 1', '  }', '}', '']);
  });

  it('keeps the extra indentation of continuations', () => {
    const lines = [
      'function f() {',
      '  if x',
      '    y()',
      '  return a',
      '      .b()',
      '      .c()',
      '}',
      '',
    ];
    assert.deepEqual(format(...lines), lines);
  });

  it('keeps wrapped arguments\' offset and moves them with their block', () => {
    assert.deepEqual(
        format('class A {', '    new(field a,', '        field b) {', '    }', '}'),
        ['class A {', '  new(field a,', '      field b) {', '  }', '}', '']);
  });

  it('indents once for brackets opened on the same line', () => {
    assert.deepEqual(
        format('f = memoize((i) {', '    return i', '  })'),
        ['f = memoize((i) {', '  return i', '})', '']);
  });

  it('removes trailing spaces and ends with one new line', () => {
    assert.deepEqual(format('x = 1  ', '  ', 'y = 2', '', ''), ['x = 1', '', 'y = 2', '']);
  });

  it('leaves multi-line strings and comments alone', () => {
    const lines = [
      'class A {',
      '  method m() {',
      '    s = \'line  ',
      '   more\'',
      '    /* a  ',
      '         b */',
      '  }',
      '}',
      '',
    ];
    assert.deepEqual(format(...lines), lines);
  });

  it('uses tabs if asked', () => {
    assert.equal(
        formatText('if x {\ny\n}\n', {tabSize: 4, insertSpaces: false}),
        'if x {\n\ty\n}\n');
  });

  it('is idempotent', () => {
    const once = formatText('a = [1,\n      2,\n 3]\nf(g(1,\n2))\n', OPTIONS);
    assert.equal(formatText(once, OPTIONS), once);
  });
});

describe('folding', () => {
  it('folds brackets, comments and imports', () => {
    const ranges = foldingRanges([
      'import io',
      'import net',
      '; One',
      '; Two',
      'class A {',
      '  method m() {',
      '    x = [1,',
      '      2]',
      '  }',
      '}',
      '/* a',
      '   b */',
      'f(1)',
    ].join('\n'));
    assert.deepEqual(
        ranges.map(r => [r.startLine, r.endLine, r.kind]),
        [
          [0, 1, 'imports'], [2, 3, 'comment'], [4, 8, undefined],
          [5, 7, undefined], [10, 11, 'comment'],
        ]);
  });
});

describe('quick fixes', () => {
  const LIBS = [
    {name: 'builtin', path: '/stdlib/builtin.zn', text: 'function cat(args) args\n'},
    {name: 'io', path: '/stdlib/io.zn', text: 'function println(a) a\n'},
  ];
  let analyzer: SemanticAnalyzer;

  before(async () => {
    analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => LIBS);
  });

  async function fixes(...lines: string[]) {
    const uri = 'file:///fixes.zn';
    const info = await analyzer.parseDocument({textDocument: {uri}, text: lines.join('\n') + '\n'});
    const diagnostics: Diagnostic[] = info.problems.map(p => ({
      range: {
        start: {line: p.row!, character: p.col!},
        end: {line: p.row!, character: p.col! + (p.length ?? 1)},
      },
      severity: DiagnosticSeverity.Warning,
      message: p.message,
      code: p.code,
      data: p.declaration && {declaration: p.declaration},
    }));
    return quickFixes(info, uri, diagnostics, {
             isModule: name => analyzer.isImportable(info.path, name),
             names: () => ['println', 'limit', 'counter', 'count'],
           })
        .map(a => [a.title, JSON.stringify(a.edit!.changes![uri])]);
  }

  it('imports a missing module', async () => {
    assert.deepEqual(await fixes('io.println(1)', 'x = other.y'), [[
      'Import \'io\'',
      JSON.stringify([{
        range: {start: {line: 0, character: 0}, end: {line: 0, character: 0}},
        newText: 'import io\n',
      }]),
    ]]);
  });

  it('makes a top-level variable a module member', async () => {
    assert.deepEqual(await fixes('limit = 3', 'function f() limit'), [[
      'Make \'limit\' a module member',
      JSON.stringify([{
        range: {start: {line: 0, character: 0}, end: {line: 0, character: 0}},
        newText: 'self.',
      }]),
    ]]);
  });
});

describe('more quick fixes', () => {
  const LIBS = [
    {name: 'builtin', path: '/stdlib/builtin.zn', text: 'function cat(args) args\n'},
    {name: 'io', path: '/stdlib/io.zn', text: 'function println(a) a\n'},
  ];

  async function fixes(...lines: string[]) {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => LIBS);
    const uri = 'file:///more_fixes.zn';
    const info = await analyzer.parseDocument({textDocument: {uri}, text: lines.join('\n') + '\n'});
    const diagnostics: Diagnostic[] = info.problems.map(p => ({
      range: {
        start: {line: p.row!, character: p.col!},
        end: {line: p.row!, character: p.col! + (p.length ?? 1)},
      },
      message: p.message,
      code: p.code,
    }));
    return quickFixes(info, uri, diagnostics, {
             isModule: name => analyzer.isImportable(info.path, name),
             names: () => ['counter', 'count', 'cat', 'limit'],
           })
        .map(a => a.title);
  }

  it('removes an unused import', async () => {
    assert.deepEqual(await fixes('import io', 'x = 1'), ['Remove unused import \'io\'']);
  });

  it('suggests similar names for an undefined one', async () => {
    assert.deepEqual(
        await fixes('counter = 1', 'x = countr + 1'),
        ['Change to \'count\'', 'Change to \'counter\'']);
  });
});

describe('similar names', () => {
  it('measures edits, counting a swap as one', () => {
    assert.equal(editDistance('count', 'count'), 0);
    assert.equal(editDistance('cuont', 'count'), 1);
    assert.equal(editDistance('cnt', 'count'), 2);
  });

  it('allows more edits for longer names', () => {
    assert.deepEqual(similarNames('lst', ['list', 'last', 'lots']), ['last', 'list']);
    assert.deepEqual(similarNames('printn', ['println', 'print', 'sprint']), ['print', 'println', 'sprint']);
  });
});

describe('unknown member fixes', () => {
  it('suggests the class\'s similar members', () => {
    const info = {text: 's.aera()\n', module: undefined} as unknown as SemanticDocInfo;
    const actions = quickFixes(info, 'file:///m.zn', [{
      range: {start: {line: 0, character: 2}, end: {line: 0, character: 6}},
      message: '\'Square\' has no member \'aera\'.',
      code: 'unknown-member',
      data: {suggestions: ['area', 'grow', 'side']},
    }], {isModule: () => false, names: () => []});
    assert.deepEqual(actions.map(a => a.title), ['Change to \'area\'']);
  });
});
