import * as assert from 'node:assert/strict';
import {before, describe, it} from 'node:test';

import {docComment, hoverMarkdown} from '../hover';
import {SemanticAnalyzer, SemanticDocInfo} from '../semantics/analyzer';

const LIBS = [
  {
    name: 'builtin',
    path: '/stdlib/builtin.zn',
    text: 'function cat(args) args\nclass String {}\n',
  },
  {
    name: 'shapes',
    path: '/stdlib/shapes.zn',
    text: [
      '; Shapes and their areas.', '', 'import builtin', '',
      '; The area of a square with sides of [side].', 'function area(side) side * side',
      '',
    ].join('\n'),
  },
];

let analyzer: SemanticAnalyzer;
let info: SemanticDocInfo;

before(async () => {
  analyzer = new SemanticAnalyzer();
  await analyzer.init([], async () => LIBS);
  info = await analyzer.parseDocument({
    textDocument: {uri: 'file:///hover.zn'},
    text: [
      'import shapes',
      '; A counter.',
      '@shapes.Thing',
      'class Counter : Base {',
      '  field n',
      '  static START = 0',
      '  new(field step) {}',
      '  ; Adds [by] to the count.',
      '  method add(by = 1) n',
      '}',
      'c = Counter(1)',
      'const LIMIT = 3',
      'function f(x) x',
      's = \'a\'',
      'p = s.split(\',\')',
      'a = shapes.area(2)',
      'q = c.add()',
      '',
    ].join('\n'),
  });
});

// The hover for the token at the 1-based [row] and [col].
function hoverAt(row: number, col: number): string {
  const tokens = info.tokens.filter(
      t => t.row === row - 1 && t.col <= col - 1 && col - 1 < t.col + t.text.length);
  const token = tokens.find(t => t.identifier !== undefined) ?? tokens[0];
  return hoverMarkdown(info, token, analyzer);
}

function code(text: string): string {
  return '```zinnia\n' + text + '\n```';
}

describe('doc comments', () => {
  it('reads the comment above a definition, past annotations', () => {
    assert.equal(
        docComment('; One\n;\n; Two [x].\n@a.B\nfunction f(x) x\n', 4),
        'One\n\nTwo `x`.');
    assert.equal(docComment('x = 1\nfunction f() 1\n', 1), undefined);
  });

  it('keeps code blocks as they are', () => {
    assert.equal(
        docComment('; See:\n; ```\n; a[i]\n; ```\nfunction f() 1\n', 4),
        'See:\n```\na[i]\n```');
  });
});

describe('hover', () => {
  it('describes classes with their constructor and comment', () => {
    assert.equal(
        hoverAt(4, 7), code('class Counter : Base\nCounter(step)') + '\n\nA counter.');
  });

  it('describes methods, fields and statics with their class', () => {
    assert.equal(
        hoverAt(9, 10),
        code('method Counter.add(by = 1)') + '\n\nAdds `by` to the count.');
    assert.equal(hoverAt(17, 7), code('method Counter.add(by = 1)') + '\n\nAdds `by` to the count.');
    assert.equal(hoverAt(5, 9), code('field Counter.n'));
    assert.equal(hoverAt(6, 10), code('static Counter.START'));
  });

  it('describes variables with their inferred class', () => {
    assert.equal(hoverAt(11, 1), code('variable c: Counter'));
    assert.equal(hoverAt(12, 7), code('const LIMIT'));
    assert.equal(hoverAt(13, 12), code('parameter x'));
    assert.equal(hoverAt(14, 1), code('variable s: String'));
  });

  it('describes functions of other modules and modules', () => {
    assert.equal(
        hoverAt(16, 12),
        code('function area(side)') + '\n\nThe area of a square with sides of `side`.');
    assert.equal(hoverAt(1, 8), code('module shapes') + '\n\nShapes and their areas.');
  });

  it('describes native methods of inferred objects', () => {
    assert.equal(hoverAt(15, 7), code('method String.split(delim)'));
  });
});

describe('hover on aliases', () => {
  it('shows the doc comment of what a module member aliases', async () => {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => [
      {name: 'builtin', path: '/stdlib/builtin.zn',
       text: 'import builtin_ext\n; See builtin_ext.sort().\nself.sort = builtin_ext.sort\n'},
      {name: 'builtin_ext', path: '/stdlib/builtin_ext.zn',
       text: '; Sorts [seq].\nfunction sort(seq) seq\n'},
    ]);
    const info = await analyzer.parseDocument(
        {textDocument: {uri: 'file:///alias.zn'}, text: 'x = sort([1])\n'});
    const token = info.tokens.find(t => t.text === 'sort' && t.identifier !== undefined)!;
    assert.equal(
        hoverMarkdown(info, token, analyzer),
        '```zinnia\nfunction sort(seq)\n```\n\nSorts `seq`.');
  });
});
