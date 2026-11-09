import * as assert from 'node:assert/strict';
import {before, describe, it} from 'node:test';

import {hoverMarkdown} from '../hover';
import {parseModule} from '../semantics/parser';
import {SemanticAnalyzer, SemanticDocInfo} from '../semantics/analyzer';

const SOURCE = [
  'declare function x(a, b)',
  'declare function z({a, b=1}) async',
  '',
  'declare class A {',
  '  field a',
  '  method b()',
  '}',
  '',
  'declare class B : A {',
  '  field c, d',
  '  new(field e, f=2)',
  '  method g(h) async',
  '  declare method i()',
  '  j(k)',
  '}',
  '',
  'class Partial {',
  '  field real',
  '  declare copy()',
  '  declare method cmp(other)',
  '  declare field native_field',
  '  method real_method() 7',
  '}',
  '',
  'b = B(1)',
  'r = b.g(2)',
  't = b.j(3)',
  'o = Partial()',
  'q = o.copy()',
  'x(1, 2)',
  '',
].join('\n');

let analyzer: SemanticAnalyzer;
let info: SemanticDocInfo;

before(async () => {
  analyzer = new SemanticAnalyzer();
  await analyzer.init([], async () => []);
  info = await analyzer.parseDocument({textDocument: {uri: 'file:///declare.zn'}, text: SOURCE});
});

function tokenAt(row: number, text: string) {
  return info.tokens.find(t => t.row === row - 1 && t.text === text && t.identifier !== undefined)!;
}

describe('declare', () => {
  it('parses declared functions, classes and members', () => {
    assert.deepEqual(parseModule(SOURCE).problems, []);
    assert.deepEqual(info.problems, []);
  });

  it('gives declared classes their members, and partly declared ones the declared members', () => {
    assert.deepEqual(
        info.module!.classes.map(
            c => [c.name.id, [...c.methods.keys()], c.fields.map(f => f.name.id)]),
        [
          ['A', ['b'], ['a']],
          ['B', ['new', 'g', 'i', 'j'], ['c', 'd', 'e']],
          ['Partial', ['copy', 'cmp', 'real_method'], ['real', 'native_field']],
        ]);
  });

  it('links uses to the declarations', () => {
    for (const [row, text, line] of [[26, 'g', 12], [27, 'j', 14], [29, 'copy', 19], [30, 'x', 1]] as const) {
      assert.equal(tokenAt(row, text).identifier!.tokens[0].token.pos.rowBegin, line, text);
    }
  });

  it('checks calls of declared functions', async () => {
    const other = await analyzer.parseDocument(
        {textDocument: {uri: 'file:///declare2.zn'}, text: 'declare function x(a, b)\nx(1, 2, 3)\n'});
    assert.deepEqual(
        other.problems.map(p => p.message),
        ['\'x\' takes 2 arguments but 3 were given; the extra ones are ignored.']);
  });

  it('shows declared names as declared on hover', () => {
    assert.equal(hoverMarkdown(info, tokenAt(1, 'x'), analyzer), '```zinnia\ndeclare function x(a, b)\n```');
    assert.equal(hoverMarkdown(info, tokenAt(19, 'copy'), analyzer), '```zinnia\ndeclare method Partial.copy()\n```');
    assert.equal(hoverMarkdown(info, tokenAt(18, 'real'), analyzer), '```zinnia\nfield Partial.real\n```');
  });

  it('still needs method bodies outside declarations', () => {
    assert.notDeepEqual(parseModule('class C {\n  method m()\n}\n').problems, []);
  });
});

describe('declared types', () => {
  const TYPED = [
    'import shapes',
    '',
    'declare class Box {',
    '  field size is Box, label, square is shapes.Square',
    '  method grow() returns Box',
    '  method area() async returns Int',
    '}',
    '',
    'class Partial {',
    '  declare field box is Box',
    '  declare method shape() returns shapes.Square',
    '  declare copy() returns Partial',
    '}',
    '',
    'declare function make() returns Box',
    '',
    'b = make()',
    'b.grow().size.square.side()',
    'p = Partial()',
    'p.copy().box.grow()',
    'p.shape().side()',
    '',
  ].join('\n');

  let typed: SemanticDocInfo;
  let typedAnalyzer: SemanticAnalyzer;

  before(async () => {
    typedAnalyzer = new SemanticAnalyzer();
    await typedAnalyzer.init([], async () => [
      {name: 'shapes', path: '/stdlib/shapes.zn', text: 'class Square {\n  method side() 1\n}\n'},
    ]);
    typed = await typedAnalyzer.parseDocument({textDocument: {uri: 'file:///typed.zn'}, text: TYPED});
  });

  function typedToken(row: number, text: string) {
    return typed.tokens.find(t => t.row === row - 1 && t.text === text)!;
  }

  it('parses return types and field types', () => {
    assert.deepEqual(parseModule(TYPED).problems, []);
    assert.deepEqual(typed.problems, []);
    assert.equal(typedToken(5, 'returns').type, 'keyword');
    assert.equal(typedToken(4, 'is').type, 'keyword');
  });

  it('infers the classes of calls and fields from their declared types', () => {
    const declaredAt = (row: number, text: string) => {
      const declaration = typedToken(row, text).identifier?.tokens[0];
      return declaration && `${declaration.filePath}:${declaration.token.pos.rowBegin}`;
    };
    assert.equal(declaredAt(18, 'grow'), '/typed.zn:5');
    assert.equal(declaredAt(18, 'size'), '/typed.zn:4');
    assert.equal(declaredAt(18, 'square'), '/typed.zn:4');
    assert.equal(declaredAt(18, 'side'), '/stdlib/shapes.zn:2');
    assert.equal(declaredAt(20, 'box'), '/typed.zn:10');
    assert.equal(declaredAt(20, 'grow'), '/typed.zn:5');
    assert.equal(declaredAt(21, 'side'), '/stdlib/shapes.zn:2');
  });

  it('links type names to their classes', () => {
    assert.equal(typedToken(5, 'Box').identifier?.tokens[0].token.pos.rowBegin, 3);
    assert.equal(typedToken(11, 'Square').identifier?.tokens[0].filePath, '/stdlib/shapes.zn');
  });

  it('shows declared types on hover', () => {
    assert.equal(
        hoverMarkdown(typed, typedToken(5, 'grow'), typedAnalyzer),
        '```zinnia\ndeclare method Box.grow() returns Box\n```');
    assert.equal(
        hoverMarkdown(typed, typedToken(11, 'shape'), typedAnalyzer),
        '```zinnia\ndeclare method Partial.shape() returns shapes.Square\n```');
    assert.equal(
        hoverMarkdown(typed, typedToken(10, 'box'), typedAnalyzer),
        '```zinnia\ndeclare field Partial.box: Box\n```');
  });
});
