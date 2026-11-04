import * as assert from 'node:assert/strict';
import {mkdtempSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import * as path from 'node:path';
import {before, describe, it} from 'node:test';

import {receiverType, typeMembers, typeMethodSignature} from '../completion';
import {renameLocations, renameTarget} from '../rename';
import {SemanticAnalyzer, SemanticDocInfo} from '../semantics/analyzer';

const LIBS = [
  {
    name: 'builtin',
    path: '/stdlib/builtin.zn',
    text: [
      'function cat(args) args',
      'class Object {',
      '  method to_s() 1',
      '}',
      'class String {',
      '  method starts_with(prefix) 1',
      '}',
      '',
    ].join('\n'),
  },
];

let analyzer: SemanticAnalyzer;
let workspace: string;

before(async () => {
  workspace = mkdtempSync(path.join(tmpdir(), 'zinnia-types-'));
  analyzer = new SemanticAnalyzer();
  await analyzer.init(
      [{uri: 'file://' + workspace, name: 'workspace'}], async () => LIBS);
});

async function open(name: string, ...lines: string[]): Promise<SemanticDocInfo> {
  const text = lines.join('\n') + '\n';
  writeFileSync(path.join(workspace, name), text);
  return analyzer.parseDocument(
      {textDocument: {uri: 'file://' + path.join(workspace, name)}, text: text});
}

// Where the identifier of the token at the 1-based [row] and [col] is
// declared, as 'row:col', or undefined if the token refers to nothing.
function declarationAt(info: SemanticDocInfo, row: number, col: number): string|undefined {
  const token = info.tokens.find(
      t => t.row === row - 1 && t.col === col - 1 && t.identifier !== undefined);
  const declaration = token?.identifier?.tokens[0];
  return declaration &&
      `${path.basename(declaration.filePath)}:${declaration.token.pos.rowBegin}:${
             declaration.token.pos.columnBegin}`;
}

const SHAPES = [
  'class Shape {',
  '  field name',
  '  method area() 0',
  '}',
  'class Square : Shape {',
  '  new(field side) {}',
  '  method grow() Square(side + 1)',
  '}',
];

describe('receiver types', () => {
  it('links members of a constructed object to their declarations', async () => {
    const info = await open(
        'objects.zn', ...SHAPES, 's = Square(2)', 'x = s.grow()', 'y = s.area()',
        'z = s.side', 'w = s.to_s()');
    assert.equal(declarationAt(info, 10, 7), 'objects.zn:7:10');
    // Inherited from Shape, and from Object.
    assert.equal(declarationAt(info, 11, 7), 'objects.zn:3:10');
    assert.equal(declarationAt(info, 12, 7), 'objects.zn:6:13');
    assert.equal(declarationAt(info, 13, 7), 'builtin.zn:3:10');
  });

  it('follows fields, self and literals', async () => {
    const info = await open(
        'fields.zn', ...SHAPES, 'class Box {', '  field shape',
        '  new() {', '    shape = Square(1)', '  }',
        '  method size() self.shape.area() + shape.side', '}',
        'b = Box()', 'n = b.shape.grow()', 't = \'text\'', 'u = t.starts_with(\'t\')');
    assert.equal(declarationAt(info, 14, 28), 'fields.zn:3:10');
    assert.equal(declarationAt(info, 14, 43), 'fields.zn:6:13');
    assert.equal(declarationAt(info, 17, 13), 'fields.zn:7:10');
    assert.equal(declarationAt(info, 19, 7), 'builtin.zn:6:10');
  });

  it('infers nothing when assignments disagree', async () => {
    const info = await open(
        'mixed.zn', ...SHAPES, 'v = Square(1)', 'v = Shape()', 'a = v.area()',
        'w = None', 'w = Shape()', 'b = w.area()');
    assert.equal(declarationAt(info, 11, 7), undefined);
    assert.equal(declarationAt(info, 14, 7), 'mixed.zn:3:10');
  });

  it('follows classes of other modules', async () => {
    await open('geometry.zn', ...SHAPES);
    const info = await open(
        'uses.zn', 'import geometry', 'sq = geometry.Square(3)', 'a = sq.area()');
    assert.equal(declarationAt(info, 3, 8), 'geometry.zn:3:10');
  });

  it('renames methods called on inferred objects', async () => {
    const info = await open(
        'renames.zn', ...SHAPES, 's = Square(2)', 'a = s.area()', 'b = s.grow().area()');
    const token = info.tokens.find(t => t.row === 2 && t.text === 'area')!;
    const target = renameTarget(token, analyzer);
    assert.ok(!('error' in target));
    assert.deepEqual(
        renameLocations(target, [info]).map(l => `${l.row + 1}:${l.col + 1}`),
        // The result of grow() isn't inferred.
        ['3:10', '10:7']);
  });

  it('completes and describes members of an inferred receiver', async () => {
    const info = await open('complete.zn', ...SHAPES, 's = Square(2)', 's.');
    const type = receiverType(info, 's', 9, 2, analyzer)!;
    assert.equal(type.name, 'Square');
    const labels = typeMembers(type, analyzer).map(i => i.label);
    for (const label of ['grow', 'side', 'area', 'name', 'to_s', 'hash']) {
      assert.ok(labels.includes(label), label);
    }
    assert.equal(typeMethodSignature(type, 'grow', analyzer)?.label, 'grow()');
    assert.equal(typeMethodSignature(type, 'hash', analyzer)?.label, 'hash()');
  });
});
