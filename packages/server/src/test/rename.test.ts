import * as assert from 'node:assert/strict';
import {mkdtempSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import * as path from 'node:path';
import {before, describe, it} from 'node:test';

import {invalidName, renameLocations, renameTarget} from '../rename';
import {SemanticAnalyzer, SemanticDocInfo} from '../semantics/analyzer';
import {matchesQuery, workspaceSymbols} from '../symbols';

const LIBS = [
  {name: 'builtin', path: '/stdlib/builtin.zn', text: 'function str(x) x\n'},
];

let analyzer: SemanticAnalyzer;
let workspace: string;

before(async () => {
  workspace = mkdtempSync(path.join(tmpdir(), 'zinnia-rename-'));
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

// The places renaming the token at the 1-based [row] and [col] of [info]
// changes, as 'file:row:col'.
function renamesAt(info: SemanticDocInfo, row: number, col: number): string[] {
  const tokens = info.tokens.filter(
      t => t.row === row - 1 && t.col <= col - 1 &&
          col - 1 < t.col + t.text.length);
  const token = tokens.find(t => t.identifier !== undefined) ?? tokens[0];
  const target = renameTarget(token, analyzer);
  if ('error' in target) {
    return [target.error];
  }
  return renameLocations(target, analyzer.analyzedDocuments())
      .map(l => `${path.basename(l.filePath)}:${l.row + 1}:${l.col + 1}`);
}

describe('rename', () => {
  it('renames a variable and its uses', async () => {
    const info = await open('vars.zn', 'x = 1', 'y = x + x', 'f = () -> x');
    assert.deepEqual(
        renamesAt(info, 2, 5), ['vars.zn:1:1', 'vars.zn:2:5', 'vars.zn:2:9', 'vars.zn:3:11']);
  });

  it('renames a parameter without touching other names', async () => {
    const info = await open(
        'params.zn', 'a = 1', 'function f(a) a + 1', 'function g(a) a');
    assert.deepEqual(renamesAt(info, 2, 12), ['params.zn:2:12', 'params.zn:2:15']);
  });

  it('renames a module member in the files that import it', async () => {
    const lib = await open('shapes.zn', 'function area(s) s', 'self.UNIT = 1',
                           'function twice() area(UNIT) + self.UNIT');
    const user = await open(
        'user.zn', 'import shapes', 'x = shapes.area(shapes.UNIT)');
    assert.deepEqual(
        renamesAt(user, 2, 12),
        ['shapes.zn:1:10', 'shapes.zn:3:18', 'user.zn:2:12']);
    assert.deepEqual(
        renamesAt(lib, 2, 6),
        ['shapes.zn:2:6', 'shapes.zn:3:23', 'shapes.zn:3:36', 'user.zn:2:24']);
  });

  it('renames a method and its uses through self in its class', async () => {
    const info = await open(
        'methods.zn', 'class A {', '  method m() 1', '  method n() self.m() + m()',
        '}', 'class B {', '  method m() 2', '  method n() self.m()', '}');
    assert.deepEqual(
        renamesAt(info, 2, 10),
        ['methods.zn:2:10', 'methods.zn:3:19', 'methods.zn:3:25']);
  });

  it('refuses names it can\'t rename', async () => {
    const info = await open('refuse.zn', 'x = str(1)', 'if x {', '}');
    assert.match(renamesAt(info, 1, 5)[0], /standard library/);
    assert.match(renamesAt(info, 2, 1)[0], /can be renamed/);
  });

  it('checks the new name', () => {
    assert.equal(invalidName('good_name'), undefined);
    assert.match(invalidName('class')!, /keyword/);
    assert.match(invalidName('1x')!, /not a valid name/);
    assert.match(invalidName('a b')!, /not a valid name/);
  });
});

describe('workspace symbols', () => {
  it('matches names by their characters in order', () => {
    assert.ok(matchesQuery('FileReader', 'fr'));
    assert.ok(matchesQuery('FileReader', ''));
    assert.ok(!matchesQuery('FileReader', 'rf'));
  });

  it('lists classes, members and functions', async () => {
    const info = await open(
        'symbols.zn', 'class Reader {', '  method read() 1', '}',
        'function read_all() 1');
    assert.deepEqual(
        workspaceSymbols(info.path, info.module!.ast, 'read')
            .map(s => [s.symbol.name, s.containerName]),
        [['Reader', undefined], ['read', 'Reader'], ['read_all', undefined]]);
  });
});
