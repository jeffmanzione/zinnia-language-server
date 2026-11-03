import * as assert from 'node:assert/strict';
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import * as path from 'node:path';
import {before, describe, it} from 'node:test';

import {renameLocations, renameTarget} from '../rename';
import {findZnFiles, SemanticAnalyzer} from '../semantics/analyzer';

const LIBS = [
  {name: 'builtin', path: '/stdlib/builtin.zn', text: 'function str(x) x\n'},
];

let workspace: string;

function write(name: string, ...lines: string[]): string {
  const filePath = path.join(workspace, name);
  mkdirSync(path.dirname(filePath), {recursive: true});
  writeFileSync(filePath, lines.join('\n') + '\n');
  return filePath;
}

async function indexed(): Promise<[SemanticAnalyzer, number]> {
  const analyzer = new SemanticAnalyzer();
  await analyzer.init(
      [{uri: 'file://' + workspace, name: 'workspace'}], async () => LIBS);
  return [analyzer, await analyzer.indexWorkspace()];
}

// The places renaming the declaration of [name] in [filePath] changes.
function renamesOf(analyzer: SemanticAnalyzer, filePath: string, name: string):
    string[] {
  const info = analyzer.lookupDocInfoFromFile(filePath)!;
  const token = info.tokens.find(
      t => t.text === name && t.identifier !== undefined &&
          t.modifiers.includes('declaration'));
  const target = renameTarget(token, analyzer);
  assert.ok(!('error' in target), JSON.stringify(target));
  return renameLocations(target, analyzer.analyzedDocuments())
      .map(l => `${path.relative(workspace, l.filePath)}:${l.row + 1}:${l.col + 1}`);
}

before(() => {
  workspace = mkdtempSync(path.join(tmpdir(), 'zinnia-index-'));
  // main imports shapes, which is found in the workspace; walking the
  // directory finds main first.
  write('a_main.zn', 'import shapes', 'x = shapes.area(1)');
  write('shapes.zn', 'function area(s) s');
  write('sub/more.zn', 'import shapes', 'y = shapes.area(2)');
  write('.hidden/skipped.zn', 'z = 1');
  write('node_modules/pkg/skipped.zn', 'z = 1');
});

describe('workspace index', () => {
  it('finds the workspace\'s files, skipping hidden and package directories', () => {
    assert.deepEqual(
        [...findZnFiles(workspace)].map(f => path.relative(workspace, f)).sort(),
        ['a_main.zn', 'shapes.zn', 'sub/more.zn']);
  });

  it('analyzes each file after the modules it imports', async () => {
    const [analyzer, count] = await indexed();
    assert.equal(count, 3);
    assert.deepEqual(
        renamesOf(analyzer, path.join(workspace, 'shapes.zn'), 'area'),
        ['a_main.zn:2:12', 'shapes.zn:1:10', 'sub/more.zn:2:12']);
  });

  it('reanalyzes a file that changed on disk, and its importers', async () => {
    const [analyzer] = await indexed();
    const shapes = write('shapes.zn', '', 'function area(s) s');
    analyzer.refreshFile(shapes);
    assert.deepEqual(
        renamesOf(analyzer, shapes, 'area'),
        ['a_main.zn:2:12', 'shapes.zn:2:10', 'sub/more.zn:2:12']);
    write('shapes.zn', 'function area(s) s');
    analyzer.refreshFile(shapes);
  });

  it('forgets a deleted file', async () => {
    const [analyzer] = await indexed();
    const extra = write('extra.zn', 'function extra() 1');
    analyzer.refreshFile(extra);
    assert.ok(analyzer.lookupDocInfoFromFile(extra) !== undefined);
    rmSync(extra);
    analyzer.forgetFile(extra);
    assert.equal(analyzer.lookupDocInfoFromFile(extra), undefined);
  });

  it('keeps the editor\'s text of an open file', async () => {
    const [analyzer] = await indexed();
    const main = path.join(workspace, 'a_main.zn');
    analyzer.setOpen(main, true);
    await analyzer.parseDocument(
        {textDocument: {uri: 'file://' + main}, text: 'edited = 1\n'});
    analyzer.refreshFile(main);
    assert.equal(analyzer.lookupDocInfoFromFile(main)!.text, 'edited = 1\n');
    // Once closed, it's analyzed from disk again.
    analyzer.setOpen(main, false);
    assert.equal(
        analyzer.lookupDocInfoFromFile(main)!.text,
        'import shapes\nx = shapes.area(1)\n');
  });
});
