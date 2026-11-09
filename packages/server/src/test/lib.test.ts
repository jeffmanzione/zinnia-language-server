import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';

import {loadZinniaLibs, normalizePath} from '../lib';
import {SemanticAnalyzer} from '../semantics/analyzer';

describe('standard library', () => {
  it('is bundled with the server', () => {
    const names = loadZinniaLibs().map(lib => lib.name);
    for (const name of ['builtin', 'io', 'async', 'struct', 'net']) {
      assert.ok(names.includes(name), `missing ${name}`);
    }
  });

  it('is loaded by default', async () => {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([]);
    assert.equal(analyzer.searchBuiltinForId('cat')?.type, 'function');
    const io = analyzer.lookupDocInfoFromModuleName('io');
    assert.ok(io !== undefined && io.path.endsWith('/stdlib/io.zn'));
    assert.deepEqual(io.problems, []);
  });
});

describe('normalizePath', () => {
  it('uses / and a lowercase drive letter', () => {
    assert.equal(normalizePath('C:\\Users\\me\\io.zn'), 'c:/Users/me/io.zn');
    assert.equal(normalizePath('/home/me/io.zn'), '/home/me/io.zn');
  });
});
