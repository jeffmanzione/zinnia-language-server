import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';

import {loadZinniaLibs} from '../lib';
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
