import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';

import {testLenses} from '../codeLens';
import {SemanticAnalyzer} from '../semantics/analyzer';

describe('test lenses', () => {
  it('runs the file from each test class', async () => {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => []);
    const info = await analyzer.parseDocument({
      textDocument: {uri: 'file:///work/math_test.zn'},
      text: [
        'import test as t', 't.Tester().test(self)', '@t.TestClass', 'class MathTest {',
        '  @t.Test', '  method test_abs() 1', '}', 'class Helper {}', '',
      ].join('\n'),
    });
    assert.deepEqual(testLenses(info), [{
      range: {start: {line: 3, character: 6}, end: {line: 3, character: 14}},
      command: {
        title: 'Run tests in file',
        command: 'zinnia.runFile',
        arguments: ['file:///work/math_test.zn'],
      },
    }]);
  });
});
