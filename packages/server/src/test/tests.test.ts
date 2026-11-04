import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';

import {SemanticAnalyzer} from '../semantics/analyzer';
import {testsOf} from '../tests';

describe('test discovery', () => {
  it('finds test classes and their test methods', async () => {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => []);
    const info = await analyzer.parseDocument({
      textDocument: {uri: 'file:///work/math_test.zn'},
      text: [
        'import test as t', 't.Tester().test(self)', '@t.TestClass', 'class MathTest {',
        '  @t.Test', '  method test_abs() 1', '  method helper() 2', '  @t.Test',
        '  method test_pow() 3', '}', 'class Helper {}', '',
      ].join('\n'),
    });
    const file = testsOf(info);
    assert.equal(file.uri, 'file:///work/math_test.zn');
    assert.deepEqual(
        file.classes.map(c => [c.name, c.range.start.line, c.range.end.line, c.methods.map(m => [m.name, m.range.start.line])]),
        [['MathTest', 3, 9, [['test_abs', 5], ['test_pow', 8]]]]);
  });
});
