import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';

import {parseFailures, ranTests} from 'zinnia-language-shared/testOutput';

// What zinnia's test runner prints when two of three tests fail, colors
// included, with the uncaught error it raises first.
const OUTPUT = [
  '\x1b[31;1mError: 2 test(s) failed.',
  '  \x1b[31;1mfail_test(\x1b[0mfail_test.zn:3\x1b[31;1m)\x1b[0m',
  '\x1b[0m',
  'Test T.test_bad=\x1b[31;1mFAIL\x1b[0m',
  '',
  '\x1b[31;1mExpectError: Expected \'3\' but was \'2\'.',
  '  \x1b[31;1mfail_test.T.test_bad(\x1b[0mfail_test.zn:13\x1b[31;1m)\x1b[0m',
  '\x1b[0m',
  'Test T.test_raise=\x1b[31;1mFAIL\x1b[0m',
  '',
  'Error: boom',
  'Tests [\x1b[31;1m1/3\x1b[0m] passed.',
  '',
].join('\r\n');

describe('test output', () => {
  it('finds the failed tests and their errors', () => {
    assert.deepEqual([...parseFailures(OUTPUT)], [
      ['T.test_bad', 'ExpectError: Expected \'3\' but was \'2\'.\n  fail_test.T.test_bad(fail_test.zn:13)'],
      ['T.test_raise', 'Error: boom'],
    ]);
  });

  it('tells whether the tests ran', () => {
    assert.ok(ranTests(OUTPUT));
    assert.ok(!ranTests('Could not parse line 3, file: x.zn\nFatally exiting.\n'));
  });
});
