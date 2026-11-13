import * as assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import * as path from 'node:path';
import {before, describe, it} from 'node:test';

import * as oniguruma from 'vscode-oniguruma';
import * as vsctm from 'vscode-textmate';

const GRAMMAR = path.join(__dirname, '../../../client/src/zinnia.tmGrammar.json');

let grammar: vsctm.IGrammar;

before(async () => {
  const wasm = readFileSync(require.resolve('vscode-oniguruma/release/onig.wasm'));
  await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (patterns: string[]) => new oniguruma.OnigScanner(patterns),
      createOnigString: (s: string) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async () => vsctm.parseRawGrammar(readFileSync(GRAMMAR, 'utf8'), GRAMMAR),
  });
  grammar = (await registry.loadGrammar('source.zn'))!;
});

// The text of each token on each line of [lines] whose scopes include [scope].
function tokensIn(lines: string[], scope: string): string[][] {
  let rule = vsctm.INITIAL;
  return lines.map(line => {
    const result = grammar.tokenizeLine(line, rule);
    rule = result.ruleStack;
    return result.tokens.filter(t => t.scopes.some(s => s.startsWith(scope)))
        .map(t => line.slice(t.startIndex, t.endIndex));
  });
}

describe('TextMate grammar', () => {
  it('ends strings after escaped backslashes and quotes', () => {
    assert.deepEqual(
        tokensIn([
          "if c == '\\\\' {",
          "  out.extend('\\\\\\\\')",
          "  s = 'it\\'s'",
          '}',
          'x = 1',
        ], 'string'),
        [
          ["'", '\\\\', "'"],
          ["'", '\\\\', '\\\\', "'"],
          ["'", 'it', "\\'", 's', "'"],
          [],
          [],
        ]);
  });

  it('marks escape sequences', () => {
    assert.deepEqual(
        tokensIn(["'a\\nb\\t'"], 'constant.character.escape'), [['\\n', '\\t']]);
  });
});
