import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';
import {SymbolKind} from 'vscode-languageserver/node';

import {parseModule} from '../semantics/parser';
import {documentSymbols} from '../symbols';

describe('documentSymbols', () => {
  it('lists classes with their members and top-level functions', () => {
    const {module, problems} = parseModule([
      'class Greeter {',
      '  field greeting',
      '  static DEFAULT = 1',
      '  new(field name) {}',
      '  method greet() name',
      '}',
      'function make() Greeter(\'x\')',
      '',
    ].join('\n'));
    assert.deepEqual(problems, []);
    const symbols = documentSymbols(module);
    assert.deepEqual(
        symbols.map(s => [s.name, s.kind]),
        [['Greeter', SymbolKind.Class], ['make', SymbolKind.Function]]);
    assert.deepEqual(symbols[0].children!.map(c => [c.name, c.kind]), [
      ['greeting', SymbolKind.Field],
      ['DEFAULT', SymbolKind.Constant],
      ['new', SymbolKind.Constructor],
      ['name', SymbolKind.Field],
      ['greet', SymbolKind.Method],
    ]);
    assert.deepEqual(
        symbols[0].selectionRange,
        {start: {line: 0, character: 6}, end: {line: 0, character: 13}});
  });
});
