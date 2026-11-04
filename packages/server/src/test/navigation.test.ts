import * as assert from 'node:assert/strict';
import {mkdtempSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import * as path from 'node:path';
import {before, describe, it} from 'node:test';

import {formatRows} from '../format';
import {documentHighlights, incomingCalls, inlayHints, outgoingCalls, prepareCallHierarchy} from '../navigation';
import {SemanticAnalyzer, SemanticDocInfo} from '../semantics/analyzer';

const LIBS = [
  {name: 'builtin', path: '/stdlib/builtin.zn', text: 'function cat(args) args\n'},
];

let analyzer: SemanticAnalyzer;
let workspace: string;

before(async () => {
  workspace = mkdtempSync(path.join(tmpdir(), 'zinnia-navigation-'));
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

function tokenAt(info: SemanticDocInfo, row: number, col: number) {
  return info.tokens.find(
      t => t.row === row - 1 && t.col <= col - 1 && col - 1 < t.col + t.text.length &&
          t.identifier !== undefined);
}

const PROGRAM = [
  'class Counter {',
  '  field n',
  '  new() { n = 0 }',
  '  method add(by, times) {',
  '    n = n + by * times',
  '    return self',
  '  }',
  '}',
  'function make(start, step) {',
  '  c = Counter()',
  '  c.add(start, 1)',
  '  return c.add(step, step)',
  '}',
  'function twice() make(1, 2)',
  'x = make(3, 4)',
];

describe('document highlights', () => {
  it('marks the declaration as a write and the uses as reads', async () => {
    const info = await open('highlights.zn', ...PROGRAM);
    assert.deepEqual(
        documentHighlights(info, tokenAt(info, 10, 3))
            .map(h => `${h.range.start.line + 1}:${h.range.start.character + 1}:${h.kind}`),
        ['10:3:3', '11:3:2', '12:10:2']);
  });
});

describe('call hierarchy', () => {
  it('finds the callers and callees of a function', async () => {
    const info = await open('calls.zn', ...PROGRAM);
    const [item] = prepareCallHierarchy(tokenAt(info, 9, 10), analyzer);
    assert.equal(item.name, 'make');
    assert.deepEqual(
        incomingCalls(item, analyzer)
            .map(c => [c.from.name, c.fromRanges.map(r => r.start.line + 1)]),
        [['twice', [14]], ['calls.zn', [15]]]);
    assert.deepEqual(
        outgoingCalls(item, analyzer)
            .map(c => [c.to.name, c.fromRanges.map(r => r.start.line + 1)]),
        [['Counter', [10]], ['add', [11, 12]]]);
  });

  it('finds the callers of a method', async () => {
    const info = await open('method_calls.zn', ...PROGRAM);
    const [item] = prepareCallHierarchy(tokenAt(info, 4, 10), analyzer);
    assert.equal(item.detail, 'Counter');
    assert.deepEqual(
        incomingCalls(item, analyzer).map(c => [c.from.name, c.fromRanges.length]),
        [['make', 2]]);
  });
});

describe('inlay hints', () => {
  it('names the parameters of positional arguments', async () => {
    const info = await open(
        'hints.zn', ...PROGRAM, 'step = 1', 'y = make(step, cat(1))',
        'z = make(start: 1, step: 2)', 'w = make(\'a, b\', (1, 2))');
    assert.deepEqual(
        inlayHints(info, 0, 100, analyzer)
            .map(h => `${h.position.line + 1}:${h.position.character + 1} ${h.label}`),
        [
          '11:9 by:', '11:16 times:', '12:16 by:', '12:22 times:',
          '14:23 start:', '14:26 step:', '15:10 start:', '15:13 step:',
          '17:10 start:', '17:16 step:', '19:10 start:', '19:18 step:',
        ]);
  });
});

describe('range formatting', () => {
  it('formats only the rows asked for', () => {
    assert.deepEqual(
        formatRows('if x {\n  y\nz\n}\n', {tabSize: 2, insertSpaces: true}, 2, 3),
        [{row: 2, newText: '  z'}]);
  });
});
