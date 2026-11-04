import * as assert from 'node:assert/strict';
import {describe, it} from 'node:test';

import {callAt, classAt, classMembers, identifierSignature, memberAccessAt, moduleMembers, signatureInformation, unimportedModuleItems, unimportedModuleMembers} from '../completion';
import {SemanticAnalyzer} from '../semantics/analyzer';
import {NATIVE_METHODS} from '../semantics/natives';

// Returns [text] without its '|', and the offset of the '|'.
function cursor(text: string): [string, number] {
  return [text.replace('|', ''), text.indexOf('|')];
}

describe('memberAccessAt', () => {
  it('finds the receiver and the prefix', () => {
    assert.deepEqual(
        memberAccessAt(...cursor('x = io.pri|')), {receiver: 'io', prefix: 'pri'});
    assert.deepEqual(
        memberAccessAt(...cursor('self.|')), {receiver: 'self', prefix: ''});
    assert.equal(memberAccessAt(...cursor('x = 1|')), undefined);
  });
});

describe('callAt', () => {
  it('finds the callee and the argument index', () => {
    assert.deepEqual(callAt(...cursor('f(1, |')), {receiver: undefined, name: 'f', argIndex: 1});
    assert.deepEqual(
        callAt(...cursor('io.println(g(1, 2), |')),
        {receiver: 'io', name: 'println', argIndex: 1});
    assert.deepEqual(
        callAt(...cursor('f(g(1, 2|')), {receiver: undefined, name: 'g', argIndex: 1});
  });

  it('skips strings, arrays and finished calls', () => {
    assert.deepEqual(
        callAt(...cursor('f(\'a, (b\', |')),
        {receiver: undefined, name: 'f', argIndex: 1});
    assert.equal(callAt(...cursor('x = [1, |')), undefined);
    assert.equal(callAt(...cursor('f(1)\ny = |')), undefined);
  });
});

describe('member completion and signatures', () => {
  const text = [
    'class Point {',
    '  field label',
    '  static ORIGIN = 0',
    '  new(field x, field y = 0) {}',
    '  method norm(scale, offset = x + 1) x',
    '}',
    'self.LIMIT = 3',
    'top = 1',
    'function make(a, b) Point(a, b)',
    '',
  ].join('\n');

  const analyzer = new SemanticAnalyzer();
  async function analyze() {
    await analyzer.init([], async () => []);
    return analyzer.parseDocument(
        {textDocument: {uri: 'file:///completion.zn'}, text: text});
  }

  it('lists a module\'s members but not its top-level variables', async () => {
    const labels = moduleMembers(await analyze(), undefined, analyzer).map(i => i.label).sort();
    assert.deepEqual(labels, ['LIMIT', 'Point', 'make']);
  });

  it('lists a class\'s members', async () => {
    const info = await analyze();
    const cls = classAt(info, 3)!;
    assert.equal(cls.name.id, 'Point');
    assert.deepEqual(
        classMembers(cls, info.text)
            .filter(i => !(i.label in NATIVE_METHODS.Object))
            .map(i => [i.label, i.detail]),
        [
          ['label', undefined], ['x', undefined], ['y', undefined],
          ['ORIGIN', undefined], ['norm', 'norm(scale, offset = x + 1)'],
        ]);
    assert.equal(classAt(info, 7), undefined);
  });

  it('describes signatures with their default values', async () => {
    const info = await analyze();
    const make = [...info.context!.block.definitions()].find(d => d.id === 'make')!;
    assert.deepEqual(
        signatureInformation('make', make.signature!, info.text),
        {label: 'make(a, b)', parameters: [{label: 'a'}, {label: 'b'}]});
    const point = [...info.context!.block.definitions()].find(d => d.id === 'Point')!;
    assert.equal(
        signatureInformation('Point', point.signature!, info.text).label,
        'Point(x, y = 0)');
  });
});

describe('native members and aliases', () => {
  const LIBS = [
    {name: 'builtin', path: '/stdlib/builtin.zn',
     text: 'import builtin_ext\nclass String {}\nself.sort = builtin_ext.sort\n'},
    {name: 'builtin_ext', path: '/stdlib/builtin_ext.zn',
     text: 'function sort(seq, cmp = 1) seq\n'},
    {name: 'math', path: '/stdlib/math.zn', text: 'function sqrt(x) x\n'},
  ];
  const analyzer = new SemanticAnalyzer();

  it('lists native functions of a module', async () => {
    await analyzer.init([], async () => LIBS);
    const math = analyzer.lookupDocInfoFromModuleName('math')!;
    const items = moduleMembers(math, 'math', analyzer);
    assert.equal(items.find(i => i.label === 'pow')?.detail, 'pow(num, power)');
    assert.equal(items.find(i => i.label === 'sqrt')?.detail, 'sqrt(x)');
  });

  it('lists native methods of a class', async () => {
    const builtin = analyzer.lookupDocInfoFromModuleName('builtin')!;
    const string = builtin.module!.classes.find(c => c.name.id === 'String')!;
    const items = classMembers(string, builtin.text);
    assert.equal(items.find(i => i.label === 'split')?.detail, 'split(delim)');
    assert.equal(items.find(i => i.label === 'hash')?.detail, 'hash()');
  });

  it('follows aliases to their signature', async () => {
    const builtin = analyzer.lookupDocInfoFromModuleName('builtin')!;
    const sort = analyzer.searchBuiltinForId('sort')!;
    assert.equal(
        identifierSignature('sort', sort, builtin, analyzer)?.label,
        'sort(seq, cmp = 1)');
    const items = moduleMembers(builtin, 'builtin', analyzer);
    assert.equal(items.find(i => i.label === 'sort')?.detail, 'sort(seq, cmp = 1)');
    assert.equal(items.find(i => i.label === 'Int')?.detail, 'Int(x)');
  });
});

describe('auto-import', () => {
  const LIBS = [
    {name: 'builtin', path: '/stdlib/builtin.zn', text: 'import io\nfunction cat(args) args\n'},
    {name: 'io', path: '/stdlib/io.zn', text: 'function println(a) a\n'},
    {name: 'json', path: '/stdlib/json.zn', text: 'function jsonify(v) v\nfunction _helper(v) v\n'},
    {name: 'net', path: '/stdlib/net.zn', text: 'class Socket {}\n'},
  ];

  async function analyze(text: string) {
    const analyzer = new SemanticAnalyzer();
    await analyzer.init([], async () => LIBS);
    const info = await analyzer.parseDocument({textDocument: {uri: 'file:///auto.zn'}, text: text});
    return {analyzer, info};
  }

  it('offers modules that aren\'t imported, importing them after the imports', async () => {
    const {analyzer, info} = await analyze('import net\nx = 1\n');
    const items = unimportedModuleItems(info, analyzer);
    // builtin's io is already visible; net is imported.
    assert.deepEqual(items.map(i => i.label), ['json']);
    assert.deepEqual(items[0].additionalTextEdits, [{
      range: {start: {line: 1, character: 0}, end: {line: 1, character: 0}},
      newText: 'import json\n',
    }]);
  });

  it('completes members of a module that isn\'t imported', async () => {
    const {analyzer, info} = await analyze('x = json.\n');
    const items = unimportedModuleMembers(info, 'json', analyzer)!;
    assert.deepEqual(items.map(i => [i.label, i.sortText]), [['jsonify', undefined], ['_helper', '~_helper']]);
    assert.equal(items[0].additionalTextEdits![0].newText, 'import json\n');
    // A name the document defines isn't a module to import.
    const shadowed = await analyze('json = 1\nx = json.\n');
    assert.equal(unimportedModuleMembers(shadowed.info, 'json', shadowed.analyzer), undefined);
  });
});
