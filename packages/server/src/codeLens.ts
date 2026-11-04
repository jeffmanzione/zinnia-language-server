// Code lenses that run a file's tests.
//
// zinnia's test module runs every test of a module at once (test.Tester), so
// each test class gets a lens that runs the whole file.

import {CodeLens} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {SemanticDocInfo} from './semantics/analyzer';

// The command the client runs a file with, given its URI.
export const RUN_FILE_COMMAND = 'zinnia.runFile';

// Returns a lens on each class of [info] annotated with test.TestClass.
export function testLenses(info: SemanticDocInfo): CodeLens[] {
  const module = info.module;
  if (module === undefined) {
    return [];
  }
  // The names the test module is imported as.
  const testNames = new Set(
      module.imports.filter(i => i.ast.source.token.text === 'test')
          .map(i => i.name.id));
  const lenses: CodeLens[] = [];
  for (const cls of module.classes) {
    const isTestClass = cls.ast.annots.some(
        a => a.secondPart !== undefined && testNames.has(a.firstPart.token.text) &&
            a.secondPart.token.text === 'TestClass');
    if (!isTestClass) {
      continue;
    }
    const token = cls.ast.name.token;
    lenses.push({
      range: {
        start: {line: token.pos.rowBegin - 1, character: token.pos.columnBegin - 1},
        end: {line: token.pos.rowEnd - 1, character: token.pos.columnEnd - 1},
      },
      command: {
        title: 'Run tests in file',
        command: RUN_FILE_COMMAND,
        arguments: [URI.file(info.path).toString()],
      },
    });
  }
  return lenses;
}
