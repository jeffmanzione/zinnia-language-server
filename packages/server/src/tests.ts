// Discovery of the tests in Zinnia files, for the client's test explorer.
//
// A test is a method annotated with test.Test in a class annotated with
// test.TestClass. zinnia's test.Tester runs a file's tests, all of them or
// the ones its --test flag lists.

import {Range} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {SemanticDocInfo} from './semantics/analyzer';
import {AnnotationExpr} from './semantics/expressions';
import {Token} from './semantics/tokenizer';

export interface TestMethod {
  name: string;
  range: Range;
}

export interface TestClass {
  name: string;
  range: Range;
  methods: TestMethod[];
}

export interface TestFile {
  uri: string;
  classes: TestClass[];
}

// The request for the tests of one document, by URI.
export const DOCUMENT_TESTS_REQUEST = 'zinnia/documentTests';
// The request for the tests of every analyzed workspace file.
export const WORKSPACE_TESTS_REQUEST = 'zinnia/workspaceTests';

function rangeOf(first: Token, last: Token): Range {
  return {
    start: {line: first.pos.rowBegin - 1, character: first.pos.columnBegin - 1},
    end: {line: last.pos.rowEnd - 1, character: last.pos.columnEnd - 1},
  };
}

// Returns the test classes of [info] and their test methods.
export function testsOf(info: SemanticDocInfo): TestFile {
  const module = info.module;
  const file: TestFile = {uri: URI.file(info.path).toString(), classes: []};
  if (module === undefined) {
    return file;
  }
  // The names the test module is imported as.
  const testNames = new Set(
      module.imports.filter(i => i.ast.source.token.text === 'test')
          .map(i => i.name.id));
  const isTestAnnotation = (annots: AnnotationExpr[]|undefined, name: string) =>
      (annots ?? []).some(
          a => a.secondPart !== undefined && testNames.has(a.firstPart.token.text) &&
              a.secondPart.token.text === name);
  for (const cls of module.classes) {
    if (!isTestAnnotation(cls.ast.annots, 'TestClass')) {
      continue;
    }
    const methods: TestMethod[] = [];
    for (const method of cls.methods.values()) {
      if (isTestAnnotation(method.ast.annots, 'Test')) {
        methods.push({
          name: method.name.id,
          range: rangeOf(method.ast.methodTok ?? method.ast.name.token,
                         method.ast.rparen ?? method.ast.name.token),
        });
      }
    }
    file.classes.push({
      name: cls.name.id,
      range: rangeOf(cls.ast.classTok, cls.ast.rbrace ?? cls.ast.name.token),
      methods: methods.sort((a, b) => a.range.start.line - b.range.start.line),
    });
  }
  return file;
}
