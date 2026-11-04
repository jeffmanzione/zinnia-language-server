// Zinnia tests in VS Code's test explorer.
//
// The server finds the tests: methods annotated with test.Test in classes
// annotated with test.TestClass. A file's tests run in one zinnia process,
// with --test listing the ones to run, and its output says which failed.

import {spawn} from 'child_process';
import * as path from 'path';
import {CancellationToken, ExtensionContext, Location, Position, Range, TestController, TestItem, TestMessage, TestRun, TestRunProfileKind, TestRunRequest, tests, Uri, workspace} from 'vscode';
import {LanguageClient} from 'vscode-languageclient/node';
import {parseFailures, ranTests, stripColors} from 'zinnia-language-shared/testOutput';

interface LspRange {
  start: {line: number, character: number};
  end: {line: number, character: number};
}

interface TestFile {
  uri: string;
  classes: {name: string, range: LspRange, methods: {name: string, range: LspRange}[]}[];
}

const DOCUMENT_TESTS_REQUEST = 'zinnia/documentTests';
const WORKSPACE_TESTS_REQUEST = 'zinnia/workspaceTests';

function toRange(range: LspRange): Range {
  return new Range(
      new Position(range.start.line, range.start.character),
      new Position(range.end.line, range.end.character));
}

// What a test item is: a file, a class or a method.
type TestKind = {kind: 'file', uri: Uri}|{kind: 'class', uri: Uri, name: string}|
    {kind: 'method', uri: Uri, className: string, name: string};

export class ZinniaTests {
  private readonly controller: TestController;
  private readonly kinds = new WeakMap<TestItem, TestKind>();

  constructor(context: ExtensionContext, private readonly client: LanguageClient) {
    this.controller = tests.createTestController('zinnia', 'Zinnia');
    context.subscriptions.push(this.controller);
    this.controller.createRunProfile(
        'Run', TestRunProfileKind.Run,
        (request, token) => this.run(request, token), true);
    this.controller.refreshHandler = () => this.refreshAll();

    // Keep a document's tests current as it changes, once edits pause.
    const timers = new Map<string, ReturnType<typeof setTimeout>>();
    context.subscriptions.push(workspace.onDidChangeTextDocument(event => {
      const document = event.document;
      if (document.languageId !== 'zinnia' || document.uri.scheme !== 'file') {
        return;
      }
      const key = document.uri.toString();
      clearTimeout(timers.get(key));
      timers.set(key, setTimeout(() => {
        timers.delete(key);
        this.refreshDocument(document.uri);
      }, 500));
    }));
    context.subscriptions.push(workspace.onDidOpenTextDocument(document => {
      if (document.languageId === 'zinnia' && document.uri.scheme === 'file') {
        this.refreshDocument(document.uri);
      }
    }));
    context.subscriptions.push(workspace.onDidDeleteFiles(event => {
      for (const uri of event.files) {
        this.controller.items.delete(uri.toString());
      }
    }));
  }

  // Lists the tests of every workspace file.
  async refreshAll(): Promise<void> {
    const files = await this.client.sendRequest<TestFile[]>(WORKSPACE_TESTS_REQUEST);
    const seen = new Set<string>();
    for (const file of files) {
      seen.add(file.uri);
      this.update(file);
    }
    this.controller.items.forEach(item => {
      if (!seen.has(item.id)) {
        this.controller.items.delete(item.id);
      }
    });
  }

  private async refreshDocument(uri: Uri): Promise<void> {
    const file =
        await this.client.sendRequest<TestFile|null>(DOCUMENT_TESTS_REQUEST, {uri: uri.toString()});
    if (file === null || file.classes.length === 0) {
      this.controller.items.delete(uri.toString());
    } else {
      this.update(file);
    }
  }

  // Replaces the items of [file] with the tests it now has.
  private update(file: TestFile): void {
    const uri = Uri.parse(file.uri);
    const fileItem = this.controller.createTestItem(
        file.uri, workspace.asRelativePath(uri), uri);
    this.kinds.set(fileItem, {kind: 'file', uri: uri});
    for (const cls of file.classes) {
      const classItem = this.controller.createTestItem(
          `${file.uri}#${cls.name}`, cls.name, uri);
      classItem.range = toRange(cls.range);
      this.kinds.set(classItem, {kind: 'class', uri: uri, name: cls.name});
      for (const method of cls.methods) {
        const methodItem = this.controller.createTestItem(
            `${file.uri}#${cls.name}.${method.name}`, method.name, uri);
        methodItem.range = toRange(method.range);
        this.kinds.set(
            methodItem, {kind: 'method', uri: uri, className: cls.name, name: method.name});
        classItem.children.add(methodItem);
      }
      fileItem.children.add(classItem);
    }
    this.controller.items.add(fileItem);
  }

  // The test methods [item] stands for.
  private methodsOf(item: TestItem): TestItem[] {
    if (this.kinds.get(item)?.kind === 'method') {
      return [item];
    }
    const methods: TestItem[] = [];
    item.children.forEach(child => methods.push(...this.methodsOf(child)));
    return methods;
  }

  private async run(request: TestRunRequest, token: CancellationToken): Promise<void> {
    const run = this.controller.createTestRun(request);
    const excluded = new Set(request.exclude ?? []);
    const requested: TestItem[] = [];
    if (request.include !== undefined) {
      requested.push(...request.include);
    } else {
      this.controller.items.forEach(item => requested.push(item));
    }
    // The methods to run, by file.
    const byFile = new Map<string, {uri: Uri, methods: TestItem[], whole: boolean}>();
    for (const item of requested) {
      const kind = this.kinds.get(item);
      if (kind === undefined) {
        continue;
      }
      const entry = byFile.get(kind.uri.toString()) ??
          {uri: kind.uri, methods: [], whole: false};
      entry.methods.push(...this.methodsOf(item).filter(m => !excluded.has(m)));
      entry.whole ||= kind.kind === 'file' && excluded.size === 0;
      byFile.set(kind.uri.toString(), entry);
    }
    for (const {uri, methods, whole} of byFile.values()) {
      if (token.isCancellationRequested) {
        break;
      }
      await this.runFile(run, uri, methods, whole, token);
    }
    run.end();
  }

  // Runs [methods] of the file at [uri], or all its tests if [whole], and
  // reports their results to [run].
  private async runFile(
      run: TestRun, uri: Uri, methods: TestItem[], whole: boolean,
      token: CancellationToken): Promise<void> {
    if (methods.length === 0) {
      return;
    }
    methods.forEach(m => run.started(m));
    const names = methods.map(m => {
      const kind = this.kinds.get(m) as {className: string, name: string};
      return `${kind.className}.${kind.name}`;
    });
    const executable =
        workspace.getConfiguration('zinnia').get<string>('executable') || 'zinnia';
    const args = whole ? [uri.fsPath] : [uri.fsPath, '--', `--test=${names.join(',')}`];
    // Path imports are relative to where zinnia runs: the workspace folder.
    const cwd = workspace.getWorkspaceFolder(uri)?.uri.fsPath ?? path.dirname(uri.fsPath);
    run.appendOutput(`> ${executable} ${args.join(' ')}\r\n`);

    const start = Date.now();
    const {output, exitCode, error} = await new Promise<{output: string, exitCode: number|null, error?: Error}>(
        resolve => {
          let output = '';
          const child = spawn(executable, args, {cwd: cwd});
          const onData = (data: Buffer) => {
            const text = data.toString();
            output += text;
            run.appendOutput(text.replace(/\r?\n/g, '\r\n'));
          };
          child.stdout.on('data', onData);
          child.stderr.on('data', onData);
          const cancel = token.onCancellationRequested(() => child.kill());
          child.on('error', error => {
            cancel.dispose();
            resolve({output, exitCode: null, error});
          });
          child.on('close', code => {
            cancel.dispose();
            resolve({output, exitCode: code});
          });
        });
    const duration = Date.now() - start;

    if (error !== undefined || !ranTests(output)) {
      // The tests didn't get to run, e.g. zinnia isn't installed or the file
      // doesn't compile.
      const message = new TestMessage(
          error !== undefined ?
              `Could not run ${executable}: ${error.message}. Set zinnia.executable to the zinnia executable.` :
              stripColors(output).trim() || `${executable} exited with code ${exitCode}.`);
      methods.forEach(m => run.errored(m, message));
      return;
    }
    const failures = parseFailures(output);
    methods.forEach((m, i) => {
      const failure = failures.get(names[i]);
      if (failure === undefined) {
        run.passed(m, duration);
      } else {
        const message = new TestMessage(failure);
        if (m.uri !== undefined && m.range !== undefined) {
          message.location = new Location(m.uri, m.range.start);
        }
        run.failed(m, message, duration);
      }
    });
  }
}
