import {existsSync, readdirSync, readFileSync} from 'fs';
import * as path from 'path';
import {WorkspaceFolder} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {SemanticTokensParams2} from '../interfaces';
import {LibFile, loadZinniaLibs, normalizePath} from '../lib';

import {ParseProblem, parseModule} from './parser';
import {generateSemanticTokens, SemanticContext, SemanticIdentifier, SemanticImport, SemanticModule, SemanticToken} from './semantic';

export {ParseProblem};

export interface SemanticDocInfo {
  path: string;
  text: string;
  context?: SemanticContext;
  module?: SemanticModule;
  tokens: SemanticToken[];
  // Syntax errors in the document. The other fields describe the parts of the
  // document that could still be parsed.
  problems: ParseProblem[];
}

// Directories that hold no Zinnia sources of the workspace's own.
function isSkippedDir(name: string): boolean {
  return name.startsWith('.') || name === 'node_modules' ||
      name.startsWith('bazel-');
}

// Yields the paths of the .zn files under [dir], with '/' separators.
export function* findZnFiles(dir: string): Generator<string> {
  let entries;
  try {
    entries = readdirSync(dir, {withFileTypes: true});
  } catch {
    return;
  }
  for (const entry of entries) {
    if (entry.isDirectory()) {
      if (!isSkippedDir(entry.name)) {
        yield* findZnFiles(path.join(dir, entry.name));
      }
    } else if (entry.name.endsWith('.zn')) {
      yield normalizePath(path.join(dir, entry.name));
    }
  }
}

// The modules [text] imports, as written: names, or paths for quoted imports.
function importsIn(text: string): {name: string, isPath: boolean}[] {
  const imports = [];
  for (const match of text.matchAll(/^\s*import\s+(i?'[^'\n]*'|[\w$]+)/gm)) {
    const isPath = match[1].endsWith('\'');
    imports.push({
      name: isPath ? match[1].replace(/^i?'|'$/g, '') : match[1],
      isPath: isPath,
    });
  }
  return imports;
}

// The path, with '/' separators, of the file at [uri].
export function filePathOf(uri: string): string {
  return normalizePath(URI.parse(uri).fsPath);
}

export class SemanticAnalyzer {
  private readonly docs_: Map<string, SemanticDocInfo> = new Map();
  // The files the editor has open, whose text comes from the editor rather
  // than the disk.
  private readonly openFiles_ = new Set<string>();
  private readonly libNamesToFilePaths_: Map<string, string> = new Map();
  private workspacePaths_: string[] = [];

  private initPromise_?: Promise<void>;
  private initResolve_?: () => void;
  private initReject_?: (errMessage: any) => void;

  constructor() {
    const {promise, resolve, reject} = Promise.withResolvers<void>();
    this.initPromise_ = promise;
    this.initResolve_ = resolve;
    this.initReject_ = reject;
  }

  // Loads the standard library with [loadLibs], which reads the bundled copy by
  // default.
  async init(
      workspaceFolders: WorkspaceFolder[],
      loadLibs: () => Promise<LibFile[]>| LibFile[] = () => loadZinniaLibs()):
      Promise<void> {
    try {
      this.workspacePaths_ = workspaceFolders.map(
          wf => filePathOf(wf.uri));
      const libs = await loadLibs();
      // Analyze builtin first, so the other modules' uses of builtins resolve.
      libs.sort((a, b) => Number(b.name === 'builtin') - Number(a.name === 'builtin'));
      for (const lib of libs) {
        this.libNamesToFilePaths_.set(lib.name, lib.path);
      }
      for (const lib of libs) {
        this.parseDocumentText_(lib.path, lib.text);
      }
    } catch (e: any) {
      // Carry on without the standard library rather than failing every
      // request.
      console.error('Could not load the Zinnia standard library:', e);
    }
    this.initResolve_!();
  }

  // Returns the analysis of the module that [imprt], in the file at
  // [fromPath], imports, if it can be found.
  async importedModule(fromPath: string, imprt: SemanticImport):
      Promise<SemanticDocInfo|undefined> {
    const source = imprt.ast.source;
    const isPath = source.kind === 'StringExpr';
    const name = isPath ? source.token.text.replace(/^i?'|'$/g, '') :
                          source.token.text;
    const libPath = isPath ? undefined : this.libNamesToFilePaths_.get(name);
    if (libPath !== undefined) {
      return this.docs_.get(libPath);
    }
    for (const dir of [...this.workspacePaths_, path.dirname(fromPath)]) {
      const filePath = normalizePath(path.join(dir, name + '.zn'));
      if (this.docs_.has(filePath)) {
        return this.docs_.get(filePath);
      }
      if (existsSync(filePath)) {
        return this.parseDocument(
            {textDocument: {uri: URI.file(filePath).toString()}});
      }
    }
    return undefined;
  }

  // The paths of the workspace files that an import of [name] in the file at
  // [fromPath] may name, in the order they're tried. [isPath] is whether the
  // import names it with a path.
  private importCandidates_(fromPath: string, name: string, isPath: boolean):
      string[] {
    if (!isPath && this.libNamesToFilePaths_.has(name)) {
      return [];
    }
    return [...this.workspacePaths_, path.dirname(fromPath)].map(
        dir => normalizePath(path.join(dir, name + '.zn')));
  }

  // The workspace file that an import of [name] in the file at [fromPath]
  // names, if it exists.
  private workspaceImportPath_(fromPath: string, name: string, isPath: boolean):
      string|undefined {
    return this.importCandidates_(fromPath, name, isPath)
        .find(filePath => this.docs_.has(filePath) || existsSync(filePath));
  }

  // The names of the modules the file at [fromPath] can import by name: the
  // standard library's, except builtin, which every module sees, and the
  // workspace files in a workspace folder or next to it.
  importableModules(fromPath: string): string[] {
    const names = new Set(
        [...this.libNamesToFilePaths_.keys()].filter(n => n !== 'builtin'));
    const dirs = new Set([...this.workspacePaths_, path.dirname(fromPath)]);
    for (const filePath of this.docs_.keys()) {
      if (filePath !== fromPath && filePath.endsWith('.zn') &&
          dirs.has(path.dirname(filePath)) && !this.isStandardLibraryFile(filePath)) {
        names.add(path.basename(filePath, '.zn'));
      }
    }
    return [...names].filter(n => /^[A-Za-z_$][\w$]*$/.test(n)).sort();
  }

  // Whether the file at [fromPath] can import a module named [name].
  isImportable(fromPath: string, name: string): boolean {
    return this.libNamesToFilePaths_.has(name) ||
        this.workspaceImportPath_(fromPath, name, false) !== undefined;
  }

  // Analyzes every Zinnia file in the workspace that isn't open or part of the
  // standard library, each after the workspace modules it imports, so that
  // references, rename and workspace symbols cover the whole workspace.
  // Returns the number of files analyzed.
  async indexWorkspace(): Promise<number> {
    await this.initPromise_;
    return this.indexDirs_(this.workspacePaths_);
  }

  private async indexDirs_(dirs: string[]): Promise<number> {
    const done = new Set<string>();
    let count = 0;
    for (const dir of dirs) {
      for (const filePath of findZnFiles(dir)) {
        if (!done.has(filePath)) {
          count += this.indexFile_(filePath, new Set(), done);
          // Let requests in between files.
          await new Promise(resolve => setImmediate(resolve));
        }
      }
    }
    return count;
  }

  // Analyzes [filePath] after the workspace modules it imports. Returns the
  // number of files analyzed.
  private indexFile_(filePath: string, visiting: Set<string>, done: Set<string>):
      number {
    if (done.has(filePath) || visiting.has(filePath) ||
        this.isStandardLibraryFile(filePath)) {
      return 0;
    }
    visiting.add(filePath);
    let text: string;
    const isOpen = this.openFiles_.has(filePath);
    try {
      text = isOpen ? this.docs_.get(filePath)?.text ?? '' :
                      readFileSync(filePath, 'utf8');
    } catch {
      done.add(filePath);
      return 0;
    }
    let count = 0;
    for (const imprt of importsIn(text)) {
      const imported =
          this.workspaceImportPath_(filePath, imprt.name, imprt.isPath);
      if (imported !== undefined) {
        count += this.indexFile_(imported, visiting, done);
      }
    }
    if (!isOpen) {
      this.parseDocumentText_(filePath, text);
      count++;
    }
    done.add(filePath);
    return count;
  }

  // Records whether the editor has [filePath] open. Once it's closed, its
  // analysis is of its text on disk again.
  setOpen(filePath: string, open: boolean): string[] {
    if (open) {
      this.openFiles_.add(filePath);
      return [];
    }
    return this.openFiles_.delete(filePath) ? this.refreshFile(filePath) : [];
  }

  // Reanalyzes [filePath], unless it's open, after it changed on disk, and the
  // workspace files that import it. Returns the paths of the files
  // reanalyzed.
  refreshFile(filePath: string): string[] {
    // A standard library module is only reanalyzed if it's the one loaded and
    // its text changed, e.g. after an edit in the editor was discarded.
    const isLib = this.isStandardLibraryFile(filePath);
    if (isLib && !this.docs_.has(filePath)) {
      return [];
    }
    const reanalyzed: string[] = [];
    if (!this.openFiles_.has(filePath)) {
      let text: string;
      try {
        text = readFileSync(filePath, 'utf8');
      } catch {
        return isLib ? [] : this.forgetFile(filePath);
      }
      if (isLib && this.docs_.get(filePath)?.text === text) {
        return [];
      }
      this.parseDocumentText_(filePath, text);
      reanalyzed.push(filePath);
    }
    return [...reanalyzed, ...this.reanalyzeImportersOf(filePath)];
  }

  // Forgets [filePath], which was deleted, and reanalyzes the workspace files
  // that import it. Returns the paths of the files reanalyzed.
  forgetFile(filePath: string): string[] {
    if (this.openFiles_.has(filePath)) {
      return [];
    }
    this.forgetOccurrencesIn_(filePath);
    return this.docs_.delete(filePath) ? this.reanalyzeImportersOf(filePath) :
                                         [];
  }

  // The analyzed workspace files that import [filePath].
  importersOf(filePath: string): SemanticDocInfo[] {
    // Any file may use a standard library module without importing it,
    // through builtin's imports.
    const isLib = this.isStandardLibraryFile(filePath);
    return [...this.docs_.values()].filter(
        doc => doc.path !== filePath && !this.isStandardLibraryFile(doc.path) &&
            (isLib ||
             importsIn(doc.text).some(
                 i => this.importCandidates_(doc.path, i.name, i.isPath)
                          .includes(filePath))));
  }

  // Reanalyzes the files that import [filePath], with the text they were last
  // analyzed with, so that they see its latest analysis. Returns their paths.
  reanalyzeImportersOf(filePath: string): string[] {
    const importers = this.importersOf(filePath);
    for (const doc of importers) {
      this.parseDocumentText_(doc.path, doc.text);
    }
    return importers.map(doc => doc.path);
  }

  // Adds a workspace folder and indexes it. Returns the number of files
  // analyzed.
  async addWorkspaceFolder(folder: WorkspaceFolder): Promise<number> {
    const dir = filePathOf(folder.uri);
    if (this.workspacePaths_.includes(dir)) {
      return 0;
    }
    this.workspacePaths_.push(dir);
    return this.indexDirs_([dir]);
  }

  // Removes a workspace folder, forgetting the files in it that aren't open.
  removeWorkspaceFolder(folder: WorkspaceFolder): void {
    const dir = filePathOf(folder.uri);
    this.workspacePaths_ = this.workspacePaths_.filter(p => p !== dir);
    const prefix = dir.endsWith('/') ? dir : dir + '/';
    for (const filePath of [...this.docs_.keys()]) {
      if (filePath.startsWith(prefix) && !this.openFiles_.has(filePath) &&
          !this.isStandardLibraryFile(filePath) &&
          !this.workspacePaths_.some(p => filePath.startsWith(p + '/'))) {
        this.forgetOccurrencesIn_(filePath);
        this.docs_.delete(filePath);
      }
    }
  }

  // Returns the analysis of the module [name] that the file at [fromPath]
  // imports, if that module has already been analyzed. [isPath] is whether
  // the import names it with a path.
  analyzedImport(fromPath: string, name: string, isPath: boolean):
      SemanticDocInfo|undefined {
    const libPath = isPath ? undefined : this.libNamesToFilePaths_.get(name);
    if (libPath !== undefined) {
      return this.docs_.get(libPath);
    }
    for (const dir of [...this.workspacePaths_, path.dirname(fromPath)]) {
      const info =
          this.docs_.get(normalizePath(path.join(dir, name + '.zn')));
      if (info !== undefined) {
        return info;
      }
    }
    return undefined;
  }

  private parseDocumentText_(path: string, text: string): SemanticDocInfo {
    const parsed = parseModule(text);
    try {
      // This document's previous analysis added its occurrences to identifiers
      // shared with other documents (e.g., builtins); remove them so they
      // don't accumulate with each reanalysis.
      this.forgetOccurrencesIn_(path);
      const [context, module, tokens, semanticProblems] =
          generateSemanticTokens(path, this, parsed.module);
      const info: SemanticDocInfo = {
        path: path,
        text: text,
        context: context,
        module: module,
        tokens: tokens,
        problems: [
          ...parsed.problems,
          ...semanticProblems,
          ...this.checkImports_(path, module),
        ],
      };
      this.docs_.set(path, info);
      return info;
    } catch (e) {
      // The semantic pass failed, which is a bug: report it, and keep the last
      // analysis so highlighting and hovers keep working.
      console.error(`Could not analyze ${path}:`, e);
      const previous = this.docs_.get(path);
      const info: SemanticDocInfo = {
        path: path,
        text: text,
        context: previous?.context,
        module: previous?.module,
        tokens: previous?.tokens ?? [],
        problems: parsed.problems,
      };
      this.docs_.set(path, info);
      return info;
    }
  }

  // Reports imports of modules that can't be found: neither a standard library
  // module nor a file in the workspace or next to the importing file.
  private checkImports_(filePath: string, module: SemanticModule):
      ParseProblem[] {
    const problems: ParseProblem[] = [];
    const libsLoaded = this.libNamesToFilePaths_.size > 0;
    for (const imprt of module.imports) {
      const source = imprt.ast.source;
      const isPath = source.kind === 'StringExpr';
      const name = isPath ? source.token.text.replace(/^i?'|'$/g, '') :
                            source.token.text;
      if (!isPath && this.libNamesToFilePaths_.has(name)) {
        continue;
      }
      const dirs = [...this.workspacePaths_, path.dirname(filePath)];
      if (dirs.some(dir => existsSync(path.join(dir, name + '.zn')))) {
        continue;
      }
      // Without the standard library, a module name may be one of its modules.
      if (!isPath && !libsLoaded) {
        continue;
      }
      problems.push({
        message: `Module '${name}' not found.`,
        row: source.token.pos.rowBegin - 1,
        col: source.token.pos.columnBegin - 1,
        length: source.token.text.length,
        severity: 'warning',
      });
    }
    return problems;
  }

  private forgetOccurrencesIn_(path: string): void {
    const previous = this.docs_.get(path);
    if (previous === undefined) {
      return;
    }
    const identifiers = new Set(
        previous.tokens.map(t => t.identifier).filter(id => id !== undefined));
    for (const id of identifiers) {
      id.tokens = id.tokens.filter(t => t.filePath !== path);
    }
  }

  private async parseDocumentWithoutWaitingForLibs_(
      params: SemanticTokensParams2): Promise<SemanticDocInfo> {
    const path = filePathOf(params.textDocument.uri);

    let text: string;
    if ('text' in params) {
      text = params.text!;
    } else {
      text = readFileSync(path).toString();
    }
    // A standard library module opened unchanged, as when the editor shows it
    // for a definition, keeps its analysis. Analyzing it again would replace
    // the identifiers that every other file's uses of it refer to.
    const existing = this.docs_.get(path);
    if (existing !== undefined && existing.text === text &&
        this.isStandardLibraryFile(path)) {
      return existing;
    }
    return this.parseDocumentText_(path, text);
  }

  async parseDocument(params: SemanticTokensParams2): Promise<SemanticDocInfo> {
    await this.initPromise_;
    return this.parseDocumentWithoutWaitingForLibs_(params);
  }

  // Whether [filePath] is a standard library module, either as fetched or in a
  // checkout of zinnia.
  isStandardLibraryFile(filePath: string): boolean {
    for (const libPath of this.libNamesToFilePaths_.values()) {
      if (libPath === filePath) {
        return true;
      }
    }
    const match = /(?:^|\/)zinnia\/lib\/([a-z_]+)\.zn$/.exec(filePath);
    return match !== null &&
        (this.libNamesToFilePaths_.size === 0 ||
         this.libNamesToFilePaths_.has(match[1]));
  }

  // The documents analyzed so far, including the standard library's.
  analyzedDocuments(): IterableIterator<SemanticDocInfo> {
    return this.docs_.values();
  }

  lookupDocInfoFromFile(filePath: string): SemanticDocInfo|undefined {
    return this.docs_.get(filePath);
  }

  lookupDocInfoFromModuleName(name: string): SemanticDocInfo|undefined {
    const libFilePath = this.libNamesToFilePaths_.get(name);
    if (!libFilePath) {
      return;
    }
    return this.lookupDocInfoFromFile(libFilePath);
  }

  private get builtin(): SemanticDocInfo|undefined {
    return this.lookupDocInfoFromModuleName('builtin');
  }

  searchBuiltinForId(id: string): SemanticIdentifier|undefined {
    return this.builtin?.context?.block.findIdentifier(id, false);
  }
}