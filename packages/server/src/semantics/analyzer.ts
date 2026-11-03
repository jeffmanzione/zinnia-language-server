import {existsSync, readdirSync, readFileSync} from 'fs';
import * as path from 'path';
import {WorkspaceFolder} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {SemanticTokensParams2} from '../interfaces';
import {LibFile, loadZinniaLibs} from '../lib';

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

function* findLocalZnFiles_(dirs: string[]): Generator<string> {
  for (const dir of dirs) {
    const files = readdirSync(dir, {withFileTypes: true});
    for (const file of files) {
      if (file.isDirectory()) {
        yield* findLocalZnFiles_([path.join(dir, file.name)]);
      } else if (file.name.endsWith('.zn')) {
        yield path.join(dir, file.name);
      }
    }
  }
}

export class SemanticAnalyzer {
  private readonly docs_: Map<string, SemanticDocInfo> = new Map();
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

  // Loads the standard library with [loadLibs], which fetches it from GitHub
  // by default.
  // Loads the standard library with [loadLibs], which reads the bundled copy by
  // default.
  async init(
      workspaceFolders: WorkspaceFolder[],
      loadLibs: () => Promise<LibFile[]>| LibFile[] = () => loadZinniaLibs()):
      Promise<void> {
    try {
      this.workspacePaths_ = workspaceFolders.map(
          wf => URI.parse(wf.uri).fsPath.replace(/\\/g, '/'));
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

  private async lookupSemanticDocInfo_(moduleName: string):
      Promise<SemanticDocInfo|null> {
    if (this.libNamesToFilePaths_.has(moduleName)) {
      return this.docs_.get(this.libNamesToFilePaths_.get(moduleName)!)!;
    }
    for (const fileName of findLocalZnFiles_(this.workspacePaths_)) {
      if (moduleName == this.convertFileNameToModuleFormat_(fileName)) {
        return this.parseDocument({textDocument: {uri: fileName}});
      }
    }
    return null;
  }

  private convertFileNameToModuleFormat_(fileName: string): string {
    for (const wsPath of this.workspacePaths_) {
      if (fileName.startsWith(wsPath)) {
        return fileName.slice(wsPath.length, fileName.length - '.zn'.length);
      }
    }
    return fileName.slice(0, fileName.length - '.zn'.length);
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
      const filePath = path.join(dir, name + '.zn').replace(/\\/g, '/');
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

  async lookupModule(moduleName: string): Promise<SemanticModule|null> {
    return (await this.lookupSemanticDocInfo_(moduleName))?.module ?? null;
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
    const path = URI.parse(params.textDocument.uri).fsPath.replace(/\\/g, '/');

    let text: string;
    if ('text' in params) {
      text = params.text!;
    } else {
      text = readFileSync(path).toString();
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