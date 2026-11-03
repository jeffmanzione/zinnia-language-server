import {readdirSync, readFileSync} from 'fs';
import * as path from 'path';
import {expectEOF, expectSingleResult} from 'typescript-parsec';
import {WorkspaceFolder} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {SemanticTokensParams2} from '../interfaces';
import {fetchZinniaLibs, FileContents} from '../lib';

import {MODULE} from './parser';
import {generateSemanticTokens, SemanticContext, SemanticIdentifier, SemanticModule, SemanticToken} from './semantic';
import {zinniaTokenizer} from './tokenizer';

// A problem found while parsing a document. Rows and columns are 0-based. A
// problem with no position is at the end of the document.
export interface ParseProblem {
  message: string;
  row?: number;
  col?: number;
}

export interface SemanticDocInfo {
  path: string;
  context?: SemanticContext;
  module?: SemanticModule;
  tokens: SemanticToken[];
  // Problems found in the latest version of the document. When it fails to
  // parse, the other fields are from the last version that parsed, if any.
  problems: ParseProblem[];
}

function toParseProblem_(e: any): ParseProblem {
  // typescript-parsec throws TokenErrors, which carry the position separately
  // from the message.
  if (e && typeof e.errorMessage === 'string') {
    const message = e.errorMessage.replace(
        /<END-OF-FILE>/g, 'the end of the file');
    if (e.pos === undefined) {
      return {message: message};
    }
    return {
      message: message,
      row: e.pos.rowBegin - 1,
      col: e.pos.columnBegin - 1,
    };
  }
  return {message: String(e)};
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
  async init(
      workspaceFolders: WorkspaceFolder[],
      loadLibs: () => Promise<FileContents[]> = fetchZinniaLibs): Promise<void> {
    try {
      this.workspacePaths_ = workspaceFolders.map(
          wf => URI.parse(wf.uri).fsPath.replace(/\\/g, '/'));
      const libs = await loadLibs();
      for (const lib of libs) {
        const info = this.parseDocumentText_(lib.uri, lib.text);
        this.libNamesToFilePaths_.set(
            info.path.slice(
                'zinnia/lib/'.length, info.path.length - '.zn'.length),
            info.path);
      }
    } catch (e: any) {
      // Carry on without the standard library (e.g., when offline) rather than
      // failing every request.
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

  async lookupModule(moduleName: string): Promise<SemanticModule|null> {
    return (await this.lookupSemanticDocInfo_(moduleName))?.module ?? null;
  }

  private parseDocumentText_(path: string, text: string): SemanticDocInfo {
    try {
      const token = zinniaTokenizer.parse(text);
      const parserOutput = MODULE.parse(token);
      const output = expectSingleResult(expectEOF(parserOutput));
      // This document's previous analysis added its occurrences to identifiers
      // shared with other documents (e.g., builtins); remove them so they
      // don't accumulate with each reanalysis.
      this.forgetOccurrencesIn_(path);
      const [context, module, tokens] =
          generateSemanticTokens(path, this, output);
      const info: SemanticDocInfo = {
        path: path,
        context: context,
        module: module,
        tokens: tokens,
        problems: [],
      };
      this.docs_.set(path, info);
      return info;
    } catch (e) {
      // Keep the last good analysis so highlighting and hovers keep working
      // while the document is being edited.
      const previous = this.docs_.get(path);
      const info: SemanticDocInfo = {
        path: path,
        context: previous?.context,
        module: previous?.module,
        tokens: previous?.tokens ?? [],
        problems: [toParseProblem_(e)],
      };
      this.docs_.set(path, info);
      return info;
    }
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