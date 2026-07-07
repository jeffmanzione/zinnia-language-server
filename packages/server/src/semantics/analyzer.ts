import {readdirSync, readFileSync} from 'fs';
import * as path from 'path';
import {expectEOF, expectSingleResult} from 'typescript-parsec';
import {WorkspaceFolder} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {SemanticTokensParams2} from '../interfaces';
import {fetchZinniaLibs} from '../lib';

import {MODULE} from './parser';
import {generateSemanticTokens, SemanticContext, SemanticIdentifier, SemanticModule, SemanticToken} from './semantic';
import {zinniaTokenizer} from './tokenizer';

export interface SemanticDocInfo {
  path: string;
  context: SemanticContext;
  module: SemanticModule;
  tokens: SemanticToken[];
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

  async init(workspaceFolders: WorkspaceFolder[]): Promise<void> {
    try {
      this.workspacePaths_ = workspaceFolders.map(
          wf => URI.parse(wf.uri).fsPath.replace(/\\/g, '/'));
      const libs = await fetchZinniaLibs();
      for (const lib of libs) {
        const info = this.parseDocumentText_(lib.uri, lib.text);
        this.libNamesToFilePaths_.set(
            info.path.slice(
                'zinnia/lib/'.length, info.path.length - '.zn'.length),
            info.path);
      }
      this.initResolve_!();
    } catch (e: any) {
      this.initReject_!(e);
    }
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
    console.log(`Processing ${path}`);
    try {
      const token = zinniaTokenizer.parse(text);
      const parserOutput = MODULE.parse(token);
      const output = expectSingleResult(expectEOF(parserOutput));
      const [context, module, tokens] =
          generateSemanticTokens(path, this, output);
      const info: SemanticDocInfo = {
        path: path,
        context: context,
        module: module,
        tokens: tokens,
      };
      this.docs_.set(path, info);
      return info;
    } catch (e) {
      console.log(e);
      return {} as SemanticDocInfo;
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
    return this.builtin?.context.block.findIdentifier(id, false);
  }
}