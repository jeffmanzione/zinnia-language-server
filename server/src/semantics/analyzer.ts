import {readdirSync, readFileSync} from 'fs';
import * as path from 'path';
import {expectEOF, expectSingleResult} from 'typescript-parsec';
import {WorkspaceFolder} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {DocParams} from '../interfaces';
import {fetchZinniaLibs} from '../lib';

import {MODULE} from './parser';
import {generateSemanticTokens, SemanticContext, SemanticIdentifier, SemanticModule, SemanticToken} from './semantic';
import {zinniaTokenizer} from './tokenizer';

export interface SemanticDocInfo {
  path: string;
  context: SemanticContext;
  module: SemanticModule;
  tokens: SemanticToken[];
  version: number;
}

function* _findLocalZnFiles(dirs: string[]): Generator<string> {
  for (const dir of dirs) {
    const files = readdirSync(dir, {withFileTypes: true});
    for (const file of files) {
      if (file.isDirectory()) {
        yield* _findLocalZnFiles([path.join(dir, file.name)]);
      } else if (file.name.endsWith('.zn')) {
        yield path.join(dir, file.name);
      }
    }
  }
}

export class SemanticAnalyzer {
  private readonly _docs: Map<string, SemanticDocInfo> = new Map();
  private readonly _libNamesToFilePaths: Map<string, string> = new Map();
  private _workspacePaths: string[] = [];

  private _initPromise?: Promise<void>;
  private _initResolve?: () => void;
  private _initReject?: (errMessage: any) => void;

  constructor() {
    const {promise, resolve, reject} = Promise.withResolvers<void>();
    this._initPromise = promise;
    this._initResolve = resolve;
    this._initReject = reject;
  }

  async init(workspaceFolders: WorkspaceFolder[]): Promise<void> {
    try {
      this._workspacePaths = workspaceFolders.map(
          wf => URI.parse(wf.uri).fsPath.replace(/\\/g, '/'));
      const libs = await fetchZinniaLibs();
      for (const lib of libs) {
        const info = await this._parseDocumentWithoutWaitingForLibs(lib);
        this._libNamesToFilePaths.set(
            info.path.slice(
                '/zinnia/lib/'.length, info.path.length - '.zn'.length),
            info.path);
      }
      this._initResolve!();
    } catch (e: any) {
      this._initReject!(e);
    }
  }

  private async _lookupSemanticDocInfo(moduleName: string):
      Promise<SemanticDocInfo|null> {
    if (this._libNamesToFilePaths.has(moduleName)) {
      return this._docs.get(this._libNamesToFilePaths.get(moduleName)!)!;
    }
    for (const fileName of _findLocalZnFiles(this._workspacePaths)) {
      if (moduleName == this._convertFileNameToModuleFormat(fileName)) {
        return this.parseDocument(fileName);
      }
    }
    return null;
  }

  private _convertFileNameToModuleFormat(fileName: string): string {
    for (const wsPath of this._workspacePaths) {
      if (fileName.startsWith(wsPath)) {
        return fileName.slice(wsPath.length, fileName.length - '.zn'.length);
      }
    }
    return fileName.slice(0, fileName.length - '.zn'.length);
  }

  async lookupModule(moduleName: string): Promise<SemanticModule|null> {
    return (await this._lookupSemanticDocInfo(moduleName))?.module ?? null;
  }

  private async _parseDocumentWithoutWaitingForLibs(params: DocParams|string):
      Promise<SemanticDocInfo> {
    let path: string;
    let version: number;
    if (typeof params === 'string') {
      path = params;
      version = 0;
    } else {
      path = URI.parse(params.uri).fsPath.replace(/\\/g, '/');
      version = params.version;
    }

    // const info = this._docs.get(path);
    // if (info?.version == version) {
    // 	return info;
    // }

    console.log(`Processing ${path}`);

    let text: string;
    if (typeof params === 'string') {
      text = readFileSync(path).toString();
    } else {
      text = params.text;
    }

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
        version: version
      };
      this._docs.set(path, info);
      return info;
    } catch (e) {
      console.log(e);
      return {} as SemanticDocInfo;
    }
  }

  async parseDocument(params: DocParams|string): Promise<SemanticDocInfo> {
    await this._initPromise;
    return this._parseDocumentWithoutWaitingForLibs(params);
  }

  lookupDocInfoFromFile(filePath: string): SemanticDocInfo|undefined {
    return this._docs.get(filePath);
  }

  lookupDocInfoFromModuleName(name: string): SemanticDocInfo|undefined {
    const libFilePath = this._libNamesToFilePaths.get(name);
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