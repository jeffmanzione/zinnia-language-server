import { SemanticModule, SemanticToken, generateSemanticTokens } from './semantic';
import { zinniaTokenizer } from './tokenizer';
import { MODULE } from './parser';
import { expectEOF, expectSingleResult } from 'typescript-parsec';
import { DocParams } from '../interfaces';
import { fetchZinniaLibs } from '../lib';
import { WorkspaceFolder } from 'vscode-languageserver/node';
import { readdirSync, readFileSync } from 'fs';
import { URI } from 'vscode-uri';
import * as path from 'path';

interface SemanticDocInfo {
	path: string;
	module: SemanticModule;
	tokens: SemanticToken[];
	version: number;
}

function* _findLocalZnFiles(dirs: string[]): Generator<string> {
	for (const dir of dirs) {
		const files = readdirSync(dir, { withFileTypes: true });
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

	async init(workspaceFolders: WorkspaceFolder[]): Promise<void> {
		this._workspacePaths = workspaceFolders.map(wf => URI.parse(wf.uri).fsPath);
		const libs = await fetchZinniaLibs();
		for (const lib of libs) {
			const info = this.parseDocument(lib);
			this._libNamesToFilePaths.set(info.path.slice('lib/'.length, info.path.length - '.zn'.length), info.path);
		}
	}

	private _lookupSemanticDocInfo(moduleName: string): SemanticDocInfo | null {
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

	lookupModule(moduleName: string): SemanticModule | null {
		return this._lookupSemanticDocInfo(moduleName)?.module ?? null;
	}

	parseDocument(params: DocParams | string): SemanticDocInfo {
		let path: string;
		let version: number;
		if (typeof params === 'string') {
			path = params;
			version = 0;
		} else {
			path = URI.parse(params.uri).fsPath;
			version = params.version;
		}

		const info = this._docs.get(path);
		if (info?.version == version) {
			return info;
		}

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
			const [module, tokens] = generateSemanticTokens(output);
			const info: SemanticDocInfo = {
				path: path,
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
}