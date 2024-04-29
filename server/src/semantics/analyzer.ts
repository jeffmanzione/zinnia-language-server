import { TextDocument } from 'vscode-languageserver-textdocument';
import { SemanticModule, SemanticToken, generateSemanticTokens } from './semantic';
import { zinniaTokenizer } from './tokenizer';
import { MODULE } from './parser';
import { expectEOF, expectSingleResult } from 'typescript-parsec';
import { DocParams } from '../server';


interface DocInfo {
	version: number;
	module: SemanticModule;
	tokens: SemanticToken[];
}

export class SemanticAnalyzer {
	docs: Map<string, DocInfo>;
	constructor() {
		this.docs = new Map();
	}

	fetchSemanticTokens(params: DocParams): SemanticToken[] {
		const info = this.docs.get(params.uri);
		if (info?.version == params.version) {
			return info.tokens;
		}
		try {
			const token = zinniaTokenizer.parse(params.text);
			const parserOutput = MODULE.parse(token);
			const output = expectSingleResult(expectEOF(parserOutput));
			const [module, tokens] = generateSemanticTokens(output);
			const info = {
				version: params.version,
				module: module,
				tokens: tokens
			} satisfies DocInfo;
			this.docs.set(params.uri, info);
			return info.tokens;
		} catch (e) {
			console.log(e);
			return [];
		}
	}
}