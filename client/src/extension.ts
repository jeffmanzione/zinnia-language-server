import * as path from 'path';

import {
	languages,
	workspace,
	CancellationToken,
	DocumentSemanticTokensProvider,
	ExtensionContext,
	SemanticTokens,
	SemanticTokensBuilder,
	SemanticTokensLegend,
	TextDocument
} from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

import { zinniaTokenizer } from './tokenizer';
import { CONSTANT, EXPRESSION, MODULE, POSTFIX } from './parser';
import { expectEOF, expectSingleResult } from 'typescript-parsec';

let client: LanguageClient;

const tokenTypes = new Map<string, number>();
const tokenModifiers = new Map<string, number>();

const legend = (function () {
	const tokenTypesLegend = [
		'comment', 'string', 'keyword', 'number', 'regexp', 'operator', 'namespace',
		'type', 'struct', 'class', 'interface', 'enum', 'typeParameter', 'function',
		'method', 'decorator', 'macro', 'variable', 'parameter', 'property', 'label'
	];
	tokenTypesLegend.forEach((tokenType, index) => tokenTypes.set(tokenType, index));

	const tokenModifiersLegend = [
		'declaration', 'documentation', 'readonly', 'static', 'abstract', 'deprecated',
		'modification', 'async'
	];
	tokenModifiersLegend.forEach((tokenModifier, index) => tokenModifiers.set(tokenModifier, index));

	return new SemanticTokensLegend(tokenTypesLegend, tokenModifiersLegend);
})();

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	const serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
	);

	context.subscriptions.push(
		languages.registerDocumentSemanticTokensProvider(
			{ scheme: 'file', language: 'zinnia', pattern: "**/*.zn" },
			new ZinniaDocumentSemanticTokensProvider(),
			legend)
	);


	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'zinnia', pattern: '**/*.zn' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'zinnia',
		'Zinnia',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

interface IParsedToken {
	line: number;
	startCharacter: number;
	length: number;
	tokenType: string;
	tokenModifiers: string[];
}


class ZinniaDocumentSemanticTokensProvider implements DocumentSemanticTokensProvider {
	async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
		const allTokens = this._parseText(document.getText());
		const builder = new SemanticTokensBuilder();
		allTokens.forEach((token) => {
			builder.push(token.line, token.startCharacter, token.length, this._encodeTokenType(token.tokenType), this._encodeTokenModifiers(token.tokenModifiers));
		});
		return builder.build();
	}

	private _encodeTokenType(tokenType: string): number {
		if (tokenTypes.has(tokenType)) {
			return tokenTypes.get(tokenType)!;
		} else if (tokenType === 'notInLegend') {
			return tokenTypes.size + 2;
		}
		return 0;
	}

	private _encodeTokenModifiers(strTokenModifiers: string[]): number {
		let result = 0;
		for (let i = 0; i < strTokenModifiers.length; i++) {
			const tokenModifier = strTokenModifiers[i];
			if (tokenModifiers.has(tokenModifier)) {
				result = result | (1 << tokenModifiers.get(tokenModifier)!);
			} else if (tokenModifier === 'notInLegend') {
				result = result | (1 << tokenModifiers.size + 2);
			}
		}
		return result;
	}

	private _parseText(text: string): IParsedToken[] {
		const token = zinniaTokenizer.parse(text);
		const parserOutput = MODULE.parse(token);
		const output = expectSingleResult(expectEOF(parserOutput));
		console.log(output);


		// const classes: string[] = [];
		// const functions: string[] = [];
		// const methods: string[] = [];
		// const imports: string[] = [];

		// for (let i = 0; i < rawTokens.length; i++) {
		// 	const prevToken = (i == 0) ? null : rawTokens[i - 1];
		// 	const token = rawTokens[i];
		// 	const nextToken = (i == rawTokens.length - 1) ? null : rawTokens[i + 1];
		// 	if (prevToken?.text == 'import') {
		// 		imports.push(token.text);
		// 	} else if (prevToken?.text == 'class' ||
		// 		(prevToken?.text == '.' && this._isCapitalized(token.text))) {
		// 		classes.push(token.text);
		// 	} else if (prevToken?.text == 'def' ||
		// 		(prevToken?.text != '.' && nextToken?.text == '(')) {
		// 		functions.push(token.text);
		// 	} else if (token.text == 'new' ||
		// 		prevToken?.text == 'method' ||
		// 		(prevToken?.text == '.' && nextToken?.text == '(')) {
		// 		console.log(token);
		// 		methods.push(token.text);
		// 		// } else if (prevToken?.text == 'field') {
		// 		// 	token.type = 'property';
		// 	} else if (nextToken?.text == ':') {
		// 		token.type = 'parameter';
		// 	}
		// }

		// const tokens: IParsedToken[] = [];
		// for (const token of rawTokens) {
		// 	let tokenType;
		// 	const tokenModifiers: string[] = [];
		// 	if (classes.indexOf(token.text) >= 0) {
		// 		tokenType = 'class';
		// 	} else if (functions.indexOf(token.text) >= 0) {
		// 		tokenType = 'function';
		// 	} else if (methods.indexOf(token.text) >= 0) {
		// 		tokenType = 'method';
		// 	} else if (imports.indexOf(token.text) >= 0) {
		// 		tokenType = 'namespace';
		// 	} else {
		// 		tokenType = token.type;
		// 	}

		// 	if (token.text == 'HELLO') {
		// 		tokenModifiers.push('static');
		// 	}

		// 	tokens.push({
		// 		line: token.line - 1,
		// 		startCharacter: token.column - 1,
		// 		length: token.text.length,
		// 		tokenType: tokenType,
		// 		tokenModifiers: tokenModifiers
		// 	});
		// }
		// return tokens;
		return [];
	}

	private _isCapitalized(word: string): boolean {
		return word[0] == word[0].toUpperCase();
	}
}