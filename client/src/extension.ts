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
import { MODULE, } from './parser';
import { expectEOF, expectSingleResult } from 'typescript-parsec';
import { SemanticToken, generateSemanticTokens } from './semantic';

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
		'declaration', 'defaultLibrary', 'documentation', 'readonly', 'static', 'abstract', 'deprecated',
		'modification', 'async', 'local'
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


class ZinniaDocumentSemanticTokensProvider implements DocumentSemanticTokensProvider {
	async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
		const allTokens = this._parseText(document.getText());
		const builder = new SemanticTokensBuilder();
		allTokens.forEach((token) => {
			builder.push(token.row, token.col, token.text.length, this._encodeTokenType(token.type), this._encodeTokenModifiers(token.modifiers));
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
		for (const element of strTokenModifiers) {
			const tokenModifier = element;
			if (tokenModifiers.has(tokenModifier)) {
				result = result | (1 << tokenModifiers.get(tokenModifier)!);
			} else if (tokenModifier === 'notInLegend') {
				result = result | (1 << tokenModifiers.size + 2);
			}
		}
		return result;
	}

	private _parseText(text: string): SemanticToken[] {
		const token = zinniaTokenizer.parse(text);
		const parserOutput = MODULE.parse(token);
		const output = expectSingleResult(expectEOF(parserOutput));
		console.log(output);
		return generateSemanticTokens(output);
	}
}