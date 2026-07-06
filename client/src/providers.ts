import {CancellationToken, Disposable, DocumentSemanticTokensProvider, Hover, HoverProvider, languages, SemanticTokens, SemanticTokensBuilder, SemanticTokensLegend, TextDocument} from 'vscode';
import {LanguageClient} from 'vscode-languageclient/node';
import {tokenModifiersLegend as TOKEN_MODIFIERS_LEGEND, tokenTypesLegend as TOKEN_TYPES_LEGEND, ZINNIA_DOCUMENT_SELECTOR} from './constants';


interface SemanticToken {
  text: string;
  col: number;
  row: number;
  type: string;
  modifiers: string[];
}

interface DocParams {
  text: string;
  uri: string;
  version: number;
}

const _encodeTokenType = (
    tokenType: string,
    tokenTypes: Map<string, number>,
    ): number => {
  if (tokenTypes.has(tokenType)) {
    return tokenTypes.get(tokenType)!;
  } else if (tokenType === 'notInLegend') {
    return tokenTypes.size + 2;
  }
  return 0;
};

const _encodeTokenModifiers = (
    strTokenModifiers: string[],
    tokenModifiers: Map<string, number>,
    ): number => {
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
};

const _parseText = (
    document: TextDocument,
    client: LanguageClient,
    ): Promise<SemanticToken[]> => {
  return client.sendRequest('textDocument/semanticTokens/full', {
    text: document.getText(),
    uri: document.uri.fsPath,
    version: document.version
  } satisfies DocParams);
};

const createDocumentTokensProvider_ = (
    clientProvider: () => LanguageClient,
    tokenTypes: Map<string, number>,
    tokenModifiers: Map<string, number>,
    ): DocumentSemanticTokensProvider => {
  return {
    provideDocumentSemanticTokens:
        async(document: TextDocument, _: CancellationToken):
            Promise<SemanticTokens> => {
              const allTokens = await _parseText(document, clientProvider());
              const builder = new SemanticTokensBuilder();
              allTokens.forEach((token) => {
                builder.push(
                    token.row, token.col, token.text.length,
                    _encodeTokenType(token.type, tokenTypes),
                    _encodeTokenModifiers(token.modifiers, tokenModifiers));
              });
              return builder.build();
            }
  };
};

export const createDocumentTokensProvider =
    (clientProvider: () => LanguageClient): Disposable => {
      const tokenTypes = new Map<string, number>();
      const tokenModifiers = new Map<string, number>();
      TOKEN_TYPES_LEGEND.forEach(
          (tokenType, index) => tokenTypes.set(tokenType, index));
      TOKEN_MODIFIERS_LEGEND.forEach(
          (tokenModifier, index) => tokenModifiers.set(tokenModifier, index));

      const legend =
          new SemanticTokensLegend(TOKEN_TYPES_LEGEND, TOKEN_MODIFIERS_LEGEND);

      return languages.registerDocumentSemanticTokensProvider(
          ZINNIA_DOCUMENT_SELECTOR,
          createDocumentTokensProvider_(
              clientProvider, tokenTypes, tokenModifiers),
          legend);
    };

const createHoverProvider_ =
    (clientProvider: () => LanguageClient): HoverProvider => {
      return {
        async provideHover(document, position, _) {
          const hoverContents: string[] = await clientProvider().sendRequest(
              'textDocument/semanticTokens/hover', {
                uri: document.uri.fsPath,
                version: document.version,
                position: position
              });
          return {contents: hoverContents} as Hover;
        }
      };
    };

export const createHoverSubscription =
    (clientProvider: () => LanguageClient): Disposable => {
      return languages.registerHoverProvider(
          'zinnia', createHoverProvider_(clientProvider));
    };