import {CancellationToken, Disposable, DocumentSemanticTokensProvider, Hover, HoverProvider, languages, SemanticTokens, SemanticTokensBuilder, TextDocument} from 'vscode';
import {LanguageClient, SemanticTokensParams} from 'vscode-languageclient/node';
import {ZINNIA_DOCUMENT_SELECTOR, ZINNIA_SEMANTIC_TOKENS_LEGEND, ZINNIA_TOKEN_MODIFIERS_MAP, ZINNIA_TOKEN_TYPES_MAP} from 'zinnia-language-shared/constants';

interface SemanticToken {
  text: string;
  col: number;
  row: number;
  type: string;
  modifiers: string[];
}

interface SemanticTokensParams2 extends SemanticTokensParams {
  text?: string;
}

const encodeTokenType_ = (
    tokenType: string,
    ): number => {
  if (ZINNIA_TOKEN_TYPES_MAP.has(tokenType)) {
    return ZINNIA_TOKEN_TYPES_MAP.get(tokenType)!;
  } else if (tokenType === 'notInLegend') {
    return ZINNIA_TOKEN_TYPES_MAP.size + 2;
  }
  return 0;
};

const encodeTokenModifiers_ = (
    strTokenModifiers: string[],
    ): number => {
  let result = 0;
  for (const element of strTokenModifiers) {
    const tokenModifier = element;
    if (ZINNIA_TOKEN_MODIFIERS_MAP.has(tokenModifier)) {
      result = result | (1 << ZINNIA_TOKEN_MODIFIERS_MAP.get(tokenModifier)!);
    } else if (tokenModifier === 'notInLegend') {
      result = result | (1 << ZINNIA_TOKEN_MODIFIERS_MAP.size + 2);
    }
  }
  return result;
};

const parseText_ = (
    document: TextDocument,
    client: LanguageClient,
    ): Promise<SemanticToken[]> => {
  return client.sendRequest('textDocument/semanticTokens/full', {
    textDocument: {uri: document.uri.fsPath},
    text: document.getText()
  } satisfies SemanticTokensParams2);
};

const createDocumentTokensProvider_ = (
    clientProvider: () => LanguageClient,
    ): DocumentSemanticTokensProvider => {
  const refreshEntireDocument = async (document: TextDocument) => {
    const allTokens = await parseText_(document, clientProvider());
    const builder = new SemanticTokensBuilder();
    allTokens.forEach((token) => {
      builder.push(
          token.row, token.col, token.text.length, encodeTokenType_(token.type),
          encodeTokenModifiers_(token.modifiers));
    });
    return builder.build();
  };

  return {
    provideDocumentSemanticTokens:
        async(document: TextDocument, _: CancellationToken):
            Promise<SemanticTokens> => refreshEntireDocument(document)
  };
};

export const createDocumentTokensProvider =
    (clientProvider: () => LanguageClient): Disposable => {
      return languages.registerDocumentSemanticTokensProvider(
          ZINNIA_DOCUMENT_SELECTOR,
          createDocumentTokensProvider_(clientProvider),
          ZINNIA_SEMANTIC_TOKENS_LEGEND);
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