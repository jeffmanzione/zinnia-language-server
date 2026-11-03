import path from 'path';
import {DocumentFilter as VsCodeDocumentFilter, SemanticTokensLegend} from 'vscode';
export const ZINNIA_LANGUAGE_ID = 'zinnia';

export const ZINNIA_LANGUAGE_NAME = 'Zinnia';

export const SERVER_ENTRY_POINT_PATH =
    path.join('packages', 'server', 'out', 'server.js');


export const ZINNIA_DOCUMENT_SELECTOR: VsCodeDocumentFilter = {
  scheme: 'file',
  language: ZINNIA_LANGUAGE_ID,
  pattern: '**/*.zn'
};

import {ZINNIA_TOKEN_MODIFIERS, ZINNIA_TOKEN_TYPES} from './legend';

export {ZINNIA_TOKEN_MODIFIERS, ZINNIA_TOKEN_TYPES};


export const ZINNIA_TOKEN_TYPES_MAP = new Map<string, number>();
export const ZINNIA_TOKEN_MODIFIERS_MAP = new Map<string, number>();

ZINNIA_TOKEN_TYPES.forEach(
    (tokenType, index) => ZINNIA_TOKEN_TYPES_MAP.set(tokenType, index));
ZINNIA_TOKEN_MODIFIERS.forEach(
    (tokenModifier, index) =>
        ZINNIA_TOKEN_MODIFIERS_MAP.set(tokenModifier, index));


export const ZINNIA_SEMANTIC_TOKENS_LEGEND =
    new SemanticTokensLegend(ZINNIA_TOKEN_TYPES, ZINNIA_TOKEN_MODIFIERS);