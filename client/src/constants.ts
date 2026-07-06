import path from 'path';
import {DocumentFilter as VsCodeDocumentFilter} from 'vscode';
import {DocumentFilter} from 'vscode-languageclient/node';

export const ZINNIA_LANGUAGE_ID = 'zinnia';

export const ZINNIA_LANGUAGE_NAME = 'Zinnia';

export const SERVER_ENTRY_POINT_PATH = path.join('server', 'out', 'server.js');


export const ZINNIA_DOCUMENT_SELECTOR: DocumentFilter&VsCodeDocumentFilter = {
  scheme: 'file',
  language: ZINNIA_LANGUAGE_ID,
  pattern: '**/*.zn'
};

export const tokenTypesLegend = [
  'comment',       'string',   'keyword', 'number',    'regexp',    'operator',
  'namespace',     'type',     'struct',  'class',     'interface', 'enum',
  'typeParameter', 'function', 'method',  'decorator', 'macro',     'variable',
  'parameter',     'property', 'label'
];

export const tokenModifiersLegend = [
  'declaration', 'defaultLibrary', 'documentation', 'readonly', 'static',
  'abstract', 'deprecated', 'modification', 'async', 'local'
];