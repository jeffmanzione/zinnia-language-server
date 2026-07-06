import {Position} from 'vscode-languageserver-textdocument';
import {SemanticTokensParams} from 'vscode-languageserver/node';

export interface HoverParams {
  uri: string;
  position: Position;
  version: number;
}
export interface DirEntry {
  name: string;
  path: string;
}

export interface SemanticTokensParams2 extends SemanticTokensParams {
  text?: string;
}
