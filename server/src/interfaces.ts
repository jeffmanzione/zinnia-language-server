import {Position} from 'vscode-languageserver-textdocument';

export interface DocParams {
  text: string;
  uri: string;
  version: number;
}

export interface HoverParams {
  uri: string;
  position: Position;
  version: number;
}
export interface DirEntry {
  name: string;
  path: string;
}
