import { Expression } from './expressions';

export interface ImportStat {
	kind: 'ImportStat' | 'ImportAsStat';
	name: string;
	source: string;
}

export type Statement =
	| ImportStat
	| Expression;

export interface Module {
	statements: Statement[];
}