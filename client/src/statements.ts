import * as parsec from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { AssignExpr, AssignLhsExpr, ConditionExpr, Expression } from './expressions';

type Token = parsec.Token<TokenKind>;

export interface ImportStat {
	kind: 'ImportStat' | 'ImportAsStat';
	name: string;
	source: string;
}

export interface ForeachStat {
	kind: 'ForeachStat';
	forTok: Token;
	lparen?: Token;
	lhs: AssignLhsExpr;
	inTok: Token;
	iter: ConditionExpr;
	rparen?: Token;
	stat: Statement;
}

export interface ForStat {
	kind: 'ForStat';
	forTok: Token;
	lparen?: Token;
	first: AssignExpr;
	comma1: Token;
	second: ConditionExpr;
	comma2: Token;
	third: AssignExpr;
	rparen?: Token;
	stat: Statement;
}

export interface WhileStat {
	kind: 'WhileStat';
	whileTok: Token;
	lparen?: Token;
	cond: ConditionExpr;
	rparen?: Token;
	stat: Statement;
}

export type IterStat =
	| ForeachStat
	| ForStat
	| WhileStat;

export interface CompoundStat {
	kind: 'CompoundStat';
	stats: Statement[];
	lbrace: Token;
	rbrace: Token;
}

export type Statement =
	| IterStat
	| ImportStat
	| CompoundStat
	| Expression;

export interface Module {
	statements: Statement[];
}