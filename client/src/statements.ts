import * as parsec from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { AssignExpr, AssignLhsExpr, ConditionExpr, Expression, TupleExpr } from './expressions';

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

export interface SelectStat {
	kind: 'SelectStat';
	ifTok: Token;
	cond: TupleExpr;
	ifTrue: Statement;
	elseTok?: Token;
	ifFalse?: Statement;
}

export interface ExitStat {
	kind: 'ExitStat';
	exit: Token;
	expr?: AssignExpr;
}

export interface RaiseStat {
	kind: 'RaiseStat';
	raise: Token;
	expr: AssignExpr;
}

export interface TryStat {
	kind: 'TryStat';
	tryTok: Token;
	stat: Statement;
	catchTok: Token;
	catchAssign: AssignLhsExpr;
	catchStat: Statement;
}

export interface JumpStat {
	kind: 'JumpStat';
	token: Token;
	expr?: TupleExpr;
}

export type Statement =
	| IterStat
	| ImportStat
	| CompoundStat
	| SelectStat
	| ExitStat
	| RaiseStat
	| TryStat
	| JumpStat
	| Expression;

export interface Module {
	statements: Statement[];
}