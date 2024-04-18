import * as parsec from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { AnnotationExpr, AssignExpr, AssignLhsExpr, ConditionExpr, Expression, IdentifierExpr, NewExpr, ParamExpr, PostfixExpr, StringExpr, TupleExpr } from './expressions';

type Token = parsec.Token<TokenKind>;

export interface ImportStat {
	kind: 'ImportStat' | 'ImportAsStat';
	importTok: Token;
	asTok?: Token;
	name?: IdentifierExpr;
	source: IdentifierExpr | StringExpr;
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

export interface FunctionStat {
	kind: 'FunctionStat';
	defTok: Token;
	asyncTok?: Token;
	lparen: Token;
	rparen: Token;
	name: IdentifierExpr;
	params: ParamExpr[];
	isNamed: boolean;
	stat: Statement;
	annots: AnnotationExpr[];
}

export interface MethodStat {
	kind: 'MethodStat';
	methodTok: Token;
	asyncTok?: Token;
	lparen: Token;
	rparen: Token;
	name: IdentifierExpr | NewExpr;
	params: ParamExpr[];
	isNamed: boolean;
	stat: Statement;
	annots: AnnotationExpr[];
}

export interface FieldStat {
	kind: 'FieldStat';
	fieldTok: Token;
	fields: IdentifierExpr[];
}

export interface StaticStat {
	kind: 'StaticStat';
	staticTok: Token;
	name: IdentifierExpr;
	eq: Token;
	expr: ConditionExpr;
}

export type ClassMemberStat =
	| FieldStat
	| StaticStat
	| MethodStat;

export interface ClassStat {
	kind: 'ClassStat';
	classTok: Token;
	name: IdentifierExpr;
	colon?: Token;
	super?: PostfixExpr;
	lbrace?: Token;
	rbrace?: Token;
	stats: ClassMemberStat[];
	annots: AnnotationExpr[];
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
	| FunctionStat
	| ClassStat
	| Expression;

export interface Module {
	statements: Statement[];
}