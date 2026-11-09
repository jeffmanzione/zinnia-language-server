import {Token, TokenKind} from './tokenizer';
import { AnnotationExpr, AssignExpr, AssignLhsExpr, ConditionExpr, Expression, IdentifierExpr, NewExpr, ParamExpr, PostfixChainExpr, PostfixExpr, StringExpr, TupleExpr } from './expressions';

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

// The class a declared function or method returns, as in `returns A`, or a
// declared field holds, as in `is A`: a class name, or module.ClassName.
export interface TypeAnnotation {
	keyword: Token;
	type: IdentifierExpr | PostfixChainExpr;
}

export interface FunctionStat {
	kind: 'FunctionStat';
	// For a declared function, which exists at runtime without code here.
	declareTok?: Token;
	defTok: Token;
	asyncTok?: Token;
	lparen: Token;
	rparen: Token;
	name: IdentifierExpr;
	params: ParamExpr[];
	isNamed: boolean;
	// None for a declared function.
	stat?: Statement;
	// For a declared function, the class it returns, if given.
	returnType?: TypeAnnotation;
	annots: AnnotationExpr[];
}

export interface SpecialMethodIdentifierExpr {
	kind: 'SpecialMethodIdentifierExpr';
	token: Token;
}

export interface MethodStat {
	kind: 'MethodStat';
	// For a declared method, which exists at runtime without code here.
	declareTok?: Token;
	methodTok?: Token;
	asyncTok?: Token;
	lparen: Token;
	rparen: Token;
	name: IdentifierExpr | NewExpr | SpecialMethodIdentifierExpr;
	params: ParamExpr[];
	isNamed: boolean;
	// None for a declared method, or a method of a declared class.
	stat?: Statement;
	// For a declared method, the class it returns, if given.
	returnType?: TypeAnnotation;
	annots: AnnotationExpr[];
}

export interface FieldStat {
	kind: 'FieldStat';
	declareTok?: Token;
	fieldTok: Token;
	fields: IdentifierExpr[];
	// For a declared field, the class it holds, if given; one per field.
	types: (TypeAnnotation | undefined)[];
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
	// For a declared class, which exists at runtime without code here.
	declareTok?: Token;
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