import * as parsec from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { CompoundStat } from './statements';

type Token = parsec.Token<TokenKind>;

export interface IdentifierExpr {
	kind: 'IdentifierExpr';
	token: Token;
}

export interface NoneExpr {
	kind: 'NoneExpr';
	token: Token;
}

export interface BoolExpr {
	kind: 'BoolExpr';
	token: Token;
	value: boolean;
}

export interface IntExpr {
	kind: 'IntExpr';
	token: Token;
	value: number;
}

export interface FloatExpr {
	kind: 'FloatExpr';
	token: Token;
	value: number;

}

export interface StringExpr {
	kind: 'StringExpr';
	token: Token;
	text: string;
}

export interface NewExpr {
	kind: 'NewExpr';
	token: Token;
}

export interface TupleChainExpr {
	kind: 'TupleChainExpr';
	values: Expression[];
}

export interface ArrayExpr {
	kind: 'ArrayExpr';
	values: Expression[];
}

export interface MapEntryExpr {
	kind: 'MapEntryExpr';
	key: PostfixExpr;
	value: PostfixExpr;
}

export interface MapExpr {
	kind: 'MapExpr';
	entries: MapEntryExpr[];
}

export interface NamedArgExpr {
	kind: 'NamedArgExpr';
	name: IdentifierExpr;
	value: Expression;
	colonToken: Token;
}

export interface FunctionCallExpr {
	kind: 'FunctionCallExpr';
	args: Expression[] | NamedArgExpr[];
	lparen: Token;
	rparen: Token;
}

export interface ArrayIndexExpr {
	kind: 'ArrayIndexExpr';
	indices: TupleExpr;
	lbrack: Token;
	rbrack: Token;
}

export interface MemberAccessExpr {
	kind: 'MemberAccessExpr';
	field: IdentifierExpr | NewExpr;
	period: Token;
}

export interface EmptyParensExpr {
	kind: 'EmptyParensExpr';
	lparen: Token;
	rparen: Token;
}

export interface ParensExpr {
	kind: 'ParensExpr';
	expr: Expression;
	lparen: Token;
	rparen: Token;
}

export interface PostfixChainExpr {
	kind: 'PostfixChainExpr';
	lhs: PrimaryExpr;
	postfixes: PostfixExpr1[];
}

export interface RangeExpr {
	kind: 'RangeExpr';
	start: PostfixExpr;
	end: PostfixExpr;
	inc?: PostfixExpr;
}

export interface UnaryChainExpr {
	kind: 'UnaryChainExpr';
	unaries: Token[];
	expr: RangeExpr | PostfixExpr;
}

export interface BinaryChainExpr {
	kind: 'BinaryChainExpr';
	tokens: Token[];
	exprs: BinaryExpr[];
}

export interface MultChainExpr {
	kind: 'MultChainExpr';
	tokens: Token[];
	exprs: BinaryExpr[];
}

export interface AddChainExpr {
	kind: 'AddChainExpr';
	tokens: Token[];
	exprs: MultExpr[];
}

export interface InExpr {
	kind: 'InExpr';
	lhs: AddExpr;
	rhs: AddExpr;
	is: Token;
}

export interface RelationChainExpr {
	kind: 'RelationChainExpr';
	tokens: Token[];
	exprs: (InExpr | AddExpr)[];
}

export interface EqualChainExpr {
	kind: 'EqualChainExpr';
	tokens: Token[];
	exprs: RelationExpr[];
}

export interface AndChainExpr {
	kind: 'AndChainExpr';
	tokens: Token[];
	exprs: EqualExpr[];
}

export interface OrChainExpr {
	kind: 'OrChainExpr';
	tokens: Token[];
	exprs: AndExpr[];
}

export interface IsExpr {
	kind: 'IsExpr';
	lhs: OrExpr;
	rhs: OrExpr;
	is: Token;
}

export interface ConditionBaseExpr {
	kind: 'ConditionBaseExpr';
	if: Token;
	condition: IsExpr | OrExpr;
	then?: Token;
	ifTrue: ConditionExpr;
	else?: Token;
	ifFalse?: ConditionExpr;
}

export interface AssignTupleExpr {
	kind: 'AssignTupleExpr';
	assigns: AssignLhsExpr[];
	lparen: Token;
	rparen: Token;
}

export interface AssignArrayExpr {
	kind: 'AssignArrayExpr';
	assigns: AssignLhsExpr[];
	lbrack: Token;
	rbrack: Token;
}

export type AssignLhsExpr =
	| IdentifierExpr
	| AssignTupleExpr
	| AssignArrayExpr;

export interface AssignBaseExpr {
	kind: 'AssignBaseExpr';
	lhs: AssignLhsExpr;
	rhs: ConditionExpr;
	eq: Token;
}

export interface ParamExpr {
	kind: 'ParamExpr';
	field?: Token;
	name: IdentifierExpr;
	eq?: Token;
	defaultValue?: ConditionExpr;
}

export interface AnonExpr {
	kind: 'AnonExpr';
	asyncTok?: Token;
	lparen: Token;
	rparen: Token;
	params: ParamExpr[];
	isNamed: boolean;
	arrow?: Token;
	expr: AssignExpr | CompoundStat;
}

export interface AnnotationExpr {
	kind: 'AnnotationExpr';
	at: Token;
	expr: PostfixExpr;
}

export type ConstantExpr =
	| NoneExpr
	| BoolExpr
	| IntExpr
	| FloatExpr;

export type PrimaryExpr =
	| IdentifierExpr
	| NewExpr
	| ConstantExpr
	| StringExpr
	| ArrayExpr
	| MapExpr
	| ParensExpr
	| AnonExpr;

const constantExprs = new Set([
	'NoneExpr',
	'IntExpr',
	'FloatExpr',
	'BoolExpr',
]);

export function isConstantExpr(expr: object): boolean {
	if (!('kind' in expr)) {
		return false;
	}
	return constantExprs.has('kind' in expr ? expr.kind as string : '');
}

const primaryExprs = new Set([
	'IdentifierExpr',
	'NewExpr',
	'StringExpr',
	'ArrayExpr',
	'MapExpr',
	'TupleExpr'
]);

export function isPrimaryExpr(expr: object): boolean {
	if (isConstantExpr(expr)) {
		return true;
	}
	if (!('kind' in expr)) {
		return false;
	}
	const str = 'kind' in expr ? expr.kind as string : '';
	return primaryExprs.has(str);
}

export type Expression = TupleExpr;

export type PostfixExpr1 =
	| ArrayIndexExpr
	| MemberAccessExpr
	| EmptyParensExpr
	| FunctionCallExpr;

export type PostfixExpr =
	| PrimaryExpr
	| PostfixChainExpr;

export type UnaryExpr =
	| UnaryChainExpr
	| PostfixExpr
	| RangeExpr;

export type BinaryExpr =
	| BinaryChainExpr
	| UnaryExpr;

export type MultExpr =
	| MultChainExpr
	| BinaryExpr;

export type AddExpr =
	| AddChainExpr
	| MultExpr;

export type RelationExpr =
	| RelationChainExpr
	| InExpr
	| AddExpr;

export type EqualExpr =
	| EqualChainExpr
	| RelationExpr;

export type AndExpr =
	| AndChainExpr
	| EqualExpr;

export type OrExpr =
	| OrChainExpr
	| AndExpr;

export type ConditionExpr =
	| ConditionBaseExpr
	| IsExpr
	| OrExpr;

export type AssignExpr =
	| AssignBaseExpr
	| ConditionExpr;

export type TupleExpr =
	| TupleChainExpr
	| AssignExpr;