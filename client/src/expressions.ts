import * as parsec from 'typescript-parsec';
import { TokenKind } from './tokenizer';

type Token = parsec.Token<TokenKind>;

export interface IdentifierExpr {
	kind: 'IdentifierExpr';
	value: string;
}

export interface BoolExpr {
	kind: 'BoolExpr';
	value: boolean;
}

export interface IntExpr {
	kind: 'IntExpr';
	value: number;
}

export interface FloatExpr {
	kind: 'FloatExpr';
	value: number;

}

export interface StringExpr {
	kind: 'StringExpr';
	text: string;
}

export interface NewExpr {
	kind: 'NewExpr';
	token: Token;
}

export interface TupleExpr {
	kind: 'TupleExpr';
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

export interface ParenExpr {
	kind: 'ParenExpr';
	expr: Expression;
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

export type ConstantExpr =
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
	| TupleExpr;

const primaryExprs = new Set([
	'IdentifierExpr',
	'NewExpr',
	'ConstantExpr',
	'StringExpr',
	'ArrayExpr',
	'MapExpr',
	'TupleExpr'
]);

export function isPrimaryExpr(expr: object): boolean {
	if ('kind'! in expr) {
		return false;
	}
	return primaryExprs.has('kind' in expr ? expr.kind as string : '');
}

export type Expression =
	| PostfixExpr
	| BinaryExpr;

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