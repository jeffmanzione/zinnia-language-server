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
	key: Expression;
	value: Expression;
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

export type Expression =
	| IdentifierExpr
	| BoolExpr
	| IntExpr
	| FloatExpr
	| StringExpr
	| TupleExpr
	| ArrayExpr
	| MapEntryExpr
	| MapExpr
	| NewExpr
	| NamedArgExpr
	| ParenExpr;