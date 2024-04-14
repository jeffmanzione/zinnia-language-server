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

export interface TupleExpr {
	kind: 'TupleExpr';
	values: Expression[];
}

export interface ArrayExpr {
	kind: 'ArrayExpr';
	values: Expression[];
}

export interface ParenExpr {
	kind: 'ParenExpr';
	expr: Expression;
}

export type Expression =
	| IdentifierExpr
	| BoolExpr
	| IntExpr
	| FloatExpr
	| StringExpr
	| TupleExpr
	| ArrayExpr
	| ParenExpr;