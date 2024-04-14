export interface PrimitiveType<T> {
	kind: 'PrimitiveType';
	name: T;
}

export type BoolType = PrimitiveType<'Bool'>;
export type CharType = PrimitiveType<'Char'>;
export type IntType = PrimitiveType<'Int'>;
export type FloatType = PrimitiveType<'Float'>;
export type StringType = PrimitiveType<'String'>;

export interface ArrayType {
	kind: 'ArrayType';
}

export interface TupleType {
	kind: 'TupleType';
	types: Type[];
}

export interface MapType {
	kind: 'MapType';
}

export type Type =
	| BoolType
	| CharType
	| IntType
	| FloatType
	| StringType
	| ArrayType
	| TupleType
	| MapType;