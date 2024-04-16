import * as parsec from 'typescript-parsec';
import { Parser, rep_sc, rule } from 'typescript-parsec';
import { alt_sc, apply, kleft, kmid, kright, list_sc, lrec_sc, opt_sc, seq, tok } from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { ArrayExpr, ArrayIndexExpr, BoolExpr, ConstantExpr, EmptyParensExpr, Expression, FloatExpr, FunctionCallExpr, IdentifierExpr, IntExpr, MapEntryExpr, MapExpr, MemberAccessExpr, NamedArgExpr, NewExpr, PostfixExpr, PostfixExpr1, PrimaryExpr, RangeExpr, StringExpr, TupleExpr, UnaryExpr, isPrimaryExpr, BinaryExpr, MultExpr, AddExpr, InExpr, RelationExpr, EqualExpr, AndExpr, OrExpr, IsExpr } from './expressions';
import { Statement, Module, ImportStat } from './statements';

type Token = parsec.Token<TokenKind>;

function applyIdentifier(value: Token): IdentifierExpr {
	return {
		kind: 'IdentifierExpr',
		value: value.text
	};
}

function applyBool(value: Token): BoolExpr {
	return {
		kind: 'BoolExpr',
		value: (/True/i).test(value.text)
	};
}

function applyInt(value: Token): IntExpr {
	return {
		kind: 'IntExpr',
		value: Number(value.text)
	};
}

function applyFloat(value: Token): FloatExpr {
	return {
		kind: 'FloatExpr',
		value: Number(value.text)
	};
}

function applyString(value: Token): StringExpr {
	return {
		kind: 'StringExpr',
		text: value.text
	};
}

function applyNew(value: Token): NewExpr {
	return {
		kind: 'NewExpr',
		token: value
	};
}

function applyTuple(exprs: Expression[]): TupleExpr {
	return {
		kind: 'TupleExpr',
		values: exprs
	};
}

function applyArray(tupleExpr: TupleExpr): ArrayExpr {
	return {
		kind: 'ArrayExpr',
		values: tupleExpr.values
	};
}

function applyMapEntry(entry: [PostfixExpr, Token, PostfixExpr]): MapEntryExpr {
	const [key, _, value] = entry;
	return {
		kind: 'MapEntryExpr',
		key: key,
		value: value
	};
}

function applyMap(entries: [MapEntryExpr]): MapExpr {
	return {
		kind: 'MapExpr',
		entries: entries
	};
}

function applyNamedArg(entries: [IdentifierExpr, Token, Expression]): NamedArgExpr {
	const [name, colonToken, value] = entries;
	return {
		kind: 'NamedArgExpr',
		name: name,
		value: value,
		colonToken: colonToken
	};
}

function applyFunctionCall(
	entries: [Token, (TupleExpr | NamedArgExpr[]), Token]
): FunctionCallExpr {
	const [lparen, expr, rparen] = entries;
	const args: Expression[] | NamedArgExpr[] =
		('kind' in expr && expr.kind === 'TupleExpr')
			? (expr as TupleExpr).values
			: (expr as NamedArgExpr[]);
	return {
		kind: 'FunctionCallExpr',
		args: args,
		lparen: lparen,
		rparen: rparen
	};
}

function applyArrayIndex(entries: [Token, TupleExpr, Token]): ArrayIndexExpr {
	const [lbrack, indicies, rbacket] = entries;
	return {
		kind: 'ArrayIndexExpr',
		indices: indicies,
		lbrack: lbrack,
		rbrack: rbacket
	};
}

function applyMemberAccess(entries: [Token, (IdentifierExpr | NewExpr)]): MemberAccessExpr {
	const [period, field] = entries;
	return {
		kind: 'MemberAccessExpr',
		field: field,
		period: period
	};
}

function applyEmptyParens(parens: [Token, Token]): EmptyParensExpr {
	const [lparen, rparen] = parens;
	return {
		kind: 'EmptyParensExpr',
		lparen: lparen,
		rparen: rparen
	};
}

function applyPostfix(
	entry: PrimaryExpr | [PrimaryExpr, PostfixExpr1[]]
): PostfixExpr {
	if (isPrimaryExpr(entry)) {
		return entry as PrimaryExpr;
	}
	const [primary, postfixes] = entry as [PrimaryExpr, PostfixExpr1[]];
	if (postfixes.length == 0) {
		return primary;
	}
	return {
		kind: 'PostfixChainExpr',
		lhs: primary,
		postfixes: postfixes
	};
}

function applyRange(exprs: PostfixExpr[]): RangeExpr | PostfixExpr {
	if (exprs[1] === undefined) {
		return exprs[0];
	}
	const range: RangeExpr = {
		kind: 'RangeExpr',
		start: exprs[0],
		end: exprs[1],
	};
	if (exprs[2] !== undefined) {
		range.inc = exprs[2];
	}
	return range;
}

function applyUnary(exprs: [Token[], RangeExpr | PostfixExpr]): UnaryExpr {
	const [unaries, expr] = exprs;
	if (unaries.length == 0) {
		return expr;
	}
	return {
		kind: 'UnaryChainExpr',
		expr: expr,
		unaries: unaries
	};
}

function applyBinary(value: [BinaryExpr, [Token, BinaryExpr][]]): BinaryExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'BinaryChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyMult(value: [BinaryExpr, [Token, BinaryExpr][]]): MultExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'MultChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyAdd(value: [MultExpr, [Token, MultExpr][]]): AddExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'AddChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyIn(
	expr: [AddExpr, Token, AddExpr] | AddExpr
): InExpr | AddExpr {
	if ('kind' in expr) {
		return expr as AddExpr;
	}
	const [lhs, is, rhs] = expr;
	return {
		kind: 'InExpr',
		lhs: lhs,
		rhs: rhs,
		is: is
	};
}

function applyRelation(value: [InExpr | AddExpr, [Token, InExpr | AddExpr][]]): RelationExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'RelationChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyEqual(value: [RelationExpr, [Token, RelationExpr][]]): EqualExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'EqualChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyAnd(value: [EqualExpr, [Token, EqualExpr][]]): AndExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'AndChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyOr(value: [AndExpr, [Token, AndExpr][]]): OrExpr {
	const [lhs, rhs] = value;

	if (rhs.length == 0) {
		return lhs;
	}

	const exprs = [lhs];
	const tokens = [];
	for (const [tok, expr] of rhs) {
		exprs.push(expr);
		tokens.push(tok);
	}
	return {
		kind: 'OrChainExpr',
		exprs: exprs,
		tokens: tokens
	};
}

function applyIs(expr: [OrExpr, Token, OrExpr] | OrExpr): IsExpr | OrExpr {
	if ('kind' in expr) {
		return expr;
	}
	const [lhs, is, rhs] = expr;
	return {
		kind: 'IsExpr',
		lhs: lhs,
		rhs: rhs,
		is: is
	};
}

function applyImportStat(value: Token): ImportStat {
	return {
		kind: 'ImportStat',
		name: value.text,
		source: value.text
	};
}

function applyImportAsStat(value: [Token, Token]): ImportStat {
	const [name, source] = value;
	return {
		kind: 'ImportAsStat',
		name: name.text,
		source: source.text
	};
}

function applyModule(value: Statement[]): Module {
	return {
		statements: value
	};
}

// Literals and identifiers
export const IDENTIFIER = rule<TokenKind, IdentifierExpr>();
export const NEW = rule<TokenKind, NewExpr>();
export const CONSTANT = rule<TokenKind, ConstantExpr>();
export const STRING = rule<TokenKind, StringExpr>();

// Structures
export const ARRAY = rule<TokenKind, ArrayExpr>();
export const MAP_ENTRY = rule<TokenKind, MapEntryExpr>();
export const MAP = rule<TokenKind, MapExpr>();
export const TUPLE = rule<TokenKind, TupleExpr>();

// Primary
export const PRIMARY = rule<TokenKind, PrimaryExpr>();
export const PRIMARY_NO_CONSTANTS = rule<TokenKind, PrimaryExpr>();
export const EMPTY_PARENS = rule<TokenKind, EmptyParensExpr>();

// Postfix
export const POSTFIX = rule<TokenKind, PostfixExpr>();
export const POSTFIX1 = rule<TokenKind, PostfixExpr1 | undefined>();
export const NAMED_ARG = rule<TokenKind, NamedArgExpr>();
export const FUNCTION_CALL = rule<TokenKind, FunctionCallExpr>();
export const ARRAY_INDEX = rule<TokenKind, ArrayIndexExpr>();
export const MEMBER_ACCESS = rule<TokenKind, MemberAccessExpr>();

export const RANGE = rule<TokenKind, RangeExpr | PostfixExpr>();
export const UNARY = rule<TokenKind, UnaryExpr>();
export const BINARY_AND = rule<TokenKind, BinaryExpr>();
export const BINARY_XOR = rule<TokenKind, BinaryExpr>();
export const BINARY_OR = rule<TokenKind, BinaryExpr>();
export const MULT = rule<TokenKind, MultExpr>();
export const ADD = rule<TokenKind, AddExpr>();
export const IN = rule<TokenKind, InExpr | AddExpr>();
export const RELATION = rule<TokenKind, RelationExpr>();
export const EQUAL = rule<TokenKind, EqualExpr>();
export const AND = rule<TokenKind, AndExpr>();
export const OR = rule<TokenKind, OrExpr>();
export const IS = rule<TokenKind, IsExpr | OrExpr>();

// Constructive
export const CONDITIONAL = rule<TokenKind, Expression>();

export const EXPRESSION = rule<TokenKind, Expression>();

// Statements
export const IMPORT = rule<TokenKind, ImportStat>();
export const STATEMENT = rule<TokenKind, Statement>();

export const MODULE = rule<TokenKind, Module>();

IDENTIFIER.setPattern(
	apply(
		alt_sc(
			tok(TokenKind.IDENTIFIER),
			tok(TokenKind.KEYWORD_CLASS),
			tok(TokenKind.KEYWORD_MODULE),
			tok(TokenKind.KEYWORD_ASYNC),
			tok(TokenKind.KEYWORD_THEN),
			tok(TokenKind.KEYWORD_FUNCTION)
		),
		applyIdentifier
	)
);

NEW.setPattern(
	apply(
		tok(TokenKind.KEYWORD_NEW),
		applyNew
	)
);

CONSTANT.setPattern(
	alt_sc(
		apply(
			alt_sc(
				tok(TokenKind.KEYWORD_TRUE),
				tok(TokenKind.KEYWORD_FALSE),
			),
			applyBool
		),
		apply(
			tok(TokenKind.LITEARL_INTEGER),
			applyInt
		),
		apply(
			tok(TokenKind.LITEARL_FLOAT),
			applyFloat
		)
	)
);

STRING.setPattern(
	apply(
		tok(TokenKind.LITERAL_STRING),
		applyString
	)
);

ARRAY.setPattern(
	apply(
		kmid(
			tok(TokenKind.SYMBOL_LBRACKET),
			opt_sc(TUPLE),
			tok(TokenKind.SYMBOL_RBRACKET)
		),
		applyArray
	)
);

MAP_ENTRY.setPattern(
	apply(
		seq(
			POSTFIX,
			tok(TokenKind.SYMBOL_COLON),
			POSTFIX
		),
		applyMapEntry
	)
);

MAP.setPattern(
	apply(
		kmid(
			seq(
				tok(TokenKind.SYMBOL_LBRACE),
				opt_sc(tok(TokenKind.NEWLINE))
			),
			opt_sc(
				list_sc(
					MAP_ENTRY,
					tok(TokenKind.SYMBOL_COMMA)
				)
			),
			seq(
				opt_sc(tok(TokenKind.NEWLINE)),
				tok(TokenKind.SYMBOL_RBRACE)
			)
		),
		applyMap
	)
);

PRIMARY.setPattern(
	alt_sc(
		// ANON_FUNCTION_DEFINITION,
		IDENTIFIER,
		NEW,
		CONSTANT,
		STRING,
		ARRAY,
		MAP,
		kmid(
			tok(TokenKind.SYMBOL_LPAREN),
			TUPLE,
			tok(TokenKind.SYMBOL_RPAREN)
		)
	)
);

PRIMARY_NO_CONSTANTS.setPattern(
	alt_sc(
		// ANON_FUNCTION_DEFINITION,
		IDENTIFIER,
		STRING,
		ARRAY,
		MAP,
		kmid(
			tok(TokenKind.SYMBOL_LPAREN),
			TUPLE,
			tok(TokenKind.SYMBOL_RPAREN)
		)
	)
);

EMPTY_PARENS.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LPAREN),
			tok(TokenKind.SYMBOL_RPAREN)
		),
		applyEmptyParens
	)
);

NAMED_ARG.setPattern(
	apply(
		seq(
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				IDENTIFIER,
				opt_sc(tok(TokenKind.NEWLINE))
			),
			tok(TokenKind.SYMBOL_COLON),
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				POSTFIX, // TODO: Change to CONDITIONAL
				opt_sc(tok(TokenKind.NEWLINE))
			),
		),
		applyNamedArg
	)
);

FUNCTION_CALL.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LPAREN),
			alt_sc(
				list_sc(NAMED_ARG, tok(TokenKind.SYMBOL_COMMA)),
				TUPLE
			),
			tok(TokenKind.SYMBOL_RPAREN)
		),
		applyFunctionCall
	)
);

ARRAY_INDEX.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LBRACKET),
			TUPLE,
			tok(TokenKind.SYMBOL_RBRACKET)
		),
		applyArrayIndex
	)
);

MEMBER_ACCESS.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_PERIOD),
			alt_sc(
				IDENTIFIER,
				NEW
			)
		),
		applyMemberAccess
	)
);

POSTFIX1.setPattern(
	alt_sc(
		ARRAY_INDEX,
		EMPTY_PARENS,
		FUNCTION_CALL,
		kmid(
			opt_sc(tok(TokenKind.NEWLINE)),
			MEMBER_ACCESS,
			opt_sc(tok(TokenKind.NEWLINE))
		)
	)
);

POSTFIX.setPattern(
	apply(
		alt_sc(
			seq(
				PRIMARY_NO_CONSTANTS,
				rep_sc(POSTFIX1)
			),
			PRIMARY
		),
		applyPostfix
	)
);

RANGE.setPattern(
	apply(
		seq(
			POSTFIX,
			opt_sc(
				kright(
					tok(TokenKind.SYMBOL_COLON),
					POSTFIX
				)
			),
			opt_sc(
				kright(
					tok(TokenKind.SYMBOL_COLON),
					POSTFIX
				)
			)
		),
		applyRange
	)
);

UNARY.setPattern(
	apply(
		seq(
			rep_sc(
				alt_sc(
					tok(TokenKind.SYMBOL_TILDE),
					tok(TokenKind.SYMBOL_EXCLAIM),
					tok(TokenKind.SYMBOL_MINUS),
					tok(TokenKind.KEYWORD_CONST),
					tok(TokenKind.KEYWORD_AWAIT),
				)
			),
			RANGE
		),
		applyUnary
	)
);

BINARY_AND.setPattern(
	apply(
		seq(
			UNARY,
			rep_sc(
				seq(
					tok(TokenKind.SYMBOL_AMPER),
					UNARY
				)
			)
		),
		applyBinary
	)
);

BINARY_XOR.setPattern(
	apply(
		seq(
			BINARY_AND,
			rep_sc(
				seq(
					tok(TokenKind.SYMBOL_CARET),
					BINARY_AND
				)
			)
		),
		applyBinary
	)
);

BINARY_OR.setPattern(
	apply(
		seq(
			BINARY_XOR,
			rep_sc(
				seq(
					tok(TokenKind.SYMBOL_PIPE),
					BINARY_XOR
				)
			)
		),
		applyBinary
	)
);

MULT.setPattern(
	apply(
		seq(
			BINARY_OR,
			rep_sc(
				seq(
					alt_sc(
						tok(TokenKind.SYMBOL_STAR),
						tok(TokenKind.SYMBOL_FSLASH),
						tok(TokenKind.SYMBOL_PERCENT)
					),
					BINARY_OR
				)
			)
		),
		applyMult
	)
);

ADD.setPattern(
	apply(
		seq(
			MULT,
			rep_sc(
				seq(
					alt_sc(
						tok(TokenKind.SYMBOL_PLUS),
						tok(TokenKind.SYMBOL_MINUS),
					),
					MULT
				)
			)
		),
		applyAdd
	)
);

IN.setPattern(
	apply(
		alt_sc(
			seq(
				ADD,
				alt_sc(
					tok(TokenKind.KEYWORD_IN),
					tok(TokenKind.KEYWORD_NOTIN)
				),
				ADD
			),
			ADD
		),
		applyIn
	)
);

RELATION.setPattern(
	apply(
		seq(
			IN,
			rep_sc(
				seq(
					alt_sc(
						tok(TokenKind.SYMBOL_LTHAN),
						tok(TokenKind.SYMBOL_GTHAN),
						tok(TokenKind.SYMBOL_LTHANEQ),
						tok(TokenKind.SYMBOL_GTHANEQ)
					),
					IN
				)
			)
		),
		applyRelation
	)
);

EQUAL.setPattern(
	apply(
		seq(
			RELATION,
			rep_sc(
				seq(
					alt_sc(
						tok(TokenKind.SYMBOL_EQUIV),
						tok(TokenKind.SYMBOL_NEQUIV),
					),
					RELATION
				)
			)
		),
		applyEqual
	)
);

AND.setPattern(
	apply(
		seq(
			EQUAL,
			rep_sc(
				seq(
					tok(TokenKind.KEYWORD_AND),
					EQUAL
				)
			)
		),
		applyAnd
	)
);

OR.setPattern(
	apply(
		seq(
			AND,
			rep_sc(
				seq(
					tok(TokenKind.KEYWORD_OR),
					AND
				)
			)
		),
		applyOr
	)
);

IS.setPattern(
	apply(
		alt_sc(
			seq(
				OR,
				tok(TokenKind.KEYWORD_IS),
				OR
			),
			OR
		),
		applyIs
	)
);

TUPLE.setPattern(
	apply(
		list_sc(
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				IS,  // TODO: ASSIGNMENT
				opt_sc(tok(TokenKind.NEWLINE))
			),
			tok(TokenKind.SYMBOL_COMMA)),
		applyTuple
	)
);

EXPRESSION.setPattern(
	IS
);

IMPORT.setPattern(
	alt_sc(
		apply(
			kright(
				tok(TokenKind.KEYWORD_IMPORT),
				tok(TokenKind.IDENTIFIER)
			),
			applyImportStat
		),
		apply(
			seq(
				kright(
					tok(TokenKind.KEYWORD_IMPORT),
					tok(TokenKind.LITERAL_STRING)
				),
				kright(
					tok(TokenKind.KEYWORD_AS),
					tok(TokenKind.IDENTIFIER)
				)
			),
			applyImportAsStat
		)
	)
);

STATEMENT.setPattern(
	kleft(
		alt_sc(
			IMPORT,
			EXPRESSION
		),
		opt_sc(tok(TokenKind.NEWLINE))
	),
);

MODULE.setPattern(
	apply(
		rep_sc(STATEMENT),
		applyModule
	)
);