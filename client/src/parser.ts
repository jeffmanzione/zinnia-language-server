import * as parsec from 'typescript-parsec';
import { Parser, rep_sc, rule } from 'typescript-parsec';
import { alt, alt_sc, apply, kleft, kmid, kright, list, list_sc, lrec_sc, opt_sc, seq, str, tok } from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { ArrayExpr, ArrayIndexExpr, BoolExpr, Expression, FloatExpr, FunctionCallExpr, IdentifierExpr, IntExpr, MapEntryExpr, MapExpr, MemberAccessExpr, NamedArgExpr, NewExpr, StringExpr, TupleExpr } from './expressions';
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

function applyMapEntry(entry: [Expression, Token, Expression]): MapEntryExpr {
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

function applyFunctionCall(entries: [Token, (TupleExpr | NamedArgExpr[]), Token]): FunctionCallExpr {
	const [lparen, expr, rparen] = entries;
	const args: Expression[] =
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
export const CONSTANT = rule<TokenKind, Expression>();
export const STRING = rule<TokenKind, StringExpr>();

// Structures
export const ARRAY = rule<TokenKind, ArrayExpr>();
export const MAP_ENTRY = rule<TokenKind, MapEntryExpr>();
export const MAP = rule<TokenKind, MapExpr>();
export const TUPLE = rule<TokenKind, TupleExpr>();

// Primary
export const PRIMARY = rule<TokenKind, Expression>();
export const PRIMARY_NO_CONSTANTS = rule<TokenKind, Expression>();
export const EMPTY_PARENS = rule<TokenKind, Token[]>();

// Postfix

// Conditionals
export const CONDITIONAL = rule<TokenKind, Expression>();

// Postfix
export const POSTFIX = rule<TokenKind, Expression>();
export const NAMED_ARG = rule<TokenKind, NamedArgExpr>();
export const FUNCTION_CALL = rule<TokenKind, FunctionCallExpr>();
export const ARRAY_INDEX = rule<TokenKind, ArrayIndexExpr>();
export const MEMBER_ACCESS = rule<TokenKind, MemberAccessExpr>();

export const EXPRESSION = rule<TokenKind, Expression>();


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
	seq(
		tok(TokenKind.SYMBOL_LPAREN),
		tok(TokenKind.SYMBOL_RPAREN)
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
				CONDITIONAL,
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

TUPLE.setPattern(
	apply(
		list_sc(PRIMARY, tok(TokenKind.SYMBOL_COMMA)),
		applyTuple
	)
);

EXPRESSION.setPattern(
	alt_sc(
		ARRAY,
		TUPLE,
		CONSTANT,
		IDENTIFIER
	)
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