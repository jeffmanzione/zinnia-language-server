import * as parsec from 'typescript-parsec';
import { Parser, rep_sc, rule } from 'typescript-parsec';
import { alt, alt_sc, apply, kleft, kmid, kright, list, list_sc, lrec_sc, opt_sc, seq, str, tok } from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { ArrayExpr, BoolExpr, Expression, FloatExpr, IdentifierExpr, IntExpr, StringExpr, TupleExpr } from './expressions';
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

export const IDENTIFIER = rule<TokenKind, IdentifierExpr>();
export const CONSTANT = rule<TokenKind, Expression>();
export const STRING = rule<TokenKind, StringExpr>();
export const PRIMARY = rule<TokenKind, Expression>();
export const TUPLE = rule<TokenKind, TupleExpr>();
export const ARRAY = rule<TokenKind, ArrayExpr>();
export const EXPRESSION = rule<TokenKind, Expression>();

export const IMPORT = rule<TokenKind, ImportStat>();
export const STATEMENT = rule<TokenKind, Statement>();
export const MODULE = rule<TokenKind, Module>();

IDENTIFIER.setPattern(
	apply(
		tok(TokenKind.IDENTIFIER),
		applyIdentifier
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

PRIMARY.setPattern(
	alt_sc(
		IDENTIFIER,
		CONSTANT,
		STRING,
		kmid(
			tok(TokenKind.SYMBOL_LPAREN),
			TUPLE,
			tok(TokenKind.SYMBOL_RPAREN)
		)
	)
);

TUPLE.setPattern(
	apply(
		list_sc(PRIMARY, tok(TokenKind.SYMBOL_COMMA)),
		applyTuple
	)
);

ARRAY.setPattern(
	apply(
		kmid(
			tok(TokenKind.SYMBOL_LBRACKET),
			TUPLE,
			tok(TokenKind.SYMBOL_RBRACKET)
		),
		applyArray
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