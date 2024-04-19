import * as parsec from 'typescript-parsec';
import { rep_sc, rule } from 'typescript-parsec';
import { alt_sc, apply, kleft, kmid, kright, list_sc, nil, opt_sc, seq, tok } from 'typescript-parsec';
import { TokenKind } from './tokenizer';
import { ArrayExpr, ArrayIndexExpr, BoolExpr, ConstantExpr, EmptyParensExpr, Expression, FloatExpr, FunctionCallExpr, IdentifierExpr, IntExpr, MapEntryExpr, MapExpr, MemberAccessExpr, NamedArgExpr, NewExpr, ParamExpr, PostfixExpr, PostfixExpr1, PrimaryExpr, RangeExpr, StringExpr, TupleExpr, UnaryExpr, isPrimaryExpr, BinaryExpr, MultExpr, AddExpr, InExpr, RelationExpr, EqualExpr, AndExpr, OrExpr, IsExpr, ConditionExpr, AssignExpr, AssignTupleExpr, AssignArrayExpr, AssignLhsExpr, TupleChainExpr, ParensExpr, AnonExpr, AnnotationExpr } from './expressions';
import { Statement, Module, ImportStat, ForeachStat, ForStat, WhileStat, IterStat, CompoundStat, SelectStat, ExitStat, RaiseStat, TryStat, JumpStat, FunctionStat, MethodStat, FieldStat, StaticStat, ClassStat, ClassMemberStat } from './statements';

type Token = parsec.Token<TokenKind>;

function applyIdentifier(value: Token): IdentifierExpr {
	return {
		kind: 'IdentifierExpr',
		token: value
	};
}

function applyBool(value: Token): BoolExpr {
	return {
		kind: 'BoolExpr',
		token: value,
		value: (/True/i).test(value.text)
	};
}

function applyInt(value: Token): IntExpr {
	return {
		kind: 'IntExpr',
		token: value,
		value: Number(value.text)
	};
}

function applyFloat(value: Token): FloatExpr {
	return {
		kind: 'FloatExpr',
		token: value,
		value: Number(value.text)
	};
}

function applyString(value: Token): StringExpr {
	return {
		kind: 'StringExpr',
		token: value,
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
	if (exprs.length == 1) {
		return exprs[0];
	}
	return {
		kind: 'TupleChainExpr',
		values: exprs
	};
}

function applyArray(tupleExpr: TupleExpr): ArrayExpr {
	const values =
		tupleExpr.kind === 'TupleChainExpr'
			? tupleExpr.values
			: [tupleExpr];
	return {
		kind: 'ArrayExpr',
		values: values
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
		('kind' in expr && expr.kind === 'TupleChainExpr')
			? (expr as TupleChainExpr).values
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

function applyParens(expr: [Token, TupleExpr, Token]): ParensExpr {
	const [lparen, tupExpr, rparen] = expr;
	return {
		kind: 'ParensExpr',
		expr: tupExpr,
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

function applyCondition(
	expr:
		[Token, IsExpr | OrExpr, Token, ConditionExpr, Token, ConditionExpr] |
		[Token, IsExpr | OrExpr, Token, ConditionExpr] |
		IsExpr | OrExpr
): ConditionExpr {
	if ('kind' in expr) {
		return expr;
	}
	if (expr.length == 6) {
		const [_if, cond, _then, ifTrue, _else, ifFalse] = expr;
		return {
			kind: 'ConditionBaseExpr',
			if: _if,
			condition: cond,
			then: _then,
			ifTrue: ifTrue,
			else: _else,
			ifFalse: ifFalse,
		};
	}
	// Must be length 4.
	const [_if, cond, _then, ifTrue] = expr;
	return {
		kind: 'ConditionBaseExpr',
		if: _if,
		condition: cond,
		then: _then,
		ifTrue: ifTrue,
	};
}

function applyAssignTuple(expr: [Token, AssignLhsExpr[], Token]): AssignTupleExpr {
	const [lparen, assigns, rparen] = expr;
	return {
		kind: 'AssignTupleExpr',
		assigns: assigns,
		lparen: lparen,
		rparen: rparen
	};
}

function applyAssignArray(expr: [Token, AssignLhsExpr[], Token]): AssignArrayExpr {
	const [lbrack, assigns, rbrack] = expr;
	return {
		kind: 'AssignArrayExpr',
		assigns: assigns,
		lbrack: lbrack,
		rbrack: rbrack
	};
}

function applyAssignLhs(
	expr:
		AssignTupleExpr |
		AssignArrayExpr |
		IdentifierExpr
): AssignLhsExpr {
	return expr;
}

function applyAssign(
	expr:
		[AssignLhsExpr, Token, ConditionExpr] |
		ConditionExpr
): AssignExpr {
	if ('kind' in expr) {
		return expr as ConditionExpr;
	}
	const [lhs, eq, rhs] = expr;
	return {
		kind: 'AssignBaseExpr',
		lhs: lhs,
		rhs: rhs,
		eq: eq
	};
}

function applyForeach(
	expr:
		[Token, Token, AssignLhsExpr, Token, ConditionExpr, Token, Statement] |
		[Token, AssignLhsExpr, Token, ConditionExpr, Statement]
): ForeachStat {
	if (expr.length == 7) {
		const [forTok, lparen, lhs, inTok, iter, rparen, stat] = expr;
		return {
			kind: 'ForeachStat',
			forTok: forTok,
			lparen: lparen,
			lhs: lhs,
			inTok: inTok,
			iter: iter,
			rparen: rparen,
			stat: stat
		};
	}
	const [forTok, lhs, inTok, iter, stat] = expr;
	return {
		kind: 'ForeachStat',
		forTok: forTok,
		lhs: lhs,
		inTok: inTok,
		iter: iter,
		stat: stat
	};
}

function applyFor(
	expr:
		[Token, Token, AssignExpr, Token, ConditionExpr, Token, AssignExpr, Token, Statement] |
		[Token, AssignExpr, Token, ConditionExpr, Token, AssignExpr, Statement]
): ForStat {
	if (expr.length == 9) {
		const [forTok, lparen, first, comma1, second, comma2, third, rparen, stat] = expr;
		return {
			kind: 'ForStat',
			forTok: forTok,
			lparen: lparen,
			first: first,
			comma1: comma1,
			second: second,
			comma2: comma2,
			third: third,
			rparen: rparen,
			stat: stat
		};
	}
	const [forTok, first, comma1, second, comma2, third, stat] = expr;
	return {
		kind: 'ForStat',
		forTok: forTok,
		first: first,
		comma1: comma1,
		second: second,
		comma2: comma2,
		third: third,
		stat: stat
	};
}

function applyWhile(
	expr:
		[Token, Token, ConditionExpr, Token, Statement] |
		[Token, ConditionExpr, Statement]
): WhileStat {
	if (expr.length == 5) {
		const [whileTok, lparen, cond, rparen, stat] = expr;
		return {
			kind: 'WhileStat',
			whileTok: whileTok,
			lparen: lparen,
			cond: cond,
			rparen: rparen,
			stat: stat
		};
	}
	const [whileTok, cond, stat] = expr;
	return {
		kind: 'WhileStat',
		whileTok: whileTok,
		cond: cond,
		stat: stat
	};
}

function applyCompound(expr: [Token, Statement[], Token]): CompoundStat {
	const [lbrace, stats, rbrace] = expr;
	return {
		kind: 'CompoundStat',
		lbrace: lbrace,
		stats: stats,
		rbrace: rbrace
	};
}

function applySelect(
	expr: [Token, TupleExpr, Statement, [Token, Statement]?]
): SelectStat {
	if (expr[3] !== undefined) {
		const [ifTok, cond, ifTrue, [elseTok, ifFalse]] = expr;
		return {
			kind: 'SelectStat',
			ifTok: ifTok,
			cond: cond,
			ifTrue: ifTrue,
			elseTok: elseTok,
			ifFalse: ifFalse
		};
	}
	const [ifTok, cond, ifTrue] = expr;
	return {
		kind: 'SelectStat',
		ifTok: ifTok,
		cond: cond,
		ifTrue: ifTrue
	};
}

function applyExit(expr: [Token, AssignExpr?]): ExitStat {
	const [exit, val] = expr;
	return {
		kind: 'ExitStat',
		exit: exit,
		expr: val
	};
}

function applyRaise(expr: [Token, AssignExpr]): RaiseStat {
	const [raise, val] = expr;
	return {
		kind: 'RaiseStat',
		raise: raise,
		expr: val
	};
}

function applyTry(
	expr: [Token, Statement, Token, AssignLhsExpr, Statement]
): TryStat {
	const [tryTok, stat, catchTok, catchAssign, catchStat] = expr;
	return {
		kind: 'TryStat',
		tryTok: tryTok,
		stat: stat,
		catchTok: catchTok,
		catchAssign: catchAssign,
		catchStat: catchStat
	};
}

function applyJump(
	expr: [Token, TupleExpr] | Token
): JumpStat {
	if ('text' in expr) {
		return {
			kind: 'JumpStat',
			token: expr as Token
		};
	}
	const [ret, retVal] = expr;
	return {
		kind: 'JumpStat',
		token: ret,
		expr: retVal
	};
}

function applyParam(
	expr: [Token, IdentifierExpr, [Token, ConditionExpr]]
): ParamExpr {
	const [field, name, defaultValExpr] = expr;
	if (defaultValExpr !== undefined) {
		const [eq, defaultVal] = defaultValExpr;
		return {
			kind: 'ParamExpr',
			field: field,
			name: name,
			eq: eq,
			defaultValue: defaultVal
		};
	}
	return {
		kind: 'ParamExpr',
		name: name,
		field: field
	};
}

function applyAnnotation(expr: [Token, PostfixExpr]): AnnotationExpr {
	const [at, pfExpr] = expr;
	return {
		kind: 'AnnotationExpr',
		at: at,
		expr: pfExpr
	};
}

function applyFunction(
	expr: [
		AnnotationExpr[], Token, IdentifierExpr, Token,
		[Token, ParamExpr[], Token] | ParamExpr[],
		Token, Token, Statement]
): FunctionStat {
	const [annots, defTok, name, lparen, paramsExpr, rparen, asyncTok, stat] = expr;
	if (paramsExpr === undefined) {
		return {
			kind: 'FunctionStat',
			defTok: defTok,
			name: name,
			lparen: lparen,
			rparen: rparen,
			params: [],
			isNamed: true,
			asyncTok: asyncTok,
			stat: stat,
			annots: annots === undefined ? [] : annots
		};
	}
	if (paramsExpr.length == 3 && paramsExpr[1] instanceof Array) {
		const [_, params, __] = paramsExpr;
		return {
			kind: 'FunctionStat',
			defTok: defTok,
			name: name,
			lparen: lparen,
			rparen: rparen,
			params: params,
			isNamed: true,
			asyncTok: asyncTok,
			stat: stat,
			annots: annots === undefined ? [] : annots
		};
	}
	return {
		kind: 'FunctionStat',
		defTok: defTok,
		name: name,
		lparen: lparen,
		rparen: rparen,
		params: paramsExpr as ParamExpr[],
		isNamed: false,
		asyncTok: asyncTok,
		stat: stat,
		annots: annots === undefined ? [] : annots
	};
}

function applyMethod(
	expr: [
		AnnotationExpr[],
		[Token, IdentifierExpr] | NewExpr, Token,
		[Token, ParamExpr[], Token] | ParamExpr[],
		Token, Token, Statement]
): MethodStat {
	const [annots, defExpr, lparen, paramsExpr, rparen, asyncTok, stat] = expr;
	let methodTok, name;
	if ('kind' in defExpr && defExpr.kind === 'NewExpr') {
		methodTok = undefined;
		name = defExpr;
	} else {
		methodTok = defExpr[0];
		name = defExpr[1];
	}
	if (paramsExpr === undefined) {
		return {
			kind: 'MethodStat',
			methodTok: methodTok,
			name: name,
			lparen: lparen,
			rparen: rparen,
			params: [],
			isNamed: true,
			asyncTok: asyncTok,
			stat: stat,
			annots: annots === undefined ? [] : annots
		};
	} else if (paramsExpr.length == 3 && (paramsExpr[1] instanceof Array || paramsExpr[1] === undefined)) {
		const [_, params, __] = paramsExpr;
		return {
			kind: 'MethodStat',
			methodTok: methodTok,
			name: name,
			lparen: lparen,
			rparen: rparen,
			params: params as ParamExpr[],
			isNamed: true,
			asyncTok: asyncTok,
			stat: stat,
			annots: annots === undefined ? [] : annots
		};
	}
	return {
		kind: 'MethodStat',
		methodTok: methodTok,
		name: name,
		lparen: lparen,
		rparen: rparen,
		params: paramsExpr as ParamExpr[],
		isNamed: false,
		asyncTok: asyncTok,
		stat: stat,
		annots: annots === undefined ? [] : annots
	};
}

function applyAnon(
	expr: [
		Token, [Token, ParamExpr[], Token] | ParamExpr[],
		Token, Token, [Token, AssignExpr] | CompoundStat]
): AnonExpr {
	const [lparen, paramsExpr, rparen, asyncTok, body] = expr;

	let rhs: AssignExpr | CompoundStat;
	let arrow: Token;
	if ('kind' in body && body.kind === 'CompoundStat') {
		rhs = body;
		arrow = undefined;
	} else {
		rhs = body[1];
		arrow = body[0];
	}
	if (paramsExpr === undefined) {
		return {
			kind: 'AnonExpr',
			lparen: lparen,
			rparen: rparen,
			params: [],
			isNamed: true,
			asyncTok: asyncTok,
			arrow: arrow,
			expr: rhs
		};
	} else if (paramsExpr.length == 3 && paramsExpr[1] instanceof Array) {
		const [_, params, __] = paramsExpr;
		return {
			kind: 'AnonExpr',
			lparen: lparen,
			rparen: rparen,
			params: params,
			isNamed: true,
			asyncTok: asyncTok,
			arrow: arrow,
			expr: rhs
		};
	}
	return {
		kind: 'AnonExpr',
		lparen: lparen,
		rparen: rparen,
		params: paramsExpr as ParamExpr[],
		isNamed: false,
		asyncTok: asyncTok,
		arrow: arrow,
		expr: rhs
	};
}

function applyField(expr: [Token, IdentifierExpr[]]): FieldStat {
	const [fieldTok, fields] = expr;
	return {
		kind: 'FieldStat',
		fieldTok: fieldTok,
		fields: fields
	};
}

function applyStatic(
	expr: [Token, IdentifierExpr, Token, ConditionExpr]
): StaticStat {
	const [staticTok, name, eq, condExpr] = expr;
	return {
		kind: 'StaticStat',
		staticTok: staticTok,
		name: name,
		eq: eq,
		expr: condExpr
	};
}

function applyClass(expr: [
	AnnotationExpr[], Token, IdentifierExpr,
	[Token, PostfixExpr],
	[Token, ClassMemberStat[], Token] | ClassMemberStat
]): ClassStat {
	const [annots, classTok, name, superExpr, body] = expr;
	let colon, superClass;
	if (superExpr === undefined) {
		colon = undefined;
		superClass = undefined;
	} else {
		colon = superExpr[0];
		superClass = superExpr[1];
	}
	if ('kind' in body) {
		return {
			kind: 'ClassStat',
			classTok: classTok,
			name: name,
			colon: colon,
			super: superClass,
			stats: [body],
			annots: annots === undefined ? [] : annots
		};
	}
	const [lbrace, stats, rbrace] = body;
	return {
		kind: 'ClassStat',
		classTok: classTok,
		name: name,
		colon: colon,
		super: superClass,
		lbrace: lbrace,
		rbrace: rbrace,
		stats: stats,
		annots: annots === undefined ? [] : annots
	};
}

function applyImportStat(expr: [Token, IdentifierExpr | StringExpr]): ImportStat {
	const [importTok, name] = expr;
	return {
		kind: 'ImportStat',
		source: name,
		importTok: importTok
	};
}

function applyImportAsStat(value: [Token, IdentifierExpr | StringExpr, Token, IdentifierExpr]): ImportStat {
	const [importTok, source, asTok, name] = value;
	return {
		kind: 'ImportAsStat',
		name: name,
		source: source,
		importTok: importTok,
		asTok: asTok
	};
}

function applyModule(value: Statement[]): Module {
	return {
		statements: value
	};
}

// Expressions

export const EXPRESSION = rule<TokenKind, Expression>();

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
export const PARENS = rule<TokenKind, ParensExpr>();
export const EMPTY_PARENS = rule<TokenKind, EmptyParensExpr>();

// Postfix
export const POSTFIX = rule<TokenKind, PostfixExpr>();
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
export const CONDITION = rule<TokenKind, ConditionExpr>();

// Assignment
export const ASSIGN = rule<TokenKind, AssignExpr>();
export const ASSIGN_TUPLE = rule<TokenKind, AssignTupleExpr>();
export const ASSIGN_ARRAY = rule<TokenKind, AssignArrayExpr>();
export const ASSIGN_LHS = rule<TokenKind, AssignLhsExpr>();

// Statements

export const STATEMENT = rule<TokenKind, Statement>();

// Iterative
export const FOREACH = rule<TokenKind, ForeachStat>();
export const FOR = rule<TokenKind, ForStat>();
export const WHILE = rule<TokenKind, WhileStat>();
export const ITER = rule<TokenKind, IterStat>();

export const COMPOUND = rule<TokenKind, CompoundStat>();
export const SELECT = rule<TokenKind, SelectStat>();
export const EXIT = rule<TokenKind, ExitStat>();
export const RAISE = rule<TokenKind, RaiseStat>();
export const TRY = rule<TokenKind, TryStat>();
export const JUMP = rule<TokenKind, JumpStat>();

export const FUNCTION = rule<TokenKind, FunctionStat>();
export const PARAM = rule<TokenKind, ParamExpr>();
export const ANON = rule<TokenKind, AnonExpr>();
export const ANNOTATION = rule<TokenKind, AnnotationExpr>();

export const CLASS = rule<TokenKind, ClassStat>();
export const METHOD = rule<TokenKind, MethodStat>();
export const FIELD = rule<TokenKind, FieldStat>();
export const STATIC = rule<TokenKind, StaticStat>();

export const IMPORT = rule<TokenKind, ImportStat>();

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

PARENS.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LPAREN),
			TUPLE,
			tok(TokenKind.SYMBOL_RPAREN)
		),
		applyParens
	)
);

PRIMARY.setPattern(
	alt_sc(
		ANON,
		IDENTIFIER,
		NEW,
		CONSTANT,
		STRING,
		ARRAY,
		MAP,
		PARENS
	)
);

PRIMARY_NO_CONSTANTS.setPattern(
	alt_sc(
		ANON,
		IDENTIFIER,
		STRING,
		ARRAY,
		MAP,
		PARENS
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
				CONDITION,
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


POSTFIX.setPattern(
	apply(
		alt_sc(
			seq(
				PRIMARY_NO_CONSTANTS,
				kleft(
					rep_sc(
						alt_sc(
							ARRAY_INDEX,
							EMPTY_PARENS,
							FUNCTION_CALL,
							kright(
								opt_sc(tok(TokenKind.NEWLINE)),
								MEMBER_ACCESS
							)
						)
					),
					opt_sc(tok(TokenKind.NEWLINE))
				)
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

CONDITION.setPattern(
	apply(
		alt_sc(
			seq(
				tok(TokenKind.KEYWORD_IF),
				IS,
				opt_sc(tok(TokenKind.KEYWORD_THEN)),
				CONDITION,
				tok(TokenKind.KEYWORD_ELSE),
				CONDITION
			),
			seq(
				tok(TokenKind.KEYWORD_IF),
				IS,
				opt_sc(tok(TokenKind.KEYWORD_THEN)),
				CONDITION,
			),
			IS
		),
		applyCondition
	)
);

ASSIGN_TUPLE.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LPAREN),
			list_sc(ASSIGN_LHS, tok(TokenKind.SYMBOL_COMMA)),
			tok(TokenKind.SYMBOL_RPAREN)
		),
		applyAssignTuple
	)
);

ASSIGN_ARRAY.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LBRACKET),
			list_sc(ASSIGN_LHS, tok(TokenKind.SYMBOL_COMMA)),
			tok(TokenKind.SYMBOL_RBRACKET)
		),
		applyAssignArray
	)
);

ASSIGN_LHS.setPattern(
	apply(
		alt_sc(
			ASSIGN_TUPLE,
			ASSIGN_ARRAY,
			POSTFIX,
			IDENTIFIER
		),
		applyAssignLhs
	)
);

ASSIGN.setPattern(
	apply(
		alt_sc(
			seq(
				ASSIGN_LHS,
				tok(TokenKind.SYMBOL_EQUALS),
				CONDITION
			),
			CONDITION
		),
		applyAssign
	)
);

TUPLE.setPattern(
	apply(
		list_sc(
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				ASSIGN,
				opt_sc(tok(TokenKind.NEWLINE))
			),
			tok(TokenKind.SYMBOL_COMMA)),
		applyTuple
	)
);

EXPRESSION.setPattern(
	kleft(
		TUPLE,
		opt_sc(tok(TokenKind.NEWLINE))
	)
);

FOREACH.setPattern(
	apply(
		alt_sc(
			seq(
				tok(TokenKind.KEYWORD_FOR),
				tok(TokenKind.SYMBOL_LPAREN),
				ASSIGN_LHS,
				tok(TokenKind.KEYWORD_IN),
				CONDITION,
				tok(TokenKind.SYMBOL_RPAREN),
				STATEMENT
			),
			seq(
				tok(TokenKind.KEYWORD_FOR),
				ASSIGN_LHS,
				tok(TokenKind.KEYWORD_IN),
				CONDITION,
				STATEMENT
			),
		),
		applyForeach
	)
);

FOR.setPattern(
	apply(
		alt_sc(
			seq(
				tok(TokenKind.KEYWORD_FOR),
				tok(TokenKind.SYMBOL_LPAREN),
				ASSIGN,
				tok(TokenKind.SYMBOL_COMMA),
				CONDITION,
				tok(TokenKind.SYMBOL_COMMA),
				ASSIGN,
				tok(TokenKind.SYMBOL_RPAREN),
				STATEMENT
			),
			seq(
				tok(TokenKind.KEYWORD_FOR),
				ASSIGN,
				tok(TokenKind.SYMBOL_COMMA),
				CONDITION,
				tok(TokenKind.SYMBOL_COMMA),
				ASSIGN,
				STATEMENT
			),
		),
		applyFor
	)
);

WHILE.setPattern(
	apply(
		alt_sc(
			seq(
				tok(TokenKind.KEYWORD_WHILE),
				tok(TokenKind.SYMBOL_LPAREN),
				CONDITION,
				tok(TokenKind.SYMBOL_RPAREN),
				STATEMENT
			),
			seq(
				tok(TokenKind.KEYWORD_WHILE),
				CONDITION,
				STATEMENT
			),
		),
		applyWhile
	)
);

ITER.setPattern(
	alt_sc(
		FOREACH,
		FOR,
		WHILE
	)
);

COMPOUND.setPattern(
	apply(
		seq(
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				tok(TokenKind.SYMBOL_LBRACE),
				opt_sc(tok(TokenKind.NEWLINE))
			),
			rep_sc(STATEMENT),
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				tok(TokenKind.SYMBOL_RBRACE),
				opt_sc(tok(TokenKind.NEWLINE))
			)
		),
		applyCompound
	)
);

SELECT.setPattern(
	apply(
		seq(
			tok(TokenKind.KEYWORD_IF),
			TUPLE,
			STATEMENT,
			opt_sc(
				seq(
					tok(TokenKind.KEYWORD_ELSE),
					STATEMENT
				)
			)
		),
		applySelect
	)
);

EXIT.setPattern(
	apply(
		seq(
			tok(TokenKind.KEYWORD_EXIT),
			opt_sc(ASSIGN)
		),
		applyExit
	)
);

RAISE.setPattern(
	apply(
		seq(
			tok(TokenKind.KEYWORD_RAISE),
			ASSIGN
		),
		applyRaise
	)
);

TRY.setPattern(
	apply(
		seq(
			tok(TokenKind.KEYWORD_TRY),
			STATEMENT,
			tok(TokenKind.KEYWORD_CATCH),
			alt_sc(
				kmid(
					tok(TokenKind.SYMBOL_LPAREN),
					ASSIGN_LHS,
					tok(TokenKind.SYMBOL_RPAREN)
				),
				ASSIGN_LHS
			),
			STATEMENT
		),
		applyTry
	)
);

JUMP.setPattern(
	apply(
		kmid(
			opt_sc(tok(TokenKind.NEWLINE)),
			alt_sc(
				seq(
					tok(TokenKind.KEYWORD_RETURN),
					alt_sc(
						kmid(
							tok(TokenKind.SYMBOL_LPAREN),
							TUPLE,
							tok(TokenKind.SYMBOL_RPAREN)
						),
						TUPLE
					)
				),
				tok(TokenKind.KEYWORD_RETURN),
				tok(TokenKind.KEYWORD_BREAK),
				tok(TokenKind.KEYWORD_CONTINUE)
			),
			opt_sc(tok(TokenKind.NEWLINE))
		),
		applyJump
	)
);

PARAM.setPattern(
	apply(
		kmid(
			opt_sc(tok(TokenKind.NEWLINE)),
			seq(
				opt_sc(tok(TokenKind.KEYWORD_FIELD)),
				IDENTIFIER,
				opt_sc(
					seq(
						tok(TokenKind.SYMBOL_EQUALS),
						CONDITION
					)
				)
			),
			opt_sc(tok(TokenKind.NEWLINE))
		),
		applyParam
	)
);

ANNOTATION.setPattern(
	apply(
		kmid(
			opt_sc(tok(TokenKind.NEWLINE)),
			seq(
				tok(TokenKind.SYMBOL_AT),
				POSTFIX
			),
			opt_sc(tok(TokenKind.NEWLINE))
		),
		applyAnnotation
	)
);

FUNCTION.setPattern(
	apply(
		seq(
			opt_sc(rep_sc(ANNOTATION)),
			alt_sc(
				tok(TokenKind.KEYWORD_DEF),
				tok(TokenKind.KEYWORD_FUNCTION)
			),
			IDENTIFIER,
			tok(TokenKind.SYMBOL_LPAREN),
			alt_sc(
				seq(
					tok(TokenKind.SYMBOL_LBRACE),
					list_sc(PARAM, tok(TokenKind.SYMBOL_COMMA)),
					tok(TokenKind.SYMBOL_RBRACE)
				),
				list_sc(PARAM, tok(TokenKind.SYMBOL_COMMA))
			),
			tok(TokenKind.SYMBOL_RPAREN),
			opt_sc(tok(TokenKind.KEYWORD_ASYNC)),
			STATEMENT
		),
		applyFunction
	)
);

METHOD.setPattern(
	apply(
		seq(
			opt_sc(rep_sc(ANNOTATION)),
			alt_sc(
				seq(
					tok(TokenKind.KEYWORD_METHOD),
					IDENTIFIER
				),
				NEW
			),
			tok(TokenKind.SYMBOL_LPAREN),
			alt_sc(
				seq(
					tok(TokenKind.SYMBOL_LBRACE),
					list_sc(PARAM, tok(TokenKind.SYMBOL_COMMA)),
					tok(TokenKind.SYMBOL_RBRACE)
				),
				list_sc(PARAM, tok(TokenKind.SYMBOL_COMMA)),
				nil()
			),
			tok(TokenKind.SYMBOL_RPAREN),
			opt_sc(tok(TokenKind.KEYWORD_ASYNC)),
			STATEMENT
		),
		applyMethod
	)
);

ANON.setPattern(
	apply(
		seq(
			tok(TokenKind.SYMBOL_LPAREN),
			alt_sc(
				seq(
					tok(TokenKind.SYMBOL_LBRACE),
					list_sc(PARAM, tok(TokenKind.SYMBOL_COMMA)),
					tok(TokenKind.SYMBOL_RBRACE)
				),
				list_sc(PARAM, tok(TokenKind.SYMBOL_COMMA)),
				nil()
			),
			tok(TokenKind.SYMBOL_RPAREN),
			opt_sc(tok(TokenKind.KEYWORD_ASYNC)),
			alt_sc(
				seq(
					tok(TokenKind.SYMBOL_RARROW),
					ASSIGN
				),
				COMPOUND
			)
		),
		applyAnon
	)
);

FIELD.setPattern(
	apply(
		seq(
			tok(TokenKind.KEYWORD_FIELD),
			list_sc(IDENTIFIER, tok(TokenKind.SYMBOL_COMMA))
		),
		applyField
	)
);

STATIC.setPattern(
	apply(
		seq(
			tok(TokenKind.KEYWORD_STATIC),
			IDENTIFIER,
			tok(TokenKind.SYMBOL_EQUALS),
			CONDITION
		),
		applyStatic
	)
);

CLASS.setPattern(
	apply(
		seq(
			opt_sc(rep_sc(ANNOTATION)),
			tok(TokenKind.KEYWORD_CLASS),
			IDENTIFIER,
			opt_sc(
				seq(
					tok(TokenKind.SYMBOL_COLON),
					POSTFIX
				)
			),
			alt_sc(
				seq(
					kmid(
						opt_sc(tok(TokenKind.NEWLINE)),
						tok(TokenKind.SYMBOL_LBRACE),
						opt_sc(tok(TokenKind.NEWLINE))
					),
					rep_sc(
						kmid(
							opt_sc(tok(TokenKind.NEWLINE)),
							alt_sc(
								METHOD,
								FIELD,
								STATIC
							),
							opt_sc(tok(TokenKind.NEWLINE))
						)
					),
					kmid(
						opt_sc(tok(TokenKind.NEWLINE)),
						tok(TokenKind.SYMBOL_RBRACE),
						opt_sc(tok(TokenKind.NEWLINE))
					),
				),
				kmid(
					opt_sc(tok(TokenKind.NEWLINE)),
					alt_sc(
						METHOD,
						FIELD,
						STATIC
					),
					opt_sc(tok(TokenKind.NEWLINE))
				)
			)
		),
		applyClass
	)
);

IMPORT.setPattern(
	alt_sc(
		apply(
			seq(
				tok(TokenKind.KEYWORD_IMPORT),
				alt_sc(
					IDENTIFIER,
					STRING
				)
			),
			applyImportStat
		),
		apply(
			seq(
				tok(TokenKind.KEYWORD_IMPORT),
				STRING,
				tok(TokenKind.KEYWORD_AS),
				IDENTIFIER
			),
			applyImportAsStat
		)
	)
);

STATEMENT.setPattern(
	kmid(
		opt_sc(tok(TokenKind.NEWLINE)),
		alt_sc(
			EXIT,
			RAISE,
			TRY,
			SELECT,
			COMPOUND,
			ITER,
			JUMP,
			EXPRESSION
		),
		opt_sc(tok(TokenKind.NEWLINE))
	),
);

MODULE.setPattern(
	apply(
		rep_sc(
			kmid(
				opt_sc(tok(TokenKind.NEWLINE)),
				alt_sc(
					IMPORT,
					CLASS,
					FUNCTION,
					EXIT,
					RAISE,
					TRY,
					SELECT,
					COMPOUND,
					ITER,
					EXPRESSION
				),
				opt_sc(tok(TokenKind.NEWLINE))
			)
		),
		applyModule
	)
);