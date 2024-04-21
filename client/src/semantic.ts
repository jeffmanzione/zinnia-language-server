import * as parsec from 'typescript-parsec';
import { ClassStat, CompoundStat, FieldStat, ForStat, ForeachStat, ImportStat, JumpStat, MethodStat, Module, SelectStat, Statement, StaticStat, WhileStat } from './statements';
import { TokenKind } from './tokenizer';
import { AssignArrayExpr, AssignBaseExpr, AssignLhsExpr, AssignTupleExpr, ConstantExpr, Expression, IdentifierExpr, NamedArgExpr, NewExpr, ParamExpr, PostfixChainExpr, PostfixExpr, PrimaryExpr, StringExpr, UnaryChainExpr, UnaryExpr, isConstantExpr, isPostfixExpr } from './expressions';

type Token = parsec.Token<TokenKind>;


export interface SemanticToken {
	text: string;
	col: number;
	row: number;
	type: string;
	modifiers: string[];
}

type IdType =
	| 'variable'
	| 'property'
	| 'parameter'
	| 'method'
	| 'class';

class SemanticIdentifier {
	ast?: IdentifierExpr;
	id: string;
	type: IdType;
	tokens: Token[] = [];
	modifiers: string[];
	tokensGenerated: boolean = false;

	constructor(ast: IdentifierExpr, type: IdType, modifiers: string[] = [], name?: string) {
		this.ast = ast;
		this.id = name == null ? ast.token.text : name;
		this.type = type;
		this.modifiers = modifiers;
	}

	addToken(token: Token): void {
		this.tokens.push(token);
	}

	hasGenerated(): boolean {
		return this.tokensGenerated;
	}

	markGenerated(): void { this.tokensGenerated = true; }
}

interface SemanticConstant {
	ast: ConstantExpr;
	text: string;
	value: boolean | number | null;
}

interface SemanticUnary {
	ast: UnaryChainExpr;
	rhs: SemanticExpression;
}

type SemanticExpression =
	| SemanticConstant
	| SemanticIdentifier
	| SemanticUnary
	| SemanticPostfix
	| SemanticAssign;

interface SemanticAssignLhs {
	ast: AssignLhsExpr;
	expr: SemanticIdentifier | SemanticPostfix | SemanticAssignLhs[];
}

type SemanticPrimary =
	| SemanticIdentifier;

interface SemanticPostfix {
	ast: PostfixChainExpr;
	base: SemanticPrimary;
	nestedExprs: (SemanticExpression | Token)[];
}

interface SemanticAssign {
	ast: AssignBaseExpr;
	lhs: SemanticAssignLhs;
	rhs: SemanticExpression;
}

interface SemanticImport {
	ast: ImportStat;
	name: SemanticIdentifier;
	source: string;
}

interface SemanticField {
	ast: FieldStat | ParamExpr;
	token: Token;
	name: SemanticIdentifier;
}

interface SemanticStatic {
	ast: StaticStat;
	name: string;
}

interface SemanticParam {
	ast: ParamExpr;
	name: SemanticIdentifier;
	isField: boolean;
}

interface SemanticMethod {
	ast: MethodStat;
	name: SemanticIdentifier;
	params: SemanticParam[];
	stat: SemanticStatement;
}

interface SemanticClass {
	ast: ClassStat;
	name: string;
	superName?: string;
	fields: SemanticField[];
	statics: Map<string, SemanticStatic>;
	methods: Map<string, SemanticMethod>;
}

interface SemanticCompound {
	ast: CompoundStat;
	stats: SemanticStatement[];
}

interface SemanticSelect {
	ast: SelectStat;
	cond: SemanticStatement;
	ifTrue: SemanticStatement;
	ifFalse?: SemanticStatement;
}

interface SemanticWhile {
	ast: WhileStat;
	cond: SemanticExpression; // Condition
	body: SemanticStatement;
}

interface SemanticFor {
	ast: ForStat;
	first: SemanticExpression; // Assignment
	second: SemanticExpression; // Condition
	third: SemanticExpression; // Assignment
	body: SemanticStatement;
}

interface SemanticForeach {
	ast: ForeachStat;
	lhs: SemanticAssignLhs; // Assignment
	rhs: SemanticExpression; // Condition
	body: SemanticStatement;
}

type SemanticIter =
	| SemanticWhile
	| SemanticFor
	| SemanticForeach;

interface SemanticJump {
	ast: JumpStat;
	expr?: SemanticExpression;
}

type SemanticStatement =
	| SemanticCompound
	| SemanticSelect
	| SemanticIter
	| SemanticJump
	| SemanticExpression;

interface SemanticModule {
	ast: Module;
	imports: SemanticImport[];
	classes: SemanticClass[];
	statements: SemanticStatement[];
}

class Block {
	private parent?: Block;
	private members: Map<string, SemanticIdentifier> = new Map();

	constructor(parent?: Block) { this.parent = parent; }

	createIdentifier(id: IdentifierExpr, type: IdType): SemanticIdentifier {
		const sid = new SemanticIdentifier(id, type);
		this.members.set(sid.id, sid);
		return sid;
	}

	lookupOrCreateIdentifier(id: IdentifierExpr | NewExpr | string, type: IdType, modifiers: string[] = []): SemanticIdentifier {
		const idText = typeof id === 'string' ? id : id.token.text;
		const foundId = this.findIdentifier(idText);
		if (foundId != null) {
			return foundId;
		}
		const sid = new SemanticIdentifier(
			typeof id === 'string'
				? { kind: 'IdentifierExpr', token: null }
				: ('kind' in id && id.kind === 'NewExpr')
					? { kind: 'IdentifierExpr', token: id.token }
					: id,
			type,
			modifiers,
			idText);
		this.members.set(sid.id, sid);
		return sid;
	}

	findIdentifier(id: string): SemanticIdentifier {
		if (this.members.has(id)) {
			return this.members.get(id);
		}
		if (this.parent != null) {
			return this.parent.findIdentifier(id);
		}
		return null;
	}
}

class SemanticContext {
	module: SemanticModule;
	block: Block;

	constructor(block: Block = new Block()) {
		this.block = block;
	}

	setModule(module: SemanticModule): void {
		this.module = module;
	}

	newBlock(): SemanticContext {
		const copy = new SemanticContext(new Block(this.block));
		copy.setModule(this.module);
		return copy;
	}
}

function createToken(token: Token, type: string, modifiers: string[] = []): SemanticToken {
	return {
		text: token.text,
		col: token.pos.columnBegin - 1,
		row: token.pos.rowBegin - 1,
		type: type,
		modifiers: modifiers
	};
}

function generateTokensForConstant(cnst: SemanticConstant, context: SemanticContext, tokens: SemanticToken[]): void {
	tokens.push(createToken(cnst.ast.token, 'number', ['constant']));
}

function generateTokensForUnary(unary: SemanticUnary, context: SemanticContext, tokens: SemanticToken[]): void {
	for (const un of unary.ast.unaries) {
		if (un.kind === TokenKind.KEYWORD_AWAIT) {
			tokens.push(createToken(un, 'keyword'));
		}
	}
	generateTokensForExpression(unary.rhs, context, tokens);
}

function generateTokensForIdentifier(id: SemanticIdentifier, context: SemanticContext, tokens: SemanticToken[]): void {
	if (id.hasGenerated()) {
		return;
	}
	for (const tok of id.tokens) {
		tokens.push(createToken(tok, id.type, id.modifiers));
	}
	id.markGenerated();
}


function generateTokensForAssign(asgn: SemanticAssign, context: SemanticContext, tokens: SemanticToken[]): void {
	generateTokensForAssignLhs(asgn.lhs, context, tokens);
	generateTokensForExpression(asgn.rhs, context, tokens);
}

function generateTokensForPostfix(pstfx: SemanticPostfix, context: SemanticContext, tokens: SemanticToken[]): void {
	if (pstfx.base != null) { // TODO remove check.
		generateTokensForIdentifier(pstfx.base, context, tokens);
	}
	for (const nestedExpr of pstfx.nestedExprs) {
		if (nestedExpr == null) {
			continue;
		}
		if ('nextToken' in nestedExpr) {
			const tok = nestedExpr as Token;
			tokens.push(createToken(tok, tok.next.kind === TokenKind.SYMBOL_LPAREN ? 'method' : 'property'));
		} else {
			generateTokensForExpression(nestedExpr as SemanticExpression, context, tokens);
		}
	}
}

function generateTokensForExpression(expr: SemanticExpression, context: SemanticContext, tokens: SemanticToken[]): void {
	if (expr == null || expr.ast == null) {
		return;
	}
	if (isConstantExpr(expr.ast)) {
		generateTokensForConstant(expr as SemanticConstant, context, tokens);
	} else if (expr.ast.kind === 'IdentifierExpr') {
		generateTokensForIdentifier(expr as SemanticIdentifier, context, tokens);
	} else if (expr.ast.kind === 'UnaryChainExpr') {
		generateTokensForUnary(expr as SemanticUnary, context, tokens);
	} else if (expr.ast.kind === 'AssignBaseExpr') {
		generateTokensForAssign(expr as SemanticAssign, context, tokens);
	} else if (expr.ast.kind === 'PostfixChainExpr') {
		generateTokensForPostfix(expr as SemanticPostfix, context, tokens);
	}
}

function generateTokensForAssignLhs(asgn: SemanticAssignLhs, context: SemanticContext, tokens: SemanticToken[]): void {
	if (asgn.ast.kind === 'IdentifierExpr') {
		generateTokensForExpression(asgn.expr as SemanticIdentifier, context, tokens);

	} else if (isPostfixExpr(asgn.ast)) {
		generateTokensForPostfix(asgn.expr as SemanticPostfix, context, tokens);
	} else if (asgn.expr != null) {
		for (const expr of asgn.expr as SemanticAssignLhs[]) {
			generateTokensForAssignLhs(expr, context, tokens);
		}
	}
}

function generateTokensForImport(imprt: SemanticImport, context: SemanticContext, tokens: SemanticToken[]): void {
	tokens.push(createToken(imprt.ast.importTok, 'keyword'));
	if (imprt.ast.asTok != null) {
		tokens.push(createToken(imprt.ast.asTok, 'keyword'));
	}
	if (imprt.ast.source.kind === 'StringExpr') {
		tokens.push(createToken(imprt.ast.source.token, 'string'));
	} else {
		generateTokensForIdentifier(imprt.name, context, tokens);
	}
}

function generateTokensForClass(cls: SemanticClass, context: SemanticContext, tokens: SemanticToken[]): void {
	tokens.push(createToken(cls.ast.classTok, 'keyword'));
	tokens.push(createToken(cls.ast.name.token, 'class'));
	for (const classAst of cls.ast.stats) {
		if (classAst.kind === 'FieldStat') {
			tokens.push(createToken(classAst.fieldTok, 'keyword'));
		}
	}
	for (const field of cls.fields) {
		tokens.push(createToken(field.token, 'property'));
		if (field.ast.kind === 'ParamExpr') {
			tokens.push(createToken(field.ast.field, 'keyword'));
		}
	}
	for (const [_, meth] of cls.methods) {
		if (meth.ast.methodTok != null) {
			tokens.push(createToken(meth.ast.methodTok, 'keyword'));
		}
		generateTokensForIdentifier(meth.name, context, tokens);
		if (meth.ast.asyncTok != null) {
			tokens.push(createToken(meth.ast.asyncTok, 'keyword', ['async']));
		}
		for (const param of meth.params) {
			generateTokensForIdentifier(param.name, context, tokens);
			if (param.isField) {
				tokens.push(createToken(param.ast.field, 'keyword'));
			}
		}
		generateTokensForStatement(meth.stat, context, tokens);
	}
}

function generateTokensForStatement(stat: SemanticStatement, context: SemanticContext, tokens: SemanticToken[]): void {
	if (stat === undefined) {
		return;
	} else if (stat.ast.kind === 'CompoundStat') {
		const compoundStat = stat as SemanticCompound;
		for (const sstat of compoundStat.stats) {
			generateTokensForStatement(sstat, context, tokens);
		}
	} else if (stat.ast.kind === 'SelectStat') {
		const selectStat = stat as SemanticSelect;
		tokens.push(createToken(stat.ast.ifTok, 'keyword'));
		generateTokensForStatement(selectStat.cond, context, tokens);
		generateTokensForStatement(selectStat.ifTrue, context, tokens);
		if (selectStat.ifFalse != null) {
			tokens.push(createToken(stat.ast.elseTok, 'keyword'));
			generateTokensForStatement(selectStat.ifFalse, context, tokens);
		}
	} else if (stat.ast.kind === 'ForStat') {
		tokens.push(createToken(stat.ast.forTok, 'keyword'));
		generateTokensForExpression((stat as SemanticFor).first, context, tokens);
		generateTokensForExpression((stat as SemanticFor).second, context, tokens);
		generateTokensForExpression((stat as SemanticFor).third, context, tokens);
		generateTokensForStatement((stat as SemanticFor).body, context, tokens);
	} else if (stat.ast.kind === 'ForeachStat') {
		tokens.push(createToken(stat.ast.forTok, 'keyword'));
		generateTokensForAssignLhs((stat as SemanticForeach).lhs, context, tokens);
		generateTokensForExpression((stat as SemanticForeach).rhs, context, tokens);
		generateTokensForStatement((stat as SemanticForeach).body, context, tokens);
	} else if (stat.ast.kind === 'WhileStat') {
		tokens.push(createToken(stat.ast.whileTok, 'keyword'));
		generateTokensForExpression((stat as SemanticWhile).cond, context, tokens);
		generateTokensForStatement((stat as SemanticWhile).body, context, tokens);
	} else if (stat.ast.kind === 'JumpStat') {
		tokens.push(createToken(stat.ast.token, 'keyword'));
		if ((stat as SemanticJump).expr != null) {
			generateTokensForExpression((stat as SemanticJump).expr, context, tokens);
		}
	} else {
		generateTokensForExpression(stat as SemanticExpression, context, tokens);
	}
}

function generatTokensForModule(module: SemanticModule, context: SemanticContext, tokens: SemanticToken[]): void {
	for (const imprt of module.imports) {
		generateTokensForImport(imprt, context, tokens);
	}
	for (const cls of module.classes) {
		generateTokensForClass(cls, context, tokens);
	}
	for (const stat of module.statements) {
		generateTokensForStatement(stat, context, tokens);
	}
}

function processUnary(unary: UnaryChainExpr, context: SemanticContext): SemanticUnary {
	return {
		ast: unary,
		rhs: processExpression(unary.expr, context)
	};
}

function processIdentifier(id: IdentifierExpr | NewExpr, context: SemanticContext, type: IdType = 'variable', modifiers: string[] = []): SemanticIdentifier {
	const identifier = context.block.lookupOrCreateIdentifier(id, type, modifiers);
	identifier.addToken(id.token);
	return identifier;
}

function processPostfix(postfix: PostfixChainExpr, context: SemanticContext): SemanticPostfix {
	const nestedExprs: (SemanticExpression | Token)[] = [];
	for (const pfx of postfix.postfixes) {
		if (pfx.kind === 'ArrayIndexExpr') {
			nestedExprs.push(processExpression(pfx.indices, context));
		} else if (pfx.kind === 'FunctionCallExpr' && pfx.args != null) {
			for (const arg of pfx.args) {
				nestedExprs.push(processExpression(arg, context));
			}
		} else if (pfx.kind === 'MemberAccessExpr') {
			nestedExprs.push(pfx.field.token);
		}
	}
	return {
		ast: postfix,
		base: processExpression(postfix.lhs, context) as SemanticPrimary,
		nestedExprs: nestedExprs
	};
}

function processExpression(expr: Expression | NamedArgExpr, context: SemanticContext): SemanticExpression {
	if (isConstantExpr(expr)) {
		return processConstant(expr as ConstantExpr, context);
	} else if (expr.kind === 'IdentifierExpr') {
		return processIdentifier(expr as IdentifierExpr, context);
	} else if (expr.kind === 'UnaryChainExpr') {
		return processUnary(expr as UnaryChainExpr, context);
	} else if (expr.kind === 'AssignBaseExpr') {
		return processAssign(expr as AssignBaseExpr, context);
	} else if (expr.kind === 'PostfixChainExpr') {
		return processPostfix(expr, context);
	}
	return undefined;
}

function processConstant(expr: ConstantExpr, context: SemanticContext): SemanticExpression {
	const text = expr.token.text;
	return {
		ast: expr,
		text: text,
		value: expr.kind === 'NoneExpr' ? null : expr.kind === 'BoolExpr' ? /^True$/.test(text) : +text
	};
}

function processAssignLhs(expr: AssignLhsExpr, context: SemanticContext): SemanticAssignLhs {
	if (expr.kind === 'IdentifierExpr') {
		return {
			ast: expr,
			expr: processIdentifier(expr, context)
		};
	}

	if (isPostfixExpr(expr)) {
		// TODO: handle postfix
		return {
			ast: expr,
			expr: processPostfix(expr as PostfixChainExpr, context)
		};
	}
	return {
		ast: expr,
		expr: (expr as AssignTupleExpr | AssignArrayExpr).assigns.map(a => processAssignLhs(a, context))
	};
}

function processAssign(expr: AssignBaseExpr, context: SemanticContext): SemanticAssign {
	return {
		ast: expr,
		lhs: processAssignLhs(expr.lhs, context),
		rhs: processExpression(expr.rhs, context)
	};
}

function processStatement(stat: Statement, context: SemanticContext): SemanticStatement {
	if (stat.kind === 'CompoundStat') {
		const newContext = context.newBlock();
		return {
			ast: stat,
			stats: stat.stats.map(s => processStatement(s, newContext))
		};
	} else if (stat.kind === 'SelectStat') {
		return {
			ast: stat,
			cond: processStatement(stat.cond, context),
			ifTrue: processStatement(stat.ifTrue, context.newBlock()),
			ifFalse: stat.elseTok != null ? processStatement(stat.ifFalse, context.newBlock()) : undefined
		};
	} else if (stat.kind === 'WhileStat') {
		const newContext = context.newBlock();
		return {
			ast: stat,
			cond: processExpression(stat.cond, newContext),
			body: processStatement(stat.stat, newContext)

		};
	} else if (stat.kind === 'ForStat') {
		const newContext = context.newBlock();
		return {
			ast: stat,
			first: processExpression(stat.first, newContext),
			second: processExpression(stat.second, newContext),
			third: processExpression(stat.third, newContext),
			body: processStatement(stat.stat, newContext)
		};
	} else if (stat.kind === 'ForeachStat') {
		const newContext = context.newBlock();
		return {
			ast: stat,
			lhs: processAssignLhs(stat.lhs, newContext),
			rhs: processExpression(stat.iter, newContext),
			body: processStatement(stat.stat, newContext)
		};
	} else if (stat.kind === 'JumpStat') {
		return {
			ast: stat,
			expr: stat.expr == null ? null : processExpression(stat.expr, context)
		};
	} else {
		return processExpression(stat as Expression, context);
	}
}

function createImportName(stat: ImportStat, context: SemanticContext): SemanticIdentifier {
	let name: string;
	if (stat.name == null) {
		name = stat.source.token.text;
		if (name.startsWith('\'')) {
			name = name.substring(1, name.length - 2);
			const pathParts = name.split('/');
			name = pathParts[pathParts.length - 1];
			return context.block.lookupOrCreateIdentifier(name, 'variable', ['defaultLibrary']);
		}
		return processIdentifier(stat.source as IdentifierExpr, context, 'variable', ['defaultLibrary']);
	}
	return processIdentifier(stat.name, context, 'variable', ['defaultLibrary']);

}

function processImport(stat: ImportStat, context: SemanticContext): SemanticImport {
	return {
		ast: stat,
		name: createImportName(stat, context),
		source: stat.source.token.text
	};
}

function processFields(stat: FieldStat, context: SemanticContext): SemanticField[] {
	const fields: SemanticField[] = [];
	for (const id of stat.fields) {
		fields.push({ ast: stat, token: id.token, name: processIdentifier(id, context, 'property') });
	}
	return fields;
}

function processStatic(stat: StaticStat, context: SemanticContext): SemanticStatic {
	return {
		ast: stat,
		name: stat.name.token.text
	};
}

function processMethod(stat: MethodStat, name: SemanticIdentifier, fields: SemanticField[], context: SemanticContext): SemanticMethod {
	const params: SemanticParam[] = [];
	const newContext = context.newBlock();
	for (const param of stat.params) {
		if (param.field != null) {
			fields.push({
				ast: param,
				name: processIdentifier(param.name, context, 'property'),
				token: param.name.token
			});
		}
		params.push({
			ast: param,
			name: processIdentifier(param.name, newContext, 'parameter'),
			isField: param.field != null
		});
	}
	return {
		ast: stat,
		name: name,
		params: params,
		stat: processStatement(stat.stat, newContext)
	};
}

function processClass(stat: ClassStat, context: SemanticContext): SemanticClass {
	let fields: SemanticField[] = [];
	const statics = new Map<string, SemanticStatic>;
	const methods = new Map<string, SemanticMethod>;
	for (const classStat of stat.stats) {
		if (classStat.kind === 'FieldStat') {
			fields = fields.concat(processFields(classStat, context));
		} else if (classStat.kind === 'StaticStat') {
			const sttc = processStatic(classStat, context);
			statics.set(sttc.name, sttc);
		} else {
			processIdentifier(classStat.name, context, 'method');
		}
	}
	for (const classStat of stat.stats) {
		if (classStat.kind === 'MethodStat') {
			const meth = processMethod(
				classStat,
				processIdentifier(classStat.name, context, 'method'),
				fields,
				context);
			methods.set(meth.name.id, meth);
		}
	}

	return {
		ast: stat,
		name: stat.name.token.text,
		// superName: stat.super != null ? stat.super.expr : undefined,
		fields: fields,
		statics: statics,
		methods: methods
	};
}

function processModule(module: Module, context: SemanticContext): SemanticModule {
	const imports: SemanticImport[] = [];
	const classes: SemanticClass[] = [];
	const statements: SemanticStatement[] = [];
	for (const stat of module.statements) {
		if (stat.kind === 'ImportStat' || stat.kind === 'ImportAsStat') {
			imports.push(processImport(stat, context));
		} else if (stat.kind === 'ClassStat') {
			classes.push(processClass(stat, context));
		} else {
			const sstat = processStatement(stat, context);
			if (sstat != null) {
				statements.push(sstat);
			}
		}
	}
	const semanticModule: SemanticModule = {
		ast: module,
		imports: imports,
		classes: classes,
		statements: statements
	};
	context.module = semanticModule;
	return semanticModule;
}

export function generateSemanticTokens(module: Module): SemanticToken[] {
	const context = new SemanticContext();
	const tokens: SemanticToken[] = [];
	const semanticModule = processModule(module, context);
	generatTokensForModule(semanticModule, context, tokens);
	return tokens;
}