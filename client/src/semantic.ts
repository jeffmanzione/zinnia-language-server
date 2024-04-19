import * as parsec from 'typescript-parsec';
import { ClassStat, CompoundStat, FieldStat, ImportStat, MethodStat, Module, SelectStat, Statement, StaticStat } from './statements';
import { TokenKind } from './tokenizer';
import { ParamExpr } from './expressions';

type Token = parsec.Token<TokenKind>;


export interface SemanticToken {
	text: string;
	col: number;
	row: number;
	type: string;
	modifiers: string[];
}

interface SemanticImport {
	ast: ImportStat;
	name: string;
	source: string;
}

interface SemanticField {
	ast: FieldStat | ParamExpr;
	token: Token;
	name: string;
}

interface SemanticStatic {
	ast: StaticStat;
	name: string;
}

interface SemanticParam {
	ast: ParamExpr;
	name: string;
	isField: boolean;
}

interface SemanticMethod {
	ast: MethodStat;
	name: string;
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

type SemanticStatement =
	| SemanticCompound
	| SemanticSelect;

interface SemanticModule {
	ast: Module;
	imports: SemanticImport[];
	classes: SemanticClass[];
	statements: SemanticStatement[];
}

class SemanticContext {
	module: SemanticModule;

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

function generateTokensForImport(imprt: SemanticImport, context: SemanticContext, tokens: SemanticToken[]): void {
	tokens.push(createToken(imprt.ast.importTok, 'keyword'));
	if (imprt.ast.asTok != null) {
		tokens.push(createToken(imprt.ast.asTok, 'keyword'));
	}
	if (imprt.ast.source.kind === 'StringExpr') {
		tokens.push(createToken(imprt.ast.source.token, 'string'));
	} else {
		tokens.push(createToken(imprt.ast.source.token, 'variable', ['defaultLibrary']));
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
		tokens.push(createToken(meth.ast.name.token, 'method'));
		if (meth.ast.asyncTok != null) {
			tokens.push(createToken(meth.ast.asyncTok, 'keyword', ['async']));
		}
		for (const param of meth.params) {
			tokens.push(createToken(param.ast.name.token, 'property'));
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

function processStatement(stat: Statement, context: SemanticContext): SemanticStatement {
	if (stat.kind === 'CompoundStat') {
		return {
			ast: stat,
			stats: stat.stats.map(s => processStatement(s, context))
		};
	} else if (stat.kind === 'SelectStat') {
		return {
			ast: stat,
			cond: processStatement(stat.cond, context),
			ifTrue: processStatement(stat.ifTrue, context),
			ifFalse: stat.elseTok != null ? processStatement(stat.ifFalse, context) : undefined
		};
	}
	return undefined;
}

function createImportName(stat: ImportStat): string {
	let name: string;
	if (stat.name == null) {
		name = stat.source.token.text;
		if (name.startsWith('\'')) {
			name = name.substring(1, name.length - 2);
			const pathParts = name.split('/');
			name = pathParts[pathParts.length - 1];
		}
	} else {
		name = stat.name.token.text;
	}
	return name;
}

function processImport(stat: ImportStat, context: SemanticContext): SemanticImport {
	return {
		ast: stat,
		name: createImportName(stat),
		source: stat.source.token.text
	};
}

function processFields(stat: FieldStat, context: SemanticContext): SemanticField[] {
	const fields: SemanticField[] = [];
	for (const id of stat.fields) {
		fields.push({ ast: stat, token: id.token, name: id.token.text });
	}
	return fields;
}

function processStatic(stat: StaticStat, context: SemanticContext): SemanticStatic {
	return {
		ast: stat,
		name: stat.name.token.text
	};
}

function processMethod(stat: MethodStat, fields: SemanticField[], context: SemanticContext): SemanticMethod {
	const params: SemanticParam[] = [];
	const stats: SemanticStatement[] = [];
	for (const param of stat.params) {
		if (param.field != null) {
			fields.push({
				ast: param,
				name: param.name.token.text,
				token: param.name.token
			});
		}
		params.push({
			ast: param,
			name: param.name.token.text,
			isField: param.field != null
		});
	}
	return {
		ast: stat,
		name: stat.name.token.text,
		params: params,
		stat: processStatement(stat.stat, context)
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
			const meth = processMethod(classStat, fields, context);
			methods.set(meth.name, meth);
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