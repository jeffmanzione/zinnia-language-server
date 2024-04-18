import * as parsec from 'typescript-parsec';
import { ClassStat, FieldStat, ImportStat, MethodStat, Module, StaticStat } from './statements';
import { TokenKind } from './tokenizer';

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
	ast: FieldStat;
	token: Token;
	name: string;
}

interface SemanticStatic {
	ast: StaticStat;
	name: string;
}

interface SemanticMethod {
	ast: MethodStat;
	name: string;
}

interface SemanticClass {
	ast: ClassStat;
	name: string;
	superName?: string;
	fields: SemanticField[];
	statics: Map<string, SemanticStatic>;
	methods: Map<string, SemanticMethod>;
}

interface SemanticModule {
	ast: Module;
	imports: SemanticImport[];
}

class SemanticContext {
	module: SemanticModule;

}

function createToken(token: Token, type: string, modifiers: string[] = []): SemanticToken {
	return {
		text: token.text,
		col: token.pos.columnBegin,
		row: token.pos.rowBegin,
		type: type,
		modifiers: modifiers
	};
}

function generatTokens(imprt: SemanticImport, context: SemanticContext, tokens: SemanticToken[]): void {
	tokens.push(createToken(imprt.ast.importTok, 'keyword'));
	if (imprt.ast.asTok != null) {
		tokens.push(createToken(imprt.ast.asTok, 'keyword'));
	}
}

function processImport(stat: ImportStat, context: SemanticContext): SemanticImport {
	return {
		ast: stat,
		name: stat.name.token.text,
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

function processMethod(stat: MethodStat, context: SemanticContext): SemanticMethod {
	return {
		ast: stat,
		name: stat.name.token.text
	};
}

function processClass(stat: ClassStat, context: SemanticContext): SemanticClass {
	const fields: SemanticField[] = [];
	const statics = new Map<string, SemanticStatic>;
	const methods = new Map<string, SemanticMethod>;
	for (const classStat of stat.stats) {
		if (classStat.kind === 'FieldStat') {
			fields.concat(processFields(classStat, context));
		} else if (classStat.kind === 'StaticStat') {
			const sttc = processStatic(classStat, context);
			statics[sttc.name] = sttc;
		} else {
			const meth = processMethod(classStat, context);
			methods[meth.name] = meth;
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
	for (const stat of module.statements) {
		if (stat.kind === 'ImportStat' || stat.kind === 'ImportAsStat') {
			imports.push(processImport(stat, context));
		} else if (stat.kind === 'ClassStat') {
			classes.push(processClass(stat, context));
		}
	}
	const semanticModule: SemanticModule = {
		ast: module,
		imports: imports
	};
	context.module = semanticModule;
	return semanticModule;
}

export function generateSemanticTokens(module: Module): SemanticToken[] {
	const context = new SemanticContext();
	const tokens: SemanticToken[] = [];
	const semanticModule = processModule(module, context);
	return tokens;
}