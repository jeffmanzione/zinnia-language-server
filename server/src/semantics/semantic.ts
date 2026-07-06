import * as parsec from 'typescript-parsec';

import {SemanticAnalyzer, SemanticDocInfo} from './analyzer';
import {AddChainExpr, AndChainExpr, AnnotationExpr, AnonExpr, ArrayExpr, AssignArrayExpr, AssignBaseExpr, AssignLhsExpr, AssignTupleExpr, BinaryChainExpr, ConditionBaseExpr, ConstantExpr, EqualChainExpr, Expression, IdentifierExpr, InExpr, isConstantExpr, IsExpr, isPostfixExpr, MapExpr, MultChainExpr, NamedArgExpr, NewExpr, OrChainExpr, ParamExpr, ParensExpr, PostfixChainExpr, RangeExpr, RelationChainExpr, StringExpr, TupleChainExpr, UnaryChainExpr} from './expressions';
import {ClassStat, CompoundStat, FieldStat, ForeachStat, ForStat, FunctionStat, ImportStat, JumpStat, MethodStat, Module, RaiseStat, SelectStat, SpecialMethodIdentifierExpr, Statement, StaticStat, TryStat, WhileStat} from './statements';
import {TokenKind} from './tokenizer';

type Token = parsec.Token<TokenKind>;


export interface SemanticToken {
  text: string;
  col: number;
  row: number;
  type: string;
  parentName?: string;
  modifiers: string[];
  filePath: string;
}

type IdType =
    |'variable'|'property'|'parameter'|'function'|'method'|'class'|'decorator';

interface TokenFile {
  token: Token;
  filePath: string;
}

export class SemanticIdentifier {
  ast?: IdentifierExpr;
  id: string;
  type: IdType;
  tokens: TokenFile[] = [];
  modifiers: string[];
  tokensGenerated: boolean = false;

  constructor(
      ast: IdentifierExpr, type: IdType, modifiers: string[] = [],
      name?: string) {
    this.ast = ast;
    this.id = name ?? ast.token.text;
    this.type = type;
    this.modifiers = modifiers;
  }

  addToken(token: Token, filePath: string): void {
    this.tokens.push({token: token, filePath: filePath});
  }

  hasGenerated(): boolean {
    return this.tokensGenerated;
  }

  markGenerated(): void {
    this.tokensGenerated = true;
  }
}

interface SemanticConstant {
  ast: ConstantExpr;
  text: string;
  value: boolean|number|null;
}

interface SemanticString {
  ast: StringExpr;
  text: string;
}

interface SemanticUnary {
  ast: UnaryChainExpr;
  rhs: SemanticExpression;
}

interface SemanticTuple {
  ast: TupleChainExpr;
  items: SemanticExpression[];
}

interface SemanticArray {
  ast: ArrayExpr;
  items: SemanticExpression[];
}

interface SemanticMap {
  ast: MapExpr;
  entries: Map<SemanticExpression, SemanticExpression>;
}

interface SemanticParens {
  ast: ParensExpr;
  expr: SemanticExpression;
}

type OpExpr =|BinaryChainExpr|MultChainExpr|AddChainExpr|RelationChainExpr|
    EqualChainExpr|AndChainExpr|OrChainExpr;

const opExprKinds = new Set([
  'BinaryChainExpr', 'MultChainExpr', 'AddChainExpr', 'RelationChainExpr',
  'EqualChainExpr', 'AndChainExpr', 'OrChainExpr'
]);

function isOpExpr(expr: Expression): boolean {
  return opExprKinds.has(expr.kind);
}

interface SemanticOp {
  ast: OpExpr;
  exprs: SemanticExpression[];
}

interface SemanticIs {
  ast: IsExpr;
  lhs: SemanticExpression;
  rhs: SemanticExpression;
}

interface SemanticIn {
  ast: InExpr;
  lhs: SemanticExpression;
  rhs: SemanticExpression;
}

interface SemanticCondition {
  ast: ConditionBaseExpr;
  cond: SemanticExpression;
  ifTrue: SemanticExpression;
  ifFalse?: SemanticExpression;
}

interface SemanticRange {
  ast: RangeExpr;
  start: SemanticExpression;
  end: SemanticExpression;
  inc?: SemanticExpression;
}

interface SemanticAnon {
  ast: AnonExpr;
  params: SemanticParam[];
  stat: SemanticStatement;
}

interface SemanticAnnotation {
  ast: AnnotationExpr;
  firstId: SemanticIdentifier;
  secondId?: Token;  // TODO: identifiers should look into fields too.
  params: SemanticExpression[];
}

type SemanticExpression =|SemanticConstant|SemanticString|SemanticIdentifier|
    SemanticUnary|SemanticPostfix|SemanticAssign|SemanticTuple|SemanticArray|
    SemanticMap|SemanticNamedArg|SemanticParens|SemanticOp|SemanticIs|
    SemanticIn|SemanticCondition|SemanticRange|SemanticAnon;

interface SemanticAssignLhs {
  ast: AssignLhsExpr;
  expr: SemanticIdentifier|SemanticPostfix|SemanticAssignLhs[];
}

type SemanticPrimary =|SemanticIdentifier;

interface SemanticPostfix {
  ast: PostfixChainExpr;
  base: SemanticPrimary;
  nestedExprs: (SemanticExpression|Token)[];
}

interface SemanticNamedArg {
  ast: NamedArgExpr;
  name: SemanticIdentifier;
  value: SemanticExpression;
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
  ast: FieldStat|ParamExpr;
  token: Token;
  name: SemanticIdentifier;
}

interface SemanticStatic {
  ast: StaticStat;
  name: SemanticIdentifier;
  expr: SemanticExpression;
}

interface SemanticParam {
  ast: ParamExpr;
  name: SemanticIdentifier;
  isField: boolean;
  defaultValue?: SemanticExpression;
}

interface SemanticMethod {
  ast: MethodStat;
  name: SemanticIdentifier;
  params: SemanticParam[];
  stat: SemanticStatement;
  annots: SemanticAnnotation[];
  className: string;
}

interface SemanticClass {
  ast: ClassStat;
  name: SemanticIdentifier;
  superName?: SemanticExpression;
  fields: SemanticField[];
  statics: Map<string, SemanticStatic>;
  methods: Map<string, SemanticMethod>;
  annots: SemanticAnnotation[];
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
  cond: SemanticExpression;  // Condition
  body: SemanticStatement;
}

interface SemanticFor {
  ast: ForStat;
  first: SemanticExpression;   // Assignment
  second: SemanticExpression;  // Condition
  third: SemanticExpression;   // Assignment
  body: SemanticStatement;
}

interface SemanticForeach {
  ast: ForeachStat;
  lhs: SemanticAssignLhs;   // Assignment
  rhs: SemanticExpression;  // Condition
  body: SemanticStatement;
}

type SemanticIter =|SemanticWhile|SemanticFor|SemanticForeach;

interface SemanticJump {
  ast: JumpStat;
  expr?: SemanticExpression;
}

interface SemanticTry {
  ast: TryStat;
  tryStat: SemanticStatement;
  catchAssign: SemanticAssignLhs;
  catchStat: SemanticStatement;
}

interface SemanticRaise {
  ast: RaiseStat;
  expr: SemanticExpression;
}

interface SemanticFunction {
  ast: FunctionStat;
  name: SemanticIdentifier;
  params: SemanticParam[];
  stat: SemanticStatement;
  annots: SemanticAnnotation[];
}

type SemanticStatement =|SemanticCompound|SemanticSelect|SemanticIter|
    SemanticJump|SemanticTry|SemanticRaise|SemanticFunction|SemanticExpression;

export interface SemanticModule {
  ast: Module;
  imports: SemanticImport[];
  classes: SemanticClass[];
  statements: SemanticStatement[];
}

class Block {
  private readonly members: Map<string, SemanticIdentifier> = new Map();

  constructor(
      private readonly analyzer: SemanticAnalyzer,
      private readonly parent?: Block) {}

  createIdentifier(id: IdentifierExpr, type: IdType): SemanticIdentifier {
    const sid = new SemanticIdentifier(id, type);
    this.members.set(sid.id, sid);
    return sid;
  }

  lookupOrCreateIdentifier(
      id: IdentifierExpr|SpecialMethodIdentifierExpr|NewExpr|string,
      type: IdType, modifiers: string[] = []): SemanticIdentifier {
    const idText = typeof id === 'string' ? id : id.token.text;
    const foundId = this.findIdentifier(idText);
    if (foundId != null) {
      // // Upgrade if found declaration
      // if (type === 'function' || type === 'method' || type === 'class') {
      //   foundId.type = type;
      // }
      return foundId;
    }

    let ast: IdentifierExpr;

    if (typeof id === 'string') {
      ast = {kind: 'IdentifierExpr'} as IdentifierExpr;
    } else if (id.kind === 'IdentifierExpr') {
      ast = id;
    } else {
      ast = {kind: 'IdentifierExpr', token: id.token};
    }

    const sid = new SemanticIdentifier(ast, type, modifiers, idText);
    this.members.set(sid.id, sid);
    return sid;
  }

  findIdentifier(id: string, lookInBuiltin: boolean = true): SemanticIdentifier
      |undefined {
    if (this.members.has(id)) {
      return this.members.get(id);
    }
    if (this.parent != null) {
      return this.parent.findIdentifier(id, lookInBuiltin);
    }
    if (lookInBuiltin) {
      // As a last hope, look in builtin.
      return this.analyzer.searchBuiltinForId(id);
    }
    return undefined;
  }
}

export class SemanticContext {
  module?: SemanticModule;

  constructor(
      private readonly analyzer: SemanticAnalyzer, readonly filePath: string,
      readonly block: Block = new Block(analyzer)) {}

  setModule(module: SemanticModule): void {
    this.module = module;
  }

  async lookupModule(name: string): Promise<SemanticModule|null> {
    return this.analyzer.lookupModule(name);
  }

  lookupDocInfo(moduleName: string): SemanticDocInfo|null {
    return this.analyzer.lookupDocInfoFromModuleName(moduleName) ?? null;
  }

  newBlock(): SemanticContext {
    const copy = new SemanticContext(
        this.analyzer, this.filePath, new Block(this.analyzer, this.block));
    copy.setModule(this.module!);
    return copy;
  }
}

class TokenGenerator {
  private readonly _tokens: SemanticToken[] = [];

  constructor(private readonly _filePath: string) {}

  createToken(
      token: Token,
      type: string,
      modifiers: string[] = [],
      filePath = this._filePath,
      parentName?: string,
      ): SemanticToken {
    const semanticToken = {
      text: token.text,
      col: token.pos.columnBegin - 1,
      row: token.pos.rowBegin - 1,
      type: type,
      filePath: filePath,
      modifiers: modifiers,
      parentName: parentName,
    } satisfies SemanticToken;
    this._tokens.push(semanticToken);
    return semanticToken;
  }

  createTokenFromTemplate(token: Token, template: SemanticToken):
      SemanticToken {
    const semanticToken = {
      ...template,
      text: token.text,
      col: token.pos.columnBegin - 1,
      row: token.pos.rowBegin - 1
    };
    this._tokens.push(semanticToken);
    return semanticToken;
  }

  finalizeTokens(): SemanticToken[] {
    return this._tokens.filter(tok => tok.filePath === this._filePath);
  }
}

function generateTokensForConstant(
    cnst: SemanticConstant, context: SemanticContext,
    generator: TokenGenerator): void {
  if ('tokens' in cnst.ast) {
    for (const tok of cnst.ast.tokens) {
      generator.createToken(tok, 'number', ['constant']);
    }
  } else {
    generator.createToken(cnst.ast.token, 'number', ['constant']);
  }
}


function generateTokensForString(
    strng: SemanticString, context: SemanticContext,
    generator: TokenGenerator): void {
  generator.createToken(strng.ast.token, 'string', ['constant']);
}

function generateTokensForUnary(
    unary: SemanticUnary, context: SemanticContext,
    generator: TokenGenerator): void {
  for (const un of unary.ast.unaries) {
    if (un.kind === TokenKind.KEYWORD_AWAIT) {
      generator.createToken(un, 'keyword');
    }
  }
  generateTokensForExpression(unary.rhs, context, generator);
}

function selectIdType(id: SemanticIdentifier, token: Token): IdType {
  if (id.type === 'class' || id.type === 'method' || id.type === 'property' ||
      id.type === 'parameter' || token.next?.kind !== TokenKind.SYMBOL_LPAREN) {
    return id.type;
  }
  return 'function';
}

function generateTokenForIdentiferToken(
    id: SemanticIdentifier, tok: TokenFile, context: SemanticContext,
    generator: TokenGenerator, parentName?: string): void {
  const modifiers = id.modifiers.slice();
  generator.createToken(
      tok.token, selectIdType(id, tok.token), modifiers, tok.filePath,
      parentName);
}

function generateTokensForIdentifier(
    id: SemanticIdentifier, context: SemanticContext, generator: TokenGenerator,
    parentName?: string): void {
  if (id == null /* || id.hasGenerated() */) {
    return;
  }
  let doneFirst = false;
  const modifiers = id.modifiers.slice();
  for (const tok of id.tokens) {
    // The first token is always the declaration.
    if (!doneFirst) {
      modifiers.push('declaration');
      doneFirst = true;
    }
    generateTokenForIdentiferToken(id, tok, context, generator, parentName);
  }
  id.markGenerated();
}


function generateTokensForAssign(
    asgn: SemanticAssign, context: SemanticContext,
    generator: TokenGenerator): void {
  generateTokensForAssignLhs(asgn.lhs, context, generator);
  generateTokensForExpression(asgn.rhs, context, generator);
}


function generateTokensForAnnotation(
    annot: SemanticAnnotation, context: SemanticContext,
    generator: TokenGenerator): void {
  generateTokensForIdentifier(annot.firstId, context, generator);
  if (annot.secondId != null) {
    const foundId =
        lookupMemberFromPossibleModule(annot.firstId, annot.secondId, context);
    if (foundId) {
      generateTokenForIdentiferToken(
          foundId, {
            token: annot.secondId,
            filePath: context.filePath,
          },
          context, generator);
    } else {
      generator.createToken(annot.secondId, 'decorator');
    }
  }
  if (annot.params != null) {
    for (const param of annot.params) {
      generateTokensForExpression(param, context, generator);
    }
  }
}

function lookupMemberFromPossibleModule(
    possibleModule: SemanticIdentifier, tok: Token, context: SemanticContext) {
  if (possibleModule && possibleModule.type === 'variable' &&
      possibleModule.modifiers.includes('defaultLibrary')) {
    return context.lookupDocInfo(possibleModule.id)
        ?.context.block.findIdentifier(tok.text);
  }
  return null;
}

function generateTokensForPostfix(
    pstfx: SemanticPostfix, context: SemanticContext,
    generator: TokenGenerator): void {
  if (pstfx.base != null) {  // TODO remove check.
    generateTokensForExpression(pstfx.base, context, generator);
  }
  let curId: SemanticIdentifier|null = pstfx.base;
  for (const nestedExpr of pstfx.nestedExprs) {
    if (nestedExpr == null) {
      continue;
    }
    if ('nextToken' in nestedExpr) {
      const tok = nestedExpr as Token;
      const foundId = lookupMemberFromPossibleModule(
          curId as SemanticIdentifier, tok, context);

      if (foundId) {
        // // Do this?
        // foundId.addToken(tok, context.filePath);
        generateTokenForIdentiferToken(
            foundId, {
              token: tok,
              filePath: context.filePath,
            },
            context, generator);
        curId = null;
        continue;
      }

      generator.createToken(
          tok,
          tok.next!.kind === TokenKind.SYMBOL_LPAREN ? 'method' : 'property');
    } else {
      generateTokensForExpression(
          nestedExpr as SemanticExpression, context, generator);
    }
  }
  // console.log(pstfx);
}

function generateTokensForNamedArg(
    arg: SemanticNamedArg, context: SemanticContext,
    generator: TokenGenerator): void {
  generateTokensForIdentifier(arg.name, context, generator);
  generateTokensForExpression(arg.value, context, generator);
}

function generateTokensForTuple(
    tuple: SemanticTuple, context: SemanticContext,
    generator: TokenGenerator): void {
  for (const item of tuple.items) {
    if (item == null) {
      continue;
    }
    generateTokensForExpression(item, context, generator);
  }
}

function generateTokensForArray(
    arr: SemanticArray, context: SemanticContext,
    generator: TokenGenerator): void {
  for (const item of arr.items) {
    if (item == null) {
      continue;
    }
    generateTokensForExpression(item, context, generator);
  }
}

function generateTokensForMap(
    map: SemanticMap, context: SemanticContext,
    generator: TokenGenerator): void {
  for (const [k, v] of map.entries) {
    if (k != null) {
      generateTokensForExpression(k, context, generator);
    }
    if (v != null) {
      generateTokensForExpression(v, context, generator);
    }
  }
}

function generateTokensForOp(
    op: SemanticOp, context: SemanticContext, generator: TokenGenerator) {
  for (const expr of op.exprs) {
    if (expr != null) {
      generateTokensForExpression(expr, context, generator);
    }
  }
}

function generateTokensForIs(
    expr: SemanticIs, context: SemanticContext, generator: TokenGenerator) {
  generator.createToken(expr.ast.isTok, 'keyword');
  generateTokensForExpression(expr.lhs, context, generator);
  generateTokensForExpression(expr.rhs, context, generator);
}

function generateTokensForIn(
    expr: SemanticIn, context: SemanticContext, generator: TokenGenerator) {
  generator.createToken(expr.ast.inTok, 'keyword');
  generateTokensForExpression(expr.lhs, context, generator);
  generateTokensForExpression(expr.rhs, context, generator);
}

function generateTokensForCondition(
    cond: SemanticCondition, context: SemanticContext,
    generator: TokenGenerator) {
  generator.createToken(cond.ast.if, 'keyword');
  if (cond.ast.then != null) {
    generator.createToken(cond.ast.then, 'keyword');
  }
  if (cond.ast.else != null) {
    generator.createToken(cond.ast.else, 'keyword');
  }
  generateTokensForExpression(cond.cond, context, generator);
  generateTokensForExpression(cond.ifTrue, context, generator);
  if (cond.ifFalse != null) {
    generateTokensForExpression(cond.ifFalse, context, generator);
  }
}

function generateTokensForRange(
    rng: SemanticRange, context: SemanticContext, generator: TokenGenerator) {
  generateTokensForExpression(rng.start, context, generator);
  generateTokensForExpression(rng.end, context, generator);
  if (rng.inc != null) {
    generateTokensForExpression(rng.inc, context, generator);
  }
}

function generateTokensForAnon(
    anon: SemanticAnon, context: SemanticContext,
    generator: TokenGenerator): void {
  if (anon.ast.asyncTok != null) {
    generator.createToken(anon.ast.asyncTok, 'keyword', ['async']);
  }
  for (const param of anon.params) {
    generateTokensForIdentifier(param.name, context, generator);
    if (param.isField) {
      generator.createToken(param.ast.field!, 'keyword');
    }
    if (param.defaultValue != null) {
      generateTokensForExpression(param.defaultValue, context, generator);
    }
  }
  generateTokensForStatement(anon.stat, context, generator);
}

function generateTokensForExpression(
    expr: SemanticExpression, context: SemanticContext,
    generator: TokenGenerator): void {
  if (expr?.ast == null) {
    return;
  }
  if (isConstantExpr(expr.ast)) {
    generateTokensForConstant(expr as SemanticConstant, context, generator);
  } else if (expr.ast.kind === 'StringExpr') {
    generateTokensForString(expr as SemanticString, context, generator);
  } else if (expr.ast.kind === 'IdentifierExpr') {
    generateTokensForIdentifier(expr as SemanticIdentifier, context, generator);
  } else if (expr.ast.kind === 'UnaryChainExpr') {
    generateTokensForUnary(expr as SemanticUnary, context, generator);
  } else if (expr.ast.kind === 'AssignBaseExpr') {
    generateTokensForAssign(expr as SemanticAssign, context, generator);
  } else if (expr.ast.kind === 'PostfixChainExpr') {
    generateTokensForPostfix(expr as SemanticPostfix, context, generator);
  } else if (expr.ast.kind === 'TupleChainExpr') {
    generateTokensForTuple(expr as SemanticTuple, context, generator);
  } else if (expr.ast.kind === 'ArrayExpr') {
    generateTokensForArray(expr as SemanticArray, context, generator);
  } else if (expr.ast.kind === 'MapExpr') {
    generateTokensForMap(expr as SemanticMap, context, generator);
  } else if (expr.ast.kind === 'NamedArgExpr') {
    generateTokensForNamedArg(expr as SemanticNamedArg, context, generator);
  } else if (expr.ast.kind === 'ParensExpr') {
    if ((expr as SemanticParens).expr != null) {
      generateTokensForExpression(
          (expr as SemanticParens).expr, context, generator);
    }
  } else if (expr.ast.kind === 'IsExpr') {
    return generateTokensForIs(expr as SemanticIs, context, generator);
  } else if (expr.ast.kind === 'InExpr') {
    return generateTokensForIn(expr as SemanticIn, context, generator);
  } else if (expr.ast.kind === 'ConditionBaseExpr') {
    return generateTokensForCondition(
        expr as SemanticCondition, context, generator);
  } else if (expr.ast.kind === 'RangeExpr') {
    return generateTokensForRange(expr as SemanticRange, context, generator);
  } else if (expr.ast.kind === 'AnonExpr') {
    return generateTokensForAnon(expr as SemanticAnon, context, generator);
  } else if (isOpExpr(expr.ast)) {
    generateTokensForOp(expr as SemanticOp, context, generator);
  }
}

function generateTokensForAssignLhs(
    asgn: SemanticAssignLhs, context: SemanticContext,
    generator: TokenGenerator): void {
  if (asgn.ast.kind === 'IdentifierExpr') {
    generateTokensForExpression(
        asgn.expr as SemanticIdentifier, context, generator);
  } else if (isPostfixExpr(asgn.ast)) {
    generateTokensForPostfix(asgn.expr as SemanticPostfix, context, generator);
  } else if (asgn.expr != null) {
    for (const expr of asgn.expr as SemanticAssignLhs[]) {
      generateTokensForAssignLhs(expr, context, generator);
    }
  }
}

function generateTokensForImport(
    imprt: SemanticImport, context: SemanticContext,
    generator: TokenGenerator): void {
  generator.createToken(imprt.ast.importTok, 'keyword');
  if (imprt.ast.asTok != null) {
    generator.createToken(imprt.ast.asTok, 'keyword');
  }
  if (imprt.ast.source.kind === 'StringExpr') {
    generator.createToken(imprt.ast.source.token, 'string');
  } else {
    generateTokensForIdentifier(imprt.name, context, generator);
  }
}

function generateTokensForMethod(
    meth: SemanticMethod, context: SemanticContext,
    generator: TokenGenerator): void {
  if (meth.annots != null) {
    for (const annot of meth.annots) {
      generateTokensForAnnotation(annot, context, generator);
    }
  }
  if (meth.ast.methodTok != null) {
    generator.createToken(meth.ast.methodTok, 'keyword');
  }
  generateTokensForIdentifier(meth.name, context, generator, meth.className);
  if (meth.ast.asyncTok != null) {
    generator.createToken(meth.ast.asyncTok, 'keyword', ['async']);
  }
  for (const param of meth.params) {
    generateTokensForIdentifier(param.name, context, generator);
    if (param.isField) {
      generator.createToken(param.ast.field!, 'keyword');
    }
    if (param.defaultValue != null) {
      generateTokensForExpression(param.defaultValue, context, generator);
    }
  }
  generateTokensForStatement(meth.stat, context, generator);
}

function generateTokensForClass(
    cls: SemanticClass, context: SemanticContext,
    generator: TokenGenerator): void {
  if (cls.annots != null) {
    for (const annot of cls.annots) {
      generateTokensForAnnotation(annot, context, generator);
    }
  }

  generator.createToken(cls.ast.classTok, 'keyword');
  generateTokensForIdentifier(cls.name, context, generator);

  if (cls.superName != null) {
    generateTokensForExpression(cls.superName, context, generator);
  }

  for (const [_, sttc] of cls.statics) {
    generator.createToken(sttc.ast.staticTok, 'keyword');
    generateTokensForIdentifier(sttc.name, context, generator, cls.name.id);
    generateTokensForExpression(sttc.expr, context, generator);
  }

  for (const classAst of cls.ast.stats) {
    if (classAst.kind === 'FieldStat') {
      generator.createToken(classAst.fieldTok, 'keyword');
    }
  }
  for (const field of cls.fields) {
    generator.createToken(
        field.token, 'property', undefined, undefined, cls.name.id);
    if (field.ast.kind === 'ParamExpr') {
      generator.createToken(field.ast.field!, 'keyword');
    }
  }
  for (const [_, meth] of cls.methods) {
    generateTokensForMethod(meth, context, generator);
  }
}

function generateTokensForFunction(
    func: SemanticFunction, context: SemanticContext,
    generator: TokenGenerator): void {
  if (func.annots != null) {
    for (const annot of func.annots) {
      generateTokensForAnnotation(annot, context, generator);
    }
  }
  if (func.ast.defTok != null) {
    generator.createToken(func.ast.defTok, 'keyword');
  }
  generateTokensForIdentifier(func.name, context, generator);
  if (func.ast.asyncTok != null) {
    generator.createToken(func.ast.asyncTok, 'keyword', ['async']);
  }
  for (const param of func.params) {
    generateTokensForIdentifier(param.name, context, generator);
    if (param.isField) {
      generator.createToken(param.ast.field!, 'keyword');
    }
    if (param.defaultValue != null) {
      generateTokensForExpression(param.defaultValue, context, generator);
    }
  }
  generateTokensForStatement(func.stat, context, generator);
}

function generateTokensForStatement(
    stat: SemanticStatement, context: SemanticContext,
    generator: TokenGenerator): void {
  if (stat === undefined) {
    return;
  }
  const ast = stat.ast!;
  if (ast.kind === 'CompoundStat') {
    const compoundStat = stat as SemanticCompound;
    for (const sstat of compoundStat.stats) {
      generateTokensForStatement(sstat, context, generator);
    }
  } else if (ast.kind === 'SelectStat') {
    const selectStat = stat as SemanticSelect;
    generator.createToken(ast.ifTok, 'keyword');
    generateTokensForStatement(selectStat.cond, context, generator);
    generateTokensForStatement(selectStat.ifTrue, context, generator);
    if (selectStat.ifFalse != null) {
      generator.createToken(ast.elseTok!, 'keyword');
      generateTokensForStatement(selectStat.ifFalse, context, generator);
    }
  } else if (ast.kind === 'ForStat') {
    generator.createToken(ast.forTok, 'keyword');
    generateTokensForExpression(
        (stat as SemanticFor).first, context, generator);
    generateTokensForExpression(
        (stat as SemanticFor).second, context, generator);
    generateTokensForExpression(
        (stat as SemanticFor).third, context, generator);
    generateTokensForStatement((stat as SemanticFor).body, context, generator);
  } else if (ast.kind === 'ForeachStat') {
    generator.createToken(ast.forTok, 'keyword');
    generator.createToken(ast.inTok, 'keyword');
    generateTokensForAssignLhs(
        (stat as SemanticForeach).lhs, context, generator);
    generateTokensForExpression(
        (stat as SemanticForeach).rhs, context, generator);
    generateTokensForStatement(
        (stat as SemanticForeach).body, context, generator);
  } else if (ast.kind === 'WhileStat') {
    generator.createToken(ast.whileTok, 'keyword');
    generateTokensForExpression(
        (stat as SemanticWhile).cond, context, generator);
    generateTokensForStatement(
        (stat as SemanticWhile).body, context, generator);
  } else if (ast.kind === 'JumpStat') {
    generator.createToken(ast.token, 'keyword');
    if ((stat as SemanticJump).expr != null) {
      generateTokensForExpression(
          (stat as SemanticJump).expr!, context, generator);
    }
  } else if (ast.kind === 'TryStat') {
    generator.createToken(ast.tryTok, 'keyword');
    generator.createToken(ast.catchTok, 'keyword');
    generateTokensForStatement(
        (stat as SemanticTry).tryStat, context, generator);
    generateTokensForAssignLhs(
        (stat as SemanticTry).catchAssign, context, generator);
    generateTokensForStatement(
        (stat as SemanticTry).catchStat, context, generator);
  } else if (ast.kind === 'RaiseStat') {
    generator.createToken(ast.raise, 'keyword');
    generateTokensForExpression(
        (stat as SemanticRaise).expr, context, generator);
  } else if (ast.kind === 'FunctionStat') {
    generateTokensForFunction((stat as SemanticFunction), context, generator);
  } else {
    generateTokensForExpression(stat as SemanticExpression, context, generator);
  }
}

function generatTokensForModule(
    module: SemanticModule, context: SemanticContext,
    generator: TokenGenerator): void {
  for (const imprt of module.imports) {
    generateTokensForImport(imprt, context, generator);
  }
  for (const cls of module.classes) {
    generateTokensForClass(cls, context, generator);
  }
  for (const stat of module.statements) {
    generateTokensForStatement(stat, context, generator);
  }
}

function processUnary(
    unary: UnaryChainExpr, context: SemanticContext): SemanticUnary {
  return {ast: unary, rhs: processExpression(unary.expr, context)};
}

function processIdentifier(
    id: IdentifierExpr|NewExpr|SpecialMethodIdentifierExpr,
    context: SemanticContext, type: IdType = 'variable',
    modifiers: string[] = []): SemanticIdentifier {
  const identifier =
      context.block.lookupOrCreateIdentifier(id, type, modifiers);
  identifier.addToken(id.token, context.filePath);
  return identifier;
}


function processAnnotation(
    annot: AnnotationExpr, context: SemanticContext): SemanticAnnotation {
  const first = processIdentifier(annot.firstPart, context, 'decorator');
  const second = annot?.secondPart?.token;
  const params = [];
  if (annot.args != null) {
    for (const arg of annot.args) {
      params.push(processExpression(arg, context));
    }
  }
  return {ast: annot, firstId: first, secondId: second, params: params};
}

function processPostfix(
    postfix: PostfixChainExpr, context: SemanticContext): SemanticPostfix {
  const base = processExpression(postfix.lhs, context) as SemanticPrimary;
  const nestedExprs: (SemanticExpression|Token)[] = [];
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
  return {ast: postfix, base: base, nestedExprs: nestedExprs};
}

function processNamedArg(
    arg: NamedArgExpr, context: SemanticContext): SemanticNamedArg {
  return {
    ast: arg,
    name: processIdentifier(arg.name, context, 'parameter'),
    value: processExpression(arg.value, context)
  };
}

function processTuple(
    tuple: TupleChainExpr, context: SemanticContext): SemanticTuple {
  return {
    ast: tuple,
    items: tuple.values.map(v => processExpression(v, context))
  };
}

function processArray(arr: ArrayExpr, context: SemanticContext): SemanticArray {
  return {ast: arr, items: arr.values.map(v => processExpression(v, context))};
}

function processMap(map: MapExpr, context: SemanticContext): SemanticMap {
  const entries = new Map<SemanticExpression, SemanticExpression>();
  for (const entry of map.entries) {
    entries.set(
        processExpression(entry.key, context),
        processExpression(entry.value, context));
  }
  return {ast: map, entries: entries};
}

function processOp(expr: OpExpr, context: SemanticContext): SemanticOp {
  return {ast: expr, exprs: expr.exprs.map(e => processExpression(e, context))};
}

function processIs(expr: IsExpr, context: SemanticContext): SemanticIs {
  return {
    ast: expr,
    lhs: processExpression(expr.lhs, context),
    rhs: processExpression(expr.rhs, context)
  };
}

function processIn(expr: InExpr, context: SemanticContext): SemanticIn {
  return {
    ast: expr,
    lhs: processExpression(expr.lhs, context),
    rhs: processExpression(expr.rhs, context)
  };
}

function processCondition(
    expr: ConditionBaseExpr, context: SemanticContext): SemanticCondition {
  return {
    ast: expr,
    cond: processExpression(expr.condition, context),
    ifTrue: processExpression(expr.ifTrue, context),
    ifFalse: expr.ifFalse != null ? processExpression(expr.ifFalse, context) :
                                    undefined
  };
}


function processRange(
    expr: RangeExpr, context: SemanticContext): SemanticRange {
  return {
    ast: expr,
    start: processExpression(expr.start, context),
    end: processExpression(expr.end, context),
    inc: expr.inc != null ? processExpression(expr.inc, context) : undefined
  };
}

function processAnon(expr: AnonExpr, context: SemanticContext): SemanticAnon {
  const params: SemanticParam[] = [];
  const newContext = context.newBlock();
  for (const param of expr.params) {
    params.push({
      ast: param,
      name: processIdentifier(param.name, newContext, 'parameter'),
      isField: param.field != null,
      defaultValue: param.defaultValue == null ?
          undefined :
          processExpression(param.defaultValue, newContext)
    });
  }
  return {
    ast: expr,
    params: params,
    stat: processStatement(expr.expr, newContext)
  };
}

function processExpression(
    expr: Expression|NamedArgExpr,
    context: SemanticContext): SemanticExpression {
  if (isConstantExpr(expr)) {
    return processConstant(expr as ConstantExpr, context);
  } else if (expr.kind === 'StringExpr') {
    return processString(expr, context);
  } else if (expr.kind === 'IdentifierExpr') {
    return processIdentifier(expr, context);
  } else if (expr.kind === 'UnaryChainExpr') {
    return processUnary(expr, context);
  } else if (expr.kind === 'AssignBaseExpr') {
    return processAssign(expr, context);
  } else if (expr.kind === 'PostfixChainExpr') {
    return processPostfix(expr, context);
  } else if (expr.kind === 'TupleChainExpr') {
    return processTuple(expr, context);
  } else if (expr.kind === 'ArrayExpr') {
    return processArray(expr, context);
  } else if (expr.kind === 'MapExpr') {
    return processMap(expr, context);
  } else if (expr.kind === 'NamedArgExpr') {
    return processNamedArg(expr, context);
  } else if (expr.kind === 'ParensExpr') {
    return {ast: expr, expr: processExpression(expr.expr, context)};
  } else if (expr.kind === 'IsExpr') {
    return processIs(expr, context);
  } else if (expr.kind === 'InExpr') {
    return processIn(expr, context);
  } else if (expr.kind === 'ConditionBaseExpr') {
    return processCondition(expr, context);
  } else if (expr.kind === 'RangeExpr') {
    return processRange(expr, context);
  } else if (expr.kind === 'AnonExpr') {
    return processAnon(expr, context);
  } else if (isOpExpr(expr)) {
    return processOp(expr as OpExpr, context);
  }
  // @ts-expect-error Okay
  return undefined;
}

function processConstant(
    expr: ConstantExpr, context: SemanticContext): SemanticExpression {
  let text: string;
  if ('tokens' in expr) {
    text = expr.tokens.map(tok => tok.text).join('');
  } else {
    text = expr.token.text;
  }
  return {
    ast: expr,
    text: text,
    value: expr.kind === 'NoneExpr' ?
        null :
        expr.kind === 'BoolExpr' ? /^True$/.test(text) : +text
  };
}


function processString(
    expr: StringExpr, context: SemanticContext): SemanticString {
  return {ast: expr, text: expr.text};
}

function processAssignLhs(
    expr: AssignLhsExpr, context: SemanticContext): SemanticAssignLhs {
  if (expr.kind === 'IdentifierExpr') {
    return {
      ast: expr,
      expr: processIdentifier(expr, context, 'variable', ['local'])
    };
  }

  if (isPostfixExpr(expr)) {
    // TODO: handle postfix
    return {ast: expr, expr: processPostfix(expr as PostfixChainExpr, context)};
  }
  return {
    ast: expr,
    expr: (expr as AssignTupleExpr | AssignArrayExpr)
              .assigns.map(a => processAssignLhs(a, context))
  };
}

function processAssign(
    expr: AssignBaseExpr, context: SemanticContext): SemanticAssign {
  return {
    ast: expr,
    lhs: processAssignLhs(expr.lhs, context),
    rhs: processExpression(expr.rhs, context)
  };
}


function processFunction(
    stat: FunctionStat, context: SemanticContext): SemanticFunction {
  const annots = [];
  if (stat.annots !== null) {
    for (const annot of stat.annots) {
      annots.push(processAnnotation(annot, context));
    }
  }
  const name = processIdentifier(stat.name, context, 'function');
  const params: SemanticParam[] = [];
  const newContext = context.newBlock();
  for (const param of stat.params) {
    params.push({
      ast: param,
      name: processIdentifier(param.name, newContext, 'parameter'),
      isField: param.field != null,
      defaultValue: param.defaultValue == null ?
          undefined :
          processExpression(param.defaultValue, newContext)
    });
  }
  return {
    ast: stat,
    name: name,
    params: params,
    stat: processStatement(stat.stat, newContext),
    annots: annots
  };
}

function processStatement(
    stat: Statement, context: SemanticContext): SemanticStatement {
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
      ifFalse: stat.elseTok != null ?
          processStatement(stat.ifFalse!, context.newBlock()) :
          undefined
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
      expr: stat.expr == null ? undefined :
                                processExpression(stat.expr, context)
    };
  } else if (stat.kind === 'TryStat') {
    const tryContext = context.newBlock();
    const catchContext = context.newBlock();
    return {
      ast: stat,
      tryStat: processStatement(stat.stat, tryContext),
      catchAssign: processAssignLhs(stat.catchAssign, catchContext),
      catchStat: processStatement(stat.catchStat, catchContext)
    };
  } else if (stat.kind === 'RaiseStat') {
    return {ast: stat, expr: processExpression(stat.expr, context)};
  } else if (stat.kind === 'FunctionStat') {
    return processFunction(stat, context);
  } else {
    return processExpression(stat as Expression, context);
  }
}

function createImportName(
    stat: ImportStat, context: SemanticContext): SemanticIdentifier {
  let name: string;
  if (stat.name == null) {
    name = stat.source.token.text;
    if (name.startsWith('\'')) {
      name = name.substring(1, name.length - 1);
      const pathParts = name.split('/');
      name = pathParts[pathParts.length - 1];
      return context.block.lookupOrCreateIdentifier(
          name, 'variable', ['defaultLibrary']);
    }
    return processIdentifier(
        stat.source as IdentifierExpr, context, 'variable', ['defaultLibrary']);
  }
  return processIdentifier(stat.name, context, 'variable', ['defaultLibrary']);
}

function processImport(
    stat: ImportStat, context: SemanticContext): SemanticImport {
  return {
    ast: stat,
    name: createImportName(stat, context),
    source: stat.source.token.text

  };
}

function processFields(
    stat: FieldStat, context: SemanticContext): SemanticField[] {
  const fields: SemanticField[] = [];
  for (const id of stat.fields) {
    fields.push({
      ast: stat,
      token: id.token,
      name: processIdentifier(id, context, 'property')
    });
  }
  return fields;
}

function processStatic(
    stat: StaticStat, context: SemanticContext): SemanticStatic {
  return {
    ast: stat,
    name: processIdentifier(stat.name, context, 'property', ['readonly']),
    expr: processExpression(stat.expr, context)
  };
}

function processMethod(
    stat: MethodStat, classStat: ClassStat, name: SemanticIdentifier,
    fields: SemanticField[], context: SemanticContext): SemanticMethod {
  const annots = [];
  if (stat.annots !== null) {
    for (const annot of stat.annots) {
      annots.push(processAnnotation(annot, context));
    }
  }
  const params: SemanticParam[] = [];
  const newContext = context.newBlock();
  for (const param of stat.params) {
    if (param.field != null) {
      fields.push({
        ast: param,
        name: processIdentifier(param.name, context, 'property'),
        token: param.name.token,
      });
    }
    params.push({
      ast: param,
      name: processIdentifier(param.name, newContext, 'parameter'),
      isField: param.field != null,
      defaultValue: param.defaultValue == null ?
          undefined :
          processExpression(param.defaultValue, context)
    });
  }
  return {
    ast: stat,
    name: name,
    params: params,
    stat: processStatement(stat.stat, newContext),
    className: classStat.name.token.text,
    annots
  };
}

function processClass(
    stat: ClassStat, name: SemanticIdentifier,
    context: SemanticContext): SemanticClass {
  const annots = [];
  if (stat.annots !== null) {
    for (const annot of stat.annots) {
      annots.push(processAnnotation(annot, context));
    }
  }

  let fields: SemanticField[] = [];

  const sup =
      stat.super != null ? processExpression(stat.super, context) : undefined;
  const statics = new Map<string, SemanticStatic>;
  const methods = new Map<string, SemanticMethod>;
  for (const classStat of stat.stats) {
    if (classStat.kind === 'FieldStat') {
      fields = fields.concat(processFields(classStat, context));
    } else if (classStat.kind === 'StaticStat') {
      const sttc = processStatic(classStat, context);
      statics.set(sttc.name.ast!.token.text, sttc);
    } else {
      processIdentifier(classStat.name, context, 'method');
    }
  }
  for (const classStat of stat.stats) {
    if (classStat.kind === 'MethodStat') {
      const meth = processMethod(
          classStat, stat, processIdentifier(classStat.name, context, 'method'),
          fields, context);
      methods.set(meth.name.id, meth);
    }
  }

  return {
    ast: stat,
    name: name,
    superName: sup,
    fields: fields,
    statics: statics,
    methods: methods,
    annots: annots
  };
}

function processModule(
    module: Module, context: SemanticContext): SemanticModule {
  const imports: SemanticImport[] = [];
  const classes: SemanticClass[] = [];
  const statements: SemanticStatement[] = [];
  for (const stat of module.statements) {
    if (stat.kind === 'ImportStat' || stat.kind === 'ImportAsStat') {
      imports.push(processImport(stat, context));
    } else if (stat.kind === 'ClassStat') {
      processIdentifier(stat.name, context, 'class', []);
    } else if (stat.kind === 'FunctionStat') {
      processIdentifier(stat.name, context, 'function', []);
    }
  }
  for (const stat of module.statements) {
    if (stat.kind === 'ClassStat') {
      classes.push(processClass(
          stat, processIdentifier(stat.name, context, 'class', []),
          context.newBlock()));
    }
  }
  for (const stat of module.statements) {
    if (stat.kind !== 'ImportStat' && stat.kind !== 'ImportAsStat' &&
        stat.kind !== 'ClassStat') {
      const sstat = processStatement(stat, context);
      if (sstat != null) {
        statements.push(sstat);
      }
    }
  }

  const semanticModule: SemanticModule =
      {ast: module, imports: imports, classes: classes, statements: statements};
  context.module = semanticModule;
  return semanticModule;
}

export function generateSemanticTokens(
    filePath: string, analyzer: SemanticAnalyzer,
    module: Module): [SemanticContext, SemanticModule, SemanticToken[]] {
  const context = new SemanticContext(analyzer, filePath);
  const generator = new TokenGenerator(filePath);
  const semanticModule = processModule(module, context);
  generatTokensForModule(semanticModule, context, generator);
  return [context, semanticModule, generator.finalizeTokens()];
}