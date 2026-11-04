// A recursive-descent parser for Zinnia that recovers from syntax errors.
//
// The grammar is a PEG: alternatives are tried in order and the first match
// wins, and repetition is greedy. Each rule's result is cached per position
// (packrat parsing), so the parser runs in linear time even though rules
// re-parse prefixes when an alternative fails.
//
// The parser first parses strictly. If that fails, it parses again in recovery
// mode, where a statement or class member that doesn't parse is reported and
// skipped, so the rest of the file still yields an AST.

import {AddExpr, AndExpr, AnnotationExpr, AnonExpr, ArrayExpr, ArrayIndexExpr, AssignArrayExpr, AssignExpr, AssignLhsExpr, AssignTupleExpr, BinaryExpr, ConditionExpr, ConstantExpr, EmptyParensExpr, EqualExpr, Expression, FunctionCallExpr, IdentifierExpr, InExpr, IsExpr, LengthExpr, MapEntryExpr, MapExpr, MemberAccessExpr, MultExpr, NamedArgExpr, NewExpr, OrExpr, ParamExpr, ParensExpr, PostfixExpr, PostfixExpr1, PrimaryExpr, RelationExpr, StringExpr, TupleExpr, UnaryExpr} from './expressions';
import {ClassMemberStat, ClassStat, CompoundStat, ExitStat, FieldStat, ForeachStat, ForStat, FunctionStat, ImportStat, IterStat, JumpStat, MethodStat, Module, RaiseStat, SelectStat, SpecialMethodIdentifierExpr, Statement, StaticStat, TryStat, WhileStat} from './statements';
import {Token, TokenKind, tokenize} from './tokenizer';

// A problem found in a document. Rows and columns are 0-based. A problem with
// no position is at the end of the document. Problems are errors unless they
// say otherwise.
export interface ParseProblem {
  message: string;
  row?: number;
  col?: number;
  // How many characters the problem spans, if more than one.
  length?: number;
  severity?: 'error'|'warning'|'hint';
  // Where the problem ends, if it spans lines, 0-based.
  endRow?: number;
  endCol?: number;
  // Whether the code is unneeded: unused or unreachable.
  unnecessary?: boolean;
  // What kind of problem this is, for the problems that have quick fixes.
  code?: string;
  // For a hidden top-level variable: where it's declared, 0-based.
  declaration?: {row: number, col: number};
}

export interface ParseResult {
  module: Module;
  problems: ParseProblem[];
}

// The result of a rule that matched: its value and the index after it.
interface Match<T> {
  v: T;
  i: number;
}

type Result<T> = Match<T>|undefined;

const IDENTIFIER_KINDS = new Set([
  TokenKind.IDENTIFIER, TokenKind.KEYWORD_CLASS, TokenKind.KEYWORD_MODULE,
  TokenKind.KEYWORD_ASYNC, TokenKind.KEYWORD_THEN, TokenKind.KEYWORD_FUNCTION
]);

const OPENERS = new Set(
    [TokenKind.SYMBOL_LPAREN, TokenKind.SYMBOL_LBRACKET, TokenKind.SYMBOL_LBRACE]);
const CLOSERS = new Set(
    [TokenKind.SYMBOL_RPAREN, TokenKind.SYMBOL_RBRACKET, TokenKind.SYMBOL_RBRACE]);

// How each kind of token is named in error messages.
function describeKind(kind: TokenKind): string {
  switch (kind) {
    case TokenKind.IDENTIFIER:
      return 'a name';
    case TokenKind.LITERAL_STRING:
      return 'a string';
    case TokenKind.LITEARL_INTEGER:
    case TokenKind.LITEARL_FLOAT:
      return 'a number';
    case TokenKind.NEWLINE:
      return 'a new line';
  }
  const name = TokenKind[kind];
  if (name.startsWith('KEYWORD_')) {
    const keyword = name.slice('KEYWORD_'.length).toLowerCase();
    return `'${keyword}'`;
  }
  return SYMBOL_TEXT.get(kind) ?? name;
}

const SYMBOL_TEXT = new Map<TokenKind, string>([
  [TokenKind.SYMBOL_LPAREN, '\'(\''],   [TokenKind.SYMBOL_RPAREN, '\')\''],
  [TokenKind.SYMBOL_LBRACE, '\'{\''],   [TokenKind.SYMBOL_RBRACE, '\'}\''],
  [TokenKind.SYMBOL_LBRACKET, '\'[\''], [TokenKind.SYMBOL_RBRACKET, '\']\''],
  [TokenKind.SYMBOL_EQUALS, '\'=\''],   [TokenKind.SYMBOL_COMMA, '\',\''],
  [TokenKind.SYMBOL_COLON, '\':\''],    [TokenKind.SYMBOL_PERIOD, '\'.\''],
  [TokenKind.SYMBOL_RARROW, '\'->\''],  [TokenKind.SYMBOL_PIPE, '\'|\''],
]);

class Parser {
  private readonly memo = new Map<string, Map<number, Result<unknown>>>();
  // The furthest index at which a token failed to match, and what was
  // expected there, for error messages.
  private furthest = -1;
  private expected = new Set<TokenKind>();
  readonly problems: ParseProblem[] = [];

  constructor(private readonly toks: Token[], private readonly recover: boolean) {}

  // Infrastructure

  private fail(i: number, kind: TokenKind): undefined {
    if (i > this.furthest) {
      this.furthest = i;
      this.expected = new Set([kind]);
    } else if (i === this.furthest) {
      this.expected.add(kind);
    }
    return undefined;
  }

  private tok(i: number, kind: TokenKind): Result<Token> {
    const t = this.toks[i];
    if (t !== undefined && t.kind === kind) {
      return {v: t, i: i + 1};
    }
    return this.fail(i, kind);
  }

  private tokIn(i: number, kinds: TokenKind[]): Result<Token> {
    for (const kind of kinds) {
      const r = this.tok(i, kind);
      if (r !== undefined) {
        return r;
      }
    }
    return undefined;
  }

  private cached<T>(name: string, i: number, parse: () => Result<T>): Result<T> {
    let table = this.memo.get(name);
    if (table === undefined) {
      table = new Map();
      this.memo.set(name, table);
    }
    if (table.has(i)) {
      return table.get(i) as Result<T>;
    }
    const r = parse();
    table.set(i, r);
    return r;
  }

  // Skips new lines.
  private lines(i: number): number {
    while (this.toks[i]?.kind === TokenKind.NEWLINE) {
      i++;
    }
    return i;
  }

  // Parses [item] separated by [sep], at least once. A separator not followed
  // by an item is left unconsumed.
  private list<T>(i: number, item: (i: number) => Result<T>, sep: TokenKind):
      Result<T[]> {
    const first = item(i);
    if (first === undefined) {
      return undefined;
    }
    const values = [first.v];
    i = first.i;
    while (true) {
      const s = this.tok(i, sep);
      const next = s && item(s.i);
      if (next === undefined) {
        break;
      }
      values.push(next.v);
      i = next.i;
    }
    return {v: values, i: i};
  }

  // Parses [first] followed by any number of (one of [ops], [operand]), as
  // used by the binary operator rules.
  private chain<T>(
      i: number, operand: (i: number) => Result<T>, ops: TokenKind[]):
      Result<[T, Token[], T[]]> {
    const first = operand(i);
    if (first === undefined) {
      return undefined;
    }
    i = first.i;
    const tokens: Token[] = [];
    const exprs = [first.v];
    while (true) {
      const op = this.tokIn(i, ops);
      const rhs = op && operand(op.i);
      if (rhs === undefined) {
        break;
      }
      tokens.push(op!.v);
      exprs.push(rhs.v);
      i = rhs.i;
    }
    return {v: [first.v, tokens, exprs], i: i};
  }

  // Literals and identifiers

  identifier(i: number): Result<IdentifierExpr> {
    const t = this.toks[i];
    if (t !== undefined && IDENTIFIER_KINDS.has(t.kind)) {
      return {v: {kind: 'IdentifierExpr', token: t}, i: i + 1};
    }
    return this.fail(i, TokenKind.IDENTIFIER);
  }

  private newExpr(i: number): Result<NewExpr> {
    const r = this.tok(i, TokenKind.KEYWORD_NEW);
    return r && {v: {kind: 'NewExpr', token: r.v}, i: r.i};
  }

  private constant(i: number): Result<ConstantExpr> {
    const bool = this.tokIn(i, [TokenKind.KEYWORD_TRUE, TokenKind.KEYWORD_FALSE]);
    if (bool !== undefined) {
      return {
        v: {kind: 'BoolExpr', token: bool.v, value: (/True/i).test(bool.v.text)},
        i: bool.i
      };
    }
    const none = this.tok(i, TokenKind.KEYWORD_NULL);
    if (none !== undefined) {
      return {v: {kind: 'NoneExpr', token: none.v}, i: none.i};
    }
    for (const [kind, exprKind] of [
             [TokenKind.LITEARL_FLOAT, 'FloatExpr'],
             [TokenKind.LITEARL_INTEGER, 'IntExpr']
    ] as const) {
      const minus = this.tok(i, TokenKind.SYMBOL_MINUS);
      const number = this.tok(minus?.i ?? i, kind);
      if (number !== undefined) {
        const tokens = minus ? [minus.v, number.v] : [number.v];
        const text = minus ? '-' + number.v.text : number.v.text;
        return {
          v: {kind: exprKind, tokens: tokens, value: Number(text)} as ConstantExpr,
          i: number.i
        };
      }
    }
    return undefined;
  }

  private string(i: number): Result<StringExpr> {
    const r = this.tok(i, TokenKind.LITERAL_STRING);
    return r && {v: {kind: 'StringExpr', token: r.v, text: r.v.text}, i: r.i};
  }

  // Structures

  private array(i: number): Result<ArrayExpr> {
    const empty = this.tok(i, TokenKind.SYMBOL_ARRAY_ACCESS);
    if (empty !== undefined) {
      return {v: {kind: 'ArrayExpr', values: []}, i: empty.i};
    }
    const lbrack = this.tok(i, TokenKind.SYMBOL_LBRACKET);
    if (lbrack === undefined) {
      return undefined;
    }
    const tuple = this.tuple(lbrack.i);
    const rbrack = this.tok(tuple?.i ?? lbrack.i, TokenKind.SYMBOL_RBRACKET);
    if (rbrack === undefined) {
      return undefined;
    }
    const values = tuple === undefined ? [] :
        tuple.v.kind === 'TupleChainExpr' ? tuple.v.values :
                                            [tuple.v];
    return {v: {kind: 'ArrayExpr', values: values}, i: rbrack.i};
  }

  private mapEntry(i: number): Result<MapEntryExpr> {
    const key = this.postfix(this.lines(i));
    const colon = key && this.tok(key.i, TokenKind.SYMBOL_COLON);
    const value = colon && this.postfix(colon.i);
    if (value === undefined) {
      return undefined;
    }
    return {
      v: {kind: 'MapEntryExpr', key: key!.v, value: value.v},
      i: this.lines(value.i)
    };
  }

  private map(i: number): Result<MapExpr> {
    const lbrace = this.tok(this.lines(i), TokenKind.SYMBOL_LBRACE);
    if (lbrace === undefined) {
      return undefined;
    }
    const start = this.lines(lbrace.i);
    const entries = this.list(start, j => this.mapEntry(j), TokenKind.SYMBOL_COMMA);
    const rbrace =
        this.tok(this.lines(entries?.i ?? start), TokenKind.SYMBOL_RBRACE);
    if (rbrace === undefined) {
      return undefined;
    }
    return {
      v: {kind: 'MapExpr', entries: entries?.v ?? []},
      i: this.lines(rbrace.i)
    };
  }

  // Primary

  private parens(i: number): Result<ParensExpr> {
    const lparen = this.tok(i, TokenKind.SYMBOL_LPAREN);
    const expr = lparen && this.tuple(lparen.i);
    const rparen = expr && this.tok(expr.i, TokenKind.SYMBOL_RPAREN);
    if (rparen === undefined) {
      return undefined;
    }
    return {
      v: {kind: 'ParensExpr', expr: expr!.v, lparen: lparen!.v, rparen: rparen.v},
      i: rparen.i
    };
  }

  private length(i: number): Result<LengthExpr> {
    const lpipe = this.tok(i, TokenKind.SYMBOL_PIPE);
    const expr = lpipe && this.tuple(lpipe.i);
    const rpipe = expr && this.tok(expr.i, TokenKind.SYMBOL_PIPE);
    if (rpipe === undefined) {
      return undefined;
    }
    return {
      v: {kind: 'LengthExpr', expr: expr!.v, lpipe: lpipe!.v, rpipe: rpipe.v},
      i: rpipe.i
    };
  }

  private primary(i: number, withConstants: boolean): Result<PrimaryExpr> {
    return this.anon(i) ?? this.identifier(i) ??
        (withConstants ? this.newExpr(i) ?? this.constant(i) : undefined) ??
        this.string(i) ?? this.array(i) ?? this.map(i) ?? this.parens(i) ??
        this.length(i);
  }

  // Postfix

  private namedArg(i: number): Result<NamedArgExpr> {
    const name = this.identifier(this.lines(i));
    const colon = name && this.tok(this.lines(name.i), TokenKind.SYMBOL_COLON);
    const value = colon && this.condition(this.lines(colon.i));
    if (value === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'NamedArgExpr',
        name: name!.v,
        value: value.v,
        colonToken: colon!.v
      },
      i: this.lines(value.i)
    };
  }

  // Parses `name: value, ...` or a tuple, as in calls and annotations.
  private args(i: number): Result<Expression[]|NamedArgExpr[]> {
    const named = this.list(i, j => this.namedArg(j), TokenKind.SYMBOL_COMMA);
    if (named !== undefined) {
      return named;
    }
    const tuple = this.tuple(i);
    if (tuple === undefined) {
      return undefined;
    }
    return {
      v: tuple.v.kind === 'TupleChainExpr' ? tuple.v.values : [tuple.v],
      i: tuple.i
    };
  }

  private postfixSuffix(i: number): Result<PostfixExpr1> {
    const lbrack = this.tok(i, TokenKind.SYMBOL_LBRACKET);
    if (lbrack !== undefined) {
      const indices = this.tuple(lbrack.i);
      const rbrack = indices && this.tok(indices.i, TokenKind.SYMBOL_RBRACKET);
      if (rbrack !== undefined) {
        return {
          v: {
            kind: 'ArrayIndexExpr',
            indices: indices!.v,
            lbrack: lbrack.v,
            rbrack: rbrack.v
          } satisfies ArrayIndexExpr,
          i: rbrack.i
        };
      }
    }
    const lparen = this.tok(i, TokenKind.SYMBOL_LPAREN);
    if (lparen !== undefined) {
      const rparen = this.tok(lparen.i, TokenKind.SYMBOL_RPAREN);
      if (rparen !== undefined) {
        return {
          v: {
            kind: 'EmptyParensExpr',
            lparen: lparen.v,
            rparen: rparen.v
          } satisfies EmptyParensExpr,
          i: rparen.i
        };
      }
      const args = this.args(lparen.i);
      const close = args && this.tok(args.i, TokenKind.SYMBOL_RPAREN);
      if (close !== undefined) {
        return {
          v: {
            kind: 'FunctionCallExpr',
            args: args!.v,
            lparen: lparen.v,
            rparen: close.v
          } satisfies FunctionCallExpr,
          i: close.i
        };
      }
    }
    const period = this.tok(this.lines(i), TokenKind.SYMBOL_PERIOD);
    const field = period && (this.identifier(period.i) ?? this.newExpr(period.i));
    if (field !== undefined) {
      return {
        v: {
          kind: 'MemberAccessExpr',
          field: field.v,
          period: period!.v
        } satisfies MemberAccessExpr,
        i: field.i
      };
    }
    return undefined;
  }

  postfix(i: number): Result<PostfixExpr> {
    return this.cached<PostfixExpr>('postfix', i, () => {
      const primary = this.primary(i, false);
      if (primary !== undefined) {
        const postfixes: PostfixExpr1[] = [];
        let j = primary.i;
        while (true) {
          const suffix = this.postfixSuffix(j);
          if (suffix === undefined) {
            break;
          }
          postfixes.push(suffix.v);
          j = suffix.i;
        }
        const value: PostfixExpr = postfixes.length === 0 ? primary.v : {
          kind: 'PostfixChainExpr',
          lhs: primary.v,
          postfixes: postfixes
        };
        return {v: value, i: this.lines(j)};
      }
      return this.primary(i, true);
    });
  }

  // Operators

  private range(i: number): Result<PostfixExpr> {
    const start = this.postfix(i);
    if (start === undefined) {
      return undefined;
    }
    const colon1 = this.tok(start.i, TokenKind.SYMBOL_COLON);
    const end = colon1 && this.postfix(colon1.i);
    if (end === undefined) {
      return start;
    }
    const colon2 = this.tok(end.i, TokenKind.SYMBOL_COLON);
    const inc = colon2 && this.postfix(colon2.i);
    const value = {kind: 'RangeExpr', start: start.v, end: end.v} as any;
    if (inc !== undefined) {
      value.inc = inc.v;
    }
    return {v: value, i: inc?.i ?? end.i};
  }

  private unary(i: number): Result<UnaryExpr> {
    return this.cached<UnaryExpr>('unary', i, () => {
      const unaries: Token[] = [];
      let j = i;
      while (true) {
        const op = this.tokIn(j, [
          TokenKind.SYMBOL_TILDE, TokenKind.SYMBOL_EXCLAIM,
          TokenKind.SYMBOL_MINUS, TokenKind.KEYWORD_AWAIT
        ]);
        if (op === undefined) {
          break;
        }
        unaries.push(op.v);
        j = op.i;
      }
      const expr = this.range(j);
      if (expr === undefined) {
        return undefined;
      }
      if (unaries.length === 0) {
        return expr;
      }
      return {
        v: {kind: 'UnaryChainExpr', expr: expr.v, unaries: unaries},
        i: expr.i
      };
    });
  }

  // Builds a chain expression of [kind] if there is more than one operand.
  private chainExpr<T, R>(
      kind: string, r: Result<[T, Token[], T[]]>): Result<R> {
    if (r === undefined) {
      return undefined;
    }
    const [first, tokens, exprs] = r.v;
    if (tokens.length === 0) {
      return {v: first as unknown as R, i: r.i};
    }
    return {v: {kind: kind, exprs: exprs, tokens: tokens} as R, i: r.i};
  }

  private binaryAnd(i: number): Result<BinaryExpr> {
    return this.chainExpr(
        'BinaryChainExpr',
        this.chain(i, j => this.unary(j), [TokenKind.SYMBOL_AMPER]));
  }

  private binaryXor(i: number): Result<BinaryExpr> {
    return this.chainExpr(
        'BinaryChainExpr',
        this.chain(i, j => this.binaryAnd(j), [TokenKind.SYMBOL_CARET]));
  }

  private binaryOr(i: number): Result<BinaryExpr> {
    return this.chainExpr(
        'BinaryChainExpr',
        this.chain(i, j => this.binaryXor(j), [TokenKind.SYMBOL_PIPE]));
  }

  private mult(i: number): Result<MultExpr> {
    return this.chainExpr(
        'MultChainExpr',
        this.chain(i, j => this.binaryOr(j), [
          TokenKind.SYMBOL_STAR, TokenKind.SYMBOL_FSLASH, TokenKind.SYMBOL_PERCENT
        ]));
  }

  private add(i: number): Result<AddExpr> {
    return this.chainExpr(
        'AddChainExpr',
        this.chain(
            i, j => this.mult(j), [TokenKind.SYMBOL_PLUS, TokenKind.SYMBOL_MINUS]));
  }

  private inExpr(i: number): Result<InExpr|AddExpr> {
    const lhs = this.add(i);
    if (lhs === undefined) {
      return undefined;
    }
    const op =
        this.tokIn(lhs.i, [TokenKind.KEYWORD_IN, TokenKind.KEYWORD_NOTIN]);
    const rhs = op && this.add(op.i);
    if (rhs === undefined) {
      return lhs;
    }
    return {
      v: {kind: 'InExpr', lhs: lhs.v, rhs: rhs.v, inTok: op!.v},
      i: rhs.i
    };
  }

  private isExpr(i: number): Result<IsExpr|InExpr|AddExpr> {
    const lhs = this.inExpr(i);
    if (lhs === undefined) {
      return undefined;
    }
    const op = this.tok(lhs.i, TokenKind.KEYWORD_IS);
    const rhs = op && this.inExpr(op.i);
    if (rhs === undefined) {
      return lhs;
    }
    return {
      v: {kind: 'IsExpr', lhs: lhs.v, rhs: rhs.v, isTok: op!.v},
      i: rhs.i
    };
  }

  private relation(i: number): Result<RelationExpr> {
    return this.chainExpr(
        'RelationChainExpr', this.chain(i, j => this.isExpr(j), [
          TokenKind.SYMBOL_LTHAN, TokenKind.SYMBOL_GTHAN,
          TokenKind.SYMBOL_LTHANEQ, TokenKind.SYMBOL_GTHANEQ
        ]));
  }

  private equal(i: number): Result<EqualExpr> {
    return this.chainExpr(
        'EqualChainExpr',
        this.chain(
            i, j => this.relation(j),
            [TokenKind.SYMBOL_EQUIV, TokenKind.SYMBOL_NEQUIV]));
  }

  private and(i: number): Result<AndExpr> {
    return this.chainExpr(
        'AndChainExpr',
        this.chain(i, j => this.equal(j), [TokenKind.KEYWORD_AND]));
  }

  private or(i: number): Result<OrExpr> {
    return this.cached(
        'or',
        i,
        () => this.chainExpr(
            'OrChainExpr', this.chain(i, j => this.and(j), [TokenKind.KEYWORD_OR])));
  }

  condition(i: number): Result<ConditionExpr> {
    return this.cached<ConditionExpr>('condition', i, () => {
      const ifTok = this.tok(i, TokenKind.KEYWORD_IF);
      if (ifTok !== undefined) {
        const cond = this.or(ifTok.i);
        if (cond !== undefined) {
          const thenTok = this.tok(cond.i, TokenKind.KEYWORD_THEN);
          const ifTrue = this.condition(thenTok?.i ?? cond.i);
          if (ifTrue !== undefined) {
            const elseTok = this.tok(ifTrue.i, TokenKind.KEYWORD_ELSE);
            const ifFalse = elseTok && this.condition(elseTok.i);
            if (ifFalse !== undefined) {
              return {
                v: {
                  kind: 'ConditionBaseExpr',
                  if: ifTok.v,
                  condition: cond.v,
                  then: thenTok?.v,
                  ifTrue: ifTrue.v,
                  else: elseTok!.v,
                  ifFalse: ifFalse.v,
                },
                i: ifFalse.i
              };
            }
            return {
              v: {
                kind: 'ConditionBaseExpr',
                if: ifTok.v,
                condition: cond.v,
                then: thenTok?.v,
                ifTrue: ifTrue.v,
              },
              i: ifTrue.i
            };
          }
        }
      }
      return this.or(i);
    });
  }

  // Assignment

  private assignTupleOrArray(
      i: number, open: TokenKind, close: TokenKind):
      Result<[Token, AssignLhsExpr[], Token]> {
    const opener = this.tok(i, open);
    const assigns =
        opener && this.list(opener.i, j => this.assignLhs(j), TokenKind.SYMBOL_COMMA);
    const closer = assigns && this.tok(assigns.i, close);
    if (closer === undefined) {
      return undefined;
    }
    return {v: [opener!.v, assigns!.v, closer.v], i: closer.i};
  }

  assignLhs(i: number): Result<AssignLhsExpr> {
    return this.cached<AssignLhsExpr>('assignLhs', i, () => {
      const constTok = this.tok(i, TokenKind.KEYWORD_CONST);
      const constId = constTok && this.identifier(constTok.i);
      if (constId !== undefined) {
        return {v: {...constId.v, constTok: constTok!.v}, i: constId.i};
      }
      const tuple = this.assignTupleOrArray(
          i, TokenKind.SYMBOL_LPAREN, TokenKind.SYMBOL_RPAREN);
      if (tuple !== undefined) {
        const [lparen, assigns, rparen] = tuple.v;
        return {
          v: {
            kind: 'AssignTupleExpr',
            assigns: assigns,
            lparen: lparen,
            rparen: rparen
          } satisfies AssignTupleExpr,
          i: tuple.i
        };
      }
      const array = this.assignTupleOrArray(
          i, TokenKind.SYMBOL_LBRACKET, TokenKind.SYMBOL_RBRACKET);
      if (array !== undefined) {
        const [lbrack, assigns, rbrack] = array.v;
        return {
          v: {
            kind: 'AssignArrayExpr',
            assigns: assigns,
            lbrack: lbrack,
            rbrack: rbrack
          } satisfies AssignArrayExpr,
          i: array.i
        };
      }
      return this.postfix(i) ?? this.identifier(i);
    });
  }

  assign(i: number): Result<AssignExpr> {
    return this.cached<AssignExpr>('assign', i, () => {
      const lhs = this.assignLhs(i);
      const eq = lhs && this.tok(lhs.i, TokenKind.SYMBOL_EQUALS);
      const rhs = eq && this.condition(eq.i);
      if (rhs !== undefined) {
        return {
          v: {kind: 'AssignBaseExpr', lhs: lhs!.v, rhs: rhs.v, eq: eq!.v},
          i: rhs.i
        };
      }
      return this.condition(i);
    });
  }

  tuple(i: number): Result<TupleExpr> {
    return this.cached<TupleExpr>('tuple', i, () => {
      const exprs = this.list(i, j => {
        const r = this.assign(this.lines(j));
        return r && {v: r.v, i: this.lines(r.i)};
      }, TokenKind.SYMBOL_COMMA);
      if (exprs === undefined) {
        return undefined;
      }
      if (exprs.v.length === 1) {
        return {v: exprs.v[0], i: exprs.i};
      }
      return {v: {kind: 'TupleChainExpr', values: exprs.v}, i: exprs.i};
    });
  }

  private expression(i: number): Result<Expression> {
    const tuple = this.tuple(i);
    return tuple && {v: tuple.v, i: this.lines(tuple.i)};
  }

  // Statements

  private foreach(i: number): Result<ForeachStat> {
    const forTok = this.tok(i, TokenKind.KEYWORD_FOR);
    if (forTok === undefined) {
      return undefined;
    }
    const lparen = this.tok(forTok.i, TokenKind.SYMBOL_LPAREN);
    if (lparen !== undefined) {
      const lhs = this.assignLhs(lparen.i);
      const inTok = lhs && this.tok(lhs.i, TokenKind.KEYWORD_IN);
      const iter = inTok && this.condition(inTok.i);
      const rparen = iter && this.tok(iter.i, TokenKind.SYMBOL_RPAREN);
      const stat = rparen && this.statement(rparen.i);
      if (stat !== undefined) {
        return {
          v: {
            kind: 'ForeachStat',
            forTok: forTok.v,
            lparen: lparen.v,
            lhs: lhs!.v,
            inTok: inTok!.v,
            iter: iter!.v,
            rparen: rparen!.v,
            stat: stat.v
          },
          i: stat.i
        };
      }
    }
    const lhs = this.assignLhs(forTok.i);
    const inTok = lhs && this.tok(lhs.i, TokenKind.KEYWORD_IN);
    const iter = inTok && this.condition(inTok.i);
    const stat = iter && this.statement(iter.i);
    if (stat === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'ForeachStat',
        forTok: forTok.v,
        lhs: lhs!.v,
        inTok: inTok!.v,
        iter: iter!.v,
        stat: stat.v
      },
      i: stat.i
    };
  }

  private forStat(i: number): Result<ForStat> {
    const forTok = this.tok(i, TokenKind.KEYWORD_FOR);
    if (forTok === undefined) {
      return undefined;
    }
    const lparen = this.tok(forTok.i, TokenKind.SYMBOL_LPAREN);
    if (lparen !== undefined) {
      const first = this.assign(lparen.i);
      const comma1 = first && this.tok(first.i, TokenKind.SYMBOL_COMMA);
      const second = comma1 && this.condition(comma1.i);
      const comma2 = second && this.tok(second.i, TokenKind.SYMBOL_COMMA);
      const third = comma2 && this.assign(comma2.i);
      const rparen = third && this.tok(third.i, TokenKind.SYMBOL_RPAREN);
      const stat = rparen && this.statement(rparen.i);
      if (stat !== undefined) {
        return {
          v: {
            kind: 'ForStat',
            forTok: forTok.v,
            lparen: lparen.v,
            first: first!.v,
            comma1: comma1!.v,
            second: second!.v,
            comma2: comma2!.v,
            third: third!.v,
            rparen: rparen!.v,
            stat: stat.v
          },
          i: stat.i
        };
      }
    }
    const first = this.assign(forTok.i);
    const comma1 = first && this.tok(first.i, TokenKind.SYMBOL_COMMA);
    const second = comma1 && this.condition(comma1.i);
    const comma2 = second && this.tok(second.i, TokenKind.SYMBOL_COMMA);
    const third = comma2 && this.assign(comma2.i);
    const stat = third && this.statement(third.i);
    if (stat === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'ForStat',
        forTok: forTok.v,
        first: first!.v,
        comma1: comma1!.v,
        second: second!.v,
        comma2: comma2!.v,
        third: third!.v,
        stat: stat.v
      },
      i: stat.i
    };
  }

  private whileStat(i: number): Result<WhileStat> {
    const whileTok = this.tok(i, TokenKind.KEYWORD_WHILE);
    if (whileTok === undefined) {
      return undefined;
    }
    const lparen = this.tok(whileTok.i, TokenKind.SYMBOL_LPAREN);
    if (lparen !== undefined) {
      const cond = this.condition(lparen.i);
      const rparen = cond && this.tok(cond.i, TokenKind.SYMBOL_RPAREN);
      const stat = rparen && this.statement(rparen.i);
      if (stat !== undefined) {
        return {
          v: {
            kind: 'WhileStat',
            whileTok: whileTok.v,
            lparen: lparen.v,
            cond: cond!.v,
            rparen: rparen!.v,
            stat: stat.v
          },
          i: stat.i
        };
      }
    }
    const cond = this.condition(whileTok.i);
    const stat = cond && this.statement(cond.i);
    if (stat === undefined) {
      return undefined;
    }
    return {
      v: {kind: 'WhileStat', whileTok: whileTok.v, cond: cond!.v, stat: stat.v},
      i: stat.i
    };
  }

  private iter(i: number): Result<IterStat> {
    return this.foreach(i) ?? this.forStat(i) ?? this.whileStat(i);
  }

  private compound(i: number): Result<CompoundStat> {
    return this.cached<CompoundStat>('compound', i, () => {
      const lbrace = this.tok(this.lines(i), TokenKind.SYMBOL_LBRACE);
      if (lbrace === undefined) {
        return undefined;
      }
      const body = this.items(
          this.lines(lbrace.i), j => this.statement(j), /*inBlock=*/ true);
      const end = this.lines(body.i);
      const rbrace = this.tok(end, TokenKind.SYMBOL_RBRACE);
      if (rbrace === undefined) {
        const last = this.closeUnclosedBlock(lbrace.v, end);
        return last && {
          v: {kind: 'CompoundStat', lbrace: lbrace.v, stats: body.v, rbrace: last},
          i: end
        };
      }
      return {
        v: {
          kind: 'CompoundStat',
          lbrace: lbrace.v,
          stats: body.v,
          rbrace: rbrace.v
        },
        i: this.lines(rbrace.i)
      };
    });
  }

  private select(i: number): Result<SelectStat> {
    const ifTok = this.tok(i, TokenKind.KEYWORD_IF);
    const cond = ifTok && this.tuple(ifTok.i);
    const ifTrue = cond && this.statement(cond.i);
    if (ifTrue === undefined) {
      return undefined;
    }
    const elseTok = this.tok(ifTrue.i, TokenKind.KEYWORD_ELSE);
    const ifFalse = elseTok && this.statement(elseTok.i);
    if (ifFalse !== undefined) {
      return {
        v: {
          kind: 'SelectStat',
          ifTok: ifTok!.v,
          cond: cond!.v,
          ifTrue: ifTrue.v,
          elseTok: elseTok!.v,
          ifFalse: ifFalse.v
        },
        i: ifFalse.i
      };
    }
    return {
      v: {kind: 'SelectStat', ifTok: ifTok!.v, cond: cond!.v, ifTrue: ifTrue.v},
      i: ifTrue.i
    };
  }

  private exit(i: number): Result<ExitStat> {
    const exit = this.tok(i, TokenKind.KEYWORD_EXIT);
    if (exit === undefined) {
      return undefined;
    }
    const expr = this.assign(exit.i);
    return {
      v: {kind: 'ExitStat', exit: exit.v, expr: expr?.v},
      i: expr?.i ?? exit.i
    };
  }

  private raise(i: number): Result<RaiseStat> {
    const raise = this.tok(i, TokenKind.KEYWORD_RAISE);
    const expr = raise && this.assign(raise.i);
    if (expr === undefined) {
      return undefined;
    }
    return {v: {kind: 'RaiseStat', raise: raise!.v, expr: expr.v}, i: expr.i};
  }

  private tryStat(i: number): Result<TryStat> {
    const tryTok = this.tok(i, TokenKind.KEYWORD_TRY);
    const stat = tryTok && this.statement(tryTok.i);
    const catchTok = stat && this.tok(stat.i, TokenKind.KEYWORD_CATCH);
    if (catchTok === undefined) {
      return undefined;
    }
    let catchAssign: Result<AssignLhsExpr>;
    const lparen = this.tok(catchTok.i, TokenKind.SYMBOL_LPAREN);
    const inner = lparen && this.assignLhs(lparen.i);
    const rparen = inner && this.tok(inner.i, TokenKind.SYMBOL_RPAREN);
    if (rparen !== undefined) {
      catchAssign = {v: inner!.v, i: rparen.i};
    } else {
      catchAssign = this.assignLhs(catchTok.i);
    }
    const catchStat = catchAssign && this.statement(catchAssign.i);
    if (catchStat === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'TryStat',
        tryTok: tryTok!.v,
        stat: stat!.v,
        catchTok: catchTok.v,
        catchAssign: catchAssign!.v,
        catchStat: catchStat.v
      },
      i: catchStat.i
    };
  }

  private jump(i: number): Result<JumpStat> {
    i = this.lines(i);
    const ret = this.tok(i, TokenKind.KEYWORD_RETURN);
    if (ret !== undefined) {
      const expr = this.tuple(ret.i);
      if (expr !== undefined) {
        return {
          v: {kind: 'JumpStat', token: ret.v, expr: expr.v},
          i: this.lines(expr.i)
        };
      }
      return {v: {kind: 'JumpStat', token: ret.v}, i: this.lines(ret.i)};
    }
    const jump =
        this.tokIn(i, [TokenKind.KEYWORD_BREAK, TokenKind.KEYWORD_CONTINUE]);
    return jump &&
        {v: {kind: 'JumpStat', token: jump.v}, i: this.lines(jump.i)};
  }

  statement(i: number): Result<Statement> {
    return this.cached<Statement>('statement', i, () => {
      const j = this.lines(i);
      const r: Result<Statement> = this.exit(j) ?? this.raise(j) ??
          this.tryStat(j) ?? this.select(j) ?? this.compound(j) ??
          this.iter(j) ?? this.jump(j) ?? this.expression(j);
      return r && {v: r.v, i: this.lines(r.i)};
    });
  }

  // Functions and classes

  private param(i: number): Result<ParamExpr> {
    const j = this.lines(i);
    const field = this.tok(j, TokenKind.KEYWORD_FIELD);
    const name = this.identifier(field?.i ?? j);
    if (name === undefined) {
      return undefined;
    }
    const eq = this.tok(name.i, TokenKind.SYMBOL_EQUALS);
    const defaultValue = eq && this.condition(eq.i);
    if (defaultValue !== undefined) {
      return {
        v: {
          kind: 'ParamExpr',
          field: field?.v,
          name: name.v,
          eq: eq!.v,
          defaultValue: defaultValue.v
        },
        i: this.lines(defaultValue.i)
      };
    }
    return {
      v: {kind: 'ParamExpr', name: name.v, field: field?.v},
      i: this.lines(name.i)
    };
  }

  // Parses the parameters between parentheses: `{a, b}` (named), `a, b` or
  // nothing.
  private params(i: number): Match<{params: ParamExpr[], isNamed: boolean}> {
    const lbrace = this.tok(i, TokenKind.SYMBOL_LBRACE);
    const named =
        lbrace && this.list(lbrace.i, j => this.param(j), TokenKind.SYMBOL_COMMA);
    const rbrace = named && this.tok(named.i, TokenKind.SYMBOL_RBRACE);
    if (rbrace !== undefined) {
      return {v: {params: named!.v, isNamed: true}, i: rbrace.i};
    }
    const positional = this.list(i, j => this.param(j), TokenKind.SYMBOL_COMMA);
    if (positional !== undefined) {
      return {v: {params: positional.v, isNamed: false}, i: positional.i};
    }
    // As before, a function with no parameters counts as named.
    return {v: {params: [], isNamed: true}, i: i};
  }

  private annotation(i: number): Result<AnnotationExpr> {
    const at = this.tok(this.lines(i), TokenKind.SYMBOL_AT);
    const firstPart = at && this.identifier(at.i);
    if (firstPart === undefined) {
      return undefined;
    }
    let j = firstPart.i;
    const period = this.tok(j, TokenKind.SYMBOL_PERIOD);
    const secondPart = period && this.identifier(period.i);
    if (secondPart !== undefined) {
      j = secondPart.i;
    }
    const lparen = this.tok(j, TokenKind.SYMBOL_LPAREN);
    const args = lparen && this.args(lparen.i);
    const rparen = args && this.tok(args.i, TokenKind.SYMBOL_RPAREN);
    if (rparen === undefined) {
      return {
        v: {
          kind: 'AnnotationExpr',
          at: at!.v,
          firstPart: firstPart.v,
          secondPart: secondPart?.v,
        },
        i: this.lines(j)
      };
    }
    return {
      v: {
        kind: 'AnnotationExpr',
        at: at!.v,
        firstPart: firstPart.v,
        secondPart: secondPart?.v,
        args: args!.v,
        lparen: lparen!.v,
        rparen: rparen.v
      },
      i: this.lines(rparen.i)
    };
  }

  private annotations(i: number): Match<AnnotationExpr[]> {
    const annots: AnnotationExpr[] = [];
    while (true) {
      const annot = this.annotation(i);
      if (annot === undefined) {
        return {v: annots, i: i};
      }
      annots.push(annot.v);
      i = annot.i;
    }
  }

  private functionStat(i: number): Result<FunctionStat> {
    const annots = this.annotations(i);
    const defTok = this.tok(annots.i, TokenKind.KEYWORD_FUNCTION);
    const name = defTok && this.identifier(defTok.i);
    const lparen = name && this.tok(name.i, TokenKind.SYMBOL_LPAREN);
    if (lparen === undefined) {
      return undefined;
    }
    const params = this.params(lparen.i);
    const rparen = this.tok(params.i, TokenKind.SYMBOL_RPAREN);
    if (rparen === undefined) {
      return undefined;
    }
    const asyncTok = this.tok(rparen.i, TokenKind.KEYWORD_ASYNC);
    const stat = this.statement(asyncTok?.i ?? rparen.i);
    if (stat === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'FunctionStat',
        defTok: defTok!.v,
        name: name!.v,
        lparen: lparen.v,
        rparen: rparen.v,
        params: params.v.params,
        isNamed: params.v.isNamed,
        asyncTok: asyncTok?.v,
        stat: stat.v,
        annots: annots.v
      },
      i: stat.i
    };
  }

  private method(i: number): Result<MethodStat> {
    const annots = this.annotations(i);
    let methodTok: Token|undefined;
    let name: IdentifierExpr|NewExpr|SpecialMethodIdentifierExpr;
    let j: number;
    const methodKw = this.tok(annots.i, TokenKind.KEYWORD_METHOD);
    const methodName = methodKw &&
        (this.identifier(methodKw.i) ?? this.specialMethodIdentifier(methodKw.i));
    if (methodName !== undefined) {
      methodTok = methodKw!.v;
      name = methodName.v;
      j = methodName.i;
    } else {
      const newTok = this.newExpr(annots.i);
      if (newTok === undefined) {
        return undefined;
      }
      name = newTok.v;
      j = newTok.i;
    }
    const lparen = this.tok(j, TokenKind.SYMBOL_LPAREN);
    if (lparen === undefined) {
      return undefined;
    }
    const params = this.params(lparen.i);
    const rparen = this.tok(params.i, TokenKind.SYMBOL_RPAREN);
    if (rparen === undefined) {
      return undefined;
    }
    const asyncTok = this.tok(rparen.i, TokenKind.KEYWORD_ASYNC);
    const stat = this.statement(asyncTok?.i ?? rparen.i);
    if (stat === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'MethodStat',
        methodTok: methodTok,
        name: name,
        lparen: lparen.v,
        rparen: rparen.v,
        params: params.v.params,
        isNamed: params.v.isNamed,
        asyncTok: asyncTok?.v,
        stat: stat.v,
        annots: annots.v
      },
      i: stat.i
    };
  }

  private specialMethodIdentifier(i: number): Result<SpecialMethodIdentifierExpr> {
    const t = this.tokIn(i, [
      TokenKind.SYMBOL_EQUIV, TokenKind.SYMBOL_NEQUIV, TokenKind.SYMBOL_ARRAY_SET,
      TokenKind.SYMBOL_ARRAY_ACCESS
    ]);
    return t && {v: {kind: 'SpecialMethodIdentifierExpr', token: t.v}, i: t.i};
  }

  private anon(i: number): Result<AnonExpr> {
    return this.cached<AnonExpr>('anon', i, () => {
      // x -> ...
      const param = this.identifier(i);
      if (param !== undefined) {
        const asyncTok = this.tok(param.i, TokenKind.KEYWORD_ASYNC);
        const arrow = this.tok(asyncTok?.i ?? param.i, TokenKind.SYMBOL_RARROW);
        const body = arrow && this.assign(arrow.i);
        if (body !== undefined) {
          return {
            v: {
              kind: 'AnonExpr',
              params: [{kind: 'ParamExpr', name: param.v}],
              asyncTok: asyncTok?.v,
              isNamed: false,
              arrow: arrow!.v,
              expr: body.v
            },
            i: body.i
          };
        }
      }
      // (params) -> ... or (params) {...}
      const lparen = this.tok(i, TokenKind.SYMBOL_LPAREN);
      if (lparen === undefined) {
        return undefined;
      }
      const params = this.params(lparen.i);
      const rparen = this.tok(params.i, TokenKind.SYMBOL_RPAREN);
      if (rparen === undefined) {
        return undefined;
      }
      const asyncTok = this.tok(rparen.i, TokenKind.KEYWORD_ASYNC);
      const j = asyncTok?.i ?? rparen.i;
      const arrow = this.tok(j, TokenKind.SYMBOL_RARROW);
      const arrowBody = arrow && this.assign(arrow.i);
      let body: Result<AssignExpr|CompoundStat> = arrowBody;
      if (body === undefined) {
        body = this.compound(j);
      }
      if (body === undefined) {
        return undefined;
      }
      return {
        v: {
          kind: 'AnonExpr',
          lparen: lparen.v,
          rparen: rparen.v,
          params: params.v.params,
          isNamed: params.v.isNamed,
          asyncTok: asyncTok?.v,
          arrow: arrowBody === undefined ? undefined : arrow!.v,
          expr: body.v
        },
        i: body.i
      };
    });
  }

  private field(i: number): Result<FieldStat> {
    const fieldTok = this.tok(i, TokenKind.KEYWORD_FIELD);
    const fields = fieldTok &&
        this.list(fieldTok.i, j => this.identifier(j), TokenKind.SYMBOL_COMMA);
    if (fields === undefined) {
      return undefined;
    }
    return {
      v: {kind: 'FieldStat', fieldTok: fieldTok!.v, fields: fields.v},
      i: fields.i
    };
  }

  private staticStat(i: number): Result<StaticStat> {
    const staticTok = this.tok(i, TokenKind.KEYWORD_STATIC);
    const name = staticTok && this.identifier(staticTok.i);
    const eq = name && this.tok(name.i, TokenKind.SYMBOL_EQUALS);
    const expr = eq && this.condition(eq.i);
    if (expr === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'StaticStat',
        staticTok: staticTok!.v,
        name: name!.v,
        eq: eq!.v,
        expr: expr.v
      },
      i: expr.i
    };
  }

  private classMember(i: number): Result<ClassMemberStat> {
    const j = this.lines(i);
    const r: Result<ClassMemberStat> =
        this.method(j) ?? this.field(j) ?? this.staticStat(j);
    return r && {v: r.v, i: this.lines(r.i)};
  }

  private classStat(i: number): Result<ClassStat> {
    const annots = this.annotations(i);
    const classTok = this.tok(annots.i, TokenKind.KEYWORD_CLASS);
    const name = classTok && this.identifier(classTok.i);
    if (name === undefined) {
      return undefined;
    }
    let j = name.i;
    const colon = this.tok(j, TokenKind.SYMBOL_COLON);
    const superClass = colon && this.postfix(colon.i);
    if (superClass !== undefined) {
      j = superClass.i;
    }
    const lbrace = this.tok(this.lines(j), TokenKind.SYMBOL_LBRACE);
    if (lbrace !== undefined) {
      const body = this.items(
          this.lines(lbrace.i), k => this.classMember(k), /*inBlock=*/ true);
      const end = this.lines(body.i);
      const rbrace: Result<Token> = this.tok(end, TokenKind.SYMBOL_RBRACE) ??
          this.closedAt(this.closeUnclosedBlock(lbrace.v, end), end);
      if (rbrace !== undefined) {
        return {
          v: {
            kind: 'ClassStat',
            classTok: classTok!.v,
            name: name.v,
            colon: superClass && colon!.v,
            super: superClass?.v,
            lbrace: lbrace.v,
            rbrace: rbrace.v,
            stats: body.v,
            annots: annots.v
          },
          i: this.lines(rbrace.i)
        };
      }
    }
    // A class with a single member needs no braces.
    const member = this.classMember(j);
    if (member === undefined) {
      return undefined;
    }
    return {
      v: {
        kind: 'ClassStat',
        classTok: classTok!.v,
        name: name.v,
        colon: superClass && colon!.v,
        super: superClass?.v,
        stats: [member.v],
        annots: annots.v
      },
      i: member.i
    };
  }

  private importStat(i: number): Result<ImportStat> {
    const importTok = this.tok(i, TokenKind.KEYWORD_IMPORT);
    if (importTok === undefined) {
      return undefined;
    }
    const source = this.identifier(importTok.i) ?? this.string(importTok.i);
    if (source === undefined) {
      return undefined;
    }
    const asTok = this.tok(source.i, TokenKind.KEYWORD_AS);
    const name = asTok && this.identifier(asTok.i);
    if (name !== undefined) {
      return {
        v: {
          kind: 'ImportAsStat',
          name: name.v,
          source: source.v,
          importTok: importTok.v,
          asTok: asTok!.v
        },
        i: name.i
      };
    }
    return {
      v: {kind: 'ImportStat', source: source.v, importTok: importTok.v},
      i: source.i
    };
  }

  private moduleItem(i: number): Result<Statement> {
    const j = this.lines(i);
    const r: Result<Statement> = this.importStat(j) ?? this.classStat(j) ??
        this.functionStat(j) ?? this.exit(j) ?? this.raise(j) ??
        this.tryStat(j) ?? this.select(j) ?? this.compound(j) ??
        this.iter(j) ?? this.expression(j);
    return r && {v: r.v, i: this.lines(r.i)};
  }

  // Error recovery

  // Parses [item] as many times as possible. In recovery mode, an item that
  // fails to parse is reported and skipped, up to the end of the enclosing
  // block if [inBlock].
  private items<T>(i: number, item: (i: number) => Result<T>, inBlock: boolean):
      Match<T[]> {
    const values: T[] = [];
    // When an item parses but tried to go further and failed beyond its end
    // (e.g., `b` in `b = = 2`), that failure is the likely cause if the next
    // item fails right where it ended.
    let lookahead: {end: number, furthest: number, expected: Set<TokenKind>}|
        undefined;
    while (true) {
      const start = this.lines(i);
      if (start >= this.toks.length ||
          (inBlock && this.toks[start].kind === TokenKind.SYMBOL_RBRACE) ||
          (inBlock && this.recover && this.startsTopLevelItem(start))) {
        return {v: values, i: start};
      }
      // Track the furthest failure within this item, for its error message,
      // then merge it with the enclosing rule's.
      const outerFurthest = this.furthest, outerExpected = this.expected;
      this.furthest = -1;
      this.expected = new Set();
      const r = item(start);
      const itemFurthest = this.furthest, itemExpected = this.expected;
      const mergeFurthest = () => {
        if (outerFurthest > itemFurthest) {
          this.furthest = outerFurthest;
          this.expected = outerExpected;
        } else if (outerFurthest === itemFurthest) {
          this.expected = new Set([...outerExpected, ...itemExpected]);
        }
      };
      if (r !== undefined && r.i > start) {
        lookahead = itemFurthest > r.i ?
            {end: r.i, furthest: itemFurthest, expected: itemExpected} :
            undefined;
        mergeFurthest();
        values.push(r.v);
        i = r.i;
        continue;
      }
      if (!this.recover) {
        mergeFurthest();
        return {v: values, i: start};
      }
      let errorAt = itemFurthest;
      if (lookahead !== undefined && this.lines(lookahead.end) === start &&
          lookahead.furthest > itemFurthest) {
        this.furthest = errorAt = lookahead.furthest;
        this.expected = lookahead.expected;
      }
      lookahead = undefined;
      this.report();
      mergeFurthest();
      i = this.skip(start, Math.max(errorAt, start), inBlock);
      if (i >= this.toks.length && inBlock) {
        return {v: values, i: i};
      }
    }
  }

  // Whether the token at [i] starts an unindented class, function, import or
  // annotation, which in recovery mode ends a block whose '}' is missing.
  private startsTopLevelItem(i: number): boolean {
    const t = this.toks[i];
    return t.pos.columnBegin === 1 &&
        (t.kind === TokenKind.KEYWORD_CLASS ||
         t.kind === TokenKind.KEYWORD_FUNCTION ||
         t.kind === TokenKind.KEYWORD_IMPORT || t.kind === TokenKind.SYMBOL_AT);
  }

  // In recovery mode, closes a block at [i] whose '}' is missing, reporting it.
  // Returns the token to use as the block's end, or undefined if the block
  // can't be closed here.
  private closeUnclosedBlock(lbrace: Token, i: number): Token|undefined {
    if (!this.recover ||
        (i < this.toks.length && !this.startsTopLevelItem(i))) {
      return undefined;
    }
    this.problems.push({
      message: '\'{\' is never closed; expected \'}\'.',
      row: lbrace.pos.rowBegin - 1,
      col: lbrace.pos.columnBegin - 1,
    });
    let last = i - 1;
    while (last > 0 && this.toks[last].kind === TokenKind.NEWLINE) {
      last--;
    }
    return this.toks[last] ?? lbrace;
  }

  private closedAt(last: Token|undefined, i: number): Result<Token> {
    return last && {v: last, i: i};
  }

  private report(): void {
    const t = this.toks[this.furthest];
    const expected = [...this.expected].map(describeKind);
    const found = t === undefined ? 'end of file' :
        t.kind === TokenKind.NEWLINE ? 'end of line' :
                                       `'${t.text}'`;
    if (t?.kind === TokenKind.UNKNOWN) {
      this.problems.push({
        message: t.text === '\'' ? 'Unterminated string.' :
                                   `Unexpected character '${t.text}'.`,
        row: t.pos.rowBegin - 1,
        col: t.pos.columnBegin - 1,
      });
      return;
    }
    let message = `Unexpected ${found}`;
    if (expected.length > 4 && this.expected.has(TokenKind.IDENTIFIER)) {
      message += '; expected an expression';
    } else if (expected.length > 0 && expected.length <= 4) {
      message += `; expected ${
          expected.length === 1 ?
              expected[0] :
              expected.slice(0, -1).join(', ') + ' or ' +
                  expected[expected.length - 1]}`;
    }
    message += '.';
    this.problems.push(
        t === undefined ? {message: message} : {
          message: message,
          row: t.pos.rowBegin - 1,
          col: t.pos.columnBegin - 1,
        });
  }

  // Returns the index to resume parsing at after a syntax error at [errorAt] in
  // an item that starts at [start]: the start of the next line after the error
  // that isn't inside brackets, or the closing brace of the enclosing block.
  private skip(start: number, errorAt: number, inBlock: boolean): number {
    let depth = 0;
    let k = start;
    while (k < this.toks.length) {
      const t = this.toks[k];
      if (OPENERS.has(t.kind)) {
        depth++;
      } else if (CLOSERS.has(t.kind)) {
        if (depth === 0 && inBlock && t.kind === TokenKind.SYMBOL_RBRACE &&
            k > start) {
          return k;
        }
        depth = Math.max(0, depth - 1);
      } else if (
          t.kind === TokenKind.NEWLINE && depth === 0 && k >= errorAt) {
        return k + 1;
      }
      k++;
    }
    return k;
  }

  parseModule(): Result<Module> {
    const body = this.items(0, i => this.moduleItem(i), /*inBlock=*/ false);
    if (body.i < this.toks.length) {
      return undefined;
    }
    return {v: {statements: body.v}, i: body.i};
  }

  reportFailure(): void {
    this.report();
  }
}

// Parses a Zinnia module. If it has syntax errors, the module holds the
// statements that could be parsed, and the errors are returned as problems.
export function parseModule(text: string): ParseResult {
  const tokens = tokenize(text);
  const strict = new Parser(tokens, /*recover=*/ false);
  const strictModule = strict.parseModule();
  if (strictModule !== undefined) {
    return {module: strictModule.v, problems: []};
  }
  const recovering = new Parser(tokens, /*recover=*/ true);
  const module = recovering.parseModule();
  return {
    module: module?.v ?? {statements: []},
    problems: recovering.problems.length > 0 ?
        recovering.problems :
        [{message: 'Unable to parse the file.'}],
  };
}
