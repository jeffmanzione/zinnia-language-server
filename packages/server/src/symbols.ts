import * as parsec from 'typescript-parsec';
import {DocumentSymbol, Range, SymbolKind} from 'vscode-languageserver/node';

import {ClassStat, FunctionStat, Module} from './semantics/statements';
import {TokenKind} from './semantics/tokenizer';

type Token = parsec.Token<TokenKind>;

// Converts a token's 1-based position to an LSP range.
export function tokenRange(token: Token): Range {
  return {
    start: {line: token.pos.rowBegin - 1, character: token.pos.columnBegin - 1},
    end: {line: token.pos.rowEnd - 1, character: token.pos.columnEnd - 1},
  };
}

function spanning(first: Token, last: Token|undefined): Range {
  return {
    start: tokenRange(first).start,
    end: tokenRange(last ?? first).end,
  };
}

function classSymbol(cls: ClassStat): DocumentSymbol {
  const children: DocumentSymbol[] = [];
  for (const stat of cls.stats) {
    if (stat.kind === 'FieldStat') {
      for (const field of stat.fields) {
        children.push({
          name: field.token.text,
          kind: SymbolKind.Field,
          range: tokenRange(field.token),
          selectionRange: tokenRange(field.token),
        });
      }
    } else if (stat.kind === 'StaticStat') {
      children.push({
        name: stat.name.token.text,
        kind: SymbolKind.Constant,
        range: spanning(stat.staticTok, stat.name.token),
        selectionRange: tokenRange(stat.name.token),
      });
    } else if (stat.kind === 'MethodStat') {
      const nameTok = stat.name.token;
      const isConstructor = stat.name.kind === 'NewExpr';
      children.push({
        name: isConstructor ? 'new' : nameTok.text,
        kind: isConstructor ? SymbolKind.Constructor : SymbolKind.Method,
        range: spanning(stat.methodTok ?? nameTok, stat.rparen),
        selectionRange: tokenRange(nameTok),
      });
      // Constructor parameters declared with `field` are fields too.
      for (const param of stat.params) {
        if (param.field !== undefined) {
          children.push({
            name: param.name.token.text,
            kind: SymbolKind.Field,
            range: tokenRange(param.name.token),
            selectionRange: tokenRange(param.name.token),
          });
        }
      }
    }
  }
  return {
    name: cls.name.token.text,
    kind: SymbolKind.Class,
    range: spanning(cls.classTok, cls.rbrace ?? cls.name.token),
    selectionRange: tokenRange(cls.name.token),
    children: children,
  };
}

function functionSymbol(func: FunctionStat): DocumentSymbol {
  return {
    name: func.name.token.text,
    kind: SymbolKind.Function,
    range: spanning(func.defTok, func.rparen),
    selectionRange: tokenRange(func.name.token),
  };
}

// Returns the outline of a module: its classes, with their members, and its
// top-level functions.
export function documentSymbols(module: Module): DocumentSymbol[] {
  const symbols: DocumentSymbol[] = [];
  for (const stat of module.statements) {
    if (stat.kind === 'ClassStat') {
      symbols.push(classSymbol(stat));
    } else if (stat.kind === 'FunctionStat') {
      symbols.push(functionSymbol(stat));
    }
  }
  return symbols;
}
