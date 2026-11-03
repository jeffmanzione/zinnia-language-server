import {DocumentSymbol, Range, SymbolKind} from 'vscode-languageserver/node';

import {ClassStat, FunctionStat, Module} from './semantics/statements';
import {Token, TokenKind} from './semantics/tokenizer';

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

// A symbol of a workspace file, as a DocumentSymbol with the file it's in and
// the class it's a member of.
export interface FileSymbol {
  filePath: string;
  symbol: DocumentSymbol;
  containerName?: string;
}

// Whether [query]'s characters appear in [name] in order, ignoring case, as
// editors expect of workspace symbol search.
export function matchesQuery(name: string, query: string): boolean {
  const lowerName = name.toLowerCase();
  let i = 0;
  for (const c of query.toLowerCase()) {
    i = lowerName.indexOf(c, i);
    if (i < 0) {
      return false;
    }
    i++;
  }
  return true;
}

// Returns the classes, their members and the functions of [module], in the
// file at [filePath], whose names match [query].
export function workspaceSymbols(
    filePath: string, module: Module, query: string): FileSymbol[] {
  const result: FileSymbol[] = [];
  for (const symbol of documentSymbols(module)) {
    if (matchesQuery(symbol.name, query)) {
      result.push({filePath: filePath, symbol: symbol});
    }
    for (const child of symbol.children ?? []) {
      if (matchesQuery(child.name, query)) {
        result.push(
            {filePath: filePath, symbol: child, containerName: symbol.name});
      }
    }
  }
  return result;
}
