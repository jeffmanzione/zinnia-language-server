// Document highlights, call hierarchy and inlay hints, which all work from the
// tokens of the analyzed documents and the identifiers they refer to.

import * as path from 'path';
import {CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, DocumentHighlight, DocumentHighlightKind, InlayHint, InlayHintKind, Range, SymbolKind} from 'vscode-languageserver/node';
import {URI} from 'vscode-uri';

import {identifierSignature, receiverType} from './completion';
import {nativeMethodsOf} from './semantics/natives';
import {renameLocations} from './rename';
import {SemanticAnalyzer, SemanticDocInfo} from './semantics/analyzer';
import {resolved, SemanticIdentifier, SemanticToken, TokenFile} from './semantics/semantic';

// Highlights the uses of the identifier [token] refers to in [info], marking
// its declaration as a write.
export function documentHighlights(
    info: SemanticDocInfo, token: SemanticToken|undefined): DocumentHighlight[] {
  const id = token?.identifier && resolved(token.identifier);
  const declaration = id?.tokens[0];
  if (id === undefined || declaration === undefined) {
    return [];
  }
  return renameLocations({id: id, declaration: declaration, token: token!}, [info])
      .filter(loc => loc.filePath === info.path)
      .map(loc => ({
             range: {
               start: {line: loc.row, character: loc.col},
               end: {line: loc.row, character: loc.col + loc.length},
             },
             kind: isDeclaration(declaration, loc.filePath, loc.row, loc.col) ?
                 DocumentHighlightKind.Write :
                 DocumentHighlightKind.Read,
           }));
}

function isDeclaration(
    declaration: TokenFile, filePath: string, row: number, col: number): boolean {
  return declaration.filePath === filePath &&
      declaration.token.pos.rowBegin - 1 === row &&
      declaration.token.pos.columnBegin - 1 === col;
}

// The offsets in [text] where its lines start.
function lineStarts(text: string): number[] {
  const starts = [0];
  for (let i = text.indexOf('\n'); i >= 0; i = text.indexOf('\n', i + 1)) {
    starts.push(i + 1);
  }
  return starts;
}

// Whether [token], in [text] with lines starting at [starts], is followed by
// an argument list.
function isCalled(text: string, starts: number[], token: SemanticToken): boolean {
  const end = starts[token.row] + token.col + token.text.length;
  return /^[ \t]*\(/.test(text.slice(end, end + 64));
}

// A function, method or class, with the rows its definition spans.
interface Callable {
  id: SemanticIdentifier;
  kind: SymbolKind;
  name: string;
  detail?: string;
  filePath: string;
  // The name's position, 0-based.
  row: number;
  col: number;
  // The first and last rows of the definition.
  firstRow: number;
  lastRow: number;
}

// The rows of [text] where a top-level statement starts: lines that aren't
// indented, blank, comments or closing brackets.
function topLevelRows(text: string): number[] {
  const rows: number[] = [];
  text.split(/\r?\n/).forEach((line, row) => {
    if (/^[^\s;})\]/]/.test(line)) {
      rows.push(row);
    }
  });
  return rows;
}

// The functions, methods and classes defined in [info], with their extents.
function callablesOf(info: SemanticDocInfo): Callable[] {
  const module = info.module;
  if (module === undefined) {
    return [];
  }
  const starts = topLevelRows(info.text);
  const lastLine = info.text.split(/\r?\n/).length - 1;
  // The last row of a top-level definition starting at [row]: the row before
  // the next top-level statement.
  const topLevelEnd = (row: number) => {
    const next = starts.find(r => r > row);
    return next === undefined ? lastLine : next - 1;
  };
  const callables: Callable[] = [];
  const add = (id: SemanticIdentifier, kind: SymbolKind, firstRow: number,
               lastRow: number, detail?: string, name?: string) => {
    const declaration = resolved(id).tokens[0];
    if (declaration === undefined || declaration.filePath !== info.path) {
      return;
    }
    callables.push({
      id: resolved(id),
      kind: kind,
      name: name ?? id.id,
      detail: detail,
      filePath: info.path,
      row: declaration.token.pos.rowBegin - 1,
      col: declaration.token.pos.columnBegin - 1,
      firstRow: firstRow,
      lastRow: lastRow,
    });
  };
  for (const cls of module.classes) {
    const classFirst = cls.ast.classTok.pos.rowBegin - 1;
    const classLast = (cls.ast.rbrace ?? cls.ast.name.token).pos.rowEnd - 1;
    add(cls.name, SymbolKind.Class, classFirst, classLast);
    const methods = [...cls.methods.values()].sort(
        (a, b) => a.ast.name.token.pos.rowBegin - b.ast.name.token.pos.rowBegin);
    methods.forEach((method, i) => {
      const first = (method.ast.methodTok ?? method.ast.name.token).pos.rowBegin - 1;
      const next = methods[i + 1];
      const last = next === undefined ?
          classLast :
          (next.ast.methodTok ?? next.ast.name.token).pos.rowBegin - 2;
      const isConstructor = method.ast.name.kind === 'NewExpr';
      add(method.name,
          isConstructor ? SymbolKind.Constructor : SymbolKind.Method, first,
          Math.max(first, last), cls.name.id, isConstructor ? 'new' : undefined);
    });
  }
  for (const stat of module.ast.statements) {
    if (stat.kind === 'FunctionStat') {
      const id = [...info.context?.block.definitions() ?? []].find(
          d => d.id === stat.name.token.text);
      const first = stat.defTok.pos.rowBegin - 1;
      if (id !== undefined) {
        add(id, SymbolKind.Function, first, topLevelEnd(first));
      }
    }
  }
  return callables;
}

// The innermost callable of [callables] whose definition contains [row].
function enclosing(callables: Callable[], row: number): Callable|undefined {
  let best: Callable|undefined;
  for (const callable of callables) {
    if (callable.kind !== SymbolKind.Class && row >= callable.firstRow &&
        row <= callable.lastRow &&
        (best === undefined || callable.firstRow >= best.firstRow)) {
      best = callable;
    }
  }
  return best;
}

interface ItemData {
  filePath: string;
  row: number;
  col: number;
}

function rangeAt(row: number, col: number, length: number): Range {
  return {
    start: {line: row, character: col},
    end: {line: row, character: col + length},
  };
}

function itemFor(callable: Callable): CallHierarchyItem {
  return {
    name: callable.name,
    kind: callable.kind,
    detail: callable.detail,
    uri: URI.file(callable.filePath).toString(),
    range: {
      start: {line: callable.firstRow, character: 0},
      end: {line: callable.lastRow, character: 0},
    },
    selectionRange: rangeAt(callable.row, callable.col, callable.id.id.length),
    data: {filePath: callable.filePath, row: callable.row, col: callable.col} satisfies
        ItemData,
  };
}

// An item for a file's top-level code, which calls from outside any function.
function fileItem(info: SemanticDocInfo): CallHierarchyItem {
  return {
    name: path.basename(info.path),
    kind: SymbolKind.File,
    uri: URI.file(info.path).toString(),
    range: rangeAt(0, 0, 0),
    selectionRange: rangeAt(0, 0, 0),
    data: {filePath: info.path, row: -1, col: -1} satisfies ItemData,
  };
}

// Returns the call hierarchy item for the function, method or class that
// [token] refers to.
export function prepareCallHierarchy(
    token: SemanticToken|undefined, analyzer: SemanticAnalyzer):
    CallHierarchyItem[] {
  const id = token?.identifier && resolved(token.identifier);
  const declaration = id?.tokens[0];
  if (id === undefined || declaration === undefined) {
    return [];
  }
  const info = analyzer.lookupDocInfoFromFile(declaration.filePath);
  const callable = info && callablesOf(info).find(c => c.id === id);
  return callable === undefined ? [] : [itemFor(callable)];
}

function callableOfItem(item: CallHierarchyItem, analyzer: SemanticAnalyzer):
    Callable|undefined {
  const data = item.data as ItemData;
  const info = analyzer.lookupDocInfoFromFile(data.filePath);
  return info &&
      callablesOf(info).find(c => c.row === data.row && c.col === data.col);
}

function sameDeclaration(a: TokenFile|undefined, b: Callable): boolean {
  return a !== undefined && a.filePath === b.filePath &&
      a.token.pos.rowBegin - 1 === b.row && a.token.pos.columnBegin - 1 === b.col;
}

// The calls of [item] in the analyzed documents, grouped by the function,
// method or top-level code they're in.
export function incomingCalls(
    item: CallHierarchyItem, analyzer: SemanticAnalyzer): CallHierarchyIncomingCall[] {
  const target = callableOfItem(item, analyzer);
  if (target === undefined) {
    return [];
  }
  const calls: CallHierarchyIncomingCall[] = [];
  for (const info of analyzer.analyzedDocuments()) {
    if (analyzer.isStandardLibraryFile(info.path)) {
      continue;
    }
    const callables = callablesOf(info);
    const starts = lineStarts(info.text);
    const byCaller = new Map<Callable|undefined, Range[]>();
    for (const token of info.tokens) {
      if (token.identifier === undefined ||
          !sameDeclaration(resolved(token.identifier).tokens[0], target) ||
          (info.path === target.filePath && token.row === target.row &&
           token.col === target.col) ||
          !isCalled(info.text, starts, token)) {
        continue;
      }
      const caller = enclosing(callables, token.row);
      const ranges = byCaller.get(caller) ?? [];
      ranges.push(rangeAt(token.row, token.col, token.text.length));
      byCaller.set(caller, ranges);
    }
    for (const [caller, ranges] of byCaller) {
      calls.push({from: caller ? itemFor(caller) : fileItem(info), fromRanges: ranges});
    }
  }
  return calls;
}

// The functions, methods and classes that [item] calls, grouped by callee.
export function outgoingCalls(
    item: CallHierarchyItem, analyzer: SemanticAnalyzer): CallHierarchyOutgoingCall[] {
  const source = callableOfItem(item, analyzer);
  const info = source && analyzer.lookupDocInfoFromFile(source.filePath);
  if (source === undefined || info === undefined || source.kind === SymbolKind.Class) {
    return [];
  }
  const byCallee = new Map<SemanticIdentifier, Range[]>();
  const starts = lineStarts(info.text);
  for (const token of info.tokens) {
    const id = token.identifier && resolved(token.identifier);
    if (id === undefined || token.row < source.firstRow || token.row > source.lastRow ||
        (token.row === source.row && token.col === source.col) ||
        !['function', 'method', 'class'].includes(id.type) ||
        !isCalled(info.text, starts, token)) {
      continue;
    }
    const ranges = byCallee.get(id) ?? [];
    ranges.push(rangeAt(token.row, token.col, token.text.length));
    byCallee.set(id, ranges);
  }
  const calls: CallHierarchyOutgoingCall[] = [];
  for (const [id, ranges] of byCallee) {
    const declaration = id.tokens[0];
    const calleeInfo = declaration && analyzer.lookupDocInfoFromFile(declaration.filePath);
    const callee = calleeInfo && callablesOf(calleeInfo).find(c => c.id === id);
    if (callee !== undefined && !analyzer.isStandardLibraryFile(callee.filePath)) {
      calls.push({to: itemFor(callee), fromRanges: ranges});
    }
  }
  return calls;
}

// The start offsets and texts of the arguments of the call whose '(' is at
// [open] in [text], or undefined if it isn't closed.
function argumentsAt(text: string, open: number): {start: number, text: string}[]|
    undefined {
  const args: {start: number, text: string}[] = [];
  let depth = 0;
  let start = open + 1;
  for (let i = open + 1; i < text.length; i++) {
    const c = text[i];
    if (c === '\'') {
      // Skip a string literal.
      for (i++; i < text.length && text[i] !== '\''; i++) {
        if (text[i] === '\\') {
          i++;
        }
      }
    } else if (c === ';') {
      // Skip a line comment.
      while (i < text.length && text[i] !== '\n') {
        i++;
      }
    } else if (c === '(' || c === '[' || c === '{') {
      depth++;
    } else if ((c === ')' || c === ']' || c === '}') && depth > 0) {
      depth--;
    } else if (c === ')' || (c === ',' && depth === 0)) {
      const arg = text.slice(start, i);
      const skipped = arg.length - arg.trimStart().length;
      args.push({start: start + skipped, text: arg.trim()});
      if (c === ')') {
        return args;
      }
      start = i + 1;
    }
  }
  return undefined;
}

// Hints the names of the parameters that the positional arguments of calls in
// rows [firstRow] to [lastRow] of [info] are passed to. Calls of functions
// with one parameter, arguments that are already the parameter's name, and
// named arguments get none.
export function inlayHints(
    info: SemanticDocInfo, firstRow: number, lastRow: number,
    analyzer: SemanticAnalyzer): InlayHint[] {
  const hints: InlayHint[] = [];
  const starts = lineStarts(info.text);
  for (const token of info.tokens) {
    if (token.row < firstRow || token.row > lastRow || token.identifier === undefined ||
        token.modifiers.includes('declaration')) {
      continue;
    }
    const id = resolved(token.identifier);
    if (!['function', 'method', 'class', 'variable'].includes(id.type) ||
        id.signature?.isNamed) {
      continue;
    }
    const declaration = id.tokens[0];
    const signature = identifierSignature(
        token.text, id,
        declaration && analyzer.lookupDocInfoFromFile(declaration.filePath),
        analyzer);
    const params = (signature?.parameters ?? [])
                       .map(p => String(p.label).split(' = ')[0]);
    if (params.length < 2 || signature?.label.includes('({')) {
      continue;
    }
    const end = starts[token.row] + token.col + token.text.length;
    const open = /^[ \t]*\(/.exec(info.text.slice(end, end + 64));
    if (open === null) {
      continue;
    }
    const args = argumentsAt(info.text, end + open[0].length - 1) ?? [];
    args.slice(0, params.length).forEach((arg, i) => {
      if (arg.text === '' || arg.text === params[i] ||
          /^[A-Za-z_$][\w$]*\s*:/.test(arg.text)) {
        return;
      }
      // The last line starting at or before the argument.
      let row = token.row;
      while (row + 1 < starts.length && starts[row + 1] <= arg.start) {
        row++;
      }
      hints.push({
        position: {line: row, character: arg.start - starts[row]},
        label: `${params[i]}:`,
        kind: InlayHintKind.Parameter,
        paddingRight: true,
      });
    });
  }
  return hints.sort(
      (a, b) => a.position.line - b.position.line ||
          a.position.character - b.position.character);
}

// A place to go to, with 0-based positions.
export interface DefinitionLocation {
  filePath: string;
  row: number;
  col: number;
  length: number;
}

// Returns where the name [token] of [info] is defined:
// - for a module's name, the module's file;
// - for a native method of an object whose class is inferred, the class,
//   since the method has no source;
// - for anything else, its declaration, unless it has none in source.
export function definitionOf(
    info: SemanticDocInfo, token: SemanticToken|undefined,
    analyzer: SemanticAnalyzer): DefinitionLocation|undefined {
  if (token === undefined) {
    return undefined;
  }
  if (token.identifier === undefined) {
    const before = (info.text.split(/\r?\n/)[token.row] ?? '').slice(0, token.col);
    const receiver = /([A-Za-z_$][\w$]*)\s*\.\s*$/.exec(before)?.[1];
    // A string literal, as in 'a'.trim(), is a String.
    const type = /'\s*\.\s*$/.test(before) ?
        {name: 'String', cls: analyzer.searchBuiltinForId('String')?.cls} :
        receiver && receiverType(info, receiver, token.row, token.col, analyzer);
    const declaration = type && type.cls !== undefined &&
        nativeMethodsOf(type.name)[token.text] !== undefined ?
        type.cls.name.tokens[0] :
        undefined;
    return declaration && locationOfToken(declaration);
  }
  const id = resolved(token.identifier);
  if (id.importSource !== undefined) {
    const declaration = id.tokens[0];
    const module = analyzer.analyzedImport(
        declaration?.filePath ?? info.path, id.importSource.name, id.importSource.isPath);
    if (module !== undefined) {
      return {filePath: module.path, row: 0, col: 0, length: 0};
    }
  }
  if (id.isUndefined) {
    return undefined;
  }
  const declaration = id.tokens[0];
  return declaration && locationOfToken(declaration);
}

function locationOfToken(tok: TokenFile): DefinitionLocation {
  return {
    filePath: tok.filePath,
    row: tok.token.pos.rowBegin - 1,
    col: tok.token.pos.columnBegin - 1,
    length: tok.token.text.length,
  };
}
