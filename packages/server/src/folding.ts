// Folding ranges: brackets that span lines, block comments, runs of line
// comments and runs of imports.

import {FoldingRange, FoldingRangeKind} from 'vscode-languageserver/node';

import {Token, tokenize, TokenKind} from './semantics/tokenizer';

const OPENERS = new Set([
  TokenKind.SYMBOL_LPAREN, TokenKind.SYMBOL_LBRACE, TokenKind.SYMBOL_LBRACKET,
]);
const CLOSERS = new Set([
  TokenKind.SYMBOL_RPAREN, TokenKind.SYMBOL_RBRACE, TokenKind.SYMBOL_RBRACKET,
]);

export function foldingRanges(text: string): FoldingRange[] {
  const ranges: FoldingRange[] = [];
  const stack: Token[] = [];
  // The current run of lines that each hold just a line comment, or an
  // import, as [first row, last row].
  let comments: [number, number]|undefined;
  let imports: [number, number]|undefined;
  const endRun = (run: [number, number]|undefined, kind: string) => {
    if (run !== undefined && run[1] > run[0]) {
      ranges.push({startLine: run[0], endLine: run[1], kind: kind});
    }
  };

  // The row of the line being read.
  let lineRow = -1;
  for (const token of tokenize(text, true)) {
    const row = token.pos.rowBegin - 1;
    if (token.kind === TokenKind.SPACE || token.kind === TokenKind.NEWLINE) {
      continue;
    }
    if (row !== lineRow) {
      // The first token of a line.
      lineRow = row;
      if (token.kind === TokenKind.COMMENT_LINE) {
        comments = comments !== undefined && comments[1] === row - 1 ?
            [comments[0], row] :
            (endRun(comments, FoldingRangeKind.Comment), [row, row]);
      }
      if (token.kind === TokenKind.KEYWORD_IMPORT) {
        imports = imports !== undefined && imports[1] === row - 1 ?
            [imports[0], row] :
            (endRun(imports, FoldingRangeKind.Imports), [row, row]);
      }
    }
    if (token.kind === TokenKind.COMMENT_BLOCK &&
        token.pos.rowEnd > token.pos.rowBegin) {
      ranges.push({
        startLine: row,
        endLine: token.pos.rowEnd - 1,
        kind: FoldingRangeKind.Comment,
      });
    } else if (OPENERS.has(token.kind)) {
      stack.push(token);
    } else if (CLOSERS.has(token.kind)) {
      const opener = stack.pop();
      // Keep the closing bracket's line visible.
      if (opener !== undefined && row - 1 > opener.pos.rowBegin - 1) {
        ranges.push({startLine: opener.pos.rowBegin - 1, endLine: row - 1});
      }
    }
  }
  endRun(comments, FoldingRangeKind.Comment);
  endRun(imports, FoldingRangeKind.Imports);
  return ranges.sort((a, b) => a.startLine - b.startLine);
}
