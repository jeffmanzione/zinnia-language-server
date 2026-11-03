// Formatting of whole documents.
//
// The formatter is deliberately conservative: it re-indents lines by their
// brackets, removes trailing spaces and ends the file with one new line, but
// doesn't reflow code. A line that's indented deeper than the statements
// around it, such as a continued expression or the body of a brace-less `if`,
// keeps its extra indentation, and moves with its block. Lines inside
// multi-line strings and block comments are left as they are.

import {Token, tokenize, TokenKind} from './semantics/tokenizer';

export interface FormatOptions {
  // The number of columns per indentation level.
  tabSize: number;
  insertSpaces: boolean;
}

const OPENERS = new Set([
  TokenKind.SYMBOL_LPAREN, TokenKind.SYMBOL_LBRACE, TokenKind.SYMBOL_LBRACKET,
]);
const CLOSERS = new Set([
  TokenKind.SYMBOL_RPAREN, TokenKind.SYMBOL_RBRACE, TokenKind.SYMBOL_RBRACKET,
]);
const INSIGNIFICANT = new Set([TokenKind.SPACE, TokenKind.NEWLINE]);

// An open bracket and the lines inside it.
interface Context {
  // The new indentation of the lines inside it.
  indent: number;
  // The new indentation of a line that starts by closing it, which is that of
  // the line that opened it, and that line's original indentation.
  closeIndent: number;
  closeOriginal: number;
  // For a parenthesis or square bracket: the original and new indentation of
  // the line that opened it. The lines inside keep their offset from it.
  opener?: {original: number, indent: number};
  // The original indentation of its first line, which later lines are
  // compared with.
  firstChildIndent?: number;
}

// The width of [whitespace], counting a tab as [tabSize] columns.
function widthOf(whitespace: string, tabSize: number): number {
  let width = 0;
  for (const c of whitespace) {
    width += c === '\t' ? tabSize : 1;
  }
  return width;
}

// Returns the formatted lines of [text], without line ends.
export function formatLines(text: string, options: FormatOptions): string[] {
  const unit = Math.max(1, options.tabSize);
  const indentText = (width: number) => options.insertSpaces ?
      ' '.repeat(width) :
      '\t'.repeat(Math.floor(width / unit)) + ' '.repeat(width % unit);

  const lines = text.split(/\r?\n/);
  // Tokens by row, and the rows whose start or end is inside a multi-line
  // string or comment.
  const tokensByRow: Token[][] = lines.map(() => []);
  const startsInside = new Set<number>();
  const endsInside = new Set<number>();
  for (const token of tokenize(text, true)) {
    const row = token.pos.rowBegin - 1;
    if (INSIGNIFICANT.has(token.kind)) {
      continue;
    }
    tokensByRow[row]?.push(token);
    for (let r = row; r < token.pos.rowEnd - 1; r++) {
      endsInside.add(r);
      startsInside.add(r + 1);
    }
  }

  // The top level is a context whose statements start in column 0.
  const stack: Context[] = [
    {indent: 0, closeIndent: 0, closeOriginal: 0, firstChildIndent: 0},
  ];
  const result: string[] = [];
  lines.forEach((line, row) => {
    const tokens = tokensByRow[row];
    if (startsInside.has(row)) {
      result.push(line);
      return;
    }
    if (tokens.length === 0) {
      result.push('');
      return;
    }
    const content = line.replace(/^[ \t]*/, '');
    const original = widthOf(line.slice(0, line.length - content.length), unit);
    const context = stack[stack.length - 1];
    let indent: number;
    if (CLOSERS.has(tokens[0].kind) && stack.length > 1) {
      indent = context.closeIndent;
    } else if (context.firstChildIndent === undefined) {
      context.firstChildIndent = original;
      indent = context.opener !== undefined && original > context.opener.original ?
          context.opener.indent + original - context.opener.original :
          context.indent;
      context.indent = indent;
    } else if (original > context.firstChildIndent) {
      // A continuation: keep its extra indentation.
      indent = context.indent + original - context.firstChildIndent;
    } else {
      indent = context.indent;
    }
    let formatted = indentText(indent) + content;
    if (!endsInside.has(row)) {
      formatted = formatted.replace(/[ \t]+$/, '');
    }
    result.push(formatted);
    // Brackets opened on this line indent the lines after it once, however
    // many there are. Closing a bracket opened on an earlier line returns to
    // that line's indentation, as in a wrapped parameter list followed by `{`.
    let base = indent;
    let baseOriginal = original;
    let opened = 0;
    for (const token of tokens) {
      if (OPENERS.has(token.kind)) {
        const top = stack[stack.length - 1];
        stack.push({
          indent: opened === 0 ? base + unit : top.indent,
          closeIndent: opened === 0 ? base : top.closeIndent,
          closeOriginal: opened === 0 ? baseOriginal : top.closeOriginal,
          opener: token.kind === TokenKind.SYMBOL_LBRACE ?
              undefined :
              {original: baseOriginal, indent: base},
        });
        opened++;
      } else if (CLOSERS.has(token.kind) && stack.length > 1) {
        const popped = stack.pop()!;
        if (opened > 0) {
          opened--;
        } else {
          base = popped.closeIndent;
          baseOriginal = popped.closeOriginal;
        }
      }
    }
  });
  // End with exactly one new line.
  while (result.length > 1 && result[result.length - 1] === '' &&
         result[result.length - 2] === '') {
    result.pop();
  }
  if (result[result.length - 1] !== '') {
    result.push('');
  }
  return result;
}

// Returns [text] formatted.
export function formatText(text: string, options: FormatOptions): string {
  return formatLines(text, options).join(text.includes('\r\n') ? '\r\n' : '\n');
}
