
export enum TokenKind {
  LITERAL_STRING,
  LITEARL_INTEGER,
  LITEARL_FLOAT,

  KEYWORD_ABSTRACT,
  KEYWORD_AND,
  KEYWORD_AS,
  KEYWORD_ASYNC,
  KEYWORD_AWAIT,
  KEYWORD_BREAK,
  KEYWORD_CATCH,
  KEYWORD_CLASS,
  KEYWORD_CONST,
  KEYWORD_CONTINUE,
  KEYWORD_DEL,
  KEYWORD_DELETE,
  KEYWORD_ELSE,
  KEYWORD_EXIT,
  KEYWORD_FALSE,
  KEYWORD_FIELD,
  KEYWORD_FINAL,
  KEYWORD_FINALLY,
  KEYWORD_FOR,
  KEYWORD_FUNCTION,
  KEYWORD_IF,
  KEYWORD_IMPORT,
  KEYWORD_INTERFACE,
  KEYWORD_IN,
  KEYWORD_IS,
  KEYWORD_ISNOT,
  KEYWORD_METHOD,
  KEYWORD_MODULE,
  KEYWORD_NEW,
  KEYWORD_NOTIN,
  KEYWORD_NULL,
  KEYWORD_OR,
  KEYWORD_PACKAGE,
  KEYWORD_PRIVATE,
  KEYWORD_PROTECTED,
  KEYWORD_RAISE,
  KEYWORD_READONLY,
  KEYWORD_RETURN,
  KEYWORD_STATIC,
  KEYWORD_THEN,
  KEYWORD_TRUE,
  KEYWORD_TRY,
  KEYWORD_WHILE,

  SYMBOL_ARRAY_ACCESS,
  SYMBOL_ARRAY_SET,
  SYMBOL_LPAREN,
  SYMBOL_RPAREN,
  SYMBOL_LBRACE,
  SYMBOL_RBRACE,
  SYMBOL_LBRACKET,
  SYMBOL_RBRACKET,
  SYMBOL_RARROW,
  SYMBOL_LARROW,
  SYMBOL_INCREMENT,
  SYMBOL_DECREMENT,
  SYMBOL_LTHANEQ,
  SYMBOL_GTHANEQ,
  SYMBOL_EQUIV,
  SYMBOL_NEQUIV,
  SYMBOL_PLUS,
  SYMBOL_MINUS,
  SYMBOL_STAR,
  SYMBOL_FSLASH,
  SYMBOL_BSLASH,
  SYMBOL_PERCENT,
  SYMBOL_AMPER,
  SYMBOL_PIPE,
  SYMBOL_CARET,
  SYMBOL_TILDE,
  SYMBOL_EXCLAIM,
  SYMBOL_QUESTION,
  SYMBOL_AT,
  SYMBOL_POUND,
  SYMBOL_LTHAN,
  SYMBOL_GTHAN,
  SYMBOL_EQUALS,
  SYMBOL_COMMA,
  SYMBOL_COLON,
  SYMBOL_PERIOD,

  IDENTIFIER,
  NEWLINE,
  COMMENT_LINE,
  COMMENT_BLOCK,
  SPACE,
  // A character that starts no token.
  UNKNOWN
}

// Each rule is [keep, pattern, kind]. Tokens that aren't kept (comments and
// spaces) are skipped. At each position the longest match wins, and the
// earliest rule wins a tie, so keywords beat identifiers of the same length.
export const TOKEN_RULES: [boolean, RegExp, TokenKind][] = [
  // A backslash escapes the next character, including another backslash.
  [true, /^i?'(\\.|[^'\\])*'/g, TokenKind.LITERAL_STRING],
  [true, /^\d+\.\d+/g, TokenKind.LITEARL_FLOAT],
  [true, /^\d+/g, TokenKind.LITEARL_INTEGER],

  [true, /^(\r?\n)+/g, TokenKind.NEWLINE],

  [true, /^abstract/g, TokenKind.KEYWORD_ABSTRACT],
  [true, /^and/g, TokenKind.KEYWORD_AND],
  [true, /^as/g, TokenKind.KEYWORD_AS],
  [true, /^async/g, TokenKind.KEYWORD_ASYNC],
  [true, /^await/g, TokenKind.KEYWORD_AWAIT],
  [true, /^break/g, TokenKind.KEYWORD_BREAK],
  [true, /^catch/g, TokenKind.KEYWORD_CATCH],
  [true, /^class/g, TokenKind.KEYWORD_CLASS],
  [true, /^const/g, TokenKind.KEYWORD_CONST],
  [true, /^continue/g, TokenKind.KEYWORD_CONTINUE],
  [true, /^del/g, TokenKind.KEYWORD_DEL],
  [true, /^delete/g, TokenKind.KEYWORD_DELETE],
  [true, /^else/g, TokenKind.KEYWORD_ELSE],
  [true, /^exit/g, TokenKind.KEYWORD_EXIT],
  [true, /^False/g, TokenKind.KEYWORD_FALSE],
  [true, /^field/g, TokenKind.KEYWORD_FIELD],
  [true, /^final/g, TokenKind.KEYWORD_FINAL],
  [true, /^finally/g, TokenKind.KEYWORD_FINALLY],
  [true, /^for/g, TokenKind.KEYWORD_FOR],
  [true, /^function/g, TokenKind.KEYWORD_FUNCTION],
  [true, /^if/g, TokenKind.KEYWORD_IF],
  [true, /^import/g, TokenKind.KEYWORD_IMPORT],
  [true, /^interface/g, TokenKind.KEYWORD_INTERFACE],
  [true, /^in/g, TokenKind.KEYWORD_IN],
  [true, /^is/g, TokenKind.KEYWORD_IS],
  [true, /^isnot/g, TokenKind.KEYWORD_ISNOT],
  [true, /^method/g, TokenKind.KEYWORD_METHOD],
  [true, /^module/g, TokenKind.KEYWORD_MODULE],
  [true, /^new/g, TokenKind.KEYWORD_NEW],
  [true, /^notin/g, TokenKind.KEYWORD_NOTIN],
  [true, /^None/g, TokenKind.KEYWORD_NULL],
  [true, /^or/g, TokenKind.KEYWORD_OR],
  [true, /^package/g, TokenKind.KEYWORD_PACKAGE],
  [true, /^private/g, TokenKind.KEYWORD_PRIVATE],
  [true, /^protected/g, TokenKind.KEYWORD_PROTECTED],
  [true, /^raise/g, TokenKind.KEYWORD_RAISE],
  [true, /^readonly/g, TokenKind.KEYWORD_READONLY],
  [true, /^return/g, TokenKind.KEYWORD_RETURN],
  [true, /^static/g, TokenKind.KEYWORD_STATIC],
  [true, /^then/g, TokenKind.KEYWORD_THEN],
  [true, /^True/g, TokenKind.KEYWORD_TRUE],
  [true, /^try/g, TokenKind.KEYWORD_TRY],
  [true, /^while/g, TokenKind.KEYWORD_WHILE],

  [true, /^\[\]/g, TokenKind.SYMBOL_ARRAY_ACCESS],
  [true, /^\[\]=/g, TokenKind.SYMBOL_ARRAY_SET],
  [true, /^\(/g, TokenKind.SYMBOL_LPAREN],
  [true, /^\)/g, TokenKind.SYMBOL_RPAREN],
  [true, /^\{/g, TokenKind.SYMBOL_LBRACE],
  [true, /^\}/g, TokenKind.SYMBOL_RBRACE],
  [true, /^\[/g, TokenKind.SYMBOL_LBRACKET],
  [true, /^\]/g, TokenKind.SYMBOL_RBRACKET],
  [true, /^->/g, TokenKind.SYMBOL_RARROW],
  [true, /^<-/g, TokenKind.SYMBOL_LARROW],
  [true, /^\+\+/g, TokenKind.SYMBOL_INCREMENT],
  [true, /^--/g, TokenKind.SYMBOL_DECREMENT],
  [true, /^<=/g, TokenKind.SYMBOL_LTHANEQ],
  [true, /^>=/g, TokenKind.SYMBOL_GTHANEQ],
  [true, /^==/g, TokenKind.SYMBOL_EQUIV],
  [true, /^!=/g, TokenKind.SYMBOL_NEQUIV],
  [true, /^\+/g, TokenKind.SYMBOL_PLUS],
  [true, /^-/g, TokenKind.SYMBOL_MINUS],
  [true, /^\*/g, TokenKind.SYMBOL_STAR],
  [true, /^\//g, TokenKind.SYMBOL_FSLASH],
  [true, /^\\/g, TokenKind.SYMBOL_BSLASH],
  [true, /^%/g, TokenKind.SYMBOL_PERCENT],
  [true, /^&/g, TokenKind.SYMBOL_AMPER],
  [true, /^\|/g, TokenKind.SYMBOL_PIPE],
  [true, /^\^/g, TokenKind.SYMBOL_CARET],
  [true, /^~/g, TokenKind.SYMBOL_TILDE],
  [true, /^!/g, TokenKind.SYMBOL_EXCLAIM],
  [true, /^\?/g, TokenKind.SYMBOL_QUESTION],
  [true, /^@/g, TokenKind.SYMBOL_AT],
  [true, /^#/g, TokenKind.SYMBOL_POUND],
  [true, /^</g, TokenKind.SYMBOL_LTHAN],
  [true, /^>/g, TokenKind.SYMBOL_GTHAN],
  [true, /^=/g, TokenKind.SYMBOL_EQUALS],
  [true, /^,/g, TokenKind.SYMBOL_COMMA],
  [true, /^:/g, TokenKind.SYMBOL_COLON],
  [true, /^\./g, TokenKind.SYMBOL_PERIOD],

  [true, /^\$?[a-zA-Z_][a-zA-Z0-9_]*/g, TokenKind.IDENTIFIER],
  // A line comment ends before the new line, which still separates lines.
  [false, /^;[^\n]*/g, TokenKind.COMMENT_LINE],
  [false, /^\/\*([^*]|\*+[^/])*\*+\//g, TokenKind.COMMENT_BLOCK],
  [false, /^(?:(?!\n)\s)+/g, TokenKind.SPACE]
];

// A token's position in the text. Rows and columns are 1-based, and the end is
// just after the token.
export interface TokenPosition {
  readonly index: number;
  readonly rowBegin: number;
  readonly columnBegin: number;
  readonly rowEnd: number;
  readonly columnEnd: number;
}

export interface Token {
  readonly kind: TokenKind;
  readonly text: string;
  readonly pos: TokenPosition;
  // The next token, skipping comments and spaces.
  readonly next: Token|undefined;
}

interface MutableToken {
  kind: TokenKind;
  text: string;
  pos: TokenPosition;
  next: Token|undefined;
}

const STICKY_RULES: [boolean, RegExp, TokenKind][] = TOKEN_RULES.map(
    ([keep, pattern, kind]) =>
        [keep, new RegExp(pattern.source.replace(/^\^/, ''), 'y'), kind]);

// Splits [text] into tokens. This never throws: a character that starts no
// token becomes an UNKNOWN token. Each token's next is the token after it.
// Comments and spaces are left out unless [keepAll].
export function tokenize(text: string, keepAll = false): Token[] {
  const tokens: MutableToken[] = [];
  let index = 0, row = 1, col = 1;
  while (index < text.length) {
    let bestLength = 0, bestKind = TokenKind.UNKNOWN, keep = true;
    for (const [ruleKeep, pattern, kind] of STICKY_RULES) {
      pattern.lastIndex = index;
      const match = pattern.exec(text);
      if (match !== null && match[0].length > bestLength) {
        bestLength = match[0].length;
        bestKind = kind;
        keep = ruleKeep;
      }
    }
    if (bestLength === 0) {
      bestLength = 1;
    }
    const tokenText = text.substr(index, bestLength);
    let rowEnd = row, colEnd = col;
    // Columns count UTF-16 code units, and a '\r' takes none.
    for (let k = 0; k < tokenText.length; k++) {
      const c = tokenText[k];
      if (c === '\n') {
        rowEnd++;
        colEnd = 1;
      } else if (c !== '\r') {
        colEnd++;
      }
    }
    if (keep || keepAll) {
      tokens.push({
        kind: bestKind,
        text: tokenText,
        pos: {
          index: index,
          rowBegin: row,
          columnBegin: col,
          rowEnd: rowEnd,
          columnEnd: colEnd,
        },
        next: undefined,
      });
    }
    index += bestLength;
    row = rowEnd;
    col = colEnd;
  }
  for (let i = 0; i + 1 < tokens.length; i++) {
    tokens[i].next = tokens[i + 1];
  }
  return tokens;
}

// Whether [name] is a keyword, such as 'class' or 'True'.
export function isKeyword(name: string): boolean {
  const tokens = tokenize(name);
  return tokens.length === 1 && tokens[0].text === name &&
      TokenKind[tokens[0].kind].startsWith('KEYWORD_');
}
