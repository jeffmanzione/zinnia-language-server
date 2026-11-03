// Renaming identifiers.
//
// An identifier's own tokens are its declaration and the uses the semantic pass
// resolved to it in the file that declares it. Uses elsewhere are found
// through the tokens of the other analyzed documents: `module.name` resolves
// to the module's member. `self.name` doesn't resolve to anything, so it's
// matched by name in the class, or module, that declares the member.

import {SemanticAnalyzer, SemanticDocInfo} from './semantics/analyzer';
import {resolved, SemanticClass, SemanticIdentifier, SemanticToken, TokenFile} from './semantics/semantic';
import {isKeyword} from './semantics/tokenizer';

// A place in a file to rename, with 0-based positions.
export interface RenameLocation {
  filePath: string;
  row: number;
  col: number;
  length: number;
}

export type RenameTarget = {
  id: SemanticIdentifier,
  declaration: TokenFile,
  // The token the rename was asked for at.
  token: SemanticToken,
};

const IDENTIFIER = /^\$?[a-zA-Z_][a-zA-Z0-9_]*$/;

// Returns what renaming at [token] would rename, or why it can't be renamed.
export function renameTarget(
    token: SemanticToken|undefined, analyzer: SemanticAnalyzer): RenameTarget|
    {error: string} {
  if (token === undefined || token.identifier === undefined) {
    return {error: 'Only names of variables, functions, classes and members can be renamed.'};
  }
  const id = resolved(token.identifier);
  const declaration = id.tokens[0];
  if (declaration === undefined) {
    return {error: `'${id.id}' is defined natively and can't be renamed.`};
  }
  if (analyzer.isStandardLibraryFile(declaration.filePath)) {
    return {error: `'${id.id}' is defined in the standard library and can't be renamed.`};
  }
  if (id.modifiers.includes('defaultLibrary')) {
    return {error: 'Modules can\'t be renamed.'};
  }
  return {id: id, declaration: declaration, token: token};
}

// Returns why [newName] can't be the new name of an identifier, if it can't.
export function invalidName(newName: string): string|undefined {
  if (!IDENTIFIER.test(newName)) {
    return `'${newName}' is not a valid name.`;
  }
  if (isKeyword(newName)) {
    return `'${newName}' is a keyword.`;
  }
  return undefined;
}

function sameDeclaration(a: TokenFile|undefined, b: TokenFile): boolean {
  return a !== undefined && a.filePath === b.filePath &&
      a.token.pos.rowBegin === b.token.pos.rowBegin &&
      a.token.pos.columnBegin === b.token.pos.columnBegin;
}

// The class of [info] that declares [id] as a field, static or method.
function declaringClass(info: SemanticDocInfo, id: SemanticIdentifier):
    SemanticClass|undefined {
  return info.module?.classes.find(
      cls => cls.fields.some(f => resolved(f.name) === id) ||
          [...cls.statics.values()].some(s => resolved(s.name) === id) ||
          [...cls.methods.values()].some(m => resolved(m.name) === id));
}

function rowsOf(cls: SemanticClass): [number, number] {
  return [
    cls.ast.classTok.pos.rowBegin - 1,
    (cls.ast.rbrace ?? cls.ast.name.token).pos.rowEnd - 1,
  ];
}

// The offset in [text] of the 0-based [row] and [col].
function offsetOf(text: string, row: number, col: number): number {
  let offset = 0;
  for (let i = 0; i < row; i++) {
    offset = text.indexOf('\n', offset) + 1;
  }
  return offset + col;
}

// Returns every place to rename for [target], in [docs], the analyzed
// documents.
export function renameLocations(
    target: RenameTarget, docs: Iterable<SemanticDocInfo>): RenameLocation[] {
  const {id, declaration} = target;
  const locations = new Map<string, RenameLocation>();
  const add = (filePath: string, row: number, col: number) => {
    locations.set(
        `${filePath}:${row}:${col}`,
        {filePath: filePath, row: row, col: col, length: id.id.length});
  };
  for (const tok of id.tokens) {
    add(tok.filePath, tok.token.pos.rowBegin - 1, tok.token.pos.columnBegin - 1);
  }

  const allDocs = [...docs];
  const declaringDoc = allDocs.find(d => d.path === declaration.filePath);
  const cls = declaringDoc && declaringClass(declaringDoc, id);
  const isModuleMember = declaringDoc !== undefined && cls === undefined &&
      [...declaringDoc.context?.block.definitions() ?? []].some(
          d => resolved(d) === id && !d.modifiers.includes('local'));
  const classRows = cls && rowsOf(cls);
  const otherClassRows = (declaringDoc?.module?.classes ?? []).map(rowsOf);

  for (const doc of allDocs) {
    for (const token of doc.tokens) {
      if (token.text !== id.id) {
        continue;
      }
      if (token.identifier !== undefined) {
        // Matched by declaration, since the documents may have been analyzed
        // against different analyses of the declaring one.
        if (sameDeclaration(resolved(token.identifier).tokens[0], declaration)) {
          add(token.filePath, token.row, token.col);
        }
        continue;
      }
      // `self.name`, which resolves to nothing.
      if (doc !== declaringDoc || !(cls !== undefined || isModuleMember)) {
        continue;
      }
      const before = doc.text.slice(0, offsetOf(doc.text, token.row, token.col));
      if (!/(?:^|[^\w$.])self\s*\.\s*$/.test(before)) {
        continue;
      }
      const inClass = (rows: [number, number]) =>
          token.row >= rows[0] && token.row <= rows[1];
      if (classRows !== undefined ? inClass(classRows) :
                                    !otherClassRows.some(inClass)) {
        add(token.filePath, token.row, token.col);
      }
    }
  }
  return [...locations.values()].sort(
      (a, b) => a.filePath.localeCompare(b.filePath) || a.row - b.row ||
          a.col - b.col);
}
