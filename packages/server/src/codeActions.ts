// Quick fixes for the problems the semantic pass reports.

import {CodeAction, CodeActionKind, Diagnostic, TextEdit} from 'vscode-languageserver/node';

import {SemanticDocInfo} from './semantics/analyzer';

// What a diagnostic carries for its quick fixes.
export interface DiagnosticData {
  // For a hidden top-level variable: where it's declared, 0-based.
  declaration?: {row: number, col: number};
  // For an unknown member: the members the class has.
  suggestions?: string[];
}

// The row after the document's imports, where a new import goes: after the
// last top-level import, or at the start.
export function importRow(info: SemanticDocInfo): number {
  let row = 0;
  for (const imprt of info.module?.imports ?? []) {
    row = Math.max(row, imprt.ast.importTok.pos.rowBegin);
  }
  return row;
}

// What quick fixes need to know about the names around a document.
export interface NameLookup {
  // Whether [name] is a module that can be imported.
  isModule: (name: string) => boolean;
  // The names that a misspelled name may have meant.
  names: () => string[];
}

// The edit distance between [a] and [b]: how many characters must be
// inserted, deleted, replaced or swapped with their neighbor to turn one into
// the other.
export function editDistance(a: string, b: string): number {
  const d: number[][] = [];
  for (let i = 0; i <= a.length; i++) {
    d.push([i]);
  }
  for (let j = 1; j <= b.length; j++) {
    d[0][j] = j;
  }
  for (let i = 1; i <= a.length; i++) {
    for (let j = 1; j <= b.length; j++) {
      const cost = a[i - 1] === b[j - 1] ? 0 : 1;
      d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);
      if (i > 1 && j > 1 && a[i - 1] === b[j - 2] && a[i - 2] === b[j - 1]) {
        d[i][j] = Math.min(d[i][j], d[i - 2][j - 2] + 1);
      }
    }
  }
  return d[a.length][b.length];
}

// Up to three of [candidates] that [name] is close to, closest first. A name
// of up to four characters may be one edit away, a longer one two.
export function similarNames(name: string, candidates: string[]): string[] {
  const limit = name.length <= 4 ? 1 : 2;
  return [...new Set(candidates)]
      .filter(c => c !== name)
      .map(c => ({name: c, distance: editDistance(name.toLowerCase(), c.toLowerCase())}))
      .filter(c => c.distance <= limit)
      .sort((a, b) => a.distance - b.distance || a.name.localeCompare(b.name))
      .slice(0, 3)
      .map(c => c.name);
}

// Returns the quick fixes for [diagnostics] in [info], the document they're
// in.
export function quickFixes(
    info: SemanticDocInfo, uri: string, diagnostics: Diagnostic[],
    lookup: NameLookup): CodeAction[] {
  let names: string[]|undefined;
  const actions: CodeAction[] = [];
  const lines = info.text.split(/\r?\n/);
  for (const diagnostic of diagnostics) {
    const start = diagnostic.range.start;
    const name = lines[start.line]?.slice(
        start.character, diagnostic.range.end.character);
    if (name === undefined || name === '') {
      continue;
    }
    if (diagnostic.code === 'unknown-member') {
      const members = (diagnostic.data as DiagnosticData|undefined)?.suggestions ?? [];
      for (const similar of similarNames(name, members)) {
        actions.push({
          title: `Change to '${similar}'`,
          kind: CodeActionKind.QuickFix,
          diagnostics: [diagnostic],
          edit: {changes: {[uri]: [TextEdit.replace(diagnostic.range, similar)]}},
        });
      }
    } else if (diagnostic.code === 'undefined-name') {
      names ??= lookup.names();
      for (const similar of similarNames(name, names)) {
        actions.push({
          title: `Change to '${similar}'`,
          kind: CodeActionKind.QuickFix,
          diagnostics: [diagnostic],
          edit: {changes: {[uri]: [TextEdit.replace(diagnostic.range, similar)]}},
        });
      }
      // Only a name used as `name.member` can be a missing import.
      const after = lines[start.line].slice(diagnostic.range.end.character);
      if (!/^\s*\./.test(after) || !lookup.isModule(name)) {
        continue;
      }
      const row = importRow(info);
      actions.push({
        title: `Import '${name}'`,
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        isPreferred: true,
        edit: {
          changes: {
            [uri]: [TextEdit.insert({line: row, character: 0}, `import ${name}\n`)],
          },
        },
      });
    } else if (diagnostic.code === 'unused-import') {
      // Remove the import's line, if the import is all that's on it.
      if (!/^\s*import\s/.test(lines[start.line]) ||
          (info.module?.imports ?? []).filter(
              i => i.ast.importTok.pos.rowBegin - 1 === start.line).length !== 1) {
        continue;
      }
      actions.push({
        title: `Remove unused import '${name}'`,
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        isPreferred: true,
        edit: {
          changes: {
            [uri]: [TextEdit.del({
              start: {line: start.line, character: 0},
              end: {line: start.line + 1, character: 0},
            })],
          },
        },
      });
    } else if (diagnostic.code === 'hidden-top-level-variable') {
      const declaration = (diagnostic.data as DiagnosticData|undefined)?.declaration;
      if (declaration === undefined ||
          lines[declaration.row]?.slice(declaration.col, declaration.col + name.length) !== name ||
          // A declaration already qualified, or not at the start of a statement,
          // isn't a plain `name = ...`.
          lines[declaration.row].slice(0, declaration.col).trim() !== '') {
        continue;
      }
      actions.push({
        title: `Make '${name}' a module member`,
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        isPreferred: true,
        edit: {
          changes: {
            [uri]: [TextEdit.insert(
                {line: declaration.row, character: declaration.col}, 'self.')],
          },
        },
      });
    }
  }
  return actions;
}
