// Quick fixes for the problems the semantic pass reports.

import {CodeAction, CodeActionKind, Diagnostic, TextEdit} from 'vscode-languageserver/node';

import {SemanticDocInfo} from './semantics/analyzer';

// What a diagnostic carries for its quick fixes.
export interface DiagnosticData {
  // For a hidden top-level variable: where it's declared, 0-based.
  declaration?: {row: number, col: number};
}

// The row after the document's imports, where a new import goes: after the
// last top-level import, or at the start.
function importRow(info: SemanticDocInfo): number {
  let row = 0;
  for (const imprt of info.module?.imports ?? []) {
    row = Math.max(row, imprt.ast.importTok.pos.rowBegin);
  }
  return row;
}

// Returns the quick fixes for [diagnostics] in [info], the document they're
// in. [isModule] is whether a name is a module that can be imported.
export function quickFixes(
    info: SemanticDocInfo, uri: string, diagnostics: Diagnostic[],
    isModule: (name: string) => boolean): CodeAction[] {
  const actions: CodeAction[] = [];
  const lines = info.text.split(/\r?\n/);
  for (const diagnostic of diagnostics) {
    const start = diagnostic.range.start;
    const name = lines[start.line]?.slice(
        start.character, diagnostic.range.end.character);
    if (name === undefined || name === '') {
      continue;
    }
    if (diagnostic.code === 'undefined-name') {
      // Only a name used as `name.member` can be a missing import.
      const after = lines[start.line].slice(diagnostic.range.end.character);
      if (!/^\s*\./.test(after) || !isModule(name)) {
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
