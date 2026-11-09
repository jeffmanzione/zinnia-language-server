// Hover text: what a name is, its signature or inferred class, and the comment
// written above its definition.

import {SemanticAnalyzer, SemanticDocInfo} from './semantics/analyzer';
import {inferType, resolved, SemanticClass, SemanticIdentifier, SemanticToken} from './semantics/semantic';
import {identifierSignature, nativeSignature, receiverType} from './completion';
import {nativeMethodsOf} from './semantics/natives';

// The `;` comment just above the 0-based [row] of [text], skipping the
// annotations between them, as Markdown. `[name]`, which the standard library
// uses for parameters, becomes `name` outside code blocks.
export function docComment(text: string, row: number): string|undefined {
  const lines = text.split(/\r?\n/);
  let r = row - 1;
  while (r >= 0 && /^\s*@/.test(lines[r])) {
    r--;
  }
  const comment: string[] = [];
  for (; r >= 0; r--) {
    const match = /^\s*;(?: ?)(.*)$/.exec(lines[r]);
    if (match === null) {
      break;
    }
    comment.unshift(match[1].trimEnd());
  }
  if (comment.length === 0) {
    return undefined;
  }
  let inCode = false;
  return comment
      .map(line => {
        if (/^\s*```/.test(line)) {
          inCode = !inCode;
          return line;
        }
        return inCode ? line : line.replace(/\[([A-Za-z_$][\w$]*)\]/g, '`$1`');
      })
      .join('\n')
      .trim() || undefined;
}

// The comment at the top of a module, if it's followed by a blank line or an
// import, rather than documenting the definition right after it.
function moduleComment(text: string): string|undefined {
  const lines = text.split(/\r?\n/);
  let end = 0;
  while (end < lines.length && /^\s*;/.test(lines[end])) {
    end++;
  }
  const next = lines[end] ?? '';
  if (end === 0 || (next.trim() !== '' && !/^\s*import\s/.test(next))) {
    return undefined;
  }
  return docComment(text, end);
}

// The doc comment of what [id], defined in [info], is an alias of, following
// aliases of aliases.
function aliasedDocComment(
    id: SemanticIdentifier, info: SemanticDocInfo|undefined,
    analyzer: SemanticAnalyzer, depth = 0): string|undefined {
  const alias = id.aliasOf;
  if (alias === undefined || depth > 5) {
    return undefined;
  }
  const target = alias.module === undefined ?
      info :
      analyzer.lookupDocInfoFromModuleName(alias.module);
  const definition = [...target?.context?.block.definitions() ?? []].find(
      d => d.id === alias.name);
  const declaration = definition && resolved(definition).tokens[0];
  if (target === undefined || definition === undefined || declaration === undefined) {
    return undefined;
  }
  return aliasedDocComment(resolved(definition), target, analyzer, depth + 1) ??
      docComment(target.text, declaration.token.pos.rowBegin - 1);
}

// The class of [info] whose field, static or method [id] is.
function owningClass(info: SemanticDocInfo|undefined, id: SemanticIdentifier):
    SemanticClass|undefined {
  return info?.module?.classes.find(
      cls => cls.fields.some(f => resolved(f.name) === id) ||
          [...cls.statics.values()].some(s => resolved(s.name) === id) ||
          [...cls.methods.values()].some(m => resolved(m.name) === id));
}

function codeBlock(code: string): string {
  return '```zinnia\n' + code + '\n```';
}

// Returns the Markdown to show when hovering over [token], in [info].
export function hoverMarkdown(
    info: SemanticDocInfo, token: SemanticToken, analyzer: SemanticAnalyzer): string {
  const id = token.identifier && resolved(token.identifier);
  if (id === undefined) {
    // A native member of an object whose class is inferred, or a member of an
    // object whose class isn't known.
    const line = info.text.split(/\r?\n/)[token.row] ?? '';
    const receiver = /([A-Za-z_$][\w$]*)\s*\.\s*$/.exec(line.slice(0, token.col))?.[1];
    const type = receiver && receiverType(info, receiver, token.row, token.col, analyzer);
    const native = type ? nativeMethodsOf(type.name)[token.text] : undefined;
    if (type && native !== undefined) {
      return codeBlock(`method ${type.name}.${nativeSignature(token.text, native).label}`);
    }
    return codeBlock(`${token.type === 'property' ? 'field' : token.type} ${token.text}`);
  }
  const declaration = id.tokens[0];
  const declInfo = declaration && analyzer.lookupDocInfoFromFile(declaration.filePath);
  const row = declaration ? declaration.token.pos.rowBegin - 1 : -1;
  const parts: string[] = [];
  let doc = declInfo && row >= 0 ? docComment(declInfo.text, row) : undefined;

  if (id.importSource !== undefined || id.modifiers.includes('defaultLibrary')) {
    const name = id.importSource?.name ?? id.id;
    parts.push(codeBlock(`module ${name}`));
    const module = id.importSource !== undefined ?
        analyzer.analyzedImport(token.filePath, name, id.importSource.isPath) :
        analyzer.lookupDocInfoFromModuleName(name);
    doc = module && moduleComment(module.text);
  } else if (id.type === 'class') {
    const line = declInfo?.text.split(/\r?\n/)[row] ?? '';
    const header = /class\s+[\w$]+(\s*:\s*[\w$.]+)?/.exec(line)?.[0] ?? `class ${id.id}`;
    const constructor = id.signature && identifierSignature(id.id, id, declInfo, analyzer);
    parts.push(codeBlock(header + (constructor ? `\n${constructor.label}` : '')));
  } else if (id.type === 'function' || id.type === 'method' ||
             id.signature !== undefined || id.aliasOf !== undefined) {
    const owner = owningClass(declInfo, id);
    const signature = identifierSignature(token.text, id, declInfo, analyzer);
    const label = signature?.label ?? id.id;
    parts.push(codeBlock(
        (id.type === 'method' ? 'method ' : 'function ') +
        (owner ? `${owner.name.id}.` : '') + label));
  } else {
    const owner = owningClass(declInfo, id);
    const kind = id.type === 'property' ?
        (owner?.statics.has(id.id) ? 'static' : 'field') :
        id.type === 'parameter' ? 'parameter' :
        id.isConst              ? 'const' :
                                  'variable';
    const type = inferType(id, analyzer);
    parts.push(codeBlock(
        `${kind} ${owner ? `${owner.name.id}.` : ''}${id.id}` +
        (type ? `: ${type.name}` : '')));
  }
  // A member defined as another name, as in `self.sort = builtin_ext.sort`,
  // is documented where that name is defined.
  const aliasDoc = aliasedDocComment(id, declInfo, analyzer);
  if (aliasDoc !== undefined) {
    doc = aliasDoc;
  }
  // A declared name exists at runtime, but its code isn't in source.
  if (id.isDeclared && parts.length > 0) {
    parts[0] = parts[0].replace('```zinnia\n', '```zinnia\ndeclare ');
  }
  if (doc !== undefined) {
    parts.push(doc);
  }
  return parts.join('\n\n');
}

