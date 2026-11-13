// Completion of members and signature help.
//
// While a member access or call is being typed, the line usually doesn't parse
// yet, so these read the text before the cursor and resolve names with the
// document's last analysis.

import {CompletionItem, CompletionItemKind, ParameterInformation, SignatureInformation, TextEdit} from 'vscode-languageserver/node';

import {importRow} from './codeActions';

import {SemanticAnalyzer, SemanticDocInfo} from './semantics/analyzer';
import {ParamExpr} from './semantics/expressions';
import {NATIVE_CLASSES, NATIVE_FUNCTIONS, nativeMethodsOf} from './semantics/natives';
import {classChain, inferType, InferredType, resolved, SemanticClass, SemanticIdentifier, Signature} from './semantics/semantic';

// A member access being typed: `receiver.prefix`.
export interface MemberAccess {
  receiver: string;
  prefix: string;
}

// Returns the member access that ends at [offset] in [text], if any.
export function memberAccessAt(text: string, offset: number): MemberAccess|
    undefined {
  const match = /([A-Za-z_$][\w$]*)\s*\.\s*([\w$]*)$/.exec(text.slice(0, offset));
  return match === null ? undefined : {receiver: match[1], prefix: match[2]};
}

// A call being typed: its callee, as `name`, `receiver.name` or `self.name`,
// and the index of the argument the cursor is in.
export interface CallAt {
  receiver?: string;
  name: string;
  argIndex: number;
}

// Which of the first [end] characters of [text] are in a string or a comment.
// Strings may hold escaped quotes and backslashes, as in '\'' and '\\', so they
// are found from the start.
function stringsAndComments(text: string, end: number): boolean[] {
  const skipped = new Array<boolean>(end).fill(false);
  for (let i = 0; i < end; i++) {
    const c = text[i];
    let close: number|undefined;
    if (c === '\'') {
      close = i + 1;
      while (close < end && text[close] !== '\'') {
        close += text[close] === '\\' ? 2 : 1;
      }
    } else if (c === ';') {
      close = text.indexOf('\n', i);
      close = close < 0 ? end : close - 1;
    } else if (c === '/' && text[i + 1] === '*') {
      close = text.indexOf('*/', i + 2);
      close = close < 0 ? end : close + 1;
    }
    if (close !== undefined) {
      for (let j = i; j <= close && j < end; j++) {
        skipped[j] = true;
      }
      i = close;
    }
  }
  return skipped;
}

// Returns the innermost call whose arguments contain [offset] in [text].
export function callAt(text: string, offset: number): CallAt|undefined {
  let depth = 0;
  let argIndex = 0;
  // Brackets and commas in strings and comments don't count.
  const skipped = stringsAndComments(text, offset);
  for (let i = offset - 1; i >= 0; i--) {
    if (skipped[i]) {
      continue;
    }
    const c = text[i];
    if (c === ')' || c === ']' || c === '}') {
      depth++;
    } else if (c === '(' || c === '[' || c === '{') {
      if (depth > 0) {
        depth--;
        continue;
      }
      if (c !== '(') {
        // Inside an array, map or block, not a call.
        return undefined;
      }
      const match =
          /(?:([A-Za-z_$][\w$]*)\s*\.\s*)?([A-Za-z_$][\w$]*)\s*$/.exec(
              text.slice(0, i));
      if (match === null) {
        return undefined;
      }
      return {receiver: match[1], name: match[2], argIndex: argIndex};
    } else if (c === ',' && depth === 0) {
      argIndex++;
    } else if (c === '\n' && depth === 0 && /[)}]\s*$/.test(text.slice(0, i))) {
      // A line that ends a statement can't be inside this call.
      return undefined;
    }
  }
  return undefined;
}

const KIND_OF: Record<string, CompletionItemKind> = {
  class: CompletionItemKind.Class,
  function: CompletionItemKind.Function,
  method: CompletionItemKind.Method,
  property: CompletionItemKind.Field,
  variable: CompletionItemKind.Variable,
};

// The source text of a parameter, e.g. `b = 5`.
function paramLabel(param: ParamExpr, text: string|undefined): string {
  const name = param.name.token.text;
  if (param.eq === undefined || text === undefined) {
    return name;
  }
  // The default value runs to the next ',' or the end of the parameters.
  let depth = 0;
  let end = param.eq.pos.index + 1;
  for (; end < text.length; end++) {
    const c = text[end];
    if (c === '(' || c === '[' || c === '{') {
      depth++;
    } else if (c === ')' || c === ']' || c === '}') {
      if (depth === 0) {
        break;
      }
      depth--;
    } else if (c === ',' && depth === 0) {
      break;
    }
  }
  return `${name} = ${text.slice(param.eq.pos.index + 1, end).trim()}`;
}

// Describes a function, method or constructor, e.g. `f(a, b = 5)`.
export function signatureInformation(
    name: string, signature: Signature, text: string|undefined):
    SignatureInformation {
  const params = signature.params.map(p => paramLabel(p, text));
  const inner = params.join(', ');
  return {
    label: `${name}(${signature.isNamed && params.length > 0 ? `{${inner}}` : inner})`,
    parameters: params.map(p => ({label: p}) satisfies ParameterInformation),
  };
}

// Describes a native function or method, which has no default values.
export function nativeSignature(name: string, params: string[]):
    SignatureInformation {
  return {
    label: `${name}(${params.join(', ')})`,
    parameters: params.map(p => ({label: p})),
  };
}

// Describes the function, constructor or method that [id], defined in [info],
// names, following aliases such as `self.sort = builtin_ext.sort`.
export function identifierSignature(
    name: string, id: SemanticIdentifier, info: SemanticDocInfo|undefined,
    analyzer: SemanticAnalyzer, depth = 0): SignatureInformation|undefined {
  if (id.signature !== undefined) {
    return signatureInformation(name, id.signature, info?.text);
  }
  const alias = id.aliasOf;
  if (alias === undefined || depth > 5) {
    return undefined;
  }
  const target = alias.module === undefined ?
      info :
      analyzer.lookupDocInfoFromModuleName(alias.module);
  const definition = [...target?.context?.block.definitions() ?? []].find(
      d => d.id === alias.name);
  if (definition !== undefined) {
    return identifierSignature(name, definition, target, analyzer, depth + 1);
  }
  const native = alias.module && NATIVE_FUNCTIONS[alias.module]?.[alias.name];
  return native ? nativeSignature(name, native) : undefined;
}

// The name of the class that [cls] extends, if it's written as a name.
function superclassName(cls: SemanticClass): string|undefined {
  const sup = cls.ast.super;
  if (sup?.kind === 'IdentifierExpr') {
    return sup.token.text;
  }
  if (sup?.kind === 'PostfixChainExpr') {
    const last = sup.postfixes[sup.postfixes.length - 1];
    return last.kind === 'MemberAccessExpr' ? last.field.token.text : undefined;
  }
  return undefined;
}

// The native methods of [cls] and of the class it extends.
export function nativeMethodsOfClass(cls: SemanticClass): Record<string, string[]> {
  const sup = superclassName(cls);
  return {
    ...(sup === undefined ? {} : nativeMethodsOf(sup)),
    ...nativeMethodsOf(cls.name.id),
  };
}

function itemFor(id: SemanticIdentifier, text: string|undefined): CompletionItem {
  const item: CompletionItem = {
    label: id.id,
    kind: KIND_OF[id.type] ?? CompletionItemKind.Variable,
  };
  if (id.signature !== undefined) {
    item.detail = signatureInformation(id.id, id.signature, text).label;
  }
  return item;
}

// The members a module offers: its classes, functions and `self.` members,
// including those defined natively, but not its imports or the variables of
// its top-level code.
export function moduleMembers(
    info: SemanticDocInfo, moduleName: string|undefined,
    analyzer: SemanticAnalyzer): CompletionItem[] {
  const items = new Map<string, CompletionItem>();
  for (const id of info.context?.block.definitions() ?? []) {
    if (id.modifiers.includes('local') || id.modifiers.includes('defaultLibrary')) {
      continue;
    }
    const item = itemFor(id, info.text);
    item.detail ??= identifierSignature(id.id, id, info, analyzer)?.label;
    items.set(id.id, item);
  }
  if (moduleName !== undefined) {
    for (const [name, params] of Object.entries(NATIVE_FUNCTIONS[moduleName] ?? {})) {
      if (!items.has(name)) {
        items.set(name, {
          label: name,
          kind: CompletionItemKind.Function,
          detail: nativeSignature(name, params).label,
        });
      }
    }
    for (const name of NATIVE_CLASSES[moduleName] ?? []) {
      if (!items.has(name)) {
        items.set(name, {label: name, kind: CompletionItemKind.Class});
      }
    }
  }
  // Private members, by convention named with a leading _, come last.
  for (const item of items.values()) {
    if (item.label.startsWith('_')) {
      item.sortText = `~${item.label}`;
    }
  }
  return [...items.values()];
}

// The fields, statics and methods of [cls].
export function classMembers(cls: SemanticClass, text: string|undefined):
    CompletionItem[] {
  const items: CompletionItem[] = [];
  for (const field of cls.fields) {
    items.push({label: field.name.id, kind: CompletionItemKind.Field});
  }
  for (const [name] of cls.statics) {
    items.push({label: name, kind: CompletionItemKind.Constant});
  }
  for (const [name, method] of cls.methods) {
    if (method.ast.name.kind === 'NewExpr') {
      continue;
    }
    items.push({
      label: name,
      kind: CompletionItemKind.Method,
      detail: signatureInformation(
                  name, {params: method.ast.params, isNamed: method.ast.isNamed},
                  text)
                  .label,
    });
  }
  for (const [name, params] of Object.entries(nativeMethodsOfClass(cls))) {
    if (!items.some(item => item.label === name)) {
      items.push({
        label: name,
        kind: CompletionItemKind.Method,
        detail: nativeSignature(name, params).label,
      });
    }
  }
  return items;
}

// Returns the class whose body contains the 0-based [line], if any.
export function classAt(info: SemanticDocInfo, line: number): SemanticClass|
    undefined {
  return info.module?.classes.find(cls => {
    const first = cls.ast.classTok.pos.rowBegin - 1;
    const last = (cls.ast.rbrace ?? cls.ast.name.token).pos.rowEnd - 1;
    return line >= first && line <= last;
  });
}

// Infers the class of [receiver], a name used just before [line] and
// [character], from the last analysis: the nearest earlier use of the name.
export function receiverType(
    info: SemanticDocInfo, receiver: string, line: number, character: number,
    analyzer: SemanticAnalyzer): InferredType|undefined {
  let nearest: SemanticDocInfo['tokens'][number]|undefined;
  for (const token of info.tokens) {
    if (token.text !== receiver || token.identifier === undefined ||
        token.row > line || (token.row === line && token.col >= character)) {
      continue;
    }
    if (nearest === undefined || token.row > nearest.row ||
        (token.row === nearest.row && token.col > nearest.col)) {
      nearest = token;
    }
  }
  return nearest && inferType(nearest.identifier, analyzer);
}

// The text of the file that defines [cls].
function textOf(cls: SemanticClass, analyzer: SemanticAnalyzer): string|undefined {
  const filePath = cls.name.tokens[0]?.filePath;
  return filePath === undefined ? undefined :
                                  analyzer.lookupDocInfoFromFile(filePath)?.text;
}

// The members of a value of [type], including inherited ones.
export function typeMembers(type: InferredType, analyzer: SemanticAnalyzer):
    CompletionItem[] {
  const items = new Map<string, CompletionItem>();
  if (type.cls !== undefined) {
    for (const cls of classChain(type.cls, analyzer)) {
      for (const item of classMembers(cls, textOf(cls, analyzer))) {
        if (!items.has(item.label)) {
          items.set(item.label, item);
        }
      }
    }
  }
  for (const [name, params] of Object.entries(nativeMethodsOf(type.name))) {
    if (!items.has(name)) {
      items.set(name, {
        label: name,
        kind: CompletionItemKind.Method,
        detail: nativeSignature(name, params).label,
      });
    }
  }
  return [...items.values()];
}

// Describes the method [name] of a value of [type], if it has one.
export function typeMethodSignature(
    type: InferredType, name: string,
    analyzer: SemanticAnalyzer): SignatureInformation|undefined {
  if (type.cls !== undefined) {
    for (const cls of classChain(type.cls, analyzer)) {
      const method = cls.methods.get(name);
      if (method !== undefined) {
        return signatureInformation(
            name, {params: method.ast.params, isNamed: method.ast.isNamed},
            textOf(cls, analyzer));
      }
      const native = nativeMethodsOfClass(cls)[name];
      if (native !== undefined) {
        return nativeSignature(name, native);
      }
    }
  }
  const native = nativeMethodsOf(type.name)[name];
  return native && nativeSignature(name, native);
}

// The edit that imports [name] into [info].
function importEdit(info: SemanticDocInfo, name: string): TextEdit {
  return TextEdit.insert({line: importRow(info), character: 0}, `import ${name}\n`);
}

// The module named [name] if [info] doesn't import it, and [name] isn't a
// name it defines or sees through builtin.
function unimportedModule(
    info: SemanticDocInfo, name: string,
    analyzer: SemanticAnalyzer): {info: SemanticDocInfo, libName?: string}|undefined {
  // Names used without being defined don't count.
  const defined = info.context?.block.findIdentifier(name, true);
  const isUndefined = info.problems.some(
      p => p.code === 'undefined-name' && p.message.startsWith(`'${name}'`));
  if ((info.module?.imports ?? []).some(i => i.name.id === name) ||
      (defined !== undefined && resolved(defined).tokens.length > 0 && !isUndefined) ||
      !analyzer.importableModules(info.path).includes(name)) {
    return undefined;
  }
  const lib = analyzer.lookupDocInfoFromModuleName(name);
  if (lib !== undefined) {
    return {info: lib, libName: name};
  }
  const module = analyzer.analyzedImport(info.path, name, false);
  return module && {info: module};
}

// The members of [receiver], a module [info] doesn't import yet, each of
// which imports it when completed; or undefined if [receiver] isn't one.
export function unimportedModuleMembers(
    info: SemanticDocInfo, receiver: string,
    analyzer: SemanticAnalyzer): CompletionItem[]|undefined {
  const module = unimportedModule(info, receiver, analyzer);
  if (module === undefined) {
    return undefined;
  }
  const edit = importEdit(info, receiver);
  return moduleMembers(module.info, module.libName, analyzer)
      .map(item => ({...item, additionalTextEdits: [edit]}));
}

// The modules [info] can import but doesn't, each of which imports it when
// completed.
export function unimportedModuleItems(
    info: SemanticDocInfo, analyzer: SemanticAnalyzer): CompletionItem[] {
  return analyzer.importableModules(info.path)
      .filter(name => unimportedModule(info, name, analyzer) !== undefined)
      .map(name => ({
             label: name,
             kind: CompletionItemKind.Module,
             detail: `import ${name}`,
             additionalTextEdits: [importEdit(info, name)],
           }));
}
