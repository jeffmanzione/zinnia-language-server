// Completion of members and signature help.
//
// While a member access or call is being typed, the line usually doesn't parse
// yet, so these read the text before the cursor and resolve names with the
// document's last analysis.

import {CompletionItem, CompletionItemKind, ParameterInformation, SignatureInformation} from 'vscode-languageserver/node';

import {SemanticAnalyzer, SemanticDocInfo} from './semantics/analyzer';
import {ParamExpr} from './semantics/expressions';
import {NATIVE_CLASSES, NATIVE_FUNCTIONS, nativeMethodsOf} from './semantics/natives';
import {SemanticClass, SemanticIdentifier, Signature} from './semantics/semantic';

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

// Returns the innermost call whose arguments contain [offset] in [text].
export function callAt(text: string, offset: number): CallAt|undefined {
  let depth = 0;
  let argIndex = 0;
  for (let i = offset - 1; i >= 0; i--) {
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
    } else if (c === '\'') {
      // Skip a string literal, which may contain brackets and commas.
      const start = text.lastIndexOf('\'', i - 1);
      if (start < 0) {
        return undefined;
      }
      i = start;
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
