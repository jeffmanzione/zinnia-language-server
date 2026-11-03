import {CompletionItem, CompletionItemKind, createConnection, Definition, DidChangeWatchedFilesNotification, FileChangeType, ErrorCodes, PrepareRenameResult, ResponseError, TextEdit, WorkspaceEdit, WorkspaceSymbol, Diagnostic, DiagnosticSeverity, DocumentSymbol, Hover, InitializeParams, InitializeResult, Location, MarkupKind, Position, ProposedFeatures, SemanticTokens, SemanticTokensBuilder, SignatureHelp, SignatureInformation, TextDocuments, TextDocumentSyncKind} from 'vscode-languageserver/node';
import {TextDocument} from 'vscode-languageserver-textdocument';
import {URI} from 'vscode-uri';
import {ZINNIA_TOKEN_MODIFIERS, ZINNIA_TOKEN_TYPES} from 'zinnia-language-shared/legend';

import {callAt, classAt, classMembers, identifierSignature, memberAccessAt, moduleMembers, nativeMethodsOfClass, nativeSignature, signatureInformation} from './completion';
import {loadZinniaLibs} from './lib';
import {filePathOf, SemanticAnalyzer, SemanticDocInfo} from './semantics/analyzer';
import {ImportStat} from './semantics/statements';
import {NATIVE_FUNCTIONS, NATIVE_METHODS} from './semantics/natives';
import {SemanticClass, SemanticToken} from './semantics/semantic';
import {invalidName, renameLocations, renameTarget} from './rename';
import {documentSymbols, workspaceSymbols} from './symbols';

const KEYWORDS = [
  'and',    'as',     'async',  'await',  'break', 'catch',  'class',
  'const',  'continue', 'else', 'exit',   'False', 'field',  'for',
  'function', 'if',   'import', 'in',     'is',    'method', 'new',
  'None',   'notin',  'or',     'raise',  'return', 'static', 'then',
  'True',   'try',    'while',
];

const analyzer = new SemanticAnalyzer();

// Creates a connection using whichever transport the client asked for
// (--node-ipc, --stdio or --socket), so editors other than VS Code can use the
// server too.
const connection = createConnection(ProposedFeatures.all);

const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// The latest analysis of each open document, keyed by URI.
const analyses = new Map<string, {version: number, info: Promise<SemanticDocInfo>}>();

const tokenTypeIndices = new Map(ZINNIA_TOKEN_TYPES.map((t, i) => [t, i]));
const tokenModifierBits =
    new Map(ZINNIA_TOKEN_MODIFIERS.map((m, i) => [m, 1 << i]));

function analyze(document: TextDocument): Promise<SemanticDocInfo> {
  const cached = analyses.get(document.uri);
  if (cached !== undefined && cached.version === document.version) {
    return cached.info;
  }
  const info = analyzer.parseDocument(
      {textDocument: {uri: document.uri}, text: document.getText()});
  analyses.set(document.uri, {version: document.version, info: info});
  return info;
}

async function analysisFor(uri: string): Promise<SemanticDocInfo|undefined> {
  const document = documents.get(uri);
  return document === undefined ? undefined : analyze(document);
}

function isOnToken(position: Position, token: SemanticToken): boolean {
  return position.line === token.row && position.character >= token.col &&
      position.character < token.col + token.text.length;
}

function tokenAt(info: SemanticDocInfo, position: Position): SemanticToken|
    undefined {
  // Prefer a token that refers to an identifier: `self.x = ...` has one for
  // the module member and one for the member access.
  const tokens = info.tokens.filter(token => isOnToken(position, token));
  return tokens.find(token => token.identifier !== undefined) ?? tokens[0];
}

function locationOf(filePath: string, row: number, col: number, length: number):
    Location|undefined {
  // Standard library modules are fetched from GitHub and have relative paths
  // that can't be opened.
  if (!filePath.startsWith('/') && !/^[a-zA-Z]:\//.test(filePath)) {
    return undefined;
  }
  return {
    uri: URI.file(filePath).toString(),
    range: {
      start: {line: row, character: col},
      end: {line: row, character: col + length},
    },
  };
}

function identifierLocations(token: SemanticToken): Location[] {
  const locations: Location[] = [];
  for (const ref of token.identifier?.tokens ?? []) {
    const location = locationOf(
        ref.filePath, ref.token.pos.rowBegin - 1, ref.token.pos.columnBegin - 1,
        ref.token.text.length);
    if (location !== undefined) {
      locations.push(location);
    }
  }
  return locations;
}

function hoverText(token: SemanticToken): string {
  const kind = token.modifiers.includes('defaultLibrary') ? 'module' :
      token.type === 'property'                         ? 'field' :
                                                          token.type;
  const name =
      token.parentName ? `${token.parentName}.${token.text}` : token.text;
  return `${kind} \`${name}\``;
}

function encodeTokens(tokens: SemanticToken[]): SemanticTokens {
  // Tokens must be sent in document order, once per position.
  const sorted = tokens.slice().sort((a, b) => a.row - b.row || a.col - b.col);
  const builder = new SemanticTokensBuilder();
  let lastRow = -1, lastCol = -1;
  for (const token of sorted) {
    if (token.row === lastRow && token.col === lastCol) {
      continue;
    }
    lastRow = token.row;
    lastCol = token.col;
    let modifiers = 0;
    for (const modifier of token.modifiers) {
      modifiers |= tokenModifierBits.get(modifier) ?? 0;
    }
    builder.push(
        token.row, token.col, token.text.length,
        tokenTypeIndices.get(token.type) ?? 0, modifiers);
  }
  return builder.build();
}

// Reads the standard library from the configured directory, or the bundled
// copy if none is configured or it can't be read.
function loadLibs(libraryPath: string|undefined) {
  if (libraryPath) {
    try {
      return loadZinniaLibs(libraryPath);
    } catch (e) {
      console.error(
          `Could not read the standard library from ${libraryPath}; using ` +
              'the bundled copy.',
          e);
    }
  }
  return loadZinniaLibs();
}

// Whether the client can be asked to watch files for the server.
let canWatchFiles = false;

connection.onInitialize((params: InitializeParams): InitializeResult => {
  const libraryPath = params.initializationOptions?.libraryPath as string |
      undefined;
  analyzer.init(params.workspaceFolders ?? [], () => loadLibs(libraryPath));
  canWatchFiles = params.capabilities.workspace?.didChangeWatchedFiles
                      ?.dynamicRegistration === true;
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      semanticTokensProvider: {
        legend: {
          tokenTypes: ZINNIA_TOKEN_TYPES,
          tokenModifiers: ZINNIA_TOKEN_MODIFIERS,
        },
        full: true,
      },
      hoverProvider: true,
      definitionProvider: true,
      referencesProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      renameProvider: {prepareProvider: true},
      completionProvider: {triggerCharacters: ['.']},
      signatureHelpProvider: {triggerCharacters: ['(', ','], retriggerCharacters: [',']},
    },
  };
});

connection.onInitialized(async () => {
  if (canWatchFiles) {
    // Hear about Zinnia files that change outside the editor, e.g. on a
    // checkout.
    connection.client.register(
        DidChangeWatchedFilesNotification.type,
        {watchers: [{globPattern: '**/*.zn'}]});
  }
  const start = Date.now();
  const count = await analyzer.indexWorkspace();
  connection.console.log(
      `Indexed ${count} Zinnia files in ${Date.now() - start} ms.`);
});

connection.onDidChangeWatchedFiles(params => {
  for (const change of params.changes) {
    const filePath = filePathOf(change.uri);
    if (!filePath.endsWith('.zn')) {
      continue;
    }
    if (change.type === FileChangeType.Deleted) {
      analyzer.forgetFile(filePath);
    } else {
      analyzer.refreshFile(filePath);
    }
  }
});

documents.onDidOpen(event => {
  analyzer.setOpen(filePathOf(event.document.uri), true);
});

documents.onDidChangeContent(async change => {
  const document = change.document;
  const info = await analyze(document);
  // Skip stale results if the document changed while it was analyzed.
  if (documents.get(document.uri)?.version !== document.version) {
    return;
  }
  const end = document.positionAt(document.getText().length);
  const diagnostics: Diagnostic[] = info.problems.map(problem => {
    const start = problem.row === undefined ?
        end :
        {line: problem.row, character: problem.col ?? 0};
    return {
      severity: problem.severity === 'warning' ? DiagnosticSeverity.Warning :
                                                 DiagnosticSeverity.Error,
      range: {
        start: start,
        end: {...start, character: start.character + (problem.length ?? 1)}
      },
      message: problem.message,
      source: 'zinnia',
    };
  });
  connection.sendDiagnostics({uri: document.uri, diagnostics: diagnostics});
});

documents.onDidClose(event => {
  analyses.delete(event.document.uri);
  analyzer.setOpen(filePathOf(event.document.uri), false);
  connection.sendDiagnostics({uri: event.document.uri, diagnostics: []});
});

connection.languages.semanticTokens.on(async params => {
  const info = await analysisFor(params.textDocument.uri);
  return encodeTokens(info?.tokens ?? []);
});

connection.onHover(async (params): Promise<Hover|null> => {
  const info = await analysisFor(params.textDocument.uri);
  const token = info && tokenAt(info, params.position);
  if (token === undefined || token.type === 'keyword') {
    return null;
  }
  return {
    contents: {kind: MarkupKind.Markdown, value: hoverText(token)},
    range: {
      start: {line: token.row, character: token.col},
      end: {line: token.row, character: token.col + token.text.length},
    },
  };
});

connection.onDefinition(async (params): Promise<Definition|null> => {
  const info = await analysisFor(params.textDocument.uri);
  const token = info && tokenAt(info, params.position);
  // An identifier's first token is its declaration.
  const declaration = token && identifierLocations(token)[0];
  return declaration ?? null;
});

connection.onReferences(async (params): Promise<Location[]> => {
  const info = await analysisFor(params.textDocument.uri);
  const token = info && tokenAt(info, params.position);
  if (token === undefined) {
    return [];
  }
  const locations = identifierLocations(token);
  return params.context.includeDeclaration ? locations : locations.slice(1);
});

connection.onDocumentSymbol(async (params): Promise<DocumentSymbol[]> => {
  const info = await analysisFor(params.textDocument.uri);
  return info?.module === undefined ? [] : documentSymbols(info.module.ast);
});

const COMPLETION_KINDS: Record<string, CompletionItemKind> = {
  class: CompletionItemKind.Class,
  function: CompletionItemKind.Function,
  method: CompletionItemKind.Method,
  property: CompletionItemKind.Field,
  parameter: CompletionItemKind.Variable,
  variable: CompletionItemKind.Variable,
};

// The classes of the document and of builtin, for receivers whose class isn't
// known.
function knownClasses(info: SemanticDocInfo): {cls: SemanticClass, text: string}[] {
  const builtin = analyzer.lookupDocInfoFromModuleName('builtin');
  return [
    ...(info.module?.classes ?? []).map(cls => ({cls: cls, text: info.text})),
    ...(builtin?.module?.classes ?? []).map(cls => ({cls: cls, text: builtin!.text})),
  ];
}

async function memberCompletions(
    info: SemanticDocInfo, receiver: string, line: number): Promise<CompletionItem[]> {
  if (receiver === 'self') {
    const cls = classAt(info, line);
    if (cls !== undefined) {
      return classMembers(cls, info.text);
    }
  }
  const imprt = info.module?.imports.find(i => i.name.id === receiver);
  if (imprt !== undefined) {
    const module = await analyzer.importedModule(info.path, imprt);
    return module === undefined ? [] :
                                  moduleMembers(module, moduleNameOf(imprt.ast), analyzer);
  }
  // The receiver's class isn't known, so offer the members of all known
  // classes.
  const items = new Map<string, CompletionItem>();
  for (const {cls, text} of knownClasses(info)) {
    for (const item of classMembers(cls, text)) {
      if (!items.has(item.label)) {
        items.set(item.label, item);
      }
    }
  }
  for (const methods of Object.values(NATIVE_METHODS)) {
    for (const [name, params] of Object.entries(methods)) {
      if (!items.has(name)) {
        items.set(name, {
          label: name,
          kind: CompletionItemKind.Method,
          detail: nativeSignature(name, params).label,
        });
      }
    }
  }
  return [...items.values()];
}

connection.onCompletion(async (params): Promise<CompletionItem[]> => {
  const info = await analysisFor(params.textDocument.uri);
  const document = documents.get(params.textDocument.uri);
  if (info !== undefined && document !== undefined) {
    const access =
        memberAccessAt(document.getText(), document.offsetAt(params.position));
    if (access !== undefined) {
      return memberCompletions(info, access.receiver, params.position.line);
    }
  }
  const items = new Map<string, CompletionItem>();
  for (const keyword of KEYWORDS) {
    items.set(keyword, {label: keyword, kind: CompletionItemKind.Keyword});
  }
  for (const token of info?.tokens ?? []) {
    const kind = token.modifiers.includes('defaultLibrary') ?
        CompletionItemKind.Module :
        COMPLETION_KINDS[token.type];
    if (kind !== undefined && !items.has(token.text)) {
      items.set(token.text, {label: token.text, kind: kind});
    }
  }
  return [...items.values()];
});

async function signatureFor(
    info: SemanticDocInfo, receiver: string|undefined, name: string,
    line: number): Promise<SignatureInformation|undefined> {
  const methodOf = (cls: SemanticClass|undefined, text: string) => {
    const method = cls?.methods.get(name);
    if (method !== undefined) {
      return signatureInformation(
          name, {params: method.ast.params, isNamed: method.ast.isNamed}, text);
    }
    const native = cls && nativeMethodsOfClass(cls)[name];
    return native && nativeSignature(name, native);
  };
  if (receiver === 'self') {
    const cls = classAt(info, line);
    if (cls !== undefined) {
      return methodOf(cls, info.text);
    }
    // At the top level, `self` is the module.
    receiver = undefined;
  }
  if (receiver !== undefined) {
    const imprt = info.module?.imports.find(i => i.name.id === receiver);
    if (imprt !== undefined) {
      const module = await analyzer.importedModule(info.path, imprt);
      const id = module &&
          [...module.context?.block.definitions() ?? []].find(d => d.id === name);
      if (id !== undefined) {
        return identifierSignature(name, id, module, analyzer);
      }
      const native = NATIVE_FUNCTIONS[moduleNameOf(imprt.ast) ?? '']?.[name];
      return native && nativeSignature(name, native);
    }
    // The receiver's class isn't known: use the first known class with a
    // method of this name, then the first native one.
    for (const {cls, text} of knownClasses(info)) {
      const signature = methodOf(cls, text);
      if (signature !== undefined) {
        return signature;
      }
    }
    for (const methods of Object.values(NATIVE_METHODS)) {
      if (methods[name] !== undefined) {
        return nativeSignature(name, methods[name]);
      }
    }
    return undefined;
  }
  const local =
      [...info.context?.block.definitions() ?? []].find(d => d.id === name);
  const localSignature =
      local && identifierSignature(name, local, info, analyzer);
  if (localSignature !== undefined) {
    return localSignature;
  }
  const builtin = analyzer.searchBuiltinForId(name);
  const builtinSignature = builtin &&
      identifierSignature(
          name, builtin, analyzer.lookupDocInfoFromModuleName('builtin'),
          analyzer);
  if (builtinSignature !== undefined) {
    return builtinSignature;
  }
  const native = NATIVE_FUNCTIONS.builtin[name];
  return native && nativeSignature(name, native);
}

// The name of the module [imprt] imports, unless it's given as a path.
function moduleNameOf(imprt: ImportStat): string|undefined {
  return imprt.source.kind === 'StringExpr' ? undefined : imprt.source.token.text;
}

connection.onSignatureHelp(async (params): Promise<SignatureHelp|null> => {
  const info = await analysisFor(params.textDocument.uri);
  const document = documents.get(params.textDocument.uri);
  if (info === undefined || document === undefined) {
    return null;
  }
  const call = callAt(document.getText(), document.offsetAt(params.position));
  const signature = call &&
      await signatureFor(info, call.receiver, call.name, params.position.line);
  if (signature === undefined) {
    return null;
  }
  return {
    signatures: [signature],
    activeSignature: 0,
    activeParameter: Math.min(
        call!.argIndex, Math.max(0, (signature.parameters?.length ?? 1) - 1)),
  };
});

connection.onPrepareRename(async (params): Promise<PrepareRenameResult|null> => {
  const info = await analysisFor(params.textDocument.uri);
  const target = renameTarget(info && tokenAt(info, params.position), analyzer);
  if ('error' in target) {
    throw new ResponseError(ErrorCodes.InvalidRequest, target.error);
  }
  return {
    range: {
      start: {line: target.token.row, character: target.token.col},
      end: {
        line: target.token.row,
        character: target.token.col + target.token.text.length,
      },
    },
    placeholder: target.id.id,
  };
});

connection.onRenameRequest(async (params): Promise<WorkspaceEdit|null> => {
  const info = await analysisFor(params.textDocument.uri);
  const target = renameTarget(info && tokenAt(info, params.position), analyzer);
  if ('error' in target) {
    throw new ResponseError(ErrorCodes.InvalidRequest, target.error);
  }
  const error = invalidName(params.newName);
  if (error !== undefined) {
    throw new ResponseError(ErrorCodes.InvalidParams, error);
  }
  const changes: Record<string, TextEdit[]> = {};
  for (const loc of renameLocations(target, analyzer.analyzedDocuments())) {
    const location = locationOf(loc.filePath, loc.row, loc.col, loc.length);
    if (location !== undefined) {
      (changes[location.uri] ??= [])
          .push({range: location.range, newText: params.newName});
    }
  }
  return {changes: changes};
});

connection.onWorkspaceSymbol(async (params): Promise<WorkspaceSymbol[]> => {
  const symbols: WorkspaceSymbol[] = [];
  for (const doc of analyzer.analyzedDocuments()) {
    if (doc.module === undefined || analyzer.isStandardLibraryFile(doc.path)) {
      continue;
    }
    for (const {filePath, symbol, containerName} of workspaceSymbols(
             doc.path, doc.module.ast, params.query)) {
      const location = locationOf(
          filePath, symbol.selectionRange.start.line,
          symbol.selectionRange.start.character,
          symbol.selectionRange.end.character -
              symbol.selectionRange.start.character);
      if (location !== undefined) {
        symbols.push({
          name: symbol.name,
          kind: symbol.kind,
          location: location,
          containerName: containerName,
        });
      }
    }
  }
  return symbols;
});

documents.listen(connection);
connection.listen();
