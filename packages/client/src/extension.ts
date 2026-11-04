import {commands, ExtensionContext, Terminal, Uri, window, workspace} from 'vscode';
import {LanguageClient, LanguageClientOptions, ServerOptions, TransportKind} from 'vscode-languageclient/node';
import {ZinniaTests} from './testing';
import {SERVER_ENTRY_POINT_PATH, ZINNIA_DOCUMENT_SELECTOR, ZINNIA_LANGUAGE_ID, ZINNIA_LANGUAGE_NAME} from 'zinnia-language-shared/constants';

let client: LanguageClient;
let terminal: Terminal|undefined;

// Quotes [arg] for a shell, if it needs it.
function shellQuote(arg: string): string {
  return /^[\w@%+=:,./-]+$/.test(arg) ? arg : `'${arg.replace(/'/g, `'\\''`)}'`;
}

// Runs the Zinnia file at [uri], or the active editor's, with the configured
// executable, in a terminal of its own.
function runFile(uri?: string): void {
  const file = uri !== undefined ? Uri.parse(uri) : window.activeTextEditor?.document.uri;
  if (file === undefined || file.scheme !== 'file') {
    window.showErrorMessage('Open a saved Zinnia file to run it.');
    return;
  }
  const executable =
      workspace.getConfiguration('zinnia').get<string>('executable') || 'zinnia';
  if (terminal === undefined || terminal.exitStatus !== undefined) {
    terminal = window.createTerminal('Zinnia');
  }
  terminal.show(true);
  terminal.sendText(`${shellQuote(executable)} ${shellQuote(file.fsPath)}`);
}

// The server provides its features through the standard Language Server
// Protocol. The client adds the test explorer, and a command that runs the
// current file.
export function activate(context: ExtensionContext) {
  context.subscriptions.push(commands.registerCommand('zinnia.runFile', runFile));
  const serverEntryPoint = context.asAbsolutePath(SERVER_ENTRY_POINT_PATH);
  // If the extension is launched in debug mode then the debug server options
  // are used Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: {
      module: serverEntryPoint,
      transport: TransportKind.ipc,
    },
    debug: {
      module: serverEntryPoint,
      transport: TransportKind.ipc,
    }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [ZINNIA_DOCUMENT_SELECTOR as any],
    initializationOptions: {
      libraryPath: workspace.getConfiguration('zinnia').get<string>('libraryPath'),
    },
  };

  // Create the language client and start the client.
  client = new LanguageClient(
      ZINNIA_LANGUAGE_ID, ZINNIA_LANGUAGE_NAME, serverOptions, clientOptions);

  // Start the client, which launches the server, then list the workspace's
  // tests.
  const zinniaTests = new ZinniaTests(context, client);
  client.start().then(() => zinniaTests.refreshAll());
}

export function deactivate(): Thenable<void>|undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
