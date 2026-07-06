import {ExtensionContext, workspace} from 'vscode';
import {LanguageClient, LanguageClientOptions, ServerOptions, TransportKind} from 'vscode-languageclient/node';

import {SERVER_ENTRY_POINT_PATH, ZINNIA_DOCUMENT_SELECTOR, ZINNIA_LANGUAGE_ID, ZINNIA_LANGUAGE_NAME} from './constants';
import {createDocumentTokensProvider as createDocumentTokensSubscription, createHoverSubscription} from './providers';

let client: LanguageClient;

const clientProvider = () => client;

export function activate(context: ExtensionContext) {
  context.subscriptions.push(
      createDocumentTokensSubscription(clientProvider),
      createHoverSubscription(clientProvider),
  );

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
    documentSelector: [ZINNIA_DOCUMENT_SELECTOR],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in
      // the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
      ZINNIA_LANGUAGE_ID, ZINNIA_LANGUAGE_NAME, serverOptions, clientOptions);

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void>|undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
