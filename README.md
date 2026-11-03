# Zinnia Language LSP

Zinnia Language (.zn) language extension

## Functionality

The server speaks the standard [Language Server Protocol](https://microsoft.github.io/language-server-protocol/),
so it works with any LSP client, not only VS Code. It provides:

- Semantic highlighting.
- Diagnostics for syntax errors.
- Hover information.
- Go to definition and find references.
- Document symbols (the outline).
- Completion of keywords and the names used in the file.

The standard library is fetched from the
[zinnia repository](https://github.com/jeffmanzione/zinnia) when the server
starts.

## Development

```shell
npm install
npm test        # compiles and runs the tests
npm run watch   # recompiles on change
```

Press F5 in VS Code to launch an Extension Development Host with the extension.

To use the server from another editor, run it over stdio:

```shell
node packages/server/out/server.js --stdio
```

## Structure

```
.
├── client // Language Client
│   ├── src
│   │   └── extension.ts // Language Client entry point
├── package.json // The extension manifest.
└── server // Language Server
    └── src
        └── server.ts // Language Server entry point
```
