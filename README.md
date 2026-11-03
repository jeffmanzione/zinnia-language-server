# Zinnia Language LSP

Zinnia Language (.zn) language extension

## Functionality

The server speaks the standard [Language Server Protocol](https://microsoft.github.io/language-server-protocol/),
so it works with any LSP client, not only VS Code. It provides:

- Semantic highlighting.
- Diagnostics:
  - syntax errors, with recovery so the rest of the file is still analyzed;
  - undefined names;
  - top-level variables used in functions and methods, which can't see them;
  - reassigned `const`s;
  - calls with the wrong number of arguments;
  - imports of modules that can't be found.
- Hover information.
- Go to definition (including into the standard library) and find references.
- Document symbols (the outline).
- Completion of keywords, names in scope, and members after `module.` and
  `self.`.
- Signature help for calls.

## Standard library

The server bundles a copy of the
[Zinnia standard library](https://github.com/jeffmanzione/zinnia/tree/master/zinnia/lib)
in `packages/server/stdlib`. The commit it was copied from is in
`packages/server/stdlib/ZINNIA_COMMIT`.

To use a different copy, for example the one in your own checkout of zinnia,
set `zinnia.libraryPath` to its directory (e.g. `/path/to/zinnia/zinnia/lib`)
and restart the extension.

## Development

```shell
npm install
npm test        # compiles and runs the tests
npm run watch   # recompiles on change
```

Press F5 in VS Code to launch an Extension Development Host with the extension.

To update the bundled standard library to a commit, branch or tag of zinnia:

```shell
scripts/update-stdlib.sh master
```

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
