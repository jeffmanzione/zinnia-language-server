# Zinnia Language Support

Language support for [Zinnia](https://github.com/jeffmanzione/zinnia) (`.zn`
files): a VS Code extension, and a language server that works with any editor
that speaks the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

## Features

- **Highlighting**: a TextMate grammar, plus semantic highlighting that tells
  classes, functions, methods, fields, parameters, modules and constants
  apart.
  By default, modules are green, parameters gold and fields blue in every
  theme; to change them, set `editor.semanticTokenColorCustomizations` rules
  such as `"parameter:zinnia": {"foreground": "#..."}`.
- **Diagnostics**, as you type:
  - syntax errors, with recovery so the rest of the file is still analyzed;
  - undefined names, with quick fixes to import a missing module or change
    the name to a similar one;
  - top-level variables used in functions and methods, which can't see them,
    with a quick fix that makes them module members;
  - reassigned `const`s;
  - calls with the wrong number of arguments;
  - imports of modules that can't be found;
  - unused imports (with a quick fix that removes them), unused variables in
    functions and methods, and unreachable code, shown faded.
- **Navigation**: hover, go to definition (including into the standard
  library), find references, document highlights, call hierarchy, the
  outline, and workspace symbol search.
- **Completion** of keywords, names in scope, and members after `module.`,
  `self.` and objects whose class can be inferred, including inherited and
  natively defined members.
- **Signature help** and **inlay hints** for parameter names at call sites.
- **Rename** of variables, functions, classes, fields and methods across the
  workspace.
- **Formatting** of documents and ranges, and re-indenting a line when its
  closing bracket is typed. Zinnia files default to two-space indentation.
- **Folding** of blocks, comments and imports.
- **Tests** in VS Code's test explorer: each `@test.Test` method of a
  `@test.TestClass` class can be run on its own, by class, by file or all at
  once, with failures shown at the test. Tests run with `zinnia` (set
  `zinnia.executable` if it isn't on your `PATH`) from the workspace folder,
  and need a zinnia whose `test` library takes `--test`. **Zinnia: Run File**
  runs the current file in a terminal.

The server analyzes every Zinnia file in the workspace when it starts, and
follows changes made in the editor and on disk.

Classes are inferred from what's assigned: `x = Foo()`, `module.Foo()`,
literals, other variables, fields assigned in methods, and `self` in a method.
When a variable is assigned values of different classes, it isn't inferred.

## Installing

### VS Code

Install the `.vsix` from the [latest release](https://github.com/jeffmanzione/zinnia-language-server/releases/latest)
with **Extensions: Install from VSIX...**, or from the VS Code Marketplace or
Open VSX once it's published there.

### Other editors

Build the server, then run it over stdio:

```shell
git clone https://github.com/jeffmanzione/zinnia-language-server.git
cd zinnia-language-server
npm ci
npm run bundle
node packages/server/dist/server.js --stdio
```

The server reads one initialization option, `libraryPath`: the directory of
the standard library to use instead of the bundled one.

**Neovim** (0.11 or later), in your `init.lua`:

```lua
vim.filetype.add({ extension = { zn = 'zinnia' } })
vim.lsp.config('zinnia', {
  cmd = { 'node', '/path/to/zinnia-language-server/packages/server/dist/server.js', '--stdio' },
  filetypes = { 'zinnia' },
  root_markers = { '.git' },
})
vim.lsp.enable('zinnia')
```

Neovim shows the server's semantic highlighting.

**Helix**, in `languages.toml`:

```toml
[language-server.zinnia]
command = "node"
args = ["/path/to/zinnia-language-server/packages/server/dist/server.js", "--stdio"]

[[language]]
name = "zinnia"
scope = "source.zn"
file-types = ["zn"]
comment-token = ";"
indent = { tab-width = 2, unit = "  " }
language-servers = ["zinnia"]
```

Helix highlights with tree-sitter grammars, and there's none for Zinnia yet,
so files aren't highlighted there; the other features work.

## Standard library

The server bundles a copy of the
[Zinnia standard library](https://github.com/jeffmanzione/zinnia/tree/master/zinnia/lib)
in `packages/server/stdlib`. The commit it was copied from is in
`packages/server/stdlib/ZINNIA_COMMIT`. A weekly workflow opens a pull request
when zinnia's copy changes.

To use a different copy, for example the one in your own checkout of zinnia,
set `zinnia.libraryPath` (or the `libraryPath` initialization option) to its
directory, e.g. `/path/to/zinnia/zinnia/lib`, and restart the server.

Methods and functions that Zinnia implements in C, which aren't in the
library's source, are listed in `packages/server/src/semantics/natives.ts`.

## Development

```shell
npm install
npm test        # compiles and runs the tests
npm run watch   # rebundles on change, for F5
npm run bundle  # bundles the client and server into packages/*/dist
npm run build   # packages the extension as a .vsix
```

Press F5 in VS Code to launch an Extension Development Host with the extension.

To update the bundled standard library to a commit, branch or tag of zinnia:

```shell
scripts/update-stdlib.sh master
```

### Releasing

Run the **Release** workflow from the Actions tab with the version, e.g.
`1.1.0`, or:

```shell
gh workflow run release.yml -f version=1.1.0
```

It tags a commit that sets the version (`package.json` on `main` stays at
`0.0.0`), builds and tests it, and publishes a GitHub release with the `.vsix`.
With **dry run** checked, it only builds, tests and packages, and attaches the
`.vsix` to the run.

It also publishes to the VS Code Marketplace if the repository has a
`VSCE_PAT` secret (an Azure DevOps token with the Marketplace Manage scope for
the `jeffmanzione` publisher), and to Open VSX if it has an `OVSX_PAT` secret
(an open-vsx.org token for a `jeffmanzione` namespace).

## Structure

```
.
├── package.json             // The extension manifest.
├── packages
│   ├── client               // The VS Code extension, which starts the server.
│   │   └── src/extension.ts
│   ├── server               // The language server.
│   │   ├── src
│   │   │   ├── server.ts    // Entry point: the LSP requests.
│   │   │   ├── semantics    // Tokenizer, parser and semantic analysis.
│   │   │   └── test
│   │   └── stdlib           // The bundled standard library.
│   └── shared               // Language configuration and semantic token legend.
└── scripts
    └── update-stdlib.sh
```
