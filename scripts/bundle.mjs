// Bundles the extension's client and the language server, each into one file
// with its dependencies, from their TypeScript sources. `tsc -b` still type
// checks them and builds the tests.
//
// Usage: node scripts/bundle.mjs [--watch]
import * as esbuild from 'esbuild';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const watch = process.argv.includes('--watch');

const common = {
  absWorkingDir: root,
  bundle: true,
  platform: 'node',
  format: 'cjs',
  target: 'node20',
  sourcemap: true,
  logLevel: 'info',
  // Use the shared package's sources rather than its compiled output.
  alias: {'zinnia-language-shared': './packages/shared/src'},
};

const builds = [
  {
    ...common,
    entryPoints: ['packages/client/src/extension.ts'],
    outfile: 'packages/client/dist/extension.js',
    // Provided by VS Code.
    external: ['vscode'],
  },
  {
    ...common,
    entryPoints: ['packages/server/src/server.ts'],
    // The server finds the bundled standard library in ../stdlib.
    outfile: 'packages/server/dist/server.js',
  },
];

if (watch) {
  for (const options of builds) {
    const context = await esbuild.context(options);
    await context.watch();
  }
} else {
  await Promise.all(builds.map(options => esbuild.build(options)));
}
