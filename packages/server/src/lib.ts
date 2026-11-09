import {readdirSync, readFileSync} from 'fs';
import * as path from 'path';

// A module of the Zinnia standard library.
export interface LibFile {
  // The module's name, e.g. 'io'.
  name: string;
  // The absolute path of its source.
  path: string;
  text: string;
}

// [filePath] with '/' separators and, on Windows, a lowercase drive letter, as
// VS Code's URIs have, so that the same file always has the same path.
export function normalizePath(filePath: string): string {
  return filePath.replace(/\\/g, '/').replace(/^([A-Z]):/, (_, d: string) => `${d.toLowerCase()}:`);
}

// The copy of the standard library bundled with the server. See
// scripts/update-stdlib.sh.
export const BUNDLED_LIB_DIR = path.join(__dirname, '..', 'stdlib');

// Reads the standard library's modules from [dir], which defaults to the
// bundled copy.
export function loadZinniaLibs(dir: string = BUNDLED_LIB_DIR): LibFile[] {
  return readdirSync(dir)
      .filter(file => file.endsWith('.zn'))
      .map(file => {
        const filePath = normalizePath(path.join(dir, file));
        return {
          name: file.slice(0, -'.zn'.length),
          path: filePath,
          text: readFileSync(filePath, 'utf8'),
        };
      });
}
