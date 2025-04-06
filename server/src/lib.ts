import { DirEntry, DocParams } from './interfaces';

const ZINNIA_GITHUB_REPO = 'jeffmanzione/zinnia';

const _fetchLibPaths = async (): Promise<string[]> => {
  const resp = await fetch(`https://api.github.com/repos/${ZINNIA_GITHUB_REPO}/contents/lib`);
  const data = (await resp.json()) as DirEntry[];

  const libFilePaths: string[] = [];

  for (const entry of data) {
    if (entry.name.endsWith('.zn')) {
      libFilePaths.push(entry.path);
    }
  }
  return libFilePaths;
};

const _fetchFileContents = async (filePath: string): Promise<DocParams> => {
  const resp = await fetch(`https://raw.githubusercontent.com/${ZINNIA_GITHUB_REPO}/master/${filePath}`);
  return {
    uri: filePath,
    text: await resp.text(),
    version: 1
  };
};

export const fetchZinniaLibs = async (): Promise<DocParams[]> => {
  return Promise.all(
    await _fetchLibPaths()
      .then(filePaths => filePaths.map(path => _fetchFileContents(path))));
};