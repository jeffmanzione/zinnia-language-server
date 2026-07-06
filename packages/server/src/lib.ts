import {DirEntry} from './interfaces';

const ZINNIA_GITHUB_REPO = 'jeffmanzione/zinnia';

interface FileContents {
  uri: string;
  text: string;
}

const fetchLibPaths_ = async(): Promise<string[]> => {
  const resp = await fetch(
      `https://api.github.com/repos/${ZINNIA_GITHUB_REPO}/contents/zinnia/lib`);
  const data = (await resp.json()) as DirEntry[];

  const libFilePaths: string[] = [];

  for (const entry of data) {
    if (entry.name.endsWith('.zn')) {
      libFilePaths.push(entry.path);
    }
  }
  return libFilePaths;
};

const fetchFileContents_ = async(filePath: string): Promise<FileContents> => {
  const libUrl = `https://raw.githubusercontent.com/${
      ZINNIA_GITHUB_REPO}/refs/heads/master/${filePath}`;
  try {
    const resp = await fetch(libUrl, {signal: AbortSignal.timeout(30000)});
    return {uri: filePath, text: await resp.text()};
  } catch (e: any) {
    console.error(libUrl);
    console.error(e);
    return {uri: filePath, text: ''};
  }
};

export const fetchZinniaLibs = async(): Promise<FileContents[]> => {
  return Promise.all(await fetchLibPaths_().then(
      filePaths => filePaths.map(path => fetchFileContents_(path))));
};