// Reading the output of zinnia's test runner, test.Tester.

// Removes the ANSI color codes zinnia prints.
export function stripColors(text: string): string {
  return text.replace(/\x1b\[[0-9;]*m/g, '');
}

// The failures that zinnia's test runner reports in [output]: for each
// `Class.method`, the error printed after it.
export function parseFailures(output: string): Map<string, string> {
  const failures = new Map<string, string>();
  const lines = stripColors(output).split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    const match = /^Test ([\w$]+\.[\w$]+)=FAIL\s*$/.exec(lines[i]);
    if (match === null) {
      continue;
    }
    const message: string[] = [];
    for (i++; i < lines.length && !/^Test [\w$]+\.[\w$]+=|^Tests \[/.test(lines[i]); i++) {
      message.push(lines[i]);
    }
    i--;
    failures.set(match[1], message.join('\n').trim());
  }
  return failures;
}

// Whether [output] has the summary zinnia's test runner prints once it has
// run the tests.
export function ranTests(output: string): boolean {
  return /^Tests \[\d+\/\d+\] passed\./m.test(stripColors(output));
}

