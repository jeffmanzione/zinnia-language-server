// The semantic token legend shared by the client and the server. This file must
// not import 'vscode', since the server runs outside VS Code.

export const ZINNIA_TOKEN_TYPES = [
  'comment',       'string',   'keyword', 'number',    'regexp',    'operator',
  'namespace',     'type',     'struct',  'class',     'interface', 'enum',
  'typeParameter', 'function', 'method',  'decorator', 'macro',     'variable',
  'parameter',     'property', 'label'
];

export const ZINNIA_TOKEN_MODIFIERS = [
  'declaration', 'defaultLibrary', 'documentation', 'readonly', 'static',
  'abstract', 'deprecated', 'modification', 'async', 'local'
];
