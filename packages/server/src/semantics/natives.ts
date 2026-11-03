// Methods and functions that Zinnia defines natively, in C, so they don't
// appear in the standard library's source. Each maps a name to its parameter
// names. Private helpers (named with a leading __) are left out.
//
// See zinnia/value/native/*.c in the zinnia repository.

export type NativeMembers = Record<string, string[]>;

// Native methods, by class.
export const NATIVE_METHODS: Record<string, NativeMembers> = {
  Object: {
    class: [],
    super: [],
    hash: [],
    copy: [],
    $get: ['name'],
    $set: ['name', 'value'],
    $adr: [],
    $hex_addr: [],
  },
  String: {
    len: [],
    extend: ['s'],
    copy: [],
    trim: [],
    ltrim: [],
    rtrim: [],
    lshrink: ['n'],
    rshrink: ['n'],
    split: ['delim'],
  },
  IString: {
    len: [],
    copy: [],
    trim: [],
    ltrim: [],
    rtrim: [],
    split: ['delim'],
  },
  Array: {len: [], append: ['value']},
  Tuple: {len: []},
  Range: {start: [], end: [], inc: []},
  Class: {
    name: [],
    module: [],
    super: [],
    fields: [],
    methods: [],
    is_subclass_of: ['cls'],
  },
  Function: {name: [], module: [], is_method: [], parent_class: []},
  FunctionRef: {name: [], module: [], obj: [], func: []},
  Module: {name: [], functions: [], classes: []},
  Isolate: {start: []},
  ActorRef: {isolate: []},
  StackLine: {module: [], function: [], source_linetext: []},
  DataArray: {len: [], to_arr: [], reversed: [], sorted: []},
  DataMatrix: {shape: []},
  Socket: {accept: [], connect: [], close: [], host: [], port: []},
  SocketHandle: {send: ['text'], receive: [], close: []},
};

// Native functions, by module.
export const NATIVE_FUNCTIONS: Record<string, NativeMembers> = {
  builtin: {
    Int: ['x'],
    Float: ['x'],
    Bool: ['x'],
    color: ['code'],
    is_int: ['x'],
    is_float: ['x'],
    is_bool: ['x'],
  },
  math: {
    abs: ['x'],
    ceil: ['x'],
    floor: ['x'],
    exp: ['x'],
    pow: ['num', 'power'],
    mod: ['n', 'd'],
    sin: ['x'],
    cos: ['x'],
    tan: ['x'],
    asin: ['x'],
    acos: ['x'],
    atan: ['x'],
  },
};

// Native classes, by module, which have no definition in source.
export const NATIVE_CLASSES: Record<string, string[]> = {
  builtin: ['Range', 'Isolate', 'ActorRef'],
  socket: ['Socket', 'SocketHandle'],
};

// The native methods of [className], including those every object has.
export function nativeMethodsOf(className: string): NativeMembers {
  return {...NATIVE_METHODS.Object, ...(NATIVE_METHODS[className] ?? {})};
}
