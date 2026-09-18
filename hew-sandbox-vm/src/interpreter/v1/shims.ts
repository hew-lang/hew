/// The VM's runtime-family and extern shim table.
///
/// This table is the admission authority (D517 Q415): a package may name a
/// runtime family or an extern symbol exactly when there is a shim here for it.
/// A family or symbol with no entry is rejected at load, so there is no second
/// reject list to keep in step with this one. `FileRead` and the `NativeIo`
/// suspend kind are refused by their absence.

import { UNIT, cloneValue, renderStdout, type VmValue } from "../values.js";
import { Mt19937, seededMt } from "./mt19937.js";
import type { RuntimeFamilyEntry, TrapName, VariantShape } from "./package.js";

/// A language-visible fault raised inside a shim. The executor routes it to the
/// call's `unwind` edge, or ends the program when the call names none.
export class ShimFault extends Error {
  constructor(
    readonly trap: TrapName,
    message: string,
  ) {
    super(message);
    this.name = "ShimFault";
  }
}

/// What a shim may reach for. The executor owns faults and traces; a shim
/// never touches the frame stack or the value environment.
export interface ShimHost {
  /// Write to the program's standard output.
  writeStdout(text: string): void;
  /// The next line of replay stdin, and the replay record for it.
  readLine(): string;
  /// The program's own generator. `hew_random_seed` replaces it, and it is
  /// separate from the scheduler's chaos stream.
  prng: Mt19937;
  /// The package's regex literal pool.
  regexPatterns: readonly string[];
  /// Build a value of the enum descriptor the call demands. The tag comes from
  /// that descriptor's declaration order, never from a guess.
  enumValue(shape: VariantShape, caseName: string, payload: VmValue[]): VmValue;
}

/// `shape` is the `result_shape` the call names: the variant descriptor its
/// result is built against, or `null` when the result is not an enum.
export type RuntimeShim = (
  host: ShimHost,
  args: VmValue[],
  shape: VariantShape | null,
) => VmValue;

/// Resolve a declared runtime family to its shim, or `undefined` when the VM
/// has none. Both the load-time validator and the executor go through here, so
/// admission and execution can never disagree about what is implemented.
export function resolveRuntimeShim(
  entry: RuntimeFamilyEntry,
): RuntimeShim | undefined {
  switch (entry.family) {
    case "Print":
      return printShim(entry.detail);
    case "Vector":
      return VECTOR_SHIMS[detailName(entry.detail)];
    case "Map":
      return MAP_SHIMS[detailName(entry.detail)];
    default:
      return UNIT_FAMILY_SHIMS[entry.family];
  }
}

export function resolveExternShim(symbol: string): RuntimeShim | undefined {
  return EXTERN_SHIMS[symbol];
}

/// The suspend kinds a sequential package may carry. `NativeIo` is absent, so
/// a package that suspends on native I/O is refused at load.
export const SUPPORTED_SUSPEND_KINDS: ReadonlySet<string> = new Set([
  "ValueClose",
  "Sleep",
]);

// ── families ────────────────────────────────────────────────────────────────

/// `Print { kind, newline }`. The rendering follows the value's own
/// representation: casts normalize an integer into its target range, so the
/// declared `kind` never changes how a value prints.
function printShim(detail: unknown): RuntimeShim | undefined {
  if (typeof detail !== "object" || detail === null) {
    return undefined;
  }
  const newline = (detail as { newline?: unknown }).newline === true;
  return (host, args) => {
    host.writeStdout(
      newline ? `${renderStdout(arg(args, 0))}\n` : renderStdout(arg(args, 0)),
    );
    return UNIT;
  };
}

const UNIT_FAMILY_SHIMS: Record<string, RuntimeShim | undefined> = {
  StringConcat: (_host, args) => str(`${text(args, 0)}${text(args, 1)}`),
  StringClone: (_host, args) => str(text(args, 0)),
  StringEquals: (_host, args) => bool(text(args, 0) === text(args, 1)),
  // Native string length counts codepoints, not bytes.
  StringLen: (_host, args) => int(BigInt([...text(args, 0)].length)),
  StringReplace: (_host, args) =>
    str(text(args, 0).split(text(args, 1)).join(text(args, 2))),
  // Native slicing is by codepoint and clamps rather than trapping.
  StringSlice: (_host, args) => {
    const chars = [...text(args, 0)];
    const start = Math.min(Math.max(index(args, 1), 0), chars.length);
    const end = Math.min(Math.max(index(args, 2), start), chars.length);
    return str(chars.slice(start, end).join(""));
  },
  I64ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  U64ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  U32ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  I32ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  U8ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  BoolToString: (_host, args) => str(renderStdout(arg(args, 0))),
  F64ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  CharToString: (_host, args) => str(renderStdout(arg(args, 0))),
  BytesDecodeUtf8: (_host, args) => str(decodeUtf8(arg(args, 0))),
};

const VECTOR_SHIMS: Record<string, RuntimeShim | undefined> = {
  New: () => ({ kind: "vector", elementType: "", items: [] }),
  // The receiver arrives by `move` and leaves as the call's result, so the
  // package can store the vector back into its place. The element carries the
  // ownership decision SIR proved, so it moves in as it arrives.
  Push: (_host, args) => {
    const target = vec(args, 0);
    target.items.push(arg(args, 1));
    return target;
  },
  Get: (host, args, shape) => {
    if (!shape) {
      throw new TypeError(
        "Vector::Get names no result shape to build its Option against",
      );
    }
    const items = vec(args, 0).items;
    const at = index(args, 1);
    return at >= 0 && at < items.length
      ? host.enumValue(shape, "Some", [cloneValue(items[at]!)])
      : host.enumValue(shape, "None", []);
  },
  Index: (_host, args) => {
    const items = vec(args, 0).items;
    const at = index(args, 1);
    if (at < 0 || at >= items.length) {
      throw new ShimFault(
        "vector_bounds",
        `vector index ${at} out of bounds for length ${items.length}`,
      );
    }
    return cloneValue(items[at]!);
  },
  Len: (_host, args) => int(BigInt(vec(args, 0).items.length)),
  Contains: (_host, args) => {
    const needle = comparable(arg(args, 1));
    return bool(vec(args, 0).items.some((item) => comparable(item) === needle));
  },
  Slice: (_host, args) => {
    const items = vec(args, 0).items;
    const start = Math.min(Math.max(index(args, 1), 0), items.length);
    const end = Math.min(Math.max(index(args, 2), start), items.length);
    return {
      kind: "vector",
      elementType: "",
      items: items.slice(start, end).map(cloneValue),
    };
  },
  SliceFrom: (_host, args) => {
    const items = vec(args, 0).items;
    const start = Math.min(Math.max(index(args, 1), 0), items.length);
    return {
      kind: "vector",
      elementType: "",
      items: items.slice(start).map(cloneValue),
    };
  },
};

const MAP_SHIMS: Record<string, RuntimeShim | undefined> = {
  New: () => ({ kind: "map", entries: new Map() }),
  Insert: (_host, args) => {
    const target = map(args, 0);
    const key = arg(args, 1);
    target.entries.set(comparable(key), { key, value: arg(args, 2) });
    return target;
  },
};

// ── externs ─────────────────────────────────────────────────────────────────

const EXTERN_SHIMS: Record<string, RuntimeShim | undefined> = {
  hew_regex_new: (host, args) => compileRegex(host, args),
  hew_regex_clone: (_host, args) => cloneValue(arg(args, 0)),
  // Releasing a handle the VM traces by reference is nothing to do.
  hew_regex_free: () => UNIT,
  hew_regex_is_match: (_host, args) => {
    const value = arg(args, 0);
    return bool(value.kind === "regex" && value.regex.test(text(args, 1)));
  },
  hew_regex_is_valid: (host, args) => {
    try {
      // eslint-disable-next-line no-new
      new RegExp(pattern(host, arg(args, 0)));
      return bool(true);
    } catch {
      return bool(false);
    }
  },
  hew_regex_find: (_host, args) => {
    const value = arg(args, 0);
    return str(
      value.kind === "regex"
        ? (value.regex.exec(text(args, 1))?.[0] ?? "")
        : "",
    );
  },
  hew_io_read_line: (host) => str(host.readLine()),
  hew_random_seed: (host, args) => {
    host.prng = seededMt(integer(args, 0));
    return UNIT;
  },
  hew_random_randint: (host, args) => {
    const low = integer(args, 0);
    const high = integer(args, 1);
    return int(high <= low ? low : low + host.prng.randbelow(high - low));
  },
};

/// `bytes` is a vector of byte values, so decoding walks its items.
function decodeUtf8(value: VmValue): string {
  if (value.kind === "string") {
    return value.value;
  }
  if (value.kind !== "vector") {
    return renderStdout(value);
  }
  return new TextDecoder().decode(
    Uint8Array.from(
      value.items.map((item) => (item.kind === "i64" ? Number(item.value) : 0)),
    ),
  );
}

function compileRegex(host: ShimHost, args: VmValue[]): VmValue {
  const source = pattern(host, arg(args, 0));
  return { kind: "regex", source, regex: new RegExp(source) };
}

/// A regex extern names its pattern either as a literal-pool index or as a
/// string value; both reach the same compiled handle.
function pattern(host: ShimHost, value: VmValue): string {
  if (value.kind === "i64") {
    return host.regexPatterns[Number(value.value)] ?? "";
  }
  return value.kind === "string" ? value.value : renderStdout(value);
}

// ── operand helpers ─────────────────────────────────────────────────────────

function detailName(detail: unknown): string {
  return typeof detail === "string" ? detail : "";
}

function arg(args: VmValue[], at: number): VmValue {
  return args[at] ?? UNIT;
}

function text(args: VmValue[], at: number): string {
  const value = arg(args, at);
  return value.kind === "string" ? value.value : renderStdout(value);
}

function integer(args: VmValue[], at: number): bigint {
  const value = arg(args, at);
  return value.kind === "i64" ? value.value : 0n;
}

function index(args: VmValue[], at: number): number {
  return Number(integer(args, at));
}

function vec(
  args: VmValue[],
  at: number,
): Extract<VmValue, { kind: "vector" }> {
  const value = arg(args, at);
  if (value.kind !== "vector") {
    throw new TypeError(
      `runtime call expected a vector operand, got ${value.kind}`,
    );
  }
  return value;
}

function map(args: VmValue[], at: number): Extract<VmValue, { kind: "map" }> {
  const value = arg(args, at);
  if (value.kind !== "map") {
    throw new TypeError(
      `runtime call expected a map operand, got ${value.kind}`,
    );
  }
  return value;
}

function comparable(value: VmValue): string {
  return `${value.kind}:${renderStdout(value)}`;
}

function str(value: string): VmValue {
  return { kind: "string", value };
}

function bool(value: boolean): VmValue {
  return { kind: "bool", value };
}

function int(value: bigint): VmValue {
  return { kind: "i64", value };
}
