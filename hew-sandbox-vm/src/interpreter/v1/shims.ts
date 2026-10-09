import type { Pipes } from "./pipes.js";
/// The VM's runtime-family and extern shim table.
///
/// This table is the runtime-family admission authority: a package may name a
/// runtime family or an extern symbol exactly when there is a shim here for it.
/// A family or symbol with no entry is rejected at load, so there is no second
/// reject list to keep in step with this one. `FileRead` is refused by its
/// absence, and a `NativeIo` suspension by an operation missing from
/// [`SUPPORTED_NATIVE_IO`].

import { UNIT, cloneValue, renderStdout, type VmValue } from "../values.js";
import { Mt19937, seededMt } from "./mt19937.js";
import type {
  RuntimeFamilyEntry,
  RuntimeVariantRole,
  TrapName,
  VariantShape,
} from "./package.js";

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
  pipes?: Pipes;
  newPipe?(capacity: number): VmValue;
  closePipe?(value: VmValue): void;
  releaseValue?(value: VmValue): void;
  /// Write to the program's standard output.
  writeStdout(text: string): void;
  /// Write to the program's standard error.
  writeStderr(text: string): void;
  /// The program's own generator. `hew_random_seed` replaces it, and it is
  /// separate from the scheduler's chaos stream.
  prng: Mt19937;
  /// The package's regex literal pool.
  regexPatterns: readonly string[];
  /// Build a value of the enum descriptor the call demands. The tag comes from
  /// that descriptor's declaration order, never from a guess.
  enumValue(
    shape: VariantShape,
    role: RuntimeVariantRole,
    payload: VmValue[],
  ): VmValue;
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
    case "SupervisorPool":
      return ["Member", "Get"].includes(String(entry.detail))
        ? () => {
            throw new Error("SupervisorPool requires its resumable executor");
          }
        : undefined;
    case "StructuralFormat":
      return () => {
        throw new Error("StructuralFormat requires its resumable executor");
      };
    case "FileRead":
      return entry.detail === "LastError"
        ? () => ({ kind: "string", value: "" })
        : undefined;
    case "Print":
      return printShim(entry.detail);
    case "RcNew":
    case "RcClone":
    case "RcGet":
    case "RcIsUnique":
    case "RcSet":
    case "RcStrongCount":
    case "RcWeakCount":
      return () => {
        throw new Error("shared operation requires its resumable executor");
      };
    case "Vector":
      return ["Contains", "Set", "Clear"].includes(String(entry.detail))
        ? () => {
            throw new Error(
              "vector Contains requires its selected Eq executor",
            );
          }
        : VECTOR_SHIMS[detailName(entry.detail)];
    case "Array":
      return entry.detail === "Set"
        ? () => {
            throw new Error("array mutation requires its resumable executor");
          }
        : undefined;
    case "Map":
    case "Set":
      return [
        "New",
        "Len",
        "Index",
        "Get",
        "GetBorrow",
        "ContainsKey",
        "Contains",
        "Insert",
        "Remove",
        "Clear",
        "Keys",
        "Values",
        "Entries",
        "Elements",
      ].includes(detailName(entry.detail))
        ? () => {
            throw new Error(
              "collection operation requires its resumable executor",
            );
          }
        : undefined;
    case "MathIntrinsic":
      return MATH_SHIMS[detailName(entry.detail)];
    case "FloatMethod":
      return FLOAT_METHOD_SHIMS[detailName(entry.detail)];
    case "IntMethod":
      return intMethodShim(entry.detail);
    case "IntArith":
      return intArithShim(entry.detail);
    default:
      return UNIT_FAMILY_SHIMS[entry.family];
  }
}

export function resolveExternShim(symbol: string): RuntimeShim | undefined {
  return EXTERN_SHIMS[symbol];
}

/// The suspend kinds a sequential package may carry. `NativeIo` is admitted
/// per operation through [`SUPPORTED_NATIVE_IO`] instead.
export const SUPPORTED_SUSPEND_KINDS: ReadonlySet<string> = new Set([
  "GeneratorNext",
  "Yield",
  "Sleep",
  "Ask",
  "Await",
  "Join",
  "Select",
  "StreamSend",
  "StreamNext",
]);

/// The VM serves replay standard input and in-memory sink closure.
export const SUPPORTED_NATIVE_IO: ReadonlySet<string> = new Set([
  "StdinReadLine",
  "SinkFinish",
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
  StderrWrite: (host, args) => {
    host.writeStderr(text(args, 0));
    return UNIT;
  },
  StringToBytes: (_host, args) => ({
    kind: "vector",
    elementType: "u8",
    items: [...new TextEncoder().encode(text(args, 0))].map((byte) =>
      int(BigInt(byte)),
    ),
  }),
  StringToUppercase: (_host, args) => ({
    kind: "string",
    value: text(args, 0).toUpperCase(),
  }),
  StringToLowercase: (_host, args) => ({
    kind: "string",
    value: text(args, 0).toLowerCase(),
  }),
  StreamPairSink: (host, args) => host.pipes!.extract(arg(args, 0), "sink"),
  StreamPairStream: (host, args) => host.pipes!.extract(arg(args, 0), "stream"),
  SinkClone: (host, args) => host.pipes!.cloneSink(arg(args, 0)),
  SinkClose: (host, args) => {
    host.closePipe!(arg(args, 0));
    return UNIT;
  },
  StreamClose: (host, args) => {
    host.closePipe!(arg(args, 0));
    return UNIT;
  },
  StringConcat: (_host, args) => str(`${text(args, 0)}${text(args, 1)}`),
  StringClone: (_host, args) => str(text(args, 0)),
  StringEquals: (_host, args) => bool(text(args, 0) === text(args, 1)),
  StringContains: (_host, args) => bool(text(args, 0).includes(text(args, 1))),
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
  // TextDecoder replaces each maximal invalid subsequence with U+FFFD, as
  // native lossy decoding does.
  BytesDecodeUtf8Lossy: (_host, args) => str(decodeUtf8(arg(args, 0))),
  BytesNew: () => ({ kind: "vector", elementType: "u8", items: [] }),
  BytesIsEmpty: (_host, args) => bool(vec(args, 0).items.length === 0),
  BytesLen: (_host, args) => int(BigInt(vec(args, 0).items.length)),
  BytesContains: (_host, args) => {
    const byte = integer(args, 1);
    return bool(vec(args, 0).items.some((item) => integer([item], 0) === byte));
  },
  BytesIndex: (_host, args) => {
    const items = vec(args, 0).items;
    const at = index(args, 1);
    if (at < 0 || at >= items.length) {
      throw new ShimFault(
        "vector_bounds",
        `bytes index ${at} out of bounds for length ${items.length}`,
      );
    }
    return cloneValue(items[at]!);
  },
  // A native bytes range shares the buffer; a VM value is never aliased, so
  // a copy is observably the same.
  BytesSlice: (_host, args) => {
    const items = vec(args, 0).items;
    const start = index(args, 1);
    const end = index(args, 2);
    checkSliceBounds(start, end, items.length);
    return byteVector(items.slice(start, end));
  },
  BytesSliceFrom: (_host, args) => {
    const items = vec(args, 0).items;
    const start = index(args, 1);
    checkSliceBounds(start, items.length, items.length);
    return byteVector(items.slice(start));
  },
  BytesConcat: (_host, args) =>
    byteVector([...vec(args, 0).items, ...vec(args, 1).items]),
  BytesStartsWith: (_host, args) =>
    bool(runAt(vec(args, 0).items, vec(args, 1).items, 0)),
  BytesEndsWith: (_host, args) => {
    const items = vec(args, 0).items;
    const run = vec(args, 1).items;
    return bool(runAt(items, run, items.length - run.length));
  },
  // A mutating call receives its receiver by `move` and returns it.
  BytesPush: (_host, args) => {
    const target = vec(args, 0);
    target.items.push(cloneValue(arg(args, 1)));
    return target;
  },
  BytesSet: (_host, args) => {
    const target = vec(args, 0);
    const at = index(args, 1);
    if (at < 0 || at >= target.items.length) {
      throw new ShimFault(
        "vector_bounds",
        `bytes index ${at} out of bounds for length ${target.items.length}`,
      );
    }
    target.items[at] = cloneValue(arg(args, 2));
    return target;
  },
  BytesClear: (_host, args) => {
    const target = vec(args, 0);
    target.items.length = 0;
    return target;
  },
  BytesGet: (host, args, shape) => {
    if (!shape) {
      throw new TypeError(
        "BytesGet names no result shape to build its Option against",
      );
    }
    const items = vec(args, 0).items;
    const at = index(args, 1);
    return at >= 0 && at < items.length
      ? host.enumValue(shape, "OptionSome", [cloneValue(items[at]!)])
      : host.enumValue(shape, "OptionNone", []);
  },
  // The receiver arrives by `move` and leaves in the result: alone for an
  // append, paired with the removed byte for a pop.
  BytesAppend: (_host, args) => {
    const target = vec(args, 0);
    target.items.push(...vec(args, 1).items.map(cloneValue));
    return target;
  },
  BytesPop: (host, args, shape) => {
    if (!shape) {
      throw new TypeError(
        "BytesPop names no result shape to build its Option against",
      );
    }
    const target = vec(args, 0);
    const last = target.items.pop();
    return {
      kind: "record",
      typeId: "",
      fields: [
        target,
        last === undefined
          ? host.enumValue(shape, "OptionNone", [])
          : host.enumValue(shape, "OptionSome", [last]),
      ],
    };
  },
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
      ? host.enumValue(shape, "OptionSome", [cloneValue(items[at]!)])
      : host.enumValue(shape, "OptionNone", []);
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
  Slice: (_host, args) => {
    const items = vec(args, 0).items;
    const start = index(args, 1);
    const end = index(args, 2);
    checkSliceBounds(start, end, items.length);
    return {
      kind: "vector",
      elementType: "",
      items: items.slice(start, end).map(cloneValue),
    };
  },
  SliceFrom: (_host, args) => {
    const items = vec(args, 0).items;
    const start = index(args, 1);
    checkSliceBounds(start, items.length, items.length);
    return {
      kind: "vector",
      elementType: "",
      items: items.slice(start).map(cloneValue),
    };
  },
};

function byteVector(items: readonly VmValue[]): VmValue {
  return { kind: "vector", elementType: "u8", items: items.map(cloneValue) };
}

/// Whether `run` occurs in `items` starting at `at`.
function runAt(
  items: readonly VmValue[],
  run: readonly VmValue[],
  at: number,
): boolean {
  return (
    at >= 0 &&
    at + run.length <= items.length &&
    run.every(
      (byte, offset) =>
        integer([items[at + offset]!], 0) === integer([byte], 0),
    )
  );
}

/// A slice outside `0 <= start <= end <= length` faults like native's
/// `IndexOutOfBounds` rather than clamping to the valid range.
function checkSliceBounds(start: number, end: number, length: number): void {
  if (start < 0 || end < start || end > length) {
    throw new ShimFault(
      "vector_bounds",
      `slice ${start}..${end} out of bounds for length ${length}`,
    );
  }
}

/// `std.math`. Native emits the LLVM intrinsic for each of these, so the
/// mapping is to the matching IEEE operation rather than to whatever JS spells
/// similarly: `Round` is half away from zero, `MinF64`/`MaxF64` follow
/// `minnum`/`maxnum` where a NaN operand loses, and `Fma` is fused.
const MATH_SHIMS: Record<string, RuntimeShim | undefined> = {
  Sqrt: float1(Math.sqrt),
  Exp: float1(Math.exp),
  Log: float1(Math.log),
  Sin: float1(Math.sin),
  Cos: float1(Math.cos),
  Tan: float1(Math.tan),
  Asin: float1(Math.asin),
  Acos: float1(Math.acos),
  Atan: float1(Math.atan),
  Sinh: float1(Math.sinh),
  Cosh: float1(Math.cosh),
  Tanh: float1(Math.tanh),
  Exp2: float1((x) => Math.pow(2, x)),
  Log2: float1(Math.log2),
  Log10: float1(Math.log10),
  Log1p: float1(Math.log1p),
  Expm1: float1(Math.expm1),
  Cbrt: float1(Math.cbrt),
  Floor: float1(Math.floor),
  Ceil: float1(Math.ceil),
  Trunc: float1(Math.trunc),
  AbsF64: float1(Math.abs),
  Round: float1(roundHalfAwayFromZero),
  Pow: float2(Math.pow),
  Atan2: float2(Math.atan2),
  Hypot: float2(Math.hypot),
  Copysign: float2(copysign),
  MinF64: float2(minnum),
  MaxF64: float2(maxnum),
  Fma: (_host, args) => num(fma(real(args, 0), real(args, 1), real(args, 2))),
  // `powi` takes an i32 exponent, so the second operand is an integer.
  Powi: (_host, args) => num(Math.pow(real(args, 0), Number(integer(args, 1)))),
  // f64::from_bits: reinterpret the bit pattern, not a numeric conversion.
  FromBits: (_host, args) => num(fromBits(integer(args, 0))),
  AbsI64: (_host, args) => {
    const value = integer(args, 0);
    return int(value < 0n ? -value : value);
  },
  MinI64: (_host, args) => int(bigMin(integer(args, 0), integer(args, 1))),
  MaxI64: (_host, args) => int(bigMax(integer(args, 0), integer(args, 1))),
};

function float1(apply: (x: number) => number): RuntimeShim {
  return (_host, args) => num(apply(real(args, 0)));
}

function float2(apply: (x: number, y: number) => number): RuntimeShim {
  return (_host, args) => num(apply(real(args, 0), real(args, 1)));
}

/// `llvm.round`: ties go away from zero, and the sign of a zero is kept.
/// `Math.round` ties toward positive infinity, so -2.5 would read -2.
function roundHalfAwayFromZero(x: number): number {
  if (!Number.isFinite(x) || Number.isInteger(x)) {
    return x;
  }
  // The fraction is compared directly rather than adding 0.5 first: for
  // 0.49999999999999994 that sum rounds up to exactly 1, which would read 1
  // where native reads 0.
  const truncated = Math.trunc(x);
  const fraction = x - truncated;
  return Math.abs(fraction) >= 0.5 ? truncated + Math.sign(x) : truncated;
}

function copysign(magnitude: number, sign: number): number {
  const negative = sign < 0 || Object.is(sign, -0);
  return negative ? -Math.abs(magnitude) : Math.abs(magnitude);
}

/// `llvm.minnum`: a NaN operand loses, and operands that compare equal - the
/// two zeros - resolve to the first, which is what native returns.
function minnum(left: number, right: number): number {
  if (Number.isNaN(left)) return right;
  if (Number.isNaN(right)) return left;
  return right < left ? right : left;
}

function maxnum(left: number, right: number): number {
  if (Number.isNaN(left)) return right;
  if (Number.isNaN(right)) return left;
  return right > left ? right : left;
}

/// Fused multiply-add: `a * b` is not rounded before `c` is added. The product
/// is carried exactly as an unevaluated head and tail pair, so the result
/// rounds once, as the hardware instruction native emits does.
function fma(a: number, b: number, c: number): number {
  const product = a * b;
  if (!Number.isFinite(product) || !Number.isFinite(c) || product === 0) {
    return product + c;
  }
  // Dekker's exact product: head + tail is a * b with no rounding lost.
  const SPLIT = 134217729; // 2^27 + 1
  const splitHigh = (x: number) => {
    const t = SPLIT * x;
    return t - (t - x);
  };
  const aHigh = splitHigh(a);
  const aLow = a - aHigh;
  const bHigh = splitHigh(b);
  const bLow = b - bHigh;
  const tail =
    aLow * bLow - (product - aHigh * bHigh - aLow * bHigh - aHigh * bLow);

  // Exact sum of the product head with the addend, then one rounding.
  const sum = product + c;
  const bVirtual = sum - product;
  const sumTail = product - (sum - bVirtual) + (c - bVirtual);
  return sum + (sumTail + tail);
}

/// `f64` classification and bit methods. Native reads the IEEE bits
/// directly, so `IsSignNegative` tests the sign bit (true for `-0.0` and a
/// negative NaN) rather than comparing against zero.
const FLOAT_METHOD_SHIMS: Record<string, RuntimeShim | undefined> = {
  ToBits: (_host, args) => int(toBits(real(args, 0))),
  IsNan: (_host, args) => bool(Number.isNaN(real(args, 0))),
  IsFinite: (_host, args) => bool(Number.isFinite(real(args, 0))),
  IsInfinite: (_host, args) => {
    const value = real(args, 0);
    return bool(value === Infinity || value === -Infinity);
  },
  IsSignNegative: (_host, args) => bool(toBits(real(args, 0)) >> 63n === 1n),
};

/// The receiver width an `IntMethod`/`IntArith` detail names. The VM is a
/// 64-bit target, so the pointer-sized widths are 64 bits wide.
const INT_WIDTHS: Record<
  string,
  { bits: number; signed: boolean } | undefined
> = {
  I8: { bits: 8, signed: true },
  I16: { bits: 16, signed: true },
  I32: { bits: 32, signed: true },
  I64: { bits: 64, signed: true },
  Isize: { bits: 64, signed: true },
  U8: { bits: 8, signed: false },
  U16: { bits: 16, signed: false },
  U32: { bits: 32, signed: false },
  U64: { bits: 64, signed: false },
  Usize: { bits: 64, signed: false },
};

type IntWidth = { bits: number; signed: boolean };

/// `[op, width]`, as `RuntimeCallFamily` serializes a two-field variant.
function intDetail(detail: unknown): [string, IntWidth] | undefined {
  if (!Array.isArray(detail) || detail.length !== 2) {
    return undefined;
  }
  const [op, width] = detail;
  const shape = typeof width === "string" ? INT_WIDTHS[width] : undefined;
  return typeof op === "string" && shape ? [op, shape] : undefined;
}

function wrapTo(width: IntWidth, value: bigint): bigint {
  return width.signed
    ? BigInt.asIntN(width.bits, value)
    : BigInt.asUintN(width.bits, value);
}

function intBounds(width: IntWidth): [bigint, bigint] {
  const bits = BigInt(width.bits);
  return width.signed
    ? [-(1n << (bits - 1n)), (1n << (bits - 1n)) - 1n]
    : [0n, (1n << bits) - 1n];
}

function popcount(value: bigint): bigint {
  let count = 0n;
  for (let rest = value; rest !== 0n; rest >>= 1n) {
    count += rest & 1n;
  }
  return count;
}

/// Integer bit methods. Native emits `ctpop`/`ctlz`/`cttz`/`bswap`/
/// `bitreverse`/`fshl`/`fshr` at the receiver's width, so every operation
/// works on the receiver's unsigned bit pattern and the rotate amount is taken
/// modulo the width.
function intMethodShim(detail: unknown): RuntimeShim | undefined {
  const parsed = intDetail(detail);
  if (!parsed) {
    return undefined;
  }
  const [op, width] = parsed;
  const bits = BigInt(width.bits);
  const unsigned = (args: VmValue[]) =>
    BigInt.asUintN(width.bits, integer(args, 0));
  const result = (pattern: bigint) => int(wrapTo(width, pattern));
  const rotate = (args: VmValue[], left: boolean) => {
    const pattern = unsigned(args);
    const amount = BigInt.asUintN(32, integer(args, 1)) % bits;
    const shift = left ? amount : (bits - amount) % bits;
    return result((pattern << shift) | (pattern >> ((bits - shift) % bits)));
  };
  switch (op) {
    case "CountOnes":
      return (_host, args) => int(popcount(unsigned(args)));
    case "CountZeros":
      return (_host, args) => int(bits - popcount(unsigned(args)));
    case "LeadingZeros":
      return (_host, args) => {
        const pattern = unsigned(args);
        return int(
          pattern === 0n ? bits : bits - BigInt(pattern.toString(2).length),
        );
      };
    case "TrailingZeros":
      return (_host, args) => {
        let pattern = unsigned(args);
        if (pattern === 0n) {
          return int(bits);
        }
        let count = 0n;
        while ((pattern & 1n) === 0n) {
          pattern >>= 1n;
          count += 1n;
        }
        return int(count);
      };
    case "SwapBytes":
      return (_host, args) => {
        let pattern = unsigned(args);
        let swapped = 0n;
        for (let byte = 0; byte < width.bits / 8; byte += 1) {
          swapped = (swapped << 8n) | (pattern & 0xffn);
          pattern >>= 8n;
        }
        return result(swapped);
      };
    case "ReverseBits":
      return (_host, args) => {
        let pattern = unsigned(args);
        let reversed = 0n;
        for (let bit = 0; bit < width.bits; bit += 1) {
          reversed = (reversed << 1n) | (pattern & 1n);
          pattern >>= 1n;
        }
        return result(reversed);
      };
    case "RotateLeft":
      return (_host, args) => rotate(args, true);
    case "RotateRight":
      return (_host, args) => rotate(args, false);
    default:
      return undefined;
  }
}

/// Non-trapping integer arithmetic at the receiver's width: wrapping keeps
/// the low bits, saturating clamps to the width's bounds and checked answers
/// `None` where native's overflow flag is set.
function intArithShim(detail: unknown): RuntimeShim | undefined {
  const parsed = intDetail(detail);
  if (!parsed) {
    return undefined;
  }
  const [kind, width] = parsed;
  const [min, max] = intBounds(width);
  const operations: Record<
    string,
    ((x: bigint, y: bigint) => bigint) | undefined
  > = {
    Add: (x, y) => x + y,
    Sub: (x, y) => x - y,
    Mul: (x, y) => x * y,
  };
  const mode = ["Wrapping", "Saturating", "Checked"].find((prefix) =>
    kind.startsWith(prefix),
  );
  const apply = mode ? operations[kind.slice(mode.length)] : undefined;
  if (!mode || !apply) {
    return undefined;
  }
  const exact = (args: VmValue[]) =>
    apply(wrapTo(width, integer(args, 0)), wrapTo(width, integer(args, 1)));
  switch (mode) {
    case "Wrapping":
      return (_host, args) => int(wrapTo(width, exact(args)));
    case "Saturating":
      return (_host, args) => {
        const value = exact(args);
        return int(value < min ? min : value > max ? max : value);
      };
    default:
      return (host, args, shape) => {
        if (!shape) {
          throw new TypeError(
            "checked arithmetic names no result shape to build its Option against",
          );
        }
        const value = exact(args);
        return value < min || value > max
          ? host.enumValue(shape, "OptionNone", [])
          : host.enumValue(shape, "OptionSome", [int(value)]);
      };
  }
}

function toBits(value: number): bigint {
  const view = new DataView(new ArrayBuffer(8));
  view.setFloat64(0, value);
  return view.getBigUint64(0);
}

function fromBits(bits: bigint): number {
  const view = new DataView(new ArrayBuffer(8));
  view.setBigUint64(0, BigInt.asUintN(64, bits));
  return view.getFloat64(0);
}

function bigMin(left: bigint, right: bigint): bigint {
  return right < left ? right : left;
}

function bigMax(left: bigint, right: bigint): bigint {
  return right > left ? right : left;
}

// ── externs ─────────────────────────────────────────────────────────────────

const EXTERN_SHIMS: Record<string, RuntimeShim | undefined> = {
  hew_msg_envelope_release: (host, args) => {
    host.releaseValue!(arg(args, 0));
    return UNIT;
  },
  hew_stream_channel: (host, args) => host.newPipe!(Number(integer(args, 0))),
  hew_stream_pair_is_valid: () => ({ kind: "bool", value: true }),
  hew_stream_pair_free: (host, args) => {
    host.pipes!.freePair(arg(args, 0));
    return UNIT;
  },
  hew_stream_last_error: () => ({ kind: "string", value: "" }),

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
  hew_regex_capture_index_one: (_host, args) => {
    const value = arg(args, 0);
    const capture =
      value.kind === "regex"
        ? value.regex.exec(text(args, 1))?.[index(args, 2)]
        : undefined;
    return {
      kind: "vector",
      elementType: "string",
      items: capture === undefined ? [] : [str(capture)],
    };
  },
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

function real(args: VmValue[], at: number): number {
  const value = arg(args, at);
  return value.kind === "f64" ? value.value : Number(integer(args, at));
}

function num(value: number): VmValue {
  return { kind: "f64", value };
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
