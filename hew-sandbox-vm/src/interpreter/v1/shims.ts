import {
  DIRECTORY_OPERATIONS,
  type DirectoryOperation,
} from "./browser-directory.js";
import { executeCalendar } from "./calendar.js";
import { executeEncoding, type EncodingFormat } from "./encoding-values.js";
import { charProperty, charCase } from "../unicode.js";
import type { Pipes } from "./pipes.js";
/// Runtime implementations and their host execution registration. Admission and
/// execution consume the same ordinary, resumable or refused disposition.

import {
  UNIT,
  cloneValue,
  compareUtf8,
  stringValue,
  renderStdout,
  type VmValue,
} from "../values.js";
import { Mt19937, seededMt } from "./mt19937.js";
import type {
  AggregateShape,
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

function checkedWrite(host: ShimHost, value: string, stderr: boolean): VmValue {
  try {
    if (value.length !== 0) {
      if (stderr) host.writeStderr(value);
      else host.writeStdout(value);
    }
    host.setFileError(null);
    return int(0n);
  } catch (error) {
    host.setFileError(error instanceof Error ? error.message : String(error));
    return int(-1n);
  }
}

/// What a shim may reach for. The executor owns faults and traces; a shim
/// never touches the frame stack or the value environment.
export interface ShimHost {
  /// A snapshot of the supplied process arguments, including argv[0].
  readonly argv: readonly string[];
  unixTimeMillis(): number;
  monotonicTimeNanos(): bigint;
  fileError(
    operation: "HasError" | "LastErrorKind" | "LastErrno" | "LastError",
  ): VmValue;
  setFileError(message: string | null): void;
  calendarBridge?: (request: string) => string;
  codecBridge?: (request: string) => string;
  encodingError?(format: EncodingFormat, message?: string): string;
  pipes?: Pipes;
  semaphores?: import("./semaphores.js").Semaphores;
  newPipe?(capacity: number): VmValue;
  generatorStream?(generator: VmValue): VmValue;
  encoderSink?(sink: VmValue, encoder: VmValue): VmValue;
  cloneSink?(sink: VmValue): VmValue;
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
/// `aggregate` is the checked record descriptor for an aggregate result.
export type RuntimeShim = (
  host: ShimHost,
  args: VmValue[],
  shape: VariantShape | null,
  aggregate: AggregateShape | null,
) => VmValue;

export type HostBridges = Pick<
  import("../types.js").RunOptions,
  "codecBridge" | "calendarBridge" | "directoryRoots"
>;
type BridgeRequirement = "codecBridge" | "calendarBridge";
export type DirectoryRegistration = {
  kind: "directory";
  operation: DirectoryOperation;
};
export type Refused = {
  kind: "refused";
  category: "native_only" | "not_implemented";
  capability: string;
  description?: string;
  message?: string;
};
type Ordinary = {
  kind: "ordinary";
  shim: RuntimeShim;
  bridge?: BridgeRequirement;
};
export type RuntimeContinuation =
  | {
      handler:
        | "actor_caller"
        | "context_actor_id"
        | "context_supervisor"
        | "context_trace_span"
        | "actor_refusal_log"
        | "structural_format"
        | "utf8_decode"
        | "close";
    }
  | {
      handler: "supervisor_pool";
      operation: Extract<
        RuntimeFamilyEntry,
        { family: "SupervisorPool" }
      >["detail"];
    }
  | ({ handler: "collection" } & (
      | { family: "Vector"; operation: "Contains" | "Set" | "Clear" }
      | {
          family: "Map";
          operation: Extract<RuntimeFamilyEntry, { family: "Map" }>["detail"];
        }
      | {
          family: "Set";
          operation: Extract<RuntimeFamilyEntry, { family: "Set" }>["detail"];
        }
    ))
  | {
      handler: "shared";
      operation: Extract<
        RuntimeFamilyEntry["family"],
        | "RcNew"
        | "RcClone"
        | "RcDowngrade"
        | "WeakCloneRc"
        | "WeakUpgradeRc"
        | "RcGet"
        | "RcIsUnique"
        | "RcSet"
        | "RcStrongCount"
        | "RcWeakCount"
      >;
    };
export type RuntimeRegistration =
  | Ordinary
  | Refused
  | ({ kind: "resumable" } & RuntimeContinuation);

function ordinary(shim: RuntimeShim): Ordinary {
  return { kind: "ordinary", shim };
}
function refused(capability: string, native = false): Refused {
  return {
    kind: "refused",
    category: native ? "native_only" : "not_implemented",
    capability,
  };
}
function resumable(continuation: RuntimeContinuation): RuntimeRegistration {
  return { kind: "resumable", ...continuation };
}

/// A registered algorithm may still require a host-supplied Wasm bridge.
export function registrationRefusal(
  registration:
    | RuntimeRegistration
    | DirectoryRegistration
    | { kind: "resumable"; bridge: BridgeRequirement },
  bridges: HostBridges,
): Refused | null {
  if (registration.kind === "refused") return registration;
  if (
    registration.kind === "directory" &&
    registration.operation.action === "root" &&
    !bridges.directoryRoots?.[registration.operation.access]
  )
    return {
      ...refused(`directoryRoots.${registration.operation.access}`),
      message: `This operation requires a host-supplied OPFS ${registration.operation.access} directory handle.`,
    };
  return "bridge" in registration &&
    registration.bridge &&
    typeof bridges[registration.bridge] !== "function"
    ? refused(registration.bridge)
    : null;
}

export const CODEC_REGISTRATION = {
  kind: "resumable",
  bridge: "codecBridge",
} as const;

/// Each typed runtime identity has exactly one host disposition. An added Rust
/// family or nested operation must acquire a disposition before TypeScript builds.
export function resolveRuntime(entry: RuntimeFamilyEntry): RuntimeRegistration {
  switch (entry.family) {
    case "SupervisorPool":
      return resumable({ handler: "supervisor_pool", operation: entry.detail });
    case "Print":
      return ordinary(printShim(entry.detail));
    case "FileRead":
      switch (entry.detail) {
        case "HasError":
        case "LastErrorKind":
        case "LastErrno":
        case "LastError": {
          const operation = entry.detail;
          return ordinary((host) => host.fileError(operation));
        }
        default:
          return refused(`FileRead::${entry.detail}`, true);
      }
    case "AsyncIo":
    case "Tcp":
      return refused(`${entry.family}::${entry.detail}`, true);
    case "Semaphore":
    case "Service":
    case "VecContainsScalar":
    case "VecGet":
    case "VecSliceRange":
      return refused(`${entry.family}::${entry.detail}`);
    case "Encoding":
      return (
        entry.detail.op === "GetDatetime"
          ? entry.detail.format === "Toml"
          : entry.detail.format !== "Toml" ||
            !["GetU64", "IntStatus", "FromU64", "FromNull"].includes(
              entry.detail.op,
            )
      )
        ? {
            kind: "ordinary",
            shim: (host, args) =>
              executeEncoding(host, entry.detail.format, entry.detail.op, args),
            ...(entry.detail.op === "Parse" || entry.detail.op === "Stringify"
              ? { bridge: "codecBridge" as const }
              : {}),
          }
        : refused(`Encoding::${entry.detail.format}`);
    case "VecScalar":
      return refused(entry.family);
    case "Vector":
      switch (entry.detail) {
        case "Contains":
        case "Set":
        case "Clear":
          return resumable({
            handler: "collection",
            family: "Vector",
            operation: entry.detail,
          });
        case "GetBorrow":
        case "TakeFirst":
        case "Append":
          return refused(`Vector::${entry.detail}`);
        default:
          return ordinary(VECTOR_SHIMS[entry.detail]);
      }
    case "Array":
      switch (entry.detail) {
        case "Set":
          return resumable({
            handler: "collection",
            family: "Vector",
            operation: entry.detail,
          });
        case "Len":
        case "Index":
        case "IndexBorrow":
          return ordinary(VECTOR_SHIMS[entry.detail]);
        default: {
          const exhaustive: never = entry;
          throw new Error(`unknown array operation ${String(exhaustive)}`);
        }
      }
    case "Map":
      return resumable({
        handler: "collection",
        family: "Map",
        operation: entry.detail,
      });
    case "Set":
      return resumable({
        handler: "collection",
        family: "Set",
        operation: entry.detail,
      });
    case "MathIntrinsic":
      return ordinary(MATH_SHIMS[entry.detail]);
    case "FloatMethod":
      return ordinary(FLOAT_METHOD_SHIMS[entry.detail]);
    case "IntMethod":
      return ordinary(intMethodShim(entry.detail));
    case "IntArith":
      return ordinary(intArithShim(entry.detail));
    default: {
      const registration = UNIT_FAMILIES[entry.family];
      return typeof registration === "function"
        ? ordinary(registration)
        : registration;
    }
  }
}

export function resolveExtern(entry: {
  symbol: string;
  native_capability?: string | null;
}): Ordinary | Refused | DirectoryRegistration {
  if (Object.hasOwn(DIRECTORY_OPERATIONS, entry.symbol))
    return {
      kind: "directory",
      operation:
        DIRECTORY_OPERATIONS[entry.symbol as keyof typeof DIRECTORY_OPERATIONS],
    };
  const registration = Object.hasOwn(EXTERN_SHIMS, entry.symbol)
    ? EXTERN_SHIMS[entry.symbol]
    : undefined;
  if (typeof registration === "function") return ordinary(registration);
  return (
    registration ?? {
      ...refused(entry.symbol, entry.native_capability != null),
      description: entry.native_capability ?? undefined,
    }
  );
}

type SuspendTerm = Extract<import("./package.js").TermV1, { op: "suspend" }>;
const SUSPENSION_ROUTES = {
  GeneratorNext: "GeneratorNext",
  Yield: "Yield",
  Sleep: "Sleep",
  Safepoint: "Safepoint",
  Ask: "Ask",
  Await: "Await",
  Join: "Join",
  Select: "Select",
  StreamSend: "StreamSend",
  StreamNext: "StreamNext",
  NativeIo: "NativeIo",
  Read: refused("Read", true),
  Accept: refused("Accept", true),
  SleepUntil: refused("SleepUntil"),
  ServiceAcquire: refused("ServiceAcquire"),
  SemaphoreAcquire: "SemaphoreAcquire",
  ActorSend: refused("ActorSend"),
  RestartWait: refused("RestartWait"),
  CallClosure: refused("CallClosure"),
  Timeout: refused("Timeout"),
  ScopeDeadline: refused("ScopeDeadline"),
  Offload: refused("Offload"),
} as const satisfies { [K in SuspendTerm["kind"]]: K | Refused };

type ExecutableSuspension = Extract<
  SuspendTerm,
  {
    kind: Extract<
      (typeof SUSPENSION_ROUTES)[keyof typeof SUSPENSION_ROUTES],
      string
    >;
  }
>;

export function suspensionRefusal(kind: SuspendTerm["kind"]): Refused | null {
  const route = SUSPENSION_ROUTES[kind];
  return typeof route === "string" ? null : route;
}

function executableSuspension(term: SuspendTerm): term is ExecutableSuspension {
  return suspensionRefusal(term.kind) === null;
}

type NativeIoTerm = Extract<SuspendTerm, { kind: "NativeIo" }>;
const NATIVE_IO_ROUTES = {
  StdinReadLine: "StdinReadLine",
  SinkFinish: "SinkFinish",
  TcpRead: refused("NativeIo::TcpRead", true),
  TcpWrite: refused("NativeIo::TcpWrite", true),
  TcpAccept: refused("NativeIo::TcpAccept", true),
  TcpConnect: refused("NativeIo::TcpConnect", true),
  TcpConnectTimeout: refused("NativeIo::TcpConnectTimeout", true),
  TlsConnect: refused("NativeIo::TlsConnect", true),
  TlsRead: refused("NativeIo::TlsRead", true),
  TlsWrite: refused("NativeIo::TlsWrite", true),
  WebSocketAccept: refused("NativeIo::WebSocketAccept", true),
  WebSocketSendText: refused("NativeIo::WebSocketSendText", true),
  WebSocketSendBinary: refused("NativeIo::WebSocketSendBinary", true),
  FileRead: "FileRead",
  FileWrite: "FileWrite",
} as const satisfies {
  [K in NativeIoTerm["detail"]["operation"]]: K | Refused;
};

type ExecutableNativeIo = NativeIoTerm & {
  detail: {
    operation: Extract<
      (typeof NATIVE_IO_ROUTES)[keyof typeof NATIVE_IO_ROUTES],
      string
    >;
  };
};
function nativeIoRefusal(
  operation: NativeIoTerm["detail"]["operation"],
): Refused | null {
  const route = NATIVE_IO_ROUTES[operation];
  return typeof route === "string" ? null : route;
}
function executableNativeIo(term: NativeIoTerm): term is ExecutableNativeIo {
  return nativeIoRefusal(term.detail.operation) === null;
}

/// The executor handles exactly the resumable identities registered above.
export function resolveSuspension(term: SuspendTerm) {
  if (!executableSuspension(term)) return suspensionRefusal(term.kind)!;
  if (term.kind === "NativeIo") {
    if (!executableNativeIo(term))
      return nativeIoRefusal(term.detail.operation)!;
    return { kind: "resumable" as const, term };
  }
  return { kind: "resumable" as const, term };
}

// ── families ────────────────────────────────────────────────────────────────

/// `Print { kind, newline }`. The rendering follows the value's own
/// representation: casts normalize an integer into its target range, so the
/// declared `kind` never changes how a value prints.
function printShim(
  detail: Extract<RuntimeFamilyEntry, { family: "Print" }>["detail"],
): RuntimeShim {
  const newline = detail.newline;
  return (host, args) => {
    host.writeStdout(
      newline ? `${renderStdout(arg(args, 0))}\n` : renderStdout(arg(args, 0)),
    );
    return UNIT;
  };
}

const UNIT_FAMILIES = {
  TimestampNow: (host, _args, _variant, aggregate) => {
    const millis = host.unixTimeMillis();
    if (!Number.isFinite(millis))
      throw new ShimFault("integer_overflow", "host wall clock is not finite");
    const seconds = Math.floor(millis / 1000);
    let nanos = Math.round((millis - seconds * 1000) * 1_000_000);
    let whole = BigInt(seconds);
    if (nanos === 1_000_000_000) {
      whole += 1n;
      nanos = 0;
    }
    if (
      whole < -(1n << 63n) ||
      whole >= 1n << 63n ||
      nanos < 0 ||
      nanos >= 1_000_000_000
    )
      throw new ShimFault(
        "integer_overflow",
        "host wall clock exceeds timestamp range",
      );
    if (!aggregate || aggregate.builtin !== "Timestamp")
      throw new TypeError("timestamp clock lacks its aggregate descriptor");
    return {
      kind: "record",
      typeId: aggregate.name,
      shape: aggregate.id,
      fields: [int(whole), int(BigInt(nanos))],
    };
  },
  StderrWrite: (host, args) => {
    host.writeStderr(text(args, 0));
    return UNIT;
  },
  StringToBytes: (_host, args) => ({
    kind: "vector",
    elementType: "u8",
    items: [...indexed(args, 0).encoded()].map((byte) => int(BigInt(byte))),
  }),
  StringToUppercase: (_host, args) => str(text(args, 0).toUpperCase()),
  StringToLowercase: (_host, args) => str(text(args, 0).toLowerCase()),
  StreamPairSink: (host, args) => host.pipes!.extract(arg(args, 0), "sink"),
  StreamPairStream: (host, args) => host.pipes!.extract(arg(args, 0), "stream"),
  StreamFromGenerator: (host, args) => host.generatorStream!(arg(args, 0)),
  SinkFromEncoder: (host, args) =>
    host.encoderSink!(arg(args, 0), arg(args, 1)),
  SinkClone: (host, args) => host.cloneSink!(arg(args, 0)),
  SinkClose: resumable({ handler: "close" }),
  StreamClose: resumable({ handler: "close" }),
  StringConcat: (_host, args) => str(`${text(args, 0)}${text(args, 1)}`),
  StringClone: (_host, args) => cloneValue(arg(args, 0)),
  StringEquals: (_host, args) => bool(text(args, 0) === text(args, 1)),
  StringCompare: (_host, args) =>
    int(BigInt(compareUtf8(text(args, 0), text(args, 1)))),
  StringStartsWith: (_host, args) =>
    bool(text(args, 0).startsWith(text(args, 1))),
  StringEndsWith: (_host, args) => bool(text(args, 0).endsWith(text(args, 1))),
  StringContains: (_host, args) =>
    bool(indexed(args, 0).find(indexed(args, 1)) !== undefined),
  StringTrim: (_host, args) =>
    str(text(args, 0).replace(/^\p{White_Space}+|\p{White_Space}+$/gu, "")),
  StringLen: (_host, args) => int(BigInt(indexed(args, 0).scalarLength)),
  StringByteLen: (_host, args) => int(BigInt(indexed(args, 0).byteLength)),
  StringIndex: (_host, args) => {
    const value = indexed(args, 0);
    const at = index(args, 1);
    const scalar = value.at(at);
    if (scalar === undefined)
      throw new ShimFault(
        "vector_bounds",
        `string index ${at} out of bounds for length ${value.scalarLength}`,
      );
    return str(scalar);
  },
  StringGet: (host, args, shape) => {
    if (!shape) throw new TypeError("string get requires its Option shape");
    const scalar = indexed(args, 0).at(index(args, 1));
    return scalar === undefined
      ? host.enumValue(shape, "OptionNone", [])
      : host.enumValue(shape, "OptionSome", [str(scalar)]);
  },
  StringFind: (host, args, shape) => {
    if (!shape) throw new TypeError("string find requires its Option shape");
    const found = indexed(args, 0).find(indexed(args, 1));
    return found === undefined
      ? host.enumValue(shape, "OptionNone", [])
      : host.enumValue(shape, "OptionSome", [int(BigInt(found))]);
  },
  StringFindFrom: (host, args, shape) => {
    if (!shape)
      throw new TypeError("string find_from requires its Option shape");
    const found = indexed(args, 0).find(indexed(args, 1), index(args, 2));
    return found === undefined
      ? host.enumValue(shape, "OptionNone", [])
      : host.enumValue(shape, "OptionSome", [int(BigInt(found))]);
  },
  StringAppend: (_host, args) => indexed(args, 0).append(indexed(args, 1)),
  StringPush: (_host, args) => indexed(args, 0).append(indexed(args, 1)),
  StringClear: (_host, args) => {
    indexed(args, 0).release();
    return str("");
  },
  StringIsEmpty: (_host, args) => bool(indexed(args, 0).scalarLength === 0),
  StringSplit: (_host, args) => {
    const value = indexed(args, 0).value;
    const separator = indexed(args, 1).value;
    const parts = separator === "" ? Array.from(value) : value.split(separator);
    return { kind: "vector", elementType: "string", items: parts.map(str) };
  },
  StringReplace: (_host, args) =>
    str(text(args, 0).split(text(args, 1)).join(text(args, 2))),
  StringRepeat: (_host, args) =>
    str(text(args, 0).repeat(Math.max(0, index(args, 1)))),
  StringGetRange: (host, args, shape) => {
    if (!shape)
      throw new TypeError("string range get requires its Option shape");
    const value = stringRange(args);
    return value === undefined
      ? host.enumValue(shape, "OptionNone", [])
      : host.enumValue(shape, "OptionSome", [value]);
  },
  StringSliceRange: (_host, args) => {
    const value = stringRange(args);
    if (value === undefined)
      throw new ShimFault("vector_bounds", "string range out of bounds");
    return value;
  },
  StringSliceCodepoints: (_host, args) => {
    const value = indexed(args, 0);
    const start = index(args, 1);
    const end = index(args, 2);
    const sliced = value.slice(start, end);
    if (!sliced)
      throw new ShimFault(
        "vector_bounds",
        `string slice ${start}..${end} out of bounds for length ${value.scalarLength}`,
      );
    return sliced;
  },
  StringSliceCodepointsFrom: (_host, args) => {
    const value = indexed(args, 0);
    const start = index(args, 1);
    const sliced = value.slice(start, value.scalarLength);
    if (!sliced)
      throw new ShimFault(
        "vector_bounds",
        `string slice ${start}.. out of bounds for length ${value.scalarLength}`,
      );
    return sliced;
  },
  I64ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  U64ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  U32ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  I32ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  U8ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  BoolToString: (_host, args) => str(renderStdout(arg(args, 0))),
  F64ToString: (_host, args) => str(renderStdout(arg(args, 0))),
  CharIsDigit: (_host, args) => bool(charProperty(text(args, 0), 0)),
  CharIsLetter: (_host, args) => bool(charProperty(text(args, 0), 1)),
  CharIsAlphanumeric: (_host, args) => bool(charProperty(text(args, 0), 2)),
  CharIsWhitespace: (_host, args) => bool(charProperty(text(args, 0), 3)),
  CharIsUpper: (_host, args) => bool(charProperty(text(args, 0), 4)),
  CharIsLower: (_host, args) => bool(charProperty(text(args, 0), 5)),
  CharIsPunctuation: (_host, args) => bool(charProperty(text(args, 0), 6)),
  CharIsControl: (_host, args) => bool(charProperty(text(args, 0), 7)),
  CharIsAscii: (_host, args) => bool(charProperty(text(args, 0), 8)),
  CharLenUtf8: (_host, args) => int(BigInt(indexed(args, 0).byteLength)),
  CharToUpper: (_host, args) => str(charCase(text(args, 0), "upper")),
  CharToLower: (_host, args) => str(charCase(text(args, 0), "lower")),
  CharToTitle: (_host, args) => str(charCase(text(args, 0), "title")),
  CharToDigit: (host, args, shape) => {
    if (!shape) throw new TypeError("char to_digit requires its Option shape");
    const radix = index(args, 1);
    const scalar = text(args, 0).codePointAt(0)!;
    const digit =
      scalar >= 48 && scalar <= 57
        ? scalar - 48
        : scalar >= 65 && scalar <= 90
          ? scalar - 65 + 10
          : scalar >= 97 && scalar <= 122
            ? scalar - 97 + 10
            : -1;
    return radix < 2 || radix > 36 || digit < 0 || digit >= radix
      ? host.enumValue(shape, "OptionNone", [])
      : host.enumValue(shape, "OptionSome", [int(BigInt(digit))]);
  },
  CharFromI64: checkedChar,
  CharFromU64: checkedChar,
  CharToString: (_host, args) => str(renderStdout(arg(args, 0))),
  BytesDecodeUtf8: resumable({ handler: "utf8_decode" }),
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

  ActorDemonitor: refused("ActorDemonitor"),
  ActorLink: refused("ActorLink"),
  ActorMonitor: refused("ActorMonitor"),
  ActorCaller: resumable({ handler: "actor_caller" }),
  ContextActorId: resumable({ handler: "context_actor_id" }),
  ContextSupervisor: resumable({ handler: "context_supervisor" }),
  ContextTraceSpan: resumable({ handler: "context_trace_span" }),
  ActorRefusalLog: resumable({ handler: "actor_refusal_log" }),
  ActorUnlink: refused("ActorUnlink"),
  AutoMutexAlloc: refused("AutoMutexAlloc"),
  AutoMutexFree: refused("AutoMutexFree"),
  AutoMutexLock: refused("AutoMutexLock"),
  AutoMutexUnlock: refused("AutoMutexUnlock"),
  DequePopFront: refused("DequePopFront"),
  DequePopBack: refused("DequePopBack"),
  CancelTokenIsRequested: refused("CancelTokenIsRequested"),
  CancelTokenRelease: refused("CancelTokenRelease"),
  CancelTokenRetain: refused("CancelTokenRetain"),
  ActorRequestRelease: refused("ActorRequestRelease"),
  ActorCallFree: refused("ActorCallFree"),
  ActorRequestTake: refused("ActorRequestTake"),
  DurationAbs: (_host, args) => {
    const nanos = integer(args, 0);
    return int(
      nanos === -(1n << 63n) ? (1n << 63n) - 1n : nanos < 0n ? -nanos : nanos,
    );
  },
  DurationHours: (_host, args) => int(integer(args, 0) / 3_600_000_000_000n),
  DurationIsZero: (_host, args) => bool(integer(args, 0) === 0n),
  DurationMicros: (_host, args) => int(integer(args, 0) / 1_000n),
  DurationMillis: (_host, args) => int(integer(args, 0) / 1_000_000n),
  DurationMins: (_host, args) => int(integer(args, 0) / 60_000_000_000n),
  DurationNanos: (_host, args) => int(integer(args, 0)),
  DurationSecs: (_host, args) => int(integer(args, 0) / 1_000_000_000n),
  DynBoxAlloc: refused("DynBoxAlloc"),
  DynBoxFree: refused("DynBoxFree"),
  HashMapContainsKeyLayout: refused("HashMapContainsKeyLayout"),
  HashMapClearLayout: refused("HashMapClearLayout"),
  HashMapCloneLayout: refused("HashMapCloneLayout"),
  HashMapEntriesLayout: refused("HashMapEntriesLayout"),
  HashMapFreeLayout: refused("HashMapFreeLayout"),
  HashMapGetLayout: refused("HashMapGetLayout"),
  HashMapInsertLayout: refused("HashMapInsertLayout"),
  HashMapKeysLayout: refused("HashMapKeysLayout"),
  HashMapLenLayout: refused("HashMapLenLayout"),
  HashMapNew: refused("HashMapNew"),
  HashMapNewWithLayout: refused("HashMapNewWithLayout"),
  HashMapRemoveLayout: refused("HashMapRemoveLayout"),
  HashMapValuesLayout: refused("HashMapValuesLayout"),
  HashSetContainsLayout: refused("HashSetContainsLayout"),
  HashSetClearLayout: refused("HashSetClearLayout"),
  HashSetCloneLayout: refused("HashSetCloneLayout"),
  HashSetFreeLayout: refused("HashSetFreeLayout"),
  HashSetInsertLayout: refused("HashSetInsertLayout"),
  HashSetIsEmptyLayout: refused("HashSetIsEmptyLayout"),
  HashSetLenLayout: refused("HashSetLenLayout"),
  HashSetNew: refused("HashSetNew"),
  HashSetNewWithLayout: refused("HashSetNewWithLayout"),
  HashSetRemoveLayout: refused("HashSetRemoveLayout"),
  HashSetToVecLayout: refused("HashSetToVecLayout"),
  InstantDurationSince: (_host, args) =>
    int(saturatingI64(integer(args, 0) - integer(args, 1))),
  InstantElapsed: (host, args) =>
    int(
      saturatingI64(
        saturatingI64(host.monotonicTimeNanos()) - integer(args, 0),
      ),
    ),
  InstantNow: (host) => int(saturatingI64(host.monotonicTimeNanos())),
  NodeConnect: refused("NodeConnect"),
  NodeId: refused("NodeId"),
  NodeIdDisplay: refused("NodeIdDisplay"),
  LocationNodeId: refused("LocationNodeId"),
  LocationIncarnation: refused("LocationIncarnation"),
  LocationDisplay: refused("LocationDisplay"),
  NodeIdentityKey: refused("NodeIdentityKey"),
  NodeLoadKeys: refused("NodeLoadKeys"),
  NodeLookup: refused("NodeLookup"),
  NodeRegister: refused("NodeRegister"),
  NodeSetTransport: refused("NodeSetTransport"),
  NodeShutdown: refused("NodeShutdown"),
  NodeStart: refused("NodeStart"),
  MetricCounterRegister: refused("MetricCounterRegister"),
  MetricCounterInc: refused("MetricCounterInc"),
  MetricCounterAdd: refused("MetricCounterAdd"),
  MetricGaugeRegister: refused("MetricGaugeRegister"),
  MetricGaugeSet: refused("MetricGaugeSet"),
  MetricGaugeInc: refused("MetricGaugeInc"),
  MetricGaugeDec: refused("MetricGaugeDec"),
  MetricGaugeAdd: refused("MetricGaugeAdd"),
  MetricHistogramRegister: refused("MetricHistogramRegister"),
  MetricHistogramRegisterSimple: refused("MetricHistogramRegisterSimple"),
  MetricHistogramRecord: refused("MetricHistogramRecord"),
  MetricVecRegister: refused("MetricVecRegister"),
  MetricVecWith: refused("MetricVecWith"),
  ObserveReadU64: refused("ObserveReadU64"),
  ObserveScrape: refused("ObserveScrape"),
  ObserveSeries: refused("ObserveSeries"),
  ObserveBarrier: refused("ObserveBarrier"),
  RcClone: resumable({ handler: "shared", operation: "RcClone" }),
  RcDowngrade: resumable({ handler: "shared", operation: "RcDowngrade" }),
  RcDrop: refused("RcDrop"),
  RcGet: resumable({ handler: "shared", operation: "RcGet" }),
  RcIsUnique: resumable({ handler: "shared", operation: "RcIsUnique" }),
  RcNew: resumable({ handler: "shared", operation: "RcNew" }),
  RcSet: resumable({ handler: "shared", operation: "RcSet" }),
  RcStrongCount: resumable({ handler: "shared", operation: "RcStrongCount" }),
  RcWeakCount: resumable({ handler: "shared", operation: "RcWeakCount" }),
  WeakCloneRc: resumable({ handler: "shared", operation: "WeakCloneRc" }),
  WeakDropRc: refused("WeakDropRc"),
  WeakUpgradeRc: resumable({ handler: "shared", operation: "WeakUpgradeRc" }),
  RegexCapture: refused("RegexCapture"),
  RegexCompile: refused("RegexCompile"),
  RegexFreeCapture: refused("RegexFreeCapture"),
  RegexHandle: refused("RegexHandle"),
  RegexMatch: refused("RegexMatch"),
  ReplyChannelCancel: refused("ReplyChannelCancel"),
  ReplyChannelFree: refused("ReplyChannelFree"),
  ReplyChannelNew: refused("ReplyChannelNew"),
  ReplyPayloadFree: refused("ReplyPayloadFree"),
  ReplyWait: refused("ReplyWait"),
  SelectFirst: refused("SelectFirst"),
  StreamChunks: refused("StreamChunks"),
  StreamForward: refused("StreamForward"),
  StreamLines: refused("StreamLines"),
  StreamNextLayout: refused("StreamNextLayout"),
  StreamSendLayout: refused("StreamSendLayout"),
  StreamTake: refused("StreamTake"),
  StreamTryNextLayout: refused("StreamTryNextLayout"),
  StreamTrySendLayout: refused("StreamTrySendLayout"),
  StructuralFormat: resumable({ handler: "structural_format" }),
  StringLines: refused("StringLines"),
  ProcessExit: refused("ProcessExit"),
  SupervisorDirectId: refused("SupervisorDirectId"),
  SupervisorChildGet: refused("SupervisorChildGet"),
  LocalPidSupervisorChildGet: refused("LocalPidSupervisorChildGet"),
  SupervisorNestedGet: refused("SupervisorNestedGet"),
  SupervisorPoolChildGet: refused("SupervisorPoolChildGet"),
  LocalPidSupervisorPoolChildRefGet: refused(
    "LocalPidSupervisorPoolChildRefGet",
  ),
  SupervisorPoolLen: refused("SupervisorPoolLen"),
  SupervisorStop: refused("SupervisorStop"),
  TlsAttachLocal: refused("TlsAttachLocal"),
  WebSocketAttachLocal: refused("WebSocketAttachLocal"),
  TaskFree: refused("TaskFree"),
  GeneratorFree: refused("GeneratorFree"),
  VecAppend: refused("VecAppend"),
  VecClear: refused("VecClear"),
  VecClone: refused("VecClone"),
  VecCloneLayout: refused("VecCloneLayout"),
  VecCloneOwned: refused("VecCloneOwned"),
  VecContainsLayout: refused("VecContainsLayout"),
  VecContainsOwned: refused("VecContainsOwned"),
  VecIsEmpty: refused("VecIsEmpty"),
  VecJoinStr: refused("VecJoinStr"),
  VecLen: refused("VecLen"),
  VecNew: refused("VecNew"),
  VecPopBool: refused("VecPopBool"),
  VecPopLayout: refused("VecPopLayout"),
  VecPopOwned: refused("VecPopOwned"),
  VecPushBool: refused("VecPushBool"),
  VecPushLayout: refused("VecPushLayout"),
  VecPushOwned: refused("VecPushOwned"),
  VecPushOwnedMove: refused("VecPushOwnedMove"),
  VecRemoveAtBool: refused("VecRemoveAtBool"),
  VecRemoveAtLayout: refused("VecRemoveAtLayout"),
  VecRemoveAtOwned: refused("VecRemoveAtOwned"),
  VecSetBool: refused("VecSetBool"),
  VecSetLayout: refused("VecSetLayout"),
  VecSetOwned: refused("VecSetOwned"),
  VecSetOwnedMove: refused("VecSetOwnedMove"),
  VtableDispatchPanicOnOob: refused("VtableDispatchPanicOnOob"),
} satisfies Partial<
  Record<RuntimeFamilyEntry["family"], RuntimeShim | RuntimeRegistration>
>;

const VECTOR_SHIMS = {
  Swap: (_host, args) => {
    const target = vec(args, 0);
    const left = index(args, 1);
    const right = index(args, 2);
    if (
      left < 0 ||
      left >= target.items.length ||
      right < 0 ||
      right >= target.items.length
    ) {
      throw new ShimFault("vector_bounds", "vector swap index out of bounds");
    }
    [target.items[left], target.items[right]] = [
      target.items[right]!,
      target.items[left]!,
    ];
    return target;
  },
  Pop: (_host, args) => {
    const target = vec(args, 0);
    if (target.items.length === 0) {
      throw new ShimFault("vector_bounds", "pop from empty vector");
    }
    return {
      kind: "record",
      typeId: "",
      fields: [target, target.items.pop()!],
    };
  },
  New: () => ({ kind: "vector", elementType: "", items: [] }),
  // The receiver arrives by `move` and leaves as the call's result, so the
  // package can store the vector back into its place. The element carries the
  // ownership decision SIR proved, so it moves in as it arrives.
  Push: (_host, args) => {
    const target = vec(args, 0);
    target.items.push(arg(args, 1));
    return target;
  },
  Remove: (_host, args) => {
    const target = vec(args, 0);
    const at = index(args, 1);
    if (at < 0 || at >= target.items.length) {
      throw new ShimFault(
        "vector_bounds",
        `vector index ${at} out of bounds for length ${target.items.length}`,
      );
    }
    const [item] = target.items.splice(at, 1);
    return { kind: "record", typeId: "", fields: [target, item!] };
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
  Index: (_host, args) => cloneValue(sequenceElement(args)),
  IndexBorrow: (_host, args) => sequenceElement(args),
  Len: (_host, args) => int(BigInt(vec(args, 0).items.length)),
  Join: (_host, args) => {
    const items = vec(args, 0).items;
    return str(
      items
        .map((_, at) => indexed(items, at).value)
        .join(indexed(args, 1).value),
    );
  },
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
} satisfies Record<string, RuntimeShim>;

function sequenceElement(args: VmValue[]): VmValue {
  const items = vec(args, 0).items;
  const at = index(args, 1);
  if (at < 0 || at >= items.length) {
    throw new ShimFault(
      "vector_bounds",
      `vector index ${at} out of bounds for length ${items.length}`,
    );
  }
  return items[at]!;
}

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
const MATH_SHIMS = {
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
} satisfies Record<string, RuntimeShim>;

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
const FLOAT_METHOD_SHIMS = {
  ToBits: (_host, args) => int(toBits(real(args, 0))),
  IsNan: (_host, args) => bool(Number.isNaN(real(args, 0))),
  IsFinite: (_host, args) => bool(Number.isFinite(real(args, 0))),
  IsInfinite: (_host, args) => {
    const value = real(args, 0);
    return bool(value === Infinity || value === -Infinity);
  },
  IsSignNegative: (_host, args) => bool(toBits(real(args, 0)) >> 63n === 1n),
} satisfies Record<string, RuntimeShim>;

/// The receiver width an `IntMethod`/`IntArith` detail names. The VM is a
/// 64-bit target, so the pointer-sized widths are 64 bits wide.
type IntWidthName = Extract<
  RuntimeFamilyEntry,
  { family: "IntMethod" }
>["detail"][1];
type IntWidth = { bits: number; signed: boolean };
const INT_WIDTHS = {
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
} satisfies Record<IntWidthName, IntWidth>;

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
function intMethodShim(
  detail: Extract<RuntimeFamilyEntry, { family: "IntMethod" }>["detail"],
): RuntimeShim {
  const [op, name] = detail;
  const width = INT_WIDTHS[name];
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
    default: {
      const exhaustive: never = op;
      throw new Error(`unknown integer operation ${String(exhaustive)}`);
    }
  }
}

/// Non-trapping integer arithmetic at the receiver's width: wrapping keeps
/// the low bits, saturating clamps to the width's bounds and checked answers
/// `None` where native's overflow flag is set.
function intArithShim(
  detail: Extract<RuntimeFamilyEntry, { family: "IntArith" }>["detail"],
): RuntimeShim {
  const [kind, name] = detail;
  const width = INT_WIDTHS[name];
  const [min, max] = intBounds(width);
  type ArithKind = typeof kind;
  const add = (x: bigint, y: bigint) => x + y;
  const sub = (x: bigint, y: bigint) => x - y;
  const mul = (x: bigint, y: bigint) => x * y;
  const operations = {
    WrappingAdd: ["Wrapping", add],
    WrappingSub: ["Wrapping", sub],
    WrappingMul: ["Wrapping", mul],
    SaturatingAdd: ["Saturating", add],
    SaturatingSub: ["Saturating", sub],
    SaturatingMul: ["Saturating", mul],
    CheckedAdd: ["Checked", add],
    CheckedSub: ["Checked", sub],
    CheckedMul: ["Checked", mul],
  } as const satisfies Record<
    ArithKind,
    readonly [
      "Wrapping" | "Saturating" | "Checked",
      (x: bigint, y: bigint) => bigint,
    ]
  >;
  const [mode, apply] = operations[kind];
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

function saturatingI64(value: bigint): bigint {
  return bigMax(-(1n << 63n), bigMin((1n << 63n) - 1n, value));
}

// ── externs ─────────────────────────────────────────────────────────────────

const EXTERN_SHIMS: Record<string, RuntimeShim | Ordinary | undefined> = {
  hew_semaphore_new: (host, args) => host.semaphores!.create(integer(args, 0)),
  hew_semaphore_clone: (host, args) => host.semaphores!.clone(arg(args, 0)),
  hew_semaphore_try_acquire: (host, args) =>
    bool(host.semaphores!.tryAcquire(arg(args, 0))),
  hew_semaphore_count: (host, args) =>
    int(host.semaphores!.count(arg(args, 0))),
  hew_semaphore_release: (host, args) => {
    host.semaphores!.release(arg(args, 0));
    return UNIT;
  },
  hew_semaphore_free: (host, args) => {
    host.semaphores!.close(arg(args, 0));
    return UNIT;
  },
  hew_path_separator: () => str("/"),
  hew_calendar_format: {
    kind: "ordinary",
    bridge: "calendarBridge",
    shim: (host, args, _variant, shape) =>
      executeCalendar(host.calendarBridge, "hew_calendar_format", args, shape),
  },
  hew_calendar_parse: {
    kind: "ordinary",
    bridge: "calendarBridge",
    shim: (host, args, _variant, shape) =>
      executeCalendar(host.calendarBridge, "hew_calendar_parse", args, shape),
  },
  hew_calendar_components: {
    kind: "ordinary",
    bridge: "calendarBridge",
    shim: (host, args, _variant, shape) =>
      executeCalendar(
        host.calendarBridge,
        "hew_calendar_components",
        args,
        shape,
      ),
  },
  hew_calendar_from_parts: {
    kind: "ordinary",
    bridge: "calendarBridge",
    shim: (host, args, _variant, shape) =>
      executeCalendar(
        host.calendarBridge,
        "hew_calendar_from_parts",
        args,
        shape,
      ),
  },

  hew_args_count: (host) => int(BigInt(host.argv.length)),
  // A native null managed string is canonical empty. Each result owns fresh
  // string storage; releasing it cannot change the host's supplied arguments.
  hew_args_get: (host, args) => str(host.argv[index(args, 0)] ?? ""),
  hew_actor_timer_cancel: (_host, args) => {
    const timer = arg(args, 0);
    if (timer.kind !== "timer" || timer.closed)
      throw new Error("invalid timer handle");
    timer.cancel();
    return UNIT;
  },
  hew_actor_timer_release: (_host, args) => {
    const timer = arg(args, 0);
    if (timer.kind !== "timer" || timer.closed)
      throw new Error("invalid timer handle");
    timer.closed = true;
    return UNIT;
  },
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
  hew_stream_last_error: (host) => host.fileError("LastError"),
  hew_io_write: (host, args) => checkedWrite(host, text(args, 0), false),
  hew_io_write_err: (host, args) => checkedWrite(host, text(args, 0), true),

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

export function decodeUtf8Checked(
  value: VmValue,
): { value: string } | { validUpTo: number; errorLen: number | null } {
  if (value.kind !== "vector") throw new TypeError("UTF-8 input must be bytes");
  const bytes = Uint8Array.from(
    value.items.map((item) => Number(integer([item], 0))),
  );
  let at = 0;
  while (at < bytes.length) {
    const first = bytes[at]!;
    if (first < 128) {
      at++;
      continue;
    }
    const width =
      first >= 194 && first <= 223
        ? 2
        : first >= 224 && first <= 239
          ? 3
          : first >= 240 && first <= 244
            ? 4
            : 0;
    if (width === 0) return { validUpTo: at, errorLen: 1 };
    for (let offset = 1; offset < width; offset++) {
      if (at + offset >= bytes.length) return { validUpTo: at, errorLen: null };
      const next = bytes[at + offset]!;
      const lower =
        offset === 1 && first === 224
          ? 160
          : offset === 1 && first === 240
            ? 144
            : 128;
      const upper =
        offset === 1 && first === 237
          ? 159
          : offset === 1 && first === 244
            ? 143
            : 191;
      if (next < lower || next > upper)
        return { validUpTo: at, errorLen: offset };
    }
    at += width;
  }
  return { value: new TextDecoder("utf-8", { ignoreBOM: true }).decode(bytes) };
}

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

function arg(args: VmValue[], at: number): VmValue {
  return args[at] ?? UNIT;
}

function indexed(
  args: VmValue[],
  at: number,
): Extract<VmValue, { kind: "string" }> {
  const value = arg(args, at);
  if (value.kind !== "string")
    throw new TypeError(
      `runtime call expected a string operand, got ${value.kind}`,
    );
  return value;
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
  return stringValue(value);
}

function bool(value: boolean): VmValue {
  return { kind: "bool", value };
}

function int(value: bigint): VmValue {
  return { kind: "i64", value };
}

function checkedChar(
  host: ShimHost,
  args: VmValue[],
  shape: VariantShape | null,
): VmValue {
  if (!shape) throw new TypeError("try_to_char requires its Option shape");
  const value = integer(args, 0);
  return value < 0n ||
    value > 0x10ffffn ||
    (value >= 0xd800n && value <= 0xdfffn)
    ? host.enumValue(shape, "OptionNone", [])
    : host.enumValue(shape, "OptionSome", [
        str(String.fromCodePoint(Number(value))),
      ]);
}

function stringRange(args: VmValue[]): VmValue | undefined {
  const value = indexed(args, 0);
  const inclusive = arg(args, 3);
  const openEnd = arg(args, 4);
  if (inclusive.kind !== "bool" || openEnd.kind !== "bool")
    throw new TypeError("string range flags must be bool");
  const start = integer(args, 1);
  const end = openEnd.value
    ? BigInt(value.scalarLength)
    : integer(args, 2) + (inclusive.value ? 1n : 0n);
  if (start < 0n || end < start || end > BigInt(value.scalarLength))
    return undefined;
  return value.slice(Number(start), Number(end));
}
