export type StdlibCapabilityStatus =
  | "shim"
  | "unsupported_out_of_scope"
  | "unsupported_m7_deferred_to_post_v05"
  | "unsupported_native_only";

export type UnsupportedKind =
  | "Unsupported::SANDBOX_OUT_OF_SCOPE"
  | "Unsupported::M7_DEFERRED"
  | "Unsupported::NATIVE_ONLY";

export type StdlibShimHandler =
  | "io.stdout.print"
  | "io.stdout.println"
  | "io.stderr.print"
  | "io.stderr.println"
  | "io.log"
  | "process.exit"
  | "time.now"
  | "time.sleep"
  | "time.deadline"
  | "fmt.to_string"
  | "string.len"
  | "string.concat"
  | "string.slice"
  | "math.abs"
  | "math.min"
  | "math.max"
  | "math.clamp"
  | "vec.len"
  | "vec.get"
  | "vec.push"
  | "regex.compile"
  | "regex.is_match"
  | "regex.find"
  | "regex.replace";

export interface StdlibProfileEntry {
  id: string;
  module: string;
  name: string;
  status: StdlibCapabilityStatus;
  reason: string;
  handler?: StdlibShimHandler;
  aliases?: string[];
}

export interface UnsupportedStdlibDiagnostic {
  kind: UnsupportedKind;
  symbol: string;
  status: Exclude<StdlibCapabilityStatus, "shim">;
  reason: string;
}

export const SANDBOX_STDLIB_PROFILE_VERSION = "hew.sandbox.stdlib-profile.m7";

export const SANDBOX_STDLIB_PROFILE: readonly StdlibProfileEntry[] = [
  shim("sym:core.stdout.print", "core.stdout", "print", "io.stdout.print", "page stdout buffer", ["sym:builtin.print", "sym:std.io.stdout.print"]),
  shim("sym:core.stdout.println", "core.stdout", "println", "io.stdout.println", "page stdout buffer", ["sym:builtin.println", "sym:std.io.stdout.println"]),
  shim("sym:core.stderr.print", "core.stderr", "print", "io.stderr.print", "page stderr buffer", ["sym:std.io.stderr.print"]),
  shim("sym:core.stderr.println", "core.stderr", "println", "io.stderr.println", "page stderr buffer", ["sym:std.io.stderr.println"]),
  shim("sym:std.misc.log", "std.misc", "log", "io.log", "page log sink"),
  shim("sym:core.exit", "core", "exit", "process.exit", "sandbox termination record", ["sym:builtin.exit"]),
  shim("sym:std.time.now", "std.time", "now", "time.now", "virtual clock current time", ["sym:time.now"]),
  shim("sym:std.time.sleep", "std.time", "sleep", "time.sleep", "virtual clock sleep", ["sym:time.sleep", "sym:builtin.sleep"]),
  shim("sym:std.time.deadline", "std.time", "deadline", "time.deadline", "virtual clock deadline calculation", ["sym:time.deadline"]),
  shim("sym:std.fmt.to_string", "std.fmt", "to_string", "fmt.to_string", "canonical sandbox value rendering", ["sym:std.fmt.debug"]),
  shim("sym:std.string.len", "std.string", "len", "string.len", "Unicode code-point string length"),
  shim("sym:std.string.concat", "std.string", "concat", "string.concat", "pure string concatenation"),
  shim("sym:std.string.slice", "std.string", "slice", "string.slice", "Unicode code-point string slicing"),
  shim("sym:std.math.abs", "std.math", "abs", "math.abs", "deterministic integer/float absolute value"),
  shim("sym:std.math.min", "std.math", "min", "math.min", "deterministic scalar minimum"),
  shim("sym:std.math.max", "std.math", "max", "math.max", "deterministic scalar maximum"),
  shim("sym:std.math.clamp", "std.math", "clamp", "math.clamp", "deterministic scalar clamp"),
  shim("sym:std.vec.len", "std.vec", "len", "vec.len", "VM vector length"),
  shim("sym:std.vec.get", "std.vec", "get", "vec.get", "VM vector bounds-checked lookup"),
  shim("sym:std.vec.push", "std.vec", "push", "vec.push", "VM vector append"),
  shim("sym:std.text.regex.compile", "std.text.regex", "compile", "regex.compile", "curated deterministic JavaScript RegExp subset"),
  shim("sym:std.text.regex.is_match", "std.text.regex", "is_match", "regex.is_match", "curated deterministic JavaScript RegExp subset"),
  shim("sym:std.text.regex.find", "std.text.regex", "find", "regex.find", "curated deterministic JavaScript RegExp subset"),
  shim("sym:std.text.regex.replace", "std.text.regex", "replace", "regex.replace", "curated deterministic JavaScript RegExp subset"),

  unsupported("sym:std.option", "std.option", "*", "unsupported_m7_deferred_to_post_v05", "Option helper lowering is not emitted as a sandbox stdlib call in M7"),
  unsupported("sym:std.result", "std.result", "*", "unsupported_m7_deferred_to_post_v05", "Result helper lowering is not emitted as a sandbox stdlib call in M7"),
  unsupported("sym:std.iter", "std.iter", "*", "unsupported_m7_deferred_to_post_v05", "Iterator helper lowering is deferred until post-v0.5"),
  unsupported("sym:std.sort", "std.sort", "*", "unsupported_m7_deferred_to_post_v05", "Sort helper lowering is deferred until post-v0.5"),
  unsupported("sym:std.collections.hashset", "std.collections.hashset", "*", "unsupported_m7_deferred_to_post_v05", "Visible deterministic hashset iteration is deferred until post-v0.5"),
  unsupported("sym:std.deque", "std.deque", "*", "unsupported_m7_deferred_to_post_v05", "Deque helper lowering is deferred until post-v0.5"),
  unsupported("sym:std.encoding.csv", "std.encoding.csv", "*", "unsupported_m7_deferred_to_post_v05", "CSV encoding shim is deferred until post-v0.5"),
  unsupported("sym:std.encoding.yaml", "std.encoding.yaml", "*", "unsupported_m7_deferred_to_post_v05", "YAML encoding shim is deferred until post-v0.5"),
  unsupported("sym:std.encoding.toml", "std.encoding.toml", "*", "unsupported_m7_deferred_to_post_v05", "TOML encoding shim is deferred until post-v0.5"),
  unsupported("sym:std.encoding.xml", "std.encoding.xml", "*", "unsupported_m7_deferred_to_post_v05", "XML encoding shim is deferred until post-v0.5"),
  unsupported("sym:std.encoding.markdown", "std.encoding.markdown", "*", "unsupported_m7_deferred_to_post_v05", "Markdown rendering requires a sanitizer boundary and is deferred"),
  unsupported("sym:std.encoding.msgpack", "std.encoding.msgpack", "*", "unsupported_m7_deferred_to_post_v05", "MessagePack encoding shim is deferred until post-v0.5"),
  unsupported("sym:std.encoding.protobuf", "std.encoding.protobuf", "*", "unsupported_m7_deferred_to_post_v05", "Protobuf encoding shim is deferred until post-v0.5"),
  unsupported("sym:std.encoding.compress", "std.encoding.compress", "*", "unsupported_m7_deferred_to_post_v05", "Compression shims are deferred until post-v0.5"),
  unsupported("sym:std.crypto.random_bytes", "std.crypto", "random_bytes", "unsupported_m7_deferred_to_post_v05", "Seeded non-cryptographic random bytes need an explicit lesson capability"),
  unsupported("sym:std.crypto.password", "std.crypto.password", "*", "unsupported_m7_deferred_to_post_v05", "Password and JWT helpers are entropy/time-sensitive and deferred"),
  unsupported("sym:std.bench", "std.bench", "*", "unsupported_m7_deferred_to_post_v05", "Real-time benchmarking does not apply to the virtual-clock sandbox"),
  unsupported("sym:std.net.websocket", "std.net.websocket", "*", "unsupported_m7_deferred_to_post_v05", "WebSocket host capability is deferred beyond M7"),

  unsupported("sym:std.fs", "std.fs", "*", "unsupported_native_only", "Real filesystem access is unavailable in the browser sandbox"),
  unsupported("sym:std.io.file", "std.io.file", "*", "unsupported_native_only", "File-backed I/O is unavailable in the browser sandbox"),
  unsupported("sym:std.os", "std.os", "*", "unsupported_native_only", "Host OS args, env, and system data are unavailable in the sandbox"),
  unsupported("sym:std.process", "std.process", "*", "unsupported_native_only", "Process spawning is unavailable in the sandbox"),
  unsupported("sym:std.net.tcp", "std.net.tcp", "*", "unsupported_native_only", "TCP sockets are unavailable in the deterministic browser sandbox"),
  unsupported("sym:std.net.http", "std.net.http", "*", "unsupported_native_only", "HTTP server sockets are unavailable in the deterministic browser sandbox"),
  unsupported("sym:std.net.http_client", "std.net.http_client", "*", "unsupported_native_only", "Browser fetch is not exposed to Hew semantics in the deterministic sandbox"),
  unsupported("sym:std.net.dns", "std.net.dns", "*", "unsupported_native_only", "External DNS is nondeterministic and unavailable in the sandbox"),
  unsupported("sym:std.net.tls", "std.net.tls", "*", "unsupported_native_only", "TLS networking is native-only"),
  unsupported("sym:std.net.quic", "std.net.quic", "*", "unsupported_native_only", "QUIC networking is native-only"),
  unsupported("sym:std.net.smtp", "std.net.smtp", "*", "unsupported_native_only", "SMTP networking is native-only"),
  unsupported("sym:stream.file", "stream.file", "*", "unsupported_native_only", "File-backed streams are unavailable in the browser sandbox"),
  unsupported("sym:stream.net", "stream.net", "*", "unsupported_native_only", "Network-backed streams are unavailable in the deterministic sandbox"),

  unsupported("sym:std.path.host", "std.path", "metadata", "unsupported_out_of_scope", "Host-dependent path metadata is outside the sandbox profile"),
  unsupported("sym:std.net.url", "std.net.url", "*", "unsupported_out_of_scope", "URL helpers are not exported by the M7 sandbox VM profile"),
  unsupported("sym:std.net.mime", "std.net.mime", "*", "unsupported_out_of_scope", "MIME helpers are not exported by the M7 sandbox VM profile"),
  unsupported("sym:std.net.ipnet", "std.net.ipnet", "*", "unsupported_out_of_scope", "IP network helpers are not exported by the M7 sandbox VM profile"),
  unsupported("sym:std.testing", "std.testing", "*", "unsupported_out_of_scope", "Testing helpers are compile-time/lesson tooling and are outside runtime VM shims")
] as const;

export function stdlibProfileEntries(): readonly StdlibProfileEntry[] {
  return SANDBOX_STDLIB_PROFILE;
}

export function lookupStdlibProfile(symbol: { id: string; module: string; name: string }): StdlibProfileEntry | undefined {
  return PROFILE_BY_ID.get(symbol.id) ?? PROFILE_BY_MODULE_NAME.get(`${symbol.module}.${symbol.name}`) ?? PROFILE_BY_MODULE_NAME.get(`${symbol.module}.*`);
}

export function unsupportedDiagnosticFor(
  symbol: { id: string; module: string; name: string },
  entry: StdlibProfileEntry | undefined
): UnsupportedStdlibDiagnostic {
  const status = entry?.status === "shim" || entry === undefined ? classifyUnknownStatus(symbol.module) : entry.status;
  return {
    kind: unsupportedKind(status),
    symbol: `${symbol.module}.${symbol.name}`,
    status,
    reason: entry?.status === "shim" || entry === undefined ? unknownReason(status, symbol.module) : entry.reason
  };
}

export function validateStdlibProfile(
  profile: readonly StdlibProfileEntry[],
  implementedHandlers: ReadonlySet<StdlibShimHandler>
): void {
  const seenIds = new Set<string>();
  const failures: string[] = [];
  for (const entry of profile) {
    if (seenIds.has(entry.id)) {
      failures.push(`${entry.id}: duplicate stdlib profile id`);
    }
    seenIds.add(entry.id);
    if (entry.status === "shim") {
      if (!entry.handler) {
        failures.push(`${entry.id}: shim entry missing handler`);
      } else if (!implementedHandlers.has(entry.handler)) {
        failures.push(`${entry.id}: shim handler ${entry.handler} is not implemented`);
      }
    } else if (entry.handler) {
      failures.push(`${entry.id}: unsupported entry must not declare a handler`);
    }
  }
  if (failures.length > 0) {
    throw new StdlibProfileValidationError(failures);
  }
}

export class StdlibProfileValidationError extends Error {
  constructor(readonly errors: string[]) {
    super(`Invalid sandbox stdlib profile: ${errors.join("; ")}`);
    this.name = "StdlibProfileValidationError";
  }
}

const PROFILE_BY_ID = new Map<string, StdlibProfileEntry>();
const PROFILE_BY_MODULE_NAME = new Map<string, StdlibProfileEntry>();

for (const entry of SANDBOX_STDLIB_PROFILE) {
  PROFILE_BY_ID.set(entry.id, entry);
  for (const alias of entry.aliases ?? []) {
    PROFILE_BY_ID.set(alias, entry);
  }
  PROFILE_BY_MODULE_NAME.set(`${entry.module}.${entry.name}`, entry);
}

function shim(
  id: string,
  module: string,
  name: string,
  handler: StdlibShimHandler,
  reason: string,
  aliases: string[] = []
): StdlibProfileEntry {
  return { id, module, name, status: "shim", reason, handler, aliases };
}

function unsupported(
  id: string,
  module: string,
  name: string,
  status: Exclude<StdlibCapabilityStatus, "shim">,
  reason: string
): StdlibProfileEntry {
  return { id, module, name, status, reason };
}

function unsupportedKind(status: Exclude<StdlibCapabilityStatus, "shim">): UnsupportedKind {
  switch (status) {
    case "unsupported_m7_deferred_to_post_v05":
      return "Unsupported::M7_DEFERRED";
    case "unsupported_native_only":
      return "Unsupported::NATIVE_ONLY";
    case "unsupported_out_of_scope":
      return "Unsupported::SANDBOX_OUT_OF_SCOPE";
  }
}

function classifyUnknownStatus(moduleName: string): Exclude<StdlibCapabilityStatus, "shim"> {
  if (
    moduleName.startsWith("std.fs") ||
    moduleName.startsWith("std.os") ||
    moduleName.startsWith("std.process") ||
    moduleName.startsWith("std.io.file") ||
    moduleName.startsWith("std.net") ||
    moduleName.startsWith("stream.file") ||
    moduleName.startsWith("stream.net")
  ) {
    return "unsupported_native_only";
  }
  if (moduleName.startsWith("std.encoding") || moduleName.startsWith("std.crypto") || moduleName.startsWith("std.bench")) {
    return "unsupported_m7_deferred_to_post_v05";
  }
  return "unsupported_out_of_scope";
}

function unknownReason(status: Exclude<StdlibCapabilityStatus, "shim">, moduleName: string): string {
  switch (status) {
    case "unsupported_native_only":
      return `${moduleName} requires host/native capabilities that are unavailable in the deterministic browser sandbox`;
    case "unsupported_m7_deferred_to_post_v05":
      return `${moduleName} is deferred until a post-v0.5 sandbox profile`;
    case "unsupported_out_of_scope":
      return `${moduleName} is not part of the M7 educational sandbox stdlib profile`;
  }
}
