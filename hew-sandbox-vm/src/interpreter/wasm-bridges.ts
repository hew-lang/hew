import type { RunOptions, UnicodeBridge } from "./types.js";

const UNICODE_FAMILIES = new Set([
  "CharIsDigit",
  "CharIsLetter",
  "CharIsAlphanumeric",
  "CharIsWhitespace",
  "CharIsUpper",
  "CharIsLower",
  "CharIsPunctuation",
  "CharIsControl",
  "CharIsAscii",
  "CharToUpper",
  "CharToLower",
  "CharToTitle",
]);

const BRIDGED_EXTERNS = new Set([
  "hew_calendar_format",
  "hew_calendar_format_iso8601",
  "hew_calendar_parse",
  "hew_calendar_parse_iso8601",
  "hew_calendar_from_parts",
  "hew_calendar_components",
  "hew_regex_new",
  "hew_regex_is_valid",
  "hew_sha256_hew",
  "hew_sha384_hew",
  "hew_sha512_hew",
  "hew_hmac_sha256_hew",
  "hew_constant_time_eq_hew",
  "hew_url_parse",
  "hew_url_is_valid",
  "hew_url_scheme",
  "hew_url_host",
  "hew_url_port",
  "hew_url_path",
  "hew_url_query",
  "hew_url_fragment",
  "hew_url_to_string",
  "hew_url_join",
  "hew_url_free",
]);

export type WasmBridgeOptions = Pick<
  RunOptions,
  | "codecBridge"
  | "calendarBridge"
  | "regexBridge"
  | "unicodeBridge"
  | "cryptoBridge"
  | "urlBridge"
>;

interface WasmExports {
  executeCodec?: (request: string) => string;
  executeCalendar?: (request: string) => string;
  compileSandboxRegex?: RunOptions["regexBridge"];
  charProperties?: (codepoint: number) => number;
  charToUpper?: (codepoint: number) => number;
  charToLower?: (codepoint: number) => number;
  charToTitle?: (codepoint: number) => number;
  executeCrypto?: (request: string) => string;
  executeUrl?: (request: string) => string;
  default?: WasmExports;
}

export function packageNeedsWasmBridges(pkg: unknown): boolean {
  if (typeof pkg !== "object" || pkg === null) return false;
  const candidate = pkg as {
    codecs?: unknown[];
    regex_patterns?: unknown[];
    runtime_families?: Array<{ family?: string }>;
    externs?: Array<{ symbol?: string }>;
  };
  if (Array.isArray(candidate.codecs) && candidate.codecs.length > 0)
    return true;
  if (
    Array.isArray(candidate.regex_patterns) &&
    candidate.regex_patterns.length > 0
  )
    return true;
  if (
    Array.isArray(candidate.runtime_families) &&
    candidate.runtime_families.some(
      (entry) =>
        entry?.family === "Encoding" ||
        (typeof entry?.family === "string" &&
          UNICODE_FAMILIES.has(entry.family)),
    )
  )
    return true;
  if (
    Array.isArray(candidate.externs) &&
    candidate.externs.some(
      (entry) =>
        typeof entry?.symbol === "string" && BRIDGED_EXTERNS.has(entry.symbol),
    )
  )
    return true;
  return false;
}

export function extractUnicodeBridge(
  wasmModule: unknown,
): UnicodeBridge | undefined {
  const mod = wasmModule as WasmExports | undefined;
  const source = mod?.default && !mod.charProperties ? mod.default : mod;
  if (
    typeof source?.charProperties === "function" &&
    typeof source?.charToUpper === "function" &&
    typeof source?.charToLower === "function" &&
    typeof source?.charToTitle === "function"
  ) {
    return {
      charProperties: source.charProperties,
      charToUpper: source.charToUpper,
      charToLower: source.charToLower,
      charToTitle: source.charToTitle,
    };
  }
  return undefined;
}

export function bridgesFromWasmModule(wasmModule: unknown): WasmBridgeOptions {
  const mod = wasmModule as WasmExports | undefined;
  const codecBridge = mod?.executeCodec ?? mod?.default?.executeCodec;
  const calendarBridge = mod?.executeCalendar ?? mod?.default?.executeCalendar;
  const regexBridge =
    mod?.compileSandboxRegex ?? mod?.default?.compileSandboxRegex;
  const unicodeBridge = extractUnicodeBridge(wasmModule);
  const cryptoBridge = mod?.executeCrypto ?? mod?.default?.executeCrypto;
  const urlBridge = mod?.executeUrl ?? mod?.default?.executeUrl;
  return {
    codecBridge,
    calendarBridge,
    regexBridge,
    unicodeBridge,
    cryptoBridge,
    urlBridge,
  };
}
