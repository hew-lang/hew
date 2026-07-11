import type { JsonValue } from "./types.js";

export type VmValue =
  | { kind: "unit" }
  | { kind: "bool"; value: boolean }
  | { kind: "i64"; value: bigint }
  | { kind: "f64"; value: number }
  | { kind: "string"; value: string }
  | { kind: "actor"; id: string }
  | { kind: "supervisor"; id: string }
  | { kind: "monitor"; id: string }
  | { kind: "reply"; id: string }
  | { kind: "channel"; id: string }
  | { kind: "task"; id: string }
  | { kind: "stream"; channelId: string }
  | { kind: "sink"; channelId: string }
  | { kind: "duplex"; channelId: string }
  | { kind: "regex"; source: string; regex: RegExp }
  | { kind: "record"; typeId: string; fields: VmValue[] }
  | { kind: "enum"; typeId: string; tag: number; payload: VmValue[] }
  | { kind: "vector"; elementType: string; items: VmValue[] }
  /** A first-class function reference materialised by `const.function`.
   *  `id` is the bytecode function id (e.g. `"fn:my_handler"`).
   *  Function values are immutable: `cloneValue` is identity. */
  | { kind: "function"; id: string };

export const UNIT: VmValue = { kind: "unit" };

const I64_LITERAL_DECIMAL = /^(?:0|-?[1-9]\d*)$/;

export function cloneValue(value: VmValue): VmValue {
  switch (value.kind) {
    case "unit":
      return UNIT;
    case "bool":
    case "i64":
    case "f64":
    case "string":
    case "actor":
    case "supervisor":
    case "monitor":
    case "reply":
    case "channel":
    case "task":
    case "stream":
    case "sink":
    case "duplex":
    case "function":  // function values are immutable — identity clone
      return { ...value };
    case "regex":
      return { kind: "regex", source: value.source, regex: new RegExp(value.source, value.regex.flags) };
    case "record":
      return { kind: "record", typeId: value.typeId, fields: value.fields.map(cloneValue) };
    case "enum":
      return { kind: "enum", typeId: value.typeId, tag: value.tag, payload: value.payload.map(cloneValue) };
    case "vector":
      return { kind: "vector", elementType: value.elementType, items: value.items.map(cloneValue) };
  }
}

export function valueFromLiteral(value: JsonValue): VmValue {
  const taggedI64 = taggedI64Literal(value);
  if (taggedI64 !== null) {
    return { kind: "i64", value: taggedI64 };
  }
  if (value === null) {
    return UNIT;
  }
  switch (typeof value) {
    case "boolean":
      return { kind: "bool", value };
    case "number":
      return Number.isInteger(value) ? { kind: "i64", value: BigInt(value) } : { kind: "f64", value };
    case "string":
      return { kind: "string", value };
    default:
      return { kind: "string", value: canonicalJson(value) };
  }
}

function taggedI64Literal(value: JsonValue): bigint | null {
  if (value === null || Array.isArray(value) || typeof value !== "object") {
    return null;
  }
  if (
    Object.keys(value).length !== 2 ||
    value.kind !== "i64" ||
    typeof value.value !== "string" ||
    !I64_LITERAL_DECIMAL.test(value.value)
  ) {
    return null;
  }
  const parsed = BigInt(value.value);
  return BigInt.asIntN(64, parsed) === parsed ? parsed : null;
}

/// Render an f64 value to a string matching native Hew output.
///
/// Native uses `printf("%g", x)` which produces:
/// - `inf`, `-inf`, `nan` for non-finite values (lowercase)
/// - Negative zero as `-0`
/// - 6 significant digits with trailing-zero removal
/// - Scientific notation (`e+NN`/`e-NN`, at least 2 exponent digits) when the
///   exponent is < -4 or >= 6; fixed notation otherwise
///
/// JavaScript's `String()` diverges from `%g` in several ways:
/// - `String(-0)` → `"0"` (drops the sign)
/// - Uses shortest round-trip digits (Ryu), not 6 sig-figs
/// - Switches between fixed/scientific at different thresholds
///
/// This function replicates `printf("%g")` semantics for all finite cases.
export function renderF64(value: number): string {
  if (Number.isNaN(value)) return "nan";
  if (value === Infinity) return "inf";
  if (value === -Infinity) return "-inf";
  // Negative zero: %g outputs "-0"; JavaScript's String() outputs "0".
  if (Object.is(value, -0)) return "-0";
  if (value === 0) return "0";

  const absVal = Math.abs(value);
  // `%g` precision is 6 significant digits; the format choice (fixed vs
  // scientific) is based on the exponent of the *rounded* value, not the
  // original. For example, 999999.5 rounds to 1000000 (exp=6) → scientific;
  // 0.00009999995 rounds to 0.0001 (exp=-4) → fixed.
  // Computing exp directly from Math.log10(absVal) gives the wrong answer at
  // rounding boundaries, so we round first and recompute.
  const rounded = parseFloat(absVal.toPrecision(6));
  const exp = Math.floor(Math.log10(rounded));

  let s: string;
  if (exp < -4 || exp >= 6) {
    // Scientific path: toExponential(5) gives 5 digits after the decimal
    // point = 6 significant digits, then we strip trailing zeros and
    // normalise the exponent to at least 2 digits with an explicit sign.
    s = rounded.toExponential(5);
    const eIdx = s.indexOf("e");
    let mantissa = s.slice(0, eIdx);
    const expPart = s.slice(eIdx + 1); // e.g. "+20" or "-324"
    if (mantissa.includes(".")) {
      mantissa = mantissa.replace(/\.?0+$/, "");
    }
    const sign = expPart[0]; // "+" or "-"
    let digits = expPart.slice(1);
    if (digits.length < 2) digits = "0" + digits;
    s = (value < 0 ? "-" : "") + mantissa + "e" + sign + digits;
  } else {
    // Fixed path: toPrecision(6) gives 6 significant digits; strip trailing
    // zeros (and the decimal point if it becomes empty).
    s = absVal.toPrecision(6);
    if (s.includes(".")) {
      s = s.replace(/\.?0+$/, "");
    }
    if (value < 0) s = "-" + s;
  }
  return s;
}

export function renderStdout(value: VmValue): string {
  switch (value.kind) {
    case "unit":
      return "()";
    case "bool":
      return value.value ? "true" : "false";
    case "i64":
      return value.value.toString();
    case "f64":
      return renderF64(value.value);
    case "string":
      return value.value;
    case "actor":
    case "supervisor":
    case "monitor":
    case "reply":
      return value.id;
    case "channel":
      return value.id;
    case "task":
      return value.id;
    case "stream":
    case "sink":
    case "duplex":
      return value.channelId;
    case "regex":
      return value.source;
    case "record":
      return canonicalJson({
        type: value.typeId,
        fields: value.fields.map((field) => toJsonValue(field))
      });
    case "enum":
      return canonicalJson({
        type: value.typeId,
        tag: value.tag,
        payload: value.payload.map((field) => toJsonValue(field))
      });
    case "vector":
      return canonicalJson(value.items.map((item) => toJsonValue(item)));
    case "function":
      return value.id;
  }
}

export function toJsonValue(value: VmValue): JsonValue {
  switch (value.kind) {
    case "unit":
      return null;
    case "bool":
    case "f64":
    case "string":
      return value.value;
    case "i64":
      return i64ToJsonValue(value.value);
    case "actor":
    case "supervisor":
    case "monitor":
    case "reply":
      return value.id;
    case "channel":
      return value.id;
    case "task":
      return value.id;
    case "stream":
    case "sink":
    case "duplex":
      return value.channelId;
    case "regex":
      return value.source;
    case "record":
      return { type: value.typeId, fields: value.fields.map(toJsonValue) };
    case "enum":
      return { type: value.typeId, tag: value.tag, payload: value.payload.map(toJsonValue) };
    case "vector":
      return value.items.map(toJsonValue);
    case "function":
      return value.id;
  }
}

export function canonicalJson(value: JsonValue): string {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJson).join(",")}]`;
  }
  const entries = Object.entries(value).sort(([left], [right]) => left.localeCompare(right));
  return `{${entries.map(([key, item]) => `${JSON.stringify(key)}:${canonicalJson(item)}`).join(",")}}`;
}

export function i64ToJsonValue(value: bigint): JsonValue {
  if (value >= BigInt(Number.MIN_SAFE_INTEGER) && value <= BigInt(Number.MAX_SAFE_INTEGER)) {
    return Number(value);
  }
  return value.toString();
}

export function i64ComparableJson(value: bigint): JsonValue {
  return { kind: "i64", value: i64ToJsonValue(value) };
}
