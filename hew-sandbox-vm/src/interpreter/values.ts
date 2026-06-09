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
  | { kind: "vector"; elementType: string; items: VmValue[] };

export const UNIT: VmValue = { kind: "unit" };

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

export function renderStdout(value: VmValue): string {
  switch (value.kind) {
    case "unit":
      return "()";
    case "bool":
      return value.value ? "true" : "false";
    case "i64":
      return value.value.toString();
    case "f64":
      return String(value.value);
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
