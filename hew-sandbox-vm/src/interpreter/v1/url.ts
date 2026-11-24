import { stringValue, UNIT, type VmValue } from "../values.js";

export type UrlBridge = (request: string) => string;

export interface ParsedUrlEntry {
  readonly href: string;
  readonly scheme: string;
  readonly host: string;
  readonly port: number;
  readonly path: string;
  readonly query: string;
  readonly fragment: string;
}

type UrlResponse =
  | {
      ok: true;
      valid: true;
      href: string;
      scheme: string;
      host: string;
      port: number;
      path: string;
      query: string;
      fragment: string;
    }
  | { ok: true; valid: false }
  | { ok: false; error?: unknown };

function stringArg(args: VmValue[], index: number): string {
  const value = args[index];
  if (!value || value.kind !== "string")
    throw new TypeError("URL operation requires a string argument");
  return value.value;
}

function handleArg(args: VmValue[], index: number): bigint {
  const value = args[index];
  if (!value || value.kind !== "i64")
    throw new TypeError("URL operation requires a URL handle");
  return value.value;
}

function invokeUrl(
  bridge: UrlBridge | undefined,
  payload:
    | { operation: "parse"; input: string }
    | { operation: "join"; url: string; input: string },
): ParsedUrlEntry | null {
  if (!bridge)
    throw new Error("URL operations require the hew-wasm executeUrl bridge");
  const parsed = JSON.parse(bridge(JSON.stringify(payload))) as UrlResponse;
  if (!parsed || typeof parsed !== "object" || parsed.ok !== true) {
    const message =
      parsed && typeof parsed === "object" && typeof parsed.error === "string"
        ? parsed.error
        : "URL bridge returned an error";
    throw new Error(message);
  }
  if (parsed.valid !== true) return null;
  if (
    typeof parsed.href !== "string" ||
    typeof parsed.scheme !== "string" ||
    typeof parsed.host !== "string" ||
    typeof parsed.port !== "number" ||
    !Number.isInteger(parsed.port) ||
    typeof parsed.path !== "string" ||
    typeof parsed.query !== "string" ||
    typeof parsed.fragment !== "string"
  ) {
    throw new Error("URL bridge returned an invalid parsed payload");
  }
  return {
    href: parsed.href,
    scheme: parsed.scheme,
    host: parsed.host,
    port: parsed.port,
    path: parsed.path,
    query: parsed.query,
    fragment: parsed.fragment,
  };
}

export class UrlHandleTable {
  private nextId = 1n;
  private readonly handles = new Map<bigint, ParsedUrlEntry>();

  parse(bridge: UrlBridge | undefined, args: VmValue[]): VmValue {
    const entry = invokeUrl(bridge, {
      operation: "parse",
      input: stringArg(args, 0),
    });
    if (!entry) return { kind: "i64", value: 0n };
    const id = this.nextId++;
    this.handles.set(id, entry);
    return { kind: "i64", value: id };
  }

  isValid(args: VmValue[]): VmValue {
    const id = handleArg(args, 0);
    return { kind: "bool", value: id !== 0n && this.handles.has(id) };
  }

  component(
    args: VmValue[],
    field: Exclude<keyof ParsedUrlEntry, "port">,
  ): VmValue {
    const entry = this.handles.get(handleArg(args, 0));
    return stringValue(entry ? entry[field] : "");
  }

  port(args: VmValue[]): VmValue {
    const entry = this.handles.get(handleArg(args, 0));
    return { kind: "i64", value: BigInt(entry ? entry.port : -1) };
  }

  join(bridge: UrlBridge | undefined, args: VmValue[]): VmValue {
    const base = this.handles.get(handleArg(args, 0));
    if (!base) return { kind: "i64", value: 0n };
    const entry = invokeUrl(bridge, {
      operation: "join",
      url: base.href,
      input: stringArg(args, 1),
    });
    if (!entry) return { kind: "i64", value: 0n };
    const id = this.nextId++;
    this.handles.set(id, entry);
    return { kind: "i64", value: id };
  }

  free(args: VmValue[]): VmValue {
    const id = handleArg(args, 0);
    if (id !== 0n) this.handles.delete(id);
    return UNIT;
  }
}
