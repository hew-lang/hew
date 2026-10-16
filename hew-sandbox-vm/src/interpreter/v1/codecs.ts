import { UNIT, stringValue, type VmValue } from "../values.js";
import { runtimeTag, type PackageV1, type TermV1 } from "./package.js";
import { collectionOperation, type ValueProgram } from "./value-operations.js";

type Key = { name: string; tag: number | null; flags: number };
type Node =
  | string
  | { Float: { bits: number } }
  | { Signed: { min: number; max: number } }
  | { Unsigned: { max: number } }
  | { Tuple: number[] }
  | { Array: { element: number; len: number } }
  | { Sequence: number }
  | { Set: number }
  | { Map: { key: number; value: number } }
  | { Record: [Key, number][] }
  | { Variant: [Key, number | null][] }
  | { Option: number };
interface Shape {
  root: number;
  nodes: Node[];
  names: string[];
  aggregates: (number | null)[];
  variants: (number | null)[];
  grouped_payloads: boolean[];
}
type Bridge =
  | null
  | boolean
  | number
  | string
  | Bridge[]
  | { decode_error: true };
class DecodeFailure extends Error {}
export type CodecBridge = (request: string) => string;
class Duplicate extends Error {
  constructor(readonly detail: Bridge) {
    super("duplicate decoded key");
  }
}

type CodecTerm = Extract<TermV1, { op: "wire.codec" }>;

function array(value: Bridge): Bridge[] {
  if (!Array.isArray(value))
    throw new Error("codec bridge returned a non-positional value");
  return value;
}
function fields(value: VmValue): VmValue[] {
  if (value.kind === "record") return value.fields;
  if (value.kind === "vector") return value.items;
  throw new Error(`codec aggregate received ${value.kind}`);
}
function toBridge(
  pkg: PackageV1,
  shape: Shape,
  id: number,
  value: VmValue,
): Bridge {
  const node = shape.nodes[id]!;
  if (node === "Unit") return null;
  if (node === "Bool" && value.kind === "bool") return value.value;
  if ((node === "Char" || node === "String") && value.kind === "string")
    return value.value;

  if (node === "Bytes")
    return fields(value).map((byte) => {
      if (byte.kind !== "i64") throw new Error("invalid byte");
      return Number(byte.value);
    });
  if (typeof node === "string") throw new Error(`invalid codec scalar ${node}`);
  if ("Float" in node) {
    if (value.kind !== "f64") throw new Error("codec float has wrong VM kind");
    const number = value.value;
    return Object.is(number, -0) ? "-0" : String(number);
  }
  if ("Signed" in node || "Unsigned" in node) {
    if (value.kind !== "i64")
      throw new Error("codec integer has wrong VM kind");
    return (
      "Unsigned" in node ? BigInt.asUintN(64, value.value) : value.value
    ).toString();
  }
  if ("Tuple" in node)
    return fields(value).map((field, i) =>
      toBridge(pkg, shape, node.Tuple[i]!, field),
    );
  if ("Record" in node)
    return fields(value).map((field, i) =>
      toBridge(pkg, shape, node.Record[i]![1], field),
    );
  if ("Array" in node || "Sequence" in node) {
    const child = "Array" in node ? node.Array.element : node.Sequence;
    return fields(value).map((item) => toBridge(pkg, shape, child, item));
  }
  if ("Option" in node) {
    if (value.kind !== "enum") throw new Error("codec Option is not a variant");
    return value.tag === runtimeTag(pkg.variants[value.shape]!, "OptionNone")
      ? null
      : [toBridge(pkg, shape, node.Option, value.payload[0]!)];
  }
  if ("Variant" in node) {
    if (value.kind !== "enum") throw new Error("codec enum is not a variant");
    const child = node.Variant[value.tag]![1];
    if (child === null) return [value.tag, null];
    const payload = shape.grouped_payloads[child]
      ? { kind: "record" as const, typeId: "", fields: value.payload }
      : value.payload[0]!;
    return [value.tag, toBridge(pkg, shape, child, payload)];
  }
  if (value.kind !== "map") throw new Error("codec collection is not a map");
  if ("Set" in node)
    return [...value.entries.values()].map((entry) => [
      toBridge(pkg, shape, node.Set, entry.key),
      null,
    ]);
  return [...value.entries.values()].map((entry) => [
    toBridge(pkg, shape, node.Map.key, entry.key),
    toBridge(pkg, shape, node.Map.value, entry.value),
  ]);
}

function* fromBridge(
  pkg: PackageV1,
  shape: Shape,
  id: number,
  value: Bridge,
  context: { callbacks: Record<string, number[]>; propertyInput: boolean },
): ValueProgram {
  if (
    value !== null &&
    typeof value === "object" &&
    !Array.isArray(value) &&
    value.decode_error
  )
    throw new DecodeFailure();
  const node = shape.nodes[id]!;
  const name = shape.names[id] ?? "";
  const aggregate = shape.aggregates[id] ?? undefined;
  const variant = shape.variants[id];
  if (node === "Unit") return UNIT;
  if (node === "Bool") return { kind: "bool", value: Boolean(value) };
  if (node === "Char" || node === "String") return stringValue(String(value));

  if (node === "Bytes")
    return {
      kind: "vector",
      elementType: "u8",
      items: array(value).map((byte) => ({
        kind: "i64",
        value: BigInt(String(byte)),
      })),
    };
  if (typeof node === "string") throw new Error(`invalid codec scalar ${node}`);
  if ("Float" in node) {
    const number =
      value === "inf" ? Infinity : value === "-inf" ? -Infinity : Number(value);
    return { kind: "f64", value: number };
  }
  if ("Signed" in node || "Unsigned" in node)
    return { kind: "i64", value: BigInt(String(value)) };
  if ("Option" in node) {
    if (variant === null || variant === undefined)
      throw new Error("codec Option lacks its shape");
    return {
      kind: "enum",
      typeId: name,
      shape: variant,
      tag: runtimeTag(
        pkg.variants[variant]!,
        value === null ? "OptionNone" : "OptionSome",
      ),
      payload:
        value === null
          ? []
          : [
              yield* fromBridge(
                pkg,
                shape,
                node.Option,
                array(value)[0]!,
                context,
              ),
            ],
    };
  }
  if ("Variant" in node) {
    if (variant === null || variant === undefined)
      throw new Error("codec enum lacks its shape");
    const pair = array(value),
      tag = Number(pair[0]),
      child = node.Variant[tag]![1];
    let payload: VmValue[] = [];
    if (child !== null) {
      const decoded = yield* fromBridge(pkg, shape, child, pair[1]!, context);
      payload = shape.grouped_payloads[child] ? fields(decoded) : [decoded];
    }
    return { kind: "enum", typeId: name, shape: variant, tag, payload };
  }
  if ("Map" in node || "Set" in node) {
    const family = "Map" in node ? "Map" : "Set";
    const map = yield* collectionOperation(pkg, family, "New", [], [], null);
    for (const item of array(value)) {
      const itemFields = array(item);
      const pair = "Map" in node ? itemFields : [itemFields[0]!, null];
      const key = yield* fromBridge(
        pkg,
        shape,
        "Map" in node ? node.Map.key : node.Set,
        pair[0]!,
        context,
      );
      const member =
        "Map" in node
          ? yield* fromBridge(pkg, shape, node.Map.value, pair[1]!, context)
          : UNIT;
      if (map.kind !== "map")
        throw new Error("codec collection has no map storage");
      const size = map.entries.size;
      yield* collectionOperation(
        pkg,
        family,
        "Insert",
        [map, key, member],
        context.callbacks[id] ?? [],
        null,
      );
      if (!context.propertyInput && map.entries.size === size)
        throw new Duplicate(itemFields["Map" in node ? 2 : 1]!);
    }
    return map;
  }
  const values: VmValue[] = [];
  for (const [i, item] of array(value).entries()) {
    const child =
      "Tuple" in node
        ? node.Tuple[i]!
        : "Record" in node
          ? node.Record[i]![1]
          : "Array" in node
            ? node.Array.element
            : node.Sequence;
    values.push(yield* fromBridge(pkg, shape, child, item, context));
  }
  return "Array" in node || "Sequence" in node
    ? { kind: "vector", elementType: "", items: values }
    : { kind: "record", typeId: name, shape: aggregate, fields: values };
}

export function* executeCodec(
  pkg: PackageV1,
  term: CodecTerm,
  input: VmValue,
  bridge: CodecBridge | undefined,
  propertyInput: number[],
): ValueProgram {
  if (!bridge)
    throw new Error("typed codecs require the hew-wasm executeCodec bridge");
  const encoded = pkg.codecs?.[term.codec];
  if (encoded === undefined) throw new Error("codec shape is absent");
  const shape = JSON.parse(encoded) as Shape;
  const request = {
    shape: encoded,
    format: term.format,
    direction: term.direction,
    error_root: term.decoded?.error_root,
    ...(term.direction === "encode"
      ? { value: toBridge(pkg, shape, shape.root, input) }
      : term.direction === "test_input"
        ? {
            input: propertyInput,
            ordinal: input.kind === "i64" ? Number(input.value) : -1,
          }
        : {
            input:
              input.kind === "string"
                ? [...new TextEncoder().encode(input.value)]
                : fields(input).map((byte) =>
                    byte.kind === "i64" ? Number(byte.value) : -1,
                  ),
          }),
  };
  let result = JSON.parse(bridge(JSON.stringify(request))) as {
    ok: boolean;
    value: Bridge;
    error?: Bridge;
    failure?: string;
  };
  if (result.failure !== undefined) throw new Error(result.failure);
  if (term.direction === "encode") {
    const bytes = array(result.value).map(Number);
    return term.format === 0 || term.format === 4
      ? {
          kind: "vector",
          elementType: "u8",
          items: bytes.map((byte) => ({ kind: "i64", value: BigInt(byte) })),
        }
      : stringValue(
          new TextDecoder("utf-8", {
            fatal: true,
            ignoreBOM: true,
          }).decode(new Uint8Array(bytes)),
        );
  }
  const root = result.ok ? shape.root : term.decoded!.error_root;
  const context = {
    callbacks: term.callbacks,
    propertyInput: term.direction === "test_input",
  };
  let decoded: VmValue;
  try {
    decoded = yield* fromBridge(pkg, shape, root, result.value, context);
  } catch (error) {
    if (!term.decoded) throw error;
    if (error instanceof DecodeFailure && result.error !== undefined) {
      result = { ok: false, value: result.error };
    } else if (error instanceof Duplicate) {
      result = JSON.parse(
        bridge(
          JSON.stringify({
            ...request,
            direction: "error",
            input: [],
            duplicate: error.detail,
          }),
        ),
      );
    } else {
      throw error;
    }
    if (result.failure !== undefined) throw new Error(result.failure);
    decoded = yield* fromBridge(
      pkg,
      shape,
      term.decoded.error_root,
      result.value,
      context,
    );
  }
  if (!term.decoded) return decoded;
  return {
    kind: "enum",
    typeId: pkg.variants[term.decoded.shape]!.name,
    shape: term.decoded.shape,
    tag: result.ok ? term.decoded.ok : term.decoded.error,
    payload: [decoded],
  };
}
