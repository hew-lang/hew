import {
  cloneValue,
  compareUtf8,
  stringValue,
  UNIT,
  type VmValue,
} from "../values.js";
import type { EncodingOp, EncodingFormat } from "./instructions.generated.js";
import type { ShimHost } from "./shims.js";

/** Lossless positional bridge to each format's native Value. Null denotes an invalid
 * handle; a live JSON or YAML null is ["null"]. Numeric text retains native kinds. */
export type { EncodingFormat } from "./instructions.generated.js";
export type EncodingNode =
  | ["null"]
  | ["bool", boolean]
  | ["integer" | "float" | "string", string]
  | ["array", EncodingNode[]]
  | ["object", [string, EncodingNode][]]
  | ["mapping", [EncodingNode, EncodingNode][]]
  | ["tagged", string, EncodingNode]
  | ["datetime", string];

type EncodingOwner = Extract<VmValue, { kind: "encoding" }>;
const owner = (
  format: EncodingFormat,
  value: EncodingNode | null,
): EncodingOwner => ({ kind: "encoding", format, value });
const integer = (value: number | bigint): VmValue => ({
  kind: "i64",
  value: BigInt(value),
});
function checkedValue(
  arg: VmValue | undefined,
  format: EncodingFormat,
): EncodingOwner {
  if (arg?.kind !== "encoding" || arg.format !== format)
    throw new TypeError("encoding operation requires a matching format value");
  return arg;
}
function text(arg: VmValue | undefined): string {
  if (arg?.kind !== "string")
    throw new TypeError("encoding operation requires a string");
  return arg.value;
}
function number(arg: VmValue | undefined): bigint {
  if (arg?.kind !== "i64")
    throw new TypeError("encoding operation requires an integer");
  return arg.value;
}
function equal(
  format: EncodingFormat,
  left: EncodingNode | null,
  right: EncodingNode | null,
): boolean {
  if (!left || !right || left[0] !== right[0]) return false;
  switch (left[0]) {
    case "null":
      return true;
    case "float":
      return (
        float(left[1]) === float(right[1] as string) ||
        (format === "Yaml" &&
          Number.isNaN(float(left[1])) &&
          Number.isNaN(float(right[1] as string)))
      );
    case "integer":
      return BigInt(left[1]) === BigInt(right[1]! as string);
    case "datetime":
    case "string":
    case "bool":
      return left[1] === right[1];
    case "array": {
      const items = right[1] as EncodingNode[];
      return (
        left[1].length === items.length &&
        left[1].every((item, i) => equal(format, item, items[i]!))
      );
    }
    case "tagged":
      return (
        left[1] === right[1] && equal(format, left[2], right[2] as EncodingNode)
      );
    case "mapping": {
      const entries = right[1] as [EncodingNode, EncodingNode][];
      return (
        left[1].length === entries.length &&
        left[1].every(([key, child]) => {
          const entry = entries.find(([candidate]) =>
            equal(format, key, candidate),
          );
          return entry !== undefined && equal(format, child, entry[1]);
        })
      );
    }
    case "object": {
      const entries = new Map(right[1] as [string, EncodingNode][]);
      return (
        left[1].length === entries.size &&
        left[1].every(([key, child]) =>
          equal(format, child, entries.get(key) ?? null),
        )
      );
    }
  }
}

export function executeEncoding(
  host: ShimHost,
  format: EncodingFormat,
  operation: EncodingOp,
  args: VmValue[],
): VmValue {
  const owned = (node: EncodingNode | null) => owner(format, node);
  const value = (arg: VmValue | undefined) => checkedValue(arg, format);
  const copy = (node: EncodingNode | null) => cloneValue(owned(node));
  if (operation === "Parse" || operation === "Stringify") {
    if (!host.codecBridge)
      throw new Error(
        "Encoding text operations require the hew-wasm executeCodec bridge",
      );
    const result = JSON.parse(
      host.codecBridge(
        JSON.stringify({
          encoding_value:
            operation === "Parse"
              ? { format, operation: "parse", text: text(args[0]) }
              : { format, operation: "stringify", value: value(args[0]).value },
        }),
      ),
    ) as {
      value?: EncodingNode;
      text?: string;
      error?: string;
      failure?: string;
    };
    if (result.failure) throw new Error(result.failure);
    host.encodingError!(format, result.error ?? "");
    return operation === "Parse"
      ? owned(result.value ?? null)
      : stringValue(result.text ?? "");
  }
  switch (operation) {
    case "LastError":
      return stringValue(host.encodingError!(format));
    case "ObjectNew":
      return owned(format === "Yaml" ? ["mapping", []] : ["object", []]);
    case "ArrayNew":
      return owned(["array", []]);
    case "FromNull":
      return owned(["null"]);
    case "FromString":
      return owned(["string", text(args[0])]);
    case "FromBool":
      return owned(["bool", number(args[0]) !== 0n]);
    case "FromInt":
      return owned(["integer", BigInt.asIntN(64, number(args[0])).toString()]);
    case "FromU64":
      return owned(["integer", BigInt.asUintN(64, number(args[0])).toString()]);
    case "FromFloat": {
      const arg = args[0];
      if (arg?.kind !== "f64")
        throw new TypeError("encoding operation requires a float");
      return owned(
        format !== "Json" || Number.isFinite(arg.value)
          ? ["float", Object.is(arg.value, -0) ? "-0" : String(arg.value)]
          : null,
      );
    }
    case "Eq":
      return integer(
        equal(format, value(args[0]).value, value(args[1]).value) ? 1 : 0,
      );
    case "Clone":
      return copy(value(args[0]).value);
    case "Free":
      value(args[0]).value = null;
      return UNIT;
    default:
      break;
  }
  const receiver = value(args[0]),
    node = receiver.value;
  let scalar = node;
  while (scalar?.[0] === "tagged") scalar = scalar[2];
  switch (operation) {
    case "Type":
      return integer(
        node?.[0] === "tagged" || node?.[0] === "datetime"
          ? 7
          : node?.[0] === "mapping"
            ? 6
            : node
              ? [
                  "null",
                  "bool",
                  "integer",
                  "float",
                  "string",
                  "array",
                  "object",
                ].indexOf(node[0])
              : -1,
      );
    case "IntStatus":
      return integer(
        node?.[0] === "integer"
          ? BigInt(node[1]) <= (1n << 63n) - 1n
            ? 1
            : 0
          : -1,
      );
    case "GetBool":
      return integer(scalar?.[0] === "bool" && scalar[1] ? 1 : 0);
    case "GetInt":
      return integer(
        scalar?.[0] === "integer" && BigInt(scalar[1]) <= (1n << 63n) - 1n
          ? BigInt(scalar[1])
          : 0,
      );
    case "GetU64":
      return integer(
        node?.[0] === "integer" && BigInt(node[1]) >= 0 ? BigInt(node[1]) : 0,
      );
    case "GetFloat":
      return {
        kind: "f64",
        value:
          scalar?.[0] === "float" || scalar?.[0] === "integer"
            ? float(scalar[1])
            : 0,
      };
    case "GetString":
      return stringValue(scalar?.[0] === "string" ? scalar[1] : "");
    case "ArrayLen":
      return integer(node?.[0] === "array" ? node[1].length : -1);
    case "GetDatetime":
      return stringValue(node?.[0] === "datetime" ? node[1] : "");
    case "GetField":
      return copy(
        node?.[0] === "object"
          ? (node[1].find(([key]) => key === text(args[1]))?.[1] ?? null)
          : node?.[0] === "mapping"
            ? (node[1].find(
                ([key]) => key[0] === "string" && key[1] === text(args[1]),
              )?.[1] ?? null)
            : null,
      );
    case "ArrayGet":
      return copy(
        node?.[0] === "array"
          ? (node[1][Number(BigInt.asIntN(32, number(args[1])))] ?? null)
          : null,
      );
    case "ObjectKeys":
      return owned(
        node?.[0] === "object"
          ? ["array", node[1].map(([key]) => ["string", key])]
          : node?.[0] === "mapping"
            ? ["array", node[1].map(([key]) => structuredClone(key))]
            : null,
      );
    case "ObjectSet": {
      const child = value(args[2]),
        key = text(args[1]);
      if (node?.[0] === "object" && child.value) {
        const index = node[1].findIndex(([name]) => name === key);
        if (index < 0) {
          node[1].push([key, child.value]);
          node[1].sort(([a], [b]) => compareUtf8(a, b));
        } else node[1][index] = [key, child.value];
      }
      if (node?.[0] === "mapping" && child.value) {
        const index = node[1].findIndex(
          ([name]) => name[0] === "string" && name[1] === key,
        );
        if (index < 0) node[1].push([["string", key], child.value]);
        else node[1][index] = [["string", key], child.value];
      }
      child.value = null;
      return receiver;
    }
    case "ArrayPush": {
      const child = value(args[1]);
      if (node?.[0] === "array" && child.value) node[1].push(child.value);
      child.value = null;
      return receiver;
    }
  }
}

function float(text: string): number {
  return text === "inf" || text === "Infinity"
    ? Infinity
    : text === "-inf" || text === "-Infinity"
      ? -Infinity
      : Number(text);
}
