import {
  cloneValue,
  compareUtf8,
  stringValue,
  UNIT,
  type VmValue,
} from "../values.js";
import type { EncodingOp } from "./instructions.generated.js";
import type { ShimHost } from "./shims.js";

/** Lossless positional bridge to serde_json::Value. Null denotes an invalid
 * handle; a live JSON null is ["null"]. Numeric text retains native kinds. */
export type JsonNode =
  | ["null"]
  | ["bool", boolean]
  | ["integer" | "float" | "string", string]
  | ["array", JsonNode[]]
  | ["object", [string, JsonNode][]];

type JsonOwner = Extract<VmValue, { kind: "json" }>;
const owner = (value: JsonNode | null): JsonOwner => ({ kind: "json", value });
const integer = (value: number | bigint): VmValue => ({
  kind: "i64",
  value: BigInt(value),
});
function value(arg: VmValue | undefined): JsonOwner {
  if (arg?.kind !== "json")
    throw new TypeError("JSON operation requires a JSON value");
  return arg;
}
function text(arg: VmValue | undefined): string {
  if (arg?.kind !== "string")
    throw new TypeError("JSON operation requires a string");
  return arg.value;
}
function number(arg: VmValue | undefined): bigint {
  if (arg?.kind !== "i64")
    throw new TypeError("JSON operation requires an integer");
  return arg.value;
}
function copy(node: JsonNode | null): JsonOwner {
  return cloneValue(owner(node)) as JsonOwner;
}
function equal(left: JsonNode | null, right: JsonNode | null): boolean {
  if (!left || !right || left[0] !== right[0]) return false;
  switch (left[0]) {
    case "null":
      return true;
    case "float":
      return Number(left[1]) === Number(right[1]);
    case "integer":
      return BigInt(left[1]) === BigInt(right[1]! as string);
    case "string":
    case "bool":
      return left[1] === right[1];
    case "array": {
      const items = right[1] as JsonNode[];
      return (
        left[1].length === items.length &&
        left[1].every((item, i) => equal(item, items[i]!))
      );
    }
    case "object": {
      const entries = new Map(right[1] as [string, JsonNode][]);
      return (
        left[1].length === entries.size &&
        left[1].every(([key, child]) => equal(child, entries.get(key) ?? null))
      );
    }
  }
}

export function executeJson(
  host: ShimHost,
  operation: EncodingOp,
  args: VmValue[],
): VmValue {
  if (operation === "Parse" || operation === "Stringify") {
    if (!host.codecBridge)
      throw new Error(
        "JSON text operations require the hew-wasm executeCodec bridge",
      );
    const result = JSON.parse(
      host.codecBridge(
        JSON.stringify({
          json_value:
            operation === "Parse"
              ? { operation: "parse", text: text(args[0]) }
              : { operation: "stringify", value: value(args[0]).value },
        }),
      ),
    ) as {
      value?: JsonNode;
      text?: string;
      error?: string;
      failure?: string;
    };
    if (result.failure) throw new Error(result.failure);
    host.jsonError!(result.error ?? "");
    return operation === "Parse"
      ? owner(result.value ?? null)
      : stringValue(result.text ?? "");
  }
  switch (operation) {
    case "LastError":
      return stringValue(host.jsonError!());
    case "ObjectNew":
      return owner(["object", []]);
    case "ArrayNew":
      return owner(["array", []]);
    case "FromNull":
      return owner(["null"]);
    case "FromString":
      return owner(["string", text(args[0])]);
    case "FromBool":
      return owner(["bool", number(args[0]) !== 0n]);
    case "FromInt":
      return owner(["integer", BigInt.asIntN(64, number(args[0])).toString()]);
    case "FromU64":
      return owner(["integer", BigInt.asUintN(64, number(args[0])).toString()]);
    case "FromFloat": {
      const arg = args[0];
      if (arg?.kind !== "f64")
        throw new TypeError("JSON operation requires a float");
      return owner(
        Number.isFinite(arg.value)
          ? ["float", Object.is(arg.value, -0) ? "-0" : String(arg.value)]
          : null,
      );
    }
    case "Eq":
      return integer(equal(value(args[0]).value, value(args[1]).value) ? 1 : 0);
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
  switch (operation) {
    case "Type":
      return integer(
        node
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
      return integer(node?.[0] === "bool" && node[1] ? 1 : 0);
    case "GetInt":
      return integer(
        node?.[0] === "integer" && BigInt(node[1]) <= (1n << 63n) - 1n
          ? BigInt(node[1])
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
          node?.[0] === "float" || node?.[0] === "integer"
            ? Number(node[1])
            : 0,
      };
    case "GetString":
      return stringValue(node?.[0] === "string" ? node[1] : "");
    case "ArrayLen":
      return integer(node?.[0] === "array" ? node[1].length : -1);
    case "GetField":
      return copy(
        node?.[0] === "object"
          ? (node[1].find(([key]) => key === text(args[1]))?.[1] ?? null)
          : null,
      );
    case "ArrayGet":
      return copy(
        node?.[0] === "array"
          ? (node[1][Number(BigInt.asIntN(32, number(args[1])))] ?? null)
          : null,
      );
    case "ObjectKeys":
      return owner(
        node?.[0] === "object"
          ? ["array", node[1].map(([key]) => ["string", key])]
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
      child.value = null;
      return receiver;
    }
    case "ArrayPush": {
      const child = value(args[1]);
      if (node?.[0] === "array" && child.value) node[1].push(child.value);
      child.value = null;
      return receiver;
    }
    case "GetDatetime":
      throw new Error("JSON has no datetime operation");
  }
}
