import { UNIT, cloneValue, type VmValue } from "../values.js";
import type { PackageV1 } from "./package.js";
import { ShimFault } from "./shims.js";

export type ValueRequest =
  | { kind: "call"; callee: number; args: VmValue[] }
  | { kind: "release"; value: VmValue; onFault?: VmValue };
export type ValueProgram = Generator<ValueRequest, VmValue, VmValue>;

const OFFSET = 0xcbf29ce484222325n;
const PRIME = 0x100000001b3n;
const integer = (value: bigint): VmValue => ({
  kind: "i64",
  value: BigInt.asIntN(64, value),
});
const boolean = (value: boolean): VmValue => ({ kind: "bool", value });
const tuple = (...fields: VmValue[]): VmValue => ({
  kind: "record",
  typeId: "",
  fields,
});
const mix = (state: bigint, value: bigint) =>
  BigInt.asUintN(64, (state ^ value) * PRIME);

function bytesHash(bytes: Iterable<number>): bigint {
  let state = OFFSET;
  for (const byte of bytes) state = mix(state, BigInt(byte));
  return state;
}

function bits(value: VmValue, ty: string): bigint {
  switch (value.kind) {
    case "unit":
      return 0n;
    case "bool":
      return value.value ? 1n : 0n;
    case "i64":
      return BigInt.asUintN(
        Number(ty.match(/^[iu](8|16|32|64)$/)?.[1] ?? 64),
        value.value,
      );
    case "string":
      return BigInt(value.value.codePointAt(0) ?? 0);
    case "f64": {
      const view = new DataView(new ArrayBuffer(8));
      if (ty === "f32") {
        view.setFloat32(0, value.value, true);
        return BigInt(view.getUint32(0, true));
      }
      view.setFloat64(0, value.value, true);
      return view.getBigUint64(0, true);
    }
    default:
      throw new Error(`selected scalar capability received ${value.kind}`);
  }
}

/** Execute the exact checked plan, including every derived component selection.
 * A user method yields to an ordinary VM frame and can suspend or fail. */
export function* selectedValue(
  pkg: PackageV1,
  id: number,
  args: VmValue[],
): ValueProgram {
  const plan = pkg.value_capabilities[id];
  if (!plan) throw new Error(`selected value plan ${id} is missing`);
  if (plan.callable !== undefined)
    return yield { kind: "call", callee: plan.callable, args };
  const [left, right] = args;
  if (!left) throw new Error("selected value operation has no receiver");
  const hash = plan.capability === "Hash";
  if (plan.ty === "string") {
    if (left.kind !== "string")
      throw new Error("string capability has no string");
    return hash
      ? integer(bytesHash(new TextEncoder().encode(left.value)))
      : boolean(right?.kind === "string" && left.value === right.value);
  }
  if (plan.ty === "bytes") {
    if (left.kind !== "vector")
      throw new Error("bytes capability has no bytes");
    const bytes = left.items.map((value) => Number(bits(value, "u8")));
    return hash
      ? integer(bytesHash(bytes))
      : boolean(
          right?.kind === "vector" &&
            bytes.length === right.items.length &&
            bytes.every(
              (value, i) => BigInt(value) === bits(right.items[i]!, "u8"),
            ),
        );
  }
  const components = plan.components ?? [];
  let fields: VmValue[];
  let others: VmValue[] = [];
  let selections: number[];
  if (left.kind === "record") {
    fields = left.fields;
    selections = components;
    if (!hash) {
      if (right?.kind !== "record" || right.fields.length !== fields.length)
        return boolean(false);
      others = right.fields;
    }
  } else if (left.kind === "enum") {
    if (hash)
      throw new Error(
        "derived enum Hash is outside the checked capability domain",
      );
    if (right?.kind !== "enum" || left.tag !== right.tag) return boolean(false);
    fields = left.payload;
    others = right.payload;
    selections = plan.variants?.[left.tag] ?? [];
  } else if (left.kind === "vector") {
    if (hash)
      throw new Error(
        "derived vector Hash is outside the checked capability domain",
      );
    if (right?.kind !== "vector" || left.items.length !== right.items.length)
      return boolean(false);
    fields = left.items;
    others = right.items;
    selections = fields.map(() => components[0]!);
  } else {
    return hash
      ? integer(bits(left, plan.ty))
      : boolean(!!right && bits(left, plan.ty) === bits(right, plan.ty));
  }
  let state = OFFSET;
  for (const [at, field] of fields.entries()) {
    const result = yield* selectedValue(
      pkg,
      selections[at]!,
      hash ? [field] : [field, others[at]!],
    );
    if (hash) {
      if (result.kind !== "i64")
        throw new Error("Hash returned a non-integer value");
      state = mix(state, result.value);
    } else {
      if (result.kind !== "bool")
        throw new Error("Eq returned a non-boolean value");
      if (!result.value) return boolean(false);
    }
  }
  return hash ? integer(state) : boolean(true);
}

type MapValue = Extract<VmValue, { kind: "map" }>;
type Entry = { key: VmValue; value: VmValue };
function ordered(entries: Map<string, Entry>): Map<string, Entry> {
  return new Map([...entries].sort(([a], [b]) => Number(a) - Number(b)));
}

function* probe(
  pkg: PackageV1,
  table: MapValue,
  key: VmValue,
  callbacks: number[],
): Generator<ValueRequest, { slot: string; found: boolean }, VmValue> {
  const hash = yield* selectedValue(pkg, callbacks[0]!, [key]);
  if (hash.kind !== "i64") throw new Error("Hash returned a non-integer value");
  const capacity = table.capacity ?? 16;
  const start = Number(BigInt.asUintN(64, hash.value) & BigInt(capacity - 1));
  let vacant: number | null = null;
  for (let step = 0; step < capacity; step++) {
    const slot = (start + step) % capacity;
    const entry = table.entries.get(String(slot));
    if (entry) {
      const equal = yield* selectedValue(pkg, callbacks[1]!, [entry.key, key]);
      if (equal.kind !== "bool")
        throw new Error("Eq returned a non-boolean value");
      if (equal.value) return { slot: String(slot), found: true };
    } else if (table.tombstones?.has(slot)) vacant ??= slot;
    else return { slot: String(vacant ?? slot), found: false };
  }
  if (vacant !== null) return { slot: String(vacant), found: false };
  throw new Error("collection has no empty slot");
}

function option(
  pkg: PackageV1,
  shape: number | null | undefined,
  value?: VmValue,
): VmValue {
  const descriptor = shape == null ? undefined : pkg.variants[shape];
  if (!descriptor)
    throw new Error("collection result lacks its checked Option shape");
  const name = value === undefined ? "None" : "Some";
  return {
    kind: "enum",
    typeId: descriptor.name,
    tag: descriptor.cases.findIndex((entry) => entry.name === name),
    payload: value === undefined ? [] : [value],
  };
}

export function* sharedOperation(
  operation: string,
  args: VmValue[],
): ValueProgram {
  if (operation === "RcNew")
    return { kind: "rc", cell: { value: args[0]!, refs: 1 }, closed: false };
  const receiver = args[0];
  if (receiver?.kind !== "rc" || receiver.closed)
    throw new Error("shared operation has no live Rc");
  switch (operation) {
    case "RcClone":
      return cloneValue(receiver);
    case "RcGet":
      return cloneValue(receiver.cell.value);
    case "RcIsUnique":
      return boolean(receiver.cell.refs === 1);
    case "RcStrongCount":
      return integer(BigInt(receiver.cell.refs));
    case "RcWeakCount":
      return integer(0n);
    case "RcSet": {
      const old = receiver.cell.value;
      receiver.cell.value = args[1]!;
      yield { kind: "release", value: old };
      return UNIT;
    }
    default:
      throw new Error(`shared operation ${operation} has no executor`);
  }
}

/** Open-addressed collection operations preserve native callback ordering:
 * growth hashes occupied slots before probing the incoming key. Staged slots
 * borrow their owners until every fallible callback has completed. */
export function* collectionOperation(
  pkg: PackageV1,
  family: string,
  operation: string,
  args: VmValue[],
  callbacks: number[],
  shape: number | null | undefined,
): ValueProgram {
  if (family === "Vector") {
    const receiver = args[0];
    if (receiver?.kind !== "vector")
      throw new Error("vector operation has no vector");
    if (operation === "Clear") {
      const items = receiver.items.splice(0);
      yield {
        kind: "release",
        value: { kind: "vector", elementType: receiver.elementType, items },
      };
      return receiver;
    }
    if (operation === "Set") {
      const index = args[1];
      if (index?.kind !== "i64")
        throw new Error("vector mutation has no index");
      const at = Number(index.value);
      if (at < 0 || at >= receiver.items.length)
        throw new ShimFault("vector_bounds", "vector index out of bounds");
      const old = receiver.items[at]!;
      receiver.items[at] = args[2]!;
      yield { kind: "release", value: old };
      return receiver;
    }
    for (const item of receiver.items) {
      const equal = yield* selectedValue(pkg, callbacks[0]!, [item, args[1]!]);
      if (equal.kind !== "bool")
        throw new Error("Eq returned a non-boolean value");
      if (equal.value) return boolean(true);
    }
    return boolean(false);
  }
  if (operation === "New")
    return {
      kind: "map",
      entries: new Map(),
      capacity: 16,
      tombstones: new Set(),
    };
  const receiver = args[0];
  if (receiver?.kind !== "map")
    throw new Error("collection operation has no collection");
  if (operation === "Len") return integer(BigInt(receiver.entries.size));
  if (operation === "Clear") {
    const entries = [...receiver.entries.values()];
    receiver.entries.clear();
    receiver.tombstones = new Set();
    yield {
      kind: "release",
      value: {
        kind: "vector",
        elementType: "",
        items: entries.flatMap((entry) => [entry.key, entry.value]),
      },
    };
    return receiver;
  }
  if (["Keys", "Values", "Elements", "Entries"].includes(operation)) {
    return {
      kind: "vector",
      elementType: "",
      items: [...receiver.entries.values()].map(({ key, value }) =>
        operation === "Values"
          ? cloneValue(value)
          : operation === "Entries"
            ? tuple(cloneValue(key), cloneValue(value))
            : cloneValue(key),
      ),
    };
  }
  let staged = receiver;
  if (
    operation === "Insert" &&
    (receiver.entries.size + 1) * 100 >= (receiver.capacity ?? 16) * 75
  ) {
    const capacity = (receiver.capacity ?? 16) * 2;
    staged = {
      kind: "map",
      entries: new Map(),
      capacity,
      tombstones: new Set(),
    };
    for (const entry of ordered(receiver.entries).values()) {
      const hash = yield* selectedValue(pkg, callbacks[0]!, [entry.key]);
      if (hash.kind !== "i64")
        throw new Error("Hash returned a non-integer value");
      let slot = Number(BigInt.asUintN(64, hash.value) & BigInt(capacity - 1));
      while (staged.entries.has(String(slot))) slot = (slot + 1) % capacity;
      staged.entries.set(String(slot), entry);
    }
  }
  const result = yield* probe(pkg, staged, args[1]!, callbacks);
  const entry = staged.entries.get(result.slot);
  if (operation === "Contains" || operation === "ContainsKey")
    return boolean(result.found);
  if (operation === "Get" || operation === "GetBorrow")
    return option(
      pkg,
      shape,
      entry
        ? operation === "GetBorrow"
          ? entry.value
          : cloneValue(entry.value)
        : undefined,
    );
  if (operation === "Index") {
    if (!entry) throw new ShimFault("vector_bounds", "map key not found");
    return cloneValue(entry.value);
  }
  if (operation === "Insert") {
    receiver.entries = staged.entries;
    receiver.capacity = staged.capacity;
    receiver.tombstones = staged.tombstones;
    if (family === "Set") {
      if (!result.found)
        receiver.entries.set(result.slot, { key: args[1]!, value: UNIT });
      else yield { kind: "release", value: args[1]! };
    } else {
      receiver.entries.set(result.slot, {
        key: entry?.key ?? cloneValue(args[1]!),
        value: args[2]!,
      });
      if (entry) yield { kind: "release", value: entry.value };
    }
    receiver.tombstones?.delete(Number(result.slot));
    receiver.entries = ordered(receiver.entries);
    return family === "Set"
      ? tuple(receiver, boolean(!result.found))
      : receiver;
  }
  if (operation === "Remove") {
    if (entry) {
      receiver.entries.delete(result.slot);
      (receiver.tombstones ??= new Set()).add(Number(result.slot));
      yield { kind: "release", value: entry.key, onFault: entry.value };
    }
    return tuple(
      receiver,
      family === "Set"
        ? boolean(result.found)
        : option(pkg, shape, entry?.value),
    );
  }
  throw new Error(`collection operation ${operation} has no executor`);
}
