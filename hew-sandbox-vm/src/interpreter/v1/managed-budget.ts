import { vectorStorage, type VmValue } from "../values.js";
import type { ManagedType } from "./package.js";

const U64_MAX = 0xffffffffffffffffn;
export class HeapExceeded extends Error {
  constructor() {
    super("actor heap limit exceeded");
  }
}

type Allocations = Map<object, bigint>;
export interface ManagedRoot {
  value: VmValue;
  type: number;
  slots?: boolean;
  identity?: object;
}
export interface ManagedProjection {
  identity: object;
  bytes: bigint;
  children: readonly ManagedRoot[];
}
export interface ManagedLease {
  release(): void;
}

/** Compiler-authored widths and children describe payloads; VM backings supply
 * identity and capacity. No host-object or allocator size enters this graph. */
export class ManagedLayouts {
  private readonly types = new WeakMap<object, number>();
  private readonly retired = new WeakSet<object>();

  constructor(
    readonly rows: readonly ManagedType[],
    private readonly retained?: (
      value: VmValue,
      type: number,
    ) => ManagedProjection,
  ) {}

  row(type: number): ManagedType {
    const row = this.rows[type];
    if (!row) throw new Error(`managed type ${type} is absent`);
    return row;
  }

  stamp(value: VmValue, type: number): void {
    this.types.set(value, type);
    const row = this.row(type);
    if (row.kind === "callable" && row.captures && value.kind === "closure")
      value.managedCaptures = row.captures;
    if (row.kind === "dynamic" && value.kind === "dyn")
      value.managedPayload ??= this.types.get(value.value);
    if (value.kind === "vector") {
      const storage = value.storage ?? vectorStorage(value);
      if (row.kind === "bytes") {
        storage.bytes = true;
        storage.shareOnCopy = true;
      } else if (
        row.kind === "vector" ||
        row.kind === "array" ||
        row.kind === "slice"
      ) {
        storage.shareOnCopy = row.share_on_copy;
      }
    }
  }

  width(type: number, capacity: number | bigint = 1): bigint {
    return checked(BigInt(this.row(type).slot_bytes) * BigInt(capacity));
  }

  retire(value: VmValue): void {
    this.retired.add(value);
  }

  type(value: VmValue): number {
    const type = this.types.get(value);
    if (type == null)
      throw new Error("managed payload has no compiler descriptor");
    return type;
  }

  /** Describe native copy allocations without retaining or changing inputs. */
  copy(value: VmValue, type: number): VmValue {
    const row = this.row(type);
    this.stamp(value, type);
    switch (row.kind) {
      case "scalar":
      case "handle":
      case "string":
      case "bytes":
      case "shared":
        return value;
      case "array":
      case "vector":
      case "slice": {
        if (value.kind !== "vector")
          throw new Error("managed copy has no sequence");
        const storage = vectorStorage(value);
        if (row.share_on_copy && value.items.length) return value;
        const items = value.items.map((item) => this.copy(item, row.element));
        return {
          ...value,
          items,
          storage: { ...storage, items, capacity: items.length, owners: 1 },
        };
      }
      case "aggregate":
        if (value.kind !== "record")
          throw new Error("managed copy has no fields");
        return {
          ...value,
          fields: value.fields.map((item, index) =>
            this.copy(item, row.fields[index]!),
          ),
        };
      case "variant":
        if (value.kind !== "enum")
          throw new Error("managed copy has no variant");
        return {
          ...value,
          payload: value.payload.map((item, index) =>
            this.copy(item, row.cases[value.tag]![index]!),
          ),
        };
      case "callable": {
        if (value.kind === "function") return value;
        if (value.kind !== "closure" || value.environment.kind !== "record")
          throw new Error("managed copy has no captures");
        const captures = row.captures ?? value.managedCaptures;
        if (!captures)
          throw new Error("callable has no concrete capture descriptor");
        const result: VmValue = {
          ...value,
          environment: {
            ...value.environment,
            fields: value.environment.fields.map((item, index) =>
              this.copy(item, captures[index]!),
            ),
          },
        };
        if (result.kind === "closure") result.managedCaptures = captures;
        return result;
      }
      case "map":
      case "set": {
        if (value.kind !== "map") throw new Error("managed copy has no map");
        const key = row.kind === "map" ? row.key : row.element;
        return {
          ...value,
          entries: new Map(
            [...value.entries].map(([slot, entry]) => [
              slot,
              {
                key: this.copy(entry.key, key),
                value:
                  row.kind === "map"
                    ? this.copy(entry.value, row.value)
                    : entry.value,
              },
            ]),
          ),
        };
      }
      case "dynamic":
        if (value.kind !== "dyn") throw new Error("managed copy has no box");
        return {
          ...value,
          value: this.copy(
            value.value,
            value.managedPayload ?? this.type(value.value),
          ),
        };
      case "generator":
        throw new Error("a generator cannot be copied");
      case "provider":
        if (
          row.provider === "task" ||
          row.provider === "actor_call" ||
          row.provider === "sink"
        )
          return value;
        throw new Error(
          `managed ${row.provider} copy requires its provider visitor`,
        );
    }
  }

  collect(
    root: ManagedRoot,
    allocations: Allocations,
    substitution?: { before: VmValue; after: VmValue },
    stringGrowth?: { value: VmValue; identity: object; capacity: number },
  ): void {
    const active = new Set<object>();
    const visit = (value: VmValue, type: number): void => {
      if (substitution?.before === value) value = substitution.after;
      if (this.retired.has(value)) return;
      if (value.kind === "unit") return;
      this.stamp(value, type);
      const row = this.row(type);
      const allocation = (identity: object, bytes: bigint): boolean => {
        checked(bytes);
        const prior = allocations.get(identity);
        if (prior != null && prior !== bytes)
          throw new Error("managed storage has conflicting widths");
        if (prior != null) return false;
        allocations.set(identity, bytes);
        if (active.has(identity)) return false;
        active.add(identity);
        return true;
      };
      switch (row.kind) {
        case "scalar":
        case "handle":
          return;
        case "string": {
          if (value.kind !== "string")
            throw new Error("managed string has no UTF-8 backing");
          if (stringGrowth?.value === value) {
            allocation(stringGrowth.identity, BigInt(stringGrowth.capacity));
            return;
          }
          const storage = value.managedStorage();
          if (storage.capacity)
            allocation(storage.identity, BigInt(storage.capacity));
          return;
        }
        case "bytes": {
          if (value.kind !== "vector")
            throw new Error("managed bytes have no backing");
          const storage = value.storage ?? vectorStorage(value);
          if (storage.capacity) allocation(storage, BigInt(storage.capacity));
          return;
        }
        case "vector":
        case "array":
        case "slice": {
          if (value.kind !== "vector")
            throw new Error("managed sequence has no backing");
          const storage = value.storage ?? vectorStorage(value);
          if (
            storage.capacity &&
            allocation(storage, this.width(row.element, storage.capacity))
          )
            storage.items.forEach((item) => visit(item, row.element));
          return;
        }
        case "aggregate":
          if (value.kind !== "record")
            throw new Error("managed aggregate has no fields");
          row.fields.forEach((child, index) =>
            visit(value.fields[index]!, child),
          );
          return;
        case "variant":
          if (value.kind !== "enum")
            throw new Error("managed variant has no payload");
          if (!row.indirect || allocation(value, BigInt(row.payload_bytes)))
            row.cases[value.tag]!.forEach((child, index) =>
              visit(value.payload[index]!, child),
            );
          return;
        case "map":
        case "set": {
          if (value.kind !== "map")
            throw new Error("managed map has no backing");
          const capacity = value.capacity ?? 0;
          const key = row.kind === "map" ? row.key : row.element;
          const slot =
            this.width(key) + (row.kind === "map" ? this.width(row.value) : 0n);
          if (
            capacity &&
            allocation(value.entries, checked(slot * BigInt(capacity)))
          )
            for (const entry of value.entries.values()) {
              visit(entry.key, key);
              if (row.kind === "map") visit(entry.value, row.value);
            }
          return;
        }
        case "callable": {
          if (value.kind === "function") return;
          if (value.kind !== "closure" || value.environment.kind !== "record")
            throw new Error("managed callable has no capture environment");
          const fields = value.environment.fields;
          const captures = row.captures ?? value.managedCaptures;
          if (!captures)
            throw new Error("callable has no concrete capture descriptor");
          const width = captures.reduce(
            (total, type) => checked(total + this.width(type)),
            0n,
          );
          if (allocation(value.environment, width))
            captures.forEach((type, index) => visit(fields[index]!, type));
          return;
        }
        case "shared": {
          if (value.kind !== "rc" && value.kind !== "weak")
            throw new Error("managed shared value has no cell");
          if (value.closed || !value.cell || value.cell.dying || row.weak)
            return;
          if (allocation(value.cell, this.width(row.payload)))
            visit(value.cell.value, row.payload);
          return;
        }
        case "dynamic": {
          if (value.kind !== "dyn")
            throw new Error("managed dynamic value has no box");
          const payload = value.managedPayload ?? this.types.get(value.value);
          if (payload == null)
            throw new Error(
              "managed dynamic payload has no compiler descriptor",
            );
          if (allocation(value, this.width(payload)))
            visit(value.value, payload);
          return;
        }
        case "generator":
        case "provider": {
          if (!this.retained)
            throw new Error("managed provider has no retained-value visitor");
          const graph = this.retained(value, type);
          if (allocation(graph.identity, graph.bytes))
            for (const child of graph.children) {
              if (child.slots)
                allocation(child.identity ?? child, this.width(child.type));
              visit(child.value, child.type);
            }
          return;
        }
      }
    };
    if (root.slots)
      allocations.set(root.identity ?? root, this.width(root.type));
    visit(root.value, root.type);
  }
}

/** One incarnation retains explicit ownership roots. Recomputing only a
 * limited owner's typed graph also handles mutations through projected loans:
 * aliases deduplicate the real backing identity, never a value's address. */
export class ManagedBudget {
  private readonly roots = new Set<ManagedRoot>();

  constructor(
    readonly layouts: ManagedLayouts,
    readonly limit: bigint,
  ) {}

  allocations(
    extra: readonly ManagedRoot[] = [],
    substitution?: { before: VmValue; after: VmValue },
    stringGrowth?: { value: VmValue; identity: object; capacity: number },
  ): Allocations {
    const allocations: Allocations = new Map();
    for (const root of this.roots)
      this.layouts.collect(root, allocations, substitution, stringGrowth);
    for (const root of extra) this.layouts.collect(root, allocations);
    return allocations;
  }

  get liveBytes(): bigint {
    return total(this.allocations());
  }

  check(extra: readonly ManagedRoot[]): void {
    if (total(this.allocations(extra)) > this.limit) throw new HeapExceeded();
  }

  checkPayload(bytes: bigint, children: readonly ManagedRoot[]): void {
    const allocations = this.allocations(children);
    allocations.set({}, checked(bytes));
    if (total(allocations) > this.limit) throw new HeapExceeded();
  }

  checkMutation(before: VmValue, after: VmValue): void {
    if (total(this.allocations([], { before, after })) > this.limit)
      throw new HeapExceeded();
  }

  checkStringGrowth(value: VmValue, capacity: number): void {
    if (
      total(
        this.allocations([], undefined, { value, identity: {}, capacity }),
      ) > this.limit
    )
      throw new HeapExceeded();
  }

  retain(root: ManagedRoot): ManagedLease {
    return this.retainMany([root]);
  }

  retainMany(roots: readonly ManagedRoot[]): ManagedLease {
    this.check(roots);
    roots.forEach((root) => this.roots.add(root));
    return {
      release: () => {
        roots.forEach((root) => this.roots.delete(root));
      },
    };
  }
}

function checked(bytes: bigint): bigint {
  if (bytes < 0n || bytes > U64_MAX) throw new HeapExceeded();
  return bytes;
}

function total(allocations: Allocations): bigint {
  let bytes = 0n;
  for (const width of allocations.values()) bytes = checked(bytes + width);
  return bytes;
}
