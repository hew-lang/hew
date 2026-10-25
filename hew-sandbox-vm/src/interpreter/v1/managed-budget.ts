import { vectorStorage, type VmValue } from "../values.js";
import type { ManagedType } from "./package.js";
import { cloneEncoding } from "./encoding-storage.js";

const U64_MAX = 0xffffffffffffffffn;
export class HeapExceeded extends Error {
  constructor() {
    super("actor heap limit exceeded");
  }
}

type Allocations = Map<object, bigint>;
type StorageVisitor = (
  identity: object,
  bytes: bigint,
  children: () => void,
) => void;
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
  readonly roots?: readonly ManagedRoot[];
  release(): void;
}

/** Compiler-authored widths and children describe payloads; VM backings supply
 * identity and capacity. No host-object or allocator size enters this graph. */
export class ManagedLayouts {
  private readonly types = new WeakMap<object, number>();
  private readonly retired = new WeakSet<object>();
  private readonly adoption = new Map<number, boolean>();

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
    if (
      row.kind === "callable" &&
      row.captures &&
      (value.kind === "closure" || value.kind === "record")
    )
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

  /** Propagate nested compiler descriptors before an unbounded value is copied
   * or suspended. Unknown foreign payloads need no quota graph here; acquiring
   * the same value into a bounded owner still requires its real visitor. */
  stampGraph(root: ManagedRoot): void {
    this.collect(
      root,
      new Map(),
      undefined,
      undefined,
      undefined,
      (value, type) => {
        const row = this.row(type);
        if (row.kind === "provider" && row.provider === "opaque")
          return { identity: value, bytes: 0n, children: [] };
        if (!this.retained)
          throw new Error("managed provider has no retained-value visitor");
        return this.retained(value, type);
      },
    );
  }

  needsAdoption(type: number): boolean {
    const cached = this.adoption.get(type);
    if (cached != null) return cached;
    const active = new Set<number>();
    const contains = (type: number): boolean => {
      if (active.has(type)) return false;
      active.add(type);
      const row = this.row(type);
      switch (row.kind) {
        case "generator":
        case "actor_call":
        case "dynamic":
          return true;
        case "provider":
          return ["task", "stream", "sink"].includes(row.provider);
        case "callable":
          return row.captures == null || row.captures.some(contains);
        case "aggregate":
          return row.fields.some(contains);
        case "variant":
          return row.cases.some((fields) => fields.some(contains));
        case "shared":
          return !row.weak;
        case "map":
          return contains(row.key) || contains(row.value);
        case "array":
        case "vector":
        case "slice":
        case "set":
          return contains(row.element);
        default:
          return false;
      }
    };
    const result = contains(type);
    this.adoption.set(type, result);
    return result;
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
        const environment =
          value.kind === "record"
            ? value
            : value.kind === "closure" && value.environment.kind === "record"
              ? value.environment
              : null;
        if (
          !environment ||
          (value.kind !== "closure" && value.kind !== "record")
        )
          throw new Error("managed copy has no captures");
        const captures = row.captures ?? value.managedCaptures;
        if (!captures)
          throw new Error("callable has no concrete capture descriptor");
        const copied = {
          ...environment,
          fields: environment.fields.map((item, index) =>
            this.copy(item, captures[index]!),
          ),
        };
        const result: VmValue =
          value.kind === "record" ? copied : { ...value, environment: copied };
        if (result.kind === "closure" || result.kind === "record")
          result.managedCaptures = captures;
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
      case "actor_call":
        return value;
      case "provider":
        if (
          row.provider === "json" ||
          row.provider === "yaml" ||
          row.provider === "toml"
        ) {
          if (value.kind !== "encoding")
            throw new Error("managed encoding has no tree");
          return cloneEncoding(value, true);
        }
        if (row.provider === "task" || row.provider === "sink") return value;
        throw new HeapExceeded();
    }
  }

  collect(
    root: ManagedRoot,
    allocations: Allocations,
    substitution?: { before: VmValue; after: VmValue },
    stringGrowth?: { value: VmValue; identity: object; capacity: number },
    retained?: (value: VmValue, type: number) => void,
    providerGraph = this.retained,
    storageVisitor?: StorageVisitor,
  ): void {
    const active = new Set<object>();
    const allocation = (
      identity: object,
      bytes: bigint,
      children = () => {},
    ): void => {
      checked(bytes);
      if (storageVisitor) {
        storageVisitor(identity, bytes, children);
        return;
      }
      const prior = allocations.get(identity);
      if (prior != null && prior !== bytes)
        throw new Error("managed storage has conflicting widths");
      if (prior != null || active.has(identity)) return;
      allocations.set(identity, bytes);
      active.add(identity);
      children();
      active.delete(identity);
    };
    const visit = (value: VmValue, type: number): void => {
      if (substitution?.before === value) value = substitution.after;
      if (this.retired.has(value)) return;
      if (value.kind === "unit") return;
      this.stamp(value, type);
      const row = this.row(type);
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
          if (storage.capacity)
            allocation(storage, this.width(row.element, storage.capacity), () =>
              storage.items.forEach((item) => visit(item, row.element)),
            );
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
          const payload = () =>
            row.cases[value.tag]!.forEach((child, index) =>
              visit(value.payload[index]!, child),
            );
          if (row.indirect)
            allocation(value, BigInt(row.payload_bytes), payload);
          else payload();
          return;
        case "map":
        case "set": {
          if (value.kind !== "map")
            throw new Error("managed map has no backing");
          const capacity = value.capacity ?? 0;
          const key = row.kind === "map" ? row.key : row.element;
          const slot =
            this.width(key) + (row.kind === "map" ? this.width(row.value) : 0n);
          if (capacity)
            allocation(value.entries, checked(slot * BigInt(capacity)), () => {
              for (const entry of value.entries.values()) {
                visit(entry.key, key);
                if (row.kind === "map") visit(entry.value, row.value);
              }
            });
          return;
        }
        case "callable": {
          if (value.kind === "function") return;
          const environment =
            value.kind === "record"
              ? value
              : value.kind === "closure" && value.environment.kind === "record"
                ? value.environment
                : null;
          if (
            !environment ||
            (value.kind !== "closure" && value.kind !== "record")
          )
            throw new Error("managed callable has no capture environment");
          if (this.retired.has(environment)) return;
          const fields = environment.fields;
          const captures = row.captures ?? value.managedCaptures;
          if (!captures)
            throw new Error("callable has no concrete capture descriptor");
          const width = captures.reduce(
            (total, type) => checked(total + this.width(type)),
            0n,
          );
          allocation(environment, width, () =>
            captures.forEach((type, index) => visit(fields[index]!, type)),
          );
          return;
        }
        case "shared": {
          if (value.kind !== "rc" && value.kind !== "weak")
            throw new Error("managed shared value has no cell");
          if (value.closed || !value.cell || row.weak) return;
          retained?.(value, type);
          allocation(value.cell, this.width(row.payload), () =>
            visit(value.cell!.value, row.payload),
          );
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
          allocation(value, this.width(payload), () =>
            visit(value.value, payload),
          );
          return;
        }
        case "generator":
        case "actor_call":
        case "provider": {
          retained?.(value, type);
          if (!providerGraph)
            throw new Error("managed provider has no retained-value visitor");
          const graph = providerGraph(value, type);
          allocation(graph.identity, graph.bytes, () => {
            for (const child of graph.children) {
              if (child.slots)
                allocation(
                  child.identity ?? child,
                  this.width(child.type),
                  () => visit(child.value, child.type),
                );
              else visit(child.value, child.type);
            }
          });
          return;
        }
      }
    };
    if (root.slots)
      allocation(root.identity ?? root, this.width(root.type), () =>
        visit(root.value, root.type),
      );
    else visit(root.value, root.type);
  }
}

interface RetainedStorage {
  bytes: bigint;
  children: Set<object>;
}

/** One incarnation owns backing edges. Retaining an existing backing does not
 * revisit its payload; its children belong to that backing until its final
 * local retention ends. Mutable/provider publication invalidates those edges
 * and reconciles them through the same typed visitor before acquisition. */
export class ManagedBudget {
  private readonly roots = new Set<ManagedRoot>();
  private nodes = new Map<object, RetainedStorage>();
  private edges = new Map<ManagedRoot, Set<object>>();
  private retainedBytes = 0n;
  private dirty = false;

  constructor(
    readonly layouts: ManagedLayouts,
    readonly limit: bigint,
    private readonly acquired?: (root: ManagedRoot) => void,
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
    this.refresh();
    return this.retainedBytes;
  }

  changed(): void {
    this.dirty = true;
  }

  private bytes(nodes: ReadonlyMap<object, RetainedStorage>): bigint {
    let bytes = 0n;
    for (const node of nodes.values()) bytes = checked(bytes + node.bytes);
    return bytes;
  }

  private graph(
    roots: readonly ManagedRoot[],
    known = new Map<object, RetainedStorage>(),
  ) {
    const nodes = new Map<object, RetainedStorage>();
    const edges = new Map<ManagedRoot, Set<object>>();
    let fresh = false;
    for (const root of roots) {
      let parent = new Set<object>();
      edges.set(root, parent);
      const storage: StorageVisitor = (identity, bytes, children) => {
        parent.add(identity);
        const prior = nodes.get(identity) ?? known.get(identity);
        if (prior?.bytes === bytes) return;
        fresh = true;
        if (prior && !known.has(identity))
          throw new Error("managed storage has conflicting widths");
        const node = { bytes, children: new Set<object>() };
        nodes.set(identity, node);
        const outer = parent;
        parent = node.children;
        children();
        parent = outer;
      };
      this.layouts.collect(
        root,
        new Map(),
        undefined,
        undefined,
        undefined,
        undefined,
        storage,
      );
    }
    return { nodes: fresh ? nodes : known, edges, fresh };
  }

  private refresh(): void {
    if (!this.dirty) return;
    const graph = this.graph([...this.roots]);
    this.nodes = graph.nodes;
    this.edges = graph.edges;
    this.retainedBytes = this.bytes(graph.nodes);
    this.dirty = false;
  }

  private prune(): void {
    const live = new Set<object>();
    const pending = [...this.edges.values()].flatMap((edges) => [...edges]);
    while (pending.length) {
      const identity = pending.pop()!;
      if (live.has(identity)) continue;
      live.add(identity);
      pending.push(...(this.nodes.get(identity)?.children ?? []));
    }
    for (const [identity, node] of this.nodes)
      if (!live.has(identity)) {
        this.nodes.delete(identity);
        this.retainedBytes -= node.bytes;
      }
  }

  check(extra: readonly ManagedRoot[]): void {
    this.refresh();
    let graph = this.graph(extra, this.nodes);
    if (graph.fresh) graph = this.graph([...this.roots, ...extra]);
    if (graph.fresh && this.bytes(graph.nodes) > this.limit)
      throw new HeapExceeded();
  }

  checkPayload(bytes: bigint, children: readonly ManagedRoot[]): void {
    const allocations = this.allocations(children);
    allocations.set({}, checked(bytes));
    if (total(allocations) > this.limit) throw new HeapExceeded();
    this.changed();
  }

  checkMutation(before: VmValue, after: VmValue, detached = false): void {
    const allocations = this.allocations([], { before, after });
    if (detached)
      this.layouts.collect(
        { value: before, type: this.layouts.type(before) },
        allocations,
      );
    if (total(allocations) > this.limit) throw new HeapExceeded();
    this.changed();
  }

  checkStringGrowth(value: VmValue, capacity: number, detached = false): void {
    const allocations = this.allocations([], undefined, {
      value,
      identity: {},
      capacity,
    });
    if (detached)
      this.layouts.collect(
        { value, type: this.layouts.type(value) },
        allocations,
      );
    if (total(allocations) > this.limit) throw new HeapExceeded();
    this.changed();
  }

  retain(root: ManagedRoot): ManagedLease {
    return this.retainMany([root]);
  }

  retainMany(roots: readonly ManagedRoot[]): ManagedLease {
    roots = roots.map((root) => ({
      ...root,
      ...(root.slots ? { identity: root.identity ?? root } : {}),
    }));
    this.refresh();
    let graph = this.graph(roots, this.nodes);
    const acquired = graph.fresh;
    // New storage can have been moved into an existing mutable owner since
    // its previous snapshot. Reconcile that ownership before committing any
    // new bytes; ordinary alias retains keep the existing child edges.
    if (graph.fresh) graph = this.graph([...this.roots, ...roots]);
    if (graph.fresh && this.bytes(graph.nodes) > this.limit)
      throw new HeapExceeded();
    this.nodes = graph.nodes;
    if (graph.fresh) this.retainedBytes = this.bytes(graph.nodes);
    for (const [root, edges] of graph.edges) this.edges.set(root, edges);
    roots.forEach((root) => this.roots.add(root));
    if (acquired) roots.forEach((root) => this.acquired?.(root));
    let released = false;
    return {
      roots,
      release: () => {
        if (released) return;
        released = true;
        this.refresh();
        let retained = false;
        roots.forEach((root) => {
          this.roots.delete(root);
          retained ||= !!this.edges.get(root)?.size;
          this.edges.delete(root);
        });
        if (retained) this.prune();
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
