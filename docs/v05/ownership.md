# Hew v0.5 Value-Semantics and Ownership Contract

## Core principle: single ownership, move by default

Hew's owned heap types (`string`, `bytes`, `Vec<T>`, `HashMap<K,V>`, and
user-defined types) have exactly one owner at a time. Assigning a binding,
passing an owned value to a function, or sending it to an actor **moves**
it — the source binding becomes invalid, and using it afterward is a
compile-time `use of moved value` error. There is no implicit copy-on-call.

Reaching for a second independent copy is explicit: `val.clone()` (method
form) or `clone val` (prefix form) — see below.

Sending a value to another actor is also a move at the language level, with a
runtime-level optimization for immutable-shareable types (`string`, `bytes`)
— see "Move on send" below.

## `&T` — immutable borrow marker

```hew
fn describe(s: &string) -> string {
    return s;
}
```

`&T` marks a parameter (or return type) as a **non-owning immutable borrow**:

- The callee gets a read-only view into the caller's value.
- No retain/release is performed — the borrow cannot outlive the owner.
- `&T` lowers to `ValueClass::View` in the HIR, exactly as the existing `IntentKind::Read`
  path for call arguments.

**What `&T` is not (v0.5 constraints):**

| Form | Status | Reason |
|------|--------|--------|
| `&T` (immutable borrow) | ✅ Supported | Q320 — read-only view |
| `&mut T` (mutable borrow) | ❌ Not in v0.5 | Q320 — no exclusive-borrow surface yet |
| `&var T` (legacy spelling) | ❌ Parse error | Rejected at the parser |
| Reference cycles via `&T` | ⚠️ Leak risk | Q321 — no cycle collector; avoid cycles |

The parser will reject `&mut T` with a diagnostic. `&var T` is also rejected. These forms
are reserved for future work.

## `.clone()` / `clone x` — explicit copy

`val.clone()` (method form) and `clone val` (prefix form, unary precedence) are
equivalent and are the canonical way to get a second independent copy of an
owned value without consuming the original:

```hew
let a: Vec<i64> = Vec::new();
a.push(1); a.push(2);
let b = clone a;      // == a.clone() — independent copy
b.push(99);
println(a.len());     // 2
println(b.len());     // 3
```

`string`, `Vec<T>`, `HashMap<K,V>`, `HashSet<T>`, and records (via the
`RecordCloneInplace`/`CopyCloneNoop` MIR rewrites) all have a wired copy path
and clone correctly. The `CloneNotYetSupported` diagnostic is a fail-closed
backstop: it fires for a `.clone()` call the checker cannot map to any real
copy path (a type with no clone method at all, or a heap type whose runtime
copy path genuinely isn't wired yet, e.g. `bytes`). It never fires for
already-resolved clones, and it never silently aliases — every unresolved
`.clone()` call is a compile-time error, not a runtime surprise.

## Move on send

When a value crosses an actor boundary — a `receive fn` method call or
`.send()` on a lambda-actor handle — it is **moved**: the sender can no
longer use it.

```hew
let greeting = "hello";
printer.print_message(greeting);
// println(greeting);  // compile error: use of moved value `greeting`
```

At the runtime level the send mechanism is gated by the value's
admissibility class: immutable-shareable owned types (`string`, `bytes`) are
alias-shared by refcount retain (no byte copy, with a copy-on-write fork on
first mutation of a shared buffer); mutable collections (`Vec<T>`,
`HashMap<K,V>`, `HashSet<T>`) are deep-copied into the receiver's per-actor
heap. From the program's perspective both are indistinguishable from
move-then-independent-value — the sender's binding is invalid either way.
Reach for `.clone()` first if you need to keep using the value after
sending it. See HEW-SPEC-2026.md §3.4.4 and §3.7.2 for the full model.

## Equality: structural only (Q322)

`==` in Hew compares **values**, not object identity:

```hew
let a = "hello";
let b = "hello";
a == b  // true — structural equality, always
```

There is no pointer-equality operator. Two distinct heap allocations holding the same bytes
compare equal. This invariant holds even after COW splits a shared buffer.

## Reference cycles (Q321)

Hew v0.5 uses atomic reference counting without a cycle collector. Constructing reference
cycles via `&T` borrows or actor state will cause the involved values to leak (neither the
cycle collector nor weak references exist yet).

**Guideline:** design data structures as DAGs (trees, acyclic graphs). Cycles are a
documented limitation for v0.5; future work will evaluate ORCA-style cycle collection.

## Remaining work

A zero-copy ownership-move fast path for provably-unique, single-recipient
sends of `Linear`/`iso` values is designed but not yet surfaced in edition
2026 — see HEW-SPEC-2026.md §3.7.2. Every other mechanism described above
(move-on-assignment, move-on-send with the alias-share/deep-copy gate,
`.clone()`/`clone x`) is shipped.
