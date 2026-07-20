# Hew Value-Semantics and Ownership Contract

## Core principle: copy-on-write, immutable by default

Values in Hew are immutable views shared by refcount; a copy is made only on
mutation of shared data. Passing an owned value to a function is a **borrow** —
the caller retains ownership and drops the value at its scope exit; the callee
gets an immutable copy-on-write view. Reuse after a call (`f(p); g(p)`) and
value-receiver method chains are legal. There is no use-of-moved-value error for
function calls, no ordinary reference syntax, and no lifetimes — ownership and
sharing are inferred. The entire binding surface is two keywords, `let`
(immutable) and `var` (mutable); there are no ownership annotations to write.
Lifetimes, mutable references, and a capability lattice are permanent refusals,
not deferred features.

```hew
let p = build_point();
let a = area(p);        // borrow — p is still yours
let b = perimeter(p);   // borrow again — legal, no clone needed
println(p.describe());  // value-receiver chain — still legal
```

Reaching for a second *independent, divergent* copy is a `clone`: `clone val`
(prefix form, the natural spelling) or `val.clone()` (method form) — see below.
`clone` is a cost operation, not an ownership keyword: you never need it to keep
using a value after passing or sending it somewhere. Copy-on-write already keeps
your binding valid.

## How the compiler manages memory: the drop-obligation invariant

The user-facing promise is simple: **you never free, annotate, or move — the
compiler balances the books.**

Under the hood, every live heap value carries exactly one *drop obligation*.
Creating a value — or retaining a share of one — mints an obligation; scope
exit, consumption, or hand-off discharges it, recursively through everything the
value owns. Sharing a value (reading a field out of a live composite, reading a
collection element, passing it by value while the caller continues) is a
refcount **retain**, not a raw alias and not a deep copy. Mutating a value whose
buffer is shared **forks** it first (copy-on-write). Every one of these
obligations is inferred by the compiler; none is written by you.

Ordinary Hew syntax has no references; the separate extern-signature-only FFI
view spelling is documented in the language guide's
[FFI boundary appendix](../hew-language-guide.md#appendix-a---ffi-boundary-types).

## `clone x` — the eager-copy cost operation

`clone` is not an ownership keyword. It is a stdlib **cost operation** that means
"make an independent, eager copy of this value *now*" — the opposite of
copy-on-write's lazy, on-demand fork. `clone x` (prefix form, the natural and
primary spelling) and `x.clone()` (method form) are equivalent:

```hew
let a: Vec<i64> = Vec::new();
a.push(1); a.push(2);
let b = clone a;      // == a.clone() — independent copy, forked eagerly now
b.push(99);
println(a.len());     // 2
println(b.len());     // 3
```

You reach for `clone` only when you want a **second, divergent** copy that
mutates independently of the original — as above, where `b` grows while `a` does
not. You never need it for correctness: passing a value to a function borrows it,
and sending a value to an actor takes a snapshot, so your binding stays valid
either way (see the two sections above and below). Because it eagerly duplicates,
`clone` is a cost you opt into, never a ceremony the compiler demands.

`string`, `Vec<T>`, `HashMap<K,V>`, `HashSet<T>`, and records (via the
`RecordCloneInplace`/`CopyCloneNoop` MIR rewrites) all have a wired copy path
and clone correctly. The `CloneNotYetSupported` diagnostic is a fail-closed
backstop: it fires for a `clone` the checker cannot map to any real copy path (a
type with no clone method at all, or a heap type whose runtime copy path
genuinely isn't wired yet, e.g. `bytes`). It never fires for already-resolved
clones, and it never silently aliases — every unresolved `clone` is a
compile-time error, not a runtime surprise.

## Snapshot on send

Sending a value across an **actor boundary** is a **copy-on-write snapshot**, not
a move. A `receive fn` method call or `.send()` on a lambda-actor handle gives
the receiver an independent snapshot of the value, and the sender's binding
**stays valid** — there is no use-of-moved-value error:

```hew
let greeting = "hello";
printer.print_message(greeting);
println(greeting);   // still legal — send took a snapshot, greeting is yours
```

Isolation is preserved by copy-on-write itself, not by invalidating the sender.
The receiver and the sender each own a fork of the value: if either mutates its
copy, the write forks the shared buffer, so neither can observe the other's
changes. Actors never share memory, and a snapshot guarantees they never need to.

This makes fan-out natural — send the same value to many workers in a loop with
no ceremony:

```hew
for conn in batch {
    worker.handle(conn_info, conn);   // conn_info sent each iteration, still valid
}
```

At the runtime level the snapshot mechanism is a cost detail chosen per value,
never a difference in meaning: a provably-unique value (refcount 1 at the send,
with no later use) is transferred by pointer with zero copy; an
immutable-shareable value (`string`, `bytes`) is retain-shared with a
copy-on-write fork on first mutation; a shared mutable collection (`Vec<T>`,
`HashMap<K,V>`, `HashSet<T>`) is deep-copied into the receiver's per-actor heap
today, converging to retain-share-plus-copy-on-write as the model completes. Each
tier is indistinguishable from the others at the source level — the sender always
keeps a valid, independent value.

Move-on-send returns only for a provably-unique or `resource`-kind value as a
runtime optimization (the pointer-transfer fast path above), never as a surface
rule you must reason about. Sending a **non-sendable** value — a resource-shaped
type, before the `resource` kind ships (see below) — is a fail-closed compile
error, not a silent copy. See HEW-SPEC-2026.md §3.4.4 and §3.7.2 for the full
model.

## `resource` — reserved for a future affine kind

`resource` is a **reserved word**. It names an affine (move-only) kind that will
ship after the first release: a `resource type` — a file, a socket, a unique
handle — is never clonable, never implicitly shared, has a deterministic
exactly-once destructor, and moves on send (the sender is invalidated, because a
unique resource genuinely cannot exist in two places). That is the *one* place a
surface move will ever return.

At rc1 the feature is not yet user-visible; only the **seam** is reserved:

- the word `resource` is reserved in the grammar;
- no generic or stdlib code may assume universal clonability — `clone` stays a
  capability-gated operation, so a future non-clonable kind slots in without a
  breaking retrofit;
- sending a resource-shaped value is a fail-closed compile error today (there is
  no unsound window before the kind ships).

Reserving the seam now — rather than the whole feature — closes the retrofit
danger (generics silently assuming every type is copyable) while keeping the
affine kind itself severable as the first post-rc1 ownership feature.

## The complete rejection surface: three walls

The entire ownership model rejects your program in exactly **three** places. Each
wall names the binding, its state, and the span that put it there, and each ships
a one-line escape:

| Wall | When it fires | Escape |
|---|---|---|
| mutation of a `let` | you mutate a value bound with `let` | declare it `var` |
| clone of a non-cloneable | you `clone` a value with no copy path | restructure now; the `resource` lifecycle later |
| send of a non-sendable | you send a resource-shaped value | use the owning-actor pattern; lifts when `resource` transfer ships |

There is no fourth wall. Use-after-send of a value is **not** an error (send takes
a snapshot); use-after-call is **not** an error (calls borrow). The model has no
lifetime errors, no borrow-checker vocabulary, and no capability terms to learn.

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

Hew uses atomic reference counting without a cycle collector. Constructing
reference cycles through actor state or stored self-referential structures will
cause the involved values to leak (neither the cycle collector nor weak
references exist yet).

**Guideline:** design data structures as DAGs (trees, acyclic graphs). Cycles are a
documented limitation for v0.5; future work will evaluate ORCA-style cycle collection.

## Remaining work

The pointer-transfer fast path for provably-unique sends is an inferred cost
optimization, never a user-facing annotation and never required for correctness —
see HEW-SPEC-2026.md §3.7.2. It converges further as retain-on-share extends
across all heap types, at which point the deep-copy send tier retires and every
snapshot is near-zero-copy. Everything else described above — borrow-by-default
calls with caller-side drop, copy-on-write sharing, snapshot-on-send, and
`clone x` as the eager-copy cost operation — is the model as it stands.
