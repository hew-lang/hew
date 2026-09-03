# Hew Value-Semantics and Ownership Contract

## Core principle: copy-on-write, immutable by default

Values in Hew are immutable views shared by refcount; a copy is made only on
mutation of shared data. Passing an owned value to a function is a **borrow** —
the caller retains ownership and drops the value at its scope exit; the callee
gets an immutable copy-on-write view. Reuse after a call (`f(p); g(p)`) and
value-receiver method chains are legal. There is no use-of-moved-value error for
function calls, no ordinary reference syntax, and no lifetimes — ownership and
sharing are inferred. The entire binding surface is two keywords, `let`
(immutable) and `var` (mutable), and a method that mutates its receiver says so
with `var self`; there are no ownership annotations to write. Lifetimes, mutable
references, and a capability lattice are permanent refusals, not deferred
features.

```hew
let p = build_point();
let a = area(p);        // borrow — p is still yours
let b = perimeter(p);   // borrow again — legal, no clone needed
println(p.describe());  // value-receiver chain — still legal
```

Reaching for a second _independent, divergent_ copy is a `clone`: `clone val`
(prefix form, the natural spelling) or `val.clone()` (method form) — see below.
`clone` is a cost operation, not an ownership keyword: you never need it to keep
using a value after passing or sending it somewhere. Copy-on-write already keeps
your binding valid.

## How the compiler manages memory: the drop-obligation invariant

The user-facing promise is simple: **you never free, annotate, or move — the
compiler balances the books.**

Under the hood, every live heap value carries exactly one _drop obligation_.
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

### The leak-never-double-free order

Release bugs are not all equal: a double-free corrupts memory another owner
still holds, while a leak wastes memory the program has already paid for. The
model, its runtime, and its gates enforce that severity difference as a total
order:

- **Over-release is unconditionally rejected.** Discharging an obligation that
  was never minted — a refcount that would go below zero — aborts the program
  rather than wrapping (the shared-refcount release assert in
  `hew-runtime/src/arc.rs` is the canonical site). There is no build mode in
  which a double-free proceeds.
- **Under-release is tracked and ratcheted shrink-only.** A known leak is
  recorded in the sanitizer and leak-oracle expectations (the ASan/LSan
  fixture gate on Linux, the `leaks --atExit` oracle on macOS), and the
  tracked set may only shrink: a new leak is a gate failure, and a fixed leak
  deletes its entry so it cannot silently return.
- **Abort and trap paths may leak-at-abort, never double-free.** A runtime
  trap abandons outstanding obligations rather than force-discharging them:
  an abandoned obligation is a bounded leak in a dying process, while a
  speculative release on a trap path risks freeing a value another owner
  still holds.

## `clone x` — the eager-copy cost operation

`clone` is not an ownership keyword. It is a stdlib **cost operation** that means
"make an independent, eager copy of this value _now_" — the opposite of
copy-on-write's lazy, on-demand fork. `clone x` (prefix form, the natural and
primary spelling) and `x.clone()` (method form) are equivalent:

```hew
var a: Vec<i64> = Vec.new();
a.push(1); a.push(2);
var b = clone a;      // == a.clone() — independent copy, forked eagerly now
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

Sending an ordinary value across an **actor boundary** is a logical snapshot,
not a move. A `receive fn` method call gives the receiver an independent value,
and the sender's binding **stays valid**:

```hew
let greeting = "hello";
printer.print_message(greeting);
println(greeting);   // still legal — send took a snapshot, greeting is yours
```

MIR chooses the implementation per argument. Inline values are bit-copied,
refcounted sendable leaves retain one receiver owner, and mutable collections or
aggregates are structurally materialized before the mailbox copies the carrier
bytes. A proven last use may transfer the existing owner instead, but that is an
optimization: uncertain branches, loop back-edges, aliases, and projections
always snapshot.

Cloneability is separate from sendability. `Rc<T>` and `Weak<T>` have local
clone/drop support but remain non-`Send`, including when nested in records,
tuples, or enums. Single-owner channel handles remain transfer-only lifecycle
values rather than adopting ordinary snapshot semantics.

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

Move-on-send returns only for a provably-unique value as a runtime optimization
(the pointer-transfer fast path above), never as a surface rule you must reason
about. Sending a **handle** is the one place a move is the rule rather than an
optimization, and that is the next section. See HEW-SPEC-2026.md §3.4.4 and
§3.7.2 for the full model.

## Values, handles, and callables

Everything above describes **values**. Not every type is one, and the difference
is what a second name means. Three categories cover the language:

- A **value** is copied. `let b = a` gives you two independent values (lazily,
  by copy-on-write), `==` compares them structurally, and a send delivers a
  snapshot. Scalars, `string`, `bytes`, records, enums, tuples, arrays, `Vec`,
  `HashMap`, `HashSet`, and `dyn Trait` objects are values.
- A **handle** is a name for something that lives elsewhere — an actor, a
  refcount, a resource. A second binding is a second name for the same thing,
  and methods act through the handle no matter whether the binding is `let` or
  `var`, which is why `let pid`, `let rc`, and `let d = deque.new()` are all
  legal receivers of `send`, `set`, and `push_back`.
- A **callable** is a closure, a generator, or a task. It behaves as a value
  locally and is never a message payload.

Handles come in four flavours, and the flavour is the whole difference:

| Handle   | Members                                  | What a copy does                     | What closes it                                   |
| -------- | ---------------------------------------- | ------------------------------------ | ------------------------------------------------ |
| pid      | `LocalPid`, `RemotePid`, `ChildRef`      | another name for one actor           | nothing; the actor stops                         |
| counted  | `Rc`, `Weak`, `LambdaPid`                | a refcounted retain; the count rises | the last release, through drop glue              |
| opaque   | `#[opaque]` types, such as a channel end | another name for one resource        | `close(consume self)` where the type declares it |
| resource | `#[resource]` wrappers, `Stream`, `Sink` | a move; the source binding is dead   | drop glue at scope exit, or an early `close`     |

`LambdaPid` is a **counted** handle, not a plain pid: releasing the last copy
releases the closure environment the lambda actor captured.

`resource` is an ordinary identifier — `let resource = 3` is a legal binding.
The affine kind shipped as the `#[resource]` attribute, and the linear
discipline beside it as `#[linear]`: a `#[resource]` value may be dropped and
its drop glue closes it, a `#[linear]` value may not be dropped and must be
consumed by one of its own `consume self` methods.

## `is` — identity, for handles only

`is` asks whether two names denote the same actor, the same count, or the same
resource. It admits handles and nothing else. On a value — a scalar, a
`string`, a record, a `Vec` — it is a compile error (`E_IS_VALUE_TYPE`), because
a value has no identity to compare: `let b = a` gives you a copy whose address
is a cost detail, so an identity answer would report the copy-on-write tier
rather than anything about your program. Compare values with `==`.

```hew
let p = spawn Worker();
let q = p;
println(f"{p is q}");   // true — one actor, two names
```

There is no `expr is TypeName` form.

## An opaque handle may live inside an actor

The way to own a resource is to give it to one actor: open the connection
inside the actor, keep it in that actor's state, and send the actor ordinary
operation messages. An `#[opaque]` handle, or the `#[resource]` wrapper around
one, may be an actor's init field — moved in at `spawn`, owned by that actor's
heap, closed by its drop glue when the actor stops — and may be a `receive fn`
parameter on a local send, where the send consumes it. That is how one actor
holds one connection and serves many requests over it.

> **Not yet: `E_LIMIT_OPAQUE_ACTOR`.** Today an `#[opaque]` type in a
> `receive fn` parameter or an actor init field is refused, with the wire rule's
> "message payloads must be CBOR-serializable" applied to a local send that
> never serializes. The refusal carries this Limitation code so it is
> distinguishable from the rule, and it lifts when local sends carry the
> transfer-last-use move. Until then, open the handle inside the handler.

## The complete rejection surface: four walls

The entire ownership model rejects your program in exactly **four** places. Each
wall names the binding, its state, and the span that put it there, and each ships
a one-line escape:

| Wall                       | When it fires                                                                                     | Escape                                                                                |
| -------------------------- | ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| mutation of a `let`        | you mutate a value bound with `let`, or call a `var self` method on it                            | declare it `var`                                                                      |
| clone of a non-cloneable   | you `clone` a value with no copy path                                                             | restructure, or hold the resource in an actor                                         |
| send of a non-sendable     | you send an `Rc`, a `Weak`, a `LambdaPid`, or a closure, or you send any handle to a remote actor | use the [owning-actor pattern](../hew-language-guide.md#own-a-resource-with-an-actor) |
| use after send of a handle | you send an opaque or resource handle locally and then use the binding                            | send it last, or reopen the handle in the receiving actor                             |

The fourth wall is for **handles only**. Use-after-send of a _value_ is still
not an error (send takes a snapshot) and use-after-call is still not an error
(calls borrow) — but a handle cannot be snapshotted, because there is nothing to
snapshot but the resource itself, so a local send of one is a move and the
sender's binding is dead afterwards (`E_USE_AFTER_SEND`). The model still has no
lifetime errors, no borrow-checker vocabulary, and no capability terms to learn.

## Equality: structural by default, yours if you say so (Q322)

`==` in Hew compares **values**, not addresses:

```hew
let a = "hello";
let b = "hello";
a == b  // true — structural equality, derived for free
```

`Eq`, `Ord`, `PartialOrd`, and `Hash` are derived for records and enums with no
declaration: field by field, variant by variant, and ordering lexicographically
by field order. Two distinct heap allocations holding the same bytes compare
equal, and that holds even after copy-on-write splits a shared buffer.

You can override the derived rule. `impl Eq for T { .. }` is legal, and `==`,
the ordering operators, and hashing all dispatch to your body for `T` — which is
what a case-insensitive key or a tolerance-based float comparison needs. The
derived implementation is a default, not a wall.

Identity is a separate question with a separate operator: `is`, on handles only
(above). There is no address comparison for values.

## Reference cycles (Q321)

Hew reclaims shared storage by reference counting and has no cycle collector:
a strong reference cycle through actor state or a stored self-referential
structure leaks silently. Design data as trees or DAGs; see
[Avoid reference cycles in actor state](../hew-language-guide.md#avoid-reference-cycles-in-actor-state)
for the guidance and the cycle-collection status.

## Remaining work

The pointer-transfer fast path for provably-unique sends is an inferred cost
optimization, never a user-facing annotation and never required for correctness —
see HEW-SPEC-2026.md §3.7.2. It converges further as retain-on-share extends
across all heap types, at which point the deep-copy send tier retires and every
snapshot is near-zero-copy. Everything else described above — borrow-by-default
calls with caller-side drop, copy-on-write sharing, snapshot-on-send, and
`clone x` as the eager-copy cost operation — is the model as it stands.
