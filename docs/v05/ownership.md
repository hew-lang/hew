# Values and ownership

The short code fences are illustrative fragments that refer to their
surrounding declarations.

## Core principle: copy-on-write, immutable by default

Values in Hew are immutable views shared by refcount; mutation makes a private
copy when the data is shared. Passing a value to a function borrows it: the
caller keeps ownership and the callee receives an immutable copy-on-write view.
Calls such as `f(p); g(p)` and value-receiver method chains therefore keep `p`
available. Hew infers ownership and sharing from `let`, `var`, and receiver
forms; `var self` marks a method that mutates its receiver.

```hew,ignore
let p = build_point();
let a = area(p);        // borrow — p is still yours
let b = perimeter(p);   // borrow again — legal, no clone needed
println(p.describe());  // value-receiver chain — still legal
```

Reaching for a second _independent, divergent_ copy is a `clone`: `clone val`
(prefix form, the natural spelling) or `val.clone()` (method form) — see below.
`clone` requests an independent eager copy. Passing or sending an ordinary
value already leaves the sender's binding valid through borrow or snapshot
semantics.

## How the compiler manages memory

The compiler inserts retains, copies, and releases from the program's value
flow.

Under the hood, every live heap value carries exactly one _drop obligation_.
Creating a value — or retaining a share of one — mints an obligation; scope
exit, consumption, or hand-off discharges it, recursively through everything the
value owns. Sharing a value (reading a field from a live composite, reading a
collection element, or passing it while the caller continues) retains its
refcount. Mutating a shared buffer forks it first. These obligations require no
source annotations.

Ordinary Hew syntax has no references; the separate extern-signature-only FFI
view spelling is documented in the language guide's
[FFI boundary appendix](../hew-language-guide.md#appendix-a---ffi-boundary-types).

### Release safety

A refcount release that would underflow aborts instead of wrapping. Normal
scope exits and handled failure paths discharge their live obligations. A
process-wide abort may leave allocations for the operating system to reclaim;
it never speculatively releases memory still owned by another value.

## `clone x` — the eager-copy cost operation

`clone` is a stdlib cost operation that makes an independent eager copy.
`clone x` and `x.clone()` are equivalent:

```hew,ignore
var a: Vec<i64> = Vec.new();
a.push(1); a.push(2);
var b = clone a;      // == a.clone() — independent copy, forked eagerly now
b.push(99);
println(a.len());     // 2
println(b.len());     // 3
```

Use `clone` when two copies must mutate independently, as above where `b` grows
while `a` stays unchanged. Function calls borrow values and actor sends take
snapshots, so both operations keep the caller's binding available.

`string`, `Vec<T>`, `HashMap<K,V>`, `HashSet<T>`, and records (via the
`RecordCloneInplace`/`CopyCloneNoop` MIR rewrites) have copy paths. A clone of a
type without a copy path is a compile-time error.

## Snapshot on send

Sending an ordinary value across an **actor boundary** is a logical snapshot,
not a move. A `receive fn` method call gives the receiver an independent value,
and the sender's binding **stays valid**:

```hew,ignore
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

```hew,ignore
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

| Handle   | Members                                  | What a copy does                      | What closes it                                              |
| -------- | ---------------------------------------- | ------------------------------------- | ----------------------------------------------------------- |
| actor    | an actor's own type                      | retains an alias of the same identity | releases the alias; scopes and supervision govern the actor |
| counted  | `Rc`, `Weak`, `LambdaPid`                | a refcounted retain; the count rises  | the last release, through drop glue                         |
| opaque   | `#[opaque]` types, such as a channel end | another name for one resource         | `close(consume self)` where the type declares it            |
| resource | `#[resource]` wrappers, `Stream`, `Sink` | a move; the source binding is dead    | drop glue at scope exit, or an early `close`                |

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

```hew,ignore
let p = spawn Worker;
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

> **Compiler limitation: `E_LIMIT_OPAQUE_ACTOR`.** An `#[opaque]` type in a
> `receive fn` parameter or actor init field is rejected. Open the handle
> inside the actor handler instead.

## Common ownership errors

Ownership diagnostics name the binding, its state, and the source span that
caused the conflict:

| Wall                       | When it fires                                                                                     | Escape                                                                                |
| -------------------------- | ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| mutation of a `let`        | you mutate a value bound with `let`, or call a `var self` method on it                            | declare it `var`                                                                      |
| clone of a non-cloneable   | you `clone` a value with no copy path                                                             | restructure, or hold the resource in an actor                                         |
| send of a non-sendable     | you send an `Rc`, a `Weak`, a `LambdaPid`, or a closure, or you send any handle to a remote actor | use the [owning-actor pattern](../hew-language-guide.md#own-a-resource-with-an-actor) |
| use after send of a handle | you send an opaque or resource handle locally and then use the binding                            | send it last, or reopen the handle in the receiving actor                             |

Use-after-send applies to handles. A value send takes a snapshot and a function
call borrows, while a local send of an opaque or resource handle moves it. The
`E_USE_AFTER_SEND` diagnostic points to the consuming send and the later use.

## Equality: structural by default, yours if you say so (Q322)

`==` in Hew compares **values**, not addresses:

```hew,ignore
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
