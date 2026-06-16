# Hew v0.5 Value-Semantics and Ownership Contract

> **Status**: P0 (surface). Codegen/runtime support lands in P1–P4.

## Core principle: value semantics by default

Hew programs reason in values, not references. When you pass a `string` to a function,
the callee gets its own copy of that value — it cannot observe mutations the caller makes
later, and the caller cannot observe changes inside the callee.

Under the hood, Hew uses **copy-on-write (COW)** with atomic reference-counting so that
physically distinct copies are only allocated when one side mutates the shared payload
(similar to the `bytes` mechanism, generalized to `string`, `Vec`, and other heap types in P2+).
The surface contract is pure value semantics — the COW mechanism is invisible.

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

## `.clone()` — explicit deep copy (P2+)

The `CloneNotYetSupported` diagnostic is emitted for **unresolved, zero-argument `.clone()`
calls** — i.e. a bare `x.clone()` that the type-checker has not mapped to any real method
(the M-COW value-clone path that is not yet wired):

```hew
let y = x.clone();  // error: CloneNotYetSupported  (unresolved M-COW deep-copy)
```

**Already-resolved `.clone()` methods remain legal and compile normally.** For example, a
user-declared `trait Clone { fn clone(val: Self) -> Self; }` whose call the type-checker
resolved — or a standard-library type that ships its own `.clone()` method — is not
affected by this diagnostic. The intercept is limited to unresolved, zero-arg calls that
would otherwise silently return the same handle and violate value semantics.

The runtime deep-copy path (`VWT.copy` / COW `ensure_unique`) will be connected to the
unresolved `.clone()` case in P2 once the COW refcount infrastructure has been generalized
across heap types. Until then the fail-closed diagnostic ensures no silent alias is
produced.

**Workaround for P0:** if you need an explicit copy today and have no resolved `.clone()`
method, restructure to pass by value. By-value pass already retains under M-COW (the
`IntentKind::Read` path), so the value the callee holds is semantically independent.

## Send = value-equal copy

When an actor sends a value to another actor via a `receive fn` call, the value is
**copied** (retain-then-use in the recipient, not a move from the sender):

```hew
let greeting = "hello";
printer.print_message(greeting);
// `greeting` is still valid here — send does NOT consume it
```

This is the M-COW contract: by-value pass = RETAIN (`VWT.copy`), never a move. The move
fast path (`Linear`/`iso` zero-copy handoff) is planned for P6 for provably-unique
single-recipient sends.

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

## Roadmap

| Phase | Content |
|-------|---------|
| P0 (now) | `&T` surface syntax, fail-closed diagnostic for unresolved `.clone()`, value-semantics contract |
| P1 | Layout-witness VWT defined and bound to `bytes` |
| P2 | COW refcount generalized to `string`/`Vec`/`Array`/`HashMap`/`HashSet`; `.clone()` wired |
| P3 | Drop-site emission via VWT; MIR `build_lifo_drops` integration |
| P4 | `std/`/`examples/` full migration; diagnostics polish |
| P6 | `Linear`/`iso` zero-copy move fast path for provably-unique actor sends |
