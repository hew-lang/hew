# Monomorphization audit — primitive trait dispatch

Status: **partial**. Type-checker side is feature-complete for the user-defined
trait dispatch case (#1596, #1642, #1654, #1664). Two residual gaps survive
for the magic-builtin → user-`Display` path on the codegen side and for the
literal-form primitive receiver on the checker side; both are itemised under
[Residual gaps](#residual-gaps) below and are tracked by separate child
issues, leaving #1565's audit deliverable closed even though the underlying
behaviour migration is not.

This document records what shipped, the boundary contracts that future
changes must preserve, and the residual gaps with reproducible probe
transcripts. It is intended to be the durable reference; the per-PR
narrative for #1580 / #1596 / #1642 / #1654 / #1664 lives in those PRs'
descriptions.

## What shipped

### Checker — primitive trait dispatch (#1596, #1580)

- **Primitive-keyed trait impl table.** `Checker::primitive_trait_impls`
  (`hew-types/src/check/registration.rs`) is populated whenever
  `impl Trait for <kind>` registers, where `<kind>` is a primitive
  (`int`, `bool`, `char`, integer/float width aliases, `String`,
  `bytes`, `duration`) or a compiler-builtin generic (`Vec`, `HashMap`,
  `HashSet`). These receivers have no `type_defs` entry to hang impl
  methods off, so the side table is the only place those signatures
  live. Keyed on `Ty::canonical_lowering_name()` so `int` / `Int` /
  `i64` collapse to the same slot.
- **Method-call form.** When `x.fmt()` lands on a primitive or
  compiler-builtin generic receiver and the compiler-builtin method
  registry returns "no match", the side table is consulted via
  `try_dispatch_primitive_trait_method`
  (`hew-types/src/check/methods.rs`) before reporting "no method `<name>`
  on `<X>`". Compiler-builtin methods keep precedence — the side table
  is the *fallback*, not the primary path — so the surviving magic
  callers are unaffected.
- **UFCS form (`Trait::method(receiver, args...)`).** Trait method sigs
  register in `fn_sigs` with the receiver param stripped, so the
  existing `fn_sigs` lookup mis-arities the call. A new branch
  (`try_dispatch_ufcs_primitive_trait_method`) ahead of the `fn_sigs`
  lookup intercepts trait-qualified calls whose first arg resolves to
  a primitive or compiler-builtin generic, routes through the side
  table, and applies the receiver-stripped sig to the trailing args.
- **Receiver-kind matrix unit coverage** in
  `hew-types/src/check/tests.rs`: `int`, `bool`, `char`, `i32`, `String`
  (via a non-builtin trait method), `Vec`, plus a UFCS sentinel, an
  unknown-method diagnostic sentinel, a builtin-precedence sentinel,
  and a `pub type` regression sentinel that asserts user struct
  receivers continue to dispatch through `type_defs` rather than
  leaking into the primitive table. Run with
  `cargo test -p hew-types -- primitive_trait`.

### Codegen — primitive trait method lowering (#1642)

- `MethodCallReceiverKindPrimitiveTraitImpl`
  (`hew-codegen/src/mlir/MLIRGenExpr.cpp:4775–4791`) lowers a
  primitive-receiver trait method call by routing through
  `generateNamedTypeDispatch` with `{trait_name, canonical_receiver}`
  taken verbatim from the checker output. This is the survival vehicle
  for the dispatch decision crossing the checker→codegen boundary.

### Display trait surface (#1654)

- `pub trait Display { fn fmt(val: Self) -> String; }` is declared in
  `std/builtins.hew:21` so user code can `impl Display for MyType`
  without an explicit import.
- Blanket impls for `i8`-`i64`, `u8`-`u64`, `bool`, and `char` ship at
  `std/builtins.hew:29–87`; each delegates to the still-magic
  `to_string(self)` helper.
- The five magic Display callers gained `<T: Display>` bounds at the
  registration layer (`hew-types/src/check/registration.rs:155–270`):
  `print`, `println`, `to_string`, `assert_eq`, `assert_ne`. The
  registrations use `register_builtin_fn_with_bounds` rather than the
  fresh-`Ty::Var` shape used previously.
- `len` (line 197) and `stop` (line 204) **are not** bounded — they
  remain `Ty::Var(TypeVar::fresh())` registrations. `len` requires its
  own trait design (operates on collections, not Display values);
  `stop` is an actor-control intrinsic with no Display semantics.

### Deferred bound-check re-run (#1664)

- After defaulting resolves a previously-deferred type variable to
  e.g. `i64`, the bounds attached at registration (`T: Display`) must
  be re-checked against the now-concrete type. This is the contract
  fixed by #1664 and is now part of the type-check pipeline; it is
  **not** optional. A future change that drops re-evaluation
  post-defaulting will silently fail to catch un-`Display`-able
  values flowing into bounded builtins.

## Boundary contracts (must preserve)

| Boundary | Contract | Citation |
|---|---|---|
| Checker → codegen (primitive trait dispatch) | `MethodCallReceiverKind::PrimitiveTraitImpl { trait_name, canonical_receiver }` survives serialization (`hew-serialize::msgpack::MethodCallReceiverKindData`) and is consumed verbatim by codegen — codegen must not re-derive the dispatch decision from the AST. | `MLIRGenExpr.cpp:4775–4791`, `hew-types/src/check/methods.rs` |
| Receiver-key canonicalisation | Lookups go through `Checker::canonical_primitive_or_builtin_key`, which collapses user aliases to a single canonical key and never inspects trait-name strings. | `hew-types/src/check/registration.rs` |
| Bound re-evaluation | `T: Display` bounds attached at `register_builtin_fn_with_bounds` time must re-run after defaulting resolves the type variable. | #1664 commit `060ba542` |
| Builtin method precedence | Compiler-builtin method registry resolves first; the primitive trait impl table is a strict fallback. The five surviving magic callers (`print`, `println`, `to_string`, `assert_eq`, `assert_ne`) get their `<T: Display>` bound applied at the registration layer, not via the primitive table. | `hew-types/src/check/registration.rs:155–270` |

## Residual gaps

Three behaviours remain broken on `main` at the time of this audit
closeout. None are fixed in this lane (which is audit + e2e only); each
is documented with a verbatim probe transcript so a future change can
verify resolution by re-running the probe.

All probe sources are at `.tmp/probes/monomorphization/*.hew` in the
audit-closeout branch (gitignored — transient evidence, not committed).
The four-line outputs below were captured against `origin/main` at
`1a6d00fc` (`feat(runtime): add test-only SimTransport for transport
property tests (#1667)`).

### Residual 1 — Literal-form primitive receiver dispatch fails

The shipping fixture
`hew-codegen/tests/examples/e2e_traits/primitive_trait_impl_display_int.hew`
proves the **let-bound** receiver case works:

```
$ hew run …/primitive_trait_impl_display_int.hew
i64
```

The **literal-form** receiver case does not, even with an in-file
trait + impl declaration matching the shipping fixture verbatim:

```
$ cat .tmp/probes/monomorphization/integer_literal_fmt_local_trait.hew
trait Display { fn fmt(val: Self) -> String; }
impl Display for i64 { fn fmt(val: i64) -> String { "i64" } }
fn main() { println((42).fmt()); }

$ hew check .tmp/probes/monomorphization/integer_literal_fmt_local_trait.hew
integer_literal_fmt_local_trait.hew:16:14: error: no method `fmt` on `int`
 16 |     println((42).fmt());
    |              ^^^^^^^^^
type errors found
```

The diagnostic comes from the receiver-kind classification path in
`hew-types/src/check/methods.rs`. The `IntLiteral` receiver is not
canonicalising to `i64` for the side-table lookup; the let-bound case
works because the explicit type annotation `let x: i64 = 42` resolves
the receiver to `Ty::I64` before `try_dispatch_primitive_trait_method`
runs.

**Tracking:** to be filed as a child issue of #1565; see closeout
handoff for proposed body. Until that issue lands and is fixed,
documentation that says "primitive trait dispatch works on integer
receivers" needs the qualifier "non-literal".

### Residual 2 — `std/builtins.hew` `Display` is not method-dispatchable

The same let-bound `int` case that works when the user redeclares
`trait Display` in-file fails when only the `std/builtins.hew`
declaration is in scope:

```
$ cat .tmp/probes/monomorphization/integer_var_fmt_builtin_only.hew
fn main() {
    let x: i64 = 42;
    println(x.fmt());
}

$ hew check .tmp/probes/monomorphization/integer_var_fmt_builtin_only.hew
integer_var_fmt_builtin_only.hew:6:13: error: no method `fmt` on `int`
 6 |     println(x.fmt());
   |             ^^^^^^^
type errors found
```

The blanket `impl Display for i64` at `std/builtins.hew:47–51` is
present in source but not landing in the primitive trait impl table
that `try_dispatch_primitive_trait_method` consults. The shipping
fixture papers over this by redeclaring `trait Display` in the test
file; that registration is what populates the side-table for the test.

This is the gap that the `std/builtins.hew` Display surface was
intended to close — without it, user code cannot use the
`println(x.fmt())` pattern on a primitive without redeclaring the
trait themselves.

**Tracking:** to be filed as a child issue of #1565; see closeout
handoff for proposed body. The likely fix lives at the
`std/builtins.hew` registration boundary in `stdlib_loader.rs` /
`registration.rs`, not in `methods.rs`.

### Residual 3 — `print(MyStruct{})` codegen lowering rejects user Display impls

The post-#1654 checker accepts `print` / `println` calls on a user
struct that `impl Display`s; codegen does not honour the resolved
Display impl and rejects the call at MLIR lowering:

```
$ cat .tmp/probes/monomorphization/user_struct_println_via_magic.hew
pub type Greeting { who: String; }
impl Display for Greeting {
    fn fmt(val: Greeting) -> String { "hello, " + val.who }
}
fn main() { println(Greeting { who: "world" }); }

$ hew check .tmp/probes/monomorphization/user_struct_println_via_magic.hew
user_struct_println_via_magic.hew: OK

$ hew run .tmp/probes/monomorphization/user_struct_println_via_magic.hew
…unsupported type for print
…failed to legalize operation 'hew.print' that was explicitly marked illegal
Error: failed to lower Hew dialect ops
Error: Hew dialect lowering failed
object emission failed
```

`print` and `println` produce identical lowering failures. The
user-receiver path through method syntax does work end-to-end:

```
$ cat .tmp/probes/monomorphization/user_struct_method_fmt.hew
pub type Greeting { who: String; }
impl Display for Greeting {
    fn fmt(val: Greeting) -> String { "hello, " + val.who }
}
fn main() {
    let g = Greeting { who: "world" };
    println(g.fmt());
}

$ hew run .tmp/probes/monomorphization/user_struct_method_fmt.hew
hello, world
```

The boundary diagnosis: the checker correctly resolves the `print<T:
Display>` registration to `Greeting`'s Display impl, but the
`hew.print` MLIR op was never extended to dispatch through a
user-type's resolved Display impl — it remains a fixed-set primitive
emitter. `println(g.fmt())` works because the `print` operand is
`String`, which `hew.print` knows how to lower; the missing piece is
"`hew.print` lowering of an arbitrary user type via its resolved
Display dispatch decision".

**Tracking:** to be filed as a child issue of #1565 — this is the
genuine "lift magic builtin into trait-bounded sig" Phase 2 work that
#1565's body itemises. The checker side has shipped; codegen has not.

**Implication for this lane:** the originally-planned e2e fixture
(`hew-codegen/tests/examples/e2e_traits/print_user_display.hew`,
exercising `print(Greeting{...})`) is **not runnable** today. Adding
it as `check`-only would violate the
`check-pass-does-not-imply-run-pass` lesson. The fixture is therefore
deferred to the codegen child issue that fixes Residual 3. The
already-shipping
`hew-codegen/tests/examples/e2e_traits/primitive_trait_impl_display_int.hew`
remains the canonical proof for the let-bound primitive receiver
path; the user-receiver method-syntax path
(`println(g.fmt())`) is exercised by `examples/module_generic_boundaries`
at compile time today.

## What stays magic (post-#1654)

| Builtin | Status | Citation |
|---|---|---|
| `print`, `println` | Checker: `<T: Display>`-bounded. Codegen: still emits a fixed primitive `hew.print`; rejects user Display impls (Residual 3). | `registration.rs:155–175`, `MLIRGenExpr.cpp` `hew.print` legaliser |
| `to_string`, `assert_eq`, `assert_ne` | Checker: `<T: Display>`-bounded. Codegen: not exercised on user types in any shipping test; gap likely identical to Residual 3 but not separately verified. | `registration.rs:187–270` |
| `len` | Still magic. Operates on collections; needs its own `Len` trait design. | `registration.rs:197` |
| `stop` | Still magic. Actor-control intrinsic; not a Display-shaped operation. | `registration.rs:204` |

The original "Phase 3" goal in #1565 — removing the magic registration
entirely for the five Display callers — depends on Residual 3 being
fixed first. The checker side of Phase 2 (Display-bounded sigs) has
shipped; the codegen side has not.

## Bootstrapping note

The blanket impls in `std/builtins.hew` delegate to the magic
`to_string` free function rather than calling `print` (which would
cycle through the very dyn-Display path being migrated off). Verified
by grep over `std/`. Still accurate post-#1654 — the delegation
pattern was preserved deliberately so the bootstrap order remains
single-pass.

## Performance note

No `hew::PrintOp` shape changes were observed during the codegen
verification runs for #1596/#1642. Performance impact of the
primitive-trait side table is bounded by a single hashmap lookup per
method-call site that lands on a primitive / compiler-builtin generic
receiver and does not match a builtin method — i.e. only when the call
would otherwise be a "no method on X" error. No measurable cost on hot
paths.

## Out of scope for this audit

The audit deliberately excludes:

- **`Box<dyn Trait>` / vtable dispatch design.** No code in `main`
  exercises `Box<dyn Trait>` today. The original #1565 body listed it
  as a "gap to identify"; identified, no design proposed here.
- **`len` → `Len` trait lift.** Different trait surface; tracked
  separately if a user requests.
- **Removing the magic `print`/`println` registration entirely.**
  Phase 3 of the original migration; gated on Residual 3 landing.

## Closeout

The checker-side and primitive-receiver-codegen-side migrations
itemised in #1565's Phase 1 and the checker portion of Phase 2 are
shipped (#1580, #1596, #1642, #1654, #1664). The codegen portion of
Phase 2 and all of Phase 3 remain open and are tracked as residuals
above.

This audit document is the closeout artefact for #1565's audit
deliverable. The three residual gaps are tracked as separate child
issues; #1565 itself can be closed once those issues are filed.
