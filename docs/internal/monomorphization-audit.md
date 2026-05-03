# Monomorphization audit — primitive trait dispatch

Status: in progress (issue #1565). This document tracks what has shipped,
what remains, and the codegen-seam evidence that bounds the remaining work.

## What shipped (this set of commits)

- **Primitive-keyed trait impl table.** `Checker::primitive_trait_impls` is
  populated whenever `impl Trait for <kind>` registers, where `<kind>` is a
  primitive (`int`, `bool`, `char`, integer/float width aliases, `String`,
  `bytes`, `duration`) or a compiler-builtin generic (`Vec`, `HashMap`,
  `HashSet`).  These receivers have no `type_defs` entry to hang impl
  methods off, so the side table is the only place those signatures live.
  Keyed on the canonical receiver name (`Ty::canonical_lowering_name()`)
  so `int` / `Int` / `i64` collapse to the same slot.

- **Receiver-form method dispatch.**  When `x.fmt()` lands on a primitive
  or compiler-builtin generic receiver and the compiler-builtin method
  registry returns "no match", the side table is consulted before
  reporting "no method `<name>` on `<X>`".  Compiler-builtin methods keep
  precedence — the side table is the *fallback*, not the primary path —
  so the surviving five magic dyn-Display callers (`assert_eq` /
  `assert_ne` / `to_string` / `len` / `stop`) are unaffected.

- **UFCS form (`Trait::method(receiver, args...)`).**  Trait method sigs
  register in `fn_sigs` with the receiver param stripped, so the existing
  `fn_sigs` lookup mis-arities the call.  A new branch ahead of the
  `fn_sigs` lookup intercepts trait-qualified calls whose first arg
  resolves to a primitive or compiler-builtin generic, routes through
  the same side table, and applies the receiver-stripped sig to the
  trailing args.

- **Output-boundary contract for codegen.**  Every dispatched call records
  `MethodCallReceiverKind::PrimitiveTraitImpl { trait_name,
  canonical_receiver }` so codegen can identify the resolved impl from
  the checker output without re-deriving the dispatch decision.  The
  validator at the checker-output boundary retains entries only when
  the trait is still registered and the canonical receiver key is in a
  closed-set known list.

- **Dispatch is receiver-keyed, not trait-name-keyed.**  The lookup goes
  through `Checker::canonical_primitive_or_builtin_key`, which collapses
  user aliases to a single canonical key and never inspects trait-name
  strings.  This avoids hijacking the magic `MarkerTrait::Display` path
  that still gates the surviving five intrinsics.

- **`pub trait Display { fn fmt(val: Self) -> String; }`** is now declared
  in `std/builtins.hew` so user code can implement it on its own types
  without an explicit import.  Blanket impls for `i8`-`i64`, `u8`-`u64`,
  `bool`, and `char` ship alongside; each delegates to the still-magic
  `to_string(self)` helper.

- **Receiver-kind matrix unit coverage** in
  `hew-types/src/check/tests.rs`: `int`, `bool`, `char`, `i32`, `String`
  (via a non-builtin trait method), `Vec`, plus a UFCS sentinel, an
  unknown-method diagnostic sentinel, a builtin-precedence sentinel,
  and a `pub type` regression sentinel that asserts user struct
  receivers continue to dispatch through `type_defs` rather than
  leaking into the primitive table.

## Wire format

`hew-serialize::msgpack::MethodCallReceiverKindData` gains a
`PrimitiveTraitImpl { trait_name, canonical_receiver }` variant
mirroring the checker enum.  Existing wire fixtures continue to
encode and decode without change — the new variant is additive.

## What is blocked (filed as #1593 and #1594)

Re-routing `print` / `println` from the magic `dyn Display` path through
the new generic dispatch — changing `pub fn print(value: dyn Display)` to
`pub fn print<T: Display>(value: T)` in `std/builtins.hew` and removing
the magic override at `hew-types/src/check/registration.rs` that registers
`print` / `println` with `Ty::Var(TypeVar::fresh())` as the single arg —

Attempting this surfaced a checker-side seam:

1. Source signature change alone is a **no-op** at the type-check level.
   The magic override registers `print` / `println` in `fn_sigs` with a
   fresh type variable arg, and that registration wins over the source
   signature for resolution purposes.

2. Removing the magic override produces **typechecker rejection** of
   every existing `print` / `println` call:

   ```
   error: undefined function `println`
   error: undefined function `print`
   ```

   The source-level generic signature `<T: Display>(value: T)` is not
   picked up by the same registration path that `register_builtin_fn`
   provides, so removing the override leaves the fn names completely
   unresolved.

3. Even if the typecheck side were resolved, codegen lowering of
   `x.fmt()` on a primitive receiver is a separate gap.  Today
   `hew run examples/display_int.hew` produces:

   ```
   error: method call on non-struct/enum type
          (method='fmt', receiver type: 'i64')
   ```

   from MLIR generation — codegen has no path for a primitive receiver
   resolving to a user trait method body.

The MLIR diff captured at audit time confirmed `hew::PrintOp` operand
shapes are unchanged when only the source signature changes (the magic
override wins, dispatch goes through the existing `hew::PrintOp`
emitter at unchanged shape).  Removing the override would require
either:

- Wiring source-level generic builtin functions through the same
  `register_builtin_fn` channel so the typechecker picks up
  `<T: Display>` from the source declaration, OR
- Extending `register_builtin_fn` with a generic-with-bound flavor
  that registers `print<T: Display>` directly.

Both change the checker→codegen boundary in non-trivial ways and are
out of scope for the dispatch work in this PR.

## What stays magic

Unchanged in this PR:

- `assert_eq`, `assert_ne`, `to_string`, `len`, `stop` — all five
  continue to resolve through `MarkerTrait::Display` at
  `hew-types/src/check/traits.rs`.
- `print`, `println` — continue to resolve through the magic override
  at `hew-types/src/check/registration.rs` lines 156-157.

## Performance note

No `hew::PrintOp` shape changes were observed during the codegen
verification run, so monomorphization-driven inlining is not in play
yet for primitive-receiver dispatch.  Performance impact of the new
side table is bounded by a single hashmap lookup per method-call site
that lands on a primitive / compiler-builtin generic receiver and
does not match a builtin method — i.e. only when the call would
otherwise be a "no method on X" error.  No measurable cost on hot
paths.

## Bootstrapping note

The blanket impls in `std/builtins.hew` delegate to the magic
`to_string` free function rather than calling `print` (which would
cycle through the very dyn-Display path we are migrating off).  Verified
by grep over `std/`.  When the `print` reroute follow-up lands, the
blanket impl bodies become eligible for monomorphization and the
delegation to `to_string` can be replaced with per-type intrinsics
in the same change.

## Evidence

- Regression probes (now passing):
  - `impl Display for int` — was failing with "no method `fmt` on `int`"
    before this PR; now type-checks cleanly.
  - `impl Display for Vec` — was failing with "no method `fmt` on Vec"
    before this PR; now type-checks cleanly.
  - UFCS `Display::fmt(x)` — was failing with "this function takes 0
    argument(s) but 1 were supplied" before this PR; now type-checks
    cleanly.
  - `print` via magic — was passing via magic before this PR; still passes
    via magic (regression sentinel).

- Fix closes #1580; advances #1565 by closing the primitive-receiver
  dispatch gap that audit issue identifies.

- The `print`/`println` reroute and `x.fmt()` codegen lowering remain
  open work items; filed as #1593 (print re-route) and #1594 (codegen
  lowering on primitive receivers). See those issues for the evidence
  above.
