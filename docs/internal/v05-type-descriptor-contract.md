# v0.5 Type Descriptor Contract

The canonical type descriptor for Hew v0.5 is `TypeDescriptor = ResolvedTy`
(see `hew-types/src/type_descriptor.rs`). There is no parallel representation.

## Mapping table (TypeDescriptor → PrimitiveWireKind)

Every builtin scalar/string/bytes/duration/char/unit maps to a native
`PrimitiveWireKind`; named user types and trait objects map to
`Nested(name)`; functions, closures, pointers, never, tuples, arrays, and slices
are wire-rejected or handled as composites.

## Canonical string

`canonical_string()` on `ResolvedTy` is the single source of truth for:
- monomorphisation symbol mangling (replaces ad-hoc per-call-site string building)
- `PrimitiveWireKind::Nested(name)` carriage on the wire
- IR dump identity in `hew dump-thir` / `hew dump-mir=*`

## Fail-closed rules

1. `wire_kind()` returns `Err(WireBoundaryError::NonWireType)` for wire-rejected
   types — never silently skip.
2. `from_ty_strict_generic_args(ty, n)` rejects a Named type with wrong arity
   with `BoundaryError::GenericArityMismatch` — never silently default.
3. `from_ty()` rejects `Var/Error/IntLiteral/FloatLiteral` — always has.

## Stage 9 API

Stage 9 (`hew-codegen-rs` Rust/Inkwell emitter) should call
`TypeDescriptorWireExt::wire_kind()` for every wire field instead of ad-hoc
type-name string matching. The wire boundary contract lives in `hew-types`
(`TypeDescriptorWireExt` in `hew-types/src/type_descriptor.rs`). The
`hew-wirecodec` crate was retired and is no longer in the workspace.

## Source of truth

`hew-types/src/type_descriptor.rs` (`TypeDescriptor`, `TypeDescriptorWireExt`)
