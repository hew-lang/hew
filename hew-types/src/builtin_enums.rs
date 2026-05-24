//! Public catalog of monomorphic builtin enums declared in
//! `std/builtins.hew` whose layout must be visible to MIR and codegen
//! without appearing in any program's `HirProgram::items` list.
//!
//! ## Why this exists
//!
//! Generic builtin enums (`Option<T>`, `Result<T, E>`) carry their layout
//! into MIR via [`HirModule::enum_layouts`] — one entry per concrete
//! instantiation, registered by the HIR monomorphisation pass through
//! [`crate::monomorph::EnumLayoutRegistry`]. There is no bare-name layout
//! for these generics; an unused `Option<T>` declaration is a non-event.
//!
//! Monomorphic builtin enums (e.g. `LookupError`) are different: they
//! have no type parameters, so the HIR monomorphisation pass never
//! creates per-instantiation entries for them. MIR's `enum_layouts` and
//! `machine_layout_names` therefore have no path to learn the type from
//! the generic-enum lane.
//!
//! Earlier S4 prototypes synthesised a `HirItem::TypeDecl` for every
//! monomorphic builtin enum and pushed it into `HirProgram::items`. That
//! made the type visible to MIR but **leaked across every program** —
//! including ones that never reference the type — and changed the
//! downstream sandbox-VM bytecode descriptor table for every fixture
//! (new `type:LookupError` descriptor, churned `package_id` hash).
//!
//! The fix is to carry the layout **out-of-band**: MIR registers the
//! layout directly from this catalog (parallel to how
//! `hew_hir::builtin_type_classes::builtin_type_registrations` feeds
//! struct-shaped builtins into MIR's `record_layouts`). The
//! `HirProgram::items` list stays a faithful mirror of user source.
//!
//! ## Shape constraints
//!
//! Today every monomorphic builtin enum uses only unit variants (no
//! payload fields). Adding a payloaded variant would require extending
//! [`BuiltinMonomorphicEnumVariant`] with a `field_tys` slot and
//! propagating field types into the MIR layout registration. Keep the
//! API closed to that case until the first real use lands.

/// One monomorphic builtin enum.
///
/// `variants` is in declaration order — the index in this slice matches
/// the discriminant tag value assigned by the HIR ctor pre-pass and the
/// `MachineVariantLayout` index in the MIR `EnumLayout`. Codegen and
/// match-arm dispatch depend on this agreement.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinMonomorphicEnum {
    /// Type name as written in `std/builtins.hew` (e.g. `"LookupError"`).
    pub name: &'static str,
    /// Variant names in declaration order. All payloads are empty (unit
    /// variants) by current substrate contract — see module docs.
    pub variants: &'static [BuiltinMonomorphicEnumVariant],
}

/// One variant of a monomorphic builtin enum. Unit-only today.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinMonomorphicEnumVariant {
    /// Variant name (e.g. `"NotFound"`).
    pub name: &'static str,
}

/// Catalog of monomorphic builtin enums whose layout must be registered
/// out-of-band into MIR's `enum_layouts` and `machine_layout_names`.
///
/// MIR's lower pass calls this from a `register_builtin_monomorphic_enum_layouts`
/// helper, mirroring the precedent set by
/// `hew_hir::builtin_type_classes::builtin_type_registrations` for
/// struct-shaped builtins.
#[must_use]
pub fn monomorphic_builtin_enums() -> &'static [BuiltinMonomorphicEnum] {
    &[BuiltinMonomorphicEnum {
        name: "LookupError",
        variants: &[BuiltinMonomorphicEnumVariant { name: "NotFound" }],
    }]
}
