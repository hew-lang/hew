//! Public catalog of monomorphic builtin enums declared in stdlib `.hew`
//! sources whose layouts must be visible to MIR and codegen without appearing
//! in a program's `HirProgram::items`.
//!
//! Variant order is discriminant ABI. The build script parses the owning
//! declarations through `stdlib_authority`, verifies their ordered ABI
//! fingerprints, and emits this catalog without a second readable variant list.

/// One monomorphic builtin enum.
///
/// `variants` is in declaration order: the index in this slice is the
/// discriminant tag consumed by HIR, MIR, and codegen.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinMonomorphicEnum {
    /// Type name as written in the owning stdlib source.
    pub name: &'static str,
    /// Unit variants in `.hew` declaration order.
    pub variants: &'static [BuiltinMonomorphicEnumVariant],
    /// Whether sandbox bytecode should suppress an otherwise unused descriptor.
    pub suppress_from_sandbox_emit: bool,
}

/// One unit variant of a monomorphic builtin enum.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinMonomorphicEnumVariant {
    /// Variant name.
    pub name: &'static str,
}

include!(concat!(env!("OUT_DIR"), "/builtin_enums.rs"));

/// Catalog of monomorphic builtin enums whose layouts MIR registers
/// out-of-band.
#[must_use]
pub fn monomorphic_builtin_enums() -> &'static [BuiltinMonomorphicEnum] {
    DERIVED_MONOMORPHIC_BUILTIN_ENUMS
}
