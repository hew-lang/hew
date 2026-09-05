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
    /// Exact dotted module that owns the declaration.
    pub owner: &'static str,
    /// Type name as written in the owning stdlib source.
    pub name: &'static str,
    /// Exact nominal identity (`owner.name`) used by HIR, MIR, and codegen.
    pub canonical_name: &'static str,
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
pub const fn monomorphic_builtin_enums() -> &'static [BuiltinMonomorphicEnum] {
    DERIVED_MONOMORPHIC_BUILTIN_ENUMS
}

/// Look up one generated monomorphic builtin enum by its declaration leaf or
/// exact canonical identity. Callers use this only to construct a
/// compiler-owned semantic type; source-name resolution must still prove
/// ownership before selecting a catalog entry.
#[must_use]
pub fn monomorphic_builtin_enum(name: &str) -> Option<&'static BuiltinMonomorphicEnum> {
    unique_monomorphic_builtin_enum(monomorphic_builtin_enums(), name)
}

/// Whether a named type carries both axes of generated enum authority: the
/// exact catalog owner and that catalog row's builtin discriminator.
#[must_use]
pub fn has_exact_monomorphic_builtin_enum_identity(
    name: &str,
    builtin: Option<crate::BuiltinType>,
) -> bool {
    monomorphic_builtin_enum(name).is_some_and(|fact| {
        name == fact.canonical_name && crate::lookup_builtin_type(fact.name) == builtin
    }) && builtin.is_some()
}

fn unique_monomorphic_builtin_enum<'a>(
    facts: &'a [BuiltinMonomorphicEnum],
    name: &str,
) -> Option<&'a BuiltinMonomorphicEnum> {
    let mut matches = facts
        .iter()
        .filter(|fact| name == fact.name || name == fact.canonical_name);
    let selected = matches.next()?;
    matches.next().is_none().then_some(selected)
}

/// Construct the checker type for one compiler-owned monomorphic enum with its
/// exact generated owner identity.
#[must_use]
pub fn monomorphic_builtin_enum_ty(name: &str) -> Option<crate::Ty> {
    let fact = monomorphic_builtin_enum(name)?;
    let builtin = crate::lookup_builtin_type(fact.name)?;
    Some(crate::Ty::Named {
        name: fact.canonical_name.to_string(),
        args: Vec::new(),
        builtin: Some(builtin),
    })
}

/// Construct the checker-boundary type for one compiler-owned monomorphic enum
/// with its exact generated owner identity.
#[must_use]
pub fn resolved_monomorphic_builtin_enum_ty(name: &str) -> Option<crate::ResolvedTy> {
    let fact = monomorphic_builtin_enum(name)?;
    let builtin = crate::lookup_builtin_type(fact.name)?;
    Some(crate::ResolvedTy::Named {
        name: fact.canonical_name.to_string(),
        args: Vec::new(),
        builtin: Some(builtin),
        is_opaque: false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_enum_constructors_retain_exact_identity() {
        for fact in monomorphic_builtin_enums() {
            let ty = monomorphic_builtin_enum_ty(fact.name).expect("catalog type");
            let normalized = crate::Ty::normalize_named(fact.canonical_name.to_string(), vec![]);
            let resolved =
                resolved_monomorphic_builtin_enum_ty(fact.name).expect("resolved catalog type");
            assert!(matches!(
                ty,
                crate::Ty::Named { ref name, .. } if name == fact.canonical_name
            ));
            assert!(matches!(
                resolved,
                crate::ResolvedTy::Named { ref name, .. } if name == fact.canonical_name
            ));
            assert!(matches!(
                normalized,
                crate::Ty::Named {
                    ref name,
                    builtin: Some(_),
                    ..
                } if name == fact.canonical_name
            ));
        }
    }

    #[test]
    fn duplicate_leaf_catalog_lookup_fails_closed() {
        const VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[];
        let facts = [
            BuiltinMonomorphicEnum {
                owner: "std.left",
                name: "Collision",
                canonical_name: "std.left.Collision",
                variants: VARIANTS,
                suppress_from_sandbox_emit: false,
            },
            BuiltinMonomorphicEnum {
                owner: "std.right",
                name: "Collision",
                canonical_name: "std.right.Collision",
                variants: VARIANTS,
                suppress_from_sandbox_emit: false,
            },
        ];
        assert!(unique_monomorphic_builtin_enum(&facts, "Collision").is_none());
        assert_eq!(
            unique_monomorphic_builtin_enum(&facts, "std.left.Collision")
                .map(|fact| fact.canonical_name),
            Some("std.left.Collision"),
            "an exact canonical identity remains unambiguous"
        );
    }

    #[test]
    fn exact_enum_authority_requires_owner_and_discriminator_independently() {
        let fact = monomorphic_builtin_enum("CrashAction").expect("CrashAction catalog row");
        let discriminator = crate::lookup_builtin_type(fact.name).expect("builtin discriminator");
        assert!(has_exact_monomorphic_builtin_enum_identity(
            fact.canonical_name,
            Some(discriminator)
        ));
        for missing_or_wrong in [None, Some(crate::BuiltinType::AskError)] {
            assert!(!has_exact_monomorphic_builtin_enum_identity(
                fact.canonical_name,
                missing_or_wrong
            ));
        }
        for leaf_or_foreign in [fact.name, "foreign.CrashAction"] {
            assert!(!has_exact_monomorphic_builtin_enum_identity(
                leaf_or_foreign,
                Some(discriminator)
            ));
        }
    }
}
