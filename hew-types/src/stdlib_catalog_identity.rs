//! Checker-visible identities for executable monomorphic stdlib catalog calls.
//!
//! The HIR stdlib catalog owns linkage and ABI details, while this small
//! projection is the checker-side authority for the catalog *identity* that a
//! direct call carries through HIR and MIR.  It intentionally excludes generic
//! surfaces (such as `Vec::new`) and `CompilerIntrinsic` rows, which have their
//! own checked lowering paths.  The HIR inventory gate verifies this stays in
//! lockstep with every catalog row that the checker can accept monomorphically.

/// Closed identities of catalog callables that are both monomorphic in the
/// checker and executable as ordinary HIR/MIR calls.
pub const MONOMORPHIC_CALLABLE_IDENTITIES: &[&str] = &[
    "Node::allow_peer",
    "Node::connect",
    "Node::id",
    "Node::identity_key",
    "Node::load_keys",
    "Node::set_transport",
    "Node::shutdown",
    "Node::start",
    "abs",
    "assert",
    "bytes::new",
    "ceil",
    "exit",
    "floor",
    "max",
    "min",
    "panic",
    "pow",
    "print_bool",
    "print_f64",
    "print_str",
    "println_bool",
    "println_f64",
    "println_i64",
    "println_str",
    "random.gauss",
    "random.randint",
    "random.random",
    "random.seed",
    "round",
    "sleep",
    "sleep_until",
    "sqrt",
    "string_concat",
];

/// Return the exact catalog identity for a checker-registered executable
/// monomorphic builtin.  The returned value is the catalog key, not a source
/// spelling recovered by a downstream phase.
#[must_use]
pub fn monomorphic_callable_identity(name: &str) -> Option<&'static str> {
    MONOMORPHIC_CALLABLE_IDENTITIES
        .iter()
        .copied()
        .find(|identity| *identity == name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_has_a_canonical_executable_catalog_identity() {
        assert_eq!(monomorphic_callable_identity("assert"), Some("assert"));
    }

    #[test]
    fn user_and_generic_surface_spellings_are_not_catalog_endpoints() {
        assert_eq!(monomorphic_callable_identity("user_assert"), None);
        assert_eq!(monomorphic_callable_identity("Vec::new"), None);
    }
}
