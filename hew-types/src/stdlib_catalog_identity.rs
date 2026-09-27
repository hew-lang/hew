//! Checker-visible identities for executable monomorphic stdlib catalog calls.
//!
//! The HIR stdlib catalog owns linkage and ABI details, while this small
//! projection is the checker-side authority for the catalog *identity* that a
//! direct call carries through HIR and MIR.  It intentionally excludes generic
//! surfaces (such as `Vec::new`) and `CompilerIntrinsic` rows, which have their
//! own checked lowering paths.  The HIR inventory gate verifies this stays in
//! lockstep with every catalog row that the checker can accept monomorphically.

/// The catalog identity of the `assert` builtin, which HIR desugars.
pub const ASSERT: &str = "assert";

/// Closed identities of catalog callables that are both monomorphic in the
/// checker and executable as ordinary HIR/MIR calls.
pub const MONOMORPHIC_CALLABLE_IDENTITIES: &[&str] = &[
    "Node::connect",
    "Node::id",
    "Node::identity_key",
    "Node::shutdown",
    "Node::start",
    ASSERT,
    "bytes::new",
    "exit",
    "panic",
    "print_bool",
    "print_f64",
    "print_str",
    "println_bool",
    "println_f64",
    "println_i64",
    "println_str",
    "sleep",
    "sleep_until",
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

/// Return a compiler-synthetic catalog endpoint whose source surface is an
/// inherent method on an identity carrier rather than an ordinary free
/// function.  These calls deliberately do not appear in
/// [`MONOMORPHIC_CALLABLE_IDENTITIES`]: that projection is checked against
/// `fn_sigs`, while these endpoints are admitted through checker-published
/// method declarations.  Keeping the closed list here lets the checker pass
/// the catalog identity forward without inventing a runtime family or relying
/// on a method body's registration.
#[must_use]
pub fn compiler_synthetic_identity_endpoint(name: &str) -> Option<&str> {
    match name {
        "hew_node_id_display"
        | "hew_location_node_id"
        | "hew_location_slot"
        | "hew_location_incarnation"
        | "hew_location_display"
        | "hew_remote_pid_location"
        | "hew_remote_pid_node_id"
        | "hew_remote_pid_slot"
        | "hew_remote_pid_incarnation"
        | "hew_remote_pid_display" => Some(name),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every endpoint this module admits reaches SIR as a runtime family with
    /// a semantic contract. Admitting one without a contract is what left the
    /// identity accessors refusing at the SIR boundary (#3442).
    #[test]
    fn every_admitted_identity_endpoint_has_a_runtime_contract() {
        for endpoint in [
            "hew_node_id_display",
            "hew_location_node_id",
            "hew_location_slot",
            "hew_location_incarnation",
            "hew_location_display",
            "hew_remote_pid_location",
            "hew_remote_pid_node_id",
            "hew_remote_pid_slot",
            "hew_remote_pid_incarnation",
            "hew_remote_pid_display",
        ] {
            assert_eq!(
                compiler_synthetic_identity_endpoint(endpoint),
                Some(endpoint),
                "{endpoint} must stay an admitted synthetic identity endpoint"
            );
            let family = crate::runtime_call::RuntimeCallFamily::from_catalog_endpoint(endpoint)
                .unwrap_or_else(|| panic!("{endpoint} has no runtime family"));
            assert_eq!(
                family.row().symbol,
                endpoint,
                "{endpoint} must map to the family carrying its own symbol"
            );
            assert!(
                family.semantic_contract().is_some(),
                "{endpoint} has a family but no ownership-SIR contract"
            );
        }
    }

    #[test]
    fn assert_has_a_canonical_executable_catalog_identity() {
        assert_eq!(monomorphic_callable_identity("assert"), Some("assert"));
    }

    #[test]
    fn user_and_generic_surface_spellings_are_not_catalog_endpoints() {
        assert_eq!(monomorphic_callable_identity("user_assert"), None);
        assert_eq!(monomorphic_callable_identity("Vec::new"), None);
    }

    #[test]
    fn identity_carrier_synthetics_are_explicit_catalog_endpoints() {
        assert_eq!(
            compiler_synthetic_identity_endpoint("hew_remote_pid_node_id"),
            Some("hew_remote_pid_node_id")
        );
        assert_eq!(compiler_synthetic_identity_endpoint("user_symbol"), None);
    }
}
