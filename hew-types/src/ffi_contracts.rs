//! Machine-checked ownership facts for C-ABI extern symbols.
//!
//! `scripts/jit-symbol-classification.toml` is the single source of truth.
//! `hew-types/build.rs` projects it here so HIR can validate an extern
//! resource boundary before MIR lowering, while MIR consumes the exact same
//! table through its re-export.  An absent row is deliberately not a borrow.

/// Ownership disposition of one C-ABI parameter.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternParamOwnership {
    /// The callee only reads or copies this parameter.
    Borrow,
    /// The callee takes this owner and discharges the caller's obligation.
    Consume,
    /// The callee retains an additional reference.
    Retain,
}

/// Ownership disposition of a C-ABI result.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternResultOwnership {
    Fresh,
    Retained,
    Borrowed,
    None,
}

/// Recursive depth of a result's balancing release.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReleaseDischargeDepth {
    Shallow,
    Deep,
    None,
}

/// Whether the foreign callee retained a pointer into an owned result.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternResultRetention {
    Transferred,
    Unspecified,
}

/// Complete contract for one FFI symbol.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExternOwnershipContract {
    pub params: &'static [ExternParamOwnership],
    /// Qualified resource nominal for each parameter. An empty entry is a
    /// non-resource slot. This prevents a symbol-only BORROW row from being
    /// inherited by an unrelated user-defined resource with the same ABI word
    /// shape.
    pub resource_param_types: &'static [&'static str],
    pub result: ExternResultOwnership,
    pub release_symbol: &'static str,
    pub discharge_depth: ReleaseDischargeDepth,
    pub result_retention: ExternResultRetention,
}

/// Result of a contract lookup. Absence stays explicit and fail-closed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternOwnershipFact {
    Contract(&'static ExternOwnershipContract),
    Absent,
}

impl ExternOwnershipFact {
    #[must_use]
    pub const fn is_contract(self) -> bool {
        matches!(self, Self::Contract(_))
    }

    #[must_use]
    pub const fn contract(self) -> Option<&'static ExternOwnershipContract> {
        match self {
            Self::Contract(contract) => Some(contract),
            Self::Absent => None,
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/ffi_ownership_contracts.rs"));

/// Return the contract for `symbol`, or the explicit fail-closed absence fact.
#[must_use]
pub fn extern_ownership_contract(symbol: &str) -> ExternOwnershipFact {
    match FFI_OWNERSHIP_CONTRACTS.binary_search_by(|(row, _)| (*row).cmp(symbol)) {
        Ok(index) => ExternOwnershipFact::Contract(&FFI_OWNERSHIP_CONTRACTS[index].1),
        Err(_) => ExternOwnershipFact::Absent,
    }
}

/// Return one parameter's audited ownership disposition.
///
/// `None` covers an unknown symbol and an out-of-range parameter alike. Both
/// must be rejected at a resource boundary rather than defaulted to borrow.
#[must_use]
pub fn extern_param_ownership(symbol: &str, index: usize) -> Option<ExternParamOwnership> {
    extern_ownership_contract(symbol)
        .contract()
        .and_then(|contract| contract.params.get(index).copied())
}

/// Whether a source declaration may borrow this exact resource parameter.
///
/// Scalar C-ABI handles make `(symbol, index)` insufficient authority: a root
/// module could otherwise redeclare `hew_tcp_read(Foo)` and borrow an arbitrary
/// `#[resource] Foo`. The positive fact therefore binds the contract row to
/// both its qualified nominal and the module that owns that nominal. The owner
/// module is derived from the generated row itself; it is not a TCP-specific
/// spelling allowlist, so a future independently-audited resource cannot
/// silently need a second authority path. All mismatches remain
/// consuming/fail-closed.
#[must_use]
pub fn extern_resource_param_is_audited_borrow(
    symbol: &str,
    index: usize,
    declaring_module: Option<&str>,
    resource_type: &str,
) -> bool {
    let Some(contract) = extern_ownership_contract(symbol).contract() else {
        return false;
    };
    resource_param_is_audited_borrow_for_contract(contract, index, declaring_module, resource_type)
}

/// Compare a source resource boundary against one generated contract row.
///
/// Kept separately so the exact nominal/provenance rule has a synthetic test
/// without adding unaudited product rows to the classification table.
fn resource_param_is_audited_borrow_for_contract(
    contract: &ExternOwnershipContract,
    index: usize,
    declaring_module: Option<&str>,
    resource_type: &str,
) -> bool {
    if contract.params.get(index) != Some(&ExternParamOwnership::Borrow) {
        return false;
    }
    let Some(expected) = contract.resource_param_types.get(index) else {
        return false;
    };
    let Some((expected_module, _)) = expected.rsplit_once('.') else {
        return false;
    };
    if expected.is_empty() || declaring_module != Some(expected_module) {
        return false;
    }
    // HIR preserves a same-module source nominal as either a bare type
    // (`Connection`) or its module's local short qualifier (`net.Connection`),
    // while imported references may carry `std.net.Connection`. Normalize only
    // those two source-local spellings through the already-proven declaration
    // module, then require exact qualified nominal equality.
    let expected_module_short = expected_module
        .rsplit_once('.')
        .map_or(expected_module, |(_, short)| short);
    let actual = if resource_type.contains('.') {
        let local_prefix = format!("{expected_module_short}.");
        resource_type.strip_prefix(&local_prefix).map_or_else(
            || resource_type.to_string(),
            |name| format!("{expected_module}.{name}"),
        )
    } else {
        format!("{expected_module}.{resource_type}")
    };
    actual == *expected
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tcp_contracts_name_borrow_and_consume_per_parameter() {
        assert_eq!(
            extern_param_ownership("hew_tcp_read", 0),
            Some(ExternParamOwnership::Borrow)
        );
        assert_eq!(
            extern_param_ownership("hew_tcp_close", 0),
            Some(ExternParamOwnership::Consume)
        );
        assert_eq!(extern_param_ownership("hew_tcp_unclassified", 0), None);
        assert!(extern_resource_param_is_audited_borrow(
            "hew_tcp_read",
            0,
            Some("std.net"),
            "Connection",
        ));
        assert!(extern_resource_param_is_audited_borrow(
            "hew_tcp_read",
            0,
            Some("std.net"),
            "net.Connection",
        ));
        assert!(!extern_resource_param_is_audited_borrow(
            "hew_tcp_read",
            0,
            Some("std.net"),
            "Listener",
        ));
        assert!(!extern_resource_param_is_audited_borrow(
            "hew_tcp_read",
            0,
            Some("user.net"),
            "Connection",
        ));
        assert!(!extern_resource_param_is_audited_borrow(
            "hew_tcp_read",
            0,
            Some("std.net"),
            "user.net.Connection",
        ));
    }

    #[test]
    fn audited_borrow_derives_provenance_from_each_qualified_nominal() {
        // This is deliberately a synthetic contract, not a new product row:
        // it proves the carrier handles any future audited resource module
        // without retaining `std.net` as a hidden second authority.
        let contract = ExternOwnershipContract {
            params: &[ExternParamOwnership::Borrow],
            resource_param_types: &["example.io.Socket"],
            result: ExternResultOwnership::None,
            release_symbol: "",
            discharge_depth: ReleaseDischargeDepth::None,
            result_retention: ExternResultRetention::Unspecified,
        };
        assert!(resource_param_is_audited_borrow_for_contract(
            &contract,
            0,
            Some("example.io"),
            "Socket",
        ));
        assert!(resource_param_is_audited_borrow_for_contract(
            &contract,
            0,
            Some("example.io"),
            "example.io.Socket",
        ));
        assert!(resource_param_is_audited_borrow_for_contract(
            &contract,
            0,
            Some("example.io"),
            "io.Socket",
        ));
        assert!(!resource_param_is_audited_borrow_for_contract(
            &contract,
            0,
            Some("example.other"),
            "Socket",
        ));
        assert!(!resource_param_is_audited_borrow_for_contract(
            &contract,
            0,
            Some("example.io"),
            "Pipe",
        ));
    }

    #[test]
    fn tcp_handle_producers_are_fresh_only_with_their_exact_close_ritual() {
        for (symbol, release_symbol) in [
            ("hew_tcp_accept", "hew_tcp_close"),
            ("hew_tcp_connect", "hew_tcp_close"),
            ("hew_tcp_connect_timeout", "hew_tcp_close"),
            ("hew_tcp_listen", "hew_tcp_listener_close"),
        ] {
            let Some(contract) = extern_ownership_contract(symbol).contract() else {
                panic!("{symbol} must carry an ownership contract");
            };
            assert_eq!(contract.result, ExternResultOwnership::Fresh, "{symbol}");
            assert_eq!(contract.release_symbol, release_symbol, "{symbol}");
            assert_eq!(
                contract.discharge_depth,
                ReleaseDischargeDepth::Shallow,
                "{symbol}"
            );
            assert_eq!(
                contract.result_retention,
                ExternResultRetention::Transferred,
                "{symbol} must prove it transfers the returned TCP owner"
            );
        }

        let stream_bridge = extern_ownership_contract("hew_tcp_stream_from_conn")
            .contract()
            .expect("stream bridge contract");
        assert_eq!(
            stream_bridge.result_retention,
            ExternResultRetention::Transferred,
            "the bridge's temporary pair owner must not retain aliases after it is handed to Hew"
        );

        assert!(
            !matches!(
                extern_ownership_contract("hew_tcp_unclassified").contract(),
                Some(contract) if contract.result == ExternResultOwnership::Fresh
            ),
            "an absent producer row must never default to a fresh resource owner"
        );
        assert_ne!(
            extern_ownership_contract("hew_tcp_close")
                .contract()
                .expect("close contract")
                .result,
            ExternResultOwnership::Fresh,
            "a TCP spelling alone must not turn a disposer into a resource producer"
        );

        assert_eq!(
            extern_param_ownership("hew_tcp_attach_local", 0),
            Some(ExternParamOwnership::Consume),
            "active-mode attach transfers the connection's sole close authority to the reactor"
        );
    }
}
