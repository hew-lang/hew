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
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Serialize)]
pub enum ExternResultOwnership {
    Fresh,
    Retained,
    Borrowed,
    None,
}

/// Recursive depth of a result's balancing release.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, serde::Serialize)]
pub enum ReleaseDischargeDepth {
    Shallow,
    Deep,
    None,
}

/// The measured aliasing disposition of an owned foreign result.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Serialize)]
pub enum ExternResultRetention {
    /// The caller receives the sole owner of the allocation.
    Transferred,
    /// The caller receives an independently releasable refcount share of an
    /// allocation that remains aliased by another owner.
    SharedRefcount,
    /// The caller receives the only close authority for a fresh opaque
    /// resource allocation or runtime table token.
    ResourceTransfer,
    Unspecified,
}

impl ExternResultRetention {
    /// Whether one caller-side release is proven to balance this result.
    #[must_use]
    pub const fn authorizes_caller_release(self) -> bool {
        matches!(
            self,
            Self::Transferred | Self::SharedRefcount | Self::ResourceTransfer
        )
    }

    /// Whether this result is a directly releasable allocation/refcount share,
    /// rather than an opaque resource close authority.
    #[must_use]
    pub const fn authorizes_direct_allocation_release(self) -> bool {
        matches!(self, Self::Transferred | Self::SharedRefcount)
    }
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
    /// Qualified source nominal for an independently-owned opaque result.
    ///
    /// This is present only when the same generated contract also proves an
    /// owned result, measured resource transfer, and an exact consuming
    /// release edge for this nominal.
    pub resource_result_type: Option<&'static str>,
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

/// Typed producer-side projection for a closeable opaque candidate.
///
/// The owner module is derived from the qualified result nominal. Callers
/// still have to prove that a source extern declaration belongs to that exact
/// module and has the matching resolved result type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExternOwnedResourceResult {
    pub resource_type: &'static str,
    pub owner_module: &'static str,
    pub release_symbol: &'static str,
    pub discharge_depth: ReleaseDischargeDepth,
    pub result: ExternResultOwnership,
    pub result_retention: ExternResultRetention,
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

/// Return the typed owned-resource result projected from one generated row.
///
/// Invalid schema combinations are rejected by the build-time parser. This
/// helper remains defensive so a synthetic or stale in-memory row cannot mint
/// a candidate by carrying only a type spelling.
#[must_use]
pub fn extern_owned_resource_result(symbol: &str) -> Option<ExternOwnedResourceResult> {
    let contract = extern_ownership_contract(symbol).contract()?;
    owned_resource_result_for_contract(contract)
}

pub(crate) fn owned_resource_result_for_contract(
    contract: &ExternOwnershipContract,
) -> Option<ExternOwnedResourceResult> {
    let resource_type = contract.resource_result_type?;
    let (owner_module, _) = resource_type.rsplit_once('.')?;
    if owner_module.is_empty()
        || !matches!(
            contract.result,
            ExternResultOwnership::Fresh | ExternResultOwnership::Retained
        )
        || contract.release_symbol.is_empty()
        || contract.discharge_depth == ReleaseDischargeDepth::None
        || contract.result_retention != ExternResultRetention::ResourceTransfer
    {
        return None;
    }
    Some(ExternOwnedResourceResult {
        resource_type,
        owner_module,
        release_symbol: contract.release_symbol,
        discharge_depth: contract.discharge_depth,
        result: contract.result,
        result_retention: contract.result_retention,
    })
}

/// Whether a source nominal is the exact local spelling of `qualified`.
///
/// Exact declaring-module provenance is mandatory. Within that module Hew
/// source may spell its own nominal as `T`, `leaf.T`, or the full
/// `owner.path.T`; no other short-name normalization is admitted.
#[must_use]
pub fn source_nominal_matches_qualified(
    qualified: &str,
    declaring_module: Option<&str>,
    source_type: &str,
) -> bool {
    // An already fully-qualified nominal carries its own exact source owner.
    // This is the cross-module re-declaration case used by std adapters (for
    // example `std.net` borrowing `std.stream.StreamPair`).  Requiring the
    // extern block itself to live in the resource's declaring module would
    // discard that stronger type provenance and turn audited borrows into
    // moves.  A same-leaf foreign type still cannot pass this equality.
    if source_type == qualified {
        return true;
    }
    let Some((expected_module, _)) = qualified.rsplit_once('.') else {
        return false;
    };
    if expected_module.is_empty() || declaring_module != Some(expected_module) {
        return false;
    }
    let expected_module_short = expected_module
        .rsplit_once('.')
        .map_or(expected_module, |(_, short)| short);
    let actual = if source_type.contains('.') {
        let local_prefix = format!("{expected_module_short}.");
        source_type.strip_prefix(&local_prefix).map_or_else(
            || source_type.to_string(),
            |name| format!("{expected_module}.{name}"),
        )
    } else {
        format!("{expected_module}.{source_type}")
    };
    actual == qualified
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
    source_nominal_matches_qualified(expected, declaring_module, resource_type)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tcp_contracts_name_borrow_and_consume_per_parameter() {
        let read_contract = extern_ownership_contract("hew_tcp_read")
            .contract()
            .expect("TCP read contract");
        assert_eq!(
            extern_param_ownership("hew_tcp_read", 0),
            Some(ExternParamOwnership::Borrow)
        );
        assert_eq!(read_contract.result, ExternResultOwnership::Fresh);
        assert_eq!(read_contract.release_symbol, "hew_bytes_drop");
        assert_eq!(
            read_contract.result_retention,
            ExternResultRetention::Transferred
        );
        assert_eq!(
            extern_param_ownership("hew_tcp_close", 0),
            Some(ExternParamOwnership::Consume)
        );
        assert_eq!(
            extern_param_ownership("hew_observe_read_u64", 0),
            Some(ExternParamOwnership::Borrow)
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
            resource_result_type: None,
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
    fn owned_result_projection_is_qualified_and_provenance_bound() {
        let contract = ExternOwnershipContract {
            params: &[],
            resource_param_types: &[],
            resource_result_type: Some("example.io.Socket"),
            result: ExternResultOwnership::Fresh,
            release_symbol: "example_socket_close",
            discharge_depth: ReleaseDischargeDepth::Shallow,
            result_retention: ExternResultRetention::ResourceTransfer,
        };
        let result =
            owned_resource_result_for_contract(&contract).expect("valid synthetic producer");
        assert_eq!(result.resource_type, "example.io.Socket");
        assert_eq!(result.owner_module, "example.io");
        assert!(source_nominal_matches_qualified(
            result.resource_type,
            Some(result.owner_module),
            "Socket"
        ));
        assert!(source_nominal_matches_qualified(
            result.resource_type,
            Some(result.owner_module),
            "io.Socket"
        ));
        assert!(source_nominal_matches_qualified(
            result.resource_type,
            Some(result.owner_module),
            "example.io.Socket"
        ));
        assert!(source_nominal_matches_qualified(
            result.resource_type,
            Some("adapter.net"),
            "example.io.Socket"
        ));
        assert!(!source_nominal_matches_qualified(
            result.resource_type,
            None,
            "Socket"
        ));
        assert!(!source_nominal_matches_qualified(
            result.resource_type,
            Some("other.io"),
            "Socket"
        ));
        assert!(!source_nominal_matches_qualified(
            result.resource_type,
            Some(result.owner_module),
            "Pipe"
        ));
        assert!(!source_nominal_matches_qualified(
            result.resource_type,
            Some("adapter.net"),
            "other.io.Socket"
        ));
    }

    #[test]
    fn tcp_handle_producers_are_fresh_only_with_their_exact_close_ritual() {
        for (symbol, resource_type, release_symbol) in [
            ("hew_tcp_accept", "std.net.Connection", "hew_tcp_close"),
            ("hew_tcp_connect", "std.net.Connection", "hew_tcp_close"),
            (
                "hew_tcp_connect_timeout",
                "std.net.Connection",
                "hew_tcp_close",
            ),
            (
                "hew_tcp_listen",
                "std.net.Listener",
                "hew_tcp_listener_close",
            ),
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
                ExternResultRetention::ResourceTransfer,
                "{symbol} must prove it transfers the returned TCP close authority"
            );
            let typed = extern_owned_resource_result(symbol)
                .unwrap_or_else(|| panic!("{symbol} must carry a typed resource result"));
            assert_eq!(typed.resource_type, resource_type, "{symbol}");
            assert_eq!(typed.owner_module, "std.net", "{symbol}");
            assert_eq!(typed.release_symbol, release_symbol, "{symbol}");
        }

        let stream_bridge = extern_ownership_contract("hew_tcp_stream_from_conn")
            .contract()
            .expect("stream bridge contract");
        assert_eq!(
            stream_bridge.result_retention,
            ExternResultRetention::ResourceTransfer,
            "the bridge must transfer the returned pair's sole close authority"
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

    #[test]
    fn string_to_bytes_carries_the_measured_transfer_contract() {
        let contract = extern_ownership_contract("hew_string_to_bytes")
            .contract()
            .expect("string-to-bytes contract");
        assert_eq!(contract.params, &[ExternParamOwnership::Borrow]);
        assert!(contract.resource_param_types.is_empty());
        assert_eq!(contract.resource_result_type, None);
        assert_eq!(contract.result, ExternResultOwnership::Fresh);
        assert_eq!(contract.release_symbol, "hew_bytes_drop");
        assert_eq!(contract.discharge_depth, ReleaseDischargeDepth::Shallow);
        assert_eq!(
            contract.result_retention,
            ExternResultRetention::Transferred
        );
    }

    #[test]
    fn string_vec_producers_carry_truthful_transfer_contracts() {
        for (symbol, params, discharge_depth) in [
            ("hew_string_chars", 1, ReleaseDischargeDepth::Shallow),
            ("hew_string_lines", 1, ReleaseDischargeDepth::Deep),
            ("hew_string_split", 2, ReleaseDischargeDepth::Deep),
        ] {
            let contract = extern_ownership_contract(symbol)
                .contract()
                .unwrap_or_else(|| panic!("{symbol} contract"));
            assert_eq!(contract.params.len(), params, "{symbol}");
            assert!(
                contract
                    .params
                    .iter()
                    .all(|param| *param == ExternParamOwnership::Borrow),
                "{symbol}"
            );
            assert_eq!(contract.result, ExternResultOwnership::Fresh, "{symbol}");
            assert_eq!(contract.release_symbol, "hew_vec_free", "{symbol}");
            assert_eq!(contract.discharge_depth, discharge_depth, "{symbol}");
            assert_eq!(
                contract.result_retention,
                ExternResultRetention::Transferred,
                "{symbol}"
            );
        }
    }

    #[test]
    fn stream_pair_extractors_transfer_the_returned_handle() {
        for (symbol, release_symbol) in [
            ("hew_stream_pair_sink", "hew_sink_close"),
            ("hew_stream_pair_sink_bytes", "hew_sink_close"),
            ("hew_stream_pair_stream", "hew_stream_close"),
            ("hew_stream_pair_stream_bytes", "hew_stream_close"),
        ] {
            let contract = extern_ownership_contract(symbol)
                .contract()
                .unwrap_or_else(|| panic!("{symbol} contract"));
            assert_eq!(contract.result, ExternResultOwnership::Fresh, "{symbol}");
            assert_eq!(contract.release_symbol, release_symbol, "{symbol}");
            assert_eq!(
                contract.result_retention,
                ExternResultRetention::Transferred,
                "{symbol} must transfer the extracted handle"
            );
        }
    }

    #[test]
    fn stdlib_byte_producers_transfer_the_returned_buffer() {
        for symbol in [
            "hew_random_bytes_hew",
            "hew_deflate_compress_hew",
            "hew_deflate_decompress_hew",
            "hew_gzip_compress_hew",
            "hew_gzip_decompress_hew",
            "hew_zlib_compress_hew",
            "hew_zlib_decompress_hew",
            "hew_msgpack_encode_bytes_hew",
            "hew_msgpack_encode_int_hew",
            "hew_msgpack_encode_string_hew",
            "hew_msgpack_from_json_hew",
            "hew_ed25519_generate_pkcs8_hew",
            "hew_ed25519_public_key_from_pkcs8_hew",
            "hew_tcp_read",
        ] {
            let contract = extern_ownership_contract(symbol)
                .contract()
                .unwrap_or_else(|| panic!("{symbol} contract"));
            assert_eq!(contract.result, ExternResultOwnership::Fresh, "{symbol}");
            assert_eq!(contract.release_symbol, "hew_bytes_drop", "{symbol}");
            assert_eq!(
                contract.result_retention,
                ExternResultRetention::Transferred,
                "{symbol} must transfer the returned buffer"
            );
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "the exhaustive table intentionally keeps every value-tree ABI row visible"
    )]
    fn value_tree_resource_parameters_are_complete_and_nominal() {
        let families = [
            (
                "std.encoding.json",
                "std.encoding.json.Value",
                &[
                    (
                        "hew_json_array_get",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_json_array_len", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_json_array_push",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Consume][..],
                    ),
                    (
                        "hew_json_array_push_bool",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_json_array_push_float",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_json_array_push_int",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_json_array_push_null",
                        &[ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_json_array_push_string",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_json_free", &[ExternParamOwnership::Consume][..]),
                    ("hew_json_get_bool", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_json_get_field",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_json_get_float", &[ExternParamOwnership::Borrow][..]),
                    ("hew_json_get_int", &[ExternParamOwnership::Borrow][..]),
                    ("hew_json_get_string", &[ExternParamOwnership::Borrow][..]),
                    ("hew_json_int_status", &[ExternParamOwnership::Borrow][..]),
                    ("hew_json_object_keys", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_json_object_set",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Consume,
                        ][..],
                    ),
                    (
                        "hew_json_object_set_bool",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_json_object_set_float",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_json_object_set_int",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_json_object_set_null",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_json_object_set_string",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    ("hew_json_stringify", &[ExternParamOwnership::Borrow][..]),
                    ("hew_json_type", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_json_unrepresentable_int_count",
                        &[ExternParamOwnership::Borrow][..],
                    ),
                ][..],
            ),
            (
                "std.encoding.toml",
                "std.encoding.toml.Value",
                &[
                    (
                        "hew_toml_array_get",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_toml_array_len", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_toml_array_push",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Consume][..],
                    ),
                    (
                        "hew_toml_array_push_bool",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_toml_array_push_float",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_toml_array_push_int",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_toml_array_push_string",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_toml_free", &[ExternParamOwnership::Consume][..]),
                    ("hew_toml_get_bool", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_toml_get_field",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_toml_get_float", &[ExternParamOwnership::Borrow][..]),
                    ("hew_toml_get_int", &[ExternParamOwnership::Borrow][..]),
                    ("hew_toml_get_string", &[ExternParamOwnership::Borrow][..]),
                    ("hew_toml_stringify", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_toml_table_set",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Consume,
                        ][..],
                    ),
                    (
                        "hew_toml_table_set_bool",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_toml_table_set_float",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_toml_table_set_int",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_toml_table_set_string",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    ("hew_toml_type", &[ExternParamOwnership::Borrow][..]),
                ][..],
            ),
            (
                "std.encoding.yaml",
                "std.encoding.yaml.Value",
                &[
                    (
                        "hew_yaml_array_get",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_yaml_array_len", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_yaml_array_push",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Consume][..],
                    ),
                    (
                        "hew_yaml_array_push_bool",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_yaml_array_push_float",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_yaml_array_push_int",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_yaml_array_push_null",
                        &[ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_yaml_array_push_string",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_yaml_free", &[ExternParamOwnership::Consume][..]),
                    ("hew_yaml_get_bool", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_yaml_get_field",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    ("hew_yaml_get_float", &[ExternParamOwnership::Borrow][..]),
                    ("hew_yaml_get_int", &[ExternParamOwnership::Borrow][..]),
                    ("hew_yaml_get_string", &[ExternParamOwnership::Borrow][..]),
                    ("hew_yaml_int_status", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_yaml_object_set",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Consume,
                        ][..],
                    ),
                    (
                        "hew_yaml_object_set_bool",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_yaml_object_set_float",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_yaml_object_set_int",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    (
                        "hew_yaml_object_set_null",
                        &[ExternParamOwnership::Borrow, ExternParamOwnership::Borrow][..],
                    ),
                    (
                        "hew_yaml_object_set_string",
                        &[
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                            ExternParamOwnership::Borrow,
                        ][..],
                    ),
                    ("hew_yaml_stringify", &[ExternParamOwnership::Borrow][..]),
                    ("hew_yaml_type", &[ExternParamOwnership::Borrow][..]),
                    (
                        "hew_yaml_unrepresentable_int_count",
                        &[ExternParamOwnership::Borrow][..],
                    ),
                ][..],
            ),
        ];

        for (module, nominal, rows) in families {
            for (symbol, expected_params) in rows {
                let contract = extern_ownership_contract(symbol)
                    .contract()
                    .unwrap_or_else(|| panic!("{symbol} must have a complete contract"));
                assert_eq!(contract.params, *expected_params, "{symbol}");
                assert_eq!(
                    contract.resource_param_types.len(),
                    contract.params.len(),
                    "{symbol} must classify every parameter slot"
                );
                for (index, disposition) in expected_params.iter().enumerate() {
                    let expected_nominal =
                        match (symbol.ends_with("_set"), symbol.ends_with("_push"), index) {
                            (true, _, 2) | (_, true, 1) | (_, _, 0) => nominal,
                            _ => "",
                        };
                    assert_eq!(
                        contract.resource_param_types[index], expected_nominal,
                        "{symbol} parameter {index}"
                    );
                    if *disposition == ExternParamOwnership::Borrow && !expected_nominal.is_empty()
                    {
                        assert!(extern_resource_param_is_audited_borrow(
                            symbol,
                            index,
                            Some(module),
                            nominal,
                        ));
                    }
                }
            }
        }
    }

    #[test]
    fn value_tree_producers_transfer_one_deep_owner() {
        for (prefix, release, symbols) in [
            (
                "json",
                "hew_json_free",
                &[
                    "hew_json_array_get",
                    "hew_json_array_new",
                    "hew_json_from_bool",
                    "hew_json_from_float",
                    "hew_json_from_int",
                    "hew_json_from_null",
                    "hew_json_from_string",
                    "hew_json_get_field",
                    "hew_json_object_keys",
                    "hew_json_object_new",
                    "hew_json_parse",
                ][..],
            ),
            (
                "toml",
                "hew_toml_free",
                &[
                    "hew_toml_array_get",
                    "hew_toml_array_new",
                    "hew_toml_from_bool",
                    "hew_toml_from_float",
                    "hew_toml_from_int",
                    "hew_toml_from_string",
                    "hew_toml_get_field",
                    "hew_toml_table_new",
                    "hew_toml_parse",
                ][..],
            ),
            (
                "yaml",
                "hew_yaml_free",
                &[
                    "hew_yaml_array_get",
                    "hew_yaml_array_new",
                    "hew_yaml_from_bool",
                    "hew_yaml_from_float",
                    "hew_yaml_from_int",
                    "hew_yaml_from_null",
                    "hew_yaml_from_string",
                    "hew_yaml_get_field",
                    "hew_yaml_object_new",
                    "hew_yaml_parse",
                ][..],
            ),
        ] {
            for symbol in symbols {
                let contract = extern_ownership_contract(symbol)
                    .contract()
                    .unwrap_or_else(|| panic!("{prefix} producer {symbol} is unclassified"));
                assert_eq!(contract.result, ExternResultOwnership::Fresh, "{symbol}");
                assert_eq!(contract.release_symbol, release, "{symbol}");
                assert_eq!(
                    contract.discharge_depth,
                    ReleaseDischargeDepth::Deep,
                    "{symbol}"
                );
                assert_eq!(
                    contract.result_retention,
                    ExternResultRetention::ResourceTransfer,
                    "{symbol}"
                );
            }
        }
    }
}
