//! In-compiler carriage of the machine-checked FFI ownership contracts.
//!
//! `scripts/jit-symbol-classification.toml` `[[ownership.contracts]]` rows are
//! the single authority for per-parameter consume/borrow facts, result
//! freshness, and the release axis of `hew_*` extern symbols (schema from
//! PR #2561, validated out-of-band by `scripts/verify-ffi-symbols.py`).
//! `hew-mir/build.rs` projects that table into a static, sorted slice at build
//! time (`FFI_OWNERSHIP_CONTRACTS`, included below), so the facts are readable
//! at the MIR lowering positions that consult callee contracts — the same seam
//! where `callee_ownership_contract` serves the emitter-symbol subset.
//!
//! CARRIAGE ONLY: nothing here is enforcement. The S1 obligation validator and
//! OWN-V1 consume these facts; until then extern calls keep today's
//! conservative behaviour (caller retains the drop obligation). An extern
//! symbol with no contract yields the explicit [`ExternOwnershipFact::Absent`]
//! verdict — never a fabricated default contract.

/// Ownership fact for one C-ABI parameter position.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternParamOwnership {
    /// The callee reads (or copies from) the parameter; the caller keeps the
    /// drop obligation.
    Borrow,
    /// The callee takes ownership; the caller's drop obligation is discharged
    /// by the call.
    Consume,
    /// The callee retains an additional reference; the caller keeps its own
    /// obligation and the callee owes an independent release.
    Retain,
}

/// Ownership fact for a callee result.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternResultOwnership {
    /// A newly owned allocation transfers to the caller.
    Fresh,
    /// An additional reference to an existing ref-counted allocation
    /// transfers to the caller.
    Retained,
    /// The result remains owned elsewhere and may be invalidated by mutation.
    Borrowed,
    /// The call hands back no owned result.
    None,
}

/// Whether the contract's release symbol recursively discharges children.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReleaseDischargeDepth {
    /// The release discharges only the top-level allocation.
    Shallow,
    /// The release recursively discharges owned children.
    Deep,
    /// No owned result, so no release axis.
    None,
}

/// One extern symbol's machine-checked ownership contract.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExternOwnershipContract {
    /// Per-parameter ownership, positionally matching the C signature.
    pub params: &'static [ExternParamOwnership],
    /// Result ownership.
    pub result: ExternResultOwnership,
    /// The `hew_*` release entry (or type-directed drop-thunk spelling) that
    /// balances an owned result; empty for borrowed/none results.
    pub release_symbol: &'static str,
    /// Recursion depth of the release symbol's discharge.
    pub discharge_depth: ReleaseDischargeDepth,
}

/// The fact available at a lowering position for an extern callee symbol.
///
/// `Absent` is an explicit verdict, not a default: the consumer must keep the
/// conservative behaviour (caller retains the drop obligation) and must never
/// synthesize a contract for an unclassified symbol.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExternOwnershipFact {
    /// The symbol carries a machine-checked contract.
    Contract(&'static ExternOwnershipContract),
    /// The symbol has no contract; fail closed (caller retains drop).
    Absent,
}

impl ExternOwnershipFact {
    /// True iff a contract is present.
    #[must_use]
    pub const fn is_contract(self) -> bool {
        matches!(self, Self::Contract(_))
    }

    /// The contract, when present.
    #[must_use]
    pub const fn contract(self) -> Option<&'static ExternOwnershipContract> {
        match self {
            Self::Contract(contract) => Some(contract),
            Self::Absent => None,
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/ffi_ownership_contracts.rs"));

/// Look up the ownership fact for an extern callee symbol.
#[must_use]
pub fn extern_ownership_contract(symbol: &str) -> ExternOwnershipFact {
    match FFI_OWNERSHIP_CONTRACTS.binary_search_by(|(row, _)| (*row).cmp(symbol)) {
        Ok(index) => ExternOwnershipFact::Contract(&FFI_OWNERSHIP_CONTRACTS[index].1),
        Err(_) => ExternOwnershipFact::Absent,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_is_sorted_and_unique() {
        // The binary-search invariant: strictly ascending symbol order.
        assert!(
            FFI_OWNERSHIP_CONTRACTS.windows(2).all(|w| w[0].0 < w[1].0),
            "generated contract table must be sorted and duplicate-free"
        );
    }

    #[test]
    fn unclassified_symbol_is_explicit_absent() {
        assert_eq!(
            extern_ownership_contract("hew_nope"),
            ExternOwnershipFact::Absent
        );
        assert_eq!(extern_ownership_contract(""), ExternOwnershipFact::Absent);
    }

    #[test]
    fn owned_results_carry_release_axis() {
        // Mirror of the verify-ffi-symbols.py coupling rule: an owned result
        // names its release; a borrowed/none result carries no release axis.
        for (symbol, contract) in FFI_OWNERSHIP_CONTRACTS {
            match contract.result {
                ExternResultOwnership::Fresh | ExternResultOwnership::Retained => {
                    assert!(
                        !contract.release_symbol.is_empty()
                            && contract.discharge_depth != ReleaseDischargeDepth::None,
                        "{symbol}: owned result requires a release axis"
                    );
                }
                ExternResultOwnership::Borrowed | ExternResultOwnership::None => {
                    assert!(
                        contract.release_symbol.is_empty()
                            && contract.discharge_depth == ReleaseDischargeDepth::None,
                        "{symbol}: unowned result must carry no release axis"
                    );
                }
            }
        }
    }

    #[test]
    fn known_contracts_readable() {
        // `hew_json_free` consumes its single value parameter
        // (hew-std/src/json.rs reconstructs and drops `val`).
        let json_free = extern_ownership_contract("hew_json_free")
            .contract()
            .expect("hew_json_free is classified");
        assert_eq!(json_free.params, [ExternParamOwnership::Consume]);
        assert_eq!(json_free.result, ExternResultOwnership::None);

        // `hew_bytes_to_string` borrows its source and returns a fresh owned
        // string balanced by exactly one `hew_string_drop`.
        let bytes_to_string = extern_ownership_contract("hew_bytes_to_string")
            .contract()
            .expect("hew_bytes_to_string is classified");
        assert_eq!(bytes_to_string.result, ExternResultOwnership::Fresh);
        assert_eq!(bytes_to_string.release_symbol, "hew_string_drop");
        assert_eq!(
            bytes_to_string.discharge_depth,
            ReleaseDischargeDepth::Shallow
        );

        // `hew_vec_get_owned` returns an alias invalidated by vec mutation.
        let get_owned = extern_ownership_contract("hew_vec_get_owned")
            .contract()
            .expect("hew_vec_get_owned is classified");
        assert_eq!(get_owned.result, ExternResultOwnership::Borrowed);
    }
}
