//! Shrink-only, issue-linked under-release allowlist for the S1
//! obligation-balance validator (`validate_obligation_balance`).
//!
//! Every entry names a KNOWN pre-existing leak (mint without discharge on
//! some exit path) that predates the validator, carries the GitHub issue
//! tracking its fix, and states the trigger that lifts the entry. The gate
//! in `lower::mod` consults this table for `ObligationUnderReleased`
//! findings only — over-release (double-free) findings are memory-unsafe
//! and NEVER route through this registry.
//!
//! REGISTRY DISCIPLINE (D139 / S1128 ratification):
//! - the table only ever SHRINKS: an entry is removed when its leak is
//!   fixed; adding an entry requires a filed issue link in the same commit;
//! - entries key on the exact function symbol + the source-level local
//!   name the finding reports — never a wildcard;
//! - `pinned_by_registry_is_shrink_only` pins the current population so a
//!   grown table fails the build until the addition is deliberate (update
//!   the pin + the issue link together).

/// One tracked pre-existing under-release hole.
pub(super) struct UnderReleaseAllowEntry {
    /// Exact `ElaboratedMirFunction::name` symbol the finding reports.
    pub function: &'static str,
    /// Exact source-level local name the finding reports.
    pub local: &'static str,
    /// The GitHub issue tracking the fix.
    #[allow(
        dead_code,
        reason = "issue-link discipline field: read by the registry pin tests \
                  and by humans triaging the entry; the gate keys on \
                  (function, local) only"
    )]
    pub issue: &'static str,
    /// The condition under which this entry is removed.
    #[allow(
        dead_code,
        reason = "lift-trigger discipline field: read by the registry pin \
                  tests and by humans triaging the entry"
    )]
    pub lift_when: &'static str,
}

/// The current registry population. Keep sorted by `function` then `local`.
pub(super) const UNDER_RELEASE_ALLOWLIST: &[UnderReleaseAllowEntry] = &[];

/// `true` iff the (function, local) pair carries a registry entry — the
/// ONLY path on which an `ObligationUnderReleased` finding does not become
/// a compile error.
#[must_use]
pub(super) fn under_release_allowlisted(function: &str, local: &str) -> bool {
    UNDER_RELEASE_ALLOWLIST
        .iter()
        .any(|entry| entry.function == function && entry.local == local)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The registry only ever shrinks. This pin fails when the population
    /// grows; raising it must accompany a filed issue link on the new entry
    /// (see the module docs).
    #[test]
    fn registry_is_shrink_only() {
        /// The pinned ceiling. Lower it when an entry is fixed; raising it
        /// requires a filed issue on the new entry in the same commit.
        const REGISTRY_PIN: usize = 0;
        #[allow(
            clippy::absurd_extreme_comparisons,
            reason = "the pin is deliberately comparable at zero: the empty \
                      registry is the goal state and the comparison text is \
                      the shrink-only contract"
        )]
        let within_pin = UNDER_RELEASE_ALLOWLIST.len() <= REGISTRY_PIN;
        assert!(
            within_pin,
            "the under-release registry grew ({} entries); every addition \
             needs a filed issue + an explicit pin raise in the same commit",
            UNDER_RELEASE_ALLOWLIST.len()
        );
    }

    /// Every entry must carry a real issue link and a lift trigger.
    #[test]
    fn registry_entries_are_issue_linked() {
        for entry in UNDER_RELEASE_ALLOWLIST {
            assert!(
                entry.issue.contains('#') || entry.issue.contains("github.com"),
                "registry entry ({}, {}) has no issue link",
                entry.function,
                entry.local
            );
            assert!(
                !entry.lift_when.is_empty(),
                "registry entry ({}, {}) has no lift trigger",
                entry.function,
                entry.local
            );
        }
    }
}
