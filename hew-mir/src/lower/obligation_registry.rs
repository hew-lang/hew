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

/// The lift trigger shared by the branch-around consume-cancellation family:
/// the elaborator removes a consumed binding's terminal drop GLOBALLY when
/// the consume (a returned aggregate / whole-value return / payload
/// move-out / call-carrier transfer) sits on only SOME paths, so every
/// guard-style early return leaks the mint. Runtime-confirmed on trunk for
/// `semver::try_parse` (44 MB / 500k error-path calls), `base64::decode`
/// (30 MB / 500k remaining==1 calls), and `scanner::lines` (47 MB / 300k
/// calls). Lifts when per-exit-path consume cancellation lands on the
/// aggregation/return seam (the PR #2784 round-4 discharge model extended
/// past `pattern.rs`) and the finding leaves the ratchet.
const BRANCH_AROUND_FAMILY: &str = "per-path consume cancellation (PR #2784 round-4 model) \
                                    covers the returned-aggregate / whole-return seam and \
                                    the finding leaves make test-hew-ratchet";

/// Tracking anchor for the family. The round-4 per-path discharge work is
/// the in-flight fix for the underlying path-insensitive cancellation.
const BRANCH_AROUND_ISSUE: &str = "https://github.com/hew-lang/hew/pull/2784";

/// The current registry population. Keep sorted by `function` then `local`.
pub(super) const UNDER_RELEASE_ALLOWLIST: &[UnderReleaseAllowEntry] = &[
    UnderReleaseAllowEntry {
        function: "base64$decode",
        local: "out",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "channel$try_new",
        local: "detail",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "check_ints",
        local: "reverse_out",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "check_strings",
        local: "out",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "generic_iter_count$$IterOwnedItem",
        local: "_0",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "distinct site within the owned-carrier snapshot seam: the \
                    generic monomorphization of an owned-element \
                    `Vec<T>::iter()` never releases its receiver snapshot \
                    clone (leaks --atExit: 600k allocations / 52.8 MB over \
                    300k calls). Lifts when the owned-carrier snapshot \
                    release lands and the finding leaves make test-hew-ratchet",
    },
    UnderReleaseAllowEntry {
        function: "main",
        local: "first",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "vertical-slice `owned_nested_tuple_record`: the second \
                    match's `None => return` guard exits while the fresh \
                    deep-cloned `first` owner is live with no drop in that \
                    exit's plan (the branch-around class; statically real, \
                    unexercised at runtime in the fixture). BROAD KEY — any \
                    `main`-local named `first` rides this entry until the \
                    family fix lands; remove with the family",
    },
    UnderReleaseAllowEntry {
        function: "main",
        local: "bag",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "vertical-slice `wire_json_vec_option_roundtrip`: `bag` is \
                    unreleased on four guard-return exits (leaks --atExit: 3 \
                    leaks / 176 bytes). Branch-around family; BROAD KEY — any \
                    `main`-local named `bag` rides this entry until the family \
                    fix lands; remove with the family",
    },
    UnderReleaseAllowEntry {
        function: "parse",
        local: "__hew_call_scrutinee",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "vertical-slice `let_else_tuple_payload`: the let-else bail \
                    path returns while the call-scrutinee Result temp is live \
                    with no drop in that exit's plan (branch-around family; \
                    fixture-benign only because the Err payload is a static \
                    string). BROAD KEY — remove with the family",
    },
    UnderReleaseAllowEntry {
        function: "scanner$collect",
        local: "cur",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scanner$lines",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scanner$words",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_identity_location",
        local: "inspected",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_remote_ask",
        local: "ask1",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_remote_ask",
        local: "ask2",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_repoint_preserves_ref",
        local: "new_value",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_repoint_preserves_ref",
        local: "old_value",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_repoint_preserves_ref",
        local: "seeded",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "semver$try_parse",
        local: "build",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "semver$try_parse",
        local: "pre",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "stream$try_bytes_pipe",
        local: "detail",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "stream$try_pipe",
        local: "detail",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "template$consume_range_body",
        local: "out",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "template$render_segment",
        local: "out",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_capture_indexed_groups",
        local: "__hew_call_scrutinee",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_concrete_enum_clone_owned_payload",
        local: "a",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_hashmap_get_owned_record_value_survives_remove",
        local: "__hew_discarded_call_result",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_hashmap_get_string_value_survives_remove",
        local: "__hew_discarded_call_result",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_remove_i64_key",
        local: "v",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_remove_string_value_moves_out_exact",
        local: "v",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_remove_string_value_moves_out_exact",
        local: "w",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_scan_blank_lines_emitted",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_scan_crlf_stripped",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_scan_empty_input_no_tokens",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_scan_iterate_three_lines",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_scan_single_line_without_newline",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_scan_trailing_newline_no_extra_token",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_stream_try_to_file_send_close_and_try_from_file",
        local: "__hew_call_scrutinee",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_with_split_resets_position",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_with_split_switches_to_words_mode",
        local: "sc",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
];

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
        /// Current population: the 27 branch-around consume-cancellation
        /// family entries (three runtime-confirmed leak sites plus their
        /// structural siblings; see `BRANCH_AROUND_FAMILY`) plus the
        /// owned-element generic `Vec<T>::iter()` receiver-snapshot leak
        /// (leaks --atExit confirmed).
        const REGISTRY_PIN: usize = 39;
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
