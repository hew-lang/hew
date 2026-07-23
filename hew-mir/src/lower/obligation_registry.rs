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
//! - entries key on the SCOPED tuple `(module, function, local, local_ty)` —
//!   never a bare `(function, local)` wildcard. The source `module` scopes a
//!   stdlib entry to its originating module (un-forgeable by user code) and
//!   the `local_ty` narrows every entry to its minting site's value shape;
//! - `registry_matches_pinned_key_set` pins the EXACT current population by
//!   scoped key, so a removed-and-replaced entry fails the build (not just a
//!   grown one) until the change is a deliberate table + pin edit.
//!
//! SCOPING LIMIT (fail-open residual, tracked as OWN-V1 follow-up):
//! a root-unit entry (`module: None`) originates from the root compilation
//! unit, which carries no dotted module path at this MIR gate — the HIR module
//! reaching `hew-mir` attributes only imported (`std.*`) items, not the root
//! file. Such an entry is therefore keyed only on `(function, local,
//! local_ty)`, which a user program COULD still alias by defining a
//! same-named function with a same-named local of the same type at a genuine
//! leak site. That is a far narrower target than the former bare-name wildcard
//! (which matched on name alone), but it is not airtight. Full closure needs a
//! per-compilation-unit source-file discriminator threaded from the frontend
//! into the MIR gate; until then a leak in user code that reproduces one of
//! these exact triples is suppressed. The stdlib (`module: Some`) entries are
//! not exposed to this residual — their module is un-forgeable.

/// One tracked pre-existing under-release hole.
pub(super) struct UnderReleaseAllowEntry {
    /// The dotted source module this function originates from (e.g.
    /// `std.base64`), or `None` for a root-compilation-unit function. Part of
    /// the scoping key: an entry only matches a finding whose function was
    /// lowered from the SAME source module. A stdlib module path is
    /// un-forgeable by user code (the compiler never attributes a user body to
    /// `std.*`), so a stdlib tracked-leak entry can never suppress a same-named
    /// leak in a user program. Root-unit entries (`None`) remain
    /// per-compilation-unit aliasable — see the module-level SCOPING LIMIT.
    pub module: Option<&'static str>,
    /// Exact `ElaboratedMirFunction::name` symbol the finding reports.
    pub function: &'static str,
    /// Exact source-level local name the finding reports.
    pub local: &'static str,
    /// Rendered type of the leaked local (`ResolvedTy` Display) at the minting
    /// site. Scoping discriminator: a same-named local of a different type
    /// cannot ride this entry.
    pub local_ty: &'static str,
    /// The GitHub issue tracking the fix.
    #[allow(
        dead_code,
        reason = "issue-link discipline field: read by the registry pin tests \
                  and by humans triaging the entry; the gate keys on \
                  (module, function, local, local_ty) only"
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
/// `semver::try_parse` (44 MB / 500k error-path calls) and `base64::decode`
/// (30 MB / 500k remaining==1 calls); the sibling `scanner` exhaustion sites
/// left this registry once #2784's carrier model discharged them. Lifts when
/// per-exit-path consume cancellation lands on the aggregation/return seam
/// (the PR #2784 round-4 discharge model extended past `pattern.rs`) and the
/// finding leaves the ratchet.
const BRANCH_AROUND_FAMILY: &str = "per-path consume cancellation (PR #2784 round-4 model) \
                                    covers the returned-aggregate / whole-return seam and \
                                    the finding leaves make test-hew-ratchet";

/// Tracking anchor for the family. The round-4 per-path discharge work is
/// the in-flight fix for the underlying path-insensitive cancellation.
const BRANCH_AROUND_ISSUE: &str = "https://github.com/hew-lang/hew/pull/2784";

/// The current registry population. Keep sorted by `function` then `local`.
pub(super) const UNDER_RELEASE_ALLOWLIST: &[UnderReleaseAllowEntry] = &[
    UnderReleaseAllowEntry {
        module: Some("std.encoding.base64"),
        function: "base64$decode",
        local: "out",
        local_ty: "bytes",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.channel"),
        function: "channel$try_new",
        local: "detail",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "check_ints",
        local: "reverse_out",
        local_ty: "Vec<i64>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "check_strings",
        local: "out",
        local_ty: "Vec<string>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "generic_iter_count$$IterOwnedItem",
        local: "_0",
        local_ty: "IterOwnedItem",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "distinct site within the owned-carrier snapshot seam: the \
                    generic monomorphization of an owned-element \
                    `Vec<T>::iter()` never releases its receiver snapshot \
                    clone (leaks --atExit: 600k allocations / 52.8 MB over \
                    300k calls). Lifts when the owned-carrier snapshot \
                    release lands and the finding leaves make test-hew-ratchet",
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "main",
        local: "__hew_call_scrutinee",
        local_ty: "Result<scanner.Scanner, fs.IoError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "vertical-slice `stdlib_io_scanner_file_oracle`: #2784 admits a \
                    fresh `scanner.from_file(..)` call-result Result temp as a \
                    unique owned carrier, but its `Ok(_) => ..` discard arm \
                    drops the extracted payload without a shell drop in that \
                    exit's plan, so the carrier is unreleased on the discard \
                    path (statically real, runtime-dead in the fixture: the \
                    probed path is guaranteed-missing so the Err arm is taken). \
                    BROAD KEY — any `main`-local named `__hew_call_scrutinee` \
                    rides this entry until the discard-arm shell drop lands; \
                    remove with the family",
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "main",
        local: "first",
        local_ty: "((Rec, i64), bool)",
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
        module: None,
        function: "main",
        local: "bag",
        local_ty: "Bag",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "vertical-slice `wire_json_vec_option_roundtrip`: `bag` is \
                    unreleased on four guard-return exits (leaks --atExit: 3 \
                    leaks / 176 bytes). Branch-around family; BROAD KEY — any \
                    `main`-local named `bag` rides this entry until the family \
                    fix lands; remove with the family",
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "parse",
        local: "__hew_call_scrutinee",
        local_ty: "Result<(i64, string), string>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: "vertical-slice `let_else_tuple_payload`: the let-else bail \
                    path returns while the call-scrutinee Result temp is live \
                    with no drop in that exit's plan (branch-around family; \
                    fixture-benign only because the Err payload is a static \
                    string). BROAD KEY — remove with the family",
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "scenario_identity_location",
        local: "inspected",
        local_ty: "Result<IdentityResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "scenario_remote_ask",
        local: "ask1",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "scenario_remote_ask",
        local: "ask2",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "scenario_repoint_preserves_ref",
        local: "new_value",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "scenario_repoint_preserves_ref",
        local: "old_value",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "scenario_repoint_preserves_ref",
        local: "seeded",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.text.semver"),
        function: "semver$try_parse",
        local: "build",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.text.semver"),
        function: "semver$try_parse",
        local: "pre",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.stream"),
        function: "stream$try_bytes_pipe",
        local: "detail",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.stream"),
        function: "stream$try_pipe",
        local: "detail",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.text.template"),
        function: "template$consume_range_body",
        local: "out",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: Some("std.text.template"),
        function: "template$render_segment",
        local: "out",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "test_capture_indexed_groups",
        local: "__hew_call_scrutinee",
        local_ty: "Option<string>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "test_concrete_enum_clone_owned_payload",
        local: "a",
        local_ty: "Shape",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "test_remove_i64_key",
        local: "v",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "test_remove_string_value_moves_out_exact",
        local: "v",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "test_remove_string_value_moves_out_exact",
        local: "w",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        module: None,
        function: "test_stream_try_to_file_send_close_and_try_from_file",
        local: "__hew_call_scrutinee",
        local_ty: "Result<Stream<string>, fs.IoError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
];

/// `true` iff the `(module, function, local, local_ty)` tuple carries a
/// registry entry — the ONLY path on which an `ObligationUnderReleased`
/// finding does not become a compile error. All four components must match:
/// the source module scopes stdlib entries against user-code aliasing, and
/// the local type narrows every entry to its minting site's value shape.
#[must_use]
pub(super) fn under_release_allowlisted(
    module: Option<&str>,
    function: &str,
    local: &str,
    local_ty: &str,
) -> bool {
    UNDER_RELEASE_ALLOWLIST.iter().any(|entry| {
        entry.module == module
            && entry.function == function
            && entry.local == local
            && entry.local_ty == local_ty
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The EXACT current population, pinned by scoped key
    /// `(module, function, local, local_ty)`. A size-only ceiling let an
    /// author delete a still-needed entry and swap in a placeholder without
    /// tripping the pin; this exact set fails BOTH ways — a removed entry
    /// leaves a pinned key with no live match, and an added or altered entry
    /// leaves a live key absent from the pin. Shrinking the registry (an entry
    /// whose leak is fixed) is therefore a deliberate two-place edit: delete
    /// the table entry AND its pin row in the same commit. Growing it needs a
    /// filed issue link on the new entry AND its pin row.
    ///
    /// Post-#2784 census: the branch-around consume-cancellation family
    /// (semver / base64 / stream / template / channel runtime-confirmed leak
    /// sites plus their structural siblings; see `BRANCH_AROUND_FAMILY`), the
    /// owned-element generic `Vec<T>::iter()` receiver-snapshot leak, and the
    /// root-unit fixture holes. #2784 fixed the scanner-exhaustion family and
    /// the hashmap-get discarded-result holes; those entries left the registry.
    const PINNED_KEYS: &[(Option<&str>, &str, &str, &str)] = &[
        (Some("std.encoding.base64"), "base64$decode", "out", "bytes"),
        (Some("std.channel"), "channel$try_new", "detail", "string"),
        (None, "check_ints", "reverse_out", "Vec<i64>"),
        (None, "check_strings", "out", "Vec<string>"),
        (
            None,
            "generic_iter_count$$IterOwnedItem",
            "_0",
            "IterOwnedItem",
        ),
        (
            None,
            "main",
            "__hew_call_scrutinee",
            "Result<scanner.Scanner, fs.IoError>",
        ),
        (None, "main", "first", "((Rec, i64), bool)"),
        (None, "main", "bag", "Bag"),
        (
            None,
            "parse",
            "__hew_call_scrutinee",
            "Result<(i64, string), string>",
        ),
        (
            None,
            "scenario_identity_location",
            "inspected",
            "Result<IdentityResp, AskError>",
        ),
        (
            None,
            "scenario_remote_ask",
            "ask1",
            "Result<KvResp, AskError>",
        ),
        (
            None,
            "scenario_remote_ask",
            "ask2",
            "Result<KvResp, AskError>",
        ),
        (
            None,
            "scenario_repoint_preserves_ref",
            "new_value",
            "Result<KvResp, AskError>",
        ),
        (
            None,
            "scenario_repoint_preserves_ref",
            "old_value",
            "Result<KvResp, AskError>",
        ),
        (
            None,
            "scenario_repoint_preserves_ref",
            "seeded",
            "Result<KvResp, AskError>",
        ),
        (
            Some("std.text.semver"),
            "semver$try_parse",
            "build",
            "string",
        ),
        (Some("std.text.semver"), "semver$try_parse", "pre", "string"),
        (
            Some("std.stream"),
            "stream$try_bytes_pipe",
            "detail",
            "string",
        ),
        (Some("std.stream"), "stream$try_pipe", "detail", "string"),
        (
            Some("std.text.template"),
            "template$consume_range_body",
            "out",
            "string",
        ),
        (
            Some("std.text.template"),
            "template$render_segment",
            "out",
            "string",
        ),
        (
            None,
            "test_capture_indexed_groups",
            "__hew_call_scrutinee",
            "Option<string>",
        ),
        (None, "test_concrete_enum_clone_owned_payload", "a", "Shape"),
        (None, "test_remove_i64_key", "v", "string"),
        (
            None,
            "test_remove_string_value_moves_out_exact",
            "v",
            "string",
        ),
        (
            None,
            "test_remove_string_value_moves_out_exact",
            "w",
            "string",
        ),
        (
            None,
            "test_stream_try_to_file_send_close_and_try_from_file",
            "__hew_call_scrutinee",
            "Result<Stream<string>, fs.IoError>",
        ),
    ];

    fn live_keys() -> Vec<(
        Option<&'static str>,
        &'static str,
        &'static str,
        &'static str,
    )> {
        UNDER_RELEASE_ALLOWLIST
            .iter()
            .map(|e| (e.module, e.function, e.local, e.local_ty))
            .collect()
    }

    /// The live table matches the pinned key set EXACTLY — same count, every
    /// live key pinned, every pinned key live. Removing a still-needed entry
    /// or swapping in a placeholder fails here.
    #[test]
    fn registry_matches_pinned_key_set() {
        let live = live_keys();
        assert_eq!(
            live.len(),
            PINNED_KEYS.len(),
            "registry population changed: {} live vs {} pinned; edit the table \
             AND the pin together (with an issue link for any addition)",
            live.len(),
            PINNED_KEYS.len()
        );
        for key in &live {
            assert!(
                PINNED_KEYS.contains(key),
                "unpinned registry entry {key:?}; a new or altered allowlist \
                 entry must be added to PINNED_KEYS with an issue link"
            );
        }
        for key in PINNED_KEYS {
            assert!(
                live.contains(key),
                "pinned key {key:?} has no live entry; a fixed leak must drop \
                 its pin row in the same commit as its table entry"
            );
        }
    }

    /// No two entries share a scoped key: a duplicate would be dead weight and
    /// a sign of a merge/rekey error.
    #[test]
    fn registry_keys_are_unique() {
        let mut live = live_keys();
        let n = live.len();
        live.sort_unstable();
        live.dedup();
        assert_eq!(n, live.len(), "duplicate scoped keys in the registry");
    }

    /// Every module-mangled stdlib symbol (a `$` in the function name) MUST
    /// carry a source module — that module is the un-forgeable scope that
    /// stops user code from aliasing a stdlib tracked-leak entry. An entry
    /// that forgets it would silently regress to the old bare-name wildcard.
    #[test]
    fn stdlib_mangled_entries_are_module_scoped() {
        for e in UNDER_RELEASE_ALLOWLIST {
            if e.function.contains('$') && !e.function.contains("$$") {
                assert!(
                    e.module.is_some(),
                    "stdlib-mangled entry `{}` has no module scope; without it \
                     the entry falls back to a user-forgeable wildcard",
                    e.function
                );
            }
        }
    }

    /// The scoped key defeats cross-compilation-unit aliasing that the former
    /// bare `(function, local)` match allowed.
    #[test]
    fn scoping_key_rejects_aliased_sites() {
        // The exact minting site is allowlisted.
        assert!(under_release_allowlisted(
            None,
            "main",
            "first",
            "((Rec, i64), bool)"
        ));
        // A user program with the same function + local name but a DIFFERENT
        // local type does NOT ride the entry (the old bare match would have).
        assert!(!under_release_allowlisted(None, "main", "first", "string"));
        // A stdlib entry cannot be forged from user (root-unit) code: the
        // un-forgeable module scope must match, so a root-unit `base64$decode`
        // is not suppressed.
        assert!(!under_release_allowlisted(
            None,
            "base64$decode",
            "out",
            "bytes"
        ));
        assert!(under_release_allowlisted(
            Some("std.encoding.base64"),
            "base64$decode",
            "out",
            "bytes"
        ));
        // Wrong module also fails closed.
        assert!(!under_release_allowlisted(
            Some("std.stream"),
            "base64$decode",
            "out",
            "bytes"
        ));
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
