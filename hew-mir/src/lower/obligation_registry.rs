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
//! - entries key on the SCOPED tuple `(function, local, local_ty)` — never a
//!   bare `(function, local)` wildcard. The `local_ty` narrows every entry to
//!   its minting site's value shape;
//! - `registry_matches_pinned_key_set` pins the EXACT current population by
//!   scoped key, so a removed-and-replaced entry fails the build (not just a
//!   grown one) until the change is a deliberate table + pin edit.
//!
//! STDLIB SCOPING (un-forgeable): a stdlib entry's `function` is a
//! module-mangled symbol carrying a `$` separator (`channel$try_new`,
//! `base64$decode`, `semver$try_parse`, …). Hew identifiers are
//! `[a-zA-Z_][a-zA-Z0-9_]*` (see `hew-lexer`), so `$` can NEVER appear in a
//! user-authored function symbol — the `$`-mangled name is itself the
//! un-forgeable module scope, and no user program can produce a finding whose
//! `function` matches a stdlib entry. (A dotted source-module discriminator
//! was rejected: `HirModule::diagnostic_source_modules` attributes the same
//! stdlib function inconsistently across import paths — e.g. `std.channel`
//! vs `std.channel.channel` — so it is not a stable key. The mangling prefix
//! encodes the module and is stable.)
//!
//! SCOPING LIMIT (fail-open residual, tracked as OWN-V1 follow-up):
//! a root-unit entry (a bare, un-mangled `function` such as `main` / `parse` /
//! `check_ints`) originates from the root compilation unit, which carries no
//! stable per-file discriminator at this MIR gate. Such an entry is keyed only
//! on `(function, local, local_ty)`, which a user program COULD still alias by
//! defining a same-named function with a same-named local of the same type at
//! a genuine leak site. That is a far narrower target than the former
//! bare-name wildcard (which matched on name alone), but it is not airtight.
//! Full closure needs a per-compilation-unit source-file discriminator
//! threaded from the frontend into the MIR gate; until then a leak in user
//! code that reproduces one of these exact triples is suppressed. The stdlib
//! (`$`-mangled) entries are not exposed to this residual.

/// One tracked pre-existing under-release hole.
pub(super) struct UnderReleaseAllowEntry {
    /// Exact `ElaboratedMirFunction::name` symbol the finding reports. For a
    /// stdlib entry this is a `$`-mangled symbol (un-forgeable by user code —
    /// `$` is not a legal identifier character), which is the entry's module
    /// scope. A bare (un-mangled) symbol is a root-unit entry (see the
    /// module-level SCOPING LIMIT).
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
                  (function, local, local_ty) only"
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
        function: "base64$decode",
        local: "out",
        local_ty: "bytes",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "channel$try_new",
        local: "detail",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "check_ints",
        local: "reverse_out",
        local_ty: "Vec<i64>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "check_strings",
        local: "out",
        local_ty: "Vec<string>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
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
        function: "scenario_identity_location",
        local: "inspected",
        local_ty: "Result<IdentityResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_remote_ask",
        local: "ask1",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_remote_ask",
        local: "ask2",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_repoint_preserves_ref",
        local: "new_value",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_repoint_preserves_ref",
        local: "old_value",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "scenario_repoint_preserves_ref",
        local: "seeded",
        local_ty: "Result<KvResp, AskError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "semver$try_parse",
        local: "build",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "semver$try_parse",
        local: "pre",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "stream$try_bytes_pipe",
        local: "detail",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "stream$try_pipe",
        local: "detail",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "template$consume_range_body",
        local: "out",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "template$render_segment",
        local: "out",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_capture_indexed_groups",
        local: "__hew_call_scrutinee",
        local_ty: "Option<string>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_concrete_enum_clone_owned_payload",
        local: "a",
        local_ty: "Shape",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_remove_i64_key",
        local: "v",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_remove_string_value_moves_out_exact",
        local: "v",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_remove_string_value_moves_out_exact",
        local: "w",
        local_ty: "string",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
    UnderReleaseAllowEntry {
        function: "test_stream_try_to_file_send_close_and_try_from_file",
        local: "__hew_call_scrutinee",
        local_ty: "Result<Stream<string>, fs.IoError>",
        issue: BRANCH_AROUND_ISSUE,
        lift_when: BRANCH_AROUND_FAMILY,
    },
];

/// `true` iff the `(function, local, local_ty)` tuple carries a registry
/// entry — the ONLY path on which an `ObligationUnderReleased` finding does
/// not become a compile error. All three components must match: the
/// `$`-mangled stdlib symbol is un-forgeable (its module scope), and the
/// local type narrows every entry to its minting site's value shape.
#[must_use]
pub(super) fn under_release_allowlisted(function: &str, local: &str, local_ty: &str) -> bool {
    UNDER_RELEASE_ALLOWLIST.iter().any(|entry| {
        entry.function == function && entry.local == local && entry.local_ty == local_ty
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The EXACT current population, pinned by scoped key
    /// `(function, local, local_ty)`. A size-only ceiling let an author delete
    /// a still-needed entry and swap in a placeholder without tripping the pin;
    /// this exact set fails BOTH ways — a removed entry leaves a pinned key
    /// with no live match, and an added or altered entry leaves a live key
    /// absent from the pin. Shrinking the registry (an entry whose leak is
    /// fixed) is therefore a deliberate two-place edit: delete the table entry
    /// AND its pin row in the same commit. Growing it needs a filed issue link
    /// on the new entry AND its pin row.
    ///
    /// Post-#2784 census: the branch-around consume-cancellation family
    /// (semver / base64 / stream / template / channel runtime-confirmed leak
    /// sites plus their structural siblings; see `BRANCH_AROUND_FAMILY`), the
    /// owned-element generic `Vec<T>::iter()` receiver-snapshot leak, and the
    /// root-unit fixture holes. #2784 fixed the scanner-exhaustion family and
    /// the hashmap-get discarded-result holes; those entries left the registry.
    const PINNED_KEYS: &[(&str, &str, &str)] = &[
        ("base64$decode", "out", "bytes"),
        ("channel$try_new", "detail", "string"),
        ("check_ints", "reverse_out", "Vec<i64>"),
        ("check_strings", "out", "Vec<string>"),
        ("generic_iter_count$$IterOwnedItem", "_0", "IterOwnedItem"),
        (
            "main",
            "__hew_call_scrutinee",
            "Result<scanner.Scanner, fs.IoError>",
        ),
        ("main", "first", "((Rec, i64), bool)"),
        ("main", "bag", "Bag"),
        (
            "parse",
            "__hew_call_scrutinee",
            "Result<(i64, string), string>",
        ),
        (
            "scenario_identity_location",
            "inspected",
            "Result<IdentityResp, AskError>",
        ),
        ("scenario_remote_ask", "ask1", "Result<KvResp, AskError>"),
        ("scenario_remote_ask", "ask2", "Result<KvResp, AskError>"),
        (
            "scenario_repoint_preserves_ref",
            "new_value",
            "Result<KvResp, AskError>",
        ),
        (
            "scenario_repoint_preserves_ref",
            "old_value",
            "Result<KvResp, AskError>",
        ),
        (
            "scenario_repoint_preserves_ref",
            "seeded",
            "Result<KvResp, AskError>",
        ),
        ("semver$try_parse", "build", "string"),
        ("semver$try_parse", "pre", "string"),
        ("stream$try_bytes_pipe", "detail", "string"),
        ("stream$try_pipe", "detail", "string"),
        ("template$consume_range_body", "out", "string"),
        ("template$render_segment", "out", "string"),
        (
            "test_capture_indexed_groups",
            "__hew_call_scrutinee",
            "Option<string>",
        ),
        ("test_concrete_enum_clone_owned_payload", "a", "Shape"),
        ("test_remove_i64_key", "v", "string"),
        ("test_remove_string_value_moves_out_exact", "v", "string"),
        ("test_remove_string_value_moves_out_exact", "w", "string"),
        (
            "test_stream_try_to_file_send_close_and_try_from_file",
            "__hew_call_scrutinee",
            "Result<Stream<string>, fs.IoError>",
        ),
    ];

    fn live_keys() -> Vec<(&'static str, &'static str, &'static str)> {
        UNDER_RELEASE_ALLOWLIST
            .iter()
            .map(|e| (e.function, e.local, e.local_ty))
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

    /// The scoped key defeats cross-compilation-unit aliasing that the former
    /// bare `(function, local)` match allowed.
    #[test]
    fn scoping_key_rejects_aliased_sites() {
        // The exact minting site is allowlisted.
        assert!(under_release_allowlisted(
            "main",
            "first",
            "((Rec, i64), bool)"
        ));
        // A user program with the same function + local name but a DIFFERENT
        // local type does NOT ride the entry (the old bare match would have).
        assert!(!under_release_allowlisted("main", "first", "string"));
        // The stdlib entry matches at its own `$`-mangled symbol...
        assert!(under_release_allowlisted("base64$decode", "out", "bytes"));
        // ...and a same-named local of another type does not ride it.
        assert!(!under_release_allowlisted("base64$decode", "out", "string"));
    }

    /// Every stdlib entry keys on a `$`-mangled symbol. `$` is not a legal
    /// identifier character in Hew source (`hew-lexer` identifiers are
    /// `[a-zA-Z_][a-zA-Z0-9_]*`), so no user program can author a function that
    /// emits this symbol — the mangled name is the entry's un-forgeable scope.
    /// This test documents which entries carry that guarantee vs. the
    /// root-unit residual (bare names — see the module-level SCOPING LIMIT).
    #[test]
    fn stdlib_entries_are_dollar_mangled() {
        let mangled: Vec<&str> = UNDER_RELEASE_ALLOWLIST
            .iter()
            .filter(|e| e.function.contains('$'))
            .map(|e| e.function)
            .collect();
        // The known stdlib symbols are all `$`-scoped; the count is pinned so a
        // future stdlib entry that forgets its mangling (a bare name) is
        // noticed. `generic_iter_count$$IterOwnedItem` is a `$$` monomorphised
        // symbol and also un-forgeable.
        assert_eq!(
            mangled.len(),
            9,
            "expected 9 `$`-mangled (un-forgeable) entries, got {mangled:?}"
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
