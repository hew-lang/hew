//! Allowlist parity tests for the [`hew_types::runtime_call`] substrate.
//!
//! These tests live in `hew-mir` (not `hew-types`) because they require
//! `hew_mir::runtime_symbols::{is_known_runtime_symbol, known_runtime_symbols}`
//! — the MIR-side emitter allowlist for `Instr::CallRuntimeAbi`. The
//! substrate itself lives in `hew-types` so the type checker can
//! construct typed descriptors; this side guards the parity between
//! the typed catalog and the runtime allowlist.

use std::collections::HashSet;

use hew_mir::runtime_symbols::{is_known_runtime_symbol, known_runtime_symbols};
use hew_types::runtime_call::{
    all_runtime_call_families, all_runtime_drop_descriptors, is_pre_staged_family,
};

/// Bijection (inverse): every symbol in `known_runtime_symbols`
/// is produced by exactly one family variant. A symbol added to the
/// allowlist without a matching descriptor variant fails this test,
/// forcing every future runtime-ABI addition to update both the
/// allowlist and the typed catalog at the same time.
#[test]
fn every_allowlist_symbol_has_a_family() {
    let families = all_runtime_call_families();
    let produced: HashSet<&'static str> = families.iter().map(|f| f.c_symbol()).collect();
    let mut missing = Vec::new();
    for sym in known_runtime_symbols() {
        if !produced.contains(sym) {
            missing.push(sym);
        }
    }
    assert!(
        missing.is_empty(),
        "known_runtime_symbols entries with no RuntimeCallFamily \
         variant — every allowlist symbol must have a typed descriptor \
         so consumers can dispatch on the family rather than the string. \
         Missing: {missing:?}"
    );
}

/// Coverage (forward): every family that is NOT pre-staged produces a
/// symbol that is in `known_runtime_symbols`. The emitter partition is
/// derived from the typed family, so this pins the public MIR admission
/// API to that authority.
#[test]
fn allowlist_subset_round_trips() {
    let mut violations = Vec::new();
    for family in all_runtime_call_families() {
        if is_pre_staged_family(family) {
            continue;
        }
        let sym = family.c_symbol();
        if !is_known_runtime_symbol(sym) {
            violations.push((family, sym));
        }
    }
    assert!(
        violations.is_empty(),
        "Family→c_symbol values not in known_runtime_symbols — \
         classify the family as an MIR emitter or a pre-staged call. Offenders: \
         {violations:?}"
    );
}

/// Routing-partition disjointness: a pre-staged family's symbol must be
/// ABSENT from `known_runtime_symbols`. The two routes are
/// exclusive — pre-staged symbols ride `Terminator::Call` into the
/// codegen callee intercepts; emitter symbols ride
/// `Instr::CallRuntimeAbi`. A pre-staged symbol mistakenly added to the
/// emitter list would silently reroute its calls onto the runtime-ABI
/// path (where the family has no lowering arm) with no other test
/// firing.
#[test]
fn pre_staged_families_are_disjoint_from_the_emitter_allowlist() {
    let mut violations = Vec::new();
    for family in all_runtime_call_families() {
        if is_pre_staged_family(family) && is_known_runtime_symbol(family.c_symbol()) {
            violations.push((family, family.c_symbol()));
        }
    }
    assert!(
        violations.is_empty(),
        "pre-staged families whose symbol is ALSO in \
         known_runtime_symbols — the Terminator::Call and \
         CallRuntimeAbi routes must stay disjoint; either un-pre-stage \
         the family (wire its CallRuntimeAbi arm) or remove the symbol \
         from the allowlist. Offenders: {violations:?}"
    );
}

/// Allowlist coverage: every drop-descriptor's `c_symbol()` is in
/// `known_runtime_symbols`. (`hew_stream_close` /
/// `hew_sink_close` are not in the allowlist today — the bijection
/// excludes them with the same `is_pre_staged_*` rationale as the
/// call-family side.)
#[test]
fn drop_descriptor_symbols_in_allowlist_or_pre_staged() {
    let pre_staged: HashSet<&'static str> = [
        "hew_stream_close",
        "hew_sink_close",
        "hew_channel_sender_close",
        "hew_channel_receiver_close",
    ]
    .into_iter()
    .collect();
    for d in all_runtime_drop_descriptors() {
        let sym = d.c_symbol();
        if pre_staged.contains(sym) {
            continue;
        }
        assert!(
            is_known_runtime_symbol(sym),
            "drop descriptor {d:?} → {sym} not in known_runtime_symbols \
             and not in the pre-staged set"
        );
    }
}
