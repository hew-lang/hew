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

/// Bijection (inverse): every symbol in `MIR_EMITTER_RUNTIME_SYMBOLS`
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
            missing.push(*sym);
        }
    }
    assert!(
        missing.is_empty(),
        "MIR_EMITTER_RUNTIME_SYMBOLS entries with no RuntimeCallFamily \
         variant — every allowlist symbol must have a typed descriptor \
         so consumers can dispatch on the family rather than the string. \
         Missing: {missing:?}"
    );
}

/// Coverage (forward): every family that is NOT pre-staged produces a
/// symbol that is in `MIR_EMITTER_RUNTIME_SYMBOLS`. A new variant added
/// without the matching allowlist entry fails this test (unless the
/// contributor also adds it to `is_pre_staged_family`, which the
/// reviewer would catch).
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
        "Family→c_symbol values not in MIR_EMITTER_RUNTIME_SYMBOLS — \
         either add the symbol to the allowlist (preferred) or mark the \
         family pre-staged in `is_pre_staged_family`. Offenders: \
         {violations:?}"
    );
}

/// Allowlist coverage: every drop-descriptor's `c_symbol()` is in
/// `MIR_EMITTER_RUNTIME_SYMBOLS`. (`hew_stream_close` /
/// `hew_sink_close` are not in the allowlist today — the bijection
/// excludes them with the same `is_pre_staged_*` rationale as the
/// call-family side.)
#[test]
fn drop_descriptor_symbols_in_allowlist_or_pre_staged() {
    let pre_staged: HashSet<&'static str> =
        ["hew_stream_close", "hew_sink_close"].into_iter().collect();
    for d in all_runtime_drop_descriptors() {
        let sym = d.c_symbol();
        if pre_staged.contains(sym) {
            continue;
        }
        assert!(
            is_known_runtime_symbol(sym),
            "drop descriptor {d:?} → {sym} not in MIR_EMITTER_RUNTIME_SYMBOLS \
             and not in the pre-staged set"
        );
    }
}

/// Symbol-existence parity: every family's `c_symbol()` resolves to a
/// real, verifiable identifier — either an entry in the runtime
/// allowlist (`MIR_EMITTER_RUNTIME_SYMBOLS`), a codegen-intercept name
/// (callee-name dispatch in `hew-codegen-rs/src/llvm.rs`), a math
/// intrinsic user name, or a Stream/Sink `ElementOverload` extension
/// symbol from `hew-types/src/builtin_names.rs`. NO family is allowed
/// to fabricate a string that exists nowhere else — that would let a
/// typed catalog ship symbols the runtime cannot resolve.
#[test]
fn every_c_symbol_resolves_to_a_real_symbol() {
    // Callee-name dispatch intercepts in codegen — real identities used
    // by `Terminator::Call` matching in `hew-codegen-rs/src/llvm.rs`
    // (Channel/Stream/Sink pre-staged, RemotePidTell
    // `CalleeNameDispatchOnly` linkage in `hew-hir/src/stdlib_catalog.rs`,
    // `Node::lookup`, TCP attach).
    let codegen_intercepts: HashSet<&'static str> = [
        "Node::lookup",
        "hew_remote_pid_tell",
        "hew_tcp_attach_local",
        "hew_sink_write_bytes",
        "hew_sink_try_write_bytes",
        // Channel/stream element-layout-witness entries: codegen
        // intercepts the `Terminator::Call` and emits the layout-witness
        // ABI (`hew-codegen-rs/src/llvm.rs` recv/send intercept arms).
        "hew_channel_recv_layout",
        "hew_channel_try_recv_layout",
        "hew_channel_send_layout",
        "hew_stream_next_layout",
        "hew_stream_try_next_layout",
        "hew_stream_send_layout",
        // Layout-backed HashMap projection ops: `Terminator::Call`
        // identities lowered by `lower_hashmap_layout_direct_call`
        // (`is_hashmap_layout_runtime_symbol`) and classified by the
        // layout-fact walker on their carried family.
        "hew_hashmap_keys_layout",
        "hew_hashmap_values_layout",
        // Constructor surface forms: `CalleeNameDispatchOnly` catalog
        // rows lowered by `lower_hashmap_constructor_call`
        // (`is_hashmap_constructor_symbol`).
        "HashMap::new",
        "HashSet::new",
    ]
    .into_iter()
    .collect();

    // Math intrinsics — user-visible names from
    // `hew-codegen-rs/src/llvm.rs::math_builtin_intrinsic`.
    let math_intrinsics: HashSet<&'static str> = [
        "sqrt", "exp", "log", "sin", "cos", "abs", "min", "max", "abs_f", "min_f", "max_f", "pow",
        "floor", "ceil", "round",
    ]
    .into_iter()
    .collect();

    // Stream/Sink ElementOverload + close peers — real exports from
    // `hew-types/src/builtin_names.rs:208-265`.
    let element_overload_extras: HashSet<&'static str> = [
        "hew_sink_write_string",
        "hew_sink_try_write_string",
        "hew_sink_try_write_bytes",
        "hew_channel_sender_close",
        "hew_channel_receiver_close",
        "hew_stream_close",
        "hew_sink_close",
    ]
    .into_iter()
    .collect();

    let mut fabricated = Vec::new();
    for family in all_runtime_call_families() {
        let sym = family.c_symbol();
        let resolved = is_known_runtime_symbol(sym)
            || codegen_intercepts.contains(sym)
            || math_intrinsics.contains(sym)
            || element_overload_extras.contains(sym);
        if !resolved {
            fabricated.push((family, sym));
        }
    }
    assert!(
        fabricated.is_empty(),
        "RuntimeCallFamily::c_symbol() produced strings that do not \
         resolve to any real runtime symbol, codegen intercept, math \
         intrinsic, or element-overload extra — these would be \
         fabricated identities the runtime cannot dispatch on. \
         Offenders: {fabricated:?}"
    );
}
