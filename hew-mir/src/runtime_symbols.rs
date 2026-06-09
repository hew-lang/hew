//! Runtime-ABI symbol allowlist for `Instr::CallRuntimeAbi`.
//!
//! `Instr::CallRuntimeAbi` carries a `String` symbol naming a
//! `hew_*` C-ABI entry in `hew-runtime/`. Accepting any string at
//! construction time would invite typos that survive to link-time
//! (or worse, silently route to a wrong runtime entry). Per
//! HEW-SPEC §3.7 boundary-fail-closed and LESSONS row P0
//! `boundary-fail-closed` (49), the producer validates every
//! symbol against this allowlist BEFORE the `Instr` enters the
//! `BasicBlock::instructions` stream. A symbol absent from the
//! allowlist surfaces as a hard `MirDiagnostic` so the failure
//! lands at MIR construction, not at codegen link-time.
//!
//! Source of truth: `scripts/jit-symbol-classification.toml`'s
//! `stable` list. The full toml is parsed by `hew-runtime`'s build
//! script — `hew-mir` carries the M2-substrate subset inline so
//! the allowlist check does not require a build-time fixture or
//! parsing step. When a new runtime-ABI symbol becomes producer-
//! emittable from MIR, add it to both lists in the same change.
//! The drift-test in `tests/runtime_symbols_classification.rs` —
//! TODO when a producer for a non-substrate symbol lands — would
//! cross-verify the two lists; for now the substrate list is
//! short enough to maintain by hand.
//!
//! WHY (M2 slice 4.5c shim): the typecheck→MIR bridge that maps
//! `Duplex<S, R>::send(msg)` (a `MethodCallRewrite` side-table entry
//! in `hew-types`) to a free-function call on a runtime ABI symbol
//! does not yet reach the Rust MIR pipeline (`hew compile-v05`).
//! Producers in `hew-mir` therefore have no callsite today; the
//! allowlist + the `Instr::CallRuntimeAbi` variant land first so
//! slice 5 codegen (LLVM IR emission) has a target to wire and
//! the producer-side bridge work that lights up callers can land
//! in a follow-up slice without retrofitting the `Instr` enum.
//! WHEN-OBSOLETE: when the typecheck→HIR/MIR bridge lands (a
//! 4.5b-follow-up slice owning the bridge decision), the unit
//! tests in this module become the discovery surface for "did the
//! bridge wire every symbol on the allowlist". WHAT: a real
//! consumer of `is_known_runtime_symbol` from the HIR-to-MIR
//! Call-lowering arm in `hew-mir/src/lower.rs`.

/// M2-substrate runtime-ABI symbols that an `Instr::CallRuntimeAbi`
/// may name. Sorted lexicographically for stable diffs and
/// binary-searchable membership.
///
/// Each entry is a `#[no_mangle] extern "C" fn` exported by
/// `hew-runtime/src/duplex.rs` (or the lambda-actor sibling
/// module). The full set in `scripts/jit-symbol-classification.toml`
/// `stable` is broader (per-actor / per-mailbox / per-IO entries);
/// the substrate subset that MIR producers can emit today is the
/// list below.
// Lexicographically sorted: `hew_duplex_*` < `hew_lambda_actor_*`
// < `hew_recv_half_*` < `hew_send_half_*`. Section comments mark
// the substrate-grouping for readability; the binary-search
// invariant is over the flat ordering.
const M2_RUNTIME_SYMBOLS: &[&str] = &[
    // --- Duplex<S, R> dual-queue substrate ----------------------
    "hew_duplex_clone",
    "hew_duplex_close",
    "hew_duplex_close_half",
    "hew_duplex_pair",
    "hew_duplex_payload_free",
    "hew_duplex_recv",
    "hew_duplex_recv_half",
    "hew_duplex_send",
    "hew_duplex_send_half",
    "hew_duplex_try_recv",
    "hew_duplex_try_send",
    // --- Lambda-actor surface (overlays Duplex<Msg, Reply>) -----
    "hew_lambda_actor_clone",
    "hew_lambda_actor_downgrade",
    "hew_lambda_actor_new",
    "hew_lambda_actor_release",
    "hew_lambda_actor_send",
    "hew_lambda_actor_weak_clone",
    "hew_lambda_actor_weak_drop",
    "hew_lambda_actor_weak_send",
    // --- RecvHalf<T> ---------------------------------------------
    "hew_recv_half_recv",
    "hew_recv_half_try_recv",
    // --- SendHalf<T> ---------------------------------------------
    "hew_send_half_send",
    "hew_send_half_try_send",
];

/// Error returned when a `RuntimeCall` is constructed with a symbol that
/// is not in the M2 runtime-ABI allowlist.
///
/// Carrying the rejected symbol string lets callers emit diagnostics that
/// name the exact offender (`MirDiagnosticKind::CutoverUnsupported`,
/// codegen assertions, unit-test assertions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownRuntimeSymbol(pub String);

impl std::fmt::Display for UnknownRuntimeSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "symbol `{}` is not in the M2 runtime-ABI allowlist \
             (see `runtime_symbols::M2_RUNTIME_SYMBOLS`)",
            self.0
        )
    }
}

/// Return `true` if `symbol` is a recognised runtime-ABI entry
/// `Instr::CallRuntimeAbi` may name. Binary search; the static
/// list is sorted in `M2_RUNTIME_SYMBOLS`.
#[must_use]
pub fn is_known_runtime_symbol(symbol: &str) -> bool {
    M2_RUNTIME_SYMBOLS.binary_search(&symbol).is_ok()
}

/// Borrow the full static allowlist. Useful for tests and dump
/// surfaces that want to enumerate the recognised symbols.
#[must_use]
pub fn known_runtime_symbols() -> &'static [&'static str] {
    M2_RUNTIME_SYMBOLS
}

/// Map a user-facing builtin name to its C-ABI runtime symbol, if one
/// exists.  Returns `None` for names that are not substrate builtins.
///
/// SHIM(E2→HIR): builtin functions like `duplex_pair` appear in HIR as
/// `BindingRef { name: "duplex_pair", resolved: Unresolved }` — the
/// user-facing name from the source — not the C-ABI `hew_duplex_pair`
/// name.  This table is the bridge.
///
/// WHY: the HIR `lower_identifier` function checks the local binding
/// scope and then the AST function-item registry.  Checker-registered
/// builtins (registered via `Checker::register_builtins`) are not in
/// the AST function-item registry, so `lower_identifier` falls through
/// to the `UnresolvedSymbol` + `Unresolved` path, preserving the
/// source-level name without the `hew_` prefix.  The MIR producer must
/// map it to the C-ABI name before calling `is_known_runtime_symbol`.
///
/// WHEN obsolete: when HIR gains a `ResolvedRef::Builtin { c_symbol }`
/// variant and the checker threads the `c_symbol` mapping into
/// `lower_identifier`, at which point MIR can match on the resolved
/// variant and this table becomes dead code.
///
/// WHAT: add `ResolvedRef::Builtin { c_symbol: String }` to `hew-hir`;
/// populate it in `lower_identifier` via the checker's builtin registry;
/// match on it in `lower.rs:lower_value` and remove this function.
#[must_use]
pub fn user_name_to_c_symbol(name: &str) -> Option<&'static str> {
    // Only a small subset of checker-registered builtins have a direct
    // C-ABI counterpart that MIR can emit today.  Extend this table
    // when a new builtin gains a producer arm.
    match name {
        "duplex_pair" => Some("hew_duplex_pair"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allowlist_is_sorted_for_binary_search() {
        // Binary-search correctness depends on the list being sorted.
        // A future contributor adding an entry out of order would
        // silently break membership for some symbols; pin the
        // invariant.
        for window in M2_RUNTIME_SYMBOLS.windows(2) {
            assert!(
                window[0] < window[1],
                "M2_RUNTIME_SYMBOLS is not lexicographically sorted: \
                 {} >= {}",
                window[0],
                window[1],
            );
        }
    }

    #[test]
    fn known_substrate_symbols_recognised() {
        // Every entry in the allowlist must round-trip.
        for sym in M2_RUNTIME_SYMBOLS {
            assert!(
                is_known_runtime_symbol(sym),
                "allowlist entry {sym} should be recognised",
            );
        }
    }

    #[test]
    fn unknown_symbol_rejected() {
        // A symbol the substrate does not emit must NOT be
        // recognised — typo-class bugs need to fail closed.
        assert!(!is_known_runtime_symbol("hew_duplex_sned"));
        assert!(!is_known_runtime_symbol("hew_duplex_send_t"));
        assert!(!is_known_runtime_symbol(""));
        assert!(!is_known_runtime_symbol("printf"));
    }

    #[test]
    fn substrate_quartet_present() {
        // Spot-check the four most load-bearing entries.
        // (These are the symbols a slice-5 lowering of Duplex
        // send/recv/close/pair lands first.)
        assert!(is_known_runtime_symbol("hew_duplex_pair"));
        assert!(is_known_runtime_symbol("hew_duplex_send"));
        assert!(is_known_runtime_symbol("hew_duplex_recv"));
        assert!(is_known_runtime_symbol("hew_duplex_close"));
    }
}
