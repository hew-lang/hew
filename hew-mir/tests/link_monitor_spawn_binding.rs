//! Regression: spawning an actor with no MIR layout (e.g. an undefined
//! actor name) must not leave a binding in `owned_locals` without a
//! matching `binding_locals` entry.
//!
//! Before the fix, `let p = spawn Probe;` for an undefined `Probe`
//! pushed `p` into `owned_locals` (because `LocalPid<Probe>` is not
//! `BitCopy`) but skipped `binding_locals` insertion (the let-arm only
//! wires `binding_locals` when `lower_value` returns `Some`). The
//! drop-elaboration pass then panicked with `build_lifo_drops
//! invariant`. The fix gates the `owned_locals` push on the same
//! condition that wires `binding_locals`, so the two ledgers stay in
//! agreement.
//!
//! The type checker now rejects the undefined `Probe` up front (each
//! `LocalPid<Probe>` annotation and `spawn Probe` reports an
//! unknown-type error), so this program never reaches MIR through the
//! normal compiler pipeline. This test drives HIR and MIR lowering
//! directly regardless of those type errors to keep the MIR-boundary
//! fail-closed covered as defense in depth: lowering must still
//! complete without a panic and report `spawn of unknown actor` for
//! each binding.
//!
//! Crash artifact: `fuzz_mir/crash-259306105545067b47125ebbb163feaa185b97cc`.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::lower_hir_module;
use hew_types::error::TypeErrorKind;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const FIXTURE: &str = include_str!("fixtures/link_monitor_spawn_binding.hew");

#[test]
fn link_monitor_after_four_spawns_lowers_without_panic() {
    let parsed = hew_parser::parse(FIXTURE);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc = checker.check_program(&parsed.program);
    // The checker front-runs the MIR boundary: every reference to the
    // undefined `Probe` is rejected as an unknown type.
    assert!(
        !tc.errors.is_empty()
            && tc
                .errors
                .iter()
                .all(|e| e.kind == TypeErrorKind::UndefinedType
                    && e.message.contains("unknown type `Probe`")),
        "expected unknown-type errors for `Probe`, got: {:#?}",
        tc.errors
    );

    // Defense in depth: drive HIR and MIR lowering despite the type
    // errors so the drop-elaboration ledger invariant stays guarded.
    let hir = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );

    // Pre-fix: this panics at `build_lifo_drops invariant`.
    // Post-fix: lowering completes; MIR diagnostics report
    // `spawn of unknown actor `Probe`` but no panic.
    let pipeline = lower_hir_module(&hir.module);
    let mir_diag_kinds: Vec<_> = pipeline
        .diagnostics
        .iter()
        .map(|d| format!("{:?}", d.kind))
        .collect();
    assert!(
        mir_diag_kinds
            .iter()
            .any(|s| s.contains("spawn of unknown actor")),
        "expected `spawn of unknown actor` diagnostic, got: {mir_diag_kinds:#?}"
    );
}
