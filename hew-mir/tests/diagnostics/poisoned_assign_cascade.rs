//! Dogfood WO6 — the assignment-target `UnresolvedPlace` cascade.
//!
//! When a `let`/`var` initializer fails to lower, the binding never gets a
//! `binding_locals` slot and the initializer's failure is recorded in
//! `poisoned_let_bindings`. Before this fix, a later ASSIGNMENT to that
//! binding (unlike a later READ) had no poison guard, so it pushed a second,
//! misleading `UnresolvedPlace` diagnostic on top of the real root error.
//!
//! This test drives the exact reproducer from the WO6 finding through the
//! full pipeline (parse → check → HIR → MIR) and asserts the compile now
//! produces exactly one diagnostic — the real root cause — not two.

use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:#?}",
        tc_output.errors
    );
    let hir = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics must be empty for this reproducer: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

/// The exact WO6 reproducer: a `var` initialized from a `?`-scrutinee call
/// that returns a borrowed parameter (unsupported, NYI at MIR lowering) is
/// later reassigned. Before the fix this produced TWO build-failing
/// diagnostics: the NYI root cause, then a spurious `UnresolvedPlace` for
/// the reassignment (`left = fresh`). Assert the count of HARD (non-advisory)
/// diagnostics exactly — a `>= 1` assertion would pass on the buggy
/// compiler. `MirDiagnosticKind::is_advisory` is the crate's single source
/// of severity truth (model.rs) — this reproducer also emits an unrelated
/// advisory `ObligationUnderReleased` warning on `fresh` (a pre-existing,
/// out-of-scope drop-plan consequence of the poisoned assignment never
/// consuming its RHS; the CLI renders it as a non-failing `warning`, not an
/// `error`), so the exact-count assertion is scoped to hard errors, which is
/// what the plan's CLI reproducer output (`hew build`, two `error:` lines)
/// actually measured.
#[test]
fn poisoned_var_reassignment_reports_exactly_one_hard_diagnostic() {
    let pipeline = pipeline_with_tc(
        r"
        fn passthru(v: Vec<string>) -> Result<Vec<string>, string> { Ok(v) }
        fn caller(v: Vec<string>) -> Result<i64, string> {
            var left = passthru(v)?;
            let fresh: Vec<string> = Vec::new();
            left = fresh;
            Ok(left.len())
        }
        ",
    );

    let hard: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| !d.kind.is_advisory())
        .collect();

    assert_eq!(
        hard.len(),
        1,
        "expected exactly one hard (build-failing) diagnostic — the root NYI; got {:#?}",
        pipeline.diagnostics
    );
    assert!(
        matches!(hard[0].kind, MirDiagnosticKind::NotYetImplemented { .. }),
        "the sole hard diagnostic must be the root NotYetImplemented cause, not a \
         cascade; got {:#?}",
        hard[0]
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::UnresolvedPlace { .. })),
        "no UnresolvedPlace cascade may follow the root NotYetImplemented diagnostic \
         (advisory or otherwise); got {:#?}",
        pipeline.diagnostics
    );
}

// A `let`-bound (not `var`) poisoned binding cannot be exercised through
// this pipeline: the checker rejects any assignment to an immutable `let`
// binding (`cannot assign to immutable variable`, hew-types/src/error.rs)
// BEFORE MIR lowering runs, regardless of whether the binding is poisoned.
// The acceptance bar's "a let-bound poisoned binding assigned later behaves
// the same way" is satisfied structurally, not by a second source-level
// reproducer: `poisoned_let_bindings` is a single `HashSet<BindingId>`
// populated uniformly for both `let` and `var` initializer failures
// (hew-mir/src/lower/expr.rs:1268), and the assign-arm guard added by this
// lane checks only BindingId membership — it has no let/var branch to
// diverge on. The unit-level test
// `poisoned_assign_target_cascade::assign_to_poisoned_binding_emits_no_diagnostic`
// exercises that guard directly on synthetic HIR, independent of which
// keyword introduced the binding.
