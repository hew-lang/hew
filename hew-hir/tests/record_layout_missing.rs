//! Regression test for `RecordLayoutMissing` (fail-closed P1 fix).
//!
//! Pins the contract: when a generic record init site is accepted by the
//! type-checker (`expr_types` has the span) but `record_init_type_args`
//! has no entry for that span, HIR lowering must emit
//! `RecordLayoutMissing` instead of silently producing
//! `Named { args: [] }` and passing a broken shape to MIR/codegen.
//!
//! The missing-entry condition is simulated by stripping
//! `record_init_type_args` from an otherwise-clean `TypeCheckOutput`
//! before calling `lower_program`.  This mirrors the "missed re-record
//! path" described in the `record_record_layout` comment.
//!
//! LESSONS: `feedback-fail-closed-not-pretend` — no silent
//! fallthrough on a missing checker side-table entry.

use hew_hir::{lower_program, HirDiagnosticKind, LowerOutput, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Strips `record_init_type_args` after a clean typecheck, then lowers.
/// Verifies that `RecordLayoutMissing` fires for the generic record and
/// that it carries the correct record name.
#[test]
fn missing_record_init_type_args_emits_record_layout_missing() {
    let source = r"
        pub type Box<T> { value: T }

        fn main() {
            let a: Box<i64> = Box { value: 42 };
        }
    ";

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "typecheck errors: {:#?}",
        tc_output.errors
    );

    // Pre-condition: the checker recorded the Box<i64> init site.
    assert!(
        !tc_output.record_init_type_args.is_empty(),
        "expected checker to record type args for Box<i64> init; \
         record_init_type_args is empty — test pre-condition invalid"
    );

    // Simulate the missed re-record path: the checker accepted the init
    // expression (`expr_types` retains the span) but the type-arg entry
    // is absent (e.g. an explicit-type-arg branch that omits the call to
    // `record_concrete_record_init_type_args`).
    tc_output.record_init_type_args.clear();

    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let missing_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::RecordLayoutMissing { .. }))
        .collect();
    assert!(
        !missing_diags.is_empty(),
        "expected at least one RecordLayoutMissing diagnostic; \
         got: {:#?}",
        output.diagnostics
    );
    match &missing_diags[0].kind {
        HirDiagnosticKind::RecordLayoutMissing { record } => {
            assert_eq!(record, "Box", "diagnostic must name the offending record");
        }
        _ => unreachable!(),
    }

    // Fail-closed boundary contract: `into_result()` must return `Err` when
    // `RecordLayoutMissing` is present — downstream consumers that call
    // `into_result()` must not silently receive an under-instantiated module.
    let result = LowerOutput {
        module: output.module,
        diagnostics: output.diagnostics,
    }
    .into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err when RecordLayoutMissing is present; \
         got Ok — fail-closed boundary is broken"
    );
    let err_diags = result.unwrap_err();
    assert!(
        err_diags
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::RecordLayoutMissing { .. })),
        "Err diagnostics must contain RecordLayoutMissing; got: {err_diags:#?}",
    );
}
