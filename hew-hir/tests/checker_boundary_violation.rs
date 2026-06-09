//! Contract test for the `CheckerBoundaryViolation` diagnostic path.
//!
//! The `Expr::Call` arm in HIR lowering consults `expr_types` from
//! `TypeCheckOutput` and converts the stored `Ty` via `ResolvedTy::from_ty`.
//! When `from_ty` returns `Err(BoundaryError)` — e.g. the side-table holds an
//! unresolved inference variable — the lowerer must:
//!
//!   1. Emit `HirDiagnosticKind::CheckerBoundaryViolation` with the callee
//!      name and the `BoundaryError` description.
//!   2. Fall back to `ResolvedTy::Unit` for the call's type so lowering can
//!      continue (skeleton recovery).
//!
//! This test pins that fail-closed contract.  It constructs a
//! `TypeCheckOutput` whose `expr_types` entry at the call-expression span
//! holds a poisoned `Ty::Var(TypeVar(0))` — an unresolved inference variable —
//! and verifies the diagnostic fires AND that `into_result()` returns `Err`.
//!
//! LESSONS: checker-output-boundary (P0) — the boundary conversion is
//! fail-closed; a poisoned side-table must never silently resolve to `Unit`.
//! This test pins the path through `lower.rs:1027-1045` and the
//! `LowerOutput::into_result` boundary at the public API.

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx};
use hew_parser::ast::{Expr, Item, Stmt};
use hew_types::ty::TypeVar;
use hew_types::{SpanKey, Ty, TypeCheckOutput};

/// `foo()` in a function body, with the call-site `expr_types` entry
/// poisoned as an unresolved inference variable, must produce
/// `HirDiagnosticKind::CheckerBoundaryViolation { name: "foo", reason }` where
/// `reason` contains "inference" (from `BoundaryError::UnresolvedInference`).
#[test]
fn poisoned_expr_types_emits_checker_boundary_violation() {
    // Source: a single call expression statement so there's exactly one
    // Expr::Call span to locate.
    let source = "fn main() -> i64 { foo(); return 0; }";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    // Walk: items[0] → FnDecl → body.stmts[0] → Expression(spanned_call_expr)
    let (Item::Function(fn_decl), _) = &parsed.program.items[0] else {
        panic!("expected Item::Function as first item");
    };
    let (Stmt::Expression(call_spanned), _) = &fn_decl.body.stmts[0] else {
        panic!(
            "expected Stmt::Expression as first statement; got: {:?}",
            fn_decl.body.stmts[0]
        );
    };
    let (Expr::Call { function, .. }, call_span) = call_spanned else {
        panic!("expected Expr::Call; got: {:?}", call_spanned.0);
    };
    // Confirm the callee is `foo` so the assertion below is meaningful.
    let (Expr::Identifier(callee_name), _) = function.as_ref() else {
        panic!("expected Identifier callee; got: {:?}", function.0);
    };
    assert_eq!(callee_name, "foo", "callee must be foo");

    // Poison the call span with an unresolved inference variable.
    // TypeVar(0) is a fixed literal — no global-counter side-effects.
    let span_key = SpanKey {
        start: call_span.start,
        end: call_span.end,
    };
    // W4.015: behavior pin — hand-poison expr_types with an unresolved
    // TypeVar to cover the permanent CheckerBoundaryViolation path.
    let mut tc = TypeCheckOutput::default();
    tc.insert_expr_type(span_key, Ty::Var(TypeVar(0)));

    let lower_output = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    // The boundary violation diagnostic must be present.
    let violations: Vec<_> = lower_output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::CheckerBoundaryViolation { .. }))
        .collect();
    assert!(
        !violations.is_empty(),
        "poisoned expr_types must produce CheckerBoundaryViolation; \
         got diagnostics: {:#?}",
        lower_output.diagnostics
    );

    // The violation must name the callee and describe the inference failure.
    let HirDiagnosticKind::CheckerBoundaryViolation { name, reason } = &violations[0].kind else {
        unreachable!()
    };
    assert_eq!(name, "foo", "violation name must be the callee name");
    assert!(
        reason.to_lowercase().contains("inference") || reason.to_lowercase().contains("unresolved"),
        "reason must mention inference or unresolved; got: {reason:?}"
    );

    // into_result() must return Err: a CheckerBoundaryViolation must not be
    // silently swallowed at the public boundary.
    let lower_output2 = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        lower_output2.into_result().is_err(),
        "into_result() must return Err when CheckerBoundaryViolation is present"
    );
}
