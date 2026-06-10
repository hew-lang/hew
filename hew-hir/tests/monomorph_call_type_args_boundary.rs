//! Contract test: poisoned `call_type_args` entry must fail closed.
//!
//! A `TypeCheckOutput` whose `call_type_args` map holds a `Ty::Error` at a
//! call-site span must cause HIR lowering to:
//!   1. Emit `HirDiagnosticKind::MonomorphisationCallTypeArgsViolation`.
//!   2. Have `LowerOutput::into_result()` return `Err` — the corrupt
//!      monomorphisation entry must never pass the public pipeline boundary.
//!
//! This pins the fail-closed contract documented in `HirDiagnosticKind::
//! MonomorphisationCallTypeArgsViolation` ("Fail-closed per
//! `checker-authority` (P0) — never silently drop a poisoned entry").

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx};
use hew_parser::ast::{Expr, Item, Stmt};
use hew_types::{SpanKey, Ty, TypeCheckOutput};

/// A call to a generic function with its `call_type_args` entry poisoned as
/// `Ty::Error` must produce `MonomorphisationCallTypeArgsViolation` AND must
/// cause `into_result()` to return `Err`.
#[test]
fn poisoned_call_type_args_ty_error_emits_violation_and_fails_closed() {
    // Source: a generic function `identity<T>(x: T) -> T` and a call site
    // `identity(42)` so there is exactly one Expr::Call span to locate.
    let source = "fn identity<T>(x: T) -> T { return x; } fn main() { identity(42); }";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    // Walk: items[1] (main) → FnDecl → body.stmts[0] → Expression(call_expr)
    let (Item::Function(main_fn), _) = &parsed.program.items[1] else {
        panic!("expected Item::Function at index 1 (main)");
    };
    let (Stmt::Expression(call_spanned), _) = &main_fn.body.stmts[0] else {
        panic!(
            "expected Stmt::Expression as first statement; got: {:?}",
            main_fn.body.stmts[0]
        );
    };
    let (Expr::Call { .. }, call_span) = call_spanned else {
        panic!("expected Expr::Call; got: {:?}", call_spanned.0);
    };

    // Inject a Ty::Error at the call site into call_type_args.
    // Ty::Error → BoundaryError::TaintedError via ResolvedTy::from_ty,
    // which must trigger MonomorphisationCallTypeArgsViolation.
    let span_key = SpanKey {
        start: call_span.start,
        end: call_span.end,
        module_idx: 0,
    };
    let mut tc = TypeCheckOutput::default();
    tc.call_type_args.insert(span_key, vec![Ty::Error]);

    let lower_output = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    // The call_type_args boundary violation diagnostic must be present.
    let violations: Vec<_> = lower_output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::MonomorphisationCallTypeArgsViolation { .. }
            )
        })
        .collect();
    assert!(
        !violations.is_empty(),
        "poisoned call_type_args (Ty::Error) must produce \
         MonomorphisationCallTypeArgsViolation; got diagnostics: {:#?}",
        lower_output.diagnostics
    );

    let HirDiagnosticKind::MonomorphisationCallTypeArgsViolation { callee, reason } =
        &violations[0].kind
    else {
        unreachable!()
    };
    assert_eq!(callee, "identity", "violation callee must be 'identity'");
    assert!(
        reason.to_lowercase().contains("taint")
            || reason.to_lowercase().contains("error")
            || reason.to_lowercase().contains("poison"),
        "reason must mention the tainted/error/poison nature; got: {reason:?}"
    );

    // into_result() must return Err: MonomorphisationCallTypeArgsViolation must
    // not be silently swallowed at the public boundary.
    let lower_output2 = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        lower_output2.into_result().is_err(),
        "into_result() must return Err when MonomorphisationCallTypeArgsViolation \
         is present; got Ok(module)"
    );
}
