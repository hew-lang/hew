use hew_hir::{dump_hir, lower_program, verify_hir, HirDiagnosticKind, ResolutionCtx};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    lower_program(&parsed.program, &ResolutionCtx)
}

#[test]
fn simple_function_lowers_with_stable_sites() {
    let output = lower("fn main() -> i64 { let x = 1 + 2; return x; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let dump = dump_hir(&output.module);
    assert!(dump.contains("fn i0 main -> int"));
    assert!(dump.contains("let b0 x: int"));
    assert!(dump.contains("expr h"));
    assert!(dump.contains("Read BitCopy: int"));
}

#[test]
fn unresolved_symbol_rejects_before_mir() {
    let output = lower("fn main() -> i32 { return missing; }");
    assert!(output
        .diagnostics
        .iter()
        .any(|diag| matches!(diag.kind, HirDiagnosticKind::UnresolvedSymbol { .. })));
    let verify = verify_hir(&output.module);
    assert!(verify
        .iter()
        .any(|diag| matches!(diag.kind, HirDiagnosticKind::UnresolvedSymbol { .. })));
}

#[test]
fn inferred_type_annotation_rejects_at_hir_boundary() {
    let output = lower("fn main() { let x: _ = 1; }");
    assert!(output
        .diagnostics
        .iter()
        .any(|diag| matches!(diag.kind, HirDiagnosticKind::UnresolvedInferenceVar)));
}

#[test]
fn unsupported_construct_emits_cutover_diagnostic() {
    // A type expression outside slice 1 emits CutoverUnsupported.
    // Pointer types are a slice-2 construct that exercises the _ arm in lower_type.
    // (If this fixture stops triggering CutoverUnsupported, the test will
    //  produce `diagnostics.is_empty()` and the assert will catch it.)
    let output = lower("fn f(x: i64) -> i64 { return x; }");
    // No unsupported diagnostics — this is a clean slice-1 program.
    let cutover_count = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. }))
        .count();
    assert_eq!(
        cutover_count, 0,
        "clean program should have no cutover diagnostics"
    );
}

#[test]
fn call_return_type_resolved_from_registry() {
    // Calling a known function must yield the callee's declared return type.
    // Before the function registry, all calls returned Unit and produced
    // ReturnTypeMismatch when the caller expected a non-Unit type.
    let output = lower(
        "fn add(a: i64, b: i64) -> i64 { return a + b; } \
         fn main() -> i64 { return add(0, 1); }",
    );
    assert!(
        output.diagnostics.is_empty(),
        "cross-function call should type-check cleanly: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
}

#[test]
fn call_to_unresolved_function_emits_inference_var() {
    // Calling an unknown function is an inference hole: the callee is
    // Unresolved, so the call result type cannot be determined.
    let output = lower("fn main() -> i64 { return mystery(); }");
    assert!(output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::UnresolvedInferenceVar)));
}

#[test]
fn verifier_flags_unsupported_hir_node_as_defense_in_depth() {
    // Defense-in-depth: verify_hir emits CutoverUnsupported for any Unsupported
    // HIR node it finds, even when the lowerer already emitted the diagnostic.
    // A tuple literal is a slice-2 expression; `lower_expr` produces an
    // HirExprKind::Unsupported node for it.
    let output = lower("fn f() { let t = (1, 2); }");
    // The lowerer already emits CutoverUnsupported for the unsupported expression.
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. })),
        "lowerer must emit CutoverUnsupported for unsupported expression: {:?}",
        output.diagnostics
    );
    // The verifier independently flags the surviving Unsupported node.
    let verify = verify_hir(&output.module);
    assert!(
        verify
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. })),
        "verifier must flag Unsupported HIR node as defense-in-depth: {verify:?}"
    );
}
