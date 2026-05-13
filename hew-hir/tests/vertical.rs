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
