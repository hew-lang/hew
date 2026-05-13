use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{lower_hir_module, MirDiagnosticKind, MirStatement};

fn pipeline(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
    lower_hir_module(&output.module)
}

#[test]
fn string_return_reaches_hew_mlir_with_site_decisions() {
    let pipeline = pipeline(r#"fn main() -> String { let s = "hello"; return s; }"#);
    let mlir = pipeline.hew_mlir.dump();

    assert!(mlir.contains("hew.func @main"));
    assert!(mlir.contains("hew.bind @s : String"));
    assert!(mlir.contains("hew.return : String"));
    assert!(mlir.contains("hew.site_id"));
    assert!(mlir.contains("hew.value_decision"));
    assert!(!mlir.contains("hew.drop @s"));
}

#[test]
fn elaborated_mir_makes_owned_string_drop_explicit() {
    let pipeline = pipeline(r#"fn main() { let s = "hello"; }"#);
    let func = &pipeline.elaborated_mir[0];
    assert!(func
        .statements
        .iter()
        .any(|stmt| { matches!(stmt, MirStatement::Drop { name, .. } if name == "s") }));
}

#[test]
fn bitcopy_arithmetic_has_no_drop() {
    let pipeline = pipeline("fn main() -> i64 { let x = 1 + 2; return x; }");
    let func = &pipeline.elaborated_mir[0];
    assert!(!func
        .statements
        .iter()
        .any(|stmt| matches!(stmt, MirStatement::Drop { .. })));
}

#[test]
fn checked_mir_rejects_use_after_consume() {
    let pipeline = pipeline(r#"fn main() -> String { let s = "hello"; let t = s; return s; }"#);

    assert!(pipeline.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        MirDiagnosticKind::UseAfterConsume { ref name, .. } if name == "s"
    )));
}

#[test]
fn arithmetic_reaches_hew_mlir() {
    let pipeline = pipeline("fn main() -> i64 { let a = 3; let b = 4; return a + b; }");
    let mlir = pipeline.hew_mlir.dump();
    assert!(mlir.contains("hew.func @main"));
    assert!(mlir.contains("hew.return : int"));
    assert!(mlir.contains("hew.site_id"));
}

#[test]
fn cross_function_call_types_return_correctly() {
    // With the function registry, calling add() returns i64, not Unit.
    // Before the registry, this would produce ReturnTypeMismatch in HIR
    // and the pipeline() helper would panic on diagnostics.
    let pipeline = pipeline(
        "fn add(a: i64, b: i64) -> i64 { return a + b; } \
         fn main() -> i64 { return add(0, 1); }",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    let mlir = pipeline.hew_mlir.dump();
    assert!(mlir.contains("hew.func @add"));
    assert!(mlir.contains("hew.func @main"));
    assert!(mlir.contains("hew.return : int"));
}

#[test]
fn unknown_user_type_rejected_before_mlir() {
    // D10: Named user types with no known ValueClass must be rejected at the
    // MIR boundary.  They must never reach MLIR with "UnknownBlocked" in
    // hew.value_decision.
    let parsed = hew_parser::parse("fn f(x: Foo) -> Foo { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    // HIR has no error — type annotations resolve to Named for any identifier.
    // The fail-closed boundary is MIR.
    assert!(
        output.diagnostics.is_empty(),
        "HIR should not error on undeclared type name alone: {:?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::UnknownType { .. })),
        "unknown named type must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
    // MLIR must be empty — not emitted when there are diagnostics.
    assert!(
        pipeline.hew_mlir.dump().is_empty(),
        "MLIR must not be emitted when UnknownType diagnostics are present"
    );
}
