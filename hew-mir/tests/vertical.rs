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
    let names: Vec<&str> = pipeline.raw_mir.iter().map(|f| f.name.as_str()).collect();
    assert!(
        names.contains(&"add"),
        "raw_mir must include add: {names:?}"
    );
    assert!(
        names.contains(&"main"),
        "raw_mir must include main: {names:?}"
    );
}

#[test]
fn unknown_user_type_rejected_at_mir_boundary() {
    // D10: Named user types with no known ValueClass must be rejected at the
    // MIR boundary so they cannot reach the backend.
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
}

#[test]
fn nested_tuple_user_type_rejected_at_mir_boundary() {
    let parsed = hew_parser::parse("fn f(x: (Foo, i64)) -> (Foo, i64) { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested tuple Foo must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn nested_array_user_type_rejected_at_mir_boundary() {
    let parsed = hew_parser::parse("fn f(x: [Foo; 2]) -> [Foo; 2] { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested array Foo must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}
