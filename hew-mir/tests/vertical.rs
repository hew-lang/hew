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
