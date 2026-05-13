use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::MirDiagnosticKind;

fn emit_from_v05_pipeline(source: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );

    hew_codegen_rs::emit_mlir(&pipeline.hew_mlir)
}

#[test]
fn v05_pipeline_rejects_nested_named_type_before_codegen() {
    let parsed = hew_parser::parse("fn f(x: (Foo, i64)) -> (Foo, i64) { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested named type must be rejected before codegen: {:?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline.hew_mlir.functions.is_empty(),
        "MLIR functions must not be emitted for rejected nested named types"
    );
}

#[test]
fn v05_pipeline_emits_d7_canonical_type_strings() {
    let string_text = emit_from_v05_pipeline(r#"fn main() -> String { return "hello"; }"#);
    assert!(
        string_text.contains("hew.return : !hew.string"),
        "String must lower through emit_mlir as !hew.string: {string_text}"
    );
    assert!(
        !string_text.contains("hew.return : String"),
        "the canonical emitter must not use HewMlirModule::dump() user-facing types: {string_text}"
    );

    let int_text = emit_from_v05_pipeline("fn main() -> i64 { return 1 + 2; }");
    assert!(
        int_text.contains("hew.return : i64"),
        "i64 must lower through emit_mlir as i64: {int_text}"
    );
    assert!(
        !int_text.contains("hew.return : int"),
        "the canonical emitter must not use HewMlirModule::dump() user-facing int: {int_text}"
    );
}
