use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::MirDiagnosticKind;

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
}
