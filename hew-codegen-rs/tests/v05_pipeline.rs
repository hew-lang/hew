use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::MirDiagnosticKind;
use hew_types::TypeCheckOutput;

#[test]
fn v05_pipeline_rejects_nested_named_type_before_codegen() {
    let parsed = hew_parser::parse("fn f(x: (Foo, i64)) -> (Foo, i64) { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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

/// Bool literals lower to `Instr::ConstI64 { value: 0/1 }` once the CFG
/// construction lane lands its bool + cmp prerequisite (Slice 0). The
/// stored width is whatever HIR resolved (`ResolvedTy::Bool`, mapped to
/// i8 by `primitive_to_llvm`); `ConstI64`'s store truncates the value
/// to the dest local's width. The MIR pipeline must accept `fn main()
/// -> bool { true }` cleanly — no `CutoverUnsupported` for bool
/// literals, no `UnresolvedPlace`, nothing else.
#[test]
fn v05_pipeline_accepts_bool_literal_return() {
    let parsed = hew_parser::parse("fn main() -> bool { true }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir gate must accept this: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "bool literal must lower cleanly without diagnostics: {:?}",
        pipeline.diagnostics
    );
}

/// Float literals now lower to `Instr::FloatLit` in the MIR. The MIR
/// diagnostic stream must be empty — no `CutoverUnsupported` — for a
/// simple `fn main() -> f64 { 1.5 }` program.
#[test]
fn v05_pipeline_accepts_float_literal_in_mir() {
    let parsed = hew_parser::parse("fn main() -> f64 { 1.5 }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir must accept float literal: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "float literal must lower without MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
}

/// A direct call to a module function now lowers cleanly via
/// `Instr::CallDirect`. `main` returns `add(10, 32)`; the MIR pipeline must
/// accept the program without `CutoverUnsupported` or `UnresolvedPlace`
/// diagnostics, and codegen must emit valid LLVM IR.
#[test]
fn v05_pipeline_accepts_user_fn_call_via_call_direct() {
    let parsed = hew_parser::parse(
        "fn add(x: int, y: int) -> int { x + y }\n\
         fn main() -> int { add(10, 32) }\n",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir must accept user-fn call: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "user-fn call pipeline must have no MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
}
