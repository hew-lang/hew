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

/// As above for float literals — `1.5` cannot be moved into the return slot
/// because `Instr::ConstF64` does not exist in the Cluster 1 spine.
#[test]
fn v05_pipeline_rejects_float_literal_return_before_codegen() {
    let parsed = hew_parser::parse("fn main() -> f64 { 1.5 }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir gate must accept this; cutover happens at MIR: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::CutoverUnsupported { construct, .. }
                if construct == "float literal"
        )),
        "float literal must surface a CutoverUnsupported diagnostic: {:?}",
        pipeline.diagnostics
    );
}

/// A call expression must fail closed at MIR. `main` returns the result of
/// `add(10, 32)`; without Call lowering the return slot is uninitialised, so
/// the cutover boundary refuses the program and the driver short-circuits.
/// The function parameters surface `UnresolvedPlace` diagnostics as well —
/// either signal is sufficient to reject the program.
#[test]
fn v05_pipeline_rejects_call_expression_before_codegen() {
    let parsed = hew_parser::parse(
        "fn add(x: int, y: int) -> int { x + y }\n\
         fn main() -> int { add(10, 32) }\n",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir gate must accept this; cutover happens at MIR: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    let has_call_reject = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::CutoverUnsupported { construct, .. }
                if construct == "function call"
        )
    });
    let has_unresolved_param = pipeline
        .diagnostics
        .iter()
        .any(|d| matches!(&d.kind, MirDiagnosticKind::UnresolvedPlace { .. }));
    assert!(
        has_call_reject || has_unresolved_param,
        "call expression with parameter use must surface either a \
         CutoverUnsupported{{construct=\"function call\"}} or an \
         UnresolvedPlace diagnostic: {:?}",
        pipeline.diagnostics
    );
}
