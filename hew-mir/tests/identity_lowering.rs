/// MIR lowering tests for `Expr::Is` → `Instr::IdentityCompare`.
///
/// These tests exercise the HIR→MIR lowering path for the `is` identity
/// operator.  They run below the checker (D-2) — `TypeCheckOutput::default()`
/// is used so the allowance-set rules are not applied here; that is the
/// checker's sole responsibility.  These tests pin only the MIR shape.
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr};
use hew_types::TypeCheckOutput;

fn pipeline(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");
    lower_hir_module(&output.module)
}

#[test]
fn is_operator_emits_identity_compare_instr() {
    // The `is` operator on two i64 bindings (type validity is the checker's
    // responsibility; MIR lowering must emit `Instr::IdentityCompare`
    // regardless of the operand type).
    let pipeline = pipeline("fn f() -> i64 { let a: i64 = 1; let b: i64 = 2; let r = a is b; 0 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "`is` lowering must not produce MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let has_identity_cmp = func.blocks[0]
        .instructions
        .iter()
        .any(|i| matches!(i, Instr::IdentityCompare { .. }));
    assert!(
        has_identity_cmp,
        "`a is b` must lower to Instr::IdentityCompare; instructions: {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn is_operator_result_has_three_places() {
    // `Instr::IdentityCompare { dest, lhs, rhs }` must carry exactly three
    // places so the dataflow and drop-elaboration passes can discover all
    // Place references (mirrors `instr_places` coverage for IntCmp).
    let pipeline = pipeline("fn f() -> i64 { let a: i64 = 1; let b: i64 = 2; let r = a is b; 0 }");
    let func = &pipeline.raw_mir[0];
    let instr = func.blocks[0]
        .instructions
        .iter()
        .find(|i| matches!(i, Instr::IdentityCompare { .. }))
        .expect("IdentityCompare must be present");
    // Destructure to confirm the three-place shape compiles (field names).
    let Instr::IdentityCompare { dest, lhs, rhs } = instr else {
        panic!("expected IdentityCompare");
    };
    // All three places must be distinct locals (the HIR allocates a fresh
    // dest for the result; lhs and rhs are the operand bindings).
    assert_ne!(dest, lhs, "dest and lhs must be different places");
    assert_ne!(dest, rhs, "dest and rhs must be different places");
}

#[test]
fn is_operator_does_not_emit_int_cmp() {
    // `is` must not lower to `Instr::IntCmp`. Identity comparison is a
    // distinct operation that codegen handles via `ptrtoint` + `icmp eq`
    // for pointer-shaped types, not via the scalar integer-compare path.
    let pipeline = pipeline("fn f() -> i64 { let a: i64 = 1; let b: i64 = 2; let r = a is b; 0 }");
    let func = &pipeline.raw_mir[0];
    let has_int_cmp = func.blocks[0]
        .instructions
        .iter()
        .any(|i| matches!(i, Instr::IntCmp { .. }));
    assert!(
        !has_int_cmp,
        "`a is b` must not lower to Instr::IntCmp; `is` has its own instruction variant"
    );
}
