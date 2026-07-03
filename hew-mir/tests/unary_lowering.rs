use hew_mir::{Instr, IrPipeline};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    hew_mir::lower_hir_module(&output.module)
}

fn instr_count(pipeline: &IrPipeline, pred: impl Fn(&Instr) -> bool) -> usize {
    pipeline
        .raw_mir
        .iter()
        .flat_map(|func| &func.blocks)
        .flat_map(|block| &block.instructions)
        .filter(|instr| pred(instr))
        .count()
}

#[test]
fn bool_not_emits_bool_not_instr() {
    let p = pipeline("fn main() -> bool { !false }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(instr_count(&p, |i| matches!(i, Instr::BoolNot { .. })), 1);
}

#[test]
fn integer_negate_of_literal_folds_no_checked_neg_instr() {
    // #2372: `-5` folds to a signed literal at HIR-lowering time; there is
    // no runtime negation left to overflow-check.
    let p = pipeline("fn main() -> i64 { -5 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(
        instr_count(&p, |i| matches!(i, Instr::IntNegChecked { .. })),
        0
    );
}

#[test]
fn integer_negate_of_non_literal_emits_checked_neg_instr() {
    // Negating a runtime value (not a literal) still goes through the
    // checked-negate MIR instruction -- only literal negation folds.
    let p = pipeline("fn main(n: i64) -> i64 { -n }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(
        instr_count(&p, |i| matches!(i, Instr::IntNegChecked { .. })),
        1
    );
}

#[test]
fn float_negate_emits_float_neg_instr() {
    let p = pipeline("fn main() -> i64 { let x: f64 = -1.5; 0 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(instr_count(&p, |i| matches!(i, Instr::FloatNeg { .. })), 1);
}

#[test]
fn integer_bitwise_not_emits_int_bit_not_instr() {
    let p = pipeline("fn main() -> i64 { ~0 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(instr_count(&p, |i| matches!(i, Instr::IntBitNot { .. })), 1);
}
