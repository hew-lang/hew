use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, CmpPred, Instr, IrPipeline};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

fn find_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in raw_mir"))
}

#[test]
fn match_literal_string_lowers_to_stringlit_intcmp_chain() {
    let pipeline = pipeline_with_tc(
        r#"
fn classify(s: string) -> i64 {
    match s {
        "yes" => 1,
        "no" => 0,
        _ => -1,
    }
}"#,
    );
    let func = find_fn(&pipeline, "classify");
    let instrs: Vec<_> = func
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .collect();

    let string_literals: Vec<_> = instrs
        .iter()
        .filter_map(|instr| {
            if let Instr::StringLit { bytes, .. } = instr {
                Some(bytes.as_slice())
            } else {
                None
            }
        })
        .collect();
    assert!(
        string_literals.contains(&b"yes".as_slice()),
        "expected a StringLit for the first pattern, got {string_literals:?}"
    );
    assert!(
        string_literals.contains(&b"no".as_slice()),
        "expected a StringLit for the second pattern, got {string_literals:?}"
    );

    let eq_cmps = instrs
        .iter()
        .filter(|instr| {
            matches!(
                instr,
                Instr::IntCmp {
                    pred: CmpPred::Eq,
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        eq_cmps, 2,
        "string literal match should compare once per literal arm"
    );
}
