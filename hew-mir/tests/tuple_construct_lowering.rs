use hew_hir::{lower_program, ResolutionCtx, TargetArch};
use hew_mir::{lower_hir_module, model::Instr};
use hew_parser::parse;
use hew_types::{module_registry::ModuleRegistry, Checker};

#[test]
fn tuple_construct_emitted_for_literal() {
    let source = r"
fn main() -> (i64, bool) {
    (42, true)
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let hir_output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        hir_output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir_output.diagnostics
    );

    let mir_pipeline = lower_hir_module(&hir_output.module);
    assert!(
        mir_pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir_pipeline.diagnostics
    );

    // Find the main function in raw MIR
    let main_fn = mir_pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main function not found in MIR");

    // Check that TupleConstruct instruction exists
    let has_tuple_construct = main_fn.blocks.iter().any(|block| {
        block
            .instructions
            .iter()
            .any(|instr| matches!(instr, Instr::TupleConstruct { .. }))
    });

    assert!(
        has_tuple_construct,
        "Expected TupleConstruct instruction in main function"
    );
}

#[test]
fn tuple_construct_for_swap_function() {
    let source = r"
fn swap(a: i64, b: i64) -> (i64, i64) {
    (b, a)
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let hir_output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        hir_output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir_output.diagnostics
    );

    let mir_pipeline = lower_hir_module(&hir_output.module);
    assert!(
        mir_pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir_pipeline.diagnostics
    );

    let swap_fn = mir_pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "swap")
        .expect("swap function not found in MIR");

    let has_tuple_construct = swap_fn.blocks.iter().any(|block| {
        block
            .instructions
            .iter()
            .any(|instr| matches!(instr, Instr::TupleConstruct { .. }))
    });

    assert!(
        has_tuple_construct,
        "Expected TupleConstruct instruction in swap function"
    );
}

#[test]
fn tuple_numeric_field_access_emits_tuple_field_loads() {
    let source = r"
fn first(t: (i64, bool)) -> i64 {
    t.0
}

fn second(t: (i64, bool)) -> bool {
    t.1
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let hir_output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        hir_output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir_output.diagnostics
    );

    let mir_pipeline = lower_hir_module(&hir_output.module);
    assert!(
        mir_pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir_pipeline.diagnostics
    );

    let first_fn = mir_pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "first")
        .expect("first function not found in MIR");
    let first_indexes: Vec<_> = first_fn
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| {
            if let Instr::TupleFieldLoad { field_index, .. } = instr {
                Some(*field_index)
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        first_indexes,
        vec![0],
        "t.0 must emit exactly one TupleFieldLoad with field_index 0"
    );

    let second_fn = mir_pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "second")
        .expect("second function not found in MIR");
    let second_indexes: Vec<_> = second_fn
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| {
            if let Instr::TupleFieldLoad { field_index, .. } = instr {
                Some(*field_index)
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        second_indexes,
        vec![1],
        "t.1 must emit exactly one TupleFieldLoad with field_index 1"
    );
}
