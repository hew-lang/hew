use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, Instr, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

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
    lower_hir_module(&output.module)
}

#[test]
fn cancellation_token_intrinsic_and_release_drop_reach_mir() {
    let pipeline = pipeline_with_tc(
        r"
        fn observe(token: CancellationToken) -> bool {
            let t: CancellationToken = token;
            return t.is_cancelled();
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let func = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "observe")
        .expect("observe MIR should exist");
    assert!(
        func.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(instr, Instr::CancellationTokenIsCancelled { .. })),
        "is_cancelled() must lower to the MIR token intrinsic: {func:#?}"
    );

    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|func| func.name == "observe")
        .expect("observe elaborated MIR should exist");
    let drops: Vec<_> = elaborated
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| drop.ty == ResolvedTy::CancellationToken)
        .collect();
    assert!(
        drops.iter().any(|drop| {
            drop.kind == DropKind::Resource
                && drop.drop_fn.as_deref() == Some("hew_cancel_token_release")
        }),
        "CancellationToken locals must release a ref on drop; drops={drops:#?}"
    );
}
