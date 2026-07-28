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
                && drop.drop_fn
                    == Some(hew_mir::DropFnSpec::Runtime(
                        hew_types::runtime_call::RuntimeDropDescriptor::CancellationTokenRelease,
                    ))
        }),
        "CancellationToken locals must release a ref on drop; drops={drops:#?}"
    );
}

/// CancellationToken-in-tuple transfer: extracting `pair.0` writes down a
/// root-relative neutralization before the standalone binding becomes the sole
/// owner. The token binding and tuple may then both keep their drops: the
/// tuple's cleared token slot is a null-safe no-op, while any owned siblings
/// remain covered by its structural drop.
#[test]
fn cancellation_token_extracted_from_tuple_releases_exactly_once() {
    let pipeline = pipeline_with_tc(
        r"
        fn make(token: CancellationToken) -> (CancellationToken, i64) {
            return (token, 0);
        }
        fn run(token: CancellationToken) -> bool {
            let pair = make(token);
            let tok: CancellationToken = pair.0;
            return tok.is_cancelled();
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let run = pipeline
        .elaborated_mir
        .iter()
        .find(|func| func.name == "run")
        .expect("run elaborated MIR should exist");
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|func| func.name == "run")
        .expect("run checked MIR should exist");
    assert!(
        checked
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(
                instr,
                Instr::AggregateProjectionNeutralize { fields, .. }
                    if fields.as_slice() == [0]
            )),
        "the tuple's token slot must be cleared before its ownership transfers: {checked:#?}"
    );
    let all_drops: Vec<_> = run
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .collect();
    // The transfer marker makes the two drops disjoint: `tok` releases the
    // token, while TupleInPlace sees a null token slot and retains coverage for
    // every unmoved tuple sibling.
    assert!(
        all_drops
            .iter()
            .any(|drop| drop.kind == DropKind::TupleInPlace),
        "the neutralized (CancellationToken, i64) tuple must keep its \
         TupleInPlace drop for unmoved sibling coverage; drops={all_drops:#?}"
    );
    // Exactly one standalone token release fires (the extracted `tok`'s drop);
    // the tuple's generated in-place helper receives a null token slot.
    let releases = all_drops
        .iter()
        .filter(|drop| {
            drop.drop_fn
                == Some(hew_mir::DropFnSpec::Runtime(
                    hew_types::runtime_call::RuntimeDropDescriptor::CancellationTokenRelease,
                ))
        })
        .count();
    assert_eq!(
        releases, 1,
        "the extracted token must have exactly one standalone release; \
         drops={all_drops:#?}"
    );
}
