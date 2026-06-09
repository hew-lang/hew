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

/// Defect 1 (hard double-free), CancellationToken-in-tuple variant: a token
/// extracted out of a live tuple via `let tok = pair.0` must release its ref
/// EXACTLY ONCE. The extracted binding's standalone `hew_cancel_token_release`
/// is the sole owner; the tuple's `TupleInPlace` member-drop MUST be excluded so
/// it does not release the same aliased token a second time (a refcount
/// underflow / premature free). `CancellationToken` is the same non-refcounted-
/// at-the-drop-spine heap-owning class as `Generator`, so it is covered by the
/// same tuple sole-owner exclusion.
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
    let all_drops: Vec<_> = run
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .collect();
    // The tuple must NOT earn a TupleInPlace member-drop: the extracted `tok`
    // owns the token now, so a tuple member-drop would double-release it.
    assert!(
        all_drops
            .iter()
            .all(|drop| drop.kind != DropKind::TupleInPlace),
        "the (CancellationToken, i64) tuple must be EXCLUDED from the \
         TupleInPlace spine once its token element is extracted into a \
         standalone owned binding; drops={all_drops:#?}"
    );
    // Exactly one token release fires (the extracted `tok`'s standalone drop).
    let releases = all_drops
        .iter()
        .filter(|drop| drop.drop_fn.as_deref() == Some("hew_cancel_token_release"))
        .count();
    assert_eq!(
        releases, 1,
        "the extracted token must be released exactly once (no tuple \
         member-drop double-release); drops={all_drops:#?}"
    );
}
