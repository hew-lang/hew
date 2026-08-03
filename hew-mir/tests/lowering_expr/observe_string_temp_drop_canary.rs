//! Observe string-result ownership canary.
//!
//! `observe.scrape` / `observe.series` lower through `CallRuntimeAbi`, unlike
//! ordinary declared externs that reach `Terminator::Call`. Their ownership
//! still comes only from the declared FFI contract: a measured fresh result,
//! the matching shallow `hew_string_drop`, and transferred retention. This
//! exercises each string position that may otherwise lose that authority.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, ExitPath, Instr, IrPipeline, Place};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

const SOURCE: &str = r#"
extern "C" {
    fn hew_observe_scrape() -> string;
    fn hew_observe_series() -> string;
}

fn discarded() {
    unsafe { hew_observe_scrape() };
    unsafe { hew_observe_series() };
}

fn nested() -> i64 {
    unsafe { hew_observe_scrape() }.len() + unsafe { hew_observe_series() }.len()
}

fn bound() -> i64 {
    let scrape = unsafe { hew_observe_scrape() };
    let series = unsafe { hew_observe_series() };
    scrape.len() + series.len()
}

fn tail_return() -> string {
    unsafe { hew_observe_scrape() }
}
"#;

fn pipeline() -> IrPipeline {
    let parsed = hew_parser::parse(SOURCE);
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
    lower_hir_module(&output.module)
}

fn raw<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("missing raw MIR for {name}"))
}

fn observed_result_dests(pipeline: &IrPipeline, name: &str) -> usize {
    raw(pipeline, name)
        .blocks
        .iter()
        .map(|block| {
            let inline = block
                .instructions
                .iter()
                .filter(|instruction| {
                    matches!(
                        instruction,
                        Instr::CallRuntimeAbi(call)
                            if matches!(call.symbol(), "hew_observe_scrape" | "hew_observe_series")
                                && matches!(call.dest(), Some(Place::Local(_)))
                    )
                })
                .count();
            let terminal = usize::from(matches!(
                &block.terminator,
                hew_mir::Terminator::Call {
                    callee,
                    dest: Some(Place::Local(_)),
                    ..
                } if matches!(callee.as_str(), "hew_observe_scrape" | "hew_observe_series")
            ));
            inline + terminal
        })
        .sum()
}

fn inline_string_drops(pipeline: &IrPipeline, name: &str) -> usize {
    raw(pipeline, name)
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::Drop {
                    ty: ResolvedTy::String,
                    drop_fn: Some(spec),
                    ..
                } if *spec == hew_mir::DropFnSpec::Release("hew_string_drop")
            )
        })
        .count()
}

fn return_exit_string_drops(pipeline: &IrPipeline, name: &str) -> usize {
    pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("missing elaborated MIR for {name}"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .map(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|drop| {
                    matches!(
                        &drop.kind,
                        DropKind::CowHeap { release }
                            if release.release_symbol() == "hew_string_drop"
                    )
                })
                .count()
        })
        .max()
        .unwrap_or(0)
}

#[test]
fn audited_observe_results_keep_destinations_in_every_position() {
    let pipeline = pipeline();
    assert!(
        pipeline.diagnostics.is_empty(),
        "observe ownership fixture must lower cleanly: {:#?}",
        pipeline.diagnostics
    );

    // Even discarded calls need a real local: it is the fresh-owned temporary
    // that the audited release machinery consumes on the continuation.
    assert_eq!(observed_result_dests(&pipeline, "discarded"), 2);
    assert_eq!(observed_result_dests(&pipeline, "nested"), 2);
    assert_eq!(observed_result_dests(&pipeline, "bound"), 2);
    assert_eq!(observed_result_dests(&pipeline, "tail_return"), 1);
}

#[test]
fn audited_observe_results_have_one_noncompeting_drop_authority() {
    let pipeline = pipeline();

    // A discarded whole result is materialised through the transparent unsafe
    // block move, then released by the shared fresh-temp collector. It never
    // gets a competing scope owner.
    assert_eq!(inline_string_drops(&pipeline, "discarded"), 2);
    assert_eq!(return_exit_string_drops(&pipeline, "discarded"), 0);

    // A method receiver is an audited borrowing use. The shared temporary
    // collector follows the block-result transfer, then balances each fresh
    // value immediately after that borrow rather than minting a scope owner.
    assert_eq!(inline_string_drops(&pipeline, "nested"), 2);
    assert_eq!(return_exit_string_drops(&pipeline, "nested"), 0);

    // A binding moves the same one owner to its scope-exit plan instead; it
    // must not also get an inline release.
    assert_eq!(inline_string_drops(&pipeline, "bound"), 0);
    assert_eq!(return_exit_string_drops(&pipeline, "bound"), 2);

    // Tail return transfers the sole owner to the caller. No local cleanup in
    // this frame is valid, and exactly that absence proves the authority did
    // not widen into an unconditional observe drop.
    assert_eq!(inline_string_drops(&pipeline, "tail_return"), 0);
    assert_eq!(return_exit_string_drops(&pipeline, "tail_return"), 0);
}
