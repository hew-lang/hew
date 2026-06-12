//! Producer-bridge tests for MIR `Terminator::Yield` lowering.
//!
//! S3a scope: `HirExprKind::GenBlock` and `HirExprKind::Yield` lower to
//! `Terminator::Yield { value, next }` inside a synthetic gen-body function
//! registered in the `IrPipeline`. The enclosing function receives a
//! `Place::Local` typed as `Generator<Y, R>`.
//!
//! These tests are the structural contract that S3b (cross-yield liveness
//! synthesis) and S4 (LLVM state-machine codegen) consume. A regression here
//! means the downstream slices are looking at malformed CFG.
//!
//! LESSONS row `end-to-end-before-layer-thickening` (P0): every cross-stage
//! producer-emit must carry a structural assertion test before the consumer
//! ships. This file is that gate for the generator MIR lane.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{IrPipeline, RawMirFunction, Terminator};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_checked(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        hir.diagnostics
    );
    hew_mir::lower_hir_module(&hir.module)
}

fn find_gen_body<'a>(pipeline: &'a IrPipeline, owner: &str) -> &'a RawMirFunction {
    let prefix = format!("__hew_gen_body_{owner}_");
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.starts_with(&prefix))
        .unwrap_or_else(|| {
            panic!(
                "expected a gen-body function with prefix `{prefix}`; \
                 available raw MIR functions: {:?}",
                pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
            )
        })
}

/// For each `Terminator::Yield` in the function, return `(block_id, next_block_id)`.
fn yield_sites(func: &RawMirFunction) -> Vec<(u32, u32)> {
    func.blocks
        .iter()
        .filter_map(|b| {
            if let Terminator::Yield { next, .. } = &b.terminator {
                Some((b.id, *next))
            } else {
                None
            }
        })
        .collect()
}

/// Positive case: a gen block with two yield expressions produces a gen-body
/// function containing exactly two `Terminator::Yield` terminators.
///
/// CFG shape expected:
///   block 0: (stmts) → Yield { value: Local(N), next: 1 }
///   block 1: (stmts) → Yield { value: Local(M), next: 2 }
///   block 2: (stmts) → Return
///
/// This is the structural contract S3b reads when it traverses the CFG to
/// compute cross-yield live sets.
#[test]
fn gen_block_with_two_yields_emits_two_terminator_yield() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 1; yield 2; };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let body = find_gen_body(&pipeline, "main");
    let sites = yield_sites(body);
    assert_eq!(
        sites.len(),
        2,
        "expected 2 Terminator::Yield in gen body; got {sites:?}"
    );

    // The resume blocks must be distinct and must exist in the block list.
    let block_ids: Vec<u32> = body.blocks.iter().map(|b| b.id).collect();
    for (yield_block, resume_block) in &sites {
        assert!(
            block_ids.contains(resume_block),
            "resume block {resume_block} from yield at block {yield_block} \
             is not in the gen-body's block list: {block_ids:?}"
        );
    }

    // The two resume blocks must be different.
    assert_ne!(
        sites[0].1, sites[1].1,
        "both yields resume to the same block — \
         resume-block allocation must be monotone"
    );

    // The last block must be Return (generator body ends when exhausted).
    let last_terminator = &body
        .blocks
        .iter()
        .max_by_key(|b| b.id)
        .expect("gen body must have at least one block")
        .terminator;
    assert!(
        matches!(last_terminator, Terminator::Return),
        "gen-body's last block must terminate with Return; got {last_terminator:?}"
    );
}

/// Structural assertion: each `Terminator::Yield` carries a `value` Place
/// that is a `Place::Local(N)` bound to the lowered yield value.
/// This test also confirms that the gen-body function is registered in the
/// `IrPipeline.raw_mir` list (i.e. `generated_functions` plumbing works).
#[test]
fn terminator_yield_emitted_at_yield_site_with_local_value() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 42; };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let body = find_gen_body(&pipeline, "main");

    // Exactly one Yield terminator.
    let sites = yield_sites(body);
    assert_eq!(
        sites.len(),
        1,
        "expected 1 Terminator::Yield; got {sites:?}"
    );

    // The yield value must be a Place::Local (not ReturnSlot or a handle kind).
    let yield_block_id = sites[0].0;
    let yield_block = body
        .blocks
        .iter()
        .find(|b| b.id == yield_block_id)
        .expect("yield block must exist");
    let Terminator::Yield { value, next } = &yield_block.terminator else {
        panic!("expected Terminator::Yield");
    };
    assert!(
        matches!(value, hew_mir::Place::Local(_)),
        "Yield value must be a Place::Local; got {value:?}"
    );

    // The resume block must exist.
    let block_ids: Vec<u32> = body.blocks.iter().map(|b| b.id).collect();
    assert!(
        block_ids.contains(next),
        "resume block {next} is not in the block list: {block_ids:?}"
    );

    // Gen body is registered alongside `main` in the raw_mir list.
    assert!(
        pipeline.raw_mir.len() >= 2,
        "expected at least main + gen-body in raw_mir; got {} functions: {:?}",
        pipeline.raw_mir.len(),
        pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
    );
}

/// The gen-body function is registered in `IrPipeline.raw_mir` alongside
/// the enclosing function. This test exercises the `generated_functions`
/// plumbing that surfaces nested functions through the pipeline.
///
/// Note on the `generator_borrow_across_yield_detected` test from plan §6:
/// that validation candidate requires a live borrow at a yield site. The
/// v0.5 spine has no MIR borrow-op construction surface (see `lower.rs`
/// line ~326 comment: "Aliasing, `GeneratorBorrowAcrossYield`… have no
/// construction surface"). `MirCheck::GeneratorBorrowAcrossYield` is
/// declared in model.rs:1993 but no pass constructs it today. That test
/// will land alongside the borrow-op surface in a later slice; this note
/// documents the gap so S3a is not mis-counted as missing a required gate.
#[test]
fn gen_body_function_registered_in_raw_mir_pipeline() {
    let parsed = hew_parser::parse(
        r"
        fn main() {
            let g = gen { yield 1; };
        }
        ",
    );
    // Lower via the typecheck-free path to confirm the plumbing is
    // independent of whether the checker ran.
    let hir = hew_hir::lower_program(
        &parsed.program,
        &hew_types::TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&hir.module);
    let gen_body = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.contains("__hew_gen_body_"));
    assert!(
        gen_body.is_some(),
        "gen-body function must be registered in raw_mir; found: {:?}",
        pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
    );
}

/// CFG completeness: a gen block with multiple yields produces blocks in
/// a connected chain. Every resume block must be reachable from the entry
/// block by following terminator edges.
#[test]
fn gen_block_body_cfg_is_connected() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 1; yield 2; yield 3; };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let body = find_gen_body(&pipeline, "main");
    let sites = yield_sites(body);
    assert_eq!(sites.len(), 3, "expected 3 yields");

    // Verify that the resume block for each yield is the source block of
    // the following yield (i.e. the chain is: 0→yield→1→yield→2→yield→3→return).
    // Block ids are monotone by construction so we can sort by yield_block_id.
    let mut sorted = sites.clone();
    sorted.sort_by_key(|(bid, _)| *bid);

    for window in sorted.windows(2) {
        let (_, resume_of_first) = window[0];
        let (source_of_second, _) = window[1];
        assert_eq!(
            resume_of_first, source_of_second,
            "resume block of first yield ({resume_of_first}) must be the \
             source block of the second yield ({source_of_second})"
        );
    }
}
