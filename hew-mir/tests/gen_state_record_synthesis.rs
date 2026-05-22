//! S3b structural-assertion tests for the generator state-record
//! synthesis + cross-yield liveness pass.
//!
//! These tests are the contract S4 (LLVM state-machine codegen) reads
//! when it lowers a generator. A regression here means downstream
//! codegen and drop elaboration are looking at malformed IR.
//!
//! Coverage per the closures S3b plan §6:
//! - `gen_state_record_synthesis` — layout shape, field ordering,
//!   deterministic state-local placement.
//! - `cross_yield_live_locals_lifted_to_state` — bookend Move
//!   discipline at every yield/resume boundary.
//!
//! The third validation candidate `gen_drop_in_state_shim_per_exit`
//! lands in S3b2 (drop-shim elaboration); S3b1 does not synthesise the
//! `__drop_in_state` function. See the plan §7 split point.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::gen_state::{LIVE_LOCAL_BASE, STATE_INIT_MASK_FIELD, STATE_TAG_FIELD};
use hew_mir::{GenStateLayout, Instr, IrPipeline, Place, RawMirFunction, Terminator};
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
    let hir = lower_program(&parsed.program, &tc_output, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        hir.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
    pipeline
}

fn find_gen_body<'a>(pipeline: &'a IrPipeline, owner: &str) -> &'a RawMirFunction {
    let prefix = format!("__hew_gen_body_{owner}_");
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.starts_with(&prefix))
        .unwrap_or_else(|| {
            panic!(
                "expected a gen-body function with prefix `{prefix}`; available raw MIR \
                 functions: {:?}",
                pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
            )
        })
}

fn find_layout<'a>(pipeline: &'a IrPipeline, function_name: &str) -> &'a GenStateLayout {
    pipeline
        .gen_state_layouts
        .iter()
        .find(|l| l.function_name == function_name)
        .unwrap_or_else(|| {
            panic!(
                "expected a GenStateLayout for `{function_name}`; available layouts: {:?}",
                pipeline
                    .gen_state_layouts
                    .iter()
                    .map(|l| &l.function_name)
                    .collect::<Vec<_>>()
            )
        })
}

/// Gate #1 of plan §6 S3b: `gen_state_record_synthesis`.
///
/// A simple `gen { yield 1; yield 2; yield 3; }` produces one
/// `GenStateLayout` for the body, registered in
/// `IrPipeline.gen_state_layouts`. The layout's `function_name` matches
/// the gen-body's `RawMirFunction.name`, the tag and init-mask field
/// ordinals are the contract field 0 and 1 respectively, and
/// `yield_count` matches the actual number of `Terminator::Yield`
/// terminators in the body.
///
/// The cross-yield-live set is empty for this fixture because no body
/// local is referenced after any yield. The layout must therefore have
/// zero entries in `live_locals` — proving the analysis correctly
/// distinguishes "live across yield" from "ever assigned in body".
#[test]
fn gen_state_record_synthesis() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 1; yield 2; yield 3; };
        }
        ",
    );

    let body = find_gen_body(&pipeline, "main");
    let layout = find_layout(&pipeline, &body.name);

    // The layout's name matches the gen-body function's name (the key
    // codegen uses to resolve a `Place::GenState` projection).
    assert_eq!(
        layout.function_name, body.name,
        "GenStateLayout.function_name must match the gen-body RawMirFunction.name"
    );

    // `yield_count` matches the actual number of yield terminators.
    let actual_yields: u32 = body
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Yield { .. }))
        .count()
        .try_into()
        .unwrap();
    assert_eq!(
        layout.yield_count, actual_yields,
        "GenStateLayout.yield_count {} must equal actual yield-terminator count {}",
        layout.yield_count, actual_yields
    );

    // No body local is read after any yield → no field lifts.
    assert!(
        layout.live_locals.is_empty(),
        "no body local is used across any yield in this fixture; live_locals should be \
         empty, got {:?}",
        layout.live_locals
    );

    // Field-ordinal contract: tag = 0, init_mask = 1, live_locals @ 2..
    assert_eq!(
        STATE_TAG_FIELD, 0,
        "state-tag field must be ordinal 0 (codegen + S3b2 + S4 all consume this)"
    );
    assert_eq!(
        STATE_INIT_MASK_FIELD, 1,
        "init-mask field must be ordinal 1 (S3b2 wires set/test against this)"
    );
    assert_eq!(
        LIVE_LOCAL_BASE, 2,
        "first cross-yield-live local must occupy field ordinal 2"
    );

    // The state-record local has been allocated at the END of the
    // body's locals vector. Find it: the last entry must be a Named
    // type with the synthesised state-record name.
    let last_ty = body.locals.last().unwrap_or_else(|| {
        panic!("body locals must be non-empty after S3b synthesis (state-record local missing)")
    });
    match last_ty {
        hew_types::ResolvedTy::Named { name, .. } => {
            assert!(
                name.starts_with("Gen$state$"),
                "state-record local's type must be Named(\"Gen$state$...\"); got {name}"
            );
            assert!(
                name.contains(&body.name),
                "state-record local's type name {name:?} must reference the gen-body's \
                 name {:?}",
                body.name
            );
        }
        other => panic!("expected Named state-record type for last body local; got {other:?}"),
    }
}

/// Gate #2 of plan §6 S3b: `cross_yield_live_locals_lifted_to_state`.
///
/// Pins the bookend Move discipline. The fixture binds a local
/// (`acc`) before the first yield and reads it after each yield
/// boundary, so the local must be lifted into the state record:
///
///   - the `GenStateLayout` carries exactly one `live_locals` entry
///     covering `acc`,
///   - each yield block ends with a checkpoint
///     `Move { dest: GenState{..., field: 2}, src: Local(N_acc) }`
///     IMMEDIATELY BEFORE the `Terminator::Yield`,
///   - each resume block starts with a reload
///     `Move { dest: Local(N_acc), src: GenState{..., field: 2} }`
///     BEFORE any other instruction in that block.
///
/// The substrate S4 reads is precisely this discipline: without it,
/// codegen would lose the carried-across value at every suspension.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "single-assertion test keeps the checkpoint/reload discipline contract \
              and its over-lift counter-case in one body so a regression of either \
              half surfaces in one failure rather than two separate tests sharing \
              fixture setup"
)]
fn cross_yield_live_locals_lifted_to_state() {
    // Three yields all reading `acc` make `acc` live at the resume of
    // yield #1 and yield #2 (the resumes feed forward into another
    // `yield acc`). The resume of yield #3 is the Return block, which
    // does NOT read `acc` — so the third yield correctly carries NO
    // checkpoint (the value is dead after the last suspension).
    //
    // This shape exercises both halves of the discipline:
    //   - yields #1 and #2 → checkpoint + reload pair fires,
    //   - yield #3        → no bookend Moves emitted (proves the
    //                       analysis does not over-lift).
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen {
                let acc = 1;
                yield acc;
                yield acc;
                yield acc;
            };
        }
        ",
    );

    let body = find_gen_body(&pipeline, "main");
    let layout = find_layout(&pipeline, &body.name);

    // Exactly one local lifted (the `acc` binding).
    assert_eq!(
        layout.live_locals.len(),
        1,
        "exactly one cross-yield-live local expected; got {:?}",
        layout.live_locals
    );

    let lifted = &layout.live_locals[0];
    let lifted_local = lifted.original_local;
    let lifted_field = LIVE_LOCAL_BASE;

    // Resolve the state-local id (the last entry in the locals vec
    // after synthesis — see test #1).
    let state_local: u32 = u32::try_from(body.locals.len() - 1).unwrap();

    let expected_checkpoint = Instr::Move {
        dest: Place::GenState {
            local: state_local,
            field: lifted_field,
        },
        src: Place::Local(lifted_local),
    };
    let expected_reload = Instr::Move {
        dest: Place::Local(lifted_local),
        src: Place::GenState {
            local: state_local,
            field: lifted_field,
        },
    };

    // Collect yield blocks in source order (yield #1, #2, #3).
    let mut yield_blocks: Vec<&hew_mir::BasicBlock> = body
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Yield { .. }))
        .collect();
    yield_blocks.sort_by_key(|b| b.id);
    assert_eq!(
        yield_blocks.len(),
        3,
        "expected three yields in the fixture; got {}",
        yield_blocks.len()
    );

    // The FIRST TWO yields' resume blocks read `acc`, so each must emit
    // the checkpoint Move immediately before its `Terminator::Yield`
    // and the matching reload Move at the top of its resume block.
    for (idx, yb) in yield_blocks.iter().take(2).enumerate() {
        let last_instr = yb.instructions.last().unwrap_or_else(|| {
            panic!(
                "yield #{} (block {}) must contain at least one instruction (the checkpoint \
                 Move)",
                idx + 1,
                yb.id
            )
        });
        assert_eq!(
            last_instr,
            &expected_checkpoint,
            "yield #{} (block {})'s last instruction must be the checkpoint Move for the \
             lifted local. Expected {:?}, got {:?}. Full block instructions: {:?}",
            idx + 1,
            yb.id,
            expected_checkpoint,
            last_instr,
            yb.instructions
        );

        // Matching reload at the resume block's top.
        let Terminator::Yield { next, .. } = yb.terminator else {
            unreachable!("yield_blocks filter only retains Terminator::Yield arms")
        };
        let resume = body
            .blocks
            .iter()
            .find(|b| b.id == next)
            .unwrap_or_else(|| panic!("resume block {next} must exist"));
        let first_instr = resume.instructions.first().unwrap_or_else(|| {
            panic!(
                "resume block {next} (resume of yield #{}) must begin with the reload Move; \
                 got empty instruction vector",
                idx + 1
            )
        });
        assert_eq!(
            first_instr,
            &expected_reload,
            "resume block {next} (resume of yield #{})'s first instruction must reload the \
             lifted local. Expected {:?}, got {:?}. Full block instructions: {:?}",
            idx + 1,
            expected_reload,
            first_instr,
            resume.instructions
        );
    }

    // The THIRD yield's resume block does NOT read `acc` — the analysis
    // must NOT emit a checkpoint Move at the third yield site, nor a
    // reload at its resume block. (Over-lifting would burn memory
    // bandwidth on values codegen would immediately discard.)
    let third = yield_blocks[2];
    assert!(
        !third.instructions.contains(&expected_checkpoint),
        "yield #3 (block {}) must NOT emit a checkpoint for a local its resume does not read; \
         instructions: {:?}",
        third.id,
        third.instructions
    );

    let Terminator::Yield {
        next: third_resume, ..
    } = third.terminator
    else {
        unreachable!("yield_blocks filter only retains Terminator::Yield arms")
    };
    let resume3 = body
        .blocks
        .iter()
        .find(|b| b.id == third_resume)
        .unwrap_or_else(|| panic!("resume block {third_resume} must exist"));
    assert!(
        !resume3.instructions.contains(&expected_reload),
        "resume block {} (resume of yield #3) must NOT begin with a reload for an unused \
         lifted local; instructions: {:?}",
        third_resume,
        resume3.instructions
    );
}
