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
use hew_mir::{
    GenStateDropTable, GenStateLayout, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
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

fn find_raw<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| {
            panic!(
                "expected RawMirFunction named {name:?}; available: {:?}",
                pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
            )
        })
}

/// Gate #3 of plan §6 S3b: `gen_drop_in_state_shim_per_exit`.
///
/// Validates the two halves S3b2 owns:
///
/// 1. **Init-mask discipline** — at every yield bookend the synthesis
///    pass writes the per-site mask to `Place::GenState { local:
///    state_local, field: STATE_INIT_MASK_FIELD (= 1) }`. The
///    discipline is symmetric: each yield emits a `set` (mask = site
///    bit pattern) and each resume emits a `clear` (mask = 0). Both
///    writes flow through a shared u64 scratch local because MIR has
///    no Const-to-GenState direct instruction; the scratch is unique
///    per body and is the only U64-typed local introduced by
///    synthesis.
///
/// 2. **`__drop_in_state` shim** — synthesis emits a per-gen-body
///    `RawMirFunction` named `__hew_gen_drop_in_state_{owner}_{id}` and
///    populates `GenStateLayout.drop_shim_name` to match. The shim's
///    structured drop manifest lives in `GenStateLayout.drop_tables`:
///    one entry per state tag, where state 0 (initial) and state
///    `yield_count + 1` (Ended) hold zero lifted locals, and the
///    intermediate suspension states list exactly the per-site
///    cross-yield-live set in reverse-init drop order.
///
/// The fixture mixes two yields with one lifted local and one yield
/// with NO lifted local across its resume boundary, so the intermediate
/// drop tables cover both shapes (non-empty + empty).
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "single-assertion test keeps the init-mask discipline and \
              the per-state drop-table manifest contract in one body \
              so a regression of either half surfaces in one failure \
              rather than two separate tests sharing the gen-fixture \
              setup; mirrors the policy in \
              `cross_yield_live_locals_lifted_to_state` above."
)]
fn gen_drop_in_state_shim_per_exit() {
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

    // ─── Half A: init-mask discipline ────────────────────────────────
    //
    // STATE_INIT_MASK_FIELD is the ordinal contract; pin it here so
    // any future shuffle of field layout breaks this test loudly.
    assert_eq!(
        STATE_INIT_MASK_FIELD, 1,
        "init-mask must live at field ordinal 1; codegen and S3b2 \
         synthesis are both pinned to this"
    );

    let state_local: u32 = u32::try_from(body.locals.len() - 1).unwrap();
    let mask_field_place = Place::GenState {
        local: state_local,
        field: STATE_INIT_MASK_FIELD,
    };

    // Find the shared U64 mask scratch local: synthesis allocates
    // exactly one U64 local in the body's locals vector. It cannot be
    // the parameter (which is the gen-state record itself) nor any
    // pre-existing body local (the fixture uses I64 / Unit).
    let mask_scratch_locals: Vec<u32> = body
        .locals
        .iter()
        .enumerate()
        .filter(|(_, ty)| matches!(ty, hew_types::ResolvedTy::U64))
        .map(|(i, _)| u32::try_from(i).unwrap())
        .collect();
    assert_eq!(
        mask_scratch_locals.len(),
        1,
        "exactly one U64 mask-scratch local must be allocated by S3b2; \
         got {mask_scratch_locals:?}"
    );
    let mask_scratch = mask_scratch_locals[0];

    // Every yield block writes the mask via `ConstI64 mask_scratch =
    // <site bits>` followed by `Move mask_field_place = mask_scratch`.
    // We assert structurally: each yield block must contain a
    // mask-Move whose dest is the GenState init-mask field and whose
    // src is `Place::Local(mask_scratch)`, IMMEDIATELY preceded by a
    // `ConstI64` to the same scratch.
    let yield_blocks: Vec<&hew_mir::BasicBlock> = body
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Yield { .. }))
        .collect();
    assert!(
        !yield_blocks.is_empty(),
        "fixture must contain at least one Terminator::Yield; got 0"
    );

    for yb in &yield_blocks {
        let mask_pair = yb.instructions.windows(2).find(|w| {
            matches!(
                w,
                [
                    Instr::ConstI64 { dest, .. },
                    Instr::Move { dest: mdest, src }
                ]
                if *dest == Place::Local(mask_scratch)
                    && *mdest == mask_field_place
                    && *src == Place::Local(mask_scratch)
            )
        });
        assert!(
            mask_pair.is_some(),
            "yield block {}: missing init-mask set (ConstI64 → \
             Move into field {STATE_INIT_MASK_FIELD}). Instructions: {:?}",
            yb.id,
            yb.instructions
        );

        // Resume block must clear the mask: ConstI64 mask_scratch = 0,
        // Move mask_field_place = mask_scratch.
        let Terminator::Yield { next, .. } = yb.terminator else {
            unreachable!("filtered to Yield terminators")
        };
        let resume = body
            .blocks
            .iter()
            .find(|b| b.id == next)
            .unwrap_or_else(|| panic!("resume block {next} not found"));
        let clear_pair = resume.instructions.windows(2).find(|w| {
            matches!(
                w,
                [
                    Instr::ConstI64 { dest, value: 0 },
                    Instr::Move { dest: mdest, src }
                ]
                if *dest == Place::Local(mask_scratch)
                    && *mdest == mask_field_place
                    && *src == Place::Local(mask_scratch)
            )
        });
        assert!(
            clear_pair.is_some(),
            "resume block {next} (after yield block {}): missing \
             init-mask clear (ConstI64 0 → Move into field \
             {STATE_INIT_MASK_FIELD}). Instructions: {:?}",
            yb.id,
            resume.instructions
        );
    }

    // ─── Half B: drop shim function + per-state drop tables ─────────
    //
    // The shim name must follow the `__hew_gen_drop_in_state_*`
    // convention and be the same string the layout carries. A
    // RawMirFunction with that name must exist in the pipeline.
    assert!(
        layout
            .drop_shim_name
            .starts_with("__hew_gen_drop_in_state_"),
        "drop_shim_name must use the `__hew_gen_drop_in_state_` \
         prefix so codegen + diagnostics can recognise the shim; got \
         {:?}",
        layout.drop_shim_name
    );
    let shim = find_raw(&pipeline, &layout.drop_shim_name);

    // Shim's parameter is the state-record carrier (one param, one
    // local in the prefix). Body is a single Trap block today; S4
    // regenerates the cascade-on-tag dispatch.
    assert_eq!(
        shim.params.len(),
        1,
        "shim must take exactly one parameter (the state-record \
         carrier); got {:?}",
        shim.params
    );
    let single_block = shim
        .blocks
        .first()
        .unwrap_or_else(|| panic!("shim {:?} must contain at least one block", shim.name));
    assert!(
        matches!(single_block.terminator, Terminator::Trap { .. }),
        "shim's entry block must terminate with Trap (the fail-closed \
         placeholder until S4 wires the cascade); got {:?}",
        single_block.terminator
    );

    // drop_tables shape: one entry per state tag, total = yield_count + 2.
    let yield_count = layout.yield_count;
    assert_eq!(
        u32::try_from(layout.drop_tables.len()).expect("drop_tables length fits in u32"),
        yield_count + 2,
        "drop_tables must have one entry per state tag (initial + \
         per-yield-suspension + Ended = yield_count + 2 = {}); got \
         {} entries",
        yield_count + 2,
        layout.drop_tables.len()
    );

    // Each entry's state_tag must equal its index (self-describing
    // invariant codegen relies on).
    for (idx, table) in layout.drop_tables.iter().enumerate() {
        let idx_u32 = u32::try_from(idx).unwrap();
        assert_eq!(
            table.state_tag, idx_u32,
            "drop_tables[{idx}].state_tag must equal its index; got \
             {} (full table: {:?})",
            table.state_tag, table
        );
    }

    // The initial state (tag 0) holds NO lifted locals (nothing was
    // checkpointed yet).
    assert!(
        layout.drop_tables[0].fields_in_drop_order.is_empty(),
        "drop_tables[0] (initial state) must hold no lifted locals; \
         got {:?}",
        layout.drop_tables[0].fields_in_drop_order
    );

    // The Ended state (tag yield_count + 1) also holds NO lifted
    // locals (every checkpoint reloaded into the body before the
    // generator returned).
    let ended_idx = (yield_count + 1) as usize;
    assert!(
        layout.drop_tables[ended_idx]
            .fields_in_drop_order
            .is_empty(),
        "drop_tables[{ended_idx}] (Ended state) must hold no lifted \
         locals; got {:?}",
        layout.drop_tables[ended_idx].fields_in_drop_order
    );

    // The two intermediate suspension states (tags 1 and 2) each
    // correspond to a yield whose resume reads `acc`, so they must
    // list the lifted index 0 (the only live_locals entry) in
    // reverse-init order — which for a single-element list is just
    // `[0]`.
    for tag in 1..=2u32 {
        let entry: &GenStateDropTable = &layout.drop_tables[tag as usize];
        assert_eq!(
            entry.fields_in_drop_order,
            vec![0u32],
            "drop_tables[{tag}] (suspension after yield #{tag}, whose \
             resume reads `acc`) must list lifted index 0; got {:?}",
            entry.fields_in_drop_order
        );
    }

    // The third intermediate state (tag 3) corresponds to yield #3
    // whose resume does NOT read `acc`. The analysis must not
    // checkpoint at yield #3, therefore that state's drop table must
    // be empty. (This is the symmetric counter-case to test #2's
    // over-lift assertion: no over-drop either.)
    assert!(
        layout.drop_tables[3].fields_in_drop_order.is_empty(),
        "drop_tables[3] (suspension after yield #3, whose resume does \
         not read `acc`) must be empty (no over-drop). Got {:?}",
        layout.drop_tables[3].fields_in_drop_order
    );
}
