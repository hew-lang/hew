//! Risk-1 CFG block-target guard for `for`-range `continue`.
//!
//! The for-range lowering routes `continue` to a dedicated increment block
//! (`inc_bb`) that runs `counter = counter + 1` and re-checks the loop bound,
//! NOT straight back to the bounds-check header. Threading `continue` to the
//! header would skip the increment, pin the loop variable at the skipped
//! value, and spin forever.
//!
//! These tests assert the structural CFG edge directly so the invariant is
//! locked at MIR-construction time, independent of the runtime acceptance
//! fixtures (`tests/vertical-slice/accept/continue_for.hew`).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{Instr, IntArithOp, IrPipeline, Terminator};
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
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    hew_mir::lower_hir_module(&hir.module)
}

/// Count blocks in `main` whose terminator is `Goto { target }`.
fn count_goto_targets(pipeline: &IrPipeline, target: u32) -> usize {
    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main MIR");
    main.blocks
        .iter()
        .filter(|block| matches!(block.terminator, Terminator::Goto { target: t } if t == target))
        .count()
}

#[test]
fn for_range_continue_targets_increment_block_not_header() {
    // `continue` skips the `i == 2` iteration. If lowering is correct, the
    // skipped iteration still advances the counter through `inc_bb`.
    let pipeline = lower_checked(
        r"
        fn main() -> i64 {
            var sum: i64 = 0;
            for i in 0 .. 5 {
                if i == 2 {
                    continue;
                }
                sum = sum + i;
            }
            sum
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main MIR");

    // The increment block is the unique block performing the counter
    // self-increment: `IntArithChecked { op: Add, dest, lhs, .. }` with
    // `dest == lhs` (`counter = counter + 1`). Its overflow `Branch`'s
    // else-target is the loop header.
    let inc_block = main
        .blocks
        .iter()
        .find(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::IntArithChecked {
                        op: IntArithOp::Add,
                        dest,
                        lhs,
                        ..
                    } if dest == lhs
                )
            })
        })
        .expect("for-range must emit a counter self-increment block (inc_bb)");
    let inc_bb = inc_block.id;

    // inc_bb's terminator is the overflow `Branch`; its else-target is the
    // loop header (the bounds-check block running `IntCmp SignedLess`).
    let header_bb = match inc_block.terminator {
        Terminator::Branch { else_target, .. } => else_target,
        ref other => panic!("inc_bb must terminate in an overflow Branch, got {other:?}"),
    };
    let header_block = main
        .blocks
        .iter()
        .find(|block| block.id == header_bb)
        .expect("header block must exist");
    assert!(
        header_block
            .instructions
            .iter()
            .any(|instr| matches!(instr, Instr::IntCmp { .. })),
        "inc_bb's else-target must be the bounds-check header (an IntCmp block)"
    );
    assert_ne!(inc_bb, header_bb, "inc_bb and header_bb must be distinct");

    // Risk-1 guard. Two distinct `Goto { inc_bb }` edges must exist: the
    // body's normal fall-through AND the `continue` site. If `continue`
    // wrongly targeted the header, only the fall-through edge would remain.
    let goto_to_inc = count_goto_targets(&pipeline, inc_bb);
    assert_eq!(
        goto_to_inc, 2,
        "expected 2 Goto edges into inc_bb (body fall-through + continue), got {goto_to_inc}"
    );

    // Complementary guard: the only `Goto` into the header is the loop entry
    // edge. `continue` must NOT add a header `Goto` (the back-edge from inc_bb
    // is a `Branch`, not a `Goto`, so it is not counted here).
    let goto_to_header = count_goto_targets(&pipeline, header_bb);
    assert_eq!(
        goto_to_header, 1,
        "expected exactly 1 Goto into the header (loop entry); continue must not \
         add a header edge, got {goto_to_header}"
    );
}

#[test]
fn while_continue_targets_header_block() {
    // For `while`, `continue` correctly targets the condition/header block —
    // there is no separate increment block to route around.
    let pipeline = lower_checked(
        r"
        fn main() -> i64 {
            var i: i64 = 0;
            var sum: i64 = 0;
            while i < 5 {
                i = i + 1;
                if i == 3 {
                    continue;
                }
                sum = sum + i;
            }
            sum
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main MIR");

    // The header is the block whose terminator branches on the loop condition
    // (`IntCmp SignedLess` then a `Branch` to body/exit).
    let header_block = main
        .blocks
        .iter()
        .find(|block| {
            matches!(block.terminator, Terminator::Branch { .. })
                && block
                    .instructions
                    .iter()
                    .any(|instr| matches!(instr, Instr::IntCmp { .. }))
        })
        .expect("while must emit a condition/header block");
    let header_bb = header_block.id;

    // The loop-entry `Goto` and the back-edges (normal fall-through + the
    // `continue` site) all land on the header. Observing >= 2 `Goto { header }`
    // edges proves `continue` joins the header rather than escaping the loop.
    let goto_to_header = count_goto_targets(&pipeline, header_bb);
    assert!(
        goto_to_header >= 2,
        "expected >= 2 Goto edges into the while header (entry + back-edges \
         incl. continue), got {goto_to_header}"
    );
}
