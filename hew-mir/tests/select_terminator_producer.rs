//! Producer-bridge tests for MIR's `Terminator::Select` lowering
//! (`HirExprKind::Select` → `Terminator::Select`).
//!
//! These tests pin the MIR-to-codegen contract slice 3 reads:
//!   * `Terminator::Select { arms, next: <join_bb> }` shape;
//!   * one body block per arm, each ending in `Terminator::Goto join_bb`;
//!   * `ExitPath::Select { block: originating, next: join_bb }` wired
//!     into the elaborated function's `drop_plans` (cleanup-CFG
//!     composition with D24-2);
//!   * `SelectArm::binding` populated (Some) for value-bearing arms with a
//!     binding pattern, None for `AfterTimer` arms.
//!
//! LESSONS row `producer-bridge-before-codegen` (P0): every cross-stage
//! producer-emit must carry a structural assertion test before the
//! consumer ships, so a future regression is caught at the producer
//! gate. This file is that gate for the select-actor-ask-race lane.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{ExitPath, IrPipeline, RawMirFunction, SelectArmKind, Terminator};
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

fn lower_hir_without_typecheck(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let hir = hew_hir::lower_program(
        &parsed.program,
        &hew_types::TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&hir.module)
}

fn find_main(pipeline: &IrPipeline) -> &RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main MIR function")
}

fn find_select_terminator(func: &RawMirFunction) -> (u32, &Vec<hew_mir::SelectArm>, u32) {
    for block in &func.blocks {
        if let Terminator::Select { arms, next } = &block.terminator {
            return (block.id, arms, *next);
        }
    }
    panic!(
        "expected exactly one Terminator::Select in `{}`; block terminators: {:?}",
        func.name,
        func.blocks
            .iter()
            .map(|b| (b.id, std::mem::discriminant(&b.terminator)))
            .collect::<Vec<_>>()
    );
}

// Two `ActorAsk` arms + one `AfterTimer` arm produce a well-formed
// `Terminator::Select` with three arms, three distinct body blocks, and
// one join block reached by Goto from every body.
//
// PRODUCER-BRIDGE TEST — this is the structural contract codegen
// (slice 3) reads. A regression here means slice 3 either can't dispatch
// or dispatches into a malformed CFG.
#[test]
fn two_ask_one_after_select_emits_three_arms_three_bodies_one_join() {
    let pipeline = lower_checked(
        r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        actor Counter {
            receive fn count() -> i64 { 2 }
        }
        fn main() -> i64 {
            let p = spawn Pinger;
            let c = spawn Counter;
            let result = select {
                reply from p.ping() => reply,
                verdict from c.count() => verdict,
                after 100ms => 0,
            };
            result
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let main = find_main(&pipeline);
    let (originating_bb, arms, join_bb) = find_select_terminator(main);

    // Three arms in source order: ActorAsk, ActorAsk, AfterTimer.
    assert_eq!(arms.len(), 3, "expected three arms; got {}", arms.len());
    assert!(
        matches!(arms[0].kind, SelectArmKind::ActorAsk { ref method, .. } if method == "ping"),
        "arm 0 kind: {:?}",
        arms[0].kind
    );
    assert!(
        matches!(arms[1].kind, SelectArmKind::ActorAsk { ref method, .. } if method == "count"),
        "arm 1 kind: {:?}",
        arms[1].kind
    );
    assert!(
        matches!(arms[2].kind, SelectArmKind::AfterTimer { .. }),
        "arm 2 kind: {:?}",
        arms[2].kind
    );

    // Per-arm body blocks are distinct, distinct from join, and
    // distinct from the originating block.
    let body_bbs: Vec<u32> = arms.iter().map(|a| a.body_block).collect();
    let mut seen = std::collections::HashSet::new();
    for bb in &body_bbs {
        assert!(seen.insert(*bb), "duplicate body block id {bb}");
        assert_ne!(*bb, join_bb, "body block must not alias join");
        assert_ne!(
            *bb, originating_bb,
            "body block must not alias originating block"
        );
    }
    assert_ne!(originating_bb, join_bb);

    // Every body block terminates with `Goto { target: join_bb }` —
    // the converging shape codegen relies on for the result write.
    for body_bb in &body_bbs {
        let block = main
            .blocks
            .iter()
            .find(|b| b.id == *body_bb)
            .unwrap_or_else(|| panic!("body block {body_bb} missing from blocks vector"));
        assert!(
            matches!(block.terminator, Terminator::Goto { target } if target == join_bb),
            "body block {body_bb} must Goto join_bb {join_bb}; got {:?}",
            block.terminator
        );
    }

    // Join block exists and is referenced by the Select terminator's
    // `next` slot.
    assert!(
        main.blocks.iter().any(|b| b.id == join_bb),
        "join block {join_bb} missing from blocks vector"
    );
}

// `SelectArm.binding` populated (Some) for ActorAsk arms; None for
// AfterTimer. This is the slot codegen writes `hew_reply_wait`'s result
// into on win — its presence is the producer-bridge contract.
//
// PRODUCER-BRIDGE TEST — join-block phi shape: every winner branch
// converges on the same `result_place` (the "phi" in this MIR's
// single-slot convergence model). Each body block emits a Move into the
// same destination local; codegen reads `result_place` after join.
#[test]
fn select_arm_binding_is_some_for_ask_none_for_after() {
    let pipeline = lower_checked(
        r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let p = spawn Pinger;
            let r = select {
                reply from p.ping() => reply,
                after 50ms => 0,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );

    let main = find_main(&pipeline);
    let (_, arms, _) = find_select_terminator(main);
    assert_eq!(arms.len(), 2);
    let ask = arms
        .iter()
        .find(|a| matches!(a.kind, SelectArmKind::ActorAsk { .. }))
        .expect("ActorAsk arm");
    let after = arms
        .iter()
        .find(|a| matches!(a.kind, SelectArmKind::AfterTimer { .. }))
        .expect("AfterTimer arm");
    assert!(
        ask.binding.is_some(),
        "ActorAsk arm binding must be Some (reply slot codegen writes hew_reply_wait into); \
         got None"
    );
    assert!(
        after.binding.is_none(),
        "AfterTimer arm binding must be None (no value bound); got {:?}",
        after.binding
    );
}

// Join-block convergence: every body block writes the arm's body value
// into the SAME destination local via `Instr::Move`. Codegen reads this
// `result_place` after join, so every winner branch must converge on
// it. This is the "phi shape" the brief calls out — a single-slot phi
// in MIR's non-SSA model.
#[test]
fn every_arm_body_moves_into_same_result_local() {
    let pipeline = lower_checked(
        r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        actor Counter {
            receive fn count() -> i64 { 2 }
        }
        fn main() -> i64 {
            let p = spawn Pinger;
            let c = spawn Counter;
            let r = select {
                reply from p.ping() => reply,
                verdict from c.count() => verdict,
                after 100ms => 7,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );

    let main = find_main(&pipeline);
    let (_, arms, join_bb) = find_select_terminator(main);

    // Collect every body block's last Move destination.
    let mut move_dests: Vec<hew_mir::Place> = Vec::new();
    for arm in arms {
        let body = main
            .blocks
            .iter()
            .find(|b| b.id == arm.body_block)
            .expect("body block in blocks vec");
        let last_move = body
            .instructions
            .iter()
            .rev()
            .find_map(|instr| {
                if let hew_mir::Instr::Move { dest, .. } = instr {
                    Some(*dest)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                panic!(
                    "body block {} has no Move; expected the arm body \
                                       to Move its value into result_place",
                    arm.body_block
                )
            });
        move_dests.push(last_move);
    }

    let first = move_dests[0];
    for (i, dest) in move_dests.iter().enumerate() {
        assert_eq!(
            *dest, first,
            "arm {i} writes into a different local than arm 0; \
             every arm must converge on the same result_place"
        );
    }

    // The join block exists and is reachable from each body via Goto.
    let join = main
        .blocks
        .iter()
        .find(|b| b.id == join_bb)
        .expect("join block exists");
    // Join block's terminator is the function-tail Terminator::Return
    // (the join continues straight into the function's return path
    // because the select expression is the binding's RHS and the
    // function returns it).
    assert!(
        matches!(join.terminator, Terminator::Return),
        "expected join_bb to flow into Terminator::Return; got {:?}",
        join.terminator
    );
}

// `ExitPath::Select { block: originating, next: join_bb }` is wired
// into the elaborated function's `drop_plans`. The elaboration pass
// (`enumerate_exits`) walks `Terminator::Select` and emits this exit
// path automatically; the test pins the wiring so a regression in
// `enumerate_exits` is caught at the producer side, not at codegen.
#[test]
fn select_terminator_wires_exitpath_select_in_drop_plans() {
    let pipeline = lower_checked(
        r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let p = spawn Pinger;
            let r = select {
                reply from p.ping() => reply,
                after 50ms => 0,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );

    let elab = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("elaborated main");

    let (block, next) = elab
        .drop_plans
        .iter()
        .find_map(|(exit, _plan)| {
            if let ExitPath::Select { block, next } = exit {
                Some((*block, *next))
            } else {
                None
            }
        })
        .expect("ExitPath::Select must appear in main's drop_plans");

    // The block id in the ExitPath must match the originating block id
    // and `next` must match the join_bb the terminator carries.
    let raw_main = find_main(&pipeline);
    let (originating_bb, _, join_bb) = find_select_terminator(raw_main);
    assert_eq!(block, originating_bb, "ExitPath::Select.block mismatch");
    assert_eq!(next, join_bb, "ExitPath::Select.next mismatch");
}

// Native MIR now carries `StreamNext` arms through to `Terminator::Select`.
// We bypass the type-checker and use a literal stream operand because this
// producer-side test only pins the HIR→MIR arm-kind bridge; codegen still
// fails closed for this arm kind until the backend consumer lands.
#[test]
#[cfg(not(target_arch = "wasm32"))]
fn stream_next_arm_emits_select_arm_kind_on_native() {
    let pipeline = lower_hir_without_typecheck(
        r"
        fn main() -> i64 {
            let r = select {
                item from next(1) => 0,
                after 10ms => 0,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let main = find_main(&pipeline);
    let (_, arms, _) = find_select_terminator(main);
    assert_eq!(arms.len(), 2);
    let stream = arms
        .iter()
        .find(|a| matches!(a.kind, SelectArmKind::StreamNext { .. }))
        .expect("StreamNext arm");
    assert!(
        stream.binding.is_some(),
        "StreamNext arm binding must be Some for its item slot"
    );
}

// wasm32 remains fail-closed for StreamNext at the MIR producer boundary
// because the stream substrate is native-only.
#[test]
#[cfg(target_arch = "wasm32")]
fn stream_next_arm_rejected_on_wasm32() {
    let pipeline = lower_hir_without_typecheck(
        r"
        fn main() -> i64 {
            let r = select {
                item from next(1) => 0,
                after 10ms => 0,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::SelectArmNotImplemented { arm_kind, .. }
                if arm_kind == "StreamNext"
        )),
        "expected StreamNext to remain fail-closed on wasm32; got {:?}",
        pipeline.diagnostics
    );
}

// MIR now carries `TaskAwait` arms through to `Terminator::Select`; codegen
// keeps the defence-in-depth rejection until the backend consumer lands.
#[test]
fn task_await_arm_emits_select_arm_kind() {
    let pipeline = lower_hir_without_typecheck(
        r"
        fn main() -> i64 {
            let r = select {
                v from await 1 => 0,
                after 10ms => 0,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let main = find_main(&pipeline);
    let (_, arms, _) = find_select_terminator(main);
    assert_eq!(arms.len(), 2);
    let task = arms
        .iter()
        .find(|a| matches!(a.kind, SelectArmKind::TaskAwait { .. }))
        .expect("TaskAwait arm");
    assert!(
        task.binding.is_some(),
        "TaskAwait arm binding must be Some for its awaited value slot"
    );
}

// Single ActorAsk arm with NO after-arm (explicit-form companion).
// Pins that a one-arm select emits a one-arm Terminator::Select.
#[test]
fn single_ask_arm_no_after_emits_one_arm_select() {
    let pipeline = lower_checked(
        r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let p = spawn Pinger;
            let r = select {
                reply from p.ping() => reply,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    let main = find_main(&pipeline);
    let (_, arms, _) = find_select_terminator(main);
    assert_eq!(arms.len(), 1);
    assert!(matches!(arms[0].kind, SelectArmKind::ActorAsk { .. }));
}

// Arm-body BindingRef resolves to the per-arm reply slot — i.e.
// `binding_locals[binding_id] = SelectArm.binding` is the wiring
// codegen relies on. The arm body must reference the binding to read
// the reply value; this test pins that the BindingRef-emitted Move
// targets the same Place as `SelectArm.binding` (the reply slot).
#[test]
fn arm_body_binding_reads_resolve_to_per_arm_reply_slot() {
    let pipeline = lower_checked(
        r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let p = spawn Pinger;
            let r = select {
                reply from p.ping() => reply,
                after 10ms => 0,
            };
            r
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    let main = find_main(&pipeline);
    let (_, arms, _) = find_select_terminator(main);

    let ask_arm = arms
        .iter()
        .find(|a| matches!(a.kind, SelectArmKind::ActorAsk { .. }))
        .expect("ActorAsk arm");
    let reply_slot = ask_arm.binding.expect("ActorAsk arm has reply slot");

    let body = main
        .blocks
        .iter()
        .find(|b| b.id == ask_arm.body_block)
        .expect("ActorAsk body block");
    // The body lowers `reply` (a BindingRef) and Moves it into
    // result_place. The Move's `src` must be the reply_slot Place.
    let move_src = body
        .instructions
        .iter()
        .find_map(|i| {
            if let hew_mir::Instr::Move { src, .. } = i {
                Some(*src)
            } else {
                None
            }
        })
        .expect("body block has a Move");
    assert_eq!(
        move_src, reply_slot,
        "ActorAsk arm body's Move source must be the per-arm reply slot \
         (binding_locals[binding_id] = SelectArm.binding); src={move_src:?}, \
         expected={reply_slot:?}"
    );
}
