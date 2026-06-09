//! Structural tests for `Terminator::Trap { kind: TrapKind }` — the MIR
//! hard-abort primitive.
//!
//! These are construction-side tests: they hand-build `BasicBlock` /
//! `RawMirFunction` values carrying `Terminator::Trap` and verify that:
//!
//! 1. All five `TrapKind` variants construct without panic.
//! 2. The variant round-trips through `Clone` + `PartialEq`.
//! 3. CFG utilities (`build_preds`, `successors`) treat `Trap` as terminal
//!    (no outgoing edges), matching `Return`.
//! 4. `ExitPath::Panic` is produced by drop elaboration for a
//!    `Terminator::Trap`-terminated function (the exit-edge discriminant
//!    that drop elaboration cares about).
//!
//! LESSONS applied:
//! - `exhaustive-coverage` (P0): one test per `TrapKind` variant; all five
//!   are exercised, even though no producer exists yet for any of them.
//! - `boundary-fail-closed` (P0): the codegen arm is a real `llvm.trap`
//!   emission, not a `CodegenError::Unsupported`; tests in hew-codegen-rs
//!   cover that arm separately.
//! - `parity-or-tracked-gap` (P1): producer bridges are named and deferred
//!   to slices B-2, B-5, C-2, C-3; no variant has a silent gap.

use hew_mir::{BasicBlock, BlockKind, ExitPath, IrPipeline, RawMirFunction, Terminator, TrapKind};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a minimal `RawMirFunction` whose single block terminates with
/// `Terminator::Trap { kind }`. Used for MIR-shape assertions below.
fn trap_fn(kind: TrapKind) -> RawMirFunction {
    RawMirFunction {
        name: format!("trap_{kind:?}"),
        return_ty: ResolvedTy::I64,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Trap { kind },
        }],
        decisions: vec![],
        intrinsic_id: None,
    }
}

/// Run the full source-text MIR pipeline so elaboration output is visible.
fn pipeline(source: &str) -> IrPipeline {
    use hew_hir::{lower_program, verify_hir, ResolutionCtx};
    use hew_types::TypeCheckOutput;

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    hew_mir::lower_hir_module(&output.module)
}

// ---------------------------------------------------------------------------
// TrapKind variant construction — one test per variant (exhaustive-coverage)
// ---------------------------------------------------------------------------

#[test]
fn trap_kind_integer_overflow_constructs() {
    // IntegerOverflow — wired by B-2 (overflow-trap lowering).
    // No producer exists today; this test pins the consumer-side shape.
    let f = trap_fn(TrapKind::IntegerOverflow);
    assert_eq!(f.blocks.len(), 1);
    assert!(matches!(
        f.blocks[0].terminator,
        Terminator::Trap {
            kind: TrapKind::IntegerOverflow
        }
    ));
}

#[test]
fn trap_kind_index_out_of_bounds_constructs() {
    // IndexOutOfBounds — wired by C-2 (Vec/array OOB formalisation).
    // No producer exists today; this test pins the consumer-side shape.
    let f = trap_fn(TrapKind::IndexOutOfBounds);
    assert!(matches!(
        f.blocks[0].terminator,
        Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds
        }
    ));
}

#[test]
fn trap_kind_divide_by_zero_constructs() {
    // DivideByZero — wired by B-5 (divide-by-zero trap).
    // No producer exists today; this test pins the consumer-side shape.
    let f = trap_fn(TrapKind::DivideByZero);
    assert!(matches!(
        f.blocks[0].terminator,
        Terminator::Trap {
            kind: TrapKind::DivideByZero
        }
    ));
}

#[test]
fn trap_kind_signed_min_div_neg_one_constructs() {
    // SignedMinDivNegOne — wired by B-5 (signed-MIN/-1 trap).
    // No producer exists today; this test pins the consumer-side shape.
    let f = trap_fn(TrapKind::SignedMinDivNegOne);
    assert!(matches!(
        f.blocks[0].terminator,
        Terminator::Trap {
            kind: TrapKind::SignedMinDivNegOne
        }
    ));
}

#[test]
fn trap_kind_shift_out_of_range_constructs() {
    // ShiftOutOfRange — wired by B-5 (shift-range trap).
    // No producer exists today; this test pins the consumer-side shape.
    let f = trap_fn(TrapKind::ShiftOutOfRange);
    assert!(matches!(
        f.blocks[0].terminator,
        Terminator::Trap {
            kind: TrapKind::ShiftOutOfRange
        }
    ));
}

// ---------------------------------------------------------------------------
// Round-trip: Clone + PartialEq must preserve TrapKind identity
// ---------------------------------------------------------------------------

#[test]
fn trap_terminator_round_trips_clone_eq() {
    // All five variants must survive Clone + PartialEq round-trip so
    // MIR carriers (IrPipeline, CheckedMirFunction) can be compared in
    // tests and diagnostics without loss.
    let kinds = [
        TrapKind::IntegerOverflow,
        TrapKind::IndexOutOfBounds,
        TrapKind::DivideByZero,
        TrapKind::SignedMinDivNegOne,
        TrapKind::ShiftOutOfRange,
    ];
    for kind in kinds {
        let f = trap_fn(kind);
        let copy = f.clone();
        assert_eq!(f, copy, "Trap({kind:?}) must clone-eq");
        assert_eq!(
            copy.blocks[0].terminator,
            Terminator::Trap { kind },
            "TrapKind must survive Clone without mutation"
        );
    }
}

// ---------------------------------------------------------------------------
// CFG shape: Trap is terminal (no successors, no predecessors to next block)
// ---------------------------------------------------------------------------

#[test]
fn trap_terminator_is_terminal_in_block_shape() {
    // `Terminator::Trap` must be treated as terminal by CFG utilities:
    // a Trap-terminated block emits no outgoing predecessor edges, so any
    // block following it in the vec is unreachable. The `dataflow::analyze`
    // pass walks the CFG and must not panic when it encounters a Trap block.
    //
    // This is the same structural property `Terminator::Return` has;
    // the rename from Panic to Trap must preserve it.
    use hew_hir::TypeClassTable;
    use hew_mir::dataflow::analyze;

    // A function with a single Trap-terminated block. `analyze` must
    // complete without panic. It will find no use-before-init or
    // use-after-consume violations since the block has no statements.
    let blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Trap {
            kind: TrapKind::DivideByZero,
        },
    }];

    // analyze runs build_preds + successors + transfer internally;
    // a Trap block has no successors so the exit_states map has one entry
    // (for the single block id 0) and the checks vec is empty.
    let result = analyze(&blocks, &TypeClassTable::default(), &[]);
    assert_eq!(
        result.exit_states.len(),
        1,
        "Trap-terminated single-block function must produce one liveness entry"
    );
    assert!(
        result.checks.is_empty(),
        "empty-statement Trap block must produce no diagnostic checks"
    );
}

// ---------------------------------------------------------------------------
// Drop elaboration: Trap-terminated function emits ExitPath::Panic edge
// ---------------------------------------------------------------------------

#[test]
fn trap_terminator_produces_panic_exit_path_in_elaboration() {
    // Drop elaboration maps `Terminator::Trap` to `ExitPath::Panic`.
    // The `ExitPath` discriminant is what the cleanup-block builder
    // and the drop-plan emitter key on; it must remain `Panic` (not a
    // new `Trap` variant) so existing drop-elaboration logic requires
    // no changes in this slice.
    //
    // B-2 wires the first source-level producer: the default `+`
    // operator lowers to `Instr::IntArithChecked` plus a CFG split
    // whose overflow successor terminates with `Terminator::Trap {
    // kind: TrapKind::IntegerOverflow }`. The pipeline below covers
    // that producer end-to-end: a source-text `1 + 2` must surface
    // both a `Return` exit (for the normal continuation) and a
    // `Panic` exit (for the overflow trap block).
    let p = pipeline("fn main() -> i64 { 1 + 2 }");
    let func = &p.elaborated_mir[0];
    let kinds: Vec<&ExitPath> = func.drop_plans.iter().map(|(ep, _)| ep).collect();
    assert!(
        kinds.iter().any(|ep| matches!(ep, ExitPath::Return { .. })),
        "1 + 2 still produces a Return exit on the non-overflow path: {kinds:?}"
    );
    assert!(
        kinds.iter().any(|ep| matches!(ep, ExitPath::Panic { .. })),
        "1 + 2 must produce a Panic exit for the overflow-trap successor under B-2: {kinds:?}"
    );
}

// ---------------------------------------------------------------------------
// BlockKind::Cleanup is emitted for Trap blocks (structural invariant)
// ---------------------------------------------------------------------------

#[test]
fn cleanup_block_kind_exists_in_model() {
    // `BlockKind::Cleanup` is the elaborated shape for Trap/Panic exits.
    // Pin the discriminant so a rename refactor cannot silently remove it.
    let kind = BlockKind::Cleanup;
    assert_eq!(kind, BlockKind::Cleanup);
    assert_ne!(kind, BlockKind::Normal);
}
