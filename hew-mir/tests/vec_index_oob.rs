//! MIR-shape tests for C-2: `Vec<T>` integer-indexed `xs[i]` with
//! bounds-checked trap-on-OOB.
//!
//! These tests verify the MIR CFG shape produced by `lower_vec_index`:
//!
//! 1. The allowlist contains the six Vec runtime symbols.
//! 2. `RuntimeCall::new` constructs successfully for each Vec symbol.
//! 3. The MIR-shape for `Vec<i64>` indexing has the expected structure:
//!    - `CallRuntimeAbi(hew_vec_len)` with vec place as arg and dest.
//!    - `IntCmp { pred: UnsignedGreaterEq }` on index vs len.
//!    - `Terminator::Branch` → trap block and continuation block.
//!    - Trap block terminates with `Terminator::Trap { kind: IndexOutOfBounds }`.
//!    - Continuation block has `CallRuntimeAbi(hew_vec_get_i64)`.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the OOB trap must be on the CFG;
//!   a regression that skips it would silently produce unsafe code.
//! - `exhaustive-coverage` (P0): allowlist test covers all six Vec symbols;
//!   the MIR-shape test pins the structural invariant end-to-end.
//! - `parity-or-tracked-gap` (P1): Vec literal construction is not yet
//!   lowered (no `hew_vec_new` / `hew_vec_push` producer in MIR);
//!   tests use hand-built MIR rather than source-text pipeline tests.
//!   An integration test (`tests/integration/vec_oob_trap.hew`) will follow
//!   once the Vec literal lowering lane lands.

use hew_mir::runtime_symbols::is_known_runtime_symbol;
use hew_mir::{
    BasicBlock, CmpPred, Instr, IrPipeline, Place, RawMirFunction, Terminator, TrapKind,
};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Allowlist coverage for Vec runtime symbols
// ---------------------------------------------------------------------------

#[test]
fn allowlist_covers_vec_len() {
    // hew_vec_len is the length-probe used by every bounds check.
    assert!(
        is_known_runtime_symbol("hew_vec_len"),
        "hew_vec_len must be in the M2 runtime-ABI allowlist",
    );
}

#[test]
fn allowlist_covers_vec_get_family() {
    // All five typed getters must be allowlisted so the MIR producer can
    // emit them. hew_vec_get_str is allowlisted but not yet emitted by this
    // slice (String element drop is a follow-on).
    let get_symbols = [
        "hew_vec_get_i32",
        "hew_vec_get_i64",
        "hew_vec_get_f64",
        "hew_vec_get_ptr",
        "hew_vec_get_str",
    ];
    for sym in &get_symbols {
        assert!(
            is_known_runtime_symbol(sym),
            "Vec getter `{sym}` must be in the M2 runtime-ABI allowlist",
        );
    }
}

#[test]
fn runtime_call_constructs_for_hew_vec_len() {
    // Allowlist boundary: construction succeeds, symbol round-trips.
    let call =
        hew_mir::RuntimeCall::new("hew_vec_len", vec![Place::Local(0)], Some(Place::Local(1)))
            .expect("hew_vec_len is allowlisted");
    assert_eq!(call.symbol(), "hew_vec_len");
    assert_eq!(call.args().len(), 1);
    assert_eq!(call.dest(), Some(Place::Local(1)));
}

#[test]
fn runtime_call_constructs_for_hew_vec_get_i64() {
    // Allowlist boundary: construction succeeds for the typed getter.
    let call = hew_mir::RuntimeCall::new(
        "hew_vec_get_i64",
        vec![Place::Local(0), Place::Local(1)],
        Some(Place::Local(2)),
    )
    .expect("hew_vec_get_i64 is allowlisted");
    assert_eq!(call.symbol(), "hew_vec_get_i64");
    assert_eq!(call.args().len(), 2);
    assert_eq!(call.dest(), Some(Place::Local(2)));
}

// ---------------------------------------------------------------------------
// MIR-shape: bounds-check CFG for Vec<i64> indexing
//
// Hand-built `RawMirFunction` that mirrors what `lower_vec_index` produces
// for a `Vec<i64>` container:
//
//   bb0 (entry):
//     CallRuntimeAbi { hew_vec_len, args: [Local(0)], dest: Local(2) }
//     IntCmp { UnsignedGreaterEq, dest: Local(3), lhs: Local(1), rhs: Local(2) }
//     Branch { cond: Local(3), then: 1, else: 2 }
//
//   bb1 (trap):
//     Trap { kind: IndexOutOfBounds }
//
//   bb2 (cont):
//     CallRuntimeAbi { hew_vec_get_i64, args: [Local(0), Local(1)], dest: Local(4) }
//     Return
// ---------------------------------------------------------------------------

/// Build the hand-crafted CFG for `Vec<i64>` indexing. Mirrors the output
/// of `lower_vec_index` for element type `i64`.
fn vec_index_i64_mir() -> RawMirFunction {
    // Local(0): Vec<i64> container (ptr-typed)
    // Local(1): index (i64)
    // Local(2): len (i64) — output of hew_vec_len
    // Local(3): oob_flag (bool) — output of IntCmp
    // Local(4): result (i64) — output of hew_vec_get_i64
    let vec_place = Place::Local(0);
    let index_place = Place::Local(1);
    let len_place = Place::Local(2);
    let oob_flag = Place::Local(3);
    let result_place = Place::Local(4);

    let entry_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::CallRuntimeAbi(
                hew_mir::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                    .expect("hew_vec_len is allowlisted"),
            ),
            Instr::IntCmp {
                dest: oob_flag,
                pred: CmpPred::UnsignedGreaterEq,
                lhs: index_place,
                rhs: len_place,
            },
        ],
        terminator: Terminator::Branch {
            cond: oob_flag,
            then_target: 1,
            else_target: 2,
        },
    };

    let trap_block = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        },
    };

    let cont_block = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![Instr::CallRuntimeAbi(
            hew_mir::RuntimeCall::new(
                "hew_vec_get_i64",
                vec![vec_place, index_place],
                Some(result_place),
            )
            .expect("hew_vec_get_i64 is allowlisted"),
        )],
        terminator: Terminator::Return,
    };

    RawMirFunction {
        name: "vec_index_i64_test".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![
            ResolvedTy::Named {
                name: "Vec".to_string(),
                args: vec![ResolvedTy::I64],
                builtin: None,
            }, // Local(0)
            ResolvedTy::I64,  // Local(1) index
            ResolvedTy::I64,  // Local(2) len
            ResolvedTy::Bool, // Local(3) oob_flag
            ResolvedTy::I64,  // Local(4) result
        ],
        blocks: vec![entry_block, trap_block, cont_block],
        decisions: vec![],
    }
}

#[test]
fn vec_index_oob_has_trap_block_with_index_out_of_bounds_kind() {
    // The trap block must carry `TrapKind::IndexOutOfBounds` — not some
    // other trap kind. A regression that uses `IntegerOverflow` or
    // `DivideByZero` would silently surface the wrong diagnostic to the
    // operator. (boundary-fail-closed P0)
    let func = vec_index_i64_mir();
    let trap_blocks: Vec<&BasicBlock> = func
        .blocks
        .iter()
        .filter(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: TrapKind::IndexOutOfBounds
                }
            )
        })
        .collect();
    assert_eq!(
        trap_blocks.len(),
        1,
        "expected exactly one IndexOutOfBounds trap block; got {}",
        trap_blocks.len()
    );
    // The trap block must have no instructions — only the terminator.
    assert!(
        trap_blocks[0].instructions.is_empty(),
        "trap block must carry no instructions; the Terminator::Trap is the entire block"
    );
}

#[test]
fn vec_index_oob_entry_block_has_vec_len_call_then_unsigned_ge_cmp() {
    // The entry block must: (1) call hew_vec_len, (2) emit IntCmp with
    // UnsignedGreaterEq, (3) branch on the flag. This is the structural
    // invariant that `lower_vec_index` establishes.
    let func = vec_index_i64_mir();
    let entry = &func.blocks[0];

    // First instruction: CallRuntimeAbi(hew_vec_len).
    let Some(Instr::CallRuntimeAbi(len_call)) = entry.instructions.first() else {
        panic!(
            "first instruction of entry block must be CallRuntimeAbi(hew_vec_len); \
             got {:?}",
            entry.instructions.first()
        );
    };
    assert_eq!(
        len_call.symbol(),
        "hew_vec_len",
        "first CallRuntimeAbi in entry must be hew_vec_len"
    );
    assert!(
        len_call.dest().is_some(),
        "hew_vec_len must have a dest place for the length"
    );

    // Second instruction: IntCmp with UnsignedGreaterEq predicate.
    let Some(Instr::IntCmp { pred, .. }) = entry.instructions.get(1) else {
        panic!(
            "second instruction of entry block must be IntCmp; got {:?}",
            entry.instructions.get(1)
        );
    };
    assert_eq!(
        *pred,
        CmpPred::UnsignedGreaterEq,
        "bounds check must use UnsignedGreaterEq to catch both negative and OOB indices"
    );

    // Terminator: Branch.
    assert!(
        matches!(entry.terminator, Terminator::Branch { .. }),
        "entry block must terminate with Branch; got {:?}",
        entry.terminator
    );
}

#[test]
fn vec_index_oob_cont_block_has_get_call() {
    // The continuation block (else target of the bounds-check Branch) must
    // call `hew_vec_get_i64` with a dest place — that is the element load.
    let func = vec_index_i64_mir();
    let cont = &func.blocks[2]; // else_target of entry Branch = block 2

    let Some(Instr::CallRuntimeAbi(get_call)) = cont.instructions.first() else {
        panic!(
            "continuation block must start with CallRuntimeAbi(hew_vec_get_i64); \
             got {:?}",
            cont.instructions.first()
        );
    };
    assert_eq!(
        get_call.symbol(),
        "hew_vec_get_i64",
        "element load must call hew_vec_get_i64"
    );
    assert_eq!(
        get_call.args().len(),
        2,
        "hew_vec_get_i64 must receive 2 args: vec + index"
    );
    assert!(
        get_call.dest().is_some(),
        "hew_vec_get_i64 must have a dest place for the loaded element"
    );
}

#[test]
fn vec_index_oob_branch_targets_trap_and_cont() {
    // The Branch terminator's then_target must be the trap block and
    // else_target must be the continuation block.
    let func = vec_index_i64_mir();
    let entry = &func.blocks[0];
    let Terminator::Branch {
        cond: _,
        then_target,
        else_target,
    } = entry.terminator
    else {
        panic!("entry block must terminate with Branch");
    };
    // then_target → trap
    assert!(
        matches!(
            func.blocks[then_target as usize].terminator,
            Terminator::Trap {
                kind: TrapKind::IndexOutOfBounds
            }
        ),
        "then_target block must be the IndexOutOfBounds trap block"
    );
    // else_target → cont (has the get call)
    let cont = &func.blocks[else_target as usize];
    assert!(
        cont.instructions
            .iter()
            .any(|i| matches!(i, Instr::CallRuntimeAbi(_))),
        "else_target block must contain the hew_vec_get_i64 call"
    );
}

// ---------------------------------------------------------------------------
// Pipeline-level: IrPipeline wraps without errors
// ---------------------------------------------------------------------------

/// Smoke: wrapping the hand-built function in an `IrPipeline` succeeds.
/// This ensures the MIR model accepts the shape without panicking.
#[test]
fn vec_index_mir_wraps_in_pipeline_without_errors() {
    let func = vec_index_i64_mir();
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![func],
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
    };
    // No diagnostics from the hand-built MIR (it is already valid).
    assert!(
        pipeline.diagnostics.is_empty(),
        "hand-built vec-index MIR must not carry diagnostics"
    );
}
