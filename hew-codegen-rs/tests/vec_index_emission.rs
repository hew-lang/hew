//! C-2 codegen emission tests: `Vec<T>` integer-indexed `xs[i]` bounds-check
//! lowering to LLVM IR.
//!
//! These tests build a hand-crafted `IrPipeline` that mirrors what
//! `lower_vec_index` produces for `Vec<i64>`, then assert on the textual
//! LLVM IR that the codegen emitter correctly lowers:
//!
//! 1. `hew_vec_len(ptr) -> i64`  — extern declare + call.
//! 2. `UnsignedGreaterEq` IntCmp — emitted as `icmp uge`.
//! 3. `Terminator::Branch` on the flag — emitted as `br i1 ..., OOB, cont`.
//! 4. `Terminator::Trap { kind: IndexOutOfBounds }` — emitted as
//!    `call void @llvm.trap()` + `unreachable`.
//! 5. `hew_vec_get_i64(ptr, i64) -> i64` — extern declare + call on the
//!    continuation path.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): both the `llvm.trap` call and the
//!   `hew_vec_len` / `hew_vec_get_i64` declares must appear in the IR.
//! - `exhaustive-coverage` (P0): one test per structural assertion so
//!   each invariant has its own failure message.
//! - `parity-or-tracked-gap` (P1): Vec literal construction is not yet
//!   lowered; this test builds the pipeline by hand.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, CmpPred, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, Terminator, TrapKind,
};
use hew_types::ResolvedTy;

/// Build the hand-crafted `IrPipeline` that mirrors C-2's `lower_vec_index`
/// output for a `Vec<i64>` container.
///
/// Locals layout:
/// - Local(0): Vec<i64> handle (`ptr`-typed, the `*mut HewVec`)
/// - Local(1): index (i64)
/// - Local(2): len (i64) — hew_vec_len output
/// - Local(3): oob_flag (bool / i8) — IntCmp output
/// - Local(4): result (i64) — hew_vec_get_i64 output
///
/// CFG:
///   bb0: hew_vec_len + icmp uge + branch → bb1 (trap) / bb2 (cont)
///   bb1: llvm.trap + unreachable
///   bb2: hew_vec_get_i64 + return
fn vec_index_i64_pipeline() -> IrPipeline {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::I64],
    };

    let vec_place = Place::Local(0);
    let index_place = Place::Local(1);
    let len_place = Place::Local(2);
    let oob_flag = Place::Local(3);
    let result_place = Place::Local(4);

    let entry_bb = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            // Seed index with a known value for the test (would come from a
            // function parameter in real lowering; hand-set here).
            Instr::ConstI64 {
                dest: index_place,
                value: 0,
            },
            // Call hew_vec_len to get the length.
            Instr::CallRuntimeAbi(
                RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                    .expect("hew_vec_len is allowlisted"),
            ),
            // UnsignedGreaterEq: oob_flag = (index uge len)
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

    let trap_bb = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        },
    };

    let cont_bb = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_vec_get_i64",
                    vec![vec_place, index_place],
                    Some(result_place),
                )
                .expect("hew_vec_get_i64 is allowlisted"),
            ),
            Instr::Move {
                dest: Place::ReturnSlot,
                src: result_place,
            },
        ],
        terminator: Terminator::Return,
    };

    let raw_blocks = vec![entry_bb.clone(), trap_bb.clone(), cont_bb.clone()];

    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![
                vec_ty,           // 0 Vec handle
                ResolvedTy::I64,  // 1 index
                ResolvedTy::I64,  // 2 len
                ResolvedTy::Bool, // 3 oob_flag
                ResolvedTy::I64,  // 4 result
            ],
            blocks: raw_blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: raw_blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![
                (ExitPath::Return { block: 2 }, DropPlan::default()),
                (ExitPath::Panic { block: 1 }, DropPlan::default()),
            ],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        record_layouts: vec![],
    }
}

fn emit_ll(module_name: &str) -> String {
    let pipeline = vec_index_i64_pipeline();
    let tmp = std::env::temp_dir().join(format!("hew-c2-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("C-2 Vec-index pipeline must emit successfully");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// extern declare assertions
// ---------------------------------------------------------------------------

#[test]
fn vec_len_declare_has_correct_signature() {
    // hew_vec_len(ptr) -> i64 must be declared as an extern in the module.
    // The `intern_runtime_decl` arm for `hew_vec_len` uses `i64_ty.fn_type`
    // with a single `ptr` arg. Regression: a wrong signature would silently
    // produce a mismatched call at link time.
    let ll = emit_ll("vec_len_declare");
    assert!(
        ll.contains("declare i64 @hew_vec_len(ptr)"),
        "hew_vec_len must be declared as `i64(ptr)`; got:\n{ll}"
    );
}

#[test]
fn vec_get_i64_declare_has_correct_signature() {
    // hew_vec_get_i64(ptr, i64) -> i64.
    let ll = emit_ll("vec_get_i64_declare");
    assert!(
        ll.contains("declare i64 @hew_vec_get_i64(ptr, i64)"),
        "hew_vec_get_i64 must be declared as `i64(ptr, i64)`; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Bounds-check structure
// ---------------------------------------------------------------------------

#[test]
fn bounds_check_emits_icmp_uge() {
    // The UnsignedGreaterEq predicate must lower to `icmp uge` — the single
    // instruction that catches both negative indices and out-of-range accesses.
    // A regression to `icmp sge` would silently accept negative indices as
    // valid when cast to a large signed value.
    let ll = emit_ll("icmp_uge");
    assert!(
        ll.contains("icmp uge"),
        "bounds check must emit `icmp uge` for UnsignedGreaterEq; got:\n{ll}"
    );
}

#[test]
fn trap_path_emits_llvm_trap_then_unreachable() {
    // The OOB trap block must emit `call void @llvm.trap()` followed by
    // `unreachable`. Missing `llvm.trap` would produce a branch to an
    // unreachable without signalling the operator (boundary-fail-closed P0).
    let ll = emit_ll("trap_path");
    assert!(
        ll.contains("@llvm.trap"),
        "OOB trap path must call @llvm.trap; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "OOB trap path must be terminated by `unreachable`; got:\n{ll}"
    );
}

#[test]
fn continuation_path_calls_hew_vec_get_i64() {
    // The continuation block (success path) must call `hew_vec_get_i64`.
    let ll = emit_ll("vec_get_call");
    assert!(
        ll.contains("call i64 @hew_vec_get_i64("),
        "continuation path must call hew_vec_get_i64; got:\n{ll}"
    );
}

#[test]
fn entry_block_calls_hew_vec_len() {
    // The entry block must call `hew_vec_len` before any branch.
    let ll = emit_ll("vec_len_call");
    assert!(
        ll.contains("call i64 @hew_vec_len("),
        "entry block must call hew_vec_len; got:\n{ll}"
    );
}
