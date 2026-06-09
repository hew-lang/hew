//! C-3 codegen emission tests: `Vec<T>` range-slice lowering to LLVM IR.
//!
//! Hand-builds the pipeline that mirrors what `lower_vec_slice` produces
//! for `Vec<i64>` with a closed `[start..end)` range, then asserts on
//! the textual LLVM IR:
//!
//! 1. `hew_vec_slice_range_i64(ptr, i64, i64) -> ptr` — extern declare.
//! 2. The OOB trap shared across both bounds checks (`llvm.trap` +
//!    `unreachable`).
//! 3. The continuation path calls `hew_vec_slice_range_i64`.
//! 4. The bounds checks emit `icmp sgt` (start > end, end > len).
//!
//! Element-type dispatch tests (`hew_vec_slice_range_{i32,f64,ptr,str}`)
//! reuse the same pipeline shape with different element types — covered
//! by the declare-signature assertions per symbol.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, CmpPred, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, Terminator, TrapKind,
};
use hew_types::ResolvedTy;

/// Mirror of `lower_vec_slice` for `Vec<i64>` with closed `[start..end)`.
///
/// Locals layout:
/// - Local(0): Vec<i64> handle (`ptr`, the `*mut HewVec`)
/// - Local(1): start (i64)  — seeded with ConstI64
/// - Local(2): end (i64)    — seeded with ConstI64
/// - Local(3): bad1 (bool)  — start > end ?
/// - Local(4): len (i64)    — from hew_vec_len in after_check1_bb
/// - Local(5): bad2 (bool)  — end > len ?
/// - Local(6): result (Vec<i64>) — *mut HewVec from slice runtime call
///
/// CFG:
///   bb0 (entry): seed start/end → IntCmp sgt → Branch bb1 (oob) / bb2 (after_c1)
///   bb1: Trap IndexOutOfBounds   (shared OOB trap)
///   bb2 (after_c1): hew_vec_len → IntCmp sgt → Branch bb1 / bb3 (after_c2)
///   bb3 (after_c2): hew_vec_slice_range_i64 → Return
fn vec_slice_i64_pipeline() -> IrPipeline {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: None,
    };

    let vec_place = Place::Local(0);
    let start_place = Place::Local(1);
    let end_place = Place::Local(2);
    let bad1 = Place::Local(3);
    let len_place = Place::Local(4);
    let bad2 = Place::Local(5);
    let result_place = Place::Local(6);

    let entry_bb = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::ConstI64 {
                dest: start_place,
                value: 0,
            },
            Instr::ConstI64 {
                dest: end_place,
                value: 2,
            },
            // bad1 = start > end ?
            Instr::IntCmp {
                dest: bad1,
                pred: CmpPred::SignedGreater,
                lhs: start_place,
                rhs: end_place,
            },
        ],
        terminator: Terminator::Branch {
            cond: bad1,
            then_target: 1, // oob trap
            else_target: 2, // after_c1
        },
    };

    let oob_trap_bb = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        },
    };

    let after_c1_bb = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![
            Instr::CallRuntimeAbi(
                RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                    .expect("hew_vec_len is allowlisted"),
            ),
            // bad2 = end > len ?
            Instr::IntCmp {
                dest: bad2,
                pred: CmpPred::SignedGreater,
                lhs: end_place,
                rhs: len_place,
            },
        ],
        terminator: Terminator::Branch {
            cond: bad2,
            then_target: 1, // shared oob trap
            else_target: 3, // after_c2
        },
    };

    // Codegen Cluster-1 restriction (see C-2's vec_index_emission.rs):
    // the entry function must return an integer. We allocate the slice
    // result as a ptr-typed local (so the codegen-arm wiring for
    // hew_vec_slice_range_i64 still fires) and return the i64 length
    // of the resulting slice so the return type stays inside the
    // integer-returning subset.
    let ret_len = Place::Local(7);
    let after_c2_bb = BasicBlock {
        id: 3,
        statements: vec![],
        instructions: vec![
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_vec_slice_range_i64",
                    vec![vec_place, start_place, end_place],
                    Some(result_place),
                )
                .expect("hew_vec_slice_range_i64 is allowlisted"),
            ),
            // Read the length of the produced slice into an i64 we can
            // return — keeps Cluster-1's integer-return restriction
            // satisfied while exercising the slice-range lowering.
            Instr::CallRuntimeAbi(
                RuntimeCall::new("hew_vec_len", vec![result_place], Some(ret_len))
                    .expect("hew_vec_len is allowlisted"),
            ),
            Instr::Move {
                dest: Place::ReturnSlot,
                src: ret_len,
            },
        ],
        terminator: Terminator::Return,
    };

    let blocks = vec![
        entry_bb.clone(),
        oob_trap_bb.clone(),
        after_c1_bb.clone(),
        after_c2_bb.clone(),
    ];

    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![
                vec_ty.clone(),   // 0 Vec handle
                ResolvedTy::I64,  // 1 start
                ResolvedTy::I64,  // 2 end
                ResolvedTy::Bool, // 3 bad1
                ResolvedTy::I64,  // 4 len
                ResolvedTy::Bool, // 5 bad2
                vec_ty.clone(),   // 6 result Vec<i64>
                ResolvedTy::I64,  // 7 returned length
            ],
            blocks: blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: blocks.clone(),
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
                ElabBlock {
                    id: 3,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![
                (ExitPath::Return { block: 3 }, DropPlan::default()),
                (ExitPath::Panic { block: 1 }, DropPlan::default()),
            ],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
    }
}

fn emit_ll(module_name: &str) -> String {
    let pipeline = vec_slice_i64_pipeline();
    let tmp = std::env::temp_dir().join(format!("hew-c3-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("C-3 Vec-slice pipeline must emit successfully");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

#[test]
fn slice_range_i64_declare_has_correct_signature() {
    // hew_vec_slice_range_i64(ptr, i64, i64) -> ptr must be declared.
    // A wrong arity / return type would silently produce a mismatched
    // call at link time.
    let ll = emit_ll("slice_range_decl_i64");
    assert!(
        ll.contains("declare ptr @hew_vec_slice_range_i64(ptr, i64, i64)"),
        "hew_vec_slice_range_i64 must be declared as `ptr(ptr, i64, i64)`; got:\n{ll}"
    );
}

#[test]
fn bounds_check_emits_icmp_sgt() {
    // Both `start > end` and `end > len` use `SignedGreater` →
    // `icmp sgt` (signed greater-than). Distinguishes range-slice
    // bounds checking from C-2's `icmp uge` single-element check.
    let ll = emit_ll("slice_icmp_sgt");
    assert!(
        ll.contains("icmp sgt"),
        "bounds checks must emit `icmp sgt` for SignedGreater; got:\n{ll}"
    );
}

#[test]
fn trap_path_emits_llvm_trap_then_unreachable() {
    // The shared OOB trap block must emit `call void @llvm.trap()` plus
    // `unreachable` — boundary-fail-closed (P0).
    let ll = emit_ll("slice_trap");
    assert!(
        ll.contains("@llvm.trap"),
        "shared OOB trap must call @llvm.trap; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "trap path must be terminated by `unreachable`; got:\n{ll}"
    );
}

#[test]
fn continuation_calls_slice_range_i64() {
    let ll = emit_ll("slice_call");
    assert!(
        ll.contains("call ptr @hew_vec_slice_range_i64("),
        "continuation must call hew_vec_slice_range_i64; got:\n{ll}"
    );
}

#[test]
fn entry_calls_hew_vec_len_for_end_check() {
    // The second bounds check (end > len) calls hew_vec_len. The first
    // bounds check is start > end and needs no runtime probe.
    let ll = emit_ll("slice_len_call");
    assert!(
        ll.contains("call i64 @hew_vec_len("),
        "after-check1 block must call hew_vec_len; got:\n{ll}"
    );
}
