use std::path::Path;

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, CheckedMirFunction, CooperateKind, CooperateSite, Instr, IrPipeline, Place,
    RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn emit_ll_result(pipeline: &IrPipeline, module_name: &str) -> Result<String, CodegenError> {
    let tmp = std::env::temp_dir().join(format!("hew-cooperate-{module_name}"));
    std::fs::create_dir_all(&tmp)?;
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(pipeline, &options)?;
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).map_err(CodegenError::from)
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    emit_ll_result(pipeline, module_name).expect("cooperate pipeline must emit")
}

fn loop_pipeline_with_sites(cooperate_sites: Vec<CooperateSite>) -> IrPipeline {
    let return_ty = ResolvedTy::I64;
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 1,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Goto { target: 0 },
        },
    ];
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            params: Vec::new(),
            locals: vec![return_ty.clone()],
            blocks: blocks.clone(),
            decisions: Vec::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty,
            blocks,
            decisions: Vec::new(),
            checks: Vec::new(),
            cooperate_sites,
        }],
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        record_layouts: Vec::new(),
    }
}

fn loop_pipeline() -> IrPipeline {
    loop_pipeline_with_sites(vec![
        CooperateSite {
            bb_id: 0,
            kind: CooperateKind::FunctionEntry,
        },
        CooperateSite {
            bb_id: 1,
            kind: CooperateKind::LoopBackEdge,
        },
    ])
}

fn expect_fail_closed(pipeline: &IrPipeline, module_name: &str) -> String {
    match emit_ll_result(pipeline, module_name) {
        Err(CodegenError::FailClosed(msg)) => msg,
        Err(other) => panic!("expected CodegenError::FailClosed, got {other:?}"),
        Ok(_) => panic!("expected codegen to fail closed, got Ok(_)"),
    }
}

#[test]
fn loop_cooperate_sites_emit_runtime_calls() {
    let ll = emit_ll(&loop_pipeline(), "loop_sites");

    assert!(
        ll.contains("declare i32 @hew_actor_cooperate()"),
        "cooperate emission must declare the runtime ABI with c_int return;\n--- IR ---\n{ll}"
    );
    assert_eq!(
        ll.matches("call i32 @hew_actor_cooperate()").count(),
        2,
        "function-entry and loop back-edge sites must each emit one cooperate call;\n--- IR ---\n{ll}"
    );

    let entry_call = ll
        .find("call i32 @hew_actor_cooperate()")
        .expect("entry cooperate call");
    let first_mir_block = ll.find("bb0:").expect("first MIR block");
    assert!(
        entry_call < first_mir_block,
        "function-entry cooperate call must precede the first MIR block;\n--- IR ---\n{ll}"
    );

    let back_edge_block = ll.find("bb1:").expect("back-edge block");
    let back_edge_call = ll[back_edge_block..]
        .find("call i32 @hew_actor_cooperate()")
        .map(|offset| back_edge_block + offset)
        .expect("loop back-edge cooperate call");
    let back_edge_branch = ll[back_edge_block..]
        .find("br label %bb0")
        .map(|offset| back_edge_block + offset)
        .expect("loop back-edge branch");
    assert!(
        back_edge_call < back_edge_branch,
        "loop back-edge cooperate call must precede the back-edge branch;\n--- IR ---\n{ll}"
    );
}

#[test]
fn empty_cooperate_sites_emit_no_runtime_calls() {
    let ll = emit_ll(&loop_pipeline_with_sites(Vec::new()), "empty_sites");

    assert!(
        !ll.contains("call i32 @hew_actor_cooperate()"),
        "empty cooperate_sites must not emit cooperate calls;\n--- IR ---\n{ll}"
    );
    assert!(
        !ll.contains("declare i32 @hew_actor_cooperate()"),
        "empty cooperate_sites must not declare unused cooperate runtime ABI;\n--- IR ---\n{ll}"
    );
}

#[test]
fn function_entry_only_emits_single_cooperate_at_entry() {
    let ll = emit_ll(
        &loop_pipeline_with_sites(vec![CooperateSite {
            bb_id: 0,
            kind: CooperateKind::FunctionEntry,
        }]),
        "entry_only",
    );

    assert_eq!(
        ll.matches("call i32 @hew_actor_cooperate()").count(),
        1,
        "FunctionEntry-only cooperate_sites must emit exactly one cooperate call;\n--- IR ---\n{ll}"
    );
    let prologue = ll.find("entry:").expect("LLVM prologue entry block");
    let entry_call = ll
        .find("call i32 @hew_actor_cooperate()")
        .expect("entry cooperate call");
    let first_mir_block = ll.find("bb0:").expect("first MIR block");
    assert!(
        prologue < entry_call && entry_call < first_mir_block,
        "FunctionEntry cooperate call must be in the LLVM entry block before bb0;\n--- IR ---\n{ll}"
    );
}

#[test]
fn function_entry_on_non_entry_bb_fails_closed() {
    let msg = expect_fail_closed(
        &loop_pipeline_with_sites(vec![CooperateSite {
            bb_id: 1,
            kind: CooperateKind::FunctionEntry,
        }]),
        "function_entry_non_entry_bb",
    );

    assert!(
        msg.contains("FunctionEntry") && msg.contains("bb1") && msg.contains("bb0"),
        "FailClosed must identify the malformed FunctionEntry site and expected entry bb; got: {msg}"
    );
}

#[test]
fn loop_back_edge_on_missing_bb_fails_closed() {
    let msg = expect_fail_closed(
        &loop_pipeline_with_sites(vec![CooperateSite {
            bb_id: 99,
            kind: CooperateKind::LoopBackEdge,
        }]),
        "loop_back_edge_missing_bb",
    );

    assert!(
        msg.contains("LoopBackEdge") && msg.contains("bb99"),
        "FailClosed must identify the malformed LoopBackEdge site; got: {msg}"
    );
}

#[test]
fn missing_checked_mir_function_fails_closed() {
    let mut pipeline = loop_pipeline();
    pipeline.checked_mir[0].name = "other".to_string();

    let msg = expect_fail_closed(&pipeline, "missing_checked_mir_function");

    assert!(
        msg.contains("main") && msg.contains("no matching checked MIR"),
        "FailClosed must identify the raw_mir function missing checked MIR; got: {msg}"
    );
}
