//! Codegen-side fail-closed coverage for `Instr::CallRuntimeAbi`.
//!
//! E4 wires real `LLVMBuildCall` emission for `hew_duplex_pair` and
//! `hew_duplex_send` (and the close ritual for `hew_duplex_close`
//! via `Instr::Drop`). The happy-path coverage lives in
//! `emit_duplex_pair.rs`. This file pins the **remaining**
//! boundary-fail-closed contracts that must survive every revision:
//!
//! 1. A symbol-shape mismatch (wrong arg count for a wired symbol)
//!    still surfaces `CodegenError::FailClosed` naming the symbol —
//!    never a silent miscompile of a partially-typed call.
//! 2. M2-allowlist symbols not yet wired in codegen (today:
//!    `hew_lambda_actor_release`) remain fail-closed so the producer
//!    surface cannot outrun the codegen surface.
//!
//! LESSONS: boundary-fail-closed (P0 row 49), parity-or-tracked-gap.

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

/// Build a minimal `IrPipeline` containing one function with a
/// single `Instr::CallRuntimeAbi` in its instruction stream. The
/// helper mirrors the shape `lower_hir_module` produces: ladder
/// stages all present, drop plans empty, decisions empty.
fn pipeline_with_call_runtime_abi(symbol: &str) -> IrPipeline {
    pipeline_with_call_runtime_abi_parts(
        symbol,
        vec![Place::DuplexHandle(0), Place::Local(0)],
        None,
        vec![ResolvedTy::I64],
    )
}

fn pipeline_with_call_runtime_abi_parts(
    symbol: &str,
    args: Vec<Place>,
    dest: Option<Place>,
    locals: Vec<ResolvedTy>,
) -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::CallRuntimeAbi(
            hew_mir::RuntimeCall::new(symbol, args, dest)
                .expect("test helper called with known runtime symbol"),
        )],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals,
            blocks: raw_blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw_blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
    }
}

fn local_pid_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::Named {
            name: "Probe".to_string(),
            args: vec![],
        }],
    }
}

/// A wired symbol (`hew_duplex_send`) called with the wrong arg count
/// (here 2 instead of 3) must still surface `CodegenError::FailClosed`
/// naming the symbol — codegen rejects shape mismatches loudly rather
/// than silently emitting a partial call. The happy-path 3-arg shape
/// is covered by `emit_duplex_pair.rs`.
#[test]
fn call_runtime_abi_fails_closed_with_symbol_in_message() {
    let pipeline = pipeline_with_call_runtime_abi("hew_duplex_send");
    let tmp = std::env::temp_dir().join("hew-mir-4-5c-fail-closed");
    let options = EmitOptions {
        module_name: "fail_closed_probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("hew_duplex_send"),
                "FailClosed message must name the runtime symbol so the \
                 rejection points at the unwired seam; got: {msg}",
            );
            assert!(
                msg.contains("CallRuntimeAbi"),
                "FailClosed message must reference the Instr variant so \
                 future readers see the construction site; got: {msg}",
            );
        }
        Err(other) => panic!(
            "expected CodegenError::FailClosed, got {other:?}; the \
             CallRuntimeAbi arm must fail-closed BEFORE any LLVM \
             verification or process-spawn step"
        ),
        Ok(_) => panic!(
            "expected codegen to fail closed on Instr::CallRuntimeAbi; \
             got Ok(_). The variant landed without a fail-closed match \
             arm — boundary-fail-closed regression"
        ),
    }
}

/// `hew_lambda_actor_release` is on the M2 allowlist but has no codegen
/// lowering arm yet (the lambda-actor lane wires it). Until then, any
/// `Instr::CallRuntimeAbi` for this symbol must surface
/// `CodegenError::FailClosed` — the producer surface MUST NOT outrun
/// the codegen surface (LESSONS `parity-or-tracked-gap`).
#[test]
fn call_runtime_abi_fails_closed_for_lambda_actor_release() {
    let pipeline = pipeline_with_call_runtime_abi("hew_lambda_actor_release");
    let tmp = std::env::temp_dir().join("hew-mir-4-5c-fail-closed-lambda");
    let options = EmitOptions {
        module_name: "fail_closed_probe_lambda",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("hew_lambda_actor_release"),
                "FailClosed must name the lambda-actor lifecycle symbol; got: {msg}",
            );
        }
        Err(other) => {
            panic!("expected CodegenError::FailClosed for lambda-actor symbol; got {other:?}")
        }
        Ok(_) => panic!("expected codegen to fail closed on lambda-actor CallRuntimeAbi"),
    }
}

#[test]
fn actor_link_and_monitor_runtime_calls_are_codegen_reachable_without_dest() {
    for symbol in ["hew_actor_link", "hew_actor_monitor"] {
        let pipeline = pipeline_with_call_runtime_abi_parts(
            symbol,
            vec![Place::ActorHandle(0), Place::ActorHandle(1)],
            None,
            vec![local_pid_ty(), local_pid_ty()],
        );
        let tmp = std::env::temp_dir().join(format!("hew-mir-link-monitor-{symbol}"));
        let options = EmitOptions {
            module_name: "link_monitor_probe",
            out_dir: &tmp,
            native: false,
            wasm: false,
        };
        let result = emit_module(&pipeline, &options);
        assert!(
            result.is_ok(),
            "{symbol} CallRuntimeAbi with dest=None must reach the codegen consumer; got {result:?}"
        );
    }
}

#[test]
fn actor_monitor_runtime_call_with_dest_still_fails_closed() {
    let pipeline = pipeline_with_call_runtime_abi_parts(
        "hew_actor_monitor",
        vec![Place::ActorHandle(0), Place::ActorHandle(1)],
        Some(Place::Local(2)),
        vec![local_pid_ty(), local_pid_ty(), ResolvedTy::I64],
    );
    let tmp = std::env::temp_dir().join("hew-mir-monitor-dest-fail-closed");
    let options = EmitOptions {
        module_name: "monitor_dest_probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("hew_actor_monitor") && msg.contains("MonitorRef"),
                "dest-slot fail-closed message must name hew_actor_monitor and MonitorRef; got: {msg}"
            );
        }
        Err(other) => panic!("expected monitor dest fail-closed, got {other:?}"),
        Ok(_) => panic!("expected monitor CallRuntimeAbi with dest to fail closed"),
    }
}
