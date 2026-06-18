//! Codegen-side fail-closed coverage for `Instr::CallRuntimeAbi`.
//!
//! E4 wires real `LLVMBuildCall` emission for `hew_duplex_pair` and
//! `hew_duplex_send` (and the close ritual for `hew_duplex_close`
//! via `Instr::Drop`). The M2 lambda-actor lane extends that to
//! `hew_lambda_actor_{release, send, ask}`. The happy-path coverage
//! lives in `emit_duplex_pair.rs`. This file pins the **remaining**
//! boundary-fail-closed contracts that must survive every revision:
//!
//! 1. A symbol-shape mismatch (wrong arg count for a wired symbol)
//!    still surfaces `CodegenError::FailClosed` naming the symbol —
//!    never a silent miscompile of a partially-typed call.
//! 2. `hew_lambda_actor_release` reaches the codegen consumer
//!    successfully (Ok(_)) — the M2 lane landed; pre-lane this was a
//!    fail-closed pin, post-lane it is an Ok-pin so the parity
//!    invariant flips with the wire-up.
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
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::HashMap::new(),
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
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

fn local_pid_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::Named {
            name: "Probe".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }],
        builtin: None,
        is_opaque: false,
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
        target_triple: None,
        debug: false,
        source_path: None,
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

/// `hew_lambda_actor_release` (the M2 lambda-actor lifecycle close
/// symbol) now has a real codegen lowering arm — the M2 lambda-actor
/// lane wires `release` (and `send`/`ask`) as `CallRuntimeAbi`
/// targets, joining `hew_duplex_send` on the happy-path codegen
/// surface. This regression pins the lane's invariant:
/// `CallRuntimeAbi("hew_lambda_actor_release", [LambdaActorHandle(0)], None)`
/// must reach the codegen consumer successfully (`Ok(_)`). The
/// producer/codegen parity is held by `fn_type_for_symbol` declaring
/// the i32→ptr signature and the matching arm in `lower_call_runtime_abi`.
///
/// LESSONS: parity-or-tracked-gap (P0). Inverted from a fail-closed
/// pin to an Ok-pin when the M2 lane landed.
#[test]
fn call_runtime_abi_succeeds_for_lambda_actor_release() {
    let pipeline = pipeline_with_call_runtime_abi_parts(
        "hew_lambda_actor_release",
        vec![Place::LambdaActorHandle(0)],
        None,
        vec![ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::Unit, ResolvedTy::Unit],
            builtin: None,
            is_opaque: false,
        }],
    );
    let tmp = std::env::temp_dir().join("hew-mir-4-5c-ok-lambda-release");
    let options = EmitOptions {
        module_name: "ok_probe_lambda_release",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let result = emit_module(&pipeline, &options);
    assert!(
        result.is_ok(),
        "hew_lambda_actor_release CallRuntimeAbi with one LambdaActorHandle arg \
         and dest=None must reach the codegen consumer (M2 lane landed); got {result:?}"
    );
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
            target_triple: None,
            debug: false,
            source_path: None,
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
        target_triple: None,
        debug: false,
        source_path: None,
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
