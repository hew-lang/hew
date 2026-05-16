//! Codegen-side fail-closed coverage for `Instr::CallRuntimeAbi`.
//!
//! Slice 4.5c lands the variant + the allowlist + the codegen
//! match arm. Until slice 5 wires the real `inkwell::BuildCall`
//! lowering, any function whose instruction stream contains
//! `Instr::CallRuntimeAbi` must surface `CodegenError::FailClosed`
//! mentioning the symbol — never a silent no-op that would skip
//! the runtime call. This test pins that contract by hand-building
//! a minimal `IrPipeline` and asserting `emit_module` fails closed
//! with the symbol in the error message.
//!
//! LESSONS: boundary-fail-closed (P0 row 49) — every new `Instr`
//! variant must have a codegen-side fail-closed arm landed in the
//! same commit (returning `CodegenError::FailClosed` until the
//! real lowering wires).

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
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::CallRuntimeAbi(
            hew_mir::RuntimeCall::new(symbol, vec![Place::DuplexHandle(0), Place::Local(0)], None)
                .expect("test helper called with known runtime symbol"),
        )],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            locals: vec![ResolvedTy::I64],
            blocks: raw_blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw_blocks.clone(),
            decisions: vec![],
            checks: vec![],
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
    }
}

/// A pipeline whose function emits `Instr::CallRuntimeAbi` must
/// fail codegen with `CodegenError::FailClosed`. The error message
/// must include the runtime symbol so the rejection points at the
/// load-bearing seam (boundary-fail-closed evidence anchor).
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

/// Repeat for the lambda-actor lifecycle symbol the producer for
/// `SpawnLambdaActor` will emit at drop time. Pin that the
/// fail-closed arm is symbol-agnostic — every allowed symbol must
/// hit the same FailClosed shape, not just `hew_duplex_send`.
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
