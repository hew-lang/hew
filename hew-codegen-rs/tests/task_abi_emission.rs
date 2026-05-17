//! Codegen coverage for `intern_runtime_decl` declarations of the
//! task/scope ABI symbols added in Phase 2 (inventory rows 2/3/4).
//!
//! Each test drives `emit_module` with a hand-built `IrPipeline` containing
//! a `Instr::CallRuntimeAbi` for one of the new symbols and asserts the
//! emitted LLVM IR contains the expected `declare` signature. Passing
//! `native: false, wasm: false` skips the `hew-emit-v05` back-half so
//! these run purely in-process.
//!
//! `hew_task_spawn_thread` is NOT tested here — its `lower_call_runtime_abi`
//! arm is intentionally fail-closed pending the spawn-producer landing (the
//! fn-ptr Place convention is undecided). Its `intern_runtime_decl` entry is
//! covered by the allowlist presence test in `hew-mir`.
//!
//! LESSONS: boundary-fail-closed (P0 row 49), parity-or-tracked-gap.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, Terminator,
};
use hew_types::ResolvedTy;

/// Build a minimal `IrPipeline` containing one function with a single
/// `Instr::CallRuntimeAbi`. Mirrors the shape `lower_hir_module` produces.
///
/// `symbol` — the C-ABI symbol to call.
/// `args` — Place arguments to pass.
/// `dest` — optional destination Place.
/// `extra_locals` — additional local types beyond the one pre-allocated ptr local.
fn pipeline_with_task_abi_call(
    symbol: &str,
    args: Vec<Place>,
    dest: Option<Place>,
    extra_locals: Vec<ResolvedTy>,
) -> IrPipeline {
    let mut locals = vec![ResolvedTy::Named {
        name: "HewTask".to_string(),
        args: vec![],
    }];
    locals.extend(extra_locals);

    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::CallRuntimeAbi(
            RuntimeCall::new(symbol, args, dest)
                .unwrap_or_else(|e| panic!("symbol {symbol} not on allowlist: {e}")),
        )],
        terminator: Terminator::Return,
    }];

    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            params: vec![],
            locals: locals.clone(),
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
    }
}

fn read_ll(out_dir: &std::path::Path) -> String {
    let ll = out_dir.join("probe.ll");
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("could not read {}: {e}", ll.display()))
}

/// `hew_task_new` must produce a `declare ptr @hew_task_new()` in the IR
/// and emit a call that stores the result into the dest place.
#[test]
fn task_abi_emission_task_new_declare() {
    // dest: Place::DuplexHandle(0) — the only ptr-typed alloca we have.
    let pipeline =
        pipeline_with_task_abi_call("hew_task_new", vec![], Some(Place::DuplexHandle(0)), vec![]);
    let tmp = std::env::temp_dir().join("hew-task-abi-task-new");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("hew_task_new emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_new"),
        "emitted IR must declare @hew_task_new; got:\n{ir}",
    );
}

/// `hew_task_await_blocking` must produce a `declare ptr @hew_task_await_blocking(ptr)`
/// and emit a call storing the result (dest supplied).
#[test]
fn task_abi_emission_task_await_blocking_declare() {
    // args[0]: task ptr from DuplexHandle(0). dest: DuplexHandle(0) reused
    // for result (producers would use separate locals; this just pins the shape).
    let pipeline = pipeline_with_task_abi_call(
        "hew_task_await_blocking",
        vec![Place::DuplexHandle(0)],
        Some(Place::DuplexHandle(0)),
        vec![],
    );
    let tmp = std::env::temp_dir().join("hew-task-abi-task-await-blocking");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("hew_task_await_blocking emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_await_blocking"),
        "emitted IR must declare @hew_task_await_blocking; got:\n{ir}",
    );
}

/// `hew_task_get_result` must produce a `declare ptr @hew_task_get_result(ptr)`
/// and store the result into dest.
#[test]
fn task_abi_emission_task_get_result_declare() {
    let pipeline = pipeline_with_task_abi_call(
        "hew_task_get_result",
        vec![Place::DuplexHandle(0)],
        Some(Place::DuplexHandle(0)),
        vec![],
    );
    let tmp = std::env::temp_dir().join("hew-task-abi-task-get-result");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("hew_task_get_result emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_get_result"),
        "emitted IR must declare @hew_task_get_result; got:\n{ir}",
    );
}

/// `hew_task_free` must produce a `declare void @hew_task_free(ptr)`
/// and emit a void call (dest: None).
#[test]
fn task_abi_emission_task_free_declare() {
    let pipeline =
        pipeline_with_task_abi_call("hew_task_free", vec![Place::DuplexHandle(0)], None, vec![]);
    let tmp = std::env::temp_dir().join("hew-task-abi-task-free");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("hew_task_free emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_free"),
        "emitted IR must declare @hew_task_free; got:\n{ir}",
    );
}

/// `hew_scope_spawn` must produce a `declare i32 @hew_scope_spawn(ptr, ptr)`
/// and emit a void call (dest: None — the i32 return is discarded).
#[test]
fn task_abi_emission_scope_spawn_declare() {
    let pipeline = pipeline_with_task_abi_call(
        "hew_scope_spawn",
        vec![Place::DuplexHandle(0), Place::DuplexHandle(0)],
        None,
        vec![],
    );
    let tmp = std::env::temp_dir().join("hew-task-abi-scope-spawn");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("hew_scope_spawn emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_scope_spawn"),
        "emitted IR must declare @hew_scope_spawn; got:\n{ir}",
    );
}

/// `hew_task_spawn_thread` is fail-closed — no MIR producer has landed yet
/// and the fn-ptr Place convention is undecided (inventory row 3).
#[test]
fn task_abi_emission_task_spawn_thread_fail_closed() {
    use hew_codegen_rs::CodegenError;

    let pipeline = pipeline_with_task_abi_call(
        "hew_task_spawn_thread",
        vec![Place::DuplexHandle(0), Place::DuplexHandle(0)],
        None,
        vec![],
    );
    let tmp = std::env::temp_dir().join("hew-task-abi-spawn-thread-fail");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("hew_task_spawn_thread"),
                "FailClosed message must name the symbol; got: {msg}",
            );
        }
        Err(other) => {
            panic!("expected CodegenError::FailClosed for hew_task_spawn_thread; got {other:?}")
        }
        Ok(_) => panic!(
            "hew_task_spawn_thread arm must be fail-closed until the spawn \
             producer lands; got Ok(_)"
        ),
    }
}
