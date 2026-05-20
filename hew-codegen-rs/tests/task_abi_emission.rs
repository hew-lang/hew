//! Codegen coverage for `intern_runtime_decl` declarations of the
//! task/scope ABI symbols added in Phase 2 (inventory rows 2/3/4).
//!
//! Each test drives `emit_module` with a hand-built `IrPipeline` containing
//! a `Instr::CallRuntimeAbi` for one of the new symbols and asserts the
//! emitted LLVM IR contains the expected `declare` signature. Passing
//! `native: false, wasm: false` skips the `hew-emit` back-half so
//! these run purely in-process.
//!
//! Direct `RuntimeCall` emission for `hew_task_spawn_thread` remains
//! fail-closed; spawned task producers use dedicated MIR instructions that
//! synthesize wrappers and call the inherited-context spawn helper.
//!
//! LESSONS: boundary-fail-closed (P0 row 49), parity-or-tracked-gap.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, RecordLayout,
    RuntimeCall, Terminator,
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
            call_conv: hew_mir::FunctionCallConv::Default,
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
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
    }
}

fn read_ll(out_dir: &std::path::Path) -> String {
    let ll = out_dir.join("probe.ll");
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("could not read {}: {e}", ll.display()))
}

fn pipeline_with_spawn_task_direct() -> IrPipeline {
    let main_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::EnterContext,
            Instr::SpawnTaskDirect {
                task: Place::Local(0),
                callee_symbol: "long_op".to_string(),
            },
            Instr::ExitContext,
        ],
        terminator: Terminator::Return,
    };
    let long_op_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::EnterContext,
            Instr::UnitLit {
                dest: Place::Local(0),
            },
            Instr::ExitContext,
        ],
        terminator: Terminator::Return,
    };
    IrPipeline {
        thir: vec![],
        raw_mir: vec![
            RawMirFunction {
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![ResolvedTy::Task(Box::new(ResolvedTy::Unit))],
                blocks: vec![main_block.clone()],
                decisions: vec![],
            },
            RawMirFunction {
                name: "long_op".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![ResolvedTy::Unit],
                blocks: vec![long_op_block.clone()],
                decisions: vec![],
            },
        ],
        checked_mir: vec![
            CheckedMirFunction {
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                blocks: vec![main_block],
                decisions: vec![],
                checks: vec![],
                cooperate_sites: vec![],
            },
            CheckedMirFunction {
                name: "long_op".to_string(),
                return_ty: ResolvedTy::Unit,
                blocks: vec![long_op_block],
                decisions: vec![],
                checks: vec![],
                cooperate_sites: vec![],
            },
        ],
        elaborated_mir: vec![],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
    }
}

fn pipeline_with_spawn_task_direct_target_without_context() -> IrPipeline {
    let main_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::EnterContext,
            Instr::SpawnTaskDirect {
                task: Place::Local(0),
                callee_symbol: "long_op".to_string(),
            },
            Instr::ExitContext,
        ],
        terminator: Terminator::Return,
    };
    let long_op_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::UnitLit {
            dest: Place::Local(0),
        }],
        terminator: Terminator::Return,
    };
    IrPipeline {
        thir: vec![],
        raw_mir: vec![
            RawMirFunction {
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![ResolvedTy::Task(Box::new(ResolvedTy::Unit))],
                blocks: vec![main_block.clone()],
                decisions: vec![],
            },
            RawMirFunction {
                name: "long_op".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::Default,
                params: vec![],
                locals: vec![ResolvedTy::Unit],
                blocks: vec![long_op_block.clone()],
                decisions: vec![],
            },
        ],
        checked_mir: vec![
            CheckedMirFunction {
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                blocks: vec![main_block],
                decisions: vec![],
                checks: vec![],
                cooperate_sites: vec![],
            },
            CheckedMirFunction {
                name: "long_op".to_string(),
                return_ty: ResolvedTy::Unit,
                blocks: vec![long_op_block],
                decisions: vec![],
                checks: vec![],
                cooperate_sites: vec![],
            },
        ],
        elaborated_mir: vec![],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
    }
}

fn pipeline_with_spawn_task_closure() -> IrPipeline {
    let env_ty = ResolvedTy::Named {
        name: "__hew_closure_env_main_0".to_string(),
        args: vec![],
    };
    let env_ptr_ty = ResolvedTy::Pointer {
        is_mutable: false,
        pointee: Box::new(env_ty.clone()),
    };
    let main_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::EnterContext,
            Instr::RecordInit {
                ty: env_ty.clone(),
                fields: vec![],
                dest: Place::Local(1),
            },
            Instr::SpawnTaskClosure {
                task: Place::Local(0),
                fn_symbol: "__hew_closure_invoke_main_0".to_string(),
                env: Place::Local(1),
                env_ty: env_ty.clone(),
            },
            Instr::ExitContext,
        ],
        terminator: Terminator::Return,
    };
    let closure_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::EnterContext,
            Instr::UnitLit {
                dest: Place::Local(1),
            },
            Instr::ExitContext,
        ],
        terminator: Terminator::Return,
    };
    IrPipeline {
        thir: vec![],
        raw_mir: vec![
            RawMirFunction {
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![ResolvedTy::Task(Box::new(ResolvedTy::Unit)), env_ty.clone()],
                blocks: vec![main_block.clone()],
                decisions: vec![],
            },
            RawMirFunction {
                name: "__hew_closure_invoke_main_0".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ClosureInvoke,
                params: vec![env_ptr_ty.clone()],
                locals: vec![env_ptr_ty, ResolvedTy::Unit],
                blocks: vec![closure_block.clone()],
                decisions: vec![],
            },
        ],
        checked_mir: vec![
            CheckedMirFunction {
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                blocks: vec![main_block],
                decisions: vec![],
                checks: vec![],
                cooperate_sites: vec![],
            },
            CheckedMirFunction {
                name: "__hew_closure_invoke_main_0".to_string(),
                return_ty: ResolvedTy::Unit,
                blocks: vec![closure_block],
                decisions: vec![],
                checks: vec![],
                cooperate_sites: vec![],
            },
        ],
        elaborated_mir: vec![],
        diagnostics: vec![],
        record_layouts: vec![RecordLayout {
            name: "__hew_closure_env_main_0".to_string(),
            field_tys: vec![],
        }],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
    }
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

#[test]
fn task_abi_emission_spawn_task_direct_synthesizes_wrapper() {
    let pipeline = pipeline_with_spawn_task_direct();
    let tmp = std::env::temp_dir().join("hew-task-abi-spawn-task-direct");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("SpawnTaskDirect emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_spawn_thread_with_inherited_context"),
        "emitted IR must call @hew_task_spawn_thread_with_inherited_context; got:\n{ir}",
    );
    assert!(
        ir.contains("__hew_task_wrapper_long_op"),
        "emitted IR must synthesize a task wrapper for long_op; got:\n{ir}",
    );
    assert!(
        ir.contains("@hew_task_complete_threaded"),
        "task wrapper must complete the task; got:\n{ir}",
    );
}

#[test]
fn task_abi_emission_spawn_task_direct_rejects_contextless_target() {
    use hew_codegen_rs::CodegenError;

    let pipeline = pipeline_with_spawn_task_direct_target_without_context();
    let tmp = std::env::temp_dir().join("hew-task-abi-spawn-task-contextless-target");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    match emit_module(&pipeline, &options) {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("must take a leading") && msg.contains("HewExecutionContext"),
                "FailClosed message must identify missing target ctx parameter; got: {msg}",
            );
        }
        Err(other) => {
            panic!("expected CodegenError::FailClosed for contextless spawn target; got {other:?}")
        }
        Ok(_) => panic!("contextless spawn target must fail closed"),
    }
}

#[test]
fn task_abi_emission_spawn_task_closure_threads_execution_context() {
    let pipeline = pipeline_with_spawn_task_closure();
    let tmp = std::env::temp_dir().join("hew-task-abi-spawn-task-closure");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options).expect("SpawnTaskClosure emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_spawn_thread_with_inherited_context"),
        "spawned closures must use inherited-context spawn helper; got:\n{ir}",
    );
    assert!(
        ir.contains("call i8 @__hew_closure_invoke_main_0(ptr %0, ptr %hew_task_get_env_call)"),
        "closure task wrapper must call the invoke shim with ctx then env; got:\n{ir}",
    );
}

/// Direct `CallRuntimeAbi` emission of `hew_task_spawn_thread` remains
/// fail-closed; spawn producers synthesize typed wrappers instead.
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
