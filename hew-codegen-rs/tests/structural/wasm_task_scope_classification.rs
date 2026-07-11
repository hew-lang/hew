//! WASM parity classification for the `scope {}` structured-concurrency
//! substrate.
//!
//! `hew-runtime/src/task_scope.rs` is gated `cfg(not(target_arch = "wasm32"))`
//! in `hew-runtime/src/lib.rs`. The native impl launches `TaskEntry` work on OS
//! threads and completes joins through thread/condvar and read-slot wakeups.
//! wasm32 has no cooperative task work queue or non-blocking scope join. If
//! `emit_module` proceeded with WASM emission, `wasm-ld` would fail with an
//! undefined `hew_task_scope_*` or `hew_task_*` symbol.
//!
//! Defence in depth — the type checker (`hew-types/src/check/expressions.rs`)
//! rejects `Expr::Scope { .. }` on WASM targets via
//! `WasmUnsupportedFeature::StructuredConcurrency`. These tests verify the
//! codegen-level gate catches any direct-MIR path that bypasses the type
//! checker (e.g. test pipelines built straight against the MIR APIs).
//!
//! WASM-TODO(#1451): task-scope WASM parity sub-task under the umbrella issue.
//!
//! LESSONS: boundary-fail-closed (P0), parity-or-tracked-gap (P0),
//! user-surface-correctness (P0).

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, SelectArm,
    SelectArmKind, Terminator,
};
use hew_types::ResolvedTy;

fn task_scope_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "HewTaskScope".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

/// Build a minimal pipeline that opens a `HewTaskScope` via `hew_task_scope_new`
/// — the lead instruction emitted by `lower_task_scope` (hew-mir/src/lower.rs:7577).
/// Any wasm-target codegen of this pipeline must fail-closed with the
/// `WasmUnsupportedSubstrate` diagnostic before invoking `wasm-ld`.
fn pipeline_with_task_scope_new_call() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::CallRuntimeAbi(
            RuntimeCall::new("hew_task_scope_new", vec![], Some(Place::DuplexHandle(0)))
                .expect("hew_task_scope_new is on the runtime allowlist"),
        )],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![task_scope_ty()],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw_blocks,
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
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn pipeline_with_task_new_call() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::CallRuntimeAbi(
            RuntimeCall::new("hew_task_new", vec![], Some(Place::DuplexHandle(0)))
                .expect("hew_task_new is on the runtime allowlist"),
        )],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![ResolvedTy::Named {
                name: "HewTask".to_string(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            }],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw_blocks,
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
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn pipeline_with_spawn_task_instruction() -> IrPipeline {
    let mut pipeline = pipeline_with_task_new_call();
    pipeline.raw_mir[0].blocks[0].instructions = vec![Instr::SpawnTaskDirect {
        task: Place::DuplexHandle(0),
        callee_symbol: "worker".to_string(),
    }];
    pipeline.checked_mir[0].blocks[0].instructions = vec![Instr::SpawnTaskDirect {
        task: Place::DuplexHandle(0),
        callee_symbol: "worker".to_string(),
    }];
    pipeline
}

fn pipeline_with_task_await_select() -> IrPipeline {
    let mut pipeline = pipeline_with_task_new_call();
    let select = Terminator::Select {
        arms: vec![SelectArm {
            kind: SelectArmKind::TaskAwait {
                task: Place::DuplexHandle(0),
            },
            body_block: 0,
            binding: Some(Place::DuplexHandle(0)),
        }],
        next: 0,
    };
    pipeline.raw_mir[0].blocks[0].instructions.clear();
    pipeline.raw_mir[0].blocks[0].terminator = select.clone();
    pipeline.checked_mir[0].blocks[0].instructions.clear();
    pipeline.checked_mir[0].blocks[0].terminator = select;
    pipeline
}

fn out_dir(suffix: &str) -> std::path::PathBuf {
    let d = std::env::temp_dir().join(format!("hew-wasm-task-scope-{suffix}"));
    std::fs::create_dir_all(&d).expect("create out_dir");
    d
}

/// `hew_task_scope_new` on a wasm target must surface
/// `CodegenError::WasmUnsupportedSubstrate` before `wasm-ld` runs.
#[test]
fn task_scope_new_call_blocks_wasm_emission() {
    let pipeline = pipeline_with_task_scope_new_call();
    let dir = out_dir("new-wasm-block");
    let options = EmitOptions {
        module_name: "task_scope_new_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert!(
                symbol.starts_with("hew_task_scope_"),
                "WasmUnsupportedSubstrate symbol must be a task_scope symbol; got: {symbol}"
            );
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate error for hew_task_scope_new call with \
             wasm: true, but emit_module succeeded"
        ),
        Err(other) => {
            panic!("expected WasmUnsupportedSubstrate, got a different error: {other}")
        }
    }
}

/// Direct task ABI calls must fail closed even without a preceding scope symbol.
#[test]
fn task_new_call_blocks_wasm_emission() {
    let pipeline = pipeline_with_task_new_call();
    let dir = out_dir("task-new-wasm-block");
    let options = EmitOptions {
        module_name: "task_new_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert_eq!(symbol, "hew_task_new");
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate error for hew_task_new call with \
             wasm: true, but emit_module succeeded"
        ),
        Err(other) => {
            panic!("expected WasmUnsupportedSubstrate, got a different error: {other}")
        }
    }
}

/// `SpawnTaskDirect` synthesizes
/// `hew_task_spawn_thread_with_inherited_context` during LLVM emission, so the
/// pre-link scan must reject the MIR instruction itself.
#[test]
fn spawn_task_instruction_blocks_wasm_emission() {
    let pipeline = pipeline_with_spawn_task_instruction();
    let dir = out_dir("spawn-instr-wasm-block");
    let options = EmitOptions {
        module_name: "spawn_task_instr_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert_eq!(symbol, "hew_task_spawn_thread_with_inherited_context");
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate error for SpawnTaskDirect with \
             wasm: true, but emit_module succeeded"
        ),
        Err(other) => {
            panic!("expected WasmUnsupportedSubstrate, got a different error: {other}")
        }
    }
}

/// A `TaskAwait` select arm emits `hew_task_completion_observe`; the wasm
/// pre-link scan must reject the arm before LLVM can reference that symbol.
#[test]
fn task_await_select_arm_blocks_wasm_emission() {
    let pipeline = pipeline_with_task_await_select();
    let dir = out_dir("task-await-select-wasm-block");
    let options = EmitOptions {
        module_name: "task_await_select_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert_eq!(symbol, "hew_task_completion_observe");
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate error for TaskAwait select arm with \
             wasm: true, but emit_module succeeded"
        ),
        Err(other) => {
            panic!("expected WasmUnsupportedSubstrate, got a different error: {other}")
        }
    }
}

/// The diagnostic message MUST name `scope {}` (the source-level construct)
/// and reference the tracking marker (`WASM-TODO(#1451)`). Asserts the
/// Display string — users see this; brittle wording is intentional
/// (regression guard).
#[test]
fn task_scope_wasm_diagnostic_message_names_scope_construct() {
    let pipeline = pipeline_with_task_scope_new_call();
    let dir = out_dir("diagnostic-message");
    let options = EmitOptions {
        module_name: "task_scope_diag",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let err =
        emit_module(&pipeline, &options).expect_err("expected WasmUnsupportedSubstrate; got Ok(_)");
    let msg = format!("{err}");
    assert!(
        msg.contains("scope {}"),
        "diagnostic must name the source-level `scope {{}}` construct; got: {msg}"
    );
    // Use the parenthesised form here so the lint-wasm-todo-issue-ref.sh
    // gate does not flag this test for a bare marker.
    assert!(
        msg.contains("WASM-TODO(#1451)"),
        "diagnostic must reference WASM-TODO(#1451); got: {msg}"
    );
    assert!(
        msg.contains("hew_task_scope_new"),
        "diagnostic must name the offending C-ABI symbol; got: {msg}"
    );
}

/// Native emission of the same pipeline must still succeed (no regression on
/// the native path for the canonical substrate).
#[test]
fn task_scope_native_emission_unaffected() {
    let pipeline = pipeline_with_task_scope_new_call();
    let dir = out_dir("native-ok");
    let options = EmitOptions {
        module_name: "task_scope_native_ok",
        out_dir: &dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    emit_module(&pipeline, &options)
        .expect("native (no-wasm) emission of hew_task_scope_new must succeed");
}
