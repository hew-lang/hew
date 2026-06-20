//! W2.006 Stage 1 — WASM parity classification for the `scope {}` structured-
//! concurrency substrate.
//!
//! `hew-runtime/src/task_scope.rs` is gated `cfg(not(target_arch = "wasm32"))`
//! at `hew-runtime/src/lib.rs:466`. The native impl depends on OS-thread-backed
//! join semantics that wasm32 lacks. If `emit_module` proceeded with WASM
//! emission for a `scope {}` program, `wasm-ld` would fail with
//! `undefined symbol: hew_task_scope_*`.
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
    ExitPath, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, Terminator,
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
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![task_scope_ty()],
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),

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
    }
}

fn pipeline_with_task_scope_spawn_call() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::CallRuntimeAbi(
                RuntimeCall::new("hew_task_new", vec![], Some(Place::DuplexHandle(0)))
                    .expect("hew_task_new is on the runtime allowlist"),
            ),
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_task_scope_spawn",
                    vec![Place::DuplexHandle(0), Place::DuplexHandle(0)],
                    None,
                )
                .expect("hew_task_scope_spawn is on the runtime allowlist"),
            ),
        ],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
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
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),

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
    }
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

/// `hew_task_scope_spawn` (used by lower_spawned_fn_task / lower_spawned_closure_task)
/// must also block WASM emission.
#[test]
fn task_scope_spawn_call_blocks_wasm_emission() {
    let pipeline = pipeline_with_task_scope_spawn_call();
    let dir = out_dir("spawn-wasm-block");
    let options = EmitOptions {
        module_name: "task_scope_spawn_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
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
            "expected WasmUnsupportedSubstrate error for hew_task_scope_spawn call with \
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
        source_path: None,
    };
    emit_module(&pipeline, &options)
        .expect("native (no-wasm) emission of hew_task_scope_new must succeed");
}
