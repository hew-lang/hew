//! Codegen coverage for `intern_runtime_decl` declarations of the
//! task/scope ABI symbols added in Phase 2 (inventory rows 2/3/4).
//!
//! Each test drives `emit_module` with a hand-built `IrPipeline` containing
//! a `Instr::CallRuntimeAbi` for one of the new symbols and asserts the
//! emitted LLVM IR contains the expected `declare` signature. Passing
//! `native: false, wasm: false` keeps these focused on textual IR only.
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
    RuntimeCall, SpawnEnvFieldOwnership, Terminator,
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
        builtin: None,
        is_opaque: false,
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
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: locals.clone(),
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
                source_origin: hew_mir::SourceOrigin::Unknown,
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![ResolvedTy::Task(Box::new(ResolvedTy::Unit))],
                local_names: Vec::new(),
                local_scopes: Vec::new(),
                local_decl_bytes: Vec::new(),
                scope_table: Vec::new(),
                blocks: vec![main_block.clone()],
                decisions: vec![],
                intrinsic_id: None,
                await_deadline_ns: std::collections::HashMap::new(),
                suspend_kinds: std::collections::HashMap::new(),

                lambda_actor_user_param_locals: Vec::new(),
                span: None,
                instr_spans: ::std::collections::BTreeMap::new(),
            },
            RawMirFunction {
                source_origin: hew_mir::SourceOrigin::Unknown,
                name: "long_op".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::TaskEntry,
                params: vec![],
                locals: vec![ResolvedTy::Unit],
                local_names: Vec::new(),
                local_scopes: Vec::new(),
                local_decl_bytes: Vec::new(),
                scope_table: Vec::new(),
                blocks: vec![long_op_block.clone()],
                decisions: vec![],
                intrinsic_id: None,
                await_deadline_ns: std::collections::HashMap::new(),
                suspend_kinds: std::collections::HashMap::new(),

                lambda_actor_user_param_locals: Vec::new(),
                span: None,
                instr_spans: ::std::collections::BTreeMap::new(),
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
                source_origin: hew_mir::SourceOrigin::Unknown,
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![ResolvedTy::Task(Box::new(ResolvedTy::Unit))],
                local_names: Vec::new(),
                local_scopes: Vec::new(),
                local_decl_bytes: Vec::new(),
                scope_table: Vec::new(),
                blocks: vec![main_block.clone()],
                decisions: vec![],
                intrinsic_id: None,
                await_deadline_ns: std::collections::HashMap::new(),
                suspend_kinds: std::collections::HashMap::new(),

                lambda_actor_user_param_locals: Vec::new(),
                span: None,
                instr_spans: ::std::collections::BTreeMap::new(),
            },
            RawMirFunction {
                source_origin: hew_mir::SourceOrigin::Unknown,
                name: "long_op".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::Default,
                params: vec![],
                locals: vec![ResolvedTy::Unit],
                local_names: Vec::new(),
                local_scopes: Vec::new(),
                local_decl_bytes: Vec::new(),
                scope_table: Vec::new(),
                blocks: vec![long_op_block.clone()],
                decisions: vec![],
                intrinsic_id: None,
                await_deadline_ns: std::collections::HashMap::new(),
                suspend_kinds: std::collections::HashMap::new(),

                lambda_actor_user_param_locals: Vec::new(),
                span: None,
                instr_spans: ::std::collections::BTreeMap::new(),
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

fn pipeline_with_spawn_task_closure() -> IrPipeline {
    let env_ty = ResolvedTy::Named {
        name: "__hew_closure_env_main_0".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
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
                fields: vec![(hew_mir::FieldOffset(0), Place::Local(2))],
                dest: Place::Local(1),
            },
            Instr::SpawnTaskClosure {
                task: Place::Local(0),
                fn_symbol: "__hew_closure_invoke_main_0".to_string(),
                env: Place::Local(1),
                env_ty: env_ty.clone(),
                env_ownership: vec![SpawnEnvFieldOwnership::BorrowsOnly],
            },
            Instr::ExitContext,
        ],
        terminator: Terminator::Return,
    };
    // ClosureInvoke shims receive the execution context as an ABI parameter;
    // they must NOT carry EnterContext/ExitContext (those are only legal on
    // scheduler-installed context boundaries such as ActorHandler and TaskEntry).
    let closure_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::UnitLit {
            dest: Place::Local(1),
        }],
        terminator: Terminator::Return,
    };
    IrPipeline {
        thir: vec![],
        raw_mir: vec![
            RawMirFunction {
                source_origin: hew_mir::SourceOrigin::Unknown,
                name: "main".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ActorHandler,
                params: vec![],
                locals: vec![
                    ResolvedTy::Task(Box::new(ResolvedTy::Unit)),
                    env_ty.clone(),
                    ResolvedTy::String,
                ],
                local_names: Vec::new(),
                local_scopes: Vec::new(),
                local_decl_bytes: Vec::new(),
                scope_table: Vec::new(),
                blocks: vec![main_block.clone()],
                decisions: vec![],
                intrinsic_id: None,
                await_deadline_ns: std::collections::HashMap::new(),
                suspend_kinds: std::collections::HashMap::new(),

                lambda_actor_user_param_locals: Vec::new(),
                span: None,
                instr_spans: ::std::collections::BTreeMap::new(),
            },
            RawMirFunction {
                source_origin: hew_mir::SourceOrigin::Unknown,
                name: "__hew_closure_invoke_main_0".to_string(),
                return_ty: ResolvedTy::Unit,
                call_conv: FunctionCallConv::ClosureInvoke,
                params: vec![env_ptr_ty.clone()],
                locals: vec![env_ptr_ty, ResolvedTy::Unit],
                local_names: Vec::new(),
                local_scopes: Vec::new(),
                local_decl_bytes: Vec::new(),
                scope_table: Vec::new(),
                blocks: vec![closure_block.clone()],
                decisions: vec![],
                intrinsic_id: None,
                await_deadline_ns: std::collections::HashMap::new(),
                suspend_kinds: std::collections::HashMap::new(),

                lambda_actor_user_param_locals: Vec::new(),
                span: None,
                instr_spans: ::std::collections::BTreeMap::new(),
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
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![RecordLayout {
            name: "__hew_closure_env_main_0".to_string(),
            field_tys: vec![ResolvedTy::String],
            field_names: vec![],
        }],
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    emit_module(&pipeline, &options).expect("hew_task_free emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_free"),
        "emitted IR must declare @hew_task_free; got:\n{ir}",
    );
}

/// Canonical `scope {}` spawn lowering emits `hew_task_new` immediately
/// followed by `hew_task_scope_spawn` (W2.006). This pins the ABI shape:
/// scope spawn requires a pre-constructed `HewTask` pointer, not a raw
/// actor pointer. The MIR producer at `hew-mir/src/lower.rs:7695-7697`
/// and `:7758-7760` is the upstream invariant; this test verifies the
/// codegen surface preserves both declarations in the emitted IR with
/// `@hew_task_new` declared before `@hew_task_scope_spawn`. Legacy
/// `@hew_scope_spawn` is removed and must not appear.
#[test]
fn task_abi_emission_task_scope_spawn_paired_with_task_new() {
    // Build a pipeline with TWO CallRuntimeAbi instructions in sequence,
    // mirroring lower_spawned_fn_task: hew_task_new(dest=task) then
    // hew_task_scope_spawn(scope, task).
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::CallRuntimeAbi(
                RuntimeCall::new("hew_task_new", vec![], Some(Place::DuplexHandle(0)))
                    .expect("hew_task_new must be on allowlist"),
            ),
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_task_scope_spawn",
                    vec![Place::DuplexHandle(0), Place::DuplexHandle(0)],
                    None,
                )
                .expect("hew_task_scope_spawn must be on allowlist"),
            ),
        ],
        terminator: Terminator::Return,
    }];
    let locals = vec![ResolvedTy::Named {
        name: "HewTask".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }];
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "probe".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: locals.clone(),
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
    };

    let tmp = std::env::temp_dir().join("hew-task-abi-task-scope-spawn-paired");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    emit_module(&pipeline, &options)
        .expect("hew_task_new + hew_task_scope_spawn emission should succeed");
    let ir = read_ll(&tmp);

    // BOTH declarations must appear.
    assert!(
        ir.contains("@hew_task_new"),
        "emitted IR must declare @hew_task_new; got:\n{ir}",
    );
    assert!(
        ir.contains("@hew_task_scope_spawn"),
        "emitted IR must declare @hew_task_scope_spawn; got:\n{ir}",
    );

    // The call to hew_task_new must precede the call to hew_task_scope_spawn —
    // ABI invariant (§4 of the W2.006 plan): scope spawn requires a
    // pre-constructed task pointer. We assert this on the order of the
    // *call* instructions (not the declarations, which may be re-ordered
    // by LLVM module pretty-printing).
    let task_new_call_pos = ir
        .find("call ptr @hew_task_new")
        .expect("emitted IR must contain `call ptr @hew_task_new`");
    let task_scope_spawn_call_pos = ir
        .find("call void @hew_task_scope_spawn")
        .expect("emitted IR must contain `call void @hew_task_scope_spawn`");
    assert!(
        task_new_call_pos < task_scope_spawn_call_pos,
        "hew_task_new call must precede hew_task_scope_spawn call \
         (ABI shape invariant: scope spawn requires a pre-constructed task ptr); \
         got task_new at {task_new_call_pos}, task_scope_spawn at {task_scope_spawn_call_pos}\nIR:\n{ir}"
    );

    // Legacy ABI must be fully removed.
    assert!(
        !ir.contains("@hew_scope_spawn"),
        "legacy @hew_scope_spawn must NOT appear in emitted IR (W2.006); got:\n{ir}",
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    emit_module(&pipeline, &options).expect("SpawnTaskClosure emission should succeed");
    let ir = read_ll(&tmp);
    assert!(
        ir.contains("@hew_task_spawn_thread_with_inherited_context"),
        "spawned closures must use inherited-context spawn helper; got:\n{ir}",
    );
    assert!(
        !ir.contains("__hew_spawn_env_rc_drop_"),
        "a scope-owned closure env borrows its string capture and must not get an \
         Rc payload destructor; got:\n{ir}",
    );
    assert!(
        ir.contains("call ptr @hew_rc_new") && ir.contains("ptr null)"),
        "a borrowed scope-owned closure env must retain the null Rc callback; got:\n{ir}",
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
