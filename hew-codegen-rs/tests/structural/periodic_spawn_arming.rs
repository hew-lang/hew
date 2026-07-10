//! `#[every(duration)]` periodic handlers must be armed on the spawn path.
//!
//! Spawn-site codegen consumes `ActorHandlerLayout.every_ms` and emits one
//! `hew_actor_schedule_periodic(actor, msg_type, interval_ms)` per periodic
//! handler, after the spawn + lifecycle sequence, with a fail-closed trap on
//! a null (arming-failed) handle. The `msg_type` operand must be the layout's
//! descriptor-derived id — the same one the send path dispatches on.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::model::ActorHandlerLayout;
use hew_mir::{
    ActorLayout, BasicBlock, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn local_pid_of(actor: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: actor.to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

fn spawn_pipeline(every_ms: Option<u64>) -> IrPipeline {
    let actor_name = "PulseActor";
    let actor_pid_ty = ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![local_pid_of(actor_name)],
        builtin: None,
        is_opaque: false,
    };
    let handler_symbol = format!("{actor_name}__recv__tick");

    let spawn_fn = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "spawn_pulse_actor".to_string(),
        return_ty: actor_pid_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![actor_pid_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::SpawnActor {
                    actor_name: actor_name.to_string(),
                    state: None,
                    init_args: vec![],
                    dest: Place::Local(0),
                    max_heap_bytes: None,
                    cycle_capable: false,
                    mailbox_capacity: None,
                    overflow_policy: None,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };

    let handler_fn = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: handler_symbol.clone(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::EnterContext, Instr::ExitContext],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };

    let actor_layout = ActorLayout {
        name: actor_name.to_string(),
        defining_module: None,
        state_field_names: vec![],
        state_field_tys: vec![],
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        on_exit_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        mailbox_capacity: None,
        overflow_policy: None,
        coalesce_key_plan: None,
        handlers: vec![ActorHandlerLayout {
            name: "tick".to_string(),
            symbol: handler_symbol,
            msg_type: 7,
            param_tys: vec![],
            return_ty: ResolvedTy::Unit,
            requires_state_guard: true,
            every_ms,
            is_stream_producer: false,
        }],
        state_clone_fn_symbol: None,
        state_drop_fn_symbol: None,
        state_field_clone_kinds: None,
    };

    IrPipeline {
        thir: vec![],
        raw_mir: vec![spawn_fn, handler_fn],
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![actor_layout],
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

fn emit_ll(pipeline: &IrPipeline, slug: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-periodic-spawn-{slug}"));
    std::fs::create_dir_all(&tmp).expect("create output dir");
    let options = EmitOptions {
        module_name: "periodic_spawn_probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("periodic spawn pipeline must emit");
    let ll_path: &Path = artefacts.ll_path.as_deref().expect("ll path");
    std::fs::read_to_string(ll_path).expect("read emitted IR")
}

#[test]
fn periodic_handler_spawn_arms_timer_with_layout_msg_type() {
    let ir = emit_ll(&spawn_pipeline(Some(50)), "armed");

    let arm_pos = ir
        .find("call ptr @hew_actor_schedule_periodic(")
        .unwrap_or_else(|| panic!("expected hew_actor_schedule_periodic call in:\n{ir}"));
    assert!(
        ir.contains("i32 7, i64 50)"),
        "arming must pass the layout msg_type (7) and interval_ms (50):\n{ir}"
    );
    let spawn_pos = ir
        .find("call ptr @hew_actor_spawn")
        .unwrap_or_else(|| panic!("expected hew_actor_spawn call in:\n{ir}"));
    assert!(
        spawn_pos < arm_pos,
        "the timer must be armed on the freshly spawned actor, after spawn:\n{ir}"
    );
}

#[test]
fn periodic_arming_null_handle_traps_fail_closed() {
    let ir = emit_ll(&spawn_pipeline(Some(50)), "trap");

    assert!(
        ir.contains("periodic_arm_fail_tick"),
        "arming must branch to a per-handler fail block on a null handle:\n{ir}"
    );
    assert!(
        ir.contains("call void @hew_trap_with_code(i32 206)"),
        "a null arming handle must trap with HEW_TRAP_ACTOR_SEND_FAILED=206:\n{ir}"
    );
}

#[test]
fn message_driven_handler_spawn_does_not_arm() {
    let ir = emit_ll(&spawn_pipeline(None), "unarmed");

    assert!(
        !ir.contains("hew_actor_schedule_periodic"),
        "an actor with no #[every] handlers must not touch the periodic ABI:\n{ir}"
    );
}
