//! WASM actor trace metadata registration must be emitted on the spawn path.
//!
//! The runtime bridge consumes `hew_wasm_register_actor_meta` to attribute
//! trace events by actor type and handler name. Codegen has no module-init hook,
//! so the producer must run immediately before `hew_actor_spawn` /
//! `hew_actor_spawn_opts`.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::model::ActorHandlerLayout;
use hew_mir::{
    ActorLayout, BasicBlock, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn local_pid_of(actor: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::Named {
            name: actor.to_string(),
            args: vec![],
            builtin: None,
        }],
        builtin: None,
    }
}

fn spawn_pipeline() -> IrPipeline {
    let actor_name = "TraceActor";
    let actor_pid_ty = local_pid_of(actor_name);
    let handler_symbol = format!("{actor_name}__recv__handle_ping");

    let spawn_fn = RawMirFunction {
        name: "spawn_trace_actor".to_string(),
        return_ty: actor_pid_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![actor_pid_ty.clone()],
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
    };

    let handler_fn = RawMirFunction {
        name: handler_symbol.clone(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::EnterContext, Instr::ExitContext],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
    };

    let actor_layout = ActorLayout {
        name: actor_name.to_string(),
        state_field_names: vec![],
        state_field_tys: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        handlers: vec![ActorHandlerLayout {
            name: "handle_ping".to_string(),
            symbol: handler_symbol,
            msg_type: 7,
            param_tys: vec![],
            return_ty: ResolvedTy::Unit,
            requires_state_guard: true,
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
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![actor_layout],
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
    }
}

fn emit_ll(pipeline: &IrPipeline, slug: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-wasm-actor-meta-{slug}"));
    std::fs::create_dir_all(&tmp).expect("create output dir");
    let options = EmitOptions {
        module_name: "wasm_actor_meta_probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(pipeline, &options).expect("metadata pipeline must emit");
    let ll_path: &Path = artefacts.ll_path.as_deref().expect("ll path");
    std::fs::read_to_string(ll_path).expect("read emitted IR")
}

#[test]
fn actor_spawn_registers_trace_metadata_before_spawn() {
    let ir = emit_ll(&spawn_pipeline(), "spawn");

    let register_pos = ir
        .find("call void @hew_wasm_register_actor_meta")
        .unwrap_or_else(|| panic!("expected hew_wasm_register_actor_meta call in:\n{ir}"));
    let spawn_pos = ir
        .find("call ptr @hew_actor_spawn")
        .unwrap_or_else(|| panic!("expected hew_actor_spawn call in:\n{ir}"));
    assert!(
        register_pos < spawn_pos,
        "metadata registration must happen before actor spawn:\n{ir}"
    );

    assert!(
        ir.contains("TraceActor"),
        "actor name missing from IR:\n{ir}"
    );
    assert!(
        ir.contains("handle_ping"),
        "handler name missing from IR:\n{ir}"
    );
    assert!(
        ir.contains("store i32 7"),
        "msg_type missing from IR:\n{ir}"
    );
}
