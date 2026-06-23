use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
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
            is_opaque: false,
        }],
        builtin: None,
        is_opaque: false,
    }
}

fn spawn_pipeline(
    actor_name: &str,
    cycle_capable: bool,
    max_heap_bytes: Option<u64>,
) -> IrPipeline {
    let actor_pid_ty = local_pid_of(actor_name);
    let spawn_fn = RawMirFunction {
        name: "spawn_actor".to_string(),
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
                    max_heap_bytes,
                    cycle_capable,
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
        max_heap_bytes,
        cycle_capable,
        handlers: vec![],
        state_clone_fn_symbol: None,
        state_drop_fn_symbol: None,
        state_field_clone_kinds: None,
    };

    IrPipeline {
        thir: vec![],
        raw_mir: vec![spawn_fn],
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
    }
}

fn emit_ll(pipeline: &IrPipeline, slug: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-cycle-capable-spawn-{slug}"));
    std::fs::create_dir_all(&tmp).expect("create output dir");
    let options = EmitOptions {
        module_name: "cycle_spawn_probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("cycle spawn pipeline must emit");
    let ll_path: &Path = artefacts.ll_path.as_deref().expect("ll path");
    std::fs::read_to_string(ll_path).expect("read emitted IR")
}

#[test]
fn cycle_capable_spawn_uses_opts_abi_and_sets_policy_bit() {
    let ir = emit_ll(&spawn_pipeline("CycleActor", true, None), "cycle");

    assert!(
        ir.contains("@hew_actor_spawn_opts"),
        "cycle-capable spawn must use opts ABI even without #[max_heap]:\n{ir}"
    );
    assert!(
        ir.contains("opts_f9"),
        "HewActorOpts field 9 must be materialized for cycle_capable:\n{ir}"
    );
    assert!(
        ir.contains("store i32 1, ptr %opts_f9"),
        "cycle-capable spawn must store cycle_capable=1 in HewActorOpts:\n{ir}"
    );
}

#[test]
fn non_cycle_uncapped_spawn_keeps_plain_spawn_path() {
    let ir = emit_ll(&spawn_pipeline("PlainActor", false, None), "plain");

    assert!(
        ir.contains("call ptr @hew_actor_spawn("),
        "non-cycle uncapped spawn should keep the legacy three-arg path:\n{ir}"
    );
    assert!(
        !ir.contains("hew_actor_spawn_opts_call"),
        "non-cycle uncapped spawn must not route through opts ABI:\n{ir}"
    );
}

#[test]
fn max_heap_non_cycle_spawn_sets_cycle_policy_to_zero() {
    let ir = emit_ll(&spawn_pipeline("CappedActor", false, Some(128)), "max-heap");

    assert!(
        ir.contains("@hew_actor_spawn_opts"),
        "max-heap spawn still uses opts ABI:\n{ir}"
    );
    assert!(
        ir.contains("opts_f9"),
        "HewActorOpts field 9 must be emitted on the opts path:\n{ir}"
    );
    assert!(
        ir.contains("store i32 0, ptr %opts_f9"),
        "non-cycle max-heap spawn must store cycle_capable=0:\n{ir}"
    );
}
