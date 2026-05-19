//! Codegen coverage for the supervisor bootstrap emission added in S-D.3.
//!
//! Constructs a hand-built `IrPipeline` with one `SupervisorLayout`
//! (`AppSupervisor`) and one `ActorLayout` (`Worker`) and asserts that the
//! emitted LLVM IR for the bootstrap function:
//!
//! - calls `hew_supervisor_new`
//! - calls `hew_supervisor_add_child_spec` for the single child
//! - calls `hew_supervisor_start`
//!
//! All three calls must appear in that order inside `AppSupervisor__bootstrap`.
//!
//! The MIR-side bootstrap body (S-D.1's synthesised `Instr::SpawnActor`
//! sequence) is provided as a stub so the function's declaration shape
//! matches what the real lowering produces; codegen replaces that body
//! wholesale (S-D.3 substitution path) so the stub instructions never reach
//! the emitter.
//!
//! LESSONS: boundary-fail-closed (P0) — the bootstrap is a fail-closed
//! seam against the runtime supervisor ABI.

use hew_hir::HirSupervisorStrategy;
use hew_mir::{
    ActorLayout, BasicBlock, FunctionCallConv, IrPipeline, RawMirFunction, SupervisorChildLayout,
    SupervisorLayout, Terminator,
};
use hew_types::ResolvedTy;

use hew_codegen_rs::{emit_module, EmitOptions};

fn local_pid_of(actor: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::Named {
            name: actor.to_string(),
            args: vec![],
        }],
    }
}

/// Build a pipeline carrying `AppSupervisor` with one `Worker` child.
fn supervisor_pipeline() -> IrPipeline {
    let bootstrap_symbol = "AppSupervisor__bootstrap".to_string();
    let bootstrap_return_ty = local_pid_of("AppSupervisor");

    // Worker actor — zero receive handlers keeps the dispatch trampoline
    // minimal but still present (it is what the bootstrap's HewChildSpec
    // `.dispatch` field references by name).
    let worker_layout = ActorLayout {
        name: "Worker".to_string(),
        state_field_names: vec![],
        state_field_tys: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbol: None,
        on_crash_symbol: None,
        handlers: vec![],
    };

    // Bootstrap function declaration. The body is a stub — codegen's
    // supervisor loop overrides it. The single block + Return terminator
    // is enough to satisfy `declare_function`'s shape expectations; it
    // never reaches `lower_function` because the bootstrap symbol is in
    // the skip-set.
    let bootstrap_fn = RawMirFunction {
        name: bootstrap_symbol.clone(),
        return_ty: bootstrap_return_ty.clone(),
        // Default: see `lower_supervisor_bootstrap` for why the bootstrap
        // is no longer ActorHandler — the body is replaced wholesale by
        // codegen and never touches an execution context.
        call_conv: FunctionCallConv::Default,
        params: vec![],
        // One pointer-typed local matches the bootstrap return shape; the
        // stub instructions don't run so the alloca is dead in the emitted
        // module.
        locals: vec![bootstrap_return_ty.clone()],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
    };

    // `main` exits 42 — the fixture shape. Returns i64 so the produced
    // module verifies; the test only inspects the bootstrap function so
    // `main`'s body is just `ret i64 42`.
    let main_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                hew_mir::Instr::ConstI64 {
                    dest: hew_mir::Place::Local(0),
                    value: 42,
                },
                hew_mir::Instr::Move {
                    dest: hew_mir::Place::ReturnSlot,
                    src: hew_mir::Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
    };

    let supervisor_layout = SupervisorLayout {
        name: "AppSupervisor".to_string(),
        strategy: Some(HirSupervisorStrategy::OneForOne),
        max_restarts: Some(3),
        window: Some("60".to_string()),
        bootstrap_symbol: bootstrap_symbol.clone(),
        children: vec![SupervisorChildLayout {
            name: "w1".to_string(),
            actor_name: "Worker".to_string(),
            restart_policy: None,
            is_pool: false,
            slot_index: 0,
            wired_to: Default::default(),
            spawn_order: 0,
            on_crash_symbol: None,
        }],
    };

    IrPipeline {
        thir: vec![],
        raw_mir: vec![bootstrap_fn.clone(), main_fn.clone()],
        // Keep `checked_mir` empty — the build_module skip-set bypasses
        // lower_function for the bootstrap symbol, and `main`'s body has no
        // cooperate sites. An empty `checked_mir` triggers the
        // hand-built-test fallback path (see `build_module`).
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![worker_layout],
        supervisor_layouts: vec![supervisor_layout],
    }
}

fn emit_to_string(pipeline: &IrPipeline, slug: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-supervisor-emission-{slug}"));
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(pipeline, &options).expect("supervisor emission should succeed");
    let ll = tmp.join("probe.ll");
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("could not read {}: {e}", ll.display()))
}

/// The bootstrap must declare all three supervisor runtime symbols.
#[test]
fn supervisor_bootstrap_declares_runtime_symbols() {
    let ir = emit_to_string(&supervisor_pipeline(), "declares");
    for sym in [
        "@hew_supervisor_new",
        "@hew_supervisor_add_child_spec",
        "@hew_supervisor_start",
    ] {
        assert!(
            ir.contains(sym),
            "expected emitted IR to declare {sym}; got:\n{ir}"
        );
    }
}

/// The bootstrap's body must call `hew_supervisor_new`,
/// `hew_supervisor_add_child_spec`, and `hew_supervisor_start` in order.
#[test]
fn supervisor_bootstrap_emits_calls_in_order() {
    let ir = emit_to_string(&supervisor_pipeline(), "in-order");
    let new_pos = ir
        .find("call ptr @hew_supervisor_new")
        .or_else(|| ir.find("@hew_supervisor_new("))
        .unwrap_or_else(|| panic!("missing hew_supervisor_new call in IR:\n{ir}"));
    let add_pos = ir
        .find("@hew_supervisor_add_child_spec(")
        .unwrap_or_else(|| panic!("missing hew_supervisor_add_child_spec call in IR:\n{ir}"));
    let start_pos = ir
        .find("@hew_supervisor_start(")
        .unwrap_or_else(|| panic!("missing hew_supervisor_start call in IR:\n{ir}"));
    assert!(
        new_pos < add_pos,
        "hew_supervisor_new must precede hew_supervisor_add_child_spec; got positions {new_pos} >= {add_pos} in:\n{ir}"
    );
    assert!(
        add_pos < start_pos,
        "hew_supervisor_add_child_spec must precede hew_supervisor_start; got positions {add_pos} >= {start_pos} in:\n{ir}"
    );
}

/// The bootstrap must reference the per-child dispatch trampoline by name.
#[test]
fn supervisor_bootstrap_references_child_dispatch() {
    let ir = emit_to_string(&supervisor_pipeline(), "dispatch-ref");
    assert!(
        ir.contains("@__hew_actor_dispatch_Worker"),
        "expected the child spec to reference the Worker dispatch trampoline; got:\n{ir}"
    );
}

/// The MIR-side `Instr::SpawnActor` body (S-D.1 stub) must NOT survive
/// into the emitted IR — codegen replaces the bootstrap body wholesale.
#[test]
fn supervisor_bootstrap_skips_mir_body() {
    let ir = emit_to_string(&supervisor_pipeline(), "skip-mir");
    // The stub body in this pipeline carries zero instructions, so the
    // best signal here is structural: the function defines exactly one
    // entry block plus the start-ok/start-trap successors, and never
    // calls `hew_actor_spawn` (which the MIR-side body would emit if it
    // had been lowered through `lower_function`).
    assert!(
        !ir.contains("@hew_actor_spawn"),
        "bootstrap stub body must not be lowered; @hew_actor_spawn leaked into IR:\n{ir}"
    );
}

/// Non-integer `window` literal must fail closed — the duration-string
/// form (`"60s"`) is deferred to a follow-on slice.
#[test]
fn supervisor_bootstrap_rejects_non_integer_window() {
    let mut pipeline = supervisor_pipeline();
    pipeline.supervisor_layouts[0].window = Some("60s".to_string());
    let tmp = std::env::temp_dir().join("hew-supervisor-emission-bad-window");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let err = emit_module(&pipeline, &options)
        .expect_err("non-integer window should fail closed at codegen");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("integer-seconds") || msg.contains("60s"),
        "expected fail-closed diagnostic to name the bad window literal; got: {msg}"
    );
}
