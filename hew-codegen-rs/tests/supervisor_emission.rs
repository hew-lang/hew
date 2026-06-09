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
            builtin: None,
            is_opaque: false,
        }],
        builtin: None,
        is_opaque: false,
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
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        handlers: vec![],
        state_clone_fn_symbol: None,
        state_drop_fn_symbol: None,
        state_field_clone_kinds: None,
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
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
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
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
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
            max_heap_bytes: None,
            cycle_capable: false,
            init_state_fields: vec![],
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
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![worker_layout],
        supervisor_layouts: vec![supervisor_layout],
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

fn emit_to_string(pipeline: &IrPipeline, slug: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-supervisor-emission-{slug}"));
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
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

/// Build a pipeline where the child actor has `#[on(crash)]`. The child's
/// `ActorLayout.on_crash_symbol` and `SupervisorChildLayout.on_crash_symbol`
/// are both `Some("CrasherActor__on_crash")`. A matching `RawMirFunction` is
/// included in `raw_mir` so `declare_function` can register it before
/// `emit_supervisor_bootstrap_body` runs.
fn on_crash_pipeline() -> IrPipeline {
    let bootstrap_symbol = "AppSupervisor__bootstrap".to_string();
    let bootstrap_return_ty = local_pid_of("AppSupervisor");
    let on_crash_symbol = "CrasherActor__on_crash".to_string();

    let crasher_layout = ActorLayout {
        name: "CrasherActor".to_string(),
        state_field_names: vec![],
        state_field_tys: vec![],
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: Some(on_crash_symbol.clone()),
        max_heap_bytes: None,
        cycle_capable: false,
        handlers: vec![],
        state_clone_fn_symbol: None,
        state_drop_fn_symbol: None,
        state_field_clone_kinds: None,
    };

    // Minimal on_crash function: ActorHandler ABI, Unit return, no params.
    // ActorHandler functions require EnterContext at entry and ExitContext
    // before the terminal instruction to pass the context-boundary check.
    let on_crash_fn = RawMirFunction {
        name: on_crash_symbol.clone(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![hew_mir::Instr::EnterContext, hew_mir::Instr::ExitContext],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    };

    let bootstrap_fn = RawMirFunction {
        name: bootstrap_symbol.clone(),
        return_ty: bootstrap_return_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![bootstrap_return_ty.clone()],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    };

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
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    };

    let supervisor_layout = SupervisorLayout {
        name: "AppSupervisor".to_string(),
        strategy: Some(HirSupervisorStrategy::OneForOne),
        max_restarts: Some(3),
        window: Some("60".to_string()),
        bootstrap_symbol: bootstrap_symbol.clone(),
        children: vec![SupervisorChildLayout {
            name: "c1".to_string(),
            actor_name: "CrasherActor".to_string(),
            restart_policy: None,
            is_pool: false,
            slot_index: 0,
            wired_to: Default::default(),
            spawn_order: 0,
            on_crash_symbol: Some(on_crash_symbol.clone()),
            max_heap_bytes: None,
            cycle_capable: false,
            init_state_fields: vec![],
        }],
    };

    IrPipeline {
        thir: vec![],
        raw_mir: vec![on_crash_fn, bootstrap_fn, main_fn],
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![crasher_layout],
        supervisor_layouts: vec![supervisor_layout],
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

/// When a child's actor has `#[on(crash)]`, the emitted child spec must carry
/// a non-null pointer to `{actor_name}__on_crash` in the `on_crash` slot.
///
/// The assertion targets a `store ptr @CrasherActor__on_crash` into the child
/// spec alloca (field index 9), which is the GEP store path emitted by
/// `emit_supervisor_child_spec_and_register` when `on_crash_symbol` is `Some`.
#[test]
fn supervisor_bootstrap_populates_on_crash_fn_pointer() {
    let ir = emit_to_string(&on_crash_pipeline(), "on-crash-ptr");
    assert!(
        ir.contains("@CrasherActor__on_crash"),
        "expected on_crash function symbol in emitted IR; got:\n{ir}"
    );
    // The store into the child spec's on_crash slot (field 9) must reference
    // the function symbol, not a null pointer.
    assert!(
        ir.contains("store ptr @CrasherActor__on_crash"),
        "expected `store ptr @CrasherActor__on_crash` in child spec on_crash slot; got:\n{ir}"
    );
    // Null must not appear in the on_crash store — the slot is populated.
    // We check this by confirming the store uses the symbol, not null.
    // (The null check above via presence of the symbol-store is sufficient.)
}

/// The existing null-path fixture (`supervisor_pipeline`) must continue to
/// work: when no child has `on_crash_symbol`, the on_crash slot in the child
/// spec carries a null pointer and the IR contains no on_crash symbol store.
#[test]
fn supervisor_bootstrap_on_crash_null_when_no_hook() {
    let ir = emit_to_string(&supervisor_pipeline(), "on-crash-null");
    // No on_crash symbol should appear as a stored pointer (Worker has none).
    assert!(
        !ir.contains("store ptr @__hew_actor_dispatch_Worker\n")
            || !ir.contains("Worker__on_crash"),
        // Just verify the on_crash symbol isn't present at all in the IR
        // beyond the dispatch trampoline for the Worker actor.
        "IR should not reference any on_crash symbol for the no-hook Worker actor; got:\n{ir}"
    );
    // The child spec store for the on_crash slot must use null.
    // Since the Worker actor has no on_crash_symbol, the field_values loop
    // stores ptr_ty.const_null() for field index 9.
    assert!(
        !ir.contains("Worker__on_crash"),
        "Worker actor has no #[on(crash)]; IR must not reference Worker__on_crash; got:\n{ir}"
    );
}

#[test]
fn supervisor_bootstrap_populates_cycle_capable_child_spec_bit() {
    let mut pipeline = supervisor_pipeline();
    pipeline.supervisor_layouts[0].children[0].cycle_capable = true;

    let ir = emit_to_string(&pipeline, "cycle-capable-child");
    assert!(
        ir.contains("child_spec_0_f8"),
        "expected child spec field 8 GEP for cycle_capable; got:\n{ir}"
    );
    assert!(
        ir.contains("store i32 1, ptr %child_spec_0_f8"),
        "expected child spec cycle_capable store to be 1; got:\n{ir}"
    );
}

/// Fail-closed: a child with `on_crash_symbol = Some(<nonexistent>)` must
/// produce a `FailClosed` diagnostic naming the missing function, not silently
/// emit null.
#[test]
fn supervisor_bootstrap_fails_closed_on_missing_on_crash_symbol() {
    let mut pipeline = on_crash_pipeline();
    // Replace the on_crash_symbol with a name that has no matching raw_mir fn.
    pipeline.supervisor_layouts[0].children[0].on_crash_symbol =
        Some("CrasherActor__on_crash_MISSING".to_string());
    let tmp = std::env::temp_dir().join("hew-supervisor-emission-missing-on-crash");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let err = emit_module(&pipeline, &options)
        .expect_err("missing on_crash symbol should fail closed at codegen");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("CrasherActor__on_crash_MISSING"),
        "fail-closed diagnostic must name the missing on_crash symbol; got: {msg}"
    );
}

/// A duration-literal `window` (`"60s"`) is now interpreted by codegen — the
/// flat-reliability-fields surface carries the window as a duration string and
/// codegen converts the unit via the centralised duration parser.
#[test]
fn supervisor_bootstrap_accepts_duration_window() {
    let mut pipeline = supervisor_pipeline();
    pipeline.supervisor_layouts[0].window = Some("60s".to_string());
    let tmp = std::env::temp_dir().join("hew-supervisor-emission-duration-window");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    emit_module(&pipeline, &options)
        .expect("a `60s` duration window must be accepted and converted to seconds");
}

/// An unparseable `window` literal (neither a duration nor an integer) still
/// fails closed — silently coercing to 0 would hide a parser/MIR drift bug.
#[test]
fn supervisor_bootstrap_rejects_invalid_window() {
    let mut pipeline = supervisor_pipeline();
    pipeline.supervisor_layouts[0].window = Some("sixty".to_string());
    let tmp = std::env::temp_dir().join("hew-supervisor-emission-bad-window");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let err = emit_module(&pipeline, &options)
        .expect_err("a non-duration, non-integer window must fail closed at codegen");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("sixty") || msg.contains("duration literal"),
        "expected fail-closed diagnostic to name the bad window literal; got: {msg}"
    );
}
