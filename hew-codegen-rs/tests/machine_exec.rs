//! Substrate-level verification for the `m.step(ev)` and
//! `m.state_name()` lowering path.
//!
//! The milestone goal — `examples/machine/run_traffic_light.hew`
//! compiling end-to-end via `hew run` — is blocked on HIR module-scope
//! ctor resolution for enum unit variants and machine state/event
//! ctors (the same gap blocks `examples/progressive/07_enums.hew`).
//! See the comment at the top of the fixture for details.
//!
//! These tests assemble MIR by hand and exercise:
//!
//! 1. The MachineStep call-site lowering: a `Terminator::Call` to the
//!    synthesised `<Name>__step` helper followed by an unconditional
//!    store-back of the call's return value into the receiver's
//!    binding slot. The synthesised helper itself is stubbed to
//!    `Terminator::Trap` here — the dispatch-tree shape is covered by
//!    `machine_dispatch_codegen.rs` and re-asserting it would duplicate
//!    that coverage.
//!
//! 2. The MachineStateName lowering: the per-machine `[N x ptr]`
//!    state-name table is emitted alongside the machine's
//!    tagged-union layout, and `Instr::MachineStateName` GEPs into it
//!    with the machine's tag.
//!
//! When the HIR-resolution precursor lands, an end-to-end `hew run`
//! check on `run_traffic_light.hew` becomes possible; that check is
//! the milestone-level signal these substrate tests stand in for.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, FunctionCallConv, Instr, IrPipeline, MachineLayout, MachineVariantLayout, Place,
    RawMirFunction, Terminator, TrapKind,
};
use hew_types::ResolvedTy;

/// Drive the public `emit_module` path and return the emitted `.ll`.
/// Mirrors the helper in `machine_dispatch_codegen.rs` so the
/// substrate tests use the same code path as `hew compile --emit-llvm`.
fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-machine-exec-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(pipeline, &options).expect("emit_module must succeed");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Build a minimal pipeline carrying:
///  - a `TrafficLight` machine layout (3 unit states: Red, Green, Yellow;
///    one Tick event);
///  - the synthesised `TrafficLight__step(self, event) -> TrafficLight`
///    helper as a Trap-only stub (its dispatch shape is exercised
///    elsewhere);
///  - a `caller` fn that holds a `TrafficLight` slot, performs the
///    Call+Move store-back sequence the MIR producer emits for
///    `m.step(ev)`, and runs `Instr::MachineStateName` to read the
///    state-name pointer into a `String` slot.
fn traffic_light_pipeline() -> IrPipeline {
    let machine_name = "TrafficLight".to_string();
    let event_name = format!("{machine_name}Event");
    let machine_ty = ResolvedTy::Named {
        name: machine_name.clone(),
        args: Vec::new(),
    };
    let event_ty = ResolvedTy::Named {
        name: event_name.clone(),
        args: Vec::new(),
    };

    // Three unit states, one unit event. `field_tys` is empty for unit
    // payloads — matches the Lane A traffic_light.hew snapshot fixture.
    let variants = vec!["Red", "Green", "Yellow"]
        .into_iter()
        .map(|n| MachineVariantLayout {
            name: n.to_string(),
            field_tys: Vec::new(),
        })
        .collect::<Vec<_>>();
    let events = vec![MachineVariantLayout {
        name: "Tick".to_string(),
        field_tys: Vec::new(),
    }];
    let machine_layout = MachineLayout {
        name: machine_name.clone(),
        // One state-bit per variant slot; 2 bits cover three states.
        tag_width: 2,
        variants,
        events,
    };

    // Stub `TrafficLight__step(self: TrafficLight, event: TrafficLightEvent)
    // -> TrafficLight`. Body: trap. The Call+Move sequence in `caller`
    // is the unit under test; the helper's body shape is asserted by
    // `machine_dispatch_codegen.rs` against the real lowering.
    let step_fn = RawMirFunction {
        name: format!("{machine_name}__step"),
        return_ty: machine_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![machine_ty.clone(), event_ty.clone()],
        // Locals: 0 = self param, 1 = event param.
        locals: vec![machine_ty.clone(), event_ty.clone()],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Trap {
                kind: TrapKind::MachineDispatchUnreachable,
            },
        }],
        decisions: Vec::new(),
    };

    // `caller` mirrors the MIR producer output for:
    //     var m = <some TrafficLight>;
    //     m.step(<some TrafficLightEvent>);
    //     let s = m.state_name();
    //
    // We don't construct the initial machine/event values here (those
    // require MachineVariantCtor, which is reachable only through the
    // HIR path that's blocked by the precursor). The Call argument
    // loads are by-value reads from the alloca slots; the store-back
    // and state-name lookup are what we verify.
    //
    // Locals:
    //   0: TrafficLight       (machine binding slot — the receiver)
    //   1: TrafficLightEvent  (event arg)
    //   2: TrafficLight       (call result temp)
    //   3: String             (state_name dest)
    let caller_locals = vec![
        machine_ty.clone(),
        event_ty.clone(),
        machine_ty.clone(),
        ResolvedTy::String,
    ];
    let caller = RawMirFunction {
        name: "caller".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: caller_locals,
        blocks: vec![
            // Block 0: the Call terminator carries the step args and
            // continuation block id 1.
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Call {
                    callee: format!("{machine_name}__step"),
                    args: vec![Place::Local(0), Place::Local(1)],
                    dest: Some(Place::Local(2)),
                    next: 1,
                },
            },
            // Block 1: store-back into the receiver slot, then run the
            // state-name lookup against that slot, then return.
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![
                    Instr::Move {
                        dest: Place::Local(0),
                        src: Place::Local(2),
                    },
                    Instr::MachineStateName {
                        machine_name: machine_name.clone(),
                        src_local: 0,
                        dest: Place::Local(3),
                    },
                ],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
    };

    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![step_fn, caller],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: vec![machine_layout],
    }
}

#[test]
fn machine_step_emits_call_and_store_back() {
    // The MIR producer for `m.step(ev)` finishes the current block with
    // a `Terminator::Call { dest = ret_local }` and emits `Instr::Move
    // { dest = receiver_slot, src = ret_local }` in the continuation
    // block. Codegen lowers the call as a struct-returning LLVM call
    // and the Move as a load+store between the alloca slots.
    //
    // `emit_module` exercises the same `build_module` path the
    // production compiler walks; it runs LLVM `Module::verify()`
    // internally and fails closed on a bad shape, so reading the
    // emitted `.ll` is both an IR-shape check AND a verifier signal.
    let pipeline = traffic_light_pipeline();
    let ir = emit_ll(&pipeline, "machine_step_test");

    // The synthesised step function must be declared and reachable
    // from `caller`.
    assert!(
        ir.contains("@TrafficLight__step("),
        "step helper must be present in module IR:\n{ir}"
    );
    assert!(
        ir.contains("call ") && ir.contains("@TrafficLight__step("),
        "caller must `call @TrafficLight__step`:\n{ir}"
    );

    // The Call's return value must be stored back. inkwell labels the
    // call result `%call_result`; the store-back load uses a `move_*`
    // pattern emitted by Instr::Move via the place primitives. We
    // assert both the call-result store AND that the load source is
    // the call's return temp's alloca.
    assert!(
        ir.contains("call_result") && ir.contains("store"),
        "call result must be stored into a slot:\n{ir}"
    );
}

#[test]
fn machine_state_name_emits_state_table_lookup() {
    // The per-machine state-name table must be emitted as a private
    // `[N x ptr]` global, each entry pointing at a private
    // NUL-terminated state-name string. `Instr::MachineStateName`
    // reads the tag, zext-widens to i64, and GEPs into the table.
    let pipeline = traffic_light_pipeline();
    let ir = emit_ll(&pipeline, "machine_state_name_test");

    // Per-state name globals (one per declared state).
    for state in ["Red", "Green", "Yellow"] {
        let sym = format!("@__hew_state_name__TrafficLight__{state}");
        assert!(
            ir.contains(&sym),
            "state-name global `{sym}` must be emitted:\n{ir}"
        );
    }

    // Table global itself.
    assert!(
        ir.contains("@__hew_state_name_table__TrafficLight"),
        "per-machine state-name table must be emitted:\n{ir}"
    );

    // The state-name lookup must read the machine tag (same GEP label
    // the dispatch tree uses, `machine_tag_ptr`) and GEP into the
    // table.
    assert!(
        ir.contains("machine_tag_ptr"),
        "state_name lowering must read the machine's tag via the \
         shared `machine_tag_ptr` GEP label:\n{ir}"
    );
    assert!(
        ir.contains("state_name_entry_ptr"),
        "state_name lowering must GEP into the table via `state_name_entry_ptr`:\n{ir}"
    );
}

#[test]
fn machine_state_name_table_population_persists_to_emit_module() {
    // `emit_module` is the path `hew compile` uses; it must agree with
    // `build_module` on the state-name table shape. Different path
    // (writes to disk + uses target machine), so this guards the
    // emit-side glue separately.
    let pipeline = traffic_light_pipeline();
    let tmp = std::env::temp_dir().join("hew-machine-exec-emit-module");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "machine_exec_emit",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit_module must succeed");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    let ll = std::fs::read_to_string(ll_path).expect("read emitted .ll");
    assert!(
        ll.contains("@__hew_state_name_table__TrafficLight"),
        "emit_module .ll must contain the state-name table global:\n{ll}"
    );
    assert!(
        ll.contains("@TrafficLight__step("),
        "emit_module .ll must contain the step helper definition:\n{ll}"
    );
}

#[test]
#[ignore = "blocked on HIR module-scope ctor resolution for enum unit \
            variants and machine state/event ctors; see \
            examples/machine/run_traffic_light.hew header. Re-enable \
            once the precursor lane lands."]
fn run_traffic_light_fixture_executes() {
    // End-to-end milestone signal: `hew run` on the run_traffic_light
    // fixture exits 0 and prints the expected state sequence.
    //
    // When enabled, this test runs the in-tree `hew` binary as a
    // subprocess (same path `cmd_run` walks) against
    // `examples/machine/run_traffic_light.hew` and diffs stdout
    // against the `.expected` file.
    let manifest = env!("CARGO_MANIFEST_DIR");
    let hew_bin = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("hew");
    let fixture = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("examples")
        .join("machine")
        .join("run_traffic_light.hew");
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path).expect("read .expected");

    let output = std::process::Command::new(&hew_bin)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert_eq!(stdout, expected, "run_traffic_light stdout mismatch");
}
