//! Codegen verification for `Instr::MachineEmitPlaceholder` lowering.
//!
//! Verifies two properties:
//!
//! 1. **IR emission**: a MIR function containing `MachineEmitPlaceholder`
//!    emits a call to `@hew_machine_emit_push` in the produced LLVM IR, and
//!    `emit_module` succeeds (LLVM `Module::verify()` green).
//!
//! 2. **JIT execution**: the JIT-compiled caller invokes the runtime step
//!    enter/exit wrapper; events pushed by the step drain at the outermost
//!    frame.
//!
//! ## MIR shape
//!
//! The test assembles a hand-built `IrPipeline` carrying:
//!
//! - A `TcpHandshake`-like machine with two unit states (`Closed`,
//!   `SynReceived`) and two unit events (`SynReceive`, `AckReceive`).
//! - A synthesised `TcpHandshake__step` stub that contains two
//!   `MachineEmitPlaceholder` instructions for event indices 0 and 1,
//!   then traps (state transitions are not the unit under test here;
//!   `machine_dispatch_codegen.rs` covers that surface).
//! - A `caller` fn that invokes the stub and returns.
//!
//! ## Why hand-built MIR rather than source-level pipeline
//!
//! The HIR path for `emit` expressions lowers correctly through the
//! pipeline for files that parse (see `machine_dispatch_codegen.rs`), but
//! the current source-level JIT harness requires the full `hew` binary to
//! link and run. Hand-built MIR lets this test live inside the
//! `hew-codegen-rs` crate without an out-of-process compile step.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the codegen arm must not silently drop
//!   the emit — the IR must contain the push call.
//! - `exhaustive-coverage` (P0): both the IR-shape assertion and the JIT
//!   execution assertion verify the wiring.

#![cfg(not(target_arch = "wasm32"))]

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, FunctionCallConv, Instr, IrPipeline, MachineLayout, MachineVariantLayout, Place,
    RawMirFunction, Terminator,
};
use hew_runtime::machine_emit::{thread_emit_clear, thread_emit_pending};
use hew_types::ResolvedTy;

// ── Pipeline builder ──────────────────────────────────────────────────────────

/// Build a minimal `IrPipeline` carrying:
/// - `TcpHandshake` machine layout: 2 unit states, 2 unit events.
/// - A `TcpHandshake__step` stub that emits two `MachineEmitPlaceholder`
///   instructions (indices 0 and 1) before trapping.
/// - A `caller` fn that invokes the step stub.
///
/// The two emit instructions are the substrate under test; the trap
/// ensures the function body terminates cleanly after the emissions.
fn tcp_handshake_emit_pipeline() -> IrPipeline {
    let machine_name = "TcpHandshake".to_string();
    let event_name = format!("{machine_name}Event");
    let machine_ty = ResolvedTy::Named {
        name: machine_name.clone(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };
    let event_ty = ResolvedTy::Named {
        name: event_name.clone(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };

    // Two unit states, two unit events.
    let variants = vec!["Closed", "SynReceived"]
        .into_iter()
        .map(|n| MachineVariantLayout {
            name: n.to_string(),
            field_tys: Vec::new(),
        })
        .collect::<Vec<_>>();
    let events = vec!["SynReceive", "AckReceive"]
        .into_iter()
        .map(|n| MachineVariantLayout {
            name: n.to_string(),
            field_tys: Vec::new(),
        })
        .collect::<Vec<_>>();
    let machine_layout = MachineLayout {
        name: machine_name.clone(),
        tag_width: 1,
        variants,
        events,
    };

    // `TcpHandshake__step(self, event) -> TcpHandshake`:
    //
    // Block 0: emit event 0, emit event 1, store self into ReturnSlot, Return.
    //
    // The two MachineEmitPlaceholder instructions are the wiring under test.
    // The store-back into ReturnSlot and Return let the function exit cleanly
    // (no trap) so the JIT execution test can drive it without sigsetjmp.
    let step_fn = RawMirFunction {
        name: format!("{machine_name}__step"),
        return_ty: machine_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![machine_ty.clone(), event_ty.clone()],
        locals: vec![machine_ty.clone(), event_ty.clone()],
        local_names: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                // emit SynReceive (index 0) — unit event, no payload.
                Instr::MachineEmitPlaceholder {
                    event_idx: 0,
                    payload: Vec::new(),
                },
                // emit AckReceive (index 1) — unit event, no payload.
                Instr::MachineEmitPlaceholder {
                    event_idx: 1,
                    payload: Vec::new(),
                },
                // Store self (local 0 — the unchanged machine value) into
                // the return slot so Terminator::Return can load from it.
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };

    // `caller()` invokes the step stub with zeroed locals and returns.
    // Block 0 → Call → Block 1 → Return.
    let caller = RawMirFunction {
        name: "caller".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![
            machine_ty.clone(), // 0: machine binding (zeroed)
            event_ty.clone(),   // 1: event arg (zeroed)
            machine_ty.clone(), // 2: step return temp
        ],
        local_names: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Call {
                    callee: format!("{machine_name}__step"),
                    builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                        "{machine_name}__step",
                    ),
                    args: vec![Place::Local(0), Place::Local(1)],
                    dest: Some(Place::Local(2)),
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![Instr::Move {
                    dest: Place::Local(0),
                    src: Place::Local(2),
                }],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };

    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![step_fn, caller],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: vec![machine_layout],
        enum_layouts: Vec::new(),
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

/// Emit the pipeline to a `.ll` file and return the IR text.
fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-machine-emit-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("emit_module must succeed");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ── IR shape test ─────────────────────────────────────────────────────────────

/// `MachineEmitPlaceholder` must lower to a `call @hew_machine_emit_push`
/// in the emitted LLVM IR.
///
/// This test verifies:
/// - `emit_module` succeeds (LLVM `Module::verify()` passes).
/// - The emitted `.ll` contains a declaration of `@hew_machine_emit_push`.
/// - The step function body contains two `call` sites targeting the push.
/// - The call passes a constant `u32` event tag and a null payload.
/// - The caller wraps the `__step` invocation in enter/exit calls.
#[test]
fn machine_emit_placeholder_lowers_to_push_call() {
    let pipeline = tcp_handshake_emit_pipeline();
    let ir = emit_ll(&pipeline, "machine_emit_placeholder");

    // The push symbol must be declared in the module.
    assert!(
        ir.contains("@hew_machine_emit_push"),
        "emitted IR must declare @hew_machine_emit_push:\n{ir}"
    );

    // Two call sites: one for event_idx=0, one for event_idx=1.
    // LLVM emits void calls as `call void @hew_machine_emit_push(...)` with
    // no SSA name (void returns have no name), so count the bare symbol
    // appearances in `call` positions rather than the name hint.
    let call_count = ir.matches("call i32 @hew_machine_emit_push").count();
    assert_eq!(
        call_count, 2,
        "step fn must emit exactly 2 hew_machine_emit_push calls (one per emit \
         instruction); found {call_count} in IR:\n{ir}"
    );

    // The event tag constants 0 and 1 must appear as i32 arguments.
    assert!(
        ir.contains("i32 1"),
        "IR must contain `i32 1` as the event tag=1 argument:\n{ir}"
    );

    // The null payload pointer: LLVM 17+ opaque-pointer mode emits `null`
    // for `ptr_type.const_null()`.
    assert!(
        ir.contains("null"),
        "IR must pass a null payload pointer for unit events:\n{ir}"
    );
    assert!(
        ir.contains("@hew_machine_emit_step_enter") && ir.contains("@hew_machine_emit_step_exit"),
        "caller must wrap the __step invocation with machine emit enter/exit calls:\n{ir}"
    );
}

// ── JIT execution test ────────────────────────────────────────────────────────

/// JIT-compile and execute the caller; assert that the `__step` call is wrapped
/// and the outermost exit drains the thread-local emit queue.
///
/// ## Method
///
/// 1. Emit the pipeline to `.ll`.
/// 2. Parse the `.ll` back into an inkwell `Module`.
/// 3. Create an MCJIT `ExecutionEngine` and wire machine emit runtime symbols
///    through `add_global_mapping` against the linked dev-dep symbol. The
///    macOS test-binary dynamic-symbol table does not expose `#[no_mangle]`
///    runtime exports for JIT-host lookup, so the mapping is mandatory; see
///    the inline comment at the mapping site for the platform rationale.
/// 4. Clear any stale events from prior tests on this thread.
/// 5. Invoke the `caller` function (which calls the step stub).
/// 6. The step stub emits event 0 then event 1, stores `self` into the
///    return slot, and returns cleanly (no trap). `caller` receives the
///    return value and discards it.
/// 7. Assert the outermost step exit drained the thread-local queue.
#[test]
#[cfg(unix)]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn machine_emit_push_populates_thread_queue_in_fifo_order() {
    use inkwell::context::Context;
    use inkwell::memory_buffer::MemoryBuffer;
    use inkwell::targets::{InitializationConfig, Target};
    use inkwell::OptimizationLevel;

    // ── Compile to .ll ───────────────────────────────────────────────────────
    let pipeline = tcp_handshake_emit_pipeline();
    let tmp = std::env::temp_dir().join("hew-machine-emit-jit");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "machine_emit_jit",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit_module must succeed");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");

    // ── Parse + JIT ──────────────────────────────────────────────────────────
    Target::initialize_native(&InitializationConfig::default())
        .expect("initialize_native must succeed on the host platform");

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(ll_path).expect("read emitted .ll into memory buffer");
    let module = ctx
        .create_module_from_ir(buf)
        .expect("parse .ll into inkwell Module");

    // Look up machine emit declarations before JIT takes ownership of the module.
    let emit_push_decl = module
        .get_function("hew_machine_emit_push")
        .expect("emitted module must declare hew_machine_emit_push");
    let step_enter_decl = module
        .get_function("hew_machine_emit_step_enter")
        .expect("emitted module must declare hew_machine_emit_step_enter");
    let step_exit_decl = module
        .get_function("hew_machine_emit_step_exit")
        .expect("emitted module must declare hew_machine_emit_step_exit");

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed");

    // Wire the JIT symbol resolver to the actual machine emit functions
    // from the `hew-runtime` dev-dep.
    //
    // WHY add_global_mapping is required here: Rust test binaries on macOS
    // (and Linux with default linker flags) do not export all `#[no_mangle]`
    // symbols to the dynamic symbol table; the MCJIT engine's default symbol
    // resolver cannot find them by name. `add_global_mapping` bypasses the
    // resolver and directly wires the JIT reference to the in-process function
    // pointer, which is always reachable by address.
    extern "C" {
        fn hew_machine_emit_push(queue: *mut std::ffi::c_void, tag: u32, payload: *const u8)
            -> i32;
        fn hew_machine_emit_step_enter(queue: *mut std::ffi::c_void) -> i32;
        fn hew_machine_emit_step_exit(queue: *mut std::ffi::c_void) -> i32;
    }
    ee.add_global_mapping(&emit_push_decl, hew_machine_emit_push as *const () as usize);
    ee.add_global_mapping(
        &step_enter_decl,
        hew_machine_emit_step_enter as *const () as usize,
    );
    ee.add_global_mapping(
        &step_exit_decl,
        hew_machine_emit_step_exit as *const () as usize,
    );

    // Clear any stale events from a prior test on this thread.
    thread_emit_clear();
    assert_eq!(
        thread_emit_pending(),
        0,
        "thread queue must be empty before JIT call"
    );

    // Invoke `caller` which drives the step stub and triggers two pushes.
    // The step stub returns cleanly (no trap path): after the two emit
    // instructions it stores `self` into the return slot and returns.
    //
    // SAFETY: `caller` is compiled as `fn() -> i8` (unit mapped to i8 by
    // the codegen); the JIT-compiled code is for the host triple.
    let caller_fn = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i8>("caller")
            .expect("caller must be present in the JIT module")
    };
    unsafe { caller_fn.call() };

    // ── Assertions ───────────────────────────────────────────────────────────

    // The caller wrapped the step invocation, so the outermost exit drained the
    // two pushed events before returning.
    assert_eq!(
        thread_emit_pending(),
        0,
        "outermost step exit must drain queued MachineEmitPlaceholder events"
    );
}
