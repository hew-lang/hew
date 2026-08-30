//! Codegen verification for `Instr::MachineEmitPlaceholder` lowering.
//!
//! Verifies two properties:
//!
//! 1. **IR emission**: a MIR function containing `MachineEmitPlaceholder`
//!    emits a call to `@hew_machine_emit_push` in the produced LLVM IR, and
//!    `emit_module` succeeds (LLVM `Module::verify()` green).
//!
//! 2. **JIT execution**: the JIT-compiled caller invokes the runtime step
//!    enter / exit-keep wrapper; events pushed by the step stay queued
//!    (the deliver-design step-exit never drains — `m.take_emits(ev)` is
//!    the typed consume surface).
//!
//! ## MIR shape
//!
//! The test assembles a hand-built `IrPipeline` carrying:
//!
//! - A `TcpHandshake`-like machine with two unit states (`Closed`,
//!   `SynReceived`) and two unit events (`SynReceive`, `AckReceive`).
//! - A synthesised unit-returning `TcpHandshake__step` stub that contains two
//!   `MachineEmitPlaceholder` instructions for event indices 0 and 1
//!   (state transitions are not the unit under test here;
//!   `machine_dispatch_codegen.rs` covers that surface).
//! - A unit-signature `caller` fn that invokes the stub and returns, without
//!   fabricating machine values that the emit-wiring assertion does not need.
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
    BasicBlock, FunctionCallConv, Instr, IrPipeline, MachineLayout, MachineVariantLayout,
    RawMirFunction, Terminator,
};
use hew_runtime::machine_emit::{thread_emit_clear, thread_emit_drain, thread_emit_pending};
use hew_types::ResolvedTy;

// ── Pipeline builder ──────────────────────────────────────────────────────────

/// Build a minimal `IrPipeline` carrying:
/// - `TcpHandshake` machine layout: 2 unit states, 2 unit events.
/// - A unit `TcpHandshake__step` stub that emits two
///   `MachineEmitPlaceholder` instructions (indices 0 and 1).
/// - A `caller` fn that invokes the step stub.
///
/// The two emit instructions and the call wrapper are the substrate under
/// test; machine state/event transport is covered by the dispatch tests.
fn tcp_handshake_emit_pipeline() -> IrPipeline {
    let machine_name = "TcpHandshake".to_string();

    // Two unit states, two unit events.
    let variants = vec!["Closed", "SynReceived"]
        .into_iter()
        .map(|n| MachineVariantLayout {
            name: n.to_string(),
            field_tys: Vec::new(),
            field_names: Vec::new(),
        })
        .collect::<Vec<_>>();
    let events = vec!["SynReceive", "AckReceive"]
        .into_iter()
        .map(|n| MachineVariantLayout {
            name: n.to_string(),
            field_tys: Vec::new(),
            field_names: Vec::new(),
        })
        .collect::<Vec<_>>();
    let machine_layout = MachineLayout {
        name: machine_name.clone(),
        event_name: format!("{machine_name}Event"),
        tag_width: 1,
        variants,
        events,
    };

    // `TcpHandshake__step() -> ()`:
    //
    // Block 0: emit event 0, emit event 1, Return.
    //
    // A unit signature is sufficient for the wiring under test and avoids the
    // previous caller loading uninitialized aggregate locals before invoking
    // MCJIT, which made this oracle intermittently segfault under CI load.
    let step_fn = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::SynthesizedMachineStep {
            machine_name: machine_name.to_string(),
        },
        key: hew_mir::MirCallableKey::for_test(&format!("{machine_name}__step")),
        name: format!("{machine_name}__step"),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                // emit SynReceive (index 0) — unit event, no payload.
                Instr::MachineEmitPlaceholder {
                    event_idx: 0,
                    payload: Vec::new(),
                    machine_emit_id: 0xAAAA_BBBB_CCCC_DDDD,
                },
                // emit AckReceive (index 1) — unit event, no payload.
                Instr::MachineEmitPlaceholder {
                    event_idx: 1,
                    payload: Vec::new(),
                    machine_emit_id: 0xAAAA_BBBB_CCCC_DDDD,
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

    // `caller()` invokes the unit step stub and returns.
    // Block 0 → Call → Block 1 → Return.
    let caller = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        key: hew_mir::MirCallableKey::for_test("caller"),
        name: "caller".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Call {
                    callee: format!("{machine_name}__step"),
                    authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                        "{machine_name}__step",
                    ))
                    .map(hew_mir::CallAuthority::Runtime)
                    .unwrap_or_default(),
                    args: vec![],
                    dest: None,
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![],
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
        raw_mir: vec![step_fn, caller],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
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
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        lifecycle_registry: hew_hir::LifecycleRegistry::default(),
    }
}

/// Emit the pipeline to a `.ll` file and return the IR text.
fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = tempfile::tempdir().expect("create machine-emit scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: tmp.path(),
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
/// - The call passes the machine's stable `machine_id` (u64), a constant
///   `u32` event tag, and a null payload.
/// - The caller wraps the `__step` invocation with `hew_machine_emit_step_enter`
///   / `hew_machine_emit_step_exit_keep` (the deliver-design step-exit — NOT
///   the legacy drain-and-discard `hew_machine_emit_step_exit`, which is no
///   longer codegen-reachable).
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

    // The machine_id constant (0xAAAA_BBBB_CCCC_DDDD) must appear as an i64
    // argument — proves codegen transports MIR's machine_emit_id verbatim
    // rather than dropping or re-deriving it. LLVM prints i64 constants in
    // their signed two's-complement decimal form (the high bit is set here).
    assert!(
        ir.contains("i64 -6148895925951734307"),
        "IR must pass the machine_emit_id constant as an i64 argument:\n{ir}"
    );

    // The null payload pointer: LLVM 17+ opaque-pointer mode emits `null`
    // for `ptr_type.const_null()`.
    assert!(
        ir.contains("null"),
        "IR must pass a null payload pointer for unit events:\n{ir}"
    );
    assert!(
        ir.contains("@hew_machine_emit_step_enter")
            && ir.contains("@hew_machine_emit_step_exit_keep"),
        "caller must wrap the __step invocation with the deliver-design machine emit \
         enter / exit_keep calls:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_machine_emit_step_exit("),
        "codegen must NOT call the legacy drain-and-discard hew_machine_emit_step_exit \
         (only hew_machine_emit_step_exit_keep is codegen-reachable):\n{ir}"
    );
}

// ── JIT execution test ────────────────────────────────────────────────────────

/// JIT-compile and execute the caller; assert that the `__step` call is wrapped,
/// the outermost keep-exit preserves its events, and they remain in FIFO order.
///
/// ## Method
///
/// 1. Emit the pipeline to `.ll`.
/// 2. Parse the `.ll` back into an inkwell `Module`.
/// 3. Create an MCJIT `ExecutionEngine`, which resolves the module's `hew_*`
///    references against the runtime linked into this test binary through the
///    binary's own dynamic symbol table. `hew-codegen-rs/build.rs` is what
///    makes that table complete, on ELF and on Mach-O alike; see its
///    `export_runtime_symbols_to_jit_hosts` for why.
/// 4. Check every referenced declaration resolves, so a symbol the JIT cannot
///    find fails by name rather than as a bare SIGSEGV.
/// 5. Clear any stale events from prior tests on this thread.
/// 6. Invoke the `caller` function (which calls the step stub).
/// 7. The unit step stub emits event 0 then event 1 and returns cleanly
///    (no trap); `caller` then returns unit as well.
/// 8. Assert the outermost keep-exit preserved both events, then drain and
///    verify their machine identity and FIFO tag order.
///
/// This test is NOT `#[ignore]`d, unlike the MCJIT execution tests that were
/// removed alongside it. Those hand-listed a few symbols through
/// `add_global_mapping` and left every other reference to the engine's
/// process-symbol resolver, which saw nothing on either platform and left the
/// relocation at address 0. Step 3 removes the hand-list, so the set of
/// symbols the JIT can see is the set the binary actually links, and step 4
/// makes any remaining gap a named failure.
#[test]
#[cfg(unix)]
fn machine_emit_push_populates_thread_queue_in_fifo_order() {
    use inkwell::context::Context;
    use inkwell::memory_buffer::MemoryBuffer;
    use inkwell::targets::{InitializationConfig, Target};
    use inkwell::values::BasicValue;
    use inkwell::OptimizationLevel;

    // ── Compile to .ll ───────────────────────────────────────────────────────
    let pipeline = tcp_handshake_emit_pipeline();
    let tmp = tempfile::tempdir().expect("create machine-emit JIT scratch dir");
    let options = EmitOptions {
        module_name: "machine_emit_jit",
        out_dir: tmp.path(),
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

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed");

    // Fail closed on symbol resolution. MCJIT resolves a reference it cannot
    // find in the module through the process symbol table, and leaves it at
    // address 0 when that lookup misses too - reporting nothing, so the fault
    // only surfaces when JIT-compiled code calls through it, as a signal with
    // no diagnostic. Probe the same process table here, through the same
    // `dlsym` the engine's resolver uses, so an unresolvable runtime symbol
    // names itself instead.
    for declaration in module.get_functions() {
        if declaration.count_basic_blocks() != 0
            || declaration
                .as_global_value()
                .as_pointer_value()
                .get_first_use()
                .is_none()
        {
            // Defined in this module, or declared by the emitted prelude and
            // never referenced - neither reaches the symbol resolver.
            continue;
        }
        let symbol = declaration
            .get_name()
            .to_str()
            .expect("LLVM symbol names are UTF-8");
        let query = std::ffi::CString::new(symbol).expect("LLVM symbol names carry no NUL");
        // SAFETY: `query` is a live NUL-terminated C string; RTLD_DEFAULT
        // searches the already-loaded process image and loads nothing.
        let address = unsafe { libc::dlsym(libc::RTLD_DEFAULT, query.as_ptr()) };
        assert!(
            !address.is_null(),
            "the JIT-executed module references `{symbol}`, which this test binary does not \
             export for the JIT to resolve; calling it would fault on a null address. See \
             `export_runtime_symbols_to_jit_hosts` in hew-codegen-rs/build.rs"
        );
    }

    // Clear any stale events from a prior test on this thread.
    thread_emit_clear();
    assert_eq!(
        thread_emit_pending(),
        0,
        "thread queue must be empty before JIT call"
    );

    // Invoke `caller` which drives the step stub and triggers two pushes.
    // The unit step stub returns cleanly (no trap path) after the two emit
    // instructions.
    //
    // SAFETY: `caller` is compiled as `fn() -> i8` (unit mapped to i8 by
    // the codegen); the JIT-compiled code is for the host triple.
    let caller_fn = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i8>("caller")
            .expect("caller must be present in the JIT module")
    };
    unsafe { caller_fn.call() };

    // ── Assertions ───────────────────────────────────────────────────────────

    // The caller wrapped the step invocation; the deliver-design step-exit
    // (`hew_machine_emit_step_exit_keep`) does NOT drain — both pushed
    // events must still be queued for a later `take_emits`.
    assert_eq!(
        thread_emit_pending(),
        2,
        "the deliver-design step-exit must KEEP queued MachineEmitPlaceholder events, \
         not drain them"
    );

    let mut emitted = Vec::new();
    thread_emit_drain(|event, _append| {
        emitted.push((event.machine_id, event.tag, event.payload));
        Ok::<(), std::convert::Infallible>(())
    })
    .expect("draining the JIT-populated thread queue must succeed");
    assert_eq!(
        emitted,
        vec![
            (0xAAAA_BBBB_CCCC_DDDD, 0, std::ptr::null()),
            (0xAAAA_BBBB_CCCC_DDDD, 1, std::ptr::null()),
        ],
        "MachineEmitPlaceholder events must preserve machine identity, null unit payloads, \
         and source FIFO order"
    );
    assert_eq!(
        thread_emit_pending(),
        0,
        "the FIFO assertion must leave the thread-local queue clean"
    );
}
