//! Codegen verification for the P5-RX sub-stage 1 receive-ABI scaffolding:
//! the dormant `borrow_mode` discriminant threaded through the actor
//! dispatch trampoline and the borrow-load receipt primitive.
//!
//! This is the codegen half of the borrow-load receipt proof. The runtime
//! half — that `hew_msg_envelope_payload_ptr` hands back the borrowed
//! payload without consuming a refcount — is pinned by
//! `mailbox::tests::envelope_payload_ptr_is_borrow_only`. Here we assert the
//! trampoline actually *calls* that accessor on the borrow arm and routes
//! the loaded payload through a real control-flow merge (not a `select`),
//! and that the receive-handler ABI grew the trailing discriminant while the
//! sibling `__init` / `__on_start` ABIs (which share the `ActorHandler`
//! calling convention but are reached by different trampolines) did not.
//!
//! Everything asserted here is DORMANT at runtime: both schedulers pass
//! `borrow_mode == 0` and the envelope-mode dispatch guard fails closed
//! before any envelope node reaches dispatch, so the borrow arm is wired and
//! well-typed but never executed until the live send/guard sub-stages flip
//! it on. `emit_module`'s `Module::verify()` (exercised by `emit_ll_text`)
//! is the load-bearing shape check: a mis-arity'd handler call or a
//! malformed phi would fail verify here.

use std::path::Path;
#[cfg(unix)]
use std::sync::atomic::{AtomicUsize, Ordering};

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::lower_hir_module;
use hew_mir::{
    BasicBlock, EnumLayout, FunctionCallConv, Instr, IrPipeline, MachineVariantLayout, Place,
    RawMirFunction, Terminator,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn pipeline_from_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

fn emit_ll_text(pipeline: &hew_mir::IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-actor-borrow-abi-{module_name}"));
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
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Return the single `define ... @<name>(...)` header line for a function.
fn define_line<'a>(ll: &'a str, fn_name: &str) -> &'a str {
    let needle = format!("@{fn_name}(");
    ll.lines()
        .find(|l| l.trim_start().starts_with("define") && l.contains(&needle))
        .unwrap_or_else(|| panic!("no `define` line for `{fn_name}`; IR:\n{ll}"))
}

#[test]
fn dispatch_trampoline_emits_dormant_borrow_load_receipt() {
    // A Counter actor with init(i64), #[on(start)], and two receive handlers
    // (`increment(i64)`, `total() -> i64`): exercises both a payload-bearing
    // handler and a unit handler, plus the sibling init/on_start ABIs that
    // must NOT grow the discriminant. `main` deliberately does not `spawn`
    // (the IR-only harness uses `TypeCheckOutput::default()`, under which
    // spawn-destination typing is unavailable) — the actor's handlers and
    // dispatch trampoline are emitted from the definition regardless of use.
    let source = r#"
actor Counter {
    var count: i64;

    init(initial: i64) {
        count = initial;
    }

    receive fn increment(n: i64) {
        count = count + n;
    }

    receive fn total() -> i64 {
        count
    }

    #[on(start)]
    fn boot() {
        count += 1;
    }
}

fn main() -> i64 {
    0
}
"#;
    let pipeline = pipeline_from_source(source);
    let ll = emit_ll_text(&pipeline, "actor_counter_init");

    // The dispatch trampoline must be present and carry the 6th i32
    // discriminant param (matching `HewDispatchFn`).
    let tramp = define_line(&ll, "__hew_actor_dispatch_Counter");
    assert!(
        tramp.contains("ptr %0, ptr %1, i32 %2, ptr %3, i64 %4, i32 %5"),
        "dispatch trampoline missing the 6-param (borrow_mode) ABI; got:\n{tramp}"
    );

    // borrow_mode discriminant test + real control-flow split (not select):
    // a copy arm, a borrow arm that calls the envelope accessor, and a phi
    // merge feeding the payload loads.
    assert!(
        ll.contains("dispatch_is_borrow") && ll.contains("icmp ne i32"),
        "trampoline missing borrow_mode discriminant compare; IR:\n{ll}"
    );
    for block in ["borrow_src", "copy_src", "payload_src"] {
        assert!(
            ll.contains(block),
            "trampoline missing `{block}` basic block — borrow-load receipt \
             must use real control flow, not a select; IR:\n{ll}"
        );
    }
    assert!(
        ll.contains("call ptr @hew_msg_envelope_payload_ptr(ptr %3)"),
        "borrow arm must resolve the payload via hew_msg_envelope_payload_ptr \
         on the node `data` pointer; IR:\n{ll}"
    );
    assert!(
        ll.contains("phi ptr"),
        "trampoline must phi the copy/borrow payload source pointer; IR:\n{ll}"
    );

    // GATE 2 (fail-closed null guard): the borrow arm must null-check the
    // borrowed payload pointer and divert a null to `hew_panic` (never a
    // silent by-value load). The guard is real control flow with a dedicated
    // null block; the load only ever sees a proven-non-null pointer.
    for block in ["borrow_payload_null", "borrow_payload_ok"] {
        assert!(
            ll.contains(block),
            "borrow arm missing `{block}` — Gate 2 null guard must use real \
             control flow before the payload load; IR:\n{ll}"
        );
    }
    assert!(
        ll.contains("call void @hew_panic()"),
        "borrow arm null guard must fail closed via hew_panic; IR:\n{ll}"
    );
    assert!(
        ll.contains("declare void @hew_panic()"),
        "missing extern declaration of hew_panic for the null guard; IR:\n{ll}"
    );

    // The envelope accessor must be declared with the borrow-only signature.
    assert!(
        ll.contains("declare ptr @hew_msg_envelope_payload_ptr(ptr)"),
        "missing extern declaration of hew_msg_envelope_payload_ptr; IR:\n{ll}"
    );

    // Receive-handler ABI grew the trailing i32 discriminant...
    let inc = define_line(&ll, "Counter__recv__increment");
    assert!(
        inc.contains("(ptr %0, i64 %1, i32 %2)"),
        "receive handler `increment` must carry the trailing borrow_mode i32; \
         got:\n{inc}"
    );
    let total = define_line(&ll, "Counter__recv__total");
    assert!(
        total.contains("(ptr %0, i32 %1)"),
        "receive handler `total` (unit payload) must carry the trailing \
         borrow_mode i32; got:\n{total}"
    );

    // ...but the sibling ActorHandler-convention functions reached by other
    // trampolines must NOT — their ABI is unchanged this sub-stage.
    let init = define_line(&ll, "Counter__init");
    assert!(
        init.contains("(ptr %0, i64 %1)"),
        "init ABI must be unchanged (no trailing borrow_mode); got:\n{init}"
    );
    let on_start = define_line(&ll, "Counter__on_start");
    assert!(
        on_start.contains("(ptr %0)"),
        "on_start ABI must be unchanged (no trailing borrow_mode); got:\n{on_start}"
    );
}

/// Return the body lines of `define ... @<name>(...)` up to the closing `}`.
fn define_body<'a>(ll: &'a str, fn_name: &str) -> Vec<&'a str> {
    let needle = format!("@{fn_name}(");
    let mut out = Vec::new();
    let mut in_fn = false;
    for line in ll.lines() {
        if line.trim_start().starts_with("define") && line.contains(&needle) {
            in_fn = true;
        }
        if in_fn {
            out.push(line);
            if line.trim() == "}" {
                break;
            }
        }
    }
    assert!(!out.is_empty(), "no body for `{fn_name}`; IR:\n{ll}");
    out
}

/// GATE 1 — receive-parameter ownership follows the dispatch mode.
///
/// In copy mode (`borrow_mode == 0`) the handler owns the mailbox-transferred
/// payload and must release an unused heap-owning parameter at function exit.
/// In live-borrow mode the envelope remains the sole owner, so the identical
/// drop must be suppressed and `hew_msg_envelope_release` performs the one
/// release when the node is freed.
///
/// We pin both halves at the lowering level: a `string` payload (a non-`Copy`,
/// heap-owning type) received into a handler with an unused binding emits one
/// real destructor in the `borrow_mode == 0` arm, a merge that bypasses it for
/// live borrows, and no retain or envelope release in the handler itself.
#[test]
fn receive_handler_payload_drop_is_borrow_mode_gated() {
    let source = r#"
actor Inbox {
    receive fn store(s: string) {}
}

fn main() -> i64 {
    0
}
"#;
    let pipeline = pipeline_from_source(source);
    let ll = emit_ll_text(&pipeline, "inbox_string_recv");

    // The handler must carry the borrow_mode i32 ABI and the string payload.
    let header = define_line(&ll, "Inbox__recv__store");
    assert!(
        header.contains("(ptr %0, ptr %1, i32 %2)"),
        "store handler must take (ctx, string payload, borrow_mode i32); got:\n{header}"
    );

    let body = define_body(&ll, "Inbox__recv__store").join("\n");
    assert!(
        body.contains("borrow_drop_copy_only") && body.contains("borrow_drop_merge"),
        "the received parameter's drop must be enclosed by a real \
         borrow-mode branch and merge; Body:\n{body}"
    );
    assert!(
        body.contains("borrow_drop_is_copy") && body.contains("icmp eq i32 %2"),
        "the received parameter may be released only when borrow_mode == 0; \
         Body:\n{body}"
    );
    assert_eq!(
        body.matches("call void @hew_string_drop").count(),
        1,
        "copy mode must contain exactly one release for the unused transferred \
         string parameter; Body:\n{body}"
    );
    for forbidden in [
        "hew_string_free",
        "hew_vec_free",
        "hew_vec_drop",
        "hew_msg_envelope_release",
        "drop_in_place",
    ] {
        assert!(
            !body.contains(forbidden),
            "GATE 1 violated: receive handler emits unrelated ownership action \
             `{forbidden}`; Body:\n{body}"
        );
    }

    // An unused parameter never escapes into another owner, so neither mode
    // needs a retain.
    assert!(
        !body.contains("hew_string_clone") && !body.contains("borrow_clone"),
        "a non-escaping borrowed view must not be retained — no `hew_string_clone` \
         should appear in the handler. Body:\n{body}"
    );
}

/// GATE 1 — every wired non-String owning message shape follows the same
/// copy-owner/live-view split as String.
///
/// The MIR elaborator emits distinct releases for bytes, a plain Vec, and a
/// recursively owning record. All three must sit exclusively in the
/// `borrow_mode == 0` region: a live envelope receipt owns the complete payload
/// bytes and its drop glue performs the one recursive release.
#[test]
fn receive_handler_non_string_payload_drops_are_borrow_mode_gated() {
    let source = r#"
type Packet {
    label: string;
    values: Vec<i64>;
}

actor Inbox {
    receive fn take_bytes(payload: bytes) {}
    receive fn take_values(values: Vec<i64>) {}
    receive fn take_packet(packet: Packet) {}
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "inbox_non_string_recv");
    for (handler, release) in [
        ("Inbox__recv__take_bytes", "@hew_bytes_drop"),
        ("Inbox__recv__take_values", "call void @hew_vec_free("),
        (
            "Inbox__recv__take_packet",
            "@__hew_record_drop_inplace_Packet",
        ),
    ] {
        let body = define_body(&ll, handler).join("\n");
        assert!(
            body.contains("borrow_drop_copy_only") && body.contains("borrow_drop_merge"),
            "{handler} must gate its owning non-String parameter drop through \
             a real borrow-mode branch+merge; Body:\n{body}"
        );
        assert!(
            body.contains("borrow_drop_is_copy") && body.contains("icmp eq i32 %2"),
            "{handler} may release its message view only when borrow_mode == 0; \
             Body:\n{body}"
        );
        assert_eq!(
            body.matches(release).count(),
            1,
            "{handler} copy-mode arm must contain exactly one `{release}` call; \
             Body:\n{body}"
        );
        assert!(
            !body.contains("@hew_msg_envelope_release"),
            "{handler} must leave envelope release to the scheduler; Body:\n{body}"
        );
        assert!(
            !body.contains("borrow_escape_trap"),
            "{handler} only discards its message view, so the gated drop is \
             sufficient and the live-borrow path must remain usable; Body:\n{body}"
        );
    }
}

#[cfg(unix)]
static GENERATED_STRING_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

#[cfg(unix)]
unsafe extern "C" fn count_generated_string_drop(value: *mut std::ffi::c_char) {
    GENERATED_STRING_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    // SAFETY: the generated message callback passes the live Hew String owner
    // embedded in the queued payload. This shim observes, then delegates to,
    // the production destructor the callback was generated to invoke.
    unsafe { hew_runtime::string::hew_string_drop(value) };
}

#[cfg(unix)]
fn generated_string_drop_count() -> usize {
    GENERATED_STRING_DROP_COUNT.load(Ordering::SeqCst)
}

/// Composition oracle for generated owning-message cleanup on terminal ask.
///
/// The runtime half deliberately owns no worker: a real ask node containing a
/// heap-owning Hew `string` is queued while its actor is `Idle`, then the
/// production stop path wins `Idle -> Stopped` before dispatch. The callback
/// installed on that actor is not a hand-written stand-in: MCJIT executes
/// codegen's actual `__hew_message_drop_Inbox` function. Its call to
/// `hew_string_drop` is forwarded through a counted shim, making a leak (zero
/// calls) and a double-drop (two calls, normally also allocator failure)
/// independently visible.
#[test]
#[cfg(unix)]
fn terminal_ask_runs_generated_owning_payload_drop_once_and_wakes_waiter() {
    use hew_runtime::internal::types::{HewActorState, HewError, HEW_REPLY_FAIL_ACTOR_STOPPED};
    use inkwell::context::Context;
    use inkwell::memory_buffer::MemoryBuffer;
    use inkwell::targets::{InitializationConfig, Target};
    use inkwell::OptimizationLevel;

    GENERATED_STRING_DROP_COUNT.store(0, Ordering::SeqCst);

    let source = r#"
actor Inbox {
    receive fn hold(value: string) -> i64 {
        0
    }
}

fn main() -> i64 {
    0
}
"#;
    let pipeline = pipeline_from_source(source);
    let msg_type = pipeline
        .actor_layouts
        .iter()
        .find(|layout| layout.name == "Inbox")
        .and_then(|layout| {
            layout
                .handlers
                .iter()
                .find(|handler| handler.name == "hold")
        })
        .map(|handler| handler.msg_type)
        .expect("Inbox.hold must have a protocol-issued message id");
    let tmp = tempfile::tempdir().expect("create terminal-ask composition scratch dir");
    let options = EmitOptions {
        module_name: "terminal_ask_owning_payload",
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit composition module");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("composition module must emit LLVM IR");

    Target::initialize_native(&InitializationConfig::default()).expect("initialize native target");
    let context = Context::create();
    let buffer = MemoryBuffer::create_from_file(ll_path).expect("read composition LLVM IR");
    let module = context
        .create_module_from_ir(buffer)
        .expect("parse composition LLVM IR");
    if let Some(constructors) = module.get_global("llvm.global_ctors") {
        // This oracle executes one internal callback, not module startup.
        // Removing the constructor keeps unrelated codec-registration runtime
        // symbols outside the deliberately minimal MCJIT mapping surface.
        // SAFETY: the parsed module is exclusively owned here, before JIT
        // engine creation, and no instruction refers to this appending global.
        unsafe { constructors.delete() };
    }
    let string_drop_decl = module
        .get_function("hew_string_drop")
        .expect("generated callback must declare hew_string_drop");
    // Turn the callback's runtime declaration into a JIT-local forwarding
    // definition. Embedding the host address avoids relying on the process
    // dynamic-symbol table (Rust test binaries do not export this shim), while
    // preserving the generated callback's exact call and payload walk.
    let string_drop_entry = context.append_basic_block(string_drop_decl, "host_observer");
    let builder = context.create_builder();
    builder.position_at_end(string_drop_entry);
    let ptr_ty = context.ptr_type(inkwell::AddressSpace::default());
    let shim_address = context
        .i64_type()
        .const_int(
            count_generated_string_drop as *const () as usize as u64,
            false,
        )
        .const_to_pointer(ptr_ty);
    let string_drop_ty = string_drop_decl.get_type();
    builder
        .build_indirect_call(
            string_drop_ty,
            shim_address,
            &[string_drop_decl
                .get_first_param()
                .expect("string drop parameter")
                .into()],
            "observe_string_drop",
        )
        .expect("build string drop observer call");
    builder
        .build_return(None)
        .expect("return from string drop observer");
    module
        .verify()
        .expect("composition module must remain valid after installing the observer");
    let generated_drop_fn = module
        .get_function("__hew_message_drop_Inbox")
        .expect("actor codegen must emit the typed message drop callback");
    generated_drop_fn.set_linkage(inkwell::module::Linkage::External);
    let generated_drop_helper = module
        .get_function(&format!("__hew_message_drop_Inbox_{msg_type}"))
        .expect("actor codegen must emit the typed payload drop helper");
    generated_drop_helper.set_linkage(inkwell::module::Linkage::External);

    let engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create composition JIT");
    let callback = unsafe {
        engine.get_function::<hew_runtime::mailbox::HewMessageDropFn>("__hew_message_drop_Inbox")
    }
    .expect("resolve generated message drop callback");
    // SAFETY: codegen emits this named function with the HewMessageDropFn ABI,
    // and `engine` remains live through the runtime call.
    let generated_drop = unsafe { callback.as_raw() };

    // A header-aware, heap-owning Hew String. The payload is its one-field
    // generated carrier; successful ask submission transfers that owner.
    let owned = unsafe { hew_runtime::string::hew_string_from_char(i32::from(b'x')) };
    assert!(!owned.is_null(), "allocate owning String payload");
    let mut payload = owned;

    // SAFETY: `payload` exactly matches `Inbox.hold(string)`'s generated
    // one-pointer message layout. Successful submission transfers `owned`.
    let report = unsafe {
        hew_runtime::actor::composition_test_support::terminalize_queued_ask(
            generated_drop,
            msg_type,
            (&raw mut payload).cast(),
            std::mem::size_of_val(&payload),
            generated_string_drop_count,
        )
    };

    assert_eq!(report.send_result, HewError::Ok as i32);
    assert_eq!(report.queued_before_stop, 1, "ask must be queued");
    assert_eq!(
        report.payload_drops_before_stop, 0,
        "submission must transfer, not prematurely destroy, the payload owner"
    );
    assert_eq!(
        report.actor_state,
        HewActorState::Stopped as i32,
        "terminalization must win before dispatch"
    );
    assert!(
        report.wait_returned_null,
        "terminal reclaim must wake the ask waiter"
    );
    assert_eq!(
        report.failure_kind, HEW_REPLY_FAIL_ACTOR_STOPPED,
        "waiter must classify the terminal wake as ActorStopped"
    );
    assert_eq!(
        report.queued_after_stop, 0,
        "terminal reclaim must retire the exact ask node"
    );
    assert_eq!(
        GENERATED_STRING_DROP_COUNT.load(Ordering::SeqCst),
        1,
        "generated payload callback must release the owning String exactly once"
    );
}

/// A non-String live-borrow payload cannot yet be retained for re-send.
///
/// Copy mode still owns the Vec and may transfer it to the next mailbox, but a
/// live receipt is an envelope view. Re-sending that view would give two
/// envelopes the same Vec owner and double-release it, so the handler must trap
/// before user code whenever `borrow_mode != 0`.
#[test]
fn receive_handler_non_string_resend_fails_closed_under_live_borrow() {
    let source = r#"
actor Consumer {
    receive fn take(values: Vec<i64>) {}
}

actor Relay {
    let consumer: LocalPid<Consumer>;

    receive fn forward(values: Vec<i64>) {
        consumer.take(values);
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "relay_vec_resend");
    let body = define_body(&ll, "Relay__recv__forward").join("\n");

    assert!(
        body.contains("borrow_escape_trap") && body.contains("borrow_escape_ok"),
        "a non-String re-send must arm the live-borrow fail-closed entry trap; \
         Body:\n{body}"
    );
    assert!(
        body.contains("borrow_escape_is_live") && body.contains("icmp ne i32 %2"),
        "the re-send trap must reject only borrow_mode != 0; Body:\n{body}"
    );
    assert!(
        body.contains("call void @hew_panic()") && body.contains("unreachable"),
        "the live-borrow re-send arm must panic and terminate before the send; \
         Body:\n{body}"
    );
    assert_eq!(
        body.matches("@hew_actor_send_by_id").count(),
        1,
        "copy mode must retain the ordinary single mailbox transfer; Body:\n{body}"
    );
    assert!(
        !body.contains("borrow_drop_copy_only") && !body.contains("borrow_drop_merge"),
        "the forwarding handler must not keep a scope-exit Vec release after \
         transferring the payload to the next mailbox; Body:\n{body}"
    );
    // The release sits on the shared UNDELIVERED edge — the block both
    // undelivered outcomes flow through: `ErrActorStopped` (the documented
    // no-op, which then continues to the successor) and every other non-zero
    // status (which traps in `actor_send_fail`). Placing it on the trap block
    // alone leaked the payload on every send to an already-terminal actor.
    let undelivered_label = body
        .find("actor_send_undelivered:")
        .expect("undelivered-send recovery block must be present");
    let fail_label = body
        .find("actor_send_fail:")
        .expect("send failure trap block must be present");
    let release = body
        .find("call void @hew_vec_free_owned")
        .expect("an undelivered send must reclaim its Vec carrier");
    assert!(
        release > undelivered_label
            && release < fail_label
            && body.matches("call void @hew_vec_free_owned").count() == 1,
        "the sole Vec release must sit on the shared undelivered-send edge, \
         before the fail-closed trap block, so BOTH the stopped-recipient no-op \
         and the trap release it exactly once; Body:\n{body}"
    );
}

/// GATE 2a (A625) — field-store escape retains under a borrow_mode gate.
///
/// `last = s` carries the borrowed view into an owned actor field. The store's
/// owned slot must take its OWN retained owner so the field drop and the
/// envelope release each free a distinct buffer. Codegen emits a runtime
/// `borrow_mode != 0` branch that retains via `hew_string_clone` (copy mode
/// stores the original unchanged).
#[test]
fn receive_handler_field_store_emits_borrow_gated_retain() {
    let source = r#"
actor Inbox {
    var last: string;

    init() {
        last = "";
    }

    receive fn keep(s: string) {
        last = s;
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "inbox_field_retain");
    let body = define_body(&ll, "Inbox__recv__keep").join("\n");

    assert!(
        body.contains("field_store_borrow_clone") && body.contains("field_store_borrow_merge"),
        "field-store escape must lower the retain through a real borrow_mode \
         branch+merge (not a select); Body:\n{body}"
    );
    assert!(
        body.contains("call ptr @hew_string_clone"),
        "field-store escape must retain the borrowed view via hew_string_clone; \
         Body:\n{body}"
    );
    assert!(
        body.contains("icmp ne i32 %2"),
        "the retain must be gated on the trailing borrow_mode i32 (param %2) \
         being non-zero; Body:\n{body}"
    );
    assert!(
        body.contains("declare ptr @hew_string_clone(ptr)")
            || ll.contains("declare ptr @hew_string_clone(ptr)"),
        "missing extern declaration of hew_string_clone; IR:\n{ll}"
    );

    let begin = body
        .find("call i1 @hew_dispatch_state_cleanup_begin_replace")
        .unwrap_or_else(|| panic!("state replacement must enter its fatal phase; Body:\n{body}"));
    let clone = body
        .find("call ptr @hew_string_clone")
        .expect("borrow-gated replacement clone");
    let materialize = body
        .find("store ptr %field_store_owner, ptr %state_f0_replacement")
        .expect("actual replacement materialization");
    let old_release = body
        .find("call void @hew_string_drop")
        .expect("old actor-state String release");
    let no_source_prepare = body
        .find("call void @hew_dispatch_state_cleanup_prepare")
        .expect("no-source preparation branch");
    let live_store = body
        .lines()
        .find(|line| line.contains("store ptr %field_store_owner, ptr %actor_state_field_0_ptr"))
        .and_then(|line| body.find(line))
        .unwrap_or_else(|| panic!("missing final live state store; Body:\n{body}"));
    assert!(
        begin < clone
            && clone < materialize
            && materialize < old_release
            && old_release < no_source_prepare
            && no_source_prepare < live_store,
        "state replacement ordering must be begin < clone/materialize < old release < no-source prepare < live store; Body:\n{body}"
    );
}

#[test]
fn looped_state_store_reuses_one_entry_replacement_scratch() {
    let source = r#"
actor LoopWriter {
    var value: i64;

    init() {
        value = 0;
    }

    receive fn write(n: i64) {
        var i: i64 = 0;
        while i < 2 {
            value = n;
            i += 1;
        }
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(
        &pipeline_from_source(source),
        "loop_state_replacement_scratch",
    );
    let body = define_body(&ll, "LoopWriter__recv__write").join("\n");
    assert_eq!(
        body.matches("%state_f0_replacement = alloca i64").count(),
        1,
        "a looped state assignment must reuse one scratch alloca; Body:\n{body}"
    );
    let alloca = body
        .find("%state_f0_replacement = alloca i64")
        .expect("replacement scratch alloca");
    let first_loop_block = body
        .find("\nbb0:")
        .unwrap_or_else(|| panic!("loop control flow must be present; Body:\n{body}"));
    assert!(
        alloca < first_loop_block,
        "replacement scratch must live in the entry/alloca prologue, never the loop body; Body:\n{body}"
    );
}

/// GATE 2a (A625) — return-position escape retains under a borrow_mode gate.
///
/// `return s` lowers as `Move { dest: ReturnSlot, src }`. The returned value
/// must be the handler's own retained owner so the caller's drop and the
/// envelope release free distinct buffers.
#[test]
fn receive_handler_return_emits_borrow_gated_retain() {
    let source = r#"
actor Echo {
    let n: i64;

    init() {
        n = 0;
    }

    receive fn echo(s: string) -> string {
        s
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "echo_return_retain");
    let body = define_body(&ll, "Echo__recv__echo").join("\n");

    assert!(
        body.contains("move_return_borrow_clone") && body.contains("move_return_borrow_merge"),
        "return-position escape must lower the retain through a real borrow_mode \
         branch+merge; Body:\n{body}"
    );
    assert!(
        body.contains("call ptr @hew_string_clone"),
        "return-position escape must retain via hew_string_clone; Body:\n{body}"
    );
    assert!(
        body.contains("icmp ne i32 %2"),
        "return retain must be gated on the trailing borrow_mode i32; Body:\n{body}"
    );
}

/// GATE 2a (A625) — a discarded owned local derived from a borrowed view has
/// its scope-exit drop SUPPRESSED under borrow mode.
///
/// `let t = s;` moves the borrowed view into an owned local `t` (taint
/// propagates). When `t` is otherwise unused it would be dropped at scope exit;
/// under a live borrow receipt that drop would free the envelope-owned buffer.
/// Codegen gates the drop on `borrow_mode == 0` (copy mode keeps its private
/// owner and drops normally; borrow mode skips the drop, leaving the single
/// release to the envelope).
#[test]
fn receive_handler_discarded_owned_local_suppresses_drop() {
    let source = r#"
actor TestSink {
    var n: i64;

    init() {
        n = 0;
    }

    receive fn discard(s: string) {
        let t = s;
        n = n + 1;
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "sink_discard_suppress");
    let body = define_body(&ll, "TestSink__recv__discard").join("\n");

    assert!(
        body.contains("borrow_drop_copy_only") && body.contains("borrow_drop_merge"),
        "a discarded borrowed-derived owned local must gate its drop through a \
         real borrow_mode branch+merge; Body:\n{body}"
    );
    assert!(
        body.contains("borrow_drop_is_copy") && body.contains("icmp eq i32 %2"),
        "the drop must be suppressed unless borrow_mode == 0 (copy mode); \
         Body:\n{body}"
    );
    assert!(
        body.contains("hew_string_drop"),
        "the copy-mode arm must still drop its private owner; Body:\n{body}"
    );
    // The borrowed view itself is not cloned for a plain discard (no escape
    // into a persisting owner).
    assert!(
        !body.contains("hew_string_clone"),
        "a discarded (non-escaping-to-owner) view must not be retained; \
         Body:\n{body}"
    );
}

/// RUNTIME FAIL-CLOSED (A625) — a borrowed view that escapes through a vector
/// this stage does not retain still COMPILES (copy mode is safe today), but the
/// handler carries a `borrow_mode != 0` entry trap so a future live borrow
/// receipt fails closed instead of double-freeing.
///
/// `let t = id(s); last = t` routes the borrowed handle through a call whose
/// result aliases the envelope-owned buffer and then stores it into an owned
/// field. Stage 2a retains only at four ratified sinks (field store, return,
/// re-send, moved-into owned local); a call argument is not one of them, so the
/// laundered handle would create a second owner under a live borrow receipt.
/// Under `borrow_mode == 0` (the only mode wired today) the handler owns a
/// private copy and is safe — so codegen must NOT reject it, but MUST guard the
/// live path. LESSONS: boundary-fail-closed.
#[test]
fn receive_handler_call_transitive_escape_traps_under_live_borrow() {
    let source = r#"
fn id(x: string) -> string {
    x
}

actor Relay {
    var last: string;

    init() {
        last = "";
    }

    receive fn relay(s: string) {
        let t = id(s);
        last = t;
    }
}

fn main() -> i64 {
    0
}
"#;
    // Copy-mode-safe program must still compile (no fail-closed rejection).
    let ll = emit_ll_text(&pipeline_from_source(source), "relay_call_transitive");
    let body = define_body(&ll, "Relay__recv__relay").join("\n");

    assert!(
        body.contains("borrow_escape_trap") && body.contains("borrow_escape_ok"),
        "an unhandled borrow escape must arm a runtime fail-closed entry trap; \
         Body:\n{body}"
    );
    assert!(
        body.contains("borrow_escape_is_live") && body.contains("icmp ne i32 %2"),
        "the trap must be gated on the trailing borrow_mode i32 being non-zero \
         (live borrow receipt); Body:\n{body}"
    );
    assert!(
        body.contains("call void @hew_panic()") && body.contains("unreachable"),
        "the live-borrow arm must fail closed via hew_panic + unreachable; \
         Body:\n{body}"
    );
}

/// RUNTIME FAIL-CLOSED (A625) — composite construction over a borrowed view is
/// also trapped under a live borrow receipt (the handle would be shared into an
/// aggregate whose recursive drop releases the borrowed buffer).
#[test]
fn receive_handler_record_construction_escape_traps_under_live_borrow() {
    let source = r#"
type Wrap {
    name: string,
}

actor Builder {
    var last: string;

    init() {
        last = "";
    }

    receive fn build(s: string) {
        let w = Wrap { name: s };
        last = w.name;
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "builder_record_ctor");
    let body = define_body(&ll, "Builder__recv__build").join("\n");

    assert!(
        body.contains("borrow_escape_trap") && body.contains("borrow_escape_is_live"),
        "an aggregate borrow escape must arm the runtime fail-closed entry trap; \
         Body:\n{body}"
    );
    assert!(
        body.contains("call void @hew_panic()"),
        "the live-borrow arm must fail closed via hew_panic; Body:\n{body}"
    );
}

/// NEGATIVE (A625) — a handler whose borrowed view escapes ONLY through the four
/// ratified sinks (here a field store) carries NO entry trap: those sinks retain
/// correctly under both modes, so the live path is safe and must not be guarded.
#[test]
fn receive_handler_handled_sink_has_no_escape_trap() {
    let source = r#"
actor Inbox {
    var last: string;

    init() {
        last = "";
    }

    receive fn keep(s: string) {
        last = s;
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "inbox_no_trap");
    let body = define_body(&ll, "Inbox__recv__keep").join("\n");

    assert!(
        !body.contains("borrow_escape_trap") && !body.contains("borrow_escape_is_live"),
        "a handler escaping only through handled sinks must NOT arm the \
         fail-closed entry trap; Body:\n{body}"
    );
    // ...and it still retains at the field-store sink.
    assert!(
        body.contains("call ptr @hew_string_clone"),
        "the field-store sink must still retain the borrowed view; Body:\n{body}"
    );
}

/// P0 ANTI-REGRESSION (A625) — a borrowed `String` receive view moved into an
/// OWNED AGGREGATE payload (`Place::MachineVariant`, the per-field aggregate
/// store MIR emits for machine/enum-variant construction) must take its OWN
/// owner via a `borrow_mode`-gated `hew_string_clone` BEFORE the aggregate
/// move — exactly like the field-store / return / re-send sinks.
///
/// This pins the fix for the Stage-2a security-review blocker: the escape
/// detector previously treated ALL `Instr::Move` as handled and skipped them
/// before checking source operands, while the generic `Move` retain fired only
/// for `Place::ReturnSlot`. A tainted view moved into `Place::MachineVariant`
/// therefore entered an owned aggregate with NEITHER a retain NOR the
/// fail-closed entry trap — a latent double-free/UAF against
/// `hew_msg_envelope_release` once live `borrow_mode == 1` is enabled.
///
/// Hand-assembled MIR (no surface drop-elaboration in the way) so the assertion
/// targets the aggregate move directly. PRE-FIX this test FAILS: no
/// `hew_string_clone` is emitted (the move is not a recognised retain sink) and
/// no `borrow_escape_trap` is armed (the blanket `Move` skip swallows it).
/// POST-FIX the gated retain appears and exactly-once holds (the aggregate's
/// drop frees the clone; the envelope release frees the original).
fn boxed_enum_recv_pipeline() -> IrPipeline {
    // enum Boxed { Hold { s: string }; Empty; }
    let boxed_ty = ResolvedTy::Named {
        name: "Boxed".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let enum_layout = EnumLayout {
        name: "Boxed".to_string(),
        tag_width: 1, // ceil(log2(2)) = 1 bit for 2 variants
        variants: vec![
            MachineVariantLayout {
                name: "Hold".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    };
    // Receive handler `Keeper.stash(s: string)`:
    //   local 0: string   // the borrowed receive param (taint root)
    //   local 1: Boxed     // the owned aggregate under construction
    //   local 2: i64       // tag constant scratch
    // Body:
    //   ConstI64 { dest: local 2, value: 0 }                       // Hold tag
    //   Move { dest: MachineTag(1), src: local 2 }
    //   Move { dest: MachineVariant{1, variant 0, field 0}, src: local 0 }
    //   Return
    let handler = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::SynthesizedActorHandler {
            kind: hew_mir::ActorHandlerKind::Receive,
            actor_layout_key: "Keeper".to_string(),
        },
        key: hew_mir::MirCallableKey::for_test("Keeper__recv__stash"),
        name: "Keeper__recv__stash".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![ResolvedTy::String],
        locals: vec![ResolvedTy::String, boxed_ty, ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::EnterContext,
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 0,
                },
                Instr::Move {
                    dest: Place::MachineTag(1),
                    src: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::MachineVariant {
                        local: 1,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                    src: Place::Local(0),
                },
                Instr::ExitContext,
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
    crate::mir_fixture::complete_stages(IrPipeline {
        entry_exit_plan: None,
        raw_mir: vec![handler],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
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
    })
}

#[test]
fn receive_handler_move_into_owned_aggregate_retains_under_live_borrow() {
    let ll = emit_ll_text(&boxed_enum_recv_pipeline(), "keeper_aggregate_move");
    let body = define_body(&ll, "Keeper__recv__stash").join("\n");

    // (a) The borrowed view is RETAINED before it enters the owned aggregate.
    // PRE-FIX this assertion fails: the aggregate move emitted no clone.
    assert!(
        body.contains("call ptr @hew_string_clone"),
        "a borrowed view moved into an owned aggregate (Place::MachineVariant) \
         must retain via hew_string_clone before the aggregate store; Body:\n{body}"
    );
    // The retain is a real borrow_mode-gated branch+merge (not an unconditional
    // clone that would leak a refcount in copy mode).
    assert!(
        body.contains("move_owned_aggregate_borrow_clone")
            && body.contains("move_owned_aggregate_borrow_merge"),
        "the aggregate retain must lower through a real borrow_mode branch+merge; \
         Body:\n{body}"
    );
    assert!(
        body.contains("icmp ne i32 %2"),
        "the aggregate retain must be gated on the trailing borrow_mode i32 \
         (param %2) being non-zero; Body:\n{body}"
    );
    // (b) Because the aggregate move IS a handled retain sink post-fix, the
    // handler must NOT also arm the coarse fail-closed entry trap (a buggy fix
    // that emitted both clone AND panic would leave borrow mode unusable).
    assert!(
        !body.contains("borrow_escape_trap") && !body.contains("borrow_escape_is_live"),
        "a retained aggregate sink must NOT also arm the fail-closed entry trap; \
         Body:\n{body}"
    );
    assert!(
        body.contains("declare ptr @hew_string_clone(ptr)")
            || ll.contains("declare ptr @hew_string_clone(ptr)"),
        "missing extern declaration of hew_string_clone; IR:\n{ll}"
    );
}

#[test]
fn receive_handler_resend_materializes_snapshot_owner() {
    let source = r#"
actor Consumer {
    var last: string;
    receive fn take(value: string) {
        last = value;
    }
}

actor Relay {
    var consumer: LocalPid<Consumer>;
    var last: string;
    receive fn forward(value: string) {
        consumer.take(value);
        last = value;
    }
}

fn main() -> i64 {
    0
}
"#;
    let ll = emit_ll_text(&pipeline_from_source(source), "relay_resend");
    let body = define_body(&ll, "Relay__recv__forward").join("\n");

    // Snapshot-send resolves the still-live receive parameter as
    // `SnapshotRetain` and lowers its `ValueSnapshotClone` unconditionally.
    // That fresh owner replaces the old borrow_mode branch+merge retain.
    let clone_pos = body
        .find("%snapshot_string = call ptr @hew_string_clone")
        .unwrap_or_else(|| {
            panic!("a re-sent borrowed view must materialize an owned snapshot; Body:\n{body}")
        });
    let carrier_store = body
        .lines()
        .find(|line| {
            line.trim_start()
                .starts_with("store ptr %snapshot_string, ptr %local_")
        })
        .unwrap_or_else(|| {
            panic!("the snapshot owner must be stored in a fresh send carrier; Body:\n{body}")
        });
    let carrier = carrier_store
        .split(", ptr ")
        .nth(1)
        .and_then(|tail| tail.split(',').next())
        .expect("snapshot carrier store shape");
    let send_line = body
        .lines()
        .find(|line| line.contains("@hew_actor_send_by_id"))
        .unwrap_or_else(|| panic!("the handler must still emit the actor send; Body:\n{body}"));
    let send_pos = body.find(send_line).expect("send line is in body");
    assert!(
        clone_pos < send_pos && send_line.contains(&format!("ptr {carrier},")),
        "the owned snapshot carrier must feed the actor send; carrier={carrier}; Body:\n{body}"
    );
    assert!(
        body.contains("field_store_borrow_clone") && body.contains("field_store_borrow_merge"),
        "the borrowed source used after the send must independently take ownership \
         when stored in Relay state; Body:\n{body}"
    );
    assert!(
        !body.contains("borrow_escape_trap"),
        "the snapshot send and retained state store are both handled borrow sinks; Body:\n{body}"
    );
}
