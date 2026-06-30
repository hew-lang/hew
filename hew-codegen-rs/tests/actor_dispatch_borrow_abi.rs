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

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::lower_hir_module;
use hew_mir::{
    BasicBlock, EnumLayout, FunctionCallConv, Instr, IrPipeline, MachineVariantLayout, Place,
    RawMirFunction, Terminator,
};
use hew_types::{module_registry::ModuleRegistry, BuiltinType, Checker, ResolvedTy};

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

/// GATE 1 — non-owning receive lowering.
///
/// A receive handler binds its message payload as a borrow: the single
/// final drop of an aliased payload is owned by the envelope and runs once
/// in `hew_msg_envelope_release` when the node is freed. The handler MUST
/// therefore emit NO drop / release / free of the received binding — a
/// handler-side drop on top of the envelope release would double-free a
/// destructor-bearing payload (String / Vec / Arc).
///
/// We pin this at the lowering level: a `string` payload (a non-`Copy`,
/// heap-owning type) received into a handler with an unused binding emits a
/// body that does not call any string/collection destructor. An *owned*
/// param would be dropped at function exit (it would land in the MIR
/// `owned_locals` set); a borrowed receive binding is never added there
/// (`hew-mir/src/lower.rs` `lower_params`), so no drop is emitted.
#[test]
fn receive_handler_does_not_drop_borrowed_payload() {
    let source = r#"
actor Inbox {
    var count: i64;

    init() {
        count = 0;
    }

    receive fn store(s: string) {
        count = count + 1;
    }
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

    // The handler body must NOT release/drop/free the borrowed payload —
    // the sole drop is the envelope's `hew_msg_envelope_release` on node
    // free. Any of these calls inside the handler would double-drop under a
    // live borrow receive.
    let body = define_body(&ll, "Inbox__recv__store").join("\n");
    for forbidden in [
        "hew_string_drop",
        "hew_string_free",
        "hew_vec_free",
        "hew_vec_drop",
        "hew_msg_envelope_release",
        "drop_in_place",
    ] {
        assert!(
            !body.contains(forbidden),
            "GATE 1 violated: receive handler emits `{forbidden}` on the \
             borrowed payload — the handler must be non-owning (no drop); \
             the sole drop is hew_msg_envelope_release at node free. \
             Body:\n{body}"
        );
    }

    // De-vacuity (P5-RX Stage 2a, A625): a borrowed view that NEVER escapes an
    // owned sink must also emit NO retain. Without this the test would pass
    // identically before and after the retain-on-escape mechanism (the binding
    // was never in `owned_locals`, so it never dropped regardless) — pinning
    // the *absence* of a spurious `hew_string_clone` makes the test sensitive
    // to the new sink machinery: a retain mistakenly fired on a non-escaping
    // view would trip here.
    assert!(
        !body.contains("hew_string_clone") && !body.contains("borrow_clone"),
        "a non-escaping borrowed view must not be retained — no `hew_string_clone` \
         should appear in the handler. Body:\n{body}"
    );
    assert!(
        !body.contains("borrow_drop_copy_only"),
        "a non-escaping borrowed view has no owned drop to suppress — the gated \
         drop machinery must not appear. Body:\n{body}"
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
actor Sink {
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
    let body = define_body(&ll, "Sink__recv__discard").join("\n");

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
record Wrap {
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
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![handler],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
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
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
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

/// FUNC-REVIEW FOLLOWUP (A625) — positive IR assertion for the re-send sink.
///
/// The re-send retain (`Terminator::Send` payload, lowered at the
/// `send_payload` borrow-gate) was implemented but never asserted in IR. A
/// borrowed `String` view re-sent to another actor must take its OWN owner via
/// a `borrow_mode`-gated `hew_string_clone` into a scratch slot before the send
/// — otherwise the new envelope the runtime builds would release a buffer the
/// original envelope still owns. Hand-assembled so the `Terminator::Send` is
/// exercised directly without the actor-method-call lowering surface.
fn relay_resend_recv_pipeline() -> IrPipeline {
    // `LocalPid<Unit>` is the canonical actor-dispatch-local handle — the
    // unit inner type is irrelevant for the ABI shape (it only determines
    // the message-type layout, not the handle alloca).
    let actor_ty =
        ResolvedTy::named_builtin("LocalPid", BuiltinType::LocalPid, vec![ResolvedTy::Unit]);
    // Receive handler `Relay.forward(s: string)`:
    //   local 0: string      // borrowed receive param (taint root)
    //   local 1: LocalPid    // canonical actor-local handle (the re-send target)
    // Block 0: EnterContext; Send { actor: ActorHandle(1), value: Local(0) }
    // Block 1: ExitContext; Return
    let handler = RawMirFunction {
        name: "Relay__recv__forward".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![ResolvedTy::String],
        locals: vec![ResolvedTy::String, actor_ty], // local 0: string (taint root), local 1: LocalPid
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![Instr::EnterContext],
                terminator: Terminator::Send {
                    actor: Place::ActorHandle(1),
                    msg_type: 1,
                    value: Place::Local(0),
                    next: 1,
                    alias_mode: hew_mir::SendAliasMode::Copy,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![Instr::ExitContext],
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
        raw_mir: vec![handler],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
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

#[test]
fn receive_handler_resend_emits_borrow_gated_retain() {
    let ll = emit_ll_text(&relay_resend_recv_pipeline(), "relay_resend");
    let body = define_body(&ll, "Relay__recv__forward").join("\n");

    assert!(
        body.contains("send_payload_borrow_clone") && body.contains("send_payload_borrow_merge"),
        "a re-sent borrowed view must lower its retain through a real \
         borrow_mode branch+merge; Body:\n{body}"
    );
    assert!(
        body.contains("call ptr @hew_string_clone"),
        "the re-send sink must retain the borrowed view via hew_string_clone; \
         Body:\n{body}"
    );
    assert!(
        body.contains("icmp ne i32 %2"),
        "the re-send retain must be gated on the trailing borrow_mode i32 \
         (param %2); Body:\n{body}"
    );
    // Sanity: the actual send is still emitted (the retain feeds the send).
    assert!(
        body.contains("@hew_actor_send_by_id"),
        "the handler must still emit the actor send; Body:\n{body}"
    );
    // A handled retain sink must not also arm the fail-closed entry trap.
    assert!(
        !body.contains("borrow_escape_trap"),
        "a retained re-send sink must NOT also arm the fail-closed entry trap; \
         Body:\n{body}"
    );
}
