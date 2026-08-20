//! W60.114 follow-up — two gaps in the actor-handler `bytes` ownership mint
//! `fix(mir): release actor-delivered bytes payloads` (20d4f398) opened:
//!
//! 1. **Invalid-free blocker.** `mod.rs`'s `lower_params` registers a
//!    non-consumed `ActorHandler` `bytes` PARAMETER as handler-owned (so
//!    drop elaboration releases a terminal delivery), but
//!    `actor_handler_mints_an_owner_for_message` — the CALLER-side predicate
//!    that decides whether `lower_actor_send` must ask the provenance
//!    question before handing a value to the mailbox — never listed
//!    `ResolvedTy::Bytes`. A caller could therefore send a bare
//!    proven-foreign `bytes` pointer (a value this program never allocated)
//!    straight into a receive handler, which would mint an owner from the
//!    parameter's TYPE alone and free a buffer the host still owns.
//!    Record-wrapped foreign `bytes` was already refused (`is_owned_aggregate_
//!    record_ty` catches the record), so this pinned specifically the BARE
//!    `bytes` hole.
//!
//! 2. **Cancellation ownership.** A receive handler that cooperates (a
//!    `CooperateKind::FunctionEntry` cancellation checkpoint, inserted
//!    automatically the moment the handler's first block does anything more
//!    than immediately yield) and THEN forwards its owned `bytes` parameter
//!    to another actor leaked the parameter on cancellation-before-transfer:
//!    `derive_local_bytes_drop_allowed`'s escape scan excludes a `Send`/`Ask`-
//!    read binding from `allowed` WHOLE-FUNCTION (its own doc: "excluded
//!    twice over"), which is sound for every exit downstream of the transfer
//!    but wrong for the entry-cancel exit that precedes it. Closed by
//!    `derive_bytes_actor_transfer_blocks` — a reachability-based re-admission
//!    that releases on pre-transfer cancellation/terminal exits and on the
//!    single CFG frontier where a non-transfer branch joins the transfer's
//!    forward-closed downstream region, gated on the SAME dataflow proving
//!    the binding still definitely `Live` there.

use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline_with_tc(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

fn drops_for(pipeline: &hew_mir::IrPipeline, fn_name: &str, exit_label: &str) -> Vec<String> {
    let f = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in elaborated_mir"));
    f.drop_plans
        .iter()
        .find(|(exit, _)| format!("{exit:?}").starts_with(exit_label))
        .unwrap_or_else(|| {
            panic!(
                "no {exit_label} exit in {fn_name}: {:#?}",
                f.drop_plans.iter().map(|(e, _)| e).collect::<Vec<_>>()
            )
        })
        .1
        .drops
        .iter()
        .map(|d| format!("{:?} {:?} {:?}", d.place, d.ty, d.kind))
        .collect()
}

fn has_bytes_drop(pipeline: &hew_mir::IrPipeline, fn_name: &str, exit_label: &str) -> bool {
    drops_for(pipeline, fn_name, exit_label)
        .iter()
        .any(|d| d.contains("Bytes"))
}

fn bytes_drops_for<'a>(
    pipeline: &'a hew_mir::IrPipeline,
    fn_name: &str,
    exit_label: &str,
) -> Vec<&'a hew_mir::ElabDrop> {
    let function = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in elaborated_mir"));
    function
        .drop_plans
        .iter()
        .find(|(exit, _)| format!("{exit:?}").starts_with(exit_label))
        .unwrap_or_else(|| panic!("no {exit_label} exit in {fn_name}"))
        .1
        .drops
        .iter()
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .collect()
}

// ---------------------------------------------------------------------------
// Gap 1 — the invalid-free blocker (caller-side mint predicate)
// ---------------------------------------------------------------------------

const FOREIGN_BYTES_PRELUDE: &str = r#"extern "C" {
    fn host_bytes() -> bytes;
}
actor TestSink {
    receive fn take(data: bytes) { let _ = data.len(); }
}
"#;

/// THE HOLE. A bare proven-foreign `bytes` value (a root `extern "C" -> bytes`
/// result — NOT adopted the way `extern "C" -> string` is; adoption is defined
/// at `return_ty == String` and nowhere else, see `let_binder_owns_proven_
/// foreign_value`) handed straight to an actor handler's mailbox. The handler
/// mints its scope-exit `hew_bytes_drop` from the parameter's TYPE alone
/// (`lower_params`'s `ActorHandler`+`Bytes` arm), so without the caller-side
/// refusal this would free a buffer the host still owns.
#[test]
fn bare_proven_foreign_bytes_into_an_actor_handler_mailbox_is_refused() {
    let source = format!(
        "{FOREIGN_BYTES_PRELUDE}\
         fn main() -> i64 {{\n    \
         let sink = spawn TestSink();\n    \
         let b = unsafe {{ host_bytes() }};\n    \
         sink.take(b);\n    \
         0\n}}\n"
    );
    let pipeline = pipeline_with_tc(&source);
    assert!(
        pipeline.diagnostics.iter().any(|d| format!("{:?}", d.kind)
            .contains("ownership transfer of a proven-foreign value")
            && format!("{:?}", d.kind).contains("actor handler's mailbox")),
        "expected the caller-side refusal for a bare foreign `bytes` mailbox \
         transfer, got: {:#?}",
        pipeline.diagnostics
    );
}

/// Tell and ask differ only in reply plumbing; both deliver the argument to
/// the same `ActorHandler` parameter prologue and therefore enforce the same
/// caller-side provenance boundary.
#[test]
fn bare_proven_foreign_bytes_into_an_actor_ask_mailbox_is_refused() {
    let source = r#"
extern "C" {
    fn host_bytes() -> bytes;
}
actor TestSink {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
}
fn main() -> i64 {
    let sink = spawn TestSink();
    let b = unsafe { host_bytes() };
    let _ = await sink.take(b);
    0
}
"#;
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.iter().any(|d| format!("{:?}", d.kind)
            .contains("ownership transfer of a proven-foreign value")
            && format!("{:?}", d.kind).contains("actor handler's mailbox")),
        "expected the caller-side refusal for a bare foreign `bytes` ask \
         transfer, got: {:#?}",
        pipeline.diagnostics
    );
}

/// Select and join lower actor calls through their own task lowering surfaces,
/// so each must run the same caller-side foreign-provenance preflight as a
/// direct ask.
#[test]
fn bare_proven_foreign_bytes_into_select_and_join_mailboxes_are_refused() {
    for source in [
        r#"
extern "C" { fn host_bytes() -> bytes; }
actor TestSink { receive fn take(data: bytes) -> i64 { data.len() as i64 } }
fn main() -> i64 {
    let sink = spawn TestSink();
    let b = unsafe { host_bytes() };
    select {
        reply from sink.take(b) => reply,
        after 1ms => -1,
    }
}
"#,
        r#"
extern "C" { fn host_bytes() -> bytes; }
actor TestSink {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
    receive fn ping() -> i64 { 1 }
}
fn main() -> i64 {
    let sink = spawn TestSink();
    let b = unsafe { host_bytes() };
    let (a, _) = join { sink.take(b), sink.ping() };
    a
}
"#,
    ] {
        let pipeline = pipeline_with_tc(source);
        assert!(
            pipeline.diagnostics.iter().any(|d| format!("{:?}", d.kind)
                .contains("ownership transfer of a proven-foreign value")
                && format!("{:?}", d.kind).contains("actor handler's mailbox")),
            "expected the caller-side foreign-bytes refusal, got: {:#?}",
            pipeline.diagnostics
        );
    }
}

/// The parity control this hole broke: a record WRAPPING the identical
/// foreign `bytes` field was already refused before this fix
/// (`is_owned_aggregate_record_ty` catches the record's heap field). Pins
/// that the fix does not accidentally loosen the already-correct case.
#[test]
fn record_wrapped_proven_foreign_bytes_into_an_actor_handler_mailbox_is_refused() {
    let source = format!(
        "{FOREIGN_BYTES_PRELUDE}\
         record Holder {{ payload: bytes }}\n\
         actor RecordSink {{\n    \
         receive fn take(data: Holder) {{ let _ = data.payload.len(); }}\n\
         }}\n\
         fn main() -> i64 {{\n    \
         let sink = spawn RecordSink();\n    \
         let h = Holder {{ payload: unsafe {{ host_bytes() }} }};\n    \
         sink.take(h);\n    \
         0\n}}\n"
    );
    let pipeline = pipeline_with_tc(&source);
    assert!(
        pipeline
            .diagnostics
            .iter()
            .any(|d| format!("{:?}", d.kind)
                .contains("ownership transfer of a proven-foreign value")),
        "expected the pre-existing record-wrapped refusal, got: {:#?}",
        pipeline.diagnostics
    );
}

/// Counterfactual A — genuine mailbox/reactor-owned `bytes` (a domestic
/// literal this frame allocated) sent to the SAME handler must still compile
/// clean. The refusal is provenance-directed, not a ban on sending `bytes`.
#[test]
fn domestic_literal_bytes_into_an_actor_handler_mailbox_still_compiles() {
    let source = r#"
actor TestSink {
    receive fn take(data: bytes) { let _ = data.len(); }
}
fn main() -> i64 {
    let sink = spawn TestSink();
    let b = b"hello";
    sink.take(b);
    0
}
"#;
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "a domestic `bytes` literal must still reach the mailbox clean: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        has_bytes_drop(&pipeline, "TestSink__recv__take", "Return"),
        "and the terminal handler must still mint its scope-exit release"
    );
}

/// Counterfactual B — a handler-owned `bytes` parameter FORWARDED into a
/// second actor's mailbox is domestic from the second handler's point of
/// view (the mailbox's own delivered copy), so it must still compile clean
/// and still transfer ownership onward (see `recv_handler_forwarded_bytes_
/// param_transfers_drop_to_recipient` in `vertical.rs` for the drop-plan
/// pin); this test only pins that the caller-side refusal added for gap 1
/// does not reject the forwarding shape itself.
#[test]
fn forwarded_handler_owned_bytes_into_a_second_actor_mailbox_still_compiles() {
    let source = r"
actor Recipient {
    receive fn take(data: bytes) { let _ = data.len(); }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) { recipient.take(data); }
}
fn main() -> i64 { 0 }
";
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "forwarding a handler-owned `bytes` parameter must still compile clean: {:#?}",
        pipeline.diagnostics
    );
}

// ---------------------------------------------------------------------------
// Gap 2 — path-sensitive cancellation ownership
// ---------------------------------------------------------------------------

/// A forwarding handler shaped exactly like Opus's counterexample: some
/// pre-transfer work (forcing a `CooperateKind::FunctionEntry` cancellation
/// checkpoint at block 0 — a handler whose FIRST block terminator is already
/// the `send` gets no checkpoint at all, since the actor cooperates via the
/// yield-equivalent send anyway) followed by a forwarding `send`.
const COOPERATE_THEN_FORWARD_SOURCE: &str = r#"
actor Recipient {
    receive fn take(data: bytes) { let _ = data.len(); }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) {
        println("forwarding");
        recipient.take(data);
    }
}
fn main() -> i64 { 0 }
"#;

/// THE CANCELLATION HOLE. Cancellation at the function-entry checkpoint fires
/// in the prologue, strictly before the rest of the handler body — including
/// the forwarding `send` — ever runs. The handler is therefore still the
/// untransferred sole owner of `data` on that exit and must release it.
#[test]
fn cooperate_then_forward_drops_the_parameter_on_cancel_before_transfer() {
    let pipeline = pipeline_with_tc(COOPERATE_THEN_FORWARD_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        has_bytes_drop(&pipeline, "Forwarder__recv__forward", "Cancel"),
        "cancellation reached before the forwarding send must release the \
         still-untransferred handler-owned `bytes` parameter"
    );
}

/// Successful transfer: the `Send` exit itself, and the `Return` exit that
/// follows it, must NOT carry a second release — the receiving actor's
/// mailbox now owns the sole reference. A double-drop here would free the
/// buffer the recipient handler is still reading.
#[test]
fn cooperate_then_forward_does_not_source_drop_on_a_successful_send() {
    let pipeline = pipeline_with_tc(COOPERATE_THEN_FORWARD_SOURCE);
    assert!(
        !has_bytes_drop(&pipeline, "Forwarder__recv__forward", "Send"),
        "the send transfers the sole reference; the source frame must not \
         also drop it: {:?}",
        drops_for(&pipeline, "Forwarder__recv__forward", "Send")
    );
    assert!(
        bytes_drops_for(&pipeline, "Forwarder__recv__forward", "Return")
            .iter()
            .all(|drop| drop.guard.is_some()),
        "the return exit, reached only after a successful send on this \
         straight-line handler, may retain only the transfer-suppressed \
         guarded drop: {:?}",
        drops_for(&pipeline, "Forwarder__recv__forward", "Return")
    );
}

/// A suspendable actor ask is represented by a bare `Suspend` terminator plus
/// a `SuspendKind::Ask` side-table payload. It is the same mailbox transfer as
/// `Send`: cancel before the suspend still owns the bytes, while the suspend
/// and its resumed return path do not.
#[test]
fn cooperate_then_suspending_ask_has_cancel_only_source_drop() {
    let source = r#"
actor Recipient {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) {
        println("forwarding");
        let _ = await recipient.take(data);
    }
}
fn main() -> i64 { 0 }
"#;
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        has_bytes_drop(&pipeline, "Forwarder__recv__forward", "Cancel"),
        "cancellation before the suspending ask must release the untransferred bytes"
    );
    for exit in ["Suspend", "Return"] {
        assert!(
            bytes_drops_for(&pipeline, "Forwarder__recv__forward", exit)
                .iter()
                .all(|drop| drop.guard.is_some()),
            "{exit} is downstream of the transfer and may retain only a \
             transfer-suppressed guarded drop"
        );
    }
}

/// Actor-ask payloads nested in suspending select arms and join branches are
/// mailbox transfers too. Their carriers must suppress downstream source
/// drops while preserving the pre-carrier cancellation release.
#[test]
fn cooperate_then_select_or_join_has_cancel_only_source_drop() {
    let select_source = r#"
actor Recipient {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) {
        println("selecting");
        let _ = select {
            reply from recipient.take(data) => reply,
            after 1ms => -1,
        };
    }
}
fn main() -> i64 { 0 }
"#;
    let join_source = r#"
actor Recipient {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
    receive fn ping() -> i64 { 1 }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) {
        println("joining");
        let _ = join { recipient.take(data), recipient.ping() };
    }
}
fn main() -> i64 { 0 }
"#;
    for source in [select_source, join_source] {
        let pipeline = pipeline_with_tc(source);
        assert!(
            pipeline.diagnostics.is_empty(),
            "no diagnostics expected: {:#?}",
            pipeline.diagnostics
        );
        assert!(
            has_bytes_drop(&pipeline, "Forwarder__recv__forward", "Cancel"),
            "cancel before the select/join mailbox carrier must release the bytes"
        );
        let f = pipeline
            .elaborated_mir
            .iter()
            .find(|f| f.name == "Forwarder__recv__forward")
            .expect("Forwarder__recv__forward must be present");
        for (exit, plan) in &f.drop_plans {
            if matches!(exit, hew_mir::ExitPath::Cancel { .. }) {
                continue;
            }
            assert!(
                plan.drops
                    .iter()
                    .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
                    .all(|drop| drop.guard.is_some()),
                "after the select/join carrier submits the payload, only a \
                 transfer-suppressed guarded source drop may remain at \
                 {exit:?}: {plan:#?}"
            );
        }
    }
}

/// Regression for Opus's branch counterexample. `BindingState::Live` proves
/// initialisation, not last use: a non-forwarding branch can contain several
/// Call checkpoints before its final scope-close Goto. None of those
/// checkpoints may release `data`; the shared Return carries one guarded drop
/// that fires only on the non-transfer path.
#[test]
fn conditional_forward_does_not_drop_before_later_non_transfer_branch_uses() {
    let source = r#"
actor Recipient {
    receive fn take(data: bytes) { println("DATA"); }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool) {
        println("go");
        if flag {
            recipient.take(data);
        } else {
            println("skip");
            let n = data.len();
            println(n as i64);
            let first = data[0];
            println(first as i64);
        }
    }
}
fn main() -> i64 { 0 }
"#;
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:#?}",
        pipeline.diagnostics
    );
    let f = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Forwarder__recv__forward")
        .expect("Forwarder__recv__forward must be present");
    let call_drop_count = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Call { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .count();
    let goto_drop_count = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .count();
    let return_bytes: Vec<_> = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Return { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .collect();
    assert_eq!(
        call_drop_count, 0,
        "a sequential call checkpoint is before later uses, not a release boundary: {:#?}",
        f.drop_plans
    );
    assert_eq!(
        goto_drop_count, 0,
        "the non-transfer Goto must not release before the shared guarded exit: {:#?}",
        f.drop_plans
    );
    assert_eq!(
        return_bytes.len(),
        1,
        "the shared exit must contain exactly one Bytes drop: {:#?}",
        f.drop_plans
    );
    assert!(
        return_bytes[0].guard.is_some(),
        "the shared Bytes drop must be guarded by the path-local transfer flag"
    );
    let flag = return_bytes[0].guard.expect("guard asserted above");
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Forwarder__recv__forward")
        .expect("Forwarder__recv__forward raw MIR must be present");
    let flag_writes: Vec<i64> = raw
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            hew_mir::Instr::ConstI64 { dest, value } if *dest == flag => Some(*value),
            _ => None,
        })
        .collect();
    assert_eq!(
        flag_writes,
        [0, 1],
        "the actor-message Bytes guard must be initialised once and set exactly \
         once on the consuming branch: {:#?}",
        raw.blocks
    );
}

/// Nested scope joins and loop back-edges can place arbitrarily many Gotos
/// before the final merge. None may carry an eager release; the one guarded
/// shared-exit drop remains the sole authority.
#[test]
fn nested_and_looping_non_transfer_paths_release_once_at_the_final_join() {
    let nested = r#"
actor Recipient { receive fn take(data: bytes) { println("DATA"); } }
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool, g: bool) {
        println("go");
        if flag {
            recipient.take(data);
        } else {
            println("skip");
            let n = data.len();
            println(n as i64);
            if g { println("a"); } else { println("b"); }
        }
    }
}
fn main() -> i64 { 0 }
"#;
    let looping = r#"
actor Recipient { receive fn take(data: bytes) { println("DATA"); } }
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool, n: i64) {
        println("go");
        if flag {
            recipient.take(data);
        } else {
            var i: i64 = 0;
            while i < n {
                println("tick");
                i = i + 1;
            }
        }
    }
}
fn main() -> i64 { 0 }
"#;
    for source in [nested, looping] {
        let pipeline = pipeline_with_tc(source);
        assert!(
            pipeline.diagnostics.is_empty(),
            "no diagnostics expected: {:#?}",
            pipeline.diagnostics
        );
        let f = pipeline
            .elaborated_mir
            .iter()
            .find(|f| f.name == "Forwarder__recv__forward")
            .expect("Forwarder__recv__forward must be present");
        let goto_drop_count = f
            .drop_plans
            .iter()
            .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Goto { .. }))
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
            .count();
        let return_bytes: Vec<_> = f
            .drop_plans
            .iter()
            .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Return { .. }))
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
            .collect();
        assert_eq!(
            goto_drop_count, 0,
            "nested joins and loop edges must not release before the shared \
             guarded exit: {:#?}",
            f.drop_plans
        );
        assert_eq!(
            return_bytes.len(),
            1,
            "the shared exit must carry exactly one Bytes drop: {:#?}",
            f.drop_plans
        );
        assert!(
            return_bytes[0].guard.is_some(),
            "the shared Bytes drop must retain its transfer guard"
        );
    }
}

/// A mailbox transfer on one branch does not make it the binding's sole
/// ownership sink. If a disjoint branch moves the same bytes into a local
/// aggregate, that aggregate's in-place drop is the one release on that path;
/// re-admitting the source bytes drop there would free the same pointer twice.
#[test]
fn aggregate_owner_on_the_non_transfer_branch_suppresses_source_readmission() {
    let source = r#"
record Holder { payload: bytes }
actor Recipient { receive fn take(data: bytes) { println("DATA"); } }
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool) {
        println("go");
        if flag {
            recipient.take(data);
        } else {
            let h = Holder { payload: data };
            println("held");
        }
    }
}
fn main() -> i64 { 0 }
"#;
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:#?}",
        pipeline.diagnostics
    );
    let f = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Forwarder__recv__forward")
        .expect("Forwarder__recv__forward must be present");
    let goto_bytes = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .count();
    let aggregate_drops = f
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
        .count();
    let cancel_bytes = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Cancel { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes) && drop.guard.is_none())
        .count();
    assert_eq!(
        goto_bytes, 0,
        "the Holder owns and releases the bytes on the non-transfer branch: {:#?}",
        f.drop_plans
    );
    assert!(
        aggregate_drops >= 1,
        "the non-transfer branch must retain its Holder in-place release"
    );
    assert_eq!(
        cancel_bytes, 1,
        "only pre-construction entry cancellation owns an unguarded direct \
         bytes release; later source drops must be transfer-suppressed: {:#?}",
        f.drop_plans
    );
}

/// Enum payload construction writes through `Place::EnumVariant` rather than
/// `RecordInit`, but it is the same owning-aggregate sink: the enum's in-place
/// drop recursively releases its bytes payload and must remain the sole free.
#[test]
fn enum_owner_on_the_non_transfer_branch_suppresses_source_readmission() {
    let source = r#"
actor Recipient { receive fn take(data: bytes) { println("DATA"); } }
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool) {
        println("go");
        if flag {
            recipient.take(data);
        } else {
            let h = Some(data);
            println("held");
        }
    }
}
fn main() -> i64 { 0 }
"#;
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:#?}",
        pipeline.diagnostics
    );
    let f = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Forwarder__recv__forward")
        .expect("Forwarder__recv__forward must be present");
    let goto_bytes = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .count();
    let enum_drops = f
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.kind, hew_mir::DropKind::EnumInPlace))
        .count();
    let cancel_bytes = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Cancel { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes) && drop.guard.is_none())
        .count();
    assert_eq!(
        goto_bytes, 0,
        "the Option owns and releases the bytes on the non-transfer branch: {:#?}",
        f.drop_plans
    );
    assert!(enum_drops >= 1, "the Option in-place release must remain");
    assert_eq!(
        cancel_bytes, 1,
        "only pre-construction entry cancellation owns an unguarded direct \
         bytes release: {:#?}",
        f.drop_plans
    );
}

/// Undelivered send: forwarding through a FUNGIBLE supervisor-child reference
/// (`app.worker`) re-resolves the child at the send site and skips delivery
/// on a not-live slot (F-04's recoverable fail-closed tell). The skipped-
/// delivery `Goto` edge must release the payload exactly once — the same
/// buffer neither a double-free nor a leak.
#[test]
fn cooperate_then_forward_through_a_not_live_fungible_child_releases_once() {
    let source = r"
actor Worker {
    receive fn take(data: bytes) { let _ = data.len(); }
}
supervisor App {
    strategy: one_for_one,
    child worker: Worker
}
actor Forwarder {
    let app: LocalPid<App>;
    receive fn forward(data: bytes) {
        app.worker.take(data);
    }
}
fn main() -> i64 { 0 }
";
    let pipeline = pipeline_with_tc(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:#?}",
        pipeline.diagnostics
    );
    let f = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Forwarder__recv__forward")
        .expect("Forwarder__recv__forward must be present");
    // The not-live recover edge is a `Goto` (F-04 joins straight back into
    // normal control flow); the live-delivery edge is the `Send` itself.
    // The successful delivery sets the guard; the not-live path does not. Both
    // converge on the shared Return, whose one guarded drop therefore releases
    // only the undelivered path.
    let send_drop_count = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Send { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .count();
    let goto_drop_count = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .count();
    let return_bytes: Vec<_> = f
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Return { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.ty, hew_types::ResolvedTy::Bytes))
        .collect();
    assert_eq!(
        send_drop_count, 0,
        "a live delivery transfers ownership; the send exit must not drop: {:#?}",
        f.drop_plans
    );
    assert_eq!(
        goto_drop_count, 0,
        "the recover edge must defer release to the shared guarded exit: {:#?}",
        f.drop_plans
    );
    assert_eq!(
        return_bytes.len(),
        1,
        "the shared exit must carry exactly one Bytes drop: {:#?}",
        f.drop_plans
    );
    assert!(
        return_bytes[0].guard.is_some(),
        "the shared Bytes drop must discriminate delivered from undelivered paths"
    );
}
