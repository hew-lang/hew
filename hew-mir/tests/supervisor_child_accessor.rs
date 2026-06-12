//! S2 slice tests: supervisor child-accessor MIR intercept.
//!
//! Validates that `HirExprKind::FieldAccess` on a supervisor-typed LHS is
//! intercepted BEFORE the `record_field_orders` lookup and routed to the
//! `hew_supervisor_child_get` runtime call path.
//!
//! Coverage:
//! - Static child access lowers to `CallRuntimeAbi("hew_supervisor_child_get")`
//!   followed by `RecordFieldLoad` extractions and a live-tag branch.
//! - The destination Place is typed `LocalPid<ChildActor>` (`ActorHandle`).
//! - A `Terminator::Trap { kind: TrapKind::SupervisorChildUnavailable }` block
//!   is reachable from the field-access site.
//! - Pool child access emits `NotYetImplemented` and does NOT emit a
//!   `CallRuntimeAbi` or `RecordFieldLoad` for that site.
//! - No supervisor field access reaches the `record_field_orders` path
//!   (verified by absence of any "unregistered record type" diagnostic).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, MirDiagnosticKind, Place, Terminator, TrapKind};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Lower a Hew source program to MIR, asserting no parse, HIR, or
/// unintended MIR diagnostics.
fn lower_module(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

/// A minimal supervisor + child actor program with a function that accesses
/// the child via the supervisor PID.
const STATIC_CHILD_ACCESS_SOURCE: &str = r"
actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: one_for_one,
    child worker: Worker
}

fn get_worker(app: LocalPid<App>) -> LocalPid<Worker> {
    app.worker
}
";

#[test]
fn static_child_access_emits_supervisor_child_get_call() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    // Find the `get_worker` function.
    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .expect("get_worker function lowered");

    // Confirm a `CallRuntimeAbi` with symbol `hew_supervisor_child_get` is present.
    let has_child_get = func
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .any(|i| {
            matches!(i,
                Instr::CallRuntimeAbi(call)
                if call.symbol() == "hew_supervisor_child_get"
            )
        });
    assert!(
        has_child_get,
        "static child access must emit hew_supervisor_child_get"
    );
}

#[test]
fn static_child_access_call_has_sup_place_and_slot_index_args() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .expect("get_worker function lowered");

    let call = func
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find_map(|i| match i {
            Instr::CallRuntimeAbi(call) if call.symbol() == "hew_supervisor_child_get" => {
                Some(call)
            }
            _ => None,
        })
        .expect("hew_supervisor_child_get call found");

    // args[0] = sup_place, args[1] = slot_index_const (i64 0 for first child).
    assert_eq!(
        call.args().len(),
        2,
        "call takes exactly sup_place + slot_index"
    );
    // args[0] should be a Place::Local (the lowered supervisor PID parameter).
    assert!(
        matches!(call.args()[0], Place::Local(_)),
        "first arg must be the supervisor PID place; got {:?}",
        call.args()[0]
    );
    // args[1] should also be a Place::Local (the ConstI64 slot index).
    assert!(
        matches!(call.args()[1], Place::Local(_)),
        "second arg must be the slot-index constant place; got {:?}",
        call.args()[1]
    );
    // dest must be present (the struct return value place).
    assert!(
        call.dest().is_some(),
        "hew_supervisor_child_get must have a dest (struct return place)"
    );
}

#[test]
fn static_child_access_dest_is_actor_handle_typed_local_pid() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .expect("get_worker function lowered");

    // The final handle place returned to the caller must be an ActorHandle(N)
    // whose backing local is typed `LocalPid<Worker>`.
    let handle_place = func
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find_map(|i| match i {
            Instr::Move {
                dest: Place::ActorHandle(n),
                ..
            } => Some(*n),
            _ => None,
        });
    assert!(
        handle_place.is_some(),
        "a Move into ActorHandle(N) must exist on the success path"
    );
    let handle_id = handle_place.unwrap();

    let ty = func
        .locals
        .get(handle_id as usize)
        .expect("handle local index in bounds");
    assert!(
        matches!(ty,
            hew_types::ResolvedTy::Named { name, args, .. }
            if name == "LocalPid" && args.len() == 1
               && matches!(&args[0],
                   hew_types::ResolvedTy::Named { name: inner, .. }
                   if inner == "Worker")
        ),
        "handle local must be typed LocalPid<Worker>; got {ty:?}"
    );
}

#[test]
fn static_child_access_has_trap_block_for_unavailable() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .expect("get_worker function lowered");

    let has_sup_trap = func.blocks.iter().any(|b| {
        matches!(
            &b.terminator,
            Terminator::Trap {
                kind: TrapKind::SupervisorChildUnavailable
            }
        )
    });
    assert!(
        has_sup_trap,
        "a SupervisorChildUnavailable trap block must be present for the tag != 0 path"
    );
}

#[test]
fn static_child_access_emits_record_field_loads_for_tag_and_handle() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .expect("get_worker function lowered");

    let field_loads: Vec<_> = func
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter(|i| matches!(i, Instr::RecordFieldLoad { .. }))
        .collect();

    // Expect at least two: one for tag (offset 0) and one for handle (offset 1).
    assert!(
        field_loads.len() >= 2,
        "expected at least 2 RecordFieldLoad instructions (tag + handle); got {}",
        field_loads.len()
    );
}

#[test]
fn static_child_access_does_not_emit_not_yet_implemented() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    // No diagnostic about "unregistered record type" or pool/nested should appear.
    let bad_diag = pipeline.diagnostics.iter().find(|d| {
        matches!(&d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
            if construct.contains("unregistered record type")
                || construct.contains("supervisor")
        )
    });
    assert!(
        bad_diag.is_none(),
        "static child access must not fall through to the record-field path; \
         got unexpected diagnostic: {bad_diag:?}"
    );
}

/// Pool child access is now rejected at HIR pre-pass (Lane FC-P1-C). The MIR
/// `NotYetImplemented` arm at `hew-mir/src/lower.rs:4413` remains as
/// defense-in-depth but is no longer reachable from the compile driver because
/// HIR `into_result()` returns `Err` before MIR lowering begins.
///
/// Asserts: the source program emits `SupervisorPoolChildAccessorUnsupported`
/// at HIR and HIR `into_result()` fails. Reaching MIR is itself a regression.
#[test]
fn pool_child_access_rejected_at_hir_before_mir() {
    let source = r"
        actor Worker { receive fn ping() {} }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool worker: Worker
        }

        fn get_pool_worker(sup_pid: LocalPid<Pool>) -> LocalPid<Worker> {
            sup_pid.worker
        }
        ";

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    // HIR must reject the program with the pool-accessor gate diagnostic.
    let pool_gate_fired = hir.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            hew_hir::HirDiagnosticKind::SupervisorPoolChildAccessorUnsupported { .. }
        )
    });
    assert!(
        pool_gate_fired,
        "Pool child accessor must be rejected by HIR pre-pass gate; \
         diagnostics: {:#?}",
        hir.diagnostics
    );
    assert!(
        hir.into_result().is_err(),
        "HIR into_result() must be Err for pool child accessor (Lane FC-P1-C)"
    );
}
