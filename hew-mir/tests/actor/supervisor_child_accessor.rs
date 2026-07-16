//! Supervisor child-accessor MIR intercept tests (F-04 fungible reference).
//!
//! Validates that `HirExprKind::FieldAccess` on a supervisor-typed LHS is
//! intercepted BEFORE the `record_field_orders` lookup and routed to the
//! `hew_supervisor_child_get` runtime call path, producing a FUNGIBLE child
//! reference rather than a snapshotted, trap-on-not-live handle.
//!
//! Coverage:
//! - Static child access lowers to `CallRuntimeAbi("hew_supervisor_child_get")`
//!   to seed the handle alloca, then a `RecordFieldLoad` of the handle field.
//! - The destination Place is typed `LocalPid<ChildActor>` (`ActorHandle`).
//! - The accessor does NOT trap on a not-live slot: liveness is re-resolved at
//!   each send/ask (the fungible model), so the accessor emits no
//!   `Terminator::Trap { kind: SupervisorChildUnavailable }`.
//! - Pool child field access constructs a first-class `SupervisorPool<S, T>`
//!   view, including after binding, and safe `.get(i)` lowers through the pool
//!   runtime lookup plus layout-aware Option construction.
//! - No supervisor field access reaches the `record_field_orders` path
//!   (verified by absence of any "unregistered record type" diagnostic).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, FieldOffset, Instr, MirDiagnosticKind, Place, Terminator, TrapKind,
};
use hew_types::{module_registry::ModuleRegistry, BuiltinType, Checker};

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

/// A function that BINDS a fungible child reference and then sends through it.
/// The send must re-resolve (a second `hew_supervisor_child_get`) rather than
/// reuse the bind-site snapshot, and must NOT trap on a not-live slot.
const STATIC_CHILD_SEND_SOURCE: &str = r"
actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: one_for_one,
    child worker: Worker
}

fn poke(app: LocalPid<App>) {
    let w = app.worker;
    w.ping();
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
fn static_child_access_does_not_trap_on_not_live() {
    // F-04: the fungible accessor never traps. A not-live slot is the SEND's
    // concern (re-resolved + fail-closed there), not the accessor's. The
    // accessor only seeds the handle alloca, so no SupervisorChildUnavailable
    // trap block is emitted for a bare `app.worker`.
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
        !has_sup_trap,
        "the fungible accessor must NOT emit a SupervisorChildUnavailable trap; \
         liveness is the send's concern (re-resolve + fail-closed Err/drop)"
    );
}

#[test]
fn static_child_access_emits_handle_field_load() {
    // The accessor seed extracts field 1 (the handle pointer) from the
    // ChildLookupResult struct to populate the ActorHandle alloca. It does NOT
    // extract the tag (no liveness branch in the accessor) — the tag is read
    // only at the send re-resolve.
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .expect("get_worker function lowered");

    let handle_field_loads: Vec<_> = func
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter(|i| {
            matches!(
                i,
                Instr::RecordFieldLoad {
                    field_offset: FieldOffset(1),
                    ..
                }
            )
        })
        .collect();

    assert!(
        !handle_field_loads.is_empty(),
        "the accessor must extract the handle field (offset 1) to seed the alloca"
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

#[test]
fn fungible_send_reresolves_child_at_send_site() {
    // F-04 core: a tell through a bound fungible child reference re-resolves the
    // current child at the SEND site. The bind seeds one `child_get`; the send
    // emits a SECOND `child_get` (the re-resolve), so a restart between the bind
    // and the send reaches the FRESH child rather than the snapshotted pointer.
    let pipeline = lower_module(STATIC_CHILD_SEND_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "poke")
        .expect("poke function lowered");

    let child_get_count = func
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter(|i| {
            matches!(i,
                Instr::CallRuntimeAbi(call)
                if call.symbol() == "hew_supervisor_child_get"
            )
        })
        .count();

    assert_eq!(
        child_get_count, 2,
        "a bound-then-sent fungible child ref must emit two child_get calls \
         (bind seed + send re-resolve); got {child_get_count}"
    );
}

#[test]
fn fungible_send_has_no_program_killing_trap() {
    // F-04: the send through a fungible reference fail-closes a not-live slot as
    // a recoverable drop (a `Goto` to the continuation), NOT a
    // SupervisorChildUnavailable trap. No trap block of that kind exists anywhere
    // in the lowered `poke`.
    let pipeline = lower_module(STATIC_CHILD_SEND_SOURCE);

    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "poke")
        .expect("poke function lowered");

    let has_sup_trap = func.blocks.iter().any(|b| {
        matches!(
            &b.terminator,
            Terminator::Trap {
                kind: TrapKind::SupervisorChildUnavailable
            }
        )
    });
    assert!(
        !has_sup_trap,
        "the fungible send must fail-closed recoverably (drop/Err), never a \
         SupervisorChildUnavailable trap"
    );
}

#[test]
fn pool_child_field_and_get_lower_end_to_end() {
    let source = r"
        actor Worker { receive fn ping() {} }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool workers: Worker(count: 2)
        }

        fn inspect(sup_pid: LocalPid<Pool>) -> i64 {
            let workers = sup_pid.workers;
            let maybe = workers.get(0);
            let _ = maybe;
            workers.len()
        }
        ";

    let pipeline = lower_module(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "pool field/get lowering diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let func = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "inspect")
        .expect("inspect function lowered");
    let has_pool_view = func
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| {
            matches!(
                instr,
                Instr::RecordInit {
                    ty: hew_types::ResolvedTy::Named {
                        builtin: Some(BuiltinType::SupervisorPool),
                        ..
                    },
                    ..
                }
            )
        });
    assert!(
        has_pool_view,
        "pool field must construct SupervisorPool view"
    );
    let has_pool_lookup = func
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| {
            matches!(
                instr,
                Instr::CallRuntimeAbi(call)
                    if call.symbol() == "hew_supervisor_pool_child_get"
            )
        });
    assert!(
        has_pool_lookup,
        "pool.get must emit hew_supervisor_pool_child_get"
    );
    let has_option_materialiser = func.blocks.iter().any(|block| {
        matches!(
            &block.terminator,
            Terminator::Call { callee, .. }
                if callee == "hew_supervisor_pool_get_option"
        )
    });
    assert!(
        has_option_materialiser,
        "pool.get must route lookup through the layout-aware Option materialiser"
    );
}
