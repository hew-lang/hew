//! Supervisor `ChildRef` MIR lowering tests.
//!
//! Static child and pool accessors construct the same pointer-free
//! `ChildRef<T>` value: supervisor token plus static child slot. Every ask or
//! tell extracts that stable role from the value at its use site. No raw child
//! pointer lookup or per-local provenance table participates.
//!
//! Coverage:
//! - Static child access constructs a typed two-field `ChildRef` record.
//! - `ChildRef` sends, asks, and select arms carry stable-role Places.
//! - Access and submission never emit a `SupervisorChildUnavailable` trap.
//! - Pool index/get uses the stable pool-slot descriptor lookup and produces
//!   `ChildRef` values through the same representation.
//! - Supervisor field access never falls through to user-record lowering.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, FieldOffset, Instr, MirDiagnosticKind, Place, Terminator, TrapKind,
};
use hew_types::{
    module_registry::ModuleRegistry, runtime_call::RuntimeCallFamily, BuiltinType, Checker,
};

/// Runtime calls are unwind-capable CFG boundaries, so production lowering
/// carries them as typed `Terminator::Call` values rather than mid-block
/// instructions. Match the closed runtime family as the semantic authority and
/// the symbol as its linker-facing invariant.
fn runtime_terminator_calls(
    func: &hew_mir::RawMirFunction,
    family: RuntimeCallFamily,
) -> impl Iterator<Item = (&[Place], Option<Place>)> + '_ {
    func.blocks.iter().filter_map(move |block| {
        let Terminator::Call {
            callee,
            authority,
            args,
            dest,
            ..
        } = &block.terminator
        else {
            return None;
        };
        (authority.runtime_family() == Some(family) && callee == family.c_symbol())
            .then_some((args.as_slice(), *dest))
    })
}

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

fn get_worker(app: LocalPid<App>) -> ChildRef<Worker> {
    app.worker
}
";

/// Bind a `ChildRef` and send through it after ordinary value flow.
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
fn static_child_access_constructs_child_ref_without_runtime_child_lookup() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);
    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .unwrap();

    assert_eq!(child_ref_slot_constants(func), vec![0]);
    assert_eq!(
        runtime_terminator_calls(func, RuntimeCallFamily::SupervisorChildGet).count(),
        0,
        "ChildRef construction must never snapshot a child pointer",
    );
}

#[test]
fn static_child_access_carries_supervisor_token_and_slot_fields() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);
    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .unwrap();
    let fields = func
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .find_map(|instr| match instr {
            Instr::RecordInit {
                ty:
                    hew_types::ResolvedTy::Named {
                        builtin: Some(BuiltinType::ChildRef),
                        ..
                    },
                fields,
                ..
            } => Some(fields),
            _ => None,
        })
        .expect("ChildRef record init");

    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0, FieldOffset(0));
    assert_eq!(fields[1].0, FieldOffset(1));
    assert!(matches!(fields[0].1, Place::Local(_)));
    assert!(matches!(fields[1].1, Place::Local(_)));
}

#[test]
fn static_child_access_dest_is_child_ref_typed_value() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);
    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .unwrap();
    let dest = func
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .find_map(|instr| match instr {
            Instr::RecordInit {
                ty:
                    hew_types::ResolvedTy::Named {
                        builtin: Some(BuiltinType::ChildRef),
                        ..
                    },
                dest: Place::Local(local),
                ..
            } => Some(*local),
            _ => None,
        })
        .expect("ChildRef destination local");

    assert!(matches!(
        &func.locals[dest as usize],
        hew_types::ResolvedTy::Named {
            builtin: Some(BuiltinType::ChildRef),
            args,
            ..
        } if matches!(args.as_slice(), [hew_types::ResolvedTy::Named { name, .. }] if name == "Worker")
    ));
}

#[test]
fn static_child_access_does_not_trap_on_not_live() {
    // F-04: the ChildRef accessor never traps. A not-live slot is the SEND's
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
        "the ChildRef accessor must NOT emit a SupervisorChildUnavailable trap; \
         liveness is the send's concern (re-resolve + fail-closed Err/drop)"
    );
}

#[test]
fn static_child_access_never_loads_a_child_pointer() {
    let pipeline = lower_module(STATIC_CHILD_ACCESS_SOURCE);
    let func = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "get_worker")
        .unwrap();
    assert!(
        func.blocks
            .iter()
            .flat_map(|b| &b.instructions)
            .all(|instr| {
                !matches!(
                    instr,
                    Instr::RecordFieldLoad {
                        field_offset: FieldOffset(1),
                        ..
                    }
                )
            }),
        "bare ChildRef construction must not load a pointer from ChildLookupResult"
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
fn child_ref_send_carries_stable_role_without_runtime_lookup() {
    let pipeline = lower_module(STATIC_CHILD_SEND_SOURCE);
    let func = pipeline.raw_mir.iter().find(|f| f.name == "poke").unwrap();

    assert_eq!(stable_role_slot_constants(func), vec![0]);
    assert_eq!(
        runtime_terminator_calls(func, RuntimeCallFamily::SupervisorChildGet).count(),
        0,
        "ChildRef send must use its value-carried role, not child_get",
    );
}

#[test]
fn child_ref_send_has_no_program_killing_trap() {
    // F-04: the send through a ChildRef fail-closes a not-live slot as
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
        "the ChildRef send must fail-closed recoverably (drop/Err), never a \
         SupervisorChildUnavailable trap"
    );
}

fn const_i64_values(func: &hew_mir::RawMirFunction) -> std::collections::HashMap<Place, i64> {
    func.blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::ConstI64 { dest, value } => Some((*dest, *value)),
            _ => None,
        })
        .collect()
}

fn child_ref_slot_constants(func: &hew_mir::RawMirFunction) -> Vec<i64> {
    let constants = const_i64_values(func);
    func.blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::RecordInit {
                ty:
                    hew_types::ResolvedTy::Named {
                        builtin: Some(BuiltinType::ChildRef),
                        ..
                    },
                fields,
                ..
            } => fields
                .iter()
                .find(|(offset, _)| *offset == FieldOffset(1))
                .and_then(|(_, source)| constants.get(source).copied()),
            _ => None,
        })
        .collect()
}

fn stable_role_slot_constants(func: &hew_mir::RawMirFunction) -> Vec<i64> {
    let constants = const_i64_values(func);
    let mut record_fields = std::collections::HashMap::new();
    let mut field_loads = std::collections::HashMap::new();
    let mut moves = std::collections::HashMap::new();
    for instr in func.blocks.iter().flat_map(|block| &block.instructions) {
        match instr {
            Instr::RecordInit { fields, dest, .. } => {
                for (offset, source) in fields {
                    record_fields.insert((*dest, *offset), *source);
                }
            }
            Instr::RecordFieldLoad {
                record,
                field_offset,
                dest,
            } => {
                field_loads.insert(*dest, (*record, *field_offset));
            }
            Instr::Move { dest, src } => {
                moves.insert(*dest, *src);
            }
            _ => {}
        }
    }
    let chase = |mut place: Place| {
        while let Some(source) = moves.get(&place) {
            place = *source;
        }
        place
    };
    let resolve = |place: Place| {
        let place = chase(place);
        constants
            .get(&place)
            .copied()
            .or_else(|| {
                let (record, offset) = field_loads.get(&place)?;
                let record = chase(*record);
                let source = chase(*record_fields.get(&(record, *offset))?);
                constants.get(&source).copied()
            })
            .expect("stable role slot must trace to the ChildRef slot constant")
    };
    let mut slots = Vec::new();
    let mut push_role = |role: Option<hew_mir::StableActorRole>| {
        if let Some(role) = role {
            slots.push(resolve(role.slot_index));
        }
    };
    for block in &func.blocks {
        if let Some(hew_mir::SuspendKind::Ask { stable_role, .. }) =
            func.suspend_kinds.get(&block.id)
        {
            push_role(*stable_role);
        }
        match &block.terminator {
            Terminator::Send { stable_role, .. } | Terminator::Ask { stable_role, .. } => {
                push_role(*stable_role);
            }
            Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
                for arm in arms {
                    if let hew_mir::SelectArmKind::ActorAsk { stable_role, .. } = &arm.kind {
                        push_role(*stable_role);
                    }
                }
            }
            Terminator::Join { branches, .. } => {
                for branch in branches {
                    push_role(branch.stable_role);
                }
            }
            _ => {}
        }
    }
    slots
}

/// P0 sibling-ask misroute regression (dogfood F1, mechanism 1): inside an
/// actor receive handler (the suspending lowering path), each stable-role
/// child accessor must bake ITS OWN slot-index constant. Before the fix,
/// `lower_actor_body_handlers` lowered handlers with an EMPTY
/// `supervisor_layout_map`, so `partitioned_static_slot_index` fell back to 0
/// for every access — every `await sup.<child>.<method>()` after the first
/// misrouted to slot 0 regardless of the child written in source.
#[test]
fn actor_handler_sibling_asks_bake_distinct_slot_indices() {
    let source = r"
actor Worker {
    let id: i64;
    receive fn whoami() -> i64 { id }
}

supervisor Pool {
    strategy: one_for_one;
    child w1: Worker(id: 1);
    child w2: Worker(id: 2);
    child w3: Worker(id: 3);
}

actor Dispatcher {
    var sup: LocalPid<Pool>;
    receive fn run() {
        let a = await sup.w1.whoami();
        let _ = a;
        let b = await sup.w2.whoami();
        let _ = b;
        let c = await sup.w3.whoami();
        let _ = c;
    }
}

fn main() {
    let sup = spawn Pool;
    let d = spawn Dispatcher(sup: sup);
    d.run();
}
";
    let pipeline = lower_module(source);
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.contains("Dispatcher") && f.name.contains("run"))
        .expect("Dispatcher receive handler lowered");
    let slots = stable_role_slot_constants(handler);
    assert_eq!(
        slots,
        vec![0, 1, 2],
        "each sibling accessor in a receive handler must bake its own slot \
         index (w1=0, w2=1, w3=2); a collapsed vector means handler lowering \
         lost the supervisor layout map"
    );
}

/// Blast-radius companion for the same defect class: `select` arms that ask
/// DIFFERENT supervisor children from inside a receive handler share the
/// stable-role machinery (`ChildRef` field extraction), so their accessor seeds
/// must also carry distinct slot indices.
#[test]
fn actor_handler_select_arms_bake_distinct_slot_indices() {
    let source = r"
actor Worker {
    let id: i64;
    receive fn whoami() -> i64 { id }
}

supervisor Pool {
    strategy: one_for_one;
    child w1: Worker(id: 1);
    child w2: Worker(id: 2);
}

actor Dispatcher {
    var sup: LocalPid<Pool>;
    receive fn pick() -> i64 {
        let winner = select {
            reply from sup.w1.whoami() => reply,
            reply from sup.w2.whoami() => reply,
            after 500ms => -7,
        };
        winner
    }
}

fn main() {
    let sup = spawn Pool;
    let d = spawn Dispatcher(sup: sup);
    let r = await d.pick();
    let _ = r;
}
";
    let pipeline = lower_module(source);
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.contains("Dispatcher") && f.name.contains("pick"))
        .expect("Dispatcher select handler lowered");
    let slots = stable_role_slot_constants(handler);
    assert_eq!(
        slots,
        vec![0, 1],
        "select arms asking different supervisor children must bake distinct \
         slot indices (w1=0, w2=1)"
    );
}

#[test]
fn pool_child_field_and_get_lower_end_to_end() {
    let source = r"
        actor Worker { receive fn ping() {} }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool workers: Worker count: 2
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
    let has_pool_lookup =
        runtime_terminator_calls(func, RuntimeCallFamily::LocalPidSupervisorPoolChildRefGet)
            .next()
            .is_some();
    assert!(
        has_pool_lookup,
        "pool.get must emit the stable pool ChildRef lookup"
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
