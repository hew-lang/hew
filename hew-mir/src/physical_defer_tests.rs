use super::*;

#[path = "../../hew-sir/tests/support/defer.rs"]
mod fixture;

fn module(nested: bool) -> PhysicalModule {
    let semantic = if nested {
        fixture::nested(true)
    } else {
        fixture::module(true)
    };
    lower_physical_module(&semantic, tests::target_for_inventory(&semantic))
        .unwrap()
        .into_unverified()
}

fn probe(module: &mut PhysicalModule) -> &mut PhysicalFunction {
    let callable = module
        .callables
        .iter()
        .find(|c| c.declaration.full_path() == "probe")
        .unwrap()
        .id;
    module
        .functions
        .iter_mut()
        .find(|f| f.callable == callable)
        .unwrap()
}

fn block(function: &mut PhysicalFunction, id: u32) -> &mut PhysicalBlock {
    function
        .blocks
        .iter_mut()
        .find(|b| b.id == BlockId(id))
        .unwrap()
}

fn refuses(module: &PhysicalModule, expected: &str) {
    let error = verify_physical_module(module).unwrap_err();
    assert!(
        error.to_string().contains(expected),
        "expected {expected:?}: {error}"
    );
}

#[test]
fn physical_defer_verifies_optional_parks_and_rejects_reuse_and_skipped_actions() {
    let mut nested = module(true);
    for body in &mut probe(&mut nested).blocks {
        match &mut body.terminator {
            PhysicalTerminator::EnterDefer {
                defer: DeferId(3),
                park,
                ..
            }
            | PhysicalTerminator::FinishDefer {
                defer: DeferId(3),
                park,
                ..
            } => *park = FaultParkId(0),
            _ => {}
        }
    }
    refuses(&nested, "overwrites a live park");

    let mut wrong = module(false);
    let PhysicalTerminator::FinishDefer { park, .. } = &mut block(probe(&mut wrong), 7).terminator
    else {
        unreachable!()
    };
    *park = FaultParkId(1);
    refuses(&wrong, "finishes the wrong park");

    let mut skipped = module(false);
    let next = match &block(probe(&mut skipped), 11).terminator {
        PhysicalTerminator::FinishDefer { next, .. } => next.clone(),
        _ => unreachable!(),
    };
    let PhysicalTerminator::FinishDefer {
        next: skipped_next, ..
    } = &mut block(probe(&mut skipped), 7).terminator
    else {
        unreachable!()
    };
    *skipped_next = next;
    refuses(&skipped, "invalidates a pending defer dependency");
}

#[test]
fn physical_defer_requires_body_local_cleanup_and_checked_fault_origin() {
    let mut local = module(true);
    block(probe(&mut local), 18).ops.pop();
    refuses(&local, "leaves body-local storage, owners or loans live");

    let mut origin = module(false);
    let PhysicalTerminator::CheckedRaiseFault { kind, .. } =
        &mut block(probe(&mut origin), 15).terminator
    else {
        unreachable!()
    };
    *kind = TrapKind::DivideByZero;
    refuses(&origin, "producing failure edge");

    let mut dependency = module(false);
    let PhysicalOp::RegisterDefer { dependencies, .. } =
        &mut block(probe(&mut dependency), 0).ops[4]
    else {
        unreachable!()
    };
    dependencies.clear();
    refuses(&dependency, "omit free storage places");
}

#[test]
fn physical_defer_rejects_fault_loops_and_ordinary_return_after_fault() {
    let mut stuck = module(false);
    let function = probe(&mut stuck);
    let mut edge = match &block(function, 7).terminator {
        PhysicalTerminator::FinishDefer { next, .. } => next.clone(),
        _ => unreachable!(),
    };
    edge.target = BlockId(16);
    function.blocks.push(PhysicalBlock {
        id: BlockId(16),
        arguments: vec![],
        ops: vec![],
        terminator: PhysicalTerminator::Goto(edge.clone()),
    });
    let PhysicalTerminator::Call { unwind, .. } = &mut block(function, 5).terminator else {
        unreachable!()
    };
    *unwind = Some(edge);
    refuses(&stuck, "call failure bypasses bounded cleanup");

    let mut returned = module(false);
    let function = probe(&mut returned);
    let result = function
        .storage
        .iter()
        .find(|slot| slot.origin == StorageOrigin::Value(hew_sir::ValueId(10)))
        .unwrap()
        .id;
    block(function, 14).terminator = PhysicalTerminator::Return {
        value: Some(ReturnTransfer::Borrow(result)),
    };
    refuses(&returned, "cleanup");
}

#[test]
fn physical_defer_preserves_failed_result_absence() {
    let mut failed = module(false);
    let function = probe(&mut failed);
    let result = function
        .storage
        .iter()
        .find(|slot| slot.origin == StorageOrigin::Value(hew_sir::ValueId(16)))
        .unwrap()
        .id;
    let value = function
        .storage
        .iter()
        .find(|slot| slot.origin == StorageOrigin::Value(hew_sir::ValueId(17)))
        .unwrap()
        .id;
    block(function, 9).ops[0] = PhysicalOp::Transfer {
        dest: value,
        source: result,
    };
    refuses(&failed, "uninitialized");
}

#[test]
fn physical_defer_keeps_the_parked_linear_cleanup_cause() {
    use hew_sir::{BoundaryDecision, SemOpKind, SemTerminator, ValueId};
    let mut semantic = local_fixture::module(local_fixture::Case::LinearTrap);
    semantic
        .string_literals
        .insert(hew_sir::StringLiteralId(0), "body".into());
    let function = local_fixture::probe(&mut semantic);
    function.blocks = vec![
        fixture::block(
            0,
            vec![
                local_fixture::alloc(),
                local_fixture::init(0),
                fixture::op(SemOpKind::RegisterDefer {
                    defer: DeferId(0),
                    scope: DeferScopeId(0),
                    dependencies: vec![hew_sir::PlaceId(0)],
                }),
                fixture::value(
                    SemOpKind::ConstStr(hew_sir::StringLiteralId(0)),
                    2,
                    ResolvedTy::String,
                    OwnKind::Owned,
                ),
            ],
            SemTerminator::Panic {
                message: fixture::boundary(ValueId(2), BoundaryDecision::Borrow),
                cleanup: fixture::edge(1),
            },
        ),
        fixture::block(
            1,
            vec![local_fixture::destroy(2)],
            SemTerminator::EnterDefer {
                defer: DeferId(0),
                park: FaultParkId(0),
                body: fixture::edge(2),
            },
        ),
        fixture::block(
            2,
            vec![local_fixture::end()],
            SemTerminator::FinishDefer {
                defer: DeferId(0),
                park: FaultParkId(0),
                next: fixture::edge(3),
            },
        ),
        fixture::block(
            3,
            vec![],
            SemTerminator::CleanupDispatch {
                normal: fixture::edge(4),
                fault: fixture::edge(5),
            },
        ),
        fixture::block(4, vec![], local_fixture::done()),
        fixture::block(5, vec![], SemTerminator::ResumeUnwind),
    ];
    fixture::normalize(&mut semantic);
    let mut physical = lower_physical_module(&semantic, tests::target_for_inventory(&semantic))
        .unwrap()
        .into_unverified();
    let function = probe(&mut physical);
    let PhysicalTerminator::Panic { cleanup, .. } = block(function, 0).terminator.clone() else {
        unreachable!()
    };
    block(function, 0).terminator = PhysicalTerminator::Goto(cleanup);
    block(function, 0).ops.pop();
    block(function, 1).ops.clear();
    refuses(&physical, "trap-only cleanup lost its fault exit cause");
}
