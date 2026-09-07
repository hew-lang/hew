use super::tests::target_for_inventory;
use super::*;
use local_fixture::Case;

fn fixture(case: Case) -> PhysicalModule {
    let semantic = local_fixture::module(case);
    lower_physical_module(&semantic, target_for_inventory(&semantic))
        .unwrap()
        .into_unverified()
}
fn probe(module: &mut PhysicalModule) -> &mut PhysicalFunction {
    let id = module
        .callables
        .iter()
        .find(|callable| callable.declaration.full_path() == "probe")
        .unwrap()
        .id;
    module
        .functions
        .iter_mut()
        .find(|function| function.callable == id)
        .unwrap()
}
fn refuses(module: &PhysicalModule, expected: &str) {
    let error = verify_physical_module(module).unwrap_err();
    assert!(
        error.message.contains(expected),
        "expected {expected}: {error:?}"
    );
}

#[test]
fn local_lifetimes_lower_across_joins_loans_empty_loops_and_trap_cleanup() {
    for case in [
        Case::JoinEnd,
        Case::JoinAssign,
        Case::Borrow,
        Case::EmptyLoop,
        Case::ZeroSized,
        Case::LinearTrap,
    ] {
        fixture(case);
    }
}

#[test]
fn unexpanded_local_and_zero_sized_storage_each_have_one_content_cell() {
    for case in [Case::JoinEnd, Case::ZeroSized] {
        let mut module = fixture(case);
        let function = probe(&mut module);
        let local = function
            .storage
            .iter()
            .find(|slot| matches!(slot.origin, StorageOrigin::Local(_)))
            .unwrap();
        let partition = &function.place_storage[&local.id];
        assert!(partition.path.is_empty());
        assert_eq!(partition.leaves.len(), 1);
        assert_eq!(partition.leaves[0].storage, local.id);
        if matches!(case, Case::ZeroSized) {
            assert_eq!(local.layout.size, 0);
            assert!(partition.leaves[0].destroy.is_none());
        }
    }
}

#[test]
fn physical_lifetime_rejects_double_start_missing_start_and_active_empty_exit() {
    for mutation in 0..3 {
        let mut module = fixture(Case::EmptyLoop);
        let operations = &mut probe(&mut module).blocks[1].ops;
        match mutation {
            0 => {
                operations.insert(1, operations[0].clone());
            }
            1 => {
                operations.remove(0);
            }
            2 => {
                operations.pop();
            }
            _ => unreachable!(),
        }
        refuses(
            &module,
            if mutation == 1 {
                "inactive local"
            } else {
                "active"
            },
        );
    }
}

#[test]
fn physical_local_rejects_uninitialized_join_reads_and_live_zero_sized_initialization() {
    let mut module = fixture(Case::JoinEnd);
    let function = probe(&mut module);
    let take = function.blocks[1].ops[0].clone();
    function.blocks[2].ops.insert(0, take);
    refuses(&module, "not initialized on every path");
    let mut module = fixture(Case::ZeroSized);
    probe(&mut module).blocks[0].ops.remove(2);
    // Reuse the parameter to isolate StoreInit's live-cell check from SSA reads.
    let function = probe(&mut module);
    let PhysicalOp::Transfer { source, .. } = &mut function.blocks[0].ops[2] else {
        panic!("init")
    };
    *source = function.parameters[0];
    refuses(&module, "overwrites initialized aggregate contents");
}

#[test]
fn physical_local_cleanup_cannot_end_a_live_nested_loan_or_reuse_a_certificate() {
    let mut module = fixture(Case::Borrow);
    let function = probe(&mut module);
    let end = function.blocks[0].ops.pop().unwrap();
    function.blocks[0].ops.insert(4, end);
    refuses(&module, "dependent loan");
    let mut module = fixture(Case::JoinEnd);
    let function = probe(&mut module);
    // A harmless scalar definition changes the cleanup site but not lifetimes.
    function.blocks[2].ops.insert(
        0,
        PhysicalOp::Const {
            dest: function.parameters[1],
            value: PhysicalConst::Bool(false),
        },
    );
    refuses(&module, "certified source or site");
}

#[test]
fn linear_cleanup_requires_the_checked_trap_disposition() {
    let mut semantic = local_fixture::module(Case::LinearTrap);
    local_fixture::probe(&mut semantic).blocks[0].terminator =
        SemTerminator::Return { value: None };
    assert!(lower_physical_module(&semantic, target_for_inventory(&semantic)).is_err());
    let mut trap = fixture(Case::LinearTrap);
    let mut ordinary = fixture(Case::JoinEnd);
    let PhysicalOp::StorageDead {
        cleanup: ordinary_cleanup,
        ..
    } = probe(&mut ordinary).blocks[2].ops.last().unwrap()
    else {
        panic!("ordinary cleanup")
    };
    let PhysicalOp::StorageDead { cleanup, .. } =
        probe(&mut trap).blocks[0].ops.last_mut().unwrap()
    else {
        panic!("trap cleanup")
    };
    assert_eq!(cleanup.mode(), hew_sir::CleanupMode::Trap);
    *cleanup = ordinary_cleanup.clone();
    refuses(&trap, "ordinary cleanup cannot discard live linear");
}

#[test]
fn physical_trap_certificate_cannot_survive_an_ordinary_exit_or_later_effect() {
    let mut accepted = vec![];
    for mutation in 0..3 {
        let mut module = fixture(Case::LinearTrap);
        let function = probe(&mut module);
        if mutation == 1 {
            // Define a scalar after EndLifetime without moving its certified site.
            function.blocks[0].ops.push(PhysicalOp::Const {
                dest: function.parameters[1],
                value: PhysicalConst::Bool(false),
            });
        } else if mutation == 0 {
            function.blocks[0].terminator = PhysicalTerminator::Return { value: None };
        } else {
            let trap = function.blocks[0].terminator.clone();
            function.blocks[0].terminator = PhysicalTerminator::RuntimeCall {
                action: PhysicalRuntimeAction::Print {
                    kind: hew_types::runtime_call::PrintKind::Bool,
                    newline: true,
                },
                args: vec![ArgumentTransfer::Clone {
                    source: function.parameters[1],
                    action: CloneAction::Bitwise,
                }],
                result: None,
                normal: PhysicalEdge {
                    target: BlockId(1),
                    transfers: vec![],
                    leaf_transfers: vec![],
                },
                failure: None,
            };
            function.blocks.push(PhysicalBlock {
                id: BlockId(1),
                arguments: vec![],
                ops: vec![],
                terminator: trap,
            });
        }
        match verify_physical_module(&module) {
            Ok(()) => accepted.push(mutation),
            Err(error) => assert!(
                error.message.contains("certified trap cleanup region"),
                "{error:?}"
            ),
        }
    }
    assert!(
        accepted.is_empty(),
        "accepted stale cleanup certificates: {accepted:?}"
    );
}

#[test]
fn certified_cleanup_can_cross_a_finite_diamond_but_cannot_escape_or_cycle() {
    let mut module = fixture(Case::LinearTrap);
    let function = probe(&mut module);
    let trap = function.blocks[0].terminator.clone();
    let edge = |target| PhysicalEdge {
        target: BlockId(target),
        transfers: vec![],
        leaf_transfers: vec![],
    };
    let block = |id, terminator| PhysicalBlock {
        id: BlockId(id),
        arguments: vec![],
        ops: vec![],
        terminator,
    };
    function.blocks[0].terminator = PhysicalTerminator::Goto(edge(1));
    function.blocks.extend([
        block(
            1,
            PhysicalTerminator::Branch {
                condition: function.parameters[1],
                then_target: edge(2),
                else_target: edge(3),
            },
        ),
        block(2, PhysicalTerminator::Goto(edge(4))),
        block(3, PhysicalTerminator::Goto(edge(4))),
        block(4, trap),
    ]);
    verify_physical_module(&module).unwrap();
    for cycle in [false, true] {
        let mut broken = module.clone();
        let function = probe(&mut broken);
        if cycle {
            function.blocks[4].terminator = PhysicalTerminator::Goto(PhysicalEdge {
                target: BlockId(1),
                transfers: vec![],
                leaf_transfers: vec![],
            });
        } else {
            function.blocks[3].terminator = PhysicalTerminator::Return { value: None };
        }
        refuses(&broken, "certified trap cleanup region");
    }
}

#[test]
fn certified_fault_cleanup_drains_tasks_without_admitting_ordinary_continuation() {
    let mut module = fixture(Case::LinearTrap);
    let function = probe(&mut module);
    let edge = PhysicalEdge {
        target: BlockId(1),
        transfers: vec![],
        leaf_transfers: vec![],
    };
    function.blocks[0].terminator = PhysicalTerminator::TaskScopeJoin {
        scope: TaskScopeId(0),
        mode: hew_sir::TaskScopeJoinMode::PropagateFault,
        normal: edge.clone(),
        unwind: edge,
    };
    function.blocks.push(PhysicalBlock {
        id: BlockId(1),
        arguments: vec![],
        ops: vec![PhysicalOp::TaskScopeClose {
            scope: TaskScopeId(0),
        }],
        terminator: PhysicalTerminator::PropagateFault,
    });
    // Scope ancestry and fault availability have separate physical analyses.
    // This refinement must retain its incoming-fault obligation across a drain.
    let needs_fault = partial::verify_trap_cleanup_refinement(function).unwrap();
    assert!(needs_fault.contains(&BlockId(0)));
    assert!(needs_fault.contains(&BlockId(1)));

    for ordinary_join in [false, true] {
        let mut broken = function.clone();
        if ordinary_join {
            let PhysicalTerminator::TaskScopeJoin { mode, .. } = &mut broken.blocks[0].terminator
            else {
                panic!("task drain")
            };
            *mode = hew_sir::TaskScopeJoinMode::Wait;
        } else {
            broken.blocks[1].terminator = PhysicalTerminator::Return { value: None };
        }
        assert!(partial::verify_trap_cleanup_refinement(&broken).is_err());
    }
}

#[test]
fn expanded_local_partitions_refuse_missing_zero_sized_cells_and_synthetic_root_bits() {
    let semantic = partial_fixture::local_module(partial_fixture::Case::MixedReplacement);
    let module = lower_physical_module(&semantic, target_for_inventory(&semantic))
        .unwrap()
        .into_unverified();
    let function = &module.functions[0];
    let root = function
        .storage
        .iter()
        .find(|slot| matches!(slot.origin, StorageOrigin::Local(_)))
        .unwrap()
        .id;
    let partition = &function.place_storage[&root];
    assert_eq!(partition.leaves.len(), 5);
    assert!(partition.leaves.iter().all(|leaf| leaf.storage != root));
    for root_bit in [false, true] {
        let mut broken = module.clone();
        let partition = broken.functions[0].place_storage.get_mut(&root).unwrap();
        if root_bit {
            partition.leaves.push(PhysicalPlaceLeaf {
                storage: root,
                destroy: None,
            });
        } else {
            partition
                .leaves
                .retain(|leaf| function.storage[leaf.storage.0 as usize].ty != ResolvedTy::Unit);
        }
        assert!(verify_physical_module(&broken).is_err());
    }
}
