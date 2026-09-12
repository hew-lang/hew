use super::tests::target_for_inventory;
use super::*;
use partial_fixture::Case;

fn fixture(case: Case) -> PhysicalModule {
    let semantic = partial_fixture::module(case);
    let diagnostics = hew_sir::verify_module(&semantic);
    assert!(diagnostics.is_empty(), "{case:?}: {diagnostics:?}");
    lower_physical_module(&semantic, target_for_inventory(&semantic))
        .unwrap_or_else(|error| panic!("{case:?}: {error:?}"))
        .into_unverified()
}

#[test]
fn partial_roots_lower_through_replacement_permutation_fault_and_zero_sized_paths() {
    for case in [
        Case::MixedReplacement,
        Case::LiveReplacement,
        Case::DeadReplacement,
        Case::BranchReplacement,
        Case::Permutation,
        Case::Fault,
        Case::ZeroSized,
    ] {
        fixture(case);
    }
}

#[test]
fn aggregate_alias_paths_preserve_zero_sized_leaf_identity() {
    let physical = fixture(Case::ZeroSized);
    let function = &physical.functions[0];
    let root = &function.place_storage[&function.parameters[0]];
    assert_eq!(root.leaves.len(), 5);
    let zero = root
        .leaves
        .iter()
        .find(|leaf| function.storage[leaf.storage.0 as usize].ty == ResolvedTy::Unit)
        .unwrap();
    assert_eq!(function.storage[zero.storage.0 as usize].layout.size, 0);
    assert!(zero.destroy.is_none());
    let alias = &function.place_storage[&zero.storage];
    assert_eq!(alias.root, function.parameters[0]);
    assert_eq!(alias.path[0].field, 2);
    assert_eq!(alias.leaves, vec![zero.clone()]);
}

#[test]
fn verifier_rejects_changed_paths_partitions_and_leaf_cleanup() {
    for mutation in 0..5 {
        let mut physical = fixture(Case::MixedReplacement);
        let function = &mut physical.functions[0];
        let root = function.parameters[0];
        let leaf = function.place_storage[&root].leaves[0].storage;
        match mutation {
            0 => function.place_storage.get_mut(&leaf).unwrap().path[0].field = 99,
            1 => {
                function.place_storage.get_mut(&root).unwrap().leaves.pop();
            }
            2 => function.place_storage.get_mut(&root).unwrap().leaves[0].destroy = None,
            3 => {
                function.place_storage.remove(&leaf);
            }
            4 => function.storage[leaf.0 as usize].own = OwnKind::None,
            _ => unreachable!(),
        }
        let error = verify_physical_module(&physical).unwrap_err();
        assert!(
            error.message.contains("aggregate"),
            "mutation {mutation}: {error:?}"
        );
    }
}

#[test]
fn verifier_rejects_missing_or_permuted_initialization_transfers() {
    for reverse in [false, true] {
        let mut physical = fixture(Case::Permutation);
        let PhysicalTerminator::Goto(edge) = &mut physical.functions[0].blocks[2].terminator else {
            panic!("loop backedge")
        };
        if reverse {
            edge.leaf_transfers.swap(0, 1);
        } else {
            edge.leaf_transfers.pop();
        }
        let error = verify_physical_module(&physical).unwrap_err();
        assert!(
            error.message.contains("initialization transfer map"),
            "{error:?}"
        );
    }

    let mut physical = fixture(Case::Permutation);
    let PhysicalTerminator::Goto(edge) = &mut physical.functions[0].blocks[0].terminator else {
        panic!("loop entry")
    };
    edge.transfers[1].0 = edge.transfers[0].0;
    let error = verify_physical_module(&physical).unwrap_err();
    assert!(
        error.message.contains("owning root more than once"),
        "{error:?}"
    );
}

#[test]
fn verifier_rejects_taking_a_leaf_twice_and_a_whole_root_with_a_dead_zero_sized_leaf() {
    let mut physical = fixture(Case::MixedReplacement);
    let operation = physical.functions[0].blocks[0].ops[0].clone();
    physical.functions[0].blocks[0].ops.insert(2, operation);
    let error = verify_physical_module(&physical).unwrap_err();
    assert!(error.message.contains("uninitialized storage"), "{error:?}");

    let mut physical = fixture(Case::ZeroSized);
    physical.functions[0].blocks[0].ops.remove(1);
    let error = verify_physical_module(&physical).unwrap_err();
    assert!(error.message.contains("uninitialized storage"), "{error:?}");
}
