use super::*;

#[path = "../../hew-sir/tests/support/select.rs"]
mod fixture;

fn target(semantic: &SemModule) -> PhysicalTarget {
    let mut target = tests::target_for_inventory(semantic);
    target.insert_layout(
        ResolvedTy::Task(Box::new(ResolvedTy::I64)),
        PhysicalLayout {
            size: 8,
            align: 8,
            repr: PhysicalRepr::Pointer,
        },
    );
    target
}

#[test]
fn task_select_preserves_borrows_and_requires_resumability() {
    for (count, timeout) in [(2, false), (2, true), (0, true)] {
        let semantic = fixture::module(count, timeout);
        let mut physical = lower_physical_module(&semantic, target(&semantic))
            .unwrap()
            .into_unverified();
        assert!(physical.callables[0].is_resumable);
        let PhysicalTerminator::TaskSelect { tasks, .. } =
            &physical.functions[0].blocks[0].terminator
        else {
            panic!("missing task selection")
        };
        assert_eq!(tasks.len(), count as usize);
        assert!(tasks
            .iter()
            .all(|task| matches!(task, ArgumentTransfer::Borrow(_))));
        physical.callables[0].is_resumable = false;
        assert!(verify_physical_module(&physical).is_err());
    }
}

#[test]
fn task_select_rejects_consuming_observations() {
    let semantic = fixture::module(2, true);
    let mut physical = lower_physical_module(&semantic, target(&semantic))
        .unwrap()
        .into_unverified();
    let PhysicalTerminator::TaskSelect { tasks, .. } =
        &mut physical.functions[0].blocks[0].terminator
    else {
        unreachable!()
    };
    let ArgumentTransfer::Borrow(slot) = tasks[0] else {
        unreachable!()
    };
    tasks[0] = ArgumentTransfer::Move(slot);
    let error = verify_physical_module(&physical).unwrap_err();
    assert!(
        error.to_string().contains("borrowed task handles"),
        "{error}"
    );
}
