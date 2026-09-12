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
        let PhysicalTerminator::TaskSelect { sources, .. } =
            &physical.functions[0].blocks[0].terminator
        else {
            panic!("missing task selection")
        };
        assert_eq!(sources.len(), count as usize);
        assert!(sources
            .iter()
            .all(|source| matches!(source, PhysicalSelectSource::Task(_))));
        assert!(sources
            .iter()
            .all(|source| matches!(source.transfer(), ArgumentTransfer::Borrow(_))));
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
    let PhysicalTerminator::TaskSelect { sources, .. } =
        &mut physical.functions[0].blocks[0].terminator
    else {
        unreachable!()
    };
    let ArgumentTransfer::Borrow(slot) = sources[0].transfer() else {
        unreachable!()
    };
    sources[0] = PhysicalSelectSource::Task(ArgumentTransfer::Move(slot));
    let error = verify_physical_module(&physical).unwrap_err();
    assert!(
        error.to_string().contains("borrowed source handles"),
        "{error}"
    );
}
