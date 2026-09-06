use super::*;
use encoding_fixture::{op, operand};

fn stream_owner() -> (SemModule, ResolvedTy) {
    let ty = ResolvedTy::Named {
        name: "Stream".into(),
        args: vec![ResolvedTy::String],
        builtin: Some(BuiltinType::Stream),
        is_opaque: false,
    };
    let mut module = encoding_fixture::skeleton(vec![ty.clone()], ResolvedTy::Unit);
    module.resources.insert(
        ty.clone(),
        hew_sir::ResourceRelease::Builtin(
            hew_types::runtime_call::RuntimeDropDescriptor::StreamClose,
        ),
    );
    module.functions[0].blocks = vec![hew_sir::SemBlock {
        id: BlockId(0),
        args: vec![],
        ops: vec![op(0, SemOpKind::DestroyValue { value: operand(0) }, vec![])],
        terminator: SemTerminator::Return { value: None },
    }];
    (module, ty)
}

#[test]
fn resource_physical_recipe_requires_exact_release_pointer_and_no_copy() {
    let (semantic, ty) = stream_owner();
    let mut target = super::tests::target_for_inventory(&semantic);
    target.insert_layout(
        ty.clone(),
        PhysicalLayout {
            size: 8,
            align: 8,
            repr: PhysicalRepr::Pointer,
        },
    );
    let physical = lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified();
    assert!(matches!(
        physical.functions[0].blocks[0].ops[0],
        PhysicalOp::Destroy {
            action: DestroyAction::Resource(_),
            ..
        }
    ));
    assert!(verify_clone_action(&physical, &ty, OwnKind::Owned, CloneAction::Bitwise).is_err());
    assert!(
        verify_destroy_action(&physical, &ty, OwnKind::Owned, DestroyAction::StringRelease)
            .is_err()
    );
    assert!(verify_destroy_action(
        &physical,
        &ty,
        OwnKind::Owned,
        DestroyAction::Resource(PhysicalResourceId(1))
    )
    .is_err());
    for mutation in 0..3 {
        let mut changed = physical.clone();
        match mutation {
            0 => changed.resources.clear(),
            1 => changed.resources[0].ty = ResolvedTy::String,
            2 => changed.target.insert_layout(
                ty.clone(),
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Integer { bits: 64 },
                },
            ),
            _ => unreachable!(),
        }
        assert!(
            verify_physical_module(&changed).is_err(),
            "mutation {mutation}"
        );
    }
}
