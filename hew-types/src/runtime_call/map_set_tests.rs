//! Tests for map/set semantic contracts.

use super::*;

fn builtin(kind: BuiltinType, arguments: Vec<ResolvedTy>) -> ResolvedTy {
    ResolvedTy::named_builtin(kind.canonical_name(), kind, arguments)
}

#[test]
fn map_selection_and_projection_keep_exact_nested_value_types() {
    let value = builtin(
        BuiltinType::Vec,
        vec![builtin(
            BuiltinType::Result,
            vec![ResolvedTy::I64, ResolvedTy::String],
        )],
    );
    let map = builtin(
        BuiltinType::HashMap,
        vec![ResolvedTy::String, value.clone()],
    );
    let optional = builtin(BuiltinType::Option, vec![value.clone()]);
    let get = RuntimeCallFamily::Map(MapValueOp::Get)
        .semantic_contract()
        .unwrap();
    assert!(get.matches_signature(&[map.clone(), ResolvedTy::String], &optional));
    assert!(!get.matches_signature(&[map.clone(), ResolvedTy::I64], &optional));
    assert!(!get.matches_signature(
        &[map.clone(), ResolvedTy::String],
        &builtin(BuiltinType::Option, vec![ResolvedTy::String])
    ));
    assert!(!get.matches_signature(
        &[map.clone(), ResolvedTy::String],
        &ResolvedTy::named_user("Option", vec![value.clone()])
    ));

    let entries = RuntimeCallFamily::Map(MapValueOp::Entries)
        .semantic_contract()
        .unwrap();
    let pairs = builtin(
        BuiltinType::Vec,
        vec![ResolvedTy::Tuple(vec![ResolvedTy::String, value.clone()])],
    );
    assert!(entries.matches_signature(std::slice::from_ref(&map), &pairs));
    assert!(!entries.matches_signature(
        std::slice::from_ref(&map),
        &builtin(
            BuiltinType::Vec,
            vec![ResolvedTy::Tuple(vec![value, ResolvedTy::String])]
        )
    ));
    assert_eq!(
        RuntimeCallFamily::Map(MapValueOp::Get).result_authority(),
        RuntimeResultAuthority::IndependentValue
    );
    assert_eq!(
        RuntimeCallFamily::Map(MapValueOp::Entries).result_authority(),
        RuntimeResultAuthority::IndependentValue
    );
}

#[test]
fn collection_constructors_require_canonical_identity_and_arity() {
    for (family, kind, arguments) in [
        (
            RuntimeCallFamily::Map(MapValueOp::New),
            BuiltinType::HashMap,
            vec![ResolvedTy::String, ResolvedTy::I64],
        ),
        (
            RuntimeCallFamily::Set(SetValueOp::New),
            BuiltinType::HashSet,
            vec![ResolvedTy::String],
        ),
    ] {
        let contract = family.semantic_contract().unwrap();
        let receiver = builtin(kind, arguments.clone());
        assert!(contract.matches_signature(&[], &receiver));
        assert!(contract.matches_signature(
            &[],
            &ResolvedTy::named_builtin("renamed.Collection", kind, arguments.clone())
        ));
        assert!(!contract.matches_signature(
            &[],
            &ResolvedTy::named_user(kind.canonical_name(), arguments)
        ));
        assert!(!contract.matches_signature(&[], &builtin(kind, vec![])));
        assert!(
            !contract.matches_signature(&[], &builtin(BuiltinType::Vec, vec![ResolvedTy::String]))
        );
        assert!(!contract.matches_signature(std::slice::from_ref(&receiver), &receiver));
    }
}

#[test]
fn map_updates_replace_the_receiver_and_preserve_input_owners() {
    let map = builtin(
        BuiltinType::HashMap,
        vec![ResolvedTy::String, ResolvedTy::Bytes],
    );
    let insert = RuntimeCallFamily::Map(MapValueOp::Insert);
    assert!(insert
        .semantic_contract()
        .unwrap()
        .matches_signature(&[map.clone(), ResolvedTy::String, ResolvedTy::Bytes], &map));
    assert!(!insert.semantic_contract().unwrap().matches_signature(
        &[map.clone(), ResolvedTy::String, ResolvedTy::Bytes],
        &ResolvedTy::Unit
    ));
    assert_eq!(insert.arg_consume_verdict(0), ConsumeVerdict::ProvenConsume);
    assert_eq!(insert.arg_consume_verdict(1), ConsumeVerdict::ProvenBorrow);
    // The value's ingress follows its clone fact - copied in when it has
    // one, moved in when it has none - so the per-argument table cannot
    // prove a borrow for it.
    assert_eq!(
        insert.arg_consume_verdict(2),
        ConsumeVerdict::ConservativeConsume
    );

    let removed = builtin(BuiltinType::Option, vec![ResolvedTy::Bytes]);
    let remove = RuntimeCallFamily::Map(MapValueOp::Remove)
        .semantic_contract()
        .unwrap();
    assert!(remove.matches_signature(
        &[map.clone(), ResolvedTy::String],
        &ResolvedTy::Tuple(vec![map.clone(), removed.clone()])
    ));
    assert!(!remove.matches_signature(&[map.clone(), ResolvedTy::String], &removed));
    assert!(!remove.matches_signature(
        &[map, ResolvedTy::String],
        &ResolvedTy::Tuple(vec![
            builtin(
                BuiltinType::HashMap,
                vec![ResolvedTy::I64, ResolvedTy::Bytes]
            ),
            removed
        ])
    ));
}

#[test]
fn set_updates_return_presence_and_adopt_an_owned_element() {
    let set = builtin(BuiltinType::HashSet, vec![ResolvedTy::String]);
    let vector = builtin(BuiltinType::Vec, vec![ResolvedTy::String]);
    for operation in [SetValueOp::Insert, SetValueOp::Remove] {
        let family = RuntimeCallFamily::Set(operation);
        let contract = family.semantic_contract().unwrap();
        assert!(contract.matches_signature(
            &[set.clone(), ResolvedTy::String],
            &ResolvedTy::Tuple(vec![set.clone(), ResolvedTy::Bool])
        ));
        assert!(!contract.matches_signature(
            &[vector.clone(), ResolvedTy::String],
            &ResolvedTy::Tuple(vec![vector.clone(), ResolvedTy::Bool])
        ));
        assert!(!contract.matches_signature(&[set.clone(), ResolvedTy::String], &ResolvedTy::Bool));
        // The element's ingress follows its clone fact - copied in when
        // it has one, moved in when it has none - so insertion cannot
        // prove a borrow for it, while removal only probes.
        let expected = if operation == SetValueOp::Insert {
            ConsumeVerdict::ConservativeConsume
        } else {
            ConsumeVerdict::ProvenBorrow
        };
        assert_eq!(family.arg_consume_verdict(1), expected);
        assert!(family.invalidates_collection_element_aliases());
    }
    let elements = RuntimeCallFamily::Set(SetValueOp::Elements);
    assert!(elements
        .semantic_contract()
        .unwrap()
        .matches_signature(std::slice::from_ref(&set), &vector));
    assert!(!elements.invalidates_collection_element_aliases());
}
