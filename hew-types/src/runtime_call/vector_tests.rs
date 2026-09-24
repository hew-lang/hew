//! Tests for vector semantic contracts.

use super::*;

fn vector(element: ResolvedTy) -> ResolvedTy {
    ResolvedTy::named_builtin("Vec", crate::BuiltinType::Vec, vec![element])
}

#[test]
fn vector_contract_binds_receiver_element_and_result_together() {
    let values = vector(ResolvedTy::String);
    let other = vector(ResolvedTy::I64);
    let contract = RuntimeCallFamily::Vector(VecValueOp::Push)
        .semantic_contract()
        .unwrap();
    assert!(contract.matches_signature(&[values.clone(), ResolvedTy::String], &values));
    assert!(!contract.matches_signature(&[values.clone(), ResolvedTy::I64], &values));
    assert!(!contract.matches_signature(&[values.clone(), ResolvedTy::String], &other));
    assert!(!contract.matches_signature(
        &[
            ResolvedTy::named_user("Vec", vec![ResolvedTy::String]),
            ResolvedTy::String
        ],
        &values
    ));
    assert!(!contract.matches_signature(std::slice::from_ref(&values), &values));
    let renamed = ResolvedTy::named_builtin(
        "std.collections.Sequence",
        crate::BuiltinType::Vec,
        vec![ResolvedTy::String],
    );
    assert!(contract.matches_signature(&[renamed.clone(), ResolvedTy::String], &renamed));
}

#[test]
fn vector_results_are_exact_for_every_operation() {
    let values = vector(vector(ResolvedTy::String));
    let element = vector(ResolvedTy::String);
    let optional =
        ResolvedTy::named_builtin("Option", crate::BuiltinType::Option, vec![element.clone()]);
    let cases = [
        (VecValueOp::New, vec![], values.clone()),
        (VecValueOp::Len, vec![values.clone()], ResolvedTy::I64),
        (
            VecValueOp::Index,
            vec![values.clone(), ResolvedTy::I64],
            element.clone(),
        ),
        (
            VecValueOp::Get,
            vec![values.clone(), ResolvedTy::I64],
            optional.clone(),
        ),
        (
            VecValueOp::Set,
            vec![values.clone(), ResolvedTy::I64, element],
            values.clone(),
        ),
        (
            VecValueOp::Pop,
            vec![values.clone()],
            ResolvedTy::Tuple(vec![values.clone(), vector(ResolvedTy::String)]),
        ),
        (VecValueOp::Clear, vec![values.clone()], values),
    ];
    for (op, args, result) in cases {
        let contract = RuntimeCallFamily::Vector(op).semantic_contract().unwrap();
        assert!(contract.matches_signature(&args, &result), "{op:?}");
        assert!(
            !contract.matches_signature(&args, &ResolvedTy::Bool),
            "{op:?} must reject an unrelated result"
        );
    }
}

#[test]
fn vector_read_contracts_never_publish_an_interior_owner() {
    assert_eq!(
        RuntimeCallFamily::Vector(VecValueOp::Index)
            .semantic_contract()
            .unwrap()
            .result,
        RuntimeResultEffect::IndependentValue(RuntimeValueKind::TypeArgument(0))
    );
    assert_eq!(
        RuntimeCallFamily::Vector(VecValueOp::Get)
            .semantic_contract()
            .unwrap()
            .result,
        RuntimeResultEffect::IndependentValue(RuntimeValueKind::Applied(
            BuiltinType::Option,
            &[RuntimeValueKind::TypeArgument(0)],
        ))
    );
    assert_eq!(
        RuntimeCallFamily::Vector(VecValueOp::Index)
            .semantic_contract()
            .unwrap()
            .failures,
        &[RuntimeLogicalFailure::IndexOutOfBounds]
    );
    assert!(RuntimeCallFamily::Vector(VecValueOp::Get)
        .semantic_contract()
        .unwrap()
        .failures
        .is_empty());
}
