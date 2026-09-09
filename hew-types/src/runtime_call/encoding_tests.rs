#[allow(
    clippy::wildcard_imports,
    reason = "test the enclosing descriptor authority"
)]
use super::*;

fn value(format: EncodingFormat) -> ResolvedTy {
    ResolvedTy::Named {
        name: format.builtin().canonical_name().to_string(),
        args: vec![],
        builtin: Some(format.builtin()),
        is_opaque: true,
    }
}

/// Independent source ABI expectations, including C-void mutation results.
fn source_signature(
    op: EncodingOp,
    value: &ResolvedTy,
) -> (Vec<ResolvedTy>, ResolvedTy, Vec<bool>) {
    use ResolvedTy::{String, Unit, F64, I32, I64, U64};
    let (params, result) = match op {
        EncodingOp::Parse | EncodingOp::FromString => (vec![String], value.clone()),
        EncodingOp::LastError => (vec![], String),
        EncodingOp::Stringify | EncodingOp::GetString => (vec![value.clone()], String),
        EncodingOp::Type | EncodingOp::IntStatus | EncodingOp::GetBool | EncodingOp::ArrayLen => {
            (vec![value.clone()], I32)
        }
        EncodingOp::GetInt => (vec![value.clone()], I64),
        EncodingOp::GetU64 => (vec![value.clone()], U64),
        EncodingOp::GetFloat => (vec![value.clone()], F64),
        EncodingOp::GetField => (vec![value.clone(), String], value.clone()),
        EncodingOp::ArrayGet => (vec![value.clone(), I32], value.clone()),
        EncodingOp::ObjectNew | EncodingOp::ArrayNew | EncodingOp::FromNull => {
            (vec![], value.clone())
        }
        EncodingOp::FromBool => (vec![I32], value.clone()),
        EncodingOp::FromInt => (vec![I64], value.clone()),
        EncodingOp::FromU64 => (vec![U64], value.clone()),
        EncodingOp::FromFloat => (vec![F64], value.clone()),
        EncodingOp::Eq => (vec![value.clone(), value.clone()], I32),
        EncodingOp::ObjectSet => (vec![value.clone(), String, value.clone()], Unit),
        EncodingOp::ArrayPush => (vec![value.clone(), value.clone()], Unit),
        EncodingOp::Clone | EncodingOp::ObjectKeys => (vec![value.clone()], value.clone()),
        EncodingOp::Free => (vec![value.clone()], Unit),
    };
    let mut consuming = vec![false; params.len()];
    match op {
        EncodingOp::ObjectSet => consuming[2] = true,
        EncodingOp::ArrayPush => consuming[1] = true,
        EncodingOp::Free => consuming[0] = true,
        _ => {}
    }
    (params, result, consuming)
}

#[test]
fn every_encoding_extern_checks_its_complete_source_abi_and_identity() {
    for format in EncodingFormat::iter() {
        for op in EncodingOp::iter() {
            let family = RuntimeCallFamily::Encoding { format, op };
            let symbol = family.c_symbol();
            let declaration = format!("{}.{}", format.module(), symbol);
            let (params, result, consuming) = source_signature(op, &value(format));
            let admits = |params: &[ResolvedTy], result: &ResolvedTy, consuming: &[bool]| {
                family.matches_encoding_extern(
                    format.module(),
                    &declaration,
                    symbol,
                    params,
                    result,
                    consuming,
                )
            };
            assert!(admits(&params, &result, &consuming), "{family:?}");
            assert!(
                !admits(&params, &ResolvedTy::Bool, &consuming),
                "wrong result: {family:?}"
            );
            let mut extra = params.clone();
            extra.push(ResolvedTy::I32);
            assert!(
                !admits(&extra, &result, &consuming),
                "extra argument: {family:?}"
            );
            for index in 0..params.len() {
                let mut wrong = params.clone();
                wrong[index] = ResolvedTy::Bool;
                assert!(
                    !admits(&wrong, &result, &consuming),
                    "wrong arg {index}: {family:?}"
                );
                let mut wrong_ownership = consuming.clone();
                wrong_ownership[index] = !wrong_ownership[index];
                assert!(
                    !admits(&params, &result, &wrong_ownership),
                    "wrong ownership {index}: {family:?}"
                );
            }
            assert!(!family.matches_encoding_extern(
                "user.json",
                &declaration,
                symbol,
                &params,
                &result,
                &consuming
            ));
            assert!(!family.matches_encoding_extern(
                format.module(),
                symbol,
                symbol,
                &params,
                &result,
                &consuming
            ));
            assert!(!family.matches_encoding_extern(
                format.module(),
                &declaration,
                "user_parse",
                &params,
                &result,
                &consuming
            ));
        }
    }
}

#[test]
fn encoding_receiver_binding_preserves_the_owner_and_rejects_impostors() {
    let original = value(EncodingFormat::Json);
    let contract = EncodingOp::GetField.contract(EncodingFormat::Json);
    let bound = contract
        .instantiate(&[original.clone(), ResolvedTy::String], &original)
        .unwrap();
    assert_eq!(bound.result_ty, original);
    assert!(collection_type_arguments(&original).is_none());
    assert!(RuntimeValueKind::TypeArgument(0)
        .resolve(Some(&original))
        .is_none());

    let malformed = [
        value(EncodingFormat::Yaml),
        ResolvedTy::named_opaque("std.encoding.json.Value", vec![]),
        ResolvedTy::named_builtin("alias.Value", BuiltinType::JsonValue, vec![]),
        ResolvedTy::named_builtin(
            "std.encoding.json.Value",
            BuiltinType::JsonValue,
            vec![ResolvedTy::I64],
        ),
    ];
    for impostor in malformed {
        assert!(!contract.matches_signature(&[impostor.clone(), ResolvedTy::String], &impostor));
        assert!(!contract.matches_signature(&[original.clone(), ResolvedTy::String], &impostor));
        assert!(!EncodingOp::FromInt
            .contract(EncodingFormat::Json)
            .matches_signature(&[ResolvedTy::I64], &impostor));
    }
    for (op, scalar) in [
        (EncodingOp::FromInt, ResolvedTy::I64),
        (EncodingOp::FromU64, ResolvedTy::U64),
        (EncodingOp::FromFloat, ResolvedTy::F64),
    ] {
        let bound = op
            .contract(EncodingFormat::Json)
            .instantiate(&[scalar], &original)
            .unwrap();
        assert_eq!(bound.result_ty, original);
    }
}

#[test]
fn encoding_mutation_moves_both_owners_and_returns_the_updated_receiver() {
    for format in EncodingFormat::iter() {
        for op in [EncodingOp::ObjectSet, EncodingOp::ArrayPush] {
            let owner = value(format);
            let (params, _, consuming) = source_signature(op, &owner);
            let family = RuntimeCallFamily::Encoding { format, op };
            let contract = family.semantic_contract().unwrap();
            let child = params.len() - 1;
            assert!(contract.failures.is_empty());
            assert_eq!(contract.arguments[0].effect, RuntimeArgumentEffect::Move);
            assert_eq!(
                contract.arguments[child].effect,
                RuntimeArgumentEffect::Move
            );
            assert_eq!(
                contract.result,
                RuntimeResultEffect::UpdatedReceiver(RuntimeValueKind::Receiver(format.builtin()))
            );
            assert_eq!(
                contract
                    .instantiate(&params, &ResolvedTy::Unit)
                    .unwrap()
                    .result_ty,
                owner
            );
            assert!(!contract.matches_signature(&params, &ResolvedTy::Unit));
            assert_eq!(
                family.arg_consume_verdict(child),
                ConsumeVerdict::ProvenConsume
            );
            assert!(family.consumes_receiver());
            assert!(!consuming[0] && consuming[child]);
        }
    }
}
