#[path = "support/runtime_utf8.rs"]
mod fixture;

use fixture::{decode_module, decode_result_ty, facts, utf8_error_ty};
use hew_sir::{
    runtime_variant_shape_refs, verify_module, AggregateShapeId, CallResult, CallUnwind,
    SemTerminator, SirDiagnosticKind, VariantShapeId,
};
use hew_types::{CloneKind, ResolvedTy, RuntimeVariantResultKind, TypeInstanceKey, ValueClass};

#[test]
fn validating_utf8_decode_returns_an_owned_variant_on_the_normal_edge() {
    let module = decode_module();
    assert!(
        verify_module(&module).is_empty(),
        "exact UTF-8 result contract must verify: {:#?}",
        verify_module(&module)
    );
    let result_ty = match &module.functions[0].blocks[0].terminator {
        SemTerminator::RtCall {
            result: CallResult::Value(result),
            unwind,
            ..
        } => {
            assert_eq!(*unwind, CallUnwind::NotApplicable);
            result.ty.clone()
        }
        other => panic!("fixture must contain a value-producing runtime call: {other:?}"),
    };
    let refs = runtime_variant_shape_refs(
        RuntimeVariantResultKind::Utf8Decode,
        &result_ty,
        &module.aggregate_shapes,
        &module.variant_shapes,
    )
    .expect("the semantic result must join to all exact demanded descriptors");
    assert_eq!(refs.result, VariantShapeId(0));
    assert_eq!(refs.error, AggregateShapeId(0));
    assert_eq!(refs.error_len, VariantShapeId(1));
}

#[test]
fn validating_utf8_decode_refuses_a_same_leaf_error_type() {
    let mut module = decode_module();
    let lookalike = utf8_error_ty("application.Utf8Error");
    let result_ty = decode_result_ty(lookalike.clone());
    let SemTerminator::RtCall {
        result: CallResult::Value(result),
        ..
    } = &mut module.functions[0].blocks[0].terminator
    else {
        panic!("fixture must contain a value-producing runtime call");
    };
    result.ty = result_ty.clone();
    module.functions[0].blocks[1].args[0].ty = result_ty.clone();
    module.variant_shapes[0].enum_ty = result_ty;
    module.variant_shapes[0].variants[1].fields[0].ty = lookalike;

    assert!(verify_module(&module).iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("exact runtime variant result")
                || reason.contains("does not admit")
    )));
}

#[test]
fn validating_utf8_decode_refuses_a_malformed_nested_option_descriptor() {
    let mut module = decode_module();
    module.variant_shapes[1].variants[0].fields[0].ty = ResolvedTy::U8;
    module.type_facts.insert(
        TypeInstanceKey(ResolvedTy::U8),
        facts(ValueClass::BitCopy, CloneKind::Bits),
    );

    assert!(verify_module(&module).iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("malformed Option descriptor")
    )));
}
