use std::collections::BTreeMap;

use hew_hir::ItemId;
use hew_sir::{
    AggregateShapeId, BlockArg, BlockId, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind,
    CallableId, CallableInstance, Edge, FunctionSourceOrigin, OpId, Operand, OwnKind, Provenance,
    SemAbiParam, SemAggregateField, SemAggregateShape, SemBlock, SemCallConv, SemCallable,
    SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature,
    SemTerminator, SemVariant, SemVariantField, SemVariantShape, ValueDef, ValueId, VariantShapeId,
};
use hew_types::{
    BuiltinType, CloneKind, DefId, ResolvedTy, RuntimeCallFamily, SendFact, TypeFacts,
    TypeInstanceKey, ValueClass,
};

pub(super) fn utf8_error_ty(path: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: path.to_string(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    }
}

fn option_i64_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Option".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: Some(BuiltinType::Option),
        is_opaque: false,
    }
}

pub(super) fn decode_result_ty(error: ResolvedTy) -> ResolvedTy {
    ResolvedTy::Named {
        name: "Result".to_string(),
        args: vec![ResolvedTy::String, error],
        builtin: Some(BuiltinType::Result),
        is_opaque: false,
    }
}

pub(super) fn facts(class: ValueClass, clone: CloneKind) -> TypeFacts {
    TypeFacts {
        class,
        clone,
        send: SendFact::Known(true),
        hash: false,
        eq: false,
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "one complete SIR fixture keeps the runtime result, exact nested descriptors, and cleanup CFG auditable together"
)]
pub(super) fn decode_module() -> SemModule {
    let error_ty = utf8_error_ty("std.encoding.utf8.Utf8Error");
    let option_ty = option_i64_ty();
    let result_ty = decode_result_ty(error_ty.clone());
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("decode"),
        name: "decode".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::RootUnit,
        params: vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bytes,
            own: OwnKind::Guaranteed,
        }],
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::RtCall {
                    id: OpId(0),
                    family: RuntimeCallFamily::BytesDecodeUtf8,
                    args: vec![BoundaryOperand {
                        operand: Operand { value: ValueId(0) },
                        decision: BoundaryDecision::Borrow,
                    }],
                    result: CallResult::Value(ValueDef {
                        id: ValueId(1),
                        ty: result_ty.clone(),
                        own: OwnKind::Owned,
                    }),
                    normal: Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: ValueId(1) }],
                    },
                    unwind: CallUnwind::NotApplicable,
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(2),
                    ty: result_ty.clone(),
                    own: OwnKind::Owned,
                }],
                ops: vec![SemOp {
                    id: OpId(1),
                    results: Vec::new(),
                    kind: SemOpKind::DestroyValue {
                        value: Operand { value: ValueId(2) },
                    },
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return { value: None },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
    };
    let mut type_facts = BTreeMap::new();
    for (ty, row) in [
        (
            ResolvedTy::Bytes,
            facts(ValueClass::CowValue, CloneKind::Retain),
        ),
        (
            ResolvedTy::String,
            facts(ValueClass::CowValue, CloneKind::Retain),
        ),
        (ResolvedTy::I64, facts(ValueClass::BitCopy, CloneKind::Bits)),
        (
            option_ty.clone(),
            facts(ValueClass::BitCopy, CloneKind::Bits),
        ),
        (
            error_ty.clone(),
            facts(ValueClass::BitCopy, CloneKind::Bits),
        ),
        (
            result_ty.clone(),
            facts(ValueClass::CowValue, CloneKind::FieldWise),
        ),
    ] {
        type_facts.insert(TypeInstanceKey(ty), row);
    }
    SemModule {
        callables: vec![SemCallable {
            id: CallableId(0),
            function: ItemId(0),
            declaration: function.declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol: function.name.clone(),
            source_origin: function.source_origin.clone(),
            signature: SemSignature {
                params: vec![SemAbiParam {
                    ty: ResolvedTy::Bytes,
                    passing: SemParamPassing::Borrow,
                    caller_visible_projection: false,
                }],
                return_ty: ResolvedTy::Unit,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        }],
        generic_templates: Vec::new(),
        root_unit_callables: vec![CallableId(0)],
        entry_exit_plan: None,
        entry_callable: None,
        functions: vec![function],
        aggregate_shapes: vec![SemAggregateShape {
            id: AggregateShapeId(0),
            aggregate_ty: error_ty.clone(),
            instance: error_ty
                .nominal_instance()
                .expect("canonical source error must carry nominal identity"),
            fields: vec![
                SemAggregateField {
                    name: "valid_up_to".to_string(),
                    ty: ResolvedTy::I64,
                },
                SemAggregateField {
                    name: "error_len".to_string(),
                    ty: option_ty.clone(),
                },
            ],
        }],
        variant_shapes: vec![
            SemVariantShape {
                id: VariantShapeId(0),
                enum_ty: result_ty,
                is_indirect: false,
                variants: vec![
                    SemVariant {
                        name: "Ok".to_string(),
                        fields: vec![SemVariantField {
                            name: "0".to_string(),
                            ty: ResolvedTy::String,
                        }],
                    },
                    SemVariant {
                        name: "Err".to_string(),
                        fields: vec![SemVariantField {
                            name: "0".to_string(),
                            ty: error_ty,
                        }],
                    },
                ],
            },
            SemVariantShape {
                id: VariantShapeId(1),
                enum_ty: option_ty,
                is_indirect: false,
                variants: vec![
                    SemVariant {
                        name: "Some".to_string(),
                        fields: vec![SemVariantField {
                            name: "0".to_string(),
                            ty: ResolvedTy::I64,
                        }],
                    },
                    SemVariant {
                        name: "None".to_string(),
                        fields: Vec::new(),
                    },
                ],
            },
        ],
        type_facts,
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
    }
}
