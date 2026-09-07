//! Semantic encoding bodies independent of source wrapper admission.

use hew_sir as sir;
use hew_types::runtime_call::EncodingFormat;
use hew_types::{
    ResolvedTy, RuntimeArgumentEffect, RuntimeCallFamily, TypeFactContext, TypeFactService,
};
use std::collections::BTreeMap;

pub fn value(format: EncodingFormat) -> ResolvedTy {
    ResolvedTy::Named {
        name: format.builtin().canonical_name().to_string(),
        args: vec![],
        builtin: Some(format.builtin()),
        is_opaque: true,
    }
}
pub fn operand(id: u32) -> sir::Operand {
    sir::Operand {
        value: sir::ValueId(id),
    }
}
pub fn op(id: u32, kind: sir::SemOpKind, results: Vec<sir::ValueDef>) -> sir::SemOp {
    sir::SemOp {
        id: sir::OpId(id),
        kind,
        results,
        provenance: sir::Provenance::Synthesized,
    }
}
pub fn result(id: u32, ty: ResolvedTy, own: sir::OwnKind) -> sir::ValueDef {
    sir::ValueDef {
        id: sir::ValueId(id),
        ty,
        own,
    }
}

pub fn skeleton(params: Vec<ResolvedTy>, return_ty: ResolvedTy) -> sir::SemModule {
    let mut facts = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
    for ty in params.iter().chain([&return_ty]) {
        facts.require(ty).unwrap();
    }
    let own = |ty: &ResolvedTy| {
        sir::OwnKind::of_class(facts.rows()[&hew_types::TypeInstanceKey(ty.clone())].class)
    };
    let declaration = hew_types::DefId::for_test("encoding_probe");
    let callable = sir::SemCallable {
        id: sir::CallableId(0),
        function: hew_hir::ItemId(0),
        declaration: declaration.clone(),
        instance: sir::CallableInstance::Monomorphic,
        symbol: "encoding_probe".into(),
        source_origin: sir::FunctionSourceOrigin::Unknown,
        signature: sir::SemSignature {
            params: params
                .iter()
                .map(|ty| sir::SemAbiParam {
                    ty: ty.clone(),
                    passing: if own(ty) == sir::OwnKind::Owned {
                        sir::SemParamPassing::Consume
                    } else {
                        sir::SemParamPassing::ReadOnly
                    },
                    caller_visible_projection: false,
                })
                .collect(),
            return_ty: return_ty.clone(),
        },
        call_conv: sir::SemCallConv::Default,
        kind: sir::SemCallableKind::HewDirect,
    };
    let function = sir::SemFunction {
        id: callable.function,
        callable: callable.id,
        declaration,
        name: callable.symbol.clone(),
        source_origin: callable.source_origin.clone(),
        span: 0..0,
        params: params
            .into_iter()
            .enumerate()
            .map(|(index, ty)| sir::BlockArg {
                value: sir::ValueId(u32::try_from(index).unwrap()),
                own: own(&ty),
                ty,
            })
            .collect(),
        return_ty,
        entry: sir::BlockId(0),
        bindings: vec![],
        places: vec![],
        blocks: vec![],
    };
    sir::SemModule {
        actors: Vec::new(),
        supervisors: Vec::new(),
        resources: BTreeMap::new(),
        closures: vec![],
        vtables: vec![],
        value_capabilities: BTreeMap::new(),
        callables: vec![callable],
        generic_templates: vec![],
        root_unit_callables: vec![],
        entry_exit_plan: None,
        entry_callable: None,
        functions: vec![function],
        aggregate_shapes: vec![],
        variant_shapes: vec![],
        type_facts: facts.rows().clone(),
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
    }
}

pub fn operation(family: RuntimeCallFamily) -> sir::SemModule {
    let owner = value(family.encoding_format().unwrap());
    let contract = family.semantic_contract().unwrap();
    let params = contract
        .arguments
        .iter()
        .map(|arg| arg.ty.resolve(Some(&owner)).unwrap())
        .collect::<Vec<_>>();
    let signature = contract.instantiate(&params, &owner).unwrap();
    let mut module = skeleton(params, signature.result_ty.clone());
    let own = sir::OwnKind::of_class(
        module.type_facts[&hew_types::TypeInstanceKey(signature.result_ty.clone())].class,
    );
    let function = &mut module.functions[0];
    let has_result = signature.result_ty != ResolvedTy::Unit;
    let cleanup = function
        .params
        .iter()
        .zip(contract.arguments)
        .filter(|(param, arg)| {
            param.own == sir::OwnKind::Owned && arg.effect != RuntimeArgumentEffect::Move
        })
        .enumerate()
        .map(|(index, (param, _))| {
            op(
                u32::try_from(index + 1).unwrap(),
                sir::SemOpKind::DestroyValue {
                    value: operand(param.value.0),
                },
                vec![],
            )
        })
        .collect();
    function.blocks = vec![
        sir::SemBlock {
            id: sir::BlockId(0),
            args: vec![],
            ops: vec![],
            terminator: sir::SemTerminator::RtCall {
                id: sir::OpId(0),
                family,
                args: function
                    .params
                    .iter()
                    .zip(contract.arguments)
                    .map(|(param, arg)| sir::BoundaryOperand {
                        operand: operand(param.value.0),
                        decision: match arg.effect {
                            RuntimeArgumentEffect::Value => {
                                unreachable!("encoding contracts have fixed transfers")
                            }
                            RuntimeArgumentEffect::Borrow => sir::BoundaryDecision::Borrow,
                            RuntimeArgumentEffect::Copy => sir::BoundaryDecision::Copy,
                            RuntimeArgumentEffect::Move => sir::BoundaryDecision::Move,
                        },
                    })
                    .collect(),
                result: if has_result {
                    sir::CallResult::Value(result(100, signature.result_ty.clone(), own))
                } else {
                    sir::CallResult::Unit
                },
                normal: sir::Edge {
                    target: sir::BlockId(1),
                    args: if has_result {
                        vec![operand(100)]
                    } else {
                        vec![]
                    },
                },
                unwind: sir::CallUnwind::NotApplicable,
            },
        },
        sir::SemBlock {
            id: sir::BlockId(1),
            args: if has_result {
                vec![sir::BlockArg {
                    value: sir::ValueId(101),
                    ty: signature.result_ty,
                    own,
                }]
            } else {
                vec![]
            },
            ops: cleanup,
            terminator: sir::SemTerminator::Return {
                value: has_result.then(|| sir::BoundaryOperand {
                    operand: operand(101),
                    decision: if own == sir::OwnKind::Owned {
                        sir::BoundaryDecision::Move
                    } else {
                        sir::BoundaryDecision::Copy
                    },
                }),
            },
        },
    ];
    sir::check_module(&module).unwrap();
    module
}

pub fn copy(format: EncodingFormat) -> sir::SemModule {
    let ty = value(format);
    let mut module = skeleton(vec![ty.clone()], ty.clone());
    module.callables[0].signature.params[0].passing = sir::SemParamPassing::Borrow;
    let function = &mut module.functions[0];
    function.params[0].own = sir::OwnKind::Guaranteed;
    function.blocks = vec![sir::SemBlock {
        id: sir::BlockId(0),
        args: vec![],
        ops: vec![op(
            0,
            sir::SemOpKind::CopyValue { source: operand(0) },
            vec![result(1, ty, sir::OwnKind::Owned)],
        )],
        terminator: sir::SemTerminator::Return {
            value: Some(sir::BoundaryOperand {
                operand: operand(1),
                decision: sir::BoundaryDecision::Move,
            }),
        },
    }];
    sir::check_module(&module).unwrap();
    module
}

pub fn local_copy(format: EncodingFormat) -> sir::SemModule {
    let ty = value(format);
    let mut module = skeleton(vec![ty.clone()], ty.clone());
    let function = &mut module.functions[0];
    function.places = vec![sir::PlaceDecl {
        id: sir::PlaceId(0),
        ty: ty.clone(),
        origin: sir::PlaceOrigin::Local,
    }];
    function.blocks = vec![sir::SemBlock {
        id: sir::BlockId(0),
        args: vec![],
        ops: vec![
            op(
                0,
                sir::SemOpKind::AllocPlace {
                    place: sir::PlaceId(0),
                },
                vec![],
            ),
            op(
                1,
                sir::SemOpKind::StoreInit {
                    place: sir::PlaceId(0),
                    value: operand(0),
                },
                vec![],
            ),
            op(
                2,
                sir::SemOpKind::LoadCopy {
                    place: sir::PlaceId(0),
                },
                vec![result(1, ty, sir::OwnKind::Owned)],
            ),
            op(
                3,
                sir::SemOpKind::EndLifetime {
                    place: sir::PlaceId(0),
                },
                vec![],
            ),
        ],
        terminator: sir::SemTerminator::Return {
            value: Some(sir::BoundaryOperand {
                operand: operand(1),
                decision: sir::BoundaryDecision::Move,
            }),
        },
    }];
    sir::check_module(&module).unwrap();
    module
}
