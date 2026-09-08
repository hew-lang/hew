// Source-independent semantic inputs shared by physical and LLVM tests.

use std::collections::BTreeMap;

use hew_sir as sir;
use hew_types::{ResolvedTy, TypeFactContext, TypeFactService};
use sir::OwnKind::{None as Plain, Owned};

#[derive(Clone, Copy, Debug)]
pub enum Case {
    MixedReplacement,
    LiveReplacement,
    DeadReplacement,
    BranchReplacement,
    Permutation,
    Fault,
    ZeroSized,
}

pub fn inner_ty() -> ResolvedTy {
    ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::String])
}

pub fn root_ty() -> ResolvedTy {
    ResolvedTy::Tuple(vec![
        ResolvedTy::String,
        inner_ty(),
        ResolvedTy::Unit,
        ResolvedTy::I64,
    ])
}

fn operand(value: u32) -> sir::Operand {
    sir::Operand {
        value: sir::ValueId(value),
    }
}

fn result(id: u32, ty: ResolvedTy, own: sir::OwnKind) -> sir::ValueDef {
    sir::ValueDef {
        id: sir::ValueId(id),
        ty,
        own,
    }
}

fn op(id: u32, kind: sir::SemOpKind, results: Vec<sir::ValueDef>) -> sir::SemOp {
    sir::SemOp {
        id: sir::OpId(id),
        kind,
        results,
        provenance: sir::Provenance::Synthesized,
    }
}

fn take(id: u32, place: u32, value: u32, ty: ResolvedTy, own: sir::OwnKind) -> sir::SemOp {
    op(
        id,
        sir::SemOpKind::LoadTake {
            place: sir::PlaceId(place),
        },
        vec![result(value, ty, own)],
    )
}

fn drop_value(id: u32, value: u32) -> sir::SemOp {
    op(
        id,
        sir::SemOpKind::DestroyValue {
            value: operand(value),
        },
        vec![],
    )
}

fn assign(id: u32, place: u32, value: u32) -> sir::SemOp {
    op(
        id,
        sir::SemOpKind::StoreAssign {
            place: sir::PlaceId(place),
            value: operand(value),
        },
        vec![],
    )
}

fn edge(target: u32, values: &[u32]) -> sir::Edge {
    sir::Edge {
        target: sir::BlockId(target),
        args: values.iter().copied().map(operand).collect(),
    }
}

fn block(id: u32, ops: Vec<sir::SemOp>, terminator: sir::SemTerminator) -> sir::SemBlock {
    sir::SemBlock {
        id: sir::BlockId(id),
        args: vec![],
        ops,
        terminator,
    }
}

fn finish(id: u32, roots: &[u32], fault: bool) -> sir::SemBlock {
    let mut ops: Vec<_> = roots
        .iter()
        .enumerate()
        .map(|(index, root)| drop_value(1000 + id * 10 + u32::try_from(index).unwrap(), *root))
        .collect();
    let terminator = if fault {
        sir::SemTerminator::Trap {
            kind: sir::TrapKind::DivideByZero,
        }
    } else {
        ops.push(op(
            1005 + id * 10,
            sir::SemOpKind::ConstInteger(42),
            vec![result(200 + id, ResolvedTy::I64, sir::OwnKind::None)],
        ));
        sir::SemTerminator::Return {
            value: Some(sir::BoundaryOperand {
                operand: operand(200 + id),
                decision: sir::BoundaryDecision::Copy,
            }),
        }
    };
    block(id, ops, terminator)
}

fn root_args(values: &[u32]) -> Vec<sir::BlockArg> {
    values
        .iter()
        .map(|value| sir::BlockArg {
            value: sir::ValueId(*value),
            ty: root_ty(),
            own: sir::OwnKind::Owned,
        })
        .collect()
}

fn partition(root: u32, start: u32) -> Vec<sir::PlaceDecl> {
    [
        (None, 0, ResolvedTy::String),
        (None, 1, inner_ty()),
        (Some(sir::PlaceId(start + 1)), 0, ResolvedTy::String),
        (Some(sir::PlaceId(start + 1)), 1, ResolvedTy::String),
        (None, 2, ResolvedTy::Unit),
        (None, 3, ResolvedTy::I64),
    ]
    .into_iter()
    .enumerate()
    .map(|(index, (parent, field, ty))| sir::PlaceDecl {
        id: sir::PlaceId(start + u32::try_from(index).unwrap()),
        ty,
        origin: sir::PlaceOrigin::Aggregate {
            base: parent.map_or(
                sir::PlaceBase::Value(sir::ValueId(root)),
                sir::PlaceBase::Place,
            ),
            shape: sir::AggregateShapeRef::Tuple,
            field,
        },
    })
    .collect()
}

pub fn module(case: Case) -> sir::SemModule {
    let mut places = partition(0, 0);
    places.extend(partition(1, 6));
    let blocks = match case {
        Case::MixedReplacement | Case::LiveReplacement | Case::DeadReplacement => {
            replacement_blocks(case)
        }
        Case::BranchReplacement => branch_blocks(&mut places),
        Case::Permutation => permutation_blocks(&mut places),
        Case::Fault => fault_blocks(),
        Case::ZeroSized => zero_sized_blocks(),
    };
    let declaration = hew_types::DefId::for_test("partial_storage");
    let callable = sir::SemCallable {
        id: sir::CallableId(0),
        function: hew_hir::ItemId(0),
        declaration: declaration.clone(),
        instance: sir::CallableInstance::Monomorphic,
        symbol: "partial_storage".into(),
        source_origin: sir::FunctionSourceOrigin::Unknown,
        signature: sir::SemSignature {
            params: vec![
                sir::SemAbiParam {
                    ty: root_ty(),
                    passing: sir::SemParamPassing::Consume,
                    caller_visible_projection: false,
                },
                sir::SemAbiParam {
                    ty: root_ty(),
                    passing: sir::SemParamPassing::Consume,
                    caller_visible_projection: false,
                },
                sir::SemAbiParam {
                    ty: ResolvedTy::Bool,
                    passing: sir::SemParamPassing::ReadOnly,
                    caller_visible_projection: false,
                },
            ],
            return_ty: ResolvedTy::I64,
        },
        call_conv: sir::SemCallConv::Default,
        kind: sir::SemCallableKind::HewDirect,
    };
    let mut params = root_args(&[0, 1]);
    params.push(sir::BlockArg {
        value: sir::ValueId(2),
        ty: ResolvedTy::Bool,
        own: Plain,
    });
    let function = sir::SemFunction {
        id: callable.function,
        callable: callable.id,
        declaration,
        name: callable.symbol.clone(),
        source_origin: callable.source_origin.clone(),
        span: 0..0,
        params,
        return_ty: ResolvedTy::I64,
        entry: sir::BlockId(0),
        bindings: vec![],
        places,
        blocks,
    };
    let mut facts = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
    for ty in [
        root_ty(),
        ResolvedTy::Bool,
        ResolvedTy::I64,
        ResolvedTy::Unit,
    ] {
        facts.require(&ty).unwrap();
    }
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

/// Exercise the same partial writes through a function-owned Local allocation.
pub fn local_module(case: Case) -> sir::SemModule {
    assert!(matches!(
        case,
        Case::MixedReplacement | Case::LiveReplacement | Case::DeadReplacement | Case::Fault
    ));
    let mut module = module(case);
    let function = &mut module.functions[0];
    for place in &mut function.places {
        if let sir::PlaceOrigin::Aggregate { base, .. } = &mut place.origin {
            if *base == sir::PlaceBase::Value(sir::ValueId(0)) {
                *base = sir::PlaceBase::Place(sir::PlaceId(24));
            }
        }
    }
    function.places.push(sir::PlaceDecl {
        id: sir::PlaceId(24),
        ty: root_ty(),
        origin: sir::PlaceOrigin::Local,
    });
    for block in &mut function.blocks {
        for operation in &mut block.ops {
            if matches!(&operation.kind, sir::SemOpKind::DestroyValue { value } if value.value == sir::ValueId(0))
            {
                operation.kind = sir::SemOpKind::EndLifetime {
                    place: sir::PlaceId(24),
                };
            }
        }
    }
    function.blocks[0].ops.splice(
        0..0,
        [
            op(
                2000,
                sir::SemOpKind::AllocPlace {
                    place: sir::PlaceId(24),
                },
                vec![],
            ),
            op(
                2001,
                sir::SemOpKind::StoreInit {
                    place: sir::PlaceId(24),
                    value: operand(0),
                },
                vec![],
            ),
        ],
    );
    module
}

fn replacement_blocks(case: Case) -> Vec<sir::SemBlock> {
    let mut ops = match case {
        Case::MixedReplacement => {
            vec![take(0, 2, 3, ResolvedTy::String, Owned), drop_value(1, 3)]
        }
        Case::DeadReplacement => vec![take(0, 1, 3, inner_ty(), Owned), drop_value(1, 3)],
        _ => vec![],
    };
    ops.extend([take(2, 7, 4, inner_ty(), Owned), assign(3, 1, 4)]);
    let mut done = finish(0, &[0, 1], false);
    ops.append(&mut done.ops);
    done.ops = ops;
    vec![done]
}

fn branch_blocks(places: &mut Vec<sir::PlaceDecl>) -> Vec<sir::SemBlock> {
    places.extend(partition(100, 12));
    places.extend(partition(101, 18));
    let mut done = finish(3, &[100, 101], false);
    done.args = root_args(&[100, 101]);
    let mut ops = vec![take(2, 19, 4, inner_ty(), Owned), assign(3, 13, 4)];
    ops.append(&mut done.ops);
    done.ops = ops;
    vec![
        block(
            0,
            vec![],
            sir::SemTerminator::Branch {
                condition: operand(2),
                then_target: edge(1, &[]),
                else_target: edge(2, &[]),
            },
        ),
        block(
            1,
            vec![take(0, 2, 3, ResolvedTy::String, Owned), drop_value(1, 3)],
            sir::SemTerminator::Goto(edge(3, &[0, 1])),
        ),
        block(2, vec![], sir::SemTerminator::Goto(edge(3, &[0, 1]))),
        done,
    ]
}

fn permutation_blocks(places: &mut Vec<sir::PlaceDecl>) -> Vec<sir::SemBlock> {
    places.extend(partition(100, 12));
    places.extend(partition(101, 18));
    let mut header = block(
        1,
        vec![],
        sir::SemTerminator::Branch {
            condition: operand(102),
            then_target: edge(2, &[]),
            else_target: edge(3, &[]),
        },
    );
    header.args = root_args(&[100, 101]);
    header.args.push(sir::BlockArg {
        value: sir::ValueId(102),
        ty: ResolvedTy::Bool,
        own: Plain,
    });
    vec![
        block(
            0,
            vec![
                take(0, 2, 3, ResolvedTy::String, Owned),
                drop_value(1, 3),
                take(2, 6, 4, ResolvedTy::String, Owned),
                drop_value(3, 4),
            ],
            sir::SemTerminator::Goto(edge(1, &[0, 1, 2])),
        ),
        header,
        block(
            2,
            vec![op(
                4,
                sir::SemOpKind::ConstBool(false),
                vec![result(104, ResolvedTy::Bool, Plain)],
            )],
            sir::SemTerminator::Goto(edge(1, &[101, 100, 104])),
        ),
        finish(3, &[100, 101], false),
    ]
}

fn fault_blocks() -> Vec<sir::SemBlock> {
    vec![
        block(
            0,
            vec![
                take(0, 2, 3, ResolvedTy::String, Owned),
                drop_value(1, 3),
                take(2, 5, 4, ResolvedTy::I64, Plain),
                op(
                    3,
                    sir::SemOpKind::ConstInteger(1),
                    vec![result(5, ResolvedTy::I64, Plain)],
                ),
            ],
            sir::SemTerminator::CheckedBinary {
                id: sir::OpId(4),
                op: hew_parser::ast::BinaryOp::Divide,
                lhs: operand(5),
                rhs: operand(4),
                result: result(6, ResolvedTy::I64, Plain),
                normal: edge(1, &[6]),
                failures: vec![
                    sir::CheckedFailure {
                        kind: sir::TrapKind::DivideByZero,
                        edge: edge(2, &[]),
                    },
                    sir::CheckedFailure {
                        kind: sir::TrapKind::SignedMinDivNegOne,
                        edge: edge(3, &[]),
                    },
                ],
            },
        ),
        {
            let mut normal = finish(1, &[0, 1], false);
            normal.args.push(sir::BlockArg {
                value: sir::ValueId(7),
                ty: ResolvedTy::I64,
                own: Plain,
            });
            normal
        },
        finish(2, &[0, 1], true),
        {
            let mut failure = finish(3, &[0, 1], true);
            failure.terminator = sir::SemTerminator::Trap {
                kind: sir::TrapKind::SignedMinDivNegOne,
            };
            failure
        },
    ]
}

fn zero_sized_blocks() -> Vec<sir::SemBlock> {
    let mut done = finish(0, &[4, 1], false);
    let mut ops = vec![
        take(0, 4, 3, ResolvedTy::Unit, Plain),
        op(
            1,
            sir::SemOpKind::StoreInit {
                place: sir::PlaceId(4),
                value: operand(3),
            },
            vec![],
        ),
        op(
            2,
            sir::SemOpKind::Move { source: operand(0) },
            vec![result(4, root_ty(), Owned)],
        ),
    ];
    ops.append(&mut done.ops);
    done.ops = ops;
    vec![done]
}
