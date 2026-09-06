use hew_hir::ItemId;
use hew_sir::{
    Binding, BindingId, BindingTarget, BlockArg, BlockId, BoundaryDecision, BoundaryOperand,
    CallResult, CallUnwind, CallableId, Edge, FunctionSourceOrigin, Operand, OperandSlot, OwnKind,
    PlaceDecl, PlaceId, SemBlock, SemFunction, SemTerminator, SemVariantArm, SnapshotDecision,
    SuccessorSlot, SuspendKind, ValueDef, ValueId, VariantShapeId,
};
use hew_types::{DefId, ResolvedTy};

fn function(bindings: Vec<Binding>, places: Vec<PlaceDecl>) -> SemFunction {
    SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("model_completion"),
        name: "model_completion".to_string(),
        span: 0..20,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![BlockArg {
            value: ValueId(7),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        }],
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Return { value: None },
        }],
        places,
        bindings,
    }
}

#[test]
fn two_aliases_can_target_one_value() {
    let value = ValueId(7);
    let function = function(
        vec![
            Binding {
                id: BindingId(0),
                name: "source".to_string(),
                span: 1..7,
                mutable: false,
                target: BindingTarget::Value(value),
            },
            Binding {
                id: BindingId(1),
                name: "alias".to_string(),
                span: 9..14,
                mutable: false,
                target: BindingTarget::Value(value),
            },
        ],
        Vec::new(),
    );

    let aliases = function
        .bindings
        .iter()
        .map(|binding| (binding.id, binding.target))
        .collect::<Vec<_>>();
    assert_eq!(
        aliases,
        vec![
            (BindingId(0), BindingTarget::Value(value)),
            (BindingId(1), BindingTarget::Value(value)),
        ]
    );
    assert_eq!(
        function
            .binding_naming(value)
            .map(|binding| binding.name.as_str()),
        Some("alias")
    );
}

#[test]
fn mutable_source_binding_can_target_a_place() {
    let place = PlaceId(3);
    let function = function(
        vec![Binding {
            id: BindingId(0),
            name: "counter".to_string(),
            span: 4..11,
            mutable: true,
            target: BindingTarget::Place(place),
        }],
        vec![PlaceDecl {
            id: place,
            ty: ResolvedTy::I64,
            runtime_owned: false,
        }],
    );

    let binding = function
        .binding_rooting(place)
        .expect("the place must retain its source binding provenance");
    assert_eq!(binding.id, BindingId(0));
    assert_eq!(binding.name, "counter");
    assert_eq!(binding.span, 4..11);
    assert!(binding.mutable);
    assert!(function.binding_rooting(PlaceId(4)).is_none());
}

fn boundary(value: ValueId, decision: BoundaryDecision) -> BoundaryOperand {
    BoundaryOperand {
        operand: Operand { value },
        decision,
    }
}

#[test]
fn invoke_value_result_flows_to_the_normal_block_and_visits_both_cfg_edges() {
    let value = ValueId(7);
    let mut function = function(Vec::new(), Vec::new());
    function.return_ty = ResolvedTy::I64;
    function.blocks = vec![
        SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Call {
                id: hew_sir::OpId(0),
                callee: CallableId(0),
                args: vec![
                    boundary(value, BoundaryDecision::Borrow),
                    boundary(value, BoundaryDecision::Copy),
                    boundary(value, BoundaryDecision::Move),
                ],
                result: CallResult::Value(ValueDef {
                    id: ValueId(8),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }),
                normal: Edge {
                    target: BlockId(1),
                    args: vec![Operand { value: ValueId(8) }],
                },
                unwind: CallUnwind::Cleanup(Edge {
                    target: BlockId(2),
                    args: Vec::new(),
                }),
            },
        },
        SemBlock {
            id: BlockId(1),
            args: vec![BlockArg {
                value: ValueId(9),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            }],
            ops: Vec::new(),
            terminator: SemTerminator::Return {
                value: Some(boundary(ValueId(9), BoundaryDecision::Move)),
            },
        },
        SemBlock {
            id: BlockId(2),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::ResumeUnwind,
        },
    ];

    let call = &function.blocks[0].terminator;
    let mut boundaries = Vec::new();
    call.visit_boundary_operands(|slot, input| boundaries.push((slot, input.decision)));
    assert_eq!(
        boundaries,
        vec![
            (OperandSlot(0), BoundaryDecision::Borrow),
            (OperandSlot(1), BoundaryDecision::Copy),
            (OperandSlot(2), BoundaryDecision::Move),
        ]
    );

    let mut results = Vec::new();
    call.visit_results(|result| results.push(result.id));
    assert_eq!(results, vec![ValueId(8)]);

    let mut successors = Vec::new();
    call.visit_successors_with_slots(|slot, edge| {
        successors.push((slot, edge.target, edge.args.clone()));
    });
    assert_eq!(
        successors,
        vec![
            (
                SuccessorSlot(0),
                BlockId(1),
                vec![Operand { value: ValueId(8) }],
            ),
            (SuccessorSlot(1), BlockId(2), Vec::new()),
        ]
    );
    assert_eq!(function.blocks[1].args[0].value, ValueId(9));
}

#[test]
fn consuming_variant_switch_exposes_each_arms_fields_and_edge() {
    let switch = SemTerminator::SwitchVariant {
        id: hew_sir::OpId(4),
        shape: VariantShapeId(2),
        scrutinee: Operand { value: ValueId(7) },
        arms: vec![
            SemVariantArm {
                variant: 0,
                fields: vec![ValueDef {
                    id: ValueId(8),
                    ty: ResolvedTy::String,
                    own: OwnKind::Owned,
                }],
                target: Edge {
                    target: BlockId(1),
                    args: vec![Operand { value: ValueId(8) }],
                },
            },
            SemVariantArm {
                variant: 1,
                fields: Vec::new(),
                target: Edge {
                    target: BlockId(2),
                    args: Vec::new(),
                },
            },
        ],
    };

    let mut results = Vec::new();
    switch.visit_results(|result| results.push(result.id));
    assert_eq!(results, [ValueId(8)]);

    let mut operands = Vec::new();
    switch.visit_operands(|slot, operand| operands.push((slot, operand.value)));
    assert_eq!(
        operands,
        [(OperandSlot(0), ValueId(7)), (OperandSlot(1), ValueId(8)),]
    );

    let mut successors = Vec::new();
    switch.visit_successors_with_slots(|slot, edge| {
        successors.push((slot, edge.target, edge.args.clone()));
    });
    assert_eq!(
        successors,
        [
            (
                SuccessorSlot(0),
                BlockId(1),
                vec![Operand { value: ValueId(8) }],
            ),
            (SuccessorSlot(1), BlockId(2), Vec::new()),
        ]
    );
}

#[test]
fn no_unwind_call_visits_only_its_normal_cfg_edge() {
    let call = SemTerminator::Call {
        id: hew_sir::OpId(0),
        callee: CallableId(0),
        args: Vec::new(),
        result: CallResult::Unit,
        normal: Edge {
            target: BlockId(1),
            args: Vec::new(),
        },
        unwind: CallUnwind::NotApplicable,
    };

    let mut successors = Vec::new();
    call.visit_successors_with_slots(|slot, edge| successors.push((slot, edge.target)));
    assert_eq!(successors, vec![(SuccessorSlot(0), BlockId(1))]);
}

#[test]
fn return_value_carries_a_boundary_decision_and_unit_is_explicit() {
    let value_return = SemTerminator::Return {
        value: Some(boundary(ValueId(7), BoundaryDecision::Move)),
    };
    let mut boundaries = Vec::new();
    value_return.visit_boundary_operands(|slot, input| boundaries.push((slot, input.clone())));
    assert_eq!(
        boundaries,
        vec![(OperandSlot(0), boundary(ValueId(7), BoundaryDecision::Move),)]
    );

    let unit_return = SemTerminator::Return { value: None };
    let mut unit_boundaries = Vec::new();
    unit_return.visit_boundary_operands(|slot, input| unit_boundaries.push((slot, input.clone())));
    assert!(unit_boundaries.is_empty());
}

#[test]
fn snapshot_boundary_decisions_are_visible_to_the_terminator_visitor() {
    let value = ValueId(7);
    let terminator = SemTerminator::Suspend {
        kind: SuspendKind::ActorSend,
        inputs: vec![
            boundary(value, BoundaryDecision::Snapshot(SnapshotDecision::Share)),
            boundary(
                value,
                BoundaryDecision::Snapshot(SnapshotDecision::DeepCopy),
            ),
            boundary(
                value,
                BoundaryDecision::Snapshot(SnapshotDecision::Transfer),
            ),
        ],
        resumes: Vec::new(),
        cancel: Edge {
            target: BlockId(0),
            args: Vec::new(),
        },
    };

    let mut visited = Vec::new();
    terminator.visit_boundary_operands(|slot, input| visited.push((slot, input.decision)));
    assert_eq!(
        visited,
        vec![
            (
                OperandSlot(0),
                BoundaryDecision::Snapshot(SnapshotDecision::Share),
            ),
            (
                OperandSlot(1),
                BoundaryDecision::Snapshot(SnapshotDecision::DeepCopy),
            ),
            (
                OperandSlot(2),
                BoundaryDecision::Snapshot(SnapshotDecision::Transfer),
            ),
        ]
    );
}

#[test]
fn indirect_callee_rewrites_without_shifting_arguments_or_result_edges() {
    let mut call = SemTerminator::IndirectCall {
        id: hew_sir::OpId(4),
        callee: boundary(ValueId(9), BoundaryDecision::Borrow),
        signature: hew_sir::SemSignature {
            params: vec![hew_sir::SemAbiParam {
                ty: ResolvedTy::I64,
                passing: hew_sir::SemParamPassing::ReadOnly,
                caller_visible_projection: false,
            }],
            return_ty: ResolvedTy::I64,
        },
        args: vec![boundary(ValueId(7), BoundaryDecision::Copy)],
        result: CallResult::Value(ValueDef {
            id: ValueId(8),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        }),
        normal: Edge {
            target: BlockId(1),
            args: vec![Operand { value: ValueId(8) }],
        },
        unwind: CallUnwind::Cleanup(Edge {
            target: BlockId(2),
            args: vec![Operand { value: ValueId(6) }],
        }),
    };
    let mut uses = Vec::new();
    call.visit_operands(|slot, operand| uses.push((slot.0, operand.value.0)));
    assert_eq!(uses, [(0, 9), (1, 7), (2, 8), (3, 6)]);
    assert_eq!(call.operand_context(OperandSlot(0)), "indirect callee");
    assert_eq!(call.operand_context(OperandSlot(1)), "call argument");
    assert_eq!(
        call.operand_context(OperandSlot(2)),
        "call normal-edge argument"
    );
    assert_eq!(
        call.operand_context(OperandSlot(3)),
        "call unwind-edge argument"
    );
    call.visit_operands_mut(|slot, operand| {
        if slot == OperandSlot(0) {
            operand.value = ValueId(19);
        }
    });
    let mut boundaries = Vec::new();
    call.visit_boundary_operands(|slot, operand| {
        boundaries.push((slot.0, operand.operand.value.0, operand.decision));
    });
    assert_eq!(
        boundaries,
        [
            (0, 19, BoundaryDecision::Borrow),
            (1, 7, BoundaryDecision::Copy)
        ]
    );
    let mut results = Vec::new();
    call.visit_results(|result| results.push(result.id));
    assert_eq!(results, [ValueId(8)]);
    assert_eq!(
        call.successor(SuccessorSlot(0)).unwrap().args[0].value,
        ValueId(8)
    );
    assert_eq!(
        call.successor(SuccessorSlot(1)).unwrap().args[0].value,
        ValueId(6)
    );
}
