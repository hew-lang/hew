use hew_hir::ItemId;
use hew_sir::{
    dump_sir, Binding, BindingId, BindingTarget, BlockArg, BlockId, BoundaryDecision,
    BoundaryOperand, CallableId, Edge, FunctionSourceOrigin, OpId, Operand, OperandSlot, OwnKind,
    PlaceDecl, PlaceId, Provenance, SemBlock, SemFunction, SemModule, SemOp, SemOpKind,
    SemTerminator, SnapshotDecision, SuspendKind, ValueId,
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
fn every_boundary_decision_is_visible_to_dump_and_model_visitors() {
    let value = ValueId(7);
    let mut function = function(Vec::new(), Vec::new());
    function.blocks[0].ops.push(SemOp {
        id: OpId(0),
        results: Vec::new(),
        kind: SemOpKind::Call {
            callee: CallableId(0),
            args: vec![
                boundary(value, BoundaryDecision::Borrow),
                boundary(value, BoundaryDecision::Copy),
                boundary(value, BoundaryDecision::Move),
            ],
        },
        provenance: Provenance::Synthesized,
    });
    function.blocks[0].terminator = SemTerminator::Suspend {
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
    function.blocks[0].ops[0]
        .visit_boundary_operands(|slot, input| visited.push((slot, input.decision)));
    function.blocks[0]
        .terminator
        .visit_boundary_operands(|slot, input| visited.push((slot, input.decision)));
    assert_eq!(
        visited,
        vec![
            (OperandSlot(0), BoundaryDecision::Borrow),
            (OperandSlot(1), BoundaryDecision::Copy),
            (OperandSlot(2), BoundaryDecision::Move),
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

    let dump = dump_sir(&SemModule {
        functions: vec![function],
        ..SemModule::default()
    });
    for rendered in [
        "borrow %7",
        "copy %7",
        "move %7",
        "snapshot.share %7",
        "snapshot.deep_copy %7",
        "snapshot.transfer %7",
    ] {
        assert!(dump.contains(rendered), "missing `{rendered}` in:\n{dump}");
    }
}

#[test]
fn operand_remains_value_only() {
    let operand = Operand { value: ValueId(9) };
    let Operand { value } = operand;

    assert_eq!(value, ValueId(9));
}
