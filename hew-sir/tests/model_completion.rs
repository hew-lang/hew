use hew_hir::ItemId;
use hew_sir::{
    Binding, BindingId, BindingTarget, BlockArg, BlockId, CallableId, FunctionSourceOrigin,
    OwnKind, PlaceDecl, PlaceId, SemBlock, SemFunction, SemTerminator, ValueId,
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
