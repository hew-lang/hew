//! Reduced SIR contracts, independent of source partial-move production.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, place_plan, verify_module, AggregateShapeRef, BlockArg, BlockId,
    BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, Edge, OpId, Operand, OwnKind,
    OwnerRoot, PlaceBase, PlaceDecl, PlaceId, PlaceOrigin, Provenance, SemBlock, SemFunction,
    SemModule, SemOp, SemOpKind, SemTerminator, SirDiagnosticKind, SirLoweringStatus, ValueDef,
    ValueId,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn fixture() -> SemModule {
    fixture_source(
        r#"
        fn probe(consume value: (string, (string, bytes)), flag: bool) {}
        fn main() { probe(("outer", ("inner", b"payload")), true); }
    "#,
    )
}

fn fixture_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &checked);
    assert!(
        lowered
            .statuses
            .iter()
            .any(|status| status.name == "probe"
                && matches!(status.status, SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.statuses
    );
    assert_valid(&lowered.module);
    let mut module = lowered.module;
    let function = probe(&mut module);
    assert_eq!(function.params[0].value, ValueId(0));
    assert_eq!(function.params[1].value, ValueId(1));
    function.places = partition(0, 0);
    function.blocks = vec![block(0, vec![destroy(0, 0)], done())];
    module
}

fn probe(module: &mut SemModule) -> &mut SemFunction {
    module
        .functions
        .iter_mut()
        .find(|body| body.declaration.full_path() == "probe")
        .unwrap()
}

fn root_ty() -> ResolvedTy {
    ResolvedTy::Tuple(vec![ResolvedTy::String, inner_ty()])
}

fn inner_ty() -> ResolvedTy {
    ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::Bytes])
}

fn partition(root: u32, start: u32) -> Vec<PlaceDecl> {
    [
        (None, 0, ResolvedTy::String),
        (None, 1, inner_ty()),
        (Some(PlaceId(start + 1)), 0, ResolvedTy::String),
        (Some(PlaceId(start + 1)), 1, ResolvedTy::Bytes),
    ]
    .into_iter()
    .enumerate()
    .map(|(index, (parent, field, ty))| PlaceDecl {
        id: PlaceId(start + u32::try_from(index).unwrap()),
        ty,
        origin: PlaceOrigin::Aggregate {
            base: parent.map_or(PlaceBase::Value(ValueId(root)), PlaceBase::Place),
            shape: AggregateShapeRef::Tuple,
            field,
        },
    })
    .collect()
}

fn operand(value: u32) -> Operand {
    Operand {
        value: ValueId(value),
    }
}

fn owned(value: u32, ty: ResolvedTy) -> ValueDef {
    ValueDef {
        id: ValueId(value),
        ty,
        own: OwnKind::Owned,
    }
}

fn op(id: u32, kind: SemOpKind, results: Vec<ValueDef>) -> SemOp {
    SemOp {
        id: OpId(id),
        kind,
        results,
        provenance: Provenance::Synthesized,
    }
}

fn take(id: u32, place: u32, value: u32, ty: ResolvedTy) -> SemOp {
    op(
        id,
        SemOpKind::LoadTake {
            place: PlaceId(place),
        },
        vec![owned(value, ty)],
    )
}

fn copy(id: u32, place: u32, value: u32, ty: ResolvedTy) -> SemOp {
    op(
        id,
        SemOpKind::LoadCopy {
            place: PlaceId(place),
        },
        vec![owned(value, ty)],
    )
}

fn assign(id: u32, place: u32, value: u32) -> SemOp {
    op(
        id,
        SemOpKind::StoreAssign {
            place: PlaceId(place),
            value: operand(value),
        },
        vec![],
    )
}

fn destroy(id: u32, value: u32) -> SemOp {
    op(
        id,
        SemOpKind::DestroyValue {
            value: operand(value),
        },
        vec![],
    )
}

fn done() -> SemTerminator {
    SemTerminator::Return { value: None }
}

fn edge(target: u32, root: Option<u32>) -> Edge {
    Edge {
        target: BlockId(target),
        args: root.into_iter().map(operand).collect(),
    }
}

fn block(id: u32, ops: Vec<SemOp>, terminator: SemTerminator) -> SemBlock {
    SemBlock {
        id: BlockId(id),
        args: vec![],
        ops,
        terminator,
    }
}

fn root_block(id: u32, root: u32, ops: Vec<SemOp>, terminator: SemTerminator) -> SemBlock {
    let mut block = block(id, ops, terminator);
    block.args.push(BlockArg {
        value: ValueId(root),
        ty: root_ty(),
        own: OwnKind::Owned,
    });
    block
}

fn assert_valid(module: &SemModule) {
    let diagnostics = verify_module(module);
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");
}

fn assert_lifetime(module: &SemModule, expected: &str) {
    let diagnostics = verify_module(module);
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic.kind,
        SirDiagnosticKind::OwnershipLifetime { reason, .. } if reason == expected)),
        "{diagnostics:#?}"
    );
}

#[test]
fn nested_take_preserves_siblings_and_one_root_cleanup() {
    let mut module = fixture();
    probe(&mut module).blocks[0].ops = vec![
        take(0, 2, 2, ResolvedTy::String),
        destroy(1, 2),
        copy(2, 0, 3, ResolvedTy::String),
        destroy(3, 3),
        copy(4, 3, 4, ResolvedTy::Bytes),
        destroy(5, 4),
        destroy(6, 0),
    ];
    assert_valid(&module);
    let function = probe(&mut module).clone();
    let plan = place_plan(&function, &module.aggregate_shapes, &module.type_facts).unwrap();
    assert_eq!(
        plan.leaves(OwnerRoot::Value(ValueId(0))).unwrap(),
        [PlaceId(0), PlaceId(2), PlaceId(3)]
    );
    let nested = plan.projection(PlaceId(2)).unwrap();
    assert_eq!(
        nested
            .path
            .iter()
            .map(|step| step.field)
            .collect::<Vec<_>>(),
        [1, 0]
    );
    assert_eq!(nested.recipe.ty, ResolvedTy::String);
    assert_eq!(
        plan.projection(PlaceId(1)).unwrap().leaves,
        [PlaceId(2), PlaceId(3)]
    );

    probe(&mut module).blocks[0]
        .ops
        .insert(2, take(7, 2, 5, ResolvedTy::String));
    probe(&mut module).blocks[0].ops.insert(3, destroy(8, 5));
    assert_lifetime(
        &module,
        "aggregate field is not initialized on every incoming path",
    );
}

#[test]
fn replacing_nested_leaves_restores_a_taken_parent() {
    let mut module = fixture();
    probe(&mut module).blocks[0].ops = vec![
        copy(0, 0, 2, ResolvedTy::String),
        copy(1, 3, 3, ResolvedTy::Bytes),
        take(2, 1, 4, inner_ty()),
        destroy(3, 4),
        assign(4, 2, 2),
        assign(5, 3, 3),
        op(
            6,
            SemOpKind::CopyValue { source: operand(0) },
            vec![owned(5, root_ty())],
        ),
        destroy(7, 5),
        destroy(8, 0),
    ];
    assert_valid(&module);
    let assignment = probe(&mut module).blocks[0].ops.remove(5);
    probe(&mut module).blocks[0].ops.insert(6, assignment);
    assert_lifetime(
        &module,
        "partially consumed aggregate cannot be copied, borrowed, invoked or transferred",
    );
}

fn joined_fixture() -> SemModule {
    let mut module = fixture();
    let function = probe(&mut module);
    for (root, start) in [(2, 4), (3, 8), (4, 12)] {
        function.places.extend(partition(root, start));
    }
    function.blocks = vec![
        block(
            0,
            vec![],
            SemTerminator::Branch {
                condition: operand(1),
                then_target: edge(1, Some(0)),
                else_target: edge(2, Some(0)),
            },
        ),
        root_block(
            1,
            2,
            vec![take(0, 6, 5, ResolvedTy::String), destroy(1, 5)],
            SemTerminator::Goto(edge(3, Some(2))),
        ),
        root_block(2, 3, vec![], SemTerminator::Goto(edge(3, Some(3)))),
        root_block(3, 4, vec![destroy(2, 4)], done()),
    ];
    module
}

#[test]
fn partial_state_crosses_root_versions_and_joined_assignment_restores_it() {
    let mut module = joined_fixture();
    assert_valid(&module);
    probe(&mut module).blocks[3].ops = vec![
        copy(2, 12, 6, ResolvedTy::String),
        assign(3, 14, 6),
        op(
            4,
            SemOpKind::CopyValue { source: operand(4) },
            vec![owned(7, root_ty())],
        ),
        destroy(5, 7),
        destroy(6, 4),
    ];
    assert_valid(&module);
    probe(&mut module).blocks[3]
        .ops
        .insert(0, copy(7, 14, 8, ResolvedTy::String));
    probe(&mut module).blocks[3].ops.insert(1, destroy(8, 8));
    assert_lifetime(
        &module,
        "aggregate field is not initialized on every incoming path",
    );
}

fn assert_projection_error(module: &mut SemModule, expected: &str) {
    let function = probe(module).clone();
    let error = place_plan(&function, &module.aggregate_shapes, &module.type_facts).unwrap_err();
    assert!(error.contains(expected), "{error}");
    let diagnostics = verify_module(module);
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| matches!(&diagnostic.kind,
        SirDiagnosticKind::InvalidCallable { reason, .. } if reason.contains(expected))),
        "{diagnostics:#?}"
    );
}

#[test]
fn projection_query_rejects_missing_siblings_and_lost_cfg_partitions() {
    let mut missing = joined_fixture();
    probe(&mut missing)
        .places
        .retain(|place| place.id != PlaceId(3));
    assert_projection_error(&mut missing, "omits a sibling field");

    let mut collapsed = joined_fixture();
    probe(&mut collapsed)
        .places
        .retain(|place| ![PlaceId(14), PlaceId(15)].contains(&place.id));
    assert_projection_error(&mut collapsed, "loses or invents projected field state");

    let mut forgotten = joined_fixture();
    let function = probe(&mut forgotten).clone();
    let plan = place_plan(
        &function,
        &forgotten.aggregate_shapes,
        &forgotten.type_facts,
    )
    .unwrap();
    probe(&mut forgotten)
        .places
        .retain(|place| plan.projection(place.id).unwrap().root != OwnerRoot::Value(ValueId(4)));
    assert_projection_error(&mut forgotten, "loses or invents projected field state");
}

#[test]
fn projection_query_rejects_ambiguous_or_inexact_paths() {
    let mut duplicate = fixture();
    let mut place = probe(&mut duplicate).places[2].clone();
    place.id = PlaceId(10);
    probe(&mut duplicate).places.push(place);
    assert_projection_error(&mut duplicate, "duplicate places for one field");

    let mut cycle = fixture();
    if let PlaceOrigin::Aggregate { base, .. } = &mut probe(&mut cycle).places[1].origin {
        *base = PlaceBase::Place(PlaceId(3));
    }
    assert_projection_error(&mut cycle, "cyclic parent path");

    let mut changed_root = fixture();
    if let PlaceOrigin::Aggregate { base, .. } = &mut probe(&mut changed_root).places[2].origin {
        *base = PlaceBase::Value(ValueId(1));
    }
    assert_projection_error(&mut changed_root, "requires an owned root");

    let mut wrong_type = fixture();
    probe(&mut wrong_type).places[2].ty = ResolvedTy::Bytes;
    assert_projection_error(&mut wrong_type, "differs from its exact field type");

    let mut outside = fixture();
    if let PlaceOrigin::Aggregate { field, .. } = &mut probe(&mut outside).places[2].origin {
        *field = 8;
    }
    assert_projection_error(&mut outside, "out of bounds");
}

#[test]
fn a_borrowed_root_cannot_supply_projected_ownership() {
    let mut module = fixture();
    probe(&mut module).blocks[0].ops = vec![
        op(
            0,
            SemOpKind::BeginBorrow { owner: operand(0) },
            vec![ValueDef {
                own: OwnKind::Guaranteed,
                ..owned(2, root_ty())
            }],
        ),
        op(1, SemOpKind::EndBorrow { borrow: operand(2) }, vec![]),
        destroy(2, 0),
    ];
    probe(&mut module).places = partition(2, 0);
    assert_projection_error(&mut module, "requires an owned root");
}

#[test]
fn initializing_requires_empty_contents_but_assignment_accepts_mixed_contents() {
    let mut module = joined_fixture();
    probe(&mut module).blocks[3].ops = vec![
        copy(2, 12, 6, ResolvedTy::String),
        op(
            3,
            SemOpKind::StoreInit {
                place: PlaceId(14),
                value: operand(6),
            },
            vec![],
        ),
        destroy(4, 4),
    ];
    assert_lifetime(
        &module,
        "aggregate field initialization would overwrite a live value",
    );
    probe(&mut module).blocks[3].ops[1] = assign(3, 14, 6);
    assert_valid(&module);
}

#[test]
fn whole_value_calls_and_returns_require_complete_roots() {
    let mut call = fixture();
    let target = probe(&mut call).callable;
    probe(&mut call).blocks = vec![
        block(
            0,
            vec![take(0, 2, 2, ResolvedTy::String), destroy(1, 2)],
            SemTerminator::Call {
                id: OpId(2),
                callee: target,
                args: vec![
                    BoundaryOperand {
                        operand: operand(0),
                        decision: BoundaryDecision::Move,
                    },
                    BoundaryOperand {
                        operand: operand(1),
                        decision: BoundaryDecision::Borrow,
                    },
                ],
                result: CallResult::Unit,
                normal: Some(edge(1, None)),
                unwind: CallUnwind::Cleanup(edge(2, None)),
            },
        ),
        block(1, vec![], done()),
        block(2, vec![], SemTerminator::ResumeUnwind),
    ];
    assert_lifetime(
        &call,
        "partially consumed aggregate cannot be copied, borrowed, invoked or transferred",
    );

    let mut returned = fixture_source(
        r#"
        fn probe(consume value: (string, (string, bytes)), flag: bool) -> (string, (string, bytes)) { value }
        fn main() { let result = probe(("outer", ("inner", b"payload")), true); }
    "#,
    );
    probe(&mut returned).blocks = vec![block(
        0,
        vec![take(0, 2, 2, ResolvedTy::String), destroy(1, 2)],
        SemTerminator::Return {
            value: Some(BoundaryOperand {
                operand: operand(0),
                decision: BoundaryDecision::Move,
            }),
        },
    )];
    assert_lifetime(
        &returned,
        "partially consumed aggregate cannot be copied, borrowed, invoked or transferred",
    );
    probe(&mut returned).blocks[0].ops = vec![];
    assert_valid(&returned);
}

#[test]
fn loop_backedge_preserves_partial_state_until_reinitialization() {
    let mut module = fixture();
    let function = probe(&mut module);
    function.places.extend(partition(2, 4));
    function.places.extend(partition(5, 8));
    function.blocks = vec![
        block(0, vec![], SemTerminator::Goto(edge(1, Some(0)))),
        root_block(
            1,
            2,
            vec![
                take(0, 6, 3, ResolvedTy::String),
                destroy(1, 3),
                copy(2, 4, 4, ResolvedTy::String),
                assign(3, 6, 4),
            ],
            SemTerminator::Branch {
                condition: operand(1),
                then_target: edge(1, Some(2)),
                else_target: edge(2, Some(2)),
            },
        ),
        root_block(2, 5, vec![destroy(4, 5)], done()),
    ];
    assert_valid(&module);
    probe(&mut module).blocks[1].ops.truncate(2);
    assert_lifetime(
        &module,
        "aggregate field is not initialized on every incoming path",
    );
}

#[test]
fn loop_permutations_snapshot_distinct_root_availability() {
    let mut module = fixture();
    let function = probe(&mut module);
    for (root, start) in [(2, 4), (4, 8), (5, 12), (6, 16), (7, 20)] {
        function.places.extend(partition(root, start));
    }
    let pair_edge = |target, first, second| Edge {
        target: BlockId(target),
        args: vec![operand(first), operand(second)],
    };
    let mut iteration = root_block(
        1,
        4,
        vec![
            copy(3, 14, 8, ResolvedTy::String),
            destroy(4, 8),
            take(5, 14, 9, ResolvedTy::String),
            destroy(6, 9),
            copy(7, 8, 10, ResolvedTy::String),
            assign(8, 10, 10),
        ],
        SemTerminator::Branch {
            condition: operand(1),
            then_target: pair_edge(1, 5, 4),
            else_target: pair_edge(2, 4, 5),
        },
    );
    iteration.args.push(BlockArg {
        value: ValueId(5),
        ty: root_ty(),
        own: OwnKind::Owned,
    });
    let mut exit = root_block(
        2,
        6,
        vec![
            copy(9, 18, 11, ResolvedTy::String),
            destroy(10, 11),
            copy(11, 20, 12, ResolvedTy::String),
            destroy(12, 12),
            destroy(13, 6),
            destroy(14, 7),
        ],
        done(),
    );
    exit.args.push(BlockArg {
        value: ValueId(7),
        ty: root_ty(),
        own: OwnKind::Owned,
    });
    function.blocks = vec![
        block(
            0,
            vec![
                op(
                    0,
                    SemOpKind::CopyValue { source: operand(0) },
                    vec![owned(2, root_ty())],
                ),
                take(1, 2, 3, ResolvedTy::String),
                destroy(2, 3),
            ],
            SemTerminator::Goto(pair_edge(1, 0, 2)),
        ),
        iteration,
        exit,
    ];
    assert_valid(&module);
    // The second exit root retains the missing field after the permutation.
    probe(&mut module).blocks[2]
        .ops
        .insert(0, copy(15, 22, 13, ResolvedTy::String));
    probe(&mut module).blocks[2].ops.insert(1, destroy(16, 13));
    assert_lifetime(
        &module,
        "aggregate field is not initialized on every incoming path",
    );
}

#[test]
fn call_failure_cleanup_keeps_the_partially_consumed_root() {
    let mut module = fixture_source(
        r#"
        fn consume_text(consume text: string) -> i64 { 1 }
        fn probe(consume value: (string, (string, bytes)), flag: bool) { let n = consume_text("argument"); }
        fn main() { probe(("outer", ("inner", b"payload")), true); }
    "#,
    );
    let callee = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "consume_text")
        .unwrap()
        .callable;
    probe(&mut module).blocks = vec![
        block(
            0,
            vec![take(0, 2, 2, ResolvedTy::String)],
            SemTerminator::Call {
                id: OpId(1),
                callee,
                args: vec![BoundaryOperand {
                    operand: operand(2),
                    decision: BoundaryDecision::Move,
                }],
                result: CallResult::Value(ValueDef {
                    id: ValueId(3),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }),
                normal: Some(edge(1, None)),
                unwind: CallUnwind::Cleanup(edge(2, None)),
            },
        ),
        block(1, vec![destroy(2, 0)], done()),
        block(2, vec![destroy(3, 0)], SemTerminator::ResumeUnwind),
    ];
    // Original source locals are replaced by this reduced semantic fixture.
    probe(&mut module).bindings.retain(|binding| {
        matches!(
            binding.target,
            hew_sir::BindingTarget::Value(ValueId(0 | 1))
        )
    });
    assert_valid(&module);
    probe(&mut module).blocks[2]
        .ops
        .insert(0, copy(4, 2, 4, ResolvedTy::String));
    probe(&mut module).blocks[2].ops.insert(1, destroy(5, 4));
    assert_lifetime(
        &module,
        "aggregate field is not initialized on every incoming path",
    );
}

fn record_fixture() -> SemModule {
    let mut module = fixture_source(
        r#"
        type Inner { name: string, payload: bytes }
        type Outer { label: string, inner: Inner }
        fn probe(consume value: Outer, flag: bool) {}
        fn main() { probe(Outer { label: "outer", inner: Inner { name: "inner", payload: b"payload" } }, true); }
    "#,
    );
    let root = probe(&mut module).params[0].ty.clone();
    let outer = module
        .aggregate_shapes
        .iter()
        .find(|shape| shape.aggregate_ty == root)
        .unwrap();
    let inner_ty = outer.fields[1].ty.clone();
    let outer_shape = AggregateShapeRef::Record(outer.id);
    let inner = module
        .aggregate_shapes
        .iter()
        .find(|shape| shape.aggregate_ty == inner_ty)
        .unwrap();
    let inner_shape = AggregateShapeRef::Record(inner.id);
    let function = probe(&mut module);
    function.places[1].ty = inner_ty;
    for place in &mut function.places {
        if let PlaceOrigin::Aggregate { base, shape, .. } = &mut place.origin {
            *shape = if matches!(base, PlaceBase::Place(_)) {
                inner_shape
            } else {
                outer_shape
            };
        }
    }
    module
}

#[test]
fn named_record_paths_preserve_siblings_and_reject_custom_cleanup_ancestors() {
    let mut module = record_fixture();
    probe(&mut module).blocks[0].ops = vec![
        take(0, 2, 2, ResolvedTy::String),
        destroy(1, 2),
        copy(2, 0, 3, ResolvedTy::String),
        destroy(3, 3),
        destroy(4, 0),
    ];
    assert_valid(&module);
    let outer_ty = probe(&mut module).params[0].ty.clone();
    let inner_ty = probe(&mut module).places[1].ty.clone();
    for ty in [outer_ty, inner_ty] {
        for marker in [
            hew_types::DeclarationMarker::Resource,
            hew_types::DeclarationMarker::Linear,
        ] {
            let mut invalid = module.clone();
            invalid
                .aggregate_shapes
                .iter_mut()
                .find(|shape| shape.aggregate_ty == ty)
                .unwrap()
                .marker = marker;
            assert_projection_error(
                &mut invalid,
                "cannot traverse a resource or linear ancestor",
            );
        }
    }
}

#[test]
fn custom_cleanup_fields_remain_indivisible_transferable_leaves() {
    for marker in [
        hew_types::DeclarationMarker::Resource,
        hew_types::DeclarationMarker::Linear,
    ] {
        let mut module = record_fixture();
        let inner_ty = probe(&mut module).places[1].ty.clone();
        module
            .aggregate_shapes
            .iter_mut()
            .find(|shape| shape.aggregate_ty == inner_ty)
            .unwrap()
            .marker = marker;
        probe(&mut module).places.truncate(2);
        let function = probe(&mut module).clone();
        let plan = place_plan(&function, &module.aggregate_shapes, &module.type_facts).unwrap();
        assert_eq!(
            plan.leaves(OwnerRoot::Value(ValueId(0))).unwrap(),
            [PlaceId(0), PlaceId(1)]
        );
        assert_eq!(plan.projection(PlaceId(1)).unwrap().recipe.ty, inner_ty);
    }
}

#[test]
fn opaque_fields_are_leaves_but_cannot_be_projection_ancestors() {
    let mut module = record_fixture();
    let inner_ty = probe(&mut module).places[1].ty.clone();
    let mut opaque_ty = inner_ty.clone();
    let ResolvedTy::Named { is_opaque, .. } = &mut opaque_ty else {
        panic!("named inner fixture")
    };
    *is_opaque = true;
    let facts = module.type_facts[&hew_types::TypeInstanceKey(inner_ty.clone())];
    module
        .type_facts
        .insert(hew_types::TypeInstanceKey(opaque_ty.clone()), facts);
    for shape in &mut module.aggregate_shapes {
        if shape.aggregate_ty == inner_ty {
            shape.aggregate_ty = opaque_ty.clone();
        }
        for field in &mut shape.fields {
            if field.ty == inner_ty {
                field.ty = opaque_ty.clone();
            }
        }
    }
    probe(&mut module).places[1].ty = opaque_ty.clone();
    assert_projection_error(&mut module, "cannot traverse an opaque ancestor");
    probe(&mut module).places.truncate(2);
    let function = probe(&mut module).clone();
    let plan = place_plan(&function, &module.aggregate_shapes, &module.type_facts).unwrap();
    assert_eq!(plan.projection(PlaceId(1)).unwrap().recipe.ty, opaque_ty);
}

#[test]
fn zero_sized_no_drop_fields_still_have_initialization_identity() {
    let mut module = fixture_source(
        r#"
        type Empty {}
        fn probe(consume value: (string, Empty), flag: bool) {}
        fn main() { probe(("outer", Empty {}), true); }
    "#,
    );
    let ResolvedTy::Tuple(fields) = &probe(&mut module).params[0].ty else {
        panic!("tuple fixture")
    };
    let empty_ty = fields[1].clone();
    probe(&mut module).places = vec![
        PlaceDecl {
            id: PlaceId(0),
            ty: ResolvedTy::String,
            origin: PlaceOrigin::Aggregate {
                base: PlaceBase::Value(ValueId(0)),
                shape: AggregateShapeRef::Tuple,
                field: 0,
            },
        },
        PlaceDecl {
            id: PlaceId(1),
            ty: empty_ty.clone(),
            origin: PlaceOrigin::Aggregate {
                base: PlaceBase::Value(ValueId(0)),
                shape: AggregateShapeRef::Tuple,
                field: 1,
            },
        },
    ];
    probe(&mut module).blocks[0].ops = vec![
        op(
            0,
            SemOpKind::LoadTake { place: PlaceId(1) },
            vec![ValueDef {
                id: ValueId(2),
                ty: empty_ty.clone(),
                own: OwnKind::None,
            }],
        ),
        destroy(1, 0),
    ];
    assert_valid(&module);
    probe(&mut module).blocks[0].ops.insert(
        1,
        op(
            2,
            SemOpKind::LoadCopy { place: PlaceId(1) },
            vec![ValueDef {
                id: ValueId(3),
                ty: empty_ty,
                own: OwnKind::None,
            }],
        ),
    );
    assert_lifetime(
        &module,
        "aggregate field is not initialized on every incoming path",
    );
}
