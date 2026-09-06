use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, AggregateShapeRef, BoundaryDecision, CallResult, SemOpKind,
    SemParamPassing, SemTerminator, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_sir::LoweredModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    lower_module(&hir.module, &facts)
}

fn assert_main_lowered(lowered: &hew_sir::LoweredModule) {
    assert!(
        matches!(
            lowered.statuses.iter().find(|status| status.name == "main"),
            Some(status) if matches!(status.status, SirLoweringStatus::Lowered)
        ),
        "main must lower: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "aggregate source must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
}

#[test]
fn owned_tuple_construction_and_repeated_borrows_are_explicit() {
    let lowered = lower_source(
        r#"
        fn keep_text(value: string) {}
        fn keep_bytes(value: bytes) {}

        fn main() {
            let original = "original";
            let pair = (original, b"A");
            keep_text(original);
            keep_text(pair.0);
            keep_text(pair.0);
            keep_bytes(pair.1);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main must have a body");
    assert!(main.blocks.iter().flat_map(|block| &block.ops).any(|op| {
        matches!(
            op.kind,
            SemOpKind::AggregateMake {
                shape: AggregateShapeRef::Tuple,
                ..
            }
        )
    }));
    let plan = hew_sir::place_plan(
        main,
        &lowered.module.aggregate_shapes,
        &lowered.module.type_facts,
    )
    .unwrap();
    let fields: Vec<_> = main
        .blocks
        .iter()
        .flat_map(|b| &b.ops)
        .filter_map(|op| match &op.kind {
            SemOpKind::LoadBorrow { place } => {
                let projection = plan.projection(*place).unwrap();
                assert_eq!(projection.path.len(), 1);
                assert_eq!(projection.path[0].shape, AggregateShapeRef::Tuple);
                assert_eq!(op.results[0].own, hew_sir::OwnKind::Guaranteed);
                Some((projection.root, projection.path[0].field))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        fields.iter().map(|(_, field)| *field).collect::<Vec<_>>(),
        [0, 0, 1]
    );
    assert!(
        fields.iter().all(|(root, _)| *root == fields[0].0),
        "repeated reads borrow the same tuple owner"
    );
}

#[test]
fn owned_record_shape_and_field_order_are_exact() {
    let lowered = lower_source(
        r#"
        type Packet { label: string, payload: bytes }

        fn keep_text(value: string) {}
        fn keep_bytes(value: bytes) {}

        fn main() {
            let original_label = "label";
            let original_payload = b"A";
            let packet = Packet { payload: original_payload, label: original_label };
            let packet_copy = packet;
            keep_text(original_label);
            keep_bytes(original_payload);
            keep_text(packet.label);
            keep_text(packet.label);
            keep_bytes(packet.payload);
            keep_text(packet_copy.label);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let [shape] = lowered.module.aggregate_shapes.as_slice() else {
        panic!("one demanded record type must publish exactly one shape")
    };
    assert_eq!(shape.instance.nominal.display_name(), "Packet");
    assert_eq!(
        shape
            .fields
            .iter()
            .map(|field| field.name.as_str())
            .collect::<Vec<_>>(),
        ["label", "payload"],
        "the descriptor must retain declaration order, not initializer order"
    );
    let main = lowered
        .module
        .functions
        .iter()
        .find(|f| f.name == "main")
        .unwrap();
    let plan = hew_sir::place_plan(
        main,
        &lowered.module.aggregate_shapes,
        &lowered.module.type_facts,
    )
    .unwrap();
    let fields: Vec<_> = main
        .blocks
        .iter()
        .flat_map(|b| &b.ops)
        .filter_map(|op| match &op.kind {
            SemOpKind::LoadBorrow { place } => {
                let projection = plan.projection(*place).unwrap();
                assert_eq!(projection.path.len(), 1);
                assert_eq!(
                    projection.path[0].shape,
                    AggregateShapeRef::Record(shape.id)
                );
                Some((projection.root, projection.path[0].field))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        fields.iter().map(|(_, field)| *field).collect::<Vec<_>>(),
        [0, 0, 1, 0]
    );
    assert_eq!(fields[0].0, fields[1].0);
    assert_eq!(fields[0].0, fields[2].0);
    assert_ne!(
        fields[0].0, fields[3].0,
        "the copied record must have an independent owner"
    );
    assert!(
        lowered.module.functions.iter().any(|function| {
            function
                .blocks
                .iter()
                .flat_map(|block| &block.ops)
                .any(|op| {
                    matches!(op.kind, SemOpKind::CopyValue { .. })
                        && op.results.first().is_some_and(|result| {
                            result.ty == shape.aggregate_ty && result.own == hew_sir::OwnKind::Owned
                        })
                })
        }),
        "an ordinary aggregate binding alias must be one explicit whole-value copy"
    );
}

#[test]
fn owned_projection_refuses_a_missing_clone_recipe() {
    let mut lowered = lower_source(
        r#"
        type Packet { label: string }
        fn keep_text(value: string) {}
        fn main() {
            let packet = Packet { label: "label" };
            let label = packet.label;
            keep_text(label);
        }
        "#,
    );
    let projection = lowered
        .module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .find(|op| matches!(op.kind, SemOpKind::LoadCopy { .. }))
        .expect("source must produce an aggregate projection")
        .id;
    lowered
        .module
        .type_facts
        .get_mut(&hew_types::TypeInstanceKey(hew_types::ResolvedTy::String))
        .expect("checker must publish string facts")
        .clone = hew_types::CloneKind::None;

    assert!(verify_module(&lowered.module).iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidOperation { op, reason }
                if *op == projection && reason.contains("has no copy operation")
        )
    }));
}

#[test]
fn aggregate_call_borrows_caller_and_returns_an_independent_owner() {
    let lowered = lower_source(
        r#"
        type Packet { label: string, payload: bytes }

        fn echo(value: Packet) -> Packet { value }
        fn unused(value: Packet) -> Packet { value }
        fn keep_text(value: string) {}

        fn main() {
            let original = Packet { label: "label", payload: b"A" };
            let returned = echo(original);
            keep_text(original.label);
            keep_text(returned.label);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let echo = lowered
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "echo")
        .expect("echo must have an exact callable header");
    assert_eq!(echo.signature.params[0].passing, SemParamPassing::Borrow);
    assert_eq!(
        echo.signature.return_ty,
        lowered.module.aggregate_shapes[0].aggregate_ty
    );
    let echo_body = lowered
        .module
        .functions
        .iter()
        .find(|function| function.callable == echo.id)
        .expect("echo must have a demanded body");
    assert!(echo_body
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .any(|op| {
            matches!(op.kind, SemOpKind::CopyValue { .. })
                && op.results.first().is_some_and(|result| {
                    result.ty == echo.signature.return_ty && result.own == hew_sir::OwnKind::Owned
                })
        }));

    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main must have a body");
    assert!(main.blocks.iter().any(|block| {
        matches!(
            &block.terminator,
            SemTerminator::Call {
                args,
                result: CallResult::Value(result),
                ..
            } if args.len() == 1
                && args[0].decision == BoundaryDecision::Borrow
                && result.ty == echo.signature.return_ty
                && result.own == hew_sir::OwnKind::Owned
        )
    }));

    assert!(matches!(
        lowered
            .statuses
            .iter()
            .find(|status| status.name == "unused")
            .map(|status| &status.status),
        Some(SirLoweringStatus::NotReached)
    ));

    let mut missing_shape = lowered.module.clone();
    missing_shape.aggregate_shapes.clear();
    assert!(verify_module(&missing_shape).iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidCallable { callable, reason }
                if *callable == echo.id
                    && reason.contains("outside the owned-call SIR surface")
        )
    }));
}

#[test]
fn aggregate_patterns_consume_copies_and_bind_every_owned_field() {
    let lowered = lower_source(
        r#"
        type Packet { label: string, payload: bytes }

        fn make_packet() -> Packet {
            Packet { label: "label", payload: b"payload" }
        }
        fn keep_text(value: string) {}
        fn keep_bytes(value: bytes) {}

        fn main() {
            let original = make_packet();
            let { label, payload } = original;
            keep_text(original.label);
            keep_text(label);
            keep_bytes(payload);

            let nested = (("nested", b"inner"), b"outer");
            let ((nested_label, nested_payload), outer_payload) = nested;
            let original_inner = nested.0;
            keep_text(original_inner.0);
            keep_text(nested_label);
            keep_bytes(nested_payload);
            keep_bytes(outer_payload);

            let ignored = ("kept", b"discarded");
            let (kept, _) = ignored;
            keep_text(kept);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main must have a body");
    let destructures = main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .filter(|operation| matches!(operation.kind, SemOpKind::Destructure { .. }))
        .collect::<Vec<_>>();
    assert_eq!(
        destructures.len(),
        4,
        "record, outer tuple, nested tuple and wildcard tuple must each remain one operation"
    );
    assert!(
        destructures
            .iter()
            .all(|operation| operation.results.len() == 2),
        "each destructure must account for every field, including wildcard fields"
    );
    assert!(
        main.blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|operation| {
                matches!(operation.kind, SemOpKind::CopyValue { .. })
                    && operation.results.first().is_some_and(|result| {
                        lowered
                            .module
                            .aggregate_shapes
                            .iter()
                            .any(|shape| shape.aggregate_ty == result.ty)
                    })
            }),
        "destructuring an ordinary record binding must first copy the whole owner"
    );
}

#[test]
fn aggregate_destructure_refuses_a_result_outside_the_exact_shape() {
    let mut lowered = lower_source(
        r#"
        type Packet { label: string, payload: bytes }
        fn main() {
            let packet = Packet { label: "label", payload: b"payload" };
            let { label, payload } = packet;
        }
        "#,
    );
    let destructure = lowered
        .module
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .flat_map(|block| &mut block.ops)
        .find(|operation| matches!(operation.kind, SemOpKind::Destructure { .. }))
        .expect("source must produce an aggregate destructure");
    let operation = destructure.id;
    destructure.results[0].ty = hew_types::ResolvedTy::I64;

    assert!(verify_module(&lowered.module).iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidOperation { op, reason }
                if *op == operation
                    && reason.contains("aggregate.destructure result 0")
                    && reason.contains("expected `string`")
        )
    }));
}

#[test]
fn nested_record_and_tuple_argument_loans_close_on_both_runtime_edges() {
    let lowered = lower_source(
        r#"
        type Inner { items: Vec<string> }
        type Outer { pair: (Inner, string) }
        fn main() -> i64 {
            let outer = Outer { pair: (Inner { items: ["first"] }, "sibling") };
            outer.pair.0.items[0].len()
        }
        "#,
    );
    assert_main_lowered(&lowered);
    let main = lowered
        .module
        .functions
        .iter()
        .find(|f| f.name == "main")
        .unwrap();
    let loans: Vec<_> = main
        .blocks
        .iter()
        .flat_map(|b| &b.ops)
        .filter(|op| matches!(op.kind, SemOpKind::LoadBorrow { .. }))
        .map(|op| {
            assert_eq!(op.results[0].own, hew_sir::OwnKind::Guaranteed);
            op.results[0].id
        })
        .collect();
    assert_eq!(
        loans.len(),
        1,
        "the leaf borrows directly from its owning root"
    );
    let plan = hew_sir::place_plan(
        main,
        &lowered.module.aggregate_shapes,
        &lowered.module.type_facts,
    )
    .unwrap();
    let place = main
        .blocks
        .iter()
        .flat_map(|b| &b.ops)
        .find_map(|op| match &op.kind {
            SemOpKind::LoadBorrow { place } if op.results[0].id == loans[0] => Some(*place),
            _ => None,
        })
        .unwrap();
    let projection = plan.projection(place).unwrap();
    let owner = projection.root;
    assert_eq!(
        projection
            .path
            .iter()
            .map(|step| step.field)
            .collect::<Vec<_>>(),
        [0, 0, 0]
    );
    assert_eq!(projection.recipe.own, hew_sir::OwnKind::Owned);
    assert_eq!(
        plan.leaves(owner).unwrap().len(),
        2,
        "include the retained tuple sibling"
    );
    let (normal, fault) = main
        .blocks
        .iter()
        .find_map(|b| match &b.terminator {
            SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Index),
                normal,
                unwind: hew_sir::CallUnwind::Cleanup(fault),
                result: CallResult::Value(result),
                ..
            } => {
                assert_eq!(
                    result.own,
                    hew_sir::OwnKind::Owned,
                    "index still extracts an independent item"
                );
                Some((normal.target, fault.target))
            }
            _ => None,
        })
        .unwrap();
    for target in [normal, fault] {
        let block = main.blocks.iter().find(|b| b.id == target).unwrap();
        let ended: Vec<_> = block
            .ops
            .iter()
            .take(loans.len())
            .map(|op| match &op.kind {
                SemOpKind::EndBorrow { borrow } => borrow.value,
                other => panic!("loans must end before owner cleanup: {other:?}"),
            })
            .collect();
        assert_eq!(ended, loans.iter().rev().copied().collect::<Vec<_>>());
    }
}

#[test]
fn borrowed_temporary_fields_can_return_an_independent_owner() {
    let lowered = lower_source(
        r#"
        type Inner { text: string }
        type Outer { inner: Inner }
        fn make() -> Outer { Outer { inner: Inner { text: "kept" } } }
        fn echo(value: string) -> string { value }
        fn main() -> i64 {
            let kept = echo(make().inner.text);
            kept.len()
        }
        "#,
    );
    assert_main_lowered(&lowered);
    let main = lowered
        .module
        .functions
        .iter()
        .find(|f| f.name == "main")
        .unwrap();
    assert_eq!(
        main.blocks
            .iter()
            .flat_map(|b| &b.ops)
            .filter(|op| { matches!(op.kind, SemOpKind::AggregateProjectBorrow { .. }) })
            .count(),
        2
    );
    let echo = lowered
        .module
        .functions
        .iter()
        .find(|f| f.name == "echo")
        .unwrap();
    assert!(
        echo.blocks.iter().flat_map(|b| &b.ops).any(|op| {
            matches!(op.kind, SemOpKind::CopyValue { .. })
                && op.results[0].own == hew_sir::OwnKind::Owned
        }),
        "the callee must copy the guaranteed parameter before returning it"
    );
}

#[test]
fn earlier_arguments_capture_owned_fields_before_later_effects() {
    for later in [r#"{ holder.items = ["new"]; 0 }"#, "indices[99]"] {
        let lowered = lower_source(&format!(
            r#"
            type Holder {{ items: Vec<string> }}
            fn read(items: Vec<string>, index: i64) -> string {{ items[index] }}
            fn main() -> i64 {{
                var holder = Holder {{ items: ["old"] }};
                let indices = [0];
                let kept = read(holder.items, {later});
                kept.len() + holder.items[0].len()
            }}
            "#,
        ));
        assert_main_lowered(&lowered);
        let read = lowered
            .module
            .callables
            .iter()
            .find(|c| c.symbol == "read")
            .unwrap();
        let main = lowered
            .module
            .functions
            .iter()
            .find(|f| f.name == "main")
            .unwrap();
        let captured = main
            .blocks
            .iter()
            .find_map(|b| match &b.terminator {
                SemTerminator::Call { callee, args, .. } if *callee == read.id => {
                    Some(args[0].operand.value)
                }
                _ => None,
            })
            .unwrap();
        assert!(
            main.blocks.iter().flat_map(|b| &b.ops).any(|op| {
                matches!(op.kind, SemOpKind::LoadCopy { .. })
                    && op.results[0].id == captured
                    && op.results[0].own == hew_sir::OwnKind::Owned
            }),
            "later mutation or failure requires a captured independent value"
        );
    }
}

#[test]
fn scalar_arguments_copy_the_exact_nested_leaf() {
    let lowered = lower_source(
        r#"
        type Inner { items: Vec<string>, count: i64 }
        type Outer { inner: Inner }
        fn echo(value: i64) -> i64 { value }
        fn main() -> i64 {
            let outer = Outer { inner: Inner { items: ["kept"], count: 7 } };
            echo(outer.inner.count)
        }
        "#,
    );
    assert_main_lowered(&lowered);
    let main = lowered
        .module
        .functions
        .iter()
        .find(|f| f.name == "main")
        .unwrap();
    let operations: Vec<_> = main.blocks.iter().flat_map(|b| &b.ops).collect();
    let plan = hew_sir::place_plan(
        main,
        &lowered.module.aggregate_shapes,
        &lowered.module.type_facts,
    )
    .unwrap();
    let scalar = operations
        .iter()
        .find(|op| matches!(op.kind, SemOpKind::LoadCopy { .. }))
        .unwrap();
    let SemOpKind::LoadCopy { place } = scalar.kind else {
        unreachable!()
    };
    let projection = plan.projection(place).unwrap();
    assert_eq!(
        projection
            .path
            .iter()
            .map(|step| step.field)
            .collect::<Vec<_>>(),
        [0, 1]
    );
    assert_eq!(projection.recipe.own, hew_sir::OwnKind::None);
    assert!(
        !operations
            .iter()
            .any(|op| matches!(op.kind, SemOpKind::LoadBorrow { .. })),
        "reading a scalar leaf needs no intermediate owner loan"
    );
    assert!(
        operations.iter().any(|op| {
            matches!(op.kind, SemOpKind::LoadCopy { .. })
                && op.results[0].own == hew_sir::OwnKind::None
                && op.results[0].ty == hew_types::ResolvedTy::I64
        }),
        "the scalar field is still an independent bit copy"
    );
}

#[test]
fn runtime_read_keeps_bindings_replaced_by_index_evaluation() {
    let lowered = lower_source(
        r#"
        type Holder { items: Vec<string> }
        fn main() -> i64 {
            var holder = Holder { items: ["old"] };
            let kept = holder.items[{ holder.items = ["new"]; 0 }];
            kept.len() + holder.items[0].len()
        }
        "#,
    );
    assert_main_lowered(&lowered);
    let main = lowered
        .module
        .functions
        .iter()
        .find(|f| f.name == "main")
        .unwrap();
    assert!(
        main.blocks.iter().flat_map(|b| &b.ops).any(|op| {
            matches!(op.kind, SemOpKind::LoadCopy { .. })
                && op.results[0].own == hew_sir::OwnKind::Owned
        }),
        "the receiver must be captured before the index expression replaces it"
    );
}
