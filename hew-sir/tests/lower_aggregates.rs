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
fn owned_tuple_construction_and_repeated_projection_are_explicit() {
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
    assert_eq!(
        main.blocks
            .iter()
            .flat_map(|block| &block.ops)
            .filter(|op| matches!(op.kind, SemOpKind::AggregateProjectCopy { .. }))
            .count(),
        3,
        "each ordinary owned tuple field read must produce its own explicit copy"
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
    assert_eq!(
        lowered
            .module
            .functions
            .iter()
            .find(|function| function.name == "main")
            .expect("main must have a body")
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .filter(|op| matches!(op.kind, SemOpKind::AggregateProjectCopy { .. }))
            .count(),
        4
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
            keep_text(packet.label);
        }
        "#,
    );
    let projection = lowered
        .module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .find(|op| matches!(op.kind, SemOpKind::AggregateProjectCopy { .. }))
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
