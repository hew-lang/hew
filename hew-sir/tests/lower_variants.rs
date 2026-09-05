use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BoundaryDecision, SemOpKind, SemParamPassing, SemTerminator,
    SirDiagnosticKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

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
        "variant source must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
}

#[test]
fn user_enum_call_borrows_caller_and_match_consumes_a_copy() {
    let lowered = lower_source(
        r#"
        enum Choice { Text(string); Empty }

        fn keep_text(value: string) {}
        fn inspect(value: Choice) -> i64 {
            match value {
                .Text(text) => { keep_text(text); 1 },
                .Empty => 0,
            }
        }

        fn main() {
            let original = Choice.Text("hello");
            inspect(original);
            inspect(original);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let [shape] = lowered.module.variant_shapes.as_slice() else {
        panic!("one demanded user enum must publish exactly one variant shape")
    };
    assert_eq!(shape.enum_ty.user_facing().to_string(), "Choice");
    assert_eq!(shape.variants[0].name, "Text");
    assert_eq!(shape.variants[0].fields[0].ty, ResolvedTy::String);

    let inspect = lowered
        .module
        .callables
        .iter()
        .find(|callable| callable.symbol == "inspect")
        .expect("inspect must have an exact callable header");
    assert_eq!(inspect.signature.params[0].passing, SemParamPassing::Borrow);
    let body = lowered
        .module
        .functions
        .iter()
        .find(|function| function.callable == inspect.id)
        .expect("inspect must have a demanded body");
    assert!(body.blocks.iter().flat_map(|block| &block.ops).any(|op| {
        matches!(op.kind, SemOpKind::CopyValue { .. })
            && op
                .results
                .first()
                .is_some_and(|result| result.ty == shape.enum_ty)
    }));
    assert!(body.blocks.iter().any(|block| {
        matches!(
            &block.terminator,
            SemTerminator::SwitchVariant { arms, .. }
                if arms.len() == 2
                    && arms[0].fields.len() == 1
                    && arms[1].fields.is_empty()
        )
    }));

    let main = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main must have a body");
    assert_eq!(
        main.blocks
            .iter()
            .filter_map(|block| match &block.terminator {
                SemTerminator::Call { callee, args, .. } if *callee == inspect.id => Some(args),
                _ => None,
            })
            .filter(|args| { args.len() == 1 && args[0].decision == BoundaryDecision::Borrow })
            .count(),
        2,
        "both ordinary calls must borrow the same caller-owned enum"
    );
}

#[test]
fn result_constructor_return_and_exhaustive_match_transfer_owned_payloads() {
    let lowered = lower_source(
        r#"
        fn make() -> Result<string, string> {
            Ok("accepted")
        }

        fn choose(value: Result<string, string>) -> string {
            match value {
                .Ok(text) => text,
                .Err(reason) => reason,
            }
        }

        fn keep_text(value: string) {}
        fn main() {
            let original = make();
            let first = choose(original);
            let second = choose(original);
            keep_text(first);
            keep_text(second);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let result_shape = lowered
        .module
        .variant_shapes
        .iter()
        .find(|shape| shape.enum_ty.user_facing().to_string() == "Result<string, string>")
        .expect("Result<string, string> must have one exact descriptor");
    assert_eq!(result_shape.variants.len(), 2);
    assert!(result_shape
        .variants
        .iter()
        .all(|variant| variant.fields.len() == 1 && variant.fields[0].ty == ResolvedTy::String));
    assert!(lowered
        .module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.ops)
        .any(|op| matches!(op.kind, SemOpKind::VariantMake { .. })));
}

#[test]
fn fresh_option_match_accounts_for_unbound_owned_payload() {
    let lowered = lower_source(
        r#"
        fn classify(value: Option<string>) -> i64 {
            match value {
                .Some(_) => 1,
                .None => 0,
            }
        }

        fn main() {
            classify(Some("temporary"));
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let classify = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "classify")
        .expect("classify must have a body");
    let some_block = classify
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::SwitchVariant { arms, .. } => arms
                .iter()
                .find(|arm| arm.variant == 0)
                .map(|arm| arm.target.target),
            _ => None,
        })
        .expect("Some arm must have a payload block");
    let some_field = classify
        .blocks
        .iter()
        .find(|block| block.id == some_block)
        .and_then(|block| block.args.first())
        .map(|field| field.value)
        .expect("Some arm target must materialize its owned payload");
    assert!(
        classify
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|op| {
                matches!(
                    &op.kind,
                    SemOpKind::DestroyValue { value } if value.value == some_field
                )
            }),
        "an unbound owned payload must still be destroyed on its selected arm"
    );
}

#[test]
fn bitcopy_record_and_option_use_exact_descriptors_without_owner_glue() {
    let lowered = lower_source(
        r"
        type Point { x: i64, y: i64 }

        fn point_x(point: Point) -> i64 { point.x }
        fn option_value(value: Option<i64>) -> i64 {
            match value {
                .Some(number) => number,
                .None => 0,
            }
        }

        fn main() {
            let point = Point { x: 2, y: 3 };
            point_x(point);
            point_x(point);
            let optional = Some(5);
            option_value(optional);
            option_value(optional);
        }
        ",
    );
    assert_main_lowered(&lowered);

    let point = lowered
        .module
        .aggregate_shapes
        .iter()
        .find(|shape| shape.aggregate_ty.user_facing().to_string() == "Point")
        .expect("Point must retain its exact record descriptor");
    let optional = lowered
        .module
        .variant_shapes
        .iter()
        .find(|shape| shape.enum_ty.user_facing().to_string() == "Option<i64>")
        .expect("Option<i64> must retain its exact variant descriptor");
    for ty in [&point.aggregate_ty, &optional.enum_ty] {
        let facts = lowered
            .module
            .type_facts
            .get(&hew_types::TypeInstanceKey((*ty).clone()))
            .expect("descriptor type must retain its checker facts");
        assert_eq!(facts.class, hew_types::ValueClass::BitCopy);
        assert_eq!(facts.clone, hew_types::CloneKind::Bits);
    }
}

#[test]
fn guarded_variant_match_lowers_explicit_predicate_cfg() {
    let lowered = lower_source(
        r#"
        fn choose(value: Option<string>) -> i64 {
            match value {
                .Some(text) if text == "special" => 1,
                .Some(_) => 2,
                .None => 0,
            }
        }

        fn main() {
            choose(Some("ordinary"));
        }
        "#,
    );

    assert_main_lowered(&lowered);
    let choose = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "choose")
        .unwrap_or_else(|| panic!("choose must have a body: {:#?}", lowered.statuses));
    assert!(choose.blocks.iter().any(|block| matches!(
        block.terminator,
        SemTerminator::RtCall {
            family: hew_types::RuntimeCallFamily::StringEquals,
            ..
        }
    )));
    assert!(choose
        .blocks
        .iter()
        .any(|block| matches!(block.terminator, SemTerminator::Branch { .. })));
}

#[test]
fn scalar_literal_payloads_use_their_exact_sir_constants() {
    let lowered = lower_source(
        r"
        enum Token { Value(f64, char); Empty }

        fn classify(value: Token) -> i64 {
            match value {
                .Value(1.5, 'x') => 1,
                .Value(_, _) => 2,
                .Empty => 0,
            }
        }

        fn main() {
            classify(Token.Value(1.5, 'x'));
        }
        ",
    );
    assert_main_lowered(&lowered);

    let classify = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "classify")
        .expect("classify must have a body");
    let operations = classify
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .collect::<Vec<_>>();
    assert!(operations
        .iter()
        .any(|operation| matches!(operation.kind, SemOpKind::ConstF64(value) if value.to_bits() == 1.5_f64.to_bits())));
    assert!(operations
        .iter()
        .any(|operation| matches!(operation.kind, SemOpKind::ConstChar('x'))));
}

#[test]
fn verifier_refuses_scalar_literals_with_forged_result_types() {
    let mut lowered = lower_source(
        r"
        enum Token { Value(f64, char); Empty }

        fn classify(value: Token) -> i64 {
            match value {
                .Value(1.5, 'x') => 1,
                .Value(_, _) => 2,
                .Empty => 0,
            }
        }

        fn main() {
            classify(Token.Value(1.5, 'x'));
        }
        ",
    );
    assert_main_lowered(&lowered);

    for operation in lowered
        .module
        .functions
        .iter_mut()
        .flat_map(|function| &mut function.blocks)
        .flat_map(|block| &mut block.ops)
    {
        if matches!(operation.kind, SemOpKind::ConstF64(_)) {
            operation.results[0].ty = ResolvedTy::Char;
        } else if matches!(operation.kind, SemOpKind::ConstChar(_)) {
            operation.results[0].ty = ResolvedTy::F64;
        }
    }

    let diagnostics = verify_module(&lowered.module);
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidConstType {
            expected: "f64",
            actual,
            ..
        } if actual == "char"
    )));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidConstType {
            expected: "char",
            actual,
            ..
        } if actual == "f64"
    )));
}

#[test]
fn match_payload_can_move_while_an_outer_fallback_remains_live() {
    let lowered = lower_source(
        r#"
        fn keep_text(value: string) {}
        fn choose(value: Option<string>) -> string {
            let fallback = "fallback";
            let selected = match value {
                .Some(text) => text,
                .None => fallback,
            };
            keep_text(fallback);
            selected
        }

        fn main() {
            keep_text(choose(None));
            keep_text(choose(Some("chosen")));
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let choose = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "choose")
        .unwrap_or_else(|| panic!("choose must have a body: {:#?}", lowered.statuses));
    let fallback = choose
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|operation| {
            matches!(operation.kind, SemOpKind::ConstStr(_))
                .then(|| operation.results.first().map(|result| result.id))
                .flatten()
        })
        .expect("choose must define its fallback string");
    assert!(
        choose
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .any(|operation| matches!(
                &operation.kind,
                SemOpKind::CopyValue { source } if source.value == fallback
            )),
        "the None arm must copy its outer fallback instead of consuming it"
    );
}

#[test]
fn ordered_guards_thread_mutation_into_later_same_variant_arms() {
    let lowered = lower_source(
        r"
        enum Number { Some(i64); None }

        fn classify(value: Number) -> i64 {
            var attempts = 0;
            match value {
                .Some(number) if { attempts = attempts + 1; number < 0 } => attempts,
                .Some(number) if { attempts = attempts + 1; number > 0 } => attempts,
                .Some(0) => attempts,
                .Some(_) => attempts,
                .None => attempts,
            }
        }

        fn main() {
            classify(Number.Some(5));
        }
        ",
    );
    assert_main_lowered(&lowered);

    let classify = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "classify")
        .expect("classify must have a body");
    assert!(
        classify
            .blocks
            .iter()
            .filter(|block| matches!(block.terminator, SemTerminator::Branch { .. }))
            .count()
            >= 3,
        "two guards and one literal predicate must remain ordered CFG decisions"
    );
    assert!(
        classify.blocks.iter().any(|block| block.args.len() >= 2),
        "the match join must carry both its value and the mutated outer binding"
    );
}

#[test]
fn nested_match_and_failed_string_guard_preserve_the_later_payload() {
    let lowered = lower_source(
        r#"
        fn choose(value: Result<Option<string>, string>) -> string {
            match value {
                .Ok(.Some(text)) if text == "skip" => "wrong",
                .Ok(.Some(text)) => text,
                .Ok(.None) => "none",
                .Err(reason) => reason,
            }
        }

        fn keep_text(value: string) {}
        fn main() {
            let original = Ok(Some("kept"));
            keep_text(choose(original));
            keep_text(choose(original));
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let choose = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "choose")
        .unwrap_or_else(|| panic!("choose must have a body: {:#?}", lowered.statuses));
    assert!(
        choose
            .blocks
            .iter()
            .filter(|block| matches!(block.terminator, SemTerminator::SwitchVariant { .. }))
            .count()
            >= 3,
        "the outer Result and independently initialized nested Option tests must be explicit"
    );
    assert!(choose.blocks.iter().any(|block| matches!(
        block.terminator,
        SemTerminator::RtCall {
            family: hew_types::RuntimeCallFamily::StringEquals,
            ..
        }
    )));
}

#[test]
fn unit_match_allows_a_selected_divergent_handler() {
    let lowered = lower_source(
        r#"
        fn report(value: string) {}
        fn handle(value: Result<string, string>) {
            match value {
                .Ok(text) => report(text),
                .Err(error) => { report(error); return; },
            }
        }

        fn main() {
            handle(Ok("ok"));
            handle(Err("error"));
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let handle = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "handle")
        .expect("handle must have a body");
    assert!(handle
        .blocks
        .iter()
        .any(|block| matches!(block.terminator, SemTerminator::SwitchVariant { .. })));
    assert!(
        handle
            .blocks
            .iter()
            .filter(|block| matches!(block.terminator, SemTerminator::Return { value: None }))
            .count()
            >= 2,
        "both the selected divergent handler and ordinary continuation must return explicitly"
    );
}

#[test]
fn result_propagation_lowers_expression_return_without_a_fake_value() {
    let lowered = lower_source(
        r#"
        fn pair(value: Result<string, string>) -> Result<(string, string), string> {
            let first = value?;
            Ok((first, "second"))
        }

        fn main() {
            pair(Ok("first"));
            pair(Err("failure"));
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let pair = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "pair")
        .unwrap_or_else(|| panic!("pair must have a body: {:#?}", lowered.statuses));
    assert_eq!(
        pair.blocks
            .iter()
            .filter(|block| matches!(block.terminator, SemTerminator::Return { value: Some(_) }))
            .count(),
        2,
        "the propagated Err and ordinary Ok each terminate with their real Result value"
    );
}

#[test]
fn never_typed_return_initializer_stops_before_binding_or_sibling_work() {
    let lowered = lower_source(
        r#"
        fn stop() -> Result<string, string> {
            let unreachable = return Err("stopped");
            Ok(unreachable)
        }

        fn main() {
            stop();
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let stop = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "stop")
        .unwrap_or_else(|| panic!("stop must have a body: {:#?}", lowered.statuses));
    assert_eq!(
        stop.blocks.len(),
        1,
        "return must not create a dead cursor block"
    );
    assert!(matches!(
        stop.blocks[0].terminator,
        SemTerminator::Return { value: Some(_) }
    ));
    assert!(
        stop.blocks[0]
            .ops
            .iter()
            .all(|op| !matches!(op.kind, SemOpKind::CopyValue { .. })),
        "unreachable binding work must not be emitted after the return"
    );
}

#[test]
fn let_else_binds_the_success_payload_into_the_enclosing_scope() {
    let lowered = lower_source(
        r#"
        fn choose(value: Option<string>) -> string {
            let .Some(text) = value else { return "missing"; };
            text
        }

        fn keep_text(value: string) {}
        fn main() {
            let original = Some("present");
            keep_text(choose(original));
            keep_text(choose(original));
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let choose = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "choose")
        .unwrap_or_else(|| panic!("choose must have a body: {:#?}", lowered.statuses));
    assert!(choose
        .blocks
        .iter()
        .any(|block| matches!(block.terminator, SemTerminator::SwitchVariant { .. })));
    assert!(choose.bindings.iter().any(|binding| binding.name == "text"));
}

#[test]
fn owning_if_expression_joins_independent_string_values() {
    let lowered = lower_source(
        r#"
        enum Setting { Small(bool); Missing }

        fn describe(value: Setting) -> string {
            match value {
                .Small(enabled) => if enabled { "small" } else { "disabled" },
                .Missing => "missing",
            }
        }

        fn discard(enabled: bool) {
            let ignored = if enabled { "unused" } else { "also unused" };
        }

        fn keep_text(value: string) {}
        fn main() {
            keep_text(describe(Setting.Small(true)));
            keep_text(describe(Setting.Small(false)));
            discard(true);
        }
        "#,
    );
    assert_main_lowered(&lowered);

    let describe = lowered
        .module
        .functions
        .iter()
        .find(|function| function.name == "describe")
        .unwrap_or_else(|| panic!("describe must have a body: {:#?}", lowered.statuses));
    assert!(describe.blocks.iter().any(|block| {
        block
            .args
            .iter()
            .any(|argument| argument.ty == ResolvedTy::String)
    }));
}
