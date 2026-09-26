use super::*;

fn check_selected_tests(source: &str, names: &[&str]) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let selections = names
        .iter()
        .map(|name| {
            let (index, (_, span)) = parsed
                .program
                .items
                .iter()
                .enumerate()
                .find(|(_, (item, _))| {
                    matches!(item, Item::Function(function) if function.name.name.as_str() == *name)
                })
                .expect("selected source function");
            crate::DeclarationOccurrence::new_with_synthetic_ordinal(
                None,
                span,
                index,
                crate::DeclarationKind::Function,
                0,
            )
        })
        .collect();
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.set_test_entry_selections(selections);
    checker.check_program(&parsed.program)
}

#[test]
fn selected_tests_publish_ordered_exit_plans_without_main() {
    let source = "fn first() {} fn second() -> i32 { 7 }";
    let output = check_selected_tests(source, &["second", "first"]);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output.entry_exit_plan.is_none());
    assert_eq!(output.test_entry_plans.len(), 2);
    assert_eq!(
        output.test_entry_plans[0].entry,
        output.defs.lookup_path("second").unwrap()
    );
    assert_eq!(
        output.test_entry_plans[0].action,
        EntryExitAction::Integer(EntryIntegerType::I32)
    );
    assert_eq!(
        output.test_entry_plans[1].entry,
        output.defs.lookup_path("first").unwrap()
    );
    assert_eq!(output.test_entry_plans[1].action, EntryExitAction::Unit);
}

#[test]
fn selected_result_test_carries_error_display_identity() {
    let source =
        app_error_source(".Err(AppError.Failed(\"failed\"))").replace("fn main", "fn result_case");
    let output = check_selected_tests(&source, &["result_case"]);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output.entry_exit_plan.is_none());
    let [plan] = output.test_entry_plans.as_slice() else {
        panic!("expected one selected test plan");
    };
    assert_eq!(plan.entry, output.defs.lookup_path("result_case").unwrap());
    assert!(matches!(plan.action, EntryExitAction::Result { .. }));
}

#[test]
fn selected_test_signature_failures_leave_no_partial_dispatch_plan() {
    for (source, names) in [
        ("fn good() {} fn bad(value: i64) {}", vec!["good", "bad"]),
        ("fn bad() -> string { \"bad\" }", vec!["bad"]),
        ("fn bad<T>() {}", vec!["bad"]),
        ("fn good() {}", vec!["good", "good"]),
    ] {
        let output = check_selected_tests(source, &names);
        assert!(output.entry_exit_plan.is_none());
        assert!(output.test_entry_plans.is_empty(), "{source}");
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind.as_kind_str() == "E_TEST_SIGNATURE"),
            "{source}: {:#?}",
            output.errors
        );
    }
}

#[test]
fn explicit_empty_test_selection_suppresses_implicit_main() {
    let output = check_selected_tests("fn main() {}", &[]);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output.entry_exit_plan.is_none());
    assert!(output.test_entry_plans.is_empty());
}

#[test]
fn selected_test_occurrence_must_belong_to_the_root_program() {
    let parsed = hew_parser::parse("fn main() {}");
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.set_test_entry_selections(vec![crate::DeclarationOccurrence::new(
        None,
        &(100..110),
        crate::DeclarationKind::Function,
        0,
    )]);
    let output = checker.check_program(&parsed.program);
    assert!(output.entry_exit_plan.is_none());
    assert!(output.test_entry_plans.is_empty());
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::TestSignature
                && error.message.contains("not a root function")
        }),
        "{:#?}",
        output.errors
    );
}

fn app_error_source(main_body: &str) -> String {
    format!(
        r"
        enum AppError {{ Failed(string), }}

        impl Display for AppError {{
            fn fmt(self) -> string {{
                match self {{ .Failed(message) => message }}
            }}
        }}

        impl Error for AppError {{}}

        fn main() -> Result<(), AppError> {{ {main_body} }}
        "
    )
}

#[test]
fn unit_main_produces_unit_entry_exit_plan() {
    let output = check_source("fn main() {}");
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );

    let plan = output
        .entry_exit_plan
        .expect("unit main must publish an exit plan");
    assert_eq!(
        Some(plan.entry),
        output.defs.lookup_path("main"),
        "the selected entry must be the checker-minted declaration identity"
    );
    assert_eq!(plan.action, EntryExitAction::Unit);
}

#[test]
fn integer_main_produces_typed_integer_entry_exit_plan() {
    let output = check_source("fn main() -> i64 { return 37; }");
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );

    let plan = output
        .entry_exit_plan
        .expect("integer main must publish an exit plan");
    assert_eq!(plan.action, EntryExitAction::Integer(EntryIntegerType::I64));
}

#[test]
fn result_main_carries_resolved_display_declaration() {
    let output = check_source(&app_error_source(
        ".Err(AppError.Failed(\"displayed failure\"))",
    ));
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );

    let plan = output
        .entry_exit_plan
        .as_ref()
        .expect("Result main must publish an exit plan");
    let EntryExitAction::Result {
        result_ty,
        error_ty,
        display,
    } = &plan.action
    else {
        panic!("expected Result exit plan, got {:?}", plan.action);
    };
    assert!(matches!(
        result_ty,
        ResolvedTy::Named {
            head: crate::TypeHead::Builtin(BuiltinType::Result),
            ..
        }
    ));
    assert_eq!(error_ty.user_facing().to_string(), "AppError");
    let EntryDisplayTarget::Declared {
        declaration,
        instance,
    } = display
    else {
        panic!("a concrete entry error type renders through a resolved declaration");
    };
    assert!(
        output
            .impl_method_declaration_ids
            .values()
            .any(|resolved| resolved == declaration),
        "Display target must be one of the checker's resolved impl declarations"
    );
    assert_eq!(instance, &EntryCallableInstance::Declared);
}

#[test]
fn result_main_without_error_conformance_is_rejected() {
    let output = check_source(
        r#"enum NonError {
    Failed(string);
}

impl Display for NonError {
    fn fmt(self) -> string {
        match self {
            .Failed(message) => message,
        }
    }
}

fn main() -> Result<(), NonError> {
    .Err(NonError.Failed("not an Error"))
}
"#,
    );

    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied
                && error.message.contains("NonError")
                && error.message.contains("Error")
        }),
        "missing Error conformance must be rejected: {:?}",
        output.errors
    );
    assert!(output.entry_exit_plan.is_none());
}
