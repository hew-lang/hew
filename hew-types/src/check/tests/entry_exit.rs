use super::*;

fn app_error_source(main_body: &str) -> String {
    format!(
        r"
        enum AppError {{ Failed(string); }}

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
        Some(&plan.entry),
        output.identity.declaration_by_path("main"),
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
        "Err(AppError.Failed(\"displayed failure\"))",
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
            builtin: Some(BuiltinType::Result),
            ..
        }
    ));
    assert_eq!(error_ty.user_facing().to_string(), "AppError");
    assert!(
        output
            .impl_method_declaration_ids
            .values()
            .any(|declaration| declaration == &display.declaration),
        "Display target must be one of the checker's resolved impl declarations"
    );
    assert!(display.type_args.is_empty());
}

#[test]
fn result_main_without_error_conformance_is_rejected() {
    let output = check_source(
        r#"
        enum NonError { Failed(string); }

        impl Display for NonError {
            fn fmt(self) -> string {
                match self { .Failed(message) => message }
            }
        }

        fn main() -> Result<(), NonError> {
            Err(NonError.Failed("not an Error"))
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
