use super::{check_source, TypeErrorKind};

#[test]
fn lazy_default_accepts_payload_and_rejects_error_swallowing() {
    let checked = check_source("fn f(value: Option<i64>) -> i64 { value ?? 7 }");
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let checked = check_source("fn f(value: Result<i64, string>) -> i64 { value ?? 7 }");
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::InvalidOperation)),
        "{:?}",
        checked.errors
    );
}

#[test]
fn lazy_default_rejects_wrong_payload_type() {
    let checked = check_source("fn f(value: Option<i64>) -> i64 { value ?? true }");
    assert!(
        !checked.errors.is_empty(),
        "default must have the Option payload type"
    );
}

#[test]
fn local_handler_binds_error_for_ordinary_calls() {
    let checked = check_source("fn recover(problem: string) -> i64 { 7 } fn f(value: Result<i64, string>) -> i64 { value handle problem { recover(problem) } }");
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn local_handler_accepts_lexical_return_and_mutation() {
    let checked = check_source("fn f(value: Result<i64, string>) -> i64 { var count = 0; let number = value handle problem { count = 2; return count; }; number + count }");
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn local_handler_rejects_absence_and_wrong_branch_type() {
    for source in [
        "fn f(value: Option<i64>) -> i64 { value handle problem { 7 } }",
        "fn f(value: Result<i64, string>) -> i64 { value handle problem { true } }",
    ] {
        let checked = check_source(source);
        assert!(
            !checked.errors.is_empty(),
            "{source}: invalid recovery must be rejected"
        );
    }
}

#[test]
fn local_handler_binding_does_not_escape() {
    let checked = check_source(
        "fn f(value: Result<i64, string>) -> string { value handle problem { 7 }; problem }",
    );
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::UndefinedVariable)),
        "{:?}",
        checked.errors
    );
}

#[test]
fn local_handler_keeps_loop_control_lexical() {
    let checked = check_source("fn f(value: Result<i64, string>) { for i in 0..3 { let number = value handle problem { continue; }; } }");
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn local_handler_question_mark_uses_the_enclosing_return_type() {
    let checked = check_source("fn f(value: Result<i64, string>, fallback: Option<i64>) -> Result<i64, string> { let number = value handle problem { fallback? }; Ok(number) }");
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::InvalidOperation)),
        "{:?}",
        checked.errors
    );
}

#[test]
fn propagation_rejects_absence_error_conflation() {
    for source in [
        "fn f(value: Option<i64>) -> Result<i64, string> { Ok(value?) }",
        "fn f(value: Result<i64, string>) -> Option<i64> { Some(value?) }",
    ] {
        let checked = check_source(source);
        assert!(
            checked
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::InvalidOperation)),
            "{source}: {:?}",
            checked.errors
        );
    }
}

#[test]
fn propagation_accepts_same_container_with_different_success_type() {
    for source in [
        "fn f(value: Option<i64>) -> Option<string> { value?; Some(\"done\") }",
        "fn f(value: Result<i64, string>) -> Result<bool, string> { value?; Ok(true) }",
    ] {
        let checked = check_source(source);
        assert!(checked.errors.is_empty(), "{source}: {:?}", checked.errors);
    }
}

#[test]
fn required_optional_binding_exposes_payload_after_divergent_else() {
    let checked = check_source(
        "fn f(value: Option<i64>) -> i64 { let number = value else { return 0; }; number + 1 }",
    );
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn required_optional_binding_rejects_fallthrough_else() {
    let checked = check_source("fn f(value: Option<i64>) { let number = value else { 0 }; }");
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::LetElseDoesNotDiverge)),
        "{:?}",
        checked.errors
    );
}

#[test]
fn plain_optional_binding_keeps_option_type() {
    let checked =
        check_source("fn f(value: Option<i64>) -> Option<i64> { let number = value; number }");
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn required_optional_annotation_describes_payload() {
    let checked = check_source(
        "fn f(value: Option<i64>) -> i64 { let number: i64 = value else { return 0; }; number }",
    );
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn required_optional_binding_rejects_result() {
    let checked = check_source(
        "fn f(value: Result<i64, string>) -> i64 { let number = value else { return 0; }; number }",
    );
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::InvalidOperation)),
        "{:?}",
        checked.errors
    );
}

#[test]
fn required_optional_failure_arm_cannot_see_success_binding() {
    let checked = check_source(
        "fn f(value: Option<i64>) -> i64 { let number = value else { return number; }; number }",
    );
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::UndefinedVariable)),
        "success binding must not exist on the absence path"
    );
}
