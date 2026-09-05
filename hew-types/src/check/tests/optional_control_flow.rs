use super::{check_source, TypeErrorKind};

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
