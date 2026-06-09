use std::collections::HashMap;

use super::repl::{find_type_query_expr_type, ReplSession};
use super::session::Session;

#[test]
fn eval_type_command_preserves_numeric_int_identity() {
    let mut session = ReplSession::new();
    let result = session.eval(":type 42");

    assert!(!result.had_errors, "errors: {:?}", result.errors);
    assert_eq!(result.output, "i64\n");
}

#[test]
fn eval_type_command_uses_session_bound_variables() {
    let mut session = ReplSession::new();
    session.add_binding_for_test("var x = 10;");

    let result = session.eval(":type x");

    assert!(!result.had_errors, "errors: {:?}", result.errors);
    assert_eq!(result.output, "i64\n");
}

#[test]
fn eval_type_command_fails_closed_when_no_type_info_exists() {
    let session = Session::new();
    let source = session.build_type_query("42");
    let empty_expr_types = HashMap::new();

    assert!(find_type_query_expr_type(&source, &empty_expr_types).is_none());
}

#[test]
fn eval_type_command_reports_type_errors_not_unknown() {
    let mut session = ReplSession::new();
    let result = session.eval(":type missing_name_for_type_query");

    assert!(result.had_errors);
    assert!(result.output.is_empty());
    assert!(
        !result.errors.iter().any(|error| error == "unknown"),
        "errors: {:?}",
        result.errors
    );
}

#[test]
fn eval_generator_expression_displays_yield_and_return_types() {
    let mut session = ReplSession::new();

    let result = session.eval("gen { yield 1; yield 2; }");

    assert!(!result.had_errors, "errors: {:?}", result.errors);
    assert_eq!(result.output, "<generator yielding i64, returning ()>\n");
}

#[test]
fn eval_string_generator_expression_displays_yield_type() {
    let mut session = ReplSession::new();

    let result = session.eval(r#"gen { yield "x"; }"#);

    assert!(!result.had_errors, "errors: {:?}", result.errors);
    assert_eq!(result.output, "<generator yielding string, returning ()>\n");
}
