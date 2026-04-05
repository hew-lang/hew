use super::repl::ReplSession;

#[test]
fn eval_type_command_preserves_numeric_int_identity() {
    let mut session = ReplSession::new();
    let result = session.eval(":type 42");

    assert!(!result.had_errors, "errors: {:?}", result.errors);
    assert_eq!(result.output, "int\n");
}
