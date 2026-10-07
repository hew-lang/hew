use super::{check_source, TypeErrorKind};

#[test]
fn fallible_returns_accept_exact_success_and_explicit_error() {
    for source in [
        "fn f() -> (i64, string) fails string { return (10, \"hello\"); }",
        "fn f() -> i64 fails string { return error \"missing\"; }",
        "fn f() -> i64 fails string { 7 }",
        "fn f() fails string { return; }",
        "fn f() fails string { }",
        "fn f(value: Result<i64, string>) -> Result<i64, string> fails bool { return value; }",
        "fn f(value: Result<i64, string>) -> Result<i64, string> fails bool { value }",
        "fn f(value: Result<i64, string>) -> i64 fails string { value? }",
    ] {
        let checked = check_source(source);
        assert!(checked.errors.is_empty(), "{source}: {:?}", checked.errors);
    }
}

#[test]
fn fallible_returns_reject_wrong_boundary_and_nested_context() {
    for source in [
        "fn f() -> i64 fails string { return error 7; }",
        "fn f() -> i64 fails string { return \"wrong\"; }",
        "fn f(value: Result<i64, string>) -> i64 fails string { return value; }",
        "fn f(value: Result<i64, string>) -> i64 fails string { value }",
        "fn f() -> Result<i64, string> { return error \"wrong\"; }",
        "fn f() -> i64 fails string { let inner = || -> i64 { return error \"wrong\"; }; 7 }",
    ] {
        let checked = check_source(source);
        assert!(
            !checked.errors.is_empty(),
            "{source}: invalid error boundary accepted"
        );
    }
}

#[test]
fn fallible_receive_completion_carries_its_declared_failure() {
    let source = "actor Worker { receive fn read() -> i64 fails string { 7 } } \
         fn main() { let w = spawn Worker(); let _ = w.read(); }";
    let checked = check_source(source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let start = source.find("w.read()").expect("call site");
    let call = checked
        .expr_types
        .get(&crate::check::SpanKey::in_module(
            &(start..start + "w.read()".len()),
            0,
        ))
        .expect("completion call type");
    let (success, failure) = call.as_result().expect("completion result");
    assert_eq!(success, &crate::Ty::I64, "{call:?}");
    // `fails string` becomes the envelope's `Failed(string)`; the handler's own
    // `Result` never reaches the caller as a nested value.
    let crate::Ty::Named {
        head: name_head,
        args,
        ..
    } = failure
    else {
        panic!("completion error is not nominal: {failure:?}");
    };
    assert_eq!(name_head.spelling(), crate::KnownDecl::ActorError.path());
    assert_eq!(args[0], crate::Ty::String, "{failure:?}");
}

#[test]
fn fallible_method_bodies_share_function_return_rules() {
    for source in [
        "type Reader {} impl Reader { fn read() -> i64 fails string { return 7; } }",
        "trait Read { fn read(self) -> i64 fails string { return 7; } }",
        "actor Reader { fn read() -> i64 fails string { return 7; } }",
    ] {
        let checked = check_source(source);
        assert!(checked.errors.is_empty(), "{source}: {:?}", checked.errors);
    }
}

#[test]
fn fallible_default_unit_returns_retain_distinct_source_clause_identities() {
    let source = "trait First { fn finish(self) fails string {} } trait Second { fn finish(self) fails string {} }";
    let checked = check_source(source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let parsed = hew_parser::parse(source);
    let mut clauses = Vec::new();
    for (item, _) in &parsed.program.items {
        let hew_parser::ast::Item::Trait(declaration) = item else {
            panic!("trait");
        };
        let hew_parser::ast::TraitItem::Method(method) = &declaration.items[0] else {
            panic!("method");
        };
        clauses.push(
            method
                .return_type
                .as_ref()
                .expect("fallible clause")
                .1
                .clone(),
        );
    }
    assert_ne!(clauses[0], clauses[1]);
    assert_eq!(checked.result_return_coercions.len(), clauses.len());
    for clause in clauses {
        assert_eq!(
            checked
                .result_return_coercions
                .get(&super::SpanKey::in_module(&clause, 0)),
            Some(&super::ResultReturnKind::Success)
        );
    }
}

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
    let checked = check_source("fn f(value: Result<i64, string>, fallback: Option<i64>) -> Result<i64, string> { let number = value handle problem { fallback? }; .Ok(number) }");
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
        "fn f(value: Option<i64>) -> i64 fails string { value? }",
        "fn f(value: Result<i64, string>) -> Option<i64> { .Some(value?) }",
    ] {
        let checked = check_source(source);
        assert!(
            checked.errors.iter().any(|error| matches!(
                error.kind,
                TypeErrorKind::InvalidOperation | TypeErrorKind::NoFailureEdge
            )),
            "{source}: {:?}",
            checked.errors
        );
    }
}

#[test]
fn propagation_accepts_same_container_with_different_success_type() {
    for source in [
        "fn f(value: Option<i64>) -> Option<string> { value?; .Some(\"done\") }",
        "fn f(value: Result<i64, string>) -> bool fails string { value?; true }",
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

#[test]
fn an_inferred_closure_opens_its_edge_only_at_a_result_exit() {
    // A `?` on an `Option` opens no edge: the closure returns its `Option`.
    for source in [
        "fn wrap(v: i64) -> Option<i64> { .Some(v) } \
         fn f(xs: Vec<Option<i64>>) -> Vec<Option<i64>> { xs.map(|o| wrap(o? + 1)) }",
        "fn wrap(v: i64) -> Option<i64> { .Some(v) } \
         fn f(o: Option<i64>) -> Option<i64> { let g = |o: Option<i64>| wrap(o? * 3); g(o) }",
        // A `return v` before the first `Result` exit is the success.
        "fn parse(t: string) -> i64 fails string { 1 } \
         fn f() -> Result<i64, string> { let g = |t: string| { if t == \"r\" { return 7; } parse(t)? }; g(\"x\") }",
    ] {
        let checked = check_source(source);
        assert!(checked.errors.is_empty(), "{source}: {:?}", checked.errors);
    }
}

#[test]
fn a_failing_closure_refuses_absence_and_a_nested_result_tail() {
    for (source, message) in [
        (
            "fn parse(t: string) -> i64 fails string { 1 } \
             fn f() { let g = |t: string, o: Option<i64>| { let n = parse(t)?; n + o? }; let _ = g(\"a\", .None); }",
            "`?` cannot propagate absence here",
        ),
        (
            "fn parse(t: string) -> i64 fails string { 1 } \
             fn f() { let g = |o: Option<i64>, t: string| { let x = o?; x + parse(t)? }; let _ = g(.None, \"a\"); }",
            "`?` cannot propagate absence here",
        ),
        (
            "fn a(t: string) -> Result<i64, string> { .Ok(1) } \
             fn f() { let g = |t: string| { let _ = a(t)?; a(t) }; let _ = g(\"a\"); }",
            "would be returned as data inside another `Result`",
        ),
    ] {
        let checked = check_source(source);
        assert!(
            checked
                .errors
                .iter()
                .any(|error| error.message.contains(message)),
            "{source}: {:?}",
            checked.errors
        );
    }
}

#[test]
fn a_missing_edge_fix_it_is_a_clause_that_compiles() {
    for (source, clause) in [
        (
            "fn p(t: string) -> i64 fails string { 1 } \
             fn mk(t: string) -> Result<fn(i64) -> i64, string> { let k = p(t)?; .Ok(|n: i64| n * k) }",
            "`-> (fn(i64) -> i64) fails string`",
        ),
        (
            "fn d() -> Result<i64, i64> { return error \"x\"; }",
            "`-> i64 fails string`",
        ),
    ] {
        let checked = check_source(source);
        let error = checked
            .errors
            .iter()
            .find(|error| error.kind == TypeErrorKind::NoFailureEdge)
            .unwrap_or_else(|| panic!("{source}: {:?}", checked.errors));
        assert!(
            error.suggestions.iter().any(|hint| hint.contains(clause)),
            "{source}: {:?}",
            error.suggestions
        );
    }
    let checked = check_source(
        "fn p(t: string) -> i64 fails string { 1 } \
         fn f() -> i64 fails string { let g = gen { yield p(\"x\")?; }; for v in g { return v; } 0 }",
    );
    let error = checked
        .errors
        .iter()
        .find(|error| error.kind == TypeErrorKind::NoFailureEdge)
        .unwrap_or_else(|| panic!("{:?}", checked.errors));
    assert!(
        error.message.contains("`gen {}` block has none"),
        "{error:?}"
    );
    assert!(
        error.suggestions.iter().all(|hint| !hint.contains("fails")),
        "{:?}",
        error.suggestions
    );
}
