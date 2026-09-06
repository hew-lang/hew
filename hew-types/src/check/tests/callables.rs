use super::*;
use crate::{
    CallableCallMode, ClosureCaptureAccess, ClosureCaptureAcquisition, ClosureCaptureConsumption,
};

fn capture(output: &TypeCheckOutput, name: &str) -> ClosureCaptureFact {
    output
        .closure_capture_facts
        .values()
        .flatten()
        .find(|fact| fact.name == name)
        .expect("capture fact")
        .clone()
}

#[test]
fn private_counter_accepts_immutable_source_and_requires_mutable_callee() {
    let output = check_source("fn main() { let count: i64 = 0; var next = capture(var count) || { count = count + 1; count }; next(); next(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let fact = capture(&output, "count");
    assert_eq!(fact.acquisition, ClosureCaptureAcquisition::Snapshot);
    assert_eq!(fact.access, ClosureCaptureAccess::Var);
    assert_eq!(fact.consumption, ClosureCaptureConsumption::Retained);
    assert!(output
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Closure { capabilities, .. }
        if capabilities.call == CallableCallMode::Var && capabilities.clone)));
    let output = check_source("fn main() { let count: i64 = 0; let next = capture(var count) || { count = count + 1; count }; next(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::MutabilityError),
        "{:?}",
        output.errors
    );
}

#[test]
fn implicit_capture_mutation_names_the_private_prefix() {
    for source in [
        "fn main() { var n: i64 = 0; let f = || { n = n + 1; }; }",
        "fn main() { var xs: Vec<i32> = Vec.new(); let f = || xs.push(1); }",
    ] {
        let output = check_source(source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains("capture(var ")),
            "{:?}",
            output.errors
        );
    }
}

#[test]
fn private_prefix_resolves_existing_binding_and_respects_body_shadowing() {
    let output = check_source("fn main() { let count: i64 = 0; let f = capture(var count) || { var count: i64 = 2; count = 3; count }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.closure_capture_facts.values().all(Vec::is_empty));
    let output = check_source("fn main() { let f = capture(var missing) || 1; }");
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::UndefinedVariable));
}

#[test]
fn move_snapshot_acquisition_does_not_require_call_once() {
    let output = check_source("fn main() { let text = \"hew\"; let f = move || text; f(); f(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let fact = capture(&output, "text");
    assert_eq!(fact.acquisition, ClosureCaptureAcquisition::Move);
    assert_eq!(fact.consumption, ClosureCaptureConsumption::Retained);
    assert!(output
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Closure { capabilities, .. }
        if capabilities.call == CallableCallMode::Read && capabilities.clone)));
}

#[test]
fn nested_closure_acquisition_is_a_capture_of_the_enclosing_closure() {
    let output = check_source(
        "fn main() { let text = \"hew\"; let outer = || { let inner = || text; inner }; }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let facts: Vec<_> = output
        .closure_capture_facts
        .values()
        .flatten()
        .filter(|fact| fact.name == "text")
        .collect();
    assert_eq!(facts.len(), 2);
    assert_eq!(facts[0].binding_id, facts[1].binding_id);
}

#[test]
fn call_once_parameter_is_consumed_and_field_consumption_is_independent() {
    let output = check_source("fn use_once(f: fn[once]() -> i64) { f(); f(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
    let output = check_source("type Callbacks { first: fn[once]() -> i64, second: fn[once]() -> i64 } fn invoke(pair: Callbacks) { (pair.first)(); (pair.second)(); (pair.first)(); }");
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::UseAfterMove | TypeErrorKind::UseAfterConsume
        )),
        "{:?}",
        output.errors
    );
}

#[test]
fn zero_argument_function_item_remains_a_callable_value() {
    let output =
        check_source("fn value() -> i64 { 7 } fn main() { let f = value; let n: i64 = f(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Function { capabilities, params, ret }
        if capabilities.clone && params.is_empty() && **ret == Ty::I64)));
}

#[test]
fn resource_capture_requires_move_and_body_consumption_requires_once() {
    let declarations = "#[resource] type Socket { fd: i64 } impl Socket { fn close(consuming self) {} fn take(consuming self) -> i64 { self.fd } }";
    let output = check_source(&format!(
        "{declarations} fn main() {{ let socket = Socket {{ fd: 7 }}; let f = || socket.fd; }}"
    ));
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::ClosureExplicitMoveRequired { .. }
        )),
        "{:?}",
        output.errors
    );
    let output = check_source(&format!("{declarations} fn main() {{ let socket = Socket {{ fd: 7 }}; let f = move || socket.fd; f(); f(); }}"));
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Closure { capabilities, .. }
        if capabilities.call == CallableCallMode::Read && !capabilities.clone)));
    let output = check_source(&format!("{declarations} fn main() {{ let socket = Socket {{ fd: 7 }}; let f = move || socket.take(); f(); f(); }}"));
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
    assert_eq!(
        capture(&output, "socket").consumption,
        ClosureCaptureConsumption::Consumed
    );
}

#[test]
fn consumption_in_a_diverging_arm_still_requires_once() {
    let output = check_source("#[resource] type Socket { fd: i64 } impl Socket { fn close(consuming self) {} } fn main() { let socket = Socket { fd: 7 }; let f = move |finish: bool| { if finish { socket.close(); return; } }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        capture(&output, "socket").consumption,
        ClosureCaptureConsumption::Consumed
    );
    assert!(output
        .expr_types
        .values()
        .any(|ty| matches!(ty, Ty::Closure { capabilities, .. }
        if capabilities.call == CallableCallMode::Once && !capabilities.clone)));
}

#[test]
fn callable_coercions_only_weaken_guarantees() {
    let modes = [
        "",
        "[clone]",
        "[var]",
        "[var, clone]",
        "[once]",
        "[once, clone]",
    ];
    for (from, actual) in modes.iter().enumerate() {
        for (to, expected) in modes.iter().enumerate() {
            let source = format!("fn accept(f: fn{expected}() -> i64) {{}} fn forward(f: fn{actual}() -> i64) {{ accept(f); }}");
            let output = check_source(&source);
            let accepted = from / 2 <= to / 2 && (to % 2 == 0 || from % 2 == 1);
            assert_eq!(
                output.errors.is_empty(),
                accepted,
                "{source}: {:?}",
                output.errors
            );
        }
    }
}

#[test]
fn callable_parameter_and_result_signatures_remain_invariant() {
    for (expected, actual) in [
        (
            "fn(fn[var]() -> i64) -> i64",
            "fn(fn[once]() -> i64) -> i64",
        ),
        ("fn() -> fn() -> i64", "fn() -> fn[clone]() -> i64"),
        ("fn(i64) -> i64", "fn(i32) -> i64"),
    ] {
        let source =
            format!("fn accept(f: {expected}) {{}} fn forward(f: {actual}) {{ accept(f); }}");
        let output = check_source(&source);
        assert!(!output.errors.is_empty(), "accepted {source}");
    }
}

#[test]
fn explicit_erasure_controls_later_calls_and_clone() {
    let output = check_source("fn main() { let f: fn[once]() -> i64 = || 1; f(); f(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
    let output = check_source("fn main() { let f: fn() -> i64 = || 1; let duplicate = clone f; }");
    assert!(
        !output.errors.is_empty(),
        "plain fn must not regain Clone through its initializer"
    );
    let output = check_source("fn main() { let f: fn[clone]() -> i64 = || 1; let duplicate = clone f; f(); duplicate(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn lambda_arguments_cannot_hide_private_mutation() {
    let output = check_source("fn accept(f: fn() -> i64) {} fn main() { let n: i64 = 0; accept(capture(var n) || { n = n + 1; n }); }");
    assert!(
        !output.errors.is_empty(),
        "a var closure cannot satisfy a read-only function parameter"
    );
}

#[test]
fn callable_joins_forget_guarantees_independently_of_arm_order() {
    for choice in [
        "if flag { a } else { b }",
        "if flag { b } else { a }",
        "match flag { true => a, false => b }",
        "match flag { true => b, false => a }",
    ] {
        let source = format!("fn choose_callable(a: fn() -> i64, b: fn[once, clone]() -> i64, flag: bool) {{ let f = {choice}; f(); f(); }}");
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UseAfterMove),
            "{source}: {:?}",
            output.errors
        );
        assert!(output
            .expr_types
            .values()
            .any(|ty| matches!(ty, Ty::Function { capabilities, .. }
            if capabilities.call == CallableCallMode::Once && !capabilities.clone)));
    }
}

#[test]
fn callable_qualifiers_survive_aggregate_erasure() {
    let output = check_source(
        "fn main() { let pair: (fn[once]() -> i64, i64) = (|| 1, 0); (pair.0)(); (pair.0)(); }",
    );
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::UseAfterMove | TypeErrorKind::UseAfterConsume
        )),
        "{:?}",
        output.errors
    );
    for (expected, actual, accepted) in [
        (
            "Option<fn[once]() -> i64>",
            "Option<fn[clone]() -> i64>",
            true,
        ),
        ("Option<fn[clone]() -> i64>", "Option<fn() -> i64>", false),
        (
            "Result<fn[once]() -> i64, string>",
            "Result<fn[clone]() -> i64, string>",
            true,
        ),
    ] {
        let source = format!(
            "fn accept(value: {expected}) {{}} fn forward(value: {actual}) {{ accept(value); }}"
        );
        let output = check_source(&source);
        assert_eq!(
            output.errors.is_empty(),
            accepted,
            "{source}: {:?}",
            output.errors
        );
    }
}

#[test]
fn method_style_callable_field_invocation_checks_the_selected_place() {
    let declarations = "type Callbacks { first: fn[once]() -> i64, second: fn[once]() -> i64 }";
    let output = check_source(&format!(
        "{declarations} fn invoke(pair: Callbacks) {{ pair.first(); pair.second(); }}"
    ));
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source(&format!(
        "{declarations} fn invoke(pair: Callbacks) {{ pair.first(); pair.first(); }}"
    ));
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::UseAfterMove | TypeErrorKind::UseAfterConsume
        )),
        "{:?}",
        output.errors
    );
    let output = check_source(
        "type Counter { next: fn[var]() -> i64 } fn invoke(counter: Counter) { counter.next(); }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::MutabilityError),
        "{:?}",
        output.errors
    );
}

#[test]
fn callable_erasure_cannot_discard_linear_capture_obligations() {
    let output = check_source("#[linear] type Ticket { value: i64 } impl Ticket { fn finish(consuming self) -> i64 { self.value } } fn erase(ticket: Ticket) -> fn[once]() -> i64 { move || ticket.finish() }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("linear ownership obligation")),
        "{:?}",
        output.errors
    );
}

#[test]
fn transferring_a_captured_owner_requires_once() {
    let declarations =
        "#[resource] type Socket { fd: i64 } impl Socket { fn close(consuming self) {} } type Holder { socket: Socket } enum Envelope { Owned(Socket) }";
    for body in [
        "{ Holder { socket: socket } }",
        "Envelope.Owned(socket)",
        "socket",
        "{ socket }",
        "{ return socket; }",
        "{ let owned = socket; owned }",
        "if flag { socket } else { socket }",
        "match flag { true => socket, false => socket }",
        "(socket, 7)",
    ] {
        let source = format!("{declarations} fn main() {{ let socket = Socket {{ fd: 7 }}; let f = move |flag: bool| {body}; let owned = f(true); }}");
        let output = check_source(&source);
        assert!(output.errors.is_empty(), "{body}: {:?}", output.errors);
        assert_eq!(
            capture(&output, "socket").consumption,
            ClosureCaptureConsumption::Consumed,
            "{body}"
        );
        assert!(output.expr_types.values().any(|ty| matches!(ty, Ty::Closure { capabilities, .. } if capabilities.call == CallableCallMode::Once && !capabilities.clone)), "{body}");
    }
}

#[test]
fn moving_an_erased_callable_prevents_reuse() {
    let output = check_source("fn invoke(f: fn() -> i64) { let other = f; f(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
    let output = check_source("fn invoke(f: fn[clone]() -> i64) { let other = f; f(); other(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output =
        check_source("fn wrap(f: fn() -> i64) { let outer = move || f; outer(); outer(); }");
    assert_eq!(
        capture(&output, "f").consumption,
        ClosureCaptureConsumption::Consumed
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
}

#[test]
fn mutable_binding_erasure_preserves_the_initializer_capabilities() {
    let source = "fn main() { let n: i64 = 7; var f: fn[once]() -> i64 = || n; f(); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let literal = source.find("|| n").unwrap();
    assert!(output.expr_types.iter().any(|(span, ty)| span.start == literal && matches!(ty, Ty::Closure { capabilities, .. } if capabilities.call == CallableCallMode::Read && capabilities.clone)));
}

#[test]
fn inferred_lambda_returns_join_callable_guarantees() {
    for returns in [
        "if flag { return a; } return b;",
        "if flag { return b; } return a;",
        "if flag { return a; } b",
        "if flag { return b; } a",
    ] {
        let source = format!("fn choose(a: fn[clone]() -> i64, b: fn[once, clone]() -> i64) {{ let f = move |flag: bool| {{ {returns} }}; let result = f(true); result(); }}");
        let output = check_source(&source);
        assert!(output.errors.is_empty(), "{returns}: {:?}", output.errors);
        assert!(output.expr_types.values().any(|ty| matches!(ty, Ty::Closure { ret, .. } if matches!(&**ret, Ty::Function { capabilities, .. } if capabilities.call == CallableCallMode::Once && capabilities.clone))), "{returns}");
    }
    let output = check_source("fn main() { let f = |flag: bool| { if flag { return; } 7 }; }");
    assert!(
        !output.errors.is_empty(),
        "unit and value returns must disagree"
    );
}

#[test]
fn capture_syntax_composes_with_conditional_result_and_option_types() {
    for body in ["if flag { .Ok(.Some(capture(var count) || { count = count + 1; count })) } else { .Ok(.None) }", "match flag { true => .Ok(.Some(capture(var count) || { count = count + 1; count })), false => .Ok(.None) }"] {
        let source = format!("fn choose(flag: bool) -> Result<Option<fn[var, clone]() -> i64>, string> {{ let count: i64 = 0; {body} }}");
        let output = check_source(&source);
        assert!(output.errors.is_empty(), "{body}: {:?}", output.errors);
        assert_eq!(capture(&output, "count").access, ClosureCaptureAccess::Var);
        assert!(output.expr_types.values().any(|ty| matches!(ty, Ty::Closure { capabilities, .. } if capabilities.call == CallableCallMode::Var && capabilities.clone)));
    }
}

#[test]
fn contextual_builtin_composition_keeps_shape_and_payload_errors() {
    for source in [
        "fn make() -> Option<i64> { .None(7) }",
        "fn make() -> Result<i64, string> { .Ok }",
        "fn make() -> Result<i64, string> { .Ok(true) }",
        "fn make() -> Option<i64> { .Missing }",
    ] {
        let output = check_source(source);
        assert!(!output.errors.is_empty(), "accepted {source}");
    }
    let output = check_source(
        r#"enum Outcome { Some(string), None } fn make() -> Outcome { .Some("hew") }"#,
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}
