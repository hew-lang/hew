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
fn once_invocation_requires_an_owned_parameter() {
    for qualifier in ["once", "once, clone"] {
        let output = check_source(&format!(
            "fn invoke(f: fn[{qualifier}]() -> i64) {{ f(); }}"
        ));
        let error = output
            .errors
            .iter()
            .find(|error| error.message.contains("E_OWN_CONSUME_BORROWED"))
            .expect("borrowed once diagnostic");
        assert_eq!(error.kind, TypeErrorKind::OwnConsumeBorrowed);
        assert!(error
            .suggestions
            .iter()
            .any(|text| text.contains("consume f:")));
        let output = check_source(&format!(
            "fn invoke(consume f: fn[{qualifier}]() -> i64) {{ f(); }}"
        ));
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let output = check_source(&format!(
            "fn invoke(consume f: fn[{qualifier}]() -> i64) {{ f(); f(); }}"
        ));
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UseAfterMove),
            "{:?}",
            output.errors
        );
    }
}

#[test]
fn ordinary_callable_arguments_remain_borrowed_without_clone() {
    for ty in ["fn() -> i64", "fn[once]() -> i64", "fn[clone]() -> i64"] {
        let output = check_source(&format!(
            "fn inspect(f: {ty}) {{}} fn forward(f: {ty}) {{ inspect(f); inspect(f); }}"
        ));
        assert!(output.errors.is_empty(), "{:?}", output.errors);
    }
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
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
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
fn once_callable_fields_allow_independent_owned_use() {
    for qualifier in ["once", "once, clone"] {
        let declarations = format!("type Callbacks {{ first: fn[{qualifier}]() -> i64, second: fn[{qualifier}]() -> i64 }}");
        for invocation in ["pair.first()", "(pair.first)()"] {
            let output = check_source(&format!(
                "{declarations} fn invoke(consume pair: Callbacks) {{ {invocation}; }}"
            ));
            assert!(output.errors.is_empty(), "{:?}", output.errors);
        }
        let output = check_source(&format!("{declarations} fn invoke(consume pair: Callbacks) {{ let Callbacks {{ first, second }} = pair; first(); second(); }}"));
        assert!(output.errors.is_empty(), "{:?}", output.errors);
    }
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

const PARTIAL_JOB: &str = "type Job { done: fn[once]() -> i64, label: string }
    fn new_job() -> Job { Job { done: || 7, label: \"ready\" } }
    fn inspect(job: Job) { println(job.label); }";

#[test]
fn partial_move_complete_job_keeps_siblings_usable() {
    let output = check_source(&format!(
        "{PARTIAL_JOB}
        fn complete_job(consume job: Job) -> i64 {{
            let result = job.done(); println(job.label); result
        }}
        fn main() {{ println(complete_job(new_job())); }}"
    ));
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source(
        "type Pair { left: fn[once]() -> i64, right: fn[once]() -> i64 }
        fn complete(consume pair: Pair) -> i64 { pair.left() + pair.right() }
        fn main() { println(complete(Pair { left: || 1, right: || 2 })); }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn partial_move_nested_records_and_tuples_preserve_siblings() {
    let output = check_source(&format!(
        "{PARTIAL_JOB}
        type Batch {{ pair: (Job, fn[once]() -> i64), label: string }}
        fn complete(consume batch: Batch) {{
            batch.pair.0.done(); println(batch.pair.0.label);
            batch.pair.1(); println(batch.label);
        }}
        fn main() {{ complete(Batch {{ pair: (new_job(), || 9), label: \"batch\" }}); }}"
    ));
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for body in [
        "let inner = batch.job; batch.job.done();",
        "complete_job(batch.job); batch.job.done();",
    ] {
        let output = check_source(&format!(
            "{PARTIAL_JOB}
            type Batch {{ job: Job, label: string }}
            fn complete_job(consume job: Job) {{ job.done(); }}
            fn complete(consume batch: Batch) {{ {body} }}"
        ));
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UseAfterMove),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn partial_move_whole_and_selected_use_require_reinitialization() {
    for body in [
        "job.done(); job.done();",
        "job.done(); inspect(job);",
        "if flag { job.done(); } inspect(job);",
        "if flag { job.done(); } job.done();",
        "job.done(); if flag { job.done = || 8; } inspect(job);",
        "job.done(); for i in 0..n { job.done = || 8; } inspect(job);",
        "for i in 0..n { if flag { job.done(); break; } job.done = || 8; } inspect(job);",
    ] {
        let output = check_source(&format!(
            "{PARTIAL_JOB}
            fn complete(consume var job: Job, flag: bool, n: i64) {{ {body} }}"
        ));
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UseAfterMove),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn partial_move_reinitialization_restores_whole_use() {
    for body in [
        "job.done(); job.done = || 8; inspect(job); job.done();",
        "job.done(); job = new_job(); inspect(job); job.done();",
        "job.done(); if flag { job.done = || 8; } else { job.done = || 9; } inspect(job);",
        "for i in 0..n { job.done(); println(job.label); job.done = || 8; } inspect(job);",
        "for i in 0..n { if flag { job.done(); job.done = || 8; continue; } job.done(); job.done = || 9; } inspect(job);",
    ] {
        let output = check_source(&format!(
            "{PARTIAL_JOB}
            fn complete(consume var job: Job, flag: bool, n: i64) {{ {body} }}"
        ));
        assert!(output.errors.is_empty(), "{body}: {:?}", output.errors);
    }
}

#[test]
fn partial_move_borrowed_parameters_and_capture_acquisitions_are_rejected() {
    for body in [
        "job.done();",
        "let done = job.done; done();",
        "let complete = || job.done(); complete();",
        "let complete = move || job.done(); complete();",
    ] {
        let output = check_source(&format!("{PARTIAL_JOB} fn complete(job: Job) {{ {body} }}"));
        assert!(
            output.errors.iter().any(|error| matches!(
                error.kind,
                TypeErrorKind::OwnConsumeBorrowed
                    | TypeErrorKind::ClosureExplicitMoveRequired { .. }
            )),
            "{body}: {:?}",
            output.errors
        );
    }
    let output = check_source(&format!("{PARTIAL_JOB}
        fn complete(consume job: Job) {{ let finish = move || {{ job.done(); println(job.label); }}; finish(); }}"));
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn partial_move_custom_cleanup_ancestors_must_remain_whole() {
    for prefix in ["#[resource]", "#[linear]"] {
        let declarations = format!(
            "{prefix} type Bundle {{ done: fn[once]() -> i64 }}
            impl Bundle {{ fn close(consuming self) {{ }} }}
            type Outer {{ inner: (Bundle, string) }}"
        );
        for body in [
            "outer.inner.0.done();",
            "let done = outer.inner.0.done; done();",
        ] {
            let output = check_source(&format!(
                "{declarations}
                fn complete(consume outer: Outer) {{ {body} }}"
            ));
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::OwnPartialConsume),
                "{prefix} {body}: {:?}",
                output.errors
            );
        }
        let output = check_source(&format!("{declarations}
            fn complete(consume outer: Outer) {{ let inner = outer.inner.0; inner.close(); println(outer.inner.1); }}"));
        assert!(output.errors.is_empty(), "{prefix}: {:?}", output.errors);
        let output = check_source(&format!("{declarations}
            fn complete(consume outer: Outer) {{ let inner = outer.inner.0; let again = outer.inner.0; }}"));
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UseAfterMove),
            "{prefix}: {:?}",
            output.errors
        );
    }
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
    let output = check_source("fn invoke(consume f: fn() -> i64) { let other = f; f(); }");
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
    let output = check_source(
        "fn wrap(consume f: fn() -> i64) { let outer = move || f; outer(); outer(); }",
    );
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
        let source = format!("fn choose(consume a: fn[clone]() -> i64, consume b: fn[once, clone]() -> i64) {{ let f = move |flag: bool| {{ {returns} }}; let result = f(true); result(); }}");
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

#[test]
fn generic_function_values_instantiate_each_reference() {
    for source in [
        "fn id<T>(x: T) -> T { x } fn main() { let f: fn[clone](i64) -> i64 = id; let g: fn(string) -> string = id; f(4); g(\"hew\"); }",
        "fn id<T>(x: T) -> T { x } fn main() { let f = id<i64>; f(4); }",
        "fn id<T>(x: T) -> T { x } fn main() { let f = id; f(4); }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
        assert!(output.call_type_args.values().any(|args| args == &[Ty::I64]));
        assert!(output.direct_call_targets.values().any(|target|
            matches!(target, crate::CallTarget::User(declaration) if declaration.full_path() == "id")));
    }
}

#[test]
fn generic_function_values_enforce_explicit_arity_and_inferred_bounds() {
    for source in [
        "fn id<T>(x: T) -> T { x } fn main() { let f = id<i64, string>; }",
        "fn id(x: i64) -> i64 { x } fn main() { let f = id<i64>; }",
        "type Holder { call: fn(i64) -> i64 } fn bad(holder: Holder) { let f = holder.call<i64>; }",
        "fn id<T>(x: T) -> T { x } fn main() { let f = id<i64>; let g = f<string>; }",
        "trait Allowed { fn ok(self) -> bool; } fn id<T: Allowed>(x: T) -> T { x } fn main() { let f: fn(i64) -> i64 = id; }",
        "trait Allowed { fn ok(self) -> bool; } fn id<T: Allowed>(x: T) -> T { x } fn main() { let f = id<i64>; }",
    ] {
        let output = check_source(source);
        assert!(!output.errors.is_empty(), "accepted invalid function value: {source}");
    }
}

#[test]
fn consuming_function_items_cannot_erase_parameter_ownership() {
    for source in [
        "fn invoke(consume f: fn[once]() -> i64) { f(); } fn main() { let erased = invoke; }",
        "fn take<T>(consume value: T) {} fn main() { let erased = take<i64>; }",
        "fn take<T>(consume value: T) {} fn main() { let erased: fn(i64) = take; }",
    ] {
        let output = check_source(source);
        assert!(
            output.errors.iter().any(|error| error
                .message
                .contains("callable types do not preserve parameter ownership modes")),
            "{source}: {:?}",
            output.errors
        );
    }
    let output = check_source(
        "fn invoke(consume f: fn[once]() -> i64) { f(); } fn main() { invoke(|| 1); }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn lambda_parameters_keep_the_ordinary_borrow_contract() {
    for source in [
        "fn main() { let invoke = |f: fn[once]() -> i64| f(); }",
        "fn main() { let invoke: fn(fn[once]() -> i64) -> i64 = |f| f(); }",
        "fn main() { let invoke = move |f: fn[once, clone]() -> i64| f(); }",
    ] {
        let output = check_source(source);
        let error = output
            .errors
            .iter()
            .find(|error| error.kind == TypeErrorKind::OwnConsumeBorrowed)
            .unwrap_or_else(|| panic!("{source}: {:?}", output.errors));
        assert!(error
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("named function")
                && suggestion.contains("consume f:")));
    }
    let output =
        check_source("fn main() { let invoke = |f: fn() -> i64| { f(); f() }; invoke(|| 7); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn declared_consume_arguments_invalidate_cloneable_callables() {
    for call in ["take(f)", "take(f: f)"] {
        let source = format!("fn take(consume f: fn[once, clone]() -> i64) {{ f(); }} fn main() {{ let f: fn[once, clone]() -> i64 = || 7; {call}; f(); }}");
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UseAfterMove),
            "{source}: {:?}",
            output.errors
        );
    }
    let output = check_source("fn take(consume f: fn[clone]() -> i64) {} fn main() { let f: fn[clone]() -> i64 = || 7; take(f); f(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
    let output = check_source("fn take(consume f: fn[once, clone]() -> i64) { f(); } fn forward(f: fn[once, clone]() -> i64) { take(f); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::OwnConsumeBorrowed),
        "{:?}",
        output.errors
    );
    let output = check_source("fn take(consume f: fn[once, clone]() -> i64) { f(); } fn forward(consume f: fn[once, clone]() -> i64) { take(f); } fn main() { forward(|| 7); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn borrowed_mutable_callable_requires_clone_only_when_invoked() {
    let output = check_source(
        "fn unused(var cb: fn[var]() -> i64) {} fn invoked(var cb: fn[var]() -> i64) { cb(); }",
    );
    let errors: Vec<_> = output
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::OwnMutateBorrowed)
        .collect();
    assert_eq!(errors.len(), 1, "{:?}", output.errors);
    assert!(errors[0]
        .suggestions
        .iter()
        .any(|text| text.contains("consume") && text.contains("cb")));
    assert!(errors[0]
        .suggestions
        .iter()
        .any(|text| text.contains("fn[var, clone]")));
    for source in [
        "fn invoke(var cb: fn[var, clone]() -> i64) { cb(); cb(); }",
        "fn invoke(consume var cb: fn[var]() -> i64) { cb(); cb(); }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    }
}

const FRESH_MUTABLE_CALLBACK: &str =
    "fn fresh_owned() -> fn[var]() -> i64 { let n = 0; capture(var n) || { n += 1; n } }";

#[test]
fn borrowed_mutable_callable_accepts_definite_replacement() {
    for body in [
        "cb = fresh_owned(); cb();",
        "if flag { cb = fresh_owned(); } else { cb = fresh_owned(); } cb();",
        "if flag { return; } else { cb = fresh_owned(); } cb();",
        "match flag { true => { cb = fresh_owned(); }, false => { cb = fresh_owned(); } } cb();",
        "cb = fresh_owned(); if flag { cb(); } cb();",
    ] {
        let source = format!(
            "{FRESH_MUTABLE_CALLBACK} fn invoke(var cb: fn[var]() -> i64, flag: bool) {{ {body} }}"
        );
        let output = check_source(&source);
        assert!(output.errors.is_empty(), "{body}: {:?}", output.errors);
    }
}

#[test]
fn borrowed_mutable_callable_keeps_borrow_on_any_reaching_branch() {
    for body in [
        "if flag { cb = fresh_owned(); } cb();",
        "if flag { cb = fresh_owned(); } else {} cb();",
        "if flag {} else { cb = fresh_owned(); } cb();",
        "if flag { cb = fresh_owned(); } else { cb(); }",
        "match flag { true => { cb = fresh_owned(); }, false => {} } cb();",
    ] {
        let source = format!(
            "{FRESH_MUTABLE_CALLBACK} fn invoke(var cb: fn[var]() -> i64, flag: bool) {{ {body} }}"
        );
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::OwnMutateBorrowed),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn borrowed_mutable_callable_loop_replacement_cannot_hide_zero_iterations() {
    for body in [
        "while flag { cb = fresh_owned(); cb(); } cb();",
        "for i in 0..n { cb = fresh_owned(); cb(); } cb();",
    ] {
        let source = format!("{FRESH_MUTABLE_CALLBACK} fn invoke(var cb: fn[var]() -> i64, flag: bool, n: i64) {{ {body} }}");
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::OwnMutateBorrowed),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn borrowed_mutable_callable_field_uses_the_selected_guarantee() {
    let output = check_source("type Holder { next: fn[var]() -> i64, shared: Vec<i64> } fn invoke(var holder: Holder) { await holder.next(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::OwnMutateBorrowed),
        "{:?}",
        output.errors
    );
    let output = check_source("type Holder { next: fn[var, clone]() -> i64, shared: Vec<i64> } fn invoke(var holder: Holder) { await holder.next(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn borrowed_mutable_callable_shadowing_does_not_replace_the_parameter() {
    let source = format!("{FRESH_MUTABLE_CALLBACK} fn invoke(var cb: fn[var]() -> i64) {{ {{ var cb = fresh_owned(); cb(); }} cb(); }}");
    let output = check_source(&source);
    assert_eq!(
        output
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::OwnMutateBorrowed)
            .count(),
        1,
        "{:?}",
        output.errors
    );
}

#[test]
fn borrowed_mutable_callable_plain_aggregate_clone_control() {
    for source in [
        "type Holder { next: fn[var, clone]() -> i64, label: string } fn invoke(var holder: Holder) { if true { holder.next(); } }",
        "fn invoke(var pair: (fn[var, clone]() -> i64, string)) { for i in 0..2 { pair.0(); } }",
        "fn invoke(var pair: (fn[var, clone]() -> i64, i64)) { for i in 0..2 { pair.0(); } }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    }
}

#[test]
fn borrowed_once_callable_uses_the_same_replacement_provenance() {
    for body in [
        "cb = || 7; cb();",
        "if flag { cb = || 7; } else { cb = || 8; } cb();",
    ] {
        let source = format!("fn invoke(var cb: fn[once]() -> i64, flag: bool) {{ {body} }}");
        let output = check_source(&source);
        assert!(output.errors.is_empty(), "{body}: {:?}", output.errors);
    }
    let output = check_source(
        "fn invoke(var cb: fn[once]() -> i64, flag: bool) { if flag { cb = || 7; } cb(); }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::OwnConsumeBorrowed),
        "{:?}",
        output.errors
    );
}

#[test]
fn mutable_clone_parameter_owns_an_independent_once_copy() {
    let output = check_source("fn invoke(var cb: fn[once, clone]() -> i64) -> i64 { cb() } fn main() { let cb: fn[once, clone]() -> i64 = || 7; invoke(cb); cb(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source("fn take(consume cb: fn[once, clone]() -> i64) { cb(); } fn forward(var cb: fn[once, clone]() -> i64) { take(cb); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source("fn invoke(var cb: fn[once, clone]() -> i64) { cb(); cb(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
    let output = check_source("fn invoke(cb: fn[once, clone]() -> i64) { cb(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::OwnConsumeBorrowed),
        "{:?}",
        output.errors
    );
}
