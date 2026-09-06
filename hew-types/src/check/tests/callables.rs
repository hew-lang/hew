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
    let output = check_source("fn main() { let count: i64 = 0; var next = [var count] || { count = count + 1; count }; next(); next(); }");
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
    let output = check_source("fn main() { let count: i64 = 0; let next = [var count] || { count = count + 1; count }; next(); }");
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
                .any(|error| error.message.contains("[var ")),
            "{:?}",
            output.errors
        );
    }
}

#[test]
fn private_prefix_resolves_existing_binding_and_respects_body_shadowing() {
    let output = check_source("fn main() { let count: i64 = 0; let f = [var count] || { var count: i64 = 2; count = 3; count }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.closure_capture_facts.values().all(Vec::is_empty));
    let output = check_source("fn main() { let f = [var missing] || 1; }");
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
