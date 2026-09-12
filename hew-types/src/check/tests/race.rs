//! Race checker negatives: the operand contract from spec 4.11.2, checked
//! independently of the runtime acceptance matrix in `tests/core-acceptance`.

use super::check_source;
use crate::error::TypeErrorKind;

#[test]
fn empty_race_is_refused() {
    let output = check_source("fn main() { let result = race {}; }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InvalidOperation
                && error.message.contains("race requires at least one call")),
        "{:?}",
        output.errors
    );
}

#[test]
fn race_operands_must_share_one_type() {
    let output = check_source(
        "fn number() -> i64 { 1 } fn text() -> string { \"a\" } fn main() { let result = race { number(), text() }; }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "{:?}",
        output.errors
    );
}

#[test]
fn await_on_a_race_operand_is_refused() {
    // Spec 4.11.2: "await is never written on a race operand; the race is
    // what waits", mirroring the select arm-source rule (4.11.1). The
    // operand's own type still checks so other diagnostics keep reporting.
    let source = "fn work() -> i64 { 1 } fn main() { let result = race { await work(), work() }; }";
    let output = check_source(source);
    let error = output
        .errors
        .iter()
        .find(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error
                    .message
                    .contains("a race operand never writes `await`")
        })
        .unwrap_or_else(|| panic!("{:?}", output.errors));
    assert_eq!(&source[error.span.clone()], "await work()");
    let plain = source.replace("await ", "");
    let output = check_source(&plain);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn race_rejects_non_send_fork_inputs() {
    let output = check_source(
        "fn use_value(value: Rc<i64>) -> i64 { 1 } fn main() { let value = Rc.new(1); let result = race { use_value(value), use_value(value) }; }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InvalidSend),
        "{:?}",
        output.errors
    );
}

#[test]
fn race_consumed_input_is_unusable_after_the_race() {
    let output = check_source(
        "fn take(consume value: string) -> string { value } fn hold() -> string { \"other\" } fn main() { let text = \"hello\"; let result = race { take(text), hold() }; println(text); }",
    );
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::UseAfterMove | TypeErrorKind::UseAfterConsume
        )),
        "{:?}",
        output.errors
    );
}
