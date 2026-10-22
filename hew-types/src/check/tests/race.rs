//! Race checker negatives: the operand contract from spec 4.11.2, checked
//! independently of the runtime acceptance matrix in `tests/core-acceptance`.

use super::check_source;
use crate::error::TypeErrorKind;

#[test]
fn empty_race_is_refused() {
    let output = check_source("fn main() { let result = race []; }");
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
        "fn number() -> i64 { 1 } fn text() -> string { \"a\" } fn main() { let result = race [ number(), text() ]; }",
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
    let source = "fn work() -> i64 { 1 } fn main() { let result = race [ await work(), work() ]; }";
    let output = check_source(source);
    let error = output
        .errors
        .iter()
        .find(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error.message.contains("race expects calls or tasks")
        })
        .unwrap_or_else(|| panic!("{:?}", output.errors));
    assert_eq!(&source[error.span.clone()], "await work()");
    let plain = source.replace("await ", "");
    let output = check_source(&plain);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn race_rejects_non_send_fork_inputs() {
    for source in [
        "fn use_value(value: Rc<i64>) -> i64 { 1 } fn main() { let value = Rc.new(1); let result = race [ use_value(value), use_value(value) ]; }",
        "fn use_value(value: Rc<i64>) -> i64 { sleep(1ms); 1 } fn main() { let value = Rc.new(1); let result = select { answer from use_value(value) => answer, after 1s => 0 }; }",
        "type Reader { value: Rc<i64>; } impl Reader { fn read(self) -> i64 { sleep(1ms); 1 } } fn main() { let reader = Reader { value: Rc.new(1) }; let result = select { answer from reader.read() => answer, after 1s => 0 }; }",
        "trait Reader { fn[suspends] read(self) -> i64; } type Counter {} impl Reader for Counter { fn read(self) -> i64 { sleep(1ms); 1 } } fn main() { let reader: dyn Reader = Counter {}; let result = select { answer from reader.read() => answer, after 1s => 0 }; }",
    ] {
        let output = check_source(source);
        assert!(output.errors.iter().any(|error| error.kind == TypeErrorKind::InvalidSend), "{source}: {:?}", output.errors);
    }
    let output = check_source("trait Reader { fn read(self) -> i64; } type Counter { value: Rc<i64>; } impl Reader for Counter { fn read(self) -> i64 { 1 } } fn main() { let reader: dyn (Reader + Send + Sync) = Counter { value: Rc.new(1) }; }");
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
fn owning_race_and_select_calls_copy_ordinary_inputs() {
    for source in [
        "fn take(consume value: string) -> string { value } fn hold() -> string { \"other\" } fn main() { let text = \"hello\"; let result = race [ take(text), hold() ]; println(text); }",
        "fn take(consume value: string) -> string { sleep(1ms); value } fn main() { let text = \"hello\"; let result = select { answer from take(text) => answer, after 1s => \"timer\" }; println(text); }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    }
}

#[test]
fn consuming_select_still_moves_an_affine_trait_object() {
    let source =
"trait Finish { fn finish(consume self) -> string; } type Label { text: string; } impl Finish for Label { fn finish(consume self) -> string { self.text } } fn main() { let job: dyn (Finish + Send) = Label { text: \"hello\" }; let result = select { answer from job.finish() => answer, after 1s => \"timer\" }; println(job); }";
    let output = check_source(source);
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::UseAfterMove | TypeErrorKind::UseAfterConsume
        )),
        "{source}: {:?}",
        output.errors
    );
}
