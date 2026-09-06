use super::check_source;
use crate::check::effects::SuspensionEffect;

#[test]
fn suspending_calls_require_exact_operand_permission() {
    let source = "fn work() -> i64 { let task = fork { 7 }; await task }\nfn identity(x: i64) -> i64 { x }\nfn main() { let x = await identity(work()); }";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")
                && &source[error.span.clone()] == "work()"),
        "{:?}",
        output.errors
    );
    let output = check_source(&source.replace("identity(work())", "identity(await work())"));
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn recursive_effects_follow_checked_calls() {
    let output = check_source("fn a(n: i64) -> i64 { if n == 0 { let t = fork { 1 }; return await t; } await b(n - 1) } fn b(n: i64) -> i64 { await a(n) } fn main() { let x = await b(2); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(
        output
            .suspension_effects
            .bodies
            .values()
            .filter(|effect| **effect == SuspensionEffect::MaySuspend)
            .count()
            >= 3
    );
}

#[test]
fn pure_closure_calls_stay_synchronous() {
    let output = check_source("fn main() { let f = |x: i64| x + 1; let y = f(2); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .calls
        .values()
        .all(|effect| *effect == SuspensionEffect::Never));
}

#[test]
fn fork_rejects_values_and_accepts_call_batches() {
    let output = check_source("fn main() { let task = fork 42; }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("fork expects a call")),
        "{:?}",
        output.errors
    );
    let output = check_source("fn value() -> i64 { 1 } fn main() { let task = fork [value(), value()]; let values = await task; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn scopes_produce_their_body_value() {
    let output = check_source(
        "fn answer() -> i64 { let value = scope { 42 }; value } fn main() { let x = answer(); }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn scope_recovery_preserves_an_ordinary_result_value() {
    let output = check_source(
        r"fn main() {
        let value: Result<i64, string> = scope { Ok(42) } handle failure {
            match failure {
                .Deadline { message } => Err(message),
                .Fault { message } => Err(message),
            }
        };
    }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn callback_effects_specialize_at_each_call() {
    let source = "fn invoke(f: fn(i64) -> i64, x: i64) -> i64 { await f(x) } fn pure(x: i64) -> i64 { x } fn work(x: i64) -> i64 { let t = fork { 1 }; x + await t } fn main() { let a = invoke(pure, 1); let b = await invoke(work, 2); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let rejected = source.replace("await invoke(work", "invoke(work");
    let output = check_source(&rejected);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")
                && rejected[error.span.start..].starts_with("invoke")),
        "{:?}",
        output.errors
    );
}

#[test]
fn builtin_sleep_effect_does_not_apply_to_a_source_shadow() {
    let output = check_source("fn work() { await sleep(1ms); } fn main() { work(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")),
        "{:?}",
        output.errors
    );
    let output =
        check_source("fn sleep(value: i64) -> i64 { value } fn main() { let x = sleep(1); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn scope_deadline_requires_duration_and_preserves_value() {
    let output = check_source("fn main() { let value: i64 = scope within 2s { 42 }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source("fn main() { let value = scope within 2 { 42 }; }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("duration")),
        "{:?}",
        output.errors
    );
}
