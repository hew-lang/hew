use super::check_source;
use crate::check::effects::SuspensionEffect;

#[test]
fn fork_rejects_non_send_arguments_and_indirect_captures() {
    for source in [
        "fn use_value(value: Rc<i64>) -> i64 { 1 } fn main() { let value = Rc.new(1); let task = fork use_value(value); }",
        "fn main() { let value = Rc.new(1); let callback = || { let held = value; 1 }; let task = fork callback(); }",
        "type Job { run: fn() -> i64 } fn main() { let value = Rc.new(1); let job = Job { run: || { let held = value; 1 } }; let task = fork job.run(); }",
        "type Payload { value: Rc<i64> } fn use_value(value: Payload) -> i64 { 1 } fn main() { let value = Payload { value: Rc.new(1) }; let task = fork use_value(value); }",
        "fn use_value(value: (fn() -> i64, Rc<i64>)) -> i64 { 1 } fn main() { let value = (|| 1, Rc.new(1)); let task = fork use_value(value); }",
        "type Payload { value: Rc<i64> } impl Payload { fn run(self) -> i64 { 1 } } fn main() { let value = Payload { value: Rc.new(1) }; let task = fork value.run(); }",
    ] {
        let output = check_source(source);
        assert!(output.errors.iter().any(|error| matches!(error.kind, crate::error::TypeErrorKind::InvalidSend)), "{source}: {:?}", output.errors);
    }
}

#[test]
fn fork_cannot_retain_borrowed_parameters() {
    let output = check_source("fn echo(value: string) -> string { value } fn launch(value: string) { let task = fork echo(value); } fn main() {} ");
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, crate::error::TypeErrorKind::OwnConsumeBorrowed)),
        "{:?}",
        output.errors
    );
}

#[test]
fn fork_accepts_owning_arguments_and_send_callable_values() {
    for source in [
        "fn echo(value: string) -> string { value } fn main() { let task = fork echo(\"hello\"); let result = await task; }",
        "fn main() { let value = \"hello\"; let callback = || value; let task = fork callback(); let result = await task; }",
        "type Job { run: fn() -> i64 } fn main() { let job = Job { run: || 1 }; let task = fork job.run(); let result = await task; }",
        "fn work() -> i64 { 1 } fn main() { let callback = work; let task = fork callback(); let result = await task; }",
        "fn work(value: i64) -> i64 { value } fn main() { let tasks = fork [work(1), work(2)]; let result = await tasks; }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    }
}

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

#[test]
fn distinct_callback_fields_keep_distinct_effects() {
    let source = "type Job { run: fn() -> i64, describe: fn() -> i64 } fn invoke(job: Job) -> i64 { await job.describe() } fn work() -> i64 { let task = fork { 1 }; await task } fn main() { let job = Job { run: work, describe: || 2 }; let value = invoke(job); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let rejected = source.replace("await job.describe()", "await job.run()");
    let output = check_source(&rejected);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")
                && &rejected[error.span.clone()] == "invoke(job)"),
        "{:?}",
        output.errors
    );
}

#[test]
fn pure_callable_record_and_tuple_projections_are_synchronous() {
    let output = check_source("type Job { run: fn() -> i64 } fn main() { let job = Job { run: || 2 }; let a = job.run(); let pair = (|| 3, || 4); let b = (pair.0)(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn collection_callbacks_use_their_own_effect() {
    let source = "fn work(x: i64) -> i64 { let task = fork { 1 }; x + await task } fn main() { let values = [1, 2]; let mapped = await values.map(work); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source(&source.replace("await values.map", "values.map"));
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")),
        "{:?}",
        output.errors
    );
}

#[test]
fn named_callback_arguments_follow_parameter_identity() {
    let output = check_source("fn invoke(f: fn(i64) -> i64, x: i64) -> i64 { await f(x) } fn pure(x: i64) -> i64 { x } fn main() { let value = invoke(x: 1, f: pure); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn named_actor_handlers_publish_body_effects() {
    let output =
        check_source("actor Worker { receive fn run() { await sleep(1ms); } } fn main() {}");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .bodies
        .iter()
        .any(|(body, effect)| matches!(body,
        crate::check::effects::EffectBody::Declaration(id) if id.full_path() == "Worker::run")
            && *effect == SuspensionEffect::MaySuspend));
}

#[test]
fn method_callback_parameters_account_for_the_receiver() {
    let output = check_source("type Runner {} impl Runner { fn invoke(self, f: fn() -> i64) -> i64 { await f() } } fn main() { let runner = Runner {}; let value = runner.invoke(|| 3); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn replacing_a_callable_field_invalidates_its_old_effect() {
    let source = "type Job { run: fn() -> i64 } fn work() -> i64 { let task = fork { 1 }; await task } fn main() { var job = Job { run: || 2 }; job.run = work; let value = job.run(); }";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")
                && &source[error.span.clone()] == "job.run()"),
        "{:?}",
        output.errors
    );
}

#[test]
fn a_later_loop_assignment_cannot_hide_suspension() {
    let source = "fn work() -> i64 { let task = fork { 1 }; await task } fn main() { var callback: fn() -> i64 = || 2; for i in 0..2 { let value = callback(); callback = work; } }";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("this call may suspend")
                && &source[error.span.clone()] == "callback()"),
        "{:?}",
        output.errors
    );
}
