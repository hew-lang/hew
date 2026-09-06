use super::check_source;
use crate::check::effects::SuspensionEffect;

#[test]
fn lazy_generator_creators_do_not_inherit_deferred_body_effects() {
    let source = r"
gen fn delayed() -> i64 { await sleep(1ms); yield 1; }
fn create() { let unused = delayed(); }
fn create_block() { let unused = gen { await sleep(1ms); yield 1; }; }
fn consume() { for value in delayed() { println(value); } }
fn main() {
    create();
    create_block();
    let factory = delayed;
    var values = factory();
    let step = await values.next();
    await consume();
}
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for (expression, expected) in [
        ("delayed()", SuspensionEffect::Never),
        ("create()", SuspensionEffect::Never),
        ("create_block()", SuspensionEffect::Never),
        ("factory()", SuspensionEffect::Never),
        ("values.next()", SuspensionEffect::MaySuspend),
        ("consume()", SuspensionEffect::MaySuspend),
    ] {
        let effects: Vec<_> = output
            .suspension_effects
            .calls
            .iter()
            .filter(|(key, _)| &source[key.start..key.end] == expression)
            .map(|(_, effect)| *effect)
            .collect();
        assert!(!effects.is_empty(), "missing call: {expression}");
        assert!(
            effects.iter().all(|effect| *effect == expected),
            "{expression}: {effects:?}"
        );
    }
    assert!(output.suspension_effects.bodies.iter().any(|(body, effect)|
        matches!(body, crate::check::effects::EffectBody::Generator(id) if id.full_path() == "delayed")
            && *effect == SuspensionEffect::MaySuspend));
    assert!(output
        .suspension_effects
        .bodies
        .iter()
        .any(|(body, effect)| matches!(
            body,
            crate::check::effects::EffectBody::GeneratorBlock(_)
        ) && *effect == SuspensionEffect::MaySuspend));
    for call in ["values.next()", "consume()", "sleep(1ms)"] {
        let source = source.replace(&format!("await {call}"), call);
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains("this call may suspend")
                    && &source[error.span.clone()] == call),
            "{call}: {:?}",
            output.errors
        );
    }
}

#[test]
fn deferred_generator_iteration_cannot_suspend() {
    let output = check_source("gen fn delayed() -> i64 { await sleep(1ms); yield 1; } fn main() { defer { for value in delayed() { println(value); } } }");
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("a deferred body cannot suspend while advancing a generator")),
        "{:?}",
        output.errors
    );
}

#[test]
fn actor_ask_task_boundaries_preserve_reply_and_transport_errors() {
    let source = r"
actor Worker {
    receive fn value() -> i64 { 41 }
    receive fn checked() -> Result<i64, string> { Ok(41) }
}
fn main() {
    let worker = spawn Worker();
    let direct = await worker.value();
    let child = fork worker.value();
    let joined = await child;
    let checked = fork worker.checked();
    let checked_joined = await checked;
    let batch = fork (worker.value(), worker.checked());
    let batch_joined = await batch;
    let values = fork [worker.value(), worker.value()];
    let values_joined = await values;
    let selected = select { value = await worker.value() => value };
    let callback = actor |n: i64| -> i64 { n };
    let callback_direct = await callback(1);
    let callback_child = fork callback(2);
    let callback_joined = await callback_child;
}
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let reply = crate::Ty::result(crate::Ty::I64, crate::Ty::ask_error());
    let checked_reply = crate::Ty::result(
        crate::Ty::result(crate::Ty::I64, crate::Ty::String),
        crate::Ty::ask_error(),
    );
    for (expression, expected) in [
        ("worker.value()", reply.clone()),
        ("await worker.value()", reply.clone()),
        (
            "fork worker.value()",
            crate::Ty::Task(Box::new(reply.clone())),
        ),
        ("await child", reply.clone()),
        ("await checked", checked_reply.clone()),
        (
            "await batch",
            crate::Ty::Tuple(vec![reply.clone(), checked_reply]),
        ),
        (
            "await values",
            crate::Ty::Named {
                name: "Vec".into(),
                builtin: Some(crate::BuiltinType::Vec),
                args: vec![reply.clone()],
            },
        ),
        (
            "select { value = await worker.value() => value }",
            reply.clone(),
        ),
        ("await callback(1)", reply.clone()),
        ("fork callback(2)", crate::Ty::Task(Box::new(reply.clone()))),
        ("await callback_child", reply),
    ] {
        let start = source.find(expression).unwrap();
        assert_eq!(
            output.expr_types.get(&crate::check::SpanKey::in_module(
                &(start..start + expression.len()),
                0
            )),
            Some(&expected),
            "{expression}"
        );
    }
    let captures: Vec<_> = output
        .suspension_effects
        .fork_transfers
        .iter()
        .filter(|(key, _)| &source[key.start..key.end] == "worker")
        .collect();
    assert_eq!(captures.len(), 6);
    for (_, fact) in captures {
        assert_eq!(fact.acquisition, crate::ClosureCaptureAcquisition::Snapshot);
        assert!(fact.is_send && fact.is_sync);
    }
}

#[test]
fn fork_task_boundary_moves_nominal_resources_and_rejects_borrowed_views() {
    let source = "#[resource] type Socket { fd: i64 } fn read(socket: Socket) -> i64 { socket.fd } fn main() { let socket = Socket { fd: 1 }; let child = fork read(socket); let result = await child; }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .fork_transfers
        .iter()
        .any(|(key, fact)| &source[key.start..key.end] == "socket"
            && fact.acquisition == crate::ClosureCaptureAcquisition::Move
            && fact.is_send));
    for (source, expected) in [
        ("#[resource] type Socket { fd: i64 } fn read(socket: Socket) -> i64 { socket.fd } fn launch(socket: Socket) { let child = fork read(socket); } fn main() {}", crate::error::TypeErrorKind::OwnConsumeBorrowed),
        ("#[resource] type Socket { fd: i64 } fn read(socket: Socket) -> i64 { socket.fd } fn main() { let socket = Socket { fd: 1 }; let child = fork read(socket); println(socket.fd); }", crate::error::TypeErrorKind::UseAfterMove),
        ("extern \"C\" { fn get() -> &i64; fn read(value: &i64) -> i64; } fn main() { unsafe { let view = get(); let child = fork read(view); } }", crate::error::TypeErrorKind::InvalidSend),
    ] {
        let output = check_source(source);
        assert!(output.errors.iter().any(|error| error.kind == expected), "{source}: {:?}", output.errors);
    }
}

#[test]
fn actor_ask_task_boundary_transfers_resources_once() {
    let source = r"
#[resource] type Socket { fd: i64 }
impl Socket { fn detach(consuming self) -> i64 { self.fd } }
actor Worker { receive fn read(socket: Socket) -> i64 { socket.detach() } }
fn main() {
    let worker = spawn Worker();
    let socket = Socket { fd: 1 };
    let child = fork worker.read(socket);
    let result = await child;
    let second = Socket { fd: 2 };
    let third = Socket { fd: 3 };
    let batch = fork (worker.read(second), worker.read(third));
    let results = await batch;
}
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for name in ["socket", "second", "third"] {
        assert!(output
            .suspension_effects
            .fork_transfers
            .iter()
            .any(|(key, fact)| &source[key.start..key.end] == name
                && fact.acquisition == crate::ClosureCaptureAcquisition::Move));
    }
    let source = source.replace("worker.read(third)", "worker.read(second)");
    let output = check_source(&source);
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            crate::error::TypeErrorKind::UseAfterMove
                | crate::error::TypeErrorKind::UseAfterConsume
        )),
        "{:?}",
        output.errors
    );
}

#[test]
fn select_join_ignores_a_diverging_winner() {
    let output = check_source("fn choose() -> i64 { let first = fork { 41 }; let second = fork { 0 }; select { a = await first => return a, b = await second => b }; await first } fn main() {} ");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn select_classifies_an_awaited_actor_ask_by_checked_dispatch() {
    let output = check_source("actor Worker { receive fn value() -> i64 { 41 } } fn main() { let worker = spawn Worker(); let result = select { value = await worker.value() => value }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(matches!(
        output
            .select_sources
            .values()
            .next()
            .and_then(|sources| sources.first()),
        Some(crate::check::CheckedSelectSource::ActorAsk { .. })
    ));
}

#[test]
fn select_prepares_task_handles_and_consumes_only_the_winner() {
    let source = "fn main() { let first = fork { 41 }; let second = fork { 0 }; let value = select { a = await first => a + await second, b = await second => b + await first }; }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let sources = output
        .select_sources
        .values()
        .next()
        .expect("checked select");
    assert_eq!(sources.len(), 2);
    for (entry, expected) in sources.iter().zip(["first", "second"]) {
        let crate::check::CheckedSelectSource::TaskAwait { operand } = entry else {
            panic!("{entry:?}")
        };
        assert_eq!(source[operand.start..operand.end].trim(), expected);
    }
}

#[test]
fn select_winner_is_unavailable_in_its_arm_and_after_a_join() {
    for tail in [
        "select { a = await first => await first, b = await second => b };",
        "select { a = await first => a, b = await second => b }; let again = await first;",
    ] {
        let output = check_source(&format!(
            "fn main() {{ let first = fork {{ 41 }}; let second = fork {{ 0 }}; {tail} }}"
        ));
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, crate::error::TypeErrorKind::UseAfterMove)),
            "{:?}",
            output.errors
        );
    }
}

#[test]
fn select_timeout_retains_every_task() {
    let output = check_source("fn main() { let first = fork { 41 }; let second = fork { 0 }; let value = select { a = await first => a, b = await second => b, after 1ms => await first + await second }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

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
fn fork_promotes_borrowed_value_parameters_into_owning_captures() {
    let source = "fn echo(value: string) -> string { value } fn launch(value: string) { let task = fork echo(value); println(value); } fn main() {}";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .fork_transfers
        .iter()
        .any(|(key, fact)| source[key.start..key.end].trim() == "value"
            && fact.is_send
            && fact.is_sync
            && fact.acquisition == crate::ClosureCaptureAcquisition::Snapshot));
    let output = check_source("fn launch(value: string) { let task = fork { await sleep(1ms); println(value); }; println(value); } fn main() {}");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .closure_capture_facts
        .values()
        .flatten()
        .any(|capture| capture.name == "value"
            && capture.acquisition == crate::ClosureCaptureAcquisition::Snapshot));
    let output = check_source("type Label { value: string } fn launch(data: bytes, label: Label) { let task = fork { println(label.value); data }; println(label.value); let retained = data; } fn main() {}");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn fork_cannot_promote_an_affine_borrow() {
    let output = check_source("#[resource] type Socket {} impl Socket { fn close(consuming self) {} } fn launch(socket: Socket) { let task = fork { socket.close(); }; } fn main() {}");
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            crate::error::TypeErrorKind::OwnConsumeBorrowed
                | crate::error::TypeErrorKind::ForkBorrowCapture { .. }
        )),
        "{:?}",
        output.errors
    );
}

#[test]
fn ordinary_fork_operands_snapshot_owned_values_and_reusable_closures() {
    let source = "fn echo(value: string) -> string { value } fn main() { let text = \"hello\"; let length = fork text.len(); println(await length); println(text); let prefix = \"prefix\"; let callback = || prefix; let child = fork callback(); let again = callback(); let batch = fork (echo(text), echo(text)); let body = fork { println(text); }; println(text); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for (key, fact) in &output.suspension_effects.fork_transfers {
        if matches!(source[key.start..key.end].trim(), "text" | "callback") {
            assert_eq!(fact.acquisition, crate::ClosureCaptureAcquisition::Snapshot);
        }
    }
}

#[test]
fn consuming_fork_arguments_still_transfer_the_original_owner() {
    let source = "fn take(consume value: string) -> string { value } fn main() { let text = \"hello\"; let child = fork take(text); println(text); }";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, crate::error::TypeErrorKind::UseAfterMove)),
        "{:?}",
        output.errors
    );
    assert!(output
        .suspension_effects
        .fork_transfers
        .iter()
        .any(|(key, fact)| source[key.start..key.end].trim() == "text"
            && fact.acquisition == crate::ClosureCaptureAcquisition::Move));
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
