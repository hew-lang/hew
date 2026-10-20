use std::fmt::Write as _;

use super::check_source;
use crate::check::effects::SuspensionEffect;

#[test]
fn supervisor_construction_requires_a_suspending_callable_slot() {
    let source = r"
        actor Worker { receive fn ping() {} }
        supervisor Tree { child worker: Worker; }
        fn construct() -> Tree { spawn Tree }
        fn main() { let factory: fn[suspends]() -> Tree = construct; }
    ";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let synchronous = source.replace("fn[suspends]()", "fn()");
    let output = check_source(&synchronous);
    assert_eq!(output.errors.len(), 1, "{:?}", output.errors);
    assert!(
        output.errors[0]
            .message
            .contains("function `construct` suspends via `supervisor construction`"),
        "{:?}",
        output.errors
    );
    assert!(output.errors[0].message.contains("write `fn[suspends]`"));
}

#[test]
fn recursive_generic_parameter_swaps_keep_finite_instances_precise() {
    let source = r#"
fn swap<A, B>(consume first: A, consume second: B, n: i64) {
    if n > 0 { swap(second, first, n - 1); }
}
fn main() { swap(1, "second", 2); }
"#;
    assert_call_effect(source, "swap(1, \"second\", 2)", SuspensionEffect::Never);
    let resource = source.replace("fn main() { swap(1, \"second\", 2); }", "#[resource] type Ticket {} impl Ticket { fn close(consume self) { sleep(1ms); } } fn main() { swap(Ticket {}, 7, 2); }");
    assert_call_effect(
        &resource,
        "swap(Ticket {}, 7, 2)",
        SuspensionEffect::MaySuspend,
    );
}

#[test]
fn effect_witnesses_follow_source_calls_across_hash_seeds() {
    let source = r"
fn first() { sleep(1ms); }
fn second() { let child = fork { 1 }; let _ = await child; }
fn wrapper() { first(); second(); }
fn main() { let callback: fn() -> () = wrapper; }
";
    for _ in 0..16 {
        let output = check_source(source);
        assert_eq!(output.errors.len(), 1, "{:?}", output.errors);
        assert!(
            output.errors[0].message.contains("via `first(...)`"),
            "{:?}",
            output.errors
        );
    }
}

/// Effects recorded for every call whose source text is `expression`.
fn call_effects(
    output: &crate::check::TypeCheckOutput,
    source: &str,
    expression: &str,
) -> Vec<SuspensionEffect> {
    let effects: Vec<_> = output
        .suspension_effects
        .calls
        .iter()
        .filter(|(key, _)| {
            key.module_idx == 0 && source[key.start..key.end].trim_end() == expression
        })
        .map(|(_, effect)| *effect)
        .collect();
    assert!(!effects.is_empty(), "missing call: {expression}");
    effects
}

fn assert_call_effect(source: &str, expression: &str, expected: SuspensionEffect) {
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    let effects = call_effects(&output, source, expression);
    assert!(
        effects.iter().all(|effect| *effect == expected),
        "{expression}: {effects:?}"
    );
}

#[test]
fn deferred_pure_scopes_inherit_contents_without_admitting_suspension() {
    let source = "fn pure() -> i64 { 42 } fn cleanup() { defer { let value = scope { scope { pure() } }; println(value); } } fn main() { cleanup(); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let start = source.rfind("cleanup()").unwrap();
    assert_eq!(
        output
            .suspension_effects
            .calls
            .get(&crate::check::SpanKey::in_module(
                &(start..start + "cleanup()".len()),
                0
            )),
        Some(&SuspensionEffect::Never)
    );
    for body in [
        "scope { sleep(1ms); }",
        "scope { sleep(1ms); }",
        "scope { let child = fork { 1 }; }",
        "scope { let child = fork pure(); }",
        "scope { work(); }",
        "scope within 1ms { pure(); }",
    ] {
        let output = check_source(&format!("fn pure() -> i64 {{ 1 }} fn work() {{ sleep(1ms); }} fn main() {{ defer {{ {body}; }} }}"));
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains("a deferred body cannot")),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn lazy_generator_creators_do_not_inherit_deferred_body_effects() {
    let source = r"
gen fn delayed() -> i64 { sleep(1ms); yield 1; }
fn create() -> Generator<i64, ()> { delayed() }
fn create_block() -> Generator<i64, ()> { gen { sleep(1ms); yield 1; } }
fn consume() { for value in delayed() { println(value); } }
fn main() {
    create();
    create_block();
    let factory = delayed;
    var values = factory();
    let step = values.next();
    consume();
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
        let effects = call_effects(&output, source, expression);
        assert!(
            effects.iter().all(|effect| *effect == expected),
            "{expression}: {effects:?}"
        );
    }
    assert!(output.suspension_effects.bodies.iter().any(|(body, effect)|
        matches!(body, crate::check::effects::EffectBody::Generator(id) if output.defs.path(*id) == "delayed")
            && *effect == SuspensionEffect::MaySuspend));
    assert!(output
        .suspension_effects
        .bodies
        .iter()
        .any(|(body, effect)| matches!(
            body,
            crate::check::effects::EffectBody::GeneratorBlock(_)
        ) && *effect == SuspensionEffect::MaySuspend));
    let plain = source.replace("await ", "");
    let output = check_source(&plain);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for call in ["values.next()", "consume()"] {
        assert!(call_effects(&output, &plain, call)
            .iter()
            .all(|effect| *effect == SuspensionEffect::MaySuspend));
    }
}

#[test]
fn deferred_generator_iteration_cannot_suspend() {
    let output = check_source("gen fn delayed() -> i64 { sleep(1ms); yield 1; } fn main() { defer { for value in delayed() { println(value); } } }");
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("a deferred body cannot suspend while advancing a generator")),
        "{:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one program pins every task-boundary shape of a completion call"
)]
fn actor_ask_task_boundaries_preserve_reply_and_transport_errors() {
    let source = r"
actor Worker {
    receive fn value() -> i64 { 41 }
    receive fn checked() -> Result<i64, string> { .Ok(41) }
}
fn main() {
    let worker = spawn Worker;
    let direct = worker.value();
    let child = fork worker.value();
    let joined = await child;
    let checked = fork worker.checked();
    let checked_joined = await checked;
    let batch = fork (worker.value(), worker.checked());
    let batch_joined = await batch;
    let values = fork [worker.value(), worker.value()];
    let values_joined = await values;
    let selected = select { value from worker.value() => value };
    let callback = actor |n: i64| -> i64 { n };
    let callback_direct = callback(1);
    let callback_child = fork callback(2);
    let callback_joined = await callback_child;
}
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    // A completion call yields `Result<R, ActorError<E>>`: `E` is the
    // handler's declared failure (`Never` here — neither handler declares
    // `fails`) and `M` is the call's own sealed message type, so the error arm
    // is pinned by shape rather than by one spelling shared across call sites.
    let reply = crate::Ty::I64;
    let checked_reply = crate::Ty::result(crate::Ty::I64, crate::Ty::String);
    let actor_error = |ty: &crate::Ty| -> bool {
        matches!(ty, crate::Ty::Named { args, head: name_head @ (crate::TypeHead::Nominal(_) | crate::TypeHead::Param(_) | crate::TypeHead::Unresolved(_)) }
            if name_head.spelling() == crate::KnownDecl::ActorError.path()
                && args.first() == Some(&crate::Ty::never_type()))
    };
    let completion = |ty: &crate::Ty, success: &crate::Ty| -> bool {
        ty.as_result()
            .is_some_and(|(ok, err)| ok == success && actor_error(err))
    };
    for (expression, expected) in [
        ("worker.value()", reply.clone()),
        ("worker.value()", reply.clone()),
        ("await child", reply.clone()),
        ("await checked", checked_reply.clone()),
        (
            "select { value from worker.value() => value }",
            reply.clone(),
        ),
        ("callback(1)", reply.clone()),
        ("await callback_child", reply.clone()),
    ] {
        let start = source.find(expression).unwrap();
        let ty = output
            .expr_types
            .get(&crate::check::SpanKey::in_module(
                &(start..start + expression.len()),
                0,
            ))
            .unwrap_or_else(|| panic!("{expression} has no checked type"));
        assert!(completion(ty, &expected), "{expression}: {ty:?}");
    }
    for (expression, expected) in [
        ("fork worker.value()", reply.clone()),
        ("fork callback(2)", reply.clone()),
    ] {
        let start = source.find(expression).unwrap();
        let ty = output
            .expr_types
            .get(&crate::check::SpanKey::in_module(
                &(start..start + expression.len()),
                0,
            ))
            .unwrap_or_else(|| panic!("{expression} has no checked type"));
        let crate::Ty::Task(inner) = ty else {
            panic!("{expression} is not a task: {ty:?}");
        };
        assert!(completion(inner, &expected), "{expression}: {ty:?}");
    }
    let start = source.find("await batch").unwrap();
    let batch = output
        .expr_types
        .get(&crate::check::SpanKey::in_module(
            &(start..start + "await batch".len()),
            0,
        ))
        .expect("batch join type");
    let crate::Ty::Tuple(joined) = batch else {
        panic!("batch join is not a tuple: {batch:?}");
    };
    assert!(completion(&joined[0], &reply), "{batch:?}");
    assert!(completion(&joined[1], &checked_reply), "{batch:?}");
    let start = source.find("await values").unwrap();
    let values = output
        .expr_types
        .get(&crate::check::SpanKey::in_module(
            &(start..start + "await values".len()),
            0,
        ))
        .expect("vector join type");
    let crate::Ty::Named {
        head: name_head,
        args,
        ..
    } = values
    else {
        panic!("vector join is not a Vec: {values:?}");
    };
    assert_eq!(name_head.spelling(), "Vec");
    assert!(completion(&args[0], &reply), "{values:?}");
    let captures: Vec<_> = output
        .suspension_effects
        .fork_transfers
        .iter()
        .filter(|(key, _)| key.module_idx == 0 && &source[key.start..key.end] == "worker")
        .collect();
    assert_eq!(captures.len(), 6);
    for (_, fact) in captures {
        assert_eq!(fact.acquisition, crate::ClosureCaptureAcquisition::Snapshot);
        assert!(fact.is_send && fact.is_sync);
    }
}

#[test]
fn fork_task_boundary_moves_nominal_resources_and_rejects_borrowed_views() {
    let source = "#[resource]\ntype Socket {\n    fd: i64;\n}\n\nfn read(socket: Socket) -> i64 {\n    socket.fd\n}\n\nfn main() {\n    let socket = Socket { fd: 1 };\n    let child = fork read(socket);\n    let result = await child;\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .fork_transfers
        .iter()
        .any(|(key, fact)| key.module_idx == 0
            && &source[key.start..key.end] == "socket"
            && fact.acquisition == crate::ClosureCaptureAcquisition::Move
            && fact.is_send));
    for (source, expected) in [
        ("#[resource]\ntype Socket {\n    fd: i64;\n}\n\nfn read(socket: Socket) -> i64 {\n    socket.fd\n}\n\nfn launch(socket: Socket) {\n    let child = fork read(socket);\n}\n\nfn main() {}\n", crate::error::TypeErrorKind::OwnConsumeBorrowed),
        ("#[resource]\ntype Socket {\n    fd: i64;\n}\n\nfn read(socket: Socket) -> i64 {\n    socket.fd\n}\n\nfn main() {\n    let socket = Socket { fd: 1 };\n    let child = fork read(socket);\n    println(socket.fd);\n}\n", crate::error::TypeErrorKind::UseAfterMove),
        ("extern \"C\" { fn get() -> &i64; fn read(value: &i64) -> i64; } fn main() { unsafe { let view = get(); let child = fork read(view); } }", crate::error::TypeErrorKind::InvalidSend),
    ] {
        let output = check_source(source);
        assert!(output.errors.iter().any(|error| error.kind == expected), "{source}: {:?}", output.errors);
    }
}

#[test]
fn actor_ask_task_boundary_transfers_resources_once() {
    let source = r"#[resource]
type Socket {
    fd: i64;
}

impl Socket {
    fn detach(consume self) -> i64 {
        self.fd
    }
}

actor Worker {
    receive fn read(socket: Socket) -> i64 {
        socket.detach()
    }
}

fn main() {
    let worker = spawn Worker;
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
            .any(|(key, fact)| key.module_idx == 0
                && &source[key.start..key.end] == name
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
    let output = check_source("fn choose() -> i64 { let first = fork { 41 }; let second = fork { 0 }; select { a from first => return a, b from second => b }; await first } fn main() {} ");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn select_classifies_an_actor_call_source_by_checked_dispatch() {
    let output = check_source("actor Worker { receive fn value() -> i64 { 41 } } fn main() { let worker = spawn Worker; let result = select { value from worker.value() => value }; }");
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
    let source = "fn main() { let first = fork { 41 }; let second = fork { 0 }; let value = select { a from first => a + await second, b from second => b + await first }; }";
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
        "select { a from first => await first, b from second => b };",
        "select { a from first => a, b from second => b }; let again = await first;",
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
    let output = check_source("fn main() { let first = fork { 41 }; let second = fork { 0 }; let value = select { a from first => a, b from second => b, after 1ms => await first + await second }; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn select_preparation_rejects_changing_an_already_borrowed_task() {
    for (prepare, expected) in [
        (
            "task = fork { 42 };",
            crate::error::TypeErrorKind::OwnMutateBorrowed,
        ),
        (
            "let value = await task;",
            crate::error::TypeErrorKind::OwnConsumeBorrowed,
        ),
    ] {
        let output = check_source(&format!(
            "fn main() {{ var task = fork {{ 17 }}; select {{ value from task => value, after {{ {prepare} 0ms }} => 0 }}; }}"
        ));
        let diagnostic = output
            .errors
            .iter()
            .find(|error| error.kind == expected)
            .unwrap_or_else(|| panic!("{prepare}: {:?}", output.errors));
        assert!(
            diagnostic.message.contains("prepared task"),
            "{diagnostic:?}"
        );
        assert_eq!(diagnostic.notes.len(), 1);
    }
}

#[test]
fn select_preparation_tracks_binding_identity_and_disjoint_fields() {
    for source in [
        "fn main() { let task = fork { 17 }; select { value from task => value, after { let task = fork { 42 }; let value = await task; 0ms } => await task }; }",
        "fn main() { var pair = (fork { 17 }, fork { 42 }); select { value from pair.0 => value, after { pair.1 = fork { 0 }; 0ms } => await pair.0 }; }",
        "fn main() { var task = fork { 17 }; select { value from task => value, after 0ms => { task = fork { 42 }; await task } }; }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    }
}

#[test]
fn nested_select_keeps_the_outer_preparation_borrow() {
    let output = check_source("fn main() { var task = fork { 17 }; let second = fork { 42 }; select { value from task => value, after { select { value from second => { task = fork { 0 }; 0ms } }; 0ms } => 0 }; }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == crate::error::TypeErrorKind::OwnMutateBorrowed),
        "{:?}",
        output.errors
    );
}

#[test]
fn ordinary_task_await_consumes_its_handle_before_the_next_use() {
    let output = check_source(
        "fn main() { let task = fork { 42 }; let first = await task; let second = await task; }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == crate::error::TypeErrorKind::UseAfterMove),
        "{:?}",
        output.errors
    );
}

#[test]
fn fork_rejects_non_send_arguments_and_indirect_captures() {
    for source in [
        "fn use_value(value: Rc<i64>) -> i64 { 1 } fn main() { let value = Rc.new(1); let task = fork use_value(value); }",
        "fn main() { let value = Rc.new(1); let callback = || { let held = value; 1 }; let task = fork callback(); }",
        "type Job {\n    run: fn() -> i64;\n}\n\nfn main() {\n    let value = Rc.new(1);\n    let job = Job { run: || {\n        let held = value;\n        1\n    } };\n    let task = fork job.run();\n}\n",
        "type Payload {\n    value: Rc<i64>;\n}\n\nfn use_value(value: Payload) -> i64 {\n    1\n}\n\nfn main() {\n    let value = Payload { value: Rc.new(1) };\n    let task = fork use_value(value);\n}\n",
        "fn use_value(value: (fn() -> i64, Rc<i64>)) -> i64 { 1 } fn main() { let value = (|| 1, Rc.new(1)); let task = fork use_value(value); }",
        "type Payload {\n    value: Rc<i64>;\n}\n\nimpl Payload {\n    fn run(self) -> i64 {\n        1\n    }\n}\n\nfn main() {\n    let value = Payload { value: Rc.new(1) };\n    let task = fork value.run();\n}\n",
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
        .any(|(key, fact)| key.module_idx == 0
            && source[key.start..key.end].trim() == "value"
            && fact.is_send
            && fact.is_sync
            && fact.acquisition == crate::ClosureCaptureAcquisition::Snapshot));
    let output = check_source("fn launch(value: string) { let task = fork { sleep(1ms); println(value); }; println(value); } fn main() {}");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .closure_capture_facts
        .values()
        .flatten()
        .any(|capture| capture.name == "value"
            && capture.acquisition == crate::ClosureCaptureAcquisition::Snapshot));
    let output = check_source("type Label {\n    value: string;\n}\n\nfn launch(data: bytes, label: Label) {\n    let task = fork {\n        println(label.value);\n        data\n    };\n    println(label.value);\n    let retained = data;\n}\n\nfn main() {}\n");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn fork_cannot_promote_an_affine_borrow() {
    let output = check_source("#[resource] type Socket {} impl Socket { fn close(consume self) {} } fn launch(socket: Socket) { let task = fork { socket.close(); }; } fn main() {}");
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
        if key.module_idx == 0 && matches!(source[key.start..key.end].trim(), "text" | "callback") {
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
        .any(|(key, fact)| key.module_idx == 0
            && source[key.start..key.end].trim() == "text"
            && fact.acquisition == crate::ClosureCaptureAcquisition::Move));
}

#[test]
fn fork_accepts_owning_arguments_and_send_callable_values() {
    for source in [
        "fn echo(value: string) -> string { value } fn main() { let task = fork echo(\"hello\"); let result = await task; }",
        "fn main() { let value = \"hello\"; let callback = || value; let task = fork callback(); let result = await task; }",
        "type Job {\n    run: fn() -> i64;\n}\n\nfn main() {\n    let job = Job { run: || 1 };\n    let task = fork job.run();\n    let result = await task;\n}\n",
        "fn work() -> i64 { 1 } fn main() { let callback = work; let task = fork callback(); let result = await task; }",
        "fn work(value: i64) -> i64 { value } fn main() { let tasks = fork [work(1), work(2)]; let result = await tasks; }",
    ] {
        let output = check_source(source);
        assert!(output.errors.is_empty(), "{source}: {:?}", output.errors);
    }
}

#[test]
fn plain_calls_inherit_suspension_transparently() {
    let source = "fn work() -> i64 { let task = fork { 7 }; await task }\nfn identity(x: i64) -> i64 { x }\nfn main() { let _x = identity(work()); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.warnings.is_empty(), "{:?}", output.warnings);
    assert_eq!(
        call_effects(&output, source, "work()"),
        vec![SuspensionEffect::MaySuspend]
    );
    assert_eq!(
        call_effects(&output, source, "identity(work())"),
        vec![SuspensionEffect::Never]
    );
    assert!(output
        .suspension_effects
        .bodies
        .iter()
        .any(|(body, effect)| matches!(body,
        crate::check::effects::EffectBody::Declaration(id) if output.defs.path(*id) == "main")
            && *effect == SuspensionEffect::MaySuspend));
}
#[test]
fn recursive_effects_follow_checked_calls() {
    let output = check_source("fn a(n: i64) -> i64 { if n == 0 { let t = fork { 1 }; return await t; } b(n - 1) } fn b(n: i64) -> i64 { a(n) } fn main() { let x = b(2); }");
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
fn effect_graph_shared_descendants_remain_synchronous() {
    let mut source = String::from("fn f0() -> i64 { 1 }\n");
    for level in 1..=26 {
        let child = level - 1;
        writeln!(source, "fn f{level}() -> i64 {{ f{child}() + f{child}() }}").unwrap();
    }
    source.push_str("fn main() { let _ = f26(); }\n");
    let output = check_source(&source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output
            .suspension_effects
            .calls
            .keys()
            .filter(|key| key.module_idx == 0)
            .count(),
        53
    );
    assert!(output
        .suspension_effects
        .calls
        .iter()
        .filter(|(key, _)| key.module_idx == 0)
        .all(|(_, effect)| *effect == SuspensionEffect::Never));
}

#[test]
fn effect_graph_ignores_opaque_data_origins_in_shared_cleanup_calls() {
    let mut source = String::from(
        "#[opaque] type Data {}\n#[resource] type Guard {}\nimpl Guard { fn close(consume self) {} }\nfn fresh(value: Data) -> Data { value }\nfn f0(value: Data) { let guard = Guard {}; }\n",
    );
    // Distinct call sites share descendants. Data provenance cannot change
    // Guard's close effect, so these calls must not multiply effect instances.
    for level in 1..=22 {
        let child = level - 1;
        writeln!(
            source,
            "fn f{level}(value: Data) {{ f{child}(fresh(value)); f{child}(fresh(value)); }}"
        )
        .unwrap();
    }
    source.push_str("fn main() {}\n");
    let output = check_source(&source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .calls
        .iter()
        .filter(|(key, _)| key.module_idx == 0)
        .all(|(_, effect)| *effect == SuspensionEffect::Never));
}

#[test]
fn effect_origins_follow_callables_extracted_from_linear_values() {
    let source = r"
#[resource] type Ticket { id: i64; }
impl Ticket { fn close(consume self) { sleep(1ms); } }
#[linear] type Pack<T> { callback: T; }
impl<T> Pack<T> { fn take(consume self) -> T { let Pack { callback } = self; callback } }
fn discard(consume pack: Pack<fn() -> i64>) { let callback = pack.take(); }
fn pure() -> i64 { 1 }
fn synchronous() { discard(Pack { callback: pure }); }
fn main() {
    let ticket = Ticket { id: 1 };
    let capturing = move || ticket.id;
    synchronous();
    discard(Pack { callback: capturing });
}
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        call_effects(&output, source, "discard(Pack { callback: pure })"),
        vec![SuspensionEffect::Never]
    );
    assert_eq!(
        call_effects(&output, source, "discard(Pack { callback: capturing })"),
        vec![SuspensionEffect::MaySuspend]
    );
}

#[test]
fn effect_graph_ignores_irrelevant_growing_generic_arguments() {
    let source = r"
fn recurse<T>(n: i64) {
    if n > 0 { recurse<Vec<T>>(n - 1); }
}
fn main() { recurse<i64>(1); }
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for expression in ["recurse<Vec<T>>(n - 1)", "recurse<i64>(1)"] {
        assert_eq!(
            call_effects(&output, source, expression),
            vec![SuspensionEffect::Never]
        );
    }
}

#[test]
fn effect_graph_recursive_cleanup_reaches_every_member() {
    let source = r"
#[resource]
type Ticket { id: i64; }
impl Ticket {
    fn close(consume self) { sleep(1ms); }
}
fn discard<T>(consume value: T) {}
fn first(n: i64) {
    if n > 0 { second(n - 1); }
    else { discard(Ticket { id: n }); }
}
fn second(n: i64) { third(n); }
fn third(n: i64) { first(n); }
fn main() { first(2); second(2); third(2); }
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for expression in [
        "first(n)",
        "second(n - 1)",
        "third(n)",
        "first(2)",
        "second(2)",
        "third(2)",
    ] {
        assert!(call_effects(&output, source, expression)
            .iter()
            .all(|effect| *effect == SuspensionEffect::MaySuspend));
    }
    let plain = source.replace("sleep(1ms);", "");
    assert_call_effect(&plain, "first(2)", SuspensionEffect::Never);
}

#[test]
fn effect_graph_generic_cleanup_keeps_concrete_close_arguments() {
    let declarations = r"
#[resource]
type Ticket { id: i64; }
impl Ticket {
    fn close(consume self) { sleep(1ms); }
}
#[resource]
type Wrapper<T> { value: T; }
impl<T> Wrapper<T> {
    fn close(consume self) { discard(self.value); }
}
fn discard<T>(consume value: T) {}
fn forward<T>(consume value: T) { discard(value); }
";
    for calls in [
        "forward(7); forward(Wrapper { value: 8 }); forward(Wrapper { value: Ticket { id: 1 } }); forward(9);",
        "forward(Wrapper { value: Ticket { id: 1 } }); forward(Wrapper { value: 8 }); forward(7); forward(9);",
    ] {
        let source = format!("{declarations}\nfn main() {{ {calls} }}");
        let output = check_source(&source);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        for expression in ["forward(7)", "forward(Wrapper { value: 8 })", "forward(9)"] {
            assert_eq!(
                call_effects(&output, &source, expression),
                vec![SuspensionEffect::Never]
            );
        }
        assert_eq!(
            call_effects(
                &output,
                &source,
                "forward(Wrapper { value: Ticket { id: 1 } })"
            ),
            vec![SuspensionEffect::MaySuspend]
        );
    }
}

#[test]
fn effect_graph_reusable_calls_do_not_release_once_captures() {
    let source = r"
#[resource]
type Ticket { id: i64; }
impl Ticket {
    fn close(consume self) { sleep(1ms); }
}
fn main() {
    let retained = Ticket { id: 1 };
    let reusable = move || retained.id;
    let _ = reusable();
    let _ = reusable();
    let ticket = Ticket { id: 2 };
    let once: fn[once,suspends]() -> Ticket = move || ticket;
    let result = once();
    result.close();
}
";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        call_effects(&output, source, "reusable()"),
        vec![SuspensionEffect::Never; 2]
    );
    assert_eq!(
        call_effects(&output, source, "once()"),
        vec![SuspensionEffect::MaySuspend]
    );
}

#[test]
fn pure_closure_calls_stay_synchronous() {
    let output = check_source("fn main() { let f = |x: i64| x + 1; let y = f(2); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .calls
        .iter()
        .filter(|(key, _)| key.module_idx == 0)
        .all(|(_, effect)| *effect == SuspensionEffect::Never));
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
        let value: Result<i64, string> = scope { Result.Ok(42) } handle failure {
            match failure {
                .Deadline { message } => .Err(message),
                .Fault { message } => .Err(message),
            }
        };
    }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn higher_order_effects_follow_the_written_parameter_type() {
    let source = "fn invoke(f: fn[suspends](i64) -> i64, x: i64) -> i64 { f(x) } fn pure(x: i64) -> i64 { x } fn work(x: i64) -> i64 { let t = fork { 1 }; x + await t } fn main() { let a = invoke(pure, 1); let b = invoke(work, 2); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for call in ["f(x)", "invoke(pure, 1)", "invoke(work, 2)"] {
        assert_eq!(
            call_effects(&output, source, call),
            vec![SuspensionEffect::MaySuspend],
            "{call}"
        );
    }
    let plain = source.replace("fn[suspends](i64)", "fn(i64)");
    let output = check_source(&plain);
    let start = plain.rfind("work, 2").unwrap();
    assert_eq!(
        output.errors.iter().map(|error| (error.span.clone(), error.message.as_str())).collect::<Vec<_>>(),
        vec![(start..start + "work".len(), "function `work` suspends via `fork`; `fn(i64) -> i64` never suspends, write `fn[suspends]`")]
    );
    assert_eq!(
        call_effects(&output, &plain, "f(x)"),
        vec![SuspensionEffect::Never]
    );
    assert_eq!(
        call_effects(&output, &plain, "invoke(pure, 1)"),
        vec![SuspensionEffect::Never]
    );
}
#[test]
fn builtin_sleep_effect_does_not_apply_to_a_source_shadow() {
    assert_call_effect(
        "fn work() { sleep(1ms); } fn main() { work(); }",
        "work()",
        SuspensionEffect::MaySuspend,
    );
    assert_call_effect(
        "fn sleep(value: i64) -> i64 { value } fn main() { let x = sleep(1); }",
        "sleep(1)",
        SuspensionEffect::Never,
    );
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
fn callable_fields_carry_their_written_effect() {
    let source = "type Job {\n    run: fn[suspends]() -> i64;\n    describe: fn() -> i64;\n}\n\nfn invoke(job: Job) -> i64 {\n    job.describe()\n}\n\nfn work() -> i64 {\n    let task = fork {\n        1\n    };\n    await task\n}\n\nfn main() {\n    let job = Job { run: work, describe: || 2 };\n    let value = invoke(job);\n}\n";
    assert_call_effect(source, "invoke(job)", SuspensionEffect::Never);
    assert_call_effect(
        &source.replace("job.describe()", "job.run()"),
        "invoke(job)",
        SuspensionEffect::MaySuspend,
    );
    let plain = source.replace("run: fn[suspends]()", "run: fn()");
    let output = check_source(&plain);
    let start = plain.rfind("run: work").unwrap() + "run: ".len();
    assert_eq!(
        output.errors.iter().map(|error| (error.span.clone(), error.message.as_str())).collect::<Vec<_>>(),
        vec![(start..start + "work".len(), "function `work` suspends via `fork`; `fn() -> i64` never suspends, write `fn[suspends]`")]
    );
}
#[test]
fn pure_callable_record_and_tuple_projections_are_synchronous() {
    let output = check_source("type Job {\n    run: fn() -> i64;\n}\n\nfn main() {\n    let job = Job { run: || 2 };\n    let a = job.run();\n    let pair = (|| 3, || 4);\n    let b = (pair.0)();\n}\n");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn collection_callbacks_use_their_own_effect() {
    let source = "fn work(x: i64) -> i64 { let task = fork { 1 }; x + await task } fn pure(x: i64) -> i64 { x } fn main() { let values = [1, 2]; let slow = values.map(work); let fast = values.map(pure); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        call_effects(&output, source, "values.map(work)"),
        vec![SuspensionEffect::MaySuspend]
    );
    assert_eq!(
        call_effects(&output, source, "values.map(pure)"),
        vec![SuspensionEffect::Never]
    );
}
#[test]
fn named_callback_arguments_follow_parameter_identity() {
    let output = check_source("fn invoke(f: fn(i64) -> i64, x: i64) -> i64 { f(x) } fn pure(x: i64) -> i64 { x } fn main() { let value = invoke(x: 1, f: pure); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn named_actor_handlers_publish_body_effects() {
    let output = check_source("actor Worker { receive fn run() { sleep(1ms); } } fn main() {}");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .suspension_effects
        .bodies
        .iter()
        .any(|(body, effect)| matches!(body,
        crate::check::effects::EffectBody::Declaration(id) if output.defs.path(*id) == "Worker::run")
            && *effect == SuspensionEffect::MaySuspend));
}

#[test]
fn method_callback_parameters_take_the_written_effect() {
    let source = "type Runner {} impl Runner { fn invoke(self, f: fn[suspends]() -> i64) -> i64 { f() } } fn main() { let runner = Runner {}; let value = runner.invoke(|| 3); }";
    assert_call_effect(source, "runner.invoke(|| 3)", SuspensionEffect::MaySuspend);
    assert_call_effect(
        &source.replace("fn[suspends]()", "fn()"),
        "runner.invoke(|| 3)",
        SuspensionEffect::Never,
    );
}
#[test]
fn assigning_a_suspending_function_to_a_plain_callable_field_is_rejected() {
    let source = "type Job {\n    run: fn() -> i64;\n}\n\nfn work() -> i64 {\n    let task = fork {\n        1\n    };\n    await task\n}\n\nfn main() {\n    var job = Job { run: || 2 };\n    job.run = work;\n    let value = job.run();\n}\n";
    let output = check_source(source);
    let start = source.rfind("= work").unwrap() + 2;
    assert_eq!(
        output.errors.iter().map(|error| (error.span.clone(), error.message.as_str())).collect::<Vec<_>>(),
        vec![(start..start + "work".len(), "function `work` suspends via `fork`; `fn() -> i64` never suspends, write `fn[suspends]`")]
    );
    assert_call_effect(
        &source.replace("run: fn()", "run: fn[suspends]()"),
        "job.run()",
        SuspensionEffect::MaySuspend,
    );
}
#[test]
fn a_later_loop_assignment_cannot_hide_suspension() {
    let source = "fn work() -> i64 { let task = fork { 1 }; await task } fn main() { var callback: fn() -> i64 = || 2; for i in 0..2 { let value = callback(); callback = work; } }";
    let output = check_source(source);
    let start = source.rfind("= work").unwrap() + 2;
    assert_eq!(
        output.errors.iter().map(|error| (error.span.clone(), error.message.as_str())).collect::<Vec<_>>(),
        vec![(start..start + "work".len(), "function `work` suspends via `fork`; `fn() -> i64` never suspends, write `fn[suspends]`")]
    );
    assert_call_effect(
        &source.replace("callback: fn()", "callback: fn[suspends]()"),
        "callback()",
        SuspensionEffect::MaySuspend,
    );
}

#[test]
fn plain_callable_obligations_inside_a_closure_retain_their_construction_context() {
    let pure = "fn main() { let outer = || { let inner: fn() -> i64 = || { 1 }; inner() }; println(outer()); }";
    let output = check_source(pure);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let suspending = pure.replace("|| { 1 }", "|| { sleep(1ms); 1 }");
    let output = check_source(&suspending);
    assert_eq!(output.errors.len(), 1, "{:?}", output.errors);
    let error = &output.errors[0];
    assert_eq!(&suspending[error.span.clone()], "|| { sleep(1ms); 1 }");
    assert!(
        error.message.contains("closure suspends via `sleep(...)`"),
        "{}",
        error.message
    );
}

#[test]
fn closure_effects_follow_the_literal_through_bindings_and_joins() {
    let source = "fn main() -> i64 { let call = || 5; let picked = if true { call } else { call }; println(picked()); 0 }";
    assert_call_effect(source, "picked()", SuspensionEffect::Never);
    let slow = "fn main() { let call = || { sleep(1ms); 5 }; let picked = if true { call } else { call }; let value = picked(); }";
    assert_call_effect(slow, "picked()", SuspensionEffect::MaySuspend);
}

#[test]
fn distinct_closures_join_into_a_written_type_with_obligations() {
    let source = "fn main() { let flag = true; let picked = if flag { || 1 } else { || 2 }; let value = picked(); }";
    assert_call_effect(source, "picked()", SuspensionEffect::Never);
    let slow = source.replace("|| 1", "|| { sleep(1ms); 1 }");
    let output = check_source(&slow);
    assert_eq!(
        output.errors.iter().map(|error| error.message.as_str()).collect::<Vec<_>>(),
        vec!["closure suspends via `sleep(...)`; `fn[clone]() -> i64` never suspends, write `fn[suspends]`"]
    );
    let annotated = slow.replace("let picked =", "let picked: fn[suspends]() -> i64 =");
    assert_call_effect(&annotated, "picked()", SuspensionEffect::MaySuspend);
}

#[test]
fn reassigning_a_closure_binding_joins_it_to_the_shape_both_hold() {
    // Each closure literal has its own type; an inferred `var` binding widens
    // to the callable shape both literals satisfy, so no annotation is needed.
    let output = check_source("fn main() { var f = || 1; f = || 2; let value = f(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output =
        check_source("fn main() { var f: fn() -> i64 = || 1; f = || 2; let value = f(); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    // A closure of a different shape has no join to widen to.
    let output = check_source("fn main() { var f = || 1; f = |n: i64| n; }");
    let error = output
        .errors
        .first()
        .expect("a closure of another shape has no shared type");
    assert_eq!(
        error.kind,
        crate::error::TypeErrorKind::ClosureShapeMismatch
    );
    assert!(error.message.contains("compatible callable shapes"));
}

#[test]
fn generic_instantiation_keeps_closure_effects() {
    let source = "type Holder<T> {\n    value: T;\n}\n\nfn main() {\n    let holder = Holder { value: || {\n        sleep(1ms);\n        1\n    } };\n    let value = holder.value();\n}\n";
    assert_call_effect(source, "holder.value()", SuspensionEffect::MaySuspend);
    assert_call_effect(
        &source.replace("sleep(1ms);", ""),
        "holder.value()",
        SuspensionEffect::Never,
    );
}

#[test]
fn suspending_callable_types_do_not_coerce_into_plain_ones() {
    let output = check_source("fn accept(f: fn() -> i64) -> i64 { f() } fn forward(f: fn[suspends]() -> i64) -> i64 { accept(f) } fn main() {}");
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, crate::error::TypeErrorKind::Mismatch { .. })),
        "{:?}",
        output.errors
    );
    let source = "fn accept(f: fn[suspends]() -> i64) -> i64 { f() } fn forward(f: fn() -> i64) -> i64 { accept(f) } fn main() {}";
    assert_call_effect(source, "accept(f)", SuspensionEffect::MaySuspend);
}

#[test]
fn deferred_bodies_name_the_suspending_call() {
    let output = check_source("fn work() { sleep(1ms); } fn main() { defer { work(); } }");
    assert_eq!(
        output
            .errors
            .iter()
            .map(|error| error.message.as_str())
            .collect::<Vec<_>>(),
        vec!["a deferred body cannot suspend: `work(...)` may suspend"]
    );
}

#[test]
fn await_on_a_plain_call_is_refused_and_await_on_a_value_is_rejected() {
    let source = "fn work() -> i64 { sleep(1ms); 1 } fn main() { let x = await work(); }";
    let output = check_source(source);
    let error = output.errors.first().expect("redundant await is refused");
    assert_eq!(&source[error.span.clone()], "work()");
    assert_eq!(
        error.message,
        "`await` on a plain call adds nothing: the call suspends on its own"
    );
    assert_eq!(
        error.suggestions,
        vec!["remove `await`, or fork the call to run it concurrently".to_string()]
    );
    let plain = source.replace("await ", "");
    let output = check_source(&plain);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        call_effects(&output, &plain, "work()"),
        vec![SuspensionEffect::MaySuspend]
    );
    let output = check_source("fn main() { let n: i64 = 1; let x = await n; }");
    assert_eq!(
        output
            .errors
            .iter()
            .map(|error| error.message.as_str())
            .collect::<Vec<_>>(),
        vec!["`await` joins a task; `i64` is not one"]
    );
    let output = check_source("actor Worker { receive fn value() -> i64 { 41 } } fn main() { let worker = spawn Worker; let _reply = worker.value(); let task = fork { 1 }; let _joined = await task; let callback = actor |n: i64| -> i64 { n }; let _answer = callback(1); stop(worker); stopped(worker); }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.warnings.is_empty(), "{:?}", output.warnings);
}

#[test]
fn fork_bodies_prove_send_through_captured_record_fields() {
    let output = check_source("type Job {\n    run: fn() -> i64;\n}\n\nfn main() {\n    let job = Job { run: || 1 };\n    let task = fork {\n        job.run();\n    };\n}\n");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source("type Job {\n    run: fn() -> i64;\n}\n\nfn main() {\n    let value = Rc.new(1);\n    let job = Job { run: move || {\n        let held = value;\n        1\n    } };\n    let task = fork {\n        job.run();\n    };\n}\n");
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, crate::error::TypeErrorKind::InvalidSend)),
        "{:?}",
        output.errors
    );
}

#[test]
fn collection_callbacks_follow_a_written_parameter_type() {
    let source =
        "fn apply(f: fn(i64) -> i64, values: Vec<i64>) -> Vec<i64> { values.map(f) } fn main() {}";
    assert_call_effect(source, "values.map(f)", SuspensionEffect::Never);
    assert_call_effect(
        &source.replace("f: fn(i64)", "f: fn[suspends](i64)"),
        "values.map(f)",
        SuspensionEffect::MaySuspend,
    );
}

#[test]
fn array_literals_join_distinct_callables_into_their_written_type() {
    let source = "fn double(x: i64) -> i64 { x * 2 } fn triple(x: i64) -> i64 { x * 3 } fn main() { let fns = [double, triple]; let closures = [|x: i64| x + 1, |x: i64| x + 2]; let value = fns[0](1) + closures[1](1); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let slow = source.replace("|x: i64| x + 2", "|x: i64| { sleep(1ms); x }");
    let output = check_source(&slow);
    assert_eq!(
        output.errors.iter().map(|error| error.message.as_str()).collect::<Vec<_>>(),
        vec!["closure suspends via `sleep(...)`; `fn[clone](i64) -> i64` never suspends, write `fn[suspends]`"]
    );
}

#[test]
fn ok_coerced_tails_keep_the_written_callable_contract() {
    let output = check_source("fn make() -> (fn() -> i64) fails string { || 1 } fn main() {}");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let output = check_source(
        "fn make() -> (fn() -> i64) fails string { || { sleep(1ms); 1 } } fn main() {}",
    );
    assert_eq!(
        output
            .errors
            .iter()
            .map(|error| error.message.as_str())
            .collect::<Vec<_>>(),
        vec![
            "closure suspends via `sleep(...)`; `fn() -> i64` never suspends, write `fn[suspends]`"
        ]
    );
}

const DYN_TICKER: &str = "trait Ticker {\n    fn tick(self) -> i64;\n}\n\ntype Clock {\n    n: i64;\n}\n\nimpl Ticker for Clock {\n    fn tick(self) -> i64 {\n        self.n\n    }\n}\n\nfn run(t: dyn Ticker) {\n    defer {\n        println(t.tick());\n    }\n    println(t.tick());\n}\n\nfn main() {\n    run(Clock { n: 1 });\n}\n";

const DYN_GENERIC_CLEANUP: &str = r"
trait Finish { fn finish(consume self); }
type Parcel<T> { value: T; }
impl<T> Finish for Parcel<T> {
    fn finish(consume self) {
        let Parcel { value } = self;
        discard(value);
    }
}
fn discard<T>(consume value: T) {}
fn erase<U>(consume value: Parcel<U>) -> dyn Finish { value }
#[resource]
type Ticket {}
impl Ticket { fn close(consume self) { sleep(1ms); } }
";

#[test]
fn pure_dyn_filler_does_not_erase_generic_cleanup_effect() {
    let source = format!(
        "{DYN_GENERIC_CLEANUP}\nfn main() {{ let value = erase(Parcel {{ value: 7 }}); value.finish(); }}"
    );
    let output = check_source(&source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(
        output
            .suspension_effects
            .bodies
            .iter()
            .any(|(body, effect)| {
                matches!(body, crate::check::effects::EffectBody::Declaration(id)
            if output.defs.path(*id) == "Parcel::<impl Finish for Parcel<T>>::finish")
                    && *effect == SuspensionEffect::MaySuspend
            }),
        "a pure concrete filler must not overwrite the generic body's effect"
    );
}

#[test]
fn dyn_filler_keeps_suspending_cleanup_in_a_generic_erasure() {
    for calls in [
        "let plain = erase(Parcel { value: 7 }); let slow = erase(Parcel { value: Ticket {} });",
        "let slow = erase(Parcel { value: Ticket {} }); let plain = erase(Parcel { value: 7 });",
    ] {
        let source = format!("{DYN_GENERIC_CLEANUP}\nfn main() {{ {calls} }}");
        let output = check_source(&source);
        assert_eq!(output.errors.len(), 1, "{:?}", output.errors);
        let error = &output.errors[0];
        assert!(error.message.contains("E_DYN_SLOT_SUSPENDS"), "{error:?}");
        let rejected = &source[error.span.clone()];
        assert!(rejected.contains("Ticket {}"), "{rejected}: {error:?}");
    }
}

#[test]
fn dyn_slot_effect_follows_the_trait_method_declaration() {
    // A plain trait method is a written boundary that never suspends, so a
    // dispatch through it may run in a deferred body (A421).
    assert_call_effect(DYN_TICKER, "t.tick()", SuspensionEffect::Never);
    let source = "enum E { Bad; }\nimpl Display for E { fn fmt(self) -> string { \"Bad\" } }\nimpl Error for E {}\nfn run(e: dyn Error) { defer { println(e.fmt()); } println(\"body\"); }\nfn main() { let e: dyn Error = E.Bad; run(e); }\n";
    assert_call_effect(source, "e.fmt()", SuspensionEffect::Never);

    // A `fn[suspends]` slot may suspend, so a deferred dispatch is refused.
    let suspends = DYN_TICKER.replace("fn tick(self) -> i64;", "fn[suspends] tick(self) -> i64;");
    let output = check_source(&suspends);
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("a deferred body cannot suspend: `tick(...)`")),
        "{:?}",
        output.errors
    );
}

#[test]
fn a_suspending_impl_cannot_fill_a_plain_dyn_slot() {
    let suspending_impl = DYN_TICKER.replace(
        "    fn tick(self) -> i64 {\n        self.n",
        "    fn tick(self) -> i64 {\n        sleep(1ms);\n        self.n",
    );
    let output = check_source(&suspending_impl);
    let start = suspending_impl.rfind("Clock { n: 1 }").unwrap();
    assert_eq!(
        output
            .errors
            .iter()
            .map(|error| (error.span.clone(), error.message.as_str()))
            .collect::<Vec<_>>(),
        vec![(
            start..start + "Clock { n: 1 }".len(),
            "E_DYN_SLOT_SUSPENDS: function `Clock::<impl Ticker for Clock>::tick` suspends via \
             `sleep(...)`, so it cannot fill `Ticker::tick` in `dyn Ticker`; that trait method \
             never suspends unless it is declared `fn[suspends]`"
        )]
    );

    // Declared `fn[suspends]`, the slot admits the impl; only the deferred
    // dispatch is refused.
    let declared = suspending_impl
        .replace("fn tick(self) -> i64;", "fn[suspends] tick(self) -> i64;")
        .replace("    defer {\n        println(t.tick());\n    }\n", "");
    let output = check_source(&declared);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        call_effects(&output, &declared, "t.tick()"),
        vec![SuspensionEffect::MaySuspend]
    );
}

#[test]
fn the_trait_object_layout_publishes_closure_slots_and_effects() {
    let output = check_source(&DYN_TICKER.replace(
        "fn tick(self) -> i64;",
        "fn tick(self) -> i64;\n    fn[suspends] wait(var self);",
    ));
    let layouts: Vec<_> = output.trait_object_layouts.values().collect();
    assert_eq!(layouts.len(), 1, "{layouts:?}");
    let layout = layouts[0];
    assert_eq!(layout.closure.len(), 1);
    let shape: Vec<_> = layout
        .slots
        .iter()
        .map(|slot| {
            (
                slot.receiver,
                slot.effect,
                slot.declaring_trait == layout.closure[0],
            )
        })
        .collect();
    assert_eq!(
        shape,
        vec![
            (
                crate::check::DynReceiver::Borrow,
                crate::check::SlotEffect::Plain,
                true
            ),
            (
                crate::check::DynReceiver::BorrowMut,
                crate::check::SlotEffect::Suspends,
                true
            ),
        ]
    );
    assert_eq!(layout.slot_of(layout.slots[1].method), Some(1));
}

#[test]
fn inline_recursive_cleanup_preserves_the_source_diagnostic() {
    let source = r"
type Grow<T> {
    tag: i64;
    sub: Grow<Grow<i64>>;
}
fn main() {
    let value: Grow<i64> = Grow { tag: 1, sub: missing };
}
";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("undefined variable `missing`")),
        "{:?}",
        output.errors
    );
}
