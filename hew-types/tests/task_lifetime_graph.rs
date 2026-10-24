use std::fmt::Write as _;

mod common;

fn assert_scope_escape(source: &str) {
    let output = common::typecheck_isolated(source);
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("a task handle cannot outlive its scope")),
        "expected task lifetime rejection: {:#?}",
        output.errors
    );
}

#[test]
fn shared_nominal_member_graph_proves_task_absence_once() {
    let mut source = String::from("type T0 { value: i64; }\n");
    for level in 1..=28 {
        writeln!(
            source,
            "type T{level} {{ left: T{previous}; right: T{previous}; }}",
            previous = level - 1
        )
        .unwrap();
    }
    source.push_str("fn inspect(value: T28) { scope { let task = fork { 1 }; let _ = await task; }; } fn main() {}");
    let output = common::typecheck_isolated(&source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn shared_callable_return_graph_keeps_all_origins() {
    let mut source = String::from("fn f0() -> fn() -> i64 { || 7 }\n");
    for level in 1..=28 {
        writeln!(source,
            "fn f{level}() -> fn() -> i64 {{ if true {{ f{previous}() }} else {{ f{previous}() }} }}",
            previous = level - 1
        ).unwrap();
    }
    source.push_str("fn main() { let task = fork { 1 }; let callback = f28(); println(callback()); let _ = await task; }");
    let output = common::typecheck_isolated(&source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn shared_forwarding_graph_retains_a_captured_task() {
    let callback = "fn[once,suspends]() -> i64";
    let mut source = format!("fn f0(consume job: {callback}) -> {callback} {{ job }}\n");
    for level in 1..=24 {
        writeln!(source,
            "fn f{level}(consume job: {callback}) -> {callback} {{ if true {{ f{previous}(job) }} else {{ f{previous}(job) }} }}",
            previous = level - 1
        ).unwrap();
    }
    write!(source,
        "fn make() -> {callback} {{ let task = fork {{ 7 }}; f24(move || {{ await task }}) }} fn main() {{ let _ = make(); }}"
    ).unwrap();
    assert_scope_escape(&source);
}

#[test]
fn separate_generic_calls_keep_their_actual_argument_origins() {
    assert_scope_escape(
        "fn identity<T>(consume value: T) -> T { value }
         fn make() -> fn[once,suspends]() -> i64 {
             let safe = identity(move || { let child = fork { 1 }; await child });
             let _ = safe();
             let task = fork { 7 };
             identity(move || { await task })
         }
         fn main() { let _ = make(); }",
    );
}

#[test]
fn generic_fork_callback_requires_send_when_actual_values_arrive() {
    let declaration = "fn run<T: Send>(consume value: T, callback: fn[clone](T) -> i64) -> i64 {
        let task = fork callback(value);
        await task
    }";
    let output = common::typecheck(declaration);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let safe = format!("{declaration} fn main() {{ let _ = run(1, |item: i64| item); }}");
    let output = common::typecheck(&safe);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let unsafe_capture = format!(
        "{declaration} fn main() {{
        let local = Rc.new(7);
        let _ = run(1, move |item: i64| item + local.strong_count());
    }}"
    );
    let output = common::typecheck(&unsafe_capture);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == hew_types::error::TypeErrorKind::InvalidSend),
        "{:#?}",
        output.errors
    );
}

#[test]
fn exact_field_projection_does_not_include_an_unreturned_capture() {
    let source = "type Jobs {
        safe: fn[once,suspends]() -> i64;
        scoped: fn[once,suspends]() -> i64;
    }
    fn make() -> fn[once,suspends]() -> i64 {
        let task = fork { 7 };
        let jobs = Jobs {
            safe: move || { let child = fork { 1 }; await child },
            scoped: move || { await task }
        };
        jobs.safe
    }
    fn main() { let callback = make(); let _ = callback(); }";
    let output = common::typecheck_isolated(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_scope_escape(&source.replace("jobs.safe\n", "jobs.scoped\n"));
}

#[test]
fn repeated_closure_factory_preserves_each_capture_environment() {
    let source = "type Jobs {
        safe: fn[once,suspends]() -> i64;
        scoped: fn[once,suspends]() -> i64;
    }
    fn capture(consume callback: fn[once,suspends]() -> i64) -> fn[once,suspends]() -> i64 {
        move || { callback() }
    }
    fn make() -> fn[once,suspends]() -> i64 {
        let task = fork { 7 };
        let jobs = Jobs {
            safe: capture(move || { 1 }),
            scoped: capture(move || { await task })
        };
        jobs.safe
    }
    fn main() { let callback = make(); let _ = callback(); }";
    let output = common::typecheck_isolated(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_scope_escape(&source.replace("jobs.safe\n", "jobs.scoped\n"));
}

#[test]
fn recursive_projection_graph_keeps_a_reachable_capture() {
    assert_scope_escape(include_str!(
        "../../tests/core-acceptance/cases/task-recursive-callback-escape.hew"
    ));
}

#[test]
fn recursive_projection_graph_allows_task_free_callbacks() {
    let source =
        include_str!("../../tests/core-acceptance/cases/task-recursive-callback-escape.hew")
            .replace("move || { await task }", "move || { 7 }");
    let output = common::typecheck_isolated(&source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn recursive_rebinding_keeps_the_original_argument_environment() {
    let source = "fn choose(consume first: fn[once,suspends]() -> i64, consume second: fn[once,suspends]() -> i64, again: bool) -> fn[once,suspends]() -> i64 {
        if again {
            choose(second, first, false)
        } else {
            let factory = move || { move || { first() } };
            factory()
        }
    }
    fn make() -> fn[once,suspends]() -> i64 {
        let task = fork { 7 };
        choose(move || { 1 }, move || { await task }, true)
    }
    fn main() { let _ = make(); }";
    assert_scope_escape(source);
    let safe = source.replace("move || { await task }", "move || { 7 }");
    let output = common::typecheck_isolated(&safe);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn recursive_projection_growth_without_formals_reaches_a_fixed_point() {
    let source = "type Node {
        next: Vec<Node>;
        callback: fn[clone]() -> i64;
    }
    fn again() -> Node { clone again().next[0] }
    fn main() {
        let task = fork { 1 };
        let _ = again();
        let _ = await task;
    }";
    let output = common::typecheck_isolated(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}
