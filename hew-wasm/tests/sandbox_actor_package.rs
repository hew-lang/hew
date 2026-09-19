//! The package carries what an actor program needs before the VM runs one.
//!
//! Actors still reach the AST emitter through the dispatch, so these pin the
//! walker directly: the tables and operations are what the VM will be fed, and
//! a gap here would otherwise only surface when the dispatch flips.

use hew_wasm::sandbox::sir_emit;

fn semantics(source: &str) -> hew_sir::LoweredModule {
    let state = hew_compile::run_source_frontend(
        source,
        "actor_package.hew",
        &hew_compile::FrontendOptions::default(),
    );
    let tco = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .expect("the program type-checks");
    hew_compile::Session::new(
        hew_compile::SessionTarget::browser(),
        hew_compile::DiagnosticPolicy::default(),
    )
    .lower_program(&state.program, tco)
    .expect("the program lowers")
    .into_semantics()
}

const COUNTER: &str = r"
actor Counter {
    var count: i64,
    receive fn bump(n: i64) -> i64 { count = count + n; count }
}

fn main() {
    let c = spawn Counter(count: 0);
    println(match c.bump(3) { .Ok(v) => v, .Err(_e) => 0 - 1 });
}
";

#[test]
fn an_actor_declaration_reaches_the_package_with_its_handlers() {
    let module = semantics(COUNTER);
    let package = sir_emit::emit_package(&module.module, "sandbox-vm-export", "0", "test")
        .expect("an actor module projects into a package");

    let actor = package
        .actors
        .first()
        .expect("the actor reaches the package");
    assert_eq!(actor.state_fields.len(), 1, "one state seat");
    assert!(
        actor.state_fields[0].mutable,
        "`var count` is a mutable seat"
    );
    let handler = actor
        .handlers
        .first()
        .expect("the handler reaches the package");
    assert_eq!(handler.name, "bump");
    assert_eq!(handler.params, 1, "one message parameter");
    assert!(
        package.functions.get(handler.callable as usize).is_some(),
        "the handler names a function in this package"
    );
}

#[test]
fn an_actor_call_names_its_operation_rather_than_a_symbol() {
    let module = semantics(COUNTER);
    let package = sir_emit::emit_package(&module.module, "sandbox-vm-export", "0", "test")
        .expect("an actor module projects into a package");

    let operations: Vec<String> = package
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter(|block| block.term["op"] == "actor.call")
        .filter_map(|block| block.term["operation"]["op"].as_str().map(str::to_string))
        .collect();
    assert!(
        operations.iter().any(|op| op == "spawn"),
        "spawning the actor is an operation the package names: {operations:?}"
    );
    assert!(
        !operations.is_empty(),
        "an actor program reaches at least one actor operation"
    );
}

fn execute(source: &str) -> serde_json::Value {
    execute_expected(source, "ok")
}

fn execute_expected(source: &str, status: &str) -> serde_json::Value {
    let compiled =
        hew_wasm::sandbox::compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile through the public browser entry point");
    assert!(
        compiled
            .diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != "error"),
        "{:?}",
        compiled.diagnostics
    );
    let package = compiled.bytecode.expect("verified source emits bytecode");
    assert_eq!(package.schema_version, "hew.sandbox.bytecode.v1");
    let file = tempfile::NamedTempFile::new().expect("package file");
    serde_json::to_writer(file.as_file(), &package).expect("serialize package");
    let output = std::process::Command::new("node")
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/../hew-sandbox-vm"))
        .args([
            "--input-type=module",
            "-e",
            r"
            import fs from 'node:fs';
            import { runBytecode } from './dist/interpreter/index.js';
            const package_ = JSON.parse(fs.readFileSync(process.argv[1], 'utf8'));
            process.stdout.write(JSON.stringify(runBytecode(package_)));
        ",
        ])
        .arg(file.path())
        .output()
        .expect("run the package in Node");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let trace: serde_json::Value = serde_json::from_slice(&output.stdout).expect("VM trace");
    assert_eq!(trace["result"], status, "{trace:#}");
    trace
}

fn stdout(trace: &serde_json::Value) -> String {
    trace["final_state"]["stdout"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_str().unwrap())
        .collect()
}

#[test]
fn actor_state_is_shared_between_completed_turns() {
    let trace = execute(
        r"
actor Counter {
    var count: i64,
    receive fn bump(n: i64) -> i64 { count = count + n; count }
}
fn main() {
    let counter = spawn Counter(count: 2);
    println(match counter.bump(3) { .Ok(n) => n, .Err(_) => -1 });
    println(match counter.bump(4) { .Ok(n) => n, .Err(_) => -1 });
}
",
    );
    assert_eq!(stdout(&trace), "5\n9\n");
}

#[test]
fn a_parked_handler_resumes_with_its_state_after_another_actor_replies() {
    let trace = execute(
        r"
actor Doubler {
    receive fn twice(n: i64) -> i64 { n * 2 }
}
actor Relay {
    let worker: Doubler,
    var calls: i64,
    receive fn forward(n: i64) -> i64 {
        calls = calls + 1;
        let result = match worker.twice(n) { .Ok(value) => value, .Err(_) => -1 };
        result + calls
    }
}
fn main() {
    let worker = spawn Doubler;
    let relay = spawn Relay(worker: worker, calls: 0);
    println(match relay.forward(5) { .Ok(n) => n, .Err(_) => -1 });
    println(match relay.forward(7) { .Ok(n) => n, .Err(_) => -1 });
}
",
    );
    assert_eq!(stdout(&trace), "11\n16\n");
}

#[test]
fn a_handler_fault_is_contained_and_later_calls_observe_the_dead_actor() {
    let trace = execute(
        r#"
actor Worker {
    receive fn fail() -> i64 { panic("worker failed"); }
}
fn main() {
    let worker = spawn Worker;
    println(match worker.fail() {
        .Err(.Trapped) => "contained",
        _ => "wrong outcome",
    });
    println(match worker.fail() {
        .Err(.Dead) => "dead",
        _ => "wrong outcome",
    });
}
"#,
    );
    assert_eq!(stdout(&trace), "contained\ndead\n");
}

#[test]
fn supervisor_children_execute_their_declared_spawn_functions() {
    let trace = execute(include_str!(
        "../../examples/playground/concurrency/supervisor.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "Worker 1 processing 10\nWorker 2 processing 20\nw1 -> 10\nw2 -> 20\n"
    );
}

#[test]
fn a_supervised_role_resolves_the_fresh_state_after_a_fault() {
    let trace = execute(
        r#"
actor Worker {
    var count: i64,
    receive fn bump() -> i64 { count = count + 1; count }
    receive fn fail() { panic("restart me"); }
}
supervisor Tree {
    strategy: one_for_one,
    intensity: 3 within 10s,
    child worker: Worker(count: 10),
}
fn main() {
    let tree = spawn Tree;
    let role = tree.worker;
    println(match role.bump() { .Ok(n) => n, .Err(_) => -1 });
    let _ = role.fail();
    println(match role.bump() { .Ok(n) => n, .Err(_) => -1 });
}
"#,
    );
    assert_eq!(stdout(&trace), "11\n11\n");
}

#[test]
fn close_waits_for_the_stop_hook() {
    let trace = execute(
        r#"
actor Worker {
    #[on(stop)]
    fn stopped() { println("stopped"); }
    receive fn work() { println("worked"); }
}
fn main() {
    let worker = spawn Worker;
    let _ = worker.work();
    close(worker);
    println("closed");
}
"#,
    );
    assert_eq!(stdout(&trace), "worked\nstopped\nclosed\n");
}

#[test]
fn a_declared_handler_failure_is_the_callers_typed_error() {
    let trace = execute(
        r#"
actor Worker {
    receive fn work(n: i64) -> i64 fails string {
        if n < 0 { return error "negative"; }
        n * 2
    }
}
fn main() {
    let worker = spawn Worker;
    println(match worker.work(-1) { .Err(.Failed(message)) => message, _ => "wrong outcome" });
    println(match worker.work(5) { .Ok(n) => n, .Err(_) => -1 });
}
"#,
    );
    assert_eq!(stdout(&trace), "negative\n10\n");
}

#[test]
fn forked_tasks_publish_values_and_a_scope_joins_discarded_handles() {
    let trace = execute(
        r#"
fn double(n: i64) -> i64 { sleep(1ms); n * 2 }
fn main() {
    scope {
        let first = fork double(3);
        let second = fork double(4);
        println(await first);
        println(await second);
        fork { sleep(2ms); println("drained"); };
    }
    println("scope finished");
}
"#,
    );
    assert_eq!(stdout(&trace), "6\n8\ndrained\nscope finished\n");
}

#[test]
fn select_keeps_a_losing_task_joinable() {
    let trace = execute(
        r#"
fn delayed(n: i64, delay: duration) -> i64 { sleep(delay); n }
fn main() {
    scope {
        let slow = fork delayed(2, 20ms);
        let fast = fork delayed(1, 1ms);
        select {
            value from slow => println(value),
            value from fast => { println(value); println(await slow); },
        }
    }
}
"#,
    );
    assert_eq!(stdout(&trace), "1\n2\n");
}

#[test]
fn select_waits_on_actor_completions() {
    let trace = execute(
        r#"
actor Worker {
    let delay: duration,
    receive fn work(n: i64) -> i64 { sleep(delay); n }
}
fn main() {
    let slow = spawn Worker(delay: 20ms);
    let fast = spawn Worker(delay: 1ms);
    select {
        result from slow.work(2) => println(result.expect("slow")),
        result from fast.work(1) => println(result.expect("fast")),
    }
}
"#,
    );
    assert_eq!(stdout(&trace), "1\n");
}

#[test]
fn an_actor_handler_joins_parallel_requests_without_reentering_its_state() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/scope-result-task-handle.hew"
    ));
    assert_eq!(stdout(&trace), "18\n");
}

#[test]
fn pipe_send_and_receive_resume_through_backpressure() {
    let trace = execute(
        r#"
import std.stream;
fn main() {
    let (sink, input): (stream.Sink<string>, stream.Stream<string>) =
        stream.pipe(1).expect("pipe");
    scope {
        fork {
            sink.send("first").expect("send");
            sink.send("second").expect("send");
            sink.close();
        };
        println(input.recv().expect("first"));
        println(input.recv().expect("second"));
        println(match input.recv() { .None => "EOF", .Some(_) => "extra" });
        input.close();
    }
}
"#,
    );
    assert_eq!(stdout(&trace), "first\nsecond\nEOF\n");
}

#[test]
fn selection_timeout_leaves_the_pipe_read_half_usable() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/select-timer-arm.hew"
    ));
    assert_eq!(stdout(&trace), "timeout\nreceived late\nclosed\n");
}

#[test]
fn actor_task_and_pipe_selection_compose_without_consuming_losers() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/select-actor-call-arm.hew"
    ));
    assert_eq!(stdout(&trace), "OWNED ACTOR REPLY\ntask still joinable\nreceiver still usable\nreceiver evaluated once\nprepared receiver\n");
}

#[test]
fn a_scope_deadline_runs_cleanup_before_recovery() {
    let trace = execute(
        r#"
fn main() {
    let result = scope within 2ms {
        defer println("cleaned");
        sleep(10s);
        "unreachable"
    } handle failure {
        match failure {
            .Deadline { message } => "deadline",
            .Fault { message } => "wrong failure",
        }
    };
    println(result);
}
"#,
    );
    assert_eq!(stdout(&trace), "cleaned\ndeadline\n");
    assert_eq!(trace["final_state"]["virtual_clock"]["current_ms"], 2);
}

#[test]
fn race_cancels_and_drains_the_loser_before_returning() {
    let trace = execute(
        r#"
fn delayed(value: i64, delay: duration) -> i64 {
    defer println(value);
    sleep(delay);
    value
}
fn main() {
    println(race { delayed(1, 1ms), delayed(2, 10s) });
}
"#,
    );
    assert_eq!(stdout(&trace), "1\n2\n1\n");
    assert_eq!(trace["final_state"]["virtual_clock"]["current_ms"], 1);
}

#[test]
fn a_task_fault_reaches_scope_recovery_after_deferred_cleanup() {
    let trace = execute(
        r#"
fn fail() -> i64 {
    defer println("child cleaned");
    panic("child failed");
}
fn main() {
    scope {
        defer println("parent cleaned");
        let task = fork fail();
        println(await task);
    } handle failure {
        match failure {
            .Fault { message } => println(message),
            .Deadline { message } => println("unexpected deadline"),
        }
    };
}
"#,
    );
    assert_eq!(
        stdout(&trace),
        "child cleaned\nparent cleaned\nchild failed\n"
    );
}

#[test]
fn mailbox_submission_reports_acceptance_and_full_rejection() {
    let trace = execute(
        r#"
actor Worker {
    mailbox 1 overflow block,
    receive fn work(value: i64) { println(value); }
}
actor Driver {
    receive fn run(worker: Worker) {
        let inbox = mailbox(worker, on_full: .Reject);
        println(match inbox.work(1) {
            .Ok(.Accepted) => "accepted",
            _ => "wrong first outcome",
        });
        println(match inbox.work(2) {
            .Err(failure) => match failure.reason { .Full => "full", _ => "wrong reason" },
            _ => "wrong second outcome",
        });
    }
}
fn main() {
    let worker = spawn Worker;
    let driver = spawn Driver;
    let _ = driver.run(worker);
}
"#,
    );
    assert_eq!(stdout(&trace), "accepted\nfull\n1\n");
}

#[test]
fn waiting_submission_resumes_when_a_mailbox_slot_opens() {
    let trace = execute(
        r#"
actor Worker {
    mailbox 1 overflow block,
    receive fn work(value: i64) { println(value); }
}
actor Driver {
    receive fn run(worker: Worker) {
        let inbox = mailbox(worker, on_full: .Wait);
        let _ = inbox.work(1);
        let _ = inbox.work(2);
        println("submitted");
    }
}
fn main() {
    let worker = spawn Worker;
    let driver = spawn Driver;
    let _ = driver.run(worker);
}
"#,
    );
    assert_eq!(stdout(&trace), "1\nsubmitted\n2\n");
}

#[test]
fn a_clean_actor_close_releases_every_forked_pipe_producer() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/actor-fork-sink-clean-close.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "handler returned\nfirst=1\nsecond=2\nclean-none-3\n"
    );
}

#[test]
fn a_crashed_actor_discloses_its_fault_after_buffered_pipe_values() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/actor-fork-sink-crash-disclosed.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "handler crashed\nfirst=1\n");
    assert_eq!(
        trace["final_state"]["runtime_failures"][0]["message"],
        "producer crash"
    );
}

#[test]
fn a_failed_child_cancels_other_work_before_scope_recovery() {
    let trace = execute(
        r#"
fn fail() { sleep(1ms); panic("child failed"); }
fn slow() {
    defer println("slow cleaned");
    sleep(10s);
    println("unreachable");
}
fn main() {
    scope {
        fork fail();
        fork slow();
    } handle failure {
        match failure {
            .Fault { message } => println(message),
            .Deadline { message } => println("wrong failure"),
        }
    };
}
"#,
    );
    assert_eq!(stdout(&trace), "slow cleaned\nchild failed\n");
    assert_eq!(trace["final_state"]["virtual_clock"]["current_ms"], 1);
}

#[test]
fn rejected_completion_requests_keep_owned_payloads_for_retry_and_redirection() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/actor-policy-reject.hew"
    ));
    assert_eq!(stdout(&trace), "submitted: accepted\nrejected: the destination mailbox is full\ntrue\n2\n2\ntrue\ntrue\nclosed\n");
}

#[test]
fn a_crash_hook_observes_the_last_valid_state_before_restart() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/actor-crash-last-valid-state.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "CHANGED!\nlast valid crash state, fresh restart\n"
    );
}

#[test]
fn crash_info_uses_the_native_fault_code_and_class() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/crash-info-hook-fields.hew"
    ));
    assert_eq!(stdout(&trace), "212\nUserPanic\n");
    assert_eq!(trace["final_state"]["exit_code"], 42);
}

#[test]
fn structural_display_preserves_checked_nested_and_alias_selections() {
    for (source, expected) in [
        (include_str!("../../tests/core-acceptance/cases/structural-rendering-display.hew"), "Report { current: west=3C, history: [Some(west=3C), None], tagged: reviewed:9 }\n(west=3C, archived:kept)\n[Circle { radius: 3 }, Square(1), Sample { reading: west=3C }, Missing]\n{latest: west=3C}\nwest=3C\nwest\n"),
        (include_str!("../../tests/core-acceptance/cases/structural-rendering-aliases.hew"), "Report { readings: readings(2), archived: Some(readings(2)), plain: [3, 5], pair: (readings(2), west) }\n[Update { readings: readings(2) }, History(Some(readings(2)))]\nStations { values: {west: readings(2)} }\nreadings(2)\n"),
    ] {
        assert_eq!(stdout(&execute(source)), expected);
    }
}

#[test]
fn a_nested_display_fault_skips_later_callbacks_and_unwinds() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/structural-rendering-display-fault.hew"),
        "trap",
    );
    assert_eq!(trace["result"], "trap");
    assert_eq!(stdout(&trace), "");
}

#[test]
fn generators_stay_lazy_and_drain_early_close_before_returning() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/generator-values.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "WORD\nWORD:0\nWORD:1\nWORD:2\nWORD cleaned\nEARLY:0\nEARLY cleaned\nafter early\ndone\n"
    );
}

#[test]
fn generators_close_through_records_variants_captures_and_lazy_owners() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/generator-nested-values.hew"
    ));
    assert_eq!(stdout(&trace), "record\nrecord cleaned\nenum\nenum cleaned\nclosure\nclosure cleaned\npartial\nEXTRACTED\npartial cleaned\nlazy\nlazy cleaned\ndone\n");
}

#[test]
fn a_generator_cleanup_fault_unwinds_its_consumer() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/generator-close-fault.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "first\n");
}

#[test]
fn generator_close_composes_with_race_cancellation() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/race-loser-generator-cleanup.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "closed loser\nselected\ndone\nclosed winner\n"
    );
}

#[test]
fn generator_deadline_drains_producer_and_consumer_cleanup() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/generator-deadline.hew"),
        "panic",
    );
    assert_eq!(
        stdout(&trace),
        "PRODUCER\nPRODUCER cleaned\nCONSUMER cleaned\n"
    );
}

#[test]
fn yielded_and_returned_generators_keep_distinct_owned_outputs() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/generator-nested-outputs.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "yielded\nreceived\nyielded cleaned\nreturned\nreturned cleaned\nfinished\ndone\n"
    );
}

#[test]
fn generic_actors_stream_owned_values_with_backpressure() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/actor-generic-instances.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "distinct generic actors and owned replies\n"
    );
}
