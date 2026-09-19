//! Verified SIR packages execute concurrency, recovery and owned cleanup through
//! the public browser compiler and the sandbox VM.

use hew_wasm::sandbox::sir_emit;
use std::fmt::Write as _;

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
    execute_options(source, status, "{}")
}

fn execute_options(source: &str, status: &str, options: &str) -> serde_json::Value {
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
            process.stdout.write(JSON.stringify(runBytecode(package_, JSON.parse(process.argv[2]))));
        ",
        ])
        .arg(file.path())
        .arg(options)
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
        r"
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
",
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
        r"
fn delayed(value: i64, delay: duration) -> i64 {
    defer println(value);
    sleep(delay);
    value
}
fn main() {
    println(race { delayed(1, 1ms), delayed(2, 10s) });
}
",
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

#[test]
fn nested_supervisor_roles_follow_group_replacements() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/supervisor-mixed-strategies.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "mixed child kinds obey declaration-ordered restart strategies\n"
    );
    assert_eq!(trace["final_state"]["exit_code"], 0);
}

#[test]
fn pools_restart_only_the_failed_member_from_its_template() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/supervisor-pool-lifecycle.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "4\nno member 4\n4\n4\n4\n4\n1\n3\n11\n4\n4\n4\nstopped\n"
    );
    assert_eq!(trace["final_state"]["exit_code"], 0);
}

#[test]
fn declined_roles_stay_spent_and_keep_the_run_unsuccessful() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/supervisor-declined-role-group-restart.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "a declined role stays dead through a sibling group restart\n"
    );
    assert_eq!(trace["final_state"]["exit_code"], 1);
}

#[test]
fn a_nested_start_failure_drains_previously_started_siblings() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/supervisor-nested-init-fault.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "COMPLETED CHILD RELEASED\n");
}

#[test]
fn zero_restart_budget_settles_nested_roles_without_restarting() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/supervisor-zero-budget.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "ONE INITIAL INCARNATION\nzero budget performs no restart\n"
    );
    assert_eq!(trace["final_state"]["exit_code"], 1);
}

#[test]
fn simultaneous_sibling_faults_settle_after_effective_group_recovery() {
    let source = r#"
actor Worker {
    var value: i64 = 7,
    receive fn fail() { sleep(1ms); panic("group failure"); }
    receive fn get() -> i64 { value }
}
supervisor Group {
    strategy: one_for_all,
    intensity: 1 within 60s,
    child first: Worker,
    child second: Worker,
}
fn main() {
    let group = spawn Group;
    scope {
        let first = fork group.first.fail();
        let second = fork group.second.fail();
        let _ = await first;
        let _ = await second;
    }
    let _ = await_restart group.first;
    println(group.first.get().expect("recovered first"));
    println(group.second.get().expect("recovered second"));
    close(group);
}
"#;
    for seed in [0, 1, 7, 42] {
        let options =
            serde_json::json!({"schedulerPolicy": "chaos", "replay": {"seed": seed}}).to_string();
        let trace = execute_options(source, "ok", &options);
        assert_eq!(stdout(&trace), "7\n7\n");
        assert_eq!(trace["final_state"]["exit_code"], 0, "{trace:#}");
        let replay = serde_json::json!({"replay": trace["replay"]}).to_string();
        assert_eq!(execute_options(source, "ok", &replay), trace);
    }
}

#[test]
fn nested_display_resumes_peer_calls_and_drains_actor_close_cancellation() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/structural-rendering-actor-lifecycle.hew"
    ));
    assert_eq!(stdout(&trace), "start [1, 2]\nfinished ONE\n{west: [ONE]}\nfinished ONE\nfault returned\nstart [1, 2]\nfinished CANCELLED\ncancelled formatter closed\n");
    assert_eq!(trace["final_state"]["exit_code"], 1);
    assert_eq!(trace["final_state"]["virtual_clock"]["current_ms"], 0);
}

#[test]
fn authored_resource_close_runs_once_for_explicit_and_implicit_release() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/resource-close-consumes.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "close e1\nreleased early\nlate live 2\nclose l2\n"
    );
}

#[test]
fn an_owned_return_transfers_its_close_fault_to_the_caller() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/resource-close-fault-returned-owner.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "working\nclose 7\n");
}

#[test]
fn resource_close_faults_drain_remaining_handler_and_actor_owners() {
    let frame = execute(include_str!(
        "../../tests/core-acceptance/cases/resource-close-fault-frame-owners.hew"
    ));
    assert_eq!(stdout(&frame), "close 2\nclose 1\nrestarted\n");
    assert_eq!(frame["final_state"]["exit_code"], 0);
    let state = execute_expected(
        include_str!("../../tests/core-acceptance/cases/resource-close-fault-actor-state.hew"),
        "panic",
    );
    assert_eq!(stdout(&state), "close 2\nclose 1\n");
}

#[test]
fn cancellation_preserves_a_resource_close_failure() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/resource-close-fault-cancel-edge.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "close 9\n");
}

#[test]
fn dead_actor_calls_drain_their_consumed_message_before_returning() {
    let source = include_str!("../../tests/core-acceptance/cases/resource-close-dead-ask.hew");
    let trace = execute(source);
    assert_eq!(stdout(&trace), "closed 9\ndead\n");
    assert_eq!(trace["final_state"]["exit_code"], 0);

    let failure = source.replace(
        "expect(\"audit\");",
        "expect(\"audit\"); panic(\"close failed\");",
    );
    let trace = execute_expected(&failure, "panic");
    assert_eq!(stdout(&trace), "closed 9\n");
}

#[test]
fn consuming_close_matches_the_shared_native_manifests() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/core-acceptance");
    for name in [
        "actor_state_closure_field",
        "resource-close-actor-composition",
        "resource-close-actor-queued",
        "resource-close-erased-owners",
        "resource-close-task-results",
        "resource-close-sealed-request",
        "resource-close-displaced-values",
        "resource-close-displaced-fault",
        "resource-close-actor-generator",
    ] {
        let manifest = std::fs::read_to_string(root.join(format!("cases/{name}.toml")))
            .expect("shared native manifest");
        let manifest: toml::Value = toml::from_str(&manifest).expect("valid manifest");
        let case = &manifest["case"][0];
        let source = std::fs::read_to_string(root.join(case["source"].as_str().unwrap()))
            .expect("shared native source");
        let trace = execute(&source);
        assert_eq!(
            stdout(&trace),
            case["expected"]["stdout"].as_str().unwrap(),
            "{name}"
        );
        assert_eq!(
            trace["final_state"]["exit_code"].as_i64(),
            case["expected"]["exit"].as_integer(),
            "{name}"
        );
    }
}

#[test]
fn a_cancelled_unadmitted_call_closes_its_payload_before_recovery() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/resource-close-pending-ask.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "closed 2\ndeadline\nclosed 1\ngate closed\n"
    );
    assert_eq!(trace["final_state"]["exit_code"], 0);
}

#[test]
fn a_late_owned_reply_drains_before_the_next_actor_turn() {
    let source = include_str!("../../tests/core-acceptance/cases/resource-close-late-reply.hew");
    let trace = execute(source);
    assert_eq!(stdout(&trace), "deadline\nclosed 9\nready\n");
    assert_eq!(trace["final_state"]["exit_code"], 0);

    let definitions = &source[..source.find("fn main()").unwrap()];
    let selection = format!(
        r#"{definitions}
fn main() {{
    let audit = spawn Audit;
    let worker = spawn Worker;
    select {{
        value from worker.create(audit) => println(value.expect("created").id),
        after 1ms => println("timeout"),
    }}
    worker.ready().expect("ready");
    close(worker);
    close(audit);
}}
"#
    );
    let trace = execute(&selection);
    assert_eq!(stdout(&trace), "timeout\nclosed 9\nready\n");
    assert_eq!(trace["final_state"]["exit_code"], 0);
}

#[test]
fn trait_object_release_waits_for_its_resources_peer_call() {
    let trace = execute(
        r#"
actor Audit {
    receive fn record(id: i64) { sleep(1ms); println(f"closed {id}"); }
}
#[resource]
type Ticket { audit: Audit, id: i64, }
impl Ticket {
    fn close(consume self) { self.audit.record(self.id).expect("close ticket"); }
}
trait Identified { fn id(value: Self) -> i64; }
impl Identified for Ticket { fn id(value: Ticket) -> i64 { value.id } }
actor Worker {
    receive fn run(audit: Audit) {
        {
            let ticket: dyn Identified = Ticket { audit: audit, id: 7 };
            println(ticket.id());
        }
        println("released");
    }
}
fn main() {
    let audit = spawn Audit;
    let worker = spawn Worker;
    worker.run(audit).expect("run worker");
    close(worker);
    close(audit);
}
"#,
    );
    assert_eq!(stdout(&trace), "7\nclosed 7\nreleased\n");
    assert_eq!(trace["final_state"]["exit_code"], 0);
}

#[test]
fn task_results_transfer_or_close_their_owned_resources() {
    let trace = execute(
        r#"
#[resource]
type Ticket { id: i64, }
impl Ticket { fn close(consume self) { println(f"closed {self.id}"); } }
fn ticket(id: i64) -> Ticket { Ticket { id: id } }
fn main() {
    scope {
        let taken = fork ticket(1);
        let value = await taken;
        println(f"taken {value.id}");
        value.close();
        let _unused = fork ticket(2);
        sleep(1ms);
    }
    println("drained");
}
"#,
    );
    assert_eq!(stdout(&trace), "taken 1\nclosed 1\nclosed 2\ndrained\n");
}

#[test]
fn closing_a_pipe_releases_buffered_and_rejected_owned_items() {
    let trace = execute(
        r#"
import std.stream;
#[resource]
type Ticket { id: i64, }
impl Ticket { fn close(consume self) { println(f"closed {self.id}"); } }
fn main() {
    let (output, input): (Sink<Ticket>, Stream<Ticket>) = stream.pipe(2).expect("pipe");
    let _ = output.send(Ticket { id: 1 });
    let _ = output.send(Ticket { id: 2 });
    input.close();
    let _ = output.send(Ticket { id: 3 });
    output.close();
    println("drained");
}
"#,
    );
    assert_eq!(stdout(&trace), "closed 1\nclosed 2\nclosed 3\ndrained\n");
}

#[test]
fn main_return_drains_accepted_work_before_closing_actor_resources() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/resource-field-record-drop.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "held 1 scope\nclosed 1\nworked task\nclosed 2\nkept actor\ndone\nclosed 3\n"
    );
}

#[test]
fn collection_hash_and_equality_callbacks_resume_actor_calls() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/collection-callback-actor.hew"
    ));
    assert_eq!(stdout(&trace), "collection callbacks complete\n");
    assert_eq!(trace["final_state"]["exit_code"], 0);
}

#[test]
fn a_missing_map_index_takes_the_checked_fault_cleanup_edge() {
    let trace = execute_expected(
        r#"
fn main() {
    defer { println("map lookup cleaned"); }
    let values: HashMap<string, i64> = HashMap.new();
    println(values["missing"]);
}
"#,
        "trap",
    );
    assert_eq!(stdout(&trace), "map lookup cleaned\n");
}

#[test]
fn collection_callbacks_drain_affine_owners_on_cancellation_and_fault() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/collection-callback-lifecycle.hew"
    ));
    let mut expected = String::new();
    for phase in 0..3 {
        for collection in ["map", "set"] {
            writeln!(expected, "cancel {collection} phase {phase}\nCALLBACK LOCAL CLOSED\nowner 0 closed\nowner 99 closed\n{collection} closed").unwrap();
        }
    }
    expected.push_str("fault during rehash\nCALLBACK LOCAL CLOSED\nowner 0 closed\nowner 99 closed\ncallback fault returned\n");
    assert_eq!(stdout(&trace), expected);
    assert_eq!(trace["final_state"]["exit_code"], 1);
}

#[test]
fn displaced_map_close_fault_releases_the_published_replacement() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/resource-close-fault-map-insert.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "close 1\nclose 2\nclose 99\n");
}

#[test]
fn sets_own_affine_elements_through_duplicate_remove_and_clear() {
    let trace = execute(include_str!(
        "../../tests/core-acceptance/cases/resource-set-lifecycle.hew"
    ));
    assert_eq!(
        stdout(&trace),
        "close 12\nclose 11\nclose 22\nempty\nclose 15\n"
    );
}

#[test]
fn displaced_resource_faults_drain_shared_vector_and_array_owners() {
    for (source, expected) in [
        (
            include_str!("../../tests/core-acceptance/cases/resource-close-fault-rc-set.hew"),
            "working\nclose 1\nclose 2\n",
        ),
        (
            include_str!("../../tests/core-acceptance/cases/resource-close-fault-vec-set.hew"),
            "working\nclose 1\nclose 2\n",
        ),
        (
            include_str!("../../tests/core-acceptance/cases/resource-close-fault-vec-clear.hew"),
            "close 1\nclose 99\n",
        ),
        (
            include_str!("../../tests/core-acceptance/cases/resource-close-fault-array-set.hew"),
            "working\nclose 1\nclose 2\nclose 3\n",
        ),
    ] {
        let trace = execute_expected(source, "panic");
        assert_eq!(stdout(&trace), expected);
    }
}

#[test]
fn map_removal_owns_its_result_before_retiring_the_query() {
    let trace = execute_expected(
        include_str!("../../tests/core-acceptance/cases/resource-close-fault-map-remove.hew"),
        "panic",
    );
    assert_eq!(stdout(&trace), "close 1\nclose 3\nclose 2\nclose 99\n");
}

#[test]
fn actor_close_drains_active_and_queued_resource_messages_before_its_barrier() {
    let source = r#"
import std.stream;
actor Audit { receive fn record(id: i64) { sleep(1ms); println(f"closed {id}"); } }
#[resource]
type Ticket { audit: Audit, id: i64, fail_close: bool, }
impl Ticket {
    fn close(consume self) {
        self.audit.record(self.id).expect("audit");
        if self.fail_close { panic("queued close failed"); }
    }
}
actor Worker {
    receive fn hold(ticket: Ticket, started: Sink<i64>) {
        started.send(ticket.id).expect("started");
        sleep(1h);
        println("unreached");
    }
}
fn main() {
    let audit = spawn Audit;
    let worker = spawn Worker;
    let (output, input): (Sink<i64>, Stream<i64>) = stream.pipe(1).expect("pipe");
    mailbox(worker).hold(Ticket { audit: audit, id: 1, fail_close: false }, output.clone()).expect("first");
    input.recv().expect("started");
    mailbox(worker).hold(Ticket { audit: audit, id: 2, fail_close: FAIL_CLOSE }, output.clone()).expect("queued");
    close(worker);
    println("barrier");
    output.close();
    input.close();
    close(audit);
}
"#;
    for fails in [false, true] {
        let trace = execute_expected(
            &source.replace("FAIL_CLOSE", if fails { "true" } else { "false" }),
            if fails { "panic" } else { "ok" },
        );
        let output = stdout(&trace);
        let mut lines: Vec<_> = output.lines().collect();
        if !fails {
            assert_eq!(lines.pop(), Some("barrier"));
            assert_eq!(trace["final_state"]["exit_code"], 0);
        }
        lines.sort_unstable();
        assert_eq!(lines, ["closed 1", "closed 2"]);
        assert!(
            trace["final_state"]["virtual_clock"]["current_ms"]
                .as_f64()
                .unwrap()
                < 100.0
        );
    }
}

#[test]
fn an_actor_state_close_fault_reaches_its_sibling_pipe_sink() {
    let trace = execute_expected(
        r#"
import std.stream;
#[resource]
type Ticket { id: i64, }
impl Ticket { fn close(consume self) { println(self.id); panic("state close failed"); } }
actor Owner {
    var output: Sink<i64>,
    var ticket: Ticket,
    receive fn ready() { output.send(42).expect("queued item"); }
}
fn main() {
    let (output, input): (Sink<i64>, Stream<i64>) = stream.pipe(1).expect("pipe");
    let owner = spawn Owner(output: output, ticket: Ticket { id: 7 });
    owner.ready().expect("ready");
    scope { close(owner); println("incorrect clean close"); } handle failure {
        match failure {
            .Fault { message } => println("close fault"),
            .Deadline { message } => println("incorrect deadline"),
        }
    };
    println(input.recv().expect("buffered item"));
    let _end = input.recv();
    println("incorrect clean end");
}
"#,
        "panic",
    );
    assert_eq!(stdout(&trace), "7\nclose fault\n42\n");
    assert!(trace["final_state"]["runtime_failures"]
        .to_string()
        .contains("state close failed"));
}

#[test]
fn a_main_fault_still_closes_idle_actor_resources() {
    let trace = execute_expected(
        r#"
#[resource]
type Ticket { name: string, }
impl Ticket { fn close(consume self) { println(self.name); } }
actor Owner { var ticket: Ticket, receive fn ready() {} }
fn main() {
    let owner = spawn Owner(ticket: Ticket { name: "actor owner closed" });
    owner.ready().expect("ready");
    let _local = Ticket { name: "main owner closed" };
    panic("main failed");
}
"#,
        "panic",
    );
    assert_eq!(stdout(&trace), "main owner closed\nactor owner closed\n");
}

#[test]
fn checked_wait_policy_overrides_the_destination_overflow_declaration() {
    let source = r#"
actor Worker {
    mailbox 1 overflow OVERFLOW,
    receive fn work(value: i64) { println(value); }
}
actor Driver {
    receive fn run(worker: Worker) {
        let inbox = mailbox(worker, on_full: .Wait);
        for value in 0..3 { inbox.work(value).expect("waited admission"); }
    }
}
fn main() {
    let worker = spawn Worker;
    let driver = spawn Driver;
    driver.run(worker).expect("driver");
    sleep(1ms);
    close(worker);
    close(driver);
}
"#;
    for overflow in ["fail", "drop_old", "drop_new"] {
        let trace = execute(&source.replace("OVERFLOW", overflow));
        assert_eq!(stdout(&trace), "0\n1\n2\n");
        assert_eq!(trace["final_state"]["exit_code"], 0);
    }
}

#[test]
fn drop_newest_runs_the_discarded_messages_authored_close() {
    let trace = execute(
        r#"
#[resource]
type Ticket { id: i64, }
impl Ticket { fn close(consume self) { println(f"closed {self.id}"); } }
actor Worker {
    mailbox 1 overflow fail,
    receive fn work(ticket: Ticket) { println(f"handled {ticket.id}"); }
}
actor Driver {
    receive fn run(worker: Worker) {
        let inbox = mailbox(worker, on_full: .DropNewest);
        inbox.work(Ticket { id: 1 }).expect("first");
        inbox.work(Ticket { id: 2 }).expect("discarded");
    }
}
fn main() {
    let worker = spawn Worker;
    let driver = spawn Driver;
    driver.run(worker).expect("driver");
    close(worker);
    close(driver);
    println("barrier");
}
"#,
    );
    let output = stdout(&trace);
    let mut lines: Vec<_> = output.lines().collect();
    assert_eq!(lines.pop(), Some("barrier"));
    lines.sort_unstable();
    assert_eq!(lines, ["closed 1", "closed 2", "handled 1"]);
}
