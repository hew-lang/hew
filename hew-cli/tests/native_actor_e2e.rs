//! Native actor ownership and checked cleanup through the process scheduler.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn actor_scope_deadlines_retain_their_closure_source() {
    run_actor(
        include_str!("../../examples/actor/scope_deadline_suspend.hew"),
        "timeout=7\nwork=0\n",
        0,
        "",
    );
}

#[test]
fn nested_actor_closures_retain_captured_values() {
    run_actor(
        r#"
actor Worker {
    receive fn work(text: string) -> string {
        let outer = || {
            let inner = || text + " nested";
            inner()
        };
        outer()
    }
}
fn main() {
    let worker = spawn Worker;
    match worker.work("kept") {
        .Ok(text) => println(text),
        .Err(_) => panic("actor closure failed"),
    }
}
"#,
        "kept nested\n",
        0,
        "",
    );
}

fn run_actor(source: &str, expected: &str, status: i32, diagnostic: &str) {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("actor.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("actor-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .arg("--opt-level")
            .arg(opt)
            .arg("-o")
            .arg(&binary);
        let output = run_bounded_command(build, format!("build native actor O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let mut run = Command::new(binary);
        run.env("HEW_WORKERS", "1");
        let output = run_bounded_command(run, format!("run native actor O{opt}"));
        assert_eq!(
            output.status.code(),
            Some(status),
            "{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            expected,
            "{}",
            describe_output(&output)
        );
        if diagnostic.is_empty() {
            assert!(output.stderr.is_empty(), "{}", describe_output(&output));
        } else {
            assert!(
                String::from_utf8_lossy(&output.stderr).contains(diagnostic),
                "{}",
                describe_output(&output)
            );
        }
    }
}

#[test]
fn messages_and_state_preserve_independent_values() {
    run_actor(
        r#"actor Recorder {
    var label: string,
    receive fn append(text: string) {
        label = label + text;
        println(label);
    }
}

fn main() {
    var prefix = "original:";
    let recorder = spawn Recorder(label: prefix);
    prefix = "changed:";
    var message = "one";
    let _ = mailbox(recorder, on_full: .Reject).append(message);
    message = message + "two";
    let _ = mailbox(recorder, on_full: .Reject).append(message);
}
"#,
        "original:one\noriginal:oneonetwo\n",
        0,
        "",
    );
}

#[test]
fn actor_handles_copy_through_records_and_vectors() {
    run_actor(
        r"actor Ledger {
    var total: i64,
    receive fn add(amount: i64) {
        total += amount;
        println(total);
    }
}
type Directory {
    ledgers: Vec<LocalPid<Ledger>>,
}
fn deliver(directory: Directory) {
    for ledger in directory.ledgers {
        let _ = mailbox(ledger, on_full: .Reject).add(3);
    }
}
fn main() {
    let ledger = spawn Ledger(total: 4);
    let directory = Directory { ledgers: [ledger] };
    let copied = directory;
    deliver(copied);
    deliver(directory);
}
",
        "7\n10\n",
        0,
        "",
    );
}

#[test]
fn initializer_finishes_before_message_delivery() {
    run_actor(
        r#"actor Recorder {
    var label: string = "boot:",
    init(initial: string) {
        label = label + initial;
    }
    receive fn append(text: string) {
        label = label + text;
        println(label);
    }
}
fn main() {
    let recorder = spawn Recorder(initial: "ready:");
    let _ = mailbox(recorder, on_full: .Reject).append("done");
}
"#,
        "boot:ready:done\n",
        0,
        "",
    );
}

#[test]
fn spawn_arguments_evaluate_in_source_order_before_defaults() {
    run_actor(
        r#"fn mark(label: string) -> string { println(label); label }
actor Ordered {
    var first: string,
    var second: string,
    var third: string = mark("default"),
    receive fn show() { println(first + second + third); }
}
fn main() {
    let ordered = spawn Ordered(second: mark("second"), first: mark("first"));
    let _ = mailbox(ordered, on_full: .Reject).show();
}
"#,
        "second\nfirst\ndefault\nfirstseconddefault\n",
        0,
        "",
    );
}

#[test]
fn handler_fault_runs_message_and_state_defers() {
    run_actor(
        r#"actor Worker {
    var label: string,
    receive fn fail(message: string) {
        defer { println(label); }
        defer { println(message); }
        label = "actor-cleanup";
        panic("handler-fault");
    }
}
fn main() {
    let worker = spawn Worker(label: "initial");
    let _ = mailbox(worker, on_full: .Reject).fail("message-cleanup");
}
"#,
        "message-cleanup\nactor-cleanup\n",
        1,
        "actor crash",
    );
}

#[test]
fn native_ask_aggregate_reply_preserves_fields_and_argument_order() {
    run_actor(
        r#"type Parcel { label: string, notes: Vec<string> }
fn number() -> i64 { println("number"); 7 }
fn text() -> string { println("text"); "label".to_upper() }
actor Maker {
    receive fn make(number: i64, label: string) -> Parcel {
        sleep(1ms);
        println(number);
        Parcel { label: label, notes: ["owned".to_upper()] }
    }
}
fn main() {
    let maker = spawn Maker();
    match maker.make(label: text(), number: number()) {
        .Ok(parcel) => { println(parcel.label); println(parcel.notes[0]); }
        .Err(_) => panic("unexpected ask failure"),
    }
}
"#,
        "text\nnumber\n7\nLABEL\nOWNED\n",
        0,
        "",
    );
}

#[test]
fn native_ask_direct_and_fork_return_owned_replies() {
    run_actor(
        r#"actor Echo {
    receive fn echo(message: string) -> string { message.to_upper() }
}
fn main() {
    let echo = spawn Echo();
    match echo.echo("direct".to_upper()) {
        .Ok(value) => println(value),
        .Err(_) => println("unexpected-error"),
    }
    let child = fork echo.echo("forked".to_upper());
    match await child {
        .Ok(value) => println(value),
        .Err(_) => println("unexpected-error"),
    }
    println("caller-continues");
}
"#,
        "DIRECT\nFORKED\ncaller-continues\n",
        0,
        "",
    );
}

#[test]
fn native_ask_preserves_strict_turn_while_another_actor_replies() {
    run_actor(
        r#"actor Echo {
    receive fn echo(message: string) -> string {
        sleep(1ms);
        message.to_upper()
    }
}
actor Relay {
    var phase: string,
    receive fn run(echo: LocalPid<Echo>, message: string) {
        phase = "waiting";
        match echo.echo(message) {
            .Ok(value) => { phase = value; }
            .Err(_) => { phase = "unexpected-error"; }
        }
        println("reply-complete");
    }
    receive fn observe() { println(phase); }
}
fn main() {
    let echo = spawn Echo();
    let relay = spawn Relay(phase: "initial");
    let _ = mailbox(relay, on_full: .Reject).run(echo, "finished".to_upper());
    let _ = mailbox(relay, on_full: .Reject).observe();
}
"#,
        "reply-complete\nFINISHED\n",
        0,
        "",
    );
}

#[test]
fn native_ask_receiver_fault_returns_error_without_cancelling_caller() {
    run_actor(
        r#"actor Broken {
    receive fn fail(message: string) -> string {
        defer { println(message); }
        sleep(1ms);
        panic("receiver-fault");
    }
}
fn report(value: string) { println(value); }
fn main() {
    let first = spawn Broken();
    match first.fail("direct-cleanup".to_upper()) {
        .Ok(value) => report(value),
        .Err(ActorError.Trapped) => println("handler-trapped"),
        .Err(_) => println("unexpected-error"),
    }
    let second = spawn Broken();
    let child = fork second.fail("fork-cleanup".to_upper());
    match await child {
        .Ok(value) => report(value),
        .Err(ActorError.Trapped) => println("handler-trapped"),
        .Err(_) => println("unexpected-error"),
    }
    println("caller-continues");
}
"#,
        "DIRECT-CLEANUP\nhandler-trapped\nFORK-CLEANUP\nhandler-trapped\ncaller-continues\n",
        1,
        "actor crash",
    );
}

#[test]
fn native_ask_deadline_abandons_reply_and_actor_finishes_owned_cleanup() {
    run_actor(
        r#"actor Slow {
    receive fn echo(message: string) -> string {
        defer { println("receiver-finished"); }
        sleep(30ms);
        message.to_upper()
    }
}
fn main() {
    let slow = spawn Slow();
    let outcome = scope within 1ms {
        match slow.echo("late".to_upper()) {
            .Ok(value) => value,
            .Err(_) => "unexpected-error",
        }
    } handle failure {
        match failure { .Deadline { message } => "timed-out", .Fault { message } => "unexpected fault", }
    };
    println(outcome);
    println("caller-continues");
}
"#,
        "timed-out\ncaller-continues\nreceiver-finished\n",
        0,
        "",
    );
}

#[test]
fn suspended_turn_preserves_state_and_releases_the_only_worker() {
    run_actor(
        r#"actor Probe {
    receive fn run() { println("other-actor"); }
}
actor Gate {
    var phase: string,
    receive fn observe() { println(phase); }
    receive fn slow(message: string) {
        phase = "before";
        sleep(20ms);
        phase = message;
        println("finished");
    }
}
fn main() {
    let probe = spawn Probe();
    let gate = spawn Gate(phase: "initial");
    let _ = mailbox(gate, on_full: .Reject).slow("after".to_upper());
    let _ = mailbox(gate, on_full: .Reject).observe();
    let _ = mailbox(probe, on_full: .Reject).run();
}
"#,
        "other-actor\nfinished\nAFTER\n",
        0,
        "",
    );
}

#[test]
fn resumed_handler_fault_runs_owned_defers_before_actor_failure() {
    run_actor(
        r#"actor Worker {
    var label: string,
    receive fn fail(message: string) {
        defer { println(label); }
        defer { println(message); }
        sleep(1ms);
        label = "actor-cleanup";
        panic("resumed-handler-fault");
    }
}
fn main() {
    let worker = spawn Worker(label: "initial");
    let _ = mailbox(worker, on_full: .Reject).fail("message-cleanup".to_upper());
}
"#,
        "MESSAGE-CLEANUP\nactor-cleanup\n",
        1,
        "actor crash",
    );
}

#[test]
fn initializer_fault_cleans_state_and_root_before_publication() {
    run_actor(
        r#"actor Broken {
    var label: string = "seed".to_upper(),
    init(message: string) {
        defer { println(label); }
        label = message;
        panic("init-fault");
    }
    receive fn show() { println("must-not-run"); }
}
fn main() {
    defer { println("root-cleanup"); }
    let broken = spawn Broken(message: "init-cleanup".to_upper());
    let _ = mailbox(broken, on_full: .Reject).show();
}
"#,
        "INIT-CLEANUP\nroot-cleanup\n",
        212,
        "UserPanic",
    );
}

#[test]
fn receive_arguments_evaluate_in_source_order_before_protocol_ordering() {
    run_actor(
        r#"fn number() -> i64 { println("number"); 7 }
fn text() -> string { println("text"); "payload" }
actor Worker { receive fn process(number: i64, text: string) { println(number); println(text); } }
fn main() {
    let worker = spawn Worker();
    let _ = mailbox(worker, on_full: .Reject).process(text: text(), number: number());
}
"#,
        "text\nnumber\n7\npayload\n",
        0,
        "",
    );
}

#[test]
fn full_mailbox_returns_message_for_retry_and_explicit_discard_drops_it() {
    run_actor(
        r#"type Parcel { label: string, notes: Vec<string> }
fn payload(label: string) -> Parcel {
    Parcel { label: label.to_upper(), notes: ["owned".to_upper()] }
}
actor Worker {
    mailbox 1,
    receive fn process(parcel: Parcel) {
        if parcel.label == "RETRY" { println(parcel.label + ":" + parcel.notes[0]); }
    }
    receive fn exercise(me: LocalPid<Worker>, backup: LocalPid<Worker>) {
        let _ = mailbox(me, on_full: .Reject).process(payload("filler"));
        let newest = mailbox(me, on_full: .DropNewest);
        match newest.process(payload("discard")) {
            .Ok(.Discarded) => println("discarded"),
            _ => panic("expected explicit discard"),
        }
        match mailbox(me, on_full: .Reject).process(payload("retry")) {
            .Err(rejected) => {
                match rejected.reason {
                    .Full => println("full"),
                    _ => panic("expected full mailbox"),
                }
                let _ = rejected.message.to(backup);
            }
            .Ok(_) => panic("expected rejection"),
        }
    }
}
fn main() {
    let worker = spawn Worker();
    let backup = spawn Worker();
    let _ = mailbox(worker, on_full: .Reject).exercise(worker, backup);
}
"#,
        "discarded\nfull\nRETRY:OWNED\n",
        0,
        "",
    );
}

#[test]
fn a_handler_can_submit_an_owned_message_to_its_own_actor() {
    run_actor(
        r"actor Gate {
    receive fn observe() { println(42); }
    receive fn relay(me: LocalPid<Gate>) {
        let _ = mailbox(me, on_full: .Reject).observe();
    }
}
fn main() {
    let gate = spawn Gate;
    let _ = mailbox(gate, on_full: .Reject).relay(gate);
}
",
        "42\n",
        0,
        "",
    );
}

#[test]
fn waiting_submission_releases_worker_and_transfers_owned_message_on_capacity() {
    run_actor(
        r#"actor Probe { receive fn run() { println("other-actor"); } }
actor Sink {
    mailbox 1,
    receive fn hold(me: LocalPid<Sink>, driver: LocalPid<Driver>, probe: LocalPid<Probe>) {
        let _ = mailbox(driver, on_full: .Reject).run(me, probe);
        sleep(20ms);
    }
    receive fn process(value: string) { println(value); }
}
actor Driver {
    receive fn run(sink: LocalPid<Sink>, probe: LocalPid<Probe>) {
        let _ = mailbox(sink, on_full: .Reject).process("first".to_upper());
        let _ = mailbox(probe, on_full: .Reject).run();
        let waiting = mailbox(sink, on_full: .Wait);
        match waiting.process("second".to_upper()) {
            .Ok(.Accepted) => println("admitted"),
            _ => panic("unexpected wait failure"),
        }
    }
}
fn main() {
    let sink = spawn Sink();
    let driver = spawn Driver();
    let probe = spawn Probe();
    let _ = mailbox(sink, on_full: .Reject).hold(sink, driver, probe);
}
"#,
        "other-actor\nFIRST\nadmitted\nSECOND\n",
        0,
        "",
    );
}

#[test]
fn waiting_submission_deadline_cleans_sender_without_delivering_pending_message() {
    run_actor(
        r#"actor Probe { receive fn run() { println("other-actor"); } }
actor Sink {
    mailbox 1,
    receive fn hold(me: LocalPid<Sink>, driver: LocalPid<Driver>, probe: LocalPid<Probe>) {
        let _ = mailbox(driver, on_full: .Reject).run(me, probe);
        sleep(30ms);
    }
    receive fn process(value: string) { println(value); }
}
actor Driver {
    receive fn run(sink: LocalPid<Sink>, probe: LocalPid<Probe>) {
        let _ = mailbox(sink, on_full: .Reject).process("first".to_upper());
        let _ = mailbox(probe, on_full: .Reject).run();
        let waiting = mailbox(sink, on_full: .Wait);
        let outcome = scope within 1ms {
            defer println("sender-cleanup");
            let _ = waiting.process("must-not-arrive".to_upper());
            "unexpected acceptance"
        } handle failure {
            match failure { .Deadline { message } => "deadline", .Fault { message } => "unexpected fault", }
        };
        println(outcome);

    }
}
fn main() {
    let sink = spawn Sink();
    let driver = spawn Driver();
    let probe = spawn Probe();
    let _ = mailbox(sink, on_full: .Reject).hold(sink, driver, probe);
}
"#,
        "other-actor\nsender-cleanup\ndeadline\nFIRST\n",
        0,
        "",
    );
}

#[test]
fn actor_close_waits_for_handler_cleanup() {
    run_actor(
        r#"actor Holder {
    label: string,
    receive fn slow(me: LocalPid<Holder>, closer: LocalPid<Closer>) {
        defer println(label);
        let _ = mailbox(closer, on_full: .Reject).started(me);
        sleep(1s);
        println("must-not-complete");
    }
}
actor Closer {
    receive fn started(holder: LocalPid<Holder>) {
        let result = scope within 1ms {
            await holder;
            "unexpected clean wait"
        } handle failure {
            match failure { .Deadline { message } => "waiter-deadline", .Fault { message } => "unexpected fault", }
        };
        println(result);
        close(holder);
        println("closed");
    }
}
fn main() {
    let holder = spawn Holder(label: "cleaned".to_upper());
    let closer = spawn Closer();
    let _ = mailbox(holder, on_full: .Reject).slow(holder, closer);
}
"#,
        "waiter-deadline\nCLEANED\nclosed\n",
        0,
        "",
    );
}

#[test]
fn actor_termination_fault_reaches_waiter_recovery() {
    run_actor(
        r#"actor Broken { receive fn fail() { defer println("receiver-cleanup"); sleep(1ms); panic("receiver-failed"); } }
fn main() {
    let broken = spawn Broken();
    let _ = mailbox(broken, on_full: .Reject).fail();
    let result = scope { await broken; "unexpected clean termination" } handle failure {
        match failure { .Fault { message } => "observed fault", .Deadline { message } => "unexpected deadline", }
    };
    println(result);
}
"#,
        "receiver-cleanup\nobserved fault\n",
        1,
        "actor crash",
    );
}

#[test]
fn local_actor_ask_cycle_runs_cleanup_and_can_be_recovered() {
    run_actor(r#"actor Peer {
    receive fn run(other: LocalPid<Peer>, me: LocalPid<Peer>) -> string {
        match other.reply(me) { .Ok(value) => value, .Err(_) => "unexpected ask failure", }
    }
    receive fn reply(other: LocalPid<Peer>) -> string {
        let answer = scope {
            defer println("cycle-cleanup");
            let result = other.leaf();
            match result { .Ok(value) => value, .Err(_) => "unexpected response", }
        } handle failure {
            match failure { .Fault { message } => { println(message); "recovered" }, .Deadline { message } => "unexpected deadline", }
        };
        answer
    }
    receive fn leaf() -> string { "leaf" }
}
fn main() {
    let first = spawn Peer();
    let second = spawn Peer();
    match first.run(second, first) { .Ok(value) => println(value), .Err(_) => panic("unexpected failure"), }
    println("caller-continues");
}
"#, "cycle-cleanup\nhew: failure: UserPanic (212): local actor wait cycle at ask: 2 -> 1 -> 2\n\nrecovered\ncaller-continues\n", 0, "");
}
