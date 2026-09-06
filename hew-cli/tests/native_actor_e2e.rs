//! Native actor ownership and checked cleanup through the process scheduler.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

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
    let _ = send recorder.append(message);
    message = message + "two";
    let _ = send recorder.append(message);
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
        let _ = send ledger.add(3);
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
    let _ = send recorder.append("done");
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
    let _ = send ordered.show();
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
    let _ = send worker.fail("message-cleanup");
}
"#,
        "message-cleanup\nactor-cleanup\n",
        1,
        "actor crash",
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
        await sleep(20ms);
        phase = message;
        println("finished");
    }
}
fn main() {
    let probe = spawn Probe();
    let gate = spawn Gate(phase: "initial");
    let _ = send gate.slow("after".to_upper());
    let _ = send gate.observe();
    let _ = send probe.run();
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
        await sleep(1ms);
        label = "actor-cleanup";
        panic("resumed-handler-fault");
    }
}
fn main() {
    let worker = spawn Worker(label: "initial");
    let _ = send worker.fail("message-cleanup".to_upper());
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
    let _ = send broken.show();
}
"#,
        "INIT-CLEANUP\nroot-cleanup\n",
        212,
        "UserPanic",
    );
}

#[test]
fn description_readdress_preserves_the_owned_payload() {
    run_actor(
        r#"type Parcel { label: string, notes: Vec<string> }
fn payload() -> Parcel { Parcel { label: "payload".to_upper(), notes: ["owned".to_upper()] } }
actor Worker {
    var name: string,
    receive fn process(parcel: Parcel) { println(name + parcel.label + ":" + parcel.notes[0]); }
}
fn main() {
    let primary = spawn Worker(name: "wrong:");
    let backup = spawn Worker(name: "backup:");
    let message = primary.process(payload());
    println("constructed");
    let readdressed = message.to(backup);
    let _ = send readdressed;
}
"#,
        "constructed\nbackup:PAYLOAD:OWNED\n",
        0,
        "",
    );
}

#[test]
fn unsubmitted_description_does_not_run_the_handler() {
    run_actor(
        r#"type Parcel { label: string, notes: Vec<string> }
fn payload() -> Parcel { Parcel { label: "payload".to_upper(), notes: ["owned".to_upper()] } }
actor Worker { receive fn process(_parcel: Parcel) { println("must-not-run"); } }
fn main() {
    let worker = spawn Worker();
    let _message = worker.process(payload());
    println("constructed");
}
"#,
        "constructed\n",
        0,
        "",
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
    let _ = send worker.process(text: text(), number: number());
}
"#,
        "text\nnumber\n7\npayload\n",
        0,
        "",
    );
}

#[test]
fn failure_carrier_transfers_its_owned_message_field() {
    run_actor(
        r#"type Parcel { label: string, notes: Vec<string> }
fn payload() -> Parcel { Parcel { label: "payload".to_upper(), notes: ["owned".to_upper()] } }
actor Worker { receive fn process(parcel: Parcel) { println(parcel.label + ":" + parcel.notes[0]); } }
fn main() {
    let worker = spawn Worker();
    let pending = worker.process(payload());
    let failure = SendFailure { reason: .Full, message: pending };
    let _ = send failure.message;
}
"#,
        "PAYLOAD:OWNED\n",
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
        let _ = send me.process(payload("filler"));
        let newest = policy(me, on_full: .DropNewest);
        match send newest.process(payload("discard")) {
            .Ok(.Discarded) => println("discarded"),
            _ => panic("expected explicit discard"),
        }
        match send me.process(payload("retry")) {
            .Err(rejected) => {
                match rejected.reason {
                    .Full => println("full"),
                    _ => panic("expected full mailbox"),
                }
                let _ = send rejected.message.to(backup);
            }
            .Ok(_) => panic("expected rejection"),
        }
    }
}
fn main() {
    let worker = spawn Worker();
    let backup = spawn Worker();
    let _ = send worker.exercise(worker, backup);
}
"#,
        "discarded\nfull\nRETRY:OWNED\n",
        0,
        "",
    );
}
