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
        let output = run_bounded_command(Command::new(binary), format!("run native actor O{opt}"));
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
    recorder.append(message);
    message = message + "two";
    recorder.append(message);
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
        ledger.add(3);
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
    recorder.append("done");
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
    ordered.show();
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
    worker.fail("message-cleanup");
}
"#,
        "message-cleanup\nactor-cleanup\n",
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
    broken.show();
}
"#,
        "INIT-CLEANUP\nroot-cleanup\n",
        212,
        "UserPanic",
    );
}
