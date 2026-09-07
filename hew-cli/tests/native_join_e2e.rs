//! Native joins use structured tasks, preserving preparation and result order.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

fn run_join(source: &str, expected: &str, status: i32, diagnostic: &str) {
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
        let output = run_bounded_command(build, format!("build native join O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let mut run = Command::new(binary);
        run.env("HEW_WORKERS", "1");
        let output = run_bounded_command(run, format!("run native join O{opt}"));
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
fn every_branch_starts_before_waiting_and_actors_progress_with_one_worker() {
    run_join(
        r#"
actor Gate {
    var opened: bool = false,
    receive fn ready() -> bool { opened }
    receive fn open() { opened = true; }
}
actor Waiter {
    receive fn wait(gate: LocalPid<Gate>) -> string {
        loop {
            match await gate.ready() {
                .Ok(opened) => { if opened { return "first".to_upper(); } },
                .Err(_) => panic("gate failed"),
            }
        }
    }
}
actor Opener {
    receive fn open(gate: LocalPid<Gate>) -> string {
        let _ = send gate.open();
        "second".to_upper()
    }
}
fn report(result: Result<string, AskError>) {
    match result { .Ok(value) => println(value), .Err(_) => panic("join failed"), }
}
fn main() {
    let gate = spawn Gate();
    let waiter = spawn Waiter();
    let opener = spawn Opener();
    scope within 1s {
        let (first, second) = join { waiter.wait(gate), opener.open(gate) };
        report(first);
        report(second);
    };
}
"#,
        "FIRST\nSECOND\n",
        0,
        "",
    );
}

#[test]
fn owned_results_and_named_arguments_keep_parent_source_order() {
    run_join(
        r#"
type Parcel { label: string, notes: Vec<string> }
actor Maker {
    receive fn make(first: string, second: string) -> Parcel {
        println("handler");
        Parcel { label: first + ":" + second, notes: ["owned".to_upper()] }
    }
}
type ReceiverInput { maker: LocalPid<Maker>, label: string }
fn receiver(consume input: ReceiverInput) -> LocalPid<Maker> {
    println(input.label);
    input.maker
}
fn mark(label: string) -> string { println(label); label.to_upper() }
fn late(label: string) -> string { println(label); await sleep(10ms); label.to_upper() }
fn report(result: Result<Parcel, AskError>) {
    match result {
        .Ok(parcel) => { println(parcel.label); println(parcel.notes[0]); },
        .Err(_) => panic("join failed"),
    }
}
fn main() {
    let first = spawn Maker();
    let second = spawn Maker();
    let left_input = ReceiverInput { maker: first, label: "receiver one" };
    let right_input = ReceiverInput { maker: second, label: "receiver two" };
    let (left, right) = join {
        receiver(left_input).make(second: mark("b"), first: mark("a")),
        receiver(right_input).make(second: await late("d"), first: mark("c")),
    };
    report(left);
    report(right);
}
"#,
        "receiver one\nb\na\nreceiver two\nd\nc\nhandler\nhandler\nA:B\nOWNED\nC:D\nOWNED\n",
        0,
        "",
    );
}

#[test]
fn one_branch_returns_its_result_without_a_tuple_wrapper() {
    run_join(
        r#"
actor Echo { receive fn echo(value: string) -> string { value.to_upper() } }
fn main() {
    let echo = spawn Echo();
    let result: Result<string, AskError> = join { await echo.echo("one") };
    match result { .Ok(value) => println(value), .Err(_) => panic("join failed"), }
}
"#,
        "ONE\n",
        0,
        "",
    );
}

#[test]
fn ask_errors_are_values_and_do_not_cancel_successful_branches() {
    run_join(
        r#"
actor Broken {
    receive fn fail() -> string {
        defer println("broken cleanup");
        panic("receiver failed");
    }
}
actor Healthy { receive fn echo() -> string { await sleep(2ms); "healthy".to_upper() } }
fn main() {
    defer println("parent cleanup");
    let broken = spawn Broken();
    let healthy = spawn Healthy();
    let (failed, success) = join { broken.fail(), healthy.echo() };
    match failed {
        .Err(AskError.HandlerTrapped) => println("ordinary error"),
        .Err(_) => panic("wrong error"),
        .Ok(_) => panic("wrong success"),
    }
    match success { .Ok(value) => println(value), .Err(_) => panic("cancelled sibling"), }
    println("joined");
}
"#,
        "broken cleanup\nordinary error\nHEALTHY\njoined\nparent cleanup\n",
        1,
        "actor crash",
    );
}

#[test]
fn argument_failure_starts_no_children_and_releases_parent_resources() {
    run_join(
        r#"
actor Echo { receive fn echo(value: string) -> string { println("unexpected child"); value } }
fn fail() -> string { panic("argument failed"); }
fn main() {
    defer println("parent cleanup");
    let echo = spawn Echo();
    let _result = join { echo.echo("prepared".to_upper()), echo.echo(fail()) };
}
"#,
        "parent cleanup\n",
        212,
        "argument failed",
    );
}

#[test]
fn an_outer_deadline_drains_join_tasks_before_parent_cleanup() {
    run_join(r#"
actor Slow {
    receive fn echo(value: string) -> string {
        println("receiver started");
        defer println("receiver cleanup");
        await sleep(100ms);
        value.to_upper()
    }
}
fn main() {
    let first = spawn Slow();
    let second = spawn Slow();
    scope within 20ms {
        defer println("parent cleanup");
        let _result = join { first.echo("one"), second.echo("two") };
        println("missed deadline");
    } handle failure {
        match failure {
            .Deadline { message } => println("deadline recovered"),
            .Fault { message } => panic(message),
        }
    };
}
"#, "receiver started\nreceiver started\nparent cleanup\ndeadline recovered\nreceiver cleanup\nreceiver cleanup\n", 0, "");
}
