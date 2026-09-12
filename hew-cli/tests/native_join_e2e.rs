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
    receive fn wait(gate: Gate) -> string {
        loop {
            match gate.ready() {
                .Ok(opened) => { if opened { return "first".to_upper(); } },
                .Err(_) => panic("gate failed"),
            }
        }
    }
}
actor Opener {
    receive fn open(gate: Gate) -> string {
        let _ = gate.open();
        "second".to_upper()
    }
}
fn report(result: string) { println(result); }
fn main() {
    let gate = spawn Gate();
    let waiter = spawn Waiter();
    let opener = spawn Opener();
    scope within 1s {
        let (first, second) = await fork (waiter.wait(gate), opener.open(gate));
        match first { .Ok(value) => report(value), .Err(_) => panic("join failed"), }
        match second { .Ok(value) => report(value), .Err(_) => panic("join failed"), }
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
type ReceiverInput { maker: Maker, label: string }
fn receiver(consume input: ReceiverInput) -> Maker {
    println(input.label);
    input.maker
}
fn mark(label: string) -> string { println(label); label.to_upper() }
fn late(label: string) -> string { println(label); sleep(10ms); label.to_upper() }
fn report(parcel: Parcel) { println(parcel.label); println(parcel.notes[0]); }
fn main() {
    let first = spawn Maker();
    let second = spawn Maker();
    let left_input = ReceiverInput { maker: first, label: "receiver one" };
    let right_input = ReceiverInput { maker: second, label: "receiver two" };
    let (left, right) = await fork (
        receiver(left_input).make(second: mark("b"), first: mark("a")),
        receiver(right_input).make(second: late("d"), first: mark("c")),
    );
    match left { .Ok(parcel) => report(parcel), .Err(_) => panic("join failed"), }
    match right { .Ok(parcel) => report(parcel), .Err(_) => panic("join failed"), }
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
    let result = await fork echo.echo("one");
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
actor Healthy { receive fn echo() -> string { sleep(2ms); "healthy".to_upper() } }
fn main() {
    defer println("parent cleanup");
    let broken = spawn Broken();
    let healthy = spawn Healthy();
    let (failed, success) = await fork (broken.fail(), healthy.echo());
    match failed {
        .Err(ActorError.Trapped) => println("ordinary error"),
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
    // A panic raised while preparing a fork operand's own argument exits with
    // the ordinary top-level panic status (1); the UserPanic trap code (212)
    // is a diagnostic label inside the message, not the process exit code.
    run_join(
        r#"
actor Echo { receive fn echo(value: string) -> string { println("unexpected child"); value } }
fn fail() -> string { panic("argument failed"); }
fn main() {
    defer println("parent cleanup");
    let echo = spawn Echo();
    let _result = await fork (echo.echo("prepared".to_upper()), echo.echo(fail()));
}
"#,
        "parent cleanup\n",
        1,
        "argument failed",
    );
}

/// A deadline drains the joined task frames before the parent's defer runs.
/// Owned witness arguments also cover tasks cancelled before their bodies start.
/// Actor handlers are independent of their callers' ask tasks; explicitly close
/// the actors after recovery without imposing an order on their own cleanup.
#[test]
fn an_outer_deadline_drains_join_tasks_before_parent_cleanup() {
    run_join(
        r#"
#[resource]
type TaskFrame { id: i64 }
impl TaskFrame {
    fn close(consume self) { println("task cleanup"); }
}
actor Slow {
    receive fn echo(value: string) -> string {
        sleep(100ms);
        value.to_upper()
    }
}
fn request(receiver: Slow, value: string, consume frame: TaskFrame) -> string {
    let reply = receiver.echo(value).expect("reply");
    assert(frame.id > 0);
    reply
}
fn main() {
    let first = spawn Slow();
    let second = spawn Slow();
    scope within 20ms {
        defer println("parent cleanup");
        let _result = await fork (request(first, "one", TaskFrame { id: 1 }), request(second, "two", TaskFrame { id: 2 }));
        println("missed deadline");
    } handle failure {
        match failure {
            .Deadline { message } => println("deadline recovered"),
            .Fault { message } => panic(message),
        }
    };
    close(first);
    close(second);
    println("actors closed");
}
"#,
        "task cleanup\ntask cleanup\nparent cleanup\ndeadline recovered\nactors closed\n",
        0,
        "",
    );
}
