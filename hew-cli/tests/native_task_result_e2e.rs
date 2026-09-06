//! Scope-owned task results close before enclosing resource cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

const STARTED: &str = r#"
gen fn values(label: string) -> string {
    let _pending = fork { await sleep(5s); println("missed cancellation"); };
    defer println(label + " closed");
    yield label;
    yield "still live";
}
fn started(label: string) -> Generator<string, ()> {
    let result = values(label);
    match await result.next() { .Some(value) => println(value), .None => panic("empty"), }
    result
}
"#;

#[test]
fn ignored_started_generator_closes_before_parent_defer() {
    run_task(
        &format!(
            "{STARTED}{}",
            r#"
fn work() {
    defer println("parent cleanup");
    let _child = fork { await started("result") };
}
fn main() { await work(); println("done"); }
"#
        ),
        "result\nresult closed\nparent cleanup\ndone\n",
        0,
        "",
    );
}

#[test]
fn nested_result_closes_through_its_shared_layout() {
    run_task(
        &format!(
            "{STARTED}{}",
            r#"
type Holder { value: Generator<string, ()>, label: string }
enum Wrapped { Full(Holder), Empty }
fn work() {
    defer println("parent cleanup");
    let _child = fork {
        Wrapped.Full(Holder { value: await started("nested"), label: "owned".to_upper() })
    };
}
fn main() { await work(); println("done"); }
"#
        ),
        "nested\nnested closed\nparent cleanup\ndone\n",
        0,
        "",
    );
}

#[test]
fn observed_result_survives_its_old_scope() {
    run_task(
        &format!(
            "{STARTED}{}",
            r#"
fn main() {
    let result = scope {
        let child = fork { await started("transferred") };
        await child
    };
    println("scope closed");
    match await result.next() { .Some(value) => println(value), .None => panic("closed early"), }
}

"#
        ),
        "transferred\nscope closed\nstill live\ntransferred closed\n",
        0,
        "",
    );
}

#[test]
fn abandoned_closure_result_closes_its_started_capture() {
    run_task(
        &format!(
            "{STARTED}{}",
            r#"
fn work() {
    defer println("parent cleanup");
    let _child = fork {
        let result = await started("capture");
        let callback: fn[once]() -> () = move || { let _held = result; };
        callback
    };
}
fn main() { await work(); }
"#
        ),
        "capture\ncapture closed\nparent cleanup\n",
        0,
        "",
    );
}

#[test]
fn result_cleanup_failure_propagates_after_parent_defer() {
    run_task(
        r#"
gen fn values() -> i64 {
    defer panic("result cleanup failed");
    yield 1;
}
fn work() {
    defer println("parent cleanup");
    let _child = fork {
        let result = values();
        match await result.next() { .Some(value) => println(value), .None => panic("empty"), }
        result
    };
}
fn main() { await work(); }
"#,
        "1\nparent cleanup\n",
        212,
        "result cleanup failed",
    );
}

#[test]
fn cancelled_scope_closes_abandoned_result_before_parent_defer() {
    run_task(
        &format!(
            "{STARTED}{}",
            r#"
fn main() {
    scope within 100ms {
        defer println("parent cleanup");
        let _child = fork { await started("cancelled") };
        await sleep(5s);
        println("missed deadline");
    };
}
"#
        ),
        "cancelled\ncancelled closed\nparent cleanup\n",
        254,
        "Deadline",
    );
}

fn run_task(source: &str, expected: &str, status: i32, diagnostic: &str) {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("tasks.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("tasks-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .args(["build"])
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build native task O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output = run_bounded_command(Command::new(binary), format!("run native task O{opt}"));
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
