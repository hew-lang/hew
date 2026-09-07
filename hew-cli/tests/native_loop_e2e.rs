//! Native statement loops preserve control flow, bounds and lexical cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

fn run_loop(source: &str, expected: &str, status: i32, diagnostic: &str) {
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
        let output = run_bounded_command(build, format!("build native loop O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let mut run = Command::new(binary);
        run.env("HEW_WORKERS", "1");
        let output = run_bounded_command(run, format!("run native loop O{opt}"));
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
fn labels_continue_outer_loops_and_run_nested_cleanup() {
    run_loop(r#"
fn value() -> string { println("operand"); "discard".to_upper() }
fn main() {
    var count = 0;
    @outer: loop {
        defer println("outer iteration");
        count += 1;
        @inner: while true {
            defer println("inner iteration");
            if count < 3 { continue @outer; }
            let _ = value();
            break @outer;
        }
    }
    println(count);
    var sum = 0;
    @range: for i in 0..4 {
        loop {
            if i == 1 { continue @range; }
            sum += i;
            break;
        }
    }
    println(sum);
}
"#, "inner iteration\nouter iteration\ninner iteration\nouter iteration\noperand\ninner iteration\nouter iteration\n3\n5\n", 0, "");
}

#[test]
fn range_adapters_evaluate_operands_once_and_respect_endpoints() {
    run_loop(
        r#"
fn mark(n: i64) -> i64 { println(n); n }
fn main() {
    for i in (mark(1)..=mark(7)).rev().step_by(mark(3)) { println(i); }
    for i in (1..7).step_by(2) { println(i); }
    for i in (1..7).rev().step_by(2) { println(i); }
    for i in (5..2).rev() { panic("empty"); }
    for i in 5..=2 { panic("empty"); }
}
"#,
        "1\n7\n3\n7\n4\n1\n1\n3\n5\n6\n4\n2\n",
        0,
        "",
    );
}

#[test]
fn inclusive_limits_and_strides_exhaust_without_overflow() {
    run_loop(r#"
fn main() {
    let high: i64 = 9223372036854775807;
    let low: i64 = -9223372036854775807 - 1;
    for i in high..=high { println(i); }
    for i in (low..=low).rev() { println(i); }
    for i in (low..low).rev() { panic("empty minimum"); }
    for i in ((high - 1)..=high).step_by(2) { println(i); }
    for i in (low..=(low + 1)).rev().step_by(2) { println(i); }
    let first: u8 = 254;
    let last: u8 = 255;
    let zero: u8 = 0;
    let one: u8 = 1;
    let two: u8 = 2;
    for i in first..=last { println(i as i64); }
    for i in (zero..=one).rev().step_by(two) { println(i as i64); }
}
"#, "9223372036854775807\n-9223372036854775808\n9223372036854775806\n-9223372036854775807\n254\n255\n1\n", 0, "");
}

#[test]
fn runtime_stride_failure_runs_outer_cleanup() {
    run_loop(
        r#"
fn stride() -> i64 { 0 }
fn main() {
    defer println("cleanup");
    for i in (0..4).step_by(stride()) { panic("entered"); }
}
"#,
        "cleanup\n",
        212,
        "step_by requires a positive step",
    );
}

#[test]
fn loop_body_fault_drains_before_outer_cleanup() {
    run_loop(
        r#"
fn bad() -> string { panic("operand failed") }
fn main() {
    defer println("parent");
    loop {
        defer println("iteration");
        let owned = "retained".to_upper();
        let _ = bad();
        break;
    }
}
"#,
        "iteration\nparent\n",
        212,
        "operand failed",
    );
}

#[test]
fn labelled_exits_run_defers_and_close_generators_before_parent_cleanup() {
    run_loop(
        r#"
gen fn words() -> string {
    defer println("generator cleaned");
    yield "owned".to_upper();
    yield "later";
}
fn main() {
    defer println("parent");
    @outer: for i in 0..3 {
        loop {
            defer println("inner");
            let values = words();
            match values.next() { .Some(text) => println(text), .None => panic("empty"), }
            if i == 0 { continue @outer; }
            break @outer;
        }
    }
    println("done");
}
"#,
        "OWNED\ninner\ngenerator cleaned\nOWNED\ninner\ngenerator cleaned\ndone\nparent\n",
        0,
        "",
    );
}
