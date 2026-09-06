//! Source tasks preserve values and drain children before lexical cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

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

#[test]
fn await_transfers_owned_values_and_scope_returns_its_tail() {
    run_task(
        r#"
fn main() {
    let answer = scope {
        let first = fork { await sleep(1ms); "child value" };
        let second = fork { 42 };
        println(await first);
        await second
    };
    println(answer);
}
"#,
        "child value\n42\n",
        0,
        "",
    );
}

#[test]
fn return_joins_children_before_parent_defers_and_result_publication() {
    run_task(
        r#"
fn work() -> string {
    let label = "captured value";
    defer println("parent cleanup");
    let _child = fork {
        defer println("child cleanup");
        await sleep(1ms);
        println(label);
        "unused child result"
    };
    return "parent result";
}
fn main() { println(await work()); }
"#,
        "captured value\nchild cleanup\nparent cleanup\nparent result\n",
        0,
        "",
    );
}

#[test]
fn child_fault_cancels_and_drains_siblings_before_parent_cleanup() {
    run_task(
        r#"
fn fail() { panic("child failed"); }
fn work() {
    defer println("parent cleanup");
    let _sibling = fork {
        defer println("sibling cleanup");
        await sleep(5s);
        println("sibling escaped cancellation");
    };
    let _failure = fork {
        defer println("failed child cleanup");
        await sleep(1ms);
        fail();
    };
}
fn main() { await work(); }
"#,
        "failed child cleanup\nsibling cleanup\nparent cleanup\n",
        212,
        "child failed",
    );
}

#[test]
fn borrowed_values_are_promoted_into_child_captures() {
    run_task(
        r#"
fn work(label: string) {
    let child = fork { await sleep(1ms); label };
    println(await child);
    println(label);
}
fn main() {
    let original = "borrowed value";
    await work(original);
    println(original);
}
"#,
        "borrowed value\nborrowed value\nborrowed value\n",
        0,
        "",
    );
}

#[test]
fn a_scope_cannot_publish_a_child_handle_after_closing() {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("escape.hew");
    std::fs::write(
        &input,
        "fn main() { let escaped = scope { fork { 42 } }; println(await escaped); }",
    )
    .unwrap();
    let mut build = Command::new(hew_binary());
    build
        .arg("build")
        .arg(&input)
        .arg("-o")
        .arg(dir.path().join("escape"));
    let output = run_bounded_command(build, "reject escaped scoped task");
    assert!(!output.status.success(), "{}", describe_output(&output));
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("scoped task handle"),
        "{}",
        describe_output(&output)
    );
}

#[test]
fn loop_exit_joins_its_inner_scope_before_continuing() {
    run_task(
        r#"
fn main() {
    for i in 0..2 {
        scope {
            let _child = fork { await sleep(1ms); println(i); };
            break;
        }
    }
    println("loop finished");
}
"#,
        "0\nloop finished\n",
        0,
        "",
    );
}
