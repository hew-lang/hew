//! Source tasks preserve values and drain children before lexical cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn returning_past_recovery_does_not_catch_an_outer_child_fault() {
    run_task(
        r#"
fn fail() -> i64 { await sleep(2ms); panic("outer child"); }
fn choose() -> string {
    let _outer = fork { await fail() };
    scope { return "returned"; } handle failure { println("incorrect handler"); };
    "fallback"
}
fn main() { println(await choose()); }
"#,
        "",
        212,
        "outer child",
    );
}

#[test]
fn recovery_preserves_child_and_deferred_fault_diagnostics() {
    run_task(
        r#"
fn fail() -> i64 {
    defer println("child cleanup");
    panic("child fault");
}
fn main() {
    let result = scope {
        defer { println("scope cleanup"); panic("cleanup fault"); }
        let child = fork { fail() };
        await child;
        "unreachable"
    } handle failure {
        match failure {
            .Deadline { message } => { println("unexpected deadline"); },
            .Fault { message } => { println(message); },
        }
        "recovered"
    };
    println(result);
}
"#,
        "child cleanup\nscope cleanup\nhew: failure: UserPanic (212): child fault\nhew: secondary failure: UserPanic (212): cleanup fault\n\nrecovered\n",
        0,
        "",
    );
}

#[test]
fn recovery_handler_failure_reaches_the_outer_boundary() {
    run_task(
        r#"
fn main() {
    scope {
        scope { panic("inner"); } handle failure {
            println("inner recovered");
            panic("handler fault");
        };
    } handle failure {
        match failure {
            .Deadline { message } => { println("unexpected deadline"); },
            .Fault { message } => { println(message); },
        }
    };
    let result = scope { "success" } handle failure { panic("incorrect handler"); };
    println(result);
}
"#,
        "inner recovered\nhew: failure: UserPanic (212): handler fault\n\nsuccess\n",
        0,
        "",
    );
}

#[test]
fn recovery_transfers_an_owned_result_after_fault_cleanup() {
    run_task(
        r#"
fn main() {
    let result = scope {
        defer println("scope cleanup");
        panic("broken é");
        "unreachable"
    } handle failure {
        match failure {
            .Deadline { message } => { println("unexpected deadline"); },
            .Fault { message } => { println(message); },
        }
        "recovered"
    };
    println(result);
}
"#,
        "scope cleanup\nhew: failure: UserPanic (212): broken é\n\nrecovered\n",
        0,
        "",
    );
}

#[test]
fn recovery_distinguishes_its_deadline_after_cleanup() {
    run_task(
        r#"
fn main() {
    let result = scope within 1ms {
        defer println("scope cleanup");
        await sleep(1s);
        "unreachable"
    } handle failure {
        match failure {
            .Deadline { message } => "deadline recovered",
            .Fault { message } => "unexpected fault",
        }
    };
    println(result);
}
"#,
        "scope cleanup\ndeadline recovered\n",
        0,
        "",
    );
}

#[test]
fn parent_cancellation_bypasses_inner_recovery() {
    run_task(
        r#"
fn main() {
    scope within 1ms {
        defer println("outer cleanup");
        scope {
            defer println("inner cleanup");
            await sleep(1s);
        } handle failure { println("incorrect inner handler"); };
    } handle failure {
        match failure {
            .Deadline { message } => { println("outer recovered"); },
            .Fault { message } => { println("unexpected fault"); },
        }
    };
    println("done");
}
"#,
        "inner cleanup\nouter cleanup\nouter recovered\ndone\n",
        0,
        "",
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

#[test]
fn scope_deadline_interrupts_its_body_and_runs_cleanup() {
    run_task(
        r#"
fn main() {
    defer println("parent cleanup");
    scope within 1ms {
        defer println("scope cleanup");
        await sleep(5s);
        println("missed deadline");
    };
}
"#,
        "scope cleanup\nparent cleanup\n",
        254,
        "Deadline",
    );
}

#[test]
fn scope_deadline_reaches_a_nested_call_before_scope_cleanup() {
    run_task(
        r#"
fn wait_for_work() {
    defer println("call cleanup");
    await sleep(5s);
    println("missed deadline");
}
fn main() {
    defer println("parent cleanup");
    scope within 1ms {
        defer println("scope cleanup");
        await wait_for_work();
    };
}
"#,
        "call cleanup\nscope cleanup\nparent cleanup\n",
        254,
        "Deadline",
    );
}
