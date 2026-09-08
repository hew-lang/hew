//! Source tasks preserve values and drain children before lexical cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn assertions_evaluate_once_and_unwind_through_scope_recovery() {
    run_task(
        r#"
fn check(value: bool) -> bool { println("condition"); value }
fn fail() {
    let owned = "owned assertion value";
    defer println(owned);
    assert(check(false));
    println("unreachable");
}
fn main() {
    let kept = "still live";
    assert(check(true));
    println(kept);
    scope {
        defer println("parent cleanup");
        let child = fork fail();
        await child;
    } handle failure {
        match failure {
            .Fault { message } => println(message),
            .Deadline { message } => println("wrong failure"),
        }
    };
    println(kept);
}
"#,
        "condition\nstill live\ncondition\nowned assertion value\nparent cleanup\nhew: failure: UserPanic (212): assertion failed\n\nstill live\n",
        0,
        "",
    );
}

#[test]
fn discarded_results_close_temporaries_and_preserve_existing_owners() {
    run_task(
        r#"
gen fn held() -> string {
    defer println("generator closed");
    yield "first";
    yield "second";
}
fn produce() -> Generator<string, ()> {
    let values = held();
    let _first = values.next();
    values
}
fn main() {
    { println("block"); };
    { produce() };
    let task = fork produce();
    await task;
    let kept = produce();
    kept;
    match kept.next() {
        .Some(value) => println(value),
        .None => panic("discarded binding was closed"),
    }
    println("continued");
}
"#,
        "block\ngenerator closed\ngenerator closed\nsecond\ncontinued\ngenerator closed\n",
        0,
        "",
    );
}

#[test]
fn never_returning_calls_preserve_cleanup_and_recovery() {
    run_task(
        r#"
fn invoke<T>(action: fn() -> T) -> T { action() }
fn main() {
    let message = "bottom call";
    let fail = || { defer println("callee cleanup"); panic(message); };
    scope {
        defer println("caller cleanup");
        fail();
    } handle failure { println("recovered call"); };
    scope { invoke(fail); } handle failure { println("recovered generic call"); };
    scope {
        let child = fork fail();
        await child;
    } handle failure { println("recovered child call"); };
}
"#,
        "callee cleanup\ncaller cleanup\nrecovered call\ncallee cleanup\nrecovered generic call\ncallee cleanup\nrecovered child call\n",
        0,
        "",
    );
}

#[test]
fn never_returning_children_can_be_awaited_and_recovered() {
    run_task(
        r#"
fn main() {
    scope {
        let child = fork { defer println("child cleanup"); panic("bottom child"); };
        await child;
    } handle failure {
        match failure {
            .Deadline { message } => println("wrong deadline"),
            .Fault { message } => println(message),
        }
    };
    let fail = true;
    let result = scope {
        let child = fork {
            if fail { panic("implicit join"); } else { panic("other branch"); }
        };
        "unreachable"
    } handle failure { "joined and recovered" };
    println(result);
}
"#,
        "child cleanup\nhew: failure: UserPanic (212): bottom child\n\njoined and recovered\n",
        0,
        "",
    );
}

#[test]
fn a_deadline_cancels_and_drains_a_never_returning_child() {
    run_task(
        r#"
fn main() {
    scope within 5ms {
        let child = fork {
            defer println("child cleanup");
            sleep(1s);
            panic("unexpected completion");
        };
        await child;
    } handle failure {
        match failure {
            .Deadline { message } => println("deadline recovered"),
            .Fault { message } => println("wrong fault"),
        }
    };
}
"#,
        "child cleanup\ndeadline recovered\n",
        0,
        "",
    );
}

#[test]
fn selecting_a_never_returning_child_propagates_its_fault() {
    run_task(
        r#"
fn main() {
    scope {
        let child = fork { panic("selected bottom child"); };
        select { value from child => println("incorrect arm") };
    } handle failure { println("recovered selection"); };
}
"#,
        "recovered selection\n",
        0,
        "",
    );
}

#[test]
fn returning_past_recovery_does_not_catch_an_outer_child_fault() {
    run_task(
        r#"
fn fail() -> i64 { sleep(2ms); panic("outer child"); }
fn choose() -> string {
    let _outer = fork { fail() };
    scope { return "returned"; } handle failure { println("incorrect handler"); };
    "fallback"
}
fn main() { println(choose()); }
"#,
        "",
        1,
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
        defer { scope { println("scope cleanup"); panic("cleanup fault"); }; }
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
        sleep(1s);
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
            sleep(1s);
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

#[test]
fn task_selection_preserves_the_loser_and_transfers_owned_results() {
    run_task(
        r#"
fn main() {
    let first = fork { sleep(1ms); "first" };
    let second = fork { "second" };
    let result = select {
        a from first => { let b = await second; a + ":" + b },
        b from second => { let a = await first; a + ":" + b },
    };
    println(result);
}
"#,
        "first:second\n",
        0,
        "",
    );
}

#[test]
fn task_selection_timer_preserves_both_tasks_and_evaluates_duration_once() {
    run_task(
        r#"
fn duration() -> duration { println("timer"); 0ms }
fn main() {
    let first = fork { sleep(100ms); 17 };
    let second = fork { sleep(100ms); 42 };
    let result = select {
        a from first => a + await second,
        b from second => b + await first,
        after duration() => { println("timeout"); await first + await second },
    };
    println(result);
    select { after 0ms => println("only timer") };
}
"#,
        "timer\ntimeout\n59\nonly timer\n",
        0,
        "",
    );
}

#[test]
fn task_selection_cancellation_drains_children_before_the_parent_defer() {
    run_task(
        r#"
fn main() {
    scope within 20ms {
        defer println("parent cleanup");
        let child = fork { sleep(1s); println("late"); 42 };
        select { value from child => println(value) };
    };
}
"#,
        "parent cleanup\n",
        1,
        "Deadline",
    );
}

#[test]
fn task_selection_propagates_child_faults_without_entering_the_arm() {
    run_task(
        r#"
fn fail() -> i64 { panic("selected child failed"); }
fn main() {
    defer println("parent cleanup");
    let child = fork {
        defer println("child cleanup");
        sleep(1ms);
        fail()
    };
    select { value from child => println(value) };
}
"#,
        "child cleanup\nparent cleanup\n",
        1,
        "selected child failed",
    );
}

#[test]
fn task_selection_handles_projected_temporary_and_unit_tasks() {
    run_task(
        r#"
fn main() {
    let pair = (fork { sleep(1ms); 17 }, fork { 42 });
    let sum = select {
        a from pair.0 => a + await pair.1,
        b from pair.1 => b + await pair.0,
    };
    println(sum);
    let result = select {
        value from fork { println("created"); 42 } => value,
    };
    println(result);
    let child = fork { sleep(1ms); println("child"); };
    select { done from child => println("selected") };
}
"#,
        "59\ncreated\n42\nchild\nselected\n",
        0,
        "",
    );
}

#[test]
fn task_selection_releases_prepared_loans_when_timer_evaluation_faults() {
    run_task(
        r#"
fn duration() -> duration { panic("timer preparation failed"); }
fn main() {
    defer println("parent cleanup");
    let child = fork { sleep(1s); 42 };
    select {
        value from child => println(value),
        after duration() => println("timer"),
    };
}
"#,
        "parent cleanup\n",
        1,
        "timer preparation failed",
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
        let first = fork { sleep(1ms); "child value" };
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
        sleep(1ms);
        println(label);
        "unused child result"
    };
    return "parent result";
}
fn main() { println(work()); }
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
        sleep(5s);
        println("sibling escaped cancellation");
    };
    let _failure = fork {
        defer println("failed child cleanup");
        sleep(1ms);
        fail();
    };
}
fn main() { work(); }
"#,
        "failed child cleanup\nsibling cleanup\nparent cleanup\n",
        1,
        "child failed",
    );
}

#[test]
fn borrowed_values_are_promoted_into_child_captures() {
    run_task(
        r#"
fn work(label: string) {
    let child = fork { sleep(1ms); label };
    println(await child);
    println(label);
}
fn main() {
    let original = "borrowed value";
    work(original);
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
            let _child = fork { sleep(1ms); println(i); };
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
        sleep(5s);
        println("missed deadline");
    };
}
"#,
        "scope cleanup\nparent cleanup\n",
        1,
        "Deadline",
    );
}

#[test]
fn scope_deadline_reaches_a_nested_call_before_scope_cleanup() {
    run_task(
        r#"
fn wait_for_work() {
    defer println("call cleanup");
    sleep(5s);
    println("missed deadline");
}
fn main() {
    defer println("parent cleanup");
    scope within 1ms {
        defer println("scope cleanup");
        wait_for_work();
    };
}
"#,
        "call cleanup\nscope cleanup\nparent cleanup\n",
        1,
        "Deadline",
    );
}
