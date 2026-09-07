//! First-completion race publishes an ordinary error only after loser cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

fn run_race(source: &str, expected: &str) {
    check_race(source, |output| assert_eq!(output, expected));
}

fn check_race(source: &str, check: impl Fn(&str)) {
    require_codegen();
    let directory = tempdir();
    let input = directory.path().join("race.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(directory.path(), &format!("race-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build race O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let mut run = Command::new(binary);
        run.env("HEW_WORKERS", "1");
        let output = run_bounded_command(run, format!("run race O{opt} with one worker"));
        assert!(output.status.success(), "{}", describe_output(&output));
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
        check(&String::from_utf8_lossy(&output.stdout));
    }
}

#[test]
fn race_prepares_inputs_before_launch_and_drains_losers_before_an_err_result() {
    run_race(
        r#"
fn prepare(value: i64) -> i64 { println(value); value }
fn work(value: i64, delay: duration) -> Result<i64, string> {
    defer println(100 + value);
    sleep(delay);
    if value == 2 { Result.Err("winner") } else { Result.Ok(value) }
}
fn main() {
    let result = race { work(prepare(1), 5s), work(prepare(2), 50ms) };
    match result {
        .Ok(value) => println(value),
        .Err(message) => println(message),
    }
}
"#,
        "1\n2\n102\n101\nwinner\n",
    );
}

#[test]
fn race_closes_losing_generators_and_keeps_the_winner_usable() {
    run_race(
        r#"
gen fn held(label: string) -> string {
    defer println(label);
    yield label;
    yield "again";
}
fn produce(label: string, delay: duration) -> Generator<string, ()> {
    let values = held(label);
    let _first = values.next();
    sleep(delay);
    values
}
fn main() {
    let winner = race { produce("loser", 5s), produce("winner", 20ms) };
    println("winner ready");
    match winner.next() {
        .Some(value) => println(value),
        .None => panic("winning generator was closed"),
    }
}
"#,
        "loser\nwinner ready\nagain\nwinner\n",
    );
}

#[test]
fn parent_deadline_drains_all_race_children_before_recovery() {
    check_race(
        r#"
fn work(label: string) -> string {
    defer println(label);
    sleep(5s);
    "unreachable"
}
fn main() {
    scope within 20ms {
        let result = race { work("first"), work("second") };
        println(result);
    } handle failure {
        match failure {
            .Deadline { message } => println("deadline"),
            .Fault { message } => panic(message),
        }
    };
    println("continued");
}
"#,
        |output| {
            let prefix = output
                .strip_suffix("deadline\ncontinued\n")
                .expect("cleanup must precede recovery");
            let mut children = prefix.lines().collect::<Vec<_>>();
            children.sort_unstable();
            assert_eq!(children, ["first", "second"]);
        },
    );
}

#[test]
fn loser_cleanup_fault_releases_the_selected_owner_before_recovery() {
    run_race(
        r#"
gen fn held(label: string) -> string { defer println(label); yield label; yield "again"; }
gen fn broken() -> string { defer panic("loser cleanup"); yield "started"; }
fn slow() -> Generator<string, ()> {
    let values = broken();
    let _first = values.next();
    sleep(5s);
    values
}
fn fast() -> Generator<string, ()> {
    let values = held("winner cleanup");
    let _first = values.next();
    sleep(20ms);
    values
}
fn main() {
    scope {
        let _winner = race { slow(), fast() };
        println("unreachable");
    } handle failure {
        match failure {
            .Fault { message } => println(message.contains("loser cleanup")),
            .Deadline { message } => panic(message),
        }
        println("recovered");
    };
}
"#,
        "winner cleanup\ntrue\nrecovered\n",
    );
}
