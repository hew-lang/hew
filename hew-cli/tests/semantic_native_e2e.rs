//! Native semantic behaviour retained across compiler representation changes.

mod support;
use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

const CLOSED_DIRECT_CALLS: &str = r"
fn main() -> i64 {
    if twice(40) == 42 {
        0
    } else {
        1
    }
}

fn twice(value: i64) -> i64 {
    increment(increment(value))
}

fn increment(value: i64) -> i64 {
    value + 1
}
";

const VIRTUAL_TUPLE_PROJECTION: &str = r"
fn main() -> i64 {
    let pair = (0, 42);
    pair.0
}
";

const VIRTUAL_TUPLE_SCALAR_PARAMS: &str = r"
fn pair_second(x: i64, y: i64) -> i64 {
    let pair = (x, y);
    pair.1
}

fn main() -> i64 {
    pair_second(42, 0)
}
";

const GENERIC_DIRECT_CALLS: &str = r"
pub fn id<T>(value: T) -> T {
    value
}

pub fn relay<U>(value: U) -> U {
    id(id(value))
}

fn main() -> i64 {
    if relay(40) == 40 {
        0
    } else {
        1
    }
}
";

const RECURSIVE_DIRECT_CALLS: &str = r"
fn main() -> i64 {
    countdown(5)
}

fn countdown(value: i64) -> i64 {
    if value == 0 {
        0
    } else {
        countdown(value - 1)
    }
}
";

const UNREACHABLE_UNSUPPORTED_BODY: &str = r"
fn main() -> i64 {
    selected()
}

fn selected() -> i64 {
    0
}

fn unrelated_effectful() -> i64 {
    println(42);
    0
}
";

const SHORT_CIRCUIT_EXECUTABLE: &str = r"
fn sir_and_value(flag: bool) -> i64 {
    if flag && false {
        1
    } else {
        0
    }
}

fn sir_or_value(flag: bool) -> i64 {
    if flag || false {
        1
    } else {
        0
    }
}

fn main() -> i64 {
    let total = sir_and_value(false) + sir_and_value(true) + sir_or_value(false) + sir_or_value(true);
    if total == 1 { 0 } else { 1 }
}
";

const CONSTANT_CFG_CANONICALIZATION: &str = r"
fn main() -> i64 {
    if true {
        0
    } else {
        9
    }
}
";

fn assert_native_exit(source_text: &str, expected: i32) {
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("program.hew");
    std::fs::write(&source, source_text).unwrap();
    for optimization in ["0", "2"] {
        let mut compile = Command::new(hew_binary());
        compile
            .arg("compile")
            .arg(&source)
            .arg("--opt-level")
            .arg(optimization)
            .arg("--emit-dir")
            .arg(dir.path());
        let compiled =
            run_bounded_command(compile, format!("compile semantic case at O{optimization}"));
        assert!(compiled.status.success(), "{}", describe_output(&compiled));
        let binary = hew_testutil::compiled_binary_path(dir.path(), "program");
        let result = run_bounded_command(Command::new(binary), "execute semantic case");
        assert_eq!(
            result.status.code(),
            Some(expected),
            "{}",
            describe_output(&result)
        );
        assert!(
            result.stdout.is_empty() && result.stderr.is_empty(),
            "{}",
            describe_output(&result)
        );
    }
}

#[test]
fn direct_calls_preserve_results() {
    assert_native_exit(CLOSED_DIRECT_CALLS, 0);
}

#[test]
fn tuple_projection_returns_selected_field() {
    assert_native_exit(VIRTUAL_TUPLE_PROJECTION, 0);
}

#[test]
fn tuple_projection_preserves_parameter_order() {
    assert_native_exit(VIRTUAL_TUPLE_SCALAR_PARAMS, 0);
}

#[test]
fn nested_generic_calls_preserve_results() {
    assert_native_exit(GENERIC_DIRECT_CALLS, 0);
}

#[test]
fn recursive_calls_reach_the_base_case() {
    assert_native_exit(RECURSIVE_DIRECT_CALLS, 0);
}

#[test]
fn unreachable_functions_do_not_execute() {
    assert_native_exit(UNREACHABLE_UNSUPPORTED_BODY, 0);
}

#[test]
fn short_circuit_truth_table_preserves_results() {
    assert_native_exit(SHORT_CIRCUIT_EXECUTABLE, 0);
}

#[test]
fn constant_branch_returns_the_selected_arm() {
    assert_native_exit(CONSTANT_CFG_CANONICALIZATION, 0);
}
