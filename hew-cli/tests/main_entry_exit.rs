mod support;

use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
};

use support::{describe_output, hew_binary, repo_root, require_codegen};

fn fixture(name: &str) -> PathBuf {
    repo_root()
        .join("hew-cli/tests/fixtures/main_entry_exit")
        .join(name)
}

fn compile_fixture(source: &Path) -> (tempfile::TempDir, PathBuf) {
    require_codegen();

    let output_dir = tempfile::tempdir().expect("create native output directory");
    let output = Command::new(hew_binary())
        .args(["compile", "--emit-dir"])
        .arg(output_dir.path())
        .arg(source)
        .output()
        .expect("compile fixture");

    assert!(output.status.success(), "{}", describe_output(&output));

    let stdout = String::from_utf8(output.stdout).expect("compiler output is UTF-8");
    let executable = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .map(PathBuf::from)
        .expect("compiler reports native executable");

    (output_dir, executable)
}

fn run_fixture(name: &str) -> Output {
    let (_output_dir, executable) = compile_fixture(&fixture(name));
    Command::new(executable)
        .output()
        .expect("run compiled fixture")
}

#[test]
fn result_error_prints_display_and_exits_one() {
    let output = run_fixture("result_err.hew");

    assert_eq!(
        output.status.code(),
        Some(1),
        "{}",
        describe_output(&output)
    );
    assert!(output.stdout.is_empty(), "{}", describe_output(&output));
    assert_eq!(output.stderr, b"error: displayed failure\n");
}

#[test]
fn generic_result_error_calls_the_selected_display_instance() {
    let output = run_fixture("generic_result_err.hew");

    assert_eq!(
        output.status.code(),
        Some(1),
        "{}",
        describe_output(&output)
    );
    assert!(output.stdout.is_empty(), "{}", describe_output(&output));
    assert_eq!(output.stderr, b"error: generic failure\n");
}

#[test]
fn specialized_result_error_calls_the_declared_display_body() {
    let output = run_fixture("specialized_result_err.hew");

    assert_eq!(
        output.status.code(),
        Some(1),
        "{}",
        describe_output(&output)
    );
    assert!(output.stdout.is_empty(), "{}", describe_output(&output));
    assert_eq!(output.stderr, b"error: specialized failure\n");
}

#[test]
fn result_ok_exits_zero() {
    let output = run_fixture("result_ok.hew");

    assert_eq!(
        output.status.code(),
        Some(0),
        "{}",
        describe_output(&output)
    );
    assert!(output.stdout.is_empty(), "{}", describe_output(&output));
    assert!(output.stderr.is_empty(), "{}", describe_output(&output));
}

#[test]
fn result_error_type_must_implement_error() {
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(fixture("non_error.hew"))
        .output()
        .expect("check rejected fixture");

    assert_eq!(
        output.status.code(),
        Some(1),
        "{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Error") && stderr.contains("does not satisfy"),
        "{}",
        describe_output(&output)
    );
}

#[test]
fn integer_result_remains_the_process_exit_code() {
    let output = run_fixture("integer.hew");

    assert_eq!(
        output.status.code(),
        Some(37),
        "{}",
        describe_output(&output)
    );
    assert!(output.stdout.is_empty(), "{}", describe_output(&output));
    assert!(output.stderr.is_empty(), "{}", describe_output(&output));
}
