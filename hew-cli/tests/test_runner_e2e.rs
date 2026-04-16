mod support;

use std::path::Path;
use std::process::Command;

use support::{hew_binary, require_codegen};

fn write_file(root: &Path, relative_path: &str, contents: &str) {
    let path = root.join(relative_path);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(path, contents).unwrap();
}

fn run_suite(files: &[(&str, &str)], extra_args: &[&str]) -> std::process::Output {
    let dir = tempfile::tempdir().unwrap();
    for (path, contents) in files {
        write_file(dir.path(), path, contents);
    }

    Command::new(hew_binary())
        .arg("test")
        .arg(".")
        .args(extra_args)
        .current_dir(dir.path())
        .output()
        .unwrap()
}

#[test]
fn passing_suite_exits_zero() {
    require_codegen();

    let output = run_suite(
        &[(
            "passing_test.hew",
            "#[test]\nfn passes() {\n    assert(true);\n}\n",
        )],
        &["--no-color"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test passes ... ok"));
    assert!(stdout.contains("1 passed; 0 failed; 0 ignored"));
}

#[test]
fn failing_suite_exits_non_zero() {
    require_codegen();

    let output = run_suite(
        &[(
            "failing_test.hew",
            "#[test]\nfn fails() {\n    panic(\"expected failure\");\n}\n",
        )],
        &["--no-color"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test fails ... FAILED"));
    assert!(stdout.contains("expected failure"));
}

#[test]
fn mixed_suite_reports_each_test_and_exits_non_zero() {
    require_codegen();

    let output = run_suite(
        &[
            (
                "alpha_test.hew",
                "#[test]\nfn alpha() {\n    assert(true);\n}\n",
            ),
            (
                "nested/beta_test.hew",
                "#[test]\nfn beta() {\n    panic(\"boom\");\n}\n",
            ),
        ],
        &["--no-color"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test alpha ... ok"));
    assert!(stdout.contains("test beta ... FAILED"));
    assert!(stdout.contains("1 passed; 1 failed; 0 ignored"));
}

#[test]
fn parse_errors_fail_the_suite() {
    let output = run_suite(
        &[(
            "broken_test.hew",
            "#[test]\nfn broken( {\n    assert(true);\n}\n",
        )],
        &["--no-color"],
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("expected"));
}

#[test]
fn timeout_exit_code_is_non_zero() {
    require_codegen();

    let output = run_suite(
        &[(
            "timeout_test.hew",
            "#[test]\nfn forever() {\n    while true {\n        println(\"spin\");\n    }\n}\n",
        )],
        &["--no-color", "--timeout", "1"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test forever ... FAILED"));
    assert!(stdout.contains("test timed out after 1s"));
}

#[test]
fn missing_path_exits_non_zero() {
    let dir = tempfile::tempdir().unwrap();
    let output = Command::new(hew_binary())
        .arg("test")
        .arg(dir.path().join("missing"))
        .arg("--no-color")
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("path not found"));
}

#[test]
fn junit_passing_suite_emits_xml_on_stdout() {
    require_codegen();

    let output = run_suite(
        &[(
            "passing_junit_test.hew",
            "#[test]\nfn ok() {\n    assert(true);\n}\n",
        )],
        &["--format", "junit"],
    );

    assert!(
        output.status.success(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(output.status.code(), Some(0));
    assert!(
        output.stderr.is_empty(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("<testsuites"), "stdout: {stdout}");
    assert!(stdout.contains("<testsuite"), "stdout: {stdout}");
    assert!(
        stdout.contains(r#"<testcase name="ok""#),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("</testsuites>"), "stdout: {stdout}");
}

#[test]
fn junit_failing_suite_emits_failure_element_and_exits_one() {
    require_codegen();

    let output = run_suite(
        &[(
            "failing_junit_test.hew",
            "#[test]\nfn bad() {\n    panic(\"boom\");\n}\n",
        )],
        &["--format", "junit"],
    );

    assert!(!output.status.success());
    assert_eq!(output.status.code(), Some(1));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains(r#"<testcase name="bad""#),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("<failure message="), "stdout: {stdout}");
    assert!(stdout.contains("boom"), "stdout: {stdout}");
}

#[test]
fn junit_ignored_suite_emits_skipped_element() {
    require_codegen();

    let output = run_suite(
        &[(
            "ignored_junit_test.hew",
            "#[test]\n#[ignore]\nfn skip_me() {\n    assert(false);\n}\n",
        )],
        &["--format", "junit"],
    );

    assert!(
        output.status.success(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(output.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(r#"<testcase name="skip_me""#),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("<skipped/>"), "stdout: {stdout}");
}

#[test]
fn filter_with_no_matching_tests_exits_zero_and_reports_zero_tests() {
    require_codegen();

    let output = run_suite(
        &[(
            "filter_target_test.hew",
            "#[test]\nfn alpha() {\n    assert(true);\n}\n",
        )],
        &["--no-color", "--filter", "this_pattern_matches_nothing"],
    );

    assert!(
        output.status.success(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(output.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("running 0 tests"), "stdout: {stdout}");
    assert!(
        stdout.contains("0 passed; 0 failed; 0 ignored"),
        "stdout: {stdout}"
    );
}
