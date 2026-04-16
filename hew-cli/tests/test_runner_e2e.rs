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
fn ignored_test_is_skipped_and_counted() {
    require_codegen();

    let output = run_suite(
        &[(
            "ignored_test.hew",
            "#[test]\n#[ignore]\nfn skipped() {\n    panic(\"ignored tests should not run\");\n}\n",
        )],
        &["--no-color"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test skipped ... ignored"));
    assert!(stdout.contains("0 passed; 0 failed; 1 ignored"));
}

#[test]
fn include_ignored_flag_runs_skipped_tests() {
    require_codegen();

    let output = run_suite(
        &[(
            "ignored_test.hew",
            "#[test]\n#[ignore]\nfn skipped() {\n    panic(\"ignored test ran\");\n}\n",
        )],
        &["--no-color", "--include-ignored"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test skipped ... FAILED"));
    assert!(stdout.contains("ignored test ran"));
    assert!(stdout.contains("0 passed; 1 failed; 0 ignored"));
}

#[test]
fn filter_narrows_to_matching_tests() {
    require_codegen();

    let output = run_suite(
        &[
            (
                "alpha_test.hew",
                "#[test]\nfn keeps_me() {\n    assert(true);\n}\n",
            ),
            (
                "beta_test.hew",
                "#[test]\nfn skip_me() {\n    panic(\"filtered test should not run\");\n}\n",
            ),
        ],
        &["--no-color", "--filter", "keeps"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("running 1 tests"));
    assert!(stdout.contains("test keeps_me ... ok"));
    assert!(!stdout.contains("skip_me"));
    assert!(stdout.contains("1 passed; 0 failed; 0 ignored"));
}

#[test]
fn should_panic_test_passes_when_it_panics() {
    require_codegen();

    let output = run_suite(
        &[(
            "should_panic_test.hew",
            "#[test]\n#[should_panic]\nfn expected_panic() {\n    panic(\"boom\");\n}\n",
        )],
        &["--no-color"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test expected_panic ... ok"));
    assert!(stdout.contains("1 passed; 0 failed; 0 ignored"));
}

#[test]
fn should_panic_test_fails_when_it_does_not_panic() {
    require_codegen();

    let output = run_suite(
        &[(
            "should_panic_test.hew",
            "#[test]\n#[should_panic]\nfn expected_panic() {\n    assert(true);\n}\n",
        )],
        &["--no-color"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test expected_panic ... FAILED"));
    assert!(stdout.contains("expected test to panic, but it completed successfully"));
    assert!(stdout.contains("0 passed; 1 failed; 0 ignored"));
}

#[test]
fn no_test_files_in_directory_exits_zero() {
    let dir = tempfile::tempdir().unwrap();
    write_file(dir.path(), "notes/readme.txt", "not a Hew test\n");

    let output = Command::new(hew_binary())
        .arg("test")
        .arg(".")
        .arg("--no-color")
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(output.status.code(), Some(0));
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("No test files found."), "stderr: {stderr}");
}

#[test]
fn no_test_functions_found_exits_zero() {
    let output = run_suite(
        &[("helpers_test.hew", "fn helper() -> int {\n    42\n}\n")],
        &["--no-color"],
    );

    assert!(output.status.success());
    assert_eq!(output.status.code(), Some(0));
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("No test functions found."),
        "stderr: {stderr}"
    );
}

#[test]
fn multi_path_invocation_aggregates_results() {
    require_codegen();

    let dir = tempfile::tempdir().unwrap();
    write_file(
        dir.path(),
        "suite_a/alpha_test.hew",
        "#[test]\nfn alpha() {\n    assert(true);\n}\n",
    );
    write_file(
        dir.path(),
        "suite_b/beta_test.hew",
        "#[test]\nfn beta() {\n    assert(true);\n}\n",
    );

    let output = Command::new(hew_binary())
        .arg("test")
        .arg("suite_a")
        .arg("suite_b")
        .arg("--no-color")
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(output.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("running 2 tests"), "stdout: {stdout}");
    assert!(stdout.contains("test alpha ... ok"), "stdout: {stdout}");
    assert!(stdout.contains("test beta ... ok"), "stdout: {stdout}");
    assert!(
        stdout.contains("2 passed; 0 failed; 0 ignored"),
        "stdout: {stdout}"
    );
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
