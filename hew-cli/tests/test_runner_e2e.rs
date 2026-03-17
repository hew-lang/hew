use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn require_codegen() -> bool {
    static BUILD_OK: OnceLock<bool> = OnceLock::new();
    *BUILD_OK.get_or_init(|| {
        Command::new("make")
            .args(["runtime", "stdlib"])
            .current_dir(repo_root())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
