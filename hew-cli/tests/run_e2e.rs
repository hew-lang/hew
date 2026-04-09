mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

#[test]
fn timeout_zero_is_rejected() {
    let output = Command::new(hew_binary())
        .args(["run", "--timeout", "0", "placeholder.hew"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: --timeout must be at least 1 second"));
}

#[test]
fn run_timeout_exit_code_is_non_zero() {
    require_codegen();

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("timeout_run.hew");
    std::fs::write(
        &path,
        "fn main() {\n    var i = 0;\n    loop {\n        i = i + 1;\n    }\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg("--timeout")
        .arg("1")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: program timed out after 1s"));
}
