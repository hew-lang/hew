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
    if !require_codegen() {
        return;
    }

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
