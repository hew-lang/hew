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
        .args(["eval", "--timeout", "0", "1 + 1"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: --timeout must be at least 1 second"));
}

#[test]
fn eval_timeout_exit_code_is_non_zero() {
    if !require_codegen() {
        return;
    }

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("timeout_eval.hew");
    std::fs::write(
        &path,
        "fn spin_forever() {\n    loop {\n        println(\"spin\");\n    }\n}\n\nspin_forever()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("1")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: evaluation timed out after 1s"));
}
