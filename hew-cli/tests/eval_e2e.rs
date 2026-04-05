mod support;

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::OnceLock;

use support::strip_ansi;

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

fn run_eval_with_stdin(args: &[&str], input: &str) -> Output {
    let mut child = Command::new(hew_binary())
        .args(args)
        .current_dir(repo_root())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin.write_all(input.as_bytes()).unwrap();
    }

    child.wait_with_output().unwrap()
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
fn eval_inline_expression_succeeds() {
    if !require_codegen() {
        return;
    }

    let output = Command::new(hew_binary())
        .args(["eval", "1 + 2"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn eval_file_in_repl_context_succeeds() {
    if !require_codegen() {
        return;
    }

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("positive_eval.hew");
    std::fs::write(
        &path,
        "fn identity<T>(x: T) -> T {\n    x\n}\n\nidentity(42)\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn eval_stdin_in_repl_context_succeeds() {
    if !require_codegen() {
        return;
    }

    let output = run_eval_with_stdin(
        &["eval", "-f", "-"],
        "fn identity<T>(x: T) -> T {\n    x\n}\n\nidentity(42)\n",
    );

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
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

#[test]
fn eval_large_stdout_completes_before_timeout() {
    if !require_codegen() {
        return;
    }

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("large_stdout_eval.hew");
    std::fs::write(
        &path,
        "scope {\n    var i = 0;\n    while i < 20000 {\n        println(\"line\");\n        i = i + 1;\n    }\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("5")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.lines().count(), 20_000);
    assert!(stdout.starts_with("line\n"), "stdout: {stdout}");
    assert!(stdout.ends_with("line\n"), "stdout: {stdout}");
}

#[test]
fn eval_large_stderr_completes_before_timeout() {
    if !require_codegen() {
        return;
    }

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("large_stderr_eval.hew");
    std::fs::write(
        &path,
        "import std::io;\n\nfn spam_err() {\n    var i = 0;\n    while i < 20000 {\n        io.write_err(\"line\\n\");\n        i = i + 1;\n    }\n}\n\nspam_err()\n42\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("5")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn eval_inline_parse_errors_render_cli_diagnostics() {
    let output = Command::new(hew_binary())
        .args(["eval", "1 +"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<eval>:2:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("2 |     println(1 +);"), "stderr: {stderr}");
    assert!(stderr.contains("|                ^"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_file_type_errors_render_cli_diagnostics() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("negative_eval.hew");
    std::fs::write(&path, "fn broken() -> i64 {\n    \"oops\"\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let header = format!("{}:2:", path.display());
    assert!(stderr.contains(&header), "stderr: {stderr}");
    assert!(
        stderr.contains("type mismatch: expected `int`, found `String`"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("2 |     \"oops\""), "stderr: {stderr}");
    assert!(stderr.contains("|     ^^^^^^"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}
