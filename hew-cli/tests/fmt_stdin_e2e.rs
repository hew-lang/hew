use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

fn run_fmt(args: &[&str], input: &str) -> Output {
    let mut child = Command::new(hew_binary())
        .args(args)
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
fn fmt_stdin_writes_formatted_source_to_stdout() {
    let input = "fn main() { let x = 1; }\n";
    let output = run_fmt(&["fmt", "--stdin"], input);

    assert!(
        output.status.success(),
        "hew fmt --stdin failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_ne!(stdout, input);
    assert!(stdout.contains("fn main() {\n"), "stdout: {stdout}");
    assert!(stdout.contains("    let x = 1;\n"), "stdout: {stdout}");
    assert!(stdout.contains("}\n"), "stdout: {stdout}");
    assert!(
        String::from_utf8_lossy(&output.stderr).is_empty(),
        "unexpected stderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fmt_check_stdin_succeeds_for_formatted_source() {
    let input = "fn main() {\n    let x = 1;\n}\n";
    let output = run_fmt(&["fmt", "--check", "--stdin"], input);

    assert!(
        output.status.success(),
        "hew fmt --check --stdin failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no stderr, got: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fmt_check_stdin_fails_for_unformatted_source() {
    let output = run_fmt(&["fmt", "--check", "--stdin"], "fn main() { let x = 1; }\n");

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("<stdin>: needs formatting"),
        "stderr: {stderr}"
    );
}

#[test]
fn fmt_stdin_rejects_file_arguments() {
    let output = Command::new(hew_binary())
        .args(["fmt", "--stdin", "main.hew"])
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("--stdin"), "stderr: {stderr}");
    assert!(stderr.contains("cannot be used with"), "stderr: {stderr}");
}
