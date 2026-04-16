mod support;

use std::io::Write;
use std::process::{Command, Output, Stdio};

use support::{hew_binary, strip_ansi};

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
fn fmt_no_args_exits_one_with_usage_message() {
    let output = Command::new(hew_binary()).args(["fmt"]).output().unwrap();

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Usage: hew fmt"), "stderr: {stderr}");
    assert!(stderr.contains("--stdin | <file.hew>"), "stderr: {stderr}");
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

#[test]
fn fmt_stdin_parse_errors_render_cli_diagnostics() {
    let output = run_fmt(&["fmt", "--stdin"], "fn main( {\n");

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<stdin>:1:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | fn main( {"), "stderr: {stderr}");
    assert!(stderr.contains('^'), "stderr: {stderr}");
    assert!(!stderr.contains("ParseError {"), "stderr: {stderr}");
}

#[test]
fn fmt_inplace_reports_formatted_to_stderr() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("needs_fmt.hew");
    std::fs::write(&path, "fn main() { let x = 1; }\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew fmt in-place failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    let expected = format!("Formatted {}", path.display());
    assert!(stderr.contains(&expected), "stderr: {stderr}");

    let rewritten = std::fs::read_to_string(&path).unwrap();
    assert!(
        rewritten.contains("fn main() {\n"),
        "file not rewritten: {rewritten}"
    );
}

#[test]
fn fmt_inplace_already_formatted_is_silent() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("already_fmt.hew");
    std::fs::write(&path, "fn main() {\n    let x = 1;\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew fmt in-place failed:\nstdout: {}\nstderr: {}",
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
        "expected no stderr for already-formatted file, got: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fmt_file_parse_errors_render_cli_diagnostics() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("broken.hew");
    std::fs::write(&path, "fn main( {\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let header = format!("{}:1:", path.display());
    assert!(stderr.contains(&header), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | fn main( {"), "stderr: {stderr}");
    assert!(stderr.contains('^'), "stderr: {stderr}");
    assert!(!stderr.contains("ParseError {"), "stderr: {stderr}");
}

// ---------------------------------------------------------------------------
// Multi-file --check batch behavior
// ---------------------------------------------------------------------------

/// All files already formatted → exit 0, no output on stdout or stderr.
#[test]
fn fmt_check_multi_file_all_formatted_exits_zero() {
    let dir = tempfile::tempdir().unwrap();
    let a = dir.path().join("a.hew");
    let b = dir.path().join("b.hew");
    std::fs::write(&a, "fn foo() {\n    1\n}\n").unwrap();
    std::fs::write(&b, "fn bar() {\n    2\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--check"])
        .arg(&a)
        .arg(&b)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected exit 0 when all files are formatted\nstdout: {}\nstderr: {}",
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

/// Some files need formatting → per-file message on stderr, aggregate exit 1,
/// no final summary count line.
#[test]
fn fmt_check_multi_file_some_unformatted_reports_per_file_and_exits_one() {
    let dir = tempfile::tempdir().unwrap();
    let good = dir.path().join("good.hew");
    let bad = dir.path().join("bad.hew");
    // already formatted
    std::fs::write(&good, "fn foo() {\n    1\n}\n").unwrap();
    // needs formatting (single-line)
    std::fs::write(&bad, "fn bar() { 2 }\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--check"])
        .arg(&good)
        .arg(&bad)
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    // only the unformatted file is reported
    let bad_name = bad.display().to_string();
    assert!(
        stderr.contains(&format!("{bad_name}: needs formatting")),
        "expected '{bad_name}: needs formatting' in stderr: {stderr}",
    );
    // the already-formatted file must not appear
    let good_name = good.display().to_string();
    assert!(
        !stderr.contains(&good_name),
        "good file should not appear in stderr: {stderr}",
    );
    // no summary count line (e.g. "2 files checked, 1 needs formatting")
    assert!(
        !stderr.contains("files checked"),
        "unexpected summary line in stderr: {stderr}",
    );
}

/// All files need formatting → each gets its own line, exit 1, no summary.
#[test]
fn fmt_check_multi_file_all_unformatted_reports_each_file() {
    let dir = tempfile::tempdir().unwrap();
    let a = dir.path().join("a.hew");
    let b = dir.path().join("b.hew");
    std::fs::write(&a, "fn foo() { 1 }\n").unwrap();
    std::fs::write(&b, "fn bar() { 2 }\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--check"])
        .arg(&a)
        .arg(&b)
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    let a_name = a.display().to_string();
    let b_name = b.display().to_string();
    assert!(
        stderr.contains(&format!("{a_name}: needs formatting")),
        "expected '{a_name}: needs formatting' in stderr: {stderr}",
    );
    assert!(
        stderr.contains(&format!("{b_name}: needs formatting")),
        "expected '{b_name}: needs formatting' in stderr: {stderr}",
    );
    assert!(
        !stderr.contains("files checked"),
        "unexpected summary line in stderr: {stderr}",
    );
}
