use std::path::PathBuf;
use std::process::Command;

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

fn run_doc(args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .expect("failed to spawn hew binary")
}

/// A syntactically valid Hew source file should exit 0.
#[test]
fn valid_file_exits_zero() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("ok.hew");
    std::fs::write(
        &src,
        "/// A simple module.\nfn greet(name: str) -> str {\n    name\n}\n",
    )
    .unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_doc(&[
        "doc",
        src.to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert!(
        output.status.success(),
        "expected exit 0 for valid file\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

/// A file with parse errors must exit nonzero and emit an error message.
#[test]
fn parse_error_file_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("bad.hew");
    // Intentionally broken syntax: unclosed brace.
    std::fs::write(&src, "fn broken( {\n").unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_doc(&[
        "doc",
        src.to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert!(
        !output.status.success(),
        "expected nonzero exit for parse-error file\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Error:"),
        "expected 'Error:' in stderr, got: {stderr}",
    );
}

/// Passing a nonexistent file must exit nonzero.
#[test]
fn nonexistent_file_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_doc(&[
        "doc",
        "/this/path/does/not/exist.hew",
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert!(
        !output.status.success(),
        "expected nonzero exit for nonexistent file\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}
