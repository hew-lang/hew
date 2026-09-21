//! Source-level checks for reports from verified semantic ownership operations.

mod support;

use std::path::Path;
use support::{hew_binary, strip_ansi};

fn fixture_path() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("explain_cow_actor_sends.hew")
}

#[test]
fn explain_cow_distinguishes_retained_transferred_and_bit_copy_arguments() {
    let fixture = fixture_path();
    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");

    let output = std::process::Command::new(hew_binary())
        .args(["check", "--explain-cow", fixture_str])
        .output()
        .expect("hew binary must run");

    assert!(
        output.status.success(),
        "hew check --explain-cow must exit 0 on a valid file\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));

    // Collect only the send-entry lines (not the trailing "file: OK" on stderr).
    let send_lines: Vec<&str> = stdout.lines().filter(|l| l.contains("send -")).collect();
    assert_eq!(
        send_lines.len(),
        3,
        "expected exactly 3 send entries in --explain-cow output, got {}:\n{stdout}",
        send_lines.len()
    );

    let transfer_line = send_lines
        .iter()
        .find(|line| line.ends_with("string: move"))
        .copied();
    assert!(
        transfer_line.is_some(),
        "expected a fresh string ownership transfer, got:\n{stdout}"
    );

    assert!(
        send_lines
            .iter()
            .any(|line| line.contains("string: move (retain)")),
        "expected the still-live string to be retained:\n{stdout}"
    );

    let copy_line = send_lines
        .iter()
        .find(|line| line.contains("i64: move (bit copy)"))
        .copied();
    assert!(
        copy_line.is_some(),
        "expected one bit-copy argument, got:\n{stdout}"
    );

    // Every entry must name the fixture file.
    let fixture_name = fixture
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("explain_cow_actor_sends.hew");
    for line in &send_lines {
        assert!(
            line.contains(fixture_name) || line.contains(fixture_str),
            "entry must name the fixture file, got: {line:?}"
        );
    }
}

/// `hew check --explain-cow` produces no send lines when the file has no
/// actor sends.
#[test]
fn explain_cow_no_sends_produces_no_send_lines() {
    let dir = support::tempdir();
    let file = dir.path().join("no_sends.hew");
    std::fs::write(&file, "fn main() {\n    let x = 1 + 2;\n}\n").expect("write fixture");

    let output = std::process::Command::new(hew_binary())
        .args(["check", "--explain-cow", file.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    assert!(
        output.status.success(),
        "hew check --explain-cow must succeed on a file with no actors\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let send_lines: Vec<&str> = stdout.lines().filter(|l| l.contains("send -")).collect();
    assert!(
        send_lines.is_empty(),
        "no actor sends: expected no send lines in output, got: {send_lines:?}"
    );
}
