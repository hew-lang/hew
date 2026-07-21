//! End-to-end golden test for `hew check --explain-cow`.
//!
//! Invokes the `hew` binary on a fixture file containing two actor send sites
//! and asserts that the `--explain-cow` output matches the expected format and
//! site list.
//!
//! Fixture: `tests/fixtures/explain_cow_actor_sends.hew`
//! - Line 24: `printer.print_message(greeting)` — sends a `String` value
//!   (non-Copy); classifier returns ALIAS (bare-identifier non-Copy non-Drop
//!   send, alias gate enabled).
//! - Line 25: `counter.increment(42)` — sends an `i64` literal (Copy type);
//!   classifier returns COPY (Copy type, not eligible for alias path).
//!
//! The golden test pins the two-class output so that any regression in the
//! classifier or the renderer is immediately visible.

mod support;

use std::path::Path;
use support::{hew_binary, strip_ansi};

fn fixture_path() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("explain_cow_actor_sends.hew")
}

/// `hew check --explain-cow` exits 0 and emits one line per actor send site.
///
/// The fixture has two sends: a last-use `String` and an `i64` literal.
/// - exit code 0 (file type-checks successfully)
/// - exactly two send entries appear in stdout
/// - the String send is classified `TRANSFER_LAST_USE`
/// - the i64 send is classified `SNAPSHOT_BIT_COPY`
/// - each entry names the fixture file
#[test]
fn explain_cow_renders_mir_authored_send_modes() {
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
        2,
        "expected exactly 2 send entries in --explain-cow output, got {}:\n{stdout}",
        send_lines.len()
    );

    let transfer_line = send_lines
        .iter()
        .find(|line| line.contains("TRANSFER_LAST_USE"))
        .copied();
    assert!(
        transfer_line.is_some(),
        "expected one TRANSFER_LAST_USE entry, got:\n{stdout}"
    );

    let copy_line = send_lines
        .iter()
        .find(|line| line.contains("SNAPSHOT_BIT_COPY"))
        .copied();
    assert!(
        copy_line.is_some(),
        "expected one SNAPSHOT_BIT_COPY entry, got:\n{stdout}"
    );

    // Both entries must name the fixture file.
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
    let send_lines: Vec<&str> = stdout.lines().filter(|l| l.contains("send —")).collect();
    assert!(
        send_lines.is_empty(),
        "no actor sends: expected no send lines in output, got: {send_lines:?}"
    );
}
