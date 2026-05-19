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
/// The fixture has two sends: a `String` (non-Copy bare identifier, yields
/// ALIAS) and an `i64` (Copy type, yields COPY). The test verifies:
/// - exit code 0 (file type-checks successfully)
/// - exactly two send entries appear in stdout
/// - the String send is classified ALIAS
/// - the i64 send is classified COPY with the feature-gate reason
/// - each entry names the fixture file
#[test]
fn explain_cow_golden_alias_and_copy_sites() {
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
    let send_lines: Vec<&str> = stdout.lines().filter(|l| l.contains("send —")).collect();
    assert_eq!(
        send_lines.len(),
        2,
        "expected exactly 2 send entries in --explain-cow output, got {}:\n{stdout}",
        send_lines.len()
    );

    // Line 24 (String send) must be ALIAS.
    let alias_line = send_lines.iter().find(|l| l.contains("ALIAS")).copied();
    assert!(
        alias_line.is_some(),
        "expected one ALIAS entry (String send), got:\n{stdout}"
    );
    let alias_line = alias_line.unwrap();
    assert!(
        alias_line.contains(":24:"),
        "ALIAS entry must be on line 24 (String send), got: {alias_line:?}"
    );

    // Line 25 (i64 send) must be COPY with the feature-gate reason.
    let copy_line = send_lines.iter().find(|l| l.contains("COPY")).copied();
    assert!(
        copy_line.is_some(),
        "expected one COPY entry (i64 send), got:\n{stdout}"
    );
    let copy_line = copy_line.unwrap();
    assert!(
        copy_line.contains(":25:"),
        "COPY entry must be on line 25 (i64 send), got: {copy_line:?}"
    );
    assert!(
        copy_line.contains("non-identifier expression"),
        "COPY entry for the i64 literal send must name the `non-identifier expression` reason, got: {copy_line:?}"
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
