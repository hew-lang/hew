//! End-to-end golden test for `hew check --explain-cow`.
//!
//! Invokes the `hew` binary on a fixture file containing two actor send sites
//! and asserts that the `--explain-cow` output matches the expected format and
//! site list.
//!
//! Fixture: `tests/fixtures/explain_cow_actor_sends.hew`
//! - Line 24: `printer.print_message(greeting)` — sends a `String` value.
//!   Phase α records COPY (feature gate disabled) at every send site.
//! - Line 25: `counter.increment(42)` — sends an `int` literal (Copy type).
//!   Phase α records COPY (feature gate disabled).
//!
//! The golden test pins the Phase-α all-COPY output so that any regression in
//! the classifier or the renderer is immediately visible. The alias-enable
//! commit (commit 7 in this lane) will update the String-send entry to ALIAS
//! once `classify_actor_send_aliasing` is wired in.

mod support;

use std::path::Path;
use support::{hew_binary, strip_ansi};

fn fixture_path() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("explain_cow_actor_sends.hew")
}

/// `hew check --explain-cow` exits 0 and emits one COPY line per actor send.
///
/// Phase α records COPY at every send site (alias gate disabled). The fixture
/// has two sends: a `String` (non-Copy, still COPY in α) and an `int`
/// (Copy type, also COPY). The test verifies:
/// - exit code 0 (file type-checks successfully)
/// - exactly two send entries appear in stdout
/// - both entries are classified COPY with the feature-gate reason
/// - each entry names the fixture file at the expected line
#[test]
fn explain_cow_golden_two_copy_sites() {
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

    // Both entries must be COPY in Phase α (alias gate is disabled).
    for line in &send_lines {
        assert!(
            line.contains("COPY"),
            "Phase α: every send must be COPY (alias gate disabled), got: {line:?}"
        );
        assert!(
            line.contains("feature gate disabled"),
            "COPY entry must carry the feature-gate-disabled reason, got: {line:?}"
        );
    }

    // Line 24: String send.
    let line24 = send_lines.iter().find(|l| l.contains(":24:")).copied();
    assert!(
        line24.is_some(),
        "expected a send entry on line 24 (String send), got:\n{stdout}"
    );

    // Line 25: int send.
    let line25 = send_lines.iter().find(|l| l.contains(":25:")).copied();
    assert!(
        line25.is_some(),
        "expected a send entry on line 25 (int send), got:\n{stdout}"
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
