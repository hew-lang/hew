//! A diagnosable condition must not core-dump the compiler.
//!
//! # What went wrong
//!
//! `scripts/corpus-ratchet.sh stdlib` detects a failure with an unpiped run and then
//! REPORTS it through a pipe:
//!
//! ```sh
//! "$HEW_BIN" check "$path" 2>&1 | head -3 | sed 's/^/    /'
//! ```
//!
//! `head -3` closes the pipe after three lines. Rust installs `SIG_IGN` for
//! SIGPIPE before `main`, so the fourth write returns `EPIPE`, and the print
//! macros PANIC on a failed write (`failed printing to stderr`). This
//! workspace builds with `panic = "abort"`, so that panic became a SIGABRT.
//!
//! The consequence was a compiler that exited **134 with a core dump** on a
//! file it had a perfectly good diagnostic for — and, because the ratchet runs
//! under `set -e -o pipefail`, the failing status also truncated the report
//! after the first entry, hiding a second regressed file entirely. The user
//! got nothing, twice over: no diagnostic, and an incomplete list.
//!
//! The same file run WITHOUT a pipe exited 1 and printed everything, which is
//! why the abort read as platform-specific when it was not.
//!
//! # The fix
//!
//! Restore SIGPIPE's default disposition once, at the top of `main`, before
//! any output exists. That covers every subcommand and every one of the ~250
//! print sites at once, and gives the ordinary Unix outcome for this
//! situation: the process dies by SIGPIPE (141), silently, which is exactly
//! what `head` expects from its producer.
//!
//! These cases pin BOTH halves: the process must not abort, and the
//! diagnostics must still arrive in full when nobody closes the pipe.

mod support;

use std::fmt::Write as _;
use std::io::Write;
use std::process::{Command, Stdio};
use support::hew_binary;

/// The volume matters. A pipe holds 64 KiB before it blocks, so a small
/// diagnostic run is fully buffered and the producer never learns the reader
/// left — the abort would be a race. `REJECTED_ERRORS` type errors render well
/// past that, so the compiler is guaranteed to still be writing when `head`
/// closes, which is the condition under test.
const REJECTED_ERRORS: usize = 400;

fn rejected_source() -> String {
    let mut src = String::from("fn main() -> i64 {\n");
    for i in 0..REJECTED_ERRORS {
        let _ = writeln!(src, "    let v{i}: i64 = \"s{i}\";");
    }
    src.push_str("    0\n}\n");
    src
}

fn write_fixture(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("hew-sigpipe-{}-{name}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("fixture dir");
    let path = dir.join("rejected.hew");
    let mut file = std::fs::File::create(&path).expect("fixture file");
    file.write_all(rejected_source().as_bytes())
        .expect("write fixture");
    path
}

/// SIGABRT. Anything else — a clean exit, or death by SIGPIPE — is acceptable;
/// this is the single outcome that destroys the user's information.
#[cfg(unix)]
const SIGABRT: i32 = 6;

#[cfg(unix)]
#[test]
fn a_rejected_file_reported_through_a_closed_pipe_does_not_abort() {
    use std::os::unix::process::ExitStatusExt;

    let fixture = write_fixture("closed-pipe");

    // `head -3` is the ratchet's own reporting reader, reproduced exactly:
    // it takes three lines and closes, while `hew check` still has more to
    // say.
    let mut checker = Command::new(hew_binary())
        .arg("check")
        .arg(&fixture)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn hew check");

    let stdout = checker.stdout.take().expect("stdout pipe");
    let stderr = checker.stderr.take().expect("stderr pipe");

    let mut head = Command::new("head")
        .arg("-3")
        .stdin(Stdio::from(stderr))
        .stdout(Stdio::null())
        .spawn()
        .expect("spawn head");
    drop(stdout);

    let _ = head.wait().expect("head exits");
    let status = checker.wait().expect("hew check exits");

    assert_ne!(
        status.signal(),
        Some(SIGABRT),
        "a closed output pipe must not abort the compiler; a diagnosable \
         condition that core-dumps gives the user nothing at all"
    );

    let _ = std::fs::remove_dir_all(fixture.parent().expect("fixture dir"));
}

/// The other half. Resetting the disposition must not cost a single line of
/// diagnostic when the reader stays open — otherwise the abort would have been
/// traded for silence.
#[test]
fn the_same_file_still_reports_its_full_diagnostics_when_nobody_closes_the_pipe() {
    let fixture = write_fixture("open-pipe");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&fixture)
        .output()
        .expect("run hew check");

    assert!(
        !output.status.success(),
        "the fixture must be rejected for this pin to mean anything"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    let reported = stderr.matches("type mismatch").count();
    assert_eq!(
        reported, REJECTED_ERRORS,
        "every diagnostic must still be printed in full, not truncated at the \
         first short write"
    );

    let _ = std::fs::remove_dir_all(fixture.parent().expect("fixture dir"));
}
