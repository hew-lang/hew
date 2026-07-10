//! Empirical leak / UAF oracle for supervisor child-spec cleanup and the
//! `#[on(crash)]` `CrashInfo.message` lifecycle on a REAL crash.
//!
//! ## The bug this pins
//!
//! When a real `#[on(crash)]` hook names a `CrashInfo` parameter and returns a
//! `CrashAction`, the supervisor invokes the codegen-emitted `__on_crash` on the
//! actual trap. That hook's synthetic prologue builds `CrashInfo { code, message
//! }` from the two ABI params. The `message` field is a borrow — the supervisor
//! owns the underlying string and frees it after the call.
//!
//! Two compounding defects (fixed together) made this path abort or corrupt the
//! heap on every real crash:
//!
//!   (a) Representation mismatch. The supervisor handed the hook a bare Rust
//!       `CString` (no Hew refcount header), but the field is typed `string`
//!       (Hew header-aware). `hew_string_clone`/`hew_string_drop` read a 16-byte
//!       header at `data - CSTRING_HEADER_SIZE`; on a headerless pointer that is
//!       an OOB read that misses the sentinel and `libc::abort()`s the runtime.
//!       Fix: the supervisor now allocates the message via `str_to_malloc`
//!       (header-aware, rc == 1) and frees it via `free_cstring`.
//!
//!   (b) Move-of-borrow. The MIR field-init `agg_alias`'d (moved) the borrowed
//!       message into the owned `CrashInfo.message` and then dropped it — freeing
//!       the supervisor's buffer out from under it (double-free / UAF). Fix: the
//!       prologue now CLONES the borrow (`hew_string_clone`, a `+1` owner) into
//!       the field; the function frame drops that owner once; the supervisor's
//!       own `free_cstring` balances the original allocation.
//!
//! With both fixes the refcount is balanced: supervisor `str_to_malloc` (rc=1) →
//! hook `hew_string_clone` (rc=2) → hook `CrashInfo` drop (rc=1) → supervisor
//! `free_cstring` (rc=0, freed). One allocation, freed exactly once.
//!
//! ## What this oracle measures
//!
//! Runs three committed fixtures under the macOS poisoned-allocator triple
//! (`MallocScribble` + `MallocPreScribble` + `MallocGuardEdges`) and
//! `leaks --atExit`:
//!
//!   1. `supervisor_normal_return_cleanup` returns 42 with a live supervisor and
//!      three registered children. This is the direct #2252 normal-return
//!      child-spec ownership oracle.
//!   2. `on_crash_action_restart_real_crash` returns 42 after a real crash and
//!      restart, proving the cleanup tail preserves the crash-message path.
//!   3. `supervisor_stop_basic` explicitly stops its supervisor and returns 0,
//!      proving the implicit cleanup tail does not double-consume the root.
//!
//! Every fixture must report exactly zero leaked nodes and bytes.
//!
//! ## Skip behaviour
//!
//! macOS-only (`leaks(1)` is Darwin's allocator inspector; Linux coverage is the
//! ASan/LSan gate in `scripts/asan-fixture-check.sh`, which runs the same fixture
//! as a clean probe). On other platforms the test logs `skip:` and returns.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Compile the named committed fixture to a native binary and return its path.
fn compile_fixture(name: &str, dir: &std::path::Path) -> PathBuf {
    let src = repo_root()
        .join("tests/vertical-slice/accept")
        .join(format!("{name}.hew"));
    assert!(src.is_file(), "fixture not found: {}", src.display());

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            src.to_str().expect("src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let bin = stdout
        .lines()
        .find_map(|l| l.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no `native:` line for {name}:\n{stdout}"))
        .to_string();
    PathBuf::from(bin)
}

/// Run `bin` under `leaks --atExit` + the poisoned-allocator triple with
/// `MallocStackLogging` so leak roots carry symbolised stacks. Returns the full
/// report text, or `None` when `leaks` declined to attach.
fn leaks_report(bin: &std::path::Path) -> Option<String> {
    let output = Command::new("leaks")
        .arg("--atExit")
        .arg("--")
        .arg(bin)
        .env("MallocStackLogging", "1")
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("HEW_WORKERS", "2")
        .output()
        .ok()?;
    if !output.status.success() && output.stdout.is_empty() {
        eprintln!(
            "skip: leaks declined to attach to {}: {}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
    Some(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn leak_summary(report: &str) -> Option<(usize, usize)> {
    report.lines().find_map(|line| {
        let rest = line.strip_prefix("Process ")?;
        if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            return None;
        }
        let summary = rest.split_once(": ")?.1;
        let mut words = summary.split_whitespace();
        let leaks = words.next()?.parse().ok()?;
        let leak_word = words.next()?;
        if leak_word != "leak" && leak_word != "leaks" {
            return None;
        }
        if words.next()? != "for" {
            return None;
        }
        let bytes = words.next()?.parse().ok()?;
        Some((leaks, bytes))
    })
}

fn assert_plain_exit(bin: &std::path::Path, expected: i32, label: &str) {
    let run = Command::new(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("HEW_WORKERS", "2")
        .output()
        .unwrap_or_else(|error| panic!("run {label} under guard malloc: {error}"));
    assert_eq!(
        run.status.code(),
        Some(expected),
        "{label} did not preserve its expected exit code under the poisoned allocator.\n{}",
        describe_output(&run)
    );
}

/// Normal return, real-crash restart, and explicit stop must all converge on the
/// canonical supervisor-root destructor path exactly once.
#[test]
fn supervisor_exit_paths_are_leak_free_under_guard_malloc() {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: leaks(1) is macOS-only (Linux coverage: scripts/asan-fixture-check.sh)");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: `leaks` binary not on PATH");
        return;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("on-crash-message-leak-")
        .tempdir()
        .expect("tempdir");

    for (fixture, expected_exit) in [
        ("supervisor_normal_return_cleanup", 42),
        ("on_crash_action_restart_real_crash", 42),
        ("supervisor_stop_basic", 0),
    ] {
        let bin = compile_fixture(fixture, dir.path());
        assert_plain_exit(&bin, expected_exit, fixture);
        let Some(report) = leaks_report(&bin) else {
            return;
        };
        let summary = leak_summary(&report).unwrap_or_else(|| {
            panic!(
                "leaks did not produce a usable summary for {fixture}; a double-free, OOB, \
                 or early abort may have prevented the snapshot. Report:\n{report}"
            )
        });
        assert_eq!(
            summary,
            (0, 0),
            "{fixture} must free every registered supervisor root exactly once; \
             observed {} leak(s) for {} byte(s). Report:\n{report}",
            summary.0,
            summary.1
        );
    }
}
