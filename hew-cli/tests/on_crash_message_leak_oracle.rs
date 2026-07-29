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
//!   2. `on_crash_action_restart_real_crash` returns 43 after a real crash,
//!      state-template mutation, and restart, proving the cleanup tail
//!      preserves the crash-message path.
//!   3. `supervisor_stop_basic` explicitly stops its supervisor and returns 0,
//!      proving the implicit cleanup tail does not double-consume the root.
//!
//! Every fixture must report exactly zero leaked nodes and bytes.
//!
//! ## Skip behaviour
//!
//! macOS-only (`leaks(1)` is Darwin's allocator inspector; Linux coverage is the
//! ASan/LSan gate in `scripts/asan-fixture-check.sh`, which runs the same fixture
//! as a clean probe). On other platforms libtest records a counted skip.

#![cfg(unix)]

mod support;

use support::leak_slope::{parse_leaks_summary, require_leaks_tool};

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

/// Compile the named committed fixture to a native binary and return its path.
fn compile_fixture(name: &str, dir: &std::path::Path) -> PathBuf {
    let src = repo_root()
        .join("tests/vertical-slice/accept")
        .join(format!("{name}.hew"));
    assert!(src.is_file(), "fixture not found: {}", src.display());

    let mut command = Command::new(hew_binary());
    command
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            src.to_str().expect("src utf-8"),
        ])
        .current_dir(repo_root());
    let output = run_bounded_command(command, format!("compile fixture {name}"));

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
/// `MallocStackLogging` so leak roots carry symbolised stacks. A declined
/// attachment is a provisioning failure, never a successful measurement.
fn leaks_report(bin: &std::path::Path) -> String {
    let mut command = Command::new("leaks");
    command
        .arg("--atExit")
        .arg("--")
        .arg(bin)
        .env("MallocStackLogging", "1")
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("HEW_WORKERS", "2");
    let output = run_bounded_command(command, format!("inspect {} with leaks(1)", bin.display()));
    assert!(
        output.status.success() || !output.stdout.is_empty(),
        "leaks declined to attach to {}: {}. A leak oracle that cannot measure must not \
         report success.",
        bin.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn assert_plain_exit(bin: &std::path::Path, expected: i32, label: &str) {
    let mut command = Command::new(bin);
    command
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("HEW_WORKERS", "2");
    let run = run_bounded_command(command, format!("run {label} under guard malloc"));
    assert_eq!(
        run.status.code(),
        Some(expected),
        "{label} did not preserve its expected exit code under the poisoned allocator.\n{}",
        describe_output(&run)
    );
}

fn assert_fixture_is_leak_free(
    fixture: &str,
    expected_exit: i32,
    dir: &std::path::Path,
    invariant: &str,
) {
    let bin = compile_fixture(fixture, dir);
    assert_plain_exit(&bin, expected_exit, fixture);
    let report = leaks_report(&bin);
    let summary = parse_leaks_summary(&report).unwrap_or_else(|| {
        panic!(
            "leaks did not produce a usable summary for {fixture}; a double-free, OOB, \
             or early abort may have prevented the snapshot. Report:\n{report}"
        )
    });
    assert_eq!(
        summary,
        (0, 0),
        "{fixture} {invariant}; observed {} leak(s) for {} byte(s). \
         Report:\n{report}",
        summary.0,
        summary.1
    );
}

/// Normal return, real-crash restart, and explicit stop must all converge on the
/// canonical supervisor-root destructor path exactly once.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn supervisor_exit_paths_are_leak_free_under_guard_malloc() {
    require_leaks_tool();

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("on-crash-message-leak-")
        .tempdir()
        .expect("tempdir");

    for (fixture, expected_exit) in [
        ("supervisor_normal_return_cleanup", 42),
        ("on_crash_action_restart_real_crash", 43),
        ("supervisor_stop_basic", 0),
    ] {
        assert_fixture_is_leak_free(
            fixture,
            expected_exit,
            dir.path(),
            "must free every registered supervisor root exactly once",
        );
    }
}

/// A config supervisor whose children all use literal init args must release
/// the otherwise-unadopted config buffer at process exit.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn supervisor_literal_only_config_param_no_leak() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("supervisor-literal-config-leak-")
        .tempdir()
        .expect("tempdir");
    assert_fixture_is_leak_free(
        "supervisor_literal_only_config_param",
        0,
        dir.path(),
        "must not leak the config buffer allocated without a runtime adopter \
         (S1 fix regression)",
    );
}
