//! Generic free-fn task spawn: owned-`string` value-arg safety oracle.
//!
//! ## Design contract (LESSONS.md `by-value-heap-params-are-borrows`)
//!
//! By-value heap params in Hew are `IntentKind::Read` borrows — the callee
//! correctly does NOT drop them (only `#[resource]` params are `Consume`).
//! The caller retains ownership; a callee-side drop of a borrowed buffer
//! double-frees the caller's live allocation → UAF.
//!
//! The fork environment receives the source's owner, so its Rc payload callback
//! releases the string after the task completes. The callee remains a borrow and
//! must not participate in teardown.
//!
//! ## What this oracle pins
//!
//! 1. **No double-free** — the generic-fork shape completes under
//!    `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`. A callee-side
//!    drop of the borrowed string buffer would abort here before any result is
//!    read.
//!
//! 2. **Flat leak slope** — the low/high standalone fork binaries differ by at
//!    most `SLOPE_TOLERANCE` nodes, proving the environment releases its moved
//!    string once per invocation.
//!
//! macOS-only (`leaks(1)` is Darwin's allocator inspector); skips elsewhere.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── iteration counts ──────────────────────────────────────────────────────

/// Low count: exercises the loop body enough times to warm the scheduler
/// while staying near the constant-overhead floor.
const LOW_ITERS: usize = 20;

/// High count keeps a one-node-per-iteration regression well above the shared
/// tolerance while remaining fast enough for the standalone `leaks` runs.
const HIGH_ITERS: usize = 100;

/// Maximum permitted low/high leak-node delta for the same fork program.
const SLOPE_TOLERANCE: usize = 16;

// ── source generators ─────────────────────────────────────────────────────

/// Generic-fork shape: a generic `fn generic_str_sink<T>(s: string)` spawned
/// with an owned heap string at a concrete type arg (`i64`). The string is
/// built fresh each iteration inside an actor handler so it is guaranteed
/// heap-allocated (not a static literal), then moved into the fork env.
///
/// Under `by-value-heap-params-are-borrows`, the callee does not drop `s`;
/// the fork environment's Rc callback is the sole release site.
fn generic_fork_source(iters: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         fn generic_str_sink<T>(s: string) {{}}\n\
         \n\
         actor ForkDriver {{\n\
         \x20   receive fn run_n(n: i64) -> i64 {{\n\
         \x20       var i: i64 = 0;\n\
         \x20       while i < n {{\n\
         \x20           let s = string.repeat(\"owned-heap-string-payload\", 20);\n\
         \x20           scope {{\n\
         \x20               fork {{ generic_str_sink::<i64>(s); }}\n\
         \x20           }};\n\
         \x20           i = i + 1;\n\
         \x20       }}\n\
         \x20       0\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let d = spawn ForkDriver;\n\
         \x20   match await d.run_n({iters}) {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

// ── plumbing (same shape as sibling oracles) ──────────────────────────────

fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and
/// return `Some(leak_count)` when the report is parseable.
fn measure_leaks(bin: &std::path::Path) -> Option<usize> {
    let output = Command::new("leaks")
        .arg("--atExit")
        .arg("--")
        .arg(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .ok()?;
    let report = String::from_utf8_lossy(&output.stdout);
    if !report.contains(" leaks for ") && !report.contains(" leak for ") {
        eprintln!(
            "skip: leaks did not emit a usable summary for {}: stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
    for line in report.lines() {
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            continue;
        }
        if let Some(rest) = line.strip_prefix("Process ") {
            if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                continue;
            }
            if let Some(after_colon) = rest.split_once(": ").map(|(_, s)| s) {
                if let Some(n) = after_colon.split_whitespace().next() {
                    if let Ok(n) = n.parse::<usize>() {
                        eprintln!("  parsed leak count from line: {line}");
                        return Some(n);
                    }
                }
            }
        }
    }
    None
}

/// Run `bin` under the poisoned-allocator triple (no `leaks`) and assert it
/// exits cleanly. A callee-side drop of a borrowed string buffer (the
/// known-catastrophic fix) would cause `MallocScribble` to scribble the
/// freed bytes before the caller re-reads them, producing a crash or a
/// scribbled result detectable by the non-zero exit.
fn assert_no_poisoned_allocator_abort(bin: &std::path::Path, label: &str) {
    let output = Command::new(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("MallocCheckHeapEach", "256")
        .output()
        .unwrap_or_else(|e| panic!("run {label} binary under poisoned allocator: {e}"));
    assert!(
        output.status.success(),
        "{label}: aborted under the poisoned-allocator triple — a callee-side \
         drop of a by-value string param double-freed the caller's live buffer \
         (by-value-heap-params-are-borrows): {}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
}

fn leaks_available() -> bool {
    cfg!(target_os = "macos")
        && Command::new("which")
            .arg("leaks")
            .output()
            .is_ok_and(|o| o.status.success())
}

// ── oracles ───────────────────────────────────────────────────────────────

/// The generic-fork shape must complete under the poisoned-allocator triple.
/// A callee-side drop of the borrowed string buffer would crash here.
#[test]
fn generic_fork_owned_str_no_double_free() {
    if !leaks_available() {
        eprintln!("skip: generic_spawn_owned_arg oracle: leaks(1) is macOS-only / not on PATH");
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("generic-spawn-str-ndf-")
        .tempdir()
        .expect("tempdir");

    let fork_bin = compile_to_native(
        &generic_fork_source(HIGH_ITERS),
        dir.path(),
        "generic_fork_str_high",
    );
    assert_no_poisoned_allocator_abort(&fork_bin, "generic-fork owned-string arg");
}

/// The generic-fork environment must release its moved string once per
/// invocation, keeping the low/high standalone leak slope flat.
#[test]
fn generic_fork_owned_str_leak_slope_is_flat() {
    if !leaks_available() {
        eprintln!("skip: generic_spawn_owned_arg oracle: leaks(1) is macOS-only / not on PATH");
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("generic-spawn-str-slope-")
        .tempdir()
        .expect("tempdir");

    let fork_low = compile_to_native(
        &generic_fork_source(LOW_ITERS),
        dir.path(),
        "generic_fork_str_low",
    );
    let fork_high = compile_to_native(
        &generic_fork_source(HIGH_ITERS),
        dir.path(),
        "generic_fork_str_high",
    );
    let Some(fork_low_leaks) = measure_leaks(&fork_low) else {
        return;
    };
    let Some(fork_high_leaks) = measure_leaks(&fork_high) else {
        return;
    };
    let fork_delta = fork_high_leaks.saturating_sub(fork_low_leaks);

    eprintln!(
        "generic_fork_owned_str_leak_slope: \
         fork_low={fork_low_leaks} fork_high={fork_high_leaks} \
         fork_delta={fork_delta} tolerance={SLOPE_TOLERANCE}"
    );

    assert!(
        fork_delta <= SLOPE_TOLERANCE,
        "generic-fork owned-string argument leaks per iteration — the Rc \
         environment callback did not release the moved argument. \
         fork_delta={fork_delta} \
         tolerance={SLOPE_TOLERANCE}. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to identify \
         the extra allocation site.",
        fork_high.display()
    );
}
