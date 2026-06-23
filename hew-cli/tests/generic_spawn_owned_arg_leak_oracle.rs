//! Generic free-fn task spawn: owned-`string` value-arg safety oracle.
//!
//! ## Design contract (LESSONS.md `by-value-heap-params-are-borrows`)
//!
//! By-value heap params in Hew are `IntentKind::Read` borrows — the callee
//! correctly does NOT drop them (only `#[resource]` params are `Consume`).
//! The caller retains ownership; a callee-side drop of a borrowed buffer
//! double-frees the caller's live allocation → UAF.
//!
//! Under the current M-COW move-only spine the fork-env rc-box passes a
//! `null_drop` destructor, so the by-value string arg inside the env is
//! released by no one — exactly as the direct-call baseline behaves:
//!
//! > "the fork form leaks exactly where the direct call already leaks —
//! > never a double-free."
//! > (lower.rs `lower_spawned_args_call_task`, WHEN-OBSOLETE: M-COW
//! > retain-on-share; the env transfer then needs a real retain/release
//! > pair in lockstep with call args.)
//!
//! ## What this oracle pins
//!
//! 1. **No double-free** — both the generic-fork shape and the direct-call
//!    baseline complete under `MallocScribble`/`MallocPreScribble`/
//!    `MallocGuardEdges`. Any callee-side drop of the borrowed string buffer
//!    (the known-catastrophic "fix" that caused 59 UAF/double-free failures
//!    in v056) aborts here before any result is read.
//!
//! 2. **Conservative-leak parity** — the per-iteration leak delta of the
//!    generic-fork path equals the direct-call baseline delta within
//!    `SLOPE_TOLERANCE` nodes. This proves the generic symbol resolution
//!    added by generic symbol resolution introduces no *additional* heap retention per fork.
//!
//! The conservative leak itself (one string node per iteration, both paths)
//! is the documented design, not a bug. It will be eliminated when M-COW
//! grows retain-on-share (WHEN-OBSOLETE comment above).
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

/// High count for the slope check. A slope of 1.0 node/iter (one string
/// buffer leaked per iteration, both paths) produces a delta of
/// `HIGH - LOW = 80` vs the tolerance of 16 (5× SNR) — both paths must sit
/// at exactly the same node-count delta for the parity assertion to hold.
/// Kept at 100 (down from 500) so the four compile+leaks measurements finish
/// well inside the 300 s nextest slow-timeout on the slowest CI runner.
const HIGH_ITERS: usize = 100;

/// Maximum permitted leak-node delta *between the fork and direct paths* at
/// the same iteration count. A single runtime/scheduler allocation difference
/// between the two binaries (e.g. different actor-spawn overhead) means the
/// absolute counts may differ by a small constant; but the PER-ITERATION
/// DELTA must be equal (both paths leak 1 node/iter). This tolerance absorbs
/// small constant offsets while still catching any extra per-iteration node.
const SLOPE_TOLERANCE: usize = 16;

// ── source generators ─────────────────────────────────────────────────────

/// Generic-fork shape: a generic `fn generic_str_sink<T>(s: string)` spawned
/// with an owned heap string at a concrete type arg (`i64`). The string is
/// built fresh each iteration inside an actor handler so it is guaranteed
/// heap-allocated (not a static literal), then moved into the fork env.
///
/// Under `by-value-heap-params-are-borrows`, the callee does not drop `s`;
/// the env rc-box uses a `null_drop` destructor so the string bytes are also
/// not freed via the env. The string leaks conservatively — exactly 1 node
/// per iteration, matching the direct-call baseline.
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

/// Direct-call baseline: the same owned heap string passed to a non-generic,
/// non-fork callee. Same conservative-leak shape (1 node/iter); serves as
/// the denominator for the parity assertion. If the two deltas diverge the
/// fork path is retaining something the direct call does not.
fn direct_call_source(iters: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         fn direct_str_sink(s: string) {{}}\n\
         \n\
         actor DirectDriver {{\n\
         \x20   receive fn run_n(n: i64) -> i64 {{\n\
         \x20       var i: i64 = 0;\n\
         \x20       while i < n {{\n\
         \x20           let s = string.repeat(\"owned-heap-string-payload\", 20);\n\
         \x20           direct_str_sink(s);\n\
         \x20           i = i + 1;\n\
         \x20       }}\n\
         \x20       0\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let d = spawn DirectDriver;\n\
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

/// Both the generic-fork shape and the direct-call baseline must run to
/// completion under the poisoned-allocator triple. Any callee-side drop of
/// the borrowed string buffer (the revert-triggering "fix" from v056) crashes
/// here: `MallocScribble` poisons freed bytes; the caller's subsequent reads
/// of the scribbled buffer abort or produce a wrong exit code.
///
/// This is the headline safety property: the conservative leak is the correct
/// design; the absence of a double-free is the proof.
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
    let direct_bin = compile_to_native(
        &direct_call_source(HIGH_ITERS),
        dir.path(),
        "direct_call_str_high",
    );

    assert_no_poisoned_allocator_abort(&fork_bin, "generic-fork owned-string arg");
    assert_no_poisoned_allocator_abort(&direct_bin, "direct-call owned-string arg (baseline)");
}

/// Per-iteration leak delta for the generic-fork path must equal the
/// direct-call baseline delta within `SLOPE_TOLERANCE`. Both paths
/// conservatively leak exactly one string node per iteration under the
/// current M-COW move-only spine (WHEN-OBSOLETE: retain-on-share).
///
/// A diverging delta would mean the generic symbol resolution or fork-env
/// construction is retaining an *additional* heap node per invocation — an
/// extra leak the direct-call path does not have.
#[test]
fn generic_fork_owned_str_leak_matches_direct_call_baseline() {
    if !leaks_available() {
        eprintln!("skip: generic_spawn_owned_arg oracle: leaks(1) is macOS-only / not on PATH");
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("generic-spawn-str-parity-")
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
    let direct_low = compile_to_native(
        &direct_call_source(LOW_ITERS),
        dir.path(),
        "direct_call_str_low",
    );
    let direct_high = compile_to_native(
        &direct_call_source(HIGH_ITERS),
        dir.path(),
        "direct_call_str_high",
    );

    let Some(fork_low_leaks) = measure_leaks(&fork_low) else {
        return;
    };
    let Some(fork_high_leaks) = measure_leaks(&fork_high) else {
        return;
    };
    let Some(direct_low_leaks) = measure_leaks(&direct_low) else {
        return;
    };
    let Some(direct_high_leaks) = measure_leaks(&direct_high) else {
        return;
    };

    let fork_delta = fork_high_leaks.saturating_sub(fork_low_leaks);
    let direct_delta = direct_high_leaks.saturating_sub(direct_low_leaks);

    eprintln!(
        "generic_fork_owned_str_leak_parity: \
         fork_low={fork_low_leaks} fork_high={fork_high_leaks} fork_delta={fork_delta} \
         direct_low={direct_low_leaks} direct_high={direct_high_leaks} \
         direct_delta={direct_delta} tolerance={SLOPE_TOLERANCE}"
    );

    let delta_diff = fork_delta.abs_diff(direct_delta);
    assert!(
        delta_diff <= SLOPE_TOLERANCE,
        "generic-fork owned-string arg leaks MORE per iteration than the \
         direct-call baseline — the generic symbol resolution or fork-env \
         construction is retaining an extra heap node per invocation. \
         fork_delta={fork_delta} direct_delta={direct_delta} diff={delta_diff} \
         tolerance={SLOPE_TOLERANCE}. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to identify \
         the extra allocation site.",
        fork_high.display()
    );
}
