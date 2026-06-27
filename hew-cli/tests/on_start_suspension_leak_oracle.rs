//! Lifecycle-hook suspend/resume coro-frame reclamation (#2269).
//!
//! A suspending `#[on(start)]` hook lowers as an `llvm.coro` presplitcoroutine.
//! The fix parks the hook's continuation against the spawned actor and lets the
//! scheduler's resume re-entry drive it to completion across the suspension; the
//! continuation frame is reclaimed exactly once via `hew_cont_destroy` (either by
//! the spawn site's Ready arm or by the scheduler's `destroy_parked` when the
//! resume completes). A regression that parked-but-never-destroyed, or that
//! re-allocated the frame per spawn without reclaiming it, would leak one coro
//! frame PER spawned actor — a per-actor slope.
//!
//! Slope oracle: spawn a LOW count and a HIGH count of actors whose `#[on(start)]`
//! sleeps once before writing state, drain them to completion, and assert the
//! leak NODE-count delta is bounded by a small constant independent of the actor
//! count. A per-actor coro-frame leak shows up as `HIGH - LOW` extra nodes (one
//! per actor); the tolerance absorbs only one-off scheduler/runtime allocations.
//!
//! macOS-only (`leaks(1)`); logs `skip:` and returns on any other platform or
//! when `leaks` declines to attach.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low actor count: enough to spawn + resume several suspending hooks.
const LOW_ACTORS: usize = 3;

/// High actor count for the slope check. A per-actor coro-frame leak would
/// produce `HIGH - LOW = 37` extra leak nodes against the tolerance of 5.
const HIGH_ACTORS: usize = 40;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. A
/// per-actor coro-frame leak (the regression class) each produces its own node.
const SLOPE_TOLERANCE: usize = 5;

/// Spawn `count` actors whose `#[on(start)]` hook suspends once (a `sleep`),
/// then writes state. `main` sleeps long enough for every hook to resume and
/// complete before the process exits, so a non-reclaimed coro frame is a true
/// leak (not work still in flight).
fn suspending_on_start_source(count: usize) -> String {
    use std::fmt::Write as _;
    let spawns = (0..count).fold(String::new(), |mut acc, i| {
        let _ = writeln!(acc, "    let a{i} = spawn Slow;");
        acc
    });
    format!(
        "actor Slow {{\n\
         \x20   var value: i64 = 0;\n\
         \x20   #[on(start)] fn started() {{\n\
         \x20       sleep(5ms);\n\
         \x20       value = 1;\n\
         \x20   }}\n\
         \x20   receive fn get() -> i64 {{ value }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         {spawns}\
         \x20   sleep(500ms);\n\
         }}\n"
    )
}

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

/// Run `bin` under the macOS poisoned-allocator triple + `leaks --atExit` and
/// return the parsed leak-node count, or `None` if `leaks` declined to attach.
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
    if !output.status.success() && output.stdout.is_empty() {
        eprintln!(
            "skip: leaks declined to attach to {}: {}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
    let report = String::from_utf8_lossy(&output.stdout);
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
    eprintln!(
        "skip: leaks did not emit a leak summary for {}: stderr=\n{}",
        bin.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    None
}

/// Spawning N actors with suspending `#[on(start)]` hooks must NOT leak a coro
/// frame per actor. A regression that parked-but-never-destroyed the lifecycle
/// continuation would show a per-actor slope (`HIGH - LOW` extra nodes).
#[test]
fn suspending_on_start_no_per_actor_coro_frame_leak() {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: leaks(1) is macOS-only");
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
        .prefix("on-start-suspend-leak-")
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(&suspending_on_start_source(LOW_ACTORS), dir.path(), "low");
    let bin_high = compile_to_native(&suspending_on_start_source(HIGH_ACTORS), dir.path(), "high");

    let Some(low_leaks) = measure_leaks(&bin_low) else {
        return;
    };
    let Some(high_leaks) = measure_leaks(&bin_high) else {
        return;
    };

    eprintln!(
        "on_start_suspension: low_actors={LOW_ACTORS} low_leaks={low_leaks} \
         high_actors={HIGH_ACTORS} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "per-actor coro-frame leak SLOPE — low_actors={LOW_ACTORS} low_leaks={low_leaks}, \
         high_actors={HIGH_ACTORS} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a suspending `#[on(start)]` continuation \
         frame is parked but never reclaimed (one `hew_cont_destroy` per spawned actor is \
         missing). Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}
