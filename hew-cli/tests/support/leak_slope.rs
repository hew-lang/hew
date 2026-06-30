//! Shared leak-oracle slope harness — the single authority for the
//! per-iteration leak-slope methodology the `*_leak_oracle.rs` integration
//! tests build on.
//!
//! ## Why slope, not single-shot exact-zero
//!
//! A single `leaks --atExit` measurement is nondeterministic: transient
//! allocations, attach races, and constant root-set baseline noise let an exact
//! `0 leaks for 0 total leaked bytes` assertion pass (or fail) spuriously, so an
//! exact-zero gate cannot be trusted to catch a real per-iteration leak. The
//! trustworthy signal is the PER-ITERATION SLOPE: compile the SAME shape at a
//! LOW and a HIGH iteration count, measure leak NODE counts under the
//! poisoned-allocator triple, and assert the delta stays within a small
//! constant (`high_leaks <= low_leaks + tolerance`). The delta cancels the
//! constant baseline noise; a genuine per-iteration leak shows as a positive
//! slope that scales with the iteration count.
//!
//! This module consolidates the previously per-file copies of `compile_to_native`
//! / `measure_leaks` / `assert_frame_slope_below_tolerance` into one place so the
//! slope logic cannot drift between oracles. The poisoned-allocator scribble
//! primitive ([`run_under_malloc_scribble`]) is shared for the double-free /
//! use-after-free correctness pins that accompany the slope assertions.

#![allow(
    dead_code,
    reason = "shared leak-oracle helpers are not used by every test target"
)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use super::{describe_output, hew_binary, repo_root, require_codegen};

/// Low iteration count: exercises the per-iteration path enough times to leave
/// the constant-overhead floor while staying cheap to compile and scan.
pub const LOW_FRAMES: usize = 3;

/// High iteration count for the slope check. A per-iteration leak of even one
/// node grows by `HIGH_FRAMES - LOW_FRAMES = 47` nodes here — an order of
/// magnitude above the tolerance.
pub const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-NODE delta between the HIGH and LOW probes. Absorbs
/// the one-off scheduler/runtime allocations that appear only in the HIGH run
/// while still catching a slope of ~0.1 leaks/iteration.
pub const SLOPE_TOLERANCE: usize = 5;

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path. Panics with the captured compiler output on failure.
pub fn compile_to_native(source: &str, dir: &Path, name: &str) -> PathBuf {
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and return
/// `Some(leak_node_count)` when `leaks` produced a usable
/// `Process <pid>: N leak(s) for B total leaked bytes.` summary.
///
/// Returns `None` (with a `skip:` notice on stderr) when `leaks(1)` declines to
/// attach or does not emit the expected summary line — the caller treats that
/// as a graceful skip rather than a failure.
pub fn measure_leaks(bin: &Path) -> Option<usize> {
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
    let mut parsed: Option<usize> = None;
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
                        parsed = Some(n);
                        break;
                    }
                }
            }
        }
    }
    if parsed.is_none() {
        eprintln!(
            "skip: leaks did not emit a `Process <pid>: N leak(s) for B total leaked bytes.` \
             summary for {}: stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    parsed
}

/// macOS + `leaks(1)` availability guard shared by every leak probe. Logs a
/// `skip:` notice and returns `false` when the slope measurement cannot run on
/// this host (non-macOS, or `leaks` missing from `PATH`).
pub fn leaks_supported(shape_name: &str) -> bool {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return false;
    }
    let avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !avail {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
    }
    avail
}

/// Build `shape_name` at [`LOW_FRAMES`] and [`HIGH_FRAMES`], measure leak NODE
/// counts, and assert the delta stays within [`SLOPE_TOLERANCE`]. The canonical
/// entry point — see [`assert_frame_slope_below_tolerance_with`] for an explicit
/// iteration/tolerance override.
pub fn assert_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
    assert_frame_slope_below_tolerance_with(
        shape_name,
        source_fn,
        LOW_FRAMES,
        HIGH_FRAMES,
        SLOPE_TOLERANCE,
    );
}

/// Explicit-parameter variant of [`assert_frame_slope_below_tolerance`].
///
/// Builds the shape at `low_frames` and `high_frames`, measures leak NODE counts
/// under the poisoned-allocator triple, and asserts
/// `high_leaks <= low_leaks + tolerance`. A positive slope above the tolerance
/// means a per-iteration allocation is not being released; the failure message
/// names the excess and the re-run command. Logs `skip:` and returns when
/// `leaks(1)` is unavailable.
pub fn assert_frame_slope_below_tolerance_with(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
    tolerance: usize,
) {
    if !leaks_supported(shape_name) {
        return;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("leak-slope-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(low_frames),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(high_frames),
        dir.path(),
        &format!("{shape_name}_high"),
    );

    let Some(low_leaks) = measure_leaks(&bin_low) else {
        return;
    };
    let Some(high_leaks) = measure_leaks(&bin_high) else {
        return;
    };

    eprintln!(
        "{shape_name}: low_frames={low_frames} low_leaks={low_leaks} \
         high_frames={high_frames} high_leaks={high_leaks} tolerance={tolerance}"
    );
    assert!(
        high_leaks <= low_leaks + tolerance,
        "{shape_name}: per-iteration leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {tolerance} indicates a per-iteration allocation is not being released. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked allocation \
         stack.",
        high_leaks.saturating_sub(low_leaks + tolerance),
        bin_high.display()
    );
}

/// Run `bin` under the poisoned-allocator triple (`MallocScribble` +
/// `MallocPreScribble` + `MallocGuardEdges`) and return the captured output.
/// Shared primitive for the no-double-free / use-after-free correctness pins:
/// an over-eager drop frees memory the program still owns, which the scribbled
/// allocator turns into an abort (double-free) or a poisoned read.
pub fn run_under_malloc_scribble(bin: &Path) -> Output {
    Command::new(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|error| panic!("run {} under poisoned allocator: {error}", bin.display()))
}
