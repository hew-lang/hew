//! Owned-element `Vec<T>::clone()` receiver-borrow leak oracle.
//!
//! ## The leak this pins
//!
//! `v.clone()` on an owned-element `Vec<T>` (an element that owns heap — here
//! `Vec<Vec<string>>`) lowers to `hew_vec_clone_owned`, which deep-clones each
//! element through the descriptor `clone_fn` into a FRESH, independently owned
//! Vec. The original receiver `v` is only BORROWED: it keeps its own buffer and
//! must keep its scope-exit `hew_vec_free_owned`.
//!
//! The MIR drop elaborator decides which owned Vecs earn that release with the
//! fail-closed escape-scan `derive_local_collection_drop_allowed`
//! (`hew-mir/src/lower.rs`), which excludes any candidate read by an
//! unclassified runtime call as an owning escape. `hew_vec_clone_owned` was
//! ABSENT from the receiver-borrow allow-list (`is_vec_receiver_borrow_symbol`)
//! even though its siblings `hew_vec_clone` / `hew_vec_clone_layout` were
//! present, so a local owned-element Vec used as the receiver of `.clone()` was
//! treated as escaped, dropped from `owned_vec_drop_allowed`, and never freed.
//! The clone RESULT was released, but the ORIGINAL handle (its buffer, every
//! inner collection handle/buffer, and every element string) leaked on each
//! scope exit. The fix adds `hew_vec_clone_owned` to the receiver-borrow
//! classifier (it is a borrow, not an ownership hand-off).
//!
//! ## Slope methodology
//!
//! Mirrors `nested_vec_push_leak_oracle.rs` / `vec_local_drop_leak_oracle.rs`:
//! compile the same clone-in-a-loop shape at a LOW and a HIGH frame count,
//! measure leak NODE counts under `leaks --atExit` with the poisoned-allocator
//! triple, and assert the delta stays within a small constant independent of
//! frames. The pre-fix bug class is PER-FRAME GROWTH — the original receiver's
//! whole tree leaks every iteration — which over the `50 - 3 = 47`-frame delta
//! lands an order of magnitude above the +5 tolerance. Absolute counts are
//! deliberately not asserted; runtime/scheduler one-off allocations jitter by
//! a node or two.
//!
//! ## Deep-clone independence + no-double-free pin
//!
//! A second fixture mutates the clone (`w.push(..)`) after `let w = v.clone()`
//! and asserts the original `v` is unchanged (length + inner row intact) and
//! the program exits cleanly with the exact reconstruction under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`. If the clone
//! aliased the receiver's buffer instead of deep-copying, the independent
//! release of two handles over one allocation would double-free (crash under
//! the poisoned allocator) or the mutation would corrupt `v`.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! on other platforms it logs `skip:` and returns. The scribble pin runs on any
//! unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A per-frame leak of the original
/// receiver tree (outer handle + buffer + inner handle + buffer + element
/// strings) lands well above the tolerance over the 47-frame delta.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off
/// scheduler/runtime allocations that appear only in the HIGH run while still
/// catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Per-frame clone-and-drop shape for the leak slope: each iteration builds a
/// fresh owned-element `Vec<Vec<string>>`, clones it into `w`, reads BOTH, and
/// lets both go out of scope on the back-edge. Post-fix the original receiver
/// `v` keeps its scope-exit `hew_vec_free_owned` (it is only borrowed by
/// `clone`), so both trees are released each frame and the leak count is flat.
/// Pre-fix `v` was excluded from the drop allow-set and its whole tree leaked
/// every iteration.
fn owned_vec_clone_drop_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<Vec<string>> = [];\n\
         \x20       let row: Vec<string> = [];\n\
         \x20       row.push(\"owned-clone-leak-oracle-cell-one\");\n\
         \x20       row.push(\"owned-clone-leak-oracle-cell-two\");\n\
         \x20       v.push(row);\n\
         \x20       let w = v.clone();\n\
         \x20       total = total + v.len() + w.len() + v.get(0).len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Deep-clone independence + no-double-free pin: `w = v.clone()` then mutate `w`
/// only. `v` must stay one row of two cells; `w` must have the extra row. Exact
/// reconstruction proves the clone deep-copied (no alias of `v`'s buffer) and
/// both handles release independently under the poisoned allocator.
const OWNED_VEC_CLONE_INDEPENDENT_SOURCE: &str = "\
fn main() {\n\
\x20   let v: Vec<Vec<string>> = [];\n\
\x20   let row: Vec<string> = [];\n\
\x20   row.push(\"orig-aaa\");\n\
\x20   row.push(\"orig-bbb\");\n\
\x20   v.push(row);\n\
\x20   let w = v.clone();\n\
\x20   let extra: Vec<string> = [];\n\
\x20   extra.push(\"clone-ccc\");\n\
\x20   w.push(extra);\n\
\x20   print(f\"v{v.len()}r{v.get(0).len()}c;w{w.len()}r;\");\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact output: `v` is untouched by `w`'s push (deep clone), so one
/// row of two cells; `w` gained a second row.
const OWNED_VEC_CLONE_INDEPENDENT_EXPECTED: &str = "v1r2c;w2r;OK";

// ── leak measurement plumbing (same shape as nested_vec_push_leak_oracle) ───

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path.
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and return
/// `Some(leak_count)` when `leaks` produced a usable report.
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

/// Build `shape_name` at `low_frames` and `high_frames`, measure leak NODE
/// counts, and assert the delta stays within `SLOPE_TOLERANCE`.
fn assert_frame_slope_below_tolerance(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
) {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
        return;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-owned-clone-leak-{shape_name}-"))
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
         high_frames={high_frames} high_leaks={high_leaks} \
         tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates the original owned Vec receiver of `.clone()` \
         is not being released — `hew_vec_clone_owned` must be classified as a receiver borrow so \
         the receiver keeps its scope-exit `hew_vec_free_owned`. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-owned-clone-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|error| panic!("run {name} binary: {error}"));

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates the clone \
         aliased the receiver's buffer and the two independent releases double-freed it;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must reconstruct the original verbatim — a differing/empty cell or wrong length \
         indicates `w = v.clone()` aliased `v` instead of deep-copying;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// THE clone-leak regression: the original receiver of `.clone()` on an
/// owned-element `Vec<Vec<string>>` must keep its scope-exit release, so the
/// per-frame leak slope is flat. Reverting the `hew_vec_clone_owned` classifier
/// entry re-excludes the receiver and grows the leak ~6 nodes/frame, ~282 NODES
/// over the 47-frame delta — far above the +5 tolerance.
#[test]
fn owned_vec_clone_receiver_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "owned_vec_clone_drop_loop",
        owned_vec_clone_drop_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Deep-clone independence + no-double-free: mutating the clone leaves the
/// original intact and both handles release once under the poisoned allocator.
#[test]
fn owned_vec_clone_independent_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "owned_vec_clone_independent",
        OWNED_VEC_CLONE_INDEPENDENT_SOURCE,
        OWNED_VEC_CLONE_INDEPENDENT_EXPECTED,
    );
}
