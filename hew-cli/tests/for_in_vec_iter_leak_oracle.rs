//! `for x in vec` cursor (`VecIter<T>`) scope-exit drop oracles: per-iteration
//! leak slope plus a poisoned-allocator no-use-after-free pin for the
//! place-source reuse shape (issue #1949).
//!
//! Empirical oracle for the for-in cursor leak class. A `for x in <vec>` loop
//! desugars to a synthetic `VecIter<T> { vec, idx }` cursor that holds the
//! source `Vec` handle. Pre-fix neither the source binding nor the cursor freed
//! that handle on loop exit, so every for-in over a fresh per-iteration Vec
//! leaked the handle and its buffer (two nodes per iteration). The fix splits
//! ownership by source shape:
//!   - **place source** (`for x in v`): the source binding keeps its own
//!     scope-exit drop (`derive_local_collection_drop_allowed` exempts the
//!     cursor's `CowShare` ingress) and the cursor BORROWS — it does not drop.
//!   - **rvalue / consumed source** (`for x in make_vec()`, `for x in
//!     v.into_iter()`): the cursor solely owns the handle and frees it via a
//!     per-scope-exit `RecordFieldDrop` on the cursor's `vec` field on every
//!     outer-loop iteration (`emit_scope_vec_iter_drops`).
//!
//! ## Scope: `BitCopy`-element vecs only
//!
//! The fix is restricted to `BitCopy`-element vecs (`Vec<i64>`, `Vec<f64>`,
//! `Vec<bool>`, …). The for-in body extracts each element through
//! `hew_vec_get_*`; for an OWNED element (`string` via a refcount-retaining
//! `hew_vec_get_str`, or a heap-owning element via `hew_vec_get_owned`) that
//! extracted reference can be moved into another owner inside the body
//! (`for w in words { map.insert(w, …) }`). Freeing the source vec then runs the
//! per-element release over slots that alias the value the downstream owner also
//! frees — a double-free. `BitCopy` elements are only ever value-copied, never
//! retained-and-consumed, so freeing the source vec's buffer cannot collide.
//! Owned/string-element for-in source vecs are left undropped (leak, as before
//! the fix — fail-closed, never a double-free); freeing them safely needs the
//! element-escape accounting and is deferred. The
//! `for_in_owned_element_consume_*` pins below prove that path runs CLEAN (no
//! double-free) rather than asserting a flat slope.
//!
//! ## Slope methodology
//!
//! Mirrors `vec_local_drop_leak_oracle.rs`: compile the same shape at a LOW
//! frame count and a HIGH frame count, measure leak NODE counts under `leaks
//! --atExit` with the poisoned-allocator triple, and assert the delta stays
//! within a small constant independent of frames. The pre-fix bug class is
//! PER-FRAME GROWTH (one leaked cursor handle + buffer per loop iteration —
//! the measured pre-fix delta over `50 - 3 = 47` frames was 94 nodes), an
//! order of magnitude above the +5 tolerance.
//!
//! ## No-use-after-free pin
//!
//! The place-source shape (`for x in v { … } …; v[0]`) reuses the source `v`
//! AFTER the loop. The cursor must NOT free the shared handle at loop exit (a
//! use-after-free against the post-loop read AND a double-free against the
//! source binding's own scope-exit drop). The pin runs the shape under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and asserts the
//! exact post-loop checksum: a producer-side double-free or a scribbled value
//! aborts (or corrupts the checksum) before the assertion is read.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); on other platforms they log `skip:` and return. The scribble
//! pin runs on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge path at least twice while
/// staying close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. The pre-fix slope was ~2.0 leak/frame
/// (cursor handle + buffer per iteration), producing `HIGH_FRAMES - LOW_FRAMES
/// = 47`-times-two excess nodes against the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off
/// scheduler/runtime allocations that appear only in the HIGH run while still
/// catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Fixture A — the rvalue-source shape. `make_vec(i)` returns a fresh Vec each
/// iteration; the for-in cursor is its sole owner. Pre-fix: the cursor handle
/// and its buffer leak every iteration. Post-fix: the cursor's scope-exit
/// `RecordFieldDrop` frees the `vec` handle on every outer iteration.
fn rvalue_source_loop_source(frames: usize) -> String {
    format!(
        "fn make_vec(n: i64) -> Vec<i64> {{\n\
         \x20   let v: Vec<i64> = Vec::new();\n\
         \x20   v.push(n);\n\
         \x20   v.push(n * 2);\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       for x in make_vec(i) {{\n\
         \x20           total = total + x;\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture B — the place-source shape. A fresh per-iteration `let v` is
/// CowShare-captured into the cursor; the source binding `v` (not the cursor)
/// owns the handle and frees it on the while back-edge. Both pre-fix leak
/// classes (no source drop, no cursor drop) collapsed to the same per-frame
/// growth; post-fix the source binding's drop fires every iteration.
fn place_source_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<i64> = Vec::new();\n\
         \x20       v.push(i);\n\
         \x20       v.push(i * 2);\n\
         \x20       for x in v {{\n\
         \x20           total = total + x;\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture C — the `Vec<bool>` rvalue source. A second `BitCopy`-element width
/// (1-byte) confirms the cursor's `hew_vec_free` release is element-width
/// agnostic and the slope stays flat.
fn bool_rvalue_source_loop_source(frames: usize) -> String {
    format!(
        "fn make_vec(n: i64) -> Vec<bool> {{\n\
         \x20   let v: Vec<bool> = Vec::new();\n\
         \x20   v.push(true);\n\
         \x20   v.push(false);\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       for b in make_vec(i) {{\n\
         \x20           if b {{\n\
         \x20               total = total + 1;\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Owned-element no-double-free shapes (issue #1949, deferred-safe boundary).
/// A `for w in <Vec<string>>` body that CONSUMES the retained element into a
/// `HashMap` is the shape the `BitCopy` gate exists to protect: freeing the source
/// vec's string elements would double-free the value the map now owns. Both a
/// place source (`for w in words`) and an rvalue source (`for w in mk()`) must
/// run CLEAN under the poisoned allocator (the source vec is intentionally left
/// undropped — a leak, never a double-free). Two distinct keys (all vacant) so
/// the map ends with 2 entries.
const OWNED_PLACE_CONSUME_SOURCE: &str = "\
fn main() {\n\
\x20   let words: Vec<string> = Vec::new();\n\
\x20   words.push(\"a\" + \"1\");\n\
\x20   words.push(\"b\" + \"2\");\n\
\x20   let counts: HashMap<string, i64> = HashMap::new();\n\
\x20   for w in words {\n\
\x20       counts.insert(w, 1);\n\
\x20   }\n\
\x20   print(counts.len());\n\
\x20   print(\"OK\");\n\
}\n";

const OWNED_RVALUE_CONSUME_SOURCE: &str = "\
fn mk() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec::new();\n\
\x20   v.push(\"a\" + \"1\");\n\
\x20   v.push(\"b\" + \"2\");\n\
\x20   return v;\n\
}\n\
\n\
fn main() {\n\
\x20   let counts: HashMap<string, i64> = HashMap::new();\n\
\x20   for w in mk() {\n\
\x20       counts.insert(w, 1);\n\
\x20   }\n\
\x20   print(counts.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// No-use-after-free shape: a place-source `for x in v` followed by a post-loop
/// read of `v`. The cursor must borrow (not free) the shared handle so the
/// post-loop `v[0] + v[1] + v.len()` reads live memory and the source binding's
/// own scope-exit drop is the single free. Checksum: 40 + 2 (loop sum) + 40 + 2
/// (post-loop element reads) + 2 (len) = 86.
const REUSE_SHAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   var sum: i64 = 0;\n\
\x20   for x in v {\n\
\x20       sum = sum + x;\n\
\x20   }\n\
\x20   sum = sum + v[0] + v[1] + v.len();\n\
\x20   print(sum);\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ───

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

/// Build the shape at `low_frames` and `high_frames`, measure leak NODE counts,
/// and assert the delta stays within `SLOPE_TOLERANCE`.
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
        .prefix(&format!("for-in-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration for-in cursor (`VecIter`) \
         handle is not being released (pre-fix slope is ~2.0 leak/frame). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see which stack the leaked block \
         came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Fixture A: per-iteration `for x in make_vec(i)` cursors must not leak. The
/// cursor is the sole owner of each fresh Vec and must free it on every outer
/// iteration. Reverting the `emit_scope_vec_iter_drops` registration fails this
/// by ~94 nodes.
#[test]
fn for_in_rvalue_source_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_rvalue_source",
        rvalue_source_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture B: per-iteration `for x in v` over a fresh place-bound Vec must not
/// leak. The source binding keeps its scope-exit drop while the cursor borrows.
/// Reverting the `derive_local_collection_drop_allowed` cursor-ingress
/// exemption (so the source loses its drop again) fails this by ~94 nodes.
#[test]
fn for_in_place_source_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_place_source",
        place_source_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture C: per-iteration `for b in make_vec(i)` over a `Vec<bool>` must not
/// leak — a second `BitCopy` element width confirms the cursor release is
/// element-width agnostic.
#[test]
fn for_in_bool_rvalue_source_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_bool_rvalue_source",
        bool_rvalue_source_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Run `source` under the poisoned-allocator triple and assert it prints
/// `expected` untouched. Shared by the no-double-free pins.
fn assert_runs_clean(shape_name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("for-in-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|e| panic!("run {shape_name} binary: {e}"));

    assert!(
        output.status.success(),
        "{shape_name} must run clean under the poisoned allocator — a crash here \
         indicates a double-free of a shared element/handle;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{shape_name} must print `{expected}` untouched — a scribbled value \
         indicates a freed-but-still-read element;\n{}",
        describe_output(&output)
    );
}

/// No-use-after-free pin: a place-source `for x in v` whose source is read
/// AFTER the loop runs to completion under the poisoned-allocator triple and
/// prints the exact checksum (86) then `OK`. A cursor-side free of the shared
/// handle would be a use-after-free against the post-loop read and a
/// double-free against the source binding's own scope-exit drop — under
/// `MallocScribble`/`MallocGuardEdges` that aborts (or scribbles the values)
/// before the checksum is printed.
#[test]
fn for_in_place_source_reuse_runs_clean_under_malloc_scribble() {
    assert_runs_clean("reuse_shape", REUSE_SHAPE_SOURCE, "86OK");
}

/// No-double-free pin: a place-source `for w in words` over a `Vec<string>`
/// whose retained element is CONSUMED into a `HashMap`. The source vec is the
/// shape the `BitCopy` gate protects — it is intentionally left undropped so the
/// per-element release never collides with the value the map now owns. Must run
/// clean and print `2OK`.
#[test]
fn for_in_owned_element_consume_place_runs_clean_under_malloc_scribble() {
    assert_runs_clean("owned_place_consume", OWNED_PLACE_CONSUME_SOURCE, "2OK");
}

/// No-double-free pin: the rvalue analogue (`for w in mk()` over a
/// `Vec<string>` with the element consumed into a `HashMap`). The cursor is the
/// sole handle owner but its owned elements escape into the map, so the cursor
/// is NOT registered for the per-scope-exit release. Must run clean and print
/// `2OK`.
#[test]
fn for_in_owned_element_consume_rvalue_runs_clean_under_malloc_scribble() {
    assert_runs_clean("owned_rvalue_consume", OWNED_RVALUE_CONSUME_SOURCE, "2OK");
}
