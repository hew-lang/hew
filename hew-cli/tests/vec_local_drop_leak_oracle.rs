//! Plain `Vec<T>` local scope-exit drop oracles: per-iteration leak slope
//! plus poisoned-allocator no-double-free pins for the escape shapes.
//!
//! Empirical oracle for the plain-Vec leak class: a local `Vec<T>` whose
//! element is a `BitCopy` scalar or `string` had no drop class at all (a
//! plain Vec is `ValueClass::CowValue` but `cow_value_leaf_drop_symbol`
//! deliberately excludes containers), so every plain Vec local leaked its
//! backing buffer — and, for `Vec<string>`, every element — at scope exit.
//! The fix is the `plain_vec_drop_allowed` fail-closed admission authority
//! in `hew-mir/src/lower.rs` (riding `derive_local_collection_drop_allowed`
//! with a default-deny element filter) plus the
//! `DropKind::CowHeap { "hew_vec_free" }` interception arm in
//! `build_lifo_drops`.
//!
//! ## Slope methodology
//!
//! Mirrors `bytes_drop_leak_oracle.rs` / `recv_loop_leak_oracle.rs`: compile
//! the same shape at a LOW frame count and a HIGH frame count, measure leak
//! NODE counts under `leaks --atExit` with the poisoned-allocator triple,
//! and assert the delta stays within a small constant independent of frames.
//! The pre-fix bug class is PER-FRAME GROWTH (one leaked vec per loop
//! iteration — two nodes for `Vec<i64>`: handle + buffer; more for
//! `Vec<string>`), which over a `50 - 3 = 47`-frame delta lands an order of
//! magnitude above the +5 tolerance. Absolute counts are deliberately not
//! asserted — runtime/scheduler one-off allocations jitter by ±1 node.
//!
//! ## No-double-free pins
//!
//! The escape shapes the prover must EXCLUDE (a returned vec, a vec
//! consumed by a by-value call) run to completion under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and must exit
//! with the expected value: a producer-side drop of a handle the new owner
//! also releases would crash (or corrupt the result) under the poisoned
//! allocator before any assertion is read.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); on other platforms they log `skip:` and return. The
//! scribble pins run on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge path at least twice while
/// staying close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A slope of >= 1.0 leak/frame (the
/// pre-fix measurement for the plain-Vec leak class) produces
/// `HIGH_FRAMES - LOW_FRAMES = 47`-plus excess nodes against the tolerance
/// of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off
/// scheduler/runtime allocations that appear only in the HIGH run while
/// still catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Fixture A — the probe's primary leaking shape. A `while` loop creates one
/// fresh `Vec<i64>` per iteration, pushes into it, and reads it back
/// (receiver-borrowing ops that must NOT suppress the drop), then lets it go
/// out of scope on the back-edge. Pre-fix: the handle and its buffer leak
/// every iteration. Post-fix: the per-iteration local is proven sole-owner
/// and released on every back-edge plus the final fall-through.
fn i64_vec_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<i64> = Vec::new();\n\
         \x20       v.push(i);\n\
         \x20       v.push(i * 2);\n\
         \x20       total = total + v.len() + v[0];\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture B — the string-element variant. The runtime's `ElemKind::String`
/// walk inside `hew_vec_free` must release the element copies along with the
/// buffer, so the slope stays flat even though every iteration allocates
/// fresh element strings inside the vec.
fn string_vec_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<string> = Vec::new();\n\
         \x20       v.push(\"per-iteration-element-one\");\n\
         \x20       v.push(\"per-iteration-element-two\");\n\
         \x20       total = total + v.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Escape shapes — the no-double-free side. `make()` returns its vec (the
/// caller owns the release), and `total()` consumes one by value (the callee
/// owns the release). A producer-side scope-exit drop on either handle would
/// free what the new owner also frees; under the poisoned-allocator triple
/// that crashes before main can return its checksum.
const ESCAPE_SHAPES_SOURCE: &str = "\
fn make() -> Vec<i64> {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   return v;\n\
}\n\
\n\
fn total(xs: Vec<i64>) -> i64 {\n\
\x20   xs[0] + xs[1]\n\
}\n\
\n\
fn main() {\n\
\x20   let made = make();\n\
\x20   let a = made[0] + made[1];\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(20);\n\
\x20   v.push(1);\n\
\x20   let b = total(v);\n\
\x20   print(a + b);\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as bytes_drop_leak_oracle) ──────

/// Compile `source` to a native binary via `hew compile --emit-dir` and
/// return the binary path.
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
/// return `Some(leak_count)` when `leaks` produced a usable report.
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

/// Build the shape at `low_frames` and `high_frames`, measure leak NODE
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
        .prefix(&format!("vec-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration Vec allocation is not \
         being released (pre-fix slope is >= 1.0 leak/frame for the plain-Vec leak class). \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see which stack the \
         leaked block came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Fixture A: per-iteration `Vec<i64>` locals must not leak. Pre-fix slope
/// is >= 1.0 leak/frame (no scope-exit drop for plain Vec); post-fix the
/// per-iteration handle and buffer are released on every back-edge.
/// Reverting either the `plain_vec_drop_allowed` admission or the
/// `CowHeap { hew_vec_free }` interception arm fails this by ~47+ nodes.
#[test]
fn vec_i64_local_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_i64_local_loop",
        i64_vec_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture B: per-iteration `Vec<string>` locals must not leak — the single
/// `hew_vec_free` releases the element copies through the runtime's
/// `ElemKind::String` walk along with the buffer and handle.
#[test]
fn vec_string_local_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_string_local_loop",
        string_vec_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// No-double-free pin: the returned-vec and consumed-by-value shapes run to
/// completion under the poisoned-allocator triple and print the expected
/// checksum (42 + 21 = 63). A producer-side drop of either escaped handle
/// would free memory the new owner also frees — under
/// `MallocScribble`/`MallocGuardEdges` that aborts (or scribbles the
/// values) before the checksum is printed.
#[test]
fn vec_escape_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-leak-escape-shapes-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(ESCAPE_SHAPES_SOURCE, dir.path(), "escape_shapes");

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run escape-shapes binary");

    assert!(
        output.status.success(),
        "escape shapes must run clean under the poisoned allocator — a \
         crash here indicates a producer-side drop of a moved-out vec \
         (double free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "63OK",
        "escape shapes must print the 42 + 21 checksum untouched — a \
         scribbled value indicates the producer freed a handle the new \
         owner still reads;\n{}",
        describe_output(&output)
    );
}
