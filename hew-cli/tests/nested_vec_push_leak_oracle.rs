//! Nested-collection `Vec` push ownership oracles (hew-lang/hew#1722).
//!
//! `Vec<Vec<T>>`, `Vec<HashMap<..>>`, and `Vec<HashSet<..>>` used to push a
//! SHALLOW element handle (`hew_vec_push_ptr`): the outer `Vec` stored the
//! caller's inner-collection pointer without cloning. A reused or cleared
//! accumulator — the canonical CSV row-builder pattern — then aliased every
//! already-pushed row to the SAME backing store, so a later `clear()` (or the
//! source's own scope-exit free) emptied/freed rows still reachable through the
//! outer `Vec`. Reads returned empty/garbage cells (silent corruption) and the
//! outer `Vec`'s free double-freed the inner backing the source already freed.
//!
//! The fix routes these element types through the W5.016 owned-descriptor ABI
//! (`hew_vec_push_owned` + a synthesized per-element clone/drop thunk) with
//! COPY-IN semantics, so each pushed element is deep-cloned and the outer `Vec`
//! owns its own storage — exactly like `Vec<OwnedRecord>`.
//!
//! ## What each oracle pins
//!
//! - **Exact-contents under the poisoned-allocator triple** (any unix): the
//!   primary #1722 regression. The reuse-then-`clear()` pattern is run under
//!   `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and must print the
//!   EXACT reconstructed cell contents. Pre-fix this either aborts on the
//!   double-free or prints scribbled/empty cells (a use-after-free read); the
//!   string equality catches the corruption before any tolerance is involved.
//!
//! - **Per-frame leak slope** (macOS-only, via `leaks(1)`): a forward guard on
//!   the synthesized `drop_fn`. Post-fix, dropping a `Vec<Vec<string>>` runs each
//!   element's drop thunk and frees all inner storage, so the leak-node count
//!   is independent of the frame count. A regression that makes the element
//!   drop thunk a no-op (leak instead of double-free) would show a per-frame
//!   slope this catches; the same methodology as the sibling
//!   `vec_local_drop_leak_oracle.rs`.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! elsewhere it logs `skip:` and returns. The scribble correctness pins run on
//! any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A per-frame leak of the inner vecs
/// (handle + buffer + element strings) lands well above the tolerance over the
/// `HIGH_FRAMES - LOW_FRAMES = 47`-frame delta.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off
/// scheduler/runtime allocations that appear only in the HIGH run while still
/// catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// The #1722 reproduction: a loop-carried `cur_row: Vec<string>` accumulator is
/// filled, flushed into `rows: Vec<Vec<string>>` with a direct `rows.push`,
/// then `clear()`ed and reused. Post-fix each flush deep-copies, so every row
/// keeps its own two cells; pre-fix every row aliased `cur_row`'s backing and
/// the `clear()` emptied them all (and the outer free double-freed).
///
/// `main` reconstructs the exact grid into a single string so a corrupted cell
/// (empty or scribbled) changes the output verbatim.
const VEC_VEC_STRING_REUSE_SOURCE: &str = "\
fn build_rows(n: i64) -> Vec<Vec<string>> {\n\
\x20   let rows: Vec<Vec<string>> = [];\n\
\x20   let cur_row: Vec<string> = [];\n\
\x20   var i: i64 = 0;\n\
\x20   while i < n {\n\
\x20       cur_row.push(f\"r{i}c0\");\n\
\x20       cur_row.push(f\"r{i}c1\");\n\
\x20       rows.push(cur_row);\n\
\x20       cur_row.clear();\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   rows\n\
}\n\
\n\
fn main() {\n\
\x20   let rows = build_rows(3);\n\
\x20   var out: string = \"\";\n\
\x20   for r in 0 .. rows.len() {\n\
\x20       let row = rows[r];\n\
\x20       for c in 0 .. row.len() {\n\
\x20           out = out + row[c] + \"|\";\n\
\x20       }\n\
\x20       out = out + \";\";\n\
\x20   }\n\
\x20   print(out);\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact grid for `VEC_VEC_STRING_REUSE_SOURCE`. Any aliasing/UAF
/// corruption changes at least one cell, so the equality fails verbatim.
const VEC_VEC_STRING_REUSE_EXPECTED: &str = "r0c0|r0c1|;r1c0|r1c1|;r2c0|r2c1|;OK";

/// `Vec<HashMap<string,i64>>` reuse variant: each map carries one entry whose
/// value is the iteration index times seven, pushed into `rows`, then the map
/// is re-created fresh each turn. Post-fix every stored map round-trips its own
/// value; pre-fix they aliased a single map handle.
const VEC_HASHMAP_REUSE_SOURCE: &str = "\
fn main() {\n\
\x20   let rows: Vec<HashMap<string, i64>> = [];\n\
\x20   let key: string = \"k\";\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 3 {\n\
\x20       let m: HashMap<string, i64> = HashMap::new();\n\
\x20       m.insert(clone key, i * 7);\n\
\x20       rows.push(m);\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   var out: string = \"\";\n\
\x20   for r in 0 .. rows.len() {\n\
\x20       let row = rows[r];\n\
\x20       match row.get(key) {\n\
\x20           Some(v) => { out = out + f\"{v}\" + \"|\"; },\n\
\x20           None => { out = out + \"MISSING|\"; },\n\
\x20       }\n\
\x20   }\n\
\x20   print(out);\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact output for `VEC_HASHMAP_REUSE_SOURCE`: values 0, 7, 14.
const VEC_HASHMAP_REUSE_EXPECTED: &str = "0|7|14|OK";

/// Per-frame build-and-drop shape for the leak slope: each iteration builds a
/// fresh `Vec<Vec<string>>` (two rows from a reused accumulator), reads it, and
/// lets it go out of scope on the back-edge. Post-fix the outer drop runs each
/// element's drop thunk and frees the inner handles, buffers, and element
/// strings, so the leak count is flat across frame counts.
fn vec_vec_string_drop_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let rows: Vec<Vec<string>> = [];\n\
         \x20       let cur: Vec<string> = [];\n\
         \x20       cur.push(\"per-iteration-row-element-one\");\n\
         \x20       cur.push(\"per-iteration-row-element-two\");\n\
         \x20       rows.push(cur);\n\
         \x20       cur.clear();\n\
         \x20       cur.push(\"per-iteration-row-element-three\");\n\
         \x20       rows.push(cur);\n\
         \x20       total = total + rows.len() + rows[0].len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ────

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
        .prefix(&format!("nested-vec-push-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration nested-Vec element is not \
         being released — the synthesized element drop thunk must free each inner collection. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout. Shared by the correctness pins.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("nested-vec-push-{name}-"))
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
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free of an inner collection the source accumulator also freed (#1722 pre-fix \
         aliasing);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must reconstruct every cell verbatim — a differing/empty cell indicates the \
         pushed element aliased the reused source backing (use-after-free read) instead of \
         being copied in;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// PRIMARY #1722 regression: the reuse-then-`clear()` row builder must keep
/// every stored row's exact cells under the poisoned allocator. Reverting any
/// of the five congruent classifiers (so the push falls back to
/// `hew_vec_push_ptr`) fails this — pre-fix the binary aborts on the
/// double-free or prints empty/scribbled cells.
#[test]
fn vec_vec_string_reuse_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_vec_string_reuse",
        VEC_VEC_STRING_REUSE_SOURCE,
        VEC_VEC_STRING_REUSE_EXPECTED,
    );
}

/// `Vec<HashMap<..>>` breadth pin: each stored map must round-trip its own
/// entry value under the poisoned allocator. Guards the `HashMap` arm of the
/// collection clone/drop thunk selection.
#[test]
fn vec_hashmap_reuse_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_hashmap_reuse",
        VEC_HASHMAP_REUSE_SOURCE,
        VEC_HASHMAP_REUSE_EXPECTED,
    );
}

/// Forward leak guard: building and dropping a `Vec<Vec<string>>` per frame
/// must not grow the leak-node count with the frame count. Post-fix the outer
/// drop runs the synthesized element drop thunk, releasing each inner handle,
/// buffer, and element string. A no-op drop thunk would show a per-frame slope.
#[test]
fn vec_vec_string_outer_drop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_vec_string_drop_loop",
        vec_vec_string_drop_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}
