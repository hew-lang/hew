//! Owned-Vec element-store TEMP leak oracles: a fresh, unbound aggregate rvalue
//! used as a `Vec::push` / `Vec::set` element source must not leak its heap.
//!
//! ## The bug this pins
//!
//! `hew_vec_push_owned` / `hew_vec_set_owned` are COPY-IN: they deep-clone the
//! element into the slot. An UNBOUND materialised rvalue element
//! (`v.push(Holder { .. })`, `v.set(i, Holder { .. })`) has no binding and no
//! scope-exit drop to balance that clone, so the source temp's owned heap leaks
//! once per store. The fix routes such a fresh materialised owner to the MOVE-in
//! siblings `hew_vec_push_owned_move` / `hew_vec_set_owned_move`, which
//! byte-transfer the element's heap into the slot without a clone (the source
//! temp is then dead). `push` was already routed; this oracle also pins `set`,
//! whose move sibling this lane added.
//!
//! ## Why a DEEP-OWNED element (not a string field)
//!
//! Measured on the fix base: a record with a `string` / `Option<string>` field
//! does NOT leak under copy-in — strings are refcount-shared, `clone_fn` retains
//! the same buffer, and the dead source's share is reclaimed when the vec is
//! freed. Only a DEEP-OWNED element (here `Holder { items: Vec<string> }`, whose
//! `clone_fn` deep-copies into a DISTINCT buffer) exposes the leak: the vec gets
//! a fresh copy and the source temp's original heap is never freed (~4 nodes per
//! store: the inner Vec handle + buffer + two element strings). A
//! `Vec<record{string}>` fixture would pass even with the bug — so this oracle
//! deliberately uses a nested-collection element.
//!
//! ## Slope methodology
//!
//! Mirrors `vec_local_drop_leak_oracle.rs`: compile the same shape at LOW and
//! HIGH frame counts, measure leak NODE counts under `leaks --atExit` + the
//! poisoned-allocator triple, and assert the delta stays within a small constant
//! independent of frames. Pre-fix, `set`'s slope is ~4 leaks/frame (≈188 excess
//! nodes over the 47-frame delta), an order of magnitude past the +5 tolerance.
//!
//! ## No-double-free pins
//!
//! The COPY shapes the router must NOT relax — a bound local pushed twice
//! (shared → both must clone), and a bound local read after a push — run to
//! completion under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and
//! must exit with the expected value. Routing a `BindingRef` source to move
//! would transfer heap a live binding (or the second push) still reads, so the
//! poisoned allocator would crash or scribble before the checksum prints.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); elsewhere they log `skip:` and return. The scribble pins run on
//! any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A pre-fix slope of ~4 leaks/frame for
/// the deep-owned element leak class produces `HIGH_FRAMES - LOW_FRAMES = 47`
/// frames × 4 ≈ 188 excess nodes against the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off runtime/scheduler
/// allocations that appear only in the HIGH run while still catching a slope of
/// ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

/// Shared prelude: a deep-owned element type and a fresh-Vec producer. `mkItems`
/// returns a FRESH `Vec<string>` (rc=1, its own buffer) so each element the
/// store ingests owns distinct heap the leak oracle can count.
const PRELUDE: &str = "\
record Holder { items: Vec<string> }\n\
fn mkItems(i: i64) -> Vec<string> {\n\
\x20   var xs: Vec<string> = Vec::new();\n\
\x20   xs.push(\"deep-elem-a\");\n\
\x20   xs.push(\"deep-elem-b\");\n\
\x20   return xs;\n\
}\n";

// ── fixtures ──────────────────────────────────────────────────────────────

/// P1 — unbound `Holder { .. }` PUSH per iteration. The element is a fresh
/// materialised owner routed to `hew_vec_push_owned_move`; the per-iteration
/// source temp transfers its heap into the slot and leaks nothing. Reverting the
/// push routing regresses this by ~4 nodes/frame.
fn push_temp_source(frames: usize) -> String {
    format!(
        "{PRELUDE}\
         fn main() -> i64 {{\n\
         \x20   var v: Vec<Holder> = Vec::new();\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       v.push(Holder {{ items: mkItems(i) }});\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   v.len()\n\
         }}\n"
    )
}

/// P2 — unbound `Holder { .. }` SET per iteration over a one-element seed. The
/// element is routed to `hew_vec_set_owned_move` (the sibling this lane added);
/// `set`'s overwrite-drop frees the replaced element and the move transfers the
/// new element's heap, so the source temp leaks nothing. Pre-fix
/// (`hew_vec_set_owned`, COPY-IN) this leaks ~4 nodes/frame.
fn set_temp_source(frames: usize) -> String {
    format!(
        "{PRELUDE}\
         fn main() -> i64 {{\n\
         \x20   var v: Vec<Holder> = Vec::new();\n\
         \x20   v.push(Holder {{ items: mkItems(0) }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       v.set(0, Holder {{ items: mkItems(i) }});\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   v.len()\n\
         }}\n"
    )
}

/// No-double-free pins — the COPY shapes the router must NOT relax. `sharedTwice`
/// pushes ONE bound local twice (shared → both pushes MUST clone; the local's
/// scope-exit drop releases the original once). `readAfter` reads the local
/// after a push (MUST clone; a move would leave the read observing transferred
/// bytes). A wrongly-routed move here double-frees or reads freed heap; under the
/// poisoned-allocator triple that aborts (or scribbles) before the checksum
/// prints. Expected stdout: `sharedTwice` pushes 2×3 = 6 elements → 6;
/// `readAfter` reads `h.items.len()` (== 2) across 3 iterations → 6; then `OK`
/// (so `66OK`).
const NO_DOUBLE_FREE_SOURCE: &str = "\
record Holder { items: Vec<string> }\n\
fn mkItems(i: i64) -> Vec<string> {\n\
\x20   var xs: Vec<string> = Vec::new();\n\
\x20   xs.push(\"deep-elem-a\");\n\
\x20   xs.push(\"deep-elem-b\");\n\
\x20   return xs;\n\
}\n\
fn sharedTwice() -> i64 {\n\
\x20   var v: Vec<Holder> = Vec::new();\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 3 {\n\
\x20       let h = Holder { items: mkItems(i) };\n\
\x20       v.push(h);\n\
\x20       v.push(h);\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   v.len()\n\
}\n\
fn readAfter() -> i64 {\n\
\x20   var v: Vec<Holder> = Vec::new();\n\
\x20   var total: i64 = 0;\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 3 {\n\
\x20       let h = Holder { items: mkItems(i) };\n\
\x20       v.push(h);\n\
\x20       total = total + h.items.len();\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   total\n\
}\n\
fn main() {\n\
\x20   print(sharedTwice());\n\
\x20   print(readAfter());\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ──

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
        .prefix(&format!("vec-elem-store-leak-{shape_name}-"))
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
         high_frames={high_frames} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration owned-element temp is not \
         being released — the unbound aggregate element source is deep-cloned COPY-IN and its \
         heap leaks. Route the fresh materialised element to the MOVE-in sibling \
         (`hew_vec_push_owned_move` / `hew_vec_set_owned_move`). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked block's stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// P1: an unbound `Holder { .. }` PUSH per iteration must not leak — the fresh
/// materialised element is moved into the slot, not deep-cloned. Reverting the
/// `expr_is_materialized_owner` push routing fails this by ~188 nodes.
#[test]
fn vec_push_owned_temp_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("push_temp", push_temp_source, LOW_FRAMES, HIGH_FRAMES);
}

/// P2: an unbound `Holder { .. }` SET per iteration must not leak — the fresh
/// materialised element is routed to `hew_vec_set_owned_move`. Reverting the set
/// routing (or the runtime move sibling) fails this by ~188 nodes: this is the
/// hole the lane closed, and the fixture whose absence let it ship.
#[test]
fn vec_set_owned_temp_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("set_temp", set_temp_source, LOW_FRAMES, HIGH_FRAMES);
}

/// No-double-free pin: the COPY shapes the router must NOT relax run clean under
/// the poisoned-allocator triple and print the expected checksums. A bound local
/// pushed twice, or read after a push, stays COPY-IN; routing either to move
/// double-frees or reads transferred-out heap, which aborts or scribbles here.
#[test]
fn vec_element_store_copy_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-elem-store-copy-shapes-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(NO_DOUBLE_FREE_SOURCE, dir.path(), "copy_shapes");

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run copy-shapes binary");

    assert!(
        output.status.success(),
        "copy shapes must run clean under the poisoned allocator — a crash here \
         indicates a bound-local element source was wrongly routed to a MOVE store \
         (double free / read-after-move);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "66OK",
        "copy shapes must print sharedTwice()=6, readAfter()=6, then OK — a \
         scribbled or wrong value indicates the source local's heap was moved out \
         while a live reader (the second push, or the after-read) still needed it;\n{}",
        describe_output(&output)
    );
}
