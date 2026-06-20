//! Owned-element `Vec<record-with-collection-field>` array-literal leak oracle
//! (v0.6-generic-direct-call).
//!
//! A record whose field is itself a collection (`type Boxed { payload: [i64] }`)
//! owns heap through that field even when every element is `BitCopy`. The three
//! heap-ownership authorities (`ty_contains_heap_owning`, the MIR
//! `named_elem_owns_heap` harvest, codegen `resolved_ty_contains_heap_leaf`)
//! historically treated only `string`/`bytes` as heap leaves and recursed into a
//! collection's element type-args, so `Boxed` classified non-heap-owning. A
//! `Vec<Boxed>` then fell through BOTH the owned-element drop arm (the harvest
//! never registered `Boxed`) AND the plain-vec arm (`ValueClass::of_ty(Boxed)`
//! correctly says `CowValue`), landing in NO drop class — the whole chain (outer
//! buffer + element records + each element's `payload` buffer) leaked. Indexing
//! the Vec (`xs[0]`) made it observable end to end.
//!
//! A second layer: once `Vec<Boxed>` classifies owned, the array-literal desugar
//! pushed each element with the COPY-IN `hew_vec_push_owned` (deep clone), which
//! leaked the fresh `record_init` temp the clone was made from — the array
//! literal's element is a throwaway temp with no scope-exit drop to retain. The
//! fix routes array-literal owned pushes to `hew_vec_push_owned_move` (a
//! byte-copy heap transfer with no clone), so the element's heap moves into the
//! Vec and the Vec's `hew_vec_free_owned` releases it exactly once.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): an
//!   array literal of `Boxed { payload: [..] }` is indexed and every element's
//!   payload cell is read back. Pre-fix this leaks (silent) or, under a
//!   double-free regression, aborts; the exact-string equality plus the clean
//!   exit pin both directions.
//! - **Per-frame leak slope** (macOS-only via `leaks(1)`): build-and-drop a
//!   `Vec<Boxed>` per frame. Post-fix the leak-node count is flat across frame
//!   counts; a regression that drops the outer Vec shallowly (or fails to move
//!   the element in) shows a per-frame slope.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A per-frame leak of the element record
/// payloads (one inner `Vec<i64>` per element) lands well above tolerance over
/// the `HIGH_FRAMES - LOW_FRAMES = 47`-frame delta.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// An array literal of `Boxed { payload: [i64] }` indexed and read back. Each
/// element owns a heap `payload` buffer; the outer `Vec<Boxed>` must be built
/// through the owned-element descriptor ABI and freed with `hew_vec_free_owned`,
/// which runs `__hew_record_drop_inplace_Boxed` (freeing each `payload`) over
/// every live element. Reads `payload` cells from two distinct elements so a
/// shallow/garbage element changes the output verbatim.
const VEC_BOXED_ARRAY_LITERAL_SOURCE: &str = "\
type Boxed {\n\
\x20   payload: [i64];\n\
}\n\
\n\
fn main() {\n\
\x20   let xs = [Boxed { payload: [10, 20] }, Boxed { payload: [30, 40] }];\n\
\x20   let a = xs[0];\n\
\x20   let b = xs[1];\n\
\x20   print(f\"{a.payload[0]}|{a.payload[1]}|{b.payload[0]}|{b.payload[1]}\");\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact output for `VEC_BOXED_ARRAY_LITERAL_SOURCE`.
const VEC_BOXED_ARRAY_LITERAL_EXPECTED: &str = "10|20|30|40OK";

/// Per-frame build-and-drop shape for the leak slope: each iteration builds a
/// fresh `Vec<Boxed>` array literal, reads it, and lets it drop on the
/// back-edge. Post-fix the outer drop runs each element's record drop thunk and
/// frees its `payload` buffer, so the leak count is flat across frame counts.
fn vec_boxed_drop_loop_source(frames: usize) -> String {
    format!(
        "type Boxed {{\n\
         \x20   payload: [i64];\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let xs = [Boxed {{ payload: [10, 20] }}, Boxed {{ payload: [30] }}];\n\
         \x20       let a = xs[0];\n\
         \x20       total = total + a.payload[0] + xs.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── leak measurement plumbing (same shape as nested_vec_push_leak_oracle) ────

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
        .prefix(&format!("vec-record-collection-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration owned-element record (or its \
         collection field) is not being released — `Vec<record-with-collection-field>` must be \
         built owned and freed via `hew_vec_free_owned` running the per-element record drop \
         thunk. Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-record-collection-{name}-"))
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
         double-free of an owned-element record payload (a move-in/clone-in mismatch);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back every element's payload cells verbatim — a differing/garbage cell \
         indicates the element record or its collection field was shallow-copied or dropped \
         early;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: an array literal of `Boxed { payload: [i64] }` indexed
/// and read back must print every payload cell verbatim and exit clean under the
/// poisoned allocator. Reverting the collection-field heap-leaf classification
/// (so `Vec<Boxed>` is built plain) or the array-literal move-in (so the element
/// temp double-frees / aliases) fails this.
#[test]
fn vec_boxed_array_literal_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_boxed_array_literal",
        VEC_BOXED_ARRAY_LITERAL_SOURCE,
        VEC_BOXED_ARRAY_LITERAL_EXPECTED,
    );
}

/// Forward leak guard: building and dropping a `Vec<Boxed>` (record with a
/// `[i64]` collection field) per frame must not grow the leak-node count with
/// the frame count. Post-fix the outer drop runs the synthesized record drop
/// thunk, releasing each element's `payload` buffer. A regression that
/// mis-classifies the record non-heap-owning (no drop at all) shows a per-frame
/// slope this catches.
#[test]
fn vec_boxed_outer_drop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_boxed_drop_loop",
        vec_boxed_drop_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}
