//! Generic `Vec<T>.get` owned-element leak oracle (hew-lang/hew#1929 Stage 2).
//!
//! `Vec<T>.get` where `T` is a type parameter bound to a non-Copy record type
//! routes through the owned-element ABI: the getter calls `hew_vec_get_owned`
//! (borrows the live slot and transfers ownership to the caller), while the
//! Vec's scope-exit drop calls `hew_vec_free_owned` (runs the per-element
//! record drop thunk over every live slot). Pre-fix these two paths aliased
//! the same heap string — the getter transferred ownership while `free_owned`
//! also released the slot — producing a double-free on the first element's
//! `label` field within a few iterations. A missed drop on the other side
//! would grow the leak-node count linearly with iteration count.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): push an
//!   owned `Name { label: "owned-ok" }`, get it back through a generic helper,
//!   read the field, and drop. Under `MallocScribble`/`MallocPreScribble`/
//!   `MallocGuardEdges` a double-free aborts the process; a use-after-free read
//!   returns scribbled memory, changing the output. The string equality plus the
//!   clean exit pin both regression directions.
//!
//! - **Per-frame leak slope** (macOS-only via `leaks(1)`): build a
//!   `Vec<Name>`, get the element via the generic helper, and drop both — once
//!   per frame. Post-fix the leak-node count is flat across frame counts. A
//!   regression that drops the Vec shallowly (no `hew_vec_free_owned`, no per-
//!   element `label` drop) shows a per-frame slope this catches. This is the
//!   committed oracle the cross-ecosystem review identified as missing — the
//!   agent ran `leaks(1)` manually but did not assert 0/0 in a committed test.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! elsewhere it logs `skip:` and returns. The scribble correctness pin runs on
//! any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A per-frame leak of one heap string
/// per `Name` record (one `label` buffer not freed by the element drop thunk)
/// accumulates well above tolerance over the `HIGH_FRAMES - LOW_FRAMES = 47`
/// delta.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Absorbs
/// one-off scheduler/runtime allocations that appear only in the HIGH run while
/// still catching a slope of ~0.1 leaks/frame (same headroom as sibling oracles).
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Single round-trip under the poisoned allocator: push one `Name { label:
/// "owned-ok" }`, call `first<T>` (the generic returning helper), read
/// `got.label`, and let both the returned record and the Vec drop at scope
/// exit. Under `MallocScribble` a double-free aborts; a UAF read on `got.label`
/// returns scribbled memory (not `"owned-ok"`). The exact-string equality plus
/// clean exit pin both directions simultaneously.
const GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn first<T>(v: Vec<T>) -> T {\n\
\x20   v.get(0)\n\
}\n\
\n\
fn main() {\n\
\x20   let vn: Vec<Name> = Vec::new();\n\
\x20   vn.push(Name { label: \"owned-ok\" });\n\
\x20   let got = first(vn);\n\
\x20   print(got.label);\n\
}\n";

/// Expected exact output for `GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE`. Any
/// aliasing or UAF changes the `got.label` read.
const GENERIC_GET_SINGLE_ROUNDTRIP_EXPECTED: &str = "owned-ok";

/// Per-frame build-and-drop shape for the leak slope: each iteration pushes one
/// owned `Name`, gets it back through the generic helper, reads the label
/// length (forces a live read of the slot before drop), and lets both the
/// returned record and the Vec drop on the back-edge. Post-fix the Vec drop
/// runs `hew_vec_free_owned` → `__hew_record_drop_inplace_Name` → frees
/// `label`, so the leak count is flat across frame counts.
fn generic_get_drop_loop_source(frames: usize) -> String {
    format!(
        "type Name {{ label: string; }}\n\
         \n\
         fn first<T>(v: Vec<T>) -> T {{\n\
         \x20   v.get(0)\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let vn: Vec<Name> = Vec::new();\n\
         \x20       vn.push(Name {{ label: \"owned-ok\" }});\n\
         \x20       let got = first(vn);\n\
         \x20       total = total + got.label.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── leak measurement plumbing (same shape as vec_record_collection_field_leak_oracle) ──

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
/// `Some(leak_count)` when `leaks` produced a usable report. Returns `None` and
/// emits a `skip:` notice when `leaks(1)` declines to attach or does not emit
/// the expected summary line.
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
/// counts via `leaks(1)`, and assert the delta stays within `SLOPE_TOLERANCE`.
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
        .prefix(&format!("vec-generic-get-owned-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates that `Vec<T>.get` for an owned record element \
         leaks the element's heap string — `hew_vec_free_owned` must run the per-element record \
         drop thunk (`__hew_record_drop_inplace_Name`) to free the `label` buffer. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked allocation stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-generic-get-owned-{name}-"))
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
         double-free of the owned `Name.label` string (the generic getter and \
         `hew_vec_free_owned` both released the same slot);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back the label verbatim — scribbled/empty output indicates a \
         use-after-free read on `got.label` (the slot was freed before the caller read it);\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: a single push-get-read-drop round-trip through a
/// generic `first<T>` helper must print `"owned-ok"` and exit clean under the
/// poisoned allocator. Reverting the owned-getter routing (so the getter aliases
/// the Vec's slot while `hew_vec_free_owned` also releases it) fails this with
/// either an abort (double-free) or garbled output (UAF read).
#[test]
fn vec_generic_get_owned_element_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_generic_get_single_roundtrip",
        GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE,
        GENERIC_GET_SINGLE_ROUNDTRIP_EXPECTED,
    );
}

/// Forward leak guard: pushing one owned `Name`, getting it via a generic
/// helper, and dropping both per frame must not grow the leak-node count with
/// the frame count. Post-fix `hew_vec_free_owned` runs
/// `__hew_record_drop_inplace_Name` on the slot, releasing the `label` buffer.
/// A regression that makes the element drop thunk a no-op (missing record-drop
/// synthesis for a type-parameter-bound element type) shows a per-frame slope
/// this catches.
#[test]
fn vec_generic_get_owned_element_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_generic_get_drop_loop",
        generic_get_drop_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}
