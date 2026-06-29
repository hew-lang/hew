//! `var`-local overwrite leak oracles: reassigning an owned `var` in a loop
//! must release the prior value, not just the final one (#53).
//!
//! Companion of `state_overwrite_leak_oracle.rs`. State fields ride
//! `__hew_record_overwrite_release` through `lower_actor_state_field_store`;
//! this pins the var-local analogue (`emit_local_overwrite_release`): a
//! `var r: Rec = ...; while ... { r = make(); }` body whose bare `Instr::Move`
//! used to overwrite the slot and only free the LAST value at scope exit,
//! leaking one record per iteration.
//!
//! ## Slope methodology
//!
//! Identical to `state_overwrite_leak_oracle.rs`: compile each shape at a LOW
//! and a HIGH frame count, measure leak NODE counts under `leaks --atExit`
//! with the poisoned-allocator triple, and assert the delta stays within a
//! small constant. Pre-fix the record/string var shapes leaked exactly one
//! block per frame (a 47-block delta over 50 - 3 frames). macOS-only for the
//! `leaks` oracles; the alias/escape UAF pins run on any unix.
//!
//! ## Alias / escape UAF pins
//!
//! The overwrite release is gated on `owned_locals` membership, so it never
//! frees a value the RHS already consumed: a self-reassign `r = Rec { .., ..r }`
//! and a consume-then-reassign `take(r); r = make()` both move the old value
//! out before the store, so the release is skipped. Two scribble pins run
//! those shapes under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`
//! and assert an exact exit code — an over-eager release frees a buffer the
//! new value (or the callee) re-owns and the trailing read crashes before the
//! assertion is reached.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the re-store path at least twice.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check (47-frame delta against the +5
/// tolerance; the pre-fix slope is 1 block/frame).
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Record var overwrite: `r` holds a freshly built `Rec` every iteration. The
/// prior record's heap `string` must be released on each `r = make(i)` store;
/// pre-fix every overwrite leaked the old record. Returns the final name length
/// (11 = `"payload-xyz"`).
fn record_var_overwrite_source(frames: usize) -> String {
    format!(
        "record Rec {{\n\
         \x20   name: string,\n\
         }}\n\
         \n\
         fn make(i: i64) -> Rec {{\n\
         \x20   Rec {{ name: \"payload-\" + \"xyz\" }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var r: Rec = make(0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       r = make(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   r.name.len()\n\
         }}\n"
    )
}

/// String var overwrite: `s` is rebound to a heap-built string every iteration.
/// Each `s = ...` must release the previous owner (pre-fix: one leaked block
/// per frame). Returns the final length (4 = `"pong"`).
fn string_var_overwrite_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var s: string = \"start\" + \"!\";\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       s = \"po\" + \"ng\";\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   s.len()\n\
         }}\n"
    )
}

/// Self-reassign UAF pin: `r = Rec { n: i, ..r }` byte-copies the old `name`
/// pointer into the new record, so the old value is consumed and the overwrite
/// release MUST be skipped. The name is heap-built (`.to_upper()`) so an
/// over-eager release would actually free it; the trailing read then crashes
/// under the poisoned triple. Exit = 7 (`"KEEPER-"`).
const RECORD_SELF_REASSIGN_SOURCE: &str = "\
record Rec {
    name: string,
    n: i64,
}

fn main() -> i64 {
    var r: Rec = Rec { name: \"keeper-\".to_upper(), n: 0 };
    var i: i64 = 0;
    while i < 25 {
        r = Rec { n: i, ..r };
        i = i + 1;
    }
    r.name.len()
}
";

/// Consume-then-reassign UAF pin: `take(r)` moves the record into a by-value
/// callee that frees it, then `r = make()` rebinds. The old value is gone
/// before the store, so the overwrite release must be skipped (membership in
/// `owned_locals` was dropped at the consume). An over-eager release double-
/// frees the buffer the callee already freed. Exit = 5 (`"fresh"`).
const RECORD_CONSUME_REASSIGN_SOURCE: &str = "\
record Rec {
    name: string,
}

fn take(r: Rec) -> i64 {
    r.name.len()
}

fn make() -> Rec {
    Rec { name: \"fresh\".to_upper() }
}

fn main() -> i64 {
    var r: Rec = make();
    var i: i64 = 0;
    var t: i64 = 0;
    while i < 25 {
        t = t + take(r);
        r = make();
        i = i + 1;
    }
    r.name.len()
}
";

// ── plumbing (same shape as state_overwrite_leak_oracle) ──────────────────

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
                        return Some(n);
                    }
                }
            }
        }
    }
    eprintln!(
        "skip: leaks did not emit a `Process <pid>: N leak(s) ...` summary for {}",
        bin.display()
    );
    None
}

fn assert_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
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
        .prefix(&format!("var-overwrite-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(LOW_FRAMES),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(HIGH_FRAMES),
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
        "{shape_name}: low_frames={LOW_FRAMES} low_leaks={low_leaks} \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low={low_leaks} (frames={LOW_FRAMES}), \
         high={high_leaks} (frames={HIGH_FRAMES}). Excess of {} NODES over tolerance \
         {SLOPE_TOLERANCE} means a per-overwrite value is not released. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Compile and run under the poisoned-allocator triple (no `leaks` dependency)
/// and assert an exact exit code. An over-eager overwrite release frees a buffer
/// the new value / callee re-owns; the trailing read crashes or returns a
/// scribbled length.
fn assert_scribbled_run_exit(shape_name: &str, source: &str, expected_exit: i32) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("var-overwrite-uaf-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run fixture binary");
    assert_eq!(
        output.status.code(),
        Some(expected_exit),
        "{shape_name}: expected clean exit {expected_exit} under the poisoned-allocator \
         triple; an over-eager var overwrite release frees a value the RHS already \
         consumed. Output:\n{}",
        describe_output(&output)
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Record var: reassigning an owned record in a loop releases the prior record
/// every iteration (pre-fix slope 1 block/frame).
#[test]
fn record_var_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("record_var_overwrite", record_var_overwrite_source);
}

/// String var: reassigning a string in a loop releases the prior owner.
#[test]
fn string_var_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("string_var_overwrite", string_var_overwrite_source);
}

/// UAF pin — self-reassign keeps the byte-copied `..r` leaf alive (release
/// skipped; final name is `"KEEPER-"`, len 7).
#[test]
fn record_self_reassign_keeps_aliased_leaf_alive() {
    assert_scribbled_run_exit("record_self_reassign", RECORD_SELF_REASSIGN_SOURCE, 7);
}

/// UAF pin — consume-then-reassign does not double-free the value the callee
/// already freed (release skipped; final name is `"FRESH"`, len 5).
#[test]
fn record_consume_then_reassign_no_double_free() {
    assert_scribbled_run_exit("record_consume_reassign", RECORD_CONSUME_REASSIGN_SOURCE, 5);
}
