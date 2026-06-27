//! `HashMap.insert` / `HashSet.insert` overwrite-path key leak oracles: a
//! per-overwrite leak slope plus poisoned-allocator exactly-once pins for the
//! vacant and overwrite paths (issue #2033).
//!
//! Empirical oracle for the overwrite-key leak class. The MIR move-checker
//! statically consumes the key binding at every insert site
//! (`consume_moved_builtin_method_arg`), which is correct on the VACANT path
//! (the runtime moves the caller's key into the slot) but WRONG on the
//! OVERWRITE path (the runtime keeps the stored key and the caller's freshly
//! built duplicate key is orphaned — the static consume suppresses its
//! scope-exit drop and nothing else frees it). The fix is the Stage-C
//! conditional-drop materialiser in `hew-codegen-rs/src/llvm.rs`
//! (`emit_insert_overwrite_key_release`): branch on the insert's `i1` return
//! and, on the overwrite path (`returned == false`), release the caller's key
//! with its type-appropriate drop.
//!
//! ## Slope methodology
//!
//! Mirrors `vec_local_drop_leak_oracle.rs`: compile the same overwrite shape at
//! a LOW frame count and a HIGH frame count, measure leak NODE counts under
//! `leaks --atExit` with the poisoned-allocator triple, and assert the delta
//! stays within a small constant independent of frames. The pre-fix bug class
//! is PER-OVERWRITE GROWTH (one orphaned caller key per colliding insert — the
//! measured pre-fix delta over `50 - 3 = 47` frames was 47 nodes), an order of
//! magnitude above the +5 tolerance.
//!
//! ## Exactly-once pins
//!
//! The vacant path must STILL consume the caller's key (the runtime owns it) —
//! the overwrite release must NOT fire on a vacant insert, or it double-frees
//! the key the map now owns. Two scribble pins run the distinct-key (all
//! vacant) and repeated-key (vacant-then-overwrite) shapes under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and assert the exact
//! map size: a double-free aborts (or corrupts the count) before the assertion
//! is read.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); on other platforms they log `skip:` and return. The scribble
//! pins run on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: at least two overwrites past the first vacant insert.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. The pre-fix slope was ~1.0 leak/frame
/// (one orphaned caller key per overwrite), producing `HIGH_FRAMES - LOW_FRAMES
/// = 47` excess nodes against the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// `HashMap` overwrite shape — the same heap-built `string` key is inserted
/// every iteration, so the first insert is vacant and every later insert is an
/// overwrite that orphans the caller's freshly built key. Pre-fix: one leaked
/// key per overwrite. Post-fix: the conditional release frees it on every
/// overwrite.
fn hashmap_overwrite_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   let m: HashMap<string, i64> = HashMap::new();\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let k: string = \"fixed-overwrite-key-\" + \"z\";\n\
         \x20       m.insert(k, i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   m.len()\n\
         }}\n"
    )
}

/// `HashSet` overwrite shape — a set element is its own key, so re-inserting
/// the same heap-built element orphans the caller's duplicate on every
/// overwrite, exactly like the map key.
fn hashset_overwrite_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   let s: HashSet<string> = HashSet::new();\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let e: string = \"fixed-overwrite-elem-\" + \"z\";\n\
         \x20       s.insert(e);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   s.len()\n\
         }}\n"
    )
}

/// Vacant-path exactly-once shape: three DISTINCT heap-built keys, all vacant
/// inserts. The runtime owns each moved-in key; the overwrite release must NOT
/// fire (every `returned == true`), and the map's own scope-exit drop frees the
/// three stored keys exactly once. Prints the map size (3) then `OK`. A
/// spurious overwrite-path release on a vacant insert would double-free the
/// key the map owns.
const VACANT_SHAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let m: HashMap<string, i64> = HashMap::new();\n\
\x20   m.insert(\"k\" + \"1\", 1);\n\
\x20   m.insert(\"k\" + \"2\", 2);\n\
\x20   m.insert(\"k\" + \"3\", 3);\n\
\x20   print(m.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// Overwrite-path exactly-once shape: the same heap-built key inserted three
/// times — one vacant, two overwrites. The map ends with one entry; the two
/// orphaned caller keys are each freed once by the conditional release, and the
/// map's scope-exit drop frees the one stored key. Prints the map size (1) then
/// `OK`. A double-free (overwrite release + a stale scope-exit drop) or a
/// missing release (leak) is caught by the allocator / the size assertion.
const OVERWRITE_SHAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let m: HashMap<string, i64> = HashMap::new();\n\
\x20   m.insert(\"dup\" + \"key\", 1);\n\
\x20   m.insert(\"dup\" + \"key\", 2);\n\
\x20   m.insert(\"dup\" + \"key\", 3);\n\
\x20   print(m.len());\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ───

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
        .prefix(&format!("map-overwrite-leak-{shape_name}-"))
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
        "{shape_name}: per-overwrite leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates the caller's duplicate key is not released \
         on the overwrite path (pre-fix slope is ~1.0 leak/frame). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see which stack the leaked block \
         came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Run `source` under the poisoned-allocator triple and assert it prints
/// `<expected_len>OK` untouched — a double-free aborts and a leak/corruption
/// changes the count.
fn assert_runs_clean(shape_name: &str, source: &str, expected_stdout: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("map-overwrite-{shape_name}-"))
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
         indicates a double-free of a key the map owns (vacant) or a stale \
         scope-exit drop racing the overwrite release;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected_stdout,
        "{shape_name} must print `{expected_stdout}` untouched — a wrong count \
         indicates a corrupted/double-freed key blob;\n{}",
        describe_output(&output)
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// `HashMap.insert` overwrite must not leak the caller's duplicate key.
/// Reverting `emit_insert_overwrite_key_release` fails this by ~47 nodes.
#[test]
fn hashmap_overwrite_no_per_frame_key_leak_slope() {
    assert_frame_slope_below_tolerance(
        "hashmap_overwrite",
        hashmap_overwrite_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// `HashSet.insert` overwrite must not leak the caller's duplicate element.
#[test]
fn hashset_overwrite_no_per_frame_key_leak_slope() {
    assert_frame_slope_below_tolerance(
        "hashset_overwrite",
        hashset_overwrite_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Vacant-path exactly-once: three distinct keys all moved into the map; the
/// overwrite release must NOT fire (no double-free of the map-owned keys). The
/// map's scope-exit drop frees them once. Prints `3OK`.
#[test]
fn hashmap_vacant_inserts_exactly_once_under_malloc_scribble() {
    assert_runs_clean("hashmap_vacant", VACANT_SHAPE_SOURCE, "3OK");
}

/// Overwrite-path exactly-once: one vacant + two overwrites of the same key.
/// The two orphaned caller keys are each freed once by the conditional release;
/// the one stored key is freed once by the map's scope-exit drop. Prints `1OK`.
#[test]
fn hashmap_overwrite_inserts_exactly_once_under_malloc_scribble() {
    assert_runs_clean("hashmap_overwrite_once", OVERWRITE_SHAPE_SOURCE, "1OK");
}
