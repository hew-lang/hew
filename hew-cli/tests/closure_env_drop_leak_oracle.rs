//! Closure-environment scope-exit drop oracles: per-iteration leak slope plus
//! poisoned-allocator no-double-free pins for the heap-env closure shapes this
//! lane completes (nested env-of-env, type-parameterised owned capture, and the
//! `BorrowMut` counter-factory write-back).
//!
//! ## Why this oracle exists
//!
//! Closures capture heap into their environment. An escaping closure promotes
//! its env to a `hew_dyn_box_alloc` box with a leading free-thunk slot; when the
//! closure pair drops — on normal scope exit, an early return, or the
//! cancel/actor-shutdown drop path — the env free thunk must release every owned
//! capture field (Act 1) and then free the box (Act 2). A captured field that is
//! ITSELF a closure pair recurses into the inner env's free thunk, so a nested
//! capture must drop transitively. This oracle is the binary-level proof that
//! the manifest is complete for the shapes the lane admits: a missing field-drop
//! leaks per iteration (caught by the slope), and a double-counted field
//! double-frees (caught by the poisoned allocator).
//!
//! ## Slope methodology
//!
//! Mirrors `vec_local_drop_leak_oracle.rs`: compile the same shape at a LOW and
//! a HIGH frame count, measure leak NODE counts under `leaks --atExit` with the
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` triple, and assert
//! the delta stays within a small constant independent of frame count. A
//! per-iteration leak (one un-dropped env box, or an un-dropped owned capture
//! inside it) grows ~1+ node/frame and lands an order of magnitude above the
//! tolerance across the `50 - 3 = 47`-frame delta. Absolute counts are not
//! asserted — runtime one-off allocations jitter by a few nodes.
//!
//! ## No-double-free pin
//!
//! The combined shape exercises a nested env-of-env, a counter factory, and a
//! type-parameterised owned capture in one process, then prints a deterministic
//! checksum. Under the poisoned-allocator triple, a producer-side double-drop of
//! any env box (or a captured field the inner free thunk also releases) corrupts
//! the heap or aborts before the checksum is printed, so a clean
//! `125OK` is the no-double-free witness.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); on other platforms they log `skip:` and return. The scribble pin
//! runs on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A slope of >= 1.0 leak/frame (one
/// un-dropped env box per iteration) produces `HIGH_FRAMES - LOW_FRAMES = 47`+
/// excess nodes against the tolerance.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off scheduler/runtime
/// allocations that appear only in the HIGH run while still catching a slope of
/// ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Fixture A — nested env-of-env (#4b heap). `make_pair` returns an outer
/// closure that captured an INNER closure binding; the outer escapes, so its
/// env is heap-boxed and the inner closure pair is heap-promoted and rides into
/// the outer env. Each iteration creates and drops the whole env-of-env, so the
/// outer free thunk must recurse into the inner env's free thunk every frame.
/// Pre-recursion: the inner box leaks one node/frame.
fn env_of_env_loop_source(frames: usize) -> String {
    format!(
        "fn make_pair(base: i64) -> fn(i64) -> i64 {{\n\
         \x20   let inner = |x: i64| x + base;\n\
         \x20   |y: i64| inner(y) + 1\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let f = make_pair(i);\n\
         \x20       total = total + f(10);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture B — type-parameterised OWNED capture (#6). `pick<T>` captures a `T`
/// into a closure; instantiated at `string`, the env field is a heap string the
/// escaping env free thunk must release. The `make_str` indirection forces the
/// closure to escape its constructor frame (heap env) so the drop runs through
/// the box free thunk, not a stack scope exit.
fn owned_type_param_loop_source(frames: usize) -> String {
    format!(
        "fn hold<T>(x: T) -> fn() -> i64 {{\n\
         \x20   || {{ let _h = x; 0 }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let g = hold(\"per-iteration-owned-captured-string\");\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Fixture C — `BorrowMut` counter factory (#1′). `make_counter` returns a closure
/// capturing a local `count` by `BorrowMut`; the env is heap-boxed and mutated in
/// place via `ClosureEnvFieldStore`. The scalar field is `BitCopy` (no owned
/// drop), so this isolates the ENV BOX free path: each iteration's box must be
/// released even though the closure was invoked and mutated.
fn counter_factory_loop_source(frames: usize) -> String {
    format!(
        "fn make_counter() -> fn() -> i64 {{\n\
         \x20   var count: i64 = 0;\n\
         \x20   || {{ count = count + 1; count }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let c = make_counter();\n\
         \x20       total = total + c() + c();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture D — flat OWNED capture baseline (non-generic). An escaping closure
/// captures a concrete `string` and borrows it. The env free thunk must release
/// the string; this isolates the single-owned-field drop from the generic
/// substitution path (Fixture B) and the nesting recursion (Fixture A).
fn flat_owned_capture_loop_source(frames: usize) -> String {
    format!(
        "fn make_holder(label: string) -> fn() -> i64 {{\n\
         \x20   || label.len()\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let h = make_holder(\"per-iteration-flat-owned-string\");\n\
         \x20       total = total + h();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// No-double-free pin source — exercises a nested env-of-env, a counter factory,
/// and a type-parameterised owned capture in one process, then prints the
/// checksum `111 + 6 + 8 = 125`. A double-drop of any env box (or a captured
/// field the inner free thunk also releases) aborts or scribbles before the
/// checksum prints under the poisoned allocator.
const NO_DOUBLE_FREE_SOURCE: &str = "\
fn make_pair(base: i64) -> fn(i64) -> i64 {\n\
\x20   let inner = |x: i64| x + base;\n\
\x20   |y: i64| inner(y) + 1\n\
}\n\
fn make_counter() -> fn() -> i64 {\n\
\x20   var count: i64 = 0;\n\
\x20   || { count = count + 1; count }\n\
}\n\
fn pick<T>(x: T) -> T {\n\
\x20   let get = || x;\n\
\x20   get()\n\
}\n\
fn main() {\n\
\x20   let f = make_pair(100);\n\
\x20   let a = f(10);\n\
\x20   let c = make_counter();\n\
\x20   let b = c() + c() + c();\n\
\x20   let s = pick(\"captured\");\n\
\x20   let d = s.len();\n\
\x20   print(a + b + d);\n\
\x20   print(\"OK\");\n\
}\n";

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
        .prefix(&format!("closure-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration closure env (or an owned \
         capture inside it) is not being released. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked block's stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Nested env-of-env (#4b heap): the outer escaping env's free thunk must
/// recurse into the captured inner closure's env free thunk every frame.
#[test]
fn closure_env_of_env_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "closure_env_of_env",
        env_of_env_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Type-parameterised owned capture (#6): a `string` captured through a generic
/// `T` is released by the escaping env free thunk after monomorphisation
/// substitutes the env field to its concrete owned type.
#[test]
fn closure_owned_type_param_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "closure_owned_type_param",
        owned_type_param_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// `BorrowMut` counter factory (#1′): the heap env box of an invoked-and-mutated
/// escaping closure is released every frame.
#[test]
fn closure_counter_factory_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "closure_counter_factory",
        counter_factory_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Flat owned capture baseline: a single concrete-`string` capture field is
/// released by the env free thunk, isolating the owned-field drop from nesting
/// and generics.
#[test]
fn closure_flat_owned_capture_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "closure_flat_owned_capture",
        flat_owned_capture_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// No-double-free pin: the combined nested + counter + owned-capture shape runs
/// to completion under the poisoned-allocator triple and prints the `125`
/// checksum. A double-drop of any env box (or a captured field the inner free
/// thunk also releases) aborts or scribbles the value before it prints.
#[test]
fn closure_env_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("closure-leak-no-double-free-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(NO_DOUBLE_FREE_SOURCE, dir.path(), "no_double_free");

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run closure no-double-free binary");

    assert!(
        output.status.success(),
        "closure env shapes must run clean under the poisoned allocator — a \
         crash here indicates a double-drop of an escaped closure env box;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "125OK",
        "closure env shapes must print the 111 + 6 + 8 checksum untouched — a \
         scribbled value indicates a producer-side double-drop of an env field;\n{}",
        describe_output(&output)
    );
}
