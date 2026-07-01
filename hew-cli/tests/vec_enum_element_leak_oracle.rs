//! `Vec<enum>` local scope-exit drop oracles: per-iteration leak slope for the
//! direct-enum element class, plus an owned-route-intact pin and a
//! poisoned-allocator no-double-free pin for the escape shapes.
//!
//! Empirical oracle for the direct-enum Vec leak class. A local `Vec<E>` whose
//! element `E` is a direct (fieldless / non-indirect) user enum owns no heap,
//! so it must ride the PLAIN release path (`hew_vec_free`, buffer + handle).
//! Pre-fix, the plain-vs-owned routing in `hew-mir/src/lower.rs` decided
//! plainness from `ValueClass::of_ty(elem) == ValueClass::BitCopy` alone.
//! `ValueClass` deliberately excludes user enums (hew-hir's
//! `finalize_user_record_value_classes` covers records only), so a direct
//! enum was never `BitCopy`: `is_plain_vec_element` said "not plain" and
//! `is_owned_vec_element` said "not owned", `classify_vec_element_release`
//! returned `Unsupported(UnenumeratedShape)`, and — because the compile-time
//! reject only fires on `NoReleaseProtocol` — the program still compiled with
//! NO drop emitted. Every `Vec<direct-enum>` local leaked its buffer AND its
//! handle at scope exit: two nodes per frame.
//!
//! The fix reads the single heap-ownership authority instead of re-deriving
//! heap-ness from the value class: the plain-Vec admission arms gained the
//! additive `ty_is_direct_enum_element(elem) && !named_elem_owns_heap(elem)`
//! disjunct, so a heap-free direct enum is correctly PLAIN and gets the plain
//! release. The `ValueClass::BitCopy` disjunct is retained unchanged for
//! records and `BitCopy` builtins.
//!
//! ## Slope methodology
//!
//! Mirrors `vec_local_drop_leak_oracle.rs`: compile the same shape at a LOW
//! frame count and a HIGH frame count, measure leak NODE counts under
//! `leaks --atExit` with the poisoned-allocator triple, and assert the delta
//! stays within a small constant independent of frames. The pre-fix bug class
//! is PER-FRAME GROWTH (one leaked vec per loop iteration — two nodes for a
//! direct enum: handle + buffer), which over a `50 - 3 = 47`-frame delta lands
//! ~94 nodes above the +5 tolerance. Absolute counts are deliberately not
//! asserted — runtime/scheduler one-off allocations jitter by +/-1 node.
//!
//! ## Owned-route-intact pin
//!
//! The fix is additive and surgical: it flips ONLY heap-free direct enums to
//! plain. A payload-carrying enum (`Label(string)`) still owns heap, so it
//! must keep riding the OWNED release path (`hew_vec_free_owned`, which walks
//! and releases each element's payload). `payload_enum_vec_loop` asserts that
//! shape stays flat too — a regression that mis-routed it to plain would leak
//! the per-element `string` payloads (slope), and an over-drop that released a
//! payload twice would crash under the scribble triple. Zero leaks + clean
//! exit proves the owned route survived and the fix does not over-drop.
//!
//! ## No-double-free pin
//!
//! The escape shapes the prover must EXCLUDE (a returned vec, a vec consumed
//! by a by-value call) run to completion under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and must exit with
//! the expected value: a producer-side drop of a handle the new owner also
//! releases would crash (or corrupt the result) under the poisoned allocator
//! before any assertion is read.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); on other platforms they log `skip:` and return. The scribble
//! pin runs on any unix host.
//!
//! The tuple-of-enum variant (`Vec<(Colour, i64)>`) that also exercises the
//! `tuple_is_all_bitcopy` release arm is intentionally NOT a runtime fixture
//! here: `Vec::new`/array-literal construction of a tuple-element vec is a
//! separate NYI on trunk (`no constructor lowering for element type
//! Tuple(..)`), so the shape is unreachable at runtime. That arm's routing is
//! covered by the `release_bucket_partition_is_total_over_vec_elements`
//! totality test in `hew-mir/src/lower.rs`.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge path at least twice while
/// staying close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A slope of ~2.0 leaks/frame (the
/// pre-fix measurement for the direct-enum Vec leak class: buffer + handle)
/// produces roughly `2 * (HIGH_FRAMES - LOW_FRAMES) = 94` excess nodes against
/// the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off
/// scheduler/runtime allocations that appear only in the HIGH run while still
/// catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Fixture A — the probe's primary leaking shape. A `while` loop creates one
/// fresh `Vec<Colour>` per iteration (`Colour` is a direct, fieldless user
/// enum that owns no heap), pushes into it, and reads it back, then lets it go
/// out of scope on the back-edge. Pre-fix: the handle and its buffer leak
/// every iteration (the enum was classified neither plain nor owned, so no
/// drop was emitted). Post-fix: the direct enum is proven heap-free by
/// `named_elem_owns_heap`, routed PLAIN, and released on every back-edge plus
/// the final fall-through.
fn colour_vec_loop_source(frames: usize) -> String {
    format!(
        "enum Colour {{ Red; Green; Blue; }}\n\
         \n\
         fn tag(c: Colour) -> i64 {{\n\
         \x20   match c {{\n\
         \x20       Colour::Red => 1,\n\
         \x20       Colour::Green => 2,\n\
         \x20       Colour::Blue => 3,\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<Colour> = Vec::new();\n\
         \x20       v.push(Colour::Red);\n\
         \x20       v.push(Colour::Green);\n\
         \x20       total = total + v.len() + tag(v[0]);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture B — the owned-route-intact pin. `Shape::Label(string)` owns heap,
/// so `Vec<Shape>` must keep riding the OWNED release path even after the
/// fix. The `hew_vec_free_owned` walk releases each element's `string`
/// payload along with the buffer and handle, so the slope stays flat. A
/// regression that mis-routed this to plain would leak the per-element
/// payloads (a slope); this fixture is the complement to Fixture A that
/// proves the fix flips ONLY heap-free direct enums.
fn payload_enum_vec_loop_source(frames: usize) -> String {
    format!(
        "enum Shape {{ Dot; Label(string); }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<Shape> = Vec::new();\n\
         \x20       v.push(Shape::Label(\"heap-owning-per-iteration-payload\"));\n\
         \x20       v.push(Shape::Dot);\n\
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
/// that crashes before main can print its checksum. Values: `make()` yields
/// `[Green, Blue]` (tags 2 + 3 = 5); the consumed vec is `[Blue, Blue]`
/// (tags 3 + 3 = 6); the sum is 11.
const ESCAPE_SHAPES_SOURCE: &str = "\
enum Colour { Red; Green; Blue; }\n\
\n\
fn tag(c: Colour) -> i64 {\n\
\x20   match c {\n\
\x20       Colour::Red => 1,\n\
\x20       Colour::Green => 2,\n\
\x20       Colour::Blue => 3,\n\
\x20   }\n\
}\n\
\n\
fn make() -> Vec<Colour> {\n\
\x20   let v: Vec<Colour> = Vec::new();\n\
\x20   v.push(Colour::Green);\n\
\x20   v.push(Colour::Blue);\n\
\x20   return v;\n\
}\n\
\n\
fn total(xs: Vec<Colour>) -> i64 {\n\
\x20   tag(xs[0]) + tag(xs[1])\n\
}\n\
\n\
fn main() {\n\
\x20   let made = make();\n\
\x20   let a = tag(made[0]) + tag(made[1]);\n\
\x20   let v: Vec<Colour> = Vec::new();\n\
\x20   v.push(Colour::Blue);\n\
\x20   v.push(Colour::Blue);\n\
\x20   let b = total(v);\n\
\x20   print(a + b);\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ──

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
        .prefix(&format!("vec-enum-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration Vec allocation is not being \
         released. For the direct-enum shape this is the pre-fix leak class (routing read \
         `ValueClass::BitCopy` instead of the `named_elem_owns_heap` authority, so the enum was \
         neither plain nor owned and no drop was emitted; slope ~2.0 leaks/frame). \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see which stack the leaked \
         block came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Fixture A: per-iteration `Vec<Colour>` locals (direct fieldless enum) must
/// not leak. Pre-fix slope is ~2.0 leaks/frame (buffer + handle, no drop
/// emitted because the enum was neither plain nor owned); post-fix the
/// per-iteration handle and buffer are released on every back-edge. Reverting
/// the `ty_is_direct_enum_element(elem) && !named_elem_owns_heap(elem)`
/// disjunct in `is_plain_vec_element` fails this by ~94 nodes.
#[test]
fn vec_colour_local_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_colour_local_loop",
        colour_vec_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture B: per-iteration `Vec<Shape>` locals where `Shape::Label(string)`
/// owns heap must ALSO stay flat — via the OWNED route, not plain. This is the
/// complement to Fixture A: it proves the fix is surgical (flips only heap-free
/// direct enums) and does not over-drop. A mis-route to plain would leak the
/// per-element `string` payloads; an over-drop would crash under the scribble
/// triple that `leaks` runs with.
#[test]
fn vec_payload_enum_local_loop_stays_on_owned_route() {
    assert_frame_slope_below_tolerance(
        "vec_payload_enum_local_loop",
        payload_enum_vec_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// No-double-free pin: the returned-vec and consumed-by-value shapes run to
/// completion under the poisoned-allocator triple and print the expected
/// checksum (5 + 6 = 11). A producer-side drop of either escaped `Vec<Colour>`
/// handle would free memory the new owner also frees — under
/// `MallocScribble`/`MallocGuardEdges` that aborts (or scribbles the values)
/// before the checksum is printed.
#[test]
fn vec_enum_escape_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-enum-leak-escape-shapes-")
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
        "escape shapes must run clean under the poisoned allocator — a crash here indicates a \
         producer-side drop of a moved-out vec (double free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "11OK",
        "escape shapes must print the 5 + 6 checksum untouched — a scribbled value indicates the \
         producer freed a handle the new owner still reads;\n{}",
        describe_output(&output)
    );
}
