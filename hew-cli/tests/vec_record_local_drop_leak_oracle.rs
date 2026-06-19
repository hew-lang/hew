//! Constant-leak oracle for the `Vec<record>` (`BitCopy` value-aggregate element)
//! scope-exit release.
//!
//! ## The leak this pins
//!
//! A `Vec<Point>` where `type Point { x: i64, y: i64 }` is an all-`BitCopy`
//! value record. Such a Vec is constructed inline via `hew_vec_new_with_layout`
//! (the `_layout` ABI codegen uses for a value-aggregate element), NOT through
//! the owned-element descriptor (`hew_vec_new_with_elem_layout`). Pre-fix the
//! MIR drop elaborator's plain-vec predicate (`binding_ty_is_plain_vec`,
//! `hew-mir/src/lower.rs`) only admitted SCALAR and `string` elements, and the
//! owned-element predicate (`is_owned_vec_element`) only admits HEAP-OWNING
//! records (a string/bytes/nested-collection field). A `BitCopy` record element
//! fell into the gap between the two: NEITHER `owned_vec_drop_allowed` nor
//! `plain_vec_drop_allowed` claimed the binding, so `build_lifo_drops` emitted
//! NO drop and the Vec's backing buffer LEAKED on every scope exit.
//!
//! The fix extends `binding_ty_is_plain_vec` to also admit a `Named` /
//! `Tuple` element whose `ValueClass::of_ty` is `BitCopy` — the precise
//! complement of the owned-element class (a heap-owning or closure-bearing
//! aggregate is never `BitCopy`). The matching release is the plain
//! `hew_vec_free` (buffer + handle; a `BitCopy` element owns no heap so no
//! per-element drop runs).
//!
//! ## Methodology: absolute count against a scalar-Vec control
//!
//! macOS `leaks --atExit` traces conservatively: a leaked pointer still sitting
//! in a function-scope `alloca` at process exit is treated as REACHABLE and not
//! flagged. A single-frame `let pts: Vec<Point> = ...; pts.len()` therefore
//! reports 0 leaks even pre-fix (the handle lingers in `main`'s stack slot).
//!
//! To expose the leak the way Valgrind does, each fixture allocates the Vec in
//! a HELPER fn that returns, run in a loop: the stack slot is reused every
//! iteration, so each leaked backing buffer becomes genuinely unreachable and
//! `leaks` counts it. Pre-fix a record-element fixture stood
//! `ITERATIONS * allocations-per-iter` nodes above the control; post-fix it
//! sits at the control's floor.
//!
//! The CONTROL performs the identical loop+helper shape with a `Vec<i64>`
//! element — a Vec that was ALREADY dropped correctly pre-fix (`hew_vec_free`
//! on the scalar path), establishing the runtime's one-off allocation floor.
//! Each record fixture must match that floor within a small jitter tolerance.
//! This is the `== 0` / floor-equality assertion the LESSONS
//! `assert-distinguishes-garbage` rule calls for, not a happy-path `> 0`.
//!
//! ## Skip behaviour
//!
//! `leaks(1)` is Darwin's allocator inspector; on non-macOS hosts the leak
//! probes log `skip:` and return. The `MallocScribble` no-double-free pin runs
//! on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Loop iterations each fixture's helper-fn is called. Each iteration leaks its
/// helper's Vec backing buffer(s) pre-fix; the helper return reuses the stack
/// slot so the leaked buffers are unreachable at exit (the shape `leaks`
/// detects). Picked well above the jitter tolerance so a regression of even a
/// few un-dropped buffers is unambiguous.
const ITERATIONS: usize = 64;

/// Permitted leak-node delta between a record fixture and the scalar-Vec
/// control. Pre-fix every iteration leaked at least one backing buffer, so the
/// fixture stood >= `ITERATIONS` nodes above the control; post-fix it matches.
/// The tolerance of 2 absorbs the +/-1 scheduler/runtime one-off jitter the
/// sibling oracles document while staying far below `ITERATIONS`.
const FLOOR_TOLERANCE: usize = 2;

// -- fixtures ----------------------------------------------------------------

/// Control: a `Vec<i64>` allocated + populated in a helper, looped
/// `ITERATIONS` times. The scalar Vec was dropped correctly pre-fix
/// (`hew_vec_free`), so this program leaks nothing and establishes the floor.
fn control_scalar_vec_source() -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    let xs: Vec<i64> = Vec::new();
    xs.push(n);
    xs.push(n);
    xs.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 70; }}
    0
}}
",
        expected = ITERATIONS * 2
    )
}

/// Empty + pushed `Vec<Point>` (`BitCopy` record element). Pre-fix the backing
/// buffer leaked once per helper call (the gap between the owned and plain drop
/// classes); post-fix `hew_vec_free` releases it.
fn vec_record_new_push_source() -> String {
    format!(
        "\
type Point {{ x: i64, y: i64 }}

fn build(n: i64) -> i64 {{
    let pts: Vec<Point> = Vec::new();
    pts.push(Point {{ x: n, y: n }});
    pts.push(Point {{ x: n, y: n }});
    pts.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 71; }}
    0
}}
",
        expected = ITERATIONS * 2
    )
}

/// Array literal `[Point { .. }]` of a `BitCopy` record. Lowers to the same
/// `hew_vec_new_with_layout` backing buffer; pre-fix it leaked once per call.
fn vec_record_array_literal_source() -> String {
    format!(
        "\
type Point {{ x: i64, y: i64 }}

fn build(n: i64) -> i64 {{
    let pts = [Point {{ x: n, y: n }}];
    pts.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {ITERATIONS} {{ return 72; }}
    0
}}
"
    )
}

/// Array-repeat `[Point { .. }; 3]` of a `BitCopy` record — the array-repeat
/// gate's own leak-clean shape. The repeat clones the element into 3 inline
/// slots of one backing buffer; pre-fix that buffer leaked once per call.
fn vec_record_array_repeat_source() -> String {
    format!(
        "\
type Point {{ x: i64, y: i64 }}

fn build(n: i64) -> i64 {{
    let pts = [Point {{ x: n, y: n }}; 3];
    pts.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 73; }}
    0
}}
",
        expected = ITERATIONS * 3
    )
}

/// No-double-free pin: a `Vec<Point>` plus a read of its element back into a
/// record local, a scalar array-repeat, and a `Vec<string>` (the CAP-09 path),
/// all in one frame. The record-Vec free must run EXACTLY once and must not
/// touch the inline `BitCopy` elements as if they owned heap; the string-Vec and
/// scalar paths must stay clean. A double-free or wrong-ABI free crashes under
/// `MallocScribble` / `MallocGuardEdges` before the sentinel prints.
const NO_DOUBLE_FREE_SOURCE: &str = "\
type Point { x: i64, y: i64 }

fn main() {
    let pts: Vec<Point> = Vec::new();
    pts.push(Point { x: 10, y: 20 });
    let p = pts[0];
    let lit = [Point { x: 3, y: 4 }; 2];
    let words: Vec<string> = Vec::new();
    words.push(\"alpha\");
    let ns = [7; 3];
    let sum = p.x + p.y + lit.len() + words.len() + ns[0];
    print(sum);
    print(\"OK\");
}
";

// -- leak measurement plumbing (shape shared with the sibling oracles) -------

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

/// macOS + `leaks(1)` availability guard shared by every leak probe.
fn leaks_available(shape_name: &str) -> bool {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return false;
    }
    let avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !avail {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
    }
    avail
}

/// Compile + measure `fixture` against the scalar-Vec control and assert the
/// fixture sits at the control's allocation floor (within `FLOOR_TOLERANCE`).
/// A pre-fix record-Vec leak puts the fixture >= `ITERATIONS` nodes above.
fn assert_no_record_vec_leak_over_control(shape_name: &str, fixture_source: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-record-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin = compile_to_native(&control_scalar_vec_source(), dir.path(), "control");
    let fixture_bin = compile_to_native(fixture_source, dir.path(), shape_name);

    let Some(control_leaks) = measure_leaks(&control_bin) else {
        return;
    };
    let Some(fixture_leaks) = measure_leaks(&fixture_bin) else {
        return;
    };

    eprintln!(
        "{shape_name}: control_leaks={control_leaks} fixture_leaks={fixture_leaks} \
         tolerance={FLOOR_TOLERANCE} iterations={ITERATIONS}"
    );
    assert!(
        fixture_leaks <= control_leaks + FLOOR_TOLERANCE,
        "{shape_name}: record-element Vec backing buffer LEAK -- the fixture leaked \
         {fixture_leaks} nodes against the scalar-Vec control's floor of {control_leaks} \
         (tolerance {FLOOR_TOLERANCE}). An excess of {} nodes means a `Vec<record>` built via \
         `hew_vec_new_with_layout` was not released at scope exit. The fix is the BitCopy \
         value-aggregate arm in `binding_ty_is_plain_vec` (hew-mir/src/lower.rs), which routes \
         such a Vec to `hew_vec_free`. Re-run with `MallocStackLogging=1 leaks --atExit -- {}` \
         to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

// -- oracles -----------------------------------------------------------------

/// `let pts: Vec<Point> = Vec::new(); pts.push(..)` -- the empty+pushed record
/// Vec. Pre-fix its backing buffer leaked once per helper call; post-fix it
/// sits at the scalar control floor. Reverting the plain-vec admission arm
/// fails this by >= `ITERATIONS` nodes.
#[test]
fn vec_record_new_push_no_leak() {
    assert_no_record_vec_leak_over_control("vec_record_new_push", &vec_record_new_push_source());
}

/// `[Point { .. }]` array literal of a `BitCopy` record -- the same
/// `hew_vec_new_with_layout` backing buffer must be released at scope exit.
#[test]
fn vec_record_array_literal_no_leak() {
    assert_no_record_vec_leak_over_control(
        "vec_record_array_literal",
        &vec_record_array_literal_source(),
    );
}

/// `[Point { .. }; 3]` array-repeat of a `BitCopy` record -- the array-repeat
/// gate's own leak-clean shape. The cloned 3-slot buffer must be released once.
#[test]
fn vec_record_array_repeat_no_leak() {
    assert_no_record_vec_leak_over_control(
        "vec_record_array_repeat",
        &vec_record_array_repeat_source(),
    );
}

/// No-double-free pin: the record-Vec free must run EXACTLY once and must not
/// over-drop the inline `BitCopy` elements; the co-resident `Vec<string>`
/// (CAP-09) and scalar paths must stay clean. A double-free or wrong-ABI free
/// crashes under the poisoned-allocator triple before the sentinel prints.
/// Runs on any unix.
#[test]
fn vec_record_release_is_exactly_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-record-no-double-free-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(NO_DOUBLE_FREE_SOURCE, dir.path(), "no_double_free");

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run no-double-free binary");

    assert!(
        output.status.success(),
        "record-Vec release must run exactly once -- a crash here indicates a double-free of \
         the backing buffer or a wrong-ABI free that walked the inline BitCopy elements as if \
         they owned heap;\n{}",
        describe_output(&output)
    );
    // p.x(10) + p.y(20) + lit.len()(2) + words.len()(1) + ns[0](7) = 40,
    // followed by the `OK` sentinel.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "40OK",
        "output must be untouched -- a scribbled value indicates an over-drop corrupted a live \
         value;\n{}",
        describe_output(&output)
    );
}
