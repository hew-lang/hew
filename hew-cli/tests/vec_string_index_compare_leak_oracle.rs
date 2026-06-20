//! Constant-leak oracle for the `Vec<string>` index-temp-into-string-compare
//! shape and the owned array-repeat (`["x"; N]`) clone shape.
//!
//! ## Why a second oracle (the gap the slope oracle structurally misses)
//!
//! `vec_local_drop_leak_oracle.rs` measures a per-FRAME leak SLOPE: it
//! compiles the same shape at a low and a high loop count and asserts the
//! leak-node DELTA stays flat. That methodology catches per-iteration growth
//! but is blind to a CONSTANT leak that does not scale with the loop — and
//! string indexing into a comparison was exactly such a constant leak.
//!
//! `xs[i]` over a `Vec<string>` lowers to `hew_vec_get_str`, which returns a
//! FRESH `+1`-retained owner the caller must release with `hew_string_drop`
//! (`hew-runtime/src/vec.rs`). When that temp is an OPERAND to a string
//! compare (`xs[i] == "hello"`, `!=`, and the `< <= > >=` ordering family),
//! the compare lowers to `Instr::IntCmp` whose string operands codegen routes
//! through `hew_string_equals` / `hew_string_compare` — pure `strcmp` borrows
//! that never free the operand. The retained temp was therefore never
//! dropped: one leaked node (32 bytes) per indexed compare, a fixed count
//! independent of any enclosing loop, so the slope oracle reported flat.
//!
//! The fix is the single-instruction-use admission arm in
//! `nested_fresh_string_temp_drop` (`hew-mir/src/lower.rs`), which now splices
//! one inline `hew_string_drop` after a borrowing string-compare that consumes
//! a fresh `hew_vec_get_str` temp.
//!
//! ## Methodology: absolute count against a no-index control
//!
//! Each fixture is measured under `leaks --atExit` with the poisoned-allocator
//! triple, and its absolute leak-node count is compared against a CONTROL
//! program that performs the same allocations WITHOUT the leaking
//! index-into-compare (it binds `xs[i]` to a named local, whose scope-exit
//! drop is already correct, then compares the local). The control establishes
//! the runtime's one-off allocation floor; the test program must sit at that
//! same floor. Pre-fix the index-into-compare fixture lands `count`-elements
//! ABOVE the control (one leaked retain per compare); post-fix they match
//! within a tiny tolerance for scheduler/runtime jitter.
//!
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

use std::fmt::Write as _;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Number of indexed-compare operations the leaking fixtures perform. Pre-fix
/// each one leaked exactly one retained `hew_vec_get_str` node, so the fixture
/// stood this many nodes above the control. Picked > the jitter tolerance so a
/// regression of even a single un-dropped compare is unambiguous.
const COMPARE_COUNT: usize = 8;

/// Permitted leak-node delta between a test fixture and the no-index control.
/// Each indexed compare reads a DISTINCT element buffer, so the pre-fix leak
/// scales 1 node per compare (`COMPARE_COUNT` nodes above the control floor);
/// post-fix the fixture matches the control exactly. The tolerance of 1
/// absorbs the +/-1 scheduler/runtime one-off jitter the sibling slope oracle
/// documents while staying far below `COMPARE_COUNT`.
const FLOOR_TOLERANCE: usize = 1;

// -- fixtures ----------------------------------------------------------------

/// Control: push `COMPARE_COUNT` DISTINCT strings into a `Vec<string>`, then
/// for each index bind `xs[i]` to a NAMED local and compare the local. The
/// named local is a function-scope owned `string` whose scope-exit drop is
/// already correct, so this program has no index-temp leak -- it establishes
/// the zero-leak allocation floor the fixtures are measured against.
fn control_named_local_source() -> String {
    let mut body = String::new();
    body.push_str("fn main() -> i64 {\n");
    body.push_str("    let xs: Vec<string> = Vec::new();\n");
    for i in 0..COMPARE_COUNT {
        // Each element is a DISTINCT buffer so a leaked retain would be a
        // DISTINCT `leaks` node (the fixtures rely on the same distinctness).
        let _ = writeln!(body, "    xs.push(\"elem-{i}\");");
    }
    body.push_str("    var hits: i64 = 0;\n");
    for i in 0..COMPARE_COUNT {
        // Bind the indexed element to a named local, then compare the local.
        // The named local drops correctly at function scope -- no temp leak.
        let _ = writeln!(body, "    let s{i} = xs[{i}];");
        let _ = writeln!(body, "    if s{i} == \"elem-{i}\" {{ hits = hits + 1; }}");
    }
    let _ = writeln!(body, "    if hits != {COMPARE_COUNT} {{ return 80; }}");
    body.push_str("    0\n}\n");
    body
}

/// Test: the leaking shape. Compare `xs[i]` DIRECTLY against a literal in a
/// boolean condition -- the `hew_vec_get_str` temp flows straight into the
/// `IntCmp` with no named binding. Each index is a DISTINCT element buffer, so
/// pre-fix each leaked a DISTINCT retained node; post-fix the inline
/// `hew_string_drop` balances each.
fn index_eq_compare_source() -> String {
    let mut body = String::new();
    body.push_str("fn main() -> i64 {\n");
    body.push_str("    let xs: Vec<string> = Vec::new();\n");
    for i in 0..COMPARE_COUNT {
        let _ = writeln!(body, "    xs.push(\"elem-{i}\");");
    }
    body.push_str("    var hits: i64 = 0;\n");
    for i in 0..COMPARE_COUNT {
        let _ = writeln!(
            body,
            "    if xs[{i}] == \"elem-{i}\" {{ hits = hits + 1; }}"
        );
    }
    let _ = writeln!(body, "    if hits != {COMPARE_COUNT} {{ return 81; }}");
    body.push_str("    0\n}\n");
    body
}

/// Test: the `!=` and `< <= > >=` ordering family flow through the same
/// borrowing `IntCmp` codegen path, so a fresh `xs[i]` temp into any of them
/// leaked identically pre-fix. Mixing the predicates pins that the admission
/// covers the whole borrowing-string-compare family, not just `==`. Each index
/// is a DISTINCT element buffer.
fn index_mixed_compare_source() -> String {
    let mut body = String::new();
    body.push_str("fn main() -> i64 {\n");
    body.push_str("    let xs: Vec<string> = Vec::new();\n");
    for i in 0..COMPARE_COUNT {
        let _ = writeln!(body, "    xs.push(\"elem-{i}\");");
    }
    body.push_str("    var hits: i64 = 0;\n");
    for i in 0..COMPARE_COUNT {
        let op = match i % 6 {
            0 => "!=",
            1 => "<",
            2 => "<=",
            3 => ">",
            4 => ">=",
            _ => "==",
        };
        let _ = writeln!(body, "    if xs[{i}] {op} \"zzz\" {{ hits = hits + 1; }}");
    }
    // Only the comparisons true at runtime increment; we don't pin the exact
    // count (it depends on the predicate mix), just that the program runs and
    // exits 0. The leak count is what this oracle asserts.
    body.push_str("    if hits < 0 { return 82; }\n");
    body.push_str("    0\n}\n");
    body
}

/// Owned array-repeat: `["hello"; N]` must produce N independent string clones
/// and release every one at scope exit -- the lane's own leak-clean gate.
/// Array-repeat CLONES the element, so each slot is an INDEPENDENT buffer even
/// though every value is "hello"; a leaked index-into-compare retain is
/// therefore a DISTINCT `leaks` node per slot. Comparing every slot makes the
/// pre-fix leak scale to `COMPARE_COUNT` so this fixture has real teeth.
fn array_repeat_string_source() -> String {
    let mut body = String::new();
    body.push_str("fn main() -> i64 {\n");
    let _ = writeln!(body, "    let xs = [\"hello\"; {COMPARE_COUNT}];");
    let _ = writeln!(body, "    if xs.len() != {COMPARE_COUNT} {{ return 83; }}");
    body.push_str("    var hits: i64 = 0;\n");
    for i in 0..COMPARE_COUNT {
        let _ = writeln!(body, "    if xs[{i}] == \"hello\" {{ hits = hits + 1; }}");
    }
    let _ = writeln!(body, "    if hits != {COMPARE_COUNT} {{ return 84; }}");
    body.push_str("    0\n}\n");
    body
}

/// No-double-free pin: the index-into-compare shape plus a scalar
/// array-repeat plus a `println(xs[i])` (the print path drops its own temp).
/// The fix must release the compare temp EXACTLY once: a double-free of the
/// retained temp, or a free of the Vec's own element, crashes under
/// `MallocScribble`/`MallocGuardEdges` before the checksum prints.
const NO_DOUBLE_FREE_SOURCE: &str = "\
fn main() {\n\
\x20   let xs: Vec<string> = Vec::new();\n\
\x20   xs.push(\"alpha\");\n\
\x20   var hits: i64 = 0;\n\
\x20   if xs[0] == \"alpha\" { hits = hits + 1; }\n\
\x20   if xs[0] != \"beta\" { hits = hits + 1; }\n\
\x20   println(xs[0]);\n\
\x20   let ns = [7; 3];\n\
\x20   hits = hits + ns[0];\n\
\x20   print(hits);\n\
\x20   print(\"OK\");\n\
}\n";

// -- leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ---

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

/// Compile + measure `fixture` against the no-index control and assert the
/// fixture sits at the control's allocation floor (within `FLOOR_TOLERANCE`).
/// A pre-fix index-temp leak puts the fixture `COMPARE_COUNT` nodes above.
fn assert_no_constant_leak_over_control(shape_name: &str, fixture_source: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-str-cmp-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin = compile_to_native(&control_named_local_source(), dir.path(), "control");
    let fixture_bin = compile_to_native(fixture_source, dir.path(), shape_name);

    let Some(control_leaks) = measure_leaks(&control_bin) else {
        return;
    };
    let Some(fixture_leaks) = measure_leaks(&fixture_bin) else {
        return;
    };

    eprintln!(
        "{shape_name}: control_leaks={control_leaks} fixture_leaks={fixture_leaks} \
         tolerance={FLOOR_TOLERANCE} compares={COMPARE_COUNT}"
    );
    assert!(
        fixture_leaks <= control_leaks + FLOOR_TOLERANCE,
        "{shape_name}: CONSTANT index-temp leak -- the fixture leaked {fixture_leaks} nodes \
         against the no-index control's floor of {control_leaks} (tolerance {FLOOR_TOLERANCE}). \
         An excess of {} nodes means a fresh `hew_vec_get_str` temp fed into a string compare \
         was not released. The fix is the borrowing-string-compare admission arm in \
         `nested_fresh_string_temp_drop` (hew-mir/src/lower.rs). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

// -- oracles -----------------------------------------------------------------

/// `xs[0] == "needle"` -- the primary leaking shape. Pre-fix this stood
/// `COMPARE_COUNT` nodes above the no-index control; post-fix it sits at the
/// floor. Reverting the admission arm fails this by ~`COMPARE_COUNT` nodes.
#[test]
fn vec_string_index_eq_compare_no_constant_leak() {
    assert_no_constant_leak_over_control("index_eq_compare", &index_eq_compare_source());
}

/// `!=` and the `< <= > >=` ordering family flow through the same borrowing
/// `IntCmp` codegen path; a fresh `xs[0]` temp into any of them must be
/// released. Pins the admission covers the whole compare family.
#[test]
fn vec_string_index_mixed_compare_no_constant_leak() {
    assert_no_constant_leak_over_control("index_mixed_compare", &index_mixed_compare_source());
}

/// Owned array-repeat `["hello"; 8]` must release every cloned element at
/// scope exit -- the lane's leak-clean gate. Compared against the same
/// no-index control floor.
#[test]
fn array_repeat_string_clone_no_leak() {
    assert_no_constant_leak_over_control("array_repeat_string", &array_repeat_string_source());
}

/// No-double-free pin: the index-into-compare temp must be released EXACTLY
/// once. A double-free of the retained temp (or a free of the Vec's own
/// element) crashes under the poisoned-allocator triple; the `println(xs[i])`
/// and scalar `[7;3]` paths must stay clean (no over-drop). Runs on any unix.
#[test]
fn index_compare_release_is_exactly_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-str-cmp-no-double-free-")
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
        "index-into-compare must release its temp exactly once -- a crash here \
         indicates a double-free of the retained `hew_vec_get_str` temp or a \
         free of the Vec's own element;\n{}",
        describe_output(&output)
    );
    // `println(xs[0])` prints `alpha\n`; then hits = 1 (==alpha) + 1 (!=beta)
    // + 7 (ns[0]) = 9, followed by the `OK` sentinel.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "alpha\n9OK",
        "output must be untouched -- a scribbled value indicates an over-drop \
         corrupted a live string;\n{}",
        describe_output(&output)
    );
}
