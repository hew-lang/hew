//! Constant-leak / double-free oracle for the forwarded-fn-parameter →
//! struct-field store shape (`fn make_handler(f: fn(i64)->i64) -> Handler {
//! Handler { action: f } }`).
//!
//! ## What this proves
//!
//! A closure literal passed as a call argument is `Escapes`-classified by the
//! checker, so its env is heap-boxed at the literal site before it crosses the
//! call boundary into the `f` parameter. Storing `f` into an owning record
//! field transfers that heap env into the record: the record becomes the sole
//! owner and frees the env EXACTLY once at its own drop. The parameter's own
//! scope-exit drop is suppressed (its local is read by the `RecordInit`
//! ingress, so it is marked aliased and excluded from the kept-drop set).
//!
//! The failure modes this oracle catches:
//!   * a SECOND owner (the parameter AND the record both freeing the env) —
//!     a double-free that crashes under `MallocScribble`/`MallocGuardEdges`;
//!   * a LEAK (neither owner freeing the env) — a `leaks` node above the
//!     control floor;
//!   * an OVER-DROP corrupting a live value — a scribbled output.
//!
//! ## Methodology: absolute count against a no-store control
//!
//! Each fixture is measured under `leaks --atExit` with the poisoned-allocator
//! triple, and its absolute leak-node count is compared against a CONTROL
//! program that allocates the SAME closure env but does NOT store it into a
//! struct — it builds the adder, invokes it locally, and lets the owned `let`
//! binding drop at function scope (the already-correct closure-pair drop path).
//! The control establishes the runtime's one-alloc/one-free floor; a correct
//! field store must sit at that SAME floor. Pre-fix the store fixture cannot
//! even compile (`ClosurePairBorrowedStore`), so this oracle is also the
//! executable spec for the lift; post-fix it must match the control within a
//! tiny tolerance for scheduler/runtime jitter.
//!
//! This is the floor-EQUALITY assertion the LESSONS `assert-distinguishes-
//! garbage` rule calls for, not a happy-path `> 0`.
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

/// Permitted leak-node delta between a test fixture and the no-store control.
/// A correct field store frees the env exactly once, matching the control's
/// floor exactly; the tolerance of 1 absorbs the +/-1 scheduler/runtime
/// one-off jitter the sibling `vec_string_index_compare_leak_oracle`
/// documents. A pre-fix double-owner would crash (caught by the no-double-free
/// pin) and a leak would stand well above this floor.
const FLOOR_TOLERANCE: usize = 1;

// -- fixtures ----------------------------------------------------------------

/// Control: build the SAME closure env (`make_adder(7)`), bind it to an owned
/// `let`, invoke it locally, and let the binding drop at function scope. This
/// is the already-correct closure-pair drop path: one heap env allocated, one
/// freed. It establishes the zero-leak allocation floor the store fixtures are
/// measured against — NO struct field store is involved.
const CONTROL_NO_STORE_SOURCE: &str = "\
fn make_adder(n: i64) -> fn(i64) -> i64 {\n\
\x20   |x: i64| x + n\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let f = make_adder(7);\n\
\x20   let r = f(35);\n\
\x20   if r != 42 { return 80; }\n\
\x20   0\n\
}\n";

/// Test: the D16 pattern. Forward the `f` parameter into a record field, then
/// invoke the stored closure through the field. The record owns the heap env
/// and frees it once at its drop; `f`'s own scope-exit drop is suppressed.
/// Same one-alloc/one-free env count as the control — no second owner, no
/// leak.
const FORWARD_PARAM_INTO_FIELD_SOURCE: &str = "\
type Handler {\n\
\x20   action: fn(i64) -> i64;\n\
}\n\
\n\
fn make_adder(n: i64) -> fn(i64) -> i64 {\n\
\x20   |x: i64| x + n\n\
}\n\
\n\
fn make_handler(f: fn(i64) -> i64) -> Handler {\n\
\x20   Handler { action: f }\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let h = make_handler(make_adder(7));\n\
\x20   let r = h.action(35);\n\
\x20   if r != 42 { return 81; }\n\
\x20   0\n\
}\n";

/// Test: the struct holding the closure drops on a NORMAL function exit
/// (`cleanup-all-exits`). The Handler is built, used, and goes out of scope at
/// the tail of `use_then_drop`; its env-drop must fire on that exit, freeing
/// the env exactly once. Same env floor as the control.
const STORE_AND_DROP_STRUCT_SOURCE: &str = "\
type Handler {\n\
\x20   action: fn(i64) -> i64;\n\
}\n\
\n\
fn make_adder(n: i64) -> fn(i64) -> i64 {\n\
\x20   |x: i64| x + n\n\
}\n\
\n\
fn use_then_drop(f: fn(i64) -> i64) -> i64 {\n\
\x20   let h = Handler { action: f };\n\
\x20   h.action(35)\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let r = use_then_drop(make_adder(7));\n\
\x20   if r != 42 { return 82; }\n\
\x20   0\n\
}\n";

// -- leak measurement plumbing (same shape as the sibling leak oracles) ------

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

/// Compile + measure `fixture` against the no-store control and assert the
/// fixture sits at the control's allocation floor (within `FLOOR_TOLERANCE`).
/// A leaked env puts the fixture above the floor; a double-free crashes the
/// fixture under the poisoned allocator before `leaks` reports.
fn assert_env_freed_once_at_floor(shape_name: &str, fixture_source: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("closure-in-struct-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin = compile_to_native(CONTROL_NO_STORE_SOURCE, dir.path(), "control");
    let fixture_bin = compile_to_native(fixture_source, dir.path(), shape_name);

    let Some(control_leaks) = measure_leaks(&control_bin) else {
        return;
    };
    let Some(fixture_leaks) = measure_leaks(&fixture_bin) else {
        return;
    };

    eprintln!(
        "{shape_name}: control_leaks={control_leaks} fixture_leaks={fixture_leaks} \
         tolerance={FLOOR_TOLERANCE}"
    );
    assert!(
        fixture_leaks <= control_leaks + FLOOR_TOLERANCE,
        "{shape_name}: closure env LEAKED -- the field-store fixture leaked {fixture_leaks} \
         nodes against the no-store control's floor of {control_leaks} (tolerance \
         {FLOOR_TOLERANCE}). An excess of {} nodes means the forwarded-param closure env was \
         stored into the record but neither the record nor the parameter freed it. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

// -- oracles -----------------------------------------------------------------

/// The D16 pattern: forward the `f` parameter into a record field and invoke
/// through the field. The record owns and frees the heap env exactly once;
/// the parameter's own drop is suppressed. Floor-equal with the no-store
/// control. A second owner (param + record both freeing) crashes under the
/// no-double-free pin below; a leak fails this floor assertion.
#[test]
fn forward_param_into_field_env_freed_once() {
    assert_env_freed_once_at_floor("forward_param_into_field", FORWARD_PARAM_INTO_FIELD_SOURCE);
}

/// A struct holding the forwarded closure drops on a normal function exit
/// (`cleanup-all-exits`); its env-drop fires on that exit, freeing the env
/// exactly once. Floor-equal with the no-store control.
#[test]
fn store_and_drop_struct_env_freed_once() {
    assert_env_freed_once_at_floor("store_and_drop_struct", STORE_AND_DROP_STRUCT_SOURCE);
}

/// No-double-free pin: the forwarded-param field store must free the closure
/// env EXACTLY once. A second owner (the parameter freeing the same env the
/// record owns) is a double-free that crashes under `MallocScribble`/
/// `MallocGuardEdges` before the result prints. Runs on any unix host.
#[test]
fn forward_param_into_field_freed_exactly_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("closure-in-struct-no-double-free-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        FORWARD_PARAM_INTO_FIELD_SOURCE,
        dir.path(),
        "no_double_free",
    );

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run no-double-free binary");

    assert!(
        output.status.success(),
        "forwarded-param field store must free the closure env exactly once -- a crash here \
         indicates a double-free: the parameter freed the same heap env the record now owns. \
         The record must be the sole owner (the parameter's scope-exit drop is suppressed by \
         the aliased-local scan in `derive_closure_pair_drop_allowed`);\n{}",
        describe_output(&output)
    );
    // `main` returns 0 on the correct 35 + 7 = 42 path; a non-zero exit means
    // the stored closure dispatched the wrong value through the field.
    assert_eq!(
        output.status.code(),
        Some(0),
        "stored closure must dispatch 35 + 7 = 42 through the field; a non-zero exit means a \
         miscomputed dispatch or a scribbled env corrupted the captured `n`;\n{}",
        describe_output(&output)
    );
}
