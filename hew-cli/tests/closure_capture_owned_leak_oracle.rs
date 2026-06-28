//! Constant-leak / double-free oracle for the closure-env KEYSTONE: an
//! escaping (heap-boxed) closure that captures an OWNED value (`string`,
//! `Vec`) must free that captured value EXACTLY once when the closure is
//! dropped.
//!
//! ## What this proves
//!
//! A closure that escapes its introducing scope (returned, stored, passed to a
//! higher-order callee) has its capture environment heap-boxed at the literal
//! site: `[free_thunk: ptr][captures...]`. The captured owning value's handle
//! is byte-copied into the captures region and the caller's own binding drop is
//! suppressed (its local is read by the env `RecordInit` ingress → marked
//! aliased → excluded from the kept-drop set), so the env becomes the SOLE
//! owner of that handle.
//!
//! Before the keystone, the per-closure free thunk freed ONLY the box
//! (`hew_dyn_box_free`) and never released the captured owning handle — every
//! escaping closure that captured a runtime-built `string`/`Vec` leaked it (2
//! leaks under `leaks --atExit`). The keystone wires the free thunk to drop
//! each owned captured field through the canonical per-field drop authority
//! (`emit_field_drop_step`) BEFORE freeing the box, so the captured value is
//! released exactly once as the env's sole owner.
//!
//! The failure modes this oracle catches:
//!   * a LEAK (the env frees the box but not the captured handle) — a `leaks`
//!     node above the control floor (the pre-keystone bug);
//!   * a SECOND owner (the caller binding AND the env both freeing the handle)
//!     — a double-free that crashes under `MallocScribble`/`MallocGuardEdges`;
//!   * an OVER-DROP corrupting a live value — a scribbled output / non-zero
//!     exit.
//!
//! ## Methodology: absolute count against a no-capture control
//!
//! Each capturing fixture is measured under `leaks --atExit` with the
//! poisoned-allocator triple and its absolute leak-node count is compared
//! against a CONTROL program that builds the SAME owned value but does NOT
//! capture it into an escaping closure — it builds the value, reads it, and
//! lets the owned `let` binding drop at function scope (the already-correct
//! local owned-value drop path). The control establishes the runtime's
//! one-alloc/one-free floor; a correct escaping capture must sit at that SAME
//! floor. Pre-keystone the capturing fixture sits ABOVE the floor (the leaked
//! handle); post-keystone it matches within a tiny scheduler/runtime jitter
//! tolerance.
//!
//! This is the floor-EQUALITY assertion the LESSONS `assert-distinguishes-
//! garbage` / `drop-allowset-from-value-flow` rules call for, not a happy-path
//! `> 0`.
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

/// Permitted leak-node delta between a capturing fixture and the no-capture
/// control. A correct escaping capture frees the owned handle exactly once,
/// matching the control's floor; the tolerance of 1 absorbs the +/-1
/// scheduler/runtime one-off jitter the sibling leak oracles document. A
/// pre-keystone leak stands one or more nodes above this floor.
const FLOOR_TOLERANCE: usize = 1;

// -- fixtures ----------------------------------------------------------------

/// Control (string): build a runtime-concatenated owned `string`, read it, and
/// let the owned `let` binding drop at function scope. No closure capture is
/// involved — this is the already-correct local string drop path. It
/// establishes the one-alloc/one-free floor the capturing fixture is measured
/// against.
const CONTROL_STRING_NO_CAPTURE_SOURCE: &str = "\
fn build(n: i64) -> i64 {\n\
\x20   let label = \"row:\" + to_string(n);\n\
\x20   label.len()\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let r = build(7);\n\
\x20   if r != 5 { return 80; }\n\
\x20   0\n\
}\n";

/// Test (string): a returned closure captures the runtime-built owned
/// `string`. The closure escapes `make_label`, so its env is heap-boxed and
/// the captured `label` handle is owned by the env. `make_label`'s own `label`
/// drop is suppressed; the env's free thunk must release the captured `string`
/// exactly once when the returned closure is dropped in `main`. Same
/// one-alloc/one-free floor as the control.
const RETURN_CLOSURE_CAPTURES_STRING_SOURCE: &str = "\
fn make_label(n: i64) -> fn() -> i64 {\n\
\x20   let label = \"row:\" + to_string(n);\n\
\x20   || label.len()\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let f = make_label(7);\n\
\x20   let r = f();\n\
\x20   if r != 5 { return 81; }\n\
\x20   0\n\
}\n";

/// Control (Vec): build an owned `Vec<i64>`, read its length, and let the owned
/// `let` binding drop at function scope. The already-correct local owned-Vec
/// drop path; establishes the floor for the Vec-capture fixture.
const CONTROL_VEC_NO_CAPTURE_SOURCE: &str = "\
fn build() -> i64 {\n\
\x20   var xs: Vec<i64> = Vec::new();\n\
\x20   xs.push(10);\n\
\x20   xs.push(20);\n\
\x20   xs.len()\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let r = build();\n\
\x20   if r != 2 { return 82; }\n\
\x20   0\n\
}\n";

/// Test (Vec): a returned closure captures the owned `Vec<i64>`. The closure
/// escapes, its env is heap-boxed, and the captured `xs` handle is owned by the
/// env. The free thunk must release the captured `Vec` exactly once at the
/// returned closure's drop. Same floor as the control.
const RETURN_CLOSURE_CAPTURES_VEC_SOURCE: &str = "\
fn make_counter() -> fn() -> i64 {\n\
\x20   var xs: Vec<i64> = Vec::new();\n\
\x20   xs.push(10);\n\
\x20   xs.push(20);\n\
\x20   || xs.len()\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let f = make_counter();\n\
\x20   let r = f();\n\
\x20   if r != 2 { return 83; }\n\
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

/// Compile + measure `fixture` against `control` and assert the fixture sits at
/// the control's allocation floor (within `FLOOR_TOLERANCE`). A leaked capture
/// puts the fixture above the floor; a double-free crashes the fixture under
/// the poisoned allocator before `leaks` reports.
fn assert_capture_freed_once_at_floor(shape_name: &str, control: &str, fixture: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("closure-capture-owned-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin = compile_to_native(control, dir.path(), "control");
    let fixture_bin = compile_to_native(fixture, dir.path(), shape_name);

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
        "{shape_name}: closure-captured owned value LEAKED -- the escaping-capture fixture \
         leaked {fixture_leaks} nodes against the no-capture control's floor of {control_leaks} \
         (tolerance {FLOOR_TOLERANCE}). An excess of {} nodes means the captured owning handle \
         was byte-copied into the heap env but the env free thunk freed only the box, never the \
         handle. Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

/// Run `source` to native, execute under the poisoned-allocator triple, and
/// assert exit 0. A crash here is a double-free (the caller binding AND the env
/// both freeing the captured handle); a non-zero exit is a miscomputed read off
/// a scribbled capture.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!(
            "closure-capture-owned-no-double-free-{shape_name}-"
        ))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run no-double-free binary");

    assert!(
        output.status.success(),
        "{shape_name}: escaping closure capture must free the owned value exactly once -- a \
         crash here indicates a double-free: the caller binding freed the same handle the env \
         now owns. The env must be the sole owner (the caller's scope-exit drop is suppressed by \
         the aliased-local scan);\n{}",
        describe_output(&output)
    );
    assert_eq!(
        output.status.code(),
        Some(0),
        "{shape_name}: the captured value must read back correctly through the closure; a \
         non-zero exit means a scribbled capture corrupted the read;\n{}",
        describe_output(&output)
    );
}

// -- oracles -----------------------------------------------------------------

/// Keystone (string): a returned closure capturing a runtime-built owned
/// `string` frees the captured handle exactly once at the closure's drop.
/// Floor-equal with the no-capture control. Pre-keystone this leaked the
/// captured string (the free thunk freed only the box).
#[test]
fn return_closure_captures_string_freed_once() {
    assert_capture_freed_once_at_floor(
        "return_closure_captures_string",
        CONTROL_STRING_NO_CAPTURE_SOURCE,
        RETURN_CLOSURE_CAPTURES_STRING_SOURCE,
    );
}

/// Keystone (Vec): a returned closure capturing an owned `Vec<i64>` frees the
/// captured handle exactly once at the closure's drop. Floor-equal with the
/// no-capture control. Pre-keystone this leaked the captured Vec.
#[test]
fn return_closure_captures_vec_freed_once() {
    assert_capture_freed_once_at_floor(
        "return_closure_captures_vec",
        CONTROL_VEC_NO_CAPTURE_SOURCE,
        RETURN_CLOSURE_CAPTURES_VEC_SOURCE,
    );
}

/// No-double-free pin (string): the escaping capture frees the owned `string`
/// EXACTLY once. A second owner crashes under `MallocScribble`/
/// `MallocGuardEdges`. Runs on any unix host.
#[test]
fn return_closure_captures_string_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "string_no_double_free",
        RETURN_CLOSURE_CAPTURES_STRING_SOURCE,
    );
}

/// No-double-free pin (Vec): the escaping capture frees the owned `Vec` EXACTLY
/// once. Runs on any unix host.
#[test]
fn return_closure_captures_vec_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("vec_no_double_free", RETURN_CLOSURE_CAPTURES_VEC_SOURCE);
}
