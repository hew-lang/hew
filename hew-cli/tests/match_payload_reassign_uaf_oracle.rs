//! Projected-payload move-out UAF / double-free oracle (issue #2523).
//!
//! An enum/machine payload projected out by a `match` arm binder is a
//! byte-copy ALIAS of the scrutinee's payload slot (`_v = move mvar.V.F`),
//! never an independent buffer. Moving that binder into a new owner
//! (`var w = v`) and then releasing the new owner (`w = seed()`, or the new
//! owner's scope-exit drop) frees storage the scrutinee's own composite
//! drop still owns — a double-free, and, if the scrutinee is re-read, a
//! use-after-free of the freed payload.
//!
//! The fix has two halves, both pinned here:
//!
//!   * **Move-neutralization (memory safety).** At the move-out the source
//!     payload slot is neutralized (nulled) so the scrutinee's null-tolerant
//!     composite drop no-ops and the new owner's release is the SOLE free of
//!     the buffer. Pinned by the poisoned-allocator runtime pins (a pre-fix
//!     double-free aborts under `MallocScribble`) and the leak-slope guard
//!     (the neutralize must not invert the UAF into a per-frame leak).
//!
//!   * **Consume agreement (fail-closed diagnostic).** Moving the projected
//!     binder consumes the scrutinee; a later re-read (e.g. the next
//!     iteration of the reporter's `while` loop) is rejected at compile time
//!     as a use-after-move rather than left to null-dereference at runtime
//!     (`hew_vec_len` is not null-tolerant). Pinned by the compile-fail
//!     fixture.
//!
//! The control fixture proves the link is gated strictly on a genuine
//! move-out: a read-only borrow (`v.len()`) of the projected payload keeps
//! the scrutinee live and the loop still yields its total.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{assert_frame_slope_below_tolerance, compile_to_native};
use support::{describe_output, hew_binary, repo_root, require_codegen};

/// `Vec<i64>` producer shared by every fixture — a fresh, solely-owned heap
/// buffer of length 3 per call.
const SEED_FN: &str = "\
fn seed() -> Vec<i64> {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(1);\n\
\x20   v.push(2);\n\
\x20   v.push(3);\n\
\x20   v\n\
}\n";

/// Compile a source and assert it is REJECTED with `expected` in the
/// diagnostic stream (the fail-closed use-after-move surface).
fn assert_compile_fails(shape_name: &str, source: &str, expected: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("payload-reassign-fail-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{shape_name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        !output.status.success(),
        "{shape_name}: expected a fail-closed use-after-move rejection, but compile \
         succeeded — the projected-payload move-out did not consume the scrutinee:\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains(expected),
        "{shape_name}: compile failed but did not mention `{expected}`:\n{combined}"
    );
}

/// Compile a source and run it under the poisoned-allocator triple (no
/// `leaks` dependency — works on any unix). Asserts a clean exit with
/// `expected_exit`. A pre-fix double-free of the aliased payload aborts under
/// `MallocScribble`; a use-after-free reads scribbled memory and miscomputes.
fn assert_scribbled_run_exit(shape_name: &str, source: &str, expected_exit: i32) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("payload-reassign-uaf-{shape_name}-"))
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
         triple. A projected-payload move-out that does not neutralize the source slot \
         double-frees the buffer (scrutinee composite drop + new owner release). \
         Output:\n{}",
        describe_output(&output)
    );
}

// ── fixture sources ─────────────────────────────────────────────────────────

/// The reporter's original shape (issue #2523): the projected payload is
/// moved into `w`, `w` is reassigned (freeing the moved buffer), and the
/// enum is re-matched on the next `while` iteration — a use-after-move that
/// MUST be rejected at compile time.
fn reread_loop_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match b {{\n\
         \x20           Box::Full(v) => {{\n\
         \x20               sum = sum + v.len();\n\
         \x20               var w = v;\n\
         \x20               w = seed();\n\
         \x20               sum = sum + w.len();\n\
         \x20           }}\n\
         \x20           Box::Empty => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   println(sum);\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// Move-out + reassign, single match, no re-read. `w = seed()` is the sole
/// free of the moved payload buffer; the scrutinee's scope-exit composite
/// drop must no-op on the neutralized slot. Pre-fix: double-free aborts.
/// Returns `3 + 3 = 6`.
fn move_reassign_no_reread_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box::Full(v) => {{\n\
         \x20           out = v.len();\n\
         \x20           var w = v;\n\
         \x20           w = seed();\n\
         \x20           out = out + w.len();\n\
         \x20       }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// Move-out with NO reassign, single match, no re-read. The new owner `w`
/// frees the moved buffer exactly once at its scope exit; the scrutinee's
/// composite drop must no-op on the neutralized slot. Pre-fix: double-free
/// aborts. Returns `3 + 3 = 6`.
fn move_scope_drop_no_reread_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box::Full(v) => {{\n\
         \x20           out = v.len();\n\
         \x20           let w = v;\n\
         \x20           out = out + w.len();\n\
         \x20       }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// Aggregate payload: the projected binder is a `(Vec<i64>, Vec<i64>)`
/// tuple; the neutralize must null BOTH heap leaves in the payload slot so
/// the scrutinee's composite drop double-frees neither. Move-out, no
/// re-read. Returns `3 + 3 + 3 = 9`.
fn aggregate_payload_no_reread_source() -> String {
    format!(
        "enum Pair {{\n\
         \x20   Both((Vec<i64>, Vec<i64>));\n\
         \x20   Neither;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Pair::Both((seed(), seed()));\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Pair::Both(v) => {{\n\
         \x20           out = v.0.len() + v.1.len();\n\
         \x20           let w = v;\n\
         \x20           out = out + w.0.len();\n\
         \x20       }}\n\
         \x20       Pair::Neither => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// Leak-slope guard: a FRESH scrutinee per iteration is projected,
/// moved-out, and reassigned — no cross-iteration re-read, so it compiles.
/// Every buffer is freed exactly once per frame (moved buffer by
/// `w = seed()`, the seed buffer by `w`'s scope drop, the scrutinee slot
/// neutralized). The neutralize must not invert the UAF into a per-frame
/// leak — the slope stays flat.
fn fresh_scrutinee_loop_source(frames: usize) -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var i = 0;\n\
         \x20   var acc = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Box::Full(seed());\n\
         \x20       match b {{\n\
         \x20           Box::Full(v) => {{\n\
         \x20               var w = v;\n\
         \x20               w = seed();\n\
         \x20               acc = acc + w.len();\n\
         \x20           }}\n\
         \x20           Box::Empty => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   acc\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// Control: a read-only borrow of the projected payload (`v.len()`) with NO
/// move-out. The scrutinee stays live, the loop re-matches legitimately, and
/// the total is exact. Guards against the consume-link over-firing on a
/// borrow (which would spuriously reject this idiomatic code). Returns
/// `3 * 5 = 15`.
fn borrow_only_loop_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match b {{\n\
         \x20           Box::Full(v) => {{\n\
         \x20               sum = sum + v.len();\n\
         \x20           }}\n\
         \x20           Box::Empty => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sum\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// (Consume agreement) The reporter's re-reading loop must be rejected at
/// compile time as a use-after-move, not left to null-dereference at runtime.
#[test]
fn reread_after_payload_move_is_use_after_move() {
    assert_compile_fails(
        "reread_loop",
        &reread_loop_source(),
        "used after it was consumed",
    );
}

/// (Memory safety) Move-out + reassign, no re-read: the moved buffer is freed
/// exactly once; the scrutinee slot is neutralized. No double-free.
#[test]
fn move_reassign_no_reread_single_free() {
    assert_scribbled_run_exit("move_reassign", &move_reassign_no_reread_source(), 6);
}

/// (Memory safety) Move-out with no reassign, no re-read: the new owner's
/// scope-exit drop is the sole free; the scrutinee slot is neutralized.
#[test]
fn move_scope_drop_no_reread_single_free() {
    assert_scribbled_run_exit("move_scope_drop", &move_scope_drop_no_reread_source(), 6);
}

/// (Memory safety, aggregate) An aggregate-tuple payload move-out neutralizes
/// BOTH heap leaves — the scrutinee composite drop double-frees neither.
#[test]
fn aggregate_payload_move_out_single_free() {
    assert_scribbled_run_exit(
        "aggregate_payload",
        &aggregate_payload_no_reread_source(),
        9,
    );
}

/// (Leak-slope) The neutralize must not invert the UAF into a per-frame leak.
#[test]
fn fresh_scrutinee_move_reassign_flat_leak_slope() {
    assert_frame_slope_below_tolerance("payload_reassign_fresh", fresh_scrutinee_loop_source);
}

/// (Over-fire guard) A read-only borrow of the projected payload keeps the
/// scrutinee live — the loop still re-matches and totals correctly.
#[test]
fn borrow_only_projection_keeps_scrutinee_live() {
    assert_scribbled_run_exit("borrow_only", &borrow_only_loop_source(), 15);
}
