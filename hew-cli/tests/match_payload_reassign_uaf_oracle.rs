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

// ── #2523 F1: re-readable *place* scrutinees ────────────────────────────────
//
// A projected heap payload moved out of a re-readable PLACE scrutinee
// (`match h.b`, `match pair.0`, `match o.inner.b`, `match self.field`) is a
// silent same-root-cause use-after-free: the match COPIES the place into a
// temp, so nulling the temp cannot reach the origin field's storage — the
// moved-from field keeps a dangling pointer the new owner later frees. Actual
// physical neutralization of a copied place scrutinee is not soundly
// expressible, so the move-out is REJECTED fail-closed before codegen. These
// oracles pin the rejection (and prove the sound sibling paths are untouched).

/// A record-FIELD place scrutinee (`match h.b`) matched in a loop, projected
/// payload moved into `w`, `w` reassigned. Pre-fix: compiles clean and
/// segfaults on the loop re-read. MUST reject at compile time.
fn field_place_reread_loop_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         record Holder {{ b: Box, }}\n\
         \n\
         fn main() {{\n\
         \x20   let h = Holder {{ b: Box::Full(seed()) }};\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match h.b {{\n\
         \x20           Box::Full(v) => {{ sum = sum + v.len(); var w = v; w = seed(); }}\n\
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

/// A record-FIELD place scrutinee move-out with NO re-read (single match). The
/// temp-neutralize cannot reach `h.b`, so it leaks / risks a double-free at
/// `h`'s drop. Uniformly rejected fail-closed (a leak is not an acceptable
/// end state for a heap ownership transfer).
fn field_place_no_reread_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         record Holder {{ b: Box, }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let h = Holder {{ b: Box::Full(seed()) }};\n\
         \x20   var out = 0;\n\
         \x20   match h.b {{\n\
         \x20       Box::Full(v) => {{ out = v.len(); var w = v; w = seed(); out = out + w.len(); }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// A TUPLE-index place scrutinee (`match pair.0`) — same code path as a record
/// field. MUST reject.
fn tuple_place_reread_loop_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let pair = (Box::Full(seed()), 0);\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match pair.0 {{\n\
         \x20           Box::Full(v) => {{ sum = sum + v.len(); var w = v; w = seed(); }}\n\
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

/// A NESTED record-field place scrutinee (`match o.inner.b`) — the recursive
/// place-root predicate must see through the projection chain. MUST reject.
fn nested_field_place_reread_loop_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         record Inner {{ b: Box, }}\n\
         record Outer {{ inner: Inner, }}\n\
         \n\
         fn main() {{\n\
         \x20   let o = Outer {{ inner: Inner {{ b: Box::Full(seed()) }} }};\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match o.inner.b {{\n\
         \x20           Box::Full(v) => {{ sum = sum + v.len(); var w = v; w = seed(); }}\n\
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

/// A read-only borrow of a record-FIELD place scrutinee must stay valid — the
/// rejection fires only on a move-out (`Consume`), never on a borrow. The loop
/// re-matches `h.b` every iteration and totals `3 * 5 = 15`.
fn field_place_borrow_only_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         record Holder {{ b: Box, }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let h = Holder {{ b: Box::Full(seed()) }};\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match h.b {{\n\
         \x20           Box::Full(v) => {{ sum = sum + v.len(); }}\n\
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

/// An EPHEMERAL (call) scrutinee move-out (`match mk()`) is a fresh sole-owner
/// temp with no re-readable origin — it stays soundly neutralizable and must
/// keep compiling and free exactly once. Guards against over-rejecting the
/// ephemeral path when tightening the place path. Returns `3 + 3 = 6`.
fn call_scrutinee_move_reassign_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         fn mk() -> Box {{ Box::Full(seed()) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var out = 0;\n\
         \x20   match mk() {{\n\
         \x20       Box::Full(v) => {{ out = v.len(); var w = v; w = seed(); out = out + w.len(); }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F1 · use-after-free) A record-field place scrutinee move-out in a loop must
/// be rejected at compile time — not left to segfault on the re-read.
#[test]
fn field_place_move_out_reread_is_rejected() {
    assert_compile_fails(
        "field_place_reread",
        &field_place_reread_loop_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a re-readable place",
    );
}

/// (F1) A single-match field-place move-out (no re-read) is also rejected —
/// the temp-neutralize cannot reach `h.b`, so the transfer is unsound.
#[test]
fn field_place_move_out_no_reread_is_rejected() {
    assert_compile_fails(
        "field_place_no_reread",
        &field_place_no_reread_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a re-readable place",
    );
}

/// (F1) A tuple-index place scrutinee move-out is rejected (same code path).
#[test]
fn tuple_place_move_out_is_rejected() {
    assert_compile_fails(
        "tuple_place_reread",
        &tuple_place_reread_loop_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a re-readable place",
    );
}

/// (F1) A nested record-field place scrutinee move-out is rejected — the
/// place-root predicate sees through the projection chain.
#[test]
fn nested_field_place_move_out_is_rejected() {
    assert_compile_fails(
        "nested_field_place",
        &nested_field_place_reread_loop_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a re-readable place",
    );
}

/// (F1 · over-fire guard) A read-only borrow of a field-place scrutinee stays
/// valid — the rejection fires only on a move-out, never a borrow.
#[test]
fn field_place_borrow_only_keeps_scrutinee_live() {
    assert_scribbled_run_exit("field_place_borrow", &field_place_borrow_only_source(), 15);
}

/// (F1 · over-fire guard) An ephemeral (call) scrutinee move-out is NOT a
/// re-readable place — it stays soundly neutralizable, compiles, and frees
/// exactly once under the poisoned-allocator triple.
#[test]
fn call_scrutinee_move_reassign_single_free() {
    assert_scribbled_run_exit("call_scrutinee", &call_scrutinee_move_reassign_source(), 6);
}

// ── #2523 F1b: default-deny — wrapper-hidden / non-enumerated place scrutinees ─
//
// The F1 fix classified by an allowlist of REJECT shapes and defaulted every
// other shape to the sound-only temp-neutralize — a fail-OPEN direction. A
// place projection hidden behind a `Block` (`match { h.b }`) or `If`
// (`match if c { h.b } else { h.b }`) wrapper is not one of the enumerated
// place shapes, so it slipped through to the temp-neutralize and double-freed
// deterministically (5/5), identical mechanism to the direct `match h.b`. The
// classifier is now fail-CLOSED: only a bare owning binding or a *proven*
// ephemeral producer (call / constructor / literal / await) takes the
// neutralize path; every other shape — wrappers and any un-enumerated or future
// HIR shape — is REJECTED before codegen. These oracles pin that direction so a
// wrapper-hidden alias can never again reach the unsound neutralize.

/// F1b primary: a record-FIELD place hidden behind a `Block` wrapper
/// (`match { h.b }`). Pre-F1b: compiled clean and double-freed 5/5. MUST reject.
fn block_wrapped_field_place_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         record Holder {{ b: Box, }}\n\
         \n\
         fn main() {{\n\
         \x20   let h = Holder {{ b: Box::Full(seed()) }};\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match {{ h.b }} {{\n\
         \x20           Box::Full(v) => {{ sum = sum + v.len(); var w = v; w = seed(); }}\n\
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

/// F1b, second complex shape: a record-FIELD place behind an `If` wrapper
/// (`match if c { h.b } else { h.b }`). Same alias-carrying tail, a different
/// non-enumerated wrapper. MUST reject (fail-closed default).
fn if_wrapped_field_place_source() -> String {
    format!(
        "enum Box {{\n\
         \x20   Full(Vec<i64>);\n\
         \x20   Empty;\n\
         }}\n\
         record Holder {{ b: Box, }}\n\
         \n\
         fn main() {{\n\
         \x20   let h = Holder {{ b: Box::Full(seed()) }};\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match if i < 2 {{ h.b }} else {{ h.b }} {{\n\
         \x20           Box::Full(v) => {{ sum = sum + v.len(); var w = v; w = seed(); }}\n\
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

/// (F1b) A block-wrapped field-place scrutinee move-out is rejected — the
/// fail-closed default catches the wrapper the syntactic allowlist missed.
#[test]
fn block_wrapped_place_move_out_is_rejected() {
    assert_compile_fails(
        "block_wrapped_place",
        &block_wrapped_field_place_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a re-readable place",
    );
}

/// (F1b) An if-wrapped field-place scrutinee move-out is rejected — a second
/// non-enumerated wrapper shape, proving the default-deny is not `Block`-only.
#[test]
fn if_wrapped_place_move_out_is_rejected() {
    assert_compile_fails(
        "if_wrapped_place",
        &if_wrapped_field_place_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a re-readable place",
    );
}

// ── #2523 F2: exhaustive projection-ownership correction (nested / capture /
//     two independent fields) ─────────────────────────────────────────────────
//
// Three defects found by the codegen + cross reviews, one root cause (a
// projected heap payload moved out of a scrutinee whose real storage the
// temp-neutralize cannot reach, or a false use-after-consume on a valid
// second-field move):
//
//   1. NESTED binders (`Outer::Wrap(Inner::Full(v))`) are bound from a
//      TRANSIENT copy the predicate phase loads, not the outer value's real
//      nested slot; neutralizing the transient cannot reach it, so a heap
//      move-out double-frees / leaks. Now REJECTED fail-closed (the outer
//      value's storage is not expressible as a single `Place::MachineVariant`).
//      The enum and machine nested payloads share the identical
//      `nested_binding_jobs` / `Place::MachineVariant` seam, so this enum
//      coverage pins both.
//   2. A closure-CAPTURED binding (`match b` inside `|| { … }` that captures
//      `b`) is read from the closure environment by BYTE-COPY
//      (`ClosureEnvFieldLoad`), NOT moved into the temp; the captured copy
//      survives the move and double-frees when the env drops. Now REJECTED.
//   3. TWO independent heap fields moved in one arm
//      (`Both(x, y) => var wx = x; var wy = y;`) must both single-free — the
//      per-field partial-projection consume-mark keeps the aggregate re-read
//      forbidding without a false use-after-consume on the second move, and the
//      neutralized-transfer edges keep the two owners in separate move
//      components so neither scope-exit drop is wrongly stripped.

/// (F2 item 1) A NESTED enum payload move-out from an ephemeral scrutinee
/// (`match mk()`). The nested binder aliases a transient copy; the move-out is
/// rejected fail-closed. Pre-fix: compiled and leaked / double-freed.
fn nested_enum_move_out_source() -> String {
    format!(
        "enum Inner {{ Full(Vec<i64>); Hollow; }}\n\
         enum Outer {{ Wrap(Inner); Bare; }}\n\
         fn mk() -> Outer {{ Outer::Wrap(Inner::Full(seed())) }}\n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   match mk() {{\n\
         \x20       Outer::Wrap(Inner::Full(v)) => {{ var w = v; total = w.len(); w = seed(); total = total + w.len(); }}\n\
         \x20       Outer::Wrap(Inner::Hollow) => {{}}\n\
         \x20       Outer::Bare => {{}}\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 1) The same nested move-out from an owned BINDING scrutinee
/// (`match b`). Rejected fail-closed for the same reason: the nested slot is
/// reachable only through a transient copy the neutralize cannot null.
fn nested_enum_move_out_binding_source() -> String {
    format!(
        "enum Inner {{ Full(Vec<i64>); Hollow; }}\n\
         enum Outer {{ Wrap(Inner); Bare; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Outer::Wrap(Inner::Full(seed()));\n\
         \x20   var total = 0;\n\
         \x20   match b {{\n\
         \x20       Outer::Wrap(Inner::Full(v)) => {{ var w = v; total = w.len(); w = seed(); total = total + w.len(); }}\n\
         \x20       Outer::Wrap(Inner::Hollow) => {{}}\n\
         \x20       Outer::Bare => {{}}\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 1 control) A borrow-only NESTED destructure never reaches the
/// consume hook, so it stays valid: the loop reads the nested payload and
/// totals correctly with no double-free.
fn nested_enum_borrow_only_source() -> String {
    format!(
        "enum Inner {{ Full(Vec<i64>); Hollow; }}\n\
         enum Outer {{ Wrap(Inner); Bare; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Outer::Wrap(Inner::Full(seed()));\n\
         \x20   var total = 0;\n\
         \x20   match b {{\n\
         \x20       Outer::Wrap(Inner::Full(v)) => {{ total = v.len(); }}\n\
         \x20       Outer::Wrap(Inner::Hollow) => {{}}\n\
         \x20       Outer::Bare => {{}}\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 2) A closure-CAPTURED binding matched inside the closure, with a
/// projected-payload move-out. The binding is read from the closure env by
/// byte-copy, so the move-out is rejected fail-closed. Pre-fix: segfault /
/// double-free 3/3 under the poisoned allocator.
fn captured_binding_move_out_source() -> String {
    format!(
        "enum Box {{ Full(Vec<i64>); Empty; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var total = 0;\n\
         \x20   let f = || {{\n\
         \x20       match b {{\n\
         \x20           Box::Full(v) => {{ var w = v; total = w.len(); w = seed(); }}\n\
         \x20           Box::Empty => {{}}\n\
         \x20       }}\n\
         \x20   }};\n\
         \x20   f();\n\
         \x20   total\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 2 control) A closure-captured binding matched borrow-ONLY inside
/// the closure never hits the consume hook, so it stays valid: it compiles and
/// runs clean (the env-copy is read, never moved out).
fn captured_binding_borrow_only_source() -> String {
    format!(
        "enum Box {{ Full(Vec<i64>); Empty; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   let f = || -> i64 {{\n\
         \x20       match b {{\n\
         \x20           Box::Full(v) => v.len(),\n\
         \x20           Box::Empty => 0,\n\
         \x20       }}\n\
         \x20   }};\n\
         \x20   f()\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 3) TWO independent heap payload fields moved out of one arm, then
/// both reassigned. Each field single-frees: the moved payload is freed once at
/// its reassignment, the fresh owner once at scope exit. No false
/// use-after-consume on the second move; no leak. Returns `3 + 3 = 6`.
fn two_field_move_out_source() -> String {
    format!(
        "enum Pair {{ Both(Vec<i64>, Vec<i64>); Neither; }}\n\
         fn mk() -> Pair {{ Pair::Both(seed(), seed()) }}\n\
         fn main() -> i64 {{\n\
         \x20   var out = 0;\n\
         \x20   match mk() {{\n\
         \x20       Pair::Both(x, y) => {{\n\
         \x20           var wx = x; var wy = y;\n\
         \x20           wx = seed(); wy = seed();\n\
         \x20           out = wx.len() + wy.len();\n\
         \x20       }}\n\
         \x20       Pair::Neither => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 3) The two-field move from an owned BINDING scrutinee — the shape
/// the F1b consume-mark regressed into a false `E_MIR_CHECK: b is used after it
/// was consumed` on the SECOND field. Must compile and single-free. Returns 6.
fn two_field_move_out_binding_source() -> String {
    format!(
        "enum Pair {{ Both(Vec<i64>, Vec<i64>); Neither; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Pair::Both(seed(), seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Pair::Both(x, y) => {{\n\
         \x20           var wx = x; var wy = y;\n\
         \x20           wx = seed(); wy = seed();\n\
         \x20           out = wx.len() + wy.len();\n\
         \x20       }}\n\
         \x20       Pair::Neither => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 3, negative re-read) After BOTH fields are moved out, the aggregate
/// binding must not be re-readable: a second `match b` is rejected at compile
/// time as a use-after-move. Proves the per-field partial-projection mark still
/// forbids re-read (the consume state is not weakened).
fn two_field_reread_source() -> String {
    format!(
        "enum Pair {{ Both(Vec<i64>, Vec<i64>); Neither; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Pair::Both(seed(), seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Pair::Both(x, y) => {{ var wx = x; var wy = y; out = wx.len() + wy.len(); }}\n\
         \x20       Pair::Neither => {{}}\n\
         \x20   }}\n\
         \x20   match b {{\n\
         \x20       Pair::Both(x2, y2) => {{ out = out + x2.len() + y2.len(); }}\n\
         \x20       Pair::Neither => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 1) A nested projected-payload move-out from an ephemeral scrutinee
/// is rejected fail-closed with the nested-pattern diagnostic.
#[test]
fn nested_enum_move_out_is_rejected() {
    assert_compile_fails(
        "nested_enum_move",
        &nested_enum_move_out_source(),
        "cannot move the heap-owning payload `v` out of a nested `match` pattern",
    );
}

/// (F2 item 1) The same nested move-out from an owned binding scrutinee is also
/// rejected — the reject is on the nested-destructure shape, not the scrutinee
/// origin.
#[test]
fn nested_enum_move_out_binding_is_rejected() {
    assert_compile_fails(
        "nested_enum_move_binding",
        &nested_enum_move_out_binding_source(),
        "cannot move the heap-owning payload `v` out of a nested `match` pattern",
    );
}

/// (F2 item 1 control) A borrow-only nested destructure stays valid and frees
/// exactly once. Returns 3.
#[test]
fn nested_enum_borrow_only_is_valid() {
    assert_scribbled_run_exit("nested_enum_borrow", &nested_enum_borrow_only_source(), 3);
}

/// (F2 item 2) A projected-payload move-out of a closure-captured binding is
/// rejected fail-closed with the captured-binding diagnostic.
#[test]
fn captured_binding_move_out_is_rejected() {
    assert_compile_fails(
        "captured_binding_move",
        &captured_binding_move_out_source(),
        "cannot move the heap-owning payload `v` out of a `match` on a closure-captured binding",
    );
}

/// (F2 item 2 control) A borrow-only match on a captured binding stays valid —
/// no consume hook runs, so the env-copy is never moved out. Returns 3.
#[test]
fn captured_binding_borrow_only_is_valid() {
    assert_scribbled_run_exit(
        "captured_binding_borrow",
        &captured_binding_borrow_only_source(),
        3,
    );
}

/// (F2 item 3) Two independent heap fields moved and reassigned in one arm
/// single-free — no false use-after-consume, no leak. Ephemeral scrutinee.
#[test]
fn two_field_move_out_single_free() {
    assert_scribbled_run_exit("two_field_move", &two_field_move_out_source(), 6);
}

/// (F2 item 3) The owned-binding two-field shape that F1b regressed into a
/// false use-after-consume now compiles and single-frees.
#[test]
fn two_field_move_out_binding_single_free() {
    assert_scribbled_run_exit(
        "two_field_move_binding",
        &two_field_move_out_binding_source(),
        6,
    );
}

/// (F2 item 3, negative re-read) The aggregate cannot be re-read after any
/// field move — the second `match b` is a compile-time use-after-move.
#[test]
fn two_field_reread_is_use_after_move() {
    assert_compile_fails(
        "two_field_reread",
        &two_field_reread_source(),
        "used after it was consumed",
    );
}

// ── F2 (guard continuation): fallthrough-capable match-arm guards ────────────
//
// A `match`-arm guard is evaluated BEFORE the arm is committed; a false guard
// falls through to a later arm. If the guard CONSUMES a projected heap payload
// (a block-expression guard containing a move, e.g. `if { var w = v; ... }`),
// the consume neutralizes (nulls) the shared scrutinee payload slot before the
// guard outcome is known. A false guard then falls through to a later arm that
// re-projects the now-null slot → null-fault / abort. Because guard truth is
// not statically known and every arm guard can fall through, projected
// heap-payload CONSUMPTION inside any match-arm guard is rejected fail-closed,
// regardless of the guard's (would-be) truth. A read-only borrow in the guard
// (`v.len() > 100`) never reaches the consume hook and stays valid.

/// (F2 guard) A block-expression guard that MOVES the projected payload (`var
/// w = v`) and then falls through (guard false) to a later arm that
/// re-destructures the same slot. The consume must be rejected at compile time
/// as a guard-fallthrough move rather than null-fault at runtime.
fn guarded_consume_fallthrough_source() -> String {
    format!(
        "enum Box {{ Full(Vec<i64>); Empty; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box::Full(v) if {{ var w = v; w.len() > 100 }} => {{ out = 1; }}\n\
         \x20       Box::Full(v2) => {{ out = v2.len(); }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 guard) The same block-move guard with a would-be-TRUE predicate
/// (`w.len() > 0`). The reject is on the consume-in-fallthrough-guard shape,
/// not the guard's truth — guard truth is not statically known, so this is
/// rejected identically.
fn true_guarded_consume_source() -> String {
    format!(
        "enum Box {{ Full(Vec<i64>); Empty; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box::Full(v) if {{ var w = v; w.len() > 0 }} => {{ out = 1; }}\n\
         \x20       Box::Full(v2) => {{ out = v2.len(); }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 guard control) A borrow-only guard (`v.len() > 100`) that evaluates
/// false and falls through to a later arm. The borrow never reaches the
/// consume hook, so the scrutinee payload stays live and the later arm reads it
/// correctly. Guard false → `v2.len()` = 3.
fn borrow_only_guard_fallthrough_source() -> String {
    format!(
        "enum Box {{ Full(Vec<i64>); Empty; }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box::Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box::Full(v) if v.len() > 100 => {{ out = 1; }}\n\
         \x20       Box::Full(v2) => {{ out = v2.len(); }}\n\
         \x20       Box::Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 guard) A projected heap-payload consumed inside a fallthrough-capable
/// guard is rejected fail-closed — the false-guard fallthrough would otherwise
/// re-project a neutralized (null) payload slot and abort.
#[test]
fn guarded_consume_fallthrough_is_rejected() {
    assert_compile_fails(
        "guarded_consume_fallthrough",
        &guarded_consume_fallthrough_source(),
        "cannot move the heap-owning payload `v` out of a `match`-arm guard that can fall through",
    );
}

/// (F2 guard) The would-be-true guarded consume is rejected identically — the
/// policy is on the consume-in-guard shape, not the guard's (unknown) truth.
#[test]
fn true_guarded_consume_is_rejected() {
    assert_compile_fails(
        "true_guarded_consume",
        &true_guarded_consume_source(),
        "cannot move the heap-owning payload `v` out of a `match`-arm guard that can fall through",
    );
}

/// (F2 guard control) A borrow-only guard that falls through stays valid and
/// the later arm reads the still-live payload. Returns 3.
#[test]
fn borrow_only_guard_fallthrough_runs_safely() {
    assert_scribbled_run_exit(
        "borrow_only_guard_fallthrough",
        &borrow_only_guard_fallthrough_source(),
        3,
    );
}
