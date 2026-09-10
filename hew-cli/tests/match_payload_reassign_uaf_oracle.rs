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
//!   * **Consume agreement.** Moving the projected binder is a real transfer
//!     of the payload, so a projection the arm only reads leaves the
//!     scrutinee readable afterwards. The accepted move-out shapes and their
//!     exactly-once release live in the `payload-move-out-*` and
//!     `payload-projection-reread` core-acceptance cases, which run under
//!     ASan/LSan; this file keeps the runtime pins and the borrow controls.
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
\x20   var v: Vec<i64> = Vec.new();\n\
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

/// Move-out + reassign, single match, no re-read. `w = seed()` is the sole
/// free of the moved payload buffer; the scrutinee's scope-exit composite
/// drop must no-op on the neutralized slot. Pre-fix: double-free aborts.
/// Returns `3 + 3 = 6`.
fn move_reassign_no_reread_source() -> String {
    format!(
        "enum Box {{ \n\
         \x20   Full(Vec<i64>), \n\
         \x20   Empty, \n\n\
         \x20}}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box.Full(v) => {{\n\
         \x20           out = v.len();\n\
         \x20           var w = v;\n\
         \x20           w = seed();\n\
         \x20           out = out + w.len();\n\
         \x20       }}\n\
         \x20       Box.Empty => {{}}\n\
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
        "enum Box {{ \n\
         \x20   Full(Vec<i64>), \n\
         \x20   Empty, \n\n\
         \x20}}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box.Full(v) => {{\n\
         \x20           out = v.len();\n\
         \x20           let w = v;\n\
         \x20           out = out + w.len();\n\
         \x20       }}\n\
         \x20       Box.Empty => {{}}\n\
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
        "enum Pair {{ \n\
         \x20   Both((Vec<i64>, Vec<i64>)), \n\
         \x20   Neither, \n\n\
         \x20}}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Pair.Both((seed(), seed()));\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Pair.Both(v) => {{\n\
         \x20           out = v.0.len() + v.1.len();\n\
         \x20           let w = v;\n\
         \x20           out = out + w.0.len();\n\
         \x20       }}\n\
         \x20       Pair.Neither => {{}}\n\
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
        "enum Box {{ \n\
         \x20   Full(Vec<i64>), \n\
         \x20   Empty, \n\n\
         \x20}}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var i = 0;\n\
         \x20   var acc = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Box.Full(seed());\n\
         \x20       match b {{\n\
         \x20           Box.Full(v) => {{\n\
         \x20               var w = v;\n\
         \x20               w = seed();\n\
         \x20               acc = acc + w.len();\n\
         \x20           }}\n\
         \x20           Box.Empty => {{}}\n\
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
        "enum Box {{ \n\
         \x20   Full(Vec<i64>), \n\
         \x20   Empty, \n\n\
         \x20}}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match b {{\n\
         \x20           Box.Full(v) => {{\n\
         \x20               sum = sum + v.len();\n\
         \x20           }}\n\
         \x20           Box.Empty => {{}}\n\
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
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
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
// A projected payload moved out of a re-readable PLACE scrutinee
// (`match h.b`, `match pair.0`, `match o.inner.b`) reaches the origin's real
// storage, so the move is accepted and the payload is released once; the
// `payload-move-out-places` core-acceptance case pins that under ASan/LSan.
// What stays here is the pair of controls: a read-only borrow of a field
// place keeps the scrutinee live, and an ephemeral scrutinee's move-out frees
// exactly once.

/// A read-only borrow of a record-FIELD place scrutinee must stay valid — the
/// rejection fires only on a move-out (`Consume`), never on a borrow. The loop
/// re-matches `h.b` every iteration and totals `3 * 5 = 15`.
fn field_place_borrow_only_source() -> String {
    format!(
        "enum Box {{ \n\
         \x20   Full(Vec<i64>), \n\
         \x20   Empty, \n\n\
         \x20}}\n\
         type Holder {{ b: Box, }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let h = Holder {{ b: Box.Full(seed()) }};\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       match h.b {{\n\
         \x20           Box.Full(v) => {{ sum = sum + v.len(); }}\n\
         \x20           Box.Empty => {{}}\n\
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
        "enum Box {{ \n\
         \x20   Full(Vec<i64>), \n\
         \x20   Empty, \n\n\
         \x20}}\n\
         fn mk() -> Box {{ Box.Full(seed()) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var out = 0;\n\
         \x20   match mk() {{\n\
         \x20       Box.Full(v) => {{ out = v.len(); var w = v; w = seed(); out = out + w.len(); }}\n\
         \x20       Box.Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
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

// ── #2523 F2: exhaustive projection-ownership correction (nested / capture /
//     two independent fields) ─────────────────────────────────────────────────
//
// Three defects found by the codegen + cross reviews, one root cause (a
// projected heap payload moved out of a scrutinee whose real storage the
// temp-neutralize cannot reach, or a false use-after-consume on a valid
// second-field move):
//
//   1. NESTED binders (`Outer::Wrap(Inner::Full(v))`) reach the outer value's
//      real nested slot, so the move-out is accepted and releases once; the
//      `payload-move-out-patterns` core-acceptance case pins it. The borrow-only
//      control stays here.
//   2. A closure-CAPTURED binding (`match b` inside `|| { … }` that captures
//      `b`) is read from the closure environment by BYTE-COPY
//      (`ClosureEnvFieldLoad`), NOT moved into the temp; the captured copy
//      survives the move and double-frees when the env drops. Now REJECTED.
//   3. TWO independent heap fields moved in one arm
//      (`Both(x, y) => var wx = x; var wy = y;`) must both single-free — the
//      first move must not raise a false use-after-consume on the second, and
//      the two owners stay in separate move components so neither scope-exit
//      drop is wrongly stripped.

/// (F2 item 1 control) A borrow-only NESTED destructure never reaches the
/// consume hook, so it stays valid: the loop reads the nested payload and
/// totals correctly with no double-free.
fn nested_enum_borrow_only_source() -> String {
    format!(
        "enum Inner {{  Full(Vec<i64>), Hollow }}\n\
         enum Outer {{  Wrap(Inner), Bare }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Outer.Wrap(Inner.Full(seed()));\n\
         \x20   var total = 0;\n\
         \x20   match b {{\n\
         \x20       Outer.Wrap(Inner.Full(v)) => {{ total = v.len(); }}\n\
         \x20       Outer.Wrap(Inner.Hollow) => {{}}\n\
         \x20       Outer.Bare => {{}}\n\
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
        "enum Box {{  Full(Vec<i64>), Empty }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var total = 0;\n\
         \x20   var f = capture(var total) || {{\n\
         \x20       match b {{\n\
         \x20           Box.Full(v) => {{ var w = v; total = w.len(); w = seed(); }}\n\
         \x20           Box.Empty => {{}}\n\
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
        "enum Box {{  Full(Vec<i64>), Empty }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   let f = || -> i64 {{\n\
         \x20       match b {{\n\
         \x20           Box.Full(v) => v.len(),\n\
         \x20           Box.Empty => 0,\n\
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
        "enum Pair {{  Both(Vec<i64>, Vec<i64>), Neither }}\n\
         fn mk() -> Pair {{ Pair.Both(seed(), seed()) }}\n\
         fn main() -> i64 {{\n\
         \x20   var out = 0;\n\
         \x20   match mk() {{\n\
         \x20       Pair.Both(x, y) => {{\n\
         \x20           var wx = x; var wy = y;\n\
         \x20           wx = seed(); wy = seed();\n\
         \x20           out = wx.len() + wy.len();\n\
         \x20       }}\n\
         \x20       Pair.Neither => {{}}\n\
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
        "enum Pair {{  Both(Vec<i64>, Vec<i64>), Neither }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Pair.Both(seed(), seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Pair.Both(x, y) => {{\n\
         \x20           var wx = x; var wy = y;\n\
         \x20           wx = seed(); wy = seed();\n\
         \x20           out = wx.len() + wy.len();\n\
         \x20       }}\n\
         \x20       Pair.Neither => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (F2 item 1 control) A borrow-only nested destructure stays valid and frees
/// exactly once. Returns 3.
#[test]
fn nested_enum_borrow_only_is_valid() {
    assert_scribbled_run_exit("nested_enum_borrow", &nested_enum_borrow_only_source(), 3);
}

/// (F2 item 2) A projected-payload move-out of a closure-captured binding
/// compiles and runs clean: `capture(...)` names an independent immutable
/// snapshot per binding (HEW-SPEC-2026 closure syntax), so the closure's copy
/// of `b` owns its own payload and moving `v` out of it cannot alias the
/// outer scope's `b`. This superseded the prior fail-closed rejection from
/// before the named-capture redesign. `total` is materialised through the
/// closure's own private field (same as `closure_captured_var_writeback`),
/// so `main`'s outer `total` stays 0 after `f()` runs.
#[test]
fn captured_binding_move_out_is_valid() {
    assert_scribbled_run_exit(
        "captured_binding_move",
        &captured_binding_move_out_source(),
        0,
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

// ── F2 (guard continuation): fallthrough-capable match-arm guards ────────────
//
// A `match`-arm guard is evaluated BEFORE the arm is committed; a false guard
// falls through to a later arm, which re-projects the same payload. A guard
// over a value payload takes its own copy, so the fallthrough arm still reads
// a live payload — the `payload-projection-reread` core-acceptance case pins
// both the false and the taken guard. A guard that genuinely consumes a
// resource binder is still refused by the ownership stage
// (`E_OWN_GUARD_CONSUME`). The borrow-only guard control stays here.

/// (F2 guard control) A borrow-only guard (`v.len() > 100`) that evaluates
/// false and falls through to a later arm. The borrow never reaches the
/// consume hook, so the scrutinee payload stays live and the later arm reads it
/// correctly. Guard false → `v2.len()` = 3.
fn borrow_only_guard_fallthrough_source() -> String {
    format!(
        "enum Box {{  Full(Vec<i64>), Empty }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var out = 0;\n\
         \x20   match b {{\n\
         \x20       Box.Full(v) if v.len() > 100 => {{ out = 1; }}\n\
         \x20       Box.Full(v2) => {{ out = v2.len(); }}\n\
         \x20       Box.Empty => {{}}\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
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

/// A borrow-only `if let` over an owning binding, re-projected on every loop
/// iteration. `if let` destructures through the same payload-binder registrar
/// as `match`, so an owner minted for the borrowed payload frees it at the end
/// of the first iteration and leaves `b` holding null. Totals `3 * 5 = 15`.
fn if_let_borrow_only_source() -> String {
    format!(
        "enum Box {{  Full(Vec<i64>), Empty }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var i = 0;\n\
         \x20   var sum = 0;\n\
         \x20   while i < 5 {{\n\
         \x20       if let Box.Full(v) = b {{\n\
         \x20           sum = sum + v.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sum\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// A borrow-only `while let` over an owning binding: the loop re-projects the
/// same payload every iteration and breaks once the total passes ten, so a
/// binder that released the payload on the first iteration would read freed
/// memory on the second. Totals `3 * 4 = 12`.
fn while_let_borrow_only_source() -> String {
    format!(
        "enum Box {{  Full(Vec<i64>), Empty }}\n\
         fn main() -> i64 {{\n\
         \x20   let b = Box.Full(seed());\n\
         \x20   var sum = 0;\n\
         \x20   while let Box.Full(v) = b {{\n\
         \x20       sum = sum + v.len();\n\
         \x20       if sum > 10 {{\n\
         \x20           break;\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   sum\n\
         }}\n\
         \n\
         {SEED_FN}"
    )
}

/// (Alias control) A borrow-only `if let` keeps its scrutinee live across the
/// loop: the binder aliases `b`'s payload instead of minting a second release
/// authority for it.
#[test]
fn if_let_borrow_only_keeps_scrutinee_live() {
    assert_scribbled_run_exit("if_let_borrow", &if_let_borrow_only_source(), 15);
}

/// (Alias control) The same for `while let`, whose payload binders escape into
/// the loop body scope and are released on a different edge.
#[test]
fn while_let_borrow_only_keeps_scrutinee_live() {
    assert_scribbled_run_exit("while_let_borrow", &while_let_borrow_only_source(), 12);
}
