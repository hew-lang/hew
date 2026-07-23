//! Leak oracle for the callee-side drop of a CONSUMED by-value enum param
//! (#2732).
//!
//! When a function takes a heap-owning enum composite by value and its body
//! `match`es the param (`fn ef(e: Result<string, string>) { match e { .. } }`),
//! the parameter-body summary classifies the scrutinee CONSUME: the caller moves
//! the value in and does NOT drop it, so the CALLEE owns the shell. The enum's
//! `match` payload binders are non-owning aliases (they do not drop the extracted
//! payload), so with no callee-side shell drop the active variant's heap payload
//! leaked one allocation per call — the enum twin of the record match-drain
//! callee-drop that already works.
//!
//! #2732 registers such a consumed enum param into `owned_locals` so the
//! fail-closed `derive_enum_composite_drop_allowed` prover emits the tag-aware
//! `EnumInPlace` shell drop on every consuming path.
//!
//! Exactly-once teeth:
//! - the per-iteration leak SLOPE stays flat (no per-call leak);
//! - the poisoned allocator does not abort; AND
//! - the MOVE-OUT negative control: a `match` arm that moves the payload OUT
//!   (`Ok(x) => x`) must NOT also drop the shell — the returned binder now owns
//!   the buffer, so a shell drop would double-free. The prover's escape scan
//!   excludes the composite on that path; the poisoned allocator proves no
//!   double-free.
//! - the Item A / #2743 interaction: a consumed enum passed as an inline
//!   TEMPORARY (`ef(Ok("a" + "b"))`) is dropped by the callee, NOT the caller —
//!   the caller mint is borrow-gated and stays silent, so the two paths are
//!   mutually exclusive (single drop, no double-free).

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// Consumed `Result<string, string>` param matched with non-moving arms (the
/// binders `x`/`y` are read/ignored, never moved out), so the callee owns and
/// drops the shell exactly once. Passed a NAMED scrutinee. `ef` returns 2.
fn result_named_consume_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> i64 {{ match e {{ Ok(x) => 2, Err(y) => 3 }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let e: Result<string, string> = Ok(\"a\" + \"b\");\n\
         \x20       total = total + ef(e);\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 73; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The #2743 interaction: the consumed enum is an inline TEMPORARY. The caller
/// mint is borrow-gated (silent for a consuming callee), so ONLY the callee
/// drops the shell — single release, no double-free.
fn result_temp_consume_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> i64 {{ match e {{ Ok(x) => 2, Err(y) => 3 }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + ef(Ok(\"a\" + \"b\"));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 73; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A user enum with an owned-payload variant, consumed by a matching callee and
/// passed as an inline temporary. `handle` returns 1 for the `Text` arm.
fn user_enum_consume_source(frames: usize) -> String {
    format!(
        "enum Msg {{ Text(string); Code(i64) }}\n\
         fn handle(m: Msg) -> i64 {{ match m {{ Text(s) => 1, Code(n) => n }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + handle(Msg::Text(\"a\" + \"b\"));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 1 {{ return 74; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// MOVE-OUT negative control: every arm MOVES the payload out and returns it, so
/// the returned binder owns the buffer and the caller (`let s = ef(..)`) drops
/// it once. The callee must NOT also drop the shell — the escape scan excludes
/// the composite on the move-out path. Content-pinned: `s == "ab"` (len 2).
fn move_out_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> string {{ match e {{ Ok(x) => x, Err(y) => y }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let s = ef(Ok(\"a\" + \"b\"));\n\
         \x20       total = total + s.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 75; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// MIXED-ARM pin: the `Ok` arm MOVES the payload out (the returned binder owns
/// it; the arm neutralizes the variant slot so the callee's carrier drop
/// no-ops on that path) while the `Err` arm only reads (its payload must be
/// released by the callee's carrier drop on that path). Exactly-once on BOTH
/// paths is the per-arm path-sensitivity tooth: a path-insensitive shell drop
/// double-frees the `Ok` path; a path-insensitive suppression leaks the `Err`
/// path. `ef` per iteration: Ok yields "ab" (2), Err yields "err" (3).
fn mixed_move_out_and_read_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> string {{ match e {{ Ok(x) => x, Err(y) => \"e\" + \"rr\" }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let s = ef(Ok(\"a\" + \"b\"));\n\
         \x20       let t = ef(Err(\"c\" + \"d\"));\n\
         \x20       total = total + s.len() + t.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 5 {{ return 76; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// LET-SHARE pin: `let s = x` on a `string` payload binder is a RETAIN-share
/// (+1 on the buffer), not a move — the variant slot keeps its release
/// authority and the callee's carrier drop releases the original count while
/// the retained binding crosses to the caller. Neutralizing the slot on a
/// share edge leaks one buffer per call; treating the share as a move
/// double-frees.
fn move_out_via_let_share_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> string {{\n\
         \x20   match e {{\n\
         \x20       Ok(x) => {{ let s = x; s }},\n\
         \x20       Err(y) => y\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let s = ef(Ok(\"a\" + \"b\"));\n\
         \x20       total = total + s.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 77; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Poisoned-allocator exactly-once pin: a leak leaves the payload behind (caught
/// by the slope test); a double-free (a callee shell drop that races the caller
/// drop, or a shell drop on a move-out arm) aborts under the scribbled
/// allocator; a mis-read fails the program's `total != frames * K` content
/// assertion. A clean, successful exit means released exactly once.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("enum-callee-consume-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: a consumed by-value enum param must be released exactly \
         once by the callee — and never on a move-out arm;\n{}",
        describe_output(&output)
    );
}

#[test]
fn result_named_consume_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_callee_result_named", result_named_consume_source);
}

#[test]
fn result_temp_consume_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_callee_result_temp", result_temp_consume_source);
}

#[test]
fn user_enum_consume_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_callee_user_enum", user_enum_consume_source);
}

#[test]
fn move_out_arm_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_callee_move_out", move_out_source);
}

#[test]
fn result_named_consume_does_not_double_free() {
    assert_no_double_free("result_named_df", &result_named_consume_source(50));
}

#[test]
fn result_temp_consume_does_not_double_free() {
    assert_no_double_free("result_temp_df", &result_temp_consume_source(50));
}

#[test]
fn user_enum_consume_does_not_double_free() {
    assert_no_double_free("user_enum_df", &user_enum_consume_source(50));
}

#[test]
fn move_out_arm_does_not_double_free() {
    assert_no_double_free("move_out_df", &move_out_source(50));
}

#[test]
fn mixed_move_out_and_read_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_callee_mixed_arms", mixed_move_out_and_read_source);
}

#[test]
fn mixed_move_out_and_read_does_not_double_free() {
    assert_no_double_free("mixed_arms_df", &mixed_move_out_and_read_source(50));
}

#[test]
fn move_out_via_let_share_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("enum_callee_let_share", move_out_via_let_share_source);
}

#[test]
fn move_out_via_let_share_does_not_double_free() {
    assert_no_double_free("let_share_df", &move_out_via_let_share_source(50));
}
