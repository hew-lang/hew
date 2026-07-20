//! Leak oracle for FORM 1 — a `Vec::push` of a fresh COMPOSITE call-result whose
//! producer tail-returns its construction through a `let`-bound local (the
//! `[..]` array-literal desugar).
//!
//! `fn mkHolder(i) -> Holder { Holder { items: [f"x{i}", f"y{i}"] } }` builds its
//! `items` field through the array-literal desugar (`{ let __a = Vec::new();
//! __a.push(..); __a }`). Before the fresh-owner see-through, the freshness
//! fixpoint dead-ended that `let`-bound-local tail at the fail-closed OPAQUE
//! leaf, so `mkHolder` was mis-classified NON-fresh; `v.push(mkHolder(i))` then
//! took the copy-in `hew_vec_push_owned` router and the moved-in Holder's inner
//! `Vec<string>` heap leaked one payload per iteration.
//!
//! Seeing through the desugar proves `mkHolder` fresh, so the push consult routes
//! the element to move-in and the value is released exactly once (when the
//! container is dropped). Two independent signals:
//! - flat per-iteration leak slope (`leaks --atExit`, LOW vs HIGH frames); and
//! - the poisoned-allocator triple does not abort AND the exact-content
//!   assertion holds (released exactly once — not zero, not twice).
//!
//! This shape FAILS (positive slope) with the see-through reverted.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// `v.push(mkHolder(i))` where `mkHolder` builds its `Vec<string>` field through
/// the array-literal desugar. `v.len()` is read each iteration (a length borrow,
/// NOT a get-clone) so the pushed element must exist and be released once. The
/// per-iteration container holds exactly one Holder.
fn push_fresh_composite_source(frames: usize) -> String {
    format!(
        "type Holder {{ items: Vec<string> }}\n\
         fn mkHolder(i: i64) -> Holder {{ Holder {{ items: [f\"x{{i % 10}}\", f\"y{{i % 10}}\"] }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       var v: Vec<Holder> = [];\n\
         \x20       v.push(mkHolder(i));\n\
         \x20       total = total + v.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 71; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A producer that binds the construction to a `let` before returning it
/// (`let h = Holder { .. }; h`) — the second fix-(i) see-through idiom, distinct
/// from the array desugar. Must also route to move / flat 0.
fn push_letbound_composite_source(frames: usize) -> String {
    format!(
        "type Holder {{ items: Vec<string> }}\n\
         fn mkHolder(i: i64) -> Holder {{ let h = Holder {{ items: [f\"x{{i % 10}}\"] }}; h }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       var v: Vec<Holder> = [];\n\
         \x20       v.push(mkHolder(i));\n\
         \x20       total = total + v.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 72; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A producer that moves a fresh, deep-owned Holder through two immutable
/// bindings before returning it. The move from `a` to `b` is the only use of
/// `a`; proving the chain fresh routes `v.push(mkHolder(i))` to move-in so the
/// nested `Vec<string>` is released with the outer container.
fn push_nested_single_move_composite_source(frames: usize) -> String {
    format!(
        "type Holder {{ items: Vec<string> }}\n\
         fn mkHolder(i: i64) -> Holder {{\n\
         \x20   let a = Holder {{ items: [f\"x{{i % 10}}\", f\"y{{i % 10}}\"] }};\n\
         \x20   let b = a;\n\
         \x20   b\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       var v: Vec<Holder> = [];\n\
         \x20       v.push(mkHolder(i));\n\
         \x20       total = total + v.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 75; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Poisoned-allocator exactly-once pin: a leak is caught by the slope test; a
/// double-free aborts under the scribbled allocator; a mis-read fails the
/// program's content assertion (non-zero exit). Clean success == read-correct
/// and released exactly once.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("fresh-composite-push-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: a fresh composite pushed by a proven-fresh producer must be \
         moved in and released exactly once;\n{}",
        describe_output(&output)
    );
}

#[test]
fn push_fresh_composite_call_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fresh_composite_push_array", push_fresh_composite_source);
}

#[test]
fn push_letbound_composite_call_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fresh_composite_push_letbound",
        push_letbound_composite_source,
    );
}

#[test]
fn push_nested_single_move_composite_has_no_per_cycle_leak_slope() {
    assert_frame_slope_below_tolerance(
        "fresh_composite_push_nested_single_move",
        push_nested_single_move_composite_source,
    );
}

#[test]
fn push_fresh_composite_call_does_not_double_free() {
    assert_no_double_free(
        "fresh_composite_push_array_df",
        &push_fresh_composite_source(50),
    );
}

#[test]
fn push_letbound_composite_call_does_not_double_free() {
    assert_no_double_free(
        "fresh_composite_push_letbound_df",
        &push_letbound_composite_source(50),
    );
}

#[test]
fn push_nested_single_move_composite_does_not_double_free() {
    assert_no_double_free(
        "fresh_composite_push_nested_single_move_df",
        &push_nested_single_move_composite_source(50),
    );
}
