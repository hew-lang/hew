//! Leak oracle for P3 — a fresh COMPOSITE call-result passed directly into a
//! BORROWING parameter, with no user `let`.
//!
//! `borrowSum(mkOuter(i))` hands the fresh `Outer` temporary straight to a
//! borrowing callee. The temporary has no `let`, so no scope-exit drop was
//! minted; before the composite mint admission, `caller_borrowed_temp_arg_owned_ty`
//! admitted a fresh producer ONLY for a `String` result, so the fresh `Outer`'s
//! inner heap (two strings) leaked one payload set per iteration.
//!
//! `mkOuter` is proven fresh by the module sole-owner prover
//! (`callee_returns_fresh_owner`); the widened composite `Call` arm now mints a
//! caller-side drop gated by `proven_borrow_args`, so the temporary is released
//! exactly once. Signals: flat leak slope + no double-free under the poisoned
//! allocator + exact-content assertion. FAILS (positive slope) with the mint
//! admission reverted.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// `borrowSum(mkOuter(i))` — a fresh nested-record producer into a borrowing
/// param. `borrowSum` reads `o.inner.a.len() + o.inner.b.len()` (projections =
/// borrows), so `o` is proven-borrow and the caller owns and drops the fresh
/// `Outer`. Each field is `"a" + "b"` (len 2) → 4 per iteration.
fn borrow_fresh_composite_source(frames: usize) -> String {
    format!(
        "type Inner {{ a: string, b: string }}\n\
         type Outer {{ inner: Inner }}\n\
         fn mkOuter(i: i64) -> Outer {{ Outer {{ inner: Inner {{ a: f\"x{{i % 10}}\", b: f\"y{{i % 10}}\" }} }} }}\n\
         fn borrowSum(o: Outer) -> i64 {{ o.inner.a.len() + o.inner.b.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + borrowSum(mkOuter(i));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 4 {{ return 73; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A producer that reaches its fresh construction through a `let`-bound local
/// (`let o = Outer { .. }; o`) — exercises fix (i)'s see-through AND fix (ii)'s
/// mint together on the borrow-param seam.
fn borrow_letbound_composite_source(frames: usize) -> String {
    format!(
        "type Inner {{ a: string, b: string }}\n\
         type Outer {{ inner: Inner }}\n\
         fn mkOuter(i: i64) -> Outer {{ let o = Outer {{ inner: Inner {{ a: f\"x{{i % 10}}\", b: f\"y{{i % 10}}\" }} }}; o }}\n\
         fn borrowSum(o: Outer) -> i64 {{ o.inner.a.len() + o.inner.b.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + borrowSum(mkOuter(i));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 4 {{ return 74; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("fresh-composite-borrow-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: a fresh composite temporary passed to a borrowing callee \
         must be released exactly once at caller scope exit;\n{}",
        describe_output(&output)
    );
}

#[test]
fn borrow_fresh_composite_call_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fresh_composite_borrow", borrow_fresh_composite_source);
}

#[test]
fn borrow_letbound_composite_call_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fresh_composite_borrow_letbound",
        borrow_letbound_composite_source,
    );
}

#[test]
fn borrow_fresh_composite_call_does_not_double_free() {
    assert_no_double_free(
        "fresh_composite_borrow_df",
        &borrow_fresh_composite_source(50),
    );
}

#[test]
fn borrow_letbound_composite_call_does_not_double_free() {
    assert_no_double_free(
        "fresh_composite_borrow_letbound_df",
        &borrow_letbound_composite_source(50),
    );
}
