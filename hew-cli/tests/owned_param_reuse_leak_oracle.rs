//! Leak oracle for caller-side owned by-value param drop under REUSE.
//!
//! An owned composite (record / tuple / enum) bound to a `let` and passed BY
//! VALUE to TWO borrowing free functions in sequence (`let r = …; f(r); g(r);`)
//! is borrowed by each call, not consumed: under the copy-on-write borrow model
//! the CALLER retains ownership and drops the value exactly ONCE at its own
//! scope exit — not once per call (which would double-free), and not zero times
//! (which would leak per iteration).
//!
//! This pins the reuse edge of the record/tuple/enum caller-side drop:
//! reuse-across-two-borrowing-calls was previously untested, and this oracle
//! closes that gap. It is the composite-param twin of
//! `owned_vec_cross_function_release_leak_oracle` (the shipped collection
//! precedent).
//!
//! Two independent signals per shape:
//! - the per-iteration leak SLOPE stays flat (`leaks --atExit`, LOW vs HIGH
//!   frame counts under the poisoned-allocator triple) — no per-call/per-iter
//!   leak; and
//! - the poisoned allocator (`MallocScribble`+`MallocPreScribble`+
//!   `MallocGuardEdges`) does not abort — the value is released exactly once, so
//!   the second borrowing call never reads freed storage and scope-exit never
//!   double-frees.
//!
//! The enum shape uses non-consuming (borrow) callee bodies: a `match`
//! scrutinee and a method receiver of an owned enum param are classified CONSUME
//! by the parameter-body summary (destructure / receiver intent), so the
//! caller-side proven-borrow shape for an enum is a callee that does not consume
//! it. Reads of an enum payload via `match`/method receivers are a separate
//! (summary / callee-side) concern, out of scope for this caller-side oracle.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// Record reused across two borrowing free calls. `f`/`g` read `r.s.len()` — a
/// field projection is a borrow, so `r` is proven-borrow to both and the caller
/// drops it once per iteration. `r.s == "ab"` (len 2), so each iteration adds 4.
fn record_reuse_source(frames: usize) -> String {
    format!(
        "type Rec {{ s: string }}\n\
         \n\
         fn f(r: Rec) -> i64 {{ r.s.len() }}\n\
         fn g(r: Rec) -> i64 {{ r.s.len() }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let r = Rec {{ s: \"a\" + \"b\" }};\n\
         \x20       let a = f(r);\n\
         \x20       let b = g(r);\n\
         \x20       total = total + a + b;\n\
         \x20   }}\n\
         \x20   if total != {frames} * 4 {{ return 71; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Tuple reused across two borrowing free calls. `tf`/`tg` read `t.0`/`t.1`
/// (element projections = borrows), so `t` is proven-borrow to both and the
/// caller drops it once. `t.0 == "ab"` (2) + `t.1 == "cde"` (3) → 5 per iter.
fn tuple_reuse_source(frames: usize) -> String {
    format!(
        "fn tf(t: (string, string)) -> i64 {{ t.0.len() }}\n\
         fn tg(t: (string, string)) -> i64 {{ t.1.len() }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let t = (\"a\" + \"b\", \"c\" + \"de\");\n\
         \x20       let a = tf(t);\n\
         \x20       let b = tg(t);\n\
         \x20       total = total + a + b;\n\
         \x20   }}\n\
         \x20   if total != {frames} * 5 {{ return 72; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Enum reused across two borrowing free calls. `ef`/`eg` do not consume their
/// param (non-consuming body = the proven-borrow enum shape), so `e` is
/// proven-borrow to both and the caller drops it once. The heap payload
/// (`Ok("ab")`) must be released exactly once per iteration.
fn enum_reuse_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> i64 {{ 2 }}\n\
         fn eg(e: Result<string, string>) -> i64 {{ 3 }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let e: Result<string, string> = Ok(\"a\" + \"b\");\n\
         \x20       let a = ef(e);\n\
         \x20       let b = eg(e);\n\
         \x20       total = total + a + b;\n\
         \x20   }}\n\
         \x20   if total != {frames} * 5 {{ return 73; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Poisoned-allocator no-double-free pin: an over-eager caller drop (freeing the
/// value after the first borrowing call, or twice at scope exit) reads/aborts
/// under the scribbled allocator. A clean exit means exactly-once release.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("owned-param-reuse-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: an owned by-value param reused across two borrowing calls \
         must be released exactly once at caller scope exit;\n{}",
        describe_output(&output)
    );
}

#[test]
fn record_param_reuse_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_param_reuse_record", record_reuse_source);
}

#[test]
fn tuple_param_reuse_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_param_reuse_tuple", tuple_reuse_source);
}

#[test]
fn enum_param_reuse_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_param_reuse_enum", enum_reuse_source);
}

#[test]
fn record_param_reuse_does_not_double_free() {
    assert_no_double_free("record_reuse_df", &record_reuse_source(50));
}

#[test]
fn tuple_param_reuse_does_not_double_free() {
    assert_no_double_free("tuple_reuse_df", &tuple_reuse_source(50));
}

#[test]
fn enum_param_reuse_does_not_double_free() {
    assert_no_double_free("enum_reuse_df", &enum_reuse_source(50));
}
