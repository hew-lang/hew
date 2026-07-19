//! Leak oracle for the caller-side drop of a fresh owned composite/string
//! argument TEMPORARY (#2743).
//!
//! An inline fresh rvalue passed BY VALUE to a BORROWING free function —
//! `g(Rec { s: "a" + "b" })`, `tf(("a" + "b", "c" + "de"))`, `ef(Ok("a" + "b"))`,
//! `h("a" + "b")` — has no user `let`, so no `BindingId` and no scope-exit drop.
//! #2735 completed the caller-side drop for the NAMED shape
//! (`let r = Rec { .. }; g(r)`) by preserving the binding's scope-exit drop
//! through the alias-escape scan; for a temporary there is no drop to preserve,
//! so nobody frees the fresh value and it leaks one heap payload per iteration.
//!
//! #2743 mints a synthetic owner over the temporary at the call site so it flows
//! through the identical `owned_locals` machinery as the named shape and is
//! released exactly ONCE at caller scope exit — not zero times (leak), not once
//! per call and again at scope exit (double-free).
//!
//! Two independent signals per shape:
//! - the per-iteration leak SLOPE stays flat (`leaks --atExit`, LOW vs HIGH frame
//!   counts) — no per-iteration leak; and
//! - the poisoned allocator (`MallocScribble` + `MallocPreScribble` +
//!   `MallocGuardEdges`) does not abort AND the program's exact-content assertion
//!   (`total != frames * K`) holds — the fresh value is read correctly by the
//!   borrowing callee and released exactly once.
//!
//! The existing `owned_param_reuse_leak_oracle` pins only the already-passing
//! NAMED/reuse shape; this file is the TEMPORARY twin it does not cover. The
//! NAMED-canary cases here additionally prove #2743 did not regress the #2735
//! named path (they must stay flat/leak-0).

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// Record TEMPORARY to a borrowing callee. `g` reads `r.s.len()` (a field
/// projection = borrow), so `r` is proven-borrow and the caller owns the fresh
/// `Rec` inline value and drops it once per iteration. `r.s == "ab"` (len 2).
fn record_temp_source(frames: usize) -> String {
    format!(
        "type Rec {{ s: string }}\n\
         fn g(r: Rec) -> i64 {{ r.s.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + g(Rec {{ s: \"a\" + \"b\" }});\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 71; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Tuple TEMPORARY to a borrowing callee. `tf` reads `t.0`/`t.1` (element
/// projections = borrows). `t.0 == "ab"` (2) + `t.1 == "cde"` (3) → 5 per iter.
/// The fresh 2-string tuple owns two heap payloads that must each be released
/// exactly once.
fn tuple_temp_source(frames: usize) -> String {
    format!(
        "fn tf(t: (string, string)) -> i64 {{ t.0.len() + t.1.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + tf((\"a\" + \"b\", \"c\" + \"de\"));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 5 {{ return 72; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Enum TEMPORARY to a borrowing callee. `ef` does NOT consume its param (a
/// non-consuming body is the proven-borrow enum shape), so the caller owns the
/// fresh `Ok("ab")` composite and releases its heap payload exactly once per
/// iteration. `ef` returns 2.
fn enum_temp_source(frames: usize) -> String {
    format!(
        "fn ef(e: Result<string, string>) -> i64 {{ 2 }}\n\
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

/// String TEMPORARY to a borrowing callee. `h` reads `s.len()` (a borrow), so
/// the caller owns the fresh concat buffer and releases it once per iteration.
/// `"a" + "b" == "ab"` (len 2). A string is not a composite — its caller drop
/// routes through the refcount-contract prover, not the composite escape scan.
fn string_temp_source(frames: usize) -> String {
    format!(
        "fn h(s: string) -> i64 {{ s.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + h(\"a\" + \"b\");\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 74; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// NAMED-local canary (record): `let r = Rec { .. }; g(r)`. The #2735 named path
/// — must stay leak-0/flat, proving #2743's temporary mint did not regress it.
fn record_named_canary_source(frames: usize) -> String {
    format!(
        "type Rec {{ s: string }}\n\
         fn g(r: Rec) -> i64 {{ r.s.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let r = Rec {{ s: \"a\" + \"b\" }};\n\
         \x20       total = total + g(r);\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 81; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// NAMED-local canary (string): `let s = "a" + "b"; h(s)`. Must stay leak-0/flat.
fn string_named_canary_source(frames: usize) -> String {
    format!(
        "fn h(s: string) -> i64 {{ s.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let s = \"a\" + \"b\";\n\
         \x20       total = total + h(s);\n\
         \x20   }}\n\
         \x20   if total != {frames} * 2 {{ return 82; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Poisoned-allocator exactly-once pin: a leak leaves the payload behind (caught
/// by the slope test); a double-free (freeing after the call AND at scope exit)
/// aborts under the scribbled allocator; a mis-read fresh value fails the
/// program's `total != frames * K` content assertion (non-zero exit). A clean,
/// successful exit means read-correct and released exactly once.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("owned-composite-temp-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: a fresh owned composite/string argument temporary passed \
         to a borrowing callee must be released exactly once at caller scope \
         exit;\n{}",
        describe_output(&output)
    );
}

#[test]
fn record_temp_arg_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_composite_temp_record", record_temp_source);
}

#[test]
fn tuple_temp_arg_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_composite_temp_tuple", tuple_temp_source);
}

#[test]
fn enum_temp_arg_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_composite_temp_enum", enum_temp_source);
}

#[test]
fn string_temp_arg_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_composite_temp_string", string_temp_source);
}

#[test]
fn record_temp_arg_does_not_double_free() {
    assert_no_double_free("record_temp_df", &record_temp_source(50));
}

#[test]
fn tuple_temp_arg_does_not_double_free() {
    assert_no_double_free("tuple_temp_df", &tuple_temp_source(50));
}

#[test]
fn enum_temp_arg_does_not_double_free() {
    assert_no_double_free("enum_temp_df", &enum_temp_source(50));
}

#[test]
fn string_temp_arg_does_not_double_free() {
    assert_no_double_free("string_temp_df", &string_temp_source(50));
}

#[test]
fn record_named_canary_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_composite_named_record", record_named_canary_source);
}

#[test]
fn string_named_canary_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_composite_named_string", string_named_canary_source);
}

#[test]
fn record_named_canary_does_not_double_free() {
    assert_no_double_free("record_named_df", &record_named_canary_source(50));
}

#[test]
fn string_named_canary_does_not_double_free() {
    assert_no_double_free("string_named_df", &string_named_canary_source(50));
}
