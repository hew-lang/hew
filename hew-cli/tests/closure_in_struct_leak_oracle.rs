//! Per-iteration leak / double-free oracle for the forwarded-fn-parameter →
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
//!     a double-free that aborts under the poisoned-allocator triple;
//!   * a LEAK (neither owner freeing the env) — a positive per-iteration leak
//!     slope;
//!   * an OVER-DROP corrupting a live value — a scribbled output.
//!
//! ## Methodology: per-iteration leak slope
//!
//! The forward-into-field and store-and-drop shapes are each built into a loop
//! that allocates the heap-boxed closure env, stores it into a fresh `Handler`
//! record, invokes the stored closure, and lets the record drop — once per
//! iteration. The shape is compiled at a LOW and a HIGH iteration count and the
//! leak NODE counts are differenced (see [`support::leak_slope`]): the record
//! frees its owned env every iteration and holds the slope flat; a leaked env
//! grows the node count with the iteration count. The delta cancels the constant
//! baseline noise a single-shot `leaks --atExit` count cannot, so the gate is
//! deterministic.
//!
//! ## Skip behaviour
//!
//! `leaks(1)` is Darwin's allocator inspector; on non-macOS hosts the slope
//! probes log `skip:` and return. The `MallocScribble` no-double-free pin runs
//! on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// -- fixtures ----------------------------------------------------------------

/// The D16 pattern, looped: forward the `f` parameter into a record field, then
/// invoke the stored closure through the field. The record owns the heap env and
/// frees it once at its drop; `f`'s own scope-exit drop is suppressed. `h` drops
/// at the end of each iteration, so a leaked env grows the slope. `h.action(1)`
/// returns `1 + i`, summed and self-checked so the calls cannot be eliminated and
/// the scribble pin's `success()` holds.
fn forward_param_into_field_loop_source(frames: usize) -> String {
    let expected_total = frames + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Handler {{ action: fn(i64) -> i64; }}\n\
         fn make_adder(n: i64) -> fn(i64) -> i64 {{ |x: i64| x + n }}\n\
         fn make_handler(f: fn(i64) -> i64) -> Handler {{ Handler {{ action: f }} }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let h = make_handler(make_adder(i));\n\
         \x20       total = total + h.action(1);\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 81; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The struct holding the closure drops on a NORMAL function exit
/// (`cleanup-all-exits`), looped. `use_then_drop` builds a `Handler`, invokes it,
/// and lets it drop at the tail; its env-drop must fire on that exit, freeing the
/// env exactly once. A leaked env grows the slope.
fn store_and_drop_struct_loop_source(frames: usize) -> String {
    let expected_total = frames + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Handler {{ action: fn(i64) -> i64; }}\n\
         fn make_adder(n: i64) -> fn(i64) -> i64 {{ |x: i64| x + n }}\n\
         fn use_then_drop(f: fn(i64) -> i64) -> i64 {{\n\
         \x20   let h = Handler {{ action: f }};\n\
         \x20   h.action(1)\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + use_then_drop(make_adder(i)); }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 82; }}\n\
         \x20   0\n\
         }}\n"
    )
}

// -- correctness pins --------------------------------------------------------

/// Compile + run `source` under the poisoned-allocator triple, assert clean exit
/// — the no-double-free pin. A second owner (the parameter freeing the same env
/// the record owns) aborts under the scribbled allocator; a non-zero exit is a
/// miscomputed dispatch or the fixture's own total check.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("closure-in-struct-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{shape_name}: forwarded-param field store must free the closure env exactly once -- a \
         crash here indicates a double-free: the parameter freed the same heap env the record now \
         owns. The record must be the sole owner (the parameter's scope-exit drop is suppressed by \
         the aliased-local scan in `derive_closure_pair_drop_allowed`); a non-zero exit is a \
         miscomputed dispatch, a scribbled env, or the fixture's own total check;\n{}",
        describe_output(&output)
    );
}

// -- oracles -----------------------------------------------------------------

/// Slope oracle: the D16 forward-param-into-field shape frees the record-owned
/// closure env every iteration — flat leak slope. A leaked env (neither the
/// record nor the parameter freeing it) grows the slope.
#[test]
fn forward_param_into_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "forward_param_into_field",
        forward_param_into_field_loop_source,
    );
}

/// Slope oracle: a struct holding the forwarded closure drops on a normal
/// function exit and frees its env every iteration — flat leak slope.
#[test]
fn store_and_drop_struct_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("store_and_drop_struct", store_and_drop_struct_loop_source);
}

/// No-double-free pin: the forwarded-param field store frees the closure env
/// EXACTLY once across 200 iterations. A second owner aborts under the poisoned
/// allocator before the result prints. Runs on any unix host.
#[test]
fn forward_param_into_field_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "forward_param_into_field_df",
        &forward_param_into_field_loop_source(200),
    );
}
