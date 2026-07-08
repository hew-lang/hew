//! Per-iteration leak / double-free oracle for the closure-env KEYSTONE: an
//! escaping (heap-boxed) closure that captures an OWNED value (`string`,
//! `Vec`, a heap-owning record, an owned-payload enum) must free that
//! captured value EXACTLY once when the closure is dropped.
//!
//! ## What this proves
//!
//! A closure that escapes its introducing scope (returned, stored, passed to a
//! higher-order callee) has its capture environment heap-boxed at the literal
//! site: `[free_thunk: ptr][captures...]`. The captured owning value's handle
//! is byte-copied into the captures region and the caller's own binding drop is
//! suppressed (its local is read by the env `RecordInit` ingress → marked
//! aliased → excluded from the kept-drop set), so the env becomes the SOLE
//! owner of that handle. The env free thunk must release each owned captured
//! field through the per-field drop authority before freeing the box, so the
//! captured value is freed exactly once as the env's sole owner.
//!
//! The failure modes this oracle catches:
//!   * a LEAK (the env frees the box but not the captured handle) — a positive
//!     per-iteration leak slope;
//!   * a SECOND owner (the caller binding AND the env both freeing the handle)
//!     — a double-free that aborts under the poisoned-allocator triple;
//!   * an OVER-DROP corrupting a live value — a scribbled output / non-zero
//!     exit.
//!
//! ## Methodology: per-iteration leak slope
//!
//! Each capturing shape is built into a loop that constructs the owned value,
//! captures it into a returned (escaping) closure, invokes the closure, and
//! lets it drop — once per iteration. The shape is compiled at a LOW and a HIGH
//! iteration count and the leak NODE counts are differenced (see
//! [`support::leak_slope`]): a correct escaping capture frees the captured
//! handle every iteration and holds the slope flat; a leaked capture grows the
//! node count with the iteration count. The delta cancels the constant baseline
//! noise a single-shot `leaks --atExit` count cannot, so the gate is
//! deterministic.
//!
//! The captured `string` is built with `.to_upper()` (a fresh runtime heap
//! string with no concat-temp of its own), and the captured `Vec` with
//! `Vec::new()` + `push` (a provably-heap buffer) — so the only per-iteration
//! heap node the slope can see is the captured handle the env must free.
//!
//! ## Skip behaviour
//!
//! `leaks(1)` is Darwin's allocator inspector; on non-macOS hosts the slope
//! probes log `skip:` and return. The `MallocScribble` no-double-free pins run
//! on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// -- fixtures ----------------------------------------------------------------

/// Escaping closure captures a runtime-built owned `string`. `make_label`
/// builds a fresh heap `string` via `.to_upper()` and returns `|| label.len()`,
/// so the closure env is heap-boxed and owns the captured `label`; the env free
/// thunk must release it exactly once when the returned closure drops at the end
/// of each loop iteration. `label.len()` (16 for the uppercased seed) plus the
/// iteration index is summed so the calls cannot be eliminated; `main`
/// self-checks the total so the scribble pin's `success()` holds.
fn captures_string_loop_source(frames: usize) -> String {
    let seed_len: usize = "row-payload-seed".len();
    let expected_total = frames * seed_len + frames * frames.saturating_sub(1) / 2;
    format!(
        "fn make_label(n: i64) -> fn() -> i64 {{\n\
         \x20   let label = \"row-payload-seed\".to_upper();\n\
         \x20   || label.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_label(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 91; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Escaping closure captures an owned `Vec<i64>`. `make_counter` builds the Vec
/// with `Vec::new()` + `push` (a provably-heap buffer) and returns `|| xs.len()`,
/// so the env is heap-boxed and owns the captured `xs`; the free thunk must
/// release the Vec exactly once when the returned closure drops. Each call
/// returns `2 + n`, summed so the calls cannot be eliminated and `main`
/// self-checks the total.
fn captures_vec_loop_source(frames: usize) -> String {
    let expected_total = frames * 2 + frames * frames.saturating_sub(1) / 2;
    format!(
        "fn make_counter(n: i64) -> fn() -> i64 {{\n\
         \x20   var xs: Vec<i64> = Vec::new();\n\
         \x20   xs.push(10);\n\
         \x20   xs.push(20);\n\
         \x20   || xs.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_counter(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 92; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Escaping closure captures a heap-owning RECORD (`Holder { counts: Vec<i64> }`)
/// built from a local binding and moved into the env (#2419). The record
/// capture dispatches through the synthesised `__hew_record_drop_inplace_Holder`
/// (seeded by the closure-capture drop-seed pass); the free thunk must run it
/// exactly once per iteration so the record's owned Vec is released — a missing
/// body was an LLVM verify reject, an empty one is a positive slope here.
fn captures_record_loop_source(frames: usize) -> String {
    let expected_total = frames * 2 + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Holder {{\n\
         \x20   counts: Vec<i64>;\n\
         }}\n\
         fn make_holder(n: i64) -> fn() -> i64 {{\n\
         \x20   let counts: Vec<i64> = Vec::new();\n\
         \x20   counts.push(10);\n\
         \x20   counts.push(20);\n\
         \x20   let h = Holder {{ counts: counts }};\n\
         \x20   || h.counts.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_holder(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 93; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Escaping closure captures an ENUM whose active variant owns a runtime-built
/// `string` payload — the enum twin of the record capture (#2419). The free
/// thunk dispatches `__hew_enum_drop_inplace_Tag`, whose tag-aware body must
/// release the payload string exactly once per iteration.
fn captures_enum_loop_source(frames: usize) -> String {
    let expected_total = frames + frames * frames.saturating_sub(1) / 2;
    format!(
        "enum Tag {{\n\
         \x20   Named(string);\n\
         \x20   Anon;\n\
         }}\n\
         fn make_tagger(n: i64) -> fn() -> i64 {{\n\
         \x20   let t = Tag::Named(\"row-payload-seed\".to_upper());\n\
         \x20   || match t {{\n\
         \x20       Tag::Named(_) => 1 + n,\n\
         \x20       Tag::Anon => n,\n\
         \x20   }}\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_tagger(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 94; }}\n\
         \x20   0\n\
         }}\n"
    )
}

// -- correctness pins --------------------------------------------------------

/// Run `source` to native, execute under the poisoned-allocator triple, and
/// assert clean exit. A crash here is a double-free (the caller binding AND the
/// env both freeing the captured handle); a non-zero exit is a miscomputed read
/// off a scribbled capture, or the fixture's own total check failing.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("closure-capture-owned-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{shape_name}: escaping closure capture must free the owned value exactly once -- a \
         crash here indicates a double-free: the caller binding freed the same handle the env \
         now owns. The env must be the sole owner (the caller's scope-exit drop is suppressed by \
         the aliased-local scan); a non-zero exit is a scribbled-read miscompute or the fixture's \
         own total check;\n{}",
        describe_output(&output)
    );
}

// -- oracles -----------------------------------------------------------------

/// Slope oracle (string): a returned closure capturing a runtime-built owned
/// `string` frees the captured handle every iteration — flat leak slope.
/// Pre-keystone the free thunk freed only the box and this leaked the captured
/// string per iteration (positive slope).
#[test]
fn return_closure_captures_string_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_string", captures_string_loop_source);
}

/// Slope oracle (Vec): a returned closure capturing an owned `Vec<i64>` frees
/// the captured buffer every iteration — flat leak slope.
#[test]
fn return_closure_captures_vec_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_vec", captures_vec_loop_source);
}

/// Slope oracle (record, #2419): a returned closure capturing a heap-owning
/// record frees the record's owned Vec every iteration through the synthesised
/// `__hew_record_drop_inplace_Holder` — flat leak slope.
#[test]
fn return_closure_captures_record_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_record", captures_record_loop_source);
}

/// Slope oracle (enum, #2419 twin): a returned closure capturing an enum with
/// an owned string payload frees the payload every iteration through the
/// synthesised `__hew_enum_drop_inplace_Tag` — flat leak slope.
#[test]
fn return_closure_captures_enum_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_enum", captures_enum_loop_source);
}

/// No-double-free pin (string): the escaping capture frees the owned `string`
/// EXACTLY once across 200 iterations. A second owner aborts under the poisoned
/// allocator. Runs on any unix host.
#[test]
fn return_closure_captures_string_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_string_df", &captures_string_loop_source(200));
}

/// No-double-free pin (Vec): the escaping capture frees the owned `Vec` EXACTLY
/// once across 200 iterations. Runs on any unix host.
#[test]
fn return_closure_captures_vec_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_vec_df", &captures_vec_loop_source(200));
}

/// No-double-free pin (record, #2419): the escaping record capture frees the
/// record's owned Vec EXACTLY once across 200 iterations — the caller binding's
/// drop is suppressed (the env is the sole owner), so a second release aborts
/// under the poisoned allocator. Runs on any unix host.
#[test]
fn return_closure_captures_record_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_record_df", &captures_record_loop_source(200));
}

/// No-double-free pin (enum, #2419 twin): the escaping enum capture frees the
/// owned string payload EXACTLY once across 200 iterations. Runs on any unix
/// host.
#[test]
fn return_closure_captures_enum_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_enum_df", &captures_enum_loop_source(200));
}
