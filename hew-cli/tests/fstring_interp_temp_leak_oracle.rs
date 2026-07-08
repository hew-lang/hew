//! Per-iteration leak / double-free oracle for f-string interpolation of a
//! non-string value: the `Display::fmt` conversion temp (`hew_i64_to_string`)
//! and — outside a moved/yielded position — the `hew_string_concat` join
//! result must each be released exactly once per interpolation.
//!
//! ## What this proves
//!
//! `f"item-{i}"` desugars (`hew-hir/src/lower.rs::lower_interpolated_string`)
//! to a chain of `stdlib_catalog` presentation-name calls: `to_string_i64(i)`
//! producing a fresh conversion temp, then `string_concat(lit, temp)` joining
//! it with the literal segment. Both calls reach MIR as
//! `Terminator::Call { callee: <catalog name>, .. }`, and the catalog name
//! `string_concat` (unlike its `hew_string_concat` c-symbol sibling, and
//! unlike the already-covered `to_string_i64`/`println_str` catalog names)
//! had no `callee_ownership_contract` row — it fell through to
//! `CalleeOwnershipContract::FAIL_CLOSED`, so
//! `collect_nested_fresh_string_temp_drops`
//! (`hew-mir/src/lower.rs`, W5.011 P3) could never admit either the concat's
//! own fresh-owned result OR the conversion temp feeding it as a borrowed
//! argument. Fixed by dual-listing `"string_concat"` alongside
//! `"hew_string_concat"` in `hew-mir/src/runtime_symbols.rs`'s
//! `callee_ownership_contract` — the runtime behaviour is byte-identical (the
//! catalog entry is a `BuiltinLinkage::RuntimeFfiShim` over the same symbol),
//! so the contract must be too.
//!
//! A SEPARATE, second gap affected only generator bodies: `gen fn`/`gen {}`
//! coroutine ramps are lowered via `lower_gen_block`'s own hand-rolled
//! `RawMirFunction` construction, which never called
//! `apply_nested_fresh_string_temp_drops` at all (every ordinary function
//! gets it via `lower_function`'s shared post-`finalize_blocks` pipeline).
//! Fixed by adding the identical call at the analogous point in
//! `lower_gen_block`.
//!
//! ## Failure modes this oracle catches
//!
//!   * a LEAK (the conversion temp or the concat result is never released) —
//!     a positive per-iteration leak slope;
//!   * a SECOND owner (the concat result double-freed by both the inline temp
//!     drop and its downstream consumer) — a double-free that aborts under
//!     the poisoned-allocator triple;
//!   * an OVER-DROP corrupting a live value — a scribbled output / non-zero
//!     exit.
//!
//! ## Methodology: per-iteration leak slope
//!
//! Each shape loops `frames` times, interpolating the loop counter into an
//! f-string every iteration. Compiled at a LOW and a HIGH iteration count and
//! the leak NODE counts are differenced (see [`support::leak_slope`]): a
//! correct release holds the slope flat; a leaked temp grows the node count
//! with the iteration count.
//!
//! ## Two shapes
//!
//!   * **statement position** (`fstring_scalar_interp_loop_source`) — a plain
//!     `while` loop `println`-ing `f"item-{i}"`; both the conversion temp AND
//!     the concat result are unbound nested temps (red: 2 leaked nodes/iter
//!     on main pre-fix).
//!   * **gen-body** (`fstring_gen_yield_interp_loop_source`) — a standalone
//!     `gen fn` yielding `f"item-{i}"` per iteration; the concat result is
//!     consumed by the yield-transport (already correctly handled elsewhere),
//!     so only the conversion temp is at risk (red: 1 leaked node/iter
//!     pre-fix, reproduces with zero receive-gen/actor surface).
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

/// Statement position: `println(f"item-{i}")` in a `while` loop. Both the
/// `hew_i64_to_string` conversion temp and the `hew_string_concat` join
/// result are unbound nested temps fed straight into `println`, never a
/// `let` binding — exactly the shape `collect_nested_fresh_string_temp_drops`
/// must admit without a scope-exit binding to anchor on. `total` sums the
/// loop counter so the calls cannot be eliminated and `main` self-checks it.
fn fstring_scalar_interp_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       println(f\"item-{{i}}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 95; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Gen-body: a standalone `gen fn` yields `f"item-{i}"` per iteration. The
/// concat result is published through the yield-transport (a MOVE, correctly
/// excluded from this mechanism's admission), but the `hew_i64_to_string`
/// conversion temp feeding the concat is still an unbound nested temp INSIDE
/// the coroutine ramp — the second (pipeline-wiring) gap this oracle covers.
/// No receive-gen or actor surface is involved.
fn fstring_gen_yield_interp_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "gen fn items(n: i64) -> string {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < n {{\n\
         \x20       yield f\"item-{{i}}\";\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var idx: i64 = 0;\n\
         \x20   for v in items({frames}) {{\n\
         \x20       println(v);\n\
         \x20       total = total + idx;\n\
         \x20       idx = idx + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 96; }}\n\
         \x20   0\n\
         }}\n"
    )
}

// -- correctness pins --------------------------------------------------------

/// Run `source` to native, execute under the poisoned-allocator triple, and
/// assert clean exit. A crash here is a double-free (the conversion temp or
/// the concat result released twice); a non-zero exit is a scribbled-read
/// miscompute or the fixture's own total check failing.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("fstring-interp-temp-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{shape_name}: f-string interpolation temps must be released exactly once -- a crash \
         here indicates a double-free of the conversion temp or the concat result; a non-zero \
         exit is a scribbled-read miscompute or the fixture's own total check;\n{}",
        describe_output(&output)
    );
}

// -- oracles -----------------------------------------------------------------

/// Slope oracle (statement position): `println(f"item-{i}")` releases both
/// nested temps every iteration — flat leak slope. Pre-fix this leaked 2
/// nodes/iteration (`hew_i64_to_string` + `hew_string_concat`).
#[test]
fn fstring_scalar_interp_statement_position_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_scalar_stmt", fstring_scalar_interp_loop_source);
}

/// Slope oracle (gen-body): a standalone `gen fn` yielding `f"item-{i}"`
/// releases the conversion temp every iteration — flat leak slope. Pre-fix
/// this leaked 1 node/iteration, reproducing with zero receive-gen/actor
/// surface (the string-yield control shape).
#[test]
fn fstring_scalar_interp_gen_yield_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_scalar_gen", fstring_gen_yield_interp_loop_source);
}

/// No-double-free pin (statement position): both nested temps release
/// EXACTLY once across 200 iterations. A second owner aborts under the
/// poisoned allocator. Runs on any unix host.
#[test]
fn fstring_scalar_interp_statement_position_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_scalar_stmt_df",
        &fstring_scalar_interp_loop_source(200),
    );
}

/// No-double-free pin (gen-body): the conversion temp releases EXACTLY once
/// per yield across 200 iterations. Runs on any unix host.
#[test]
fn fstring_scalar_interp_gen_yield_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_scalar_gen_df",
        &fstring_gen_yield_interp_loop_source(200),
    );
}
