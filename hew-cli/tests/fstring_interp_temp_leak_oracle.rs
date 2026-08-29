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

/// Multi-interpolation f-string in statement position: `interps` copies of the
/// loop counter interpolated into ONE f-string per iteration
/// (`f"a={i} b={i} c={i}"` for `interps == 3`). Each interpolation adds a
/// `to_string_i64` conversion (a block-terminating `Terminator::Call` that
/// SPLITS the concat chain) plus a `hew_string_concat` join. #2726: the
/// intermediate concat result whose consuming concat lands in the NEXT block
/// (past the `to_string` split) was never admitted by
/// `collect_nested_fresh_string_temp_drops` — the instruction-use concat-chain
/// branch required def and use in the SAME block, so every `to_string`-split
/// intermediate leaked. 1–2 interpolations stay in one block (clean); 3+
/// cross a split and leaked pre-fix, at a rate that scales with `interps`.
fn fstring_multi_interp_loop_source(interps: usize, frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    let fstr = (0..interps)
        .map(|j| format!("s{j}={{i}}"))
        .collect::<Vec<_>>()
        .join(" ");
    format!(
        "fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       println(f\"{fstr}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 94; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn fstring_one_interp_loop_source(frames: usize) -> String {
    fstring_multi_interp_loop_source(1, frames)
}
fn fstring_two_interp_loop_source(frames: usize) -> String {
    fstring_multi_interp_loop_source(2, frames)
}
fn fstring_three_interp_loop_source(frames: usize) -> String {
    fstring_multi_interp_loop_source(3, frames)
}
fn fstring_four_interp_loop_source(frames: usize) -> String {
    fstring_multi_interp_loop_source(4, frames)
}
fn fstring_five_interp_loop_source(frames: usize) -> String {
    fstring_multi_interp_loop_source(5, frames)
}

/// Call-result directly interpolated: `println(f"v={mk(i)}")` where `mk`
/// is a USER function returning a fresh owned `string`. The call result is
/// consumed straight into the interpolation with no intervening `let`, so
/// the only thing that can release it is a caller-side temp-arg drop minted
/// at the call. Pre-fix `caller_borrowed_temp_arg_owned_ty` recognised only
/// runtime fresh-string producers, so a user-function result leaked 1 node
/// per iteration. Binding first (`let s = mk(i)`) or dropping the f-string
/// (`println(mk(i))`) were both already clean — those are the controls
/// below.
fn fstring_user_call_result_interp_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "fn mk(i: i64) -> string {{ f\"tok{{i}}\" }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       println(f\"v={{mk(i)}}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 90; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Control for [`fstring_user_call_result_interp_loop_source`]: the identical
/// program with the call result bound to a `let` before interpolation. This
/// path was always clean (the binding anchors the string sole-owner prover);
/// pin it so a future change to the temp-arg mint cannot regress it into a
/// double release.
fn fstring_user_call_bound_interp_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "fn mk(i: i64) -> string {{ f\"tok{{i}}\" }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let s = mk(i);\n\
         \x20       println(f\"v={{s}}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 89; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A user function forwards a by-value string parameter through its return.
/// The returned pointer aliases the argument, but the callee retains one share
/// before writing the return slot, so the anonymous call result is a real
/// caller-owned carrier. Direct interpolation must release that share once.
///
/// The payload length intentionally keeps every leaked allocation in the
/// 48-byte size class: at 20 iterations the unfixed compiler reports the exact
/// 20-allocation / 960-byte baseline.
fn fstring_forwarded_return_interp_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "fn passthru(value: string) -> string {{ value }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       println(f\"value={{passthru(f\"owned-carrier-token-{{i}}\")}}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 83; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Bound positive-path control for
/// [`fstring_forwarded_return_interp_loop_source`]. The binding already
/// anchors the returned share's ordinary scope-exit drop; the direct-consumer
/// repair must not add a second owner.
fn fstring_forwarded_return_bound_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "fn passthru(value: string) -> string {{ value }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let returned = passthru(f\"owned-carrier-token-{{i}}\");\n\
         \x20       println(f\"value={{returned}}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 82; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Every return path yields one releasable share, through two distinct
/// mechanisms: `holder.value` is retained by the record-field load and
/// `fallback` is retained before the return-slot move. The precise provenance
/// summary is `ParamsOnly`, not fresh, but the one-share postcondition is
/// uniform across the branch.
///
/// Both payload spellings stay in the 32-byte size class: at 20 iterations the
/// unfixed compiler reports the exact 20-allocation / 640-byte baseline.
fn fstring_mixed_projection_forward_return_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum();
    format!(
        "type Holder {{ value: string }}\n\
         fn choose(holder: Holder, fallback: string, project: bool) -> string {{\n\
         \x20   if project {{ holder.value }} else {{ fallback }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let holder = Holder {{ value: f\"a-{{i}}\" }};\n\
         \x20       let fallback = f\"b-{{i}}\";\n\
         \x20       println(f\"value={{choose(holder, fallback, i % 2 == 0)}}\");\n\
         \x20       total = total + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 81; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// A mutable string parameter has an implicit borrowed entry definition plus
/// the explicit owned reassignment definition. The false branch returns the
/// entry alias, so the callee must retain it even though the MIR writer scan
/// also sees the sibling owned `Move`.
///
/// Before the fail-closed coverage check, the caller released an unretained
/// alias and then the still-live `owned` binding released the same buffer:
/// allocator poisoning reported a missing C-string header sentinel.
fn fstring_conditional_var_param_return_loop_source(frames: usize) -> String {
    let expected_total: usize = (0..frames).sum::<usize>() * 2;
    format!(
        "fn pick(var value: string, replace: bool) -> string {{\n\
         \x20   if replace {{ value = \"replacement\"; }}\n\
         \x20   value\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let owned = f\"live-token-{{i}}\";\n\
         \x20       println(f\"carrier={{pick(owned, false)}}\");\n\
         \x20       if owned.len() < 12 {{ return 80; }}\n\
         \x20       total = total + i + i;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_total} {{ return 79; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Match-payload variant: an `Option<string>` payload binder interpolated
/// directly. `f"v={s}"` lowers the binder through the stdlib `impl Display
/// for string` (`string::fmt`), a Hew-bodied callee — which the enum
/// composite escape scan read as an ownership ESCAPE, stripping the whole
/// `Option<string>` of its `EnumInPlace` scope-exit drop and leaking the
/// payload every iteration. `println(s)` (no f-string) was clean, because
/// `println` is on the hardcoded borrow-sink list. The binder is read AGAIN
/// after the interpolation so an over-eager release surfaces as a scribbled
/// length rather than passing silently.
fn fstring_enum_payload_interp_loop_source(frames: usize) -> String {
    let expected_len: usize = (0..frames).map(|i| format!("tok{i}").len()).sum();
    format!(
        "fn mkopt(i: i64) -> Option<string> {{ Some(f\"tok{{i}}\") }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match mkopt(i) {{\n\
         \x20           Some(s) => {{ println(f\"v={{s}}\"); total = total + s.len(); }}\n\
         \x20           None => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_len} {{ return 88; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// `Result<string, string>` sibling of
/// [`fstring_enum_payload_interp_loop_source`] — the same defect reached
/// through a two-owned-payload enum, where BOTH variants carry a `string` the
/// composite drop must dispose tag-aware.
///
/// BOTH tags are constructed and BOTH payloads are interpolated. Exercising
/// only `Ok` would leave the `Err` arm's payload -- a distinct variant slot
/// with its own field offset in the tagged union -- entirely unobserved, so a
/// tag-blind drop that disposed the wrong slot would pass unnoticed. The
/// alternation is odd/even on the loop counter so both arms run on every
/// probe frame count, and the two running totals are checked independently at
/// exit, which pins that each arm read its OWN payload intact.
fn fstring_result_payload_interp_loop_source(frames: usize) -> String {
    let ok_len: usize = (0..frames)
        .filter(|i| i % 2 == 0)
        .map(|i| format!("tok{i}").len())
        .sum();
    let err_len: usize = (0..frames)
        .filter(|i| i % 2 == 1)
        .map(|i| format!("bad{i}").len())
        .sum();
    format!(
        "fn mkres(i: i64) -> Result<string, string> {{\n\
         \x20   if i % 2 == 0 {{ Ok(f\"tok{{i}}\") }} else {{ Err(f\"bad{{i}}\") }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var ok_total: i64 = 0;\n\
         \x20   var err_total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match mkres(i) {{\n\
         \x20           Ok(s) => {{ println(f\"v={{s}}\"); ok_total = ok_total + s.len(); }}\n\
         \x20           Err(e) => {{ println(f\"e={{e}}\"); err_total = err_total + e.len(); }}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if ok_total != {ok_len} {{ return 87; }}\n\
         \x20   if err_total != {err_len} {{ return 86; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Fail-closed pin with a NON-COPY-IN sink. The `Option<string>` payload is
/// interpolated (the shape the fix admits) AND then MOVED into an outer
/// `var` that outlives the match arm. `last = s` aliases the same refcounted
/// buffer with no copy-in, so a composite drop admitted here would release a
/// buffer the outer binding still owns — a double release, not a leak.
///
/// The escape must keep excluding the composite: this program is EXPECTED to
/// leak. What is pinned is that it exits cleanly with the exact post-loop
/// values, i.e. the buffer stayed alive and was never released twice. A
/// `Vec::push` sink would prove nothing here — it byte-copies the handle and
/// so cannot exhibit the failure at all.
fn fstring_enum_payload_escapes_loop_source(frames: usize) -> String {
    let expected_len: usize = (0..frames).map(|i| format!("tok{i}").len()).sum();
    let last = format!("tok{}", frames - 1);
    format!(
        "fn mkopt(i: i64) -> Option<string> {{ Some(f\"tok{{i}}\") }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   var last: string = \"\";\n\
         \x20   while i < {frames} {{\n\
         \x20       match mkopt(i) {{\n\
         \x20           Some(s) => {{ println(f\"v={{s}}\"); last = s; }}\n\
         \x20           None => {{}}\n\
         \x20       }}\n\
         \x20       total = total + last.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if last != \"{last}\" {{ return 86; }}\n\
         \x20   if total != {expected_len} {{ return 85; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Fail-closed pin with a non-copy-in sink at a CALL boundary: the fresh
/// call-result temp is handed to a user function that aliases it into a
/// record the caller then owns and reads after the call returns. The
/// temp-arg mint must NOT claim this buffer — the record does. Expected to
/// leak; pinned to exit cleanly with the exact total, proving the record's
/// field was never released out from under the read.
fn fstring_user_call_result_escapes_loop_source(frames: usize) -> String {
    let expected_len: usize = (0..frames).map(|i| format!("tok{i}").len()).sum();
    format!(
        "type Box {{ s: string }}\n\
         fn mk(i: i64) -> string {{ f\"tok{{i}}\" }}\n\
         fn keep(s: string) -> Box {{ Box {{ s: s }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = keep(mk(i));\n\
         \x20       println(f\"v={{b.s}}\");\n\
         \x20       total = total + b.s.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected_len} {{ return 84; }}\n\
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
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_scalar_interp_statement_position_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_scalar_stmt", fstring_scalar_interp_loop_source);
}

/// Slope oracle (gen-body): a standalone `gen fn` yielding `f"item-{i}"`
/// releases the conversion temp every iteration — flat leak slope. Pre-fix
/// this leaked 1 node/iteration, reproducing with zero receive-gen/actor
/// surface (the string-yield control shape).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_scalar_interp_gen_yield_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_scalar_gen", fstring_gen_yield_interp_loop_source);
}

/// No-double-free pin (statement position): both nested temps release
/// EXACTLY once across 200 iterations. A second owner aborts under the
/// poisoned allocator. Runs on any unix host.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_scalar_interp_statement_position_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_scalar_stmt_df",
        &fstring_scalar_interp_loop_source(200),
    );
}

/// No-double-free pin (gen-body): the conversion temp releases EXACTLY once
/// per yield across 200 iterations. Runs on any unix host.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_scalar_interp_gen_yield_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_scalar_gen_df",
        &fstring_gen_yield_interp_loop_source(200),
    );
}

// -- #2726 multi-interpolation concat-chain oracles --------------------------

/// No-regression slope pins: 1- and 2-interpolation f-strings keep the whole
/// concat chain in one block (no `to_string` split between an intermediate and
/// its consuming concat), so they were always clean. Pin them flat so a future
/// change to the admission cannot silently regress the already-correct cases.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_one_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_1interp", fstring_one_interp_loop_source);
}
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_two_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_2interp", fstring_two_interp_loop_source);
}

/// Teeth (#2726): 3-, 4-, and 5-interpolation f-strings. Each interpolation
/// past the second splits the concat chain across a `to_string` terminator, so
/// the intermediate concat result consumed in the next block leaked once per
/// split per iteration pre-fix — a positive slope that scales with the
/// interpolation count (3→~1, 4→~3, 5→~4 leaked nodes/iteration). With the
/// cross-block domination admission the slope is flat.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_three_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_3interp", fstring_three_interp_loop_source);
}
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_four_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_4interp", fstring_four_interp_loop_source);
}
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_five_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fstring_5interp", fstring_five_interp_loop_source);
}

/// No-double-free pins (#2726): the intermediate concat results release EXACTLY
/// once across 200 iterations for 3-, 4-, and 5-interpolation f-strings. A
/// second owner (an over-eager cross-block drop that double-frees) aborts under
/// the poisoned-allocator triple; a scribbled read miscomputes the total.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_three_interp_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("fstring_3interp_df", &fstring_three_interp_loop_source(200));
}
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_four_interp_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("fstring_4interp_df", &fstring_four_interp_loop_source(200));
}
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_five_interp_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("fstring_5interp_df", &fstring_five_interp_loop_source(200));
}

// -- #2803 owned-call-result / match-payload interpolation oracles -----------

/// Teeth: a USER function's fresh `string` result interpolated DIRECTLY into
/// an f-string. Pre-fix the caller-side temp-arg mint recognised only runtime
/// fresh-string producers, so the result leaked 1 node/iteration
/// (`println(f"v={mk(i)}")` — 5 leaks / 160 bytes over 5 iterations).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_user_call_result_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_user_call_result",
        fstring_user_call_result_interp_loop_source,
    );
}

/// Control: binding the call result first was always clean. Pinned so the
/// temp-arg mint cannot regress it into a second owner.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_user_call_bound_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_user_call_bound",
        fstring_user_call_bound_interp_loop_source,
    );
}

/// Teeth: an `Option<string>` payload binder interpolated directly. Pre-fix
/// the Hew-bodied `string::fmt` call read as an ownership escape and stripped
/// the composite's `EnumInPlace` drop, leaking the payload every iteration.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_enum_payload_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_enum_payload",
        fstring_enum_payload_interp_loop_source,
    );
}

/// Teeth: the `Result<string, string>` sibling — both variants own a string,
/// so the composite drop must dispose the active one tag-aware.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_result_payload_interp_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_result_payload",
        fstring_result_payload_interp_loop_source,
    );
}

/// No-double-free pin: the user-call result releases EXACTLY once across 200
/// interpolations. A second owner aborts under the poisoned allocator; an
/// over-drop scribbles the read and trips the fixture's own total check.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_user_call_result_interp_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_user_call_result_df",
        &fstring_user_call_result_interp_loop_source(200),
    );
}

/// A forwarded return carrier consumed directly by interpolation must have a
/// flat allocation slope. The unfixed 20-frame authority is exactly
/// 20 allocations / 960 bytes.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_forwarded_return_carrier_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_forwarded_return_carrier",
        fstring_forwarded_return_interp_loop_source,
    );
}

/// The projection/forward mixed return must also remain flat. The unfixed
/// 20-frame authority is exactly 20 allocations / 640 bytes.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_mixed_projection_forward_return_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_mixed_projection_forward_return",
        fstring_mixed_projection_forward_return_loop_source,
    );
}

/// Binding the forwarded carrier is the positive-path control: it keeps its
/// pre-existing single scope-exit owner.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_forwarded_return_bound_control_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "fstring_forwarded_return_bound",
        fstring_forwarded_return_bound_loop_source,
    );
}

/// Missing-release counterfactual: the direct forwarded carrier used to leak
/// once per iteration. Extra-release counterfactual: a second owner aborts or
/// scribbles the live payload under the poisoned allocator.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_forwarded_return_carrier_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_forwarded_return_carrier_df",
        &fstring_forwarded_return_interp_loop_source(200),
    );
}

/// Both branches of the mixed carrier release exactly once under allocator
/// poisoning; the alternating predicate exercises each path 100 times.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_mixed_projection_forward_return_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_mixed_projection_forward_return_df",
        &fstring_mixed_projection_forward_return_loop_source(200),
    );
}

/// Regression for the borrowed parameter entry definition that has no MIR
/// writer. The false branch must return a retained share; otherwise the direct
/// consumer frees `owned` and its later read/drop trips the poisoned allocator.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_conditional_var_param_return_survives_malloc_scribble() {
    assert_no_double_free(
        "fstring_conditional_var_param_return_df",
        &fstring_conditional_var_param_return_loop_source(200),
    );
}

/// No-double-free pin: the `Option<string>` payload releases EXACTLY once
/// across 200 iterations, and stays readable after the interpolation.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_enum_payload_interp_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_enum_payload_df",
        &fstring_enum_payload_interp_loop_source(200),
    );
}

/// No-double-free pin: the `Result<string, string>` payload releases EXACTLY
/// once across 200 iterations.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_result_payload_interp_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_result_payload_df",
        &fstring_result_payload_interp_loop_source(200),
    );
}

/// Fail-closed pin (non-copy-in sink, enum payload): interpolating a payload
/// that ALSO escapes into an outer `var` must keep the composite excluded.
/// The program is expected to leak; what is pinned is that the aliased buffer
/// is never released twice — a double release scribbles `last`/`total` and
/// trips the fixture's own post-loop checks, or aborts outright.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_enum_payload_escape_is_not_released_twice_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_enum_payload_escape_df",
        &fstring_enum_payload_escapes_loop_source(200),
    );
}

/// Fail-closed pin (non-copy-in sink, call boundary): a fresh call-result
/// temp aliased by the callee into a record the caller keeps must not be
/// claimed by the temp-arg mint. Expected to leak; pinned to never release
/// the record's field out from under the caller's read.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn fstring_user_call_result_escape_is_not_released_twice_under_malloc_scribble() {
    assert_no_double_free(
        "fstring_user_call_result_escape_df",
        &fstring_user_call_result_escapes_loop_source(200),
    );
}
