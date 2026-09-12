//! Bound-string original discharge oracle for match destructures that
//! CONSUME the scrutinee root — the empirical (compiled-binary) half of the
//! per-field accounting matrix whose skipped-field sibling lives in
//! `match_skipped_field_drop_leak_oracle.rs`.
//!
//! ## The stranded original
//!
//! When a `match` binds a `string`-typed field of an owned record/tuple and
//! the scrutinee root is CONSUMED by the destructure (a non-captured,
//! non-alias `BindingRef` with bindings), the binder owns a CLONE — codegen
//! retains string field loads via `hew_string_clone` — while the ORIGINAL
//! handle still sits in the root slot. The root's composite drop is
//! suppressed (the binder seeds `release_owner_bases`) and the consume mark
//! retracts the root from `owned_locals`, so nothing releases the original:
//! it leaks one buffer per destructure.
//!
//! The bind loop discharges the original IN PLACE right after the load —
//! `Instr::FieldDropInPlace` raw-loads the root's field handle, releases it,
//! and null-stores the slot. The binder is then the sole owner (its own drop
//! balances the retained clone), so the shape is leak-free AND double-free
//! free even when the arm returns the binder.
//!
//! ## Gating (the three double-free guards)
//!
//! The discharge fires ONLY when all three hold — each false gate would
//! double-free:
//!   - the scrutinee earns the consume mark (a non-consumed root keeps its
//!     composite drop, which frees the original);
//!   - the root is NOT an interior alias (an alias's original belongs to the
//!     OUTER composite, which frees it — `FieldDropInPlace` would null-store
//!     the alias slot, not the owner's);
//!   - the bound field is `string`-typed (the retaining-load class; non-
//!     string binders take the one handle and leave a dead root slot).
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! Each shape compiles at LOW and HIGH iteration counts and the leak-NODE
//! delta must stay within tolerance (`support::leak_slope`). A regressed
//! discharge leaks one node per iteration — an order of magnitude above the
//! tolerance. macOS-only for the slope legs (`leaks(1)`); the scribble and
//! MIR pins run on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── looped slope fixtures ───────────────────────────────────────────────

/// Record parent, BOUND `string` field, `BitCopy` sibling wildcarded, root
/// consumed, binder USED in the arm body. Pre-fix behaviour leaked one
/// `to_upper()` original per iteration (the binder held the clone, the root
/// slot's original was never released); the in-place discharge holds the
/// slope flat.
fn bound_string_record_loop_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   v: i64,\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   inner: Inner,\n\
         \x20   c: string,\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = Outer {{ inner: Inner {{ v: k }}, c: \"bound-string-heap\".to_upper() }};\n\
         \x20   let x = match o {{\n\
         \x20       Outer {{ inner: _, c }} => c.len(),\n\
         \x20   }};\n\
         \x20   x\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + run_cycle(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Tuple parent, BOUND `string` element, `BitCopy` element also bound, root
/// consumed — the same stranded-original hazard through `FieldAddr::Tuple`
/// addressing.
fn bound_string_tuple_loop_source(frames: usize) -> String {
    format!(
        "fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = (k, \"bound-tuple-heap\".to_upper());\n\
         \x20   let x = match o {{\n\
         \x20       (a, c) => a + c.len(),\n\
         \x20   }};\n\
         \x20   x\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + run_cycle(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scribble / escape fixtures (single-cycle, exact stdout) ─────────────

/// Single-cycle bound-string destructure that USES the binder (`c.len()`)
/// and prints a sentinel. A double-free of the discharged original — or of
/// the binder's clone — aborts under the poisoned allocator.
const BOUND_STRING_USED_SCRIBBLE_SOURCE: &str = "\
type Inner {\n\
\x20   v: i64,\n\
}\n\
\n\
type Outer {\n\
\x20   inner: Inner,\n\
\x20   c: string,\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let o = Outer { inner: Inner { v: 7 }, c: \"scribble-heap\".to_upper() };\n\
\x20   let n = match o {\n\
\x20       Outer { inner: _, c } => c.len(),\n\
\x20   };\n\
\x20   if n == 13 {\n\
\x20       print(\"k\");\n\
\x20   }\n\
\x20   0\n\
}\n";

/// Single-cycle destructure where the arm RETURNS the bound string and the
/// caller reads it. The binder carries its own `+1` (balanced by the
/// caller-side drop); the in-place discharge releases ONLY the root's share,
/// so returning the binder is neither a leak nor a use-after-free.
const BOUND_STRING_ESCAPE_SOURCE: &str = "\
type Inner {\n\
\x20   v: i64,\n\
}\n\
\n\
type Outer {\n\
\x20   inner: Inner,\n\
\x20   c: string,\n\
}\n\
\n\
fn take(o: Outer) -> string {\n\
\x20   match o {\n\
\x20       Outer { inner: _, c } => c,\n\
\x20   }\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let o = Outer { inner: Inner { v: 1 }, c: \"ESCAPE-HEAP\" };\n\
\x20   let s = take(o);\n\
\x20   print(s);\n\
\x20   0\n\
}\n";

// ── slope oracles ───────────────────────────────────────────────────────

/// Bound `string` record field on a consumed root holds a flat leak slope —
/// the stranded-original pin (record parent). Losing the in-place discharge
/// leaks one original per iteration and trips the tolerance.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn bound_string_record_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("bound_string_record", bound_string_record_loop_source);
}

/// Bound `string` tuple element on a consumed root holds a flat leak slope —
/// the stranded-original pin (tuple parent).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn bound_string_tuple_element_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("bound_string_tuple", bound_string_tuple_loop_source);
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

/// The binder-used destructure runs clean under the poisoned allocator and
/// prints its sentinel — an over-eager free of the discharged original (or a
/// re-drop of the binder's clone) aborts here.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn bound_string_binder_used_no_double_free_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("bound-string-used-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BOUND_STRING_USED_SCRIBBLE_SOURCE,
        dir.path(),
        "bound_string_used_scribble",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "bound-string destructure must run clean under the poisoned allocator — a crash \
         here means the original discharge double-freed the binder's clone;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "k",
        "sentinel mismatch — scribbled output indicates a use-after-free;\n{}",
        describe_output(&output)
    );
}

/// The escaping-binder shape runs clean under the poisoned allocator and
/// prints the returned string exactly — the in-place discharge releases only
/// the root's share, so the binder survives the return and the caller reads
/// live memory.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn bound_string_binder_escapes_no_double_free_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("bound-string-escape-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BOUND_STRING_ESCAPE_SOURCE,
        dir.path(),
        "bound_string_escape_scribble",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "escaping bound-string binder must run clean under the poisoned allocator — a crash \
         here means the discharge freed the binder's handle out from under the caller;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "ESCAPE-HEAP",
        "returned-binder output mismatch — indicates a use-after-free of the escaped string;\n{}",
        describe_output(&output)
    );
}

// ── MIR emission pins ────────────────────────────────────────────────────
//
// Lost coverage: `--dump-mir raw` (retired) previously pinned two facts by
// grepping/counting `FieldDropInPlace` occurrences in the raw MIR text: a
// bound `string` field on a consumed root emits exactly one discharge
// (`ty=string`), and an interior-alias scrutinee emits none (the negative
// control — discharging through an alias would double-free the outer
// composite's original). Physical MIR's structured (Debug) dump has no
// equivalent single-line text to grep or count occurrences of, so this
// MIR-emission coverage has no direct replacement here. The same
// use/no-double-free behaviour for a bound-string binder is still proven
// end to end above by the leak-slope and malloc-scribble oracles in this
// file.
