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

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

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

/// Interior-alias scrutinee (`let mid = o.a;` byte-copies the member) with a
/// bound `string` field. The original belongs to the OUTER composite, so the
/// discharge MUST be gated off — this fixture pins that NO
/// `FieldDropInPlace` is emitted through the alias slot.
const ALIAS_SCRUTINEE_BOUND_STRING_SOURCE: &str = "\
type Alias {\n\
\x20   s: string,\n\
\x20   n: i64,\n\
}\n\
\n\
type Holder {\n\
\x20   a: Alias,\n\
\x20   h: i64,\n\
}\n\
\n\
fn f(o: Holder) -> i64 {\n\
\x20   let mid = o.a;\n\
\x20   let x = match mid {\n\
\x20       Alias { s, n } => s.len() + n,\n\
\x20   };\n\
\x20   x\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   f(Holder { a: Alias { s: \"x\".to_upper(), n: 1 }, h: 2 })\n\
}\n";

// ── slope oracles ───────────────────────────────────────────────────────

/// Bound `string` record field on a consumed root holds a flat leak slope —
/// the stranded-original pin (record parent). Losing the in-place discharge
/// leaks one original per iteration and trips the tolerance.
#[test]
fn bound_string_record_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("bound_string_record", bound_string_record_loop_source);
}

/// Bound `string` tuple element on a consumed root holds a flat leak slope —
/// the stranded-original pin (tuple parent).
#[test]
fn bound_string_tuple_element_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("bound_string_tuple", bound_string_tuple_loop_source);
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

/// The binder-used destructure runs clean under the poisoned allocator and
/// prints its sentinel — an over-eager free of the discharged original (or a
/// re-drop of the binder's clone) aborts here.
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

/// Run `hew compile --dump-mir raw` over `source` and return the dump.
/// Panics (with the compiler output) if MIR construction fails.
fn dump_raw_mir(name: &str, source: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix(&format!("bound-string-mir-pin-{name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", "raw"])
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir");
    assert!(
        output.status.success(),
        "MIR construction must succeed for {name}:\n{}",
        describe_output(&output)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// A bound `string` field on a CONSUMED, non-alias root emits exactly one
/// `FieldDropInPlace` carrying the string type — the stranded-original
/// discharge at the MIR level (the record parent).
#[test]
fn consumed_root_bound_string_emits_field_drop_in_place_mir() {
    require_codegen();

    let source = "\
type Inner {\n\
\x20   v: i64,\n\
}\n\
\n\
type Outer {\n\
\x20   inner: Inner,\n\
\x20   c: string,\n\
}\n\
\n\
fn consume(o: Outer) -> i64 {\n\
\x20   let x = match o {\n\
\x20       Outer { inner: _, c } => c.len(),\n\
\x20   };\n\
\x20   x\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   consume(Outer { inner: Inner { v: 0 }, c: \"s\" })\n\
}\n";
    let dump = dump_raw_mir("consumed_bound_string", source);
    let discharges = dump.matches("drop_field_in_place").count();
    assert_eq!(
        discharges, 1,
        "a bound string field on a consumed root must emit exactly one FieldDropInPlace; dump:\n{dump}"
    );
    assert!(
        dump.contains("ty=string"),
        "the bound-string discharge must carry the string type; dump:\n{dump}"
    );
}

/// An interior-alias scrutinee with a bound `string` field emits NO
/// `FieldDropInPlace` through the alias — the original belongs to the outer
/// composite, so discharging through the alias slot would double-free. The
/// alias gate is the authority.
#[test]
fn alias_scrutinee_bound_string_emits_no_field_discharge_mir() {
    require_codegen();

    let dump = dump_raw_mir("alias_bound_string", ALIAS_SCRUTINEE_BOUND_STRING_SOURCE);
    assert!(
        !dump.contains("drop_field_in_place"),
        "an interior-alias scrutinee must emit NO FieldDropInPlace — the outer composite owns \
         the original; discharging through the alias would double-free. dump:\n{dump}"
    );
}
