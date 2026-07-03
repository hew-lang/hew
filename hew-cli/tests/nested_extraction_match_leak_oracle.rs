//! Nested-extraction partial-destructure oracle — the compiled-binary pins
//! for the alias-scrutinee ownership model.
//!
//! ## The shape
//!
//! ```text
//! let inner = outer.field;          // un-retained member byte-copy
//! match inner { Inner { a, b: _ } => … }
//! ```
//!
//! The extracted binding's storage is an interior ALIAS of the outer
//! aggregate: `RecordFieldLoad` / `TupleFieldLoad` byte-copy the member
//! with no retain (only `string`-typed dests retain), and an enum payload
//! binder (`match opt { Some(row) => … }`) aliases the composite's payload
//! storage the same way. The originals every wildcarded field points at
//! are owned by the OUTER aggregate, whose composite in-place drop frees
//! each of them exactly once at scope exit.
//!
//! A partial destructure of such an alias must therefore emit NO
//! skipped-field discharge (`lower_match_project`'s alias-scrutinee gate):
//! discharging through the alias frees heap the owner's composite walk
//! re-frees — `FieldDropInPlace` null-stores the ALIAS slot, never the
//! owner's, so the owner's slot still carries the freed pointer. Bound
//! `string` binders keep their retained `+1` with its own balancing drop;
//! the composite frees the original share.
//!
//! ## What regressed without the gate (reproduced, fresh builds)
//!
//! - record parent: `FieldDropInPlace` freed `inner.b` through the alias
//!   while the outer root's `RecordInPlace` stayed admitted (the prover's
//!   direct rule resolved only `alias_of`, never `field_binders`) —
//!   double-free: Guard-Malloc SIGSEGV / `free_cstring` sentinel abort on
//!   the second iteration.
//! - tuple parent: the blanket owning-sink scan (no `FieldDropInPlace`
//!   exemption) misread the discharge as an element escape and
//!   blanket-excluded every tuple root — the untouched sibling and the
//!   bound element's original leaked (~2 nodes/iteration).
//! - enum parent (two-step nested destructure): the payload-binder
//!   discharge left the composite's `EnumInPlace` admitted — the same
//!   double-free through the payload alias.
//!
//! The composite-drop provers' direct `FieldDropInPlace` rules (record:
//! provenance-precise via `field_binders`; tuple: `elem_binders` blanket;
//! enum: `payload_binders` blanket) remain the fail-closed net for any
//! emitter that does discharge through an alias — those are pinned
//! structurally in `hew-mir`'s prover tests.
//!
//! ## Unconstructible variant (pinned by diagnostic, not by a runtime test)
//!
//! "The same field discharged via two different match arms" cannot be
//! built: a record/tuple project `match` lowers as a single irrefutable
//! destructure and REFUSES arm guards fail-closed ("guarded record/tuple
//! match destructure" NYI), so no second arm can carry a second discharge
//! of the same field.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── looped slope fixtures ───────────────────────────────────────────────

/// Record parent: nested owned record extracted into a `let`, then
/// partially destructured (bound `a`, skipped `b`). The outer root's
/// composite must free `inner.a`'s original, `inner.b`, and the untouched
/// sibling `c` — exactly once each.
fn nested_record_extraction_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   a: string,\n\
         \x20   b: string,\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   inner: Inner,\n\
         \x20   c: string,\n\
         }}\n\
         \n\
         fn make_outer(k: i64) -> Outer {{\n\
         \x20   Outer {{\n\
         \x20       inner: Inner {{\n\
         \x20           a: \"nested-a-heap-payload\".to_upper(),\n\
         \x20           b: \"nested-b-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"nested-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let inner = o.inner;\n\
         \x20   let x = match inner {{\n\
         \x20       Inner {{ a, b: _ }} => a.len(),\n\
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
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Tuple parent: inner pair extracted into a `let`, then partially
/// destructured. The regression leaked the outer sibling `c` AND the bound
/// element's original (~2 nodes/iteration); the slope pins both freed.
fn nested_tuple_extraction_source(frames: usize) -> String {
    format!(
        "fn make_triple(k: i64) -> ((string, string), string) {{\n\
         \x20   let a = \"tuple-a-heap-payload\".to_upper();\n\
         \x20   let b = \"tuple-b-heap-payload\".to_upper();\n\
         \x20   let c = \"tuple-c-heap-payload\".to_upper();\n\
         \x20   ((a, b), c)\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let t = make_triple(k);\n\
         \x20   let inner = t.0;\n\
         \x20   let x = match inner {{\n\
         \x20       (a, _) => a.len(),\n\
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
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// The escapee-is-itself-composite variant: the skipped field of the
/// extracted binder is a nested owned RECORD (`sub: Leaf`), the shape with
/// NO null-store anywhere — a composite re-walk is a guaranteed
/// double-free, an emitted in-place drop through the alias the guaranteed
/// trigger.
fn nested_record_composite_skip_source(frames: usize) -> String {
    format!(
        "type Leaf {{\n\
         \x20   s: string,\n\
         \x20   t: string,\n\
         }}\n\
         \n\
         type Mid {{\n\
         \x20   x: string,\n\
         \x20   sub: Leaf,\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   mid: Mid,\n\
         \x20   c: string,\n\
         }}\n\
         \n\
         fn make_outer(k: i64) -> Outer {{\n\
         \x20   Outer {{\n\
         \x20       mid: Mid {{\n\
         \x20           x: \"mid-x-heap-payload\".to_upper(),\n\
         \x20           sub: Leaf {{\n\
         \x20               s: \"leaf-s-heap-payload\".to_upper(),\n\
         \x20               t: \"leaf-t-heap-payload\".to_upper(),\n\
         \x20           }},\n\
         \x20       }},\n\
         \x20       c: \"outer-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let x = match mid {{\n\
         \x20       Mid {{ x, sub: _ }} => x.len(),\n\
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
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Enum parent: the payload binder from a tag match is the alias scrutinee
/// of the inner project match — the two-step nested destructure. The
/// composite's `EnumInPlace` owns every payload original.
fn enum_payload_nested_destructure_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   a: string,\n\
         \x20   b: string,\n\
         }}\n\
         \n\
         enum Wrap {{\n\
         \x20   Item(Inner);\n\
         \x20   Nothing;\n\
         }}\n\
         \n\
         fn make_wrap(k: i64) -> Wrap {{\n\
         \x20   Wrap::Item(Inner {{\n\
         \x20       a: \"wrap-a-heap-payload\".to_upper(),\n\
         \x20       b: \"wrap-b-heap-payload\".to_upper(),\n\
         \x20   }})\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let w = make_wrap(k);\n\
         \x20   let x = match w {{\n\
         \x20       Wrap::Item(row) => {{\n\
         \x20           match row {{\n\
         \x20               Inner {{ a, b: _ }} => a.len(),\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20       Wrap::Nothing => 0,\n\
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
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── slope oracles ───────────────────────────────────────────────────────

/// Record-parent nested extraction holds a flat slope: `inner.b`,
/// `inner.a`'s original, and the sibling `c` are each freed exactly once
/// by the retained outer composite.
#[test]
fn nested_record_extraction_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("nested_record_extract", nested_record_extraction_source);
}

/// Tuple-parent nested extraction holds a flat slope — the regression
/// leaked ~2 nodes/iteration (the blanket-excluded composite stranded the
/// sibling and the bound element's original).
#[test]
fn nested_tuple_extraction_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("nested_tuple_extract", nested_tuple_extraction_source);
}

/// Skipped-composite (escapee-is-itself-composite) variant holds a flat
/// slope.
#[test]
fn nested_composite_skip_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "nested_composite_skip",
        nested_record_composite_skip_source,
    );
}

/// Enum-payload two-step nested destructure holds a flat slope.
#[test]
fn enum_payload_nested_destructure_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "enum_payload_nested",
        enum_payload_nested_destructure_source,
    );
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

/// Run one shape at a small iteration count under the poisoned allocator
/// and require a clean exit with the loop completing. The regression
/// aborted here (freed-memory scribbling clobbers the cstring header
/// sentinel, so the composite re-walk's second free aborts) or crashed
/// with SIGSEGV.
fn assert_scribble_clean(name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("nested-extract-scribble-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort/crash \
         here means a skipped-field discharge freed storage the owner's \
         composite drop re-walked (double-free through the extraction \
         alias);\n{}",
        describe_output(&output)
    );
}

/// Record parent: no composite re-walk of the alias-discharged field.
#[test]
fn nested_record_extraction_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("nested_record_extract", &nested_record_extraction_source(4));
}

/// Tuple parent: no re-walk, and no over-exclusion strand (the slope test
/// carries the leak half of the pin).
#[test]
fn nested_tuple_extraction_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("nested_tuple_extract", &nested_tuple_extraction_source(4));
}

/// Skipped-composite variant: inline composites have NO null-store, so
/// this is the shape where a re-walk is unconditionally fatal.
#[test]
fn nested_composite_skip_no_double_free_under_malloc_scribble() {
    assert_scribble_clean(
        "nested_composite_skip",
        &nested_record_composite_skip_source(4),
    );
}

/// Enum-payload two-step destructure: the composite's `EnumInPlace` must
/// not re-free the payload field discharged through the binder alias.
#[test]
fn enum_payload_nested_destructure_no_double_free_under_malloc_scribble() {
    assert_scribble_clean(
        "enum_payload_nested",
        &enum_payload_nested_destructure_source(4),
    );
}

// ── MIR emission pin ────────────────────────────────────────────────────

/// The alias-scrutinee gate's contract, pinned at the MIR level: an
/// extracted-binder destructure emits NO `FieldDropInPlace` (nothing may
/// discharge through the alias) and the OUTER root keeps its composite
/// in-place drop (it owns every original). This is the emission half of
/// the fix; the prover-side net is pinned in `hew-mir`'s unit tests.
#[test]
fn extracted_binder_destructure_emits_no_field_drop_and_keeps_composite() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("nested-extract-mir-pin-")
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join("nested_extract_pin.hew");
    std::fs::write(&hew_src, nested_record_extraction_source(4)).expect("write hew source");

    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", "elab"])
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir elab");
    assert!(
        output.status.success(),
        "elaborated-MIR dump must succeed:\n{}",
        describe_output(&output)
    );
    let dump = String::from_utf8_lossy(&output.stdout);

    assert!(
        !dump.contains("drop_field_in_place"),
        "an extracted-binder (alias) scrutinee must not discharge skipped \
         fields through the alias — the owner's composite walk would re-free \
         them; dump:\n{dump}"
    );
    assert!(
        dump.contains("kind=record_in_place"),
        "the outer root owns every original and must keep its composite \
         in-place drop; dump:\n{dump}"
    );
}
