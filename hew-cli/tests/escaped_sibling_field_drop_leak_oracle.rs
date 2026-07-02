//! Escaped-sibling field-drop oracle (#2212) — the empirical
//! (compiled-binary) half of the sibling-discharge contract whose MIR side
//! lives in `examples/v05/checked-mir/escaped_sibling_field_drop.hew`.
//!
//! ## The shape
//!
//! Extracting ONE owned field out of a multi-owned-field record and letting
//! it escape (`fn extract() -> Inner { let b = Outer { .. }; b.inner }`)
//! excludes the record's composite scope-exit drop — the escapee owns that
//! field now. The record's NON-escaped owned siblings are still solely
//! owned by the record slot; without the in-place discharge they leaked at
//! exactly one allocation per frame (#2212: slope 1, 64 B `tag` nodes).
//!
//! `apply_escaped_record_sibling_field_drops` splices one
//! `Instr::FieldDropInPlace` per dischargeable sibling directly after the
//! escape, where the value flow proves the record's last use. The
//! composite-drop prover's per-binder attribution
//! (`attribute_field_binder_provenance`) additionally narrows the escape
//! exclusion to the escaping binder's OWN root, so an unrelated record in
//! the same function keeps its composite drop.
//!
//! ## The two safety boundaries (the R2 review targets)
//!
//! - The ESCAPED field is never discharged: the caller owns it. The
//!   scribble pin reads the escapee's heap payload after the callee's
//!   discharge ran — a wrong discharge is a poisoned read/abort.
//! - A record READ after the escape refuses the discharge entirely: the
//!   read-after-escape scribble pin observes the sibling's payload after
//!   the escape instruction, so an over-eager splice frees the buffer that
//!   read still observes.
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! Slope legs compile at LOW and HIGH iteration counts and the leak-NODE
//! delta must stay within tolerance (`support::leak_slope`); a regressed
//! discharge leaks one node per frame — an order of magnitude above the
//! tolerance. macOS-only for the slope legs (`leaks(1)`); the scribble
//! pins run on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── looped slope fixtures ───────────────────────────────────────────────

/// The #2212 repro: two owned fields, one escapes per frame, the sibling
/// must be discharged in place. Pre-fix slope: exactly 1 leak/frame (the
/// `tag` buffer).
fn escaped_sibling_loop_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   label: string;\n\
         \x20   n: i64;\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   inner: Inner;\n\
         \x20   tag: string;\n\
         }}\n\
         \n\
         fn extract(k: i64) -> Inner {{\n\
         \x20   let b = Outer {{\n\
         \x20       inner: Inner {{ label: \"escaped-inner-heap-payload\".to_upper(), n: k }},\n\
         \x20       tag: \"sibling-tag-heap-payload\".to_upper(),\n\
         \x20   }};\n\
         \x20   b.inner\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let inner = extract(i);\n\
         \x20       total = total + inner.n;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Attribution narrowing: a SECOND record in the extracting function whose
/// fields never escape must keep its composite drop while the first
/// record's binder escapes. Pre-attribution the blanket exclusion leaked
/// the bystander's two buffers per frame as well (slope 3); post-fix the
/// bystander drops composite, the escaping record's sibling discharges in
/// place, and the slope is flat.
fn escaped_sibling_bystander_loop_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   label: string;\n\
         \x20   n: i64;\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   inner: Inner;\n\
         \x20   tag: string;\n\
         }}\n\
         \n\
         fn extract(k: i64) -> Inner {{\n\
         \x20   let bystander = Outer {{\n\
         \x20       inner: Inner {{ label: \"bystander-inner-payload\".to_upper(), n: k }},\n\
         \x20       tag: \"bystander-tag-payload\".to_upper(),\n\
         \x20   }};\n\
         \x20   let b = Outer {{\n\
         \x20       inner: Inner {{ label: \"escaped-inner-heap-payload\".to_upper(), n: k }},\n\
         \x20       tag: \"sibling-tag-heap-payload\".to_upper(),\n\
         \x20   }};\n\
         \x20   b.inner\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let inner = extract(i);\n\
         \x20       total = total + inner.n;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scribble fixtures (single-cycle, exact stdout) ──────────────────────

/// The escapee's heap payload is READ after the callee's sibling discharge
/// ran. A discharge of the ESCAPED field would free the buffer this read
/// observes (poisoned read / abort under the scribbled allocator).
const ESCAPEE_USED_SCRIBBLE_SOURCE: &str = "\
type Inner {\n\
\x20   label: string;\n\
\x20   n: i64;\n\
}\n\
\n\
type Outer {\n\
\x20   inner: Inner;\n\
\x20   tag: string;\n\
}\n\
\n\
fn extract() -> Inner {\n\
\x20   let b = Outer {\n\
\x20       inner: Inner { label: \"escaped-inner-heap-payload\".to_upper(), n: 7 },\n\
\x20       tag: \"sibling-tag-heap-payload\".to_upper(),\n\
\x20   };\n\
\x20   b.inner\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let inner = extract();\n\
\x20   if inner.label.len() == 26 && inner.n == 7 {\n\
\x20       print(\"k\");\n\
\x20   }\n\
\x20   0\n\
}\n";

/// The record's sibling is READ after the escape instruction (the binder
/// packs into a fresh aggregate, then `b.tag` is read). The discharge must
/// REFUSE this shape — an over-eager splice at the escape frees the `tag`
/// buffer the later read observes. The refusal keeps the record's
/// remaining fields on the pre-fix leak path (bounded here: one cycle),
/// which is the fail-closed direction.
const READ_AFTER_ESCAPE_SCRIBBLE_SOURCE: &str = "\
type Inner {\n\
\x20   label: string;\n\
\x20   n: i64;\n\
}\n\
\n\
type Outer {\n\
\x20   inner: Inner;\n\
\x20   tag: string;\n\
}\n\
\n\
type Wrap {\n\
\x20   inner: Inner;\n\
}\n\
\n\
fn pack() -> Wrap {\n\
\x20   let b = Outer {\n\
\x20       inner: Inner { label: \"escaped-inner-heap-payload\".to_upper(), n: 7 },\n\
\x20       tag: \"sibling-tag-heap-payload\".to_upper(),\n\
\x20   };\n\
\x20   let w = Wrap { inner: b.inner };\n\
\x20   if b.tag.len() == 24 {\n\
\x20       return w;\n\
\x20   }\n\
\x20   w\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let w = pack();\n\
\x20   if w.inner.n == 7 {\n\
\x20       print(\"k\");\n\
\x20   }\n\
\x20   0\n\
}\n";

// ── slope oracles ───────────────────────────────────────────────────────

/// The #2212 repro holds a flat leak slope: the non-escaped `tag` sibling
/// is discharged in place once per frame. A regressed discharge leaks one
/// 24-byte buffer per frame and trips the tolerance.
#[test]
fn escaped_sibling_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("escaped_sibling", escaped_sibling_loop_source);
}

/// A bystander record in the same function holds a flat slope: per-binder
/// escape attribution excludes ONLY the escaping record, so the bystander
/// keeps its composite drop. Reverting to the blanket every-root exclusion
/// leaks the bystander's two buffers per frame as well.
#[test]
fn escaped_sibling_bystander_record_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "escaped_sibling_bystander",
        escaped_sibling_bystander_loop_source,
    );
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

/// The escaped field is never discharged: the caller reads the escapee's
/// heap payload after the callee ran the sibling discharge, clean under
/// the poisoned allocator and with the exact sentinel.
#[test]
fn escaped_field_not_dropped_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("escaped-sibling-escapee-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(ESCAPEE_USED_SCRIBBLE_SOURCE, dir.path(), "escapee_used");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "the escapee-read shape must run clean under the poisoned allocator — a crash \
         here means the discharge freed the ESCAPED field the caller still owns;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "k",
        "sentinel mismatch — the escapee's payload read poisoned bytes;\n{}",
        describe_output(&output)
    );
}

/// A record read after the escape refuses the discharge: the sibling read
/// (`b.tag.len()`) after the escape instruction observes live bytes, clean
/// under the poisoned allocator and with the exact sentinel.
#[test]
fn record_read_after_escape_not_discharged_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("escaped-sibling-read-after-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        READ_AFTER_ESCAPE_SCRIBBLE_SOURCE,
        dir.path(),
        "read_after_escape",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "the read-after-escape shape must run clean under the poisoned allocator — a \
         crash here means the discharge fired before a live read of the record;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "k",
        "sentinel mismatch — the post-escape sibling read poisoned bytes;\n{}",
        describe_output(&output)
    );
}
