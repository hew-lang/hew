//! Nested-`Vec` field-push drop oracle (hew-lang/hew#2721).
//!
//! ## The shape
//!
//! A record whose field is itself a collection (`type Rec { field: [i64] }`)
//! has that field pushed into a `Vec<Vec<i64>>` by COPY-IN:
//!
//! ```text
//! let r = Rec { field: [n, n + 1, n + 2] };
//! xs.push(r.field);            // hew_vec_push_owned — DEEP-CLONES the field
//! ```
//!
//! `hew_vec_push_owned` deep-clones its element operand into the slot, so `xs`
//! owns a DISJOINT clone and `r` keeps sole ownership of the ORIGINAL field
//! buffer. Two owners, two buffers, each freed exactly once: the container's
//! element drop-thunk frees the clone, and `r`'s scope-exit `RecordInPlace`
//! composite drop frees the original.
//!
//! ## What regressed (pre-fix, reproduced on the Stage-1 tip `882ff31d9`)
//!
//! The composite field-binder escape scan (`note_field_escape`) treated the
//! pushed field binder as a whole escape and excluded `r`'s composite drop —
//! as if the push CONSUMED the field. But copy-in clones, so excluding `r`
//! stranded the ORIGINAL field buffer: **2 leak nodes / frame** (one `Vec<i64>`
//! header + buffer) rooted at `hew_vec_new_with_elem_size`, on `882ff31d9`.
//! (Historically — pre the caller-side owned-param drop — the same mis-accounting
//! surfaced as a double-free; the caller-drop widening turned it into a leak, so
//! the anchor fix is now a missing-drop, not a wrong-drop.)
//!
//! ## The fix
//!
//! A field binder read as the operand of a COPY-IN element store is deep-cloned,
//! not consumed, so it is exempt from `note_field_escape` — mirroring the
//! whole-record copy-in alias-escape exemption already present. `r` is re-admitted
//! and its `RecordInPlace` frees the original field exactly once. The MOVE variant
//! (`hew_vec_push_owned_move`) stays non-exempt (it consumes its source).
//!
//! ## What this oracle pins
//!
//! * a flat per-iteration leak SLOPE across the LOW/HIGH frame delta — the
//!   original field buffer is freed every frame (was ~2 nodes/frame);
//! * no double-free / use-after-free under the poisoned allocator: an over-eager
//!   re-admission that ALSO freed the clone-source's original through a second
//!   owner would abort here. The exact-contents read-back proves the clone in
//!   `xs` is a disjoint deep copy, not an alias of the freed original.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// Per-frame build-push-read-drop shape for the leak slope. Each iteration
/// builds a fresh `Rec { field: [i, i+1, i+2] }`, pushes the field copy-in into
/// a fresh `Vec<Vec<i64>>`, reads both the clone (via `xs`) and the retained
/// original (via `r.field`), then lets both go out of scope on the back-edge.
/// Post-fix the clone (container drop) AND the original (record composite drop)
/// are freed each frame, so the leak count is flat.
fn field_push_drop_loop_source(frames: usize) -> String {
    format!(
        "type Rec {{ field: [i64] }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let xs: [[i64]] = [];\n\
         \x20       let r = Rec {{ field: [i, i + 1, i + 2] }};\n\
         \x20       xs.push(r.field);\n\
         \x20       total = total + xs[0].len() + r.field.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Deterministic exact-contents source: push a known field copy-in, then read
/// BOTH the clone (`xs[0][k]`) and the retained original (`r.field[k]`) and sum
/// them. If the fix mis-shares the clone and the original one buffer, the
/// scribbled allocator poisons the second read or aborts on the double-free.
const FIELD_PUSH_EXACT_SOURCE: &str = "type Rec { field: [i64] }\n\
     \n\
     fn main() {\n\
     \x20   let xs: [[i64]] = [];\n\
     \x20   let r = Rec { field: [10, 20, 30] };\n\
     \x20   xs.push(r.field);\n\
     \x20   let clone_sum = xs[0][0] + xs[0][1] + xs[0][2];\n\
     \x20   let orig_sum = r.field[0] + r.field[1] + r.field[2];\n\
     \x20   println(f\"{clone_sum},{orig_sum}\");\n\
     }\n";

/// Both the disjoint clone in `xs` and the retained original in `r` must read
/// back 60 (10+20+30) under the poisoned allocator.
const FIELD_PUSH_EXACT_EXPECTED: &str = "60,60\n";

/// Compile `source`, run under the poisoned-allocator triple, assert clean exit
/// with `expected` stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("nested-vec-field-push-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash indicates the pushed \
         copy-in clone and the record's retained original share one buffer that is freed twice \
         (#2721 over-eager re-admission);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the disjoint clone AND the retained original verbatim — a differing \
         value indicates the clone aliased the original buffer instead of a deep copy;\n{}",
        describe_output(&output)
    );
}

/// PRIMARY #2721 regression: the field-push build-drop loop must hold a flat
/// leak slope. Reverting the copy-in field-binder exemption strands the record's
/// original field buffer (~2 nodes/frame) and this fails.
#[test]
fn field_push_drop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("nested_vec_field_push", field_push_drop_loop_source);
}

/// Exact-contents / no-double-free pin: the deep-cloned element in `xs` and the
/// record's retained original are disjoint owners; both read back 60 and neither
/// double-frees under the poisoned allocator.
#[test]
fn field_push_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "field_push_exact",
        FIELD_PUSH_EXACT_SOURCE,
        FIELD_PUSH_EXACT_EXPECTED,
    );
}
