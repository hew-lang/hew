//! Owned-element `Vec<record-with-collection-field>` array-literal leak oracle
//! (v0.6-generic-direct-call).
//!
//! A record whose field is itself a collection (`type Boxed { payload: [i64] }`)
//! owns heap through that field even when every element is `BitCopy`. The three
//! heap-ownership authorities (`ty_contains_heap_owning`, the MIR
//! `named_elem_owns_heap` harvest, codegen `resolved_ty_contains_heap_leaf`)
//! historically treated only `string`/`bytes` as heap leaves and recursed into a
//! collection's element type-args, so `Boxed` classified non-heap-owning. A
//! `Vec<Boxed>` then fell through BOTH the owned-element drop arm (the harvest
//! never registered `Boxed`) AND the plain-vec arm (`ValueClass::of_ty(Boxed)`
//! correctly says `CowValue`), landing in NO drop class — the whole chain (outer
//! buffer + element records + each element's `payload` buffer) leaked. Indexing
//! the Vec (`xs[0]`) made it observable end to end.
//!
//! A second layer: once `Vec<Boxed>` classifies owned, the array-literal desugar
//! pushed each element with the COPY-IN `hew_vec_push_owned` (deep clone), which
//! leaked the fresh `record_init` temp the clone was made from — the array
//! literal's element is a throwaway temp with no scope-exit drop to retain. The
//! fix routes array-literal owned pushes to `hew_vec_push_owned_move` (a
//! byte-copy heap transfer with no clone), so the element's heap moves into the
//! Vec and the Vec's `hew_vec_free_owned` releases it exactly once.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): an
//!   array literal of `Boxed { payload: [..] }` is indexed and every element's
//!   payload cell is read back. Pre-fix this leaks (silent) or, under a
//!   double-free regression, aborts; the exact-string equality plus the clean
//!   exit pin both directions.
//! - **Per-frame leak slope** (macOS-only via `leaks(1)`): build-and-drop a
//!   `Vec<Boxed>` per frame. Post-fix the leak-node count is flat across frame
//!   counts; a regression that drops the outer Vec shallowly (or fails to move
//!   the element in) shows a per-frame slope.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// An array literal of `Boxed { payload: [i64] }` indexed and read back. Each
/// element owns a heap `payload` buffer; the outer `Vec<Boxed>` must be built
/// through the owned-element descriptor ABI and freed with `hew_vec_free_owned`,
/// which runs `__hew_record_drop_inplace_Boxed` (freeing each `payload`) over
/// every live element. Reads `payload` cells from two distinct elements so a
/// shallow/garbage element changes the output verbatim.
const VEC_BOXED_ARRAY_LITERAL_SOURCE: &str = "\
type Boxed {\n\
\x20   payload: [i64];\n\
}\n\
\n\
fn main() {\n\
\x20   let xs = [Boxed { payload: [10, 20] }, Boxed { payload: [30, 40] }];\n\
\x20   let a = xs[0];\n\
\x20   let b = xs[1];\n\
\x20   print(f\"{a.payload[0]}|{a.payload[1]}|{b.payload[0]}|{b.payload[1]}\");\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact output for `VEC_BOXED_ARRAY_LITERAL_SOURCE`.
const VEC_BOXED_ARRAY_LITERAL_EXPECTED: &str = "10|20|30|40OK";

/// Per-frame build-and-drop shape for the leak slope: each iteration builds a
/// fresh `Vec<Boxed>` array literal, reads it, and lets it drop on the
/// back-edge. Post-fix the outer drop runs each element's record drop thunk and
/// frees its `payload` buffer, so the leak count is flat across frame counts.
fn vec_boxed_drop_loop_source(frames: usize) -> String {
    format!(
        "type Boxed {{\n\
         \x20   payload: [i64];\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let xs = [Boxed {{ payload: [10, 20] }}, Boxed {{ payload: [30] }}];\n\
         \x20       let a = xs[0];\n\
         \x20       total = total + a.payload[0] + xs.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-record-collection-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free of an owned-element record payload (a move-in/clone-in mismatch);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back every element's payload cells verbatim — a differing/garbage cell \
         indicates the element record or its collection field was shallow-copied or dropped \
         early;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: an array literal of `Boxed { payload: [i64] }` indexed
/// and read back must print every payload cell verbatim and exit clean under the
/// poisoned allocator. Reverting the collection-field heap-leaf classification
/// (so `Vec<Boxed>` is built plain) or the array-literal move-in (so the element
/// temp double-frees / aliases) fails this.
#[test]
fn vec_boxed_array_literal_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_boxed_array_literal",
        VEC_BOXED_ARRAY_LITERAL_SOURCE,
        VEC_BOXED_ARRAY_LITERAL_EXPECTED,
    );
}

/// Forward leak guard: building and dropping a `Vec<Boxed>` (record with a
/// `[i64]` collection field) per frame must not grow the leak-node count with
/// the frame count. Post-fix the outer drop runs the synthesized record drop
/// thunk, releasing each element's `payload` buffer. A regression that
/// mis-classifies the record non-heap-owning (no drop at all) shows a per-frame
/// slope this catches.
#[test]
fn vec_boxed_outer_drop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("vec_boxed_drop_loop", vec_boxed_drop_loop_source);
}
