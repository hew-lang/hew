//! Conditionally-moved collection locals — leak-slope oracles plus
//! poisoned-allocator exactly-once pins (#2418).
//!
//! Empirical oracle for the conditional-move leak class: an owned collection
//! local moved out on only SOME control-flow paths (`let xs = make(); if take
//! { let ys = xs; }`) was retracted from the scope-exit set at its consume
//! site path-insensitively, so the NOT-moved path leaked the value at return
//! (2 nodes / 176 B per small `Vec`). The fix keeps the registration behind a
//! path-sensitive runtime drop-flag (`hew-mir/src/lower.rs`,
//! `collection_drop_flags`): the not-moved path releases at scope exit, the
//! moved path skips — an unguarded release there would double-free, because
//! the whole-value move does not null the source slot.
//!
//! ## Slope methodology
//!
//! Shared harness (`support::leak_slope`): compile the same shape at LOW and
//! HIGH iteration counts, measure leak NODE counts under `leaks --atExit`
//! with the poisoned-allocator triple, assert the delta stays within the
//! tolerance. The pre-fix defect is PER-CALL GROWTH on the not-taken path —
//! 2 nodes per call for `Vec<i64>`, more for `Vec<string>` — which over the
//! 47-frame delta lands an order of magnitude above the +5 tolerance.
//!
//! ## Exactly-once pins
//!
//! The moved path must NOT double-free: the guarded scope-exit drop has to
//! skip where the arm's owner already released. The pin binaries run every
//! shape (taken and not-taken branches, nested joins, loop + break, the
//! array-literal chain, `HashMap`, `Vec<string>`) to completion under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and must print
//! the exact checksum — a double-free aborts (or scribbles the values)
//! before the output completes.
//!
//! ## Skip behaviour
//!
//! Slope oracles are macOS-only (`leaks(1)`); elsewhere they log `skip:` and
//! return. The scribble pins run on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope fixtures — the not-taken path leaks per call pre-fix ────────────

/// `Vec<i64>` conditionally moved, branch never taken: one leaked vec per
/// call pre-fix (the primary #2418 shape).
fn vec_conditional_not_taken_source(frames: usize) -> String {
    format!(
        "fn make_vec() -> Vec<i64> {{\n\
         \x20   let v: Vec<i64> = Vec::new();\n\
         \x20   v.push(40);\n\
         \x20   v.push(2);\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn probe(take: bool) -> i64 {{\n\
         \x20   let xs = make_vec();\n\
         \x20   var out: i64 = 1;\n\
         \x20   if take {{\n\
         \x20       let ys = xs;\n\
         \x20       out = ys.len();\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + probe(false);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// `Vec<string>` variant: the guarded `hew_vec_free` must release the element
/// copies through the runtime's `ElemKind::String` walk as well.
fn vec_string_conditional_not_taken_source(frames: usize) -> String {
    format!(
        "fn make_strs() -> Vec<string> {{\n\
         \x20   let v: Vec<string> = Vec::new();\n\
         \x20   v.push(\"per-call-element-one\");\n\
         \x20   v.push(\"per-call-element-two\");\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn probe(take: bool) -> i64 {{\n\
         \x20   let xs = make_strs();\n\
         \x20   var out: i64 = 1;\n\
         \x20   if take {{\n\
         \x20       let ys = xs;\n\
         \x20       out = ys.len();\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + probe(false);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// `HashMap` handle variant — the collection-handle class rides the same
/// drop-flag machinery.
fn hashmap_conditional_not_taken_source(frames: usize) -> String {
    format!(
        "fn probe(take: bool) -> i64 {{\n\
         \x20   let m: HashMap<string, i64> = HashMap::new();\n\
         \x20   m.insert(\"k\", 42);\n\
         \x20   var out: i64 = 1;\n\
         \x20   if take {{\n\
         \x20       let n = m;\n\
         \x20       out = n.len();\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + probe(false);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// TAKEN path per call: the arm's owner (`ys`) releases and the guarded
/// source drop must SKIP — a mis-gated guard shows as either a crash under
/// the scribble pin below or, for a silently-tolerated re-free, would not
/// leak; the slope here pins the leak direction of the taken path (the flag
/// must not suppress the arm owner's release).
fn vec_conditional_taken_source(frames: usize) -> String {
    format!(
        "fn make_vec() -> Vec<i64> {{\n\
         \x20   let v: Vec<i64> = Vec::new();\n\
         \x20   v.push(40);\n\
         \x20   v.push(2);\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn probe(take: bool) -> i64 {{\n\
         \x20   let xs = make_vec();\n\
         \x20   var out: i64 = 1;\n\
         \x20   if take {{\n\
         \x20       let ys = xs;\n\
         \x20       out = ys.len();\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + probe(true);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── exactly-once pin — every shape, both branches, poisoned allocator ─────

/// Every fixed shape in one binary: repro both branches, nested joins,
/// conditional move in a loop with break (taken and never-taken), the
/// array-literal hand-off chain, `HashMap`, and `Vec<string>`. Expected
/// output is the concatenated per-shape checksums + `OK`.
const EXACTLY_ONCE_PIN_SOURCE: &str = "\
fn make_vec() -> Vec<i64> {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   return v;\n\
}\n\
\n\
fn make_strs() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec::new();\n\
\x20   v.push(\"alpha\");\n\
\x20   v.push(\"beta\");\n\
\x20   return v;\n\
}\n\
\n\
fn cond_move(take: bool) -> i64 {\n\
\x20   let xs = make_vec();\n\
\x20   var out: i64 = 0;\n\
\x20   if take {\n\
\x20       let ys = xs;\n\
\x20       out = ys.len();\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
fn nested(a: bool, b: bool) -> i64 {\n\
\x20   let xs = make_vec();\n\
\x20   var out: i64 = 0;\n\
\x20   if a {\n\
\x20       if b {\n\
\x20           let ys = xs;\n\
\x20           out = out + ys.len();\n\
\x20       }\n\
\x20       out = out + 1;\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
fn loop_move(limit: i64) -> i64 {\n\
\x20   let xs = make_vec();\n\
\x20   var hits: i64 = 0;\n\
\x20   for i in 0..4 {\n\
\x20       if i == limit {\n\
\x20           let ys = xs;\n\
\x20           hits = hits + ys.len();\n\
\x20           break;\n\
\x20       }\n\
\x20       hits = hits + 1;\n\
\x20   }\n\
\x20   hits\n\
}\n\
\n\
fn array_chain(take: bool) -> i64 {\n\
\x20   let v: Vec<i64> = [7, 8, 9];\n\
\x20   var out: i64 = 1;\n\
\x20   if take {\n\
\x20       let w = v;\n\
\x20       out = w.len();\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
fn map_move(take: bool) -> i64 {\n\
\x20   let m: HashMap<string, i64> = HashMap::new();\n\
\x20   m.insert(\"k\", 42);\n\
\x20   var out: i64 = 2;\n\
\x20   if take {\n\
\x20       let n = m;\n\
\x20       out = n.len();\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
fn strs_move(take: bool) -> i64 {\n\
\x20   let xs = make_strs();\n\
\x20   var out: i64 = 3;\n\
\x20   if take {\n\
\x20       let ys = xs;\n\
\x20       out = ys.len();\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
fn main() {\n\
\x20   print(cond_move(false));\n\
\x20   print(cond_move(true));\n\
\x20   print(nested(false, false));\n\
\x20   print(nested(true, false));\n\
\x20   print(nested(true, true));\n\
\x20   print(loop_move(2));\n\
\x20   print(loop_move(9));\n\
\x20   print(array_chain(false));\n\
\x20   print(array_chain(true));\n\
\x20   print(map_move(false));\n\
\x20   print(map_move(true));\n\
\x20   print(strs_move(false));\n\
\x20   print(strs_move(true));\n\
\x20   print(\"OK\");\n\
}\n";

/// The concatenated `print` output of [`EXACTLY_ONCE_PIN_SOURCE`]:
/// `cond_move` 0,2 · `nested` 0,1,3 · `loop_move` 4,4 · `array_chain` 1,3 ·
/// `map_move` 2,1 · `strs_move` 3,2 · OK.
const EXACTLY_ONCE_PIN_EXPECTED: &str = "0201344132132OK";

// ── oracles ───────────────────────────────────────────────────────────────

/// Not-taken conditional move of a `Vec<i64>` must not leak per call.
/// Pre-fix slope is 2 nodes/call (handle + buffer, never registered for the
/// scope-exit drop); post-fix the guarded drop releases each call's vec.
#[test]
fn vec_conditional_move_not_taken_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_conditional_not_taken",
        vec_conditional_not_taken_source,
    );
}

/// `Vec<string>` variant: the guarded free must walk the string elements.
#[test]
fn vec_string_conditional_move_not_taken_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance(
        "vec_string_conditional_not_taken",
        vec_string_conditional_not_taken_source,
    );
}

/// `HashMap` handle variant of the not-taken conditional move.
#[test]
fn hashmap_conditional_move_not_taken_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance(
        "hashmap_conditional_not_taken",
        hashmap_conditional_not_taken_source,
    );
}

/// Taken path: the arm owner's release must keep firing (the guard gates
/// only the source's scope-exit drop, never the destination's).
#[test]
fn vec_conditional_move_taken_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance("vec_conditional_taken", vec_conditional_taken_source);
}

/// Exactly-once pin: every shape (both branches) runs to completion under
/// the poisoned-allocator triple and prints the exact checksums. A
/// double-free — the guarded source drop firing on the moved path, or the
/// dedup admitting two owners over one handle — aborts or scribbles the
/// output before `OK`.
#[test]
fn conditional_move_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("conditional-move-pin-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(EXACTLY_ONCE_PIN_SOURCE, dir.path(), "exactly_once_pin");

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "conditional-move shapes must run clean under the poisoned allocator \
         — a crash indicates the guarded scope-exit drop fired on the moved \
         path (double free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        EXACTLY_ONCE_PIN_EXPECTED,
        "conditional-move shapes must print the exact checksums — a \
         scribbled value indicates a freed handle was still read;\n{}",
        describe_output(&output)
    );
}
