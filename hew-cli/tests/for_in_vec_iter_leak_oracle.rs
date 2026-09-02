//! `for x in vec` cursor (`VecIter<T>`) scope-exit drop oracles: per-iteration
//! leak slope plus a poisoned-allocator no-use-after-free pin for the
//! place-source reuse shape (issue #1949).
//!
//! Empirical oracle for the for-in cursor leak class. A `for x in <vec>` loop
//! desugars to a synthetic `VecIter<T> { vec, idx }` cursor that holds the
//! source `Vec` handle. Pre-fix neither the source binding nor the cursor freed
//! that handle on loop exit, so every for-in over a fresh per-iteration Vec
//! leaked the handle and its buffer (two nodes per iteration). The fix splits
//! ownership by source shape:
//!   - **place source** (`for x in v`): the source binding keeps its own
//!     scope-exit drop (`derive_local_collection_drop_allowed` exempts the
//!     cursor's `CowShare` ingress) and the cursor BORROWS — it does not drop.
//!   - **rvalue / consumed source** (`for x in make_vec()`, `for x in
//!     v.into_iter()`): the cursor solely owns the handle and frees it via a
//!     per-scope-exit `RecordFieldDrop` on the cursor's `vec` field on every
//!     outer-loop iteration (`emit_scope_vec_iter_drops`).
//!
//! ## Element-class scope (leak #3, closed)
//!
//! Snapshot ownership is decided by the source topology
//! (`vec_iter_let_cursor_owns_handle` plus the actor-state / owned-index
//! projection exclusions). Yield ownership is now independent of ordinary
//! indexing: `VecIter::next` always calls the descriptor-backed
//! `hew_vec_get_clone -> Option<T>` choke. Every admitted element is therefore
//! a fresh owner, including nested `Vec`, `HashMap`, and `HashSet` values.
//! Ordinary `xs[i]` keeps its existing borrowing getters.
//!
//! The checker proves clone totality before constructing or advancing a
//! `VecIter`; closure pairs and opaque/resource values are rejected. MIR may
//! therefore release both sides independently: the common per-yield lifecycle
//! releases an unescaped clone at body/edge exit, while the cursor release
//! walks the complete snapshot, including slots never yielded because of an
//! early exit.
//!
//! The `for_in_owned_iter_move*` pins prove the admitted (deep-copy) classes stay
//! sound when the body moves a yielded element into another owner, through both a
//! copy-in sink (`Vec::push`) and a non-copy-in sink (`HashMap::insert`). A
//! borrowing `for x in v` (which shares the still-live source's buffer) is still
//! never freed by the cursor — the `for_in_owned_element_consume_*` pins hold
//! that boundary.
//!
//! ## Early-exit edges (leak #3, round 3)
//!
//! The scope-exit release above is LEXICAL: it runs on the fall-through path out
//! of the cursor's desugar block, and it dispositions the cursor `ScopeReleased`
//! — which also removes it from the function-exit LIFO the terminator DROP PLANS
//! are built from. An early `return` / `break @outer` / `continue @outer` jumps
//! past that block, so pre-fix such a cursor had NO release on either path and
//! leaked its whole snapshot tree once per call.
//! `emit_vec_iter_drops_for_exit_edge` emits the release inline on those edges,
//! bounded to `active_scopes[min_scope_depth..]` so a `break`/`continue` of the
//! cursor's OWN loop is excluded (that edge lands inside the desugar block, whose
//! close is still the single release). The `for_in_early_*` / `for_in_labelled_*`
//! oracles assert an EXACT ZERO leak count on each edge; the `*_sink` pins prove
//! the added release is still single-free through a NON-COPY-IN sink; and the
//! nested-yield early-return pair proves both the escaped clone and the
//! abandoned snapshot remain independently owned.
//!
//! ## Slope methodology
//!
//! Mirrors `vec_local_drop_leak_oracle.rs`: compile the same shape at a LOW
//! frame count and a HIGH frame count, measure leak NODE counts under `leaks
//! --atExit` with the poisoned-allocator triple, and assert the delta stays
//! within a small constant independent of frames. The pre-fix bug class is
//! PER-FRAME GROWTH (one leaked cursor handle + buffer per loop iteration —
//! the measured pre-fix delta over `50 - 3 = 47` frames was 94 nodes), an
//! order of magnitude above the +5 tolerance.
//!
//! ## No-use-after-free pin
//!
//! The place-source shape (`for x in v { … } …; v[0]`) reuses the source `v`
//! AFTER the loop. The cursor must NOT free the shared handle at loop exit (a
//! use-after-free against the post-loop read AND a double-free against the
//! source binding's own scope-exit drop). The pin runs the shape under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and asserts the
//! exact post-loop checksum: a producer-side double-free or a scribbled value
//! aborts (or corrupts the checksum) before the assertion is read.
//!
//! ## Skip behaviour
//!
//! The slope oracles are macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); on other platforms they log `skip:` and return. The scribble
//! pin runs on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::leak_slope::{
    measure_leaks, require_leaks_tool, require_macos_poisoned_allocator, run_probe_witness,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge path at least twice while
/// staying close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. The pre-fix slope was ~2.0 leak/frame
/// (cursor handle + buffer per iteration), producing `HIGH_FRAMES - LOW_FRAMES
/// = 47`-times-two excess nodes against the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off
/// scheduler/runtime allocations that appear only in the HIGH run while still
/// catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Fixture A — the rvalue-source shape. `make_vec(i)` returns a fresh Vec each
/// iteration; the for-in cursor is its sole owner. Pre-fix: the cursor handle
/// and its buffer leak every iteration. Post-fix: the cursor's scope-exit
/// `RecordFieldDrop` frees the `vec` handle on every outer iteration.
fn rvalue_source_loop_source(frames: usize) -> String {
    format!(
        "fn make_vec(n: i64) -> Vec<i64> {{\n\
         \x20   let v: Vec<i64> = Vec.new();\n\
         \x20   v.push(n);\n\
         \x20   v.push(n * 2);\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       for x in make_vec(i) {{\n\
         \x20           total = total + x;\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture B — the place-source shape. A fresh per-iteration `let v` is
/// CowShare-captured into the cursor; the source binding `v` (not the cursor)
/// owns the handle and frees it on the while back-edge. Both pre-fix leak
/// classes (no source drop, no cursor drop) collapsed to the same per-frame
/// growth; post-fix the source binding's drop fires every iteration.
fn place_source_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<i64> = Vec.new();\n\
         \x20       v.push(i);\n\
         \x20       v.push(i * 2);\n\
         \x20       for x in v {{\n\
         \x20           total = total + x;\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// `HashSet` field projection with owned string elements. Each outer frame owns a
/// record-held set, `to_vec()` creates a fresh owned snapshot, and `VecIter`
/// clone-out creates one independently droppable string per body iteration.
fn hashset_owned_field_loop_source(frames: usize) -> String {
    format!(
        "type SetBox {{ words: HashSet<string> }}\n\
         \n\
         fn make_box() -> SetBox {{\n\
         \x20   let words: HashSet<string> = HashSet.new();\n\
         \x20   words.insert(\"field\" + \"-alpha-padding-padding\");\n\
         \x20   words.insert(\"field\" + \"-beta-padding-padding\");\n\
         \x20   words.insert(\"field\" + \"-gamma-padding-padding\");\n\
         \x20   SetBox {{ words: words }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let holder = make_box();\n\
         \x20       var total: i64 = 0;\n\
         \x20       var count: i64 = 0;\n\
         \x20       for word in holder.words {{\n\
         \x20           total = total + word.len();\n\
         \x20           count = count + 1;\n\
         \x20       }}\n\
         \x20       if count != 3 || holder.words.len() != 3 {{ return 91; }}\n\
         \x20       println(total);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Fixture C — the `Vec<bool>` rvalue source. A second `BitCopy`-element width
/// (1-byte) confirms the cursor's `hew_vec_free` release is element-width
/// agnostic and the slope stays flat.
fn bool_rvalue_source_loop_source(frames: usize) -> String {
    format!(
        "fn make_vec(n: i64) -> Vec<bool> {{\n\
         \x20   let v: Vec<bool> = Vec.new();\n\
         \x20   v.push(true);\n\
         \x20   v.push(false);\n\
         \x20   return v;\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       for b in make_vec(i) {{\n\
         \x20           if b {{\n\
         \x20               total = total + 1;\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture D (leak #3) — an owned-RECORD `.iter()` snapshot. `v.iter()` over a
/// `Vec<Item>` (a two-`string` record) deep-clones the receiver into a fresh
/// `hew_vec_clone_owned` snapshot the cursor solely owns; each iteration reads
/// each element back as an independent `hew_vec_get_clone`. Pre-fix the snapshot
/// was excluded from the cursor drop registration by the `BitCopy`-only gate and
/// its whole tree (buffer + every element's two strings) leaked every iteration
/// (~6 nodes/frame). Post-fix the cursor's scope-exit `RecordFieldDrop` frees the
/// snapshot via `hew_vec_free_owned` on every outer iteration.
fn owned_record_iter_loop_source(frames: usize) -> String {
    format!(
        "type Item {{ name: string, tag: string }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<Item> = Vec.new();\n\
         \x20       v.push(Item {{ name: \"iter-owned-name\", tag: \"iter-owned-tag\" }});\n\
         \x20       v.push(Item {{ name: \"iter-owned-two\", tag: \"iter-owned-2t\" }});\n\
         \x20       for it in v.iter() {{\n\
         \x20           total = total + it.name.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

/// Fixture E (leak #3) — a `Vec<string>` `.iter()` snapshot. `string` is a
/// `Plain` Vec element (the runtime's `ElemKind::String` walk releases each slot
/// inside `hew_vec_free`), so the cursor's snapshot release is `hew_vec_free`,
/// not the owned-element ABI — a distinct release from Fixture D over the same
/// leak class. Pre-fix the snapshot leaked every iteration; post-fix it is freed.
fn string_iter_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<string> = Vec.new();\n\
         \x20       v.push(\"iter-string-alpha\");\n\
         \x20       v.push(\"iter-string-beta\");\n\
         \x20       for s in v.iter() {{\n\
         \x20           total = total + s.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

/// Cursor-only omission: the nested snapshot is empty, so no yield exists to
/// hide a missing cursor release. Each outer iteration must free both the
/// `.iter()` snapshot and the still-live source.
fn empty_nested_vec_iter_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let rows: Vec<Vec<string>> = Vec.new();\n\
         \x20       for row in rows.iter() {{\n\
         \x20           let _ = row.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   i % 7\n\
         }}\n"
    )
}

/// Per-yield omission: eight nested Vec clones are discarded on every frame.
/// A missing body-end release grows with yields even if cursor cleanup is
/// correct; a missing cursor release grows with the snapshot tree.
fn nested_vec_full_drain_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let rows: Vec<Vec<string>> = Vec.new();\n\
         \x20       var r: i64 = 0;\n\
         \x20       while r < 8 {{\n\
         \x20           let row: Vec<string> = Vec.new();\n\
         \x20           row.push(\"nested\" + \"-value\");\n\
         \x20           rows.push(row);\n\
         \x20           r = r + 1;\n\
         \x20       }}\n\
         \x20       for row in rows.iter() {{\n\
         \x20           total = total + row.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

fn nested_hashmap_full_drain_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let maps: Vec<HashMap<string, i64>> = Vec.new();\n\
         \x20       let map: HashMap<string, i64> = HashMap.new();\n\
         \x20       map.insert(\"nested\" + \"-key\", 7);\n\
         \x20       maps.push(map);\n\
         \x20       for yielded in maps.iter() {{\n\
         \x20           total = total + yielded.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

fn nested_hashset_full_drain_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let sets: Vec<HashSet<string>> = Vec.new();\n\
         \x20       let set: HashSet<string> = HashSet.new();\n\
         \x20       set.insert(\"nested\" + \"-member\");\n\
         \x20       sets.push(set);\n\
         \x20       for yielded in sets.iter() {{\n\
         \x20           total = total + yielded.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

/// Partial-drain omission: one clone is yielded and discarded, then the cursor
/// must release a snapshot that still contains two un-yielded rows.
fn nested_vec_break_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let rows: Vec<Vec<string>> = Vec.new();\n\
         \x20       var r: i64 = 0;\n\
         \x20       while r < 3 {{\n\
         \x20           let row: Vec<string> = Vec.new();\n\
         \x20           row.push(\"partial\" + \"-value\");\n\
         \x20           rows.push(row);\n\
         \x20           r = r + 1;\n\
         \x20       }}\n\
         \x20       for row in rows.iter() {{\n\
         \x20           total = total + row.len();\n\
         \x20           break;\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

/// Structural aggregate yield: both string fields of every tuple clone must be
/// released at body exit, then both copies in the cursor snapshot at cursor
/// exit. A whole-value no-op drop leaks four string owners per frame.
fn tuple_string_full_drain_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let pairs: Vec<(string, string)> = Vec.new();\n\
         \x20       pairs.push((\"tuple\" + \"-left\", \"tuple\" + \"-right\"));\n\
         \x20       pairs.push((\"second\" + \"-left\", \"second\" + \"-right\"));\n\
         \x20       for pair in pairs.iter() {{\n\
         \x20           total = total + pair.0.len() + pair.1.len();\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

/// First-class root-scope cursor plus a discarded `next()` result. Each call
/// must release the Option payload immediately and the cursor snapshot on the
/// function fallthrough edge.
fn manual_cursor_ignored_next_source(frames: usize) -> String {
    format!(
        "fn frame() -> i64 {{\n\
         \x20   let rows: Vec<Vec<string>> = Vec.new();\n\
         \x20   let row: Vec<string> = Vec.new();\n\
         \x20   row.push(\"manual\" + \"-payload\");\n\
         \x20   rows.push(row);\n\
         \x20   var cursor = rows.iter();\n\
         \x20   let _ = cursor.next();\n\
         \x20   1\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total % 7\n\
         }}\n"
    )
}

/// Path-sensitive first-class cursor ownership. The false branch must preserve
/// the source obligation, the true branch must transfer it to `moved`, and a
/// fresh reassignment must release the overwritten snapshot before re-arming
/// the destination owner bit.
fn cursor_transfer_and_reassign_source(frames: usize) -> String {
    format!(
        "fn conditional_frame(take: bool) {{\n\
         \x20   let rows: Vec<Vec<string>> = Vec.new();\n\
         \x20   let row: Vec<string> = Vec.new();\n\
         \x20   row.push(\"branch\" + \"-payload\");\n\
         \x20   rows.push(row);\n\
         \x20   var cursor = rows.iter();\n\
         \x20   if take {{\n\
         \x20       var moved = cursor;\n\
         \x20       let _ = moved.next();\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn reassign_frame() {{\n\
         \x20   let rows: Vec<Vec<string>> = Vec.new();\n\
         \x20   let row: Vec<string> = Vec.new();\n\
         \x20   row.push(\"overwrite\" + \"-payload\");\n\
         \x20   rows.push(row);\n\
         \x20   var cursor = rows.iter();\n\
         \x20   cursor = rows.iter();\n\
         \x20   let _ = cursor.next();\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       conditional_frame(false);\n\
         \x20       conditional_frame(true);\n\
         \x20       reassign_frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   i % 7\n\
         }}\n"
    )
}

/// Clone-out escape controls. A nested collection yielded by `.iter()` is
/// stored in `HashMap::insert`, which keeps the handed-in handle without a
/// copy-in. The yielded owner must be disjoint from both the snapshot slot and
/// the original source element, so all three trees can be released safely.
const NESTED_VEC_ESCAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let vv: Vec<Vec<string>> = Vec.new();\n\
\x20   let row: Vec<string> = Vec.new();\n\
\x20   row.push(\"escape\" + \"-alpha\");\n\
\x20   row.push(\"escape\" + \"-beta\");\n\
\x20   vv.push(row);\n\
\x20   let sink: HashMap<i64, Vec<string>> = HashMap.new();\n\
\x20   var k: i64 = 0;\n\
\x20   for inner in vv.iter() {\n\
\x20       sink.insert(k, inner);\n\
\x20       k = k + 1;\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// The `Vec<HashMap<..>>` twin of [`NESTED_VEC_ESCAPE_SOURCE`].
const NESTED_HASHMAP_ESCAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let vm: Vec<HashMap<string, i64>> = Vec.new();\n\
\x20   let m: HashMap<string, i64> = HashMap.new();\n\
\x20   m.insert(\"escape\" + \"-key\", 7);\n\
\x20   vm.push(m);\n\
\x20   let sink: HashMap<i64, HashMap<string, i64>> = HashMap.new();\n\
\x20   var k: i64 = 0;\n\
\x20   for inner in vm.iter() {\n\
\x20       sink.insert(k, inner);\n\
\x20       k = k + 1;\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// The `Vec<HashSet<..>>` twin.
const NESTED_HASHSET_ESCAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let vs: Vec<HashSet<string>> = Vec.new();\n\
\x20   let s: HashSet<string> = HashSet.new();\n\
\x20   s.insert(\"escape\" + \"-member\");\n\
\x20   vs.push(s);\n\
\x20   let sink: HashMap<i64, HashSet<string>> = HashMap.new();\n\
\x20   var k: i64 = 0;\n\
\x20   for inner in vs.iter() {\n\
\x20       sink.insert(k, inner);\n\
\x20       k = k + 1;\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// The EARLY-BREAK variant: one element escapes into the sink and the loop
/// breaks, leaving two un-yielded slots behind. The escaped clone and all three
/// snapshot slots must remain disjoint. Three rows pushed, one inserted, so the
/// program prints `1OK`.
const NESTED_VEC_ESCAPE_BREAK_SOURCE: &str = "\
fn main() {\n\
\x20   let vv: Vec<Vec<string>> = Vec.new();\n\
\x20   var r: i64 = 0;\n\
\x20   while r < 3 {\n\
\x20       let row: Vec<string> = Vec.new();\n\
\x20       row.push(\"brk\" + \"-alpha\");\n\
\x20       row.push(\"brk\" + \"-beta\");\n\
\x20       vv.push(row);\n\
\x20       r = r + 1;\n\
\x20   }\n\
\x20   let sink: HashMap<i64, Vec<string>> = HashMap.new();\n\
\x20   for inner in vv.iter() {\n\
\x20       sink.insert(0, inner);\n\
\x20       break;\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// Owned-element no-double-free shapes. A `for w in <Vec<string>>` body that
/// CONSUMES each yielded element into a `HashMap` — the two source topologies
/// must both stay sound. Place source (`for w in words`): the cursor BORROWS the
/// live source, which keeps its own scope-exit drop, so the cursor never frees
/// the shared handle. Rvalue source (`for w in mk()`): the cursor solely owns the
/// snapshot and frees it via `hew_vec_free` (a `Plain` `string` element) — the
/// consumed elements are independent `hew_vec_get_clone` deep copies the map owns
/// separately, so the snapshot free never collides with the map's. Both must run
/// CLEAN under the poisoned allocator. Two distinct keys (all vacant) so the map
/// ends with 2 entries.
const OWNED_PLACE_CONSUME_SOURCE: &str = "\
fn main() {\n\
\x20   let words: Vec<string> = Vec.new();\n\
\x20   words.push(\"a\" + \"1\");\n\
\x20   words.push(\"b\" + \"2\");\n\
\x20   let counts: HashMap<string, i64> = HashMap.new();\n\
\x20   for w in words {\n\
\x20       counts.insert(w, 1);\n\
\x20   }\n\
\x20   print(counts.len());\n\
\x20   print(\"OK\");\n\
}\n";

const OWNED_RVALUE_CONSUME_SOURCE: &str = "\
fn mk() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"a\" + \"1\");\n\
\x20   v.push(\"b\" + \"2\");\n\
\x20   return v;\n\
}\n\
\n\
fn main() {\n\
\x20   let counts: HashMap<string, i64> = HashMap.new();\n\
\x20   for w in mk() {\n\
\x20       counts.insert(w, 1);\n\
\x20   }\n\
\x20   print(counts.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// No-double-free shape (leak #3): an owned-element `.iter()` snapshot whose
/// yielded elements are MOVED into another owner (`collected`) inside the body.
/// This is the exact single-free-vs-per-iteration-copy interleaving the fix must
/// keep sound: the cursor frees the snapshot buffer + its remaining slots via
/// `hew_vec_free_owned`, while each yielded element is an INDEPENDENT
/// `hew_vec_get_clone` deep copy that `collected` owns and frees separately. If
/// the yielded value aliased the snapshot buffer, freeing both would double-free
/// under the poisoned allocator. Three pushes iterated, all collected, so the
/// program prints `3OK`.
const OWNED_ITER_MOVE_SOURCE: &str = "\
type Item { name: string, tag: string }\n\
fn main() {\n\
\x20   let v: Vec<Item> = Vec.new();\n\
\x20   v.push(Item { name: \"move-a\", tag: \"move-a-tag\" });\n\
\x20   v.push(Item { name: \"move-b\", tag: \"move-b-tag\" });\n\
\x20   v.push(Item { name: \"move-c\", tag: \"move-c-tag\" });\n\
\x20   let collected: Vec<Item> = Vec.new();\n\
\x20   for it in v.iter() {\n\
\x20       collected.push(it);\n\
\x20   }\n\
\x20   print(collected.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// The NON-COPY-IN sink twin of [`OWNED_ITER_MOVE_SOURCE`]. `Vec::push` of an
/// owned record COPIES the element into the destination slot, which would mask a
/// yield that aliased the snapshot; `HashMap::insert` stores the value the caller
/// handed it. The owned-record yield is a `hew_vec_get_clone` DEEP COPY, so the
/// map and the snapshot own disjoint trees and the cursor's `hew_vec_free_owned`
/// walk collides with nothing. Two distinct keys, so the program prints `2OK`.
const OWNED_ITER_MOVE_NONCOPY_SINK_SOURCE: &str = "\
type Item { name: string, tag: string }\n\
fn main() {\n\
\x20   let v: Vec<Item> = Vec.new();\n\
\x20   v.push(Item { name: \"sink\" + \"-a\", tag: \"sink\" + \"-a-tag\" });\n\
\x20   v.push(Item { name: \"sink\" + \"-b\", tag: \"sink\" + \"-b-tag\" });\n\
\x20   let sink: HashMap<i64, Item> = HashMap.new();\n\
\x20   var k: i64 = 0;\n\
\x20   for it in v.iter() {\n\
\x20       sink.insert(k, it);\n\
\x20       k = k + 1;\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// No-use-after-free shape: a place-source `for x in v` followed by a post-loop
/// read of `v`. The cursor must borrow (not free) the shared handle so the
/// post-loop `v[0] + v[1] + v.len()` reads live memory and the source binding's
/// own scope-exit drop is the single free. Checksum: 40 + 2 (loop sum) + 40 + 2
/// (post-loop element reads) + 2 (len) = 86.
const REUSE_SHAPE_SOURCE: &str = "\
fn main() {\n\
\x20   let v: Vec<i64> = Vec.new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   var sum: i64 = 0;\n\
\x20   for x in v {\n\
\x20       sum = sum + x;\n\
\x20   }\n\
\x20   sum = sum + v[0] + v[1] + v.len();\n\
\x20   print(sum);\n\
\x20   print(\"OK\");\n\
}\n";

/// Poisoned-allocator twin of [`hashset_owned_field_loop_source`]. The source
/// record-held `HashSet`, fresh `Vec` snapshot, and yielded string clones are three
/// disjoint ownership layers and must all tear down exactly once.
const HASHSET_OWNED_FIELD_SOURCE: &str = "\
type SetBox { words: HashSet<string> }\n\
\n\
fn main() {\n\
\x20   let words: HashSet<string> = HashSet.new();\n\
\x20   words.insert(\"field\" + \"-alpha\");\n\
\x20   words.insert(\"field\" + \"-beta\");\n\
\x20   words.insert(\"field\" + \"-gamma\");\n\
\x20   let holder = SetBox { words: words };\n\
\x20   var count: i64 = 0;\n\
\x20   for word in holder.words {\n\
\x20       if word.len() > 0 { count = count + 1; }\n\
\x20   }\n\
\x20   print(count);\n\
\x20   print(\"OK\");\n\
}\n";

// ── early-exit fixtures (leak #3, round 3) ────────────────────────────────

/// Call count for the early-exit probes. `leaks --atExit` UNDER-reports a
/// per-call leak whose last pointer still sits in a stale loop-temp alloca, so
/// these shapes are AMPLIFIED — 50 calls each allocating a three-element
/// `Vec<string>` of padded literals — until the leaked set is far larger than
/// any one live frame. All of them measured 250 leaked nodes / ~19 KB before
/// the exit-edge release existed.
const EARLY_EXIT_CALLS: usize = 50;

/// The shared `Vec<string>` producer for the early-exit shapes. `string` is an
/// ADMITTED element class: its yield is `hew_vec_get_clone`, a `+1` retain (a
/// fresh owner), so the cursor's snapshot IS registered for a release. The
/// literals are concatenated at runtime and padded so each element is a
/// distinct heap allocation that `leaks` reports individually.
const EARLY_EXIT_PRODUCER: &str = "\
fn make_vec() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"early\" + \"-alpha-padding-padding-padding\");\n\
\x20   v.push(\"early\" + \"-beta-padding-padding-padding\");\n\
\x20   v.push(\"early\" + \"-gamma-padding-padding-padding\");\n\
\x20   return v;\n\
}\n";

/// Wrap `frame_and_main_body` in the shared producer plus a `main` that calls
/// `frame()` [`EARLY_EXIT_CALLS`] times.
fn early_exit_program(frame: &str) -> String {
    format!(
        "{EARLY_EXIT_PRODUCER}\n{frame}\n\
         fn main() {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {EARLY_EXIT_CALLS} {{\n\
         \x20       frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   print(\"OK\");\n\
         }}\n"
    )
}

/// Fixture (a) — an early `return` out of the loop body. The cursor's lexical
/// release (`emit_scope_vec_iter_drops`) sits on the loop's fall-through exit
/// block, which `return` jumps past; worse, that lexical release also
/// dispositions the cursor `ScopeReleased`, which removes it from the
/// function-exit LIFO the return DROP PLAN is built from (the plan reads
/// `(none)` under `--dump-mir elab`). So pre-fix the snapshot had no release on
/// EITHER path and the whole tree leaked once per call.
fn early_return_source() -> String {
    early_exit_program(
        "fn frame() {\n\
         \x20   for word in make_vec() {\n\
         \x20       print(word.len());\n\
         \x20       return;\n\
         \x20   }\n\
         }\n",
    )
}

/// Fixture (b) — error propagation lowered onto that same return path. `?`
/// desugars to `match { .Ok(v) => v, .Err(e) => { return Err(e) } }`, whose
/// `return` is an EXPRESSION-position `HirExprKind::Return` rather than the
/// statement-position `HirStmtKind::Return` of fixture (a) — a second lowering
/// shell that needs the same exit-edge release. `check` always fails, so every
/// call takes the propagation arm out of the loop body.
fn try_propagation_source() -> String {
    format!(
        "{EARLY_EXIT_PRODUCER}\n\
         fn check(n: i64) -> Result<i64, string> {{\n\
         \x20   if n > 0 {{\n\
         \x20       return Err(\"propagate\" + \"-boom\");\n\
         \x20   }}\n\
         \x20   return Ok(n);\n\
         }}\n\
         \n\
         fn frame() -> Result<i64, string> {{\n\
         \x20   for word in make_vec() {{\n\
         \x20       let n = check(word.len())?;\n\
         \x20       return Ok(n);\n\
         \x20   }}\n\
         \x20   return Ok(0);\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {EARLY_EXIT_CALLS} {{\n\
         \x20       match frame() {{\n\
         \x20           Ok(v) => {{ }}\n\
         \x20           Err(e) => {{ }}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   print(\"OK\");\n\
         }}\n"
    )
}

/// Fixture (c) — a labelled escape from an ENCLOSING loop. `break @outer` inside
/// the inner `for` jumps to the OUTER loop's exit, past the inner cursor's
/// desugar-block close. This is the edge the exit-edge release's scope window
/// must INCLUDE while still excluding a plain `break` of the cursor's own loop
/// (which lands inside the desugar block and is released by the fall-through
/// close — the `for_in_*_own_break_*` pin below holds that half).
fn labelled_break_source() -> String {
    early_exit_program(
        "fn frame() {\n\
         \x20   @outer: for i in 0 .. 3 {\n\
         \x20       for word in make_vec() {\n\
         \x20           print(word.len());\n\
         \x20           break @outer;\n\
         \x20       }\n\
         \x20   }\n\
         }\n",
    )
}

/// Fixture (c′) — the labelled-CONTINUE twin. `continue @outer` restarts the
/// outer loop, likewise abandoning the inner cursor past its close, and runs the
/// edge twice per call. The same window must keep a `continue` of the cursor's
/// OWN loop excluded — that cursor is still mid-iteration and releasing it would
/// free the snapshot out from under the next `next()`.
fn labelled_continue_source() -> String {
    early_exit_program(
        "fn frame() {\n\
         \x20   @outer: for i in 0 .. 2 {\n\
         \x20       for word in make_vec() {\n\
         \x20           print(word.len());\n\
         \x20           continue @outer;\n\
         \x20       }\n\
         \x20   }\n\
         }\n",
    )
}

/// No-double-free pin for the exit-edge release: an ADMITTED (`string`) cursor
/// abandoned by an early `return` whose yielded element was moved into a
/// NON-COPY-IN sink (`HashMap::insert` stores the handle it is handed; a
/// `Vec::push` would byte-copy it in and structurally cannot expose the bug).
/// The `hew_vec_get_clone` yield is a `+1` retain, so the map's owner and the
/// snapshot slot's are independent and the exit-edge `hew_vec_free` collides
/// with nothing. Prints `1` (map length) then `1OK`.
const ADMITTED_RETURN_SINK_SOURCE: &str = "\
fn mk() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"ret\" + \"-alpha\");\n\
\x20   v.push(\"ret\" + \"-beta\");\n\
\x20   v.push(\"ret\" + \"-gamma\");\n\
\x20   return v;\n\
}\n\
\n\
fn frame(sink: HashMap<i64, string>) -> i64 {\n\
\x20   for w in mk() {\n\
\x20       sink.insert(1, w);\n\
\x20       return 1;\n\
\x20   }\n\
\x20   return 0;\n\
}\n\
\n\
fn main() {\n\
\x20   let sink: HashMap<i64, string> = HashMap.new();\n\
\x20   print(frame(sink));\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// The labelled-escape twin of [`ADMITTED_RETURN_SINK_SOURCE`]: the same
/// non-copy-in sink reached over `break @outer` instead of `return`. Prints
/// `1OK`.
const ADMITTED_LABELLED_BREAK_SINK_SOURCE: &str = "\
fn mk() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"brk\" + \"-alpha\");\n\
\x20   v.push(\"brk\" + \"-beta\");\n\
\x20   v.push(\"brk\" + \"-gamma\");\n\
\x20   return v;\n\
}\n\
\n\
fn main() {\n\
\x20   let sink: HashMap<i64, string> = HashMap.new();\n\
\x20   @outer: for i in 0 .. 3 {\n\
\x20       for w in mk() {\n\
\x20           sink.insert(i, w);\n\
\x20           break @outer;\n\
\x20       }\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// The OWN-LOOP `break` half of the window proof: the same admitted cursor and
/// the same non-copy-in sink, but the `break` targets the cursor's OWN loop. The
/// exit-edge window must NOT fire here — the break lands inside the cursor's
/// desugar block, whose fall-through close is the single release. Prints `1OK`;
/// a second release would be caught by the poisoned allocator.
const ADMITTED_OWN_BREAK_SINK_SOURCE: &str = "\
fn mk() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"own\" + \"-alpha\");\n\
\x20   v.push(\"own\" + \"-beta\");\n\
\x20   v.push(\"own\" + \"-gamma\");\n\
\x20   return v;\n\
}\n\
\n\
fn main() {\n\
\x20   let sink: HashMap<i64, string> = HashMap.new();\n\
\x20   for w in mk() {\n\
\x20       sink.insert(1, w);\n\
\x20       break;\n\
\x20   }\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// Nested-clone exit-edge proof: the first inner Vec escapes into a non-copy-in
/// sink and the cursor is abandoned by `return`. The sink owns the clone while
/// the exit edge releases the complete snapshot; both must be leak- and
/// double-free-free. Prints `1OK`.
const NESTED_YIELD_EARLY_RETURN_SOURCE: &str = "\
fn frame() -> i64 {\n\
\x20   let vv: Vec<Vec<string>> = Vec.new();\n\
\x20   let row: Vec<string> = Vec.new();\n\
\x20   row.push(\"alias\" + \"-alpha\");\n\
\x20   row.push(\"alias\" + \"-beta\");\n\
\x20   vv.push(row);\n\
\x20   let row2: Vec<string> = Vec.new();\n\
\x20   row2.push(\"alias\" + \"-gamma\");\n\
\x20   vv.push(row2);\n\
\x20   let sink: HashMap<i64, Vec<string>> = HashMap.new();\n\
\x20   for inner in vv.iter() {\n\
\x20       sink.insert(0, inner);\n\
\x20       return sink.len();\n\
\x20   }\n\
\x20   return 0;\n\
}\n\
\n\
fn main() {\n\
\x20   print(frame());\n\
\x20   print(\"OK\");\n\
}\n";

/// Fixture (d) — a `return` nested inside a `match` ARM inside the loop body.
/// The arm opens its own scope between the cursor's desugar block and the
/// return, so the exit edge unwinds more active scopes than fixture (a) does.
/// `min_scope_depth = 0` must still reach the cursor's block: the window is
/// `active_scopes[0..]`, not "the innermost scope". `classify` always fails, so
/// every call takes the `Err` arm.
fn match_arm_return_source() -> String {
    format!(
        "{EARLY_EXIT_PRODUCER}\n\
         fn classify(n: i64) -> Result<i64, string> {{\n\
         \x20   if n > 0 {{\n\
         \x20       return Err(\"classify\" + \"-boom\");\n\
         \x20   }}\n\
         \x20   return Ok(n);\n\
         }}\n\
         \n\
         fn frame() {{\n\
         \x20   for word in make_vec() {{\n\
         \x20       match classify(word.len()) {{\n\
         \x20           Ok(v) => {{ }}\n\
         \x20           Err(e) => {{ return; }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {EARLY_EXIT_CALLS} {{\n\
         \x20       frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   print(\"OK\");\n\
         }}\n"
    )
}

/// The second producer for the NESTED-cursor shapes — a distinct padding so the
/// inner and outer snapshots are separate allocations in the `leaks` report.
const NESTED_INNER_PRODUCER: &str = "\
fn make_inner() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"inner\" + \"-alpha-padding-padding-padding\");\n\
\x20   v.push(\"inner\" + \"-beta-padding-padding-padding\");\n\
\x20   v.push(\"inner\" + \"-gamma-padding-padding-padding\");\n\
\x20   return v;\n\
}\n";

/// Fixture (e) — TWO nested `for` loops, each with its OWN admitted cursor, left
/// by a `return` from the inner body. This is the multi-cursor case the other
/// fixtures never reach: the labelled shapes nest a `for` inside a RANGE loop,
/// which registers no cursor at all, so only one release is ever in play. Here
/// `active_scopes` holds both desugar blocks at the return, and the
/// `min_scope_depth = 0` window must release BOTH — in reverse registration
/// order — with neither lexical close reachable. One missed release leaks; one
/// doubled release aborts the `*_sink` twin below.
fn nested_cursors_return_source() -> String {
    format!(
        "{EARLY_EXIT_PRODUCER}\n{NESTED_INNER_PRODUCER}\n\
         fn frame() {{\n\
         \x20   for outer in make_vec() {{\n\
         \x20       for inner in make_inner() {{\n\
         \x20           print(outer.len() + inner.len());\n\
         \x20           return;\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {EARLY_EXIT_CALLS} {{\n\
         \x20       frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   print(\"OK\");\n\
         }}\n"
    )
}

/// The no-double-free twin of the nested-cursor shape: BOTH yields are moved
/// into a non-copy-in sink (`HashMap::insert`) before the `return` abandons both
/// cursors. Each yield is a `hew_vec_get_clone` `+1` retain, so the map's two
/// owners are independent of the two snapshots the exit edge frees. A second
/// release of either snapshot — or a release that walked a slot the map now owns
/// — aborts here under the poisoned allocator. Prints `2OK`.
const NESTED_CURSORS_RETURN_SINK_SOURCE: &str = "\
fn mk_a() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"na\" + \"-alpha\");\n\
\x20   v.push(\"na\" + \"-beta\");\n\
\x20   return v;\n\
}\n\
\n\
fn mk_b() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"nb\" + \"-alpha\");\n\
\x20   v.push(\"nb\" + \"-beta\");\n\
\x20   return v;\n\
}\n\
\n\
fn frame(sink: HashMap<i64, string>) -> i64 {\n\
\x20   for a in mk_a() {\n\
\x20       for b in mk_b() {\n\
\x20           sink.insert(0, a);\n\
\x20           sink.insert(1, b);\n\
\x20           return 1;\n\
\x20       }\n\
\x20   }\n\
\x20   return 0;\n\
}\n\
\n\
fn main() {\n\
\x20   let sink: HashMap<i64, string> = HashMap.new();\n\
\x20   let n = frame(sink);\n\
\x20   print(sink.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// Use-after-free pin for the exit edge: the abandoned cursor's OWN YIELD is the
/// value the `return` carries out. `word` is a `hew_vec_get_clone` `+1` retain, so
/// the snapshot release the exit edge now emits frees the vec's copy while the
/// returned string keeps its own owner — the caller must read it intact.
///
/// This is the shape that distinguishes a correct exit-edge release from one
/// that frees the returned value's backing: the release is emitted on the SAME
/// edge that hands the value to the caller, after the return value has been
/// moved out. If the release walked the returned leaf, `MallocScribble` fills it
/// with `0x55` and the printed text is corrupted (or the process aborts) before
/// the assertion is read.
const RETURN_YIELDED_VALUE_SOURCE: &str = "\
fn mk() -> Vec<string> {\n\
\x20   let v: Vec<string> = Vec.new();\n\
\x20   v.push(\"yield\" + \"-returned-value\");\n\
\x20   v.push(\"yield\" + \"-second\");\n\
\x20   return v;\n\
}\n\
\n\
fn frame() -> string {\n\
\x20   for word in mk() {\n\
\x20       return word;\n\
\x20   }\n\
\x20   return \"none\";\n\
}\n\
\n\
fn main() {\n\
\x20   print(frame());\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as vec_local_drop_leak_oracle) ───

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path.
fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let bin = stdout
        .lines()
        .find_map(|l| l.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no `native:` line for {name}:\n{stdout}"))
        .to_string();
    PathBuf::from(bin)
}

// Leak measurement itself comes from the shared `support::leak_slope` harness:
// its `measure_leaks` panics — rather than returning `None` and letting the
// caller early-return green — when `leaks(1)` declines to attach, exceeds the
// deadline, or emits no parseable summary.

/// Build the shape at `low_frames` and `high_frames`, measure leak NODE counts,
/// and assert the delta stays within `SLOPE_TOLERANCE`.
///
/// Fails closed on every axis: `require_leaks_tool` panics on an unsupported
/// host or a missing `leaks(1)`, the shared `measure_leaks` panics when the
/// inspector cannot attach, times out, or emits no parseable summary, and
/// `run_probe_witness` establishes that the HIGH probe actually performed at
/// least as much work as the LOW one before either leak number is trusted.
fn assert_frame_slope_below_tolerance(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
) {
    require_leaks_tool();

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("for-in-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(low_frames),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(high_frames),
        dir.path(),
        &format!("{shape_name}_high"),
    );

    let low_lines = run_probe_witness(&bin_low, &[]);
    let high_lines = run_probe_witness(&bin_high, &[]);
    assert!(
        high_lines >= low_lines,
        "{shape_name}: HIGH probe printed {high_lines} lines but LOW printed {low_lines}. \
         The HIGH run performs at least as much observable work as the LOW one for every \
         shape here, so fewer lines means the probe did not run the loop under test and its \
         leak count is not a slope sample."
    );

    let low_leaks = measure_leaks(&bin_low);
    let high_leaks = measure_leaks(&bin_high);

    eprintln!(
        "{shape_name}: low_frames={low_frames} low_leaks={low_leaks} \
         high_frames={high_frames} high_leaks={high_leaks} \
         tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration for-in cursor (`VecIter`) \
         handle is not being released (pre-fix slope is ~2.0 leak/frame). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see which stack the leaked block \
         came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Measure the leak-node count of `source` once and hand it to `check`.
///
/// An EXACT count, not a slope: the early-exit shapes have a clean natural-exit
/// twin measuring zero, so the correct answer is a hard `0` and any per-call
/// residue is the bug.
///
/// Fails closed: an unsupported host, a missing `leaks(1)`, an attach refusal, a
/// timeout, or an unparseable report all panic instead of skipping. The work
/// witness matters especially here — an exact-zero assertion is trivially
/// satisfied by a probe that crashed before allocating anything, so the probe
/// must first be shown to reach its own exit under its own control.
fn with_leak_count(shape_name: &str, source: &str, check: impl FnOnce(usize)) {
    require_leaks_tool();

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("for-in-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let program_lines = run_probe_witness(&bin, &[]);
    let leaks = measure_leaks(&bin);
    eprintln!("{shape_name}: leaks={leaks} program_lines={program_lines}");
    check(leaks);
}

/// Assert `source` leaks EXACTLY zero nodes.
fn assert_zero_leaks(shape_name: &str, source: &str) {
    with_leak_count(shape_name, source, |leaks| {
        assert_eq!(
            leaks, 0,
            "{shape_name}: expected ZERO leaked nodes over {EARLY_EXIT_CALLS} calls; got \
             {leaks}. A non-zero count here means the `for x in …` snapshot cursor abandoned \
             by this early exit is never released — the lexical `emit_scope_vec_iter_drops` \
             sits past the exit edge AND its `ScopeReleased` disposition empties the exit's \
             drop plan, so `emit_vec_iter_drops_for_exit_edge` is the only release. Re-run \
             with `MallocStackLogging=1 leaks --atExit -- <bin>` for the allocating stack.",
        );
    });
}

/// Assert the LOW and HIGH frame endpoints are both exactly clean. These
/// nested-container rows have no accepted constant residue: checking both
/// endpoints distinguishes cursor-only, per-yield, and partial-drain omissions
/// while retaining the existing slope tests for older allocator-noise probes.
fn assert_frame_endpoints_zero(shape_name: &str, source_fn: fn(usize) -> String) {
    for frames in [LOW_FRAMES, HIGH_FRAMES] {
        let endpoint = format!("{shape_name}_{frames}_frames");
        let source = source_fn(frames);
        with_leak_count(&endpoint, &source, |leaks| {
            assert_eq!(
                leaks, 0,
                "{endpoint}: expected exactly zero leaks; nested VecIter clone-out \
                 must release every yielded owner and the complete cursor snapshot"
            );
        });
    }
}

/// Fixture A: per-iteration `for x in make_vec(i)` cursors must not leak. The
/// cursor is the sole owner of each fresh Vec and must free it on every outer
/// iteration. Reverting the `emit_scope_vec_iter_drops` registration fails this
/// by ~94 nodes.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_rvalue_source_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_rvalue_source",
        rvalue_source_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture B: per-iteration `for x in v` over a fresh place-bound Vec must not
/// leak. The source binding keeps its scope-exit drop while the cursor borrows.
/// Reverting the `derive_local_collection_drop_allowed` cursor-ingress
/// exemption (so the source loses its drop again) fails this by ~94 nodes.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_place_source_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_place_source",
        place_source_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// A record-field `HashSet<string>` source must release the source set, its
/// `to_vec()` snapshot, and every clone-out yield with no per-frame residue.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn hashset_owned_field_for_in_has_zero_leak_endpoints() {
    assert_frame_endpoints_zero(
        "hashset_owned_field_for_in",
        hashset_owned_field_loop_source,
    );
}

/// Fixture C: per-iteration `for b in make_vec(i)` over a `Vec<bool>` must not
/// leak — a second `BitCopy` element width confirms the cursor release is
/// element-width agnostic.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_bool_rvalue_source_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_bool_rvalue_source",
        bool_rvalue_source_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture D (leak #3): per-iteration `for it in v.iter()` over an owned-RECORD
/// `Vec<Item>` must not leak. The cursor's `hew_vec_clone_owned` snapshot is
/// freed via `hew_vec_free_owned` on every outer iteration. Reverting the
/// `vec_iter_cursor_release_symbol` admission (back to the `BitCopy`-only gate)
/// fails this by ~6 nodes/frame.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_owned_record_iter_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_owned_record_iter",
        owned_record_iter_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture E (leak #3): per-iteration `for s in v.iter()` over a `Vec<string>`
/// must not leak. The `Plain` element release is `hew_vec_free` (the runtime
/// walks the string slots). Confirms the snapshot fix is not owned-descriptor
/// specific.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_string_iter_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "for_in_string_iter",
        string_iter_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Empty nested snapshots isolate the cursor release: no yielded owner exists.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_empty_nested_vec_iter_no_per_frame_leak_slope() {
    assert_frame_endpoints_zero(
        "for_in_empty_nested_vec_iter",
        empty_nested_vec_iter_loop_source,
    );
}

/// Full-drain nested Vec snapshots exercise both cursor cleanup and repeated
/// body-end cleanup of discarded clone-out yields.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_vec_full_drain_no_per_frame_leak_slope() {
    assert_frame_endpoints_zero(
        "for_in_nested_vec_full_drain",
        nested_vec_full_drain_loop_source,
    );
}

/// `HashMap` clone-out yields use the layout-aware per-yield release.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_hashmap_full_drain_no_per_frame_leak_slope() {
    assert_frame_endpoints_zero(
        "for_in_nested_hashmap_full_drain",
        nested_hashmap_full_drain_loop_source,
    );
}

/// `HashSet` clone-out yields use the layout-aware per-yield release.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_hashset_full_drain_no_per_frame_leak_slope() {
    assert_frame_endpoints_zero(
        "for_in_nested_hashset_full_drain",
        nested_hashset_full_drain_loop_source,
    );
}

/// A break releases the yielded clone and the cursor snapshot's un-yielded
/// nested rows.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_vec_break_no_per_frame_leak_slope() {
    assert_frame_endpoints_zero("for_in_nested_vec_break", nested_vec_break_loop_source);
}

/// Owned tuple yields require a recursive in-place body-end drop.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_tuple_string_full_drain_has_zero_leak_endpoints() {
    assert_frame_endpoints_zero(
        "for_in_tuple_string_full_drain",
        tuple_string_full_drain_loop_source,
    );
}

/// A manual cursor in function-root scope and its ignored owned `next()` result
/// must both be wired into cleanup.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn vec_iter_manual_cursor_ignored_next_has_zero_leak_endpoints() {
    assert_frame_endpoints_zero(
        "vec_iter_manual_cursor_ignored_next",
        manual_cursor_ignored_next_source,
    );
}

/// Branch-local transfers and fresh overwrites must keep an exact-zero slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn vec_iter_cursor_transfer_and_reassign_has_zero_leak_endpoints() {
    assert_frame_endpoints_zero(
        "vec_iter_cursor_transfer_and_reassign",
        cursor_transfer_and_reassign_source,
    );
}

/// Run `source` under the poisoned-allocator triple and assert it prints
/// `expected` untouched. Shared by the no-double-free pins.
fn assert_runs_clean(shape_name: &str, source: &str, expected: &str) {
    require_macos_poisoned_allocator();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("for-in-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|e| panic!("run {shape_name} binary: {e}"));

    assert!(
        output.status.success(),
        "{shape_name} must run clean under the poisoned allocator — a crash here \
         indicates a double-free of a shared element/handle;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{shape_name} must print `{expected}` untouched — a scribbled value \
         indicates a freed-but-still-read element;\n{}",
        describe_output(&output)
    );
}

/// No-use-after-free pin: a place-source `for x in v` whose source is read
/// AFTER the loop runs to completion under the poisoned-allocator triple and
/// prints the exact checksum (86) then `OK`. A cursor-side free of the shared
/// handle would be a use-after-free against the post-loop read and a
/// double-free against the source binding's own scope-exit drop — under
/// `MallocScribble`/`MallocGuardEdges` that aborts (or scribbles the values)
/// before the checksum is printed.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_place_source_reuse_runs_clean_under_malloc_scribble() {
    assert_runs_clean("reuse_shape", REUSE_SHAPE_SOURCE, "86OK");
}

/// The `HashSet` field path must remain disjoint under aggressive allocator
/// poisoning: source teardown, Vec snapshot teardown, and yielded-string
/// teardown must not double-free or read a scribbled owner.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn hashset_owned_field_for_in_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "hashset_owned_field_for_in",
        HASHSET_OWNED_FIELD_SOURCE,
        "3OK",
    );
}

/// No-double-free pin: a place-source `for w in words` over a `Vec<string>`
/// whose yielded element is CONSUMED into a `HashMap`. The cursor BORROWS the
/// live source `words`, which keeps its own scope-exit drop — the cursor never
/// frees the shared handle, so the per-element release never collides with the
/// value the map now owns. Must run clean and print `2OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_owned_element_consume_place_runs_clean_under_malloc_scribble() {
    assert_runs_clean("owned_place_consume", OWNED_PLACE_CONSUME_SOURCE, "2OK");
}

/// No-double-free pin: the rvalue analogue (`for w in mk()` over a
/// `Vec<string>` with the yielded element consumed into a `HashMap`). The cursor
/// solely owns the snapshot and frees it via `hew_vec_free`; the consumed
/// elements are independent `hew_vec_get_clone` deep copies the map frees
/// separately, so the snapshot free never collides. Must run clean and print
/// `2OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_owned_element_consume_rvalue_runs_clean_under_malloc_scribble() {
    assert_runs_clean("owned_rvalue_consume", OWNED_RVALUE_CONSUME_SOURCE, "2OK");
}

/// No-double-free pin (leak #3): an owned-element `.iter()` snapshot whose
/// yielded elements are MOVED into another owner. The cursor now frees the
/// snapshot via `hew_vec_free_owned`, and the moved-out elements are independent
/// `hew_vec_get_clone` deep copies `collected` frees separately. If the snapshot
/// free aliased a moved-out element, this aborts under the poisoned allocator.
/// Must run clean and print `3OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_owned_iter_move_runs_clean_under_malloc_scribble() {
    assert_runs_clean("owned_iter_move", OWNED_ITER_MOVE_SOURCE, "3OK");
}

/// No-double-free pin: the NON-COPY-IN sink twin of the move oracle. `Vec::push`
/// copies the element into its slot and would mask an aliasing yield;
/// `HashMap::insert` stores what it is handed. The owned-record yield is a deep
/// copy, so the map's tree and the snapshot's are disjoint. Must print `2OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_owned_iter_move_noncopy_sink_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "owned_iter_move_noncopy_sink",
        OWNED_ITER_MOVE_NONCOPY_SINK_SOURCE,
        "2OK",
    );
}

/// Clone-out pin: a `Vec<Vec<string>>` `.iter()` whose yielded inner Vec escapes
/// into a non-copy-in sink. The yielded Vec, snapshot slot, and source slot are
/// three independent owners. Must print `1OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_vec_escape_runs_clean_under_malloc_scribble() {
    assert_runs_clean("nested_vec_escape", NESTED_VEC_ESCAPE_SOURCE, "1OK");
}

/// Clone-out pin: the `Vec<HashMap<..>>` twin.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_hashmap_escape_runs_clean_under_malloc_scribble() {
    assert_runs_clean("nested_hashmap_escape", NESTED_HASHMAP_ESCAPE_SOURCE, "1OK");
}

/// Clone-out pin: the `Vec<HashSet<..>>` twin.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_hashset_escape_runs_clean_under_malloc_scribble() {
    assert_runs_clean("nested_hashset_escape", NESTED_HASHSET_ESCAPE_SOURCE, "1OK");
}

/// Clone-out pin: the EARLY-BREAK variant. One clone escapes and two snapshot
/// slots stay un-yielded; the cursor still releases all three snapshot slots.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_vec_escape_break_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "nested_vec_escape_break",
        NESTED_VEC_ESCAPE_BREAK_SOURCE,
        "1OK",
    );
}

// ── early-exit oracles (leak #3, round 3) ─────────────────────────────────

/// Fixture (a): an early `return` out of the loop body must leak ZERO. Pre-fix
/// this measured 250 leaked nodes / 19,200 bytes over 50 calls while the
/// identical natural-exit shape measured zero — the lexical release is past the
/// return and the return's drop plan reads `(none)` because that same lexical
/// release already dispositioned the cursor `ScopeReleased`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_early_return_leaks_nothing() {
    assert_zero_leaks("early_return", &early_return_source());
}

/// Fixture (b): error propagation lowered onto the return path must leak ZERO.
/// `?`'s `Err` arm is an EXPRESSION-position return — a different lowering shell
/// from fixture (a)'s statement-position one — and needs its own exit-edge
/// release. Pre-fix: 250 leaked nodes / 16,800 bytes over 50 calls.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_try_propagation_return_leaks_nothing() {
    assert_zero_leaks("try_propagation", &try_propagation_source());
}

/// Fixture (c): a labelled escape from an ENCLOSING loop must leak ZERO.
/// `break @outer` jumps past the inner cursor's desugar-block close, so the
/// exit-edge window has to include it. Pre-fix: 250 leaked nodes / 19,200 bytes
/// over 50 calls.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_labelled_break_of_enclosing_loop_leaks_nothing() {
    assert_zero_leaks("labelled_break", &labelled_break_source());
}

/// Fixture (c′): the labelled-CONTINUE twin must leak ZERO — and the same
/// window must still keep an own-loop `continue` silent, which the surviving
/// natural-drain oracles above depend on.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_labelled_continue_of_enclosing_loop_leaks_nothing() {
    assert_zero_leaks("labelled_continue", &labelled_continue_source());
}

/// Fixture (d): a `return` from inside a `match` ARM inside the loop body must
/// leak ZERO. The arm's own scope sits between the cursor's desugar block and
/// the exit edge, proving the `min_scope_depth = 0` window spans every active
/// scope rather than just the innermost one.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_match_arm_return_leaks_nothing() {
    assert_zero_leaks("match_arm_return", &match_arm_return_source());
}

/// Fixture (e): two nested `for` loops, each with its OWN admitted cursor, left
/// by a `return` from the inner body — BOTH snapshots must be released, so the
/// count is ZERO. The labelled fixtures nest inside a RANGE loop and therefore
/// only ever have one cursor live; this is the only oracle where the exit edge
/// must emit more than one release.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_cursors_return_leaks_nothing() {
    assert_zero_leaks("nested_cursors_return", &nested_cursors_return_source());
}

/// The no-double-free twin of fixture (e): both yields moved into a non-copy-in
/// sink before the `return` abandons both cursors. A doubled release of either
/// snapshot aborts under the poisoned allocator. Prints `2OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_cursors_return_sink_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "nested_cursors_return_sink",
        NESTED_CURSORS_RETURN_SINK_SOURCE,
        "2OK",
    );
}

/// Use-after-free pin: the value the `return` carries out IS the abandoned
/// cursor's own yield. The `hew_vec_get_clone` retain makes the returned string an
/// independent owner, so the exit-edge snapshot release must leave it readable.
/// A release that walked the returned leaf scribbles it to `0x55` before `main`
/// prints it.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_return_yielded_value_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "return_yielded_value",
        RETURN_YIELDED_VALUE_SOURCE,
        "yield-returned-valueOK",
    );
}

/// No-double-free pin for the exit edge: an ADMITTED (`string`) cursor
/// abandoned by an early `return` whose element was moved into a NON-COPY-IN
/// sink (`HashMap::insert`). The retain-yield gives the map its own `+1`, so the
/// exit-edge `hew_vec_free` is the snapshot's only owner. Prints `11OK`
/// (`frame` result, map length, `OK`).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_early_return_sink_runs_clean_under_malloc_scribble() {
    assert_runs_clean("admitted_return_sink", ADMITTED_RETURN_SINK_SOURCE, "11OK");
}

/// The labelled-escape twin of the no-double-free pin. Prints `1OK`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_labelled_break_sink_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "admitted_labelled_break_sink",
        ADMITTED_LABELLED_BREAK_SINK_SOURCE,
        "1OK",
    );
}

/// The OWN-LOOP `break` half of the window proof: the exit-edge release must NOT
/// fire for a break of the cursor's own loop (the fall-through close already
/// covers it). Prints `1OK` under the poisoned allocator.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_own_break_sink_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "admitted_own_break_sink",
        ADMITTED_OWN_BREAK_SINK_SOURCE,
        "1OK",
    );
}

/// Clone-out pin for the exit edge: an escaped nested Vec survives while the
/// abandoned cursor snapshot is released.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_yield_early_return_runs_clean_under_malloc_scribble() {
    assert_runs_clean(
        "nested_yield_early_return",
        NESTED_YIELD_EARLY_RETURN_SOURCE,
        "1OK",
    );
}

/// The same exit-edge shape must leave neither the escaped clone nor the
/// abandoned snapshot behind.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_in_nested_yield_early_return_leaks_nothing() {
    assert_zero_leaks(
        "nested_yield_early_return",
        NESTED_YIELD_EARLY_RETURN_SOURCE,
    );
}
