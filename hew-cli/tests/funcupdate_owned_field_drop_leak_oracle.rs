//! Functional-update owned-field drop leak oracle.
//!
//! Exercises `{ ..base, field: new_value }` where `field` is an owned
//! heap type (`string`, `bytes`, `Vec<T>`, `HashMap`, `HashSet`).
//!
//! Before the fix (the functional-update overridden-owned-field leak in
//! `hew-mir/src/lower.rs`), every iteration of a
//! `{ ..base, label: new_string }` loop that overrides a heap-owning field
//! leaked exactly ONE allocation node (the old value of `label` from
//! `base` was never released — the new record owned the replacement, the
//! base's composite drop was suppressed by `derive_owned_record_drop_allowed`,
//! but the overridden field's old allocation was abandoned).
//!
//! After the fix, the functional-update arm emits, before the `RecordInit`:
//!
//! 1. `RecordFieldDrop { record: base, field_offset: N, … }` for every
//!    single-pointer COW field (`string`, `Vec<T>`, `HashMap`, `HashSet`):
//!    raw load → release → null-store.
//! 2. The same owning-slot `RecordFieldDrop` for the fat `bytes`
//!    `{ptr,offset,len}` triple: GEP field 0 → release → null-store.
//!
//! `Generator` is also a single-pointer COW field handled by
//! `RecordFieldDrop`, but a *functional update* can never reach one: a record
//! carrying a `Generator` field is front-stopped before this lowering by
//! record-clone-thunk synthesis (the coroutine handle has no per-field `dup`
//! symbol). The `hew_gen_coro_destroy` override-drop arm is therefore
//! exercised out of band (via tuple/enum match-destructure), not by this
//! oracle, and is deliberately NOT claimed here.
//!
//! …so every replaced owned value is released at the construction site on
//! every execution path. The release is sound because `..base` consumes the
//! base (the move-checker rejects any later read), so the freed old value has
//! no surviving reader.
//!
//! ## Shape coverage
//!
//! * **string field** — the most common override case; pre-fix slope ~1.0
//!   node/frame.
//! * **bytes field** — override a `bytes` field in a local loop; verifies
//!   the `hew_bytes_drop` inline-drop arm is correctly selected.
//! * **Vec<i64> field** — override a plain `Vec<i64>` field (no owned
//!   elements); exercises `hew_vec_free`.
//! * **`HashMap` field** — override a `HashMap<string,i64>` field; exercises
//!   the `hew_hashmap_free_layout` single-pointer COW release.
//! * **`HashSet` field** — override a `HashSet<i64>` field; exercises the
//!   `hew_hashset_free_layout` single-pointer COW release.
//! * **multi-field** — override a `string` AND a `Vec<i64>` in the same
//!   update expression; each must be independently released.
//!
//! ## Slope methodology
//!
//! Mirrors `bytes_drop_leak_oracle.rs`: compile the same shape at LOW
//! and HIGH frame counts, measure leak NODE counts under `leaks --atExit`
//! with the poisoned-allocator triple (`MallocScribble` et al.), and assert
//! the delta stays within `SLOPE_TOLERANCE` nodes regardless of frame count.
//!
//! macOS-only: the `leaks(1)` allocator inspector is a Darwin tool.  The
//! oracle skips (with an explanatory message) when `leaks` is unavailable.

#![cfg(unix)]

mod support;

use support::leak_slope::{measure_leaks, require_leaks_tool, run_under_malloc_scribble};

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: stays close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check.  A slope of 1.0 leak/frame
/// (pre-fix measurement) produces `50 - 3 = 47` excess nodes against
/// the tolerance of `5`.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
const SLOPE_TOLERANCE: usize = 5;

// ── fixture sources ──────────────────────────────────────────────────────

/// Functional update overriding a `string` field in a plain local loop.
///
/// `label` is replaced each iteration with a fresh `hew_string_repeat`
/// allocation.  Pre-fix: one leaked `cstring` node per iteration.
/// Post-fix: the old `label` is released via `hew_string_drop` before
/// the new record is built.
fn string_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Cfg {{\n\
         \x20   label: string,\n\
         \x20   count: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var c = Cfg {{ label: string.repeat(\"a\", 32), count: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       c = Cfg {{ label: string.repeat(\"b\", 32), ..c }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   c.count\n\
         }}\n"
    )
}

/// Functional update overriding a `bytes` field in a local loop.
///
/// Each iteration replaces `buf` with a fresh `to_bytes()` allocation.
/// Pre-fix: one leaked bytes-buffer node per iteration.
/// Post-fix: the old `buf` is released via `hew_bytes_drop`.
fn bytes_field_source(frames: usize) -> String {
    format!(
        "record ByteHolder {{\n\
         \x20   buf: bytes,\n\
         \x20   count: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var h = ByteHolder {{ buf: \"initial\".to_bytes(), count: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       h = ByteHolder {{ buf: \"loop-payload\".to_bytes(), ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.count\n\
         }}\n"
    )
}

fn direct_string_field_store_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         type Cfg {{ label: string, count: i64 }}\n\
         fn main() -> i64 {{\n\
         \x20   var c = Cfg {{ label: string.repeat(\"a\", 32), count: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       c.label = string.repeat(\"b\", 32);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if c.label.len() == 32 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

fn direct_string_self_store_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         type Cfg {{ label: string }}\n\
         fn main() -> i64 {{\n\
         \x20   var c = Cfg {{ label: string.repeat(\"self\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       c.label = c.label;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if c.label.len() == 128 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// A heap-string field reassigned inside a CALLEE that is invoked once per
/// frame. Unlike the in-`main` loop shapes above, the reassigned owner is
/// abandoned at the CALLEE'S scope exit, so a lost record scope-drop leaks one
/// buffer PER CALL — a per-frame slope, not a constant. The in-`main` loops
/// leak the final value only once (constant) and so cannot see the record's
/// missing `RecordInPlace` scope drop; this callee shape is the coverage that
/// was absent when a `RecordFieldStore` into a non-inline-enum field dropped
/// the record's scope-exit obligation.
fn callee_scope_string_store_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         type Cfg {{ label: string, count: i64 }}\n\
         fn churn(seed: string) -> i64 {{\n\
         \x20   var c = Cfg {{ label: seed, count: 0 }};\n\
         \x20   c.label = string.repeat(\"b\", 32);\n\
         \x20   c.label.len()\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var acc: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       acc = acc + churn(string.repeat(\"a\", 32));\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if acc > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// The MIXED rule-17 case: a callee record carrying an affine `#[resource]`
/// field BESIDE the reassigned heap-string sibling. The string is reassigned
/// (fresh owner) and the resource is explicitly closed, both inside the callee.
/// The record must keep its `RecordInPlace` scope drop to release the string
/// sibling, while that same drop must NOT re-close the already-discharged
/// handle. A missing scope drop leaks the string (slope); a widened drop that
/// re-runs the closed handle double-frees under `MallocScribble`.
fn callee_scope_resource_string_sibling_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         #[resource]\n\
         type Handle {{ fd: i64 }}\n\
         impl Handle {{ fn close(consuming self) {{}} }}\n\
         type Carrier {{ handle: Handle, note: string }}\n\
         fn consume_it(seed: string) -> i64 {{\n\
         \x20   var c = Carrier {{ handle: Handle {{ fd: 1 }}, note: seed }};\n\
         \x20   c.note = string.repeat(\"b\", 32);\n\
         \x20   let n = c.note.len();\n\
         \x20   c.handle.close();\n\
         \x20   n\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var acc: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       acc = acc + consume_it(string.repeat(\"a\", 32));\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if acc > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

fn direct_bytes_field_store_source(frames: usize) -> String {
    format!(
        "record Holder {{ payload: bytes }}\n\
         fn main() -> i64 {{\n\
         \x20   var h = Holder {{ payload: \"initial\".to_bytes() }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       h.payload = \"replacement\".to_bytes();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if h.payload.len() == 11 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Functional update overriding a `Vec<i64>` field in a local loop.
///
/// Each iteration replaces `items` with a fresh Vec allocation.
/// Pre-fix: one leaked Vec-header node per iteration.
/// Post-fix: the old `items` is released via `hew_vec_free`.
fn vec_field_source(frames: usize) -> String {
    format!(
        "record VecHolder {{\n\
         \x20   items: Vec<i64>,\n\
         \x20   tag: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: Vec<i64> = Vec.new();\n\
         \x20   init.push(99);\n\
         \x20   var h = VecHolder {{ items: init, tag: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: Vec<i64> = Vec.new();\n\
         \x20       next.push(i);\n\
         \x20       h = VecHolder {{ items: next, ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.tag\n\
         }}\n"
    )
}

/// Functional update overriding a `HashMap<string,i64>` field in a loop.
///
/// Each iteration replaces `m` with a fresh map allocation.
/// Pre-fix: one leaked HashMap-control node per iteration.
/// Post-fix: the old `m` is released via `hew_hashmap_free_layout`.
fn hashmap_field_source(frames: usize) -> String {
    format!(
        "record MapHolder {{\n\
         \x20   m: HashMap<string, i64>,\n\
         \x20   tag: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: HashMap<string, i64> = HashMap.new();\n\
         \x20   init.insert(\"seed\", 1);\n\
         \x20   var h = MapHolder {{ m: init, tag: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: HashMap<string, i64> = HashMap.new();\n\
         \x20       next.insert(\"k\", i);\n\
         \x20       h = MapHolder {{ m: next, ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.tag\n\
         }}\n"
    )
}

/// Functional update overriding a `HashSet<i64>` field in a loop.
///
/// Each iteration replaces `s` with a fresh set allocation.
/// Pre-fix: one leaked HashSet-control node per iteration.
/// Post-fix: the old `s` is released via `hew_hashset_free_layout`.
fn hashset_field_source(frames: usize) -> String {
    format!(
        "record SetHolder {{\n\
         \x20   s: HashSet<i64>,\n\
         \x20   tag: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: HashSet<i64> = HashSet<i64>.new();\n\
         \x20   init.insert(99);\n\
         \x20   var h = SetHolder {{ s: init, tag: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: HashSet<i64> = HashSet<i64>.new();\n\
         \x20       next.insert(i);\n\
         \x20       h = SetHolder {{ s: next, ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.tag\n\
         }}\n"
    )
}

/// Functional update overriding both a `string` AND a `Vec<i64>` field.
///
/// Pre-fix: two leaked nodes per iteration (one per overridden field).
/// Post-fix: both fields independently released before `RecordInit`.
fn multi_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Multi {{\n\
         \x20   label: string,\n\
         \x20   items: Vec<i64>,\n\
         \x20   id: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: Vec<i64> = Vec.new();\n\
         \x20   init.push(0);\n\
         \x20   var m = Multi {{ label: string.repeat(\"z\", 16), items: init, id: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: Vec<i64> = Vec.new();\n\
         \x20       next.push(i);\n\
         \x20       m = Multi {{ label: string.repeat(\"y\", 16), items: next, ..m }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   m.id\n\
         }}\n"
    )
}

// ── carry-axis sources ─────────────────────────────────────────────────────
//
// Every override source above CARRIES only a BitCopy (`count` / `tag` / `id`)
// field — the coverage gap that let the carried-record SIGSEGV and the
// carried-closure double-free ship while the prior suites passed. These
// sources invert the axis: each OVERRIDES a churn `string` and CARRIES the
// owned field under test every frame, so a per-frame leak in the carry path
// (the consumed base's field not transferred, or transferred without the base
// excluded from its composite drop) shows as slope. Slope-0 confirms the
// carried field is moved exactly once per frame.

/// Carry a nested owned-RECORD field while churning a `string`.
fn carry_record_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Inner {{\n\
         \x20   label: string,\n\
         \x20   n: i64,\n\
         }}\n\
         type Pair {{\n\
         \x20   keep: Inner,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var p = Pair {{ keep: Inner {{ label: string.repeat(\"k\", 32), n: 1 }}, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.n\n\
         }}\n"
    )
}

/// Carry a `string` field while churning a different `string` field.
fn carry_string_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: string,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var p = Pair {{ keep: string.repeat(\"k\", 32), churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `Vec<string>` (owned-element) field while churning a `string`.
fn carry_vec_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: Vec<string>,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let v: Vec<string> = Vec.new();\n\
         \x20   v.push(string.repeat(\"k\", 32));\n\
         \x20   var p = Pair {{ keep: v, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `HashMap<string,string>` field while churning a `string`.
fn carry_hashmap_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: HashMap<string, string>,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let m: HashMap<string, string> = HashMap.new();\n\
         \x20   m.insert(string.repeat(\"k\", 32), string.repeat(\"v\", 32));\n\
         \x20   var p = Pair {{ keep: m, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `HashSet<string>` field while churning a `string`.
fn carry_hashset_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: HashSet<string>,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let s: HashSet<string> = HashSet.new();\n\
         \x20   s.insert(string.repeat(\"k\", 32));\n\
         \x20   var p = Pair {{ keep: s, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `bytes` field while churning a `string`.
fn carry_bytes_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: bytes,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var p = Pair {{ keep: string.repeat(\"k\", 32).to_bytes(), churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

// The tuple-carry sources below measure the heap-owning-TUPLE field carry in
// BOTH drop orders, because the two orders exercise different halves of the
// transfer:
//
//   * ESCAPE order — the result leaves the frame that consumed the base, so the
//     base's frame teardown runs while the carried tuple is still live. A carry
//     that failed to hand the tuple over frees it here (use-after-free) or never
//     frees it (leak).
//   * SAME-FRAME order — base and result both die at one scope exit, result
//     first. A carry that transferred without excluding the base releases the
//     tuple twice.
//
// Neither source READS the carried tuple back: a tuple-field read seeds an
// owned-field binder that the record prover conservatively treats as an escape,
// excluding the whole record from its `RecordInPlace` drop. That exclusion leaks
// on `main` today with no functional update in sight, and would swamp the signal
// these fixtures exist to measure. Value-level read-back of the carried leaves
// is pinned separately by `accept_carry_nested_and_collection_bearing_tuples`
// in `funcupdate_consume_semantics.rs`.

/// Carry a `(string, i64)` tuple field, ESCAPE drop order: the update happens in
/// a helper whose result is returned, so the consumed base dies one frame below
/// the surviving carry.
fn carry_tuple_field_escape_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: (string, i64),\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn step() -> Pair {{\n\
         \x20   let b = Pair {{ keep: (string.repeat(\"k\", 32), 1), churn: string.repeat(\"a\", 32) }};\n\
         \x20   Pair {{ churn: string.repeat(\"b\", 32), ..b }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = step();\n\
         \x20       total = total + r.churn.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Carry a `(string, i64)` tuple field, SAME-FRAME drop order: base and result
/// are two bindings in one loop-body scope, so the result is released first and
/// the consumed base's teardown follows immediately.
fn carry_tuple_field_same_frame_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Pair {{\n\
         \x20   keep: (string, i64),\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Pair {{ keep: (string.repeat(\"k\", 32), 1), churn: string.repeat(\"a\", 32) }};\n\
         \x20       let s = Pair {{ churn: string.repeat(\"b\", 32), ..b }};\n\
         \x20       total = total + s.churn.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Carry a `(Inner, i64)` tuple field — the carried heap leaf is two layers
/// down (tuple element → record field). Escape drop order.
fn carry_tuple_of_record_field_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         \n\
         type Inner {{\n\
         \x20   label: string,\n\
         \x20   n: i64,\n\
         }}\n\
         type Pair {{\n\
         \x20   keep: (Inner, i64),\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn step() -> Pair {{\n\
         \x20   let b = Pair {{ keep: (Inner {{ label: string.repeat(\"k\", 32), n: 1 }}, 1), churn: string.repeat(\"a\", 32) }};\n\
         \x20   Pair {{ churn: string.repeat(\"b\", 32), ..b }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = step();\n\
         \x20       total = total + r.churn.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// The round-2 reproducer: tuple → record → tuple → `Option<record>`.
/// This source is deliberately checked, not compiled: the release machinery
/// has no move-specific enum carry, so the recursive rule must stop it before
/// a leaking executable can be emitted.
fn carry_record_nested_option_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         type Leaf {{ label: string, n: i64 }}\n\
         type Wrapper {{ inner: (Option<Leaf>, i64), tag: string }}\n\
         type T {{ pair: (Wrapper, i64), churn: string }}\n\
         fn mk() -> T {{\n\
         \x20   let b = T {{\n\
         \x20       pair: (Wrapper {{ inner: (Some(Leaf {{ label: string.repeat(\"k\", 32), n: 1 }}), 9), tag: string.repeat(\"w\", 32) }}, 5),\n\
         \x20       churn: string.repeat(\"a\", 32),\n\
         \x20   }};\n\
         \x20   T {{ churn: string.repeat(\"b\", 32), ..b }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = mk();\n\
         \x20       total = total + r.churn.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Isolation control for the reproducer: identical nested payload, constructed
/// directly without a functional update.
fn nested_option_without_funcupdate_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         type Leaf {{ label: string, n: i64 }}\n\
         type Wrapper {{ inner: (Option<Leaf>, i64), tag: string }}\n\
         type T {{ pair: (Wrapper, i64), churn: string }}\n\
         fn mk() -> T {{\n\
         \x20   T {{\n\
         \x20       pair: (Wrapper {{ inner: (Some(Leaf {{ label: string.repeat(\"k\", 32), n: 1 }}), 9), tag: string.repeat(\"w\", 32) }}, 5),\n\
         \x20       churn: string.repeat(\"a\", 32),\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = mk();\n\
         \x20       total = total + r.churn.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total - total\n\
         }}\n"
    )
}

/// Isolation control for the reproducer: identical functional-update nesting,
/// with the unsupported enum path replaced by a carry-sound string leaf.
fn nested_record_without_option_source(frames: usize) -> String {
    format!(
        "import std.string;\n\
         type Wrapper {{ inner: (string, i64), tag: string }}\n\
         type T {{ pair: (Wrapper, i64), churn: string }}\n\
         fn mk() -> T {{\n\
         \x20   let b = T {{\n\
         \x20       pair: (Wrapper {{ inner: (string.repeat(\"k\", 32), 9), tag: string.repeat(\"w\", 32) }}, 5),\n\
         \x20       churn: string.repeat(\"a\", 32),\n\
         \x20   }};\n\
         \x20   T {{ churn: string.repeat(\"b\", 32), ..b }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = mk();\n\
         \x20       total = total + r.churn.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total - total\n\
         }}\n"
    )
}

// ── plumbing (same shape as bytes_drop_leak_oracle) ───────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir` and
/// return the binary path.
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

/// Build the shape at LOW and HIGH frame counts, measure leak NODE
/// counts, and assert the delta stays within `SLOPE_TOLERANCE`.
fn assert_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
    require_leaks_tool();

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("funcupdate-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(LOW_FRAMES),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(HIGH_FRAMES),
        dir.path(),
        &format!("{shape_name}_high"),
    );

    let low_leaks = measure_leaks(&bin_low);
    let high_leaks = measure_leaks(&bin_high);

    eprintln!(
        "{shape_name}: low_frames={LOW_FRAMES} low_leaks={low_leaks} \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks} \
         tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={LOW_FRAMES} low_leaks={low_leaks}, \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates the old field value is not being released \
         at the field-replacement site (pre-fix slope: ~1 node/frame per overridden owned \
         field). Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to identify the \
         leaked stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
    assert!(
        high_leaks + SLOPE_TOLERANCE >= low_leaks,
        "{shape_name}: HIGH leak count is more than {SLOPE_TOLERANCE} below LOW \
         (low={low_leaks}, high={high_leaks}) — the binary did not finish before \
         `leaks --atExit` snapshotted. Increase the iteration count."
    );
}

fn assert_scribble_clean(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("field-store-scribble-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name} must not free the replacement through the old field owner:\n{}",
        describe_output(&output)
    );
}

fn assert_check_fails_closed(shape_name: &str, source: &str) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("funcupdate-check-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{shape_name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");
    let output = Command::new(hew_binary())
        .args(["check", hew_src.to_str().expect("hew src utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");
    assert!(
        !output.status.success(),
        "{shape_name} must fail closed before a leaking executable is emitted:\n{}",
        describe_output(&output)
    );
    let out = describe_output(&output);
    assert!(
        out.contains("carry of owned non-record field") || out.contains("E_NOT_YET_IMPLEMENTED"),
        "{shape_name} must stop at the carry-rule diagnostic; got:\n{out}"
    );
}

fn assert_exact_zero_leaks(shape_name: &str, source_fn: fn(usize) -> String) {
    require_leaks_tool();
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("funcupdate-zero-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let source = source_fn(HIGH_FRAMES);
    let bin = compile_to_native(&source, dir.path(), shape_name);
    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks, 0,
        "{shape_name} must report exactly zero leaks over {HIGH_FRAMES} iterations"
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name} must exit cleanly under MallocScribble:\n{}",
        describe_output(&output)
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// `string` field override: pre-fix slope ~1.0 node/frame (one leaked
/// `cstring` buffer per iteration); post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_string_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_string_field", string_field_source);
}

/// `bytes` field override: pre-fix slope ~1.0 node/frame (one leaked
/// `BytesTriple` data-ptr per iteration); post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_bytes_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_bytes_field", bytes_field_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn direct_string_field_store_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance(
        "direct_string_field_store",
        direct_string_field_store_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn direct_string_self_store_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("direct_string_self_store", direct_string_self_store_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn direct_bytes_field_store_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("direct_bytes_field_store", direct_bytes_field_store_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn direct_string_field_stores_are_clean_under_malloc_scribble() {
    assert_scribble_clean(
        "direct_string_field_store",
        &direct_string_field_store_source(32),
    );
    assert_scribble_clean(
        "direct_string_self_store",
        &direct_string_self_store_source(32),
    );
}

/// A record whose heap-string field is reassigned inside a per-frame callee
/// must retain its scope-exit `RecordInPlace` drop: pre-fix each call abandoned
/// the reassigned buffer at the callee's scope exit — a per-frame slope the
/// in-`main` loop shapes cannot see; post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn callee_scope_string_store_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance(
        "callee_scope_string_store",
        callee_scope_string_store_source,
    );
}

/// The rule-17 mixed case: a callee record with an affine `#[resource]` field
/// beside the reassigned string sibling must release the string sibling once
/// per frame (flat slope) WITHOUT re-closing the discharged handle.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn callee_scope_resource_string_sibling_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance(
        "callee_scope_resource_string_sibling",
        callee_scope_resource_string_sibling_source,
    );
}

/// The rule-17 double-free guard: the mixed resource+string callee must exit
/// cleanly under `MallocScribble` — the record's `RecordInPlace` drop releases
/// the string sibling but must not re-run the already-closed handle's release.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn callee_scope_resource_string_sibling_is_clean_under_malloc_scribble() {
    assert_scribble_clean(
        "callee_scope_resource_string_sibling",
        &callee_scope_resource_string_sibling_source(32),
    );
    assert_scribble_clean(
        "callee_scope_string_store",
        &callee_scope_string_store_source(32),
    );
}

/// `Vec<i64>` field override: pre-fix slope ~1.0 node/frame (one leaked
/// Vec header per iteration); post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_vec_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_vec_field", vec_field_source);
}

/// `HashMap<string,i64>` field override: pre-fix slope ~1.0 node/frame
/// (one leaked map-control node per iteration); post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_hashmap_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_hashmap_field", hashmap_field_source);
}

/// `HashSet<i64>` field override: pre-fix slope ~1.0 node/frame (one
/// leaked set-control node per iteration); post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_hashset_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_hashset_field", hashset_field_source);
}

/// Multi-field override (`string` + `Vec<i64>`): pre-fix slope ~2.0
/// nodes/frame (two leaked allocations per iteration); post-fix slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_multi_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_multi_field", multi_field_source);
}

// ── carry-axis oracles (coverage-gap closure) ──────────────────────────────

/// Carried nested owned-RECORD field: pre-fix this SIGSEGV'd on the
/// double-freed carried record; post-fix the carry transfers it once per
/// frame — slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_record_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_record_field", carry_record_field_source);
}

/// Carried `string` field (the retain-vs-exclude question): the
/// carried string is moved once per frame, not retained-without-release —
/// slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_string_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_string_field", carry_string_field_source);
}

/// Carried `Vec<string>` (owned-element) field: moved once per frame, slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_vec_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_vec_field", carry_vec_field_source);
}

/// Carried `HashMap<string,string>` field: moved once per frame, slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_hashmap_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_hashmap_field",
        carry_hashmap_field_source,
    );
}

/// Carried `HashSet<string>` field: moved once per frame, slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_hashset_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_hashset_field",
        carry_hashset_field_source,
    );
}

/// Carried `bytes` field: the fat `{ptr,len,cap}` triple is moved once per
/// frame, slope 0.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_bytes_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_bytes_field", carry_bytes_field_source);
}

/// Carried `(string, i64)` tuple field, ESCAPE drop order: the result outlives
/// the frame that consumed the base. Slope 0 — the tuple's allocation is handed
/// over once and released once.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_tuple_field_escape_order_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_tuple_field_escape",
        carry_tuple_field_escape_source,
    );
}

/// Carried `(string, i64)` tuple field, SAME-FRAME drop order: result and
/// consumed base die at one scope exit. Slope 0, and no crash — a transfer that
/// left the base owning the tuple would double-free here.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_tuple_field_same_frame_order_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_tuple_field_same_frame",
        carry_tuple_field_same_frame_source,
    );
}

/// Carried `(Inner, i64)` tuple field: the heap leaf sits a tuple element AND a
/// record field deep, so the transfer must carry the whole nested obligation.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn funcupdate_carry_tuple_of_record_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_tuple_of_record_field",
        carry_tuple_of_record_field_source,
    );
}

/// The leaking round-2 shape must be refused before native code generation.
/// A fail-closed diagnostic is the zero-leak outcome while enum-shaped carries
/// have no move-specific release protocol.
#[test]
fn funcupdate_carry_record_with_nested_option_fails_closed() {
    assert_check_fails_closed(
        "funcupdate_carry_record_nested_option",
        &carry_record_nested_option_source(HIGH_FRAMES),
    );
}

/// Control 1: the nested enum payload is leak-clean when no functional update
/// carries it through a shallow field load.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn nested_option_without_funcupdate_has_exactly_zero_leaks() {
    assert_exact_zero_leaks(
        "nested_option_without_funcupdate",
        nested_option_without_funcupdate_source,
    );
}

/// Control 2: the same functional-update nesting is leak-clean when every leaf
/// is carry-sound and no enum boundary is present.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn nested_record_without_option_has_exactly_zero_leaks() {
    assert_exact_zero_leaks(
        "nested_record_without_option",
        nested_record_without_option_source,
    );
}
