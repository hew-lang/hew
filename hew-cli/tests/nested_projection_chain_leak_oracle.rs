//! Nested-projection-chain ownership oracle — the compiled-binary pins for a
//! MULTI-level field-projection alias chain.
//!
//! ## The shape
//!
//! ```text
//! let o    = make_outer(k);   // fresh owner of the whole tree
//! let mid  = o.mid;           // byte-copy interior alias of `o`
//! let leaf = mid.leaf;        // byte-copy interior alias of `mid` (of `o`)
//! match leaf { Leaf { s, t: _ } => … }
//! ```
//!
//! Each `let x = agg.field` where the field is an inline aggregate
//! (record / tuple / inline-enum) byte-copies the member with NO retain, so
//! `mid` and `leaf` are interior ALIASES of the still-live `o` — not
//! independent owners. `o`'s composite in-place drop frees every original in
//! the tree exactly once; `mid` and `leaf` must emit NO composite drop of
//! their own (a re-walk of storage `o` still owns is a double-free — inline
//! composites have no null-store, so the second free lands on live memory).
//!
//! ## What regressed without the fix (reproduced, fresh builds)
//!
//! `mid` is a NON-consumed field-binder of `o` that seeded the record
//! composite prover's `release_owner_bases` (it registered as an owned local
//! by TYPE alone), so the prover's Defect-1 blanket tripped and excluded
//! EVERY root — `o`, `mid`, and `leaf` — from its composite drop. The whole
//! tree leaked (~4 nodes / iteration for the record chain, the outer `c`, the
//! `mid.x`, and both leaf strings). Classifying the projection as a
//! byte-copy alias (`Disposition::AliasOf`) takes `mid`/`leaf` out of the
//! release-owner set, so the blanket no longer trips and `o` stays admitted.
//!
//! Unrecorded provenance keeps the fail-closed blanket (leak, never
//! double-free): only a projection whose owner root is named at the defining
//! write is reclassified.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── looped slope fixtures ───────────────────────────────────────────────

/// Two-level record chain (#2375). The outer root's composite must free the
/// outer `c`, `mid.x`, and both leaf strings — each exactly once.
fn two_level_record_chain_source(frames: usize) -> String {
    format!(
        "type Leaf {{\n\
         \x20   s: string,\n\
         \x20   t: string,\n\
         }}\n\
         \n\
         type Mid {{\n\
         \x20   leaf: Leaf,\n\
         \x20   x: string,\n\
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
         \x20           leaf: Leaf {{\n\
         \x20               s: \"leaf-s-heap-payload\".to_upper(),\n\
         \x20               t: \"leaf-t-heap-payload\".to_upper(),\n\
         \x20           }},\n\
         \x20           x: \"mid-x-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"outer-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let leaf = mid.leaf;\n\
         \x20   let x = match leaf {{\n\
         \x20       Leaf {{ s, t: _ }} => s.len(),\n\
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

/// Two-level record chain, both leaf fields SKIPPED (`{ s: _, t: _ }`). The
/// outer composite still owns and frees the whole tree; nothing is bound out.
fn two_level_double_skip_source(frames: usize) -> String {
    format!(
        "type Leaf {{\n\
         \x20   s: string,\n\
         \x20   t: string,\n\
         }}\n\
         \n\
         type Mid {{\n\
         \x20   leaf: Leaf,\n\
         \x20   x: string,\n\
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
         \x20           leaf: Leaf {{\n\
         \x20               s: \"leaf-s-heap-payload\".to_upper(),\n\
         \x20               t: \"leaf-t-heap-payload\".to_upper(),\n\
         \x20           }},\n\
         \x20           x: \"mid-x-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"outer-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let leaf = mid.leaf;\n\
         \x20   let x = match leaf {{\n\
         \x20       Leaf {{ s: _, t: _ }} => 1,\n\
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

/// Two-level TUPLE chain — the tuple composite prover's twin of the record
/// path. `o.0` then `mid.0`, both byte-copy aliases; the outer tuple frees
/// every original.
fn two_level_tuple_chain_source(frames: usize) -> String {
    format!(
        "fn make_nested(k: i64) -> (((string, string), string), string) {{\n\
         \x20   let a = \"tup-a-heap-payload\".to_upper();\n\
         \x20   let b = \"tup-b-heap-payload\".to_upper();\n\
         \x20   let c = \"tup-c-heap-payload\".to_upper();\n\
         \x20   let d = \"tup-d-heap-payload\".to_upper();\n\
         \x20   ((( a, b ), c ), d)\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_nested(k);\n\
         \x20   let mid = o.0;\n\
         \x20   let leaf = mid.0;\n\
         \x20   let x = match leaf {{\n\
         \x20       (s, _) => s.len(),\n\
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

/// Bystander control: an aliased chain AND an unrelated standalone owned
/// record in the same frame. Admitting the chain's outer root must not
/// disturb the standalone — it keeps its own composite drop and neither
/// leaks nor double-frees.
fn bystander_root_source(frames: usize) -> String {
    format!(
        "type Mid {{\n\
         \x20   a: string,\n\
         \x20   b: string,\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   mid: Mid,\n\
         \x20   c: string,\n\
         }}\n\
         \n\
         type Bystander {{\n\
         \x20   name: string,\n\
         }}\n\
         \n\
         fn make_outer(k: i64) -> Outer {{\n\
         \x20   Outer {{\n\
         \x20       mid: Mid {{\n\
         \x20           a: \"by-a-heap-payload\".to_upper(),\n\
         \x20           b: \"by-b-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"by-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn make_bystander(k: i64) -> Bystander {{\n\
         \x20   Bystander {{ name: \"bystander-heap-payload\".to_upper() }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let extra = make_bystander(k);\n\
         \x20   let n = match mid {{\n\
         \x20       Mid {{ a, b: _ }} => a.len(),\n\
         \x20   }};\n\
         \x20   n + extra.name.len()\n\
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

/// `HandleTransfer` control: `let g = h.g` extracts a `Generator` field — a
/// single-pointer heap leaf the load TRANSFERS to the binder. The binder
/// becomes the owner (consumed by the for-loop here); the record's whole-root
/// exclusion posture is correct and UNCHANGED by the alias fix (a `Generator`
/// is never classified `ByteCopyAlias`). The pin is no double-free.
fn handle_transfer_control_source(frames: usize) -> String {
    format!(
        "type Holder {{\n\
         \x20   g: Generator<i64, i64>,\n\
         \x20   label: string,\n\
         }}\n\
         \n\
         fn counter() -> Generator<i64, i64> {{\n\
         \x20   gen {{\n\
         \x20       yield 1;\n\
         \x20       yield 2;\n\
         \x20       return 0;\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn make_holder(k: i64) -> Holder {{\n\
         \x20   Holder {{ g: counter(), label: \"held-heap-payload\".to_upper() }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let h = make_holder(k);\n\
         \x20   let g = h.g;\n\
         \x20   var acc: i64 = 0;\n\
         \x20   for n in g {{ acc = acc + n; }}\n\
         \x20   acc\n\
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

/// Owner-consumed control: the alias binds, then the OWNER root is moved out
/// (`sink(o)` takes it by value). The alias must emit NO drop of its own —
/// the consumer frees the tree. The pin is no double-free (the alias never
/// re-frees storage the consumer already released).
fn owner_consumed_control_source(frames: usize) -> String {
    format!(
        "type Mid {{\n\
         \x20   a: string,\n\
         \x20   b: string,\n\
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
         \x20           a: \"oc-a-heap-payload\".to_upper(),\n\
         \x20           b: \"oc-b-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"oc-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn sink(o: Outer) -> i64 {{ o.c.len() }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let n = sink(o);\n\
         \x20   n\n\
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

/// Escaped-deep-alias control (record): the deep projection alias is RETURNED
/// to the caller, which reads it. The alias points into the owner's still-live
/// subtree, so the owner's composite must NOT free that subtree — otherwise the
/// caller reads (and frees) storage the owner already released (double-free /
/// use-after-free). Fail-closed: the owner is excluded and its non-escaped
/// siblings leak; the pin is no double-free, not a flat slope.
fn escaped_return_record_source(frames: usize) -> String {
    format!(
        "type Leaf {{\n\
         \x20   s: string,\n\
         \x20   t: string,\n\
         }}\n\
         \n\
         type Mid {{\n\
         \x20   leaf: Leaf,\n\
         \x20   x: string,\n\
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
         \x20           leaf: Leaf {{\n\
         \x20               s: \"esc-s-heap-payload\".to_upper(),\n\
         \x20               t: \"esc-t-heap-payload\".to_upper(),\n\
         \x20           }},\n\
         \x20           x: \"esc-x-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"esc-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn deep(k: i64) -> Leaf {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let leaf = mid.leaf;\n\
         \x20   leaf\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let l = deep(k);\n\
         \x20   l.s.len()\n\
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

/// Escaped-deep-alias control (store into an owning record): the deep alias is
/// stored into a fresh owning record (`Holder {{ held: leaf }}`) the caller
/// keeps and reads. Same double-free boundary as the returned variant — the
/// owner must not free the aliased subtree the escapee now shares.
fn escaped_into_record_source(frames: usize) -> String {
    format!(
        "type Leaf {{\n\
         \x20   s: string,\n\
         \x20   t: string,\n\
         }}\n\
         \n\
         type Mid {{\n\
         \x20   leaf: Leaf,\n\
         \x20   x: string,\n\
         }}\n\
         \n\
         type Outer {{\n\
         \x20   mid: Mid,\n\
         \x20   c: string,\n\
         }}\n\
         \n\
         type Holder {{\n\
         \x20   held: Leaf,\n\
         }}\n\
         \n\
         fn make_outer(k: i64) -> Outer {{\n\
         \x20   Outer {{\n\
         \x20       mid: Mid {{\n\
         \x20           leaf: Leaf {{\n\
         \x20               s: \"esc-s-heap-payload\".to_upper(),\n\
         \x20               t: \"esc-t-heap-payload\".to_upper(),\n\
         \x20           }},\n\
         \x20           x: \"esc-x-heap-payload\".to_upper(),\n\
         \x20       }},\n\
         \x20       c: \"esc-c-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn deep(k: i64) -> Holder {{\n\
         \x20   let o = make_outer(k);\n\
         \x20   let mid = o.mid;\n\
         \x20   let leaf = mid.leaf;\n\
         \x20   Holder {{ held: leaf }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let h = deep(k);\n\
         \x20   h.held.s.len()\n\
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

/// Escaped-deep-alias control (TUPLE twin): the deep tuple alias is returned to
/// the caller, which reads it. The tuple prover's twin of the returned-record
/// double-free boundary.
fn escaped_return_tuple_source(frames: usize) -> String {
    format!(
        "fn make_nested(k: i64) -> (((string, string), string), string) {{\n\
         \x20   let a = \"esc-a-heap-payload\".to_upper();\n\
         \x20   let b = \"esc-b-heap-payload\".to_upper();\n\
         \x20   let c = \"esc-c-heap-payload\".to_upper();\n\
         \x20   let d = \"esc-d-heap-payload\".to_upper();\n\
         \x20   ((( a, b ), c ), d)\n\
         }}\n\
         \n\
         fn deep(k: i64) -> (string, string) {{\n\
         \x20   let o = make_nested(k);\n\
         \x20   let mid = o.0;\n\
         \x20   let leaf = mid.0;\n\
         \x20   leaf\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let l = deep(k);\n\
         \x20   l.0.len()\n\
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

// ── slope oracles (leak half) ───────────────────────────────────────────

/// Two-level record chain holds a flat slope — the outer composite frees the
/// whole tree; the intermediate aliases free nothing (#2375).
#[test]
fn two_level_record_chain_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_record", two_level_record_chain_source);
}

/// Double-skip destructure holds a flat slope — nothing bound out, the outer
/// composite owns the whole tree.
#[test]
fn two_level_double_skip_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_double_skip", two_level_double_skip_source);
}

/// Two-level tuple chain holds a flat slope — the tuple prover's twin of the
/// record path.
#[test]
fn two_level_tuple_chain_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_tuple", two_level_tuple_chain_source);
}

/// Bystander standalone record is freed exactly once alongside the admitted
/// chain root — flat slope.
#[test]
fn bystander_root_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_bystander", bystander_root_source);
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

fn assert_scribble_clean(name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("chain-scribble-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort/crash \
         here means a projection alias re-freed storage the owner's composite \
         drop already released (double-free through the projection \
         alias);\n{}",
        describe_output(&output)
    );
}

/// Two-level record chain: no composite re-walk of the aliased tree.
#[test]
fn two_level_record_chain_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_record", &two_level_record_chain_source(6));
}

/// Two-level tuple chain: no re-walk of the aliased tuple tree.
#[test]
fn two_level_tuple_chain_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_tuple", &two_level_tuple_chain_source(6));
}

/// Double-skip: the outer composite frees the whole tree once, no re-walk.
#[test]
fn two_level_double_skip_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_double_skip", &two_level_double_skip_source(6));
}

/// `HandleTransfer` control: the transferred `Generator` handle is released
/// exactly once (by the for-loop consume); the record's whole-root exclusion
/// keeps its composite from re-freeing the moved handle.
#[test]
fn handle_transfer_control_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_handle_transfer", &handle_transfer_control_source(6));
}

/// Owner-consumed control: the alias emits no drop; the consumer frees the
/// tree, and nothing re-frees it.
#[test]
fn owner_consumed_control_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_owner_consumed", &owner_consumed_control_source(6));
}

/// Escaped-return record: the deep alias is handed to the caller. The owner's
/// composite must not free the escapee's still-shared subtree — a re-walk here
/// is the double-free the projection-alias classification must exclude.
#[test]
fn escaped_return_record_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_escaped_return", &escaped_return_record_source(6));
}

/// Escaped into an owning record: the deep alias is stored into a fresh record
/// the caller keeps. Same double-free boundary as the returned variant.
#[test]
fn escaped_into_record_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_escaped_store", &escaped_into_record_source(6));
}

/// Escaped-return TUPLE twin: the deep tuple alias is handed to the caller —
/// the tuple prover's twin of the returned-record double-free boundary.
#[test]
fn escaped_return_tuple_no_double_free_under_malloc_scribble() {
    assert_scribble_clean("chain_escaped_tuple", &escaped_return_tuple_source(6));
}

// ── MIR emission pins ────────────────────────────────────────────────────

fn elab_dump(source: &str, prefix: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join("chain_pin.hew");
    std::fs::write(&hew_src, source).expect("write hew source");

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
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// The distinct MIR locals dropped via `record_in_place` in a dump section —
/// the same composite may appear on several exit edges (return + cancel), so
/// membership, not line count, is the fact under test.
fn record_in_place_locals(section: &str) -> std::collections::BTreeSet<String> {
    section
        .lines()
        .filter(|line| line.contains("kind=record_in_place"))
        .filter_map(|line| {
            let rest = line.trim_start().strip_prefix("drop ")?;
            let local = rest.split_whitespace().next()?;
            Some(local.to_string())
        })
        .collect()
}

/// The two-level chain admits exactly ONE composite in-place drop — the outer
/// root's — and emits none for the intermediate aliases. Only a single local
/// (the outer `Outer` root) appears with `record_in_place`; the alias binders
/// `mid` / `leaf` emit none (though the outer's own drop repeats across exit
/// edges).
#[test]
fn two_level_record_chain_admits_only_the_outer_composite() {
    let dump = elab_dump(&two_level_record_chain_source(4), "chain-record-mir-");
    let run_cycle = dump
        .split("fn ")
        .find(|section| section.starts_with("run_cycle"))
        .expect("run_cycle section present in dump");
    let composites = record_in_place_locals(run_cycle);
    assert_eq!(
        composites.len(),
        1,
        "exactly the outer root keeps its composite in-place drop; the \
         intermediate projection aliases emit none; got {composites:?}\n{run_cycle}"
    );
}

/// Escaped-return record: when the deep alias escapes into the return, the
/// owner `o` is EXCLUDED from its composite in-place drop (fail-closed: the
/// escapee shares the owner's subtree, so the owner must not free it). `deep`
/// emits zero `record_in_place` drops — neither the aliases (they never own)
/// nor the owner (its subtree escaped).
#[test]
fn escaped_return_record_excludes_the_owner_composite() {
    let dump = elab_dump(
        &escaped_return_record_source(4),
        "chain-escaped-return-mir-",
    );
    let deep = dump
        .split("fn ")
        .find(|section| section.starts_with("deep"))
        .expect("deep section present in dump");
    assert_eq!(
        deep.matches("kind=record_in_place").count(),
        0,
        "the deep alias escapes into the return, so the owner is excluded from \
         its composite drop (leak-not-double-free); got a composite drop:\n{deep}"
    );
}

/// Owner-consumed control: the alias `mid` emits no drop and the consumed
/// owner `o` emits no scope-exit composite drop in the caller (the consumer
/// `sink` owns it) — so `run_cycle` emits zero `record_in_place` drops.
#[test]
fn owner_consumed_control_emits_no_alias_or_owner_drop() {
    let dump = elab_dump(
        &owner_consumed_control_source(4),
        "chain-owner-consumed-mir-",
    );
    let run_cycle = dump
        .split("fn ")
        .find(|section| section.starts_with("run_cycle"))
        .expect("run_cycle section present in dump");
    assert_eq!(
        run_cycle.matches("kind=record_in_place").count(),
        0,
        "the alias emits no drop and the consumed owner is freed by the \
         callee, so the caller emits no composite drop;\n{run_cycle}"
    );
}
