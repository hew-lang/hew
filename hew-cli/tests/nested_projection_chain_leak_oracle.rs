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

/// Match-hop escape (record): an INTERMEDIATE hop of the return alias chain is
/// destructured via `match` (not a field projection) and the leaf bound out of
/// the destructure is returned. The match-bound leaf is a byte-copy alias of
/// the owner's subtree (the destructure load copies the inline aggregate with
/// no retain), so the owner's composite must NOT free the subtree the caller
/// now holds — a re-walk here is the #2384 double-free (`free_cstring` header
/// sentinel abort). The compensator must still release the non-escaped siblings
/// along the match-bound chain (`mid.x`, `o.c`) exactly once.
fn match_intermediate_return_leaf_source(frames: usize) -> String {
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
         \x20   let leaf = match mid {{\n\
         \x20       Mid {{ leaf, x: _ }} => leaf,\n\
         \x20   }};\n\
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

/// Match-hop escape (TUPLE twin): the intermediate tuple hop is destructured
/// via `match (l, _)` and the bound element returned — the tuple prover's twin
/// of the match-hop double-free boundary.
fn match_intermediate_return_tuple_source(frames: usize) -> String {
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
         \x20   let leaf = match mid {{\n\
         \x20       (l, _) => l,\n\
         \x20   }};\n\
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

/// Escaped-return record holds a FLAT slope: when the deep alias escapes into
/// the return, the owner's composite drop is excluded (so the caller frees the
/// escapee's subtree exactly once), and the multi-hop sibling-discharge walk
/// releases the non-escaped siblings ALONG the chain — the outer `c` through the
/// owner and the intermediate `mid.x` through the `mid` alias — each exactly
/// once. Without that walk the widened exclusion removed the composite drop
/// while nothing discharged the deeper siblings, so `mid.x` and the outer `c`
/// leaked every frame (2 strings / call). This is the slope check that was
/// missing for the escape shapes; the double-free pin below only proved no
/// re-free, not the absence of a per-iteration leak.
#[test]
fn escaped_return_record_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_escaped_return", escaped_return_record_source);
}

/// Escaped-into-record holds a FLAT slope: the deep alias is stored into a fresh
/// owning record the caller keeps, so the owner's composite is excluded and the
/// same multi-hop sibling walk discharges the non-escaped siblings along the
/// chain exactly once. The `RecordInit` twin of the returned-record slope.
#[test]
fn escaped_into_record_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_escaped_store", escaped_into_record_source);
}

/// Escaped-return TUPLE holds a FLAT slope (#2383): when the deep tuple alias
/// escapes into the return, the tuple prover excludes the owner's composite
/// drop (the caller frees the escapee's pair exactly once), and the multi-hop
/// chain-sibling walk discharges the non-escaped siblings ALONG the chain —
/// the intermediate `mid.1` through the `mid` alias and the outer `o.1`
/// through the owner — each exactly once. Before the walk covered tuple
/// roots, the widened exclusion removed the composite drop while nothing
/// discharged the deeper siblings, so all four strings of the nested tuple
/// leaked every frame (slope 4/call; the double-free pin above only proved no
/// re-free, not the absence of a per-iteration leak).
#[test]
fn escaped_return_tuple_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("chain_escaped_tuple", escaped_return_tuple_source);
}

/// #2387 match-hop return holds a FLAT slope: the owner composite is excluded
/// so the returned `Leaf` is not re-freed, while the sibling compensator follows
/// the match-bound immediate-parent chain and releases `mid.x` plus `o.c`.
#[test]
fn match_intermediate_return_leaf_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "chain_match_hop_return",
        match_intermediate_return_leaf_source,
    );
}

/// Tuple twin of the match-hop return: the shared chain helper must compensate
/// non-escaped tuple siblings while preserving the returned pair.
#[test]
fn match_intermediate_return_tuple_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "chain_match_hop_return_tuple",
        match_intermediate_return_tuple_source,
    );
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

/// #2384 — match-hop escape (record): the leaf bound out of a `match` on an
/// intermediate alias hop and returned must run clean under the poisoned
/// allocator. Before the fix the match-bound binder was invisible to the
/// composite-drop prover (the alias forward-walk followed only whole-value
/// `Move`s, and the destructure hop is a field load off the scrutinee copy),
/// so the owner's composite re-freed the strings the returned `Leaf` still
/// owns — the `free_cstring` header-sentinel abort.
#[test]
fn match_intermediate_return_leaf_no_double_free_under_malloc_scribble() {
    assert_scribble_clean(
        "chain_match_hop_return",
        &match_intermediate_return_leaf_source(6),
    );
}

/// #2384 tuple twin: the element bound out of a `match` on an intermediate
/// tuple hop and returned — same double-free boundary through the tuple
/// prover.
#[test]
fn match_intermediate_return_tuple_no_double_free_under_malloc_scribble() {
    assert_scribble_clean(
        "chain_match_hop_return_tuple",
        &match_intermediate_return_tuple_source(6),
    );
}

// ── MIR emission pins ────────────────────────────────────────────────────

fn mir_dump(source: &str, prefix: &str, stage: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join("chain_pin.hew");
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", stage])
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("invoke hew compile --dump-mir {stage}: {error}"));
    assert!(
        output.status.success(),
        "{stage} MIR dump must succeed:\n{}",
        describe_output(&output)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn checked_dump(source: &str, prefix: &str) -> String {
    mir_dump(source, prefix, "checked")
}

fn elab_dump(source: &str, prefix: &str) -> String {
    mir_dump(source, prefix, "elab")
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

/// Proven-borrow owner control: `sink(o: Outer)` only reads `o`, so it is a
/// proven-borrowing helper that registers no callee-side drop. Under the
/// copy-on-write borrow model the caller therefore RETAINS the owner and frees
/// it exactly once at its own scope exit: `run_cycle` emits exactly ONE
/// `record_in_place` drop — the owner `o`'s composite. The alias `mid` (a
/// Read-projection of `o.mid`) never owns, so it emits none. Exactly-once is
/// the invariant: two owner drops would be a double-free, zero would be a leak.
#[test]
fn owner_consumed_control_emits_single_owner_drop_no_alias_drop() {
    let dump = elab_dump(
        &owner_consumed_control_source(4),
        "chain-owner-consumed-mir-",
    );
    let run_cycle = dump
        .split("fn ")
        .find(|section| section.starts_with("run_cycle"))
        .expect("run_cycle section present in dump");
    // Teeth 1: the owner frees exactly once — one composite in-place drop, the
    // outer root's. Zero here is the leak (proven-borrow callee freed nothing);
    // two is the double-free (caller AND callee both freed).
    let composites = record_in_place_locals(run_cycle);
    assert_eq!(
        composites.len(),
        1,
        "the proven-borrowing `sink` frees nothing, so the caller retains the \
         owner and frees it exactly once; got {composites:?}\n{run_cycle}"
    );
    // Teeth 2: the single composite is the OWNER's (`ty=Outer`), never the
    // alias `mid` (a Read-projection into the owner's subtree, `ty=Mid`). A
    // `Mid` composite here would be the alias wrongly re-freeing the shared
    // subtree — a double-free of the owner's payload.
    assert!(
        !run_cycle.contains("ty=Mid kind=record_in_place"),
        "the alias `mid` must emit no composite drop; a `ty=Mid` in-place drop \
         means the alias re-freed the owner's shared subtree;\n{run_cycle}"
    );
}

/// #2387 match-hop return: the owner composite stays excluded (no
/// `record_in_place` re-walk of the returned leaf), while checked MIR contains
/// exactly the two sibling discharges for `mid.x` and `o.c`.
#[test]
fn match_intermediate_return_leaf_emits_only_sibling_field_drops() {
    let source = match_intermediate_return_leaf_source(4);
    let checked = checked_dump(&source, "chain-match-hop-return-checked-");
    let checked_deep = checked
        .split("fn ")
        .find(|section| section.starts_with("deep"))
        .expect("deep section present in checked dump");
    let drops: Vec<_> = checked_deep
        .lines()
        .filter_map(|line| line.trim().strip_prefix("drop_field_in_place "))
        .collect();
    assert_eq!(
        drops.len(),
        2,
        "the match-hop return must emit exactly two sibling drops — one for \
         mid.x and one for o.c; got {drops:?}\n{checked_deep}"
    );
    assert!(
        drops
            .iter()
            .all(|line| line.ends_with(".field[1] ty=string")),
        "both sibling drops should release field 1 string siblings (mid.x and \
         o.c), never the escaped field 0 leaf; got {drops:?}\n{checked_deep}"
    );
    let bases: std::collections::BTreeSet<_> = drops
        .iter()
        .filter_map(|line| line.split_once(".field[1] ty=string").map(|(base, _)| base))
        .collect();
    assert_eq!(
        bases.len(),
        2,
        "the two sibling drops must address distinct chain levels (mid and \
         outer), not repeat one field slot; got {drops:?}\n{checked_deep}"
    );

    let elab = elab_dump(&source, "chain-match-hop-return-elab-");
    let elab_deep = elab
        .split("fn ")
        .find(|section| section.starts_with("deep"))
        .expect("deep section present in elab dump");
    assert_eq!(
        elab_deep.matches("kind=record_in_place").count(),
        0,
        "the returned match-bound leaf keeps the owner composite excluded; a \
         record_in_place drop here would re-walk the escaped subtree:\n{elab_deep}"
    );
    assert_eq!(
        elab_deep.matches("kind=tuple_in_place").count(),
        0,
        "the record repro must not acquire an unrelated tuple composite drop:\n{elab_deep}"
    );
}
