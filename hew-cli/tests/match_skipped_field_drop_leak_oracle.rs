//! Skipped-field drop oracle for partial match destructures — the
//! empirical (compiled-binary) half of the exact-drop-op matrix whose MIR
//! side lives in `examples/v05/checked-mir/match_skip_*.hew`.
//!
//! ## What the safety-drop loop emits per skipped shape
//!
//! A match arm that binds SOME fields of an owned record/tuple scrutinee
//! and wildcards the rest discharges every skipped owned field before the
//! arm body runs:
//!
//! - skipped `string` fields → `Instr::FieldDropInPlace` (raw slot load,
//!   `hew_string_drop`, pointer-word null-store). String field LOADS
//!   retain via `hew_string_clone`, so a load+`Drop` pair retain-cancels
//!   and leaks the ORIGINAL slot value — the in-place drop is the only
//!   leak-free discharge for strings.
//! - skipped aggregates (record / tuple / inline enum / indirect enum /
//!   fixed array) → `Instr::FieldDropInPlace`, type-directed at codegen
//!   through `emit_heap_slot_drop`.
//! - skipped non-string leaves (`Vec` / `bytes` / hash collections) keep
//!   the load + inline-`Drop` leaf path (their loads do not retain).
//!
//! Parent suppression is exactly-once: the composite-drop provers exclude
//! the scrutinee root directly on the `FieldDropInPlace` base. Inline
//! composites have NO null-store postcondition, so a composite re-walk of
//! the freed field would be a double-free — the scribble pins below abort
//! on exactly that regression.
//!
//! ## Why every binder in these fixtures is `BitCopy`
//!
//! The suppression must hold with NO heap-owning extracted binder (the
//! bitcopy-binder case): a no-temp `FieldDropInPlace` seeds neither
//! `field_binders` nor `release_owner_bases`, so only the direct prover
//! exclusion keeps the composite out. Binding `n: i64` isolates exactly
//! that rule — and keeps the slope signal clean of the separate
//! bound-owned-field extraction behaviour.
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! Each shape compiles at LOW and HIGH iteration counts and the leak-NODE
//! delta must stay within tolerance (see `support::leak_slope`). A
//! regressed discharge leaks one node per iteration — an order of
//! magnitude above the tolerance. macOS-only for the slope legs
//! (`leaks(1)`); the scribble pins run on any unix host.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── looped slope fixtures ───────────────────────────────────────────────

/// Record parent, skipped `string` sibling, `BitCopy` binder. Pre-in-place
/// behaviour leaked one `to_upper()` buffer per iteration (the
/// retain-cancel); the in-place drop holds the slope flat.
fn skip_string_record_loop_source(frames: usize) -> String {
    format!(
        "type Tagged {{\n\
         \x20   n: i64,\n\
         \x20   s: string,\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let t = Tagged {{ n: k, s: \"skip-string-heap-payload\".to_upper() }};\n\
         \x20   let x = match t {{\n\
         \x20       Tagged {{ n: x, s: _ }} => x,\n\
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

/// Tuple parent, skipped `string` element, `BitCopy` binder — the same
/// retain-cancel hazard through `FieldAddr::Tuple` addressing.
fn skip_string_tuple_loop_source(frames: usize) -> String {
    format!(
        "fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let t = (k, \"skip-tuple-heap-payload\".to_upper());\n\
         \x20   let x = match t {{\n\
         \x20       (x, _) => x,\n\
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

/// Record parent, skipped owned-RECORD sibling (`Inner` owns a `string`),
/// `BitCopy` binder, inside a `while` loop — the back-edge leg: the
/// in-place drop must fire exactly once per iteration and no back-edge
/// `DropPlan` may re-walk the freed field.
fn skip_record_field_loop_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   value: string,\n\
         }}\n\
         \n\
         type Holder {{\n\
         \x20   n: i64,\n\
         \x20   inner: Inner,\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   let h = Holder {{\n\
         \x20       n: k,\n\
         \x20       inner: Inner {{ value: \"skip-record-heap-payload\".to_upper() }},\n\
         \x20   }};\n\
         \x20   let x = match h {{\n\
         \x20       Holder {{ n: x, inner: _ }} => x,\n\
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

/// Two branch-local destructures — one partial-skip (in-place drops), one
/// all-wildcard (retained composite drop) — flowing into a join. Each
/// path must discharge its own local exactly once and nothing may re-drop
/// after the join.
fn skip_join_paths_loop_source(frames: usize) -> String {
    format!(
        "type Inner {{\n\
         \x20   value: string,\n\
         }}\n\
         \n\
         type Duo {{\n\
         \x20   n: i64,\n\
         \x20   inner: Inner,\n\
         \x20   s: string,\n\
         }}\n\
         \n\
         fn make(n: i64) -> Duo {{\n\
         \x20   Duo {{\n\
         \x20       n: n,\n\
         \x20       inner: Inner {{ value: \"join-inner-heap-payload\".to_upper() }},\n\
         \x20       s: \"join-s-heap-payload\".to_upper(),\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn run_cycle(k: i64) -> i64 {{\n\
         \x20   if k - (k / 2) * 2 == 0 {{\n\
         \x20       let d = make(k);\n\
         \x20       let x = match d {{\n\
         \x20           Duo {{ n: x, inner: _, s: _ }} => x,\n\
         \x20       }};\n\
         \x20       return x + 1;\n\
         \x20   }}\n\
         \x20   let e = make(k);\n\
         \x20   let z = match e {{\n\
         \x20       Duo {{ n: _, inner: _, s: _ }} => 1,\n\
         \x20   }};\n\
         \x20   z\n\
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

// ── scribble fixtures (single-cycle, exact stdout) ──────────────────────

/// Single-cycle skipped-aggregate destructure printing `k`. A composite
/// re-walk of the in-place-freed `Inner` (no null-store saves an inline
/// composite) aborts under the poisoned allocator.
const SKIP_AGGREGATE_SCRIBBLE_SOURCE: &str = "\
type Inner {\n\
\x20   value: string,\n\
}\n\
\n\
type Holder {\n\
\x20   n: i64,\n\
\x20   inner: Inner,\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let h = Holder {\n\
\x20       n: 7,\n\
\x20       inner: Inner { value: \"scribble-heap-payload\".to_upper() },\n\
\x20   };\n\
\x20   let x = match h {\n\
\x20       Holder { n: x, inner: _ } => x,\n\
\x20   };\n\
\x20   if x == 7 {\n\
\x20       print(\"k\");\n\
\x20   }\n\
\x20   0\n\
}\n";

// ── slope oracles ───────────────────────────────────────────────────────

/// Skipped `string` record field holds a flat leak slope — the
/// retain-cancel pin (record parent). A reroute back onto the load+drop
/// path leaks one buffer per iteration and trips the tolerance.
#[test]
fn skipped_string_record_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("skip_string_record", skip_string_record_loop_source);
}

/// Skipped `string` tuple element holds a flat leak slope — the
/// retain-cancel pin (tuple parent).
#[test]
fn skipped_string_tuple_element_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("skip_string_tuple", skip_string_tuple_loop_source);
}

/// Skipped owned-record field inside a loop holds a flat leak slope —
/// the back-edge leg: `FieldDropInPlace` fires exactly once per
/// iteration, and the bitcopy-binder prover exclusion (not a heap-owning
/// binder) suppresses the composite.
#[test]
fn skipped_record_field_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("skip_record_field_loop", skip_record_field_loop_source);
}

/// Partial-skip and all-wildcard paths joining holds a flat leak slope —
/// each path discharges its own local exactly once (in-place drops vs the
/// retained composite drop) and nothing re-drops after the join.
#[test]
fn skipped_field_join_paths_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("skip_join_paths", skip_join_paths_loop_source);
}

// ── scribble (double-free / use-after-free) pins ────────────────────────

/// The skipped-aggregate destructure runs clean under the poisoned
/// allocator and prints its sentinel — a composite re-drop of the
/// in-place-freed field (inline composites have no null-store) aborts
/// here.
#[test]
fn skipped_aggregate_field_no_double_free_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("skip-aggregate-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        SKIP_AGGREGATE_SCRIBBLE_SOURCE,
        dir.path(),
        "skip_agg_scribble",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "skipped-aggregate destructure must run clean under the poisoned allocator — a \
         crash here means the composite drop re-walked the in-place-freed field;\n{}",
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

/// The join shape runs clean under the poisoned allocator — neither path
/// double-frees and the join block re-drops nothing.
#[test]
fn skipped_field_join_paths_no_double_free_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("skip-join-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &skip_join_paths_loop_source(4),
        dir.path(),
        "skip_join_scribble",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "join-path destructures must run clean under the poisoned allocator;\n{}",
        describe_output(&output)
    );
}

// ── MIR emission pins for shapes refused downstream of MIR ─────────────
//
// A record carrying an indirect-enum field or a fixed-array field does
// not compile to native today: the record layout fill embeds shapes the
// init path refuses fail-closed (`E_CODEGEN_FRONT_*`), UPSTREAM of any
// drop. The skipped-field admission and emission contract still holds at
// the MIR level — these pins dump `--dump-mir raw` and assert the exact
// op, so the day the layout seam learns these shapes the drop path is
// already correct (and codegen's field-addressed lowering is pinned at
// the IR level in `hew-codegen-rs`'s `field_drop_in_place_*` tests).

/// Run `hew compile --dump-mir raw` over `source` and return the dump.
/// Panics (with the compiler output) if MIR construction fails.
fn dump_raw_mir(name: &str, source: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix(&format!("skip-mir-pin-{name}-"))
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

/// A skipped indirect-enum field emits `FieldDropInPlace` carrying the
/// enum type (codegen dispatches `is_indirect_enum` FIRST — the recursive
/// node free, never the inline enum helper).
#[test]
fn skipped_indirect_enum_field_emits_field_drop_in_place_mir() {
    require_codegen();

    let source = "\
indirect enum Chain {\n\
\x20   End;\n\
\x20   Link(string, Chain);\n\
}\n\
\n\
type Holder {\n\
\x20   n: i64,\n\
\x20   chain: Chain,\n\
}\n\
\n\
fn consume(h: Holder) -> i64 {\n\
\x20   let x = match h {\n\
\x20       Holder { n: x, chain: _ } => x,\n\
\x20   };\n\
\x20   x\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   consume(Holder { n: 0, chain: Chain::End })\n\
}\n";
    let dump = dump_raw_mir("skip_indirect", source);
    assert!(
        dump.contains("drop_field_in_place") && dump.contains("ty=Chain"),
        "skipped indirect-enum field must emit FieldDropInPlace with the enum ty; dump:\n{dump}"
    );
}

/// A skipped fixed-array-of-owned field emits `FieldDropInPlace` carrying
/// the array type (per-element walk at codegen).
#[test]
fn skipped_array_field_emits_field_drop_in_place_mir() {
    require_codegen();

    let source = "\
type Holder {\n\
\x20   n: i64,\n\
\x20   slots: [string; 3],\n\
}\n\
\n\
fn consume(h: Holder) -> i64 {\n\
\x20   let x = match h {\n\
\x20       Holder { n: x, slots: _ } => x,\n\
\x20   };\n\
\x20   x\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   consume(Holder { n: 0, slots: [\"s\"; 3] })\n\
}\n";
    let dump = dump_raw_mir("skip_array", source);
    assert!(
        dump.contains("drop_field_in_place") && dump.contains("ty=[string; 3]"),
        "skipped fixed-array field must emit FieldDropInPlace with the array ty; dump:\n{dump}"
    );
}
