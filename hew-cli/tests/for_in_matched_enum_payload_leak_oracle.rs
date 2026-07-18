//! `for it in <matched enum collection payload>` drop oracle (hew-lang/hew#2725, site b).
//!
//! ## The shape
//!
//! An enum with a collection-carrying variant is matched, and the destructured
//! payload binder is iterated by a `for`-in:
//!
//! ```text
//! enum Msg { Empty; Batch([i64]) }
//! match m { Batch(items) => { for it in items { … } }, Empty => {} }
//! ```
//!
//! `items` is a match-projected payload binder that CowShare-borrows the enum's
//! payload buffer; `for it in items` lowers it into a `VecIter { vec: items, idx }`
//! cursor that iterates the shared buffer without freeing it. The scrutinee `m`
//! is NOT consumed (the match reads the payload), so `m` retains sole ownership
//! and its scope-exit `EnumInPlace` composite drop is the single freer of the
//! payload; the borrowing cursor frees nothing.
//!
//! ## What regressed (pre-fix, reproduced on the Stage-1 tip `882ff31d9`)
//!
//! The enum-composite escape scan (`derive_enum_composite_drop_allowed`) read the
//! cursor's `record_init VecIter { vec: items }` ingress of the payload binder as
//! a payload ESCAPE and excluded `m` from its `EnumInPlace` drop — as if the
//! payload left the composite. The place-source cursor borrows (it is never
//! registered for a handle drop), so with `m` excluded NOTHING freed the payload:
//! **2 leak nodes / frame** (the `Vec<i64>` header + buffer), on `882ff31d9`.
//! The sibling INDEX read `items[j]` of the same payload was already clean (its
//! receiver-borrow is exempt), so the gap was specific to the for-in cursor
//! ingress.
//!
//! ## The fix
//!
//! A payload binder read as the `vec` field of a `record_init VecIter` cursor is
//! a borrow — exempt it from the payload-escape scan, mirroring the cursor-ingress
//! exemption `derive_local_collection_drop_allowed` already applies and the
//! `items[j]` / `.len()` receiver-borrow exemptions. `m` is re-admitted and its
//! `EnumInPlace` frees the payload exactly once.
//!
//! ## What this oracle pins
//!
//! * a flat per-iteration leak SLOPE across the LOW/HIGH frame delta — the enum
//!   payload buffer is freed every frame (was ~2 nodes/frame);
//! * no double-free / use-after-free under the poisoned allocator: the handler
//!   alternates the `Batch`/`Empty` variants so both the composite `EnumInPlace`
//!   drop and the empty-variant no-op run every iteration; a cursor that also
//!   freed the shared buffer would abort here.
//! * the INDEX-read sibling (`items[j]`) stays flat — the canary the pre-fix
//!   report cited as already-clean, guarding against the fix over-reaching.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// PRIMARY #2725b shape: `for it in items` over a matched `Batch([i64])` payload.
/// Alternates `Batch`/`Empty` so both the composite drop and the empty no-op run.
fn for_in_matched_payload_loop_source(frames: usize) -> String {
    format!(
        "enum Msg {{ Empty; Batch([i64]) }}\n\
         \n\
         fn frame(n: i64) -> i64 {{\n\
         \x20   let m: Msg = if n % 2 == 0 {{ Msg::Batch([n, n + 1, n + 2]) }} else {{ Msg::Empty }};\n\
         \x20   var sum: i64 = 0;\n\
         \x20   match m {{\n\
         \x20       Msg::Batch(items) => {{ for it in items {{ sum = sum + it; }} }},\n\
         \x20       Msg::Empty => {{ sum = sum + 1; }},\n\
         \x20   }}\n\
         \x20   sum\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total < 0 {{ 73 }} else {{ 0 }}\n\
         }}\n"
    )
}

/// INDEX-read canary: `items[j]` of the same matched payload was already clean
/// pre-fix (receiver-borrow exempt). Pinned flat so the for-in fix does not
/// regress the sibling read.
fn index_read_matched_payload_loop_source(frames: usize) -> String {
    format!(
        "enum Msg {{ Empty; Batch([i64]) }}\n\
         \n\
         fn frame(n: i64) -> i64 {{\n\
         \x20   let m: Msg = if n % 2 == 0 {{ Msg::Batch([n, n + 1, n + 2]) }} else {{ Msg::Empty }};\n\
         \x20   var sum: i64 = 0;\n\
         \x20   match m {{\n\
         \x20       Msg::Batch(items) => {{ sum = sum + items[0] + items.len(); }},\n\
         \x20       Msg::Empty => {{ sum = sum + 1; }},\n\
         \x20   }}\n\
         \x20   sum\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total < 0 {{ 74 }} else {{ 0 }}\n\
         }}\n"
    )
}

/// Deterministic exact-contents / no-double-free source: match a `Batch` payload,
/// sum it via for-in, print the sum. Under the poisoned allocator a cursor that
/// freed the shared buffer double-frees against the composite `EnumInPlace`.
const FOR_IN_PAYLOAD_EXACT_SOURCE: &str = "enum Msg { Empty; Batch([i64]) }\n\
     \n\
     fn main() {\n\
     \x20   let m: Msg = Msg::Batch([10, 20, 30]);\n\
     \x20   var sum: i64 = 0;\n\
     \x20   match m {\n\
     \x20       Msg::Batch(items) => { for it in items { sum = sum + it; } },\n\
     \x20       Msg::Empty => {},\n\
     \x20   }\n\
     \x20   println(f\"{sum}\");\n\
     }\n";

const FOR_IN_PAYLOAD_EXACT_EXPECTED: &str = "60\n";

fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("for-in-matched-payload-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash indicates the borrowing \
         for-in cursor freed the payload buffer the composite EnumInPlace also frees (#2725b \
         over-eager cursor free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must sum the matched payload verbatim under the poisoned allocator;\n{}",
        describe_output(&output)
    );
}

/// PRIMARY #2725b regression: the for-in-over-matched-payload loop must hold a
/// flat leak slope. Reverting the cursor-ingress exemption excludes the enum
/// composite drop and strands the payload buffer (~2 nodes/frame).
#[test]
fn for_in_matched_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "for_in_matched_enum_payload",
        for_in_matched_payload_loop_source,
    );
}

/// INDEX-read canary: the sibling `items[j]` read stays flat.
#[test]
fn index_read_matched_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "index_read_matched_enum_payload",
        index_read_matched_payload_loop_source,
    );
}

/// Exact-contents / no-double-free pin under the poisoned allocator.
#[test]
fn for_in_matched_payload_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "for_in_payload_exact",
        FOR_IN_PAYLOAD_EXACT_SOURCE,
        FOR_IN_PAYLOAD_EXACT_EXPECTED,
    );
}
