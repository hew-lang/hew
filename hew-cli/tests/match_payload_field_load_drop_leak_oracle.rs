//! Retained string field-load drop leak oracle — the match/if-let payload arm.
//!
//! When a `match`/`if let` binds a record payload (`Some(r)`) and reads a heap
//! `string` field back out (`let name = r.name`), the field-load dest is a
//! FRESH `+1`-retained owner (codegen `retain_string_field_load`), not a
//! no-retain interior alias. `derive_cow_fresh_borrowed_owner` admits it for one
//! balancing `hew_string_drop`. The scope-close `Goto` filter must therefore
//! EXEMPT it from the projection-alias taint so its drop fires when the binder
//! leaves scope crossing the match join — it threads the REAL `locals` table so
//! `string_field_load_producer_dest` keeps the exemption (an empty table would
//! taint every field-load dest, strand the release at the join, and leak). The
//! payload alias itself (`r`) stays tainted via the `Move`-from-interior arm and
//! is freed once by the composite, so there is no double-free either way.
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! The cloned-field arm (`r.name.to_upper()`) is looped at a LOW and a HIGH
//! iteration count and the leak-node delta is asserted within a small tolerance:
//! the delta cancels the nondeterministic constant baseline a single-shot `== 0`
//! count cannot. A field-load owner stranded at the join would leak one buffer
//! per iteration (positive slope); the correct scope-close drop holds it flat.
//! The bare-field-load arm (`let name = r.name`) must at minimum run clean — the
//! field share drops once, never twice — which the scribble guard pins.
//!
//! macOS-only for the slope assertion (`leaks(1)` is Darwin's allocator
//! inspector); the scribble correctness pins run on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Single-cycle `match`-arm record payload, a heap `string` field cloned out
/// (`r.name.to_upper()`), borrow-only final use, printing `"m"`. The scribble
/// correctness pin for the cloned-field arm.
const MATCH_FIELD_CLONE_SCRIBBLE_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn make(n: i64) -> Option<Row> {\n\
\x20   if n > 0 { Some(Row { name: \"g64-fieldload-heap\".to_upper() }) } else { None }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   match opt {\n\
\x20       Some(r) => { let name = r.name.to_upper(); if !name.is_empty() { print(\"m\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Bare string field-load binder (`let name = r.name`): the field share must be
/// freed exactly once. The composite frees the record's copy; the binder's
/// retained share drops on scope-close. Borrow-only final use; must run clean
/// (no double-free) under the poisoned allocator.
const MATCH_FIELD_BARE_SCRIBBLE_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn make(n: i64) -> Option<Row> {\n\
\x20   if n > 0 { Some(Row { name: \"g64-bare-fieldload\".to_upper() }) } else { None }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   match opt {\n\
\x20       Some(r) => { let name = r.name; if !name.is_empty() { print(\"m\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Looped cloned-field-load arm for the per-iteration slope probe. Each cycle
/// binds a FRESH `Some(Row { name: "…".to_upper() })`, clones the field out
/// (`r.name.to_upper()`), and returns its length so the loop is not dead code.
/// The cloned owner must drop on the match scope-close edge; a stranded release
/// leaks one buffer per iteration.
fn match_field_clone_loop_source(frames: usize) -> String {
    format!(
        "type Row {{ name: string; }}\n\
         \n\
         fn make(n: i64) -> Option<Row> {{\n\
         \x20   if n > 0 {{ Some(Row {{ name: \"g64-fieldload-heap\".to_upper() }}) }} else {{ None }}\n\
         }}\n\
         \n\
         fn run_cycle(n: i64) -> i64 {{\n\
         \x20   let opt = make(n);\n\
         \x20   var got: i64 = 0;\n\
         \x20   match opt {{\n\
         \x20       Some(r) => {{ let name = r.name.to_upper(); if !name.is_empty() {{ got = name.len(); }} }}\n\
         \x20       None => {{}}\n\
         \x20   }}\n\
         \x20   got\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(i + 1); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ run_loop({frames}) }}\n"
    )
}

// ── scribble correctness pin ──────────────────────────────────────────────

/// Compile + run under the poisoned allocator; assert clean exit + verbatim out.
fn assert_clean_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("match-field-load-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here means the field-load \
         share was double-freed (the binder release and composite both freed it);\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "{name} must read the bound field back verbatim — scribbled output is a use-after-free;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Slope oracle: the cloned field-load (`r.name.to_upper()`) in a match arm
/// looped at LOW vs HIGH iteration counts holds the leak-node count flat — the
/// fresh owner drops on the scope-close edge every iteration. A stranded release
/// (empty-`locals` taint) grows the count with the iteration count.
#[test]
fn match_payload_field_clone_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g64_match_field_clone", match_field_clone_loop_source);
}

#[test]
fn match_payload_field_clone_no_double_free_under_malloc_scribble() {
    assert_clean_under_malloc_scribble(
        "g64_match_field_clone_scribble",
        MATCH_FIELD_CLONE_SCRIBBLE_SOURCE,
        "m",
    );
}

/// Bare field-load binder (`let name = r.name`): the field share drops exactly
/// once — never double-freed against the composite's recursive record drop.
#[test]
fn match_payload_bare_field_load_no_double_free_under_malloc_scribble() {
    assert_clean_under_malloc_scribble(
        "g64_match_bare_field",
        MATCH_FIELD_BARE_SCRIBBLE_SOURCE,
        "m",
    );
}
