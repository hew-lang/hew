//! Borrowed-`string` collection-ingress exactly-once oracle.
//!
//! `HashMap::insert` / `HashSet::insert` take ownership of what they are given:
//! the runtime byte-copies the key/value/element blob into the slot and releases
//! it later through the layout descriptor's `drop_fn` (a MOVE ingress, with
//! copy-in intentionally absent — `hew-runtime/src/hashmap.rs`). A by-value
//! `string` parameter, by contrast, is BORROWED: `string` sits on the `CoW` borrow
//! spine, so `lower_params` mints no callee-side owner for it and the CALLER
//! keeps the count and drops it at its own scope exit.
//!
//! Pairing those two directly makes the collection a SECOND owner of the
//! caller's count. MIR's static consume at the insert site cannot suppress the
//! caller's drop — it lives in another frame — so both released the same buffer
//! and the program aborted at teardown with
//! `hew-cabi: free_cstring: C-string header sentinel missing`, AFTER printing
//! correct output. The fix retains (`+1`) the borrowed operand before the move,
//! so the collection owns the new count and the caller still owns its own.
//!
//! ## Why both oracles are required
//!
//! A retain with no matching release stops the abort by LEAKING, which is not a
//! fix. These two pin the exactly-once property from both sides:
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): run the
//!   reported repro — a helper that takes a `string` parameter, calls
//!   `counts.get(category)`, then `counts.insert(category, next)` — over
//!   repeated and distinct HEAP keys, plus the `HashMap` value and `HashSet`
//!   element siblings. Under `MallocScribble`/`MallocPreScribble`/
//!   `MallocGuardEdges` a double-free aborts and a use-after-free read returns
//!   scribbled bytes, so the exact stdout plus the clean exit pin the
//!   over-release direction.
//!
//! - **Per-iteration leak slope** (macOS-only via `leaks(1)`): the same
//!   insert-through-a-parameter cycle looped at a LOW and a HIGH iteration
//!   count must hold the leak NODE count flat. An unbalanced retain shows as a
//!   positive slope that scales with the iteration count, pinning the
//!   under-release direction.
//!
//! Every key is a HEAP string (`.to_upper()` / concatenation). A static literal
//! drops as a no-op and would make either probe vacuous.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! elsewhere it logs `skip:` and returns. The scribble correctness pin runs on
//! any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// The reported repro plus its siblings, run once under the poisoned allocator.
///
/// `bump` is the exact reduced shape from the field report: a `string`
/// parameter used first as a `get` key and then as an `insert` key. The input
/// repeats `coffee`, so both the vacant path (the map moves the key in) and the
/// overwrite path (the runtime keeps the stored key and codegen releases the
/// caller's duplicate) execute. `store_value` and `add_member` cover the
/// `HashMap` VALUE and `HashSet` ELEMENT positions of the same MOVE ingress.
const BORROWED_STRING_INGRESS_SOURCE: &str = "\
fn bump(counts: HashMap<string, i64>, category: string) {\n\
\x20   let next = match counts.get(category) { Some(old) => old + 1, None => 1 };\n\
\x20   counts.insert(category, next);\n\
}\n\
\n\
fn store_value(labels: HashMap<i64, string>, id: i64, label: string) {\n\
\x20   labels.insert(id, label);\n\
}\n\
\n\
fn add_member(members: HashSet<string>, name: string) {\n\
\x20   members.insert(name);\n\
}\n\
\n\
fn main() {\n\
\x20   let counts: HashMap<string, i64> = HashMap::new();\n\
\x20   for raw in \" coffee , rent , coffee \".split(\",\") {\n\
\x20   \x20   bump(counts, raw.trim());\n\
\x20   }\n\
\x20   let labels: HashMap<i64, string> = HashMap::new();\n\
\x20   let members: HashSet<string> = HashSet::new();\n\
\x20   store_value(labels, 1, \"label\".to_upper());\n\
\x20   store_value(labels, 1, \"relabel\".to_upper());\n\
\x20   add_member(members, \"member\".to_upper());\n\
\x20   add_member(members, \"member\".to_upper());\n\
\x20   let coffee = match counts.get(\"coffee\") { Some(n) => n, None => -1 };\n\
\x20   let label = match labels.get(1) { Some(s) => s, None => \"absent\" };\n\
\x20   print(f\"{coffee}|{counts.len()}|{label}|{members.len()}\");\n\
}\n";

/// Expected exact output. `coffee` was bumped twice, three commas yield three
/// categories minus the duplicate (`coffee`, `rent`), the label was overwritten
/// once, and the set deduplicated the repeated member. A use-after-free read on
/// the overwritten label returns scribbled bytes instead of `RELABEL`.
const BORROWED_STRING_INGRESS_EXPECTED: &str = "2|2|RELABEL|1";

/// Looped insert-through-a-borrowed-parameter cycle for the slope probe.
///
/// Each `run_cycle()` builds a FRESH heap key (`"key".to_upper()`), passes it
/// through a `string` parameter into a map insert and a set insert, and returns
/// the summed lengths so nothing can be elided. Both collections drop at the end
/// of the cycle, releasing the retained counts; the caller's own counts drop
/// with the parameters' source temps. Zero per-iteration retention. An
/// unbalanced retain holds one buffer per iteration and grows the node count
/// with `frames`.
fn borrowed_ingress_loop_source(frames: usize) -> String {
    format!(
        "fn store_key(counts: HashMap<string, i64>, key: string, amount: i64) {{\n\
         \x20   counts.insert(key, amount);\n\
         }}\n\
         \n\
         fn store_member(members: HashSet<string>, name: string) {{\n\
         \x20   members.insert(name);\n\
         }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let counts: HashMap<string, i64> = HashMap::new();\n\
         \x20   let members: HashSet<string> = HashSet::new();\n\
         \x20   store_key(counts, \"key\".to_upper(), 1);\n\
         \x20   store_key(counts, \"key\".to_upper(), 2);\n\
         \x20   store_member(members, \"member\".to_upper());\n\
         \x20   counts.len() + members.len()\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ if run_loop({frames}) > 0 {{ 0 }} else {{ 1 }} }}\n"
    )
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Over-release pin: the get-then-insert repro plus the value/element siblings
/// must print exact values and exit clean under the poisoned allocator.
/// Reverting the borrowed-ingress retain fails this with the `free_cstring`
/// sentinel abort at teardown.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrowed_string_collection_ingress_does_not_double_free() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("hashmap-borrowed-string-ingress-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BORROWED_STRING_INGRESS_SOURCE,
        dir.path(),
        "borrowed_string_ingress",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "the get-then-insert repro must run clean under the poisoned allocator — a crash here \
         indicates the borrowed `string` operand was moved into the collection while the caller \
         still held and dropped the same count;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        BORROWED_STRING_INGRESS_EXPECTED,
        "the repro must read back exact counts and the overwritten label verbatim — scribbled or \
         shifted output indicates a use-after-free read on a released key/value;\n{}",
        describe_output(&output)
    );
}

/// Under-release pin: the same ingress looped at LOW vs HIGH iteration counts
/// must hold the leak-node count flat. A retain that no release balances (the
/// naive way to silence the double-free) grows the count with the iteration
/// count and trips the slope assertion.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrowed_string_collection_ingress_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("borrowed_string_ingress", borrowed_ingress_loop_source);
}
