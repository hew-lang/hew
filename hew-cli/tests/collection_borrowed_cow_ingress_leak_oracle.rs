//! Carrier-general collection-ingress exactly-once oracles: the borrowed
//! `bytes` parameter, the REASSIGNED parameter generation, and the
//! `VecIter` `string` yield binder.
//!
//! All three are instances of one question — "which frame owns the value
//! currently in this binding?" — answered at a `HashMap`/`HashSet` MOVE
//! ingress or an ownership-transferring use:
//!
//! - a caller-borrowed `bytes` parameter is retained (`+1`) before the move,
//!   exactly like the `string` case
//!   (`hashmap_borrowed_string_ingress_leak_oracle.rs`);
//! - a parameter REASSIGNED to a fresh value holds a frame-owned generation,
//!   so the ingress consumes (transfers) it — a retain there is a count
//!   nothing releases;
//! - a `VecIter` `string` yield binder is frame-owned with a body-end release
//!   authority, so consuming uses lower as retain-backed shares and the
//!   binder's per-iteration drop still runs.
//!
//! Each shape gets both directions: the poisoned-allocator run pins
//! over-release (a double free aborts, a use-after-free read scribbles), and
//! the `leaks(1)` LOW/HIGH slope pins under-release (an unbalanced retain
//! grows the node count with the iteration count).

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── borrowed bytes value ingress ────────────────────────────────────────────

/// A `bytes` payload reaching a `HashMap` VALUE slot through a by-value
/// parameter. Without the carrier-general retain the map teardown and the
/// caller's drop released the same buffer (abort before printing).
const BORROWED_BYTES_INGRESS_SOURCE: &str = "\
fn ingest(store: HashMap<string, bytes>, payload: bytes) -> i64 {\n\
\x20   store.insert(\"k\" + \"-heap\", payload);\n\
\x20   return store.len();\n\
}\n\
\n\
fn main() {\n\
\x20   let store: HashMap<string, bytes> = HashMap::new();\n\
\x20   let payload = b\"0123456789abcdef\";\n\
\x20   let n = ingest(store, payload);\n\
\x20   let still = payload.len();\n\
\x20   print(f\"{n}|{still}\");\n\
}\n";

const BORROWED_BYTES_INGRESS_EXPECTED: &str = "1|16";

fn borrowed_bytes_loop_source(frames: usize) -> String {
    format!(
        "fn ingest(store: HashMap<string, bytes>, payload: bytes) -> i64 {{\n\
         \x20   store.insert(\"k\" + \"-heap\", payload);\n\
         \x20   return store.len();\n\
         }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let store: HashMap<string, bytes> = HashMap::new();\n\
         \x20   let payload = b\"0123456789abcdef0123456789abcdef\";\n\
         \x20   return ingest(store, payload);\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   return total;\n\
         }}\n\
         \n\
         fn main() -> i64 {{ if run_loop({frames}) > 0 {{ 0 }} else {{ 1 }} }}\n"
    )
}

// ── reassigned-parameter generation boundary ────────────────────────────────

/// The parameter slot is reassigned to a fresh heap string before the insert:
/// the new generation is frame-owned, so the ingress must CONSUME it. Covers
/// the vacant path and (second call) the overwrite path.
const REASSIGNED_PARAM_INGRESS_SOURCE: &str = "\
fn put(counts: HashMap<string, i64>, var key: string, amount: i64) -> i64 {\n\
\x20   key = (\"fresh\" + \"-key\").to_upper();\n\
\x20   counts.insert(key, amount);\n\
\x20   return counts.len();\n\
}\n\
\n\
fn main() {\n\
\x20   let counts: HashMap<string, i64> = HashMap::new();\n\
\x20   let first = put(counts, \"caller\" + \"-a\", 1);\n\
\x20   let second = put(counts, \"caller\" + \"-b\", 2);\n\
\x20   let stored = match counts.get(\"FRESH-KEY\") { Some(n) => n, None => -1 };\n\
\x20   print(f\"{first}|{second}|{stored}\");\n\
}\n";

const REASSIGNED_PARAM_INGRESS_EXPECTED: &str = "1|1|2";

fn reassigned_param_loop_source(frames: usize) -> String {
    format!(
        "fn put(counts: HashMap<string, i64>, var key: string, amount: i64) -> i64 {{\n\
         \x20   key = (\"fresh\" + \"-key\").to_upper();\n\
         \x20   counts.insert(key, amount);\n\
         \x20   return counts.len();\n\
         }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let counts: HashMap<string, i64> = HashMap::new();\n\
         \x20   let a = put(counts, \"caller\" + \"-a\", 1);\n\
         \x20   let b = put(counts, \"caller\" + \"-b\", 2);\n\
         \x20   return a + b;\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   return total;\n\
         }}\n\
         \n\
         fn main() -> i64 {{ if run_loop({frames}) > 0 {{ 0 }} else {{ 1 }} }}\n"
    )
}

// ── VecIter string yield binder shares ──────────────────────────────────────

/// The two reported yield-binder shapes: a conditional map ingress before an
/// abandonment point (formerly the `E_NOT_YET_IMPLEMENTED` wall) and a `var`
/// assignment escaping the loop (formerly `ObligationUnderReleased`).
const YIELD_SHARE_SOURCE: &str = "\
fn keep_expensive(totals: HashMap<string, i64>) -> i64 {\n\
\x20   let kept: HashMap<string, i64> = HashMap::new();\n\
\x20   for category in totals.keys() {\n\
\x20   \x20   let cents = totals.get(category).unwrap_or(0);\n\
\x20   \x20   if cents > 500 {\n\
\x20   \x20   \x20   kept.insert(category, cents);\n\
\x20   \x20   }\n\
\x20   \x20   let probe = totals.len();\n\
\x20   \x20   if probe < 0 { return -1; }\n\
\x20   }\n\
\x20   return kept.len();\n\
}\n\
\n\
fn best_category(totals: HashMap<string, i64>) -> string {\n\
\x20   var best_name = \"\";\n\
\x20   var best_cents = -1;\n\
\x20   for category in totals.keys() {\n\
\x20   \x20   let cents = totals.get(category).unwrap_or(0);\n\
\x20   \x20   if cents > best_cents {\n\
\x20   \x20   \x20   best_cents = cents;\n\
\x20   \x20   \x20   best_name = category;\n\
\x20   \x20   }\n\
\x20   }\n\
\x20   return best_name;\n\
}\n\
\n\
fn main() {\n\
\x20   let totals: HashMap<string, i64> = HashMap::new();\n\
\x20   totals.insert(\"groceries\" + \"-heap\", 1200);\n\
\x20   totals.insert(\"transit\" + \"-heap\", 400);\n\
\x20   totals.insert(\"rent\" + \"-heap\", 90000);\n\
\x20   let kept = keep_expensive(totals.clone());\n\
\x20   let best = best_category(totals.clone());\n\
\x20   print(f\"{kept}|{best}|{totals.len()}\");\n\
}\n";

const YIELD_SHARE_EXPECTED: &str = "2|rent-heap|3";

fn yield_share_loop_source(frames: usize) -> String {
    format!(
        "fn keep_expensive(totals: HashMap<string, i64>) -> i64 {{\n\
         \x20   let kept: HashMap<string, i64> = HashMap::new();\n\
         \x20   for category in totals.keys() {{\n\
         \x20   \x20   let cents = totals.get(category).unwrap_or(0);\n\
         \x20   \x20   if cents > 500 {{\n\
         \x20   \x20   \x20   kept.insert(category, cents);\n\
         \x20   \x20   }}\n\
         \x20   }}\n\
         \x20   return kept.len();\n\
         }}\n\
         \n\
         fn best_category(totals: HashMap<string, i64>) -> string {{\n\
         \x20   var best_name = \"\";\n\
         \x20   var best_cents = -1;\n\
         \x20   for category in totals.keys() {{\n\
         \x20   \x20   let cents = totals.get(category).unwrap_or(0);\n\
         \x20   \x20   if cents > best_cents {{\n\
         \x20   \x20   \x20   best_cents = cents;\n\
         \x20   \x20   \x20   best_name = category;\n\
         \x20   \x20   }}\n\
         \x20   }}\n\
         \x20   return best_name;\n\
         }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let totals: HashMap<string, i64> = HashMap::new();\n\
         \x20   totals.insert(\"groceries\" + \"-heap\", 1200);\n\
         \x20   totals.insert(\"transit\" + \"-heap\", 400);\n\
         \x20   totals.insert(\"rent\" + \"-heap\", 90000);\n\
         \x20   let kept = keep_expensive(totals.clone());\n\
         \x20   let best = best_category(totals.clone());\n\
         \x20   return kept + best.len();\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   return total;\n\
         }}\n\
         \n\
         fn main() -> i64 {{ if run_loop({frames}) > 0 {{ 0 }} else {{ 1 }} }}\n"
    )
}

// ── oracles ─────────────────────────────────────────────────────────────────

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrowed_bytes_value_ingress_does_not_double_free() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("hashmap-borrowed-bytes-ingress-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BORROWED_BYTES_INGRESS_SOURCE,
        dir.path(),
        "borrowed_bytes_ingress",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "the bytes value-ingress repro must run clean under the poisoned allocator — a crash \
         means the borrowed `bytes` operand was moved into the map while the caller still held \
         and dropped the same count;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        BORROWED_BYTES_INGRESS_EXPECTED,
        "exact readback required — scribbled or shifted output indicates a use-after-free;\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrowed_bytes_value_ingress_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("borrowed_bytes_ingress", borrowed_bytes_loop_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn reassigned_param_ingress_reads_back_exact_values() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("hashmap-reassigned-param-ingress-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        REASSIGNED_PARAM_INGRESS_SOURCE,
        dir.path(),
        "reassigned_param_ingress",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "the reassigned-parameter repro must run clean under the poisoned allocator — a crash \
         means the fresh generation was still treated as caller-borrowed (or the overwrite \
         path double-released the stored key);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        REASSIGNED_PARAM_INGRESS_EXPECTED,
        "exact readback required — the overwrite path must keep the stored key readable;\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn reassigned_param_ingress_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("reassigned_param_ingress", reassigned_param_loop_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn vec_iter_yield_share_reads_back_exact_values() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("vec-iter-yield-share-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(YIELD_SHARE_SOURCE, dir.path(), "vec_iter_yield_share");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "the yield-share repro must run clean under the poisoned allocator — a crash means a \
         share of the yield binder double-released the binder's count (or the map teardown \
         freed an un-retained key);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        YIELD_SHARE_EXPECTED,
        "exact readback required — the escaped share must stay readable after the loop;\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn vec_iter_yield_share_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("vec_iter_yield_share", yield_share_loop_source);
}
