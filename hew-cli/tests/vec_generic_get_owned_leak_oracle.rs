//! Generic `Vec<T>.get` owned-element leak oracle (hew-lang/hew#1929 Stage 2).
//!
//! `Vec<T>.get` where `T` is a type parameter bound to a non-Copy record routes
//! through `hew_vec_get_clone`, which writes a fresh owner into `Option<T>`.
//! The caller still owns the source Vec and must run `hew_vec_free_owned` at
//! scope exit. Two ownership holes combined in the tracked leak:
//! 1. the collection escape scan treated `first(vn)` as a handoff even though
//!    `first` only borrowed its parameter, suppressing the caller's Vec drop;
//! 2. `vn.push(Name { ... })` used copy-in for an anonymous fresh record, leaving
//!    the source temporary's retained `label` owner with no balancing drop.
//!
//! The fix proves borrow-only direct-call parameters and moves fresh
//! materialised owned push rvalues into the Vec.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): push an
//!   owned `Name { label: "owned-ok" }`, get it back through a generic helper,
//!   read the field, and drop. Under `MallocScribble`/`MallocPreScribble`/
//!   `MallocGuardEdges` a double-free aborts the process; a use-after-free read
//!   returns scribbled memory, changing the output. The string equality plus the
//!   clean exit pin both regression directions.
//!
//! - **Per-iteration leak slope** (macOS-only via `leaks(1)`): the
//!   push-get(move-out)-drop cycle, with a NON-VACUOUS heap `label`
//!   (`"owned-ok".to_upper()` — a static literal needs no free and would mask the
//!   leak), looped at a LOW and a HIGH iteration count. The leak-node delta must
//!   stay within tolerance. A getter that moves the owned element out of its
//!   `Option<T>` while the caller's source Vec drop is suppressed and the fresh
//!   push source is stranded leaks ~3 nodes per iteration: the Vec handle,
//!   backing buffer, and element label. The parameter-body proof must keep the
//!   caller Vec admitted while still excluding helpers that return/store the
//!   parameter, and fresh owned push rvalues must transfer rather than clone.
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

/// Single round-trip under the poisoned allocator: push one `Name { label:
/// "owned-ok" }`, call `first<T>` (the generic returning helper), read
/// `got.label`, and let both the returned record and the Vec drop at scope
/// exit. Under `MallocScribble` a double-free aborts; a UAF read on `got.label`
/// returns scribbled memory (not `"owned-ok"`). The exact-string equality plus
/// clean exit pin both directions simultaneously.
const GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn first<T>(v: Vec<T>) -> Option<T> {\n\
\x20   v.get(0)\n\
}\n\
\n\
fn main() {\n\
\x20   let vn: Vec<Name> = Vec::new();\n\
\x20   vn.push(Name { label: \"owned-ok\" });\n\
\x20   let got = match first(vn) {\n\
\x20       Some(g) => g,\n\
\x20       None => { print(\"none\"); return; }\n\
\x20   };\n\
\x20   print(got.label);\n\
}\n";

/// Expected exact output for `GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE`. Any
/// aliasing or UAF changes the `got.label` read.
const GENERIC_GET_SINGLE_ROUNDTRIP_EXPECTED: &str = "owned-ok";

/// Looped push-get(move-out)-drop cycle for the per-iteration slope probe. Each
/// cycle builds a FRESH single-element `Vec<Name>` with a HEAP `label`
/// (`"owned-ok".to_upper()` — non-vacuous), moves the element out of the
/// generic getter's `Option<Name>` (`Some(g) => g`), and returns `got.label.len()`
/// so the moved element cannot be elided. A getter that transfers the owned
/// element without running its `label` drop on the moved-out value (or a by-value
/// generic param that leaves the Vec unreleased) leaks per iteration.
fn generic_get_loop_source(frames: usize) -> String {
    format!(
        "type Name {{ label: string; }}\n\
         \n\
         fn first<T>(v: Vec<T>) -> Option<T> {{\n\
         \x20   v.get(0)\n\
         }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let vn: Vec<Name> = Vec::new();\n\
         \x20   vn.push(Name {{ label: \"owned-ok\".to_upper() }});\n\
         \x20   let got = match first(vn) {{\n\
         \x20       Some(g) => g,\n\
         \x20       None => {{ return 0; }}\n\
         \x20   }};\n\
         \x20   got.label.len()\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ run_loop({frames}) }}\n"
    )
}

// ── scribble correctness pin ──────────────────────────────────────────────

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-generic-get-owned-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free of the owned `Name.label` string (the generic getter and \
         `hew_vec_free_owned` both released the same slot);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back the label verbatim — scribbled/empty output indicates a \
         use-after-free read on `got.label` (the slot was freed before the caller read it);\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: a single push-get-read-drop round-trip through a
/// generic `first<T>` helper must print `"owned-ok"` and exit clean under the
/// poisoned allocator. Reverting the owned-getter routing (so the getter aliases
/// the Vec's slot while `hew_vec_free_owned` also releases it) fails this with
/// either an abort (double-free) or garbled output (UAF read).
#[test]
fn vec_generic_get_owned_element_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_generic_get_single_roundtrip",
        GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE,
        GENERIC_GET_SINGLE_ROUNDTRIP_EXPECTED,
    );
}

/// Slope oracle: the non-vacuous push-get(move-out)-drop cycle must hold the
/// leak-node count flat.
#[test]
fn vec_generic_get_owned_element_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("vec_generic_get_owned", generic_get_loop_source);
}
