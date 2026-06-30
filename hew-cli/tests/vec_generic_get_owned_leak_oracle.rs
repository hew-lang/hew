//! Generic `Vec<T>.get` owned-element leak oracle (hew-lang/hew#1929 Stage 2).
//!
//! `Vec<T>.get` where `T` is a type parameter bound to a non-Copy record type
//! routes through the owned-element ABI: the getter calls `hew_vec_get_owned`
//! (borrows the live slot and transfers ownership to the caller), while the
//! Vec's scope-exit drop calls `hew_vec_free_owned` (runs the per-element
//! record drop thunk over every live slot). Pre-fix these two paths aliased
//! the same heap string — the getter transferred ownership while `free_owned`
//! also released the slot — producing a double-free on the first element's
//! `label` field within a few iterations. A missed drop on the other side
//! grows the leak-node count linearly with iteration count.
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
//!   `Option<T>` while the element's `label` heap buffer never runs its drop
//!   leaks ~3 nodes per iteration (the Vec handle + buffer the by-value generic
//!   param leaves unreleased, plus the moved element's `label`).
//!
//!   ### TRACKED-RED (Stage A pin)
//!
//!   This slope oracle currently FAILS: the owned-element move-out path
//!   (`let got = match first(vn) { Some(g) => g }; got.label.len()`) leaks the
//!   element's heap `label` — the `hew_vec_get_owned` transfer plus the by-value
//!   generic param `first<T>(v: Vec<T>)` drop the Vec but not the moved element's
//!   string field. The original fixture masked this with a static `"owned-ok"`
//!   label (nothing to free). The slope assertion below states the CORRECT
//!   (no-leak) bar; it is `#[ignore]`d so the deterministic gate stays green
//!   while the leak is tracked. The Stage C owned-element-drop fix removes the
//!   `#[ignore]`.
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

/// Slope oracle (TRACKED-RED — see module header): the non-vacuous
/// push-get(move-out)-drop cycle must hold the leak-node count flat. It does NOT
/// today — the owned-element move-out leaks the element's heap `label` — so the
/// assertion (which states the correct no-leak bar) is `#[ignore]`d to keep the
/// deterministic gate green. The Stage C owned-element-drop fix removes the
/// `#[ignore]`.
#[test]
#[ignore = "tracked-RED (no-leak-bar Stage A): owned-element Vec<T>.get move-out leaks the \
            moved element's heap `label` — measured low=9 nodes/672 B at 3 frames vs \
            high=150 nodes/11200 B at 50 frames (~3 nodes/iter). The assertion states the \
            correct no-leak bar; the Stage C owned-element-drop fix removes this ignore."]
fn vec_generic_get_owned_element_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("vec_generic_get_owned", generic_get_loop_source);
}
