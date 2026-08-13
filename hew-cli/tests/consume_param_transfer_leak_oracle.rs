//! Consumed-param transfer oracle (#2549): a `#[resource]` child moved into a
//! container via a `consume`-parameter builder must be freed EXACTLY ONCE.
//!
//! The json/toml/yaml `Value` builders take the child by `consume` and pass the
//! opaque resource directly to a native builder whose FFI parameter also takes
//! ownership: `json.object().with("items", child)` /
//! `json.array().push(child)`. The caller's moved binding therefore has no
//! scope-exit close; its allocation is freed once by the container that now owns
//! it, reached when the enclosing root is closed. If the moved binding also
//! closed the child, it would be freed twice: once at the old caller and once by
//! the container.
//!
//! ## What each oracle pins
//!
//! - **No double-free under the poisoned-allocator triple.** A loop that builds
//!   a json object holding a transferred array of transferred int children, then
//!   closes the root, runs clean under `MallocScribble`/`MallocPreScribble`/
//!   `MallocGuardEdges`. A per-iteration double-free of a transferred child
//!   aborts; the loop amplifies it.
//!
//! - **Per-iteration leak slope (macOS `leaks(1)`).** The same fresh-every-
//!   iteration build holds the leak-node count flat across a LOW and a HIGH
//!   iteration count. A regression that suppressed the child's close but never
//!   ran the transfer (leaking the child) — or double-closed the root — shows a
//!   non-zero slope. The delta cancels the nondeterministic constant baseline a
//!   single-shot `== 0` cannot.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// Transfer-builder loop over the real json native handles. Each iteration:
/// builds a two-element array by `push`-ing two `from_int` children (each child
/// consumed, its handle transferred to the array), stores the array into a fresh
/// object with `with` (the array consumed, transferred to the object), reads a
/// scalar field back, frees the read-back field through the compatibility alias,
/// then closes the object root through the canonical disposer
/// (which recursively frees the transferred array + its int children exactly
/// once). `build(i)` returns the read-back tag so `main` self-checks the running
/// total and returns 0 for the scribble pin's `success()` assertion.
fn transfer_loop_source(frames: usize) -> String {
    let expected_total = frames * frames.saturating_sub(1) / 2;
    format!(
        "import std::encoding::json;\n\
         fn build(n: i64) -> i64 {{\n\
         \x20   let arr = json.array().push(json.from_int(n)).push(json.from_int(n + 1));\n\
         \x20   let obj = json.object().with(\"items\", arr).with_int(\"tag\", n);\n\
         \x20   let field = obj.get_field(\"tag\");\n\
         \x20   let got = field.get_int();\n\
         \x20   field.free();\n\
         \x20   obj.close();\n\
         \x20   got\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + build(i); }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 70; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Double-free pin: the transfer-builder loop (200 iterations) runs clean under
/// the poisoned allocator. Each transferred child is freed exactly once — by the
/// container that took ownership — never also by the moved-out caller binding.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn consume_param_transfer_no_double_free_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("consume-param-transfer-df-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&transfer_loop_source(200), dir.path(), "transfer_df");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "transfer builder must run clean under the poisoned allocator — an abort \
         here is a double-free of a `consume`-transferred child (the old owner closed \
         it AND the container that took ownership freed it), or a non-zero exit is \
         the fixture's own running-total check failing;\n{}",
        describe_output(&output)
    );
}

/// Slope oracle: the transfer-builder loop holds the leak-node count flat across
/// LOW vs HIGH iteration counts. A transferred child (and the object root) is
/// freed exactly once per iteration; a leaked child or a leaked root shows a
/// positive slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn consume_param_transfer_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("consume_param_transfer", transfer_loop_source);
}
