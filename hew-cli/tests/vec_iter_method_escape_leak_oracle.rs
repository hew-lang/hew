//! `Vec<T>::iter()` cursor drop-safety oracles: the `iter()` cursor must own an
//! INDEPENDENT snapshot of its backing buffer, so it can coexist with — or
//! outlive — the source vec without a double-free or use-after-free.
//!
//! ## The bug this pins
//!
//! `v.iter()` produces a `VecIter<T> { vec, idx }` cursor, the by-reference twin
//! of `v.into_iter()`. The original lowering `Capture`-shared the source's heap
//! handle into the cursor. Hew's `Vec` is a single-owner heap handle with no
//! buffer refcount and Hew has no lifetimes, so a shared handle is unsound in
//! every shape where the source vec stays reachable:
//!   - **double-free** — a named cursor coexisting with a live source
//!     (`let c = v.iter(); v.len()`): the cursor's record-drop frees the handle
//!     AND the source binding frees the same handle at the same scope exit;
//!   - **mutation aliasing** — mutating the source after taking the cursor
//!     (`let c = v.iter(); v.push(x)`): a shared cursor observes the appended
//!     elements (it is a live view, not the snapshot iterator semantics demand);
//!   - **use-after-free** — freeing the source's buffer under a live cursor
//!     (`let c = v.iter(); v = Vec::new()`): the reassignment frees the handle
//!     the cursor still points at, and consuming the cursor reads freed memory.
//!
//! (A cursor that *escapes* its source's scope — returned from a function, held
//! across an `await` — was a pre-fix LEAK rather than a use-after-free: the drop
//! elaborator suppresses the source's scope-exit free when the value escapes, so
//! the borrowed buffer is orphaned rather than dangled. The fix gives the escaped
//! cursor an owned clone instead; the `iter_escape_*` pins below are
//! forward-looking guarantees that the escape paths never become a UAF and read
//! correct values, not fail-pre regression witnesses.)
//!
//! ## The fix
//!
//! `iter()` gives the cursor an INDEPENDENT, deep/retaining clone of the source
//! (`hew_vec_clone`) that it solely owns — for a place receiver the HIR rewrite
//! re-reads the source through a synthesised `recv.clone()`; for an rvalue
//! receiver it consumes the temporary directly, exactly as `into_iter` does. The
//! cursor frees its own clone exactly once on its own drop (registered for
//! scope-exit, async-cancel, and actor shutdown by the same owned-cursor path
//! `into_iter` uses); the source keeps its own independent buffer and its own
//! free. Distinct allocations: no double-free, no aliasing, no dangle.
//!
//! ## Coverage
//!
//! - **Teeth (fail pre-fix, pass post-fix), under the poisoned-allocator triple:**
//!   - `iter_coexist_no_double_free` — a cursor alongside a live source. Pre-fix
//!     the shared handle is freed twice; `MallocGuardEdges` aborts.
//!   - `iter_source_mutation_independence` — mutate the source after taking the
//!     cursor; the cursor must still yield the original 2 elements. Pre-fix the
//!     shared cursor observes 5.
//!   - `iter_source_freed_under_cursor` — free the source's buffer under a live
//!     cursor, then consume it. Pre-fix the cursor reads the freed handle
//!     (segfault / scribbled checksum).
//! - **No per-frame leak (slope), post-fix:** the coexisting cursor frees its
//!   clone every iteration while the source frees its own buffer — a flat leak
//!   slope proves the clone does not accumulate. (Pre-fix the shape double-frees
//!   and the probe skips on the crash; the slope guards the clean post-fix path
//!   against a future leak regression.)
//! - **Forward-looking escape guarantees:** `iter_escape_return` /
//!   `iter_escape_async` exercise the task's named escape contexts (a cursor
//!   returned from a function whose source is a local; a cursor held across an
//!   `await` while its source block exits) and assert they read correct values
//!   with no UAF under the poisoned allocator.
//!
//! The remaining escape context — storing a cursor in ACTOR state and using it
//! after the constructing message returns — is statically impossible:
//! `VecIter<T>` is not `Send`, so it cannot cross a spawn/message/reply boundary,
//! and a `VecIter` actor field has no MIR `RecordLayout` to lower. That rejection
//! is pinned by the `vec_iter_actor_field_non_send` vertical-slice reject fixture
//! rather than a runtime oracle.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! on other platforms it logs `skip:` and returns. The scribble pins run on any
//! unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope fixture (clean post-fix; flat node delta) ────────────────────────

/// Coexistence shape: each iteration builds a fresh `let v`, takes a cursor
/// `let _c = v.iter()` that clones the buffer, reads the live source, and lets
/// BOTH `v` and `_c` drop on the back-edge. The source frees its own buffer and
/// the cursor frees its clone — two distinct allocations, both released every
/// frame. Post-fix the leaked-node count is flat; pre-fix the shared buffer is
/// double-freed (the slope probe skips on the crash — see the scribble pin).
fn coexist_drop_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let v: Vec<i64> = Vec::new();\n\
         \x20       v.push(i);\n\
         \x20       v.push(i * 2);\n\
         \x20       let _c = v.iter();\n\
         \x20       total = total + v.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── scribble-pin fixtures ──────────────────────────────────────────────────

/// Double-free pin (teeth): a cursor and its source are both live locals that
/// drop at the same scope exit. The cursor's clone and the source's buffer are
/// distinct allocations, so neither free collides. Pre-fix the shared buffer is
/// freed twice — `MallocGuardEdges` aborts. `v.len()` = 2.
const COEXIST_DOUBLE_FREE_SOURCE: &str = "\
fn main() {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   let _c = v.iter();\n\
\x20   print(v.len());\n\
\x20   print(\"OK\");\n\
}\n";

/// Mutation-independence pin (teeth): the source is mutated AFTER the cursor is
/// taken. `iter()` snapshots an independent clone, so the cursor must still yield
/// exactly the 2 original elements. A shared cursor observes the 3 appended
/// elements and counts 5. Prints the count: `2`.
const MUTATION_INDEPENDENCE_SOURCE: &str = "\
fn main() {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   let c = v.iter();\n\
\x20   v.push(99);\n\
\x20   v.push(7);\n\
\x20   v.push(13);\n\
\x20   var n: i64 = 0;\n\
\x20   for _x in c {\n\
\x20       n = n + 1;\n\
\x20   }\n\
\x20   print(n);\n\
}\n";

/// Use-after-free pin (teeth): the source `var v` is reassigned (freeing its old
/// buffer) while a cursor over it is live, then the cursor is consumed. Post-fix
/// the cursor owns an independent clone, so the reassignment frees a distinct
/// buffer and the cursor reads live memory. Pre-fix the cursor's handle dangles
/// after the reassignment's free — a scribbled read or a segfault. 40 + 2 = 42.
const SOURCE_FREED_UNDER_CURSOR_SOURCE: &str = "\
fn main() {\n\
\x20   var v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   let c = v.iter();\n\
\x20   v = Vec::new();\n\
\x20   v.push(1);\n\
\x20   var sum: i64 = 0;\n\
\x20   for x in c {\n\
\x20       sum = sum + x;\n\
\x20   }\n\
\x20   print(sum);\n\
\x20   print(\"OK\");\n\
}\n";

/// Escape pin (forward-looking): a cursor is returned from a function whose
/// source vec is a LOCAL, then consumed in the caller. The cursor's clone
/// outlives the callee's scope-exit free. Reads live memory: 40 + 2 = 42.
const ESCAPE_RETURN_SOURCE: &str = "\
fn make_iter() -> VecIter<i64> {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(40);\n\
\x20   v.push(2);\n\
\x20   v.iter()\n\
}\n\
\n\
fn main() {\n\
\x20   var sum: i64 = 0;\n\
\x20   for x in make_iter() {\n\
\x20       sum = sum + x;\n\
\x20   }\n\
\x20   print(sum);\n\
\x20   print(\"OK\");\n\
}\n";

/// Escape pin (forward-looking): a cursor is built in an inner block (whose
/// source vec drops at the block exit), held across an `await`, then consumed.
/// The cursor's clone survives the inner block's free and the suspension.
/// 40 + 2 = 42.
const ESCAPE_ASYNC_SOURCE: &str = "\
actor Echo {\n\
\x20   receive fn ping() -> i64 { 0 }\n\
}\n\
\n\
fn main() {\n\
\x20   let e = spawn Echo();\n\
\x20   let c = {\n\
\x20       let v: Vec<i64> = Vec::new();\n\
\x20       v.push(40);\n\
\x20       v.push(2);\n\
\x20       v.iter()\n\
\x20   };\n\
\x20   let _ = await e.ping();\n\
\x20   var sum: i64 = 0;\n\
\x20   for x in c {\n\
\x20       sum = sum + x;\n\
\x20   }\n\
\x20   print(sum);\n\
\x20   print(\"OK\");\n\
}\n";

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout. A shared/dangling buffer freed twice
/// aborts under `MallocGuardEdges`; a still-read freed buffer is poisoned by
/// `MallocScribble` and drifts the checksum; a live-view cursor counts the
/// appended elements.
fn assert_runs_clean(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-iter-drop-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates the iter() \
         cursor shared or dangled the source's buffer and an independent free corrupted it;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must print `{expected}` untouched — a drifted value indicates the cursor aliased \
         the source's live buffer (observed a mutation) or read a freed/scribbled buffer;\n{}",
        describe_output(&output)
    );
}

// ── oracles ────────────────────────────────────────────────────────────────

/// No-double-free (teeth): a cursor coexisting with its live source frees a
/// distinct clone, never the source's buffer. Must run clean and print `2OK`.
/// Reverting the clone-retain fix double-frees the shared handle and aborts.
#[test]
fn iter_coexist_no_double_free_under_malloc_scribble() {
    assert_runs_clean("coexist_double_free", COEXIST_DOUBLE_FREE_SOURCE, "2OK");
}

/// Mutation-independence (teeth): mutating the source after `iter()` must not
/// change what the cursor yields — the cursor holds an independent clone, not a
/// live view. Must print `2`. Pre-fix the shared cursor observes the appended
/// elements and prints `5`.
#[test]
fn iter_source_mutation_independence_under_malloc_scribble() {
    assert_runs_clean("mutation_independence", MUTATION_INDEPENDENCE_SOURCE, "2");
}

/// No-use-after-free (teeth): freeing the source's buffer (via reassignment)
/// under a live cursor must not dangle the cursor. Must run clean and print
/// `42OK`. Pre-fix the cursor's handle dangles after the reassignment frees it
/// and the consume faults on the scribbled buffer.
#[test]
fn iter_source_freed_under_cursor_no_uaf_under_malloc_scribble() {
    assert_runs_clean(
        "source_freed_under_cursor",
        SOURCE_FREED_UNDER_CURSOR_SOURCE,
        "42OK",
    );
}

/// No per-frame leak (post-fix guard): a coexisting `let _c = v.iter()` cursor
/// frees its clone every frame while the source frees its own buffer — flat leak
/// slope. Reverting the fix makes this shape double-free (the probe skips on the
/// crash); the slope guards the clean path against a future per-frame leak.
#[test]
fn iter_coexist_cursor_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("iter_coexist_drop_loop", coexist_drop_loop_source);
}

/// Escape — return (forward-looking): a cursor returned from a fn whose source
/// is a local and consumed in the caller reads live memory. Must print `42OK`.
#[test]
fn iter_escape_return_no_uaf_under_malloc_scribble() {
    assert_runs_clean("escape_return", ESCAPE_RETURN_SOURCE, "42OK");
}

/// Escape — async (forward-looking): a cursor held across an `await` while its
/// source block exits reads live memory after the suspension. Must print `42OK`.
#[test]
fn iter_escape_async_no_uaf_under_malloc_scribble() {
    assert_runs_clean("escape_async", ESCAPE_ASYNC_SOURCE, "42OK");
}
