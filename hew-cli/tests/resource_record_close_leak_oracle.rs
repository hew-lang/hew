//! Field-bearing `#[resource]` record `close()` oracle (spec §3.7.3 / §10(d)).
//!
//! A `#[resource]` type that owns a heap/aggregate field (a nested record, a
//! `string`, a `Vec`, …) is admitted to the owned-aggregate-record drop set and
//! routes to the recursive `__hew_record_drop_inplace_<R>` thunk. That thunk
//! freed the record's heap leaves but silently SKIPPED the user `close(self)` —
//! losing the RAII contract spec §3.7.3 guarantees ("`close()` runs exactly once
//! per value"). A nested `#[resource]` field's `close()` was skipped the same
//! way, two record layers deep.
//!
//! The fix runs the user `close(self)` as the FIRST step of the drop thunk —
//! before the reverse-order field teardown (spec §10(d): the resource's own
//! close fires, then fields drop in reverse declaration order). Because the
//! thunk is recursive, a nested `#[resource]` field's `close()` fires through
//! its OWN thunk, reached by the field-drop step. close operates on the external
//! resource only; the field-wise teardown still frees each heap leaf exactly
//! once, so the change adds the RAII close without touching heap ownership (no
//! double-free).
//!
//! ## What each oracle pins
//!
//! - **Semantic — close fires (the non-vacuous pre-fix signal).** The primary
//!   defect of this gap is a SILENTLY SKIPPED `close()`, not a byte leak (the
//!   record teardown already freed the heap leaves). So the pre-fix baseline is
//!   the ABSENCE of the close side-effects: pre-fix a field-bearing
//!   `#[resource]` (and its nested `#[resource]` field) print neither
//!   `outer-closed` nor `inner-closed` on scope-exit drop; post-fix BOTH print,
//!   once each, outer before inner (§10(d) order). A regression that drops the
//!   thunk close-call fails the exact-stdout assertion.
//!
//! - **No double-close on an explicit `close()`.** An explicit `o.close()`
//!   consumes the record (the move-checker removes it from the scope-exit drop
//!   set), so `close` fires EXACTLY once — the explicit call — and the thunk
//!   close does NOT also run. A regression that ran the thunk close on a
//!   consumed value would print the close side-effect twice.
//!
//! - **No double-free under the poisoned-allocator triple (any unix).** A
//!   field-bearing `#[resource]` whose heap field is solely record-owned: the
//!   thunk runs `close(self)` then frees the field exactly once. If close and
//!   the teardown each freed the field this aborts under
//!   `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`. The loop body
//!   amplifies any per-iteration double-free.
//!
//! - **Per-iteration leak slope (macOS via `leaks(1)`).** A field-bearing
//!   `#[resource]` owning a real `Vec<i64>` field freed via `Vec::new()` + `push`
//!   (provably a heap buffer, not a folded literal), built fresh every iteration,
//!   holds the leak-node count flat across a LOW and a HIGH iteration count. The
//!   delta cancels the nondeterministic constant baseline a single-shot `== 0`
//!   count cannot; a thunk that skipped the field teardown would leak the `Vec`
//!   buffer every iteration (positive slope). The same looped source drives the
//!   double-free scribble pin, so both the leak and the double-free directions
//!   share one provably-heap fixture.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Nested field-bearing resource: `Outer { inner: Inner; tag: i64 }` where both
/// `Outer` and `Inner` are `#[resource]`. On scope-exit drop the thunk runs
/// `Outer::close` then field-teardown recurses into `Inner`'s thunk, which runs
/// `Inner::close`. Pre-fix: only `before-scope-exit` printed. Post-fix: outer
/// then inner close fire (§10(d) order).
const NESTED_CLOSE_SOURCE: &str = "\
#[resource] type Inner { fd: i64; }\n\
impl Inner { fn close(self) { println(\"inner-closed\"); } }\n\
#[resource] type Outer { inner: Inner; tag: i64; }\n\
impl Outer { fn close(self) { println(\"outer-closed\"); } }\n\
fn make_inner() -> Inner { Inner { fd: 3 } }\n\
fn main() {\n\
\x20   let o = Outer { inner: make_inner(), tag: 7 };\n\
\x20   println(\"before-scope-exit\");\n\
}\n";

/// Expected post-fix output: the marker line, then outer close, then nested
/// inner close — close runs before field teardown, and the nested field's close
/// runs as part of that teardown.
const NESTED_CLOSE_EXPECTED: &str = "before-scope-exit\nouter-closed\ninner-closed\n";

/// Explicit-consume control: `o.close()` consumes `o`, so the scope-exit thunk
/// drop is suppressed (no double-close). `outer-closed` prints EXACTLY once.
/// `inner` is owned by the consumed value; the user `close(self)` chose not to
/// close it, so `inner-closed` does NOT print (no thunk teardown on a consumed
/// value) — leaking `inner`'s (here scalar) storage is acceptable; a
/// double-close / double-free is not.
const EXPLICIT_CONSUME_SOURCE: &str = "\
#[resource] type Inner { fd: i64; }\n\
impl Inner { fn close(self) { println(\"inner-closed\"); } }\n\
#[resource] type Outer { inner: Inner; tag: i64; }\n\
impl Outer { fn close(self) { println(\"outer-closed\"); } }\n\
fn make_inner() -> Inner { Inner { fd: 3 } }\n\
fn main() {\n\
\x20   let o = Outer { inner: make_inner(), tag: 7 };\n\
\x20   o.close();\n\
\x20   println(\"after-explicit\");\n\
}\n";

/// Expected: exactly one `outer-closed` (the explicit call), then the marker.
/// A second `outer-closed` would mean the thunk close ran on a consumed value.
const EXPLICIT_CONSUME_EXPECTED: &str = "outer-closed\nafter-explicit\n";

/// Heap-field resource loop, the shared source for both the double-free scribble
/// pin and the per-iteration leak slope. `Box { payload: Vec<i64>; fd: i64 }` is
/// `#[resource]`; the `Vec` is built with `Vec::new()` + `push` (provably a heap
/// buffer) and moved into the record (solely record-owned). The thunk runs the
/// empty `Box::close` then frees the `Vec` exactly once. `build(i)` returns its
/// `fd`, and `main` self-checks the running total (`sum 0..frames`) and returns 0
/// so the scribble pin's `success()` assertion holds; the slope harness ignores
/// the exit status and measures leak nodes. If `close` also freed the `Vec` the
/// loop aborts under the poisoned allocator; if the teardown skipped it the leak
/// count grows with the iteration count.
fn heap_field_loop_source(frames: usize) -> String {
    let expected_total = frames * frames.saturating_sub(1) / 2;
    format!(
        "#[resource] type Box {{ payload: Vec<i64>; fd: i64; }}\n\
         impl Box {{ fn close(self) {{ }} }}\n\
         fn build(n: i64) -> i64 {{\n\
         \x20   let v: Vec<i64> = Vec::new();\n\
         \x20   v.push(n);\n\
         \x20   v.push(n + 1);\n\
         \x20   let b = Box {{ payload: v, fd: n }};\n\
         \x20   b.fd\n\
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

// ── correctness pins ───────────────────────────────────────────────────────

/// Compile `source`, run it (no allocator poisoning), assert clean exit + exact
/// stdout — the close-fires / no-double-close semantic pin.
fn assert_exact_stdout(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("resource-record-close-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let output = std::process::Command::new(&bin)
        .output()
        .unwrap_or_else(|error| panic!("run {name} binary: {error}"));

    assert!(
        output.status.success(),
        "{name} must run clean;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name}: a field-bearing `#[resource]` record's user `close(self)` must fire \
         on scope-exit drop, before field teardown (spec §3.7.3 / §10(d)), and a nested \
         `#[resource]` field's `close()` must fire through the recursive drop thunk. \
         Wrong output means the thunk skipped a close (pre-fix gap) OR ran one twice \
         (double-close);\n{}",
        describe_output(&output)
    );
}

/// Compile + run `source` under the poisoned-allocator triple, assert clean exit
/// (the double-free pin) and the i64 exit status the fixture computes.
fn assert_no_double_free_under_malloc_scribble(name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("resource-record-close-df-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort here is a \
         double-free of a `#[resource]` record's heap field (the user `close(self)` and \
         the field-wise teardown both released the same buffer), or a non-zero exit is \
         the fixture's own sum/teardown check failing;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Nested field-bearing resource: outer close fires, then nested inner close,
/// on scope-exit drop. Pre-fix neither printed (silently-skipped close).
#[test]
fn resource_record_nested_close_fires_on_drop() {
    assert_exact_stdout("nested_close", NESTED_CLOSE_SOURCE, NESTED_CLOSE_EXPECTED);
}

/// Explicit `o.close()` consumes the record: close fires exactly once (the
/// explicit call), the scope-exit thunk close is suppressed — no double-close.
#[test]
fn resource_record_explicit_close_single_no_double() {
    assert_exact_stdout(
        "explicit_consume",
        EXPLICIT_CONSUME_SOURCE,
        EXPLICIT_CONSUME_EXPECTED,
    );
}

/// Double-free pin: the field-bearing `#[resource]` loop (200 iterations) runs
/// clean under the poisoned allocator — the thunk frees the `Vec<i64>` field
/// exactly once after the user close. A per-iteration double-free aborts.
#[test]
fn resource_record_heap_field_no_double_free_under_malloc_scribble() {
    assert_no_double_free_under_malloc_scribble("heap_field", &heap_field_loop_source(200));
}

/// Slope oracle: the field-bearing `#[resource]` loop holds the leak-node count
/// flat across LOW vs HIGH iteration counts — the drop thunk frees the `Vec<i64>`
/// field every iteration after running the user `close`. A skipped teardown leaks
/// the buffer per iteration (positive slope).
#[test]
fn resource_record_heap_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("resource_record_heap_field", heap_field_loop_source);
}
