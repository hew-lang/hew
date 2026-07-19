//! Actor mailbox record-with-collection-field drop canary (hew-lang/hew#2722).
//!
//! ## The shape
//!
//! A record whose field is a collection (`type Boxed { payload: [i64] }`) is sent
//! to an actor by value; the receive handler reads the collection field:
//!
//! ```text
//! actor Sink { receive fn take(b: Boxed) { for v in b.payload { … } } }
//! sink.take(Boxed { payload: [i, i + 1, i + 2] });
//! ```
//!
//! #2722 reported this leaking the record's collection field ~2 nodes/message:
//! the mailbox delivery path did not recurse through the collection field.
//!
//! ## Status on the Stage-1 tip (`882ff31d9`)
//!
//! The collection-field delivery path is CLEAN on `882ff31d9` — the by-value
//! receive-handler param's recursive teardown frees the record and its
//! collection field(s) once per delivered message. This is a CANARY that locks
//! that behaviour: the plain single-collection-field record and a two-collection
//! -field record both hold a flat per-message leak slope.
//!
//! Out of scope (a DISTINCT root, not collection-field recursion): a
//! mailbox-delivered record carrying a `string` field still leaks its string
//! header — the `alloc_cstring_data` actor-mailbox class of #2717, tracked
//! separately. This oracle deliberately uses collection-only records so it pins
//! exactly the #2722 collection-recursion behaviour.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// PRIMARY #2722 shape: a `Boxed { payload: [i64] }` sent per message; the
/// handler iterates `b.payload`. A final `await` synchronises so every message
/// is delivered and dropped before the `leaks` snapshot.
fn mailbox_record_collection_loop_source(frames: usize) -> String {
    format!(
        "type Boxed {{ payload: [i64] }}\n\
         \n\
         actor Sink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(b: Boxed) {{\n\
         \x20       var s: i64 = 0;\n\
         \x20       for v in b.payload {{ s = s + v; }}\n\
         \x20       seen = seen + s;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn Sink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Boxed {{ payload: [i, i + 1, i + 2] }};\n\
         \x20       sink.take(b);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.total() {{\n\
         \x20       Ok(v) => {{ if v < 0 {{ 74 }} else {{ 0 }} }},\n\
         \x20       Err(_e) => 1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Two-collection-field record: both `payload` and `extra` must be freed per
/// delivered message.
fn mailbox_record_two_collections_loop_source(frames: usize) -> String {
    format!(
        "type Boxed {{ payload: [i64]; extra: [i64] }}\n\
         \n\
         actor Sink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(b: Boxed) {{\n\
         \x20       var s: i64 = 0;\n\
         \x20       for v in b.payload {{ s = s + v; }}\n\
         \x20       for v in b.extra {{ s = s + v; }}\n\
         \x20       seen = seen + s;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn Sink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Boxed {{ payload: [i, i + 1, i + 2], extra: [i * 2, i * 3] }};\n\
         \x20       sink.take(b);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.total() {{\n\
         \x20       Ok(v) => {{ if v < 0 {{ 75 }} else {{ 0 }} }},\n\
         \x20       Err(_e) => 1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// #2747 BORROW-only handler, `[i64]` field: the handler reads
/// `b.payload.len()` and discards the record — no `for` drain, no move into
/// state. The mailbox hand-off consumes the caller's original, so the delivered
/// record is the handler frame's sole property; its collection field buffer must
/// be freed at handler-scope exit. Pre-fix this leaked 2 nodes / message (§0:
/// 40 leaks / 2880 B over 20 sends) because the record was never registered as
/// owned by the handler. The `[i64]` slice spelling.
fn mailbox_record_borrow_only_slice_source(frames: usize) -> String {
    format!(
        "type Boxed {{ payload: [i64] }}\n\
         \n\
         actor Sink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(b: Boxed) {{\n\
         \x20       seen = seen + b.payload.len();\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn Sink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Boxed {{ payload: [i, i + 1, i + 2] }};\n\
         \x20       sink.take(b);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.total() {{\n\
         \x20       Ok(v) => {{ if v < 0 {{ 74 }} else {{ 0 }} }},\n\
         \x20       Err(_e) => 1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// #2747 BORROW-only handler, `Vec<i64>` field: identical body to the slice
/// arm, only the field spelling differs. §0 proved the two `.ll` dumps are
/// byte-identical except the module name — this arm locks the fix as
/// spelling-agnostic (both leaked 40 / 2880 B pre-fix).
fn mailbox_record_borrow_only_vec_source(frames: usize) -> String {
    format!(
        "type Boxed {{ payload: Vec<i64> }}\n\
         \n\
         actor Sink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(b: Boxed) {{\n\
         \x20       seen = seen + b.payload.len();\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn Sink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Boxed {{ payload: [i, i + 1, i + 2] }};\n\
         \x20       sink.take(b);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.total() {{\n\
         \x20       Ok(v) => {{ if v < 0 {{ 74 }} else {{ 0 }} }},\n\
         \x20       Err(_e) => 1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// #2747 double-free BOUNDARY: the handler MOVES the delivered record's owned
/// field INTO actor state (`store = b.payload`). The field escapes; the
/// synthesised `state_drop_fn` is that buffer's single free. The escape-scan
/// must SUPPRESS the handler's `RecordInPlace` drop for the retained record —
/// admitting it would free the same buffer twice. Each frame's store overwrite
/// releases the previous handle, so the slope stays flat while exactly one live
/// buffer survives into the next frame. This is the boundary the fix must NOT
/// cross: single-dropped, no double-free.
fn mailbox_record_retained_into_state_source(frames: usize) -> String {
    format!(
        "type Boxed {{ payload: Vec<i64> }}\n\
         \n\
         actor Sink {{\n\
         \x20   var seen: i64;\n\
         \x20   var store: Vec<i64>;\n\
         \x20   receive fn take(b: Boxed) {{\n\
         \x20       store = b.payload;\n\
         \x20       seen = seen + store.len();\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn Sink(seen: 0, store: []);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = Boxed {{ payload: [i, i + 1, i + 2] }};\n\
         \x20       sink.take(b);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.total() {{\n\
         \x20       Ok(v) => {{ if v < 0 {{ 74 }} else {{ 0 }} }},\n\
         \x20       Err(_e) => 1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// #2747 retained-into-state EXACT-CONTENT pin. Seven sends store successive
/// `[i*10, i, i]` buffers into actor state; the last retained buffer is
/// `[60, 6, 6]`. `peek()` reads `store[0]` AFTER every send, proving the final
/// retained buffer survived intact — a handler double-free (freeing the buffer
/// the state field re-owns) turns this read into a scribbled-memory access that
/// crashes or returns a poisoned value under the allocator triple. `main`
/// returns the exact `60`.
const MAILBOX_RETAINED_CONTENT_SOURCE: &str = "type Boxed { payload: Vec<i64> }\n\
     \n\
     actor Sink {\n\
     \x20   var store: Vec<i64>;\n\
     \x20   receive fn take(b: Boxed) {\n\
     \x20       store = b.payload;\n\
     \x20   }\n\
     \x20   receive fn peek() -> i64 { store[0] }\n\
     }\n\
     \n\
     fn main() -> i64 {\n\
     \x20   let sink = spawn Sink(store: []);\n\
     \x20   var i: i64 = 0;\n\
     \x20   while i < 7 {\n\
     \x20       let b = Boxed { payload: [i * 10, i, i] };\n\
     \x20       sink.take(b);\n\
     \x20       i = i + 1;\n\
     \x20   }\n\
     \x20   match await sink.peek() {\n\
     \x20       Ok(v) => v,\n\
     \x20       Err(_e) => 99,\n\
     \x20   }\n\
     }\n";

#[test]
fn mailbox_record_collection_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "mailbox_record_collection",
        mailbox_record_collection_loop_source,
    );
}

#[test]
fn mailbox_record_two_collections_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "mailbox_record_two_collections",
        mailbox_record_two_collections_loop_source,
    );
}

/// #2747 — the BORROW-only slice handler holds a flat slope post-fix (leaked
/// pre-fix). The primary regression this fix closes.
#[test]
fn mailbox_record_borrow_only_slice_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "mailbox_record_borrow_only_slice",
        mailbox_record_borrow_only_slice_source,
    );
}

/// #2747 — the BORROW-only `Vec<i64>` handler holds a flat slope post-fix,
/// proving the fix is spelling-agnostic (both `[i64]` and `Vec<i64>` leaked
/// identically pre-fix).
#[test]
fn mailbox_record_borrow_only_vec_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "mailbox_record_borrow_only_vec",
        mailbox_record_borrow_only_vec_source,
    );
}

/// #2747 boundary — the retained-into-state handler must NOT gain a per-frame
/// leak (the state overwrite releases the previous buffer) AND must not
/// double-free (over-admission crashes under the poisoned allocator, spiking
/// the slope or aborting the run).
#[test]
fn mailbox_record_retained_into_state_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "mailbox_record_retained_into_state",
        mailbox_record_retained_into_state_source,
    );
}

/// #2747 boundary — exact-content / no-double-free pin for the retained-into-
/// state record. Runs under the poisoned-allocator triple and asserts the exact
/// `store[0]` of the last retained buffer (`60`) survives every send. If the
/// handler double-freed the retained buffer, the trailing state read would trap
/// or return a scribbled value.
#[test]
fn mailbox_record_retained_into_state_keeps_exact_content() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("mailbox-retained-content-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        MAILBOX_RETAINED_CONTENT_SOURCE,
        dir.path(),
        "mailbox_retained_content",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(60),
        "retained-into-state record must be freed exactly once: the final \
         store[0] (60) must survive under the poisoned allocator — a handler \
         double-free of the retained buffer scribbles or aborts the state read. \
         Output:\n{}",
        describe_output(&output)
    );
}
