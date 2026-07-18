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

use support::leak_slope::assert_frame_slope_below_tolerance;

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
