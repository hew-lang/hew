//! Owned-element `Vec<record/enum-with-collection-field>` COPY-IN push leak
//! oracle.
//!
//! The array-literal sibling (`vec_record_collection_field_leak_oracle`) pins
//! the MOVE path (`hew_vec_push_owned_move`). This oracle pins the COPY-IN
//! `.push` path (`hew_vec_push_owned`): pushing a record/enum whose field is a
//! collection deep-clones the element into the Vec, so the source retains its
//! own owner. The clone thunk must recurse through the collection field via the
//! owned-collection ABI (`hew_vec_clone_owned` / `hew_vec_free_owned`); a
//! shallow copy aliases the source's `payload` buffer (double-free under the
//! poisoned allocator), and a missing per-element drop shows a per-frame leak
//! slope.
//!
//! ## What each oracle pins
//!
//! - **Push-clone exact contents under the poisoned-allocator triple** (any
//!   unix): the SAME source `Boxed { payload: [..] }` is pushed twice and the
//!   source is read back afterward. Three independent owners (two Vec elements
//!   plus the still-live source) of three distinct `payload` buffers. A shallow
//!   clone aliases them → double/triple free under `MallocScribble`; the exact
//!   read-back of all three plus the clean exit pins both the deep-clone and the
//!   exactly-once drop.
//! - **Self-recursive enum push** (`enum Reply { Arr([Reply]) }`): pushing the
//!   nested-vec variant exercises the recursive enum clone/drop thunk through
//!   the inner `Vec<Reply>`. Clean exit = no double-free of the nested buffer.
//! - **Per-frame leak slope** (macOS-only via `leaks(1)`): build-push-drop a
//!   `Vec<Boxed>` / `Vec<Reply>` per frame. Post-fix the leak-node count is flat
//!   across frame counts; a regression that drops an element shallowly (or
//!   fails to clone the source in) shows a per-frame slope.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// The SAME `Boxed { payload: [i64] }` source pushed twice (copy-in clone) and
/// then read back from the source itself. Three owners of three distinct
/// `payload` buffers: `xs[0]`, `xs[1]`, and the live `src`. A shallow clone
/// aliases them and a garbage/differing cell (or a poisoned-allocator abort)
/// exposes it.
const PUSH_CLONE_RECORD_SOURCE: &str = "\
type Boxed {\n\
\x20   payload: [i64];\n\
}\n\
\n\
fn main() {\n\
\x20   var xs: [Boxed] = [];\n\
\x20   let src = Boxed { payload: [10, 20] };\n\
\x20   xs.push(src);\n\
\x20   xs.push(src);\n\
\x20   let a = xs[0];\n\
\x20   let b = xs[1];\n\
\x20   let s0 = src.payload[0];\n\
\x20   let s1 = src.payload[1];\n\
\x20   print(f\"{a.payload[0]}|{a.payload[1]}|{b.payload[0]}|{b.payload[1]}|{s0}|{s1}\");\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact output for `PUSH_CLONE_RECORD_SOURCE`. Both Vec elements are
/// clones of the same source (`[10, 20]`), and the source is read back too.
const PUSH_CLONE_RECORD_EXPECTED: &str = "10|20|10|20|10|20OK";

/// A self-recursive enum (`Array([Reply])` — the Redis/JSON reply shape) pushed
/// into a `Vec<Reply>`. The nested-vec variant forces the recursive enum
/// clone/drop thunk to recurse through the inner `Vec<Reply>` via the
/// owned-collection ABI. Reads back the Vec length so a garbage tag changes the
/// output.
const PUSH_RECURSIVE_ENUM_SOURCE: &str = "\
enum Reply {\n\
\x20   Nil;\n\
\x20   Num(i64);\n\
\x20   Arr([Reply]);\n\
}\n\
\n\
fn main() {\n\
\x20   var xs: [Reply] = [];\n\
\x20   xs.push(Reply::Num(7));\n\
\x20   xs.push(Reply::Arr([Reply::Nil, Reply::Num(9)]));\n\
\x20   let n = xs.len();\n\
\x20   print(f\"{n}\");\n\
\x20   print(\"OK\");\n\
}\n";

/// Expected exact output for `PUSH_RECURSIVE_ENUM_SOURCE`.
const PUSH_RECURSIVE_ENUM_EXPECTED: &str = "2OK";

/// Per-frame build-push-drop shape for the record leak slope: each iteration
/// builds a fresh source, pushes two clones into a fresh `Vec<Boxed>`, reads it,
/// and lets both source and Vec drop on the back-edge. Post-fix the outer drop
/// runs each element's record drop thunk (freeing its `payload` buffer) and the
/// source drops its own, so the leak count is flat across frame counts.
fn push_clone_record_drop_loop_source(frames: usize) -> String {
    format!(
        "type Boxed {{\n\
         \x20   payload: [i64];\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       var xs: [Boxed] = [];\n\
         \x20       let src = Boxed {{ payload: [10, 20] }};\n\
         \x20       xs.push(src);\n\
         \x20       xs.push(src);\n\
         \x20       let a = xs[0];\n\
         \x20       total = total + a.payload[0] + xs.len() + src.payload[1];\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Per-frame build-push-drop shape for the self-recursive enum leak slope.
fn push_recursive_enum_drop_loop_source(frames: usize) -> String {
    format!(
        "enum Reply {{\n\
         \x20   Nil;\n\
         \x20   Num(i64);\n\
         \x20   Arr([Reply]);\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       var xs: [Reply] = [];\n\
         \x20       xs.push(Reply::Num(7));\n\
         \x20       xs.push(Reply::Arr([Reply::Nil, Reply::Num(9)]));\n\
         \x20       total = total + xs.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-record-push-clone-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free from a shallow copy-in clone of an owned-element collection field;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back every owner's payload cells verbatim — a differing/garbage cell \
         indicates the pushed element or the live source was shallow-copied or dropped early;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: pushing the SAME `Boxed { payload: [i64] }` twice and
/// reading back both Vec elements AND the still-live source must print every
/// payload cell verbatim and exit clean under the poisoned allocator. Routing
/// the copy-in push to a shallow byte-copy (aliasing the source buffer) or
/// dropping an element shallowly fails this.
#[test]
fn push_clone_record_collection_field_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "push_clone_record",
        PUSH_CLONE_RECORD_SOURCE,
        PUSH_CLONE_RECORD_EXPECTED,
    );
}

/// Exact-contents pin for the self-recursive enum push: the recursive enum
/// clone/drop thunk must recurse through the inner `Vec<Reply>` without
/// double-freeing the nested buffer.
#[test]
fn push_recursive_enum_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "push_recursive_enum",
        PUSH_RECURSIVE_ENUM_SOURCE,
        PUSH_RECURSIVE_ENUM_EXPECTED,
    );
}

/// Forward leak guard: building-pushing-dropping a `Vec<Boxed>` per frame with
/// a live source must not grow the leak-node count with the frame count. A
/// regression that fails to clone the source in (or drops an element shallowly)
/// shows a per-frame slope this catches.
#[test]
fn push_clone_record_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "push_clone_record_drop_loop",
        push_clone_record_drop_loop_source,
    );
}

/// Forward leak guard for the self-recursive enum push path.
#[test]
fn push_recursive_enum_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "push_recursive_enum_drop_loop",
        push_recursive_enum_drop_loop_source,
    );
}
