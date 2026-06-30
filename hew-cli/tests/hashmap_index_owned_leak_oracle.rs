//! `HashMap` `m[k]` owned-value leak oracle (indexed-accessor convergence).
//!
//! `m[k]` over a `HashMap<K, V>` is the trapping `Index::at` accessor: on a hit
//! the runtime semantic-clones the stored value into the caller's slot through
//! `hew_hashmap_get_clone_layout`, producing a FRESH, independently-droppable
//! owner — never a borrow into the live table. The table keeps its own copy and
//! frees it on `remove`/overwrite/drop. If the read aliased the table's slot
//! instead of cloning, removing the key would free the buffer the caller still
//! holds, producing a use-after-free read (scribbled bytes) or a double-free
//! (the caller's drop and `hew_hashmap_remove_layout` both releasing it).
//!
//! This is the headline GAP-2 drop-safety invariant for the `HashMap` indexed accessor
//! (`by-value-heap-params-are-borrows` P0), proven on the new trapping `m[k]`
//! path. `m.get(k) -> Option<V>` routes through the same clone choke, so this
//! oracle pins the choke for both siblings.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): insert
//!   an owned `Name { label: "owned-ok" }`, read it back through `m["k"]`,
//!   remove the key (freeing the table's copy), then read the cloned record's
//!   field. Under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` a
//!   double-free aborts the process and a use-after-free read returns scribbled
//!   memory, changing the output. The string equality plus the clean exit pin
//!   both regression directions.
//!
//! - **Per-iteration leak slope** (macOS-only via `leaks(1)`): compile the
//!   insert/index/remove cycle looped at a LOW and a HIGH iteration count,
//!   measure leak NODE counts under the poisoned-allocator triple, and assert
//!   the delta stays within a small constant tolerance. The delta cancels the
//!   nondeterministic constant baseline that a single-shot `== 0` count cannot,
//!   so the gate is deterministic: a genuine per-iteration leak (the `m[k]`
//!   clone retains but no drop runs) shows as a positive slope that scales with
//!   the iteration count, while the clean clone-then-drop path holds flat. The
//!   looped fixture builds a HEAP `label` via `.to_upper()` (a static literal
//!   would make the probe vacuous) and sums `got.label.len()` into the returned
//!   total so the clone cannot be elided. See `support::leak_slope` for the
//!   shared harness.
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

/// Single round-trip under the poisoned allocator: insert one `Name { label:
/// "owned-ok" }`, read it back through the trapping `m["k"]` accessor, remove
/// the key (freeing the table's copy), and read `got.label`. Under
/// `MallocScribble` a double-free aborts; a UAF read on `got.label` returns
/// scribbled memory (not `"owned-ok"`). The exact-string equality plus clean
/// exit pin both directions simultaneously.
const INDEX_GET_SINGLE_ROUNDTRIP_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn main() {\n\
\x20   let m: HashMap<string, Name> = HashMap::new();\n\
\x20   m.insert(\"k\", Name { label: \"owned-ok\" });\n\
\x20   let got: Name = m[\"k\"];\n\
\x20   let _ = m.remove(\"k\");\n\
\x20   print(got.label);\n\
}\n";

/// Expected exact output for `INDEX_GET_SINGLE_ROUNDTRIP_SOURCE`. Any aliasing
/// or UAF changes the `got.label` read.
const INDEX_GET_SINGLE_ROUNDTRIP_EXPECTED: &str = "owned-ok";

/// Looped insert/index/remove cycle for the per-iteration slope probe. Each
/// `run_cycle()` inserts a FRESH heap `label` (`"owned-ok".to_upper()`), reads
/// it back through the trapping `m["k"]` clone, removes the key (freeing the
/// table's copy), and returns `got.label.len()` so the cloned record cannot be
/// elided. `run_loop` drives `frames` cycles and sums the lengths; `main`
/// returns the total so the loop is not dead code. A `m[k]` clone whose drop
/// never runs would retain one `label` buffer per iteration, growing the
/// leak-node count with `frames`; the correct clone-then-drop path holds the
/// count flat.
fn index_roundtrip_loop_source(frames: usize) -> String {
    format!(
        "type Name {{ label: string; }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let m: HashMap<string, Name> = HashMap::new();\n\
         \x20   m.insert(\"k\", Name {{ label: \"owned-ok\".to_upper() }});\n\
         \x20   let got: Name = m[\"k\"];\n\
         \x20   let _ = m.remove(\"k\");\n\
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
        .prefix(&format!("hashmap-index-owned-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free of the owned `Name.label` string (the `m[k]` read aliased the table's \
         slot while `hew_hashmap_remove_layout` also released it);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back the label verbatim — scribbled/empty output indicates a \
         use-after-free read on `got.label` (the table slot was freed by `remove` before the \
         caller read the clone);\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: a single insert/index/remove/read round-trip must print
/// `"owned-ok"` and exit clean under the poisoned allocator. Reverting the
/// trapping read to alias the table's slot (so `m[k]` borrows while
/// `hew_hashmap_remove_layout` also releases it) fails this with either an abort
/// (double-free) or garbled output (UAF read).
#[test]
fn hashmap_index_owned_value_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "hashmap_index_single_roundtrip",
        INDEX_GET_SINGLE_ROUNDTRIP_SOURCE,
        INDEX_GET_SINGLE_ROUNDTRIP_EXPECTED,
    );
}

/// Per-iteration leak-slope oracle: the insert/index/remove cycle looped at LOW
/// vs HIGH iteration counts must hold the leak-node count flat (within the
/// shared tolerance). `m[k]` clones `label` into `got`; `m.remove("k")` frees
/// the table's copy; `got` drops at the end of each cycle, freeing the clone —
/// zero per-iteration retention. A regression that leaks the clone (a retain
/// with no matching drop) grows the count with the iteration count and trips the
/// slope assertion.
#[test]
fn hashmap_index_owned_value_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("hashmap_index_owned", index_roundtrip_loop_source);
}
