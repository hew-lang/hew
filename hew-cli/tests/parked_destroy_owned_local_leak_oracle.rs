//! Destroy-while-parked owned-local leak / double-free oracle (#2395).
//!
//! ## What this proves
//!
//! A coroutine (actor handler / receive-gen pump / generator body) parked at a
//! suspend point and then DESTROYED without resuming — a supervisor stopping a
//! parked child, teardown — must drop the Hew heap values owned by its live
//! locals on the destroy (abandon) edge, before its frame is freed. Before the
//! fix the frame was freed but its owned locals leaked (2 leaks / 176 B on the
//! plain-actor `Vec` shape); the fix emits the suspend exit's elaborated drop
//! plan on the `coro.suspend` case-1 edge.
//!
//! The failure modes this oracle catches:
//!   * a LEAK — the owned local is not dropped on the abandon edge (a non-zero
//!     leak count on the deterministic single-shot shape);
//!   * a DOUBLE-FREE — a value MOVED OUT across the suspend is wrongly dropped on
//!     the abandon edge as well as by its new owner, which the poisoned allocator
//!     turns into an abort (the moved-out wall).
//!
//! ## Methodology
//!
//! These shapes are deterministic single-shot teardowns (not per-iteration
//! slopes): the handler holds one owned value live across a `sleep`, `main`
//! lets it park, then `supervisor_stop` destroys it while parked. The correct
//! program leaks exactly zero nodes, so an exact-zero `leaks --atExit` assertion
//! is trustworthy (there is no per-iteration baseline noise to cancel). The
//! moved-out wall is pinned under the poisoned-allocator triple: a double-free of
//! the single shared buffer aborts.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    compile_to_native, measure_leaks, require_leaks_tool, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// A plain actor holds an owned `Vec<i64>` live across `sleep(10s)`; the
/// supervisor stops it while parked, destroying the coroutine without resuming.
/// The `Vec`'s heap buffer must be freed on the abandon edge — 0 leaks.
const PARKED_VEC_TEARDOWN: &str = r#"
actor Sleeper {
    receive fn work() {
        let xs: Vec<i64> = Vec::new();
        xs.push(1);
        xs.push(2);
        xs.push(3);
        sleep(10s);
        println(f"{xs.len()}");
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child sleeper: Sleeper;
}

fn main() {
    let sup = spawn App;
    let s = sup.sleeper;
    s.work();
    sleep(200ms);
    supervisor_stop(sup);
}
"#;

/// A first-class `VecIter<i64>` owns a cloned Vec snapshot but is deliberately
/// excluded from the ordinary owned-local LIFO. When the handler is destroyed
/// while parked, the cursor's flag-gated abandon drop must release field 0;
/// the resumed-only `for` remains unreachable, so no lexical cursor cleanup can
/// mask a missing destroy-edge release.
const PARKED_VEC_ITER_TEARDOWN: &str = r#"
actor Sleeper {
    receive fn work() {
        let values: Vec<i64> = Vec::new();
        values.push(40);
        values.push(2);
        let cursor = values.iter();
        sleep(10s);
        var sum: i64 = 0;
        for value in cursor {
            sum = sum + value;
        }
        println(f"{sum}");
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child sleeper: Sleeper;
}

fn main() {
    let sup = spawn App;
    let s = sup.sleeper;
    s.work();
    sleep(200ms);
    supervisor_stop(sup);
}
"#;

/// Wall: a local MOVED OUT across the suspend must NOT be dropped on the abandon
/// edge. `let ys = xs;` moves the `Vec` handle (xs and ys share ONE buffer); at
/// the park xs is Consumed and ys is Live, so only ys is dropped on destroy.
/// Dropping both would free the shared buffer twice.
const MOVED_OUT_ACROSS_SUSPEND: &str = r#"
actor Mover {
    receive fn go() {
        let xs: Vec<i64> = Vec::new();
        xs.push(1);
        xs.push(2);
        xs.push(3);
        let ys = xs;
        sleep(10s);
        println(f"{ys.len()}");
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child mover: Mover;
}

fn main() {
    let sup = spawn App;
    let m = sup.mover;
    m.go();
    sleep(200ms);
    supervisor_stop(sup);
}
"#;

/// A collection move-out result bound across the suspend: `v.remove(1)` moves
/// the owned string OUT of the vec (the kernel runs no drop — the binding is
/// the sole owner) while the vec keeps its two remaining elements. On destroy
/// while parked the abandon plan must drop the binding exactly once AND the
/// vec's walk must not re-touch the moved-out slot: dropping `removed` twice —
/// or having the vec's free re-drop it — frees the same buffer twice.
const REMOVED_ELEMENT_ACROSS_SUSPEND: &str = r#"
actor Sleeper {
    receive fn work() {
        let v: Vec<string> = Vec::new();
        v.push("element-alpha-longish-to-force-a-heap-allocation");
        v.push("element-beta-longish-to-force-a-heap-allocation");
        v.push("element-gamma-longish-to-force-a-heap-allocation");
        let removed = v.remove(1);
        sleep(10s);
        println(f"{removed.len()} {v.len()}");
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child sleeper: Sleeper;
}

fn main() {
    let sup = spawn App;
    let s = sup.sleeper;
    s.work();
    sleep(200ms);
    supervisor_stop(sup);
}
"#;

/// The `HashMap` sibling: `m.remove(k)` drops the stored key in the kernel and
/// moves the value out as the `Some` payload; the bound value and the map (one
/// surviving pair) are both live across the park. On destroy the abandon plan
/// drops the binding and the map exactly once each — the tombstoned slot must
/// never be re-dropped by the map's free walk.
const TAKEN_VALUE_ACROSS_SUSPEND: &str = r#"
actor Sleeper {
    receive fn work() {
        let m: HashMap<string, string> = HashMap::new();
        m.insert("key-alpha-long-enough-to-heap-allocate", "val-alpha-long-enough-to-heap-allocate");
        m.insert("key-beta-long-enough-to-heap-allocate", "val-beta-long-enough-to-heap-allocate");
        let taken = match m.remove("key-beta-long-enough-to-heap-allocate") {
            Some(s) => s,
            None => "MISS",
        };
        sleep(10s);
        println(f"{taken.len()} {m.len()}");
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child sleeper: Sleeper;
}

fn main() {
    let sup = spawn App;
    let s = sup.sleeper;
    s.work();
    sleep(200ms);
    supervisor_stop(sup);
}
"#;

/// A live enum payload binder becomes the delayed-release owner when its parent
/// is overwritten, then crosses a real suspend. Destroying the parked actor
/// must run the suspend plan's flag-gated binder drop exactly once.
const OVERWRITTEN_ENUM_BINDER_ACROSS_SUSPEND: &str = r#"
enum Box {
    Full(string);
    Empty;
}

actor Sleeper {
    receive fn work() {
        var opt = Box::Full(f"parked-overwritten-payload");
        match opt {
            Full(s) => {
                opt = Box::Empty;
                sleep(10s);
                println(f"{s.len()}");
            },
            Empty => {},
        }
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child sleeper: Sleeper;
}

fn main() {
    let sup = spawn App;
    let s = sup.sleeper;
    s.work();
    sleep(200ms);
    supervisor_stop(sup);
}
"#;

/// The #2395 regression pin: an owned local live across a suspend, destroyed
/// while parked, leaks zero nodes. Skips gracefully when `leaks(1)` is
/// unavailable (non-macOS or `leaks` off PATH).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn parked_destroy_frees_owned_local_zero_leaks() {
    let shape = "parked_vec_teardown";
    require_leaks_tool();
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("parked-destroy-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(PARKED_VEC_TEARDOWN, dir.path(), shape);
    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks,
        0,
        "destroy-while-parked leaked {leaks} node(s): the owned Vec live across \
         the sleep was not dropped on the coroutine abandon edge (#2395). Re-run \
         with `MallocStackLogging=1 leaks --atExit -- {}` for the leaked stack.",
        bin.display()
    );
}

/// First-class cursor regression: its cloned snapshot is not in
/// `owned_locals`, so only the dedicated flag-gated `VecIter` abandon drop can
/// free it when the coroutine is destroyed while parked.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn parked_destroy_frees_first_class_vec_iter_snapshot_zero_leaks() {
    let shape = "parked_vec_iter_teardown";
    require_leaks_tool();
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("parked-vec-iter-destroy-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(PARKED_VEC_ITER_TEARDOWN, dir.path(), shape);

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "destroy-while-parked VecIter teardown aborted under the poisoned \
         allocator — the cursor snapshot release competed with another owner:\n{}",
        describe_output(&output)
    );

    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks,
        0,
        "destroy-while-parked leaked {leaks} node(s): the first-class VecIter \
         snapshot was not released on the coroutine abandon edge. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` for the leaked stack.",
        bin.display()
    );
}

/// The moved-out wall: a value moved across the suspend must not be double-freed
/// on the abandon edge. Runs under the Darwin poisoned-allocator triple on
/// macOS, where a double-free of the shared buffer aborts, and also asserts zero
/// leaks there.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn moved_out_across_suspend_no_double_free() {
    let shape = "moved_out_across_suspend";
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("moved-out-suspend-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(MOVED_OUT_ACROSS_SUSPEND, dir.path(), shape);

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "moved-out-across-suspend aborted under the poisoned allocator — a value \
         moved out before the park was double-freed on the abandon edge:\n{}",
        describe_output(&output)
    );

    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks, 0,
        "moved-out-across-suspend leaked {leaks} node(s): the surviving \
         owner (ys) was not freed on the abandon edge.",
    );
}

/// A bound `Vec.remove(i)` move-out result live across the park is dropped
/// exactly once on the abandon edge: no double-free of the moved-out string
/// under the poisoned allocator, zero leaks (binding + remaining elements +
/// buffer all freed).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn removed_element_across_suspend_dropped_exactly_once() {
    let shape = "removed_element_across_suspend";
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("removed-elem-suspend-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(REMOVED_ELEMENT_ACROSS_SUSPEND, dir.path(), shape);

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "removed-element-across-suspend aborted under the poisoned allocator — \
         the moved-out Vec.remove result was dropped on the abandon edge more \
         than once (binding drop + a stale vec-slot drop):\n{}",
        describe_output(&output)
    );

    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks, 0,
        "removed-element-across-suspend leaked {leaks} node(s): the bound \
         move-out result (or the vec's remaining elements) was not freed \
         on the coroutine abandon edge.",
    );
}

/// A bound `HashMap.remove(k)` `Some` payload live across the park is dropped
/// exactly once on the abandon edge; the map's tombstoned slot is never
/// re-dropped and its surviving pair frees exactly once.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn hashmap_taken_value_across_suspend_dropped_exactly_once() {
    let shape = "hashmap_taken_value_across_suspend";
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("taken-value-suspend-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(TAKEN_VALUE_ACROSS_SUSPEND, dir.path(), shape);

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "hashmap-taken-value-across-suspend aborted under the poisoned allocator \
         — the moved-out remove value was re-dropped on the abandon edge (a \
         tombstoned-slot re-drop or a doubled binding drop):\n{}",
        describe_output(&output)
    );

    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks, 0,
        "hashmap-taken-value-across-suspend leaked {leaks} node(s): the \
         bound Some payload, the dropped key, or the map's surviving pair \
         was not freed exactly once across remove + abandon.",
    );
}

/// The delayed binder owner must survive ordinary suspension and be released
/// by task destruction. A stale parent owner double-frees under allocator
/// poisoning; a missing suspend-plan binder drop leaks one node.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn overwritten_enum_payload_binder_drops_on_parked_destroy() {
    let shape = "overwritten_enum_binder_across_suspend";
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("overwritten-enum-binder-suspend-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(OVERWRITTEN_ENUM_BINDER_ACROSS_SUSPEND, dir.path(), shape);

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "destroying the parked task must not release the overwritten parent and delayed \
         binder as competing owners:\n{}",
        describe_output(&output)
    );

    let leaks = measure_leaks(&bin);
    assert_eq!(
        leaks, 0,
        "destroying the parked task leaked {leaks} node(s): the live binder that inherited \
         the overwritten enum payload was absent from the suspend cleanup plan",
    );
}
