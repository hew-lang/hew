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
    compile_to_native, leaks_supported, measure_leaks, run_under_malloc_scribble,
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

/// The #2395 regression pin: an owned local live across a suspend, destroyed
/// while parked, leaks zero nodes. Skips gracefully when `leaks(1)` is
/// unavailable (non-macOS or `leaks` off PATH).
#[test]
fn parked_destroy_frees_owned_local_zero_leaks() {
    let shape = "parked_vec_teardown";
    if !leaks_supported(shape) {
        return;
    }
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("parked-destroy-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(PARKED_VEC_TEARDOWN, dir.path(), shape);
    let Some(leaks) = measure_leaks(&bin) else {
        return;
    };
    assert_eq!(
        leaks,
        0,
        "destroy-while-parked leaked {leaks} node(s): the owned Vec live across \
         the sleep was not dropped on the coroutine abandon edge (#2395). Re-run \
         with `MallocStackLogging=1 leaks --atExit -- {}` for the leaked stack.",
        bin.display()
    );
}

/// The moved-out wall: a value moved across the suspend must not be double-freed
/// on the abandon edge. Runs under the poisoned-allocator triple on any unix; a
/// double-free of the shared buffer aborts. Also asserts zero leaks on macOS.
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

    if leaks_supported(shape) {
        if let Some(leaks) = measure_leaks(&bin) {
            assert_eq!(
                leaks, 0,
                "moved-out-across-suspend leaked {leaks} node(s): the surviving \
                 owner (ys) was not freed on the abandon edge.",
            );
        }
    }
}

/// A bound `Vec.remove(i)` move-out result live across the park is dropped
/// exactly once on the abandon edge: no double-free of the moved-out string
/// under the poisoned allocator, zero leaks (binding + remaining elements +
/// buffer all freed).
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

    if leaks_supported(shape) {
        if let Some(leaks) = measure_leaks(&bin) {
            assert_eq!(
                leaks, 0,
                "removed-element-across-suspend leaked {leaks} node(s): the bound \
                 move-out result (or the vec's remaining elements) was not freed \
                 on the coroutine abandon edge.",
            );
        }
    }
}

/// A bound `HashMap.remove(k)` `Some` payload live across the park is dropped
/// exactly once on the abandon edge; the map's tombstoned slot is never
/// re-dropped and its surviving pair frees exactly once.
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

    if leaks_supported(shape) {
        if let Some(leaks) = measure_leaks(&bin) {
            assert_eq!(
                leaks, 0,
                "hashmap-taken-value-across-suspend leaked {leaks} node(s): the \
                 bound Some payload, the dropped key, or the map's surviving pair \
                 was not freed exactly once across remove + abandon.",
            );
        }
    }
}
