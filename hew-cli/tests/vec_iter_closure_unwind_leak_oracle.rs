//! Fresh `VecIter` temporary ownership across a trapping closure call.
//!
//! The caller owns the snapshot produced by `values.iter()`. A normal return
//! releases it immediately after the call; a panic in the closure returns a
//! failure status to the caller, whose failure edge must run the same release
//! once before the actor's crash propagates.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, require_leaks_tool, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const TRAPPING_VEC_ITER_CLOSURE: &str = r#"
actor VecIterCrasher {
    receive fn boom() {
        let root = Rc.new(41);
        var values: Vec<Rc<i64>> = Vec.new();
        values.push(root);

        let crash = |incoming: VecIter<Rc<i64>>| {
            let _ = incoming;
            panic("vec-iter-closure-trap");
            let _ = 0;
        };
        crash(values.iter());
        // The call's failure edge owns `values`; the later suspend state
        // owns `moved`. Releasing `moved` on the failure edge would release
        // an uninitialized destination after the closure traps.
        let moved = values;
        sleep(1ms);
        let _ = moved.len();
    }
}

actor VecIterProbe {
    receive fn ping() -> i64 { 0 }
}

fn main() -> i64 {
    let crasher = spawn VecIterCrasher;
    let probe = spawn VecIterProbe;
    let _ = crasher.boom();
    sleep(300ms);
    match probe.ping() {
        .Ok(value) => value,
        .Err(_) => 1,
    }
}
"#;

#[test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leaks(1) and malloc poisoning are macOS-only"
)]
fn temporary_vec_iter_closure_trap_releases_snapshot_without_leak() {
    require_codegen();
    require_leaks_tool();

    let temp = tempfile::tempdir().expect("create VecIter closure unwind oracle dir");
    let bin = compile_to_native(
        TRAPPING_VEC_ITER_CLOSURE,
        temp.path(),
        "vec_iter_closure_unwind",
    );

    let witness = run_under_malloc_scribble(&bin);
    assert_eq!(
        witness.status.code(),
        Some(1),
        "actor-isolated trapping closure must reach the runtime's ordinary crash exit, not a memory-safety signal:\n{}",
        describe_output(&witness)
    );
    assert!(
        String::from_utf8_lossy(&witness.stderr).contains("vec-iter-closure-trap"),
        "trap witness did not execute the closure body:\n{}",
        describe_output(&witness)
    );

    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "fresh VecIter snapshot leaked or retained stale cleanup authority on closure unwind"
    );
}
