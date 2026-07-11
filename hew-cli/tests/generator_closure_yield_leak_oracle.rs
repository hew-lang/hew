//! Pending generator output leak oracle for a yielded capturing closure.
//!
//! A generator constructor runs to its first `yield`, so each loop iteration
//! leaves a closure pair pending in the generator companion's output slot. The
//! closure captures a heap-owning string and therefore owns both its env box
//! and the captured string allocation. Dropping the generator without consuming
//! that output must invoke the per-yield drop thunk and release both allocations.
//!
//! Before the fix, `ty_owns_heap` classified `ResolvedTy::Closure` as non-owning,
//! `MakeGenerator` stored a null output-drop thunk, and this exact eight-iteration
//! fixture leaked 16 nodes / 896 bytes under `leaks --atExit`.

#![cfg(unix)]

mod support;

use support::leak_slope::{compile_to_native, leaks_supported, measure_leaks};
use support::require_codegen;

const PENDING_CAPTURED_CLOSURE: &str = r#"
fn main() {
    var i = 0;
    while i < 8 {
        let _g = gen {
            let captured = f"generator-yielded-closure-owned-capture-{i}";
            yield || captured.len();
        };
        i = i + 1;
    }
}
"#;

#[test]
fn pending_generator_drops_yielded_heap_capturing_closure() {
    let shape = "generator_yielded_heap_capturing_closure";
    if !leaks_supported(shape) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("generator-closure-yield-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(PENDING_CAPTURED_CLOSURE, dir.path(), shape);
    let Some(leaks) = measure_leaks(&bin) else {
        return;
    };

    assert_eq!(
        leaks, 0,
        "dropping a generator with a yielded heap-capturing closure pending leaked \
         {leaks} allocation(s): the generator companion's output-drop thunk must \
         release the closure env box and its captured string"
    );
}
