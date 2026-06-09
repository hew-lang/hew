//! MIR-shape tests for C-3: `Vec<T>` range-slice (`xs[a..b]`,
//! `xs[a..=b]`, and open-end forms) with bounds-checked trap-on-OOB
//! and overflow-trap on the inclusive `+1` bump.
//!
//! These tests verify:
//!
//! 1. The allowlist contains the five `hew_vec_slice_range_*` symbols.
//! 2. `RuntimeCall::new` accepts each symbol with the (vec, start, end)
//!    arity.
//! 3. Allowlist-rejection still fails closed for typos.
//!
//! The structural MIR-shape assertion (CFG with two bounds checks +
//! optional overflow trap + slice call) is exercised at codegen via
//! the LLVM-IR emission test (`hew-codegen-rs/tests/vec_slice_emission.rs`)
//! which catches both the producer and the lowering arms simultaneously.
//! Vec literal construction is not yet lowered (C-2/C-3 share that gap),
//! so a source-text pipeline test would need a Vec parameter; that
//! coverage lives in the codegen emission file with a hand-built pipeline.

use hew_mir::runtime_symbols::is_known_runtime_symbol;
use hew_mir::Place;

#[test]
fn allowlist_covers_slice_range_family() {
    let symbols = [
        "hew_vec_slice_range_i32",
        "hew_vec_slice_range_i64",
        "hew_vec_slice_range_f64",
        "hew_vec_slice_range_ptr",
        "hew_vec_slice_range_str",
    ];
    for sym in &symbols {
        assert!(
            is_known_runtime_symbol(sym),
            "Vec slice symbol `{sym}` must be in the M2 runtime-ABI allowlist",
        );
    }
}

#[test]
fn runtime_call_constructs_for_hew_vec_slice_range_i64() {
    let call = hew_mir::RuntimeCall::new(
        "hew_vec_slice_range_i64",
        vec![Place::Local(0), Place::Local(1), Place::Local(2)],
        Some(Place::Local(3)),
    )
    .expect("hew_vec_slice_range_i64 is allowlisted");
    assert_eq!(call.symbol(), "hew_vec_slice_range_i64");
    assert_eq!(call.args().len(), 3);
    assert_eq!(call.dest(), Some(Place::Local(3)));
}

#[test]
fn runtime_call_constructs_for_hew_vec_slice_range_str() {
    // String slicing is supported (runtime copies header-aware elements); the
    // allowlist boundary must accept the symbol so the MIR producer can
    // emit a Vec<String>-slice without `NotYetImplemented`.
    let call = hew_mir::RuntimeCall::new(
        "hew_vec_slice_range_str",
        vec![Place::Local(0), Place::Local(1), Place::Local(2)],
        Some(Place::Local(3)),
    )
    .expect("hew_vec_slice_range_str is allowlisted");
    assert_eq!(call.symbol(), "hew_vec_slice_range_str");
}

#[test]
fn unknown_slice_symbol_rejected() {
    // Typo-class bugs (`_slice_` vs `_silce_`, or wrong element suffix)
    // must fail closed.
    assert!(!is_known_runtime_symbol("hew_vec_silce_range_i64"));
    assert!(!is_known_runtime_symbol("hew_vec_slice_range_u8"));
    assert!(!is_known_runtime_symbol("hew_vec_slice_range_"));
}
