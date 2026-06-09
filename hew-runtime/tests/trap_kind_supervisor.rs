//! Verify the supervisor maps each `HEW_TRAP_*` error code back to the
//! correct `ExitReason` variant.
//!
//! This is the inverse of the codegen-side change in
//! `hew-codegen-rs/src/llvm.rs` (`Terminator::Trap`): codegen emits
//! `hew_trap_with_code(<code>)` per `TrapKind`; the longjmp seam
//! records `code` as the actor's `error_code`; `ExitReason::from_error_code`
//! converts the raw i32 into the named variant the supervisor reports.
//!
//! Pure mapping tests — no actor crash machinery. The end-to-end
//! "real overflow inside an actor surfaces as `ExitReason::IntegerOverflow`"
//! coverage is in `overflow_trap_as_actor_crash.rs`.

use hew_runtime::supervisor::{
    ExitReason, HEW_TRAP_DIVIDE_BY_ZERO, HEW_TRAP_HEAP_EXCEEDED, HEW_TRAP_INDEX_OUT_OF_BOUNDS,
    HEW_TRAP_INTEGER_OVERFLOW, HEW_TRAP_SHIFT_OUT_OF_RANGE, HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
};

/// `HEW_TRAP_*` codes occupy a private band above POSIX signals
/// (which are < 32 on every supported platform). 200..=205 is the
/// reserved range; future trap kinds extend this band upward.
#[test]
fn trap_codes_are_above_posix_signal_range() {
    for code in [
        HEW_TRAP_HEAP_EXCEEDED,
        HEW_TRAP_INTEGER_OVERFLOW,
        HEW_TRAP_DIVIDE_BY_ZERO,
        HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
        HEW_TRAP_SHIFT_OUT_OF_RANGE,
        HEW_TRAP_INDEX_OUT_OF_BOUNDS,
    ] {
        assert!(
            code >= 64,
            "trap code {code} must be strictly above the POSIX signal range \
             (real-time signals reach 64 on Linux); otherwise from_error_code \
             collides with an OS signal"
        );
    }
}

/// Each trap code is a distinct constant. No accidental aliasing.
#[test]
fn trap_codes_are_pairwise_distinct() {
    let codes = [
        HEW_TRAP_HEAP_EXCEEDED,
        HEW_TRAP_INTEGER_OVERFLOW,
        HEW_TRAP_DIVIDE_BY_ZERO,
        HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
        HEW_TRAP_SHIFT_OUT_OF_RANGE,
        HEW_TRAP_INDEX_OUT_OF_BOUNDS,
    ];
    for (i, a) in codes.iter().enumerate() {
        for b in &codes[i + 1..] {
            assert_ne!(
                a, b,
                "two trap codes must not share a value (got {a} == {b})"
            );
        }
    }
}

#[test]
fn integer_overflow_code_maps_to_integer_overflow_variant() {
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_INTEGER_OVERFLOW),
        ExitReason::IntegerOverflow,
    );
}

#[test]
fn divide_by_zero_code_maps_to_divide_by_zero_variant() {
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_DIVIDE_BY_ZERO),
        ExitReason::DivideByZero,
    );
}

#[test]
fn signed_min_div_neg_one_code_maps_to_signed_min_div_neg_one_variant() {
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE),
        ExitReason::SignedMinDivNegOne,
    );
}

#[test]
fn shift_out_of_range_code_maps_to_shift_out_of_range_variant() {
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_SHIFT_OUT_OF_RANGE),
        ExitReason::ShiftOutOfRange,
    );
}

#[test]
fn index_out_of_bounds_code_maps_to_index_out_of_bounds_variant() {
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_INDEX_OUT_OF_BOUNDS),
        ExitReason::IndexOutOfBounds,
    );
}

/// Codes outside the reserved trap band still flow through
/// `ExitReason::Signal` so OS-signal crashes (SIGSEGV, SIGFPE, etc.)
/// keep their existing reporting shape.
#[test]
fn posix_signals_still_map_to_signal_variant() {
    assert_eq!(ExitReason::from_error_code(4), ExitReason::Signal(4)); // SIGILL
    assert_eq!(ExitReason::from_error_code(8), ExitReason::Signal(8)); // SIGFPE
    assert_eq!(ExitReason::from_error_code(11), ExitReason::Signal(11)); // SIGSEGV
}

/// `HEW_TRAP_HEAP_EXCEEDED` mapping is unchanged by this fix — the
/// arena-cap precedent that the per-kind work mirrors.
#[test]
fn heap_exceeded_code_still_maps_to_heap_exceeded_variant() {
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_HEAP_EXCEEDED),
        ExitReason::HeapExceeded,
    );
}

/// `code == 0` is the "no crash" sentinel; it must map to `Normal`.
#[test]
fn zero_code_maps_to_normal() {
    assert_eq!(ExitReason::from_error_code(0), ExitReason::Normal);
}
