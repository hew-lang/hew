//! Executable sole-owner/retention proof for `hew_string_to_bytes`.
//!
//! A `fresh` declaration is not enough to let the compiler mint a caller
//! release. These probes exercise the real runtime boundary and establish:
//!
//! - R1: the result storage is distinct from the source and from a concurrent
//!   second result;
//! - R2: every non-empty result arrives with the bytes header's live owner
//!   count equal to one;
//! - R3: source and result remain usable when released in either order, and
//!   mutations on either side do not affect the other;
//! - R4: one `hew_bytes_drop` per non-empty result balances repeated low/high
//!   allocation loops under the sanitizer/leak harness.
//!
//! The bytes header is part of the runtime's documented representation:
//! `[refcount:u32 | capacity:u32 | data...]`. Reading those two words is a
//! non-mutating ownership oracle at the actual allocation site.

use std::sync::atomic::{AtomicU32, Ordering};

use hew_cabi::string::{string_as_bytes, string_from_str, HewString};
use hew_runtime::bytes::{hew_bytes_drop, hew_bytes_push, hew_bytes_set, BytesTriple};
use hew_runtime::string::{hew_string_drop, hew_string_to_bytes};

const BYTES_HEADER_SIZE: usize = 8;

/// Read the owner count stored immediately before a live bytes data pointer.
///
/// # Safety
///
/// `ptr` must be a live, non-null pointer returned by the bytes runtime.
#[expect(
    clippy::cast_ptr_alignment,
    reason = "the documented bytes header is malloc-aligned; AtomicU32 needs four-byte alignment"
)]
unsafe fn owner_count(ptr: *mut u8) -> u32 {
    // SAFETY: the caller supplies a live bytes pointer; its refcount is the
    // first AtomicU32 in the eight-byte header.
    unsafe { (&*ptr.sub(BYTES_HEADER_SIZE).cast::<AtomicU32>()).load(Ordering::Acquire) }
}

/// Read the capacity word stored immediately before a live bytes data pointer.
///
/// # Safety
///
/// `ptr` must be a live, non-null pointer returned by the bytes runtime.
#[expect(
    clippy::cast_ptr_alignment,
    reason = "the documented bytes header is malloc-aligned; u32 needs four-byte alignment"
)]
unsafe fn capacity(ptr: *mut u8) -> u32 {
    // SAFETY: the capacity is the second u32 in the documented bytes header.
    unsafe { ptr.sub(4).cast::<u32>().read() }
}

/// Borrow the active bytes region.
///
/// # Safety
///
/// `triple` must describe a live bytes value.
unsafe fn active_bytes(triple: &BytesTriple) -> &[u8] {
    if triple.ptr.is_null() || triple.len == 0 {
        return &[];
    }
    // SAFETY: the caller guarantees the triple is live and internally valid.
    unsafe {
        std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
    }
}

fn runtime_string(text: &str) -> *mut HewString {
    string_from_str(text)
}

#[test]
fn empty_result_has_no_storage_or_release_obligation() {
    let source = runtime_string("");
    // SAFETY: `source` is a live NUL-terminated runtime string.
    let result = unsafe { hew_string_to_bytes(source) };
    assert!(result.ptr.is_null());
    assert_eq!(result.offset, 0);
    assert_eq!(result.len, 0);

    // A bytes drop is the uniform release ritual and is a no-op for the
    // canonical empty result.
    // SAFETY: null is explicitly accepted by `hew_bytes_drop`.
    unsafe { hew_bytes_drop(result.ptr) };
    // SAFETY: the bytes no-op cannot affect the independently-owned source.
    assert_eq!(unsafe { string_as_bytes(source) }, b"");
    // SAFETY: this is the source string's one balancing release.
    // SAFETY: `source` is the final live owner returned by the test constructor.
    unsafe { hew_string_drop(source) };
}

#[test]
fn live_results_and_source_are_independent_and_release_in_both_orders() {
    let source = runtime_string("source-owner");
    // SAFETY: `source` remains live across both conversions.
    let mut first = unsafe { hew_string_to_bytes(source) };
    // SAFETY: as above.
    let second = unsafe { hew_string_to_bytes(source) };

    assert!(!first.ptr.is_null() && !second.ptr.is_null());
    assert_ne!(first.ptr, second.ptr, "two live results aliased storage");
    assert_ne!(
        first.ptr.cast::<()>(),
        source.cast::<()>(),
        "result retained the source storage"
    );
    assert_ne!(
        second.ptr.cast::<()>(),
        source.cast::<()>(),
        "second result retained the source storage"
    );
    // SAFETY: both results are live non-null bytes allocations.
    unsafe {
        assert_eq!(owner_count(first.ptr), 1);
        assert_eq!(owner_count(second.ptr), 1);
        assert_eq!(active_bytes(&first), b"source-owner");
        assert_eq!(active_bytes(&second), b"source-owner");
    }

    // Mutating one result cannot change the source or the concurrent result.
    // SAFETY: `first` is live, uniquely owned, and index zero is in bounds.
    unsafe { hew_bytes_set(&mut first, 0, b'B') };
    // SAFETY: all three values remain live.
    unsafe {
        assert_eq!(active_bytes(&first), b"Bource-owner");
        assert_eq!(active_bytes(&second), b"source-owner");
        assert_eq!(string_as_bytes(source), b"source-owner");
    }

    // Bytes-first order: release one result, then prove the source is intact.
    // SAFETY: R2 measured exactly one owner for `first`.
    unsafe { hew_bytes_drop(first.ptr) };
    // SAFETY: the source is still live.
    let source_bytes = unsafe { string_as_bytes(source) };
    assert_eq!(source_bytes, b"source-owner");

    // Source-first order: release the source, then prove the other result is
    // still readable and independently releasable.
    // SAFETY: this is the source's one balancing release.
    unsafe { hew_string_drop(source) };
    // SAFETY: `second` has independent live storage.
    assert_eq!(unsafe { active_bytes(&second) }, b"source-owner");
    // SAFETY: R2 measured exactly one owner for `second`.
    unsafe { hew_bytes_drop(second.ptr) };
}

#[test]
fn embedded_nul_and_trailing_data_are_copied_without_aliasing() {
    let source = runtime_string("ab\0hidden-tail\0");
    // SAFETY: `source` is a live managed string.
    let result = unsafe { hew_string_to_bytes(source) };
    assert_eq!(result.len, 15);
    assert_ne!(
        result.ptr,
        source.cast::<u8>(),
        "the result aliases the raw C-string source"
    );
    // SAFETY: `result` is a live non-empty bytes allocation.
    unsafe {
        assert_eq!(owner_count(result.ptr), 1);
        assert_eq!(active_bytes(&result), b"ab\0hidden-tail\0");
    }
    // SAFETY: `source` is the final live owner and the copied bytes are independent.
    unsafe { hew_string_drop(source) };
    // SAFETY: `result` is independently live.
    assert_eq!(unsafe { active_bytes(&result) }, b"ab\0hidden-tail\0");
    // SAFETY: R2 measured exactly one result owner.
    unsafe { hew_bytes_drop(result.ptr) };
}

#[test]
fn capacity_changing_mutation_never_writes_back_into_the_source() {
    let source = runtime_string("0123456789abcdef");
    // SAFETY: `source` is a live NUL-terminated runtime string.
    let mut result = unsafe { hew_string_to_bytes(source) };
    // SAFETY: `result` is a live non-empty bytes allocation.
    let before_capacity = unsafe { capacity(result.ptr) };
    assert_eq!(before_capacity, 16, "fixture must begin at exact capacity");
    // SAFETY: `result` is live and uniquely owned.
    unsafe { hew_bytes_push(&mut result, b'!') };
    assert_eq!(result.len, 17);
    // SAFETY: the possibly-reallocated result is live.
    unsafe {
        assert!(capacity(result.ptr) >= 17);
        assert_eq!(owner_count(result.ptr), 1);
        assert_eq!(active_bytes(&result), b"0123456789abcdef!");
        assert_eq!(string_as_bytes(source), b"0123456789abcdef");
    }

    // Release result first, then read and release source.
    // SAFETY: the post-growth result still carries exactly one owner.
    unsafe { hew_bytes_drop(result.ptr) };
    // SAFETY: source storage is independent and remains live.
    let source_bytes = unsafe { string_as_bytes(source) };
    assert_eq!(source_bytes, b"0123456789abcdef");
    // SAFETY: this is the source's one balancing release.
    unsafe { hew_string_drop(source) };
}

fn run_release_loop(rounds: usize) {
    let source = runtime_string("loop");
    for _ in 0..rounds {
        // SAFETY: `source` remains live for the full loop.
        let result = unsafe { hew_string_to_bytes(source) };
        assert!(!result.ptr.is_null());
        // SAFETY: every result is live and non-null until the matching drop.
        unsafe {
            assert_eq!(owner_count(result.ptr), 1);
            assert_eq!(active_bytes(&result), b"loop");
            hew_bytes_drop(result.ptr);
        }
    }
    // SAFETY: all result storage was independent, so the source remains live.
    assert_eq!(unsafe { string_as_bytes(source) }, b"loop");
    // SAFETY: this is the source's one balancing release.
    unsafe { hew_string_drop(source) };
}

#[test]
fn repeated_low_and_high_loops_balance_one_release_per_allocation() {
    run_release_loop(3);
    run_release_loop(1_024);
}
