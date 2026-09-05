//! Who owns the buffer a `*_last_error` export hands back (hew-lang/hew#2828).
//!
//! # The question, and why it needs an instrument
//!
//! `hew_process_last_error() -> string` is *declared* a fresh owner by
//! `scripts/jit-symbol-classification.toml`. A declaration is not evidence.
//! Both readings of it are unsafe if guessed: withhold the caller's release and
//! the buffer leaks; mint one over a pointer the runtime still owns and a double
//! free becomes reachable. So the answer has to come from what the runtime
//! actually does with the buffer after it returns it.
//!
//! # The instrument
//!
//! An owner-count / allocation-balance oracle read at the real allocation site.
//! Every header-aware Hew string carries the runtime's own live-owner count in
//! its 16-byte header: `alloc_cstring_data` stamps `rc = 1`, `free_cstring`
//! decrements and releases the block at zero. [`cstring_ensure_unique`] is the
//! public, non-destructive read of that count — it returns its argument
//! unchanged exactly when `rc == 1`, and a *different* pointer (a copy-on-write
//! fork) when any other owner exists. So `ensure_unique(p) == p` is a direct
//! statement that no other owner of `p` exists anywhere in the process.
//!
//! Three probes per symbol, run by [`assert_result_is_transferred`]:
//!
//! * **R1 — distinct live addresses.** Two results obtained back to back, both
//!   still live, must be different pointers. A borrow into storage the callee
//!   keeps cannot satisfy this; a fresh allocation per call cannot fail it.
//! * **R2 — sole ownership at handoff.** `rc == 1` on each result. If the
//!   runtime had retained an owner the count would be at least two and the
//!   caller's release would not be the balancing one.
//! * **R3 — the callee's state survives the caller's release.** After the
//!   caller releases both results through the named release symbol
//!   (`hew_string_drop`), the next call still reports the same message. The
//!   callee therefore held no pointer into what was freed.
//!
//! R1 ∧ R2 ∧ R3 is the transfer: exactly one owner exists, it is the caller's,
//! and releasing it disturbs nothing the runtime kept.
//!
//! # The counterfactual that keeps the oracle load-bearing
//!
//! [`hew_last_error_is_a_borrow_not_a_transfer`] runs R1 against
//! `hew_last_error`, the sibling export `hew_process_last_error` is built on
//! top of. It FAILS R1 — repeated calls return the identical address, because
//! the pointer is the interior of the thread-local `CString` the runtime keeps.
//! That is a different answer for a symbol of the same family, established by
//! the same probe, and it is why R1 is not a formality: a symbol that borrows
//! is distinguishable here.
//!
//! The hew-std half of the family (`hew_tls_last_error`,
//! `hew_smtp_last_error`, `hew_cron_last_error`, …) is probed by the identical
//! harness in `hew-std/src/last_error_retention.rs`; it lives there because
//! those exports are that crate's.

use std::ffi::{c_char, CStr, CString};

use hew_cabi::cabi::cstring_ensure_unique;
use hew_runtime::process::hew_process_last_error;
use hew_runtime::stream_error::hew_stream_last_error;

/// Record an error in the runtime's thread-local process-error slot without
/// touching the filesystem or spawning anything: a negative `argc` is rejected
/// before any argument is read.
fn induce_process_error() {
    let cmd = CString::new("hew-2828-oracle").expect("literal has no NUL");
    // SAFETY: `argc` is negative, so the export returns before dereferencing
    // `cmd` or `args`; the pointers are valid regardless.
    unsafe {
        hew_runtime::process::hew_process_run_args(cmd.as_ptr(), std::ptr::null(), -1);
    }
}

/// Record an error in the runtime's stream/sink slot. `hew_stream_last_error`
/// TAKES the message, so this runs before every read rather than once.
fn induce_stream_error() {
    let msg = b"hew-2828-oracle: stream";
    // SAFETY: `msg` is a valid initialized byte range of the stated length.
    unsafe { hew_runtime::stream_error::hew_stream_set_last_error(msg.as_ptr(), msg.len()) };
}

/// Run R1/R2/R3 against one `-> string` export and assert the result is the
/// caller's to release. `induce` runs before every read so a take-and-clear
/// slot (`hew_stream_last_error`) and a clone-on-read slot
/// (`hew_process_last_error`) are measured on the same terms.
fn assert_result_is_transferred(
    symbol: &str,
    induce: fn(),
    call: unsafe extern "C" fn() -> *mut c_char,
) {
    induce();
    // SAFETY: the export takes no arguments and returns null or a header-aware
    // Hew string.
    let first = unsafe { call() };
    assert!(
        !first.is_null(),
        "{symbol}: expected a message after inducing one"
    );
    induce();
    // SAFETY: as above.
    let second = unsafe { call() };
    assert!(
        !second.is_null(),
        "{symbol}: expected a message after inducing one"
    );

    // R1 — both results are live at once and must be distinct allocations.
    assert_ne!(
        first, second,
        "{symbol}: two live results share an address, so the export hands out a \
         borrow into storage it keeps rather than a fresh allocation"
    );

    // R2 — the caller holds the sole owner of each.
    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: the pointer is a live header-aware Hew string from the export.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: the {label} result was not solely owned at handoff \
             (rc > 1), so the caller's release would not balance it"
        );
    }

    // SAFETY: `first` is a live header-aware Hew string.
    let text = unsafe { CStr::from_ptr(first) }.to_owned();

    // R3 — release both through the release symbol the contract names, then
    // read again: the callee's own state must be untouched by the free.
    // SAFETY: each pointer is a live, solely-owned header-aware Hew string
    // (R2), so this is its balancing release.
    unsafe {
        hew_cabi::cabi::free_cstring(first);
        hew_cabi::cabi::free_cstring(second);
    }
    induce();
    // SAFETY: as above.
    let third = unsafe { call() };
    assert!(
        !third.is_null(),
        "{symbol}: the slot did not survive the release"
    );
    // SAFETY: `third` is a live header-aware Hew string.
    let after = unsafe { CStr::from_ptr(third) }.to_owned();
    assert_eq!(
        text, after,
        "{symbol}: the message changed after the caller released an earlier \
         result, so the callee retained a pointer into the freed buffer"
    );
    // SAFETY: `third` is live and solely owned.
    unsafe { hew_cabi::cabi::free_cstring(third) };
}

/// `hew_process_last_error` transfers: it copies the thread-local message into
/// a fresh header-aware allocation and keeps nothing.
#[test]
fn process_last_error_result_is_transferred() {
    assert_result_is_transferred(
        "hew_process_last_error",
        induce_process_error,
        hew_process_last_error,
    );
}

/// `hew_stream_last_error` transfers: it TAKES the stored message and allocates
/// the returned C string from it. The take-and-clear shape is why `induce` runs
/// before every read — the answer to the ownership question is the same.
#[test]
fn stream_last_error_result_is_transferred() {
    assert_result_is_transferred(
        "hew_stream_last_error",
        induce_stream_error,
        hew_stream_last_error,
    );
}

/// The counterfactual. `hew_last_error` is the same family and the same slot —
/// `hew_process_last_error` reads it — but it returns the interior of the
/// `CString` the runtime keeps, so two consecutive results are the SAME
/// address. It fails R1, it is not the caller's to release, and no
/// `[[ownership.contracts]]` row may call it an owned transfer.
///
/// Without this the R1 probe could be vacuous: if every `-> string` export in
/// the process allocated afresh, "distinct addresses" would be a fact about the
/// allocator rather than about retention. Here is a real export where the probe
/// separates the two answers.
#[test]
fn hew_last_error_is_a_borrow_not_a_transfer() {
    induce_process_error();
    let first = hew_runtime::hew_last_error();
    let second = hew_runtime::hew_last_error();
    assert!(!first.is_null(), "the slot must hold a message");
    assert_eq!(
        first, second,
        "hew_last_error must hand back the same interior pointer twice — it is a \
         borrow into the runtime's thread-local, and the R1 probe must be able \
         to see the difference"
    );
    // Deliberately NOT released: the buffer is the runtime's. Reading it after
    // the next mutation would be the use-after-free the borrow contract warns
    // about, which is why the symbol carries `result = "borrowed"`.
    // SAFETY: `first` is a live NUL-terminated C string owned by the runtime.
    let borrowed = unsafe { CStr::from_ptr(first) }.to_owned();
    assert!(
        borrowed.to_bytes().starts_with(b"hew_process_run_args"),
        "the borrow must read back the message the runtime stored"
    );
}
