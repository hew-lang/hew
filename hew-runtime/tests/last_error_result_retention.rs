//! Managed error results transfer ownership independently of the error slot.
//! Two live reads own independent allocations. Releasing one must preserve the
//! other, and releasing both must leave the producer usable. Sanitizer runs
//! check the balancing releases. The generic `hew_last_error` diagnostic remains
//! a borrowed foreign C string and supplies the contrasting lifetime case.

use hew_cabi::string::{string_as_str, string_release};
use std::ffi::CStr;

use hew_runtime::process::hew_process_last_error;
use hew_runtime::stream_error::hew_stream_last_error;

/// Record an error in the runtime's thread-local process-error slot without
/// touching the filesystem or spawning anything: a negative `argc` is rejected
/// before any argument is read.
fn induce_process_error() {
    // SAFETY: `argc` is negative, so the export returns before dereferencing
    // `cmd` or `args`; the pointers are valid regardless.
    unsafe {
        hew_runtime::process::hew_process_run_args(std::ptr::null(), std::ptr::null(), -1);
    }
}

/// Record an error in the runtime's stream/sink slot. `hew_stream_last_error`
/// TAKES the message, so this runs before every read rather than once.
fn induce_stream_error() {
    let msg = b"hew-2828-oracle: stream";
    // SAFETY: `msg` is a valid initialized byte range of the stated length.
    unsafe { hew_runtime::stream_error::hew_stream_set_last_error(msg.as_ptr(), msg.len()) };
}

/// Check independent ownership against one `-> string` export and assert the result is the
/// caller's to release. `induce` runs before every read so a take-and-clear
/// slot (`hew_stream_last_error`) and a clone-on-read slot
/// (`hew_process_last_error`) are measured on the same terms.
fn assert_result_is_transferred(
    symbol: &str,
    induce: fn(),
    call: unsafe extern "C" fn() -> *mut hew_cabi::string::HewString,
) {
    induce();
    // SAFETY: the export takes no arguments and returns null or a managed
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

    // SAFETY: `first` is a live managed Hew string.
    let text = unsafe { string_as_str(first) }.to_owned();

    // R3 — release both through the release symbol the contract names, then
    // read again: the callee's own state must be untouched by the free.
    // SAFETY: each pointer is a live, solely-owned managed Hew string
    // transferred by its getter, so this is its balancing release.
    unsafe {
        string_release(first);
        assert_eq!(string_as_str(second), text);
        string_release(second);
    }
    induce();
    // SAFETY: as above.
    let third = unsafe { call() };
    assert!(
        !third.is_null(),
        "{symbol}: the slot did not survive the release"
    );
    // SAFETY: `third` is a live managed Hew string.
    let after = unsafe { string_as_str(third) }.to_owned();
    assert_eq!(
        text, after,
        "{symbol}: the message changed after the caller released an earlier \
         result, so the callee retained a pointer into the freed buffer"
    );
    // SAFETY: `third` is live and solely owned.
    unsafe { string_release(third) };
}

/// `hew_process_last_error` transfers: it copies the thread-local message into
/// a fresh managed allocation and keeps nothing.
#[test]
fn process_last_error_result_is_transferred() {
    induce_process_error();
    let first = hew_process_last_error();
    let second = hew_process_last_error();
    // SAFETY: each error read transfers a managed owner independent of TLS.
    unsafe {
        let expected = string_as_str(first).to_owned();
        hew_runtime::hew_clear_error();
        assert_eq!(string_as_str(second), expected);
        string_release(first);
        assert_eq!(string_as_str(second), expected);
        string_release(second);
    }
}

/// `hew_stream_last_error` transfers: it TAKES the stored message and allocates
/// the returned managed string from it. The take-and-clear shape is why `induce` runs
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
