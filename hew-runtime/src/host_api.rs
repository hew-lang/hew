//! Experimental synchronous host text/error façade. No scheduler is initialized.
//! Pointer preconditions and ownership are declared in `hew_host.h`.

use hew_cabi::host_error::{publish, read_utf8, HewError, HewText};
use hew_cabi::string::{
    string_as_bytes, string_from_str_nonnull, string_into_nonnull, string_release,
};

static EMPTY_VIEW: [u8; 1] = [0];

/// Normalize a consumed compiler-owned string into a non-null public text owner.
/// This is a compiler-private adapter, not a declaration in `hew_host.h`.
///
/// # Safety
/// `text` is null (canonical empty) or one live managed-string owner whose
/// release obligation is transferred here. No outstanding borrow may remain.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_host_text_from_owned(text: *mut HewText) -> *mut HewText {
    // SAFETY: the caller transfers the optional canonical managed-string owner.
    unsafe { string_into_nonnull(text) }
}

/// Copy borrowed UTF-8 into a non-null text owner, including empty text.
///
/// # Safety
/// Inputs obey `read_utf8`; output slots obey `publish` and do not alias inputs.
#[no_mangle]
pub unsafe extern "C" fn hew_host_text_from_utf8(
    data: *const u8,
    len: usize,
    out: *mut *mut HewText,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller supplies a live byte range and distinct empty slots.
    unsafe {
        publish(
            read_utf8(data, len).map(string_from_str_nonnull),
            out,
            error,
        )
    }
}

/// Borrow the complete text bytes until this owner is released.
///
/// # Safety
/// `text` is a live non-null host text; `len` is writable and disjoint from it.
#[no_mangle]
pub unsafe extern "C" fn hew_host_text_data(text: *const HewText, len: *mut usize) -> *const u8 {
    // SAFETY: the caller borrows a live managed-string owner.
    let bytes = unsafe { string_as_bytes(text) };
    // SAFETY: the caller supplies a disjoint writable extent slot.
    unsafe {
        *len = bytes.len();
    }
    // Give empty views readable sentinel storage, not a one-past-header pointer.
    if bytes.is_empty() {
        EMPTY_VIEW.as_ptr()
    } else {
        bytes.as_ptr()
    }
}

/// Release one host text owner. Null is accepted.
///
/// # Safety
/// A non-null input is a unique release obligation with no outstanding borrows.
#[no_mangle]
pub unsafe extern "C" fn hew_host_text_release(text: *mut HewText) {
    // SAFETY: the caller transfers one canonical managed-string owner or null.
    unsafe {
        string_release(text);
    }
}

/// Borrow all error bytes until the error is released; never print or consume it.
///
/// # Safety
/// `error` is live and non-null; `len` is writable and disjoint from the owner.
#[no_mangle]
pub unsafe extern "C" fn hew_host_error_message(
    error: *const HewError,
    len: *mut usize,
) -> *const u8 {
    // SAFETY: the caller borrows a live opaque error.
    let bytes = unsafe { &*error }.message().as_bytes();
    // SAFETY: the caller supplies a writable extent slot.
    unsafe {
        *len = bytes.len();
    }
    if bytes.is_empty() {
        EMPTY_VIEW.as_ptr()
    } else {
        bytes.as_ptr()
    }
}

/// Inspect the public error code without consuming the owner.
///
/// # Safety
/// `error` must be a live non-null error owner for this borrow.
#[no_mangle]
pub unsafe extern "C" fn hew_host_error_code(error: *const HewError) -> i32 {
    // SAFETY: the caller borrows a live error.
    unsafe { &*error }.code()
}

/// Release one error owner and its complete diagnostic text. Null is accepted.
///
/// # Safety
/// A non-null input transfers one unique owner with no outstanding borrows.
#[no_mangle]
pub unsafe extern "C" fn hew_host_error_release(error: *mut HewError) {
    if !error.is_null() {
        // SAFETY: the caller transfers the unique error allocation.
        drop(unsafe { Box::from_raw(error) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fault::{hew_fault_combine, hew_fault_into_host_error, hew_fault_new_panic};
    use hew_cabi::host_error::HostStatus;
    use hew_cabi::string::string_from_str;

    #[test]
    fn text_owns_utf8_nul_and_empty_and_errors_survive_later_calls() {
        // SAFETY: all inputs are live byte slices, all output slots are distinct
        // and empty, and each returned owner is released after its final borrow.
        unsafe {
            let mut error = std::ptr::null_mut();
            let mut text = std::ptr::null_mut();
            assert_eq!(
                hew_host_text_from_utf8(b"\xff".as_ptr(), 1, &raw mut text, &raw mut error),
                HostStatus::InvalidUtf8 as i32
            );
            assert!(text.is_null());
            assert!(!error.is_null());
            let mut len = 0;
            let diagnostic =
                std::slice::from_raw_parts(hew_host_error_message(error, &raw mut len), len)
                    .to_vec();
            for input in ["", "hé\0雪\nend"] {
                let mut source = input.as_bytes().to_vec();
                let mut next_error = std::ptr::null_mut();
                assert_eq!(
                    hew_host_text_from_utf8(
                        source.as_ptr(),
                        source.len(),
                        &raw mut text,
                        &raw mut next_error
                    ),
                    0
                );
                assert!(next_error.is_null());
                assert!(!text.is_null());
                source.fill(b'x');
                drop(source);
                let view = hew_host_text_data(text, &raw mut len);
                assert_eq!(std::slice::from_raw_parts(view, len), input.as_bytes());
                if input.is_empty() {
                    assert_eq!(*view, 0);
                }
                hew_host_text_release(text);
                text = std::ptr::null_mut();
            }
            assert_eq!(hew_host_error_code(error), HostStatus::InvalidUtf8 as i32);
            assert_eq!(
                std::slice::from_raw_parts(hew_host_error_message(error, &raw mut len), len),
                diagnostic
            );
            hew_host_error_release(error);
            // The compiler's canonical null-empty result becomes a normal owner.
            let normalized = hew_host_text_from_owned(std::ptr::null_mut());
            assert!(!normalized.is_null());
            hew_host_text_release(normalized);
            let canonical = string_from_str("transfer\0雪");
            let normalized = hew_host_text_from_owned(canonical);
            assert_eq!(normalized, canonical);
            assert_eq!(string_as_bytes(normalized), "transfer\0雪".as_bytes());
            hew_host_text_release(normalized);
            hew_host_text_release(std::ptr::null_mut());
            hew_host_error_release(std::ptr::null_mut());
        }
    }

    #[test]
    fn consuming_fault_bridge_preserves_all_diagnostics_as_one_host_error() {
        // SAFETY: every constructor borrows live text, every combine consumes
        // distinct unique faults, and conversion leaves only one error owner.
        unsafe {
            let primary_text = string_from_str("primary\0é\nend");
            let secondary_text = string_from_str("secondary\0雪");
            let primary = hew_fault_new_panic(primary_text);
            let secondary = hew_fault_combine(
                hew_fault_new_panic(secondary_text),
                hew_fault_new_panic(std::ptr::null()),
            );
            string_release(primary_text);
            string_release(secondary_text);
            let error = hew_fault_into_host_error(hew_fault_combine(primary, secondary));
            assert!(!error.is_null());
            assert_eq!(hew_host_error_code(error), HostStatus::LogicalFault as i32);
            let expected = "hew: failure: UserPanic (212): primary\0é\nend\n\
                hew: secondary failure: UserPanic (212): secondary\0雪\n\
                hew: secondary failure: UserPanic (212): \n";
            for _ in 0..2 {
                let mut len = 0;
                let view = hew_host_error_message(error, &raw mut len);
                assert_eq!(std::slice::from_raw_parts(view, len), expected.as_bytes());
            }
            hew_host_error_release(error);
        }
    }
}
