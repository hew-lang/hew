//! Owned logical-failure transport for the private native Hew callable ABI.
//!
//! A successful call initializes its result and leaves the fault output null.
//! A failed call transfers one non-null fault owner instead; callers execute
//! their explicit SIR cleanup before propagating or disposing of that owner.
//! This module does not install a catch boundary or recover hardware faults.
//! The handle is opaque to generated code and is not a public embedding API.

use std::io::{self, Write};

use hew_cabi::string::{string_as_str, HewString};

use crate::internal::types::{ExitReason, HEW_TRAP_USER_PANIC};

/// Private completion codes kept distinct from source panic and trap codes.
pub const HEW_FAULT_CANCELLED: i32 = -1;
pub const HEW_FAULT_DEADLINE: i32 = -2;

/// An opaque, uniquely owned logical fault. Never free with a foreign allocator.
#[derive(Debug)]
pub struct HewFault {
    code: i32,
    message: Option<Box<str>>,
    secondary_diagnostics: String,
}

/// Create an owned logical-failure code. Unknown codes report as `UnknownFault`.
///
/// Returns one owner, released by [`hew_fault_drop`]. Allocation failure remains
/// process-fatal; this operation never converts allocator failure into a fault.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_fault_new(code: i32) -> *mut HewFault {
    Box::into_raw(Box::new(HewFault {
        code,
        message: None,
        secondary_diagnostics: String::new(),
    }))
}

/// Copy a borrowed managed string into one owned logical panic fault.
/// The caller may release the original string immediately after this returns.
/// Allocation failure remains process-fatal, as with [`hew_fault_new`].
///
/// # Safety
/// `message` must be a live managed string for this call; null means empty.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_new_panic(message: *const HewString) -> *mut HewFault {
    // SAFETY: the caller supplies a live length-carrying UTF-8 string borrow.
    let message = unsafe { string_as_str(message) }.into();
    Box::into_raw(Box::new(HewFault {
        code: HEW_TRAP_USER_PANIC,
        message: Some(message),
        secondary_diagnostics: String::new(),
    }))
}

/// Combine optional fault owners, preserving the primary code and message.
/// Secondary diagnostics are flattened in occurrence order into owned text.
/// This compiler-private operation does not read or change callable status.
/// Allocation failure remains process-fatal, as with [`hew_fault_new`].
///
/// # Safety
/// Each non-null argument must be a live, uniquely owned [`HewFault`], with no
/// outstanding borrows. Non-null arguments must be distinct. Both owners are
/// consumed; only the returned owner may subsequently be used or dropped.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_combine(
    primary: *mut HewFault,
    secondary: *mut HewFault,
) -> *mut HewFault {
    if primary.is_null() {
        return secondary;
    }
    if secondary.is_null() {
        return primary;
    }
    // SAFETY: the caller transfers distinct unique owners without live borrows.
    let primary_fault = unsafe { &mut *primary };
    // SAFETY: secondary is a distinct allocation consumed exactly once here.
    let secondary = unsafe { Box::from_raw(secondary) };
    let diagnostic = format!(
        "hew: secondary failure: {} ({})",
        fault_reason(secondary.code),
        secondary.code
    );
    primary_fault.secondary_diagnostics.push_str(&diagnostic);
    if let Some(message) = &secondary.message {
        primary_fault.secondary_diagnostics.push_str(": ");
        primary_fault.secondary_diagnostics.push_str(message);
    }
    primary_fault.secondary_diagnostics.push('\n');
    primary_fault
        .secondary_diagnostics
        .push_str(&secondary.secondary_diagnostics);
    primary
}

/// Consume one private logical fault into a public host error without reporting.
/// The diagnostic preserves the complete primary and secondary report bytes;
/// the public category is independent of internal trap-code numbering.
///
/// # Safety
/// `fault` is one live non-null unique owner, with no outstanding borrows.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_into_host_error(
    fault: *mut HewFault,
) -> *mut hew_cabi::host_error::HewError {
    use hew_cabi::host_error::{HewError, HostStatus};
    // SAFETY: the caller transfers a non-null unique fault allocation.
    let fault = unsafe { Box::from_raw(fault) };
    let mut diagnostic = Vec::new();
    // Writing to Vec cannot return an I/O error; allocation failure is fatal.
    let _ = write_report(&fault, &mut diagnostic);
    // SAFETY: write_report concatenates UTF-8 str components and ASCII formatting.
    // Taking the bytes preserves all text without replacement or a second copy.
    let message = unsafe { String::from_utf8_unchecked(diagnostic) };
    HewError::new(HostStatus::LogicalFault, message).into_raw()
}

/// Release one fault owner. Null is accepted for an empty fault output slot.
///
/// # Safety
/// A non-null pointer must be a live owner returned by [`hew_fault_new`] or
/// [`hew_fault_new_panic`] or [`hew_fault_combine`]. No
/// borrower may remain in use, and this owner must not have been released before.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_drop(fault: *mut HewFault) {
    if !fault.is_null() {
        // SAFETY: the caller transfers the unique allocation owner to us.
        drop(unsafe { Box::from_raw(fault) });
    }
}

/// Borrow a fault to report it to stderr; return 0 on success, 1 on I/O failure
/// or an absent fault. Reporting does not consume or replace the owner.
///
/// # Safety
/// A non-null pointer must refer to a live [`HewFault`] for the duration of this
/// call. No concurrent call may release it.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_report(fault: *const HewFault) -> i32 {
    // SAFETY: a non-null input is valid for this borrow by the caller's contract.
    let Some(fault) = (unsafe { fault.as_ref() }) else {
        return 1;
    };
    // Unlike eprintln!, an output error must not panic across this C boundary.
    i32::from(write_report(fault, &mut io::stderr().lock()).is_err())
}

fn fault_reason(code: i32) -> &'static str {
    if code == HEW_FAULT_CANCELLED {
        return "Cancelled";
    }
    if code == HEW_FAULT_DEADLINE {
        return "Deadline";
    }
    match ExitReason::from_error_code(code) {
        // A logical handle cannot make a hardware fault recoverable.
        ExitReason::Signal(_) | ExitReason::Normal => "UnknownFault",
        reason => reason.trap_kind_name(),
    }
}

fn write_report(fault: &HewFault, output: &mut impl Write) -> io::Result<()> {
    let code = fault.code;
    let reason = fault_reason(code);
    write!(output, "hew: failure: {reason} ({code})")?;
    if let Some(message) = &fault.message {
        output.write_all(b": ")?;
        output.write_all(message.as_bytes())?;
    }
    output.write_all(b"\n")?;
    output.write_all(fault.secondary_diagnostics.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn panic_fault(text: &str) -> *mut HewFault {
        let source = hew_cabi::string::string_from_str(text);
        // SAFETY: the constructor copies the live source borrow.
        let fault = unsafe { hew_fault_new_panic(source) };
        // SAFETY: the fault no longer borrows the source owner.
        unsafe { hew_cabi::string::string_release(source) };
        fault
    }

    #[test]
    fn combine_transfers_each_optional_owner() {
        for has_primary in [false, true] {
            for has_secondary in [false, true] {
                let primary = if has_primary {
                    hew_fault_new(202)
                } else {
                    std::ptr::null_mut()
                };
                let secondary = if has_secondary {
                    panic_fault("secondary")
                } else {
                    std::ptr::null_mut()
                };
                // SAFETY: these are distinct unique owners or null.
                let combined = unsafe { hew_fault_combine(primary, secondary) };
                assert_eq!(combined, if has_primary { primary } else { secondary });
                if !combined.is_null() {
                    // SAFETY: only the combined owner remains live.
                    let fault = unsafe { &*combined };
                    assert_eq!(
                        fault.code,
                        if has_primary {
                            202
                        } else {
                            HEW_TRAP_USER_PANIC
                        }
                    );
                    assert_eq!(
                        fault.message.as_deref(),
                        if has_primary { None } else { Some("secondary") }
                    );
                    assert_eq!(
                        fault.secondary_diagnostics,
                        if has_primary && has_secondary {
                            "hew: secondary failure: UserPanic (212): secondary\n"
                        } else {
                            ""
                        }
                    );
                }
                // SAFETY: transfer the sole remaining owner, including null.
                unsafe { hew_fault_drop(combined) };
            }
        }
    }

    #[test]
    fn nested_combine_preserves_primary_and_retryable_diagnostic_order() {
        let text = "primary\0é 🦀\nunchanged";
        let primary = panic_fault(text);
        // SAFETY: each combination consumes distinct unique allocations.
        let combined = unsafe {
            let primary = hew_fault_combine(primary, hew_fault_new(202));
            let secondary = hew_fault_combine(panic_fault("secondary\0雪\nend"), panic_fault(""));
            hew_fault_combine(primary, secondary)
        };
        assert_eq!(combined, primary);
        // SAFETY: combined is the sole live owner throughout these report borrows.
        let fault = unsafe { &*combined };
        assert_eq!(fault.code, HEW_TRAP_USER_PANIC);
        assert_eq!(fault.message.as_deref(), Some(text));
        let expected = format!(
            "hew: failure: UserPanic (212): {text}\n\
             hew: secondary failure: DivideByZero (202)\n\
             hew: secondary failure: UserPanic (212): secondary\0雪\nend\n\
             hew: secondary failure: UserPanic (212): \n"
        );
        let mut output = Vec::new();
        write_report(fault, &mut output).unwrap();
        assert_eq!(output, expected.as_bytes());
        // Exercise partial writes in both primary and secondary diagnostics,
        // including UTF-8, NUL and the explicitly empty secondary message.
        for limit in 0..output.len() {
            let mut buffer = vec![0; limit];
            assert_eq!(
                write_report(fault, &mut buffer.as_mut_slice())
                    .unwrap_err()
                    .kind(),
                io::ErrorKind::WriteZero
            );
            assert_eq!(buffer, output[..limit]);
            let mut retry = Vec::new();
            write_report(fault, &mut retry).unwrap();
            assert_eq!(retry, output);
        }
        // SAFETY: reports are nonconsuming; release the combined owner and text.
        unsafe { hew_fault_drop(combined) };
    }

    #[test]
    fn panic_owns_the_complete_message_after_the_source_is_released() {
        for text in ["", "before\0é 🦀\nafter"] {
            let source = hew_cabi::string::string_from_str(text);
            // SAFETY: the constructor borrows this live managed string.
            let fault = unsafe { hew_fault_new_panic(source) };
            // SAFETY: the constructor has copied the message; the source owner is ours.
            unsafe { hew_cabi::string::string_release(source) };
            let mut output = Vec::new();
            // SAFETY: the constructor returned a uniquely owned live fault.
            write_report(unsafe { &*fault }, &mut output).unwrap();
            assert_eq!(
                output,
                format!("hew: failure: UserPanic (212): {text}\n").as_bytes()
            );
            // Fail at every output boundary, including inside UTF-8 and after NUL.
            // A partial report must leave the fault available for another attempt.
            for limit in 0..output.len() {
                let mut buffer = vec![0; limit];
                // SAFETY: reporting only borrows the same live fault owner.
                let error =
                    write_report(unsafe { &*fault }, &mut buffer.as_mut_slice()).unwrap_err();
                assert_eq!(error.kind(), io::ErrorKind::WriteZero);
                assert_eq!(buffer, output[..limit]);
            }
            let mut retry = Vec::new();
            // SAFETY: failed reports neither release nor mutate the fault.
            write_report(unsafe { &*fault }, &mut retry).unwrap();
            assert_eq!(retry, output);
            // SAFETY: the test transfers its sole fault and message owner.
            unsafe { hew_fault_drop(fault) };
        }
    }

    #[test]
    fn absent_fault_is_not_reported_as_success() {
        // SAFETY: the ABI explicitly accepts an empty fault output slot.
        assert_eq!(unsafe { hew_fault_report(std::ptr::null()) }, 1);
    }

    #[test]
    fn logical_fault_report_preserves_canonical_reason_and_code() {
        let mut output = Vec::new();
        write_report(
            &HewFault {
                code: 202,
                message: None,
                secondary_diagnostics: String::new(),
            },
            &mut output,
        )
        .unwrap();
        assert_eq!(output, b"hew: failure: DivideByZero (202)\n");
    }

    #[test]
    fn unknown_fault_code_is_not_reported_as_a_hardware_signal() {
        for (code, expected) in [
            (-7, "hew: failure: UnknownFault (-7)\n"),
            (0, "hew: failure: UnknownFault (0)\n"),
        ] {
            let mut output = Vec::new();
            write_report(
                &HewFault {
                    code,
                    message: None,
                    secondary_diagnostics: String::new(),
                },
                &mut output,
            )
            .unwrap();
            assert_eq!(output, expected.as_bytes());
        }
    }

    #[test]
    fn report_write_failure_is_returned_without_panicking() {
        struct Unwritable;
        impl Write for Unwritable {
            fn write(&mut self, _: &[u8]) -> io::Result<usize> {
                Err(io::ErrorKind::BrokenPipe.into())
            }

            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }
        assert_eq!(
            write_report(
                &HewFault {
                    code: 202,
                    message: None,
                    secondary_diagnostics: String::new(),
                },
                &mut Unwritable
            )
            .unwrap_err()
            .kind(),
            io::ErrorKind::BrokenPipe
        );
    }
}
