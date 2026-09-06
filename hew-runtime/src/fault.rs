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

/// An opaque, uniquely owned logical fault. Never free with a foreign allocator.
#[derive(Debug)]
pub struct HewFault {
    code: i32,
    message: Option<Box<str>>,
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
    }))
}

/// Release one fault owner. Null is accepted for an empty fault output slot.
///
/// # Safety
/// A non-null pointer must be a live owner returned by [`hew_fault_new`] or
/// [`hew_fault_new_panic`]. No
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

fn write_report(fault: &HewFault, output: &mut impl Write) -> io::Result<()> {
    let code = fault.code;
    let reason = match ExitReason::from_error_code(code) {
        // This transport cannot certify that a raw integer represents a signal
        // or make a hardware fault recoverable by giving it a logical handle.
        ExitReason::Signal(_) | ExitReason::Normal => "UnknownFault",
        reason => reason.trap_kind_name(),
    };
    write!(output, "hew: failure: {reason} ({code})")?;
    if let Some(message) = &fault.message {
        output.write_all(b": ")?;
        output.write_all(message.as_bytes())?;
    }
    output.write_all(b"\n")
}

#[cfg(test)]
mod tests {
    use super::*;

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
                    message: None
                },
                &mut Unwritable
            )
            .unwrap_err()
            .kind(),
            io::ErrorKind::BrokenPipe
        );
    }
}
