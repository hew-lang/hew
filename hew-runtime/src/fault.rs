//! Owned logical-failure transport for the private native Hew callable ABI.
//!
//! A successful call initializes its result and leaves the fault output null.
//! A failed call transfers one non-null fault owner instead; callers execute
//! their explicit SIR cleanup before propagating or disposing of that owner.
//! This module does not install a catch boundary or recover hardware faults.
//! The handle is opaque to generated code and is not a public embedding API.

use std::io::{self, Write};

use crate::internal::types::ExitReason;

/// An opaque, uniquely owned logical fault. Never free with a foreign allocator.
#[derive(Debug)]
pub struct HewFault {
    code: i32,
}

/// Create an owned logical-failure code. Unknown codes report as `UnknownFault`.
///
/// Returns one owner, released by [`hew_fault_drop`]. Allocation failure remains
/// process-fatal; this operation never converts allocator failure into a fault.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_fault_new(code: i32) -> *mut HewFault {
    Box::into_raw(Box::new(HewFault { code }))
}

/// Release one fault owner. Null is accepted for an empty fault output slot.
///
/// # Safety
/// A non-null pointer must be a live owner returned by [`hew_fault_new`]. No
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
    i32::from(write_report(fault.code, &mut io::stderr().lock()).is_err())
}

fn write_report(code: i32, output: &mut impl Write) -> io::Result<()> {
    let reason = match ExitReason::from_error_code(code) {
        // This transport cannot certify that a raw integer represents a signal
        // or make a hardware fault recoverable by giving it a logical handle.
        ExitReason::Signal(_) | ExitReason::Normal => "UnknownFault",
        reason => reason.trap_kind_name(),
    };
    writeln!(output, "hew: failure: {reason} ({code})")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn absent_fault_is_not_reported_as_success() {
        // SAFETY: the ABI explicitly accepts an empty fault output slot.
        assert_eq!(unsafe { hew_fault_report(std::ptr::null()) }, 1);
    }

    #[test]
    fn logical_fault_report_preserves_canonical_reason_and_code() {
        let mut output = Vec::new();
        write_report(202, &mut output).unwrap();
        assert_eq!(output, b"hew: failure: DivideByZero (202)\n");
    }

    #[test]
    fn unknown_fault_code_is_not_reported_as_a_hardware_signal() {
        for (code, expected) in [
            (-7, "hew: failure: UnknownFault (-7)\n"),
            (0, "hew: failure: UnknownFault (0)\n"),
        ] {
            let mut output = Vec::new();
            write_report(code, &mut output).unwrap();
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
            write_report(202, &mut Unwritable).unwrap_err().kind(),
            io::ErrorKind::BrokenPipe
        );
    }
}
