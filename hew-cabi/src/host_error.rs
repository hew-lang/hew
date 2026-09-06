//! Shared error owners for the experimental, synchronous public host API.
//! The public header exposes neither this representation nor private trap codes.

use crate::string::HewString;

/// The public text owner uses the canonical managed-string allocation.
pub type HewText = HewString;

/// Public status codes mirrored by `include/hew_host.h` and exercised by C clients.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostStatus {
    /// Success, with no error owner.
    Ok = 0,
    /// Input bytes are not UTF-8.
    InvalidUtf8 = 1,
    /// UTF-8 input is not a JSON document.
    InvalidJson = 2,
    /// An object does not contain the requested field.
    MissingField = 3,
    /// The value or field has the wrong JSON kind.
    WrongKind = 4,
    /// A value could not be encoded as JSON.
    EncodeError = 5,
    /// A compiled Hew call failed logically, without a foreign unwind.
    LogicalFault = 6,
}

/// An opaque, independent error owner. Release with the host API, never `free`.
#[derive(Debug)]
pub struct HewError {
    code: HostStatus,
    message: String,
}

impl HewError {
    /// Construct an owned diagnostic; callers must supply a non-success code.
    #[must_use]
    pub fn new(code: HostStatus, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }

    /// The stable host status, independent of private runtime trap codes.
    #[must_use]
    pub fn code(&self) -> i32 {
        self.code as i32
    }

    /// Borrow all diagnostic bytes, including embedded NUL and newlines.
    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Transfer this independent owner to a foreign caller.
    #[must_use]
    pub fn into_raw(self) -> *mut Self {
        Box::into_raw(Box::new(self))
    }
}

/// Validate a borrowed pointer-plus-length UTF-8 input, accepting null + zero.
///
/// # Errors
/// Returns an independently owned `InvalidUtf8` error for malformed UTF-8.
///
/// # Safety
/// Nonempty inputs name readable storage of `len <= isize::MAX` bytes, live
/// throughout the returned borrow. Empty inputs need no readable storage.
pub unsafe fn read_utf8<'a>(data: *const u8, len: usize) -> Result<&'a str, HewError> {
    let bytes = if len == 0 {
        &[]
    } else {
        // SAFETY: the caller establishes a live, bounded byte range.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    std::str::from_utf8(bytes)
        .map_err(|err| HewError::new(HostStatus::InvalidUtf8, err.to_string()))
}

/// Publish exactly one result or error owner through distinct empty output slots.
///
/// # Safety
/// `out` and `error` name distinct writable, initially null pointer slots which
/// do not overlap any live input. A successful result contains a non-null owner.
pub unsafe fn publish<T>(
    result: Result<*mut T, HewError>,
    out: *mut *mut T,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller supplies distinct writable pointer slots.
    unsafe {
        *out = std::ptr::null_mut();
        *error = std::ptr::null_mut();
    }
    match result {
        Ok(value) => {
            // SAFETY: out is a writable empty slot, receiving one owner.
            unsafe {
                *out = value;
            }
            HostStatus::Ok as i32
        }
        Err(failure) => {
            let code = failure.code();
            // SAFETY: error is a writable empty slot, receiving one owner.
            unsafe {
                *error = failure.into_raw();
            }
            code
        }
    }
}

/// Publish a mutation outcome without manufacturing a value owner.
///
/// # Safety
/// `error` is a writable initially null pointer slot, disjoint from inputs.
pub unsafe fn publish_unit(result: Result<(), HewError>, error: *mut *mut HewError) -> i32 {
    // SAFETY: the caller supplies a writable empty error slot.
    unsafe {
        *error = std::ptr::null_mut();
    }
    match result {
        Ok(()) => HostStatus::Ok as i32,
        Err(failure) => {
            let code = failure.code();
            // SAFETY: the caller supplies a writable empty error slot.
            unsafe {
                *error = failure.into_raw();
            }
            code
        }
    }
}
