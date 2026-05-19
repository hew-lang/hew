//! Duration helper ABI shared by native and WASM runtime builds.

use std::ffi::c_int;

/// Duration value exchanged with C code.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewDuration {
    /// Milliseconds.
    pub ms: u64,
}

/// Create a [`HewDuration`] from whole seconds.
///
/// # Safety
///
/// No preconditions — pure arithmetic.
#[no_mangle]
pub unsafe extern "C" fn hew_seconds(s: c_int) -> HewDuration {
    HewDuration {
        ms: u64::from(s.cast_unsigned()).wrapping_mul(1000),
    }
}

/// Create a [`HewDuration`] from milliseconds.
///
/// # Safety
///
/// No preconditions — pure arithmetic.
#[no_mangle]
pub unsafe extern "C" fn hew_milliseconds(ms: c_int) -> HewDuration {
    HewDuration {
        ms: u64::from(ms.cast_unsigned()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seconds_positive_converts_to_milliseconds() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_seconds(5) };
        assert_eq!(d.ms, 5000);
    }

    #[test]
    fn seconds_negative_matches_native_unsigned_cast_semantics() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_seconds(-1) };
        assert_eq!(d.ms, u64::from(u32::MAX).wrapping_mul(1000));
    }

    #[test]
    fn milliseconds_positive_passes_through() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_milliseconds(42) };
        assert_eq!(d.ms, 42);
    }

    #[test]
    fn milliseconds_negative_matches_native_unsigned_cast_semantics() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_milliseconds(-1) };
        assert_eq!(d.ms, u64::from(u32::MAX));
    }
}
