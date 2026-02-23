//! Atomic operation helpers.
//!
//! Thin wrappers providing named ordering constants for clarity.
//! Uses `std::sync::atomic` exclusively — no platform-specific intrinsics.

pub use std::sync::atomic::Ordering;

/// Cache line size for padding (64 bytes on x86/ARM, 128 on POWER).
#[cfg(target_arch = "powerpc64")]
pub const CACHE_LINE: usize = 128;

#[cfg(not(target_arch = "powerpc64"))]
pub const CACHE_LINE: usize = 64;
