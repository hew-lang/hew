//! Shared helpers for poison-recovery on `std::sync` primitives.
//!
//! The Hew runtime recovers from poisoned locks rather than panicking,
//! because a panicked thread should not cascade-crash independent actors.

use std::sync::{
    Condvar, Mutex, MutexGuard, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use std::time::Duration;

/// Extension trait for [`Mutex`] that recovers from poisoned locks.
pub(crate) trait MutexExt<T> {
    fn lock_or_recover(&self) -> MutexGuard<'_, T>;
}

impl<T> MutexExt<T> for Mutex<T> {
    fn lock_or_recover(&self) -> MutexGuard<'_, T> {
        self.lock().unwrap_or_else(PoisonError::into_inner)
    }
}

/// Extension trait for [`RwLock`] that recovers from poisoned locks.
pub(crate) trait RwLockExt<T> {
    fn read_or_recover(&self) -> RwLockReadGuard<'_, T>;
    fn write_or_recover(&self) -> RwLockWriteGuard<'_, T>;
}

impl<T> RwLockExt<T> for RwLock<T> {
    fn read_or_recover(&self) -> RwLockReadGuard<'_, T> {
        self.read().unwrap_or_else(PoisonError::into_inner)
    }

    fn write_or_recover(&self) -> RwLockWriteGuard<'_, T> {
        self.write().unwrap_or_else(PoisonError::into_inner)
    }
}

/// Extension trait for [`Condvar`] that recovers from poisoned waits.
pub(crate) trait CondvarExt {
    fn wait_or_recover<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T>;

    fn wait_timeout_or_recover<'a, T>(
        &self,
        guard: MutexGuard<'a, T>,
        dur: Duration,
    ) -> (MutexGuard<'a, T>, std::sync::WaitTimeoutResult);
}

/// Convert a C string pointer to a Rust `&str`, setting last error on failure.
///
/// # Safety
///
/// `ptr` must be a valid, null-terminated C string or null.
pub(crate) unsafe fn cstr_to_str<'a>(
    ptr: *const std::ffi::c_char,
    context: &str,
) -> Option<&'a str> {
    if ptr.is_null() {
        crate::set_last_error(format!("{context}: null pointer"));
        return None;
    }
    // SAFETY: Caller guarantees ptr is a valid NUL-terminated C string.
    if let Ok(s) = unsafe { std::ffi::CStr::from_ptr(ptr) }.to_str() {
        Some(s)
    } else {
        crate::set_last_error(format!("{context}: invalid UTF-8"));
        None
    }
}

impl CondvarExt for Condvar {
    fn wait_or_recover<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T> {
        self.wait(guard).unwrap_or_else(PoisonError::into_inner)
    }

    fn wait_timeout_or_recover<'a, T>(
        &self,
        guard: MutexGuard<'a, T>,
        dur: Duration,
    ) -> (MutexGuard<'a, T>, std::sync::WaitTimeoutResult) {
        self.wait_timeout(guard, dur)
            .unwrap_or_else(PoisonError::into_inner)
    }
}
