//! Shared helpers for poison-recovery on `std::sync` primitives.
//!
//! The Hew runtime recovers from poisoned locks rather than panicking,
//! because a panicked thread should not cascade-crash independent actors.

#[cfg(not(target_arch = "wasm32"))]
use std::sync::{Condvar, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::sync::{Mutex, MutexGuard, PoisonError};
#[cfg(not(target_arch = "wasm32"))]
use std::time::Duration;

/// Build a JSON array by writing each element directly into the output string.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn json_array<I, F>(items: I, mut write_item: F) -> String
where
    I: IntoIterator,
    F: FnMut(&mut String, I::Item),
{
    let mut json = String::from("[");
    let mut first = true;
    for item in items {
        if first {
            first = false;
        } else {
            json.push(',');
        }
        write_item(&mut json, item);
    }
    json.push(']');
    json
}

/// Push a JSON string value into `out`, escaping control characters as needed.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn push_json_string(out: &mut String, s: &str) {
    use std::fmt::Write as _;

    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => {
                let _ = write!(out, "\\u{:04x}", ch as u32);
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
}

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
#[cfg(not(target_arch = "wasm32"))]
pub(crate) trait RwLockExt<T> {
    fn read_or_recover(&self) -> RwLockReadGuard<'_, T>;
    fn write_or_recover(&self) -> RwLockWriteGuard<'_, T>;
}

#[cfg(not(target_arch = "wasm32"))]
impl<T> RwLockExt<T> for RwLock<T> {
    fn read_or_recover(&self) -> RwLockReadGuard<'_, T> {
        self.read().unwrap_or_else(PoisonError::into_inner)
    }

    fn write_or_recover(&self) -> RwLockWriteGuard<'_, T> {
        self.write().unwrap_or_else(PoisonError::into_inner)
    }
}

/// Extension trait for [`Condvar`] that recovers from poisoned waits.
#[cfg(not(target_arch = "wasm32"))]
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
#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_array_empty_returns_empty_array() {
        let json = json_array(std::iter::empty::<i32>(), |_, _| unreachable!());
        assert_eq!(json, "[]");
    }

    #[test]
    fn json_array_multiple_items_inserts_commas_between_items() {
        use std::fmt::Write as _;

        let json = json_array([1, 2, 3], |out, value| {
            let _ = write!(out, "{value}");
        });

        assert_eq!(json, "[1,2,3]");
    }

    #[test]
    fn push_json_string_escapes_json_metacharacters_and_controls() {
        let mut json = String::new();
        push_json_string(&mut json, "quo\"te\\slash\nline\rreturn\t\x1f");
        assert_eq!(json, r#""quo\"te\\slash\nline\rreturn\t\u001f""#);
    }
}
