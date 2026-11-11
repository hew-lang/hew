//! Owners for the managed strings the std FFI borrows and returns.
//!
//! hew-runtime installs the global allocator, so these targets bound memory
//! through libFuzzer's RSS limit rather than a heap meter. Every result is
//! released through its documented contract: a leak here would surface as a
//! false out-of-memory after a few minutes.

#![allow(
    dead_code,
    reason = "shared by every fuzz target; each target uses a subset"
)]

use hew_cabi::string::{string_as_str, string_from_str, string_release, HewString};

/// One borrowed-input owner, released on drop.
pub struct Managed(*mut HewString);

impl Managed {
    pub fn new(text: &str) -> Self {
        Self(string_from_str(text))
    }

    /// Adopt an owned managed-string result (null is the empty string).
    pub fn adopt(result: *mut HewString) -> Self {
        Self(result)
    }

    pub fn ptr(&self) -> *const HewString {
        self.0
    }

    pub fn text(&self) -> &str {
        // SAFETY: this owner keeps the string live for the borrow.
        unsafe { string_as_str(self.0) }
    }
}

impl Drop for Managed {
    fn drop(&mut self) {
        // SAFETY: this owner holds the one reference `string_from_str` minted.
        unsafe { string_release(self.0) };
    }
}

/// Copy out and release an owned managed-string result (null is empty).
pub fn take(result: *mut HewString) -> String {
    // SAFETY: `result` is null or an owned, live managed string.
    let text = unsafe { string_as_str(result) }.to_owned();
    // SAFETY: the caller transferred its only reference.
    unsafe { string_release(result) };
    text
}

/// Split `data` into a selector byte and UTF-8 text, or `None`.
pub fn selector_text(data: &[u8]) -> Option<(u8, &str)> {
    let (&selector, rest) = data.split_first()?;
    Some((selector, std::str::from_utf8(rest).ok()?))
}

/// Split UTF-8 `text` at the first NUL into two fields.
pub fn two_fields(text: &str) -> (&str, &str) {
    text.split_once('\0').unwrap_or((text, ""))
}
