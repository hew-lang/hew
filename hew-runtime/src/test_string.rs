//! Managed string owners for runtime unit tests.
use hew_cabi::string::{string_from_str, string_release, HewString};

pub(crate) struct ManagedString(*mut HewString);
// SAFETY: the fixture exposes only immutable bytes and owns an atomic reference.
unsafe impl Send for ManagedString {}
// SAFETY: shared access only borrows immutable managed string storage.
unsafe impl Sync for ManagedString {}
impl ManagedString {
    pub(crate) fn new(value: impl AsRef<str>) -> Self {
        Self(string_from_str(value.as_ref()))
    }
    pub(crate) fn as_ptr(&self) -> *const HewString {
        self.0
    }
}
impl Clone for ManagedString {
    fn clone(&self) -> Self {
        // SAFETY: self holds a live owner; the clone acquires its own reference.
        Self(unsafe { hew_cabi::string::string_retain(self.0) })
    }
}
impl Drop for ManagedString {
    fn drop(&mut self) {
        // SAFETY: the fixture holds one owner until this balancing release.
        unsafe { string_release(self.0) };
    }
}
