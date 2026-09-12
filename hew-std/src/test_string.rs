//! Managed string owners for direct standard-library tests.
use hew_cabi::string::{string_from_str, string_release, HewString};

pub(crate) struct ManagedString(*mut HewString);

impl ManagedString {
    pub(crate) fn new(value: impl AsRef<str>) -> Self {
        Self(string_from_str(value.as_ref()))
    }

    pub(crate) fn as_ptr(&self) -> *const HewString {
        self.0
    }
}

impl Drop for ManagedString {
    fn drop(&mut self) {
        // SAFETY: this fixture owns one reference until the balancing release.
        unsafe { string_release(self.0) };
    }
}
