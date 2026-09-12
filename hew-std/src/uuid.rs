//! Hew runtime: `uuid_gen` module.
//!
//! Provides UUID v4 and v7 generation and validation for compiled Hew
//! programs. Generated identifiers are managed strings: the caller receives one
//! owner and releases it with `hew_string_drop`.
use hew_cabi::string::{string_as_str, string_from_str, HewString};
use uuid::Uuid;

/// Generate a UUID v4 (random) string.
///
/// Returns one owned managed string holding the hyphenated UUID (36 bytes).
/// Release it with `hew_string_drop`.
#[no_mangle]
pub extern "C" fn hew_uuid_v4() -> *mut HewString {
    string_from_str(&Uuid::new_v4().to_string())
}

/// Generate a UUID v7 (time-ordered, random) string.
///
/// Returns one owned managed string holding the hyphenated UUID (36 bytes).
/// Release it with `hew_string_drop`.
#[no_mangle]
pub extern "C" fn hew_uuid_v7() -> *mut HewString {
    string_from_str(&Uuid::now_v7().to_string())
}

/// Validate a UUID string.
///
/// Returns `1` if the text is a valid UUID, `0` otherwise.
///
/// # Safety
///
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_uuid_parse(s: *const HewString) -> i32 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let text = unsafe { string_as_str(s) };
    i32::from(Uuid::parse_str(text).is_ok())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    fn read_and_release(value: *mut HewString) -> String {
        assert!(
            !value.is_null(),
            "a generated UUID is never the empty string"
        );
        // SAFETY: `value` is the live owner returned by the producer.
        let text = unsafe { string_as_str(value) }.to_string();
        // SAFETY: this test holds the only owner of `value`.
        unsafe { string_release(value) };
        text
    }

    #[test]
    fn test_uuid_v4_format() {
        let text = read_and_release(hew_uuid_v4());
        assert_eq!(text.len(), 36);
        assert!(Uuid::parse_str(&text).is_ok());
    }

    #[test]
    fn test_uuid_v7_format() {
        let text = read_and_release(hew_uuid_v7());
        assert_eq!(text.len(), 36);
        let parsed = Uuid::parse_str(&text).unwrap();
        assert_eq!(parsed.get_version(), Some(uuid::Version::SortRand));
    }

    #[test]
    fn test_uuid_parse_valid_and_invalid() {
        let valid = ManagedString::new("550e8400-e29b-41d4-a716-446655440000");
        // SAFETY: `valid` owns a live managed string for this call.
        assert_eq!(unsafe { hew_uuid_parse(valid.as_ptr()) }, 1);

        let invalid = ManagedString::new("not-a-uuid");
        // SAFETY: `invalid` owns a live managed string for this call.
        assert_eq!(unsafe { hew_uuid_parse(invalid.as_ptr()) }, 0);

        // SAFETY: null is the canonical empty string and parses as invalid.
        assert_eq!(unsafe { hew_uuid_parse(std::ptr::null()) }, 0);
    }

    /// FFI signature-parity guard (finding FFI-1).
    ///
    /// `hew_uuid_parse` returns `i32` on the Rust side (`#[no_mangle]` above).
    /// The Hew binding in `uuid.hew` must declare the same return type, or the
    /// compiled call reads a 32-bit value through a 1-byte `bool` ABI and gets
    /// garbage in the high bytes. This test parses the `.hew` extern block and
    /// fails closed if its declared return type is anything other than `-> i32`,
    /// so any drift back to `bool` (or any other type) goes red here.
    #[test]
    fn hew_binding_declares_uuid_parse_returns_i32() {
        let hew_src = include_str!("../../std/misc/uuid/uuid.hew");

        // Locate the `hew_uuid_parse` extern declaration line.
        let decl = hew_src
            .lines()
            .map(str::trim)
            .find(|line| line.starts_with("fn hew_uuid_parse"))
            .expect("uuid.hew must declare an extern `fn hew_uuid_parse`");

        // Strip any trailing line comment before inspecting the signature.
        let signature = decl.split("//").next().unwrap_or(decl).trim();

        assert!(
            signature.contains("-> i32"),
            "uuid.hew binding for hew_uuid_parse must return i32 to match the \
             Rust `#[no_mangle] -> i32` signature; found: {signature:?}"
        );
        // The mismatched `bool` declaration must never return.
        assert!(
            !signature.contains("-> bool"),
            "uuid.hew binding for hew_uuid_parse must not declare `-> bool` \
             (Rust returns i32); found: {signature:?}"
        );
    }
}
