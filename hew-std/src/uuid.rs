//! Hew runtime: `uuid_gen` module.
//!
//! Provides UUID v4 and v7 generation for compiled Hew programs. Generated
//! identifiers are managed strings: the caller receives one owner and
//! releases it with `hew_string_drop`.
use hew_cabi::string::{string_from_str, HewString};
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

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::string::{string_as_str, string_release};

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
}
