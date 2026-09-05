use hew_cabi::string::{
    cstring_copy_release, cstring_from_string_copy, string_as_bytes, string_as_str,
    string_from_cstr_copy, string_from_str, string_from_utf8, string_release, string_retain,
    string_to_cstring, StringFromCStrError,
};
use std::ffi::CString;

#[test]
fn embedded_nul_round_trips_through_managed_string() {
    let value = string_from_str("a\0b");

    // SAFETY: `value` is a live managed string for the duration of the borrows.
    unsafe {
        assert_eq!(string_as_bytes(value), b"a\0b");
        assert_eq!(string_as_str(value), "a\0b");
        string_release(value);
    }
}

#[test]
fn empty_string_uses_the_canonical_null_handle() {
    let value = string_from_str("");

    assert!(value.is_null());
    // SAFETY: null is the canonical live representation of the empty string.
    unsafe {
        assert_eq!(string_as_bytes(value), b"");
        assert_eq!(string_as_str(value), "");
        assert!(string_retain(value).is_null());
        string_release(value);
    }
}

#[test]
fn retained_handle_survives_releasing_the_original_owner() {
    let value = string_from_str("é\0中");
    // SAFETY: `value` is a live managed string owner.
    let retained = unsafe { string_retain(value) };
    // SAFETY: the first release leaves the retained owner live.
    unsafe { string_release(value) };

    // SAFETY: `retained` remains a live owner until the final release below.
    unsafe {
        assert_eq!(string_as_str(retained), "é\0中");
        string_release(retained);
    }
}

#[test]
fn invalid_utf8_is_rejected_without_creating_a_managed_value() {
    let error = string_from_utf8(&[0xf0, 0x28, 0x8c, 0x28])
        .expect_err("malformed UTF-8 must not enter the managed string carrier");

    assert_eq!(error.valid_up_to(), 0);
}

#[test]
fn foreign_cstr_is_validated_and_copied_into_managed_storage() {
    let input = CString::new("héllo").unwrap();
    // SAFETY: `input` is a live NUL-terminated C string for this call.
    let value = unsafe { string_from_cstr_copy(input.as_ptr()) }.unwrap();

    // SAFETY: `value` is a live managed string until the release below.
    unsafe {
        assert_eq!(string_as_str(value), "héllo");
        string_release(value);
    }
}

#[test]
fn invalid_foreign_cstr_utf8_is_rejected() {
    let input = [0xff_u8, 0];
    // SAFETY: `input` is NUL-terminated and readable for this call.
    let error = unsafe { string_from_cstr_copy(input.as_ptr().cast()) }
        .expect_err("invalid foreign UTF-8 must not enter managed storage");

    assert_eq!(error, StringFromCStrError::InvalidUtf8);
}

#[test]
fn embedded_nul_cannot_be_silently_narrowed_to_a_cstr() {
    let value = string_from_str("a\0b");
    // SAFETY: `value` is a live managed string.
    let result = unsafe { cstring_from_string_copy(value) };

    assert!(result.is_err());
    // SAFETY: `value` remains owned by this test after the failed conversion.
    unsafe { string_release(value) };
}

#[test]
fn explicit_cstr_copy_uses_its_own_allocator_pair() {
    let value = string_from_str("café");
    // SAFETY: `value` is live and contains no embedded NUL.
    let cstr = unsafe { cstring_from_string_copy(value) }.unwrap();

    // SAFETY: the returned pointer remains live until `cstring_copy_release`.
    unsafe {
        assert_eq!(std::ffi::CStr::from_ptr(cstr).to_bytes(), "café".as_bytes());
        cstring_copy_release(cstr);
        string_release(value);
    }
}

#[test]
fn rust_cstring_copy_has_independent_lifetime_and_rejects_nul() {
    let value = string_from_str("é中🙂");
    // SAFETY: the managed source remains live until copying finishes.
    unsafe {
        let foreign = string_to_cstring(value).unwrap();
        string_release(value);
        assert_eq!(foreign.to_str().unwrap(), "é中🙂");
        drop(foreign);
        let empty = string_to_cstring(core::ptr::null()).unwrap();
        assert_eq!(empty.as_bytes_with_nul(), &[0]);
        let nul = string_from_str("a\0b");
        assert_eq!(
            string_to_cstring(nul),
            Err(hew_cabi::string::StringToCStrError::InteriorNul)
        );
        assert_eq!(string_as_str(nul), "a\0b");
        string_release(nul);
    }
}
