use hew_cabi::string::{string_as_bytes, string_as_str, HewString};
use hew_runtime::bytes::hew_bytes_drop;
use hew_runtime::string::{
    hew_string_byte_length, hew_string_char_at, hew_string_clone, hew_string_concat,
    hew_string_drop, hew_string_find, hew_string_length, hew_string_literal_new, hew_string_slice,
    hew_string_to_bytes_owned, hew_string_to_uppercase,
};
use std::mem::MaybeUninit;

unsafe fn literal(text: &str) -> *mut HewString {
    let mut out = MaybeUninit::<*mut HewString>::uninit();
    // SAFETY: `text` is valid UTF-8 and `out` is unique uninitialized storage.
    unsafe {
        hew_string_literal_new(
            text.as_ptr(),
            u32::try_from(text.len()).unwrap(),
            out.as_mut_ptr(),
        );
        out.assume_init()
    }
}

#[test]
fn literal_preserves_embedded_nul_and_both_length_units() {
    // SAFETY: helper constructs a managed string owner.
    let value = unsafe { literal("A\0é中🙂") };

    // SAFETY: `value` remains live until the final release.
    unsafe {
        assert_eq!(string_as_bytes(value), "A\0é中🙂".as_bytes());
        assert_eq!(hew_string_byte_length(value), 11);
        assert_eq!(hew_string_length(value), 5);
        hew_string_drop(value);
    }
}

#[test]
fn concat_and_uppercase_preserve_full_length_bounded_contents() {
    // SAFETY: helpers construct managed string owners.
    let left = unsafe { literal("a\0é") };
    // SAFETY: helper constructs a managed string owner.
    let right = unsafe { literal("中") };
    // SAFETY: both inputs are live borrowed handles.
    let joined = unsafe { hew_string_concat(left, right) };
    // SAFETY: `joined` is a live borrowed handle.
    let upper = unsafe { hew_string_to_uppercase(joined) };

    // SAFETY: all handles remain live until released below.
    unsafe {
        assert_eq!(string_as_str(joined), "a\0é中");
        assert_eq!(string_as_str(upper), "A\0É中");
        hew_string_drop(upper);
        hew_string_drop(joined);
        hew_string_drop(right);
        hew_string_drop(left);
    }
}

#[test]
fn retained_copy_survives_original_drop() {
    // SAFETY: helper constructs a managed string owner.
    let original = unsafe { literal("copy\0me") };
    // SAFETY: `original` is live.
    let copy = unsafe { hew_string_clone(original) };
    // SAFETY: the retained copy keeps the allocation live.
    unsafe { hew_string_drop(original) };

    // SAFETY: `copy` remains live until released below.
    unsafe {
        assert_eq!(string_as_str(copy), "copy\0me");
        hew_string_drop(copy);
    }
}

#[test]
fn scalar_positions_include_nul_and_ignore_utf8_byte_width() {
    // SAFETY: the helpers create independent managed owners.
    let value = unsafe { literal("é\0中🙂z") };
    // SAFETY: the helper creates an independent managed owner.
    let needle = unsafe { literal("🙂z") };
    // SAFETY: both handles are live; slicing returns an independent owner.
    unsafe {
        assert_eq!(hew_string_find(value, needle), 3);
        assert_eq!(hew_string_char_at(value, 1), 0);
        assert_eq!(hew_string_char_at(value, 3), '🙂' as i32);
        let slice = hew_string_slice(value, 1, 4);
        hew_string_drop(value);
        hew_string_drop(needle);
        assert_eq!(string_as_str(slice), "\0中🙂");
        hew_string_drop(slice);
    }
}

#[test]
fn string_to_bytes_copies_every_utf8_byte_including_nul() {
    // SAFETY: helper constructs a managed string owner.
    let value = unsafe { literal("x\0é") };
    let mut out = MaybeUninit::uninit();
    // SAFETY: `value` is borrowed and `out` is unique uninitialized storage.
    unsafe { hew_string_to_bytes_owned(value, out.as_mut_ptr()) };
    // SAFETY: the runtime initialized the result on return.
    let bytes = unsafe { out.assume_init() };

    // SAFETY: the triple owns an allocation valid for its active range.
    unsafe {
        let active =
            std::slice::from_raw_parts(bytes.ptr.add(bytes.offset as usize), bytes.len as usize);
        assert_eq!(active, "x\0é".as_bytes());
        hew_bytes_drop(bytes.ptr);
        hew_string_drop(value);
    }
}
