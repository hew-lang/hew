//! Checked public host JSON operations over the existing serde value authority.
//! Inputs are borrowed; copies and extracted text are independent owners.

use hew_cabi::host_error::{publish, publish_unit, read_utf8, HewError, HewText, HostStatus};
use hew_cabi::string::{string_as_str, string_from_str_nonnull};

use super::{boxed_value, hew_json_free, HewJsonValue};

/// Copy and parse a borrowed UTF-8 document, returning an owned error directly.
/// Unlike the package ABI, this boundary needs no ambient last-error slot.
///
/// # Safety
/// Input bytes obey `read_utf8`; distinct empty outputs obey `publish`.
#[no_mangle]
pub unsafe extern "C" fn hew_host_json_parse(
    data: *const u8,
    len: usize,
    out: *mut *mut HewJsonValue,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller supplies a live byte range for this borrow.
    let result = unsafe { read_utf8(data, len) }.and_then(|text| {
        serde_json::from_str(text)
            .map(boxed_value)
            .map_err(|err| HewError::new(HostStatus::InvalidJson, err.to_string()))
    });
    // SAFETY: the caller supplies distinct writable empty pointer slots.
    unsafe { publish(result, out, error) }
}

/// Return an independent logical copy without changing the source.
///
/// # Safety
/// `value` is live and non-null; distinct empty outputs obey `publish`.
#[no_mangle]
pub unsafe extern "C" fn hew_host_json_copy(
    value: *const HewJsonValue,
    out: *mut *mut HewJsonValue,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller borrows a live value and supplies distinct empty slots.
    unsafe { publish(Ok(boxed_value((*value).inner.clone())), out, error) }
}

/// Extract an independent text owner from an object field.
/// Missing fields and wrong kinds have distinct public error codes.
///
/// # Safety
/// `value` is live and non-null; key bytes obey `read_utf8`, outputs `publish`.
#[no_mangle]
pub unsafe extern "C" fn hew_host_json_get_text(
    value: *const HewJsonValue,
    key: *const u8,
    key_len: usize,
    out: *mut *mut HewText,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller supplies a live key byte range.
    let result = unsafe { read_utf8(key, key_len) }.and_then(|key| {
        // SAFETY: the caller borrows a live JSON value.
        let object = unsafe { &*value }
            .inner
            .as_object()
            .ok_or_else(|| HewError::new(HostStatus::WrongKind, "json: expected an object"))?;
        let child = object
            .get(key)
            .ok_or_else(|| HewError::new(HostStatus::MissingField, "json: field not found"))?;
        let text = child
            .as_str()
            .ok_or_else(|| HewError::new(HostStatus::WrongKind, "json: expected a string field"))?;
        Ok(string_from_str_nonnull(text))
    });
    // SAFETY: the caller supplies distinct writable empty pointer slots.
    unsafe { publish(result, out, error) }
}

/// Set a text field, copying the borrowed text and preserving the object on error.
///
/// # Safety
/// `value` is live, non-null and exclusively borrowed. `text` is a live non-null
/// host text. Key bytes obey `read_utf8`; the empty error slot is disjoint.
#[no_mangle]
pub unsafe extern "C" fn hew_host_json_set_text(
    value: *mut HewJsonValue,
    key: *const u8,
    key_len: usize,
    text: *const HewText,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller supplies a live key byte range.
    let result = unsafe { read_utf8(key, key_len) }.and_then(|key| {
        // SAFETY: the caller exclusively borrows the live JSON value.
        let object = unsafe { &mut *value }
            .inner
            .as_object_mut()
            .ok_or_else(|| HewError::new(HostStatus::WrongKind, "json: expected an object"))?;
        // SAFETY: the caller supplies a live borrowed managed string.
        let replacement = serde_json::Value::String(unsafe { string_as_str(text) }.to_owned());
        object.insert(key.to_owned(), replacement);
        Ok(())
    });
    // SAFETY: error is a writable initially empty slot, disjoint from inputs.
    unsafe { publish_unit(result, error) }
}

/// Encode a borrowed JSON value as an independent UTF-8 text owner.
///
/// # Safety
/// `value` is live and non-null; distinct empty outputs obey `publish`.
#[no_mangle]
pub unsafe extern "C" fn hew_host_json_encode(
    value: *const HewJsonValue,
    out: *mut *mut HewText,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller borrows a live JSON value.
    let result = serde_json::to_string(&unsafe { &*value }.inner)
        .map(|text| string_from_str_nonnull(&text))
        .map_err(|err| HewError::new(HostStatus::EncodeError, err.to_string()));
    // SAFETY: the caller supplies distinct writable empty pointer slots.
    unsafe { publish(result, out, error) }
}

/// Release one JSON owner and its tree. Null is accepted.
///
/// # Safety
/// A non-null value transfers one unique owner with no outstanding borrows.
#[no_mangle]
pub unsafe extern "C" fn hew_host_json_release(value: *mut HewJsonValue) {
    // SAFETY: the caller transfers one live JSON owner or null.
    unsafe {
        hew_json_free(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_runtime::host_api::{hew_host_error_release, hew_host_text_release};

    #[test]
    fn copy_mutation_and_extraction_preserve_independent_owners() {
        let initial_owners = super::super::live_value_boxes();
        // SAFETY: pointers name live owners, all output slots begin empty and
        // disjoint; each borrow ends before the corresponding release.
        unsafe {
            let input = br#"{"label":"original"}"#;
            let mut source = std::ptr::null_mut();
            let mut copy = std::ptr::null_mut();
            let mut error = std::ptr::null_mut();
            assert_eq!(
                hew_host_json_parse(input.as_ptr(), input.len(), &raw mut source, &raw mut error),
                0
            );
            assert_eq!(hew_host_json_copy(source, &raw mut copy, &raw mut error), 0);
            let replacement = string_from_str_nonnull("new\0雪");
            assert_eq!(
                hew_host_json_set_text(copy, b"label".as_ptr(), 5, replacement, &raw mut error),
                0
            );
            hew_host_text_release(replacement);
            let mut extracted = std::ptr::null_mut();
            assert_eq!(
                hew_host_json_get_text(
                    copy,
                    b"label".as_ptr(),
                    5,
                    &raw mut extracted,
                    &raw mut error
                ),
                0
            );
            hew_host_json_release(copy);
            assert_eq!(string_as_str(extracted), "new\0雪");
            hew_host_text_release(extracted);
            extracted = std::ptr::null_mut();
            assert_eq!(
                hew_host_json_get_text(
                    source,
                    b"label".as_ptr(),
                    5,
                    &raw mut extracted,
                    &raw mut error
                ),
                0
            );
            hew_host_json_release(source);
            assert_eq!(string_as_str(extracted), "original");
            hew_host_text_release(extracted);
            assert!(error.is_null());
            hew_host_error_release(error);
        }
        assert_eq!(super::super::live_value_boxes(), initial_owners);
    }

    #[test]
    fn invalid_key_and_wrong_parent_preserve_json_and_error_owners() {
        let initial_owners = super::super::live_value_boxes();
        // SAFETY: inputs are valid slices/owners and all outputs are distinct
        // empty slots. Errors are retained independently across subsequent calls.
        unsafe {
            let text = string_from_str_nonnull("");
            for input in ["{}", "[]"] {
                let mut value = std::ptr::null_mut();
                let mut error = std::ptr::null_mut();
                assert_eq!(
                    hew_host_json_parse(
                        input.as_ptr(),
                        input.len(),
                        &raw mut value,
                        &raw mut error
                    ),
                    0
                );
                assert_eq!(
                    hew_host_json_set_text(value, b"\xff".as_ptr(), 1, text, &raw mut error),
                    HostStatus::InvalidUtf8 as i32
                );
                let message = (*error).message().to_owned();
                let mut next_error = std::ptr::null_mut();
                let mut encoded = std::ptr::null_mut();
                assert_eq!(
                    hew_host_json_encode(value, &raw mut encoded, &raw mut next_error),
                    0
                );
                assert_eq!(string_as_str(encoded), input);
                hew_host_text_release(encoded);
                assert_eq!((*error).message(), message);
                hew_host_error_release(error);
                error = std::ptr::null_mut();
                let mut missing_text = std::ptr::null_mut();
                let code = hew_host_json_get_text(
                    value,
                    b"absent".as_ptr(),
                    6,
                    &raw mut missing_text,
                    &raw mut error,
                );
                assert!(missing_text.is_null());
                assert_eq!(
                    code,
                    if input == "{}" {
                        HostStatus::MissingField
                    } else {
                        HostStatus::WrongKind
                    } as i32
                );
                hew_host_error_release(error);
                hew_host_json_release(value);
            }
            hew_host_text_release(text);
        }
        assert_eq!(super::super::live_value_boxes(), initial_owners);
    }
}
