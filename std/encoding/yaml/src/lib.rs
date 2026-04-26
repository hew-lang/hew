//! Hew `std::encoding::yaml` — YAML parsing and generation.
//!
//! Provides YAML parsing, serialization, and value access for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. All returned [`HewYamlValue`] pointers are heap-allocated
//! via `Box` and must be freed with [`hew_yaml_free`].

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use base64::Engine as _;
use hew_cabi::{
    cabi::str_to_malloc,
    vec::{hwvec_to_u8, u8_to_hwvec, HewVec},
};
use std::ffi::CStr;
use std::os::raw::c_char;

/// Reject YAML inputs larger than 1 MiB before parsing to avoid memory abuse.
const YAML_PARSE_SIZE_LIMIT_BYTES: usize = 1024 * 1024;
/// Reject YAML inputs declaring more than 32 anchors before parsing.
const YAML_PARSE_ANCHOR_LIMIT: usize = 32;
/// Reject YAML inputs referencing more than 1024 aliases before parsing.
const YAML_PARSE_ALIAS_LIMIT: usize = 1024;

/// Opaque wrapper around a [`serde_yaml::Value`].
///
/// Returned by [`hew_yaml_parse`], [`hew_yaml_get_field`], and
/// [`hew_yaml_array_get`].
/// Must be freed with [`hew_yaml_free`].
#[derive(Debug)]
pub struct HewYamlValue {
    inner: serde_yaml::Value,
}

/// Wrap a [`serde_yaml::Value`] into a heap-allocated [`HewYamlValue`].
fn boxed_value(v: serde_yaml::Value) -> *mut HewYamlValue {
    Box::into_raw(Box::new(HewYamlValue { inner: v }))
}

std::thread_local! {
    static LAST_PARSE_ERROR: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

fn set_parse_last_error(msg: impl Into<String>) {
    LAST_PARSE_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_parse_last_error() {
    LAST_PARSE_ERROR.with(|error| *error.borrow_mut() = None);
}

fn get_parse_last_error() -> String {
    LAST_PARSE_ERROR.with(|error| error.borrow().clone().unwrap_or_default())
}

fn validate_yaml_input_limits(input: &str) -> Result<(), String> {
    if input.len() > YAML_PARSE_SIZE_LIMIT_BYTES {
        return Err(format!(
            "invalid YAML input: size limit exceeded ({} bytes > {} byte cap)",
            input.len(),
            YAML_PARSE_SIZE_LIMIT_BYTES
        ));
    }

    if !input
        .as_bytes()
        .iter()
        .any(|byte| matches!(byte, b'&' | b'*'))
    {
        return Ok(());
    }

    let bytes = input.as_bytes();
    let mut anchors = 0usize;
    let mut aliases = 0usize;
    let mut idx = 0usize;
    let mut in_single_quote = false;
    let mut in_double_quote = false;

    while idx < bytes.len() {
        match bytes[idx] {
            b'\'' if !in_double_quote => {
                if in_single_quote && bytes.get(idx + 1) == Some(&b'\'') {
                    idx += 2;
                    continue;
                }
                in_single_quote = !in_single_quote;
            }
            b'"' if !in_single_quote => {
                in_double_quote = !in_double_quote;
            }
            b'\\' if in_double_quote => {
                idx += 2;
                continue;
            }
            b'&' | b'*'
                if !in_single_quote
                    && !in_double_quote
                    && bytes
                        .get(idx + 1)
                        .is_some_and(|next| is_yaml_anchor_alias_name_byte(*next)) =>
            {
                if bytes[idx] == b'&' {
                    anchors += 1;
                    if anchors > YAML_PARSE_ANCHOR_LIMIT {
                        return Err(format!(
                            "invalid YAML input: anchor limit exceeded ({anchors} > {YAML_PARSE_ANCHOR_LIMIT})"
                        ));
                    }
                } else {
                    aliases += 1;
                    if aliases > YAML_PARSE_ALIAS_LIMIT {
                        return Err(format!(
                            "invalid YAML input: alias limit exceeded ({aliases} > {YAML_PARSE_ALIAS_LIMIT})"
                        ));
                    }
                }
            }
            _ => {}
        }

        idx += 1;
    }

    Ok(())
}

fn is_yaml_anchor_alias_name_byte(byte: u8) -> bool {
    matches!(
        byte,
        b'a'..=b'z'
            | b'A'..=b'Z'
            | b'0'..=b'9'
            | b'_'
            | b'-'
            | b'.'
    )
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a YAML string into a [`HewYamlValue`].
///
/// Returns null on parse error or invalid input.
/// Call [`hew_yaml_last_error`] to retrieve the current thread's parse failure.
///
/// # Safety
///
/// `yaml_str` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_parse(yaml_str: *const c_char) -> *mut HewYamlValue {
    if yaml_str.is_null() {
        set_parse_last_error("invalid YAML input: null pointer");
        return std::ptr::null_mut();
    }
    // SAFETY: yaml_str is a valid NUL-terminated C string per caller contract.
    let Ok(s) = (unsafe { CStr::from_ptr(yaml_str) }).to_str() else {
        set_parse_last_error("invalid YAML input: input was not valid UTF-8");
        return std::ptr::null_mut();
    };
    if let Err(err) = validate_yaml_input_limits(s) {
        set_parse_last_error(err);
        return std::ptr::null_mut();
    }
    match serde_yaml::from_str::<serde_yaml::Value>(s) {
        Ok(val) => {
            clear_parse_last_error();
            boxed_value(val)
        }
        Err(err) => {
            set_parse_last_error(err.to_string());
            std::ptr::null_mut()
        }
    }
}

/// Return the last YAML error recorded on the current thread.
///
/// This slot is shared by parse failures and byte-extraction failures from
/// [`hew_yaml_get_bytes`]. Returns an empty string when the most recent
/// operation succeeded or explicitly cleared the error slot.
#[no_mangle]
pub extern "C" fn hew_yaml_last_error() -> *mut c_char {
    str_to_malloc(&get_parse_last_error())
}

/// Serialize a [`HewYamlValue`] back to a YAML string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_yaml_string_free`]. Returns null on error.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`].
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_stringify(val: *const HewYamlValue) -> *mut c_char {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    match serde_yaml::to_string(&v.inner) {
        Ok(s) => str_to_malloc(&s),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return the type tag of a [`HewYamlValue`].
///
/// Type codes: 0=null, 1=bool, 2=number\_int, 3=number\_float, 4=string,
/// 5=sequence, 6=mapping. Returns -1 if `val` is null.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_type(val: *const HewYamlValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    match &v.inner {
        serde_yaml::Value::Null => 0,
        serde_yaml::Value::Bool(_) => 1,
        serde_yaml::Value::Number(n) => {
            if n.is_i64() || n.is_u64() {
                2
            } else {
                3
            }
        }
        serde_yaml::Value::String(_) => 4,
        serde_yaml::Value::Sequence(_) => 5,
        serde_yaml::Value::Mapping(_) => 6,
        serde_yaml::Value::Tagged(t) => {
            // Unwrap tagged values to their inner type.
            let inner_wrapper = HewYamlValue {
                inner: t.value.clone(),
            };
            let inner_ptr: *const HewYamlValue = std::ptr::addr_of!(inner_wrapper);
            // SAFETY: inner_ptr points to a valid local HewYamlValue.
            unsafe { hew_yaml_type(inner_ptr) }
        }
    }
}

/// Get the boolean value from a [`HewYamlValue`].
///
/// Returns 1 if `val` is a YAML boolean, 0 otherwise (including null).
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_is_bool(val: *const HewYamlValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    i32::from(matches!(v.inner, serde_yaml::Value::Bool(_)))
}

/// Returns 1 if `val` is a YAML integer-valued number, 0 otherwise.
///
/// Matches the values that [`hew_yaml_get_int`] can extract without coercion.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_is_int(val: *const HewYamlValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    match &v.inner {
        serde_yaml::Value::Number(n) if n.is_i64() || n.is_u64() => 1,
        _ => 0,
    }
}

/// Returns 1 if `val` is any YAML number (integer or float), 0 otherwise.
///
/// Matches the values that [`hew_yaml_get_float`] can extract — YAML, like
/// JSON, does not distinguish integer-valued floats at the value level.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_is_float(val: *const HewYamlValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    i32::from(matches!(v.inner, serde_yaml::Value::Number(_)))
}

/// Returns 1 for `true`, 0 for `false` or if the value is not a boolean.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_bool(val: *const HewYamlValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    i32::from(v.inner.as_bool().unwrap_or(false))
}

/// Get the integer value from a [`HewYamlValue`].
///
/// Returns the `i64` value, or 0 if the value is not an integer.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_int(val: *const HewYamlValue) -> i64 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    v.inner.as_i64().unwrap_or(0)
}

/// Get the floating-point value from a [`HewYamlValue`].
///
/// Returns the `f64` value, or 0.0 if the value is not a number.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_float(val: *const HewYamlValue) -> f64 {
    if val.is_null() {
        return 0.0;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    v.inner.as_f64().unwrap_or(0.0)
}

/// Get the string value from a [`HewYamlValue`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_yaml_string_free`]. Returns null if the value is not a string.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_string(val: *const HewYamlValue) -> *mut c_char {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    match v.inner.as_str() {
        Some(s) => str_to_malloc(s),
        None => std::ptr::null_mut(),
    }
}

/// Get a base64-decoded bytes value from a [`HewYamlValue`].
///
/// Returns a newly allocated [`HewVec`] for valid string inputs. Null,
/// non-string, or invalid base64 inputs return null and overwrite
/// [`hew_yaml_last_error`] so callers never observe a stale error from a prior
/// parse or decode failure.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_bytes(val: *const HewYamlValue) -> *mut HewVec {
    if val.is_null() {
        set_parse_last_error("invalid YAML bytes: value was null or key not found");
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    if let Some(s) = v.inner.as_str() {
        let decoded = match base64::engine::general_purpose::STANDARD.decode(s) {
            Ok(decoded) => decoded,
            Err(err) => {
                set_parse_last_error(format!("invalid YAML bytes: base64 decode failed: {err}"));
                return std::ptr::null_mut();
            }
        };
        clear_parse_last_error();
        // SAFETY: allocates a new HewVec owned by the caller.
        unsafe { u8_to_hwvec(&decoded) }
    } else {
        set_parse_last_error("invalid YAML bytes: value was not a string");
        std::ptr::null_mut()
    }
}

/// Get a field from a YAML mapping by key.
///
/// Returns a new heap-allocated [`HewYamlValue`] (clone of the field). The
/// caller must free it with [`hew_yaml_free`]. Returns null if the value is not
/// a mapping or the key is not found.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
/// `key` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_field(
    val: *const HewYamlValue,
    key: *const c_char,
) -> *mut HewYamlValue {
    if val.is_null() || key.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: key is a valid NUL-terminated C string per caller contract.
    let Ok(key_str) = (unsafe { CStr::from_ptr(key) }).to_str() else {
        return std::ptr::null_mut();
    };
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    let serde_yaml::Value::Mapping(mapping) = &v.inner else {
        return std::ptr::null_mut();
    };
    let yaml_key = serde_yaml::Value::String(key_str.to_owned());
    match mapping.get(&yaml_key) {
        Some(field) => boxed_value(field.clone()),
        None => std::ptr::null_mut(),
    }
}

/// Get the length of a YAML sequence.
///
/// Returns the sequence length, or -1 if the value is not a sequence.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    reason = "YAML sequences won't exceed i32::MAX in practice"
)]
pub unsafe extern "C" fn hew_yaml_array_len(val: *const HewYamlValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    match &v.inner {
        serde_yaml::Value::Sequence(seq) => seq.len() as i32,
        _ => -1,
    }
}

/// Get an element from a YAML sequence by index.
///
/// Returns a new heap-allocated [`HewYamlValue`] (clone of the element). The
/// caller must free it with [`hew_yaml_free`]. Returns null if the value is not
/// a sequence or the index is out of bounds.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_get(
    val: *const HewYamlValue,
    index: i32,
) -> *mut HewYamlValue {
    if val.is_null() || index < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewYamlValue pointer per caller contract.
    let v = unsafe { &*val };
    let serde_yaml::Value::Sequence(seq) = &v.inner else {
        return std::ptr::null_mut();
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    match seq.get(index as usize) {
        Some(elem) => boxed_value(elem.clone()),
        None => std::ptr::null_mut(),
    }
}

/// Free a [`HewYamlValue`] previously returned by any of the `hew_yaml_*`
/// functions.
///
/// # Safety
///
/// `val` must be a pointer previously returned by a `hew_yaml_*` function,
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_free(val: *mut HewYamlValue) {
    if val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(val) });
}

/// Free a C string previously returned by [`hew_yaml_stringify`] or
/// [`hew_yaml_get_string`].
///
/// # Safety
///
/// `s` must be a pointer previously returned by `hew_yaml_stringify` or
/// `hew_yaml_get_string`, and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_string_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc and has not been freed.
    unsafe { libc::free(s.cast()) };
}

// ---------------------------------------------------------------------------
// Object builder — typed field setters
// ---------------------------------------------------------------------------

/// Create a new empty YAML mapping.
///
/// Returns a heap-allocated [`HewYamlValue`] wrapping an empty YAML mapping.
/// Must be freed with [`hew_yaml_free`].
#[no_mangle]
pub extern "C" fn hew_yaml_object_new() -> *mut HewYamlValue {
    boxed_value(serde_yaml::Value::Mapping(serde_yaml::Mapping::new()))
}

/// Set a boolean field on a YAML mapping.
///
/// Does nothing if `obj` is null, not a mapping, or `key` is null.
///
/// # Safety
///
/// `obj` must be a valid non-null [`HewYamlValue`] pointer. `key` must be a
/// valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_bool(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: i32,
) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(
            serde_yaml::Value::String(key_str),
            serde_yaml::Value::Bool(val != 0),
        );
    }
}

/// Set an integer field on a YAML mapping.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_int(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: i64,
) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(
            serde_yaml::Value::String(key_str),
            serde_yaml::Value::Number(serde_yaml::Number::from(val)),
        );
    }
}

/// Set a float field on a YAML mapping.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_float(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: f64,
) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(
            serde_yaml::Value::String(key_str),
            serde_yaml::Value::Number(serde_yaml::Number::from(val)),
        );
    }
}

/// Set a string field on a YAML mapping. The string value is copied.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`]. `val` must be a valid NUL-terminated
/// C string.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_string(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: *const c_char,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key and val are valid NUL-terminated strings.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let val_str = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(
            serde_yaml::Value::String(key_str),
            serde_yaml::Value::String(val_str),
        );
    }
}

/// Set a bytes field on a YAML mapping as a base64-encoded string.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`]. `val` must be a valid bytes
/// [`HewVec`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_bytes(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: *mut HewVec,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val is non-null (checked above) and points to a valid bytes HewVec.
    let encoded = base64::engine::general_purpose::STANDARD.encode(unsafe { hwvec_to_u8(val) });
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(
            serde_yaml::Value::String(key_str),
            serde_yaml::Value::String(encoded),
        );
    }
}

/// Set a `char` (Unicode codepoint) field on a YAML mapping as an integer.
///
/// `val` is the Unicode codepoint as an `i64`. The public wire descriptor and
/// C++ consumer use the integer-codepoint range `0..=0x10_FFFF` (see
/// `IntegerBounds::for_kind(Char)` in `hew-wirecodec/src/plan.rs`).
/// Emitted as a YAML integer.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_char(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: i64,
) {
    // SAFETY: delegates to set_int under the same null-or-valid-pointer contract.
    unsafe { hew_yaml_object_set_int(obj, key, val) }
}

/// Set a `duration` (nanoseconds as i64) field on a YAML mapping as an integer.
///
/// `val` is the duration in nanoseconds, encoded as a signed i64. Negative
/// values represent time in the past. Emitted as a YAML integer.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_duration(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: i64,
) {
    // SAFETY: delegates to set_int under the same null-or-valid-pointer contract.
    unsafe { hew_yaml_object_set_int(obj, key, val) }
}

/// Get the `char` (Unicode codepoint) from a [`HewYamlValue`] integer field.
///
/// Returns the integer value as an `i64` (same as `hew_yaml_get_int`).
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_char(val: *const HewYamlValue) -> i64 {
    // SAFETY: delegates under the same null-or-valid-pointer contract.
    unsafe { hew_yaml_get_int(val) }
}

/// Get the `duration` (nanoseconds as i64) from a [`HewYamlValue`] integer field.
///
/// Returns the integer value as an `i64` (same as `hew_yaml_get_int`).
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewYamlValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_get_duration(val: *const HewYamlValue) -> i64 {
    // SAFETY: delegates under the same null-or-valid-pointer contract.
    unsafe { hew_yaml_get_int(val) }
}

// ---------------------------------------------------------------------------
// Object builder — Value child setter and null setter
// ---------------------------------------------------------------------------

/// Set a [`HewYamlValue`] child on a YAML mapping, taking ownership.
///
/// The `val` pointer is consumed and must not be freed by the caller.
/// Does nothing if `obj` is null, not a mapping, or `key` is null.
///
/// # Safety
///
/// `obj` must be a valid non-null [`HewYamlValue`] pointer. `key` must be a
/// valid NUL-terminated C string. `val` must be a valid non-null
/// [`HewYamlValue`] pointer that the caller relinquishes ownership of.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set(
    obj: *mut HewYamlValue,
    key: *const c_char,
    val: *mut HewYamlValue,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val was allocated with Box::into_raw and caller transfers ownership.
    let child = unsafe { Box::from_raw(val) };
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(serde_yaml::Value::String(key_str), child.inner);
    }
}

/// Set a null field on a YAML mapping.
///
/// Does nothing if `obj` is null, not a mapping, or `key` is null.
///
/// # Safety
///
/// Same as [`hew_yaml_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_object_set_null(obj: *mut HewYamlValue, key: *const c_char) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key_str = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Mapping(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(serde_yaml::Value::String(key_str), serde_yaml::Value::Null);
    }
}

// ---------------------------------------------------------------------------
// Array builder
// ---------------------------------------------------------------------------

/// Create a new empty YAML sequence.
///
/// Returns a heap-allocated [`HewYamlValue`] wrapping an empty YAML sequence.
/// Must be freed with [`hew_yaml_free`].
#[no_mangle]
pub extern "C" fn hew_yaml_array_new() -> *mut HewYamlValue {
    boxed_value(serde_yaml::Value::Sequence(Vec::new()))
}

/// Push a boolean onto a YAML sequence.
///
/// Does nothing if `arr` is null or not a sequence.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewYamlValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push_bool(arr: *mut HewYamlValue, val: i32) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Sequence(seq) = &mut unsafe { &mut *arr }.inner {
        seq.push(serde_yaml::Value::Bool(val != 0));
    }
}

/// Push an integer onto a YAML sequence.
///
/// Does nothing if `arr` is null or not a sequence.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewYamlValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push_int(arr: *mut HewYamlValue, val: i64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Sequence(seq) = &mut unsafe { &mut *arr }.inner {
        seq.push(serde_yaml::Value::Number(serde_yaml::Number::from(val)));
    }
}

/// Push a float onto a YAML sequence.
///
/// Does nothing if `arr` is null or not a sequence.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewYamlValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push_float(arr: *mut HewYamlValue, val: f64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Sequence(seq) = &mut unsafe { &mut *arr }.inner {
        seq.push(serde_yaml::Value::Number(serde_yaml::Number::from(val)));
    }
}

/// Push a string onto a YAML sequence. The string value is copied.
///
/// Does nothing if `arr` is null, not a sequence, or `val` is null.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewYamlValue`] pointer. `val` must be a
/// valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push_string(arr: *mut HewYamlValue, val: *const c_char) {
    if arr.is_null() || val.is_null() {
        return;
    }
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let val_str = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Sequence(seq) = &mut unsafe { &mut *arr }.inner {
        seq.push(serde_yaml::Value::String(val_str));
    }
}

/// Push a null onto a YAML sequence.
///
/// Does nothing if `arr` is null or not a sequence.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewYamlValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push_null(arr: *mut HewYamlValue) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Sequence(seq) = &mut unsafe { &mut *arr }.inner {
        seq.push(serde_yaml::Value::Null);
    }
}

/// Push a [`HewYamlValue`] child onto a YAML sequence, taking ownership.
///
/// The `val` pointer is consumed and must not be freed by the caller.
/// Does nothing if `arr` is null, not a sequence, or `val` is null.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewYamlValue`] pointer. `val` must be a
/// valid non-null [`HewYamlValue`] pointer that the caller relinquishes
/// ownership of.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_array_push(arr: *mut HewYamlValue, val: *mut HewYamlValue) {
    if arr.is_null() || val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and caller transfers ownership.
    let child = unsafe { Box::from_raw(val) };
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_yaml::Value::Sequence(seq) = &mut unsafe { &mut *arr }.inner {
        seq.push(child.inner);
    }
}

// ---------------------------------------------------------------------------
// Scalar constructors
// ---------------------------------------------------------------------------

/// Create a [`HewYamlValue`] wrapping a boolean.
///
/// Returns a heap-allocated [`HewYamlValue`]. Must be freed with
/// [`hew_yaml_free`].
#[no_mangle]
pub extern "C" fn hew_yaml_from_bool(val: i32) -> *mut HewYamlValue {
    boxed_value(serde_yaml::Value::Bool(val != 0))
}

/// Create a [`HewYamlValue`] wrapping an integer.
///
/// Returns a heap-allocated [`HewYamlValue`]. Must be freed with
/// [`hew_yaml_free`].
#[no_mangle]
pub extern "C" fn hew_yaml_from_int(val: i64) -> *mut HewYamlValue {
    boxed_value(serde_yaml::Value::Number(serde_yaml::Number::from(val)))
}

/// Create a [`HewYamlValue`] wrapping a float.
///
/// Returns a heap-allocated [`HewYamlValue`]. Must be freed with
/// [`hew_yaml_free`].
#[no_mangle]
pub extern "C" fn hew_yaml_from_float(val: f64) -> *mut HewYamlValue {
    boxed_value(serde_yaml::Value::Number(serde_yaml::Number::from(val)))
}

/// Create a [`HewYamlValue`] wrapping a string. The string value is copied.
///
/// Returns a heap-allocated [`HewYamlValue`]. Must be freed with
/// [`hew_yaml_free`]. Returns null if `val` is null.
///
/// # Safety
///
/// `val` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_from_string(val: *const c_char) -> *mut HewYamlValue {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let s = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    boxed_value(serde_yaml::Value::String(s))
}

/// Create a [`HewYamlValue`] wrapping null.
///
/// Returns a heap-allocated [`HewYamlValue`]. Must be freed with
/// [`hew_yaml_free`].
#[no_mangle]
pub extern "C" fn hew_yaml_from_null() -> *mut HewYamlValue {
    boxed_value(serde_yaml::Value::Null)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[expect(
    clippy::approx_constant,
    reason = "test data uses hardcoded floats, not mathematical constants"
)]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::fmt::Write as _;

    /// Helper: parse a YAML string and return the owned pointer.
    fn parse(yaml: &str) -> *mut HewYamlValue {
        let c = CString::new(yaml).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        unsafe { hew_yaml_parse(c.as_ptr()) }
    }

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_yaml_string_free(ptr) };
        s
    }

    /// Helper: read a bytes `HewVec` pointer and free it.
    unsafe fn read_and_free_bytes(ptr: *mut HewVec) -> Vec<u8> {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid bytes HewVec returned by this crate.
        let bytes = unsafe { hwvec_to_u8(ptr) };
        // SAFETY: ptr was allocated by the runtime allocator.
        unsafe { hew_cabi::vec::hew_vec_free(ptr) };
        bytes
    }

    #[test]
    fn parse_mapping_and_get_fields() {
        let val = parse("name: hew\nversion: 42\nactive: true\n");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            assert_eq!(hew_yaml_type(val), 6); // mapping

            let name_key = CString::new("name").unwrap();
            let name = hew_yaml_get_field(val, name_key.as_ptr());
            assert!(!name.is_null());
            assert_eq!(hew_yaml_type(name), 4); // string
            let name_str = read_and_free_cstr(hew_yaml_get_string(name));
            assert_eq!(name_str, "hew");
            hew_yaml_free(name);

            let ver_key = CString::new("version").unwrap();
            let ver = hew_yaml_get_field(val, ver_key.as_ptr());
            assert!(!ver.is_null());
            assert_eq!(hew_yaml_type(ver), 2); // number_int
            assert_eq!(hew_yaml_get_int(ver), 42);
            hew_yaml_free(ver);

            let active_key = CString::new("active").unwrap();
            let active = hew_yaml_get_field(val, active_key.as_ptr());
            assert!(!active.is_null());
            assert_eq!(hew_yaml_type(active), 1); // bool
            assert_eq!(hew_yaml_get_bool(active), 1);
            hew_yaml_free(active);

            hew_yaml_free(val);
        }
    }

    #[test]
    fn parse_sequence_and_iterate() {
        let val = parse("- 10\n- 20\n- 30\n");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            assert_eq!(hew_yaml_type(val), 5); // sequence
            assert_eq!(hew_yaml_array_len(val), 3);

            let elem = hew_yaml_array_get(val, 1);
            assert!(!elem.is_null());
            assert_eq!(hew_yaml_get_int(elem), 20);
            hew_yaml_free(elem);

            // Out of bounds returns null.
            assert!(hew_yaml_array_get(val, 5).is_null());

            hew_yaml_free(val);
        }
    }

    #[test]
    fn nested_mapping_access() {
        let yaml = "outer:\n  inner:\n    value: 99\n";
        let val = parse(yaml);
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let outer_key = CString::new("outer").unwrap();
            let outer = hew_yaml_get_field(val, outer_key.as_ptr());
            assert!(!outer.is_null());

            let inner_key = CString::new("inner").unwrap();
            let inner = hew_yaml_get_field(outer, inner_key.as_ptr());
            assert!(!inner.is_null());

            let value_key = CString::new("value").unwrap();
            let v = hew_yaml_get_field(inner, value_key.as_ptr());
            assert!(!v.is_null());
            assert_eq!(hew_yaml_get_int(v), 99);

            hew_yaml_free(v);
            hew_yaml_free(inner);
            hew_yaml_free(outer);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn stringify_roundtrip() {
        let original = "name: hew\ncount: 5\n";
        let val = parse(original);
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let yaml_str = hew_yaml_stringify(val);
            let result = read_and_free_cstr(yaml_str);
            // Re-parse both to compare structurally.
            let v1: serde_yaml::Value = serde_yaml::from_str(original).unwrap();
            let v2: serde_yaml::Value = serde_yaml::from_str(&result).unwrap();
            assert_eq!(v1, v2);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn type_checking_all_variants() {
        // SAFETY: All pointers come from parse() which returns valid HewYamlValue.
        unsafe {
            let null_val = parse("~");
            assert_eq!(hew_yaml_type(null_val), 0);
            hew_yaml_free(null_val);

            let bool_val = parse("false");
            assert_eq!(hew_yaml_type(bool_val), 1);
            assert_eq!(hew_yaml_get_bool(bool_val), 0);
            hew_yaml_free(bool_val);

            let int_val = parse("42");
            assert_eq!(hew_yaml_type(int_val), 2);
            assert_eq!(hew_yaml_get_int(int_val), 42);
            hew_yaml_free(int_val);

            let float_val = parse("3.14");
            assert_eq!(hew_yaml_type(float_val), 3);
            let f = hew_yaml_get_float(float_val);
            assert!((f - 3.14).abs() < f64::EPSILON);
            hew_yaml_free(float_val);

            let str_val = parse("\"hello\"");
            assert_eq!(hew_yaml_type(str_val), 4);
            let s = read_and_free_cstr(hew_yaml_get_string(str_val));
            assert_eq!(s, "hello");
            hew_yaml_free(str_val);

            let seq_val = parse("[]");
            assert_eq!(hew_yaml_type(seq_val), 5);
            assert_eq!(hew_yaml_array_len(seq_val), 0);
            hew_yaml_free(seq_val);

            let map_val = parse("{}");
            assert_eq!(hew_yaml_type(map_val), 6);
            hew_yaml_free(map_val);

            // Null pointer returns -1.
            assert_eq!(hew_yaml_type(std::ptr::null()), -1);
        }
    }

    #[test]
    fn parse_invalid_returns_null() {
        // Completely malformed YAML (unmatched braces) should fail.
        let val = parse("}{][");
        assert!(val.is_null());

        // SAFETY: null pointer is safe for hew_yaml_parse.
        unsafe {
            assert!(hew_yaml_parse(std::ptr::null()).is_null());
        }
    }

    #[test]
    fn float_via_get_float() {
        let val = parse("2.718");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let f = hew_yaml_get_float(val);
            assert!((f - 2.718).abs() < 1e-10);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn array_construction() {
        let arr = hew_yaml_array_new();
        assert!(!arr.is_null());

        // SAFETY: arr is a valid HewYamlValue from hew_yaml_array_new.
        unsafe {
            hew_yaml_array_push_bool(arr, 1);
            hew_yaml_array_push_int(arr, 42);
            hew_yaml_array_push_float(arr, 2.5);
            let s = CString::new("hello").unwrap();
            hew_yaml_array_push_string(arr, s.as_ptr());
            hew_yaml_array_push_null(arr);

            assert_eq!(hew_yaml_type(arr), 5); // sequence
            assert_eq!(hew_yaml_array_len(arr), 5);

            let e0 = hew_yaml_array_get(arr, 0);
            assert_eq!(hew_yaml_type(e0), 1); // bool
            assert_eq!(hew_yaml_get_bool(e0), 1);
            hew_yaml_free(e0);

            let e1 = hew_yaml_array_get(arr, 1);
            assert_eq!(hew_yaml_type(e1), 2); // int
            assert_eq!(hew_yaml_get_int(e1), 42);
            hew_yaml_free(e1);

            let e2 = hew_yaml_array_get(arr, 2);
            assert_eq!(hew_yaml_type(e2), 3); // float
            let f = hew_yaml_get_float(e2);
            assert!((f - 2.5).abs() < f64::EPSILON);
            hew_yaml_free(e2);

            let e3 = hew_yaml_array_get(arr, 3);
            assert_eq!(hew_yaml_type(e3), 4); // string
            let s_out = read_and_free_cstr(hew_yaml_get_string(e3));
            assert_eq!(s_out, "hello");
            hew_yaml_free(e3);

            let e4 = hew_yaml_array_get(arr, 4);
            assert_eq!(hew_yaml_type(e4), 0); // null
            hew_yaml_free(e4);

            hew_yaml_free(arr);
        }
    }

    #[test]
    fn array_push_value_child() {
        let arr = hew_yaml_array_new();
        assert!(!arr.is_null());

        // SAFETY: arr and inner are valid HewYamlValue pointers.
        unsafe {
            let inner = hew_yaml_object_new();
            let k = CString::new("nested").unwrap();
            hew_yaml_object_set_int(inner, k.as_ptr(), 7);
            // Push takes ownership of inner — do not free it.
            hew_yaml_array_push(arr, inner);

            assert_eq!(hew_yaml_array_len(arr), 1);
            let elem = hew_yaml_array_get(arr, 0);
            assert_eq!(hew_yaml_type(elem), 6); // mapping

            let field = hew_yaml_get_field(elem, k.as_ptr());
            assert_eq!(hew_yaml_get_int(field), 7);
            hew_yaml_free(field);
            hew_yaml_free(elem);
            hew_yaml_free(arr);
        }
    }

    #[test]
    fn object_set_value_child() {
        let obj = hew_yaml_object_new();
        assert!(!obj.is_null());

        // SAFETY: obj and child are valid HewYamlValue pointers.
        unsafe {
            let child_arr = hew_yaml_array_new();
            hew_yaml_array_push_int(child_arr, 1);
            hew_yaml_array_push_int(child_arr, 2);

            let k = CString::new("items").unwrap();
            // Set takes ownership of child_arr — do not free it.
            hew_yaml_object_set(obj, k.as_ptr(), child_arr);

            let field = hew_yaml_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_yaml_type(field), 5); // sequence
            assert_eq!(hew_yaml_array_len(field), 2);
            hew_yaml_free(field);
            hew_yaml_free(obj);
        }
    }

    #[test]
    fn object_set_null_field() {
        let obj = hew_yaml_object_new();
        assert!(!obj.is_null());

        // SAFETY: obj is a valid HewYamlValue from hew_yaml_object_new.
        unsafe {
            let k = CString::new("empty").unwrap();
            hew_yaml_object_set_null(obj, k.as_ptr());

            let field = hew_yaml_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_yaml_type(field), 0); // null
            hew_yaml_free(field);
            hew_yaml_free(obj);
        }
    }

    #[test]
    fn scalar_constructors() {
        // SAFETY: all pointers come from hew_yaml_from_* functions.
        unsafe {
            let bool_val = hew_yaml_from_bool(1);
            assert_eq!(hew_yaml_type(bool_val), 1);
            assert_eq!(hew_yaml_get_bool(bool_val), 1);
            hew_yaml_free(bool_val);

            let bool_false = hew_yaml_from_bool(0);
            assert_eq!(hew_yaml_get_bool(bool_false), 0);
            hew_yaml_free(bool_false);

            let int_val = hew_yaml_from_int(-99);
            assert_eq!(hew_yaml_type(int_val), 2);
            assert_eq!(hew_yaml_get_int(int_val), -99);
            hew_yaml_free(int_val);

            let float_val = hew_yaml_from_float(1.5);
            assert_eq!(hew_yaml_type(float_val), 3);
            let fv = hew_yaml_get_float(float_val);
            assert!((fv - 1.5).abs() < f64::EPSILON);
            hew_yaml_free(float_val);

            let str_c = CString::new("world").unwrap();
            let str_val = hew_yaml_from_string(str_c.as_ptr());
            assert_eq!(hew_yaml_type(str_val), 4);
            let sv = read_and_free_cstr(hew_yaml_get_string(str_val));
            assert_eq!(sv, "world");
            hew_yaml_free(str_val);

            // Null pointer input returns null.
            assert!(hew_yaml_from_string(std::ptr::null()).is_null());

            let null_val = hew_yaml_from_null();
            assert_eq!(hew_yaml_type(null_val), 0);
            hew_yaml_free(null_val);
        }
    }

    #[test]
    fn stringify_builder_roundtrip() {
        let obj = hew_yaml_object_new();
        assert!(!obj.is_null());

        // SAFETY: all pointers are valid HewYamlValue from builder functions.
        unsafe {
            let k_name = CString::new("name").unwrap();
            let v_name = CString::new("hew").unwrap();
            hew_yaml_object_set_string(obj, k_name.as_ptr(), v_name.as_ptr());

            let k_version = CString::new("version").unwrap();
            hew_yaml_object_set_int(obj, k_version.as_ptr(), 1);

            let k_tags = CString::new("tags").unwrap();
            let tags = hew_yaml_array_new();
            let t1 = CString::new("lang").unwrap();
            let t2 = CString::new("actor").unwrap();
            hew_yaml_array_push_string(tags, t1.as_ptr());
            hew_yaml_array_push_string(tags, t2.as_ptr());
            hew_yaml_object_set(obj, k_tags.as_ptr(), tags);

            // Stringify and re-parse to verify structural equality.
            let yaml_out = hew_yaml_stringify(obj);
            assert!(!yaml_out.is_null());
            let yaml_str = read_and_free_cstr(yaml_out);

            let reparsed = hew_yaml_parse(CString::new(yaml_str).unwrap().as_ptr());
            assert!(!reparsed.is_null());

            // Verify fields survived the roundtrip.
            let name_field = hew_yaml_get_field(reparsed, k_name.as_ptr());
            assert!(!name_field.is_null());
            let name_out = read_and_free_cstr(hew_yaml_get_string(name_field));
            assert_eq!(name_out, "hew");
            hew_yaml_free(name_field);

            let ver_field = hew_yaml_get_field(reparsed, k_version.as_ptr());
            assert_eq!(hew_yaml_get_int(ver_field), 1);
            hew_yaml_free(ver_field);

            let tags_field = hew_yaml_get_field(reparsed, k_tags.as_ptr());
            assert_eq!(hew_yaml_array_len(tags_field), 2);
            hew_yaml_free(tags_field);

            hew_yaml_free(reparsed);
            hew_yaml_free(obj);
        }
    }

    #[test]
    fn object_with_nested_values() {
        // Build: {meta: {author: "slepp", stable: true}, scores: [10, 20]}
        let root = hew_yaml_object_new();

        // SAFETY: all pointers are valid HewYamlValue from builder functions.
        unsafe {
            let meta = hew_yaml_object_new();
            let k_author = CString::new("author").unwrap();
            let v_author = CString::new("slepp").unwrap();
            hew_yaml_object_set_string(meta, k_author.as_ptr(), v_author.as_ptr());
            let k_stable = CString::new("stable").unwrap();
            hew_yaml_object_set_bool(meta, k_stable.as_ptr(), 1);

            let scores = hew_yaml_array_new();
            hew_yaml_array_push_int(scores, 10);
            hew_yaml_array_push_int(scores, 20);

            let k_meta = CString::new("meta").unwrap();
            hew_yaml_object_set(root, k_meta.as_ptr(), meta);
            let k_scores = CString::new("scores").unwrap();
            hew_yaml_object_set(root, k_scores.as_ptr(), scores);

            // Verify nested structure.
            let meta_out = hew_yaml_get_field(root, k_meta.as_ptr());
            assert_eq!(hew_yaml_type(meta_out), 6); // mapping

            let author_out = hew_yaml_get_field(meta_out, k_author.as_ptr());
            let author_str = read_and_free_cstr(hew_yaml_get_string(author_out));
            assert_eq!(author_str, "slepp");
            hew_yaml_free(author_out);

            let stable_out = hew_yaml_get_field(meta_out, k_stable.as_ptr());
            assert_eq!(hew_yaml_get_bool(stable_out), 1);
            hew_yaml_free(stable_out);
            hew_yaml_free(meta_out);

            let scores_out = hew_yaml_get_field(root, k_scores.as_ptr());
            assert_eq!(hew_yaml_array_len(scores_out), 2);
            let s0 = hew_yaml_array_get(scores_out, 0);
            assert_eq!(hew_yaml_get_int(s0), 10);
            hew_yaml_free(s0);
            let s1 = hew_yaml_array_get(scores_out, 1);
            assert_eq!(hew_yaml_get_int(s1), 20);
            hew_yaml_free(s1);
            hew_yaml_free(scores_out);

            hew_yaml_free(root);
        }
    }

    #[test]
    fn null_handling_throughout() {
        // Verify null-safety on all new functions.
        // SAFETY: testing null-pointer behaviour on all builder functions.
        unsafe {
            // Array push functions with null arr should be no-ops.
            hew_yaml_array_push_bool(std::ptr::null_mut(), 1);
            hew_yaml_array_push_int(std::ptr::null_mut(), 1);
            hew_yaml_array_push_float(std::ptr::null_mut(), 1.0);
            hew_yaml_array_push_string(std::ptr::null_mut(), std::ptr::null());
            hew_yaml_array_push_null(std::ptr::null_mut());
            hew_yaml_array_push(std::ptr::null_mut(), std::ptr::null_mut());

            // Object set functions with null obj should be no-ops.
            hew_yaml_object_set(std::ptr::null_mut(), std::ptr::null(), std::ptr::null_mut());
            hew_yaml_object_set_null(std::ptr::null_mut(), std::ptr::null());

            // Scalar constructors with null string input.
            assert!(hew_yaml_from_string(std::ptr::null()).is_null());

            // Stringify with null.
            assert!(hew_yaml_stringify(std::ptr::null()).is_null());

            // Push on a non-sequence is a silent no-op.
            let obj = hew_yaml_object_new();
            hew_yaml_array_push_int(obj, 5);
            assert_eq!(hew_yaml_type(obj), 6); // still a mapping, unmodified
            hew_yaml_free(obj);

            // Object set on a non-mapping is a silent no-op.
            let arr = hew_yaml_array_new();
            let k = CString::new("key").unwrap();
            hew_yaml_object_set_bool(arr, k.as_ptr(), 1);
            assert_eq!(hew_yaml_type(arr), 5); // still a sequence, unmodified
            assert_eq!(hew_yaml_array_len(arr), 0);
            hew_yaml_free(arr);
        }
    }

    // -----------------------------------------------------------------------
    // FFI boundary: null-pointer safety
    // -----------------------------------------------------------------------

    #[test]
    fn null_pointer_safety_all_getters() {
        // SAFETY: testing null-pointer behaviour on all getter functions.
        unsafe {
            assert!(hew_yaml_parse(std::ptr::null()).is_null());
            let err = read_and_free_cstr(hew_yaml_last_error());
            assert!(!err.is_empty());
            assert_eq!(hew_yaml_type(std::ptr::null()), -1);
            assert_eq!(hew_yaml_get_bool(std::ptr::null()), 0);
            assert_eq!(hew_yaml_get_int(std::ptr::null()), 0);
            assert!((hew_yaml_get_float(std::ptr::null())).abs() < f64::EPSILON);
            assert!(hew_yaml_get_string(std::ptr::null()).is_null());
            assert!(hew_yaml_get_field(std::ptr::null(), std::ptr::null()).is_null());
            assert_eq!(hew_yaml_array_len(std::ptr::null()), -1);
            assert!(hew_yaml_array_get(std::ptr::null(), 0).is_null());
            assert!(hew_yaml_stringify(std::ptr::null()).is_null());
        }
    }

    #[test]
    fn parse_failure_sets_last_error() {
        let bad = parse(": invalid ::: yaml");
        assert!(bad.is_null());

        // SAFETY: hew_yaml_last_error returns a malloc-allocated C string.
        let err = unsafe { read_and_free_cstr(hew_yaml_last_error()) };
        assert!(!err.is_empty());
    }

    #[test]
    fn parse_success_clears_last_error() {
        assert!(parse(": invalid ::: yaml").is_null());

        let ok = parse("name: hew\n");
        assert!(!ok.is_null());

        // SAFETY: hew_yaml_last_error returns a malloc-allocated C string.
        let err = unsafe { read_and_free_cstr(hew_yaml_last_error()) };
        assert!(err.is_empty());

        // SAFETY: ok is a valid pointer returned by parse.
        unsafe { hew_yaml_free(ok) };
    }

    fn billion_laughs_alias_limit_yaml() -> String {
        let mut yaml = String::from("a: &a [\"x\"]\n");
        let mut previous = String::from("a");

        for level in b'b'..=b'i' {
            let name = char::from(level).to_string();
            write!(yaml, "{name}: &{name} [").expect("write YAML alias bomb prefix");
            for alias_idx in 0..9 {
                if alias_idx > 0 {
                    yaml.push_str(", ");
                }
                yaml.push('*');
                yaml.push_str(&previous);
            }
            yaml.push_str("]\n");
            previous = name;
        }

        yaml.push_str("boom: [");
        for alias_idx in 0..=YAML_PARSE_ALIAS_LIMIT {
            if alias_idx > 0 {
                yaml.push_str(", ");
            }
            yaml.push('*');
            yaml.push_str(&previous);
        }
        yaml.push_str("]\n");
        yaml
    }

    #[test]
    fn parse_rejects_alias_bomb_before_deserialization() {
        let yaml = billion_laughs_alias_limit_yaml();
        let val = parse(&yaml);
        assert!(val.is_null());

        // SAFETY: hew_yaml_last_error returns a malloc-allocated C string.
        let err = unsafe { read_and_free_cstr(hew_yaml_last_error()) };
        assert!(err.contains("alias limit"));
    }

    #[test]
    fn parse_rejects_input_over_size_cap() {
        let yaml = "a".repeat(YAML_PARSE_SIZE_LIMIT_BYTES * 2);
        let val = parse(&yaml);
        assert!(val.is_null());

        // SAFETY: hew_yaml_last_error returns a malloc-allocated C string.
        let err = unsafe { read_and_free_cstr(hew_yaml_last_error()) };
        assert!(err.contains("size limit"));
        assert!(err.contains("byte cap"));
    }

    #[test]
    fn parse_accepts_normal_anchor_and_alias_usage() {
        let yaml = r#"
defaults: &defaults
  enabled: true
  tags: ["base"]
profile: &profile
  <<: *defaults
  name: hew
release: &release
  <<: *profile
  version: 1
copy_one: *defaults
copy_two: *profile
copy_three: *release
"#;
        let val = parse(yaml);
        assert!(!val.is_null());

        // SAFETY: val is a valid pointer returned by parse.
        unsafe {
            let key = CString::new("copy_three").unwrap();
            let copy_three = hew_yaml_get_field(val, key.as_ptr());
            assert!(!copy_three.is_null());
            assert_eq!(hew_yaml_type(copy_three), 6);
            hew_yaml_free(copy_three);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn parse_ignores_anchor_like_markers_inside_quotes() {
        let val = parse("'&not_anchor *not_alias'");
        assert!(!val.is_null());

        // SAFETY: val is a valid pointer returned by parse.
        unsafe {
            assert_eq!(hew_yaml_type(val), 4);
            let s = read_and_free_cstr(hew_yaml_get_string(val));
            assert_eq!(s, "&not_anchor *not_alias");
            hew_yaml_free(val);
        }
    }

    #[test]
    fn get_bytes_invalid_base64_returns_null_and_sets_last_error() {
        clear_parse_last_error();
        let val = parse("b: '!!!not-base64!!!'\n");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let key = CString::new("b").unwrap();
            let field = hew_yaml_get_field(val, key.as_ptr());
            assert!(!field.is_null());

            let bytes = hew_yaml_get_bytes(field);
            assert!(bytes.is_null());

            let err = read_and_free_cstr(hew_yaml_last_error());
            assert!(err.contains("invalid YAML bytes"));
            assert!(err.contains("base64") || err.contains("decode"));

            hew_yaml_free(field);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn get_bytes_empty_base64_returns_empty_vec() {
        clear_parse_last_error();
        let val = parse("b: ''\n");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let key = CString::new("b").unwrap();
            let field = hew_yaml_get_field(val, key.as_ptr());
            assert!(!field.is_null());

            let bytes = read_and_free_bytes(hew_yaml_get_bytes(field));
            assert!(bytes.is_empty());
            assert!(read_and_free_cstr(hew_yaml_last_error()).is_empty());

            hew_yaml_free(field);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn get_bytes_missing_key_overwrites_stale_last_error() {
        clear_parse_last_error();
        let bad = parse("b: '!!!not-base64!!!'\n");
        assert!(!bad.is_null());

        // SAFETY: bad is a valid HewYamlValue from parse.
        unsafe {
            let key = CString::new("b").unwrap();
            let field = hew_yaml_get_field(bad, key.as_ptr());
            assert!(!field.is_null());
            assert!(hew_yaml_get_bytes(field).is_null());
            assert!(!read_and_free_cstr(hew_yaml_last_error()).is_empty());
            hew_yaml_free(field);
            hew_yaml_free(bad);
        }

        let val = parse("present: 'aGV3'\n");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let key = CString::new("missing").unwrap();
            let field = hew_yaml_get_field(val, key.as_ptr());
            assert!(field.is_null());

            assert!(hew_yaml_get_bytes(field).is_null());
            let err = read_and_free_cstr(hew_yaml_last_error());
            assert!(err.contains("null") || err.contains("key not found"));

            hew_yaml_free(val);
        }
    }

    #[test]
    fn get_bytes_valid_base64_round_trips_without_last_error() {
        clear_parse_last_error();
        let val = parse("b: 'aGV3'\n");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewYamlValue from parse.
        unsafe {
            let key = CString::new("b").unwrap();
            let field = hew_yaml_get_field(val, key.as_ptr());
            assert!(!field.is_null());

            let bytes = read_and_free_bytes(hew_yaml_get_bytes(field));
            assert_eq!(bytes, b"hew");
            assert!(read_and_free_cstr(hew_yaml_last_error()).is_empty());

            hew_yaml_free(field);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn null_pointer_safety_all_setters() {
        // SAFETY: testing null-pointer behaviour on all builder functions.
        unsafe {
            hew_yaml_object_set_bool(std::ptr::null_mut(), std::ptr::null(), 1);
            hew_yaml_object_set_int(std::ptr::null_mut(), std::ptr::null(), 1);
            hew_yaml_object_set_float(std::ptr::null_mut(), std::ptr::null(), 1.0);
            hew_yaml_object_set_string(std::ptr::null_mut(), std::ptr::null(), std::ptr::null());
            hew_yaml_object_set_null(std::ptr::null_mut(), std::ptr::null());
            hew_yaml_object_set_char(std::ptr::null_mut(), std::ptr::null(), 65);
            hew_yaml_object_set_duration(std::ptr::null_mut(), std::ptr::null(), 1_000_000);
            hew_yaml_object_set(std::ptr::null_mut(), std::ptr::null(), std::ptr::null_mut());
            hew_yaml_array_push_bool(std::ptr::null_mut(), 1);
            hew_yaml_array_push_int(std::ptr::null_mut(), 1);
            hew_yaml_array_push_float(std::ptr::null_mut(), 1.0);
            hew_yaml_array_push_string(std::ptr::null_mut(), std::ptr::null());
            hew_yaml_array_push_null(std::ptr::null_mut());
            hew_yaml_array_push(std::ptr::null_mut(), std::ptr::null_mut());

            // get_char / get_duration on null must return 0.
            assert_eq!(hew_yaml_get_char(std::ptr::null()), 0);
            assert_eq!(hew_yaml_get_duration(std::ptr::null()), 0);

            // Free on null must also be a no-op.
            hew_yaml_free(std::ptr::null_mut());
            hew_yaml_string_free(std::ptr::null_mut());
        }
    }

    #[test]
    fn set_char_round_trips_bmp_codepoint() {
        // SAFETY: obj is valid from hew_yaml_object_new.
        unsafe {
            let obj = hew_yaml_object_new();
            let k = CString::new("cp").unwrap();
            hew_yaml_object_set_char(obj, k.as_ptr(), 0x41); // 'A' = 65
            let field = hew_yaml_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_yaml_get_char(field), 0x41);
            hew_yaml_free(field);
            hew_yaml_free(obj);
        }
    }

    #[test]
    fn set_duration_round_trips_nanoseconds() {
        let ns: i64 = 1_500_000_000; // 1.5 seconds in nanoseconds
                                     // SAFETY: obj is valid from hew_yaml_object_new.
        unsafe {
            let obj = hew_yaml_object_new();
            let k = CString::new("dur").unwrap();
            hew_yaml_object_set_duration(obj, k.as_ptr(), ns);
            let field = hew_yaml_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_yaml_get_duration(field), ns);
            hew_yaml_free(field);
            hew_yaml_free(obj);
        }
    }

    #[test]
    fn set_duration_round_trips_negative_nanoseconds() {
        let ns: i64 = -500_000_000; // 0.5 seconds in the past
                                    // SAFETY: obj is valid from hew_yaml_object_new.
        unsafe {
            let obj = hew_yaml_object_new();
            let k = CString::new("past").unwrap();
            hew_yaml_object_set_duration(obj, k.as_ptr(), ns);
            let field = hew_yaml_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_yaml_get_duration(field), ns);
            hew_yaml_free(field);
            hew_yaml_free(obj);
        }
    }

    #[test]
    fn get_field_null_key_returns_null() {
        let val = parse("name: hew\n");
        assert!(!val.is_null());
        // SAFETY: val is valid; passing null key.
        unsafe {
            assert!(hew_yaml_get_field(val, std::ptr::null()).is_null());
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Type mismatch: getters return safe defaults on wrong type
    // -----------------------------------------------------------------------

    #[test]
    fn type_mismatch_get_int_on_string_returns_zero() {
        let val = parse("\"not a number\"");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_yaml_get_int(val), 0);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn type_mismatch_get_string_on_int_returns_null() {
        let val = parse("42");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_yaml_get_string(val).is_null());
            hew_yaml_free(val);
        }
    }

    #[test]
    fn type_mismatch_get_bool_on_string_returns_zero() {
        let val = parse("\"true\"");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_yaml_get_bool(val), 0);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn type_mismatch_get_float_on_string_returns_zero() {
        let val = parse("\"3.14\"");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!((hew_yaml_get_float(val)).abs() < f64::EPSILON);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn array_len_on_non_sequence_returns_negative_one() {
        let val = parse("key: value\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_yaml_array_len(val), -1);
            hew_yaml_free(val);
        }
    }

    #[test]
    fn get_field_on_non_mapping_returns_null() {
        let val = parse("- 1\n- 2\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let k = CString::new("key").unwrap();
            assert!(hew_yaml_get_field(val, k.as_ptr()).is_null());
            hew_yaml_free(val);
        }
    }

    #[test]
    fn get_field_missing_key_returns_null() {
        let val = parse("a: 1\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let k = CString::new("nonexistent").unwrap();
            assert!(hew_yaml_get_field(val, k.as_ptr()).is_null());
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Sequence index boundary conditions
    // -----------------------------------------------------------------------

    #[test]
    fn array_get_negative_index_returns_null() {
        let val = parse("- 1\n- 2\n- 3\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_yaml_array_get(val, -1).is_null());
            assert!(hew_yaml_array_get(val, i32::MIN).is_null());
            hew_yaml_free(val);
        }
    }

    #[test]
    fn array_get_on_non_sequence_returns_null() {
        let val = parse("key: value\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_yaml_array_get(val, 0).is_null());
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Builder operations on wrong type are silent no-ops
    // -----------------------------------------------------------------------

    #[test]
    fn builder_set_on_sequence_is_noop() {
        // SAFETY: arr is a valid sequence.
        unsafe {
            let arr = hew_yaml_array_new();
            let k = CString::new("key").unwrap();
            let v = CString::new("val").unwrap();

            hew_yaml_object_set_bool(arr, k.as_ptr(), 1);
            hew_yaml_object_set_int(arr, k.as_ptr(), 42);
            hew_yaml_object_set_float(arr, k.as_ptr(), 1.5);
            hew_yaml_object_set_string(arr, k.as_ptr(), v.as_ptr());
            hew_yaml_object_set_null(arr, k.as_ptr());

            assert_eq!(hew_yaml_array_len(arr), 0);
            hew_yaml_free(arr);
        }
    }

    #[test]
    fn builder_push_on_mapping_is_noop() {
        // SAFETY: obj is a valid mapping.
        unsafe {
            let obj = hew_yaml_object_new();
            let s = CString::new("test").unwrap();

            hew_yaml_array_push_bool(obj, 1);
            hew_yaml_array_push_int(obj, 42);
            hew_yaml_array_push_float(obj, 1.5);
            hew_yaml_array_push_string(obj, s.as_ptr());
            hew_yaml_array_push_null(obj);

            // Mapping is still empty.
            assert_eq!(hew_yaml_type(obj), 6);
            hew_yaml_free(obj);
        }
    }

    // -----------------------------------------------------------------------
    // YAML-specific: null variants
    // -----------------------------------------------------------------------

    #[test]
    fn yaml_null_variants_all_type_zero() {
        // YAML recognises multiple null representations.
        // SAFETY: all pointers from parse.
        unsafe {
            let tilde = parse("~");
            assert_eq!(hew_yaml_type(tilde), 0);
            hew_yaml_free(tilde);

            let word_null = parse("null");
            assert_eq!(hew_yaml_type(word_null), 0);
            hew_yaml_free(word_null);

            // Empty document is null in YAML.
            let empty = parse("");
            assert_eq!(hew_yaml_type(empty), 0);
            hew_yaml_free(empty);
        }
    }

    // -----------------------------------------------------------------------
    // YAML-specific: boolean variants
    // -----------------------------------------------------------------------

    #[test]
    fn yaml_boolean_true_false_literals() {
        // SAFETY: all pointers from parse.
        unsafe {
            let t = parse("true");
            assert_eq!(hew_yaml_type(t), 1);
            assert_eq!(hew_yaml_get_bool(t), 1);
            hew_yaml_free(t);

            let f = parse("false");
            assert_eq!(hew_yaml_type(f), 1);
            assert_eq!(hew_yaml_get_bool(f), 0);
            hew_yaml_free(f);
        }
    }

    #[test]
    fn yaml_quoted_true_is_string_not_bool() {
        // Quoting "true" should preserve it as a string, not coerce to bool.
        let val = parse("\"true\"");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_yaml_type(val), 4); // string
            let s = read_and_free_cstr(hew_yaml_get_string(val));
            assert_eq!(s, "true");
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Integer boundary values
    // -----------------------------------------------------------------------

    #[test]
    fn integer_boundary_values() {
        // SAFETY: all pointers from from_int/parse.
        unsafe {
            let max_val = hew_yaml_from_int(i64::MAX);
            assert_eq!(hew_yaml_get_int(max_val), i64::MAX);
            hew_yaml_free(max_val);

            let min_val = hew_yaml_from_int(i64::MIN);
            assert_eq!(hew_yaml_get_int(min_val), i64::MIN);
            hew_yaml_free(min_val);

            let zero_val = hew_yaml_from_int(0);
            assert_eq!(hew_yaml_get_int(zero_val), 0);
            hew_yaml_free(zero_val);

            // Roundtrip i64::MAX through parse.
            let max_str = format!("{}", i64::MAX);
            let parsed = parse(&max_str);
            assert!(!parsed.is_null());
            assert_eq!(hew_yaml_get_int(parsed), i64::MAX);
            hew_yaml_free(parsed);
        }
    }

    // -----------------------------------------------------------------------
    // Unicode through CString FFI boundary
    // -----------------------------------------------------------------------

    #[test]
    fn unicode_emoji_roundtrip() {
        let val = parse("\"Hello 🌍🎉 world\"");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let s = read_and_free_cstr(hew_yaml_get_string(val));
            assert_eq!(s, "Hello 🌍🎉 world");
            hew_yaml_free(val);
        }
    }

    #[test]
    fn unicode_multibyte_in_mapping_key() {
        let val = parse("clé: 42\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let k = CString::new("clé").unwrap();
            let field = hew_yaml_get_field(val, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_yaml_get_int(field), 42);
            hew_yaml_free(field);
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Malformed input error handling
    // -----------------------------------------------------------------------

    #[test]
    fn malformed_yaml_tab_indentation() {
        // YAML forbids tabs for indentation.
        let val = parse("parent:\n\tchild: value\n");
        assert!(val.is_null());
    }

    #[test]
    fn malformed_yaml_unmatched_brace() {
        assert!(parse("}{][").is_null());
    }

    // -----------------------------------------------------------------------
    // YAML multiline strings
    // -----------------------------------------------------------------------

    #[test]
    fn yaml_literal_block_scalar() {
        // The `|` indicator preserves newlines.
        let val = parse("|\n  line one\n  line two\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_yaml_type(val), 4); // string
            let s = read_and_free_cstr(hew_yaml_get_string(val));
            assert!(s.contains("line one"));
            assert!(s.contains("line two"));
            hew_yaml_free(val);
        }
    }

    #[test]
    fn yaml_folded_block_scalar() {
        // The `>` indicator folds newlines into spaces.
        let val = parse(">\n  line one\n  line two\n");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_yaml_type(val), 4); // string
            let s = read_and_free_cstr(hew_yaml_get_string(val));
            assert!(s.contains("line one"));
            assert!(s.contains("line two"));
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // YAML float special values
    // -----------------------------------------------------------------------

    #[test]
    fn yaml_special_float_infinity() {
        let val = parse(".inf");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let f = hew_yaml_get_float(val);
            assert!(f.is_infinite() && f.is_sign_positive());
            hew_yaml_free(val);
        }
    }

    #[test]
    fn yaml_special_float_nan() {
        let val = parse(".nan");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let f = hew_yaml_get_float(val);
            assert!(f.is_nan());
            hew_yaml_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Roundtrip: build → stringify → parse → verify
    // -----------------------------------------------------------------------

    #[test]
    fn roundtrip_complex_builder_structure() {
        // Build: {items: [{id: 1, label: "α"}, {id: 2, label: "β"}], count: 2}
        // SAFETY: all pointers from builder functions.
        unsafe {
            let root = hew_yaml_object_new();
            let items = hew_yaml_array_new();

            for (id, label) in [(1_i64, "α"), (2, "β")] {
                let item = hew_yaml_object_new();
                let k_id = CString::new("id").unwrap();
                hew_yaml_object_set_int(item, k_id.as_ptr(), id);
                let k_label = CString::new("label").unwrap();
                let v_label = CString::new(label).unwrap();
                hew_yaml_object_set_string(item, k_label.as_ptr(), v_label.as_ptr());
                hew_yaml_array_push(items, item);
            }

            let k_items = CString::new("items").unwrap();
            hew_yaml_object_set(root, k_items.as_ptr(), items);
            let k_count = CString::new("count").unwrap();
            hew_yaml_object_set_int(root, k_count.as_ptr(), 2);

            // Stringify and re-parse.
            let yaml_str = hew_yaml_stringify(root);
            let yaml_text = read_and_free_cstr(yaml_str);
            hew_yaml_free(root);

            let reparsed = parse(&yaml_text);
            assert!(!reparsed.is_null());

            // Verify count survived.
            let count_field = hew_yaml_get_field(reparsed, k_count.as_ptr());
            assert_eq!(hew_yaml_get_int(count_field), 2);
            hew_yaml_free(count_field);

            // Verify items[1].label == "β".
            let items_field = hew_yaml_get_field(reparsed, k_items.as_ptr());
            assert_eq!(hew_yaml_array_len(items_field), 2);
            let item1 = hew_yaml_array_get(items_field, 1);
            let k_label = CString::new("label").unwrap();
            let label_field = hew_yaml_get_field(item1, k_label.as_ptr());
            let label_str = read_and_free_cstr(hew_yaml_get_string(label_field));
            assert_eq!(label_str, "β");
            hew_yaml_free(label_field);
            hew_yaml_free(item1);
            hew_yaml_free(items_field);
            hew_yaml_free(reparsed);
        }
    }

    #[test]
    fn canonical_type_tag_prefix_matches_issue_1321() {
        // #1321: lock the shared 0..6 prefix documented in
        // std/encoding/wire/value_trait.hew so YAML stays aligned with JSON/TOML.
        // SAFETY: every handle below is allocated by boxed_value and freed once
        // by hew_yaml_free in the same scope.
        unsafe {
            let null_val = boxed_value(serde_yaml::Value::Null);
            assert_eq!(hew_yaml_type(null_val), 0);
            hew_yaml_free(null_val);

            let bool_val = boxed_value(serde_yaml::Value::Bool(true));
            assert_eq!(hew_yaml_type(bool_val), 1);
            hew_yaml_free(bool_val);

            let int_val = boxed_value(serde_yaml::to_value(42_i64).expect("serialize YAML int"));
            assert_eq!(hew_yaml_type(int_val), 2);
            hew_yaml_free(int_val);

            let float_val =
                boxed_value(serde_yaml::to_value(3.25_f64).expect("serialize YAML float"));
            assert_eq!(hew_yaml_type(float_val), 3);
            hew_yaml_free(float_val);

            let string_val = boxed_value(serde_yaml::Value::String("hew".to_owned()));
            assert_eq!(hew_yaml_type(string_val), 4);
            hew_yaml_free(string_val);

            let array_val = boxed_value(serde_yaml::Value::Sequence(Vec::new()));
            assert_eq!(hew_yaml_type(array_val), 5);
            hew_yaml_free(array_val);

            let object_val = boxed_value(serde_yaml::Value::Mapping(serde_yaml::Mapping::new()));
            assert_eq!(hew_yaml_type(object_val), 6);
            hew_yaml_free(object_val);
        }
    }
}
