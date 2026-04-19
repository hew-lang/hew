//! Hew `std::encoding::json` — JSON parsing and generation.
//!
//! Provides JSON parsing, serialization, and value access for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. All returned [`HewJsonValue`] pointers are heap-allocated
//! via `Box` and must be freed with [`hew_json_free`].

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

/// Opaque wrapper around a [`serde_json::Value`].
///
/// Returned by [`hew_json_parse`], [`hew_json_get_field`],
/// [`hew_json_array_get`], and [`hew_json_object_keys`].
/// Must be freed with [`hew_json_free`].
#[derive(Debug)]
pub struct HewJsonValue {
    inner: serde_json::Value,
}

/// Wrap a [`serde_json::Value`] into a heap-allocated [`HewJsonValue`].
fn boxed_value(v: serde_json::Value) -> *mut HewJsonValue {
    Box::into_raw(Box::new(HewJsonValue { inner: v }))
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

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a JSON string into a [`HewJsonValue`].
///
/// Returns null on parse error or invalid input.
/// Call [`hew_json_last_error`] to retrieve the current thread's parse failure.
///
/// # Safety
///
/// `json_str` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_parse(json_str: *const c_char) -> *mut HewJsonValue {
    if json_str.is_null() {
        set_parse_last_error("invalid JSON input: null pointer");
        return std::ptr::null_mut();
    }
    // SAFETY: json_str is a valid NUL-terminated C string per caller contract.
    let Ok(s) = unsafe { CStr::from_ptr(json_str) }.to_str() else {
        set_parse_last_error("invalid JSON input: input was not valid UTF-8");
        return std::ptr::null_mut();
    };
    match serde_json::from_str::<serde_json::Value>(s) {
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

/// Return the last JSON parse error recorded on the current thread.
///
/// Returns an empty string when no parse error has been recorded.
#[no_mangle]
pub extern "C" fn hew_json_last_error() -> *mut c_char {
    str_to_malloc(&get_parse_last_error())
}

/// Serialize a [`HewJsonValue`] back to a JSON string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_json_string_free`]. Returns null on error.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`].
#[no_mangle]
pub unsafe extern "C" fn hew_json_stringify(val: *const HewJsonValue) -> *mut c_char {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    let s = serde_json::to_string(&v.inner).unwrap_or_default();
    str_to_malloc(&s)
}

/// Return the type tag of a [`HewJsonValue`].
///
/// Type codes: 0=null, 1=bool, 2=number\_int, 3=number\_float, 4=string,
/// 5=array, 6=object. Returns -1 if `val` is null.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_type(val: *const HewJsonValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match &v.inner {
        serde_json::Value::Null => 0,
        serde_json::Value::Bool(_) => 1,
        serde_json::Value::Number(n) => {
            if n.is_i64() || n.is_u64() {
                2
            } else {
                3
            }
        }
        serde_json::Value::String(_) => 4,
        serde_json::Value::Array(_) => 5,
        serde_json::Value::Object(_) => 6,
    }
}

/// Get the boolean value from a [`HewJsonValue`].
///
/// Returns 1 if `val` is a JSON boolean, 0 otherwise (including null).
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_is_bool(val: *const HewJsonValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    i32::from(matches!(v.inner, serde_json::Value::Bool(_)))
}

/// Returns 1 if `val` is a JSON integer-valued number, 0 otherwise.
///
/// Matches the values that [`hew_json_get_int`] can extract without coercion.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_is_int(val: *const HewJsonValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match &v.inner {
        serde_json::Value::Number(n) if n.is_i64() || n.is_u64() => 1,
        _ => 0,
    }
}

/// Returns 1 if `val` is any JSON number (integer or float), 0 otherwise.
///
/// Matches the values that [`hew_json_get_float`] can extract — JSON does not
/// distinguish integer-valued floats at the value level, so both integer and
/// float numbers are accepted.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_is_float(val: *const HewJsonValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    i32::from(matches!(v.inner, serde_json::Value::Number(_)))
}

/// Returns 1 for `true`, 0 for `false` or if the value is not a boolean.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_bool(val: *const HewJsonValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    i32::from(v.inner.as_bool().unwrap_or(false))
}

/// Get the integer value from a [`HewJsonValue`].
///
/// Returns the `i64` value, or 0 if the value is not an integer.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_int(val: *const HewJsonValue) -> i64 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    v.inner.as_i64().unwrap_or(0)
}

/// Get the floating-point value from a [`HewJsonValue`].
///
/// Returns the `f64` value, or 0.0 if the value is not a number.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_float(val: *const HewJsonValue) -> f64 {
    if val.is_null() {
        return 0.0;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    v.inner.as_f64().unwrap_or(0.0)
}

/// Get the string value from a [`HewJsonValue`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_json_string_free`]. Returns null if the value is not a string.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_string(val: *const HewJsonValue) -> *mut c_char {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match v.inner.as_str() {
        Some(s) => str_to_malloc(s),
        None => std::ptr::null_mut(),
    }
}

/// Get a base64-decoded bytes value from a [`HewJsonValue`].
///
/// Returns a newly allocated [`HewVec`] for valid string inputs. Non-string
/// values return null. Invalid base64 inputs decode to an empty bytes vector to
/// match `std::encoding::base64::decode`.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_bytes(val: *const HewJsonValue) -> *mut HewVec {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match v.inner.as_str() {
        Some(s) => {
            let decoded = base64::engine::general_purpose::STANDARD
                .decode(s)
                .unwrap_or_default();
            // SAFETY: allocates a new HewVec owned by the caller.
            unsafe { u8_to_hwvec(&decoded) }
        }
        None => std::ptr::null_mut(),
    }
}

/// Get a field from a JSON object by key.
///
/// Returns a new heap-allocated [`HewJsonValue`] (clone of the field). The
/// caller must free it with [`hew_json_free`]. Returns null if the value is not
/// an object or the key is not found.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
/// `key` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_field(
    val: *const HewJsonValue,
    key: *const c_char,
) -> *mut HewJsonValue {
    if val.is_null() || key.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: key is a valid NUL-terminated C string per caller contract.
    let Ok(key_str) = unsafe { CStr::from_ptr(key) }.to_str() else {
        return std::ptr::null_mut();
    };
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match v.inner.get(key_str) {
        Some(field) => boxed_value(field.clone()),
        None => std::ptr::null_mut(),
    }
}

/// Get the length of a JSON array.
///
/// Returns the array length, or -1 if the value is not an array.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    reason = "JSON arrays won't exceed i32::MAX in practice"
)]
pub unsafe extern "C" fn hew_json_array_len(val: *const HewJsonValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match v.inner.as_array() {
        Some(arr) => arr.len() as i32,
        None => -1,
    }
}

/// Get an element from a JSON array by index.
///
/// Returns a new heap-allocated [`HewJsonValue`] (clone of the element). The
/// caller must free it with [`hew_json_free`]. Returns null if the value is not
/// an array or the index is out of bounds.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_get(
    val: *const HewJsonValue,
    index: i32,
) -> *mut HewJsonValue {
    if val.is_null() || index < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    match v.inner.as_array().and_then(|arr| arr.get(index as usize)) {
        Some(elem) => boxed_value(elem.clone()),
        None => std::ptr::null_mut(),
    }
}

/// Return the keys of a JSON object as a JSON array of strings.
///
/// Returns a new heap-allocated [`HewJsonValue`] containing the keys. The
/// caller must free it with [`hew_json_free`]. Returns null if the value is not
/// an object.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_keys(val: *const HewJsonValue) -> *mut HewJsonValue {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid HewJsonValue pointer per caller contract.
    let v = unsafe { &*val };
    match v.inner.as_object() {
        Some(obj) => {
            let keys: Vec<serde_json::Value> = obj
                .keys()
                .map(|k| serde_json::Value::String(k.clone()))
                .collect();
            boxed_value(serde_json::Value::Array(keys))
        }
        None => std::ptr::null_mut(),
    }
}

/// Free a [`HewJsonValue`] previously returned by any of the `hew_json_*`
/// functions.
///
/// # Safety
///
/// `val` must be a pointer previously returned by a `hew_json_*` function,
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_json_free(val: *mut HewJsonValue) {
    if val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(val) });
}

/// Free a C string previously returned by [`hew_json_stringify`] or
/// [`hew_json_get_string`].
///
/// # Safety
///
/// `s` must be a pointer previously returned by `hew_json_stringify` or
/// `hew_json_get_string`, and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_json_string_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc and has not been freed.
    unsafe { libc::free(s.cast()) };
}

// ---------------------------------------------------------------------------
// Object builder — typed field setters
// ---------------------------------------------------------------------------

/// Create a new empty JSON object.
///
/// Returns a heap-allocated [`HewJsonValue`] wrapping an empty JSON object.
/// Must be freed with [`hew_json_free`].
#[no_mangle]
pub extern "C" fn hew_json_object_new() -> *mut HewJsonValue {
    boxed_value(serde_json::Value::Object(serde_json::Map::new()))
}

/// Set a boolean field on a JSON object.
///
/// Does nothing if `obj` is null, not an object, or `key` is null.
///
/// # Safety
///
/// `obj` must be a valid non-null [`HewJsonValue`] pointer. `key` must be a
/// valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_bool(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: i32,
) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(key, serde_json::Value::Bool(val != 0));
    }
}

/// Set an integer field on a JSON object.
///
/// # Safety
///
/// Same as [`hew_json_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_int(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: i64,
) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(
            key,
            serde_json::Value::Number(serde_json::Number::from(val)),
        );
    }
}

/// Set a float field on a JSON object.
///
/// # Safety
///
/// Same as [`hew_json_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_float(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: f64,
) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        if let Some(n) = serde_json::Number::from_f64(val) {
            map.insert(key, serde_json::Value::Number(n));
        }
    }
}

/// Set a string field on a JSON object. The string value is copied.
///
/// # Safety
///
/// Same as [`hew_json_object_set_bool`]. `val` must be a valid NUL-terminated
/// C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_string(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: *const c_char,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key and val are valid NUL-terminated strings.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let val = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(key, serde_json::Value::String(val));
    }
}

/// Set a bytes field on a JSON object as a base64-encoded string.
///
/// # Safety
///
/// Same as [`hew_json_object_set_bool`]. `val` must be a valid bytes
/// [`HewVec`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_bytes(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: *mut HewVec,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val is non-null (checked above) and points to a valid bytes HewVec.
    let encoded = base64::engine::general_purpose::STANDARD.encode(unsafe { hwvec_to_u8(val) });
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(key, serde_json::Value::String(encoded));
    }
}

/// Set a `char` (Unicode codepoint) field on a JSON object as an integer.
///
/// `val` is the Unicode codepoint as an `i64`. Only BMP codepoints (0..=0xFFFF)
/// are expected at this boundary (see `IntegerBounds::for_kind(Char)` SHIM in
/// `hew-wirecodec/src/plan.rs` for the lift-to-full-Unicode tracking note).
/// Emitted as a JSON number (integer).
///
/// # Safety
///
/// Same as [`hew_json_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_char(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: i64,
) {
    // SAFETY: delegates to set_int after verifying null contracts; caller
    // guarantees obj and key are valid per the same contract as set_bool.
    unsafe { hew_json_object_set_int(obj, key, val) }
}

/// Set a `duration` (nanoseconds as i64) field on a JSON object as an integer.
///
/// `val` is the duration in nanoseconds, encoded as a signed i64. Negative
/// values represent time in the past. Emitted as a JSON number (integer).
///
/// # Safety
///
/// Same as [`hew_json_object_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_duration(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: i64,
) {
    // SAFETY: delegates to set_int after verifying null contracts; caller
    // guarantees obj and key are valid per the same contract as set_bool.
    unsafe { hew_json_object_set_int(obj, key, val) }
}

/// Get the `char` (Unicode codepoint) from a [`HewJsonValue`] integer field.
///
/// Returns the integer value as an `i64` (same as `hew_json_get_int`). The
/// caller is responsible for verifying the value is in the BMP range before
/// interpreting it as a char codepoint.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_char(val: *const HewJsonValue) -> i64 {
    // SAFETY: delegates under the same null-or-valid-pointer contract.
    unsafe { hew_json_get_int(val) }
}

/// Get the `duration` (nanoseconds as i64) from a [`HewJsonValue`] integer field.
///
/// Returns the integer value as an `i64` (same as `hew_json_get_int`).
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewJsonValue`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_get_duration(val: *const HewJsonValue) -> i64 {
    // SAFETY: delegates under the same null-or-valid-pointer contract.
    unsafe { hew_json_get_int(val) }
}

// ---------------------------------------------------------------------------
// Object builder — null and Value child setters
// ---------------------------------------------------------------------------

/// Set a null field on a JSON object.
///
/// Does nothing if `obj` is null or not an object, or `key` is null.
///
/// # Safety
///
/// `obj` must be a valid non-null [`HewJsonValue`] pointer. `key` must be a
/// valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set_null(obj: *mut HewJsonValue, key: *const c_char) {
    if obj.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(key, serde_json::Value::Null);
    }
}

/// Set a [`HewJsonValue`] child on a JSON object. Takes ownership of `val`
/// (the caller must not free it).
///
/// Does nothing if `obj` is null or not an object, or `key` or `val` is null.
///
/// # Safety
///
/// `obj` must be a valid non-null [`HewJsonValue`] pointer. `key` must be a
/// valid NUL-terminated C string. `val` must be a heap-allocated
/// [`HewJsonValue`] that this function takes ownership of.
#[no_mangle]
pub unsafe extern "C" fn hew_json_object_set(
    obj: *mut HewJsonValue,
    key: *const c_char,
    val: *mut HewJsonValue,
) {
    if obj.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees obj is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val was allocated with Box::into_raw; we take ownership here.
    let child = unsafe { Box::from_raw(val) };
    // SAFETY: obj is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Object(map) = &mut unsafe { &mut *obj }.inner {
        map.insert(key, child.inner);
    }
}

// ---------------------------------------------------------------------------
// Array builder
// ---------------------------------------------------------------------------

/// Create a new empty JSON array.
///
/// Returns a heap-allocated [`HewJsonValue`] wrapping an empty JSON array.
/// Must be freed with [`hew_json_free`].
#[no_mangle]
pub extern "C" fn hew_json_array_new() -> *mut HewJsonValue {
    boxed_value(serde_json::Value::Array(Vec::new()))
}

/// Push a boolean onto a JSON array.
///
/// Does nothing if `arr` is null or not an array.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewJsonValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_push_bool(arr: *mut HewJsonValue, val: i32) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(serde_json::Value::Bool(val != 0));
    }
}

/// Push an integer onto a JSON array.
///
/// Does nothing if `arr` is null or not an array.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewJsonValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_push_int(arr: *mut HewJsonValue, val: i64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(serde_json::Value::Number(serde_json::Number::from(val)));
    }
}

/// Push a float onto a JSON array.
///
/// Does nothing if `arr` is null or not an array.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewJsonValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_push_float(arr: *mut HewJsonValue, val: f64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        if let Some(n) = serde_json::Number::from_f64(val) {
            vec.push(serde_json::Value::Number(n));
        }
    }
}

/// Push a string onto a JSON array. The string value is copied.
///
/// Does nothing if `arr` or `val` is null, or `arr` is not an array.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewJsonValue`] pointer. `val` must be a
/// valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_push_string(arr: *mut HewJsonValue, val: *const c_char) {
    if arr.is_null() || val.is_null() {
        return;
    }
    // SAFETY: val is a valid NUL-terminated C string per caller contract.
    let s = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(serde_json::Value::String(s));
    }
}

/// Push null onto a JSON array.
///
/// Does nothing if `arr` is null or not an array.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewJsonValue`] pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_push_null(arr: *mut HewJsonValue) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(serde_json::Value::Null);
    }
}

/// Push a [`HewJsonValue`] child onto a JSON array. Takes ownership of `val`
/// (the caller must not free it).
///
/// Does nothing if `arr` or `val` is null, or `arr` is not an array.
///
/// # Safety
///
/// `arr` must be a valid non-null [`HewJsonValue`] pointer. `val` must be a
/// heap-allocated [`HewJsonValue`] that this function takes ownership of.
#[no_mangle]
pub unsafe extern "C" fn hew_json_array_push(arr: *mut HewJsonValue, val: *mut HewJsonValue) {
    if arr.is_null() || val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw; we take ownership here.
    let child = unsafe { Box::from_raw(val) };
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let serde_json::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(child.inner);
    }
}

// ---------------------------------------------------------------------------
// Scalar value constructors
// ---------------------------------------------------------------------------

/// Create a [`HewJsonValue`] containing a boolean.
///
/// `val` is interpreted as C boolean: 0 = false, non-zero = true.
#[no_mangle]
pub extern "C" fn hew_json_from_bool(val: i32) -> *mut HewJsonValue {
    boxed_value(serde_json::Value::Bool(val != 0))
}

/// Create a [`HewJsonValue`] containing an integer.
#[no_mangle]
pub extern "C" fn hew_json_from_int(val: i64) -> *mut HewJsonValue {
    boxed_value(serde_json::Value::Number(serde_json::Number::from(val)))
}

/// Create a [`HewJsonValue`] containing a float.
///
/// Returns null if the float is NaN or infinity (not representable in JSON).
#[no_mangle]
pub extern "C" fn hew_json_from_float(val: f64) -> *mut HewJsonValue {
    match serde_json::Number::from_f64(val) {
        Some(n) => boxed_value(serde_json::Value::Number(n)),
        None => std::ptr::null_mut(),
    }
}

/// Create a [`HewJsonValue`] containing a string. The string value is copied.
///
/// Returns null if `val` is null.
///
/// # Safety
///
/// `val` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_json_from_string(val: *const c_char) -> *mut HewJsonValue {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid NUL-terminated C string per caller contract.
    let s = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    boxed_value(serde_json::Value::String(s))
}

/// Create a [`HewJsonValue`] containing null.
#[no_mangle]
pub extern "C" fn hew_json_from_null() -> *mut HewJsonValue {
    boxed_value(serde_json::Value::Null)
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

    /// Helper: parse a JSON string and return the owned pointer.
    fn parse(json: &str) -> *mut HewJsonValue {
        let c = CString::new(json).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        unsafe { hew_json_parse(c.as_ptr()) }
    }

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_json_string_free(ptr) };
        s
    }

    #[test]
    fn parse_object_and_get_fields() {
        let val = parse(r#"{"name":"hew","version":42,"active":true}"#);
        assert!(!val.is_null());

        // SAFETY: val is a valid HewJsonValue from parse.
        unsafe {
            assert_eq!(hew_json_type(val), 6); // object

            let name_key = CString::new("name").unwrap();
            let name = hew_json_get_field(val, name_key.as_ptr());
            assert!(!name.is_null());
            assert_eq!(hew_json_type(name), 4); // string
            let name_str = read_and_free_cstr(hew_json_get_string(name));
            assert_eq!(name_str, "hew");
            hew_json_free(name);

            let ver_key = CString::new("version").unwrap();
            let ver = hew_json_get_field(val, ver_key.as_ptr());
            assert!(!ver.is_null());
            assert_eq!(hew_json_type(ver), 2); // number_int
            assert_eq!(hew_json_get_int(ver), 42);
            hew_json_free(ver);

            let active_key = CString::new("active").unwrap();
            let active = hew_json_get_field(val, active_key.as_ptr());
            assert!(!active.is_null());
            assert_eq!(hew_json_type(active), 1); // bool
            assert_eq!(hew_json_get_bool(active), 1);
            hew_json_free(active);

            hew_json_free(val);
        }
    }

    #[test]
    fn parse_array_and_iterate() {
        let val = parse(r"[10, 20, 30]");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewJsonValue from parse.
        unsafe {
            assert_eq!(hew_json_type(val), 5); // array
            assert_eq!(hew_json_array_len(val), 3);

            let elem = hew_json_array_get(val, 1);
            assert!(!elem.is_null());
            assert_eq!(hew_json_get_int(elem), 20);
            hew_json_free(elem);

            // Out of bounds returns null.
            assert!(hew_json_array_get(val, 5).is_null());

            hew_json_free(val);
        }
    }

    #[test]
    fn nested_field_access() {
        let val = parse(r#"{"outer":{"inner":{"value":99}}}"#);
        assert!(!val.is_null());

        // SAFETY: val is a valid HewJsonValue from parse.
        unsafe {
            let outer_key = CString::new("outer").unwrap();
            let outer = hew_json_get_field(val, outer_key.as_ptr());
            assert!(!outer.is_null());

            let inner_key = CString::new("inner").unwrap();
            let inner = hew_json_get_field(outer, inner_key.as_ptr());
            assert!(!inner.is_null());

            let value_key = CString::new("value").unwrap();
            let v = hew_json_get_field(inner, value_key.as_ptr());
            assert!(!v.is_null());
            assert_eq!(hew_json_get_int(v), 99);

            hew_json_free(v);
            hew_json_free(inner);
            hew_json_free(outer);
            hew_json_free(val);
        }
    }

    #[test]
    fn stringify_roundtrip() {
        let original = r#"{"a":1,"b":"hello"}"#;
        let val = parse(original);
        assert!(!val.is_null());

        // SAFETY: val is a valid HewJsonValue from parse.
        unsafe {
            let json_str = hew_json_stringify(val);
            let result = read_and_free_cstr(json_str);
            // Re-parse both to compare structurally (key order may differ).
            let v1: serde_json::Value = serde_json::from_str(original).unwrap();
            let v2: serde_json::Value = serde_json::from_str(&result).unwrap();
            assert_eq!(v1, v2);
            hew_json_free(val);
        }
    }

    #[test]
    fn type_checking_all_variants() {
        // SAFETY: All pointers come from parse() which returns valid HewJsonValue.
        unsafe {
            let null_val = parse("null");
            assert_eq!(hew_json_type(null_val), 0);
            hew_json_free(null_val);

            let bool_val = parse("false");
            assert_eq!(hew_json_type(bool_val), 1);
            assert_eq!(hew_json_get_bool(bool_val), 0);
            hew_json_free(bool_val);

            let int_val = parse("42");
            assert_eq!(hew_json_type(int_val), 2);
            assert_eq!(hew_json_get_int(int_val), 42);
            hew_json_free(int_val);

            let float_val = parse("3.14");
            assert_eq!(hew_json_type(float_val), 3);
            let f = hew_json_get_float(float_val);
            assert!((f - 3.14).abs() < f64::EPSILON);
            hew_json_free(float_val);

            let str_val = parse(r#""hello""#);
            assert_eq!(hew_json_type(str_val), 4);
            let s = read_and_free_cstr(hew_json_get_string(str_val));
            assert_eq!(s, "hello");
            hew_json_free(str_val);

            let arr_val = parse("[]");
            assert_eq!(hew_json_type(arr_val), 5);
            assert_eq!(hew_json_array_len(arr_val), 0);
            hew_json_free(arr_val);

            let obj_val = parse("{}");
            assert_eq!(hew_json_type(obj_val), 6);
            hew_json_free(obj_val);

            // Null pointer returns -1.
            assert_eq!(hew_json_type(std::ptr::null()), -1);
        }
    }

    #[test]
    fn object_keys() {
        let val = parse(r#"{"x":1,"y":2}"#);
        assert!(!val.is_null());

        // SAFETY: val is a valid HewJsonValue from parse.
        unsafe {
            let keys = hew_json_object_keys(val);
            assert!(!keys.is_null());
            assert_eq!(hew_json_type(keys), 5); // array
            assert_eq!(hew_json_array_len(keys), 2);
            hew_json_free(keys);
            hew_json_free(val);
        }
    }

    #[test]
    fn parse_invalid_returns_null() {
        let val = parse("{invalid json}}}");
        assert!(val.is_null());

        // SAFETY: null pointer is safe for hew_json_parse.
        unsafe {
            assert!(hew_json_parse(std::ptr::null()).is_null());
        }
    }

    #[test]
    fn float_via_get_float() {
        let val = parse("2.718");
        assert!(!val.is_null());

        // SAFETY: val is a valid HewJsonValue from parse.
        unsafe {
            let f = hew_json_get_float(val);
            assert!((f - 2.718).abs() < 1e-10);
            hew_json_free(val);
        }
    }

    #[test]
    fn object_builder_with_nested() {
        // SAFETY: All pointers come from builder functions.
        unsafe {
            // Build inner object: {"enabled": true, "count": 5}
            let inner_obj = hew_json_object_new();
            let k_enabled = CString::new("enabled").unwrap();
            hew_json_object_set_bool(inner_obj, k_enabled.as_ptr(), 1);
            let k_count = CString::new("count").unwrap();
            hew_json_object_set_int(inner_obj, k_count.as_ptr(), 5);

            // Build array: [1, 2, 3]
            let arr = hew_json_array_new();
            hew_json_array_push_int(arr, 1);
            hew_json_array_push_int(arr, 2);
            hew_json_array_push_int(arr, 3);

            // Build outer object with nested children
            let obj = hew_json_object_new();
            let k_name = CString::new("name").unwrap();
            let v_name = CString::new("test").unwrap();
            hew_json_object_set_string(obj, k_name.as_ptr(), v_name.as_ptr());
            let k_config = CString::new("config").unwrap();
            hew_json_object_set(obj, k_config.as_ptr(), inner_obj);
            // inner_obj is consumed — do not free it.
            let k_items = CString::new("items").unwrap();
            hew_json_object_set(obj, k_items.as_ptr(), arr);
            // arr is consumed — do not free it.

            // Stringify and verify structure
            let json_str = hew_json_stringify(obj);
            let result = read_and_free_cstr(json_str);
            let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();

            assert_eq!(parsed["name"], "test");
            assert_eq!(parsed["config"]["enabled"], true);
            assert_eq!(parsed["config"]["count"], 5);
            assert_eq!(parsed["items"][0], 1);
            assert_eq!(parsed["items"][1], 2);
            assert_eq!(parsed["items"][2], 3);

            hew_json_free(obj);
        }
    }

    #[test]
    fn array_construction() {
        // SAFETY: All pointers come from builder functions.
        unsafe {
            let arr = hew_json_array_new();
            assert!(!arr.is_null());
            assert_eq!(hew_json_type(arr), 5); // array
            assert_eq!(hew_json_array_len(arr), 0);

            // Push mixed types
            hew_json_array_push_bool(arr, 1);
            hew_json_array_push_int(arr, 42);
            hew_json_array_push_float(arr, 3.14);
            let s = CString::new("hello").unwrap();
            hew_json_array_push_string(arr, s.as_ptr());
            hew_json_array_push_null(arr);

            // Push a nested object via hew_json_array_push
            let child = hew_json_object_new();
            let k = CString::new("nested").unwrap();
            hew_json_object_set_bool(child, k.as_ptr(), 0);
            hew_json_array_push(arr, child);
            // child is consumed — do not free it.

            assert_eq!(hew_json_array_len(arr), 6);

            // Verify element values
            let e0 = hew_json_array_get(arr, 0);
            assert_eq!(hew_json_type(e0), 1); // bool
            assert_eq!(hew_json_get_bool(e0), 1);
            hew_json_free(e0);

            let e1 = hew_json_array_get(arr, 1);
            assert_eq!(hew_json_type(e1), 2); // int
            assert_eq!(hew_json_get_int(e1), 42);
            hew_json_free(e1);

            let e2 = hew_json_array_get(arr, 2);
            assert_eq!(hew_json_type(e2), 3); // float
            assert!((hew_json_get_float(e2) - 3.14).abs() < f64::EPSILON);
            hew_json_free(e2);

            let e3 = hew_json_array_get(arr, 3);
            assert_eq!(hew_json_type(e3), 4); // string
            let e3_str = read_and_free_cstr(hew_json_get_string(e3));
            assert_eq!(e3_str, "hello");
            hew_json_free(e3);

            let e4 = hew_json_array_get(arr, 4);
            assert_eq!(hew_json_type(e4), 0); // null
            hew_json_free(e4);

            let e5 = hew_json_array_get(arr, 5);
            assert_eq!(hew_json_type(e5), 6); // object
            hew_json_free(e5);

            hew_json_free(arr);
        }
    }

    #[test]
    fn scalar_constructors() {
        // SAFETY: All pointers come from scalar constructor functions.
        unsafe {
            // Bool
            let bool_val = hew_json_from_bool(1);
            assert!(!bool_val.is_null());
            assert_eq!(hew_json_type(bool_val), 1);
            assert_eq!(hew_json_get_bool(bool_val), 1);
            hew_json_free(bool_val);

            let b_false = hew_json_from_bool(0);
            assert_eq!(hew_json_get_bool(b_false), 0);
            hew_json_free(b_false);

            // Int
            let int_val = hew_json_from_int(-99);
            assert!(!int_val.is_null());
            assert_eq!(hew_json_type(int_val), 2);
            assert_eq!(hew_json_get_int(int_val), -99);
            hew_json_free(int_val);

            // Float
            let float_val = hew_json_from_float(2.718);
            assert!(!float_val.is_null());
            assert_eq!(hew_json_type(float_val), 3);
            assert!((hew_json_get_float(float_val) - 2.718).abs() < 1e-10);
            hew_json_free(float_val);

            // Float: NaN returns null
            assert!(hew_json_from_float(f64::NAN).is_null());

            // Float: infinity returns null
            assert!(hew_json_from_float(f64::INFINITY).is_null());

            // String
            let s_val = CString::new("colour").unwrap();
            let str_val = hew_json_from_string(s_val.as_ptr());
            assert!(!str_val.is_null());
            assert_eq!(hew_json_type(str_val), 4);
            let s_str = read_and_free_cstr(hew_json_get_string(str_val));
            assert_eq!(s_str, "colour");
            hew_json_free(str_val);

            // String: null input returns null
            assert!(hew_json_from_string(std::ptr::null()).is_null());

            // Null
            let null_val = hew_json_from_null();
            assert!(!null_val.is_null());
            assert_eq!(hew_json_type(null_val), 0);
            hew_json_free(null_val);
        }
    }

    #[test]
    fn null_handling() {
        // SAFETY: All pointers come from builder functions.
        unsafe {
            // Object with null field
            let obj = hew_json_object_new();
            let k = CString::new("empty").unwrap();
            hew_json_object_set_null(obj, k.as_ptr());

            let field = hew_json_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_json_type(field), 0); // null
            hew_json_free(field);

            // Verify stringification includes the null field
            let json_str = hew_json_stringify(obj);
            let result = read_and_free_cstr(json_str);
            let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
            assert!(parsed["empty"].is_null());

            hew_json_free(obj);

            // Array with null element
            let arr = hew_json_array_new();
            hew_json_array_push_null(arr);
            assert_eq!(hew_json_array_len(arr), 1);

            let elem = hew_json_array_get(arr, 0);
            assert!(!elem.is_null());
            assert_eq!(hew_json_type(elem), 0); // null
            hew_json_free(elem);

            hew_json_free(arr);
        }
    }

    // -----------------------------------------------------------------------
    // FFI boundary: null-pointer safety
    // -----------------------------------------------------------------------

    #[test]
    fn null_pointer_safety_all_getters() {
        // Every getter must handle a null pointer without crashing.
        // SAFETY: testing null-pointer behaviour on all getter functions.
        unsafe {
            assert!(hew_json_parse(std::ptr::null()).is_null());
            let err = read_and_free_cstr(hew_json_last_error());
            assert!(!err.is_empty());
            assert_eq!(hew_json_type(std::ptr::null()), -1);
            assert_eq!(hew_json_get_bool(std::ptr::null()), 0);
            assert_eq!(hew_json_get_int(std::ptr::null()), 0);
            assert!((hew_json_get_float(std::ptr::null())).abs() < f64::EPSILON);
            assert!(hew_json_get_string(std::ptr::null()).is_null());
            assert!(hew_json_get_field(std::ptr::null(), std::ptr::null()).is_null());
            assert_eq!(hew_json_array_len(std::ptr::null()), -1);
            assert!(hew_json_array_get(std::ptr::null(), 0).is_null());
            assert!(hew_json_object_keys(std::ptr::null()).is_null());
            assert!(hew_json_stringify(std::ptr::null()).is_null());
        }
    }

    #[test]
    fn parse_failure_sets_last_error() {
        let bad = parse("{invalid json}");
        assert!(bad.is_null());

        // SAFETY: hew_json_last_error returns a malloc-allocated C string.
        let err = unsafe { read_and_free_cstr(hew_json_last_error()) };
        assert!(!err.is_empty());
    }

    #[test]
    fn parse_success_clears_last_error() {
        assert!(parse("{invalid json}").is_null());

        let ok = parse("{}");
        assert!(!ok.is_null());

        // SAFETY: hew_json_last_error returns a malloc-allocated C string.
        let err = unsafe { read_and_free_cstr(hew_json_last_error()) };
        assert!(err.is_empty());

        // SAFETY: ok is a valid pointer returned by parse.
        unsafe { hew_json_free(ok) };
    }

    #[test]
    fn null_pointer_safety_all_setters() {
        // Every setter/push must be a no-op with null pointers.
        // SAFETY: testing null-pointer behaviour on all builder functions.
        unsafe {
            hew_json_object_set_bool(std::ptr::null_mut(), std::ptr::null(), 1);
            hew_json_object_set_int(std::ptr::null_mut(), std::ptr::null(), 1);
            hew_json_object_set_float(std::ptr::null_mut(), std::ptr::null(), 1.0);
            hew_json_object_set_string(std::ptr::null_mut(), std::ptr::null(), std::ptr::null());
            hew_json_object_set_char(std::ptr::null_mut(), std::ptr::null(), 65);
            hew_json_object_set_duration(std::ptr::null_mut(), std::ptr::null(), 1_000_000);
            hew_json_object_set_null(std::ptr::null_mut(), std::ptr::null());
            hew_json_object_set(std::ptr::null_mut(), std::ptr::null(), std::ptr::null_mut());
            hew_json_array_push_bool(std::ptr::null_mut(), 1);
            hew_json_array_push_int(std::ptr::null_mut(), 1);
            hew_json_array_push_float(std::ptr::null_mut(), 1.0);
            hew_json_array_push_string(std::ptr::null_mut(), std::ptr::null());
            hew_json_array_push_null(std::ptr::null_mut());
            hew_json_array_push(std::ptr::null_mut(), std::ptr::null_mut());

            // get_char / get_duration on null must return 0.
            assert_eq!(hew_json_get_char(std::ptr::null()), 0);
            assert_eq!(hew_json_get_duration(std::ptr::null()), 0);

            // Free on null must also be a no-op.
            hew_json_free(std::ptr::null_mut());
            hew_json_string_free(std::ptr::null_mut());
        }
    }

    #[test]
    fn set_char_round_trips_bmp_codepoint() {
        // SAFETY: obj is valid from hew_json_object_new.
        unsafe {
            let obj = hew_json_object_new();
            let k = CString::new("cp").unwrap();
            hew_json_object_set_char(obj, k.as_ptr(), 0x41); // 'A' = 65
            let field = hew_json_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_json_get_char(field), 0x41);
            hew_json_free(field);
            hew_json_free(obj);
        }
    }

    #[test]
    fn set_duration_round_trips_nanoseconds() {
        let ns: i64 = 1_500_000_000; // 1.5 seconds in nanoseconds
                                     // SAFETY: obj is valid from hew_json_object_new.
        unsafe {
            let obj = hew_json_object_new();
            let k = CString::new("dur").unwrap();
            hew_json_object_set_duration(obj, k.as_ptr(), ns);
            let field = hew_json_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_json_get_duration(field), ns);
            hew_json_free(field);
            hew_json_free(obj);
        }
    }

    #[test]
    fn set_duration_round_trips_negative_nanoseconds() {
        let ns: i64 = -500_000_000; // 0.5 seconds in the past
                                    // SAFETY: obj is valid from hew_json_object_new.
        unsafe {
            let obj = hew_json_object_new();
            let k = CString::new("past").unwrap();
            hew_json_object_set_duration(obj, k.as_ptr(), ns);
            let field = hew_json_get_field(obj, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_json_get_duration(field), ns);
            hew_json_free(field);
            hew_json_free(obj);
        }
    }

    #[test]
    fn get_field_null_key_returns_null() {
        let val = parse(r#"{"a":1}"#);
        assert!(!val.is_null());
        // SAFETY: val is valid; passing null key.
        unsafe {
            assert!(hew_json_get_field(val, std::ptr::null()).is_null());
            hew_json_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Type mismatch: getters return safe defaults on wrong type
    // -----------------------------------------------------------------------

    #[test]
    fn type_mismatch_get_int_on_string_returns_zero() {
        let val = parse(r#""not a number""#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_json_get_int(val), 0);
            hew_json_free(val);
        }
    }

    #[test]
    fn type_mismatch_get_string_on_int_returns_null() {
        let val = parse("42");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_json_get_string(val).is_null());
            hew_json_free(val);
        }
    }

    #[test]
    fn type_mismatch_get_bool_on_string_returns_zero() {
        let val = parse(r#""true""#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_json_get_bool(val), 0);
            hew_json_free(val);
        }
    }

    #[test]
    fn type_mismatch_get_float_on_string_returns_zero() {
        let val = parse(r#""3.14""#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!((hew_json_get_float(val)).abs() < f64::EPSILON);
            hew_json_free(val);
        }
    }

    #[test]
    fn array_len_on_non_array_returns_negative_one() {
        let val = parse(r#"{"key":"value"}"#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert_eq!(hew_json_array_len(val), -1);
            hew_json_free(val);
        }
    }

    #[test]
    fn object_keys_on_non_object_returns_null() {
        let val = parse("[1, 2, 3]");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_json_object_keys(val).is_null());
            hew_json_free(val);
        }
    }

    #[test]
    fn get_field_on_non_object_returns_null() {
        let val = parse("[1, 2]");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let k = CString::new("key").unwrap();
            assert!(hew_json_get_field(val, k.as_ptr()).is_null());
            hew_json_free(val);
        }
    }

    #[test]
    fn get_field_missing_key_returns_null() {
        let val = parse(r#"{"a":1}"#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let k = CString::new("nonexistent").unwrap();
            assert!(hew_json_get_field(val, k.as_ptr()).is_null());
            hew_json_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Array index boundary conditions
    // -----------------------------------------------------------------------

    #[test]
    fn array_get_negative_index_returns_null() {
        let val = parse("[1, 2, 3]");
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_json_array_get(val, -1).is_null());
            assert!(hew_json_array_get(val, i32::MIN).is_null());
            hew_json_free(val);
        }
    }

    #[test]
    fn array_get_on_non_array_returns_null() {
        let val = parse(r#"{"key":"val"}"#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            assert!(hew_json_array_get(val, 0).is_null());
            hew_json_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Builder operations on wrong type are silent no-ops
    // -----------------------------------------------------------------------

    #[test]
    fn builder_set_on_array_is_noop() {
        // SAFETY: arr is a valid array from hew_json_array_new.
        unsafe {
            let arr = hew_json_array_new();
            let k = CString::new("key").unwrap();
            let v = CString::new("val").unwrap();

            hew_json_object_set_bool(arr, k.as_ptr(), 1);
            hew_json_object_set_int(arr, k.as_ptr(), 42);
            hew_json_object_set_float(arr, k.as_ptr(), 1.5);
            hew_json_object_set_string(arr, k.as_ptr(), v.as_ptr());
            hew_json_object_set_null(arr, k.as_ptr());

            // Array is still empty — setters were no-ops.
            assert_eq!(hew_json_array_len(arr), 0);
            hew_json_free(arr);
        }
    }

    #[test]
    fn builder_push_on_object_is_noop() {
        // SAFETY: obj is a valid object from hew_json_object_new.
        unsafe {
            let obj = hew_json_object_new();
            let s = CString::new("test").unwrap();

            hew_json_array_push_bool(obj, 1);
            hew_json_array_push_int(obj, 42);
            hew_json_array_push_float(obj, 1.5);
            hew_json_array_push_string(obj, s.as_ptr());
            hew_json_array_push_null(obj);

            // Object is still empty — pushes were no-ops.
            let keys = hew_json_object_keys(obj);
            assert_eq!(hew_json_array_len(keys), 0);
            hew_json_free(keys);
            hew_json_free(obj);
        }
    }

    // -----------------------------------------------------------------------
    // Float edge cases at the FFI boundary
    // -----------------------------------------------------------------------

    #[test]
    fn object_set_float_nan_silently_discarded() {
        // NaN is not representable in JSON; the setter must not insert it.
        // SAFETY: obj is a valid object.
        unsafe {
            let obj = hew_json_object_new();
            let k = CString::new("bad").unwrap();
            hew_json_object_set_float(obj, k.as_ptr(), f64::NAN);

            // Field should not exist.
            assert!(hew_json_get_field(obj, k.as_ptr()).is_null());
            hew_json_free(obj);
        }
    }

    #[test]
    fn object_set_float_infinity_silently_discarded() {
        // SAFETY: obj is a valid object.
        unsafe {
            let obj = hew_json_object_new();
            let k = CString::new("inf").unwrap();
            hew_json_object_set_float(obj, k.as_ptr(), f64::INFINITY);
            assert!(hew_json_get_field(obj, k.as_ptr()).is_null());

            let k2 = CString::new("neginf").unwrap();
            hew_json_object_set_float(obj, k2.as_ptr(), f64::NEG_INFINITY);
            assert!(hew_json_get_field(obj, k2.as_ptr()).is_null());

            hew_json_free(obj);
        }
    }

    #[test]
    fn array_push_float_nan_silently_discarded() {
        // SAFETY: arr is a valid array.
        unsafe {
            let arr = hew_json_array_new();
            hew_json_array_push_float(arr, f64::NAN);
            hew_json_array_push_float(arr, f64::INFINITY);

            // Neither value should have been pushed.
            assert_eq!(hew_json_array_len(arr), 0);
            hew_json_free(arr);
        }
    }

    // -----------------------------------------------------------------------
    // Integer boundary values
    // -----------------------------------------------------------------------

    #[test]
    fn integer_boundary_values() {
        // SAFETY: all pointers from parse/from_int.
        unsafe {
            let max_val = hew_json_from_int(i64::MAX);
            assert_eq!(hew_json_get_int(max_val), i64::MAX);
            hew_json_free(max_val);

            let min_val = hew_json_from_int(i64::MIN);
            assert_eq!(hew_json_get_int(min_val), i64::MIN);
            hew_json_free(min_val);

            let zero_val = hew_json_from_int(0);
            assert_eq!(hew_json_get_int(zero_val), 0);
            hew_json_free(zero_val);

            // Roundtrip i64::MAX through parse→stringify→parse.
            let max_str = format!("{}", i64::MAX);
            let parsed = parse(&max_str);
            assert!(!parsed.is_null());
            assert_eq!(hew_json_get_int(parsed), i64::MAX);
            hew_json_free(parsed);
        }
    }

    // -----------------------------------------------------------------------
    // Unicode through the CString FFI boundary
    // -----------------------------------------------------------------------

    #[test]
    fn unicode_emoji_roundtrip() {
        let val = parse(r#""Hello 🌍🎉 world""#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let s = read_and_free_cstr(hew_json_get_string(val));
            assert_eq!(s, "Hello 🌍🎉 world");
            hew_json_free(val);
        }
    }

    #[test]
    fn unicode_escape_sequences_decoded() {
        // JSON \uXXXX escapes should be decoded by the parser.
        let val = parse(r#""\u0048\u0065\u0077""#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let s = read_and_free_cstr(hew_json_get_string(val));
            assert_eq!(s, "Hew");
            hew_json_free(val);
        }
    }

    #[test]
    fn unicode_multibyte_in_object_key() {
        let val = parse(r#"{"clé":42}"#);
        assert!(!val.is_null());
        // SAFETY: val is valid.
        unsafe {
            let k = CString::new("clé").unwrap();
            let field = hew_json_get_field(val, k.as_ptr());
            assert!(!field.is_null());
            assert_eq!(hew_json_get_int(field), 42);
            hew_json_free(field);
            hew_json_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Malformed input error handling
    // -----------------------------------------------------------------------

    #[test]
    fn malformed_json_missing_closing_brace() {
        assert!(parse(r#"{"key": 1"#).is_null());
    }

    #[test]
    fn malformed_json_trailing_comma_in_array() {
        // Trailing commas are invalid in strict JSON.
        assert!(parse("[1, 2, 3,]").is_null());
    }

    #[test]
    fn malformed_json_single_quotes() {
        assert!(parse("{'key': 'val'}").is_null());
    }

    #[test]
    fn malformed_json_unterminated_string() {
        assert!(parse(r#"{"key": "unterminated"#).is_null());
    }

    #[test]
    fn empty_string_parses_as_error() {
        // An empty string is not valid JSON.
        assert!(parse("").is_null());
    }

    // -----------------------------------------------------------------------
    // Deeply nested structure
    // -----------------------------------------------------------------------

    #[test]
    fn deeply_nested_object_parse_and_access() {
        // Build 50 levels: {"a":{"a":{"a":...42...}}}
        let mut json = "42".to_string();
        for _ in 0..50 {
            json = format!(r#"{{"a":{json}}}"#);
        }
        let val = parse(&json);
        assert!(!val.is_null());

        // Walk 50 levels deep and verify the leaf.
        // SAFETY: val is valid from parse.
        unsafe {
            let key = CString::new("a").unwrap();
            let mut current = val;
            for _ in 0..50 {
                let next = hew_json_get_field(current, key.as_ptr());
                assert!(!next.is_null());
                if current != val {
                    hew_json_free(current);
                }
                current = next;
            }
            assert_eq!(hew_json_get_int(current), 42);
            hew_json_free(current);
            hew_json_free(val);
        }
    }

    // -----------------------------------------------------------------------
    // Roundtrip: build → stringify → parse → verify
    // -----------------------------------------------------------------------

    #[test]
    fn roundtrip_complex_builder_structure() {
        // Build: {"items":[{"id":1,"label":"α"},{"id":2,"label":"β"}],"count":2}
        // SAFETY: all pointers from builder functions.
        unsafe {
            let root = hew_json_object_new();
            let items = hew_json_array_new();

            for (id, label) in [(1_i64, "α"), (2, "β")] {
                let item = hew_json_object_new();
                let k_id = CString::new("id").unwrap();
                hew_json_object_set_int(item, k_id.as_ptr(), id);
                let k_label = CString::new("label").unwrap();
                let v_label = CString::new(label).unwrap();
                hew_json_object_set_string(item, k_label.as_ptr(), v_label.as_ptr());
                hew_json_array_push(items, item);
            }

            let k_items = CString::new("items").unwrap();
            hew_json_object_set(root, k_items.as_ptr(), items);
            let k_count = CString::new("count").unwrap();
            hew_json_object_set_int(root, k_count.as_ptr(), 2);

            // Stringify and re-parse.
            let json_str = hew_json_stringify(root);
            let json_text = read_and_free_cstr(json_str);
            hew_json_free(root);

            let reparsed = parse(&json_text);
            assert!(!reparsed.is_null());

            // Verify count survived.
            let count_field = hew_json_get_field(reparsed, k_count.as_ptr());
            assert_eq!(hew_json_get_int(count_field), 2);
            hew_json_free(count_field);

            // Verify items[1].label == "β".
            let items_field = hew_json_get_field(reparsed, k_items.as_ptr());
            assert_eq!(hew_json_array_len(items_field), 2);
            let item1 = hew_json_array_get(items_field, 1);
            let k_label = CString::new("label").unwrap();
            let label_field = hew_json_get_field(item1, k_label.as_ptr());
            let label_str = read_and_free_cstr(hew_json_get_string(label_field));
            assert_eq!(label_str, "β");
            hew_json_free(label_field);
            hew_json_free(item1);
            hew_json_free(items_field);
            hew_json_free(reparsed);
        }
    }
}
