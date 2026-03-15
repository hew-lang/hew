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

use hew_cabi::cabi::str_to_malloc;
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

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a JSON string into a [`HewJsonValue`].
///
/// Returns null on parse error or invalid input.
///
/// # Safety
///
/// `json_str` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_json_parse(json_str: *const c_char) -> *mut HewJsonValue {
    if json_str.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: json_str is a valid NUL-terminated C string per caller contract.
    let Ok(s) = unsafe { CStr::from_ptr(json_str) }.to_str() else {
        return std::ptr::null_mut();
    };
    match serde_json::from_str::<serde_json::Value>(s) {
        Ok(val) => boxed_value(val),
        Err(_) => std::ptr::null_mut(),
    }
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
}
