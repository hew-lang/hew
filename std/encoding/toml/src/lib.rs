//! Hew runtime: `toml_parser` module.
//!
//! Provides TOML parsing and value inspection for compiled Hew programs.
//! All returned strings are allocated with `libc::malloc` so callers can
//! free them with `libc::free`. Opaque [`HewTomlValue`] handles must be
//! freed with [`hew_toml_free`].

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::str_to_malloc;
use std::ffi::CStr;
use std::os::raw::c_char;

/// Opaque wrapper around a [`toml::Value`].
///
/// Heap-allocated via `Box`; must be freed with [`hew_toml_free`].
#[derive(Debug)]
pub struct HewTomlValue {
    inner: toml::Value,
}

/// Wrap a [`toml::Value`] into a heap-allocated [`HewTomlValue`].
fn boxed_value(v: toml::Value) -> *mut HewTomlValue {
    Box::into_raw(Box::new(HewTomlValue { inner: v }))
}

/// Parse a TOML string into an opaque [`HewTomlValue`].
///
/// Returns null on parse error or invalid input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null, which returns null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_parse(s: *const c_char) -> *mut HewTomlValue {
    if s.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let Ok(rust_str) = unsafe { CStr::from_ptr(s) }.to_str() else {
        return std::ptr::null_mut();
    };
    match rust_str.parse::<toml::Table>() {
        Ok(table) => Box::into_raw(Box::new(HewTomlValue {
            inner: toml::Value::Table(table),
        })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return the type of a TOML value.
///
/// Type codes: 0 = string, 1 = integer, 2 = float, 3 = boolean,
/// 4 = datetime, 5 = array, 6 = table. Returns -1 on null input.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_type(val: *const HewTomlValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::String(_) => 0,
        toml::Value::Integer(_) => 1,
        toml::Value::Float(_) => 2,
        toml::Value::Boolean(_) => 3,
        toml::Value::Datetime(_) => 4,
        toml::Value::Array(_) => 5,
        toml::Value::Table(_) => 6,
    }
}

/// Get the string value from a TOML string value.
///
/// Returns a `malloc`-allocated NUL-terminated C string. The caller must
/// free it with `libc::free`. Returns null if `val` is null or not a string.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_string(val: *const HewTomlValue) -> *mut c_char {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::String(s) => str_to_malloc(s),
        _ => std::ptr::null_mut(),
    }
}

/// Get the integer value from a TOML integer value.
///
/// Returns 0 if `val` is null or not an integer.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_int(val: *const HewTomlValue) -> i64 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Integer(i) => *i,
        _ => 0,
    }
}

/// Get the float value from a TOML float value.
///
/// Returns 0.0 if `val` is null or not a float.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_float(val: *const HewTomlValue) -> f64 {
    if val.is_null() {
        return 0.0;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Float(f) => *f,
        _ => 0.0,
    }
}

/// Get the boolean value from a TOML boolean value.
///
/// Returns 0 (false) or 1 (true). Returns 0 if `val` is null or not a boolean.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_bool(val: *const HewTomlValue) -> i32 {
    if val.is_null() {
        return 0;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Boolean(b) => i32::from(*b),
        _ => 0,
    }
}

/// Look up a field in a TOML table by key.
///
/// Returns a new heap-allocated [`HewTomlValue`] (cloned) or null if the
/// key does not exist, `val` is null, or `val` is not a table.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `key` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_get_field(
    val: *const HewTomlValue,
    key: *const c_char,
) -> *mut HewTomlValue {
    if val.is_null() || key.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: key is a valid NUL-terminated C string per caller contract.
    let Ok(key_str) = unsafe { CStr::from_ptr(key) }.to_str() else {
        return std::ptr::null_mut();
    };
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    let toml::Value::Table(table) = &(unsafe { &*val }.inner) else {
        return std::ptr::null_mut();
    };
    match table.get(key_str) {
        Some(v) => Box::into_raw(Box::new(HewTomlValue { inner: v.clone() })),
        None => std::ptr::null_mut(),
    }
}

/// Return the number of elements in a TOML array.
///
/// Returns -1 if `val` is null or not an array.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_len(val: *const HewTomlValue) -> i32 {
    if val.is_null() {
        return -1;
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    match &unsafe { &*val }.inner {
        toml::Value::Array(a) => i32::try_from(a.len()).unwrap_or(i32::MAX),
        _ => -1,
    }
}

/// Get an element from a TOML array by index.
///
/// Returns a new heap-allocated [`HewTomlValue`] (cloned) or null if out
/// of bounds, `val` is null, or `val` is not an array.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_get(
    val: *const HewTomlValue,
    index: i32,
) -> *mut HewTomlValue {
    if val.is_null() || index < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    let toml::Value::Array(arr) = &(unsafe { &*val }.inner) else {
        return std::ptr::null_mut();
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    match arr.get(index as usize) {
        Some(v) => Box::into_raw(Box::new(HewTomlValue { inner: v.clone() })),
        None => std::ptr::null_mut(),
    }
}

/// Serialize a TOML value back to a TOML-formatted string.
///
/// Returns a `malloc`-allocated NUL-terminated C string. The caller must
/// free it with `libc::free`. Returns null if `val` is null or serialization
/// fails.
///
/// # Safety
///
/// `val` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_stringify(val: *const HewTomlValue) -> *mut c_char {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is a valid pointer to a HewTomlValue per caller contract.
    let v = &unsafe { &*val }.inner;
    match toml::to_string(v) {
        Ok(s) => str_to_malloc(&s),
        Err(_) => std::ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Table builder — typed field setters
// ---------------------------------------------------------------------------

/// Create a new empty TOML table.
///
/// Returns a heap-allocated [`HewTomlValue`] wrapping an empty table.
/// Must be freed with [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_table_new() -> *mut HewTomlValue {
    boxed_value(toml::Value::Table(toml::map::Map::new()))
}

/// Set a boolean field on a TOML table.
///
/// Does nothing if `tbl` is null, not a table, or `key` is null.
///
/// # Safety
///
/// `tbl` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `key` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_bool(
    tbl: *mut HewTomlValue,
    key: *const c_char,
    val: i32,
) {
    if tbl.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees tbl is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::Boolean(val != 0));
    }
}

/// Set an integer field on a TOML table.
///
/// # Safety
///
/// Same as [`hew_toml_table_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_int(
    tbl: *mut HewTomlValue,
    key: *const c_char,
    val: i64,
) {
    if tbl.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees tbl is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::Integer(val));
    }
}

/// Set a float field on a TOML table.
///
/// # Safety
///
/// Same as [`hew_toml_table_set_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_float(
    tbl: *mut HewTomlValue,
    key: *const c_char,
    val: f64,
) {
    if tbl.is_null() || key.is_null() {
        return;
    }
    // SAFETY: caller guarantees tbl is valid; key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::Float(val));
    }
}

/// Set a string field on a TOML table. The string value is copied.
///
/// # Safety
///
/// Same as [`hew_toml_table_set_bool`]. `val` must be a valid NUL-terminated
/// C string (or null, in which case this is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set_string(
    tbl: *mut HewTomlValue,
    key: *const c_char,
    val: *const c_char,
) {
    if tbl.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees tbl is valid; key and val are valid NUL-terminated strings.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let val = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, toml::Value::String(val));
    }
}

/// Set a [`HewTomlValue`] child on a TOML table, taking ownership.
///
/// The `val` pointer is consumed and must not be used or freed after this call.
/// Does nothing if `tbl` is null, not a table, `key` is null, or `val` is null.
///
/// # Safety
///
/// `tbl` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `key` must be a valid NUL-terminated C string (or null).
/// `val` must be a pointer previously returned by a function in this module
/// and must not have been freed already (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_table_set(
    tbl: *mut HewTomlValue,
    key: *const c_char,
    val: *mut HewTomlValue,
) {
    if tbl.is_null() || key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees key is a valid NUL-terminated string.
    let key = unsafe { CStr::from_ptr(key) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    let child = unsafe { Box::from_raw(val) };
    // SAFETY: tbl is non-null (checked above) and valid per caller contract.
    if let toml::Value::Table(map) = &mut unsafe { &mut *tbl }.inner {
        map.insert(key, child.inner);
    }
}

// ---------------------------------------------------------------------------
// Array builder — typed element pushers
// ---------------------------------------------------------------------------

/// Create a new empty TOML array.
///
/// Returns a heap-allocated [`HewTomlValue`] wrapping an empty array.
/// Must be freed with [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_array_new() -> *mut HewTomlValue {
    boxed_value(toml::Value::Array(Vec::new()))
}

/// Push a boolean onto a TOML array.
///
/// Does nothing if `arr` is null or not an array.
///
/// # Safety
///
/// `arr` must be a valid pointer to a [`HewTomlValue`] (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_bool(arr: *mut HewTomlValue, val: i32) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::Boolean(val != 0));
    }
}

/// Push an integer onto a TOML array.
///
/// # Safety
///
/// Same as [`hew_toml_array_push_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_int(arr: *mut HewTomlValue, val: i64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::Integer(val));
    }
}

/// Push a float onto a TOML array.
///
/// # Safety
///
/// Same as [`hew_toml_array_push_bool`].
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_float(arr: *mut HewTomlValue, val: f64) {
    if arr.is_null() {
        return;
    }
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::Float(val));
    }
}

/// Push a string onto a TOML array. The string value is copied.
///
/// Does nothing if `arr` is null, not an array, or `val` is null.
///
/// # Safety
///
/// `arr` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `val` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push_string(arr: *mut HewTomlValue, val: *const c_char) {
    if arr.is_null() || val.is_null() {
        return;
    }
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let val = unsafe { CStr::from_ptr(val) }
        .to_str()
        .unwrap_or("")
        .to_owned();
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(toml::Value::String(val));
    }
}

/// Push a [`HewTomlValue`] child onto a TOML array, taking ownership.
///
/// The `val` pointer is consumed and must not be used or freed after this call.
/// Does nothing if `arr` is null, not an array, or `val` is null.
///
/// # Safety
///
/// `arr` must be a valid pointer to a [`HewTomlValue`] (or null).
/// `val` must be a pointer previously returned by a function in this module
/// and must not have been freed already (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_array_push(arr: *mut HewTomlValue, val: *mut HewTomlValue) {
    if arr.is_null() || val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    let child = unsafe { Box::from_raw(val) };
    // SAFETY: arr is non-null (checked above) and valid per caller contract.
    if let toml::Value::Array(vec) = &mut unsafe { &mut *arr }.inner {
        vec.push(child.inner);
    }
}

// ---------------------------------------------------------------------------
// Scalar constructors
// ---------------------------------------------------------------------------

/// Create a TOML boolean value.
///
/// Returns a heap-allocated [`HewTomlValue`]. Must be freed with
/// [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_from_bool(val: i32) -> *mut HewTomlValue {
    boxed_value(toml::Value::Boolean(val != 0))
}

/// Create a TOML integer value.
///
/// Returns a heap-allocated [`HewTomlValue`]. Must be freed with
/// [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_from_int(val: i64) -> *mut HewTomlValue {
    boxed_value(toml::Value::Integer(val))
}

/// Create a TOML float value.
///
/// Returns a heap-allocated [`HewTomlValue`]. Must be freed with
/// [`hew_toml_free`].
#[no_mangle]
pub extern "C" fn hew_toml_from_float(val: f64) -> *mut HewTomlValue {
    boxed_value(toml::Value::Float(val))
}

/// Create a TOML string value. The string is copied.
///
/// Returns null if `val` is null or not valid UTF-8. Must be freed with
/// [`hew_toml_free`].
///
/// # Safety
///
/// `val` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_from_string(val: *const c_char) -> *mut HewTomlValue {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: val is non-null (checked above) and valid per caller contract.
    let Ok(s) = unsafe { CStr::from_ptr(val) }.to_str() else {
        return std::ptr::null_mut();
    };
    boxed_value(toml::Value::String(s.to_owned()))
}

/// Free a [`HewTomlValue`] previously returned by any function in this
/// module.
///
/// # Safety
///
/// `val` must be a pointer previously returned by a function in this module
/// and must not have been freed already (or null, which is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_toml_free(val: *mut HewTomlValue) {
    if val.is_null() {
        return;
    }
    // SAFETY: val was allocated with Box::into_raw and has not been freed.
    let _ = unsafe { Box::from_raw(val) };
}

#[cfg(test)]
#[expect(
    clippy::approx_constant,
    reason = "test data uses hardcoded floats, not mathematical constants"
)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_parse_and_get_string() {
        let input = CString::new("name = \"hew\"").expect("CString::new failed");
        // SAFETY: input is a valid CString.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        let key = CString::new("name").expect("CString::new failed");
        // SAFETY: root and key are valid.
        let field = unsafe { hew_toml_get_field(root, key.as_ptr()) };
        assert!(!field.is_null());
        // SAFETY: field is valid.
        assert_eq!(unsafe { hew_toml_type(field) }, 0); // string

        // SAFETY: field is a string value.
        let s = unsafe { hew_toml_get_string(field) };
        assert!(!s.is_null());
        // SAFETY: s is a valid NUL-terminated C string from malloc_str.
        let result = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
        assert_eq!(result, "hew");

        // SAFETY: s was allocated with libc::malloc.
        unsafe { libc::free(s.cast()) };
        // SAFETY: field was allocated by this module.
        unsafe { hew_toml_free(field) };
        // SAFETY: root was allocated by this module.
        unsafe { hew_toml_free(root) };
    }

    #[test]
    fn test_parse_numeric_types() {
        let input =
            CString::new("port = 8080\npi = 3.14\nenabled = true").expect("CString::new failed");
        // SAFETY: input is a valid CString.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        let key_port = CString::new("port").expect("CString::new failed");
        // SAFETY: root and key_port are valid.
        let port = unsafe { hew_toml_get_field(root, key_port.as_ptr()) };
        assert!(!port.is_null());
        // SAFETY: port is valid.
        assert_eq!(unsafe { hew_toml_type(port) }, 1); // integer
                                                       // SAFETY: port is a valid integer TOML value.
        assert_eq!(unsafe { hew_toml_get_int(port) }, 8080);

        let key_pi = CString::new("pi").expect("CString::new failed");
        // SAFETY: root and key_pi are valid.
        let pi = unsafe { hew_toml_get_field(root, key_pi.as_ptr()) };
        assert!(!pi.is_null());
        // SAFETY: pi is valid.
        assert_eq!(unsafe { hew_toml_type(pi) }, 2); // float
                                                     // SAFETY: pi is a valid float TOML value.
        let pi_val = unsafe { hew_toml_get_float(pi) };
        assert!((pi_val - 3.14).abs() < f64::EPSILON);

        let key_en = CString::new("enabled").expect("CString::new failed");
        // SAFETY: root and key_en are valid.
        let en = unsafe { hew_toml_get_field(root, key_en.as_ptr()) };
        assert!(!en.is_null());
        // SAFETY: en is valid.
        assert_eq!(unsafe { hew_toml_type(en) }, 3); // boolean
                                                     // SAFETY: en is a valid boolean TOML value.
        assert_eq!(unsafe { hew_toml_get_bool(en) }, 1);

        // SAFETY: all pointers were allocated by this module.
        unsafe {
            hew_toml_free(en);
            hew_toml_free(pi);
            hew_toml_free(port);
            hew_toml_free(root);
        }
    }

    #[test]
    fn test_array_access() {
        let input = CString::new("ports = [80, 443, 8080]").expect("CString::new failed");
        // SAFETY: input is a valid CString.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        let key = CString::new("ports").expect("CString::new failed");
        // SAFETY: root and key are valid.
        let arr = unsafe { hew_toml_get_field(root, key.as_ptr()) };
        assert!(!arr.is_null());
        // SAFETY: arr is valid.
        assert_eq!(unsafe { hew_toml_type(arr) }, 5); // array
                                                      // SAFETY: arr is a valid array TOML value.
        assert_eq!(unsafe { hew_toml_array_len(arr) }, 3);

        // SAFETY: arr is a valid array value.
        let elem = unsafe { hew_toml_array_get(arr, 1) };
        assert!(!elem.is_null());
        // SAFETY: elem is valid.
        assert_eq!(unsafe { hew_toml_get_int(elem) }, 443);

        // Out-of-bounds returns null.
        // SAFETY: arr is valid.
        let oob = unsafe { hew_toml_array_get(arr, 10) };
        assert!(oob.is_null());

        // SAFETY: all pointers were allocated by this module.
        unsafe {
            hew_toml_free(elem);
            hew_toml_free(arr);
            hew_toml_free(root);
        }
    }

    #[test]
    fn test_null_inputs() {
        // All functions must handle null gracefully.
        // SAFETY: testing null handling.
        unsafe {
            assert!(hew_toml_parse(std::ptr::null()).is_null());
            assert_eq!(hew_toml_type(std::ptr::null()), -1);
            assert!(hew_toml_get_string(std::ptr::null()).is_null());
            assert_eq!(hew_toml_get_int(std::ptr::null()), 0);
            assert!((hew_toml_get_float(std::ptr::null())).abs() < f64::EPSILON);
            assert_eq!(hew_toml_get_bool(std::ptr::null()), 0);
            assert!(hew_toml_get_field(std::ptr::null(), std::ptr::null()).is_null());
            assert_eq!(hew_toml_array_len(std::ptr::null()), -1);
            assert!(hew_toml_array_get(std::ptr::null(), 0).is_null());
            assert!(hew_toml_stringify(std::ptr::null()).is_null());
            hew_toml_free(std::ptr::null_mut()); // must not crash
        }
    }

    #[test]
    fn test_stringify_roundtrip() {
        let input = CString::new("key = \"value\"").expect("CString::new failed");
        // SAFETY: input is a valid CString.
        let root = unsafe { hew_toml_parse(input.as_ptr()) };
        assert!(!root.is_null());

        // SAFETY: root is valid.
        let s = unsafe { hew_toml_stringify(root) };
        assert!(!s.is_null());
        // SAFETY: s is a valid NUL-terminated C string.
        let roundtrip = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
        assert!(roundtrip.contains("key"));
        assert!(roundtrip.contains("value"));

        // SAFETY: s was allocated with libc::malloc; root by this module.
        unsafe {
            libc::free(s.cast());
            hew_toml_free(root);
        }
    }

    // -----------------------------------------------------------------------
    // Builder tests
    // -----------------------------------------------------------------------

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn test_table_builder_typed_setters() {
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let tbl = hew_toml_table_new();
            assert!(!tbl.is_null());
            assert_eq!(hew_toml_type(tbl), 6); // table

            let k_name = CString::new("name").unwrap();
            let v_name = CString::new("hew").unwrap();
            hew_toml_table_set_string(tbl, k_name.as_ptr(), v_name.as_ptr());

            let k_port = CString::new("port").unwrap();
            hew_toml_table_set_int(tbl, k_port.as_ptr(), 8080);

            let k_pi = CString::new("pi").unwrap();
            hew_toml_table_set_float(tbl, k_pi.as_ptr(), 3.14);

            let k_en = CString::new("enabled").unwrap();
            hew_toml_table_set_bool(tbl, k_en.as_ptr(), 1);

            // Read back via getter API.
            let name = hew_toml_get_field(tbl, k_name.as_ptr());
            assert!(!name.is_null());
            assert_eq!(read_and_free_cstr(hew_toml_get_string(name)), "hew");
            hew_toml_free(name);

            let port = hew_toml_get_field(tbl, k_port.as_ptr());
            assert!(!port.is_null());
            assert_eq!(hew_toml_get_int(port), 8080);
            hew_toml_free(port);

            let pi = hew_toml_get_field(tbl, k_pi.as_ptr());
            assert!(!pi.is_null());
            assert!((hew_toml_get_float(pi) - 3.14).abs() < f64::EPSILON);
            hew_toml_free(pi);

            let en = hew_toml_get_field(tbl, k_en.as_ptr());
            assert!(!en.is_null());
            assert_eq!(hew_toml_get_bool(en), 1);
            hew_toml_free(en);

            hew_toml_free(tbl);
        }
    }

    #[test]
    fn test_table_set_value_child() {
        // Build a nested structure: { server = { host = "localhost", port = 9090 } }
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let inner = hew_toml_table_new();
            let k_host = CString::new("host").unwrap();
            let v_host = CString::new("localhost").unwrap();
            hew_toml_table_set_string(inner, k_host.as_ptr(), v_host.as_ptr());

            let k_port = CString::new("port").unwrap();
            hew_toml_table_set_int(inner, k_port.as_ptr(), 9090);

            let outer = hew_toml_table_new();
            let k_server = CString::new("server").unwrap();
            // Takes ownership of inner — do not free inner after this.
            hew_toml_table_set(outer, k_server.as_ptr(), inner);

            // Verify nested access.
            let server = hew_toml_get_field(outer, k_server.as_ptr());
            assert!(!server.is_null());
            assert_eq!(hew_toml_type(server), 6); // table

            let host = hew_toml_get_field(server, k_host.as_ptr());
            assert!(!host.is_null());
            assert_eq!(read_and_free_cstr(hew_toml_get_string(host)), "localhost");
            hew_toml_free(host);

            let port = hew_toml_get_field(server, k_port.as_ptr());
            assert!(!port.is_null());
            assert_eq!(hew_toml_get_int(port), 9090);
            hew_toml_free(port);

            hew_toml_free(server);
            hew_toml_free(outer);
        }
    }

    #[test]
    fn test_array_builder_typed_pushers() {
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let arr = hew_toml_array_new();
            assert!(!arr.is_null());
            assert_eq!(hew_toml_type(arr), 5); // array
            assert_eq!(hew_toml_array_len(arr), 0);

            hew_toml_array_push_int(arr, 10);
            hew_toml_array_push_int(arr, 20);
            hew_toml_array_push_int(arr, 30);
            assert_eq!(hew_toml_array_len(arr), 3);

            let elem = hew_toml_array_get(arr, 1);
            assert!(!elem.is_null());
            assert_eq!(hew_toml_get_int(elem), 20);
            hew_toml_free(elem);

            hew_toml_free(arr);
        }
    }

    #[test]
    fn test_array_push_mixed_scalars() {
        // TOML arrays are homogeneous in spec, but toml::Value allows mixed.
        // We test push_bool, push_float, push_string independently.
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let bools = hew_toml_array_new();
            hew_toml_array_push_bool(bools, 1);
            hew_toml_array_push_bool(bools, 0);
            assert_eq!(hew_toml_array_len(bools), 2);
            let b0 = hew_toml_array_get(bools, 0);
            assert_eq!(hew_toml_get_bool(b0), 1);
            hew_toml_free(b0);
            let b1 = hew_toml_array_get(bools, 1);
            assert_eq!(hew_toml_get_bool(b1), 0);
            hew_toml_free(b1);
            hew_toml_free(bools);

            let floats = hew_toml_array_new();
            hew_toml_array_push_float(floats, 1.5);
            hew_toml_array_push_float(floats, 2.5);
            assert_eq!(hew_toml_array_len(floats), 2);
            let f0 = hew_toml_array_get(floats, 0);
            assert!((hew_toml_get_float(f0) - 1.5).abs() < f64::EPSILON);
            hew_toml_free(f0);
            hew_toml_free(floats);

            let strings = hew_toml_array_new();
            let s_a = CString::new("alpha").unwrap();
            let s_b = CString::new("beta").unwrap();
            hew_toml_array_push_string(strings, s_a.as_ptr());
            hew_toml_array_push_string(strings, s_b.as_ptr());
            assert_eq!(hew_toml_array_len(strings), 2);
            let e1 = hew_toml_array_get(strings, 1);
            assert_eq!(read_and_free_cstr(hew_toml_get_string(e1)), "beta");
            hew_toml_free(e1);
            hew_toml_free(strings);
        }
    }

    #[test]
    fn test_array_push_value_child() {
        // Push sub-tables into an array: [[servers]] equivalent.
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let arr = hew_toml_array_new();

            let tbl1 = hew_toml_table_new();
            let k = CString::new("host").unwrap();
            let v = CString::new("alpha").unwrap();
            hew_toml_table_set_string(tbl1, k.as_ptr(), v.as_ptr());
            hew_toml_array_push(arr, tbl1); // takes ownership

            let tbl2 = hew_toml_table_new();
            let v2 = CString::new("beta").unwrap();
            hew_toml_table_set_string(tbl2, k.as_ptr(), v2.as_ptr());
            hew_toml_array_push(arr, tbl2); // takes ownership

            assert_eq!(hew_toml_array_len(arr), 2);

            let elem = hew_toml_array_get(arr, 0);
            assert_eq!(hew_toml_type(elem), 6); // table
            let host = hew_toml_get_field(elem, k.as_ptr());
            assert_eq!(read_and_free_cstr(hew_toml_get_string(host)), "alpha");
            hew_toml_free(host);
            hew_toml_free(elem);

            hew_toml_free(arr);
        }
    }

    #[test]
    fn test_scalar_constructors() {
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let b = hew_toml_from_bool(1);
            assert!(!b.is_null());
            assert_eq!(hew_toml_type(b), 3); // boolean
            assert_eq!(hew_toml_get_bool(b), 1);
            hew_toml_free(b);

            let b_false = hew_toml_from_bool(0);
            assert_eq!(hew_toml_get_bool(b_false), 0);
            hew_toml_free(b_false);

            let i = hew_toml_from_int(42);
            assert!(!i.is_null());
            assert_eq!(hew_toml_type(i), 1); // integer
            assert_eq!(hew_toml_get_int(i), 42);
            hew_toml_free(i);

            let f = hew_toml_from_float(2.718);
            assert!(!f.is_null());
            assert_eq!(hew_toml_type(f), 2); // float
            assert!((hew_toml_get_float(f) - 2.718).abs() < f64::EPSILON);
            hew_toml_free(f);

            let cs = CString::new("hello").unwrap();
            let s = hew_toml_from_string(cs.as_ptr());
            assert!(!s.is_null());
            assert_eq!(hew_toml_type(s), 0); // string
            assert_eq!(read_and_free_cstr(hew_toml_get_string(s)), "hello");
            hew_toml_free(s);

            // Null input returns null.
            assert!(hew_toml_from_string(std::ptr::null()).is_null());
        }
    }

    #[test]
    fn test_builder_stringify_roundtrip() {
        // Build { name = "hew", version = 1, tags = ["fast", "safe"] }
        // then stringify and re-parse.
        // SAFETY: all pointers come from this module's builder functions.
        unsafe {
            let tbl = hew_toml_table_new();

            let k_name = CString::new("name").unwrap();
            let v_name = CString::new("hew").unwrap();
            hew_toml_table_set_string(tbl, k_name.as_ptr(), v_name.as_ptr());

            let k_ver = CString::new("version").unwrap();
            hew_toml_table_set_int(tbl, k_ver.as_ptr(), 1);

            let tags = hew_toml_array_new();
            let t1 = CString::new("fast").unwrap();
            let t2 = CString::new("safe").unwrap();
            hew_toml_array_push_string(tags, t1.as_ptr());
            hew_toml_array_push_string(tags, t2.as_ptr());

            let k_tags = CString::new("tags").unwrap();
            hew_toml_table_set(tbl, k_tags.as_ptr(), tags); // takes ownership

            // Stringify.
            let s = hew_toml_stringify(tbl);
            assert!(!s.is_null());
            let toml_str = read_and_free_cstr(s);
            assert!(toml_str.contains("name"));
            assert!(toml_str.contains("hew"));
            assert!(toml_str.contains("version"));

            // Re-parse the stringified output.
            let cs = CString::new(toml_str).unwrap();
            let reparsed = hew_toml_parse(cs.as_ptr());
            assert!(!reparsed.is_null());

            let name = hew_toml_get_field(reparsed, k_name.as_ptr());
            assert!(!name.is_null());
            assert_eq!(read_and_free_cstr(hew_toml_get_string(name)), "hew");
            hew_toml_free(name);

            let ver = hew_toml_get_field(reparsed, k_ver.as_ptr());
            assert!(!ver.is_null());
            assert_eq!(hew_toml_get_int(ver), 1);
            hew_toml_free(ver);

            let tags_field = hew_toml_get_field(reparsed, k_tags.as_ptr());
            assert!(!tags_field.is_null());
            assert_eq!(hew_toml_array_len(tags_field), 2);
            let tag0 = hew_toml_array_get(tags_field, 0);
            assert_eq!(read_and_free_cstr(hew_toml_get_string(tag0)), "fast");
            hew_toml_free(tag0);
            hew_toml_free(tags_field);

            hew_toml_free(reparsed);
            hew_toml_free(tbl);
        }
    }

    #[test]
    fn test_builder_null_safety() {
        // All builder functions must handle null gracefully.
        // SAFETY: testing null handling.
        unsafe {
            // Table setters with null table.
            let k = CString::new("k").unwrap();
            let v = CString::new("v").unwrap();
            hew_toml_table_set_bool(std::ptr::null_mut(), k.as_ptr(), 1);
            hew_toml_table_set_int(std::ptr::null_mut(), k.as_ptr(), 1);
            hew_toml_table_set_float(std::ptr::null_mut(), k.as_ptr(), 1.0);
            hew_toml_table_set_string(std::ptr::null_mut(), k.as_ptr(), v.as_ptr());
            hew_toml_table_set(std::ptr::null_mut(), k.as_ptr(), std::ptr::null_mut());

            // Table setters with null key.
            let tbl = hew_toml_table_new();
            hew_toml_table_set_bool(tbl, std::ptr::null(), 1);
            hew_toml_table_set_int(tbl, std::ptr::null(), 1);
            hew_toml_table_set_float(tbl, std::ptr::null(), 1.0);
            hew_toml_table_set_string(tbl, std::ptr::null(), v.as_ptr());
            hew_toml_table_set(tbl, std::ptr::null(), std::ptr::null_mut());
            // Table should still be empty.
            assert_eq!(hew_toml_array_len(tbl), -1); // not an array → -1
            hew_toml_free(tbl);

            // Array pushers with null array.
            hew_toml_array_push_bool(std::ptr::null_mut(), 1);
            hew_toml_array_push_int(std::ptr::null_mut(), 1);
            hew_toml_array_push_float(std::ptr::null_mut(), 1.0);
            hew_toml_array_push_string(std::ptr::null_mut(), v.as_ptr());
            hew_toml_array_push(std::ptr::null_mut(), std::ptr::null_mut());

            // Array push_string with null string.
            let arr = hew_toml_array_new();
            hew_toml_array_push_string(arr, std::ptr::null());
            assert_eq!(hew_toml_array_len(arr), 0); // nothing pushed
            hew_toml_free(arr);
        }
    }
}
