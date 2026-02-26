//! Hew runtime: `toml_parser` module.
//!
//! Provides TOML parsing and value inspection for compiled Hew programs.
//! All returned strings are allocated with `libc::malloc` so callers can
//! free them with `libc::free`. Opaque [`HewTomlValue`] handles must be
//! freed with [`hew_toml_free`].

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
}
