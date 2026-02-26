//! Hew `std::encoding::yaml` — YAML parsing and generation.
//!
//! Provides YAML parsing, serialization, and value access for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. All returned [`HewYamlValue`] pointers are heap-allocated
//! via `Box` and must be freed with [`hew_yaml_free`].

use hew_cabi::cabi::str_to_malloc;
use std::ffi::CStr;
use std::os::raw::c_char;

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

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a YAML string into a [`HewYamlValue`].
///
/// Returns null on parse error or invalid input.
///
/// # Safety
///
/// `yaml_str` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_yaml_parse(yaml_str: *const c_char) -> *mut HewYamlValue {
    if yaml_str.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: yaml_str is a valid NUL-terminated C string per caller contract.
    let Ok(s) = (unsafe { CStr::from_ptr(yaml_str) }).to_str() else {
        return std::ptr::null_mut();
    };
    match serde_yaml::from_str::<serde_yaml::Value>(s) {
        Ok(val) => boxed_value(val),
        Err(_) => std::ptr::null_mut(),
    }
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

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
}
