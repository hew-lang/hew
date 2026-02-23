//! Integration tests: apply `#[hew_export]` to sample functions and verify
//! the generated `__hew_export_meta_*` companion functions return correct
//! metadata.

use std::ffi::c_char;

use hew_export_macro::hew_export;

// ---------------------------------------------------------------------------
// Sample annotated functions
// ---------------------------------------------------------------------------

#[hew_export(
    module = "std::encoding::json",
    name = "parse",
    doc = "Parse a JSON string"
)]
pub unsafe extern "C" fn hew_json_parse(input: *const c_char) -> *const c_char {
    input // stub
}

#[hew_export(module = "std::file")]
pub unsafe extern "C" fn hew_file_read(path: *const c_char) -> *mut c_char {
    path.cast_mut() // stub
}

#[hew_export(module = "std::file", doc = "Check if a file exists")]
pub extern "C" fn hew_file_exists(path: *const c_char) -> i32 {
    let _ = path;
    0
}

#[hew_export(module = "std::math", name = "add_ints")]
pub extern "C" fn hew_math_add_ints(a: i32, b: i32) -> i32 {
    a + b
}

#[hew_export(module = "std::io", name = "read_line")]
pub extern "C" fn hew_stdin_read_line() -> *mut c_char {
    std::ptr::null_mut()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[test]
fn json_parse_metadata() {
    let meta = __hew_export_meta_hew_json_parse();
    assert_eq!(meta.module, "std::encoding::json");
    assert_eq!(meta.hew_name, "parse");
    assert_eq!(meta.c_name, "hew_json_parse");
    assert_eq!(meta.params, vec![("input", "string")]);
    assert_eq!(meta.return_type, Some("string"));
    assert_eq!(meta.doc, "Parse a JSON string");
}

#[test]
fn file_read_name_derived_from_module() {
    let meta = __hew_export_meta_hew_file_read();
    assert_eq!(meta.module, "std::file");
    // "hew_file_" prefix stripped → "read"
    assert_eq!(meta.hew_name, "read");
    assert_eq!(meta.c_name, "hew_file_read");
    assert_eq!(meta.params, vec![("path", "string")]);
    assert_eq!(meta.return_type, Some("string"));
    assert_eq!(meta.doc, "");
}

#[test]
fn file_exists_returns_i32() {
    let meta = __hew_export_meta_hew_file_exists();
    assert_eq!(meta.hew_name, "exists");
    assert_eq!(meta.return_type, Some("i32"));
    assert_eq!(meta.doc, "Check if a file exists");
}

#[test]
fn math_add_two_i32_params() {
    let meta = __hew_export_meta_hew_math_add_ints();
    assert_eq!(meta.hew_name, "add_ints");
    assert_eq!(meta.params, vec![("a", "i32"), ("b", "i32")]);
    assert_eq!(meta.return_type, Some("i32"));
}

#[test]
fn stdin_read_line_no_params_returns_string() {
    let meta = __hew_export_meta_hew_stdin_read_line();
    assert_eq!(meta.module, "std::io");
    assert_eq!(meta.hew_name, "read_line");
    assert!(meta.params.is_empty());
    assert_eq!(meta.return_type, Some("string"));
}
