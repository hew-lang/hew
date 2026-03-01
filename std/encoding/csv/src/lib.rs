//! Hew runtime: `csv_parser` module.
//!
//! Provides CSV parsing and cell access for compiled Hew programs.
//! All returned strings are allocated with `libc::malloc` and NUL-terminated.
//! All returned [`HewCsvTable`] pointers are heap-allocated via `Box` and must
//! be freed with [`hew_csv_free`].

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

/// Parsed CSV table.
///
/// Returned by [`hew_csv_parse`], [`hew_csv_parse_no_headers`], and
/// [`hew_csv_parse_file`]. Must be freed with [`hew_csv_free`].
#[derive(Debug)]
pub struct HewCsvTable {
    headers: Vec<String>,
    rows: Vec<Vec<String>>,
}

/// Parse a CSV string. The first row is treated as headers.
///
/// Returns a heap-allocated [`HewCsvTable`], or null on error.
/// The caller must free it with [`hew_csv_free`].
///
/// # Safety
///
/// `data` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_parse(data: *const c_char) -> *mut HewCsvTable {
    if data.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: data is a valid NUL-terminated C string per caller contract.
    let Some(s) = (unsafe { cstr_to_str(data) }) else {
        return std::ptr::null_mut();
    };
    parse_csv_str(s, true)
}

/// Parse a CSV string without a header row.
///
/// Auto-generates headers as `"col0"`, `"col1"`, etc.
/// Returns a heap-allocated [`HewCsvTable`], or null on error.
/// The caller must free it with [`hew_csv_free`].
///
/// # Safety
///
/// `data` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_parse_no_headers(data: *const c_char) -> *mut HewCsvTable {
    if data.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: data is a valid NUL-terminated C string per caller contract.
    let Some(s) = (unsafe { cstr_to_str(data) }) else {
        return std::ptr::null_mut();
    };
    parse_csv_str(s, false)
}

/// Parse a CSV file at `path`. The first row is treated as headers.
///
/// Returns a heap-allocated [`HewCsvTable`], or null on error.
/// The caller must free it with [`hew_csv_free`].
///
/// # Safety
///
/// `path` must be a valid NUL-terminated C string containing a filesystem path.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_parse_file(path: *const c_char) -> *mut HewCsvTable {
    if path.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: path is a valid NUL-terminated C string per caller contract.
    let Some(path_str) = (unsafe { cstr_to_str(path) }) else {
        return std::ptr::null_mut();
    };
    let Ok(content) = std::fs::read_to_string(path_str) else {
        return std::ptr::null_mut();
    };
    parse_csv_str(&content, true)
}

/// Return the number of data rows (excluding the header row).
///
/// # Safety
///
/// `table` must be a valid pointer to a [`HewCsvTable`] allocated by one of
/// the parse functions.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_rows(table: *const HewCsvTable) -> i32 {
    if table.is_null() {
        return 0;
    }
    // SAFETY: table is a valid HewCsvTable pointer per caller contract.
    let t = unsafe { &*table };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "C ABI: CSV row count fits in i32"
    )]
    {
        t.rows.len() as i32
    }
}

/// Return the number of columns.
///
/// # Safety
///
/// `table` must be a valid pointer to a [`HewCsvTable`] allocated by one of
/// the parse functions.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_cols(table: *const HewCsvTable) -> i32 {
    if table.is_null() {
        return 0;
    }
    // SAFETY: table is a valid HewCsvTable pointer per caller contract.
    let t = unsafe { &*table };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "C ABI: CSV column count fits in i32"
    )]
    {
        t.headers.len() as i32
    }
}

/// Get the header name at `col`. Returns a `malloc`-allocated, NUL-terminated
/// C string. Returns null if `col` is out of bounds.
///
/// # Safety
///
/// `table` must be a valid pointer to a [`HewCsvTable`].
#[no_mangle]
pub unsafe extern "C" fn hew_csv_header(table: *const HewCsvTable, col: i32) -> *mut c_char {
    if table.is_null() || col < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: table is a valid HewCsvTable pointer per caller contract.
    let t = unsafe { &*table };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    let idx = col as usize;
    match t.headers.get(idx) {
        Some(h) => str_to_malloc(h),
        None => std::ptr::null_mut(),
    }
}

/// Get the cell value at (`row`, `col`). Returns a `malloc`-allocated,
/// NUL-terminated C string. Returns null if out of bounds.
///
/// # Safety
///
/// `table` must be a valid pointer to a [`HewCsvTable`].
#[no_mangle]
pub unsafe extern "C" fn hew_csv_get(table: *const HewCsvTable, row: i32, col: i32) -> *mut c_char {
    if table.is_null() || row < 0 || col < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: table is a valid HewCsvTable pointer per caller contract.
    let t = unsafe { &*table };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    let r = row as usize;
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    let c = col as usize;
    t.rows
        .get(r)
        .and_then(|row_vec| row_vec.get(c))
        .map_or(std::ptr::null_mut(), |val| str_to_malloc(val))
}

/// Get the cell value at (`row`, column named `col_name`). Returns a
/// `malloc`-allocated, NUL-terminated C string. Returns null if the column
/// name is not found or `row` is out of bounds.
///
/// # Safety
///
/// `table` must be a valid pointer to a [`HewCsvTable`].
/// `col_name` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_get_by_name(
    table: *const HewCsvTable,
    row: i32,
    col_name: *const c_char,
) -> *mut c_char {
    if table.is_null() || col_name.is_null() || row < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: col_name is a valid NUL-terminated C string per caller contract.
    let Some(name) = (unsafe { cstr_to_str(col_name) }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: table is a valid HewCsvTable pointer per caller contract.
    let t = unsafe { &*table };
    let Some(col_idx) = t.headers.iter().position(|h| h == name) else {
        return std::ptr::null_mut();
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    let r = row as usize;
    t.rows
        .get(r)
        .and_then(|row_vec| row_vec.get(col_idx))
        .map_or(std::ptr::null_mut(), |val| str_to_malloc(val))
}

/// Free a [`HewCsvTable`] previously returned by one of the parse functions.
///
/// # Safety
///
/// `table` must be a pointer previously returned by [`hew_csv_parse`],
/// [`hew_csv_parse_no_headers`], or [`hew_csv_parse_file`], and must not
/// have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_csv_free(table: *mut HewCsvTable) {
    if table.is_null() {
        return;
    }
    // SAFETY: table was allocated with Box::into_raw in parse_csv_str.
    drop(unsafe { Box::from_raw(table) });
}

/// Internal helper: parse a CSV string with or without headers.
fn parse_csv_str(data: &str, has_headers: bool) -> *mut HewCsvTable {
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(has_headers)
        .from_reader(data.as_bytes());

    let headers = if has_headers {
        match reader.headers() {
            Ok(h) => h.iter().map(String::from).collect::<Vec<_>>(),
            Err(_) => return std::ptr::null_mut(),
        }
    } else {
        // Peek at the first record to determine column count.
        Vec::new()
    };

    let mut rows = Vec::new();
    for result in reader.records() {
        let Ok(record) = result else {
            return std::ptr::null_mut();
        };
        rows.push(record.iter().map(String::from).collect::<Vec<_>>());
    }

    // Generate synthetic headers when none were provided.
    let headers = if has_headers {
        headers
    } else {
        let ncols = rows.first().map_or(0, Vec::len);
        (0..ncols).map(|i| format!("col{i}")).collect()
    };

    Box::into_raw(Box::new(HewCsvTable { headers, rows }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    /// Helper: convert a `*mut c_char` from malloc into a Rust String, then free it.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated malloc'd C string.
        let s = unsafe { CStr::from_ptr(ptr) }
            .to_str()
            .expect("valid UTF-8")
            .to_owned();
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn test_parse_simple_csv() {
        let csv_data = CString::new("name,age\nAlice,30\nBob,25").unwrap();
        // SAFETY: csv_data is a valid CString.
        let table = unsafe { hew_csv_parse(csv_data.as_ptr()) };
        assert!(!table.is_null());

        // SAFETY: table is valid from hew_csv_parse.
        assert_eq!(unsafe { hew_csv_rows(table) }, 2);
        // SAFETY: table is valid from hew_csv_parse.
        assert_eq!(unsafe { hew_csv_cols(table) }, 2);

        // SAFETY: table is valid; returned ptr is malloc'd.
        let h0 = unsafe { read_and_free(hew_csv_header(table, 0)) };
        assert_eq!(h0, "name");
        // SAFETY: table is valid; returned ptr is malloc'd.
        let h1 = unsafe { read_and_free(hew_csv_header(table, 1)) };
        assert_eq!(h1, "age");

        // SAFETY: table is valid.
        let val = unsafe { read_and_free(hew_csv_get(table, 0, 0)) };
        assert_eq!(val, "Alice");
        // SAFETY: table is valid; returned ptr is malloc'd.
        let val = unsafe { read_and_free(hew_csv_get(table, 1, 1)) };
        assert_eq!(val, "25");

        // SAFETY: table was allocated by hew_csv_parse.
        unsafe { hew_csv_free(table) };
    }

    #[test]
    fn test_parse_no_headers() {
        let csv_data = CString::new("a,b,c\nd,e,f").unwrap();
        // SAFETY: csv_data is a valid CString.
        let table = unsafe { hew_csv_parse_no_headers(csv_data.as_ptr()) };
        assert!(!table.is_null());

        // SAFETY: table is valid from hew_csv_parse_no_headers.
        assert_eq!(unsafe { hew_csv_rows(table) }, 2);
        // SAFETY: table is valid from hew_csv_parse_no_headers.
        assert_eq!(unsafe { hew_csv_cols(table) }, 3);

        // Synthetic headers.
        // SAFETY: table is valid.
        let h0 = unsafe { read_and_free(hew_csv_header(table, 0)) };
        assert_eq!(h0, "col0");
        // SAFETY: table is valid; returned ptr is malloc'd.
        let h2 = unsafe { read_and_free(hew_csv_header(table, 2)) };
        assert_eq!(h2, "col2");

        // First row should be "a,b,c" since there's no header consumption.
        // SAFETY: table is valid.
        let val = unsafe { read_and_free(hew_csv_get(table, 0, 0)) };
        assert_eq!(val, "a");

        // SAFETY: table was allocated by hew_csv_parse_no_headers.
        unsafe { hew_csv_free(table) };
    }

    #[test]
    fn test_get_by_name() {
        let csv_data = CString::new("city,pop\nTokyo,14000000\nParis,2100000").unwrap();
        // SAFETY: csv_data is a valid CString.
        let table = unsafe { hew_csv_parse(csv_data.as_ptr()) };
        assert!(!table.is_null());

        let col = CString::new("pop").unwrap();
        // SAFETY: table and col are valid.
        let val = unsafe { read_and_free(hew_csv_get_by_name(table, 0, col.as_ptr())) };
        assert_eq!(val, "14000000");

        let col_city = CString::new("city").unwrap();
        // SAFETY: table and col_city are valid.
        let val = unsafe { read_and_free(hew_csv_get_by_name(table, 1, col_city.as_ptr())) };
        assert_eq!(val, "Paris");

        // Non-existent column returns null.
        let bad_col = CString::new("missing").unwrap();
        // SAFETY: table and bad_col are valid.
        let ptr = unsafe { hew_csv_get_by_name(table, 0, bad_col.as_ptr()) };
        assert!(ptr.is_null());

        // SAFETY: table was allocated by hew_csv_parse.
        unsafe { hew_csv_free(table) };
    }

    #[test]
    fn test_parse_file() {
        use std::io::Write;

        let mut tmp = tempfile::NamedTempFile::new().expect("create tempfile");
        write!(tmp, "x,y\n1,2\n3,4").expect("write csv");
        tmp.flush().expect("flush");

        let path = CString::new(tmp.path().to_str().unwrap()).unwrap();
        // SAFETY: path is a valid CString pointing to a readable file.
        let table = unsafe { hew_csv_parse_file(path.as_ptr()) };
        assert!(!table.is_null());

        // SAFETY: table is valid from hew_csv_parse_file.
        assert_eq!(unsafe { hew_csv_rows(table) }, 2);
        // SAFETY: table is valid from hew_csv_parse_file.
        assert_eq!(unsafe { hew_csv_cols(table) }, 2);

        // SAFETY: table is valid.
        let val = unsafe { read_and_free(hew_csv_get(table, 1, 0)) };
        assert_eq!(val, "3");

        // SAFETY: table was allocated by hew_csv_parse_file.
        unsafe { hew_csv_free(table) };
    }

    #[test]
    fn test_empty_csv() {
        // Empty string — headers-only parse yields 0 rows.
        let csv_data = CString::new("").unwrap();
        // SAFETY: csv_data is a valid CString.
        let table = unsafe { hew_csv_parse(csv_data.as_ptr()) };
        assert!(!table.is_null());
        // SAFETY: table is valid.
        assert_eq!(unsafe { hew_csv_rows(table) }, 0);
        // SAFETY: table is valid from hew_csv_parse.
        assert_eq!(unsafe { hew_csv_cols(table) }, 0);

        // Out-of-bounds access returns null.
        // SAFETY: table is valid.
        let ptr = unsafe { hew_csv_get(table, 0, 0) };
        assert!(ptr.is_null());

        // SAFETY: table was allocated by hew_csv_parse.
        unsafe { hew_csv_free(table) };

        // Null input returns null.
        // SAFETY: passing null is the documented error case.
        let table = unsafe { hew_csv_parse(std::ptr::null()) };
        assert!(table.is_null());
    }

    #[test]
    fn test_null_and_bounds() {
        // hew_csv_free with null is a no-op.
        // SAFETY: null is the documented no-op case.
        unsafe { hew_csv_free(std::ptr::null_mut()) };

        // hew_csv_rows/cols with null returns 0.
        // SAFETY: null is handled.
        assert_eq!(unsafe { hew_csv_rows(std::ptr::null()) }, 0);
        // SAFETY: null is handled by hew_csv_cols.
        assert_eq!(unsafe { hew_csv_cols(std::ptr::null()) }, 0);

        // hew_csv_header with null table returns null.
        // SAFETY: null is handled.
        assert!(unsafe { hew_csv_header(std::ptr::null(), 0) }.is_null());

        // hew_csv_get with negative indices returns null.
        let csv_data = CString::new("a\n1").unwrap();
        // SAFETY: csv_data is a valid CString.
        let table = unsafe { hew_csv_parse(csv_data.as_ptr()) };
        assert!(!table.is_null());
        // SAFETY: table is valid; negative index is handled.
        assert!(unsafe { hew_csv_get(table, -1, 0) }.is_null());
        // SAFETY: table is valid; negative index is handled.
        assert!(unsafe { hew_csv_get(table, 0, -1) }.is_null());
        // SAFETY: table was allocated by hew_csv_parse.
        unsafe { hew_csv_free(table) };
    }
}
