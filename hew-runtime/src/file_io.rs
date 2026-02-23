//! Hew runtime: `file_io` module.
//!
//! Synchronous file I/O operations with C ABI.

use std::ffi::{c_char, CStr, CString};

/// Read an entire file and return a heap-allocated C string.
///
/// Returns a null pointer on error (invalid path, I/O failure, etc.).
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string. The caller is responsible
/// for calling `free()` on the returned pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `path` is a valid, NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return std::ptr::null_mut();
    };
    let Ok(contents) = std::fs::read_to_string(rust_path) else {
        return std::ptr::null_mut();
    };
    let Ok(c_contents) = CString::new(contents) else {
        return std::ptr::null_mut();
    };
    // SAFETY: `libc::strdup` copies a NUL-terminated string to the C heap; caller must free.
    unsafe { libc::strdup(c_contents.as_ptr()) }
}

/// Write a string to a file, overwriting any existing content.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// Both `path` and `content` must be valid, NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_file_write(path: *const c_char, content: *const c_char) -> i32 {
    if path.is_null() || content.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees both pointers are valid C strings.
    let (c_path, c_content) = unsafe { (CStr::from_ptr(path), CStr::from_ptr(content)) };
    let (Ok(rust_path), Ok(rust_content)) = (c_path.to_str(), c_content.to_str()) else {
        return -1;
    };
    if std::fs::write(rust_path, rust_content).is_ok() {
        0
    } else {
        -1
    }
}

/// Append a string to a file.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// Both `path` and `content` must be valid, NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_file_append(path: *const c_char, content: *const c_char) -> i32 {
    use std::io::Write;

    if path.is_null() || content.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees both pointers are valid C strings.
    let (c_path, c_content) = unsafe { (CStr::from_ptr(path), CStr::from_ptr(content)) };
    let (Ok(rust_path), Ok(rust_content)) = (c_path.to_str(), c_content.to_str()) else {
        return -1;
    };
    let Ok(mut file) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(rust_path)
    else {
        return -1;
    };
    if file.write_all(rust_content.as_bytes()).is_ok() {
        0
    } else {
        -1
    }
}

/// Check whether a file exists.
///
/// Returns 1 if the file exists, 0 if not.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_exists(path: *const c_char) -> i32 {
    if path.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `path` is a valid C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return 0;
    };
    i32::from(std::path::Path::new(rust_path).exists())
}

/// Delete a file.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_delete(path: *const c_char) -> i32 {
    if path.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `path` is a valid C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return -1;
    };
    if std::fs::remove_file(rust_path).is_ok() {
        0
    } else {
        -1
    }
}

/// Return the size of a file in bytes.
///
/// Returns -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_size(path: *const c_char) -> i64 {
    if path.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `path` is a valid C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return -1;
    };
    match std::fs::metadata(rust_path) {
        Ok(meta) => {
            let len = meta.len();
            // Saturate to i64::MAX if the file is absurdly large.
            i64::try_from(len).unwrap_or(i64::MAX)
        }
        Err(_) => -1,
    }
}

/// Read one line from stdin and return a heap-allocated C string.
///
/// Returns a null pointer on EOF or error.
///
/// # Safety
///
/// The caller is responsible for calling `free()` (via `CString::from_raw`)
/// on the returned pointer.
#[no_mangle]
pub extern "C" fn hew_stdin_read_line() -> *mut c_char {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(0) | Err(_) => std::ptr::null_mut(),
        Ok(_) => {
            // Trim the trailing newline, if present.
            if buf.ends_with('\n') {
                buf.pop();
                if buf.ends_with('\r') {
                    buf.pop();
                }
            }
            match CString::new(buf) {
                Ok(cs) => cs.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
    }
}

/// Read the raw bytes of a file into a `bytes` HewVec (i32 elements).
///
/// Returns an empty HewVec on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_bytes(path: *const c_char) -> *mut crate::vec::HewVec {
    if path.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { crate::vec::hew_vec_new() };
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { crate::vec::hew_vec_new() };
    };
    match std::fs::read(rust_path) {
        Ok(data) => {
            // SAFETY: data slice is valid.
            unsafe { crate::vec::u8_to_hwvec(&data) }
        }
        Err(_) => {
            // SAFETY: hew_vec_new allocates a valid empty HewVec.
            unsafe { crate::vec::hew_vec_new() }
        }
    }
}

/// Write a `bytes` HewVec to a file, overwriting any existing content.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
/// `data` must be a valid, non-null pointer to a HewVec (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_file_write_bytes(
    path: *const c_char,
    data: *mut crate::vec::HewVec,
) -> i32 {
    if path.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return -1;
    };
    // SAFETY: data validity forwarded to hwvec_to_u8.
    let bytes = unsafe { crate::vec::hwvec_to_u8(data) };
    if std::fs::write(rust_path, &bytes).is_ok() {
        0
    } else {
        -1
    }
}
