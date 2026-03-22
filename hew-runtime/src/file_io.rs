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
        Ok(0) => {
            crate::hew_clear_error();
            std::ptr::null_mut()
        }
        Err(e) => {
            crate::set_last_error(format!("hew_stdin_read_line: {e}"));
            std::ptr::null_mut()
        }
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
                Err(e) => {
                    crate::set_last_error(format!(
                        "hew_stdin_read_line: input contains interior NUL at byte {}",
                        e.nul_position()
                    ));
                    std::ptr::null_mut()
                }
            }
        }
    }
}

/// Read the raw bytes of a file into a `bytes` `HewVec` (i32 elements).
///
/// Returns an empty `HewVec` on error.
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

/// Write a `bytes` `HewVec` to a file, overwriting any existing content.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
/// `data` must be a valid, non-null pointer to a `HewVec` (i32 elements).
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

// ---------------------------------------------------------------------------
// Directory operations
// ---------------------------------------------------------------------------

/// Create a directory.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_mkdir(path: *const c_char) -> i32 {
    if path.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return -1;
    };
    if std::fs::create_dir(rust_path).is_ok() {
        0
    } else {
        -1
    }
}

/// Create a directory and all its parent components.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_mkdir_all(path: *const c_char) -> i32 {
    if path.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return -1;
    };
    if std::fs::create_dir_all(rust_path).is_ok() {
        0
    } else {
        -1
    }
}

/// List entries in a directory.
///
/// Returns a `HewVec` of entry names (not full paths). Caller must free with
/// `hew_vec_free`. Returns an empty vec on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_list_dir(path: *const c_char) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new_str allocates a valid empty HewVec<String>.
    let v = unsafe { crate::vec::hew_vec_new_str() };
    if path.is_null() {
        return v;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return v;
    };
    let Ok(entries) = std::fs::read_dir(rust_path) else {
        return v;
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Ok(name_str) = name.into_string() else {
            continue;
        };
        let Ok(c_name) = CString::new(name_str) else {
            continue;
        };
        // SAFETY: v is a valid HewVec; c_name is a valid NUL-terminated C string.
        unsafe { crate::vec::hew_vec_push_str(v, c_name.as_ptr()) };
    }
    v
}

/// Rename or move a file or directory.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// Both `from` and `to` must be valid, NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_rename(from: *const c_char, to: *const c_char) -> i32 {
    if from.is_null() || to.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `from` is a valid NUL-terminated C string.
    let Ok(from_str) = (unsafe { CStr::from_ptr(from) }).to_str() else {
        return -1;
    };
    // SAFETY: caller guarantees `to` is a valid NUL-terminated C string.
    let Ok(to_str) = (unsafe { CStr::from_ptr(to) }).to_str() else {
        return -1;
    };
    if std::fs::rename(from_str, to_str).is_ok() {
        0
    } else {
        -1
    }
}

/// Copy a file.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// Both `from` and `to` must be valid, NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_copy(from: *const c_char, to: *const c_char) -> i32 {
    if from.is_null() || to.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `from` is a valid NUL-terminated C string.
    let Ok(from_str) = (unsafe { CStr::from_ptr(from) }).to_str() else {
        return -1;
    };
    // SAFETY: caller guarantees `to` is a valid NUL-terminated C string.
    let Ok(to_str) = (unsafe { CStr::from_ptr(to) }).to_str() else {
        return -1;
    };
    if std::fs::copy(from_str, to_str).is_ok() {
        0
    } else {
        -1
    }
}

/// Check whether a path is a directory.
///
/// Returns 1 if the path is a directory, 0 otherwise.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_is_dir(path: *const c_char) -> i32 {
    if path.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return 0;
    };
    i32::from(std::path::Path::new(rust_path).is_dir())
}

#[cfg(test)]
mod fs_tests {
    use super::*;

    #[test]
    fn test_mkdir_and_is_dir() {
        let dir = std::env::temp_dir().join("hew_test_mkdir");
        let _ = std::fs::remove_dir_all(&dir);
        let path = CString::new(dir.to_str().unwrap()).unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let rc = unsafe { hew_fs_mkdir(path.as_ptr()) };
        assert_eq!(rc, 0);
        // SAFETY: path is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_fs_is_dir(path.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_mkdir_all_nested() {
        let dir = std::env::temp_dir().join("hew_test_mkdir_all/a/b/c");
        let _ = std::fs::remove_dir_all(std::env::temp_dir().join("hew_test_mkdir_all"));
        let path = CString::new(dir.to_str().unwrap()).unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let rc = unsafe { hew_fs_mkdir_all(path.as_ptr()) };
        assert_eq!(rc, 0);
        // SAFETY: path is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_fs_is_dir(path.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(std::env::temp_dir().join("hew_test_mkdir_all"));
    }

    #[test]
    fn test_list_dir() {
        let dir = std::env::temp_dir().join("hew_test_list_dir");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("alpha.txt"), "a").unwrap();
        std::fs::write(dir.join("beta.txt"), "b").unwrap();
        let path = CString::new(dir.to_str().unwrap()).unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let v = unsafe { hew_fs_list_dir(path.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec.
        let len = unsafe { crate::vec::hew_vec_len(v) };
        assert_eq!(len, 2);
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_rename_file() {
        let dir = std::env::temp_dir().join("hew_test_rename");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let src = dir.join("src.txt");
        let dst = dir.join("dst.txt");
        std::fs::write(&src, "hello").unwrap();
        let from = CString::new(src.to_str().unwrap()).unwrap();
        let to = CString::new(dst.to_str().unwrap()).unwrap();
        // SAFETY: both args are valid NUL-terminated C strings.
        let rc = unsafe { hew_fs_rename(from.as_ptr(), to.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(dst.exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_copy_file() {
        let dir = std::env::temp_dir().join("hew_test_copy");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let src = dir.join("orig.txt");
        let dst = dir.join("copy.txt");
        std::fs::write(&src, "hello").unwrap();
        let from = CString::new(src.to_str().unwrap()).unwrap();
        let to = CString::new(dst.to_str().unwrap()).unwrap();
        // SAFETY: both args are valid NUL-terminated C strings.
        let rc = unsafe { hew_fs_copy(from.as_ptr(), to.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(dst.exists());
        assert!(src.exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_is_dir_null() {
        // SAFETY: Null pointer is explicitly handled.
        assert_eq!(unsafe { hew_fs_is_dir(std::ptr::null()) }, 0);
    }
}
