//! Hew runtime: `file_io` module.
//!
//! Synchronous file I/O operations with C ABI.
//!
//! All functions that return `*mut c_char` allocate via `libc::malloc`. The
//! caller owns the returned pointer and must free it with `libc::free`.

use crate::cabi::str_to_malloc;
use hew_cabi::sink::set_last_error_with_errno;
use std::ffi::{c_char, CStr, CString};

/// Read an entire file and return a `malloc`-allocated, NUL-terminated C string.
///
/// Returns a null pointer on error (invalid path, I/O failure, interior NUL
/// byte, etc.).
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
///
/// # Ownership
///
/// The caller owns the returned pointer and must free it with `libc::free`.
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
    // Reject files with interior NUL bytes (can't represent as C string).
    if contents.contains('\0') {
        return std::ptr::null_mut();
    }
    str_to_malloc(&contents)
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

/// Check whether any filesystem path exists.
///
/// Returns 1 if the path exists, 0 if not.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_path_exists(path: *const c_char) -> i32 {
    // SAFETY: forwarded unchanged to the existing existence probe.
    unsafe { hew_file_exists(path) }
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

/// Read one line from stdin and return a `malloc`-allocated, NUL-terminated
/// C string with the trailing newline stripped.
///
/// Returns a null pointer on EOF or error.
///
/// # Safety
///
/// No preconditions.
///
/// # Ownership
///
/// The caller owns the returned pointer and must free it with `libc::free`.
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
            if buf.contains('\0') {
                crate::set_last_error("hew_stdin_read_line: input contains interior NUL byte");
                return std::ptr::null_mut();
            }
            str_to_malloc(&buf)
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
    // SAFETY: `path` is the caller-provided C string pointer for this ABI entrypoint.
    let Some(rust_path) = (unsafe { crate::util::cstr_to_str(path, "hew_file_read_bytes") }) else {
        let msg = "hew_file_read_bytes: invalid path string";
        crate::set_last_error(msg);
        set_last_error_with_errno(
            msg.into(),
            22, // EINVAL: Invalid argument
        );
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { crate::vec::hew_vec_new() };
    };
    match std::fs::read(rust_path) {
        Ok(data) => {
            crate::hew_clear_error();
            // SAFETY: data slice is valid.
            unsafe { crate::vec::u8_to_hwvec(&data) }
        }
        Err(error) => {
            let msg = format!("hew_file_read_bytes: {error}");
            crate::set_last_error(&msg);
            set_last_error_with_errno(msg, error.raw_os_error().unwrap_or(0));
            // SAFETY: hew_vec_new allocates a valid empty HewVec.
            unsafe { crate::vec::hew_vec_new() }
        }
    }
}

/// Return a malloc-owned copy of the current thread's last file I/O error.
// WASM-TODO: confirm whether file-I/O error reporting needs a dedicated wasm-visible contract.
#[no_mangle]
pub extern "C" fn hew_file_last_error() -> *mut c_char {
    let ptr = crate::hew_last_error();
    if ptr.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: `ptr` comes from thread-local last-error storage and remains valid for this read.
    let Some(text) = (unsafe { crate::util::cstr_to_str(ptr, "hew_file_last_error") }) else {
        return str_to_malloc("");
    };
    str_to_malloc(text)
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
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// Create a unique temp directory for a single test, cleaning up any prior run.
    fn test_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("hew_fio_{name}"));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// Build a `CString` from a path.
    fn cpath(p: &std::path::Path) -> CString {
        CString::new(p.to_str().unwrap()).unwrap()
    }

    /// Build a `CString` with invalid-UTF-8 bytes (0xFF 0xFE) — valid C string,
    /// but `CStr::to_str()` will fail.
    fn invalid_utf8_cstring() -> CString {
        CString::new(vec![0xFF, 0xFE]).unwrap()
    }

    /// Recover the Rust string that `hew_file_read` returned via `libc::strdup`,
    /// then free the C allocation.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null pointer returned by `libc::strdup`.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(
            !ptr.is_null(),
            "expected non-null pointer from hew_file_read"
        );
        // SAFETY: ptr was returned by libc::strdup → valid, NUL-terminated C string.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated by libc::strdup; returning ownership to the C heap.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    // -----------------------------------------------------------------------
    // hew_file_read
    // -----------------------------------------------------------------------

    #[test]
    fn read_existing_file_returns_contents() {
        let dir = test_dir("read_ok");
        let file = dir.join("hello.txt");
        std::fs::write(&file, "hello world").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string pointing to an existing file.
        let ptr = unsafe { hew_file_read(p.as_ptr()) };
        // SAFETY: ptr was returned by hew_file_read (libc::strdup allocation).
        let contents = unsafe { read_and_free(ptr) };
        assert_eq!(contents, "hello world");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_empty_file_returns_empty_string() {
        let dir = test_dir("read_empty");
        let file = dir.join("empty.txt");
        std::fs::write(&file, "").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        let ptr = unsafe { hew_file_read(p.as_ptr()) };
        // SAFETY: ptr was returned by hew_file_read.
        let contents = unsafe { read_and_free(ptr) };
        assert_eq!(contents, "");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_utf8_content_preserves_encoding() {
        let dir = test_dir("read_utf8");
        let file = dir.join("unicode.txt");
        let text = "café ☕ naïve 日本語";
        std::fs::write(&file, text).unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        let ptr = unsafe { hew_file_read(p.as_ptr()) };
        // SAFETY: ptr was returned by hew_file_read.
        let contents = unsafe { read_and_free(ptr) };
        assert_eq!(contents, text);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_null_path_returns_null() {
        // SAFETY: null is the value under test; the function handles it gracefully.
        let ptr = unsafe { hew_file_read(std::ptr::null()) };
        assert!(ptr.is_null());
    }

    #[test]
    fn read_nonexistent_file_returns_null() {
        let p = CString::new("/tmp/hew_fio_surely_does_not_exist.txt").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let ptr = unsafe { hew_file_read(p.as_ptr()) };
        assert!(ptr.is_null());
    }

    #[test]
    fn read_invalid_utf8_path_returns_null() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        let ptr = unsafe { hew_file_read(bad.as_ptr()) };
        assert!(ptr.is_null());
    }

    // -----------------------------------------------------------------------
    // hew_file_write
    // -----------------------------------------------------------------------

    #[test]
    fn write_creates_new_file() {
        let dir = test_dir("write_new");
        let file = dir.join("new.txt");
        let p = cpath(&file);
        let c = CString::new("content").unwrap();
        // SAFETY: both p and c are valid NUL-terminated C strings.
        let rc = unsafe { hew_file_write(p.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, 0);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "content");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn write_overwrites_existing_content() {
        let dir = test_dir("write_over");
        let file = dir.join("over.txt");
        std::fs::write(&file, "old").unwrap();
        let p = cpath(&file);
        let c = CString::new("new").unwrap();
        // SAFETY: both p and c are valid NUL-terminated C strings.
        let rc = unsafe { hew_file_write(p.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, 0);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "new");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn write_empty_content_creates_empty_file() {
        let dir = test_dir("write_empty");
        let file = dir.join("empty.txt");
        let p = cpath(&file);
        let c = CString::new("").unwrap();
        // SAFETY: both p and c are valid NUL-terminated C strings.
        let rc = unsafe { hew_file_write(p.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, 0);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn write_null_path_returns_error() {
        let c = CString::new("data").unwrap();
        // SAFETY: null path is the value under test; the function handles it.
        let rc = unsafe { hew_file_write(std::ptr::null(), c.as_ptr()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn write_null_content_returns_error() {
        let p = CString::new("/tmp/hew_fio_write_null_content.txt").unwrap();
        // SAFETY: null content is the value under test; the function handles it.
        let rc = unsafe { hew_file_write(p.as_ptr(), std::ptr::null()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn write_to_nonexistent_directory_returns_error() {
        let p = CString::new("/tmp/hew_fio_no_such_dir/sub/file.txt").unwrap();
        let c = CString::new("data").unwrap();
        // SAFETY: both p and c are valid NUL-terminated C strings.
        let rc = unsafe { hew_file_write(p.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn write_invalid_utf8_path_returns_error() {
        let bad = invalid_utf8_cstring();
        let c = CString::new("data").unwrap();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        let rc = unsafe { hew_file_write(bad.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, -1);
    }

    // -----------------------------------------------------------------------
    // hew_file_append
    // -----------------------------------------------------------------------

    #[test]
    fn append_adds_to_existing_file() {
        let dir = test_dir("append_add");
        let file = dir.join("log.txt");
        std::fs::write(&file, "line1\n").unwrap();
        let p = cpath(&file);
        let c = CString::new("line2\n").unwrap();
        // SAFETY: both p and c are valid NUL-terminated C strings.
        let rc = unsafe { hew_file_append(p.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, 0);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "line1\nline2\n");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn append_creates_file_if_missing() {
        let dir = test_dir("append_create");
        let file = dir.join("new.txt");
        let p = cpath(&file);
        let c = CString::new("first").unwrap();
        // SAFETY: both p and c are valid NUL-terminated C strings.
        let rc = unsafe { hew_file_append(p.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, 0);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "first");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn append_null_path_returns_error() {
        let c = CString::new("data").unwrap();
        // SAFETY: null path is the value under test; the function handles it.
        let rc = unsafe { hew_file_append(std::ptr::null(), c.as_ptr()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn append_null_content_returns_error() {
        let p = CString::new("/tmp/hew_fio_append_null.txt").unwrap();
        // SAFETY: null content is the value under test; the function handles it.
        let rc = unsafe { hew_file_append(p.as_ptr(), std::ptr::null()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn append_invalid_utf8_path_returns_error() {
        let bad = invalid_utf8_cstring();
        let c = CString::new("data").unwrap();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        let rc = unsafe { hew_file_append(bad.as_ptr(), c.as_ptr()) };
        assert_eq!(rc, -1);
    }

    // -----------------------------------------------------------------------
    // hew_file_exists
    // -----------------------------------------------------------------------

    #[test]
    fn exists_returns_one_for_existing_file() {
        let dir = test_dir("exists_yes");
        let file = dir.join("present.txt");
        std::fs::write(&file, "x").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_file_exists(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn exists_returns_zero_for_missing_file() {
        let p = CString::new("/tmp/hew_fio_no_such_file_exists.txt").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_file_exists(p.as_ptr()) }, 0);
    }

    #[test]
    fn exists_null_path_returns_zero() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_file_exists(std::ptr::null()) }, 0);
    }

    #[test]
    fn exists_invalid_utf8_path_returns_zero() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        assert_eq!(unsafe { hew_file_exists(bad.as_ptr()) }, 0);
    }

    // -----------------------------------------------------------------------
    // hew_path_exists
    // -----------------------------------------------------------------------

    #[test]
    fn path_exists_returns_one_for_existing_directory() {
        let dir = test_dir("path_exists_dir_yes");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_exists(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn path_exists_returns_zero_for_missing_path() {
        let p = CString::new("/tmp/hew_path_exists_ghost").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_exists(p.as_ptr()) }, 0);
    }

    // -----------------------------------------------------------------------
    // hew_file_delete
    // -----------------------------------------------------------------------

    #[test]
    fn delete_removes_existing_file() {
        let dir = test_dir("delete_ok");
        let file = dir.join("doomed.txt");
        std::fs::write(&file, "bye").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        let rc = unsafe { hew_file_delete(p.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(!file.exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn delete_nonexistent_file_returns_error() {
        let p = CString::new("/tmp/hew_fio_delete_ghost.txt").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let rc = unsafe { hew_file_delete(p.as_ptr()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn delete_null_path_returns_error() {
        // SAFETY: null is the value under test; the function handles it.
        let rc = unsafe { hew_file_delete(std::ptr::null()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn delete_invalid_utf8_path_returns_error() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        let rc = unsafe { hew_file_delete(bad.as_ptr()) };
        assert_eq!(rc, -1);
    }

    // -----------------------------------------------------------------------
    // hew_file_size
    // -----------------------------------------------------------------------

    #[test]
    fn size_returns_correct_byte_count() {
        let dir = test_dir("size_ok");
        let file = dir.join("sized.txt");
        std::fs::write(&file, "12345").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_file_size(p.as_ptr()) }, 5);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn size_empty_file_returns_zero() {
        let dir = test_dir("size_empty");
        let file = dir.join("empty.txt");
        std::fs::write(&file, "").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_file_size(p.as_ptr()) }, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn size_null_path_returns_error() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_file_size(std::ptr::null()) }, -1);
    }

    #[test]
    fn size_nonexistent_file_returns_error() {
        let p = CString::new("/tmp/hew_fio_size_ghost.txt").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_file_size(p.as_ptr()) }, -1);
    }

    #[test]
    fn size_invalid_utf8_path_returns_error() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        assert_eq!(unsafe { hew_file_size(bad.as_ptr()) }, -1);
    }

    #[test]
    fn size_multibyte_utf8_counts_bytes_not_chars() {
        let dir = test_dir("size_utf8");
        let file = dir.join("utf8.txt");
        // "é" is 2 bytes in UTF-8; "☕" is 3 bytes → 5 bytes total
        std::fs::write(&file, "é☕").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_file_size(p.as_ptr()) }, 5);
        let _ = std::fs::remove_dir_all(&dir);
    }

    // -----------------------------------------------------------------------
    // hew_file_read_bytes / hew_file_write_bytes
    // -----------------------------------------------------------------------

    #[test]
    fn read_bytes_roundtrip_with_write_bytes() {
        let dir = test_dir("bytes_rt");
        let file = dir.join("data.bin");
        let data: Vec<u8> = vec![0, 1, 127, 128, 255];
        std::fs::write(&file, &data).unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string; v is returned by hew_file_read_bytes.
        unsafe {
            let v = hew_file_read_bytes(p.as_ptr());
            assert!(!v.is_null());
            let recovered = crate::vec::hwvec_to_u8(v);
            assert_eq!(recovered, data);
            crate::vec::hew_vec_free(v);
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_null_path_returns_empty_vec() {
        // SAFETY: null is the value under test; v is returned by hew_file_read_bytes.
        unsafe {
            let v = hew_file_read_bytes(std::ptr::null());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 0);
            crate::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn read_bytes_nonexistent_file_returns_empty_vec() {
        let dir = test_dir("bytes_missing");
        let file = dir.join("ghost.bin");
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string; v is returned by hew_file_read_bytes.
        unsafe {
            let v = hew_file_read_bytes(p.as_ptr());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 0);
            crate::vec::hew_vec_free(v);

            let error = read_and_free(hew_file_last_error());
            assert!(!error.is_empty());
            assert!(error.contains("hew_file_read_bytes"));
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_invalid_utf8_path_returns_empty_vec() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string; v is returned by hew_file_read_bytes.
        unsafe {
            let v = hew_file_read_bytes(bad.as_ptr());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 0);
            crate::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn write_bytes_creates_file() {
        let dir = test_dir("wbytes_ok");
        let file = dir.join("out.bin");
        let p = cpath(&file);
        let data: &[u8] = &[10, 20, 30];
        // SAFETY: p is a valid NUL-terminated C string; v is created by u8_to_hwvec.
        unsafe {
            let v = crate::vec::u8_to_hwvec(data);
            let rc = hew_file_write_bytes(p.as_ptr(), v);
            assert_eq!(rc, 0);
            crate::vec::hew_vec_free(v);
        }
        assert_eq!(std::fs::read(&file).unwrap(), data);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn write_bytes_null_path_returns_error() {
        // SAFETY: null path is the value under test; v is created by hew_vec_new.
        unsafe {
            let v = crate::vec::hew_vec_new();
            let rc = hew_file_write_bytes(std::ptr::null(), v);
            assert_eq!(rc, -1);
            crate::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn write_bytes_null_data_returns_error() {
        let p = CString::new("/tmp/hew_fio_wbytes_null.bin").unwrap();
        // SAFETY: null data is the value under test; the function handles it.
        let rc = unsafe { hew_file_write_bytes(p.as_ptr(), std::ptr::null_mut()) };
        assert_eq!(rc, -1);
    }

    // -----------------------------------------------------------------------
    // hew_fs_mkdir
    // -----------------------------------------------------------------------

    #[test]
    fn mkdir_creates_directory() {
        let base = test_dir("mkdir_ok");
        let dir = base.join("child");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string.
        let rc = unsafe { hew_fs_mkdir(p.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(dir.is_dir());
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn mkdir_already_exists_returns_error() {
        let dir = test_dir("mkdir_dup");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string (dir already exists via test_dir).
        let rc = unsafe { hew_fs_mkdir(p.as_ptr()) };
        assert_eq!(rc, -1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn mkdir_null_path_returns_error() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_mkdir(std::ptr::null()) }, -1);
    }

    #[test]
    fn mkdir_invalid_utf8_path_returns_error() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        assert_eq!(unsafe { hew_fs_mkdir(bad.as_ptr()) }, -1);
    }

    // -----------------------------------------------------------------------
    // hew_fs_mkdir_all
    // -----------------------------------------------------------------------

    #[test]
    fn mkdir_all_creates_nested_directories() {
        let base = std::env::temp_dir().join("hew_fio_mkdirall");
        let _ = std::fs::remove_dir_all(&base);
        let nested = base.join("a/b/c");
        let p = cpath(&nested);
        // SAFETY: p is a valid NUL-terminated C string.
        let rc = unsafe { hew_fs_mkdir_all(p.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(nested.is_dir());
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn mkdir_all_idempotent_on_existing() {
        let dir = test_dir("mkdirall_idem");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string (dir already exists).
        let rc = unsafe { hew_fs_mkdir_all(p.as_ptr()) };
        assert_eq!(rc, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn mkdir_all_null_path_returns_error() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_mkdir_all(std::ptr::null()) }, -1);
    }

    // -----------------------------------------------------------------------
    // hew_fs_list_dir
    // -----------------------------------------------------------------------

    #[test]
    fn list_dir_returns_entry_names() {
        let dir = test_dir("listdir_ok");
        std::fs::write(dir.join("alpha.txt"), "a").unwrap();
        std::fs::write(dir.join("beta.txt"), "b").unwrap();
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string; v is returned by hew_fs_list_dir.
        unsafe {
            let v = hew_fs_list_dir(p.as_ptr());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 2);
            crate::vec::hew_vec_free(v);
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn list_dir_empty_directory_returns_empty_vec() {
        let dir = test_dir("listdir_empty");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string; v is returned by hew_fs_list_dir.
        unsafe {
            let v = hew_fs_list_dir(p.as_ptr());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 0);
            crate::vec::hew_vec_free(v);
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn list_dir_null_path_returns_empty_vec() {
        // SAFETY: null is the value under test; v is returned by hew_fs_list_dir.
        unsafe {
            let v = hew_fs_list_dir(std::ptr::null());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 0);
            crate::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn list_dir_nonexistent_returns_empty_vec() {
        let p = CString::new("/tmp/hew_fio_listdir_ghost").unwrap();
        // SAFETY: p is a valid NUL-terminated C string; v is returned by hew_fs_list_dir.
        unsafe {
            let v = hew_fs_list_dir(p.as_ptr());
            assert!(!v.is_null());
            assert_eq!(crate::vec::hew_vec_len(v), 0);
            crate::vec::hew_vec_free(v);
        }
    }

    // -----------------------------------------------------------------------
    // hew_fs_rename
    // -----------------------------------------------------------------------

    #[test]
    fn rename_moves_file() {
        let dir = test_dir("rename_ok");
        let src = dir.join("src.txt");
        let dst = dir.join("dst.txt");
        std::fs::write(&src, "hello").unwrap();
        let f = cpath(&src);
        let t = cpath(&dst);
        // SAFETY: both f and t are valid NUL-terminated C strings.
        let rc = unsafe { hew_fs_rename(f.as_ptr(), t.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(!src.exists());
        assert_eq!(std::fs::read_to_string(&dst).unwrap(), "hello");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn rename_null_from_returns_error() {
        let t = CString::new("/tmp/hew_fio_rename_dst.txt").unwrap();
        // SAFETY: null from is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_rename(std::ptr::null(), t.as_ptr()) }, -1);
    }

    #[test]
    fn rename_null_to_returns_error() {
        let f = CString::new("/tmp/hew_fio_rename_src.txt").unwrap();
        // SAFETY: null to is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_rename(f.as_ptr(), std::ptr::null()) }, -1);
    }

    #[test]
    fn rename_nonexistent_source_returns_error() {
        let f = CString::new("/tmp/hew_fio_rename_ghost.txt").unwrap();
        let t = CString::new("/tmp/hew_fio_rename_dst2.txt").unwrap();
        // SAFETY: both f and t are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_fs_rename(f.as_ptr(), t.as_ptr()) }, -1);
    }

    // -----------------------------------------------------------------------
    // hew_fs_copy
    // -----------------------------------------------------------------------

    #[test]
    fn copy_duplicates_file_preserving_source() {
        let dir = test_dir("copy_ok");
        let src = dir.join("orig.txt");
        let dst = dir.join("dup.txt");
        std::fs::write(&src, "payload").unwrap();
        let f = cpath(&src);
        let t = cpath(&dst);
        // SAFETY: both f and t are valid NUL-terminated C strings.
        let rc = unsafe { hew_fs_copy(f.as_ptr(), t.as_ptr()) };
        assert_eq!(rc, 0);
        assert!(src.exists());
        assert_eq!(std::fs::read_to_string(&dst).unwrap(), "payload");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn copy_null_from_returns_error() {
        let t = CString::new("/tmp/hew_fio_copy_dst.txt").unwrap();
        // SAFETY: null from is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_copy(std::ptr::null(), t.as_ptr()) }, -1);
    }

    #[test]
    fn copy_null_to_returns_error() {
        let f = CString::new("/tmp/hew_fio_copy_src.txt").unwrap();
        // SAFETY: null to is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_copy(f.as_ptr(), std::ptr::null()) }, -1);
    }

    #[test]
    fn copy_nonexistent_source_returns_error() {
        let f = CString::new("/tmp/hew_fio_copy_ghost.txt").unwrap();
        let t = CString::new("/tmp/hew_fio_copy_dst2.txt").unwrap();
        // SAFETY: both f and t are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_fs_copy(f.as_ptr(), t.as_ptr()) }, -1);
    }

    // -----------------------------------------------------------------------
    // hew_fs_is_dir
    // -----------------------------------------------------------------------

    #[test]
    fn is_dir_returns_one_for_directory() {
        let dir = test_dir("isdir_yes");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_fs_is_dir(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_dir_returns_zero_for_file() {
        let dir = test_dir("isdir_file");
        let file = dir.join("not_a_dir.txt");
        std::fs::write(&file, "x").unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_fs_is_dir(p.as_ptr()) }, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_dir_returns_zero_for_nonexistent() {
        let p = CString::new("/tmp/hew_fio_isdir_ghost").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_fs_is_dir(p.as_ptr()) }, 0);
    }

    #[test]
    fn is_dir_null_path_returns_zero() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_fs_is_dir(std::ptr::null()) }, 0);
    }

    // -----------------------------------------------------------------------
    // Integration: write → read → size → delete → exists
    // -----------------------------------------------------------------------

    #[test]
    fn full_file_lifecycle() {
        let dir = test_dir("lifecycle");
        let file = dir.join("lifecycle.txt");
        let p = cpath(&file);
        let content = CString::new("lifecycle test").unwrap();
        // SAFETY: all pointers are valid NUL-terminated C strings throughout this block.
        unsafe {
            assert_eq!(hew_file_write(p.as_ptr(), content.as_ptr()), 0);
            assert_eq!(hew_file_exists(p.as_ptr()), 1);
            assert_eq!(hew_file_size(p.as_ptr()), 14);
            let ptr = hew_file_read(p.as_ptr());
            let s = read_and_free(ptr);
            assert_eq!(s, "lifecycle test");
            assert_eq!(hew_file_delete(p.as_ptr()), 0);
            assert_eq!(hew_file_exists(p.as_ptr()), 0);
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
}
