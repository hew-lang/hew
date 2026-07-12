//! Hew runtime: `file_io` module.
//!
//! Synchronous file I/O operations with C ABI.
//!
//! All functions that return `*mut c_char` allocate via `libc::malloc`. The
//! caller owns the returned pointer and must free it with `hew_string_drop`.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::cabi::str_to_malloc;
use crate::stream_error::{set_last_error_with_errno, take_last_error};
use std::ffi::{c_char, CStr, CString};

fn clear_file_io_error() {
    crate::hew_clear_error();
    let _ = take_last_error();
}

fn set_file_io_error(operation: &str, error: &std::io::Error) {
    let message = format!("{operation}: {error}");
    crate::set_last_error(&message);
    set_last_error_with_errno(message, error.raw_os_error().unwrap_or(libc::EIO));
}

fn set_file_io_errno(operation: &str, message: impl std::fmt::Display, errno: i32) {
    let message = format!("{operation}: {message}");
    crate::set_last_error(&message);
    set_last_error_with_errno(message, errno);
}

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
/// The caller owns the returned pointer and must free it with `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        let msg = "hew_file_read: invalid path string";
        crate::set_last_error(msg);
        set_last_error_with_errno(msg.into(), 22);
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `path` is a valid, NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        let msg = "hew_file_read: invalid path string";
        crate::set_last_error(msg);
        set_last_error_with_errno(msg.into(), 22);
        return std::ptr::null_mut();
    };
    let contents = match std::fs::read_to_string(rust_path) {
        Ok(contents) => contents,
        Err(error) => {
            set_file_io_error("hew_file_read", &error);
            return std::ptr::null_mut();
        }
    };
    // Reject files with interior NUL bytes (can't represent as C string).
    if contents.contains('\0') {
        let msg = "hew_file_read: contents contain interior NUL byte";
        crate::set_last_error(msg);
        set_last_error_with_errno(msg.into(), 22);
        return std::ptr::null_mut();
    }
    clear_file_io_error();
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
    match std::fs::write(rust_path, rust_content) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_file_write", &error);
            -1
        }
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
    let mut file = match std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(rust_path)
    {
        Ok(file) => file,
        Err(error) => {
            set_file_io_error("hew_file_append", &error);
            return -1;
        }
    };
    match file.write_all(rust_content.as_bytes()) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_file_append", &error);
            -1
        }
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
    match std::fs::remove_file(rust_path) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_file_delete", &error);
            -1
        }
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
/// The caller owns the returned pointer and must free it with `hew_string_drop`.
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

/// Read the raw bytes of a file and return them as a `BytesTriple` value.
///
/// Returns an empty triple (`ptr=null, offset=0, len=0`) on error; the caller
/// must inspect `hew_stream_last_errno()` to distinguish a genuine empty file
/// from a read failure.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_bytes(path: *const c_char) -> crate::bytes::BytesTriple {
    // SAFETY: `path` is the caller-provided C string pointer for this ABI entrypoint.
    let Some(rust_path) = (unsafe { crate::util::cstr_to_str(&path, "hew_file_read_bytes") })
    else {
        let msg = "hew_file_read_bytes: invalid path string";
        crate::set_last_error(msg);
        set_last_error_with_errno(
            msg.into(),
            22, // EINVAL: Invalid argument
        );
        return crate::bytes::BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    };
    match std::fs::read(rust_path) {
        Ok(data) => {
            // Clear both the runtime LAST_ERROR and the cabi LAST_ERROR/LAST_ERRNO
            // so that stale messages and errno from a prior failed call are not
            // visible to callers that inspect hew_last_error() or
            // hew_stream_last_error() after a successful read.
            clear_file_io_error();
            let len = u32::try_from(data.len()).unwrap_or(u32::MAX);
            // SAFETY: data is a valid Vec<u8>; len <= data.len().
            unsafe { crate::bytes::hew_bytes_from_static(data.as_ptr(), len) }
        }
        Err(error) => {
            set_file_io_error("hew_file_read_bytes", &error);
            crate::bytes::BytesTriple {
                ptr: std::ptr::null_mut(),
                offset: 0,
                len: 0,
            }
        }
    }
}

/// Out-pointer variant of [`hew_file_read_bytes`] for Windows x64 MSVC sret fix.
///
/// Writes the result into `*out` and returns void. Every bytes-triple producer
/// exports a `_raw` sibling so that codegen can use the out-pointer calling
/// convention on all platforms and avoid the MSVC sret mismatch.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
/// `out` must be a valid, writable pointer to a `BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_bytes_raw(
    path: *const c_char,
    out: *mut crate::bytes::BytesTriple,
) {
    // SAFETY: preconditions forwarded from caller contract above.
    let triple = unsafe { hew_file_read_bytes(path) };
    // SAFETY: caller guarantees `out` is a valid BytesTriple slot.
    unsafe { out.write(triple) };
}

/// Write a `BytesTriple` to a file, overwriting any existing content.
///
/// `data` is the address of the caller's `BytesTriple` alloca, passed via the
/// by-pointer bytes consumer convention (`is_bytes_by_pointer_consumer`).
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
/// `data` must be a valid, non-null pointer to a `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_file_write_bytes(
    path: *const c_char,
    data: *const crate::bytes::BytesTriple,
) -> i32 {
    if path.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return -1;
    };
    // SAFETY: caller guarantees `data` is a valid BytesTriple pointer.
    let triple = unsafe { &*data };
    let slice = if triple.len == 0 || triple.ptr.is_null() {
        &[] as &[u8]
    } else {
        // SAFETY: triple.ptr + triple.offset is valid for triple.len bytes per BytesTriple invariant.
        unsafe {
            std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
        }
    };
    match std::fs::write(rust_path, slice) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_file_write_bytes", &error);
            -1
        }
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
    match std::fs::create_dir(rust_path) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_fs_mkdir", &error);
            -1
        }
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
    match std::fs::create_dir_all(rust_path) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_fs_mkdir_all", &error);
            -1
        }
    }
}

/// List entries in a directory.
///
/// Returns a `HewVec` of entry names (not full paths). Caller must free with
/// `hew_vec_free`. Returns an empty vec and records the failure on error.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string.
///
/// # Panics
///
/// Panics if an entry name contains a NUL byte, which cannot occur for
/// real filesystem entry names on supported platforms.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_list_dir(path: *const c_char) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new_str allocates a valid empty HewVec<String>.
    let v = unsafe { crate::vec::hew_vec_new_str() };
    if path.is_null() {
        set_file_io_errno("hew_fs_list_dir", "invalid path string", libc::EINVAL);
        return v;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        set_file_io_errno("hew_fs_list_dir", "invalid path string", libc::EINVAL);
        return v;
    };
    let entries = match std::fs::read_dir(rust_path) {
        Ok(entries) => entries,
        Err(error) => {
            set_file_io_error("hew_fs_list_dir", &error);
            return v;
        }
    };
    let mut names = Vec::new();
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(error) => {
                set_file_io_error("hew_fs_list_dir", &error);
                return v;
            }
        };
        let name = entry.file_name();
        let Ok(name_str) = name.into_string() else {
            set_file_io_errno(
                "hew_fs_list_dir",
                "entry name is not valid UTF-8",
                libc::EILSEQ,
            );
            return v;
        };
        names.push(name_str);
    }
    for name in names {
        let c_name = CString::new(name).expect("file names cannot contain NUL");
        // SAFETY: v is a valid HewVec; c_name is a valid NUL-terminated C string.
        unsafe { crate::vec::hew_vec_push_str(v, c_name.as_ptr()) };
    }
    clear_file_io_error();
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
    match std::fs::rename(from_str, to_str) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_fs_rename", &error);
            -1
        }
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
    match std::fs::copy(from_str, to_str) {
        Ok(_) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("hew_fs_copy", &error);
            -1
        }
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
        // SAFETY: ptr was allocated header-aware by str_to_malloc (hew_file_read).
        unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (test consumer of str_to_malloc output; header-aware in S1)
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
        assert_ne!(crate::stream_error::take_last_errno(), 0);
        let _ = crate::stream_error::take_last_error();
    }

    #[test]
    fn read_nonexistent_file_returns_null() {
        let p = CString::new("/tmp/hew_fio_surely_does_not_exist.txt").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let ptr = unsafe { hew_file_read(p.as_ptr()) };
        assert!(ptr.is_null());
        assert_ne!(crate::stream_error::take_last_errno(), 0);
        let err = crate::stream_error::take_last_error();
        assert!(err
            .as_deref()
            .is_some_and(|message| message.contains("hew_file_read")));
    }

    #[test]
    fn read_invalid_utf8_path_returns_null() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        let ptr = unsafe { hew_file_read(bad.as_ptr()) };
        assert!(ptr.is_null());
    }

    #[test]
    fn read_invalid_utf8_contents_returns_null_and_sets_errno() {
        let dir = test_dir("read_invalid_utf8_contents");
        let file = dir.join("invalid_utf8.txt");
        std::fs::write(&file, [0xFF, 0xFE, b'a', b'\n']).unwrap();
        let p = cpath(&file);

        // SAFETY: p is a valid NUL-terminated C string pointing to a file whose
        // contents are intentionally invalid UTF-8.
        let ptr = unsafe { hew_file_read(p.as_ptr()) };

        assert!(ptr.is_null());
        assert_ne!(crate::stream_error::hew_stream_last_errno(), 0);
        let _ = crate::stream_error::take_last_error();
        let _ = std::fs::remove_dir_all(&dir);
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

    /// Helper: free a `BytesTriple` allocated by the runtime (non-null `ptr`).
    ///
    /// # Safety
    ///
    /// `triple.ptr` must be a pointer allocated by `hew_bytes_from_static` (or
    /// any other bytes runtime allocator using `alloc_buf`).
    unsafe fn free_bytes_triple(triple: &crate::bytes::BytesTriple) {
        if !triple.ptr.is_null() {
            // SAFETY: caller guarantees ptr was allocated by hew_bytes_from_static.
            unsafe { crate::bytes::hew_bytes_drop(triple.ptr) };
        }
    }

    #[test]
    fn read_bytes_roundtrip_with_write_bytes() {
        let dir = test_dir("bytes_rt");
        let file = dir.join("data.bin");
        let data: Vec<u8> = vec![0, 1, 127, 128, 255];
        std::fs::write(&file, &data).unwrap();
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string; triple is returned by hew_file_read_bytes.
        unsafe {
            let triple = hew_file_read_bytes(p.as_ptr());
            assert_eq!(triple.len as usize, data.len());
            let slice = std::slice::from_raw_parts(
                triple.ptr.add(triple.offset as usize),
                triple.len as usize,
            );
            assert_eq!(slice, data.as_slice());
            free_bytes_triple(&triple);
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_null_path_returns_empty_triple() {
        // SAFETY: null is the value under test; the function handles it gracefully.
        let triple = unsafe { hew_file_read_bytes(std::ptr::null()) };
        assert_eq!(triple.len, 0);
        assert!(triple.ptr.is_null());
    }

    #[test]
    fn read_bytes_nonexistent_file_returns_empty_triple() {
        let dir = test_dir("bytes_missing");
        let file = dir.join("ghost.bin");
        let p = cpath(&file);
        // SAFETY: p is a valid NUL-terminated C string; triple is returned by hew_file_read_bytes.
        unsafe {
            let triple = hew_file_read_bytes(p.as_ptr());
            assert_eq!(triple.len, 0);
            assert!(triple.ptr.is_null());

            // hew_last_error() returns a thread-local pointer; copy it before
            // any subsequent call could overwrite it.
            let err_ptr = crate::hew_last_error();
            assert!(!err_ptr.is_null());
            let error = CStr::from_ptr(err_ptr).to_str().unwrap_or("").to_owned();
            assert!(!error.is_empty());
            assert!(error.contains("hew_file_read_bytes"));
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_success_clears_errno_after_prior_failure() {
        // Regression: hew_file_read_bytes must clear LAST_ERRNO on its success
        // path so that a stale errno from a prior failed read cannot be mistaken
        // for an error by callers that inspect hew_stream_last_errno() after a
        // successful read (e.g. try_read_bytes in std/fs.hew).
        use hew_cabi::sink::hew_stream_last_errno;

        let dir = test_dir("bytes_errno_clear");

        // Step 1: trigger a failure so LAST_ERRNO is set to a non-zero value.
        let ghost_path = dir.join("does_not_exist.bin");
        let ghost = cpath(&ghost_path);
        // SAFETY: ghost is a valid NUL-terminated C string.
        let _ = unsafe { hew_file_read_bytes(ghost.as_ptr()) };
        // LAST_ERRNO should now be non-zero (ENOENT / 2 on POSIX systems).
        // We deliberately do NOT read it here — reading would clear it via
        // take_last_errno(), defeating the regression test.

        // Step 2: write a real file and read it successfully.
        let real_path = dir.join("real.bin");
        let content: &[u8] = &[1, 2, 3];
        std::fs::create_dir_all(&dir).expect("create test dir");
        std::fs::write(&real_path, content).expect("write test file");
        let real = cpath(&real_path);
        // SAFETY: real is a valid NUL-terminated C string.
        unsafe {
            let triple = hew_file_read_bytes(real.as_ptr());
            assert_eq!(triple.len, 3);
            free_bytes_triple(&triple);
        }

        // Step 3: LAST_ERRNO must be 0 — the success path cleared it.
        // If the fix is missing, this will be non-zero (ENOENT from step 1).
        let errno_after_success = hew_stream_last_errno();
        assert_eq!(
            errno_after_success, 0,
            "errno must be cleared on successful hew_file_read_bytes; \
             stale non-zero errno would cause try_read_bytes to return a false Err"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_success_clears_both_last_errors_after_prior_failure() {
        // Regression: hew_file_read_bytes on its success path must clear both
        // the runtime LAST_ERROR (hew_last_error) and the cabi LAST_ERROR
        // (hew_stream_last_error). Previously, set_last_error_with_errno("", 0)
        // left the cabi LAST_ERROR as Some("") — causing hew_stream_last_error()
        // to return a malloc'd empty string instead of NULL, violating the
        // "NULL if none" contract. And hew_last_error() retained the stale
        // message because the runtime LAST_ERROR was not cleared at all.
        use hew_cabi::sink::hew_stream_last_error;

        let dir = test_dir("bytes_both_errors_clear");

        // Step 1: trigger a failure so both LAST_ERROR stores are populated.
        let ghost_path = dir.join("does_not_exist.bin");
        let ghost = cpath(&ghost_path);
        // SAFETY: ghost is a valid NUL-terminated C string.
        let _ = unsafe { hew_file_read_bytes(ghost.as_ptr()) };

        // Step 2: write a real file and read it successfully.
        let real_path = dir.join("real.bin");
        let content: &[u8] = &[7, 8, 9];
        std::fs::create_dir_all(&dir).expect("create test dir");
        std::fs::write(&real_path, content).expect("write test file");
        let real = cpath(&real_path);
        // SAFETY: real is a valid NUL-terminated C string.
        unsafe {
            let triple = hew_file_read_bytes(real.as_ptr());
            assert_eq!(triple.len, 3);
            free_bytes_triple(&triple);
        }

        // Step 3a: hew_stream_last_error() must return NULL (cabi LAST_ERROR
        // must be None, not Some("")).
        let stream_err_ptr = hew_stream_last_error();
        assert!(
            stream_err_ptr.is_null(),
            "hew_stream_last_error() must return NULL after successful read; \
             got non-null pointer (cabi LAST_ERROR was Some(\"\") instead of None)"
        );
        // No free needed — we asserted it is null.

        // Step 3b: hew_last_error() must return null (runtime LAST_ERROR cleared).
        let runtime_err_ptr = crate::hew_last_error();
        assert!(
            runtime_err_ptr.is_null(),
            "hew_last_error() must return null after successful read; \
             stale runtime LAST_ERROR was not cleared on the success path"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_failure_message_survives_a_standalone_errno_read() {
        // Regression (std/fs.hew try_read_bytes leak, hew-lang/hew#1728-class):
        // hew_stream_last_errno() and hew_stream_last_error() are independent
        // thread-local reads (see stream_error.rs's own doc comment) — reading
        // one does not clear the other. try_read_bytes in std/fs.hew reads only
        // hew_stream_last_errno() on its failure path; if it does not also
        // drain hew_stream_last_error(), a stale `hew_file_read_bytes: ...`
        // message from this failed read stays parked in the thread-local and
        // can leak into a later, unrelated hew_last_error() /
        // hew_stream_last_error() read (e.g. a subsequent fs.read() panic
        // message). This test pins the FFI-layer precondition for that fix:
        // after hew_file_read_bytes fails and only the errno is consumed, the
        // message must still be present and draining it must yield the
        // *current* failure's own text, not an empty/stale value.
        use hew_cabi::sink::{hew_stream_last_errno, hew_stream_last_error};

        let dir = test_dir("bytes_errno_only_read_leak");
        let ghost_path = dir.join("does_not_exist.bin");
        let ghost = cpath(&ghost_path);

        // SAFETY: ghost is a valid NUL-terminated C string.
        let triple = unsafe { hew_file_read_bytes(ghost.as_ptr()) };
        assert_eq!(triple.len, 0);
        assert!(triple.ptr.is_null());

        // Mirror try_read_bytes's exact failure-path call: read errno alone
        // first, the way the .hew wrapper does before deciding to return Err.
        let errno = hew_stream_last_errno();
        assert_ne!(errno, 0, "expected a non-zero errno from the missing file");

        // The message must still be there for a caller that goes on to drain
        // it (the fix), and it must be this call's own message, not empty.
        // SAFETY: hew_stream_last_error() returns a fresh, valid C string or
        // null; we only read it and then free it via the documented path.
        unsafe {
            let msg_ptr = hew_stream_last_error();
            assert!(
                !msg_ptr.is_null(),
                "hew_stream_last_error() must still return the message after a \
                 standalone hew_stream_last_errno() read — they are documented \
                 as independent reads, so try_read_bytes's fix must explicitly \
                 drain this instead of assuming the errno read already did"
            );
            let message = CStr::from_ptr(msg_ptr).to_str().unwrap_or("").to_owned();
            crate::string::hew_string_drop(msg_ptr);
            assert!(
                message.contains("hew_file_read_bytes"),
                "expected the current failed read's own message, got {message:?}"
            );
        }

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_bytes_invalid_utf8_path_returns_empty_triple() {
        let bad = invalid_utf8_cstring();
        // SAFETY: bad is a valid NUL-terminated C string (non-UTF-8 is handled).
        let triple = unsafe { hew_file_read_bytes(bad.as_ptr()) };
        assert_eq!(triple.len, 0);
        assert!(triple.ptr.is_null());
    }

    #[test]
    fn write_bytes_creates_file() {
        let dir = test_dir("wbytes_ok");
        let file = dir.join("out.bin");
        let p = cpath(&file);
        let data: &[u8] = &[10, 20, 30];
        // Build a BytesTriple pointing at the data slice (no allocation needed).
        let triple = crate::bytes::BytesTriple {
            ptr: data.as_ptr().cast_mut(),
            offset: 0,
            len: u32::try_from(data.len()).unwrap_or(u32::MAX),
        };
        // SAFETY: p is a valid NUL-terminated C string; triple points to valid data.
        let rc = unsafe { hew_file_write_bytes(p.as_ptr(), &raw const triple) };
        assert_eq!(rc, 0);
        assert_eq!(std::fs::read(&file).unwrap(), data);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn write_bytes_null_path_returns_error() {
        let triple = crate::bytes::BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
        // SAFETY: null path is the value under test; the function handles it.
        let rc = unsafe { hew_file_write_bytes(std::ptr::null(), &raw const triple) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn write_bytes_null_data_returns_error() {
        let p = CString::new("/tmp/hew_fio_wbytes_null.bin").unwrap();
        // SAFETY: null data is the value under test; the function handles it.
        let rc = unsafe { hew_file_write_bytes(p.as_ptr(), std::ptr::null()) };
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
