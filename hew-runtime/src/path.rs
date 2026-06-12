//! Hew runtime: `path` module — filesystem metadata and glob expansion.
//!
//! Provides the C ABI symbols called from `std/path.hew`:
//!
//! - `hew_path_is_file`   — test whether a path is a regular file
//! - `hew_path_is_dir`    — test whether a path is a directory
//! - `hew_path_absolute`  — resolve a path to an absolute form
//! - `hew_glob`           — expand a glob pattern via POSIX `glob(3)`
//! - `hew_glob_count`     — number of matched paths in a `HewGlobResult`
//! - `hew_glob_get`       — retrieve a matched path by index
//! - `hew_glob_free`      — release a `HewGlobResult`
//!
//! All functions fail-closed (null / false / 0) rather than propagating I/O
//! errors, consistent with the existing `hew_path_exists` contract in
//! `file_io.rs`.  Glob expansion uses the platform's POSIX `glob(3)` via
//! `libc` and copies the matched strings into a heap-owned `Vec<String>` so
//! that the `libc::glob_t` can be freed immediately after the walk.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::cabi::str_to_malloc;
use std::ffi::{c_char, CStr};

// ── Simple filesystem-metadata helpers ──────────────────────────────────────

/// Test whether `path` refers to a regular file.
///
/// Returns `1` if the path names a regular file, `0` otherwise (including on
/// null input, invalid UTF-8, I/O error, or any other failure).
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_path_is_file(path: *const c_char) -> i32 {
    if path.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return 0;
    };
    i32::from(std::path::Path::new(rust_path).is_file())
}

/// Test whether `path` refers to a directory.
///
/// Returns `1` if the path names a directory, `0` otherwise (including on
/// null input, invalid UTF-8, I/O error, or any other failure).
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_path_is_dir(path: *const c_char) -> i32 {
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

/// Return the absolute form of `path`.
///
/// Resolves the path against the process working directory using
/// [`std::path::absolute`].  Returns a `malloc`-allocated, NUL-terminated
/// C string that the caller must free with `hew_string_drop`.
///
/// Returns null on null input, invalid UTF-8, or resolution failure.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string (or null).
///
/// # Ownership
///
/// The caller owns the returned pointer and must free it with `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_path_absolute(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `path` is a valid NUL-terminated C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return std::ptr::null_mut();
    };
    let Ok(abs) = std::path::absolute(rust_path) else {
        return std::ptr::null_mut();
    };
    let Some(abs_str) = abs.to_str() else {
        return std::ptr::null_mut();
    };
    str_to_malloc(abs_str)
}

// ── Glob expansion ───────────────────────────────────────────────────────────

/// Heap-owned result of a glob expansion.
///
/// Opaque to the Hew compiler — always manipulated through the
/// `hew_glob_*` C ABI.
#[derive(Debug)]
pub struct HewGlobResult {
    /// The matched paths, each an owned UTF-8 string.
    matches: Vec<String>,
}

/// Expand `pattern` using POSIX `glob(3)` and return a `HewGlobResult`.
///
/// The returned pointer is heap-allocated and must be freed by calling
/// `hew_glob_free`.  Returns a non-null pointer even when there are zero
/// matches — the caller uses `hew_glob_count` to check.
///
/// Returns null only on allocation failure or null input.
///
/// # Safety
///
/// `pattern` must be a valid, NUL-terminated C string (or null).
///
/// # Ownership
///
/// The caller owns the returned pointer and must release it with `hew_glob_free`.
#[no_mangle]
pub unsafe extern "C" fn hew_glob(pattern: *const c_char) -> *mut HewGlobResult {
    if pattern.is_null() {
        return Box::into_raw(Box::new(HewGlobResult { matches: vec![] }));
    }
    // SAFETY: caller guarantees `pattern` is a valid NUL-terminated C string.
    let c_pattern = unsafe { CStr::from_ptr(pattern) };

    let matches = glob_expand(c_pattern);
    Box::into_raw(Box::new(HewGlobResult { matches }))
}

/// Perform the POSIX `glob(3)` expansion and return the matched strings.
///
/// Uses `libc::glob` on unix targets.
fn glob_expand(pattern: &CStr) -> Vec<String> {
    #[cfg(target_family = "unix")]
    {
        glob_expand_unix(pattern)
    }
    // SHIM: Windows glob not yet implemented.  The symbols stay present and
    // link-clean, and the gap is RECORDED (fail-closed: an empty result with
    // last_error set is diagnosable; a silent empty result would fabricate
    // "no matches").
    // WHEN obsolete: when a Windows target gains an end-to-end path/glob test
    // lane.  WHAT the real solution looks like: FindFirstFileW/FindNextFileW
    // expansion with the same HewGlobResult ownership contract.
    #[cfg(not(target_family = "unix"))]
    {
        let _ = pattern;
        crate::set_last_error("hew_glob: glob expansion is not implemented on this platform");
        vec![]
    }
}

#[cfg(target_family = "unix")]
fn glob_expand_unix(pattern: &CStr) -> Vec<String> {
    use libc::{glob as libc_glob, glob_t, globfree, GLOB_ERR};

    let mut g: glob_t;
    // SAFETY: `glob_t` is a C struct; zeroing it is the correct initialisation
    // before passing it to `glob(3)`.
    g = unsafe { std::mem::zeroed() };

    // GLOB_ERR  — abort on read errors (directories we cannot open)
    // GLOB_NOCHECK — return the pattern unchanged if there are no matches,
    //                rather than reporting GLOB_NOMATCH.
    //
    // We ignore GLOB_NOCHECK here: we interpret GLOB_NOMATCH as "zero results"
    // which is the least-surprise behaviour for a Hew stdlib glob call.
    let flags = GLOB_ERR;

    // SAFETY: `g` is a zeroed `glob_t`; `pattern.as_ptr()` is a valid C string
    // for the lifetime of this call; no error callback.
    let rc = unsafe { libc_glob(pattern.as_ptr(), flags, None, &raw mut g) };

    // rc == 0        → success, g.gl_pathc paths in g.gl_pathv
    // rc == GLOB_NOMATCH → no matches; gl_pathc may be 0; still need globfree
    // other          → error; still need globfree
    //
    // In all cases we must call globfree before returning.
    let mut results: Vec<String> = Vec::new();

    if rc == 0 {
        let count = g.gl_pathc;
        for i in 0..count {
            // SAFETY: gl_pathv[i] is a valid NUL-terminated C string for
            // indices 0..gl_pathc, per POSIX glob(3).
            let entry_ptr = unsafe { *g.gl_pathv.add(i) };
            if entry_ptr.is_null() {
                break;
            }
            // SAFETY: entry_ptr is a valid NUL-terminated C string.
            if let Ok(s) = unsafe { CStr::from_ptr(entry_ptr) }.to_str() {
                results.push(s.to_owned());
            }
        }
    }

    // SAFETY: `g` was initialised and (successfully or not) filled by `glob`.
    unsafe { globfree(&raw mut g) };

    results
}

/// Return the number of paths in `result`.
///
/// Returns 0 on null input.
///
/// # Safety
///
/// `result` must be a pointer returned by [`hew_glob`] that has not yet been
/// freed, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_glob_count(result: *mut HewGlobResult) -> i32 {
    if result.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `result` is a live HewGlobResult.
    let r = unsafe { &*result };
    i32::try_from(r.matches.len()).unwrap_or(i32::MAX)
}

/// Return the path at `index` from `result` as a `malloc`-allocated C string.
///
/// Returns null on null input, out-of-range index, or interior NUL.
///
/// # Safety
///
/// `result` must be a pointer returned by [`hew_glob`] that has not yet been
/// freed, or null.
///
/// # Ownership
///
/// The returned pointer is `malloc`-allocated.  The caller must free it with
/// `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_glob_get(result: *mut HewGlobResult, index: i32) -> *mut c_char {
    if result.is_null() || index < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `result` is a live HewGlobResult.
    let r = unsafe { &*result };
    // SAFETY: index >= 0 guarded above.
    #[expect(clippy::cast_sign_loss, reason = "index < 0 is guarded above")]
    let idx = index as usize;
    let Some(s) = r.matches.get(idx) else {
        return std::ptr::null_mut();
    };
    // Verify no interior NUL before allocating the C string.
    if s.contains('\0') {
        return std::ptr::null_mut();
    }
    str_to_malloc(s)
}

/// Free a `HewGlobResult` returned by [`hew_glob`].
///
/// Passing null is safe and has no effect.
///
/// # Safety
///
/// `result` must be a pointer returned by [`hew_glob`] that has not yet been
/// freed, or null.  After this call the pointer is invalid.
#[no_mangle]
pub unsafe extern "C" fn hew_glob_free(result: *mut HewGlobResult) {
    if result.is_null() {
        return;
    }
    // SAFETY: `result` was produced by `Box::into_raw(Box::new(...))` in
    // `hew_glob`; we are the sole owner.
    drop(unsafe { Box::from_raw(result) });
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::path::PathBuf;

    fn cpath(p: &std::path::Path) -> CString {
        CString::new(p.to_str().unwrap()).unwrap()
    }

    fn test_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("hew_path_{name}"));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    // ── hew_path_is_file ────────────────────────────────────────────────

    #[test]
    fn is_file_returns_one_for_regular_file() {
        let dir = test_dir("is_file_yes");
        let f = dir.join("test.txt");
        std::fs::write(&f, "x").unwrap();
        let p = cpath(&f);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_is_file(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_file_returns_zero_for_directory() {
        let dir = test_dir("is_file_dir");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string (directory path).
        assert_eq!(unsafe { hew_path_is_file(p.as_ptr()) }, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_file_returns_zero_for_nonexistent() {
        let p = CString::new("/tmp/hew_path_is_file_ghost_12345").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_is_file(p.as_ptr()) }, 0);
    }

    #[test]
    fn is_file_returns_zero_for_null() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_path_is_file(std::ptr::null()) }, 0);
    }

    // ── hew_path_is_dir ─────────────────────────────────────────────────

    #[test]
    fn is_dir_returns_one_for_directory() {
        let dir = test_dir("is_dir_yes");
        let p = cpath(&dir);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_is_dir(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_dir_returns_zero_for_regular_file() {
        let dir = test_dir("is_dir_file");
        let f = dir.join("test.txt");
        std::fs::write(&f, "x").unwrap();
        let p = cpath(&f);
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_is_dir(p.as_ptr()) }, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_dir_returns_zero_for_nonexistent() {
        let p = CString::new("/tmp/hew_path_is_dir_ghost_12345").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_path_is_dir(p.as_ptr()) }, 0);
    }

    #[test]
    fn is_dir_returns_zero_for_null() {
        // SAFETY: null is the value under test; the function handles it.
        assert_eq!(unsafe { hew_path_is_dir(std::ptr::null()) }, 0);
    }

    // ── hew_path_absolute ───────────────────────────────────────────────

    #[test]
    fn absolute_returns_non_null_for_valid_path() {
        let p = CString::new("/tmp").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let ptr = unsafe { hew_path_absolute(p.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr was returned by hew_path_absolute (str_to_malloc alloc).
        unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open
    }

    #[test]
    fn absolute_resolves_dot_to_cwd() {
        let p = CString::new(".").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let ptr = unsafe { hew_path_absolute(p.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was returned by hew_path_absolute.
        unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open
                                                   // Absoluteness is platform-shaped: POSIX roots at '/', Windows at a
                                                   // drive prefix. Assert via the platform's own notion, not a literal.
        assert!(
            std::path::Path::new(&s).is_absolute(),
            "absolute path must be platform-absolute; got {s}"
        );
    }

    #[test]
    fn absolute_returns_null_for_null_input() {
        // SAFETY: null is the value under test; the function handles it.
        let ptr = unsafe { hew_path_absolute(std::ptr::null()) };
        assert!(ptr.is_null());
    }

    // ── hew_glob_* ──────────────────────────────────────────────────────

    #[cfg(target_family = "unix")]
    #[test]
    fn glob_returns_non_null_for_matching_pattern() {
        let dir = test_dir("glob_match");
        std::fs::write(dir.join("a.txt"), "").unwrap();
        std::fs::write(dir.join("b.txt"), "").unwrap();
        let pattern = format!("{}/*.txt", dir.to_str().unwrap());
        let cp = CString::new(pattern).unwrap();
        // SAFETY: cp is a valid NUL-terminated C string.
        let res = unsafe { hew_glob(cp.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        let count = unsafe { hew_glob_count(res) };
        assert_eq!(count, 2);
        // SAFETY: res is live; index 0 is valid.
        let p0 = unsafe { hew_glob_get(res, 0) };
        assert!(!p0.is_null());
        // SAFETY: p0 is a valid NUL-terminated C string from str_to_malloc.
        unsafe { crate::cabi::free_cstring(p0) }; // CSTRING-FREE: str-open
                                                  // SAFETY: res is a live HewGlobResult.
        unsafe { hew_glob_free(res) };
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Pins the non-unix SHIM in glob_expand: matching files exist, but the
    // unimplemented platform returns an empty result AND records the gap in
    // last_error (fail-closed: diagnosable, never a fabricated "no matches").
    #[cfg(not(target_family = "unix"))]
    #[test]
    fn glob_unsupported_platform_returns_empty_and_records_error() {
        let dir = test_dir("glob_match");
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let pattern = format!("{}/*.txt", dir.to_str().unwrap());
        let cp = CString::new(pattern).unwrap();
        // SAFETY: cp is a valid NUL-terminated C string.
        let res = unsafe { hew_glob(cp.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        assert_eq!(unsafe { hew_glob_count(res) }, 0);
        let err = crate::hew_last_error();
        assert!(!err.is_null());
        // SAFETY: hew_last_error returns a valid NUL-terminated C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert!(msg.contains("glob expansion is not implemented"));
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn glob_returns_empty_result_for_no_matches() {
        let p = CString::new("/tmp/hew_path_glob_nomatch_*_zzz_999").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let res = unsafe { hew_glob(p.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        let count = unsafe { hew_glob_count(res) };
        assert_eq!(count, 0);
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
    }

    #[test]
    fn glob_null_pattern_returns_empty_result() {
        // SAFETY: null is the value under test; hew_glob handles it.
        let res = unsafe { hew_glob(std::ptr::null()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        let count = unsafe { hew_glob_count(res) };
        assert_eq!(count, 0);
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
    }

    #[test]
    fn glob_free_null_is_safe() {
        // SAFETY: null is explicitly allowed by hew_glob_free's contract.
        unsafe { hew_glob_free(std::ptr::null_mut()) };
    }

    #[test]
    fn glob_count_null_returns_zero() {
        // SAFETY: null is explicitly allowed by hew_glob_count's contract.
        assert_eq!(unsafe { hew_glob_count(std::ptr::null_mut()) }, 0);
    }

    #[test]
    fn glob_get_null_result_returns_null() {
        // SAFETY: null result is explicitly allowed by hew_glob_get's contract.
        assert!(unsafe { hew_glob_get(std::ptr::null_mut(), 0) }.is_null());
    }

    #[test]
    fn glob_get_out_of_range_returns_null() {
        let p = CString::new("/tmp/hew_path_glob_oor_*_zzz_999").unwrap();
        // SAFETY: p is a valid NUL-terminated C string.
        let res = unsafe { hew_glob(p.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is live; index 99 is out of range for zero matches.
        let ptr = unsafe { hew_glob_get(res, 99) };
        assert!(ptr.is_null());
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
    }
}
