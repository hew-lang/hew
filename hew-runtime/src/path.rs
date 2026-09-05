//! Hew runtime: `path` module — filesystem metadata and glob expansion.
//!
//! Provides the C ABI symbols called from `std/path.hew`:
//!
//! - `hew_path_is_file`   — test whether a path is a regular file
//! - `hew_path_is_dir`    — test whether a path is a directory
//! - `hew_path_absolute`  — resolve a path to an absolute form
//! - `hew_glob`           — expand a glob pattern via POSIX `glob(3)`
//! - `hew_glob_is_valid`  — whether the expansion completed without error
//! - `hew_glob_error`     — the expansion failure detail, when there is one
//! - `hew_glob_count`     — number of matched paths in a `HewGlobResult`
//! - `hew_glob_get`       — retrieve a matched path by index
//! - `hew_glob_free`      — release a `HewGlobResult`
//!
//! The metadata helpers fail-closed (null / false / 0) rather than propagating
//! I/O errors, consistent with the existing `hew_path_exists` contract in
//! `file_io.rs`.  Glob expansion uses the platform's POSIX `glob(3)` via
//! `libc` and copies the matched strings into a heap-owned `Vec<String>` so
//! that the `libc::glob_t` can be freed immediately after the walk.  An
//! expansion that aborts (an unreadable directory, an allocation failure, or a
//! platform with no implementation) records the failure on the result instead
//! of returning zero matches, so a caller can tell "nothing matched" apart from
//! "the walk never completed".
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use hew_cabi::string::{string_as_str, string_from_str, HewString};
use std::ffi::CStr;

// ── Simple filesystem-metadata helpers ──────────────────────────────────────

/// Test whether `path` refers to a regular file.
///
/// Returns `1` if the path names a regular file, `0` otherwise (including on
/// empty input, interior NUL, I/O error, or any other failure).
///
/// # Safety
///
/// `path` must be a live managed string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_path_is_file(path: *const HewString) -> i32 {
    if path.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `path` is a live managed string.
    let rust_path = unsafe { string_as_str(path) };
    if rust_path.contains('\0') {
        return 0;
    }
    i32::from(std::path::Path::new(rust_path).is_file())
}

/// Test whether `path` refers to a directory.
///
/// Returns `1` if the path names a directory, `0` otherwise (including on
/// empty input, interior NUL, I/O error, or any other failure).
///
/// # Safety
///
/// `path` must be a live managed string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_path_is_dir(path: *const HewString) -> i32 {
    if path.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `path` is a live managed string.
    let rust_path = unsafe { string_as_str(path) };
    if rust_path.contains('\0') {
        return 0;
    }
    i32::from(std::path::Path::new(rust_path).is_dir())
}

/// Return the absolute form of `path`.
///
/// Resolves the path against the process working directory using
/// [`std::path::absolute`]. Returns an owned managed string that the caller
/// must release with `hew_string_drop`.
///
/// Returns null on empty input, interior NUL, or resolution failure.
///
/// # Safety
///
/// `path` must be a live managed string (or null).
///
/// # Ownership
///
/// The caller owns the returned pointer and must free it with `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_path_absolute(path: *const HewString) -> *mut HewString {
    if path.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `path` is a live managed string.
    let rust_path = unsafe { string_as_str(path) };
    if rust_path.contains('\0') {
        return std::ptr::null_mut();
    }
    let Ok(abs) = std::path::absolute(rust_path) else {
        return std::ptr::null_mut();
    };
    let Some(abs_str) = abs.to_str() else {
        return std::ptr::null_mut();
    };
    string_from_str(abs_str)
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
    /// `Some(detail)` when the expansion did not complete.  A failed
    /// expansion has no meaningful match list.
    error: Option<String>,
}

/// Expand a glob pattern and return the matches, or the failure detail.
type GlobExpansion = Result<Vec<String>, String>;

/// Expand `pattern` using POSIX `glob(3)` and return a `HewGlobResult`.
///
/// The returned pointer is heap-allocated and must be freed by calling
/// `hew_glob_free`.  Returns a non-null pointer even when there are zero
/// matches — the caller uses `hew_glob_count` to check.  A pattern whose
/// expansion aborted produces a result that reports `hew_glob_is_valid` as
/// false, which is distinct from a completed expansion with zero matches.
///
/// Returns null only on allocation failure.
///
/// # Safety
///
/// `pattern` must be a live managed string (or null).
///
/// # Ownership
///
/// The caller owns the returned pointer and must release it with `hew_glob_free`.
#[no_mangle]
pub unsafe extern "C" fn hew_glob(pattern: *const HewString) -> *mut HewGlobResult {
    if pattern.is_null() {
        crate::set_last_error("hew_glob: pattern is null");
        return Box::into_raw(Box::new(HewGlobResult {
            matches: vec![],
            error: Some("hew_glob: pattern is null".to_owned()),
        }));
    }
    // SAFETY: caller guarantees `pattern` is a live managed string.
    let expansion = unsafe { hew_cabi::string::string_to_cstring(pattern) }
        .map_err(|_| "hew_glob: pattern contains interior NUL".to_owned())
        .and_then(|pattern| glob_expand(&pattern));

    match expansion {
        Ok(matches) => {
            crate::hew_clear_error();
            Box::into_raw(Box::new(HewGlobResult {
                matches,
                error: None,
            }))
        }
        Err(detail) => {
            crate::set_last_error(detail.clone());
            Box::into_raw(Box::new(HewGlobResult {
                matches: vec![],
                error: Some(detail),
            }))
        }
    }
}

/// Perform the POSIX `glob(3)` expansion and return the matched strings.
///
/// Uses `libc::glob` on unix targets.
fn glob_expand(pattern: &CStr) -> GlobExpansion {
    #[cfg(target_family = "unix")]
    {
        glob_expand_unix(pattern)
    }
    // SHIM: Windows glob not yet implemented.  The symbols stay present and
    // link-clean, and the gap is RECORDED (fail-closed: the expansion reports
    // an explicit failure, so it can never be read as "no matches").
    // WHEN obsolete: when a Windows target gains an end-to-end path/glob test
    // coverage.  WHAT the real solution looks like: FindFirstFileW/FindNextFileW
    // expansion with the same HewGlobResult ownership contract.
    #[cfg(not(target_family = "unix"))]
    {
        let _ = pattern;
        Err("hew_glob: glob expansion is not implemented on this platform".to_owned())
    }
}

#[cfg(target_family = "unix")]
fn glob_expand_unix(pattern: &CStr) -> GlobExpansion {
    use libc::{glob as libc_glob, glob_t, globfree, GLOB_ABORTED, GLOB_ERR, GLOB_NOMATCH};

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
    // rc == GLOB_NOMATCH → the walk completed and matched nothing
    // other          → the walk aborted (unreadable directory, out of memory);
    //                  the partial match list is not a result
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

    let pattern_text = pattern.to_string_lossy();
    match rc {
        0 => Ok(results),
        GLOB_NOMATCH => Ok(Vec::new()),
        GLOB_ABORTED => Err(format!(
            "hew_glob: expansion of '{pattern_text}' aborted: a matching directory could not be read"
        )),
        other => Err(format!(
            "hew_glob: expansion of '{pattern_text}' failed with glob(3) status {other}"
        )),
    }
}

/// Report whether the expansion behind `result` completed.
///
/// Returns `false` on null input and on any expansion that aborted, so an
/// empty match list from a completed walk is never confused with a failure.
///
/// # Safety
///
/// `result` must be a pointer returned by [`hew_glob`] that has not yet been
/// freed, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_glob_is_valid(result: *mut HewGlobResult) -> bool {
    if result.is_null() {
        return false;
    }
    // SAFETY: caller guarantees `result` is a live HewGlobResult.
    let r = unsafe { &*result };
    r.error.is_none()
}

/// Return the failure detail recorded on `result` as an owned managed
/// string, or an empty string when the expansion completed.
///
/// # Safety
///
/// `result` must be a pointer returned by [`hew_glob`] that has not yet been
/// freed, or null.
///
/// # Ownership
///
/// The returned pointer is a managed owner.  The caller must free it with
/// `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_glob_error(result: *mut HewGlobResult) -> *mut HewString {
    if result.is_null() {
        return string_from_str("hew_glob: result handle is null");
    }
    // SAFETY: caller guarantees `result` is a live HewGlobResult.
    let r = unsafe { &*result };
    match r.error.as_deref() {
        Some(detail) => string_from_str(detail),
        None => string_from_str(""),
    }
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

/// Return the path at `index` from `result` as an owned managed string.
///
/// Returns null on null input or out-of-range index.
///
/// # Safety
///
/// `result` must be a pointer returned by [`hew_glob`] that has not yet been
/// freed, or null.
///
/// # Ownership
///
/// The returned pointer is a managed owner.  The caller must free it with
/// `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_glob_get(result: *mut HewGlobResult, index: i32) -> *mut HewString {
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
    string_from_str(s)
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
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    #[test]
    fn managed_paths_reject_nul_before_os_calls() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("present.txt");
        std::fs::write(&file, "unchanged").unwrap();
        let file_path = ManagedString::new(format!("{}\0suffix", file.display()));
        let dir_path = ManagedString::new(format!("{}\0suffix", dir.path().display()));
        // SAFETY: the managed inputs remain live through every borrowed call.
        unsafe {
            assert_eq!(hew_path_is_file(file_path.as_ptr()), 0);
            assert_eq!(hew_path_is_dir(dir_path.as_ptr()), 0);
            assert!(hew_path_absolute(file_path.as_ptr()).is_null());
            let glob = hew_glob(file_path.as_ptr());
            assert!(!hew_glob_is_valid(glob));
            let error = hew_glob_error(glob);
            hew_glob_free(glob);
            assert!(string_as_str(error).contains("interior NUL"));
            string_release(error);
        }
        assert_eq!(std::fs::read_to_string(file).unwrap(), "unchanged");
    }
    use std::path::PathBuf;

    fn cpath(p: &std::path::Path) -> ManagedString {
        ManagedString::new(p.to_str().unwrap())
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
        // SAFETY: p is a live managed string.
        assert_eq!(unsafe { hew_path_is_file(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_file_returns_zero_for_directory() {
        let dir = test_dir("is_file_dir");
        let p = cpath(&dir);
        // SAFETY: p is a live managed string (directory path).
        assert_eq!(unsafe { hew_path_is_file(p.as_ptr()) }, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_file_returns_zero_for_nonexistent() {
        let p = ManagedString::new("/tmp/hew_path_is_file_ghost_12345");
        // SAFETY: p is a live managed string.
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
        // SAFETY: p is a live managed string.
        assert_eq!(unsafe { hew_path_is_dir(p.as_ptr()) }, 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_dir_returns_zero_for_regular_file() {
        let dir = test_dir("is_dir_file");
        let f = dir.join("test.txt");
        std::fs::write(&f, "x").unwrap();
        let p = cpath(&f);
        // SAFETY: p is a live managed string.
        assert_eq!(unsafe { hew_path_is_dir(p.as_ptr()) }, 0);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn is_dir_returns_zero_for_nonexistent() {
        let p = ManagedString::new("/tmp/hew_path_is_dir_ghost_12345");
        // SAFETY: p is a live managed string.
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
        let p = ManagedString::new("/tmp");
        // SAFETY: p is a live managed string.
        let ptr = unsafe { hew_path_absolute(p.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr was returned by hew_path_absolute (string_from_str alloc).
        unsafe { string_release(ptr) };
    }

    #[test]
    fn absolute_resolves_dot_to_cwd() {
        let p = ManagedString::new(".");
        // SAFETY: p is a live managed string.
        let ptr = unsafe { hew_path_absolute(p.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a live managed string from string_from_str.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: ptr was returned by hew_path_absolute.
        unsafe { string_release(ptr) };
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
        let cp = ManagedString::new(pattern);
        // SAFETY: cp is a live managed string.
        let res = unsafe { hew_glob(cp.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        assert!(unsafe { hew_glob_is_valid(res) });
        // SAFETY: res is a live HewGlobResult.
        let count = unsafe { hew_glob_count(res) };
        assert_eq!(count, 2);
        // SAFETY: res is live; index 0 is valid.
        let p0 = unsafe { hew_glob_get(res, 0) };
        assert!(!p0.is_null());
        // SAFETY: p0 is a live managed string from string_from_str.
        unsafe { string_release(p0) };
        // SAFETY: res is a live HewGlobResult.
        unsafe { hew_glob_free(res) };
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// Read `hew_glob_error` for `res` as an owned Rust string.
    fn glob_error_text(res: *mut HewGlobResult) -> String {
        // SAFETY: res is a live HewGlobResult or null; both are in contract.
        let ptr = unsafe { hew_glob_error(res) };
        // SAFETY: ptr is a live managed string from string_from_str.
        let text = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: ptr was returned by hew_glob_error (string_from_str alloc).
        unsafe { string_release(ptr) };
        text
    }

    // Pins the non-unix SHIM in glob_expand: matching files exist, but the
    // unimplemented platform reports an explicit expansion failure rather than
    // zero matches (fail-closed: never readable as a fabricated "no matches").
    #[cfg(not(target_family = "unix"))]
    #[test]
    fn glob_unsupported_platform_reports_failure_not_no_matches() {
        let dir = test_dir("glob_match");
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let pattern = format!("{}/*.txt", dir.to_str().unwrap());
        let cp = ManagedString::new(pattern);
        // SAFETY: cp is a live managed string.
        let res = unsafe { hew_glob(cp.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        assert!(!unsafe { hew_glob_is_valid(res) });
        assert!(glob_error_text(res).contains("glob expansion is not implemented"));
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn glob_no_match_reflects_the_platform_contract() {
        let p = ManagedString::new("/tmp/hew_path_glob_nomatch_*_zzz_999");
        // SAFETY: p is a live managed string.
        let res = unsafe { hew_glob(p.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        let count = unsafe { hew_glob_count(res) };
        assert_eq!(count, 0);
        #[cfg(target_family = "unix")]
        {
            // A completed POSIX walk that matched nothing is a success.
            // SAFETY: res is a live HewGlobResult.
            assert!(unsafe { hew_glob_is_valid(res) });
            assert_eq!(glob_error_text(res), "");
        }
        #[cfg(not(target_family = "unix"))]
        {
            // The non-Unix shim cannot perform the walk, so zero paths is an
            // explicit unsupported-platform failure, not "no matches".
            // SAFETY: res is a live HewGlobResult.
            assert!(!unsafe { hew_glob_is_valid(res) });
            assert!(glob_error_text(res).contains("glob expansion is not implemented"));
        }
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
    }

    // #22: an aborted walk used to be indistinguishable from "no matches".
    // A directory with no read permission is the portable POSIX reproducer:
    // GLOB_ERR makes glob(3) return GLOB_ABORTED with zero paths collected.
    #[cfg(target_family = "unix")]
    #[test]
    fn glob_unreadable_directory_reports_failure_not_no_matches() {
        use std::os::unix::fs::PermissionsExt as _;

        let dir = test_dir("glob_denied");
        let denied = dir.join("denied");
        std::fs::create_dir_all(&denied).unwrap();
        std::fs::write(denied.join("present.txt"), "x").unwrap();
        std::fs::set_permissions(&denied, std::fs::Permissions::from_mode(0o000)).unwrap();

        let readable = std::fs::read_dir(&denied).is_ok();
        // A privileged test runner can read a 0o000 directory; the abort
        // condition does not exist there, so there is nothing to assert.
        if !readable {
            let pattern = format!("{}/*", denied.to_str().unwrap());
            let cp = ManagedString::new(pattern);
            // SAFETY: cp is a live managed string.
            let res = unsafe { hew_glob(cp.as_ptr()) };
            assert!(!res.is_null());
            // SAFETY: res is a live HewGlobResult.
            assert_eq!(unsafe { hew_glob_count(res) }, 0);
            // SAFETY: res is a live HewGlobResult.
            let completed = unsafe { hew_glob_is_valid(res) };
            assert!(
                !completed,
                "an aborted walk must not report as a completed expansion"
            );
            assert!(glob_error_text(res).contains("aborted"));
            // SAFETY: res is live.
            unsafe { hew_glob_free(res) };
        }

        std::fs::set_permissions(&denied, std::fs::Permissions::from_mode(0o755)).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn glob_null_pattern_reports_failure() {
        // SAFETY: null is the value under test; hew_glob handles it.
        let res = unsafe { hew_glob(std::ptr::null()) };
        assert!(!res.is_null());
        // SAFETY: res is a live HewGlobResult.
        let count = unsafe { hew_glob_count(res) };
        assert_eq!(count, 0);
        // SAFETY: res is a live HewGlobResult.
        assert!(!unsafe { hew_glob_is_valid(res) });
        assert!(glob_error_text(res).contains("pattern is null"));
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
    }

    #[test]
    fn glob_is_valid_and_error_handle_null_result() {
        // SAFETY: null is explicitly allowed by both contracts.
        assert!(!unsafe { hew_glob_is_valid(std::ptr::null_mut()) });
        assert!(glob_error_text(std::ptr::null_mut()).contains("null"));
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
        let p = ManagedString::new("/tmp/hew_path_glob_oor_*_zzz_999");
        // SAFETY: p is a live managed string.
        let res = unsafe { hew_glob(p.as_ptr()) };
        assert!(!res.is_null());
        // SAFETY: res is live; index 99 is out of range for zero matches.
        let ptr = unsafe { hew_glob_get(res, 99) };
        assert!(ptr.is_null());
        // SAFETY: res is live.
        unsafe { hew_glob_free(res) };
    }
}
