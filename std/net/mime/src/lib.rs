//! Hew runtime: MIME type detection from file paths and extensions.
//!
//! Provides MIME type guessing and classification for compiled Hew programs.
//! All returned strings are allocated with `libc::malloc` and NUL-terminated.

use hew_cabi::cabi::str_to_malloc;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::path::Path;

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Guess the MIME type from a file path (e.g. `"index.html"` → `"text/html"`).
///
/// Returns a `malloc`-allocated, NUL-terminated C string. Falls back to
/// `"application/octet-stream"` if the type cannot be determined. Returns null
/// only on null input.
///
/// # Safety
///
/// `path` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_mime_from_path(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: path is a valid NUL-terminated C string per caller contract.
    let Ok(path_str) = unsafe { CStr::from_ptr(path) }.to_str() else {
        return str_to_malloc("application/octet-stream");
    };
    let mime = mime_guess::from_path(Path::new(path_str)).first_or_octet_stream();
    str_to_malloc(mime.as_ref())
}

/// Guess the MIME type from a file extension without the leading dot
/// (e.g. `"html"` → `"text/html"`).
///
/// Returns a `malloc`-allocated, NUL-terminated C string. Falls back to
/// `"application/octet-stream"` if the type cannot be determined. Returns null
/// only on null input.
///
/// # Safety
///
/// `ext` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_mime_from_ext(ext: *const c_char) -> *mut c_char {
    if ext.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: ext is a valid NUL-terminated C string per caller contract.
    let Ok(ext_str) = unsafe { CStr::from_ptr(ext) }.to_str() else {
        return str_to_malloc("application/octet-stream");
    };
    let mime = mime_guess::from_ext(ext_str).first_or_octet_stream();
    str_to_malloc(mime.as_ref())
}

/// Check whether a MIME type string starts with `"text/"`.
///
/// Returns 1 if it does, 0 otherwise.
///
/// # Safety
///
/// `mime` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_mime_is_text(mime: *const c_char) -> i32 {
    // SAFETY: mime is a valid NUL-terminated C string per caller contract;
    // forwarding to mime_starts_with with same guarantee.
    unsafe { mime_starts_with(mime, "text/") }
}

/// Check whether a MIME type string starts with `"image/"`.
///
/// Returns 1 if it does, 0 otherwise.
///
/// # Safety
///
/// `mime` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_mime_is_image(mime: *const c_char) -> i32 {
    // SAFETY: mime is a valid NUL-terminated C string per caller contract;
    // forwarding to mime_starts_with with same guarantee.
    unsafe { mime_starts_with(mime, "image/") }
}

/// Check whether a MIME type string starts with `"audio/"`.
///
/// Returns 1 if it does, 0 otherwise.
///
/// # Safety
///
/// `mime` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_mime_is_audio(mime: *const c_char) -> i32 {
    // SAFETY: mime is a valid NUL-terminated C string per caller contract;
    // forwarding to mime_starts_with with same guarantee.
    unsafe { mime_starts_with(mime, "audio/") }
}

/// Check whether a MIME type string starts with `"video/"`.
///
/// Returns 1 if it does, 0 otherwise.
///
/// # Safety
///
/// `mime` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_mime_is_video(mime: *const c_char) -> i32 {
    // SAFETY: mime is a valid NUL-terminated C string per caller contract;
    // forwarding to mime_starts_with with same guarantee.
    unsafe { mime_starts_with(mime, "video/") }
}

/// Free a string previously returned by [`hew_mime_from_path`] or
/// [`hew_mime_from_ext`].
///
/// # Safety
///
/// `s` must be a pointer previously returned by a `hew_mime_*` string function,
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_mime_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc in str_to_malloc.
    unsafe { libc::free(s.cast()) };
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Check if the MIME string starts with a given prefix.
///
/// # Safety
///
/// `mime` must be a valid NUL-terminated C string, or null.
unsafe fn mime_starts_with(mime: *const c_char, prefix: &str) -> i32 {
    if mime.is_null() {
        return 0;
    }
    // SAFETY: mime is a valid NUL-terminated C string per caller contract.
    let Ok(mime_str) = unsafe { CStr::from_ptr(mime) }.to_str() else {
        return 0;
    };
    i32::from(mime_str.starts_with(prefix))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn mime_from_path_html() {
        let path = CString::new("index.html").unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let result = unsafe { hew_mime_from_path(path.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string from hew_mime_from_path.
        assert_eq!(unsafe { read_and_free(result) }, "text/html");
    }

    #[test]
    fn mime_from_ext_json() {
        let ext = CString::new("json").unwrap();
        // SAFETY: ext is a valid NUL-terminated C string.
        let result = unsafe { hew_mime_from_ext(ext.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string from hew_mime_from_ext.
        assert_eq!(unsafe { read_and_free(result) }, "application/json");
    }

    #[test]
    fn mime_from_path_image() {
        let path = CString::new("photo.png").unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let result = unsafe { hew_mime_from_path(path.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string from hew_mime_from_path.
        assert_eq!(unsafe { read_and_free(result) }, "image/png");
    }

    #[test]
    fn mime_unknown_extension() {
        let path = CString::new("data.xyz_unknown_ext").unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let result = unsafe { hew_mime_from_path(path.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string from hew_mime_from_path.
        assert_eq!(unsafe { read_and_free(result) }, "application/octet-stream");
    }

    #[test]
    fn mime_type_classification() {
        let text = CString::new("text/html").unwrap();
        let image = CString::new("image/png").unwrap();
        let audio = CString::new("audio/mpeg").unwrap();
        let video = CString::new("video/mp4").unwrap();
        let app = CString::new("application/json").unwrap();

        // SAFETY: all CString pointers are valid NUL-terminated strings.
        unsafe {
            assert_eq!(hew_mime_is_text(text.as_ptr()), 1);
            assert_eq!(hew_mime_is_text(image.as_ptr()), 0);

            assert_eq!(hew_mime_is_image(image.as_ptr()), 1);
            assert_eq!(hew_mime_is_image(text.as_ptr()), 0);

            assert_eq!(hew_mime_is_audio(audio.as_ptr()), 1);
            assert_eq!(hew_mime_is_audio(text.as_ptr()), 0);

            assert_eq!(hew_mime_is_video(video.as_ptr()), 1);
            assert_eq!(hew_mime_is_video(app.as_ptr()), 0);
        }
    }

    #[test]
    fn null_handling() {
        // SAFETY: null is safe for all functions.
        unsafe {
            assert!(hew_mime_from_path(std::ptr::null()).is_null());
            assert!(hew_mime_from_ext(std::ptr::null()).is_null());
            assert_eq!(hew_mime_is_text(std::ptr::null()), 0);
            assert_eq!(hew_mime_is_image(std::ptr::null()), 0);
            assert_eq!(hew_mime_is_audio(std::ptr::null()), 0);
            assert_eq!(hew_mime_is_video(std::ptr::null()), 0);
            hew_mime_free(std::ptr::null_mut());
        }
    }
}
