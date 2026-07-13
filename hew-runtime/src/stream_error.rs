//! Single-owner thread-local error channel for fallible stream/sink operations.
//!
//! This module is the ONE definition of the `hew_stream_*` C ABI in the entire
//! linked image. It lives in `hew-runtime` (which is bundled into `libhew.a`)
//! rather than in `hew-cabi` for two load-bearing reasons:
//!
//!   * **Link correctness.** `hew-cabi` is statically linked into every native
//!     package (HTTP, and any ecosystem module). If the `#[no_mangle]`
//!     definitions lived in `hew-cabi`, each package staticlib would carry its
//!     own copy of `hew_stream_last_error` / `hew_stream_last_errno`, and the
//!     final link against `libhew.a` would fail with a duplicate-symbol error
//!     (the macOS linker rejects these unconditionally). Declaring them here —
//!     and only here — means a package's `hew-cabi` references them as
//!     undefined symbols that resolve against `libhew.a` at final link.
//!
//!   * **State correctness.** The error state is a thread-local. If each package
//!     carried its own copy, a producer inside a separately-compiled package
//!     would write *its* thread-local while the program read *libhew*'s — and
//!     the error would be silently lost (fail-soft). A single owner keeps the
//!     producer (`hew_cabi::sink::set_last_error`) and the consumer
//!     (`hew_stream_last_error`) pointed at the same cell.
//!
//! `hew-cabi` declares these four symbols as `extern "C"` imports and exposes
//! thin safe wrappers, so the public `hew_cabi::sink` API is unchanged.

use std::cell::RefCell;
use std::ffi::c_char;

thread_local! {
    static LAST_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
    /// OS errno associated with the last error, or 0 when not set.
    static LAST_ERRNO: RefCell<i32> = const { RefCell::new(0) };
    /// Canonical, platform-independent error-kind tag for the last error, or
    /// 0 (`IO_ERROR_KIND_UNCLASSIFIED`) when not set. See [`io_error_kind_tag`].
    static LAST_ERROR_KIND: RefCell<i32> = const { RefCell::new(0) };
}

// ── Canonical error-kind tags ─────────────────────────────────────────────────
//
// POSIX errno and Windows Win32 error codes diverge — "already exists" is
// `EEXIST` = 17 on unix but `ERROR_ALREADY_EXISTS` = 183 on Windows, and
// "access denied" is `EACCES` = 13 vs `ERROR_ACCESS_DENIED` = 5 — and worse,
// some codes *collide* with different meanings across the two spaces (Win32 5 =
// access-denied, POSIX 5 = `EIO`). A classifier that sees only the raw integer
// therefore cannot be correct on both platforms at once.
//
// `std::io::ErrorKind` is the portable authority, but it only exists runtime
// side where the `io::Error` is in scope. These tags carry that classification
// across the C ABI so `std/fs.hew` picks the right `IoError` variant identically
// on every platform, while the variant payload still surfaces the raw OS errno
// for platform-specific inspection.
//
// Tag 0 means "unclassified": the consumer falls back to mapping the raw errno,
// which preserves platform detail for kinds without a canonical tag (for
// example `EISDIR`/`ENOTDIR` -> `IoError::Other(raw)`). Keep the tag values in
// sync with `io_error_from_kind` in `std/fs.hew`.

/// No canonical classification; the consumer falls back to raw-errno mapping.
pub const IO_ERROR_KIND_UNCLASSIFIED: i32 = 0;
/// Portable `std::io::ErrorKind::NotFound`.
pub const IO_ERROR_KIND_NOT_FOUND: i32 = 1;
/// Portable `std::io::ErrorKind::PermissionDenied`.
pub const IO_ERROR_KIND_PERMISSION_DENIED: i32 = 2;
/// Portable `std::io::ErrorKind::AlreadyExists`.
pub const IO_ERROR_KIND_ALREADY_EXISTS: i32 = 3;
/// Portable `std::io::ErrorKind::TimedOut`.
pub const IO_ERROR_KIND_TIMED_OUT: i32 = 4;

/// Map a portable [`std::io::ErrorKind`] to its canonical cross-platform tag.
///
/// Returns [`IO_ERROR_KIND_UNCLASSIFIED`] for kinds without a stable
/// user-facing `IoError` variant, so the consumer keeps the raw errno detail
/// rather than flattening it to a wrong classification.
#[must_use]
pub fn io_error_kind_tag(kind: std::io::ErrorKind) -> i32 {
    use std::io::ErrorKind;
    match kind {
        ErrorKind::NotFound => IO_ERROR_KIND_NOT_FOUND,
        ErrorKind::PermissionDenied => IO_ERROR_KIND_PERMISSION_DENIED,
        ErrorKind::AlreadyExists => IO_ERROR_KIND_ALREADY_EXISTS,
        ErrorKind::TimedOut => IO_ERROR_KIND_TIMED_OUT,
        _ => IO_ERROR_KIND_UNCLASSIFIED,
    }
}

// ── In-crate Rust helpers ─────────────────────────────────────────────────────
//
// Used directly by the runtime's own fallible call sites (`stream`, `file_io`).
// Native packages reach the same thread-local through the C ABI below, via the
// `hew_cabi::sink` wrappers.

/// Store an error message for the current thread. Retrievable via
/// [`hew_stream_last_error`]. The associated errno and error-kind are cleared.
pub fn set_last_error(msg: String) {
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
    LAST_ERRNO.with(|e| *e.borrow_mut() = 0);
    LAST_ERROR_KIND.with(|k| *k.borrow_mut() = 0);
}

/// Store an error message together with its OS errno for the current thread.
/// Both are retrievable via [`hew_stream_last_error`] and [`hew_stream_last_errno`].
///
/// The canonical error-kind is reset to unclassified: callers on this path do
/// not have a Rust `io::Error` in scope, so the consumer maps the raw errno.
pub fn set_last_error_with_errno(msg: String, errno: i32) {
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
    LAST_ERRNO.with(|e| *e.borrow_mut() = errno);
    LAST_ERROR_KIND.with(|k| *k.borrow_mut() = 0);
}

/// Store an error message together with its OS errno *and* its canonical,
/// platform-independent error-kind tag for the current thread. Retrievable via
/// [`hew_stream_last_error`], [`hew_stream_last_errno`], and
/// [`hew_stream_last_error_kind`].
///
/// Use this at call sites where a Rust [`std::io::Error`] is in scope, passing
/// `errno = error.raw_os_error()` and `kind = io_error_kind_tag(error.kind())`,
/// so the raw platform code is preserved while classification stays correct
/// across platforms whose errno spaces diverge.
pub fn set_last_error_with_errno_and_kind(msg: String, errno: i32, kind: i32) {
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
    LAST_ERRNO.with(|e| *e.borrow_mut() = errno);
    LAST_ERROR_KIND.with(|k| *k.borrow_mut() = kind);
}

/// Take and clear the last error, if any. Also clears the associated errno and
/// error-kind.
#[must_use]
pub fn take_last_error() -> Option<String> {
    LAST_ERRNO.with(|e| *e.borrow_mut() = 0);
    LAST_ERROR_KIND.with(|k| *k.borrow_mut() = 0);
    LAST_ERROR.with(|e| e.borrow_mut().take())
}

/// Take and clear the last errno, if any. Returns 0 when none was set.
///
/// The canonical error-kind is metadata of the errno, so it is cleared here
/// too: a caller that reads only the errno cannot leave a stale classification
/// parked for an unrelated later read.
#[must_use]
pub fn take_last_errno() -> i32 {
    LAST_ERROR_KIND.with(|k| *k.borrow_mut() = 0);
    LAST_ERRNO.with(|e| {
        let v = *e.borrow();
        *e.borrow_mut() = 0;
        v
    })
}

/// Take and clear the canonical error-kind tag, if any. Returns
/// [`IO_ERROR_KIND_UNCLASSIFIED`] when none was set.
///
/// Consumers MUST read this before [`take_last_errno`] / [`hew_stream_last_errno`],
/// which clear the tag as errno metadata.
#[must_use]
pub fn take_last_error_kind() -> i32 {
    LAST_ERROR_KIND.with(|k| {
        let v = *k.borrow();
        *k.borrow_mut() = 0;
        v
    })
}

/// Reconstruct an owned `String` from a borrowed byte range. A null pointer or
/// zero length yields an empty string. Invalid UTF-8 is replaced lossily; the
/// producers (`hew_cabi::sink`) always pass valid UTF-8, so this is defensive.
///
/// # Safety
/// When `ptr` is non-null and `len` is non-zero, `ptr` must point to at least
/// `len` initialized bytes that remain valid for the duration of the call.
unsafe fn bytes_to_string(ptr: *const u8, len: usize) -> String {
    if ptr.is_null() || len == 0 {
        return String::new();
    }
    // SAFETY: caller guarantees `ptr` is valid for `len` initialized bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    String::from_utf8_lossy(slice).into_owned()
}

// ── C ABI (the single definition in the linked image) ─────────────────────────

/// Producer entry point for native packages: store an error message for the
/// current thread (errno cleared to 0). Reached via `hew_cabi::sink::set_last_error`.
///
/// # Safety
/// `ptr`/`len` must describe a valid initialized byte range, or `ptr` may be
/// null with `len` 0. The bytes are copied into the runtime-owned thread-local;
/// the caller's buffer is not retained.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_set_last_error(ptr: *const u8, len: usize) {
    // SAFETY: forwarded contract — see this fn's `# Safety`.
    let msg = unsafe { bytes_to_string(ptr, len) };
    set_last_error(msg);
}

/// Producer entry point for native packages: store an error message together
/// with its OS errno. Reached via `hew_cabi::sink::set_last_error_with_errno`.
///
/// # Safety
/// As [`hew_stream_set_last_error`]; `errno` is stored verbatim.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_set_last_error_with_errno(
    ptr: *const u8,
    len: usize,
    errno: i32,
) {
    // SAFETY: forwarded contract — see this fn's `# Safety`.
    let msg = unsafe { bytes_to_string(ptr, len) };
    set_last_error_with_errno(msg, errno);
}

/// Return the last stream/sink error as a header-aware C string, or NULL if none.
///
/// Clears both the error message and the associated errno after reading.
/// The returned string is allocated via [`hew_cabi::cabi::alloc_cstring_from_str`]
/// (the header-aware path) and MUST be released through `hew_string_drop` /
/// [`hew_cabi::cabi::free_cstring`]. Bare `libc::free` will abort.
///
/// ALLOCATOR-PAIRING: cstring (`alloc_cstring_from_str` — header-aware; must be
/// freed via `free_cstring` / `hew_string_drop`, never bare `libc::free`).
#[no_mangle]
pub extern "C" fn hew_stream_last_error() -> *mut c_char {
    match take_last_error() {
        Some(msg) => {
            // Use the header-aware allocator so that the Hew drop spine's
            // hew_string_drop → free_cstring can verify the magic sentinel and
            // release the allocation without aborting.  If the message contains
            // interior NUL bytes (rare: OS errors never do, but defensive), fall
            // back to a safe diagnostic string before allocating.
            let safe_msg: &str = if msg.contains('\0') {
                "hew_stream_last_error: stored error message contained interior NUL"
            } else {
                &msg
            };
            let ptr = hew_cabi::cabi::alloc_cstring_from_str(safe_msg); // ALLOCATOR-PAIRING: cstring
            if ptr.is_null() {
                set_last_error("hew_stream_last_error: allocation failed".to_string());
                return std::ptr::null_mut();
            }
            ptr
        }
        None => std::ptr::null_mut(),
    }
}

/// Return the OS errno associated with the last stream/sink error, or 0 if none.
///
/// Clears the errno after reading. This export is intentionally independent of
/// [`hew_stream_last_error`] — callers may read errno without consuming the message,
/// or vice-versa, though typical usage reads both.
///
/// INTERNAL-ABI: populated by `set_last_error_with_errno` at runtime call sites where
/// a Rust `io::Error` is in scope. String-only errors (no OS errno) leave this as 0.
#[no_mangle]
pub extern "C" fn hew_stream_last_errno() -> i32 {
    take_last_errno()
}

/// Return the canonical, platform-independent error-kind tag associated with
/// the last stream/sink error, or 0 (`IO_ERROR_KIND_UNCLASSIFIED`) if none.
///
/// Clears the tag after reading. Consumers MUST read this *before*
/// [`hew_stream_last_errno`], which also clears the tag as errno metadata. See
/// the `IO_ERROR_KIND_*` constants and [`io_error_kind_tag`] for the mapping;
/// `std/fs.hew` uses this to classify I/O failures identically across platforms
/// whose raw errno spaces diverge (POSIX `EEXIST` = 17 vs Win32
/// `ERROR_ALREADY_EXISTS` = 183, etc.) while still surfacing the raw errno in
/// the `IoError` payload.
///
/// INTERNAL-ABI: populated by `set_last_error_with_errno_and_kind` at runtime
/// call sites where a Rust `io::Error` is in scope. Error paths without a Rust
/// error kind leave this as 0.
#[no_mangle]
pub extern "C" fn hew_stream_last_error_kind() -> i32 {
    take_last_error_kind()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;

    // ── Thread-local error helpers ───────────────────────────────────────

    #[test]
    fn take_last_error_returns_none_when_empty() {
        // Clear any residual state from other tests in this thread.
        let _ = take_last_error();
        assert_eq!(take_last_error(), None);
    }

    #[test]
    fn set_and_take_roundtrip() {
        set_last_error("something went wrong".to_string());
        let err = take_last_error();
        assert_eq!(err.as_deref(), Some("something went wrong"));
    }

    #[test]
    fn take_clears_the_error() {
        set_last_error("first".to_string());
        let _ = take_last_error();
        assert_eq!(take_last_error(), None, "second take should be None");
    }

    #[test]
    fn set_overwrites_previous_error() {
        set_last_error("old".to_string());
        set_last_error("new".to_string());
        assert_eq!(take_last_error().as_deref(), Some("new"));
    }

    #[test]
    fn set_last_error_empty_string() {
        set_last_error(String::new());
        assert_eq!(take_last_error().as_deref(), Some(""));
    }

    // ── hew_stream_last_error (C ABI) ────────────────────────────────────

    #[test]
    fn hew_stream_last_error_returns_null_when_no_error() {
        // Clear any residual state.
        let _ = take_last_error();
        let ptr = hew_stream_last_error();
        assert!(ptr.is_null());
    }

    #[test]
    fn hew_stream_last_error_returns_valid_cstring() {
        set_last_error("connection refused".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly alloc_cstring_from_str (header-aware) allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "connection refused");
            hew_cabi::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn hew_stream_last_error_clears_after_read() {
        set_last_error("timeout".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open
                                                      // Second call should return null — error was consumed.
        let ptr2 = hew_stream_last_error();
        assert!(ptr2.is_null(), "error should be cleared after first read");
    }

    #[test]
    fn hew_stream_last_error_handles_unicode() {
        set_last_error("échec de connexion 🔥".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "échec de connexion 🔥");
            hew_cabi::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn hew_stream_last_error_surfaces_interior_nul_diagnostic() {
        set_last_error("bad\0message".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert!(recovered.contains("contained interior NUL"));
            hew_cabi::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    // ── hew_stream_last_errno (C ABI) ────────────────────────────────────

    #[test]
    fn hew_stream_last_errno_returns_zero_when_not_set() {
        // Clear any residual state.
        let _ = take_last_error();
        let _ = take_last_errno();
        assert_eq!(hew_stream_last_errno(), 0);
    }

    #[test]
    fn set_last_error_with_errno_roundtrip_via_abi() {
        set_last_error_with_errno("connection refused".to_string(), 111);
        assert_eq!(hew_stream_last_errno(), 111);
        // Consuming errno does not consume the message.
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "connection refused");
            hew_cabi::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn hew_stream_last_errno_clears_after_read() {
        set_last_error_with_errno("some error".to_string(), 42);
        // Consume via C ABI.
        let first = hew_stream_last_errno();
        assert_eq!(first, 42);
        // Second read must return 0 — errno was cleared.
        assert_eq!(
            hew_stream_last_errno(),
            0,
            "errno must be cleared after first read"
        );
        // Clean up the message side.
        let ptr = hew_stream_last_error();
        if !ptr.is_null() {
            // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
            unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn set_last_error_plain_clears_errno() {
        // Pre-load an errno.
        set_last_error_with_errno("original".to_string(), 99);
        // Overwrite with plain set_last_error — errno must reset to 0.
        set_last_error("replacement".to_string());
        assert_eq!(
            take_last_errno(),
            0,
            "plain set_last_error must clear errno to 0"
        );
        // Clean up the message.
        let _ = take_last_error();
    }

    #[test]
    fn errno_and_message_are_independent_paths() {
        // Set with errno, consume errno first, message should still be present.
        set_last_error_with_errno("disk full".to_string(), 28);
        let errno = take_last_errno();
        assert_eq!(errno, 28);
        // Message should survive the errno read.
        let msg = take_last_error();
        assert_eq!(msg.as_deref(), Some("disk full"));
        // After take_last_error the errno path is also cleared.
        assert_eq!(take_last_errno(), 0);
    }

    // ── canonical error-kind tag (cross-platform classification) ─────────

    #[test]
    fn io_error_kind_tag_maps_portable_kinds() {
        use std::io::ErrorKind;
        // These portable kinds are what Rust's std maps the divergent raw OS
        // codes to (e.g. Windows ERROR_ALREADY_EXISTS=183 and POSIX EEXIST=17
        // both become ErrorKind::AlreadyExists), so tagging on the kind is
        // correct on every platform without seeing the raw integer.
        assert_eq!(
            io_error_kind_tag(ErrorKind::NotFound),
            IO_ERROR_KIND_NOT_FOUND
        );
        assert_eq!(
            io_error_kind_tag(ErrorKind::PermissionDenied),
            IO_ERROR_KIND_PERMISSION_DENIED
        );
        assert_eq!(
            io_error_kind_tag(ErrorKind::AlreadyExists),
            IO_ERROR_KIND_ALREADY_EXISTS
        );
        assert_eq!(
            io_error_kind_tag(ErrorKind::TimedOut),
            IO_ERROR_KIND_TIMED_OUT
        );
        // Kinds without a canonical IoError variant stay unclassified so the
        // consumer keeps the raw errno (e.g. EISDIR/ENOTDIR -> Other(raw)).
        assert_eq!(
            io_error_kind_tag(ErrorKind::WouldBlock),
            IO_ERROR_KIND_UNCLASSIFIED
        );
        assert_eq!(
            io_error_kind_tag(ErrorKind::Other),
            IO_ERROR_KIND_UNCLASSIFIED
        );
    }

    #[test]
    fn set_last_error_with_errno_and_kind_roundtrip() {
        let _ = take_last_error();
        set_last_error_with_errno_and_kind(
            "hew_fs_mkdir: File exists".to_string(),
            17,
            IO_ERROR_KIND_ALREADY_EXISTS,
        );
        // Read kind before errno (documented ordering).
        assert_eq!(hew_stream_last_error_kind(), IO_ERROR_KIND_ALREADY_EXISTS);
        assert_eq!(hew_stream_last_errno(), 17);
        // Kind is consumed after reading.
        assert_eq!(hew_stream_last_error_kind(), IO_ERROR_KIND_UNCLASSIFIED);
        let _ = take_last_error();
    }

    #[test]
    fn take_last_errno_clears_kind_metadata() {
        set_last_error_with_errno_and_kind("boom".to_string(), 13, IO_ERROR_KIND_PERMISSION_DENIED);
        // A consumer that reads only the errno must not leave a stale kind
        // parked for an unrelated later read.
        assert_eq!(take_last_errno(), 13);
        assert_eq!(take_last_error_kind(), IO_ERROR_KIND_UNCLASSIFIED);
        let _ = take_last_error();
    }

    #[test]
    fn plain_and_errno_setters_reset_kind() {
        // A stale AlreadyExists kind must not survive into a later error set
        // through a path that has no Rust error kind in scope.
        set_last_error_with_errno_and_kind("old".to_string(), 17, IO_ERROR_KIND_ALREADY_EXISTS);
        set_last_error_with_errno("new-errno-only".to_string(), 2);
        assert_eq!(take_last_error_kind(), IO_ERROR_KIND_UNCLASSIFIED);

        set_last_error_with_errno_and_kind("old2".to_string(), 17, IO_ERROR_KIND_ALREADY_EXISTS);
        set_last_error("new-message-only".to_string());
        assert_eq!(take_last_error_kind(), IO_ERROR_KIND_UNCLASSIFIED);
        let _ = take_last_error();
    }

    #[test]
    fn take_last_error_message_clears_kind() {
        set_last_error_with_errno_and_kind(
            "drain me".to_string(),
            17,
            IO_ERROR_KIND_ALREADY_EXISTS,
        );
        // Draining the message also drops the classification.
        let _ = take_last_error();
        assert_eq!(take_last_error_kind(), IO_ERROR_KIND_UNCLASSIFIED);
        assert_eq!(take_last_errno(), 0);
    }

    // ── free_cstring roundtrip: hew_stream_last_error allocator contract ──
    //
    // This test acts as the Hew drop spine: it calls hew_stream_last_error and
    // releases the result via free_cstring (exactly what hew_string_drop does at
    // runtime). It asserts no abort/leak, proving the allocator pairing survives
    // the move into hew-runtime (the allocator still comes from hew_cabi::cabi).

    /// Drop-spine simulation: `hew_stream_last_error` + `free_cstring` roundtrip.
    #[test]
    fn hew_stream_last_error_free_cstring_roundtrip() {
        set_last_error("connection reset by peer".to_string());
        let ptr = hew_stream_last_error();
        assert!(
            !ptr.is_null(),
            "error must be non-null after set_last_error"
        );
        // SAFETY: ptr was produced by hew_stream_last_error via alloc_cstring_from_str
        // (header-aware). Reading it as a C string and releasing via free_cstring
        // both satisfy the header-aware precondition.
        unsafe {
            let msg = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(msg, "connection reset by peer");
            // This is the exact call the Hew drop spine makes via hew_string_drop.
            hew_cabi::cabi::free_cstring(ptr); // CSTRING-FREE: str-open
        }
        // Error was consumed; a second call must return null.
        assert!(hew_stream_last_error().is_null());
    }
}
