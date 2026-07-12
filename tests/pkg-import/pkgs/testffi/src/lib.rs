//! Native FFI backing for the `hew::testffi` fixture package.
//!
//! Mirrors the ecosystem `hew::db::sqlite` ABI shape with zero dependencies:
//! result sets are heap handles carried as `i64` inside a `#[repr(C)]`
//! single-field struct (matching Hew's `type Result { handle: i64; }`),
//! strings cross the boundary as C strings — `*const c_char` in, malloc'd
//! `*mut c_char` out. Generated callers adopt returned strings into Hew's
//! refcounted string domain and release the foreign allocation with `free`.
//! Handles must be released exactly once via
//! [`hew_testffi_result_free`].

use std::os::raw::{c_char, c_void};

extern "C" {
    fn malloc(size: usize) -> *mut c_void;
}

/// Copy `s` into a fresh malloc'd, NUL-terminated C string.
fn str_to_malloc(s: &str) -> *mut c_char {
    let bytes = s.as_bytes();
    // SAFETY: malloc'd block is len + 1 bytes; both copies stay in bounds.
    unsafe {
        let buf = malloc(bytes.len() + 1).cast::<u8>();
        if buf.is_null() {
            return std::ptr::null_mut();
        }
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf, bytes.len());
        *buf.add(bytes.len()) = 0;
        buf.cast::<c_char>()
    }
}

/// A fixture result set: `rows` synthetic rows derived from `seed`.
struct TestResult {
    seed: i64,
    rows: i32,
}

/// FFI handle matching Hew's `type Result { handle: i64; }` layout.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TestResultHandle {
    handle: i64,
}

impl TestResultHandle {
    fn from_box(result: TestResult) -> Self {
        Self {
            handle: Box::into_raw(Box::new(result)) as i64,
        }
    }

    // SAFETY: `handle` must be a pointer produced by `from_box` (or 0).
    unsafe fn as_ref<'a>(self) -> Option<&'a TestResult> {
        let ptr = self.handle as *const TestResult;
        // SAFETY: per the type contract the pointer is live or null.
        unsafe { ptr.as_ref() }
    }
}

/// Ask reply: a 32-bit count derived from the actor seed.
#[no_mangle]
pub extern "C" fn hew_testffi_count32(seed: i64) -> i32 {
    (seed as i32) * 2
}

/// Ask reply: a 64-bit count derived from the actor seed.
#[no_mangle]
pub extern "C" fn hew_testffi_count64(seed: i64) -> i64 {
    seed * 1_000_000_007
}

/// Ask reply: a malloc'd C string naming the seed.
#[no_mangle]
pub extern "C" fn hew_testffi_name(seed: i64) -> *mut c_char {
    str_to_malloc(&format!("testffi-{seed}"))
}

/// Ask reply: a heap-allocated result set with `rows` rows.
#[no_mangle]
pub extern "C" fn hew_testffi_query(seed: i64, rows: i32) -> TestResultHandle {
    if rows < 0 {
        return TestResultHandle { handle: 0 };
    }
    TestResultHandle::from_box(TestResult { seed, rows })
}

/// Row count of a result set; -1 for a null handle.
#[no_mangle]
pub extern "C" fn hew_testffi_result_rows(result: TestResultHandle) -> i32 {
    // SAFETY: the handle was produced by `hew_testffi_query` (or is 0).
    unsafe { result.as_ref() }.map_or(-1, |r| r.rows)
}

/// Cell value at `row` as a malloc'd C string; empty string out of range.
#[no_mangle]
pub extern "C" fn hew_testffi_result_get(result: TestResultHandle, row: i32) -> *mut c_char {
    // SAFETY: the handle was produced by `hew_testffi_query` (or is 0).
    let text = match unsafe { result.as_ref() } {
        Some(r) if row >= 0 && row < r.rows => format!("row{row}:{}", r.seed),
        _ => String::new(),
    };
    str_to_malloc(&text)
}

/// Sum-style aggregate over the result set; -1 for a null handle.
#[no_mangle]
pub extern "C" fn hew_testffi_result_total(result: TestResultHandle) -> i64 {
    // SAFETY: the handle was produced by `hew_testffi_query` (or is 0).
    unsafe { result.as_ref() }.map_or(-1, |r| r.seed + i64::from(r.rows))
}

/// Release a result set. A zero handle is a no-op; releasing twice is
/// undefined exactly as for the real packages (single-release contract).
#[no_mangle]
pub extern "C" fn hew_testffi_result_free(result: TestResultHandle) {
    if result.handle == 0 {
        return;
    }
    // SAFETY: the handle was produced by `hew_testffi_query` and is released
    // exactly once per the package contract.
    unsafe {
        drop(Box::from_raw(result.handle as *mut TestResult));
    }
}
