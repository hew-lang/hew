//! Native FFI backing for the `hew::testffi` fixture package.
//!
//! Mirrors the ecosystem `hew::db::sqlite` ABI shape with zero dependencies:
//! result sets are heap handles carried as `i64` inside a `#[repr(C)]`
//! single-field struct (matching Hew's `type Result { handle: i64; }`),
//! strings cross the boundary as owned managed Hew string handles. The
//! linked runtime creates their storage and generated callers release it.
//! Handles must be released exactly once via
//! [`hew_testffi_result_free`].

extern "C" {
    fn hew_string_literal_new(data: *const u8, len: u32, out: *mut *mut u8);
}

/// Copy `s` into an owned managed string using the linked Hew runtime.
fn managed_string(s: &str) -> *mut u8 {
    let mut handle = std::ptr::null_mut();
    let len = u32::try_from(s.len()).expect("fixture string fits the runtime ABI");
    // SAFETY: `s` is readable UTF-8 and `handle` is unique output storage.
    unsafe { hew_string_literal_new(s.as_ptr(), len, &mut handle) };
    handle
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

/// Ask reply: an owned managed string naming the seed.
#[no_mangle]
pub extern "C" fn hew_testffi_name(seed: i64) -> *mut u8 {
    managed_string(&format!("testffi-{seed}"))
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

/// Cell value at `row` as an owned managed string; empty out of range.
#[no_mangle]
pub extern "C" fn hew_testffi_result_get(result: TestResultHandle, row: i32) -> *mut u8 {
    // SAFETY: the handle was produced by `hew_testffi_query` (or is 0).
    let text = match unsafe { result.as_ref() } {
        Some(r) if row >= 0 && row < r.rows => format!("row{row}:{}", r.seed),
        _ => String::new(),
    };
    managed_string(&text)
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
