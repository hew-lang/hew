//! Regression tests for generator and stream bugs.

use std::ffi::c_void;
use std::ptr;
use std::sync::mpsc;
use std::time::Duration;

use hew_runtime::generator::{
    hew_gen_ctx_create, hew_gen_free, hew_gen_next, hew_gen_yield, HewGenCtx,
};
use hew_runtime::stream::{hew_stream_lines, hew_stream_next, HewStream};

// ---------------------------------------------------------------------------
// Bug 1 regression: calling hew_gen_next after completion must not deadlock
// ---------------------------------------------------------------------------

extern "C" fn empty_body(_arg: *mut c_void, _ctx: *mut HewGenCtx) {
    // Body returns immediately without yielding anything.
}

#[test]
fn generator_post_completion_next_returns_null() {
    let (done_tx, done_rx) = mpsc::channel();

    std::thread::spawn(move || {
        unsafe {
            let ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);

            // First call: triggers the body and receives the done sentinel.
            let mut size: usize = 0;
            let val = hew_gen_next(ctx, &mut size as *mut usize);
            assert!(val.is_null(), "first next after empty body should be null");

            // Second call: must NOT deadlock — should return null immediately.
            let val2 = hew_gen_next(ctx, &mut size as *mut usize);
            assert!(val2.is_null(), "second next after done should be null");

            // Third call for good measure.
            let val3 = hew_gen_next(ctx, &mut size as *mut usize);
            assert!(val3.is_null(), "third next after done should be null");

            hew_gen_free(ctx);
        }
        let _ = done_tx.send(());
    });

    // Timeout: if hew_gen_next deadlocks, we'll catch it here.
    done_rx
        .recv_timeout(Duration::from_secs(2))
        .expect("generator_post_completion_next deadlocked (timed out after 2s)");
}

// ---------------------------------------------------------------------------
// Bug 2 regression: empty lines in LinesStream must not be treated as EOF
// ---------------------------------------------------------------------------

/// Helper: create a HewStream from a byte slice using hew_stream_from_bytes.
unsafe fn stream_from_bytes(data: &[u8]) -> *mut HewStream {
    unsafe { hew_runtime::stream::hew_stream_from_bytes(data.as_ptr(), data.len(), 0) }
}

#[test]
fn stream_lines_empty_line_preserved() {
    unsafe {
        let input = b"hello\n\nworld\n";
        let raw = stream_from_bytes(input);
        let lines = hew_stream_lines(raw);

        let mut items: Vec<String> = Vec::new();
        loop {
            let ptr = hew_stream_next(lines);
            if ptr.is_null() {
                break;
            }
            let cstr = std::ffi::CStr::from_ptr(ptr.cast());
            items.push(cstr.to_string_lossy().into_owned());
            libc::free(ptr);
        }

        assert_eq!(items.len(), 3, "expected 3 lines, got {items:?}");
        assert_eq!(items[0], "hello");
        assert_eq!(items[1], "", "second line should be empty");
        assert_eq!(items[2], "world");

        hew_runtime::stream::hew_stream_close(lines);
    }
}

// ---------------------------------------------------------------------------
// Bug 3 regression: yielding null must not terminate the generator
// ---------------------------------------------------------------------------

extern "C" fn null_then_value_body(_arg: *mut c_void, ctx: *mut HewGenCtx) {
    unsafe {
        // First yield: null pointer, size 0 — should NOT look like "done".
        hew_gen_yield(ctx, ptr::null_mut(), 0);

        // Second yield: a real value (the integer 99 as 4 bytes).
        let val: i32 = 99;
        hew_gen_yield(
            ctx,
            &val as *const i32 as *mut c_void,
            std::mem::size_of::<i32>(),
        );
    }
}

#[test]
fn generator_null_yield_does_not_terminate() {
    let (done_tx, done_rx) = mpsc::channel();

    std::thread::spawn(move || {
        unsafe {
            let ctx = hew_gen_ctx_create(null_then_value_body, ptr::null_mut(), 0);
            let mut count = 0;

            loop {
                let mut size: usize = usize::MAX;
                let val = hew_gen_next(ctx, &mut size as *mut usize);
                if val.is_null() {
                    // Done sentinel — generator finished.
                    break;
                }
                count += 1;
                libc::free(val);
            }

            hew_gen_free(ctx);
            let _ = done_tx.send(count);
        }
    });

    let count = done_rx
        .recv_timeout(Duration::from_secs(2))
        .expect("generator_null_yield test deadlocked");

    assert_eq!(
        count, 2,
        "expected 2 items from generator (null-yield + value), got {count}"
    );
}
