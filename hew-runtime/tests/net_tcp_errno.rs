//! Errno discriminator tests for TCP FFI guard paths.

use std::ffi::c_char;

use hew_cabi::sink::hew_stream_last_errno;
use hew_runtime::transport::{
    hew_tcp_broadcast_except, hew_tcp_set_read_timeout, hew_tcp_set_write_timeout,
};

fn clear_errno() {
    let _ = hew_stream_last_errno();
}

#[test]
fn tcp_set_read_timeout_invalid_handle_sets_ebadf() {
    clear_errno();

    let status = hew_tcp_set_read_timeout(99_999, 100);

    assert_eq!(status, -1);
    assert_eq!(hew_stream_last_errno(), 9);
}

#[test]
fn tcp_set_write_timeout_invalid_handle_sets_ebadf() {
    clear_errno();

    let status = hew_tcp_set_write_timeout(99_999, 100);

    assert_eq!(status, -1);
    assert_eq!(hew_stream_last_errno(), 9);
}

#[test]
fn tcp_broadcast_except_null_msg_sets_einval() {
    clear_errno();

    // SAFETY: This test intentionally exercises the null-pointer guard path.
    let status = unsafe { hew_tcp_broadcast_except(99_999, std::ptr::null()) };

    assert_eq!(status, -1);
    assert_eq!(hew_stream_last_errno(), 22);
}

#[test]
fn tcp_broadcast_except_bad_utf8_sets_einval() {
    clear_errno();

    let invalid_utf8 = [0xFF_u8, 0];

    // SAFETY: `invalid_utf8` is NUL-terminated and lives for the duration of the call.
    let status =
        unsafe { hew_tcp_broadcast_except(99_999, invalid_utf8.as_ptr().cast::<c_char>()) };

    assert_eq!(status, -1);
    assert_eq!(hew_stream_last_errno(), 22);
}
