//! Errno discriminator tests for TCP FFI guard paths.

use hew_cabi::sink::hew_stream_last_errno;
use hew_runtime::bytes::BytesTriple;
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

    let invalid_utf8 = [0xFF_u8, 0xFE];
    #[allow(clippy::cast_possible_truncation, reason = "test slice is 2 bytes")]
    let triple = BytesTriple {
        ptr: invalid_utf8.as_ptr().cast_mut(),
        offset: 0,
        len: invalid_utf8.len() as u32,
    };

    // SAFETY: `invalid_utf8` lives for the duration of the call; `triple` is a
    // valid BytesTriple pointing into that slice.
    let status = unsafe { hew_tcp_broadcast_except(99_999, &raw const triple) };

    assert_eq!(status, -1);
    assert_eq!(hew_stream_last_errno(), 22);
}
