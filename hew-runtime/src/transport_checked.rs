//! Native compiler adapters from borrowed managed strings to the TCP C ABI.
//!
//! These calls retain the existing transport's synchronous behaviour. Connect
//! requires an installed runtime for DNS offload; the caller still waits on a
//! condition variable, then performs the TCP handshake synchronously. Neither
//! that wait nor the handshake polls native coroutine cancellation. The public
//! transport's `write_all` is also synchronous. These adapters do not provide
//! cooperative I/O or make one-worker coroutine execution non-blocking.

use hew_cabi::string::{string_to_cstring, HewString};
use std::ffi::c_char;

use crate::{stream_error, transport};

fn invalid_input(operation: &str, reason: &str) -> i32 {
    stream_error::set_last_error_with_errno(format!("{operation}: {reason}"), 22); // EINVAL
    -1
}

/// Borrow a managed value for one transport call, releasing its foreign copy
/// when the call returns. Keep transport diagnostics, or publish an unknown
/// error when an existing transport failure supplies no diagnostic.
///
/// # Safety
///
/// `address` must be null (empty) or a live managed string for this call.
unsafe fn with_c_address(
    address: *const HewString,
    operation: &str,
    call: impl FnOnce(*const c_char) -> i32,
) -> i32 {
    let _ = stream_error::take_last_error();
    // SAFETY: the caller keeps the managed owner alive throughout the copy.
    let Ok(address) = (unsafe { string_to_cstring(address) }) else {
        return invalid_input(operation, "address contains an embedded NUL");
    };
    let result = call(address.as_ptr());
    if result < 0 && !stream_error::hew_stream_has_error() {
        // Some legacy transport failures have no OS error available. Do not
        // invent an errno or expose a previous operation's thread-local error.
        stream_error::set_last_error(format!("{operation}: TCP operation failed"));
    }
    result
}

/// Listen on a managed `host:port` address. Returns a positive listener or -1.
/// Embedded NUL is rejected with EINVAL in the stream error channel.
///
/// # Safety
///
/// `address` must be null (empty) or a live borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_tcp_listen(address: *const HewString) -> i32 {
    // SAFETY: the managed borrow is valid and the temporary C string remains
    // alive until the synchronous transport call returns.
    unsafe {
        with_c_address(address, "hew_checked_tcp_listen", |address| {
            transport::hew_tcp_listen(address)
        })
    }
}

/// Connect to a managed `host:port` address. Returns a positive connection or -1.
/// Requires an installed runtime; this call has no deadline and can block.
///
/// # Safety
///
/// `address` must be null (empty) or a live borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_tcp_connect(address: *const HewString) -> i32 {
    // SAFETY: the managed borrow is valid and the foreign copy outlives the call.
    unsafe {
        with_c_address(address, "hew_checked_tcp_connect", |address| {
            transport::hew_tcp_connect(address)
        })
    }
}

/// Connect to a managed host and port with a millisecond timeout. Returns a
/// positive connection or -1. Invalid ports, negative timeouts and embedded NUL
/// report EINVAL. A positive timeout bounds the DNS wait and TCP handshake;
/// it does not cancel a DNS lookup already running on the blocking pool. Zero
/// retains the existing transport behaviour: DNS has no deadline and the
/// connection budget is exhausted once resolution returns.
///
/// # Safety
///
/// `host` must be null (empty) or a live borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_tcp_connect_timeout(
    host: *const HewString,
    port: i32,
    timeout_ms: i32,
) -> i32 {
    if u16::try_from(port).is_err() || timeout_ms < 0 {
        return invalid_input(
            "hew_checked_tcp_connect_timeout",
            "port must be in 0..=65535 and timeout must be non-negative",
        );
    }
    // SAFETY: the managed borrow is valid and the foreign copy outlives the call.
    unsafe {
        with_c_address(host, "hew_checked_tcp_connect_timeout", |host| {
            transport::hew_tcp_connect_timeout(host, port, timeout_ms)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::string::{string_as_str, string_from_str, string_release};

    fn managed_call(value: &str, call: impl FnOnce(*const HewString) -> i32) -> i32 {
        let managed = string_from_str(value);
        let result = call(managed);
        // SAFETY: adapters borrow the owner; it remains readable and is released
        // once by the caller after the transport has finished using its copy.
        unsafe {
            assert_eq!(string_as_str(managed), value);
            string_release(managed);
        }
        result
    }

    #[test]
    fn managed_addresses_connect_and_release_loopback_handles() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: managed_call holds each managed owner through the FFI call.
        let listener = managed_call("127.0.0.1:0", |address| unsafe {
            hew_checked_tcp_listen(address)
        });
        assert!(listener > 0);
        let port = transport::hew_tcp_listener_local_port(listener);
        assert!(port > 0);
        stream_error::set_last_error_with_errno("stale".into(), 9);
        // SAFETY: managed_call holds the address through the FFI call.
        let first = managed_call(&format!("127.0.0.1:{port}"), |address| unsafe {
            hew_checked_tcp_connect(address)
        });
        assert!(first > 0);
        assert!(!stream_error::hew_stream_has_error());
        let first_peer = transport::hew_tcp_accept(listener);
        assert!(first_peer > 0);
        // SAFETY: managed_call holds the host through the FFI call.
        let second = managed_call("127.0.0.1", |host| unsafe {
            hew_checked_tcp_connect_timeout(host, port, 2000)
        });
        assert!(second > 0);
        let second_peer = transport::hew_tcp_accept(listener);
        assert!(second_peer > 0);
        for connection in [first, first_peer, second, second_peer] {
            assert_eq!(transport::hew_tcp_close(connection), 0);
            assert_eq!(transport::hew_tcp_close(connection), -1);
        }
        assert_eq!(transport::hew_tcp_listener_close(listener), 0);
        assert_eq!(transport::hew_tcp_listener_close(listener), -1);
        stream_error::set_last_error_with_errno("stale".into(), 9);
        // SAFETY: managed_call keeps the host live. The listener has closed,
        // so this also covers a transport failure with no supplied diagnostic.
        let failed = managed_call("127.0.0.1", |host| unsafe {
            hew_checked_tcp_connect_timeout(host, port, 2000)
        });
        assert_eq!(failed, -1);
        assert_ne!(hew_cabi::sink::take_last_errno(), 9);
        assert!(!hew_cabi::sink::take_last_error().unwrap().contains("stale"));
    }

    #[test]
    fn embedded_nul_rejects_all_adapters_without_truncation() {
        for call in [
            hew_checked_tcp_listen as unsafe extern "C" fn(*const HewString) -> i32,
            hew_checked_tcp_connect,
        ] {
            // SAFETY: the managed string includes its full embedded-NUL length.
            let result = managed_call("127.0.0.1:0\0ignored", |address| unsafe { call(address) });
            assert_eq!(result, -1);
            assert_eq!(hew_cabi::sink::take_last_errno(), 22);
            assert!(hew_cabi::sink::take_last_error().unwrap().contains("NUL"));
        }
        // SAFETY: the managed string includes its full embedded-NUL length.
        let result = managed_call("127.0.0.1\0ignored", |host| unsafe {
            hew_checked_tcp_connect_timeout(host, 80, 100)
        });
        assert_eq!(result, -1);
        assert_eq!(hew_cabi::sink::take_last_errno(), 22);
        assert!(hew_cabi::sink::take_last_error().unwrap().contains("NUL"));
    }

    #[test]
    fn invalid_inputs_publish_fresh_thread_local_errors() {
        let _guard = crate::runtime_test_guard();
        for call in [
            hew_checked_tcp_listen as unsafe extern "C" fn(*const HewString) -> i32,
            hew_checked_tcp_connect,
        ] {
            stream_error::set_last_error_with_errno("stale".into(), 9);
            // SAFETY: the invalid port is still a valid UTF-8 managed string.
            let result = managed_call("127.0.0.1:é", |address| unsafe { call(address) });
            assert_eq!(result, -1);
            let error = hew_cabi::sink::take_last_error().unwrap();
            assert!(!error.contains("stale"));
        }
        for (port, timeout) in [(-1, 100), (65536, 100), (80, -1)] {
            // SAFETY: managed_call keeps the owner live even for invalid scalars.
            let result = managed_call("127.0.0.1", |host| unsafe {
                hew_checked_tcp_connect_timeout(host, port, timeout)
            });
            assert_eq!(result, -1);
            assert_eq!(hew_cabi::sink::take_last_errno(), 22);
            assert!(hew_cabi::sink::take_last_error().is_some());
        }
        // SAFETY: null is the canonical empty managed string.
        assert_eq!(unsafe { hew_checked_tcp_listen(std::ptr::null()) }, -1);
        assert!(hew_cabi::sink::take_last_error().is_some());

        stream_error::set_last_error_with_errno("caller error".into(), 9);
        std::thread::spawn(|| {
            assert!(!stream_error::hew_stream_has_error());
            // SAFETY: managed_call keeps the embedded-NUL string live.
            let result = managed_call("bad\0host", |host| unsafe { hew_checked_tcp_connect(host) });
            assert_eq!(result, -1);
            assert_eq!(hew_cabi::sink::take_last_errno(), 22);
        })
        .join()
        .unwrap();
        assert_eq!(hew_cabi::sink::take_last_errno(), 9);
        assert_eq!(
            hew_cabi::sink::take_last_error().as_deref(),
            Some("caller error")
        );
    }

    #[test]
    fn connect_without_runtime_returns_the_transport_error() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(crate::runtime::rt_default().is_none());
        for timed in [false, true] {
            let host = if timed { "127.0.0.1" } else { "127.0.0.1:1" };
            // SAFETY: managed_call keeps each address or host owner live.
            let result = managed_call(host, |host| unsafe {
                if timed {
                    hew_checked_tcp_connect_timeout(host, 1, 100)
                } else {
                    hew_checked_tcp_connect(host)
                }
            });
            assert_eq!(result, -1);
            assert_eq!(hew_cabi::sink::take_last_errno(), 22);
            assert!(hew_cabi::sink::take_last_error()
                .unwrap()
                .contains("no runtime"));
        }
    }
}
