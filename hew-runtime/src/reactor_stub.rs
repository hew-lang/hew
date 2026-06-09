//! Fail-closed active-mode reactor stub for non-Unix native targets.
//!
//! The real reactor (`reactor.rs`) drives epoll/kqueue over raw socket file
//! descriptors on a dedicated OS thread. That design depends on Unix `RawFd`
//! semantics and the platform poller in [`crate::io_time`], neither of which has
//! a Windows IOCP/readiness backend yet. Rather than silently linking a
//! half-working reactor, this stub keeps the cross-module entry points that
//! `transport`, `actor`, and `scheduler` call unconditionally and reports active
//! mode as unavailable:
//!
//! - [`reactor_attach`] fails closed (sets the last-error string, returns -1).
//! - [`reactor_await_read`] fails closed (sets the last-error string, returns
//!   -1) and does not retain the caller's read slot.
//! - [`reactor_detach_conn`] / [`reactor_detach_read_slot`] /
//!   [`reactor_detach_actor`] / [`reactor_shutdown`] are no-ops because nothing
//!   can be attached.
//!
//! Passive (blocking) TCP I/O via the blocking pool is unaffected; only the
//! "I/O completion as a mailbox message" active mode is gated off here. This
//! stub should be retired when the W-6 IOCP backend lands.

use std::ffi::c_int;

use crate::transport::HewActorRef;

/// Attach a TCP connection to the active-mode reactor.
///
/// Fail-closed on this target: there is no readiness poller, so active mode is
/// unavailable. Returns -1 after recording an error message.
///
/// # Safety
///
/// Matches the real `reactor::reactor_attach` ABI: `actor_ref` may be any
/// pointer (it is never dereferenced here). No preconditions are imposed
/// because the function does not touch the pointee.
pub(crate) unsafe fn reactor_attach(
    _conn: c_int,
    _actor_ref: *const HewActorRef,
    _on_data_type: i32,
    _on_close_type: i32,
) -> c_int {
    crate::set_last_error(
        "hew_tcp_attach: active-mode I/O reactor is not available on this platform",
    );
    -1
}

/// Register an `await conn.read()` with the active-mode reactor.
///
/// Fail-closed on this target: there is no readiness poller, so await-read
/// suspension over TCP cannot be resumed by a reactor. Returns -1 after
/// recording an error message and leaves `read_slot` ownership unchanged.
///
/// # Safety
///
/// Matches the real `reactor::reactor_await_read` ABI. Pointers are never
/// dereferenced and the read slot is not retained.
pub(crate) unsafe fn reactor_await_read(
    _conn: c_int,
    _actor_ref: *const HewActorRef,
    _read_slot: *mut crate::read_slot::HewReadSlot,
) -> c_int {
    crate::set_last_error(
        "hew_conn_await_read: active-mode I/O reactor is not available on this platform",
    );
    -1
}

/// Detach a connection from the reactor. No-op: nothing is ever attached.
pub(crate) fn reactor_detach_conn(_conn: c_int) {}

/// Remove a one-shot read-slot registration. No-op: nothing is ever attached.
pub(crate) fn reactor_detach_read_slot(_read_slot: *mut crate::read_slot::HewReadSlot) -> bool {
    false
}

/// Remove every registration owned by an actor. No-op: nothing is ever attached.
pub(crate) fn reactor_detach_actor(_actor_key: usize) {}

/// Stop the reactor thread. No-op: the reactor thread is never started.
pub(crate) fn reactor_shutdown() {}
