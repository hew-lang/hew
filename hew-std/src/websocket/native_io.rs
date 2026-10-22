//! WebSocket accepts use the shared cancellable native I/O request lifecycle.

use hew_runtime::async_io::library::{submit_listener_job, take_library_result};
use hew_runtime::async_io::{AsyncIoStatus, HewAsyncIo};
use hew_runtime::wake::HewWaker;

use super::{accept_connection, HewWsAcceptResult, HewWsConn, HewWsServer};
use std::sync::Arc;
use tungstenite::protocol::Role;

/// Submit an accept without lending the source server to the blocking worker.
///
/// # Safety
/// `server` and `waker` are null or live borrowed handles during submission.
#[no_mangle]
pub unsafe extern "C" fn hew_async_ws_server_accept(
    server: *const HewWsServer,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends the handle until its shared owner is retained.
    let inner = unsafe { server.as_ref() }.map(|server| Arc::clone(&server.inner));
    // SAFETY: the caller lends the descriptor; the job retains its own server.
    unsafe {
        submit_listener_job(waker, move |cancellation| {
            let inner = inner?;
            let _accept_guard = inner.active_accepts.begin(&inner.cancel)?;
            match accept_connection(&inner, Some(cancellation)) {
                HewWsAcceptResult::Accepted(ws) => inner
                    .active_accepts
                    .publish_if_open(&inner.cancel, || HewWsConn::new(*ws, Role::Server, None)),
                HewWsAcceptResult::Cancelled | HewWsAcceptResult::Error => None,
            }
        })
    }
}

/// Transfer an accepted connection; undelivered connections close on request drop.
///
/// # Safety
/// `operation` is a live accept request and `out` is writable pointer storage.
#[no_mangle]
pub unsafe extern "C" fn hew_async_ws_take_accept(
    operation: *const HewAsyncIo,
    out: *mut *mut HewWsConn,
) -> i32 {
    // SAFETY: the caller owns the matching request and its output storage.
    unsafe {
        if let Some(connection) = take_library_result::<Option<HewWsConn>>(operation) {
            *out = connection.map_or(std::ptr::null_mut(), |conn| Box::into_raw(Box::new(conn)));
            1
        } else if hew_runtime::async_io::hew_async_io_status(operation)
            == AsyncIoStatus::Error as i32
        {
            *out = std::ptr::null_mut();
            1
        } else {
            0
        }
    }
}
