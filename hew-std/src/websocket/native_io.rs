//! WebSocket accepts and sends use the shared cancellable native I/O lifecycle.

use hew_runtime::async_io::library::{submit_listener_job, submit_socket_job, take_library_result};
use hew_runtime::async_io::{AsyncIoStatus, HewAsyncIo};
use hew_runtime::wake::HewWaker;

use super::{
    accept_connection, clear_ws_last_error, send_shared, set_ws_last_error, tls, BytesTriple,
    HewString, HewWsAcceptResult, HewWsConn, HewWsServer, SendCompletion, WS_INVALID_ARGUMENT,
};
use hew_cabi::string::string_as_str;
use std::sync::Arc;
use tungstenite::protocol::Role;
use tungstenite::Message;

/// Submit a text send with request-owned connection and message storage.
///
/// # Safety
/// `connection`, `text` and `waker` are null or live borrowed values for submit.
#[no_mangle]
pub unsafe extern "C" fn hew_async_ws_send_text(
    connection: *const HewWsConn,
    text: *const HewString,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: inputs are borrowed for submission and copied into the request.
    let (inner, text) = unsafe {
        (
            connection.as_ref().map(|conn| Arc::clone(&conn.inner)),
            string_as_str(text).to_owned(),
        )
    };
    // SAFETY: the descriptor is borrowed for submit; the closure owns its inputs.
    unsafe {
        submit_socket_job(waker, move |cancellation| {
            let inner = inner.ok_or_else(|| {
                (
                    WS_INVALID_ARGUMENT,
                    "websocket.send_text: null connection".to_owned(),
                )
            })?;
            send_shared(
                &inner,
                Message::text(text),
                "websocket.send_text",
                Some(cancellation),
            )
        })
    }
}

/// Submit a binary send without lending message storage to a blocking worker.
///
/// # Safety
/// `connection`, `data` and `waker` are null or live borrowed values for submit.
#[no_mangle]
pub unsafe extern "C" fn hew_async_ws_send_binary(
    connection: *const HewWsConn,
    data: *const BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: inputs are borrowed for submission and copied into the request.
    let (inner, data) = unsafe {
        (
            connection.as_ref().map(|conn| Arc::clone(&conn.inner)),
            tls::bytes_view(data).to_vec(),
        )
    };
    // SAFETY: the descriptor is borrowed for submit; the closure owns its inputs.
    unsafe {
        submit_socket_job(waker, move |cancellation| {
            let inner = inner.ok_or_else(|| {
                (
                    WS_INVALID_ARGUMENT,
                    "websocket.send_binary: null connection".to_owned(),
                )
            })?;
            send_shared(
                &inner,
                Message::binary(data),
                "websocket.send_binary",
                Some(cancellation),
            )
        })
    }
}

/// Restore the send's ordinary result/error on its caller's resume thread.
///
/// # Safety
/// `operation` is a live send request and `out` is writable i32 result storage.
#[no_mangle]
pub unsafe extern "C" fn hew_async_ws_take_send(
    operation: *const HewAsyncIo,
    out: *mut i32,
) -> i32 {
    // SAFETY: the caller owns the matching request and result storage.
    unsafe {
        if let Some(completion) = take_library_result::<SendCompletion>(operation) {
            match completion {
                Ok(()) => {
                    clear_ws_last_error();
                    *out = 0;
                }
                Err((errno, detail)) => {
                    set_ws_last_error(errno, detail);
                    *out = -1;
                }
            }
            1
        } else if hew_runtime::async_io::hew_async_io_restore_error(operation)
            == AsyncIoStatus::Error as i32
        {
            set_ws_last_error(
                0,
                hew_runtime::stream_error::take_last_error()
                    .unwrap_or_else(|| "WebSocket send failed".to_owned()),
            );
            *out = -1;
            1
        } else {
            0
        }
    }
}

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
