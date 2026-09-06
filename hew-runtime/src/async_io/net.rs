//! Actor-independent one-shot operations on the existing TCP reactor.

use std::sync::Arc;

use super::HewAsyncIo;
use crate::wake::HewWaker;

unsafe fn start(handle: i32, accept: bool, waker: *const HewWaker) -> *const HewAsyncIo {
    // SAFETY: the caller borrows a valid descriptor through this call.
    let operation = unsafe { HewAsyncIo::with_source(waker, true) };
    if let Err(error) =
        crate::reactor::reactor_await_async_io(handle, accept, Arc::clone(&operation))
    {
        operation.complete(Err(error));
    }
    Arc::into_raw(operation)
}

/// Start a one-shot nonblocking TCP read. EOF is successful empty bytes.
/// The connection remains borrowed until completion or cancellation detaches
/// the operation and `hew_async_io_cleanup_status` reports quiescence; its
/// original resource owner retains its close authority.
///
/// # Safety
/// `connection` is a live transport handle kept alive across the pending wait.
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_read(
    connection: i32,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies the borrowed handle and readiness descriptor.
    unsafe { start(connection, false, waker) }
}

/// Start a one-shot nonblocking accept. The operation owns the newly accepted
/// connection until `hew_async_io_take_handle` transfers it to the resume edge.
/// An untaken or late connection is closed when its owning result is discarded.
///
/// # Safety
/// `listener` is a live transport handle kept alive across the pending wait.
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_accept(
    listener: i32,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies the borrowed handle and readiness descriptor.
    unsafe { start(listener, true, waker) }
}
