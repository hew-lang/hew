//! Actor-independent one-shot operations on the existing TCP reactor.

use std::sync::Arc;

use super::{HewAsyncIo, IoFailure, IoValue};
use crate::reactor::AsyncIoAction;
use crate::wake::HewWaker;

unsafe fn start(handle: i32, action: AsyncIoAction, waker: *const HewWaker) -> *const HewAsyncIo {
    // SAFETY: the caller borrows a valid descriptor through this call.
    let operation = unsafe { HewAsyncIo::with_source(waker, true) };
    if let Err(error) =
        crate::reactor::reactor_await_async_io(handle, action, Arc::clone(&operation))
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
    unsafe { start(connection, AsyncIoAction::Read { deadline: None }, waker) }
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
    unsafe { start(listener, AsyncIoAction::Accept, waker) }
}

/// Submit a complete byte write, retaining partial progress across readiness.
/// Data is copied immediately. The connection loan lasts until producer
/// quiescence; cancellation can leave a prefix committed to the peer.
///
/// # Safety
/// `connection` is live until quiescence. `data` is null or a valid borrowed
/// bytes carrier with a readable region. `waker` is null or a valid descriptor.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_write(
    connection: i32,
    data: *const crate::bytes::BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends the carrier and its region through submission.
    let bytes = unsafe { data.as_ref() }
        .ok_or_else(|| IoFailure::invalid("TCP write carrier is null"))
        .and_then(|data| {
            if data.len > i32::MAX as u32 || (data.len != 0 && data.ptr.is_null()) {
                return Err(IoFailure::invalid(
                    "TCP write region is invalid or its count exceeds i32",
                ));
            }
            if data.len == 0 {
                return Ok(Vec::new());
            }
            // SAFETY: the source contract supplies a readable active region.
            Ok(unsafe {
                std::slice::from_raw_parts(data.ptr.add(data.offset as usize), data.len as usize)
            }
            .to_vec())
        });
    match bytes {
        Ok(bytes) if !bytes.is_empty() => {
            // SAFETY: the resource loan and waker remain valid for this request.
            unsafe { start(connection, AsyncIoAction::write(bytes), waker) }
        }
        result => {
            // SAFETY: the borrowed waker obeys the ordinary submission contract.
            let operation = unsafe { HewAsyncIo::new(waker) };
            operation.complete(result.map(|_| IoValue::Count(0)));
            Arc::into_raw(operation)
        }
    }
}
