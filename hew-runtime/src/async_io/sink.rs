use std::sync::Arc;

use super::{HewAsyncIo, IoValue};
use crate::stream::HewSink;
use crate::wake::HewWaker;

/// # Safety
/// The sink's exclusive loan survives the request and producer quiescence.
#[no_mangle]
pub unsafe extern "C" fn hew_async_sink_finish(
    sink: *mut HewSink,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies a live sink loan or null.
    let Some(sink_ref) = (unsafe { sink.as_mut() }) else {
        // SAFETY: the borrowed waker is valid through submission.
        let operation = unsafe { HewAsyncIo::new(waker) };
        operation.complete(Ok(IoValue::Count(0)));
        return Arc::into_raw(operation);
    };
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(connection) = sink_ref.native_connection() {
        let bytes = sink_ref.take_pending();
        // SAFETY: the caller retains the sink loan through request quiescence.
        return unsafe { super::net::start_tcp_sink_finish(connection, bytes, sink, waker) };
    }
    if !sink_ref.is_closed() && sink_ref.channel_core_ptr().is_null() {
        #[cfg(not(target_arch = "wasm32"))]
        // SAFETY: the producer retains the caller's exclusive heap loan.
        return unsafe { super::file::start_sink_finish(sink, waker) };
        #[cfg(target_arch = "wasm32")]
        panic!("content sink finish is unavailable on wasm32");
    }
    sink_ref.close();
    // SAFETY: the borrowed waker is valid through submission.
    let operation = unsafe { HewAsyncIo::new(waker) };
    operation.complete(Ok(IoValue::Count(0)));
    Arc::into_raw(operation)
}

/// # Safety
/// The cleanup invocation retains its sink loan and this live request.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_finish_cleanup_poll(
    operation: *const HewAsyncIo,
    state: *const crate::coro_state::HewCoroState,
) -> i32 {
    // SAFETY: both handles remain live through this cleanup poll.
    unsafe {
        if super::hew_async_io_status(operation) == 0 {
            return 0;
        }
        let waker = crate::coro_state::hew_coro_state_waker(state);
        super::hew_async_io_cleanup_status(operation, waker)
    }
}

/// # Safety
/// The request is complete and quiescent, and remains live through this call.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_finish_cleanup_fault(
    operation: *const HewAsyncIo,
) -> *mut crate::fault::HewFault {
    // SAFETY: the caller retains a complete request.
    if unsafe { super::hew_async_io_status(operation) } == 1 {
        return std::ptr::null_mut();
    }
    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: the caller retains the failed request and its diagnostic.
    let message =
        unsafe { super::failure_message(operation) }.unwrap_or_else(|| "sink finish failed".into());
    #[cfg(target_arch = "wasm32")]
    let message = "sink finish failed".into();
    Box::into_raw(Box::new(crate::fault::HewFault::with_message(
        crate::internal::types::HEW_TRAP_USER_PANIC,
        message,
    )))
}
