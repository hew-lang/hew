use std::sync::Arc;

use super::{HewAsyncIo, IoValue};
use crate::stream::HewSink;
#[cfg(not(target_arch = "wasm32"))]
use crate::util::MutexExt;
use crate::wake::HewWaker;

#[cfg(not(target_arch = "wasm32"))]
pub(super) struct Finish {
    pending: Option<hew_cabi::sink::PendingFinish>,
    owner: Option<Arc<crate::stream::sender::SharedSender>>,
    waker: Arc<crate::wake::OwnedWaker>,
    child: *const HewAsyncIo,
    cancelled: bool,
}

// SAFETY: the outer operation serializes progress; retained source loans pin all backing storage.
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Send for Finish {}

#[cfg(not(target_arch = "wasm32"))]
impl Finish {
    pub(super) fn cancel(&mut self) {
        self.cancelled = true;
        if !self.child.is_null() {
            // SAFETY: this owner cancels its retained child before mandatory producer drain.
            unsafe { super::hew_async_io_cancel(self.child) };
        }
    }
    pub(super) fn poll(&mut self) -> Option<Result<IoValue, super::IoFailure>> {
        if self.child.is_null() {
            if self.owner.as_ref().is_some_and(|owner| !owner.idle()) {
                return None;
            }
            // SAFETY: final retirement prevents new admission, and readiness proved every prior producer quiescent.
            self.child = unsafe {
                if let Some(owner) = &self.owner {
                    hew_async_sink_finish(owner.underlying(), self.waker.descriptor())
                } else {
                    let mut pending = self.pending.take().unwrap_or_else(|| std::process::abort());
                    if let Some(connection) = pending.native_connection() {
                        let bytes = pending.take_pending();
                        super::net::start_tcp_sink_finish(
                            connection,
                            bytes,
                            pending,
                            self.waker.descriptor(),
                        )
                    } else {
                        super::file::start_sink_finish(pending, self.waker.descriptor())
                    }
                }
            };
            if self.cancelled {
                self.cancel();
            }
        }
        // SAFETY: this finish owns the child and retains its source loan through producer quiescence.
        if unsafe { super::hew_async_io_status(self.child) } == 0
            // SAFETY: logical readiness precedes mandatory producer cleanup.
            || unsafe { super::hew_async_io_cleanup_status(self.child, self.waker.descriptor()) } == 0
        {
            return None;
        }
        // SAFETY: the quiescent producer retains one completed, uniquely consumed result.
        let result = match std::mem::replace(
            &mut *unsafe { &*self.child }.state.lock_or_recover(),
            super::State::Taken,
        ) {
            super::State::Ready(result) => result,
            super::State::Cancelled => Err(super::IoFailure::from_io(
                "sink finish cancelled",
                &std::io::Error::from_raw_os_error(crate::stream_error::CANCELLED_ERRNO),
            )),
            _ => std::process::abort(),
        };
        // SAFETY: the child has completed and every producer released its borrowed storage.
        unsafe { super::hew_async_io_free(std::mem::replace(&mut self.child, std::ptr::null())) };
        if let Some(pending) = self.pending.take() {
            pending.finish();
        }
        Some(result)
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Drop for Finish {
    fn drop(&mut self) {
        if self.pending.is_some() || !self.child.is_null() {
            std::process::abort();
        }
    }
}

/// # Safety
/// The sink's read loan survives the request and producer quiescence.
#[no_mangle]
pub unsafe extern "C" fn hew_async_sink_finish(
    sink: *mut HewSink,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies a live sink loan or null.
    let Some(sink_ref) = (unsafe { sink.as_ref() }) else {
        // SAFETY: the borrowed waker is valid through submission.
        let operation = unsafe { HewAsyncIo::new(waker) };
        operation.complete(Ok(IoValue::Count(0)));
        return Arc::into_raw(operation);
    };
    // SAFETY: the borrowed waker is valid through submission.
    let operation = unsafe { HewAsyncIo::new(waker) };
    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: the caller's loan survives this operation's mandatory finish drain.
    if let Some(pending) = unsafe { sink_ref.begin_finish() } {
        if !pending.needs_close() {
            pending.finish();
            operation.complete(Ok(IoValue::Count(0)));
            return Arc::into_raw(operation);
        }
        // SAFETY: the pending backing pins its exact adapter allocation.
        let owner = (!pending.adapter().is_null())
            .then(|| unsafe { crate::stream::sender::retain_adapter(pending.adapter()) });
        // SAFETY: the borrowed descriptor is retained until native finish cleanup completes.
        let waker = Arc::new(unsafe { crate::wake::OwnedWaker::retain(&*waker) });
        if let Some(owner) = &owner {
            owner.observe(&waker);
        }
        *operation.sink_finish.lock_or_recover() = Some(Finish {
            pending: Some(pending),
            owner,
            waker,
            child: std::ptr::null(),
            cancelled: false,
        });
        return Arc::into_raw(operation);
    }
    #[cfg(target_arch = "wasm32")]
    sink_ref.finish();
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
