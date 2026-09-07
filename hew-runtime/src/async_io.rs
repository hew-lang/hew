//! Owned native I/O operations for suspendable Hew code.
//!
//! Producers own their inputs and one `Arc` until they finish. A coroutine owns
//! the returned reference and takes a result only on its resume edge. No worker
//! retains a pointer into the coroutine frame. Completion and cancellation use
//! one locked state transition; a losing producer drops its result normally.
//! Wakers are retained heap targets and run only after the state lock is released.

use std::io;
use std::ptr;
use std::sync::{Arc, Mutex};

use hew_cabi::string::{string_from_str, HewString};

use crate::bytes::BytesTriple;
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};

mod connect;
mod connect_deadline;
mod file;
mod net;
pub use connect::{hew_async_tcp_connect, hew_async_tcp_connect_timeout};
pub use file::{hew_async_file_read, hew_async_file_write, hew_async_file_write_string};
pub use net::{hew_async_tcp_accept, hew_async_tcp_read, hew_async_tcp_write};

#[cfg(test)]
mod tests;

/// Compiler-private operation states. A successful take returns `Success` and
/// changes the stored state to `Taken`; every other take leaves its out-pointer
/// untouched. The error state is read through the operation's error accessors.
#[repr(i32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AsyncIoStatus {
    Pending = 0,
    Success = 1,
    Error = 2,
    Cancelled = 3,
    Taken = 4,
}

/// Error metadata travels with the operation, never through worker-local errno
/// or the synchronous stream error channel.
pub(crate) struct IoFailure {
    kind: i32,
    errno: i32,
    message: String,
}

impl IoFailure {
    pub(crate) fn from_io(operation: &str, error: &io::Error) -> Self {
        Self {
            kind: crate::stream_error::io_error_kind_tag(error.kind()),
            // Existing source wrappers distinguish failure from EOF/success
            // using nonzero errno, including portable errors without an OS code.
            errno: error.raw_os_error().unwrap_or(libc::EIO),
            message: format!("{operation}: {error}"),
        }
    }

    pub(crate) fn invalid(message: &str) -> Self {
        Self {
            kind: crate::stream_error::IO_ERROR_KIND_UNCLASSIFIED,
            errno: libc::EINVAL,
            message: message.into(),
        }
    }
}

/// An accepted socket remains owned by its operation until a successful take.
/// Cancellation after readiness and completion after cancellation both drop it.
pub(crate) struct AcceptedConnection(pub(crate) i32);

impl Drop for AcceptedConnection {
    fn drop(&mut self) {
        if self.0 >= 0 {
            crate::transport::tcp_close_orphan_conn(self.0);
        }
    }
}

pub(crate) enum IoValue {
    Bytes(Vec<u8>),
    Count(i64),
    Connection(AcceptedConnection),
}

enum State {
    Pending(Option<OwnedWaker>),
    Ready(Result<IoValue, IoFailure>),
    Cancelled,
    Taken,
}

impl State {
    fn status(&self) -> AsyncIoStatus {
        match self {
            Self::Pending(_) => AsyncIoStatus::Pending,
            Self::Ready(Ok(_)) => AsyncIoStatus::Success,
            Self::Ready(Err(_)) => AsyncIoStatus::Error,
            Self::Cancelled => AsyncIoStatus::Cancelled,
            Self::Taken => AsyncIoStatus::Taken,
        }
    }
}

/// Opaque resource held across a suspended I/O call.
pub struct HewAsyncIo {
    state: Mutex<State>,
    reactor: bool,
    cleanup: Mutex<Cleanup>,
    connect_deadline: Mutex<Option<connect_deadline::ConnectDeadline>>,
}

#[derive(Default)]
struct Cleanup {
    producers: usize,
    waker: Option<OwnedWaker>,
}

/// A producer lease covers queued work and every in-flight readiness snapshot.
/// Its last release proves no producer can still touch borrowed resources.
pub(crate) struct IoProducer(Arc<HewAsyncIo>);

impl IoProducer {
    pub(crate) fn new(operation: Arc<HewAsyncIo>) -> Self {
        operation.cleanup.lock_or_recover().producers += 1;
        Self(operation)
    }
}

impl Clone for IoProducer {
    fn clone(&self) -> Self {
        Self::new(Arc::clone(&self.0))
    }
}

impl std::ops::Deref for IoProducer {
    type Target = HewAsyncIo;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for IoProducer {
    fn drop(&mut self) {
        let waker = {
            let mut cleanup = self.cleanup.lock_or_recover();
            cleanup.producers -= 1;
            if cleanup.producers == 0 {
                cleanup.waker.take()
            } else {
                None
            }
        };
        if let Some(waker) = waker {
            waker.wake();
        }
    }
}

impl std::fmt::Debug for HewAsyncIo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewAsyncIo")
            .field("status", &self.state.lock_or_recover().status())
            .finish_non_exhaustive()
    }
}

impl HewAsyncIo {
    /// # Safety
    /// A non-null descriptor obeys the shared `HewWaker` lifetime contract.
    unsafe fn new(waker: *const HewWaker) -> Arc<Self> {
        // SAFETY: the caller lends the descriptor through construction.
        unsafe { Self::with_source(waker, false) }
    }

    unsafe fn with_source(waker: *const HewWaker, reactor: bool) -> Arc<Self> {
        // SAFETY: the caller lends a live descriptor for this call, or null.
        let owned = unsafe { waker.as_ref().map(|waker| OwnedWaker::retain(waker)) };
        Arc::new(Self {
            state: Mutex::new(State::Pending(owned)),
            reactor,
            cleanup: Mutex::new(Cleanup::default()),
            connect_deadline: Mutex::new(None),
        })
    }

    pub(crate) fn is_pending(&self) -> bool {
        matches!(*self.state.lock_or_recover(), State::Pending(_))
    }

    pub(crate) fn complete(&self, result: Result<IoValue, IoFailure>) {
        let result = match result {
            Ok(IoValue::Bytes(bytes)) if u32::try_from(bytes.len()).is_err() => {
                Err(IoFailure::from_io(
                    "I/O result exceeds Hew bytes capacity",
                    &io::Error::from_raw_os_error(libc::EFBIG),
                ))
            }
            other => other,
        };
        let waker = {
            let mut state = self.state.lock_or_recover();
            if !matches!(*state, State::Pending(_)) {
                // The producer still owns result. In particular, a late accept
                // closes its connection here instead of stranding a table entry.
                return;
            }
            let State::Pending(waker) = std::mem::replace(&mut *state, State::Ready(result)) else {
                unreachable!("pending state was checked under this lock")
            };
            waker
        };
        self.clear_connect_deadline();
        if let Some(waker) = waker {
            waker.wake();
        }
    }

    fn cancel(&self) -> bool {
        self.cancel_with_notification(false)
    }

    pub(crate) fn cancel_and_wake(&self) -> bool {
        self.cancel_with_notification(true)
    }

    fn cancel_with_notification(&self, notify: bool) -> bool {
        let (won, waker) = {
            let mut state = self.state.lock_or_recover();
            if matches!(*state, State::Pending(_)) {
                let State::Pending(waker) = std::mem::replace(&mut *state, State::Cancelled) else {
                    unreachable!("pending state was checked under this lock")
                };
                (true, waker)
            } else {
                (false, None)
            }
        };
        self.clear_connect_deadline();
        if self.reactor {
            crate::reactor::reactor_detach_async_io(self);
        }
        // Releasing a readiness target can call user-supplied runtime callbacks.
        // Never run those callbacks while holding the operation state lock.
        if let Some(waker) = waker {
            if notify {
                waker.wake();
            }
        }
        won
    }

    fn discard_result(&self) {
        let result = {
            let mut state = self.state.lock_or_recover();
            if matches!(*state, State::Ready(_)) {
                Some(std::mem::replace(&mut *state, State::Taken))
            } else {
                None
            }
        };
        // A completed producer may still hold its notification reference. The
        // creator's free nevertheless releases an untaken socket immediately.
        drop(result);
    }
}

/// Inspect readiness without consuming the operation or any result.
///
/// # Safety
/// `operation` must be null or a live creator-owned operation reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_status(operation: *const HewAsyncIo) -> i32 {
    // SAFETY: validity is the caller's contract; null is a defined error.
    unsafe { operation.as_ref() }.map_or(AsyncIoStatus::Error as i32, |operation| {
        operation.state.lock_or_recover().status() as i32
    })
}

/// Observe producer quiescence, optionally registering a retained cleanup wake.
/// Returns 1 when every producer has released its inputs and borrowed resources,
/// otherwise 0. A null operation is already drained. Registration and the last
/// producer release share one lock, so callers cannot miss the cleanup wake.
/// Cancel first, then wait for this to return 1 before releasing resources lent
/// to the operation. Logical `Cancelled` readiness alone is insufficient.
///
/// # Safety
/// `operation` is null or a live creator-owned reference. `waker` is null (poll)
/// or a valid borrowed descriptor for the sole cleanup waiter.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_cleanup_status(
    operation: *const HewAsyncIo,
    waker: *const HewWaker,
) -> i32 {
    // SAFETY: validity of the borrowed reference is the caller's contract.
    let Some(operation) = (unsafe { operation.as_ref() }) else {
        return 1;
    };
    // Retain and release callbacks must run outside the cleanup lock.
    // SAFETY: the descriptor is borrowed for this call or null.
    let retained = unsafe { waker.as_ref().map(|waker| OwnedWaker::retain(waker)) };
    let (ready, previous) = {
        let mut cleanup = operation.cleanup.lock_or_recover();
        if cleanup.producers == 0 {
            (1, retained)
        } else if retained.is_some() {
            (0, std::mem::replace(&mut cleanup.waker, retained))
        } else {
            (0, None)
        }
    };
    drop(previous);
    ready
}

/// Cancel a pending operation. Returns 1 if cancellation won, otherwise 0.
/// Cancellation is silent: its caller already owns the coroutine's cancel edge.
/// A running file syscall may finish later, owning only its copied inputs and
/// producer reference; its result is discarded.
///
/// # Safety
/// `operation` must be null or a live creator-owned operation reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_cancel(operation: *const HewAsyncIo) -> i32 {
    // SAFETY: validity is the caller's contract.
    unsafe { operation.as_ref() }.map_or(0, |operation| i32::from(operation.cancel()))
}

/// Cancel and release the creator reference, disposing of an untaken result.
/// Producer references keep queued or in-flight work alive until it finishes.
///
/// # Safety
/// `operation` is null or the owned reference returned by a start operation;
/// it must not be used after this call. A producer never calls this function.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_free(operation: *const HewAsyncIo) {
    if operation.is_null() {
        return;
    }
    // SAFETY: the caller transfers exactly the Arc reference returned by start.
    let owned = unsafe { Arc::from_raw(operation) };
    owned.cancel();
    owned.discard_result();
    drop(owned);
}

/// Transfer bytes to the resume edge. Empty bytes are a successful value.
/// The output is untouched unless this returns `AsyncIoStatus::Success`.
///
/// # Safety
/// `operation` is a live operation from a bytes-producing start. `out` is null
/// or aligned writable storage for one `BytesTriple`, valid during this call
/// only. Successful output owns one managed bytes reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_take_bytes(
    operation: *const HewAsyncIo,
    out: *mut BytesTriple,
) -> i32 {
    // SAFETY: the caller provides a live operation, or null.
    let Some(operation) = (unsafe { operation.as_ref() }) else {
        return AsyncIoStatus::Error as i32;
    };
    if out.is_null() {
        return AsyncIoStatus::Error as i32;
    }
    let mut state = operation.state.lock_or_recover();
    let State::Ready(Ok(IoValue::Bytes(bytes))) = &*state else {
        return if state.status() == AsyncIoStatus::Success {
            AsyncIoStatus::Error as i32
        } else {
            state.status() as i32
        };
    };
    let Ok(len) = u32::try_from(bytes.len()) else {
        return AsyncIoStatus::Error as i32;
    };
    // SAFETY: bytes owns len readable bytes; out is aligned writable storage.
    // The managed allocation belongs to the resume edge only after this write.
    unsafe { out.write(crate::bytes::hew_bytes_from_static(bytes.as_ptr(), len)) };
    *state = State::Taken;
    AsyncIoStatus::Success as i32
}

/// Transfer a count from a completed write. The output is untouched on failure.
///
/// # Safety
/// `operation` is live; `out` is null or aligned writable i64 storage.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_take_count(
    operation: *const HewAsyncIo,
    out: *mut i64,
) -> i32 {
    // SAFETY: the caller provides a live operation, or null.
    let Some(operation) = (unsafe { operation.as_ref() }) else {
        return AsyncIoStatus::Error as i32;
    };
    if out.is_null() {
        return AsyncIoStatus::Error as i32;
    }
    let mut state = operation.state.lock_or_recover();
    let State::Ready(Ok(IoValue::Count(count))) = &*state else {
        return if state.status() == AsyncIoStatus::Success {
            AsyncIoStatus::Error as i32
        } else {
            state.status() as i32
        };
    };
    // SAFETY: out is writable; the lock excludes concurrent take/cancellation.
    unsafe { out.write(*count) };
    *state = State::Taken;
    AsyncIoStatus::Success as i32
}

/// Transfer an accepted connection handle. Until this succeeds the operation
/// owns its close authority, including when readiness preceded cancellation.
///
/// # Safety
/// `operation` is live; `out` is null or aligned writable i64 storage. A
/// successful caller must eventually close the returned transport handle.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_take_handle(
    operation: *const HewAsyncIo,
    out: *mut i64,
) -> i32 {
    // SAFETY: the caller provides a live operation, or null.
    let Some(operation) = (unsafe { operation.as_ref() }) else {
        return AsyncIoStatus::Error as i32;
    };
    if out.is_null() {
        return AsyncIoStatus::Error as i32;
    }
    let mut state = operation.state.lock_or_recover();
    let State::Ready(Ok(IoValue::Connection(handle))) = &mut *state else {
        return if state.status() == AsyncIoStatus::Success {
            AsyncIoStatus::Error as i32
        } else {
            state.status() as i32
        };
    };
    // SAFETY: out is writable; ownership is disarmed under the same lock.
    unsafe { out.write(i64::from(handle.0)) };
    handle.0 = -1;
    *state = State::Taken;
    AsyncIoStatus::Success as i32
}

/// Restore ordinary I/O error metadata on the resumed execution thread.
/// Success clears stale errors; pending/cancelled operations leave the channel
/// unchanged. Call immediately before continuing the existing source wrapper,
/// while the operation is still owned; this does not consume its result.
///
/// # Safety
/// `operation` is null or a live operation reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_restore_error(operation: *const HewAsyncIo) -> i32 {
    // SAFETY: the caller owns a live reference through its resume edge.
    let Some(operation) = (unsafe { operation.as_ref() }) else {
        return AsyncIoStatus::Error as i32;
    };
    let state = operation.state.lock_or_recover();
    match &*state {
        State::Ready(Err(error)) => crate::stream_error::set_last_error_with_errno_and_kind(
            error.message.clone(),
            error.errno,
            error.kind,
        ),
        State::Ready(Ok(_)) | State::Taken => {
            let _ = crate::stream_error::take_last_error();
        }
        State::Pending(_) | State::Cancelled => {}
    }
    state.status() as i32
}

/// Return the operation's portable stream error-kind tag, without consuming it.
///
/// # Safety
/// `operation` is null or a live operation reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_error_kind(operation: *const HewAsyncIo) -> i32 {
    // SAFETY: the caller provides a live operation, or null.
    unsafe { operation.as_ref() }.map_or(0, |operation| match &*operation.state.lock_or_recover() {
        State::Ready(Err(error)) => error.kind,
        _ => 0,
    })
}

/// Return the operation's OS error code, without consuming it.
///
/// # Safety
/// `operation` is null or a live operation reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_errno(operation: *const HewAsyncIo) -> i32 {
    // SAFETY: the caller provides a live operation, or null.
    unsafe { operation.as_ref() }.map_or(libc::EINVAL, |operation| {
        match &*operation.state.lock_or_recover() {
            State::Ready(Err(error)) => error.errno,
            State::Cancelled => libc::ECANCELED,
            _ => 0,
        }
    })
}

/// Copy error text into a managed string owned by the caller. The copy remains
/// valid after freeing the operation; release it with the ordinary string drop.
///
/// # Safety
/// `operation` is null or a live operation reference.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_error(operation: *const HewAsyncIo) -> *mut HewString {
    // SAFETY: the caller provides a live operation, or null.
    unsafe { operation.as_ref() }.map_or_else(
        || string_from_str("invalid asynchronous I/O operation"),
        |operation| match &*operation.state.lock_or_recover() {
            State::Ready(Err(error)) => string_from_str(&error.message),
            State::Cancelled => string_from_str("asynchronous I/O cancelled"),
            _ => ptr::null_mut(),
        },
    )
}
