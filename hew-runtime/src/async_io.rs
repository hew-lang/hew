//! Owned native I/O operations for suspendable Hew code.
//!
//! On wasm32 standard input and in-memory sink finish complete at submission.
//! Files, sockets and deadlines need native I/O facilities.
//!
//! A coroutine owns the returned reference and takes a result only on its
//! resume edge. Pool producers own their inputs and one `Arc` until they
//! finish. A readiness operation runs its syscall on the owning task's worker:
//! it tries at submission, and after the reactor reports readiness it tries
//! again from `hew_async_io_status`. Completion and cancellation use one locked
//! state transition; a losing producer drops its result normally. Wakers are
//! retained heap targets and run only after the state lock is released.

use std::io;
use std::ptr;
#[cfg(not(target_arch = "wasm32"))]
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

use hew_cabi::string::{string_from_str, HewString};

use crate::bytes::BytesTriple;
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};

#[cfg(not(target_arch = "wasm32"))]
mod connect;
#[cfg(not(target_arch = "wasm32"))]
mod deadline;
#[cfg(not(target_arch = "wasm32"))]
mod file;
#[cfg(not(target_arch = "wasm32"))]
mod net;
mod offload;
mod sink;
pub use sink::hew_async_sink_finish;
mod stdin;
#[cfg(not(target_arch = "wasm32"))]
pub use connect::{hew_async_tcp_connect, hew_async_tcp_connect_timeout};
#[cfg(not(target_arch = "wasm32"))]
pub(crate) use file::{start_sink_write, start_stream_read};
#[cfg(not(target_arch = "wasm32"))]
pub(crate) use net::{accept_now, start_tcp_readable, start_tcp_stream_write};
#[cfg(not(target_arch = "wasm32"))]
pub use net::{hew_async_tcp_accept, hew_async_tcp_read, hew_async_tcp_write};
pub use offload::hew_async_offload;
#[cfg(windows)]
pub(crate) use stdin::ensure_reader as ensure_stdin_reader;
pub use stdin::hew_async_stdin_read_line;

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

    /// The failure a blocking stream backing left in the stream error slot,
    /// keeping its kind so a sink can classify it.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn take_recorded() -> Option<Self> {
        let kind = crate::stream_error::take_last_error_kind();
        let errno = crate::stream_error::take_last_errno();
        crate::stream_error::take_last_error().map(|message| Self {
            kind,
            errno: if errno == 0 { libc::EIO } else { errno },
            message,
        })
    }

    /// New root work that a termination request refused.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn shutting_down(operation: &str) -> Self {
        let errno = crate::stream_error::CANCELLED_ERRNO;
        Self {
            kind: crate::stream_error::io_error_kind_tag(
                io::Error::from_raw_os_error(errno).kind(),
            ),
            errno,
            message: crate::shutdown::refusal_message(operation),
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
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
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct AcceptedConnection(pub(crate) i32);

#[cfg(not(target_arch = "wasm32"))]
impl Drop for AcceptedConnection {
    fn drop(&mut self) {
        if self.0 >= 0 {
            crate::transport::tcp_close_orphan_conn(self.0);
        }
    }
}

pub(crate) enum IoValue {
    #[cfg_attr(
        target_arch = "wasm32",
        expect(
            dead_code,
            reason = "wasm32 produces StdinLine and Count; native producers build this and the shared take entries name it"
        )
    )]
    Bytes(Vec<u8>),
    #[cfg_attr(
        target_arch = "wasm32",
        expect(
            dead_code,
            reason = "wasm32 produces StdinLine and Count; native producers build this and the shared take entries name it"
        )
    )]
    StreamItem(Option<Vec<u8>>),
    Count(i64),
    #[cfg(not(target_arch = "wasm32"))]
    Connection(AcceptedConnection),
    StdinLine(stdin::Line),
    /// An `#[offload]` call's environment holding its result.
    Offload(offload::OffloadEnv),
}

enum State {
    Pending(Option<Arc<OwnedWaker>>),
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
    /// The socket, action and readiness flag of a readiness operation.
    #[cfg(not(target_arch = "wasm32"))]
    net: Option<net::NetOp>,
    cleanup: Mutex<Cleanup>,
    #[cfg(windows)]
    file_cancel: file::FileCancellation,
    #[cfg(not(target_arch = "wasm32"))]
    deadline: Mutex<Option<deadline::Deadline>>,
    /// The running pool job producing the result, told when its caller gives up.
    #[cfg(not(target_arch = "wasm32"))]
    detach: Mutex<Option<crate::blocking_pool::Detach>>,
}

#[derive(Default)]
struct Cleanup {
    producers: usize,
    waker: Option<OwnedWaker>,
}

/// A producer lease covers queued work and every in-flight readiness snapshot.
/// Its last release proves no producer can still touch borrowed resources.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct IoProducer(Arc<HewAsyncIo>);

#[cfg(not(target_arch = "wasm32"))]
impl IoProducer {
    pub(crate) fn new(operation: Arc<HewAsyncIo>) -> Self {
        operation.cleanup.lock_or_recover().producers += 1;
        Self(operation)
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Clone for IoProducer {
    fn clone(&self) -> Self {
        Self::new(Arc::clone(&self.0))
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl std::ops::Deref for IoProducer {
    type Target = HewAsyncIo;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(not(target_arch = "wasm32"))]
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
    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn new(waker: *const HewWaker) -> Arc<Self> {
        // SAFETY: the caller lends the descriptor through construction.
        unsafe { Self::with_net(waker, None) }
    }

    /// # Safety
    /// A non-null descriptor obeys the shared `HewWaker` lifetime contract.
    #[cfg(target_arch = "wasm32")]
    unsafe fn new(waker: *const HewWaker) -> Arc<Self> {
        // SAFETY: the caller lends a live descriptor for this call, or null.
        let owned = unsafe {
            waker
                .as_ref()
                .map(|waker| Arc::new(OwnedWaker::retain(waker)))
        };
        Arc::new(Self {
            state: Mutex::new(State::Pending(owned)),
            cleanup: Mutex::new(Cleanup::default()),
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn with_net(waker: *const HewWaker, net: Option<net::NetOp>) -> Arc<Self> {
        // SAFETY: the caller lends a live descriptor for this call, or null.
        let owned = unsafe {
            waker
                .as_ref()
                .map(|waker| Arc::new(OwnedWaker::retain(waker)))
        };
        Arc::new(Self {
            state: Mutex::new(State::Pending(owned)),
            net,
            cleanup: Mutex::new(Cleanup::default()),
            deadline: Mutex::new(None),
            detach: Mutex::new(None),
            #[cfg(windows)]
            file_cancel: file::FileCancellation::default(),
        })
    }

    /// Record the reactor's readiness report and wake the owning task, whose
    /// next status poll performs the syscall. Runs on the reactor thread.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn signal_ready(&self) {
        let Some(net) = &self.net else {
            return;
        };
        net.ready.store(true, Ordering::SeqCst);
        let waker = match &*self.state.lock_or_recover() {
            State::Pending(waker) => waker.clone(),
            _ => None,
        };
        if let Some(waker) = waker {
            waker.wake();
        }
    }

    /// Run the pending syscall on this worker if readiness was reported.
    ///
    /// # Safety
    /// `operation` is a live creator-owned reference.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn advance_if_ready(operation: *const Self) {
        // SAFETY: the caller lends a live reference.
        let this = unsafe { &*operation };
        let Some(net) = &this.net else {
            return;
        };
        if net.ready.swap(false, Ordering::SeqCst) && this.is_pending() {
            // SAFETY: the creator reference keeps the allocation live; this
            // adds the reference the slot may retain as its waiter.
            let operation = unsafe {
                Arc::increment_strong_count(operation);
                Arc::from_raw(operation)
            };
            net::advance(&operation);
        }
    }

    /// Register the pool job now producing this result, so a caller that
    /// gives up releases the job's thread from the pool's cap. Runs on the
    /// job's thread.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn produced_by(&self, detach: crate::blocking_pool::Detach) {
        let state = self.state.lock_or_recover();
        if matches!(*state, State::Pending(_)) {
            // Cancellation changes the state under this lock, then takes this.
            *self.detach.lock_or_recover() = Some(detach);
        } else {
            drop(state);
            detach.detach();
        }
    }

    /// Whether this wait holds shutdown's drain open while it is parked: every
    /// wait except a process root's, which resumes on its own thread.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn holds_drain(&self) -> bool {
        self.net.as_ref().is_none_or(|net| net.holds_drain)
    }

    #[cfg(not(target_arch = "wasm32"))]
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
        #[cfg(not(target_arch = "wasm32"))]
        self.clear_deadline();
        if let Some(waker) = waker {
            waker.wake();
        }
        // Withdraw after the wake, so shutdown's idle probe never sees neither
        // a waiter nor a runnable task.
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(net) = &self.net {
            net.slot.forget(self);
        }
    }

    fn cancel(&self) -> bool {
        self.cancel_with_notification(false)
    }

    #[cfg(not(target_arch = "wasm32"))]
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
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.clear_deadline();
            if let Some(net) = &self.net {
                net.slot.forget(self);
            }
            if won {
                if let Some(detach) = self.detach.lock_or_recover().take() {
                    detach.detach();
                }
            }
        }
        #[cfg(windows)]
        if won {
            self.file_cancel.cancel();
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
    if operation.is_null() {
        return AsyncIoStatus::Error as i32;
    }
    // SAFETY: validity is the caller's contract.
    unsafe {
        #[cfg(not(target_arch = "wasm32"))]
        HewAsyncIo::advance_if_ready(operation);
        (*operation).state.lock_or_recover().status() as i32
    }
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
    #[cfg(not(target_arch = "wasm32"))]
    if ready == 1 && !operation.is_pending() {
        if let Some(failure) = operation.net.as_ref().and_then(net::NetOp::finish_sink) {
            let mut state = operation.state.lock_or_recover();
            if matches!(*state, State::Ready(Ok(_))) {
                *state = State::Ready(Err(failure));
            }
        }
    }
    #[cfg(windows)]
    if ready == 0
        && matches!(*operation.state.lock_or_recover(), State::Cancelled)
        && operation.file_cancel.cancel()
    {
        // A cancellation can precede kernel submission. Retry while this
        // producer owns the registered handle, before allowing its loan to end.
        // SAFETY: cleanup borrows the caller's live notification descriptor.
        if let Some(waker) = unsafe { waker.as_ref() } {
            // SAFETY: the descriptor stays live through this call.
            unsafe { OwnedWaker::retain(waker) }.wake();
        }
    }
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
    let bytes = match &*state {
        State::Ready(Ok(IoValue::Bytes(bytes))) => bytes.as_slice(),
        State::Ready(Ok(IoValue::StdinLine(line))) => line.bytes(),
        _ => {
            return if state.status() == AsyncIoStatus::Success {
                AsyncIoStatus::Error as i32
            } else {
                state.status() as i32
            };
        }
    };
    let Ok(len) = u32::try_from(bytes.len()) else {
        return AsyncIoStatus::Error as i32;
    };
    // SAFETY: the region is live under the state lock; the carrier copies it.
    unsafe { out.write(crate::bytes::hew_bytes_from_static(bytes.as_ptr(), len)) };
    if let State::Ready(Ok(IoValue::StdinLine(line))) = &mut *state {
        line.consume();
    }
    *state = State::Taken;
    AsyncIoStatus::Success as i32
}

/// Take the result of a stream's accept or readiness wait as a stream poll
/// status: 1 with an item, 2 at end of stream, 3 on failure, 0 while pending.
/// A wait the shutdown sweep cancelled, or one refused because shutdown closed
/// listener admission, ends the stream.
///
/// # Safety
/// `operation` is the live result of an accept or readable submission.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn take_watch_item(
    operation: *const HewAsyncIo,
    item: impl FnOnce(IoValue) -> Option<Vec<u8>>,
) -> (i32, Option<Vec<u8>>) {
    if operation.is_null() {
        return (3, None);
    }
    // SAFETY: the caller lends a live operation reference.
    unsafe { HewAsyncIo::advance_if_ready(operation) };
    // SAFETY: as above.
    let operation = unsafe { &*operation };
    let mut state = operation.state.lock_or_recover();
    match &*state {
        State::Pending(_) => return (0, None),
        State::Cancelled => return (2, None),
        State::Ready(Err(failure)) if failure.errno == crate::stream_error::CANCELLED_ERRNO => {
            return (2, None)
        }
        State::Ready(Ok(_)) => {}
        _ => return (3, None),
    }
    let State::Ready(Ok(value)) = std::mem::replace(&mut *state, State::Taken) else {
        unreachable!("checked under the same lock");
    };
    match item(value) {
        Some(item) => (1, Some(item)),
        None => (3, None),
    }
}

/// Take an accepted connection as a `size`-byte handle item; the stream's
/// consumer becomes its owner.
///
/// # Safety
/// `operation` is the live result of an accept submission.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn take_accepted_item(
    operation: *const HewAsyncIo,
    size: usize,
) -> (i32, Option<Vec<u8>>) {
    // SAFETY: forwards the caller's contract.
    unsafe {
        take_watch_item(operation, |value| {
            let IoValue::Connection(mut connection) = value else {
                return None;
            };
            Some(connection_item(
                std::mem::replace(&mut connection.0, -1),
                size,
            ))
        })
    }
}

/// A connection handle as a `size`-byte item: the slot image of the
/// receiver's `Connection`.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn connection_item(handle: i32, size: usize) -> Vec<u8> {
    match size {
        4 => handle.to_ne_bytes().to_vec(),
        _ => i64::from(handle).to_ne_bytes().to_vec(),
    }
}

/// Take a completed readiness wait as one `()` item.
///
/// # Safety
/// `operation` is the live result of a readable submission.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn take_readiness_item(operation: *const HewAsyncIo) -> (i32, Option<Vec<u8>>) {
    // SAFETY: forwards the caller's contract.
    unsafe {
        take_watch_item(operation, |value| {
            matches!(value, IoValue::Count(_)).then(Vec::new)
        })
    }
}

/// Take one content-backed stream result without conflating an empty item
/// with EOF. The operation retains an untaken result through cancellation.
///
/// # Safety
/// `operation` is the live result of a stream-read submission.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn take_stream_item(operation: *const HewAsyncIo) -> (i32, Option<Vec<u8>>) {
    if operation.is_null() {
        return (3, None);
    }
    // SAFETY: the caller lends a live operation reference.
    unsafe { HewAsyncIo::advance_if_ready(operation) };
    // SAFETY: as above.
    let operation = unsafe { &*operation };
    let mut state = operation.state.lock_or_recover();
    if matches!(*state, State::Pending(_)) {
        return (0, None);
    }
    if !matches!(
        *state,
        State::Ready(Ok(IoValue::StreamItem(_) | IoValue::Bytes(_)))
    ) {
        return (3, None);
    }
    let item = match std::mem::replace(&mut *state, State::Taken) {
        State::Ready(Ok(IoValue::StreamItem(item))) => item,
        State::Ready(Ok(IoValue::Bytes(bytes))) => (!bytes.is_empty()).then_some(bytes),
        _ => unreachable!(),
    };
    (if item.is_some() { 1 } else { 2 }, item)
}

/// The diagnostic a failed operation carries, for the caller's trap report.
///
/// # Safety
/// `operation` is null or a live operation reference.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn failure_message(operation: *const HewAsyncIo) -> Option<String> {
    // SAFETY: the caller lends a live operation reference, or null.
    let operation = unsafe { operation.as_ref() }?;
    match &*operation.state.lock_or_recover() {
        State::Ready(Err(failure)) => Some(failure.message.clone()),
        State::Cancelled => Some(operation.net.as_ref().map_or_else(
            || "I/O wait cancelled".to_string(),
            |net| format!("wait on {} cancelled", net.slot.describe()),
        )),
        _ => None,
    }
}

/// How a failed socket write ended.
#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WriteFailure {
    /// The peer reset or closed the connection.
    Closed,
    /// The write deadline passed; this many bytes of the item reached the OS.
    TimedOut(i64),
    /// Any other failure, which traps.
    Other,
}

/// Classify a failed write operation for its sink's typed result.
///
/// # Safety
/// `operation` is a live operation reference whose status is `Error`.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn write_failure(operation: *const HewAsyncIo) -> WriteFailure {
    // SAFETY: the caller lends a live operation reference.
    let operation = unsafe { &*operation };
    let state = operation.state.lock_or_recover();
    let State::Ready(Err(failure)) = &*state else {
        return WriteFailure::Other;
    };
    match failure.kind {
        crate::stream_error::IO_ERROR_KIND_CONNECTION_CLOSED => WriteFailure::Closed,
        crate::stream_error::IO_ERROR_KIND_TIMED_OUT => {
            WriteFailure::TimedOut(committed(operation))
        }
        _ => WriteFailure::Other,
    }
}

/// Bytes of a write's item that reached the OS, or 0 for any other operation.
#[cfg(not(target_arch = "wasm32"))]
fn committed(operation: &HewAsyncIo) -> i64 {
    let written = operation
        .net
        .as_ref()
        .and_then(net::NetOp::written)
        .unwrap_or(0);
    i64::try_from(written).unwrap_or(i64::MAX)
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
#[cfg(not(target_arch = "wasm32"))]
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

/// Move an `#[offload]` call's result into `out` and release its arguments.
/// The output is untouched unless this returns `AsyncIoStatus::Success`.
///
/// # Safety
/// `operation` is live and came from `hew_async_offload`; `out` is writable for
/// `size` bytes, and `offset`/`size` locate the result in its environment.
#[no_mangle]
pub unsafe extern "C" fn hew_async_io_take_offload(
    operation: *const HewAsyncIo,
    out: *mut u8,
    offset: usize,
    size: usize,
) -> i32 {
    // SAFETY: the caller provides a live operation, or null.
    let Some(operation) = (unsafe { operation.as_ref() }) else {
        return AsyncIoStatus::Error as i32;
    };
    let taken = {
        let mut state = operation.state.lock_or_recover();
        if !matches!(&*state, State::Ready(Ok(IoValue::Offload(_)))) {
            return if state.status() == AsyncIoStatus::Success {
                AsyncIoStatus::Error as i32
            } else {
                state.status() as i32
            };
        }
        std::mem::replace(&mut *state, State::Taken)
    };
    let State::Ready(Ok(IoValue::Offload(mut env))) = taken else {
        unreachable!("offload result was checked under the state lock")
    };
    // SAFETY: forwarded from this function's contract.
    unsafe { env.take_result(out, offset, size) };
    // Releasing the arguments runs generated destructors: outside the lock.
    drop(env);
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
        State::Ready(Err(error)) => {
            #[cfg(not(target_arch = "wasm32"))]
            crate::stream_error::set_last_write_committed(
                if error.kind == crate::stream_error::IO_ERROR_KIND_TIMED_OUT {
                    committed(operation)
                } else {
                    0
                },
            );
            crate::stream_error::set_last_error_with_errno_and_kind(
                error.message.clone(),
                error.errno,
                error.kind,
            );
        }
        State::Ready(Ok(IoValue::Offload(env))) => env.error.restore(),
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
            State::Cancelled => crate::stream_error::CANCELLED_ERRNO,
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
