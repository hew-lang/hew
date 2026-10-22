//! Owned socket jobs supplied by native standard-library crates.

use std::any::Any;
use std::ffi::c_void;
use std::net::{Shutdown, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use super::{HewAsyncIo, IoFailure, IoProducer, IoValue, State};
use crate::util::MutexExt;
use crate::wake::HewWaker;

/// Cancellation shared with a native socket job.
/// Only an owned connection attempt publishes a socket for shutdown.
#[derive(Debug, Default)]
pub struct SocketCancellation {
    cancelled: AtomicBool,
    socket: Mutex<Option<TcpStream>>,
    listener: bool,
}

impl SocketCancellation {
    /// Publish an owned connection before starting its TLS handshake.
    /// Borrowed operations observe cancellation without shutting down the socket.
    ///
    /// # Errors
    /// Returns an error if cloning fails or cancellation already won.
    pub fn bind(&self, socket: &TcpStream) -> std::io::Result<()> {
        let socket = socket.try_clone()?;
        let mut current = self.socket.lock_or_recover();
        if self.is_cancelled() {
            let _ = socket.shutdown(Shutdown::Both);
            return Err(std::io::Error::new(
                std::io::ErrorKind::Interrupted,
                "socket operation cancelled",
            ));
        }
        *current = Some(socket);
        Ok(())
    }

    /// Whether the owning native request has been cancelled.
    #[must_use]
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::Acquire)
            || (self.listener && crate::reactor::listener_admission_closed())
    }

    pub(super) fn cancel(&self) {
        self.cancelled.store(true, Ordering::Release);
        let socket = self.socket.lock_or_recover().take();
        if let Some(socket) = socket {
            let _ = socket.shutdown(Shutdown::Both);
        }
    }
}

struct Job<F> {
    operation: IoProducer,
    cancel: Arc<SocketCancellation>,
    run: F,
}

unsafe extern "C" fn run_job<F, T>(context: *mut c_void)
where
    F: FnOnce(&Arc<SocketCancellation>) -> T + Send + 'static,
    T: Any + Send,
{
    // SAFETY: successful pool admission transfers this unique job to one call.
    let Job {
        operation,
        cancel,
        run,
    } = *unsafe { Box::from_raw(context.cast::<Job<F>>()) };
    if !operation.is_pending() {
        return;
    }
    if let Some(detach) = crate::blocking_pool::Detach::current() {
        operation.produced_by(detach);
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| run(&cancel)))
        .map(|result| IoValue::Library(Box::new(result)))
        .map_err(|payload| {
            crate::util::quarantine_panic_payload(payload);
            IoFailure::invalid("native socket operation panicked")
        });
    crate::blocking_pool::job_finishing();
    if cancel.is_cancelled() {
        operation.cancel_and_wake();
    }
    operation.complete(result);
}

/// Submit an owned native-library socket operation to the shared pool.
///
/// The job owns all inputs and its undelivered result. Cancellation interrupts
/// its published socket and permits the caller to resume without lending any
/// storage to work that remains blocked in address resolution.
///
/// # Safety
/// `waker` is null or a live borrowed descriptor for this call.
///
/// # Panics
/// Panics if a newly created, unpublished operation is already shared.
pub unsafe fn submit_socket_job<F, T>(waker: *const HewWaker, run: F) -> *const HewAsyncIo
where
    F: FnOnce(&Arc<SocketCancellation>) -> T + Send + 'static,
    T: Any + Send,
{
    // SAFETY: this forwards the caller's borrowed waker and owned inputs.
    unsafe { submit_job(waker, run, false) }
}

/// Submit a native accept governed by the reactor's listener admission.
/// Closing admission cancels the wait and wakes its caller, including a process
/// root. Existing connections continue using ordinary socket jobs.
///
/// # Safety
/// `waker` is null or a live borrowed descriptor for this call.
///
/// # Panics
/// Panics if a newly created, unpublished operation is already shared.
pub unsafe fn submit_listener_job<F, T>(waker: *const HewWaker, run: F) -> *const HewAsyncIo
where
    F: FnOnce(&Arc<SocketCancellation>) -> T + Send + 'static,
    T: Any + Send,
{
    // SAFETY: this forwards the caller's borrowed waker and owned inputs.
    unsafe { submit_job(waker, run, true) }
}

unsafe fn submit_job<F, T>(waker: *const HewWaker, run: F, listener: bool) -> *const HewAsyncIo
where
    F: FnOnce(&Arc<SocketCancellation>) -> T + Send + 'static,
    T: Any + Send,
{
    // SAFETY: the caller lends the descriptor through submission.
    let mut operation = unsafe { HewAsyncIo::new(waker) };
    let cancel = Arc::new(SocketCancellation {
        listener,
        ..SocketCancellation::default()
    });
    // The operation has not been published or cloned yet.
    Arc::get_mut(&mut operation)
        .expect("unpublished socket operation")
        .socket_cancel = Some(Arc::clone(&cancel));
    if cancel.is_cancelled() {
        operation.cancel_and_wake();
        return Arc::into_raw(operation);
    }
    if crate::shutdown::refuses_new_root_work() {
        operation.complete(Err(IoFailure::shutting_down("native socket operation")));
        return Arc::into_raw(operation);
    }
    let Some(pool) = crate::blocking_pool::shared_blocking_pool_opt() else {
        operation.complete(Err(IoFailure::invalid(
            "native socket operation requires an installed runtime",
        )));
        return Arc::into_raw(operation);
    };
    let job = Box::new(Job {
        operation: IoProducer::new(Arc::clone(&operation)),
        cancel,
        run,
    });
    // SAFETY: the job owns all inputs and successful admission transfers it.
    let submission =
        unsafe { crate::blocking_pool::submit_abandonable_owned(pool, run_job::<F, T>, job) };
    if let Err(job) = submission {
        drop(job);
        operation.complete(Err(IoFailure::invalid(
            "native socket operation pool could not accept work",
        )));
    }
    Arc::into_raw(operation)
}

/// Transfer a library result to its matching resume adapter.
///
/// # Safety
/// `operation` is a live operation submitted with result type `T`.
pub unsafe fn take_library_result<T: Any + Send>(operation: *const HewAsyncIo) -> Option<T> {
    // SAFETY: the caller lends its live operation reference.
    let operation = unsafe { operation.as_ref() }?;
    let result = {
        let mut state = operation.state.lock_or_recover();
        if !matches!(&*state, State::Ready(Ok(IoValue::Library(value))) if value.is::<T>()) {
            return None;
        }
        std::mem::replace(&mut *state, State::Taken)
    };
    let State::Ready(Ok(IoValue::Library(value))) = result else {
        unreachable!("library result checked under its state lock")
    };
    value.downcast().ok().map(|value| *value)
}
