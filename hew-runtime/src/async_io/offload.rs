//! `#[offload]` extern calls on the blocking pool.
//!
//! The compiler stores the call's arguments in a heap environment and
//! generates two thunks for each offloaded extern: `run` calls the C function
//! with those arguments and stores its result in the environment, and
//! `release` destroys the arguments and, when asked, the result. The job owns
//! the environment until the caller takes the result; a cancelled caller
//! resumes at once and the environment is released by whichever side finishes
//! last, exactly once.

use std::alloc::Layout;
use std::sync::Arc;

use super::{HewAsyncIo, IoValue};
use crate::wake::HewWaker;

/// Calls the extern with the environment's arguments and stores its result.
pub type OffloadRun = unsafe extern "C" fn(env: *mut u8);
/// Destroys the environment's arguments, and its result when `result_live`.
pub type OffloadRelease = unsafe extern "C" fn(env: *mut u8, result_live: i32);

/// The runtime I/O error slot set by the extern on the pool thread. It travels
/// with the result and is restored on the caller's thread before it resumes.
#[derive(Default)]
pub(crate) struct CarriedError {
    message: Option<String>,
    errno: i32,
    kind: i32,
}

impl CarriedError {
    fn capture() -> Self {
        // Each take clears the metadata after it: kind, errno, then message.
        let kind = crate::stream_error::take_last_error_kind();
        let errno = crate::stream_error::take_last_errno();
        Self {
            message: crate::stream_error::take_last_error(),
            errno,
            kind,
        }
    }

    pub(crate) fn restore(&self) {
        match &self.message {
            Some(message) => crate::stream_error::set_last_error_with_errno_and_kind(
                message.clone(),
                self.errno,
                self.kind,
            ),
            None => {
                let _ = crate::stream_error::take_last_error();
            }
        }
    }
}

/// One call's argument and result storage. Dropping it releases what it still
/// owns and frees the allocation.
pub(crate) struct OffloadEnv {
    env: *mut u8,
    layout: Layout,
    run: OffloadRun,
    release: OffloadRelease,
    result_live: bool,
    pub(crate) error: CarriedError,
}

// SAFETY: the environment holds owned Hew values (atomically counted or bit
// copies) and is touched by one thread at a time: the job until it completes,
// then the caller's take or whichever side drops it.
unsafe impl Send for OffloadEnv {}

impl OffloadEnv {
    fn run(&mut self) {
        // SAFETY: the compiler generated `run` for this environment's layout.
        unsafe { (self.run)(self.env) };
        self.result_live = true;
        self.error = CarriedError::capture();
    }

    /// Move the result into `out` and keep only the arguments to release.
    ///
    /// # Safety
    /// `out` is writable for `size` bytes and `offset + size` lies within the
    /// environment, as the compiler laid it out.
    pub(crate) unsafe fn take_result(&mut self, out: *mut u8, offset: usize, size: usize) {
        if self.result_live && size != 0 {
            // SAFETY: the caller's contract bounds both regions.
            unsafe { std::ptr::copy_nonoverlapping(self.env.add(offset), out, size) };
        }
        self.result_live = false;
    }
}

impl Drop for OffloadEnv {
    fn drop(&mut self) {
        // SAFETY: `release` matches this layout; the allocation is ours.
        unsafe {
            (self.release)(self.env, i32::from(self.result_live));
            std::alloc::dealloc(self.env, self.layout);
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn run_offload_job(context: *mut std::ffi::c_void) {
    use super::IoProducer;
    // SAFETY: a successful submit transfers this unique Box to one callback.
    let (operation, mut env) =
        *unsafe { Box::from_raw(context.cast::<(IoProducer, OffloadEnv)>()) };
    if !operation.is_pending() {
        // Cancelled before it started: only the arguments are released.
        return;
    }
    if let Some(detach) = crate::blocking_pool::Detach::current() {
        operation.produced_by(detach);
    }
    // A C call cannot unwind: a panic inside it aborts the process.
    env.run();
    operation.complete(Ok(IoValue::Offload(env)));
}

/// Run an `#[offload]` extern call without blocking the calling worker.
///
/// `env` came from `hew_alloc(size, align)` and holds the call's owned
/// arguments; ownership passes to the operation. Without an installed pool
/// (wasm32, or a stopped runtime) the call runs before this returns. Take the
/// result with `hew_async_io_take_offload`; free the operation on every edge.
///
/// # Safety
/// `run` and `release` are the compiler's thunks for `env`'s layout, `env` is
/// an initialized environment allocated with `size` and `align`, and `waker`
/// is null or a live borrowed descriptor.
///
/// # Panics
/// Panics if `size` and `align` do not form a layout, which the compiler's
/// environment always does.
#[no_mangle]
pub unsafe extern "C" fn hew_async_offload(
    run: OffloadRun,
    release: OffloadRelease,
    env: *mut u8,
    size: usize,
    align: usize,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    let layout =
        Layout::from_size_align(size.max(1), align).expect("compiler-laid offload environment");
    let mut env = OffloadEnv {
        env,
        layout,
        run,
        release,
        result_live: false,
        error: CarriedError::default(),
    };
    // SAFETY: the start entrypoint lends the waker for this call.
    let operation = unsafe { HewAsyncIo::new(waker) };
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(pool) = crate::blocking_pool::shared_blocking_pool_opt() {
        let job = Box::into_raw(Box::new((
            super::IoProducer::new(Arc::clone(&operation)),
            env,
        )));
        // SAFETY: the pool is installed; the job is owned until its callback.
        // It owns its arguments and finishing it touches only the job, so
        // runtime exit leaves a call whose caller gave up running.
        let status =
            unsafe { crate::blocking_pool::submit_abandonable(pool, run_offload_job, job.cast()) };
        if status == 0 {
            return Arc::into_raw(operation);
        }
        // SAFETY: failed admission did not transfer the job.
        let (_, rejected) = *unsafe { Box::from_raw(job) };
        env = rejected;
    }
    env.run();
    operation.complete(Ok(IoValue::Offload(env)));
    Arc::into_raw(operation)
}
