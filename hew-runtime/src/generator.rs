//! Hew runtime: thread-based generator context for cross-actor streaming.
//!
//! A generator body runs in a dedicated thread and communicates yielded
//! values back to the consumer via [`std::sync::mpsc`] channels.  The
//! consumer drives iteration by calling [`hew_gen_next`], which sends a
//! resume signal and then receives the next yielded value.
//!
//! ## Protocol
//!
//! 1. [`hew_gen_ctx_create`] spawns the generator thread; it blocks on an
//!    initial resume signal.
//! 2. [`hew_gen_next`] sends `true` on the resume channel, then receives
//!    the next value from the yield channel.
//! 3. Inside the generator thread, [`hew_gen_yield`] sends a value on the
//!    yield channel, then blocks on the resume channel.
//! 4. When the body returns, the thread sends a `GenValue { null, 0 }`
//!    "done" sentinel and exits.
//! 5. [`hew_gen_free`] cancels (if running) and joins the thread.

use std::ffi::c_void;
use std::ptr;
use std::sync::mpsc;
use std::thread;

// ── Value envelope ──────────────────────────────────────────────────────

/// Envelope for a yielded value.  A null `data` pointer signals "done".
struct GenValue {
    data: *mut c_void,
    size: usize,
}

// SAFETY: `GenValue` only wraps a malloc'd pointer that is transferred
// across exactly one channel boundary; only one thread accesses it at a
// time.
unsafe impl Send for GenValue {}

// ── Generator context ───────────────────────────────────────────────────

/// Thread-based generator context.
///
/// All four channel endpoints live here.  The consumer side uses
/// `yield_rx` and `resume_tx`; the generator thread uses `yield_tx` and
/// `resume_rx` (accessed through the raw `*mut HewGenCtx` pointer passed
/// to [`hew_gen_yield`]).
pub struct HewGenCtx {
    /// Generator thread sends yielded values here (thread-side sender).
    yield_tx: mpsc::Sender<GenValue>,
    /// Consumer receives yielded values here.
    yield_rx: mpsc::Receiver<GenValue>,
    /// Consumer sends resume/cancel signals here.
    resume_tx: mpsc::Sender<bool>,
    /// Generator thread receives resume/cancel signals here.
    resume_rx: mpsc::Receiver<bool>,
    /// Join handle for the generator thread.
    handle: Option<thread::JoinHandle<()>>,
}

// SAFETY: The two threads partition access to the fields: the consumer
// thread uses `yield_rx` and `resume_tx`; the generator thread uses
// `yield_tx` and `resume_rx`.  `handle` is only accessed by the consumer.
// mpsc senders/receivers are individually `Send`.  No field is accessed
// from both threads simultaneously.
unsafe impl Send for HewGenCtx {}
// SAFETY: Same partitioned-access argument.  The raw-pointer API means
// Rust's borrow checker is not involved; we enforce the protocol manually.
unsafe impl Sync for HewGenCtx {}

impl std::fmt::Debug for HewGenCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewGenCtx")
            .field(
                "handle_alive",
                &self.handle.as_ref().map(|h| !h.is_finished()),
            )
            .finish_non_exhaustive()
    }
}

// ── Create ──────────────────────────────────────────────────────────────

/// Create a generator context and spawn the generator thread.
///
/// The spawned thread waits for an initial resume signal before calling
/// `body_fn(body_arg_copy, ctx)`, so the caller can store the returned
/// context pointer before iteration begins.
///
/// `body_arg` is deep-copied (`arg_size` bytes) so the caller may free
/// the original immediately.
///
/// # Panics
///
/// Panics if `malloc` fails to allocate `arg_size` bytes for the deep
/// copy of `body_arg`.
///
/// # Safety
///
/// - `body_fn` must be a valid function pointer with C calling convention.
/// - `body_arg` must point to at least `arg_size` readable bytes, or be
///   null when `arg_size` is 0.
/// - The returned pointer must eventually be freed with [`hew_gen_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_gen_ctx_create(
    body_fn: extern "C" fn(*mut c_void, *mut HewGenCtx),
    body_arg: *mut c_void,
    arg_size: usize,
) -> *mut HewGenCtx {
    let (yield_tx, yield_rx) = mpsc::channel::<GenValue>();
    let (resume_tx, resume_rx) = mpsc::channel::<bool>();

    // Deep-copy body_arg so the caller can free the original.
    let arg_copy: *mut c_void = if arg_size > 0 && !body_arg.is_null() {
        // SAFETY: Caller guarantees body_arg points to arg_size readable bytes.
        unsafe {
            let buf = libc::malloc(arg_size);
            assert!(!buf.is_null(), "hew_gen_ctx_create: malloc failed");
            ptr::copy_nonoverlapping(body_arg.cast::<u8>(), buf.cast::<u8>(), arg_size);
            buf
        }
    } else {
        ptr::null_mut()
    };

    let ctx = Box::into_raw(Box::new(HewGenCtx {
        yield_tx,
        yield_rx,
        resume_tx,
        resume_rx,
        handle: None,
    }));

    // Cast raw pointers to usize so the closure is Send (same pattern
    // as task_scope.rs).
    let ctx_raw = ctx as usize;
    let arg_raw = arg_copy as usize;
    let fn_raw = body_fn as usize;

    let handle = thread::spawn(move || {
        let ctx_ptr = ctx_raw as *mut HewGenCtx;
        let arg_copy = arg_raw as *mut c_void;
        // SAFETY: fn_raw is a valid extern "C" fn pointer passed to
        // hew_gen_ctx_create by the caller.
        let body: extern "C" fn(*mut c_void, *mut HewGenCtx) =
            unsafe { std::mem::transmute(fn_raw) };

        // SAFETY: ctx_ptr is valid — allocated above.  The generator
        // thread accesses only yield_tx and resume_rx through ctx_ptr;
        // the consumer accesses only yield_rx and resume_tx.
        let ctx_ref = unsafe { &*ctx_ptr };

        // Wait for the first resume signal before running the body.
        let go = ctx_ref.resume_rx.recv().unwrap_or(false);
        if !go {
            let _ = ctx_ref.yield_tx.send(GenValue {
                data: ptr::null_mut(),
                size: 0,
            });
            if !arg_copy.is_null() {
                // SAFETY: arg_copy was allocated with libc::malloc above.
                unsafe { libc::free(arg_copy) };
            }
            return;
        }

        // Run the generator body.  Catch panics so we always free
        // arg_copy and send the done sentinel.
        let body_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            body(arg_copy, ctx_ptr);
        }));

        // Free the deep-copied argument.
        if !arg_copy.is_null() {
            // SAFETY: arg_copy was allocated with libc::malloc above.
            unsafe { libc::free(arg_copy) };
        }

        if body_result.is_err() {
            eprintln!("hew: generator body panicked");
        }

        // Signal "done" — body completed without further yields.
        let _ = ctx_ref.yield_tx.send(GenValue {
            data: ptr::null_mut(),
            size: 0,
        });
    });

    // SAFETY: ctx was just allocated above and the generator thread has
    // not yet accessed the `handle` field (it is blocked on resume_rx).
    unsafe {
        (*ctx).handle = Some(handle);
    }

    ctx
}

// ── Yield ───────────────────────────────────────────────────────────────

/// Yield a value from the generator thread.
///
/// Deep-copies `value` (malloc + memcpy of `size` bytes), sends it on the
/// yield channel, then blocks until the consumer calls [`hew_gen_next`]
/// again (or [`hew_gen_free`] cancels the generator).
///
/// Returns `true` if the generator should continue, `false` if it was
/// cancelled (the body function should return immediately on `false`).
///
/// # Safety
///
/// - `ctx` must be a valid pointer created by [`hew_gen_ctx_create`].
/// - `value` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
/// - Must only be called from the generator thread.
#[no_mangle]
pub unsafe extern "C" fn hew_gen_yield(
    ctx: *mut HewGenCtx,
    value: *mut c_void,
    size: usize,
) -> bool {
    if ctx.is_null() {
        return false;
    }

    // Deep-copy the yielded value.
    let data = if size > 0 && !value.is_null() {
        // SAFETY: Caller guarantees value points to size readable bytes.
        unsafe {
            let buf = libc::malloc(size);
            if buf.is_null() {
                // malloc failure — signal done to avoid consumer deadlock.
                let ctx_ref = &*ctx;
                let _ = ctx_ref.yield_tx.send(GenValue {
                    data: ptr::null_mut(),
                    size: 0,
                });
                return false;
            }
            ptr::copy_nonoverlapping(value.cast::<u8>(), buf.cast::<u8>(), size);
            buf
        }
    } else {
        ptr::null_mut()
    };

    // SAFETY: ctx is valid per caller contract.  Only the generator
    // thread accesses yield_tx and resume_rx.
    let ctx_ref = unsafe { &*ctx };

    // Send the yielded value to the consumer.
    if ctx_ref.yield_tx.send(GenValue { data, size }).is_err() {
        // Consumer dropped — free the copy and let the body exit.
        if !data.is_null() {
            // SAFETY: data was allocated with libc::malloc above.
            unsafe { libc::free(data) };
        }
        return false;
    }

    // Block until the consumer signals resume or cancel.
    // Returns true to continue, false to exit the body.
    ctx_ref.resume_rx.recv().unwrap_or(false)
}

// ── Next ────────────────────────────────────────────────────────────────

/// Get the next yielded value from the generator.
///
/// Sends a resume signal, then blocks until the generator yields or
/// completes.  Returns the yielded value (caller owns the pointer, free
/// with `libc::free`) or null when the generator is done.
///
/// # Safety
///
/// - `ctx` must be a valid pointer created by [`hew_gen_ctx_create`].
/// - `out_size` must be a valid pointer to a `usize`.
/// - Must only be called from the consumer thread.
#[no_mangle]
pub unsafe extern "C" fn hew_gen_next(ctx: *mut HewGenCtx, out_size: *mut usize) -> *mut c_void {
    if ctx.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: ctx is valid per caller contract.  Only the consumer
    // thread accesses resume_tx and yield_rx.
    let ctx_ref = unsafe { &*ctx };

    // Signal the generator thread to resume (or start).
    if ctx_ref.resume_tx.send(true).is_err() {
        // Generator thread already exited.
        return ptr::null_mut();
    }

    // Wait for the next yielded value.
    match ctx_ref.yield_rx.recv() {
        Ok(val) if val.data.is_null() => {
            // "Done" sentinel.
            if !out_size.is_null() {
                // SAFETY: out_size is valid per caller contract.
                unsafe { *out_size = 0 };
            }
            ptr::null_mut()
        }
        Ok(val) => {
            if !out_size.is_null() {
                // SAFETY: out_size is valid per caller contract.
                unsafe { *out_size = val.size };
            }
            val.data
        }
        Err(_) => {
            // Channel closed — generator thread exited unexpectedly.
            ptr::null_mut()
        }
    }
}

// ── Free ────────────────────────────────────────────────────────────────

/// Free a generator context.
///
/// Sends a cancel signal if the generator thread is still waiting, joins
/// the thread, and deallocates the context.
///
/// # Safety
///
/// `ctx` must have been returned by [`hew_gen_ctx_create`] and must not
/// be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_gen_free(ctx: *mut HewGenCtx) {
    if ctx.is_null() {
        return;
    }

    // SAFETY: ctx was Box-allocated and is exclusively owned by caller.
    unsafe {
        // Signal cancel — ignore errors (thread may have already exited).
        let _ = (*ctx).resume_tx.send(false);

        // Join the generator thread.
        if let Some(handle) = (*ctx).handle.take() {
            let _ = handle.join();
        }

        drop(Box::from_raw(ctx));
    }
}
