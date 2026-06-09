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
//! 4. When the body returns, the thread sends a `GenValue { is_done: true }`
//!    "done" sentinel and exits.
//! 5. [`hew_gen_free`] cancels (if running) and joins the thread.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::thread;

use crate::set_last_error;
use crate::task_scope::{hew_cancel_token_release, hew_cancel_token_retain, HewCancellationToken};

// ── Value envelope ──────────────────────────────────────────────────────

/// Envelope for a yielded value.
struct GenValue {
    data: *mut c_void,
    size: usize,
    /// `true` only on the final "done" sentinel sent when the body returns.
    is_done: bool,
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
///
/// The `parent_cancel_token` slot is a borrowed reference (ref-counted via
/// [`hew_cancel_token_retain`]) captured at [`hew_gen_ctx_create`] time from
/// the consumer's `HewExecutionContext::cancel_token`. The generator thread
/// has no execution-context of its own (it is spawned with a bare
/// `extern "C" fn` body); the captured snapshot is therefore the only way
/// the generator-body cancel-observation seam in
/// `hew-codegen-rs/src/llvm.rs` (`Terminator::Yield` arm) can reach the
/// enclosing scope's cancel token. Cancellation through the token tree is
/// observed by the body at every yield via
/// [`hew_gen_ctx_parent_cancel_token`].
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
    /// Set to `true` once the done sentinel has been received.
    done: AtomicBool,
    /// Defence-in-depth idempotency guard for [`hew_gen_free`] (Defect 3).
    /// `hew_gen_free` swaps this to `true` before touching any other field; if
    /// it was already `true` the call is a no-op (the context was already
    /// cancelled/joined/freed). With the MIR drop-spine fixes (Defects 1+2) the
    /// context frees exactly once, so this flag never fires in correct programs;
    /// it exists so that a FUTURE drop-edge miss degrades to a benign LEAK
    /// rather than heap corruption (a double `Box::from_raw`). It lives in the
    /// allocation, so the swap-and-check MUST happen before the `Box::from_raw`
    /// that deallocates it.
    released: AtomicBool,
    /// Borrowed reference (ref-counted) to the enclosing
    /// `HewExecutionContext::cancel_token` of the consumer thread at the
    /// moment of generator creation. May be null when no execution context
    /// is installed (test-fixture path) or when the enclosing scope has no
    /// cancel token. Released in [`hew_gen_free`].
    parent_cancel_token: *mut HewCancellationToken,
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
        done: AtomicBool::new(false),
        released: AtomicBool::new(false),
        // Capture the consumer thread's enclosing cancel token (if any) so
        // the generator body's cancel-observation seam can reach the
        // parent scope's token through `hew_gen_ctx_parent_cancel_token`.
        // The capture is borrowed (ref-counted via
        // `hew_cancel_token_retain`); `hew_gen_free` releases it. A null
        // current context (test fixture / no enclosing scope) yields a
        // null snapshot, which the FFI is required to tolerate (the
        // observation helper returns "not cancelled" for null tokens).
        //
        // SAFETY: `current_context()` is the thread-local accessor and
        // returns a pointer that is either null or installed by the
        // scheduler for the duration of the current dispatch; we read
        // `cancel_token` only when non-null, and `hew_cancel_token_retain`
        // is itself null-safe for additional defence-in-depth.
        parent_cancel_token: unsafe {
            let ctx_ptr = crate::execution_context::current_context();
            if ctx_ptr.is_null() {
                ptr::null_mut()
            } else {
                let snapshot = (*ctx_ptr).cancel_token;
                hew_cancel_token_retain(snapshot);
                snapshot
            }
        },
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
                is_done: true,
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
            is_done: true,
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
    cabi_guard!(ctx.is_null(), false);

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
                    is_done: true,
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
    if ctx_ref
        .yield_tx
        .send(GenValue {
            data,
            size,
            is_done: false,
        })
        .is_err()
    {
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
    cabi_guard!(ctx.is_null(), ptr::null_mut());

    // SAFETY: ctx is valid per caller contract.  Only the consumer
    // thread accesses resume_tx and yield_rx.
    let ctx_ref = unsafe { &*ctx };

    // If the generator already completed, return null immediately
    // without touching the channels (avoids deadlock on re-call).
    if ctx_ref.done.load(Ordering::Acquire) {
        if !out_size.is_null() {
            // SAFETY: out_size is non-null and valid per caller contract.
            unsafe { *out_size = 0 };
        }
        return ptr::null_mut();
    }

    // Signal the generator thread to resume (or start).
    if ctx_ref.resume_tx.send(true).is_err() {
        // Generator thread already exited.
        return ptr::null_mut();
    }

    // Wait for the next yielded value.
    match ctx_ref.yield_rx.recv() {
        Ok(val) if val.is_done => {
            // "Done" sentinel — mark so subsequent calls return immediately.
            ctx_ref.done.store(true, Ordering::Release);
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
            if val.data.is_null() {
                // Null-yield (not done): allocate a 1-byte buffer so the
                // consumer sees a non-null pointer and doesn't stop early.
                // SAFETY: requesting 1 byte from the system allocator.
                let buf = unsafe { libc::malloc(1) };
                if !buf.is_null() {
                    // SAFETY: buf is non-null and points to at least 1 allocated byte.
                    unsafe { *buf.cast::<u8>() = 0 };
                }
                buf
            } else {
                val.data
            }
        }
        Err(_) => {
            // Channel closed — generator thread exited unexpectedly.
            ctx_ref.done.store(true, Ordering::Release);
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
    cabi_guard!(ctx.is_null());

    // SAFETY: ctx was Box-allocated and is exclusively owned by caller.
    unsafe {
        // Defect 3 — idempotency guard. Swap the `released` flag to `true`
        // BEFORE touching any other field; if it was already `true` the context
        // has already been cancelled/joined/freed, so a second `hew_gen_free` is
        // a no-op. This MUST precede the `Box::from_raw` below (which frees the
        // allocation the flag lives in). The flag never fires in correct
        // programs — the MIR drop spine (Defects 1+2) frees the context exactly
        // once — but with it a future drop-edge miss degrades to a benign LEAK
        // instead of a double `Box::from_raw` heap corruption. `AcqRel` so the
        // winning thread's prior writes are visible and a loser observes them.
        if (*ctx).released.swap(true, Ordering::AcqRel) {
            return;
        }

        // Signal cancel — ignore errors (thread may have already exited).
        let _ = (*ctx).resume_tx.send(false);

        // Join the generator thread.
        if let Some(handle) = (*ctx).handle.take() {
            if handle.join().is_err() {
                set_last_error("generator thread panicked during execution");
            }
        }

        // Drain any unconsumed values from the yield channel so we don't
        // leak the malloc'd data pointers inside each GenValue.
        while let Ok(val) = (*ctx).yield_rx.try_recv() {
            if !val.data.is_null() {
                libc::free(val.data);
            }
        }

        // Release the parent cancel-token snapshot captured at create-time.
        // Null is tolerated by the FFI; non-null was retained when stored
        // into the field, so the matched release here returns the token to
        // its pre-capture refcount.
        let parent_token = (*ctx).parent_cancel_token;
        (*ctx).parent_cancel_token = ptr::null_mut();
        hew_cancel_token_release(parent_token);

        drop(Box::from_raw(ctx));
    }
}

// ── Cancel-token observation seam ───────────────────────────────────────

/// Return the parent `HewCancellationToken` captured at generator-context
/// creation time. The returned pointer is borrowed; callers MUST NOT release
/// it (the generator context owns the retain). May be null when no
/// enclosing scope had a cancel token, in which case callers should treat
/// it as "not cancelled" (the runtime's
/// [`crate::task_scope::hew_cancel_token_is_requested`] is null-safe).
///
/// This accessor is the load-bearing operand source for the
/// `hew-codegen-rs` cancel-observation seam at every `Terminator::Yield`:
/// after the body resumes from `hew_gen_yield`, the body calls this
/// function and feeds its result into `hew_cancel_token_is_requested` to
/// detect an out-of-band parent-scope cancel. Without this swap the seam
/// would observe a const-null operand and never fire.
///
/// # Safety
///
/// `ctx` must be a valid pointer returned by [`hew_gen_ctx_create`], or
/// null. Null returns null; non-null borrows from the context.
#[no_mangle]
pub unsafe extern "C" fn hew_gen_ctx_parent_cancel_token(
    ctx: *mut HewGenCtx,
) -> *mut HewCancellationToken {
    cabi_guard!(ctx.is_null(), ptr::null_mut());
    // SAFETY: caller-guaranteed live ctx; the parent_cancel_token slot is
    // only written at create-time and cleared at free-time.
    unsafe { (*ctx).parent_cancel_token }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generator body that yields `count` values (each a heap-allocated i32)
    /// and then returns.
    extern "C" fn yielding_body(arg: *mut c_void, ctx: *mut HewGenCtx) {
        // SAFETY: arg points to a u32 count value copied by hew_gen_ctx_create.
        let count = unsafe { *arg.cast::<u32>() };
        for i in 0..count {
            // SAFETY: Allocate an i32 on the heap to yield.
            unsafe {
                let buf = libc::malloc(std::mem::size_of::<i32>());
                assert!(!buf.is_null());
                *buf.cast::<i32>() = i.cast_signed();
                let keep_going = hew_gen_yield(ctx, buf, std::mem::size_of::<i32>());
                libc::free(buf);
                if !keep_going {
                    return;
                }
            }
        }
    }

    /// Generator body that yields nothing and returns immediately.
    extern "C" fn empty_body(_arg: *mut c_void, _ctx: *mut HewGenCtx) {}

    #[test]
    fn free_with_unconsumed_values_does_not_leak() {
        // Yield 5 values but only consume 1, then free.
        // Before the fix, the remaining 4 malloc'd buffers would leak.
        // SAFETY: All pointers come from hew_gen_ctx_create / hew_gen_next
        // and are valid for the duration of this test.
        unsafe {
            let mut count: u32 = 5;
            let ctx = hew_gen_ctx_create(
                yielding_body,
                (&raw mut count).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );

            // Consume only the first value.
            let mut sz: usize = 0;
            let val = hew_gen_next(ctx, &raw mut sz);
            assert!(!val.is_null());
            assert_eq!(sz, std::mem::size_of::<i32>());
            assert_eq!(*val.cast::<i32>(), 0);
            libc::free(val);

            // Free with 4 values still queued — must not leak.
            hew_gen_free(ctx);
        }
    }

    #[test]
    fn free_empty_generator() {
        // A generator that yields nothing — free should work cleanly.
        // SAFETY: All pointers come from hew_gen_ctx_create / hew_gen_next.
        unsafe {
            let ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);

            // Drive once to get the done sentinel.
            let mut sz: usize = 0;
            let val = hew_gen_next(ctx, &raw mut sz);
            assert!(val.is_null());
            assert_eq!(sz, 0);

            hew_gen_free(ctx);
        }
    }

    #[test]
    fn free_before_any_iteration() {
        // Create a generator but free it without ever calling next.
        // The thread is blocked on the initial resume signal; sending
        // cancel (false) must not deadlock and must not leak.
        // SAFETY: All pointers come from hew_gen_ctx_create.
        unsafe {
            let mut count: u32 = 10;
            let ctx = hew_gen_ctx_create(
                yielding_body,
                (&raw mut count).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );

            hew_gen_free(ctx);
        }
    }

    // ── Null guard tests ───────────────────────────────────────────────

    #[test]
    fn gen_yield_null_ctx_returns_false() {
        // SAFETY: testing cabi_guard path — null ctx is expected.
        unsafe {
            assert!(!hew_gen_yield(ptr::null_mut(), ptr::null_mut(), 0));
        }
    }

    #[test]
    fn gen_next_null_ctx_returns_null() {
        // SAFETY: testing cabi_guard path — null ctx is expected.
        unsafe {
            let mut sz: usize = 42;
            let val = hew_gen_next(ptr::null_mut(), &raw mut sz);
            assert!(val.is_null());
        }
    }

    #[test]
    fn gen_free_null_is_noop() {
        // SAFETY: null is a documented no-op.
        unsafe {
            hew_gen_free(ptr::null_mut());
        }
    }

    // ── Defect 3: idempotent free ──────────────────────────────────────

    #[test]
    fn gen_free_is_idempotent_via_released_flag() {
        // Defect 3 (defence-in-depth): `hew_gen_free` swaps the `released` flag
        // to `true` before any teardown, so a second call is a no-op. With the
        // MIR drop-spine fixes (Defects 1+2) the context frees exactly once;
        // this proves the runtime backstop so a FUTURE drop-edge miss degrades
        // to a benign LEAK rather than a double `Box::from_raw`.
        //
        // Reading `released` after the real `hew_gen_free` would be a
        // use-after-free (it deallocates the box), so this test exercises the
        // swap-guard semantics in isolation on a ctx we KEEP alive: pre-set
        // `released = true` and assert `hew_gen_free` early-returns WITHOUT
        // joining the thread or freeing the box (the thread + box are then
        // reclaimed by a manual teardown that clears the flag).
        //
        // SAFETY: ctx is a live box from hew_gen_ctx_create; we never let
        // hew_gen_free deallocate it while we still observe it.
        unsafe {
            let mut count: u32 = 3;
            let ctx = hew_gen_ctx_create(
                yielding_body,
                (&raw mut count).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );

            // Simulate "already released": the swap returns the prior value.
            let prior = (*ctx).released.swap(true, Ordering::AcqRel);
            assert!(!prior, "a fresh ctx must start un-released");

            // The thread handle must still be present (no teardown happened).
            assert!(
                (*ctx).handle.is_some(),
                "pre-teardown ctx must still own its join handle"
            );

            // hew_gen_free now observes released == true and is a no-op: it
            // must NOT take the join handle nor free the box.
            hew_gen_free(ctx);
            assert!(
                (*ctx).handle.is_some(),
                "second free (released already true) must be a no-op: the join \
                 handle must be untouched and the box must not be freed"
            );

            // Manual teardown: clear the flag and free for real so the thread
            // joins and the box is reclaimed (no leak from this test).
            (*ctx).released.store(false, Ordering::Release);
            hew_gen_free(ctx);
        }
    }

    // ── Iteration behaviour ────────────────────────────────────────────

    #[test]
    fn full_iteration_yields_all_values_in_order() {
        // SAFETY: all pointers are from hew_gen_ctx_create / hew_gen_next.
        unsafe {
            let mut count: u32 = 4;
            let ctx = hew_gen_ctx_create(
                yielding_body,
                (&raw mut count).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );

            for i in 0..4i32 {
                let mut sz: usize = 0;
                let val = hew_gen_next(ctx, &raw mut sz);
                assert!(!val.is_null(), "value {i} should not be null");
                assert_eq!(sz, std::mem::size_of::<i32>());
                assert_eq!(*val.cast::<i32>(), i);
                libc::free(val);
            }

            // One more next should return done sentinel.
            let mut sz: usize = 99;
            let val = hew_gen_next(ctx, &raw mut sz);
            assert!(val.is_null());
            assert_eq!(sz, 0);

            hew_gen_free(ctx);
        }
    }

    #[test]
    fn next_after_done_returns_null_immediately() {
        // SAFETY: all pointers are from hew_gen_ctx_create / hew_gen_next.
        unsafe {
            let ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);

            // First call exhausts the generator.
            let mut sz: usize = 0;
            let val = hew_gen_next(ctx, &raw mut sz);
            assert!(val.is_null());

            // Second call hits the done flag — no channel interaction.
            let mut sz2: usize = 99;
            let val2 = hew_gen_next(ctx, &raw mut sz2);
            assert!(val2.is_null());
            assert_eq!(sz2, 0);

            hew_gen_free(ctx);
        }
    }

    #[test]
    fn next_with_null_out_size_does_not_crash() {
        // SAFETY: all pointers are from hew_gen_ctx_create / hew_gen_next.
        unsafe {
            let ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);
            let val = hew_gen_next(ctx, ptr::null_mut());
            assert!(val.is_null());
            hew_gen_free(ctx);
        }
    }

    // ── Edge cases ─────────────────────────────────────────────────────

    #[test]
    fn create_with_null_arg_and_zero_size_succeeds() {
        // SAFETY: null body_arg with size 0 is documented as valid.
        unsafe {
            let ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);
            assert!(!ctx.is_null());
            hew_gen_free(ctx);
        }
    }

    /// Generator body that yields a zero-size value (null data, size 0).
    extern "C" fn zero_size_yield_body(_arg: *mut c_void, ctx: *mut HewGenCtx) {
        // SAFETY: ctx is valid; yield with null/0 tests the sentinel path.
        unsafe {
            hew_gen_yield(ctx, ptr::null_mut(), 0);
        }
    }

    #[test]
    fn zero_size_yield_returns_non_null_sentinel() {
        // SAFETY: all pointers are from hew_gen_ctx_create / hew_gen_next.
        unsafe {
            let ctx = hew_gen_ctx_create(zero_size_yield_body, ptr::null_mut(), 0);

            let mut sz: usize = 99;
            let val = hew_gen_next(ctx, &raw mut sz);
            // The consumer allocates a 1-byte sentinel for null yields.
            assert!(!val.is_null());
            assert_eq!(sz, 0);
            libc::free(val);

            // Done sentinel after the single yield.
            let val2 = hew_gen_next(ctx, ptr::null_mut());
            assert!(val2.is_null());

            hew_gen_free(ctx);
        }
    }

    #[test]
    fn multiple_generators_concurrent() {
        // SAFETY: all pointers are from hew_gen_ctx_create / hew_gen_next.
        unsafe {
            let mut count1: u32 = 3;
            let mut count2: u32 = 2;

            let ctx1 = hew_gen_ctx_create(
                yielding_body,
                (&raw mut count1).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );
            let ctx2 = hew_gen_ctx_create(
                yielding_body,
                (&raw mut count2).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );

            // Interleave consumption — both generators run independently.
            let mut sz: usize = 0;
            let v = hew_gen_next(ctx1, &raw mut sz);
            assert_eq!(*v.cast::<i32>(), 0);
            libc::free(v);

            let v = hew_gen_next(ctx2, &raw mut sz);
            assert_eq!(*v.cast::<i32>(), 0);
            libc::free(v);

            let v = hew_gen_next(ctx1, &raw mut sz);
            assert_eq!(*v.cast::<i32>(), 1);
            libc::free(v);

            let v = hew_gen_next(ctx2, &raw mut sz);
            assert_eq!(*v.cast::<i32>(), 1);
            libc::free(v);

            // ctx2 is done (yielded 2 values).
            let v = hew_gen_next(ctx2, &raw mut sz);
            assert!(v.is_null());

            // ctx1 has one more.
            let v = hew_gen_next(ctx1, &raw mut sz);
            assert_eq!(*v.cast::<i32>(), 2);
            libc::free(v);

            let v = hew_gen_next(ctx1, &raw mut sz);
            assert!(v.is_null());

            hew_gen_free(ctx1);
            hew_gen_free(ctx2);
        }
    }

    // ── Parent cancel-token snapshot (cancel-observation seam) ─────────

    #[test]
    fn parent_cancel_token_null_when_no_execution_context() {
        // No execution context installed on this thread → snapshot is null.
        // SAFETY: ctx is from hew_gen_ctx_create; null snapshot is a
        // documented FFI shape (callers treat null as "not cancelled").
        unsafe {
            let ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);
            let token = hew_gen_ctx_parent_cancel_token(ctx);
            assert!(
                token.is_null(),
                "parent cancel snapshot must be null when no execution context is installed"
            );
            hew_gen_free(ctx);
        }
    }

    #[test]
    fn parent_cancel_token_null_ctx_returns_null() {
        // SAFETY: testing the cabi_guard path — null ctx is expected.
        unsafe {
            assert!(hew_gen_ctx_parent_cancel_token(ptr::null_mut()).is_null());
        }
    }

    #[test]
    fn parent_cancel_token_observes_parent_scope_cancel() {
        // Install an execution context whose cancel_token is a fresh root
        // token, create a generator under that context, and verify that
        // cancelling the parent token is observable through the
        // generator-context snapshot via
        // `hew_cancel_token_is_requested` — the exact path the codegen
        // cancel-observation seam at every yield consults.
        //
        // SAFETY: token + ctx + gen ctx are all valid for the test
        // duration; we drive the FFI in the same well-defined order as
        // production code.
        unsafe {
            let parent = crate::task_scope::hew_cancel_token_new_child(ptr::null_mut());
            let mut exec_ctx = crate::execution_context::HewExecutionContext {
                cancel_token: parent,
                ..crate::execution_context::HewExecutionContext::default()
            };
            let prev = crate::execution_context::set_current_context(&raw mut exec_ctx);

            let gen_ctx = hew_gen_ctx_create(empty_body, ptr::null_mut(), 0);
            let snapshot = hew_gen_ctx_parent_cancel_token(gen_ctx);
            assert_eq!(
                snapshot, parent,
                "gen-ctx must snapshot the consumer's execution context cancel token"
            );

            // Before parent cancel: observation returns 0 ("not requested").
            assert_eq!(
                crate::task_scope::hew_cancel_token_is_requested(snapshot),
                0,
                "fresh parent token must not report cancellation"
            );

            // Cancel the parent; the snapshot — being the same token —
            // observes the request immediately.
            crate::task_scope::hew_cancel_token_cancel(parent, 0);
            assert_eq!(
                crate::task_scope::hew_cancel_token_is_requested(snapshot),
                1,
                "post-cancel snapshot must report cancellation through the codegen seam"
            );

            hew_gen_free(gen_ctx);
            let restored = crate::execution_context::set_current_context(prev);
            assert_eq!(restored, &raw mut exec_ctx);
            crate::task_scope::hew_cancel_token_release(parent);
        }
    }
}
