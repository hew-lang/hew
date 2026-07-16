//! Shared `#[cfg(test)]` scaffolding for the TLS/SMTP/QUIC actor-scoped
//! last-error regression tests (#2659).
//!
//! `hew_sched_init`/`hew_sched_shutdown`/`hew_runtime_cleanup` operate on a
//! single process-global scheduler slot, and `hew_runtime_cleanup` reclaims
//! *every* tracked actor, not just the caller's. A guard with its own
//! private lock only serializes tests within the module that declared it —
//! it does nothing to stop a TLS regression test's teardown from racing a
//! concurrently running SMTP or QUIC regression test's still-live actor.
//! Every protocol's actor-drift regression test shares this ONE lock/guard
//! so they mutually exclude each other on the single scheduler slot, not
//! just their own siblings within the same file.

use std::ffi::c_void;
use std::sync::{Mutex, MutexGuard, PoisonError};

use hew_runtime::actor::{self, HewActor};
use hew_runtime::{scheduler, set_current_context, HewExecutionContext};

static NET_ERROR_SLOT_RUNTIME_TEST_LOCK: Mutex<()> = Mutex::new(());

/// RAII scheduler install/teardown guard shared by every TLS/SMTP/QUIC
/// actor-drift regression test. Holding the single process-wide lock for
/// the guard's full lifetime means one test's `hew_runtime_cleanup` can
/// never reclaim an actor a concurrently running sibling test still owns.
pub(crate) struct NetErrorSlotRuntimeGuard {
    _lock: MutexGuard<'static, ()>,
}

impl NetErrorSlotRuntimeGuard {
    pub(crate) fn new() -> Self {
        let lock = NET_ERROR_SLOT_RUNTIME_TEST_LOCK
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        assert_eq!(scheduler::hew_sched_init(), 0);
        Self { _lock: lock }
    }
}

impl Drop for NetErrorSlotRuntimeGuard {
    fn drop(&mut self) {
        scheduler::hew_sched_shutdown();
        scheduler::hew_runtime_cleanup();
    }
}

/// Acquire the shared scheduler lock WITHOUT installing a runtime.
///
/// The fail-closed no-runtime tests need the guarantee that no runtime is
/// installed while they exercise a blocking-pool FFI entrypoint. Holding this
/// lock excludes every [`NetErrorSlotRuntimeGuard`]-based test from installing a
/// scheduler on the single process-global slot for the returned guard's
/// lifetime, so `shared_blocking_pool_opt()` observes `None`.
pub(crate) fn lock_without_runtime() -> MutexGuard<'static, ()> {
    NET_ERROR_SLOT_RUNTIME_TEST_LOCK
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
}

/// Spawn a minimal, never-scheduled actor solely to obtain a stable,
/// dereferenceable actor identity for `hew_actor_current_id()`. The dispatch
/// stub is never invoked — this actor receives no messages.
pub(crate) fn spawn_error_slot_test_actor() -> *mut HewActor {
    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    // SAFETY: null state / size 0 is a documented no-state spawn.
    unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) }
}

/// RAII actor-execution-context install/restore guard. Restoration happens
/// in `Drop`, not only after a closure returns normally — Rust runs `Drop`
/// impls while unwinding, so a panic while `actor` is installed still
/// restores the previous thread-local context before the unwind continues.
/// A guard-based bracket (rather than install-call-restore) means a later
/// test on the same OS thread never observes a stale actor pointer left
/// installed by a panicked predecessor.
pub(crate) struct ActorContextGuard {
    // Owns the installed context's backing storage for the guard's whole
    // lifetime; `set_current_context` only stores the pointer, so this must
    // outlive every access made while it is installed.
    _ctx: Box<HewExecutionContext>,
    prev: *mut HewExecutionContext,
}

impl ActorContextGuard {
    pub(crate) fn install(actor: *mut HewActor) -> Self {
        let mut ctx = Box::new(HewExecutionContext {
            actor,
            ..Default::default()
        });
        let ctx_ptr: *mut HewExecutionContext = &raw mut *ctx;
        let prev = set_current_context(ctx_ptr);
        Self { _ctx: ctx, prev }
    }
}

impl Drop for ActorContextGuard {
    fn drop(&mut self) {
        let _ = set_current_context(self.prev);
    }
}

/// Install `actor` as the current dispatch's actor for the duration of `f`
/// — the same install/restore bracket codegen places around every real
/// dispatch (`hew_context_install`/`hew_context_restore`) — so
/// `hew_actor_current_id()` resolves to `actor`'s id inside `f` exactly as
/// it would mid-dispatch. Context restoration is unwind-safe: it happens in
/// `ActorContextGuard::drop`, which still runs if `f` panics.
pub(crate) fn with_actor_context<R>(actor: *mut HewActor, f: impl FnOnce() -> R) -> R {
    let _guard = ActorContextGuard::install(actor);
    f()
}
