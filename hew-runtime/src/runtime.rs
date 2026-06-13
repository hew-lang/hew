//! Runtime instance state — the de-globalized home for the process-wide
//! authorities that were previously free `static`s.
//!
//! # Why this module exists
//!
//! Historically every runtime authority (the scheduler, the live-actor table,
//! the name registry, shutdown phase, timers, the reactor, the node slot, …)
//! lived as an independent process-global `static`. That made it impossible
//! for more than one Hew runtime to exist in one process, forced test
//! serialization on the shared globals, and left ownership of the teardown
//! order spread across modules.
//!
//! [`RuntimeInner`] gathers those authorities into one owned struct. A single
//! **droppable** default slot ([`DEFAULT_RUNTIME`]) holds the one runtime that
//! today's AOT and JIT programs use. The slot is an [`AtomicPtr`] (not a
//! `OnceLock`) for the same reason the old `SCHEDULER` pointer was: the runtime
//! must be *freeable* on `hew_runtime_cleanup`, releasing the scheduler's
//! crossbeam deques, parkers, and stealer handles. A bare `OnceLock<RuntimeInner>`
//! could never free.
//!
//! # M1 scope (internal de-globalization only)
//!
//! This module adds **no** new `#[no_mangle] extern "C"` symbol. `RuntimeInner`,
//! and the slot helpers are pure Rust internals. The default runtime is
//! constructed and torn down exclusively through the existing lifecycle entry
//! points (`hew_sched_init` / `hew_runtime_cleanup` / `hew_sched_shutdown`).
//! The public host/embedding handle API (`hew_runtime_new/retain/release/...`)
//! is a later deliverable and is intentionally absent here.
//!
//! Subsystems migrate into [`RuntimeInner`] one at a time; each migration flips
//! a resolver's *source* to read its field off [`rt_default`] without changing
//! any caller or any observable behaviour. Single-runtime behaviour is
//! identical: there is exactly one runtime, and every resolver reads it.

#![cfg(not(target_arch = "wasm32"))]

use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};

use crate::lifetime::live_actors::LiveActors;
use crate::lifetime::poison_safe::PoisonSafe;
use crate::scheduler::Scheduler;
use crate::shutdown::SupervisorPtr;

/// Process-wide runtime identity.
///
/// Distinct from a PID's `node_id`: a `RuntimeId` tags the runtime instance
/// that owns an actor/timer/capability, so that — once more than one runtime
/// can exist — cross-runtime routing can fail closed on an id mismatch without
/// dereferencing any handle. In M1 there is exactly one runtime, always
/// [`RuntimeId::DEFAULT`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RuntimeId(pub u64);

impl RuntimeId {
    /// The id of the single default runtime used by AOT and JIT programs.
    pub const DEFAULT: RuntimeId = RuntimeId(0);
}

/// The owned state of one Hew runtime instance.
///
/// Each field is an authority that previously lived as a process-global
/// `static`. Subsystems are migrated into this struct one per commit; fields
/// are added as each subsystem's statics move here.
///
/// Dropping a `RuntimeInner` drops every owned field in declaration order.
/// For the scheduler this releases the crossbeam deques, parkers, stealer
/// handles, and global injector queue — exactly the resources the old
/// `AtomicPtr<Scheduler>` slot existed to free on cleanup.
pub(crate) struct RuntimeInner {
    /// Stable identity of this runtime instance.
    id: RuntimeId,
    /// M:N work-stealing scheduler. Was the `SCHEDULER` global pointer.
    pub(crate) scheduler: Scheduler,
    /// Live-actor liveness registry + deferred-teardown join handles. Was the
    /// `LIVE_ACTORS` + `DEFERRED_TEARDOWN_THREADS` globals.
    pub(crate) live_actors: LiveActors,
    /// Current graceful-shutdown phase (running/quiesce/drain/terminate/done/
    /// failed). Was the `SHUTDOWN_PHASE` global. `PHASE_RUNNING == 0`, so a
    /// freshly-constructed runtime starts running.
    pub(crate) shutdown_phase: AtomicI32,
    /// Registered top-level supervisors stopped (bottom-up) during graceful
    /// shutdown and freed by `hew_runtime_cleanup`. Was the
    /// `TOP_LEVEL_SUPERVISORS` global.
    pub(crate) supervisor_roots: PoisonSafe<Vec<SupervisorPtr>>,
}

impl RuntimeInner {
    /// Construct the inner state for a runtime that owns `scheduler`.
    pub(crate) fn new(scheduler: Scheduler) -> Self {
        Self {
            id: RuntimeId::DEFAULT,
            scheduler,
            live_actors: LiveActors::new(),
            shutdown_phase: AtomicI32::new(crate::shutdown::PHASE_RUNNING),
            supervisor_roots: PoisonSafe::new(Vec::new()),
        }
    }

    /// This runtime's identity.
    #[allow(
        dead_code,
        reason = "read by cross-runtime routing once >1 runtime exists (M3/M4)"
    )]
    pub(crate) fn id(&self) -> RuntimeId {
        self.id
    }
}

/// Number of [`RuntimeInner`] instances dropped, for drop-order/release-count
/// oracles. A leak detector running green is *non-evidence* for ownership
/// correctness (a thread-backed handle leak is invisible to it); the honest
/// oracle is an exact drop-count assertion under a known teardown sequence.
#[cfg(test)]
pub(crate) static RUNTIME_INNER_DROPS: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

/// Test-only drop observer. Production builds compile no `Drop` impl, so the
/// inner frees by field-drop exactly as the old `Box<Scheduler>` did; the
/// `#[cfg(test)]` impl only records that the field-drop happened, then lets the
/// fields drop normally as it returns.
#[cfg(test)]
impl Drop for RuntimeInner {
    fn drop(&mut self) {
        RUNTIME_INNER_DROPS.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    }
}

// ── Default runtime slot ────────────────────────────────────────────────

/// Default runtime pointer. Installed once by `hew_sched_init()` (via
/// [`install_default`]), freed by `hew_runtime_cleanup()` (via
/// [`take_default`]). Using `AtomicPtr` rather than `OnceLock` keeps the
/// inner state *droppable*: cleanup detaches the pointer and drops the boxed
/// `RuntimeInner`, freeing the scheduler's deques/parkers/stealers — a
/// `OnceLock<RuntimeInner>` could never free.
pub(crate) static DEFAULT_RUNTIME: AtomicPtr<RuntimeInner> = AtomicPtr::new(std::ptr::null_mut());

/// Resolve a reference to the default `RuntimeInner`, if one is installed.
///
/// # Safety of the returned reference
///
/// The reference is valid until `hew_runtime_cleanup()` detaches and drops the
/// inner. Cleanup runs only after all worker threads have been joined, so any
/// caller reachable from worker/dispatch code holds a valid reference for the
/// duration of its use. This mirrors the old `get_scheduler()` contract.
#[inline]
pub(crate) fn rt_default() -> Option<&'static RuntimeInner> {
    let ptr = DEFAULT_RUNTIME.load(Ordering::Acquire);
    if ptr.is_null() {
        None
    } else {
        // SAFETY: non-null means `install_default` published a boxed
        // `RuntimeInner`, valid until `take_default` frees it after all
        // workers are joined.
        Some(unsafe { &*ptr })
    }
}

/// Resolve the runtime that owns the work on this thread — fail closed.
///
/// This is the resolver every de-globalized subsystem reads through (the
/// live-actor registry, name registry, shutdown phase, supervisor roots,
/// timers, node slot). It returns the installed default runtime, or **panics**
/// when none is installed.
///
/// # Why fail-closed, not lazy-default
///
/// The explicit-install model (Q119/A119) requires a runtime to be installed
/// before any runtime authority is touched: AOT and JIT programs install it via
/// `hew_sched_init`; tests install it via the runtime test guard
/// (`crate::runtime_test_guard`) or `NoWorkerSchedulerForTest`. There is no
/// lazy auto-create and no silent `None` fallback — touching a runtime
/// authority with no runtime installed is a hard logic error, so it traps loudly
/// instead of fabricating state that would leak past the missing lifecycle call
/// (`no-fail-open-fallback-after-authority`).
///
/// Single-runtime today means `rt_current()` always resolves to the one default
/// runtime; the per-thread `CURRENT_RUNTIME` selection arrives with the TLS
/// install (M2).
#[inline]
pub(crate) fn rt_current() -> &'static RuntimeInner {
    rt_default().unwrap_or_else(|| {
        panic!(
            "hew-runtime: no runtime installed — a runtime authority was used \
             before hew_sched_init (production) or without a runtime test guard \
             (tests). Install a default RuntimeInner first."
        )
    })
}

/// Publish `inner` as the default runtime via compare-exchange.
///
/// Returns `true` when this call installed `inner`. Returns `false` when a
/// runtime was already installed; in that case the passed-in box is dropped,
/// matching the old "second `hew_sched_init` is a harmless no-op" semantics.
pub(crate) fn install_default(inner: Box<RuntimeInner>) -> bool {
    let ptr = Box::into_raw(inner);
    if DEFAULT_RUNTIME
        .compare_exchange(
            std::ptr::null_mut(),
            ptr,
            Ordering::AcqRel,
            Ordering::Relaxed,
        )
        .is_err()
    {
        // Another initialization beat us — drop ours.
        // SAFETY: we just allocated this box via `Box::into_raw`.
        drop(unsafe { Box::from_raw(ptr) });
        return false;
    }
    true
}

/// Detach and reclaim the default runtime, if one is installed.
///
/// The caller owns the returned box and decides when it drops — letting
/// `hew_runtime_cleanup` perform the scheduler-frees-last drop order. Returns
/// `None` when no runtime was installed.
pub(crate) fn take_default() -> Option<Box<RuntimeInner>> {
    let ptr = DEFAULT_RUNTIME.swap(std::ptr::null_mut(), Ordering::AcqRel);
    if ptr.is_null() {
        None
    } else {
        // SAFETY: the pointer was installed by `install_default`; the caller
        // guarantees (via the cleanup contract) that all workers are joined,
        // so no thread can still reference it.
        Some(unsafe { Box::from_raw(ptr) })
    }
}

/// Raw default-runtime pointer for the worker-loop swap-detection handshake.
///
/// Workers cache the pointer they bound to and compare against this on each
/// iteration; the test-only scheduler-swap harness relies on the comparison
/// to drain cleanly before freeing a transient runtime. Production never swaps
/// the pointer after init, so this comparison is compiled out of release
/// builds — hence the `#[cfg(test)]` gate.
#[cfg(test)]
#[inline]
pub(crate) fn default_runtime_ptr(ordering: Ordering) -> *mut RuntimeInner {
    DEFAULT_RUNTIME.load(ordering)
}

// ── Test-only default-slot manipulation ──────────────────────────────────
//
// The scheduler's free-path UAF tests install a worker-less scheduler and
// later restore the previous one. Before de-globalization they swapped the
// `SCHEDULER` pointer directly; now they swap the `RuntimeInner` pointer. These
// helpers keep that manipulation in one place and out of the production path.

/// Report whether the runtime at `ptr` owns a worker-backed scheduler.
///
/// The runtime test guard installs only **worker-less** placeholder runtimes,
/// and must reclaim its placeholder on drop only while it is still that
/// placeholder. Pointer-equality alone is unsound: when a test upgrades to a
/// real scheduler (`init_real_scheduler_for_test` frees the placeholder, then
/// `hew_sched_init` installs a worker-backed runtime), the allocator may hand
/// the freed box's address back for the new runtime (an ABA alias) — so a
/// guard that frees on pointer-equality alone would free a *live* worker-backed
/// runtime out from under its running workers. Checking that the slot's runtime
/// is still worker-less defeats that alias: a reused address now holds a
/// worker-backed scheduler, so the guard correctly declines to free it. A
/// `NoWorkerSchedulerForTest` that swaps the placeholder out and back leaves it
/// worker-less, so the guard still reclaims it (no leak).
///
/// # Safety
///
/// `ptr` must be null or a valid pointer to an installed `RuntimeInner` (the
/// caller holds the scheduler-test lock, so the slot is not concurrently freed).
#[cfg(test)]
pub(crate) unsafe fn runtime_ptr_is_worker_backed(ptr: *mut RuntimeInner) -> bool {
    if ptr.is_null() {
        return false;
    }
    // SAFETY: caller guarantees `ptr` is a live installed runtime.
    let inner = unsafe { &*ptr };
    inner.scheduler.is_worker_backed()
}

/// Install `inner` as the default runtime, returning the previously-installed
/// pointer (null if none). Test-only unconditional swap (no compare-exchange):
/// the caller restores `previous` on teardown.
#[cfg(test)]
pub(crate) fn test_swap_default(inner: *mut RuntimeInner) -> *mut RuntimeInner {
    DEFAULT_RUNTIME.swap(inner, Ordering::SeqCst)
}

/// Store `inner` as the default runtime with release ordering (test-only).
#[cfg(test)]
pub(crate) fn test_store_default(inner: *mut RuntimeInner) {
    DEFAULT_RUNTIME.store(inner, Ordering::Release);
}

/// Free a `RuntimeInner` raw pointer previously boxed for a test guard
/// (test-only). No-op on null.
///
/// # Safety
///
/// `inner` must be a pointer obtained from `Box::into_raw` for a `RuntimeInner`
/// that is no longer installed in the slot and is unreferenced by any thread
/// (the caller — the runtime test guard — holds `SCHED_TEST_MUTEX` and frees on
/// teardown, after any spawned worker has been joined).
#[cfg(test)]
pub(crate) unsafe fn free_runtime_for_test(inner: *mut RuntimeInner) {
    if !inner.is_null() {
        // SAFETY: see fn docs — the caller guarantees `inner` came from
        // `Box::into_raw` and is now unreferenced.
        drop(unsafe { Box::from_raw(inner) });
    }
}
