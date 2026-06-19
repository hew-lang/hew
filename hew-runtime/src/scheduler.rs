//! M:N work-stealing scheduler for the Hew actor runtime.
//!
//! Manages a pool of OS worker threads that cooperatively execute actors.
//! Each worker owns a local Chase-Lev deque; when idle, workers steal from
//! peers (random victim selection) or the shared global injector queue.
//!
//! # C ABI
//!
//! - [`hew_sched_init`] — create and start the scheduler.
//! - [`hew_sched_shutdown`] — signal shutdown, join workers.
//!
//! # Internal API
//!
//! - [`sched_enqueue`] — submit an actor for scheduling.
//! - [`sched_try_wake`] — wake a parked worker thread.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::{c_int, c_void};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
// `AtomicPtr` is used only by test-only code: the `SCHED_TEST_DRAIN_PTR`
// handshake and the in-module `HewActor` test literals. The default-runtime
// slot itself now lives in `crate::runtime`.
#[cfg(test)]
use std::sync::atomic::AtomicPtr;
use std::sync::{Condvar, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use rand::RngExt;

use crate::actor::{self, HewActor, HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET};
use crate::deque::{GlobalQueue, WorkDeque, WorkStealer};
use crate::deterministic::hew_deterministic_set_seed;
use crate::execution_context::HewExecutionContext;
use crate::internal::types::HewActorState;
use crate::lifetime::poison_safe::PoisonSafe;
use crate::mailbox::{
    self, hew_mailbox_has_messages, hew_mailbox_try_recv, hew_msg_node_free, HewMailbox,
};
use crate::set_last_error;
use crate::util::MutexExt;

// ── Constants ───────────────────────────────────────────────────────────

/// Park timeout — workers recheck the shutdown flag at this interval.
const PARK_TIMEOUT: Duration = Duration::from_millis(10);

// ── Observability counters ──────────────────────────────────────────────

pub(crate) static TASKS_SPAWNED: AtomicU64 = AtomicU64::new(0);
pub(crate) static TASKS_COMPLETED: AtomicU64 = AtomicU64::new(0);
pub(crate) static STEALS_TOTAL: AtomicU64 = AtomicU64::new(0);
pub(crate) static MESSAGES_SENT: AtomicU64 = AtomicU64::new(0);
pub(crate) static MESSAGES_RECEIVED: AtomicU64 = AtomicU64::new(0);
pub(crate) static ACTIVE_WORKERS: AtomicU64 = AtomicU64::new(0);

struct ActivationMetricsGuard;

impl ActivationMetricsGuard {
    fn new() -> Self {
        ACTIVE_WORKERS.fetch_add(1, Ordering::Relaxed);
        Self
    }
}

impl Drop for ActivationMetricsGuard {
    fn drop(&mut self) {
        ACTIVE_WORKERS.fetch_sub(1, Ordering::Relaxed);
        TASKS_COMPLETED.fetch_add(1, Ordering::Relaxed);
    }
}

#[cfg(test)]
static ACTIVATE_PRE_REENQUEUE_HOOK: PoisonSafe<Option<fn(*mut HewActor)>> = PoisonSafe::new(None);

/// Fires inside `activate_actor` immediately after the worker WINS the
/// `Runnable -> Running` CAS — the exact CAS->marker-gap location. A regression
/// test installs a hook here to fire an external trap in the precise window the
/// pre-fix code left `dispatch_active == false` while the actor was already
/// `Running`, and asserts the fix (claim before the CAS) keeps the flag set so
/// the free-quiescence predicate refuses.
#[cfg(test)]
static ACTIVATE_POST_CAS_HOOK: PoisonSafe<Option<fn(*mut HewActor)>> = PoisonSafe::new(None);

#[cfg(any(test, debug_assertions))]
static INJECT_NULL_LOCK_SEAT_ONCE: AtomicBool = AtomicBool::new(false);

#[cfg(any(test, debug_assertions))]
pub fn inject_null_lock_seat_once_for_test() {
    INJECT_NULL_LOCK_SEAT_ONCE.store(true, Ordering::Release);
}

#[cfg(any(test, debug_assertions))]
fn dispatch_lock_seat_for_actor(
    actor: *mut HewActor,
) -> *mut crate::execution_context::HewActorStateLockState {
    if INJECT_NULL_LOCK_SEAT_ONCE.swap(false, Ordering::AcqRel) {
        std::ptr::null_mut()
    } else {
        crate::actor::actor_state_lock_seat(actor)
    }
}

#[cfg(not(any(test, debug_assertions)))]
fn dispatch_lock_seat_for_actor(
    actor: *mut HewActor,
) -> *mut crate::execution_context::HewActorStateLockState {
    crate::actor::actor_state_lock_seat(actor)
}

#[cfg(test)]
fn run_activate_pre_reenqueue_hook(actor: *mut HewActor) {
    let hook = ACTIVATE_PRE_REENQUEUE_HOOK.access(|h| *h);
    if let Some(hook) = hook {
        hook(actor);
    }
}

#[cfg(test)]
fn run_activate_post_cas_hook(actor: *mut HewActor) {
    let hook = ACTIVATE_POST_CAS_HOOK.access(|h| *h);
    if let Some(hook) = hook {
        hook(actor);
    }
}

// ── Reply-channel readers (ctx-backed) ──────────────────────────────────
//
// R17 sole-authority: the reply channel for the currently-dispatched message
// lives on [`HewExecutionContext::reply_channel`], and the "consumed" bit
// lives in [`HewExecutionContext::flags`] under
// [`HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED`]. Nested dispatch (worker A asks
// worker B mid-select) restores the outer arm's reply channel via the
// existing `prev_context` chain — no thread-local backing store survives.
//
// The reader/marker pair `hew_get_reply_channel` /
// `mark_current_reply_channel_consumed` lives in [`crate::execution_context`]
// because it is purely ctx-backed and target-neutral (native and WASM share
// one definition). It is re-exported here so existing call sites that import
// from `crate::scheduler::*` keep compiling.
pub use crate::execution_context::hew_get_reply_channel;
pub(crate) use crate::execution_context::mark_current_reply_channel_consumed;

fn current_reply_channel_consumed_on(
    ctx: *mut crate::execution_context::HewExecutionContext,
) -> bool {
    if ctx.is_null() {
        return false;
    }
    // SAFETY: caller passes a context still installed (or recently installed
    // and not yet unmapped) for this worker's dispatch frame.
    unsafe { ((*ctx).flags & crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED) != 0 }
}

fn clear_reply_channel_on(ctx: *mut crate::execution_context::HewExecutionContext) -> *mut c_void {
    if ctx.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller passes a context still backed by live storage for this
    // worker's dispatch frame.
    unsafe {
        let ch = (*ctx).reply_channel;
        (*ctx).reply_channel = std::ptr::null_mut();
        (*ctx).flags &= !crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
        ch
    }
}

fn stash_suspended_cancel_token(a: &HewActor, token: *mut crate::task_scope::HewCancellationToken) {
    if token.is_null() {
        return;
    }
    // SAFETY: the dispatch execution context holds a live token; retain it for
    // the parked continuation's resume context.
    unsafe { crate::task_scope::hew_cancel_token_retain(token) };
    let old = a
        .suspended_cancel_token
        .swap(token.cast(), Ordering::AcqRel);
    if !old.is_null() {
        // SAFETY: the actor slot owned the old retained token.
        unsafe { crate::task_scope::hew_cancel_token_release(old.cast()) };
    }
}

fn clear_suspended_cancel_token(a: &HewActor) {
    let token = a
        .suspended_cancel_token
        .swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !token.is_null() {
        // SAFETY: the actor slot owned this retained token.
        unsafe { crate::task_scope::hew_cancel_token_release(token.cast()) };
    }
}

fn restore_current_context_after_dispatch() {
    let ctx = crate::execution_context::current_context();
    if !ctx.is_null() {
        // SAFETY: scheduler-installed contexts point to live stack slots until
        // the dispatch recovery path restores the previously active context.
        let prev = unsafe { (*ctx).prev_context };
        let _ = crate::execution_context::set_current_context(prev);
    }
}

// ── Global scheduler instance ───────────────────────────────────────────

use crate::runtime::{self, RuntimeInner};

/// Test-only drain handshake for [`NoWorkerSchedulerForTest`].
///
/// `SCHED_TEST_DRAIN_PTR` holds the transient, worker-less default runtime
/// currently installed by the guard (or null when none is installed).
/// `SCHED_TEST_DRAIN_COUNT` is the number of worker threads currently executing
/// the body of [`worker_loop`] while bound to that transient.
///
/// A late-starting worker from a prior `hew_sched_init` can cache the transient
/// pointer during the install window; the guard's `Drop` must not free the
/// transient while such a worker is mid-iteration. Workers register in the
/// counter (and re-check the default-runtime pointer) before touching any
/// scheduler field; `Drop` spins until the counter reaches zero before freeing.
/// Both statics live for the lifetime of the process and are never freed, so the
/// handshake itself is memory-safe even as the transient runtime is torn down.
/// In non-test builds none of this is compiled, so the production hot path is
/// unaffected.
#[cfg(test)]
static SCHED_TEST_DRAIN_PTR: AtomicPtr<RuntimeInner> = AtomicPtr::new(std::ptr::null_mut());

#[cfg(test)]
static SCHED_TEST_DRAIN_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

/// Scope guard that decrements [`SCHED_TEST_DRAIN_COUNT`] when a worker leaves
/// the loop body, covering every exit path (`continue`, `break`, and panic).
#[cfg(test)]
struct SchedDrainGuard;

#[cfg(test)]
impl Drop for SchedDrainGuard {
    fn drop(&mut self) {
        SCHED_TEST_DRAIN_COUNT.fetch_sub(1, Ordering::SeqCst);
    }
}

/// Get a reference to the default runtime's scheduler, if initialized.
///
/// # Safety
///
/// The returned reference is valid as long as `hew_runtime_cleanup()`
/// has not been called. Since cleanup only runs after all worker
/// threads have been joined, this is safe for all normal use.
fn get_scheduler() -> Option<&'static Scheduler> {
    // The scheduler now lives in the default `RuntimeInner`; its lifetime is
    // the runtime's, freed by `hew_runtime_cleanup` after workers are joined.
    runtime::rt_default().map(|rt| &rt.scheduler)
}

/// Return `true` when the native scheduler has no observable work left to drain.
///
/// This is used by graceful shutdown to stop waiting once the runtime is
/// already idle, rather than always sleeping until the full wall-clock drain
/// timeout expires.
pub(crate) fn drain_is_idle() -> bool {
    let Some(sched) = get_scheduler() else {
        return true;
    };

    if ACTIVE_WORKERS.load(Ordering::Acquire) != 0 {
        return false;
    }
    if !sched.global_queue.is_empty() {
        return false;
    }
    if sched.stealers.iter().any(|stealer| !stealer.is_empty()) {
        return false;
    }

    ACTIVE_WORKERS.load(Ordering::Acquire) == 0
}

/// The scheduler owns the shared global queue, per-worker stealers,
/// shutdown flag, and condvar for worker parking.
///
/// Worker thread handles are stored behind a `PoisonSafe` so they can be
/// `take`-n during shutdown (`JoinHandle` is `Send` but not `Sync`).
pub(crate) struct Scheduler {
    // native-only: worker_handles are OS JoinHandles; no WASM scheduler thread model
    worker_handles: PoisonSafe<Vec<Option<JoinHandle<()>>>>,
    global_queue: GlobalQueue,
    stealers: Vec<WorkStealer>,
    shutdown: AtomicBool,
    /// Per-worker parking primitives. Each worker parks on its own
    /// `Mutex/Condvar` to avoid contention on a single global lock.
    parkers: Vec<Parker>,
    worker_count: usize,
}

#[cfg(test)]
impl Scheduler {
    /// Whether this scheduler owns real worker threads (non-empty `stealers`),
    /// as opposed to a worker-less placeholder. The runtime test guard uses
    /// this to refuse freeing a runtime that was upgraded to a real scheduler
    /// under a reused box address (see `runtime::runtime_ptr_is_worker_backed`).
    /// Mirrors the placeholder discriminator in `init_real_scheduler_for_test`.
    pub(crate) fn is_worker_backed(&self) -> bool {
        !self.stealers.is_empty()
    }
}

/// Per-worker parking primitive.
struct Parker {
    mutex: Mutex<()>,
    cond: Condvar,
}

// SAFETY: All fields are either `Sync` (`AtomicBool`, `Mutex`, `Condvar`,
// `GlobalQueue`, `Vec<WorkStealer>`, `Vec<Parker>`) or wrapped in a
// `PoisonSafe` (`JoinHandle`).
unsafe impl Sync for Scheduler {}

// ── Xorshift64 PRNG for victim selection ────────────────────────────────

fn randomized_scheduler_seed() -> u64 {
    let seed = rand::rng().random::<u64>();
    if seed == 0 {
        1
    } else {
        seed
    }
}

/// Minimal xorshift64 PRNG — one per worker thread.
struct Xorshift64(u64);

impl Xorshift64 {
    fn new(seed: u64) -> Self {
        Self(if seed == 0 { 1 } else { seed })
    }

    fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }
}

// ── C ABI ───────────────────────────────────────────────────────────────

/// Initialize and start the M:N scheduler.
///
/// Spawns one worker thread per available CPU core (falls back to 4).
/// Calling this more than once is a harmless no-op (returns 0).
///
/// Returns 0 on success. On failure the process exits with code 1 after
/// printing a diagnostic — scheduler init failure is unrecoverable.
#[no_mangle]
pub extern "C" fn hew_sched_init() -> c_int {
    let default_count = thread::available_parallelism().map_or(4, std::num::NonZeroUsize::get);

    let worker_count = match std::env::var("HEW_WORKERS") {
        Ok(val) => match val.parse::<usize>() {
            Ok(n) if n > 0 => n.clamp(1, crate::actor::HEW_MAX_WORKERS),
            _ => {
                eprintln!("warning: HEW_WORKERS={val} is invalid, using default");
                default_count
            }
        },
        Err(_) => default_count,
    }
    .clamp(1, crate::actor::HEW_MAX_WORKERS);

    match std::env::var("HEW_SEED") {
        Ok(seed_str) => {
            if let Ok(seed) = seed_str.parse::<u64>() {
                hew_deterministic_set_seed(seed);
            } else {
                eprintln!(
                    "hew-runtime: HEW_SEED='{seed_str}' not parseable as u64; using randomized seed"
                );
                hew_deterministic_set_seed(randomized_scheduler_seed());
            }
        }
        Err(_) => hew_deterministic_set_seed(randomized_scheduler_seed()),
    }

    // Phase 1: Create all deques and collect stealers BEFORE spawning
    // threads. Workers steal from each other's deques, so every deque
    // must exist before any worker runs.
    let mut deques = Vec::with_capacity(worker_count);
    let mut stealers = Vec::with_capacity(worker_count);

    for _ in 0..worker_count {
        // SAFETY: Pointers pushed into these deques will be valid
        // `*mut HewActor` managed by the actor lifecycle.
        let (deque, stealer) = unsafe { WorkDeque::new() };
        deques.push(deque);
        stealers.push(stealer);
    }

    // SAFETY: Same validity guarantee as above.
    let global_queue = unsafe { GlobalQueue::new() };

    let parkers: Vec<Parker> = (0..worker_count)
        .map(|_| Parker {
            mutex: Mutex::new(()),
            cond: Condvar::new(),
        })
        .collect();

    let scheduler = Box::new(Scheduler {
        worker_handles: PoisonSafe::new(Vec::new()),
        global_queue,
        stealers,
        shutdown: AtomicBool::new(false),
        parkers,
        worker_count,
    });

    // Install via CAS; second calls are harmless no-ops.
    // NOTE: The runtime is visible to concurrent callers from this point,
    // but workers are not yet spawned. A concurrent `hew_sched_init` will
    // return 0 (no-op) even if this thread later fails to spawn workers and
    // tears down. This is acceptable because init is expected to run once
    // from the main thread before any actor work begins.
    if !runtime::install_default(Box::new(RuntimeInner::new(*scheduler))) {
        // Another thread beat us — `install_default` already dropped ours.
        return 0;
    }

    // Install crash signal handlers for the entire process.
    crate::signal::init_crash_handling();

    crate::observe::configure_from_env();
    crate::observe::register_reset_hooks();

    // Install SIGTERM/SIGINT handlers for graceful shutdown.
    // SAFETY: Called from main thread during initialization.
    unsafe { crate::shutdown::install_shutdown_signal_handlers() };

    // Phase 2: Spawn worker threads.
    // If ANY worker fails to spawn, treat it as an initialisation failure:
    // clean up already-spawned workers and tear down the scheduler.
    let mut handles: Vec<Option<JoinHandle<()>>> = Vec::with_capacity(worker_count);
    let mut spawn_err: Option<std::io::Error> = None;

    // The runtime we just installed owns these workers; thread its pointer into
    // each spawn closure so the worker `enter()`s it (TLS) at loop entry. `rt_default`
    // resolves the pointer this thread published via `install_default` above — a
    // null here would mean a concurrent `take_default` raced cleanup against init,
    // which `hew_sched_init`'s once-from-main-thread contract forbids; bail loudly
    // rather than spawn an unbound worker.
    let Some(rt) = runtime::rt_default().map(|r| WorkerRuntimePtr(r as *const RuntimeInner)) else {
        eprintln!("hew: scheduler init failed — default runtime lost after install");
        std::process::exit(1);
    };

    for (id, deque) in deques.into_iter().enumerate() {
        match thread::Builder::new()
            .name(format!("hew-worker-{id}"))
            .spawn(move || worker_loop(id, rt, &deque))
        {
            Ok(handle) => handles.push(Some(handle)),
            Err(e) => {
                spawn_err = Some(e);
                break;
            }
        }
    }

    if let Some(e) = spawn_err {
        // Worker spawn failed — tear down everything.
        let spawned = handles.len();
        set_last_error(format!(
            "hew_sched_init: failed to spawn worker {spawned}: {e}"
        ));
        eprintln!(
            "hew: scheduler init failed — could not spawn worker {spawned}/{worker_count}: {e}"
        );
        teardown_after_spawn_failure(handles);
        std::process::exit(1);
    }

    // We know `SCHEDULER` was just set by us.
    let Some(sched) = get_scheduler() else {
        eprintln!("hew: scheduler init failed — global pointer lost after CAS");
        std::process::exit(1);
    };
    sched.worker_handles.access(|lock| *lock = handles);

    // Register subsystem reset hooks for JIT session lifecycle.
    // Tracing first so events are cleared before the profiler type registry.
    crate::tracing::register_trace_reset_hook();
    // Profiler dispatch-registry clear second (native + profiler feature only;
    // the stub profiler module does not expose register_reset_hooks).
    #[cfg(all(not(target_arch = "wasm32"), feature = "profiler"))]
    crate::profiler::register_reset_hooks();

    // Start the profiler if HEW_PPROF is set.
    crate::profiler::maybe_start();
    // Start the OTel exporter if HEW_OTEL_ENDPOINT is set.
    crate::otel::maybe_start();

    0
}

/// Signal, wake, and join scheduler workers, optionally detaching the owning
/// default runtime from its slot for caller-controlled final drop.
///
/// When `handles` is `None`, the worker-handle list is swapped out of the
/// scheduler mutex before joining so no lock is held across the
/// potentially-unbounded `join()` calls.
///
/// When `take_scheduler` is true, the default `RuntimeInner` is detached from
/// its slot and returned to the caller, who drops it last (dropping the
/// scheduler — deques, parkers, stealers — as the final teardown step). The
/// scheduler is owned by the runtime, so detaching the runtime is what frees
/// the scheduler.
fn teardown_workers(
    scheduler: Option<*const Scheduler>,
    handles: Option<Vec<Option<JoinHandle<()>>>>,
    take_scheduler: bool,
) -> Option<Box<RuntimeInner>> {
    // Take a raw pointer (not `&'static Scheduler`): a reference argument is
    // strongly protected for the whole call, and the `take_scheduler` branch
    // below `Box::from_raw`-frees this same allocation. Freeing strongly-protected
    // memory is Stacked-Borrows UB, so the pre-free shutdown work uses only
    // short-lived references whose protectors end before the free.
    if let Some(sched_ptr) = scheduler {
        // SAFETY: caller guarantees the pointer is valid for this call; the
        // borrow is dropped before any free below.
        let sched = unsafe { &*sched_ptr };
        sched.shutdown.store(true, Ordering::Release);
        for parker in &sched.parkers {
            parker.cond.notify_one();
        }
    }

    let mut handles = handles.unwrap_or_else(|| {
        scheduler.map_or_else(Vec::new, |sched_ptr| {
            // SAFETY: as above; the borrow is confined to this closure.
            unsafe { &*sched_ptr }.worker_handles.access(std::mem::take)
        })
    });
    let current_id = thread::current().id();
    for handle in &mut handles {
        if let Some(ref h) = handle {
            if h.thread().id() == current_id {
                let _ = handle.take();
                continue;
            }
        }
        if let Some(h) = handle.take() {
            if h.join().is_err() {
                eprintln!("hew: scheduler worker thread panicked during shutdown");
            }
        }
    }

    if !take_scheduler {
        return None;
    }

    // Detach the owning runtime from its slot. Worker teardown above ensures no
    // thread can still access it; the caller drops the runtime last so the
    // scheduler's deques/parkers/stealers free as the final step.
    runtime::take_default()
}

/// Clean up after a worker spawn failure during initialisation.
///
/// Signals shutdown, joins all successfully-spawned workers, then detaches and
/// drops the default runtime (freeing its scheduler).
fn teardown_after_spawn_failure(handles: Vec<Option<JoinHandle<()>>>) {
    drop(teardown_workers(
        get_scheduler().map(|s| s as *const Scheduler),
        Some(handles),
        true,
    ));
}

/// Gracefully shut down the scheduler.
///
/// Sets the shutdown flag, wakes all parked workers, then joins every
/// worker thread. Safe to call if the scheduler was never initialized.
#[no_mangle]
pub extern "C" fn hew_sched_shutdown() {
    let Some(sched) = get_scheduler() else {
        return;
    };

    teardown_workers(Some(sched as *const Scheduler), None, false);

    // Write profile files on exit if HEW_PROF_OUTPUT is set.  Must run BEFORE
    // session_reset() so that the dispatch-type registry is still populated
    // and actor type labels appear correctly in the profile output.  If
    // session_reset() ran first it would clear DISPATCH_TYPE_REGISTRY and
    // every actor would fall back to "Actor" in the snapshot.
    crate::profiler::maybe_write_on_exit();

    // Fire all registered session reset hooks (tracing clear, profiler
    // dispatch-registry clear) after the exit profile has been written.
    // Workers are joined at this point so no concurrent actor activations can
    // race the hook callbacks.
    crate::session::session_reset();
}

/// Clean up all remaining runtime resources after shutdown.
///
/// Must be called after [`hew_sched_shutdown`] — all worker threads must
/// have been joined before this runs. Frees any actors that were not
/// explicitly freed by user code, clears the name registry, and drops
/// the scheduler itself (crossbeam deques, parkers, stealer handles).
///
/// In compiled Hew programs this is called automatically after the
/// scheduler shuts down. It is a no-op if the scheduler was never
/// initialized.
#[no_mangle]
pub extern "C" fn hew_runtime_cleanup() {
    // Stop the active-mode I/O reactor BEFORE any actors are freed. The reactor
    // thread delivers socket data into actor mailboxes; joining it here ensures
    // no in-flight readiness event can target an actor that cleanup_all_actors
    // is about to free (cleanup-all-exits / reactor shutdown ordering).
    crate::reactor::reactor_shutdown();

    // Stop the periodic ticker thread before any timer wheel memory is freed.
    // SAFETY: shutdown_ticker joins the thread, so no concurrent ticks after this.
    unsafe { crate::timer_periodic::hew_periodic_shutdown() };

    // Join lingering workers WITHOUT detaching the runtime yet (`take=false`).
    // The supervisor/actor/registry sweep below reads runtime-owned state
    // (the live-actor registry, deferred-teardown join handles), so the runtime
    // must stay installed in its slot until the sweep completes. Detaching it
    // here would make `rt_current()` trap mid-cleanup.
    teardown_workers(get_scheduler().map(|s| s as *const Scheduler), None, false);

    // Free any registered top-level supervisors — this drops their child
    // specs (names + init_state) via the InternalChildSpec Drop impl.
    // Workers are already joined so we cannot send stop messages; we just
    // drop the struct.
    // SAFETY: All workers have been joined by hew_sched_shutdown.
    unsafe { crate::shutdown::free_registered_supervisors() };

    // SAFETY: All workers have been joined by hew_sched_shutdown.
    unsafe { actor::cleanup_all_actors() };

    // Clear the name registry so no dangling pointers remain.
    crate::registry::hew_registry_clear();

    // SHIM: JIT reload must clear handler_names here; see #1234 Commit 1 rebase.
    // WHY: The `handler_names` side-table added by #1234 Commit 1 is part of
    //      the WASM bridge (bridge.rs MetaState), which is not compiled on
    //      native.  When a native equivalent arises, clearing it belongs here.
    // WHEN: Remove this marker if a native handler_names structure is introduced
    //       and needs teardown alongside the other native registry state.
    // REAL: `handler_names.clear()` adjacent to `hew_registry_clear()` above.

    // FINAL step: detach the runtime from its slot and drop it. Workers are
    // already joined (above) and the runtime-owned sweep is complete, so nothing
    // can still reference it. Dropping it frees the scheduler (deques, parkers,
    // stealers, global queue) and the now-empty live-actor registry.
    drop(runtime::take_default());
}

// ── Internal API ────────────────────────────────────────────────────────

/// Submit an actor to the global queue and wake a worker.
///
/// # Panics
///
/// Panics if the scheduler has not been initialized.
pub fn sched_enqueue(actor: *mut HewActor) {
    let sched = get_scheduler().expect("scheduler not initialized");
    TASKS_SPAWNED.fetch_add(1, Ordering::Relaxed);
    sched.global_queue.push(actor.cast::<()>());
    sched_try_wake();
}

/// Fail-closed wake-routing net (R4).
///
/// The scheduler wake path resolves its scheduler through [`get_scheduler`] →
/// [`runtime::rt_default`], **independent** of `rt_current()`, so an `enter()`
/// (or [`runtime::enter_actor_runtime`]) does NOT reroute the wake. Until M4
/// reroutes the wake to the actor's owning runtime, a wake for a non-default
/// actor would silently enqueue on the default runtime's scheduler — the same
/// silent-default hazard `enter_actor_runtime` traps on the send/teardown side.
///
/// This asserts the resolved (default) scheduler's runtime owns the actor about
/// to be enqueued, and traps otherwise (`no-fail-open-fallback-after-authority`:
/// assert-net first, reroute/delete at M4). A single-runtime
/// (`RuntimeId::DEFAULT`) actor never trips it — `rt_default()` IS its runtime —
/// so the production hot path is unaffected. Full rerouting of the wake to the
/// owning runtime's scheduler is M4; this lane only installs the trap.
#[inline]
fn assert_wake_routes_to_owning_runtime(actor_runtime_id: crate::runtime_id::RuntimeId) {
    if actor_runtime_id == crate::runtime_id::RuntimeId::DEFAULT {
        return;
    }
    let resolved = runtime::rt_default().map(RuntimeInner::runtime_id);
    assert_eq!(
        resolved,
        Some(actor_runtime_id),
        "hew-runtime: enqueue_resume: a wake for an actor owned by runtime {} would route \
         through the default scheduler (runtime {:?}); the scheduler wake path is not yet \
         rerouted to the owning runtime (M4) — failing closed rather than waking on a \
         foreign scheduler",
        actor_runtime_id.as_u64(),
        resolved.map(crate::runtime_id::RuntimeId::as_u64),
    );
}

/// Wake a `Suspended` actor whose parked continuation has become resumable.
///
/// This is the SINGLE resume edge every readiness source feeds: the seed/test
/// source today, and post-slice-4 the reactor (NEW-1), reply slot (NEW-3),
/// channels (NEW-4), and wire reply (NEW-5). It is the dual of `sched_enqueue`
/// for a parked-then-woken actor and is structurally identical to the
/// `Sleeping → Runnable` re-enqueue the wasm sleeper-drain already proves.
///
/// `cont` is the continuation the source has made resumable. If the actor's
/// resume slot is empty (the park has not finished publishing — the FG3
/// lost-wake window), the wake is RECORDED via `mark_pending_wake` and the
/// suspend edge drains it; the wake is never lost.
///
/// CAS discipline (fail-closed): the wake only enqueues on a successful
/// `Suspended → Runnable` CAS. If the actor is terminal (`Stopped`/`Crashed`)
/// or not yet `Suspended`, the CAS fails and the actor is NOT enqueued —
/// mirroring the `Idle → Runnable` waker discipline in `activate_actor`, which
/// closes the use-after-free window against a freed actor.
///
/// # Safety
///
/// `actor`, if non-null, may reference a freed `HewActor` — this function does
/// NOT trust the pointer to be live. It re-confirms liveness against the
/// `LIVE_ACTORS` registry under the registry lock before dereferencing, so a
/// stale pointer from an abandoned/late reply is rejected atomically rather than
/// dereferenced. `cont`, if non-null, must be the continuation parked on
/// `actor` (a `coro.begin` frame). The caller (a readiness source) owns the wake
/// edge; the executor owns teardown.
pub unsafe fn enqueue_resume(actor: *mut HewActor, cont: *mut c_void) {
    if actor.is_null() {
        return;
    }

    // W6.010 caller-actor UAF guard (S1). `enqueue_resume` is reached not only by
    // a live reply but also by the orphan-retire teardown
    // (`hew_reply_channel_retire_orphaned_ask_sender_ref`), which the CALLEE
    // mailbox runs during its own teardown. The reply channel stores a raw
    // `caller_actor` pointer that nothing nulls when the CALLER is freed, and
    // `cleanup_all_actors` frees actors in nondeterministic `HashMap` order — so
    // the caller box can already be freed when the callee teardown fires this
    // wake. Dereferencing `actor` directly would be a heap-use-after-free.
    //
    // `with_live_actor` makes the liveness check and the wake one atomic action:
    // it holds the `LIVE_ACTORS` registry lock across the closure, and EVERY free
    // path (`hew_actor_free_inner`, `drain_quiesced_actor`, `cleanup_all_actors`)
    // removes the actor from `LIVE_ACTORS` BEFORE reclaiming the box. So while the
    // closure runs the box cannot be freed, and if the caller was already torn
    // down the closure never runs (the pointer is no longer tracked) — the stale
    // wake is dropped, never dereferenced. The freed caller's continuation is
    // already destroyed by its own C1 teardown, so dropping the wake is correct.
    let enqueued = crate::lifetime::live_actors::with_live_actor(actor, |a| {
        // Capture the actor's owning-runtime id under the registry lock (the
        // actor is guaranteed live here). The R4 wake-routing net is asserted
        // AFTER `with_live_actor` returns and the lock is released (below), so a
        // trap never poisons the registry lock.
        let actor_runtime_id = a.runtime_id;
        // If the park has not yet stored a handle, the suspend edge is mid-park
        // (the FG3 window). Record the wake so the suspend edge re-enqueues; do
        // NOT store the handle ourselves (the suspend edge owns the slot write).
        let parked = a.suspended_cont.load(Ordering::Acquire);
        if parked.is_null() {
            crate::coro_exec::mark_pending_wake(a);
            // The actor is not yet `Suspended`; the CAS below would fail anyway.
            // Re-check after marking: if the park JUST finished publishing
            // `Suspended` between our load and the mark, fall through to the CAS
            // so the wake is delivered now rather than waiting on the suspend
            // edge's pending-wake drain. (Two-phase park, both directions.)
            if a.actor_state.load(Ordering::Acquire) != HewActorState::Suspended as i32 {
                let _ = cont; // handle is owned by the suspend edge; nothing to store.
                return (false, actor_runtime_id);
            }
        }

        // CAS Suspended → Runnable; only enqueue on success (fail-closed against
        // a terminal or not-yet-parked actor).
        if a.actor_state
            .compare_exchange(
                HewActorState::Suspended as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            (true, actor_runtime_id)
        } else {
            // The actor was not `Suspended` yet (park still completing) — record
            // the wake so the suspend edge observes it. Terminal actors also land
            // here; marking is harmless (the actor will never park again).
            crate::coro_exec::mark_pending_wake(a);
            (false, actor_runtime_id)
        }
    });

    // `sched_enqueue` pushes onto the global queue and wakes a worker; it does not
    // dereference the actor, so it is safe to run after dropping the registry lock
    // (the successful `Suspended → Runnable` CAS already latched the actor out of
    // any racing free path, exactly like the `Idle → Runnable` waker discipline).
    //
    // R4 wake-routing net: `sched_enqueue` resolves its scheduler through
    // `get_scheduler()` → `rt_default()`, which `enter()` does not reroute, so an
    // actor owned by a non-default runtime would wake on the WRONG scheduler.
    // Fail closed before the enqueue (single-runtime actors pass straight
    // through). The runtime id was captured under the registry lock above, so
    // this reads no freed memory.
    if let Some((true, actor_runtime_id)) = enqueued {
        assert_wake_routes_to_owning_runtime(actor_runtime_id);
        sched_enqueue(actor);
    }
}

/// Wake one parked worker.
///
/// Uses a round-robin counter to distribute wake-ups across workers,
/// avoiding always waking the same worker.
pub fn sched_try_wake() {
    static WAKE_COUNTER: AtomicU64 = AtomicU64::new(0);
    if let Some(sched) = get_scheduler() {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "modulo by worker_count keeps result within usize range"
        )]
        let idx =
            (WAKE_COUNTER.fetch_add(1, Ordering::Relaxed) % sched.worker_count as u64) as usize;
        crate::observe::record_scheduler_unpark();
        sched.parkers[idx].cond.notify_one();
    }
}

// ── Worker loop ─────────────────────────────────────────────────────────

/// `Send` carrier for the owning-runtime pointer threaded into a worker's
/// spawn closure.
///
/// A bare `*const RuntimeInner` is `!Send`, so it cannot cross the
/// `thread::spawn` boundary on its own. The pointer references the
/// `RuntimeInner` this worker is bound to (the one `hew_sched_init` just
/// installed); it is valid until `teardown_workers` joins this worker, which
/// `hew_runtime_cleanup` guarantees happens *before* `take_default` frees the
/// runtime — the same valid-until-join contract that already governs the
/// scheduler borrow (`get_scheduler()` below). No Arc or refcount is taken;
/// ownership of the runtime stays with the default slot.
#[derive(Clone, Copy)]
struct WorkerRuntimePtr(*const RuntimeInner);

// SAFETY: the pointer is only dereferenced on the worker thread (via `enter()`
// at worker entry) and the `RuntimeInner` it names outlives the worker by the
// join-before-free contract documented above. Crossing the spawn boundary is
// the sole reason for this wrapper.
unsafe impl Send for WorkerRuntimePtr {}

/// Main loop executed by each worker thread.
///
/// `rt` is the [`RuntimeInner`] that owns this worker. The worker `enter()`s it
/// for the loop's entire lifetime so every in-dispatch `rt_current()` read
/// (the live-actor registry, name registry, shutdown phase, timers, node slot)
/// resolves through the per-thread [`crate::runtime::CURRENT_RUNTIME`] slot
/// rather than the default-slot fallback. In single-runtime the entered runtime
/// equals the default, so behaviour is preserved; the value is the TLS path
/// being live and tsan-clean.
fn worker_loop(id: usize, rt: WorkerRuntimePtr, local: &WorkDeque) {
    // Production invariant: the default runtime pointer never changes after
    // hew_sched_init, so a missing scheduler here is a hard logic error — keep
    // the loud panic (the same contract as before this fix).
    let sched = get_scheduler().expect("scheduler not initialized");
    // Bind this thread to its owning runtime for the loop's lifetime. The guard
    // restores the previous (null) CURRENT_RUNTIME on every exit edge — normal
    // return, panic, or a longjmp/trap unwinding out of the loop — preserving
    // lifecycle-symmetry. The pointer is valid until this worker is joined (see
    // WorkerRuntimePtr); `enter()` takes no ownership.
    //
    // SAFETY: `rt.0` is the non-null `RuntimeInner` this worker was spawned
    // against, valid until join by the WorkerRuntimePtr contract — so it
    // outlives this guard (held for the loop body) and every `rt_current()`
    // deref taken through it, satisfying `enter`'s lifetime obligation.
    let _rt_guard = unsafe { runtime::enter(&*rt.0) };
    // In test builds, capture the raw default-runtime pointer this worker is
    // bound to. The NoWorkerSchedulerForTest harness can swap the runtime out
    // from under a late-starting worker; the per-iteration check below detects
    // that swap and drains cleanly before any freed pointer is dereferenced.
    // See NoWorkerSchedulerForTest::Drop for the matching spin-until-zero drain.
    #[cfg(test)]
    let rt_raw: *mut RuntimeInner = runtime::default_runtime_ptr(Ordering::Relaxed);
    let mut rng = Xorshift64::new(crate::deterministic::effective_worker_seed(id as u64));
    crate::observe::set_current_worker_shard(id);

    // Install per-worker signal stack, recovery context, and block async signals.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "worker count is bounded by HEW_MAX_WORKERS (256), well within u32 range"
    )]
    crate::signal::init_worker_recovery(id as u32);

    loop {
        // Test-only drain handshake. In production the default-runtime pointer
        // is never swapped during a worker's lifetime, so this entire block is
        // compiled away with zero hot-path overhead.
        #[cfg(test)]
        let _drain = {
            // Fast path: if the default-runtime pointer no longer matches the
            // one this worker bound to (NoWorkerSchedulerForTest::install/Drop
            // swapped it), exit before touching any scheduler field.
            if runtime::default_runtime_ptr(Ordering::Relaxed) != rt_raw {
                break;
            }
            // If we are bound to the installed transient runtime, register in
            // the process-static drain counter so the guard's Drop defers the
            // free until we leave the loop body. The statics are never freed, so
            // this registration is memory-safe even though `sched` may belong to
            // the transient about to be torn down.
            if SCHED_TEST_DRAIN_PTR.load(Ordering::SeqCst) == rt_raw {
                SCHED_TEST_DRAIN_COUNT.fetch_add(1, Ordering::SeqCst);
                // Re-read the runtime pointer *after* the increment. This SeqCst
                // load pairs with the SeqCst swap + counter load in Drop: if Drop
                // already swapped the transient out, back out (decrement) and
                // exit before dereferencing `sched`. The total-order guarantee
                // makes it impossible for both this load to miss the swap and
                // Drop's spin to miss our increment, so a worker still inside
                // the body is always reflected in the counter Drop waits on.
                if runtime::default_runtime_ptr(Ordering::SeqCst) != rt_raw {
                    SCHED_TEST_DRAIN_COUNT.fetch_sub(1, Ordering::SeqCst);
                    break;
                }
                Some(SchedDrainGuard)
            } else {
                None
            }
        };
        // The shutdown read sits AFTER the test-only drain guard above so that
        // EVERY scheduler-field access — including this loop-exit check — is
        // inside the guarded region. With the old `while` form the condition
        // read happened between iterations, after the previous guard had
        // decremented the drain counter, leaving a window where
        // NoWorkerSchedulerForTest::Drop could observe count==0 and free the
        // transient while this thread was about to read `shutdown` (ASan
        // heap-use-after-free, release-gate Sanitizers job). In production the
        // reordering is semantics-neutral: shutdown is still checked at the
        // top of every iteration before any work is taken.
        if sched.shutdown.load(Ordering::Acquire) {
            break;
        }
        // 1. Pop from local deque (LIFO — cache-friendly).
        if let Some(ptr) = local.pop() {
            activate_actor(ptr.cast::<HewActor>());
            continue;
        }

        // 2. Steal from a random peer.
        if let Some(actor) = try_steal_from_peers(sched, id, &mut rng) {
            activate_actor(actor);
            continue;
        }

        // 3. Try global queue (batch steal into local deque).
        if let Some(ptr) = sched.global_queue.steal_batch_and_pop(local) {
            activate_actor(ptr.cast::<HewActor>());
            continue;
        }

        // 4. Check if a signal-initiated shutdown needs to be started.
        crate::shutdown::check_signal_shutdown();

        // 5. Park on per-worker condvar until notified or timeout.
        let parker = &sched.parkers[id];
        let guard = parker.mutex.lock_or_recover();
        if sched.shutdown.load(Ordering::Acquire) {
            break;
        }
        crate::observe::record_scheduler_park();
        let _ = parker.cond.wait_timeout(guard, PARK_TIMEOUT);
    }
}

/// Try to steal an actor from a random peer worker's deque.
fn try_steal_from_peers(
    sched: &Scheduler,
    self_id: usize,
    rng: &mut Xorshift64,
) -> Option<*mut HewActor> {
    let n = sched.worker_count;
    if n <= 1 {
        return None;
    }

    #[expect(
        clippy::cast_possible_truncation,
        reason = "worker count is bounded by HEW_MAX_WORKERS (256), well within usize range"
    )]
    let start = (rng.next_u64() % n as u64) as usize;
    for i in 0..n {
        let victim = (start + i) % n;
        if victim == self_id {
            continue;
        }
        if let Some(ptr) = sched.stealers[victim].steal() {
            STEALS_TOTAL.fetch_add(1, Ordering::Relaxed);
            return Some(ptr.cast::<HewActor>());
        }
    }

    None
}

// ── Slice-4 suspend/resume executor edges ───────────────────────────────

/// The SUSPEND edge: park the actor's current continuation against a readiness
/// source and return to the worker WITHOUT re-enqueuing, freeing the worker to
/// run someone else. This is the executor's third dispatch outcome.
///
/// Ordering is load-bearing (the four footguns):
/// 1. Release the per-actor state lock (FG2 / R2 P0) so senders do not
///    deadlock while the actor is suspended. The lock is held across each
///    message dispatch; a suspend that returns between acquire and release MUST
///    release it here, exactly as the panic path releases on its crash edge.
/// 2. `begin_park` publishes the `Parked` tag (FG3 phase 1), BEFORE the handle
///    is stored. It does NOT clear `pending_wake`: a reply armed inside the
///    coroutine body can fire before this edge, so the wake flag is kept
///    monotonic within a cycle and consumed once by `take_pending_wake` at each
///    drain (the lost-wake fix; see `coro_exec::begin_park`).
/// 3. `finish_park` stores the handle (FG3 phase 2).
/// 4. CAS `Running → Suspended` so wakes can find the parked actor.
/// 5. Drain the FG3 lost-wake flag: a wake that fired in the park window
///    (between phase 1 and the published `Suspended`) set `pending_wake`; if so
///    we re-enqueue ourselves (the wake's own `Suspended → Runnable` CAS lost
///    the race), so the wake is observed exactly once and never lost.
///
/// # Safety
///
/// `actor` is owned by the calling activation frame (the Running CAS is held),
/// `cont` is the live, suspended continuation handle the dispatch produced. The
/// caller MUST have released the per-actor state lock before this call (the
/// dispatch loop's `hew_actor_state_lock_release_for_context` on the dispatch
/// return edge) so a suspended actor does not hold its lock against senders
/// (FG2 / R2 P0).
unsafe fn park_suspended_activation(actor: *mut HewActor, cont: *mut c_void) -> bool {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };

    // (1) Lock release is the caller's responsibility on the real dispatch
    // path (it holds the execution context); the seed/test path holds no lock.

    // (2) FG3 phase 1: publish the park intent before storing the handle.
    if !crate::coro_exec::begin_park(a).is_ok() {
        // P1-B: begin_park refused (tag not Empty/Resuming — e.g. a stale
        // Destroyed that the quiescent re-arm has not reached, or a corrupt
        // tag). We still OWN `cont` (the dispatch produced it and it was never
        // stored), so destroy it here rather than dropping it silently — a
        // dropped handle leaks the coro frame + any frame-owned heap values.
        // `hew_cont_destroy` is null-safe and runs the single cleanup outline.
        // SAFETY: `cont` is the live, not-yet-parked, not-yet-destroyed frame
        // this activation produced; no other owner exists (park never stored
        // it, so no resume/destroy edge can race it).
        unsafe { crate::cont::hew_cont_destroy(cont) };
        return false;
    }
    // (3) FG3 phase 2: store the handle.
    // SAFETY: `cont` is a live suspended continuation per the fn contract.
    unsafe { crate::coro_exec::finish_park(a, cont) };

    // (4) Publish `Suspended` so wakes can find us. CAS from Running; if it
    // fails the actor was concurrently stopped/crashed — undo the park.
    if a.actor_state
        .compare_exchange(
            HewActorState::Running as i32,
            HewActorState::Suspended as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        // The actor left Running underneath us (stop/crash). Destroy the parked
        // continuation exactly once rather than leaking the frame.
        // SAFETY: we still own the parked handle (no concurrent resume could
        // have started — the actor never reached Suspended).
        let _ = unsafe { crate::coro_exec::destroy_parked(a) };
        return false;
    }

    // (5) FG3: drain a wake that fired in the park window. If present, the
    // wake's own `Suspended → Runnable` CAS lost the race (we had not yet
    // published Suspended), so we re-enqueue ourselves to deliver it once.
    if crate::coro_exec::take_pending_wake(a)
        && a.actor_state
            .compare_exchange(
                HewActorState::Suspended as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
    {
        sched_enqueue(actor);
    }
    crate::observe::record_coroutine_suspend();
    crate::observe::hew_observe_probe_suspend(
        a.dispatch
            .map_or(std::ptr::null(), |f| f as *const std::ffi::c_void),
    );
    true
}

/// The RESUME re-entry: drive the actor's parked continuation to its next
/// suspend (or completion) and settle the activation.
///
/// - `ResumePoll::Pending` → the continuation suspended again. Re-park: CAS
///   `Running → Suspended` (the handle stays parked, tag already back to
///   `Parked`), and if a wake fired in the meantime drain it and re-enqueue.
///   The actor is left `Suspended`, awaiting the next wake.
/// - `ResumePoll::Ready` → the continuation completed. Destroy it exactly once
///   (FG1) — which nulls the slot in the same critical section (FG4) — then
///   fall through to the standard idle/requeue settle so queued messages are
///   still served.
/// - refused (`None`) → the slot was null or the tag was not resumable
///   (FG2/FG4). Treat as completed (nothing live to drive) and settle to idle.
///
/// # Safety
///
/// `actor` is owned by the calling activation frame (the Running CAS is held).
unsafe fn resume_suspended_activation(actor: *mut HewActor) {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };

    // W6.010 value routing: re-establish an execution context carrying the
    // handler's stashed reply channel (saved at park) BEFORE driving the resume,
    // so the resumed coroutine body's final-return `hew_reply` (via
    // `hew_get_reply_channel`) deposits the reply to the handler's caller. The
    // suspend tore down the original dispatch context; without this the body
    // would see no reply channel and the caller would hang (R1). The context is
    // a scheduler-owned stack carrier for the duration of the resume, restored
    // after (mirroring the fresh-dispatch carrier install/restore).
    let stashed_reply = a.suspended_reply_channel.load(Ordering::Acquire);
    let stashed_cancel_token = a.suspended_cancel_token.load(Ordering::Acquire);
    let mut resume_context = HewExecutionContext {
        actor,
        actor_id: a.id,
        parent_supervisor: a.supervisor,
        supervisor_child_index: a.supervisor_child_index,
        flags: 0,
        cancel_token: stashed_cancel_token.cast(),
        task_scope: std::ptr::null_mut(),
        arena: a.arena,
        trace: crate::tracing::HewTraceContext::default(),
        partition_policy: std::ptr::null_mut(),
        prev_context: crate::execution_context::current_context(),
        lock_seat: dispatch_lock_seat_for_actor(actor),
        reply_channel: stashed_reply,
    };
    let prev_context = resume_context.prev_context;
    let installed_prev = crate::execution_context::set_current_context(&raw mut resume_context);
    debug_assert_eq!(installed_prev, prev_context);

    // Crash recovery for the RESUME re-entry. The resumed continuation runs the
    // REST of a suspended handler — code that can `panic()` / hard-trap exactly
    // like a fresh dispatch (e.g. a handler that `sleep_ms`-suspends, resumes,
    // then crashes; or one that crashes after an `await` resumes). The fresh
    // dispatch path wraps its handler in a `sigsetjmp` frame (`activate_actor`),
    // but the resume drive had none: a trap here unwound past the worker frame
    // and downed the whole process instead of crashing only this actor. Install
    // the same recovery point in THIS stack frame (so the `jmp_buf` stays valid
    // for the resume) and route a trap to the crash branch below — fail-closed,
    // matching the actor-isolation contract `signal.rs` documents.
    //
    // SAFETY: `actor` is owned by this frame (Running CAS held) and stays valid
    // through the resume. No message node backs a resume, so pass null `msg`
    // (`prepare_dispatch_recovery` records `msg_type = 0` for it).
    let jmp_buf_ptr =
        unsafe { crate::signal::prepare_dispatch_recovery(actor, std::ptr::null_mut()) };
    let is_normal_path = if jmp_buf_ptr.is_null() {
        true
    } else {
        // SAFETY: `jmp_buf_ptr` is non-null (checked) and valid per-thread.
        let ret = unsafe { crate::signal::sigsetjmp(jmp_buf_ptr, 1) };
        if ret == 0 {
            crate::signal::mark_recovery_active();
            true
        } else {
            false
        }
    };

    if !is_normal_path {
        // A `panic()` / hard trap fired inside the resumed continuation and
        // longjmped back to the `sigsetjmp` above. The C stack (including the
        // `.resume` outline) is unwound; the coroutine never reached its next
        // suspend, so its tag is stuck at `Resuming` and the frame is live.
        // Recover exactly as the fresh-dispatch crash branch does, plus reclaim
        // the crash-abandoned coroutine frame (which `destroy_parked` refuses
        // for a `Resuming` tag — that refusal protects a LIVE resume, but this
        // one is provably dead).
        //
        // SAFETY: reached immediately after `sigsetjmp` returned non-zero on
        // this worker thread. `actor` is owned by this frame (Running CAS held);
        // `resume_context` is the live stack local installed above, still the
        // current execution context.
        unsafe { resume_crash_recovery(actor, &raw mut resume_context) };
        return;
    }

    crate::observe::record_coroutine_resume();
    // SAFETY: the parked handle is the executor-owned frame; `resume_park`
    // enforces FG2/FG4 internally (refuses a null slot or non-Parked tag).
    let poll = unsafe { crate::coro_exec::resume_park(a) };

    // Dispatch's resume step completed without a trap — clear the recovery point
    // so a later stale signal can't jump to this dead frame.
    crate::signal::clear_dispatch_recovery();

    // Restore the prior context now that the resume step (resume + poll, and any
    // body-side reply deposit it performed) has run. On a Ready completion the
    // body already deposited its reply; clear the stash so a re-armed multi-await
    // actor does not reuse a freed channel. On Pending the handler re-parked, so
    // the stash stays for the next resume.
    let restored = crate::execution_context::set_current_context(prev_context);
    debug_assert_eq!(restored, &raw mut resume_context);
    if matches!(poll, Some(crate::cont::ResumePoll::Ready) | None) {
        a.suspended_reply_channel
            .store(std::ptr::null_mut(), Ordering::Release);
        clear_suspended_cancel_token(a);
    }

    match poll {
        Some(crate::cont::ResumePoll::Pending) => {
            // Re-park: the continuation suspended again. CAS back to Suspended.
            if a.actor_state
                .compare_exchange(
                    HewActorState::Running as i32,
                    HewActorState::Suspended as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                crate::observe::record_coroutine_suspend();
                crate::observe::hew_observe_probe_suspend(
                    a.dispatch
                        .map_or(std::ptr::null(), |f| f as *const std::ffi::c_void),
                );
                // FG3: a wake during the resume window must not be lost.
                if crate::coro_exec::take_pending_wake(a)
                    && a.actor_state
                        .compare_exchange(
                            HewActorState::Suspended as i32,
                            HewActorState::Runnable as i32,
                            Ordering::AcqRel,
                            Ordering::Acquire,
                        )
                        .is_ok()
                {
                    sched_enqueue(actor);
                }
            } else {
                // Stopped/crashed under us — destroy the parked frame once.
                // SAFETY: no concurrent resume; we just observed Pending.
                let _ = unsafe { crate::coro_exec::destroy_parked(a) };
            }
        }
        Some(crate::cont::ResumePoll::Ready) | None => {
            // Completed (or refused: nothing live). Destroy exactly once (FG1),
            // which nulls the slot in the same critical section (FG4).
            // SAFETY: the tag is `Done` (Ready) or already terminal (None);
            // destroy_parked refuses a second teardown.
            let _ = unsafe { crate::coro_exec::destroy_parked(a) };
            // P1-B: the continuation is fully reclaimed and the slot is null
            // (FG4). Re-arm the tag `Destroyed → Empty` on this quiescent edge
            // so the SAME actor can park a NEW continuation on its next
            // `await` (multi-await). Fail-closed: `re_arm` only transitions a
            // Destroyed tag with a null slot, so a refusal here (the None
            // branch where nothing was ever parked) is harmless.
            let _ = crate::coro_exec::re_arm(a);
            // Settle: the resumed dispatch is finished. Mirror the post-loop
            // idle/requeue CAS so queued messages are served. Reset the arena
            // for the completed activation.
            if !a.arena.is_null() {
                // SAFETY: arena was created at spawn; no references survive.
                unsafe { crate::arena::hew_arena_reset(a.arena) };
            }
            settle_after_activation(actor, 0);
        }
    }
}

/// Crash recovery for a trap raised inside a RESUMED continuation.
///
/// Reached only when [`resume_suspended_activation`]'s `sigsetjmp` returns
/// non-zero — a `panic()` / hard trap fired inside the resumed handler and
/// longjmped back. Mirrors the fresh-dispatch crash branch in [`activate_actor`]
/// (lock release → swap unwind → reply routing → `Crashing` CAS → arena reset →
/// late crash-reply → terminal `Crashed`), with the resume-specific differences:
///   * the reply channel is read from the resume's installed context
///     (`resume_context`, carrying the handler's stashed reply channel), not a
///     mailbox node — a resume has no `msg` to free;
///   * the crash-abandoned coroutine frame (tag stuck at `Resuming` because the
///     longjmp skipped the settle) is reclaimed via
///     [`crate::coro_exec::abandon_resuming_after_crash`] BEFORE the actor frees,
///     since `destroy_parked` refuses a `Resuming` tag.
///
/// # Safety
///
/// Called immediately after `sigsetjmp` returned non-zero on this worker thread.
/// `actor` is owned by this frame (Running CAS held). `resume_context` is the
/// still-installed dispatch context (a live stack local in the caller frame);
/// the prior context is restored via `restore_current_context_after_dispatch`,
/// which walks `resume_context`'s `prev_context`.
unsafe fn resume_crash_recovery(actor: *mut HewActor, resume_context: *mut HewExecutionContext) {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };
    let actor_arena = a.arena;

    // Generated dispatch wrappers acquire the actor-state lock before the
    // handler body; the longjmp bypassed their cleanup edges, so release any
    // guard this dispatch held before the crash path notifies supervisors.
    // SAFETY: `actor` is the actor this frame is resuming; the release helper
    // tolerates an unheld/unregistered lock.
    unsafe {
        let _ = crate::actor::hew_actor_state_lock_release_after_panic(actor);
    }

    // A child suspending-closure call that trapped/longjmped inside the resume
    // bypassed the driver's swap-pop and driver-channel teardown. Restore the
    // outer reply routing and tear those channels down BEFORE reading the reply
    // channel below (mirrors the fresh-dispatch crash branch).
    crate::execution_context::reply_channel_swap_unwind();

    // Capture the crashed resume's reply-channel state from the still-installed
    // resume context (carrying the handler's stashed reply channel) before
    // restoring the previous context.
    let reply_consumed = current_reply_channel_consumed_on(resume_context);
    let crash_reply = clear_reply_channel_on(resume_context);
    restore_current_context_after_dispatch();

    // The resume's reply channel was a snapshot of the actor's stashed slot;
    // clear that slot + the stashed cancel token so a re-armed actor does not
    // reuse the freed channel/token (the crashed activation is terminal).
    a.suspended_reply_channel
        .store(std::ptr::null_mut(), Ordering::Release);
    clear_suspended_cancel_token(a);

    // Transition `Running → Crashing` (accepting `Stopping` too, as a pending
    // self-stop is dominated by the crash) BEFORE any publication so a waiter in
    // `hew_actor_free` cannot observe the actor terminal and free the arena out
    // from under this worker. Fails only if already terminal / a state this
    // worker did not set.
    let took_crashing = loop {
        let cur = a.actor_state.load(Ordering::Acquire);
        if cur != HewActorState::Running as i32 && cur != HewActorState::Stopping as i32 {
            break false;
        }
        if a.actor_state
            .compare_exchange(
                cur,
                HewActorState::Crashing as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            break true;
        }
    };

    // Reclaim the crash-abandoned coroutine frame (tag stuck at `Resuming`)
    // exactly once, while `Crashing` keeps the actor non-quiescent so the box
    // stays alive. Frees the frame block WITHOUT running the `coro.destroy`
    // cleanup outline — the coroutine was RUNNING (between suspend points) when
    // it trapped, so re-running the last suspend's cleanup would double-free the
    // registrations its resume edge already released. Frame-owned Hew heap values
    // are arena-backed and reclaimed by the arena reset below.
    // SAFETY: the longjmp killed the resume, so no concurrent resume/destroy can
    // run; this worker owns the actor exclusively.
    let _ = unsafe { crate::coro_exec::abandon_resuming_after_crash(a) };

    // Per-activation arena cleanup BEFORE publishing terminal `Crashed`.
    if took_crashing && !actor_arena.is_null() {
        // SAFETY: arena was created at spawn; the crash discards all in-flight
        // data, and `Crashing` prevents `hew_actor_free` from reclaiming it
        // ahead of us.
        unsafe { crate::arena::hew_arena_reset(actor_arena) };
    }

    // If the handler did not already deposit its reply, send the empty crash
    // fallback so the waiting `hew_actor_ask` caller resolves to `Err` rather
    // than deadlocking. Done before publishing `Crashed` so the channel-count
    // invariant is observable as soon as the actor is terminal.
    if !reply_consumed && !crash_reply.is_null() {
        // SAFETY: `crash_reply` is a valid `HewReplyChannel` pointer.
        unsafe {
            let _ = crate::reply_channel::hew_reply(crash_reply.cast(), std::ptr::null_mut(), 0);
        }
    }

    // Publish terminal `Crashed` and run supervisor / link / monitor
    // notifications. `handle_crash_recovery` invokes `hew_actor_trap` which
    // CAS-transitions `Crashing → Crashed`.
    if took_crashing {
        // SAFETY: called immediately after `sigsetjmp` returned non-zero on this
        // worker; per-activation cleanup (frame reclaim, arena reset, late
        // crash-reply) has already run.
        unsafe { crate::signal::handle_crash_recovery() };
    } else {
        // An external trap already published a terminal state during the resume;
        // do not re-run `handle_crash_recovery` (it would walk a possibly-freed
        // `current_actor`). Just invalidate the jmp_buf.
        crate::signal::clear_dispatch_recovery();
    }
}

/// Shared post-activation settle: CAS `Running → Runnable` (re-enqueue) when
/// the mailbox still has work, else `Running → Idle` with the standard
/// idle→runnable / idle→stopped rechecks. Factored out so the resume re-entry
/// and the message loop share one settle path.
fn settle_after_activation(actor: *mut HewActor, msgs_processed: u32) {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };
    let mailbox = a.mailbox.cast::<HewMailbox>();

    let cur_state = a.actor_state.load(Ordering::Acquire);
    if cur_state == HewActorState::Stopping as i32 {
        if a.actor_state
            .compare_exchange(
                HewActorState::Stopping as i32,
                HewActorState::Stopped as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
            crate::actor_group::notify_actor_death(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            unsafe { crate::actor::call_terminate_fn(actor) };
        }
        return;
    }
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    actor::update_hibernation_state(a, msgs_processed);

    let has_more = if mailbox.is_null() {
        false
    } else {
        // SAFETY: mailbox pointer is valid for the actor's lifetime.
        unsafe { hew_mailbox_has_messages(mailbox) != 0 }
    };

    if has_more {
        if a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            sched_enqueue(actor);
        }
    } else if a
        .actor_state
        .compare_exchange(
            HewActorState::Running as i32,
            HewActorState::Idle as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid for the actor's lifetime.
            && unsafe { hew_mailbox_has_messages(mailbox) != 0 }
        {
            if a.actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                sched_enqueue(actor);
            }
        } else if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid for the actor's lifetime.
            && unsafe { mailbox::mailbox_is_closed(mailbox) }
            && a.actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Stopped as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
            crate::actor_group::notify_actor_death(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            unsafe { crate::actor::call_terminate_fn(actor) };
        }
    }
}

// ── Actor activation ────────────────────────────────────────────────────

/// RAII marker that an `activate_actor` frame owns an actor's in-flight
/// activation. Set `dispatch_active = true` *before* the worker attempts the
/// `Runnable -> Running` CAS and clears it on EVERY exit of the activation
/// (settle, suspend-park, crash-break, normal fall-through, or a lost CAS).
///
/// The flag exists because an external `hew_actor_trap` CAS-es the actor
/// straight to the quiescent `Crashed`/`Stopped` state out from under this
/// worker — erasing the `Running` state while the worker is still reading
/// `a.actor_state`, the arena, and the mailbox in its post-dispatch settle. The
/// async free that the trap's supervisor-notify ultimately drives
/// (`hew_actor_free`) treats `Crashed`/`Stopped` as quiescent and would reclaim
/// the box + mailbox in that window — a use-after-free that surfaces as a
/// corrupted msg-node tripping the envelope-mode dispatch guard. `hew_actor_free`
/// additionally waits on this flag, so it never frees under an in-flight
/// activation regardless of which thread published the terminal state.
///
/// CAS-gap ordering (why claim runs BEFORE the CAS, not after): a trap can only
/// flip a `Running` actor terminal. If the flag were set *after* a successful
/// `Runnable -> Running` CAS, the actor would be `Running` and trap-stealable
/// for the instructions between the CAS and the set, during which the flag
/// still reads `false`. A trap landing there flips `Running -> Crashed`, and a
/// free observing `Crashed && !dispatch_active` reclaims the box before the
/// worker sets the flag and continues reading the (now freed) box — UAF.
/// Claiming the flag *before* the CAS guarantees it is already `true` the
/// instant the actor can become `Running`, so the trap-stealable window is
/// never flag-`false`. On a lost CAS the worker is not the owner, so the guard
/// is dropped on the early-return path, clearing the flag.
///
/// No-clobber: a single actor is enqueued at most once per `Runnable` epoch
/// (every `X -> Runnable` transition is CAS-gated and only the winner
/// `sched_enqueue`s) and the Chase-Lev deque hands each queued pointer to
/// exactly one worker, so at most one `activate_actor` frame runs per actor at a
/// time. A worker that loses the `Runnable -> Running` CAS therefore has no
/// concurrent winner whose flag its clear could erase.
struct ActivationOwnership<'a> {
    flag: &'a std::sync::atomic::AtomicBool,
}

impl<'a> ActivationOwnership<'a> {
    /// Mark the activation owned. Call BEFORE the `Runnable -> Running` CAS so
    /// the flag is already published when the actor first becomes `Running`.
    fn claim(flag: &'a std::sync::atomic::AtomicBool) -> Self {
        flag.store(true, Ordering::Release);
        Self { flag }
    }
}

impl Drop for ActivationOwnership<'_> {
    fn drop(&mut self) {
        // Release so a free path that subsequently observes the cleared flag
        // also observes every write this activation made to the actor box.
        self.flag.store(false, Ordering::Release);
    }
}

/// Activate an actor: CAS state to `Running`, drain messages up to budget,
/// then transition back to `Idle` or re-enqueue as `Runnable`.
#[expect(
    clippy::too_many_lines,
    reason = "actor activation state machine with multiple CAS transitions"
)]
fn activate_actor(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: the dequeued pointer references a live actor box. `hew_actor_free`
    // latches the actor out of `Idle` into the `Stopped` terminal state *before*
    // `untrack_actor` (after detaching the reactor and scrubbing links/monitors).
    // Once `Stopped`, every waker's `CAS Idle->Runnable` fails, so no reactor,
    // link/monitor, or direct-send wake can enqueue the actor in the window
    // between that latch and untrack+free — closing the use-after-free class. (A
    // consumer-side LIVE_ACTORS membership check was considered but rejected: it
    // cannot defeat the untrack-before-free ABA window — a reused address can
    // pass a bare membership check — and it would add a registry-lock acquisition
    // to every activation. The producer-side latch is the cheaper, complete fix.)
    let a = unsafe { &*actor };

    // Skip terminal states.
    let state = a.actor_state.load(Ordering::Acquire);
    if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
        return;
    }

    // Mark the activation owned BEFORE the CAS so `dispatch_active` is already
    // published the instant this actor can become `Running` and thus
    // trap-stealable. A trap can only flip a `Running` actor terminal; if the
    // flag were claimed *after* a winning CAS, the actor would be `Running`
    // with the flag still `false` for the instructions in between, and a trap
    // landing there (`Running -> Crashed`) plus an async free observing
    // `Crashed && !dispatch_active` would reclaim the box out from under this
    // still-running worker — the CAS->marker-gap UAF. Held until every exit
    // below (settle / suspend-park / crash-break / fall-through) so the async
    // free path cannot reclaim the actor box while this worker is still reading
    // it. See `ActivationOwnership`.
    let activation_ownership = ActivationOwnership::claim(&a.dispatch_active);

    // CAS: RUNNABLE → RUNNING.
    if a.actor_state
        .compare_exchange(
            HewActorState::Runnable as i32,
            HewActorState::Running as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        // Lost the CAS: this worker never owned the activation (the actor was
        // already terminal / claimed / no longer `Runnable`). Clear the flag we
        // optimistically set — no concurrent winner exists to clobber (single
        // dispatch per actor; see `ActivationOwnership`), so this restores the
        // flag to the `false` a non-owning worker must leave behind.
        drop(activation_ownership);
        return;
    }

    // The CAS won: keep ownership for the rest of this frame.
    let _activation_ownership = activation_ownership;

    // Test-only rendezvous at the exact CAS->marker-gap location: the actor is
    // now `Running` and trap-stealable. A regression test fires an external trap
    // HERE and asserts `dispatch_active` is already set (claimed before the CAS),
    // so a concurrent free refuses to reclaim the box under this worker.
    #[cfg(test)]
    run_activate_post_cas_hook(actor);

    // Resume re-entry (slice-4 executor). This activation may be a resumed
    // continuation rather than a fresh message dispatch. The discriminator —
    // carried by the suspend edge (commit 2/3) and verified before any branch
    // keys off it (R2) — is a live parked continuation: `cont_tag == Parked`
    // AND a non-null `suspended_cont` slot. `enqueue_resume` woke us
    // (Suspended → Runnable) and we just CAS'd Runnable → Running, so if the
    // discriminator is present this is a resume. Drive the continuation to its
    // next suspend (or completion) instead of draining the mailbox from
    // scratch; on completion destroy it exactly once and fall through to the
    // normal requeue/idle CAS so any queued messages are still served.
    if crate::coro_exec::has_live_parked_cont(a) {
        // SAFETY: `actor` is owned by this frame (we hold the Running CAS); the
        // parked handle is the executor-owned frame the suspend edge stored.
        unsafe { resume_suspended_activation(actor) };
        // After a resume we fall through to the standard post-loop requeue/idle
        // logic below (which reads `cur_state` etc.). A still-suspended
        // continuation re-parked itself inside `resume_suspended_activation`
        // and already returned the actor to `Suspended` + broke out — handled
        // by the early return there.
        return;
    }

    let base_budget = {
        let b = a.budget.load(Ordering::Relaxed);
        if b > 0 {
            b
        } else {
            HEW_MSG_BUDGET
        }
    };
    // Scale budget by priority: high (0) = 2×, normal (1) = 1×, low (2) = ½×.
    let budget = match a.priority.load(Ordering::Relaxed) {
        actor::HEW_PRIORITY_HIGH => base_budget.saturating_mul(2),
        actor::HEW_PRIORITY_LOW => (base_budget / 2).max(1),
        _ => base_budget,
    };
    let mailbox = a.mailbox.cast::<HewMailbox>();
    // Cache arena pointer before dispatch — the actor may be freed by a
    // supervisor on another worker during crash recovery, making `a.arena`
    // a dangling read.
    let actor_arena = a.arena;

    let _activation_metrics = ActivationMetricsGuard::new();

    let mut msgs_processed: u32 = 0;
    let mut crashed = false;

    if !mailbox.is_null() {
        // Process up to `budget` messages.
        for _ in 0..budget {
            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            let msg = unsafe { hew_mailbox_try_recv(mailbox) };
            if msg.is_null() {
                break;
            }

            // Dispatch the message (with profiling and crash recovery).
            if let Some(dispatch) = a.dispatch {
                let t0 = std::time::Instant::now();
                // SAFETY: `msg` is exclusively owned by this worker.
                let msg_ref = unsafe { &*msg };
                let observe_dispatch_ticket = crate::observe::observe_dispatch_begin();
                // Prepare crash recovery context (stores actor/msg metadata).
                //
                // SAFETY: `actor` is valid (CAS succeeded, we own it) and
                // will remain valid through dispatch. `msg` is a valid
                // HewMsgNode from hew_mailbox_try_recv.
                let jmp_buf_ptr =
                    unsafe { crate::signal::prepare_dispatch_recovery(actor, msg.cast()) };

                // Call sigsetjmp in THIS stack frame (activate_actor) so the
                // jmp_buf remains valid for the entire dispatch. sigsetjmp
                // returns 0 on initial call, non-zero after siglongjmp.
                //
                // SAFETY: jmp_buf_ptr is either null (no crash protection)
                // or a valid pointer to the per-thread recovery context.
                let is_normal_path = if jmp_buf_ptr.is_null() {
                    true
                } else {
                    // SAFETY: jmp_buf_ptr is non-null (checked above) and valid per-thread.
                    let ret = unsafe { crate::signal::sigsetjmp(jmp_buf_ptr, 1) };
                    if ret == 0 {
                        crate::signal::mark_recovery_active();
                        true
                    } else {
                        false
                    }
                };

                if is_normal_path {
                    // Check for injected crash fault (testing only).
                    if crate::deterministic::check_crash_fault(a.id) {
                        // Simulate a crash: use hew_actor_trap to trigger
                        // the full crash path (link propagation, monitor
                        // notification, supervisor restart).
                        crate::signal::clear_dispatch_recovery();
                        crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                        // SAFETY: `actor` is valid — we hold it via CAS.
                        unsafe { crate::actor::hew_actor_trap(actor, -1) };
                        // SAFETY: `msg` is exclusively owned by this worker.
                        unsafe { hew_msg_node_free(msg) };
                        crate::crash::record_injected_crash(a.id);
                        crashed = true;
                        break;
                    }

                    // Check for injected delay fault (testing only).
                    let delay_ms = crate::deterministic::check_delay_fault(a.id);

                    // Reset reduction counter for this dispatch.
                    a.reductions
                        .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

                    // The reply channel travels with the dispatch carrier
                    // (`execution_context.reply_channel` below) so that
                    // `hew_get_reply_channel` reads sole-authoritatively from
                    // the currently-installed context. Nested dispatch is
                    // restored via `prev_context`.

                    // Phase α COW: envelope-aware dispatch.  Legacy
                    // (copy-mode) nodes carry payload bytes in
                    // `data`/`data_size` and dispatch by value.
                    // Envelope-mode (aliased) nodes hold a refcounted
                    // `HewMsgEnvelope`; their receive ABI is borrow-only
                    // and does not exist yet (P5.2). Branch on the
                    // discriminator: copy-mode dispatches; envelope-mode
                    // fails closed (see the guard below) rather than
                    // double-dropping a payload through the owned-value
                    // handler.
                    let (dispatch_data, dispatch_size) = if msg_ref.envelope.is_null() {
                        (msg_ref.data, msg_ref.data_size)
                    } else {
                        // FAIL-CLOSED (P5.3): an envelope-mode (aliased)
                        // node has reached the *owned-value* dispatch ABI.
                        //
                        // Under the D355 borrow model the receiver of an
                        // aliased message must BORROW the payload read-only
                        // (via `hew_msg_envelope_payload_ptr`); the single
                        // final `drop_glue` is owned by the envelope and run
                        // exactly once by `hew_msg_envelope_release` when the
                        // node is freed. `dispatch` below is the ordinary
                        // owned-value handler trampoline — handing a
                        // destructor-bearing payload (String / Vec / Arc) to
                        // it *by value* would drop it once in the handler AND
                        // again in `hew_msg_envelope_release`: a double-free /
                        // use-after-free.
                        //
                        // No compiled program can reach this branch yet:
                        // codegen alias lowering is a no-op until P5.2 adds
                        // the borrow-only receive ABI (and an exactly-once-
                        // drop ASan e2e). This guard exists so that FFI /
                        // embedding misuse — anything that hand-builds an
                        // envelope-mode node and feeds it to the scheduler
                        // before that ABI exists — fails loudly instead of
                        // corrupting memory. P5.2 removes this guard ONLY
                        // when it lands the borrow-only receive lowering.
                        //
                        // This is the live boundary for the aliased-send gate
                        // (moved here from the send path in P5.3): the send /
                        // enqueue / release machinery is fully exercised, but
                        // owned-value *dispatch* of an envelope node is
                        // refused. Hard fail (`hew_panic`), never a
                        // `debug_assert` — release builds must fail closed too.
                        eprintln!(
                            "fatal: envelope-mode (aliased) message reached owned-value \
                             dispatch before the borrow-only receive ABI exists (P5.2); \
                             refusing to double-drop"
                        );
                        crate::actor::hew_panic();
                        // `hew_panic` never returns (longjmps to the scheduler
                        // crash frame, or exits the process when no recovery
                        // context is installed). Diverge to satisfy the type.
                        unreachable!("hew_panic returned from the envelope-mode dispatch guard");
                    };

                    let mut execution_context = HewExecutionContext {
                        actor,
                        actor_id: a.id,
                        parent_supervisor: a.supervisor,
                        supervisor_child_index: a.supervisor_child_index,
                        flags: 0,
                        cancel_token: std::ptr::null_mut(),
                        task_scope: std::ptr::null_mut(),
                        arena: a.arena,
                        trace: msg_ref.trace_context,
                        partition_policy: std::ptr::null_mut(),
                        prev_context: crate::execution_context::current_context(),
                        lock_seat: dispatch_lock_seat_for_actor(actor),
                        reply_channel: msg_ref.reply_channel,
                    };
                    let prev_context = execution_context.prev_context;
                    // Publish a single raw pointer to the dispatch-local context
                    // and thread it through every subsequent use. Re-borrowing the
                    // local with `&raw mut` is fine (SharedReadWrite tags coexist),
                    // but capturing it by `&mut` — as
                    // `catch_unwind(AssertUnwindSafe(|| dispatch(&raw mut
                    // execution_context, …)))` previously did — issues a Unique
                    // retag that invalidates the pointer already stored in the
                    // thread-local context slot. The post-dispatch `hew_trace_end`
                    // read of that slot was then Stacked-Borrows UB (Miri:
                    // tracing.rs `(*ctx).trace`). Threading one raw pointer avoids
                    // any further borrow of the local, so the published pointer
                    // stays valid for the whole dispatch.
                    let ec_ptr: *mut HewExecutionContext = &raw mut execution_context;
                    let installed_prev = crate::execution_context::set_current_context(ec_ptr);
                    debug_assert_eq!(installed_prev, prev_context);
                    crate::tracing::hew_trace_begin(a.id, msg_ref.msg_type);

                    // SAFETY: `execution_context` is the scheduler-owned stack
                    // context for this dispatch and its lock seat came from the
                    // actor's registered sidecar. The helper fails closed when
                    // the seat is absent or poisoned.
                    let lock_acquired =
                        unsafe { crate::actor::hew_actor_state_lock_acquire_for_context(ec_ptr) }
                            == crate::actor::HEW_ACTOR_STATE_LOCK_OK;
                    if !lock_acquired {
                        // Refuse to enter the handler without the per-actor lock.
                        // SAFETY: `actor` is the actor currently owned by this
                        // scheduler frame.
                        unsafe {
                            crate::actor::hew_actor_trap(
                                actor,
                                crate::actor::HEW_ACTOR_STATE_LOCK_ERR,
                            );
                        }
                        crate::signal::clear_dispatch_recovery();
                        crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                        // Read reply-channel state from the dispatch ctx
                        // BEFORE restoring `prev_context`, so the values come
                        // from this dispatch's frame.
                        let reply_consumed = current_reply_channel_consumed_on(ec_ptr);
                        let crash_reply = clear_reply_channel_on(ec_ptr);
                        let restored_context =
                            crate::execution_context::set_current_context(prev_context);
                        debug_assert_eq!(restored_context, ec_ptr);

                        if !reply_consumed && !crash_reply.is_null() {
                            // SAFETY: crash_reply is a valid HewReplyChannel pointer.
                            unsafe {
                                let _ = crate::reply_channel::hew_reply(
                                    crash_reply.cast(),
                                    std::ptr::null_mut(),
                                    0,
                                );
                            }
                        }
                        // SAFETY: msg is exclusively owned by this worker.
                        unsafe {
                            (*msg).reply_channel = std::ptr::null_mut();
                            hew_msg_node_free(msg);
                        }
                        crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                        crashed = true;
                        break;
                    }

                    // SAFETY: `dispatch`, `ctx`, and `a.state` are valid;
                    // message fields come from a well-formed `HewMsgNode`.
                    //
                    // D-A.2 (R326/R327): the trampoline returns the dispatch
                    // suspend outcome as a nullable continuation handle — `null`
                    // for a run-to-completion handler (every handler today; the
                    // suspend substrate is dormant), or the `coro.begin` handle
                    // when a handler suspended. The handle is captured here; the
                    // production park edge (commit 4) consumes a non-null handle
                    // to park the activation.
                    let dispatch_result = catch_unwind(AssertUnwindSafe(|| unsafe {
                        dispatch(
                            ec_ptr,
                            a.state,
                            msg_ref.msg_type,
                            dispatch_data,
                            dispatch_size,
                            // P5-RX sub-stage 1: copy-mode receipt only. Only
                            // copy-mode nodes (`msg_ref.envelope.is_null()`)
                            // reach this dispatch — envelope-mode nodes fail
                            // closed at the guard above before this point — so
                            // borrow_mode is unconditionally 0 here. The live
                            // envelope-mode receipt (passing 1 + the envelope
                            // pointer as `dispatch_data`) lands with guard
                            // removal in a later sub-stage.
                            0,
                        )
                    }));

                    // SAFETY: `execution_context.lock_seat` was initialized from the
                    // live actor immediately before the matching acquire.
                    let release_result =
                        unsafe { crate::actor::hew_actor_state_lock_release_for_context(ec_ptr) };
                    if release_result != crate::actor::HEW_ACTOR_STATE_LOCK_OK {
                        // SAFETY: `actor` is the actor currently owned by this
                        // scheduler frame.
                        unsafe {
                            crate::actor::hew_actor_trap(
                                actor,
                                crate::actor::HEW_ACTOR_STATE_LOCK_ERR,
                            );
                        }
                        crate::signal::clear_dispatch_recovery();
                        crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                        let _ = clear_reply_channel_on(ec_ptr);
                        let restored_context =
                            crate::execution_context::set_current_context(prev_context);
                        debug_assert_eq!(restored_context, ec_ptr);
                        // SAFETY: msg is exclusively owned by this worker.
                        unsafe {
                            (*msg).reply_channel = std::ptr::null_mut();
                            hew_msg_node_free(msg);
                        }
                        crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                        crashed = true;
                        break;
                    }

                    // D-A.2: the suspend handle the trampoline returned. `null`
                    // on the run-to-completion path (every handler today — no
                    // source construct produces a `coro.suspend` while the
                    // substrate is dormant); a non-null handle is parked on the
                    // suspend edge below.
                    let suspend_handle: *mut c_void = if let Ok(handle) = &dispatch_result {
                        *handle
                    } else {
                        set_last_error("actor dispatch panicked");
                        std::ptr::null_mut()
                    };

                    // W6.010 value routing: a suspending handler still owes a
                    // reply to ITS caller. Stash this dispatch's reply channel on
                    // the actor BEFORE the context/msg reply-channel teardown
                    // below clears it, so the resume edge can re-establish a
                    // context carrying it and the resumed coroutine body deposits
                    // the reply (the body, not the unwound trampoline frame, owns
                    // the deposit — the trampoline's out-slot is dead by resume).
                    // The channel reference is transferred to the actor slot: the
                    // suspend path below skips the normal reply teardown so the
                    // channel is NOT freed here; the resume edge consumes it.
                    if !suspend_handle.is_null() {
                        a.suspended_reply_channel
                            .store(msg_ref.reply_channel, Ordering::Release);
                        // SAFETY: `ec_ptr` points at the live dispatch-local
                        // context; reading `cancel_token` through it avoids
                        // re-borrowing the local (which would Unique-retag and
                        // invalidate the published thread-local pointer).
                        let cancel_token = unsafe { (*ec_ptr).cancel_token };
                        stash_suspended_cancel_token(a, cancel_token.cast());
                    }

                    // SEC-2 sibling edge: a Rust unwind caught by `catch_unwind`
                    // (`dispatch_result` is `Err`) that crossed a child
                    // suspending-closure ramp / `hew_cont_resume` bypassed the
                    // codegen swap-pop and driver-channel teardown, leaving the
                    // outer context pointed at the driver channel with the driver
                    // channel's refs live. Restore the outer reply routing and
                    // tear those channels down BEFORE the normal teardown below
                    // reads `current_reply_channel_consumed_on` / clears the reply
                    // channel — otherwise it reads and nulls the wrong channel and
                    // corrupts the outer ask routing (mirrors the native crash
                    // branch). No-double-free: this fires only while a swap is
                    // still open; the normal-return (`Ok`) edge already popped its
                    // own frames via the codegen swap-pop, so the swap stack is
                    // empty there and this is intentionally skipped. The child
                    // never deposited (it unwound), so the unwind releases both the
                    // retained sender ref and the creator ref, matching the codegen
                    // abandon path.
                    if dispatch_result.is_err() {
                        crate::execution_context::reply_channel_swap_unwind();
                    }

                    let reply_consumed = current_reply_channel_consumed_on(ec_ptr);
                    let _ = clear_reply_channel_on(ec_ptr);

                    // Preserve the mailbox-owned reply sender only for teardown
                    // states so hew_msg_node_free can publish the self-stop
                    // fallback reply. Ordinary no-reply returns must keep the
                    // prior pending-ask behavior instead of resolving early.
                    let actor_state = a.actor_state.load(Ordering::Acquire);
                    if reply_consumed
                        || (actor_state != HewActorState::Stopping as i32
                            && actor_state != HewActorState::Stopped as i32)
                    {
                        // SAFETY: msg is exclusively owned by this worker.
                        unsafe { (*msg).reply_channel = std::ptr::null_mut() };
                    }

                    // Dispatch completed successfully — clear recovery point.
                    crate::signal::clear_dispatch_recovery();
                    crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                    let restored_context =
                        crate::execution_context::set_current_context(prev_context);
                    debug_assert_eq!(restored_context, ec_ptr);

                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "single message dispatch will never exceed u64::MAX nanoseconds"
                    )]
                    let elapsed_ns = t0.elapsed().as_nanos() as u64;
                    msgs_processed += 1;
                    a.prof_messages_processed.fetch_add(1, Ordering::Relaxed);
                    a.prof_processing_time_ns
                        .fetch_add(elapsed_ns, Ordering::Relaxed);
                    crate::observe::record_actor_turn(elapsed_ns);
                    crate::observe::hew_observe_probe_turn(
                        a.dispatch
                            .map_or(std::ptr::null(), |f| f as *const std::ffi::c_void),
                        msg_ref.msg_type,
                        elapsed_ns,
                    );
                    crate::observe::observe_dispatch_attributed(observe_dispatch_ticket);

                    // SAFETY: `msg` was returned by `hew_mailbox_try_recv` and is
                    // now exclusively owned by this worker.
                    unsafe { hew_msg_node_free(msg) };

                    // SUSPEND EDGE (D-A.2 / R326/R327): the handler suspended at a
                    // non-final `coro.suspend` and handed back its `coro.begin`
                    // frame handle. Park it against the executor and break out of
                    // the message loop WITHOUT re-enqueuing — the worker is freed
                    // to run other actors; a wake (`enqueue_resume`) later
                    // re-enqueues this actor and the resume re-entry drives the
                    // parked continuation. The per-actor lock was already released
                    // on the dispatch-return edge above (FG2). `park_suspended_activation`
                    // publishes `Parked` + stores the handle + CAS `Running → Suspended`
                    // and drains a lost wake (FG3). Dormant today: no handler
                    // returns a non-null handle (no source produces a suspend).
                    if !suspend_handle.is_null() {
                        // SAFETY: `actor` is owned by this frame (Running CAS held);
                        // `suspend_handle` is the live, suspended continuation the
                        // dispatch produced; the lock is released.
                        let parked = unsafe { park_suspended_activation(actor, suspend_handle) };
                        if parked {
                            // Parked: the actor is now `Suspended` (or was
                            // re-enqueued by a lost-wake drain). Do not requeue
                            // or settle here — the resume re-entry owns the rest
                            // of this activation's lifecycle.
                            return;
                        }
                        clear_suspended_cancel_token(a);
                        // Park refused (actor concurrently stopped/crashed): the
                        // handle was destroyed once inside the park guard. Fall
                        // through to the standard settle so the terminal state is
                        // honoured.
                    }

                    // Apply injected delay after dispatch (testing only).
                    if delay_ms > 0 {
                        std::thread::sleep(Duration::from_millis(u64::from(delay_ms)));
                    }
                } else {
                    // Recovered from a crash signal (SEGV/BUS/FPE/ILL).
                    // handle_crash_recovery marks the actor as Crashed and
                    // logs the crash to stderr.
                    //
                    // Generated actor dispatch wrappers acquire the actor-state
                    // lock before entering the handler body. Signal recovery
                    // jumps back into this scheduler frame and bypasses those
                    // wrapper cleanup edges, so release any guard held by this
                    // dispatch before the crash path can notify supervisors.
                    // SAFETY: `actor` is the actor currently being dispatched
                    // by this scheduler frame; the release helper tolerates an
                    // unheld or unregistered lock because not every dispatch
                    // callback is compiler-generated.
                    unsafe {
                        let _ = crate::actor::hew_actor_state_lock_release_after_panic(actor);
                    }
                    // A child suspending-closure call that trapped/longjmped
                    // bypassed the driver's swap-pop and driver-channel teardown,
                    // leaving the outer context pointing at the driver channel and
                    // the driver channel's refs live. Restore the outer reply
                    // routing and tear those channels down BEFORE we read the
                    // dispatch reply channel below, so the fallback reply targets
                    // the real outer channel and no channel ref leaks.
                    crate::execution_context::reply_channel_swap_unwind();
                    // Capture the crashed dispatch's reply-channel state from
                    // the still-installed ctx before restoring `prev_context`.
                    // The ctx pointer becomes stale after the restore, so we
                    // must read it here.
                    let crashed_ctx = crate::execution_context::current_context();
                    let reply_consumed = current_reply_channel_consumed_on(crashed_ctx);
                    let crash_reply = clear_reply_channel_on(crashed_ctx);
                    restore_current_context_after_dispatch();

                    // Transition `Running → Crashing` (or `Stopping → Crashing`
                    // — see below) BEFORE any publication step so that waiters
                    // in `hew_actor_free` (which treats `Crashed` as quiescent
                    // at `actor.rs::actor_free_state_is_quiescent`) cannot
                    // observe the actor as terminal and free `a.arena` /
                    // `a.mailbox` out from under this worker.  `Crashing` is
                    // deliberately *not* quiescent (mirrors the existing
                    // `Stopping → Stopped` two-step).
                    //
                    // The CAS accepts BOTH `Running` and `Stopping` as the
                    // starting state.  `Stopping` arises when the handler
                    // called `hew_actor_self_stop` (`actor.rs:3956`) before
                    // panicking; the crash dominates the pending self-stop,
                    // so we still need full crash bookkeeping (publish
                    // `Crashed`, run link/monitor/supervisor notification)
                    // rather than letting the actor finalise quietly as
                    // `Stopped`.  Pre-fix, the legacy ordering
                    // `handle_crash_recovery` → arena_reset → … achieved this
                    // because `hew_actor_trap`'s own CAS loop accepts any
                    // non-terminal current state and writes `Crashed`; the
                    // new ordering must preserve the same dominance semantics.
                    //
                    // The CAS fails only if the state is already terminal
                    // (`Crashed`/`Stopped`) or in some other state this
                    // worker did not put it in.  In that case the actor may
                    // already have been freed by another thread; we skip
                    // arena_reset and the publication call to avoid racing,
                    // but msg/reply cleanup (which we still uniquely own)
                    // runs unconditionally.
                    let took_crashing = loop {
                        let cur = a.actor_state.load(Ordering::Acquire);
                        if cur != HewActorState::Running as i32
                            && cur != HewActorState::Stopping as i32
                        {
                            break false;
                        }
                        if a.actor_state
                            .compare_exchange(
                                cur,
                                HewActorState::Crashing as i32,
                                Ordering::AcqRel,
                                Ordering::Acquire,
                            )
                            .is_ok()
                        {
                            break true;
                        }
                    };

                    // Per-activation cleanup BEFORE publishing terminal
                    // `Crashed`.  While in `Crashing` no external thread can
                    // observe the actor as quiescent, so `actor_arena`
                    // remains valid for this worker.
                    if took_crashing && !actor_arena.is_null() {
                        // SAFETY: Arena was created during spawn; crash
                        // discards all in-flight data.  The `Crashing`
                        // state we just CAS'd into prevents
                        // `hew_actor_free` from running ahead of us and
                        // reclaiming the arena box.
                        unsafe { crate::arena::hew_arena_reset(actor_arena) };
                    }

                    // If dispatch has not already consumed the sender-side
                    // reply reference, send an empty reply so the waiting
                    // caller of hew_actor_ask is unblocked rather than
                    // deadlocking.  This happens BEFORE publication of
                    // `Crashed` so the test invariant
                    // `active_channel_count() == 0` is observable as soon
                    // as state becomes `Crashed`.
                    if !reply_consumed && !crash_reply.is_null() {
                        // SAFETY: crash_reply is a valid HewReplyChannel pointer.
                        unsafe {
                            let _ = crate::reply_channel::hew_reply(
                                crash_reply.cast(),
                                std::ptr::null_mut(),
                                0,
                            );
                        }
                    }
                    // Clear the node's reply_channel so hew_msg_node_free
                    // doesn't send a duplicate reply.
                    // SAFETY: msg is exclusively owned by this worker.
                    unsafe { (*msg).reply_channel = std::ptr::null_mut() };

                    // Free the message node. The dispatch didn't complete,
                    // but the node itself (allocated by mailbox_send) is
                    // still valid — siglongjmp only unwound the dispatch
                    // stack frames, not the scheduler frame.
                    //
                    // SAFETY: `msg` is exclusively owned by this worker.
                    unsafe { hew_msg_node_free(msg) };

                    // Publish terminal `Crashed` and run supervisor / link
                    // / monitor notifications.  `handle_crash_recovery`
                    // invokes `hew_actor_trap` which CAS-transitions
                    // `Crashing → Crashed` (the trap loop accepts any
                    // non-terminal current state).  Notifications run
                    // AFTER per-activation cleanup so that any waiter
                    // woken by the `Crashed` write observes an arena that
                    // has already been reset and a msg-node that has
                    // already been freed.
                    //
                    // SAFETY: called immediately after sigsetjmp returned
                    // non-zero, on the same worker thread.
                    if took_crashing {
                        // SAFETY: called immediately after sigsetjmp returned
                        // non-zero, on the same worker thread; per-activation
                        // cleanup (arena reset, msg-node free, late
                        // crash-reply) has already run above.
                        unsafe { crate::signal::handle_crash_recovery() };
                    } else {
                        // External trap already published a terminal state
                        // during dispatch; do not call
                        // `handle_crash_recovery` again (it would walk a
                        // potentially-freed `state.current_actor`).  The
                        // external trap already performed the propagation
                        // and `clear_dispatch_recovery` invalidates the
                        // jmp_buf.
                        crate::signal::clear_dispatch_recovery();
                    }

                    // Stop processing further messages for this actor.
                    crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                    crashed = true;
                    break;
                }
            } else {
                // No dispatch function - just free the message
                // SAFETY: `msg` was returned by `hew_mailbox_try_recv` and is
                // now exclusively owned by this worker.
                unsafe { hew_msg_node_free(msg) };
            }

            // If actor self-stopped during dispatch, stop processing.
            let mid_state = a.actor_state.load(Ordering::Acquire);
            if mid_state == HewActorState::Stopping as i32
                || mid_state == HewActorState::Stopped as i32
                || mid_state == HewActorState::Crashed as i32
            {
                break;
            }
        }
    }

    if !crashed && !actor_arena.is_null() {
        // SAFETY: Arena was created during spawn; no references survive past activation.
        unsafe { crate::arena::hew_arena_reset(actor_arena) };
    }

    // After a crash, the actor may have been freed by a supervisor on
    // another worker — do not access `a` or `mailbox` from here on.
    if crashed {
        return;
    }

    // Check if actor transitioned to Stopping during dispatch (self-stop).
    let cur_state = a.actor_state.load(Ordering::Acquire);
    if cur_state == HewActorState::Stopping as i32 {
        // Finalize: Stopping → Stopped.
        if a.actor_state
            .compare_exchange(
                HewActorState::Stopping as i32,
                HewActorState::Stopped as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
            crate::actor_group::notify_actor_death(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            unsafe { crate::actor::call_terminate_fn(actor) };
        }
        return;
    }

    // Check if actor was stopped or crashed during dispatch.
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    // Hibernation: track idle activations.
    actor::update_hibernation_state(a, msgs_processed);

    #[cfg(test)]
    run_activate_pre_reenqueue_hook(actor);

    // After processing: check for remaining messages.
    let has_more = if mailbox.is_null() {
        false
    } else {
        // SAFETY: mailbox pointer is valid.
        unsafe { hew_mailbox_has_messages(mailbox) != 0 }
    };

    if has_more {
        // Budget exhausted, more work pending → RUNNING → RUNNABLE, re-enqueue.
        // Only re-enqueue if CAS succeeds; if it fails the actor was
        // stopped/freed concurrently and touching it would be use-after-free.
        if a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            sched_enqueue(actor);
        }
    } else {
        // No more messages → RUNNING → IDLE.
        if a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Idle as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            // Recheck: a sender may have pushed a message while we were
            // RUNNING but before we transitioned to IDLE.  The sender's
            // CAS IDLE→RUNNABLE would have failed, so we must re-check.
            if !mailbox.is_null()
                // SAFETY: mailbox pointer is valid for the actor's lifetime.
                && unsafe { hew_mailbox_has_messages(mailbox) != 0 }
            {
                // Messages appeared → IDLE → RUNNABLE, re-enqueue.
                if a.actor_state
                    .compare_exchange(
                        HewActorState::Idle as i32,
                        HewActorState::Runnable as i32,
                        Ordering::AcqRel,
                        Ordering::Acquire,
                    )
                    .is_ok()
                {
                    sched_enqueue(actor);
                }
            } else if !mailbox.is_null()
                // SAFETY: mailbox pointer is valid for the actor's lifetime.
                && unsafe { mailbox::mailbox_is_closed(mailbox) }
            {
                // Mailbox closed while draining → IDLE → STOPPED.
                if a.actor_state
                    .compare_exchange(
                        HewActorState::Idle as i32,
                        HewActorState::Stopped as i32,
                        Ordering::AcqRel,
                        Ordering::Acquire,
                    )
                    .is_ok()
                {
                    crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
                    crate::actor_group::notify_actor_death(a.id);
                    // SAFETY: actor just transitioned to Stopped; dispatch is finished.
                    unsafe { crate::actor::call_terminate_fn(actor) };
                }
            }
        }
    }
}

#[cfg(test)]
pub(crate) fn activate_actor_for_test(actor: *mut HewActor) {
    activate_actor(actor);
}

/// Serialises every test that reads or writes the module-level `SCHEDULER`
/// pointer or the `ACTIVE_WORKERS` counter. When `cargo test` runs these in
/// parallel those globals race, producing intermittent SIGSEGV / SIGABRT.
/// Both the in-module scheduler tests and the cross-module
/// [`NoWorkerSchedulerForTest`] guard acquire this single lock, so any test
/// (in any module) that installs a scheduler is mutually serialized.
#[cfg(test)]
pub(crate) static SCHED_TEST_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());

#[cfg(test)]
thread_local! {
    /// Re-entrancy depth for [`SCHED_TEST_MUTEX`]. The std `Mutex` is not
    /// re-entrant, so a test that nests two runtime-installing guards on the
    /// same thread (e.g. `runtime_test_guard()` then
    /// `NoWorkerSchedulerForTest::install()`) must take the lock exactly once.
    static SCHED_TEST_LOCK_DEPTH: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Re-entrant lock guard over [`SCHED_TEST_MUTEX`].
///
/// Acquired by every runtime-installing test guard so they share one
/// serialization point on the process-global default-runtime slot. The
/// outermost acquisition on a thread holds the real `MutexGuard`; nested
/// acquisitions hold `None` and only bump the depth counter, so re-acquiring on
/// the same thread does not deadlock.
#[cfg(test)]
pub(crate) struct SchedTestLock {
    _guard: Option<std::sync::MutexGuard<'static, ()>>,
}

#[cfg(test)]
impl SchedTestLock {
    /// Acquire the re-entrant scheduler-test lock.
    pub(crate) fn acquire() -> Self {
        let held = SCHED_TEST_LOCK_DEPTH.with(|depth| {
            let current = depth.get();
            depth.set(current + 1);
            current > 0
        });
        if held {
            Self { _guard: None }
        } else {
            let guard = SCHED_TEST_MUTEX
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            Self {
                _guard: Some(guard),
            }
        }
    }

    /// True when this thread already holds the lock (a runtime is installed by
    /// an outer guard).
    pub(crate) fn is_held() -> bool {
        SCHED_TEST_LOCK_DEPTH.with(|depth| depth.get() > 0)
    }
}

#[cfg(test)]
impl Drop for SchedTestLock {
    fn drop(&mut self) {
        SCHED_TEST_LOCK_DEPTH.with(|depth| {
            depth.set(depth.get().saturating_sub(1));
        });
    }
}

/// Build a worker-less [`Scheduler`] (single global queue, no worker threads)
/// for test fixtures that need a runtime installed but must not have anything
/// drain the queue.
///
/// Shared by the in-module free-path tests, [`NoWorkerSchedulerForTest`], and
/// the crate-wide runtime test guard (`crate::runtime_test_guard`), which
/// installs a default `RuntimeInner` so every de-globalized authority resolver
/// (`rt_current`) has a runtime to read.
#[cfg(test)]
pub(crate) fn worker_less_scheduler() -> Scheduler {
    Scheduler {
        worker_count: 1,
        parkers: vec![Parker {
            mutex: Mutex::new(()),
            cond: Condvar::new(),
        }],
        stealers: Vec::new(),
        worker_handles: PoisonSafe::new(Vec::new()),
        // SAFETY: single-threaded test setup with scheduler-owned queue state.
        global_queue: unsafe { crate::deque::GlobalQueue::new() },
        shutdown: AtomicBool::new(false),
    }
}

/// Box a fresh worker-less default `RuntimeInner` for the runtime test guard.
///
/// Returns the raw pointer; the caller owns it and frees it via
/// [`crate::runtime::free_runtime_for_test`] on guard teardown.
#[cfg(test)]
pub(crate) fn worker_less_runtime_box() -> *mut RuntimeInner {
    Box::into_raw(Box::new(RuntimeInner::new(worker_less_scheduler())))
}

/// Install a real, worker-backed scheduler for a test that needs worker threads
/// (remote dispatch/ask, native crash recovery, drain).
///
/// A test holding `runtime_test_guard()` for serialization has a worker-less
/// placeholder runtime installed so registry probes resolve `rt_current()`. A
/// plain [`hew_sched_init`] would CAS-no-op against that placeholder and never
/// spawn workers. Detach and free the placeholder first — it owns no worker
/// threads, so this cannot strand a running worker — then init the real
/// scheduler. A real, worker-backed scheduler already shared from an earlier
/// test (non-empty `stealers`) is left installed so its running workers are
/// never freed underneath them. `stealers` is module-private, so this helper
/// (not the calling test) makes the placeholder-vs-real decision.
#[cfg(test)]
pub(crate) fn init_real_scheduler_for_test() {
    let is_placeholder = get_scheduler().is_none_or(|s| s.stealers.is_empty());
    if is_placeholder {
        // Detach the placeholder (so `hew_sched_init` installs a fresh runtime
        // rather than CAS-no-opping), install the worker-backed runtime, then
        // move the placeholder's distributed-node state onto it before dropping
        // it. Before de-globalization the active node / known-node list / reply
        // table were process statics that survived this swap; the transfer
        // preserves that so a test may start a node before calling this helper.
        let placeholder = runtime::take_default();
        assert_eq!(hew_sched_init(), 0, "scheduler init");
        if let Some(placeholder) = placeholder {
            runtime::rt_current()
                .node
                .test_transfer_from(&placeholder.node);
        }
    } else {
        assert_eq!(hew_sched_init(), 0, "scheduler init");
    }
}

/// Box `sched` inside a default `RuntimeInner` and install it in the runtime
/// slot, returning the raw `RuntimeInner` pointer the test owns.
///
/// Tests must hold [`SCHED_TEST_MUTEX`] across install + teardown so the slot
/// is not raced. Teardown detaches via [`take_default_runtime_for_test`].
#[cfg(test)]
fn install_scheduler_for_test(sched: Scheduler) -> *mut RuntimeInner {
    let rt_ptr = Box::into_raw(Box::new(RuntimeInner::new(sched)));
    runtime::test_store_default(rt_ptr);
    rt_ptr
}

/// Detach and drop the default runtime installed by a test (no-op if absent).
#[cfg(test)]
fn take_default_runtime_for_test() {
    let ptr = runtime::test_swap_default(std::ptr::null_mut());
    if !ptr.is_null() {
        // SAFETY: installed by `install_scheduler_for_test` via Box::into_raw;
        // the test holds SCHED_TEST_MUTEX so no other thread can free it, and
        // any worker spawned by the test has been joined before teardown.
        drop(unsafe { Box::from_raw(ptr) });
    }
}

/// Guard that installs a worker-less scheduler for the duration of a test and
/// removes + frees it on drop.
///
/// A worker-less scheduler lets a test call [`sched_enqueue`] (which requires an
/// initialized scheduler) while guaranteeing that **nothing drains the queue** —
/// so an enqueued actor pointer stays observable for assertions. Used by the
/// free-path UAF tests (both the reactor-detach and non-reactor link/monitor
/// wake windows) to prove a freed actor is never left queued.
///
/// # Drop quiescence
///
/// Worker threads from a prior [`hew_sched_init`] may start executing
/// `worker_loop` during the install window (after the global `SCHEDULER`
/// pointer is swapped) and cache the test-only scheduler pointer.  The
/// `Drop` impl sets `shutdown = true` on the installed scheduler, notifies its
/// parkers, then performs a deterministic, load-independent drain: it spins
/// until the process-static `SCHED_TEST_DRAIN_COUNT` (incremented by each
/// worker on loop-body entry, decremented on every exit path) reaches zero,
/// and only then frees the scheduler.  Together with the pointer re-check in
/// `worker_loop`, this closes the test-only heap-use-after-free window without
/// touching the production hot path.
#[cfg(test)]
pub(crate) struct NoWorkerSchedulerForTest {
    previous: *mut RuntimeInner,
    // Held for the guard's lifetime so the installed runtime cannot race the
    // module-level scheduler tests (or another worker-less install) on the
    // default-runtime pointer. Re-entrant: if an outer runtime test guard
    // already holds SCHED_TEST_MUTEX, this is a no-op depth bump (no deadlock).
    // Dropped after the runtime is restored.
    _sched_lock: SchedTestLock,
}

#[cfg(test)]
impl NoWorkerSchedulerForTest {
    /// Install a runtime whose scheduler has a single global queue and no
    /// worker threads.
    pub(crate) fn install() -> Self {
        let sched_lock = SchedTestLock::acquire();
        // SAFETY: single-threaded test setup; the scheduler owns its queue state
        // and lives until this guard is dropped.
        let rt_ptr = Box::into_raw(Box::new(RuntimeInner::new(worker_less_scheduler())));
        // Publish the transient pointer *before* the runtime swap so any worker
        // that subsequently observes the default runtime == rt_ptr also sees a
        // matching SCHED_TEST_DRAIN_PTR and registers in the drain counter.
        SCHED_TEST_DRAIN_PTR.store(rt_ptr, Ordering::SeqCst);
        let previous = runtime::test_swap_default(rt_ptr);
        Self {
            previous,
            _sched_lock: sched_lock,
        }
    }

    /// Pop one actor pointer from the global queue, or `None` if empty.
    ///
    /// Takes `&self` so it can only be called while the guard (and thus the
    /// installed scheduler) is alive; the queue itself is the process-global one.
    #[allow(
        clippy::unused_self,
        reason = "receiver ties the call to the installed-scheduler guard lifetime"
    )]
    pub(crate) fn pop_global(&self) -> Option<*mut HewActor> {
        let sched = get_scheduler()?;
        // SAFETY: single-threaded test deque used only to receive the pop.
        let (local, _stealer) = unsafe { crate::deque::WorkDeque::new() };
        let popped = sched.global_queue.steal_batch_and_pop(&local);
        let first = popped.map(<*mut ()>::cast::<HewActor>);
        // The batch-and-pop may have moved extra items into `local`; push them
        // back so a follow-up pop still sees them. The test only enqueues one
        // item, so this is defensive.
        let mut extra = Vec::new();
        while let Some(p) = local.pop() {
            extra.push(p);
        }
        for p in extra {
            sched.global_queue.push(p);
        }
        first
    }
}

#[cfg(test)]
impl Drop for NoWorkerSchedulerForTest {
    fn drop(&mut self) {
        let ptr = runtime::test_swap_default(self.previous);
        if !ptr.is_null() {
            // SAFETY: `ptr` was allocated by `install` via Box::into_raw and has
            // not been freed yet (Drop runs once, under SCHED_TEST_MUTEX).
            let installed = unsafe { &*ptr };
            // Tell any worker bound to this transient to stop, and wake parked
            // ones so they re-check promptly rather than waiting out PARK_TIMEOUT.
            installed.scheduler.shutdown.store(true, Ordering::Release);
            for parker in &installed.scheduler.parkers {
                parker.cond.notify_all();
            }
            // Deterministic drain (replaces the old timing-based sleep): spin
            // until every worker that entered the loop body bound to this
            // transient has left it (counter == 0). The SeqCst swap above pairs
            // with each worker's post-increment runtime-pointer re-read, so any
            // worker still inside the body is already counted here. After the
            // swap no new worker can enter (they fail the pointer check at the
            // loop top), so the counter only decreases — the spin is bounded by
            // the longest single in-flight loop-body iteration, independent of
            // system load.
            //
            // The drain is unconditional and cheap: with no late worker bound to
            // this transient the counter is already zero and the spin exits
            // immediately, so the common worker-less case pays nothing.
            //
            // A hard timeout converts a genuine hang (a worker wedged forever in
            // the loop body) into a loud panic instead of a silent deadlock.
            let spin_start = std::time::Instant::now();
            while SCHED_TEST_DRAIN_COUNT.load(Ordering::SeqCst) != 0 {
                assert!(
                    spin_start.elapsed() < Duration::from_secs(30),
                    "NoWorkerSchedulerForTest::drop: worker drain did not reach \
                     zero within 30s — a worker thread is stuck in worker_loop \
                     holding a reference to the transient scheduler \
                     (SCHED_TEST_DRAIN_COUNT={})",
                    SCHED_TEST_DRAIN_COUNT.load(Ordering::Relaxed),
                );
                std::thread::yield_now();
            }
            // No worker references the transient now; stop tracking it so a
            // future install starts from a clean slate, then free.
            SCHED_TEST_DRAIN_PTR.store(std::ptr::null_mut(), Ordering::SeqCst);
            // SAFETY: `ptr` was allocated by `install` via Box::into_raw and is
            // now unreferenced by any worker (the drain above observed zero).
            drop(unsafe { Box::from_raw(ptr) });
        }
    }
}

// ── Cooperative yielding ────────────────────────────────────────────────

/// Cooperatively yield if the actor's reduction budget is exhausted.
///
/// The compiler inserts calls to this function at yield points (loop
/// headers, function calls). Each call decrements the reduction counter.
/// When it reaches 0 the actor yields to the scheduler via
/// [`std::thread::yield_now`], and the counter is reset.
///
/// Returns 0 if the actor should continue, 1 if it yielded, and 2 if the
/// current task scope has requested cancellation.
///
/// # Safety
///
/// No preconditions — may be called from any context. When called
/// outside an installed execution context, this sets `hew_last_error` and
/// returns 0.
#[no_mangle]
pub extern "C" fn hew_actor_cooperate() -> c_int {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return 0;
    }

    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    let (actor, cancel_token, scope) =
        unsafe { ((*ctx).actor, (*ctx).cancel_token, (*ctx).task_scope) };

    if !cancel_token.is_null() {
        // SAFETY: cancel_token is owned by the installed task scope.
        if unsafe { crate::task_scope::hew_cancel_token_is_requested(cancel_token) } != 0 {
            return 2;
        }
    }

    if !scope.is_null() {
        // SAFETY: scope is valid per canonical context installation contract.
        if unsafe { crate::task_scope::hew_task_scope_is_cancelled(scope) } != 0 {
            return 2;
        }
    }

    if actor.is_null() {
        return 0;
    }

    // SAFETY: actor was read from the installed canonical context.
    let a = unsafe { &*actor };

    // Decrement reduction counter. If still positive, continue.
    let prev = a.reductions.fetch_sub(1, Ordering::Relaxed);
    if prev > 1 {
        return 0;
    }

    // Budget exhausted — reset counter and yield to OS scheduler.
    a.reductions
        .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

    thread::yield_now();
    1
}

// ── Metrics query API ───────────────────────────────────────────────────

/// Return the total number of tasks spawned (enqueued) since startup or last reset.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_tasks_spawned() -> u64 {
    TASKS_SPAWNED.load(Ordering::Relaxed)
}

/// Return the total number of actor message-batch completions since startup or last reset.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_tasks_completed() -> u64 {
    TASKS_COMPLETED.load(Ordering::Relaxed)
}

/// Return the total number of work-steals from peer deques since startup or last reset.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_steals() -> u64 {
    STEALS_TOTAL.load(Ordering::Relaxed)
}

/// Return the total number of messages sent to mailboxes since startup or last reset.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_messages_sent() -> u64 {
    MESSAGES_SENT.load(Ordering::Relaxed)
}

/// Return the total number of messages received from mailboxes since startup or last reset.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_messages_received() -> u64 {
    MESSAGES_RECEIVED.load(Ordering::Relaxed)
}

/// Return the number of workers currently processing actors.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_active_workers() -> u64 {
    ACTIVE_WORKERS.load(Ordering::Relaxed)
}

/// Reset all scheduler metrics counters to zero.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_reset() {
    TASKS_SPAWNED.store(0, Ordering::Relaxed);
    TASKS_COMPLETED.store(0, Ordering::Relaxed);
    STEALS_TOTAL.store(0, Ordering::Relaxed);
    MESSAGES_SENT.store(0, Ordering::Relaxed);
    MESSAGES_RECEIVED.store(0, Ordering::Relaxed);
    ACTIVE_WORKERS.store(0, Ordering::Relaxed);
    crate::observe::reset_all();
}

/// Return the total number of worker threads.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_worker_count() -> u64 {
    get_scheduler().map_or(0, |s| s.worker_count as u64)
}

/// Return the approximate length of the global run queue.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_global_queue_len() -> u64 {
    get_scheduler().map_or(0, |s| s.global_queue.len() as u64)
}

/// Return the aggregate approximate depth across global and per-worker queues.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_queue_depth() -> u64 {
    scheduler_queue_depth()
}

/// Return the number of actors observed in the Runnable state.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_runnable_actors() -> u64 {
    crate::profiler::actor_registry::state_count(HewActorState::Runnable as i32)
}

/// Return the number of runnable coroutine continuations.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_runnable_coroutines() -> u64 {
    crate::profiler::actor_registry::runnable_coroutine_count()
}

/// Return worker-thread park events; coroutine suspends are counted separately.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_parks_total() -> u64 {
    crate::observe::scheduler_parks_total()
}

/// Return worker wake notifications.
#[no_mangle]
pub extern "C" fn hew_sched_metrics_unparks_total() -> u64 {
    crate::observe::scheduler_unparks_total()
}

#[must_use]
pub(crate) fn scheduler_queue_depth() -> u64 {
    get_scheduler().map_or(0, |s| {
        let local_depth: usize = s.stealers.iter().map(crate::deque::WorkStealer::len).sum();
        (s.global_queue.len() + local_depth) as u64
    })
}

/// Consolidated scheduler metrics snapshot.
///
/// All fields are captured at approximately the same instant.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewSchedMetrics {
    /// Total tasks spawned (enqueued) since startup/reset.
    pub tasks_spawned: u64,
    /// Total message-batch activations completed since startup/reset.
    pub tasks_completed: u64,
    /// Total work-steals from peer deques since startup/reset.
    pub steals: u64,
    /// Total messages sent to mailboxes since startup/reset.
    pub messages_sent: u64,
    /// Total messages received from mailboxes since startup/reset.
    pub messages_received: u64,
    /// Workers currently processing actors.
    pub active_workers: u64,
    /// Total worker threads.
    pub worker_count: u64,
    /// Approximate global run queue depth.
    pub global_queue_len: u64,
    /// Approximate aggregate run queue depth.
    pub queue_depth: u64,
    /// Runnable actor count.
    pub runnable_actors: u64,
    /// Runnable coroutine count.
    pub runnable_coroutines: u64,
    /// Worker-thread park events.
    pub parks_total: u64,
    /// Worker wake notifications.
    pub unparks_total: u64,
}

/// Fill a [`HewSchedMetrics`] snapshot struct.
///
/// # Safety
///
/// `out` must be a valid pointer to a [`HewSchedMetrics`] struct.
#[no_mangle]
pub unsafe extern "C" fn hew_sched_metrics_snapshot(out: *mut HewSchedMetrics) {
    if out.is_null() {
        return;
    }
    // SAFETY: caller guarantees `out` is valid.
    let m = unsafe { &mut *out };
    m.tasks_spawned = TASKS_SPAWNED.load(Ordering::Relaxed);
    m.tasks_completed = TASKS_COMPLETED.load(Ordering::Relaxed);
    m.steals = STEALS_TOTAL.load(Ordering::Relaxed);
    m.messages_sent = MESSAGES_SENT.load(Ordering::Relaxed);
    m.messages_received = MESSAGES_RECEIVED.load(Ordering::Relaxed);
    m.active_workers = ACTIVE_WORKERS.load(Ordering::Relaxed);
    if let Some(s) = get_scheduler() {
        m.worker_count = s.worker_count as u64;
        m.global_queue_len = s.global_queue.len() as u64;
    } else {
        m.worker_count = 0;
        m.global_queue_len = 0;
    }
    m.queue_depth = scheduler_queue_depth();
    m.runnable_actors =
        crate::profiler::actor_registry::state_count(HewActorState::Runnable as i32);
    m.runnable_coroutines = crate::profiler::actor_registry::runnable_coroutine_count();
    m.parks_total = crate::observe::scheduler_parks_total();
    m.unparks_total = crate::observe::scheduler_unparks_total();
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    use std::sync::atomic::AtomicI32;

    // `SCHED_TEST_MUTEX` is defined at module scope (see above) so the
    // cross-module `NoWorkerSchedulerForTest` guard serializes against these
    // tests too; `use super::*` brings it into scope here.

    struct ActivatePreReenqueueHookGuard;

    impl ActivatePreReenqueueHookGuard {
        fn install(hook: fn(*mut HewActor)) -> Self {
            ACTIVATE_PRE_REENQUEUE_HOOK.access(|h| {
                assert!(h.replace(hook).is_none(), "test hook already installed");
            });
            Self
        }
    }

    impl Drop for ActivatePreReenqueueHookGuard {
        fn drop(&mut self) {
            ACTIVATE_PRE_REENQUEUE_HOOK.access(|h| *h = None);
        }
    }

    struct ActivatePostCasHookGuard;

    impl ActivatePostCasHookGuard {
        fn install(hook: fn(*mut HewActor)) -> Self {
            ACTIVATE_POST_CAS_HOOK.access(|h| {
                assert!(h.replace(hook).is_none(), "test hook already installed");
            });
            Self
        }
    }

    impl Drop for ActivatePostCasHookGuard {
        fn drop(&mut self) {
            ACTIVATE_POST_CAS_HOOK.access(|h| *h = None);
        }
    }

    /// Helper: build a minimal `HewActor` with sensible defaults.
    fn stub_actor() -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 1,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Runnable as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(actor::HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: std::ptr::null_mut(),
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
        }
    }

    /// RAII guard that registers a stub actor in `LIVE_ACTORS` for the duration
    /// of a test and untracks it on drop. `enqueue_resume` now confirms liveness
    /// against `LIVE_ACTORS` before waking (the W6.010 caller-actor UAF guard),
    /// so a wake-expecting test must present its stub as a LIVE actor. Each guard
    /// assigns a unique `id` so concurrent tests do not collide in the
    /// process-wide registry.
    struct TrackedTestActor {
        ptr: *mut HewActor,
    }

    impl TrackedTestActor {
        fn install(mut actor: HewActor) -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(1);
            actor.id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
            // Own the actor through a single raw pointer from `Box::into_raw`
            // rather than storing a `Box` alongside a derived raw pointer: under
            // Stacked Borrows, moving the `Box` (e.g. returning `Self`) retags and
            // invalidates the pointee, so the saved raw tag would be stale. The
            // `into_raw` pointer keeps valid provenance across the struct move and
            // `Drop` reconstitutes the `Box` to free it exactly once.
            let ptr: *mut HewActor = Box::into_raw(Box::new(actor));
            // SAFETY: `ptr` is a freshly-boxed, fully-initialised actor.
            unsafe { crate::lifetime::live_actors::track_actor(ptr) };
            Self { ptr }
        }

        fn ptr(&self) -> *mut HewActor {
            self.ptr
        }

        /// Untrack the actor WITHOUT freeing the box, modelling a caller actor
        /// torn down before a late/orphan-retire reply fires. After this returns
        /// `enqueue_resume(ptr)` must observe the actor as no longer live.
        fn untrack(&self) {
            crate::lifetime::live_actors::untrack_actor(self.ptr);
        }
    }

    impl std::ops::Deref for TrackedTestActor {
        type Target = HewActor;
        fn deref(&self) -> &HewActor {
            // SAFETY: `ptr` owns a live, boxed actor for the guard's lifetime.
            unsafe { &*self.ptr }
        }
    }

    impl Drop for TrackedTestActor {
        fn drop(&mut self) {
            // Idempotent: `untrack_actor` only removes a matching entry; a
            // double-untrack (test already called `untrack`) is a no-op.
            crate::lifetime::live_actors::untrack_actor(self.ptr);
            // SAFETY: `ptr` came from `Box::into_raw` in `install`; reclaim the
            // box so the actor is freed exactly once.
            unsafe { drop(Box::from_raw(self.ptr)) };
        }
    }

    #[test]
    fn activate_transitions_runnable_to_idle() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let actor = stub_actor();
        let ptr: *mut HewActor = (&raw const actor).cast_mut();

        activate_actor(ptr);

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32
        );
    }

    #[test]
    fn activate_skips_stopped_actor() {
        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        let ptr: *mut HewActor = (&raw const actor).cast_mut();

        activate_actor(ptr);

        // State should remain STOPPED.
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32
        );
    }

    #[test]
    fn activate_skips_idle_actor() {
        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Release);
        let ptr: *mut HewActor = (&raw const actor).cast_mut();

        activate_actor(ptr);

        // CAS should fail — state stays IDLE.
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32
        );
    }

    /// `enqueue_resume` on a `Suspended` actor CASes it to `Runnable` and
    /// pushes it onto the global queue — the wake edge for a parked
    /// continuation, structurally the dual of the `Sleeping → Runnable`
    /// sleeper-drain re-enqueue.
    #[test]
    fn enqueue_resume_wakes_suspended_actor() {
        let sched = NoWorkerSchedulerForTest::install();
        let actor = TrackedTestActor::install(stub_actor());
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        // Park a (non-null sentinel) handle so the wake edge sees a published
        // slot rather than the FG3 mid-park window. A bare sentinel is fine:
        // enqueue_resume only reads the slot for null-ness, it never resumes.
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        // SAFETY: actor is live (tracked) for this scope; sentinel handle is
        // never resumed.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32,
            "enqueue_resume must CAS Suspended -> Runnable"
        );
        assert_eq!(
            sched.pop_global(),
            Some(actor_ptr),
            "the woken actor must be enqueued exactly once"
        );
    }

    /// W6.010 caller-actor UAF guard (S1): `enqueue_resume` must NOT dereference
    /// or wake a caller actor that has already been freed (untracked from
    /// `LIVE_ACTORS`). This drives the exact teardown ordering the security gate
    /// reproduced under `ASan`: a `Suspended` caller parked on a reply channel is
    /// torn down (untracked) FIRST, and only then does the callee's
    /// orphan-retire reply path fire `enqueue_resume` on the now-stale pointer.
    /// With the registry-liveness guard the wake is dropped (no enqueue, no
    /// deref); without it, the deref of the freed box is a heap-use-after-free.
    #[test]
    fn enqueue_resume_drops_wake_for_freed_caller() {
        let sched = NoWorkerSchedulerForTest::install();
        let actor = TrackedTestActor::install(stub_actor());
        // The caller is parked: Suspended with a published (sentinel) handle.
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        // Teardown ordering: the caller is freed (untracked) BEFORE the late
        // reply fires. The box still exists in this test (so a buggy deref would
        // read live-but-logically-dead memory rather than crash), but the actor
        // is no longer tracked — exactly the production window where
        // `cleanup_all_actors` already drained the registry.
        actor.untrack();

        // SAFETY: `actor_ptr` is a stale (untracked) pointer; `enqueue_resume`
        // must reject it via the registry check rather than dereference it.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "a freed (untracked) caller must NOT be CAS'd to Runnable"
        );
        assert_eq!(
            sched.pop_global(),
            None,
            "a wake targeting a freed caller must enqueue nothing"
        );
        assert!(
            !crate::coro_exec::take_pending_wake(&actor),
            "a freed caller must not record a pending wake either"
        );
    }

    /// The same UAF guard exercised through the full reply path: a reply
    /// published on a channel whose parked-waiter caller has been freed
    /// (untracked) must reach `enqueue_resume`, find the caller dead, and drop
    /// the wake — never deref the stale `caller_actor`. This mirrors the
    /// orphan-retire teardown (`hew_reply_channel_retire_orphaned_ask_sender_ref`)
    /// the callee mailbox runs while a caller is gone.
    #[test]
    fn reply_to_freed_parked_waiter_drops_wake() {
        let _guard = crate::runtime_test_guard();
        let sched = NoWorkerSchedulerForTest::install();
        let actor = TrackedTestActor::install(stub_actor());
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        let ch = crate::reply_channel::hew_reply_channel_new();
        let payload = 99_i64;
        // SAFETY: `ch` is a fresh live channel; retain the sender ref the reply
        // consumes; arm the parked-waiter BEFORE the caller is freed (the
        // production order: set_parked_waiter happens-before the ask submit).
        unsafe {
            crate::reply_channel::hew_reply_channel_retain(ch);
            crate::reply_channel::hew_reply_channel_set_parked_waiter(ch, actor_ptr);
        }

        // Free (untrack) the caller while the reply is still in flight.
        actor.untrack();

        // SAFETY: the reply fires on a channel whose `caller_actor` is now stale;
        // the deposit + wake must not deref the freed caller.
        unsafe {
            let delivered = crate::reply_channel::hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i64>(),
            );
            assert!(
                delivered,
                "the reply still deposits its value on the channel"
            );
        }

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "the reply must not revive a freed caller"
        );
        assert_eq!(
            sched.pop_global(),
            None,
            "a reply to a freed parked waiter must enqueue nothing"
        );

        // The value sits on `ch`; drain + free so the test leaks nothing.
        // SAFETY: the test still holds a channel reference.
        unsafe {
            let v = crate::reply_channel::hew_reply_wait(ch).cast::<i64>();
            assert!(!v.is_null());
            libc::free(v.cast());
            crate::reply_channel::hew_reply_channel_free(ch);
        }
    }

    /// `enqueue_resume` is fail-closed: a terminal (`Stopped`) actor is never
    /// enqueued — the CAS fails and the actor stays terminal, mirroring the
    /// `Idle → Runnable` waker discipline that closes the use-after-free window.
    #[test]
    fn enqueue_resume_fail_closed_on_terminal_actor() {
        let sched = NoWorkerSchedulerForTest::install();
        // Tracked (live in the registry) but TERMINAL — so this exercises the
        // state-CAS fail-closed arm, not the registry-liveness drop.
        let actor = TrackedTestActor::install(stub_actor());
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        // SAFETY: actor is live (tracked); terminal so it is never resumed/enqueued.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32,
            "a terminal actor must not be revived by a wake"
        );
        assert_eq!(
            sched.pop_global(),
            None,
            "a terminal actor must never be enqueued"
        );
    }

    /// R4 wake-routing net: `enqueue_resume` resolves the scheduler it enqueues
    /// on through `get_scheduler()` → `rt_default()`, which `enter()` (and
    /// `enter_actor_runtime`) does NOT reroute. A wake for an actor owned by a
    /// NON-default runtime would silently land on the default runtime's
    /// scheduler, so the net traps BEFORE the mis-routed enqueue. The DEFAULT
    /// control is the existing `enqueue_resume_wakes_suspended_actor` — a default
    /// actor enqueues without tripping the net — so single-runtime behaviour is
    /// unchanged. Full rerouting of the wake to the owning runtime is M4.
    #[test]
    fn enqueue_resume_traps_on_foreign_runtime_wake() {
        let sched = NoWorkerSchedulerForTest::install();
        // A tracked, parked, Suspended actor stamped to a non-default runtime.
        let mut stub = stub_actor();
        stub.runtime_id = crate::runtime_id::RuntimeId(1);
        let actor = TrackedTestActor::install(stub);
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        // Park a (non-null sentinel) handle so the wake edge sees a published
        // slot rather than the FG3 mid-park window; the sentinel is never resumed.
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        let trapped = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // SAFETY: actor is live (tracked) for this scope; the sentinel handle
            // is never resumed. The trap fires after the registry lock is
            // released, so it does not poison the live-actor registry.
            unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };
        }));
        assert!(
            trapped.is_err(),
            "waking a non-default-runtime actor through the un-rerouted default scheduler must TRAP"
        );
        assert_eq!(
            sched.pop_global(),
            None,
            "the trap must fire BEFORE the mis-routed enqueue — nothing reaches the queue"
        );
    }

    /// FG3: a wake that fires before the park publishes a handle (the
    /// `suspended_cont` slot is still null) is RECORDED via `pending_wake`
    /// rather than lost, so the suspend edge can re-enqueue it.
    #[test]
    fn enqueue_resume_mid_park_records_pending_wake() {
        let _sched = NoWorkerSchedulerForTest::install();
        let actor = TrackedTestActor::install(stub_actor());
        // Park not yet published: state not Suspended, slot null.
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);
        let actor_ptr = actor.ptr();

        // SAFETY: actor is live (tracked); null slot means no handle is ever
        // resumed.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };

        assert!(
            crate::coro_exec::take_pending_wake(&actor),
            "a wake in the park window must be recorded, not lost (FG3)"
        );
    }

    /// W6.010 waiter-kind: a reply to a channel whose waiter is a PARKED
    /// CONTINUATION wakes the caller actor via `enqueue_resume` (CAS
    /// Suspended -> Runnable + enqueue), NOT the condvar. The resumed
    /// continuation reads the now-ready value on its resume edge.
    #[test]
    fn reply_to_parked_waiter_enqueues_resume() {
        // Serialize against tests that read the process-wide reply-channel
        // counter (actor::tests::native_ask_*). Acquire the channel-counter
        // guard BEFORE installing the scheduler (the actor tests' lock order).
        let _guard = crate::runtime_test_guard();
        let sched = NoWorkerSchedulerForTest::install();
        let actor = TrackedTestActor::install(stub_actor());
        // The caller is parked: Suspended with a published (sentinel) handle.
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        let ch = crate::reply_channel::hew_reply_channel_new();
        let payload = 42_i64;
        // SAFETY: `ch` is a fresh live channel; retain for the sender ref the
        // reply consumes; `actor_ptr` is live (tracked) for this scope.
        unsafe {
            crate::reply_channel::hew_reply_channel_retain(ch);
            crate::reply_channel::hew_reply_channel_set_parked_waiter(ch, actor_ptr);
            let delivered = crate::reply_channel::hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i64>(),
            );
            assert!(delivered, "reply to a parked waiter must deliver the value");
        }

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32,
            "a parked-continuation reply must wake the caller via enqueue_resume"
        );
        assert_eq!(
            sched.pop_global(),
            Some(actor_ptr),
            "the woken caller actor must be enqueued exactly once"
        );

        // The value is held on `ch` for the resume edge to read; drain + free.
        // SAFETY: `ch` is still live (the test holds the waiter reference).
        unsafe {
            let v = crate::reply_channel::hew_reply_wait(ch).cast::<i64>();
            assert!(!v.is_null(), "the reply value must be readable on resume");
            assert_eq!(*v, 42, "the resumed caller binds the CORRECT reply value");
            libc::free(v.cast());
            crate::reply_channel::hew_reply_channel_free(ch);
        }
    }

    /// NEW-1 (reactor fd-IO): the reactor's resume-mode wake is the SECOND
    /// production source of `enqueue_resume` (after the reply path). This models
    /// the reactor depositing read bytes into a parked handler's read slot and
    /// waking it: the parked actor transitions `Suspended → Runnable`, is
    /// enqueued exactly once, and the resume edge reads the CORRECT bytes back
    /// from the slot (the value-routing edge — not garbage). It is the runtime
    /// half of the `await conn.read()` cycle, mirroring
    /// `reply_to_parked_waiter_enqueues_resume` for the reactor source.
    #[test]
    fn reactor_data_deposit_resumes_parked_handler_with_bytes() {
        let _guard = crate::runtime_test_guard();
        let sched = NoWorkerSchedulerForTest::install();
        let actor = TrackedTestActor::install(stub_actor());
        // The handler is parked on the fd: Suspended with a published handle.
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        let actor_ptr = actor.ptr();

        // The reactor's resume-mode deposit + wake (what `handle_ready_resume`
        // does on `Data`): build an owned bytes value, deposit it into the slot,
        // then `enqueue_resume(actor, null)`.
        let slot = crate::read_slot::hew_read_slot_new();
        let payload = b"reactor-delivered-bytes";
        let payload_len = u32::try_from(payload.len()).expect("payload fits u32");
        // SAFETY: payload valid for its len; copied into a refcount-1 buffer.
        let triple = unsafe { crate::bytes::hew_bytes_from_static(payload.as_ptr(), payload_len) };
        // SAFETY: fresh slot; the deposit takes ownership of the triple.
        let wake = unsafe { crate::read_slot::read_slot_deposit_data(slot, triple) };
        assert!(wake, "a non-cancelled deposit must signal a wake");
        // SAFETY: `actor_ptr` is live (tracked) for this scope.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32,
            "a reactor fd-readiness wake must transition the parked handler to Runnable"
        );
        assert_eq!(
            sched.pop_global(),
            Some(actor_ptr),
            "the woken handler must be enqueued exactly once"
        );

        // The resume edge reads the bytes back — the value-routing edge.
        // SAFETY: `slot` is the fresh live slot; status read after the deposit.
        let status = unsafe { crate::read_slot::hew_read_slot_status(slot) };
        assert_eq!(status, crate::read_slot::ReadStatus::Data as i32);
        // SAFETY: `slot` is live; take transfers ownership of the deposited buffer.
        let taken = unsafe { crate::read_slot::hew_read_slot_take(slot) };
        assert_eq!(taken.len as usize, payload.len());
        // SAFETY: take transferred ownership of a refcount-1 buffer of len bytes.
        let read_back = unsafe {
            std::slice::from_raw_parts(taken.ptr.add(taken.offset as usize), taken.len as usize)
        };
        assert_eq!(
            read_back, payload,
            "the resumed handler must bind the CORRECT reactor-delivered bytes"
        );
        // SAFETY: take transferred ownership of the buffer.
        unsafe { crate::bytes::hew_bytes_drop(taken.ptr) };
        // SAFETY: the creator ref is the last ref; this reclaims the slot.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };
    }

    /// W6.010 waiter-kind (E6): a reply to a channel with NO parked waiter
    /// (the default — a foreign/main-thread condvar ask) does NOT touch the
    /// scheduler queue; the foreign thread is woken by the condvar. The
    /// foreign-thread ask path must not regress.
    #[test]
    fn reply_to_condvar_waiter_does_not_enqueue() {
        // Serialize against the process-wide reply-channel counter (see the
        // parked-waiter test above) — guard before installing the scheduler.
        let _guard = crate::runtime_test_guard();
        let sched = NoWorkerSchedulerForTest::install();

        let ch = crate::reply_channel::hew_reply_channel_new();
        let payload = 7_i64;
        // SAFETY: `ch` is a fresh live channel; retain for the sender ref. No
        // `set_parked_waiter` -> `caller_actor` stays null (condvar path).
        unsafe {
            crate::reply_channel::hew_reply_channel_retain(ch);
            let delivered = crate::reply_channel::hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i64>(),
            );
            assert!(delivered, "condvar reply must still deliver the value");
        }

        assert!(
            sched.pop_global().is_none(),
            "a condvar-waiter reply must not enqueue any actor (E6 foreign-thread path)"
        );

        // SAFETY: `ch` is still live; the value was deposited on the fast path.
        unsafe {
            let v = crate::reply_channel::hew_reply_wait(ch).cast::<i64>();
            assert!(!v.is_null());
            assert_eq!(*v, 7);
            libc::free(v.cast());
            crate::reply_channel::hew_reply_channel_free(ch);
        }
    }

    /// The SUSPEND edge parks a scratch continuation, publishes `Suspended`,
    /// and does NOT re-enqueue (no pending wake) — the worker is freed. The
    /// per-cont tag is `Parked` and the slot carries the handle (the resume
    /// re-entry discriminator).
    #[test]
    fn park_suspended_activation_publishes_suspended_and_parks_cont() {
        let sched = NoWorkerSchedulerForTest::install();
        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        let mut frame = Box::new(crate::coro_exec::test_support::ScratchFrame::new(2));
        let handle = (&raw mut *frame).cast::<std::ffi::c_void>();

        // SAFETY: actor owned by this frame; scratch handle is live.
        let parked = unsafe { park_suspended_activation(actor_ptr, handle) };
        assert!(parked, "park must succeed from Running with an Empty tag");

        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "suspend edge publishes Suspended"
        );
        assert_eq!(
            actor.suspended_cont.load(Ordering::Acquire),
            handle,
            "the handle is parked for the resume re-entry"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Parked as i32
        );
        assert!(
            sched.pop_global().is_none(),
            "no pending wake -> the worker is freed without re-enqueue"
        );

        // Clean up the parked scratch frame.
        // SAFETY: parked handle is live and not yet destroyed.
        assert!(unsafe { crate::coro_exec::destroy_parked(&actor) }.is_ok());
    }

    /// Full seed round-trip through `activate_actor`: park a scratch cont that
    /// completes on its 2nd resume, wake it twice, and assert the resume
    /// re-entry drives it to Ready and destroys it exactly once — the actor
    /// settles to Idle and the slot is nulled (FG1/FG4).
    #[test]
    fn activate_resumes_parked_cont_to_ready_and_destroys_once() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // Scratch frame: Ready on the 2nd resume.
        let mut frame = Box::new(crate::coro_exec::test_support::ScratchFrame::new(2));
        let handle = (&raw mut *frame).cast::<std::ffi::c_void>();

        // SAFETY: actor owned; scratch handle live.
        assert!(unsafe { park_suspended_activation(actor_ptr, handle) });
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32
        );

        // First wake + activate: resume #1 -> Pending -> re-parked Suspended.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        activate_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "Pending resume re-parks the actor as Suspended"
        );
        assert_eq!(
            frame.resumes.load(Ordering::Acquire),
            1,
            "exactly one resume on the first activation"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Parked as i32
        );

        // Second wake + activate: resume #2 -> Ready -> destroy once -> Idle.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        activate_actor(actor_ptr);
        assert_eq!(
            frame.resumes.load(Ordering::Acquire),
            2,
            "second activation drove the continuation to completion"
        );
        assert_eq!(
            frame.destroyed.load(Ordering::Acquire),
            1,
            "FG1: the Ready continuation is destroyed exactly once"
        );
        assert!(
            actor.suspended_cont.load(Ordering::Acquire).is_null(),
            "FG4: the slot is nulled in the Destroyed critical section"
        );
        // P1-B: a completed resume re-arms the tag `Destroyed -> Empty` on the
        // quiescent edge so the actor can park a NEW continuation on its next
        // `await` (multi-await). The destroy still ran exactly once (asserted
        // above via the scratch frame's `destroyed` counter); the tag is now
        // back to the armed `Empty` steady state, not the terminal `Destroyed`.
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Empty as i32,
            "P1-B: the completed continuation is reclaimed and the tag re-armed to Empty"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32,
            "a completed resume with an empty mailbox settles to Idle"
        );
    }

    /// R2 (P0) deadlock-avoidance: the suspend/resume executor edges hold NO
    /// per-actor state lock, so a sender is never blocked by a suspended actor.
    /// A message enqueued while the actor is parked in `Suspended` lands in the
    /// mailbox without blocking, and the resume re-entry then settles to
    /// `Runnable` (re-enqueued) so the queued message is served.
    ///
    /// The per-message lock RELEASE on the production suspend edge is LANDED
    /// (NEW-3a): the dispatch loop releases the per-actor state lock on the
    /// normal dispatch-return edge (`hew_actor_state_lock_release_for_context`,
    /// the matching release of the acquire above the `dispatch` call) BEFORE the
    /// suspend handle is captured and parked, so a suspended actor never holds
    /// its lock against senders. The production edge is asserted by
    /// `production_suspend_edge_releases_the_actor_lock` below (a real dispatch
    /// fn returning a non-null handle, with a registered lock). This test pins
    /// the complementary executor-edge invariant: the seed driver itself holds
    /// no lock, and a send to a `Suspended` actor never blocks.
    #[test]
    fn suspended_actor_accepts_sends_without_blocking() {
        // The worker-less scheduler holds SCHED_TEST_MUTEX for us AND lets the
        // settle path call `sched_enqueue` without a draining worker.
        let _sched = NoWorkerSchedulerForTest::install();
        // SAFETY: fresh mailbox, no preconditions.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());

        let mut actor = stub_actor();
        actor.mailbox = mailbox.cast();
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // Park a scratch cont (completes on the 1st resume).
        let mut frame = Box::new(crate::coro_exec::test_support::ScratchFrame::new(1));
        let handle = (&raw mut *frame).cast::<std::ffi::c_void>();
        // SAFETY: actor owned; scratch handle live.
        assert!(unsafe { park_suspended_activation(actor_ptr, handle) });
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32
        );

        // A sender enqueues while the actor is Suspended — must not block and
        // must land in the mailbox (no lock held against it).
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 is valid.
            unsafe { mailbox::hew_mailbox_send(mailbox, 5, ptr::null_mut(), 0) },
            0,
            "a send to a Suspended actor must not block or fail"
        );
        assert!(
            // SAFETY: mailbox is live.
            unsafe { hew_mailbox_has_messages(mailbox.cast::<HewMailbox>()) != 0 },
            "the message landed in the mailbox"
        );

        // Wake + activate: resume #1 -> Ready -> destroy once. The pending
        // mailbox message means the settle re-enqueues as Runnable.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        activate_actor(actor_ptr);
        assert_eq!(
            frame.destroyed.load(Ordering::Acquire),
            1,
            "the resumed continuation is destroyed exactly once"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32,
            "a queued message after a completed resume re-enqueues the actor"
        );

        // Teardown: drain the queued message + free the mailbox safely.
        // SAFETY: single-threaded test; nothing else references the mailbox.
        unsafe {
            let msg = hew_mailbox_try_recv(mailbox.cast::<HewMailbox>());
            if !msg.is_null() {
                hew_msg_node_free(msg);
            }
            mailbox::hew_mailbox_free(mailbox);
        }
    }

    #[test]
    fn xorshift64_produces_different_values() {
        let mut rng = Xorshift64::new(42);
        let a = rng.next_u64();
        let b = rng.next_u64();
        let c = rng.next_u64();

        assert_ne!(a, b);
        assert_ne!(b, c);
        assert_ne!(a, c);
    }

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut std::ffi::c_void,
        _msg_type: i32,
        _data: *mut std::ffi::c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    #[test]
    fn activate_records_dispatch_span_events() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // Also hold the tracing serialisation lock: this test calls
        // hew_trace_reset/enable/drain and must not race with tracing-module
        // tests that hold their own tracing lock.  Consistent lock order:
        // SCHED_TEST_MUTEX first, then TRACING_TEST_LOCK.
        let _tg = crate::tracing::tracing_test_guard();
        crate::tracing::hew_trace_reset();
        crate::tracing::hew_trace_enable(1);

        // SAFETY: creating a new mailbox with no preconditions.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is non-null and was just created above.
            unsafe { mailbox::hew_mailbox_send(mailbox, 77, ptr::null_mut(), 0) },
            0
        );

        let actor = HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 42,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: mailbox.cast(),
            actor_state: AtomicI32::new(HewActorState::Runnable as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(actor::HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: ptr::null_mut(),
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
        };
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        activate_actor(actor_ptr);

        let mut events = [crate::tracing::HewTraceEvent {
            trace_id_hi: 0,
            trace_id_lo: 0,
            span_id: 0,
            parent_span_id: 0,
            actor_id: 0,
            event_type: 0,
            msg_type: 0,
            timestamp_ns: 0,
        }; 4];
        // SAFETY: events buffer is valid and large enough for 4 entries.
        let count = unsafe { crate::tracing::hew_trace_drain(events.as_mut_ptr(), 4) };
        assert_eq!(count, 2);
        assert_eq!(events[0].event_type, crate::tracing::SPAN_BEGIN);
        assert_eq!(events[0].actor_id, 42);
        assert_eq!(events[0].msg_type, 77);
        assert_eq!(events[1].event_type, crate::tracing::SPAN_END);
        assert_eq!(events[1].msg_type, 77);

        // SAFETY: mailbox was created in this test and is not used afterwards.
        unsafe { mailbox::hew_mailbox_free(mailbox) };
        crate::tracing::hew_trace_reset();
    }

    // Observations captured by `trap_self_mid_dispatch` from INSIDE the
    // activation, so the test can assert the dispatch-ownership invariant that
    // gates `hew_actor_free` against the free-under-active-dispatch UAF.
    static MID_DISPATCH_ACTIVE: AtomicBool = AtomicBool::new(false);
    static MID_DISPATCH_STATE: AtomicI32 = AtomicI32::new(-1);
    static MID_DISPATCH_FREE_REFUSED: AtomicBool = AtomicBool::new(false);

    /// Dispatch handler that traps its own actor mid-dispatch (modelling an
    /// external `hew_actor_trap` forcing the actor terminal while a worker still
    /// owns the activation), then snapshots — from inside the still-running
    /// activation — the dispatch-ownership flag, the (now terminal) state, and
    /// whether the free-quiescence predicate would refuse to free at this
    /// instant. This is the exact dangerous window: state is `Crashed` but the
    /// worker is still reading the actor box.
    unsafe extern "C-unwind" fn trap_self_mid_dispatch(
        ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut std::ffi::c_void,
        _msg_type: i32,
        _data: *mut std::ffi::c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: the scheduler installs a valid context for the dispatch.
        let actor = unsafe { (*ctx).actor };
        // SAFETY: `actor` is the live actor owning this activation.
        unsafe {
            crate::actor::hew_actor_trap(actor, 1);
            let a = &*actor;
            MID_DISPATCH_ACTIVE.store(a.dispatch_active.load(Ordering::Acquire), Ordering::SeqCst);
            MID_DISPATCH_STATE.store(a.actor_state.load(Ordering::Acquire), Ordering::SeqCst);
            // The free-quiescence predicate the free path uses: terminal state
            // AND no active dispatch. Inside this handler the activation is
            // still owned, so it must refuse (so an async free waits us out).
            let terminal = a.actor_state.load(Ordering::Acquire) == HewActorState::Crashed as i32;
            let owned = a.dispatch_active.load(Ordering::Acquire);
            MID_DISPATCH_FREE_REFUSED.store(terminal && owned, Ordering::SeqCst);
        }
        std::ptr::null_mut()
    }

    /// A worker that wins an activation owns the actor box until it leaves
    /// `activate_actor` — even when an external trap forces the actor terminal
    /// mid-dispatch. `dispatch_active` stays set across the trap (so the async
    /// free path that the trap's supervisor-notify ultimately drives waits the
    /// worker out instead of reclaiming the box+mailbox under it), and is
    /// cleared once the activation returns. Regression guard for the
    /// free-under-active-dispatch use-after-free that surfaced as a corrupted
    /// msg-node tripping the envelope-mode dispatch guard on Windows-IOCP timing
    /// (#58/#2 `child_supervisor_recovery_recreates_nested_subtree`).
    #[test]
    fn activation_owns_actor_across_external_trap_until_settle() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        MID_DISPATCH_ACTIVE.store(false, Ordering::SeqCst);
        MID_DISPATCH_STATE.store(-1, Ordering::SeqCst);
        MID_DISPATCH_FREE_REFUSED.store(false, Ordering::SeqCst);

        // SAFETY: creating a new mailbox with no preconditions.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is non-null and was just created above.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );

        let mut actor = stub_actor();
        actor.dispatch = Some(trap_self_mid_dispatch);
        actor.mailbox = mailbox.cast();
        actor.actor_state = AtomicI32::new(HewActorState::Runnable as i32);
        let actor_ptr: *mut HewActor = Box::into_raw(Box::new(actor));

        activate_actor(actor_ptr);

        // SAFETY: `actor_ptr` is still owned by this test (no supervisor free).
        let a = unsafe { &*actor_ptr };

        // Inside the activation, after the trap: the actor was terminal AND the
        // activation still owned it — exactly the window a naive free would
        // have reclaimed the box in.
        assert!(
            MID_DISPATCH_ACTIVE.load(Ordering::SeqCst),
            "dispatch_active must stay set across an external trap while the worker owns the activation"
        );
        assert_eq!(
            MID_DISPATCH_STATE.load(Ordering::SeqCst),
            HewActorState::Crashed as i32,
            "the trap must have published the terminal state mid-dispatch"
        );
        assert!(
            MID_DISPATCH_FREE_REFUSED.load(Ordering::SeqCst),
            "free-quiescence must REFUSE while terminal-but-dispatch-active (the UAF window)"
        );

        // After the activation returns, ownership is released so the free path
        // can proceed.
        assert!(
            !a.dispatch_active.load(Ordering::Acquire),
            "dispatch_active must clear once the activation leaves activate_actor"
        );
        assert_eq!(
            a.actor_state.load(Ordering::Acquire),
            HewActorState::Crashed as i32,
            "the actor remains Crashed after the activation settles"
        );

        // SAFETY: the test exclusively owns the actor and mailbox; the mailbox
        // drain releases any node enqueued before the trap.
        unsafe {
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor_ptr));
        }
    }

    // Cross-thread coordination for the CAS->marker-gap race test. The worker
    // thread (running `activate_actor`) and a concurrent freer thread rendezvous
    // through these so the free races the worker's post-trap settle reads.
    static GAP_TRAP_FIRED: AtomicBool = AtomicBool::new(false);
    static GAP_FREE_RETURNED: AtomicI32 = AtomicI32::new(i32::MIN);
    static GAP_FLAG_AT_TRAP: AtomicBool = AtomicBool::new(false);

    /// Hook fired at the CAS->marker-gap location (immediately after the
    /// `Runnable -> Running` CAS wins, while the worker owns the activation).
    /// Models an external `hew_actor_trap` from another thread landing in the
    /// gap: it forces the actor terminal (`Running -> Crashed`), snapshots the
    /// dispatch-ownership flag, then SIGNALS a concurrent freer thread to call
    /// `hew_actor_free` while this worker is still inside the activation and
    /// about to run its settle reads of the actor box. The free must observe
    /// `dispatch_active == true` and WAIT, never reclaiming the box under us.
    ///
    /// Pre-fix (claim AFTER the CAS) the flag could be `false` here, so the freer
    /// would reclaim the box and the worker's subsequent settle reads (and the
    /// guard's own `dispatch_active` store on frame exit) would touch freed
    /// memory — the UAF this guards against, caught by ASan/LSan.
    fn trap_then_signal_free(actor: *mut HewActor) {
        // SAFETY: `actor` is the live actor this worker just CAS'd to `Running`.
        unsafe {
            crate::actor::hew_actor_trap(actor, 1);
            let a = &*actor;
            GAP_FLAG_AT_TRAP.store(a.dispatch_active.load(Ordering::Acquire), Ordering::SeqCst);
        }
        // Release the freer: it now races our settle. The freer must block on
        // `dispatch_active` until this activation returns.
        GAP_TRAP_FIRED.store(true, Ordering::Release);
        // Give the freer a wide window to wake, enter `hew_actor_free`, and reach
        // its quiescence load while this worker is still inside the activation. A
        // correct free is blocked on `dispatch_active` (set before the CAS), so
        // this is harmless; a pre-fix free would (incorrectly) observe
        // terminal-without-marker here and reclaim the box under us — the UAF
        // this widened window makes ASan-visible.
        std::thread::sleep(std::time::Duration::from_millis(40));
    }

    /// `dispatch_active` is claimed BEFORE the `Runnable -> Running` CAS, so an
    /// external trap landing in the CAS->marker gap (the actor is `Running` and
    /// owned) keeps the flag set, and a concurrent `hew_actor_free` racing the
    /// worker's settle WAITS on the flag instead of reclaiming the box under the
    /// worker. Regression guard (ASan/LSan teeth) for the residual
    /// CAS->marker-gap use-after-free: pre-fix the flag was set only after the
    /// CAS, leaving a window where a trap (`Running -> Crashed`) plus an async
    /// free reclaimed the box under the still-running worker.
    #[test]
    fn activation_claims_ownership_before_running_cas_closes_gap() {
        // Unique ids across repeated runs so the process-wide `LIVE_ACTORS`
        // registry never collides with a prior iteration's actor.
        static GAP_ID: AtomicU64 = AtomicU64::new(900_001);

        // `runtime_test_guard` installs a worker-less default runtime (so
        // `track_actor` / `hew_actor_free` have runtime authority) and holds the
        // shared scheduler-test lock for serialisation.
        let _rt = crate::runtime_test_guard();

        GAP_TRAP_FIRED.store(false, Ordering::SeqCst);
        GAP_FREE_RETURNED.store(i32::MIN, Ordering::SeqCst);
        GAP_FLAG_AT_TRAP.store(false, Ordering::SeqCst);

        let _hook = ActivatePostCasHookGuard::install(trap_then_signal_free);

        // SAFETY: creating a new mailbox with no preconditions.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is non-null and was just created above.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );

        // Track the actor in `LIVE_ACTORS` so the freer thread's `hew_actor_free`
        // runs its full production path (quiescence wait -> untrack -> reclaim).
        // The freer OWNS the box reclaim; the test must not free it again.
        let mut actor = stub_actor();
        actor.dispatch = Some(noop_dispatch);
        actor.mailbox = mailbox.cast();
        actor.actor_state = AtomicI32::new(HewActorState::Runnable as i32);
        actor.id = GAP_ID.fetch_add(1, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = Box::into_raw(Box::new(actor));
        // SAFETY: freshly-boxed, fully-initialised actor.
        unsafe { crate::lifetime::live_actors::track_actor(actor_ptr) };

        // Freer thread: wait for the gap trap, then race `hew_actor_free` against
        // the worker's settle. `hew_actor_free` must block on `dispatch_active`.
        let freer_ptr = actor_ptr as usize;
        let freer = std::thread::spawn(move || {
            while !GAP_TRAP_FIRED.load(Ordering::Acquire) {
                std::hint::spin_loop();
            }
            let p = freer_ptr as *mut HewActor;
            // SAFETY: `p` is the tracked actor box; `hew_actor_free` reclaims it.
            let rc = unsafe { crate::actor::hew_actor_free(p) };
            GAP_FREE_RETURNED.store(rc, Ordering::SeqCst);
        });

        // Worker: the post-CAS hook traps + signals the freer, then this returns
        // through the settle (reading the actor box) while the freer races. With
        // the fix the freer is blocked on `dispatch_active` so these reads are
        // safe; without it ASan/LSan reports the use-after-free here.
        activate_actor(actor_ptr);

        freer.join().expect("freer thread panicked");

        // The flag was set at the trap instant (claimed before the CAS), so the
        // freer's quiescence wait could never observe terminal-without-marker.
        assert!(
            GAP_FLAG_AT_TRAP.load(Ordering::SeqCst),
            "dispatch_active must be set at the gap trap (claimed before the CAS)"
        );
        // The free completed (rc 0) — it waited the worker out, then reclaimed.
        assert_eq!(
            GAP_FREE_RETURNED.load(Ordering::SeqCst),
            0,
            "the concurrent free must succeed after waiting out the activation"
        );

        // The freer's `hew_actor_free` reclaimed BOTH the actor box and its
        // mailbox (and untracked it from `LIVE_ACTORS`). The test owns neither
        // afterward — freeing again would be a double-free. `mailbox` is left
        // unused past this point intentionally.
        let _ = mailbox;
    }

    /// A lost `Runnable -> Running` CAS must clear the `dispatch_active` flag the
    /// worker optimistically set before the CAS — otherwise a non-owning worker
    /// would leave the flag set and wedge a later free to its deadline. This
    /// guards the no-deadlock half of the CAS->marker-gap fix (every failure path
    /// of the claim-before-CAS clears the flag).
    #[test]
    fn lost_running_cas_clears_optimistic_dispatch_active() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let mut actor = stub_actor();
        // `Idle` is not terminal (it passes the early terminal-state skip and
        // reaches the claim + CAS) but is not `Runnable` either, so the
        // `Runnable -> Running` CAS fails — exercising the lost-CAS path that
        // must clear the optimistically-claimed flag.
        actor.actor_state = AtomicI32::new(HewActorState::Idle as i32);
        let actor_ptr: *mut HewActor = Box::into_raw(Box::new(actor));
        activate_actor(actor_ptr);

        // SAFETY: the test exclusively owns the actor.
        let a = unsafe { &*actor_ptr };
        assert!(
            !a.dispatch_active.load(Ordering::Acquire),
            "a worker that lost the Runnable->Running CAS must clear the optimistic flag"
        );
        assert_eq!(
            a.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32,
            "a lost CAS must not change the actor state"
        );

        // SAFETY: the test exclusively owns the actor box.
        unsafe { drop(Box::from_raw(actor_ptr)) };
    }

    /// A dispatch handler that returns a non-null `coro.begin`-shaped handle:
    /// the D-A.2 suspend outcome. Allocates a scratch coroutine frame (leaked as
    /// a raw pointer — the executor's `destroy_parked` reclaims it) and hands its
    /// handle back, modelling a handler that suspended at a non-final
    /// `coro.suspend`. Completes on the 1st resume.
    unsafe extern "C-unwind" fn suspend_once_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut std::ffi::c_void,
        _msg_type: i32,
        _data: *mut std::ffi::c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let frame = Box::new(crate::coro_exec::test_support::ScratchFrame::new(1));
        Box::into_raw(frame).cast::<c_void>()
    }

    /// PRODUCTION SUSPEND EDGE (D-A.2, commit 4): a handler that returns a
    /// non-null handle from the dispatch trampoline drives the dispatch loop to
    /// PARK the activation — CAS `Running → Suspended`, store the handle, break
    /// the message loop without re-enqueue (the worker is freed). This proves the
    /// trampoline-return → `park_suspended_activation` wiring, not just the
    /// executor edge in isolation.
    #[test]
    fn dispatch_returning_handle_parks_the_activation() {
        let _sched = NoWorkerSchedulerForTest::install();
        // SAFETY: fresh mailbox with a single message to drive one dispatch.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 is valid.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );

        let mut actor = stub_actor();
        actor.dispatch = Some(suspend_once_dispatch);
        actor.mailbox = mailbox.cast();
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        activate_actor(actor_ptr);

        // The handler suspended: the dispatch loop parked the returned handle.
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "a handler that returns a non-null handle parks the activation as Suspended"
        );
        assert!(
            !actor.suspended_cont.load(Ordering::Acquire).is_null(),
            "the returned handle is parked in the resume slot"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Parked as i32,
            "the parked cont tag is Parked"
        );

        // Teardown: destroy the parked scratch frame exactly once (the abandoned
        // path the C1 free wiring formalises in the next layer) + free mailbox.
        // SAFETY: the parked handle is live and not yet destroyed.
        assert!(unsafe { crate::coro_exec::destroy_parked(&actor) }.is_ok());
        // SAFETY: single-threaded test; mailbox unused afterwards.
        unsafe {
            let msg = hew_mailbox_try_recv(mailbox.cast::<HewMailbox>());
            if !msg.is_null() {
                hew_msg_node_free(msg);
            }
            mailbox::hew_mailbox_free(mailbox);
        }
    }

    /// PRODUCTION SUSPEND-EDGE LOCK RELEASE (R2 P0, NEW-3a): a handler that
    /// suspends (returns a non-null handle) must leave the actor's per-actor
    /// state lock RELEASED while it is parked, so a sender / another message is
    /// never blocked by a suspended actor holding its lock. Drives the FULL
    /// production message loop (`activate_actor` → lock acquire → dispatch → lock
    /// release → park) with a real dispatch fn + a registered lock, and asserts
    /// the lock is NOT held once the actor is `Suspended`. The lock seat is the
    /// test-mode auto-created sidecar `dispatch_lock_seat_for_actor` resolves.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn production_suspend_edge_releases_the_actor_lock() {
        let _sched = NoWorkerSchedulerForTest::install();
        // SAFETY: fresh mailbox with a single message to drive one dispatch.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 is valid.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );

        let mut actor = stub_actor();
        actor.dispatch = Some(suspend_once_dispatch);
        actor.mailbox = mailbox.cast();
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // Prime the per-actor state lock seat the dispatch loop will
        // acquire/release (the same test-mode sidecar `dispatch_lock_seat_for_actor`
        // resolves), and confirm it starts unheld.
        let _seat = crate::actor::actor_state_lock_seat(actor_ptr);
        assert_eq!(
            crate::actor::actor_state_lock_is_held_for_test(actor_ptr),
            Some(false),
            "the actor lock starts unheld"
        );

        activate_actor(actor_ptr);

        // The handler suspended → the activation is parked as Suspended.
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "the suspending handler parks the activation"
        );
        // The P0: the per-actor lock is RELEASED across the suspend edge — a
        // suspended actor holds no lock against senders.
        assert_eq!(
            crate::actor::actor_state_lock_is_held_for_test(actor_ptr),
            Some(false),
            "the per-actor state lock is released across the suspend edge (R2 P0)"
        );

        // A concurrent send to the Suspended actor must not block (no lock held).
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 is valid.
            unsafe { mailbox::hew_mailbox_send(mailbox, 2, ptr::null_mut(), 0) },
            0,
            "a send to a Suspended actor must not block"
        );

        // Teardown: destroy the parked frame once + drain + free mailbox.
        // SAFETY: the parked handle is live and not yet destroyed.
        assert!(unsafe { crate::coro_exec::destroy_parked(&actor) }.is_ok());
        // SAFETY: single-threaded test; mailbox unused afterwards.
        unsafe {
            loop {
                let msg = hew_mailbox_try_recv(mailbox.cast::<HewMailbox>());
                if msg.is_null() {
                    break;
                }
                hew_msg_node_free(msg);
            }
            mailbox::hew_mailbox_free(mailbox);
        }
    }

    /// A dispatch handler that suspends and stays Pending across TWO resumes
    /// before completing — models a handler that suspends at a non-final
    /// `coro.suspend`, is resumed once (still Pending), then completes on the
    /// second resume. The trampoline-equivalent first poll is modelled by
    /// returning the handle (Pending); the executor drives the rest.
    unsafe extern "C-unwind" fn suspend_twice_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut std::ffi::c_void,
        _msg_type: i32,
        _data: *mut std::ffi::c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let frame = Box::new(crate::coro_exec::test_support::ScratchFrame::new(2));
        Box::into_raw(frame).cast::<c_void>()
    }

    /// FULL PRODUCTION ROUND-TRIP (D-4 Pending-then-resume): a suspendable
    /// handler's trampoline-produced handle is parked, woken via the production
    /// wake edge (`enqueue_resume`), resumed to completion, and destroyed exactly
    /// once — driving `dispatch → park → enqueue_resume → resume(Pending) →
    /// re-park → enqueue_resume → resume(Ready) → destroy → settle`. Proves the
    /// trampoline-produced handle (not a bare seed) completes the executor cycle
    /// with no leak.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn production_round_trip_pending_then_resume_to_ready_destroys_once() {
        let _sched = NoWorkerSchedulerForTest::install();
        // SAFETY: fresh mailbox with one message to drive the first dispatch.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 is valid.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );

        let mut stub = stub_actor();
        stub.dispatch = Some(suspend_twice_dispatch);
        stub.mailbox = mailbox.cast();
        stub.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        // Tracked: the production wake edge (`enqueue_resume`) now confirms
        // registry liveness before resuming.
        let actor = TrackedTestActor::install(stub);
        let actor_ptr = actor.ptr();

        // Dispatch → the handler suspends; the activation parks.
        activate_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "the suspending handler parks the activation"
        );
        let parked_handle = actor.suspended_cont.load(Ordering::Acquire);
        assert!(!parked_handle.is_null(), "a handle is parked");

        // Wake #1: enqueue_resume → Runnable → activate → resume #1 (Pending) →
        // re-park as Suspended.
        // SAFETY: actor live; parked handle is the executor-owned frame.
        unsafe { enqueue_resume(actor_ptr, parked_handle) };
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32,
            "the wake CASes Suspended -> Runnable"
        );
        activate_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "a Pending resume re-parks the activation"
        );

        // Wake #2: enqueue_resume → activate → resume #2 (Ready) → destroy once →
        // settle to Idle (empty mailbox).
        // SAFETY: actor live; same parked handle.
        unsafe { enqueue_resume(actor_ptr, parked_handle) };
        activate_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32,
            "a completed resume with an empty mailbox settles to Idle"
        );
        assert!(
            actor.suspended_cont.load(Ordering::Acquire).is_null(),
            "FG4: the slot is nulled when the Ready continuation is destroyed"
        );
        // P1-B: the completed continuation is destroyed exactly once (the slot
        // is null above) and the tag is RE-ARMED `Destroyed -> Empty` on the
        // quiescent edge, so this actor can park a NEW continuation on its next
        // `await` rather than being stuck terminal (the one-shot leak this lane
        // closes).
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Empty as i32,
            "P1-B: the completed continuation is reclaimed and the tag re-armed to Empty"
        );

        // SAFETY: single-threaded test; mailbox unused afterwards.
        unsafe { mailbox::hew_mailbox_free(mailbox) };
    }

    /// P1-B multi-await at the SCHEDULER layer: one actor parks, is woken,
    /// resumes to completion, is destroyed and RE-ARMED, then services a SECOND
    /// message that ALSO suspends — parking a NEW continuation. Without the
    /// quiescent re-arm the second dispatch's `begin_park` would refuse from the
    /// terminal `Destroyed` tag, leaking the second handle and dropping the
    /// activation (the fail-OPEN this lane closes). Both continuations are
    /// destroyed exactly once; the actor ends armed (`Empty`) for a third await.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn p1b_multi_await_parks_and_completes_twice_on_one_actor() {
        let _sched = NoWorkerSchedulerForTest::install();
        // SAFETY: fresh mailbox; two messages drive two dispatches.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        // SAFETY: mailbox live; null payloads of size 0 are valid.
        unsafe {
            assert_eq!(mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0), 0);
            assert_eq!(mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0), 0);
        }

        let mut stub = stub_actor();
        stub.dispatch = Some(suspend_once_dispatch);
        stub.mailbox = mailbox.cast();
        stub.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        // Tracked: the production wake edge (`enqueue_resume`) now confirms
        // registry liveness before resuming.
        let actor = TrackedTestActor::install(stub);
        let actor_ptr = actor.ptr();

        // ── First await cycle. ──
        // Dispatch → the handler suspends; the activation parks the 1st handle.
        activate_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "first dispatch parks the activation"
        );
        let handle1 = actor.suspended_cont.load(Ordering::Acquire);
        assert!(!handle1.is_null(), "first handle is parked");
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Parked as i32
        );

        // Wake → resume(Ready) → destroy once → re-arm to Empty. The mailbox
        // still has the 2nd message, so the completed resume settles to Runnable
        // (not Idle) and is re-enqueued.
        // SAFETY: actor live; handle1 is the executor-owned frame.
        unsafe { enqueue_resume(actor_ptr, handle1) };
        activate_actor(actor_ptr);
        assert!(
            actor.suspended_cont.load(Ordering::Acquire).is_null(),
            "FG4: first handle's slot nulled on destroy"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Empty as i32,
            "P1-B: tag re-armed to Empty after the first completion"
        );

        // ── Second await cycle (the one a one-shot tag would leak). ──
        // Ensure the actor is Runnable to service the 2nd message, then dispatch.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        activate_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "second dispatch parks a NEW continuation (re-arm worked)"
        );
        let handle2 = actor.suspended_cont.load(Ordering::Acquire);
        assert!(!handle2.is_null(), "second handle is parked");
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Parked as i32
        );

        // Wake → resume(Ready) → destroy once → re-arm. Mailbox now empty → Idle.
        // SAFETY: actor live; handle2 is the executor-owned frame.
        unsafe { enqueue_resume(actor_ptr, handle2) };
        activate_actor(actor_ptr);
        assert!(
            actor.suspended_cont.load(Ordering::Acquire).is_null(),
            "FG4: second handle's slot nulled on destroy"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Empty as i32,
            "P1-B: tag re-armed to Empty after the second completion (ready for a third)"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32,
            "the actor settles to Idle once the mailbox drains"
        );

        // SAFETY: single-threaded test; mailbox unused afterwards.
        unsafe { mailbox::hew_mailbox_free(mailbox) };
    }

    /// D-4 Ready-immediately: a run-to-completion dispatch (the trampoline drove
    /// the coroutine to Ready in one step, deposited the reply, and returned a
    /// null handle) does NOT park — the activation settles normally. This is the
    /// other half of the D-4 shape pair: a suspendable handler that reaches its
    /// final suspend on the first poll completes this dispatch with no park.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn ready_immediately_dispatch_does_not_park() {
        let _sched = NoWorkerSchedulerForTest::install();
        // SAFETY: fresh mailbox with one message.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 is valid.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );

        let mut actor = stub_actor();
        // noop_dispatch returns null — modelling a handler the trampoline drove
        // to Ready on the first poll (reply deposited, no park).
        actor.dispatch = Some(noop_dispatch);
        actor.mailbox = mailbox.cast();
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        activate_actor(actor_ptr);

        // No park: the actor never reaches Suspended, the slot stays empty.
        assert_ne!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32,
            "a Ready-immediately dispatch must NOT park the activation"
        );
        assert!(
            actor.suspended_cont.load(Ordering::Acquire).is_null(),
            "no handle is parked for a Ready-immediately dispatch"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Empty as i32,
            "the cont tag stays Empty when nothing parks"
        );

        // SAFETY: single-threaded test; mailbox unused afterwards.
        unsafe { mailbox::hew_mailbox_free(mailbox) };
    }

    #[test]
    fn hew_sched_init_returns_zero_on_success() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // hew_sched_init is idempotent — second call is a no-op returning 0.
        let result = hew_sched_init();
        assert_eq!(result, 0);
    }

    /// The ticker thread must be stopped during runtime cleanup so it
    /// doesn't access freed timer-wheel memory.  We start the global
    /// wheel (which spawns the ticker), then call `hew_runtime_cleanup`
    /// and verify the ticker is stopped.
    #[test]
    fn ticker_stops_during_runtime_cleanup() {
        use crate::timer_periodic::{TICKER_RUNNING, TICKER_TEST_MUTEX};

        // Serialise against the actor/monitor/link test family (all of which hold
        // the shared scheduler-test lock via `runtime_test_guard`):
        // `hew_runtime_cleanup` → `cleanup_all_actors` frees EVERY registered
        // actor, so it must not run while a parallel test still has a live actor
        // (ASan heap-use-after-free). This test manages its own runtime lifecycle
        // (it calls `hew_runtime_cleanup` directly), so it takes the shared lock
        // without installing a runtime, then drives the global wheel itself.
        let _sched_guard = SchedTestLock::acquire();

        // Hold the shared ticker mutex for the duration of this test so it
        // cannot race with timer_periodic tests that poll the same globals.
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Install a worker-less runtime so `hew_runtime_cleanup`'s runtime-owned
        // sweep (cleanup_all_actors → drain via rt_current) has a runtime to
        // read; cleanup detaches and drops it as its final step.
        install_scheduler_for_test(worker_less_scheduler_for_test());

        // Start the global wheel so the ticker is running.
        let _tw = crate::timer_periodic::global_wheel();
        std::thread::sleep(std::time::Duration::from_millis(20));

        // The ticker may have been stopped by a parallel test that shares
        // the global wheel.  We can only assert the post-condition.
        hew_runtime_cleanup();

        assert!(
            !TICKER_RUNNING.load(Ordering::Acquire),
            "Ticker must be stopped after runtime cleanup"
        );
    }

    /// Build a worker-less scheduler suitable for deterministic lifecycle tests.
    fn worker_less_scheduler_for_test() -> Scheduler {
        super::worker_less_scheduler()
    }

    /// Drop-order / release-count oracle for the M1 de-globalization: the
    /// default runtime is detached from its slot *before* it is freed, and it
    /// is freed exactly once on cleanup. A leak detector running green is
    /// non-evidence for this (thread-backed handle leaks are invisible to it);
    /// the honest oracle is the exact `RUNTIME_INNER_DROPS` count below.
    #[test]
    fn default_runtime_detaches_before_free_and_drops_exactly_once() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let before = runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst);
        let _rt_ptr = install_scheduler_for_test(worker_less_scheduler_for_test());

        // While installed, nothing has been freed.
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before,
            "installing the runtime must not free anything"
        );

        // Detach: the slot is cleared, but the box is still alive (not yet freed)
        // — proving the free is driven by the explicit drop, never by the slot.
        let taken = runtime::take_default().expect("a runtime was installed");
        assert!(
            runtime::default_runtime_ptr(Ordering::SeqCst).is_null(),
            "take_default must clear the slot before the runtime is freed"
        );
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before,
            "detaching the runtime from its slot must not free it"
        );

        // Explicit drop is the single free site — exactly once, never twice.
        drop(taken);
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before + 1,
            "the detached runtime must be freed exactly once on explicit drop"
        );

        // The slot is empty and a second teardown is a harmless no-op (no double
        // free): take_default returns None and the drop count is unchanged.
        assert!(runtime::take_default().is_none());
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before + 1,
            "a second teardown of an empty slot must not free anything"
        );
    }

    /// A second `install_default` is a harmless no-op that frees the rejected
    /// runtime exactly once (the "second `hew_sched_init` is a no-op" contract):
    /// no leak of the loser, no displacement of the installed runtime.
    #[test]
    fn second_install_is_noop_and_frees_the_rejected_runtime_once() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let first = install_scheduler_for_test(worker_less_scheduler_for_test());
        let before = runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst);

        // Attempt to install a second runtime: rejected (slot occupied).
        let installed = runtime::install_default(Box::new(RuntimeInner::new(
            worker_less_scheduler_for_test(),
        )));
        assert!(!installed, "second install must be rejected");
        // The rejected runtime is freed immediately, exactly once.
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before + 1,
            "install_default must free the rejected runtime exactly once"
        );
        // The originally-installed runtime is still the one in the slot.
        assert_eq!(
            runtime::default_runtime_ptr(Ordering::SeqCst),
            first,
            "second install must not displace the installed runtime"
        );

        take_default_runtime_for_test();
    }

    #[test]
    fn spawn_failure_teardown_joins_partial_worker_set_and_drops_scheduler() {
        use std::sync::Arc;

        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let sched = Scheduler {
            worker_count: 1,
            parkers: vec![Parker {
                mutex: Mutex::new(()),
                cond: Condvar::new(),
            }],
            stealers: Vec::new(),
            worker_handles: PoisonSafe::new(Vec::new()),
            // SAFETY: single-threaded test setup with scheduler-owned queue state.
            global_queue: unsafe { crate::deque::GlobalQueue::new() },
            shutdown: AtomicBool::new(false),
        };
        let _rt_ptr = install_scheduler_for_test(sched);

        let exited = Arc::new(AtomicBool::new(false));
        let exited2 = Arc::clone(&exited);
        let handle = thread::spawn(move || {
            while get_scheduler().is_some_and(|sched| !sched.shutdown.load(Ordering::Acquire)) {
                thread::sleep(Duration::from_millis(10));
            }
            exited2.store(true, Ordering::Release);
        });

        teardown_after_spawn_failure(vec![Some(handle)]);

        assert!(
            exited.load(Ordering::Acquire),
            "spawn-failure teardown must join every spawned worker"
        );
        assert!(
            runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
            "spawn-failure teardown must clear the default runtime pointer"
        );
    }

    /// `hew_sched_shutdown` must skip joining the calling thread's own
    /// handle to avoid self-join deadlock.  We insert the spawned
    /// thread's `JoinHandle` into `worker_handles` and then call
    /// `hew_sched_shutdown` from that same thread — without the fix
    /// this would deadlock.
    #[test]
    fn shutdown_skips_self_join() {
        use std::sync::Arc;
        use std::time::Instant;
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let parker = Parker {
            mutex: Mutex::new(()),
            cond: Condvar::new(),
        };
        let sched = Scheduler {
            worker_count: 1,
            parkers: vec![parker],
            stealers: Vec::new(),
            worker_handles: PoisonSafe::new(Vec::new()),
            // SAFETY: no preconditions for GlobalQueue::new().
            global_queue: unsafe { crate::deque::GlobalQueue::new() },
            shutdown: AtomicBool::new(false),
        };
        let _rt_ptr = install_scheduler_for_test(sched);

        let barrier = Arc::new(std::sync::Barrier::new(2));
        let done = Arc::new(AtomicBool::new(false));
        let done2 = Arc::clone(&done);
        let barrier2 = Arc::clone(&barrier);

        let handle = thread::spawn(move || {
            // Wait until our own handle has been inserted into worker_handles.
            barrier2.wait();
            // Now worker_handles contains our own JoinHandle.  Without
            // the self-join skip this would deadlock.
            hew_sched_shutdown();
            done2.store(true, Ordering::Release);
        });

        // Insert the thread's handle into worker_handles so
        // hew_sched_shutdown will encounter it during the join loop.
        {
            let sched = get_scheduler().expect("scheduler installed above");
            sched
                .worker_handles
                .access(|handles| handles.push(Some(handle)));
        }
        // Release the spawned thread to call hew_sched_shutdown.
        barrier.wait();

        // Poll for completion — 2 s timeout detects deadlock.
        let deadline = Instant::now() + Duration::from_secs(2);
        while !done.load(Ordering::Acquire) && Instant::now() < deadline {
            thread::sleep(Duration::from_millis(10));
        }
        assert!(
            done.load(Ordering::Acquire),
            "hew_sched_shutdown deadlocked on self-join"
        );

        // Clean up the default runtime.
        take_default_runtime_for_test();
    }

    #[test]
    fn shutdown_releases_worker_handles_while_join_pending() {
        use std::sync::Arc;
        use std::time::Instant;

        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let sched = Scheduler {
            worker_count: 1,
            parkers: vec![Parker {
                mutex: Mutex::new(()),
                cond: Condvar::new(),
            }],
            stealers: Vec::new(),
            worker_handles: PoisonSafe::new(Vec::new()),
            // SAFETY: single-threaded test setup with scheduler-owned queue state.
            global_queue: unsafe { crate::deque::GlobalQueue::new() },
            shutdown: AtomicBool::new(false),
        };
        let rt_ptr = install_scheduler_for_test(sched);
        // Stable borrow of the installed scheduler for this test's field reads;
        // valid until `take_default_runtime_for_test` frees the runtime below.
        // SAFETY: `rt_ptr` is live for the whole test (held under SCHED_TEST_MUTEX).
        let sched_ptr: *const Scheduler = unsafe { &raw const (*rt_ptr).scheduler };

        let release_worker = Arc::new(AtomicBool::new(false));
        let release_worker2 = Arc::clone(&release_worker);
        let handle = thread::spawn(move || {
            while !release_worker2.load(Ordering::Acquire) {
                thread::sleep(Duration::from_millis(10));
            }
        });
        // SAFETY: sched_ptr borrows the installed runtime, valid until cleanup below.
        unsafe { &*sched_ptr }
            .worker_handles
            .access(|handles| handles.push(Some(handle)));

        let shutdown_done = Arc::new(AtomicBool::new(false));
        let shutdown_done2 = Arc::clone(&shutdown_done);
        let shutdown_thread = thread::spawn(move || {
            hew_sched_shutdown();
            shutdown_done2.store(true, Ordering::Release);
        });

        let deadline = Instant::now() + Duration::from_secs(2);
        while {
            // SAFETY: the scheduler remains installed until cleanup below.
            let sched = unsafe { &*sched_ptr };
            !sched.shutdown.load(Ordering::Acquire)
        } && Instant::now() < deadline
        {
            thread::sleep(Duration::from_millis(10));
        }

        let touch_succeeded = {
            // SAFETY: the scheduler remains installed until cleanup below.
            let sched = unsafe { &*sched_ptr };
            let mut touched = false;
            while Instant::now() < deadline {
                if sched
                    .worker_handles
                    .try_access(|handles| assert!(handles.is_empty()))
                    .is_some()
                {
                    touched = true;
                    break;
                }
                thread::sleep(Duration::from_millis(10));
            }
            touched
        };
        assert!(
            touch_succeeded,
            "worker_handles must stay accessible while shutdown joins a blocked worker"
        );
        assert!(
            !shutdown_done.load(Ordering::Acquire),
            "shutdown should still be waiting on the worker join during the concurrent touch"
        );

        release_worker.store(true, Ordering::Release);
        shutdown_thread.join().unwrap();
        assert!(shutdown_done.load(Ordering::Acquire));
        // SAFETY: sched_ptr remains valid until the cleanup swap below.
        unsafe { &*sched_ptr }
            .worker_handles
            .access(|handles| assert!(handles.is_empty()));

        take_default_runtime_for_test();
    }

    #[test]
    fn runtime_cleanup_joins_workers_and_drops_scheduler() {
        use std::sync::Arc;
        use std::time::Instant;

        // Serialise against the actor/monitor/link test family (all of which hold
        // the shared scheduler-test lock via `runtime_test_guard`): this test runs
        // the process-global `hew_runtime_cleanup` → `cleanup_all_actors`, which
        // frees EVERY actor in the registry. Without serialization it tears down
        // an actor a parallel libtest thread is still using (ASan
        // heap-use-after-free). This test installs and manages its own runtime, so
        // it takes the shared lock directly (no guard-installed runtime).
        let _sched_guard = SchedTestLock::acquire();

        let rt_ptr = install_scheduler_for_test(worker_less_scheduler_for_test());
        // Stable borrow of the installed scheduler; valid until hew_runtime_cleanup
        // detaches and drops the runtime below.
        // SAFETY: `rt_ptr` is live until cleanup (held under SCHED_TEST_MUTEX).
        let sched_ptr: *const Scheduler = unsafe { &raw const (*rt_ptr).scheduler };

        let release_worker = Arc::new(AtomicBool::new(false));
        let release_worker2 = Arc::clone(&release_worker);
        let joined = Arc::new(AtomicBool::new(false));
        let joined2 = Arc::clone(&joined);
        let handle = thread::spawn(move || {
            while !release_worker2.load(Ordering::Acquire) {
                thread::sleep(Duration::from_millis(10));
            }
            joined2.store(true, Ordering::Release);
        });
        // SAFETY: sched_ptr borrows the installed runtime, valid until runtime cleanup.
        unsafe { &*sched_ptr }
            .worker_handles
            .access(|handles| handles.push(Some(handle)));

        let cleanup_done = Arc::new(AtomicBool::new(false));
        let cleanup_done2 = Arc::clone(&cleanup_done);
        let cleanup_thread = thread::spawn(move || {
            hew_runtime_cleanup();
            cleanup_done2.store(true, Ordering::Release);
        });

        let deadline = Instant::now() + Duration::from_secs(2);
        while {
            // SAFETY: the scheduler remains installed until runtime cleanup swaps it out.
            let sched = unsafe { &*sched_ptr };
            !sched.shutdown.load(Ordering::Acquire)
        } && Instant::now() < deadline
        {
            thread::sleep(Duration::from_millis(10));
        }
        release_worker.store(true, Ordering::Release);

        cleanup_thread.join().unwrap();
        assert!(cleanup_done.load(Ordering::Acquire));
        assert!(
            joined.load(Ordering::Acquire),
            "runtime cleanup must join any remaining worker handles"
        );
        assert!(
            runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
            "runtime cleanup must clear the default runtime pointer"
        );
    }

    /// Drop-order oracle for the live-actors-on-runtime cleanup restructure:
    /// `hew_runtime_cleanup` must run the runtime-owned sweep (joining the
    /// deferred-teardown reapers, which read `rt_current()`) WHILE the runtime
    /// is still installed, and only THEN detach + drop it — exactly once.
    ///
    /// This is the load-bearing ordering: on the pre-restructure path the
    /// runtime was detached up-front, so `cleanup_all_actors` →
    /// `drain_deferred_teardown_threads` → `rt_current()` would trap on a
    /// missing runtime mid-cleanup. A registered-but-not-yet-joined reaper that
    /// the cleanup joins is the proof the sweep saw an installed runtime; the
    /// exact `RUNTIME_INNER_DROPS` delta proves the drop is the single final
    /// step, never doubled and never driven by detaching the slot.
    #[test]
    fn runtime_cleanup_sweeps_before_detach_and_drops_runtime_once() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        install_scheduler_for_test(worker_less_scheduler_for_test());

        // Register a deferred-teardown reaper on the runtime-owned registry,
        // gated open by `release`. `cleanup_all_actors` (inside cleanup) must
        // JOIN it before sweeping — and must do so against an installed runtime,
        // or the join's `rt_current()` would trap.
        let release = std::sync::Arc::new(AtomicBool::new(false));
        let joined = std::sync::Arc::new(AtomicBool::new(false));
        let release2 = std::sync::Arc::clone(&release);
        let joined2 = std::sync::Arc::clone(&joined);
        crate::lifetime::live_actors::push_deferred_teardown_thread(thread::spawn(move || {
            while !release2.load(Ordering::Acquire) {
                thread::sleep(Duration::from_millis(5));
            }
            joined2.store(true, Ordering::Release);
        }));

        let before = runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst);

        let cleanup_done = std::sync::Arc::new(AtomicBool::new(false));
        let cleanup_done2 = std::sync::Arc::clone(&cleanup_done);
        let cleanup = thread::spawn(move || {
            hew_runtime_cleanup();
            cleanup_done2.store(true, Ordering::Release);
        });

        // Cleanup is blocked inside the runtime-owned sweep joining the gated
        // reaper: the runtime must still be installed (not detached up-front),
        // and it must not have been dropped yet.
        thread::sleep(Duration::from_millis(40));
        assert!(
            !cleanup_done.load(Ordering::Acquire),
            "cleanup must block in the runtime-owned sweep until the reaper is joined"
        );
        assert!(
            !runtime::default_runtime_ptr(Ordering::SeqCst).is_null(),
            "runtime must stay installed through the sweep — detach is the FINAL step"
        );
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before,
            "runtime must not be dropped before the sweep completes"
        );

        // Release the reaper; cleanup joins it, then detaches + drops last.
        release.store(true, Ordering::Release);
        cleanup.join().unwrap();

        assert!(
            joined.load(Ordering::Acquire),
            "the runtime-owned sweep must have joined the deferred-teardown reaper"
        );
        assert!(
            runtime::default_runtime_ptr(Ordering::SeqCst).is_null(),
            "cleanup must detach the runtime as its final step"
        );
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before + 1,
            "cleanup must drop the runtime exactly once as the final teardown step"
        );
    }

    /// Drop-order oracle for the supervisor-roots de-globalization: a registered
    /// top-level supervisor root lives in the runtime-owned `supervisor_roots`
    /// list, and `hew_runtime_cleanup` frees it via `free_registered_supervisors`
    /// **while the runtime is still installed** — before the runtime is detached
    /// and dropped.
    ///
    /// The proof is the ordering, not a leak count: `free_registered_supervisors`
    /// (which reads `rt_current().supervisor_roots`) runs strictly before the
    /// live-actor sweep inside cleanup. We gate cleanup mid-sweep on a deferred
    /// reaper join (which happens during the *later* `cleanup_all_actors` step)
    /// and observe that, at that point, the supervisor root has already been
    /// swept out of the runtime-owned list while the runtime is still installed
    /// and not yet dropped. If the sweep instead read a detached runtime,
    /// `rt_current()` would trap and cleanup would never reach the join.
    #[test]
    fn runtime_cleanup_frees_supervisor_root_before_detach() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        install_scheduler_for_test(worker_less_scheduler_for_test());

        // Register a top-level supervisor root on the runtime-owned list.
        // SAFETY: hew_supervisor_new returns a valid owned supervisor pointer.
        let sup = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: sup is a valid pointer returned by hew_supervisor_new.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        assert!(
            crate::shutdown::is_supervisor_registered_for_test(sup),
            "supervisor root must be registered on the runtime-owned list before cleanup"
        );

        // Gate cleanup inside the LATER live-actor sweep (the deferred-teardown
        // join), so when it blocks here `free_registered_supervisors` has already
        // run. The reaper dereferences the runtime-owned live-actor registry, so
        // the runtime must still be installed for cleanup to reach the join.
        let release = std::sync::Arc::new(AtomicBool::new(false));
        let release2 = std::sync::Arc::clone(&release);
        crate::lifetime::live_actors::push_deferred_teardown_thread(thread::spawn(move || {
            while !release2.load(Ordering::Acquire) {
                thread::sleep(Duration::from_millis(5));
            }
        }));

        let before = runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst);

        let cleanup = thread::spawn(|| hew_runtime_cleanup());

        // Cleanup is now blocked in the actor-sweep join — which is AFTER the
        // supervisor-roots sweep. While blocked, the runtime is still installed
        // and not yet dropped, and the supervisor root has already been freed.
        thread::sleep(Duration::from_millis(40));
        assert!(
            !runtime::default_runtime_ptr(Ordering::SeqCst).is_null(),
            "runtime must stay installed through the supervisor-roots sweep"
        );
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before,
            "runtime must not be dropped before the supervisor-roots sweep completes"
        );
        assert!(
            !crate::shutdown::is_supervisor_registered_for_test(sup),
            "free_registered_supervisors must empty the runtime-owned supervisor \
             roots while the runtime is still installed, before detach/drop"
        );

        // Let cleanup finish: it joins the reaper, then detaches + drops last.
        release.store(true, Ordering::Release);
        cleanup.join().unwrap();

        assert!(
            runtime::default_runtime_ptr(Ordering::SeqCst).is_null(),
            "cleanup must detach the runtime as its final step"
        );
        assert_eq!(
            runtime::RUNTIME_INNER_DROPS.load(Ordering::SeqCst),
            before + 1,
            "cleanup must drop the runtime exactly once after sweeping supervisor roots"
        );
    }

    #[test]
    fn drain_is_idle_requires_empty_scheduler() {
        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let parker = Parker {
            mutex: Mutex::new(()),
            cond: Condvar::new(),
        };
        // SAFETY: single-threaded test setup; the deque lives for the whole test.
        let (queued_work, queued_stealer) = unsafe { crate::deque::WorkDeque::new() };
        let sched = Scheduler {
            worker_count: 1,
            parkers: vec![parker],
            stealers: vec![queued_stealer],
            worker_handles: PoisonSafe::new(Vec::new()),
            // SAFETY: single-threaded test setup with scheduler-owned queue state.
            global_queue: unsafe { crate::deque::GlobalQueue::new() },
            shutdown: AtomicBool::new(false),
        };
        let rt_ptr = install_scheduler_for_test(sched);
        // Stable borrow of the installed scheduler; valid until teardown below.
        // SAFETY: `rt_ptr` is live for the whole test (held under SCHED_TEST_MUTEX).
        let sched_ptr: *const Scheduler = unsafe { &raw const (*rt_ptr).scheduler };

        ACTIVE_WORKERS.store(0, Ordering::Release);
        assert!(
            drain_is_idle(),
            "empty scheduler should be considered drained"
        );

        ACTIVE_WORKERS.store(1, Ordering::Release);
        assert!(
            !drain_is_idle(),
            "active worker must keep drain wait alive until dispatch completes"
        );

        ACTIVE_WORKERS.store(0, Ordering::Release);
        // SAFETY: sched_ptr was allocated above and remains owned by this test.
        unsafe {
            (&*sched_ptr)
                .global_queue
                .push(std::ptr::dangling_mut::<()>());
        };
        assert!(
            !drain_is_idle(),
            "global queue work must keep drain wait alive"
        );
        // SAFETY: sched_ptr remains valid until the cleanup swap below.
        let drain_local = unsafe { &*sched_ptr };
        assert!(
            drain_local
                .global_queue
                .steal_batch_and_pop(&queued_work)
                .is_some(),
            "test setup must be able to drain injected work"
        );
        while queued_work.pop().is_some() {}

        queued_work.push(std::ptr::dangling_mut::<()>());
        assert!(
            !drain_is_idle(),
            "local worker deque work must keep drain wait alive"
        );
        assert_eq!(queued_work.pop(), Some(std::ptr::dangling_mut::<()>()));
        assert!(
            drain_is_idle(),
            "scheduler should report drained after work clears"
        );

        take_default_runtime_for_test();
        ACTIVE_WORKERS.store(0, Ordering::Release);
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "single scenario test keeps setup, hook assertion, and cleanup together"
    )]
    fn activate_keeps_worker_active_until_reenqueue_decision_finishes() {
        static HOOK_SEEN: AtomicBool = AtomicBool::new(false);

        fn assert_pending_work_still_counts_active(_actor: *mut HewActor) {
            HOOK_SEEN.store(true, Ordering::Release);
            assert_eq!(
                ACTIVE_WORKERS.load(Ordering::Acquire),
                1,
                "worker must stay active until re-enqueue decision is resolved"
            );
            assert!(
                !drain_is_idle(),
                "shutdown drain must not observe idle while activation still owns pending work"
            );
        }

        let _g = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let _hook = ActivatePreReenqueueHookGuard::install(assert_pending_work_still_counts_active);
        HOOK_SEEN.store(false, Ordering::Release);

        let parker = Parker {
            mutex: Mutex::new(()),
            cond: Condvar::new(),
        };
        let sched = Scheduler {
            worker_count: 1,
            parkers: vec![parker],
            stealers: Vec::new(),
            worker_handles: PoisonSafe::new(Vec::new()),
            // SAFETY: single-threaded test setup with scheduler-owned queue state.
            global_queue: unsafe { crate::deque::GlobalQueue::new() },
            shutdown: AtomicBool::new(false),
        };
        let rt_ptr = install_scheduler_for_test(sched);
        // Stable borrow of the installed scheduler; valid until teardown below.
        // SAFETY: `rt_ptr` is live for the whole test (held under SCHED_TEST_MUTEX).
        let sched_ptr: *const Scheduler = unsafe { &raw const (*rt_ptr).scheduler };

        // SAFETY: mailbox is created for this test and freed before exit.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is valid and owned by this test.
            unsafe { mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) },
            0
        );
        assert_eq!(
            // SAFETY: mailbox is valid and owned by this test.
            unsafe { mailbox::hew_mailbox_send(mailbox, 2, ptr::null_mut(), 0) },
            0
        );

        let actor = HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 7,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: mailbox.cast(),
            actor_state: AtomicI32::new(HewActorState::Runnable as i32),
            budget: AtomicI32::new(1),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(actor::HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: ptr::null_mut(),
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
        };
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        ACTIVE_WORKERS.store(0, Ordering::Release);
        activate_actor(actor_ptr);

        assert!(
            HOOK_SEEN.load(Ordering::Acquire),
            "test hook must run before the re-enqueue decision"
        );
        assert_eq!(ACTIVE_WORKERS.load(Ordering::Acquire), 0);
        // SAFETY: sched_ptr remains valid until the cleanup swap below.
        let sched = unsafe { &*sched_ptr };
        assert!(
            !sched.global_queue.is_empty(),
            "pending mailbox work must be re-enqueued before the worker becomes idle"
        );

        // Drain the scheduled actor pointer so the scheduler can be dropped cleanly.
        // SAFETY: single-threaded test cleanup; the deque lives for the rest of the test.
        let (drain_deque, _drain_stealer) = unsafe { crate::deque::WorkDeque::new() };
        assert_eq!(
            sched.global_queue.steal_batch_and_pop(&drain_deque),
            Some(actor_ptr.cast())
        );
        while drain_deque.pop().is_some() {}

        // SAFETY: mailbox is owned by this test and no longer referenced after activation.
        unsafe { mailbox::hew_mailbox_free(mailbox) };

        take_default_runtime_for_test();
        ACTIVE_WORKERS.store(0, Ordering::Release);
    }

    thread_local! {
        /// Passes the test's driver-owned channel into the C-ABI dispatch fn
        /// (which cannot take extra args) so the handler can open the swap with
        /// the exact channel the test created and later verifies is torn down.
        static CATCH_UNWIND_SWAP_DRIVER: std::cell::Cell<*mut c_void> =
            const { std::cell::Cell::new(std::ptr::null_mut()) };
    }

    /// A dispatch handler that models a child suspending-closure call which
    /// Rust-unwinds (panics) WHILE its scoped reply-channel swap is still open —
    /// exactly the bypass the `catch_unwind` `Err` edge must clean up. It
    /// installs the driver channel as a scoped swap (as the
    /// `SuspendingCallClosure` driver does) and then panics BEFORE the codegen
    /// swap-pop runs, so the swap is left open across the unwind.
    unsafe extern "C-unwind" fn swap_open_then_panic_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut std::ffi::c_void,
        _msg_type: i32,
        _data: *mut std::ffi::c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let driver = CATCH_UNWIND_SWAP_DRIVER.with(std::cell::Cell::get);
        crate::execution_context::hew_context_reply_channel_swap_push(driver);
        panic!("suspending-closure child Rust-unwound with the reply-channel swap open");
    }

    /// SEC-2 SIBLING EDGE regression: a Rust unwind caught by the dispatcher's
    /// `catch_unwind` (`dispatch_result == Err`) that crossed a child
    /// suspending-closure ramp while the scoped reply-channel swap was still
    /// OPEN must be cleaned up on the `Err` teardown edge, mirroring the native
    /// siglongjmp crash branch. Drives the FULL production dispatch loop
    /// (`activate_actor` → lock acquire → `catch_unwind(dispatch)` → `Err` →
    /// swap unwind → teardown) with a real ask message carrying the OUTER reply
    /// channel and a handler that opens the swap then panics, and asserts:
    ///   * the driver-owned channel is torn down exactly once (no leak), and
    ///   * ONLY the driver — the OUTER ask channel is left intact, so the outer
    ///     ask still routes to its own channel (not the driver channel), and
    ///   * with no double-free: the outer channel is still a live, free-once
    ///     channel and the swap stack is fully drained.
    ///
    /// Pre-fix the `Err` edge skipped `reply_channel_swap_unwind`, so the driver
    /// channel leaked and the teardown read/cleared the wrong (driver) channel
    /// and nulled the real outer message reply.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn catch_unwind_err_edge_unwinds_open_swap_and_frees_driver_channel() {
        // This test measures `active_channel_count()` deltas, so it must hold
        // the crate-wide serialisation guard for its entire body (the
        // documented convention in `reply_channel::tests`) — a concurrent
        // test allocating or freeing a reply channel corrupts the deltas.
        // Acquired outermost, before the scheduler install, matching the
        // crate's global lock order.
        let _rt = crate::runtime_test_guard();
        let _sched = NoWorkerSchedulerForTest::install();

        let baseline = crate::reply_channel::active_channel_count();
        assert_eq!(
            crate::execution_context::reply_channel_swap_stack_depth(),
            0,
            "the swap stack starts empty"
        );

        // OUTER ask channel: the message's reply channel — the real outer
        // routing lane that must survive the child's unwind. One ref (the
        // sender-side ref an ask mints and the node carries).
        let outer_ch = crate::reply_channel::hew_reply_channel_new();
        assert!(!outer_ch.is_null());

        // DRIVER-owned channel for the child suspending-closure swap: new
        // (creator ref) + retain (sender ref), exactly as the
        // `SuspendingCallClosure` driver wires it. The unwind must free BOTH
        // refs because the child never deposited.
        let driver_ch = crate::reply_channel::hew_reply_channel_new();
        assert!(!driver_ch.is_null());
        // SAFETY: driver_ch was just created and is live.
        unsafe { crate::reply_channel::hew_reply_channel_retain(driver_ch) };
        CATCH_UNWIND_SWAP_DRIVER.with(|c| c.set(driver_ch.cast()));

        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline + 2,
            "outer + driver channels are live before dispatch"
        );

        // One ask message carrying the OUTER reply channel.
        // SAFETY: fresh mailbox with a single ask message to drive one dispatch.
        let mailbox = unsafe { mailbox::hew_mailbox_new() };
        assert!(!mailbox.is_null());
        assert_eq!(
            // SAFETY: mailbox is live; null payload of size 0 with a valid
            // outer reply channel is a well-formed ask send.
            unsafe {
                mailbox::hew_mailbox_send_with_reply(
                    mailbox.cast(),
                    1,
                    ptr::null_mut(),
                    0,
                    outer_ch.cast(),
                )
            },
            0
        );

        let mut actor = stub_actor();
        actor.dispatch = Some(swap_open_then_panic_dispatch);
        actor.mailbox = mailbox.cast();
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // Drive the dispatch: the handler opens the swap then Rust-unwinds; the
        // dispatcher's `catch_unwind` catches it (`Err`) and the `Err` edge
        // unwinds the still-open swap and tears the driver channel down BEFORE
        // the normal reply teardown reads/clears the reply channel.
        activate_actor(actor_ptr);

        assert_eq!(
            crate::execution_context::reply_channel_swap_stack_depth(),
            0,
            "the catch_unwind Err edge drained every open swap (no leftover frame)"
        );
        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline + 1,
            "the driver channel was torn down exactly once on the catch_unwind Err \
             edge — and ONLY the driver: the outer ask channel survives, so the \
             outer ask still routes to its own channel; no leak, no wrong-channel free"
        );

        // The OUTER channel is still a live, free-once channel: releasing its
        // sole ref reclaims it exactly once (no double-free / corruption from
        // the swap bracket crossing the unwind).
        // SAFETY: outer_ch is live and holds exactly one ref.
        unsafe { crate::reply_channel::hew_reply_channel_free(outer_ch) };
        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline,
            "the outer channel was intact and freed exactly once"
        );

        CATCH_UNWIND_SWAP_DRIVER.with(|c| c.set(std::ptr::null_mut()));
        // SAFETY: single-threaded test; mailbox unused afterwards.
        unsafe { mailbox::hew_mailbox_free(mailbox) };
    }
}
