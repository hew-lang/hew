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
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
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

/// Global scheduler pointer. Initialized once by `hew_sched_init()`,
/// freed by `hew_runtime_cleanup()`. Using `AtomicPtr` instead of
/// `OnceLock` allows the scheduler to be dropped on shutdown, freeing
/// the crossbeam deques, parkers, and stealer handles.
static SCHEDULER: AtomicPtr<Scheduler> = AtomicPtr::new(std::ptr::null_mut());

/// Get a reference to the global scheduler, if initialized.
///
/// # Safety
///
/// The returned reference is valid as long as `hew_runtime_cleanup()`
/// has not been called. Since cleanup only runs after all worker
/// threads have been joined, this is safe for all normal use.
fn get_scheduler() -> Option<&'static Scheduler> {
    let ptr = SCHEDULER.load(Ordering::Acquire);
    if ptr.is_null() {
        None
    } else {
        // SAFETY: Non-null means hew_sched_init set it, and the
        // scheduler remains valid until hew_runtime_cleanup frees it
        // (which only happens after all workers are joined).
        Some(unsafe { &*ptr })
    }
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
struct Scheduler {
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

    // Store via CAS; second calls are harmless no-ops.
    // NOTE: The scheduler is visible to concurrent callers from this point,
    // but workers are not yet spawned. A concurrent `hew_sched_init` will
    // return 0 (no-op) even if this thread later fails to spawn workers and
    // tears down. This is acceptable because init is expected to run once
    // from the main thread before any actor work begins.
    let ptr = Box::into_raw(scheduler);
    if SCHEDULER
        .compare_exchange(
            std::ptr::null_mut(),
            ptr,
            Ordering::AcqRel,
            Ordering::Relaxed,
        )
        .is_err()
    {
        // Another thread beat us — drop ours.
        // SAFETY: We just allocated this Box.
        drop(unsafe { Box::from_raw(ptr) });
        return 0;
    }

    // Install crash signal handlers for the entire process.
    crate::signal::init_crash_handling();

    // Install SIGTERM/SIGINT handlers for graceful shutdown.
    // SAFETY: Called from main thread during initialization.
    unsafe { crate::shutdown::install_shutdown_signal_handlers() };

    // Phase 2: Spawn worker threads.
    // If ANY worker fails to spawn, treat it as an initialisation failure:
    // clean up already-spawned workers and tear down the scheduler.
    let mut handles: Vec<Option<JoinHandle<()>>> = Vec::with_capacity(worker_count);
    let mut spawn_err: Option<std::io::Error> = None;

    for (id, deque) in deques.into_iter().enumerate() {
        match thread::Builder::new()
            .name(format!("hew-worker-{id}"))
            .spawn(move || worker_loop(id, &deque))
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

/// Signal, wake, and join scheduler workers, optionally detaching the
/// scheduler from the global pointer for caller-controlled final drop.
///
/// When `handles` is `None`, the worker-handle list is swapped out of the
/// scheduler mutex before joining so no lock is held across the
/// potentially-unbounded `join()` calls.
fn teardown_workers(
    scheduler: Option<&'static Scheduler>,
    handles: Option<Vec<Option<JoinHandle<()>>>>,
    take_scheduler: bool,
) -> Option<Box<Scheduler>> {
    if let Some(sched) = scheduler {
        sched.shutdown.store(true, Ordering::Release);
        for parker in &sched.parkers {
            parker.cond.notify_one();
        }
    }

    let mut handles = handles.unwrap_or_else(|| {
        scheduler.map_or_else(Vec::new, |sched| {
            sched.worker_handles.access(std::mem::take)
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

    let ptr = SCHEDULER.swap(std::ptr::null_mut(), Ordering::AcqRel);
    if ptr.is_null() {
        None
    } else {
        // SAFETY: The pointer was installed by `hew_sched_init`, and worker
        // teardown above ensures no thread can still access it.
        Some(unsafe { Box::from_raw(ptr) })
    }
}

/// Clean up after a worker spawn failure during initialisation.
///
/// Signals shutdown, joins all successfully-spawned workers, then removes
/// and drops the scheduler from the global pointer.
fn teardown_after_spawn_failure(handles: Vec<Option<JoinHandle<()>>>) {
    drop(teardown_workers(get_scheduler(), Some(handles), true));
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

    teardown_workers(Some(sched), None, false);

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

    // Detach the scheduler from the global pointer and join any lingering
    // workers before freeing actor/timer state they might still reference.
    let scheduler = teardown_workers(get_scheduler(), None, true);

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

    // Free the scheduler itself (deques, parkers, stealers, global queue).
    drop(scheduler);
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
/// `actor`, if non-null, must reference a live `HewActor`. `cont`, if non-null,
/// must be the continuation parked on `actor` (a `coro.begin` frame). The
/// caller (a readiness source) owns the wake edge; the executor owns teardown.
pub unsafe fn enqueue_resume(actor: *mut HewActor, cont: *mut c_void) {
    if actor.is_null() {
        return;
    }
    // SAFETY: caller guarantees `actor` references a live HewActor.
    let a = unsafe { &*actor };

    // If the park has not yet stored a handle, the suspend edge is mid-park
    // (the FG3 window). Record the wake so the suspend edge re-enqueues; do NOT
    // store the handle ourselves (the suspend edge owns the slot write).
    let parked = a.suspended_cont.load(Ordering::Acquire);
    if parked.is_null() {
        crate::coro_exec::mark_pending_wake(a);
        // The actor is not yet `Suspended`; the CAS below would fail anyway.
        // Re-check after marking: if the park JUST finished publishing
        // `Suspended` between our load and the mark, fall through to the CAS so
        // the wake is delivered now rather than waiting on the suspend edge's
        // pending-wake drain. (Two-phase park, both directions covered.)
        if a.actor_state.load(Ordering::Acquire) != HewActorState::Suspended as i32 {
            let _ = cont; // handle is owned by the suspend edge; nothing to store.
            return;
        }
    }

    // CAS Suspended → Runnable; only enqueue on success (fail-closed against a
    // terminal or not-yet-parked actor).
    if a.actor_state
        .compare_exchange(
            HewActorState::Suspended as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        sched_enqueue(actor);
    } else {
        // The actor was not `Suspended` yet (park still completing) — record
        // the wake so the suspend edge observes it. Terminal actors also land
        // here; marking is harmless (the actor will never park again).
        crate::coro_exec::mark_pending_wake(a);
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
        sched.parkers[idx].cond.notify_one();
    }
}

// ── Worker loop ─────────────────────────────────────────────────────────

/// Main loop executed by each worker thread.
fn worker_loop(id: usize, local: &WorkDeque) {
    let sched = get_scheduler().expect("scheduler not initialized");
    let mut rng = Xorshift64::new(crate::deterministic::effective_worker_seed(id as u64));

    // Install per-worker signal stack, recovery context, and block async signals.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "worker count is bounded by HEW_MAX_WORKERS (256), well within u32 range"
    )]
    crate::signal::init_worker_recovery(id as u32);

    while !sched.shutdown.load(Ordering::Acquire) {
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
/// 2. `begin_park` publishes the `Parked` tag and clears any stale wake (FG3
///    phase 1), BEFORE the handle is stored.
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

    // SAFETY: the parked handle is the executor-owned frame; `resume_park`
    // enforces FG2/FG4 internally (refuses a null slot or non-Parked tag).
    let poll = unsafe { crate::coro_exec::resume_park(a) };

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
        return;
    }

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
                    let installed_prev =
                        crate::execution_context::set_current_context(&raw mut execution_context);
                    debug_assert_eq!(installed_prev, prev_context);
                    crate::tracing::hew_trace_begin(a.id, msg_ref.msg_type);

                    // SAFETY: `execution_context` is the scheduler-owned stack
                    // context for this dispatch and its lock seat came from the
                    // actor's registered sidecar. The helper fails closed when
                    // the seat is absent or poisoned.
                    let lock_acquired = unsafe {
                        crate::actor::hew_actor_state_lock_acquire_for_context(
                            &raw mut execution_context,
                        )
                    } == crate::actor::HEW_ACTOR_STATE_LOCK_OK;
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
                        let reply_consumed =
                            current_reply_channel_consumed_on(&raw mut execution_context);
                        let crash_reply = clear_reply_channel_on(&raw mut execution_context);
                        let restored_context =
                            crate::execution_context::set_current_context(prev_context);
                        debug_assert_eq!(restored_context, &raw mut execution_context);

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
                            &raw mut execution_context,
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
                    let release_result = unsafe {
                        crate::actor::hew_actor_state_lock_release_for_context(
                            &raw mut execution_context,
                        )
                    };
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
                        let _ = clear_reply_channel_on(&raw mut execution_context);
                        let restored_context =
                            crate::execution_context::set_current_context(prev_context);
                        debug_assert_eq!(restored_context, &raw mut execution_context);
                        // SAFETY: msg is exclusively owned by this worker.
                        unsafe {
                            (*msg).reply_channel = std::ptr::null_mut();
                            hew_msg_node_free(msg);
                        }
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

                    let reply_consumed =
                        current_reply_channel_consumed_on(&raw mut execution_context);
                    let _ = clear_reply_channel_on(&raw mut execution_context);

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
                    debug_assert_eq!(restored_context, &raw mut execution_context);

                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "single message dispatch will never exceed u64::MAX nanoseconds"
                    )]
                    let elapsed_ns = t0.elapsed().as_nanos() as u64;
                    msgs_processed += 1;
                    a.prof_messages_processed.fetch_add(1, Ordering::Relaxed);
                    a.prof_processing_time_ns
                        .fetch_add(elapsed_ns, Ordering::Relaxed);

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

/// Guard that installs a worker-less scheduler for the duration of a test and
/// removes + frees it on drop.
///
/// A worker-less scheduler lets a test call [`sched_enqueue`] (which requires an
/// initialized scheduler) while guaranteeing that **nothing drains the queue** —
/// so an enqueued actor pointer stays observable for assertions. Used by the
/// free-path UAF tests (both the reactor-detach and non-reactor link/monitor
/// wake windows) to prove a freed actor is never left queued.
#[cfg(test)]
pub(crate) struct NoWorkerSchedulerForTest {
    previous: *mut Scheduler,
    // Held for the guard's lifetime so the installed scheduler cannot race the
    // module-level scheduler tests (or another worker-less install) on the
    // global `SCHEDULER` pointer. Dropped after the scheduler is restored.
    _sched_lock: std::sync::MutexGuard<'static, ()>,
}

#[cfg(test)]
impl NoWorkerSchedulerForTest {
    /// Install a scheduler with a single global queue and no worker threads.
    pub(crate) fn install() -> Self {
        let sched_lock = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // SAFETY: single-threaded test setup; the scheduler owns its queue state
        // and lives until this guard is dropped.
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
        let sched_ptr = Box::into_raw(Box::new(sched));
        let previous = SCHEDULER.swap(sched_ptr, Ordering::AcqRel);
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
        let ptr = SCHEDULER.swap(self.previous, Ordering::AcqRel);
        if !ptr.is_null() {
            // SAFETY: `ptr` was allocated by `install` via Box::into_raw and no
            // worker threads ever referenced it (worker-less scheduler).
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
        let actor = stub_actor();
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
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is live for this scope; sentinel handle is never resumed.
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

    /// `enqueue_resume` is fail-closed: a terminal (`Stopped`) actor is never
    /// enqueued — the CAS fails and the actor stays terminal, mirroring the
    /// `Idle → Runnable` waker discipline that closes the use-after-free window.
    #[test]
    fn enqueue_resume_fail_closed_on_terminal_actor() {
        let sched = NoWorkerSchedulerForTest::install();
        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is live; terminal so it is never resumed/enqueued.
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

    /// FG3: a wake that fires before the park publishes a handle (the
    /// `suspended_cont` slot is still null) is RECORDED via `pending_wake`
    /// rather than lost, so the suspend edge can re-enqueue it.
    #[test]
    fn enqueue_resume_mid_park_records_pending_wake() {
        let _sched = NoWorkerSchedulerForTest::install();
        let actor = stub_actor();
        // Park not yet published: state not Suspended, slot null.
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is live; null slot means no handle is ever resumed.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };

        assert!(
            crate::coro_exec::take_pending_wake(&actor),
            "a wake in the park window must be recorded, not lost (FG3)"
        );
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
        assert_eq!(
            actor.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Destroyed as i32
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
    /// The per-message lock RELEASE on the production suspend edge (between
    /// `hew_actor_state_lock_acquire_for_context` and the matching release)
    /// lands with the Slice-5 trampoline rewrite that routes real `await`
    /// through this seam; the seed driver here holds no lock, and the invariant
    /// this slice enforces is that the executor edges themselves are lock-free.
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

        // Acquire SCHED_TEST_MUTEX first (consistent lock order: sched → ticker).
        // hew_runtime_cleanup clears the global SCHEDULER pointer, so this test
        // must be serialised relative to other scheduler tests.
        let _sched_guard = SCHED_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Hold the shared ticker mutex for the duration of this test so it
        // cannot race with timer_periodic tests that poll the same globals.
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

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
        let sched_ptr = Box::into_raw(Box::new(sched));
        SCHEDULER.store(sched_ptr, Ordering::Release);

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
            SCHEDULER.load(Ordering::Acquire).is_null(),
            "spawn-failure teardown must clear the global scheduler pointer"
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
        let sched_ptr = Box::into_raw(Box::new(sched));
        SCHEDULER.store(sched_ptr, Ordering::Release);

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
            // SAFETY: sched_ptr was just allocated above and is valid.
            let sched = unsafe { &*SCHEDULER.load(Ordering::Acquire) };
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

        // Clean up the scheduler pointer.
        let ptr = SCHEDULER.swap(ptr::null_mut(), Ordering::AcqRel);
        if !ptr.is_null() {
            // SAFETY: ptr was allocated with Box::into_raw above and no
            // other thread references it after the swap.
            drop(unsafe { Box::from_raw(ptr) });
        }
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
        let sched_ptr = Box::into_raw(Box::new(sched));
        SCHEDULER.store(sched_ptr, Ordering::Release);

        let release_worker = Arc::new(AtomicBool::new(false));
        let release_worker2 = Arc::clone(&release_worker);
        let handle = thread::spawn(move || {
            while !release_worker2.load(Ordering::Acquire) {
                thread::sleep(Duration::from_millis(10));
            }
        });
        // SAFETY: sched_ptr was just allocated above and remains valid until cleanup below.
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

        let ptr = SCHEDULER.swap(ptr::null_mut(), Ordering::AcqRel);
        if !ptr.is_null() {
            // SAFETY: ptr was allocated with Box::into_raw above and no references remain.
            drop(unsafe { Box::from_raw(ptr) });
        }
    }

    #[test]
    fn runtime_cleanup_joins_workers_and_drops_scheduler() {
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
        let sched_ptr = Box::into_raw(Box::new(sched));
        SCHEDULER.store(sched_ptr, Ordering::Release);

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
        // SAFETY: sched_ptr was just allocated above and remains valid until runtime cleanup.
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
            SCHEDULER.load(Ordering::Acquire).is_null(),
            "runtime cleanup must clear the global scheduler pointer"
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
        let sched_ptr = Box::into_raw(Box::new(sched));
        SCHEDULER.store(sched_ptr, Ordering::Release);

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

        let ptr = SCHEDULER.swap(ptr::null_mut(), Ordering::AcqRel);
        if !ptr.is_null() {
            // SAFETY: ptr was allocated with Box::into_raw above and no references remain.
            drop(unsafe { Box::from_raw(ptr) });
        }
        ACTIVE_WORKERS.store(0, Ordering::Release);
    }

    #[test]
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
        let sched_ptr = Box::into_raw(Box::new(sched));
        SCHEDULER.store(sched_ptr, Ordering::Release);

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

        let ptr = SCHEDULER.swap(ptr::null_mut(), Ordering::AcqRel);
        if !ptr.is_null() {
            // SAFETY: ptr was allocated with Box::into_raw above and no references remain.
            drop(unsafe { Box::from_raw(ptr) });
        }
        ACTIVE_WORKERS.store(0, Ordering::Release);
    }
}
