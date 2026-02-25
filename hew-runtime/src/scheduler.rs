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

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Condvar, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use crate::actor::{self, HewActor, HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET};
use crate::deque::{GlobalQueue, WorkDeque, WorkStealer};
use crate::internal::types::HewActorState;
use crate::mailbox::{
    self, hew_mailbox_has_messages, hew_mailbox_try_recv, hew_msg_node_free, HewMailbox,
};

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

/// The scheduler owns the shared global queue, per-worker stealers,
/// shutdown flag, and condvar for worker parking.
///
/// Worker thread handles are stored behind a `Mutex` so they can be
/// `take`-n during shutdown (`JoinHandle` is `Send` but not `Sync`).
struct Scheduler {
    worker_handles: Mutex<Vec<Option<JoinHandle<()>>>>,
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
// `Mutex` (`JoinHandle`).
unsafe impl Sync for Scheduler {}

// ── Xorshift64 PRNG for victim selection ────────────────────────────────

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
/// Calling this more than once is a no-op.
#[no_mangle]
pub extern "C" fn hew_sched_init() {
    let default_count = thread::available_parallelism()
        .map(std::num::NonZeroUsize::get)
        .unwrap_or(4);

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
        worker_handles: Mutex::new(Vec::new()),
        global_queue,
        stealers,
        shutdown: AtomicBool::new(false),
        parkers,
        worker_count,
    });

    // Store via CAS; second calls are harmless no-ops.
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
        return;
    }

    // Install crash signal handlers for the entire process.
    crate::signal::init_crash_handling();

    // Install SIGTERM/SIGINT handlers for graceful shutdown.
    // SAFETY: Called from main thread during initialization.
    unsafe { crate::shutdown::install_shutdown_signal_handlers() };

    // Phase 2: Spawn worker threads.
    let mut handles = Vec::with_capacity(worker_count);
    for (id, deque) in deques.into_iter().enumerate() {
        let Ok(handle) = thread::Builder::new()
            .name(format!("hew-worker-{id}"))
            .spawn(move || worker_loop(id, &deque))
        else {
            continue;
        };
        handles.push(Some(handle));
    }

    // We know `SCHEDULER` was just set by us.
    let Some(sched) = get_scheduler() else {
        return;
    };
    let mut lock = match sched.worker_handles.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    *lock = handles;

    // Start the profiler if HEW_PPROF is set.
    crate::profiler::maybe_start();
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

    // Signal shutdown.
    sched.shutdown.store(true, Ordering::Release);

    // Wake all parked workers so they observe the flag.
    for parker in &sched.parkers {
        parker.cond.notify_one();
    }

    // Join worker threads.
    let mut handles = match sched.worker_handles.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    for handle in &mut *handles {
        if let Some(h) = handle.take() {
            let _ = h.join();
        }
    }

    // Write profile files on exit if HEW_PROF_OUTPUT is set.
    crate::profiler::maybe_write_on_exit();
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

    // Free the scheduler itself (deques, parkers, stealers, global queue).
    let ptr = SCHEDULER.swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !ptr.is_null() {
        // SAFETY: The pointer was allocated with Box::into_raw in
        // hew_sched_init. All worker threads have been joined, so no
        // concurrent access is possible.
        drop(unsafe { Box::from_raw(ptr) });
    }
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
        let guard = parker.mutex.lock().expect("lock poisoned");
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

// ── Actor activation ────────────────────────────────────────────────────

/// Activate an actor: CAS state to `Running`, drain messages up to budget,
/// then transition back to `Idle` or re-enqueue as `Runnable`.
#[expect(
    clippy::too_many_lines,
    reason = "actor activation state machine with multiple CAS transitions"
)]
fn activate_actor(actor: *mut HewActor) {
    // SAFETY: Only valid actor pointers are ever enqueued by the runtime.
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

    ACTIVE_WORKERS.fetch_add(1, Ordering::Relaxed);

    // Set thread-local CURRENT_ACTOR so hew_actor_self() works during dispatch.
    let prev_actor = actor::set_current_actor(actor);

    // Set per-actor arena as thread-local so hew_arena_malloc routes through it.
    let prev_arena = crate::arena::set_current_arena(a.arena);

    let mut msgs_processed: u32 = 0;

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
                        break;
                    }

                    // Check for injected delay fault (testing only).
                    let delay_ms = crate::deterministic::check_delay_fault(a.id);

                    // Reset reduction counter for this dispatch.
                    a.reductions
                        .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

                    // SAFETY: `dispatch` and `a.state` are valid; message fields
                    // come from a well-formed `HewMsgNode`.
                    unsafe {
                        let msg_ref = &*msg;
                        dispatch(a.state, msg_ref.msg_type, msg_ref.data, msg_ref.data_size);
                    }

                    // Dispatch completed successfully — clear recovery point.
                    crate::signal::clear_dispatch_recovery();

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

                    // Apply injected delay after dispatch (testing only).
                    if delay_ms > 0 {
                        std::thread::sleep(Duration::from_millis(u64::from(delay_ms)));
                    }
                } else {
                    // Recovered from a crash signal (SEGV/BUS/FPE/ILL).
                    // handle_crash_recovery marks the actor as Crashed and
                    // logs the crash to stderr.
                    //
                    // SAFETY: called immediately after sigsetjmp returned
                    // non-zero, on the same worker thread.
                    unsafe { crate::signal::handle_crash_recovery() };

                    // Restore arena and reset — crash discards all in-flight data.
                    crate::arena::set_current_arena(prev_arena);
                    if !a.arena.is_null() {
                        // SAFETY: Arena was created during spawn; crash discards
                        // all in-flight data.
                        unsafe { crate::arena::hew_arena_reset(a.arena) };
                    }

                    // Free the message node. The dispatch didn't complete,
                    // but the node itself (allocated by mailbox_send) is
                    // still valid — siglongjmp only unwound the dispatch
                    // stack frames, not the scheduler frame.
                    //
                    // SAFETY: `msg` is exclusively owned by this worker.
                    unsafe { hew_msg_node_free(msg) };

                    // Stop processing further messages for this actor.
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

    // Restore previous CURRENT_ACTOR and arena.
    actor::set_current_actor(prev_actor);
    crate::arena::set_current_arena(prev_arena);
    if !a.arena.is_null() {
        // SAFETY: Arena was created during spawn; no references survive past activation.
        unsafe { crate::arena::hew_arena_reset(a.arena) };
    }

    ACTIVE_WORKERS.fetch_sub(1, Ordering::Relaxed);
    TASKS_COMPLETED.fetch_add(1, Ordering::Relaxed);

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
            crate::actor_group::notify_actor_death(a.id);
        }
        return;
    }

    // Check if actor was stopped or crashed during dispatch.
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    // Hibernation: track idle activations.
    let hib_threshold = a.hibernation_threshold.load(Ordering::Relaxed);
    if msgs_processed == 0 && hib_threshold > 0 {
        let prev_idle = a.idle_count.fetch_add(1, Ordering::Relaxed);
        if prev_idle + 1 >= hib_threshold {
            a.hibernating.store(1, Ordering::Relaxed);
        }
    } else if msgs_processed > 0 {
        // Reset idle counter on any message processing.
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
    }

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
                    crate::actor_group::notify_actor_death(a.id);
                }
            }
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
/// Returns 0 if the actor should continue, 1 if it yielded.
///
/// # Safety
///
/// No preconditions — may be called from any context. When called
/// outside an actor dispatch (i.e. `CURRENT_ACTOR` is null), this is
/// a no-op.
#[no_mangle]
pub extern "C" fn hew_actor_cooperate() -> c_int {
    let actor = actor::hew_actor_self();
    if actor.is_null() {
        return 0;
    }

    // SAFETY: hew_actor_self returned a valid, non-null actor pointer.
    let a = unsafe { &*actor };

    // Decrement reduction counter. If still positive, continue.
    let prev = a.reductions.fetch_sub(1, Ordering::Relaxed);
    if prev > 1 {
        return 0;
    }

    // Budget exhausted — reset counter and yield to OS scheduler.
    a.reductions
        .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

    // Check if the current task scope is cancelled. This is a read-only
    // observation; the actor/task will handle cancellation on its next
    // explicit check.  Foundation for future auto-cancellation.
    let scope = crate::task_scope::current_task_scope();
    if !scope.is_null() {
        // SAFETY: scope is valid per hew_task_scope_set_current contract.
        let _ = unsafe { (*scope).cancelled.load(Ordering::Acquire) };
    }

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

    /// Helper: build a minimal `HewActor` with sensible defaults.
    fn stub_actor() -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 1,
            pid: 0,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Runnable as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
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
        }
    }

    #[test]
    fn activate_transitions_runnable_to_idle() {
        let actor = stub_actor();
        let ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

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
        let ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

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
        let ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        activate_actor(ptr);

        // CAS should fail — state stays IDLE.
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Idle as i32
        );
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
}
