//! Hew runtime: actor struct definition and state constants.
//!
//! Defines the [`HewActor`] struct layout for C ABI compatibility and the
//! actor state machine constants. The full actor API (spawn, send, activate)
//! will be implemented in a future iteration.

use crate::util::MutexExt;
#[cfg(not(target_arch = "wasm32"))]
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, Ordering};
use std::sync::Mutex;

use crate::internal::types::{HewActorState, HewError, HewOverflowPolicy};
#[cfg(not(target_arch = "wasm32"))]
use crate::mailbox::{self, HewMailbox};
#[cfg(not(target_arch = "wasm32"))]
use crate::reply_channel::{self, HewReplyChannel};
#[cfg(not(target_arch = "wasm32"))]
use crate::scheduler;

// ── Thread-local current actor ──────────────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
thread_local! {
    /// The actor currently being dispatched on this worker thread.
    static CURRENT_ACTOR: Cell<*mut HewActor> = const { Cell::new(ptr::null_mut()) };
}

#[cfg(target_arch = "wasm32")]
static mut CURRENT_ACTOR_WASM: *mut HewActor = ptr::null_mut();

/// Set the current actor for this worker thread, returning the previous value.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn set_current_actor(actor: *mut HewActor) -> *mut HewActor {
    CURRENT_ACTOR.with(|c| c.replace(actor))
}

/// Set the current actor, returning the previous value.
#[cfg(target_arch = "wasm32")]
#[allow(dead_code)]
pub(crate) fn set_current_actor(actor: *mut HewActor) -> *mut HewActor {
    // SAFETY: WASM is single-threaded, no data races possible.
    unsafe {
        let prev = CURRENT_ACTOR_WASM;
        CURRENT_ACTOR_WASM = actor;
        prev
    }
}

/// Get the ID of the actor currently being dispatched on this thread.
///
/// Returns -1 if no actor is active (called from main or non-actor context).
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_actor_current_id() -> i64 {
    CURRENT_ACTOR.with(|c| {
        let ptr = c.get();
        if ptr.is_null() {
            -1
        } else {
            // SAFETY: ptr is non-null and points to a valid HewActor set by the scheduler.
            #[expect(clippy::cast_possible_wrap, reason = "actor IDs fit in i64")]
            {
                // SAFETY: ptr is non-null and valid (checked above, set by scheduler).
                unsafe { &*ptr }.id as i64
            }
        }
    })
}

/// Get the ID of the actor currently being dispatched.
///
/// Returns -1 if no actor is active (called from main or non-actor context).
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_actor_current_id() -> i64 {
    // SAFETY: WASM is single-threaded.
    unsafe {
        if CURRENT_ACTOR_WASM.is_null() {
            -1
        } else {
            #[expect(clippy::cast_possible_wrap, reason = "actor IDs fit in i64")]
            {
                (&*CURRENT_ACTOR_WASM).id as i64
            }
        }
    }
}

/// Default message processing budget per activation.
pub const HEW_MSG_BUDGET: i32 = 256;

/// Default reduction budget per dispatch call.
///
/// This is the number of "reduction points" (loop iterations, function
/// calls) an actor can execute within a single message dispatch before
/// it yields. 4000 is roughly similar to Erlang's default of 4000
/// reductions.
pub const HEW_DEFAULT_REDUCTIONS: i32 = 4000;

/// Maximum number of workers the scheduler supports.
pub const HEW_MAX_WORKERS: usize = 256;

/// Priority levels for actor scheduling.
pub const HEW_PRIORITY_HIGH: i32 = 0;
/// Normal priority (default).
pub const HEW_PRIORITY_NORMAL: i32 = 1;
/// Low priority.
pub const HEW_PRIORITY_LOW: i32 = 2;

// ── Actor struct ────────────────────────────────────────────────────────

/// Actor struct layout. MUST match the C definition exactly.
///
/// The `sched_link_next` field (intrusive MPSC next pointer) MUST be the
/// first field so that `*mut HewActor` can be cast to/from `*mut MpscNode`.
#[repr(C)]
pub struct HewActor {
    /// Intrusive MPSC node for the global scheduler queue.
    pub sched_link_next: AtomicPtr<HewActor>,

    /// Unique, monotonically increasing actor ID.
    pub id: u64,

    /// Unique process identifier (PID) for this actor.
    pub pid: u64,

    /// Actor-owned mutable state.
    pub state: *mut c_void,

    /// Size of the state allocation.
    pub state_size: usize,

    /// Dispatch function (4-param canonical signature).
    pub dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,

    /// Pointer to the actor's mailbox.
    ///
    /// Typed as `*mut c_void` to avoid circular module dependencies;
    /// the scheduler casts to `*mut HewMailbox` when processing messages.
    pub mailbox: *mut c_void,

    /// Current lifecycle state (CAS transitions).
    pub actor_state: AtomicI32,

    /// Messages to process per activation.
    pub budget: AtomicI32,

    /// Saved initial state for supervisor restart (deep copy).
    pub init_state: *mut c_void,

    /// Size of the initial state.
    pub init_state_size: usize,

    /// Optional coalesce key function for message coalescing.
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,

    /// Optional cleanup function called when the actor is freed.
    /// Generated from the `terminate { ... }` block in the actor declaration.
    pub terminate_fn: Option<unsafe extern "C" fn(*mut c_void)>,

    /// Guard flag ensuring the terminate callback runs exactly once.
    pub terminate_called: AtomicBool,

    /// Set to `true` after the terminate callback returns (or was skipped).
    /// Free paths spin-wait on this to avoid freeing state while terminate
    /// is still running on another thread.
    pub terminate_finished: AtomicBool,

    /// Error code set by `hew_actor_trap` (0 = no error).
    pub error_code: AtomicI32,

    /// Back-pointer to the supervising [`HewSupervisor`] (null if unsupervised).
    pub supervisor: *mut c_void,

    /// Index of this actor within its supervisor's child array.
    pub supervisor_child_index: i32,

    // ── Priority scheduling ─────────────────────────────────────────────
    /// Scheduling priority: 0 = high, 1 = normal (default), 2 = low.
    ///
    /// Higher-priority actors get their message budget multiplied,
    /// allowing them to process more messages per activation.
    pub priority: AtomicI32,

    // ── Reduction-based preemption ────────────────────────────────────
    /// Remaining reduction budget for the current dispatch. Decremented
    /// at compiler-inserted yield points. When it reaches 0 the actor
    /// yields control back to the scheduler.
    pub reductions: AtomicI32,

    // ── Hibernation ─────────────────────────────────────────────────────
    /// Number of consecutive activations with zero messages.
    /// When this reaches `hibernation_threshold`, the actor is
    /// considered hibernating and its arena may be freed.
    pub idle_count: AtomicI32,

    /// Number of consecutive idle activations before hibernation.
    /// 0 disables hibernation (default).
    pub hibernation_threshold: AtomicI32,

    /// Whether the actor is currently hibernating.
    /// Set to 1 when `idle_count` >= `hibernation_threshold`.
    pub hibernating: AtomicI32,

    // ── Profiler stats (appended at end to preserve C ABI layout) ────
    /// Total messages dispatched to this actor.
    pub prof_messages_processed: AtomicU64,

    /// Cumulative nanoseconds spent in dispatch for this actor.
    pub prof_processing_time_ns: AtomicU64,

    /// Per-actor arena bump allocator. Set as thread-local during dispatch
    /// so `hew_arena_malloc` routes through it. Reset after each activation.
    #[cfg(not(target_arch = "wasm32"))]
    pub arena: *mut crate::arena::ActorArena,
    /// WASM stub: arena is not used on WASM (allocations go through libc directly).
    #[cfg(target_arch = "wasm32")]
    pub arena: *mut c_void,
}

// SAFETY: `HewActor` is designed for concurrent access across worker threads.
// All mutable shared fields use atomic types. Raw pointers are managed by
// the scheduler/actor lifecycle which ensures exclusive access during
// activation (CAS `RUNNABLE` → `RUNNING`).
unsafe impl Send for HewActor {}
// SAFETY: Concurrent reads/writes of shared mutable fields use atomics.
// Raw-pointer fields are lifecycle-managed by scheduler CAS transitions.
unsafe impl Sync for HewActor {}

impl std::fmt::Debug for HewActor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewActor")
            .field("id", &self.id)
            .field("pid", &self.pid)
            .field("actor_state", &self.actor_state)
            .field("budget", &self.budget.load(Ordering::Relaxed))
            .field("arena", &self.arena)
            .finish_non_exhaustive()
    }
}

// ── Spawn options ───────────────────────────────────────────────────────

/// Monotonically increasing actor serial counter.
static NEXT_ACTOR_SERIAL: AtomicU64 = AtomicU64::new(1);

// PID is now unified with id — actors use location-transparent IDs everywhere.

// ── Live actor tracking ────────────────────────────────────────────────

/// Wrapper so `*mut HewActor` can be stored in a collection.
#[derive(Debug)]
struct ActorPtr(*mut HewActor);

// SAFETY: Actor pointers are managed by the runtime and only freed
// under controlled conditions (shutdown or explicit free).
unsafe impl Send for ActorPtr {}

/// Map from actor ID → pointer for O(1) lookups by ID.
static LIVE_ACTORS: Mutex<Option<HashMap<u64, ActorPtr>>> = Mutex::new(None);

/// Register an actor in the live tracking map.
///
/// # Safety
///
/// `actor` must be a valid, fully initialised `HewActor` pointer whose
/// `id` field is already set.
fn track_actor(actor: *mut HewActor) {
    // SAFETY: caller guarantees `actor` is valid and initialised.
    let id = unsafe { (*actor).id };
    let mut guard = LIVE_ACTORS.lock_or_recover();
    guard
        .get_or_insert_with(HashMap::new)
        .insert(id, ActorPtr(actor));
}

/// Remove an actor from the live tracking map.
///
/// Returns `true` if the actor was present and removed, `false` if it
/// was not found (e.g. already consumed by [`cleanup_all_actors`]).
/// Only removes the entry if the stored pointer matches `actor`, guarding
/// against the (unlikely) case of an ID collision after serial overflow.
fn untrack_actor(actor: *mut HewActor) -> bool {
    // SAFETY: caller guarantees `actor` is valid and not yet freed.
    let id = unsafe { (*actor).id };
    let mut guard = LIVE_ACTORS.lock_or_recover();
    if let Some(map) = guard.as_mut() {
        if let std::collections::hash_map::Entry::Occupied(entry) = map.entry(id) {
            if entry.get().0 == actor {
                entry.remove();
                return true;
            }
        }
    }
    false
}

/// Check whether an actor pointer is still live (tracked and not yet freed).
pub(crate) fn is_actor_live(actor: *mut HewActor) -> bool {
    let guard = LIVE_ACTORS.lock_or_recover();
    if let Some(map) = guard.as_ref() {
        return map.values().any(|ptr| ptr.0 == actor);
    }
    false
}

/// Free all remaining tracked actors. Called during scheduler shutdown
/// after all worker threads have been joined.
///
/// # Safety
///
/// Must only be called after all worker threads have stopped (native)
/// or when no dispatch is in progress (WASM).
pub(crate) unsafe fn cleanup_all_actors() {
    let actors = {
        let mut guard = LIVE_ACTORS.lock_or_recover();
        match guard.as_mut() {
            Some(map) => std::mem::take(map),
            None => HashMap::new(),
        }
    };

    for ActorPtr(actor) in actors.into_values() {
        if actor.is_null() {
            continue;
        }
        // Run terminate for actors that never reached a terminal state
        // (still IDLE at process exit). Skip crashed actors — their state
        // may be corrupted. The terminate_called flag inside
        // call_terminate_fn prevents double-execution for actors that
        // already ran their terminate at a state transition.
        // SAFETY: actor is valid (from LIVE_ACTORS); scheduler is shut down.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        if state != HewActorState::Crashed as i32 {
            // SAFETY: No concurrent dispatch — scheduler is shut down.
            unsafe { call_terminate_fn(actor) };
        }

        // Clean up periodic timers, links, and monitors before freeing.
        crate::timer_periodic::cancel_all_timers_for_actor(actor);
        // SAFETY: actor is valid (from LIVE_ACTORS set, not yet freed).
        let actor_id = unsafe { (*actor).id };
        crate::link::remove_all_links_for_actor(actor_id, actor);
        crate::monitor::remove_all_monitors_for_actor(actor_id, actor);

        // SAFETY: Caller guarantees no concurrent dispatch.
        // SAFETY: The actor was allocated by a spawn function and has not been freed yet.
        unsafe { free_actor_resources(actor) };
    }
}

/// Free an actor's resources without spin-waiting or untracking.
///
/// This is the internal implementation shared by [`hew_actor_free`] and
/// [`cleanup_all_actors`].
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn free_actor_resources(actor: *mut HewActor) {
    #[cfg(feature = "profiler")]
    // SAFETY: `actor` is valid.
    unsafe {
        crate::profiler::actor_registry::unregister(actor);
    };

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Wait for any in-progress terminate callback to complete. This
    // prevents freeing state while another thread is running terminate.
    // Bounded to 5 seconds to avoid hanging forever if terminate blocks.
    let terminate_deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let mut terminate_timed_out = false;
    while a.terminate_called.load(Ordering::Acquire)
        && !a.terminate_finished.load(Ordering::Acquire)
    {
        if std::time::Instant::now() >= terminate_deadline {
            eprintln!(
                "hew: warning: actor {} terminate callback did not finish within 5s, quarantining actor",
                a.id
            );
            terminate_timed_out = true;
            break;
        }
        std::hint::spin_loop();
    }

    // If the terminate callback is still running, the state pointer is in
    // use on another thread. Quarantine the actor (intentional leak) to
    // avoid use-after-free. The memory cost is bounded because this only
    // happens for actors whose terminate hangs.
    if terminate_timed_out {
        return;
    }

    // SAFETY: State was malloc'd by deep_copy_state.
    unsafe {
        libc::free(a.state);
        libc::free(a.init_state);
    }

    if !a.arena.is_null() {
        // SAFETY: Arena was created by hew_arena_new during spawn.
        unsafe { crate::arena::hew_arena_free_all(a.arena) };
    }

    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: Mailbox was allocated by hew_mailbox_new.
        unsafe { mailbox::hew_mailbox_free(mb) };
    }

    // SAFETY: Actor was allocated with Box::new / Box::into_raw.
    drop(unsafe { Box::from_raw(actor) });
}

/// Free an actor's resources (WASM version — no arena cleanup).
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(target_arch = "wasm32")]
unsafe fn free_actor_resources(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // SAFETY: State was malloc'd by deep_copy_state.
    unsafe {
        libc::free(a.state);
        libc::free(a.init_state);
    }

    // Free the mailbox if present.
    let mb = a.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
    if !mb.is_null() {
        // SAFETY: Mailbox was allocated by hew_mailbox_new.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mb) };
    }

    // SAFETY: Actor was allocated with Box::new / Box::into_raw.
    drop(unsafe { Box::from_raw(actor) });
}

// ── Terminate callback invocation ───────────────────────────────────────

/// Run the actor's terminate callback exactly once, with crash recovery.
///
/// Sets up `CURRENT_ACTOR` and (on worker threads) a `sigsetjmp` recovery
/// frame so that Hew panics or signals inside the terminate block are
/// caught instead of aborting the process.
///
/// Called at terminal state transitions (→ Stopped), **not** at free time.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live [`HewActor`] in a terminal
/// state (`Stopped`) that is not currently being dispatched.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn call_terminate_fn(actor: *mut HewActor) {
    // SAFETY: caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Guard: only run once across all terminal-transition paths.
    if a.terminate_called.swap(true, Ordering::AcqRel) {
        return;
    }

    let Some(terminate_fn) = a.terminate_fn else {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    };

    if a.state.is_null() {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    }

    let state = a.state;
    let prev_actor = set_current_actor(actor);

    // Set up crash recovery (returns null on non-worker threads).
    // SAFETY: `actor` is valid and in a terminal state; null msg is fine.
    let jmp_buf_ptr = unsafe { crate::signal::prepare_dispatch_recovery(actor, ptr::null_mut()) };

    let is_normal_path = if jmp_buf_ptr.is_null() {
        // No recovery context (external thread or not initialised).
        // Hew panics (longjmp) from an external thread will still
        // abort the process — that's an acceptable limitation.
        true
    } else {
        // SAFETY: jmp_buf_ptr is valid (from prepare_dispatch_recovery).
        let ret = unsafe { crate::signal::sigsetjmp(jmp_buf_ptr, 1) };
        if ret == 0 {
            crate::signal::mark_recovery_active();
            true
        } else {
            false
        }
    };

    if is_normal_path {
        // catch_unwind guards against Rust panics; the sigsetjmp frame
        // (when present) guards against Hew panics and signals.
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // SAFETY: terminate_fn and state are valid; actor is not
            // being dispatched.
            unsafe { terminate_fn(state) };
        }));
        if !jmp_buf_ptr.is_null() {
            crate::signal::clear_dispatch_recovery();
        }
    } else {
        // Terminate block crashed via signal/longjmp. The actor is
        // already in a terminal state so hew_actor_trap is a no-op for
        // the state transition, but handle_crash_recovery properly
        // clears in_recovery and logs a crash report.
        // SAFETY: called immediately after sigsetjmp returned non-zero.
        unsafe { crate::signal::handle_crash_recovery() };
    }

    a.terminate_finished.store(true, Ordering::Release);
    set_current_actor(prev_actor);
}

/// Run the actor's terminate callback exactly once (WASM version).
///
/// No signal recovery on WASM — `catch_unwind` is the only guard.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live [`HewActor`] in a terminal
/// state (`Stopped`) that is not currently being dispatched.
#[cfg(target_arch = "wasm32")]
pub(crate) unsafe fn call_terminate_fn(actor: *mut HewActor) {
    let a = unsafe { &*actor };

    if a.terminate_called.swap(true, Ordering::AcqRel) {
        return;
    }

    let Some(terminate_fn) = a.terminate_fn else {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    };

    if a.state.is_null() {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    }

    let state = a.state;
    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        unsafe { terminate_fn(state) };
    }));
    a.terminate_finished.store(true, Ordering::Release);
}

/// Actor spawn options for [`hew_actor_spawn_opts`].
#[repr(C)]
#[derive(Debug)]
pub struct HewActorOpts {
    /// Pointer to initial state (deep-copied).
    pub init_state: *mut c_void,
    /// Size of `init_state` in bytes.
    pub state_size: usize,
    /// Dispatch function.
    pub dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
    /// Mailbox capacity (`-1` or `0` = unbounded).
    pub mailbox_capacity: i32,
    /// Overflow policy (see [`HewOverflowPolicy`]).
    pub overflow: i32,
    /// Optional coalesce key function.
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    /// Fallback policy used when coalescing finds no key match.
    pub coalesce_fallback: i32,
    /// Messages per activation (`0` = default).
    pub budget: i32,
}

fn parse_overflow_policy(policy: i32) -> HewOverflowPolicy {
    match policy {
        x if x == HewOverflowPolicy::Block as i32 => HewOverflowPolicy::Block,
        x if x == HewOverflowPolicy::DropOld as i32 => HewOverflowPolicy::DropOld,
        x if x == HewOverflowPolicy::Fail as i32 => HewOverflowPolicy::Fail,
        x if x == HewOverflowPolicy::Coalesce as i32 => HewOverflowPolicy::Coalesce,
        _ => HewOverflowPolicy::DropNew,
    }
}

// ── Spawn ───────────────────────────────────────────────────────────────
// All spawn functions use native mailbox/scheduler and are not available on WASM.
// WASM actors are created through the bridge module instead.

/// Deep-copy `src` into a new malloc'd buffer.
///
/// Returns null if `src` is null or `size` is 0.
///
/// # Safety
///
/// `src` must point to at least `size` readable bytes.
unsafe fn deep_copy_state(src: *mut c_void, size: usize) -> *mut c_void {
    if src.is_null() || size == 0 {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `src` is readable for `size` bytes.
    unsafe {
        let dst = libc::malloc(size);
        assert!(!dst.is_null(), "OOM allocating actor state ({size} bytes)");
        ptr::copy_nonoverlapping(src.cast::<u8>(), dst.cast::<u8>(), size);
        dst
    }
}

/// Configuration for the internal actor spawn helper.
///
/// All three public spawn functions build one of these and delegate to
/// [`spawn_actor_internal`].
struct ActorSpawnConfig {
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
    mailbox: *mut c_void,
    budget: i32,
    coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
}

/// Shared implementation for all native actor spawn functions.
///
/// # Safety
///
/// - `config.state` must be a deep-copied allocation (or null for zero-sized state).
/// - `config.mailbox` must be a valid mailbox pointer (already configured).
#[cfg(not(target_arch = "wasm32"))]
#[expect(
    clippy::needless_pass_by_value,
    reason = "config is a lightweight aggregate of Copy fields; consuming it reads clearly at call sites"
)]
unsafe fn spawn_actor_internal(config: ActorSpawnConfig) -> *mut HewActor {
    // SAFETY: Caller already deep-copied state; make a second copy for restart.
    let init_state = unsafe { deep_copy_state(config.state, config.state_size) };

    let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: actor_id,
        pid: actor_id,
        state: config.state,
        state_size: config.state_size,
        dispatch: config.dispatch,
        mailbox: config.mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(config.budget),
        init_state,
        init_state_size: config.state_size,
        coalesce_key_fn: config.coalesce_key_fn,
        terminate_fn: None,
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
        error_code: AtomicI32::new(0),
        supervisor: ptr::null_mut(),
        supervisor_child_index: -1,
        priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
        reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
        idle_count: AtomicI32::new(0),
        hibernation_threshold: AtomicI32::new(0),
        hibernating: AtomicI32::new(0),
        prof_messages_processed: AtomicU64::new(0),
        prof_processing_time_ns: AtomicU64::new(0),
        arena: crate::arena::hew_arena_new(),
    });

    let raw = Box::into_raw(actor);
    track_actor(raw);
    #[cfg(feature = "profiler")]
    // SAFETY: `raw` was just allocated by `Box::into_raw` and is valid.
    unsafe {
        crate::profiler::actor_registry::register(raw);
    };
    crate::tracing::hew_trace_lifecycle(actor_id, crate::tracing::SPAN_SPAWN);
    raw
}

/// Shared implementation for all WASM actor spawn functions.
///
/// # Safety
///
/// Same requirements as [`spawn_actor_internal`] but for WASM targets.
#[cfg(target_arch = "wasm32")]
#[expect(
    clippy::needless_pass_by_value,
    reason = "config is a lightweight aggregate of Copy fields; consuming it reads clearly at call sites"
)]
unsafe fn spawn_actor_internal(config: ActorSpawnConfig) -> *mut HewActor {
    // SAFETY: Caller already deep-copied state; make a second copy for restart.
    let init_state = unsafe { deep_copy_state(config.state, config.state_size) };

    let serial = NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed);
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: serial,
        pid: serial,
        state: config.state,
        state_size: config.state_size,
        dispatch: config.dispatch,
        mailbox: config.mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(config.budget),
        init_state,
        init_state_size: config.state_size,
        coalesce_key_fn: config.coalesce_key_fn,
        terminate_fn: None,
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
        error_code: AtomicI32::new(0),
        supervisor: ptr::null_mut(),
        supervisor_child_index: -1,
        priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
        reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
        idle_count: AtomicI32::new(0),
        hibernation_threshold: AtomicI32::new(0),
        hibernating: AtomicI32::new(0),
        prof_messages_processed: AtomicU64::new(0),
        prof_processing_time_ns: AtomicU64::new(0),
        arena: ptr::null_mut(),
    });

    let raw = Box::into_raw(actor);
    track_actor(raw);
    raw
}

/// Spawn a new actor with an unbounded mailbox.
///
/// The initial state is deep-copied. The returned pointer must be freed
/// with [`hew_actor_free`].
///
/// # Safety
///
/// - `state` must point to at least `state_size` readable bytes, or be
///   null when `state_size` is 0.
/// - `dispatch` will be called from worker threads with the actor's
///   state pointer.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };

    // SAFETY: hew_mailbox_new returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new() };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
        })
    }
}

/// Spawn a new actor from a [`HewActorOpts`] struct.
///
/// Uses a bounded mailbox if `opts.mailbox_capacity > 0`.
///
/// # Safety
///
/// - `opts` must be a valid pointer to a [`HewActorOpts`].
/// - Same state/dispatch requirements as [`hew_actor_spawn`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts(opts: *const HewActorOpts) -> *mut HewActor {
    if opts.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` is valid.
    let opts = unsafe { &*opts };

    // SAFETY: Caller guarantees state validity.
    let actor_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, opts.coalesce_key_fn, coalesce_fallback);
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            mailbox: mailbox.cast(),
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
        })
    }
}

/// Spawn a new actor with a bounded mailbox.
///
/// # Safety
///
/// Same requirements as [`hew_actor_spawn`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_bounded(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
    capacity: i32,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };

    // SAFETY: Returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new_bounded(capacity) };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
        })
    }
}

// ── Send ────────────────────────────────────────────────────────────────
// Send functions use native mailbox/scheduler. WASM sends go through bridge.

/// Send a message to an actor (fire-and-forget).
///
/// Deep-copies `data`. If the actor is idle, transitions it to runnable
/// and enqueues it on the scheduler.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { actor_send_internal(actor, msg_type, data, size) };
}

/// Send a wire-encoded message to an actor.
///
/// Extracts raw bytes from the `HewVec` (bytes type), deep-copies them
/// into the actor's mailbox, and frees the `HewVec`.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `bytes` must be a valid `HewVec*` (bytes type) or null.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wire(
    actor: *mut HewActor,
    msg_type: i32,
    bytes: *mut crate::vec::HewVec,
) {
    if bytes.is_null() || actor.is_null() {
        return;
    }
    // SAFETY: bytes is a valid HewVec. Extract raw byte data.
    let data = unsafe { crate::vec::hwvec_to_u8(bytes) };
    // SAFETY: actor is valid, data slice is valid.
    unsafe { actor_send_internal(actor, msg_type, data.as_ptr() as *mut c_void, data.len()) };
    // SAFETY: bytes was allocated by hew_vec and is no longer needed.
    unsafe { crate::vec::hew_vec_free(bytes) };
}

/// Send a message to an actor by actor ID.
///
/// Returns 0 on success, -1 if the actor ID is not currently live.
///
/// # Safety
///
/// `data` must point to at least `size` readable bytes, or be null when
/// `size` is 0.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_by_id(
    actor_id: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> c_int {
    let sent_local = {
        let guard = LIVE_ACTORS.lock_or_recover();
        guard.as_ref().is_some_and(|map| {
            if let Some(entry) = map.get(&actor_id) {
                let actor = entry.0;
                if actor.is_null() {
                    return false;
                }
                // SAFETY: actor pointer was discovered while LIVE_ACTORS is
                // locked, so it cannot be concurrently untracked/freed
                // during this send.
                unsafe { actor_send_internal(actor, msg_type, data, size) };
                true
            } else {
                false
            }
        })
    };

    if sent_local {
        return 0;
    }

    // Actor not found locally. If the PID belongs to a remote node,
    // route through the distributed node infrastructure.
    if crate::pid::hew_pid_is_local(actor_id) == 0 {
        // SAFETY: data validity is guaranteed by caller contract.
        return unsafe { crate::hew_node::try_remote_send(actor_id, msg_type, data, size) };
    }
    -1
}

/// Try to send a message, returning an error code on failure.
///
/// Returns `0` on success, or a negative error code (see [`HewError`]).
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_try_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    let mb = a.mailbox.cast::<HewMailbox>();

    // SAFETY: Mailbox is valid for the actor's lifetime.
    let result = unsafe { mailbox::hew_mailbox_try_send(mb, msg_type, data, size) };
    if result != 0 {
        return result;
    }

    // CAS IDLE → RUNNABLE; on success, schedule the actor.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        scheduler::sched_enqueue(actor);
    }

    0
}

// ── Close / Stop / Free ─────────────────────────────────────────────────

/// Close an actor, rejecting new messages.
///
/// Transitions the actor state to `Stopping`.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_close(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Close the mailbox so future sends are rejected.
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: mailbox is valid for actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // If actor is IDLE, transition directly to STOPPED.
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
        // SAFETY: actor just transitioned to Stopped; not being dispatched.
        unsafe { call_terminate_fn(actor) };
    }
}

/// Stop an actor, sending a system shutdown message.
///
/// Transitions the actor state to `Stopping` and enqueues a system
/// message (`msg_type = -1`) to signal the actor's dispatch function.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_stop(actor: *mut HewActor) {
    // Close the mailbox to reject new messages and transition to STOPPED if idle.
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { hew_actor_close(actor) };

    // SAFETY: Caller guarantees `actor` is valid and remains valid throughout this function.
    let a = unsafe { &*actor };
    let mb = a.mailbox.cast::<HewMailbox>();

    // If actor is still RUNNABLE or RUNNING, let it drain naturally.
    // Enqueue a sys message (-1) so the dispatch function sees the stop signal.
    // SAFETY: Mailbox is valid for the actor's lifetime.
    unsafe {
        mailbox::hew_mailbox_send_sys(mb, -1, ptr::null_mut(), 0);
    }
}

/// Free an actor and all associated resources.
///
/// Spin-waits until the actor reaches a terminal state, then frees state,
/// mailbox, and the actor itself.
///
/// # Safety
///
/// - `actor` must have been returned by a spawn function.
/// - The actor must not be used after this call.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) -> c_int {
    if actor.is_null() {
        crate::set_last_error("hew_actor_free: null actor pointer");
        return -1;
    }

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Wait until actor reaches a terminal or idle state (with timeout).
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    loop {
        let state = a.actor_state.load(Ordering::Acquire);
        if state == HewActorState::Stopped as i32
            || state == HewActorState::Crashed as i32
            || state == HewActorState::Idle as i32
        {
            break;
        }
        if std::time::Instant::now() >= deadline {
            break;
        }
        std::thread::yield_now();
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if state != HewActorState::Stopped as i32
        && state != HewActorState::Crashed as i32
        && state != HewActorState::Idle as i32
    {
        crate::set_last_error("actor still running after timeout");
        return -2;
    }

    // Cancel periodic timers, links, and monitors BEFORE untracking so
    // that any in-flight timer callback or propagation that checks
    // LIVE_ACTORS still sees this actor as live and can safely bail out.
    crate::timer_periodic::cancel_all_timers_for_actor(actor);
    let actor_id = a.id;
    crate::link::remove_all_links_for_actor(actor_id, actor);
    crate::monitor::remove_all_monitors_for_actor(actor_id, actor);

    // Remove from live tracking. If the actor was already consumed by
    // cleanup_all_actors (returns false), skip freeing to avoid
    // double-free.
    if !untrack_actor(actor) {
        crate::set_last_error("hew_actor_free: actor already freed or not tracked");
        return -1;
    }

    // Run terminate for actors freed while still Idle (never explicitly
    // stopped). terminate_called prevents double-execution.
    if state != HewActorState::Crashed as i32 {
        // SAFETY: actor is valid, not being dispatched (wait loop above).
        unsafe { call_terminate_fn(actor) };
    }

    // SAFETY: Caller guarantees `actor` is valid and not being dispatched.
    unsafe { free_actor_resources(actor) };
    0
}

// ── Budget API ──────────────────────────────────────────────────────────

/// Set the per-actor message processing budget.
///
/// A budget of `0` resets to the default ([`HEW_MSG_BUDGET`]).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_budget(actor: *mut HewActor, budget: u32) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "budget values are small positive integers, well within i32 range"
    )]
    if budget == 0 {
        a.budget.store(HEW_MSG_BUDGET, Ordering::Relaxed);
    } else {
        a.budget.store(budget as i32, Ordering::Relaxed);
    }
}

/// Query the current per-actor message processing budget.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_budget(actor: *const HewActor) -> u32 {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_sign_loss,
        reason = "budget is always set to a positive value"
    )]
    let result = a.budget.load(Ordering::Relaxed) as u32;
    result
}

/// Register a terminate callback on an actor.
///
/// The terminate function is called with the actor's state pointer when
/// the actor transitions to the Stopped state (or at process exit for
/// actors still idle). Panics inside the callback are caught and do not
/// prevent cleanup.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `terminate_fn` must point to a function with C ABI that accepts
///   a single `*mut c_void` (the actor state).
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_terminate(
    actor: *mut HewActor,
    terminate_fn: unsafe extern "C" fn(*mut c_void),
) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &mut *actor };
    a.terminate_fn = Some(terminate_fn);
}

/// Set the per-actor reduction budget (operations per dispatch).
///
/// A value of `0` resets to the default ([`HEW_DEFAULT_REDUCTIONS`]).
/// Higher values allow an actor to run longer before yielding.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_reductions(actor: *mut HewActor, reductions: u32) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "reduction values are small positive integers, well within i32 range"
    )]
    if reductions == 0 {
        a.reductions
            .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);
    } else {
        a.reductions.store(reductions as i32, Ordering::Relaxed);
    }
}

/// Query the current per-actor reduction budget.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_reductions(actor: *const HewActor) -> u32 {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_sign_loss,
        reason = "reductions is always set to a positive value"
    )]
    {
        a.reductions.load(Ordering::Relaxed) as u32
    }
}

/// Enable hibernation for an actor.
///
/// When an actor goes through `threshold` consecutive activations with
/// zero messages, it is marked as hibernating. A hibernating actor is
/// skipped by the scheduler until a new message arrives.
///
/// Pass 0 to disable hibernation (default).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_hibernation(actor: *mut HewActor, threshold: c_int) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.hibernation_threshold
        .store(threshold.max(0), Ordering::Relaxed);
    // Reset hibernation state when threshold changes.
    a.idle_count.store(0, Ordering::Relaxed);
    a.hibernating.store(0, Ordering::Relaxed);
}

/// Return 1 if the actor is currently hibernating, 0 otherwise.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_is_hibernating(actor: *const HewActor) -> c_int {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.hibernating.load(Ordering::Relaxed)
}

/// Wake an actor from hibernation.
///
/// This is automatically called when a message is sent to a hibernating
/// actor, but can also be called explicitly.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wake(actor: *mut HewActor) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.idle_count.store(0, Ordering::Relaxed);
    a.hibernating.store(0, Ordering::Relaxed);
}

/// Set the scheduling priority for an actor.
///
/// - 0 = high priority (gets 2× message budget)
/// - 1 = normal priority (default)
/// - 2 = low priority (gets ½ message budget)
///
/// Values outside 0-2 are clamped.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_priority(actor: *mut HewActor, priority: c_int) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    let clamped = priority.clamp(HEW_PRIORITY_HIGH, HEW_PRIORITY_LOW);
    a.priority.store(clamped, Ordering::Relaxed);
}

/// Query the current scheduling priority.
///
/// Returns 0 (high), 1 (normal), or 2 (low).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_priority(actor: *const HewActor) -> c_int {
    cabi_guard!(actor.is_null(), HEW_PRIORITY_NORMAL);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.priority.load(Ordering::Relaxed)
}

// ── Internal send helper ────────────────────────────────────────────────

/// Send a message, returning a runtime error code.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_send_result_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Check for injected drop fault (testing only). Silently discard
    // the message without enqueuing it.
    if crate::deterministic::check_drop_fault(a.id) {
        return HewError::Ok as i32; // Pretend success.
    }

    let mb = a.mailbox.cast::<HewMailbox>();

    // SAFETY: Mailbox is valid for the actor's lifetime.
    let result = unsafe { mailbox::hew_mailbox_send(mb, msg_type, data, size) };
    if result != 0 {
        return result;
    }

    let sender = hew_actor_self();
    let trace_actor_id = if sender.is_null() {
        a.id
    } else {
        // SAFETY: the scheduler sets CURRENT_ACTOR to a live actor during dispatch.
        unsafe { (*sender).id }
    };
    crate::tracing::record_send(trace_actor_id, msg_type);

    // CAS IDLE → RUNNABLE; on success, schedule the actor.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // Clear hibernation state — the actor has work to do.
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        scheduler::sched_enqueue(actor);
    }

    HewError::Ok as i32
}

/// Send a message, returning `true` on success.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_send_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> bool {
    // SAFETY: same preconditions as actor_send_result_internal; we only
    // translate its error code into a boolean success/failure result.
    unsafe { actor_send_result_internal(actor, msg_type, data, size) == HewError::Ok as i32 }
}

// ── Ask (request-response) ──────────────────────────────────────────────
// Ask functions use native reply channels and are not available on WASM.

/// Send a synchronous request and block until a reply arrives.
///
/// The reply channel pointer is **packed at the end** of the message
/// data, matching the C runtime convention:
/// `[original_data | reply_channel_ptr]`
///
/// Returns the reply value (caller must free with [`libc::free`]), or
/// null if no reply was produced.
///
/// # Safety
///
/// - `actor` must be a valid actor pointer.
/// - `data` must point to at least `size` readable bytes, or be null.
///
#[cfg(not(target_arch = "wasm32"))]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "packed buffer is allocated via malloc which guarantees suitable alignment for any built-in type"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let ptr_size = std::mem::size_of::<*mut c_void>();
    let Some(total) = size.checked_add(ptr_size) else {
        return std::ptr::null_mut();
    };

    let ch = reply_channel::hew_reply_channel_new();

    // Pack: [original_data | reply_channel_ptr]
    // SAFETY: malloc for packed buffer.
    let packed = unsafe { libc::malloc(total) };
    if packed.is_null() {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return ptr::null_mut();
    }
    // SAFETY: copying data into packed buffer; reply channel pointer slot may be
    // SAFETY: unaligned, so write_unaligned is required.
    unsafe {
        if size > 0 && !data.is_null() {
            ptr::copy_nonoverlapping(data.cast::<u8>(), packed.cast::<u8>(), size);
        }
        let ch_slot = packed.cast::<u8>().add(size).cast::<*mut c_void>();
        ptr::write_unaligned(ch_slot, ch.cast());
    }

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };
    // SAFETY: actor is valid, packed data is valid.
    let send_result = unsafe { actor_send_result_internal(actor, msg_type, packed, total) };
    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };

    if send_result != HewError::Ok as i32 {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return std::ptr::null_mut();
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait(ch) };

    // SAFETY: ch was created by hew_reply_channel_new.
    unsafe { reply_channel::hew_reply_channel_free(ch) };

    result
}

/// Send a message and block until the actor replies or the timeout
/// expires.
///
/// Returns the reply value, or null on timeout.
///
/// # Safety
///
/// Same requirements as [`hew_actor_ask`].
///
#[cfg(not(target_arch = "wasm32"))]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "packed buffer is allocated via malloc which guarantees suitable alignment for any built-in type"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_timeout(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: i32,
) -> *mut c_void {
    let ptr_size = std::mem::size_of::<*mut c_void>();
    let Some(total) = size.checked_add(ptr_size) else {
        return std::ptr::null_mut();
    };

    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: malloc for packed buffer.
    let packed = unsafe { libc::malloc(total) };
    if packed.is_null() {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return ptr::null_mut();
    }
    // SAFETY: copying data into packed buffer; reply channel pointer slot may be
    // SAFETY: unaligned, so write_unaligned is required.
    unsafe {
        if size > 0 && !data.is_null() {
            ptr::copy_nonoverlapping(data.cast::<u8>(), packed.cast::<u8>(), size);
        }
        let ch_slot = packed.cast::<u8>().add(size).cast::<*mut c_void>();
        ptr::write_unaligned(ch_slot, ch.cast());
    }

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };
    // SAFETY: actor is valid, packed data is valid.
    let send_result = unsafe { actor_send_result_internal(actor, msg_type, packed, total) };
    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };

    if send_result != HewError::Ok as i32 {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return std::ptr::null_mut();
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait_timeout(ch, timeout_ms) };

    if result.is_null() {
        // Timeout: mark the channel as cancelled so the late replier
        // handles cleanup via its retained sender-side reference.
        // SAFETY: ch is still live while the caller-side reference is released.
        unsafe { reply_channel::hew_reply_channel_cancel(ch) };
        // SAFETY: release the caller-side reference after recording cancellation.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
    } else {
        // Got a reply — release the caller-side reference.
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
    }

    result
}

/// Send a message with a caller-provided reply channel.
///
/// The reply channel is packed into the message data.
/// The caller is responsible for waiting on and freeing `ch`.
///
/// # Safety
///
/// - `actor` must be a valid actor pointer.
/// - `data` must point to at least `size` readable bytes, or be null.
/// - `ch` must be a valid reply channel pointer.
///
/// Returns `0` ([`HewError::Ok`]) on success, or a negative [`HewError`] code
/// if the ask could not be submitted. Callers must handle failures explicitly
/// instead of waiting on `ch`, because no reply will ever arrive in that case.
///
#[cfg(not(target_arch = "wasm32"))]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "packed buffer is allocated via malloc which guarantees suitable alignment for any built-in type"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_with_channel(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut HewReplyChannel,
) -> i32 {
    let ptr_size = std::mem::size_of::<*mut c_void>();
    let Some(total) = size.checked_add(ptr_size) else {
        return HewError::ErrOom as i32;
    };

    // SAFETY: malloc for packed buffer.
    let packed = unsafe { libc::malloc(total) };
    if packed.is_null() {
        return HewError::ErrOom as i32;
    }
    // SAFETY: copying data into packed buffer; reply channel pointer slot may be
    // SAFETY: unaligned, so write_unaligned is required.
    unsafe {
        if size > 0 && !data.is_null() {
            ptr::copy_nonoverlapping(data.cast::<u8>(), packed.cast::<u8>(), size);
        }
        let ch_slot = packed.cast::<u8>().add(size).cast::<*mut c_void>();
        ptr::write_unaligned(ch_slot, ch.cast());
    }

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };
    // SAFETY: actor is valid, packed data is valid.
    let send_result = unsafe { actor_send_result_internal(actor, msg_type, packed, total) };
    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };

    if send_result != HewError::Ok as i32 {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
    }

    send_result
}

/// Perform a blocking ask against an actor identified by PID.
///
/// Looks up the actor in `LIVE_ACTORS`, packs a reply channel into the
/// message, and waits for the reply. Returns the reply pointer and writes
/// the reply size to `*out_size`.
///
/// Returns null if the actor is not found locally or the send fails.
///
/// # Safety
///
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
/// - `out_size` must be a valid, non-null writable pointer.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn hew_actor_ask_by_id(
    actor_id: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let ptr_size = std::mem::size_of::<*mut c_void>();
    let Some(total) = size.checked_add(ptr_size) else {
        return std::ptr::null_mut();
    };

    let ch = reply_channel::hew_reply_channel_new();

    // Pack: [original_data | reply_channel_ptr]
    // SAFETY: malloc for packed buffer.
    let packed = unsafe { libc::malloc(total) };
    if packed.is_null() {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return ptr::null_mut();
    }
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "write_unaligned on the next line handles misalignment"
    )]
    // SAFETY: copying data into packed buffer; write_unaligned handles the
    // potentially-misaligned channel slot.
    unsafe {
        if size > 0 && !data.is_null() {
            ptr::copy_nonoverlapping(data.cast::<u8>(), packed.cast::<u8>(), size);
        }
        let ch_slot = packed.cast::<u8>().add(size).cast::<*mut c_void>();
        ptr::write_unaligned(ch_slot, ch.cast());
    }

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };

    // Look up actor and send packed message.
    let sent = {
        let guard = LIVE_ACTORS.lock_or_recover();
        guard.as_ref().is_some_and(|map| {
            if let Some(entry) = map.get(&actor_id) {
                let actor = entry.0;
                if actor.is_null() {
                    return false;
                }
                // SAFETY: actor and packed data are valid while LIVE_ACTORS
                // is locked.
                let rc = unsafe { actor_send_result_internal(actor, msg_type, packed, total) };
                rc == HewError::Ok as i32
            } else {
                false
            }
        })
    };

    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };

    if !sent {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return std::ptr::null_mut();
    }

    let mut reply_size: usize = 0;
    // SAFETY: ch is valid and single-reader; reply_size is a valid stack pointer.
    let result = unsafe { reply_channel::hew_reply_wait_with_size(ch, &raw mut reply_size) };

    // Store the reply size in a thread-local so the caller can retrieve it.
    LAST_REPLY_SIZE.set(reply_size);

    // SAFETY: ch was created by hew_reply_channel_new.
    unsafe { reply_channel::hew_reply_channel_free(ch) };

    result
}

// Thread-local storage for the reply size from the last `hew_actor_ask_by_id`.
std::thread_local! {
    static LAST_REPLY_SIZE: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Retrieve the size of the reply data from the most recent
/// `hew_actor_ask_by_id` call on the current thread.
pub(crate) unsafe fn hew_reply_data_size(_ptr: *mut c_void) -> usize {
    LAST_REPLY_SIZE.get()
}

// ── Trap / Error ────────────────────────────────────────────────────────

/// Trap (panic) an actor: store an error code, close the mailbox, and
/// transition to a terminal state. If the actor has a supervisor, notify it.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_trap(actor: *mut HewActor, error_code: i32) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Choose terminal state: Crashed if error_code != 0, Stopped otherwise.
    let terminal = if error_code != 0 {
        HewActorState::Crashed as i32
    } else {
        HewActorState::Stopped as i32
    };

    // Read supervisor fields before setting terminal state to avoid a race
    // where the supervisor on another thread frees the actor between the
    // state transition and the supervisor field reads.
    let supervisor = a.supervisor;
    let supervisor_child_index = a.supervisor_child_index;
    let actor_id = a.id;

    // Close mailbox to reject new messages.
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: mailbox is valid for actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // Transition to terminal state using CAS to ensure only one thread
    // can successfully trap/stop the actor. If another thread already
    // transitioned to a terminal state, bail out.
    loop {
        let current = a.actor_state.load(Ordering::Acquire);
        if current == HewActorState::Stopped as i32 || current == HewActorState::Crashed as i32 {
            return;
        }
        if a.actor_state
            .compare_exchange(current, terminal, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
        {
            break;
        }
    }

    // Store error code only after winning the CAS race.
    a.error_code.store(error_code, Ordering::Release);
    let lifecycle_event = if terminal == HewActorState::Crashed as i32 {
        crate::tracing::SPAN_CRASH
    } else {
        crate::tracing::SPAN_STOP
    };
    crate::tracing::hew_trace_lifecycle(actor_id, lifecycle_event);

    // Propagate exit to linked actors and notify monitors.
    // Do this BEFORE notifying supervisor to ensure proper ordering.
    crate::link::propagate_exit_to_links(actor_id, error_code);
    crate::monitor::notify_monitors_on_death(actor_id, terminal);

    // Wake any actor group condvars waiting on this actor.
    crate::actor_group::notify_actor_death(actor_id);

    // Notify supervisor if one exists.
    if !supervisor.is_null() {
        // SAFETY: supervisor back-pointer was set by hew_supervisor_add_child.
        unsafe {
            crate::supervisor::hew_supervisor_notify_child_event(
                supervisor.cast(),
                supervisor_child_index,
                actor_id,
                terminal,
            );
        }
    }
}

/// Return the error code stored on an actor (0 = no error).
///
/// # Safety
///
/// `actor` must be a valid pointer to a [`HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_error(actor: *const HewActor) -> i32 {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { &*actor }.error_code.load(Ordering::Acquire)
}

// ── Self (thread-local) ─────────────────────────────────────────────────

/// Return the actor currently being dispatched on this worker thread.
///
/// Returns null if called outside of a dispatch context.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_actor_self() -> *mut HewActor {
    CURRENT_ACTOR.with(Cell::get)
}

/// Return the actor currently being dispatched.
///
/// Returns null if called outside of a dispatch context.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_actor_self() -> *mut HewActor {
    // SAFETY: WASM is single-threaded.
    unsafe { CURRENT_ACTOR_WASM }
}

/// Trigger a panic in the current execution context.
///
/// Inside an actor: longjmps back to the scheduler, which marks the
/// actor as `Crashed`. The supervisor (if any) will restart the actor
/// according to its restart strategy.
///
/// Outside an actor (e.g. `main`): exits the process with code 101.
///
/// This function never returns.
#[no_mangle]
pub extern "C" fn hew_panic() {
    // Try direct longjmp recovery first. This avoids going through the
    // signal/exception path, which is essential on Windows where longjmp
    // from a VEH handler causes STATUS_BAD_STACK.
    //
    // SAFETY: Called from actor dispatch context (stack chain includes the
    // scheduler's sigsetjmp frame). If recovery context exists, longjmps
    // directly — never returns. If no context, returns and we fall through
    // to a clean process exit.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe {
        crate::signal::try_direct_longjmp();
    }

    // No recovery context (e.g. panic called from main) — exit cleanly.
    // Exit code 101 follows Rust's convention for panics.
    std::process::exit(101);
}

/// Crash the current actor after printing a message.
///
/// # Safety
///
/// `msg` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_panic_msg(msg: *const std::ffi::c_char) {
    if !msg.is_null() {
        // SAFETY: msg is non-null (checked above) and caller guarantees valid C string.
        let s = unsafe { std::ffi::CStr::from_ptr(msg) };
        if let Ok(text) = s.to_str() {
            if !text.is_empty() {
                eprintln!("{text}");
            }
        }
    }
    hew_panic();
}

/// Return the PID of the given actor.
///
/// # Safety
///
/// `actor` must be a valid pointer to a [`HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_pid(actor: *mut HewActor) -> u64 {
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { &*actor }.pid
}

/// Return the PID of the actor currently being dispatched on this worker
/// thread.
///
/// Returns `0` if called outside of a dispatch context.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_actor_self_pid() -> u64 {
    let actor = CURRENT_ACTOR.with(Cell::get);
    if actor.is_null() {
        return 0;
    }
    // SAFETY: The thread-local is only set to a valid actor during dispatch.
    unsafe { &*actor }.pid
}

/// Return the PID of the actor currently being dispatched.
///
/// Returns `0` if called outside of a dispatch context.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_actor_self_pid() -> u64 {
    // SAFETY: WASM is single-threaded.
    let actor = unsafe { CURRENT_ACTOR_WASM };
    if actor.is_null() {
        return 0;
    }
    // SAFETY: The static is only set to a valid actor during dispatch.
    unsafe { &*actor }.pid
}

/// Self-stop: the currently running actor requests its own shutdown.
///
/// Closes the mailbox and CAS transitions from `Running` to `Stopping`.
/// The scheduler will handle the final transition to `Stopped` after
/// dispatch returns.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_actor_self_stop() {
    let actor = CURRENT_ACTOR.with(Cell::get);
    if actor.is_null() {
        return;
    }
    // SAFETY: The thread-local is only set to a valid actor during dispatch.
    let a = unsafe { &*actor };

    // Close the mailbox to reject new messages.
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: mailbox is valid for actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // CAS Running → Stopping. Only the dispatching worker can be in Running
    // for this actor, so this CAS should succeed.
    let _ = a.actor_state.compare_exchange(
        HewActorState::Running as i32,
        HewActorState::Stopping as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
}

/// Self-stop: the currently running actor requests its own shutdown.
///
/// CAS transitions from `Running` to `Stopping`.
/// The WASM scheduler will handle the final transition to `Stopped` after
/// dispatch returns. No mailbox close needed — WASM is cooperative
/// single-threaded, so no concurrent sends can race.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_actor_self_stop() {
    // SAFETY: WASM is single-threaded.
    let actor = unsafe { CURRENT_ACTOR_WASM };
    if actor.is_null() {
        return;
    }
    // SAFETY: The static is only set to a valid actor during dispatch.
    let a = unsafe { &*actor };

    // CAS Running → Stopping.
    let _ = a.actor_state.compare_exchange(
        HewActorState::Running as i32,
        HewActorState::Stopping as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
}

// ── WASM actor API ──────────────────────────────────────────────────────
// On WASM, spawn/send/ask/stop/close use the WASM mailbox and cooperative
// scheduler. These provide the same C ABI surface as native so that
// codegen-emitted calls resolve transparently.

#[cfg(target_arch = "wasm32")]
extern "C" {
    fn hew_mailbox_new() -> *mut c_void;
    fn hew_mailbox_new_bounded(capacity: i32) -> *mut c_void;
    fn hew_mailbox_new_with_policy(capacity: usize, policy: HewOverflowPolicy) -> *mut c_void;
    fn hew_mailbox_send(mb: *mut c_void, msg_type: i32, data: *mut c_void, size: usize) -> i32;
    fn hew_mailbox_send_sys(mb: *mut c_void, msg_type: i32, data: *mut c_void, size: usize) -> i32;
    fn hew_mailbox_close(mb: *mut c_void);
    fn hew_wasm_sched_enqueue(actor: *mut c_void);
    fn hew_sched_run();
}

/// Spawn a new actor with an unbounded mailbox (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    // SAFETY: hew_mailbox_new is a trusted FFI constructor returning a valid mailbox pointer.
    let mailbox = unsafe { hew_mailbox_new() };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox,
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
        })
    }
}

/// Spawn a new actor with a bounded mailbox (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn_bounded`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_bounded(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
    capacity: i32,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    // SAFETY: hew_mailbox_new_bounded is a trusted FFI constructor returning a valid mailbox pointer.
    let mailbox = unsafe { hew_mailbox_new_bounded(capacity) };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox,
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
        })
    }
}

/// Spawn a new actor from options (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn_opts`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts(opts: *const HewActorOpts) -> *mut HewActor {
    if opts.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` points to a valid HewActorOpts.
    let opts = unsafe { &*opts };

    // SAFETY: Caller guarantees opts.init_state is readable for opts.state_size bytes.
    let actor_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Trusted FFI constructor; capacity/policy were derived from opts above.
        unsafe { hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Trusted FFI constructor for an unbounded mailbox.
        unsafe { hew_mailbox_new() }
    };

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            mailbox,
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
        })
    }
}

/// Send a message to an actor (WASM, fire-and-forget).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_send`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // SAFETY: Mailbox is valid for the actor's lifetime.
    unsafe { hew_mailbox_send(a.mailbox, msg_type, data, size) };

    // Transition IDLE → RUNNABLE and enqueue.
    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        // SAFETY: actor is valid.
        unsafe { hew_wasm_sched_enqueue(actor.cast()) };
    }
}

/// Try to send a message (WASM). Identical to [`hew_actor_send`] on WASM
/// since there is no blocking distinction.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_try_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `actor` is a valid pointer.
    let a = unsafe { &*actor };
    // SAFETY: a.mailbox is a valid mailbox pointer for the actor's lifetime.
    let result = unsafe { hew_mailbox_send(a.mailbox, msg_type, data, size) };
    if result != 0 {
        return result;
    }

    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        // SAFETY: actor is valid.
        unsafe { hew_wasm_sched_enqueue(actor.cast()) };
    }

    0
}

/// Cooperative ask: send a request and run the scheduler until a reply
/// arrives (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    use crate::reply_channel_wasm;

    let ptr_size = std::mem::size_of::<*mut c_void>();
    let Some(total) = size.checked_add(ptr_size) else {
        return ptr::null_mut();
    };

    let ch = reply_channel_wasm::hew_reply_channel_new();

    // Pack: [original_data | reply_channel_ptr]
    // SAFETY: malloc for packed buffer.
    let packed = unsafe { libc::malloc(total) };
    if packed.is_null() {
        // SAFETY: ch was created by hew_reply_channel_new above.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        return ptr::null_mut();
    }
    // SAFETY: packed is a total-byte malloc allocation; data is readable for size bytes when non-null.
    // SAFETY: reply channel pointer slot may be unaligned, so write_unaligned is required.
    unsafe {
        if size > 0 && !data.is_null() {
            ptr::copy_nonoverlapping(data.cast::<u8>(), packed.cast::<u8>(), size);
        }
        let ch_slot = packed.cast::<u8>().add(size).cast::<*mut c_void>();
        ptr::write_unaligned(ch_slot, ch.cast());
    }

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel_wasm::hew_reply_channel_retain(ch) };
    // Send the packed message.
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // SAFETY: a.mailbox is a valid mailbox pointer.
    let send_result = unsafe { hew_mailbox_send(a.mailbox, msg_type, packed, total) };
    // SAFETY: packed buffer ownership transferred to mailbox (deep-copied).
    unsafe { libc::free(packed) };

    if send_result != HewError::Ok as i32 {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        // SAFETY: release the caller-side reference before returning failure.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        return ptr::null_mut();
    }

    // Transition IDLE → RUNNABLE and enqueue.
    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // SAFETY: actor is valid.
        unsafe { hew_wasm_sched_enqueue(actor.cast()) };
    }

    // Cooperatively process messages until the reply is deposited.
    // SAFETY: scheduler must be initialized.
    unsafe { hew_sched_run() };

    // Read the reply and free the channel.
    // SAFETY: ch is a valid reply channel pointer created above.
    let reply = unsafe { reply_channel_wasm::reply_take(ch) };
    // SAFETY: ch was created by hew_reply_channel_new and is no longer needed.
    unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };

    reply
}

/// Close an actor, rejecting new messages (WASM).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_close(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Close the mailbox.
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe { hew_mailbox_close(a.mailbox) };
    }

    // If IDLE, transition directly to STOPPED.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // SAFETY: actor just transitioned to Stopped; not being dispatched.
        unsafe { call_terminate_fn(actor) };
    }
}

/// Stop an actor, sending a system shutdown message (WASM).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_stop(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { hew_actor_close(actor) };

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Send a system shutdown message (-1).
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe { hew_mailbox_send_sys(a.mailbox, -1, ptr::null_mut(), 0) };
    }
}

/// Free an actor and all associated resources (WASM).
///
/// # Safety
///
/// - `actor` must have been returned by a spawn function.
/// - The actor must not be used after this call.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) -> c_int {
    if actor.is_null() {
        return 0;
    }

    if !untrack_actor(actor) {
        return 0;
    }

    // SAFETY: Caller guarantees `actor` is valid and not being dispatched.
    unsafe { free_actor_resources(actor) };
    0
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
    }

    #[test]
    fn ask_with_channel_send_failure_returns_error() {
        // SAFETY: Spawning with null state and a valid dispatch function.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor pointer is valid — returned by hew_actor_spawn above.
        unsafe {
            hew_actor_close(actor);
        }

        let ch = reply_channel::hew_reply_channel_new();
        // SAFETY: actor and ch are valid pointers from their respective constructors.
        let rc = unsafe { hew_actor_ask_with_channel(actor, 0, std::ptr::null_mut(), 0, ch) };
        assert_eq!(rc, HewError::ErrActorStopped as i32);

        // SAFETY: ch and actor are valid pointers; freeing resources after test.
        unsafe {
            reply_channel::hew_reply_channel_free(ch);
            assert_eq!(hew_actor_free(actor), 0);
        }
    }

    #[test]
    fn free_actor_resources_completes_when_terminate_finishes_quickly() {
        // SAFETY: null state, valid dispatch.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor pointer is valid — returned by hew_actor_spawn.
        unsafe {
            hew_actor_close(actor);
        }

        let start = std::time::Instant::now();
        // SAFETY: actor is valid, closed, and in a terminal-safe state.
        let rc = unsafe { hew_actor_free(actor) };
        let elapsed = start.elapsed();

        assert_eq!(rc, 0);
        assert!(
            elapsed < std::time::Duration::from_secs(1),
            "free should complete quickly for a cooperating actor, took {elapsed:?}"
        );
    }

    #[test]
    fn free_actor_resources_times_out_on_hanging_terminate() {
        // Simulate an actor whose terminate_called is true but
        // terminate_finished never becomes true. The bounded spin-wait in
        // free_actor_resources should time out after ~5s and proceed.
        // SAFETY: null state, valid dispatch.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid.
        let a = unsafe { &*actor };
        // Simulate a hung terminate: called=true, finished=false.
        a.terminate_called.store(true, Ordering::Release);
        a.terminate_finished.store(false, Ordering::Release);
        // Put actor in Stopped state so hew_actor_free doesn't fail the
        // state check.
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        let start = std::time::Instant::now();
        // SAFETY: actor is valid and in Stopped state.
        let rc = unsafe { hew_actor_free(actor) };
        let elapsed = start.elapsed();

        assert_eq!(rc, 0);
        // Should take roughly 5 seconds (the timeout), not hang forever.
        assert!(
            elapsed >= std::time::Duration::from_secs(4),
            "should wait ~5s before timing out, took {elapsed:?}"
        );
        assert!(
            elapsed < std::time::Duration::from_secs(10),
            "should not hang much longer than the timeout, took {elapsed:?}"
        );
    }
}
