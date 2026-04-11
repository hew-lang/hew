//! Hew runtime: actor struct definition and state constants.
//!
//! Defines the [`HewActor`] struct layout for C ABI compatibility and the
//! actor state machine constants. The full actor API (spawn, send, activate)
//! will be implemented in a future iteration.

use crate::util::MutexExt;
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, Ordering};
use std::sync::Mutex;

use crate::internal::types::{AskError, HewActorState, HewError, HewOverflowPolicy};
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

// ── Thread-local local ask error ────────────────────────────────────────

thread_local! {
    /// Error discriminant for the most recent `hew_actor_ask` /
    /// `hew_actor_ask_timeout` / `hew_actor_ask_by_id` call on this thread.
    ///
    /// Set to an [`AskError`] value on every NULL return; reset to
    /// `AskError::None` on every non-NULL return and after being read via
    /// `hew_actor_ask_take_last_error`.
    static LAST_ACTOR_ASK_ERROR: Cell<i32> = const { Cell::new(AskError::None as i32) };
}

/// Write `err` to the local-ask error slot and return `null`.
#[inline]
fn actor_ask_null(err: AskError) -> *mut c_void {
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(err as i32));
    ptr::null_mut()
}

/// Clear the local-ask error slot (called on successful ask return).
#[inline]
fn actor_ask_clear() {
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
}

/// Read and clear the last local ask error for the current thread.
///
/// Intended to be called by `hew_node.rs` when bridging a local delegation
/// error into the node error slot, without exposing the slot directly.
pub(crate) fn actor_ask_take_last_error_raw() -> i32 {
    LAST_ACTOR_ASK_ERROR.with(|c| {
        let v = c.get();
        c.set(AskError::None as i32);
        v
    })
}

/// Map a send-side [`HewError`] code to its [`AskError`] discriminant.
///
/// Only `ErrMailboxFull` has a dedicated ask-error discriminant.  All other
/// failure codes mean the actor is unreachable and map to `ActorStopped`.
/// The WASM ask path normalises `ErrClosed` → `ErrActorStopped` before
/// calling this function, so `ErrClosed` never reaches the `_` arm in
/// practice.
#[inline]
fn send_err_to_ask_err(code: i32) -> AskError {
    const FULL: i32 = HewError::ErrMailboxFull as i32;
    match code {
        FULL => AskError::MailboxFull,
        // JUSTIFIED: `ErrActorStopped` (-2) is the normal "unreachable" code.
        // `ErrOom` (-5) has no dedicated ask-error discriminant — OOM is a
        // fatal system condition and callers cannot usefully retry.  `ErrClosed`
        // (-4) is normalised to `ErrActorStopped` by the WASM ask path before
        // reaching here, but would also be correctly subsumed.  Any future
        // unknown code is similarly "actor unreachable" — the only actionable
        // send-side distinction for callers is `MailboxFull` vs `ActorStopped`.
        _ => AskError::ActorStopped,
    }
}

/// Read and clear the last local ask error discriminant for the current thread.
///
/// Returns one of the [`AskError`] values as an `i32`.  The slot is reset to
/// `AskError::None` (0) after each call, so repeated calls without an
/// intervening failed ask return 0.
///
/// Call this immediately after `hew_actor_ask` or `hew_actor_ask_timeout`
/// returns `NULL` to distinguish the failure reason:
///
/// - `0` (`None`): the ask succeeded (non-null reply) or returned a
///   legitimate null reply; no error.
/// - `5` (`Timeout`): deadline elapsed before the handler replied.
/// - `9` (`ActorStopped`): the target actor was stopped, the mailbox was
///   closed (actor not found), or message-node allocation failed (OOM) —
///   all cases where the send could not be delivered and retry is not useful.
/// - `10` (`MailboxFull`): bounded mailbox was at capacity.
/// - `11` (`OrphanedAsk`): send succeeded but the actor's mailbox was torn
///   down before the handler called `hew_reply`.
/// - `12` (`NoRunnableWork`): WASM cooperative path only — no runnable work
///   remains, so the ask loop cannot make further progress.
#[no_mangle]
pub extern "C" fn hew_actor_ask_take_last_error() -> i32 {
    actor_ask_take_last_error_raw()
}

#[cfg(all(test, not(target_arch = "wasm32")))]
thread_local! {
    static FAIL_ACTOR_STATE_ALLOC_ON_NTH: Cell<usize> = const { Cell::new(usize::MAX) };
}

#[cfg(all(test, not(target_arch = "wasm32")))]
struct ActorStateAllocFailureGuard;

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for ActorStateAllocFailureGuard {
    fn drop(&mut self) {
        FAIL_ACTOR_STATE_ALLOC_ON_NTH.with(|slot| slot.set(usize::MAX));
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fail_actor_state_alloc_on_nth(n: usize) -> ActorStateAllocFailureGuard {
    FAIL_ACTOR_STATE_ALLOC_ON_NTH.with(|slot| slot.set(n));
    ActorStateAllocFailureGuard
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn should_fail_actor_state_alloc() -> bool {
    FAIL_ACTOR_STATE_ALLOC_ON_NTH.with(|slot| {
        let remaining = slot.get();
        if remaining == usize::MAX {
            return false;
        }
        if remaining == 0 {
            slot.set(usize::MAX);
            return true;
        }
        slot.set(remaining - 1);
        false
    })
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
    /// Per-actor arena bump allocator on WASM.  Allocated during spawn via
    /// `hew_arena_new()`, installed as the current arena during each activation,
    /// reset after each dispatch cycle, and freed during actor teardown.
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

#[inline]
fn actor_free_state_is_quiescent(state: i32) -> bool {
    state == HewActorState::Stopped as i32
        || state == HewActorState::Crashed as i32
        || state == HewActorState::Idle as i32
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone, Copy)]
struct DeferredActorFree(*mut HewActor);

#[cfg(not(target_arch = "wasm32"))]
// SAFETY: the deferred free thread only observes the raw pointer value after
// the owning dispatch thread has requested teardown.
unsafe impl Send for DeferredActorFree {}

#[cfg(not(target_arch = "wasm32"))]
fn free_deferred_actor(deferred: DeferredActorFree) {
    // SAFETY: the runtime still owns `actor`; the background thread simply
    // retries the same free once the current dispatch unwinds.
    let rc = unsafe { hew_actor_free(deferred.0) };
    if rc != 0 {
        eprintln!("hew: warning: deferred actor free failed with rc={rc}");
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn defer_actor_free_on_background_thread(actor: *mut HewActor) -> c_int {
    let deferred = DeferredActorFree(actor);
    if std::thread::Builder::new()
        .name("deferred-actor-free".into())
        .spawn(move || free_deferred_actor(deferred))
        .is_err()
    {
        crate::set_last_error("hew_actor_free: failed to spawn deferred free thread");
        return -1;
    }
    0
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
        #[cfg(not(target_arch = "wasm32"))]
        {
            crate::timer_periodic::cancel_all_timers_for_actor(actor);
            // SAFETY: actor is valid (from LIVE_ACTORS set, not yet freed).
            let actor_id = unsafe { (*actor).id };
            crate::link::remove_all_links_for_actor(actor_id, actor);
            crate::monitor::remove_all_monitors_for_actor(actor_id, actor);
        }

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

/// Free an actor's resources (WASM version — delegates to `free_actor_resources_wasm`).
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(target_arch = "wasm32")]
unsafe fn free_actor_resources(actor: *mut HewActor) {
    // SAFETY: target_arch = wasm32 shares the same invariants as the test helper.
    unsafe { free_actor_resources_wasm(actor) };
}

/// Free an actor's resources using the WASM cleanup path.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn free_actor_resources_wasm(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // SAFETY: State was malloc'd by deep_copy_state.
    unsafe {
        libc::free(a.state);
        libc::free(a.init_state);
    }

    if !a.arena.is_null() {
        // SAFETY: Arena was created by hew_arena_new during spawn.
        unsafe { crate::arena::hew_arena_free_all(a.arena.cast::<crate::arena::ActorArena>()) };
    }

    if !a.mailbox.is_null() {
        let mb = a.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
        // SAFETY: this helper is only used with WASM mailboxes.
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

fn actor_state_malloc(size: usize) -> *mut c_void {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    {
        if should_fail_actor_state_alloc() {
            return ptr::null_mut();
        }
    }

    // SAFETY: `size` is forwarded to libc unchanged.
    unsafe { libc::malloc(size) }
}

/// Deep-copy `src` into a new malloc'd buffer.
///
/// Returns null if `src` is null, `size` is 0, or allocation fails.
/// On allocation failure, sets `hew_last_error` with the details.
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
        let dst = actor_state_malloc(size);
        if dst.is_null() {
            crate::set_last_error(format!(
                "OOM: failed to allocate {size} bytes for actor state copy"
            ));
            return ptr::null_mut();
        }
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

    // OOM on the restart-state copy: free resources the caller transferred
    // ownership of and propagate the failure as null.
    if !config.state.is_null() && config.state_size > 0 && init_state.is_null() {
        // SAFETY: config.state was malloc'd by the caller's deep_copy_state;
        // config.mailbox was allocated by the caller via hew_mailbox_new*.
        unsafe {
            libc::free(config.state);
            let mb = config.mailbox.cast::<HewMailbox>();
            if !mb.is_null() {
                mailbox::hew_mailbox_free(mb);
            }
        }
        return ptr::null_mut();
    }

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

    // OOM on the restart-state copy: free resources the caller transferred
    // ownership of and propagate the failure as null.
    if !config.state.is_null() && config.state_size > 0 && init_state.is_null() {
        // SAFETY: config.state was malloc'd by the caller's deep_copy_state;
        // config.mailbox was allocated by the caller via hew_mailbox_new*.
        unsafe {
            libc::free(config.state);
            let mb = config.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
            if !mb.is_null() {
                crate::mailbox_wasm::hew_mailbox_free(mb);
            }
        }
        return ptr::null_mut();
    }

    // Allocate the per-actor arena bump allocator.  Mirror the native path:
    // if allocation fails, free all resources already owned and return null.
    let arena = crate::arena::hew_arena_new();
    if arena.is_null() {
        // SAFETY: state / init_state were malloc'd above; mailbox was
        // allocated by the caller.
        unsafe {
            libc::free(config.state);
            libc::free(init_state);
            let mb = config.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
            if !mb.is_null() {
                crate::mailbox_wasm::hew_mailbox_free(mb);
            }
        }
        return ptr::null_mut();
    }

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
        arena: arena.cast::<c_void>(),
    });

    let raw = Box::into_raw(actor);
    track_actor(raw);
    raw
}

/// Spawn a new actor with an unbounded mailbox.
///
/// The initial state is deep-copied. The returned pointer must be freed
/// with [`hew_actor_free`]. Returns null on allocation failure
/// (details via [`hew_last_error`]).
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
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

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
    if !opts.init_state.is_null() && opts.state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

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
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

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
/// Closes the mailbox, transitions idle actors directly to `Stopped`, and
/// only enqueues a shutdown system message (`msg_type = -1`) when the actor is
/// already `Running`. Runnable actors already have a queued activation, so
/// closing their mailbox is enough to let that activation drain naturally to
/// `Stopped` without fabricating an extra system-queue entry.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_stop(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid and remains valid throughout this function.
    let a = unsafe { &*actor };
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: Mailbox is valid for the actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

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
        return;
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if state != HewActorState::Running as i32 {
        return;
    }

    if !mb.is_null() {
        // Running actors are already inside a dispatch; enqueue one sys
        // message (-1) so the next mailbox poll observes the shutdown
        // request. Runnable actors already have queued work and do not need an
        // extra wake-up signal just to notice that the mailbox closed.
        // SAFETY: Mailbox is valid for the actor's lifetime.
        unsafe {
            let _ = mailbox::mailbox_send_stop_sys_once(mb);
        }
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

    if hew_actor_self() == actor {
        let state = a.actor_state.load(Ordering::Acquire);
        if state == HewActorState::Stopping as i32 || actor_free_state_is_quiescent(state) {
            return defer_actor_free_on_background_thread(actor);
        }
        crate::set_last_error("hew_actor_free: current actor is still dispatching");
        return -2;
    }

    // Wait until actor reaches a terminal or idle state (with timeout).
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    loop {
        let state = a.actor_state.load(Ordering::Acquire);
        if actor_free_state_is_quiescent(state) {
            break;
        }
        if std::time::Instant::now() >= deadline {
            break;
        }
        std::thread::yield_now();
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if !actor_free_state_is_quiescent(state) {
        crate::set_last_error("actor still running after timeout");
        return -2;
    }

    // Cancel periodic timers, links, and monitors BEFORE untracking so
    // that any in-flight timer callback or propagation that checks
    // LIVE_ACTORS still sees this actor as live and can safely bail out.
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::timer_periodic::cancel_all_timers_for_actor(actor);
        let actor_id = a.id;
        crate::link::remove_all_links_for_actor(actor_id, actor);
        crate::monitor::remove_all_monitors_for_actor(actor_id, actor);
        // SAFETY: actor is still live here, so named-node cleanup can
        // resolve the owning node and unregister any names bound to this PID.
        unsafe { crate::hew_node::unregister_actor_names(actor_id) };
    }

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
    unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ptr::null_mut()) }
}

/// Like [`actor_send_result_internal`] but with an explicit reply channel
/// that is set on the message node (for the ask pattern).
#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_send_result_internal_reply(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> i32 {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Check for injected drop fault (testing only). Silently discard
    // the message without enqueuing it.
    if crate::deterministic::check_drop_fault(a.id) {
        return HewError::Ok as i32; // Pretend success.
    }

    let mb = a.mailbox.cast::<HewMailbox>();

    let result = if reply_channel.is_null() {
        // SAFETY: Mailbox is valid for the actor's lifetime; data/size from caller.
        unsafe { mailbox::hew_mailbox_send(mb, msg_type, data, size) }
    } else {
        // SAFETY: Mailbox is valid for the actor's lifetime; reply_channel is non-null and valid.
        unsafe { mailbox::hew_mailbox_send_with_reply(mb, msg_type, data, size, reply_channel) }
    };
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
// Native asks block on threaded reply channels; WASM asks cooperate by
// driving the single-threaded scheduler in bounded ticks.

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
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let ch = reply_channel::hew_reply_channel_new();

    // Retain a sender-side reference — the receiver will call hew_reply
    // which releases one reference, and we release ours after waiting.
    // SAFETY: ch was created by hew_reply_channel_new.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };

    // Send the message with the reply channel in the HewMsgNode field
    // (not packed in the data buffer).
    // SAFETY: actor is valid, data is valid for size bytes.
    let send_result =
        unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast()) };

    if send_result != HewError::Ok as i32 {
        // Release both references (sender + ours).
        // SAFETY: ch was created by hew_reply_channel_new; releasing the send-side ref.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        // SAFETY: ch was created by hew_reply_channel_new; releasing our retained ref.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return actor_ask_null(send_err_to_ask_err(send_result));
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait(ch) };

    if result.is_null() {
        // Distinguish an orphaned ask (mailbox teardown before reply) from a
        // legitimate null reply deposited by the handler.
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_orphaned = unsafe { (*ch).orphaned.load(Ordering::Acquire) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        // Legitimate null reply — clear any stale error.
        actor_ask_clear();
    } else {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }

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
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_timeout(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: i32,
) -> *mut c_void {
    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };
    // SAFETY: actor is valid, data is valid for size bytes.
    let send_result =
        unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast()) };

    if send_result != HewError::Ok as i32 {
        // Release both references (sender + ours).
        // SAFETY: ch was created by hew_reply_channel_new; releasing the send-side ref.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        // SAFETY: ch was created by hew_reply_channel_new; releasing our retained ref.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return actor_ask_null(send_err_to_ask_err(send_result));
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait_timeout(ch, timeout_ms) };

    if result.is_null() {
        // Distinguish timeout (channel not ready) from legitimate null reply or orphan.
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_ready = unsafe { reply_channel::hew_reply_channel_is_ready(ch) };
        if !is_ready {
            // Deadline elapsed before any reply arrived.
            // Mark the channel as cancelled so the late replier handles cleanup.
            // SAFETY: ch is still live while the caller-side reference is released.
            unsafe { reply_channel::hew_reply_channel_cancel(ch) };
            // SAFETY: release the caller-side reference after recording cancellation.
            unsafe { reply_channel::hew_reply_channel_free(ch) };
            return actor_ask_null(AskError::Timeout);
        }
        // Channel is ready but value is null — could be orphaned or legitimate.
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_orphaned = unsafe { (*ch).orphaned.load(Ordering::Acquire) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        actor_ask_clear();
    } else {
        // Got a non-null reply — release the caller-side reference.
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }

    result
}

#[cfg(any(target_arch = "wasm32", test))]
const HEW_WASM_ASK_TICK_ACTIVATIONS: i32 = 1;

#[cfg(target_arch = "wasm32")]
#[inline]
fn is_terminal(state: i32) -> bool {
    state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32
}

#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn wake_wasm_actor(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        // SAFETY: actor is valid and the cooperative scheduler is initialized.
        unsafe { crate::scheduler_wasm::hew_wasm_sched_enqueue(actor.cast()) };
    }
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
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_with_channel(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut HewReplyChannel,
) -> i32 {
    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };

    // Send with reply channel in the msg node field.
    // SAFETY: actor is valid, data is valid for size bytes.
    let send_result =
        unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast()) };

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
    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };

    // Look up actor and send with reply channel in the msg node field.
    // Capture the send error code (not just bool) for accurate error discrimination.
    let send_result_code: Option<i32> = {
        let guard = LIVE_ACTORS.lock_or_recover();
        guard.as_ref().and_then(|map| {
            let entry = map.get(&actor_id)?;
            let actor = entry.0;
            if actor.is_null() {
                return None; // actor registered but pointer is null
            }
            // SAFETY: actor and data are valid while LIVE_ACTORS is locked.
            let rc =
                unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast()) };
            Some(rc)
        })
    };

    let send_ok = send_result_code.is_some_and(|rc| rc == HewError::Ok as i32);

    if !send_ok {
        // Release both references (sender + ours).
        // SAFETY: ch was created by hew_reply_channel_new; releasing the send-side ref.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        // SAFETY: ch was created by hew_reply_channel_new; releasing our retained ref.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        let ask_err = send_result_code.map_or(AskError::ActorStopped, send_err_to_ask_err); // None → actor not found
        return actor_ask_null(ask_err);
    }

    let mut reply_size: usize = 0;
    // SAFETY: ch is valid and single-reader; reply_size is a valid stack pointer.
    let result = unsafe { reply_channel::hew_reply_wait_with_size(ch, &raw mut reply_size) };

    // Store the reply size in a thread-local so the caller can retrieve it.
    LAST_REPLY_SIZE.set(reply_size);

    if result.is_null() {
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_orphaned = unsafe { (*ch).orphaned.load(Ordering::Acquire) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        actor_ask_clear();
    } else {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }

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
/// Closes the mailbox and CAS transitions from `Running` to `Stopping`.
/// The WASM scheduler will handle the final transition to `Stopped` after
/// dispatch returns.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn actor_self_stop_wasm_impl(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: caller guarantees `actor` is the currently running actor.
    let a = unsafe { &*actor };

    let mailbox = a.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
    if !mailbox.is_null() {
        // SAFETY: mailbox is valid for the actor's lifetime.
        unsafe { crate::mailbox_wasm::hew_mailbox_close(mailbox) };
    }

    // CAS Running → Stopping.
    let _ = a.actor_state.compare_exchange(
        HewActorState::Running as i32,
        HewActorState::Stopping as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_actor_self_stop() {
    // SAFETY: WASM is single-threaded.
    let actor = unsafe { CURRENT_ACTOR_WASM };
    // SAFETY: CURRENT_ACTOR_WASM is only set during dispatch.
    unsafe { actor_self_stop_wasm_impl(actor) };
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
    fn hew_mailbox_send_sys(mb: *mut c_void, msg_type: i32, data: *mut c_void, size: usize);
    fn hew_mailbox_close(mb: *mut c_void);
    fn hew_wasm_sched_enqueue(actor: *mut c_void);
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
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }
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
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }
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
    if !opts.init_state.is_null() && opts.state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Trusted FFI constructor; capacity/policy were derived from opts above.
        unsafe { hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Trusted FFI constructor for an unbounded mailbox.
        unsafe { hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox is a valid WASM mailbox pointer created above.
    unsafe {
        crate::mailbox_wasm::hew_mailbox_set_coalesce_config(
            mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>(),
            opts.coalesce_key_fn,
            coalesce_fallback,
        );
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

/// Shared WASM send-with-channel primitive for ask/select lowering.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask_with_channel`].
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn ask_with_channel_wasm_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { crate::reply_channel_wasm::hew_reply_channel_retain(ch.cast()) };

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // SAFETY: a.mailbox is a valid mailbox pointer; ch is a valid reply channel.
    let mut send_result = unsafe {
        crate::mailbox_wasm::hew_mailbox_send_with_reply(a.mailbox.cast(), msg_type, data, size, ch)
    };
    if send_result == HewError::ErrClosed as i32 {
        send_result = HewError::ErrActorStopped as i32;
    }
    if send_result != HewError::Ok as i32 {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { crate::reply_channel_wasm::hew_reply_channel_free(ch.cast()) };
        return send_result;
    }

    // SAFETY: actor is valid and owned by the runtime.
    unsafe { wake_wasm_actor(actor) };

    HewError::Ok as i32
}

#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn actor_ask_wasm_impl(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: Option<i32>,
) -> *mut c_void {
    use crate::reply_channel_wasm;

    let ch = reply_channel_wasm::hew_reply_channel_new();

    // SAFETY: ch is a live reply channel and `actor`/`data` follow the
    // same preconditions as this shared ask implementation.
    let send_result =
        unsafe { ask_with_channel_wasm_internal(actor, msg_type, data, size, ch.cast()) };
    if send_result != HewError::Ok as i32 {
        // SAFETY: ch was created by hew_reply_channel_new and the failed send
        // already released the queued sender-side retain.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        return actor_ask_null(send_err_to_ask_err(send_result));
    }

    let deadline = timeout_ms.map(|ms| {
        std::time::Instant::now()
            + std::time::Duration::from_millis(u64::try_from(ms.max(0)).unwrap_or(0))
    });

    loop {
        // SAFETY: ch stays live until we release the caller-side reference below.
        if unsafe { reply_channel_wasm::reply_ready(ch) } {
            break;
        }

        if deadline.is_some_and(|limit| std::time::Instant::now() >= limit) {
            // Timeout: mark the channel as cancelled so any later replier or
            // queued-message cleanup releases the sender-side reference.
            // SAFETY: ch is still live while we release our caller-side ref.
            unsafe { reply_channel_wasm::hew_reply_channel_cancel(ch) };
            // SAFETY: release the caller-side reference after recording cancellation.
            unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
            return actor_ask_null(AskError::Timeout);
        }

        // SAFETY: scheduler must be initialized by the runtime/host.
        let remaining = unsafe { crate::bridge::hew_wasm_tick(HEW_WASM_ASK_TICK_ACTIVATIONS) };

        if deadline.is_some_and(|limit| std::time::Instant::now() >= limit) {
            // The tick may have run a blocking dispatch (for example, the
            // current WASM sleep shim). Treat replies that materialize after
            // the deadline as timed out and free any buffered payload.
            // SAFETY: ch remains live until we release the caller-side ref below.
            unsafe { reply_channel_wasm::hew_reply_channel_cancel(ch) };
            // SAFETY: release the caller-side reference after recording cancellation.
            unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
            return actor_ask_null(AskError::Timeout);
        }

        // SAFETY: ch stays live until we release the caller-side reference below.
        if unsafe { reply_channel_wasm::reply_ready(ch) } {
            break;
        }

        if remaining == 0 {
            // No runnable work remains. Cancel the channel for both bounded and
            // unbounded asks so any later replier skips allocating reply data.
            // SAFETY: ch remains live until the caller-side reference is released below.
            unsafe { reply_channel_wasm::hew_reply_channel_cancel(ch) };
            // SAFETY: release the caller-side reference before returning without a reply.
            unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
            // Distinguish between a stopped actor (orphaned ask) and genuinely
            // no runnable work (the actor is alive but idle with nothing to run).
            // SAFETY: actor is valid for the duration of this call (caller guarantee).
            let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
            if actor_state == HewActorState::Stopped as i32
                || actor_state == HewActorState::Crashed as i32
            {
                return actor_ask_null(AskError::OrphanedAsk);
            }
            return actor_ask_null(AskError::NoRunnableWork);
        }
    }

    // SAFETY: ch is a valid reply channel pointer created above.
    let reply = unsafe { reply_channel_wasm::reply_take(ch) };

    if reply.is_null() {
        // Distinguish an orphaned ask (mailbox teardown retired the channel via
        // `retire_reply_channel`) from a legitimate null reply deposited by the
        // handler.  `retire_reply_channel` sets `orphaned = true` before calling
        // `hew_reply`; a handler that explicitly replies null does NOT set it.
        // This is the sole discriminant — actor terminal state is intentionally
        // NOT used here because a handler can legitimately call
        //   hew_reply(ch, NULL, 0); hew_actor_self_stop();
        // in the same dispatch, producing a null reply with a terminal actor.
        // SAFETY: ch is still live — we release it immediately below.
        let is_orphaned = unsafe { reply_channel_wasm::reply_is_orphaned(ch) };
        // SAFETY: ch was created by hew_reply_channel_new and is no longer needed.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        actor_ask_clear();
    } else {
        // SAFETY: ch was created by hew_reply_channel_new and is no longer needed.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }
    reply
}

/// Send a message with a caller-provided reply channel (WASM).
///
/// Mirrors the native send-with-channel contract for `select.add`: retain
/// the caller-provided reply channel for the queued send, wake an idle actor,
/// and return a status code without waiting for a reply.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask_with_channel`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_with_channel(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    // SAFETY: same preconditions as ask_with_channel_wasm_internal.
    unsafe { ask_with_channel_wasm_internal(actor, msg_type, data, size, ch) }
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
    // SAFETY: same preconditions as actor_ask_wasm_impl.
    unsafe { actor_ask_wasm_impl(actor, msg_type, data, size, None) }
}

/// Cooperative ask with timeout: send a request and drive the scheduler in
/// bounded ticks until the reply arrives or the timeout expires (WASM).
///
/// Returns the reply value, or null on timeout / when no runnable work
/// remains that can satisfy the ask before control returns to the host.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask_timeout`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_timeout(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: i32,
) -> *mut c_void {
    // SAFETY: same preconditions as actor_ask_wasm_impl.
    unsafe { actor_ask_wasm_impl(actor, msg_type, data, size, Some(timeout_ms)) }
}

/// Cooperative await: pump the scheduler until the actor reaches a terminal
/// state (WASM).
///
/// Returns the actor error code (0 for clean stop, non-zero for crash).
/// Returns `-1` for null actor pointers.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await(actor: *mut HewActor) -> i32 {
    if actor.is_null() {
        return -1;
    }

    // SAFETY: caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    if is_terminal(a.actor_state.load(Ordering::Acquire)) {
        return a.error_code.load(Ordering::Acquire);
    }

    loop {
        // SAFETY: scheduler must be initialized by the runtime/host.
        let remaining = unsafe { crate::bridge::hew_wasm_tick(HEW_WASM_ASK_TICK_ACTIVATIONS) };
        if is_terminal(a.actor_state.load(Ordering::Acquire)) {
            return a.error_code.load(Ordering::Acquire);
        }
        if remaining == 0 {
            return HewError::ErrTimeout as i32;
        }
    }
}

/// Cooperative await-all: wait for all provided actors to reach terminal
/// states by pumping the WASM scheduler.
///
/// Returns `0` if every actor stopped normally, or the first non-zero
/// error code encountered. Returns `-1` on null/invalid arguments.
///
/// # Safety
///
/// - `actors` must point to an array of at least `count` valid
///   `*mut HewActor` pointers (null entries are skipped).
/// - `count` must be non-negative.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await_all(actors: *const *mut HewActor, count: i64) -> i32 {
    if actors.is_null() || count < 0 {
        return -1;
    }

    let mut first_error = 0;
    #[expect(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        reason = "count >= 0 checked above; practical array sizes fit in usize"
    )]
    for i in 0..count as usize {
        // SAFETY: caller guarantees the array is valid for `count` elements.
        let actor = unsafe { *actors.add(i) };
        if actor.is_null() {
            continue;
        }
        // SAFETY: actor pointer validity follows the caller contract.
        let rc = unsafe { hew_actor_await(actor) };
        if first_error == 0 && rc != 0 {
            first_error = rc;
        }
    }
    first_error
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
    let a = unsafe { &*actor };
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe { hew_mailbox_close(a.mailbox) };
    }

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
        return;
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if state != HewActorState::Running as i32 {
        return;
    }

    // Running actors are already inside a dispatch; enqueue one sys message so
    // the next mailbox poll observes the shutdown request. Runnable actors
    // already have queued work and do not need an extra wake-up signal.
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe {
            let _ = crate::mailbox_wasm::mailbox_send_stop_sys_once(a.mailbox.cast());
        }
    }
}

/// Free an actor and all associated resources (WASM).
///
/// Waits until the actor is quiescent (`Stopped`, `Crashed`, or `Idle`)
/// before untracking and freeing it, mirroring the native free contract.
///
/// # Safety
///
/// - `actor` must have been returned by a spawn function.
/// - The actor must not be used after this call.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn actor_free_wasm_impl(actor: *mut HewActor) -> c_int {
    if actor.is_null() {
        crate::set_last_error("hew_actor_free: null actor pointer");
        return -1;
    }

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    loop {
        let state = a.actor_state.load(Ordering::Acquire);
        if actor_free_state_is_quiescent(state) {
            break;
        }
        if std::time::Instant::now() >= deadline {
            break;
        }
        #[cfg(target_arch = "wasm32")]
        std::hint::spin_loop();
        #[cfg(not(target_arch = "wasm32"))]
        std::thread::yield_now();
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if !actor_free_state_is_quiescent(state) {
        return -2;
    }

    if !untrack_actor(actor) {
        crate::set_last_error("hew_actor_free: actor already freed or not tracked");
        return -1;
    }

    if state != HewActorState::Crashed as i32 {
        // SAFETY: the wait loop above ensures the actor is not being dispatched.
        unsafe { call_terminate_fn(actor) };
    }

    // SAFETY: Caller guarantees `actor` is valid and not being dispatched.
    unsafe { free_actor_resources_wasm(actor) };
    0
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) -> c_int {
    // SAFETY: same preconditions as actor_free_wasm_impl.
    unsafe { actor_free_wasm_impl(actor) }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    static LAST_NATIVE_ASK_REPLY_CHANNEL: AtomicPtr<reply_channel::HewReplyChannel> =
        AtomicPtr::new(ptr::null_mut());

    struct NativeSchedulerGuard;

    impl NativeSchedulerGuard {
        fn new() -> Self {
            assert_eq!(crate::scheduler::hew_sched_init(), 0);
            Self
        }
    }

    impl Drop for NativeSchedulerGuard {
        fn drop(&mut self) {
            crate::scheduler::hew_sched_shutdown();
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
    }

    fn wait_for_condition(
        timeout: std::time::Duration,
        mut condition: impl FnMut() -> bool,
    ) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        while std::time::Instant::now() < deadline {
            if condition() {
                return true;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        condition()
    }

    fn defer_state_transition(
        actor: *mut HewActor,
        target_state: HewActorState,
        delay: std::time::Duration,
    ) -> std::thread::JoinHandle<()> {
        let actor_addr = actor as usize;
        std::thread::spawn(move || {
            std::thread::sleep(delay);
            // SAFETY: the test keeps the actor allocation alive until the
            // background transition fires.
            unsafe {
                (*(actor_addr as *mut HewActor))
                    .actor_state
                    .store(target_state as i32, Ordering::Release);
            }
        })
    }

    unsafe extern "C" fn native_self_stop_without_reply_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let ch = crate::scheduler::hew_get_reply_channel().cast::<reply_channel::HewReplyChannel>();
        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ch, Ordering::Release);
        hew_actor_self_stop();
    }

    unsafe extern "C" fn native_reply_once_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return;
        }
        let mut value: i32 = 21;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }
    }

    unsafe extern "C" fn native_late_reply_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        std::thread::sleep(std::time::Duration::from_millis(20));
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return;
        }
        let mut value: i32 = 99;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }
    }

    unsafe extern "C" fn native_reply_then_trap_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return;
        }
        let mut value: i32 = 123;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }
        hew_panic();
    }

    fn make_stop_test_actor(initial_state: HewActorState) -> (*mut HewActor, *mut HewMailbox) {
        // SAFETY: test helper fully owns the returned actor/mailbox and never publishes them.
        unsafe {
            let mailbox = mailbox::hew_mailbox_new();
            assert!(!mailbox.is_null());
            let actor = Box::into_raw(Box::new(HewActor {
                sched_link_next: AtomicPtr::new(ptr::null_mut()),
                id: 1,
                pid: 0,
                state: ptr::null_mut(),
                state_size: 0,
                dispatch: Some(noop_dispatch),
                mailbox: mailbox.cast(),
                actor_state: AtomicI32::new(initial_state as i32),
                budget: AtomicI32::new(HEW_MSG_BUDGET),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                coalesce_key_fn: None,
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
            }));
            (actor, mailbox)
        }
    }

    fn make_tracked_wasm_free_test_actor(initial_state: HewActorState) -> *mut HewActor {
        let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
        let actor = Box::into_raw(Box::new(HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: actor_id,
            pid: actor_id,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(initial_state as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
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
        }));
        track_actor(actor);
        actor
    }

    #[test]
    fn ask_with_channel_send_failure_returns_error() {
        let _guard = crate::runtime_test_guard();
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
    fn native_ask_self_stop_without_reply_returns_null_and_releases_channel() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);
        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ptr::null_mut(), Ordering::Release);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_self_stop_without_reply_dispatch),
            )
        };
        assert!(!actor.is_null());

        let actor_addr = actor as usize;
        let (tx, rx) = std::sync::mpsc::channel();
        let ask_thread = std::thread::spawn(move || {
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was spawned by this test and remains live until the thread joins.
            let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
            let reply_is_null = reply.is_null();
            if !reply.is_null() {
                // SAFETY: successful ask replies are malloc-allocated.
                unsafe { libc::free(reply) };
            }
            tx.send(reply_is_null)
                .expect("native ask waiter should report its result");
        });

        let reply_is_null = match rx.recv_timeout(std::time::Duration::from_secs(1)) {
            Ok(reply_is_null) => reply_is_null,
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                let ch = LAST_NATIVE_ASK_REPLY_CHANNEL.swap(ptr::null_mut(), Ordering::AcqRel);
                if !ch.is_null() {
                    // SAFETY: this is the captured in-flight reply channel from the stalled ask.
                    unsafe { crate::reply_channel::hew_reply(ch, ptr::null_mut(), 0) };
                }
                let recovered = rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("manual fallback reply should unblock a stalled self-stop ask");
                ask_thread
                    .join()
                    .expect("native ask waiter thread should not panic after cleanup");
                assert!(
                    recovered,
                    "manual fallback reply should still resolve self-stop asks as null"
                );
                panic!("native hew_actor_ask should resolve null after self-stop without manual cleanup");
            }
            Err(err) => panic!("native ask waiter thread disconnected unexpectedly: {err:?}"),
        };

        ask_thread
            .join()
            .expect("native ask waiter thread should not panic");

        assert!(
            reply_is_null,
            "ask should resolve as null when the actor self-stops before replying"
        );
        // SAFETY: `actor` remains allocated and owned by this test while we
        // inspect its atomic state.
        let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert!(
            actor_state == HewActorState::Stopping as i32
                || actor_state == HewActorState::Stopped as i32,
            "self-stop ask should leave the actor in teardown, got state {actor_state}"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                // SAFETY: `actor` remains allocated and owned by this test while
                // we poll its atomic state.
                unsafe {
                    (*actor).actor_state.load(Ordering::Acquire) == HewActorState::Stopped as i32
                }
            }),
            "self-stop ask should eventually drive the actor to Stopped"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "self-stop ask cleanup should release the native reply channel",
        );

        // SAFETY: actor is stopped and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_successful_reply_returns_value_without_duplicate_cleanup() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the ask.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(!reply.is_null(), "native ask should return the reply value");
        // SAFETY: non-null asks return a malloc-allocated i32 payload here.
        assert_eq!(unsafe { *reply.cast::<i32>() }, 21);
        // SAFETY: successful ask replies are malloc-allocated.
        unsafe { libc::free(reply) };

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "successful native asks should leave no live reply channels",
        );

        // SAFETY: actor is idle and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_timeout_rejects_late_reply_after_blocking_dispatch() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_late_reply_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the timed ask.
        let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 1) };
        assert!(
            reply.is_null(),
            "timed native asks should reject replies that only arrive after the timeout"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "timed-out native asks should release late-reply channels after cancellation",
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                // SAFETY: actor remains owned by this test while waiting for dispatch to finish.
                let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
                state == HewActorState::Idle as i32 || state == HewActorState::Stopped as i32
            }),
            "late-reply dispatch should finish after the timeout path",
        );

        // SAFETY: actor is quiescent and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_reply_then_trap_returns_value_without_duplicate_crash_reply() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_reply_then_trap_dispatch),
            )
        };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the ask.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(
            !reply.is_null(),
            "asks should preserve the first reply even if dispatch traps afterwards"
        );
        // SAFETY: non-null asks return a malloc-allocated i32 payload here.
        assert_eq!(unsafe { *reply.cast::<i32>() }, 123);
        // SAFETY: successful ask replies are malloc-allocated.
        unsafe { libc::free(reply) };

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                // SAFETY: actor remains owned by this test while we poll its state.
                let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
                state == HewActorState::Crashed as i32
            }),
            "reply-then-trap dispatch should still transition the actor to Crashed",
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "trap-after-reply asks should not double-complete or leak reply channels",
        );

        // SAFETY: actor is quiescent and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    // ── ask error discrimination tests ───────────────────────────────────

    /// `hew_actor_ask` on a stopped actor sets `ActorStopped` in the error slot.
    #[test]
    fn native_ask_stopped_actor_sets_actor_stopped_error() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is valid.
        unsafe { hew_actor_stop(actor) };

        // Reset error slot, then attempt ask.
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is stopped but pointer remains valid.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(reply.is_null(), "ask on stopped actor must return null");
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::ActorStopped as i32,
            "stopped actor must report ActorStopped error"
        );

        // SAFETY: actor is stopped and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// `hew_actor_ask_timeout` on a stopped actor sets `ActorStopped`.
    #[test]
    fn native_ask_timeout_stopped_actor_sets_actor_stopped_error() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is live and single-owner; stopping it to force send failure.
        unsafe { hew_actor_stop(actor) };

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is stopped.
        let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 50) };
        assert!(reply.is_null());
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::ActorStopped as i32,
            "send failure on stopped actor must report ActorStopped"
        );
        // SAFETY: actor was stopped above; no asks are pending.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// `hew_actor_ask_timeout` fires `Timeout` when the handler does not reply in time.
    #[test]
    fn native_ask_timeout_sets_timeout_error() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        // SAFETY: null state + valid dispatch.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_late_reply_dispatch)) };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is valid; 1 ms deadline is too short for the 20 ms handler.
        let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 1) };
        assert!(reply.is_null(), "ask must time out");
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::Timeout as i32,
            "timed-out ask must report Timeout"
        );

        // Let the late-reply dispatch finish and free the actor cleanly.
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "late-reply channel must be released after cancellation",
        );
        // SAFETY: actor was spawned above and all channels are drained.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    /// `hew_actor_ask` when the actor self-stops without replying sets `OrphanedAsk`.
    #[test]
    fn native_ask_orphaned_sets_orphaned_ask_error() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ptr::null_mut(), Ordering::Release);
        // SAFETY: null state + valid dispatch.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_self_stop_without_reply_dispatch),
            )
        };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));

        let actor_addr = actor as usize;
        let (tx, rx) = std::sync::mpsc::channel();
        let handle = std::thread::spawn(move || {
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was spawned above and remains live until the thread joins.
            let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
            let is_null = reply.is_null();
            if !reply.is_null() {
                // SAFETY: reply was allocated by the runtime and ownership transfers to caller.
                unsafe { libc::free(reply) };
            }
            let err = hew_actor_ask_take_last_error();
            tx.send((is_null, err)).expect("sender should be live");
        });

        let (is_null, err) = if let Ok(v) = rx.recv_timeout(std::time::Duration::from_secs(2)) {
            v
        } else {
            // Fallback: manually unblock a stalled ask (test environment artefact).
            let ch = LAST_NATIVE_ASK_REPLY_CHANNEL.swap(ptr::null_mut(), Ordering::AcqRel);
            if !ch.is_null() {
                // SAFETY: ch was retrieved from the atomic; hew_reply takes ownership.
                unsafe { crate::reply_channel::hew_reply(ch, ptr::null_mut(), 0) };
            }
            rx.recv_timeout(std::time::Duration::from_secs(1))
                .expect("fallback reply should unblock ask")
        };
        handle.join().expect("ask thread must not panic");

        assert!(is_null, "orphaned ask must return null");
        assert_eq!(
            err,
            AskError::OrphanedAsk as i32,
            "orphaned ask must report OrphanedAsk"
        );

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "orphaned ask must release its reply channel"
        );
        // SAFETY: actor has self-stopped; all channels are released.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    /// Successful ask clears the error slot.
    #[test]
    fn native_ask_success_clears_error_slot() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        // SAFETY: null state + valid dispatch.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
        assert!(!actor.is_null());

        // Poison slot, then succeed.
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
        // SAFETY: actor is valid.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(!reply.is_null(), "ask must succeed");
        // SAFETY: non-null reply is malloc-allocated.
        unsafe { libc::free(reply) };
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::None as i32,
            "successful ask must clear the error slot"
        );

        // SAFETY: actor is live; ask has returned and no pending channels remain.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    /// `hew_actor_ask_take_last_error` resets the slot to None after reading.
    #[test]
    fn actor_ask_take_last_error_resets_slot() {
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
        let first = hew_actor_ask_take_last_error();
        let second = hew_actor_ask_take_last_error();
        assert_eq!(
            first,
            AskError::Timeout as i32,
            "first take must return Timeout"
        );
        assert_eq!(
            second,
            AskError::None as i32,
            "second take must return None"
        );
    }

    // ── MailboxFull / NoRunnableWork discrimination (native) ─────────────

    /// `hew_actor_ask` on a bounded mailbox that is at capacity returns `MailboxFull`.
    ///
    /// The send inside the ask sees a full mailbox (capacity = 1, one pre-queued
    /// message) and returns `ErrMailboxFull` before the ask-wait loop is entered.
    ///
    /// The pre-fill is done by calling `hew_mailbox_send` directly on the mailbox
    /// pointer.  This bypasses `actor_send_result_internal_reply` (and therefore
    /// `sched_enqueue`) intentionally: we want the message to sit in the mailbox
    /// without the actor being scheduled, so the slot is still occupied when the
    /// ask executes.  The actor remains in the `Idle` state throughout, which lets
    /// `hew_actor_stop` CAS it directly to `Stopped` for clean teardown — no
    /// scheduler is required.
    #[test]
    fn native_ask_bounded_mailbox_full_sets_mailbox_full_error() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn_bounded(ptr::null_mut(), 0, Some(noop_dispatch), 1) };
        assert!(!actor.is_null());

        // Directly enqueue one message into the mailbox, bypassing the actor-state
        // transition and scheduler enqueue.  The actor stays Idle; the mailbox now
        // has count=1 == capacity=1.
        // SAFETY: actor is valid; mailbox pointer is valid for the actor's lifetime.
        let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };
        // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
        // The null data pointer is intentional — the message slot just needs to exist.
        let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
        assert_eq!(
            pre_fill,
            HewError::Ok as i32,
            "pre-fill into empty bounded mailbox must succeed"
        );

        // Reset the error slot, then ask. The send inside the ask hits the full
        // mailbox and returns ErrMailboxFull immediately — the ask-wait loop is
        // never entered.
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is valid; the ask send will fail with MailboxFull.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(
            reply.is_null(),
            "ask into full bounded mailbox must return null"
        );
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::MailboxFull as i32,
            "full bounded mailbox must report MailboxFull"
        );

        // Actor is still Idle (no state transition occurred during pre-fill).
        // hew_actor_stop CAS Idle → Stopped succeeds; no scheduler needed.
        // SAFETY: actor is valid; closing a live actor's mailbox is safe.
        unsafe { hew_actor_stop(actor) };
        // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
        // pre-filled message during free_actor_resources.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// Bounded-mailbox actor that self-stops without replying sets `OrphanedAsk`,
    /// not `MailboxFull`: the mailbox has room for the ask message, so the
    /// discriminant is the orphaned reply channel, not a send failure.
    #[test]
    fn native_ask_bounded_actor_orphan_sets_orphaned_ask_error() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ptr::null_mut(), Ordering::Release);
        // capacity=8: plenty of room for the ask message, so the send succeeds
        // and the discriminant is the orphaned reply channel.
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe {
            hew_actor_spawn_bounded(
                ptr::null_mut(),
                0,
                Some(native_self_stop_without_reply_dispatch),
                8,
            )
        };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));

        let actor_addr = actor as usize;
        let (tx, rx) = std::sync::mpsc::channel();
        let handle = std::thread::spawn(move || {
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was spawned above and remains live until the thread joins.
            let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
            let is_null = reply.is_null();
            if !reply.is_null() {
                // SAFETY: reply was allocated by the runtime and ownership transfers to caller.
                unsafe { libc::free(reply) };
            }
            let err = hew_actor_ask_take_last_error();
            tx.send((is_null, err)).expect("sender should be live");
        });

        let (is_null, err) = if let Ok(v) = rx.recv_timeout(std::time::Duration::from_secs(2)) {
            v
        } else {
            let ch = LAST_NATIVE_ASK_REPLY_CHANNEL.swap(ptr::null_mut(), Ordering::AcqRel);
            if !ch.is_null() {
                // SAFETY: ch was retrieved from the atomic; hew_reply takes ownership.
                unsafe { crate::reply_channel::hew_reply(ch, ptr::null_mut(), 0) };
            }
            rx.recv_timeout(std::time::Duration::from_secs(1))
                .expect("fallback reply should unblock ask")
        };
        handle.join().expect("ask thread must not panic");

        assert!(is_null, "bounded-actor orphaned ask must return null");
        assert_eq!(
            err,
            AskError::OrphanedAsk as i32,
            "bounded-actor orphaned ask must report OrphanedAsk, not MailboxFull"
        );

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "orphaned ask on bounded actor must release its reply channel"
        );
        // SAFETY: actor has self-stopped; all channels are released.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    #[test]
    fn stop_idle_actor_is_idempotent_and_queues_no_shutdown_sys_messages() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Spawning with null state and a valid dispatch function.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            let mb = (*actor).mailbox.cast::<HewMailbox>();
            assert_eq!(mailbox::hew_mailbox_sys_len(mb), 0);

            hew_actor_stop(actor);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Stopped as i32
            );
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mb),
                0,
                "stopping an idle actor should not enqueue an unprocessable shutdown signal"
            );

            hew_actor_stop(actor);
            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mb),
                0,
                "repeated stop calls on a stopped actor must not accumulate system messages"
            );

            assert_eq!(hew_actor_free(actor), 0);
        }
    }

    #[test]
    fn stop_runnable_actor_does_not_enqueue_shutdown_signal() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_stop(actor);
            hew_actor_stop(actor);
            assert!(
                mailbox::mailbox_is_closed(mailbox),
                "stop must close runnable actors before they drain their queued activation"
            );
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                0,
                "runnable actors already have a queued activation and should not receive an extra shutdown signal"
            );
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn close_then_stop_runnable_actor_keeps_shutdown_queue_empty() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Runnable as i32,
                "close should leave runnable actors runnable while only closing the mailbox"
            );
            assert!(
                mailbox::mailbox_is_closed(mailbox),
                "close must mark the mailbox closed before stop is requested"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                0,
                "stop after close should not enqueue a redundant shutdown signal for runnable actors"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                0,
                "repeated stop after close must keep runnable actors' shutdown queue empty"
            );

            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn stop_running_actor_enqueues_at_most_one_shutdown_signal() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_stop(actor);
            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                1,
                "only the first stop call should enqueue a shutdown system message for a running actor"
            );
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn close_then_stop_running_actor_enqueues_shutdown_signal_once() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Running as i32,
                "close should leave running actors running while only closing the mailbox"
            );
            assert!(
                mailbox::mailbox_is_closed(mailbox),
                "close must mark the mailbox closed before stop is requested"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                1,
                "stop after close must still enqueue one shutdown system message for a running actor"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                1,
                "repeated stop after close must not accumulate shutdown system messages for a running actor"
            );

            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn free_actor_resources_completes_when_terminate_finishes_quickly() {
        let _guard = crate::runtime_test_guard();
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
    fn free_current_actor_from_dispatch_is_deferred() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test fully owns the spawned actor and only mutates its
        // fields while no other runtime threads can access it.
        unsafe {
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            (*actor)
                .actor_state
                .store(HewActorState::Stopping as i32, Ordering::Release);

            let prev_actor = set_current_actor(actor);
            let unblock = defer_state_transition(
                actor,
                HewActorState::Stopped,
                std::time::Duration::from_millis(200),
            );

            let start = std::time::Instant::now();
            let rc = hew_actor_free(actor);
            let elapsed = start.elapsed();

            unblock.join().unwrap();
            set_current_actor(prev_actor);

            let freed =
                wait_for_condition(std::time::Duration::from_secs(2), || !is_actor_live(actor));
            if !freed && is_actor_live(actor) {
                (*actor)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(hew_actor_free(actor), 0);
            }

            assert_eq!(
                rc, 0,
                "current-thread frees should defer instead of timing out"
            );
            assert!(
                elapsed < std::time::Duration::from_millis(100),
                "current-thread free should return immediately instead of waiting for dispatch teardown, took {elapsed:?}"
            );
            assert!(
                freed,
                "actor should be freed asynchronously after dispatch unwinds"
            );
        }
    }

    #[test]
    fn deep_copy_state_copies_data_correctly() {
        let src: [u8; 4] = [0xDE, 0xAD, 0xBE, 0xEF];
        // SAFETY: src is a valid 4-byte buffer.
        let dst = unsafe { deep_copy_state(src.as_ptr().cast_mut().cast(), 4) };
        assert!(!dst.is_null());
        // SAFETY: dst is a freshly-allocated 4-byte buffer.
        let copied = unsafe { std::slice::from_raw_parts(dst.cast::<u8>(), 4) };
        assert_eq!(copied, &src);
        // SAFETY: dst was allocated with libc::malloc.
        unsafe { libc::free(dst) };
    }

    #[test]
    fn deep_copy_state_null_source_returns_null() {
        // SAFETY: null source is explicitly handled.
        let dst = unsafe { deep_copy_state(ptr::null_mut(), 64) };
        assert!(dst.is_null());
        // No error should be set for a legitimate null/zero call.
        assert!(crate::hew_last_error().is_null());
    }

    #[test]
    fn deep_copy_state_zero_size_returns_null() {
        let src: u8 = 42;
        // SAFETY: src is valid; size=0 triggers the early return.
        let dst = unsafe { deep_copy_state(std::ptr::from_ref(&src).cast_mut().cast(), 0) };
        assert!(dst.is_null());
    }

    #[test]
    fn deep_copy_state_alloc_failure_returns_null_and_sets_error() {
        let _guard = crate::runtime_test_guard();
        let src: u8 = 1;
        crate::hew_clear_error();
        let _guard = fail_actor_state_alloc_on_nth(0);
        // SAFETY: src is valid; allocation failure is injected by the test.
        let dst = unsafe { deep_copy_state(std::ptr::from_ref(&src).cast_mut().cast(), 1) };
        assert!(dst.is_null(), "should return null on allocation failure");
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "hew_last_error should be set after OOM");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert!(
            msg.contains("OOM"),
            "error message should mention OOM, got: {msg}"
        );
    }

    #[test]
    fn free_actor_resources_times_out_on_hanging_terminate() {
        let _guard = crate::runtime_test_guard();
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

    #[test]
    fn free_current_actor_from_terminate_is_deferred() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test fully owns the spawned actor and simulates the
        // terminate callback state on the current thread.
        unsafe {
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            let a = &*actor;
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            a.terminate_called.store(true, Ordering::Release);
            a.terminate_finished.store(false, Ordering::Release);

            let prev_actor = set_current_actor(actor);

            let start = std::time::Instant::now();
            let rc = hew_actor_free(actor);
            let elapsed = start.elapsed();

            a.terminate_finished.store(true, Ordering::Release);
            set_current_actor(prev_actor);

            let _ = wait_for_condition(std::time::Duration::from_secs(2), || !is_actor_live(actor));

            assert_eq!(rc, 0, "reentrant terminate frees should still succeed");
            assert!(
                elapsed < std::time::Duration::from_secs(1),
                "reentrant free should defer instead of spin-waiting in terminate, took {elapsed:?}"
            );
        }
    }

    #[test]
    fn wasm_free_waits_for_quiescent_actor_state_before_freeing() {
        let _guard = crate::runtime_test_guard();
        let actor = make_tracked_wasm_free_test_actor(HewActorState::Runnable);

        let start = std::time::Instant::now();
        // SAFETY: actor is tracked and owned by this test.
        let rc = unsafe { actor_free_wasm_impl(actor) };
        let elapsed = start.elapsed();

        assert_eq!(rc, -2, "runnable WASM actors must not be freed immediately");
        assert!(
            elapsed >= std::time::Duration::from_secs(1),
            "WASM free should wait for quiescence before timing out, took {elapsed:?}"
        );
        assert!(
            is_actor_live(actor),
            "timed-out WASM free must leave the actor tracked to avoid dangling scheduler pointers"
        );

        // SAFETY: actor remains tracked after the timed-out free attempt.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            assert_eq!(actor_free_wasm_impl(actor), 0);
        }
    }

    #[test]
    fn wasm_free_reports_null_actor_failure_like_native_free() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();

        // SAFETY: null actor pointer is explicitly rejected by the free path.
        let rc = unsafe { actor_free_wasm_impl(ptr::null_mut()) };

        assert_eq!(
            rc, -1,
            "WASM free should mirror native null-pointer failure"
        );
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "WASM free should populate hew_last_error");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert_eq!(msg, "hew_actor_free: null actor pointer");
    }

    #[test]
    fn wasm_free_reports_untracked_actor_failure_like_native_free() {
        let _guard = crate::runtime_test_guard();
        let actor = make_tracked_wasm_free_test_actor(HewActorState::Stopped);
        assert!(
            untrack_actor(actor),
            "test precondition: actor should start tracked"
        );
        crate::hew_clear_error();

        // SAFETY: actor remains allocated and owned by this test.
        let rc = unsafe { actor_free_wasm_impl(actor) };

        assert_eq!(rc, -1, "WASM free should mirror native untrack failure");
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "WASM free should populate hew_last_error");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert_eq!(msg, "hew_actor_free: actor already freed or not tracked");

        // SAFETY: untrack failure must not free the actor; the test still owns it.
        unsafe { drop(Box::from_raw(actor)) };
    }

    /// `actor_free_wasm_impl` must free the actor's arena when it is non-null.
    ///
    /// The existing WASM free tests use actors with `arena: ptr::null_mut()` and
    /// therefore never enter the `if !a.arena.is_null()` branch in
    /// `free_actor_resources_wasm`.  This test constructs an actor with a live
    /// arena (mirroring what `spawn_actor_internal` on WASM does) and verifies:
    ///
    /// 1. `hew_arena_free_all` was called with **this specific arena's address**
    ///    (via `crate::arena::LAST_FREED_ARENA_ADDR`, a thread-local).  The
    ///    assertion fails if the non-null arena branch is accidentally removed.
    /// 2. `actor_free_wasm_impl` returns 0 (success).
    /// 3. The actor is removed from the live-actor set.
    ///
    /// ## Why this is order-independent under parallel test execution
    ///
    /// `LAST_FREED_ARENA_ADDR` is a **thread-local**, not a global counter.
    /// Tests on other threads update their own copy; only the thread executing
    /// this test touches the local that this test reads.  `actor_free_wasm_impl`
    /// is synchronous, so nothing on this thread can overwrite the value between
    /// the call and the assertion.
    #[test]
    fn wasm_free_with_arena_releases_arena_on_teardown() {
        let _guard = crate::runtime_test_guard();

        // Allocate a real arena exactly as spawn_actor_internal (WASM) does.
        let arena = crate::arena::hew_arena_new();
        assert!(!arena.is_null(), "arena allocation must succeed");
        // Capture the address before transferring ownership to the actor struct.
        let arena_addr = arena as usize;

        let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
        let actor = Box::into_raw(Box::new(HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: actor_id,
            pid: actor_id,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Stopped as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
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
            // Wire up the real arena — same assignment as spawn_actor_internal (WASM).
            arena,
        }));
        track_actor(actor);

        // SAFETY: actor is Box-allocated, tracked, in Stopped state, not dispatching.
        // state / init_state are null (libc::free(null) is a no-op), mailbox is null.
        let rc = unsafe { actor_free_wasm_impl(actor) };

        // Primary assertion: hew_arena_free_all must have been called with exactly
        // this actor's arena address.  LAST_FREED_ARENA_ADDR is thread-local so
        // parallel tests on other threads cannot interfere.
        let last_freed = crate::arena::LAST_FREED_ARENA_ADDR.with(std::cell::Cell::get);
        assert_eq!(
            last_freed, arena_addr,
            "free_actor_resources_wasm must call hew_arena_free_all with the actor's own arena"
        );

        assert_eq!(rc, 0, "WASM free with non-null arena must succeed");
        assert!(
            !is_actor_live(actor),
            "freed actor must be removed from the live-actor set"
        );
    }

    #[test]
    fn spawn_with_restart_state_alloc_failure_returns_null_and_sets_error() {
        let _guard = crate::runtime_test_guard();
        let src: u8 = 1;
        crate::hew_clear_error();
        let _guard = fail_actor_state_alloc_on_nth(1);
        // SAFETY: src is valid; allocation failure is injected into the restart-state copy.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::from_ref(&src).cast_mut().cast(),
                1,
                Some(noop_dispatch),
            )
        };
        assert!(actor.is_null(), "spawn should return null on OOM");
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "hew_last_error should be set after OOM");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert!(
            msg.contains("OOM"),
            "error message should mention OOM, got: {msg}"
        );
    }
}

#[cfg(all(test, target_arch = "wasm32"))]
mod wasm_tests {
    use super::*;

    unsafe extern "C" fn self_stop_without_reply_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        hew_actor_self_stop();
    }

    unsafe extern "C" fn reply_once_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let ch = crate::scheduler_wasm::hew_get_reply_channel();
        let mut value: i32 = 21;
        unsafe {
            crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                size_of::<i32>(),
            );
        }
    }

    unsafe extern "C" fn late_reply_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        std::thread::sleep(std::time::Duration::from_millis(20));
        let ch = crate::scheduler_wasm::hew_get_reply_channel();
        let mut value: i32 = 99;
        unsafe {
            crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                size_of::<i32>(),
            );
        }
    }

    /// Dispatch that replies with a null payload and then self-stops in the
    /// same activation.  Used to verify that null-reply + self-stop is NOT
    /// misclassified as an orphaned ask.
    unsafe extern "C" fn null_reply_then_self_stop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let ch = crate::scheduler_wasm::hew_get_reply_channel();
        if !ch.is_null() {
            // SAFETY: ch is the scheduler-installed reply channel; depositing
            // a null payload is a legitimate zero-size reply.
            unsafe {
                crate::reply_channel_wasm::hew_reply(ch.cast(), ptr::null_mut(), 0);
            }
        }
        // Self-stop AFTER the explicit null reply — must NOT set orphaned.
        hew_actor_self_stop();
    }

    #[test]
    fn ask_self_stop_without_reply_returns_null_and_releases_channel() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(self_stop_without_reply_dispatch));
            assert!(!actor.is_null());

            let reply = hew_actor_ask(actor, 1, ptr::null_mut(), 0);
            assert!(
                reply.is_null(),
                "ask should resolve as null when the actor stops before replying"
            );
            assert_eq!(
                (&*actor).actor_state.load(Ordering::Relaxed),
                HewActorState::Stopped as i32
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "ask cleanup should release the sender-side WASM reply-channel ref"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    #[test]
    fn ask_successful_reply_returns_value_without_duplicate_cleanup() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(reply_once_dispatch));
            assert!(!actor.is_null());

            let reply = hew_actor_ask(actor, 1, ptr::null_mut(), 0);
            assert!(!reply.is_null(), "happy-path ask should return a reply");
            assert_eq!(*reply.cast::<i32>(), 21);
            libc::free(reply);

            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "successful asks should leave no live WASM reply channels"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    #[test]
    fn wasm_ask_timeout_rejects_late_reply_after_blocking_tick() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(late_reply_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, Some(1));
            assert!(
                reply.is_null(),
                "timed WASM asks should reject replies that only arrive after the timeout"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::Timeout as i32,
                "timed-out WASM ask must report Timeout"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "timed-out WASM asks should free buffered late replies and reply channels"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    // ── WASM ask error discrimination tests ─────────────────────────────

    /// WASM ask on a stopped actor (send failure) sets `ActorStopped`.
    #[test]
    fn wasm_ask_stopped_actor_sets_actor_stopped_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            // Dispatch function is irrelevant — the actor will be stopped before
            // the ask is submitted, so dispatch is never invoked.
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(self_stop_without_reply_dispatch));
            assert!(!actor.is_null());
            hew_actor_stop(actor);

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(reply.is_null(), "ask on stopped actor must return null");
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::ActorStopped as i32,
                "stopped actor send failure must report ActorStopped"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    /// WASM unbounded ask when actor stops without replying sets `OrphanedAsk`.
    #[test]
    fn wasm_ask_self_stop_sets_orphaned_ask_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(self_stop_without_reply_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(reply.is_null(), "orphaned WASM ask must return null");
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::OrphanedAsk as i32,
                "WASM ask orphaned by actor self-stop must report OrphanedAsk"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    /// WASM ask success clears the error slot.
    #[test]
    fn wasm_ask_success_clears_error_slot() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(reply_once_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(!reply.is_null(), "WASM ask must succeed");
            // SAFETY: reply was allocated by the runtime; caller takes ownership.
            unsafe { libc::free(reply) };
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::None as i32,
                "successful WASM ask must clear error slot"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    /// Regression: `hew_reply(ch, NULL, 0); hew_actor_self_stop()` in the same
    /// dispatch must be treated as a legitimate null reply, NOT as OrphanedAsk.
    ///
    /// The `orphaned` flag is only set by `retire_reply_channel` (called when
    /// the mailbox is torn down WITHOUT a handler reply).  When the handler
    /// explicitly replies — even with null — `orphaned` stays false.
    #[test]
    fn wasm_ask_null_reply_then_self_stop_is_not_orphaned() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            // SAFETY: null state + valid dispatch.
            let actor =
                hew_actor_spawn(ptr::null_mut(), 0, Some(null_reply_then_self_stop_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(
                reply.is_null(),
                "explicit null reply must still be returned as null"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::None as i32,
                "null reply + self-stop must NOT be classified as OrphanedAsk"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "null reply + self-stop must not leak reply channels"
            );

            // SAFETY: actor stopped itself; pointer is still allocated.
            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    // ── MailboxFull / NoRunnableWork discrimination (WASM) ───────────────

    /// Dispatch that does nothing: receives the message but does not reply and
    /// does not self-stop. Used to drive `MailboxFull` and `NoRunnableWork` tests.
    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
    }

    /// `hew_actor_ask` on a bounded WASM mailbox that is at capacity returns
    /// `MailboxFull`.
    ///
    /// WASM is cooperative: the scheduler only runs when ticked, so a pre-queued
    /// message stays in the mailbox until `hew_wasm_tick` is called. The ask send
    /// therefore hits a full mailbox and fails before the scheduler loop is entered.
    #[test]
    fn wasm_ask_bounded_mailbox_full_sets_mailbox_full_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            // Spawn with capacity=1 (default DropNew overflow policy).
            let actor = hew_actor_spawn_bounded(ptr::null_mut(), 0, Some(noop_dispatch), 1);
            assert!(!actor.is_null());

            // Pre-fill the single slot before ticking the scheduler.
            // On WASM the scheduler is cooperative: the actor stays Runnable until
            // we call hew_wasm_tick, so the slot remains occupied.
            hew_actor_send(actor, 1, ptr::null_mut(), 0);

            // The ask send hits the full mailbox and returns ErrMailboxFull before
            // the scheduler loop is entered.
            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(
                reply.is_null(),
                "ask into full bounded WASM mailbox must return null"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::MailboxFull as i32,
                "full bounded WASM mailbox must report MailboxFull"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "failed WASM ask must not leak reply channels"
            );

            // Tick to drain the pre-filled message (actor → Idle after noop_dispatch).
            crate::bridge::hew_wasm_tick(HEW_WASM_ASK_TICK_ACTIVATIONS);
            // Actor is Idle — close and free without a separate stop.
            hew_actor_stop(actor);
            assert_eq!(hew_actor_free(actor), 0);

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    /// WASM unbounded ask returns `NoRunnableWork` when the scheduler has no more
    /// runnable actors and the handler never replied.
    ///
    /// `noop_dispatch` processes the ask message but does not call `hew_reply` and
    /// does not self-stop. After one tick the run queue is empty (`remaining == 0`)
    /// and the actor is alive (Idle), so the ask path returns `NoRunnableWork`.
    #[test]
    fn wasm_ask_no_runnable_work_sets_no_runnable_work_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(
                reply.is_null(),
                "ask when handler does not reply must return null"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::NoRunnableWork as i32,
                "no-reply handler with drained scheduler must report NoRunnableWork"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "NoRunnableWork path must not leak reply channels"
            );

            // Actor is Idle after noop_dispatch drained its message.
            // Idle is quiescent — free directly without an explicit stop.
            assert_eq!(hew_actor_free(actor), 0);

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }
}
