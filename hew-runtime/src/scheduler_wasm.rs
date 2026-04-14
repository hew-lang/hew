//! Cooperative actor scheduler for WASM targets (single-threaded).
//!
//! This is the WASM counterpart of [`crate::scheduler`]. Since WASM runs
//! in a single-threaded environment, there is no work stealing, no thread
//! parking, and no concurrent CAS contention. State transitions use plain
//! atomic stores with `Relaxed` ordering, and the run queue is a simple
//! `VecDeque`.
//!
//! # C ABI
//!
//! - [`hew_sched_init`] — create the run queue.
//! - [`hew_sched_shutdown`] — drain the queue, reset state.
//! - [`hew_sched_run`] — run all actors to completion.
//!
//! # Internal API
//!
//! - [`sched_enqueue`] — submit an actor for scheduling.

use std::collections::VecDeque;
use std::ffi::{c_int, c_void};
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, Ordering};

#[cfg(test)]
use crate::actor::HEW_PRIORITY_NORMAL;
use crate::actor::{HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET, HEW_PRIORITY_HIGH, HEW_PRIORITY_LOW};
use crate::internal::types::HewActorState;

#[inline]
fn notify_actor_group_waiters(actor_id: u64) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::actor_group::notify_actor_death(actor_id);
    }

    #[cfg(target_arch = "wasm32")]
    {
        let _ = actor_id;
    }
}

// ── HewActor layout (matches native actor.rs exactly) ───────────────────

/// Actor struct layout for WASM. Field order and types MUST match the
/// native [`crate::actor::HewActor`] definition to maintain C ABI
/// compatibility.
#[repr(C)]
#[derive(Debug)]
pub struct HewActor {
    pub sched_link_next: AtomicPtr<HewActor>,
    pub id: u64,
    pub pid: u64,
    pub state: *mut c_void,
    pub state_size: usize,
    pub dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
    pub mailbox: *mut c_void,
    pub actor_state: AtomicI32,
    pub budget: AtomicI32,
    pub init_state: *mut c_void,
    pub init_state_size: usize,
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    pub terminate_fn: Option<unsafe extern "C" fn(*mut c_void)>,
    pub terminate_called: AtomicBool,
    pub terminate_finished: AtomicBool,
    pub error_code: AtomicI32,
    pub supervisor: *mut c_void,
    pub supervisor_child_index: i32,
    pub priority: AtomicI32,
    pub reductions: AtomicI32,
    pub idle_count: AtomicI32,
    pub hibernation_threshold: AtomicI32,
    pub hibernating: AtomicI32,
    pub prof_messages_processed: AtomicU64,
    pub prof_processing_time_ns: AtomicU64,
    pub arena: *mut c_void,
}

// SAFETY: Single-threaded on WASM; on native (tests), the struct is only
// used from one thread at a time.
unsafe impl Send for HewActor {}
// SAFETY: Single-threaded on WASM; on native (tests), the struct is only
// accessed from one thread at a time.
unsafe impl Sync for HewActor {}

// Compile-time check: the WASM scheduler's local HewActor must have
// identical size, alignment, and field offsets to the canonical native
// definition so that the C ABI layout never diverges.
const _: () = {
    use std::mem::offset_of;
    type W = HewActor;
    type N = crate::actor::HewActor;

    assert!(
        size_of::<W>() == size_of::<N>(),
        "WASM HewActor size diverged from native"
    );
    assert!(
        align_of::<W>() == align_of::<N>(),
        "WASM HewActor alignment diverged from native"
    );

    // Every field must sit at the same offset in both structs.
    assert!(offset_of!(W, sched_link_next) == offset_of!(N, sched_link_next));
    assert!(offset_of!(W, id) == offset_of!(N, id));
    assert!(offset_of!(W, pid) == offset_of!(N, pid));
    assert!(offset_of!(W, state) == offset_of!(N, state));
    assert!(offset_of!(W, state_size) == offset_of!(N, state_size));
    assert!(offset_of!(W, dispatch) == offset_of!(N, dispatch));
    assert!(offset_of!(W, mailbox) == offset_of!(N, mailbox));
    assert!(offset_of!(W, actor_state) == offset_of!(N, actor_state));
    assert!(offset_of!(W, budget) == offset_of!(N, budget));
    assert!(offset_of!(W, init_state) == offset_of!(N, init_state));
    assert!(offset_of!(W, init_state_size) == offset_of!(N, init_state_size));
    assert!(offset_of!(W, coalesce_key_fn) == offset_of!(N, coalesce_key_fn));
    assert!(offset_of!(W, terminate_fn) == offset_of!(N, terminate_fn));
    assert!(offset_of!(W, terminate_called) == offset_of!(N, terminate_called));
    assert!(offset_of!(W, terminate_finished) == offset_of!(N, terminate_finished));
    assert!(offset_of!(W, error_code) == offset_of!(N, error_code));
    assert!(offset_of!(W, supervisor) == offset_of!(N, supervisor));
    assert!(offset_of!(W, supervisor_child_index) == offset_of!(N, supervisor_child_index));
    assert!(offset_of!(W, priority) == offset_of!(N, priority));
    assert!(offset_of!(W, reductions) == offset_of!(N, reductions));
    assert!(offset_of!(W, idle_count) == offset_of!(N, idle_count));
    assert!(offset_of!(W, hibernation_threshold) == offset_of!(N, hibernation_threshold));
    assert!(offset_of!(W, hibernating) == offset_of!(N, hibernating));
    assert!(offset_of!(W, prof_messages_processed) == offset_of!(N, prof_messages_processed));
    assert!(offset_of!(W, prof_processing_time_ns) == offset_of!(N, prof_processing_time_ns));
    assert!(offset_of!(W, arena) == offset_of!(N, arena));
};

// ── HewMsgNode layout (strict prefix of native mailbox.rs) ──────────────

/// Message node layout.  The shared prefix fields (`next` … `reply_channel`)
/// have identical offsets to [`crate::mailbox::HewMsgNode`] for C ABI
/// compat.  The native struct appends a `trace_context` tail field that WASM
/// intentionally omits; this struct is a strict prefix of the native layout.
#[repr(C)]
#[derive(Debug)]
pub struct HewMsgNode {
    pub next: AtomicPtr<HewMsgNode>,
    pub msg_type: i32,
    pub data: *mut c_void,
    pub data_size: usize,
    pub reply_channel: *mut c_void,
}

// Compile-time check: the WASM scheduler's local HewMsgNode must have
// identical alignment and field offsets (for the shared prefix fields) to
// the canonical native definition in `crate::mailbox`.
//
// The native struct appends `trace_context` after `reply_channel`; the WASM
// struct is intentionally a strict prefix, so we check per-field offsets and
// alignment rather than size equality.
//
// Gated to `not(target_arch = "wasm32")` because `crate::mailbox` is only
// compiled on native targets; this block therefore runs during `cargo test`
// where both modules exist simultaneously.
#[cfg(not(target_arch = "wasm32"))]
const _: () = {
    use std::mem::offset_of;
    type W = HewMsgNode;
    type N = crate::mailbox::HewMsgNode;

    assert!(
        align_of::<W>() == align_of::<N>(),
        "WASM HewMsgNode alignment diverged from native"
    );
    assert!(
        size_of::<W>() <= size_of::<N>(),
        "WASM HewMsgNode grew larger than native — layout diverged"
    );

    assert!(offset_of!(W, next) == offset_of!(N, next));
    assert!(offset_of!(W, msg_type) == offset_of!(N, msg_type));
    assert!(offset_of!(W, data) == offset_of!(N, data));
    assert!(offset_of!(W, data_size) == offset_of!(N, data_size));
    assert!(offset_of!(W, reply_channel) == offset_of!(N, reply_channel));
};

// ── External mailbox functions ──────────────────────────────────────────
// Resolved at link time: from mailbox_wasm.rs on WASM, from mailbox.rs
// on native (tests).

#[cfg(target_arch = "wasm32")]
extern "C" {
    fn hew_mailbox_try_recv(mb: *mut c_void) -> *mut HewMsgNode;
    fn hew_mailbox_has_messages(mb: *mut c_void) -> i32;
    fn hew_msg_node_free(node: *mut HewMsgNode);
}

#[cfg(all(test, not(target_arch = "wasm32")))]
unsafe fn hew_mailbox_try_recv(mb: *mut c_void) -> *mut HewMsgNode {
    // SAFETY: Tests pass a mailbox allocated by mailbox_wasm with the same
    // message-node layout as scheduler_wasm's local copy.
    unsafe { crate::mailbox_wasm::hew_mailbox_try_recv(mb.cast()).cast() }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
unsafe fn hew_mailbox_has_messages(mb: *mut c_void) -> i32 {
    // SAFETY: Tests pass a mailbox allocated by mailbox_wasm.
    unsafe { crate::mailbox_wasm::hew_mailbox_has_messages(mb.cast()) }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
unsafe fn hew_msg_node_free(node: *mut HewMsgNode) {
    // SAFETY: Tests pass message nodes allocated by mailbox_wasm.
    unsafe { crate::mailbox_wasm::hew_msg_node_free(node.cast()) }
}

// ── Clock helper ────────────────────────────────────────────────────────
// Resolved at link time: from io_time.rs on native, from wasm_stubs on WASM.

/// Return current time in milliseconds (monotonic, simtime-aware in tests).
///
/// On native targets this calls `io_time::hew_now_ms` which honours the
/// deterministic simulation clock.  On wasm32 it resolves to the
/// `wasm_stubs::hew_now_ms` symbol.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn hew_now_ms() -> u64 {
    // SAFETY: hew_now_ms from io_time.rs has no preconditions.
    unsafe { crate::io_time::hew_now_ms() }
}

#[cfg(target_arch = "wasm32")]
unsafe fn hew_now_ms() -> u64 {
    extern "C" {
        fn hew_now_ms() -> u64;
    }
    // SAFETY: symbol is always present on wasm32 (wasm_stubs).
    unsafe { hew_now_ms() }
}

// ── Arena lifecycle helpers ──────────────────────────────────────────────
//
// Both native and wasm32 builds now use the same `crate::arena` module
// (on wasm32 it resolves to `arena_wasm.rs`).  There is no longer a
// split between real-call and no-op paths.

/// Install `arena` as the per-activation current arena and return the
/// previously active arena pointer.  Mirrors `crate::arena::set_current_arena`.
fn arena_install(arena: *mut c_void) -> *mut c_void {
    crate::arena::set_current_arena(arena.cast::<crate::arena::ActorArena>()).cast::<c_void>()
}

/// Reset `arena` for reuse after a completed dispatch cycle.
/// Mirrors `crate::arena::hew_arena_reset`.  Safe to call with null.
///
/// # Safety
///
/// `arena` must be either null or a valid pointer previously returned by
/// `hew_arena_new()` that has not yet been freed.
unsafe fn arena_reset(arena: *mut c_void) {
    if !arena.is_null() {
        // SAFETY: caller guarantees arena is valid.
        unsafe { crate::arena::hew_arena_reset(arena.cast::<crate::arena::ActorArena>()) };
    }
}

// ── Global state (single-threaded, no atomics needed) ───────────────────

static mut RUN_QUEUE: Option<VecDeque<*mut HewActor>> = None;
static mut INITIALIZED: bool = false;

/// Sleep queue: actors parked until a host-driven deadline expires.
///
/// Each entry is `(deadline_ms, actor_ptr)`.  The vector is kept sorted by
/// ascending deadline so the front is always the soonest to wake.  Actors
/// in this queue are in `Idle` state; they are re-enqueued as `Runnable`
/// when the deadline passes (see [`drain_expired_sleepers`]).
///
/// Drop/cleanup contract: cleared in [`hew_sched_shutdown`].
static mut SLEEP_QUEUE: Vec<(u64, *mut HewActor)> = Vec::new();

/// Pending sleep deadline set by the currently-dispatching actor via
/// [`request_sleep`].  Zero means no pending sleep.  Consumed and reset
/// by [`activate_actor_wasm`] after each message dispatch.
static mut PENDING_SLEEP_DEADLINE_MS: u64 = 0;

/// Whether an actor is currently being activated (for `active_workers` metric).
static mut ACTIVATING: bool = false;

/// Current depth of cooperative-tick reentrant calls. Incremented on
/// entry to [`hew_wasm_sched_tick`], decremented on exit. Used by
/// [`hew_actor_cooperate`] to suppress *cooperate-driven* recursion
/// when the depth reaches [`MAX_COOPERATIVE_TICK_DEPTH`], preventing
/// unbounded WASM stack growth (WASM has a fixed, non-growable stack).
///
/// **Important**: wait-loop callers (ask/await/reply) are *not* subject
/// to this cap — they must always make forward progress to avoid
/// no-progress spins.
static mut COOPERATIVE_TICK_DEPTH: u32 = 0;

/// Maximum allowed nesting depth for cooperative ticks. The WASM default
/// stack is typically 64 KiB–1 MiB, and each activation frame is
/// non-trivial, so we cap cooperate-driven reentrancy at a conservative
/// level. Wait-loop reentry is not capped (see [`hew_actor_cooperate`]).
const MAX_COOPERATIVE_TICK_DEPTH: u32 = 16;

/// Saved arena pointer during activation.
static mut PREV_ARENA: *mut c_void = std::ptr::null_mut();

/// Reply channel for the message currently being dispatched (WASM
/// equivalent of the native thread-local `CURRENT_REPLY_CHANNEL`).
static mut CURRENT_REPLY_CHANNEL: *mut c_void = std::ptr::null_mut();
/// Whether the current dispatch consumed the reply channel's sender-side
/// reference by calling `hew_reply`.
static mut CURRENT_REPLY_CHANNEL_CONSUMED: bool = false;

fn set_current_reply_channel(ch: *mut c_void) {
    // SAFETY: Single-threaded on WASM; no concurrent access.
    unsafe {
        CURRENT_REPLY_CHANNEL = ch;
        CURRENT_REPLY_CHANNEL_CONSUMED = false;
    }
}

fn clear_current_reply_channel() -> *mut c_void {
    // SAFETY: Single-threaded on WASM; no concurrent access.
    unsafe {
        let ch = CURRENT_REPLY_CHANNEL;
        CURRENT_REPLY_CHANNEL = std::ptr::null_mut();
        CURRENT_REPLY_CHANNEL_CONSUMED = false;
        ch
    }
}

pub(crate) fn mark_current_reply_channel_consumed(ch: *mut c_void) {
    if ch.is_null() {
        return;
    }
    // SAFETY: Single-threaded on WASM; no concurrent access.
    unsafe {
        if CURRENT_REPLY_CHANNEL == ch {
            CURRENT_REPLY_CHANNEL_CONSUMED = true;
        }
    }
}

fn current_reply_channel_consumed() -> bool {
    // SAFETY: Single-threaded on WASM; no concurrent access.
    unsafe { CURRENT_REPLY_CHANNEL_CONSUMED }
}

// ── Metrics counters (plain u64, no atomics needed) ─────────────────────

static mut TASKS_SPAWNED: u64 = 0;
static mut TASKS_COMPLETED: u64 = 0;
static mut MESSAGES_SENT: u64 = 0;
static mut MESSAGES_RECEIVED: u64 = 0;

pub(crate) fn record_message_sent() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        MESSAGES_SENT += 1;
    }
}

pub(crate) fn record_message_received() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        MESSAGES_RECEIVED += 1;
    }
}

// ── Sleep timer helpers ─────────────────────────────────────────────────

/// Record a sleep request for the currently-dispatching actor.
///
/// Called by `hew_sleep_ms` on WASM.  Records the largest (latest)
/// deadline when called multiple times within a single dispatch cycle.
/// The pending deadline is consumed by [`activate_actor_wasm`] after
/// the current message dispatch returns.
pub(crate) fn request_sleep(deadline_ms: u64) {
    if deadline_ms == 0 {
        return;
    }
    // SAFETY: Single-threaded on WASM; only mutated from dispatch context.
    unsafe {
        if deadline_ms > PENDING_SLEEP_DEADLINE_MS {
            PENDING_SLEEP_DEADLINE_MS = deadline_ms;
        }
    }
}

/// Park `actor` in the sleep queue until `deadline_ms`.
///
/// Sets the actor state to `Idle` and inserts it into the sorted sleep
/// queue.  The actor is NOT in the run queue while sleeping.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is currently
/// in `Running` state (i.e., called from within `activate_actor_wasm`).
unsafe fn park_actor_sleep(actor: *mut HewActor, deadline_ms: u64) {
    // SAFETY: caller guarantees `actor` is a valid, live pointer.
    let a = unsafe { &*actor };
    // Use Sleeping (not Idle) so that message-send paths do not treat this
    // actor as wake-eligible.  Messages queue in the mailbox and are
    // delivered when the timer fires and drain_expired_sleepers re-enqueues.
    a.actor_state
        .store(HewActorState::Sleeping as i32, Ordering::Relaxed);
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative scheduler; no concurrent mutation"
    )]
    // SAFETY: Single-threaded on WASM.
    unsafe {
        // Keep the queue sorted by ascending deadline for O(1) front-peek.
        let pos = SLEEP_QUEUE.partition_point(|&(d, _)| d <= deadline_ms);
        SLEEP_QUEUE.insert(pos, (deadline_ms, actor));
    }
}

/// Wake all sleeping actors whose deadline ≤ `now_ms`.
///
/// For each expired entry:
/// - If the actor is still `Idle`, transitions it to `Runnable` and
///   re-enqueues it on the run queue.
/// - If the actor has since been stopped/crashed, discards the entry.
///
/// Returns the number of actors woken.
///
/// # Safety
///
/// Must be called from within the WASM scheduler's single-threaded
/// execution context.
unsafe fn drain_expired_sleepers(now_ms: u64) -> u32 {
    let mut woken: u32 = 0;
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative scheduler; no concurrent mutation"
    )]
    // SAFETY: Single-threaded cooperative scheduler; SLEEP_QUEUE not aliased.
    unsafe {
        while let Some(&(deadline, actor)) = SLEEP_QUEUE.first() {
            if deadline > now_ms {
                break; // Queue is sorted; all remaining deadlines are later.
            }
            SLEEP_QUEUE.remove(0);
            let state = (*actor).actor_state.load(Ordering::Relaxed);
            if state == HewActorState::Sleeping as i32 {
                (*actor)
                    .actor_state
                    .store(HewActorState::Runnable as i32, Ordering::Relaxed);
                // Fail-closed: panic if the scheduler was not initialized.
                if let Err(msg) = try_sched_enqueue(actor) {
                    panic!("{msg}");
                }
                woken += 1;
            }
            // Stopped/Crashed actors are silently discarded from the queue;
            // their resources are managed by hew_actor_close / cleanup_all_actors.
        }
    }
    woken
}

/// Remove any [`SLEEP_QUEUE`] entry for `actor`, if present.
///
/// Called by [`crate::actor::cleanup_all_actors`] just before freeing an
/// actor to prevent a use-after-free if the host calls
/// [`hew_wasm_timer_tick`] after an actor has been freed but before the
/// queue entry has been drained normally.
///
/// Idempotent: if the actor is not in the queue, this is a no-op.
///
/// # Safety
///
/// Must be called from the single-threaded WASM cooperative scheduler
/// context (same thread that owns `SLEEP_QUEUE`).
pub(crate) unsafe fn cancel_actor_sleep_queue_entry(actor: *mut HewActor) {
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative scheduler; no concurrent mutation"
    )]
    // SAFETY: single-threaded; caller upholds cooperative-scheduler invariant.
    unsafe {
        SLEEP_QUEUE.retain(|&(_, a)| !std::ptr::eq(a, actor));
    }
}

// ── C ABI ───────────────────────────────────────────────────────────────

/// Initialize the cooperative scheduler.
///
/// Creates the run queue. Calling more than once is a no-op.
/// Returns 0 on success. WASM is single-threaded so this always succeeds.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_init() -> c_int {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        if INITIALIZED {
            return 0;
        }
        RUN_QUEUE = Some(VecDeque::new());
        INITIALIZED = true;
    }
    crate::bridge::bridge_init();
    0
}

/// Drain all currently-runnable actors without spinning on the sleep queue.
///
/// Unlike [`hew_sched_run`], this function exits as soon as the run queue is
/// empty regardless of whether any actors are sleeping.  It is used by
/// [`hew_sched_shutdown`] to prevent a far-future sleep deadline from
/// indefinitely blocking teardown.
///
/// Any actor that calls `sleep_ms` during this drain will be parked into
/// `SLEEP_QUEUE`; the caller must clear that queue afterwards.
///
/// # Safety
///
/// Must be called from a single-threaded WASM context after
/// [`hew_sched_init`].
unsafe fn drain_run_queue_for_shutdown() {
    loop {
        // SAFETY: single-threaded cooperative scheduler.
        if !unsafe { step_one_actor() } {
            break; // run queue empty — do not wait for sleep deadlines
        }
    }
}

/// Shut down the cooperative scheduler.
///
/// Drains all currently-runnable actors and then resets **all** scheduler
/// lifetime statics to their initial values.  Safe to call if the scheduler
/// was never initialized.
///
/// Unlike [`hew_sched_run`], shutdown does **not** wait for sleeping actors
/// whose timer has not yet expired.  The sleep queue is cleared before the
/// drain and again after, so any actor that calls `sleep_ms` during the
/// shutdown drain cannot prolong teardown.
///
/// Resetting every static (including `ACTIVATING`, `PREV_ARENA`,
/// `CURRENT_REPLY_CHANNEL`, `CURRENT_REPLY_CHANNEL_CONSUMED`, and the
/// metrics counters) ensures that a subsequent [`hew_sched_init`] starts
/// from a genuinely clean slate even after hot-reload or test-harness reuse.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_shutdown() {
    // Cancel all sleeping actors before draining.  This prevents
    // hew_sched_run()'s sleep-queue spin-wait from blocking teardown on
    // far-future timer deadlines.
    #[expect(
        static_mut_refs,
        reason = "single-threaded shutdown; no concurrent access"
    )]
    // SAFETY: Single-threaded; no concurrent sleep-queue access during shutdown.
    unsafe {
        SLEEP_QUEUE.clear();
        PENDING_SLEEP_DEADLINE_MS = 0;
    }

    // Drain all currently-runnable actors without waiting for sleep deadlines.
    // If any actor calls sleep_ms during this drain it will be added to
    // SLEEP_QUEUE, which we clear again immediately below.
    // SAFETY: Single-threaded on WASM.
    unsafe { drain_run_queue_for_shutdown() };

    // Second clear: discard any new sleep entries created during the drain.
    #[expect(
        static_mut_refs,
        reason = "single-threaded shutdown; no concurrent access"
    )]
    // SAFETY: Single-threaded; drain_run_queue_for_shutdown has returned.
    unsafe {
        SLEEP_QUEUE.clear();
        PENDING_SLEEP_DEADLINE_MS = 0;
    }

    crate::bridge::bridge_shutdown();

    // SAFETY: Single-threaded on WASM.
    unsafe {
        RUN_QUEUE = None;
        INITIALIZED = false;
        // Reset activation-context statics so stale state from a prior
        // mid-activation abort or skipped shutdown cannot bleed into a
        // subsequent init → use cycle.
        ACTIVATING = false;
        COOPERATIVE_TICK_DEPTH = 0;
        PREV_ARENA = std::ptr::null_mut();
        CURRENT_REPLY_CHANNEL = std::ptr::null_mut();
        CURRENT_REPLY_CHANNEL_CONSUMED = false;
        // Reset metrics so a re-init cycle starts from zero.
        TASKS_SPAWNED = 0;
        TASKS_COMPLETED = 0;
        MESSAGES_SENT = 0;
        MESSAGES_RECEIVED = 0;
        // Clear the sleep queue and pending-sleep context so any actors that
        // were parked during a partial run do not linger across a re-init cycle.
        #[expect(static_mut_refs, reason = "single-threaded shutdown path")]
        SLEEP_QUEUE.clear();
        PENDING_SLEEP_DEADLINE_MS = 0;
    }
}

/// Clean up all remaining runtime resources after shutdown.
///
/// WASM counterpart of the native `hew_runtime_cleanup()`. Frees any
/// actors not explicitly freed by user code and clears the registry.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_runtime_cleanup() {
    // Free all tracked actors.
    // SAFETY: Single-threaded on WASM, called after hew_sched_shutdown.
    unsafe { crate::actor::cleanup_all_actors() };
    // Clear the name registry.
    crate::registry::hew_registry_clear();
}

/// Pop one actor from the run queue, activate it, and re-enqueue it if
/// it is still runnable. Returns `true` if an actor was activated, `false`
/// if the queue was empty or uninitialized.
///
/// # Safety
///
/// Must only be called from a single-threaded WASM context after
/// [`hew_sched_init`] has been called.
unsafe fn step_one_actor() -> bool {
    // SAFETY: Single-threaded on WASM; RUN_QUEUE is only mutated here.
    unsafe {
        let actor = match RUN_QUEUE {
            Some(ref mut q) => q.pop_front(),
            None => return false,
        };
        let Some(actor) = actor else {
            return false;
        };
        activate_actor_wasm(actor);

        // Re-enqueue if the actor is still runnable.
        let state = (*actor).actor_state.load(Ordering::Relaxed);
        if state == HewActorState::Runnable as i32 {
            if let Some(ref mut q) = RUN_QUEUE {
                q.push_back(actor);
            }
        }
        true
    }
}

/// Run all enqueued actors to completion.
///
/// Loops until both the run queue and the sleep queue are empty: pops
/// the front actor, activates it, and re-enqueues it if it still has
/// pending messages.  Between activation rounds, drains any sleeping
/// actors whose deadline has passed (using the real/simulated clock).
///
/// For standalone WASM programs where sleeping actors are the only
/// remaining work, this function spin-polls until all deadlines expire.
/// In host-driven environments, prefer [`hew_wasm_sched_tick`] /
/// [`hew_wasm_timer_tick`] to avoid blocking the host event loop.
///
/// This is the main entry point for standalone WASM programs.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_run() {
    loop {
        // SAFETY: hew_now_ms is safe on all targets; drain is single-threaded.
        let now = unsafe { hew_now_ms() };
        // SAFETY: Single-threaded; SLEEP_QUEUE accessed from cooperative scheduler only.
        unsafe { drain_expired_sleepers(now) };

        // SAFETY: Single-threaded on WASM.
        if !unsafe { step_one_actor() } {
            // Run queue empty. Stop only when the sleep queue is also empty.
            // SAFETY: Single-threaded on WASM.
            #[expect(static_mut_refs, reason = "single-threaded cooperative scheduler")]
            let sleeping = unsafe { SLEEP_QUEUE.is_empty() };
            if sleeping {
                break;
            }
            // Sleeping actors remain: spin-poll until the next deadline passes.
            // This is a cooperative spin; in WASI the OS may preempt us.
        }
    }
}

// ── Internal API ────────────────────────────────────────────────────────

/// Submit an actor to the run queue.
///
/// # Panics
///
/// Fallible inner implementation of [`sched_enqueue`].
///
/// Returns `Ok(())` when the actor was successfully placed on the run queue,
/// or `Err` with a static message when the scheduler is not initialized.
/// The public [`sched_enqueue`] wrapper calls this and panics on `Err`,
/// preserving the fail-closed contract while allowing wasm-target tests to
/// assert on the error path without relying on unwinding.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
unsafe fn try_sched_enqueue(actor: *mut HewActor) -> Result<(), &'static str> {
    // SAFETY: Single-threaded on WASM; caller guarantees actor validity.
    unsafe {
        match RUN_QUEUE {
            Some(ref mut q) => {
                q.push_back(actor);
                // Only count after the actor is actually on the queue.
                TASKS_SPAWNED += 1;
                Ok(())
            }
            None => Err("sched_enqueue: scheduler not initialized (RUN_QUEUE is None)"),
        }
    }
}

/// Enqueue an actor onto the WASM run queue.
///
/// This is the fail-closed public wrapper around [`try_sched_enqueue`].
/// Previously this silently dropped the actor while still incrementing
/// `TASKS_SPAWNED`, leaving metrics inconsistent and work silently lost.
///
/// # Panics
///
/// Panics if the scheduler has not been initialized (`RUN_QUEUE` is
/// `None`), aligning with the native scheduler's fail-closed posture.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
pub unsafe fn sched_enqueue(actor: *mut HewActor) {
    // SAFETY: caller guarantees actor validity; try_sched_enqueue has
    // the same safety contract.
    unsafe {
        if let Err(msg) = try_sched_enqueue(actor) {
            panic!("{msg}");
        }
    }
}

/// C ABI wrapper for [`sched_enqueue`], callable from [`crate::bridge`].
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_wasm_sched_enqueue(actor: *mut c_void) {
    // SAFETY: Caller guarantees actor is a valid HewActor pointer.
    unsafe { sched_enqueue(actor.cast::<HewActor>()) };
}

/// Tick-based scheduler: run up to `max_activations` actor activations,
/// then return the number of actors still in the run queue.
///
/// This is the primary host-driven scheduling API. Unlike [`hew_sched_run`]
/// which runs to completion, this returns control to the host after a
/// bounded amount of work.
///
/// This function always makes forward progress when the run queue is
/// non-empty. Cooperate-driven recursion depth is bounded inside
/// [`hew_actor_cooperate`], not here, so that wait-loop callers
/// (ask/await/reply) never observe a non-zero return without actual work
/// having been performed.
///
/// Sleeping actors whose deadline has passed (according to the current
/// clock) are re-enqueued before activations run.  Hosts that use
/// hardware/JS timers should call [`hew_wasm_timer_tick`] with an
/// explicit `now_ms` instead to avoid repeated clock reads.
///
/// # Safety
///
/// The scheduler must have been initialized with [`hew_sched_init`].
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub unsafe extern "C" fn hew_wasm_sched_tick(max_activations: i32) -> i32 {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        COOPERATIVE_TICK_DEPTH += 1;

        // Drain any sleeping actors whose deadline has now passed.
        let now = hew_now_ms();
        drain_expired_sleepers(now);

        for _ in 0..max_activations {
            if !step_one_actor() {
                break;
            }
        }

        COOPERATIVE_TICK_DEPTH -= 1;

        // Return remaining queue length.
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_possible_wrap,
            reason = "run queue length will not exceed i32::MAX"
        )]
        match RUN_QUEUE {
            Some(ref q) => q.len() as i32,
            None => 0,
        }
    }
}

/// Advance the WASM timer: re-enqueue all sleeping actors whose
/// deadline ≤ `now_ms`.
///
/// Host-driven alternative to relying on the clock inside
/// [`hew_wasm_sched_tick`].  Useful for JS hosts that receive
/// `setTimeout` callbacks with a precise timestamp, or for WASI
/// programs that advance the clock via `clock_time_get`.
///
/// Returns the number of actors woken; a return value > 0 indicates
/// that there is new work in the run queue ready for [`hew_wasm_sched_tick`].
///
/// # Safety
///
/// The scheduler must have been initialized with [`hew_sched_init`].
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub unsafe extern "C" fn hew_wasm_timer_tick(now_ms: u64) -> i32 {
    // SAFETY: Single-threaded on WASM.
    #[expect(
        clippy::cast_possible_wrap,
        reason = "number of woken actors will not exceed i32::MAX"
    )]
    // SAFETY: caller upholds single-threaded cooperative scheduler invariant.
    unsafe {
        drain_expired_sleepers(now_ms) as i32
    }
}

/// Return the number of actors currently parked in the sleep queue.
///
/// Hosts can use this together with the run-queue length returned by
/// [`hew_wasm_sched_tick`] to decide whether to schedule a future
/// timer callback or stop driving the scheduler.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_wasm_sleeping_count() -> i32 {
    // SAFETY: Single-threaded on WASM.
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "sleep queue length will not exceed i32::MAX"
    )]
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative scheduler; read-only access"
    )]
    // SAFETY: Single-threaded cooperative scheduler; SLEEP_QUEUE not mutated concurrently.
    unsafe {
        SLEEP_QUEUE.len() as i32
    }
}

// ── Actor activation ────────────────────────────────────────────────────

/// Activate an actor: drain messages up to budget, then transition to
/// the appropriate state.
///
/// This is the WASM-simplified version of the native `activate_actor`.
/// Key differences from native:
/// - No signal recovery (`sigsetjmp`/`siglongjmp`) — no signals on WASM.
/// - No `ACTIVE_WORKERS` tracking (always 1 worker).
/// - No crash fault injection or delay faults.
/// - State transitions use plain `.store()` — single thread, no contention.
///   Atomics are still used because [`HewActor`] fields are `AtomicI32`.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
#[expect(
    clippy::too_many_lines,
    reason = "reply_channel clear mirrors native scheduler"
)]
unsafe fn activate_actor_wasm(actor: *mut HewActor) {
    // SAFETY: Only valid actor pointers are ever enqueued by the runtime.
    let a = unsafe { &*actor };

    // Skip terminal states.
    let state = a.actor_state.load(Ordering::Relaxed);
    if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
        return;
    }

    // Transition: RUNNABLE -> RUNNING (plain store — single thread, no CAS needed).
    if state != HewActorState::Runnable as i32 {
        return;
    }
    a.actor_state
        .store(HewActorState::Running as i32, Ordering::Relaxed);

    // Compute budget with priority scaling.
    let raw_budget = a.budget.load(Ordering::Relaxed);
    let base_budget = if raw_budget > 0 {
        raw_budget
    } else {
        HEW_MSG_BUDGET
    };
    let budget = match a.priority.load(Ordering::Relaxed) {
        HEW_PRIORITY_HIGH => base_budget.saturating_mul(2),
        HEW_PRIORITY_LOW => (base_budget / 2).max(1),
        _ => base_budget,
    };

    // Save outer activation state so that nested activations (e.g. a dispatch
    // handler calling hew_actor_ask → hew_sched_run → activate_actor_wasm) do
    // not destroy the outer actor's view of the world (Bug #2: reentrancy fix).
    // SAFETY: Single-threaded; no data races possible.
    let saved_activating: bool = unsafe { ACTIVATING };
    // SAFETY: Single-threaded; no data races possible.
    let saved_prev_arena: *mut c_void = unsafe { PREV_ARENA };
    // SAFETY: Single-threaded; no data races possible.
    let saved_reply_channel: *mut c_void = unsafe { CURRENT_REPLY_CHANNEL };
    // SAFETY: Single-threaded; no data races possible.
    let saved_reply_channel_consumed: bool = unsafe { CURRENT_REPLY_CHANNEL_CONSUMED };

    // Register this actor in the canonical current-actor slot that actor.rs
    // self APIs (hew_actor_self, hew_actor_self_pid, hew_actor_self_stop) read.
    // Returns the previous slot value so we can restore it on exit (Bug #1 fix).
    // SAFETY: actor is valid; the cast is safe because scheduler_wasm::HewActor
    // and crate::actor::HewActor have identical C ABI layouts, verified by the
    // compile-time offset_of! assertions above.
    let prev_actor =
        crate::actor::set_current_actor(actor.cast::<c_void>().cast::<crate::actor::HewActor>());

    // Install the actor's arena as the current arena so that
    // `hew_arena_malloc` inside dispatch routes through it.  The return
    // value is the arena that was active before this activation (null when
    // no outer activation is running, or the outer actor's arena during a
    // re-entrant activation).  We stash it in PREV_ARENA so that the
    // restore step below can put it back.  The reentrancy save/restore
    // around PREV_ARENA keeps the outer activation's value intact.
    //
    // SAFETY: Single-threaded global state access.
    unsafe {
        ACTIVATING = true;
        PREV_ARENA = arena_install(a.arena);
    }

    let mailbox = a.mailbox;
    // Cache the arena pointer now — after dispatch the actor may have been
    // freed by a terminate callback, making `a.arena` a dangling read.
    let actor_arena = a.arena;
    let mut msgs_processed: u32 = 0;
    // Tracks a sleep deadline requested by a `sleep_ms` call inside dispatch.
    // Non-zero means the actor should be parked in the sleep queue instead of
    // going back to RUNNABLE/IDLE after the message loop exits.
    let mut actor_sleep_deadline: u64 = 0;
    // Save and clear PENDING_SLEEP_DEADLINE_MS on activation entry.
    //
    // Saving is required to support nested activations: if an outer actor calls
    // `sleep_ms(...)` and then performs an ask/await that drives a nested
    // `activate_actor_wasm` call, the inner activation must not destroy the
    // outer actor's pending sleep.  We save it here and restore it on exit so
    // the outer actor's dispatch loop still sees it when the ask/await returns.
    //
    // Clearing is still required so that any stale value written by
    // `request_sleep` outside a dispatch handler (or left over from an edge
    // case) cannot bleed into the inner actor being activated now.
    //
    // SAFETY: Single-threaded; PENDING is only mutated by request_sleep and
    // the per-iteration consume step, both called from this function.
    let saved_pending_sleep: u64 = unsafe {
        let d = PENDING_SLEEP_DEADLINE_MS;
        PENDING_SLEEP_DEADLINE_MS = 0;
        d
    };

    if !mailbox.is_null() {
        // Process up to `budget` messages.
        for _ in 0..budget {
            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            let msg = unsafe { hew_mailbox_try_recv(mailbox) };
            if msg.is_null() {
                break;
            }

            if let Some(dispatch) = a.dispatch {
                // Reset reduction counter for this dispatch.
                a.reductions
                    .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

                // SAFETY: `dispatch` and `a.state` are valid; message fields
                // come from a well-formed `HewMsgNode`.
                unsafe {
                    let msg_ref = &*msg;
                    set_current_reply_channel(msg_ref.reply_channel);
                    dispatch(a.state, msg_ref.msg_type, msg_ref.data, msg_ref.data_size);
                }

                let reply_consumed = current_reply_channel_consumed();
                let actor_state = a.actor_state.load(Ordering::Acquire);
                let _ = clear_current_reply_channel();
                if reply_consumed
                    || (actor_state != HewActorState::Stopping as i32
                        && actor_state != HewActorState::Stopped as i32)
                {
                    // SAFETY: msg is exclusively owned by this scheduler tick;
                    // orig_reply_channel is the sender-side reference retained by
                    // ask_with_channel_wasm_internal and is valid while the message
                    // node is alive.
                    unsafe {
                        let orig_reply_channel = (*msg).reply_channel;
                        (*msg).reply_channel = std::ptr::null_mut();
                        // When the handler is alive but did NOT call hew_reply, the
                        // sender-side reference retained by ask_with_channel_wasm_internal
                        // must be released here.  msg_node_free will skip
                        // retire_reply_channel (reply_channel is now null), so without
                        // this free the reference leaks and active_channel_count stays
                        // non-zero on the NoRunnableWork return path.
                        // When reply_consumed=true, hew_reply already released the
                        // sender-side ref, so we must not free again.
                        if !reply_consumed && !orig_reply_channel.is_null() {
                            crate::reply_channel_wasm::hew_reply_channel_free(
                                orig_reply_channel.cast(),
                            );
                        }
                    }
                }

                msgs_processed += 1;
                a.prof_messages_processed.fetch_add(1, Ordering::Relaxed);
                // Skip timing for now (use 0 for elapsed_ns). Timing can be
                // added later with WASI clock_time_get.
            }

            // SAFETY: `msg` was returned by `hew_mailbox_try_recv` and is
            // now exclusively owned by us.
            unsafe { hew_msg_node_free(msg) };

            // Consume any sleep request emitted by the dispatch — do this
            // BEFORE the mid-dispatch stop check so the global is always
            // cleared on every dispatch iteration, preventing it from
            // bleeding into the next actor if this one stops or crashes.
            // SAFETY: Single-threaded on WASM.
            let pending = unsafe {
                let d = PENDING_SLEEP_DEADLINE_MS;
                PENDING_SLEEP_DEADLINE_MS = 0;
                d
            };
            if pending > 0 {
                actor_sleep_deadline = pending;
            }

            // Check for mid-dispatch stop.
            let mid_state = a.actor_state.load(Ordering::Relaxed);
            if mid_state == HewActorState::Stopping as i32
                || mid_state == HewActorState::Stopped as i32
                || mid_state == HewActorState::Crashed as i32
            {
                // actor_sleep_deadline is intentionally discarded here; the
                // post-activation terminal check will return before reaching
                // the sleep-park block, so no dangling entry is added.
                break;
            }

            if actor_sleep_deadline > 0 {
                break; // Park after this message; defer remaining messages.
            }
        }
    }

    // Restore per-activation globals so the outer activation (if any) sees its
    // own actor, arena, and reply channel again (Bug #1 + Bug #2 fix).
    crate::actor::set_current_actor(prev_actor);
    // Restore the arena that was active before this activation and reset the
    // actor's arena for the next dispatch cycle.  Mirroring the native
    // scheduler: install prev_arena (stored in PREV_ARENA) back as current,
    // then reset the actor's bump allocator so the next activation starts
    // with a clean cursor.  WASM actors now carry a real arena allocated at
    // spawn time, so arena_install and arena_reset perform live work here.
    // Both functions handle a null pointer safely for the (test-only) case
    // of a manually constructed actor without an arena.
    // SAFETY: arena_install and arena_reset are safe with null pointers.
    // PREV_ARENA was set at activation entry; actor_arena was captured above.
    unsafe {
        // Discard the return value (the just-installed actor arena) — we are
        // restoring the previous arena, not saving a new one here.
        let _ = arena_install(PREV_ARENA);

        // Native skips arena_reset when the actor crashed (crash recovery via
        // siglongjmp resets the arena itself on the crash path).  WASM has no
        // signal/siglongjmp mechanism today, so there is no separate crash
        // recovery path that could issue a competing reset.  Unconditional
        // reset here is therefore safe and correct for WASM until a crash
        // handling mechanism is added.
        arena_reset(actor_arena);
    }
    // SAFETY: Single-threaded global state access.
    unsafe {
        PREV_ARENA = saved_prev_arena;
        CURRENT_REPLY_CHANNEL = saved_reply_channel;
        CURRENT_REPLY_CHANNEL_CONSUMED = saved_reply_channel_consumed;
        ACTIVATING = saved_activating;
        // Restore the outer actor's pending sleep deadline so that a nested
        // activation (ask/await from dispatch) cannot erase it.  The inner
        // actor's own sleep deadline was captured in `actor_sleep_deadline`
        // (a local variable) during the dispatch loop above and is applied
        // further below; the global is no longer needed for the inner actor.
        PENDING_SLEEP_DEADLINE_MS = saved_pending_sleep;
        TASKS_COMPLETED += 1;
    }

    // ── Post-activation state transitions ───────────────────────────────

    let cur_state = a.actor_state.load(Ordering::Relaxed);

    // Stopping -> Stopped: finalise the lifecycle and invoke terminate callback.
    if cur_state == HewActorState::Stopping as i32 {
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
        notify_actor_group_waiters(a.id);
        // SAFETY: actor just transitioned to Stopped; dispatch is finished.
        // call_terminate_fn has an internal `terminate_called` guard so later
        // cleanup paths (hew_actor_close / cleanup_all_actors) are idempotent.
        unsafe {
            crate::actor::call_terminate_fn(actor.cast::<crate::actor::HewActor>());
        }
        return;
    }

    // Already terminal — nothing to do.
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    // Sleep park: if the dispatch called `sleep_ms`, park the actor until the
    // deadline instead of going to IDLE/RUNNABLE.  This happens AFTER the arena
    // and activation-context are restored so the actor is in a clean state.
    if actor_sleep_deadline > 0 {
        // SAFETY: actor is Running and we have exclusive ownership here.
        unsafe { park_actor_sleep(actor, actor_sleep_deadline) };
        return;
    }

    // Hibernation tracking.
    // SAFETY: HewActor (wasm) and crate::actor::HewActor have identical layouts,
    // verified by the compile-time offset_of! assertions above.
    crate::actor::update_hibernation_state(
        unsafe { &*(actor.cast::<crate::actor::HewActor>()) },
        msgs_processed,
    );

    // Check for remaining messages.
    let has_more = if mailbox.is_null() {
        false
    } else {
        // SAFETY: mailbox pointer is valid.
        unsafe { hew_mailbox_has_messages(mailbox) != 0 }
    };

    if has_more {
        // More work pending -> RUNNING -> RUNNABLE.
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // NOTE: The caller (step_one_actor) handles re-enqueue by checking
        // the actor state after activation.
    } else {
        // No more messages -> RUNNING -> IDLE.
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // Recheck: messages may have arrived during activation. On WASM
        // this is less likely (single-threaded), but host callbacks or
        // dispatch-triggered sends can enqueue messages.
        if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid.
            && unsafe { hew_mailbox_has_messages(mailbox) != 0 }
        {
            // Messages appeared -> IDLE -> RUNNABLE.
            a.actor_state
                .store(HewActorState::Runnable as i32, Ordering::Relaxed);
            // SAFETY: actor is valid.
            unsafe { sched_enqueue(actor) };
        } else if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid.
            && unsafe { crate::mailbox_wasm::mailbox_is_closed(mailbox.cast()) }
        {
            // Mailbox closed while draining -> IDLE -> STOPPED.
            // Mirrors the native scheduler's post-drain close-path (see
            // scheduler.rs `Idle -> Stopped` branch).
            //
            // Note: native also calls hew_trace_lifecycle here, but
            // `crate::tracing` is #[cfg(not(target_arch = "wasm32"))] and does
            // not exist on the real WASM target.  Consistent with the existing
            // Stopping->Stopped path in this file which likewise omits tracing.
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Relaxed);
            notify_actor_group_waiters(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            // call_terminate_fn has an internal `terminate_called` guard so
            // cleanup paths are idempotent.
            unsafe {
                crate::actor::call_terminate_fn(actor.cast::<crate::actor::HewActor>());
            }
        }
    }
}

// ── Metrics C ABI ───────────────────────────────────────────────────────

/// Return the total number of tasks spawned (enqueued) since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_tasks_spawned() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { TASKS_SPAWNED }
}

/// Return the total number of actor activations completed since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_tasks_completed() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { TASKS_COMPLETED }
}

/// Return the total number of work-steals. Always 0 on WASM (no stealing).
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_steals() -> u64 {
    0
}

/// Return the total number of messages sent since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_messages_sent() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { MESSAGES_SENT }
}

/// Return the total number of messages received since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_messages_received() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { MESSAGES_RECEIVED }
}

/// Return the number of workers currently processing actors.
/// On WASM, returns 1 during activation, 0 otherwise.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_active_workers() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { u64::from(ACTIVATING) }
}

/// Reset all scheduler metrics counters to zero.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_reset() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        TASKS_SPAWNED = 0;
        TASKS_COMPLETED = 0;
        MESSAGES_SENT = 0;
        MESSAGES_RECEIVED = 0;
    }
}

/// Return the total number of worker threads. Always 1 on WASM.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_worker_count() -> u64 {
    1
}

/// Return the approximate length of the global run queue.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub extern "C" fn hew_sched_metrics_global_queue_len() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        match RUN_QUEUE {
            Some(ref q) => q.len() as u64,
            None => 0,
        }
    }
}

/// Get the reply channel for the currently-dispatched message (WASM).
///
/// Returns null if no reply channel was set (fire-and-forget send).
#[cfg(any(target_arch = "wasm32", test))]
#[cfg_attr(target_arch = "wasm32", no_mangle)]
#[must_use]
pub extern "C" fn hew_get_reply_channel() -> *mut c_void {
    // SAFETY: Single-threaded on WASM; no concurrent access.
    unsafe { CURRENT_REPLY_CHANNEL }
}

// ── Cooperative yielding (WASM) ─────────────────────────────────────────

/// Cooperatively yield if the actor's reduction budget is exhausted.
///
/// WASM counterpart of [`crate::scheduler::hew_actor_cooperate`]. The
/// compiler inserts calls to this function at yield points (loop headers,
/// function calls). Each call decrements the reduction counter. When it
/// reaches 0 the actor yields by driving one cooperative scheduler tick
/// via [`hew_wasm_sched_tick`], and the counter is reset.
///
/// The cooperative tick is *suppressed* when [`COOPERATIVE_TICK_DEPTH`]
/// has reached [`MAX_COOPERATIVE_TICK_DEPTH`]. This prevents unbounded
/// WASM stack growth from nested cooperate → tick → cooperate chains
/// while still allowing wait-loop callers (ask/await/reply) to drive the
/// scheduler to completion.
///
/// Returns 0 if the actor should continue, 1 if it yielded.
///
/// # Safety
///
/// No preconditions — may be called from any context. When called
/// outside an actor dispatch (i.e. `CURRENT_ACTOR_WASM` is null), this
/// is a no-op.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
#[must_use]
pub extern "C" fn hew_actor_cooperate() -> c_int {
    let actor = crate::actor::hew_actor_self();
    if actor.is_null() {
        return 0;
    }

    // SAFETY: hew_actor_self returned a valid, non-null actor pointer.
    // The WASM HewActor and crate::actor::HewActor have identical layouts
    // (verified by the compile-time offset_of! assertions above), so we
    // can safely read the reductions field through the actor pointer.
    let a = unsafe { &*actor };

    // Decrement reduction counter. If still positive, continue.
    let prev = a.reductions.fetch_sub(1, Ordering::Relaxed);
    if prev > 1 {
        return 0;
    }

    // Budget exhausted — reset counter and yield via cooperative tick.
    a.reductions
        .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

    // Guard against unbounded cooperate-driven reentrancy. When a
    // cooperate call triggers hew_wasm_sched_tick which activates another
    // actor whose cooperate also calls hew_wasm_sched_tick, the WASM
    // stack grows with each level. If we are already at the maximum
    // depth, skip the tick to prevent stack overflow. The actor simply
    // continues without yielding — this is safe because the depth cap
    // only suppresses voluntary yields, not scheduler progress needed by
    // wait loops.
    //
    // SAFETY: Single-threaded on WASM.
    let depth = unsafe { std::ptr::addr_of!(COOPERATIVE_TICK_DEPTH).read() };
    if depth >= MAX_COOPERATIVE_TICK_DEPTH {
        return 1;
    }

    // Drive one cooperative scheduler tick so other actors can make
    // progress.  This is the WASM equivalent of the native
    // `thread::yield_now()`.
    //
    // SAFETY: hew_wasm_sched_tick is re-entrant-safe for the WASM
    // cooperative scheduler (reentrancy is tested and supported).
    unsafe {
        let _ = hew_wasm_sched_tick(1);
    }

    1
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    #[cfg(not(target_arch = "wasm32"))]
    use std::sync::Arc;

    use crate::internal::types::HewError;

    /// Build a minimal `HewActor` with sensible defaults.
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
        }
    }

    #[repr(C)]
    struct AskDispatchState {
        channel: *mut c_void,
        msg_type: i32,
        value: i32,
    }

    unsafe extern "C" fn reply_with_observed_channel(
        state: *mut c_void,
        msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) {
        // SAFETY: tests initialize `state` to a valid AskDispatchState.
        let state = unsafe { &mut *state.cast::<AskDispatchState>() };
        state.channel = hew_get_reply_channel();
        state.msg_type = msg_type;
        if !data.is_null() && data_size >= std::mem::size_of::<i32>() {
            // SAFETY: validated above.
            state.value = unsafe { *data.cast::<i32>() };
        }

        if !state.channel.is_null() {
            let mut reply_value = state.value * 2;
            // SAFETY: reply channel comes from the in-flight message.
            unsafe {
                crate::reply_channel_wasm::hew_reply(
                    state.channel.cast(),
                    (&raw mut reply_value).cast(),
                    std::mem::size_of::<i32>(),
                );
            }
        }
    }

    static NOISY_DISPATCHES: AtomicI32 = AtomicI32::new(0);
    static REPLY_DISPATCHES: AtomicI32 = AtomicI32::new(0);
    static LATE_REPLY_SAW_CANCELLED: AtomicBool = AtomicBool::new(false);

    unsafe extern "C" fn noisy_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        NOISY_DISPATCHES.fetch_add(1, Ordering::Relaxed);
    }

    unsafe extern "C" fn reply_payload_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) {
        REPLY_DISPATCHES.fetch_add(1, Ordering::Relaxed);

        let ch = hew_get_reply_channel();
        assert!(
            !ch.is_null(),
            "WASM ask dispatch should expose a reply channel"
        );

        let mut reply_value = if !data.is_null() && data_size >= std::mem::size_of::<i32>() {
            // SAFETY: validated above.
            unsafe { *data.cast::<i32>() }
        } else {
            0
        };

        // SAFETY: ch is the active ask reply channel for this dispatch.
        unsafe {
            crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut reply_value).cast(),
                std::mem::size_of::<i32>(),
            );
        }
    }

    unsafe extern "C" fn reply_payload_observes_cancelled_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) {
        REPLY_DISPATCHES.fetch_add(1, Ordering::Relaxed);

        let ch = hew_get_reply_channel();
        assert!(
            !ch.is_null(),
            "WASM ask dispatch should expose a reply channel"
        );
        LATE_REPLY_SAW_CANCELLED.store(
            // SAFETY: ch is the active ask reply channel for this dispatch.
            unsafe { crate::reply_channel_wasm::test_cancelled(ch.cast()) },
            Ordering::Relaxed,
        );

        let mut reply_value = if !data.is_null() && data_size >= std::mem::size_of::<i32>() {
            // SAFETY: validated above.
            unsafe { *data.cast::<i32>() }
        } else {
            0
        };

        // SAFETY: ch is the active ask reply channel for this dispatch.
        unsafe {
            crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut reply_value).cast(),
                std::mem::size_of::<i32>(),
            );
        }
    }

    fn reset_wasm_dispatch_counters() {
        NOISY_DISPATCHES.store(0, Ordering::Relaxed);
        REPLY_DISPATCHES.store(0, Ordering::Relaxed);
        LATE_REPLY_SAW_CANCELLED.store(false, Ordering::Relaxed);
    }

    unsafe fn queue_wasm_message(actor: *mut HewActor, value: i32) {
        let mut payload = value;
        // SAFETY: actor is valid and the payload buffer is live for the call.
        let rc = unsafe {
            crate::mailbox_wasm::hew_mailbox_send(
                (*actor).mailbox.cast(),
                1,
                (&raw mut payload).cast(),
                std::mem::size_of::<i32>(),
            )
        };
        assert_eq!(rc, HewError::Ok as i32);
        // SAFETY: actor is a live cooperative WASM actor under test.
        unsafe { crate::actor::wake_wasm_actor(actor.cast::<crate::actor::HewActor>()) };
    }

    /// Reset all global state between tests.
    ///
    /// # Safety
    ///
    /// Must not be called concurrently with other test code (Rust test
    /// harness serialises tests within the same module by default).
    unsafe fn reset_globals() {
        // SAFETY: Single-threaded test environment. Use raw pointer
        // writes to avoid creating references to mutable statics.
        unsafe {
            // Drop the old value before writing None: ptr::write skips the
            // destructor, so without drop_in_place the VecDeque backing buffer
            // leaks whenever reset_globals is called with a non-empty queue
            // (e.g. when a test skips hew_sched_shutdown).
            ptr::drop_in_place(ptr::addr_of_mut!(RUN_QUEUE));
            ptr::addr_of_mut!(RUN_QUEUE).write(None);
            ptr::addr_of_mut!(INITIALIZED).write(false);
            ptr::addr_of_mut!(ACTIVATING).write(false);
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(0);
            // Reset the canonical current-actor slot (CURRENT_ACTOR_WASM on
            // wasm32, thread-local on native) rather than the removed
            // scheduler-local CURRENT_ACTOR static.
            crate::actor::set_current_actor(ptr::null_mut());
            ptr::addr_of_mut!(PREV_ARENA).write(ptr::null_mut());
            ptr::addr_of_mut!(CURRENT_REPLY_CHANNEL).write(ptr::null_mut());
            ptr::addr_of_mut!(CURRENT_REPLY_CHANNEL_CONSUMED).write(false);
            ptr::addr_of_mut!(TASKS_SPAWNED).write(0);
            ptr::addr_of_mut!(TASKS_COMPLETED).write(0);
            ptr::addr_of_mut!(MESSAGES_SENT).write(0);
            ptr::addr_of_mut!(MESSAGES_RECEIVED).write(0);
            // Clear sleep queue and pending-sleep context.
            ptr::drop_in_place(ptr::addr_of_mut!(SLEEP_QUEUE));
            ptr::addr_of_mut!(SLEEP_QUEUE).write(Vec::new());
            ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(0);
            // Clear the thread-local current arena so arena lifecycle tests
            // start from a clean slate regardless of test ordering.
            crate::arena::set_current_arena(ptr::null_mut());
        }
    }

    /// Read INITIALIZED without creating a shared reference.
    unsafe fn read_initialized() -> bool {
        // SAFETY: Single-threaded test; no concurrent mutation of INITIALIZED.
        unsafe { ptr::addr_of!(INITIALIZED).read() }
    }

    /// Read `TASKS_SPAWNED` without creating a shared reference.
    unsafe fn read_tasks_spawned() -> u64 {
        // SAFETY: Single-threaded test; no concurrent mutation of TASKS_SPAWNED.
        unsafe { ptr::addr_of!(TASKS_SPAWNED).read() }
    }

    /// Read `TASKS_COMPLETED` without creating a shared reference.
    unsafe fn read_tasks_completed() -> u64 {
        // SAFETY: Single-threaded test; no concurrent mutation of TASKS_COMPLETED.
        unsafe { ptr::addr_of!(TASKS_COMPLETED).read() }
    }

    /// Read the run queue length without creating a shared reference.
    unsafe fn read_queue_len() -> usize {
        // SAFETY: Single-threaded test — no concurrent mutation.
        unsafe {
            let q_ptr = ptr::addr_of!(RUN_QUEUE);
            match &*q_ptr {
                Some(q) => q.len(),
                None => 0,
            }
        }
    }

    /// Check if the run queue exists (Some) without creating a shared ref.
    unsafe fn run_queue_exists() -> bool {
        // SAFETY: Single-threaded test — no concurrent mutation.
        unsafe {
            let q_ptr = ptr::addr_of!(RUN_QUEUE);
            (*q_ptr).is_some()
        }
    }

    #[test]
    fn init_and_shutdown_dont_panic() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        hew_sched_init();
        // SAFETY: Single-threaded test.
        unsafe {
            assert!(read_initialized());
            assert!(run_queue_exists());
        }

        hew_sched_shutdown();
        // SAFETY: Single-threaded test.
        unsafe {
            assert!(!read_initialized());
            assert!(!run_queue_exists());
        }
    }

    /// Verify that `hew_sched_shutdown` resets every scheduler lifetime static
    /// so that a subsequent `hew_sched_init` → use cycle starts from a clean
    /// slate (hot-reload / test-harness reuse contract).
    #[test]
    fn shutdown_resets_all_stale_statics() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        // Phase 1: init and use the scheduler so metrics become non-zero.
        hew_sched_init();
        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        hew_sched_run();
        // SAFETY: Single-threaded test.
        unsafe {
            assert!(
                read_tasks_spawned() > 0,
                "need non-zero tasks_spawned before shutdown"
            );
            assert!(
                read_tasks_completed() > 0,
                "need non-zero tasks_completed before shutdown"
            );
        }

        // Phase 2: simulate stale activation state that can survive a prior
        // mid-activation abort or a test that skipped hew_sched_shutdown.
        let sentinel: u8 = 0;
        let sentinel_ptr: *mut c_void = (&raw const sentinel).cast_mut().cast();
        // SAFETY: Single-threaded; ptr::addr_of_mut! avoids creating
        // references to mutable statics.
        unsafe {
            ptr::addr_of_mut!(ACTIVATING).write(true);
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(5);
            ptr::addr_of_mut!(PREV_ARENA).write(sentinel_ptr);
            ptr::addr_of_mut!(CURRENT_REPLY_CHANNEL).write(sentinel_ptr);
            ptr::addr_of_mut!(CURRENT_REPLY_CHANNEL_CONSUMED).write(true);
            ptr::addr_of_mut!(MESSAGES_SENT).write(99);
            ptr::addr_of_mut!(MESSAGES_RECEIVED).write(99);
            // Simulate a stale sleep queue entry.
            ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(999_999);
        }

        // Phase 3: shutdown must reset every scheduler lifetime static.
        hew_sched_shutdown();
        assert_shutdown_cleared_all_statics();

        // Phase 4: re-init must start from a genuinely clean slate.
        hew_sched_init();
        // SAFETY: Single-threaded test.
        unsafe {
            assert!(read_initialized(), "must be initialized after re-init");
            assert!(run_queue_exists(), "run queue must exist after re-init");
            assert_eq!(
                read_tasks_spawned(),
                0,
                "metrics must be zero at re-init start"
            );
            assert_eq!(
                hew_sched_metrics_global_queue_len(),
                0,
                "queue must be empty after re-init"
            );
            assert!(
                !ptr::addr_of!(ACTIVATING).read(),
                "ACTIVATING must be false at re-init start"
            );
        }

        hew_sched_shutdown();
    }

    /// Assert that every scheduler lifetime static is in the post-shutdown
    /// zero/empty/null state.  Extracted to keep `shutdown_resets_all_stale_statics`
    /// within the function-length lint limit.
    fn assert_shutdown_cleared_all_statics() {
        // SAFETY: Single-threaded test; called immediately after hew_sched_shutdown.
        unsafe {
            assert!(
                !ptr::addr_of!(INITIALIZED).read(),
                "INITIALIZED must be false after shutdown"
            );
            assert!(
                ptr::addr_of!(RUN_QUEUE).read().is_none(),
                "RUN_QUEUE must be None after shutdown"
            );
            assert!(
                !ptr::addr_of!(ACTIVATING).read(),
                "ACTIVATING must be false after shutdown"
            );
            assert_eq!(
                ptr::addr_of!(COOPERATIVE_TICK_DEPTH).read(),
                0,
                "COOPERATIVE_TICK_DEPTH must be zero after shutdown"
            );
            assert!(
                ptr::addr_of!(PREV_ARENA).read().is_null(),
                "PREV_ARENA must be null after shutdown"
            );
            assert!(
                ptr::addr_of!(CURRENT_REPLY_CHANNEL).read().is_null(),
                "CURRENT_REPLY_CHANNEL must be null after shutdown"
            );
            assert!(
                !ptr::addr_of!(CURRENT_REPLY_CHANNEL_CONSUMED).read(),
                "CURRENT_REPLY_CHANNEL_CONSUMED must be false after shutdown"
            );
            assert_eq!(
                read_tasks_spawned(),
                0,
                "TASKS_SPAWNED must be zero after shutdown"
            );
            assert_eq!(
                read_tasks_completed(),
                0,
                "TASKS_COMPLETED must be zero after shutdown"
            );
            assert_eq!(
                hew_sched_metrics_messages_sent(),
                0,
                "MESSAGES_SENT must be zero after shutdown"
            );
            assert_eq!(
                hew_sched_metrics_messages_received(),
                0,
                "MESSAGES_RECEIVED must be zero after shutdown"
            );
            assert_eq!(
                ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read(),
                0,
                "PENDING_SLEEP_DEADLINE_MS must be zero after shutdown"
            );
            assert!(
                ptr::addr_of!(SLEEP_QUEUE).read().is_empty(),
                "SLEEP_QUEUE must be empty after shutdown"
            );
        }
    }

    #[test]
    fn shutdown_clears_bridge_outbound_queue() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        hew_sched_init();
        // SAFETY: null payload with zero length is explicitly supported.
        unsafe { crate::bridge::hew_wasm_emit(7, ptr::null(), 0) };
        assert_eq!(crate::bridge::hew_wasm_outbound_len(), 1);

        hew_sched_shutdown();

        assert_eq!(
            crate::bridge::hew_wasm_outbound_len(),
            0,
            "scheduler shutdown must drain bridge outbound state"
        );
    }

    #[test]
    fn mailbox_metrics_track_wasm_send_and_receive() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        hew_sched_init();
        // SAFETY: mailbox is created and used exclusively within this test.
        unsafe {
            let mb = crate::mailbox_wasm::hew_mailbox_new();
            let payload: i32 = 42;
            let payload_ptr = (&raw const payload).cast_mut().cast();

            assert_eq!(hew_sched_metrics_messages_sent(), 0);
            assert_eq!(hew_sched_metrics_messages_received(), 0);

            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_send(
                    mb,
                    1,
                    payload_ptr,
                    std::mem::size_of::<i32>(),
                ),
                HewError::Ok as i32
            );
            crate::mailbox_wasm::hew_mailbox_send_sys(
                mb,
                2,
                payload_ptr,
                std::mem::size_of::<i32>(),
            );

            assert_eq!(hew_sched_metrics_messages_sent(), 2);
            assert_eq!(hew_sched_metrics_messages_received(), 0);

            let sys = crate::mailbox_wasm::hew_mailbox_try_recv_sys(mb);
            assert!(!sys.is_null());
            crate::mailbox_wasm::hew_msg_node_free(sys);

            let user = crate::mailbox_wasm::hew_mailbox_try_recv(mb);
            assert!(!user.is_null());
            crate::mailbox_wasm::hew_msg_node_free(user);

            assert_eq!(hew_sched_metrics_messages_received(), 2);

            crate::mailbox_wasm::hew_mailbox_free(mb);
        }

        hew_sched_shutdown();
    }

    #[test]
    fn double_init_is_noop() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        hew_sched_init();
        hew_sched_init(); // Should not panic or create a second queue.

        // SAFETY: Single-threaded test.
        unsafe {
            assert!(read_initialized());
        }

        hew_sched_shutdown();
    }

    #[test]
    fn enqueue_and_run_with_null_mailbox() {
        let _guard = crate::runtime_test_guard();
        // An actor with no mailbox (null) should transition from
        // Runnable -> Running -> Idle after activation.
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };

        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(read_tasks_spawned(), 1);
            assert_eq!(read_queue_len(), 1);
        }

        hew_sched_run();

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "actor with null mailbox should transition to Idle"
        );

        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(read_tasks_completed(), 1);
        }

        hew_sched_shutdown();
    }

    #[test]
    fn activate_skips_stopped_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is valid.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "stopped actor should remain stopped"
        );

        hew_sched_shutdown();
    }

    #[test]
    fn activate_skips_crashed_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is valid.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Crashed as i32,
            "crashed actor should remain crashed"
        );

        hew_sched_shutdown();
    }

    #[test]
    fn activate_skips_idle_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is valid.
        unsafe { activate_actor_wasm(actor_ptr) };

        // State should remain IDLE (only RUNNABLE actors get activated).
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "idle actor should remain idle"
        );

        hew_sched_shutdown();
    }

    #[test]
    fn metrics_counters_increment() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is valid.
        unsafe { sched_enqueue(actor_ptr) };
        assert_eq!(hew_sched_metrics_tasks_spawned(), 1);

        hew_sched_run();
        assert_eq!(hew_sched_metrics_tasks_completed(), 1);
        assert_eq!(hew_sched_metrics_steals(), 0);
        assert_eq!(hew_sched_metrics_worker_count(), 1);
        assert_eq!(hew_sched_metrics_active_workers(), 0);

        hew_sched_shutdown();
    }

    #[test]
    fn metrics_reset() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor is valid.
        unsafe { sched_enqueue(actor_ptr) };
        hew_sched_run();

        assert!(hew_sched_metrics_tasks_spawned() > 0);
        assert!(hew_sched_metrics_tasks_completed() > 0);

        hew_sched_metrics_reset();

        assert_eq!(hew_sched_metrics_tasks_spawned(), 0);
        assert_eq!(hew_sched_metrics_tasks_completed(), 0);
        assert_eq!(hew_sched_metrics_messages_sent(), 0);
        assert_eq!(hew_sched_metrics_messages_received(), 0);

        hew_sched_shutdown();
    }

    #[test]
    fn global_queue_len_reflects_enqueued_actors() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        assert_eq!(hew_sched_metrics_global_queue_len(), 0);

        let actor1 = stub_actor();
        let actor2 = stub_actor();
        let ptr1: *mut HewActor = (&raw const actor1).cast_mut();
        let ptr2: *mut HewActor = (&raw const actor2).cast_mut();

        // SAFETY: actors are valid.
        unsafe {
            sched_enqueue(ptr1);
            sched_enqueue(ptr2);
        }
        assert_eq!(hew_sched_metrics_global_queue_len(), 2);

        hew_sched_run();
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);

        hew_sched_shutdown();
    }

    // ── Tests for Bug #1 and Bug #2 fixes ───────────────────────────────

    #[cfg(target_arch = "wasm32")]
    extern "C" {
        fn hew_mailbox_new() -> *mut c_void;
        fn hew_mailbox_send(mb: *mut c_void, msg_type: i32, data: *mut c_void, size: usize) -> i32;
        fn hew_mailbox_free(mb: *mut c_void);
    }

    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn hew_mailbox_new() -> *mut c_void {
        // SAFETY: native test runs use the wasm mailbox implementation to
        // match scheduler_wasm's test-only receive wrappers.
        unsafe { crate::mailbox_wasm::hew_mailbox_new().cast() }
    }

    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn hew_mailbox_send(
        mb: *mut c_void,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) -> i32 {
        // SAFETY: native test runs pass a mailbox allocated by mailbox_wasm.
        unsafe { crate::mailbox_wasm::hew_mailbox_send(mb.cast(), msg_type, data, size) }
    }

    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn hew_mailbox_free(mb: *mut c_void) {
        // SAFETY: native test runs pass a mailbox allocated by mailbox_wasm.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mb.cast()) }
    }

    fn stub_actor_with_id(id: u64) -> HewActor {
        let mut a = stub_actor();
        a.id = id;
        a
    }

    // Dispatch callback that records hew_actor_current_id() into a static.
    static DISPATCH_SAW_ACTOR_ID: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);

    unsafe extern "C" fn dispatch_record_current_id(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        let id = crate::actor::hew_actor_current_id();
        DISPATCH_SAW_ACTOR_ID.store(id, std::sync::atomic::Ordering::Relaxed);
    }

    /// Bug #1 regression: `hew_actor_self` / `hew_actor_current_id` must return the
    /// dispatching actor's own ID during WASM dispatch, not -1 / null.
    ///
    /// Before the fix, `scheduler_wasm` set its own `CURRENT_ACTOR` slot while
    /// actor.rs self APIs read `CURRENT_ACTOR_WASM` — two different statics —
    /// so self APIs always saw null / returned -1.
    #[test]
    fn self_api_sees_current_actor_during_dispatch() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // SAFETY: hew_mailbox_new has no preconditions; returns an owned pointer.
        let mb = unsafe { hew_mailbox_new() };
        assert!(!mb.is_null(), "mailbox allocation failed");

        let mut actor = stub_actor_with_id(42);
        actor.dispatch = Some(dispatch_record_current_id);
        actor.mailbox = mb;
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: mb is a valid mailbox pointer; data is null with size 0.
        unsafe { hew_mailbox_send(mb, 0, ptr::null_mut(), 0) };

        DISPATCH_SAW_ACTOR_ID.store(-999, std::sync::atomic::Ordering::Relaxed);
        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        hew_sched_run();

        assert_eq!(
            DISPATCH_SAW_ACTOR_ID.load(std::sync::atomic::Ordering::Relaxed),
            42,
            "hew_actor_current_id() must return the dispatching actor's ID, not -1"
        );

        // SAFETY: mb is a valid mailbox pointer; all messages have been consumed.
        unsafe { hew_mailbox_free(mb) };
        hew_sched_shutdown();
    }

    // Statics for the nested-activation test.
    static OUTER_ID_BEFORE_INNER: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);
    static OUTER_ID_AFTER_INNER: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);

    unsafe extern "C" fn outer_dispatch_nested(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // Record current actor before triggering inner activation.
        OUTER_ID_BEFORE_INNER.store(
            crate::actor::hew_actor_current_id(),
            std::sync::atomic::Ordering::Relaxed,
        );
        // Simulate hew_actor_ask → hew_sched_run: run all pending actors,
        // including the inner actor already in the queue.
        hew_sched_run();
        // After inner activation returns, we should still be "outer".
        OUTER_ID_AFTER_INNER.store(
            crate::actor::hew_actor_current_id(),
            std::sync::atomic::Ordering::Relaxed,
        );
    }

    unsafe extern "C" fn inner_dispatch_noop(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // No-op: sufficient to exercise the nested activation path.
    }

    /// Bug #2 regression: when a WASM dispatch handler triggers nested
    /// activation (e.g. via `hew_actor_ask` → `hew_sched_run`), the inner
    /// activation must not permanently overwrite the outer activation's
    /// current-actor/reply-channel/activating globals.
    ///
    /// Before the fix, the inner `activate_actor_wasm` call would write its own
    /// actor pointer and then zero everything on exit, leaving the outer
    /// dispatch with a null current actor.
    #[test]
    fn nested_activation_preserves_outer_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // SAFETY: hew_mailbox_new has no preconditions; returns an owned pointer.
        let mb_outer = unsafe { hew_mailbox_new() };
        assert!(!mb_outer.is_null(), "outer mailbox allocation failed");
        // SAFETY: hew_mailbox_new has no preconditions; returns an owned pointer.
        let mb_inner = unsafe { hew_mailbox_new() };
        assert!(!mb_inner.is_null(), "inner mailbox allocation failed");

        let mut actor_outer = stub_actor_with_id(10);
        actor_outer.dispatch = Some(outer_dispatch_nested);
        actor_outer.mailbox = mb_outer;
        let outer_ptr: *mut HewActor = (&raw const actor_outer).cast_mut();

        let mut actor_inner = stub_actor_with_id(20);
        actor_inner.dispatch = Some(inner_dispatch_noop);
        actor_inner.mailbox = mb_inner;
        let inner_ptr: *mut HewActor = (&raw const actor_inner).cast_mut();

        // SAFETY: mailboxes are valid; data is null with size 0.
        unsafe { hew_mailbox_send(mb_outer, 0, ptr::null_mut(), 0) };
        // SAFETY: mailboxes are valid; data is null with size 0.
        unsafe { hew_mailbox_send(mb_inner, 0, ptr::null_mut(), 0) };

        // Enqueue outer first so it runs first; inner will be in the queue
        // when outer's dispatch calls hew_sched_run().
        OUTER_ID_BEFORE_INNER.store(-999, std::sync::atomic::Ordering::Relaxed);
        OUTER_ID_AFTER_INNER.store(-999, std::sync::atomic::Ordering::Relaxed);
        // SAFETY: actors are valid, scheduler is initialized.
        unsafe { sched_enqueue(outer_ptr) };
        // SAFETY: actors are valid, scheduler is initialized.
        unsafe { sched_enqueue(inner_ptr) };

        hew_sched_run();

        assert_eq!(
            OUTER_ID_BEFORE_INNER.load(std::sync::atomic::Ordering::Relaxed),
            10,
            "outer dispatch must see itself as current actor before inner activation"
        );
        assert_eq!(
            OUTER_ID_AFTER_INNER.load(std::sync::atomic::Ordering::Relaxed),
            10,
            "outer dispatch must still see itself after nested activation returns (save/restore)"
        );

        // SAFETY: mailboxes are valid; all messages have been consumed.
        unsafe { hew_mailbox_free(mb_outer) };
        // SAFETY: mailboxes are valid; all messages have been consumed.
        unsafe { hew_mailbox_free(mb_inner) };
        hew_sched_shutdown();
    }

    #[test]
    fn bounded_wasm_ask_does_not_drain_other_actors() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();

        let mut noisy = stub_actor();
        noisy.dispatch = Some(noisy_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        noisy.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        noisy
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        noisy.budget.store(1, Ordering::Relaxed);
        let noisy_ptr: *mut HewActor = (&raw mut noisy).cast();

        let mut replier = stub_actor();
        replier.dispatch = Some(reply_payload_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        replier.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        replier
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        replier.budget.store(1, Ordering::Relaxed);
        let replier_ptr: *mut HewActor = (&raw mut replier).cast();

        // SAFETY: actors and their mailboxes are valid for the duration of the test.
        unsafe {
            queue_wasm_message(noisy_ptr, 1);
            queue_wasm_message(noisy_ptr, 2);
            queue_wasm_message(noisy_ptr, 3);
        }

        let ask_value = 77i32;
        // SAFETY: actors and payload remain valid for the duration of the ask.
        let reply = unsafe {
            crate::actor::actor_ask_wasm_impl(
                replier_ptr.cast(),
                1,
                (&raw const ask_value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                None,
            )
        };

        assert!(!reply.is_null(), "bounded ask should receive a reply");
        // SAFETY: reply is an i32 payload allocated by hew_reply above.
        unsafe {
            assert_eq!(*reply.cast::<i32>(), ask_value);
            libc::free(reply);
        }

        assert_eq!(NOISY_DISPATCHES.load(Ordering::Relaxed), 1);
        assert_eq!(REPLY_DISPATCHES.load(Ordering::Relaxed), 1);
        assert_eq!(hew_sched_metrics_global_queue_len(), 1);
        // SAFETY: noisy mailbox remains owned by this test.
        unsafe {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_len(noisy.mailbox.cast()),
                2
            );
        }
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        hew_sched_shutdown();
        // SAFETY: mailboxes are no longer referenced after scheduler shutdown.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(noisy.mailbox.cast());
            crate::mailbox_wasm::hew_mailbox_free(replier.mailbox.cast());
            reset_globals();
        }
    }

    #[test]
    fn bounded_wasm_ask_timeout_cancels_before_target_activation() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        let mut replier = stub_actor();
        replier.dispatch = Some(reply_payload_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        replier.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        replier
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        replier.budget.store(1, Ordering::Relaxed);
        let replier_ptr: *mut HewActor = (&raw mut replier).cast();

        let ask_value = 23i32;
        // SAFETY: actor and payload remain valid for the duration of the ask.
        let reply = unsafe {
            crate::actor::actor_ask_wasm_impl(
                replier_ptr.cast(),
                1,
                (&raw const ask_value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                Some(0),
            )
        };

        assert!(
            reply.is_null(),
            "zero-timeout ask should cancel before the target actor runs"
        );
        assert_eq!(REPLY_DISPATCHES.load(Ordering::Relaxed), 0);
        assert_eq!(hew_sched_metrics_global_queue_len(), 1);
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            1,
            "timeout should release only the caller-side ref until the queued message is handled"
        );

        // SAFETY: scheduler is initialized and the queued actor remains valid.
        let remaining = unsafe { crate::bridge::hew_wasm_tick(1) };
        assert_eq!(remaining, 0);
        assert_eq!(REPLY_DISPATCHES.load(Ordering::Relaxed), 1);
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        hew_sched_shutdown();
        // SAFETY: mailbox is no longer referenced after scheduler shutdown.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(replier.mailbox.cast());
            reset_globals();
        }
    }

    #[test]
    fn zero_timeout_wasm_ask_unblocks_promptly_and_mailbox_cleanup_releases_channel() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        let mut replier = stub_actor();
        replier.dispatch = Some(reply_payload_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        replier.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        // Pretend the target is already mid-activation so the zero-timeout ask
        // cannot wake it; this leaves the cancelled request queued for the
        // explicit mailbox teardown path to retire.
        replier
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Relaxed);
        replier.budget.store(1, Ordering::Relaxed);
        let replier_ptr: *mut HewActor = (&raw mut replier).cast();

        let started = std::time::Instant::now();
        // SAFETY: actor remains valid for the duration of the cancelled ask.
        let reply = unsafe {
            crate::actor::actor_ask_wasm_impl(replier_ptr.cast(), 1, ptr::null_mut(), 0, Some(0))
        };
        let elapsed = started.elapsed();

        assert!(
            reply.is_null(),
            "zero-timeout ask should return null before cleanup runs"
        );
        assert!(
            elapsed < std::time::Duration::from_millis(250),
            "zero-timeout ask should unblock promptly (elapsed={elapsed:?})"
        );
        assert_eq!(REPLY_DISPATCHES.load(Ordering::Relaxed), 0);
        assert_eq!(
            hew_sched_metrics_global_queue_len(),
            0,
            "non-runnable actors must not be enqueued while the ask is timing out"
        );
        // SAFETY: mailbox remains owned by this test until the explicit free below.
        unsafe {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_len(replier.mailbox.cast()),
                1,
                "cancelled ask should remain queued until mailbox cleanup retires it"
            );
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            1,
            "only the queued sender-side reply-channel ref should remain live before cleanup"
        );

        // SAFETY: mailbox belongs to this test and is not referenced by the run queue.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_close(replier.mailbox.cast());
            crate::mailbox_wasm::hew_mailbox_free(replier.mailbox.cast());
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            0,
            "mailbox teardown must retire orphaned cancelled asks"
        );

        hew_sched_shutdown();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
    }

    #[test]
    fn unbounded_wasm_ask_cancels_when_no_runnable_work_remains() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        let mut replier = stub_actor();
        replier.dispatch = Some(reply_payload_observes_cancelled_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        replier.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        replier
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Relaxed);
        replier.budget.store(1, Ordering::Relaxed);
        let replier_ptr: *mut HewActor = (&raw mut replier).cast();

        let ask_value = 41i32;
        // SAFETY: actor and payload remain valid for the duration of the ask.
        let reply = unsafe {
            crate::actor::actor_ask_wasm_impl(
                replier_ptr.cast(),
                1,
                (&raw const ask_value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                None,
            )
        };

        assert!(
            reply.is_null(),
            "unbounded ask should return null when no runnable work remains"
        );
        assert_eq!(REPLY_DISPATCHES.load(Ordering::Relaxed), 0);
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            1,
            "returning without a reply should leave only the queued sender-side ref"
        );
        assert!(
            !LATE_REPLY_SAW_CANCELLED.load(Ordering::Relaxed),
            "the deferred dispatch has not run yet"
        );

        replier
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        // SAFETY: actor remains valid for this test.
        unsafe { crate::actor::wake_wasm_actor(replier_ptr.cast()) };

        // SAFETY: scheduler is initialized and the queued actor remains valid.
        let remaining = unsafe { crate::bridge::hew_wasm_tick(1) };
        assert_eq!(remaining, 0);
        assert_eq!(REPLY_DISPATCHES.load(Ordering::Relaxed), 1);
        assert!(
            LATE_REPLY_SAW_CANCELLED.load(Ordering::Relaxed),
            "late repliers should observe the cancelled channel after ask returns null"
        );
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        hew_sched_shutdown();
        // SAFETY: mailbox is no longer referenced after scheduler shutdown.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(replier.mailbox.cast());
            reset_globals();
        }
    }

    #[test]
    fn ask_with_channel_internal_enqueues_idle_actor_and_preserves_reply_channel() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut dispatch_state = AskDispatchState {
            channel: ptr::null_mut(),
            msg_type: 0,
            value: 0,
        };
        let mut actor = stub_actor();
        actor.state = (&raw mut dispatch_state).cast();
        actor.dispatch = Some(reply_with_observed_channel);
        // SAFETY: this test creates and exclusively owns the mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        let ch = crate::reply_channel_wasm::hew_reply_channel_new();
        let value: i32 = 21;
        // SAFETY: actor, channel, and payload are valid for the duration of the test.
        let rc = unsafe {
            crate::actor::ask_with_channel_wasm_internal(
                actor_ptr.cast(),
                7,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                ch.cast(),
            )
        };
        assert_eq!(rc, HewError::Ok as i32);
        assert_eq!(
            // SAFETY: `ch` remains live for the duration of this test.
            unsafe { crate::reply_channel_wasm::test_ref_count(ch) },
            2,
            "queued send must retain the caller-provided reply channel"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32
        );
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(read_queue_len(), 1);
        }

        // SAFETY: mailbox belongs to this test, and the returned node is exclusively owned.
        let msg = unsafe { crate::mailbox_wasm::hew_mailbox_try_recv(actor.mailbox.cast()) };
        assert!(!msg.is_null());
        // SAFETY: simulate the scheduler's reply-channel plumbing for a single message.
        unsafe {
            let dispatch = actor.dispatch.expect("test actor must have a dispatch");
            let msg_ref = &*msg;
            set_current_reply_channel(msg_ref.reply_channel);
            dispatch(
                actor.state,
                msg_ref.msg_type,
                msg_ref.data,
                msg_ref.data_size,
            );
            let _ = clear_current_reply_channel();
            (*msg).reply_channel = ptr::null_mut();
            crate::mailbox_wasm::hew_msg_node_free(msg);
        }

        assert_eq!(
            dispatch_state.channel,
            ch.cast(),
            "dispatch must observe the caller-provided reply channel"
        );
        assert_eq!(dispatch_state.msg_type, 7);
        assert_eq!(dispatch_state.value, value);
        assert_eq!(
            // SAFETY: `ch` remains live until the explicit free below.
            unsafe { crate::reply_channel_wasm::test_ref_count(ch) },
            1,
            "reply delivery must release the queued sender-side retain"
        );

        // SAFETY: reply_take returns a malloc'd pointer or null.
        let reply = unsafe { crate::reply_channel_wasm::reply_take(ch) };
        assert!(!reply.is_null());
        // SAFETY: reply points to an i32 allocated by hew_reply above.
        unsafe {
            assert_eq!(*reply.cast::<i32>(), value * 2);
            libc::free(reply);
            crate::reply_channel_wasm::hew_reply_channel_free(ch);
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
            reset_globals();
        }
    }

    #[test]
    fn ask_with_channel_internal_releases_retained_reply_ref_on_send_failure() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        // SAFETY: this test creates and exclusively owns the mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        // SAFETY: mailbox belongs to this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_close(actor.mailbox.cast()) };

        let ch = crate::reply_channel_wasm::hew_reply_channel_new();
        // SAFETY: `ch` is a live reply channel allocated for this test.
        assert_eq!(unsafe { crate::reply_channel_wasm::test_ref_count(ch) }, 1);

        // SAFETY: actor and channel are valid; closed mailbox forces the failure path.
        let rc = unsafe {
            crate::actor::ask_with_channel_wasm_internal(
                actor_ptr.cast(),
                1,
                ptr::null_mut(),
                0,
                ch.cast(),
            )
        };
        assert_eq!(rc, HewError::ErrActorStopped as i32);
        assert_eq!(
            // SAFETY: `ch` is still live because the test retains ownership.
            unsafe { crate::reply_channel_wasm::test_ref_count(ch) },
            1,
            "failed sends must release the queued sender-side retain"
        );
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(read_queue_len(), 0);
            crate::reply_channel_wasm::hew_reply_channel_free(ch);
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }

        hew_sched_shutdown();
    }

    // ── Terminate-parity tests ──────────────────────────────────────────────

    static TERMINATE_COUNT: AtomicI32 = AtomicI32::new(0);

    unsafe extern "C" fn counting_terminate_fn(_state: *mut c_void) {
        TERMINATE_COUNT.fetch_add(1, Ordering::Relaxed);
    }

    /// Dispatch that self-stops by storing `Stopping` into the actor state.
    ///
    /// `state` is the actor pointer itself — this avoids needing a platform-
    /// specific `hew_actor_self_stop` call (which differs between native and
    /// WASM targets) while still exercising the real scheduler post-activation
    /// `Stopping → Stopped` branch.
    unsafe extern "C" fn self_stopping_dispatch(
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // SAFETY: `state` is a valid `HewActor` pointer set by the test.
        // The actor is in `Running` state during dispatch.
        let actor = state.cast::<HewActor>();
        // SAFETY: actor points to the HewActor passed as state by this test;
        // the actor is valid and in Running state during dispatch.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Stopping as i32, Ordering::Relaxed);
        }
    }

    unsafe extern "C" fn self_stopping_dispatch_via_api(
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // SAFETY: tests pass the live actor pointer itself as `state`.
        unsafe { crate::actor::actor_self_stop_wasm_impl(state.cast::<crate::actor::HewActor>()) };
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn spawn_actor_group_waiter(
        group: *mut crate::actor_group::HewActorGroup,
        timeout_ms: i32,
        ready: Arc<AtomicBool>,
    ) -> std::thread::JoinHandle<i32> {
        let group_addr = group as usize;
        std::thread::spawn(move || {
            ready.store(true, Ordering::Release);
            // SAFETY: the caller keeps the group alive until the waiter joins.
            unsafe {
                crate::actor_group::hew_actor_group_wait_timeout(
                    group_addr as *mut crate::actor_group::HewActorGroup,
                    timeout_ms,
                )
            }
        })
    }

    // Keep the waiter timeout above actor_group's 10 ms polling quantum so a
    // missed condvar wake still gets another state re-check before the test
    // deadline. This narrows Darwin arm64 timing sensitivity without hiding
    // real regressions in the self-stop / closed-mailbox wake paths.
    #[cfg(not(target_arch = "wasm32"))]
    const ACTOR_GROUP_WAITER_TEST_TIMEOUT_MS: i32 = 25;

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn self_stop_closes_mailbox_and_wakes_actor_group_waiters() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        actor.state = actor_ptr.cast::<c_void>();
        actor.dispatch = Some(self_stopping_dispatch_via_api);
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();

        // SAFETY: the actor group is created and destroyed within this test.
        let group = unsafe { crate::actor_group::hew_actor_group_new() };
        assert!(!group.is_null());
        assert_eq!(
            // SAFETY: group and actor are valid for the duration of this test.
            unsafe { crate::actor_group::hew_actor_group_add(group, actor_ptr.cast()) },
            0
        );

        let waiter_ready = Arc::new(AtomicBool::new(false));
        let waiter = spawn_actor_group_waiter(
            group,
            ACTOR_GROUP_WAITER_TEST_TIMEOUT_MS,
            Arc::clone(&waiter_ready),
        );
        while !waiter_ready.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        std::thread::sleep(std::time::Duration::from_millis(1));

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox allocated above.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        hew_sched_run();

        assert_eq!(
            waiter.join().unwrap(),
            0,
            "self-stop finalization must wake actor-group waiters before their timeout expires"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "actor must reach Stopped after self-stop dispatch"
        );
        assert!(
            // SAFETY: mailbox remains live until the explicit free below.
            unsafe { crate::mailbox_wasm::mailbox_is_closed(actor.mailbox.cast()) },
            "self-stop must close the mailbox before scheduler finalization"
        );
        assert_eq!(
            // SAFETY: mailbox remains live and closed at this point.
            unsafe {
                crate::mailbox_wasm::hew_mailbox_send(actor.mailbox.cast(), 7, ptr::null_mut(), 0)
            },
            HewError::ErrClosed as i32,
            "post-stop sends must be rejected once self-stop closes the mailbox"
        );

        // SAFETY: resources were allocated for this test and remain live here.
        unsafe {
            crate::actor_group::hew_actor_group_destroy(group);
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        hew_sched_shutdown();
    }

    /// The WASM scheduler must invoke `terminate_fn` as part of the
    /// `Stopping → Stopped` state transition — not only at
    /// `cleanup_all_actors` / process exit (parity with native scheduler).
    #[test]
    fn terminate_fn_fires_on_stopping_to_stopped_scheduler_path() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        TERMINATE_COUNT.store(0, Ordering::Relaxed);
        hew_sched_init();

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        // Pass the actor itself as state so self_stopping_dispatch can store
        // Stopping during dispatch. Actor starts Runnable (stub default).
        actor.state = actor_ptr.cast::<c_void>();
        actor.dispatch = Some(self_stopping_dispatch);
        actor.terminate_fn = Some(counting_terminate_fn);
        // Give the actor a mailbox with one message so dispatch fires.
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox allocated above; the payload is a
        // stack-local i32 that is copied into the message node by the callee.
        // Queue one message so the dispatch function is actually called.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        hew_sched_run();

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "actor must reach Stopped after scheduler transition"
        );
        assert_eq!(
            TERMINATE_COUNT.load(Ordering::Relaxed),
            1,
            "terminate_fn must fire exactly once on the Stopping→Stopped scheduler path"
        );
        assert!(
            actor.terminate_called.load(Ordering::Acquire),
            "terminate_called guard must be set"
        );
        assert!(
            actor.terminate_finished.load(Ordering::Acquire),
            "terminate_finished guard must be set"
        );

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// `terminate_fn` must not fire a second time if `cleanup_all_actors`
    /// (or any other cleanup path) runs after the scheduler already fired it.
    #[test]
    fn terminate_fn_not_double_invoked_by_cleanup_after_scheduler_stop() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        TERMINATE_COUNT.store(0, Ordering::Relaxed);
        hew_sched_init();

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        actor.state = actor_ptr.cast::<c_void>();
        actor.dispatch = Some(self_stopping_dispatch);
        actor.terminate_fn = Some(counting_terminate_fn);
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox; payload is a stack-local i32.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        hew_sched_run();

        // Scheduler already ran terminate_fn; a second call from a cleanup
        // path (cleanup_all_actors / hew_actor_close) must be a no-op thanks
        // to the `terminate_called` guard inside `call_terminate_fn`.
        // SAFETY: actor is in Stopped state and not being dispatched.
        unsafe {
            crate::actor::call_terminate_fn(actor_ptr.cast::<crate::actor::HewActor>());
        }

        assert_eq!(
            TERMINATE_COUNT.load(Ordering::Relaxed),
            1,
            "terminate_fn must not be invoked twice even when cleanup path runs after scheduler"
        );

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// Post-drain mailbox-closed → `Stopped` parity with native scheduler.
    ///
    /// When the mailbox is closed after all messages are processed the WASM
    /// scheduler must transition the actor `Idle → Stopped` and fire
    /// `terminate_fn`, exactly as the native scheduler does on the same path.
    #[test]
    fn terminate_fn_fires_on_closed_mailbox_idle_to_stopped() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        TERMINATE_COUNT.store(0, Ordering::Relaxed);
        hew_sched_init();

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        // state must be non-null for call_terminate_fn to invoke the callback.
        actor.state = actor_ptr.cast::<c_void>();
        actor.terminate_fn = Some(counting_terminate_fn);
        // Use a no-op dispatch so the actor does not self-stop.
        actor.dispatch = Some(inner_dispatch_noop);
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();

        // Enqueue one message so the actor is activated and reaches Idle.
        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox; payload is a stack-local i32.
        unsafe { queue_wasm_message(actor_ptr, 0) };

        // Close the mailbox *before* running the scheduler.  After dispatch
        // drains the single message the recheck finds no new messages but the
        // mailbox is closed, which should trigger Idle → Stopped.
        // SAFETY: mailbox is valid and exclusively owned by this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_close(actor.mailbox.cast()) };

        hew_sched_run();

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "actor must reach Stopped after closed-mailbox drain (WASM close-path parity)"
        );
        assert_eq!(
            TERMINATE_COUNT.load(Ordering::Relaxed),
            1,
            "terminate_fn must fire exactly once on the closed-mailbox Idle→Stopped path"
        );
        assert!(
            actor.terminate_called.load(Ordering::Acquire),
            "terminate_called guard must be set on close path"
        );
        assert!(
            actor.terminate_finished.load(Ordering::Acquire),
            "terminate_finished guard must be set on close path"
        );

        // SAFETY: mailbox was allocated for this test (closed but not freed).
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn actor_group_wait_timeout_wakes_on_closed_mailbox_idle_to_stopped() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        actor.dispatch = Some(inner_dispatch_noop);
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();

        // SAFETY: the actor group is created and destroyed within this test.
        let group = unsafe { crate::actor_group::hew_actor_group_new() };
        assert!(!group.is_null());
        assert_eq!(
            // SAFETY: group and actor are valid for the duration of this test.
            unsafe { crate::actor_group::hew_actor_group_add(group, actor_ptr.cast()) },
            0
        );

        let waiter_ready = Arc::new(AtomicBool::new(false));
        let waiter = spawn_actor_group_waiter(
            group,
            ACTOR_GROUP_WAITER_TEST_TIMEOUT_MS,
            Arc::clone(&waiter_ready),
        );
        while !waiter_ready.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        std::thread::sleep(std::time::Duration::from_millis(1));

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox allocated above.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        // SAFETY: mailbox is valid and exclusively owned by this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_close(actor.mailbox.cast()) };

        hew_sched_run();

        assert_eq!(
            waiter.join().unwrap(),
            0,
            "closed-mailbox finalization must wake actor-group waiters before their timeout expires"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "actor must reach Stopped after closed-mailbox drain"
        );

        // SAFETY: resources were allocated for this test and remain live here.
        unsafe {
            crate::actor_group::hew_actor_group_destroy(group);
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        hew_sched_shutdown();
    }

    /// Idempotency: `terminate_fn` must not be invoked a second time if a
    /// cleanup path runs after the close-path already fired it.
    #[test]
    fn terminate_fn_not_double_invoked_by_cleanup_after_closed_mailbox_stop() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        TERMINATE_COUNT.store(0, Ordering::Relaxed);
        hew_sched_init();

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        actor.state = actor_ptr.cast::<c_void>();
        actor.terminate_fn = Some(counting_terminate_fn);
        actor.dispatch = Some(inner_dispatch_noop);
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox; payload is a stack-local i32.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        // SAFETY: mailbox is valid and exclusively owned by this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_close(actor.mailbox.cast()) };

        hew_sched_run();

        // Simulate a redundant cleanup call (e.g. hew_actor_close / process
        // shutdown) after the scheduler already finalised the actor.
        // SAFETY: actor is in Stopped state and not being dispatched.
        unsafe {
            crate::actor::call_terminate_fn(actor_ptr.cast::<crate::actor::HewActor>());
        }

        assert_eq!(
            TERMINATE_COUNT.load(Ordering::Relaxed),
            1,
            "terminate_fn must not be invoked twice after closed-mailbox stop"
        );

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    // ── Arena lifecycle parity tests ─────────────────────────────────────
    //
    // These tests verify that activate_actor_wasm installs/restores/resets
    // arenas with the same lifecycle contract as the native scheduler.
    // They compile and run on both native test builds (crate::arena backed
    // by mmap/VirtualAlloc) and wasm32 (crate::arena backed by arena_wasm,
    // which uses std::alloc).

    /// `hew_arena_malloc` must route through the actor's arena during
    /// dispatch and fall back to libc malloc once activation finishes.
    #[test]
    fn arena_is_installed_during_dispatch_and_cleared_after() {
        // Items must precede all statements to satisfy clippy::items_after_statements.
        static ARENA_DURING_DISPATCH: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn capture_arena_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            // Record current arena (as usize) via the internal getter.
            let ptr = crate::arena::set_current_arena(ptr::null_mut()); // read-then-restore
            crate::arena::set_current_arena(ptr); // put it back
            ARENA_DURING_DISPATCH.store(ptr as usize, std::sync::atomic::Ordering::Relaxed);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Create a real arena so we can detect whether alloc routes through it.
        let actor_arena = crate::arena::hew_arena_new();
        assert!(!actor_arena.is_null(), "arena creation must succeed");

        let mut actor = stub_actor();
        actor.dispatch = Some(capture_arena_dispatch);
        actor.arena = actor_arena.cast::<c_void>();
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox.
        unsafe { queue_wasm_message(actor_ptr, 0) };

        // Verify no arena is active before running.
        assert_eq!(
            crate::arena::set_current_arena(ptr::null_mut()) as usize,
            0,
            "no arena should be active before activation"
        );

        hew_sched_run();

        // Dispatch must have seen the actor's arena.
        assert_eq!(
            ARENA_DURING_DISPATCH.load(std::sync::atomic::Ordering::Relaxed),
            actor_arena as usize,
            "dispatch must run with the actor's arena installed"
        );

        // After activation the current arena must be restored to null.
        let post_arena = crate::arena::set_current_arena(ptr::null_mut());
        assert!(
            post_arena.is_null(),
            "current arena must be null after activation completes"
        );

        // SAFETY: mailbox and arena were allocated for this test.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
            crate::arena::hew_arena_free_all(actor_arena);
        }
        hew_sched_shutdown();
    }

    /// After activation the arena cursor must be reset to zero (ready for
    /// the next dispatch cycle).
    #[test]
    fn arena_is_reset_after_activation() {
        // Items must precede all statements to satisfy clippy::items_after_statements.
        // Dispatch allocates from the arena so the cursor advances.
        unsafe extern "C" fn alloc_in_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            // SAFETY: arena is installed by the scheduler before dispatch.
            unsafe { crate::arena::hew_arena_malloc(64) };
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor_arena = crate::arena::hew_arena_new();
        assert!(!actor_arena.is_null());

        let mut actor = stub_actor();
        actor.dispatch = Some(alloc_in_dispatch);
        actor.arena = actor_arena.cast::<c_void>();
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        hew_sched_run();

        // After activation the arena must be reset: allocating again should
        // return the same base pointer as the very first allocation would.
        // We verify by allocating from the (now-reset) arena directly and
        // confirming the cursor is back at the start.
        // SAFETY: arena is valid and not currently installed.
        let p1 = unsafe { (*actor_arena).alloc(1, 1) };
        assert!(!p1.is_null(), "post-reset alloc must succeed");
        // SAFETY: same arena — reset once more so subsequent tests are clean.
        unsafe { (*actor_arena).reset() };
        // SAFETY: arena is valid and cursor is at zero after reset.
        let p2 = unsafe { (*actor_arena).alloc(1, 1) };
        // Both allocations from a freshly-reset arena share the same base.
        assert_eq!(p1, p2, "arena cursor must be at zero after reset");

        // SAFETY: mailbox and arena were allocated for this test.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
            crate::arena::hew_arena_free_all(actor_arena);
        }
        hew_sched_shutdown();
    }

    /// Nested (re-entrant) activation must restore the outer actor's arena
    /// when the inner activation completes.
    #[test]
    fn arena_restored_on_reentrant_activation() {
        // Items must precede all statements to satisfy clippy::items_after_statements.

        // We record what arena was active after the inner activation returns.
        static OUTER_POST_DISPATCH: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);

        // Outer dispatch: enqueues and runs the inner actor inline (simulating
        // re-entrant activation through hew_actor_ask / hew_sched_run).
        unsafe extern "C" fn outer_dispatch(
            state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            // SAFETY: state was set to a valid *mut HewActor pointer by the test.
            let inner: *mut HewActor = unsafe { *state.cast::<*mut HewActor>() };
            // SAFETY: inner is a valid live actor; sched_enqueue and hew_sched_run
            // are safe to call from within a dispatch on the same single thread.
            unsafe {
                sched_enqueue(inner);
                queue_wasm_message_static(inner, 0);
                // Run the inner actor inline — this is the re-entrant path.
                hew_sched_run();
            }
            // After inner activation completes, current arena must be outer's.
            let current = crate::arena::set_current_arena(ptr::null_mut());
            crate::arena::set_current_arena(current); // restore
            OUTER_POST_DISPATCH.store(current as usize, std::sync::atomic::Ordering::Relaxed);
        }

        // We can't call `queue_wasm_message` (which uses a local static) from
        // inside an extern "C" fn body, so we need a plain-fn wrapper.
        unsafe fn queue_wasm_message_static(actor: *mut HewActor, value: i32) {
            let mut payload = value;
            // SAFETY: actor is a valid live actor with an initialized mailbox.
            let rc = unsafe {
                crate::mailbox_wasm::hew_mailbox_send(
                    (*actor).mailbox.cast(),
                    1,
                    (&raw mut payload).cast(),
                    std::mem::size_of::<i32>(),
                )
            };
            let _ = rc; // ignore error in test helper
                        // SAFETY: actor is a valid live WASM actor.
            unsafe { crate::actor::wake_wasm_actor(actor.cast::<crate::actor::HewActor>()) };
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Two separate arenas — one per actor.
        let outer_arena = crate::arena::hew_arena_new();
        let inner_arena = crate::arena::hew_arena_new();
        assert!(!outer_arena.is_null() && !inner_arena.is_null());

        // Inner actor: simple no-op dispatch (no dispatch fn set → messages freed).
        let mut inner_actor = stub_actor();
        inner_actor.id = 2;
        inner_actor.arena = inner_arena.cast::<c_void>();
        // SAFETY: test exclusively owns this mailbox.
        inner_actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        let mut inner_ptr: *mut HewActor = (&raw mut inner_actor).cast();

        let mut outer_actor = stub_actor();
        outer_actor.id = 1;
        outer_actor.arena = outer_arena.cast::<c_void>();
        outer_actor.dispatch = Some(outer_dispatch);
        // Pass inner_ptr via state so outer_dispatch can enqueue it.
        outer_actor.state = (&raw mut inner_ptr).cast::<c_void>();
        // SAFETY: test exclusively owns this mailbox.
        outer_actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        let outer_ptr: *mut HewActor = (&raw mut outer_actor).cast();

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(outer_ptr) };
        // SAFETY: actor has a valid mailbox.
        unsafe { queue_wasm_message(outer_ptr, 0) };
        hew_sched_run();

        // After the inner activation finished and returned to outer_dispatch,
        // outer_dispatch must have seen *outer*'s arena still installed.
        assert_eq!(
            OUTER_POST_DISPATCH.load(std::sync::atomic::Ordering::Relaxed),
            outer_arena as usize,
            "outer actor's arena must be active when inner activation returns"
        );

        // After everything, no arena must be active.
        let post = crate::arena::set_current_arena(ptr::null_mut());
        assert!(
            post.is_null(),
            "current arena must be null after all activations complete"
        );

        // SAFETY: mailboxes and arenas were allocated for this test.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(outer_actor.mailbox.cast());
            crate::mailbox_wasm::hew_mailbox_free(inner_actor.mailbox.cast());
            crate::arena::hew_arena_free_all(outer_arena);
            crate::arena::hew_arena_free_all(inner_arena);
        }
        hew_sched_shutdown();
    }

    /// With null arena (current WASM default), activation is a no-op for
    /// arena lifecycle but must not leave any arena installed.
    #[test]
    fn null_arena_activation_leaves_no_arena_installed() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor(); // arena field is ptr::null_mut()
                                      // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.dispatch = Some(noisy_dispatch);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        hew_sched_run();

        let post = crate::arena::set_current_arena(ptr::null_mut());
        assert!(
            post.is_null(),
            "null arena actor must leave current arena as null after activation"
        );

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// `actor_ask_wasm_impl` with a generous wall-clock deadline returns the
    /// reply when the actor dispatches within the first scheduler tick.
    ///
    /// Coverage note: this exercises the `Some(timeout_ms)` branch of
    /// `actor_ask_wasm_impl` on the success path (deadline does not expire
    /// before the reply arrives).  The complementary failure branch
    /// (`bounded_wasm_ask_timeout_cancels_before_target_activation`) covers
    /// `Some(0)`. The actual-target wasm32-wasip1 libtest lane now exercises
    /// both branches directly.
    #[test]
    fn wasm_ask_with_generous_timeout_returns_reply_when_actor_is_fast() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access to shared globals.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();
        assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

        let mut replier = stub_actor();
        replier.dispatch = Some(reply_payload_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        replier.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        replier
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        replier.budget.store(1, Ordering::Relaxed);
        let replier_ptr: *mut HewActor = (&raw mut replier).cast();

        let ask_value = 13i32;
        // SAFETY: actor and payload remain valid for the duration of the ask.
        let reply = unsafe {
            crate::actor::actor_ask_wasm_impl(
                replier_ptr.cast(),
                1,
                (&raw const ask_value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                Some(5_000), // 5-second deadline — the fast actor replies in one tick
            )
        };

        assert!(
            !reply.is_null(),
            "ask_timeout with a generous deadline should return the actor's reply"
        );
        // SAFETY: reply is an i32 payload allocated by hew_reply in reply_payload_dispatch.
        unsafe {
            assert_eq!(
                *reply.cast::<i32>(),
                ask_value,
                "reply payload must match the sent value"
            );
            libc::free(reply);
        }
        assert_eq!(
            REPLY_DISPATCHES.load(Ordering::Relaxed),
            1,
            "dispatch must run exactly once"
        );
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            0,
            "successful timed WASM ask must leave no live reply channels"
        );

        hew_sched_shutdown();
        // SAFETY: mailbox is no longer referenced after scheduler shutdown.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(replier.mailbox.cast());
            reset_globals();
        }
    }

    // ── Budget / reduction-enforcement tests ─────────────────────────────

    /// Violation counter: incremented by `dispatch_check_reductions` whenever
    /// the reductions field is not `HEW_DEFAULT_REDUCTIONS` at dispatch entry.
    static REDUCTIONS_WRONG_COUNT: AtomicI32 = AtomicI32::new(0);

    /// Dispatch that verifies the reductions field is reset before each call.
    ///
    /// Expects `state` to point to the owning `HewActor` — set by the test via
    /// `actor.state = actor_ptr.cast()` so that this function can read the
    /// field directly without requiring a global actor slot.
    unsafe extern "C" fn dispatch_check_reductions(
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // SAFETY: state was set to a valid *mut HewActor by the test.
        let a = unsafe { &*state.cast::<HewActor>() };
        if a.reductions.load(Ordering::Relaxed) != HEW_DEFAULT_REDUCTIONS {
            REDUCTIONS_WRONG_COUNT.fetch_add(1, Ordering::Relaxed);
        }
        NOISY_DISPATCHES.fetch_add(1, Ordering::Relaxed);
    }

    /// Queue `count` messages directly into an actor's mailbox without calling
    /// `wake_wasm_actor`.  Use when the actor is already `Runnable` (or when
    /// the caller will enqueue it explicitly) to avoid double-enqueue.
    ///
    /// # Safety
    ///
    /// `actor` must be a valid pointer to a `HewActor` whose mailbox is live.
    unsafe fn queue_messages_only(actor: *mut HewActor, count: usize) {
        // Fixed payload — dispatch callbacks only care about message count.
        let mut payload: i32 = 0;
        for i in 0..count {
            // SAFETY: actor and its mailbox are valid; payload outlives the call.
            let rc = unsafe {
                crate::mailbox_wasm::hew_mailbox_send(
                    (*actor).mailbox.cast(),
                    1,
                    (&raw mut payload).cast(),
                    std::mem::size_of::<i32>(),
                )
            };
            assert_eq!(
                rc,
                HewError::Ok as i32,
                "queue_messages_only: send #{i} failed"
            );
        }
    }

    /// An actor with `budget=3` and 5 queued messages must process exactly 3
    /// messages per activation, leaving 2 in the mailbox.
    #[test]
    fn budget_enforces_message_cap_per_activation() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();

        let mut actor = stub_actor();
        actor.dispatch = Some(noisy_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.budget.store(3, Ordering::Relaxed);
        // stub_actor() starts Runnable; queue_messages_only skips wake so no
        // double-enqueue occurs.
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // Queue 5 messages without waking (actor is already Runnable).
        // SAFETY: actor and mailbox are valid.
        unsafe { queue_messages_only(actor_ptr, 5) };

        // One activation: must consume exactly budget=3 messages.
        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            NOISY_DISPATCHES.load(Ordering::Relaxed),
            3,
            "budget=3 must dispatch exactly 3 messages per activation"
        );
        // 2 messages remain → actor must be Runnable (not Idle) after activation.
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor with remaining messages must be Runnable after a budget-capped activation"
        );
        // SAFETY: mailbox is still live; no one else holds a reference.
        unsafe {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_len(actor.mailbox.cast()),
                2,
                "2 messages must remain in the mailbox after budget-capped activation"
            );
        }

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// High-priority scaling: `budget=4` with `HEW_PRIORITY_HIGH` (2× factor)
    /// must dispatch 8 messages per activation.
    #[test]
    fn high_priority_doubles_effective_budget() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();

        let mut actor = stub_actor();
        actor.dispatch = Some(noisy_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.budget.store(4, Ordering::Relaxed);
        actor.priority.store(HEW_PRIORITY_HIGH, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // 10 messages — more than the 8 the scaled budget allows.
        // SAFETY: actor and mailbox are valid.
        unsafe { queue_messages_only(actor_ptr, 10) };

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            NOISY_DISPATCHES.load(Ordering::Relaxed),
            8,
            "HIGH priority with budget=4 must dispatch 4×2=8 messages per activation"
        );
        // SAFETY: mailbox is still live.
        unsafe {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_len(actor.mailbox.cast()),
                2,
                "2 messages must remain after high-priority activation"
            );
        }

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// Low-priority scaling: `budget=4` with `HEW_PRIORITY_LOW` (÷2 factor)
    /// must dispatch 2 messages per activation.
    #[test]
    fn low_priority_halves_effective_budget() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();

        let mut actor = stub_actor();
        actor.dispatch = Some(noisy_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.budget.store(4, Ordering::Relaxed);
        actor.priority.store(HEW_PRIORITY_LOW, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // 10 messages — more than the 2 the scaled budget allows.
        // SAFETY: actor and mailbox are valid.
        unsafe { queue_messages_only(actor_ptr, 10) };

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            NOISY_DISPATCHES.load(Ordering::Relaxed),
            2,
            "LOW priority with budget=4 must dispatch 4÷2=2 messages per activation"
        );
        // SAFETY: mailbox is still live.
        unsafe {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_len(actor.mailbox.cast()),
                8,
                "8 messages must remain after low-priority activation"
            );
        }

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// Low-priority floor: `budget=1` with `HEW_PRIORITY_LOW` yields
    /// `max(1÷2, 1) = 1`, so exactly 1 message is dispatched.
    #[test]
    fn low_priority_budget_floor_is_one() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();

        let mut actor = stub_actor();
        actor.dispatch = Some(noisy_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.budget.store(1, Ordering::Relaxed);
        actor.priority.store(HEW_PRIORITY_LOW, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // SAFETY: actor and mailbox are valid.
        unsafe { queue_messages_only(actor_ptr, 3) };

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            NOISY_DISPATCHES.load(Ordering::Relaxed),
            1,
            "LOW priority with budget=1 must dispatch exactly 1 message (floor clamps 0 to 1)"
        );
        // SAFETY: mailbox is still live.
        unsafe {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_len(actor.mailbox.cast()),
                2,
                "2 messages must remain after the single-message floor activation"
            );
        }

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// Zero budget falls back to `HEW_MSG_BUDGET` (256): a mailbox with fewer
    /// than 256 messages must be fully drained in a single activation.
    #[test]
    fn zero_budget_falls_back_to_default_msg_budget() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();

        let mut actor = stub_actor();
        actor.dispatch = Some(noisy_dispatch);
        // SAFETY: test creates and exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        // budget=0 → activate_actor_wasm falls back to HEW_MSG_BUDGET (256).
        actor.budget.store(0, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // 5 messages — well within the 256-message fallback budget.
        // SAFETY: actor and mailbox are valid.
        unsafe { queue_messages_only(actor_ptr, 5) };

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            NOISY_DISPATCHES.load(Ordering::Relaxed),
            5,
            "zero budget must fall back to HEW_MSG_BUDGET and drain all 5 messages"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "actor must be Idle when all messages are drained under the fallback budget"
        );

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// The scheduler resets the reduction counter to `HEW_DEFAULT_REDUCTIONS`
    /// before every dispatch call, regardless of how many messages the actor
    /// has already processed in the current activation.
    ///
    /// Coverage note: the actual-target wasm32-wasip1 libtest lane now covers
    /// this reduction-reset invariant directly.
    #[test]
    fn reductions_reset_to_default_per_dispatch() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        reset_wasm_dispatch_counters();
        REDUCTIONS_WRONG_COUNT.store(0, Ordering::Relaxed);

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();
        // Pass the actor pointer as state so dispatch_check_reductions can
        // inspect the reductions field without a separate global.
        actor.state = actor_ptr.cast::<c_void>();
        actor.dispatch = Some(dispatch_check_reductions);
        // SAFETY: test creates and exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.budget.store(3, Ordering::Relaxed);

        // SAFETY: actor and mailbox are valid.
        unsafe { queue_messages_only(actor_ptr, 3) };

        // SAFETY: actor is valid and scheduler is initialized.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            NOISY_DISPATCHES.load(Ordering::Relaxed),
            3,
            "dispatch must run exactly 3 times (one per queued message)"
        );
        assert_eq!(
            REDUCTIONS_WRONG_COUNT.load(Ordering::Relaxed),
            0,
            "reductions must equal HEW_DEFAULT_REDUCTIONS at the start of every dispatch call"
        );

        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// A spawned actor's arena must be non-null, installed as the current arena
    /// during every dispatch cycle, and freed cleanly when the actor is torn down.
    ///
    /// This test exercises the three new invariants introduced by the spawn-path
    /// arena allocation:
    ///   1. `spawn_actor_internal` (WASM) now calls `hew_arena_new()` — the arena
    ///      pointer on a fresh actor is non-null.
    ///   2. The scheduler installs that arena as the current arena before calling
    ///      dispatch and restores the previous arena afterwards.
    ///   3. `free_actor_resources_wasm` frees the arena when the pointer is
    ///      non-null, mirroring the native teardown path.
    #[test]
    fn spawn_path_arena_is_installed_during_dispatch_and_freed_on_teardown() {
        // Items before statements required by clippy::items_after_statements.
        static ARENA_SEEN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn capture_arena_ptr(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            // Peek at the current arena without permanently clearing it.
            let ptr = crate::arena::set_current_arena(ptr::null_mut());
            crate::arena::set_current_arena(ptr); // restore
            ARENA_SEEN.store(ptr as usize, std::sync::atomic::Ordering::Relaxed);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // ── 1. Allocate an arena exactly as the updated spawn path does ──────
        let arena = crate::arena::hew_arena_new();
        assert!(
            !arena.is_null(),
            "hew_arena_new must succeed (spawn-path precondition)"
        );

        // ── 2. Wire up a heap-allocated actor with that arena ─────────────────
        //      We use Box::into_raw so that free_actor_resources_wasm can
        //      reclaim it via Box::from_raw at teardown.
        // SAFETY: hew_mailbox_new has no preconditions; returns an owned pointer.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        assert!(!mailbox.is_null(), "mailbox allocation must succeed");

        let actor = Box::into_raw(Box::new(HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 99,
            pid: 99,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(capture_arena_ptr),
            mailbox: mailbox.cast(),
            actor_state: AtomicI32::new(HewActorState::Runnable as i32),
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
            // Assign the arena just as spawn_actor_internal now does.
            arena: arena.cast::<c_void>(),
        }));

        // ── 3. Enqueue one message and run dispatch ───────────────────────────
        // SAFETY: actor points to a live test actor allocated above and is valid
        // to enqueue on the single-threaded test scheduler.
        unsafe { sched_enqueue(actor) };
        // SAFETY: actor remains live and queue_wasm_message requires a valid actor
        // pointer plus a trivially copyable i32 payload.
        unsafe { queue_wasm_message(actor, 0) };

        hew_sched_run();

        // Dispatch must have seen the actor's own arena as the current arena.
        assert_eq!(
            ARENA_SEEN.load(std::sync::atomic::Ordering::Relaxed),
            arena as usize,
            "dispatch must run with the spawn-path arena installed"
        );

        // The current arena must have been restored to null after activation.
        let post_run_arena = crate::arena::set_current_arena(ptr::null_mut());
        assert!(
            post_run_arena.is_null(),
            "current arena must be null after activation completes"
        );

        // ── 4. Teardown via free_actor_resources_wasm ─────────────────────────
        //      This exercises the new null-checked arena free path.  A crash or
        //      double-free here would surface under ASAN / Valgrind.
        //
        //      The two HewActor types (scheduler_wasm::HewActor and
        //      actor::HewActor) are layout-identical — verified by the
        //      compile-time offset assertions above the struct definition —
        //      so the cast is valid.
        // SAFETY: actor is Box-allocated, not being dispatched, and the arena +
        // mailbox are both valid.  state / init_state are null so libc::free(null)
        // is a no-op.
        unsafe { crate::actor::free_actor_resources_wasm(actor.cast::<crate::actor::HewActor>()) };

        hew_sched_shutdown();
    }

    // ── hew_actor_cooperate tests ───────────────────────────────────────

    #[test]
    fn cooperate_outside_dispatch_is_noop() {
        // When no actor is being dispatched, cooperate must return 0 (no-op).
        let result = hew_actor_cooperate();
        assert_eq!(result, 0, "cooperate outside dispatch must return 0");
    }

    #[test]
    fn cooperate_decrements_reductions_and_returns_zero_when_budget_remains() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        actor.reductions.store(100, Ordering::Relaxed);

        // Install the actor as the current dispatch actor.
        let prev =
            crate::actor::set_current_actor((&raw mut actor).cast::<crate::actor::HewActor>());

        let result = hew_actor_cooperate();
        assert_eq!(result, 0, "cooperate must return 0 when budget remains");
        assert_eq!(
            actor.reductions.load(Ordering::Relaxed),
            99,
            "cooperate must decrement reductions by 1"
        );

        crate::actor::set_current_actor(prev);
        hew_sched_shutdown();
    }

    #[test]
    fn cooperate_yields_and_resets_when_budget_exhausted() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        // Set reductions to 1 so the next cooperate exhausts the budget.
        actor.reductions.store(1, Ordering::Relaxed);

        let prev =
            crate::actor::set_current_actor((&raw mut actor).cast::<crate::actor::HewActor>());

        let result = hew_actor_cooperate();
        assert_eq!(result, 1, "cooperate must return 1 when budget exhausted");
        assert_eq!(
            actor.reductions.load(Ordering::Relaxed),
            HEW_DEFAULT_REDUCTIONS,
            "cooperate must reset reductions to default after yield"
        );

        crate::actor::set_current_actor(prev);
        hew_sched_shutdown();
    }

    #[test]
    fn cooperate_at_exactly_zero_reductions_yields() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        // Edge case: reductions already at 0 (fetch_sub wraps to -1 < 1).
        actor.reductions.store(0, Ordering::Relaxed);

        let prev =
            crate::actor::set_current_actor((&raw mut actor).cast::<crate::actor::HewActor>());

        let result = hew_actor_cooperate();
        assert_eq!(result, 1, "cooperate at zero reductions must yield");
        assert_eq!(
            actor.reductions.load(Ordering::Relaxed),
            HEW_DEFAULT_REDUCTIONS,
            "cooperate must reset reductions after yield at zero"
        );

        crate::actor::set_current_actor(prev);
        hew_sched_shutdown();
    }

    // ── sched_enqueue fail-closed tests ─────────────────────────────────
    //
    // On wasm32-wasip1 panics abort the binary (no unwinding), so
    // `#[should_panic]` and `catch_unwind` are unusable.  We verify the
    // fail-closed semantics on all targets via the fallible
    // `try_sched_enqueue` helper, and additionally confirm the panic
    // wrapper on host targets where unwinding is available.

    /// Verify that the fallible path returns `Err` when the scheduler is
    /// not initialized — works on every target including wasm.
    #[test]
    fn try_enqueue_returns_err_when_run_queue_is_none() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        // Do NOT call hew_sched_init — RUN_QUEUE stays None.

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: actor is valid; we expect Err, not UB.
        let result = unsafe { try_sched_enqueue(actor_ptr) };
        assert!(
            result.is_err(),
            "try_sched_enqueue must return Err when RUN_QUEUE is None"
        );
    }

    /// Verify that `TASKS_SPAWNED` is not incremented when enqueue fails.
    #[test]
    fn try_enqueue_does_not_increment_tasks_spawned_on_failure() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        // RUN_QUEUE is None — try_sched_enqueue should return Err.

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: actor is valid.
        let _ = unsafe { try_sched_enqueue(actor_ptr) };

        // TASKS_SPAWNED must not have been incremented.
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                read_tasks_spawned(),
                0,
                "TASKS_SPAWNED must remain 0 when enqueue fails"
            );
        }
    }

    /// On hosts that support unwinding, also confirm the public wrapper panics.
    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[should_panic(expected = "scheduler not initialized")]
    fn enqueue_panics_when_run_queue_is_none() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: actor is valid; the test expects panic.
        unsafe { sched_enqueue(actor_ptr) };
    }

    #[test]
    fn enqueue_succeeds_when_scheduler_initialized() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };

        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                read_tasks_spawned(),
                1,
                "TASKS_SPAWNED must be 1 after successful enqueue"
            );
            assert_eq!(read_queue_len(), 1, "queue must contain the enqueued actor");
        }

        hew_sched_shutdown();
    }

    // ── Cooperative tick recursion bound tests ──────────────────────────

    #[test]
    fn cooperate_skips_tick_at_max_depth() {
        // When COOPERATIVE_TICK_DEPTH is at the maximum, hew_actor_cooperate
        // must NOT call hew_wasm_sched_tick (to avoid stack overflow), but
        // must still return 1 to signal a yield.
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        // Set reductions to 1 so the next cooperate exhausts the budget.
        actor.reductions.store(1, Ordering::Relaxed);

        let prev =
            crate::actor::set_current_actor((&raw mut actor).cast::<crate::actor::HewActor>());

        // Simulate being at the maximum depth.
        // SAFETY: Single-threaded test; ptr::addr_of_mut! avoids references.
        unsafe {
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(MAX_COOPERATIVE_TICK_DEPTH);
        }

        // Enqueue a second actor so the queue is non-empty (if the tick
        // were called it would drain one).
        let other = stub_actor();
        let other_ptr: *mut HewActor = (&raw const other).cast_mut();
        // SAFETY: valid actor, scheduler initialized.
        unsafe { sched_enqueue(other_ptr) };

        let result = hew_actor_cooperate();
        assert_eq!(result, 1, "cooperate must return 1 (yielded) at max depth");

        // The queued actor must NOT have been activated — cooperate skipped
        // the tick entirely.
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                read_queue_len(),
                1,
                "cooperate at max depth must not drive the scheduler"
            );
        }

        // Reset depth so shutdown can drain properly.
        // SAFETY: Single-threaded test.
        unsafe {
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(0);
        }

        crate::actor::set_current_actor(prev);
        hew_sched_shutdown();
    }

    #[test]
    fn sched_tick_makes_progress_at_high_depth() {
        // Regression test for the depth-cap no-progress spin: even when
        // COOPERATIVE_TICK_DEPTH is at MAX, hew_wasm_sched_tick must still
        // run actors and make forward progress. Wait-loop callers
        // (ask/await/reply) depend on this to avoid infinite spinning.
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Simulate being at the maximum depth.
        // SAFETY: Single-threaded test; ptr::addr_of_mut! avoids references.
        unsafe {
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(MAX_COOPERATIVE_TICK_DEPTH);
        }

        // Enqueue an actor so the queue is non-empty.
        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: valid actor, scheduler initialized.
        unsafe { sched_enqueue(actor_ptr) };

        // hew_wasm_sched_tick must still run the actor (queue drains to 0).
        // SAFETY: scheduler is initialized.
        let remaining = unsafe { hew_wasm_sched_tick(10) };
        assert_eq!(
            remaining, 0,
            "sched_tick must make progress regardless of depth (wait loops depend on this)"
        );

        // Queue must be empty — the actor was activated.
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                read_queue_len(),
                0,
                "actor must have been activated even at max depth"
            );
        }

        // Reset depth so shutdown can drain properly.
        // SAFETY: Single-threaded test.
        unsafe {
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(0);
        }

        hew_sched_shutdown();
    }

    #[test]
    fn wait_loop_returns_zero_when_queue_drains_at_max_depth() {
        // Simulates the exact scenario that caused the blocker: a wait-loop
        // caller calls hew_wasm_sched_tick at max cooperate depth with a
        // non-empty queue. The tick must drain the queue and return 0,
        // allowing the wait loop to exit cleanly instead of spinning.
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Set depth to MAX to simulate deep cooperate nesting.
        // SAFETY: Single-threaded test.
        unsafe {
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(MAX_COOPERATIVE_TICK_DEPTH);
        }

        // Enqueue one actor with no dispatch (will be dequeued and its
        // mailbox check will find nothing, returning it to idle).
        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        // SAFETY: valid actor, scheduler initialized.
        unsafe { sched_enqueue(actor_ptr) };

        // This mimics what a wait loop does: call tick, check return value.
        // With the old bug, this would return 1 (queue length) without doing
        // work, and the wait loop would spin forever.
        // SAFETY: scheduler is initialized.
        let remaining = unsafe { hew_wasm_sched_tick(1) };

        // The tick must have consumed the actor, not returned early.
        assert_eq!(
            remaining, 0,
            "tick must drain queue at max depth — returning nonzero without progress causes \
             wait-loop spins"
        );

        // SAFETY: Single-threaded test.
        unsafe {
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(0);
        }

        hew_sched_shutdown();
    }

    #[test]
    fn tick_depth_increments_and_decrements() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Verify depth starts at 0.
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(COOPERATIVE_TICK_DEPTH).read(),
                0,
                "depth must start at 0"
            );
        }

        // Call tick with empty queue — depth should be 0 after (incremented then
        // decremented within the call).
        // SAFETY: scheduler is initialized.
        let _ = unsafe { hew_wasm_sched_tick(1) };

        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(COOPERATIVE_TICK_DEPTH).read(),
                0,
                "depth must return to 0 after tick completes"
            );
        }

        hew_sched_shutdown();
    }

    // ── WASM sleeping-actor timer tests ─────────────────────────────────

    /// `request_sleep` stores the deadline; a later call with a smaller
    /// deadline is ignored; a call with a larger deadline replaces it.
    #[test]
    fn request_sleep_takes_latest_deadline() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        request_sleep(100);
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read(),
                100,
                "first request should be stored"
            );
        }

        request_sleep(50); // smaller — ignored
                           // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read(),
                100,
                "smaller deadline should be ignored"
            );
        }

        request_sleep(200); // larger — replaces
                            // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read(),
                200,
                "larger deadline should replace"
            );
        }

        // Cleanup
        // SAFETY: Single-threaded test; no concurrent readers.
        unsafe { ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(0) };
    }

    /// `drain_expired_sleepers` re-enqueues actors whose deadline has passed
    /// and leaves actors whose deadline is still in the future.
    #[test]
    fn drain_expired_sleepers_wakes_ready_actors() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        let mut b = stub_actor();
        b.id = 2;
        let b_ptr: *mut HewActor = (&raw mut b);
        b.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // Park actor `a` at t=100 and actor `b` at t=300.
        // SAFETY: actors are valid for the duration of the test.
        unsafe {
            park_actor_sleep(a_ptr, 100);
            park_actor_sleep(b_ptr, 300);
            assert_eq!(
                hew_wasm_sleeping_count(),
                2,
                "both actors should be sleeping"
            );
        }

        // Advance to t=200: only `a` should wake.
        // SAFETY: Single-threaded test.
        let woken = unsafe { drain_expired_sleepers(200) };
        assert_eq!(woken, 1, "only actor a should wake at t=200");
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor a should be Runnable after wake"
        );
        assert_eq!(
            b.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32,
            "actor b should remain Sleeping before its deadline"
        );
        // SAFETY: Single-threaded test.
        assert_eq!(hew_wasm_sleeping_count(), 1, "one actor still sleeping");
        assert_eq!(
            hew_sched_metrics_global_queue_len(),
            1,
            "actor a should be in run queue"
        );

        // Advance to t=400: `b` should wake.
        // SAFETY: Single-threaded test.
        let woken = unsafe { drain_expired_sleepers(400) };
        assert_eq!(woken, 1, "actor b should wake at t=400");
        assert_eq!(
            b.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor b should be Runnable after wake"
        );
        assert_eq!(hew_wasm_sleeping_count(), 0, "no actors sleeping");

        hew_sched_shutdown();
    }

    /// `drain_expired_sleepers` silently discards stopped/crashed actors.
    #[test]
    fn drain_expired_sleepers_discards_terminal_actors() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // SAFETY: actor is valid for the duration of the test.
        unsafe { park_actor_sleep(a_ptr, 50) };

        // Mark the actor as stopped before the timer fires.
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);

        // SAFETY: Single-threaded test.
        let woken = unsafe { drain_expired_sleepers(100) };
        assert_eq!(woken, 0, "stopped actor should be discarded, not woken");
        assert_eq!(hew_wasm_sleeping_count(), 0, "sleep queue should be empty");
        assert_eq!(
            hew_sched_metrics_global_queue_len(),
            0,
            "run queue should be empty"
        );

        hew_sched_shutdown();
    }

    /// A dispatch that calls `request_sleep` causes the actor to be parked
    /// after the message boundary, not re-enqueued as Runnable.
    #[test]
    fn actor_is_parked_after_sleep_request_in_dispatch() {
        // Declare items before any statements to satisfy `items_after_statements`.
        static DISPATCHED: AtomicI32 = AtomicI32::new(0);
        // SAFETY: `hew_now_ms` is safe to call from dispatch; `request_sleep`
        // is designed to be called from within a dispatch handler.
        unsafe extern "C" fn sleeping_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            DISPATCHED.fetch_add(1, Ordering::Relaxed);
            // Simulate sleep_ms(500): record a deadline 500 ms from now.
            // In simulated time: now=0, so deadline=500.
            // SAFETY: hew_now_ms is safe to call from within dispatch.
            let now = unsafe { hew_now_ms() };
            request_sleep(now + 500);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        // Use simulated time so we can control deadlines deterministically.
        crate::deterministic::hew_simtime_enable(0);

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.dispatch = Some(sleeping_dispatch);
        a.mailbox = mailbox.cast();
        let a_ptr: *mut HewActor = (&raw mut a);

        // Enqueue actor in scheduler, then send it a message.
        // SAFETY: actor and mailbox are valid.
        unsafe { sched_enqueue(a_ptr) };
        // SAFETY: actor has a valid mailbox.
        unsafe { queue_wasm_message(a_ptr, 42) };

        // Run one tick.
        // SAFETY: Single-threaded test.
        let _ = unsafe { hew_wasm_sched_tick(1) };

        assert_eq!(
            DISPATCHED.load(Ordering::Relaxed),
            1,
            "dispatch should have run once"
        );
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32,
            "actor should be Sleeping (parked) after sleep request"
        );
        assert_eq!(
            hew_wasm_sleeping_count(),
            1,
            "actor should be in sleep queue"
        );

        // Advance simulated time to t=499: actor should NOT wake yet.
        crate::deterministic::hew_simtime_advance_ms(499);
        // SAFETY: Single-threaded test.
        let _ = unsafe { hew_wasm_sched_tick(1) };
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32,
            "actor should still be Sleeping at t=499"
        );

        // Advance to t=500 (exactly the deadline): actor should wake.
        crate::deterministic::hew_simtime_advance_ms(1);
        let now_ms = crate::deterministic::hew_simtime_now_ms();
        // `hew_simtime_now_ms` returns i64; non-negative after enable(0) + advance.
        #[expect(
            clippy::cast_sign_loss,
            reason = "simtime starts at 0 and only advances forward; always non-negative"
        )]
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(now_ms as u64) };
        assert_eq!(woken, 1, "actor should wake at deadline t=500");
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor should be Runnable after timer fires"
        );
        assert_eq!(hew_wasm_sleeping_count(), 0, "sleep queue should be empty");

        crate::deterministic::hew_simtime_disable();
        // SAFETY: mailbox was allocated by hew_mailbox_new above; free to avoid leak.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
    }

    /// [`hew_wasm_sleeping_count`] returns 0 when no actors are sleeping.
    #[test]
    fn sleeping_count_zero_when_empty() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        assert_eq!(hew_wasm_sleeping_count(), 0);
        hew_sched_shutdown();
    }

    /// [`hew_wasm_timer_tick`] re-enqueues only actors whose deadline ≤ `now_ms`.
    #[test]
    fn timer_tick_wakes_at_exact_deadline() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // SAFETY: actor valid for duration of test.
        unsafe { park_actor_sleep(a_ptr, 1000) };

        // One ms before deadline: nothing wakes.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(999) };
        assert_eq!(woken, 0);
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32
        );

        // Exactly at deadline: actor wakes.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(1000) };
        assert_eq!(woken, 1);
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32
        );

        hew_sched_shutdown();
    }

    // ── Blocker regression tests ─────────────────────────────────────────

    /// Regression: `hew_sched_shutdown` must not hang waiting for a far-future
    /// deadline to expire (Blocker 2).  If the fix regresses, this test will
    /// time-out or take > 10 seconds.
    #[test]
    fn shutdown_does_not_hang_with_sleeping_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // Park actor with a deadline 1 hour in the future.
        let far_future: u64 = 3_600_000;
        // SAFETY: actor is valid for duration of test.
        unsafe { park_actor_sleep(a_ptr, far_future) };

        assert_eq!(
            hew_wasm_sleeping_count(),
            1,
            "actor should be in sleep queue"
        );

        // Shutdown must return promptly (not spin until t=3600000).
        hew_sched_shutdown();

        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "sleep queue must be empty after shutdown"
        );
    }

    /// Regression: `cancel_actor_sleep_queue_entry` removes the actor before
    /// free, so a subsequent `hew_wasm_timer_tick` does not dereference a
    /// freed actor pointer (Blocker 1).
    #[test]
    fn cancel_sleep_entry_prevents_dangling_pointer_after_free() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // SAFETY: actor is valid.
        unsafe { park_actor_sleep(a_ptr, 500) };
        assert_eq!(hew_wasm_sleeping_count(), 1);

        // Simulate what cleanup_all_actors does before freeing: cancel the entry.
        // SAFETY: Single-threaded; actor is still valid here.
        unsafe { cancel_actor_sleep_queue_entry(a_ptr) };

        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "entry must be removed before the actor is freed"
        );

        // A subsequent timer tick must not touch the (now-removed) entry.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(1000) };
        assert_eq!(woken, 0, "no actors should wake after entry was cancelled");

        hew_sched_shutdown();
    }

    /// Regression: `PENDING_SLEEP_DEADLINE_MS` must be cleared even when the
    /// actor stops or crashes mid-dispatch, not just on the normal path
    /// (Blocker 3).  If it were not cleared, the pending value would bleed
    /// into the next actor activation.
    #[test]
    fn pending_sleep_cleared_when_actor_crashes_mid_dispatch() {
        // Items before statements.
        static CRASH_COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C" fn crashing_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            CRASH_COUNT.fetch_add(1, Ordering::Relaxed);
            // Call request_sleep to write PENDING_SLEEP_DEADLINE_MS ...
            request_sleep(99_999);
            // ... then immediately crash the actor.
            // SAFETY: current actor pointer is valid during dispatch.
            let actor = crate::actor::hew_actor_self().cast::<HewActor>();
            if !actor.is_null() {
                // SAFETY: atom store is safe.
                unsafe {
                    (*actor)
                        .actor_state
                        .store(HewActorState::Crashed as i32, Ordering::Relaxed);
                }
            }
        }

        static NORMAL_COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C" fn normal_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            NORMAL_COUNT.fetch_add(1, Ordering::Relaxed);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mb_crash = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut crash_actor = stub_actor();
        crash_actor.id = 1;
        crash_actor.dispatch = Some(crashing_dispatch);
        crash_actor.mailbox = mb_crash.cast();
        let crash_ptr: *mut HewActor = (&raw mut crash_actor);

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mb_normal = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut normal_actor = stub_actor();
        normal_actor.id = 2;
        normal_actor.dispatch = Some(normal_dispatch);
        normal_actor.mailbox = mb_normal.cast();
        let normal_ptr: *mut HewActor = (&raw mut normal_actor);

        // Enqueue both actors, send crash_actor a message.
        // SAFETY: actors and mailboxes are valid.
        unsafe {
            sched_enqueue(crash_ptr);
            queue_wasm_message(crash_ptr, 0);
        }

        // Run one tick: crash_actor runs, sets sleep pending, then crashes.
        // SAFETY: Single-threaded test.
        let _ = unsafe { hew_wasm_sched_tick(1) };

        assert_eq!(CRASH_COUNT.load(Ordering::Relaxed), 1, "crash dispatch ran");

        // PENDING_SLEEP_DEADLINE_MS must have been cleared by the fix.
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read(),
                0,
                "PENDING_SLEEP_DEADLINE_MS must be 0 after crash dispatch"
            );
        }

        // Now enqueue normal_actor and tick. It must NOT be parked.
        // SAFETY: actors and mailboxes are valid.
        unsafe {
            sched_enqueue(normal_ptr);
            queue_wasm_message(normal_ptr, 0);
        }
        // SAFETY: Single-threaded test.
        let _ = unsafe { hew_wasm_sched_tick(1) };

        assert_eq!(
            NORMAL_COUNT.load(Ordering::Relaxed),
            1,
            "normal dispatch ran"
        );
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "normal actor must NOT be parked due to leaked pending deadline"
        );
        assert_eq!(
            normal_actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "normal actor should be Idle (messages drained), not parked in sleep queue"
        );

        // Cleanup.
        // SAFETY: mailboxes were heap-allocated above.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_free(mb_crash);
            crate::mailbox_wasm::hew_mailbox_free(mb_normal);
        }
        hew_sched_shutdown();
    }

    // ── Fix 1 regression: activation-entry PENDING clear ────────────────

    /// Regression: `PENDING_SLEEP_DEADLINE_MS` set before activation (e.g.,
    /// from `hew_sleep_ms` called outside a dispatch) must not cause a
    /// spurious sleep park when the activated actor processes no messages.
    ///
    /// This exercises the belt-and-suspenders clear at activation entry
    /// introduced as part of the two-blocker repair pass.
    #[test]
    fn activation_entry_clears_stale_pending_sleep() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // Manually inject a stale PENDING to simulate sleep_ms called outside
        // of a dispatch (e.g., top-level Hew main block).
        // SAFETY: Single-threaded test; no concurrent access.
        unsafe { ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(99_999) };

        // Activate an actor with an EMPTY mailbox — no dispatch runs, so
        // without the entry-clear the stale PENDING would never be consumed
        // by the per-iteration step.
        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.mailbox = mailbox.cast();
        // Set Runnable so activate_actor_wasm doesn't return early.
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        let a_ptr: *mut HewActor = (&raw mut a);
        // SAFETY: actor and mailbox are valid for the duration of the test.
        unsafe { activate_actor_wasm(a_ptr) };

        // The activation must NOT have parked the actor in the sleep queue.
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "stale PENDING must not park an actor that processed no messages"
        );
        // The actor should be Idle (went through activation, no messages).
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "actor should be Idle after empty-mailbox activation"
        );
        // With save/restore semantics, PENDING is restored to its pre-entry value
        // (99_999) at activation exit.  The key property — that the stale value
        // did NOT cause this actor to park — is already verified by the sleep-queue
        // and actor-state assertions above.  The stale value will similarly not
        // cause the next actor to park because the per-message consume step never
        // ran (empty mailbox → dispatch loop never entered).
        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(
                ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read(),
                99_999,
                "PENDING_SLEEP_DEADLINE_MS is restored to its saved value by save/restore"
            );
        }
        // Cleanup: clear the stale pending value so it doesn't leak into later tests.
        // SAFETY: Single-threaded test; no concurrent access.
        unsafe { ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(0) };

        // SAFETY: mailbox was heap-allocated above.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
    }

    // ── Fix 2 regression: Sleeping vs Idle state, no early wake ─────────

    /// Regression: a message sent to a sleeping actor must NOT wake it before
    /// the timer fires.  The message must queue in the mailbox and be
    /// delivered only when `drain_expired_sleepers` transitions the actor
    /// from `Sleeping` → `Runnable`.
    ///
    /// Also verifies that there is no stale `SLEEP_QUEUE` entry after the timer
    /// fires (no double-enqueue / phantom wake).
    #[test]
    fn message_to_sleeping_actor_queues_without_early_wake() {
        static DISPATCHED: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C" fn counting_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            DISPATCHED.fetch_add(1, Ordering::Relaxed);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        crate::deterministic::hew_simtime_enable(0);

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.dispatch = Some(counting_dispatch);
        a.mailbox = mailbox.cast();
        // Leave state at the stub default (Runnable) — park_actor_sleep will
        // transition it to Sleeping internally.
        let a_ptr: *mut HewActor = (&raw mut a);

        // Park actor directly at t=1000 (simulating post-dispatch park).
        // SAFETY: actor is valid; scheduler is initialized.
        unsafe { park_actor_sleep(a_ptr, 1000) };
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32,
            "actor must be Sleeping after park"
        );
        assert_eq!(hew_wasm_sleeping_count(), 1, "actor must be in sleep queue");

        // Send a message to the sleeping actor via the standard send path
        // (queue_wasm_message mirrors hew_actor_send: mailbox_send + wake_wasm_actor).
        // wake_wasm_actor only wakes Idle actors; it must be a no-op for Sleeping.
        // SAFETY: actor and mailbox are valid.
        unsafe { queue_wasm_message(a_ptr, 0) };

        // State must still be Sleeping — NOT Runnable — after the send.
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32,
            "sleeping actor must NOT be woken by a message send"
        );
        assert_eq!(hew_wasm_sleeping_count(), 1, "still in sleep queue");
        assert_eq!(
            hew_sched_metrics_global_queue_len(),
            0,
            "sleeping actor must NOT be in the run queue after message send"
        );
        // Dispatch must NOT have run (actor was not activated).
        assert_eq!(
            DISPATCHED.load(Ordering::Relaxed),
            0,
            "no dispatch before timer"
        );

        // Advance time past deadline and drain: actor wakes, processes message.
        // SAFETY: Single-threaded test.
        let woken = unsafe { drain_expired_sleepers(1001) };
        assert_eq!(woken, 1, "actor must wake when timer fires");
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor must be Runnable after timer fires"
        );
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "sleep queue must be empty after wake"
        );

        // Actually activate the actor to drain the queued message.
        // SAFETY: actor and mailbox are valid.
        unsafe { activate_actor_wasm(a_ptr) };
        assert_eq!(
            DISPATCHED.load(Ordering::Relaxed),
            1,
            "queued message delivered"
        );

        // No phantom re-wake: sleep queue is still empty.
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "no stale sleep entry after message delivery"
        );

        crate::deterministic::hew_simtime_disable();
        // SAFETY: mailbox was heap-allocated above.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
    }

    // ── Sonnet 4.6 repair regressions ────────────────────────────────────

    /// Regression: a nested `activate_actor_wasm` call (e.g. from ask/await
    /// inside a dispatch handler) must NOT erase the outer actor's pending
    /// sleep request.  If the save/restore is missing, `actor_sleep_deadline`
    /// for the outer actor will be 0 and it will never park.
    #[test]
    fn nested_activation_preserves_outer_pending_sleep() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut inner = stub_actor();
        inner.id = 99;
        inner.dispatch = Some(inner_dispatch_noop);
        inner.mailbox = mailbox.cast();
        inner
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        let inner_ptr: *mut HewActor = (&raw mut inner);

        // Simulate the outer actor having called sleep_ms(500): write directly
        // into the global, the same way request_sleep() does.
        // SAFETY: Single-threaded test; simulates outer-actor sleep request.
        unsafe { ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(500) };

        // Queue a message so the inner actor's dispatch loop actually runs.
        // SAFETY: inner actor and mailbox are valid.
        unsafe { queue_wasm_message(inner_ptr, 0) };

        // Activate the inner actor directly — this is what a nested ask/await
        // does at runtime.  With the fix, it must save 500, clear to 0, run,
        // then restore 500.  Without the fix, it clears to 0 and never restores.
        // SAFETY: inner actor is Runnable and its mailbox is valid.
        unsafe { activate_actor_wasm(inner_ptr) };

        // The outer actor's pending sleep (500) must survive the nested activation.
        // SAFETY: Single-threaded test.
        let pending = unsafe { ptr::addr_of!(PENDING_SLEEP_DEADLINE_MS).read() };
        assert_eq!(
            pending, 500,
            "outer actor's pending sleep must be preserved after nested activation"
        );

        // Cleanup: clear the pending deadline and free resources.
        // SAFETY: Single-threaded test; no concurrent readers.
        unsafe { ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(0) };
        // SAFETY: mailbox was heap-allocated above.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
    }

    /// Regression: if a runnable actor calls `sleep_ms` during the shutdown
    /// drain, `hew_sched_shutdown` must still return promptly.  Before the fix,
    /// the newly-added sleep entry caused the scheduler spin-loop to block until
    /// the (far-future) deadline expired.  If this test hangs, the fix regressed.
    #[test]
    fn shutdown_does_not_hang_when_draining_actor_calls_sleep() {
        // Declare items before statements (items-after-statements lint).
        static DRAIN_DISPATCHED: AtomicI32 = AtomicI32::new(0);
        // SAFETY: `request_sleep` is safe to call from dispatch context.
        unsafe extern "C" fn sleep_requesting_dispatch(
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            DRAIN_DISPATCHED.fetch_add(1, Ordering::Relaxed);
            // Far-future absolute deadline — hangs if not cleared on shutdown.
            request_sleep(u64::MAX / 2);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.dispatch = Some(sleep_requesting_dispatch);
        a.mailbox = mailbox.cast();
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        let a_ptr: *mut HewActor = (&raw mut a);

        // SAFETY: actor and scheduler are valid.
        unsafe { sched_enqueue(a_ptr) };
        // SAFETY: actor and mailbox are valid.
        unsafe { queue_wasm_message(a_ptr, 0) };

        // Shutdown must drain the run queue AND clear the sleep entry created
        // by sleep_requesting_dispatch — without blocking on the deadline.
        hew_sched_shutdown();

        // Verify the dispatch actually ran (actor was drained, not skipped).
        assert_eq!(
            DRAIN_DISPATCHED.load(Ordering::Relaxed),
            1,
            "actor dispatch must have run during shutdown drain"
        );
        // Sleep queue must be empty: new entries must not survive shutdown.
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "sleep queue must be empty after shutdown regardless of new entries"
        );

        // SAFETY: mailbox was heap-allocated above.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
    }

    /// Regression: `actor_ask_wasm_impl` (and the shared wait loops) must not
    /// bail when the run queue is empty but the sleep queue is non-empty.
    ///
    /// Before the fix, `remaining == 0` was treated as "no further progress
    /// possible" even when sleeping actors would eventually wake and deposit a
    /// reply.  This test spawns an actor whose phase-1 dispatch:
    ///
    ///   1. Retains the reply channel for later use.
    ///   2. Schedules a 1 ms cooperative sleep (`request_sleep`).
    ///   3. Self-sends a continuation message (`msg_type=2`) so the actor is
    ///      re-activated after the sleep expires.
    ///
    /// Phase-2 dispatch (the continuation) deposits the reply.
    ///
    /// Without the fix the ask loop sees `remaining == 0` after phase 1 (the
    /// actor is sleeping, not in the run queue) and returns `NoRunnableWork`.
    /// With the fix the loop recognises that sleepers remain, keeps driving
    /// `hew_wasm_sched_tick`, and eventually receives the reply.
    #[test]
    fn ask_reply_after_sleep_parks_then_wakes() {
        use std::sync::atomic::{AtomicPtr, Ordering as AOrdering};

        static STORED_CH: AtomicPtr<crate::reply_channel_wasm::WasmReplyChannel> =
            AtomicPtr::new(ptr::null_mut());

        /// Phase 1 (`msg_type` == 1): retain the reply channel, request a 1 ms
        /// cooperative sleep, self-send a continuation.
        /// Phase 2 (`msg_type` == 2): deposit the reply on the stored channel.
        unsafe extern "C" fn sleep_then_reply_dispatch(
            _state: *mut c_void,
            msg_type: i32,
            _data: *mut c_void,
            _size: usize,
        ) {
            if msg_type == 1 {
                let ch = hew_get_reply_channel();
                // Extra retain: the message teardown path will release the
                // message's reference; we need our own ref to survive it.
                // SAFETY: ch is the active reply channel for this dispatch;
                // single-threaded cooperative scheduler, no data races.
                unsafe {
                    crate::reply_channel_wasm::hew_reply_channel_retain(ch.cast());
                }
                STORED_CH.store(ch.cast(), AOrdering::Relaxed);

                // Schedule a ≈1 ms sleep (real wall-clock time).
                // SAFETY: hew_now_ms has no preconditions.
                let now = unsafe { hew_now_ms() };
                request_sleep(now.saturating_add(1));

                // Self-send continuation so the actor is re-activated after
                // the sleep.  The actor is RUNNING during dispatch so
                // `wake_wasm_actor` is a no-op; the message queues in the
                // mailbox and is delivered when the timer fires.
                let me = crate::actor::hew_actor_self();
                if !me.is_null() {
                    // SAFETY: `me` is the currently-running actor; its mailbox
                    // is valid for the duration of the dispatch.
                    let _ = unsafe {
                        crate::mailbox_wasm::hew_mailbox_send(
                            (*me).mailbox.cast(),
                            2,
                            ptr::null_mut(),
                            0,
                        )
                    };
                }
            } else if msg_type == 2 {
                // Phase 2: deposit the reply on the stashed channel.
                let ch = STORED_CH.swap(ptr::null_mut(), AOrdering::Relaxed);
                if !ch.is_null() {
                    let mut v: i32 = 7;
                    // SAFETY: ch was retained in phase 1; the caller's ref
                    // keeps it alive.  hew_reply will release our extra retain.
                    unsafe {
                        crate::reply_channel_wasm::hew_reply(
                            ch,
                            (&raw mut v).cast(),
                            std::mem::size_of::<i32>(),
                        );
                    }
                }
            }
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut actor = stub_actor();
        actor.dispatch = Some(sleep_then_reply_dispatch);
        actor.mailbox = mailbox.cast();
        // Start Idle so that `ask_with_channel_wasm_internal` → `wake_wasm_actor`
        // transitions the actor to Runnable and enqueues it.
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor);

        // Drive the full ask loop.  Before the fix this returned null because
        // `remaining == 0` fired when the actor parked in the sleep queue.
        // Cast to actor::HewActor — both types are layout-identical (verified
        // by compile-time offset_of! assertions in scheduler_wasm.rs).
        // SAFETY: actor_ptr is valid and live for the duration of this call;
        // layout compatibility is verified by the offset_of! assertions.
        let reply = unsafe {
            crate::actor::actor_ask_wasm_impl(
                actor_ptr.cast::<crate::actor::HewActor>(),
                1,
                ptr::null_mut(),
                0,
                None,
            )
        };
        assert!(
            !reply.is_null(),
            "ask must succeed even when the handler parks in the sleep queue before replying"
        );
        // SAFETY: reply was malloc'd by hew_reply; caller takes ownership.
        unsafe {
            assert_eq!(*reply.cast::<i32>(), 7, "reply value must match");
            libc::free(reply);
        }
        // All reply-channel references must be balanced.
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            0,
            "ask loop must release the reply channel after a sleep-deferred reply"
        );

        // SAFETY: mailbox was heap-allocated above.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
        hew_runtime_cleanup();
    }
}
