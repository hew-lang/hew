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

use crate::internal::types::HewActorState;

// ── Constants ───────────────────────────────────────────────────────────

/// Default message processing budget per activation.
const HEW_MSG_BUDGET: i32 = 256;

/// Default reduction budget per dispatch call.
const HEW_DEFAULT_REDUCTIONS: i32 = 4000;

/// Priority: high (2x budget).
const HEW_PRIORITY_HIGH: i32 = 0;

/// Priority: normal (1x budget, default).
#[cfg(test)]
const HEW_PRIORITY_NORMAL: i32 = 1;

/// Priority: low (0.5x budget).
const HEW_PRIORITY_LOW: i32 = 2;

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

// ── Arena lifecycle shim ─────────────────────────────────────────────────
//
// On native (test builds) the full arena module is available and we wire
// up real `set_current_arena` / `hew_arena_reset` calls.  On wasm32 the
// arena module is not compiled; the stubs are no-ops so the call sites
// compile and will activate automatically once WASM arena support lands.
//
// WASM-TODO: replace the wasm32 stubs below with a real allocator once a
// wasm32-compatible arena (backed by `memory.grow` or a slab) is added.

/// Install `arena` as the per-activation current arena and return the
/// previously active arena pointer. Mirrors `crate::arena::set_current_arena`.
#[cfg(not(target_arch = "wasm32"))]
fn arena_install(arena: *mut c_void) -> *mut c_void {
    crate::arena::set_current_arena(arena.cast::<crate::arena::ActorArena>()).cast::<c_void>()
}

/// WASM-TODO: arena install stub — no-op until wasm32 arena support lands.
#[cfg(target_arch = "wasm32")]
fn arena_install(_arena: *mut c_void) -> *mut c_void {
    std::ptr::null_mut()
}

/// Reset `arena` for reuse after a completed dispatch cycle.
/// Mirrors `crate::arena::hew_arena_reset`. Safe to call with null.
///
/// # Safety
///
/// `arena` must be either null or a valid pointer previously returned by
/// `hew_arena_new()` that has not yet been freed.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn arena_reset(arena: *mut c_void) {
    if !arena.is_null() {
        // SAFETY: caller guarantees arena is valid.
        unsafe { crate::arena::hew_arena_reset(arena.cast::<crate::arena::ActorArena>()) };
    }
}

/// WASM-TODO: arena reset stub — no-op until wasm32 arena support lands.
///
/// # Safety
///
/// No-op; always safe on wasm32.
#[cfg(target_arch = "wasm32")]
unsafe fn arena_reset(_arena: *mut c_void) {}

// ── Global state (single-threaded, no atomics needed) ───────────────────

static mut RUN_QUEUE: Option<VecDeque<*mut HewActor>> = None;
static mut INITIALIZED: bool = false;

/// Whether an actor is currently being activated (for `active_workers` metric).
static mut ACTIVATING: bool = false;

/// Saved arena pointer during activation.
static mut PREV_ARENA: *mut c_void = std::ptr::null_mut();

/// Reply channel for the message currently being dispatched (WASM
/// equivalent of the native thread-local `CURRENT_REPLY_CHANNEL`).
static mut CURRENT_REPLY_CHANNEL: *mut c_void = std::ptr::null_mut();

// ── Metrics counters (plain u64, no atomics needed) ─────────────────────

static mut TASKS_SPAWNED: u64 = 0;
static mut TASKS_COMPLETED: u64 = 0;
static mut MESSAGES_SENT: u64 = 0;
static mut MESSAGES_RECEIVED: u64 = 0;

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
    0
}

/// Shut down the cooperative scheduler.
///
/// Process all pending actors and then reset state. On WASM the
/// scheduler is cooperative, so we must drain the run queue (just like
/// [`hew_sched_run`]) before tearing down. Safe to call if the
/// scheduler was never initialized.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_shutdown() {
    // Process all pending messages before shutting down.
    hew_sched_run();

    // SAFETY: Single-threaded on WASM.
    unsafe {
        RUN_QUEUE = None;
        INITIALIZED = false;
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

/// Run all enqueued actors to completion.
///
/// Loops until the run queue is empty: pops the front actor, activates
/// it, and re-enqueues it if it still has pending messages.
///
/// This is the main entry point for standalone WASM programs.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_run() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        while let Some(ref mut q) = RUN_QUEUE {
            let Some(actor) = q.pop_front() else {
                break;
            };
            activate_actor_wasm(actor);

            // Re-enqueue if the actor is still runnable.
            let a = &*actor;
            let state = a.actor_state.load(Ordering::Relaxed);
            if state == HewActorState::Runnable as i32 {
                if let Some(ref mut q) = RUN_QUEUE {
                    q.push_back(actor);
                }
            }
        }
    }
}

// ── Internal API ────────────────────────────────────────────────────────

/// Submit an actor to the run queue.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
pub unsafe fn sched_enqueue(actor: *mut HewActor) {
    // SAFETY: Single-threaded on WASM; caller guarantees actor validity.
    unsafe {
        TASKS_SPAWNED += 1;
        if let Some(ref mut q) = RUN_QUEUE {
            q.push_back(actor);
        }
    }
}

/// C ABI wrapper for [`sched_enqueue`], callable from [`crate::bridge`].
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
#[cfg_attr(not(test), no_mangle)]
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
/// # Safety
///
/// The scheduler must have been initialized with [`hew_sched_init`].
#[cfg_attr(not(test), no_mangle)]
#[must_use]
pub unsafe extern "C" fn hew_wasm_sched_tick(max_activations: i32) -> i32 {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        let mut activations = 0i32;
        loop {
            if activations >= max_activations {
                break;
            }
            let actor = match RUN_QUEUE {
                Some(ref mut q) => q.pop_front(),
                None => break,
            };
            let Some(actor) = actor else {
                break;
            };
            activate_actor_wasm(actor);
            activations += 1;

            // Re-enqueue if the actor is still runnable.
            let a = &*actor;
            let state = a.actor_state.load(Ordering::Relaxed);
            if state == HewActorState::Runnable as i32 {
                if let Some(ref mut q) = RUN_QUEUE {
                    q.push_back(actor);
                }
            }
        }

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
                    CURRENT_REPLY_CHANNEL = msg_ref.reply_channel;
                    dispatch(a.state, msg_ref.msg_type, msg_ref.data, msg_ref.data_size);
                    CURRENT_REPLY_CHANNEL = std::ptr::null_mut();
                }

                // The dispatch function handled the reply channel (if any).
                // Clear it from the message node so msg_node_free doesn't
                // send a duplicate reply.
                // SAFETY: msg is exclusively owned by this scheduler tick.
                unsafe { (*msg).reply_channel = std::ptr::null_mut() };

                msgs_processed += 1;
                a.prof_messages_processed.fetch_add(1, Ordering::Relaxed);
                // Skip timing for now (use 0 for elapsed_ns). Timing can be
                // added later with WASI clock_time_get.
            }

            // SAFETY: `msg` was returned by `hew_mailbox_try_recv` and is
            // now exclusively owned by us.
            unsafe { hew_msg_node_free(msg) };

            // Check for mid-dispatch stop.
            let mid_state = a.actor_state.load(Ordering::Relaxed);
            if mid_state == HewActorState::Stopping as i32
                || mid_state == HewActorState::Stopped as i32
                || mid_state == HewActorState::Crashed as i32
            {
                break;
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
    // with a clean cursor.  The null-arena fast-path is intentional: today
    // all WASM actors carry a null arena and both calls are lightweight
    // no-ops in that case.
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
        ACTIVATING = saved_activating;
        TASKS_COMPLETED += 1;
    }

    // ── Post-activation state transitions ───────────────────────────────

    let cur_state = a.actor_state.load(Ordering::Relaxed);

    // Stopping -> Stopped: finalise the lifecycle and invoke terminate callback.
    if cur_state == HewActorState::Stopping as i32 {
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
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

    // Hibernation tracking.
    let hib_thresh = a.hibernation_threshold.load(Ordering::Relaxed);
    if msgs_processed == 0 && hib_thresh > 0 {
        let prev_idle = a.idle_count.fetch_add(1, Ordering::Relaxed);
        if prev_idle + 1 >= hib_thresh {
            a.hibernating.store(1, Ordering::Relaxed);
        }
    } else if msgs_processed > 0 {
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
    }

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
        // NOTE: The caller (hew_sched_run) handles re-enqueue by checking
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

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    use std::sync::Mutex;

    use crate::internal::types::HewError;

    /// Serialize all tests in this module since they share `static mut`
    /// global state. Rust's test harness runs tests in parallel by
    /// default; without this lock, concurrent mutation of the globals
    /// causes undefined behaviour.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

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
            ptr::addr_of_mut!(RUN_QUEUE).write(None);
            ptr::addr_of_mut!(INITIALIZED).write(false);
            ptr::addr_of_mut!(ACTIVATING).write(false);
            // Reset the canonical current-actor slot (CURRENT_ACTOR_WASM on
            // wasm32, thread-local on native) rather than the removed
            // scheduler-local CURRENT_ACTOR static.
            crate::actor::set_current_actor(ptr::null_mut());
            ptr::addr_of_mut!(PREV_ARENA).write(ptr::null_mut());
            ptr::addr_of_mut!(CURRENT_REPLY_CHANNEL).write(ptr::null_mut());
            ptr::addr_of_mut!(TASKS_SPAWNED).write(0);
            ptr::addr_of_mut!(TASKS_COMPLETED).write(0);
            ptr::addr_of_mut!(MESSAGES_SENT).write(0);
            ptr::addr_of_mut!(MESSAGES_RECEIVED).write(0);
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
        let _guard = TEST_LOCK.lock().unwrap();
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

    #[test]
    fn double_init_is_noop() {
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
    fn unbounded_wasm_ask_cancels_when_no_runnable_work_remains() {
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
            CURRENT_REPLY_CHANNEL = msg_ref.reply_channel;
            dispatch(
                actor.state,
                msg_ref.msg_type,
                msg_ref.data,
                msg_ref.data_size,
            );
            CURRENT_REPLY_CHANNEL = ptr::null_mut();
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
        let _guard = TEST_LOCK.lock().unwrap();
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

    /// The WASM scheduler must invoke `terminate_fn` as part of the
    /// `Stopping → Stopped` state transition — not only at
    /// `cleanup_all_actors` / process exit (parity with native scheduler).
    #[test]
    fn terminate_fn_fires_on_stopping_to_stopped_scheduler_path() {
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
        let _guard = TEST_LOCK.lock().unwrap();
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

    /// Idempotency: `terminate_fn` must not be invoked a second time if a
    /// cleanup path runs after the close-path already fired it.
    #[test]
    fn terminate_fn_not_double_invoked_by_cleanup_after_closed_mailbox_stop() {
        let _guard = TEST_LOCK.lock().unwrap();
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
    // arenas with the same lifecycle contract as the native scheduler.  The
    // tests run on native (test build), where the full arena module is
    // available.  On wasm32 the calls are no-ops until WASM arena support
    // lands (WASM-TODO in arena_install / arena_reset shims above).

    /// `hew_arena_malloc` must route through the actor's arena during
    /// dispatch and fall back to libc malloc once activation finishes.
    #[test]
    fn arena_is_installed_during_dispatch_and_cleared_after() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Create a real arena so we can detect whether alloc routes through it.
        let actor_arena = crate::arena::hew_arena_new();
        assert!(!actor_arena.is_null(), "arena creation must succeed");

        // Dispatch captures the arena pointer that was active mid-dispatch.
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
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor_arena = crate::arena::hew_arena_new();
        assert!(!actor_arena.is_null());

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
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Two separate arenas — one per actor.
        let outer_arena = crate::arena::hew_arena_new();
        let inner_arena = crate::arena::hew_arena_new();
        assert!(!outer_arena.is_null() && !inner_arena.is_null());

        // We record what arena was active after each dispatch returns.
        static OUTER_POST_DISPATCH: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);

        // Inner actor: simple no-op dispatch.
        let mut inner_actor = stub_actor();
        inner_actor.id = 2;
        inner_actor.arena = inner_arena.cast::<c_void>();
        // SAFETY: test exclusively owns this mailbox.
        inner_actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        let mut inner_ptr: *mut HewActor = (&raw mut inner_actor).cast();

        // Outer dispatch: enqueues and runs the inner actor inline (simulating
        // re-entrant activation through hew_actor_ask / hew_sched_run).
        unsafe extern "C" fn outer_dispatch(
            state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
        ) {
            // state carries a pointer to the inner actor.
            // SAFETY: state was set to a valid HewActor pointer by the test.
            let inner: *mut HewActor = unsafe { *state.cast::<*mut HewActor>() };
            unsafe {
                // Enqueue inner actor and send it a message.
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
            let rc = unsafe {
                crate::mailbox_wasm::hew_mailbox_send(
                    (*actor).mailbox.cast(),
                    1,
                    (&raw mut payload).cast(),
                    std::mem::size_of::<i32>(),
                )
            };
            let _ = rc; // ignore error in test helper
            unsafe { crate::actor::wake_wasm_actor(actor.cast::<crate::actor::HewActor>()) };
        }

        let mut outer_actor = stub_actor();
        outer_actor.id = 1;
        outer_actor.arena = outer_arena.cast::<c_void>();
        outer_actor.dispatch = Some(outer_dispatch);
        // Pass inner_ptr via state so outer_dispatch can enqueue it.
        outer_actor.state = (&raw mut inner_ptr as *mut *mut HewActor).cast::<c_void>();
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
        let _guard = TEST_LOCK.lock().unwrap();
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
}
