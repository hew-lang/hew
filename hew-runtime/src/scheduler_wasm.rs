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
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::VecDeque;
use std::ffi::{c_int, c_void};
#[cfg(not(target_arch = "wasm32"))]
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, AtomicU64, Ordering};

#[cfg(test)]
use crate::actor::HEW_PRIORITY_NORMAL;
use crate::actor::{HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET, HEW_PRIORITY_HIGH, HEW_PRIORITY_LOW};
use crate::internal::types::{HewActorState, HewDispatchFn, HewSysDispatchFn};
use crate::mailbox_header::{HewSysMsg, Origin};
use crate::timer_wheel::{
    hew_timer_wheel_free, hew_timer_wheel_new, hew_timer_wheel_remove,
    timer_wheel_schedule_at_handle, timer_wheel_tick_to, HewTimerHandle, HewTimerWheel,
};

static WASM_CLEANUP_RAN: AtomicBool = AtomicBool::new(false);

/// When set to `true` in tests, `wasm_timer_wheel()` returns null to simulate
/// an OOM failure from `hew_timer_wheel_new`.  Reset to `false` after use.
/// Serialised by the `runtime_test_guard` / test lock.
#[cfg(test)]
static TEST_FORCE_WHEEL_NULL: AtomicBool = AtomicBool::new(false);

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

#[inline]
fn trace_actor_stop_lifecycle(
    actor_id: u64,
    trace_context: *mut crate::execution_context::HewExecutionContext,
) {
    // WASM-R37-S2: WASM stop/on(stop) paths must be observable through the
    // same lifecycle trace event as native before invoking terminate_fn.
    //
    let installed_trace_context =
        crate::execution_context::current_context().is_null() && !trace_context.is_null();
    let prev_context = if installed_trace_context {
        crate::execution_context::set_current_context(trace_context)
    } else {
        std::ptr::null_mut()
    };
    crate::tracing::hew_trace_lifecycle(actor_id, crate::tracing::SPAN_STOP);
    if installed_trace_context {
        let restored_context = crate::execution_context::set_current_context(prev_context);
        debug_assert_eq!(restored_context, trace_context);
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
    pub state: *mut c_void,
    pub state_size: usize,
    pub dispatch: Option<HewDispatchFn>,
    pub mailbox: *mut c_void,
    pub actor_state: AtomicI32,
    pub budget: AtomicI32,
    pub init_state: *mut c_void,
    pub init_state_size: usize,
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    pub terminate_fn: Option<unsafe extern "C" fn(*mut c_void)>,
    pub state_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
    pub state_clone_fn: Option<crate::actor::HewStateCloneFn>,
    pub terminate_called: AtomicBool,
    pub terminate_finished: AtomicBool,
    pub dispatch_active: AtomicBool,
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
    // ── Slice-4 suspend/resume executor (appended; matches native exactly) ──
    pub suspended_cont: AtomicPtr<c_void>,
    pub cont_tag: AtomicI32,
    pub pending_wake: AtomicBool,
    pub suspended_reply_channel: AtomicPtr<c_void>,
    pub suspended_cancel_token: AtomicPtr<c_void>,
    // ── Runtime identity (appended; matches native exactly) ─────────────────
    // Same always-compiled `RuntimeId` type the native struct uses, so the
    // layout assert below is type-identical, not merely size-identical.
    pub runtime_id: crate::runtime_id::RuntimeId,
    // Native stores `*const RuntimeInner`; WASM has no native runtime module, so
    // keep this opaque while preserving size/alignment/offset parity.
    pub runtime: *const c_void,
    // ── Send-pin counter (appended; matches native exactly) ──────────────────
    // Incremented by `with_actor_send_by_id` before releasing LIVE_ACTORS and
    // decremented after the by-ID operation completes.  The free path waits for
    // this counter to reach 0 before reclaiming the allocation.
    pub send_pin_count: AtomicU32,
    // ── Receive-gen stream-producer sink registry (appended; matches native
    // exactly) ── `receive gen fn` is native-only today (the WASM
    // scheduler never drives a stream-producer pump), so this field is
    // never read/written here — it exists purely to preserve the layout
    // parity this module asserts.
    pub gen_sink: AtomicPtr<c_void>,
    // Stable target-word local-handle identity; mirrors the canonical tail.
    pub local_pid_id: crate::lifetime::local_handles::HewLocalPidId,
    // Full un-masked spawn serial; mirrors the canonical tail so the layout
    // parity this module asserts holds. Never read on WASM (the owner-scoped
    // role ask that consumes it is native-only), but present for size/offset
    // parity with the native `HewActor`.
    pub spawn_serial: u64,
    // The SYSTEM dispatch entry point; mirrors the canonical tail so the
    // layout parity this module asserts holds.
    pub sys_dispatch: Option<crate::internal::types::HewSysDispatchFn>,
    // One-shot typed state-drop authority; see the canonical actor field.
    pub state_drop_consumed: AtomicBool,
    // Supervisor shallow-template provenance; mirrors the canonical tail.
    pub state_drop_borrowed: AtomicBool,
    // Shutdown-drain ask gate; mirrors the canonical tail. The drain scan is
    // native-only, so this slot is never read or written on WASM — it exists
    // purely to preserve the layout parity this module asserts.
    pub parked_ask_channel: AtomicPtr<c_void>,
}

/// The dispatch entry point selected for one dequeued message — the WASM twin
/// of `crate::scheduler::DispatchTarget`. Built from the node's [`Origin`]
/// before any handler runs.
#[derive(Clone, Copy)]
enum DispatchTarget {
    /// An application message for the actor's user trampoline.
    User(HewDispatchFn),
    /// A runtime lifecycle signal for the actor's system entry point.
    Sys(HewSysDispatchFn, HewSysMsg),
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
    assert!(offset_of!(W, state_drop_fn) == offset_of!(N, state_drop_fn));
    assert!(offset_of!(W, state_clone_fn) == offset_of!(N, state_clone_fn));
    assert!(offset_of!(W, terminate_called) == offset_of!(N, terminate_called));
    assert!(offset_of!(W, terminate_finished) == offset_of!(N, terminate_finished));
    assert!(offset_of!(W, dispatch_active) == offset_of!(N, dispatch_active));
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
    assert!(offset_of!(W, suspended_cont) == offset_of!(N, suspended_cont));
    assert!(offset_of!(W, cont_tag) == offset_of!(N, cont_tag));
    assert!(offset_of!(W, pending_wake) == offset_of!(N, pending_wake));
    assert!(offset_of!(W, suspended_reply_channel) == offset_of!(N, suspended_reply_channel));
    assert!(offset_of!(W, suspended_cancel_token) == offset_of!(N, suspended_cancel_token));
    assert!(offset_of!(W, runtime_id) == offset_of!(N, runtime_id));
    assert!(offset_of!(W, runtime) == offset_of!(N, runtime));
    assert!(offset_of!(W, send_pin_count) == offset_of!(N, send_pin_count));
    assert!(offset_of!(W, gen_sink) == offset_of!(N, gen_sink));
    assert!(offset_of!(W, local_pid_id) == offset_of!(N, local_pid_id));
    assert!(offset_of!(W, spawn_serial) == offset_of!(N, spawn_serial));
    assert!(offset_of!(W, sys_dispatch) == offset_of!(N, sys_dispatch));
    assert!(offset_of!(W, state_drop_consumed) == offset_of!(N, state_drop_consumed));
    assert!(offset_of!(W, state_drop_borrowed) == offset_of!(N, state_drop_borrowed));
    assert!(offset_of!(W, parked_ask_channel) == offset_of!(N, parked_ask_channel));
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
    fn hew_mailbox_has_messages(mb: *mut c_void) -> i32;
    fn hew_msg_node_free(node: *mut HewMsgNode);
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

// ── WASM unified timer wheel ─────────────────────────────────────────────
//
// A single `HewTimerWheel` drives both sleep timers and periodic timers on
// WASM, replacing the former O(n²) sorted-Vec `SLEEP_QUEUE` and
// `PERIODIC_QUEUE`. The wheel is lazily initialised in `wasm_timer_wheel()`
// and freed (along with all pending entries' callback data) in
// `hew_sched_shutdown`.
//
// **Drop-safety contract**:
//
// Every `Box`-allocated callback-data struct (`WasmSleepCtx`) is registered
// in `SLEEP_HANDLES` at insertion and removed at fire or cancel so exactly
// one party (fire callback OR cancel path) performs the drop — the same
// "atomic claim" guarantee the native `Arc`-based periodic path upholds.
//
// **Semantics identical to native**:
// - Expiry ordering: `timer_wheel_tick_to` fires callbacks in insertion/slot
//   order, same as the native ticker path.
// - Cancel: `hew_timer_wheel_remove` atomically unlinks and frees the wheel
//   entry, handing ownership of the data to the caller.
// - Shutdown: entries are drained from the registries (freeing their ctxs)
//   before `hew_timer_wheel_free` reclaims the wheel struct itself.

/// Global WASM timer wheel.  Null until first use; freed in `hew_sched_shutdown`.
static mut WASM_TIMER_WHEEL: *mut HewTimerWheel = std::ptr::null_mut();

/// Number of actors currently parked in a sleep timer entry.
/// Incremented by `park_actor_sleep`, decremented by the sleep callback and
/// by `cancel_actor_sleep_queue_entry`.  Used by `hew_wasm_sleeping_count`.
static mut WASM_SLEEP_COUNT: usize = 0;

/// Per-actor map from actor address to the currently-pending sleep handle.
/// Because an actor can only be in `Sleeping` state once at a time, each
/// actor has at most one entry here.
///
/// Using `Option<HashMap<…>>` so that the empty-runtime case (no sleeps ever
/// scheduled) avoids the heap allocation entirely.
static mut SLEEP_HANDLES: Option<std::collections::HashMap<usize, HewTimerHandle>> = None;

/// Pending sleep deadline set by the currently-dispatching actor via
/// [`request_sleep`].  Zero means no pending sleep.  Consumed and reset
/// by [`activate_actor_wasm`] after each message dispatch.
static mut PENDING_SLEEP_DEADLINE_MS: u64 = 0;

/// Whether an actor is currently being activated (for `active_workers` metric).
static mut ACTIVATING: bool = false;

/// Scheduler shutdown phase.
///
/// WASM shutdown keeps the timer wheel allocated through parked-frame
/// retirement because `coro.destroy` cleanup may cancel timer registrations.
/// Keeping the allocation alive must not keep the wheel ACTIVE, though:
/// cooperate-driven nested ticks would otherwise fire due sleep/periodic work
/// items during the run-queue drain, and retiring an orphaned ask could
/// re-enqueue the very continuation the retirement pass is abandoning.
#[derive(Clone, Copy, PartialEq, Eq)]
enum WasmShutdownPhase {
    /// Normal host-driven or standalone execution.
    Running,
    /// Drain work that was already runnable, but do not fire timers.
    Draining,
    /// Abandon parked frames/sleep registrations; refuse continuation wakes.
    Retiring,
    /// Timer registries and the wheel are being destroyed.
    TimerTeardown,
}

static mut SHUTDOWN_PHASE: WasmShutdownPhase = WasmShutdownPhase::Running;

#[inline]
fn shutdown_phase() -> WasmShutdownPhase {
    // SAFETY: the WASM scheduler is single-threaded. Native tests serialize
    // this target-specific state with runtime_test_guard.
    unsafe { std::ptr::addr_of!(SHUTDOWN_PHASE).read() }
}

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

// Reply-channel readers (`hew_get_reply_channel`) and the consume marker
// (`mark_current_reply_channel_consumed`) live in [`crate::execution_context`]
// and are shared by native + WASM. The per-dispatch carrier (the canonical
// `HewExecutionContext` installed at the top of `activate_actor_wasm`) owns
// both the channel pointer and the `HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED` flag.
// Nested activations (worker A's ask → activate B mid-handler) get a fresh
// `HewExecutionContext` stack frame and therefore cannot clobber the outer
// arm's reply channel — the chain is restored automatically when the inner
// activation pops its ctx via `set_current_context(prev_context)`.
//
// Re-export the target-neutral mark function so existing `crate::scheduler_wasm::*`
// callers (e.g. `reply_channel_wasm::hew_reply`) keep compiling.
pub(crate) use crate::execution_context::mark_current_reply_channel_consumed;

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

// ── WASM timer wheel accessor ────────────────────────────────────────────

/// Callback data for a wheel-backed sleep entry.
struct WasmSleepCtx {
    actor: *mut HewActor,
}

// SAFETY: WASM is single-threaded; the pointer is only accessed under the
// cooperative-scheduler invariant (no concurrent mutation).
unsafe impl Send for WasmSleepCtx {}

/// Timer callback fired when a sleeping actor's deadline passes.
///
/// Transitions the actor from `Sleeping` to `Runnable` and re-enqueues it.
/// Drops the `WasmSleepCtx` and removes its handle from `SLEEP_HANDLES`.
///
/// # Safety
///
/// Called by `timer_wheel_tick_to` after the wheel lock is released.
/// The actor pointer stored in `data` must still be valid — `cancel_actor_sleep_queue_entry`
/// ensures this by removing the wheel entry (and thus preventing this callback from
/// firing) before the actor is freed.
unsafe extern "C" fn wasm_sleep_cb(data: *mut c_void) {
    // SAFETY: data is a Box<WasmSleepCtx> allocated by park_actor_sleep.
    let ctx = unsafe { Box::from_raw(data.cast::<WasmSleepCtx>()) };
    let actor = ctx.actor;
    drop(ctx);

    // SAFETY: Single-threaded; remove the now-consumed handle from the registry.
    unsafe {
        if let Some(ref mut handles) = SLEEP_HANDLES {
            handles.remove(&(actor as usize));
        }
        WASM_SLEEP_COUNT = WASM_SLEEP_COUNT.saturating_sub(1);
    }

    // Transition Sleeping → Runnable and re-enqueue.
    // SAFETY: actor is alive — cancel_actor_sleep_queue_entry guarantees the
    // wheel entry (and this callback) are removed before actor free.
    let state = unsafe { (*actor).actor_state.load(Ordering::Relaxed) };
    if state == HewActorState::Sleeping as i32 {
        // SAFETY: actor is alive (see above); actor_state and try_sched_enqueue
        // only require a valid pointer to a live HewActor.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Relaxed);
            if let Err(msg) = try_sched_enqueue(actor) {
                // Scheduler was shut down while the actor was sleeping.
                // The actor cannot be enqueued — log the situation but don't
                // panic; shutdown drains the wheel first so this branch
                // should never fire in practice.
                crate::set_last_error(msg);
            }
        }
    }
    // If the actor's state is no longer Sleeping (e.g., it was stopped or
    // crashed between the sleep park and the deadline), discard silently.
}

/// Return the WASM global timer wheel, lazily initialising it on first call.
///
/// Returns null only if `hew_timer_wheel_new` fails (allocation error), which
/// is treated as a non-fatal scheduler degradation: sleep/periodic timers
/// simply won't fire until the wheel is available.
///
/// # Safety
///
/// Must only be called from a single-threaded WASM context or a serialised
/// test environment.
pub(crate) unsafe fn wasm_timer_wheel() -> *mut HewTimerWheel {
    // In the test build, honour a per-test override that simulates an OOM
    // failure from `hew_timer_wheel_new`.  This lets tests exercise the
    // fail-closed paths in `park_actor_sleep` and `hew_actor_schedule_periodic`
    // without requiring actual memory-allocation failure.
    #[cfg(test)]
    if TEST_FORCE_WHEEL_NULL.load(Ordering::Relaxed) {
        return std::ptr::null_mut();
    }
    // SAFETY: Single-threaded cooperative scheduler.
    unsafe {
        if WASM_TIMER_WHEEL.is_null() {
            // SAFETY: hew_timer_wheel_new has no preconditions.
            WASM_TIMER_WHEEL = hew_timer_wheel_new();
        }
        WASM_TIMER_WHEEL
    }
}

/// Return the raw wheel pointer without initialising a new one.
///
/// Returns null if the wheel has not been created yet or was already freed
/// by `wasm_timers_shutdown_inner`.  Used by `timer_periodic_wasm` helpers
/// that must not inadvertently re-create the wheel during teardown.
///
/// # Safety
///
/// `WASM_TIMER_WHEEL` is a mutable static; caller must ensure single-threaded access.
pub(crate) unsafe fn wasm_timer_wheel_raw() -> *mut HewTimerWheel {
    // SAFETY: caller upholds the single-threaded WASM invariant.
    unsafe { WASM_TIMER_WHEEL }
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

/// Park `actor` in the timer wheel until `deadline_ms`.
///
/// Sets the actor state to `Sleeping` and inserts a one-shot wheel entry
/// whose callback transitions the actor back to `Runnable` and re-enqueues it.
/// The actor is NOT in the run queue while sleeping.
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
    // delivered when the timer fires and the sleep callback re-enqueues.
    a.actor_state
        .store(HewActorState::Sleeping as i32, Ordering::Relaxed);

    // SAFETY: Single-threaded; wasm_timer_wheel has no preconditions here.
    let wheel = unsafe { wasm_timer_wheel() };
    if wheel.is_null() {
        // Wheel unavailable (allocation failure) — fall back to immediate
        // re-enqueue so the actor doesn't park forever.
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // SAFETY: actor is valid; enqueue is safe.
        unsafe {
            if let Err(msg) = try_sched_enqueue(actor) {
                crate::set_last_error(msg);
            }
        }
        return;
    }

    // Allocate the callback context (Box-owned; freed by callback or cancel).
    let ctx = Box::into_raw(Box::new(WasmSleepCtx { actor }));

    // Schedule the absolute request directly. The cooperative host may leave
    // the wheel cursor stale between ticks, so translating through a separately
    // sampled relative delay can shift the effective deadline under load.
    // SAFETY: wheel is valid; ctx and actor are live.
    let handle =
        unsafe { timer_wheel_schedule_at_handle(wheel, deadline_ms, wasm_sleep_cb, ctx.cast()) };

    if handle.entry.is_null() {
        // Wheel rejected the schedule (e.g. entry allocation failure) — fail
        // closed, matching the wheel-null path above.  Drop the Box, restore
        // the actor to Runnable, and re-enqueue so it is not stranded.
        // SAFETY: ctx is exclusively owned (not yet in SLEEP_HANDLES).
        unsafe { drop(Box::from_raw(ctx)) };
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // SAFETY: actor is valid; enqueue is safe.
        unsafe {
            if let Err(msg) = try_sched_enqueue(actor) {
                crate::set_last_error(msg);
            }
        }
        return;
    }

    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative WASM scheduler; no concurrent mutation"
    )]
    // SAFETY: Single-threaded; SLEEP_HANDLES is not aliased here.
    unsafe {
        let map = SLEEP_HANDLES.get_or_insert_with(std::collections::HashMap::new);
        map.insert(actor as usize, handle);
        WASM_SLEEP_COUNT += 1;
    }
}

/// Remove any pending sleep-wheel entry for `actor`, if present.
///
/// Called before an actor is freed (`cleanup_all_actors`, `hew_actor_close`,
/// `hew_actor_stop`) to prevent the wheel's callback from firing on a freed
/// actor pointer.  The callback data (`WasmSleepCtx`) is freed here so no
/// memory is leaked.
///
/// Idempotent: if no entry exists for the actor, this is a no-op.
///
/// # Safety
///
/// Must be called from the single-threaded WASM cooperative scheduler
/// context (same thread that owns the wheel and `SLEEP_HANDLES`).
pub(crate) unsafe fn cancel_actor_sleep_queue_entry(actor: *mut crate::actor::HewActor) {
    // SAFETY: Single-threaded; SLEEP_HANDLES and the wheel are not aliased.
    unsafe {
        let wheel = WASM_TIMER_WHEEL;
        if wheel.is_null() {
            return;
        }
        if let Some(ref mut handles) = SLEEP_HANDLES {
            if let Some(handle) = handles.remove(&(actor as usize)) {
                // Remove the wheel entry atomically. If the entry is still
                // pending, `hew_timer_wheel_remove` unlinks and frees the
                // `HewTimerEntry` node and returns the data pointer (our ctx).
                // If the entry had already been collected for firing (not
                // possible in single-threaded WASM, but guarded for safety),
                // this returns null and the callback is responsible for the ctx.
                let data = hew_timer_wheel_remove(wheel, handle.entry, handle.generation);
                if !data.is_null() {
                    drop(Box::from_raw(data.cast::<WasmSleepCtx>()));
                    WASM_SLEEP_COUNT = WASM_SLEEP_COUNT.saturating_sub(1);
                }
            }
        }
    }
}

/// Drain the WASM timer wheel to `now_ms`, firing all expired sleep and
/// periodic callbacks.
///
/// Replaces the former `drain_expired_sleepers` + `drain_ready_periodic`
/// two-pass approach.  Returns `(fired_total, 0)` — the second element is
/// kept for call-site compatibility with [`hew_wasm_timer_tick`].
///
/// # Safety
///
/// Must be called from the single-threaded WASM context after
/// [`hew_sched_init`].
unsafe fn drain_timed_work(now_ms: u64) -> (u32, u32) {
    if shutdown_phase() != WasmShutdownPhase::Running {
        return (0, 0);
    }

    // SAFETY: Single-threaded; wasm_timer_wheel init is guarded.
    let wheel = unsafe { wasm_timer_wheel() };
    if wheel.is_null() {
        return (0, 0);
    }
    // SAFETY: wheel is valid; caller upholds single-threaded invariant.
    #[expect(
        clippy::cast_sign_loss,
        reason = "fired count fits in u32; timer_wheel_tick_to returns non-negative i32"
    )]
    // SAFETY: wheel is valid (wasm_timer_wheel() ensures it was allocated);
    // single-threaded WASM scheduler guarantees no concurrent mutation.
    let fired = unsafe { timer_wheel_tick_to(wheel, now_ms) } as u32;
    (fired, 0)
}

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
    // A new scheduler session owns a fresh cleanup opportunity. The actor
    // handle registry itself is reopened only after the preceding cleanup has
    // drained every route; process-wide token allocation is never reset.
    WASM_CLEANUP_RAN.store(false, Ordering::Release);
    // Register the tracing reset hook so session_reset() clears trace events
    // on WASM just as it does on the native path.  Without this the hook list
    // stays empty and session_reset() in hew_sched_shutdown is a no-op.
    crate::tracing::register_trace_reset_hook();
    // Initialise the bridge after tracing so its handler-name reset hook is
    // appended later in the shared session-reset ordering.
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
/// Any actor that calls `sleep_ms` during this drain will schedule a new
/// sleep entry on the wheel; the wheel is cleared after this function returns.
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
/// whose timer has not yet expired. The run-queue drain itself never waits on
/// the timer wheel. Timed work stays alive until the queue is empty so a parked
/// continuation's destroy cleanup can still cancel its registration; the wheel
/// is cleared once, immediately after the parked-activation retirement pass.
///
/// Resetting every static (including `ACTIVATING`, `PREV_ARENA`, and the
/// metrics counters) ensures that a subsequent [`hew_sched_init`] starts
/// from a genuinely clean slate even after hot-reload or test-harness reuse.
/// Reply-channel state is no longer a scheduler static; it lives on the
/// per-activation `HewExecutionContext` and naturally clears with the frame.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_shutdown() {
    // Keep the wheel allocated but make it inert before executing any user
    // code in the queue drain. A dispatch may call hew_actor_cooperate, which
    // re-enters hew_wasm_sched_tick; drain_timed_work consults this phase and
    // therefore cannot fire/re-arm due work during shutdown.
    // SAFETY: Single-threaded on WASM.
    unsafe {
        SHUTDOWN_PHASE = WasmShutdownPhase::Draining;
    }

    // Drain all currently-runnable actors without waiting for sleep deadlines.
    // Existing timed registrations remain intact but cannot fire unless this
    // single thread explicitly ticks the wheel; retaining them here is
    // load-bearing for continuation cleanup below.
    // SAFETY: Single-threaded on WASM.
    unsafe { drain_run_queue_for_shutdown() };

    // The unique parked-frame ownership window: the run queue is empty, no
    // activation is in progress and the synchronous single-threaded host
    // cannot interleave a resume, while timer/cancel machinery is still alive
    // for `coro.destroy` cleanup outlines. This pass does not untrack actors;
    // ordinary box/resource ownership stays with `hew_runtime_cleanup`.
    // SAFETY: post-drain single-threaded shutdown, before timer teardown.
    unsafe {
        SHUTDOWN_PHASE = WasmShutdownPhase::Retiring;
    }
    #[cfg(target_arch = "wasm32")]
    unsafe {
        debug_assert_eq!(hew_sched_metrics_global_queue_len(), 0);
        // A host may invoke shutdown while `ACTIVATING` is still set after a
        // mid-activation abort (the stale-state reset contract below). In that
        // case exclusive frame ownership is not proven: skip the destructive
        // pass and leave any parked actor for fail-closed cleanup rather than
        // treating "single-threaded" as permission to guess.
        if !std::ptr::addr_of!(ACTIVATING).read() {
            crate::actor::retire_parked_activations_wasm();
        }
        // Retiring an unanswered ask publishes an orphaned reply. Its reply
        // channel normally wakes a parked waiter through enqueue_resume; that
        // edge is suppressed in Retiring so shutdown cannot resurrect a frame
        // this pass owns. Any other producer reaching the queue here is a new
        // lifecycle edge that must be classified before timer teardown.
        debug_assert_eq!(
            hew_sched_metrics_global_queue_len(),
            0,
            "parked retirement must not publish new runnable work"
        );
    }

    // With every parked activation either reclaimed or deliberately left
    // fail-closed, discard all timer entries, including any created during the
    // shutdown drain or cancelled by a continuation cleanup above.
    // SAFETY: Single-threaded; drain and parked retirement have returned.
    unsafe {
        SHUTDOWN_PHASE = WasmShutdownPhase::TimerTeardown;
        wasm_timers_shutdown_inner();
        PENDING_SLEEP_DEADLINE_MS = 0;
    }

    crate::bridge::bridge_shutdown();

    // Fire all registered session reset hooks (tracing clear, profiler registry
    // clear on native, etc.).  Actor drain has completed; bridge state is
    // cleared; hooks run before scheduler statics are zeroed so that any hook
    // that inspects scheduler state sees a quiesced but not-yet-cleared runtime.
    crate::session::session_reset();

    // SAFETY: Single-threaded on WASM.
    unsafe {
        RUN_QUEUE = None;
        INITIALIZED = false;
        // Reset activation-context statics so stale state from a prior
        // mid-activation abort or skipped shutdown cannot bleed into a
        // subsequent init → use cycle.
        ACTIVATING = false;
        SHUTDOWN_PHASE = WasmShutdownPhase::Running;
        COOPERATIVE_TICK_DEPTH = 0;
        PREV_ARENA = std::ptr::null_mut();
        // Reset metrics so a re-init cycle starts from zero.
        TASKS_SPAWNED = 0;
        TASKS_COMPLETED = 0;
        MESSAGES_SENT = 0;
        MESSAGES_RECEIVED = 0;
        PENDING_SLEEP_DEADLINE_MS = 0;
    }
}

/// Inner helper: drain all sleep and periodic timer entries, free their
/// callback data, destroy and null the global wheel.
///
/// Called after the run-queue drain and parked-activation retirement. The drain
/// never waits on timer deadlines, so one final teardown bounds shutdown while
/// keeping cancel machinery live for continuation cleanup.
///
/// # Safety
///
/// Single-threaded; no concurrent timer access.
unsafe fn wasm_timers_shutdown_inner() {
    // Step 1: clear periodic timers via timer_periodic_wasm (owns the ctx
    // registry for periodic entries; uses the same wheel).
    // SAFETY: Single-threaded; no concurrent periodic-timer access.
    unsafe { crate::timer_periodic_wasm::hew_periodic_shutdown() };

    // Step 2: clear sleep entries from SLEEP_HANDLES, removing each from the
    // wheel so the WasmSleepCtx is freed exactly once.
    // SAFETY: Single-threaded; SLEEP_HANDLES and wheel not aliased.
    unsafe {
        let wheel = WASM_TIMER_WHEEL;
        if !wheel.is_null() {
            if let Some(ref mut handles) = SLEEP_HANDLES {
                for (_, handle) in handles.drain() {
                    let data = hew_timer_wheel_remove(wheel, handle.entry, handle.generation);
                    if !data.is_null() {
                        drop(Box::from_raw(data.cast::<WasmSleepCtx>()));
                    }
                }
            }
        }
        SLEEP_HANDLES = None;
        WASM_SLEEP_COUNT = 0;
    }

    // Step 3: destroy the wheel.  All HewTimerEntry nodes still in the wheel
    // (if any remain after the two drain steps above) are freed here.
    // SAFETY: Single-threaded; no other reference to WASM_TIMER_WHEEL exists.
    unsafe {
        if !WASM_TIMER_WHEEL.is_null() {
            hew_timer_wheel_free(WASM_TIMER_WHEEL);
            WASM_TIMER_WHEEL = std::ptr::null_mut();
        }
    }
}

/// Clean up all remaining runtime resources after shutdown.
///
/// WASM counterpart of the native `hew_runtime_cleanup()`. Frees any
/// actors not explicitly freed by user code and clears the registry.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_runtime_cleanup() {
    if WASM_CLEANUP_RAN.swap(true, Ordering::AcqRel) {
        return;
    }

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
/// Loops until both the run queue and the timer wheel are empty: pops
/// the front actor, activates it, and re-enqueues it if it still has
/// pending messages.  Between activation rounds, drains any sleeping or
/// periodic actors whose deadline has passed (using the real/simulated clock).
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
        // A nested standalone drain cannot make timer progress once shutdown
        // owns the scheduler. In particular, `drain_timed_work` deliberately
        // goes inert while Draining; without this guard a pending timer count
        // could keep a re-entrant `hew_sched_run` spinning forever.
        if shutdown_phase() != WasmShutdownPhase::Running {
            break;
        }

        // SAFETY: hew_now_ms is safe on all targets; drain is single-threaded.
        let now = unsafe { hew_now_ms() };
        // SAFETY: Single-threaded; timer wheel is owned by the cooperative scheduler.
        unsafe {
            let _ = drain_timed_work(now);
        };

        // SAFETY: Single-threaded on WASM.
        if !unsafe { step_one_actor() } {
            // Run queue empty. Stop only when no timed work remains.
            // SAFETY: single-threaded; WASM_SLEEP_COUNT and pending_periodic_count
            // are only mutated by scheduler paths that run on this thread.
            let timed_work_pending = unsafe {
                WASM_SLEEP_COUNT > 0 || crate::timer_periodic_wasm::pending_periodic_count() > 0
            };
            if !timed_work_pending {
                break;
            }
            // Sleeping actors remain: spin-poll until the next deadline passes.
            // This is a cooperative spin; in WASI the OS may preempt us.
        }
    }
}

/// Drain and tear down a standalone WASM program at normal `main` return.
///
/// `hew_sched_run` is intentionally reusable by host-driven embeddings and
/// re-entrant waits; it must not own process teardown.  The generated
/// standalone WASM entry epilogue calls this helper instead so short-lived
/// actor programs get the same shutdown -> cleanup chain native programs run
/// after their drain epilogue.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_wasm_runtime_exit() {
    hew_sched_run();
    hew_sched_shutdown();
    hew_runtime_cleanup();
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

/// Wake a `Suspended` actor whose parked continuation became resumable (wasm
/// cooperative half). The single resume edge every wasm readiness source feeds,
/// mirroring the native `scheduler::enqueue_resume` over the same ABI. Publishes
/// to the run queue before storing `Suspended -> Runnable`; if shutdown already
/// took the queue, leaves the parked frame untouched for cleanup. Records a
/// pending wake when the park has not yet published a handle (FG3 window).
///
/// # Safety
///
/// `actor`, if non-null, must reference a live `HewActor`. `cont`, if non-null,
/// is the continuation parked on `actor`.
pub unsafe fn enqueue_resume(actor: *mut HewActor, cont: *mut c_void) {
    if actor.is_null() {
        return;
    }
    if matches!(
        shutdown_phase(),
        WasmShutdownPhase::Retiring | WasmShutdownPhase::TimerTeardown
    ) {
        // Shutdown owns every parked frame in these phases. Publishing a wake
        // would race that ownership logically even on this single thread:
        // orphan-reply retirement can synchronously call this function.
        return;
    }
    let a = as_native_actor(actor);
    // SAFETY: single-threaded; actor valid.
    let state = unsafe { (*actor).actor_state.load(Ordering::Relaxed) };
    let parked = a.suspended_cont.load(Ordering::Relaxed);
    if parked.is_null() || state != HewActorState::Suspended as i32 {
        // Park not yet published (or actor not Suspended): record the wake so
        // the suspend edge drains it. Fail-closed on a terminal actor (it will
        // never park again, so the mark is harmless).
        let _ = cont; // handle owned by the suspend edge.
        crate::coro_exec::mark_pending_wake(a);
        return;
    }
    // Publish before committing the lifecycle transition. Shutdown takes
    // RUN_QUEUE before the later runtime-cleanup sweep; that sweep may retire
    // an orphaned ask, whose synchronous reply path reaches this wake after
    // the scheduler no longer accepts work. In that case the reply is still
    // resolved and its sender reference consumed, but the caller's parked
    // frame remains owned by cleanup. Marking it Runnable without a queue
    // entry would both lie about that ownership and strand the frame.
    //
    // Single-threaded WASM makes this ordering safe: no scheduler step can
    // observe the queued actor between the push and the following state store.
    // SAFETY: actor is valid and the cooperative scheduler is single-threaded.
    unsafe {
        if try_sched_enqueue(actor).is_ok() {
            (*actor)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        } else {
            debug_assert!(
                !std::ptr::addr_of!(INITIALIZED).read(),
                "initialized WASM scheduler lost its run queue"
            );
        }
    }
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
        let _ = drain_timed_work(now);

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

/// Advance the WASM timer queues: deliver due periodic messages and
/// re-enqueue all sleeping actors whose deadline ≤ `now_ms`.
///
/// Host-driven alternative to relying on the clock inside
/// [`hew_wasm_sched_tick`].  Useful for JS hosts that receive
/// `setTimeout` callbacks with a precise timestamp, or for WASI
/// programs that advance the clock via `clock_time_get`.
///
/// `now_ms` must use the same monotonic clock and epoch as the host's
/// `hew_now_ms` import, which supplies timer deadlines at registration. A
/// later-epoch value can advance the wheel past those deadlines and cause
/// subsequent registrations to be clamped to the cursor and fire immediately;
/// an earlier-epoch value can delay delivery until it reaches the stored
/// deadlines.
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
        reason = "number of timer events will not exceed i32::MAX"
    )]
    // SAFETY: caller upholds single-threaded cooperative scheduler invariant.
    unsafe {
        let (periodic, sleepers) = drain_timed_work(now_ms);
        periodic.saturating_add(sleepers) as i32
    }
}

/// Return the number of pending timed work items (sleeping actors + active periodic timers).
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
        reason = "timer count will not exceed i32::MAX"
    )]
    // SAFETY: Single-threaded cooperative scheduler; counts are only mutated
    // by sleep park/cancel/fire (single-threaded) and periodic schedule/cancel.
    unsafe {
        (WASM_SLEEP_COUNT + crate::timer_periodic_wasm::pending_periodic_count()) as i32
    }
}

// ── Slice-4 suspend/resume executor edges (wasm cooperative half) ────────

/// Cast a wasm `*mut HewActor` to the byte-identical native `HewActor` the
/// target-agnostic `coro_exec` guards operate on. The layouts are asserted
/// equal at compile time (see the `const _` block above), so this is sound.
#[inline]
fn as_native_actor<'a>(actor: *mut HewActor) -> &'a crate::actor::HewActor {
    // SAFETY: wasm and native HewActor have identical layout (compile-time
    // asserted); `actor` is a live actor owned by the cooperative scheduler.
    unsafe { &*(actor.cast::<crate::actor::HewActor>()) }
}

/// Resolve the reply a suspending handler still owed its `ask` caller, on a
/// wasm path that abandons the parked activation without ever resuming it.
///
/// The wasm mirror of `scheduler::retire_suspended_reply_channel`, and the
/// enforcement point for the slot invariant this module now maintains:
///
/// > `suspended_reply_channel` is non-null IFF the actor slot OWNS an
/// > unconsumed sender-side reference that somebody still has to resolve.
///
/// The suspend edge establishes it by moving the node's reference in only when
/// the handler has not already replied; every abandonment path discharges it
/// through here. EXACTLY ONCE: the slot is taken with a `swap`, so only the
/// caller that observes the non-null pointer publishes and releases. Two paths
/// racing to abandon the same activation -- a stop that is immediately followed
/// by a free -- resolve it once between them.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) fn retire_suspended_reply_channel_wasm(a: &crate::actor::HewActor) {
    let ch = a
        .suspended_reply_channel
        .swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !ch.is_null() {
        // SAFETY: by the slot invariant a non-null slot is an owned, unconsumed
        // sender-side reference transferred from the mailbox node at suspend.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_retire_orphaned_ask_sender_ref(ch.cast());
        }
    }
}

/// Cancel a parked activation because an out-of-band stop was latched (wasm).
///
/// The wasm mirror of `scheduler::cancel_parked_activation_for_stop`. Destroy
/// the continuation once, re-arm the executor slot, discharge the activation's
/// debts, and settle. A refused destroy proves that this frame does not own the
/// continuation: leave every slot and latch untouched so a later proven owner
/// can complete teardown.
///
/// # Safety
///
/// `actor` is owned by the calling activation and is being driven terminal.
#[must_use = "a refused stop cancellation leaves the parked activation live and must be handled fail-closed"]
#[cfg(any(target_arch = "wasm32", test))]
unsafe fn cancel_parked_activation_for_stop_wasm(actor: *mut HewActor) -> bool {
    let a = as_native_actor(actor);
    // WASM has no legal generator-sink producer. Preserve the entire
    // activation if invariant corruption populates the slot.
    #[cfg(target_arch = "wasm32")]
    if crate::actor::refuse_wasm_lifecycle_cleanup_with_gen_sink(a) {
        return false;
    }
    // SAFETY: single-threaded; this frame owns the activation, so no resume can
    // be driving the frame.
    if !unsafe { crate::coro_exec::destroy_parked(a) }.is_ok() {
        return false;
    }
    let _ = crate::coro_exec::re_arm(a);
    crate::actor::clear_suspended_cancel_token(a);
    // The continuation is now proven gone, so nothing will ever read these
    // activation-owned debts again.
    retire_suspended_reply_channel_wasm(a);
    #[cfg(not(target_arch = "wasm32"))]
    crate::actor::fault_close_registered_gen_sink(a);
    // SAFETY: single-threaded; actor valid and owned by this frame.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
    }
    // SAFETY: the actor just went terminal and is not being dispatched.
    unsafe { crate::actor::call_terminate_fn(actor.cast()) };
    true
}

/// The three stop-latch consultations around a resumed WASM activation.
///
/// Keeping the site in the diagnostic makes a counterfactual refusal
/// actionable without changing the shared cancellation primitive.
#[cfg(any(target_arch = "wasm32", test))]
#[derive(Clone, Copy)]
enum StopCancelSite {
    BeforeRepark,
    AfterSuspendedPublish,
    BeforeResume,
}

#[cfg(any(target_arch = "wasm32", test))]
impl StopCancelSite {
    const fn diagnostic_name(self) -> &'static str {
        match self {
            Self::BeforeRepark => "before re-parking a pending resume",
            Self::AfterSuspendedPublish => "after publishing Suspended",
            Self::BeforeResume => "before resuming a parked activation",
        }
    }
}

/// Run stop cancellation at one latch consultation and preserve a refused
/// activation in its only honest scheduler state.
///
/// # Safety
///
/// `actor` is the live activation owned by the calling scheduler frame.
#[cfg(any(target_arch = "wasm32", test))]
#[must_use = "callers must stop activation settlement when cancellation is refused"]
unsafe fn cancel_parked_activation_for_stop_at(actor: *mut HewActor, site: StopCancelSite) -> bool {
    // SAFETY: caller owns this activation and forwards that ownership.
    if unsafe { cancel_parked_activation_for_stop_wasm(actor) } {
        return true;
    }

    let a = as_native_actor(actor);
    // The frame/debts remain live. `Running` would claim an activation is
    // still executing after this scheduler frame returns; terminal would
    // permit destructive cleanup. Suspended is the fail-closed owner state.
    a.actor_state
        .store(HewActorState::Suspended as i32, Ordering::Release);
    let message = format!(
        "WASM actor stop cancellation refused: actor {:#x} retained its parked \
         activation {}; actor left Suspended fail-closed",
        a.id,
        site.diagnostic_name()
    );
    crate::set_last_error(&message);
    eprintln!("hew: runtime error: {message}");
    false
}

/// The SUSPEND edge (wasm): park the current continuation and publish
/// `Suspended`. Single-threaded, so the two-phase park reduces to a store
/// ordering, but it goes through the SAME `coro_exec` guards as native for
/// parity. Returns `true` on a successful park.
///
/// # Safety
///
/// `actor` is owned by the calling activation (Running on this single thread);
/// `cont` is the live, suspended continuation handle the dispatch produced.
unsafe fn park_suspended_activation_wasm(actor: *mut HewActor, cont: *mut c_void) -> bool {
    let a = as_native_actor(actor);
    if !crate::coro_exec::begin_park(a).is_ok() {
        // P1-B (parity with native): begin_park refused, but we still OWN
        // `cont` (the dispatch produced it and it was never stored). Destroy it
        // rather than dropping it silently — a dropped handle leaks the coro
        // frame + any frame-owned heap values.
        // SAFETY: `cont` is the live, not-yet-parked, not-yet-destroyed frame
        // this activation produced; no other owner exists.
        unsafe { crate::cont::hew_cont_destroy(cont) };
        return false;
    }
    // SAFETY: `cont` is a live suspended continuation per the fn contract.
    unsafe { crate::coro_exec::finish_park(a, cont) };
    // SAFETY: wasm HewActor; single-threaded store.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Relaxed);
    }
    // Drain a wake that fired during the park (FG3): re-enqueue if so.
    if crate::coro_exec::take_pending_wake(a) {
        // SAFETY: single-threaded; actor valid.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Relaxed);
            sched_enqueue(actor);
        }
    }
    true
}

/// Park a SUSPENDED lifecycle-hook continuation against a freshly-spawned (Idle)
/// actor — the wasm counterpart of the native `hew_actor_park_lifecycle_cont`
/// (scheduler.rs). The lifecycle spawn site (codegen) runs `init`/`#[on(start)]`
/// synchronously while the actor is `Idle`; a suspending hook hands back its
/// `coro.begin` handle, which this parks so the wasm cooperative scheduler's
/// resume re-entry (`resume_suspended_activation_wasm` via
/// `has_live_parked_cont`) drives it to completion on its sleep-timer wake.
/// Single-threaded, so the two-phase park reduces to a store ordering, but it
/// goes through the SAME `coro_exec` guards as native for parity.
///
/// Returns `true` when parked; `false` (and destroys the handle) when refused.
///
/// # Safety
///
/// `actor` must be the live, Idle, freshly-spawned actor; `cont` is the live,
/// suspended continuation the lifecycle ramp produced; the caller must have
/// released the actor state lock before this call.
///
/// `not(test)` `no_mangle`: native test builds also compile this module
/// (`cfg(any(wasm32, test))`) alongside the native `scheduler.rs` export of the
/// same symbol, so the wasm export drops its mangling under `test` to avoid a
/// duplicate-symbol clash — the same pattern `hew_sched_init` et al. use.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_actor_park_lifecycle_cont(
    actor: *mut HewActor,
    cont: *mut c_void,
) -> bool {
    if actor.is_null() {
        // SAFETY: `cont` (if non-null) is the live, not-yet-parked frame; no other
        // owner exists. Null-safe destroy avoids leaking the coro frame.
        unsafe { crate::cont::hew_cont_destroy(cont) };
        return false;
    }
    let a = as_native_actor(actor);
    if !crate::coro_exec::begin_park(a).is_ok() {
        // SAFETY: we still own `cont` (never stored); destroy rather than leak.
        unsafe { crate::cont::hew_cont_destroy(cont) };
        return false;
    }
    // SAFETY: `cont` is a live suspended continuation per the fn contract.
    unsafe { crate::coro_exec::finish_park(a, cont) };
    // Publish `Suspended` from the Idle spawn-window state. The actor is
    // unreachable to senders here (its handle/slot is stored only after lifecycle
    // completes), so on the single-threaded wasm scheduler this is a plain store.
    // SAFETY: wasm HewActor; single-threaded store.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Relaxed);
    }
    // Drain a wake that fired during the park (FG3): re-enqueue if so.
    if crate::coro_exec::take_pending_wake(a) {
        // SAFETY: single-threaded; actor valid.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Relaxed);
            sched_enqueue(actor);
        }
    }
    true
}

/// The RESUME re-entry (wasm): drive the parked continuation to its next
/// suspend or completion, mirroring the native `resume_suspended_activation`.
///
/// # Safety
///
/// `actor` is owned by the calling activation (Running on this single thread).
#[expect(
    clippy::needless_return,
    reason = "keep the #[must_use] stop-cancellation refusal visibly fail-closed at its call site"
)]
unsafe fn resume_suspended_activation_wasm(actor: *mut HewActor) {
    let a = as_native_actor(actor);

    // W6.010 value routing (parity with native `resume_suspended_activation`):
    // re-establish an execution context carrying the handler's stashed reply
    // channel (saved at park) BEFORE driving the resume, so the resumed body's
    // final-return `hew_reply` (via `hew_get_reply_channel`) deposits the reply
    // to the handler's caller. The suspend tore down the original dispatch
    // context; without this the body would see no reply channel and the caller
    // would hang. The context also re-establishes actor/arena/lock_seat so a
    // post-resume self/state/context read reads the live values, not a stale
    // frame — the same fix the native path and the codegen coro-aware context
    // readers rely on. The context is a scheduler-owned stack carrier restored
    // after the resume step.
    let stashed_reply = a.suspended_reply_channel.load(Ordering::Acquire);
    let mut resume_context = crate::execution_context::HewExecutionContext {
        actor: actor.cast::<c_void>().cast::<crate::actor::HewActor>(),
        actor_id: a.id,
        arena: a.arena.cast::<crate::arena::ActorArena>(),
        prev_context: crate::execution_context::current_context(),
        lock_seat: crate::actor::actor_state_lock_seat(actor.cast::<crate::actor::HewActor>()),
        reply_channel: stashed_reply,
        ..crate::execution_context::HewExecutionContext::default()
    };
    let prev_context = resume_context.prev_context;
    let installed_prev = crate::execution_context::set_current_context(&raw mut resume_context);
    debug_assert_eq!(installed_prev, prev_context);

    // SAFETY: parked handle is the executor-owned frame; resume_park enforces
    // FG2/FG4 internally.
    let poll = unsafe { crate::coro_exec::resume_park(a) };

    // Whether the resumed body actually deposited a reply through the stashed
    // channel. Read from the resume context BEFORE the restore below, because
    // `hew_reply` sets the flag on the context that is installed at the time,
    // and that pointer is stale afterwards.
    let resume_reply_consumed =
        (resume_context.flags & crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED) != 0;

    // Restore the prior context now that the resume step (resume + poll, and any
    // body-side reply deposit it performed) has run. On Pending the handler
    // re-parked, so the stash stays for the next resume. Mirrors the native
    // restore exactly.
    let restored = crate::execution_context::set_current_context(prev_context);
    debug_assert_eq!(restored, &raw mut resume_context);
    if matches!(poll, Some(crate::cont::ResumePoll::Ready) | None) {
        if resume_reply_consumed {
            // The body deposited its reply, and `hew_reply` already consumed
            // the sender-side reference. The slot no longer owns anything, so
            // clear it -- releasing again would double-free.
            a.suspended_reply_channel
                .store(std::ptr::null_mut(), Ordering::Release);
        } else {
            // The continuation finished (or the resume was refused) WITHOUT
            // replying. Storing null here -- which is what this edge used to do
            // unconditionally -- drops the asking side's only reference on the
            // floor: the ask never resolves and the channel leaks. This is an
            // abandonment like any other, so discharge the debt.
            retire_suspended_reply_channel_wasm(a);
        }
        crate::actor::clear_suspended_cancel_token(a);
    }

    match poll {
        Some(crate::cont::ResumePoll::Pending) => {
            // Latch check BEFORE re-parking (parity with native
            // `settle_pending_resume`): a stop latched while the continuation
            // was executing must not be answered by parking again, or the actor
            // goes back to sleep holding the stop and the ask.
            // SAFETY: the mailbox pointer is valid for the actor's lifetime.
            if unsafe { crate::mailbox_wasm::mailbox_stop_requested(a.mailbox.cast()) } {
                // SAFETY: this frame owns the activation.
                if !unsafe {
                    cancel_parked_activation_for_stop_at(actor, StopCancelSite::BeforeRepark)
                } {
                    return;
                }
                return;
            }

            // Re-park: suspended again.
            // SAFETY: single-threaded; actor valid.
            unsafe {
                (*actor)
                    .actor_state
                    .store(HewActorState::Suspended as i32, Ordering::Relaxed);
            }
            if crate::coro_exec::take_pending_wake(a) {
                // SAFETY: single-threaded; actor valid.
                unsafe {
                    (*actor)
                        .actor_state
                        .store(HewActorState::Runnable as i32, Ordering::Relaxed);
                    sched_enqueue(actor);
                }
                return;
            }
            // Latch re-check AFTER publishing `Suspended`, mirroring native's
            // third consultation. Today WASM has no interleaving point between
            // the pre-repark check and this load (nested activation happens
            // inside the resumed body, before both); retain the check as
            // defensive parity for any future hook added to this settle window.
            // SAFETY: the mailbox pointer is valid for the actor's lifetime.
            if unsafe { crate::mailbox_wasm::mailbox_stop_requested(a.mailbox.cast()) } {
                // SAFETY: this frame owns the activation.
                if !unsafe {
                    cancel_parked_activation_for_stop_at(
                        actor,
                        StopCancelSite::AfterSuspendedPublish,
                    )
                } {
                    debug_assert_eq!(
                        a.actor_state.load(Ordering::Acquire),
                        HewActorState::Suspended as i32,
                        "refused stop cancellation must preserve Suspended ownership"
                    );
                    return;
                }
            }
        }
        Some(crate::cont::ResumePoll::Ready) | None => {
            // Completed (or refused): destroy exactly once (FG1) — which nulls
            // the slot (FG4) — then settle.
            // SAFETY: tag is Done or terminal; destroy_parked refuses a second
            // teardown.
            let _ = unsafe { crate::coro_exec::destroy_parked(a) };
            // P1-B (parity with native): re-arm `Destroyed → Empty` on the
            // quiescent edge so this actor can park a NEW continuation on its
            // next `await`. Fail-closed: only a Destroyed tag with a null slot
            // re-arms.
            let _ = crate::coro_exec::re_arm(a);
            // SAFETY: single-threaded; actor valid.
            unsafe { settle_after_activation_wasm(actor) };
        }
    }
}

/// Shared wasm post-activation settle for a completed resume: mirror the
/// run-to-completion drain's RUNNING -> RUNNABLE / IDLE transition so a queued
/// message is still served.
///
/// # Safety
///
/// `actor` is owned by the calling activation (Running on this single thread).
unsafe fn settle_after_activation_wasm(actor: *mut HewActor) {
    // SAFETY: single-threaded; actor valid.
    let a = unsafe { &*actor };
    let mailbox = a.mailbox;
    let has_more = if mailbox.is_null() {
        false
    } else {
        // SAFETY: mailbox pointer is valid for the actor's lifetime.
        unsafe { hew_mailbox_has_messages(mailbox) != 0 }
    };
    if has_more {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // SAFETY: actor valid; re-enqueue to serve the queued message.
        unsafe { sched_enqueue(actor) };
    } else {
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
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

    // Resume re-entry (slice-4 executor, wasm cooperative half). Same ABI and
    // discriminator as native (`scheduler.rs`): a live parked continuation
    // (`cont_tag == Parked` AND a non-null `suspended_cont` slot) means this
    // activation is a resumed continuation, not a fresh message dispatch.
    // Single-threaded, so no CAS race — but the SAME coro_exec guards (FG1-FG4)
    // drive resume/destroy through one ABI.
    //
    if crate::coro_exec::has_live_parked_cont(as_native_actor(actor)) {
        // OUT-OF-BAND STOP, checked BEFORE the resume so a stopping actor never
        // runs another slice of user code (parity with native
        // `activate_actor`). The loop-top check below is on the fresh-dispatch
        // path only -- this activation returns before reaching it, so without
        // this consultation a stop latched against a parked actor would resume
        // the continuation instead of cancelling it.
        // SAFETY: the mailbox pointer is valid for the actor's lifetime
        // (null-tolerant).
        if unsafe { crate::mailbox_wasm::mailbox_stop_requested(a.mailbox.cast()) } {
            // SAFETY: this frame owns the activation (state is Running).
            if !unsafe { cancel_parked_activation_for_stop_at(actor, StopCancelSite::BeforeResume) }
            {
                return;
            }
            return;
        }
        // SAFETY: actor is Running and exclusively owned on this single thread;
        // the parked handle is the executor-owned frame.
        unsafe { resume_suspended_activation_wasm(actor) };
        return;
    }

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
    //
    // Reply-channel state is *not* saved here: it lives on the per-activation
    // `HewExecutionContext` constructed below. Nested activations install
    // their own ctx and therefore cannot clobber the outer arm's reply channel
    // — the outer ctx is automatically restored when the inner activation
    // pops its frame via `set_current_context(prev_context)`.
    // SAFETY: Single-threaded; no data races possible.
    let saved_activating: bool = unsafe { ACTIVATING };
    // SAFETY: Single-threaded; no data races possible.
    let saved_prev_arena: *mut c_void = unsafe { PREV_ARENA };

    // Install the canonical execution context that actor.rs self APIs and arena
    // routing read during this activation.
    let mut execution_context = crate::execution_context::HewExecutionContext {
        actor: actor.cast::<c_void>().cast::<crate::actor::HewActor>(),
        actor_id: a.id,
        arena: a.arena.cast::<crate::arena::ActorArena>(),
        prev_context: crate::execution_context::current_context(),
        lock_seat: crate::actor::actor_state_lock_seat(actor.cast::<crate::actor::HewActor>()),
        ..crate::execution_context::HewExecutionContext::default()
    };
    let prev_context = execution_context.prev_context;
    let installed_prev = crate::execution_context::set_current_context(&raw mut execution_context);
    debug_assert_eq!(installed_prev, prev_context);

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
    // Invariant (teardown-order contract): a non-null `mailbox` pointer must
    // remain live for the actor's entire scheduler lifetime — i.e. until the
    // actor has been removed from the run queue by a drain or shutdown.
    //
    // The production cleanup path (`free_actor_resources_wasm`) nulls this slot
    // BEFORE freeing the box; the `if !mailbox.is_null()` guard below then
    // safely skips the drain for that actor.
    //
    // Tests that hand-wire mailboxes onto stack `HewActor` instances must either
    // call `hew_sched_shutdown()` before `hew_mailbox_free()`, or use the
    // `drop_test_actor_mailbox` helper (which enforces the same null-before-free
    // order).  Freeing the mailbox while the actor is still Runnable in
    // `RUN_QUEUE` is a heap-use-after-free — the pointer remains non-null (so
    // this guard does not fire) but points to freed memory.
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

    // SUSPEND EDGE (D-A.2 / R326/R327): the `coro.begin` handle a handler hands
    // back when it suspends at a non-final `coro.suspend`. Captured across the
    // loop; a non-null handle is parked after the global restore (below).
    // Dormant today — no source construct produces a suspend, so this stays
    // null on every dispatch.
    let mut suspend_handle: *mut c_void = std::ptr::null_mut();

    if !mailbox.is_null() {
        // Process up to `budget` messages.
        for _ in 0..budget {
            // OUT-OF-BAND STOP, checked BEFORE any receive — the native shape
            // (sync parity). `hew_actor_stop` on a Running actor latches this
            // flag; it is not a message and never occupies a queue slot, so the
            // request cannot be lost when a `HewMsgNode` allocation or a
            // sys-queue growth fails. The sentinel node this replaces could be:
            // its producer allocated and grew before latching, and the caller
            // discarded the resulting `bool`.
            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            if unsafe { crate::mailbox_wasm::mailbox_stop_requested(mailbox.cast()) } {
                // Drive Running -> Stopping so the post-loop settle finalizes
                // the Stopping -> Stopped terminal transition (terminate
                // callback).
                let _ = a.actor_state.compare_exchange(
                    HewActorState::Running as i32,
                    HewActorState::Stopping as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                );
                break;
            }

            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            // Receive WITH provenance so a SYSTEM-queue lifecycle signal is not
            // confused with an application message sharing its value. Mirrors the
            // native `mailbox::mailbox_try_recv_with_origin` path (sync parity).
            let recv = unsafe { crate::mailbox_wasm::mailbox_try_recv_with_origin(mailbox.cast()) };
            let origin = recv.origin;
            let msg = recv.node.cast::<HewMsgNode>();
            if msg.is_null() {
                break;
            }

            // Route by the node's TYPED provenance — the exact native shape
            // (sync parity). A USER-queue node is never intercepted here
            // whatever its `msg_type`.
            let dispatch_target = match origin {
                Origin::Sys(kind) => {
                    let Some(sys_dispatch) = a.sys_dispatch else {
                        // Fail-closed: no system entry point registered, so the
                        // signal is dropped rather than downgraded onto the user
                        // trampoline.
                        // SAFETY: `msg` is exclusively owned by this scheduler tick.
                        unsafe { hew_msg_node_free(msg) };
                        continue;
                    };
                    Some(DispatchTarget::Sys(sys_dispatch, kind))
                }
                Origin::User => a.dispatch.map(DispatchTarget::User),
            };

            if let Some(dispatch) = dispatch_target {
                // Reset reduction counter for this dispatch.
                a.reductions
                    .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

                // SAFETY: `msg` is exclusively owned by this scheduler tick.
                let msg_ref = unsafe { &*msg };
                crate::tracing::hew_trace_begin(a.id, msg_ref.msg_type);
                // Install the per-message reply channel directly on the
                // activation's canonical context. The consumed flag is reset
                // before every dispatch so a previous handler's `hew_reply`
                // cannot bleed forward.
                execution_context.reply_channel = msg_ref.reply_channel;
                execution_context.flags &=
                    !crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;

                // SAFETY: `execution_context` is the scheduler-owned activation
                // context, and lock acquisition fails closed if its seat is absent
                // or poisoned.
                let lock_acquired = unsafe {
                    crate::actor::hew_actor_state_lock_acquire_for_context(
                        &raw mut execution_context,
                    )
                } == crate::actor::HEW_ACTOR_STATE_LOCK_OK;
                if !lock_acquired {
                    a.actor_state
                        .store(HewActorState::Crashed as i32, Ordering::Release);
                    execution_context.reply_channel = std::ptr::null_mut();
                    execution_context.flags &=
                        !crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
                    crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                    // SAFETY: msg is exclusively owned by this scheduler tick.
                    unsafe {
                        (*msg).reply_channel = std::ptr::null_mut();
                        hew_msg_node_free(msg);
                    }
                    break;
                }

                // SAFETY: `dispatch`, `ctx`, and `a.state` are valid; message
                // fields come from a well-formed `HewMsgNode`.
                //
                // D-A.2 (R326/R327): the trampoline returns the dispatch suspend
                // outcome as a nullable continuation handle — `null` for a
                // run-to-completion handler (every handler today; the suspend
                // substrate is dormant), or the `coro.begin` handle when a
                // handler suspended. The handle is captured here; the production
                // wasm park edge (commit 4) consumes a non-null handle.
                // SAFETY: this cooperative activation exclusively owns the
                // actor state until the matching finish/recovery call.
                let crash_state_drop = if a.state_drop_borrowed.load(Ordering::Acquire) {
                    None
                } else {
                    match (a.state_clone_fn, a.state_drop_fn) {
                        (Some(_), Some(drop)) => Some(drop),
                        (None, None) => None,
                        _ => panic!("actor state has half-registered clone/drop classifier proof"),
                    }
                };
                // SAFETY: this cooperative activation exclusively owns the
                // actor state until the matching finish/recovery call.
                if !unsafe {
                    crate::cont::begin_dispatch_crash_cleanup(
                        a.state,
                        a.state_size,
                        // Paired clone/drop registration is the MIR
                        // classifier's proof that the wrapper is safe to
                        // relocate into the crash escrow.
                        crash_state_drop,
                    )
                } {
                    panic!("could not establish WASM actor dispatch crash cleanup");
                }
                #[allow(
                    unused_mut,
                    reason = "wasm32 invokes this FnMut directly; host catch_unwind consumes it"
                )]
                let mut invoke_dispatch = || match dispatch {
                    DispatchTarget::User(user_dispatch) =>
                    // SAFETY: `user_dispatch` is the actor's registered
                    // application trampoline; message fields come from a
                    // well-formed `HewMsgNode`.
                    unsafe {
                        user_dispatch(
                            &raw mut execution_context,
                            a.state,
                            msg_ref.msg_type,
                            msg_ref.data,
                            msg_ref.data_size,
                            // P5-RX sub-stage 1: copy-mode receipt only.
                            // WASM-TODO(alias-messaging): envelope-mode (aliased) receive
                            // routing on the WASM scheduler is deferred to the
                            // WASM send gate; this path stays copy-mode (0).
                            0,
                        )
                    },
                    DispatchTarget::Sys(sys_dispatch, kind) => {
                        // SAFETY: `sys_dispatch` is the actor's registered system
                        // entry point and `kind` decoded from the system queue.
                        unsafe {
                            sys_dispatch(
                                &raw mut execution_context,
                                a.state,
                                kind.as_i32(),
                                msg_ref.data,
                                msg_ref.data_size,
                            );
                        }
                        std::ptr::null_mut()
                    }
                };
                // Host-side parity tests unwind so they can inspect the
                // scheduler's recovery bookkeeping. The production
                // wasm32-wasip1 sysroot is panic=abort: invoke directly so
                // this source does not imply an actor-containment boundary
                // that the artifact cannot provide.
                // WASM-TODO(actor-crash-containment): provide a target/runtime
                // unwind or explicit status ABI before treating handler panic
                // as a recoverable actor failure on Tier 2.
                #[cfg(not(target_arch = "wasm32"))]
                let dispatch_result = catch_unwind(AssertUnwindSafe(invoke_dispatch));
                #[cfg(target_arch = "wasm32")]
                let dispatch_result: Result<*mut c_void, ()> = Ok(invoke_dispatch());
                // D-A.2: the suspend handle the trampoline returned (null on the
                // run-to-completion path — every handler today). A non-null
                // handle is parked after the loop + global restore (below).
                suspend_handle = dispatch_result
                    .as_ref()
                    .copied()
                    .unwrap_or(std::ptr::null_mut());

                // SAFETY: `execution_context.lock_seat` was initialized from the
                // live actor immediately before the matching acquire.
                let release_result = unsafe {
                    crate::actor::hew_actor_state_lock_release_for_context(
                        &raw mut execution_context,
                    )
                };
                if release_result != crate::actor::HEW_ACTOR_STATE_LOCK_OK {
                    // SAFETY: dispatch returned and this activation owns the
                    // still-open cleanup scope.
                    let outcome =
                        unsafe { crate::cont::recover_dispatch_crash_cleanup_with_outcome(true) };
                    if outcome.state_authority_consumed {
                        // SAFETY: cooperative activation exclusively owns actor.
                        unsafe {
                            crate::actor::record_dispatch_state_drop_consumed(
                                actor.cast::<crate::actor::HewActor>(),
                            );
                        }
                    }
                    a.actor_state
                        .store(HewActorState::Crashed as i32, Ordering::Release);
                    execution_context.reply_channel = std::ptr::null_mut();
                    execution_context.flags &=
                        !crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
                    crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                    // SAFETY: msg is exclusively owned by this scheduler tick.
                    unsafe {
                        (*msg).reply_channel = std::ptr::null_mut();
                        hew_msg_node_free(msg);
                    }
                    break;
                }

                #[cfg(not(target_arch = "wasm32"))]
                if let Err(panic_payload) = dispatch_result {
                    crate::set_last_error("actor dispatch panicked");
                    // Tagged-crash surfacing: if the dispatch (or anything
                    // it called, e.g. `hew_arena_malloc` on cap exhaustion)
                    // stamped a HEW_TRAP_* code onto the actor before the
                    // panic, transition the actor to Crashed so
                    // ExitReason::from_error_code(actor.error_code) surfaces
                    // the named reason at the supervisor boundary. This is
                    // the WASM counterpart of the native longjmp seam,
                    // which jumps directly out of dispatch with the code
                    // already installed.
                    let cooperative_crash = a.error_code.load(Ordering::Acquire) != 0;
                    // SAFETY: catch_unwind proves the dispatch stack is
                    // abandoned and transfers its cleanup scope here.
                    let outcome = unsafe {
                        crate::cont::recover_dispatch_crash_cleanup_with_outcome(cooperative_crash)
                    };
                    if outcome.state_authority_consumed {
                        // SAFETY: cooperative activation exclusively owns actor.
                        unsafe {
                            crate::actor::record_dispatch_state_drop_consumed(
                                actor.cast::<crate::actor::HewActor>(),
                            );
                        }
                    }
                    if cooperative_crash {
                        a.actor_state
                            .store(HewActorState::Crashed as i32, Ordering::Release);
                    }
                    crate::util::quarantine_panic_payload(panic_payload);
                // SAFETY: normal dispatch return matches the scope opened
                // immediately before handler entry.
                } else if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
                    panic!("WASM actor dispatch returned with live crash-cleanup owners");
                }
                #[cfg(target_arch = "wasm32")]
                if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
                    panic!("WASM actor dispatch returned with live crash-cleanup owners");
                }

                let reply_consumed = (execution_context.flags
                    & crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED)
                    != 0;
                let actor_state = a.actor_state.load(Ordering::Acquire);
                execution_context.reply_channel = std::ptr::null_mut();
                execution_context.flags &=
                    !crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
                // SAFETY: msg is exclusively owned by this scheduler tick.
                let node_reply_channel = unsafe { (*msg).reply_channel };
                if !suspend_handle.is_null() && !reply_consumed && !node_reply_channel.is_null() {
                    // W6.010 suspend edge (parity with native, scheduler.rs:2563):
                    // a suspending handler still owes a reply to ITS caller. Stash
                    // this dispatch's reply channel on the actor and SKIP the normal
                    // teardown/free below — the channel reference is transferred to
                    // `suspended_reply_channel`, and the resume edge re-establishes a
                    // context carrying it so the resumed body deposits the reply.
                    // Without this the WASM suspend edge nulled + freed the channel
                    // here, leaving the resumed body with no channel and hanging the
                    // caller (the P1-wasm parity gap).
                    //
                    // This is a MOVE, not a copy: the node's sender-side reference
                    // becomes the actor slot's, and the node's pointer is nulled so
                    // `hew_msg_node_free` cannot also retire it. Owning it in exactly
                    // one place is what lets every abandonment path resolve it
                    // exactly once.
                    //
                    // The `reply_consumed` guard is what makes the slot invariant
                    // hold. A handler that called `hew_reply` before suspending owes
                    // nothing AND its reference has already been released by
                    // `hew_reply`'s trailing `hew_reply_channel_free`; stashing it
                    // anyway would leave the slot holding a pointer it does not own,
                    // and the next abandonment path would publish through a dead
                    // reference. Such a handler falls to the branch below, which
                    // nulls the node without a second release.
                    // SAFETY: msg is exclusively owned by this scheduler tick.
                    unsafe {
                        a.suspended_reply_channel
                            .store(node_reply_channel, Ordering::Release);
                        (*msg).reply_channel = std::ptr::null_mut();
                    }
                } else if reply_consumed
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
                crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);

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

            // Suspend edge: the handler suspended at a non-final `coro.suspend`.
            // Break out of the message loop without draining further; the park
            // (after the global restore) defers remaining messages until the
            // continuation completes. Dormant today (always null).
            if !suspend_handle.is_null() {
                break;
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
    // own context, arena, and reply channel again (Bug #1 + Bug #2 fix).
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
        ACTIVATING = saved_activating;
        // Restore the outer actor's pending sleep deadline so that a nested
        // activation (ask/await from dispatch) cannot erase it.  The inner
        // actor's own sleep deadline was captured in `actor_sleep_deadline`
        // (a local variable) during the dispatch loop above and is applied
        // further below; the global is no longer needed for the inner actor.
        PENDING_SLEEP_DEADLINE_MS = saved_pending_sleep;
        TASKS_COMPLETED += 1;
    }
    let restored_context = crate::execution_context::set_current_context(prev_context);
    debug_assert_eq!(restored_context, &raw mut execution_context);

    // ── Post-activation state transitions ───────────────────────────────

    let cur_state = a.actor_state.load(Ordering::Relaxed);

    // Stopping -> Stopped: finalise the lifecycle and invoke terminate callback.
    if cur_state == HewActorState::Stopping as i32 {
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
        trace_actor_stop_lifecycle(a.id, &raw mut execution_context);
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

    // SUSPEND EDGE (D-A.2 / R326/R327, wasm cooperative half): the handler
    // suspended at a non-final `coro.suspend` and handed back its `coro.begin`
    // frame handle. Park it against the executor and return WITHOUT settling to
    // Runnable/Idle — the wasm drain epilogue (`hew_sched_run`) drives resume of
    // parked conts when a wake (`enqueue_resume`) re-enqueues the actor. The
    // per-actor lock was released on the dispatch-return edge above. Done after
    // the global/arena restore so the actor is in a clean state. Dormant today
    // (no source construct produces a suspend, so `suspend_handle` is null).
    if !suspend_handle.is_null() {
        // SAFETY: `actor` is exclusively owned on this single thread; the lock
        // is released; `suspend_handle` is the live suspended continuation.
        let parked = unsafe { park_suspended_activation_wasm(actor, suspend_handle) };
        if parked {
            return;
        }
        // Park refused (actor concurrently stopped): the handle was destroyed
        // once inside the park guard. The suspend edge above already moved the
        // caller's reply reference into the actor slot and no resume will ever
        // consume it, so resolve it here before falling through to the standard
        // settle (parity with native, scheduler.rs:2678).
        retire_suspended_reply_channel_wasm(as_native_actor(actor));
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
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Relaxed);
            trace_actor_stop_lifecycle(a.id, &raw mut execution_context);
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

/// Return the total number of worker threads.
#[cfg_attr(not(test), no_mangle)]
#[must_use]
#[expect(static_mut_refs, reason = "single-threaded WASM metrics read")]
pub extern "C" fn hew_sched_metrics_worker_count() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { u64::from(RUN_QUEUE.is_some()) }
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

// `hew_get_reply_channel` lives in [`crate::execution_context`]; re-export so
// `crate::scheduler_wasm::hew_get_reply_channel` resolves at WASM call sites.
pub use crate::execution_context::hew_get_reply_channel;

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
/// WASM-TODO(cooperative-yield): native `hew_actor_cooperate` yields to the OS scheduler instead
/// of suppressing progress. Replace this depth cap with a stack-safe,
/// non-recursive cooperative driver so yielding never returns `1` without a
/// scheduler tick.
///
/// Returns 0 if the actor should continue, 1 if it yielded, and 2 if the
/// actor observed cancellation.
///
/// # Safety
///
/// No preconditions — may be called from any context. When called outside an
/// installed execution context, this returns 0 and leaves the thread-local
/// last-error slot untouched, so a real diagnostic set by a prior operation
/// survives.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
#[must_use]
pub extern "C" fn hew_actor_cooperate() -> c_int {
    // A fail-open cooperative-yield checkpoint must not disturb the caller's
    // LAST_ERROR: use the silent context read, not require_current_context()
    // (which sets EXECUTION_CONTEXT_NOT_INSTALLED as a side effect and would
    // clobber a real error a straight-line `main()` is about to read back).
    let ctx = crate::execution_context::current_context();
    if ctx.is_null() {
        return 0;
    }

    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    let (actor, cancel_token, scope) =
        unsafe { ((*ctx).actor, (*ctx).cancel_token, (*ctx).task_scope) };

    #[cfg(not(target_arch = "wasm32"))]
    {
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
    }

    #[cfg(target_arch = "wasm32")]
    {
        // WASM-TODO(scope): cross-task cancel_token / task_scope are
        // native-only until the WASI task-scope follow-on lands. The actor
        // task-state observation below covers the in-handler cancel source
        // that does exist on WASM (handler calls `hew_actor_stop_self`,
        // supervisor injects terminal state, etc.).
        let _ = (cancel_token, scope);
    }

    if actor.is_null() {
        return 0;
    }

    // SAFETY: actor was read from the installed canonical context.
    let a = unsafe { &*actor };

    // Observe actor-state cancellation: any terminal transition (Stopping,
    // Stopped, Crashed) that happened mid-handler must propagate to the
    // codegen-emitted `cooperate == 2 → cancel_exit` branch, matching the
    // native behaviour. On native this signal travels via task-scope cancel
    // tokens; on WASM (single-threaded, no task scopes) the observable cancel
    // sources within a handler are the actor's own state and the actor mailbox
    // closing under it. Reading them here turns previously-silent divergence
    // into the same fail-closed cancel exit that native produces.
    let actor_state = a.actor_state.load(Ordering::Acquire);
    if actor_state == HewActorState::Stopping as i32
        || actor_state == HewActorState::Stopped as i32
        || actor_state == HewActorState::Crashed as i32
    {
        return 2;
    }
    if !a.mailbox.is_null() {
        // SAFETY: actor mailbox pointer is owned by the live actor installed in
        // the current execution context.
        if unsafe { crate::mailbox_wasm::mailbox_is_closed(a.mailbox.cast()) } {
            return 2;
        }
    }

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
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};
    use std::ptr;
    #[cfg(not(target_arch = "wasm32"))]
    use std::sync::Arc;

    use crate::internal::types::HewError;

    #[test]
    fn local_pid_tail_preserves_canonical_actor_layout() {
        assert_eq!(
            std::mem::offset_of!(HewActor, local_pid_id),
            std::mem::offset_of!(crate::actor::HewActor, local_pid_id)
        );
        assert_eq!(
            std::mem::size_of::<HewActor>(),
            std::mem::size_of::<crate::actor::HewActor>()
        );
    }

    /// Build a minimal `HewActor` with sensible defaults.
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
            priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
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
            send_pin_count: std::sync::atomic::AtomicU32::new(0),
            gen_sink: AtomicPtr::new(ptr::null_mut()),
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
            spawn_serial: 1,
            sys_dispatch: None,
            state_drop_consumed: AtomicBool::new(false),
            state_drop_borrowed: AtomicBool::new(false),
            parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
        }
    }

    fn assert_last_error_eq(expected: &str) {
        let error = crate::hew_last_error();
        assert!(!error.is_null(), "runtime refusal must set hew_last_error");
        // SAFETY: `hew_last_error` returns a live NUL-terminated string until
        // the next call that mutates the thread-local error slot.
        let actual = unsafe { std::ffi::CStr::from_ptr(error) }.to_string_lossy();
        assert_eq!(actual, expected);
    }

    #[repr(C)]
    struct AskDispatchState {
        channel: *mut c_void,
        msg_type: i32,
        value: i32,
    }

    unsafe extern "C-unwind" fn reply_with_observed_channel(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        state: *mut c_void,
        msg_type: i32,
        data: *mut c_void,
        data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
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
                let _ = crate::reply_channel_wasm::hew_reply(
                    state.channel.cast(),
                    (&raw mut reply_value).cast(),
                    std::mem::size_of::<i32>(),
                );
            }
        }

        std::ptr::null_mut()
    }

    static NOISY_DISPATCHES: AtomicI32 = AtomicI32::new(0);
    static REPLY_DISPATCHES: AtomicI32 = AtomicI32::new(0);
    static LATE_REPLY_SAW_CANCELLED: AtomicBool = AtomicBool::new(false);

    unsafe extern "C-unwind" fn noisy_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        NOISY_DISPATCHES.fetch_add(1, Ordering::Relaxed);

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn reply_payload_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
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
            let _ = crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut reply_value).cast(),
                std::mem::size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn reply_payload_observes_cancelled_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
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
            let _ = crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut reply_value).cast(),
                std::mem::size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
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

    /// Fixed virtual-clock base (ms) for the timer-wheel tests below.
    ///
    /// Non-zero so it never collides with the `request_sleep` deadline==0
    /// sentinel, and not a multiple of the wheel's 256 ms L1 period so the
    /// slot/cascade math is exercised generally.
    const VIRTUAL_BASE_MS: u64 = 100_000;

    /// RAII guard that pins `hew_now_ms()` to a fixed virtual time for the
    /// duration of a timer-wheel test (wasm32 only), restoring the real clock
    /// on drop.
    ///
    /// WHY: the wasm timer tests drive `hew_wasm_timer_tick` / `drain_timed_work`
    /// with explicit deadlines derived from `hew_now_ms()`. Pinning a fixed
    /// virtual `now` makes their exact-boundary assertions independent of host
    /// execution time without weakening them with tolerances or sleeps.
    ///
    /// WHY wasm32-only: the virtual-clock seam lives in `wasm_stubs` (a
    /// wasm32-only module) and only the wasm `hew_now_ms` consults it. The wasm
    /// cooperative harness is single-threaded, so the seam needs no locking.
    /// Native runs these tests multi-threaded (num-cpus) but reads a different
    /// clock (`io_time::hew_now_ms`). Absolute timer scheduling preserves the
    /// requested boundary there, so the guard is inert and the real clock is
    /// left untouched.
    struct VirtualClock;

    impl VirtualClock {
        /// Pin `hew_now_ms()` to `base_ms` on wasm32 (inert on native — see the
        /// type doc). Every read then returns exactly `base_ms` until the guard
        /// drops; the tests never advance it.
        fn pinned_at(base_ms: u64) -> Self {
            #[cfg(target_arch = "wasm32")]
            crate::wasm_stubs::pin_virtual_clock(base_ms);
            #[cfg(not(target_arch = "wasm32"))]
            let _ = base_ms;
            VirtualClock
        }
    }

    impl Drop for VirtualClock {
        fn drop(&mut self) {
            #[cfg(target_arch = "wasm32")]
            crate::wasm_stubs::unpin_virtual_clock();
        }
    }

    /// Reset all global state between tests.
    ///
    /// # Safety
    ///
    /// Must not be called concurrently with other test code (Rust test
    /// harness serialises tests within the same module by default).
    unsafe fn reset_globals() {
        WASM_CLEANUP_RAN.store(false, Ordering::Release);

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
            ptr::addr_of_mut!(SHUTDOWN_PHASE).write(WasmShutdownPhase::Running);
            ptr::addr_of_mut!(COOPERATIVE_TICK_DEPTH).write(0);
            // The canonical execution context is restored by each activation.
            ptr::addr_of_mut!(PREV_ARENA).write(ptr::null_mut());
            // Reply-channel state is per-activation ctx now; no scheduler
            // static to reset. Clear any lingering canonical context so the
            // next test starts from a null current_context.
            let _ = crate::execution_context::set_current_context(ptr::null_mut());
            ptr::addr_of_mut!(TASKS_SPAWNED).write(0);
            ptr::addr_of_mut!(TASKS_COMPLETED).write(0);
            ptr::addr_of_mut!(MESSAGES_SENT).write(0);
            ptr::addr_of_mut!(MESSAGES_RECEIVED).write(0);
            // Clear timer wheel state (sleep handles, periodic queue, wheel itself).
            wasm_timers_shutdown_inner();
            ptr::addr_of_mut!(PENDING_SLEEP_DEADLINE_MS).write(0);
            // Clear the thread-local current arena so arena lifecycle tests
            // start from a clean slate regardless of test ordering.
            crate::arena::set_current_arena(ptr::null_mut());
        }
        // Restore the real monotonic clock on wasm32, where the timer-wheel
        // tests pin a virtual clock via `VirtualClock`; this keeps a clean
        // baseline even if a prior test aborted before its guard's Drop ran.
        // Inert on native: the virtual-clock seam is wasm32-only (see
        // `VirtualClock`), so native tests never touch it.
        #[cfg(target_arch = "wasm32")]
        crate::wasm_stubs::unpin_virtual_clock();
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
            assert!(
                matches!(
                    ptr::addr_of!(SHUTDOWN_PHASE).read(),
                    WasmShutdownPhase::Running
                ),
                "shutdown phase must return to Running after shutdown"
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
            // Reply-channel state is per-activation ctx now (lives on
            // `HewExecutionContext`); there is no scheduler static to assert
            // here. The activation entry/exit handshake clears it
            // automatically when the ctx frame pops.
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
            // Use addr_of! to read without creating a reference to the mutable static.
            assert_eq!(
                ptr::addr_of!(WASM_SLEEP_COUNT).read(),
                0,
                "WASM_SLEEP_COUNT must be zero after shutdown"
            );
            assert!(
                WASM_TIMER_WHEEL.is_null(),
                "WASM_TIMER_WHEEL must be null after shutdown"
            );
        }
        // SLEEP_HANDLES is Option<HashMap<...>>; .is_none() creates a shared ref,
        // so it must live in its own #[expect(static_mut_refs)] + unsafe block.
        #[expect(
            static_mut_refs,
            reason = "single-threaded test; SLEEP_HANDLES discriminant read only, no mutation"
        )]
        // SAFETY: single-threaded test; called immediately after hew_sched_shutdown.
        unsafe {
            assert!(
                SLEEP_HANDLES.is_none(),
                "SLEEP_HANDLES must be None after shutdown"
            );
        }
        // These helpers access their respective statics without creating refs here.
        assert_eq!(
            crate::timer_periodic_wasm::pending_periodic_count(),
            0,
            "WASM_PERIODIC_COUNT must be zero after shutdown"
        );
        assert!(
            crate::timer_periodic_wasm::periodic_registry_is_none(),
            "PERIODIC_CTX_REGISTRY must be None after shutdown"
        );
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

    /// WASM twin of the native `out_of_band_stop_is_observed_and_never_dispatched`.
    ///
    /// Replaces `wasm_shutdown_sentinel_is_never_delivered_to_handler`, whose
    /// subject — a queued sentinel node that must not reach the handler — no
    /// longer exists. The stop is now a flag on the mailbox, so there is no
    /// node to mis-route in the first place; what remains to prove is that the
    /// flag IS observed and that no dispatch happens because of it.
    #[test]
    fn wasm_out_of_band_stop_is_observed_and_never_dispatched() {
        static DISPATCHES: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C-unwind" fn counting_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            DISPATCHES.fetch_add(1, Ordering::Relaxed);
            std::ptr::null_mut()
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        DISPATCHES.store(0, Ordering::Relaxed);

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.dispatch = Some(counting_dispatch);
        a.mailbox = mailbox.cast();
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        let a_ptr: *mut HewActor = (&raw mut a);

        // A real user message is queued BEHIND the stop. The loop-top check
        // must win: a stopped actor does not drain its backlog first.
        let payload: i32 = 7;
        // SAFETY: mailbox is a valid live wasm mailbox; payload outlives the send.
        unsafe {
            crate::mailbox_wasm::hew_mailbox_send(
                mailbox,
                9,
                (&raw const payload).cast_mut().cast(),
                size_of::<i32>(),
            );
            crate::mailbox_wasm::mailbox_request_stop(mailbox);
        }

        // SAFETY: actor is valid and Runnable.
        unsafe { activate_actor_wasm(a_ptr) };

        assert_eq!(
            DISPATCHES.load(Ordering::Relaxed),
            0,
            "the stop must be observed at loop top, before any dispatch"
        );
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "observing the out-of-band stop must self-stop the actor"
        );

        // SAFETY: actor is terminal; the mailbox is drained and freed once.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
    }

    // WASM twin of the native
    // `user_msg_type_minus_one_reaches_handler_and_does_not_terminate`: a
    // USER-queue send of `-1` (no stop) must reach the handler and leave the
    // actor live. Fails on the pre-fix wasm path (value-only interception would
    // drop it and stop the actor) — here it also proves the provenance gate.
    #[test]
    fn wasm_user_msg_type_minus_one_reaches_handler_and_does_not_terminate() {
        static USER_HANDLED: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C-unwind" fn user_minus_one_probe_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            if msg_type == -1 {
                USER_HANDLED.fetch_add(1, Ordering::Relaxed);
            }
            std::ptr::null_mut()
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        USER_HANDLED.store(0, Ordering::Relaxed);

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.dispatch = Some(user_minus_one_probe_dispatch);
        a.mailbox = mailbox.cast();
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        let a_ptr: *mut HewActor = (&raw mut a);

        // A USER-queue send (hew_mailbox_send routes to the user queue) carrying
        // the reserved sentinel VALUE — a legitimate application message.
        // SAFETY: mailbox is valid; null payload with size 0.
        let rc =
            unsafe { crate::mailbox_wasm::hew_mailbox_send(mailbox, -1, std::ptr::null_mut(), 0) };
        assert_eq!(
            rc, 0,
            "user send of msg_type == -1 must enqueue successfully"
        );

        // SAFETY: actor is valid and Runnable.
        unsafe { activate_actor_wasm(a_ptr) };

        assert_eq!(
            USER_HANDLED.load(Ordering::Relaxed),
            1,
            "a user-queue message with msg_type == -1 must reach the handler, \
             not be intercepted as a shutdown signal"
        );
        let state = a.actor_state.load(Ordering::Relaxed);
        assert!(
            state != HewActorState::Stopped as i32 && state != HewActorState::Crashed as i32,
            "delivering a user-queue msg_type == -1 must not terminate the actor (state={state})"
        );

        // SAFETY: mailbox is drained and freed once.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
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

    /// Wasm parity with the native suspend/resume executor: the cooperative
    /// `activate_actor_wasm` resume re-entry drives a parked scratch
    /// continuation to completion across two ticks, destroying it exactly once
    /// (FG1/FG4) and settling the actor to Idle. Same ABI, single-threaded.
    #[test]
    fn wasm_activate_resumes_parked_cont_to_ready_and_destroys_once() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // Scratch frame: Ready on the 2nd resume.
        let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(2);
        let handle = frame.handle();

        // SAFETY: actor owned on this single thread; scratch handle live.
        assert!(unsafe { park_suspended_activation_wasm(actor_ptr, handle) });
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "suspend edge publishes Suspended"
        );

        // Tick 1: resume #1 -> Pending -> re-parked Suspended.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // SAFETY: actor valid.
        unsafe { activate_actor_wasm(actor_ptr) };
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "Pending resume re-parks as Suspended on wasm too"
        );
        assert_eq!(frame.resumes.load(Ordering::Relaxed), 1);

        // Tick 2: resume #2 -> Ready -> destroy once -> Idle.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // SAFETY: actor valid.
        unsafe { activate_actor_wasm(actor_ptr) };
        assert_eq!(frame.resumes.load(Ordering::Relaxed), 2);
        assert_eq!(
            frame.destroyed.load(Ordering::Relaxed),
            1,
            "FG1: the Ready continuation is destroyed exactly once on wasm"
        );
        assert!(
            actor.suspended_cont.load(Ordering::Relaxed).is_null(),
            "FG4: slot nulled in the Destroyed critical section"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "a completed resume with an empty mailbox settles to Idle"
        );

        hew_sched_shutdown();
    }

    /// A wasm dispatch handler that suspends — returns a non-null `coro.begin`-
    /// shaped handle (the D-A.2 suspend outcome the trampoline surfaces on a
    /// Pending poll). The scratch frame completes on its 1st resume.
    unsafe extern "C-unwind" fn suspend_once_dispatch_wasm(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        crate::coro_exec::test_support::ScratchFrame::into_executor_owned_handle(1)
    }

    /// Test-only borrowed-frame variant for witnesses that need to inspect the
    /// frame after the runtime has invoked its destroy outline. Unlike the
    /// production-shaped helper above, its destroy outline releases the inner
    /// guard only; the test remains responsible for the outer `Box`.
    #[cfg(target_arch = "wasm32")]
    unsafe extern "C-unwind" fn suspend_once_dispatch_test_owned_wasm(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        crate::coro_exec::test_support::ScratchFrameOwner::new(1).into_handle()
    }

    /// Actual-target WASM continuation probe whose destroy outline frees its
    /// own frame allocation and cancels a real actor timer. Global counts are
    /// safe because wasm execution and the runtime test guard are both
    /// single-threaded.
    #[cfg(target_arch = "wasm32")]
    #[repr(C)]
    struct ShutdownBalanceFrame {
        resume: Option<unsafe extern "C" fn(*mut c_void)>,
        destroy: Option<unsafe extern "C" fn(*mut c_void)>,
        actor: *mut crate::actor::HewActor,
        _frame_owned_heap: Box<ShutdownOwnedProbe>,
    }

    #[cfg(target_arch = "wasm32")]
    struct ShutdownOwnedProbe {
        _allocation_tooth: u64,
    }

    #[cfg(target_arch = "wasm32")]
    impl Drop for ShutdownOwnedProbe {
        fn drop(&mut self) {
            SHUTDOWN_FRAME_OWNED_DROPS.fetch_add(1, Ordering::AcqRel);
        }
    }

    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_FRAME_ALLOCS: AtomicU64 = AtomicU64::new(0);
    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_FRAME_FREES: AtomicU64 = AtomicU64::new(0);
    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_FRAME_OWNED_DROPS: AtomicU64 = AtomicU64::new(0);
    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_FRAME_TIMER_CANCELS: AtomicU64 = AtomicU64::new(0);
    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_COOPERATE_CALLS: AtomicU64 = AtomicU64::new(0);
    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_PERIODIC_DISPATCHES: AtomicU64 = AtomicU64::new(0);
    #[cfg(target_arch = "wasm32")]
    static SHUTDOWN_SLEEP_TERMINATES: AtomicU64 = AtomicU64::new(0);

    #[cfg(target_arch = "wasm32")]
    unsafe extern "C" fn shutdown_balance_resume(_frame: *mut c_void) {
        panic!("shutdown balance frame must be destroyed without resuming");
    }

    #[cfg(target_arch = "wasm32")]
    unsafe extern "C" fn shutdown_balance_destroy(frame: *mut c_void) {
        // SAFETY: the executor's `Parked -> Destroyed` guard made this call the
        // frame's sole owner.
        let frame = unsafe { Box::from_raw(frame.cast::<ShutdownBalanceFrame>()) };
        // The timer wheel/registry must still exist while a parked cleanup
        // outline runs. Cancel the actor's real periodic registration through
        // the ordinary path; a premature scheduler timer teardown makes these
        // assertions fail before the frame is counted free.
        // SAFETY: shutdown is synchronous and single-threaded; this read only
        // verifies that timer teardown has not yet consumed the global wheel.
        let timer_wheel_is_live = !unsafe { wasm_timer_wheel_raw() }.is_null();
        assert!(
            timer_wheel_is_live,
            "parked-frame destroy ran after the WASM timer wheel disappeared"
        );
        assert!(
            crate::timer_periodic_wasm::pending_periodic_count() > 0,
            "parked-frame destroy ran after periodic registrations disappeared"
        );
        // SAFETY: frame.actor is the live, still-tracked actor that owns this
        // continuation; shutdown has exclusive single-threaded access.
        unsafe { crate::timer_periodic_wasm::cancel_all_timers_for_actor(frame.actor) };
        SHUTDOWN_FRAME_TIMER_CANCELS.fetch_add(1, Ordering::AcqRel);
        drop(frame);
        SHUTDOWN_FRAME_FREES.fetch_add(1, Ordering::AcqRel);
    }

    #[cfg(target_arch = "wasm32")]
    unsafe extern "C-unwind" fn shutdown_balance_dispatch(
        ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: scheduler installed a live per-activation context.
        let actor = unsafe { (*ctx).actor };
        // A far-future real registration proves shutdown does not need to wait
        // for the timer and keeps timer/cancel machinery observable at destroy.
        // SAFETY: actor is live for this dispatch and remains tracked while
        // its parked frame exists.
        let timer = unsafe {
            crate::timer_periodic_wasm::hew_actor_schedule_periodic(actor, 99, u64::MAX / 4)
        };
        assert!(!timer.is_null(), "timer registration for shutdown probe");
        SHUTDOWN_FRAME_ALLOCS.fetch_add(1, Ordering::AcqRel);
        Box::into_raw(Box::new(ShutdownBalanceFrame {
            resume: Some(shutdown_balance_resume),
            destroy: Some(shutdown_balance_destroy),
            actor,
            _frame_owned_heap: Box::new(ShutdownOwnedProbe {
                _allocation_tooth: 0x5a5a_a5a5_dead_beef,
            }),
        }))
        .cast()
    }

    #[cfg(target_arch = "wasm32")]
    unsafe extern "C-unwind" fn shutdown_cooperate_dispatch(
        ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: the scheduler installed a live context for this dispatch.
        let actor = unsafe { (*ctx).actor };
        // Exhaust the budget so this is a real cooperate-driven nested tick,
        // not merely a call that returns without touching the scheduler.
        unsafe { (*actor).reductions.store(1, Ordering::Relaxed) };
        assert_eq!(hew_actor_cooperate(), 1);
        SHUTDOWN_COOPERATE_CALLS.fetch_add(1, Ordering::AcqRel);
        ptr::null_mut()
    }

    #[cfg(target_arch = "wasm32")]
    unsafe extern "C-unwind" fn shutdown_periodic_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        SHUTDOWN_PERIODIC_DISPATCHES.fetch_add(1, Ordering::AcqRel);
        ptr::null_mut()
    }

    #[cfg(target_arch = "wasm32")]
    unsafe extern "C" fn shutdown_sleep_terminate(_state: *mut c_void) {
        SHUTDOWN_SLEEP_TERMINATES.fetch_add(1, Ordering::AcqRel);
    }

    /// PRODUCTION SUSPEND EDGE (wasm parity): a handler that returns a non-null
    /// handle from the dispatch trampoline drives the cooperative message loop to
    /// PARK the activation — CAS to `Suspended`, store the handle. Mirrors the
    /// native `dispatch_returning_handle_parks_the_activation`; the wasm
    /// deliverable is parity (E9 — actors are wasm-rejected from source, so the
    /// synthetic dispatch fn is the producer).
    #[test]
    fn wasm_dispatch_returning_handle_parks_the_activation() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        // SAFETY: test exclusively owns this mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor.dispatch = Some(suspend_once_dispatch_wasm);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        // Enqueue one message and drive one cooperative tick.
        // SAFETY: actor is valid, scheduler initialized, mailbox live.
        unsafe { sched_enqueue(actor_ptr) };
        // SAFETY: actor has a valid mailbox.
        unsafe { queue_wasm_message(actor_ptr, 0) };
        hew_sched_run();

        // The handler suspended: the cooperative loop parked the returned handle.
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "a handler returning a non-null handle parks the activation on wasm"
        );
        assert!(
            !actor.suspended_cont.load(Ordering::Relaxed).is_null(),
            "the returned handle is parked in the resume slot on wasm"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Relaxed),
            crate::internal::types::ContTag::Parked as i32,
            "the parked cont tag is Parked on wasm"
        );

        // Teardown: destroy the parked scratch frame exactly once + free mailbox.
        // SAFETY: the parked handle is live and not yet destroyed.
        assert!(unsafe { crate::coro_exec::destroy_parked(as_native_actor(actor_ptr)) }.is_ok());
        // SAFETY: mailbox was allocated for this test.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        hew_sched_shutdown();
    }

    /// P1-wasm parity (W6.010): a suspending handler still owes a reply to its
    /// caller, so the wasm suspend edge must STASH the message's reply channel
    /// into `suspended_reply_channel` and SKIP the normal teardown/free — exactly
    /// like the native path — so the resume edge can re-establish a context
    /// carrying it. Before this fix the wasm dispatch loop nulled + freed the
    /// reply channel before parking, leaving the resumed body with no channel
    /// (the caller would hang). Drives one cooperative tick with an ask
    /// (reply-channel-bearing) message + a suspending dispatch and asserts the
    /// channel was stashed, not freed.
    #[test]
    fn wasm_suspend_edge_stashes_reply_channel_for_resume() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut actor = stub_actor();
        actor.dispatch = Some(suspend_once_dispatch_wasm);
        // SAFETY: this test creates and exclusively owns the mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = (&raw mut actor).cast();

        let ch = crate::reply_channel_wasm::hew_reply_channel_new();
        let value: i32 = 7;
        // SAFETY: actor, channel, and payload are valid for the test duration.
        let rc = unsafe {
            crate::actor::ask_with_channel_wasm_internal(
                actor_ptr.cast(),
                1,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                ch.cast(),
            )
        };
        assert_eq!(rc, HewError::Ok as i32);
        // The queued send retained the caller's channel (refs == 2).
        assert_eq!(
            // SAFETY: `ch` is live for the test.
            unsafe { crate::reply_channel_wasm::test_ref_count(ch) },
            2,
            "the ask must retain the reply channel"
        );

        // Drive the cooperative loop: dispatch suspends, the loop parks.
        hew_sched_run();

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "the suspending handler parks the activation"
        );
        // The reply channel was STASHED on the actor (not nulled/freed): the
        // resume edge consumes it. This is the parity fix.
        let a = as_native_actor(actor_ptr);
        assert_eq!(
            a.suspended_reply_channel.load(Ordering::Relaxed),
            ch.cast::<c_void>(),
            "the suspend edge must stash the reply channel for the resume edge"
        );
        // The sender-side retain is still outstanding (NOT freed on the suspend
        // edge) — the channel remains live for the eventual reply.
        assert_eq!(
            // SAFETY: `ch` is live for the test.
            unsafe { crate::reply_channel_wasm::test_ref_count(ch) },
            2,
            "the suspend edge must NOT free the reply channel ref"
        );

        // Teardown: destroy the parked frame, then release both refs + mailbox.
        // SAFETY: the parked handle is live and not yet destroyed.
        assert!(unsafe { crate::coro_exec::destroy_parked(a) }.is_ok());
        a.suspended_reply_channel
            .store(ptr::null_mut(), Ordering::Relaxed);
        // SAFETY: release the stashed sender ref and the test's waiter ref.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(ch.cast());
            crate::reply_channel_wasm::hew_reply_channel_free(ch.cast());
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        hew_sched_shutdown();
    }

    /// A wasm dispatch handler that REPLIES and THEN suspends. The reply
    /// consumes the dispatch's sender-side reference (`hew_reply` ends in
    /// `hew_reply_channel_free`), so the handler owes nothing by the time it
    /// parks. Exists to exercise the suspend edge's `reply_consumed` guard.
    unsafe extern "C-unwind" fn reply_then_suspend_dispatch_wasm(
        ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: the dispatch trampoline installs a live context for the arm.
        let ch = unsafe { (*ctx).reply_channel };
        // SAFETY: `ch` is the dispatch's sender-side reference, unconsumed.
        unsafe {
            let _ = crate::reply_channel_wasm::hew_reply(ch.cast(), ptr::null_mut(), 0);
        }
        crate::coro_exec::test_support::ScratchFrame::into_executor_owned_handle(1)
    }

    /// Park an actor inside an `ask` whose handler suspended, and hand back the
    /// caller's reply channel. Shared setup for the wasm abandonment
    /// regressions below.
    ///
    /// Takes a raw pointer rather than `&mut` so the same setup serves both the
    /// stack actors the stop/resume/suspend tests own and the heap-allocated,
    /// live-tracked actor the free test hands to `actor_free_wasm_impl`.
    ///
    /// # Safety
    ///
    /// `actor` must outlive the returned channel's use and be owned by the test.
    unsafe fn park_wasm_ask(
        actor_ptr: *mut HewActor,
        dispatch: HewDispatchFn,
    ) -> (
        *mut HewActor,
        *mut crate::reply_channel_wasm::WasmReplyChannel,
    ) {
        // SAFETY: the caller owns `actor_ptr` and nothing else references it yet.
        let actor = unsafe { &mut *actor_ptr };
        actor.dispatch = Some(dispatch);
        // SAFETY: this test creates and exclusively owns the mailbox.
        actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        let ch = crate::reply_channel_wasm::hew_reply_channel_new();
        let value: i32 = 7;
        // SAFETY: actor, channel, and payload are valid for the test duration.
        let rc = unsafe {
            crate::actor::ask_with_channel_wasm_internal(
                actor_ptr.cast(),
                1,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
                ch.cast(),
            )
        };
        assert_eq!(rc, HewError::Ok as i32);
        hew_sched_run();
        (actor_ptr, ch)
    }

    /// FINDING 3 (wasm stop-while-parked). A handler that suspends mid-`ask`
    /// owes its caller a reply. The wasm stop path used to return early for any
    /// non-`Running` actor, so a `Suspended` actor was never woken, never
    /// cancelled, and never retired the reply: the asking side polled
    /// `reply_ready` until the run queue drained and then gave up with no
    /// status, and the channel reference in `suspended_reply_channel` leaked.
    ///
    /// Asserts BOTH halves: the ask RESOLVES (replied + orphaned, so the caller
    /// unblocks with a classifiable failure rather than by queue exhaustion),
    /// AND the refcount returns to baseline (exactly one release: the test's own
    /// waiter reference is all that is left).
    #[test]
    fn wasm_stopping_a_parked_ask_handler_resolves_the_asking_side() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        let baseline = crate::reply_channel_wasm::active_channel_count();

        let mut actor = stub_actor();
        // SAFETY: the actor lives for the whole test.
        let (actor_ptr, ch) =
            unsafe { park_wasm_ask(std::ptr::from_mut(&mut actor), suspend_once_dispatch_wasm) };
        let a = as_native_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "the suspending handler parks the activation"
        );
        assert_eq!(
            a.suspended_reply_channel.load(Ordering::Relaxed),
            ch.cast::<c_void>(),
            "the suspend edge stashes the unanswered reply channel"
        );

        // Stop the parked actor.
        // SAFETY: `actor_ptr` is the live actor this test owns.
        unsafe { crate::actor::actor_stop_wasm_impl(actor_ptr.cast()) };
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "stopping a parked actor must WAKE it: latching alone strands the stop"
        );
        hew_sched_run();

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "the woken activation observes the latch and cancels the park"
        );
        assert!(
            a.suspended_cont.load(Ordering::Relaxed).is_null(),
            "the cancelled park destroys the continuation"
        );
        assert!(
            a.suspended_reply_channel.load(Ordering::Relaxed).is_null(),
            "the slot invariant: the retired slot no longer owns a reference"
        );

        // The asking side is UNBLOCKED: the channel resolved with a status.
        // SAFETY: the test still holds its own reference to `ch`.
        unsafe {
            assert!(
                crate::reply_channel_wasm::test_replied(ch),
                "the abandoned ask must RESOLVE, not spin until the queue drains"
            );
            assert!(
                crate::reply_channel_wasm::reply_is_orphaned(ch),
                "it resolves as orphaned, distinguishable from a null reply"
            );
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "EXACTLY ONE release: only the caller's own reference is left"
            );
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            baseline + 1,
            "the channel is still live while the caller holds it"
        );

        // Teardown: the caller drops its reference; the channel returns to
        // baseline. A second release here would underflow, a zero would leak.
        // SAFETY: releasing the test's own reference exactly once.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(ch.cast());
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            baseline,
            "the reply channel refcount returns to baseline"
        );
        hew_sched_shutdown();
    }

    /// The wasm FREE-while-parked path — the fourth abandonment route, and the
    /// one this round's enumeration exists to make visible. `actor_free_wasm_impl`
    /// used to have no C1 teardown at all: `Suspended` is not quiescent, so the
    /// free spun to its two-second deadline and returned `-2` with the frame,
    /// the actor box, and the asking side's only reply reference all still live.
    /// Unlike the stop path no wake can rescue it — the caller asked for the box
    /// back — so the teardown has to destroy the frame itself and settle the
    /// debt on the spot.
    ///
    /// HARNESS LIMIT, stated rather than papered over: this drives
    /// `cancel_parked_activation_for_free_wasm`, the branch the fix adds, NOT
    /// the whole of `actor_free_wasm_impl`. That function's tail calls
    /// `finalize_quiescent_actor_cleanup`, whose `free_actor_resources`
    /// resolves to the NATIVE body under `cfg(test)` and frees a wasm mailbox
    /// with the native destructor. End-to-end coverage of the wasm free needs a
    /// real wasm32 runner, not another native test.
    ///
    /// Asserts BOTH halves, like its native twin: the actor settles terminal
    /// with the frame destroyed, AND the ask resolves orphaned with exactly one
    /// release — zero would leave the asking side waiting, two would underflow.
    #[test]
    fn wasm_freeing_a_parked_ask_handler_resolves_the_asking_side() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        let baseline = crate::reply_channel_wasm::active_channel_count();

        let mut actor = stub_actor();
        // SAFETY: the actor lives for the whole test.
        let (actor_ptr, ch) =
            unsafe { park_wasm_ask(std::ptr::from_mut(&mut actor), suspend_once_dispatch_wasm) };
        let a = as_native_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "the suspending handler parks the activation"
        );
        assert_eq!(
            a.suspended_reply_channel.load(Ordering::Relaxed),
            ch.cast::<c_void>(),
            "the suspend edge stashes the unanswered reply channel"
        );

        // The path under test: teardown abandons the activation outright.
        // SAFETY: nothing is dispatching this actor.
        unsafe { crate::actor::cancel_parked_activation_for_free_wasm(a) };

        assert!(
            a.suspended_cont.load(Ordering::Relaxed).is_null(),
            "the free destroys the parked continuation instead of spinning to -2"
        );
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "the actor reaches a quiescent state, so the free can proceed"
        );
        assert!(
            a.suspended_reply_channel.load(Ordering::Relaxed).is_null(),
            "the slot invariant: the retired slot no longer owns a reference"
        );

        // SAFETY: the test still holds its own reference to `ch`.
        unsafe {
            assert!(
                crate::reply_channel_wasm::test_replied(ch),
                "the abandoned ask must RESOLVE, not wait on a destroyed handler"
            );
            assert!(
                crate::reply_channel_wasm::reply_is_orphaned(ch),
                "it resolves as orphaned, distinguishable from a null reply"
            );
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "EXACTLY ONE release: only the caller's own reference is left"
            );
        }

        // A second sweep — `free_actor_resources_wasm` runs one on
        // every free route — must be a no-op, not a second release.
        crate::scheduler_wasm::retire_suspended_reply_channel_wasm(a);
        // SAFETY: the test still holds its own reference to `ch`.
        unsafe {
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "the swap makes overlapping abandonment routes resolve exactly once"
            );
        }

        // SAFETY: releasing the test's own reference exactly once.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(ch.cast());
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            baseline,
            "the reply channel refcount returns to baseline"
        );
        hew_sched_shutdown();
    }

    /// A stop path that loses the continuation destroy guard must not pretend
    /// the activation was abandoned. The reply, cancel-token and generator
    /// sink slots remain owned, and the lifecycle latch stays non-terminal,
    /// until a later call proves frame ownership by winning `Parked ->
    /// Destroyed`.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn wasm_stop_destroy_refusal_preserves_frame_latch_and_activation_debts() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();
        let replies_before = crate::reply_channel_wasm::active_channel_count();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        let a = as_native_actor(actor_ptr);
        let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        let handle = frame.handle();
        assert!(crate::coro_exec::begin_park(a).is_ok());
        // SAFETY: the scratch frame remains live for the complete test.
        unsafe { crate::coro_exec::finish_park(a, handle) };
        assert!(crate::coro_exec::begin_resume(a).is_ok());
        actor
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);

        let reply = crate::reply_channel_wasm::hew_reply_channel_new();
        a.suspended_reply_channel
            .store(reply.cast(), Ordering::Release);
        // SAFETY: null creates an owned root cancellation token.
        let token = unsafe { crate::task_scope::hew_cancel_token_new_child(ptr::null_mut()) };
        a.suspended_cancel_token
            .store(token.cast(), Ordering::Release);
        // Register a real native generator sink so refusal must preserve this
        // debt too. The unused stream half is closed before the pair wrapper is
        // freed; the sink remains owned by the actor slot.
        // SAFETY: stream-pair accessors return the pair's live halves.
        let sink = unsafe {
            let pair = crate::stream::hew_stream_channel(1);
            let sink = crate::stream::hew_stream_pair_sink(pair);
            crate::stream::hew_stream_close(crate::stream::hew_stream_pair_stream(pair));
            crate::stream::hew_stream_pair_free(pair);
            sink
        };
        // SAFETY: actor and sink are live and exclusively owned here.
        unsafe { crate::actor::hew_actor_gen_sink_register(actor_ptr.cast(), sink) };

        // Resuming refuses destroy: no debt or latch may be changed.
        // SAFETY: this test owns the activation state.
        assert!(!unsafe { cancel_parked_activation_for_stop_wasm(actor_ptr) });
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Running as i32
        );
        assert_eq!(a.suspended_cont.load(Ordering::Acquire), handle);
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);
        assert_eq!(
            a.suspended_reply_channel.load(Ordering::Acquire),
            reply.cast()
        );
        assert_eq!(
            a.suspended_cancel_token.load(Ordering::Acquire),
            token.cast()
        );
        assert_eq!(a.gen_sink.load(Ordering::Acquire), sink.cast());
        // SAFETY: refusal preserved the channel allocation and its sole ref.
        unsafe {
            assert!(!crate::reply_channel_wasm::test_replied(reply));
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);
        }

        // Return ownership to Parked and retry. This time the frame and every
        // activation-owned debt are retired exactly once.
        assert!(crate::coro_exec::settle_pending(a).is_ok());
        // SAFETY: the actor's continuation is parked and exclusively owned.
        assert!(unsafe { cancel_parked_activation_for_stop_wasm(actor_ptr) });
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32
        );
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
        assert!(a.suspended_cont.load(Ordering::Acquire).is_null());
        assert!(a.suspended_reply_channel.load(Ordering::Acquire).is_null());
        assert!(a.suspended_cancel_token.load(Ordering::Acquire).is_null());
        assert!(a.gen_sink.load(Ordering::Acquire).is_null());
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            replies_before
        );

        hew_sched_shutdown();
    }

    /// Every stop-cancel consultation must treat `false` as a live ownership
    /// refusal, not as permission to continue settling the activation.
    ///
    /// Native tests force the executor's `Resuming` refusal; actual wasm32
    /// tests use the target's impossible non-null generator-sink sentinel.
    /// Both counterfactuals leave the same frame and reply debt untouched.
    #[test]
    fn wasm_stop_cancel_refusal_is_handled_fail_closed_at_all_three_call_sites() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();
        let replies_before = crate::reply_channel_wasm::active_channel_count();

        let cases = [
            (
                StopCancelSite::BeforeRepark,
                HewActorState::Running,
                "before re-parking a pending resume",
            ),
            (
                StopCancelSite::AfterSuspendedPublish,
                HewActorState::Suspended,
                "after publishing Suspended",
            ),
            (
                StopCancelSite::BeforeResume,
                HewActorState::Running,
                "before resuming a parked activation",
            ),
        ];

        for (site, initial_state, site_name) in cases {
            let mut actor = stub_actor();
            let actor_ptr = std::ptr::from_mut(&mut actor);
            let a = as_native_actor(actor_ptr);
            // SAFETY: this test creates and exclusively owns the mailbox.
            actor.mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast();
            let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
            let handle = frame.handle();
            assert!(crate::coro_exec::begin_park(a).is_ok());
            // SAFETY: frame stays live until the successful cleanup below.
            unsafe { crate::coro_exec::finish_park(a, handle) };
            #[cfg(not(target_arch = "wasm32"))]
            assert!(
                crate::coro_exec::begin_resume(a).is_ok(),
                "native counterfactual must force destroy refusal"
            );
            #[cfg(target_arch = "wasm32")]
            a.gen_sink.store(
                std::ptr::NonNull::<u8>::dangling()
                    .as_ptr()
                    .cast::<c_void>(),
                Ordering::Release,
            );

            let reply = crate::reply_channel_wasm::hew_reply_channel_new();
            a.suspended_reply_channel
                .store(reply.cast(), Ordering::Release);
            actor
                .actor_state
                .store(initial_state as i32, Ordering::Release);
            // SAFETY: mailbox is live and exclusively owned.
            unsafe { crate::mailbox_wasm::mailbox_request_stop(actor.mailbox.cast()) };
            crate::hew_clear_error();

            // SAFETY: this is the exact wrapper called at `site`; the test owns
            // the actor activation and forces its cancellation primitive false.
            assert!(!unsafe { cancel_parked_activation_for_stop_at(actor_ptr, site) });

            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                HewActorState::Suspended as i32,
                "{site_name}: refusal must leave the live frame Suspended"
            );
            assert_eq!(a.suspended_cont.load(Ordering::Acquire), handle);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);
            assert_eq!(
                a.suspended_reply_channel.load(Ordering::Acquire),
                reply.cast(),
                "{site_name}: refusal must preserve reply debt"
            );
            // SAFETY: mailbox remains live and refusal must retain its latch.
            assert!(unsafe { crate::mailbox_wasm::mailbox_stop_requested(actor.mailbox.cast()) });
            assert_last_error_eq(&format!(
                "WASM actor stop cancellation refused: actor {:#x} retained its \
                 parked activation {site_name}; actor left Suspended fail-closed",
                actor.id
            ));

            // Repair only the injected refusal and prove the preserved frame
            // and debt remain reclaimable exactly once.
            #[cfg(not(target_arch = "wasm32"))]
            assert!(crate::coro_exec::settle_pending(a).is_ok());
            #[cfg(target_arch = "wasm32")]
            a.gen_sink.store(ptr::null_mut(), Ordering::Release);
            // SAFETY: actor is Parked, live, and exclusively owned.
            assert!(unsafe { cancel_parked_activation_for_stop_wasm(actor_ptr) });
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert!(a.suspended_reply_channel.load(Ordering::Acquire).is_null());
            // SAFETY: successful cancellation no longer uses the mailbox.
            unsafe { crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast()) };
        }

        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            replies_before,
            "all three preserved reply debts must be retired exactly once"
        );
        hew_sched_shutdown();
    }

    /// WASM has no legal producer for `HewActor::gen_sink`. If invariant
    /// corruption nevertheless makes the slot non-null, all three lifecycle
    /// cleanup sites must refuse before touching the frame or actor box.
    ///
    /// The dangling value is an inert sentinel, never a fabricated producer:
    /// the guard must only observe that it is non-null and must never
    /// dereference or release it.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_native_only_gen_sink_refuses_all_lifecycle_cleanup_sites() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        let impossible_slot = std::ptr::NonNull::<u8>::dangling()
            .as_ptr()
            .cast::<c_void>();
        let expected = |actor_id| {
            format!(
                "WASM actor lifecycle cleanup refused: actor {actor_id:#x} carried a \
                 native-only registered generator sink; actor preserved fail-closed"
            )
        };

        // Stop/free cancellation sites: both must leave the parked frame and
        // lifecycle latch untouched.
        let actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();
        let a = as_native_actor(actor_ptr);
        let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        let handle = frame.handle();
        assert!(crate::coro_exec::begin_park(a).is_ok());
        // SAFETY: the scratch frame remains live for the complete test.
        unsafe { crate::coro_exec::finish_park(a, handle) };
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        a.gen_sink.store(impossible_slot, Ordering::Release);

        crate::hew_clear_error();
        // SAFETY: the test exclusively owns this parked activation.
        assert!(!unsafe { cancel_parked_activation_for_stop_wasm(actor_ptr) });
        assert_last_error_eq(&expected(actor.id));
        assert_eq!(a.suspended_cont.load(Ordering::Acquire), handle);
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32
        );

        crate::hew_clear_error();
        // SAFETY: the same parked activation remains exclusively owned.
        unsafe { crate::actor::cancel_parked_activation_for_free_wasm(a) };
        assert_last_error_eq(&expected(actor.id));
        assert_eq!(a.suspended_cont.load(Ordering::Acquire), handle);
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);
        assert_eq!(
            actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32
        );

        // Clear only the synthetic corruption, then reclaim the frame through
        // the ordinary proven-owner free cancellation path.
        a.gen_sink.store(ptr::null_mut(), Ordering::Release);
        // SAFETY: this test still exclusively owns the parked activation.
        unsafe { crate::actor::cancel_parked_activation_for_free_wasm(a) };
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);

        // Resource-free and complete-free sites: use a real tracked box so
        // both choke points prove they refuse before untracking, and so the
        // complete path's exact C result is observable.
        // SAFETY: zero-state spawn is supported and returns a runtime-owned box.
        let boxed_actor = unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, None) };
        assert!(!boxed_actor.is_null());
        // SAFETY: the actor is live and this single WASM thread owns it.
        unsafe {
            (*boxed_actor)
                .gen_sink
                .store(impossible_slot, Ordering::Release);
        }
        let boxed_id = unsafe { (*boxed_actor).id };
        let box_counts_before = crate::actor_balance::actor_box_counts();
        crate::hew_clear_error();

        // SAFETY: the actor is quiescent and exclusively owned.
        unsafe { crate::actor::free_actor_resources_wasm(boxed_actor) };

        assert_last_error_eq(&expected(boxed_id));
        assert!(
            crate::actor::is_actor_live(boxed_actor),
            "resource cleanup refusal must preserve live tracking"
        );
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            box_counts_before,
            "resource cleanup refusal must preserve the actor box"
        );

        crate::hew_clear_error();
        // SAFETY: resource refusal preserved the complete tracked actor.
        let free_result = unsafe { crate::actor::actor_free_wasm_impl(boxed_actor) };
        assert_eq!(
            free_result, -2,
            "generator-sink corruption must be reported as a failed free"
        );
        assert_last_error_eq(&expected(boxed_id));
        assert!(
            crate::actor::is_actor_live(boxed_actor),
            "free refusal must preserve live tracking"
        );
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            box_counts_before,
            "free refusal must preserve the actor box"
        );

        // Remove only the synthetic corruption, then use the complete free
        // path to retire and reclaim the actor normally.
        // SAFETY: the refusal above preserved the complete actor.
        unsafe {
            (*boxed_actor)
                .gen_sink
                .store(ptr::null_mut(), Ordering::Release);
            assert_eq!(crate::actor::actor_free_wasm_impl(boxed_actor), 0);
        }
        hew_sched_shutdown();
    }

    /// Evidence for the post-retirement queue invariant: orphaning a callee's
    /// suspended reply synchronously calls `hew_reply`, whose parked-waiter
    /// branch calls `enqueue_resume` for the caller. In `Retiring`, shutdown
    /// already owns that caller frame, so the wake must be suppressed rather
    /// than re-enqueued for a second execution drain.
    #[test]
    fn wasm_retirement_orphan_reply_cannot_reenqueue_parked_waiter() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();
        let replies_before = crate::reply_channel_wasm::active_channel_count();

        let waiter_actor = stub_actor();
        let retiring_actor = stub_actor();
        let waiter_ptr: *mut HewActor = (&raw const waiter_actor).cast_mut();
        let waiter_native = as_native_actor(waiter_ptr);
        let retiring_native = as_native_actor((&raw const retiring_actor).cast_mut());
        let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        let handle = frame.handle();
        assert!(crate::coro_exec::begin_park(waiter_native).is_ok());
        // SAFETY: scratch frame is live and exclusively owned for the test.
        unsafe { crate::coro_exec::finish_park(waiter_native, handle) };
        waiter_actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);

        let reply = crate::reply_channel_wasm::hew_reply_channel_new();
        // Retain the sender-side reference that the callee slot owns; the
        // original reference remains the parked caller's.
        // SAFETY: reply and caller are live.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_retain(reply);
            crate::reply_channel_wasm::hew_reply_channel_set_parked_waiter(
                reply,
                waiter_ptr.cast(),
            );
        }
        retiring_native
            .suspended_reply_channel
            .store(reply.cast(), Ordering::Release);

        // SAFETY: direct single-threaded phase seam for the retirement edge.
        unsafe { ptr::addr_of_mut!(SHUTDOWN_PHASE).write(WasmShutdownPhase::Retiring) };
        retire_suspended_reply_channel_wasm(retiring_native);

        // The orphan reply resolves and consumes the callee's reference, but
        // cannot mutate or publish the caller frame owned by retirement.
        // SAFETY: the test still owns the caller-side reply reference.
        unsafe {
            assert!(crate::reply_channel_wasm::test_replied(reply));
            assert!(crate::reply_channel_wasm::reply_is_orphaned(reply));
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);
        }
        assert_eq!(
            waiter_actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32
        );
        assert_eq!(waiter_native.suspended_cont.load(Ordering::Acquire), handle);
        assert!(!waiter_native.pending_wake.load(Ordering::Acquire));
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);

        // Restore normal phase and reclaim the test-owned objects.
        // SAFETY: all pointers remain exclusively owned by this test.
        unsafe {
            ptr::addr_of_mut!(SHUTDOWN_PHASE).write(WasmShutdownPhase::Running);
            crate::reply_channel_wasm::hew_reply_channel_free(reply);
            crate::actor::cancel_parked_activation_for_free_wasm(waiter_native);
        }
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            replies_before
        );
        hew_sched_shutdown();
    }

    /// Actual-target witness for the second shutdown edge: after
    /// `hew_sched_shutdown` has taken `RUN_QUEUE`, the later cleanup sweep can
    /// still abandon a suspended ask handler. Retiring that handler's sender
    /// reference publishes an orphaned reply synchronously, and a parked
    /// caller makes the reply path attempt `enqueue_resume`.
    ///
    /// The scheduler must resolve/release the reply and destroy the abandoned
    /// callee frame exactly once without changing or publishing the caller
    /// frame. Omitting the fallible publish in `enqueue_resume` makes this test
    /// panic at `sched_enqueue: scheduler not initialized`.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_post_shutdown_orphan_reply_cannot_publish_resume() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();
        let replies_before = crate::reply_channel_wasm::active_channel_count();

        // Build a real parked waiter frame. The reply channel below carries
        // this actor pointer exactly as codegen's parked-ask setter does.
        let waiter_actor = stub_actor();
        let waiter_ptr: *mut HewActor = (&raw const waiter_actor).cast_mut();
        let waiter_native = as_native_actor(waiter_ptr);
        let waiter_frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        let waiter_handle = waiter_frame.handle();
        assert!(crate::coro_exec::begin_park(waiter_native).is_ok());
        // SAFETY: waiter_frame is live and exclusively owned for the test.
        unsafe { crate::coro_exec::finish_park(waiter_native, waiter_handle) };
        waiter_actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);

        // Drive the production ask/message-node/suspend path for the callee.
        // The drained mailbox plus the slot/ref assertions below prove that
        // the message node was consumed and moved its sender reference into
        // the parked activation rather than leaking either authority.
        let mut callee_actor = stub_actor();
        // SAFETY: both stack actors and the channel they exchange outlive the
        // complete test.
        let (callee_ptr, reply) = unsafe {
            park_wasm_ask(
                std::ptr::from_mut(&mut callee_actor),
                suspend_once_dispatch_test_owned_wasm,
            )
        };
        let callee_native = as_native_actor(callee_ptr);
        let callee_handle = callee_native.suspended_cont.load(Ordering::Acquire);
        assert!(!callee_handle.is_null());
        let callee_frame = callee_handle.cast::<crate::coro_exec::test_support::ScratchFrame>();
        assert_eq!(
            // SAFETY: park_wasm_ask created this live mailbox.
            unsafe { crate::mailbox_wasm::hew_mailbox_len(callee_actor.mailbox.cast()) },
            0,
            "dispatch must consume the ask message node before parking"
        );
        assert_eq!(
            callee_native
                .suspended_reply_channel
                .load(Ordering::Acquire),
            reply.cast()
        );
        // SAFETY: reply is live; the caller and callee slot own one ref each.
        unsafe {
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 2);
            crate::reply_channel_wasm::hew_reply_channel_set_parked_waiter(
                reply,
                waiter_ptr.cast(),
            );
        }

        // This is the exact lifecycle seam from the blocker: shutdown has
        // returned and taken the queue, while actor cleanup still owns the
        // parked callee and its reply debt.
        hew_sched_shutdown();
        // SAFETY: waiter_ptr remains live. Refusal proves the queue is absent;
        // unlike the public length metric, this cannot conflate None and empty.
        assert!(unsafe { try_sched_enqueue(waiter_ptr).is_err() });
        // SAFETY: the stack callee remains the test's exclusive parked actor.
        unsafe { crate::actor::cancel_parked_activation_for_free_wasm(callee_native) };

        // Callee abandonment owns and settles its frame + sender ref exactly
        // once. The caller-side ref observes the terminal orphan result.
        assert_eq!(
            // SAFETY: callee_frame remains allocated until reclaimed below.
            unsafe { (*callee_frame).destroyed.load(Ordering::Acquire) },
            1
        );
        assert!(callee_native
            .suspended_cont
            .load(Ordering::Acquire)
            .is_null());
        assert!(callee_native
            .suspended_reply_channel
            .load(Ordering::Acquire)
            .is_null());
        assert_eq!(
            callee_actor.actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32
        );
        // SAFETY: the test still owns the caller-side reply reference.
        unsafe {
            assert!(crate::reply_channel_wasm::test_replied(reply));
            assert!(crate::reply_channel_wasm::reply_is_orphaned(reply));
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);
        }

        // Failed publication cannot claim the waiter activation. It remains a
        // complete parked frame for its own cleanup authority, with no pending
        // wake or ghost queue/metric entry.
        assert_eq!(
            waiter_actor.actor_state.load(Ordering::Acquire),
            HewActorState::Suspended as i32
        );
        assert_eq!(
            waiter_native.suspended_cont.load(Ordering::Acquire),
            waiter_handle
        );
        assert!(!waiter_native.pending_wake.load(Ordering::Acquire));
        assert_eq!(waiter_frame.destroyed.load(Ordering::Acquire), 0);
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);
        // SAFETY: shutdown reset the counter; the refused publish must not
        // increment it.
        assert_eq!(unsafe { read_tasks_spawned() }, 0);

        // Reclaim the caller-side authorities and the two tracked scratch-frame
        // allocations under the test's sole ownership.
        // SAFETY: every pointer remains live and exclusively test-owned.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(reply);
            crate::actor::cancel_parked_activation_for_free_wasm(waiter_native);
            crate::mailbox_wasm::hew_mailbox_free(callee_actor.mailbox.cast());
            drop(crate::coro_exec::test_support::ScratchFrameOwner::from_handle(callee_handle));
        }
        assert_eq!(waiter_frame.destroyed.load(Ordering::Acquire), 1);
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            replies_before
        );
    }

    #[test]
    fn wasm_sched_run_returns_when_shutdown_phase_makes_pending_timers_inert() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        hew_sched_init();

        // Model the re-entrant drain window directly: shutdown deliberately
        // keeps timer ownership alive but makes timer dispatch inert.
        // SAFETY: single-threaded test owns these scheduler statics.
        unsafe {
            ptr::addr_of_mut!(SHUTDOWN_PHASE).write(WasmShutdownPhase::Draining);
            ptr::addr_of_mut!(WASM_SLEEP_COUNT).write(1);
        }
        let start = std::time::Instant::now();
        hew_sched_run();
        let elapsed = start.elapsed();
        assert!(
            elapsed < std::time::Duration::from_secs(1),
            "standalone run must not spin on timed work shutdown cannot dispatch, took {elapsed:?}"
        );

        // Restore only the synthetic state, then use ordinary shutdown.
        // SAFETY: the test still exclusively owns scheduler state.
        unsafe {
            ptr::addr_of_mut!(WASM_SLEEP_COUNT).write(0);
            ptr::addr_of_mut!(SHUTDOWN_PHASE).write(WasmShutdownPhase::Running);
        }
        hew_sched_shutdown();
    }

    /// FINDING 4 (wasm resume-without-reply). The resume edge used to store
    /// null into `suspended_reply_channel` unconditionally when the
    /// continuation went `Ready`. If the resumed body never called `hew_reply`,
    /// that dropped the asking side's ONLY reference on the floor: the ask
    /// never resolved and the channel leaked. The scratch frame here completes
    /// on its first resume without replying — exactly that case.
    #[test]
    fn wasm_resume_without_a_reply_retires_the_orphaned_ask() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        let baseline = crate::reply_channel_wasm::active_channel_count();

        let mut actor = stub_actor();
        // SAFETY: the actor lives for the whole test.
        let (actor_ptr, ch) =
            unsafe { park_wasm_ask(std::ptr::from_mut(&mut actor), suspend_once_dispatch_wasm) };
        let a = as_native_actor(actor_ptr);
        assert_eq!(
            a.suspended_reply_channel.load(Ordering::Relaxed),
            ch.cast::<c_void>(),
            "the suspend edge stashes the unanswered reply channel"
        );

        // Wake the parked continuation; it runs to completion without replying.
        // SAFETY: `actor_ptr` is Suspended with a live parked continuation.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };
        hew_sched_run();

        assert!(
            a.suspended_cont.load(Ordering::Relaxed).is_null(),
            "the completed continuation is destroyed"
        );
        assert!(
            a.suspended_reply_channel.load(Ordering::Relaxed).is_null(),
            "the slot invariant: the retired slot no longer owns a reference"
        );
        // SAFETY: the test still holds its own reference to `ch`.
        unsafe {
            assert!(
                crate::reply_channel_wasm::test_replied(ch),
                "a resume that completes without replying must still RESOLVE the ask"
            );
            assert!(
                crate::reply_channel_wasm::reply_is_orphaned(ch),
                "it resolves as orphaned, not as a legitimate null reply"
            );
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "EXACTLY ONE release: only the caller's own reference is left"
            );
        }

        // SAFETY: releasing the test's own reference exactly once.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(ch.cast());
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            baseline,
            "the reply channel refcount returns to baseline"
        );
        hew_sched_shutdown();
    }

    /// FINDING 5 (wasm suspend-after-consume). The wasm suspend edge used to
    /// stash the node's reply channel whenever the handler returned a suspend
    /// handle, even when the handler had ALREADY replied — and `hew_reply`
    /// releases the sender-side reference on its way out. The slot would then
    /// hold a pointer it did not own, and the next abandonment path would
    /// publish through a dead reference. This pins the invariant the native
    /// edge establishes: the slot is non-null IFF it OWNS an unconsumed
    /// reference.
    #[test]
    fn wasm_suspend_after_reply_leaves_the_slot_null() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        let baseline = crate::reply_channel_wasm::active_channel_count();

        let mut actor = stub_actor();
        // SAFETY: the actor lives for the whole test.
        let (actor_ptr, ch) = unsafe {
            park_wasm_ask(
                std::ptr::from_mut(&mut actor),
                reply_then_suspend_dispatch_wasm,
            )
        };
        let a = as_native_actor(actor_ptr);
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Suspended as i32,
            "the handler still parks after replying"
        );
        assert!(
            a.suspended_reply_channel.load(Ordering::Relaxed).is_null(),
            "a handler that already replied owes nothing: the slot stays null"
        );
        // SAFETY: the test still holds its own reference to `ch`.
        unsafe {
            assert!(
                crate::reply_channel_wasm::test_replied(ch),
                "the handler's reply landed"
            );
            assert!(
                !crate::reply_channel_wasm::reply_is_orphaned(ch),
                "a real reply must not be misreported as an abandoned ask"
            );
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "hew_reply consumed the dispatch's reference; only the caller's remains"
            );
        }

        // Abandon the park anyway: with the slot correctly null this retires
        // nothing and cannot double-release the channel the handler consumed.
        // SAFETY: `actor_ptr` is the live actor this test owns.
        unsafe { crate::actor::actor_stop_wasm_impl(actor_ptr.cast()) };
        hew_sched_run();
        // SAFETY: the test still holds its own reference to `ch`.
        unsafe {
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "abandoning a park after a reply must not release the channel again"
            );
        }

        // SAFETY: releasing the test's own reference exactly once.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(ch.cast());
            crate::mailbox_wasm::hew_mailbox_free(actor.mailbox.cast());
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            baseline,
            "the reply channel refcount returns to baseline"
        );
        hew_sched_shutdown();
    }

    /// Wasm `enqueue_resume` wakes a Suspended actor (Suspended -> Runnable)
    /// and re-enqueues it — the cooperative dual of the native wake edge.
    #[test]
    fn wasm_enqueue_resume_wakes_suspended_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Relaxed);
        actor.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Relaxed,
        );
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Relaxed,
        );
        let actor_ptr: *mut HewActor = (&raw const actor).cast_mut();

        // SAFETY: actor valid; sentinel handle is never resumed by the wake.
        unsafe { enqueue_resume(actor_ptr, ptr::null_mut()) };
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "wasm enqueue_resume CASes Suspended -> Runnable"
        );

        // Clear the parked sentinel + tag so shutdown's run-queue drain does
        // not try to resume the (never-real) sentinel handle. enqueue_resume
        // only performs the wake; the actual resume is the activation's job and
        // is covered by the round-trip test above with a real scratch frame.
        actor.cont_tag.store(
            crate::internal::types::ContTag::Empty as i32,
            Ordering::Relaxed,
        );
        actor
            .suspended_cont
            .store(ptr::null_mut(), Ordering::Relaxed);
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

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

    /// Tear down a hand-wired stack actor's mailbox in the correct
    /// production-mirrored order: null the actor's `mailbox` slot *before*
    /// freeing the box.
    ///
    /// This mirrors [`crate::actor::free_actor_resources_wasm`]'s
    /// null-before-free invariant.  Tests that allocate a mailbox with
    /// [`hew_mailbox_new`] and wire it onto a stack [`HewActor`] must either
    /// call [`hew_sched_shutdown`] *before* freeing the mailbox, or use this
    /// helper *after* shutdown, to prevent a heap-use-after-free during the
    /// shutdown drain.
    ///
    /// # Panics (debug only)
    ///
    /// Panics if `actor.mailbox` does not match `mailbox`, or if `mailbox`
    /// is null (double-free guard).
    ///
    /// # Safety
    ///
    /// * `actor.mailbox` must equal `mailbox` (ownership must match).
    /// * `mailbox` must be a live [`HewMailboxWasm`] allocated by
    ///   [`hew_mailbox_new`].
    /// * The scheduler must have been drained (i.e. [`hew_sched_shutdown`]
    ///   called) so no enqueued actor still holds a reference to `mailbox`.
    #[allow(
        dead_code,
        reason = "test helper used selectively; available for future tests"
    )]
    unsafe fn drop_test_actor_mailbox(
        actor: &mut HewActor,
        mailbox: *mut crate::mailbox_wasm::HewMailboxWasm,
    ) {
        debug_assert!(
            !mailbox.is_null(),
            "drop_test_actor_mailbox: mailbox is null — already freed?"
        );
        debug_assert!(
            actor.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>() == mailbox,
            "drop_test_actor_mailbox: actor.mailbox does not match the supplied \
             mailbox pointer — ownership mismatch"
        );
        // Null the slot first (production parity: free_actor_resources_wasm
        // also nulls before free so the shutdown drain's null-guard fires
        // correctly for any straggler read of a.mailbox).
        actor.mailbox = std::ptr::null_mut();
        // SAFETY: caller guarantees mailbox is live and exclusively owned here.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
    }

    // Dispatch callback that records hew_actor_current_id() into a static,
    // and the silent routing probe into a sibling static for parity checks.
    static DISPATCH_SAW_ACTOR_ID: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);
    static DISPATCH_SAW_SILENT_ACTOR_ID: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);

    unsafe extern "C-unwind" fn dispatch_record_current_id(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let id = crate::actor::hew_actor_current_id();
        DISPATCH_SAW_ACTOR_ID.store(id, std::sync::atomic::Ordering::Relaxed);
        DISPATCH_SAW_SILENT_ACTOR_ID.store(
            crate::actor::hew_actor_current_id_silent(),
            std::sync::atomic::Ordering::Relaxed,
        );

        std::ptr::null_mut()
    }

    /// Bug #1 regression: `hew_actor_self` / `hew_actor_current_id` must return the
    /// dispatching actor's own ID during WASM dispatch, not -1 / null.
    ///
    /// Before the fix, `scheduler_wasm` and actor.rs used different ambient
    /// current-actor slots, so self APIs always saw null / returned -1.
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
        DISPATCH_SAW_SILENT_ACTOR_ID.store(-999, std::sync::atomic::Ordering::Relaxed);
        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };
        hew_sched_run();

        assert_eq!(
            DISPATCH_SAW_ACTOR_ID.load(std::sync::atomic::Ordering::Relaxed),
            42,
            "hew_actor_current_id() must return the dispatching actor's ID, not -1"
        );
        assert_eq!(
            DISPATCH_SAW_SILENT_ACTOR_ID.load(std::sync::atomic::Ordering::Relaxed),
            42,
            "hew_actor_current_id_silent() must agree with the diagnostic \
             accessor under dispatch"
        );

        // SAFETY: mb is a valid mailbox pointer; all messages have been consumed.
        unsafe { hew_mailbox_free(mb) };
        hew_sched_shutdown();

        // Outside dispatch both probes report -1; the silent probe must do so
        // without writing the generic last-error slot (#2658).
        let prev = crate::execution_context::set_current_context(ptr::null_mut());
        crate::hew_clear_error();
        assert_eq!(crate::actor::hew_actor_current_id_silent(), -1);
        assert!(
            crate::hew_last_error().is_null(),
            "silent probe outside dispatch must leave the generic last-error \
             slot untouched"
        );
        assert_eq!(crate::actor::hew_actor_current_id(), -1);
        let _ = crate::execution_context::set_current_context(prev);
        crate::hew_clear_error();
    }

    // Statics for the nested-activation test.
    static OUTER_ID_BEFORE_INNER: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);
    static OUTER_ID_AFTER_INNER: std::sync::atomic::AtomicI64 =
        std::sync::atomic::AtomicI64::new(-999);

    unsafe extern "C-unwind" fn outer_dispatch_nested(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
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

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn inner_dispatch_noop(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // No-op: sufficient to exercise the nested activation path.

        std::ptr::null_mut()
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
        // SAFETY: simulate the scheduler's reply-channel plumbing for a single
        // message. Install a fresh canonical context with the per-message reply
        // channel pre-populated, then run dispatch against it. The
        // `TestExecutionContext` guard pops the ctx on drop.
        unsafe {
            let dispatch = actor.dispatch.expect("test actor must have a dispatch");
            let msg_ref = &*msg;
            let mut sim_ctx = HewExecutionContext {
                reply_channel: msg_ref.reply_channel,
                ..HewExecutionContext::default()
            };
            let prev_ctx = crate::execution_context::set_current_context(&raw mut sim_ctx);
            dispatch(
                &raw mut sim_ctx,
                actor.state,
                msg_ref.msg_type,
                msg_ref.data,
                msg_ref.data_size,
                // P5-RX sub-stage 1: copy-mode receipt (dormant borrow path).
                0,
            );
            let _ = crate::execution_context::set_current_context(prev_ctx);
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
    unsafe extern "C-unwind" fn self_stopping_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
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

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn self_stopping_dispatch_via_api(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: tests pass the live actor pointer itself as `state`.
        unsafe { crate::actor::actor_self_stop_wasm_impl(state.cast::<crate::actor::HewActor>()) };

        std::ptr::null_mut()
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

        unsafe extern "C-unwind" fn capture_arena_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            // Record current arena (as usize) via the internal getter.
            let ptr = crate::arena::set_current_arena(ptr::null_mut()); // read-then-restore
            crate::arena::set_current_arena(ptr); // put it back
            ARENA_DURING_DISPATCH.store(ptr as usize, std::sync::atomic::Ordering::Relaxed);

            std::ptr::null_mut()
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
        unsafe extern "C-unwind" fn alloc_in_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            // SAFETY: arena is installed by the scheduler before dispatch.
            unsafe { crate::arena::hew_arena_malloc(64) };

            std::ptr::null_mut()
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
        unsafe extern "C-unwind" fn outer_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
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

            std::ptr::null_mut()
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

    #[cfg(target_arch = "wasm32")]
    #[test]
    fn runtime_exit_cleans_up_short_lived_actor_program() {
        static STATE_DROP_SEEN: AtomicBool = AtomicBool::new(false);

        unsafe extern "C" fn mark_state_drop(_state: *mut c_void) {
            STATE_DROP_SEEN.store(true, Ordering::Release);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        STATE_DROP_SEEN.store(false, Ordering::Release);
        hew_sched_init();
        let mut initial_state = 1_u8;

        // SAFETY: the one-byte actor state is readable, the actor has no
        // dispatch work, and the spawned actor is owned by the runtime cleanup
        // chain after this point.
        unsafe {
            let actor = crate::actor::hew_actor_spawn(
                (&raw mut initial_state).cast(),
                std::mem::size_of::<u8>(),
                None,
            );
            assert!(!actor.is_null(), "spawn must produce a tracked actor");
            crate::actor::hew_actor_set_state_drop(actor, mark_state_drop);
        }

        hew_wasm_runtime_exit();

        assert!(
            STATE_DROP_SEEN.load(Ordering::Acquire),
            "standalone WASM runtime exit must run cleanup_all_actors"
        );
    }

    /// Regression for #2825: a real wasm actor box whose activation is parked
    /// at standalone runtime exit must be reclaimed, not merely let the process
    /// return successfully. Exit status is not a leak oracle, so this reads the
    /// exact counters at the real `Box::into_raw` / `Box::from_raw` sites.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn runtime_exit_balances_a_parked_actor_box_allocation() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let (allocated_before, freed_before) = crate::actor_balance::actor_box_counts();
        hew_sched_init();

        // SAFETY: zero-sized state is represented by null; the dispatch
        // returns a live scratch continuation which the scheduler parks.
        let actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(suspend_once_dispatch_wasm))
        };
        assert!(!actor.is_null(), "spawn must produce a tracked actor");
        // SAFETY: actor is live; null is valid for a zero-sized payload.
        unsafe { crate::actor::hew_actor_send(actor, 1, ptr::null_mut(), 0) };
        // Drive only the queued activation. `hew_sched_run` is deliberately not
        // used: later variants of this oracle attach a far-future timer to the
        // parked frame, and shutdown must not wait for it.
        // SAFETY: scheduler is initialized and single-threaded.
        let _ = unsafe { hew_wasm_sched_tick(1) };
        assert_eq!(
            // SAFETY: actor remains tracked until runtime cleanup.
            unsafe { (*actor).actor_state.load(Ordering::Acquire) },
            HewActorState::Suspended as i32,
            "the oracle must reach shutdown with a genuinely parked activation"
        );

        hew_sched_shutdown();
        hew_runtime_cleanup();

        let (allocated_after, freed_after) = crate::actor_balance::actor_box_counts();
        assert_eq!(
            (
                allocated_after - allocated_before,
                freed_after - freed_before
            ),
            (1, 1),
            "WASM shutdown must balance the real parked actor-box allocation"
        );
    }

    /// Full shutdown ownership proof on the actual WASM target. One actor stays
    /// parked until the bulk pass; the other is stopped first and therefore
    /// destroyed by the run-queue drain. Both carry a real timer and unanswered
    /// ask, so the test covers bulk-vs-stop overlap, exact-once frame-owned
    /// drops, timer cancellation before teardown, reply retirement, repeated
    /// shutdown/cleanup, and real actor-box balance.
    #[cfg(target_arch = "wasm32")]
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "one end-to-end test keeps the shutdown ownership proof contiguous"
    )]
    fn wasm_shutdown_reclaims_parked_activations_and_debts_once() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let actor_counts_before = crate::actor_balance::actor_box_counts();
        let frame_allocs_before = SHUTDOWN_FRAME_ALLOCS.load(Ordering::Acquire);
        let frame_frees_before = SHUTDOWN_FRAME_FREES.load(Ordering::Acquire);
        let owned_drops_before = SHUTDOWN_FRAME_OWNED_DROPS.load(Ordering::Acquire);
        let timer_cancels_before = SHUTDOWN_FRAME_TIMER_CANCELS.load(Ordering::Acquire);
        let replies_before = crate::reply_channel_wasm::active_channel_count();
        hew_sched_init();

        // SAFETY: zero-sized state is represented by null; both actors are
        // owned by the runtime cleanup chain.
        let bulk_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(shutdown_balance_dispatch))
        };
        // SAFETY: same zero-sized state contract and runtime ownership as the
        // actor above.
        let stopped_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(shutdown_balance_dispatch))
        };
        assert!(!bulk_actor.is_null() && !stopped_actor.is_null());
        let bulk_reply = crate::reply_channel_wasm::hew_reply_channel_new();
        let stopped_reply = crate::reply_channel_wasm::hew_reply_channel_new();
        // SAFETY: actors and channels are live for the complete test.
        let bulk_send = unsafe {
            crate::actor::ask_with_channel_wasm_internal(
                bulk_actor,
                1,
                ptr::null_mut(),
                0,
                bulk_reply.cast(),
            )
        };
        assert_eq!(bulk_send, HewError::Ok as i32);
        // SAFETY: actors and channels are live for the complete test.
        let stopped_send = unsafe {
            crate::actor::ask_with_channel_wasm_internal(
                stopped_actor,
                1,
                ptr::null_mut(),
                0,
                stopped_reply.cast(),
            )
        };
        assert_eq!(stopped_send, HewError::Ok as i32);
        // SAFETY: scheduler is initialized and both queued activations are
        // driven on this single thread.
        let _ = unsafe { hew_wasm_sched_tick(2) };
        for actor in [bulk_actor, stopped_actor] {
            // SAFETY: cleanup has not run; actor remains live and tracked.
            let actor_ref = unsafe { &*actor };
            assert_eq!(
                actor_ref.actor_state.load(Ordering::Acquire),
                HewActorState::Suspended as i32
            );
            assert!(crate::coro_exec::has_live_parked_cont(actor_ref));
            assert!(
                actor_ref
                    .suspended_cancel_token
                    .load(Ordering::Acquire)
                    .is_null(),
                "task-scope cancel tokens are native-only on WASM"
            );
            assert!(
                actor_ref.gen_sink.load(Ordering::Acquire).is_null(),
                "receive-gen sinks are native-only on WASM"
            );
        }
        assert_eq!(crate::timer_periodic_wasm::pending_periodic_count(), 2);

        // Stop one parked actor before shutdown. It is queued and destroyed by
        // the shutdown drain; the untouched actor is destroyed by the bulk pass.
        // SAFETY: stopped_actor is live and parked.
        unsafe { crate::actor::hew_actor_stop(stopped_actor) };
        hew_sched_shutdown();

        for actor in [bulk_actor, stopped_actor] {
            // SAFETY: scheduler shutdown retires frames but actor boxes remain
            // tracked until runtime cleanup below.
            let actor_ref = unsafe { &*actor };
            assert_eq!(
                actor_ref.actor_state.load(Ordering::Acquire),
                HewActorState::Stopped as i32
            );
            assert!(!crate::coro_exec::has_live_parked_cont(actor_ref));
            assert!(actor_ref.suspended_cont.load(Ordering::Acquire).is_null());
            assert!(actor_ref
                .suspended_reply_channel
                .load(Ordering::Acquire)
                .is_null());
        }
        assert_eq!(
            SHUTDOWN_FRAME_ALLOCS.load(Ordering::Acquire) - frame_allocs_before,
            2
        );
        assert_eq!(
            SHUTDOWN_FRAME_FREES.load(Ordering::Acquire) - frame_frees_before,
            2,
            "stop-drain and bulk retirement must free each frame exactly once"
        );
        assert_eq!(
            SHUTDOWN_FRAME_OWNED_DROPS.load(Ordering::Acquire) - owned_drops_before,
            2,
            "each frame-owned heap value must drop exactly once"
        );
        assert_eq!(
            SHUTDOWN_FRAME_TIMER_CANCELS.load(Ordering::Acquire) - timer_cancels_before,
            2,
            "both destroy outlines must cancel while timer machinery is alive"
        );
        assert_eq!(crate::timer_periodic_wasm::pending_periodic_count(), 0);

        for reply in [bulk_reply, stopped_reply] {
            // SAFETY: the test retains the caller-side reference.
            unsafe {
                assert!(crate::reply_channel_wasm::test_replied(reply));
                assert!(crate::reply_channel_wasm::reply_is_orphaned(reply));
                assert_eq!(
                    crate::reply_channel_wasm::test_ref_count(reply),
                    1,
                    "abandonment must retire exactly the sender-side reference"
                );
            }
        }

        // Repeat both public teardown halves. The second calls must be no-ops,
        // not second frame/reply/timer frees.
        hew_sched_shutdown();
        hew_runtime_cleanup();
        hew_runtime_cleanup();
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            (actor_counts_before.0 + 2, actor_counts_before.1 + 2)
        );
        assert_eq!(
            SHUTDOWN_FRAME_FREES.load(Ordering::Acquire) - frame_frees_before,
            2
        );
        assert_eq!(
            SHUTDOWN_FRAME_TIMER_CANCELS.load(Ordering::Acquire) - timer_cancels_before,
            2
        );

        // SAFETY: release only the two caller-side references left above.
        unsafe {
            crate::reply_channel_wasm::hew_reply_channel_free(bulk_reply.cast());
            crate::reply_channel_wasm::hew_reply_channel_free(stopped_reply.cast());
        }
        assert_eq!(
            crate::reply_channel_wasm::active_channel_count(),
            replies_before
        );
    }

    /// Shutdown remains message-quiet even when an already-runnable dispatch
    /// genuinely cooperates while sleep and periodic work are due. The timer
    /// wheel stays allocated so later continuation destroys can cancel it, but
    /// nested scheduler ticks must not fire or re-arm entries once shutdown
    /// begins. Sleeping is a supported host-side state (no coroutine frame):
    /// the pre-wheel retirement pass cancels its registration and latches it
    /// terminal so cleanup reclaims the actor box exactly once.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_shutdown_cooperate_keeps_due_timers_quiet_and_retires_sleeping_actor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let actor_counts_before = crate::actor_balance::actor_box_counts();
        SHUTDOWN_COOPERATE_CALLS.store(0, Ordering::Release);
        SHUTDOWN_PERIODIC_DISPATCHES.store(0, Ordering::Release);
        SHUTDOWN_SLEEP_TERMINATES.store(0, Ordering::Release);
        hew_sched_init();
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);

        let mut sleeper_state = 1_u8;
        // SAFETY: all three actors are real tracked allocations owned by
        // runtime cleanup; sleeper_state is readable for its one-byte clone.
        let sleeper = unsafe {
            crate::actor::hew_actor_spawn(
                (&raw mut sleeper_state).cast(),
                std::mem::size_of::<u8>(),
                None,
            )
        };
        let periodic = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(shutdown_periodic_dispatch))
        };
        let cooperator = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(shutdown_cooperate_dispatch))
        };
        assert!(!sleeper.is_null() && !periodic.is_null() && !cooperator.is_null());
        // SAFETY: sleeper is live and its cloned state is non-null.
        unsafe { crate::actor::hew_actor_set_terminate(sleeper, shutdown_sleep_terminate) };

        // Schedule both entries one millisecond ahead of the pinned clock.
        // SAFETY: actors remain live and tracked through shutdown.
        unsafe {
            park_actor_sleep(
                sleeper.cast::<HewActor>(),
                VIRTUAL_BASE_MS.saturating_add(1),
            );
            let timer = crate::timer_periodic_wasm::hew_actor_schedule_periodic(periodic, 7, 1);
            assert!(!timer.is_null());
        }
        // Sleeping is a host/timer ownership state, never a coroutine-frame
        // ownership state. Retirement relies on this before taking its exact
        // Sleeping -> Stopped transition.
        assert!(
            !crate::coro_exec::has_live_parked_cont(unsafe { &*sleeper }),
            "Sleeping actors must not carry a live continuation"
        );
        assert_eq!(hew_wasm_sleeping_count(), 2);
        // SAFETY: the shared wheel was created by the registrations above.
        let earliest = unsafe {
            crate::timer_wheel::timer_wheel_earliest_abs_deadline_ms(wasm_timer_wheel_raw())
        }
        .expect("due sleep/periodic work must exist");
        assert_eq!(earliest, VIRTUAL_BASE_MS + 1);

        // Queue exactly the actor that cooperates, then advance the test clock
        // so both timer callbacks are genuinely due before shutdown starts.
        // SAFETY: cooperator is a live actor; null is valid for a zero-size
        // message payload.
        unsafe { crate::actor::hew_actor_send(cooperator, 1, ptr::null_mut(), 0) };
        crate::wasm_stubs::pin_virtual_clock(VIRTUAL_BASE_MS + 1);
        assert_eq!(hew_sched_metrics_global_queue_len(), 1);

        hew_sched_shutdown();

        assert_eq!(
            SHUTDOWN_COOPERATE_CALLS.load(Ordering::Acquire),
            1,
            "the runnable dispatch must execute a real cooperate-driven tick"
        );
        assert_eq!(
            SHUTDOWN_PERIODIC_DISPATCHES.load(Ordering::Acquire),
            0,
            "due periodic work must neither publish nor dispatch during shutdown"
        );
        // SAFETY: actor boxes remain tracked until runtime cleanup below.
        assert_eq!(
            unsafe { (*sleeper).actor_state.load(Ordering::Acquire) },
            HewActorState::Stopped as i32,
            "Sleeping is retired exactly once before its wheel entry disappears"
        );
        assert_eq!(hew_wasm_sleeping_count(), 0);
        assert_eq!(crate::timer_periodic_wasm::pending_periodic_count(), 0);
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);

        hew_runtime_cleanup();
        hew_runtime_cleanup();
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            (actor_counts_before.0 + 3, actor_counts_before.1 + 3),
            "sleeping, periodic and cooperating actor boxes must all balance"
        );
        assert_eq!(
            SHUTDOWN_SLEEP_TERMINATES.load(Ordering::Acquire),
            1,
            "Sleeping retirement must run terminate exactly once"
        );
    }

    /// `ACTIVATING=true` is an explicit ownership refusal, not a stale bit the
    /// shutdown path may wave away. A continuation parked by the real dispatch
    /// edge remains in the same `Suspended` latch with the same live frame, and
    /// cleanup leaks its actor box fail-closed.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_activating_shutdown_preserves_genuinely_parked_frame_and_actor_box() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let actor_counts_before = crate::actor_balance::actor_box_counts();
        hew_sched_init();

        // SAFETY: real tracked actor; dispatch returns the scratch frame that
        // the production suspend edge parks.
        let actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(suspend_once_dispatch_wasm))
        };
        assert!(!actor.is_null());
        // SAFETY: actor is live; null is valid for a zero-size message.
        unsafe { crate::actor::hew_actor_send(actor, 1, ptr::null_mut(), 0) };
        // SAFETY: drive exactly the queued production activation.
        let _ = unsafe { hew_wasm_sched_tick(1) };
        // SAFETY: actor remains tracked and parked.
        let handle = unsafe { (*actor).suspended_cont.load(Ordering::Acquire) };
        assert!(!handle.is_null());
        assert_eq!(
            unsafe { (*actor).actor_state.load(Ordering::Acquire) },
            HewActorState::Suspended as i32
        );

        // Simulate host shutdown while activation ownership is still latched.
        // SAFETY: single-threaded test seam.
        unsafe { ptr::addr_of_mut!(ACTIVATING).write(true) };
        hew_sched_shutdown();

        // SAFETY: shutdown deliberately kept the tracked actor allocation live.
        unsafe {
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Suspended as i32
            );
            assert_eq!((*actor).suspended_cont.load(Ordering::Acquire), handle);
            assert_eq!(
                (*actor).cont_tag.load(Ordering::Acquire),
                crate::internal::types::ContTag::Parked as i32
            );
        }

        hew_runtime_cleanup();
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            (actor_counts_before.0 + 1, actor_counts_before.1),
            "unproven activation ownership must preserve the actor box"
        );

        // Repair the intentionally refused test state after observing the
        // leak, then let the executor destroy its frame and reclaim the actor.
        // The dispatch created an executor-owned scratch frame: its destroy
        // outline frees the outer tracked allocation, so this test must not
        // reconstruct a `ScratchFrameOwner` after cancellation.
        // SAFETY: cleanup drained tracking without freeing the actor.
        unsafe {
            crate::actor::cancel_parked_activation_for_free_wasm(&*actor);
            assert!((*actor).suspended_cont.load(Ordering::Acquire).is_null());
            crate::actor::free_actor_resources_wasm(actor);
        }
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            (actor_counts_before.0 + 1, actor_counts_before.1 + 1)
        );
    }

    /// The resource-free choke point runs after timer teardown in canonical
    /// cleanup, so it must not make a second destroy attempt when pre-timer
    /// retirement was skipped. `Done` still owns the frame even though the
    /// lifecycle latch is already terminal.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_post_timer_cleanup_leaks_done_continuation_intact() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let actor_counts_before = crate::actor_balance::actor_box_counts();
        hew_sched_init();
        // SAFETY: real tracked zero-state actor.
        let actor = unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, None) };
        assert!(!actor.is_null());
        let frame_owner = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        let handle = frame_owner.into_handle();
        let frame = handle.cast::<crate::coro_exec::test_support::ScratchFrame>();
        // SAFETY: actor and frame are exclusively owned by this test.
        let a = unsafe { &*actor };
        assert!(crate::coro_exec::begin_park(a).is_ok());
        unsafe { crate::coro_exec::finish_park(a, handle) };
        assert!(crate::coro_exec::begin_resume(a).is_ok());
        assert!(crate::coro_exec::settle_ready(a).is_ok());
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        // Force the pre-timer retirement ownership proof to refuse.
        // SAFETY: single-threaded test seam.
        unsafe { ptr::addr_of_mut!(ACTIVATING).write(true) };
        hew_sched_shutdown();
        crate::hew_clear_error();
        hew_runtime_cleanup();

        // The Stopped latch made cleanup reach resource-free, but that choke
        // point must preserve the complete actor when it sees a live Done
        // continuation after timer teardown.
        assert_eq!(a.suspended_cont.load(Ordering::Acquire), handle);
        assert_eq!(
            a.cont_tag.load(Ordering::Acquire),
            crate::internal::types::ContTag::Done as i32
        );
        // SAFETY: the fail-closed resource path kept the frame live.
        assert_eq!(unsafe { (*frame).destroyed.load(Ordering::Acquire) }, 0);
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            (actor_counts_before.0 + 1, actor_counts_before.1)
        );
        assert_last_error_eq(&format!(
            "WASM actor cleanup refused: actor {:#x} retained a live continuation \
             after pre-timer retirement; actor leaked to avoid UAF",
            a.id
        ));

        // Manual repair after the assertion; production made no post-timer
        // destroy attempt.
        // SAFETY: cleanup drained tracking and the test is the sole owner.
        unsafe {
            assert!(crate::coro_exec::destroy_parked(a).is_ok());
            crate::actor::free_actor_resources_wasm(actor);
            drop(crate::coro_exec::test_support::ScratchFrameOwner::from_handle(handle));
        }
        assert_eq!(
            crate::actor_balance::actor_box_counts(),
            (actor_counts_before.0 + 1, actor_counts_before.1 + 1)
        );
    }

    /// Bite-test the allocation oracle itself: omit one free at the production
    /// shutdown-sweep branch and require the exact delta to become `(1, 0)`.
    /// If either real allocation-site counter stops moving, this self-test
    /// fails instead of blessing every leak regression with a meaningless
    /// balanced zero.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_actor_box_balance_oracle_detects_one_omitted_free() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let (allocated_before, freed_before) = crate::actor_balance::actor_box_counts();
        hew_sched_init();

        // SAFETY: a zero-state, no-dispatch actor is a real tracked actor box
        // owned by runtime cleanup.
        let actor = unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, None) };
        assert!(!actor.is_null(), "spawn must produce a tracked actor");
        crate::actor_balance::omit_next_shutdown_free_for_test();

        hew_sched_shutdown();
        hew_runtime_cleanup();

        let (allocated_after, freed_after) = crate::actor_balance::actor_box_counts();
        let observed = (
            allocated_after - allocated_before,
            freed_after - freed_before,
        );

        // The omission branch deliberately leaves the drained actor box
        // unfreed. Reclaim it directly after recording the oracle result so
        // the test harness itself does not accumulate the intentional leak.
        // SAFETY: cleanup removed the actor from liveness/handle registries but
        // deliberately skipped every actor-resource free; this test is now its
        // sole owner.
        unsafe { crate::actor::free_actor_resources_wasm(actor) };

        assert_eq!(
            observed,
            (1, 0),
            "omitting one real actor free must produce a one-box imbalance"
        );
    }

    /// A tag that does not prove a frame is parked must remain fail-closed.
    /// Even though WASM has no concurrent thread, `Resuming` means the executor
    /// has not transferred destroy ownership; shutdown must refuse the frame
    /// and ordinary cleanup must leak the box instead of guessing.
    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_shutdown_leaks_fail_closed_when_frame_ownership_is_not_proven() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        let actor_counts_before = crate::actor_balance::actor_box_counts();
        hew_sched_init();
        // SAFETY: real zero-state tracked actor, owned by runtime cleanup.
        let actor = unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, None) };
        assert!(!actor.is_null());
        let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        let handle = frame.handle();
        // Fabricate the exact refusal state without resuming the handle: the
        // frame remains live but its `Resuming` tag denies destroy ownership.
        // SAFETY: actor is live and exclusively owned on this WASM thread.
        unsafe {
            (*actor).suspended_cont.store(handle, Ordering::Release);
            (*actor).cont_tag.store(
                crate::internal::types::ContTag::Resuming as i32,
                Ordering::Release,
            );
            (*actor)
                .actor_state
                .store(HewActorState::Suspended as i32, Ordering::Release);
        }

        hew_sched_shutdown();
        // SAFETY: box remains tracked until cleanup; the refused destroy cannot
        // mutate the handle or lifecycle latch.
        unsafe {
            assert_eq!((*actor).suspended_cont.load(Ordering::Acquire), handle);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Suspended as i32
            );
        }
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);

        hew_runtime_cleanup();
        let actor_counts_after_refusal = crate::actor_balance::actor_box_counts();
        assert_eq!(
            (
                actor_counts_after_refusal.0 - actor_counts_before.0,
                actor_counts_after_refusal.1 - actor_counts_before.1
            ),
            (1, 0),
            "unproven frame ownership must leak the actor box fail-closed"
        );

        // Repair the synthetic tag after recording the refusal and reclaim the
        // intentionally leaked test objects directly.
        // SAFETY: cleanup drained liveness but skipped this actor's resources;
        // the test is now sole owner of the actor and frame.
        unsafe {
            (*actor).cont_tag.store(
                crate::internal::types::ContTag::Parked as i32,
                Ordering::Release,
            );
            crate::actor::cancel_parked_activation_for_free_wasm(&*actor);
            crate::actor::free_actor_resources_wasm(actor);
        }
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
    }

    #[cfg(target_arch = "wasm32")]
    #[test]
    fn stdlib_exit_route_cleans_up_short_lived_actor_program_once() {
        static EXIT_CODE: AtomicI32 = AtomicI32::new(i32::MIN);
        static STATE_DROP_COUNT: AtomicU64 = AtomicU64::new(0);

        fn record_exit_code(code: i32) {
            EXIT_CODE.store(code, Ordering::Release);
        }

        unsafe extern "C" fn count_state_drop(_state: *mut c_void) {
            STATE_DROP_COUNT.fetch_add(1, Ordering::AcqRel);
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK.
        unsafe { reset_globals() };
        EXIT_CODE.store(i32::MIN, Ordering::Release);
        STATE_DROP_COUNT.store(0, Ordering::Release);
        hew_sched_init();
        let mut initial_state = 1_u8;

        // SAFETY: the one-byte actor state is readable, the actor has no
        // dispatch work, and the spawned actor is owned by the runtime cleanup
        // chain after this point.
        unsafe {
            let actor = crate::actor::hew_actor_spawn(
                (&raw mut initial_state).cast(),
                std::mem::size_of::<u8>(),
                None,
            );
            assert!(!actor.is_null(), "spawn must produce a tracked actor");
            crate::actor::hew_actor_set_state_drop(actor, count_state_drop);
        }

        crate::with_hew_exit_terminator_for_test(record_exit_code, || crate::hew_exit(7));
        assert_eq!(
            EXIT_CODE.load(Ordering::Acquire),
            7,
            "stdlib exit route must preserve the requested process status"
        );
        hew_wasm_runtime_exit();
        assert_eq!(
            STATE_DROP_COUNT.load(Ordering::Acquire),
            1,
            "stdlib exit route must run cleanup_all_actors exactly once"
        );
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
    unsafe extern "C-unwind" fn dispatch_check_reductions(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: state was set to a valid *mut HewActor by the test.
        let a = unsafe { &*state.cast::<HewActor>() };
        if a.reductions.load(Ordering::Relaxed) != HEW_DEFAULT_REDUCTIONS {
            REDUCTIONS_WRONG_COUNT.fetch_add(1, Ordering::Relaxed);
        }
        NOISY_DISPATCHES.fetch_add(1, Ordering::Relaxed);

        std::ptr::null_mut()
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

        unsafe extern "C-unwind" fn capture_arena_ptr(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            // Peek at the current arena without permanently clearing it.
            let ptr = crate::arena::set_current_arena(ptr::null_mut());
            crate::arena::set_current_arena(ptr); // restore
            ARENA_SEEN.store(ptr as usize, std::sync::atomic::Ordering::Relaxed);

            std::ptr::null_mut()
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
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
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
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
            send_pin_count: std::sync::atomic::AtomicU32::new(0),
            gen_sink: AtomicPtr::new(ptr::null_mut()),
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
            spawn_serial: 99,
            sys_dispatch: None,
            state_drop_consumed: AtomicBool::new(false),
            state_drop_borrowed: AtomicBool::new(false),
            parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
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

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor: (&raw mut actor).cast::<crate::actor::HewActor>(),
            actor_id: actor.id,
            ..HewExecutionContext::default()
        });

        let result = hew_actor_cooperate();
        assert_eq!(result, 0, "cooperate must return 0 when budget remains");
        assert_eq!(
            actor.reductions.load(Ordering::Relaxed),
            99,
            "cooperate must decrement reductions by 1"
        );

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

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor: (&raw mut actor).cast::<crate::actor::HewActor>(),
            actor_id: actor.id,
            ..HewExecutionContext::default()
        });

        let result = hew_actor_cooperate();
        assert_eq!(result, 1, "cooperate must return 1 when budget exhausted");
        assert_eq!(
            actor.reductions.load(Ordering::Relaxed),
            HEW_DEFAULT_REDUCTIONS,
            "cooperate must reset reductions to default after yield"
        );

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

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor: (&raw mut actor).cast::<crate::actor::HewActor>(),
            actor_id: actor.id,
            ..HewExecutionContext::default()
        });

        let result = hew_actor_cooperate();
        assert_eq!(result, 1, "cooperate at zero reductions must yield");
        assert_eq!(
            actor.reductions.load(Ordering::Relaxed),
            HEW_DEFAULT_REDUCTIONS,
            "cooperate must reset reductions after yield at zero"
        );

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

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor: (&raw mut actor).cast::<crate::actor::HewActor>(),
            actor_id: actor.id,
            ..HewExecutionContext::default()
        });

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

    /// `drain_timed_work` re-enqueues actors whose deadline has passed
    /// and leaves actors whose deadline is still in the future.
    #[test]
    fn drain_expired_sleepers_wakes_ready_actors() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        // Pin the virtual clock (wasm32; see VirtualClock): the "b stays asleep
        // at now+200" boundary needs the wheel baseline and the park delay to
        // agree with the test's `now`, which holds deterministically only under
        // the pinned clock.
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        let mut b = stub_actor();
        b.id = 2;
        let b_ptr: *mut HewActor = (&raw mut b);
        b.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // Anchor to the clock (pinned on wasm32) so the wheel receives deadlines
        // that are strictly in the future relative to its current_ms at init
        // time.
        // SAFETY: hew_now_ms has no preconditions (pinned to VIRTUAL_BASE_MS on wasm32).
        let now = unsafe { hew_now_ms() };

        // Park actor `a` 100 ms from now and actor `b` 300 ms from now.
        // SAFETY: actors are valid for the duration of the test.
        unsafe {
            park_actor_sleep(a_ptr, now + 100);
            park_actor_sleep(b_ptr, now + 300);
            assert_eq!(
                hew_wasm_sleeping_count(),
                2,
                "both actors should be sleeping"
            );
        }

        // Advance to now+200: only `a` should wake.
        // SAFETY: Single-threaded test.
        unsafe { drain_timed_work(now + 200) };
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

        // Advance to now+400: `b` should wake.
        // SAFETY: Single-threaded test.
        unsafe { drain_timed_work(now + 400) };
        assert_eq!(
            b.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor b should be Runnable after wake"
        );
        assert_eq!(hew_wasm_sleeping_count(), 0, "no actors sleeping");

        hew_sched_shutdown();
    }

    /// Timer callbacks for stopped/crashed actors are silently discarded
    /// (actor not re-enqueued, sleep count decremented).
    #[test]
    fn drain_expired_sleepers_discards_terminal_actors() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        // Pin the virtual clock (wasm32; see VirtualClock) so the park delay and
        // the explicit drain deadline share one deterministic `now`.
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // SAFETY: hew_now_ms has no preconditions (pinned to VIRTUAL_BASE_MS on wasm32).
        let now = unsafe { hew_now_ms() };

        // SAFETY: actor is valid for the duration of the test.
        unsafe { park_actor_sleep(a_ptr, now + 50) };

        // Mark the actor as stopped before the timer fires.
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);

        // Advance the wheel past the deadline; the callback fires but does
        // NOT re-enqueue the stopped actor.
        // SAFETY: Single-threaded test.
        unsafe { drain_timed_work(now + 100) };
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "sleep count must clear even for stopped actors"
        );
        assert_eq!(
            hew_sched_metrics_global_queue_len(),
            0,
            "run queue should be empty — stopped actor must not be re-enqueued"
        );

        hew_sched_shutdown();
    }

    /// A dispatch that calls `request_sleep` causes the actor to be parked
    /// after the message boundary, not re-enqueued as Runnable.
    #[test]
    fn actor_is_parked_after_sleep_request_in_dispatch() {
        // Declare items before any statements to satisfy `items_after_statements`.
        static DISPATCHED: AtomicI32 = AtomicI32::new(0);
        static REQUESTED_DEADLINE_MS: AtomicU64 = AtomicU64::new(0);
        // SAFETY: `hew_now_ms` is safe to call from dispatch; `request_sleep`
        // is designed to be called from within a dispatch handler.
        unsafe extern "C-unwind" fn sleeping_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            DISPATCHED.fetch_add(1, Ordering::Relaxed);
            // Choose the next timer-wheel boundary at least one L0 revolution
            // away. This forces the request through L1 and verifies that the
            // boundary tick observes the cascaded sleep entry synchronously.
            // SAFETY: hew_now_ms is safe to call from within dispatch.
            let now = unsafe { hew_now_ms() };
            let deadline_ms = now.saturating_add(511) & !255;
            REQUESTED_DEADLINE_MS.store(deadline_ms, Ordering::Release);
            request_sleep(deadline_ms);

            std::ptr::null_mut()
        }

        let _guard = crate::runtime_test_guard();
        DISPATCHED.store(0, Ordering::Relaxed);
        REQUESTED_DEADLINE_MS.store(0, Ordering::Relaxed);
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        // Pin the virtual clock (wasm32; see VirtualClock) so the in-dispatch
        // request_sleep, the park delay, the wheel baseline, and the test's
        // `t0` all share one deterministic `now`, making the exact
        // parked-deadline boundary below reproducible.
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);
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

        // Drive the wheel to just before and exactly at the deadline requested
        // inside dispatch. On native hosts the monotonic clock can advance
        // between test setup and dispatch, so a pre-dispatch estimate is not
        // authoritative.
        let deadline_ms = REQUESTED_DEADLINE_MS.load(Ordering::Acquire);
        assert_ne!(deadline_ms, 0, "dispatch must publish its sleep deadline");

        // One ms before the parked deadline: actor should NOT wake yet.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(deadline_ms.saturating_sub(1)) };
        assert_eq!(woken, 0, "actor should not wake before its deadline");
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32,
            "actor should still be Sleeping before its deadline"
        );

        // At the exact parked deadline: actor should wake.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(deadline_ms) };
        assert_eq!(woken, 1, "actor should wake at its parked deadline");
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor should be Runnable after timer fires"
        );
        assert_eq!(hew_wasm_sleeping_count(), 0, "sleep queue should be empty");

        // Drain the run queue *before* freeing the mailbox.
        //
        // After the timer fires at `hew_wasm_timer_tick(deadline_ms)` above, the
        // actor is re-enqueued in RUN_QUEUE with state Runnable, but no tick has
        // been taken to drain it.  Calling `hew_mailbox_free` while the actor is
        // still enqueued leaves a dangling `a.mailbox` pointer in the slot;
        // `hew_sched_shutdown` → `drain_run_queue_for_shutdown` → `activate_actor_wasm`
        // would then call `hew_mailbox_try_recv` on the freed box — a heap-UAF.
        //
        // Production order (mirroring `free_actor_resources_wasm`): drain/shutdown
        // first so the run queue is empty, *then* free the mailbox.
        hew_sched_shutdown();
        // SAFETY: mailbox was allocated by hew_mailbox_new above; the actor has
        // been fully drained by hew_sched_shutdown so the run queue no longer
        // holds a reference to it.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
    }

    #[test]
    fn sleep_deadline_ignores_a_stale_wheel_cursor() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        // SAFETY: the runtime owns one live wheel and this test serializes access.
        let wheel = unsafe { wasm_timer_wheel() };
        // Model a cursor that differs from the clock sampled by the sleep request.
        // SAFETY: wheel is live and exclusively accessed by this test.
        unsafe { crate::timer_wheel::timer_wheel_advance_cursor_for_test(wheel, 100) };

        let mut actor = stub_actor();
        let actor_ptr: *mut HewActor = (&raw mut actor);
        // SAFETY: hew_now_ms has no preconditions and actor is live.
        let deadline = unsafe { hew_now_ms() }.saturating_add(500);
        // SAFETY: actor remains live until the scheduler is shut down below.
        unsafe { park_actor_sleep(actor_ptr, deadline) };

        // SAFETY: timer access is serialized by the test guard.
        let woken = unsafe { hew_wasm_timer_tick(deadline) };
        assert_eq!(woken, 1, "a sleeping actor must wake at its deadline");
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32
        );

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
        // Pin the virtual clock (wasm32; see VirtualClock) so the requested
        // boundary is deterministic. The seam is inert on native, where
        // absolute scheduling preserves the requested boundary directly.
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // SAFETY: hew_now_ms has no preconditions (pinned to VIRTUAL_BASE_MS on wasm32).
        let now = unsafe { hew_now_ms() };

        // SAFETY: actor valid for duration of test.
        unsafe { park_actor_sleep(a_ptr, now + 1000) };

        let deadline = now + 1000;

        // One ms before the requested deadline: nothing wakes.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(deadline - 1) };
        assert_eq!(woken, 0);
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Sleeping as i32
        );

        // Exactly at the wheel's scheduled deadline: actor wakes.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(deadline) };
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
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { hew_now_ms() };
        let far_future: u64 = now + 3_600_000;
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
        // Pin the virtual clock (wasm32; see VirtualClock) so the park delay and
        // the post-cancel tick share one deterministic `now`.
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // SAFETY: actor is valid; park_actor_sleep requires a live HewActor ptr.
        // SAFETY: hew_now_ms has no preconditions (pinned to VIRTUAL_BASE_MS on wasm32).
        let now = unsafe { hew_now_ms() };
        // SAFETY: actor is valid; park_actor_sleep requires a live HewActor ptr.
        unsafe { park_actor_sleep(a_ptr, now + 500) };
        assert_eq!(hew_wasm_sleeping_count(), 1);

        // Simulate what cleanup_all_actors does before freeing: cancel the entry.
        // SAFETY: Single-threaded; actor is still valid here.
        unsafe { cancel_actor_sleep_queue_entry(a_ptr.cast::<crate::actor::HewActor>()) };

        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "entry must be removed before the actor is freed"
        );

        // A subsequent timer tick must not touch the (now-removed) entry.
        // SAFETY: Single-threaded test.
        let woken = unsafe { hew_wasm_timer_tick(now + 1000) };
        assert_eq!(woken, 0, "no actors should wake after entry was cancelled");

        hew_sched_shutdown();
    }

    // ── Fail-closed schedule-failure regression tests (F-1 / F-2) ──────────

    /// Regression (F-2): when `hew_timer_wheel_schedule_handle` returns a null
    /// handle for a sleep timer, `park_actor_sleep` must drop the `WasmSleepCtx`
    /// Box, NOT increment `WASM_SLEEP_COUNT`, and restore the actor to
    /// `Runnable` so it is not stranded.
    #[test]
    fn park_actor_sleep_fails_closed_when_wheel_null() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // Simulate a wheel that is unavailable (OOM on creation).
        TEST_FORCE_WHEEL_NULL.store(true, Ordering::Relaxed);
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { hew_now_ms() };
        // SAFETY: actor is valid; park_actor_sleep requires a live HewActor ptr.
        unsafe { park_actor_sleep(a_ptr, now + 500) };
        TEST_FORCE_WHEEL_NULL.store(false, Ordering::Relaxed);

        // Must not have registered a sleep entry.
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "WASM_SLEEP_COUNT must not increment on wheel failure"
        );
        // Actor must be Runnable (fail-closed re-enqueue), not stranded.
        assert_eq!(
            a.actor_state.load(Ordering::Relaxed),
            HewActorState::Runnable as i32,
            "actor must be Runnable after sleep scheduling fails"
        );
        // Count must still be zero after shutdown.
        hew_sched_shutdown();
        assert_eq!(
            hew_wasm_sleeping_count(),
            0,
            "WASM_SLEEP_COUNT must remain zero after shutdown"
        );
    }

    /// Regression (F-1): when `wasm_timer_wheel()` returns null (simulated OOM),
    /// `hew_actor_schedule_periodic` must return null without registering the
    /// `WasmPeriodicCtx` or incrementing `WASM_PERIODIC_COUNT`.
    #[test]
    fn schedule_periodic_fails_closed_when_wheel_null() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let mut a = stub_actor();
        let a_ptr: *mut HewActor = (&raw mut a);

        // Simulate wheel unavailability.
        TEST_FORCE_WHEEL_NULL.store(true, Ordering::Relaxed);
        // SAFETY: actor is a valid live pointer; cast to actor::HewActor (same repr).
        let handle = unsafe {
            crate::timer_periodic_wasm::hew_actor_schedule_periodic(
                a_ptr.cast::<crate::actor::HewActor>(),
                1,
                100,
            )
        };
        TEST_FORCE_WHEEL_NULL.store(false, Ordering::Relaxed);

        assert!(handle.is_null(), "must return null on wheel failure");
        assert_eq!(
            crate::timer_periodic_wasm::pending_periodic_count(),
            0,
            "WASM_PERIODIC_COUNT must not increment on wheel failure"
        );
        assert!(
            crate::timer_periodic_wasm::periodic_registry_is_none(),
            "registry must remain None when schedule fails"
        );

        hew_sched_shutdown();
        // Shutdown assertions (F-3) now cover all 5 new statics.
    }

    /// Regression: `PENDING_SLEEP_DEADLINE_MS` must be cleared even when the
    /// actor stops or crashes mid-dispatch, not just on the normal path
    /// (Blocker 3).  If it were not cleared, the pending value would bleed
    /// into the next actor activation.
    #[test]
    fn pending_sleep_cleared_when_actor_crashes_mid_dispatch() {
        // Items before statements.
        static CRASH_COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C-unwind" fn crashing_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
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

            std::ptr::null_mut()
        }

        static NORMAL_COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C-unwind" fn normal_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            NORMAL_COUNT.fetch_add(1, Ordering::Relaxed);

            std::ptr::null_mut()
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

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn wasm_scheduler_releases_state_lock_after_handler_panic() {
        static SUCCESS_COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C-unwind" fn panic_then_success_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            state: *mut c_void,
            _msg_type: i32,
            data: *mut c_void,
            data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            // SAFETY: the test payload is either null or a queued i32 message body.
            let should_panic = !data.is_null()
                && data_size == std::mem::size_of::<i32>()
                && unsafe { *data.cast::<i32>() } == 1;
            assert!(
                !should_panic,
                "intentional wasm actor-state-lock panic release test"
            );
            let count = state.cast::<i32>();
            // SAFETY: the test actor state is a valid `i32` for this actor lifetime.
            unsafe { *count += 1 };
            SUCCESS_COUNT.fetch_add(1, Ordering::Relaxed);

            std::ptr::null_mut()
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        SUCCESS_COUNT.store(0, Ordering::Relaxed);
        // SAFETY: the test owns and frees this mailbox.
        let mb = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut state = 0_i32;
        let mut actor = stub_actor();
        actor.state = (&raw mut state).cast();
        actor.state_size = std::mem::size_of::<i32>();
        actor.dispatch = Some(panic_then_success_dispatch);
        actor.mailbox = mb.cast();
        let actor_ptr: *mut HewActor = (&raw mut actor);

        // SAFETY: the queued messages target the stack-owned test actor while active.
        unsafe {
            queue_wasm_message(actor_ptr, 1);
            queue_wasm_message(actor_ptr, 2);
            activate_actor_wasm(actor_ptr);
        }

        assert_eq!(
            SUCCESS_COUNT.load(Ordering::Relaxed),
            1,
            "second WASM dispatch should run after the first handler panics"
        );
        assert_eq!(state, 1);
        // SAFETY: mb was allocated above; actor is done, free the mailbox.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mb) };
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
    /// delivered only when the timer wheel transitions the actor
    /// from `Sleeping` → `Runnable`.
    ///
    /// Also verifies that the sleep entry is removed from the wheel after the
    /// timer fires (no double-enqueue / phantom wake).
    #[test]
    fn message_to_sleeping_actor_queues_without_early_wake() {
        static DISPATCHED: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C-unwind" fn counting_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            DISPATCHED.fetch_add(1, Ordering::Relaxed);

            std::ptr::null_mut()
        }

        let _guard = crate::runtime_test_guard();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();
        // Pin the virtual clock (wasm32; see VirtualClock) so the park delay and
        // the explicit wake-after-deadline tick share one deterministic `now`.
        let _clock = VirtualClock::pinned_at(VIRTUAL_BASE_MS);

        // SAFETY: hew_mailbox_new returns a valid heap-allocated mailbox.
        let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() };
        let mut a = stub_actor();
        a.dispatch = Some(counting_dispatch);
        a.mailbox = mailbox.cast();
        // Leave state at the stub default (Runnable) — park_actor_sleep will
        // transition it to Sleeping internally.
        let a_ptr: *mut HewActor = (&raw mut a);

        // Park actor directly with a deadline 1 s from now (simulating post-dispatch park).
        // SAFETY: hew_now_ms has no preconditions (pinned to VIRTUAL_BASE_MS on wasm32).
        let now = unsafe { hew_now_ms() };
        // SAFETY: actor is valid; scheduler is initialized.
        unsafe { park_actor_sleep(a_ptr, now + 1000) };
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
        let woken = unsafe { hew_wasm_timer_tick(now + 1001) };
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

        // SAFETY: mailbox was heap-allocated above.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mailbox) };
        hew_sched_shutdown();
    }

    // ── Nested activation regressions ────────────────────────────────────

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
        unsafe extern "C-unwind" fn sleep_requesting_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _data_size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            DRAIN_DISPATCHED.fetch_add(1, Ordering::Relaxed);
            // Far-future absolute deadline — hangs if not cleared on shutdown.
            request_sleep(u64::MAX / 2);

            std::ptr::null_mut()
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
        unsafe extern "C-unwind" fn sleep_then_reply_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            msg_type: i32,
            _data: *mut c_void,
            _size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
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
                //
                // This test intentionally stays on the REAL clock: the reply is
                // delivered by `actor_ask_wasm_impl`'s internal drive loop,
                // which advances time only by wall-clock progress. Do NOT pin a
                // VirtualClock here — a frozen clock would never cross the 1 ms
                // deadline and the ask loop would spin without ever waking.
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
                        let _ = crate::reply_channel_wasm::hew_reply(
                            ch,
                            (&raw mut v).cast(),
                            std::mem::size_of::<i32>(),
                        );
                    }
                }
            }

            std::ptr::null_mut()
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
#[cfg(target_arch = "wasm32")]
#[test]
fn production_wasi_actor_panics_are_module_fatal() {
    assert!(
        cfg!(panic = "abort"),
        "Tier 2 crash policy assumes the shipped wasm32-wasip1 sysroot is panic=abort"
    );
}
