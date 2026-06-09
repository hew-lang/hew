//! Supervisor tree for Hew actors.
//!
//! Implements event-driven supervision with three restart strategies
//! (one-for-one, one-for-all, rest-for-one) and sliding-window restart
//! tracking. Mirrors the C implementation in `hew-codegen/runtime/src/supervisor.c`.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

#[cfg(all(test, not(target_arch = "wasm32")))]
use std::cell::Cell;
use std::ffi::{c_char, c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::actor::{self, HewActor, HewActorOpts};
use crate::internal::types::{HewActorState, HewDispatchFn, HewOverflowPolicy};
use crate::io_time::hew_now_ms;
use crate::mailbox;
use crate::pool::{HewActorPool, PoolStrategy};
use crate::scheduler;
use crate::set_last_error;
use crate::util::{CondvarExt, MutexExt};

#[cfg(feature = "profiler")]
fn supervisor_strategy_name(strategy: c_int) -> &'static str {
    match strategy {
        STRATEGY_ONE_FOR_ONE => "one_for_one",
        STRATEGY_ONE_FOR_ALL => "one_for_all",
        STRATEGY_REST_FOR_ONE => "rest_for_one",
        _ => "unknown",
    }
}

#[cfg(feature = "profiler")]
fn actor_state_name(actor: *mut HewActor) -> &'static str {
    if actor.is_null() {
        return "Stopped";
    }
    // SAFETY: profiler snapshots only registered live pointers.
    let state = unsafe { (*actor).actor_state.load(Ordering::Relaxed) };
    if state == HewActorState::Idle as i32 {
        "Idle"
    } else if state == HewActorState::Runnable as i32 {
        "Runnable"
    } else if state == HewActorState::Running as i32 {
        "Running"
    } else if state == HewActorState::Blocked as i32 {
        "Blocked"
    } else if state == HewActorState::Stopping as i32 {
        "Stopping"
    } else if state == HewActorState::Crashed as i32 {
        "Crashed"
    } else if state == HewActorState::Stopped as i32 {
        "Stopped"
    } else {
        "Unknown"
    }
}

#[cfg(feature = "profiler")]
fn child_name(name: *const c_char, fallback: &str) -> String {
    if name.is_null() {
        fallback.to_owned()
    } else {
        // SAFETY: supervisor child names are stored as valid C strings.
        unsafe { std::ffi::CStr::from_ptr(name) }
            .to_string_lossy()
            .into_owned()
    }
}

#[cfg(feature = "profiler")]
fn append_tree_row(json: &mut String, first: &mut bool, depth: u16, label: &str, state: &str) {
    use std::fmt::Write as _;

    if !*first {
        json.push(',');
    }
    *first = false;
    let _ = write!(json, r#"{{"depth":{depth},"label":"#);
    crate::util::push_json_string(json, label);
    let _ = write!(json, r#","state":"{state}"}}"#);
}

#[cfg(feature = "profiler")]
fn append_supervisor_rows(
    json: &mut String,
    first: &mut bool,
    sup: *mut HewSupervisor,
    depth: u16,
) {
    if sup.is_null() {
        return;
    }

    // SAFETY: top-level supervisor pointers remain valid while registered.
    let supervisor = unsafe { &*sup };
    let self_actor_id = if supervisor.self_actor.is_null() {
        0
    } else {
        // SAFETY: self_actor belongs to the live supervisor.
        unsafe { (*supervisor.self_actor).id }
    };
    let label = format!(
        "⊞ supervisor:{self_actor_id} [{}]",
        supervisor_strategy_name(supervisor.strategy)
    );
    append_tree_row(json, first, depth, &label, "Supervisor");

    for (index, child) in supervisor
        .children
        .iter()
        .take(supervisor.child_count)
        .enumerate()
    {
        let spec = &supervisor.child_specs[index];
        let name = child_name(spec.name, &format!("child[{index}]"));
        let restarts = if spec.circuit_breaker.crash_stats.is_null() {
            0
        } else {
            // SAFETY: crash stats pointer belongs to the child spec.
            unsafe { (*spec.circuit_breaker.crash_stats).total_crashes }
        };
        let label = format!("  {name} (restarts: {restarts})");
        append_tree_row(json, first, depth + 1, &label, actor_state_name(*child));
    }

    for child_sup in &supervisor.child_supervisors {
        append_supervisor_rows(json, first, *child_sup, depth + 1);
    }

    for (i, pool) in supervisor.pool_slots.iter().enumerate() {
        let spec = &supervisor.pool_specs[i];
        let name = child_name(spec.name, &format!("pool[{i}]"));
        // SAFETY: pool pointer is Box-owned by this supervisor and valid while
        // the supervisor lock is held.
        let member_count = unsafe { crate::pool::hew_pool_size(*pool) };
        let label = format!("  {name} (members: {member_count})");
        append_tree_row(json, first, depth + 1, &label, "Pool");
    }
}

#[cfg(feature = "profiler")]
#[must_use]
pub fn snapshot_tree_json() -> String {
    let roots = crate::shutdown::registered_supervisors_snapshot();
    let mut json = String::from("[");
    let mut first = true;
    for root in roots {
        append_supervisor_rows(&mut json, &mut first, root, 0);
    }
    json.push(']');
    json
}

// ---------------------------------------------------------------------------
// Child lookup result types (shared by static and pool ABI)
// ---------------------------------------------------------------------------

/// Reasons for non-`Live` slot results returned by child-lookup ABI functions.
///
/// C ABI: `u8`. Six reasons cover the v0.5 surface; the enum is extensible by
/// adding variants without breaking the `tag` discriminant.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChildSlotReason {
    /// `tag = Live`; reason field unused.
    Ok = 0,
    /// Slot transiently null mid-restart.
    Restarting = 1,
    /// Exponential-backoff window not yet elapsed.
    BackoffDelay = 2,
    /// Circuit breaker tripped; restart suppressed.
    CircuitOpen = 3,
    /// `max_restarts` hit in window; child is Dead.
    BudgetExhausted = 4,
    /// Supervisor is shut down (`cancelled || running == 0`).
    SupervisorShutdown = 5,
    /// Key out of range or pool slot unknown; codegen bug → fail-closed.
    UnknownSlot = 6,
}

/// Result of a typed child-slot or pool-slot lookup.
///
/// C ABI: 16-byte struct passed by value (`tag + reason + padding + handle`).
/// Matches the `RecvError`-style tagged-union pattern used elsewhere in
/// `hew-runtime`.
///
/// # Ownership
///
/// `handle` is a **borrow**, not a transfer of ownership. The supervisor owns
/// the pointed-to actor for the slot's lifetime. The caller must not free it.
/// A subsequent restart may replace the pointer; treat any captured `handle`
/// as valid only within the current scheduler turn.
#[repr(C)]
#[derive(Debug)]
#[allow(
    clippy::pub_underscore_fields,
    reason = "C ABI struct: _pad is part of the wire layout"
)]
pub struct ChildLookupResult {
    /// Discriminant: 0 = Live, 1 = Transient, 2 = Dead.
    pub tag: u8,
    /// When `tag` is 1 or 2: a [`ChildSlotReason`] discriminant.
    pub reason: u8,
    /// Reserved alignment padding (callers treat as 0).
    pub _pad: [u8; 6],
    /// When `tag = 0`: the live `*mut HewActor`.
    ///
    /// For pool-slot lookups (`hew_supervisor_pool_child_get`), this field
    /// carries the actor PID (u64) encoded as a pointer-width integer. Use
    /// `hew_pid_resolve` (when available) or `hew_actor_send_by_pid` to
    /// route messages to pool members without dereferencing the value as a
    /// pointer.
    ///
    /// When `tag` is non-zero: null.
    pub handle: *mut HewActor,
}

impl ChildLookupResult {
    /// Construct a `Live` result carrying a valid actor pointer.
    #[must_use]
    pub fn live(handle: *mut HewActor) -> Self {
        Self {
            tag: 0,
            reason: ChildSlotReason::Ok as u8,
            _pad: [0; 6],
            handle,
        }
    }

    /// Construct a `Transient` result (slot is temporarily unavailable).
    #[must_use]
    pub fn transient(reason: ChildSlotReason) -> Self {
        Self {
            tag: 1,
            reason: reason as u8,
            _pad: [0; 6],
            handle: ptr::null_mut(),
        }
    }

    /// Construct a `Dead` result (slot will not recover without intervention).
    #[must_use]
    pub fn dead(reason: ChildSlotReason) -> Self {
        Self {
            tag: 2,
            reason: reason as u8,
            _pad: [0; 6],
            handle: ptr::null_mut(),
        }
    }

    /// Returns true if this result is `Live`.
    #[must_use]
    pub fn is_live(&self) -> bool {
        self.tag == 0
    }
}

// SAFETY: `handle` is a raw pointer to a `HewActor`. `HewActor` is `Send`;
// the pointer is only read by the receiver under the supervisor's slot lock.
unsafe impl Send for ChildLookupResult {}

// ---------------------------------------------------------------------------
// Pool slot substrate
// ---------------------------------------------------------------------------

/// Internal specification for a pool declared in a supervisor surface.
///
/// Parallel to `InternalChildSpec` for static children, but tracks
/// pool-specific attributes: routing strategy, capacity, and name.
struct InternalPoolSpec {
    /// Human-readable pool name (C string, heap-allocated via `libc::strdup`).
    name: *mut c_char,
    /// Routing strategy recorded for diagnostics; the live strategy is owned
    /// by the parallel `HewActorPool` in `pool_slots`.
    #[expect(
        dead_code,
        reason = "strategy is in HewActorPool; recorded here for diagnostics only"
    )]
    strategy: PoolStrategy,
    /// Soft cap on pool members (0 = unlimited).
    max_members: usize,
}

impl Drop for InternalPoolSpec {
    fn drop(&mut self) {
        if !self.name.is_null() {
            // SAFETY: name was allocated with libc::strdup.
            unsafe { libc::free(self.name.cast::<c_void>()) };
            self.name = ptr::null_mut();
        }
    }
}

// SAFETY: `name` is a heap-allocated C string owned exclusively by this spec.
// The supervisor serializes all access; no concurrent &-refs occur.
unsafe impl Send for InternalPoolSpec {}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Initial capacity for the dynamic children `Vec`.
const SUP_INITIAL_CAPACITY: usize = 16;
const MAX_RESTARTS_TRACK: usize = 32;

/// Restart strategies.
const STRATEGY_ONE_FOR_ONE: c_int = 0;
const STRATEGY_ONE_FOR_ALL: c_int = 1;
const STRATEGY_REST_FOR_ONE: c_int = 2;

/// Restart policies.
const RESTART_PERMANENT: c_int = 0;
const RESTART_TRANSIENT: c_int = 1;
const RESTART_TEMPORARY: c_int = 2;

// ── Exit reasons ─────────────────────────────────────────────────────────
//
// Trap error codes used by `hew_actor_trap` to distinguish named exit kinds
// from raw OS signal numbers. OS signal numbers are < 32 on all supported
// platforms. Hew-specific codes start at 200 to leave room for POSIX signals
// and any future OS-specific ranges.

/// Error code stored in `actor.error_code` when an actor's arena cap is
/// exhausted. Produced by `hew_arena_malloc` routing through the longjmp
/// crash seam when `cap > 0` and an allocation would exceed it.
///
/// Distinct from any POSIX signal number (which are < 32 on all supported
/// platforms). Code 200 is reserved for this purpose and must not be reused
/// for any other exit kind.
pub const HEW_TRAP_HEAP_EXCEEDED: i32 = 200;

/// Error code recorded when a MIR `Terminator::Trap { kind: IntegerOverflow }`
/// fires inside an actor dispatch. Routed through the longjmp crash seam by
/// `hew_trap_with_code` so the supervisor can distinguish overflow from a
/// raw SIGILL (the LLVM `llvm.trap` fallback on non-actor contexts).
pub const HEW_TRAP_INTEGER_OVERFLOW: i32 = 201;

/// Error code recorded for `Terminator::Trap { kind: DivideByZero }`.
pub const HEW_TRAP_DIVIDE_BY_ZERO: i32 = 202;

/// Error code recorded for `Terminator::Trap { kind: SignedMinDivNegOne }`
/// — signed integer division of the minimum value by `-1`, whose mathematical
/// result is not representable in the operand width.
pub const HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE: i32 = 203;

/// Error code recorded for `Terminator::Trap { kind: ShiftOutOfRange }`.
pub const HEW_TRAP_SHIFT_OUT_OF_RANGE: i32 = 204;

/// Error code recorded for `Terminator::Trap { kind: IndexOutOfBounds }`.
pub const HEW_TRAP_INDEX_OUT_OF_BOUNDS: i32 = 205;

/// Named exit reason for a crashed actor.
///
/// Interprets the i32 `error_code` stored on a `HewActor` after a crash.
/// The supervisor sees a raw `HewActorState::Crashed`; callers can call
/// `ExitReason::from_error_code(hew_actor_get_error(actor))` to get a named
/// reason.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExitReason {
    /// Actor's per-dispatch arena cap was exceeded (error code 200).
    ///
    /// The arena returned null from `hew_arena_malloc` because allocating the
    /// requested bytes would push `used` above `cap`. The actor crashed via
    /// the normal longjmp/supervisor seam with this code stored as its
    /// `error_code`, so teardown (timers, links, monitors, arena) runs as
    /// for any other crash (`cleanup-all-exits` LESSON).
    HeapExceeded,
    /// Actor crashed because a `Terminator::Trap { kind: IntegerOverflow }`
    /// fired during dispatch (error code 201). The MIR producer attaches
    /// this kind to `+`, `-`, `*` overflow on the default (non-wrapping)
    /// operators; codegen routes through `hew_trap_with_code(201)` before
    /// emitting `llvm.trap` as the non-actor fallback.
    IntegerOverflow,
    /// Actor crashed on `/` or `%` with a zero divisor (error code 202).
    DivideByZero,
    /// Actor crashed on signed `i{N}::MIN / -1` (error code 203), whose
    /// mathematical result overflows the operand width.
    SignedMinDivNegOne,
    /// Actor crashed on `<<` or `>>` with a shift count outside `[0, width)`
    /// (error code 204).
    ShiftOutOfRange,
    /// Actor crashed on an out-of-bounds index into a `Vec<T>` or array
    /// (error code 205).
    IndexOutOfBounds,
    /// Actor crashed with a hardware signal (SIGSEGV, SIGBUS, SIGFPE, SIGILL)
    /// or via `hew_panic()` / `hew_panic_msg()`. The raw signal number is
    /// preserved.
    Signal(i32),
    /// Actor stopped normally (`error_code` == 0).
    Normal,
}

impl ExitReason {
    /// Convert a raw `error_code` from `hew_actor_get_error` into a named
    /// `ExitReason`.
    #[must_use]
    pub fn from_error_code(code: i32) -> Self {
        match code {
            0 => ExitReason::Normal,
            HEW_TRAP_HEAP_EXCEEDED => ExitReason::HeapExceeded,
            HEW_TRAP_INTEGER_OVERFLOW => ExitReason::IntegerOverflow,
            HEW_TRAP_DIVIDE_BY_ZERO => ExitReason::DivideByZero,
            HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE => ExitReason::SignedMinDivNegOne,
            HEW_TRAP_SHIFT_OUT_OF_RANGE => ExitReason::ShiftOutOfRange,
            HEW_TRAP_INDEX_OUT_OF_BOUNDS => ExitReason::IndexOutOfBounds,
            sig => ExitReason::Signal(sig),
        }
    }
}

/// C-ABI trap entry-point invoked by codegen-emitted IR before the
/// `llvm.trap` terminator on a `Terminator::Trap { kind }` block.
///
/// Inside an actor-dispatch context, this records `code` as the actor's
/// crash reason and longjmps back to the scheduler's recovery frame —
/// matching the `HEW_TRAP_HEAP_EXCEEDED` precedent. Outside a dispatch
/// context (top-level `main`, `hew eval` REPL, JIT preview) there is no
/// recovery context; `try_direct_longjmp_with_code` is a no-op and this
/// function returns. The caller's `llvm.trap` then aborts the process
/// with SIGILL, preserving today's behaviour for non-actor crashes.
///
/// # Safety
///
/// Must be called from a worker thread that may or may not be in a
/// dispatch context; the underlying `try_direct_longjmp_with_code` is
/// safe to call in either case (it checks the thread-local recovery
/// context). Codegen always pairs the call with `llvm.trap` +
/// `unreachable` to keep the LLVM basic block terminated when the
/// longjmp path is inactive.
#[no_mangle]
pub unsafe extern "C" fn hew_trap_with_code(code: i32) {
    // SAFETY: `try_direct_longjmp_with_code` checks the per-thread
    // recovery context internally; it is a no-op when none is active.
    unsafe {
        crate::signal::try_direct_longjmp_with_code(code);
    }
}

/// System message types for supervisor events.
const SYS_MSG_CHILD_STOPPED: i32 = 100;
const SYS_MSG_CHILD_CRASHED: i32 = 101;
const SYS_MSG_SUPERVISOR_STOP: i32 = 102;

/// Link propagation system message (when linked actor crashes).
pub const SYS_MSG_EXIT: i32 = 103;
/// Monitor notification system message (when monitored actor dies).
pub const SYS_MSG_DOWN: i32 = 104;
/// Delayed restart: timer thread → supervisor mailbox (avoids budget race).
const SYS_MSG_DELAYED_RESTART: i32 = 105;

/// Payload for [`SYS_MSG_DELAYED_RESTART`] system messages.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct DelayedRestartEvent {
    child_index: usize,
}

/// Overflow policy: drop new messages.
const OVERFLOW_DROP_NEW: c_int = 1;

/// Default maximum restart delay in milliseconds (30 seconds).
const DEFAULT_MAX_RESTART_DELAY_MS: u64 = 30_000;

/// Initial restart delay in milliseconds.
const INITIAL_RESTART_DELAY_MS: u64 = 100;

// ---------------------------------------------------------------------------
// Child spec
// ---------------------------------------------------------------------------

/// Specification for a supervised child actor.
#[repr(C)]
#[derive(Debug)]
pub struct HewChildSpec {
    pub name: *const c_char,
    pub init_state: *mut c_void,
    pub init_state_size: usize,
    pub dispatch: Option<HewDispatchFn>,
    pub restart_policy: c_int,
    pub mailbox_capacity: c_int,
    pub overflow: c_int,
    /// Per-dispatch arena cap in bytes. 0 = unbounded. Mirrors
    /// `hew_actor_spawn_opts::arena_cap_bytes`; supervisor restart path
    /// re-applies this cap to every restarted child so `#[max_heap(N)]`
    /// actors retain their cap across crashes.
    pub arena_cap_bytes: usize,
}

/// Child lifecycle event (sent as system message payload).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct ChildEvent {
    child_index: c_int,
    child_id: u64,
    exit_state: c_int,
}

// ---------------------------------------------------------------------------
// Supervisor init function type (for nested supervisor restart)
// ---------------------------------------------------------------------------

/// Init function pointer type for child supervisors.
/// Called to create and start a fresh supervisor instance.
/// Returns a pointer to the new `HewSupervisor`.
pub type SupervisorInitFn = unsafe extern "C" fn() -> *mut HewSupervisor;

/// Specification for a child supervisor so the parent can restart it.
#[derive(Debug)]
struct SupervisorChildSpec {
    init_fn: SupervisorInitFn,
}

// ---------------------------------------------------------------------------
// Supervisor struct
// ---------------------------------------------------------------------------

/// Supervisor managing a set of child actors.
pub struct HewSupervisor {
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,

    children: Vec<*mut HewActor>,
    child_specs: Vec<InternalChildSpec>,
    child_count: usize,

    /// Child supervisors managed by this supervisor.
    child_supervisors: Vec<*mut HewSupervisor>,
    /// Restart specs for child supervisors (parallel to `child_supervisors`).
    /// Entries are `None` for child supervisors added without an init fn.
    child_supervisor_specs: Vec<Option<SupervisorChildSpec>>,

    restart_times: [u64; MAX_RESTARTS_TRACK],
    restart_count: usize,
    restart_head: usize,

    running: AtomicI32,
    cancelled: AtomicBool,
    teardown_claimed: AtomicBool,
    pending_restart_timers: AtomicUsize,
    self_actor: *mut HewActor,
    /// Serializes public child-slot reads against restart-time replacement.
    children_lock: Mutex<()>,

    /// Parent supervisor (set by `hew_supervisor_add_child_supervisor`).
    parent: *mut HewSupervisor,
    /// Index of this supervisor in parent's `child_supervisors` vec.
    index_in_parent: usize,

    /// Restart notification shared by public wait helpers.
    /// The counter increments once per completed restart cycle (including
    /// budget exhaustion), and `hew_supervisor_set_restart_notify` resets it
    /// for deterministic test sequencing.
    restart_notify: Option<Arc<(Mutex<usize>, Condvar)>>,

    /// Pool children declared via `pool name: Type` in the supervisor surface.
    ///
    /// Indexed by checker-assigned pool slot index. Disjoint from the static
    /// `children` slot space — a `pool_key` of 0 is the first *pool* child,
    /// not the first static child. Each entry is a `Box`-owned `HewActorPool`.
    pool_slots: Vec<*mut HewActorPool>,
    /// Parallel spec for pool children. `pool_slots[i]` and `pool_specs[i]`
    /// always have the same length.
    pool_specs: Vec<InternalPoolSpec>,
}

/// Circuit breaker configuration and state for a child.
#[derive(Debug)]
struct CircuitBreakerState {
    /// Circuit breaker state: CLOSED, OPEN, or `HALF_OPEN`.
    state: c_int,
    /// Maximum crashes allowed within `window_secs` before opening.
    max_crashes: u32,
    /// Time window in seconds for tracking crashes.
    window_secs: u32,
    /// Cooldown period in seconds before transitioning from OPEN to `HALF_OPEN`.
    cooldown_secs: u32,
    /// Timestamp when circuit was opened (monotonic nanoseconds).
    opened_at_ns: u64,
    /// Crash statistics for this child.
    crash_stats: *mut crate::crash::CrashStats,
}

impl Default for CircuitBreakerState {
    fn default() -> Self {
        Self {
            state: 0, // HEW_CIRCUIT_BREAKER_CLOSED
            max_crashes: 0,
            window_secs: 0,
            cooldown_secs: 60,
            opened_at_ns: 0,
            // SAFETY: crash::hew_crash_stats_new returns valid pointer.
            crash_stats: unsafe { crate::crash::hew_crash_stats_new() },
        }
    }
}

impl Drop for CircuitBreakerState {
    fn drop(&mut self) {
        if !self.crash_stats.is_null() {
            // SAFETY: crash_stats was created by hew_crash_stats_new.
            unsafe { crate::crash::hew_crash_stats_free(self.crash_stats) };
        }
    }
}

/// Internal owned copy of a child spec.
#[derive(Debug)]
struct InternalChildSpec {
    name: *mut c_char,
    init_state: *mut c_void,
    init_state_size: usize,
    dispatch: Option<HewDispatchFn>,
    restart_policy: c_int,
    mailbox_capacity: c_int,
    overflow: c_int,
    /// Exponential backoff restart delay in milliseconds.
    restart_delay_ms: u64,
    /// Maximum restart delay (default 30 seconds).
    max_restart_delay_ms: u64,
    /// Next allowed restart time (monotonic nanoseconds).
    next_restart_time_ns: u64,
    /// Circuit breaker state for this child.
    circuit_breaker: CircuitBreakerState,
    /// Per-dispatch arena cap in bytes. 0 = unbounded. Copied from
    /// `HewChildSpec::arena_cap_bytes` at spec-registration time and
    /// applied by every restart path so restarted actors keep the cap
    /// originally set by `#[max_heap(N)]`.
    arena_cap_bytes: usize,
    /// Codegen-emitted drop callback for owned state fields (e.g. `Vec`, `String`).
    /// Registered via [`hew_supervisor_set_child_state_drop`] after the child spec
    /// is added. Every restart path calls this on the newly spawned actor so that
    /// restarted actors free their heap-allocated state fields on teardown.
    state_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
}

impl Drop for InternalChildSpec {
    fn drop(&mut self) {
        if !self.init_state.is_null() {
            // SAFETY: init_state was allocated with libc::malloc in
            // hew_supervisor_add_child_spec.
            unsafe { libc::free(self.init_state) };
            self.init_state = ptr::null_mut();
        }
        if !self.name.is_null() {
            // SAFETY: name was allocated with libc::strdup in
            // hew_supervisor_add_child_spec.
            unsafe { libc::free(self.name.cast::<c_void>()) };
            self.name = ptr::null_mut();
        }
    }
}

impl Default for InternalChildSpec {
    fn default() -> Self {
        Self {
            name: ptr::null_mut(),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            dispatch: None,
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            restart_delay_ms: 0,
            max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
            next_restart_time_ns: 0,
            circuit_breaker: CircuitBreakerState::default(),
            arena_cap_bytes: 0,
            state_drop_fn: None,
        }
    }
}

impl std::fmt::Debug for HewSupervisor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewSupervisor")
            .field("strategy", &self.strategy)
            .field("child_count", &self.child_count)
            .finish_non_exhaustive()
    }
}

// SAFETY: Supervisor is accessed through C ABI calls which assume
// single-threaded access or external synchronisation.
unsafe impl Send for HewSupervisor {}
// SAFETY: All mutable access is through `*mut` pointers in C ABI functions
// which rely on external synchronisation; no concurrent &-ref sharing occurs.
unsafe impl Sync for HewSupervisor {}

/// Wrapper to send an actor pointer to a background thread for deferred cleanup.
struct DeferredFree(*mut HewActor);
// SAFETY: `HewActor` is `Send`; the pointer is exclusively owned by the
// receiving thread after the supervisor nulls its copy.
unsafe impl Send for DeferredFree {}

/// Wrapper to stop an exhausted child supervisor off the scheduler thread.
#[derive(Clone, Copy)]
struct DeferredSupervisorStop(*mut HewSupervisor);
// SAFETY: ownership is transferred to the background thread after the parent
// replaces its slot with a fresh child supervisor.
unsafe impl Send for DeferredSupervisorStop {}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Get the current monotonic time in nanoseconds (cross-platform).
fn monotonic_time_ns() -> u64 {
    use std::sync::OnceLock;
    use std::time::Instant;
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    let epoch = EPOCH.get_or_init(Instant::now);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ns since process start won't exceed u64"
    )]
    {
        epoch.elapsed().as_nanos() as u64
    }
}

/// Count restarts within the sliding window.
#[expect(clippy::cast_sign_loss, reason = "C ABI: window_secs is non-negative")]
fn restart_within_window(sup: &HewSupervisor) -> c_int {
    // SAFETY: no preconditions.
    let now = unsafe { hew_now_ms() };
    let window_ms = (sup.window_secs as u64).wrapping_mul(1000);

    let mut count: c_int = 0;
    let limit = sup.restart_count.min(MAX_RESTARTS_TRACK);
    for i in 0..limit {
        let idx = (sup.restart_head + MAX_RESTARTS_TRACK - 1 - i) % MAX_RESTARTS_TRACK;
        if now.wrapping_sub(sup.restart_times[idx]) <= window_ms {
            count += 1;
        } else {
            break;
        }
    }
    count
}

/// Record a restart timestamp.
fn record_restart(sup: &mut HewSupervisor) {
    // SAFETY: no preconditions.
    sup.restart_times[sup.restart_head] = unsafe { hew_now_ms() };
    sup.restart_head = (sup.restart_head + 1) % MAX_RESTARTS_TRACK;
    if sup.restart_count < MAX_RESTARTS_TRACK {
        sup.restart_count += 1;
    }
}

/// Escalate a failure to the parent supervisor.
///
/// Sends a `SYS_MSG_CHILD_CRASHED` system message with `child_index = -1`
/// to indicate a child supervisor (not actor) has failed. `child_id` carries
/// this supervisor's index in the parent's `child_supervisors` vec.
///
/// # Safety
///
/// `sup.parent` must be non-null and point to a valid `HewSupervisor`.
fn escalate_to_parent(sup: &HewSupervisor) {
    // SAFETY: caller guarantees parent is valid.
    let parent = unsafe { &*sup.parent };
    if parent.self_actor.is_null() {
        return;
    }
    let event = ChildEvent {
        child_index: -1,
        child_id: sup.index_in_parent as u64,
        exit_state: HewActorState::Crashed as c_int,
    };
    // SAFETY: parent.self_actor is valid.
    unsafe {
        let mb = (*parent.self_actor)
            .mailbox
            .cast::<crate::mailbox::HewMailbox>();
        mailbox::hew_mailbox_send_sys(
            mb,
            SYS_MSG_CHILD_CRASHED,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildEvent>(),
        );
        let current = (*parent.self_actor).actor_state.load(Ordering::Acquire);
        if current == HewActorState::Idle as i32
            && (*parent.self_actor)
                .actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Relaxed,
                )
                .is_ok()
        {
            scheduler::sched_enqueue(parent.self_actor);
        }
    }
}

/// Check if circuit breaker allows restart for a child.
#[expect(
    clippy::match_same_arms,
    reason = "CLOSED and HALF_OPEN have same logic but different semantic meaning"
)]
fn circuit_breaker_should_restart(spec: &mut InternalChildSpec) -> bool {
    // If circuit breaker is not configured (max_crashes == 0), always allow restart
    if spec.circuit_breaker.max_crashes == 0 {
        return true;
    }

    let now_ns = monotonic_time_ns();

    match spec.circuit_breaker.state {
        0 => {
            // HEW_CIRCUIT_BREAKER_CLOSED
            // Always allow restart when closed
            true
        }
        1 => {
            // HEW_CIRCUIT_BREAKER_OPEN
            // Check if cooldown period has passed
            let cooldown_ns =
                u64::from(spec.circuit_breaker.cooldown_secs).wrapping_mul(1_000_000_000);
            if now_ns.wrapping_sub(spec.circuit_breaker.opened_at_ns) >= cooldown_ns {
                // Transition to half-open for probe restart
                spec.circuit_breaker.state = 2; // HEW_CIRCUIT_BREAKER_HALF_OPEN
                true
            } else {
                false
            }
        }
        2 => {
            // HEW_CIRCUIT_BREAKER_HALF_OPEN
            // Allow one probe restart
            true
        }
        _ => false,
    }
}

/// Update circuit breaker state after a crash.
fn circuit_breaker_record_crash(spec: &mut InternalChildSpec, signal: i32) {
    let now_ns = monotonic_time_ns();

    // Record crash in statistics
    if !spec.circuit_breaker.crash_stats.is_null() {
        // SAFETY: crash_stats was created by hew_crash_stats_new.
        unsafe {
            crate::crash::hew_crash_stats_record(spec.circuit_breaker.crash_stats, signal, now_ns);
        }
    }

    // Check if circuit breaker is configured (max_crashes > 0)
    if spec.circuit_breaker.max_crashes == 0 {
        return;
    }

    match spec.circuit_breaker.state {
        0 => {
            // HEW_CIRCUIT_BREAKER_CLOSED
            // Check if we should open the circuit
            let window_ns = u64::from(spec.circuit_breaker.window_secs).wrapping_mul(1_000_000_000);
            if !spec.circuit_breaker.crash_stats.is_null() {
                // SAFETY: crash_stats was created by hew_crash_stats_new.
                let recent_count = unsafe {
                    crate::crash::hew_crash_stats_recent_count(
                        spec.circuit_breaker.crash_stats,
                        window_ns,
                    )
                };
                if recent_count >= spec.circuit_breaker.max_crashes {
                    spec.circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN
                    spec.circuit_breaker.opened_at_ns = now_ns;
                }
            }
        }
        2 => {
            // HEW_CIRCUIT_BREAKER_HALF_OPEN
            // Probe restart failed, go back to open
            spec.circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN
            spec.circuit_breaker.opened_at_ns = now_ns;
        }
        _ => {
            // Already open, no state change needed
        }
    }
}

/// Update circuit breaker state after a successful restart.
fn circuit_breaker_record_success(spec: &mut InternalChildSpec) {
    if spec.circuit_breaker.state == 2 {
        // HEW_CIRCUIT_BREAKER_HALF_OPEN
        // Probe restart succeeded, close the circuit
        spec.circuit_breaker.state = 0; // HEW_CIRCUIT_BREAKER_CLOSED
    }
}

/// Check if enough time has passed for a delayed restart.
fn restart_delay_allows_restart(spec: &InternalChildSpec) -> bool {
    if spec.next_restart_time_ns == 0 {
        return true;
    }
    let now_ns = monotonic_time_ns();
    now_ns >= spec.next_restart_time_ns
}

/// Apply exponential backoff delay to the child spec.
fn apply_restart_backoff(spec: &mut InternalChildSpec) {
    if spec.restart_delay_ms == 0 {
        // First restart, set to initial delay
        spec.restart_delay_ms = INITIAL_RESTART_DELAY_MS;
    } else {
        // Double the delay, capped at max
        spec.restart_delay_ms = spec
            .restart_delay_ms
            .wrapping_mul(2)
            .min(spec.max_restart_delay_ms);
    }

    let delay_ns = spec.restart_delay_ms.wrapping_mul(1_000_000);
    spec.next_restart_time_ns = monotonic_time_ns().wrapping_add(delay_ns);
}

/// Increment the restart counter and wake any thread waiting on
/// [`hew_supervisor_wait_restart`].
fn notify_restart(sup: &HewSupervisor) {
    if let Some(ref pair) = sup.restart_notify {
        let mut count = pair.0.lock_or_recover();
        *count += 1;
        pair.1.notify_all();
    }
}

fn load_child_slot(sup: &HewSupervisor, index: usize) -> *mut HewActor {
    let _guard = sup.children_lock.lock_or_recover();
    sup.children.get(index).copied().unwrap_or(ptr::null_mut())
}

fn store_child_slot(sup: &mut HewSupervisor, index: usize, child: *mut HewActor) {
    let _guard = sup.children_lock.lock_or_recover();
    if let Some(slot) = sup.children.get_mut(index) {
        *slot = child;
    }
}

fn push_child_slot(sup: &mut HewSupervisor, child: *mut HewActor) {
    let _guard = sup.children_lock.lock_or_recover();
    sup.children.push(child);
}

fn take_child_slot(sup: &mut HewSupervisor, index: usize) -> *mut HewActor {
    let _guard = sup.children_lock.lock_or_recover();
    let Some(slot) = sup.children.get_mut(index) else {
        return ptr::null_mut();
    };
    let child = *slot;
    *slot = ptr::null_mut();
    child
}

/// Stop this supervisor, notify waiters, and escalate to the parent if present.
fn stop_and_maybe_escalate(sup: &mut HewSupervisor) {
    sup.running.store(0, Ordering::Release);
    notify_restart(sup);
    if !sup.parent.is_null() {
        escalate_to_parent(sup);
    }
}

fn stop_deferred_supervisor(deferred: DeferredSupervisorStop) {
    // SAFETY: ownership was transferred to this background thread.
    unsafe { hew_supervisor_stop(deferred.0) };
}

fn stop_owned_deferred_supervisor(deferred: DeferredSupervisorStop) {
    // SAFETY: teardown ownership was claimed by the caller before this thread
    // was spawned, so this background thread is the unique destructor.
    unsafe { stop_supervisor_owned(deferred.0) };
}

fn spawn_deferred_supervisor_stop(
    child_sup: *mut HewSupervisor,
    allow_sync_fallback: bool,
) -> bool {
    if child_sup.is_null() {
        return true;
    }

    let child_addr = child_sup as usize;
    if std::thread::Builder::new()
        .name("deferred-sup-stop".into())
        .spawn(move || {
            stop_deferred_supervisor(DeferredSupervisorStop(child_addr as *mut HewSupervisor));
        })
        .is_err()
    {
        if allow_sync_fallback {
            eprintln!(
                "hew: warning: failed to spawn deferred supervisor-stop thread, cleaning up synchronously"
            );
            stop_deferred_supervisor(DeferredSupervisorStop(child_sup));
        } else {
            eprintln!("hew: warning: failed to spawn deferred supervisor-stop thread");
        }
        return false;
    }
    true
}

fn spawn_owned_deferred_supervisor_stop(sup: *mut HewSupervisor) -> bool {
    if sup.is_null() {
        return true;
    }

    #[cfg(all(test, not(target_arch = "wasm32")))]
    if should_fail_owned_deferred_supervisor_spawn() {
        return false;
    }

    let sup_addr = sup as usize;
    if std::thread::Builder::new()
        .name("deferred-sup-stop".into())
        .spawn(move || {
            stop_owned_deferred_supervisor(DeferredSupervisorStop(sup_addr as *mut HewSupervisor));
        })
        .is_err()
    {
        eprintln!("hew: warning: failed to spawn deferred supervisor-stop thread");
        return false;
    }
    true
}

fn current_actor_supervisor(current: *mut HewActor) -> *mut HewSupervisor {
    if current.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `current` is the live actor currently dispatched on this thread.
    unsafe {
        if !(*current).supervisor.is_null() {
            return (*current).supervisor.cast::<HewSupervisor>();
        }
        let Some(dispatch) = (*current).dispatch else {
            return ptr::null_mut();
        };
        if std::ptr::fn_addr_eq(dispatch, supervisor_dispatch as HewDispatchFn)
            && !(*current).state.is_null()
        {
            return (*current).state.cast::<HewSupervisor>();
        }
    }
    ptr::null_mut()
}

fn current_thread_owns_supervisor_tree(sup: *mut HewSupervisor) -> bool {
    if sup.is_null() {
        return false;
    }

    let current = actor::hew_actor_self();
    let mut current_sup = current_actor_supervisor(current);
    while !current_sup.is_null() {
        if current_sup == sup {
            return true;
        }
        // SAFETY: the current actor keeps each supervisor on its ancestry chain
        // alive until this dispatch/terminate callback unwinds.
        current_sup = unsafe { (*current_sup).parent };
    }
    false
}

/// Stop a child supervisor without blocking the current scheduler worker.
fn defer_stop_child_supervisor(child_sup: *mut HewSupervisor) {
    let _ = spawn_deferred_supervisor_stop(child_sup, true);
}

#[cfg(all(test, not(target_arch = "wasm32")))]
thread_local! {
    static FAIL_OWNED_DEFERRED_SUPERVISOR_SPAWN: Cell<bool> = const { Cell::new(false) };
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn should_fail_owned_deferred_supervisor_spawn() -> bool {
    FAIL_OWNED_DEFERRED_SUPERVISOR_SPAWN.with(Cell::get)
}

fn claim_supervisor_teardown(sup: *mut HewSupervisor) -> bool {
    if sup.is_null() {
        return false;
    }
    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe {
        (&*sup)
            .teardown_claimed
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    }
}

fn release_supervisor_teardown(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    // SAFETY: only used to roll back a failed deferred-spawn attempt while the
    // supervisor is still live and owned by the caller.
    unsafe {
        (&*sup).teardown_claimed.store(false, Ordering::Release);
    }
}

fn request_supervisor_shutdown(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe {
        let s = &*sup;
        s.cancelled.store(true, Ordering::Release);
        s.running.store(0, Ordering::Release);
    }
}

fn wait_for_supervisor_self_actor_quiescent(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }

    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe {
        let s = &*sup;
        if s.self_actor.is_null() {
            return;
        }

        actor::hew_actor_stop(s.self_actor);
        loop {
            let state = (*s.self_actor).actor_state.load(Ordering::Acquire);
            if state != HewActorState::Running as i32 && state != HewActorState::Runnable as i32 {
                break;
            }
            std::thread::yield_now();
        }
    }
}

unsafe fn stop_supervisor_owned(sup: *mut HewSupervisor) {
    request_supervisor_shutdown(sup);
    wait_for_supervisor_self_actor_quiescent(sup);

    // SAFETY: teardown ownership is held exclusively by this thread and the
    // supervisor memory remains live until `Box::from_raw` below.
    let shared = unsafe { &*sup };
    while shared.pending_restart_timers.load(Ordering::Acquire) != 0 {
        std::thread::yield_now();
    }

    // SAFETY: teardown ownership was claimed once for this supervisor, the
    // self actor is no longer dispatching, and no other thread may consume the
    // raw pointer now.
    let mut s = unsafe { Box::from_raw(sup) };

    // Recursively stop all child supervisors first.
    for child_sup in std::mem::take(&mut s.child_supervisors) {
        if !child_sup.is_null() {
            // SAFETY: child_sup is a valid supervisor added via
            // hew_supervisor_add_child_supervisor.
            unsafe { hew_supervisor_stop(child_sup) };
        }
    }
    // Stop all children and wait for each to reach a terminal state.
    for i in 0..s.child_count {
        let child = take_child_slot(&mut s, i);
        if !child.is_null() {
            // SAFETY: child pointer is valid.
            unsafe { actor::hew_actor_stop(child) };
            // SAFETY: child pointer is valid.
            unsafe {
                loop {
                    let state = (*child).actor_state.load(Ordering::Acquire);
                    if state != HewActorState::Running as i32
                        && state != HewActorState::Runnable as i32
                    {
                        break;
                    }
                    std::thread::yield_now();
                }
                actor::hew_actor_free(child);
            }
        }
    }

    if !s.self_actor.is_null() {
        // SAFETY: self_actor was stopped and waited above; it is no longer
        // dispatching and now only needs final pointer cleanup and free.
        unsafe {
            (*s.self_actor).state = ptr::null_mut();
            (*s.self_actor).state_size = 0;
            actor::hew_actor_free(s.self_actor);
        }
        s.self_actor = ptr::null_mut();
    }

    // Free pool slots. Each pool was Box-allocated by hew_supervisor_pool_add_slot;
    // pool_specs Drop impl handles name deallocation.
    for pool in std::mem::take(&mut s.pool_slots) {
        if !pool.is_null() {
            // SAFETY: pool was created by Box::into_raw in hew_supervisor_pool_add_slot.
            unsafe { drop(Box::from_raw(pool)) };
        }
    }
}

/// Restart a child from its spec, returning the new actor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_count` (for
/// restarts) or equal to `child_count` (for initial spawns, where the
/// caller is responsible for pushing the result onto the `children` vec).
unsafe fn restart_child_from_spec(sup: &mut HewSupervisor, index: usize) -> *mut HewActor {
    // Copy scalar fields out before any mutable borrow of child_specs.
    let (opts, state_drop_fn) = {
        let spec = &sup.child_specs[index];
        let opts = HewActorOpts {
            init_state: spec.init_state,
            state_size: spec.init_state_size,
            dispatch: spec.dispatch,
            mailbox_capacity: spec.mailbox_capacity,
            overflow: spec.overflow,
            coalesce_key_fn: None,
            coalesce_fallback: HewOverflowPolicy::DropOld as c_int,
            budget: 0,
            arena_cap_bytes: spec.arena_cap_bytes,
        };
        (opts, spec.state_drop_fn)
    };

    // SAFETY: opts is valid.
    let new_child = unsafe { actor::hew_actor_spawn_opts(&raw const opts) };

    // Set supervisor back-pointer on the new child.
    if !new_child.is_null() {
        // SAFETY: new_child was just spawned and is valid.
        unsafe {
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "child index fits in i32 for any reasonable child count"
            )]
            {
                (*new_child).supervisor = std::ptr::from_mut::<HewSupervisor>(sup).cast::<c_void>();
                (*new_child).supervisor_child_index = index as i32;
            }
        }

        // Register the state-drop callback so restarted actors free their
        // heap-allocated fields (e.g. Vec, String) on teardown.
        if let Some(drop_fn) = state_drop_fn {
            // SAFETY: new_child is valid; drop_fn is a codegen-emitted
            // function with the correct signature.
            unsafe { actor::hew_actor_set_state_drop(new_child, drop_fn) };
        }

        // Record successful restart for circuit breaker.
        circuit_breaker_record_success(&mut sup.child_specs[index]);
    }

    // Update existing slot (restarts). For initial spawns, the caller
    // pushes the returned pointer onto the children vec.
    store_child_slot(sup, index, new_child);
    new_child
}

/// Restart a child supervisor from its stored init fn, returning the new
/// supervisor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_supervisors`.
unsafe fn restart_child_supervisor_from_spec(
    sup: &mut HewSupervisor,
    index: usize,
) -> *mut HewSupervisor {
    let Some(spec) = sup
        .child_supervisor_specs
        .get(index)
        .and_then(Option::as_ref)
    else {
        return ptr::null_mut();
    };
    let init_fn = spec.init_fn;
    let old_child = sup.child_supervisors[index];

    // SAFETY: `init_fn` was registered alongside this child supervisor.
    let new_child = unsafe { init_fn() };
    if new_child.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `new_child` and `sup` are valid pointers.
    unsafe {
        (*new_child).parent = std::ptr::from_mut::<HewSupervisor>(sup);
        (*new_child).index_in_parent = index;
        crate::shutdown::hew_shutdown_unregister_supervisor(new_child);
    }
    sup.child_supervisors[index] = new_child;

    if !old_child.is_null() && old_child != new_child {
        defer_stop_child_supervisor(old_child);
    }

    new_child
}

/// Restart children after checking the supervisor restart budget.
///
/// # Safety
///
/// `sup` must be valid.
unsafe fn restart_with_budget_and_strategy(sup: &mut HewSupervisor, failed_index: usize) {
    if failed_index >= sup.child_count {
        return;
    }

    let recent = restart_within_window(sup);
    if recent >= sup.max_restarts {
        stop_and_maybe_escalate(sup);
        return;
    }

    record_restart(sup);

    match sup.strategy {
        STRATEGY_ONE_FOR_ONE => {
            // SAFETY: index is valid.
            unsafe { restart_child_from_spec(sup, failed_index) };
        }
        STRATEGY_ONE_FOR_ALL => {
            // Stop all other children, then restart all.
            // Children are freed on a background thread to avoid deadlocking
            // when the scheduler has a single worker (hew_actor_free spin-waits
            // and would block the only worker running this dispatch).
            let mut deferred: Vec<DeferredFree> = Vec::new();
            for i in 0..sup.child_count {
                if i != failed_index {
                    let child = take_child_slot(sup, i);
                    if child.is_null() {
                        continue;
                    }
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(child) };
                    deferred.push(DeferredFree(child));
                }
            }
            if !deferred.is_empty()
                && std::thread::Builder::new()
                    .name("deferred-free".into())
                    .spawn(move || {
                        for d in deferred {
                            // SAFETY: actor was stopped; supervisor no longer references it.
                            // Use the restart-aware free path so the codegen
                            // state-drop does NOT run on field pointers that
                            // are still byte-aliased by `spec.init_state` and
                            // about to be reused by `restart_child_from_spec`.
                            // See `actor::free_actor_resources_with_options`.
                            unsafe { actor::hew_actor_free_for_restart(d.0) };
                        }
                    })
                    .is_err()
            {
                eprintln!("hew: warning: failed to spawn deferred-free thread");
            }
            for i in 0..sup.child_count {
                // SAFETY: index is valid.
                unsafe { restart_child_from_spec(sup, i) };
            }
        }
        STRATEGY_REST_FOR_ONE => {
            // Stop children after the failed one, then restart them.
            // Deferred free as in ONE_FOR_ALL to avoid single-worker deadlock.
            let mut deferred: Vec<DeferredFree> = Vec::new();
            for i in (failed_index + 1)..sup.child_count {
                let child = take_child_slot(sup, i);
                if !child.is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(child) };
                    deferred.push(DeferredFree(child));
                }
            }
            if !deferred.is_empty()
                && std::thread::Builder::new()
                    .name("deferred-free".into())
                    .spawn(move || {
                        for d in deferred {
                            // SAFETY: actor was stopped; supervisor no longer references it.
                            // Use the restart-aware free path so the codegen
                            // state-drop does NOT run on field pointers that
                            // are still byte-aliased by `spec.init_state` and
                            // about to be reused by `restart_child_from_spec`.
                            // See `actor::free_actor_resources_with_options`.
                            unsafe { actor::hew_actor_free_for_restart(d.0) };
                        }
                    })
                    .is_err()
            {
                eprintln!("hew: warning: failed to spawn deferred-free thread");
            }
            for i in failed_index..sup.child_count {
                // SAFETY: index is valid.
                unsafe { restart_child_from_spec(sup, i) };
            }
        }
        _ => {}
    }

    notify_restart(sup);
}

/// Restart an exhausted child supervisor subtree after checking the parent's
/// restart budget. Child-supervisor recovery is only available when the child
/// was registered with an init fn.
///
/// # Safety
///
/// `sup` must be valid and `failed_index` must be within `child_supervisors`.
unsafe fn restart_child_supervisor_with_budget(sup: &mut HewSupervisor, failed_index: usize) {
    if failed_index >= sup.child_supervisors.len() {
        stop_and_maybe_escalate(sup);
        return;
    }

    if sup
        .child_supervisor_specs
        .get(failed_index)
        .and_then(Option::as_ref)
        .is_none()
    {
        stop_and_maybe_escalate(sup);
        return;
    }

    let recent = restart_within_window(sup);
    if recent >= sup.max_restarts {
        stop_and_maybe_escalate(sup);
        return;
    }

    record_restart(sup);

    // SAFETY: `failed_index` is validated above and `sup` is the live parent
    // supervisor whose child-supervisor slot we are replacing.
    if unsafe { restart_child_supervisor_from_spec(sup, failed_index) }.is_null() {
        stop_and_maybe_escalate(sup);
        return;
    }

    notify_restart(sup);
}

/// Apply the restart strategy after a child failure.
///
/// # Safety
///
/// `sup` must be valid.
unsafe fn apply_restart(sup: &mut HewSupervisor, failed_index: usize, exit_state: c_int) {
    let spec = &mut sup.child_specs[failed_index];

    // Record crash if it was a crash (not a normal stop)
    if exit_state == HewActorState::Crashed as c_int {
        circuit_breaker_record_crash(spec, 11); // Default to SIGSEGV if signal not available
                                                // Apply exponential backoff delay after crash (only for subsequent crashes)
        if spec.restart_delay_ms > 0 {
            apply_restart_backoff(spec);
        }
    }

    // Check restart policy.
    if spec.restart_policy == RESTART_TEMPORARY {
        store_child_slot(sup, failed_index, ptr::null_mut());
        return;
    }
    if spec.restart_policy == RESTART_TRANSIENT && exit_state == HewActorState::Stopped as c_int {
        store_child_slot(sup, failed_index, ptr::null_mut());
        return;
    }

    // Check circuit breaker
    if !circuit_breaker_should_restart(spec) {
        store_child_slot(sup, failed_index, ptr::null_mut());
        return;
    }

    // Check restart delay — if backoff delay hasn't elapsed, schedule a
    // delayed restart by posting a system message to the supervisor's own
    // mailbox after the delay. This funnels budget accounting through the
    // single-threaded actor dispatch, avoiding a data race when multiple
    // timer threads fire concurrently.
    if !restart_delay_allows_restart(spec) {
        let delay_remaining_ns = spec
            .next_restart_time_ns
            .saturating_sub(monotonic_time_ns());
        let delay_ms = (delay_remaining_ns / 1_000_000).max(1);
        let sup_addr = std::ptr::from_mut::<HewSupervisor>(sup) as usize;
        let idx = failed_index;
        sup.pending_restart_timers.fetch_add(1, Ordering::AcqRel);
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(delay_ms));
            let sup_ptr = sup_addr as *mut HewSupervisor;
            // SAFETY: hew_supervisor_stop spin-waits on pending_restart_timers
            // before freeing the supervisor, so sup_ptr is still valid here.
            unsafe {
                let s = &*sup_ptr;
                if !s.cancelled.load(Ordering::Acquire)
                    && s.running.load(Ordering::Acquire) != 0
                    && !s.self_actor.is_null()
                {
                    let event = DelayedRestartEvent { child_index: idx };
                    let mb = (*s.self_actor).mailbox.cast::<crate::mailbox::HewMailbox>();
                    mailbox::hew_mailbox_send_sys(
                        mb,
                        SYS_MSG_DELAYED_RESTART,
                        (&raw const event).cast::<c_void>().cast_mut(),
                        std::mem::size_of::<DelayedRestartEvent>(),
                    );
                    // Wake the supervisor actor if idle.
                    let current = (*s.self_actor).actor_state.load(Ordering::Acquire);
                    if current == HewActorState::Idle as i32
                        && (*s.self_actor)
                            .actor_state
                            .compare_exchange(
                                HewActorState::Idle as i32,
                                HewActorState::Runnable as i32,
                                Ordering::AcqRel,
                                Ordering::Relaxed,
                            )
                            .is_ok()
                    {
                        scheduler::sched_enqueue(s.self_actor);
                    }
                }
                (*sup_ptr)
                    .pending_restart_timers
                    .fetch_sub(1, Ordering::AcqRel);
            }
        });
        return;
    }

    // Set the initial delay for the next restart
    if exit_state == HewActorState::Crashed as c_int && spec.restart_delay_ms == 0 {
        spec.restart_delay_ms = INITIAL_RESTART_DELAY_MS;
    }

    // SAFETY: supervisor and failed_index were validated by caller.
    unsafe { restart_with_budget_and_strategy(sup, failed_index) };
}

/// Supervisor dispatch function (handles system messages).
unsafe extern "C" fn supervisor_dispatch(
    state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    if state.is_null() {
        return;
    }
    // SAFETY: state points to a valid HewSupervisor.
    let sup = unsafe { &mut *state.cast::<HewSupervisor>() };

    if sup.running.load(Ordering::Acquire) == 0 {
        return;
    }

    match msg_type {
        SYS_MSG_CHILD_STOPPED | SYS_MSG_CHILD_CRASHED => {
            if data.is_null() || data_size < std::mem::size_of::<ChildEvent>() {
                return;
            }
            // SAFETY: data is valid for at least sizeof(ChildEvent).
            let event = unsafe { &*data.cast::<ChildEvent>() };

            // child_index == -1 signals a child supervisor escalation.
            if event.child_index < 0 {
                let Ok(idx) = usize::try_from(event.child_id) else {
                    stop_and_maybe_escalate(sup);
                    return;
                };
                // SAFETY: parent supervisor is valid for the lifetime of this dispatch.
                unsafe { restart_child_supervisor_with_budget(sup, idx) };
                return;
            }

            #[expect(clippy::cast_sign_loss, reason = "child_index is non-negative")]
            let idx = event.child_index as usize;
            if idx >= sup.child_count {
                return;
            }

            // Free the old child.
            let child = take_child_slot(sup, idx);
            if !child.is_null() {
                // SAFETY: child pointer is valid.
                unsafe { actor::hew_actor_free(child) };
            }

            // SAFETY: sup is valid.
            unsafe { apply_restart(sup, idx, event.exit_state) };
        }
        SYS_MSG_SUPERVISOR_STOP => {
            sup.cancelled.store(true, Ordering::Release);
            sup.running.store(0, Ordering::Release);
            // Stop child supervisors recursively.
            for child_sup in &sup.child_supervisors {
                if !child_sup.is_null() {
                    // SAFETY: child_sup is a valid supervisor.
                    unsafe { hew_supervisor_stop(*child_sup) };
                }
            }
            sup.child_supervisors.clear();
            for i in 0..sup.child_count {
                if !sup.children[i].is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(sup.children[i]) };
                }
            }
        }
        SYS_MSG_DELAYED_RESTART => {
            if data.is_null() || data_size < std::mem::size_of::<DelayedRestartEvent>() {
                return;
            }
            // SAFETY: data is valid for at least sizeof(DelayedRestartEvent).
            let event = unsafe { &*data.cast::<DelayedRestartEvent>() };
            let idx = event.child_index;
            if idx < sup.child_count {
                // SAFETY: sup is valid, idx is within bounds.
                unsafe { restart_with_budget_and_strategy(sup, idx) };
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Public C ABI
// ---------------------------------------------------------------------------

/// Create a new supervisor.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_new(
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,
) -> *mut HewSupervisor {
    let sup = Box::new(HewSupervisor {
        strategy,
        max_restarts,
        window_secs,
        children: Vec::with_capacity(SUP_INITIAL_CAPACITY),
        child_specs: Vec::with_capacity(SUP_INITIAL_CAPACITY),
        child_count: 0,
        child_supervisors: Vec::new(),
        child_supervisor_specs: Vec::new(),
        parent: ptr::null_mut(),
        index_in_parent: 0,
        restart_times: [0u64; MAX_RESTARTS_TRACK],
        restart_count: 0,
        restart_head: 0,
        running: AtomicI32::new(0),
        cancelled: AtomicBool::new(false),
        teardown_claimed: AtomicBool::new(false),
        pending_restart_timers: AtomicUsize::new(0),
        self_actor: ptr::null_mut(),
        children_lock: Mutex::new(()),
        restart_notify: Some(Arc::new((Mutex::new(0), Condvar::new()))),
        pool_slots: Vec::new(),
        pool_specs: Vec::new(),
    });
    Box::into_raw(sup)
}

/// Add a child via a child spec.
///
/// The supervisor deep-copies `init_state` and `name` from the spec.
/// The caller retains ownership of the original spec and its fields
/// (including `init_state`) and must free them independently.
/// The supervisor frees its internal copies when
/// [`hew_supervisor_stop`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `spec` must be a valid pointer to a [`HewChildSpec`].
/// - `spec.init_state` must be valid for `spec.init_state_size` bytes
///   (or null when `init_state_size` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_spec(
    sup: *mut HewSupervisor,
    spec: *const HewChildSpec,
) -> c_int {
    cabi_guard!(sup.is_null() || spec.is_null(), -1);
    // SAFETY: caller guarantees both pointers are valid.
    let s = unsafe { &mut *sup };
    // SAFETY: caller guarantees `spec` is a valid, aligned, initialized `HewChildSpec` pointer.
    let sp = unsafe { &*spec };

    let i = s.child_count;

    // Deep-copy init state.
    let state_copy = if sp.init_state_size > 0 && !sp.init_state.is_null() {
        // SAFETY: init_state is valid for init_state_size bytes.
        let buf = unsafe { libc::malloc(sp.init_state_size) };
        if buf.is_null() {
            return -1;
        }
        // SAFETY: both pointers are valid.
        unsafe {
            ptr::copy_nonoverlapping(
                sp.init_state.cast::<u8>(),
                buf.cast::<u8>(),
                sp.init_state_size,
            );
        };
        buf
    } else {
        ptr::null_mut()
    };

    // Deep-copy name.
    let name_copy = if sp.name.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees name is a valid C string.
        unsafe { libc::strdup(sp.name) }
    };

    s.child_specs.push(InternalChildSpec {
        name: name_copy,
        init_state: state_copy,
        init_state_size: sp.init_state_size,
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        // Registered by hew_supervisor_set_child_state_drop after this call.
        state_drop_fn: None,
    });

    // Spawn the child actor.
    // SAFETY: spec is valid.
    let spawned = unsafe { restart_child_from_spec(s, i) };
    push_child_slot(s, spawned);
    s.child_count += 1;
    0
}

/// Start the supervisor (create its own actor).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_start(sup: *mut HewSupervisor) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };

    s.running.store(1, Ordering::Release);

    // Create the supervisor's own actor. We pass a dummy state (the sup
    // pointer itself) and override it after spawn.
    // SAFETY: spawning with the supervisor dispatch function.
    let self_actor = unsafe {
        actor::hew_actor_spawn(
            sup.cast::<HewSupervisor>().cast::<c_void>(),
            std::mem::size_of::<HewSupervisor>(),
            Some(supervisor_dispatch),
        )
    };
    if self_actor.is_null() {
        s.running.store(0, Ordering::Release);
        return -1;
    }

    // Override the actor's state to point to our supervisor struct directly
    // (not a deep copy — we need the supervisor to receive updates).
    // SAFETY: self_actor is valid; free the deep copy.
    unsafe {
        if !(*self_actor).state.is_null() {
            libc::free((*self_actor).state);
        }
        (*self_actor).state = sup.cast::<c_void>();
        (*self_actor).state_size = 0; // mark as non-owned
    }

    s.self_actor = self_actor;

    // Auto-register top-level supervisors for graceful shutdown so they
    // are cleaned up even if the generated code omits an explicit stop.
    if s.parent.is_null() {
        // SAFETY: sup is valid and will remain valid until shutdown.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    }

    0
}

/// Notify the supervisor that a child has stopped or crashed.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - The supervisor must have been started with [`hew_supervisor_start`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_notify_child_event(
    sup: *mut HewSupervisor,
    child_index: c_int,
    child_id: u64,
    exit_state: c_int,
) {
    cabi_guard!(sup.is_null());
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    if s.self_actor.is_null() {
        return;
    }

    let event = ChildEvent {
        child_index,
        child_id,
        exit_state,
    };

    let msg_type = if exit_state == HewActorState::Crashed as c_int {
        SYS_MSG_CHILD_CRASHED
    } else {
        SYS_MSG_CHILD_STOPPED
    };

    // SAFETY: self_actor is valid, mailbox is valid.
    unsafe {
        let mb = (*s.self_actor).mailbox.cast::<crate::mailbox::HewMailbox>();
        mailbox::hew_mailbox_send_sys(
            mb,
            msg_type,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildEvent>(),
        );
    }

    // Wake up the supervisor actor.
    // SAFETY: self_actor is valid.
    unsafe {
        let current = (*s.self_actor).actor_state.load(Ordering::Acquire);
        if current == HewActorState::Idle as i32
            && (*s.self_actor)
                .actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Relaxed,
                )
                .is_ok()
        {
            scheduler::sched_enqueue(s.self_actor);
        }
    }
}

/// Stop the supervisor and all its children.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`]. The
/// pointer must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_stop(sup: *mut HewSupervisor) {
    cabi_guard!(sup.is_null());

    if !claim_supervisor_teardown(sup) {
        return;
    }

    request_supervisor_shutdown(sup);

    if current_thread_owns_supervisor_tree(sup) {
        if !spawn_owned_deferred_supervisor_stop(sup) {
            release_supervisor_teardown(sup);
            set_last_error("hew_supervisor_stop: failed to spawn deferred stop thread");
            return;
        }
        // Unregister from shutdown once the deferred owner is guaranteed to
        // finish the teardown.
        // SAFETY: `sup` is still live here and was previously registered as a
        // top-level supervisor when started.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
        return;
    }

    // Unregister from shutdown before consuming the raw pointer so later
    // shutdown sweeps cannot race this exact-once teardown owner.
    // SAFETY: `sup` is still live here and was previously registered as a
    // top-level supervisor when started.
    unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
    // SAFETY: teardown ownership was claimed above and remains unique.
    unsafe { stop_supervisor_owned(sup) };
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    struct OwnedDeferredSupervisorSpawnFailureGuard;

    impl Drop for OwnedDeferredSupervisorSpawnFailureGuard {
        fn drop(&mut self) {
            FAIL_OWNED_DEFERRED_SUPERVISOR_SPAWN.with(|slot| slot.set(false));
        }
    }

    fn fail_owned_deferred_supervisor_spawn() -> OwnedDeferredSupervisorSpawnFailureGuard {
        FAIL_OWNED_DEFERRED_SUPERVISOR_SPAWN.with(|slot| slot.set(true));
        OwnedDeferredSupervisorSpawnFailureGuard
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
            // SAFETY: the test keeps the actor allocation alive until this
            // state transition runs.
            unsafe {
                (*(actor_addr as *mut HewActor))
                    .actor_state
                    .store(target_state as i32, Ordering::Release);
            }
        })
    }

    unsafe extern "C" fn noop_child_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
    }

    unsafe fn make_supervisor_with_child() -> (*mut HewSupervisor, *mut HewActor, *mut HewActor) {
        // SAFETY: this helper creates a fresh supervisor tree for the test and
        // returns the owned raw pointers without publishing them elsewhere.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                arena_cap_bytes: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            assert_eq!(hew_supervisor_start(sup), 0);

            let child = (&(*sup).children)[0];
            let self_actor = (*sup).self_actor;
            (sup, child, self_actor)
        }
    }

    #[test]
    fn stop_supervisor_from_child_dispatch_is_deferred() {
        // SAFETY: this test owns the supervisor tree and only mutates the
        // current actor/thread-local state within the test thread.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            (*child)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);

            let prev_actor = actor::set_current_actor(child);
            let unblock = defer_state_transition(
                child,
                HewActorState::Stopped,
                std::time::Duration::from_millis(200),
            );

            let start = std::time::Instant::now();
            hew_supervisor_stop(sup);
            let elapsed = start.elapsed();

            unblock.join().unwrap();
            actor::set_current_actor(prev_actor);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || !actor::is_actor_live(
                    child
                )),
                "child actor should be freed asynchronously after deferred supervisor stop"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live(self_actor)
                }),
                "supervisor self actor should be freed asynchronously after deferred stop"
            );

            assert!(
                elapsed < std::time::Duration::from_millis(100),
                "child-owned supervisor stop should return immediately instead of waiting for the current dispatch thread, took {elapsed:?}"
            );
        }
    }

    #[test]
    fn stop_supervisor_from_child_terminate_is_deferred() {
        // SAFETY: this test owns the supervisor tree and simulates a reentrant
        // terminate callback by controlling the child actor state directly.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            let prev_actor = actor::set_current_actor(child);
            let start = std::time::Instant::now();
            hew_supervisor_stop(sup);
            let elapsed = start.elapsed();

            child_ref.terminate_finished.store(true, Ordering::Release);
            actor::set_current_actor(prev_actor);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || !actor::is_actor_live(
                    child
                )),
                "child should be released after deferred supervisor stop"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live(self_actor)
                }),
                "supervisor self actor should be released after deferred stop"
            );

            assert!(
                elapsed < std::time::Duration::from_secs(1),
                "reentrant supervisor stop should defer instead of spinning inside terminate, took {elapsed:?}"
            );
        }
    }

    #[test]
    fn concurrent_second_stop_returns_while_deferred_owner_waits() {
        // SAFETY: this test owns the supervisor tree, injects a synthetic
        // current actor for the owner-thread path, and only mutates actor
        // states through test-controlled atomics.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!child_sup.is_null());
            assert_eq!(hew_supervisor_add_child_supervisor(sup, child_sup), 0);

            (*child)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);
            (*self_actor)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);

            let prev_actor = actor::set_current_actor(child);
            let self_unblock = defer_state_transition(
                self_actor,
                HewActorState::Stopped,
                std::time::Duration::from_millis(200),
            );
            let child_unblock = defer_state_transition(
                child,
                HewActorState::Stopped,
                std::time::Duration::from_millis(250),
            );

            hew_supervisor_stop(sup);

            let finished = std::sync::Arc::new(AtomicBool::new(false));
            let elapsed_ms = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0));
            let finished_clone = std::sync::Arc::clone(&finished);
            let elapsed_clone = std::sync::Arc::clone(&elapsed_ms);
            let sup_addr = sup as usize;
            let second = std::thread::spawn(move || {
                let start = std::time::Instant::now();
                hew_supervisor_stop(sup_addr as *mut HewSupervisor);
                let elapsed = u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX);
                elapsed_clone.store(elapsed, Ordering::Release);
                finished_clone.store(true, Ordering::Release);
            });

            assert!(
                wait_for_condition(std::time::Duration::from_millis(100), || {
                    finished.load(Ordering::Acquire)
                }),
                "second stop caller should return while deferred teardown owns the supervisor"
            );
            assert!(
                elapsed_ms.load(Ordering::Acquire) < 100,
                "second stop caller should not race into teardown ownership"
            );
            assert_eq!(
                (*sup).child_supervisors.len(),
                1,
                "deferred teardown must not mutate child supervisor vectors before self actor quiesces"
            );

            second.join().unwrap();
            self_unblock.join().unwrap();
            child_unblock.join().unwrap();
            actor::set_current_actor(prev_actor);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live(child)
                }),
                "child actor should still be released after the deferred winner completes"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live(self_actor)
                }),
                "supervisor self actor should still be released after the deferred winner completes"
            );
        }
    }

    #[test]
    fn failed_deferred_spawn_keeps_supervisor_registered() {
        // SAFETY: this test owns the supervisor tree, injects the current actor
        // to exercise the owner-thread path, and then performs synchronous
        // cleanup once the injected spawn failure has been asserted.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let fail_guard = fail_owned_deferred_supervisor_spawn();

            let prev_actor = actor::set_current_actor(child);
            crate::hew_clear_error();
            hew_supervisor_stop(sup);
            actor::set_current_actor(prev_actor);

            assert!(
                crate::shutdown::is_supervisor_registered_for_test(sup),
                "failed deferred spawn must not orphan the top-level supervisor from shutdown tracking"
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null(), "spawn failure should surface an error");
            let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
            assert!(
                msg.contains("failed to spawn deferred stop thread"),
                "spawn failure should preserve the stop error, got: {msg}"
            );

            drop(fail_guard);
            hew_supervisor_stop(sup);
        }
    }

    // ---------------------------------------------------------------------------
    // Tests for hew_supervisor_child_get and hew_supervisor_nested_get
    // ---------------------------------------------------------------------------

    /// A running child returns Live with its actor pointer.
    #[test]
    fn child_get_live_returns_handle() {
        // SAFETY: test owns the supervisor tree; cleans up after assertions.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 0, "expected Live (tag=0)");
            assert_eq!(result.reason, ChildSlotReason::Ok as u8);
            assert_eq!(result.handle, child);

            hew_supervisor_stop(sup);
        }
    }

    /// An out-of-range key returns Dead(UnknownSlot).
    #[test]
    fn child_get_unknown_key_returns_dead_unknown_slot() {
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();

            // Key 1 is out of range (only key 0 is declared).
            let result = hew_supervisor_child_get(sup, 1);
            assert_eq!(result.tag, 2, "expected Dead (tag=2)");
            assert_eq!(result.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(result.handle.is_null());

            hew_supervisor_stop(sup);
        }
    }

    /// A null supervisor pointer returns Dead(SupervisorShutdown).
    #[test]
    fn child_get_null_sup_returns_dead_supervisor_shutdown() {
        // SAFETY: null pointer is the input we are testing; the function must
        // handle it gracefully and return Dead(SupervisorShutdown) without UB.
        let result = unsafe { hew_supervisor_child_get(ptr::null_mut(), 0) };
        assert_eq!(result.tag, 2, "expected Dead (tag=2)");
        assert_eq!(result.reason, ChildSlotReason::SupervisorShutdown as u8);
        assert!(result.handle.is_null());
    }

    /// After `hew_supervisor_stop`, the supervisor has `running == 0` and
    /// subsequent lookups return Dead(SupervisorShutdown).
    #[test]
    fn child_get_stopped_supervisor_returns_dead_supervisor_shutdown() {
        // SAFETY: test owns the supervisor tree; stop is called before the
        // pointer is last used.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();

            // Force running to 0 directly so we can query without spawning threads.
            (*sup).running.store(0, Ordering::Release);

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 2, "expected Dead (tag=2)");
            assert_eq!(result.reason, ChildSlotReason::SupervisorShutdown as u8);
            assert!(result.handle.is_null());

            // Restore to allow normal stop.
            (*sup).running.store(1, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// While the slot is null (simulating mid-restart), the lookup returns
    /// Transient(Restarting).
    #[test]
    fn child_get_null_slot_returns_transient_restarting() {
        // SAFETY: test owns the supervisor tree; we manually null the slot to
        // simulate the restart-in-progress window, then restore it.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            // Simulate the restart-in-progress window: null the slot under lock.
            store_child_slot(&mut *sup, 0, ptr::null_mut());

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::Restarting as u8);
            assert!(result.handle.is_null());

            // Restore the slot so teardown can reach the actor.
            store_child_slot(&mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// When the circuit breaker is OPEN (state == 1), a null slot returns
    /// Transient(CircuitOpen).
    #[test]
    fn child_get_circuit_open_null_slot_returns_transient_circuit_open() {
        // SAFETY: test owns the supervisor tree; we manually set state fields.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            // Null the slot and open the circuit breaker.
            store_child_slot(&mut *sup, 0, ptr::null_mut());
            (&mut (*sup).child_specs)[0].circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::CircuitOpen as u8);
            assert!(result.handle.is_null());

            // Restore before teardown.
            (&mut (*sup).child_specs)[0].circuit_breaker.state = 0; // HEW_CIRCUIT_BREAKER_CLOSED
            store_child_slot(&mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// When `next_restart_time_ns` is in the future (backoff window active),
    /// a null slot returns Transient(BackoffDelay).
    #[test]
    fn child_get_backoff_active_null_slot_returns_transient_backoff_delay() {
        // SAFETY: test owns the supervisor tree; we manually set next_restart_time_ns.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            // Null the slot and set the backoff deadline far in the future.
            store_child_slot(&mut *sup, 0, ptr::null_mut());
            // 1 hour from now in nanoseconds
            (&mut (*sup).child_specs)[0].next_restart_time_ns =
                monotonic_time_ns().saturating_add(3_600_000_000_000);

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::BackoffDelay as u8);
            assert!(result.handle.is_null());

            // Restore before teardown.
            (&mut (*sup).child_specs)[0].next_restart_time_ns = 0;
            store_child_slot(&mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// Verify `ChildLookupResult` is 16 bytes and has the expected field layout.
    #[test]
    fn child_lookup_result_size_and_layout() {
        use std::mem;
        assert_eq!(
            mem::size_of::<ChildLookupResult>(),
            16,
            "ChildLookupResult must be 16 bytes for C ABI compatibility"
        );
        assert_eq!(
            mem::align_of::<ChildLookupResult>(),
            mem::align_of::<*mut HewActor>(),
            "ChildLookupResult must align to pointer size"
        );
    }

    /// A non-null child supervisor returns Live with the bit-cast pointer.
    #[test]
    fn nested_get_live_returns_handle() {
        // SAFETY: test owns both supervisor trees; cleans up after assertions.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let child_sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!child_sup.is_null());
            assert_eq!(hew_supervisor_add_child_supervisor(sup, child_sup), 0);

            let result = hew_supervisor_nested_get(sup, 0);
            assert_eq!(result.tag, 0, "expected Live (tag=0)");
            assert_eq!(result.reason, ChildSlotReason::Ok as u8);
            // The handle carries the *mut HewSupervisor bit-pattern.
            assert_eq!(result.handle, child_sup.cast::<HewActor>());

            hew_supervisor_stop(sup);
        }
    }

    /// A key beyond `child_supervisors.len()` returns `Dead(UnknownSlot)`.
    #[test]
    fn nested_get_unknown_key_returns_dead_unknown_slot() {
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            // No nested supervisors added; key 0 is out of range.
            let result = hew_supervisor_nested_get(sup, 0);
            assert_eq!(result.tag, 2, "expected Dead (tag=2)");
            assert_eq!(result.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(result.handle.is_null());

            hew_supervisor_stop(sup);
        }
    }
}

/// Free a supervisor struct without stopping actors or spin-waiting.
///
/// Used during post-shutdown cleanup when worker threads are already
/// joined. Nulls the `self_actor`'s state pointer to prevent a double-free
/// in [`crate::actor::cleanup_all_actors`], then drops the Box to free
/// child spec resources via their Drop impls.
///
/// # Safety
///
/// `sup` must be a valid, non-null pointer to a `HewSupervisor`.
/// Worker threads must have been joined before calling.
pub(crate) unsafe fn free_supervisor_resources(sup: *mut HewSupervisor) {
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };
    s.cancelled.store(true, Ordering::Release);
    while s.pending_restart_timers.load(Ordering::Acquire) != 0 {
        std::thread::yield_now();
    }
    if !s.self_actor.is_null() {
        // Null out state so cleanup_all_actors won't libc::free it
        // (state points to the supervisor Box, not malloc'd memory).
        // SAFETY: self_actor is non-null (checked above) and valid for the supervisor's lifetime.
        unsafe {
            (*s.self_actor).state = ptr::null_mut();
            (*s.self_actor).state_size = 0;
        }
    }

    // Recursively free child supervisors.
    for child_sup in &s.child_supervisors {
        if !child_sup.is_null() {
            // SAFETY: child_sup is non-null (checked above) and was allocated by us.
            unsafe { free_supervisor_resources(*child_sup) };
        }
    }
    // Drop the Box — child spec Drop impls free names + init_state.
    // SAFETY: sup was allocated with Box::into_raw and is valid per caller contract.
    drop(unsafe { Box::from_raw(sup) });
}

/// Handle a crashed child actor by applying the supervisor's restart strategy.
///
/// This is a convenience entry point that can be called directly (e.g. from
/// `hew_actor_trap`) instead of going through the system-message path.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must be a valid pointer to a [`HewActor`] that belongs to `sup`.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_handle_crash(
    sup: *mut HewSupervisor,
    child: *mut HewActor,
) {
    cabi_guard!(sup.is_null() || child.is_null());
    // SAFETY: caller guarantees both pointers are valid.
    let s = unsafe { &*sup };
    // SAFETY: caller guarantees `child` is a valid HewActor pointer.
    let child_ref = unsafe { &*child };

    // Find the child index in the supervisor's children array.
    let idx = child_ref.supervisor_child_index;
    if idx < 0 {
        return;
    }

    #[expect(clippy::cast_sign_loss, reason = "guarded by idx >= 0 check above")]
    let index = idx as usize;
    if index >= s.child_count {
        return;
    }

    let exit_state = child_ref.actor_state.load(Ordering::Acquire);

    // Notify the supervisor actor via the event system.
    // SAFETY: sup is valid and child_id / exit_state are read from valid memory.
    unsafe {
        hew_supervisor_notify_child_event(sup, idx, child_ref.id, exit_state);
    }
}

/// Register a child supervisor under a parent supervisor.
///
/// The parent will recursively stop the child supervisor when the parent is
/// stopped, and the child supervisor's crash (restart budget exhausted)
/// propagates to the parent.
///
/// # Safety
///
/// - `parent` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must not already be registered as a child of another supervisor
///   (no cycles).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_supervisor(
    parent: *mut HewSupervisor,
    child: *mut HewSupervisor,
) -> c_int {
    if parent.is_null() || child.is_null() || parent == child {
        return -1;
    }
    // SAFETY: caller guarantees parent is valid.
    let p = unsafe { &mut *parent };
    let idx = p.child_supervisors.len();
    p.child_supervisors.push(child);
    p.child_supervisor_specs.push(None);
    // Set parent back-pointer on the child supervisor.
    // SAFETY: caller guarantees child is valid.
    unsafe {
        (*child).parent = parent;
        (*child).index_in_parent = idx;
        // Unregister from top-level list (was registered in
        // hew_supervisor_start when parent was still null).
        crate::shutdown::hew_shutdown_unregister_supervisor(child);
    };
    0
}

/// Register a child supervisor with an init function for restartability.
///
/// When the child supervisor's restart budget is exhausted and it escalates,
/// the parent can restart the entire subtree by calling `init_fn`.
///
/// # Safety
///
/// - `parent` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `init_fn` must be a valid function pointer that returns a new supervisor.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_supervisor_with_init(
    parent: *mut HewSupervisor,
    child: *mut HewSupervisor,
    init_fn: SupervisorInitFn,
) -> c_int {
    if parent.is_null() || child.is_null() || parent == child {
        return -1;
    }
    // SAFETY: caller guarantees parent and child are valid.
    let p = unsafe { &mut *parent };
    let idx = p.child_supervisors.len();
    p.child_supervisors.push(child);
    p.child_supervisor_specs
        .push(Some(SupervisorChildSpec { init_fn }));
    // SAFETY: child and parent are valid pointers per caller contract.
    unsafe {
        (*child).parent = parent;
        (*child).index_in_parent = idx;
        // The child was auto-registered as a top-level supervisor in
        // hew_supervisor_start (parent was null at that point). Now that
        // it has a parent, unregister it so only the true root is stopped.
        crate::shutdown::hew_shutdown_unregister_supervisor(child);
    };
    0
}

/// Return the child supervisor pointer at `index`, or null if out of range.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child_supervisor(
    sup: *mut HewSupervisor,
    index: c_int,
) -> *mut HewSupervisor {
    if sup.is_null() || index < 0 {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    if i >= s.child_supervisors.len() {
        return ptr::null_mut();
    }
    s.child_supervisors[i]
}

/// Return the child actor pointer at `index`, or null if out of range.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child(
    sup: *mut HewSupervisor,
    index: c_int,
) -> *mut HewActor {
    if sup.is_null() || index < 0 {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    if i >= s.child_count {
        return ptr::null_mut();
    }
    load_child_slot(s, i)
}

/// Return the child actor pointer at `index`, waiting up to `timeout_ms`
/// for the child to become available if it's currently being restarted.
///
/// Returns null if the child is still unavailable after the timeout, or if
/// the supervisor has been cancelled.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child_wait(
    sup: *mut HewSupervisor,
    index: c_int,
    timeout_ms: i32,
) -> *mut HewActor {
    if sup.is_null() || index < 0 {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    if i >= s.child_count {
        return ptr::null_mut();
    }

    let pair = match s.restart_notify {
        Some(ref p) => Arc::clone(p),
        None => return ptr::null_mut(),
    };

    // Fast path: child is already available.
    let child = load_child_slot(s, i);
    if !child.is_null() {
        return child;
    }

    // Slow path: child is being restarted. Wait on the restart condvar
    // instead of polling the slot without synchronization.
    #[expect(
        clippy::cast_sign_loss,
        reason = "timeout_ms is clamped to >= 0 by max(0)"
    )]
    let deadline =
        std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms.max(0) as u64);
    let mut guard = pair.0.lock_or_recover();
    loop {
        let child = load_child_slot(s, i);
        if !child.is_null() {
            return child;
        }
        // If the supervisor was cancelled, don't wait forever.
        if s.cancelled.load(Ordering::Acquire) {
            return ptr::null_mut();
        }
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return ptr::null_mut();
        }
        let (new_guard, wait_result) = pair.1.wait_timeout_or_recover(guard, remaining);
        guard = new_guard;
        if wait_result.timed_out() {
            return load_child_slot(s, i);
        }
    }
}

/// Return the total number of children (actors + child supervisors).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "child counts fit in c_int for any reasonable supervisor"
)]
pub unsafe extern "C" fn hew_supervisor_child_count(sup: *mut HewSupervisor) -> c_int {
    if sup.is_null() {
        set_last_error("hew_supervisor_child_count: supervisor is null");
        return -1;
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    (s.child_count + s.child_supervisors.len()) as c_int
}

/// Look up a static child by its compile-time-assigned slot index.
///
/// Non-blocking. Acquires `children_lock` briefly to read the slot pointer
/// and discriminator fields atomically, then releases it and returns a
/// [`ChildLookupResult`] reflecting the slot state at observation time.
///
/// Discrimination logic (in priority order):
///
/// 1. Null or invalid `sup` → `Dead(SupervisorShutdown)`.
/// 2. `cancelled || running == 0` → `Dead(SupervisorShutdown)`.
/// 3. `key >= child_count` → `Dead(UnknownSlot)` (codegen bug; fail closed).
/// 4. Slot is non-null → `Live(handle)`.
/// 5. Slot is null, `circuit_breaker.state == OPEN` → `Transient(CircuitOpen)`.
/// 6. Slot is null, `next_restart_time_ns > now` → `Transient(BackoffDelay)`.
/// 7. Slot is null, otherwise → `Transient(Restarting)`.
///
/// `BudgetExhausted` is returned only when `running == 0` has not yet
/// propagated — in practice the supervisor sets `running = 0` in the same
/// call that exhausts the budget, so callers see `SupervisorShutdown`.
/// The variant is retained in [`ChildSlotReason`] for ABI stability when
/// per-child budget tracking is added in a future release.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`] (or by a
/// nested-supervisor lookup). Behaviour is undefined if `sup` has been freed.
///
/// # C ABI
///
/// This function is part of the Hew v0.5 static-child lookup surface.
/// It is added to the MIR runtime-ABI allowlist in `hew-mir/src/runtime_symbols.rs`.
/// The MIR `CallRuntimeAbi` producer for dotted-access lowering is deferred
/// until the `Instr::CallRuntimeAbi` emitter shape is established.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_child_get(
    sup: *mut HewSupervisor,
    key: u32,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };

    // Fast-path: supervisor-level shutdown check (no lock required — atomics).
    if s.cancelled.load(Ordering::Acquire) || s.running.load(Ordering::Acquire) == 0 {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = key as usize;
    if i >= s.child_count {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // Critical section: read the slot pointer AND the per-slot discriminator
    // fields under the same lock so the (pointer, CB-state, backoff-timer)
    // triple is consistent with one lifecycle state from the FSM in §2.2.
    //
    // v0.5 single-threaded scheduler: the mutex provides exclusion against
    // the restart machinery (store_child_slot / restart_child_from_spec).
    // v0.6 SMP path: migrate to AtomicPtr<HewActor> + atomic discriminator
    // fields so readers can avoid the mutex on the common Live path.
    let _guard = s.children_lock.lock_or_recover();

    // Re-check shutdown under the lock (the supervisor could have been
    // cancelled or run out of budget between the atomic check above and
    // acquiring the lock).
    if s.cancelled.load(Ordering::Acquire) || s.running.load(Ordering::Acquire) == 0 {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let slot = s.children.get(i).copied().unwrap_or(ptr::null_mut());
    if !slot.is_null() {
        return ChildLookupResult::live(slot);
    }

    // Slot is null — classify why using the per-child spec.
    // child_specs is parallel to children and has the same length after
    // hew_supervisor_start, so the index is always valid here.
    let spec = &s.child_specs[i];

    // CB OPEN = circuit breaker is suppressing restarts during cooldown.
    // Value 1 = HEW_CIRCUIT_BREAKER_OPEN (from hew_supervisor_set_circuit_breaker).
    if spec.circuit_breaker.state == 1 {
        return ChildLookupResult::transient(ChildSlotReason::CircuitOpen);
    }

    // Backoff delay: next_restart_time_ns is a monotonic nanosecond deadline
    // set by restart_child_from_spec when exponential backoff is configured.
    // A non-zero value > now means the timer hasn't fired yet.
    let now_ns = monotonic_time_ns();
    if spec.next_restart_time_ns > 0 && spec.next_restart_time_ns > now_ns {
        return ChildLookupResult::transient(ChildSlotReason::BackoffDelay);
    }

    // Default transient: slot is null, no CB suppression, no pending backoff —
    // the restart machinery is actively spawning the replacement actor.
    ChildLookupResult::transient(ChildSlotReason::Restarting)
}

/// Look up a nested child supervisor by its compile-time-assigned slot index.
///
/// Used for traversing supervision trees one dot segment at a time:
/// `app.api.auth` calls this for `.api` (returning `*mut HewSupervisor`
/// cast as `handle`), then [`hew_supervisor_child_get`] for `.auth`.
///
/// The returned `handle` field carries a `*mut HewSupervisor` bit-pattern.
/// The compile-time type at the call site disambiguates — codegen reinterprets
/// the pointer without an additional tag because the checker has already typed
/// the dot segment as a supervisor child.
///
/// Discrimination: same FSM as [`hew_supervisor_child_get`], but over
/// `child_supervisors` and `child_supervisor_specs`. A null supervisor slot
/// (child supervisor being restarted) returns `Transient(Restarting)`;
/// an out-of-range `key` returns `Dead(UnknownSlot)`.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`] (or by a
/// prior nested lookup). Behaviour is undefined if `sup` has been freed.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_nested_get(
    sup: *mut HewSupervisor,
    key: u32,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };

    if s.cancelled.load(Ordering::Acquire) || s.running.load(Ordering::Acquire) == 0 {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = key as usize;
    if i >= s.child_supervisors.len() {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // child_supervisors is not protected by children_lock (it uses its own
    // replacement pattern via child_supervisor_specs). Read it without the
    // lock — the pointer is set during supervisor construction or restart
    // under single-threaded scheduling. This is safe for v0.5; the SMP
    // migration note from hew_supervisor_child_get applies here too.
    let child_sup = s.child_supervisors[i];
    if !child_sup.is_null() {
        // Reinterpret the supervisor pointer as HewActor* for the shared
        // result struct. Codegen reconstructs the *mut HewSupervisor at the
        // typed call site. The cast is a bit-pattern reinterpretation only;
        // neither type is read through at this point.
        // SAFETY: cast is a pointer-size-preserving reinterpretation; the
        // MIR call site at the dotted-access lowering casts back to
        // *mut HewSupervisor before dereferencing.
        return ChildLookupResult::live(child_sup.cast::<HewActor>());
    }

    // Null slot — child supervisor is being restarted or was never started.
    ChildLookupResult::transient(ChildSlotReason::Restarting)
}

/// Return whether the supervisor is still running (1) or stopped (0).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_is_running(sup: *mut HewSupervisor) -> c_int {
    cabi_guard!(sup.is_null(), 0);
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    s.running.load(Ordering::Acquire)
}

/// Configure circuit breaker settings for a child.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// `child_index` must be within the range of added children.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_circuit_breaker(
    sup: *mut HewSupervisor,
    child_index: c_int,
    max_crashes: u32,
    window_secs: u32,
    cooldown_secs: u32,
) -> c_int {
    if sup.is_null() || child_index < 0 {
        return -1;
    }

    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let index = child_index as usize;

    if index >= s.child_count {
        return -1;
    }

    let spec = &mut s.child_specs[index];
    spec.circuit_breaker.max_crashes = max_crashes;
    spec.circuit_breaker.window_secs = window_secs;
    spec.circuit_breaker.cooldown_secs = cooldown_secs;

    0
}

/// Get the current circuit breaker state for a child.
///
/// Returns 0 for CLOSED, 1 for OPEN, 2 for `HALF_OPEN`, -1 for error.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// `child_index` must be within the range of added children.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child_circuit_state(
    sup: *mut HewSupervisor,
    child_index: c_int,
) -> c_int {
    if sup.is_null() || child_index < 0 {
        return -1;
    }

    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let index = child_index as usize;

    if index >= s.child_count {
        return -1;
    }

    s.child_specs[index].circuit_breaker.state
}

// ---------------------------------------------------------------------------
// Dynamic Supervision — Add/Remove Children at Runtime
// ---------------------------------------------------------------------------

/// Dynamically add a child by spec while the supervisor is running.
///
/// Unlike [`hew_supervisor_add_child_spec`], this function can be called
/// at any time — before or after [`hew_supervisor_start`].
///
/// Returns the child index (≥ 0) on success, -1 on error.
///
/// **State-drop registration**: this function does not accept a `state_drop_fn`
/// parameter. If the child actor type has owned heap fields, the caller must
/// invoke [`hew_supervisor_set_child_state_drop`] immediately after this
/// call returns — before any other thread can crash and restart the child:
///
/// ```text
/// let idx = hew_supervisor_add_child_dynamic(sup, spec);
/// if idx >= 0 {
///     hew_supervisor_set_child_state_drop(sup, idx, my_state_drop);
/// }
/// ```
///
/// If the supervisor is already running (`hew_supervisor_start` has been
/// called), the child is spawned immediately inside this call.  A crash
/// between the return of this function and the `set_child_state_drop` call
/// will restart the child without the drop callback, leaking any owned fields
/// in the original actor's state.  For most use-cases this window is
/// acceptable; the restart callback is wired before the child processes its
/// first message.  Callers that cannot tolerate any window should stop the
/// supervisor, add the child, register the drop, then restart.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `spec` must be a valid pointer to a [`HewChildSpec`].
/// - `spec.init_state` must be valid for `spec.init_state_size` bytes
///   (or null when `init_state_size` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_dynamic(
    sup: *mut HewSupervisor,
    spec: *const HewChildSpec,
) -> c_int {
    cabi_guard!(sup.is_null() || spec.is_null(), -1);
    // SAFETY: caller guarantees both pointers are valid.
    let s = unsafe { &mut *sup };
    // SAFETY: caller guarantees `spec` is valid.
    let sp = unsafe { &*spec };

    // Deep-copy init state.
    let state_copy = if sp.init_state_size > 0 && !sp.init_state.is_null() {
        // SAFETY: init_state is valid for init_state_size bytes.
        let buf = unsafe { libc::malloc(sp.init_state_size) };
        if buf.is_null() {
            return -1;
        }
        // SAFETY: both pointers are valid.
        unsafe {
            ptr::copy_nonoverlapping(
                sp.init_state.cast::<u8>(),
                buf.cast::<u8>(),
                sp.init_state_size,
            );
        };
        buf
    } else {
        ptr::null_mut()
    };

    // Deep-copy name.
    let name_copy = if sp.name.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees name is a valid C string.
        unsafe { libc::strdup(sp.name) }
    };

    let i = s.child_count;

    s.child_specs.push(InternalChildSpec {
        name: name_copy,
        init_state: state_copy,
        init_state_size: sp.init_state_size,
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        // Registered by the caller via hew_supervisor_set_child_state_drop
        // immediately after this call returns. See the function doc comment
        // for the race-window analysis and calling contract.
        state_drop_fn: None,
    });

    // Spawn the child if the supervisor is running.
    let spawned = if s.running.load(Ordering::Acquire) != 0 {
        // SAFETY: spec is valid.
        unsafe { restart_child_from_spec(s, i) }
    } else {
        ptr::null_mut()
    };
    push_child_slot(s, spawned);
    s.child_count += 1;

    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "child index fits in c_int for any reasonable supervisor"
    )]
    {
        i as c_int
    }
}

/// Remove a child from the supervisor by index.
///
/// Stops the child actor and removes it from the supervisor's child list.
/// Returns 0 on success, -1 on error.
///
/// Note: This performs a swap-remove. The child at `child_index` is swapped
/// with the last child, so the order of remaining children may change.
/// The removed child's actor is stopped and freed.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_remove_child(
    sup: *mut HewSupervisor,
    child_index: c_int,
) -> c_int {
    if sup.is_null() || child_index < 0 {
        return -1;
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    if idx >= s.child_count {
        return -1;
    }

    // Stop and free the child actor.
    let child = s.children[idx];
    if !child.is_null() {
        // SAFETY: child pointer is valid.
        unsafe { actor::hew_actor_stop(child) };
        // SAFETY: child was stopped.
        unsafe { actor::hew_actor_free(child) };
    }

    // Free the spec's resources.
    let spec = &mut s.child_specs[idx];
    if !spec.init_state.is_null() {
        // SAFETY: init_state was allocated with libc::malloc.
        unsafe { libc::free(spec.init_state) };
        spec.init_state = ptr::null_mut();
    }
    if !spec.name.is_null() {
        // SAFETY: name was allocated with libc::strdup.
        unsafe { libc::free(spec.name.cast::<c_void>()) };
        spec.name = ptr::null_mut();
    }

    // Swap-remove to avoid shifting all elements.
    let last = s.child_count - 1;
    if idx != last {
        s.children.swap(idx, last);
        s.child_specs.swap(idx, last);

        // Update the supervisor_child_index on the swapped child.
        let swapped = s.children[idx];
        if !swapped.is_null() {
            // SAFETY: swapped child is valid.
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "child index fits in i32 for any reasonable child count"
            )]
            // SAFETY: swapped child pointer was validated as non-null above.
            unsafe {
                (*swapped).supervisor_child_index = idx as i32;
            }
        }
    }

    s.children.pop();
    s.child_specs.pop();
    s.child_count -= 1;
    0
}

/// Register a state-drop callback for a child actor spec.
///
/// Called by codegen immediately after [`hew_supervisor_add_child_spec`] to
/// attach the actor-type's drop function to the internal spec. Every restart
/// path (initial spawn and all subsequent restarts) calls the registered
/// function on the newly spawned actor so that heap-allocated state fields
/// (e.g. `Vec`, `String`) are freed on teardown.
///
/// `child_index` is the zero-based index of the child whose spec should be
/// updated. Indices are stable until [`hew_supervisor_remove_child`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `state_drop_fn` must be a valid function pointer with C ABI that accepts
///   a `*mut c_void` pointing to the actor's state struct and frees every
///   heap-allocated field inside it without freeing the struct itself.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_state_drop(
    sup: *mut HewSupervisor,
    child_index: c_int,
    state_drop_fn: unsafe extern "C" fn(*mut c_void),
) {
    if sup.is_null() || child_index < 0 {
        return;
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    if idx >= s.child_count {
        return;
    }

    s.child_specs[idx].state_drop_fn = Some(state_drop_fn);

    // Guard the children-slot read so a concurrent supervisor restart on
    // another thread cannot replace s.children[idx] between the load and
    // the hew_actor_set_state_drop call (which would leave the new actor
    // without a state-drop registration).
    let _guard = s.children_lock.lock_or_recover();

    // Register on the already-spawned actor for its first run (the initial
    // spawn happens inside add_child_spec before this setter is called).
    let child = s.children[idx];
    if !child.is_null() {
        // SAFETY: child is a valid actor pointer; state_drop_fn has the
        // correct signature.
        unsafe { actor::hew_actor_set_state_drop(child, state_drop_fn) };
    }
}

// ── Circuit breaker constants for C ABI ────────────────────────────────────────

/// Circuit breaker state: CLOSED (normal operation).
#[no_mangle]
pub static HEW_CIRCUIT_BREAKER_CLOSED: c_int = 0;

/// Circuit breaker state: OPEN (blocking restarts).
#[no_mangle]
pub static HEW_CIRCUIT_BREAKER_OPEN: c_int = 1;

/// Circuit breaker state: `HALF_OPEN` (probe restart).
#[no_mangle]
pub static HEW_CIRCUIT_BREAKER_HALF_OPEN: c_int = 2;

// ── Restart notification (deterministic testing) ────────────────────────────

/// Reset the restart notification counter on this supervisor.
///
/// Every completed restart cycle (including budget exhaustion) increments an
/// internal counter and wakes any thread blocked in
/// [`hew_supervisor_wait_restart`]. Resetting the counter lets tests wait for
/// a fresh restart cycle window.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_restart_notify(sup: *mut HewSupervisor) {
    cabi_guard!(sup.is_null());
    // SAFETY: caller guarantees `sup` is a valid pointer from `hew_supervisor_new`.
    let s = unsafe { &mut *sup };
    if let Some(ref pair) = s.restart_notify {
        let mut count = pair.0.lock_or_recover();
        *count = 0;
    } else {
        s.restart_notify = Some(Arc::new((Mutex::new(0), Condvar::new())));
    }
}

/// Block until the supervisor's restart counter reaches at least `target`,
/// or `timeout_ms` milliseconds elapse.
///
/// Returns the current restart count on success, or `0` on timeout / null
/// pointer.  The counter is cumulative and never resets.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_wait_restart(
    sup: *mut HewSupervisor,
    target: usize,
    timeout_ms: u64,
) -> usize {
    cabi_guard!(sup.is_null(), 0);
    // SAFETY: caller guarantees `sup` is a valid pointer from `hew_supervisor_new`.
    let s = unsafe { &*sup };
    let pair = match s.restart_notify {
        Some(ref p) => Arc::clone(p),
        None => return 0,
    };
    let timeout = std::time::Duration::from_millis(timeout_ms);
    let deadline = std::time::Instant::now() + timeout;
    let mut count = pair.0.lock_or_recover();
    while *count < target {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return 0;
        }
        let (guard, wait_result) = pair.1.wait_timeout_or_recover(count, remaining);
        count = guard;
        if wait_result.timed_out() && *count < target {
            return 0;
        }
    }
    *count
}

// ---------------------------------------------------------------------------
// Pool slot substrate — Phase 2.0.b
// ---------------------------------------------------------------------------

/// Register a new pool slot on the supervisor.
///
/// Allocates a fresh `HewActorPool` and appends it to `pool_slots`/`pool_specs`,
/// returning the pool slot index (≥ 0) on success, or -1 on error.
///
/// The checker assigns pool slot indices in source-declaration order, matching
/// the order this function is called during supervisor construction. The returned
/// index is the `pool_key` parameter for [`hew_supervisor_pool_child_get`] and
/// sibling pool ABI functions.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `name` must be a valid, null-terminated C string pointer; it is copied
///   internally and the caller retains ownership.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_add_slot(
    sup: *mut HewSupervisor,
    name: *const c_char,
    strategy: c_int,
    max_members: usize,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };

    let pool_strategy = match strategy {
        1 => PoolStrategy::Random,
        _ => PoolStrategy::RoundRobin,
    };

    // SAFETY: name is guaranteed valid by caller; hew_pool_new copies nothing —
    // we strdup separately so InternalPoolSpec owns the allocation.
    let name_copy: *mut c_char = if name.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees name is a valid C string.
        unsafe { libc::strdup(name) }
    };

    // Allocate the pool. hew_pool_new takes a *const c_char that must stay
    // valid for the pool's lifetime; we pass name_copy which is owned by the
    // parallel InternalPoolSpec and freed in InternalPoolSpec::drop after the
    // pool itself is freed.
    // SAFETY: name_copy is valid (non-null checked below); if null, we pass null.
    let pool = unsafe { crate::pool::hew_pool_new(name_copy, pool_strategy as c_int) };
    if pool.is_null() {
        // Free the duplicated name on allocation failure.
        if !name_copy.is_null() {
            // SAFETY: name_copy was allocated with libc::strdup.
            unsafe { libc::free(name_copy.cast::<c_void>()) };
        }
        return -1;
    }

    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "pool slot count fits in c_int for any realistic supervisor"
    )]
    let index = s.pool_slots.len() as c_int;

    s.pool_slots.push(pool);
    s.pool_specs.push(InternalPoolSpec {
        name: name_copy,
        strategy: pool_strategy,
        max_members,
    });

    index
}

/// Add an actor PID to an existing pool slot.
///
/// `pool_key` is the index returned by [`hew_supervisor_pool_add_slot`].
///
/// Returns 0 on success, -1 if `sup` is null, `pool_key` is out of range, or
/// the pool's `max_members` limit would be exceeded.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_member_add(
    sup: *mut HewSupervisor,
    pool_key: u32,
    actor_pid: u64,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        set_last_error("hew_supervisor_pool_member_add: pool_key out of range");
        return -1;
    }
    let pool = s.pool_slots[i];
    if pool.is_null() {
        set_last_error("hew_supervisor_pool_member_add: pool slot is null");
        return -1;
    }
    // Enforce max_members if configured.
    let max = s.pool_specs[i].max_members;
    if max > 0 {
        // SAFETY: pool is valid.
        let current = unsafe { crate::pool::hew_pool_size(pool) };
        if current >= max {
            set_last_error("hew_supervisor_pool_member_add: pool at max_members capacity");
            return -1;
        }
    }
    // SAFETY: pool is valid.
    unsafe { crate::pool::hew_pool_add(pool, actor_pid) }
}

/// Remove an actor PID from a pool slot.
///
/// Returns 0 on success, -1 if the pool or PID is not found.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_member_remove(
    sup: *mut HewSupervisor,
    pool_key: u32,
    actor_pid: u64,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        set_last_error("hew_supervisor_pool_member_remove: pool_key out of range");
        return -1;
    }
    let pool = s.pool_slots[i];
    if pool.is_null() {
        return -1;
    }
    // SAFETY: pool is valid.
    unsafe { crate::pool::hew_pool_remove(pool, actor_pid) }
}

/// Resolve a pool member by its position index within the pool's current
/// membership snapshot.
///
/// Returns a [`ChildLookupResult`] discriminated as:
///
/// - `Live(handle)` — `handle` carries the actor PID (u64) of the indexed
///   member, encoded as a pointer-width integer. Callers route messages via
///   the PID rather than dereferencing the value as a raw pointer. Use
///   `hew_pid_resolve` (when available) to obtain the `*mut HewActor` for
///   direct dispatch.
/// - `Dead(UnknownSlot)` — `pool_key` is out of range, the pool slot is null,
///   the supervisor is shut down, or `index` is beyond the current member
///   count (which may have shrunk due to dynamic pool member removal).
///
/// The index is **unstable**: pool members are stored as an unordered set
/// internally. A member removed between two calls (via
/// [`hew_supervisor_pool_member_remove`]) may shift other members' indices
/// because the pool uses `swap_remove`. Do not cache an index across removals.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_child_get(
    sup: *mut HewSupervisor,
    pool_key: u32,
    index: u32,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };

    // Fast-path: supervisor-level shutdown.
    if s.cancelled.load(Ordering::Acquire) || s.running.load(Ordering::Acquire) == 0 {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    let pool = s.pool_slots[i];
    if pool.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // Resolve the index within the pool's member list via the public ABI so
    // we stay within the module's encapsulation boundary.
    // SAFETY: pool was created by hew_pool_new and has not been freed.
    let pid = unsafe { crate::pool::hew_pool_get_at(pool, index as usize) };
    if pid == 0 {
        // 0 is returned both for out-of-range and for an empty pool;
        // both cases are dead from the caller's perspective.
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // Encode the PID as a *mut HewActor. Callers must treat this as an
    // opaque u64 PID — not a dereferenceable pointer — until hew_pid_resolve
    // is available to translate it.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "u64 PID encoded as pointer-width integer; caller interprets as u64"
    )]
    let handle = pid as usize as *mut HewActor;
    ChildLookupResult::live(handle)
}

/// Return the number of live members in a pool slot.
///
/// Returns -1 if `sup` is null or `pool_key` is out of range.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "member count fits in i64 for any realistic pool"
)]
pub unsafe extern "C" fn hew_supervisor_pool_len(sup: *mut HewSupervisor, pool_key: u32) -> i64 {
    if sup.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        return -1;
    }
    let pool = s.pool_slots[i];
    if pool.is_null() {
        return -1;
    }
    // SAFETY: pool is valid.
    unsafe { crate::pool::hew_pool_size(pool) as i64 }
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "pool slot unit tests — safety invariants are documented per-test in comments"
)]
mod pool_slot_tests {
    //! Unit tests for the pool slot substrate.
    //!
    //! Tests use a stopped supervisor (running == 0) deliberately: pool slot
    //! lookup checks supervisor shutdown state and returns
    //! `Dead(SupervisorShutdown)` when `running == 0`. Tests that need `Live`
    //! results call `hew_supervisor_start` first or set `running` directly
    //! via the returned raw pointer.

    use std::ptr;
    use std::sync::atomic::Ordering;

    use super::{ChildSlotReason, HewSupervisor};
    use crate::supervisor::{
        hew_supervisor_new, hew_supervisor_pool_add_slot, hew_supervisor_pool_child_get,
        hew_supervisor_pool_len, hew_supervisor_pool_member_add, hew_supervisor_pool_member_remove,
        hew_supervisor_stop,
    };

    const STRATEGY_ONE_FOR_ONE: std::ffi::c_int = 0;
    const ROUND_ROBIN: std::ffi::c_int = 0;

    unsafe fn make_sup() -> *mut HewSupervisor {
        unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) }
    }

    /// Mark the supervisor running so `pool_child_get` doesn't short-circuit.
    unsafe fn mark_running(sup: *mut HewSupervisor) {
        unsafe { (*sup).running.store(1, Ordering::Release) };
    }

    #[test]
    fn pool_child_get_live_returns_pid_as_handle() {
        // Arrange: supervisor with one pool slot, two members.
        let sup = unsafe { make_sup() };
        assert!(!sup.is_null());

        let name = std::ffi::CString::new("workers").unwrap();
        let key = unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(key, 0, "first pool slot index should be 0");

        assert_eq!(unsafe { hew_supervisor_pool_member_add(sup, 0, 1001) }, 0);
        assert_eq!(unsafe { hew_supervisor_pool_member_add(sup, 0, 2002) }, 0);

        unsafe { mark_running(sup) };

        // Act: look up index 0 and index 1.
        let r0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        let r1 = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };

        // Assert: both are Live; handles encode the PIDs.
        assert!(r0.is_live(), "index 0 should be Live");
        assert!(r1.is_live(), "index 1 should be Live");
        assert_eq!(r0.handle as u64, 1001, "handle encodes PID 1001");
        assert_eq!(r1.handle as u64, 2002, "handle encodes PID 2002");

        // Cleanup.
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_out_of_range_index_returns_dead_unknown_slot() {
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 42) };
        unsafe { mark_running(sup) };

        // Index 1 is beyond the single member.
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert_eq!(r.tag, 2, "should be Dead");
        assert_eq!(r.reason, ChildSlotReason::UnknownSlot as u8);
        assert!(r.handle.is_null());

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_after_member_removal_returns_dead_unknown_slot() {
        // Simulates "pool member dynamically scaled out": remove a member,
        // then look up by the (now-invalid) index → Dead(UnknownSlot).
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 100) };
        unsafe { mark_running(sup) };

        // Verify it's Live before removal.
        let before = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert!(before.is_live(), "should be Live before removal");

        // Remove the member.
        assert_eq!(unsafe { hew_supervisor_pool_member_remove(sup, 0, 100) }, 0);

        // Now index 0 is beyond the empty member list.
        let after = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(after.tag, 2, "should be Dead after removal");
        assert_eq!(after.reason, ChildSlotReason::UnknownSlot as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_unknown_pool_key_returns_dead() {
        let sup = unsafe { make_sup() };
        unsafe { mark_running(sup) };

        // No pools added — pool_key 0 is invalid.
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead");
        assert_eq!(r.reason, ChildSlotReason::UnknownSlot as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_null_supervisor_returns_dead_shutdown() {
        // SAFETY: intentionally passing null to verify guard.
        let r = unsafe { hew_supervisor_pool_child_get(ptr::null_mut(), 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead");
        assert_eq!(r.reason, ChildSlotReason::SupervisorShutdown as u8);
    }

    #[test]
    fn pool_child_get_stopped_supervisor_returns_dead_shutdown() {
        let sup = unsafe { make_sup() };
        // Supervisor was never started (running == 0).
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead (supervisor not running)");
        assert_eq!(r.reason, ChildSlotReason::SupervisorShutdown as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_len_tracks_member_add_and_remove() {
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("sizers").unwrap();
        let key = unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(key, 0);

        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 0);
        unsafe { hew_supervisor_pool_member_add(sup, 0, 11) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 22) };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 2);
        unsafe { hew_supervisor_pool_member_remove(sup, 0, 11) };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 1);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_len_invalid_key_returns_minus_one() {
        let sup = unsafe { make_sup() };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 99) }, -1);
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn multiple_pool_slots_have_disjoint_key_spaces() {
        let sup = unsafe { make_sup() };
        let n0 = std::ffi::CString::new("alpha").unwrap();
        let n1 = std::ffi::CString::new("beta").unwrap();

        let k0 = unsafe { hew_supervisor_pool_add_slot(sup, n0.as_ptr(), ROUND_ROBIN, 0) };
        let k1 = unsafe { hew_supervisor_pool_add_slot(sup, n1.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(k0, 0);
        assert_eq!(k1, 1);

        // Add different PIDs to each pool.
        unsafe { hew_supervisor_pool_member_add(sup, 0, 111) };
        unsafe { hew_supervisor_pool_member_add(sup, 1, 222) };
        unsafe { mark_running(sup) };

        let r0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        let r1 = unsafe { hew_supervisor_pool_child_get(sup, 1, 0) };

        assert!(r0.is_live());
        assert!(r1.is_live());
        assert_eq!(r0.handle as u64, 111);
        assert_eq!(r1.handle as u64, 222);

        unsafe { hew_supervisor_stop(sup) };
    }
}
