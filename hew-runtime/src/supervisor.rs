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
use crate::internal::types::{
    HewActorState, HewDispatchFn, HewLifecycleFn, HewOnCrashFn, HewOverflowPolicy,
};
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
    } else if state == HewActorState::Suspended as i32 {
        "Suspended"
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
    /// Static-child indices backing this pool's members, in member order.
    ///
    /// A STATIC pool (`pool name: Type(count: N)`) registers its N members as
    /// ordinary static children in `HewSupervisor.children[]` and records each
    /// member's static-child index here via
    /// [`hew_supervisor_pool_member_add_static`]. The accessor
    /// (`hew_supervisor_pool_child_get`) resolves member `i` through
    /// `children[static_members[i]]` — the LIVE slot — so a restarted member is
    /// re-resolved automatically (the restart machinery re-fills the static
    /// slot; no stale PID is cached in the pool).
    ///
    /// Empty for a DYNAMIC pool whose members are PIDs added via
    /// [`hew_supervisor_pool_member_add`] (the `hew_pool` PID-set path). The two
    /// are mutually exclusive: `pool_child_get` reads `static_members` when it is
    /// non-empty, else falls back to the PID set.
    static_members: Vec<usize>,
}

impl Drop for InternalPoolSpec {
    fn drop(&mut self) {
        if !self.name.is_null() {
            // SAFETY: name was allocated with libc::strdup.
            unsafe { libc::free(self.name.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
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

/// Restart strategies. `pub` so codegen names them by symbol when emitting
/// the `hew_supervisor_new(strategy, ...)` call from a supervisor bootstrap
/// function — single source of truth across runtime + codegen.
pub const STRATEGY_ONE_FOR_ONE: c_int = 0;
pub const STRATEGY_ONE_FOR_ALL: c_int = 1;
pub const STRATEGY_REST_FOR_ONE: c_int = 2;
/// `simple_one_for_one` (pool dynamics). Reserved in the strategy ABI so
/// every variant is explicit on the runtime side; the codegen surface that
/// emits this constant — and the per-pool runtime semantics — lands in S-E.
/// Today the match arm in `restart_with_budget_and_strategy` accepts the
/// variant as a documented no-op (pool restart is driven by the per-pool
/// machinery on `HewSupervisor.pool_*`, not by this child-restart helper).
pub const STRATEGY_SIMPLE_ONE_FOR_ONE: c_int = 3;

/// Restart policies. `pub` for the same reason as the strategy constants:
/// codegen names them when emitting `HewChildSpec.restart_policy` from a
/// supervisor bootstrap.
pub const RESTART_PERMANENT: c_int = 0;
pub const RESTART_TRANSIENT: c_int = 1;
pub const RESTART_TEMPORARY: c_int = 2;

// ── CrashAction return tags (M-4) ─────────────────────────────────────────
//
// The `HewOnCrashFn` ABI returns the hook's `CrashAction` decision as a 2-byte
// `#[repr(C)] HewCrashActionAbi { tag: u8, payload_pad: [u8;1] }` struct BY VALUE
// (mirroring the codegen `%CrashAction = { i8, [1 x i8] }`); the supervisor reads
// field 0 (the `tag`) via `tag_i32()` and decodes it against these constants, in
// `std/failure.hew::CrashAction` declaration order. The supervisor HONOURS this
// return: it takes precedence over the static `restart_policy` when a hook is
// present (the at-crash-time decision overrides the static default). A tag outside
// `0..=2` is treated fail-closed as `Restart` (the conservative default that
// preserves the pre-M-4 restart-policy behaviour).
pub const CRASH_ACTION_RESTART: i32 = 0;
pub const CRASH_ACTION_ESCALATE: i32 = 1;
pub const CRASH_ACTION_KILL: i32 = 2;

// ── Exit reasons ─────────────────────────────────────────────────────────
//
// Trap error codes and the typed `ExitReason` live in
// [`crate::internal::types`] because both native and WASM arena/dispatch
// paths must stamp the canonical code on an actor crash, and the supervisor
// module is `cfg(not(target_arch = "wasm32"))`. They are re-exported here so
// existing `crate::supervisor::*` call sites keep resolving.
pub use crate::internal::types::{
    ExitReason, HEW_TRAP_ACTOR_SEND_FAILED, HEW_TRAP_DIVIDE_BY_ZERO, HEW_TRAP_HEAP_EXCEEDED,
    HEW_TRAP_INDEX_OUT_OF_BOUNDS, HEW_TRAP_INTEGER_OVERFLOW, HEW_TRAP_SHIFT_OUT_OF_RANGE,
    HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
};

/// C-ABI trap entry-point invoked by codegen-emitted IR before the
/// `llvm.trap` terminator on a `Terminator::Trap { kind }` block.
///
/// Inside an actor-dispatch context, this records `code` as the actor's
/// crash reason and longjmps back to the scheduler's recovery frame —
/// matching the `HEW_TRAP_HEAP_EXCEEDED` precedent. Outside a dispatch
/// context (top-level `main`, `hew eval` REPL, JIT preview) there is no
/// recovery context; `try_direct_longjmp_with_code` is a no-op and this
/// function returns, then emits a diagnostic naming the trap kind before
/// the caller's `llvm.trap` terminates the process.
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
    // If we reach here, there is no actor recovery context — this trap
    // occurred in main/free-fn context. Emit a diagnostic before the
    // caller's `llvm.trap` terminates the process so the crash is never
    // silent (F1.3 / fail-closed-with-diagnostic requirement).
    //
    // eprintln! is safe here: hew_trap_with_code is called from generated
    // code, not from a signal handler, so the stderr lock is available.
    let kind = trap_kind_name(code);
    eprintln!("hew: trap in main context: {kind}");
}

/// Map a trap code to a human-readable trap kind name.
fn trap_kind_name(code: i32) -> &'static str {
    match code {
        HEW_TRAP_HEAP_EXCEEDED => "HeapExceeded",
        HEW_TRAP_INTEGER_OVERFLOW => "IntegerOverflow",
        HEW_TRAP_DIVIDE_BY_ZERO => "DivideByZero",
        HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE => "SignedMinDivNegOne",
        HEW_TRAP_SHIFT_OUT_OF_RANGE => "ShiftOutOfRange",
        HEW_TRAP_INDEX_OUT_OF_BOUNDS => "IndexOutOfBounds",
        HEW_TRAP_ACTOR_SEND_FAILED => "ActorSendFailed",
        _ => "Trap",
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
    /// Non-zero when the child actor participates in an actor-ref cycle.
    /// Future consumer: cycle-detection / Machine Lane B cycle handling.
    pub cycle_capable: c_int,
    /// Optional crash handler invoked before the restart policy is applied.
    /// Called with the execution context, trap-kind code, and actor state
    /// pointer when the child exits with `HewActorState::Crashed`.
    /// `None` / null means no handler. Not read by the runtime in this change;
    /// the invocation path is added in a follow-on change.
    pub on_crash: Option<HewOnCrashFn>,
    /// Optional lifecycle wrapper that runs the child actor's `init()` /
    /// `#[on(start)]` hooks. `None` / null when the actor declares neither.
    ///
    /// Read during `hew_supervisor_add_child_spec` (exactly as `on_crash` is)
    /// and copied into the internal spec so it fires on the INITIAL supervised
    /// spawn — the spawn happens inside `add_child_spec`, before any post-hoc
    /// setter runs, so the literal field (not the setter) is the load-bearing
    /// carrier for the initial fire. `restart_child_from_spec` then calls it on
    /// every spawn (initial and restart) from the one firing site.
    ///
    /// Read during `hew_supervisor_add_child_spec` (exactly as `on_crash` is)
    /// and copied into the internal spec so it fires on the INITIAL supervised
    /// spawn — the spawn happens inside `add_child_spec`, before any post-hoc
    /// setter runs, so the literal field (not the setter) is the load-bearing
    /// carrier for the initial fire. `restart_child_from_spec` then calls it on
    /// every spawn (initial and restart) from the one firing site.
    ///
    /// ABI: this is the trailing `#[repr(C)]` field; the codegen-emitted
    /// `hew_child_spec_struct_type` mirror appends a matching `ptr` slot in the
    /// same position. Field-order drift here is wrong-code at the FFI boundary.
    pub lifecycle_fn: Option<HewLifecycleFn>,
    /// Per-child init thunk — the v0.6 init-closure restart model.
    ///
    /// When `Some`, this thunk is THE source of the child's actor state on the
    /// initial spawn AND every restart, REPLACING the byte-copy state template
    /// (`init_state`). `restart_child_from_spec` calls `init_fn(config)` to
    /// PRODUCE a fresh, independently-owned state each time, so owned
    /// (`string`/`Vec`) init args get unaliased heap on every incarnation —
    /// the structural fix for the byte-copy-template-replay aliasing hazard
    /// (audit C1). `init_state`/`init_state_size` are left NULL/0 when this is
    /// `Some` (the template deep-copy in `add_child_spec` is skipped).
    ///
    /// Carried IN the spec literal (like `on_crash`/`lifecycle_fn`) so the
    /// INITIAL supervised spawn — which happens inside `add_child_spec` before
    /// any post-hoc setter runs — uses the thunk. The mirror precedent is
    /// `SupervisorChildSpec.init_fn` for child *supervisors*.
    ///
    /// ABI: a trailing `#[repr(C)]` field; the codegen-emitted
    /// `hew_child_spec_struct_type` mirror appends a matching `ptr` slot.
    pub init_fn: Option<HewChildInitFn>,
    /// Pointer to the supervisor's construction-time config buffer, passed to
    /// `init_fn` on every call so the thunk re-reads `config.field` and
    /// re-clones config-derived owned values per incarnation. `null` when the
    /// supervisor takes no config (the thunk is a const-only producer).
    ///
    /// This is a BORROW: the buffer is owned by the supervisor (adopted once at
    /// the first `hew_supervisor_set_child_init_fn` call) and freed exactly once
    /// at supervisor teardown. `add_child_spec` copies the pointer into the
    /// internal spec; the thunk only READS it, never frees it.
    ///
    /// ABI: a trailing `#[repr(C)]` field; the codegen mirror appends a `ptr`.
    pub config: *mut c_void,
    /// Size in bytes of the config buffer at `config`. Carried so the supervisor
    /// can adopt the buffer with its exact length and the thunk can bounds-check
    /// config reads if needed. `0` when `config` is null.
    ///
    /// ABI: the final trailing `#[repr(C)]` field; the codegen mirror appends an
    /// `i64` slot. Field-order drift here is wrong-code at the FFI boundary.
    pub config_size: usize,
}

/// Child lifecycle event (sent as system message payload).
///
/// `crash_code` carries the trap-kind integer (`HEW_TRAP_*` constants, 201–205,
/// or other actor-defined error codes) captured from the child actor's
/// `error_code` slot at the moment of the trap. It is meaningful only when
/// `exit_state == HewActorState::Crashed`; for normal stops it is `0`.
/// Routing this through the event payload lets the supervisor record the real
/// trap code in crash-stats and forward it to a registered `on_crash` handler
/// instead of falling back to the historical SIGSEGV placeholder (`11`).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct ChildEvent {
    child_index: c_int,
    child_id: u64,
    exit_state: c_int,
    crash_code: c_int,
}

// ---------------------------------------------------------------------------
// Supervisor init function type (for nested supervisor restart)
// ---------------------------------------------------------------------------

/// Init function pointer type for child supervisors.
/// Called to create and start a fresh supervisor instance.
/// Returns a pointer to the new `HewSupervisor`.
pub type SupervisorInitFn = unsafe extern "C" fn() -> *mut HewSupervisor;

/// Result of a child init thunk: a freshly-produced, independently-owned actor
/// state wrapper and its size.
///
/// `#[repr(C)]`, two-field — the codegen-emitted thunk returns this by value.
/// `state` is a `malloc`-compatible heap allocation of exactly `size` bytes
/// whose owned fields (`Vec`/`String` heap) are independent deep clones, NOT
/// aliases of any template or config field. Ownership transfers to the caller;
/// `state` is `null` on allocation failure (fail-closed — the thunk could not
/// produce a fresh state).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewChildInitResult {
    /// The fresh, owned actor state wrapper (or `null` on OOM).
    pub state: *mut c_void,
    /// Size in bytes of `state`.
    pub size: usize,
}

/// Per-child init thunk type — the v0.6 init-closure restart model for ACTOR
/// children (the analogue of [`SupervisorInitFn`] for child supervisors).
///
/// Produces a fresh, independently-owned actor state wrapper for a supervised
/// child by re-running every init-arg expression (literal const, `config.field`
/// load, or owned deep-clone) in the supervisor's config context. Called once
/// at the initial spawn and again on EVERY restart, so each incarnation gets
/// fresh, unaliased owned values — the structural fix the byte-copy template
/// could not provide. Returns [`HewChildInitResult`]; `state == null` signals
/// allocation failure (the restart path then fails closed: backoff, null slot,
/// no circuit-breaker advance — mirroring the clone-OOM policy).
///
/// `config` is a borrow of the supervisor-owned config buffer (or `null` for a
/// const-only thunk). The thunk only READS it.
pub type HewChildInitFn = unsafe extern "C" fn(config: *const c_void) -> HewChildInitResult;

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

    /// Parked `await_restart` continuations — the COOPERATIVE restart observer.
    ///
    /// Distinct from `restart_notify` (the thread-blocking Condvar barrier used
    /// by `hew_supervisor_wait_restart`). Each waiter is an actor that executed
    /// `await_restart sup.child` on a Transient slot and parked instead of
    /// thread-blocking the single cooperative scheduler. `notify_restart` fires
    /// every waiter (deposit readiness + `enqueue_resume`) after the restart
    /// cycle completes, so the resumed continuation re-resolves a Live slot
    /// (`notify_restart` runs AFTER `store_child_slot`). Drained on fire and on
    /// supervisor teardown; a cancelled slot drops its wake (the channel-core
    /// race guard).
    restart_await_waiters: Mutex<Vec<RestartAwaitWaiter>>,

    /// Pool children declared via `pool name: Type` in the supervisor surface.
    ///
    /// Indexed by checker-assigned pool slot index. Disjoint from the static
    /// `children` slot space — a `pool_key` of 0 is the first *pool* child,
    /// not the first static child. Each entry is a `Box`-owned `HewActorPool`.
    pool_slots: Vec<*mut HewActorPool>,
    /// Parallel spec for pool children. `pool_slots[i]` and `pool_specs[i]`
    /// always have the same length.
    pool_specs: Vec<InternalPoolSpec>,

    /// Construction-time config buffer (the v0.6 init-closure restart model's
    /// dynamic-data source). A single supervisor-owned `malloc`'d copy of the
    /// supervisor's spawn-time config struct, captured ONCE at bootstrap and
    /// shared (by borrow) with every child's `init_fn`. Each thunk reads
    /// `config.field` to recompute config-derived init args per incarnation.
    ///
    /// Adopted on the FIRST `hew_supervisor_set_child_init_fn` call (or carried
    /// in the first `init_fn` child's `HewChildSpec` literal); subsequent
    /// registrations re-use the same pointer. Freed EXACTLY ONCE at supervisor
    /// teardown (`stop_supervisor_owned`). The thunks only ever read it.
    /// `null` when the supervisor takes no config.
    config_buf: *mut c_void,
    /// Size in bytes of `config_buf`. `0` when `config_buf` is null.
    config_size: usize,
    /// Drop glue for the config struct's OWNED fields (`string`/`bytes`/…),
    /// run over `config_buf` once at teardown BEFORE the flat `libc::free`.
    ///
    /// The config buffer is a flat snapshot of the moved-in config value, so it
    /// OWNS the config struct's inner owned fields (the thunks only CLONE from
    /// them, never take ownership). Without this, those inner fields would leak
    /// at teardown (the flat free reclaims only the wrapper). Codegen registers
    /// the config struct's `__hew_record_drop_inplace_<T>` here when the config
    /// has owned fields; `None` for an all-scalar config (nothing to drop).
    config_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
}

/// One parked `await_restart` continuation: the awaiting actor + its readiness
/// slot. `notify_restart` fires every waiter exactly once per restart cycle,
/// depositing readiness into `slot` and re-enqueuing `actor` on the scheduler.
struct RestartAwaitWaiter {
    /// The parked-continuation actor, woken via `enqueue_resume`. Raw and
    /// possibly-stale: `enqueue_resume` re-validates liveness, never this code.
    actor: *mut HewActor,
    /// The readiness slot; the observer holds one retained ref while registered.
    slot: *mut crate::read_slot::HewReadSlot,
}

// SAFETY: `actor` is re-validated under the registry lock by `enqueue_resume`;
// `slot` is reference-counted. The waiter is only moved between the supervisor's
// `restart_await_waiters` mutex and the firing path, both single-consumer.
unsafe impl Send for RestartAwaitWaiter {}

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
    /// Copied from `HewChildSpec::cycle_capable` and forwarded into
    /// `HewActorOpts` for every restart.
    cycle_capable: c_int,
    /// Crash handler copied from `HewChildSpec::on_crash`. Invoked from
    /// `apply_restart` before the restart policy is consulted when the child
    /// exits with `HewActorState::Crashed`. `None` means no handler.
    on_crash: Option<HewOnCrashFn>,
    /// Codegen-emitted drop callback for owned state fields (e.g. `Vec`, `String`).
    /// Registered via [`hew_supervisor_set_child_state_drop`] after the child spec
    /// is added. Every restart path calls this on the newly spawned actor so that
    /// restarted actors free their heap-allocated state fields on teardown.
    state_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
    /// Codegen-emitted deep-clone callback for the spec's `init_state` template.
    /// Registered via [`hew_supervisor_set_child_state_clone`] after the child spec
    /// is added. When present, the restart path calls
    /// `state_clone_fn(spec.init_state)` to produce an independently-owned deep
    /// clone for each new actor (consumed via [`actor::hew_actor_spawn_opts_adopt`])
    /// instead of the legacy byte-copy — fixing the supervisor-restart byte-alias
    /// UAF (audit C1 / spec-section §5). Registration itself also breaks the
    /// initial-spawn byte-alias by re-cloning the template in place
    /// (see [`hew_supervisor_set_child_state_clone`] doc).
    state_clone_fn: Option<actor::HewStateCloneFn>,
    /// Codegen-emitted lifecycle wrapper that runs the child actor's `init()` /
    /// `#[on(start)]` hooks. Copied from `HewChildSpec::lifecycle_fn` at spec
    /// registration (so the INITIAL supervised spawn fires it inside
    /// `add_child_spec`), and re-applied by every restart path. `None` means
    /// the actor has no lifecycle hook and the spawn fires no wrapper.
    lifecycle_fn: Option<HewLifecycleFn>,
    /// Per-child init thunk (the v0.6 init-closure restart model). When `Some`,
    /// `restart_child_from_spec` calls `init_fn(config)` to PRODUCE a fresh,
    /// independently-owned actor state on the initial spawn and every restart,
    /// instead of cloning/byte-copying `init_state`. `init_state`/
    /// `init_state_size` are left NULL/0 in this mode. Copied from
    /// `HewChildSpec::init_fn` at spec registration so the initial supervised
    /// spawn (inside `add_child_spec`) uses the thunk.
    init_fn: Option<HewChildInitFn>,
    /// Borrowed pointer to the supervisor's `config_buf`, passed to `init_fn` on
    /// every call. The supervisor owns the allocation (freed once at teardown);
    /// this spec never frees it. `null` for a const-only thunk.
    config: *mut c_void,
}

impl Drop for InternalChildSpec {
    fn drop(&mut self) {
        if !self.init_state.is_null() {
            // Call the user-provided drop callback before freeing the wrapper so
            // owned inner fields (e.g. heap-backed payload buffers) are released
            // before the wrapper allocation is reclaimed.
            //
            // ONLY call when state_clone_fn is registered: registering a clone fn
            // re-clones spec.init_state in place, giving it an independently-owned
            // allocation. Without a clone fn the spec's init_state is a byte-alias
            // of the initial actor's init_state; the actor's lifecycle drops the
            // shared inner payload, so calling state_drop_fn here would double-free.
            //
            // WHY: init_state is a C heap wrapper whose inner fields may own malloc'd
            // memory; the wrapper-level libc::free below does not recurse into those
            // fields. state_drop_fn is the one authority on releasing them.
            // WHEN obsolete: if init_state becomes a Rust-managed type with a proper
            // Drop impl this call is no longer needed.
            if self.state_clone_fn.is_some() {
                if let Some(drop_fn) = self.state_drop_fn {
                    // SAFETY: state_drop_fn was registered by the caller for this
                    // exact init_state allocation; init_state is valid and
                    // independently owned (re-cloned at clone-fn registration time).
                    unsafe { drop_fn(self.init_state) };
                }
            }
            // SAFETY: init_state was allocated with libc::malloc in
            // hew_supervisor_add_child_spec.
            unsafe { libc::free(self.init_state) }; // ALLOCATOR-PAIRING: libc
            self.init_state = ptr::null_mut();
        }
        if !self.name.is_null() {
            // SAFETY: name was allocated with libc::strdup in
            // hew_supervisor_add_child_spec.
            unsafe { libc::free(self.name.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
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
            cycle_capable: 0,
            on_crash: None,
            state_drop_fn: None,
            state_clone_fn: None,
            lifecycle_fn: None,
            init_fn: None,
            config: ptr::null_mut(),
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

/// Get the current monotonic time in nanoseconds, anchored on the process-wide
/// epoch ([`crate::monotonic`]).
fn monotonic_time_ns() -> u64 {
    crate::monotonic::monotonic_ns()
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

/// Resolve the supervisor's own actor id for trace attribution. Returns `0`
/// when the supervisor has no backing actor yet (pre-start). Read-only; used
/// only to tag emitted supervisor lifecycle events.
fn supervisor_actor_id(sup: &HewSupervisor) -> u64 {
    if sup.self_actor.is_null() {
        0
    } else {
        // SAFETY: self_actor belongs to the live supervisor for the duration
        // of this dispatch.
        unsafe { (*sup.self_actor).id }
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
        // Child-supervisor escalation: no single trap code applies to the
        // subtree-restart-budget exhaustion that triggered this escalation.
        // Use the honest unknown value rather than fabricating a signal number.
        crash_code: 0,
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

    // Observability (read-only side effect, AFTER the escalation decision and
    // dispatch): record that this supervisor escalated to its parent. Never
    // gates control flow.
    crate::tracing::record_supervisor_event(
        supervisor_actor_id(sup),
        crate::tracing::SPAN_SUPERVISOR_ESCALATE,
        i32::try_from(sup.index_in_parent).unwrap_or(i32::MAX),
    );
}

/// Check if circuit breaker allows restart for a child.
#[expect(
    clippy::match_same_arms,
    reason = "CLOSED and HALF_OPEN have same logic but different semantic meaning"
)]
fn circuit_breaker_should_restart(spec: &mut InternalChildSpec, sup_actor_id: u64) -> bool {
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
                                                // Observability (AFTER the OPEN → HALF_OPEN transition).
                crate::tracing::record_supervisor_event(
                    sup_actor_id,
                    crate::tracing::SPAN_SUPERVISOR_CIRCUIT_HALF_OPEN,
                    0,
                );
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
fn circuit_breaker_record_crash(spec: &mut InternalChildSpec, signal: i32, sup_actor_id: u64) {
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
                // Pass the supervisor-clock `now_ns` (the SAME clock used to
                // record the crash timestamp at the top of this fn) so the
                // window comparison is single-clock. Reading a second,
                // independently-epoched clock inside `recent_count` made a
                // freshly-recorded timestamp look "in the future" under load and
                // silently undercounted crashes, leaving the breaker CLOSED.
                // SAFETY: crash_stats was created by hew_crash_stats_new.
                let recent_count = unsafe {
                    crate::crash::hew_crash_stats_recent_count(
                        spec.circuit_breaker.crash_stats,
                        window_ns,
                        now_ns,
                    )
                };
                if recent_count >= spec.circuit_breaker.max_crashes {
                    spec.circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN
                    spec.circuit_breaker.opened_at_ns = now_ns;
                    // Observability (AFTER the CLOSED → OPEN transition).
                    crate::tracing::record_supervisor_event(
                        sup_actor_id,
                        crate::tracing::SPAN_SUPERVISOR_CIRCUIT_OPEN,
                        i32::try_from(recent_count).unwrap_or(i32::MAX),
                    );
                }
            }
        }
        2 => {
            // HEW_CIRCUIT_BREAKER_HALF_OPEN
            // Probe restart failed, go back to open
            spec.circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN
            spec.circuit_breaker.opened_at_ns = now_ns;
            // Observability (AFTER the HALF_OPEN → OPEN transition).
            crate::tracing::record_supervisor_event(
                sup_actor_id,
                crate::tracing::SPAN_SUPERVISOR_CIRCUIT_OPEN,
                0,
            );
        }
        _ => {
            // Already open, no state change needed
        }
    }
}

/// Update circuit breaker state after a successful restart.
fn circuit_breaker_record_success(spec: &mut InternalChildSpec, sup_actor_id: u64) {
    if spec.circuit_breaker.state == 2 {
        // HEW_CIRCUIT_BREAKER_HALF_OPEN
        // Probe restart succeeded, close the circuit
        spec.circuit_breaker.state = 0; // HEW_CIRCUIT_BREAKER_CLOSED
                                        // Observability (AFTER the HALF_OPEN → CLOSED transition).
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_CIRCUIT_CLOSE,
            0,
        );
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

/// Increment the restart counter and wake every restart waiter.
///
/// Two wake paths fire here, both AFTER the restart cycle's `store_child_slot`
/// has made the new child reachable (this function is called at the tail of
/// `restart_with_budget_and_strategy` / `restart_child_supervisor_with_budget`):
///
/// 1. The thread-blocking Condvar barrier (`hew_supervisor_wait_restart`) — the
///    counter increment + `notify_all`.
/// 2. The COOPERATIVE `await_restart` observers — every parked continuation in
///    `restart_await_waiters` gets readiness deposited + `enqueue_resume`, then
///    the registry is drained. A resumed continuation re-resolves the slot and
///    is guaranteed Live (the store-before-notify ordering is the resume-contract
///    anchor).
///
/// ORDERING INVARIANT (lost-wakeup guard): the counter bump MUST happen before
/// `wake_restart_await_waiters` acquires `restart_await_waiters`. A racing
/// `hew_supervisor_restart_await_suspend` re-reads the counter while holding
/// `restart_await_waiters`; bumping first means that if this drain ran against an
/// empty registry (the waiter not yet pushed), the awaiting actor's under-lock
/// recheck observes the advanced counter and resolves READY instead of parking
/// against a wake that already fired. Do not reorder the bump after the drain.
fn notify_restart(sup: &HewSupervisor) {
    if let Some(ref pair) = sup.restart_notify {
        let mut count = pair.0.lock_or_recover();
        *count += 1;
        pair.1.notify_all();
    }
    wake_restart_await_waiters(sup);
}

/// Fire and drain every parked `await_restart` continuation. Mirrors the
/// task-completion observer wake discipline (`task_await_wake`): deposit a Data
/// readiness status into each waiter's slot (a no-op + no wake if its abandon
/// edge cancelled the slot first), `enqueue_resume` the parked actor on a
/// successful deposit, then release the observer's retained slot ref.
fn wake_restart_await_waiters(sup: &HewSupervisor) {
    let waiters: Vec<RestartAwaitWaiter> =
        std::mem::take(&mut *sup.restart_await_waiters.lock_or_recover());
    for waiter in waiters {
        // SAFETY: the observer holds an in-flight ref on the slot; depositing a
        // terminal status is the documented reactor-deposit contract. A no-op +
        // no wake if the abandon edge cancelled the slot first.
        let do_wake = unsafe {
            crate::read_slot::read_slot_deposit_status(
                waiter.slot,
                crate::read_slot::ReadStatus::Data,
            )
        };
        if do_wake {
            // SAFETY: `enqueue_resume` re-validates `waiter.actor` under the
            // registry lock; a freed actor drops the wake with no deref.
            unsafe { crate::scheduler::enqueue_resume(waiter.actor, ptr::null_mut()) };
        }
        // Release the observer's retained in-flight ref (the single authority).
        // SAFETY: the observer owned this ref; nothing else releases it.
        unsafe { crate::read_slot::hew_read_slot_free(waiter.slot) };
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
    if let Ok(handle) = std::thread::Builder::new()
        .name("deferred-sup-stop".into())
        .spawn(move || {
            stop_deferred_supervisor(DeferredSupervisorStop(child_addr as *mut HewSupervisor));
        })
    {
        // Register the teardown thread so `cleanup_all_actors` joins it
        // before sweeping the actors this thread still dereferences.
        crate::lifetime::live_actors::push_deferred_teardown_thread(handle);
        true
    } else {
        if allow_sync_fallback {
            eprintln!(
                "hew: warning: failed to spawn deferred supervisor-stop thread, cleaning up synchronously"
            );
            stop_deferred_supervisor(DeferredSupervisorStop(child_sup));
        } else {
            eprintln!("hew: warning: failed to spawn deferred supervisor-stop thread");
        }
        false
    }
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
    if let Ok(handle) = std::thread::Builder::new()
        .name("deferred-sup-stop".into())
        .spawn(move || {
            stop_owned_deferred_supervisor(DeferredSupervisorStop(sup_addr as *mut HewSupervisor));
        })
    {
        // Register the teardown thread so `cleanup_all_actors` joins it
        // before sweeping the actors this thread still dereferences.
        crate::lifetime::live_actors::push_deferred_teardown_thread(handle);
        true
    } else {
        eprintln!("hew: warning: failed to spawn deferred supervisor-stop thread");
        false
    }
}

/// Free a batch of stopped siblings on a background thread during a restart and
/// register the `JoinHandle` in the deferred-teardown registry.
///
/// The siblings are still tracked in `LIVE_ACTORS` until this thread runs the
/// restart-aware free on each. Like the supervisor-stop teardown sites, leaving
/// the thread detached would let `cleanup_all_actors` sweep those allocations
/// out from under an in-flight crash-restart free — a use-after-free /
/// double-free. Registering the handle puts the teardown under the same
/// join-before-sweep barrier (`drain_deferred_teardown_threads`).
fn spawn_deferred_restart_free(deferred: Vec<DeferredFree>) {
    if deferred.is_empty() {
        return;
    }
    match std::thread::Builder::new()
        .name("deferred-free".into())
        .spawn(move || {
            for d in deferred {
                // SAFETY: actor was stopped; supervisor no longer references it.
                // Use the restart-aware free path so the codegen state-drop does
                // NOT run on field pointers that are still byte-aliased by
                // `spec.init_state` and about to be reused by
                // `restart_child_from_spec`. See
                // `actor::free_actor_resources_with_options`.
                unsafe { actor::hew_actor_free_for_restart(d.0) };
            }
        }) {
        Ok(handle) => {
            // Register the teardown thread so `cleanup_all_actors` joins it
            // before sweeping the actors this thread still dereferences.
            crate::lifetime::live_actors::push_deferred_teardown_thread(handle);
        }
        Err(_) => {
            eprintln!("hew: warning: failed to spawn deferred-free thread");
        }
    }
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

/// Test-only hook fired in `hew_supervisor_restart_await_suspend` exactly in the
/// gap between the pre-park `child_get` and acquiring `restart_await_waiters`.
/// The concurrency regression installs a closure that drives a full restart cycle
/// (and its `notify_restart`) on another thread *while this thread is paused in
/// the gap*, reproducing the lost-wakeup interleaving deterministically.
#[cfg(all(test, not(target_arch = "wasm32")))]
static RESTART_AWAIT_PARK_GAP_HOOK: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fire_restart_await_park_gap_hook() {
    let hook = RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover().clone();
    if let Some(hook) = hook {
        hook();
    }
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
    let mut s = unsafe { Box::from_raw(sup) }; // ALLOCATOR-PAIRING: GlobalAlloc

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

    // Drain any parked `await_restart` continuations on teardown: wake each so
    // the resumed actor re-resolves the (now shut-down) supervisor and fails
    // closed (`child_get` → Dead(SupervisorShutdown)) rather than hanging
    // forever, and release the observer's retained slot ref. Mirrors the
    // notify_restart wake discipline; teardown is the abandon-everything edge.
    let parked: Vec<RestartAwaitWaiter> =
        std::mem::take(&mut *s.restart_await_waiters.lock_or_recover());
    for waiter in parked {
        // SAFETY: the observer holds an in-flight ref; depositing readiness is
        // the reactor-deposit contract (no-op if the abandon edge cancelled it).
        let do_wake = unsafe {
            crate::read_slot::read_slot_deposit_status(
                waiter.slot,
                crate::read_slot::ReadStatus::Data,
            )
        };
        if do_wake {
            // SAFETY: `enqueue_resume` re-validates the actor under the registry
            // lock; a freed actor drops the wake with no deref.
            unsafe { crate::scheduler::enqueue_resume(waiter.actor, ptr::null_mut()) };
        }
        // SAFETY: the observer owned this ref; nothing else releases it.
        unsafe { crate::read_slot::hew_read_slot_free(waiter.slot) };
    }

    // Free pool slots. Each pool was Box-allocated by hew_supervisor_pool_add_slot;
    // pool_specs Drop impl handles name deallocation.
    for pool in std::mem::take(&mut s.pool_slots) {
        if !pool.is_null() {
            // SAFETY: pool was created by Box::into_raw in hew_supervisor_pool_add_slot.
            unsafe { drop(Box::from_raw(pool)) }; // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    // Free the construction-time config buffer (the init-closure restart
    // model's dynamic-data source). Freed EXACTLY ONCE here: every child spec
    // holds only a BORROW of this pointer, and the thunks only ever read it.
    // After every child actor and spec is dropped above, no live thunk can run,
    // so the buffer has no remaining readers.
    if !s.config_buf.is_null() {
        // The config buffer is a flat snapshot of the moved-in config value, so
        // it OWNS the config struct's inner owned fields (`string`/`bytes`/…) —
        // the thunks only CLONE from them into actor state (the actors'
        // state_drop_fns release those clones). Run the config struct's
        // drop-inplace glue here to release the buffer's OWN inner owned fields
        // BEFORE the flat free; without it those fields leak (the flat free
        // reclaims only the wrapper). `None` for an all-scalar config (no inner
        // owned field to drop). No live thunk can run at this point (every child
        // actor and spec is dropped above), so this is the sole final reader.
        if let Some(drop_fn) = s.config_drop_fn {
            // SAFETY: drop_fn is the codegen-emitted
            // `__hew_record_drop_inplace_<ConfigTy>` for this buffer's config
            // struct; config_buf points at a fully-initialised instance of that
            // struct. Runs exactly once (config_buf is freed + nulled below).
            unsafe { drop_fn(s.config_buf) };
        }
        // SAFETY: config_buf was a libc::malloc'd buffer adopted (ownership
        // transferred) from codegen via hew_supervisor_add_child_spec /
        // hew_supervisor_set_child_init_fn. Inner owned fields were released by
        // config_drop_fn above; this free reclaims the config wrapper itself.
        unsafe { libc::free(s.config_buf) }; // ALLOCATOR-PAIRING: libc
        s.config_buf = ptr::null_mut();
        s.config_size = 0;
        s.config_drop_fn = None;
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
    let (opts, state_drop_fn, state_clone_fn, lifecycle_fn, init_fn, config) = {
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
            cycle_capable: spec.cycle_capable,
        };
        (
            opts,
            spec.state_drop_fn,
            spec.state_clone_fn,
            spec.lifecycle_fn,
            spec.init_fn,
            spec.config,
        )
    };

    // ── v0.6 init-closure restart model — the leading branch ────────────
    //
    // When the spec carries an `init_fn`, the thunk PRODUCES the child's state
    // (initial spawn AND every restart) by re-running every init-arg
    // expression against the supervisor's config. This REPLACES the byte-copy
    // template / clone-fn template paths below: there is no captured template
    // to clone, so each incarnation gets fresh, unaliased owned values — the
    // structural fix for the byte-copy-template-replay aliasing hazard the
    // checker walled off (E_SUPERVISOR_INIT_ARG_NON_BITCOPY).
    //
    // Ownership/drop contract (the memory-safety crux):
    //  - The thunk returns a fresh, fully-owned state wrapper (`res.state`).
    //  - On thunk OOM (`res.state == null`): fail closed exactly like the
    //    clone-OOM path — apply backoff, leave the slot null, do NOT advance
    //    the circuit breaker. The crash was already counted by `record_restart`.
    //  - On success: ownership of `res.state` transfers to
    //    `hew_actor_spawn_opts_adopt` (no second deep-copy). The new actor's
    //    `state_drop_fn` (registered below) frees its owned fields on the NEXT
    //    crash/teardown. The config buffer is only READ; it is freed once at
    //    supervisor teardown.
    //  - Adopt-failure free-path: `hew_actor_spawn_opts_adopt` libc::free's the
    //    wrapper on failure (it cannot run `state_drop_fn`, so inner owned
    //    fields leak — OOM-only, identical to the existing clone path, tolerated
    //    because spawn-failure here implies system-wide OOM and the supervisor
    //    escalates). The restart still fails closed (null new_child below).
    //    Re-confirmed for the owned config-init thunk (the per-field deep-clone
    //    path): the thunk-produced wrapper now genuinely carries inner owned
    //    heap fields (a cloned `string`/`bytes`), so this leg leaks them on an
    //    adopt OOM — strictly OOM-only, the same bounded leak as the clone
    //    path. Do NOT add a speculative `state_drop_fn` call on this leg: the
    //    wrapper layout the drop fn expects is only guaranteed once adopt has
    //    fully initialised the actor, so dropping a half-adopted wrapper could
    //    double-free. The bounded OOM leak is the correct fail-closed posture.
    let new_child = if let Some(init_fn) = init_fn {
        // SAFETY: `init_fn` is a codegen-emitted thunk matching the
        // `HewChildInitFn` contract; `config` is either null or the
        // supervisor-owned config buffer (alive for the supervisor's lifetime).
        let res = unsafe { init_fn(config.cast_const()) };
        if res.state.is_null() {
            // Thunk OOM: fail closed (mirror the clone-OOM policy exactly).
            apply_restart_backoff(&mut sup.child_specs[index]);
            store_child_slot(sup, index, ptr::null_mut());
            return ptr::null_mut();
        }
        // Build opts around the thunk-produced state and adopt it (no second
        // copy). `init_state`/`state_size` come from the thunk result, NOT the
        // spec template (which is null/0 on the thunk path).
        let thunk_opts = HewActorOpts {
            init_state: res.state,
            state_size: res.size,
            dispatch: opts.dispatch,
            mailbox_capacity: opts.mailbox_capacity,
            overflow: opts.overflow,
            coalesce_key_fn: None,
            coalesce_fallback: HewOverflowPolicy::DropOld as c_int,
            budget: 0,
            arena_cap_bytes: opts.arena_cap_bytes,
            cycle_capable: opts.cycle_capable,
        };
        // SAFETY: thunk_opts is valid; ownership of `res.state` transfers.
        unsafe { actor::hew_actor_spawn_opts_adopt(&raw const thunk_opts, res.state) }
    } else {
        // ── Legacy template paths (no init_fn) ──────────────────────────
        //
        // Kept for the degenerate stateless/legacy case and out-of-tree C ABI
        // callers. Pick the spawn shape based on whether the actor has a
        // registered deep-clone function.
        //
        // **state_clone_fn registered**: call the codegen-emitted clone fn to
        // produce a fresh, independently-owned wrapper from the spec's template,
        // then hand ownership to `hew_actor_spawn_opts_adopt`. This bypasses the
        // legacy `deep_copy_state` byte-copy that aliased owned heap pointers
        // between `spec.init_state` and `actor.state` (audit C1 UAF).
        //
        // **Null-clone-return policy**: when `clone_fn` itself returns null
        // (OOM allocating the new wrapper), we return early **without** calling
        // `circuit_breaker_record_success`. This is critical: a successful
        // restart's clone has to land before the breaker counts the restart as
        // healed, otherwise repeated null-clones would silently close the
        // breaker and mask OOM. The outer `restart_with_budget_and_strategy`
        // already counted this attempt via `record_restart`, so max-restarts /
        // escalation still fire on persistent failure. Backoff is also applied
        // so the supervisor doesn't busy-loop retrying clone fns.
        //
        // **state_clone_fn NOT registered**: fall back to the legacy byte-copy
        // path via `hew_actor_spawn_opts`. The Q185(c) checker remains in
        // defence-in-depth so codegen-emitted actors that should have a clone
        // fn don't silently land on this path.
        if let Some(clone_fn) = state_clone_fn {
            if opts.state_size == 0 || opts.init_state.is_null() {
                // Zero-sized or null template: clone is a no-op; nothing to adopt.
                // Use the legacy path (which also produces a null state for the
                // zero-sized case).
                // SAFETY: opts is valid.
                unsafe { actor::hew_actor_spawn_opts(&raw const opts) }
            } else {
                // SAFETY: spec.init_state is a malloc'd wrapper of `state_size`
                // bytes, replaced by the clone-aware template at registration
                // time. clone_fn matches the HewStateCloneFn contract.
                let cloned = unsafe { clone_fn(opts.init_state.cast_const()) };
                if cloned.is_null() {
                    // Clone OOM: apply backoff, leave slot null, do NOT advance
                    // circuit-breaker success. The crash that triggered this
                    // restart was already counted by `record_restart` at the
                    // outer level.
                    apply_restart_backoff(&mut sup.child_specs[index]);
                    store_child_slot(sup, index, ptr::null_mut());
                    return ptr::null_mut();
                }
                // SAFETY: opts is valid; ownership of `cloned` is transferred.
                unsafe { actor::hew_actor_spawn_opts_adopt(&raw const opts, cloned) }
            }
        } else {
            // Legacy byte-copy path: no `state_clone_fn` registered.
            //
            // SAFETY boundary: this byte-copy is only sound for BitCopy actor state
            // (plain-old-data fields with no owned heap pointers).  If `state_drop_fn`
            // is also set, the template and the spawned actor share heap pointer aliases
            // → double-free on teardown.
            //
            // The checker (E_SUPERVISOR_INIT_ARG_NON_BITCOPY) is the primary authority
            // and rejects owned-handle init args at compile time before this path is
            // reached.  Out-of-tree / hand-rolled C ABI callers that bypass the checker
            // and register `state_drop_fn` without `state_clone_fn` will encounter the
            // use-after-free silently in this path.
            //
            // WHY this is not a debug_assert: the assert would fire in existing tests
            // that probe the legacy byte-copy path directly (see
            // `state_clone_fn_null_falls_back_to_bytecopy`), which are present to
            // document backward compatibility for out-of-tree consumers.
            // WHEN obsolete: when the v0.6 init-closure restart model lands and
            // every supervised actor with owned-heap state registers `state_clone_fn`.
            // REAL FIX: extend the checker wall to cover all paths, then make
            // `state_clone_fn` mandatory for any actor with owned-heap fields.
            //
            // SAFETY: opts is valid.
            unsafe { actor::hew_actor_spawn_opts(&raw const opts) }
        }
    };

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

        // Register the state-clone callback on the actor itself for symmetry
        // and future direct-spawn restart consumers (the supervisor restart
        // path reads the clone fn from the spec, not the actor, but storing
        // it on the actor matches the state_drop_fn pattern).
        if let Some(clone_fn) = state_clone_fn {
            // SAFETY: new_child is valid; clone_fn is a codegen-emitted
            // function with the correct signature.
            unsafe { actor::hew_actor_set_state_clone(new_child, clone_fn) };
        }

        // Record successful restart for circuit breaker.
        let sup_actor_id = supervisor_actor_id(sup);
        circuit_breaker_record_success(&mut sup.child_specs[index], sup_actor_id);

        // Fire the actor's lifecycle wrapper (`init()` / `#[on(start)]`).
        //
        // THE single supervised-lifecycle firing site: both the initial
        // supervised spawn (entered here via `add_child_spec` →
        // `restart_child_from_spec(index == child_count)`) and every
        // supervisor-triggered restart (entered via
        // `restart_with_budget_and_strategy`) flow through this one call, so a
        // supervised actor runs its init/on_start exactly once per incarnation
        // — behaviourally identical to a directly-spawned actor at birth.
        //
        // Fired AFTER state_drop/clone registration and BEFORE
        // `store_child_slot`: the slot store is the visibility edge that makes
        // the child reachable via `sup.<name>`, so init/on_start complete
        // before any external code can message the child (matching the
        // direct-spawn invariant that lifecycle runs before the spawn
        // destination pointer is stored).
        //
        // The wrapper itself acquires the actor's state lock; no lock on
        // `new_child` is held here (the clone path operates on the spec
        // template, not the new actor's lock), so there is no re-entrancy.
        if let Some(lifecycle_fn) = lifecycle_fn {
            // SAFETY: new_child was just spawned and is valid; lifecycle_fn is a
            // codegen-emitted C-ABI wrapper matching the HewLifecycleFn contract.
            unsafe { lifecycle_fn(new_child) };
        }
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
        // Observability (AFTER the max-restart-intensity decision, BEFORE the
        // escalate): record that the budget was exhausted, carrying the
        // within-window restart count.
        crate::tracing::record_supervisor_event(
            supervisor_actor_id(sup),
            crate::tracing::SPAN_SUPERVISOR_MAX_RESTARTS,
            recent,
        );
        stop_and_maybe_escalate(sup);
        return;
    }

    record_restart(sup);
    crate::observe::record_actor_restart();
    // Observability (AFTER the restart decision is taken): record the restart,
    // carrying the restart strategy in the discriminator. Read-only side
    // effect; never gates control flow.
    crate::tracing::record_supervisor_event(
        supervisor_actor_id(sup),
        crate::tracing::SPAN_SUPERVISOR_RESTART,
        sup.strategy,
    );

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
            spawn_deferred_restart_free(deferred);
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
            spawn_deferred_restart_free(deferred);
            for i in failed_index..sup.child_count {
                // SAFETY: index is valid.
                unsafe { restart_child_from_spec(sup, i) };
            }
        }
        STRATEGY_SIMPLE_ONE_FOR_ONE => {
            // Static-backed pool: each pool member is an independent static child
            // in `children[]` (registered via `pool_member_add_static`), so the
            // crashed member restarts per-member exactly like ONE_FOR_ONE — the
            // members are fungible and independent, never a one-for-all group.
            // `restart_child_from_spec` re-runs the member's init thunk (fresh
            // config-derived state per incarnation) and `store_child_slot` re-fills
            // `children[failed_index]`; the pool accessor
            // (`hew_supervisor_pool_child_get`) resolves member i through that LIVE
            // static slot, so the restarted member is re-resolved automatically
            // with no stale PID cached (LESSONS
            // `replaceable-resource-handle-is-fungible-reference`).
            // SAFETY: index is valid (bounds-checked at the top of this fn).
            unsafe { restart_child_from_spec(sup, failed_index) };
        }
        unknown => {
            // Fail-closed: any non-listed strategy is a codegen/runtime ABI
            // drift. Pre-S-D this fell through a `_ => {}` wildcard, which
            // silently dropped restart requests for unrecognized strategies.
            unreachable!(
                "hew_supervisor: unknown restart strategy {unknown}; \
                 valid: ONE_FOR_ONE=0, ONE_FOR_ALL=1, REST_FOR_ONE=2, \
                 SIMPLE_ONE_FOR_ONE=3"
            );
        }
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
        // Observability (AFTER the max-restart-intensity decision): record
        // budget exhaustion on the child-supervisor recovery path too.
        crate::tracing::record_supervisor_event(
            supervisor_actor_id(sup),
            crate::tracing::SPAN_SUPERVISOR_MAX_RESTARTS,
            recent,
        );
        stop_and_maybe_escalate(sup);
        return;
    }

    record_restart(sup);
    crate::observe::record_actor_restart();
    // Observability (AFTER the restart decision): record the child-supervisor
    // subtree restart, carrying the strategy discriminator.
    crate::tracing::record_supervisor_event(
        supervisor_actor_id(sup),
        crate::tracing::SPAN_SUPERVISOR_RESTART,
        sup.strategy,
    );

    // SAFETY: `failed_index` is validated above and `sup` is the live parent
    // supervisor whose child-supervisor slot we are replacing.
    if unsafe { restart_child_supervisor_from_spec(sup, failed_index) }.is_null() {
        stop_and_maybe_escalate(sup);
        return;
    }

    notify_restart(sup);
}

/// Invoke a child's `#[on(crash)]` handler (if installed) and return its
/// `CrashAction` decision as an i32 tag, or `None` when no handler is installed.
///
/// The handler receives the crash code (widened to i64), a trap-kind diagnostic
/// message (e.g. "HeapExceeded"/"Signal"), and the child's template seed-state
/// pointer.
///
/// String-ABI contract (M-5): `crash_message` is typed `string` on the Hew side
/// (`CrashInfo.message`), so it MUST be a Hew header-aware allocation. The codegen
/// prologue CLONES it (`hew_string_clone`, a refcount bump) into the owned
/// `CrashInfo.message` field; both `hew_string_clone` and the hook's `CrashInfo`
/// drop (`hew_string_drop`) read the 16-byte header at `data -
/// CSTRING_HEADER_SIZE`. A bare Rust `CString` carries no header, so those
/// primitives OOB-read and `abort()` — the reported M-5 critical bug. We therefore
/// allocate the message through the Hew string allocator (`str_to_malloc`, rc==1)
/// and the supervisor REMAINS the owner of that original: it frees it via
/// `free_cstring` after the call. The hook's clone is an independent `+1` owner
/// released by the hook's own `CrashInfo` drop, so the two releases balance to a
/// single free with no double-free — eliminating the abort/heap-corruption the
/// pre-fix headerless-`CString` + move-of-borrow produced on every real crash.
///
/// (Known follow-up, not the reported critical bug: when the hook BODY reads
/// `info.message` via a borrowing call, the codegen field-read `hew_string_clone`
/// retain temp is not yet released by drop-elaboration for the synthetic-prologue
/// shape, a small per-crash `string` leak tracked separately. No abort, no
/// double-free, no corruption — the fail-closed crash path stays safe.)
///
/// # Safety
///
/// `handler` (when `Some`) must be a valid `HewOnCrashFn` fn-pointer; `ctx` must
/// be the live execution context for the in-flight supervisor dispatch;
/// `state_ptr` must be the child's supervisor-owned template state.
unsafe fn invoke_on_crash_handler(
    handler: Option<HewOnCrashFn>,
    state_ptr: *mut c_void,
    crash_code: c_int,
    ctx: *mut crate::execution_context::HewExecutionContext,
) -> Option<i32> {
    let handler = handler?;
    // Allocate the trap-kind message as a Hew header-aware string (rc == 1) so
    // the handler's `hew_string_clone` ingress and `CrashInfo` drop operate on a
    // valid refcount header. `trap_kind_name` is a non-empty `&'static str`, so
    // `str_to_malloc` only returns null on allocation failure; pass null through
    // (the codegen clone/drop are null-safe).
    let crash_message: *mut c_char =
        crate::cabi::str_to_malloc(ExitReason::from_error_code(crash_code).trap_kind_name());
    // Widen crash_code from c_int to i64 at the call boundary. `HewOnCrashFn`
    // uses i64 to match `CrashInfo.code: i64` in std/failure.hew.
    #[allow(
        clippy::cast_lossless,
        reason = "c_int to i64: intentional widening to match HewOnCrashFn ABI"
    )]
    let crash_code_i64 = crash_code as i64;
    // SAFETY: `handler` is a valid `HewOnCrashFn`; `ctx` is the live execution
    // context; `state_ptr` is the child's supervisor-owned template state;
    // `crash_message` is a Hew header-aware allocation (or null), owned by this
    // frame and live across the call. The hook clones it into its own owner.
    let action = unsafe { handler(ctx, crash_code_i64, crash_message, state_ptr) };
    let tag = action.tag_i32();
    // Release the supervisor's original owner of the header-aware message. The
    // hook cloned (retained) it into `CrashInfo.message` and released that owner
    // on return, so this brings the refcount to zero and frees the buffer exactly
    // once. `free_cstring` is null-safe.
    if !crash_message.is_null() {
        // SAFETY: `crash_message` came from `str_to_malloc` (header-aware) and is
        // not null; the only other owner (the hook's clone) was already released.
        unsafe { crate::cabi::free_cstring(crash_message) };
    }
    Some(tag)
}

/// Apply the restart strategy after a child failure.
///
/// # Safety
///
/// `sup` must be valid.
unsafe fn apply_restart(
    sup: &mut HewSupervisor,
    failed_index: usize,
    exit_state: c_int,
    crash_code: c_int,
    ctx: *mut crate::execution_context::HewExecutionContext,
) {
    // Capture the supervisor's actor id before borrowing `child_specs` so the
    // emission helpers can attribute events without re-borrowing `sup`.
    let sup_actor_id = supervisor_actor_id(sup);
    let spec = &mut sup.child_specs[failed_index];

    // Record crash if it was a crash (not a normal stop). `crash_code` is the
    // real trap-kind integer (`HEW_TRAP_*`, 201–205) captured from the child
    // actor's `error_code` slot at the moment of the trap; if no specific code
    // is available it is `0` (unknown). Historically this site passed the
    // literal `11` (a SIGSEGV stand-in) which made crash-stats lie about why
    // the actor died — we plumb the real value through now.
    if exit_state == HewActorState::Crashed as c_int {
        circuit_breaker_record_crash(spec, crash_code, sup_actor_id);

        // ── on(crash) handler invocation ─────────────────────────────────
        //
        // If the user declared `#[on(crash)]` on the child actor, codegen
        // populated `spec.on_crash` (S1 ABI slot + S2 MIR symbol + S3
        // codegen pointer); fire the handler now, BEFORE the restart-policy
        // gate. The handler runs in the supervisor's own actor-dispatch
        // context (`ctx`), which is the same context the surrounding
        // `supervisor_dispatch` is executing under. The crashed child's
        // actor was already torn down by `take_child_slot` in
        // `supervisor_dispatch`, so there is no risk of re-entering the
        // crashed dispatch.
        //
        // Arguments:
        //   - `ctx`: supervisor's execution context, threaded down from
        //     `supervisor_dispatch`. Re-using the supervisor ctx
        //     preserves task-scope cancellation propagation; do not
        //     synthesise a fresh ctx (cf. `f4df6354`).
        //   - `crash_code`: the trap-kind integer captured above. Cast to
        //     `c_int` for the C ABI; the handler receives a single i32
        //     register matching the MIR lowering of `PanicInfo { code: i64 }`
        //     (`hew-types/src/check/items.rs:887-906`).
        //   - `spec.init_state`: pointer to the *new* (template) seed state
        //     the next-restarted actor will be cloned from. The crashed
        //     actor's last-seen state is gone with the actor. Handler-side
        //     mutations to this pointer therefore persist across EVERY
        //     subsequent restart of this child slot, not just the next
        //     one. v0.6 may revisit ("last-seen-state" semantics, fresh
        //     template clone per crash) once the design is ratified.
        //
        // Fail-closed mechanism:
        //   - The handler ABI is `extern "C"` (non-unwinding). If the
        //     handler itself traps via `hew_trap_with_code`, the longjmp
        //     unwinds into the SUPERVISOR's recovery frame (we are inside
        //     the supervisor's dispatch), which surfaces as a supervisor-
        //     level crash that the parent restart-budget escalates per
        //     `restart_within_window`. We deliberately do NOT wrap this
        //     call in `std::panic::catch_unwind` — there is nothing to
        //     catch across a non-unwinding ABI, and silently swallowing
        //     would violate the `boundary-fail-closed` invariant.
        //
        // `CrashAction` return channel (M-3 ABI) + honouring (M-4):
        //   `std/failure.hew` declares `#[on(crash)]` returning a
        //   `CrashAction` variant (Restart/Escalate/Kill). The `HewOnCrashFn`
        //   ABI carries that decision back; the supervisor now HONOURS it
        //   (below) — the hook's at-crash-time decision takes precedence over
        //   the static `restart_policy`. `None` when no hook is installed (the
        //   policy alone decides). SAFETY: `ctx` is the live supervisor
        //   execution context for the in-flight dispatch.
        let crash_action_tag =
            unsafe { invoke_on_crash_handler(spec.on_crash, spec.init_state, crash_code, ctx) };

        // ── Honour the hook's CrashAction return (M-4) ───────────────────
        //
        // The hook return takes PRECEDENCE over the static `restart_policy`:
        // it is the at-crash-time decision; the policy is the registration
        // default. `Escalate`/`Kill` short-circuit the restart path entirely;
        // `Restart` (and any out-of-range tag, fail-closed) falls through to
        // the existing policy/budget machinery below.
        match crash_action_tag {
            Some(CRASH_ACTION_KILL) => {
                // Terminate permanently: null the slot, do not restart.
                // Mirrors the RESTART_TEMPORARY terminal arm.
                store_child_slot(sup, failed_index, ptr::null_mut());
                return;
            }
            Some(CRASH_ACTION_ESCALATE) => {
                // Propagate the failure to the supervisor's supervisor instead
                // of restarting locally.
                escalate_to_parent(sup);
                return;
            }
            // Some(CRASH_ACTION_RESTART) or any out-of-range tag (fail-closed
            // to Restart) or None (no hook): fall through to the existing
            // restart-policy + budget path below.
            _ => {}
        }

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
    if !circuit_breaker_should_restart(spec, sup_actor_id) {
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
        // Observability (AFTER the backoff-schedule decision, BEFORE spawning
        // the timer): record that a delayed restart was scheduled, carrying the
        // delay in ms (saturated to i32). Read-only; never gates control flow.
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_BACKOFF,
            i32::try_from(delay_ms).unwrap_or(i32::MAX),
        );
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
/// The supervisor's `HewDispatchFn` trampoline. D-A.2: the dispatch ABI returns
/// a nullable suspend handle; the supervisor is an internal copy-mode actor that
/// never suspends, so it always returns `null` (run-to-completion). The dispatch
/// logic lives in `supervisor_dispatch_impl` (which keeps the early-`return`
/// control flow); this thin wrapper threads the run-to-completion null handle.
unsafe extern "C-unwind" fn supervisor_dispatch(
    ctx: *mut crate::execution_context::HewExecutionContext,
    state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    borrow_mode: i32,
) -> *mut c_void {
    // SAFETY: forwards the caller's invariants unchanged to the impl.
    unsafe { supervisor_dispatch_impl(ctx, state, msg_type, data, data_size, borrow_mode) };
    std::ptr::null_mut()
}

unsafe fn supervisor_dispatch_impl(
    ctx: *mut crate::execution_context::HewExecutionContext,
    state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    // P5-RX sub-stage 1: copy-vs-borrow receipt discriminant (HewDispatchFn).
    // The supervisor is an internal copy-mode actor; always 0, ignored.
    _borrow_mode: i32,
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

            // TRACE-CONTEXT COMPLETENESS (S3, crash-recovery seam): a crash is
            // reported from `hew_actor_trap` (signal-handler context), so the
            // sys-message that woke this dispatch may carry an all-zero trace
            // context. Establish a sampled root HERE — in normal
            // supervisor-dispatch context, never in the trap — so the restart /
            // escalate / circuit spans emitted below (S2) parent under a real,
            // sampled trace id instead of an unsampled zero-parent fallback.
            crate::tracing::ensure_supervisor_trace_root();

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

            // SAFETY: sup is valid; ctx is the supervisor's own dispatch
            // context, threaded through so a registered on_crash handler
            // receives the supervisor's ctx (preserves task-scope
            // cancellation propagation per f4df6354).
            unsafe { apply_restart(sup, idx, event.exit_state, event.crash_code, ctx) };
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
                // S3: establish a sampled root for the delayed-restart span too
                // (this dispatch was woken by a timer-thread sys-send, which may
                // carry a zero trace context).
                crate::tracing::ensure_supervisor_trace_root();
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
        restart_await_waiters: Mutex::new(Vec::new()),
        pool_slots: Vec::new(),
        pool_specs: Vec::new(),
        config_buf: ptr::null_mut(),
        config_size: 0,
        config_drop_fn: None,
    });
    Box::into_raw(sup) // ALLOCATOR-PAIRING: GlobalAlloc
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

    // The v0.6 init-closure restart model: when the spec carries an `init_fn`,
    // the thunk is THE state source on the initial spawn and every restart.
    // Skip the byte-copy state template entirely — `restart_child_from_spec`
    // ignores `init_state` on the thunk path, and capturing a template here
    // would re-introduce the owned-field aliasing hazard the thunk model fixes.
    let has_init_fn = sp.init_fn.is_some();

    // Adopt the supervisor's construction-time config buffer on the first
    // init_fn child that carries one. The buffer is supervisor-owned, shared
    // by borrow with every thunk, and freed once at teardown. Re-registration
    // with the same pointer is idempotent; a conflicting non-null pointer is an
    // ABI error (codegen emits one config buffer per supervisor).
    if has_init_fn && !sp.config.is_null() {
        if s.config_buf.is_null() {
            s.config_buf = sp.config;
            s.config_size = sp.config_size;
        } else if s.config_buf != sp.config {
            // Conflicting non-null config pointer: a SECOND, different buffer.
            // Correct codegen emits ONE config buffer per supervisor (every
            // config child carries the same pointer), so this is unreachable
            // from in-tree codegen — debug_assert catches a codegen bug loudly.
            // In release, fail closed by FREEing the rejected duplicate (it will
            // never be adopted, so leaking it would be the only alternative).
            // Safe: the conflict branch only fires for a pointer that is NOT the
            // adopted buffer, so this cannot double-free the supervisor's buffer.
            debug_assert!(
                false,
                "hew_supervisor: child {i} carries a config buffer ({:p}) that differs from \
                 the supervisor's adopted buffer ({:p}); codegen must emit ONE config buffer \
                 per supervisor",
                sp.config, s.config_buf
            );
            // SAFETY: sp.config is a libc::malloc'd buffer (ALLOCATOR-PAIRING:
            // libc) distinct from the adopted s.config_buf; freeing the orphan
            // rejected duplicate is sound.
            unsafe { libc::free(sp.config) };
        }
    }

    // Deep-copy init state — only when there is no init_fn (the thunk path
    // produces state directly, leaving init_state null).
    let state_copy = if !has_init_fn && sp.init_state_size > 0 && !sp.init_state.is_null() {
        // SAFETY: init_state is valid for init_state_size bytes.
        let buf = unsafe { libc::malloc(sp.init_state_size) }; // ALLOCATOR-PAIRING: libc
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
        // On the thunk path the state size is produced by the thunk result, not
        // the spec; keep it 0 so no template path can read a stale size.
        init_state_size: if has_init_fn { 0 } else { sp.init_state_size },
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        cycle_capable: sp.cycle_capable,
        on_crash: sp.on_crash,
        // Registered by hew_supervisor_set_child_state_drop after this call.
        state_drop_fn: None,
        // Registered by hew_supervisor_set_child_state_clone after this call.
        state_clone_fn: None,
        // Carried IN the spec literal (like on_crash) so the initial supervised
        // spawn — which happens inside this call via restart_child_from_spec —
        // fires the lifecycle wrapper. A post-hoc setter would run too late to
        // cover the initial fire (see hew_supervisor_set_child_lifecycle).
        lifecycle_fn: sp.lifecycle_fn,
        // Carried IN the spec literal (like lifecycle_fn) so the INITIAL spawn
        // below uses the thunk — the load-bearing first-spawn carrier. The
        // post-hoc setter is back-fill/symmetry only.
        init_fn: sp.init_fn,
        // Borrow of the supervisor-owned config buffer (adopted above).
        config: if has_init_fn {
            s.config_buf
        } else {
            ptr::null_mut()
        },
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
            libc::free((*self_actor).state); // ALLOCATOR-PAIRING: libc
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
    crash_code: c_int,
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
        crash_code,
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
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

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

    unsafe extern "C-unwind" fn noop_child_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
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
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
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
        // Install a runtime so the live-actor registry resolves; held for the
        // whole test (it serializes actor-freeing tests on the shared lock).
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree and only mutates the
        // current actor context within the test thread.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            // Probe liveness by (id, ptr): sibling tests in this process spawn
            // actors concurrently, and a recycled allocation address would make
            // a pointer-only probe report the freed actor as live again (ABA).
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            (*child)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: child_id,
                ..HewExecutionContext::default()
            });
            let unblock = defer_state_transition(
                child,
                HewActorState::Stopped,
                std::time::Duration::from_millis(200),
            );

            let start = std::time::Instant::now();
            hew_supervisor_stop(sup);
            let elapsed = start.elapsed();

            unblock.join().unwrap();

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "child actor should be freed asynchronously after deferred supervisor stop"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(self_id, self_actor)
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
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree and simulates a reentrant
        // terminate callback by controlling the child actor state directly.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            // Probe liveness by (id, ptr) — see
            // stop_supervisor_from_child_dispatch_is_deferred for the ABA
            // rationale.
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: child_id,
                ..HewExecutionContext::default()
            });
            let start = std::time::Instant::now();
            hew_supervisor_stop(sup);
            let elapsed = start.elapsed();

            child_ref.terminate_finished.store(true, Ordering::Release);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "child should be released after deferred supervisor stop"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(self_id, self_actor)
                }),
                "supervisor self actor should be released after deferred stop"
            );

            assert!(
                elapsed < std::time::Duration::from_secs(1),
                "reentrant supervisor stop should defer instead of spinning inside terminate, took {elapsed:?}"
            );
        }
    }

    /// Serializes the deferred-teardown join-barrier tests. They share the
    /// process-global `DEFERRED_TEARDOWN_THREADS` registry and each holds its
    /// teardown open with a gated terminate; running two of them concurrently
    /// would let one test's `drain_deferred_teardown_threads` steal and join
    /// the other's still-gated handle, so the victim's own drain observes an
    /// empty registry and asserts before the stolen teardown frees its actor.
    /// Production only ever drains once, single-threaded, in
    /// `cleanup_all_actors`; this lock restores that precondition for the tests.
    static TEARDOWN_DRAIN_SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn drain_deferred_teardown_joins_in_flight_supervisor_stop() {
        let _rt = crate::runtime_test_guard();
        let _serial = TEARDOWN_DRAIN_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // The deferred-sup-stop thread dereferences the supervisor's child and
        // self actors for the whole teardown. `cleanup_all_actors` joins the
        // registered teardown threads before sweeping `LIVE_ACTORS`; this test
        // exercises that join barrier directly. The teardown is held open
        // across the drain call by an unfinished child terminate, so a drain
        // that does not join the deferred-sup-stop thread returns while the
        // supervisor self actor is still tracked.
        // SAFETY: this test owns the supervisor tree and gates the teardown
        // through test-controlled atomics, mirroring the reentrant-terminate
        // test above.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: child_id,
                ..HewExecutionContext::default()
            });
            // Deferred path: the current thread owns the supervisor tree.
            hew_supervisor_stop(sup);

            // Release the gated terminate while the drain below is joining the
            // teardown thread. The store always happens-before the child's
            // allocation is freed: the teardown thread blocks on
            // `terminate_finished` before reclaiming the child.
            let child_addr = child as usize;
            let release = std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_millis(100));
                // SAFETY: the teardown thread cannot free the child before
                // observing this store (see comment above).
                (*(child_addr as *mut HewActor))
                    .terminate_finished
                    .store(true, Ordering::Release);
            });

            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            // The join barrier guarantees the teardown finished — no polling.
            assert!(
                !actor::is_actor_live_with_id(child_id, child),
                "joined teardown must have released the child actor"
            );
            assert!(
                !actor::is_actor_live_with_id(self_id, self_actor),
                "joined teardown must have released the supervisor self actor"
            );

            release.join().unwrap();
        }
    }

    #[test]
    fn drain_deferred_teardown_joins_in_flight_restart_free() {
        let _rt = crate::runtime_test_guard();
        let _serial = TEARDOWN_DRAIN_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // The ONE_FOR_ALL / REST_FOR_ONE restart arms free stopped siblings on
        // a background "deferred-free" thread that runs
        // `hew_actor_free_for_restart` on actors still tracked in
        // `LIVE_ACTORS`. `cleanup_all_actors` must join that teardown before
        // sweeping the registry, or the sweep races the in-flight free into a
        // use-after-free / double-free. This drives the production restart spawn
        // helper directly and holds the free open across the drain via a gated
        // terminate, so a drain that does NOT join the deferred-free thread
        // (the pre-fix detached spawn) returns while the sibling is still
        // tracked and the assertion fails.
        // SAFETY: this test owns the supervisor tree and gates the teardown
        // through test-controlled atomics, mirroring the supervisor-stop
        // variant above.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;

            // Detach the sibling from the supervisor exactly as the restart
            // arms do via `take_child_slot`, then drive it to a quiescent
            // terminal state and gate its terminate open.
            let taken = take_child_slot(&mut *sup, 0);
            assert_eq!(taken, child);
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            // Production restart teardown spawn + registration.
            spawn_deferred_restart_free(vec![DeferredFree(child)]);

            // Release the gated terminate while the drain below is joining the
            // deferred-free thread. The teardown blocks in
            // `free_actor_resources_with_options` on `terminate_finished`
            // before reclaiming the sibling, so this store happens-before the
            // free.
            let child_addr = child as usize;
            let release = std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_millis(100));
                // SAFETY: the teardown thread cannot free the sibling before
                // observing this store (see comment above).
                (*(child_addr as *mut HewActor))
                    .terminate_finished
                    .store(true, Ordering::Release);
            });

            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            // The join barrier guarantees the teardown finished — no polling.
            assert!(
                !actor::is_actor_live_with_id(child_id, child),
                "joined restart teardown must have released the stopped sibling"
            );

            release.join().unwrap();
        }
    }

    #[test]
    fn concurrent_second_stop_returns_while_deferred_owner_waits() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree, injects a synthetic
        // current actor for the owner-thread path, and only mutates actor
        // states through test-controlled atomics.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            // Probe liveness by (id, ptr) — see
            // stop_supervisor_from_child_dispatch_is_deferred for the ABA
            // rationale.
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            let child_sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!child_sup.is_null());
            assert_eq!(hew_supervisor_add_child_supervisor(sup, child_sup), 0);

            (*child)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);
            (*self_actor)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: (*child).id,
                ..HewExecutionContext::default()
            });
            // Deferred-teardown windows widened 10x (200/250ms -> 2000/2500ms)
            // so a second stop returning under full-suite CI load still lands
            // well inside the window the deferred teardown owns the tree. This
            // asserts real thread scheduling, which the in-process simtime seam
            // can't fake; the deterministic fix (gate the transition on a
            // released signal, assert ordering not wall-clock) is the v0.5.5
            // de-flake (#39). The relative invariant below is unchanged.
            let self_unblock = defer_state_transition(
                self_actor,
                HewActorState::Stopped,
                std::time::Duration::from_secs(2),
            );
            let child_unblock = defer_state_transition(
                child,
                HewActorState::Stopped,
                std::time::Duration::from_millis(2_500),
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
                wait_for_condition(std::time::Duration::from_secs(5), || {
                    finished.load(Ordering::Acquire)
                }),
                "second stop caller should return while deferred teardown owns the supervisor"
            );
            assert!(
                elapsed_ms.load(Ordering::Acquire) < 1_000,
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

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "child actor should still be released after the deferred winner completes"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(self_id, self_actor)
                }),
                "supervisor self actor should still be released after the deferred winner completes"
            );
        }
    }

    #[test]
    fn failed_deferred_spawn_keeps_supervisor_registered() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree, injects the current actor
        // to exercise the owner-thread path, and then performs synchronous
        // cleanup once the injected spawn failure has been asserted.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let fail_guard = fail_owned_deferred_supervisor_spawn();

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: (*child).id,
                ..HewExecutionContext::default()
            });
            crate::hew_clear_error();
            hew_supervisor_stop(sup);

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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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

    // ── await_restart cooperative observer ───────────────────────────────────

    /// Pre-park check (R4): a Live child returns READY — no park, the waiter
    /// list stays empty. The caller binds immediately instead of suspending.
    #[test]
    fn restart_await_suspend_live_child_returns_ready_no_park() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let slot = crate::read_slot::hew_read_slot_new();
            let actor = ptr::null_mut();

            let rc = hew_supervisor_restart_await_suspend(sup, 0, actor, slot);

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a Live child must return READY (no park)"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "READY path must not register a waiter"
            );

            // The caller still owns the creator ref on a READY return.
            crate::read_slot::hew_read_slot_free(slot);
            hew_supervisor_stop(sup);
        }
    }

    /// Pre-park check (R4 fail-closed): a permanently-Dead child (supervisor
    /// shut down) returns READY rather than parking forever. The resumed caller
    /// fails closed at the send re-resolve.
    #[test]
    fn restart_await_suspend_dead_child_returns_ready_never_hangs() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            // Force shutdown so child_get classifies the slot as Dead.
            (*sup).running.store(0, Ordering::Release);
            let slot = crate::read_slot::hew_read_slot_new();

            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a permanently-Dead child must return READY (fail closed, never hang)"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "the Dead fail-closed path must not register a waiter"
            );

            crate::read_slot::hew_read_slot_free(slot);
            // Restore so teardown can reach the actor.
            (*sup).running.store(1, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// A Transient slot (mid-restart) parks: SUSPEND is returned and exactly one
    /// waiter is registered. `notify_restart` (via wake) then drains the waiter
    /// list — the resume-contract anchor (`store_child_slot` before notify).
    #[test]
    fn restart_await_suspend_transient_parks_then_notify_drains() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; manually nulls the slot to
        // simulate the restart-in-progress window, then restores it.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            // Null the slot under lock → child_get returns Transient(Restarting).
            store_child_slot(&mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            // A null actor is fine: the wake path re-validates via enqueue_resume,
            // which drops a null/dead actor's wake with no deref.
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);

            assert_eq!(
                rc, RESTART_AWAIT_SUSPEND,
                "a Transient child must park (SUSPEND)"
            );
            assert_eq!(
                (*sup).restart_await_waiters.lock_or_recover().len(),
                1,
                "the park path must register exactly one waiter"
            );

            // Restore the slot (the restart completed) and fire the notify wake.
            store_child_slot(&mut *sup, 0, child);
            notify_restart(&*sup);

            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "notify_restart must drain every parked waiter"
            );
            // notify drained + freed the observer's retained ref; the slot is gone.

            hew_supervisor_stop(sup);
        }
    }

    /// The abandon edge: detach removes the waiter and releases its ref, so a
    /// later `notify_restart` finds nothing to wake (no double-free, no leak).
    #[test]
    fn restart_await_detach_removes_waiter_before_notify() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);
            assert_eq!(rc, RESTART_AWAIT_SUSPEND);
            assert_eq!((*sup).restart_await_waiters.lock_or_recover().len(), 1);

            // Abandon: detach removes the waiter and releases the retained ref.
            hew_supervisor_restart_await_detach(sup, slot);
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "detach must remove the waiter"
            );

            // A later notify has nothing to wake (the waiter is gone).
            store_child_slot(&mut *sup, 0, child);
            notify_restart(&*sup);
            assert!((*sup).restart_await_waiters.lock_or_recover().is_empty());

            hew_supervisor_stop(sup);
        }
    }

    /// Guard that clears the park-gap hook on drop so a panicking test cannot
    /// leave the process-global hook installed for a sibling test.
    struct RestartAwaitParkGapHookGuard;

    impl Drop for RestartAwaitParkGapHookGuard {
        fn drop(&mut self) {
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = None;
        }
    }

    /// Lost-wakeup race regression (the B1-surface concurrency blocker): the
    /// default scheduler is multi-worker, so a restart cycle can complete
    /// (`store_child_slot` + `notify_restart`) in the gap between the pre-park
    /// `child_get` and the waiter push. Without the under-lock counter recheck the
    /// awaiting actor registers its waiter AFTER the drain already ran against an
    /// empty registry, so the wake is lost and the continuation parks forever.
    ///
    /// This drives the restart through the test-only park-gap hook so the racing
    /// `notify_restart` lands in exactly that window, deterministically. WITH the
    /// fix the awaiting call observes the advanced counter and resolves READY with
    /// no orphaned waiter; WITHOUT the fix it returns SUSPEND and leaves a waiter
    /// that nothing will ever drain (verified: removing the recheck makes the two
    /// assertions below fail — `rc` is SUSPEND and the registry holds one waiter).
    #[test]
    fn restart_await_suspend_notify_in_park_gap_does_not_lose_wakeup() {
        let _rt = crate::runtime_test_guard();
        let _hook_guard = RestartAwaitParkGapHookGuard;
        // SAFETY: the test owns the supervisor tree for its whole lifetime.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            // Null the slot so the pre-park `child_get` classifies it Transient
            // and the awaiting call proceeds toward parking.
            store_child_slot(&mut *sup, 0, ptr::null_mut());

            // The racing restart, fired from the gap hook: restore the slot to
            // Live and complete the restart cycle (bump counter + drain waiters).
            // At this point the awaiting call has NOT yet pushed its waiter, so the
            // drain sees an empty registry — the exact lost-wakeup interleaving.
            let sup_addr = sup as usize;
            let child_addr = child as usize;
            let hook: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
                // SAFETY: the test keeps `sup`/`child` alive until after the
                // awaiting call returns; the hook runs synchronously within it.
                let sup = sup_addr as *mut HewSupervisor;
                store_child_slot(&mut *sup, 0, child_addr as *mut HewActor);
                notify_restart(&*sup);
            });
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = Some(hook);

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);

            // Disarm the hook before any further restart machinery runs.
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = None;

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a restart completing in the park gap must resolve READY, not park \
                 against a wake that already fired (lost-wakeup race)"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "the lost-wakeup recheck must NOT register an orphaned waiter that \
                 nothing will ever drain"
            );

            // READY keeps the creator ref with the caller; free it here.
            crate::read_slot::hew_read_slot_free(slot);
            hew_supervisor_stop(sup);
        }
    }

    /// The same lost-wakeup interleaving, but with `notify_restart` fired from a
    /// SEPARATE worker thread (the realistic multi-worker shape) while the
    /// awaiting actor is paused in the park gap. A bounded join backstops the
    /// teeth: WITHOUT the fix the awaiting call parks against an already-fired,
    /// drained-empty wake and the spawned awaiting thread never completes — the
    /// join times out (an observable hang). WITH the fix it resolves READY and
    /// the thread joins promptly.
    #[test]
    fn restart_await_suspend_concurrent_notify_in_gap_wakes_then_joins() {
        let _rt = crate::runtime_test_guard();
        let _hook_guard = RestartAwaitParkGapHookGuard;
        // SAFETY: the test owns the supervisor tree for its whole lifetime.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&mut *sup, 0, ptr::null_mut());

            // Two barriers coordinate the cross-thread interleaving precisely:
            //  - `in_gap` releases the notifier once the awaiting thread is in the
            //    park gap (post pre-park check, pre push);
            //  - `notified` blocks the awaiting thread until the notifier's restart
            //    cycle (bump + drain-empty) has fully completed.
            let in_gap = Arc::new(std::sync::Barrier::new(2));
            let notified = Arc::new(std::sync::Barrier::new(2));
            let in_gap_hook = Arc::clone(&in_gap);
            let notified_hook = Arc::clone(&notified);
            let hook: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
                // Signal the notifier that we are parked in the gap, then wait for
                // it to finish the racing restart cycle before we proceed to push.
                in_gap_hook.wait();
                notified_hook.wait();
            });
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = Some(hook);

            // Awaiting actor: runs the suspend call on its own thread.
            let sup_addr = sup as usize;
            let awaiting = std::thread::spawn(move || {
                // SAFETY: the parent keeps `sup` alive until this thread joins; the
                // slot is created and freed within this thread.
                let sup = sup_addr as *mut HewSupervisor;
                let slot = crate::read_slot::hew_read_slot_new();
                let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);
                crate::read_slot::hew_read_slot_free(slot);
                rc
            });

            // Notifier: once the awaiting thread is in the gap, drive the racing
            // restart cycle (restore Live + bump counter + drain the still-empty
            // registry), then release the awaiting thread to proceed to its push.
            in_gap.wait();
            store_child_slot(&mut *sup, 0, child);
            notify_restart(&*sup);
            notified.wait();

            // Bounded teeth: poll for the awaiting thread to finish. WITHOUT the
            // recheck the awaiting call returns SUSPEND with an orphaned waiter and
            // (in a real run) the continuation never wakes; here the thread still
            // finishes (it returns SUSPEND rather than parking a real coroutine),
            // so the verdict is the rc + empty-registry assertion below, while this
            // bounded wait guarantees the test itself never hangs.
            let joined =
                wait_for_condition(std::time::Duration::from_secs(5), || awaiting.is_finished());
            assert!(
                joined,
                "awaiting thread must finish — a lost wakeup would hang it"
            );
            let rc = awaiting.join().expect("awaiting thread panicked");

            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = None;

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a concurrent notify in the park gap must resolve the awaiting actor \
                 READY, not leave it parked against an already-fired wake"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "no orphaned waiter may survive the racing restart"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// The contextless blocking helper returns (does NOT hang) for a healthy
    /// Live child with no restart in flight — the grace-window over-wait guard.
    #[test]
    fn restart_await_blocking_live_no_restart_returns_within_grace() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let start = std::time::Instant::now();

            hew_supervisor_restart_await_blocking(sup, 0);

            // Returned within a small multiple of the 250ms grace window — never
            // an infinite hang.
            assert!(
                start.elapsed() < std::time::Duration::from_secs(2),
                "blocking await on a healthy Live child must return via the grace \
                 window, not hang"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// The contextless blocking helper returns immediately for a permanently
    /// Dead child (shut-down supervisor) — R4 fail-closed, no hang.
    #[test]
    fn restart_await_blocking_dead_child_returns_immediately() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            (*sup).running.store(0, Ordering::Release);
            let start = std::time::Instant::now();

            hew_supervisor_restart_await_blocking(sup, 0);

            assert!(
                start.elapsed() < std::time::Duration::from_millis(100),
                "a permanently-Dead child must return immediately, not block"
            );

            (*sup).running.store(1, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// When the circuit breaker is OPEN (state == 1), a null slot returns
    /// Transient(CircuitOpen).
    #[test]
    fn child_get_circuit_open_null_slot_returns_transient_circuit_open() {
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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

    // ── state_clone_fn tests (Lane A1) ─────────────────────────────────────
    //
    // These tests exercise the supervisor-restart deep-clone path. The shape
    // mirrors the production C1 scenario: an actor holds a heap-allocated
    // owned field (here a malloc'd byte buffer) and the supervisor must
    // produce an independently-owned restart-state, not a byte-alias.

    /// A miniature heap-bearing state struct used to validate clone/drop
    /// callbacks. Owns `payload` (malloc'd); the `sentinel` exists so the
    /// wrapper is non-trivially sized.
    #[repr(C)]
    struct HeapState {
        payload: *mut u8,
        payload_len: usize,
        sentinel: u32,
    }

    static CLONE_CALL_COUNT: AtomicUsize = AtomicUsize::new(0);
    static DROP_CALL_COUNT: AtomicUsize = AtomicUsize::new(0);
    static CLONE_FORCE_NULL: AtomicBool = AtomicBool::new(false);
    /// Serializes the `state_clone_fn_*` tests because they share the global
    /// `CLONE_*` / `DROP_CALL_COUNT` atomics above (test binary runs tests
    /// in parallel threads by default).
    static CLONE_TEST_SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn reset_clone_counters() {
        CLONE_CALL_COUNT.store(0, Ordering::SeqCst);
        DROP_CALL_COUNT.store(0, Ordering::SeqCst);
        CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
    }

    /// Deep-clone callback: allocates a fresh `HeapState` wrapper + fresh
    /// payload buffer, copies payload bytes. Returns null if
    /// `CLONE_FORCE_NULL` is set (used by the failure-blocks-restart test).
    unsafe extern "C-unwind" fn heap_state_clone(src: *const c_void) -> *mut c_void {
        CLONE_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
        if CLONE_FORCE_NULL.load(Ordering::SeqCst) {
            return ptr::null_mut();
        }
        // SAFETY: caller (runtime) guarantees src is a HeapState wrapper.
        let src = unsafe { &*src.cast::<HeapState>() };
        // SAFETY: malloc on the C heap to pair with libc::free in drop/teardown.
        let dst = unsafe { libc::malloc(std::mem::size_of::<HeapState>()) }.cast::<HeapState>();
        if dst.is_null() {
            return ptr::null_mut();
        }
        let new_payload = if src.payload_len > 0 {
            // SAFETY: payload_len is in-bounds malloc size.
            let buf = unsafe { libc::malloc(src.payload_len) }.cast::<u8>();
            if buf.is_null() {
                // SAFETY: dst was just allocated.
                unsafe { libc::free(dst.cast::<c_void>()) };
                return ptr::null_mut();
            }
            // SAFETY: src.payload is valid for src.payload_len bytes.
            unsafe { ptr::copy_nonoverlapping(src.payload, buf, src.payload_len) };
            buf
        } else {
            ptr::null_mut()
        };
        // SAFETY: dst was just allocated.
        unsafe {
            (*dst).payload = new_payload;
            (*dst).payload_len = src.payload_len;
            (*dst).sentinel = src.sentinel;
        }
        dst.cast::<c_void>()
    }

    /// Drop callback: frees the wrapper's payload buffer (NOT the wrapper).
    unsafe extern "C" fn heap_state_drop(state: *mut c_void) {
        DROP_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
        if state.is_null() {
            return;
        }
        // SAFETY: state is a HeapState wrapper.
        let s = unsafe { &mut *state.cast::<HeapState>() };
        if !s.payload.is_null() {
            // SAFETY: payload was malloc'd by the clone callback.
            unsafe { libc::free(s.payload.cast::<c_void>()) };
            s.payload = ptr::null_mut();
        }
    }

    /// Build a heap-bearing initial-state template (caller owns the
    /// returned pointer; pass to `add_child_spec` which will byte-copy it).
    // Box return is intentional for clear ownership of the malloc-backed payload.
    #[allow(clippy::unnecessary_box_returns, reason = "explicit ownership in test")]
    fn make_heap_template() -> Box<HeapState> {
        // Use Box to keep ownership clear in the test; the runtime byte-copies
        // it into a libc::malloc buffer inside add_child_spec.
        let payload_bytes: &[u8] = b"original";
        // SAFETY: malloc payload buffer to match clone-fn's allocator.
        let payload = unsafe { libc::malloc(payload_bytes.len()) }.cast::<u8>();
        // SAFETY: payload buffer is malloc'd.
        unsafe { ptr::copy_nonoverlapping(payload_bytes.as_ptr(), payload, payload_bytes.len()) };
        Box::new(HeapState {
            payload,
            payload_len: payload_bytes.len(),
            sentinel: 0xDEAD_BEEF,
        })
    }

    unsafe fn make_supervisor_with_heap_child(
        register_clone: bool,
    ) -> (*mut HewSupervisor, Box<HeapState>) {
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 4, 1);
            assert!(!sup.is_null());

            let template = make_heap_template();
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: std::ptr::from_ref(&*template).cast_mut().cast::<c_void>(),
                init_state_size: std::mem::size_of::<HeapState>(),
                dispatch: Some(noop_child_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            hew_supervisor_set_child_state_drop(sup, 0, heap_state_drop);
            if register_clone {
                hew_supervisor_set_child_state_clone(sup, 0, heap_state_clone);
            }
            (sup, template)
        }
    }

    #[test]
    fn state_clone_fn_basic_round_trip() {
        let _rt = crate::runtime_test_guard();
        // Registers a clone fn that deep-clones HeapState, drives a restart
        // via restart_child_from_spec, verifies clone_fn was invoked and the
        // new actor's state is a distinct allocation.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);

            // Registration of clone_fn re-clones spec.init_state in place to
            // break the initial byte-alias. Expect: 1 clone call so far.
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                1,
                "set_child_state_clone must re-clone the spec template once to break initial byte-alias"
            );

            let initial_child = (&(*sup).children)[0];
            assert!(!initial_child.is_null());
            let initial_state_ptr = (*initial_child).state;
            let spec_template_after_reg = (&(*sup).child_specs)[0].init_state;
            assert_ne!(
                initial_state_ptr, spec_template_after_reg,
                "spec.init_state must be re-cloned to a distinct allocation; actor.state still byte-copied from original"
            );

            // Drive a restart. The supervisor sees state_clone_fn=Some and
            // routes through hew_actor_spawn_opts_adopt.
            let restarted = restart_child_from_spec(&mut *sup, 0);
            assert!(!restarted.is_null(), "restart must succeed");
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "clone_fn must be invoked once per restart (1 reg + 1 restart = 2 total)"
            );
            assert_ne!(
                (*restarted).state,
                spec_template_after_reg,
                "restarted actor.state must be a fresh clone, not aliasing the spec template"
            );
            assert!(
                (*restarted).init_state.is_null(),
                "adopt-spawn path must leave actor.init_state null (spec holds the template)"
            );

            // Sentinel survived the round-trip.
            let restarted_payload = &*(*restarted).state.cast::<HeapState>();
            assert_eq!(restarted_payload.sentinel, 0xDEAD_BEEF);
            assert_eq!(restarted_payload.payload_len, b"original".len());

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn state_clone_fn_failure_blocks_restart() {
        let _rt = crate::runtime_test_guard();
        // clone_fn returns null. Verify: restart returns null, child slot
        // is null, circuit-breaker success counter is NOT advanced.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);
            // Put the breaker in HALF_OPEN: if the null-clone path
            // incorrectly called `circuit_breaker_record_success`, it would
            // transition the state back to CLOSED. Observing HALF_OPEN
            // unchanged is the strongest available signal that the success
            // path was NOT taken.
            (&mut (*sup).child_specs)[0].circuit_breaker.state = 2; // HEW_CIRCUIT_BREAKER_HALF_OPEN
            let baseline_clone_calls = CLONE_CALL_COUNT.load(Ordering::SeqCst);

            CLONE_FORCE_NULL.store(true, Ordering::SeqCst);
            let restarted = restart_child_from_spec(&mut *sup, 0);
            assert!(
                restarted.is_null(),
                "null-clone-return must propagate as a failed restart"
            );
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                baseline_clone_calls + 1,
                "clone_fn must be called exactly once before the null-return short-circuit"
            );
            assert_eq!(
                (&(*sup).child_specs)[0].circuit_breaker.state,
                2,
                "circuit-breaker must remain HALF_OPEN; null-clone must NOT call record_success"
            );
            assert!(
                (&(*sup).children)[0].is_null(),
                "child slot must be null after a blocked restart"
            );

            // Clear the flag so cleanup doesn't infinite-loop in any
            // subsequent restart attempt during stop.
            CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn state_clone_fn_null_falls_back_to_bytecopy() {
        let _rt = crate::runtime_test_guard();
        // No state_clone_fn registered: restart must still succeed via the
        // legacy `hew_actor_spawn_opts` byte-copy path. This preserves
        // backward compatibility for children whose codegen has not yet
        // emitted a clone fn (out-of-tree consumers, hand-rolled actors).
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(false);
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                0,
                "no clone fn registered => no clone calls"
            );

            let restarted = restart_child_from_spec(&mut *sup, 0);
            assert!(
                !restarted.is_null(),
                "legacy byte-copy restart must still succeed"
            );
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                0,
                "legacy byte-copy path must NOT invoke clone_fn"
            );
            assert!(
                !(*restarted).init_state.is_null(),
                "legacy path must populate actor.init_state via deep_copy_state"
            );

            // Pin: the spec stayed in legacy mode (no in-place re-clone).
            // No assertion on spec.init_state value — just that the test
            // doesn't UAF.
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn state_clone_fn_alias_freedom_under_mutation() {
        let _rt = crate::runtime_test_guard();
        // C1 regression: with clone_fn registered, mutating actor.state's
        // owned heap fields must NOT dangle spec.init_state's pointers.
        // Verifies that registration breaks the initial byte-alias and that
        // a subsequent restart deep-clones from the clean spec template.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);
            let child = (&(*sup).children)[0];
            assert!(!child.is_null());

            // Simulate the actor reallocating its `payload` (Vec growth):
            // free the old payload, malloc a fresh, larger one, splice into
            // actor.state. After this, if spec.init_state still aliased the
            // old payload pointer, a clone read would UAF.
            let actor_state = &mut *(*child).state.cast::<HeapState>();
            libc::free(actor_state.payload.cast::<c_void>());
            let new_payload = libc::malloc(64).cast::<u8>();
            assert!(!new_payload.is_null());
            libc::memset(new_payload.cast::<c_void>(), 0xAB, 64);
            actor_state.payload = new_payload;
            actor_state.payload_len = 64;

            // Critically, the spec template was re-cloned at registration
            // time; its `payload` points to an independent allocation that
            // is unaffected by the mutation above.
            let spec_template = &*(&(*sup).child_specs)[0].init_state.cast::<HeapState>();
            assert_ne!(
                spec_template.payload, actor_state.payload,
                "post-registration: spec.init_state.payload must be independent from actor.state.payload"
            );
            assert_eq!(
                spec_template.payload_len,
                b"original".len(),
                "spec template payload length must reflect the clean clone, not the mutated actor"
            );

            // Restart: clone_fn reads spec.init_state (clean), not
            // actor.state (mutated). Must not UAF.
            let baseline_clones = CLONE_CALL_COUNT.load(Ordering::SeqCst);
            let restarted = restart_child_from_spec(&mut *sup, 0);
            assert!(!restarted.is_null());
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), baseline_clones + 1);

            let restarted_state = &*(*restarted).state.cast::<HeapState>();
            assert_eq!(
                restarted_state.payload_len,
                b"original".len(),
                "restart must reproduce the clean template, not the mutated actor's state"
            );
            assert_ne!(
                restarted_state.payload, spec_template.payload,
                "restart payload must be an independent clone, not aliasing the spec template"
            );

            hew_supervisor_stop(sup);
        }
    }

    const FORCED_BYTECOPY_FREED_SPEC_ENV: &str = "HEW_SUPERVISOR_FORCED_BYTECOPY_FREED_SPEC_PROBE";

    #[cfg(target_os = "macos")]
    const GUARD_MALLOC_DYLIB: &str = "/usr/lib/libgmalloc.dylib";

    unsafe fn free_spec_template_payload(sup: *mut HewSupervisor) -> *mut u8 {
        let spec_template = (&mut (*sup).child_specs)[0].init_state.cast::<HeapState>();
        assert!(!spec_template.is_null());
        let payload = (*spec_template).payload;
        assert!(!payload.is_null());
        libc::free(payload.cast::<c_void>());
        payload
    }

    unsafe fn run_forced_bytecopy_freed_spec_payload_probe() -> ! {
        // Install a runtime so spawn/track resolve; this subprocess faults
        // intentionally (GuardMalloc SIGSEGV) before the guard would drop.
        let _rt = crate::runtime_test_guard();
        let (sup, _template) = make_supervisor_with_heap_child(false);
        let dangling_payload = free_spec_template_payload(sup);

        let restarted = restart_child_from_spec(&mut *sup, 0);
        assert!(
            !restarted.is_null(),
            "legacy byte-copy restart must produce an actor"
        );
        let restarted_state = &mut *(*restarted).state.cast::<HeapState>();
        assert_eq!(
            restarted_state.payload, dangling_payload,
            "legacy byte-copy restart must preserve the freed payload alias"
        );

        *restarted_state.payload = 0xCC;
        std::process::exit(0);
    }

    #[cfg(target_os = "macos")]
    fn assert_forced_bytecopy_freed_spec_faults_under_guard_malloc(test_name: &str) {
        use std::os::unix::process::ExitStatusExt as _;

        if !std::path::Path::new(GUARD_MALLOC_DYLIB).exists() {
            eprintln!("skipping GuardMalloc alias-fault probe: {GUARD_MALLOC_DYLIB} not found");
            return;
        }

        let status = std::process::Command::new(std::env::current_exe().expect("current_exe"))
            .args(["--exact", test_name, "--nocapture"])
            .env("RUST_TEST_THREADS", "1")
            .env(FORCED_BYTECOPY_FREED_SPEC_ENV, "1")
            .env("DYLD_INSERT_LIBRARIES", GUARD_MALLOC_DYLIB)
            .env("MallocGuardEdges", "1")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .expect("spawn GuardMalloc alias-fault helper");
        assert_eq!(
            status.signal(),
            Some(libc::SIGSEGV),
            "forced byte-copy of freed spec.init_state payload must SIGSEGV under GuardMalloc; status={status:?}"
        );
    }

    #[cfg(not(target_os = "macos"))]
    fn assert_forced_bytecopy_freed_spec_faults_under_guard_malloc(_test_name: &str) {}

    #[test]
    fn null_clone_restart_blocks_freed_spec_payload_alias_probe() {
        if std::env::var_os(FORCED_BYTECOPY_FREED_SPEC_ENV).is_some() {
            // SAFETY: helper runs in a subprocess and intentionally faults
            // under GuardMalloc after constructing the legacy byte-copy alias.
            unsafe { run_forced_bytecopy_freed_spec_payload_probe() };
        }

        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree and mutates only its test state.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);
            let child = (&(*sup).children)[0];
            assert!(!child.is_null());

            // Poison the source every restart path actually reads:
            // spec.init_state, not the current child actor's state.
            let dangling_payload = free_spec_template_payload(sup);
            let baseline_clones = CLONE_CALL_COUNT.load(Ordering::SeqCst);

            CLONE_FORCE_NULL.store(true, Ordering::SeqCst);
            let restarted = restart_child_from_spec(&mut *sup, 0);
            assert!(
                restarted.is_null(),
                "null-clone policy must block the restart instead of byte-copying a freed spec payload"
            );
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                baseline_clones + 1,
                "restart must call clone_fn once before the null-return short-circuit"
            );
            assert!(
                (&(*sup).children)[0].is_null(),
                "blocked restart must leave the child slot null"
            );
            assert_eq!(
                (&*(&(*sup).child_specs)[0].init_state.cast::<HeapState>()).payload,
                dangling_payload,
                "test setup must leave the freed spec payload in place as the byte-copy falsifier"
            );

            CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
            // Null the already-freed spec payload so that InternalChildSpec::drop
            // (which now calls state_drop_fn before libc::free) does not double-free
            // the dangling pointer.  The falsifier assertion above already verified
            // it was in place; the test's correctness doesn't depend on it surviving
            // past that point.
            (&mut *(&(*sup).child_specs)[0].init_state.cast::<HeapState>()).payload =
                ptr::null_mut();
            assert_eq!(actor::hew_actor_free(child), 0);
            hew_supervisor_stop(sup);
        }

        assert_forced_bytecopy_freed_spec_faults_under_guard_malloc(
            "supervisor::tests::null_clone_restart_blocks_freed_spec_payload_alias_probe",
        );
    }

    #[test]
    fn hew_supervisor_set_child_state_clone_back_fills() {
        let _rt = crate::runtime_test_guard();
        // Setting the clone fn after add_child_spec must back-fill it onto
        // the already-spawned actor so future direct-spawn restart consumers
        // see the same callback. Mirror of the state_drop_fn back-fill test.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());
            let template = make_heap_template();
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: std::ptr::from_ref(&*template).cast_mut().cast::<c_void>(),
                init_state_size: std::mem::size_of::<HeapState>(),
                dispatch: Some(noop_child_dispatch),
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            let child = (&(*sup).children)[0];
            assert!(!child.is_null());
            assert!(
                (*child).state_clone_fn.is_none(),
                "before set_child_state_clone, actor.state_clone_fn must be None"
            );

            hew_supervisor_set_child_state_clone(sup, 0, heap_state_clone);

            let stored = (*child)
                .state_clone_fn
                .expect("back-fill must populate actor.state_clone_fn");
            assert_eq!(
                stored as *const () as usize, heap_state_clone as *const () as usize,
                "back-filled fn pointer must match the registered fn"
            );
            // The spec template was re-cloned during registration.
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), 1);

            // Stop without enabling clone-from-fail; cleans up the heap
            // allocations via state_drop_fn on actor.state and libc::free of
            // the cloned spec template.
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
    drop(unsafe { Box::from_raw(sup) }); // ALLOCATOR-PAIRING: GlobalAlloc
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
    // Read the child's recorded trap code (set by `hew_actor_trap` before the
    // state transition). `0` for non-crash exits or when no specific code was
    // recorded — we propagate the honest unknown rather than fabricating.
    let crash_code = child_ref.error_code.load(Ordering::Acquire);

    // Notify the supervisor actor via the event system.
    // SAFETY: sup is valid and child_id / exit_state are read from valid memory.
    unsafe {
        hew_supervisor_notify_child_event(sup, idx, child_ref.id, exit_state, crash_code);
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
    // The default scheduler runs one worker per core, so readers here race the
    // restart machinery (store_child_slot / restart_child_from_spec) on other
    // workers; `children_lock` is the exclusion that keeps the discriminator
    // triple coherent. Future optimization: migrate to AtomicPtr<HewActor> +
    // atomic discriminator fields so readers can avoid the mutex on the common
    // Live path.
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

/// Decomposed variant of [`hew_supervisor_child_get`] for LLVM-generated callers.
///
/// Returns the packed first word `(tag: u8, reason: u8, _pad: [u8; 6])` as a
/// plain `u64` in a single register (bits 7:0 = tag, bits 15:8 = reason).
/// Writes the actor handle as `u64` to `*handle_out` (which must be non-null).
///
/// # Why this exists
///
/// On Windows x64 (MSVC ABI) Rust returns `ChildLookupResult` (16 bytes) via
/// a hidden sret pointer in RCX, but the hew LLVM codegen used to emit a
/// register-return call site — causing the callee to treat the supervisor
/// pointer as the return buffer and the slot key as the supervisor pointer.
/// Returning a plain `u64` sidesteps the sret convention on all platforms.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// `handle_out` must point to a valid `u64`-aligned memory location.
/// Behaviour is undefined if either pointer has been freed.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_child_get_raw(
    sup: *mut HewSupervisor,
    key: u32,
    handle_out: *mut u64,
) -> u64 {
    // SAFETY: hew_supervisor_child_get has its own null + bounds checks.
    let result = unsafe { hew_supervisor_child_get(sup, key) };
    // SAFETY: caller guarantees handle_out points to a valid u64.
    unsafe { *handle_out = result.handle as usize as u64 };
    u64::from(result.tag) | (u64::from(result.reason) << 8)
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

/// Decomposed variant of [`hew_supervisor_nested_get`] for LLVM-generated callers.
///
/// Mirrors [`hew_supervisor_child_get_raw`]: returns `tag | (reason << 8)` as
/// `u64` and writes the nested supervisor handle (a `*mut HewSupervisor` cast
/// to `u64`) to `*handle_out`.  Avoids the Windows x64 sret ABI mismatch.
///
/// # Safety
///
/// Same as [`hew_supervisor_nested_get`].  `handle_out` must point to a valid
/// `u64`-aligned location.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_nested_get_raw(
    sup: *mut HewSupervisor,
    key: u32,
    handle_out: *mut u64,
) -> u64 {
    // SAFETY: hew_supervisor_nested_get has its own null + bounds checks.
    let result = unsafe { hew_supervisor_nested_get(sup, key) };
    // SAFETY: caller guarantees handle_out points to a valid u64.
    unsafe { *handle_out = result.handle as usize as u64 };
    u64::from(result.tag) | (u64::from(result.reason) << 8)
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

    // The v0.6 init-closure restart model: a dynamic child carrying an init_fn
    // produces its state via the thunk; skip the byte-copy template (mirror
    // hew_supervisor_add_child_spec).
    let has_init_fn = sp.init_fn.is_some();

    // Adopt the supervisor config buffer (idempotent on the same pointer).
    if has_init_fn && !sp.config.is_null() {
        if s.config_buf.is_null() {
            s.config_buf = sp.config;
            s.config_size = sp.config_size;
        } else if s.config_buf != sp.config {
            // Conflicting non-null config pointer — unreachable from correct
            // codegen (one buffer per supervisor). debug_assert in debug; in
            // release free the rejected duplicate (fail closed, no leak). The
            // pointer differs from the adopted buffer, so this cannot
            // double-free it.
            debug_assert!(
                false,
                "hew_supervisor_add_child_dynamic: config buffer ({:p}) differs from the \
                 supervisor's adopted buffer ({:p})",
                sp.config, s.config_buf
            );
            // SAFETY: sp.config is a libc::malloc'd orphan distinct from the
            // adopted buffer (ALLOCATOR-PAIRING: libc).
            unsafe { libc::free(sp.config) };
        }
    }

    // Deep-copy init state — only on the template (non-init_fn) path.
    let state_copy = if !has_init_fn && sp.init_state_size > 0 && !sp.init_state.is_null() {
        // SAFETY: init_state is valid for init_state_size bytes.
        let buf = unsafe { libc::malloc(sp.init_state_size) }; // ALLOCATOR-PAIRING: libc
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
        init_state_size: if has_init_fn { 0 } else { sp.init_state_size },
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        cycle_capable: sp.cycle_capable,
        on_crash: sp.on_crash,
        // Registered by the caller via hew_supervisor_set_child_state_drop
        // immediately after this call returns. See the function doc comment
        // for the race-window analysis and calling contract.
        state_drop_fn: None,
        // Registered by the caller via hew_supervisor_set_child_state_clone
        // immediately after this call returns.
        state_clone_fn: None,
        // Carried IN the spec literal (like on_crash) so the dynamic child's
        // initial spawn — which also routes through restart_child_from_spec —
        // fires the lifecycle wrapper.
        lifecycle_fn: sp.lifecycle_fn,
        // Carried IN the spec literal so the dynamic child's initial spawn uses
        // the thunk (the load-bearing first-spawn carrier).
        init_fn: sp.init_fn,
        config: if has_init_fn {
            s.config_buf
        } else {
            ptr::null_mut()
        },
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
        unsafe { libc::free(spec.init_state) }; // ALLOCATOR-PAIRING: libc
        spec.init_state = ptr::null_mut();
    }
    if !spec.name.is_null() {
        // SAFETY: name was allocated with libc::strdup.
        unsafe { libc::free(spec.name.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
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

/// Register the lifecycle wrapper for a child actor spec.
///
/// Codegen emits this call after [`hew_supervisor_add_child_spec`] for parity
/// with the state setters. It stores the wrapper pointer on the spec so it is
/// available to symmetry consumers and to any future code path that rebuilds a
/// spec without the literal carrier.
///
/// **It does NOT fire the wrapper on the already-spawned child.** UNLIKE
/// [`hew_supervisor_set_child_state_drop`] (which back-fills the running
/// actor), the initial supervised spawn's lifecycle fire already happened
/// inside `add_child_spec` → `restart_child_from_spec`, reading the
/// `lifecycle_fn` carried IN the `HewChildSpec` literal (copied at spec
/// registration). Firing here too would run `init()` / `#[on(start)]` a SECOND
/// time on the initial incarnation. The literal field is the load-bearing
/// carrier for the initial fire; this setter is back-fill symmetry only.
///
/// `child_index` is the zero-based index of the child whose spec should be
/// updated. Indices are stable until [`hew_supervisor_remove_child`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `lifecycle_fn` must be a valid C-ABI function pointer matching the
///   [`HewLifecycleFn`] contract (takes the actor pointer, runs `init` /
///   `on_start` under the actor state lock, registers the terminate hook).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_lifecycle(
    sup: *mut HewSupervisor,
    child_index: c_int,
    lifecycle_fn: HewLifecycleFn,
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

    // Store only. The initial-spawn fire already ran inside add_child_spec
    // (reading the literal-carried pointer); do NOT re-fire here.
    s.child_specs[idx].lifecycle_fn = Some(lifecycle_fn);
}

/// Register a state-clone callback for a child actor spec, breaking the
/// initial-spawn byte-alias between the spec's `init_state` template and the
/// running actor's `state` allocation.
///
/// Called by codegen (Lane A2) immediately after [`hew_supervisor_add_child_spec`]
/// (or [`hew_supervisor_add_child_dynamic`]). Stores the clone fn on the spec so
/// future restart paths use it (see `restart_child_from_spec`), back-fills it
/// on the already-spawned child actor for symmetry, **and** — critically —
/// re-clones `spec.init_state` in place using the freshly-registered
/// `state_clone_fn`, replacing the byte-copy template that
/// `hew_supervisor_add_child_spec` installed.
///
/// **Why the in-place re-clone**: prior to this setter, the spec's
/// `init_state` is a `memcpy` of the user-supplied template, and the initial
/// actor's `state` is a `memcpy` of *that* — meaning all three wrappers
/// share identical byte patterns including embedded heap pointers
/// (`Vec.ptr`, `String.ptr`, IO handles). When the actor first mutates or
/// reallocates an owned field, the spec's wrapper carries a dangling pointer
/// (root cause of audit C1 UAF). Re-cloning the spec at registration time —
/// while the actor is still idle in its mailbox queue and has not yet
/// dispatched a message — converts `spec.init_state` into an independently-
/// owned deep clone. Subsequent restarts then deep-clone *that* clean
/// template via the same `state_clone_fn`.
///
/// **Race window**: codegen emits this setter call back-to-back with
/// `hew_supervisor_add_child_spec` in the same basic block; the spawned
/// actor's mailbox is empty at this point, so no dispatch can have run yet.
/// This matches the calling contract documented on
/// [`actor::hew_actor_set_state_drop`].
///
/// **OOM on re-clone**: if the in-place clone fails (`clone_fn` returns null),
/// the spec retains its byte-copy template — restart can still fall back to
/// the legacy byte-copy path on a future crash (with the same C1 hazard, but
/// no worse than today). The `state_clone_fn` pointer is still stored so
/// future restarts retry the clone-aware path.
///
/// `child_index` is the zero-based index of the child whose spec should be
/// updated. Indices are stable until [`hew_supervisor_remove_child`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `state_clone_fn` must satisfy the [`actor::HewStateCloneFn`] contract
///   (deep-cloning, `malloc`-compatible output, null on OOM).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_state_clone(
    sup: *mut HewSupervisor,
    child_index: c_int,
    state_clone_fn: actor::HewStateCloneFn,
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

    s.child_specs[idx].state_clone_fn = Some(state_clone_fn);

    // Break the initial-spawn byte-alias by re-cloning the spec template
    // in place. Safe to do BEFORE locking children because we only read
    // immutable spec fields here; the actor's `state` is a sibling
    // byte-copy and is untouched by this re-clone.
    let (template_ptr, template_size) = {
        let spec = &s.child_specs[idx];
        (spec.init_state, spec.init_state_size)
    };
    if template_size > 0 && !template_ptr.is_null() {
        // SAFETY: template_ptr is a malloc'd wrapper of template_size bytes
        // produced by hew_supervisor_add_child_spec's byte-copy; the
        // contract of state_clone_fn admits reading from such a wrapper as
        // long as it has not yet been mutated. The race-window analysis in
        // the doc comment justifies the no-mutation precondition.
        let fresh = unsafe { state_clone_fn(template_ptr.cast_const()) };
        if !fresh.is_null() {
            // Replace spec.init_state with the independently-owned clone.
            // The OLD template's owned-field pointers byte-alias the
            // running actor's `state` fields, so we must raw-libc::free the
            // wrapper *without* calling state_drop_fn — letting the actor
            // remain the sole owner of those heap allocations until it
            // crashes or stops.
            // SAFETY: old template was allocated with libc::malloc in
            // hew_supervisor_add_child_spec / _add_child_dynamic.
            unsafe {
                libc::free(template_ptr); // ALLOCATOR-PAIRING: libc
            }
            s.child_specs[idx].init_state = fresh;
        }
        // If `fresh.is_null()` (clone OOM at registration time), leave the
        // byte-copy template in place. The state_clone_fn is still stored;
        // the next restart will retry the clone-aware path.
    }

    // Guard the children-slot read so a concurrent supervisor restart on
    // another thread cannot replace s.children[idx] between the load and
    // the hew_actor_set_state_clone call.
    let _guard = s.children_lock.lock_or_recover();

    // Register on the already-spawned actor for its first run (the initial
    // spawn happens inside add_child_spec before this setter is called).
    let child = s.children[idx];
    if !child.is_null() {
        // SAFETY: child is a valid actor pointer; state_clone_fn has the
        // correct signature.
        unsafe { actor::hew_actor_set_state_clone(child, state_clone_fn) };
    }
}

/// Register the per-child init thunk for the v0.6 init-closure restart model.
///
/// The thunk PRODUCES a fresh, independently-owned actor state on the initial
/// spawn AND every restart by re-running the child's init-arg expressions
/// against the supervisor's construction-time config (the `config` buffer). It
/// REPLACES the byte-copy state template, making owned (`string`/`Vec`) init
/// args sound under restart — each incarnation gets unaliased owned values.
///
/// **The load-bearing carrier is the `HewChildSpec` literal, not this setter.**
/// Codegen rides `init_fn` + `config` + `config_size` IN the spec literal so the
/// INITIAL supervised spawn — which fires inside `hew_supervisor_add_child_spec`
/// before any post-hoc setter runs — already uses the thunk. This setter is
/// back-fill / symbol-stability symmetry (mirroring
/// `hew_supervisor_set_child_state_clone`), and is also the additive ABI entry
/// point out-of-tree C callers use to install a thunk after `add_child_spec`.
///
/// Config-buffer ownership: the supervisor adopts `config` ONCE (the first
/// non-null registration) and frees it EXACTLY ONCE at teardown
/// (`stop_supervisor_owned`). Subsequent registrations with the same pointer are
/// idempotent; a conflicting non-null pointer is a codegen ABI error (one config
/// buffer per supervisor). The thunk only ever READS `config`.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `init_fn` must satisfy the [`HewChildInitFn`] contract (produces a fresh
///   owned state wrapper; `state == null` on OOM).
/// - `config` must be null, or a `malloc`-compatible heap allocation of
///   `config_size` bytes whose ownership transfers to the supervisor on the
///   first registration.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_init_fn(
    sup: *mut HewSupervisor,
    child_index: c_int,
    init_fn: HewChildInitFn,
    config: *mut c_void,
    config_size: usize,
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

    // Adopt the supervisor-owned config buffer once; idempotent on the same
    // pointer. (The literal carrier already adopted it inside add_child_spec for
    // the initial spawn; this keeps the setter self-consistent for out-of-tree
    // callers that install the thunk post-hoc.)
    if !config.is_null() {
        if s.config_buf.is_null() {
            s.config_buf = config;
            s.config_size = config_size;
        } else if s.config_buf != config {
            // Conflicting non-null config pointer — unreachable from correct
            // codegen (the setter runs after add_child_spec adopted the SAME
            // buffer). debug_assert in debug; in release free the rejected
            // duplicate (fail closed, no leak). It differs from the adopted
            // buffer, so this cannot double-free it.
            debug_assert!(
                false,
                "hew_supervisor_set_child_init_fn: child {idx} config buffer ({config:p}) \
                 differs from the supervisor's adopted buffer ({:p}); codegen must emit ONE \
                 config buffer per supervisor",
                s.config_buf
            );
            // SAFETY: config is a libc::malloc'd orphan distinct from the
            // adopted buffer (ALLOCATOR-PAIRING: libc).
            unsafe { libc::free(config) };
        }
    }

    s.child_specs[idx].init_fn = Some(init_fn);
    s.child_specs[idx].config = s.config_buf;
}

/// Register the config struct's drop-inplace glue so the supervisor releases the
/// config buffer's OWNED inner fields (`string`/`bytes`/…) at teardown, before
/// the flat `libc::free` of the buffer.
///
/// The config buffer is a flat snapshot of the moved-in config value and OWNS
/// its inner owned fields (the init thunks only CLONE from them). Without this
/// drop glue those fields leak at teardown. Codegen calls this once, after the
/// config buffer is materialised, when the config struct has any owned field.
/// An all-scalar config never calls it (`config_drop_fn` stays `None`).
///
/// Idempotent: re-registration with the same fn is a no-op; a CONFLICTING fn is
/// a codegen ABI error (one config struct type per supervisor) — `debug_assert`
/// in debug, last-writer-wins in release (both fns drop the same struct layout,
/// so neither leaks nor double-frees).
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `drop_fn` must be the `__hew_record_drop_inplace_<T>` for the config
///   struct type whose instance backs `config_buf`.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_config_drop_fn(
    sup: *mut HewSupervisor,
    drop_fn: unsafe extern "C" fn(*mut c_void),
) {
    if sup.is_null() {
        return;
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };
    debug_assert!(
        s.config_drop_fn
            .is_none_or(|f| std::ptr::fn_addr_eq(f, drop_fn)),
        "hew_supervisor_set_config_drop_fn: a different config drop fn is already \
         registered; codegen must register ONE config drop fn per supervisor"
    );
    s.config_drop_fn = Some(drop_fn);
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

// ── Cooperative restart-await observer (`await_restart`) ─────────────────────

/// Codegen ABI: the `await_restart` parked the continuation; the runtime wakes
/// it via `enqueue_resume` when the restart cycle completes. The caller MUST
/// `coro.suspend`.
pub const RESTART_AWAIT_SUSPEND: i32 = 0;
/// Codegen ABI: the child is already Live, or permanently Dead (will never
/// restart). The caller MUST NOT suspend and resumes immediately on the bind
/// edge — re-resolving the slot, which is either Live (proceed) or fails closed
/// at the send re-resolve (never an infinite hang).
pub const RESTART_AWAIT_READY: i32 = 1;

/// Register a suspending `await_restart sup.child`.
///
/// Returns [`RESTART_AWAIT_READY`] when the child slot is already Live (no wait
/// needed) OR permanently Dead (`SupervisorShutdown`/`UnknownSlot` — will never
/// restart, so the caller fails closed on resume rather than parking forever,
/// the R4 contract). Returns [`RESTART_AWAIT_SUSPEND`] after parking the
/// continuation as a restart observer when the slot is Transient (mid-restart /
/// backoff / circuit-open). The caller MUST `coro.suspend` on SUSPEND and bind
/// (re-fetch) on READY / resume.
///
/// This is the COOPERATIVE analogue of [`hew_supervisor_wait_restart`]; it never
/// thread-blocks the single scheduler. `key` is the static-child slot index.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `actor` is the awaiting actor (`hew_actor_self`).
/// - `slot` is a live read slot the caller created and holds the creator ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_restart_await_suspend(
    sup: *mut HewSupervisor,
    key: u32,
    actor: *mut HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
) -> i32 {
    if sup.is_null() || slot.is_null() {
        crate::set_last_error(
            "C-ABI guard failed: sup/slot null in hew_supervisor_restart_await_suspend",
        );
        // Fail closed: report READY so the caller binds immediately rather than
        // parking forever; the bind re-fetch fails closed on a dead slot.
        return RESTART_AWAIT_READY;
    }

    // SAFETY: caller guarantees `sup` is valid.
    let s = unsafe { &*sup };

    // Snapshot the restart counter BEFORE the pre-park check. `notify_restart`
    // bumps this counter (under `restart_notify.0`) before it drains waiters
    // (under `restart_await_waiters`), so re-reading it inside the registration
    // critical section detects a restart that completed in the gap between the
    // pre-park check and the push — the lost-wakeup guard (mirrors the `baseline`
    // discipline in `hew_supervisor_restart_await_blocking`).
    let baseline = s
        .restart_notify
        .as_ref()
        .map_or(0, |pair| *pair.0.lock_or_recover());

    // Pre-park state check (R4 / issue #2124): inspect the current slot before
    // parking so a Live child resumes immediately and a permanently-Dead child
    // never hangs. Only a Transient slot parks.
    // SAFETY: `sup`/`key` are the FFI contract; `child_get` does its own guards.
    let current = unsafe { hew_supervisor_child_get(sup, key) };
    // Live (0) → already running, no wait needed. Dead (2) → SupervisorShutdown /
    // UnknownSlot / BudgetExhausted, will never restart: fail closed and resume
    // immediately (the bind re-fetch surfaces the dead slot recoverably rather
    // than hanging — R4). Only a Transient (1) slot parks.
    if current.tag != 1 {
        return RESTART_AWAIT_READY;
    }

    // Test-only: deterministically drive the racing restart cycle here, in the
    // gap the lost-wakeup window opens. No-op in production builds.
    #[cfg(all(test, not(target_arch = "wasm32")))]
    fire_restart_await_park_gap_hook();

    // Park under the waiters lock, but first re-check whether the restart already
    // landed in the gap above (the lost-wakeup race the multi-worker scheduler
    // makes reachable). Holding `restart_await_waiters` while we re-read the
    // counter is the synchronization edge: `notify_restart` bumps the counter
    // before it acquires `restart_await_waiters` to drain, so if its drain already
    // ran (finding our waiter absent), the bump it performed is visible here and
    // we resolve READY instead of parking against a wake that already fired.
    let mut waiters = s.restart_await_waiters.lock_or_recover();
    let advanced = s
        .restart_notify
        .as_ref()
        .is_none_or(|pair| *pair.0.lock_or_recover() != baseline);
    if advanced {
        // A restart cycle completed (or no notify channel exists to bridge the
        // gap) since the pre-park snapshot. The wake we would park against has
        // already fired against an empty registry; resolve READY and let the bind
        // re-fetch resolve the now-settled slot rather than hang forever.
        drop(waiters);
        return RESTART_AWAIT_READY;
    }
    // Park: the observer takes an in-flight ref so the wake cannot free the slot
    // out from under the abandon edge.
    // SAFETY: caller holds the creator ref, so the slot is live to retain.
    unsafe { crate::read_slot::read_slot_retain(slot) };
    waiters.push(RestartAwaitWaiter { actor, slot });
    drop(waiters);
    RESTART_AWAIT_SUSPEND
}

/// Detach an abandoned suspending `await_restart` (the codegen abandon edge).
///
/// Removes the waiter from `restart_await_waiters` if still registered and
/// releases the observer's retained in-flight ref on the slot. If the waiter
/// already fired (drained by `notify_restart`), this is a no-op for the registry
/// and the ref was already released by the fire path — so it does NOT
/// double-free: the lookup-and-remove is the single authority that the ref is
/// still held here.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `slot` is the read slot handed to [`hew_supervisor_restart_await_suspend`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_restart_await_detach(
    sup: *mut HewSupervisor,
    slot: *mut crate::read_slot::HewReadSlot,
) {
    if sup.is_null() || slot.is_null() {
        return;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let s = unsafe { &*sup };
    let mut waiters = s.restart_await_waiters.lock_or_recover();
    if let Some(pos) = waiters.iter().position(|w| w.slot == slot) {
        waiters.swap_remove(pos);
        drop(waiters);
        // Cancel the slot so a racing wake drops, then release the retained ref.
        // SAFETY: the observer held this ref; removing the waiter is the single
        // authority that it is still live to release here.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };
    }
    // If not found, the waiter already fired and released its ref — no-op.
}

/// Blocking `await_restart` for a CONTEXTLESS caller (`main` / a free fn with
/// no parkable coroutine continuation). Blocks the calling thread on the
/// supervisor `restart_notify` Condvar until the child slot is Live again or
/// permanently Dead, then returns. The contextless analogue of
/// [`hew_supervisor_restart_await_suspend`].
///
/// This is safe to thread-block ONLY off the cooperative scheduler: `main` runs
/// on its own thread while the supervisor fires restarts on scheduler worker
/// threads, so there is no self-deadlock (unlike an actor handler, which MUST
/// use the suspending observer). Codegen routes a `Default`-callconv
/// `await_restart` here exactly as it routes a contextless `await` to a blocking
/// ask.
///
/// Returns once the slot is Live (proceed) or permanently Dead (the caller's
/// subsequent re-fetch fails closed). A short bounded poll backstops the wait so
/// a missed wake never hangs forever.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_restart_await_blocking(sup: *mut HewSupervisor, key: u32) {
    if sup.is_null() {
        return;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let s = unsafe { &*sup };

    // Snapshot the restart counter at entry. `await_restart` resolves on EITHER:
    //   (a) a restart cycle completing after the call (the counter advances) —
    //       this is the "I crashed the child, wait for it to come back" path,
    //       deterministic even though the crash tell may be in flight (the slot
    //       can read Live for an instant before the crash is processed); or
    //   (b) the slot being observed Transient (mid-restart) and then returning
    //       to Live — the "a fungible send failed, wait for recovery" path; or
    //   (c) the slot staying Live across a short grace window with NO restart in
    //       flight — a no-op `await_restart` on a healthy child returns rather
    //       than hanging forever (the over-wait guard).
    // A permanently-Dead slot returns immediately (R4 fail-closed). The fungible
    // re-resolve on the subsequent send is the liveness authority; this barrier
    // only ensures an in-flight restart has landed before we re-fetch.
    let baseline = s
        .restart_notify
        .as_ref()
        .map_or(0, |pair| *pair.0.lock_or_recover());

    // The grace window bounds case (c): if the slot is Live and no restart lands
    // within it, conclude no restart is coming and return. Short enough to stay
    // responsive, long enough to cover the crash-tell → restart latency.
    let grace_deadline = std::time::Instant::now() + std::time::Duration::from_millis(250);
    let mut saw_transient = false;

    loop {
        // SAFETY: `sup`/`key` are the FFI contract; child_get does its own guards.
        let current = unsafe { hew_supervisor_child_get(sup, key) };
        match current.tag {
            // Dead (2): permanent — never restarts. Fail closed: return now.
            2 => return,
            // Transient (1): a restart is in progress — wait for it (no grace
            // cutoff applies once we've seen the slot go Transient).
            1 => saw_transient = true,
            // Live (0): if we already saw it Transient, the restart completed —
            // return. Otherwise it may be the pre-crash Live window; fall through
            // to the counter/grace check.
            _ if saw_transient => return,
            _ => {}
        }

        if let Some(ref pair) = s.restart_notify {
            let count = pair.0.lock_or_recover();
            // (a) a restart completed since entry.
            if *count > baseline {
                return;
            }
            // (c) Live + no restart within the grace window → no restart coming.
            if !saw_transient && current.tag == 0 && std::time::Instant::now() >= grace_deadline {
                return;
            }
            let _ = pair
                .1
                .wait_timeout_or_recover(count, std::time::Duration::from_millis(20));
        } else {
            // No notify channel — slot-liveness polling with the same grace.
            if current.tag == 0 && (saw_transient || std::time::Instant::now() >= grace_deadline) {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
        }
    }
}

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
            unsafe { libc::free(name_copy.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
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
        static_members: Vec::new(),
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

/// Register a STATIC-backed pool member: a pool member whose actor lives in the
/// supervisor's `children[]` table at `static_idx`.
///
/// A static pool (`pool name: Type(count: N)`) spawns its N members as ordinary
/// static children, then records each member's static-child index here (in
/// member order). The accessor [`hew_supervisor_pool_child_get`] resolves member
/// `i` through the LIVE static slot `children[static_idx]`, so a restarted member
/// is re-resolved automatically — the restart machinery re-fills the static slot
/// and the pool view picks up the fresh actor with no stale PID cached.
///
/// Returns 0 on success; -1 if `sup` is null, `pool_key` is out of range, the
/// pool slot is null, `static_idx` is out of range, or the pool's `max_members`
/// limit would be exceeded.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_member_add_static(
    sup: *mut HewSupervisor,
    pool_key: u32,
    static_idx: u32,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &mut *sup };
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        set_last_error("hew_supervisor_pool_member_add_static: pool_key out of range");
        return -1;
    }
    if s.pool_slots[i].is_null() {
        set_last_error("hew_supervisor_pool_member_add_static: pool slot is null");
        return -1;
    }
    let static_idx = static_idx as usize;
    if static_idx >= s.child_count {
        set_last_error("hew_supervisor_pool_member_add_static: static_idx out of range");
        return -1;
    }
    // Enforce max_members if configured (counts current static members).
    let max = s.pool_specs[i].max_members;
    if max > 0 && s.pool_specs[i].static_members.len() >= max {
        set_last_error("hew_supervisor_pool_member_add_static: pool at max_members capacity");
        return -1;
    }
    s.pool_specs[i].static_members.push(static_idx);
    0
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

    // ── Static-backed pool path (`pool name: Type(count: N)`) ────────────────
    //
    // A static pool resolves member `i` through the LIVE static child slot, NOT
    // a cached PID. This is the restart re-resolution contract: the restart
    // machinery re-fills `children[static_idx]` with a FRESH actor on every
    // crash, and reading the slot here picks that up automatically — no stale
    // PID is ever returned, no pointer is cached across restart (LESSONS
    // `replaceable-resource-handle-is-fungible-reference`). The member's
    // liveness, circuit-breaker, and backoff classification reuse the static
    // child resolver verbatim.
    let static_members = &s.pool_specs[i].static_members;
    if !static_members.is_empty() {
        let Some(&static_idx) = static_members.get(index as usize) else {
            // Index beyond the member count → out of bounds (Vec[i] OOB parity).
            return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
        };
        // Resolve through the static slot resolver, which holds `children_lock`
        // and reads the (pointer, CB-state, backoff) triple coherently.
        #[expect(
            clippy::cast_possible_truncation,
            reason = "static child count is always small; u32 is the ABI key type"
        )]
        let key = static_idx as u32;
        // SAFETY: sup is valid (checked above); the resolver re-checks bounds.
        return unsafe { hew_supervisor_child_get(sup, key) };
    }

    // ── Dynamic PID-set pool path ────────────────────────────────────────────
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
    // A static-backed pool's size is its fixed static-member count (it never
    // shrinks; restart re-fills slots in place). A dynamic PID-set pool's size
    // is the live PID count.
    let static_members = &s.pool_specs[i].static_members;
    if !static_members.is_empty() {
        return static_members.len() as i64;
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

    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::{AtomicBool, Ordering};

    use super::{
        actor, restart_child_from_spec, restart_with_budget_and_strategy, take_child_slot,
        ChildSlotReason, HewChildInitResult, HewChildSpec, HewSupervisor, OVERFLOW_DROP_NEW,
        RESTART_PERMANENT,
    };
    use crate::supervisor::{
        hew_supervisor_add_child_spec, hew_supervisor_new, hew_supervisor_pool_add_slot,
        hew_supervisor_pool_child_get, hew_supervisor_pool_len, hew_supervisor_pool_member_add,
        hew_supervisor_pool_member_add_static, hew_supervisor_pool_member_remove,
        hew_supervisor_set_child_state_drop, hew_supervisor_start, hew_supervisor_stop,
    };

    const STRATEGY_ONE_FOR_ONE: std::ffi::c_int = 0;
    const STRATEGY_SIMPLE_ONE_FOR_ONE: std::ffi::c_int = 3;

    /// No-op child dispatch for the init-closure tests (mirrors the main test
    /// module's helper; defined locally to keep `pool_slot_tests` self-contained).
    unsafe extern "C-unwind" fn noop_child_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }
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
        // hew_supervisor_stop unregisters from the runtime-owned supervisor
        // roots, so the test needs a runtime installed (explicit-install model).
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        // Supervisor was never started (running == 0).
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead (supervisor not running)");
        assert_eq!(r.reason, ChildSlotReason::SupervisorShutdown as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_len_tracks_member_add_and_remove() {
        let _rt = crate::runtime_test_guard();
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
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 99) }, -1);
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn multiple_pool_slots_have_disjoint_key_spaces() {
        let _rt = crate::runtime_test_guard();
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

    // ── v0.6 init-closure restart model (C1 memory-safety core) ──────────────
    //
    // These tests exercise the init-thunk path directly at the runtime ABI: the
    // thunk produces a fresh, independently-owned state on the initial spawn and
    // every restart; thunk OOM fails closed; the config buffer is freed exactly
    // once at teardown; the byte-copy template is bypassed for init_fn children.

    use std::sync::atomic::AtomicUsize;

    /// Thunk-produced actor state: a single owned heap pointer (the "owned value"
    /// the init-closure model must re-clone fresh per incarnation) plus a tag.
    /// Models the `string`/`Vec`-bearing actor state without pulling the Hew
    /// runtime string type in — the ownership shape is identical.
    #[repr(C)]
    struct InitClosureState {
        /// Owned heap allocation; freed exactly once by the registered drop fn.
        owned: *mut u8,
        /// Tag distinguishing incarnations (read from config in the thunk).
        tag: u64,
    }

    /// Count of live `InitClosureState.owned` allocations. A correct
    /// first-spawn/crash-drop/restart-reclone/teardown sequence returns this to
    /// its starting value: every thunk allocation is matched by exactly one drop.
    static INIT_CLOSURE_LIVE_OWNED: AtomicUsize = AtomicUsize::new(0);
    /// Count of thunk invocations (proves restart re-RUNS the thunk).
    static INIT_CLOSURE_THUNK_CALLS: AtomicUsize = AtomicUsize::new(0);
    /// When set, the next thunk call returns a null state (models thunk OOM).
    static INIT_CLOSURE_FAIL_NEXT: AtomicBool = AtomicBool::new(false);

    /// The config struct the supervisor captures and the thunk reads.
    #[repr(C)]
    struct InitClosureConfig {
        seed: u64,
    }

    /// Codegen-shaped init thunk: malloc a fresh state + a fresh owned heap
    /// allocation, reading `config.seed` into the state tag. Returns a null
    /// state when `INIT_CLOSURE_FAIL_NEXT` is set (models OOM, fail-closed).
    unsafe extern "C" fn init_closure_thunk(config: *const c_void) -> HewChildInitResult {
        INIT_CLOSURE_THUNK_CALLS.fetch_add(1, Ordering::SeqCst);
        if INIT_CLOSURE_FAIL_NEXT.swap(false, Ordering::SeqCst) {
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        // SAFETY: config is the supervisor-owned buffer or null.
        let seed = if config.is_null() {
            0
        } else {
            unsafe { (*config.cast::<InitClosureConfig>()).seed }
        };
        // Fresh owned allocation — a NEW heap each incarnation, never aliased.
        // SAFETY: 8-byte alloc; null-checked by the caller's fail-closed path.
        let owned = unsafe { libc::malloc(8) }.cast::<u8>();
        if owned.is_null() {
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        INIT_CLOSURE_LIVE_OWNED.fetch_add(1, Ordering::SeqCst);
        // SAFETY: state wrapper alloc; null-checked below.
        let state = unsafe { libc::malloc(std::mem::size_of::<InitClosureState>()) }
            .cast::<InitClosureState>();
        if state.is_null() {
            // Free the owned alloc we just took before failing closed (no leak).
            // SAFETY: owned was just malloc'd.
            unsafe { libc::free(owned.cast::<c_void>()) };
            INIT_CLOSURE_LIVE_OWNED.fetch_sub(1, Ordering::SeqCst);
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        // SAFETY: state is valid for InitClosureState.
        unsafe {
            (*state).owned = owned;
            (*state).tag = seed;
        }
        HewChildInitResult {
            state: state.cast::<c_void>(),
            size: std::mem::size_of::<InitClosureState>(),
        }
    }

    /// Codegen-shaped state drop fn: frees the owned inner allocation exactly
    /// once (the wrapper itself is freed by the runtime's `libc::free`).
    unsafe extern "C" fn init_closure_drop(state: *mut c_void) {
        if state.is_null() {
            return;
        }
        let s = state.cast::<InitClosureState>();
        // SAFETY: s is a valid InitClosureState produced by the thunk.
        unsafe {
            if !(*s).owned.is_null() {
                libc::free((*s).owned.cast::<c_void>());
                (*s).owned = ptr::null_mut();
                INIT_CLOSURE_LIVE_OWNED.fetch_sub(1, Ordering::SeqCst);
            }
        }
    }

    /// Allocate a supervisor-owned config buffer (the capture codegen emits).
    fn make_config_buf(seed: u64) -> (*mut c_void, usize) {
        let size = std::mem::size_of::<InitClosureConfig>();
        // SAFETY: alloc + init; ownership transfers to the supervisor.
        let buf = unsafe { libc::malloc(size) }.cast::<InitClosureConfig>();
        assert!(!buf.is_null());
        // SAFETY: buf is valid.
        unsafe { (*buf).seed = seed };
        (buf.cast::<c_void>(), size)
    }

    fn init_closure_spec(config: *mut c_void, config_size: usize) -> HewChildSpec {
        HewChildSpec {
            name: ptr::null(),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            dispatch: Some(noop_child_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: Some(init_closure_thunk),
            config,
            config_size,
        }
    }

    #[test]
    fn init_fn_first_spawn_produces_fresh_owned_state_from_config() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) };
        assert!(!sup.is_null());
        let (cfg, cfg_size) = make_config_buf(256);

        let spec = init_closure_spec(cfg, cfg_size);
        // The thunk is the state source: add_child_spec spawns via the thunk.
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
            0
        );
        // Register the drop fn so the actor frees its owned field on teardown.
        unsafe { hew_supervisor_set_child_state_drop(sup, 0, init_closure_drop) };
        assert_eq!(unsafe { hew_supervisor_start(sup) }, 0);

        // The thunk ran exactly once (initial spawn) and produced one owned alloc.
        assert_eq!(INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst), 1);
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 1);

        // The byte-copy template was BYPASSED: the spec carries no init_state.
        unsafe {
            let s = &*sup;
            assert!(s.child_specs[0].init_state.is_null());
            assert_eq!(s.child_specs[0].init_state_size, 0);
            assert!(s.child_specs[0].init_fn.is_some());
            // The config buffer was adopted by the supervisor.
            assert_eq!(s.config_buf, cfg);
        }

        // The child actor holds the thunk-produced state with the config seed.
        unsafe {
            let child = (&*sup).children[0];
            assert!(!child.is_null());
            let st = (*child).state.cast::<InitClosureState>();
            assert!(!st.is_null());
            assert_eq!((*st).tag, 256, "tag reflects the DYNAMIC config seed");
        }

        unsafe { hew_supervisor_stop(sup) };
        // Teardown freed the owned alloc (drop fn) AND the config buffer — once.
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "no leak: the owned alloc was freed exactly once at teardown"
        );
    }

    #[test]
    fn init_fn_restart_re_runs_thunk_producing_independent_fresh_state() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60) };
        assert!(!sup_ptr.is_null());
        let (cfg, cfg_size) = make_config_buf(99);
        let spec = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };

        // Initial spawn: one thunk call, one live owned alloc.
        assert_eq!(INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst), 1);
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 1);

        // Drive a restart directly through the restart helper (the per-strategy
        // arm); the crashed child is freed by the runtime, then the thunk
        // re-runs to produce a SECOND independent state.
        unsafe {
            let s = &mut *sup_ptr;
            // Free the old child first (mirrors the crash teardown that
            // restart_with_budget_and_strategy / apply_restart performs before
            // re-spawn). This drops the first incarnation's owned alloc.
            let old = take_child_slot(s, 0);
            assert!(!old.is_null());
            actor::hew_actor_free(old);
        }
        // After the crash-drop, the first incarnation's owned alloc is gone.
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "crash-drop freed the first incarnation's owned state exactly once"
        );

        // Restart: the thunk re-runs and produces fresh state #2.
        let new_child = unsafe { restart_child_from_spec(&mut *sup_ptr, 0) };
        assert!(!new_child.is_null(), "restart re-spawned the child");
        assert_eq!(
            INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst),
            2,
            "restart RE-RAN the thunk"
        );
        // The live-owned count discipline is the load-bearing memory-safety
        // invariant: state #1 was dropped (count→0 above), state #2's thunk
        // allocated a fresh owned heap (count→1). Equal/unequal wrapper
        // *addresses* prove nothing (a freed wrapper's address can be reused by
        // the allocator); the count + a valid, config-derived state #2 is the
        // real proof the thunk re-ran and produced fresh, unaliased owned data.
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            1,
            "exactly one live owned alloc after restart (fresh state #2)"
        );

        // State #2 holds a fresh, non-null owned pointer and the config-derived
        // tag — proving the thunk re-cloned from config rather than replaying a
        // stale/aliased template value.
        let second_owned = unsafe { (*new_child).state.cast::<InitClosureState>() };
        unsafe {
            assert!(
                !(*second_owned).owned.is_null(),
                "state #2 owns a fresh heap allocation"
            );
            assert_eq!((*second_owned).tag, 99, "re-cloned from the same config");
        }

        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "no leak after restart: every thunk alloc freed exactly once"
        );
    }

    #[test]
    fn init_fn_thunk_oom_fails_closed_null_slot_no_breaker_advance() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(7);
        let spec = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };

        // Free the live child, then make the next thunk call fail (OOM).
        unsafe {
            let old = take_child_slot(&mut *sup_ptr, 0);
            actor::hew_actor_free(old);
        }
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);

        let breaker_state_before = unsafe { (&*sup_ptr).child_specs[0].circuit_breaker.state };
        INIT_CLOSURE_FAIL_NEXT.store(true, Ordering::SeqCst);

        // Restart with a failing thunk: fail closed (null new_child, null slot).
        let new_child = unsafe { restart_child_from_spec(&mut *sup_ptr, 0) };
        assert!(new_child.is_null(), "thunk OOM => fail closed (null child)");
        unsafe {
            let s = &*sup_ptr;
            assert!(
                s.children[0].is_null(),
                "the slot is left null on fail-closed"
            );
            assert_eq!(
                s.child_specs[0].circuit_breaker.state, breaker_state_before,
                "thunk OOM must NOT advance the circuit breaker (mirror clone-OOM)"
            );
        }
        // No owned alloc leaked on the OOM path (the thunk freed nothing because
        // it returned before allocating, or freed what it took).
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);

        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn init_fn_config_buffer_freed_exactly_once_at_teardown() {
        // Two init_fn children sharing ONE config buffer: the buffer is adopted
        // once and freed once at teardown (no double-free across children).
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) };
        let (cfg, cfg_size) = make_config_buf(11);

        let spec0 = init_closure_spec(cfg, cfg_size);
        let spec1 = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec0) },
            0
        );
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec1) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 1, init_closure_drop) };

        unsafe {
            // Both specs borrow the same single supervisor-owned buffer.
            let s = &*sup_ptr;
            assert_eq!(s.config_buf, cfg);
            assert_eq!(s.child_specs[0].config, cfg);
            assert_eq!(s.child_specs[1].config, cfg);
            assert_eq!(s.config_size, cfg_size);
        }
        // Two children => two thunk calls => two live owned allocs.
        assert_eq!(INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst), 2);
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 2);

        // Teardown frees both owned allocs (drop fns) and the config buffer once.
        // A double-free of the config buffer would abort under a hardened
        // allocator; reaching the assertion proves the single-free contract.
        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    // ── Static-backed pool (S3/S4): members resolve through live static slots ──

    /// Register N static children, then bind them as pool members via
    /// `pool_member_add_static`. The accessor resolves each member through its
    /// LIVE static slot, `pool_len` reports N, and an OOB index is Dead.
    #[test]
    fn static_backed_pool_resolves_members_through_live_slots() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_SIMPLE_ONE_FOR_ONE, 5, 60) };
        assert!(!sup.is_null());
        let (cfg, cfg_size) = make_config_buf(3);

        // Spawn 3 fungible members as static children (the bootstrap shape).
        for _ in 0..3 {
            let spec = init_closure_spec(cfg, cfg_size);
            assert_eq!(
                unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
                0
            );
        }
        for idx in 0..3 {
            unsafe { hew_supervisor_set_child_state_drop(sup, idx, init_closure_drop) };
        }

        // Register the pool slot and bind each static child as a member.
        let name = std::ffi::CString::new("workers").unwrap();
        let key = unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(key, 0);
        for idx in 0..3u32 {
            assert_eq!(
                unsafe { hew_supervisor_pool_member_add_static(sup, 0, idx) },
                0,
                "static member {idx} registered"
            );
        }

        unsafe { (*sup).running.store(1, Ordering::Release) };

        // len reports the fixed static-member count.
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 3);

        // Each member resolves Live to its static slot's actor.
        for idx in 0..3u32 {
            let r = unsafe { hew_supervisor_pool_child_get(sup, 0, idx) };
            assert!(r.is_live(), "member {idx} should be Live");
            let expected = unsafe { (&(*sup).children)[idx as usize] };
            assert_eq!(
                r.handle, expected,
                "member {idx} resolves to its live static-slot actor"
            );
        }

        // OOB index → Dead(UnknownSlot) (Vec[i] OOB parity).
        let oob = unsafe { hew_supervisor_pool_child_get(sup, 0, 3) };
        assert_eq!(oob.tag, 2, "index 3 is beyond the 3 members → Dead");
        assert_eq!(oob.reason, ChildSlotReason::UnknownSlot as u8);

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    /// After a static-backed member crashes and restarts, the accessor
    /// re-resolves to the FRESH actor — no stale PID is cached. This is the
    /// load-bearing restart re-resolution contract for the static pool.
    #[test]
    fn static_backed_pool_member_reresolves_after_restart() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_SIMPLE_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(42);
        // Two members.
        for _ in 0..2 {
            let spec = init_closure_spec(cfg, cfg_size);
            assert_eq!(
                unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
                0
            );
        }
        for idx in 0..2 {
            unsafe { hew_supervisor_set_child_state_drop(sup, idx, init_closure_drop) };
        }
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        for idx in 0..2u32 {
            unsafe { hew_supervisor_pool_member_add_static(sup, 0, idx) };
        }
        unsafe { (*sup).running.store(1, Ordering::Release) };

        // Snapshot member 1's actor before the crash.
        let before = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert!(before.is_live());
        let crashed_actor = before.handle;
        let thunks_before = INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst);

        // Crash member 1: free its static slot (mirrors the crash teardown), then
        // restart it through the per-child helper (the SIMPLE_ONE_FOR_ONE arm
        // calls exactly this).
        unsafe {
            let old = take_child_slot(&mut *sup, 1);
            assert_eq!(old, crashed_actor);
            actor::hew_actor_free(old);
        }
        // While the slot is null, the accessor reports the member as restarting
        // (Transient), NEVER the freed actor — a stale-PID cache would return the
        // crashed handle here.
        let mid = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert_ne!(
            mid.tag, 0,
            "a null slot must not resolve as Live (no stale PID)"
        );

        let new_actor = unsafe { restart_child_from_spec(&mut *sup, 1) };
        assert!(!new_actor.is_null(), "member 1 restarted");
        assert_eq!(
            INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst),
            thunks_before + 1,
            "restart RE-RAN the per-member init thunk"
        );

        // The accessor re-resolves member 1 to the FRESH actor through the LIVE
        // static slot — no stale PID is cached in the pool. (Comparing against the
        // crashed pointer proves nothing: a freed wrapper's address can be
        // reused; the live-slot identity is the real proof.)
        let after = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert!(after.is_live(), "member 1 is Live again after restart");
        assert_eq!(
            after.handle, new_actor,
            "pool re-resolves to the restarted actor through the live static slot"
        );
        assert_eq!(after.handle, unsafe { (&(*sup).children)[1] });

        // Member 0 is untouched by the SIMPLE_ONE_FOR_ONE per-member restart.
        let member0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert!(member0.is_live(), "member 0 stayed live");

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    /// Driving the `SIMPLE_ONE_FOR_ONE` strategy arm (not the helper directly)
    /// restarts the crashed pool member per-member and the pool re-resolves it.
    /// This proves the arm is wired (it was previously an empty no-op).
    #[test]
    fn simple_one_for_one_arm_restarts_crashed_pool_member() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_SIMPLE_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(13);
        for _ in 0..2 {
            let spec = init_closure_spec(cfg, cfg_size);
            assert_eq!(
                unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
                0
            );
        }
        for idx in 0..2 {
            unsafe { hew_supervisor_set_child_state_drop(sup, idx, init_closure_drop) };
        }
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        for idx in 0..2u32 {
            unsafe { hew_supervisor_pool_member_add_static(sup, 0, idx) };
        }
        unsafe { (*sup).running.store(1, Ordering::Release) };

        let thunks_before = INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst);
        // Crash member 0: free its slot, then drive the strategy arm (which now
        // routes SIMPLE_ONE_FOR_ONE → restart_child_from_spec for the member).
        unsafe {
            let old = take_child_slot(&mut *sup, 0);
            actor::hew_actor_free(old);
        }
        unsafe { restart_with_budget_and_strategy(&mut *sup, 0) };

        assert_eq!(
            INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst),
            thunks_before + 1,
            "the SIMPLE_ONE_FOR_ONE arm restarted the member (re-ran its thunk)"
        );
        let after = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert!(
            after.is_live(),
            "member 0 is Live again after the arm restart"
        );
        assert_eq!(after.handle, unsafe { (&(*sup).children)[0] });

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }
}
