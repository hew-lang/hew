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
use std::time::{Duration, Instant};

use crate::actor::{self, HewActor, HewActorOpts};
use crate::internal::types::{
    HewActorState, HewDispatchFn, HewLifecycleFn, HewOnCrashFn, HewSysDispatchFn,
};
use crate::io_time::hew_now_ms;
use crate::mailbox;
use crate::mailbox_header::HewSysMsg;
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

    // SAFETY: copy non-roster scalars from the live allocation.
    let (self_actor, strategy) = unsafe { ((*sup).self_actor, (*sup).strategy) };
    let self_actor_id = if self_actor.is_null() {
        0
    } else {
        // SAFETY: self_actor belongs to the live supervisor.
        unsafe { (*self_actor).id }
    };
    let label = format!(
        "⊞ supervisor:{self_actor_id} [{}]",
        supervisor_strategy_name(strategy)
    );
    append_tree_row(json, first, depth, &label, "Supervisor");

    // Snapshot display data and exact nested pins under this node's roster,
    // then release it before recursive descent. No code ever holds two roster
    // mutexes, so parent/child traversal cannot invert another lifecycle path.
    let (child_rows, nested, pool_rows) = {
        // SAFETY: top-level supervisor pointers remain valid while registered;
        // nested calls carry a stable pin acquired by their parent snapshot.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        let child_rows = roster
            .children
            .iter()
            .take(roster.child_count)
            .enumerate()
            .map(|(index, child)| {
                let spec = &roster.child_specs[index];
                let name = child_name(spec.name, &format!("child[{index}]"));
                let restarts = if spec.circuit_breaker.crash_stats.is_null() {
                    0
                } else {
                    // SAFETY: crash stats pointer belongs to the child spec.
                    unsafe { (*spec.circuit_breaker.crash_stats).total_crashes }
                };
                (
                    format!("  {name} (restarts: {restarts})"),
                    actor_state_name(*child),
                )
            })
            .collect::<Vec<_>>();
        let nested = roster
            .child_supervisors
            .iter()
            .copied()
            .zip(roster.child_supervisor_tokens.iter().copied())
            .filter_map(|(child, token)| {
                let pin = crate::lifetime::local_handles::pin_current_supervisor(token)?;
                (pin.supervisor() == child).then_some((child, pin))
            })
            .collect::<Vec<_>>();
        let pool_rows = roster
            .pool_slots
            .iter()
            .enumerate()
            .map(|(i, pool)| {
                let spec = &roster.pool_specs[i];
                let name = child_name(spec.name, &format!("pool[{i}]"));
                // SAFETY: the pool is Box-owned and cannot be removed while
                // this roster guard is held.
                let member_count = unsafe { crate::pool::hew_pool_size(*pool) };
                format!("  {name} (members: {member_count})")
            })
            .collect::<Vec<_>>();
        (child_rows, nested, pool_rows)
    };

    for (label, state) in child_rows {
        append_tree_row(json, first, depth + 1, &label, state);
    }
    for (child_sup, _pin) in nested {
        append_supervisor_rows(json, first, child_sup, depth + 1);
    }
    for label in pool_rows {
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
    /// For stable-role lookups (`hew_local_pid_supervisor_child_get`), this
    /// field carries the current child incarnation's `HewLocalPidId` encoded as
    /// a pointer-width integer. It must be passed to stable local-pid operations
    /// and never dereferenced.
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
    /// Human-readable pool name (C string, heap-allocated via `cstr_strdup`).
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
pub unsafe extern "C-unwind" fn hew_trap_with_code(code: i32) {
    crate::cont::abort_if_crash_cleanup_finalizer_trap(trap_kind_name(code));
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
///
/// Delegates to the canonical [`ExitReason`] naming so every registered trap
/// code (including additions) prints its real name here — the local table this
/// replaces had drifted, printing bare "Trap" for codes 207+. A raw signal
/// number (not a registered trap code) stays "Trap": this diagnostic names
/// codegen-emitted trap kinds, not hardware signals.
fn trap_kind_name(code: i32) -> &'static str {
    match ExitReason::from_error_code(code) {
        ExitReason::Signal(_) | ExitReason::Normal => "Trap",
        reason => reason.trap_kind_name(),
    }
}

/// Payload for [`HewSysMsg::DelayedRestart`] system messages.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct DelayedRestartEvent {
    child_identity: u64,
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
    pub coalesce_key_fn: Option<mailbox::HewCoalesceKeyFn>,
    pub coalesce_fallback: c_int,
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
    /// Typed destructor for queued message payloads evicted before dispatch.
    pub message_drop_fn: Option<mailbox::HewMessageDropFn>,
    /// The child actor's SYSTEM dispatch entry point
    /// (`__hew_actor_sys_dispatch_<Actor>`), or `None` when the actor declares
    /// no `#[on(exit)]` / `#[on(down)]` hook.
    ///
    /// Carried IN the spec rather than set post-hoc so EVERY incarnation gets
    /// it: the initial supervised spawn happens inside `add_child_spec` before
    /// any setter could run, and each restart re-registers from this field.
    ///
    /// ABI: the final trailing `#[repr(C)]` field; the codegen-emitted
    /// `hew_child_spec_struct_type` mirror appends a matching `ptr` slot.
    /// Field-order drift here is wrong-code at the FFI boundary.
    pub sys_dispatch: Option<HewSysDispatchFn>,
}

/// Child lifecycle event (payload of [`HewSysMsg::ChildStopped`] /
/// [`HewSysMsg::ChildCrashed`]).
///
/// `crash_code` carries the trap-kind integer (`HEW_TRAP_*` constants, 201–205,
/// or other actor-defined error codes) captured from the child actor's
/// `error_code` slot at the moment of the trap. It is meaningful only when
/// `exit_state == HewActorState::Crashed`; for normal stops it is `0`.
/// Routing this through the event payload lets the supervisor record the real
/// trap code in crash-stats and forward it to a registered `on_crash` handler
/// instead of falling back to the historical SIGSEGV placeholder (`11`).
///
/// `child_index` is `u32`: a negative index is unrepresentable. It formerly
/// carried `-1` to retag the whole record as a child-SUPERVISOR escalation,
/// which silently changed the meaning of the sibling `child_id` field
/// (actor id → index in `child_supervisors`). That case is now
/// [`HewSysMsg::ChildSupervisorEscalated`] with its own payload, so no field's
/// meaning depends on another's value.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct ChildEvent {
    child_index: u32,
    child_id: u64,
    exit_state: c_int,
    crash_code: c_int,
}

/// Payload of [`HewSysMsg::ChildSupervisorEscalated`]: a child SUPERVISOR
/// exhausted its restart budget and escalated to this supervisor.
///
/// A distinct type from [`ChildEvent`] because it names a distinct thing:
/// `supervisor_index` indexes `child_supervisors`, not `children`. No field
/// here changes another field's meaning.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct ChildSupervisorEscalation {
    supervisor_index: u32,
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

/// All mutable child, nested-supervisor, pool, restart-budget, and child-config
/// state. Keeping this data inside the mutex makes unlocked roster mutation
/// unrepresentable and prevents a whole-`HewSupervisor` reference from
/// aliasing concurrent raw roster updates.
struct SupervisorRoster {
    children: Vec<*mut HewActor>,
    child_specs: Vec<InternalChildSpec>,
    child_count: usize,
    next_child_spec_identity: u64,
    child_supervisors: Vec<*mut HewSupervisor>,
    child_supervisor_tokens: Vec<crate::lifetime::local_handles::HewLocalPidId>,
    child_supervisor_specs: Vec<Option<SupervisorChildSpec>>,
    restart_times: [u64; MAX_RESTARTS_TRACK],
    restart_count: usize,
    restart_head: usize,
    restart_notify: Option<Arc<(Mutex<usize>, Condvar)>>,
    pool_slots: Vec<*mut HewActorPool>,
    pool_specs: Vec<InternalPoolSpec>,
    config_buf: *mut c_void,
    config_size: usize,
    config_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
}

// SAFETY: raw roster pointers are allocation identities, not transferable
// borrows. Every access occurs while the owning supervisor's mutex is held;
// destruction first closes external access and drains admitted operations.
unsafe impl Send for SupervisorRoster {}

// ---------------------------------------------------------------------------
// Supervisor struct
// ---------------------------------------------------------------------------

/// Supervisor managing a set of child actors.
pub struct HewSupervisor {
    /// Runtime authority that owns this allocation's stable handle control.
    runtime: *const crate::runtime::RuntimeInner,
    /// Direct stable identity for this exact supervisor allocation.
    local_pid_id: crate::lifetime::local_handles::HewLocalPidId,
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,

    running: AtomicI32,
    /// Wait-free cancellation observation for public/hot read paths. The
    /// restart-timer control mirrors this state under its mutex only for timer
    /// wakeup and raw-borrow exclusion.
    cancelled: AtomicBool,
    /// Shared cancellation and lifetime authority for delayed-restart threads.
    /// A timer holds an `Arc` lease from before spawn until its final raw
    /// supervisor access is complete; teardown cancels/wakes the control and
    /// waits for every lease to drain before reclaiming this allocation.
    restart_timers: Arc<RestartTimerControl>,
    self_actor: *mut HewActor,
    /// Owns every mutable roster collection and its related count/config state.
    /// Any raw template read is converted to an owned restart snapshot while
    /// this lock is held; no spec-owned pointer crosses the guard boundary.
    roster: Mutex<SupervisorRoster>,

    /// Parent supervisor (set by `hew_supervisor_add_child_supervisor`).
    parent: *mut HewSupervisor,
    /// Index of this supervisor in parent's `child_supervisors` vec.
    index_in_parent: usize,

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
}

const SUPERVISOR_PIN_DRAIN_TIMEOUT: Duration = Duration::from_secs(30);
/// A public stop must never spin forever when a failed worker left a
/// supervisor/child activation Runnable or Running.  This matches the
/// actor-termination quiescence window: on expiry we preserve the allocation
/// and hand it back to canonical post-worker cleanup rather than free live
/// state.
const SUPERVISOR_QUIESCENCE_TIMEOUT: Duration = Duration::from_secs(5);
/// Canonical cleanup runs after workers have stopped, but delayed-restart
/// threads can still hold a raw supervisor borrow.  It gets the same bounded
/// lease: expiry retains the allocation instead of freeing under that borrow.
const SUPERVISOR_CLEANUP_TIMER_DRAIN_TIMEOUT: Duration = Duration::from_secs(5);

#[derive(Default)]
struct RestartTimerState {
    cancelled: bool,
    pending: usize,
}

/// Arc-owned cancellation and raw-borrow authority for delayed restarts.
///
/// The state mutex deliberately covers the timer's final supervisor access.
/// Consequently, once [`Self::cancel`] returns, no timer can begin or continue
/// dereferencing the supervisor. The pending count is decremented only after
/// that access ends, giving teardown a precise lifetime barrier without making
/// a 30-second backoff an uninterruptible reclamation delay.
struct RestartTimerControl {
    state: Mutex<RestartTimerState>,
    changed: Condvar,
}

impl RestartTimerControl {
    fn new() -> Self {
        Self {
            state: Mutex::new(RestartTimerState::default()),
            changed: Condvar::new(),
        }
    }

    fn begin(self: &Arc<Self>) -> Option<RestartTimerLease> {
        let mut state = self.state.lock_or_recover();
        if state.cancelled {
            return None;
        }
        state.pending += 1;
        Some(RestartTimerLease {
            control: Arc::clone(self),
        })
    }

    fn cancel(&self) {
        let mut state = self.state.lock_or_recover();
        state.cancelled = true;
        self.changed.notify_all();
    }

    fn wait_for_drain(&self, deadline: Instant) -> bool {
        let mut state = self.state.lock_or_recover();
        while state.pending != 0 {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (next, _) = self.changed.wait_timeout_or_recover(state, remaining);
            state = next;
        }
        true
    }

    #[cfg(test)]
    fn pending_for_test(&self) -> usize {
        self.state.lock_or_recover().pending
    }
}

struct RestartTimerLease {
    control: Arc<RestartTimerControl>,
}

impl RestartTimerLease {
    /// Wait interruptibly, then perform the raw supervisor access while the
    /// cancellation mutex excludes shutdown from publishing cancellation.
    fn wait_and_run(self, delay: Duration, on_elapsed: impl FnOnce()) {
        let deadline = Instant::now() + delay;
        let mut state = self.control.state.lock_or_recover();
        loop {
            if state.cancelled {
                break;
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                on_elapsed();
                break;
            }
            let (next, _) = self
                .control
                .changed
                .wait_timeout_or_recover(state, remaining);
            state = next;
        }
        drop(state);
    }
}

impl Drop for RestartTimerLease {
    fn drop(&mut self) {
        let mut state = self.control.state.lock_or_recover();
        debug_assert!(state.pending > 0);
        state.pending = state.pending.saturating_sub(1);
        self.control.changed.notify_all();
    }
}

struct ClosedSupervisorAccess {
    control: Arc<crate::lifetime::local_handles::SupervisorControl>,
    handles: *const crate::lifetime::local_handles::LocalHandles,
}

#[cfg(all(test, not(target_arch = "wasm32")))]
type SupervisorTestHook = Option<Arc<dyn Fn() + Send + Sync>>;

#[cfg(all(test, not(target_arch = "wasm32")))]
static SUPERVISOR_ACCESS_HOOK: std::sync::OnceLock<Mutex<SupervisorTestHook>> =
    std::sync::OnceLock::new();
#[cfg(all(test, not(target_arch = "wasm32")))]
static SUPERVISOR_CLOSE_HOOK: std::sync::OnceLock<Mutex<SupervisorTestHook>> =
    std::sync::OnceLock::new();
#[cfg(all(test, not(target_arch = "wasm32")))]
static SUPERVISOR_TEARDOWN_HOOK: std::sync::OnceLock<Mutex<SupervisorTestHook>> =
    std::sync::OnceLock::new();
#[cfg(all(test, not(target_arch = "wasm32")))]
static RESTART_SPEC_SNAPSHOT_HOOK: std::sync::OnceLock<Mutex<SupervisorTestHook>> =
    std::sync::OnceLock::new();
#[cfg(all(test, not(target_arch = "wasm32")))]
static DYNAMIC_CHILD_RESERVED_HOOK: std::sync::OnceLock<Mutex<SupervisorTestHook>> =
    std::sync::OnceLock::new();

#[cfg(all(test, not(target_arch = "wasm32")))]
struct SupervisorTestHookGuard(&'static std::sync::OnceLock<Mutex<SupervisorTestHook>>);

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for SupervisorTestHookGuard {
    fn drop(&mut self) {
        *self.0.get_or_init(|| Mutex::new(None)).lock_or_recover() = None;
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_supervisor_access_hook_for_test(
    hook: Arc<dyn Fn() + Send + Sync>,
) -> SupervisorTestHookGuard {
    *SUPERVISOR_ACCESS_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover() = Some(hook);
    SupervisorTestHookGuard(&SUPERVISOR_ACCESS_HOOK)
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_supervisor_close_hook_for_test(
    hook: Arc<dyn Fn() + Send + Sync>,
) -> SupervisorTestHookGuard {
    *SUPERVISOR_CLOSE_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover() = Some(hook);
    SupervisorTestHookGuard(&SUPERVISOR_CLOSE_HOOK)
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_supervisor_teardown_hook_for_test(
    hook: Arc<dyn Fn() + Send + Sync>,
) -> SupervisorTestHookGuard {
    *SUPERVISOR_TEARDOWN_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover() = Some(hook);
    SupervisorTestHookGuard(&SUPERVISOR_TEARDOWN_HOOK)
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_restart_spec_snapshot_hook_for_test(
    hook: Arc<dyn Fn() + Send + Sync>,
) -> SupervisorTestHookGuard {
    *RESTART_SPEC_SNAPSHOT_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover() = Some(hook);
    SupervisorTestHookGuard(&RESTART_SPEC_SNAPSHOT_HOOK)
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_dynamic_child_reserved_hook_for_test(
    hook: Arc<dyn Fn() + Send + Sync>,
) -> SupervisorTestHookGuard {
    *DYNAMIC_CHILD_RESERVED_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover() = Some(hook);
    SupervisorTestHookGuard(&DYNAMIC_CHILD_RESERVED_HOOK)
}

fn run_supervisor_access_hook_for_test() {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if let Some(hook) = SUPERVISOR_ACCESS_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover()
        .clone()
    {
        hook();
    }
}

fn run_supervisor_close_hook_for_test() {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if let Some(hook) = SUPERVISOR_CLOSE_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover()
        .clone()
    {
        hook();
    }
}

fn run_supervisor_teardown_hook_for_test() {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if let Some(hook) = SUPERVISOR_TEARDOWN_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover()
        .clone()
    {
        hook();
    }
}

fn run_restart_spec_snapshot_hook_for_test() {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if let Some(hook) = RESTART_SPEC_SNAPSHOT_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover()
        .clone()
    {
        hook();
    }
}

fn run_dynamic_child_reserved_hook_for_test() {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if let Some(hook) = DYNAMIC_CHILD_RESERVED_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover()
        .clone()
    {
        hook();
    }
}

unsafe fn close_supervisor_access(
    sup: *mut HewSupervisor,
    timeout: Duration,
) -> Option<ClosedSupervisorAccess> {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if FAIL_NEXT_SUPERVISOR_ACCESS_CLOSE.with(|slot| slot.replace(false)) {
        return None;
    }
    // SAFETY: callers provide a live allocation created by `hew_supervisor_new`;
    // copy scalars without creating a whole-supervisor reference.
    let (runtime_ptr, token) = unsafe { ((*sup).runtime, (*sup).local_pid_id) };
    if runtime_ptr.is_null() {
        return None;
    }
    // SAFETY: the runtime outlives every registered supervisor control.
    let runtime = unsafe { &*runtime_ptr };
    let control = runtime
        .local_handles
        .supervisor_control_for_raw(token, sup)?;
    let won_close = control.begin_close();
    runtime.local_handles.retire_supervisor_route(&control);
    if won_close {
        run_supervisor_close_hook_for_test();
    }
    if !control.wait_for_pins(timeout) {
        return None;
    }
    Some(ClosedSupervisorAccess {
        control,
        handles: &raw const runtime.local_handles,
    })
}

unsafe fn begin_supervisor_teardown(
    sup: *mut HewSupervisor,
) -> Option<crate::lifetime::local_handles::SupervisorTeardownLease> {
    // SAFETY: callers provide a live allocation created by `hew_supervisor_new`.
    let runtime_ptr = unsafe { (*sup).runtime };
    if runtime_ptr.is_null() {
        return None;
    }
    // SAFETY: cleanup cannot reclaim the runtime until this admission either
    // fails under the closed gate or returns a lease that cleanup must drain.
    unsafe { &*runtime_ptr }
        .local_handles
        .begin_supervisor_teardown()
}

#[cfg(all(test, not(target_arch = "wasm32")))]
unsafe fn close_supervisor_access_with_timeout(sup: *mut HewSupervisor, timeout: Duration) -> bool {
    // SAFETY: forwarded from the caller under the same allocation contract.
    unsafe { close_supervisor_access(sup, timeout) }.is_some()
}

fn finish_supervisor_reclamation(access: &ClosedSupervisorAccess) {
    // SAFETY: the runtime remains installed while supervisor reclamation runs.
    unsafe { &*access.handles }.remove_supervisor_control(&access.control);
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
    /// Monotonic identity for this exact roster entry. A restart snapshots it
    /// under `roster` and rechecks it before publishing, so a concurrent
    /// swap-remove cannot redirect a completed spawn into another child's slot.
    identity: u64,
    /// Metadata revision for non-template callback fields. Restarts validate
    /// it with `identity` so a lifecycle/init setter cannot be lost to a late
    /// publication from an earlier snapshot.
    revision: u64,
    name: *mut c_char,
    /// Immutable, reference-counted template generation. Restart clones this
    /// Arc under `roster`, then may call the user/codegen clone callback
    /// without holding a supervisor lock. A concurrent setter installs a new
    /// generation, while the old allocation remains alive until every restart
    /// snapshot releases its lease.
    state_template: Arc<ChildStateTemplate>,
    dispatch: Option<HewDispatchFn>,
    sys_dispatch: Option<HewSysDispatchFn>,
    restart_policy: c_int,
    mailbox_capacity: c_int,
    overflow: c_int,
    coalesce_key_fn: Option<mailbox::HewCoalesceKeyFn>,
    coalesce_fallback: c_int,
    message_drop_fn: Option<mailbox::HewMessageDropFn>,
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

/// One state-drop descriptor shared by every immutable template generation.
/// The setter may arrive after the initial generation was constructed; an
/// atomic function word lets the eventual last allocation owner observe it
/// without mutating or replacing an Arc generation.
#[derive(Debug)]
struct ChildStateDropDescriptor {
    function: AtomicUsize,
}

impl ChildStateDropDescriptor {
    fn new() -> Self {
        Self {
            function: AtomicUsize::new(0),
        }
    }

    fn store(&self, function: unsafe extern "C" fn(*mut c_void)) {
        self.function.store(function as usize, Ordering::Release);
    }

    fn load(&self) -> Option<unsafe extern "C" fn(*mut c_void)> {
        let function = self.function.load(Ordering::Acquire);
        if function == 0 {
            None
        } else {
            // SAFETY: the only non-zero values stored above came from this
            // exact C-ABI function-pointer type, and function/data pointers are
            // the same width on every supported Hew runtime target.
            Some(unsafe {
                std::mem::transmute::<usize, unsafe extern "C" fn(*mut c_void)>(function)
            })
        }
    }
}

/// The allocation retained by one or more immutable template generations.
#[derive(Debug)]
struct ChildStateTemplateAllocation {
    state: *mut c_void,
    size: usize,
    /// True only after a clone callback produced this allocation. Legacy
    /// byte-copy templates deliberately own wrapper bytes but not typed fields.
    owns_typed_fields: bool,
    state_drop: Arc<ChildStateDropDescriptor>,
}

// SAFETY: the allocation is immutable after construction. Clone callbacks
// receive only a const pointer; destruction occurs after the final Arc lease.
unsafe impl Send for ChildStateTemplateAllocation {}
// SAFETY: same immutable-allocation/final-Arc argument as the Send impl above.
unsafe impl Sync for ChildStateTemplateAllocation {}

impl Drop for ChildStateTemplateAllocation {
    fn drop(&mut self) {
        if self.state.is_null() {
            return;
        }
        if self.owns_typed_fields {
            if let Some(drop_fn) = self.state_drop.load() {
                // SAFETY: a successful state clone produced an independently
                // owned allocation for this exact actor-state layout.
                unsafe { drop_fn(self.state) };
            }
        }
        // SAFETY: every template wrapper is allocated by libc::malloc or a
        // clone callback whose contract requires malloc-compatible output.
        unsafe { libc::free(self.state) }; // ALLOCATOR-PAIRING: libc
        self.state = ptr::null_mut();
    }
}

/// Immutable template metadata published atomically under `roster`.
#[derive(Debug)]
struct ChildStateTemplate {
    allocation: Arc<ChildStateTemplateAllocation>,
    clone_fn: Option<actor::HewStateCloneFn>,
}

impl Drop for InternalChildSpec {
    fn drop(&mut self) {
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
            identity: 0,
            revision: 1,
            name: ptr::null_mut(),
            state_template: Arc::new(ChildStateTemplate {
                allocation: Arc::new(ChildStateTemplateAllocation {
                    state: ptr::null_mut(),
                    size: 0,
                    owns_typed_fields: false,
                    state_drop: Arc::new(ChildStateDropDescriptor::new()),
                }),
                clone_fn: None,
            }),
            dispatch: None,
            sys_dispatch: None,
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            restart_delay_ms: 0,
            max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
            next_restart_time_ns: 0,
            circuit_breaker: CircuitBreakerState::default(),
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
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
            .finish_non_exhaustive()
    }
}

// SAFETY: runtime-callable roster access is serialized by `roster`;
// remaining mutable fields are either atomic, supervisor-actor-owned, or used
// only during exclusive construction/teardown.
unsafe impl Send for HewSupervisor {}
// SAFETY: shared runtime access follows the same field-level authority model;
// no roster reference is created before acquiring `roster`.
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
fn restart_within_window(window_secs: c_int, roster: &SupervisorRoster) -> c_int {
    // ROSTER-GUARDED-HELPER: callers bound this borrow by their MutexGuard.
    // SAFETY: no preconditions.
    let now = unsafe { hew_now_ms() };
    let window_ms = (window_secs as u64).wrapping_mul(1000);

    let mut count: c_int = 0;
    let limit = roster.restart_count.min(MAX_RESTARTS_TRACK);
    for i in 0..limit {
        let idx = (roster.restart_head + MAX_RESTARTS_TRACK - 1 - i) % MAX_RESTARTS_TRACK;
        if now.wrapping_sub(roster.restart_times[idx]) <= window_ms {
            count += 1;
        } else {
            break;
        }
    }
    count
}

/// Record a restart timestamp.
fn record_restart(roster: &mut SupervisorRoster) {
    // ROSTER-GUARDED-HELPER: callers bound this borrow by their MutexGuard.
    // SAFETY: no preconditions.
    roster.restart_times[roster.restart_head] = unsafe { hew_now_ms() };
    roster.restart_head = (roster.restart_head + 1) % MAX_RESTARTS_TRACK;
    if roster.restart_count < MAX_RESTARTS_TRACK {
        roster.restart_count += 1;
    }
}

/// Resolve the supervisor's own actor id for trace attribution. Returns `0`
/// when the supervisor has no backing actor yet (pre-start). Read-only; used
/// only to tag emitted supervisor lifecycle events.
fn supervisor_actor_id(sup: *mut HewSupervisor) -> u64 {
    // SAFETY: callers keep the supervisor live for this scalar read.
    let self_actor = unsafe { (*sup).self_actor };
    if self_actor.is_null() {
        0
    } else {
        // SAFETY: self_actor belongs to the live supervisor for the duration
        // of this dispatch.
        unsafe { (*self_actor).id }
    }
}

/// Escalate a failure to the parent supervisor.
///
/// Sends a [`HewSysMsg::ChildSupervisorEscalated`] signal carrying this
/// supervisor's index in the parent's `child_supervisors` vec. Its own typed
/// variant, not a `ChildCrashed` retagged by a negative index.
///
/// # Safety
///
/// `sup.parent` must be non-null and point to a valid `HewSupervisor`.
fn escalate_to_parent(sup: *mut HewSupervisor) {
    // SAFETY: caller guarantees the child and its non-null parent edge remain
    // live through this synchronous notification; copy only scalar fields.
    let (parent, index_in_parent) = unsafe { ((*sup).parent, (*sup).index_in_parent) };
    // SAFETY: caller guarantees parent is valid.
    let parent_actor = unsafe { (*parent).self_actor };
    if parent_actor.is_null() {
        return;
    }
    let Ok(supervisor_index) = u32::try_from(index_in_parent) else {
        eprintln!(
            "[supervisor] refusing to escalate: child-supervisor index {index_in_parent} exceeds u32"
        );
        return;
    };
    let event = ChildSupervisorEscalation {
        supervisor_index,
        exit_state: HewActorState::Crashed as c_int,
        // Child-supervisor escalation: no single trap code applies to the
        // subtree-restart-budget exhaustion that triggered this escalation.
        // Use the honest unknown value rather than fabricating a signal number.
        crash_code: 0,
    };
    // SAFETY: caller keeps `sup` live while this scalar tracing id is copied.
    let sup_actor_id = supervisor_actor_id(sup);
    // SAFETY: parent.self_actor is valid.
    unsafe {
        let _ = actor::send_system_message(
            parent_actor,
            HewSysMsg::ChildSupervisorEscalated,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildSupervisorEscalation>(),
        );
    }

    // Observability (read-only side effect, AFTER the escalation decision and
    // dispatch): record that this supervisor escalated to its parent. Never
    // gates control flow.
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_ESCALATE,
        i32::try_from(index_in_parent).unwrap_or(i32::MAX),
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

fn schedule_delayed_restart(sup: *mut HewSupervisor, child_identity: u64, delay: Duration) {
    // SAFETY: caller keeps `sup` live through timer admission; the returned
    // lease extends raw-pointer lifetime through the spawned closure.
    let Some(timer) = (unsafe { &(*sup).restart_timers }).begin() else {
        return;
    };
    let sup_addr = sup as usize;
    let spawn_result = std::thread::Builder::new()
        .name("hew-supervisor-restart-timer".to_owned())
        .spawn(move || {
            timer.wait_and_run(delay, || {
                let sup_ptr = sup_addr as *mut HewSupervisor;
                // SAFETY: the timer lease's pending count keeps `sup_ptr`
                // allocated, and its state mutex excludes cancellation for the
                // complete raw access. Once cancellation is published this
                // closure can never run.
                unsafe {
                    let self_actor = (*sup_ptr).self_actor;
                    if !(*sup_ptr).cancelled.load(Ordering::Acquire)
                        && (*sup_ptr).running.load(Ordering::Acquire) != 0
                        && !self_actor.is_null()
                    {
                        let event = DelayedRestartEvent { child_identity };
                        let _ = actor::send_system_message(
                            self_actor,
                            HewSysMsg::DelayedRestart,
                            (&raw const event).cast::<c_void>().cast_mut(),
                            std::mem::size_of::<DelayedRestartEvent>(),
                        );
                    }
                }
            });
        });
    if let Err(error) = spawn_result {
        // The failed builder drops the closure and therefore its timer lease,
        // restoring the pending count before returning.
        set_last_error(format!(
            "failed to spawn delayed supervisor restart timer: {error}"
        ));
    }
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
fn restart_notify_snapshot(sup: *mut HewSupervisor) -> Option<Arc<(Mutex<usize>, Condvar)>> {
    // SAFETY: callers keep `sup` live; notification option publication shares
    // roster authority so no whole-supervisor reference crosses a roster lock.
    let roster = unsafe { &(*sup).roster }.lock_or_recover();
    roster.restart_notify.as_ref().map(Arc::clone)
}

fn notify_restart(sup: *mut HewSupervisor) {
    if let Some(pair) = restart_notify_snapshot(sup) {
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
fn wake_restart_await_waiters(sup: *mut HewSupervisor) {
    // SAFETY: caller keeps `sup` live while its waiter registry is drained.
    let waiters: Vec<RestartAwaitWaiter> =
        std::mem::take(&mut *unsafe { &(*sup).restart_await_waiters }.lock_or_recover());
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

fn load_child_slot(sup: *mut HewSupervisor, index: usize) -> *mut HewActor {
    // SAFETY: callers keep `sup` live for this synchronous lookup.
    let roster = unsafe { &(*sup).roster }.lock_or_recover();
    roster
        .children
        .get(index)
        .copied()
        .unwrap_or(ptr::null_mut())
}

fn store_child_slot(sup: *mut HewSupervisor, index: usize, child: *mut HewActor) {
    // SAFETY: callers keep the supervisor allocation live.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    if let Some(slot) = roster.children.get_mut(index) {
        *slot = child;
    }
}

fn take_child_slot(sup: *mut HewSupervisor, index: usize) -> *mut HewActor {
    // SAFETY: callers keep the supervisor allocation live.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    let Some(slot) = roster.children.get_mut(index) else {
        return ptr::null_mut();
    };
    let child = *slot;
    *slot = ptr::null_mut();
    child
}

/// Detach the current slot for one stable spec identity. The identity lookup
/// and null publication are indivisible with dynamic swap-remove.
fn take_child_slot_by_identity(sup: *mut HewSupervisor, identity: u64) -> *mut HewActor {
    // SAFETY: every caller keeps the supervisor live for this synchronous
    // operation; `roster` is the roster authority.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    let Some(index) = roster
        .child_specs
        .iter()
        .position(|candidate| candidate.identity == identity)
    else {
        return ptr::null_mut();
    };
    let child = roster.children[index];
    roster.children[index] = ptr::null_mut();
    child
}

fn take_child_slot_for_event(
    sup: *mut HewSupervisor,
    index: usize,
    actor_id: u64,
) -> Option<(*mut HewActor, u64)> {
    // SAFETY: dispatcher keeps `sup` live; the guard makes event validation,
    // stable-spec capture, and slot detachment one roster transaction.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    let child = roster.children.get(index).copied()?;
    if child.is_null() {
        return None;
    }
    // SAFETY: the non-null roster slot owns the actor for this critical section.
    if unsafe { (*child).id } != actor_id {
        return None;
    }
    let identity = roster.child_specs.get(index)?.identity;
    roster.children[index] = ptr::null_mut();
    Some((child, identity))
}

/// Stop this supervisor, notify waiters, and escalate to the parent if present.
fn stop_and_maybe_escalate(sup: *mut HewSupervisor) {
    // SAFETY: callers keep the allocation live through cancellation.
    publish_supervisor_cancellation(sup);
    // SAFETY: callers keep the allocation live through these field operations.
    unsafe { (*sup).running.store(0, Ordering::Release) };
    notify_restart(sup);
    // SAFETY: callers keep the supervisor allocation live through escalation.
    if unsafe { !(*sup).parent.is_null() } {
        escalate_to_parent(sup);
    }
}

fn stop_deferred_supervisor(deferred: DeferredSupervisorStop) {
    // SAFETY: ownership was transferred to this background thread.
    unsafe { hew_supervisor_stop(deferred.0) };
}

fn stop_owned_deferred_supervisor(
    deferred: DeferredSupervisorStop,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
) {
    // SAFETY: teardown ownership was claimed by the caller before this thread
    // was spawned, so this background thread is the unique destructor.
    unsafe { stop_supervisor_owned(deferred.0, &teardown) };
    drop(teardown);
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

fn spawn_owned_deferred_supervisor_stop(
    sup: *mut HewSupervisor,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
) -> bool {
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
            stop_owned_deferred_supervisor(
                DeferredSupervisorStop(sup_addr as *mut HewSupervisor),
                teardown,
            );
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
/// authority-aware free on each. Like the supervisor-stop teardown sites, leaving
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
                // Explicit provenance plus the actor's one-shot
                // `state_drop_consumed` bit decide typed-drop authority. A
                // stopped fresh sibling did not consume crash escrow, a
                // sibling that did is already marked, and a shallow-template
                // borrower never owned it; the common path handles all three.
                unsafe { actor::hew_actor_free(d.0) };
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
        let Some(sys_dispatch) = (*current).sys_dispatch else {
            return ptr::null_mut();
        };
        if std::ptr::fn_addr_eq(sys_dispatch, supervisor_sys_dispatch as HewSysDispatchFn)
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
    static FAIL_NEXT_SUPERVISOR_ACCESS_CLOSE: Cell<bool> = const { Cell::new(false) };
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
    // SAFETY: callers guarantee a live allocation; copy its stable authority
    // without creating a whole-supervisor reference.
    let (runtime, token) = unsafe { ((*sup).runtime, (*sup).local_pid_id) };
    if runtime.is_null() {
        return false;
    }
    // SAFETY: a live supervisor's runtime authority outlives its control.
    let Some(control) = (unsafe { &*runtime })
        .local_handles
        .supervisor_control_for_raw(token, sup)
    else {
        return false;
    };
    control.claim_teardown()
}

fn release_supervisor_teardown(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    // SAFETY: only used to roll back a failed deferred-spawn attempt while the
    // supervisor is still live and owned by the caller.
    let (runtime, token) = unsafe { ((*sup).runtime, (*sup).local_pid_id) };
    if runtime.is_null() {
        return;
    }
    // SAFETY: the live allocation keeps its runtime authority installed.
    if let Some(control) = (unsafe { &*runtime })
        .local_handles
        .supervisor_control_for_raw(token, sup)
    {
        control.release_teardown();
    }
}

fn request_supervisor_shutdown(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    publish_supervisor_cancellation(sup);
    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe { (*sup).running.store(0, Ordering::Release) };
}

fn publish_supervisor_cancellation(sup: *mut HewSupervisor) {
    // Publish the wait-free mirror first. Timer callbacks that already own the
    // control mutex observe this bit and skip their send; acquiring the control
    // next wakes sleepers and waits out any raw access already in progress.
    // SAFETY: callers keep `sup` live for these projected field operations.
    unsafe {
        (*sup).cancelled.store(true, Ordering::Release);
        (*sup).restart_timers.cancel();
    }
}

#[inline]
fn supervisor_quiescence_expired(deadline: Instant) -> bool {
    scheduler::shutdown_requested() || Instant::now() >= deadline
}

fn actor_is_supervisor_quiescent(actor: *mut HewActor) -> bool {
    // SAFETY: callers keep `actor` live throughout their wait.
    let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    state != HewActorState::Running as i32 && state != HewActorState::Runnable as i32
}

fn wait_for_supervisor_self_actor_quiescent(sup: *mut HewSupervisor, deadline: Instant) -> bool {
    if sup.is_null() {
        return true;
    }

    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe {
        let self_actor = (*sup).self_actor;
        if self_actor.is_null() {
            return true;
        }

        actor::hew_actor_stop(self_actor);
        loop {
            if actor_is_supervisor_quiescent(self_actor) {
                return true;
            }
            if supervisor_quiescence_expired(deadline) {
                return false;
            }
            std::thread::yield_now();
        }
    }
}

fn wait_for_pending_restart_timers(timers: &RestartTimerControl, deadline: Instant) -> bool {
    // The caller publishes cancellation before reaching this bounded wait.
    timers.wait_for_drain(deadline)
}

fn wait_for_child_quiescent(child: *mut HewActor, deadline: Instant) -> bool {
    while !actor_is_supervisor_quiescent(child) {
        if supervisor_quiescence_expired(deadline) {
            return false;
        }
        std::thread::yield_now();
    }
    true
}

unsafe fn return_supervisor_to_runtime_cleanup(sup: *mut HewSupervisor) {
    // Top-level stops unregister before the deferred owner starts. If scheduler
    // shutdown prevents that owner from reaching a safe actor-quiescence point,
    // restore the root so canonical post-worker cleanup remains the sole
    // destructor. Keep teardown claimed so a still-running worker cannot start
    // a second stop before the root sweep. Nested supervisors stay owned by
    // their parent tree.
    // SAFETY: the deferred owner still holds the live supervisor allocation.
    if unsafe { (*sup).parent.is_null() } {
        // SAFETY: the deferred owner is returning the still-live allocation
        // without consuming it.
        // SAFETY: canonical cleanup still owns the live top-level allocation.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    }
}

/// Race for a nested supervisor's teardown authority while its parent still
/// owns the roster entry. Only the winner detaches and publishes a cleanup
/// root. The stable-token pin keeps the allocation live through the claim, so
/// a losing parent can discard its roster word without dereferencing a pointer
/// the token-based winner may immediately reclaim.
///
/// # Safety
///
/// `sup` and `token` must be the parallel values extracted from one parent
/// roster critical section.
unsafe fn claim_nested_supervisor_for_detach(
    sup: *mut HewSupervisor,
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> bool {
    if sup.is_null() {
        return false;
    }
    let pin = crate::lifetime::local_handles::pin_current_supervisor(token);
    let control = if let Some(pin) = pin.as_ref() {
        if pin.supervisor() != sup {
            return false;
        }
        pin.control()
    } else {
        let Some(control) =
            crate::lifetime::local_handles::current_supervisor_control_for_raw(token, sup)
        else {
            return false;
        };
        // Cleanup may close this nested route before the parent's already-
        // admitted teardown owner resumes. Only use the raw parent ownership
        // edge after closure has also drained every admitted dereference.
        if !control.is_closed_and_drained() {
            return false;
        }
        control
    };
    if !control.claim_teardown() {
        return false;
    }
    // SAFETY: the stable pin proves the allocation live and the successful
    // claim makes this path the only authority allowed to detach/publish it.
    // On the cleanup fallback, the exact control proves access is closed and
    // fully drained while the parent roster retains allocation ownership.
    unsafe { (*sup).parent = ptr::null_mut() };
    // SAFETY: the claimed detached allocation remains live through stop or a
    // fail-closed handoff back to canonical cleanup.
    unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    drop(pin);
    true
}

fn take_nested_supervisor_roster(
    sup: *mut HewSupervisor,
) -> Vec<(
    *mut HewSupervisor,
    crate::lifetime::local_handles::HewLocalPidId,
    Option<SupervisorChildSpec>,
)> {
    // SAFETY: callers keep `sup` live and transfer the complete nested roster
    // as one lock-protected ownership unit.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    debug_assert_eq!(
        roster.child_supervisors.len(),
        roster.child_supervisor_tokens.len()
    );
    debug_assert_eq!(
        roster.child_supervisors.len(),
        roster.child_supervisor_specs.len()
    );
    let supervisors = std::mem::take(&mut roster.child_supervisors);
    let tokens = std::mem::take(&mut roster.child_supervisor_tokens);
    let specs = std::mem::take(&mut roster.child_supervisor_specs);
    supervisors
        .into_iter()
        .zip(tokens)
        .zip(specs)
        .map(|((supervisor, token), spec)| (supervisor, token, spec))
        .collect()
}

unsafe fn stop_supervisor_owned(
    sup: *mut HewSupervisor,
    teardown: &crate::lifetime::local_handles::SupervisorTeardownLease,
) {
    // ROSTER-EXCLUSIVE: access admission is closed and all stable pins drain
    // before the Box-owned teardown traversal below begins.
    // Every raw destructor path closes handle admission and drains outstanding
    // dereferences before the allocation may reach `Box::from_raw`.
    // SAFETY: the caller transfers a live supervisor allocation to this owner.
    let Some(access) = (unsafe { close_supervisor_access(sup, SUPERVISOR_PIN_DRAIN_TIMEOUT) })
    else {
        set_last_error("supervisor handle pins did not drain before reclamation");
        // SAFETY: access closure failed closed, so this owner still holds a
        // live allocation. Restore top-level ownership before its teardown
        // lease can release the runtime-cleanup barrier.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    };
    request_supervisor_shutdown(sup);
    let quiescence_deadline = Instant::now() + SUPERVISOR_QUIESCENCE_TIMEOUT;
    if !wait_for_supervisor_self_actor_quiescent(sup, quiescence_deadline) {
        set_last_error("supervisor teardown timed out waiting for self actor quiescence");
        // SAFETY: teardown ownership is still held and no allocation was
        // consumed; canonical runtime cleanup takes ownership back.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    }

    // SAFETY: teardown ownership is held exclusively by this thread and the
    // supervisor memory remains live until `Box::from_raw` below.
    // SAFETY: teardown keeps the allocation and its Arc field live through the
    // bounded wait.
    if !wait_for_pending_restart_timers(unsafe { &(*sup).restart_timers }, quiescence_deadline) {
        set_last_error("supervisor teardown timed out waiting for restart timers");
        // SAFETY: no ownership was consumed; a timer may still hold a raw
        // borrow, so canonical cleanup must retain the allocation.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    }

    // SAFETY: teardown ownership was claimed once for this supervisor, the
    // self actor is no longer dispatching, and no other thread may consume the
    // raw pointer now.
    let mut s = unsafe { Box::from_raw(sup) }; // ALLOCATOR-PAIRING: GlobalAlloc

    // Recursively stop all child supervisors first.
    for (child_sup, child_token, _child_spec) in take_nested_supervisor_roster(&raw mut *s) {
        if !child_sup.is_null() {
            // Claim while the parent-owned roster extraction and stable token
            // still jointly identify the allocation. A losing path does not
            // touch `child_sup`; the concurrent winner owns reclamation.
            // SAFETY: pointer/token are one extracted parallel entry.
            if unsafe { claim_nested_supervisor_for_detach(child_sup, child_token) } {
                // SAFETY: the claim above is the unique child teardown authority.
                unsafe {
                    stop_supervisor_with_teardown_authority(child_sup, teardown.clone(), true);
                };
            }
        }
    }
    // Stop all children and wait for each to reach a terminal state.
    let child_count = s.roster.lock_or_recover().child_count;
    for i in 0..child_count {
        let child = take_child_slot(&raw mut *s, i);
        if !child.is_null() {
            // SAFETY: child pointer is valid.
            unsafe { actor::hew_actor_stop(child) };
            if !wait_for_child_quiescent(child, quiescence_deadline) {
                set_last_error("supervisor teardown timed out waiting for child quiescence");
                // `take_child_slot` detached this still-live child. Restore it
                // before returning ownership, otherwise canonical cleanup could
                // free the supervisor while the child remains unowned/live.
                store_child_slot(&raw mut *s, i, child);
                let sup = Box::into_raw(s);
                // SAFETY: Box ownership is converted back to the raw pointer
                // expected by canonical runtime cleanup.
                unsafe { return_supervisor_to_runtime_cleanup(sup) };
                return;
            }
            // SAFETY: child has reached a wake-proof terminal state.
            unsafe { actor::hew_actor_free(child) };
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

    let (pools, config_buf, config_drop_fn) = {
        let mut roster = s.roster.lock_or_recover();
        let pools = std::mem::take(&mut roster.pool_slots);
        let config_buf = std::mem::replace(&mut roster.config_buf, ptr::null_mut());
        let config_drop_fn = roster.config_drop_fn.take();
        roster.config_size = 0;
        (pools, config_buf, config_drop_fn)
    };

    // Free pool slots. Each pool was Box-allocated by hew_supervisor_pool_add_slot;
    // pool_specs Drop impl handles name deallocation.
    for pool in pools {
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
    if !config_buf.is_null() {
        // The config buffer is a flat snapshot of the moved-in config value, so
        // it OWNS the config struct's inner owned fields (`string`/`bytes`/…) —
        // the thunks only CLONE from them into actor state (the actors'
        // state_drop_fns release those clones). Run the config struct's
        // drop-inplace glue here to release the buffer's OWN inner owned fields
        // BEFORE the flat free; without it those fields leak (the flat free
        // reclaims only the wrapper). `None` for an all-scalar config (no inner
        // owned field to drop). No live thunk can run at this point (every child
        // actor and spec is dropped above), so this is the sole final reader.
        if let Some(drop_fn) = config_drop_fn {
            // SAFETY: drop_fn is the codegen-emitted
            // `__hew_record_drop_inplace_<ConfigTy>` for this buffer's config
            // struct; config_buf points at a fully-initialised instance of that
            // struct. Runs exactly once (config_buf is freed + nulled below).
            unsafe { drop_fn(config_buf) };
        }
        // SAFETY: config_buf was a libc::malloc'd buffer adopted (ownership
        // transferred) from codegen via hew_supervisor_add_child_spec /
        // hew_supervisor_set_child_init_fn. Inner owned fields were released by
        // config_drop_fn above; this free reclaims the config wrapper itself.
        unsafe { libc::free(config_buf) }; // ALLOCATOR-PAIRING: libc
    }
    drop(s);
    finish_supervisor_reclamation(&access);
}

/// Restart a child from its spec, returning the new actor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_count` (for
/// restarts) or equal to `child_count` (for initial spawns, where the
/// caller is responsible for pushing the result onto the `children` vec).
fn restart_snapshot_is_current(
    sup: *mut HewSupervisor,
    index: usize,
    spec_identity: u64,
    spec_revision: u64,
    template: &Arc<ChildStateTemplate>,
) -> bool {
    // SAFETY: caller keeps the supervisor allocation live for this operation.
    let roster = unsafe { &(*sup).roster }.lock_or_recover();
    roster.child_specs.get(index).is_some_and(|spec| {
        spec.identity == spec_identity
            && spec.revision == spec_revision
            && Arc::ptr_eq(&spec.state_template, template)
    })
}

fn fail_restart_snapshot(
    sup: *mut HewSupervisor,
    index: usize,
    spec_identity: u64,
    spec_revision: u64,
    template: &Arc<ChildStateTemplate>,
) {
    // SAFETY: caller keeps the allocation live; roster mutation is serialized.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    let child_specs = &mut roster.child_specs;
    let Some(spec) = child_specs.get_mut(index) else {
        return;
    };
    if spec.identity != spec_identity
        || spec.revision != spec_revision
        || !Arc::ptr_eq(&spec.state_template, template)
    {
        return;
    }
    apply_restart_backoff(spec);
    // SAFETY: the same roster guard protects child-slot mutation.
    if let Some(slot) = roster.children.get_mut(index) {
        *slot = ptr::null_mut();
    }
}

fn publish_restart_snapshot(
    sup: *mut HewSupervisor,
    index: usize,
    spec_identity: u64,
    spec_revision: u64,
    template: &Arc<ChildStateTemplate>,
    child: *mut HewActor,
) -> bool {
    // SAFETY: caller keeps the allocation live; roster mutation is serialized.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: caller keeps the allocation live through this guarded publish.
    let sup_actor_id = supervisor_actor_id(sup);
    // SAFETY: the roster guard above provides exclusive spec access.
    let child_specs = &mut roster.child_specs;
    let Some(spec) = child_specs.get_mut(index) else {
        return false;
    };
    if spec.identity != spec_identity
        || spec.revision != spec_revision
        || !Arc::ptr_eq(&spec.state_template, template)
    {
        return false;
    }

    // Complete registration while the exact-generation check and slot store
    // are still indivisible. A state-drop setter may have published into the
    // shared descriptor after this restart's initial snapshot; reload it here
    // so no late actor can replace the back-filled predecessor with `None`.
    if !child.is_null() {
        if let Some(drop_fn) = template.allocation.state_drop.load() {
            // SAFETY: `child` is the unpublished actor built from this exact
            // spec generation and the descriptor matches its state layout.
            unsafe { actor::hew_actor_set_state_drop(child, drop_fn) };
        }
        if let Some(clone_fn) = template.clone_fn {
            // SAFETY: same exact-generation argument as the drop descriptor.
            unsafe { actor::hew_actor_set_state_clone(child, clone_fn) };
        }
    }
    circuit_breaker_record_success(spec, sup_actor_id);
    // SAFETY: the same roster guard protects child-slot mutation.
    if let Some(slot) = roster.children.get_mut(index) {
        *slot = child;
    }
    true
}

unsafe fn discard_unpublished_restart(child: *mut HewActor) {
    if child.is_null() {
        return;
    }
    // SAFETY: the child was freshly spawned but never published in a roster
    // slot, so this thread is its only lifecycle owner.
    unsafe {
        actor::hew_actor_stop(child);
        let _ = actor::hew_actor_free(child);
    }
}

unsafe fn restart_child_from_spec(sup: *mut HewSupervisor, index: usize) -> *mut HewActor {
    // SAFETY: forwards the caller's liveness contract; no identity constraint
    // is needed for construction and direct index-based administrative calls.
    unsafe { restart_child_from_spec_expected(sup, index, None) }
}

#[expect(
    clippy::too_many_lines,
    reason = "one linear restart transaction keeps snapshot, spawn, registration, lifecycle, and exact-generation publish visibly ordered"
)]
unsafe fn restart_child_from_spec_expected(
    sup: *mut HewSupervisor,
    mut index: usize,
    expected_identity: Option<u64>,
) -> *mut HewActor {
    // Snapshot every spec scalar and retain the immutable template generation
    // under the roster lock. The Arc is the template lifetime lease: setters
    // may publish a replacement generation and remove_child may extract/drop
    // the spec immediately after this section, but neither can reclaim the
    // bytes this restart will read. Clone/init callbacks deliberately run after
    // the lock is released so unsafe out-of-tree callbacks may re-enter a
    // supervisor API without self-deadlocking.
    let (
        spec_identity,
        spec_revision,
        template,
        child_sys_dispatch,
        opts,
        state_drop_fn,
        lifecycle_fn,
        init_fn,
        config,
    ) = {
        // SAFETY: caller guarantees `sup` is live; the guard serializes roster access.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        let child_specs = &roster.child_specs;
        if let Some(identity) = expected_identity {
            let Some(current_index) = child_specs
                .iter()
                .position(|candidate| candidate.identity == identity)
            else {
                return ptr::null_mut();
            };
            index = current_index;
        }
        let Some(spec) = child_specs.get(index) else {
            return ptr::null_mut();
        };
        let template = Arc::clone(&spec.state_template);
        let opts = HewActorOpts {
            init_state: template.allocation.state,
            state_size: template.allocation.size,
            dispatch: spec.dispatch,
            mailbox_capacity: spec.mailbox_capacity,
            overflow: spec.overflow,
            coalesce_key_fn: spec.coalesce_key_fn,
            coalesce_fallback: spec.coalesce_fallback,
            budget: 0,
            arena_cap_bytes: spec.arena_cap_bytes,
            cycle_capable: spec.cycle_capable,
            message_drop_fn: spec.message_drop_fn,
        };
        (
            spec.identity,
            spec.revision,
            template,
            spec.sys_dispatch,
            opts,
            spec.state_template.allocation.state_drop.load(),
            spec.lifecycle_fn,
            spec.init_fn,
            spec.config,
        )
    };
    run_restart_spec_snapshot_hook_for_test();
    let state_clone_fn = template.clone_fn;
    let borrows_shallow_template = init_fn.is_none()
        && state_clone_fn.is_none()
        && opts.state_size != 0
        && !opts.init_state.is_null();

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
            fail_restart_snapshot(sup, index, spec_identity, spec_revision, &template);
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
            coalesce_key_fn: opts.coalesce_key_fn,
            coalesce_fallback: opts.coalesce_fallback,
            budget: 0,
            arena_cap_bytes: opts.arena_cap_bytes,
            cycle_capable: opts.cycle_capable,
            message_drop_fn: opts.message_drop_fn,
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
                    fail_restart_snapshot(sup, index, spec_identity, spec_revision, &template);
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
            // and register `state_drop_fn` without `state_clone_fn` receive borrowed
            // provenance: typed drop is suppressed and the aliased owned fields leak
            // fail-closed rather than being freed through multiple incarnations.
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
                (*new_child).supervisor = sup.cast::<c_void>();
                (*new_child).supervisor_child_index = index as i32;
            }
            if borrows_shallow_template {
                // The legacy spawn copied only wrapper bytes from the
                // persistent spec. Its embedded owned fields remain spec
                // aliases, so this incarnation starts without typed-drop
                // authority. Fresh init/clone branches deliberately retain
                // the actor allocator's default owned provenance.
                actor::mark_state_drop_borrowed(new_child);
            }
        }

        // Register the child's SYSTEM entry point on every incarnation. It is
        // carried in the spec, not set post-hoc by the program, so a restarted
        // child receives `#[on(exit)]` / `#[on(down)]` signals exactly as its
        // predecessor did.
        // SAFETY: new_child was just spawned and is valid.
        unsafe { actor::hew_actor_set_sys_dispatch(new_child, child_sys_dispatch) };

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

        // Claim the lifecycle linearization point only while this exact spec
        // and template generation still occupy the slot. A remove that wins
        // first prevents the callback; a remove that starts after this check
        // linearizes after lifecycle began. The actor remains unpublished, so
        // a later identity mismatch simply tears it down below.
        if !restart_snapshot_is_current(sup, index, spec_identity, spec_revision, &template) {
            // SAFETY: the actor has not been stored in the supervisor roster.
            unsafe { discard_unpublished_restart(new_child) };
            return ptr::null_mut();
        }

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

    // Publish and record circuit success only if both roster identity and the
    // immutable template generation still match after lifecycle. This closes
    // remove/swap and concurrent-setter races without holding the roster lock
    // across arbitrary lifecycle code. For initial spawns the child vec has no
    // slot yet; identity validation still succeeds and the caller pushes it.
    if !new_child.is_null()
        && !publish_restart_snapshot(
            sup,
            index,
            spec_identity,
            spec_revision,
            &template,
            new_child,
        )
    {
        // SAFETY: publication failed, so no supervisor slot owns this actor.
        unsafe { discard_unpublished_restart(new_child) };
        return ptr::null_mut();
    }
    new_child
}

/// Restart a child supervisor from its stored init fn, returning the new
/// supervisor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_supervisors`.
unsafe fn restart_child_supervisor_from_spec(
    sup: *mut HewSupervisor,
    index: usize,
) -> *mut HewSupervisor {
    let (init_fn, old_child, old_token) = {
        // SAFETY: caller keeps `sup` live and the guard protects all three
        // parallel nested-supervisor vectors.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: `sup` remains live for this scoped, lock-protected read.
        let s = &*guard;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
        let Some(spec) = s.child_supervisor_specs.get(index).and_then(Option::as_ref) else {
            return ptr::null_mut();
        };
        (
            spec.init_fn,
            s.child_supervisors[index],
            s.child_supervisor_tokens[index],
        )
    };

    // SAFETY: `init_fn` was registered alongside this child supervisor.
    let new_child = unsafe { init_fn() };
    if new_child.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `new_child` and `sup` are valid pointers.
    unsafe {
        (*new_child).parent = sup;
        (*new_child).index_in_parent = index;
        crate::shutdown::hew_shutdown_unregister_supervisor(new_child);
    }
    // SAFETY: `new_child` is the live allocation returned by `init_fn`.
    let new_token = unsafe { (*new_child).local_pid_id };
    {
        // Publish pointer+token as one exact replacement. If another teardown
        // already removed/replaced this entry while init ran, return the new
        // allocation to top-level cleanup instead of corrupting its roster.
        // SAFETY: caller keeps `sup` live through this publication attempt.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        if s.child_supervisors.get(index).copied() != Some(old_child)
            || s.child_supervisor_tokens.get(index).copied() != Some(old_token)
        {
            // SAFETY: publication failed, so restore top-level ownership of
            // the still-live, otherwise-unpublished allocation.
            unsafe {
                (*new_child).parent = ptr::null_mut();
                crate::shutdown::hew_shutdown_register_supervisor(new_child);
            }
            return ptr::null_mut();
        }
        s.child_supervisors[index] = new_child;
        s.child_supervisor_tokens[index] = new_token;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
    }

    if !old_child.is_null() && old_child != new_child {
        debug_assert_ne!(old_token, new_token);
        defer_stop_child_supervisor(old_child);
    }

    new_child
}

/// Restart children after checking the supervisor restart budget.
///
/// # Safety
///
/// `sup` must be valid.
unsafe fn restart_with_budget_and_strategy(sup: *mut HewSupervisor, failed_identity: u64) {
    // SAFETY: caller keeps the allocation live; these scalar policy fields are
    // immutable after construction.
    let (strategy, max_restarts, window_secs) =
        unsafe { ((*sup).strategy, (*sup).max_restarts, (*sup).window_secs) };
    let sup_actor_id = supervisor_actor_id(sup);
    let (strategy, identities, failed_index, recent, max_restarts, sup_actor_id) = {
        // SAFETY: caller keeps `sup` live; budget bookkeeping and roster
        // snapshot are serialized with dynamic roster mutation.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;
        let Some(failed_index) = s
            .child_specs
            .iter()
            .position(|spec| spec.identity == failed_identity)
        else {
            return;
        };
        let recent = restart_within_window(window_secs, s);
        if recent < max_restarts {
            record_restart(s);
        }
        (
            strategy,
            s.child_specs
                .iter()
                .map(|spec| spec.identity)
                .collect::<Vec<_>>(),
            failed_index,
            recent,
            max_restarts,
            sup_actor_id,
        )
    };

    if recent >= max_restarts {
        // Observability (AFTER the max-restart-intensity decision, BEFORE the
        // escalate): record that the budget was exhausted, carrying the
        // within-window restart count.
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_MAX_RESTARTS,
            recent,
        );
        // SAFETY: caller keeps `sup` live; no roster reference crosses this
        // cancellation/escalation operation.
        stop_and_maybe_escalate(sup);
        return;
    }

    crate::observe::record_actor_restart();
    // Observability (AFTER the restart decision is taken): record the restart,
    // carrying the restart strategy in the discriminator. Read-only side
    // effect; never gates control flow.
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_RESTART,
        strategy,
    );

    match strategy {
        STRATEGY_ONE_FOR_ONE => {
            // SAFETY: the stable identity prevents a concurrent swap-remove
            // from retargeting this restart to a sibling.
            unsafe {
                restart_child_from_spec_expected(sup, failed_index, Some(identities[failed_index]));
            };
        }
        STRATEGY_ONE_FOR_ALL => {
            // Stop all other children, then restart all.
            // Children are freed on a background thread to avoid deadlocking
            // when the scheduler has a single worker (hew_actor_free spin-waits
            // and would block the only worker running this dispatch).
            let mut deferred: Vec<DeferredFree> = Vec::new();
            for (i, identity) in identities.iter().copied().enumerate() {
                if i != failed_index {
                    let child = take_child_slot_by_identity(sup, identity);
                    if child.is_null() {
                        continue;
                    }
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(child) };
                    deferred.push(DeferredFree(child));
                }
            }
            spawn_deferred_restart_free(deferred);
            for (i, identity) in identities.iter().copied().enumerate() {
                // SAFETY: identity lookup and exact-generation publication
                // refuse if concurrent removal retired this spec.
                unsafe { restart_child_from_spec_expected(sup, i, Some(identity)) };
            }
        }
        STRATEGY_REST_FOR_ONE => {
            // Stop children after the failed one, then restart them.
            // Deferred free as in ONE_FOR_ALL to avoid single-worker deadlock.
            let mut deferred: Vec<DeferredFree> = Vec::new();
            for identity in identities.iter().copied().skip(failed_index + 1) {
                let child = take_child_slot_by_identity(sup, identity);
                if !child.is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(child) };
                    deferred.push(DeferredFree(child));
                }
            }
            spawn_deferred_restart_free(deferred);
            for (i, identity) in identities.iter().copied().enumerate().skip(failed_index) {
                // SAFETY: identity lookup and exact-generation publication
                // refuse if concurrent removal retired this spec.
                unsafe { restart_child_from_spec_expected(sup, i, Some(identity)) };
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
            unsafe {
                restart_child_from_spec_expected(sup, failed_index, Some(identities[failed_index]));
            };
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

    // SAFETY: caller keeps `sup` live and notification state is independent of
    // the roster references, all of which were dropped above.
    notify_restart(sup);
}

/// Restart an exhausted child supervisor subtree after checking the parent's
/// restart budget. Child-supervisor recovery is only available when the child
/// was registered with an init fn.
///
/// # Safety
///
/// `sup` must be valid and `failed_index` must be within `child_supervisors`.
unsafe fn restart_child_supervisor_with_budget(sup: *mut HewSupervisor, failed_index: usize) {
    // SAFETY: these policy scalars are immutable after construction.
    let (max_restarts, window_secs, strategy) =
        unsafe { ((*sup).max_restarts, (*sup).window_secs, (*sup).strategy) };
    let sup_actor_id = supervisor_actor_id(sup);
    let (restartable, recent, max_restarts, sup_actor_id, strategy) = {
        // SAFETY: caller keeps `sup` live; nested-roster validation and budget
        // bookkeeping are one synchronized snapshot.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
        let restartable = s
            .child_supervisor_specs
            .get(failed_index)
            .and_then(Option::as_ref)
            .is_some();
        let recent = if restartable {
            restart_within_window(window_secs, s)
        } else {
            0
        };
        if restartable && recent < max_restarts {
            record_restart(s);
        }
        (restartable, recent, max_restarts, sup_actor_id, strategy)
    };

    if !restartable {
        // SAFETY: no roster reference crosses cancellation/escalation.
        stop_and_maybe_escalate(sup);
        return;
    }

    if recent >= max_restarts {
        // Observability (AFTER the max-restart-intensity decision): record
        // budget exhaustion on the child-supervisor recovery path too.
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_MAX_RESTARTS,
            recent,
        );
        // SAFETY: no roster reference crosses cancellation/escalation.
        stop_and_maybe_escalate(sup);
        return;
    }

    crate::observe::record_actor_restart();
    // Observability (AFTER the restart decision): record the child-supervisor
    // subtree restart, carrying the strategy discriminator.
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_RESTART,
        strategy,
    );

    // SAFETY: `failed_index` is validated above and `sup` is the live parent
    // supervisor whose child-supervisor slot we are replacing.
    if unsafe { restart_child_supervisor_from_spec(sup, failed_index) }.is_null() {
        // SAFETY: restart returned without leaving a nested-roster borrow.
        stop_and_maybe_escalate(sup);
        return;
    }

    // SAFETY: caller keeps `sup` live for notification.
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
    sup: *mut HewSupervisor,
    failed_identity: u64,
    exit_state: c_int,
    crash_code: c_int,
    ctx: *mut crate::execution_context::HewExecutionContext,
) {
    let crashed = exit_state == HewActorState::Crashed as c_int;
    let (spec_identity, template, on_crash, sup_actor_id) = {
        // SAFETY: caller keeps `sup` live; crash accounting and callback
        // snapshot are serialized with setters and dynamic removal.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        let sup_actor_id = supervisor_actor_id(sup);
        let Some(spec) = s
            .child_specs
            .iter_mut()
            .find(|candidate| candidate.identity == failed_identity)
        else {
            return;
        };
        if crashed {
            circuit_breaker_record_crash(spec, crash_code, sup_actor_id);
        }
        (
            spec.identity,
            Arc::clone(&spec.state_template),
            spec.on_crash,
            sup_actor_id,
        )
    };

    // The arbitrary on-crash callback runs without `roster`. The Arc
    // lease keeps the exact template generation alive across a concurrent
    // clone setter or remove_child.
    let crash_action_tag = if crashed {
        // SAFETY: `ctx` is the live supervisor dispatch context and `template`
        // leases the state allocation for the complete synchronous callback.
        unsafe { invoke_on_crash_handler(on_crash, template.allocation.state, crash_code, ctx) }
    } else {
        None
    };

    match crash_action_tag {
        Some(CRASH_ACTION_KILL) => return,
        Some(CRASH_ACTION_ESCALATE) => {
            // SAFETY: the caller keeps `sup` live; only non-roster parent state
            // is inspected after the callback lease has been released.
            if unsafe { !(*sup).parent.is_null() } {
                // SAFETY: no roster reference crosses escalation.
                escalate_to_parent(sup);
            }
            return;
        }
        _ => {}
    }

    let delay_ms = {
        // SAFETY: caller keeps `sup` live; find the stable identity again so a
        // concurrent swap-remove cannot apply policy to a sibling.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        let Some(spec) = s
            .child_specs
            .iter_mut()
            .find(|candidate| candidate.identity == spec_identity)
        else {
            return;
        };

        if crashed && spec.restart_delay_ms > 0 {
            apply_restart_backoff(spec);
        }
        if spec.restart_policy == RESTART_TEMPORARY
            || (spec.restart_policy == RESTART_TRANSIENT
                && exit_state == HewActorState::Stopped as c_int)
            || !circuit_breaker_should_restart(spec, sup_actor_id)
        {
            return;
        }
        if restart_delay_allows_restart(spec) {
            if crashed && spec.restart_delay_ms == 0 {
                spec.restart_delay_ms = INITIAL_RESTART_DELAY_MS;
            }
            None
        } else {
            let remaining = spec
                .next_restart_time_ns
                .saturating_sub(monotonic_time_ns());
            Some((remaining / 1_000_000).max(1))
        }
    };

    if let Some(delay_ms) = delay_ms {
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_BACKOFF,
            i32::try_from(delay_ms).unwrap_or(i32::MAX),
        );
        // The caller keeps the supervisor live; scheduling retains its own
        // timer lease and carries the stable child identity, never an index.
        schedule_delayed_restart(
            sup,
            spec_identity,
            std::time::Duration::from_millis(delay_ms),
        );
        return;
    }

    // SAFETY: budget/strategy resolves the stable identity under the roster
    // lock and refuses if dynamic removal retired it.
    unsafe { restart_with_budget_and_strategy(sup, spec_identity) };
}

/// The supervisor's [`HewSysDispatchFn`] — its SYSTEM entry point.
///
/// Registered as `HewActor.sys_dispatch`, never as `dispatch`, so it is
/// reachable ONLY from nodes dequeued with `Origin::Sys`. A `hew_actor_send`
/// to the supervisor's actor handle lands on the user queue and can never
/// arrive here, which is what makes a forged supervision event — the
/// `take_child_slot` + `hew_actor_free` of a LIVE child — unrepresentable
/// rather than merely gated.
///
/// The dispatch logic lives in `supervisor_sys_dispatch_impl` (which keeps the
/// early-`return` control flow).
unsafe extern "C-unwind" fn supervisor_sys_dispatch(
    ctx: *mut crate::execution_context::HewExecutionContext,
    state: *mut c_void,
    sys_msg: i32,
    data: *mut c_void,
    data_size: usize,
) {
    // SAFETY: forwards the caller's invariants unchanged to the impl.
    unsafe { supervisor_sys_dispatch_impl(ctx, state, sys_msg, data, data_size) };
}

unsafe fn supervisor_sys_dispatch_impl(
    ctx: *mut crate::execution_context::HewExecutionContext,
    state: *mut c_void,
    sys_msg: i32,
    data: *mut c_void,
    data_size: usize,
) {
    if state.is_null() {
        return;
    }
    let sup = state.cast::<HewSupervisor>();

    // SAFETY: state points to the live supervisor backing this dispatch.
    if unsafe { (*sup).running.load(Ordering::Acquire) } == 0 {
        return;
    }

    // Fail-closed decode. The scheduler already validated this value against
    // the closed set; re-decoding here keeps the callee independent of that
    // guarantee rather than trusting a raw integer.
    let Some(kind) = HewSysMsg::from_raw(sys_msg) else {
        eprintln!("[supervisor] refusing system signal with unknown kind {sys_msg}");
        return;
    };

    match kind {
        HewSysMsg::ChildStopped | HewSysMsg::ChildCrashed => {
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

            let idx = event.child_index as usize;
            let Some((child, spec_identity)) = take_child_slot_for_event(sup, idx, event.child_id)
            else {
                return;
            };

            // Free the old child.
            if !child.is_null() {
                // Explicit provenance plus the retiring incarnation's atomic
                // `state_drop_consumed` bit are the typed-drop authority.
                // Crash escrow sets consumed only after actually consuming
                // state; pre-dispatch crashes, normal stops, init-thunk state
                // and clone-produced state retain their final-drop authority,
                // while shallow-template borrowers never acquire it.
                // SAFETY: child is quiescent and no longer referenced by its
                // supervisor slot.
                unsafe { actor::hew_actor_free(child) };
            }

            // SAFETY: sup is valid; ctx is the supervisor's own dispatch
            // context, threaded through so a registered on_crash handler
            // receives the supervisor's ctx (preserves task-scope
            // cancellation propagation per f4df6354).
            unsafe { apply_restart(sup, spec_identity, event.exit_state, event.crash_code, ctx) };
        }
        HewSysMsg::ChildSupervisorEscalated => {
            if data.is_null() || data_size < std::mem::size_of::<ChildSupervisorEscalation>() {
                return;
            }
            // SAFETY: data is valid for at least sizeof(ChildSupervisorEscalation).
            let event = unsafe { &*data.cast::<ChildSupervisorEscalation>() };
            crate::tracing::ensure_supervisor_trace_root();
            let idx = event.supervisor_index as usize;
            // SAFETY: parent supervisor is valid for the lifetime of this dispatch.
            unsafe { restart_child_supervisor_with_budget(sup, idx) };
        }
        HewSysMsg::SupervisorStop => {
            // SAFETY: dispatch keeps the supervisor live.
            unsafe {
                publish_supervisor_cancellation(sup);
                (*sup).running.store(0, Ordering::Release);
            }
            let mut retained = Vec::new();
            for (child_sup, child_token, child_spec) in take_nested_supervisor_roster(sup) {
                if !child_sup.is_null() {
                    // Admission failure means no teardown winner exists yet;
                    // retain the parent ownership edge for the later canonical
                    // parent stop. Once admission succeeds, the stable-token
                    // claim decides ownership: only its winner detaches and
                    // publishes, while a loser never touches the child pointer.
                    let Some(teardown) =
                        crate::lifetime::local_handles::begin_current_supervisor_teardown()
                    else {
                        retained.push((child_sup, child_token, child_spec));
                        continue;
                    };
                    // SAFETY: pointer/token are one extracted parent entry.
                    if unsafe { claim_nested_supervisor_for_detach(child_sup, child_token) } {
                        // SAFETY: the successful claim is passed explicitly;
                        // this stop path must not race the claim a second time.
                        unsafe {
                            stop_supervisor_with_teardown_authority(child_sup, teardown, true);
                        };
                    }
                }
            }
            if !retained.is_empty() {
                // SAFETY: dispatch keeps `sup` live through this re-publication.
                let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
                // SAFETY: the guard serializes this scoped mutable roster access.
                let s = &mut *guard;
                for (child_sup, child_token, child_spec) in retained {
                    s.child_supervisors.push(child_sup);
                    s.child_supervisor_tokens.push(child_token);
                    s.child_supervisor_specs.push(child_spec);
                }
            }
            // Stop each retained actor while the roster lock prevents dynamic
            // removal from reclaiming the pointer being stopped.
            // SAFETY: dispatch keeps `sup` live for the critical section.
            let guard = unsafe { &(*sup).roster }.lock_or_recover();
            // SAFETY: the guard protects this scoped roster traversal.
            let s = &*guard;
            for child in &s.children {
                if !child.is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(*child) };
                }
            }
        }
        HewSysMsg::DelayedRestart => {
            if data.is_null() || data_size < std::mem::size_of::<DelayedRestartEvent>() {
                return;
            }
            // SAFETY: data is valid for at least sizeof(DelayedRestartEvent).
            let event = unsafe { &*data.cast::<DelayedRestartEvent>() };
            // S3: establish a sampled root for the delayed-restart span too
            // (this dispatch was woken by a timer-thread sys-send, which may
            // carry a zero trace context).
            crate::tracing::ensure_supervisor_trace_root();
            // SAFETY: the stable identity is resolved under the roster lock;
            // retired dynamic children are ignored.
            unsafe { restart_with_budget_and_strategy(sup, event.child_identity) };
        }
        // A supervisor's own actor is never linked or monitored by the runtime.
        HewSysMsg::Exit | HewSysMsg::Down => {}
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
    let runtime = crate::runtime::rt_current();
    let sup = Box::new(HewSupervisor {
        runtime: runtime as *const crate::runtime::RuntimeInner,
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        strategy,
        max_restarts,
        window_secs,
        parent: ptr::null_mut(),
        index_in_parent: 0,
        running: AtomicI32::new(0),
        cancelled: AtomicBool::new(false),
        restart_timers: Arc::new(RestartTimerControl::new()),
        self_actor: ptr::null_mut(),
        roster: Mutex::new(SupervisorRoster {
            children: Vec::with_capacity(SUP_INITIAL_CAPACITY),
            child_specs: Vec::with_capacity(SUP_INITIAL_CAPACITY),
            child_count: 0,
            next_child_spec_identity: 1,
            child_supervisors: Vec::new(),
            child_supervisor_tokens: Vec::new(),
            child_supervisor_specs: Vec::new(),
            restart_times: [0u64; MAX_RESTARTS_TRACK],
            restart_count: 0,
            restart_head: 0,
            restart_notify: Some(Arc::new((Mutex::new(0), Condvar::new()))),
            pool_slots: Vec::new(),
            pool_specs: Vec::new(),
            config_buf: ptr::null_mut(),
            config_size: 0,
            config_drop_fn: None,
        }),
        restart_await_waiters: Mutex::new(Vec::new()),
    });
    let raw = Box::into_raw(sup); // ALLOCATOR-PAIRING: GlobalAlloc
    let publication = match crate::lifetime::local_handles::begin_supervisor_publication_in(
        &runtime.local_handles,
    ) {
        Ok(publication) => publication,
        Err(error) => {
            set_last_error(format!(
                "hew_supervisor_new: handle admission failed: {error:?}"
            ));
            // SAFETY: publication failed before any control or route was stored.
            drop(unsafe { Box::from_raw(raw) });
            return ptr::null_mut();
        }
    };
    match publication.register_supervisor(runtime.runtime_id(), raw) {
        Ok(token) => {
            // SAFETY: `raw` remains exclusively construction-owned here.
            unsafe { (*raw).local_pid_id = token };
            raw
        }
        Err(error) => {
            set_last_error(format!(
                "hew_supervisor_new: handle registration failed: {error:?}"
            ));
            // SAFETY: registration rolled back without publishing this pointer.
            drop(unsafe { Box::from_raw(raw) });
            ptr::null_mut()
        }
    }
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
    // SAFETY: caller guarantees `spec` is a valid, aligned, initialized `HewChildSpec` pointer.
    let sp = unsafe { &*spec };

    // The v0.6 init-closure restart model: when the spec carries an `init_fn`,
    // the thunk is THE state source on the initial spawn and every restart.
    // Skip the byte-copy state template entirely — `restart_child_from_spec`
    // ignores `init_state` on the thunk path, and capturing a template here
    // would re-introduce the owned-field aliasing hazard the thunk model fixes.
    let has_init_fn = sp.init_fn.is_some();

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
        // Portable strdup (libc::strdup unavailable on Windows-MSVC, #2505).
        unsafe { crate::cabi::cstr_strdup(sp.name) }
    };

    let mut internal_spec = InternalChildSpec {
        identity: 0,
        revision: 1,
        name: name_copy,
        state_template: Arc::new(ChildStateTemplate {
            allocation: Arc::new(ChildStateTemplateAllocation {
                state: state_copy,
                // On the thunk path the state size is produced by the thunk
                // result, not the spec; keep it 0 so no template path can read
                // a stale size.
                size: if has_init_fn { 0 } else { sp.init_state_size },
                owns_typed_fields: false,
                state_drop: Arc::new(ChildStateDropDescriptor::new()),
            }),
            clone_fn: None,
        }),
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        coalesce_key_fn: sp.coalesce_key_fn,
        coalesce_fallback: sp.coalesce_fallback,
        message_drop_fn: sp.message_drop_fn,
        sys_dispatch: sp.sys_dispatch,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        cycle_capable: sp.cycle_capable,
        on_crash: sp.on_crash,
        // Carried IN the spec literal (like on_crash) so the initial supervised
        // spawn — which happens inside this call via restart_child_from_spec —
        // fires the lifecycle wrapper. A post-hoc setter would run too late to
        // cover the initial fire (see hew_supervisor_set_child_lifecycle).
        lifecycle_fn: sp.lifecycle_fn,
        // Carried IN the spec literal (like lifecycle_fn) so the INITIAL spawn
        // below uses the thunk — the load-bearing first-spawn carrier. The
        // post-hoc setter is back-fill/symmetry only.
        init_fn: sp.init_fn,
        // Installed from the exact adopted supervisor config in the reservation
        // transaction below.
        config: ptr::null_mut(),
    };

    // Reserve a complete null child/spec slot in one roster critical section.
    // No callback runs under the lock; restart below validates the reserved
    // identity and publishes into the placeholder.
    let i = {
        // SAFETY: caller keeps `sup` live; config adoption and complete roster
        // reservation are one lock-protected transaction.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;
        if has_init_fn && !sp.config.is_null() {
            if s.config_buf.is_null() {
                s.config_buf = sp.config;
                s.config_size = sp.config_size;
            } else if s.config_buf != sp.config {
                set_last_error("hew_supervisor_add_child_spec: conflicting config buffer");
                // SAFETY: `sp.config` is a libc-allocated orphan distinct from
                // the already-adopted buffer (ALLOCATOR-PAIRING: libc).
                unsafe { libc::free(sp.config) };
                return -1;
            }
            internal_spec.config = s.config_buf;
        }
        let i = s.child_count;
        internal_spec.identity = s.next_child_spec_identity;
        let Some(next_identity) = s.next_child_spec_identity.checked_add(1) else {
            set_last_error("hew_supervisor_add_child_spec: child-spec identity exhausted");
            return -1;
        };
        s.next_child_spec_identity = next_identity;
        s.child_specs.push(internal_spec);
        s.children.push(ptr::null_mut());
        s.child_count += 1;
        debug_assert_eq!(s.children.len(), s.child_specs.len());
        i
    };

    // SAFETY: the exact identity-backed placeholder was reserved above.
    unsafe { restart_child_from_spec(sup, i) };
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
    // SAFETY: caller guarantees sup is valid and start is a construction edge.
    unsafe { (*sup).running.store(1, Ordering::Release) };

    // Create the supervisor's own actor. We pass a dummy state (the sup
    // pointer itself) and override it after spawn.
    // SAFETY: spawning with the supervisor dispatch function.
    // The supervisor actor has NO application handlers: its entire protocol is
    // lifecycle signals, so it registers only the SYSTEM entry point. With
    // `dispatch` left `None`, any message a program sends to the supervisor's
    // actor handle is freed unread instead of reaching supervision logic.
    let self_actor = unsafe {
        actor::hew_actor_spawn(
            sup.cast::<HewSupervisor>().cast::<c_void>(),
            std::mem::size_of::<HewSupervisor>(),
            None,
        )
    };
    if self_actor.is_null() {
        // SAFETY: caller keeps `sup` live through start failure rollback.
        unsafe { (*sup).running.store(0, Ordering::Release) };
        return -1;
    }
    // SAFETY: `self_actor` is the freshly spawned supervisor actor; no other
    // thread can observe it before this call returns because the supervisor's
    // `self_actor` slot is still null.
    unsafe {
        actor::hew_actor_set_sys_dispatch(self_actor, Some(supervisor_sys_dispatch));
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

    // SAFETY: construction has exclusive authority over the self-actor slot.
    unsafe { (*sup).self_actor = self_actor };

    // Auto-register top-level supervisors for graceful shutdown so they
    // are cleaned up even if the generated code omits an explicit stop.
    // SAFETY: construction has exclusive authority over the parent edge.
    if unsafe { (*sup).parent.is_null() } {
        // SAFETY: sup is valid and will remain valid until shutdown.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    }

    0
}

/// Notify the supervisor that a supervised child ACTOR has stopped or crashed.
///
/// `child_index` is `u32`: the escalation case that formerly rode this same
/// symbol with `child_index = -1` is now
/// [`hew_supervisor_notify_child_supervisor_escalation`], so no caller —
/// external, JIT, or generated — can express the retagging value.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - The supervisor must have been started with [`hew_supervisor_start`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_notify_child_actor_event(
    sup: *mut HewSupervisor,
    child_index: u32,
    child_id: u64,
    exit_state: c_int,
    crash_code: c_int,
) {
    cabi_guard!(sup.is_null());
    // SAFETY: caller keeps `sup` live through this notification.
    let self_actor = unsafe { (*sup).self_actor };
    if self_actor.is_null() {
        return;
    }

    let event = ChildEvent {
        child_index,
        child_id,
        exit_state,
        crash_code,
    };

    let kind = if exit_state == HewActorState::Crashed as c_int {
        HewSysMsg::ChildCrashed
    } else {
        HewSysMsg::ChildStopped
    };

    // SAFETY: self_actor is valid, mailbox is valid.
    unsafe {
        let _ = actor::send_system_message(
            self_actor,
            kind,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildEvent>(),
        );
    }
}

/// Notify the supervisor that a child SUPERVISOR exhausted its restart budget
/// and escalated.
///
/// The sibling of [`hew_supervisor_notify_child_actor_event`], split out so the
/// two events cannot be confused: `supervisor_index` indexes
/// `child_supervisors`, a different collection from the one an actor event
/// indexes. The old single symbol distinguished them by `child_index == -1`,
/// which silently retagged the meaning of the neighbouring id field.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - The supervisor must have been started with [`hew_supervisor_start`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_notify_child_supervisor_escalation(
    sup: *mut HewSupervisor,
    supervisor_index: u32,
    exit_state: c_int,
    crash_code: c_int,
) {
    cabi_guard!(sup.is_null());
    // SAFETY: caller keeps `sup` live through this notification.
    let self_actor = unsafe { (*sup).self_actor };
    if self_actor.is_null() {
        return;
    }

    let event = ChildSupervisorEscalation {
        supervisor_index,
        exit_state,
        crash_code,
    };

    // SAFETY: self_actor is valid, mailbox is valid.
    unsafe {
        let _ = actor::send_system_message(
            self_actor,
            HewSysMsg::ChildSupervisorEscalated,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildSupervisorEscalation>(),
        );
    }
}

/// Stop the supervisor and all its children.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`]. The
/// pointer must not be used after this call.
unsafe fn stop_claimed_supervisor(
    sup: *mut HewSupervisor,
    root_unregistered: bool,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
) -> bool {
    if current_thread_owns_supervisor_tree(sup) {
        if !spawn_owned_deferred_supervisor_stop(sup, teardown.clone()) {
            if root_unregistered {
                // SAFETY: the failed handoff leaves the top-level allocation
                // live and teardown ownership is released below.
                unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
            }
            release_supervisor_teardown(sup);
            set_last_error("hew_supervisor_stop: failed to spawn deferred stop thread");
            drop(teardown);
            return false;
        }
        if !root_unregistered {
            // Unregister once the deferred owner is guaranteed to finish.
            // SAFETY: `sup` is live and was registered when started.
            unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
        }
        drop(teardown);
        return true;
    }

    if !root_unregistered {
        // Unregister before consuming the raw pointer so shutdown cannot race
        // this exact-once teardown owner.
        // SAFETY: `sup` is live and was registered when started.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
    }
    run_supervisor_teardown_hook_for_test();
    // SAFETY: teardown ownership is uniquely claimed by the caller.
    unsafe { stop_supervisor_owned(sup, &teardown) };
    drop(teardown);
    true
}

unsafe fn stop_supervisor_with_teardown_authority(
    sup: *mut HewSupervisor,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
    preclaimed: bool,
) {
    request_supervisor_shutdown(sup);
    // SAFETY: the caller's teardown lease keeps runtime cleanup from reclaiming
    // the live allocation or its runtime authority during access closure.
    if unsafe { close_supervisor_access(sup, SUPERVISOR_PIN_DRAIN_TIMEOUT) }.is_none() {
        // A recursively detached child no longer has a parent-owned root. Hand
        // every still-live top-level allocation back to canonical cleanup before
        // this lease can release the cleanup barrier.
        // SAFETY: access closure failed closed, so `sup` remains allocated.
        if unsafe { (*sup).parent.is_null() } {
            // SAFETY: the still-live allocation has no parent root and remains
            // valid until canonical cleanup consumes the restored root.
            unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        }
        set_last_error("hew_supervisor_stop: handle pins did not drain");
        return;
    }
    if !preclaimed && !claim_supervisor_teardown(sup) {
        return;
    }
    // SAFETY: teardown ownership was claimed above and remains unique.
    unsafe { stop_claimed_supervisor(sup, false, teardown) };
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

    // SAFETY: the public raw-pointer contract guarantees a live supervisor.
    let Some(teardown) = (unsafe { begin_supervisor_teardown(sup) }) else {
        return;
    };

    // SAFETY: the public raw-pointer contract guarantees a live supervisor;
    // the acquired lease remains visible to cleanup through final reclamation.
    unsafe { stop_supervisor_with_teardown_authority(sup, teardown, false) };
}

/// Return the stable direct identity for one supervisor allocation.
///
/// # Safety
///
/// `sup` must be a live pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_direct_id(
    sup: *mut HewSupervisor,
) -> crate::lifetime::local_handles::HewLocalPidId {
    if sup.is_null() {
        return crate::lifetime::local_handles::HewLocalPidId::INVALID;
    }
    // SAFETY: guaranteed by the caller.
    unsafe { (*sup).local_pid_id }
}

/// Query a supervisor through its stable local identity.
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_is_running(
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> c_int {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return 0;
    };
    run_supervisor_access_hook_for_test();
    let sup = pin.supervisor();
    // SAFETY: the pin prevents reclamation through these atomic loads.
    c_int::from(unsafe {
        (*sup).running.load(Ordering::Acquire) != 0 && !(*sup).cancelled.load(Ordering::Acquire)
    })
}

/// Stop a supervisor through its stable local identity.
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_stop(
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> c_int {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return 1;
    };
    let Some(teardown) = crate::lifetime::local_handles::begin_current_supervisor_teardown() else {
        drop(pin);
        return 1;
    };
    let sup = pin.supervisor();
    // Publish shutdown and close the direct route while the allocation is
    // still protected by this operation's pin. Dropping the pin then permits
    // the raw destructor path to drain without self-deadlock.
    request_supervisor_shutdown(sup);
    let control = pin.control();
    let won_close = crate::lifetime::local_handles::close_current_supervisor(&control);
    if won_close {
        run_supervisor_close_hook_for_test();
    }
    if !claim_supervisor_teardown(sup) {
        drop(pin);
        return 1;
    }
    // SAFETY: the operation pin keeps the allocation live for this field read.
    let top_level = unsafe { (*sup).parent.is_null() };
    // Remove the root while the pin still protects this allocation. Runtime
    // cleanup can no longer select it for a competing canonical destructor.
    if top_level {
        // SAFETY: the pin proves that `sup` remains live through unregister.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
    }
    drop(pin);
    if !control.wait_for_pins(SUPERVISOR_PIN_DRAIN_TIMEOUT) {
        // Restore canonical cleanup ownership for a top-level allocation whose
        // operation could not safely reach reclamation.
        if top_level {
            // SAFETY: timeout is fail-closed, so `sup` remains allocated.
            unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        }
        release_supervisor_teardown(sup);
        set_last_error("supervisor token stop: handle pins did not drain");
        return 2;
    }
    // Reclamation removes the registry's final control reference. Do not keep
    // the token-stop owner's local reference alive across the teardown lease:
    // runtime cleanup may proceed as soon as that lease is relinquished and
    // must observe the control registry fully drained.
    drop(control);
    // SAFETY: this token operation claimed teardown while pinned and already
    // removed the supervisor from the runtime cleanup root set.
    c_int::from(!unsafe { stop_claimed_supervisor(sup, top_level, teardown) }) * 2
}

#[cfg(all(test, not(target_arch = "wasm32")))]
#[allow(
    unused_unsafe,
    reason = "test-owned raw supervisors often group several unsafe operations"
)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    /// Test-only shorthand that still obtains the typed roster guard. Tests
    /// own each supervisor for the complete guard lifetime.
    macro_rules! locked_roster {
        ($sup:expr) => {{
            // SAFETY: each use is scoped to a test-owned live supervisor.
            unsafe { &(*$sup).roster }.lock_or_recover()
        }};
    }

    unsafe fn teardown_is_claimed(sup: *mut HewSupervisor) -> bool {
        // SAFETY: callers pass a test-owned live supervisor.
        let token = unsafe { (*sup).local_pid_id };
        crate::lifetime::local_handles::current_supervisor_control_for_raw(token, sup)
            .is_some_and(|control| control.teardown_is_claimed())
    }

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

    static BORROWED_NORMAL_DISPATCH_COUNT: AtomicUsize = AtomicUsize::new(0);
    static BORROWED_NORMAL_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C-unwind" fn counted_borrowed_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        BORROWED_NORMAL_DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
        ptr::null_mut()
    }

    unsafe extern "C" fn count_borrowed_normal_drop(_state: *mut c_void) {
        BORROWED_NORMAL_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
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
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
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

            let child = locked_roster!(sup).children[0];
            let self_actor = (*sup).self_actor;
            (sup, child, self_actor)
        }
    }

    #[test]
    fn supervisor_teardown_quiescence_waits_expire_fail_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh tree and restores terminal states
        // before consuming it through the normal public stop path.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();

            // A worker can die with any one of these ownership edges still
            // active.  Each wait must return at its shared deadline rather than
            // spin forever; the caller then returns the tree to cleanup without
            // freeing a live actor or timer-borrowed supervisor.
            (*self_actor)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);
            assert!(!wait_for_supervisor_self_actor_quiescent(
                sup,
                Instant::now()
            ));

            let timer = (*sup)
                .restart_timers
                .begin()
                .expect("fresh supervisor accepts a timer lease");
            assert!(!wait_for_pending_restart_timers(
                &(*sup).restart_timers,
                Instant::now()
            ));

            (*child)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);
            assert!(!wait_for_child_quiescent(child, Instant::now()));

            drop(timer);
            (*self_actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            (*child)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn runtime_cleanup_cancels_long_restart_timer_without_retry() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the production timer lease owns the raw supervisor borrow;
        // canonical cleanup cancels and drains it before reclaiming the tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let timers = Arc::clone(&(*sup).restart_timers);
            schedule_delayed_restart(sup, 0, Duration::from_secs(30));
            assert_eq!(
                timers.pending_for_test(),
                1,
                "long-backoff timer must publish its raw borrow before spawn"
            );

            let started = Instant::now();
            crate::scheduler::hew_runtime_cleanup();

            assert!(
                started.elapsed() < Duration::from_secs(2),
                "cleanup must wake a 30-second restart backoff instead of leasing its full delay"
            );
            assert_eq!(timers.pending_for_test(), 0);
            assert!(
                crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "one cleanup call must reclaim the runtime after cancellable timers drain"
            );
        }
    }

    #[test]
    fn public_stop_cancels_long_restart_timer_promptly() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: public stop owns and reclaims the fresh supervisor only after
        // the production timer lease has observed cancellation and drained.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let timers = Arc::clone(&(*sup).restart_timers);
            schedule_delayed_restart(sup, 0, Duration::from_secs(30));
            assert_eq!(timers.pending_for_test(), 1);

            let started = Instant::now();
            hew_supervisor_stop(sup);

            assert!(
                started.elapsed() < Duration::from_secs(2),
                "public stop must interrupt a long restart backoff"
            );
            assert_eq!(timers.pending_for_test(), 0);
        }
    }

    #[test]
    fn runtime_cleanup_cancels_nested_supervisor_timer_in_one_pass() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test builds one parent-owned nested tree, then canonical
        // cleanup recursively cancels every timer before reclaiming either Box.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let timers = Arc::clone(&(*nested).restart_timers);
            schedule_delayed_restart(nested, 0, Duration::from_secs(30));
            assert_eq!(timers.pending_for_test(), 1);

            let started = Instant::now();
            crate::scheduler::hew_runtime_cleanup();

            assert!(
                started.elapsed() < Duration::from_secs(2),
                "recursive cleanup must wake a nested supervisor's long timer"
            );
            assert_eq!(timers.pending_for_test(), 0);
            assert!(
                crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "nested timer cancellation must finish in the initial cleanup pass"
            );
        }
    }

    #[test]
    fn supervisor_stop_detaches_timed_out_child_before_cleanup_handoff() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns both fresh supervisors. A deliberately retained
        // production timer lease forces the nested stop through its bounded
        // fail-closed handoff; releasing it then lets canonical cleanup reclaim
        // both independent roots.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let timer = (*nested)
                .restart_timers
                .begin()
                .expect("fresh nested supervisor accepts a timer lease");

            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert!(
                (*nested).parent.is_null(),
                "child must detach before a bounded stop can return it to cleanup"
            );
            assert!(
                crate::shutdown::is_supervisor_registered_for_test(nested),
                "timed-out detached child must become an independent cleanup root"
            );
            assert!(
                locked_roster!(parent).child_supervisors.is_empty(),
                "parent must not retain a second ownership edge"
            );

            drop(timer);
            crate::scheduler::hew_runtime_cleanup();
            assert!(crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null());
        }
    }

    #[test]
    fn supervisor_stop_retains_child_when_teardown_admission_is_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns both supervisors and closes admission only in
        // its isolated runtime immediately before canonical cleanup.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);

            crate::runtime::rt_current()
                .local_handles
                .close_supervisor_teardown_admission();
            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert_eq!((*nested).parent, parent);
            assert_eq!(locked_roster!(parent).child_supervisors, vec![nested]);
            assert!(
                !crate::shutdown::is_supervisor_registered_for_test(nested),
                "without teardown admission the original parent edge remains the sole root"
            );
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    #[test]
    fn nested_parent_claim_wins_before_token_stop_without_root_imbalance() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the parent dispatch owns roster extraction; the barrier holds
        // the claimed child live at the canonical teardown edge.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let token = (*nested).local_pid_id;

            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook_guard = install_supervisor_teardown_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));

            let parent_addr = parent as usize;
            let stop = std::thread::spawn(move || {
                supervisor_sys_dispatch_impl(
                    ptr::null_mut(),
                    (parent_addr as *mut HewSupervisor).cast::<c_void>(),
                    HewSysMsg::SupervisorStop as i32,
                    ptr::null_mut(),
                    0,
                );
            });
            entered.wait();

            assert_eq!(hew_local_pid_supervisor_stop(token), 1);
            assert!(locked_roster!(parent).child_supervisors.is_empty());
            assert!(!crate::shutdown::is_supervisor_registered_for_test(nested));
            assert_eq!(
                crate::shutdown::registered_supervisor_count_for_test(),
                1,
                "parent remains the only cleanup root after its child claim wins"
            );

            release.wait();
            stop.join().expect("parent-owned nested stop");
            drop(hook_guard);
            hew_supervisor_stop(parent);
            assert_eq!(crate::shutdown::registered_supervisor_count_for_test(), 0);
        }
    }

    #[test]
    fn nested_token_stop_wins_before_parent_claim_without_dangling_root() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the token pin is held across the barrier, proving the parent
        // can lose without dereferencing or publishing the nested pointer.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let token = (*nested).local_pid_id;

            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook_guard = install_supervisor_close_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));
            let token_stop = std::thread::spawn(move || hew_local_pid_supervisor_stop(token));
            entered.wait();

            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert!(locked_roster!(parent).child_supervisors.is_empty());
            assert!(!crate::shutdown::is_supervisor_registered_for_test(nested));
            assert_eq!(
                crate::shutdown::registered_supervisor_count_for_test(),
                1,
                "token-owned nested stop must not publish a second root while parent remains"
            );
            release.wait();
            assert_eq!(token_stop.join().expect("token-owned nested stop"), 0);
            drop(hook_guard);
            hew_supervisor_stop(parent);
            assert_eq!(crate::shutdown::registered_supervisor_count_for_test(), 0);
        }
    }

    #[test]
    fn supervisor_stop_detaches_child_before_deferred_spawn_failure() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test installs one nested child as current actor solely to
        // select the production deferred-stop branch.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let fail_guard = fail_owned_deferred_supervisor_spawn();
            let ctx = TestExecutionContext::install(HewExecutionContext {
                actor: nested_child,
                actor_id: (*nested_child).id,
                ..HewExecutionContext::default()
            });

            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert!((*nested).parent.is_null());
            assert!(locked_roster!(parent).child_supervisors.is_empty());
            assert!(
                crate::shutdown::is_supervisor_registered_for_test(nested),
                "deferred spawn failure must leave the detached child rooted"
            );

            drop(ctx);
            drop(fail_guard);
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    #[test]
    fn runtime_cleanup_retains_tree_when_timer_thread_cannot_drain() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the fresh tree and deliberately holds a real
        // timer lease past the bounded cleanup wait to exercise the fail-closed
        // fallback, then releases it before retrying reclamation.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            crate::shutdown::hew_shutdown_register_supervisor(sup);
            let timer = (*sup)
                .restart_timers
                .begin()
                .expect("fresh supervisor accepts a timer lease");

            crate::scheduler::hew_runtime_cleanup();

            assert!(
                crate::shutdown::is_supervisor_registered_for_test(sup),
                "canonical cleanup must retain a supervisor while a timer could dereference it"
            );
            assert!(
                actor::is_actor_live_with_id(child_id, child)
                    && actor::is_actor_live_with_id(self_id, self_actor),
                "incomplete cleanup must leave runtime-owned actors live with the retained tree"
            );
            assert!(
                !crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "incomplete cleanup must leave the runtime installed for retry"
            );

            drop(timer);
            crate::scheduler::hew_runtime_cleanup();
            assert!(
                crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "retry after timer drain must reclaim the retained tree and runtime"
            );
        }
    }

    #[test]
    fn supervisor_restart_completes_while_metrics_reset_runs_in_parallel() {
        let _rt = crate::runtime_test_guard();
        let _scheduler = RealSchedulerGuard::new();
        // SAFETY: this test owns the supervisor and joins the concurrent
        // metrics caller before stopping it.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            locked_roster!(sup).child_specs[0].restart_policy = RESTART_PERMANENT;
            hew_supervisor_set_restart_notify(sup);

            let started = Arc::new(std::sync::Barrier::new(2));
            let reset_started = Arc::clone(&started);
            let resetter = std::thread::spawn(move || {
                reset_started.wait();
                for _ in 0..2_048 {
                    crate::scheduler::hew_sched_metrics_reset();
                }
            });

            started.wait();
            actor::hew_actor_trap(child, 1);
            assert!(
                hew_supervisor_wait_restart(sup, 1, 2_000) >= 1,
                "a supervisor restart must complete while live metrics reset runs"
            );
            resetter.join().expect("metrics resetter must not panic");
            hew_supervisor_stop(sup);
        }
    }

    /// Installs a worker-backed scheduler for tests that need real dispatch,
    /// and tears it down symmetrically. `runtime_test_guard` alone installs a
    /// worker-LESS placeholder, so nothing would ever run a handler.
    struct RealSchedulerGuard;

    impl RealSchedulerGuard {
        fn new() -> Self {
            crate::scheduler::init_real_scheduler_for_test();
            Self
        }
    }

    impl Drop for RealSchedulerGuard {
        fn drop(&mut self) {
            crate::scheduler::hew_sched_shutdown();
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    /// Byte-compatible replica of the internal supervision event payload, as
    /// an attacker holding a supervisor's actor handle would hand-build it.
    /// Declared independently of `ChildEvent` so the forgery test keeps its
    /// teeth even if the internal struct is retyped.
    #[repr(C)]
    struct ForgedChildEvent {
        child_index: c_int,
        child_id: u64,
        exit_state: c_int,
        crash_code: c_int,
    }

    /// A supervision event forged on the USER queue must never free a live
    /// child.
    ///
    /// `hew_actor_send` is public C ABI and routes to the USER queue. Before
    /// the sys/user channel split, `supervisor_dispatch_impl` matched on the
    /// raw `msg_type` VALUE with no provenance gate, so a forged `ChildEvent`
    /// delivered on the user queue drove `take_child_slot` +
    /// `hew_actor_free` on a live child — a use-after-free reachable with no
    /// hash collision. Supervision events now arrive only through the typed
    /// system dispatch entry point, which the user queue cannot reach.
    #[test]
    fn user_queue_supervision_value_does_not_free_a_live_child() {
        let _rt = crate::runtime_test_guard();
        let _sched = RealSchedulerGuard::new();
        // SAFETY: the test owns the supervisor tree for the whole body.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;
            assert!(
                actor::is_actor_live_with_id(child_id, child),
                "precondition: the child is live before the forged send"
            );

            // The forged payload an attacker holding the supervisor's actor
            // handle would build: index 0, the live child's id, Crashed.
            let forged = ForgedChildEvent {
                child_index: 0,
                child_id,
                exit_state: HewActorState::Crashed as c_int,
                crash_code: 0,
            };
            // Every value in the former reserved block, not just the
            // supervision one: none may reach the system handler.
            for forged_type in 100..=105_i32 {
                crate::actor::hew_actor_send(
                    self_actor,
                    forged_type,
                    (&raw const forged).cast::<c_void>().cast_mut(),
                    std::mem::size_of::<ForgedChildEvent>(),
                );
            }

            let freed = wait_for_condition(std::time::Duration::from_secs(2), || {
                !actor::is_actor_live_with_id(child_id, child)
            });
            assert!(
                !freed,
                "a user-queue send of a reserved system value freed a LIVE \
                 supervised child (use-after-free)"
            );
            assert_eq!(
                hew_supervisor_child_count(sup),
                1,
                "the forged user-queue send must not alter the child roster"
            );

            // NON-VACUITY: "the child survived" only means something if the
            // supervisor was actually running and WOULD have acted on a real
            // event. Deliver the same ChildCrashed by its legitimate route —
            // the privileged system send — and require that it does reclaim
            // the child. The forgery and the real thing carry identical bytes;
            // only the channel differs, which is the whole point.
            hew_supervisor_notify_child_actor_event(
                sup,
                0,
                child_id,
                HewActorState::Crashed as c_int,
                0,
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "the supervision path must be live: an event delivered on the \
                 SYSTEM channel must reclaim the child, otherwise the forgery \
                 assertions above prove nothing"
            );

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn borrowed_legacy_actor_dispatches_normal_message_without_drop_authority() {
        let _rt = crate::runtime_test_guard();
        let _sched = RealSchedulerGuard::new();
        BORROWED_NORMAL_DISPATCH_COUNT.store(0, Ordering::SeqCst);
        BORROWED_NORMAL_DROP_COUNT.store(0, Ordering::SeqCst);

        // SAFETY: this test exclusively owns the supervisor through stop.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());
            let state = 9_u64;
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: std::ptr::from_ref(&state).cast_mut().cast(),
                init_state_size: std::mem::size_of::<u64>(),
                dispatch: Some(counted_borrowed_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            hew_supervisor_set_child_state_drop(sup, 0, count_borrowed_normal_drop);
            let child = locked_roster!(sup).children[0];
            assert!((*child).state_drop_borrowed.load(Ordering::Acquire));
            assert!((*child).state_drop_fn.is_some());
            assert!((*child).state_clone_fn.is_none());

            actor::hew_actor_send(child, 77, ptr::null_mut(), 0);
            assert!(
                wait_for_condition(Duration::from_secs(2), || {
                    BORROWED_NORMAL_DISPATCH_COUNT.load(Ordering::Acquire) == 1
                }),
                "legacy borrowed actor must complete an ordinary user dispatch"
            );
            assert!(
                !(*child).state_drop_consumed.load(Ordering::Acquire),
                "normal dispatch must not fabricate crash-escrow consumption"
            );

            hew_supervisor_stop(sup);
            assert_eq!(BORROWED_NORMAL_DROP_COUNT.load(Ordering::SeqCst), 0);
        }
    }

    #[test]
    fn reserved_two_child_roster_targets_setter_by_exact_index() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: construction and inspection are single-threaded; the test
        // owns the supervisor until stop.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            let states = [11_u64, 22_u64];
            for state in &states {
                let spec = HewChildSpec {
                    name: ptr::null(),
                    init_state: std::ptr::from_ref(state).cast_mut().cast(),
                    init_state_size: std::mem::size_of::<u64>(),
                    dispatch: Some(noop_child_dispatch),
                    sys_dispatch: None,
                    restart_policy: RESTART_TEMPORARY,
                    mailbox_capacity: -1,
                    overflow: OVERFLOW_DROP_NEW,
                    coalesce_key_fn: None,
                    coalesce_fallback: OVERFLOW_DROP_NEW,
                    message_drop_fn: None,
                    arena_cap_bytes: 0,
                    cycle_capable: 0,
                    on_crash: None,
                    lifecycle_fn: None,
                    init_fn: None,
                    config: ptr::null_mut(),
                    config_size: 0,
                };
                assert_eq!(
                    hew_supervisor_add_child_spec(sup, &raw const spec),
                    0,
                    "static add remains a status-only ABI"
                );
            }

            hew_supervisor_set_child_state_drop(sup, 1, count_borrowed_normal_drop);
            assert!(locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state_drop
                .load()
                .is_none());
            assert!(locked_roster!(sup).child_specs[1]
                .state_template
                .allocation
                .state_drop
                .load()
                .is_some());
            assert!(locked_roster!(sup).children[0]
                .as_ref()
                .unwrap()
                .state_drop_fn
                .is_none());
            assert!(locked_roster!(sup).children[1]
                .as_ref()
                .unwrap()
                .state_drop_fn
                .is_some());
            hew_supervisor_stop(sup);
        }
    }

    static TEARDOWN_RACE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn count_teardown_race_state_drop(_state: *mut c_void) {
        TEARDOWN_RACE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn init_counted_teardown_race_state(
        _config: *const c_void,
    ) -> HewChildInitResult {
        // SAFETY: the runtime owns and later libc-frees this wrapper.
        let state = unsafe { libc::malloc(std::mem::size_of::<u64>()) }.cast::<u64>();
        if state.is_null() {
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        // SAFETY: state points to one freshly allocated u64.
        unsafe { *state = 17 };
        HewChildInitResult {
            state: state.cast::<c_void>(),
            size: std::mem::size_of::<u64>(),
        }
    }

    unsafe fn make_supervisor_with_counted_child() -> *mut HewSupervisor {
        // SAFETY: this helper owns the fresh supervisor; the init thunk gives
        // each child independently-owned state so the callback is an actor-
        // teardown counter, not a legacy shallow-template alias counter.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: Some(init_counted_teardown_race_state),
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            hew_supervisor_set_child_state_drop(sup, 0, count_teardown_race_state_drop);
            sup
        }
    }

    fn assert_cleanup_waits_for_synchronous_stop_owner(stop_by_token: bool) {
        let _rt = crate::runtime_test_guard();
        TEARDOWN_RACE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the helper returns one live test-owned supervisor.
        let sup = unsafe { make_supervisor_with_counted_child() };
        if stop_by_token {
            // Exercise recursive supervisor destruction after cleanup has closed
            // new teardown admission. The child must share the parent's lease.
            // SAFETY: both fresh supervisors are test-owned and unparented.
            let child = unsafe { make_supervisor_with_counted_child() };
            assert_eq!(
                // SAFETY: the pointers are live and distinct.
                unsafe { hew_supervisor_add_child_supervisor(sup, child) },
                0
            );
        }
        // SAFETY: this top-level root remains live until the stop owner or
        // canonical cleanup consumes it.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        // SAFETY: the supervisor remains live until one of the joined owners
        // reclaims it.
        let token = unsafe { hew_supervisor_direct_id(sup) };

        let teardown_entered = Arc::new(std::sync::Barrier::new(2));
        let teardown_release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = teardown_entered.clone();
        let release_hook = teardown_release.clone();
        let paused = Arc::new(AtomicBool::new(false));
        let paused_hook = paused.clone();
        let _hook = install_supervisor_teardown_hook_for_test(Arc::new(move || {
            if !paused_hook.swap(true, Ordering::AcqRel) {
                entered_hook.wait();
                release_hook.wait();
            }
        }));

        let sup_addr = sup as usize;
        let stop = std::thread::spawn(move || {
            if stop_by_token {
                hew_local_pid_supervisor_stop(token)
            } else {
                // SAFETY: the test keeps the registered allocation live until
                // this raw stop has acquired its teardown lease.
                unsafe { hew_supervisor_stop(sup_addr as *mut HewSupervisor) };
                0
            }
        });
        teardown_entered.wait();

        assert!(!crate::shutdown::is_supervisor_registered_for_test(sup));
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            if stop_by_token { (1, 2) } else { (0, 1) }
        );
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_teardown_state_for_test(),
            (true, 1)
        );

        let drain_entered = Arc::new(std::sync::Barrier::new(2));
        let drain_release = Arc::new(std::sync::Barrier::new(2));
        crate::lifetime::local_handles::install_current_supervisor_teardown_drain_hook_for_test(
            drain_entered.clone(),
            drain_release.clone(),
        );
        let (cleanup_done_tx, cleanup_done_rx) = std::sync::mpsc::channel();
        let cleanup = std::thread::spawn(move || {
            crate::scheduler::hew_runtime_cleanup();
            cleanup_done_tx
                .send(())
                .expect("cleanup completion receiver");
        });
        drain_entered.wait();
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_teardown_state_for_test(),
            (false, 1)
        );
        assert!(matches!(
            cleanup_done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));
        assert_eq!(TEARDOWN_RACE_DROP_COUNT.load(Ordering::SeqCst), 0);

        teardown_release.wait();
        assert_eq!(stop.join().expect("stop thread"), 0);
        drain_release.wait();
        cleanup.join().expect("cleanup thread");
        cleanup_done_rx.recv().expect("cleanup completion");
        assert_eq!(
            TEARDOWN_RACE_DROP_COUNT.load(Ordering::SeqCst),
            if stop_by_token { 2 } else { 1 }
        );
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn runtime_cleanup_waits_for_synchronous_token_stop_owner() {
        assert_cleanup_waits_for_synchronous_stop_owner(true);
    }

    #[test]
    fn runtime_cleanup_waits_for_synchronous_raw_stop_owner() {
        assert_cleanup_waits_for_synchronous_stop_owner(false);
    }

    #[test]
    fn direct_supervisor_pin_blocks_stop_reclamation_until_use_finishes() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: fresh allocation remains live until the stop thread completes.
        unsafe { (*sup).running.store(1, Ordering::Release) };
        // SAFETY: sup is a live supervisor created above.
        let token = unsafe { hew_supervisor_direct_id(sup) };

        let entered = Arc::new(std::sync::Barrier::new(2));
        let release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = entered.clone();
        let release_hook = release.clone();
        let _hook = install_supervisor_access_hook_for_test(Arc::new(move || {
            entered_hook.wait();
            release_hook.wait();
        }));
        let resolver = std::thread::spawn(move || hew_local_pid_supervisor_is_running(token));
        entered.wait();

        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let stop = std::thread::spawn(move || {
            done_tx
                .send(hew_local_pid_supervisor_stop(token))
                .expect("stop completion receiver");
        });
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
        while crate::lifetime::local_handles::current_supervisor_counts_for_test().0 != 0 {
            assert!(
                std::time::Instant::now() < deadline,
                "stop did not retire route"
            );
            std::thread::yield_now();
        }
        assert!(matches!(
            done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));

        release.wait();
        assert_eq!(resolver.join().expect("resolver thread"), 0);
        stop.join().expect("stop thread");
        assert_eq!(done_rx.recv().expect("stop result"), 0);
        assert_eq!(hew_local_pid_supervisor_stop(token), 1);
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn supervisor_close_wins_before_late_resolver_pin() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: sup is live.
        let token = unsafe { hew_supervisor_direct_id(sup) };

        let close_entered = Arc::new(std::sync::Barrier::new(2));
        let close_release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = close_entered.clone();
        let release_hook = close_release.clone();
        let _close_hook = install_supervisor_close_hook_for_test(Arc::new(move || {
            entered_hook.wait();
            release_hook.wait();
        }));
        let access_ran = Arc::new(AtomicBool::new(false));
        let access_ran_hook = access_ran.clone();
        let _access_hook = install_supervisor_access_hook_for_test(Arc::new(move || {
            access_ran_hook.store(true, Ordering::Release);
        }));

        let stop = std::thread::spawn(move || hew_local_pid_supervisor_stop(token));
        close_entered.wait();
        assert_eq!(hew_local_pid_supervisor_is_running(token), 0);
        assert!(!access_ran.load(Ordering::Acquire));
        close_release.wait();
        assert_eq!(stop.join().expect("stop thread"), 0);
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn runtime_cleanup_waits_for_supervisor_pin_then_empties_controls() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: sup is live and registered as a cleanup root below.
        let token = unsafe { hew_supervisor_direct_id(sup) };
        // SAFETY: this fresh supervisor remains live until cleanup consumes it.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };

        let entered = Arc::new(std::sync::Barrier::new(2));
        let release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = entered.clone();
        let release_hook = release.clone();
        let _hook = install_supervisor_access_hook_for_test(Arc::new(move || {
            entered_hook.wait();
            release_hook.wait();
        }));
        let resolver = std::thread::spawn(move || hew_local_pid_supervisor_is_running(token));
        entered.wait();

        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let cleanup = std::thread::spawn(move || {
            crate::scheduler::hew_runtime_cleanup();
            done_tx.send(()).expect("cleanup completion receiver");
        });
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
        while crate::lifetime::local_handles::current_supervisor_counts_for_test().0 != 0 {
            assert!(
                std::time::Instant::now() < deadline,
                "cleanup did not retire supervisor routes"
            );
            std::thread::yield_now();
        }
        assert!(matches!(
            done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));

        release.wait();
        assert_eq!(resolver.join().expect("resolver thread"), 0);
        cleanup.join().expect("cleanup thread");
        done_rx.recv().expect("cleanup completion");
    }

    #[test]
    fn supervisor_pin_timeout_leaks_fail_closed_until_pin_drops() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: sup is live.
        let token = unsafe { hew_supervisor_direct_id(sup) };
        let pin = crate::lifetime::local_handles::pin_current_supervisor(token)
            .expect("live supervisor pin");

        // SAFETY: sup remains allocated and test-owned. A zero timeout forces
        // the fail-closed branch while `pin` remains held.
        assert!(!unsafe { close_supervisor_access_with_timeout(sup, std::time::Duration::ZERO) });
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 1)
        );
        assert_eq!(hew_local_pid_supervisor_is_running(token), 0);

        drop(pin);
        // SAFETY: the failed close leaked the still-allocated supervisor; this
        // retry drains the now-empty pin set and owns destruction.
        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn owned_stop_returns_root_to_cleanup_when_access_close_fails() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh top-level supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // Mirror the deferred/token handoff: canonical cleanup ownership is
        // removed only after teardown admission and the exact owner claim.
        // SAFETY: `sup` remains live through this fail-closed handoff.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        // SAFETY: `sup` is a live supervisor in the current runtime.
        let teardown = unsafe { begin_supervisor_teardown(sup) }.expect("teardown lease");
        assert!(claim_supervisor_teardown(sup));
        // SAFETY: the teardown owner now holds the allocation exclusively.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };

        FAIL_NEXT_SUPERVISOR_ACCESS_CLOSE.with(|slot| slot.set(true));
        // SAFETY: the exact teardown owner retains the live allocation.
        unsafe { stop_supervisor_owned(sup, &teardown) };

        assert!(
            crate::shutdown::is_supervisor_registered_for_test(sup),
            "failed access close must return the top-level allocation to cleanup"
        );
        // The handoff remains claimed until canonical post-worker cleanup so a
        // racing worker cannot become a second destructor.
        // SAFETY: the restored root keeps `sup` live until cleanup below.
        assert!(unsafe { teardown_is_claimed(sup) });
        drop(teardown);
        crate::scheduler::hew_runtime_cleanup();
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
    fn deferred_stop_returns_root_to_cleanup_after_scheduler_shutdown() {
        let _rt = crate::runtime_test_guard();
        let _serial = TEARDOWN_DRAIN_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // SAFETY: this test owns the supervisor tree and installs the child as
        // the current actor only long enough to select the deferred stop path.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            crate::shutdown::hew_shutdown_register_supervisor(sup);
            (*child)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: (*child).id,
                ..HewExecutionContext::default()
            });
            hew_supervisor_stop(sup);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    crate::lifetime::live_actors::deferred_teardown_thread_count() == 1
                }),
                "deferred supervisor owner must be registered before shutdown"
            );

            crate::scheduler::hew_sched_shutdown();
            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            assert!(
                crate::shutdown::is_supervisor_registered_for_test(sup),
                "shutdown-aware deferred stop must restore the top-level root"
            );
            assert!(
                teardown_is_claimed(sup),
                "handoff must keep teardown claimed until canonical cleanup"
            );

            crate::scheduler::hew_runtime_cleanup();
        }
    }

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
        // a background "deferred-free" thread that runs ordinary exact-
        // authority actor teardown on actors still tracked in
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
            let taken = take_child_slot(&raw mut *sup, 0);
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
            // actor resource teardown on `terminate_finished`
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
                locked_roster!(sup).child_supervisors.len(),
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

    /// Stable role lookup returns the child's semantic identity, never its
    /// allocation address, and the supervisor token fails closed after stop.
    #[test]
    fn local_pid_child_get_returns_token_and_rejects_retired_supervisor() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the complete tree and retains only stable scalar
        // identities after stop reclaims the raw allocations.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            let child_token = (*child).local_pid_id;

            let live = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(live.tag, 0);
            assert_eq!(live.reason, ChildSlotReason::Ok as u8);
            assert_eq!(live.handle as usize, usize::from(child_token));
            assert_ne!(
                live.handle, child,
                "stable lookup exposed a raw child pointer"
            );

            let unknown = hew_local_pid_supervisor_child_get(supervisor_token, 1);
            assert_eq!(unknown.tag, 2);
            assert_eq!(unknown.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(unknown.handle.is_null());

            hew_supervisor_stop(sup);
            let retired = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(retired.tag, 2);
            assert_eq!(retired.reason, ChildSlotReason::SupervisorShutdown as u8);
            assert!(retired.handle.is_null());
        }
    }

    /// A transient restart slot remains classified and never leaks a stale
    /// incarnation token through the stable lookup ABI.
    #[test]
    fn local_pid_child_get_null_restart_slot_is_transient() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the tree and restores the child before teardown.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let transient = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(transient.tag, 1);
            assert_eq!(transient.reason, ChildSlotReason::Restarting as u8);
            assert!(transient.handle.is_null());

            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// The lookup-token-then-send shape loses the ask when the restart
    /// machinery advances the slot inside the unlocked gap. Both faces of the
    /// window, forced deterministically at the exact seam the two-call codegen
    /// sequence exposed:
    ///
    /// 1. Token resolved, replacement lands, OLD incarnation stopped, THEN the
    ///    send: refused (`ErrActorStopped`) even though the caller observed a
    ///    Live slot at resolve time.
    /// 2. Token resolved, ask ACCEPTED by the old incarnation, THEN the
    ///    replacement wave retires it: the accepted ask's reply resolves null
    ///    with only the orphaned marker — the input that surfaced as a silent
    ///    join-site trap with no diagnostic.
    #[test]
    fn stale_role_token_send_after_replacement_loses_the_ask() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree and sequences the
        // replacement wave by hand; no worker threads run.
        unsafe {
            let (sup, old_child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;

            // Face 1: resolve → replace+stop → send.
            let live = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(live.tag, 0, "pre-race lookup must observe a Live slot");
            let old_token = (*old_child).local_pid_id;
            assert_eq!(
                live.handle as usize,
                usize::from(old_token),
                "the lookup handle word is the incarnation's stable token"
            );

            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null(), "replacement spawn must succeed");
            actor::hew_actor_stop(old_child);

            let ch = crate::reply_channel::hew_reply_channel_new();
            let status =
                actor::hew_local_pid_ask_with_channel(old_token, 7, ptr::null_mut(), 0, ch.cast());
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32,
                "a stale role token resolved before the replacement wave must refuse the send"
            );
            crate::reply_channel::hew_reply_channel_free(ch);
            // The old incarnation stopped Idle → Stopped; reclaim it.
            assert_eq!(actor::hew_actor_free(old_child), 0);

            // Face 2: resolve → ask accepted → replacement wave retires the
            // target. The accepted ask's reply resolves null + orphaned.
            let live2 = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(live2.tag, 0, "replacement slot must be Live");
            let repl_token = (*replacement).local_pid_id;
            assert_eq!(live2.handle as usize, usize::from(repl_token));
            let ch2 = crate::reply_channel::hew_reply_channel_new();
            let status2 = actor::hew_local_pid_ask_with_channel(
                repl_token,
                7,
                ptr::null_mut(),
                0,
                ch2.cast(),
            );
            assert_eq!(
                status2,
                crate::internal::types::HewError::Ok as i32,
                "the ask must be accepted while the incarnation is current"
            );

            // The replacement wave retires the accepted-ask target: pull it
            // from the slot and tear it down (worker-less runtime: force the
            // terminal state the drain would have produced, then free — the
            // free path retires the queued ask's sender ref).
            let retired = take_child_slot(&raw mut *sup, 0);
            assert_eq!(retired, replacement);
            assert!(
                scheduler::discard_queued_actor_for_test(retired),
                "worker-less fixture must consume the accepted ask's wake entry"
            );
            (*retired)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            assert_eq!(actor::hew_actor_free(retired), 0);

            let reply = crate::reply_channel::hew_reply_wait(ch2);
            assert!(
                reply.is_null(),
                "an ask orphaned by the replacement wave must resolve a null reply"
            );
            assert_eq!(
                crate::reply_channel::hew_reply_channel_is_orphaned(ch2),
                1,
                "the orphaned marker is the only fact the null-only reply carries"
            );
            assert_eq!(
                crate::reply_channel::hew_reply_channel_failure_kind(ch2),
                crate::internal::types::HEW_REPLY_FAIL_ACTOR_STOPPED,
                "the status-bearing surface classifies the retirement as an actor stop"
            );
            crate::reply_channel::hew_reply_channel_free(ch2);

            hew_supervisor_stop(sup);
        }
    }

    /// The owner-scoped role ask resolves the slot and submits under ONE
    /// `roster` critical section: the slot writers cannot interpose
    /// (probed at the seam), and the ask lands in the incarnation that was
    /// current at resolve time — never a later one, never nowhere.
    #[test]
    fn role_ask_submits_to_resolved_incarnation_under_slot_lock() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree; the seam hook probes the
        // lock from a joined helper thread (synchronization only, no sleeps).
        unsafe {
            let (sup, old_child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            let sup_addr = sup as usize;

            let probed = Arc::new(AtomicBool::new(false));
            let probed_hook = Arc::clone(&probed);
            *ROLE_ASK_SUBMIT_GAP_HOOK.lock_or_recover() = Some(Arc::new(move || {
                let handle = std::thread::spawn(move || {
                    // The supervisor outlives the ask call that fires this
                    // hook; the probe only touches the lock word.
                    let sup = sup_addr as *mut HewSupervisor;
                    (*sup).roster.try_lock().is_err()
                });
                let writer_excluded = handle.join().expect("lock probe thread");
                assert!(
                    writer_excluded,
                    "roster must be held at the resolve→submit seam so \
                     store_child_slot/take_child_slot cannot interpose"
                );
                probed_hook.store(true, Ordering::Release);
            }));

            let ch = crate::reply_channel::hew_reply_channel_new();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                42,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            *ROLE_ASK_SUBMIT_GAP_HOOK.lock_or_recover() = None;
            assert_eq!(status, crate::internal::types::HewError::Ok as i32);
            assert!(
                probed.load(Ordering::Acquire),
                "the seam hook must have run inside the critical section"
            );

            // A replacement landing AFTER the owner-scoped submission cannot
            // repoint the already-enqueued ask.
            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null());

            let old_mb = (*old_child).mailbox.cast::<mailbox::HewMailbox>();
            let node = mailbox::hew_mailbox_try_recv(old_mb);
            assert!(
                !node.is_null(),
                "the ask must be enqueued in the incarnation resolved under the lock"
            );
            assert_eq!((*node).msg_type, 42);
            assert_eq!(
                (*node).reply_channel,
                ch.cast(),
                "the enqueued node must carry the caller's reply channel"
            );
            let repl_mb = (*replacement).mailbox.cast::<mailbox::HewMailbox>();
            assert!(
                mailbox::hew_mailbox_try_recv(repl_mb).is_null(),
                "the replacement incarnation must not receive the pre-swap ask"
            );

            // Node free retires the queued sender ref (orphan path) so the
            // creator-side wait resolves; then reclaim both incarnations.
            mailbox::hew_msg_node_free(node);
            assert!(crate::reply_channel::hew_reply_wait(ch).is_null());
            crate::reply_channel::hew_reply_channel_free(ch);

            assert!(
                scheduler::discard_queued_actor_for_test(old_child),
                "worker-less fixture must consume the accepted ask's wake entry"
            );
            (*old_child)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            assert_eq!(actor::hew_actor_free(old_child), 0);
            hew_supervisor_stop(sup);
        }
    }

    /// A mid-restart (null) slot refuses the owner-scoped ask closed: nothing
    /// is enqueued and the caller's channel reference survives.
    #[test]
    fn role_ask_mid_restart_slot_fails_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the tree; nulls the slot to model the restart
        // window, then restores it for teardown.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let before = crate::reply_channel::active_channel_count();
            let ch = crate::reply_channel::hew_reply_channel_new();
            crate::hew_clear_error();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32,
                "a mid-restart slot must refuse, never guess a future incarnation"
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null(), "the refusal must record a diagnostic");
            let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
            assert!(
                msg.contains("Restarting"),
                "the refusal must carry the classified slot state (tag semantics); got: {msg}"
            );
            assert_eq!(
                crate::reply_channel::active_channel_count(),
                before + 1,
                "the refused ask must preserve the caller-owned channel reference"
            );
            crate::reply_channel::hew_reply_channel_free(ch);

            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// Mechanism-2 regression (dogfood F1): a synchronous stable-role refusal
    /// must classify itself in the TLS ask-error slot, because the suspending
    /// with-channel caller binds its `Err` kind from
    /// `hew_actor_ask_take_last_error`. Before the fix the refusal returned
    /// its `HewError` code with the slot unwritten, so the failure surfaced
    /// as `Err(AskError::NoError)` — the enum's own "not an error" sentinel.
    #[test]
    fn role_ask_refusal_records_actor_stopped_ask_error() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the tree; nulls the slot to model the restart
        // window, then restores it for teardown.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            // Drain any stale value so the assertion reads THIS refusal's write.
            let _ = crate::actor::hew_actor_ask_take_last_error();
            let ch = crate::reply_channel::hew_reply_channel_new();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32,
                "a mid-restart slot must refuse closed"
            );
            assert_eq!(
                crate::actor::hew_actor_ask_take_last_error(),
                crate::internal::types::AskError::ActorStopped as i32,
                "the refusal must record a real AskError kind, never leave the \
                 slot at None (which misreports the failure as no-error)"
            );
            crate::reply_channel::hew_reply_channel_free(ch);

            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// A null reply channel is rejected before submission, but must still
    /// classify the failure for with-channel callers that read the TLS slot.
    #[test]
    fn role_ask_null_channel_records_ask_error() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree and passes a null channel to
        // exercise the submit guard directly.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;

            let _ = crate::actor::hew_actor_ask_take_last_error();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ptr::null_mut(),
            );
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrOom as i32,
                "a null reply channel must be rejected as OOM"
            );
            assert_eq!(
                crate::actor::hew_actor_ask_take_last_error(),
                crate::internal::types::AskError::ActorStopped as i32,
                "the null-channel refusal must report a real AskError, not NoError"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// Fixture for the lock-order test: a supervised child with a BOUNDED
    /// Block-policy mailbox (capacity 1), so a second enqueue WAITS for space.
    unsafe fn make_supervisor_with_block_mailbox_child(
    ) -> (*mut HewSupervisor, *mut HewActor, *mut HewActor) {
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
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: 1,
                overflow: 0, // HewOverflowPolicy::Block
                coalesce_key_fn: None,
                coalesce_fallback: 0,
                message_drop_fn: None,
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

            let child = locked_roster!(sup).children[0];
            let self_actor = (*sup).self_actor;
            (sup, child, self_actor)
        }
    }

    /// LOCK-ORDER INVARIANT: the role-ask enqueue — including a Block-policy
    /// capacity WAIT — never runs under `roster`. Holding the lock
    /// across the wait closes a cycle: the submitter waits for the child to
    /// drain its full mailbox, while the child's own handler can be blocked
    /// acquiring `roster` for a stable-role ask of its own. The pinned
    /// hook proves the lock is FREE at submission time, and a full Block
    /// mailbox is then drained by the test to complete the waiting enqueue.
    #[test]
    fn role_ask_block_mailbox_wait_runs_outside_roster() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree; thread coordination uses
        // barriers and joins only (no sleeps).
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_block_mailbox_child();
            let supervisor_token = (*sup).local_pid_id;
            let sup_addr = sup as usize;

            // Fill the capacity-1 mailbox so the role ask's enqueue must wait.
            actor::hew_actor_send(child, 1, ptr::null_mut(), 0);
            assert!(
                mailbox::hew_mailbox_len((*child).mailbox.cast()) >= 1,
                "the pre-fill send must occupy the capacity-1 mailbox"
            );

            let entered_submit = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered_submit);
            let lock_free_at_submit = Arc::new(AtomicBool::new(false));
            let lock_free_hook = Arc::clone(&lock_free_at_submit);
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = Some(Arc::new(move || {
                // The resolve phase released roster before this point;
                // probe from a joined helper thread (try_lock from the owning
                // thread is not the invariant under test).
                let probe = std::thread::spawn(move || {
                    let sup = sup_addr as *mut HewSupervisor;
                    // The supervisor outlives the ask that fires this hook;
                    // the probe only touches the lock word.
                    (*sup).roster.try_lock().is_ok()
                });
                lock_free_hook.store(probe.join().expect("lock probe"), Ordering::Release);
                entered_hook.wait();
            }));

            let ch = crate::reply_channel::hew_reply_channel_new();
            let ch_addr = ch as usize;
            let submitter = std::thread::spawn(move || {
                // The channel outlives the submission (the main thread joins
                // before freeing); the token is a Copy scalar identity.
                hew_supervisor_role_ask_with_channel(
                    supervisor_token,
                    0,
                    42,
                    ptr::null_mut(),
                    0,
                    ch_addr as *mut c_void,
                )
            });

            // The submitter is at (or past) the pinned-submit seam; the slot
            // lock must be free even though its enqueue may be waiting for
            // mailbox capacity.
            entered_submit.wait();
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
            assert!(
                lock_free_at_submit.load(Ordering::Acquire),
                "roster must be FREE during the role-ask submission \
                 (the Block-policy capacity wait must not run under the slot lock)"
            );
            assert!(
                (*sup).roster.try_lock().is_ok(),
                "slot writers must not be excluded while the enqueue waits"
            );

            // Drain the pre-fill message: capacity frees and the waiting
            // enqueue completes.
            let mb = (*child).mailbox.cast::<mailbox::HewMailbox>();
            let prefill = mailbox::hew_mailbox_try_recv(mb);
            assert!(!prefill.is_null());
            mailbox::hew_msg_node_free(prefill);

            let status = submitter.join().expect("submitter thread");
            assert_eq!(
                status,
                crate::internal::types::HewError::Ok as i32,
                "the waiting enqueue must complete once capacity frees"
            );

            // Drain the ask node (retires its queued sender ref), resolve the
            // creator-side wait, and tear down.
            let ask_node = mailbox::hew_mailbox_try_recv(mb);
            assert!(!ask_node.is_null());
            assert_eq!((*ask_node).msg_type, 42);
            mailbox::hew_msg_node_free(ask_node);
            assert!(crate::reply_channel::hew_reply_wait(ch).is_null());
            crate::reply_channel::hew_reply_channel_free(ch);

            // Worker-less runtime: the enqueue left the child Runnable with no
            // worker to drain it; restore Idle so the supervisor stop's
            // quiescence wait can finalize it.
            (*child)
                .actor_state
                .store(HewActorState::Idle as i32, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// A retirement landing between the classified resolve and the ID-pinned
    /// submission fails CLOSED with a named refusal — the interleaving that
    /// was a use-after-free in the raw lookup-then-ask shape (an unpinned
    /// child pointer dereferenced after the incarnation was freed). Covers
    /// both role-ask entry points; the channel twin also preserves the
    /// caller's creator reference.
    #[test]
    fn role_ask_retirement_between_resolve_and_submit_fails_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the tree; the hook retires + frees the
        // resolved incarnation at the exact resolve→submit seam.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            let sup_addr = sup as usize;

            // The hook retires the CURRENT incarnation: pull it from the
            // slot, force the terminal state the drain would have produced
            // (worker-less runtime), and free it — the exact former-UAF
            // interleaving, now required to fail closed.
            let retire_hook: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
                let sup = sup_addr as *mut HewSupervisor;
                // The supervisor outlives the ask firing this hook; the
                // retired incarnation is exclusively owned once pulled from
                // the slot.
                let retired = take_child_slot(&raw mut *sup, 0);
                assert!(!retired.is_null(), "hook must find the live incarnation");
                (*retired)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(retired), 0);
            });

            // Channel twin.
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = Some(Arc::clone(&retire_hook));
            let before = crate::reply_channel::active_channel_count();
            let ch = crate::reply_channel::hew_reply_channel_new();
            crate::hew_clear_error();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null());
            let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
            assert!(
                msg.contains("retired during submission"),
                "the refusal must name the retirement interleaving; got: {msg}"
            );
            assert_eq!(
                crate::reply_channel::active_channel_count(),
                before + 1,
                "the refused ask must preserve the caller-owned channel reference"
            );
            crate::reply_channel::hew_reply_channel_free(ch);

            // Blocking twin: re-arm the slot with a fresh incarnation, retire
            // it at the same seam, and require the null + AskError refusal.
            let respawned = restart_child_from_spec(sup, 0);
            assert!(!respawned.is_null());
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = Some(retire_hook);
            let reply = hew_supervisor_role_ask(supervisor_token, 0, 7, ptr::null_mut(), 0);
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
            assert!(
                reply.is_null(),
                "the blocking role ask must refuse, never dereference the retired incarnation"
            );
            assert_eq!(
                actor::hew_actor_ask_take_last_error(),
                crate::internal::types::AskError::ActorStopped as i32,
                "the blocking refusal must bind AskError::ActorStopped"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// TOOTH for the masked-`id` reuse finding: after 2^48 allocations a fresh
    /// actor can carry a retired incarnation's packed `id` (the serial is masked
    /// to 48 bits). If phase two pinned by `id` alone it would submit to that
    /// DIFFERENT actor — the exact wrong-actor delivery the identity check
    /// exists to make impossible.
    ///
    /// This fabricates the alias at the resolve→submit seam: the hook retires
    /// the resolved incarnation A, then spawns a genuine actor B tracked under
    /// A's identical packed `id` but a DISTINCT full serial (a 2^48 wrap). A
    /// naive by-`id` pin would find B and enqueue to it; the identity-verified
    /// pin must instead refuse CLOSED (the pinned actor's full serial differs
    /// from the resolved one) and never enqueue. Covers both entry points; run
    /// 20× to shake out any ordering dependence.
    /// Build a resolve→submit-seam hook that fabricates the masked-`id` alias:
    /// it retires + frees the resolved incarnation A, then spawns a genuine
    /// actor B tracked under A's identical packed `id` but a DISTINCT full
    /// serial (a 2^48 wrap), publishing B's pointer through `alias_out`.
    ///
    /// # Safety
    ///
    /// `sup_addr` must be a live `*mut HewSupervisor` the caller owns for the
    /// hook's lifetime; the caller reclaims the actor stored in `alias_out`.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn make_role_ask_alias_hook(
        sup_addr: usize,
        alias_out: Arc<AtomicUsize>,
    ) -> Arc<dyn Fn() + Send + Sync> {
        Arc::new(move || {
            let sup = sup_addr as *mut HewSupervisor;
            // SAFETY: the caller owns `sup` for the hook's lifetime; the retired
            // incarnation is exclusively owned once pulled from the slot.
            unsafe {
                // Retire the resolved incarnation A and free it, so its masked
                // `id` is vacant in LIVE_ACTORS for reuse.
                let retired = take_child_slot(&raw mut *sup, 0);
                assert!(!retired.is_null(), "hook must find the live incarnation");
                let a_id = (*retired).id;
                let a_serial = (*retired).spawn_serial;
                (*retired)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(retired), 0);

                // Spawn a genuine actor B that ALIASES A's packed `id` (a 2^48
                // serial wrap) but carries a distinct full serial — precisely
                // the shape a real id-reuse produces.
                actor::override_next_spawn_actor_identity(a_id, a_serial.wrapping_add(1u64 << 48));
                let b = actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_child_dispatch));
                assert!(!b.is_null(), "alias actor B must spawn");
                assert_eq!((*b).id, a_id, "B must reuse A's masked packed id");
                assert_ne!(
                    (*b).spawn_serial,
                    a_serial,
                    "B must carry a distinct full serial"
                );
                alias_out.store(b as usize, Ordering::Release);
            }
        })
    }

    #[test]
    fn role_ask_masked_id_alias_refuses_closed_never_enqueues() {
        for _ in 0..20 {
            let _rt = crate::runtime_test_guard();
            // SAFETY: the test owns the tree; the hook retires the resolved
            // incarnation and installs a masked-`id`-aliasing replacement at the
            // exact resolve→submit seam, then the test reclaims both.
            unsafe {
                let (sup, _child, _self_actor) = make_supervisor_with_child();
                let supervisor_token = (*sup).local_pid_id;
                let sup_addr = sup as usize;
                // Carries the fabricated alias actor B out of the hook so the
                // test can assert it never received the ask and then free it.
                let alias_out = Arc::new(AtomicUsize::new(0));

                // ── Channel twin: refuse closed with the named diagnostic. ──
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() =
                    Some(make_role_ask_alias_hook(sup_addr, Arc::clone(&alias_out)));
                let before = crate::reply_channel::active_channel_count();
                let ch = crate::reply_channel::hew_reply_channel_new();
                crate::hew_clear_error();
                let status = hew_supervisor_role_ask_with_channel(
                    supervisor_token,
                    0,
                    7,
                    ptr::null_mut(),
                    0,
                    ch.cast(),
                );
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
                assert_eq!(
                    status,
                    crate::internal::types::HewError::ErrActorStopped as i32,
                    "an aliased id must refuse closed, never submit to the wrong actor"
                );
                let err = crate::hew_last_error();
                assert!(!err.is_null());
                let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
                assert!(
                    msg.contains("retired during submission"),
                    "the refusal must name the retirement; got: {msg}"
                );
                assert_eq!(
                    crate::reply_channel::active_channel_count(),
                    before + 1,
                    "the refused ask must preserve the caller-owned channel reference"
                );
                crate::reply_channel::hew_reply_channel_free(ch);

                // The aliasing actor B must NOT have received the ask.
                let b_channel = alias_out.swap(0, Ordering::AcqRel) as *mut HewActor;
                assert!(!b_channel.is_null(), "the channel-path hook must spawn B");
                assert_eq!(
                    crate::mailbox::hew_mailbox_len((*b_channel).mailbox.cast()),
                    0,
                    "the wrong-actor alias must never be enqueued (channel twin)"
                );
                (*b_channel)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(b_channel), 0);

                // ── Blocking twin: null reply + AskError::ActorStopped. ──
                let respawned = restart_child_from_spec(sup, 0);
                assert!(!respawned.is_null());
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() =
                    Some(make_role_ask_alias_hook(sup_addr, Arc::clone(&alias_out)));
                let reply = hew_supervisor_role_ask(supervisor_token, 0, 7, ptr::null_mut(), 0);
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
                assert!(
                    reply.is_null(),
                    "the blocking role ask must refuse, never deliver to the aliased actor"
                );
                assert_eq!(
                    actor::hew_actor_ask_take_last_error(),
                    crate::internal::types::AskError::ActorStopped as i32,
                    "the blocking refusal must bind AskError::ActorStopped"
                );

                let b_blocking = alias_out.swap(0, Ordering::AcqRel) as *mut HewActor;
                assert!(!b_blocking.is_null(), "the blocking-path hook must spawn B");
                assert_eq!(
                    crate::mailbox::hew_mailbox_len((*b_blocking).mailbox.cast()),
                    0,
                    "the wrong-actor alias must never be enqueued (blocking twin)"
                );
                (*b_blocking)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(b_blocking), 0);

                hew_supervisor_stop(sup);
            }
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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::Restarting as u8);
            assert!(result.handle.is_null());

            // Restore the slot so teardown can reach the actor.
            store_child_slot(&raw mut *sup, 0, child);
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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

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
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);

            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "notify_restart must drain every parked waiter"
            );
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                1,
                "notify must release only the observer ref"
            );
            // Match the codegen bind edge: the caller releases the creator ref.
            crate::read_slot::hew_read_slot_free(slot);

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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

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
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                1,
                "detach must release only the observer ref"
            );
            // The direct caller still releases the creator ref after detach.
            crate::read_slot::hew_read_slot_free(slot);

            // A later notify has nothing to wake (the waiter is gone).
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);
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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

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
                store_child_slot(&raw mut *sup, 0, child_addr as *mut HewActor);
                notify_restart(sup);
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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

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
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);
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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());
            locked_roster!(sup).child_specs[0].circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::CircuitOpen as u8);
            assert!(result.handle.is_null());

            // Restore before teardown.
            locked_roster!(sup).child_specs[0].circuit_breaker.state = 0; // HEW_CIRCUIT_BREAKER_CLOSED
            store_child_slot(&raw mut *sup, 0, child);
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
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());
            // 1 hour from now in nanoseconds
            locked_roster!(sup).child_specs[0].next_restart_time_ns =
                monotonic_time_ns().saturating_add(3_600_000_000_000);

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::BackoffDelay as u8);
            assert!(result.handle.is_null());

            // Restore before teardown.
            locked_roster!(sup).child_specs[0].next_restart_time_ns = 0;
            store_child_slot(&raw mut *sup, 0, child);
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

    #[allow(
        clippy::unnecessary_box_returns,
        reason = "the returned template keeps explicit source ownership in these tests"
    )]
    unsafe fn add_heap_child(
        sup: *mut HewSupervisor,
        restart_policy: c_int,
        register_clone: bool,
    ) -> Box<HeapState> {
        let template = make_heap_template();
        let spec = HewChildSpec {
            name: ptr::null(),
            init_state: std::ptr::from_ref(&*template).cast_mut().cast::<c_void>(),
            init_state_size: std::mem::size_of::<HeapState>(),
            dispatch: Some(noop_child_dispatch),
            sys_dispatch: None,
            restart_policy,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: None,
            config: ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            // SAFETY: caller owns `sup`; `spec` and its template are live for the call.
            unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
            0
        );
        // SAFETY: successful registration added exactly one child to live `sup`.
        let index = c_int::try_from(unsafe { locked_roster!(sup).child_count - 1 })
            .expect("test child index fits c_int");
        // SAFETY: the child was just added at `index`.
        unsafe {
            hew_supervisor_set_child_state_drop(sup, index, heap_state_drop);
            if register_clone {
                hew_supervisor_set_child_state_clone(sup, index, heap_state_clone);
            }
        }
        template
    }

    unsafe fn make_supervisor_with_heap_child(
        register_clone: bool,
    ) -> (*mut HewSupervisor, Box<HeapState>) {
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 4, 1);
            assert!(!sup.is_null());
            let template = add_heap_child(sup, RESTART_PERMANENT, register_clone);
            (sup, template)
        }
    }

    unsafe fn dispatch_terminal_child_event(
        sup: *mut HewSupervisor,
        child_index: usize,
        kind: HewSysMsg,
        terminal_state: HewActorState,
    ) {
        // SAFETY: caller owns live `sup` and supplies a valid child index.
        let child = unsafe { locked_roster!(sup).children[child_index] };
        assert!(!child.is_null());
        // The production scheduler establishes the terminal state before it
        // sends this event to the supervisor system mailbox.
        // SAFETY: `sup`, `child`, and the stack event remain live for this
        // synchronous call; this is the same implementation reached by the
        // system mailbox.
        unsafe {
            (*child)
                .actor_state
                .store(terminal_state as i32, Ordering::Release);
            (*sup).running.store(1, Ordering::Release);
            let event = ChildEvent {
                child_index: u32::try_from(child_index).expect("test index fits u32"),
                child_id: (*child).id,
                exit_state: terminal_state as c_int,
                crash_code: 0,
            };
            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                sup.cast::<c_void>(),
                kind as i32,
                (&raw const event).cast_mut().cast::<c_void>(),
                std::mem::size_of::<ChildEvent>(),
            );
        }
    }

    #[test]
    fn production_child_events_apply_exact_state_drop_authority() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        for (terminal_state, consumed) in [
            (HewActorState::Crashed, false),
            (HewActorState::Crashed, true),
            (HewActorState::Stopped, false),
        ] {
            reset_clone_counters();
            // SAFETY: each loop iteration exclusively owns its supervisor.
            unsafe {
                let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 4, 1);
                assert!(!sup.is_null());
                let _template = add_heap_child(sup, RESTART_TEMPORARY, true);
                let child = locked_roster!(sup).children[0];
                assert!(
                    !(*child).state_drop_borrowed.load(Ordering::Acquire),
                    "clone-backed state must carry final-drop authority"
                );

                if consumed {
                    // Model the dispatch crash escrow consuming the typed
                    // state before it publishes ChildCrashed.
                    heap_state_drop((*child).state);
                    actor::record_dispatch_state_drop_consumed(child);
                }

                let kind = if terminal_state == HewActorState::Crashed {
                    HewSysMsg::ChildCrashed
                } else {
                    HewSysMsg::ChildStopped
                };
                dispatch_terminal_child_event(sup, 0, kind, terminal_state);

                assert_eq!(
                    DROP_CALL_COUNT.load(Ordering::SeqCst),
                    1,
                    "{terminal_state:?}, consumed={consumed}: production event must invoke exactly one typed drop"
                );
                assert!(locked_roster!(sup).children[0].is_null());
                hew_supervisor_stop(sup);
            }
        }
    }

    #[test]
    fn one_for_all_fresh_sibling_keeps_its_final_drop_authority() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: the test owns the complete supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ALL, 8, 60);
            assert!(!sup.is_null());
            let _template0 = add_heap_child(sup, RESTART_PERMANENT, true);
            let _template1 = add_heap_child(sup, RESTART_PERMANENT, true);
            let failed = locked_roster!(sup).children[0];
            let sibling = locked_roster!(sup).children[1];
            assert!(!(*failed).state_drop_borrowed.load(Ordering::Acquire));
            assert!(!(*sibling).state_drop_borrowed.load(Ordering::Acquire));

            // The failed actor's escrow consumes one authority. The normally
            // stopped sibling must independently consume its own authority in
            // deferred teardown; neither is a shallow-template alias.
            heap_state_drop((*failed).state);
            actor::record_dispatch_state_drop_consumed(failed);
            dispatch_terminal_child_event(sup, 0, HewSysMsg::ChildCrashed, HewActorState::Crashed);
            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "one consumed crash plus one fresh stopped sibling must drop exactly twice"
            );
            assert!(!locked_roster!(sup).children[0].is_null());
            assert!(!locked_roster!(sup).children[1].is_null());
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn rest_for_one_fresh_sibling_drops_once_and_fault_escrow_is_suppressed() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: the test owns the complete supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_REST_FOR_ONE, 8, 60);
            assert!(!sup.is_null());
            let _template0 = add_heap_child(sup, RESTART_PERMANENT, true);
            let _template1 = add_heap_child(sup, RESTART_PERMANENT, true);
            let failed = locked_roster!(sup).children[0];
            let fresh_sibling = locked_roster!(sup).children[1];

            // Crash escrow consumes the failed incarnation exactly once. The
            // later sibling stops normally and retains its independent final
            // drop authority through deferred REST_FOR_ONE teardown.
            heap_state_drop((*failed).state);
            actor::record_dispatch_state_drop_consumed(failed);
            dispatch_terminal_child_event(sup, 0, HewSysMsg::ChildCrashed, HewActorState::Crashed);
            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "REST_FOR_ONE must suppress the consumed fault escrow and drop the fresh sibling once"
            );
            assert!(!locked_roster!(sup).children[0].is_null());
            assert!(!locked_roster!(sup).children[1].is_null());
            assert_ne!(locked_roster!(sup).children[1], fresh_sibling);
            hew_supervisor_stop(sup);
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

            let initial_child = locked_roster!(sup).children[0];
            assert!(!initial_child.is_null());
            let initial_state_ptr = (*initial_child).state;
            let spec_template_after_reg = locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state;
            assert_ne!(
                initial_state_ptr, spec_template_after_reg,
                "spec.init_state must be re-cloned to a distinct allocation; actor.state still byte-copied from original"
            );

            // Drive a restart. The supervisor sees state_clone_fn=Some and
            // routes through hew_actor_spawn_opts_adopt.
            let restarted = restart_child_from_spec(sup, 0);
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
            assert!(
                (*restarted).state_drop_fn.is_some(),
                "the second actor incarnation must retain the state-drop descriptor"
            );
            assert!(
                !(*restarted).state_drop_consumed.load(Ordering::Acquire),
                "a fresh restarted incarnation begins with unconsumed final-drop authority"
            );

            // Sentinel survived the round-trip.
            let restarted_payload = &*(*restarted).state.cast::<HeapState>();
            assert_eq!(restarted_payload.sentinel, 0xDEAD_BEEF);
            assert_eq!(restarted_payload.payload_len, b"original".len());

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn restart_snapshot_cannot_publish_across_clone_generation_replacement() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: the test owns the supervisor and joins the only racing
        // restart before reclaiming either incarnation.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(false);
            let initial = locked_roster!(sup).children[0];
            assert!((*initial).state_drop_borrowed.load(Ordering::Acquire));

            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook = install_restart_spec_snapshot_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));

            let sup_addr = sup as usize;
            let stale_restart = std::thread::spawn(move || {
                // SAFETY: the parent thread keeps `sup` live through join.
                restart_child_from_spec(sup_addr as *mut HewSupervisor, 0) as usize
            });
            entered.wait();

            hew_supervisor_set_child_state_clone(sup, 0, heap_state_clone);
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), 1);
            let stored = (*initial)
                .state_clone_fn
                .expect("setter must install clone authority before ownership");
            assert!(std::ptr::fn_addr_eq(
                stored,
                heap_state_clone as actor::HewStateCloneFn
            ));
            assert!(
                !(*initial).state_drop_borrowed.load(Ordering::Acquire),
                "successful generation replacement transfers the then-current actor to owned"
            );

            release.wait();
            assert_eq!(stale_restart.join().expect("stale restart"), 0);
            assert_eq!(
                locked_roster!(sup).children[0],
                initial,
                "old-generation restart must not replace the back-filled incarnation"
            );
            drop(hook);

            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null());
            assert!(!(*replacement).state_drop_borrowed.load(Ordering::Acquire));
            assert!((*replacement).state_clone_fn.is_some());

            actor::hew_actor_stop(initial);
            assert_eq!(actor::hew_actor_free(initial), 0);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn remove_child_drops_clone_backed_actor_and_template_exactly_once_each() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: this test exclusively owns the supervisor and its sole slot.
        unsafe {
            let (sup, _source_template) = make_supervisor_with_heap_child(true);
            assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 0);
            assert_eq!(hew_supervisor_remove_child(sup, 0), 0);
            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "remove must route actor state and independently-owned spec template through one typed drop each"
            );
            assert_eq!(hew_supervisor_remove_child(sup, 0), -1);
            hew_supervisor_stop(sup);
            assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 2);
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
            locked_roster!(sup).child_specs[0].circuit_breaker.state = 2; // HEW_CIRCUIT_BREAKER_HALF_OPEN
            let baseline_clone_calls = CLONE_CALL_COUNT.load(Ordering::SeqCst);

            CLONE_FORCE_NULL.store(true, Ordering::SeqCst);
            let restarted = restart_child_from_spec(sup, 0);
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
                locked_roster!(sup).child_specs[0].circuit_breaker.state,
                2,
                "circuit-breaker must remain HALF_OPEN; null-clone must NOT call record_success"
            );
            assert!(
                locked_roster!(sup).children[0].is_null(),
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

            let restarted = restart_child_from_spec(sup, 0);
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
            let child = locked_roster!(sup).children[0];
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
            let spec_template = &*locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state
                .cast::<HeapState>();
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
            let restarted = restart_child_from_spec(sup, 0);
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
        let spec_template = locked_roster!(sup).child_specs[0]
            .state_template
            .allocation
            .state
            .cast::<HeapState>();
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

        let restarted = restart_child_from_spec(sup, 0);
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
            let child = locked_roster!(sup).children[0];
            assert!(!child.is_null());

            // Poison the source every restart path actually reads:
            // spec.init_state, not the current child actor's state.
            let dangling_payload = free_spec_template_payload(sup);
            let baseline_clones = CLONE_CALL_COUNT.load(Ordering::SeqCst);

            CLONE_FORCE_NULL.store(true, Ordering::SeqCst);
            let restarted = restart_child_from_spec(sup, 0);
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
                locked_roster!(sup).children[0].is_null(),
                "blocked restart must leave the child slot null"
            );
            assert_eq!(
                (&*locked_roster!(sup).child_specs[0]
                    .state_template
                    .allocation
                    .state
                    .cast::<HeapState>())
                    .payload,
                dangling_payload,
                "test setup must leave the freed spec payload in place as the byte-copy falsifier"
            );

            CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
            // Null the already-freed spec payload so that InternalChildSpec::drop
            // (which now calls state_drop_fn before libc::free) does not double-free
            // the dangling pointer.  The falsifier assertion above already verified
            // it was in place; the test's correctness doesn't depend on it surviving
            // past that point.
            (&mut *locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state
                .cast::<HeapState>())
                .payload = ptr::null_mut();
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
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            let child = locked_roster!(sup).children[0];
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
///
/// Returns `false` when a delayed-restart timer still borrows this tree. The
/// incomplete tree remains registered and its runtime/actors must stay alive
/// rather than being freed unsafely. There is no automatic retry; an embedder
/// may explicitly invoke cleanup again after the borrower drains.
pub(crate) unsafe fn free_supervisor_resources(sup: *mut HewSupervisor) -> bool {
    // ROSTER-EXCLUSIVE: runtime cleanup runs after workers join and closes
    // supervisor access before traversing or dropping roster storage.
    // SAFETY: canonical cleanup still owns a live raw supervisor allocation.
    let Some(access) = (unsafe { close_supervisor_access(sup, SUPERVISOR_PIN_DRAIN_TIMEOUT) })
    else {
        set_last_error("runtime cleanup left a pinned supervisor allocated");
        // SAFETY: canonical cleanup still owns this live top-level root.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        return false;
    };
    // Clone the Arc before cancellation without creating an exclusive borrow:
    // a timer that already reached its deadline may still hold a shared raw
    // access until cancellation acquires the control mutex.
    // SAFETY: caller guarantees sup is valid.
    let restart_timers = Arc::clone(unsafe { &(*sup).restart_timers });
    // SAFETY: the allocation remains live through the bounded drain below.
    publish_supervisor_cancellation(sup);
    let deadline = Instant::now() + SUPERVISOR_CLEANUP_TIMER_DRAIN_TIMEOUT;
    if !wait_for_pending_restart_timers(&restart_timers, deadline) {
        set_last_error("runtime cleanup retained supervisor with pending restart timers");
        // The timer thread still owns a raw borrow. Retain this root; an
        // explicit later cleanup may reclaim it after the borrow drains, while
        // a one-shot teardown leaks it fail-closed. Freeing now would be a
        // use-after-free in the delayed-restart thread.
        // SAFETY: canonical cleanup still owns the live top-level allocation.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        return false;
    }
    // SAFETY: every timer raw borrow drained above; canonical cleanup now has
    // exclusive access to the supervisor allocation.
    let self_actor = unsafe { (*sup).self_actor };
    if !self_actor.is_null() {
        // Null out state so cleanup_all_actors won't libc::free it
        // (state points to the supervisor Box, not malloc'd memory).
        // SAFETY: self_actor is non-null (checked above) and valid for the supervisor's lifetime.
        unsafe {
            (*self_actor).state = ptr::null_mut();
            (*self_actor).state_size = 0;
        }
    }

    // Recursively free child supervisors. A child that cannot drain its
    // restart timer becomes an independent root before its parent Box is
    // dropped, so it never retains a dangling parent pointer.
    let mut complete = true;
    for (child_sup, _child_token, _child_spec) in take_nested_supervisor_roster(sup) {
        if !child_sup.is_null() {
            // SAFETY: child_sup is non-null (checked above), workers are joined,
            // and the parent still owns the child supervisor allocation.
            unsafe { (*child_sup).parent = ptr::null_mut() };
            // SAFETY: ownership was detached above; the child either frees now
            // or re-registers itself as an independent retained root.
            complete &= unsafe { free_supervisor_resources(child_sup) };
        }
    }
    // Drop the Box — child spec Drop impls free names + init_state.
    // SAFETY: sup was allocated with Box::into_raw and is valid per caller contract.
    drop(unsafe { Box::from_raw(sup) }); // ALLOCATOR-PAIRING: GlobalAlloc
    finish_supervisor_reclamation(&access);
    complete
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
    let (child_index, child_id, exit_state, crash_code) = {
        // SAFETY: caller keeps `sup` live; the guard prevents remove/restart
        // from reclaiming `child` while its event scalars are copied out.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: roster access is serialized by the guard.
        let s = &*guard;
        let Some(index) = s.children.iter().position(|candidate| *candidate == child) else {
            return;
        };
        let Ok(child_index) = u32::try_from(index) else {
            return;
        };
        // SAFETY: the matching roster slot owns this live child for the
        // duration of the critical section.
        let child_ref = unsafe { &*child };
        (
            child_index,
            child_ref.id,
            child_ref.actor_state.load(Ordering::Acquire),
            child_ref.error_code.load(Ordering::Acquire),
        )
    };

    // Notify the supervisor actor via the event system.
    // SAFETY: sup is valid and child_id / exit_state are read from valid memory.
    unsafe {
        hew_supervisor_notify_child_actor_event(sup, child_index, child_id, exit_state, crash_code);
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
    // SAFETY: caller guarantees `child` is live through this construction edge.
    let child_token = unsafe { (*child).local_pid_id };
    // SAFETY: caller keeps parent live through this registration.
    let mut guard = unsafe { &(*parent).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable parallel-roster access.
    let p = &mut *guard;
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_tokens.len());
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_specs.len());
    let idx = p.child_supervisors.len();
    p.child_supervisors.push(child);
    p.child_supervisor_tokens.push(child_token);
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
    // SAFETY: caller guarantees `child` is live through this construction edge.
    let child_token = unsafe { (*child).local_pid_id };
    // SAFETY: caller keeps parent live through this registration.
    let mut guard = unsafe { &(*parent).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable parallel-roster access.
    let p = &mut *guard;
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_tokens.len());
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_specs.len());
    let idx = p.child_supervisors.len();
    p.child_supervisors.push(child);
    p.child_supervisor_tokens.push(child_token);
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
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    // SAFETY: caller keeps `sup` live through this nested-roster read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped parallel-roster access.
    let s = &*guard;
    debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
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
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    load_child_slot(sup, i)
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
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    let pair = {
        // SAFETY: caller keeps `sup` live; restart notification publication is
        // serialized with child roster metadata.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        match &roster.restart_notify {
            Some(pair) => Arc::clone(pair),
            None => return ptr::null_mut(),
        }
    };

    // Fast path: child is already available.
    let child = load_child_slot(sup, i);
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
        let child = load_child_slot(sup, i);
        if !child.is_null() {
            return child;
        }
        // If the supervisor was cancelled, don't wait forever.
        // SAFETY: caller keeps `sup` live for this atomic read.
        if unsafe { (*sup).cancelled.load(Ordering::Acquire) } {
            return ptr::null_mut();
        }
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return ptr::null_mut();
        }
        let (new_guard, wait_result) = pair.1.wait_timeout_or_recover(guard, remaining);
        guard = new_guard;
        if wait_result.timed_out() {
            return load_child_slot(sup, i);
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
    // SAFETY: caller keeps `sup` live through this roster-count read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped parallel-roster access.
    let s = &*guard;
    debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
    debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
    (s.child_count + s.child_supervisors.len()) as c_int
}

/// Look up a static child by its compile-time-assigned slot index.
///
/// Non-blocking. Acquires `roster` briefly to read the slot pointer
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
    child_get_from_supervisor(sup, key, ChildHandleKind::RawPointer)
}

#[derive(Clone, Copy)]
enum ChildHandleKind {
    RawPointer,
    StableLocalPid,
}

/// Resolve one static child while the supervisor allocation is known live.
///
/// Both public lookup ABIs share this discriminator authority. The stable form
/// copies the current incarnation's `LocalPid` token while `roster` is
/// held, so no caller can retain or dereference the child allocation after the
/// restart machinery replaces it.
fn child_get_from_supervisor(
    sup: *mut HewSupervisor,
    key: u32,
    handle_kind: ChildHandleKind,
) -> ChildLookupResult {
    // Fast-path: supervisor-level shutdown check (no lock required — atomics).
    // SAFETY: caller keeps `sup` live for these atomic reads.
    // SAFETY: caller keeps the allocation live through this atomic re-check.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    // Critical section: read the slot pointer AND the per-slot discriminator
    // fields under the same lock so the (pointer, CB-state, backoff-timer)
    // triple is consistent with one lifecycle state from the FSM in §2.2.
    //
    // The default scheduler runs one worker per core, so readers here race the
    // restart machinery (store_child_slot / restart_child_from_spec) on other
    // workers; `roster` is the exclusion that keeps the discriminator
    // triple coherent. Future optimization: migrate to AtomicPtr<HewActor> +
    // atomic discriminator fields so readers can avoid the mutex on the common
    // Live path.
    // SAFETY: caller keeps `sup` live through the classified roster lookup.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped child/spec access.
    let s = &*guard;

    // Re-check shutdown under the lock (the supervisor could have been
    // cancelled or run out of budget between the atomic check above and
    // acquiring the lock).
    // SAFETY: the stable-identity pin keeps the allocation live through this
    // atomic re-check.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = key as usize;
    if i >= s.child_count {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    let slot = s.children.get(i).copied().unwrap_or(ptr::null_mut());
    if !slot.is_null() {
        let handle = match handle_kind {
            ChildHandleKind::RawPointer => slot,
            ChildHandleKind::StableLocalPid => {
                // SAFETY: a non-null slot is owned by this supervisor and the
                // children lock prevents replacement/reclamation through this
                // read. Actor publication installs the token before publishing
                // the slot; an invalid token is an invariant failure and must
                // fail closed rather than exposing the allocation address.
                let token = unsafe { (*slot).local_pid_id };
                if token == crate::lifetime::local_handles::HewLocalPidId::INVALID {
                    return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
                }
                usize::from(token) as *mut HewActor
            }
        };
        return ChildLookupResult::live(handle);
    }

    // Slot is null — classify why using the per-child spec.
    classify_null_child_slot(s, i)
}

/// Classify a null child slot from its per-child spec (the FSM in §2.2):
/// circuit-breaker cooldown, backoff timer pending, or an active restart.
/// Shared by the classified lookups and the owner-scoped role ask so both
/// surfaces name the same slot state. Caller holds `roster`;
/// `child_specs` is parallel to `children` after `hew_supervisor_start`, so
/// the index is always valid here.
fn classify_null_child_slot(s: &SupervisorRoster, i: usize) -> ChildLookupResult {
    // ROSTER-GUARDED-HELPER: every caller holds this supervisor's
    // `roster` for the complete borrowed-spec classification.
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

/// Look up a static child through a stable supervisor identity.
///
/// The returned `handle` word encodes the current child's stable `LocalPid`
/// token, never a child allocation pointer. Supervisor access is pinned for the
/// complete classified lookup; the child token is copied under `roster`.
/// A restart after return retires that exact token, so a subsequent token send
/// fails closed instead of retargeting reused storage.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_child_get(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
) -> ChildLookupResult {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    };
    let sup = pin.supervisor();
    let result = child_get_from_supervisor(sup, key, ChildHandleKind::StableLocalPid);
    drop(pin);
    result
}

/// Supervisors are unavailable on the wasm runtime; retain an exact symbol so
/// runtime-family parity stays exhaustive while codegen rejects the substrate.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_child_get(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
) -> ChildLookupResult {
    ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown)
}

/// Test-only hook fired inside [`role_resolve_current_child_id`] in the gap
/// between resolving the current child slot and returning its ID — with
/// `roster` HELD. The forced-interleaving regression installs a closure
/// that probes the lock from another thread at this exact point, proving the
/// restart machinery's slot writers (`store_child_slot` / `take_child_slot`)
/// cannot interpose inside the classified-resolution critical section.
#[cfg(all(test, not(target_arch = "wasm32")))]
static ROLE_ASK_SUBMIT_GAP_HOOK: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fire_role_ask_submit_gap_hook() {
    let hook = ROLE_ASK_SUBMIT_GAP_HOOK.lock_or_recover().clone();
    if let Some(hook) = hook {
        hook();
    }
}

/// Test-only hook fired by the role-ask entry points AFTER `roster` is
/// released and BEFORE the ID-pinned submission. Two regressions install it:
/// the lock-order test probes `roster` from a helper thread and asserts
/// it is FREE (the enqueue — including a Block-policy capacity wait — never
/// runs under the slot lock), and the retirement-interleaving test frees the
/// resolved incarnation here and asserts the submission fails closed instead
/// of touching reclaimed storage.
#[cfg(all(test, not(target_arch = "wasm32")))]
static ROLE_ASK_PINNED_SUBMIT_HOOK: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fire_role_ask_pinned_submit_hook() {
    let hook = ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover().clone();
    if let Some(hook) = hook {
        hook();
    }
}

/// Name a [`ChildSlotReason`] discriminant for the role-ask refusal
/// diagnostic. Fail-closed: an out-of-range discriminant (ABI drift) names
/// itself as such rather than borrowing a real reason's name.
#[cfg(not(target_arch = "wasm32"))]
const fn child_slot_reason_name(reason: u8) -> &'static str {
    match reason {
        r if r == ChildSlotReason::Ok as u8 => "Ok",
        r if r == ChildSlotReason::Restarting as u8 => "Restarting",
        r if r == ChildSlotReason::BackoffDelay as u8 => "BackoffDelay",
        r if r == ChildSlotReason::CircuitOpen as u8 => "CircuitOpen",
        r if r == ChildSlotReason::BudgetExhausted as u8 => "BudgetExhausted",
        r if r == ChildSlotReason::SupervisorShutdown as u8 => "SupervisorShutdown",
        r if r == ChildSlotReason::UnknownSlot as u8 => "UnknownSlot",
        _ => "(unrecognized ChildSlotReason discriminant)",
    }
}

/// Refuse an owner-scoped role ask closed, recording the classified slot
/// state so a Dead slot is never conflated with a Transient one at the
/// diagnostic surface (the tag semantics of the classified lookup ABIs —
/// contrast the tag-unchecked handle extraction the retired token path
/// performed, which collapsed every non-Live state into a token-0 send).
#[cfg(not(target_arch = "wasm32"))]
fn role_ask_refuse(reason: u8, key: u32) -> i32 {
    set_last_error(format!(
        "stable-role ask refused: child slot {key} is {}",
        child_slot_reason_name(reason)
    ));
    // Classify the refusal in the TLS ask-error slot: the suspending
    // with-channel caller binds its Err kind from
    // `hew_actor_ask_take_last_error`, and an unwritten slot misreports this
    // genuine refusal as `AskError::NoError` (dogfood F1, mechanism 2).
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Resolve the CURRENT incarnation of a stable-role slot to its actor ID
/// under ONE `roster` critical section, with every refusal classified
/// by slot state (see [`role_ask_refuse`]).
///
/// This is phase one of the owner-scoped role ask. The lock covers resolution
/// and classification ONLY — never the mailbox submission. LOCK-ORDER
/// INVARIANT: `roster` must not be held across a mailbox enqueue,
/// because a Block-policy mailbox at capacity WAITS for space
/// (`mailbox.rs` `HewOverflowPolicy::Block`), and the child draining that
/// mailbox may itself issue a stable-role ask that acquires `roster` —
/// holding the lock across the wait closes a cycle (submitter waits for the
/// drainer, drainer waits for the lock). Phase two therefore submits against
/// the returned ID via the `LIVE_ACTORS` send pin (`with_actor_send_by_id`),
/// the same liveness protocol every by-ID send uses: the pin guarantees the
/// allocation outlives the submission, and a retirement that lands between
/// the phases fails CLOSED instead of touching reclaimed storage.
///
/// The returned pair is `(packed id, full spawn serial)`. The packed `id`
/// masks the serial to 48 bits (`pid::hew_pid_make`), so two incarnations can in
/// principle collide on `id`; phase two therefore matches the pinned actor's
/// full serial against the resolved one
/// (`live_actors::with_actor_send_by_identity`) so an aliased `id` refuses
/// closed rather than delivering to the wrong actor. The spawn allocator refuses
/// past `pid::MAX_ACTOR_SERIAL` rather than wrapping, so the collision is not
/// reachable in production; the serial match is what keeps wrong-actor delivery
/// unrepresentable at this seam regardless. Both scalars are copied out under
/// `roster`; no pointer crosses the lock boundary.
#[cfg(not(target_arch = "wasm32"))]
fn role_resolve_current_child_id(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
) -> Result<(u64, u64), i32> {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return Err(role_ask_refuse(
            ChildSlotReason::SupervisorShutdown as u8,
            key,
        ));
    };
    let sup = pin.supervisor();

    // Fast-path shutdown check (atomics, no lock).
    // SAFETY: the stable-identity pin keeps `sup` live for these atomic reads.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return Err(role_ask_refuse(
            ChildSlotReason::SupervisorShutdown as u8,
            key,
        ));
    }
    // SAFETY: the stable-identity pin keeps `sup` live through this lookup.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped child/spec roster access.
    let s = &*guard;

    // Re-check shutdown under the lock (the supervisor can be cancelled
    // between the atomic check above and acquiring the lock).
    // SAFETY: the stable-identity pin keeps the allocation live through this
    // atomic re-check.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return Err(role_ask_refuse(
            ChildSlotReason::SupervisorShutdown as u8,
            key,
        ));
    }

    let i = key as usize;
    if i >= s.child_count {
        return Err(role_ask_refuse(ChildSlotReason::UnknownSlot as u8, key));
    }

    let child = s.children.get(i).copied().unwrap_or(ptr::null_mut());
    if child.is_null() {
        // Mid-restart (Transient) or permanently dead: refuse rather than
        // guess a future incarnation. Nothing was enqueued; the refusal
        // carries the classified slot state (the tag semantics the lookup
        // ABIs expose) so a Dead slot is never conflated with a Transient
        // one at the diagnostic surface.
        return Err(role_ask_refuse(classify_null_child_slot(s, i).reason, key));
    }

    #[cfg(test)]
    fire_role_ask_submit_gap_hook();

    // SAFETY: `child` is the slot's live incarnation and cannot be replaced or
    // reclaimed while `roster` is held; only the scalar id + full serial
    // are copied out — no pointer crosses the lock boundary. The full serial is
    // the aliasing-proof discriminator phase two re-checks against the pinned
    // actor (the packed id alone can alias after 2^48 allocations).
    Ok(unsafe { ((*child).id, (*child).spawn_serial) })
}

/// The classified refusal for a resolution that succeeded but whose
/// incarnation was retired before the ID-pinned submission could begin
/// (`with_actor_send_by_id` found the ID no longer live). Fail-closed and
/// named: the ask enqueued nothing.
#[cfg(not(target_arch = "wasm32"))]
fn role_ask_refuse_retired(key: u32) -> i32 {
    set_last_error(format!(
        "stable-role ask refused: child slot {key} incarnation retired during submission"
    ));
    // Same TLS classification contract as `role_ask_refuse`.
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Submit an ask with a caller-owned reply channel through a fungible
/// `(stable supervisor token, static slot)` role.
///
/// Two-phase owner-scoped submission, replacing the racy
/// lookup-token-then-send shape (`hew_local_pid_supervisor_child_get` followed
/// by `hew_local_pid_ask_with_channel`), whose unlocked gap let the restart
/// machinery advance the slot between resolution and submission — the
/// resolved identity went stale, or the ask was accepted by an incarnation
/// the supervisor was about to retire, orphaning the reply with no diagnostic
/// at the join site:
///
/// 1. [`role_resolve_current_child_id`]: resolve + classify under
///    `roster`; only the incarnation's scalar ID leaves the lock.
/// 2. ID-pinned submission via `with_actor_send_by_id`: the `LIVE_ACTORS` pin
///    keeps the allocation live for the complete enqueue (including a
///    Block-policy capacity wait, which deliberately runs OUTSIDE
///    `roster` — see the lock-order invariant on phase one). A
///    retirement interposing between the phases fails closed with a named
///    refusal; an ask accepted and then retired before dispatch resolves
///    through the classified-null machinery (`hew_reply_channel_failure_kind`).
///
/// Returns `HewError::Ok` on submission; every refusal is classified in the
/// error slot. The channel-reference discipline matches
/// [`crate::actor::hew_actor_ask_with_channel`]: the caller's creator ref
/// survives failure.
///
/// # Safety
///
/// `data` and `ch` must satisfy
/// [`crate::actor::hew_actor_ask_with_channel`]'s contract.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_role_ask_with_channel(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    let (child_id, child_serial) = match role_resolve_current_child_id(token, key) {
        Ok(ids) => ids,
        Err(code) => return code,
    };
    #[cfg(test)]
    fire_role_ask_pinned_submit_hook();
    crate::lifetime::live_actors::with_actor_send_by_identity(child_id, child_serial, |actor| {
        // SAFETY: the send pin keeps `actor` live for the submission;
        // `data`/`ch` follow this fn's contract. The identity-verified pin
        // refuses closed (returns None) if the id aliased a different
        // incarnation, so no wrong-actor enqueue can occur here.
        unsafe { crate::actor::ask_with_channel_pinned(actor, msg_type, data, size, ch) }
    })
    .unwrap_or_else(|| role_ask_refuse_retired(key))
}

/// Blocking twin of [`hew_supervisor_role_ask_with_channel`] for callers with
/// no parkable continuation (`main` / free functions): resolve the role under
/// `roster` (classified refusals), then run the ID-pinned blocking ask
/// (`hew_actor_ask_by_id` — the same pin + reply-wait protocol every by-ID ask
/// uses).
///
/// This replaces the raw lookup-then-ask pair the blocking fungible path
/// emitted (`hew_supervisor_child_get` returning an UNPINNED `*mut HewActor`,
/// then `hew_actor_ask` dereferencing it), whose gap let a restart free the
/// incarnation between the lookup and the deref — a use-after-free, not a
/// refusal. No pointer crosses the resolve/submit boundary here; a retirement
/// interposing between the phases fails closed to a null reply with
/// `AskError::ActorStopped`.
///
/// Return contract matches [`crate::actor::hew_actor_ask`]: the reply buffer
/// (caller frees) or null with the ask error recorded for
/// `hew_actor_ask_take_last_error`.
///
/// # Safety
///
/// `data` must point to at least `size` readable bytes, or be null when
/// `size` is 0.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_role_ask(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let Ok((child_id, child_serial)) = role_resolve_current_child_id(token, key) else {
        return crate::actor::actor_ask_null_actor_stopped();
    };
    #[cfg(test)]
    fire_role_ask_pinned_submit_hook();
    // SAFETY: the identity-verified by-ID ask pins the resolved actor for the
    // submission and blocks on the reply channel; `data`/`size` follow this
    // fn's contract. A serial mismatch (aliased id) fails closed to a null
    // reply with `AskError::ActorStopped`, never a wrong-actor delivery.
    unsafe { crate::actor::hew_actor_ask_by_identity(child_id, child_serial, msg_type, data, size) }
}

/// Supervisors are unavailable on the wasm runtime; the owner-scoped role ask
/// keeps symbol parity and fails closed exactly like the lookup twin above.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_supervisor_role_ask_with_channel(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _ch: *mut c_void,
) -> i32 {
    // Same TLS classification contract as the native refusal paths: the
    // with-channel caller binds its Err kind from the ask-error slot.
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Supervisors are unavailable on the wasm runtime; the blocking role ask
/// keeps symbol parity and fails closed to a null reply.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_supervisor_role_ask(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) -> *mut c_void {
    // Null reply + classified error slot, matching the native blocking twin's
    // `actor_ask_null_actor_stopped` refusal surface.
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    ptr::null_mut()
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
    // SAFETY: caller keeps `sup` live for these atomic reads.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = key as usize;
    // SAFETY: caller keeps `sup` live through this nested-roster lookup.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped parallel-roster access.
    let s = &*guard;
    debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
    if i >= s.child_supervisors.len() {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // The pointer and its stable token are one parallel-roster entry protected
    // by `roster`; restart, nested stop, and public lookup cannot observe
    // different generations of the pair.
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
    // SAFETY: caller guarantees sup is valid for this atomic load.
    unsafe { (*sup).running.load(Ordering::Acquire) }
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

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let index = child_index as usize;

    // SAFETY: caller keeps `sup` live through this metadata update.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
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

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let index = child_index as usize;

    // SAFETY: caller keeps `sup` live through this metadata read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped roster access.
    let s = &*guard;
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
    // SAFETY: caller guarantees `spec` is valid.
    let sp = unsafe { &*spec };

    // The v0.6 init-closure restart model: a dynamic child carrying an init_fn
    // produces its state via the thunk; skip the byte-copy template (mirror
    // hew_supervisor_add_child_spec).
    let has_init_fn = sp.init_fn.is_some();

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
        // Portable strdup (libc::strdup unavailable on Windows-MSVC, #2505).
        unsafe { crate::cabi::cstr_strdup(sp.name) }
    };

    let mut internal_spec = InternalChildSpec {
        identity: 0,
        revision: 1,
        name: name_copy,
        state_template: Arc::new(ChildStateTemplate {
            allocation: Arc::new(ChildStateTemplateAllocation {
                state: state_copy,
                size: if has_init_fn { 0 } else { sp.init_state_size },
                owns_typed_fields: false,
                state_drop: Arc::new(ChildStateDropDescriptor::new()),
            }),
            clone_fn: None,
        }),
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        coalesce_key_fn: sp.coalesce_key_fn,
        coalesce_fallback: sp.coalesce_fallback,
        message_drop_fn: sp.message_drop_fn,
        sys_dispatch: sp.sys_dispatch,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        cycle_capable: sp.cycle_capable,
        on_crash: sp.on_crash,
        // Carried IN the spec literal (like on_crash) so the dynamic child's
        // initial spawn — which also routes through restart_child_from_spec —
        // fires the lifecycle wrapper.
        lifecycle_fn: sp.lifecycle_fn,
        // Carried IN the spec literal so the dynamic child's initial spawn uses
        // the thunk (the load-bearing first-spawn carrier).
        init_fn: sp.init_fn,
        // The exact adopted buffer is installed atomically with roster
        // reservation below. No supervisor borrow crosses this construction.
        config: ptr::null_mut(),
    };

    let (i, should_spawn) = {
        // SAFETY: caller keeps `sup` live; this guard is the sole authority for
        // config adoption and the complete child/spec placeholder reservation.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;

        // Adopt the supervisor config buffer (idempotent on the same pointer)
        // in the same transaction that publishes the spec borrowing it.
        if has_init_fn && !sp.config.is_null() {
            if s.config_buf.is_null() {
                s.config_buf = sp.config;
                s.config_size = sp.config_size;
            } else if s.config_buf != sp.config {
                set_last_error(
                    "hew_supervisor_add_child_dynamic: conflicting supervisor config buffer",
                );
                // SAFETY: `sp.config` is a libc-allocated orphan distinct from
                // the already-adopted buffer (ALLOCATOR-PAIRING: libc).
                unsafe { libc::free(sp.config) };
                return -1;
            }
            internal_spec.config = s.config_buf;
        }

        let i = s.child_count;
        internal_spec.identity = s.next_child_spec_identity;
        let Some(next_identity) = s.next_child_spec_identity.checked_add(1) else {
            set_last_error("hew_supervisor_add_child_dynamic: child-spec identity exhausted");
            return -1;
        };
        s.next_child_spec_identity = next_identity;
        s.child_specs.push(internal_spec);
        s.children.push(ptr::null_mut());
        s.child_count += 1;
        debug_assert_eq!(s.children.len(), s.child_specs.len());
        // SAFETY: caller keeps `sup` live; running is atomic and independent of
        // the mutex-protected roster transaction.
        (i, unsafe { (*sup).running.load(Ordering::Acquire) != 0 })
    };

    run_dynamic_child_reserved_hook_for_test();

    // Spawn the child if the supervisor is running.
    if should_spawn {
        // SAFETY: spec is valid.
        unsafe { restart_child_from_spec(sup, i) };
    }
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
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // Extract the complete ownership unit under the same lock used by restart
    // snapshots and setters. `InternalChildSpec` (including its Arc template)
    // is then dropped normally outside the lock; no manual wrapper-only free
    // can bypass typed template teardown.
    let (child, removed_spec) = {
        // SAFETY: caller guarantees `sup` live; roster references exist only
        // inside this lock scope.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        if idx >= s.child_count {
            return -1;
        }
        debug_assert_eq!(s.children.len(), s.child_specs.len());
        debug_assert_eq!(s.child_count, s.children.len());

        let previous_last = s.child_count - 1;
        let child = s.children.swap_remove(idx);
        let removed_spec = s.child_specs.swap_remove(idx);
        s.child_count -= 1;

        // Static-backed pool membership names child roster indices. Apply the
        // same swap-remove mapping atomically: remove the retired membership,
        // then retarget references to the former last child to its new slot.
        for pool_spec in &mut s.pool_specs {
            pool_spec.static_members.retain(|member| *member != idx);
            if idx != previous_last {
                for member in &mut pool_spec.static_members {
                    if *member == previous_last {
                        *member = idx;
                    }
                }
            }
        }

        if idx < s.child_count {
            let swapped = s.children[idx];
            if !swapped.is_null() {
                // SAFETY: the swapped child remains live and is now owned by
                // the roster slot at `idx`.
                #[expect(
                    clippy::cast_possible_truncation,
                    clippy::cast_possible_wrap,
                    reason = "child index fits in i32 for any reasonable child count"
                )]
                // SAFETY: `swapped` is the live actor retained in the roster.
                unsafe {
                    (*swapped).supervisor_child_index = idx as i32;
                }
            }
        }
        (child, removed_spec)
    };

    // Stop and free the extracted actor after releasing the roster lock.
    if !child.is_null() {
        // SAFETY: child pointer is valid.
        unsafe { actor::hew_actor_stop(child) };
        // SAFETY: child was stopped.
        unsafe { actor::hew_actor_free(child) };
    }
    drop(removed_spec);
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
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // Publish the descriptor and back-fill the current incarnation in one
    // roster critical section. Every immutable template generation shares the
    // same atomic descriptor, so an outstanding restart lease also observes it.
    // SAFETY: caller guarantees `sup` live; mutation is scoped to this roster
    // critical section.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if idx >= s.child_count {
        return;
    }
    s.child_specs[idx]
        .state_template
        .allocation
        .state_drop
        .store(state_drop_fn);

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
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // SAFETY: caller guarantees `sup` live; lifecycle metadata is part of the
    // synchronized child spec roster.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if idx >= s.child_count {
        return;
    }

    // Store only. The initial-spawn fire already ran inside add_child_spec
    // (reading the literal-carried pointer); do NOT re-fire here.
    let Some(next_revision) = s.child_specs[idx].revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_lifecycle: child-spec revision exhausted");
        return;
    };
    s.child_specs[idx].lifecycle_fn = Some(lifecycle_fn);
    s.child_specs[idx].revision = next_revision;
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
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // Lease the current immutable generation under the roster lock, then run
    // the unsafe callback lock-free. The Arc keeps `template_ptr` alive across
    // concurrent replacement/removal and permits callback re-entrancy.
    let (spec_identity, old_template) = {
        // SAFETY: caller guarantees `sup` live; this roster reference is
        // released before invoking the callback below.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard protects this scoped immutable roster access.
        let s = &*guard;
        if idx >= s.child_count {
            return;
        }
        (
            s.child_specs[idx].identity,
            Arc::clone(&s.child_specs[idx].state_template),
        )
    };

    let template_ptr = old_template.allocation.state;
    let template_size = old_template.allocation.size;
    let mut transferred_initial_state_ownership = false;
    let new_allocation = if template_size > 0 && !template_ptr.is_null() {
        // SAFETY: template_ptr is a malloc'd wrapper of template_size bytes
        // produced by hew_supervisor_add_child_spec's byte-copy; the
        // contract of state_clone_fn admits reading from such a wrapper as
        // long as it has not yet been mutated. The race-window analysis in
        // the doc comment justifies the no-mutation precondition.
        let fresh = unsafe { state_clone_fn(template_ptr.cast_const()) };
        if fresh.is_null() {
            Arc::clone(&old_template.allocation)
        } else {
            transferred_initial_state_ownership = true;
            Arc::new(ChildStateTemplateAllocation {
                state: fresh,
                size: template_size,
                owns_typed_fields: true,
                state_drop: Arc::clone(&old_template.allocation.state_drop),
            })
        }
    } else {
        Arc::clone(&old_template.allocation)
    };

    let new_template = Arc::new(ChildStateTemplate {
        allocation: new_allocation,
        clone_fn: Some(state_clone_fn),
    });

    // Install only if this exact spec still occupies the slot. A remove/swap
    // or competing setter may have advanced it while the callback ran.
    // SAFETY: callback returned and no supervisor reference crossed it;
    // reacquire before creating a scoped mutable roster reference.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if s.child_specs
        .get(idx)
        .is_none_or(|spec| spec.identity != spec_identity)
    {
        return;
    }
    let Some(next_revision) = s.child_specs[idx].revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_state_clone: child-spec revision exhausted");
        return;
    };
    s.child_specs[idx].state_template = new_template;
    s.child_specs[idx].revision = next_revision;

    // Register on the already-spawned actor for its first run (the initial
    // spawn happens inside add_child_spec before this setter is called).
    let child = s.children[idx];
    if !child.is_null() {
        // Install the clone descriptor before publishing owned provenance. A
        // dispatch that observes owned provenance therefore cannot observe a
        // half-registered actor.
        // SAFETY: child is a valid actor pointer; state_clone_fn has the
        // correct signature.
        unsafe { actor::hew_actor_set_state_clone(child, state_clone_fn) };
        if transferred_initial_state_ownership {
            // The old shallow template wrapper is gone and its field owners
            // now belong solely to the initial actor. Clear only the borrowed
            // provenance bit; a racing crash-escrow consumption remains set in
            // its independent atomic authority.
            // SAFETY: child is the live initial incarnation whose alias was
            // broken by the successful clone immediately above.
            unsafe { actor::mark_state_drop_owned(child) };
        }
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
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // SAFETY: caller guarantees `sup` live; init/config publication is part of
    // the child spec roster and never calls `init_fn` under this lock.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if idx >= s.child_count {
        return;
    }

    let Some(next_revision) = s.child_specs[idx].revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_init_fn: child-spec revision exhausted");
        return;
    };

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
    s.child_specs[idx].revision = next_revision;
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
    // SAFETY: caller guarantees sup is valid; config ownership is serialized
    // with the child specs whose init thunks borrow it.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable metadata access.
    let s = &mut *guard;
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

    // Snapshot the restart counter BEFORE the pre-park check. `notify_restart`
    // bumps this counter (under `restart_notify.0`) before it drains waiters
    // (under `restart_await_waiters`), so re-reading it inside the registration
    // critical section detects a restart that completed in the gap between the
    // pre-park check and the push — the lost-wakeup guard (mirrors the `baseline`
    // discipline in `hew_supervisor_restart_await_blocking`).
    let notify = restart_notify_snapshot(sup);
    let baseline = notify.as_ref().map_or(0, |pair| *pair.0.lock_or_recover());

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
    // SAFETY: caller keeps `sup` live while its waiter registry is updated.
    let mut waiters = unsafe { &(*sup).restart_await_waiters }.lock_or_recover();
    let advanced = notify
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
    // SAFETY: caller keeps `sup` live while its waiter registry is updated.
    let mut waiters = unsafe { &(*sup).restart_await_waiters }.lock_or_recover();
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
    let notify = restart_notify_snapshot(sup);
    let baseline = notify.as_ref().map_or(0, |pair| *pair.0.lock_or_recover());

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

        if let Some(ref pair) = notify {
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
    // SAFETY: caller keeps `sup` live; notification option publication shares
    // the roster lock with all readers.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped option mutation.
    let s = &mut *guard;
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
    let Some(pair) = restart_notify_snapshot(sup) else {
        return 0;
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
        // Portable strdup (libc::strdup unavailable on Windows-MSVC, #2505).
        unsafe { crate::cabi::cstr_strdup(name) }
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

    // SAFETY: caller keeps `sup` live; pool metadata shares the roster lock so
    // static membership cannot race dynamic child removal.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable pool-roster access.
    let s = &mut *guard;
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
    // SAFETY: caller keeps `sup` live through this membership update.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects the pool roster and selected pool lifetime.
    let s = &*guard;
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
    // SAFETY: caller keeps `sup` live through this static membership update.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes child bounds and pool membership together.
    let s = &mut *guard;
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
    // SAFETY: caller keeps `sup` live through this membership update.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects the pool roster and selected pool lifetime.
    let s = &*guard;
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
/// `index` is `u64` (not `u32`) specifically so this function can
/// bounds-check the caller's *original, untruncated* index. A pool's real
/// member count always fits comfortably in a `u32`, so any `index` that does
/// not itself fit in `usize`/`u32` range is unconditionally out of bounds and
/// is rejected as `Dead(UnknownSlot)` before any narrowing cast — matching
/// `Vec[i]` OOB-parity semantics for arbitrarily large indices instead of
/// silently wrapping (see hew-lang/hew#2244).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_child_get(
    sup: *mut HewSupervisor,
    pool_key: u32,
    index: u64,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // Fast-path: supervisor-level shutdown.
    // SAFETY: caller keeps `sup` live for these atomic reads.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    // `index` arrives untruncated here (u64). No real pool ever has anywhere
    // near `usize::MAX` members, so any index that overflows `usize` (only
    // possible on a 32-bit `usize` target) is unconditionally OOB. On the
    // universally-supported 64-bit targets this `try_into` is infallible, but
    // keeping the target-generic checked conversion (rather than a bare `as`)
    // is what actually closes the truncation-and-wraparound gap this function
    // exists to prevent (hew-lang/hew#2244) -- an `as usize` here would just
    // move the same silent-wraparound bug from `i64->u32` (codegen) to
    // `u64->usize` (here) on a 32-bit target instead of removing it.
    let Ok(index) = usize::try_from(index) else {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    };

    let i = pool_key as usize;
    // SAFETY: caller keeps `sup` live through this pool/child roster lookup.
    let roster_guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped pool/child roster access.
    let s = &*roster_guard;
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
        let Some(&static_idx) = static_members.get(index) else {
            // Index beyond the member count → out of bounds (Vec[i] OOB parity).
            return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
        };
        // Resolve through the static slot resolver, which holds `roster`
        // and reads the (pointer, CB-state, backoff) triple coherently.
        #[expect(
            clippy::cast_possible_truncation,
            reason = "static child count is always small; u32 is the ABI key type"
        )]
        let key = static_idx as u32;
        drop(roster_guard);
        // SAFETY: sup is valid (checked above); the resolver re-checks bounds.
        return unsafe { hew_supervisor_child_get(sup, key) };
    }

    // ── Dynamic PID-set pool path ────────────────────────────────────────────
    // Resolve the index within the pool's member list via the public ABI so
    // we stay within the module's encapsulation boundary.
    // SAFETY: pool was created by hew_pool_new and has not been freed.
    let pid = unsafe { crate::pool::hew_pool_get_at(pool, index) };
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
    // SAFETY: caller keeps `sup` live through this pool-roster read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped pool metadata access.
    let s = &*guard;
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
#[allow(
    unused_unsafe,
    reason = "test-owned raw supervisors often group several unsafe operations"
)]
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
    use std::sync::{Arc, Barrier};

    macro_rules! locked_roster {
        ($sup:expr) => {{
            unsafe { &(*$sup).roster }
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
        }};
    }

    use super::{
        actor, install_dynamic_child_reserved_hook_for_test, restart_child_from_spec,
        restart_with_budget_and_strategy, supervisor_sys_dispatch_impl, take_child_slot,
        ChildEvent, ChildSlotReason, HewChildInitResult, HewChildSpec, HewSupervisor,
        OVERFLOW_DROP_NEW, RESTART_PERMANENT, RESTART_TEMPORARY,
    };
    use crate::internal::types::HewActorState;
    use crate::mailbox_header::HewSysMsg;
    use crate::supervisor::{
        hew_supervisor_add_child_dynamic, hew_supervisor_add_child_spec, hew_supervisor_new,
        hew_supervisor_pool_add_slot, hew_supervisor_pool_child_get, hew_supervisor_pool_len,
        hew_supervisor_pool_member_add, hew_supervisor_pool_member_add_static,
        hew_supervisor_pool_member_remove, hew_supervisor_remove_child,
        hew_supervisor_set_child_state_drop, hew_supervisor_start, hew_supervisor_stop,
    };
    use crate::util::MutexExt;

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
    fn dynamic_add_reservation_cannot_publish_into_removed_or_swapped_slot() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        assert!(!sup.is_null());
        let base = HewChildSpec {
            name: ptr::null(),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            dispatch: Some(noop_child_dispatch),
            sys_dispatch: None,
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: None,
            config: ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup, &raw const base) },
            0
        );
        unsafe { mark_running(sup) };

        let rendezvous = Arc::new(Barrier::new(2));
        let hook_gate = Arc::clone(&rendezvous);
        let _hook = install_dynamic_child_reserved_hook_for_test(Arc::new(move || {
            hook_gate.wait();
            hook_gate.wait();
        }));
        let sup_addr = sup as usize;
        let add = std::thread::spawn(move || {
            let sup = sup_addr as *mut HewSupervisor;
            let dynamic = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            unsafe { hew_supervisor_add_child_dynamic(sup, &raw const dynamic) }
        });

        // Dynamic add has atomically reserved slot 1 but has not spawned. Remove
        // slot 0, which swap-moves the reserved identity to slot 0, then restart
        // that exact current slot. The stale add continuation must not publish a
        // second actor into its former index 1.
        rendezvous.wait();
        assert_eq!(unsafe { hew_supervisor_remove_child(sup, 0) }, 0);
        let published = unsafe { restart_child_from_spec(sup, 0) };
        assert!(!published.is_null());
        rendezvous.wait();
        assert_eq!(add.join().expect("dynamic add thread panicked"), 1);

        let roster_guard = unsafe { &(*sup).roster }.lock_or_recover();
        let s = &*roster_guard;
        assert_eq!(s.child_count, 1);
        assert_eq!(s.children, vec![published]);
        drop(roster_guard);
        unsafe { hew_supervisor_stop(sup) };
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

    /// hew-lang/hew#2244 regression: before this fix, codegen truncated the
    /// index to i32 before this function ever saw it, so a caller-supplied
    /// index of `2^32 + k` silently wrapped to `k` and could alias an
    /// unrelated Live member instead of failing bounds-checking. This test
    /// exercises the *runtime* half of the fix directly: `index` is now a
    /// `u64` ABI param specifically so a huge, never-legitimately-in-range
    /// index is rejected as `Dead(UnknownSlot)` on its own, with no
    /// dependence on codegen not truncating it upstream (belt-and-suspenders
    /// with the codegen-side fix in `hew-codegen-rs/src/runtime_abi.rs`).
    #[test]
    fn pool_child_get_huge_index_returns_dead_unknown_slot_not_aliased_member() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        // Two members: PID 1001 at index 0, PID 2002 at index 1. If a huge
        // index silently wrapped instead of failing bounds-checking, these
        // are exactly the (wrong) Live handles it could alias.
        unsafe { hew_supervisor_pool_member_add(sup, 0, 1001) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 2002) };
        unsafe { mark_running(sup) };

        // 2^32 would wrap to 0 (aliasing PID 1001) under the old i32-truncating
        // ABI; 2^32 + 1 would wrap to 1 (aliasing PID 2002). Both must be
        // rejected as out of bounds now, never resolving to either handle.
        for huge_index in [1u64 << 32, (1u64 << 32) + 1, u64::MAX] {
            let r = unsafe { hew_supervisor_pool_child_get(sup, 0, huge_index) };
            assert_eq!(r.tag, 2, "index {huge_index} should be Dead, not Live");
            assert_eq!(r.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(r.handle.is_null());
            assert_ne!(
                r.handle as u64, 1001,
                "index {huge_index} must not alias member 0's PID"
            );
            assert_ne!(
                r.handle as u64, 2002,
                "index {huge_index} must not alias member 1's PID"
            );
        }

        // Sanity: the real in-range indices still resolve Live (the fix
        // widened the ABI/added a bounds check, it did not break ordinary
        // lookups).
        let r0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        let r1 = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert!(r0.is_live() && r0.handle as u64 == 1001);
        assert!(r1.is_live() && r1.handle as u64 == 2002);

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
            sys_dispatch: None,
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
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
        {
            let s = locked_roster!(sup);
            assert!(s.child_specs[0].state_template.allocation.state.is_null());
            assert_eq!(s.child_specs[0].state_template.allocation.size, 0);
            assert!(s.child_specs[0].init_fn.is_some());
            // The config buffer was adopted by the supervisor.
            assert_eq!(s.config_buf, cfg);
        }

        // The child actor holds the thunk-produced state with the config seed.
        unsafe {
            let child = locked_roster!(sup).children[0];
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
    fn production_crash_event_drops_fresh_init_fn_state() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) };
        assert!(!sup.is_null());
        let (cfg, cfg_size) = make_config_buf(257);
        let mut spec = init_closure_spec(cfg, cfg_size);
        spec.restart_policy = RESTART_TEMPORARY;
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup, 0, init_closure_drop) };
        let child = unsafe { locked_roster!(sup).children[0] };
        assert!(!child.is_null());
        assert!(
            !unsafe { (*child).state_drop_borrowed.load(Ordering::Acquire) },
            "init-thunk state is independently owned"
        );
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 1);

        // No crash escrow consumed this state: the production terminal-event
        // path must therefore perform its one exact final drop.
        unsafe {
            (*child)
                .actor_state
                .store(HewActorState::Crashed as i32, Ordering::Release);
            (*sup).running.store(1, Ordering::Release);
            let event = ChildEvent {
                child_index: 0,
                child_id: (*child).id,
                exit_state: HewActorState::Crashed as i32,
                crash_code: 0,
            };
            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                sup.cast::<c_void>(),
                HewSysMsg::ChildCrashed as i32,
                (&raw const event).cast_mut().cast::<c_void>(),
                std::mem::size_of::<ChildEvent>(),
            );
        }
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "an unconsumed init-thunk incarnation must be dropped by terminal-event teardown"
        );
        assert!(unsafe { locked_roster!(sup).children[0].is_null() });
        unsafe { hew_supervisor_stop(sup) };
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
        let new_child = unsafe { restart_child_from_spec(sup_ptr, 0) };
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
            let old = take_child_slot(sup_ptr, 0);
            actor::hew_actor_free(old);
        }
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);

        let breaker_state_before =
            unsafe { locked_roster!(sup_ptr).child_specs[0].circuit_breaker.state };
        INIT_CLOSURE_FAIL_NEXT.store(true, Ordering::SeqCst);

        // Restart with a failing thunk: fail closed (null new_child, null slot).
        let new_child = unsafe { restart_child_from_spec(sup_ptr, 0) };
        assert!(new_child.is_null(), "thunk OOM => fail closed (null child)");
        {
            let s = locked_roster!(sup_ptr);
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

        {
            // Both specs borrow the same single supervisor-owned buffer.
            let s = locked_roster!(sup_ptr);
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
            let r = unsafe { hew_supervisor_pool_child_get(sup, 0, u64::from(idx)) };
            assert!(r.is_live(), "member {idx} should be Live");
            let expected = unsafe { locked_roster!(sup).children[idx as usize] };
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
            let old = take_child_slot(&raw mut *sup, 1);
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

        let new_actor = unsafe { restart_child_from_spec(sup, 1) };
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
        assert_eq!(after.handle, unsafe { locked_roster!(sup).children[1] });

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
        let failed_identity = unsafe { (*sup).roster.lock_or_recover().child_specs[0].identity };
        // Crash member 0: free its slot, then drive the strategy arm (which now
        // routes SIMPLE_ONE_FOR_ONE → restart_child_from_spec for the member).
        unsafe {
            let old = take_child_slot(sup, 0);
            actor::hew_actor_free(old);
        }
        unsafe { restart_with_budget_and_strategy(sup, failed_identity) };

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
        assert_eq!(after.handle, unsafe { locked_roster!(sup).children[0] });

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }
}
