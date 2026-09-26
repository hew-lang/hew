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
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use crate::actor::{self, HewActor, HewActorOpts};
use crate::clock::hew_now_ms;
use crate::exit_status::{FaultRecord, FaultRuling, RoleKey};
use crate::internal::types::{
    HewActorState, HewDispatchFn, HewLifecycleFn, HewOnCrashFn, HewSysDispatchFn,
};
use crate::lifetime::live_actors::ActorIncarnation;
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

/// Append `(supervisor_label, child_label, restarts)` rows for one supervisor
/// and, recursively, every nested child supervisor beneath it.
///
/// `restarts` reads the same `CrashStats::total_crashes` lifetime counter
/// `append_supervisor_rows` formats into its debug tree row above (under the
/// same `roster.lock_or_recover()` discipline), not the bounded
/// `roster.restart_count` ring used only for the restart-budget window.
#[cfg(feature = "profiler")]
fn append_restart_rows(rows: &mut Vec<(String, String, u64)>, sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }

    // SAFETY: copy the self-actor pointer from the live allocation.
    let self_actor = unsafe { (*sup).self_actor };
    let self_actor_id = if self_actor.is_null() {
        0
    } else {
        // SAFETY: self_actor belongs to the live supervisor.
        unsafe { (*self_actor).id }
    };
    let supervisor_label = format!("supervisor:{self_actor_id}");

    // Snapshot restart counts and nested pins under this node's roster, then
    // release it before recursive descent — same discipline as
    // `append_supervisor_rows` above.
    let (child_rows, nested) = {
        // SAFETY: top-level supervisor pointers remain valid while registered;
        // nested calls carry a stable pin acquired by their parent snapshot.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        let child_rows = roster
            .children
            .iter()
            .take(roster.child_count)
            .enumerate()
            .map(|(index, _child)| {
                let spec = &roster.child_specs[index];
                let name = child_name(spec.name, &format!("child[{index}]"));
                let restarts: u64 = if spec.circuit_breaker.crash_stats.is_null() {
                    0
                } else {
                    // SAFETY: crash stats pointer belongs to the child spec.
                    u64::from(unsafe { (*spec.circuit_breaker.crash_stats).total_crashes })
                };
                (name, restarts)
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
        (child_rows, nested)
    };

    for (child, restarts) in child_rows {
        rows.push((supervisor_label.clone(), child, restarts));
    }
    for (child_sup, _pin) in nested {
        append_restart_rows(rows, child_sup);
    }
}

/// Snapshot the lifetime restart total of every child across every registered
/// supervisor tree, as `(supervisor_label, child_label, restarts)` rows.
///
/// Backs the `supervisor.restarts_by_child_total` observe series
/// ([`crate::observe::scrape_text`]); mirrors [`snapshot_tree_json`]'s walk,
/// but reads through the non-panicking
/// [`crate::shutdown::registered_supervisors_snapshot_opt`] rather than
/// [`crate::shutdown::registered_supervisors_snapshot`] — a scrape may run
/// before `hew_sched_init` installs a runtime or after teardown drops it,
/// where there is legitimately no supervisor tree yet, not a caller bug.
#[cfg(feature = "profiler")]
#[must_use]
pub fn restarts_by_child_snapshot() -> Vec<(String, String, u64)> {
    let roots = crate::shutdown::registered_supervisors_snapshot_opt();
    let mut rows = Vec::new();
    for root in roots {
        append_restart_rows(&mut rows, root);
    }
    rows
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
    /// A STATIC pool (`pool name: Type count: N`) registers its N members as
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
            unsafe { crate::mem::buf_free(self.name.cast::<c_void>()) }; // ALLOCATOR-PAIRING: GlobalAlloc
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
// return: Kill spends the role and Escalate transfers the failure to its parent.
// Restart continues through the child's static policy, budget and circuit
// breaker; it cannot override a temporary policy. A tag outside `0..=2` also
// follows that ordinary policy path.
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

/// C-unwind trap entry point invoked by generated code for a logical Hew
/// failure. It stamps the actor's typed exit reason and unwinds through LLVM
/// cleanup landing pads. This path is deliberately separate from hardware
/// signals, which are process-fatal.
///
/// # Safety
///
/// May be called in or out of actor dispatch. An uncaught top-level payload
/// reaches Rust's process panic boundary; actor dispatch catches it.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_trap_with_code(code: i32) {
    // SAFETY: forwarded caller contract; this bridge reports the code itself.
    unsafe { trap_with_code(code, false) }
}

/// The one native trap bridge. `reported` says the caller already wrote the
/// typed diagnostic line, which [`crate::fault::hew_fault_trap`] does so its
/// fault's message survives; this entry then reports nothing further.
///
/// # Safety
///
/// Same contract as [`hew_trap_with_code`].
pub(crate) unsafe fn trap_with_code(code: i32, reported: bool) {
    crate::cont::abort_if_crash_cleanup_finalizer_trap(trap_kind_name(code));
    let actor_stamped = crate::trap_code::stamp_current_actor_error_code(code);
    if actor_stamped && crate::execution_context::current_context_can_unwind() {
        std::panic::panic_any(crate::actor::HewPanic { code });
    }
    if !actor_stamped {
        // No actor owns this trap, so no supervisor can rule on it: it is an
        // unrecovered fault and the process reports `1` (HEW-SPEC-2026 5.8)
        // after its typed line. Terminating here rather than returning to the
        // generated `llvm.trap` is what keeps the private trap code out of the
        // process status. Buffered output is flushed first, as `exit()` does.
        if !reported {
            crate::fault::report_trap_code(code);
        }
        let _ = std::io::Write::flush(&mut std::io::stdout());
        let _ = std::io::Write::flush(&mut std::io::stderr());
        // JUSTIFIED: a trap with no recovery authority ends the run; the OS
        // reclaims what the skipped destructors would have released.
        std::process::exit(1);
    }
    // No runtime-owned catch boundary exists below this call. Generated trap
    // sites follow this bridge with their target trap instruction, which owns
    // process termination without asking Rust's unwinder to cross foreign or
    // process-entry frames. Synchronous lifecycle contexts arrive here with an
    // actor stamped but intentionally return for that same generated fallback.
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
    /// The crash record this armed timer owns. The restart's EFFECT when the
    /// timer fires is the ruling that settles it.
    fault_record: u64,
}

/// One incarnation a group restart stopped and waits for.
struct StoppedIncarnation {
    /// The stopped actor, freed once it finishes; null for a nested
    /// supervisor, whose own teardown frees it.
    actor: *mut HewActor,
    completion: Arc<crate::actor_native::NativeActorCompletion>,
}

/// A group restart waiting for the incarnations it stopped to finish their
/// stop hooks and cleanup. The roster holds at most one: a failure while it
/// waits joins it, so every role in the group restarts exactly once.
struct PendingGroupRestart {
    /// Each failure the restart answers, with the crash record it rules on.
    failures: Vec<(RestartChildRole, FaultRecord)>,
    stopping: Vec<StoppedIncarnation>,
    /// Registered with every completion in `stopping`; the registrations are
    /// weak, so this is what keeps them live.
    waker: Arc<crate::wake::OwnedWaker>,
}

impl PendingGroupRestart {
    fn new(sup: *mut HewSupervisor) -> Self {
        // SAFETY: the caller keeps `sup` live; the direct id is immutable.
        let supervisor = unsafe { (*sup).local_pid_id };
        Self {
            failures: Vec::new(),
            stopping: Vec::new(),
            waker: group_restart_waker(supervisor),
        }
    }

    fn finished(&self) -> bool {
        self.stopping
            .iter()
            .all(|stopped| stopped.completion.is_finished())
    }

    /// Free every stopped actor that has finished and keep only the ones
    /// still stopping.
    fn release_finished(&mut self) {
        self.stopping.retain(|stopped| {
            if !stopped.completion.is_finished() {
                return true;
            }
            if !stopped.actor.is_null() {
                // SAFETY: the group owns this detached, terminal incarnation.
                unsafe { actor::hew_actor_free(stopped.actor) };
            }
            false
        });
    }

    /// Give up on the restart: every record is unrecovered, and a stopped
    /// actor that has not finished yet is freed off this thread.
    fn abandon(mut self) {
        for (_, record) in self.failures.drain(..) {
            crate::exit_status::settle_supervised_fault(record, FaultRuling::Unrecovered);
        }
        self.release_finished();
        spawn_deferred_restart_free(
            self.stopping
                .drain(..)
                .filter(|stopped| !stopped.actor.is_null())
                .map(|stopped| DeferredFree(stopped.actor))
                .collect(),
        );
    }
}

/// A waker that signals [`HewSysMsg::GroupRestart`] to one supervisor through
/// its stable handle, so a completion never reaches a closed supervisor.
fn group_restart_waker(
    supervisor: crate::lifetime::local_handles::HewLocalPidId,
) -> Arc<crate::wake::OwnedWaker> {
    type Target = crate::lifetime::local_handles::HewLocalPidId;
    unsafe extern "C" fn group_restart_wake(context: *mut c_void) {
        // SAFETY: every descriptor holder retains this allocation.
        signal_group_restart(unsafe { *context.cast::<Target>() });
    }
    unsafe extern "C" fn group_restart_retain(context: *mut c_void) {
        // SAFETY: the descriptor contract requires a live Arc reference.
        unsafe { Arc::increment_strong_count(context.cast::<Target>()) };
    }
    unsafe extern "C" fn group_restart_release(context: *mut c_void) {
        // SAFETY: consumes exactly one reference acquired by retain.
        unsafe { Arc::decrement_strong_count(context.cast::<Target>()) };
    }
    let target = Arc::new(supervisor);
    let descriptor = crate::wake::HewWaker {
        context: Arc::as_ptr(&target).cast_mut().cast(),
        wake: group_restart_wake,
        retain: group_restart_retain,
        release: group_restart_release,
    };
    // SAFETY: `target` keeps the allocation live during retain.
    Arc::new(unsafe { crate::wake::OwnedWaker::retain(&descriptor) })
}

/// Ask a supervisor to look at its pending group restart again.
fn signal_group_restart(supervisor: crate::lifetime::local_handles::HewLocalPidId) {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(supervisor) else {
        return;
    };
    // SAFETY: the pin protects the self actor through mailbox admission.
    let self_actor = unsafe { (*pin.supervisor()).self_actor };
    if !self_actor.is_null() {
        // SAFETY: the pin retains the target; the signal carries no payload.
        unsafe {
            actor::send_system_message(self_actor, HewSysMsg::GroupRestart, ptr::null_mut(), 0);
        }
    }
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
    /// Without `init_fn`, registration copies only these wrapper bytes. Heap
    /// fields require clone registration or an explicit external-borrow contract
    /// through [`hew_supervisor_set_child_state_borrowed`]; copying the wrapper
    /// alone never transfers field ownership to the persistent template.
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
    /// The crash record opened at the crash site. This supervisor's ruling
    /// settles it, or hands it to the next authority. `0` for a graceful stop,
    /// which is not a fault.
    fault_record: u64,
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
    child_token: crate::lifetime::local_handles::HewLocalPidId,
    exit_state: c_int,
    crash_code: c_int,
    /// The escalated crash record. The escalation TRANSFERS ownership; this
    /// parent's ruling — not the send — settles it.
    fault_record: u64,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub(crate) struct ChildSupervisorStopped {
    supervisor_index: u32,
    child_token: crate::lifetime::local_handles::HewLocalPidId,
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
#[derive(Clone, Copy, Debug)]
enum SupervisorChildSpawn {
    Legacy(SupervisorInitFn),
    Native {
        spawn: HewNativeChildSpawnFn,
        config: *const c_void,
    },
}

#[derive(Debug)]
pub(crate) struct SupervisorChildSpec {
    spawn: SupervisorChildSpawn,
    identity: u64,
    restart_policy: c_int,
    spent: bool,
}

/// All mutable child, nested-supervisor, pool, restart-budget, and child-config
/// state. Keeping this data inside the mutex makes unlocked roster mutation
/// unrepresentable and prevents a whole-`HewSupervisor` reference from
/// aliasing concurrent raw roster updates.
pub(crate) struct SupervisorRoster {
    children: Vec<*mut HewActor>,
    child_specs: Vec<InternalChildSpec>,
    child_count: usize,
    next_child_spec_identity: u64,
    child_supervisors: Vec<*mut HewSupervisor>,
    child_supervisor_tokens: Vec<crate::lifetime::local_handles::HewLocalPidId>,
    child_supervisor_specs: Vec<Option<SupervisorChildSpec>>,
    retiring_children: Vec<Arc<crate::actor_native::NativeActorCompletion>>,
    pending_group_restart: Option<PendingGroupRestart>,
    restart_times: [u64; MAX_RESTARTS_TRACK],
    restart_count: usize,
    restart_head: usize,
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

    /// Completed restart cycles on this supervisor, and the Condvar every
    /// restart barrier sleeps on.
    ///
    /// `notify_restart` advances the epoch at the tail of a restart cycle.
    /// `wake_restart_waiters` re-fires the Condvar WITHOUT advancing it when a
    /// slot instead reaches a terminal state (supervisor cancellation, or a
    /// spec this supervisor declined to restart), so a barrier re-reads the
    /// slot as Dead and returns. Every reader holds the allocation live — the
    /// native entry through a `SupervisorPin`, tests by ownership — so the
    /// epoch is an inline field rather than an `Arc`.
    ///
    /// LOCK ORDER: acquired BEFORE `roster` and AFTER `restart_await_waiters`.
    /// Nothing may acquire it while holding `roster`.
    restart_epoch: (Mutex<u64>, Condvar),

    /// Parked `await_restart` continuations — the COOPERATIVE restart observer.
    ///
    /// Distinct from `restart_epoch` (the restart counter/Condvar the
    /// contextless blocking `await_restart` and test-support observers read).
    /// Each waiter is an actor that executed
    /// `await_restart sup.child` on a Transient slot and parked instead of
    /// thread-blocking the single cooperative scheduler. `notify_restart` fires
    /// every waiter (deposit readiness + `enqueue_resume`) after the restart
    /// cycle completes, so the resumed continuation re-resolves a Live slot
    /// (`notify_restart` runs AFTER `store_child_slot`). `wake_restart_waiters`
    /// fires them the same way on a terminal ruling, so a cancellation or a
    /// spent spec resumes a continuation into a Dead slot, which fails closed
    /// at the bind. Either way a resumed continuation re-resolves to Live or
    /// Dead, never Transient. Drained on fire and on supervisor teardown; a
    /// cancelled slot drops its wake (the channel-core race guard).
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
    /// One authority per admitted lease. Cancellation rules on every still-
    /// armed record before a lease can publish drain completion.
    records: Vec<Arc<RestartTimerRecord>>,
}

/// Arc-owned cancellation and raw-borrow authority for delayed restarts.
///
/// The state mutex deliberately covers the timer's final supervisor access.
/// Consequently, once [`Self::cancel`] returns, no timer can begin or continue
/// dereferencing the supervisor. The pending count is decremented only after
/// that access ends, giving teardown a precise lifetime barrier without making
/// a 30-second backoff an uninterruptible reclamation delay.
pub(crate) struct RestartTimerControl {
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

    fn begin(self: &Arc<Self>, record: FaultRecord) -> Option<RestartTimerLease> {
        let mut state = self.state.lock_or_recover();
        if state.cancelled {
            return None;
        }
        let record = Arc::new(RestartTimerRecord::new(record));
        state.pending += 1;
        state.records.push(Arc::clone(&record));
        Some(RestartTimerLease {
            control: Arc::clone(self),
            record,
        })
    }

    fn cancel(&self) {
        let mut state = self.state.lock_or_recover();
        state.cancelled = true;
        // Rule before waking timer threads: dropping a woken lease is what
        // publishes drain completion, so settlement must precede that drop.
        for record in &state.records {
            record.settle_cancelled();
        }
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
    record: Arc<RestartTimerRecord>,
}

const RESTART_TIMER_RECORD_ARMED: u8 = 0;
const RESTART_TIMER_RECORD_FIRED: u8 = 1;
const RESTART_TIMER_RECORD_SETTLED: u8 = 2;

struct RestartTimerRecord {
    record: FaultRecord,
    state: AtomicU8,
}

impl RestartTimerRecord {
    fn new(record: FaultRecord) -> Self {
        Self {
            record,
            state: AtomicU8::new(RESTART_TIMER_RECORD_ARMED),
        }
    }

    fn fire(&self) -> bool {
        self.state
            .compare_exchange(
                RESTART_TIMER_RECORD_ARMED,
                RESTART_TIMER_RECORD_FIRED,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
    }

    fn settle_cancelled(&self) {
        self.settle_from(RESTART_TIMER_RECORD_ARMED);
    }

    fn settle_failed_fire(&self) {
        self.settle_from(RESTART_TIMER_RECORD_FIRED);
    }

    fn settle_from(&self, expected: u8) {
        if self
            .state
            .compare_exchange(
                expected,
                RESTART_TIMER_RECORD_SETTLED,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            crate::exit_status::settle_supervised_fault(self.record, FaultRuling::Unrecovered);
        }
    }
}

impl RestartTimerLease {
    /// Wait interruptibly, then perform the raw supervisor access while the
    /// cancellation mutex excludes shutdown from publishing cancellation.
    ///
    /// Cancellation and expiry race for the armed record before this lease can
    /// publish drain completion. The winner either settles the record here or
    /// transfers it to the delayed-restart event.
    fn wait_and_run(&self, delay: Duration, on_elapsed: impl FnOnce() -> bool) {
        let deadline = Instant::now() + delay;
        let mut state = self.control.state.lock_or_recover();
        loop {
            if state.cancelled {
                break;
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                if self.record.fire() && !on_elapsed() {
                    self.record.settle_failed_fire();
                }
                break;
            }
            let (next, _) = self
                .control
                .changed
                .wait_timeout_or_recover(state, remaining);
            state = next;
        }
    }
}

impl Drop for RestartTimerLease {
    fn drop(&mut self) {
        let mut state = self.control.state.lock_or_recover();
        debug_assert!(state.pending > 0);
        state.pending = state.pending.saturating_sub(1);
        state
            .records
            .retain(|record| !Arc::ptr_eq(record, &self.record));
        debug_assert_eq!(state.pending, state.records.len());
        self.control.changed.notify_all();
        drop(state);
        run_restart_timer_released_hook_for_test();
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
static RESTART_TIMER_RELEASED_HOOK: std::sync::OnceLock<Mutex<SupervisorTestHook>> =
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

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_restart_timer_released_hook_for_test(
    hook: Arc<dyn Fn() + Send + Sync>,
) -> SupervisorTestHookGuard {
    *RESTART_TIMER_RELEASED_HOOK
        .get_or_init(|| Mutex::new(None))
        .lock_or_recover() = Some(hook);
    SupervisorTestHookGuard(&RESTART_TIMER_RELEASED_HOOK)
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

fn run_restart_timer_released_hook_for_test() {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if let Some(hook) = RESTART_TIMER_RELEASED_HOOK
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
    access.control.finish_terminal();
    // SAFETY: the runtime remains installed while supervisor reclamation runs.
    unsafe { &*access.handles }.remove_supervisor_control(&access.control);
}

/// One parked `await_restart` continuation: the awaiting actor + its readiness
/// slot. `notify_restart` fires every waiter exactly once per restart cycle,
/// depositing readiness into `slot` and re-enqueuing `actor` on the scheduler.
struct RestartAwaitWaiter {
    /// The parked-continuation actor's incarnation, woken via
    /// `enqueue_resume_by_incarnation`. Captured at park time, when the actor is
    /// live; a restart that recycles the awaiter's allocation cannot inherit
    /// this wake.
    actor: ActorIncarnation,
    /// The readiness slot; the observer holds one retained ref while registered.
    slot: *mut crate::read_slot::HewReadSlot,
}

// SAFETY: `actor` is a pair of scalars carrying no allocation; `slot` is
// reference-counted. The waiter is only moved between the supervisor's
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
    /// This supervisor ruled against restarting the role: budget exhausted, a
    /// `temporary`/`transient` policy decline, a tripped breaker, or an
    /// on-crash hook answering `Kill`. Mirrors `SupervisorChildSpec::spent` for
    /// nested roles, and makes an empty slot classify Dead instead of
    /// Transient. Set only by `mark_child_spec_spent`.
    spent: bool,
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
    /// The native declared-child incarnation source. When `Some`, the adapter
    /// re-runs the child's init arguments against `config` and publishes a
    /// complete actor itself, so this restart path registers no state, no
    /// dispatch and no lifecycle wrapper of its own.
    native_spawn: Option<HewNativeChildSpawnFn>,
}

/// Spawn one incarnation of a declared native child from the supervisor's
/// config. Returns its stable handle, or `INVALID` with the refusal in the
/// fault slot.
pub type HewNativeChildSpawnFn =
    unsafe extern "C-unwind" fn(
        *const c_void,
        *mut *mut crate::fault::HewFault,
    ) -> crate::lifetime::local_handles::HewLocalPidId;

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
        // SAFETY: every template wrapper is allocated by the sized-block
        // allocator or a clone callback whose contract requires sized-block
        // output.
        unsafe { crate::mem::buf_free(self.state) }; // ALLOCATOR-PAIRING: GlobalAlloc
        self.state = ptr::null_mut();
    }
}

/// Immutable template metadata published atomically under `roster`.
#[derive(Debug)]
struct ChildStateTemplate {
    /// Explicit unsafe caller contract: typed fields are externally owned and
    /// remain live for every alias, including deferred actor reclamation.
    /// Without this contract, a drop callback still requires clone/init.
    borrows_typed_fields: bool,
    allocation: Arc<ChildStateTemplateAllocation>,
    clone_fn: Option<actor::HewStateCloneFn>,
}

impl Drop for InternalChildSpec {
    fn drop(&mut self) {
        if !self.name.is_null() {
            // SAFETY: name was allocated with libc::strdup in
            // hew_supervisor_add_child_spec.
            unsafe { crate::mem::buf_free(self.name.cast::<c_void>()) }; // ALLOCATOR-PAIRING: GlobalAlloc
            self.name = ptr::null_mut();
        }
    }
}

impl Default for InternalChildSpec {
    fn default() -> Self {
        Self {
            identity: 0,
            revision: 1,
            spent: false,
            name: ptr::null_mut(),
            state_template: Arc::new(ChildStateTemplate {
                borrows_typed_fields: false,
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
            native_spawn: None,
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
#[derive(Clone, Copy, Debug)]
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

/// The declared roles one crashing child's fault is pending under.
///
/// The child occupies slot `child_index` on `sup`; `sup` may itself occupy a
/// declared nested slot on its parent, and so on to the root. Every one of
/// those roles is spellable as `await_restart`, and every one of them is still
/// unsettled while the fault is: an escalation hands the SAME record to the
/// parent, so a parent's nested role stays pending until the parent rules.
///
/// Reads only the stable back-pointer fields [`escalate_to_parent`] already
/// reads, so the crash path takes no roster lock to attribute a fault.
///
/// # Safety
///
/// `sup` must be live. Parent edges are published once at registration and the
/// parent outlives the child that names it.
pub(crate) unsafe fn child_role_chain(sup: *mut HewSupervisor, child_index: u32) -> Vec<RoleKey> {
    let mut roles = Vec::new();
    let mut owner = sup;
    let mut slot = child_index;
    let mut nested = false;
    loop {
        // SAFETY: the caller keeps the crashing child's supervisor live, and a
        // registered parent outlives the child holding its back-pointer.
        let (token, parent, index_in_parent) = unsafe {
            (
                (*owner).local_pid_id,
                (*owner).parent,
                (*owner).index_in_parent,
            )
        };
        roles.push(RoleKey {
            owner: token,
            slot,
            nested,
        });
        if parent.is_null() {
            return roles;
        }
        let Ok(parent_slot) = u32::try_from(index_in_parent) else {
            // The same un-representable index `escalate_to_parent` refuses. No
            // role above this point can be named, so the chain ends here.
            return roles;
        };
        owner = parent;
        slot = parent_slot;
        nested = true;
    }
}

/// Escalate a failure to the parent supervisor, transferring `record` to it.
///
/// Sends a [`HewSysMsg::ChildSupervisorEscalated`] signal carrying this
/// supervisor's index in the parent's `child_supervisors` vec. Its own typed
/// variant, not a `ChildCrashed` retagged by a negative index.
///
/// Returns whether the parent ACCEPTED the escalation. A null parent actor, an
/// un-representable index, or a refused send all mean the record reached no new
/// authority — the caller must settle it `Unrecovered` rather than treat the
/// attempt as a recovery.
///
/// # Safety
///
/// `sup.parent` must be non-null and point to a valid `HewSupervisor`.
fn escalate_to_parent(sup: *mut HewSupervisor, record: FaultRecord) -> bool {
    // SAFETY: caller guarantees the child and its non-null parent edge remain
    // live through this synchronous notification; copy only scalar fields.
    let (parent, index_in_parent) = unsafe { ((*sup).parent, (*sup).index_in_parent) };
    // SAFETY: caller guarantees parent is valid.
    let parent_actor = unsafe { (*parent).self_actor };
    if parent_actor.is_null() {
        return false;
    }
    let Ok(supervisor_index) = u32::try_from(index_in_parent) else {
        eprintln!(
            "[supervisor] refusing to escalate: child-supervisor index {index_in_parent} exceeds u32"
        );
        return false;
    };
    let event = ChildSupervisorEscalation {
        supervisor_index,
        // SAFETY: the child remains live throughout escalation.
        child_token: unsafe { (*sup).local_pid_id },
        fault_record: record.as_raw(),
        exit_state: HewActorState::Crashed as c_int,
        // Child-supervisor escalation: no single trap code applies to the
        // subtree-restart-budget exhaustion that triggered this escalation.
        // Use the honest unknown value rather than fabricating a signal number.
        crash_code: 0,
    };
    // SAFETY: caller keeps `sup` live while this scalar tracing id is copied.
    let sup_actor_id = supervisor_actor_id(sup);
    // SAFETY: parent.self_actor is valid.
    let accepted = unsafe {
        actor::send_system_message(
            parent_actor,
            HewSysMsg::ChildSupervisorEscalated,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildSupervisorEscalation>(),
        )
    };

    // Observability (read-only side effect, AFTER the escalation decision and
    // dispatch): record that this supervisor escalated to its parent. Never
    // gates control flow.
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_ESCALATE,
        i32::try_from(index_in_parent).unwrap_or(i32::MAX),
    );
    accepted
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

/// Arm a delayed restart, transferring `record` to the timer.
///
/// Returns whether the timer was ARMED — admitted AND its thread spawned. A
/// refused admission or a failed spawn means no restart will ever be attempted,
/// so the caller settles the record `Unrecovered`: requesting a timer is not a
/// recovery, an armed one that fires is.
///
/// Once armed, the timer thread owns the record and settles it on every ending
/// it can have: a restart that runs rules through the `DelayedRestart` dispatch,
/// while a wake-up that reaches nobody and a cancellation that preempts the
/// wait both settle `Unrecovered` here.
fn schedule_delayed_restart(
    sup: *mut HewSupervisor,
    child_identity: u64,
    delay: Duration,
    record: FaultRecord,
) -> bool {
    // Test-only: refuse the arm before any lease is taken, which is the state a
    // failed thread spawn leaves behind (the failed builder drops the closure
    // and its lease). Process-global rather than thread-local because this runs
    // on whichever scheduler worker dispatched the crash, not on the test's
    // thread.
    #[cfg(all(test, not(target_arch = "wasm32")))]
    if should_fail_delayed_restart_arm() {
        return false;
    }
    // SAFETY: caller keeps `sup` live through timer admission; the returned
    // lease extends raw-pointer lifetime through the spawned closure.
    let Some(timer) = (unsafe { &(*sup).restart_timers }).begin(record) else {
        return false;
    };
    let sup_addr = sup as usize;
    // WHY: the restart timer is a host thread on the real clock; the
    // single-thread driver waits for it rather than reporting a deadlock.
    // WHEN obsolete: restart timers move onto the driver's timer wheel.
    let host_work = crate::driver::active().then(crate::driver::HostWork::begin);
    let spawn_result = std::thread::Builder::new()
        .name("hew-supervisor-restart-timer".to_owned())
        .spawn(move || {
            let _host_work = host_work;
            timer.wait_and_run(delay, || {
                let sup_ptr = sup_addr as *mut HewSupervisor;
                // SAFETY: the timer lease's pending count keeps `sup_ptr`
                // allocated, and its state mutex excludes cancellation for the
                // complete raw access. Once cancellation is published this
                // closure can never run.
                unsafe {
                    let self_actor = (*sup_ptr).self_actor;
                    if (*sup_ptr).cancelled.load(Ordering::Acquire)
                        || (*sup_ptr).running.load(Ordering::Acquire) == 0
                        || self_actor.is_null()
                    {
                        return false;
                    }
                    let event = DelayedRestartEvent {
                        child_identity,
                        fault_record: record.as_raw(),
                    };
                    actor::send_system_message(
                        self_actor,
                        HewSysMsg::DelayedRestart,
                        (&raw const event).cast::<c_void>().cast_mut(),
                        std::mem::size_of::<DelayedRestartEvent>(),
                    )
                }
            });
        });
    if let Err(error) = spawn_result {
        // The failed builder drops the closure and therefore its timer lease,
        // restoring the pending count before returning.
        set_last_error(format!(
            "failed to spawn delayed supervisor restart timer: {error}"
        ));
        return false;
    }
    true
}

/// Advance the restart epoch and wake every restart waiter.
///
/// Three wake paths fire here, all AFTER the restart cycle's `store_child_slot`
/// has made the new child reachable (this function is called at the tail of
/// `restart_with_budget_and_strategy` / `restart_child_supervisor_with_budget`):
///
/// 1. The `restart_epoch` counter/Condvar — the bump + `notify_all`, read by
///    `hew_supervisor_get_child_wait` and by test-support code.
/// 2. The supervision generation, which is what the contextless blocking
///    `await_restart` (`hew_supervisor_restart_await_blocking`) waits on. The
///    restart changed a slot without touching a fault record, so a barrier
///    parked on that slot has to be told.
/// 3. The COOPERATIVE `await_restart` observers — every parked continuation in
///    `restart_await_waiters` gets readiness deposited + `enqueue_resume`, then
///    the registry is drained. A resumed continuation re-resolves the slot and
///    is guaranteed Live (the store-before-notify ordering is the resume-contract
///    anchor).
///
/// ORDERING INVARIANT (lost-wakeup guard): the epoch bump MUST happen before
/// `wake_restart_await_waiters` acquires `restart_await_waiters`. A racing
/// `hew_supervisor_restart_await_suspend` re-reads the epoch while holding
/// `restart_await_waiters`; bumping first means that if this drain ran against an
/// empty registry (the waiter not yet pushed), the awaiting actor's under-lock
/// recheck observes the advanced epoch and resolves READY instead of parking
/// against a wake that already fired. Do not reorder the bump after the drain.
/// The epoch guard is released before the drain so the two locks never nest.
fn notify_restart(sup: *mut HewSupervisor) {
    {
        // SAFETY: callers keep `sup` live through this inline-field access.
        let (lock, cv) = unsafe { &(*sup).restart_epoch };
        let mut count = lock.lock_or_recover();
        *count += 1;
        cv.notify_all();
    }
    crate::exit_status::note_supervision_change();
    wake_restart_await_waiters(sup);
}

/// Re-fire every restart waiter WITHOUT advancing the epoch.
///
/// A restart barrier also has to be released when the slot it waits on reaches
/// a TERMINAL state instead of being restarted: the supervisor was cancelled or
/// ran out of budget, or the fault ruling declined to restart this spec. The
/// state transition is published before this call, so a woken barrier re-reads
/// the slot as Dead and returns. The epoch must NOT move: it counts completed
/// restart cycles, which is what `test_wait_for_restart` and
/// `hew_supervisor_get_child_wait` read. The supervision generation does move,
/// because a barrier's answer may have changed.
///
/// Same ordering as `notify_restart`: `notify_all` under the epoch mutex (a
/// bare notify could land between a waiter's slot read and its `wait` and be
/// lost), then release it before draining `restart_await_waiters`.
fn wake_restart_waiters(sup: *mut HewSupervisor) {
    {
        // SAFETY: callers keep `sup` live through this inline-field access.
        let (lock, cv) = unsafe { &(*sup).restart_epoch };
        let _epoch = lock.lock_or_recover();
        cv.notify_all();
    }
    crate::exit_status::note_supervision_change();
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
            crate::scheduler::enqueue_resume_by_incarnation(waiter.actor);
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

/// Stop this supervisor, notify waiters, and escalate `record` to the parent if
/// there is one.
///
/// Every caller reaches here because this supervisor could NOT recover a child
/// fault: restart budget exhausted, the child was not restartable, or the
/// restart itself returned no child.
///
/// The returned ruling distinguishes the two outcomes that look alike from
/// here. With a parent that ACCEPTS the escalation, recovery authority moves up
/// the tree and the record is provisional — the parent's ruling settles it. A
/// parent that is stopped, closed, or absent accepted nothing, so the record
/// reached no new authority and is `Unrecovered`. A non-null parent POINTER is
/// not a recovery; a delivered escalation is a transfer.
fn stop_and_maybe_escalate(sup: *mut HewSupervisor, record: FaultRecord) -> FaultRuling {
    // SAFETY: callers keep the allocation live through cancellation.
    publish_supervisor_cancellation(sup);
    // SAFETY: callers keep the allocation live through these field operations.
    unsafe { (*sup).running.store(0, Ordering::Release) };
    notify_restart(sup);
    // SAFETY: callers keep the supervisor allocation live through escalation.
    if unsafe { (*sup).parent.is_null() } {
        return FaultRuling::Unrecovered;
    }
    if escalate_to_parent(sup, record) {
        FaultRuling::Escalated
    } else {
        FaultRuling::Unrecovered
    }
}

fn stop_owned_deferred_supervisor(
    deferred: DeferredSupervisorStop,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
) {
    // SAFETY: the claimed supervisor and its owning runtime remain live until
    // this teardown lease is released; leave that runtime before releasing it.
    let context = unsafe { crate::runtime::enter(&*(*deferred.0).runtime) };
    // SAFETY: teardown ownership was claimed by the caller before this thread
    // was spawned, so this background thread is the unique destructor.
    unsafe { stop_supervisor_owned(deferred.0, &teardown) };
    drop(context);
    drop(teardown);
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

fn retain_nested_completion(
    sup: *mut HewSupervisor,
    token: crate::lifetime::local_handles::HewLocalPidId,
) {
    if let Some(completion) = crate::lifetime::local_handles::current_supervisor_completion(token) {
        retain_child_completion(sup, completion);
    }
}

fn retain_child_completion(
    sup: *mut HewSupervisor,
    completion: Arc<crate::actor_native::NativeActorCompletion>,
) {
    // SAFETY: the caller owns a live parent throughout its roster update.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    roster
        .retiring_children
        .retain(|pending| !pending.is_finished());
    if !completion.is_finished()
        && !roster
            .retiring_children
            .iter()
            .any(|pending| Arc::ptr_eq(pending, &completion))
    {
        roster.retiring_children.push(completion);
    }
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

/// Test-only: make the next delayed-restart arm refuse, as a failed timer
/// thread spawn does. Process-global, unlike the thread-local hooks above: the
/// arm runs on the scheduler worker that dispatched the crash.
#[cfg(all(test, not(target_arch = "wasm32")))]
static FAIL_DELAYED_RESTART_ARM: AtomicBool = AtomicBool::new(false);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn should_fail_delayed_restart_arm() -> bool {
    FAIL_DELAYED_RESTART_ARM.load(Ordering::Acquire)
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

mod child;
mod crash;
mod lifecycle;
mod pool;
mod restart;
mod role;
mod teardown;

pub use child::*;
pub(crate) use crash::*;
pub use lifecycle::*;
pub use pool::*;
pub(crate) use restart::*;
pub use role::*;
pub(crate) use teardown::*;
