//! Supervisor tree for Hew actors.
//!
//! Implements event-driven supervision with three restart strategies
//! (one-for-one, one-for-all, rest-for-one) and sliding-window restart
//! tracking. Mirrors the C implementation in `hew-codegen/runtime/src/supervisor.c`.

use std::ffi::{c_char, c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize, Ordering};

use crate::actor::{self, HewActor, HewActorOpts};
use crate::internal::types::{HewActorState, HewDispatchFn, HewOverflowPolicy};
use crate::io_time::hew_now_ms;
use crate::mailbox;
use crate::scheduler;

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

/// System message types for supervisor events.
const SYS_MSG_CHILD_STOPPED: i32 = 100;
const SYS_MSG_CHILD_CRASHED: i32 = 101;
const SYS_MSG_SUPERVISOR_STOP: i32 = 102;

/// Link propagation system message (when linked actor crashes).
pub const SYS_MSG_EXIT: i32 = 103;
/// Monitor notification system message (when monitored actor dies).
pub const SYS_MSG_DOWN: i32 = 104;

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
#[allow(dead_code)]
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
    /// Init specs for child supervisors (parallel to `child_supervisors` vec).
    child_supervisor_specs: Vec<SupervisorChildSpec>,

    restart_times: [u64; MAX_RESTARTS_TRACK],
    restart_count: usize,
    restart_head: usize,

    running: AtomicI32,
    cancelled: AtomicBool,
    pending_restart_timers: AtomicUsize,
    self_actor: *mut HewActor,

    /// Parent supervisor (set by `hew_supervisor_add_child_supervisor`).
    parent: *mut HewSupervisor,
    /// Index of this supervisor in parent's `child_supervisors` vec.
    index_in_parent: usize,
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
#[expect(
    clippy::cast_possible_wrap,
    reason = "index_in_parent fits in u64 for any reasonable supervisor tree"
)]
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

/// Restart a child from its spec, returning the new actor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_count` (for
/// restarts) or equal to `child_count` (for initial spawns, where the
/// caller is responsible for pushing the result onto the `children` vec).
unsafe fn restart_child_from_spec(sup: &mut HewSupervisor, index: usize) -> *mut HewActor {
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

        // Record successful restart for circuit breaker
        circuit_breaker_record_success(&mut sup.child_specs[index]);
    }

    // Update existing slot (restarts). For initial spawns, the caller
    // pushes the returned pointer onto the children vec.
    if index < sup.children.len() {
        sup.children[index] = new_child;
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
        sup.running.store(0, Ordering::Release);
        if !sup.parent.is_null() {
            escalate_to_parent(sup);
        }
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
                if i != failed_index && !sup.children[i].is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(sup.children[i]) };
                    deferred.push(DeferredFree(sup.children[i]));
                    sup.children[i] = ptr::null_mut();
                }
            }
            if !deferred.is_empty()
                && std::thread::Builder::new()
                    .name("deferred-free".into())
                    .spawn(move || {
                        for d in deferred {
                            // SAFETY: actor was stopped; supervisor no longer references it.
                            unsafe { actor::hew_actor_free(d.0) };
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
                if !sup.children[i].is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(sup.children[i]) };
                    deferred.push(DeferredFree(sup.children[i]));
                    sup.children[i] = ptr::null_mut();
                }
            }
            if !deferred.is_empty()
                && std::thread::Builder::new()
                    .name("deferred-free".into())
                    .spawn(move || {
                        for d in deferred {
                            // SAFETY: actor was stopped; supervisor no longer references it.
                            unsafe { actor::hew_actor_free(d.0) };
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
}

/// Apply the restart strategy after a child failure.
///
/// # Safety
///
/// `sup` must be valid.
#[expect(
    clippy::too_many_lines,
    reason = "restart strategies (one-for-one/all/rest) need inline logic"
)]
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
        sup.children[failed_index] = ptr::null_mut();
        return;
    }
    if spec.restart_policy == RESTART_TRANSIENT && exit_state == HewActorState::Stopped as c_int {
        sup.children[failed_index] = ptr::null_mut();
        return;
    }

    // Check circuit breaker
    if !circuit_breaker_should_restart(spec) {
        sup.children[failed_index] = ptr::null_mut();
        return;
    }

    // Check restart delay — if backoff delay hasn't elapsed, schedule a
    // delayed restart by spawning a timer thread. Don't abandon the child.
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
            // SAFETY: stop waits for pending timers before dropping supervisor.
            let sup_ptr = sup_addr as *mut HewSupervisor;
            unsafe {
                let s = &mut *sup_ptr;
                if !s.cancelled.load(Ordering::Acquire) && s.running.load(Ordering::Acquire) != 0 {
                    restart_with_budget_and_strategy(s, idx);
                }
                s.pending_restart_timers.fetch_sub(1, Ordering::AcqRel);
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
                sup.running.store(0, Ordering::Release);
                return;
            }

            #[expect(clippy::cast_sign_loss, reason = "child_index is non-negative")]
            let idx = event.child_index as usize;
            if idx >= sup.child_count {
                return;
            }

            // Free the old child.
            if !sup.children[idx].is_null() {
                // SAFETY: child pointer is valid.
                unsafe { actor::hew_actor_free(sup.children[idx]) };
                sup.children[idx] = ptr::null_mut();
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
        pending_restart_timers: AtomicUsize::new(0),
        self_actor: ptr::null_mut(),
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
    if sup.is_null() || spec.is_null() {
        return -1;
    }
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
    });

    // Spawn the child actor.
    // SAFETY: spec is valid.
    let spawned = unsafe { restart_child_from_spec(s, i) };
    s.children.push(spawned);
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
    if sup.is_null() {
        return;
    }
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

    // Unregister from shutdown list to prevent double-stop.
    // SAFETY: sup is still valid here.
    unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };

    // SAFETY: caller guarantees sup is valid and surrenders ownership.
    let mut s = unsafe { Box::from_raw(sup) };

    s.cancelled.store(true, Ordering::Release);
    s.running.store(0, Ordering::Release);
    while s.pending_restart_timers.load(Ordering::Acquire) != 0 {
        std::thread::yield_now();
    }

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
        if !s.children[i].is_null() {
            // SAFETY: child pointer is valid.
            unsafe { actor::hew_actor_stop(s.children[i]) };
            // Spin-wait until actor is no longer Running or Runnable.
            // SAFETY: child pointer is valid.
            unsafe {
                loop {
                    let state = (*s.children[i]).actor_state.load(Ordering::Acquire);
                    if state != HewActorState::Running as i32
                        && state != HewActorState::Runnable as i32
                    {
                        break;
                    }
                    std::thread::yield_now();
                }
                actor::hew_actor_free(s.children[i]);
            }
            s.children[i] = ptr::null_mut();
        }
    }

    // Stop the supervisor actor and wait for it to finish.
    if !s.self_actor.is_null() {
        // SAFETY: self_actor is valid.
        unsafe {
            actor::hew_actor_stop(s.self_actor);
            // Spin-wait until self_actor reaches a terminal state.
            loop {
                let state = (*s.self_actor).actor_state.load(Ordering::Acquire);
                if state != HewActorState::Running as i32 && state != HewActorState::Runnable as i32
                {
                    break;
                }
                std::thread::yield_now();
            }
            // Null the state before freeing — it points to the supervisor
            // struct, not a deep copy.
            (*s.self_actor).state = ptr::null_mut();
            (*s.self_actor).state_size = 0;
            actor::hew_actor_free(s.self_actor);
        }
        s.self_actor = ptr::null_mut();
    }

    // Child spec resources (init_state, name) are freed by the Drop impl
    // on InternalChildSpec when `s` (the Box) drops here.
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
    if sup.is_null() || child.is_null() {
        return;
    }
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
    p.child_supervisors.push(child);
    // Set parent back-pointer on the child supervisor.
    // SAFETY: caller guarantees child is valid.
    unsafe {
        (*child).parent = parent;
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
        .push(SupervisorChildSpec { init_fn });
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
    s.children[i]
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
        return 0;
    }
    // SAFETY: caller guarantees sup is valid.
    let s = unsafe { &*sup };
    (s.child_count + s.child_supervisors.len()) as c_int
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
    if sup.is_null() || spec.is_null() {
        return -1;
    }
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
    });

    // Spawn the child if the supervisor is running.
    let spawned = if s.running.load(Ordering::Acquire) != 0 {
        // SAFETY: spec is valid.
        unsafe { restart_child_from_spec(s, i) }
    } else {
        ptr::null_mut()
    };
    s.children.push(spawned);
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
    }
    if !spec.name.is_null() {
        // SAFETY: name was allocated with libc::strdup.
        unsafe { libc::free(spec.name.cast::<c_void>()) };
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
