//! Hew runtime: actor struct definition and state constants.
//!
//! Defines the [`HewActor`] struct layout for C ABI compatibility and the
//! actor state machine constants. The full actor API (spawn, send, activate)
//! will be implemented in a future iteration.

#[cfg(not(target_arch = "wasm32"))]
use std::cell::Cell;
use std::collections::HashSet;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicI32, AtomicPtr, AtomicU64, Ordering};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::Mutex;

use crate::internal::types::HewActorState;
use crate::internal::types::HewOverflowPolicy;
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

/// Wrapper so `*mut HewActor` can be stored in a `HashSet`.
#[derive(Debug, PartialEq, Eq, Hash)]
struct ActorPtr(*mut HewActor);

// SAFETY: Actor pointers are managed by the runtime and only freed
// under controlled conditions (shutdown or explicit free).
unsafe impl Send for ActorPtr {}

/// Set of all live (not-yet-freed) actor pointers.
#[cfg(not(target_arch = "wasm32"))]
static LIVE_ACTORS: Mutex<Option<HashSet<ActorPtr>>> = Mutex::new(None);

#[cfg(target_arch = "wasm32")]
// SAFETY: WASM is single-threaded. Using a static mut with manual access control.
static mut LIVE_ACTORS_WASM: Option<HashSet<ActorPtr>> = None;

/// Register an actor in the live tracking set.
#[cfg(not(target_arch = "wasm32"))]
fn track_actor(actor: *mut HewActor) {
    if let Ok(mut guard) = LIVE_ACTORS.lock() {
        guard
            .get_or_insert_with(HashSet::new)
            .insert(ActorPtr(actor));
    }
}

/// Register an actor in the live tracking set.
#[cfg(target_arch = "wasm32")]
fn track_actor(actor: *mut HewActor) {
    // SAFETY: WASM is single-threaded, no data races possible.
    unsafe {
        LIVE_ACTORS_WASM
            .get_or_insert_with(HashSet::new)
            .insert(ActorPtr(actor));
    }
}

/// Remove an actor from the live tracking set.
///
/// Returns `true` if the actor was present and removed, `false` if it
/// was not found (e.g. already consumed by [`cleanup_all_actors`]).
#[cfg(not(target_arch = "wasm32"))]
fn untrack_actor(actor: *mut HewActor) -> bool {
    if let Ok(mut guard) = LIVE_ACTORS.lock() {
        if let Some(set) = guard.as_mut() {
            return set.remove(&ActorPtr(actor));
        }
    }
    false
}

/// Remove an actor from the live tracking set.
///
/// Returns `true` if the actor was present and removed, `false` if it
/// was not found (e.g. already consumed by [`cleanup_all_actors`]).
#[cfg(target_arch = "wasm32")]
fn untrack_actor(actor: *mut HewActor) -> bool {
    // SAFETY: WASM is single-threaded, no data races possible.
    unsafe {
        if let Some(set) = LIVE_ACTORS_WASM.as_mut() {
            return set.remove(&ActorPtr(actor));
        }
    }
    false
}

/// Free all remaining tracked actors. Called during scheduler shutdown
/// after all worker threads have been joined.
///
/// # Safety
///
/// Must only be called after all worker threads have stopped — no
/// concurrent dispatch can be in progress.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn cleanup_all_actors() {
    let actors = {
        let mut guard = match LIVE_ACTORS.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        match guard.as_mut() {
            Some(set) => std::mem::take(set),
            None => HashSet::new(),
        }
    };

    for ActorPtr(actor) in actors {
        if actor.is_null() {
            continue;
        }
        // SAFETY: All workers have been joined, so no actor can be Running or Runnable.
        // SAFETY: The actor was allocated by a spawn function and has not been freed yet.
        unsafe { free_actor_resources(actor) };
    }
}

/// Free all remaining tracked actors. Called during WASM scheduler
/// shutdown.
///
/// # Safety
///
/// Must only be called when no dispatch is in progress.
#[cfg(target_arch = "wasm32")]
pub(crate) unsafe fn cleanup_all_actors() {
    // SAFETY: WASM is single-threaded, no data races possible.
    let actors = unsafe {
        match LIVE_ACTORS_WASM.as_mut() {
            Some(set) => std::mem::take(set),
            None => HashSet::new(),
        }
    };

    for ActorPtr(actor) in actors {
        if actor.is_null() {
            continue;
        }
        // SAFETY: No dispatch is in progress.
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
    if !a.mailbox.is_null() {
        extern "C" {
            fn hew_mailbox_free(mb: *mut c_void);
        }
        // SAFETY: Mailbox was allocated by hew_mailbox_new.
        unsafe { hew_mailbox_free(a.mailbox) };
    }

    // SAFETY: Actor was allocated with Box::new / Box::into_raw.
    drop(unsafe { Box::from_raw(actor) });
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
    // SAFETY: Caller guarantees `state` validity (second copy for restart).
    let init_state = unsafe { deep_copy_state(state, state_size) };

    // SAFETY: hew_mailbox_new returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new() };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: actor_id,
        pid: actor_id, // unified: pid == id (location-transparent)
        state: actor_state,
        state_size,
        dispatch,
        mailbox: mailbox.cast(),
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(HEW_MSG_BUDGET),
        init_state,
        init_state_size: state_size,
        coalesce_key_fn: None,
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
    raw
}

/// Spawn a new actor from a [`HewActorOpts`] struct.
///
/// Uses a bounded mailbox if `opts.mailbox_capacity > 0`.
///
/// # Safety
///
/// - `opts` must be a valid pointer to a [`HewActorOpts`].
/// - Same state/dispatch requirements as [`hew_actor_spawn`].
///
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
    // SAFETY: Caller guarantees state validity (second copy for restart).
    let init_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };

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

    let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: actor_id,
        pid: actor_id, // unified: pid == id (location-transparent)
        state: actor_state,
        state_size: opts.state_size,
        dispatch: opts.dispatch,
        mailbox: mailbox.cast(),
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(budget),
        init_state,
        init_state_size: opts.state_size,
        coalesce_key_fn: opts.coalesce_key_fn,
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
    raw
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
    // SAFETY: Caller guarantees `state` validity (second copy for restart).
    let init_state = unsafe { deep_copy_state(state, state_size) };

    // SAFETY: Returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new_bounded(capacity) };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: actor_id,
        pid: actor_id, // unified: pid == id
        state: actor_state,
        state_size,
        dispatch,
        mailbox: mailbox.cast(),
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(HEW_MSG_BUDGET),
        init_state,
        init_state_size: state_size,
        coalesce_key_fn: None,
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
    raw
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
        let guard = match LIVE_ACTORS.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        guard.as_ref().is_some_and(|set| {
            set.iter().any(|ptr| {
                let actor = ptr.0;
                if actor.is_null() {
                    return false;
                }
                // SAFETY: `actor` pointers in LIVE_ACTORS originate from spawn
                // functions and are removed on free.
                let matches = unsafe { (&*actor).id == actor_id };
                if matches {
                    // SAFETY: actor pointer was discovered while LIVE_ACTORS is
                    // locked, so it cannot be concurrently untracked/freed
                    // during this send.
                    unsafe { actor_send_internal(actor, msg_type, data, size) };
                    true
                } else {
                    false
                }
            })
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
    let _ = a.actor_state.compare_exchange(
        HewActorState::Idle as i32,
        HewActorState::Stopped as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
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
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }

    // Remove from live tracking. If the actor was already consumed by
    // cleanup_all_actors (returns false), skip freeing to avoid
    // double-free.
    if !untrack_actor(actor) {
        return;
    }

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Wait until actor reaches a terminal state.
    loop {
        let state = a.actor_state.load(Ordering::Acquire);
        if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
            break;
        }
        std::thread::yield_now();
    }

    // SAFETY: Caller guarantees `actor` is valid and not being dispatched.
    unsafe { free_actor_resources(actor) };
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
    if actor.is_null() {
        return;
    }
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
    if actor.is_null() {
        return 0;
    }
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
    if actor.is_null() {
        return;
    }
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
    if actor.is_null() {
        return;
    }
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
    if actor.is_null() {
        return HEW_PRIORITY_NORMAL;
    }
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.priority.load(Ordering::Relaxed)
}

// ── Internal send helper ────────────────────────────────────────────────

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
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Check for injected drop fault (testing only). Silently discard
    // the message without enqueuing it.
    if crate::deterministic::check_drop_fault(a.id) {
        return true; // Pretend success.
    }

    let mb = a.mailbox.cast::<HewMailbox>();

    // SAFETY: Mailbox is valid for the actor's lifetime.
    let result = unsafe { mailbox::hew_mailbox_send(mb, msg_type, data, size) };
    if result != 0 {
        return false;
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
        // Clear hibernation state — the actor has work to do.
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        scheduler::sched_enqueue(actor);
    }

    true
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

    // SAFETY: actor is valid, packed data is valid.
    let sent = unsafe { actor_send_internal(actor, msg_type, packed, total) };
    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };

    if !sent {
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

    // SAFETY: actor is valid, packed data is valid.
    let sent = unsafe { actor_send_internal(actor, msg_type, packed, total) };
    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };

    if !sent {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        return std::ptr::null_mut();
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait_timeout(ch, timeout_ms) };

    if result.is_null() {
        // Timeout: mark the channel as cancelled so the late replier
        // handles cleanup instead of us freeing it (which would be UAF).
        // SAFETY: ch is valid; the actor holding the channel pointer will
        // SAFETY: check this flag in hew_reply and free the channel at that point.
        unsafe { (*ch).cancelled.store(true, Ordering::Release) };
    } else {
        // Got a reply — we own the channel and can free it.
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
) {
    let ptr_size = std::mem::size_of::<*mut c_void>();
    let Some(total) = size.checked_add(ptr_size) else {
        return;
    };

    // SAFETY: malloc for packed buffer.
    let packed = unsafe { libc::malloc(total) };
    if packed.is_null() {
        return;
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

    // SAFETY: actor is valid, packed data is valid.
    unsafe { actor_send_internal(actor, msg_type, packed, total) };
    // SAFETY: packed was malloc'd above.
    unsafe { libc::free(packed) };
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
    if actor.is_null() {
        return;
    }
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
    if actor.is_null() {
        return 0;
    }
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

/// Deliberately crash the current actor by writing to null.
///
/// The crash signal handler catches the SIGSEGV, marks the actor as
/// `Crashed`, and longjmps back to the scheduler. The supervisor (if
/// any) will restart the actor according to its restart strategy.
///
/// This function never returns.
#[no_mangle]
pub extern "C" fn hew_panic() {
    // Use write_volatile to prevent LLVM from optimizing the null write
    // into a trap instruction.
    //
    // SAFETY: Intentional null dereference to trigger SIGSEGV, which the
    // SAFETY: crash signal handler catches and recovers from via siglongjmp.
    unsafe {
        core::ptr::write_volatile(core::ptr::null_mut::<i32>(), 0xDEAD_i32);
    }
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
    // SAFETY: Caller guarantees `state` validity (second copy for restart).
    let init_state = unsafe { deep_copy_state(state, state_size) };
    // SAFETY: hew_mailbox_new is a trusted FFI constructor returning a valid mailbox pointer.
    let mailbox = unsafe { hew_mailbox_new() };

    let serial = NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed);
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: serial,
        pid: serial, // unified: pid == id,
        state: actor_state,
        state_size,
        dispatch,
        mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(HEW_MSG_BUDGET),
        init_state,
        init_state_size: state_size,
        coalesce_key_fn: None,
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
    // SAFETY: Caller guarantees `state` validity (second copy for restart).
    let init_state = unsafe { deep_copy_state(state, state_size) };
    // SAFETY: hew_mailbox_new_bounded is a trusted FFI constructor returning a valid mailbox pointer.
    let mailbox = unsafe { hew_mailbox_new_bounded(capacity) };

    let serial = NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed);
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: serial,
        pid: serial, // unified: pid == id,
        state: actor_state,
        state_size,
        dispatch,
        mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(HEW_MSG_BUDGET),
        init_state,
        init_state_size: state_size,
        coalesce_key_fn: None,
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
    // SAFETY: Same as above (second copy for restart).
    let init_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };

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

    let serial = NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed);
    let actor = Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: serial,
        pid: serial, // unified: pid == id,
        state: actor_state,
        state_size: opts.state_size,
        dispatch: opts.dispatch,
        mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(budget),
        init_state,
        init_state_size: opts.state_size,
        coalesce_key_fn: opts.coalesce_key_fn,
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

    // Send the packed message.
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // SAFETY: a.mailbox is a valid mailbox pointer.
    unsafe { hew_mailbox_send(a.mailbox, msg_type, packed, total) };
    // SAFETY: packed buffer ownership transferred to mailbox (deep-copied).
    unsafe { libc::free(packed) };

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
    let _ = a.actor_state.compare_exchange(
        HewActorState::Idle as i32,
        HewActorState::Stopped as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
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
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }

    if !untrack_actor(actor) {
        return;
    }

    // SAFETY: Caller guarantees `actor` is valid and not being dispatched.
    unsafe { free_actor_resources(actor) };
}
