//! Runtime-owned actor-liveness registry (`LiveActors`).
//!
//! All access to the actor liveness map goes through the free functions
//! exported here (`track_actor`, `untrack_actor`, `drain_all_for_cleanup`)
//! or through the liveness-probe functions (`with_live_actor`,
//! `with_live_actor_by_id`, `is_actor_live`). Those free functions resolve the
//! map off the current runtime ([`crate::runtime::rt_current`]); the [`LiveActors`]
//! struct itself is owned by `RuntimeInner`.
//!
//! The inner `PoisonSafe<…>` is not re-exported; raw `.lock()` /
//! `.lock_or_recover()` on the map is unreachable by type from outside
//! this module.
//!
//! # Sharding decision
//!
//! `LiveActors` uses a single shard (`PoisonSafe<Option<HashMap<…>>>`),
//! matching the original layout and keeping the module simple.
//! `LINK_TABLE` and `MONITOR_TABLE` use 16 shards because their hot paths
//! are keyed by a single `actor_id` and rarely contend across shards.
//! `LiveActors` still uses one shard because the module primarily protects
//! liveness probes and teardown bookkeeping. Hot by-ID send/ask paths resolve
//! the pointer under the registry lock, then drop it before touching mailboxes.
//!
//! SHIM: no bench suite exists to validate a 5% regression threshold. When
//! `cargo bench -p hew-runtime` becomes available, re-evaluate whether
//! 16-shard `LiveActors` reduces hot-path contention.

use std::collections::HashMap;

use crate::actor::HewActor;
use crate::lifetime::poison_safe::PoisonSafe;
use crate::runtime::rt_current;

/// Opaque wrapper around `*mut HewActor` that makes it `Send`.
///
/// # Safety
///
/// Actor pointers are managed by the runtime and only freed under
/// controlled conditions (shutdown or explicit free via `hew_actor_free`).
pub(crate) struct ActorPtr(pub(crate) *mut HewActor);

// SAFETY: see doc on ActorPtr above.
unsafe impl Send for ActorPtr {}

/// Runtime-owned actor-liveness state.
///
/// Was the `LIVE_ACTORS` + `DEFERRED_TEARDOWN_THREADS` globals; now a field of
/// `RuntimeInner`. Dropping it drops the (normally empty after cleanup) map and
/// any still-pending teardown join handles.
pub(crate) struct LiveActors {
    /// Map of live actor id → pointer. Single shard (see module doc).
    map: PoisonSafe<Option<HashMap<u64, ActorPtr>>>,
    /// Background teardown threads (deferred actor frees and deferred supervisor
    /// stops) that hold raw actor/supervisor pointers while they run. They MUST
    /// be joined before any global sweep (`cleanup_all_actors`) frees the actors
    /// they still reference, or the sweep races an in-flight teardown into a
    /// use-after-free / double-free.
    deferred_teardown_threads: PoisonSafe<Vec<std::thread::JoinHandle<()>>>,
}

impl LiveActors {
    /// Construct an empty liveness registry for a new runtime.
    pub(crate) fn new() -> Self {
        Self {
            map: PoisonSafe::new(None),
            deferred_teardown_threads: PoisonSafe::new(Vec::new()),
        }
    }
}

/// Run `f` with mutable access to the current runtime's live-actor map.
fn with_live_actors<R>(f: impl FnOnce(&mut Option<HashMap<u64, ActorPtr>>) -> R) -> R {
    rt_current().live_actors.map.access(f)
}

/// Register an actor in the live tracking map.
///
/// # Safety
///
/// `actor` must be a valid, fully initialised `HewActor` pointer whose
/// `id` field is already set.
pub(crate) unsafe fn track_actor(actor: *mut HewActor) {
    // SAFETY: caller guarantees `actor` is valid and initialised.
    let id = unsafe { (*actor).id };
    with_live_actors(|map| {
        map.get_or_insert_with(HashMap::new)
            .insert(id, ActorPtr(actor));
    });
}

/// Remove an actor from the live tracking map.
///
/// Returns `true` if the actor was present and removed, `false` if it
/// was not found (e.g. already consumed by `drain_all_for_cleanup`).
/// Only removes the entry if the stored pointer matches `actor`.
pub(crate) fn untrack_actor(actor: *mut HewActor) -> bool {
    // SAFETY: caller guarantees `actor` is valid and not yet freed.
    let id = unsafe { (*actor).id };
    with_live_actors(|map| {
        if let Some(m) = map.as_mut() {
            if let std::collections::hash_map::Entry::Occupied(entry) = m.entry(id) {
                if entry.get().0 == actor {
                    entry.remove();
                    return true;
                }
            }
        }
        false
    })
}

/// Remove and return the actor tracked under `actor_id` if it still matches `expected`.
// live on not(wasm32) — drain_quiesced_actor; dead on wasm32; caller actor.rs:2716
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn take_actor_by_id(actor_id: u64, expected: *mut HewActor) -> Option<*mut HewActor> {
    with_live_actors(|map| {
        let tracked = map.as_mut()?.remove(&actor_id)?;
        if tracked.0 == expected {
            Some(tracked.0)
        } else {
            map.get_or_insert_with(HashMap::new)
                .insert(actor_id, tracked);
            None
        }
    })
}

/// Check whether an actor pointer is still live.
///
/// Calls `f` with a shared reference to the actor and returns `Some(f(..))`
/// if `actor` is in the live map.  Returns `None` otherwise.
///
/// The `LIVE_ACTORS` lock is held across `f`, preventing concurrent frees
/// from reclaiming the allocation while `f` runs.
pub(crate) fn with_live_actor<R>(
    actor: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    with_live_actors(|map| {
        if map
            .as_ref()
            .is_some_and(|m| m.values().any(|ptr| ptr.0 == actor))
        {
            // SAFETY: `actor` is tracked in LIVE_ACTORS; concurrent frees
            // must remove it before reclaiming the allocation.
            Some(f(unsafe { &*actor }))
        } else {
            None
        }
    })
}

/// Check whether an actor ID still maps to the expected live actor pointer.
///
/// Returns `Some(f(..))` if `actor_id` maps to `expected`; `None` otherwise.
/// The `LIVE_ACTORS` lock is held across `f`.
// live on not(wasm32) — monitor.rs + link.rs; dead on wasm32; callers monitor.rs:98, link.rs:201
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn with_live_actor_by_id<R>(
    actor_id: u64,
    expected: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    with_live_actors(|map| {
        if map
            .as_ref()
            .and_then(|m| m.get(&actor_id))
            .is_some_and(|ptr| ptr.0 == expected)
        {
            // SAFETY: `expected` is tracked under `actor_id`; concurrent frees
            // must remove that exact entry before reclaiming the allocation.
            Some(f(unsafe { &*expected }))
        } else {
            None
        }
    })
}

/// Look up an actor by ID and return a copy of its raw pointer if live.
///
/// The lock is *not* held after this returns.  Call sites that can tolerate
/// a narrow TOCTOU window (e.g. remote routing that fails gracefully if the
/// actor disappears) may use this; call sites that need the pointer to stay
/// valid must use `with_live_actor_by_id` instead.
///
/// SHIM: `hew_actor_send_by_id` and `hew_actor_ask_by_id` use this variant to
/// avoid serialising mailbox sends under a single registry mutex. When sharded
/// `LIVE_ACTORS` lands (see module-level doc), this can be replaced with a
/// handle-per-shard approach.
// live on not(wasm32) — hew_actor_send_by_id / hew_actor_ask_by_id / hew_node; dead on wasm32
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn get_actor_ptr_by_id(actor_id: u64) -> Option<*mut HewActor> {
    with_live_actors(|map| {
        map.as_ref()
            .and_then(|m| m.get(&actor_id).map(|ptr| ptr.0))
            .filter(|ptr| !ptr.is_null())
    })
}

/// Check whether an actor pointer is still live (tracked and not yet freed).
///
/// Pointer-identity only: a freed allocation whose address is reused by a
/// concurrently spawned actor probes as live again (ABA). Probes that wait
/// for an actor to *disappear* while other threads may spawn actors must use
/// [`is_actor_live_with_id`] instead.
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "supervisor and actor tests rely on the liveness probe"
    )
)]
pub(crate) fn is_actor_live(actor: *mut HewActor) -> bool {
    with_live_actor(actor, |_| ()).is_some()
}

/// Check whether `actor_id` still maps to the expected live actor pointer.
///
/// ABA-proof variant of [`is_actor_live`]: actor ids are never reused, so a
/// recycled allocation address cannot resurrect liveness for a freed actor.
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "supervisor and actor tests rely on the liveness probe"
    )
)]
pub(crate) fn is_actor_live_with_id(actor_id: u64, expected: *mut HewActor) -> bool {
    with_live_actor_by_id(actor_id, expected, |_| ()).is_some()
}

/// Take all currently tracked actors out of the live map.
///
/// Returns the drained map so the caller can free each actor.
/// Called by `actor::cleanup_all_actors` after worker threads have stopped.
pub(crate) fn drain_all_for_cleanup() -> HashMap<u64, ActorPtr> {
    with_live_actors(|map| match map.as_mut() {
        Some(m) => std::mem::take(m),
        None => HashMap::new(),
    })
}

// ── Deferred teardown threads ───────────────────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn push_deferred_teardown_thread(handle: std::thread::JoinHandle<()>) {
    rt_current()
        .live_actors
        .deferred_teardown_threads
        .access(|threads| threads.push(handle));
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn drain_deferred_teardown_threads() {
    let threads = &rt_current().live_actors.deferred_teardown_threads;
    loop {
        let handles = threads.access(|threads| {
            if threads.is_empty() {
                None
            } else {
                Some(std::mem::take(threads))
            }
        });
        let Some(handles) = handles else { return };
        for handle in handles {
            if handle.join().is_err() {
                eprintln!("hew: warning: deferred teardown thread panicked");
            }
        }
    }
}
