//! Module-private owner of the `LIVE_ACTORS` registry.
//!
//! All access to the actor liveness map goes through the free functions
//! exported here (`track_actor`, `untrack_actor`, `drain_all_for_cleanup`)
//! or through the liveness-probe functions (`with_live_actor`,
//! `with_live_actor_by_id`, `is_actor_live`).
//!
//! The inner `PoisonSafe<…>` is not re-exported; raw `.lock()` /
//! `.lock_or_recover()` on the global is unreachable by type from outside
//! this module.
//!
//! # Sharding decision
//!
//! `LIVE_ACTORS` uses a single shard (`PoisonSafe<Option<HashMap<…>>>`),
//! matching the original layout and keeping the module simple.
//! `LINK_TABLE` and `MONITOR_TABLE` use 16 shards because their hot paths
//! are keyed by a single `actor_id` and rarely contend across shards.
//! `LIVE_ACTORS` still uses one shard because the module primarily protects
//! liveness probes and teardown bookkeeping. Hot by-ID send/ask paths resolve
//! the pointer under the registry lock, then drop it before touching mailboxes.
//!
//! SHIM: no bench suite exists to validate a 5% regression threshold. When
//! `cargo bench -p hew-runtime` becomes available, re-evaluate whether
//! 16-shard `LIVE_ACTORS` reduces hot-path contention.

use std::collections::HashMap;

use crate::actor::HewActor;
use crate::lifetime::poison_safe::PoisonSafe;

/// Opaque wrapper around `*mut HewActor` that makes it `Send`.
///
/// # Safety
///
/// Actor pointers are managed by the runtime and only freed under
/// controlled conditions (shutdown or explicit free via `hew_actor_free`).
pub(crate) struct ActorPtr(pub(crate) *mut HewActor);

// SAFETY: see doc on ActorPtr above.
unsafe impl Send for ActorPtr {}

// native-only: actor globals use OS thread primitives absent in single-threaded WASM
static LIVE_ACTORS: PoisonSafe<Option<HashMap<u64, ActorPtr>>> = PoisonSafe::new(None);

/// Register an actor in the live tracking map.
///
/// # Safety
///
/// `actor` must be a valid, fully initialised `HewActor` pointer whose
/// `id` field is already set.
pub(crate) unsafe fn track_actor(actor: *mut HewActor) {
    // SAFETY: caller guarantees `actor` is valid and initialised.
    let id = unsafe { (*actor).id };
    LIVE_ACTORS.access(|map| {
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
    LIVE_ACTORS.access(|map| {
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
pub(crate) fn take_actor_by_id(actor_id: u64, expected: *mut HewActor) -> Option<*mut HewActor> {
    LIVE_ACTORS.access(|map| {
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
    LIVE_ACTORS.access(|map| {
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
pub(crate) fn with_live_actor_by_id<R>(
    actor_id: u64,
    expected: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    LIVE_ACTORS.access(|map| {
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
pub(crate) fn get_actor_ptr_by_id(actor_id: u64) -> Option<*mut HewActor> {
    LIVE_ACTORS.access(|map| {
        map.as_ref()
            .and_then(|m| m.get(&actor_id).map(|ptr| ptr.0))
            .filter(|ptr| !ptr.is_null())
    })
}

/// Check whether an actor pointer is still live (tracked and not yet freed).
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

/// Take all currently tracked actors out of the live map.
///
/// Returns the drained map so the caller can free each actor.
/// Called by `actor::cleanup_all_actors` after worker threads have stopped.
pub(crate) fn drain_all_for_cleanup() -> HashMap<u64, ActorPtr> {
    LIVE_ACTORS.access(|map| match map.as_mut() {
        Some(m) => std::mem::take(m),
        None => HashMap::new(),
    })
}

// ── DEFERRED_ACTOR_FREE_THREADS ─────────────────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
static DEFERRED_ACTOR_FREE_THREADS: PoisonSafe<Vec<std::thread::JoinHandle<()>>> =
    PoisonSafe::new(Vec::new());

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn push_deferred_actor_free_thread(handle: std::thread::JoinHandle<()>) {
    DEFERRED_ACTOR_FREE_THREADS.access(|threads| threads.push(handle));
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn drain_deferred_actor_free_threads() {
    loop {
        let handles = DEFERRED_ACTOR_FREE_THREADS.access(|threads| {
            if threads.is_empty() {
                None
            } else {
                Some(std::mem::take(threads))
            }
        });
        let Some(handles) = handles else { return };
        for handle in handles {
            if handle.join().is_err() {
                eprintln!("hew: warning: deferred actor free thread panicked");
            }
        }
    }
}
