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
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::actor::HewActor;
use crate::lifetime::poison_safe::PoisonSafe;
#[cfg(not(target_arch = "wasm32"))]
use crate::runtime::{rt_current, rt_current_opt};

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
/// Was the `LIVE_ACTORS` + `DEFERRED_TEARDOWN_THREADS` +
/// `lambda_actor::ACTIVE_LAMBDA_DISPATCH` globals; now a field of
/// `RuntimeInner`. Dropping it drops the (normally empty after cleanup) map, any
/// still-pending teardown join handles, and the lambda drain counter.
///
/// Native only: the WASM runtime is single-threaded and has no `RuntimeInner`,
/// so it backs the registry with a module-private static (`LIVE_ACTORS_WASM`)
/// instead — see the `with_live_actors` resolver below.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct LiveActors {
    /// Map of live actor id → pointer. Single shard (see module doc).
    map: PoisonSafe<Option<HashMap<u64, ActorPtr>>>,
    /// Background teardown threads (deferred actor frees and deferred supervisor
    /// stops) that hold raw actor/supervisor pointers while they run. They MUST
    /// be joined before any global sweep (`cleanup_all_actors`) frees the actors
    /// they still reference, or the sweep races an in-flight teardown into a
    /// use-after-free / double-free.
    deferred_teardown_threads: PoisonSafe<Vec<std::thread::JoinHandle<()>>>,
    /// Count of currently-running lambda-actor dispatch threads owned by this
    /// runtime. Used by `hew_lambda_drain_all`.
    active_lambda_dispatch: AtomicUsize,
}

#[cfg(not(target_arch = "wasm32"))]
impl LiveActors {
    /// Construct an empty liveness registry for a new runtime.
    pub(crate) fn new() -> Self {
        Self {
            map: PoisonSafe::new(None),
            deferred_teardown_threads: PoisonSafe::new(Vec::new()),
            active_lambda_dispatch: AtomicUsize::new(0),
        }
    }

    pub(crate) fn lambda_dispatch_fetch_add(&self, value: usize, ordering: Ordering) -> usize {
        self.active_lambda_dispatch.fetch_add(value, ordering)
    }

    pub(crate) fn lambda_dispatch_fetch_sub(&self, value: usize, ordering: Ordering) -> usize {
        self.active_lambda_dispatch.fetch_sub(value, ordering)
    }

    pub(crate) fn lambda_dispatch_load(&self, ordering: Ordering) -> usize {
        self.active_lambda_dispatch.load(ordering)
    }
}

/// Module-private liveness map for the single-threaded WASM runtime.
///
/// The WASM runtime has no `RuntimeInner` to own the registry, so the map lives
/// in a process-global static (as it did before the registry moved onto
/// `RuntimeInner` for native). Single-threaded cooperative scheduling means
/// there is no cross-thread contention; the `PoisonSafe` wrapper keeps the
/// access path identical to native.
#[cfg(target_arch = "wasm32")]
static LIVE_ACTORS_WASM: PoisonSafe<Option<HashMap<u64, ActorPtr>>> = PoisonSafe::new(None);

/// Run `f` with mutable access to the current runtime's live-actor map.
#[cfg(not(target_arch = "wasm32"))]
fn with_live_actors<R>(f: impl FnOnce(&mut Option<HashMap<u64, ActorPtr>>) -> R) -> R {
    rt_current().live_actors.map.access(f)
}

/// Run `f` with mutable access to the current runtime's live-actor map, if one
/// is installed.
///
/// The liveness probe/read surface is reachable from trap/teardown paths that
/// also support manually-constructed actors with no runtime installed. When the
/// map was a process-global static, those probes observed an empty map and
/// returned "not live"; after deglobalization they must keep that read-only
/// default while registration/removal paths continue to fail closed through
/// [`with_live_actors`].
#[cfg(not(target_arch = "wasm32"))]
fn with_live_actors_opt<R>(f: impl FnOnce(&mut Option<HashMap<u64, ActorPtr>>) -> R) -> Option<R> {
    rt_current_opt().map(|rt| rt.live_actors.map.access(f))
}

/// Run `f` with mutable access to the WASM runtime's live-actor map.
///
/// WASM still uses a module-private static, so the optional native resolver is
/// always present there.
#[cfg(target_arch = "wasm32")]
fn with_live_actors_opt<R>(f: impl FnOnce(&mut Option<HashMap<u64, ActorPtr>>) -> R) -> Option<R> {
    Some(with_live_actors(f))
}

/// Run `f` with mutable access to the WASM runtime's live-actor map.
#[cfg(target_arch = "wasm32")]
fn with_live_actors<R>(f: impl FnOnce(&mut Option<HashMap<u64, ActorPtr>>) -> R) -> R {
    LIVE_ACTORS_WASM.access(f)
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
    with_live_actors_opt(|map| {
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
    .unwrap_or(false)
}

/// Remove and return the actor tracked under `actor_id` if it still matches `expected`.
// live on not(wasm32) — drain_quiesced_actor; dead on wasm32; caller actor.rs:2716
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn take_actor_by_id(actor_id: u64, expected: *mut HewActor) -> Option<*mut HewActor> {
    with_live_actors_opt(|map| {
        let tracked = map.as_mut()?.remove(&actor_id)?;
        if tracked.0 == expected {
            Some(tracked.0)
        } else {
            map.get_or_insert_with(HashMap::new)
                .insert(actor_id, tracked);
            None
        }
    })
    .flatten()
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
    with_live_actors_opt(|map| {
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
    .flatten()
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
    with_live_actors_opt(|map| {
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
    .flatten()
}

/// Look up an actor by ID and return a copy of its raw pointer if live.
///
/// The lock is *not* held after this returns.  Call sites that can tolerate
/// a narrow TOCTOU window (e.g. remote routing that fails gracefully if the
/// actor disappears, or drain paths that immediately re-validate with
/// `with_live_actor_by_id`) may use this.
///
/// **Do not use this for send/ask dispatch** — use [`with_actor_send_by_id`]
/// instead so the actor cannot be freed between lookup and dereference.
// live on not(wasm32) — collect_pending_actor / hew_node; dead on wasm32
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn get_actor_ptr_by_id(actor_id: u64) -> Option<*mut HewActor> {
    with_live_actors_opt(|map| {
        map.as_ref()
            .and_then(|m| m.get(&actor_id).map(|ptr| ptr.0))
            .filter(|ptr| !ptr.is_null())
    })
    .flatten()
}

/// RAII guard that decrements `HewActor.send_pin_count` on drop.
///
/// Taken by [`with_actor_send_by_id`] after the liveness validation
/// (under `LIVE_ACTORS`) so the free path cannot reclaim the actor
/// allocation while the by-ID operation is in progress.
///
/// The `Release` ordering on drop pairs with the free path's `Acquire`
/// load of `send_pin_count`, ensuring all writes from the pinned
/// operation are visible to the freer before it reclaims the allocation.
// live on not(wasm32); dead on wasm32.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
struct SendPinGuard(*mut HewActor);

// SAFETY: `HewActor` is `Send`; the guard only touches the atomic
// `send_pin_count` field.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
unsafe impl Send for SendPinGuard {}

#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
impl Drop for SendPinGuard {
    fn drop(&mut self) {
        // SAFETY: the pointer was validated-live when the pin was taken.
        // The free path defers the free until `send_pin_count` reaches 0,
        // so the allocation remains valid until this drop runs.
        unsafe { (*self.0).send_pin_count.fetch_sub(1, Ordering::Release) };
    }
}

/// Look up an actor by ID and, if live, call `f` with its raw pointer while
/// guaranteeing the allocation cannot be freed for the duration of `f`.
///
/// **Lock discipline**: unlike an older design that held `LIVE_ACTORS` across
/// `f`, this function:
///
/// 1. Acquires `LIVE_ACTORS`, validates liveness, increments `send_pin_count`
///    on the actor, then **releases** `LIVE_ACTORS`.
/// 2. Calls `f(ptr)` with the lock fully released.
/// 3. Decrements `send_pin_count` via a RAII guard on return **or panic**.
///
/// The free path in `hew_actor_free_inner` treats `send_pin_count > 0` as
/// non-quiescent and defers `untrack_actor` + `finalize` until all pins are
/// released.  Because `LIVE_ACTORS` is not held across `f`, blocking mailbox
/// operations (the `Block` overflow condvar wait) and eviction retire paths
/// (`DropOld` → `hew_msg_node_free` → `retire_orphaned_ask_sender_ref` →
/// `scheduler::enqueue_resume` → `with_live_actor`) are both safe to call
/// from `f` without deadlocking.
///
/// # Lock ordering (unchanged from the module invariant)
///
/// `f` is free to acquire any lock that does not transitively re-acquire
/// `LIVE_ACTORS`.  `LIVE_ACTORS` is fully released before `f` runs, so
/// `enqueue_resume` → `with_live_actor` does not self-deadlock.
///
/// Returns `Some(f(ptr))` if the actor is live, `None` if not found.
// live on not(wasm32) — hew_actor_send_by_id / hew_actor_ask_by_id; dead on wasm32
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn with_actor_send_by_id<R>(
    actor_id: u64,
    f: impl FnOnce(*mut HewActor) -> R,
) -> Option<R> {
    // Phase 1: validate liveness and take a send pin — all under LIVE_ACTORS.
    // Incrementing send_pin_count here (before releasing the lock) ensures that
    // if a concurrent hew_actor_free_inner sees pin_count > 0 in its quiescence
    // loop, it defers the free until the pin drops.
    let ptr = with_live_actors_opt(|map| {
        let ptr = map
            .as_ref()
            .and_then(|m| m.get(&actor_id).map(|p| p.0))
            .filter(|p| !p.is_null())?;
        // SAFETY: `ptr` is tracked-live under the registry lock.  The
        // fetch_add happens before the lock is released, so the free path
        // observes pin_count > 0 before it can call untrack_actor.
        unsafe { (*ptr).send_pin_count.fetch_add(1, Ordering::AcqRel) };
        Some(ptr)
    })
    .flatten()?;

    // Phase 2: LIVE_ACTORS is released; call `f` with the pinned pointer.
    // `_pin` drops after `f` returns: decrements send_pin_count with Release
    // ordering, paired with the free path's Acquire load.
    let _pin = SendPinGuard(ptr);
    Some(f(ptr))
}

/// Resolve the per-actor-TYPE dispatch function pointer for a live actor id,
/// returned as an opaque `*const c_void` codec-registry key.
///
/// The dispatch pointer is `HewActor.dispatch` — the same
/// `__hew_actor_dispatch_<Actor>` global the codegen codec seeder registers the
/// actor's codecs under. The cross-node decode paths resolve it here from the
/// wire's `target_actor_id` so an inbound frame is decoded ONLY by the codec
/// belonging to its target actor's type (`(dispatch, msg_type)` keying), never
/// by a different actor type whose `msg_type` SipHash-collides.
///
/// Reads the dispatch field UNDER the liveness lock (via `with_live_actors_opt`)
/// so a concurrent teardown cannot free the actor mid-read. Returns `None` when
/// the actor is not live (the caller then takes the existing fail-closed drop /
/// `DecodeFailure` path — never a fabricated key) or has no dispatch set.
// live on not(wasm32) — cross-node inbound decode; dead on wasm32.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn dispatch_ptr_by_id(actor_id: u64) -> Option<*const std::ffi::c_void> {
    with_live_actors_opt(|map| {
        let actor_ptr = map
            .as_ref()
            .and_then(|m| m.get(&actor_id).map(|ptr| ptr.0))
            .filter(|ptr| !ptr.is_null())?;
        // SAFETY: `actor_ptr` is tracked-live under the lock held for this
        // closure; a concurrent free must remove the entry (under the same lock)
        // before reclaiming the allocation, so the dispatch field read is valid.
        let dispatch = unsafe { (*actor_ptr).dispatch }?;
        Some(dispatch as *const std::ffi::c_void)
    })
    .flatten()
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
    with_live_actors_opt(|map| match map.as_mut() {
        Some(m) => std::mem::take(m),
        None => HashMap::new(),
    })
    .unwrap_or_default()
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
    let Some(rt) = rt_current_opt() else { return };
    let threads = &rt.live_actors.deferred_teardown_threads;
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

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[test]
    fn liveness_reads_treat_missing_runtime_as_empty_registry() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(
            crate::runtime::rt_default().is_none(),
            "test requires the runtime slot to be empty"
        );

        let actor = std::ptr::dangling_mut::<HewActor>();
        assert_eq!(with_live_actor(actor, |_| unreachable!()), None);
        assert_eq!(with_live_actor_by_id(42, actor, |_| unreachable!()), None);
        assert_eq!(get_actor_ptr_by_id(42), None);
        assert!(!is_actor_live(actor));
        assert!(!is_actor_live_with_id(42, actor));
        assert!(drain_all_for_cleanup().is_empty());
        drain_deferred_teardown_threads();
    }
}
