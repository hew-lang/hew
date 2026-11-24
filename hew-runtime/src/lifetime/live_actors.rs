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
//! # Sharding and lock ordering
//!
//! `LiveActors` partitions actor IDs across 16 independent shards
//! (`LIVE_ACTOR_SHARDS`), matching `LINK_TABLE` and `MONITOR_TABLE`.
//!
//! **Single-shard lock invariant**: no operation in this module ever holds more
//! than one shard lock at a time. Point operations (`track_actor`,
//! `untrack_actor`, `take_actor_by_id`, `pin_actor_by_id`,
//! `with_live_actor_by_id`, `get_actor_ptr_by_id`,
//! `swap_slot_under_registry_lock`) acquire only `shards[actor_id % 16]`.
//! Whole-registry scans (`with_live_actor`, `has_drain_blocking_suspended_actor`,
//! `snapshot_live_actor_*`, `drain_all_for_cleanup`) visit shards `0..16` one
//! shard at a time, releasing shard `i` before acquiring shard `i + 1`. Because
//! an actor's `id` is immutable for its lifetime, its shard is fixed, so
//! per-actor serialization (`pin_actor_by_id` vs `untrack_actor`, and
//! `has_drain_blocking_suspended_actor` vs `swap_slot_under_registry_lock`)
//! synchronizes on that single shard lock with zero possibility of cross-shard
//! deadlock.

use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::actor::HewActor;
use crate::lifetime::poison_safe::PoisonSafe;

/// Number of shards for the live-actor registry, matching `LINK_TABLE` and
/// `MONITOR_TABLE`. Must remain a power of two.
const LIVE_ACTOR_SHARDS: usize = 16;

#[inline]
fn shard_index(actor_id: u64) -> usize {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "shard index is masked to LIVE_ACTOR_SHARDS - 1 (0..16)"
    )]
    {
        (actor_id as usize) & (LIVE_ACTOR_SHARDS - 1)
    }
}

/// Opaque wrapper around `*mut HewActor` that makes it `Send`.
///
/// # Safety
///
/// Actor pointers are managed by the runtime and only freed under
/// controlled conditions (shutdown or explicit free via `hew_actor_free`).
pub(crate) struct ActorPtr(pub(crate) *mut HewActor);

// SAFETY: see doc on ActorPtr above.
unsafe impl Send for ActorPtr {}

struct LiveActor {
    actor: ActorPtr,
}

fn retire_actor_route(actor: *mut HewActor, actor_id: u64) {
    // SAFETY: callers invoke this only after successful removal from the live
    // map and retain allocation ownership until finalization.
    let token = unsafe { (*actor).local_pid_id };
    if token == crate::lifetime::local_handles::HewLocalPidId::INVALID {
        return;
    }
    // SAFETY: callers invoke this only after removal from the live map while
    // retaining allocation ownership until finalization.
    let runtime = unsafe { (*actor).runtime };
    // SAFETY: the owning runtime outlives tracked actor route retirement.
    unsafe { crate::platform::executor::retire_tracked_actor_route(runtime, token, actor_id) };
}

/// Runtime-owned actor-liveness state.
pub(crate) struct LiveActors {
    /// Sharded maps of live actor id -> pointer (16 shards; see module doc).
    shards: [PoisonSafe<Option<FxHashMap<u64, LiveActor>>>; LIVE_ACTOR_SHARDS],
    /// Exact count of tracked actors across all shards, maintained under each
    /// shard's lock on insert, remove, and drain.
    len: AtomicUsize,
    /// Background teardown threads that hold raw actor/supervisor pointers.
    #[cfg(not(target_arch = "wasm32"))]
    deferred_teardown_threads: PoisonSafe<Vec<std::thread::JoinHandle<()>>>,
}

impl LiveActors {
    /// Construct an empty liveness registry for a runtime.
    pub(crate) const fn new() -> Self {
        Self {
            shards: [const { PoisonSafe::new(None) }; LIVE_ACTOR_SHARDS],
            len: AtomicUsize::new(0),
            #[cfg(not(target_arch = "wasm32"))]
            deferred_teardown_threads: PoisonSafe::new(Vec::new()),
        }
    }
}

/// Run `f` with mutable access to the shard owning `actor_id`.
fn with_shard<R>(
    actor_id: u64,
    f: impl FnOnce(&mut Option<FxHashMap<u64, LiveActor>>, &AtomicUsize) -> R,
) -> R {
    let actors = crate::platform::executor::current_live_actors();
    actors.shards[shard_index(actor_id)].access(|map| f(map, &actors.len))
}

/// Run `f` with mutable access to the shard owning `actor_id` when a runtime is installed.
fn with_shard_opt<R>(
    actor_id: u64,
    f: impl FnOnce(&mut Option<FxHashMap<u64, LiveActor>>, &AtomicUsize) -> R,
) -> Option<R> {
    crate::platform::executor::current_live_actors_opt()
        .map(|actors| actors.shards[shard_index(actor_id)].access(|map| f(map, &actors.len)))
}

/// Register an actor in the live tracking map.
///
/// Returns `false` without changing the existing entry when the `ActorId` is
/// already live. Publication callers must treat that collision as spawn
/// failure and roll back only their own route reservation.
///
/// # Safety
///
/// `actor` must be a valid, fully initialised `HewActor` pointer whose
/// `id` field is already set.
pub(crate) unsafe fn track_actor(actor: *mut HewActor) -> bool {
    // SAFETY: caller guarantees `actor` is valid and initialised.
    let id = unsafe { (*actor).id };
    with_shard(id, |map, len| {
        match map.get_or_insert_with(FxHashMap::default).entry(id) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(LiveActor {
                    actor: ActorPtr(actor),
                });
                len.fetch_add(1, Ordering::AcqRel);
                true
            }
            std::collections::hash_map::Entry::Occupied(_) => false,
        }
    })
}

#[cfg(test)]
pub(crate) fn actor_count_for_test() -> usize {
    crate::platform::executor::current_live_actors_opt()
        .map_or(0, |actors| actors.len.load(Ordering::Acquire))
}

/// Remove an actor from the live tracking map.
///
/// Returns `true` if the actor was present and removed, `false` if it
/// was not found (e.g. already consumed by `drain_all_for_cleanup`).
/// Only removes the entry if the stored pointer matches `actor`.
pub(crate) fn untrack_actor(actor: *mut HewActor) -> bool {
    // SAFETY: caller guarantees `actor` is valid and not yet freed.
    let id = unsafe { (*actor).id };
    let removed = with_shard_opt(id, |map, len| {
        if let Some(m) = map.as_mut() {
            if let std::collections::hash_map::Entry::Occupied(entry) = m.entry(id) {
                if entry.get().actor.0 == actor {
                    entry.remove();
                    len.fetch_sub(1, Ordering::AcqRel);
                    return true;
                }
            }
        }
        false
    })
    .unwrap_or(false);
    if removed {
        retire_actor_route(actor, id);
    }
    removed
}

/// Remove and return the actor tracked under `actor_id` if it still matches `expected`.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn take_actor_by_id(actor_id: u64, expected: *mut HewActor) -> Option<*mut HewActor> {
    let actor = with_shard_opt(actor_id, |map, len| {
        let m = map.as_mut()?;
        if let std::collections::hash_map::Entry::Occupied(entry) = m.entry(actor_id) {
            if entry.get().actor.0 == expected {
                let tracked = entry.remove();
                len.fetch_sub(1, Ordering::AcqRel);
                return Some(tracked.actor.0);
            }
        }
        None
    })
    .flatten();
    if let Some(actor) = actor {
        retire_actor_route(actor, actor_id);
    }
    actor
}

/// Check whether an actor pointer is still live.
///
/// Calls `f` with a shared reference to the actor and returns `Some(f(..))`
/// if `actor` is in the live map.  Returns `None` otherwise.
///
/// The owning shard's lock is held across `f`, preventing concurrent frees
/// from reclaiming the allocation while `f` runs. Shards are probed one at a
/// time so no two shard locks are ever held simultaneously.
pub(crate) fn with_live_actor<R>(
    actor: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    let actors = crate::platform::executor::current_live_actors_opt()?;
    let mut f = Some(f);
    for shard in &actors.shards {
        let result = shard.access(|map| {
            if map
                .as_ref()
                .is_some_and(|m| m.values().any(|ptr| ptr.actor.0 == actor))
            {
                let func = f.take().expect("closure runs at most once");
                // SAFETY: `actor` is tracked in this shard; concurrent frees
                // must remove it under this same shard lock before reclaiming
                // the allocation.
                Some(func(unsafe { &*actor }))
            } else {
                None
            }
        });
        if result.is_some() {
            return result;
        }
    }
    None
}

/// Return whether any tracked actor still owns a parked continuation that
/// represents IN-FLIGHT work the shutdown drain must wait for.
///
/// The liveness shard lock is held while reading each actor state, so a
/// concurrent free cannot reclaim an allocation beneath the scan. Shutdown uses
/// this as source-agnostic drain authority: TCP, timer, ask, stream, and other
/// await implementations all converge on `HewActorState::Suspended`.
///
/// A `Suspended` actor whose currently-dispatching `ask` was ABANDONED by its
/// caller is NOT in-flight work: the `await … | after d` deadline (or a task
/// cancel) already resolved the caller with a typed `Err`, and nothing will
/// ever consume the parked handler's reply. Blocking the drain on it would
/// regress the documented never-hangs abandonment contract into a full
/// drain-timeout plus abnormal exit. The abandonment bit is the reply
/// channel's `cancelled` flag, reached through the actor's independently
/// retained [`crate::actor::HewActor::parked_ask_channel`] gate reference —
/// safe to dereference here because every release site swaps that slot to
/// null under THIS actor's shard lock before dropping the reference
/// (`activation::release_parked_ask_channel`).
///
/// Swap `slot` to null under the owning actor's live-actors shard lock.
///
/// Serialization primitive for pointer slots that the shutdown drain scan
/// dereferences under this lock ([`has_drain_blocking_suspended_actor`]): a
/// releaser swaps here first, so a scan that observed a non-null pointer holds
/// the shard lock the swap needs and the backing allocation cannot be freed
/// beneath the dereference. The actual release of the swapped-out reference
/// belongs OUTSIDE the lock (it may run reply destructors). Falls back to a
/// bare swap when no runtime is installed — the scan resolves through the same
/// runtime slot, so no scan can be racing in that case.
pub(crate) fn swap_slot_under_registry_lock(
    actor_id: u64,
    slot: &std::sync::atomic::AtomicPtr<std::ffi::c_void>,
) -> *mut std::ffi::c_void {
    with_shard_opt(actor_id, |_, _| {
        slot.swap(std::ptr::null_mut(), Ordering::AcqRel)
    })
    .unwrap_or_else(|| slot.swap(std::ptr::null_mut(), Ordering::AcqRel))
}

/// Check whether an actor ID still maps to the expected live actor pointer.
///
/// Returns `Some(f(..))` if `actor_id` maps to `expected`; `None` otherwise.
/// The owning shard's lock is held across `f`.
pub(crate) fn with_live_actor_by_id<R>(
    actor_id: u64,
    expected: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    with_shard_opt(actor_id, |map, _| {
        if map
            .as_ref()
            .and_then(|m| m.get(&actor_id))
            .is_some_and(|ptr| ptr.actor.0 == expected)
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
/// a narrow TOCTOU window (e.g. remote routing that only compares the pointer
/// or fails gracefully if the actor disappears) may use this.
///
/// **Do not use this for send/ask dispatch** — use [`with_actor_send_by_id`]
/// instead so the actor cannot be freed between lookup and dereference.
pub(crate) fn get_actor_ptr_by_id(actor_id: u64) -> Option<*mut HewActor> {
    with_shard_opt(actor_id, |map, _| {
        map.as_ref()
            .and_then(|m| m.get(&actor_id).map(|ptr| ptr.actor.0))
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
pub(crate) struct ActorPin(*mut HewActor);

// SAFETY: `HewActor` is `Send`; the guard only touches the atomic
// `send_pin_count` field.
unsafe impl Send for ActorPin {}

impl Drop for ActorPin {
    fn drop(&mut self) {
        // SAFETY: the pointer was validated-live when the pin was taken.
        // The free path defers the free until `send_pin_count` reaches 0,
        // so the allocation remains valid until this drop runs.
        unsafe { (*self.0).send_pin_count.fetch_sub(1, Ordering::Release) };
    }
}

impl ActorPin {
    /// Borrow the pinned actor allocation.
    #[must_use]
    pub(crate) fn actor(&self) -> &HewActor {
        // SAFETY: construction increments `send_pin_count` while the actor is
        // tracked; the free path cannot reclaim it until this guard drops.
        unsafe { &*self.0 }
    }

    /// Return the pinned actor pointer for existing internal operations.
    #[must_use]
    pub(crate) fn as_ptr(&self) -> *mut HewActor {
        self.0
    }
}

/// Resolve one `ActorId` and hold its existing liveness/send pin until drop.
///
/// The live-actor lock is released before this function returns. Callers may
/// compose multiple pins, but must release them before reply waits, callbacks,
/// or user work. Existing mailbox submission keeps its pin only for that
/// submission operation.
pub(crate) fn pin_actor_by_id(actor_id: u64) -> Option<ActorPin> {
    let ptr = with_shard_opt(actor_id, |map, _| {
        let ptr = map
            .as_ref()
            .and_then(|m| m.get(&actor_id).map(|p| p.actor.0))
            .filter(|p| !p.is_null())?;
        // SAFETY: the pointer is tracked under the owning shard lock. Incrementing
        // before releasing the lock is mutually exclusive with untracking.
        unsafe { (*ptr).send_pin_count.fetch_add(1, Ordering::AcqRel) };
        Some(ptr)
    })
    .flatten()?;
    Some(ActorPin(ptr))
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
/// The free paths (`hew_actor_free_inner`, `drain_quiesced_actor`) call
/// `untrack_actor` (or `take_actor_by_id`) FIRST — under `LIVE_ACTORS` — so no
/// new pins can be taken after the map entry is removed.  They then spin until
/// `send_pin_count` drops to 0 (draining any pins taken before the removal)
/// before calling `finalize`.  Because the pin-increment and the map-removal
/// both run under `LIVE_ACTORS`, the two are mutually exclusive: either this
/// function pins before the freer removes the entry (freer waits for the pin to
/// drain), or the freer removes the entry before this function's map lookup
/// (lookup returns `None` → no pin, no use of a freed pointer).
///
/// Because `LIVE_ACTORS` is not held across `f`, blocking mailbox operations
/// (the `Block` overflow condvar wait) and eviction retire paths
/// (`DropOld` → `hew_msg_node_free` → `retire_orphaned_ask_sender_ref` →
/// `scheduler::enqueue_resume_by_incarnation` → `with_live_incarnation`) are
/// both safe to call from `f` without deadlocking.
///
/// # Lock ordering (unchanged from the module invariant)
///
/// `f` is free to acquire any lock that does not transitively re-acquire
/// `LIVE_ACTORS`.  `LIVE_ACTORS` is fully released before `f` runs, and the
/// wake path re-takes it twice without ever holding it across a call:
/// `with_live_incarnation` takes the lock to resolve the incarnation and pin
/// it, releases it, and only then does `enqueue_resume_pinned` →
/// `with_live_actor` take it again for the wake itself.  So neither step
/// self-deadlocks against this function.
///
/// Returns `Some(f(ptr))` if the actor is live, `None` if not found.
pub(crate) fn with_actor_send_by_id<R>(
    actor_id: u64,
    f: impl FnOnce(*mut HewActor) -> R,
) -> Option<R> {
    let pin = pin_actor_by_id(actor_id)?;
    Some(f(pin.as_ptr()))
}

/// Like [`with_actor_send_by_id`], but additionally verifies the pinned actor
/// IS the resolved incarnation by matching its full [`HewActor::spawn_serial`]
/// against `expected_serial` before running `f`.
///
/// The by-ID pin resolves through `LIVE_ACTORS`, keyed by the packed `id`, whose
/// serial portion is masked to 48 bits (`pid::hew_pid_make`), so a pin by `id`
/// alone could hand `f` an actor that merely collides on the masked value. The
/// two-phase owner-scoped role ask copies the resolved incarnation's full serial
/// out under `children_lock` and passes it here; on serial mismatch this returns
/// `None` WITHOUT calling `f` — fail closed, treated exactly like a retirement
/// (the ID aliased a different incarnation). Only used where the caller holds
/// the resolved incarnation's authoritative serial.
///
/// The spawn allocator now refuses past `MAX_ACTOR_SERIAL` rather than wrapping
/// into already-issued ids, so production cannot reach the collision this
/// guards; the check stays because it is what makes wrong-actor delivery
/// unrepresentable at the resolve→submit seam rather than merely unlikely, and
/// `supervisor.rs` fabricates the collision to keep it honest.
pub(crate) fn with_actor_send_by_identity<R>(
    actor_id: u64,
    expected_serial: u64,
    f: impl FnOnce(&ActorPin) -> R,
) -> Option<R> {
    let pin = pin_actor_by_id(actor_id)?;
    if pin.actor().spawn_serial != expected_serial {
        return None;
    }
    Some(f(&pin))
}

/// The exact identity of one actor incarnation: the location-transparent `id`
/// plus the never-reissued [`HewActor::spawn_serial`].
///
/// A readiness source that records a wake target records an ADDRESS when it
/// keeps a `*mut HewActor`. Addresses are recycled: the allocator hands a freed
/// actor box straight back to the next spawn, so a wake registered by one
/// incarnation and fired after that incarnation died can resolve to whatever
/// actor now occupies the address. The registry's pointer probe cannot tell the
/// two apart — both are tracked-live — so the wake lands on the wrong
/// incarnation and moves it `Suspended -> Runnable` with no readiness behind
/// it.
///
/// `spawn_serial` is the discriminator that closes it. It comes from a
/// monotonic allocator that refuses to wrap ([`crate::pid::MAX_ACTOR_SERIAL`]
/// is a hard spawn failure, not a wrap), so no two incarnations in a process
/// ever share one. Recording the pair and resolving it through
/// [`with_live_incarnation`] makes a wake name an incarnation, not an address.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct ActorIncarnation {
    actor_id: u64,
    spawn_serial: u64,
}

impl ActorIncarnation {
    /// No wake target.
    ///
    /// Actor id `0` is the runtime's invalid-actor sentinel. Minting refuses
    /// it: [`crate::pid::next_actor_id`] rejects any slot that fails
    /// `actor_slot_fits_internal_alias`, slot `0` included, before it can reach
    /// the packing primitive - which masks rather than refuses. So `0` can never
    /// name a real incarnation. Sources
    /// that park without an actor — a foreign thread, or an await registered
    /// before the caller is known — record this.
    pub(crate) const NONE: Self = Self {
        actor_id: 0,
        spawn_serial: 0,
    };

    /// Capture the incarnation of a live actor, or [`Self::NONE`] for null.
    ///
    /// # Safety
    ///
    /// `actor`, if non-null, must be live for the duration of this call. Every
    /// caller records its own actor at park time, where that holds by
    /// construction.
    pub(crate) unsafe fn of(actor: *mut HewActor) -> Self {
        if actor.is_null() {
            return Self::NONE;
        }
        // SAFETY: caller guarantees `actor` is live for this read.
        unsafe {
            Self {
                actor_id: (*actor).id,
                spawn_serial: (*actor).spawn_serial,
            }
        }
    }

    /// Rebuild an incarnation from its two scalars (an ABI-boundary record).
    pub(crate) const fn from_parts(actor_id: u64, spawn_serial: u64) -> Self {
        Self {
            actor_id,
            spawn_serial,
        }
    }

    /// Whether this records no target at all.
    pub(crate) const fn is_none(self) -> bool {
        self.actor_id == 0
    }

    /// The location-transparent actor id half of the pair.
    pub(crate) const fn actor_id(self) -> u64 {
        self.actor_id
    }

    /// The never-reissued serial half of the pair.
    pub(crate) const fn spawn_serial(self) -> u64 {
        self.spawn_serial
    }
}

/// Pin the exact incarnation named by `target` and run `f` while it is held.
///
/// Returns `None` — and runs nothing — when the incarnation is gone: the id is
/// untracked, or it now resolves to a DIFFERENT incarnation (a serial
/// mismatch). Dropping the call is the correct answer in both cases; the dead
/// incarnation's continuation was destroyed by its own teardown.
///
/// The pin is the allocation lease: [`pin_actor_by_id`] takes it under the
/// registry lock, and the free path drains `send_pin_count` to zero before
/// reclaiming the box, so `f` cannot race the target into a free or an address
/// reuse.
pub(crate) fn with_live_incarnation<R>(
    target: ActorIncarnation,
    f: impl FnOnce(&ActorPin) -> R,
) -> Option<R> {
    if target.is_none() {
        return None;
    }
    with_actor_send_by_identity(target.actor_id, target.spawn_serial, f)
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
/// ABA-proof variant of [`is_actor_live`]: it also matches `expected`, so a
/// recycled allocation address cannot resurrect liveness for a freed actor
/// (a different live actor reusing the address maps under a different `id` or
/// pointer). Note the packed `id` masks the serial to 48 bits, so full
/// incarnation identity across a 2^48 wrap needs the serial discriminator
/// (`with_actor_send_by_identity`); this probe's pointer + id match is
/// sufficient for its liveness-probe callers.
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

/// Copy the currently tracked actor pointers without removing them.
///
/// Unlike [`drain_all_for_cleanup`] this leaves the registry intact, so the
/// caller can act on live actors while reclamation ownership stays with the
/// cleanup sweep. Used by `actor::retire_parked_activations`, which must run
/// before the sweep but must not take the actors from it.
///
#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::{retire_actor_route, ActorPtr, FxHashMap, HewActor, Ordering};
    use crate::runtime::{rt_current, rt_current_opt};
    use std::collections::HashMap;

    pub(crate) fn has_drain_blocking_suspended_actor() -> bool {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return false;
        };
        for shard in &actors.shards {
            let blocked = shard.access(|map| {
                map.as_ref().is_some_and(|entries| {
                    entries.values().any(|entry| {
                        let actor = &entry.actor.0;
                        // SAFETY: the pointer remains tracked under its owning shard lock;
                        // every free path must untrack it before reclaiming the box.
                        let a = unsafe { &**actor };
                        if a.actor_state.load(Ordering::Acquire)
                            != crate::internal::types::HewActorState::Suspended as i32
                        {
                            return false;
                        }
                        if !a.checked_invocation.load(Ordering::Acquire).is_null() {
                            return true;
                        }
                        let gate = a.parked_ask_channel.load(Ordering::Acquire);
                        if gate.is_null() {
                            return true;
                        }
                        // SAFETY: the gate slot owns a retained channel reference that
                        // is released only after a swap performed under this same
                        // actor's shard lock.
                        let cancelled = unsafe {
                            (*gate.cast::<crate::reply_channel::HewReplyChannel>())
                                .cancelled
                                .load(Ordering::Acquire)
                        };
                        !cancelled
                    })
                })
            });
            if blocked {
                return true;
            }
        }
        false
    }

    /// Safe to call only once worker threads are joined: the returned pointers
    /// are unpinned, so a concurrent free would dangle them.
    pub(crate) fn snapshot_live_actor_ptrs() -> Vec<*mut HewActor> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return Vec::new();
        };
        let mut ptrs = Vec::new();
        for shard in &actors.shards {
            shard.access(|map| {
                if let Some(m) = map.as_ref() {
                    ptrs.extend(m.values().map(|entry| entry.actor.0));
                }
            });
        }
        ptrs
    }

    /// Copy identities while workers are live. Resolve each through a fresh pin
    /// before accessing its allocation; an actor may retire after this snapshot.
    pub(crate) fn snapshot_live_actor_ids() -> Vec<u64> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return Vec::new();
        };
        let mut ids = Vec::new();
        for shard in &actors.shards {
            shard.access(|map| {
                if let Some(m) = map.as_ref() {
                    ids.extend(m.keys().copied());
                }
            });
        }
        ids
    }

    /// Take all currently tracked actors out of the live map.
    pub(crate) fn drain_all_for_cleanup() -> HashMap<u64, ActorPtr> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return HashMap::new();
        };
        let mut drained = HashMap::new();
        for shard in &actors.shards {
            let taken = shard.access(|map| match map.as_mut() {
                Some(m) => {
                    let entries = std::mem::take(m);
                    actors.len.fetch_sub(entries.len(), Ordering::AcqRel);
                    entries
                }
                None => FxHashMap::default(),
            });
            for (actor_id, entry) in taken {
                if !entry.actor.0.is_null() {
                    retire_actor_route(entry.actor.0, actor_id);
                }
                drained.insert(actor_id, entry.actor);
            }
        }
        drained
    }

    pub(crate) fn push_deferred_teardown_thread(handle: std::thread::JoinHandle<()>) {
        let finished = rt_current()
            .live_actors
            .deferred_teardown_threads
            .access(|threads| {
                threads.push(handle);
                let mut finished = Vec::new();
                for i in (0..threads.len()).rev() {
                    if threads[i].is_finished() {
                        finished.push(threads.swap_remove(i));
                    }
                }
                finished
            });
        for handle in finished {
            crate::util::report_join_panic("deferred teardown thread", handle.join());
        }
    }

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
                crate::util::report_join_panic("deferred teardown thread", handle.join());
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn deferred_teardown_thread_count() -> usize {
        rt_current()
            .live_actors
            .deferred_teardown_threads
            .access(|threads| threads.len())
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) use native::deferred_teardown_thread_count;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) use native::{
    drain_all_for_cleanup, drain_deferred_teardown_threads, has_drain_blocking_suspended_actor,
    push_deferred_teardown_thread, snapshot_live_actor_ids, snapshot_live_actor_ptrs,
};

#[cfg(target_arch = "wasm32")]
mod wasm_guest {
    use super::{ActorIncarnation, Ordering};

    /// Retained guest snapshots require every actor turn and its cleanup to finish.
    pub(crate) fn has_active_guest_actor() -> bool {
        use crate::internal::types::HewActorState;

        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return false;
        };
        for shard in &actors.shards {
            let active = shard.access(|map| {
                map.as_ref().is_some_and(|entries| {
                    entries.values().any(|entry| {
                        // SAFETY: the liveness shard retains the allocation during
                        // this inspection; the guest has one execution thread.
                        let actor = unsafe { &*entry.actor.0 };
                        let state = actor.actor_state.load(Ordering::Acquire);
                        ![
                            HewActorState::Idle,
                            HewActorState::Stopped,
                            HewActorState::Crashed,
                        ]
                        .iter()
                        .any(|idle| state == *idle as i32)
                            || !actor.checked_invocation.load(Ordering::Acquire).is_null()
                    })
                })
            });
            if active {
                return true;
            }
        }
        false
    }

    /// Copy exact incarnations before cooperative cleanup advances the driver.
    pub(crate) fn snapshot_live_incarnations() -> Vec<ActorIncarnation> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return Vec::new();
        };
        let mut incarnations = Vec::new();
        for shard in &actors.shards {
            shard.access(|map| {
                if let Some(entries) = map.as_ref() {
                    incarnations.extend(entries.values().map(|entry| {
                        // SAFETY: the shard lock retains this tracked allocation.
                        unsafe { ActorIncarnation::of(entry.actor.0) }
                    }));
                }
            });
        }
        incarnations
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) use wasm_guest::{has_active_guest_actor, snapshot_live_incarnations};

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

    /// `push_deferred_teardown_thread` must opportunistically reap handles for
    /// threads that have already finished, rather than only growing the vector
    /// forever until runtime shutdown.
    #[test]
    fn push_deferred_teardown_thread_reaps_already_finished_handles() {
        use std::sync::atomic::AtomicBool;
        use std::sync::Arc;

        let _rt = crate::scheduler::NoWorkerSchedulerForTest::install();

        // Push three threads that return immediately. By the time each
        // subsequent push happens, the prior ones have almost certainly
        // already finished — but to make the assertion deterministic rather
        // than timing-dependent, explicitly wait for each one to report
        // `is_finished()` before pushing the next, so the *next* push's
        // opportunistic reap is guaranteed to see it as finished.
        for _ in 0..3 {
            let handle = std::thread::spawn(|| {});
            while !handle.is_finished() {
                std::thread::yield_now();
            }
            push_deferred_teardown_thread(handle);
        }

        // A fourth push (still-finished) must reap all three prior finished
        // handles as well as itself once it too becomes finished, leaving none
        // behind. Wait for it the same way, then push a fifth, gated-open
        // thread that must NOT be reaped while it is still running.
        let handle = std::thread::spawn(|| {});
        while !handle.is_finished() {
            std::thread::yield_now();
        }
        push_deferred_teardown_thread(handle);
        assert_eq!(
            deferred_teardown_thread_count(),
            0,
            "all four already-finished handles must have been reaped by the \
             time the fourth was pushed, since each prior push waited for \
             is_finished() before proceeding"
        );

        let release = Arc::new(AtomicBool::new(false));
        let release2 = Arc::clone(&release);
        push_deferred_teardown_thread(std::thread::spawn(move || {
            while !release2.load(Ordering::Acquire) {
                std::thread::sleep(std::time::Duration::from_millis(2));
            }
        }));

        // Push one more already-finished handle. The reap inside THIS push
        // must not touch the still-running gated thread pushed just above.
        let handle = std::thread::spawn(|| {});
        while !handle.is_finished() {
            std::thread::yield_now();
        }
        push_deferred_teardown_thread(handle);
        assert_eq!(
            deferred_teardown_thread_count(),
            1,
            "the still-running gated thread must be retained, not reaped, \
             while genuinely still running"
        );

        // Release the gate and confirm the shutdown-path full drain still
        // joins it correctly (the opportunistic reap must not have broken the
        // existing join-before-free barrier).
        release.store(true, Ordering::Release);
        drain_deferred_teardown_threads();
        assert_eq!(deferred_teardown_thread_count(), 0);
    }

    /// Multi-threaded stress test covering all 16 shards: concurrent tracking,
    /// pinning, pointer/ID lookups, slot swaps against drain scans, untracking,
    /// and final cross-shard cleanup drain must maintain exact counts with zero
    /// deadlock.
    #[test]
    fn sharded_live_actors_concurrent_stress_across_all_shards() {
        use std::sync::{Arc, Barrier};

        const THREADS: usize = 8;
        const ACTORS_PER_THREAD: usize = 32; // spans all 16 shards twice per thread

        let _rt = crate::scheduler::NoWorkerSchedulerForTest::install();
        assert_eq!(actor_count_for_test(), 0);

        // Allocate actors up front so raw pointers outlive every worker thread.
        let mut owned: Vec<Box<HewActor>> = Vec::with_capacity(THREADS * ACTORS_PER_THREAD);
        for idx in 0..(THREADS * ACTORS_PER_THREAD) {
            let mut actor = Box::new(crate::test_actor::stub_actor());
            actor.id = (idx as u64) + 1;
            actor.spawn_serial = actor.id;
            owned.push(actor);
        }

        let ptrs: Arc<Vec<usize>> = Arc::new(
            owned
                .iter_mut()
                .map(|actor| (&raw mut **actor) as usize)
                .collect(),
        );
        let start = Arc::new(Barrier::new(THREADS + 1));

        let mut workers = Vec::with_capacity(THREADS);
        for tid in 0..THREADS {
            let ptrs = Arc::clone(&ptrs);
            let start = Arc::clone(&start);
            workers.push(std::thread::spawn(move || {
                let base = tid * ACTORS_PER_THREAD;
                start.wait();
                for offset in 0..ACTORS_PER_THREAD {
                    let raw = ptrs[base + offset] as *mut HewActor;
                    let id = (base + offset) as u64 + 1;
                    // SAFETY: `owned` remains live until all threads join.
                    assert!(unsafe { track_actor(raw) });
                    // Duplicate registration on the same shard must fail closed.
                    // SAFETY: as above.
                    assert!(!unsafe { track_actor(raw) });

                    assert!(is_actor_live(raw));
                    assert!(is_actor_live_with_id(id, raw));
                    assert_eq!(get_actor_ptr_by_id(id), Some(raw));

                    let pin = pin_actor_by_id(id).expect("tracked actor must pin");
                    assert_eq!(pin.actor().id, id);
                    drop(pin);

                    // Exercise shard-serialized slot swap concurrently with scans.
                    // SAFETY: `raw` is live until threads join.
                    let slot = unsafe { &(*raw).parked_ask_channel };
                    let _ = swap_slot_under_registry_lock(id, slot);

                    // Leave half of each thread's actors tracked for drain_all_for_cleanup.
                    if offset % 2 == 0 {
                        assert!(untrack_actor(raw));
                        assert!(!is_actor_live_with_id(id, raw));
                    }
                }
            }));
        }

        // Concurrent whole-registry scans while workers mutate all 16 shards.
        start.wait();
        for _ in 0..64 {
            let _ = has_drain_blocking_suspended_actor();
            let _ = snapshot_live_actor_ids();
        }

        for worker in workers {
            worker.join().expect("live_actors stress worker panicked");
        }

        let expected_remaining = (THREADS * ACTORS_PER_THREAD) / 2;
        assert_eq!(actor_count_for_test(), expected_remaining);
        assert_eq!(snapshot_live_actor_ptrs().len(), expected_remaining);

        let drained = drain_all_for_cleanup();
        assert_eq!(drained.len(), expected_remaining);
        assert_eq!(actor_count_for_test(), 0);
        assert!(snapshot_live_actor_ids().is_empty());
    }
}
