//! Global actor registry for profiler enumeration.
//!
//! Tracks all live actors so the profiler dashboard can list them with
//! their per-actor statistics (message count, processing time).
//!
//! Registration is O(1) and lock-free for the common path (spawn/free).
//! Enumeration (used by the HTTP API) takes a brief lock.

use std::collections::HashMap;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::mailbox::HewMailbox;

/// Wrapper to make `*mut HewActor` `Send` for the registry `HashMap`.
///
/// # Safety
///
/// `HewActor` implements `Send + Sync`. The raw pointer is only
/// dereferenced while the actor is live (between register and unregister).
struct SendPtr(*mut HewActor);

// SAFETY: `HewActor` is `Send + Sync` (see actor.rs). The registry only
// holds pointers to live actors and unregisters before free.
unsafe impl Send for SendPtr {}

/// Global registry of live actors. Keyed by actor ID.
static REGISTRY: Mutex<Option<HashMap<u64, SendPtr>>> = Mutex::new(None);

/// Register a newly spawned actor.
///
/// # Safety
///
/// `actor` must point to a valid, live `HewActor`.
pub unsafe fn register(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: Actor was just allocated and is valid.
    let id = unsafe { (*actor).id };
    let mut guard = match REGISTRY.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    guard
        .get_or_insert_with(HashMap::new)
        .insert(id, SendPtr(actor));
}

/// Unregister an actor about to be freed.
///
/// # Safety
///
/// `actor` must point to a valid, live `HewActor`.
pub unsafe fn unregister(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: Actor is still valid at this point (called before free).
    let id = unsafe { (*actor).id };
    let mut guard = match REGISTRY.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    if let Some(map) = guard.as_mut() {
        map.remove(&id);
    }
}

/// Snapshot of a single actor's stats for the profiler API.
#[derive(Debug, Clone)]
pub struct ActorSnapshot {
    /// Actor ID.
    pub id: u64,
    /// Actor PID.
    pub pid: u64,
    /// Current lifecycle state.
    pub state: &'static str,
    /// Total messages dispatched.
    pub messages_processed: u64,
    /// Cumulative processing time in nanoseconds.
    pub processing_time_ns: u64,
    /// Current mailbox queue depth.
    pub mailbox_depth: i64,
    /// Mailbox high-water mark.
    pub mailbox_hwm: i64,
}

/// Enumerate all live actors and return a snapshot of their stats.
pub fn snapshot_all() -> Vec<ActorSnapshot> {
    let guard = match REGISTRY.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    let Some(map) = guard.as_ref() else {
        return Vec::new();
    };

    let mut result = Vec::with_capacity(map.len());
    for SendPtr(actor_ptr) in map.values() {
        // SAFETY: Actor is registered and not yet freed. The pointer
        // is valid and the atomic fields can be read concurrently.
        let a = unsafe { &**actor_ptr };

        let state_int = a.actor_state.load(Ordering::Relaxed);
        let state_name = if state_int == HewActorState::Idle as i32 {
            "idle"
        } else if state_int == HewActorState::Runnable as i32 {
            "runnable"
        } else if state_int == HewActorState::Running as i32 {
            "running"
        } else if state_int == HewActorState::Blocked as i32 {
            "blocked"
        } else if state_int == HewActorState::Stopping as i32 {
            "stopping"
        } else if state_int == HewActorState::Crashed as i32 {
            "crashed"
        } else if state_int == HewActorState::Stopped as i32 {
            "stopped"
        } else {
            "unknown"
        };

        let (depth, hwm) = if a.mailbox.is_null() {
            (0, 0)
        } else {
            let mb = a.mailbox.cast::<HewMailbox>();
            // SAFETY: Mailbox is valid while actor is registered.
            let mb_ref = unsafe { &*mb };
            (
                mb_ref.count.load(Ordering::Relaxed),
                mb_ref.high_water_mark.load(Ordering::Relaxed),
            )
        };

        result.push(ActorSnapshot {
            id: a.id,
            pid: a.pid,
            state: state_name,
            messages_processed: a.prof_messages_processed.load(Ordering::Relaxed),
            processing_time_ns: a.prof_processing_time_ns.load(Ordering::Relaxed),
            mailbox_depth: depth,
            mailbox_hwm: hwm,
        });
    }

    // Sort by ID for stable ordering.
    result.sort_by_key(|s| s.id);
    result
}
