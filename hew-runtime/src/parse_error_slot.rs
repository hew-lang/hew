//! Per-actor parse-error slot.
//!
//! Hew actors run on a pooled, work-stealing cooperative scheduler.  An actor
//! that calls a parser and is then parked may be resumed on a different OS
//! thread.  A plain `thread_local!` error slot would therefore drift: the
//! `*_last_error` accessor on the new thread would read either an empty slot
//! or another actor's error.
//!
//! This module provides a slot keyed by **logical actor identity**, not OS
//! thread identity:
//!
//! - When a Hew actor is currently dispatched (`hew_actor_current_id() >= 0`),
//!   the slot is stored in a process-wide `Mutex<HashMap<u64, String>>` keyed
//!   by the actor ID.
//! - When called from outside any actor (main, test, blocking-pool helper),
//!   `hew_actor_current_id()` returns -1 and the slot falls back to a
//!   `thread_local!` so that non-actor callers remain isolated from each other.
//!
//! # Slot lifecycle
//!
//! Entries in the actor map are removed when [`clear_parse_error`] is called
//! on a successful parse (the parser already clears on success).  Long-running
//! actors that fail repeatedly accumulate at most one entry each.  Full
//! reaping on actor death is deferred: no actor-destruction hook is wired
//! here yet.
//!
//! # WHY this module exists (marking the approximation)
//!
//! SHIM: uses an actor-ID-keyed global map rather than attaching the error
//! directly to the call (out-param, Strategy A).  Strategy A would require
//! changing the FFI signatures and all `.hew` bindings; that refactor is
//! deferred.  This shim is correct for all single-threaded, actor, and
//! non-actor contexts.  It becomes obsolete if the FFI surface is ever
//! changed to accept an out-parameter for the parse error.

use std::collections::HashMap;
use std::sync::Mutex;

// ── Process-wide actor-keyed error map ──────────────────────────────────────

/// Global map from actor ID → last parse error message.
///
/// Only populated when `hew_actor_current_id()` returns a non-negative value.
static ACTOR_PARSE_ERRORS: Mutex<Option<HashMap<u64, String>>> = Mutex::new(None);

fn actor_map_set(actor_id: u64, msg: String) {
    let mut guard = ACTOR_PARSE_ERRORS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    guard.get_or_insert_with(HashMap::new).insert(actor_id, msg);
}

fn actor_map_get(actor_id: u64) -> Option<String> {
    let guard = ACTOR_PARSE_ERRORS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    guard.as_ref()?.get(&actor_id).cloned()
}

fn actor_map_clear(actor_id: u64) {
    let mut guard = ACTOR_PARSE_ERRORS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if let Some(map) = guard.as_mut() {
        map.remove(&actor_id);
    }
}

// ── Per-thread fallback (non-actor callers) ─────────────────────────────────

thread_local! {
    /// Error slot for callers outside any Hew actor context.
    static THREAD_PARSE_ERROR: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

// ── Public interface ─────────────────────────────────────────────────────────

/// Record `msg` as the most recent parse error for the current logical context
/// (actor or thread).
pub fn set_parse_error(msg: impl Into<String>) {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_set(id as u64, msg.into());
    } else {
        THREAD_PARSE_ERROR.with(|slot| *slot.borrow_mut() = Some(msg.into()));
    }
}

/// Clear the parse error for the current logical context.
pub fn clear_parse_error() {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_clear(id as u64);
    } else {
        THREAD_PARSE_ERROR.with(|slot| *slot.borrow_mut() = None);
    }
}

/// Return (and clone) the last parse error for the current logical context,
/// or `None` if no error has been set.
#[must_use]
pub fn get_parse_error() -> Option<String> {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_get(id as u64)
    } else {
        THREAD_PARSE_ERROR.with(|slot| slot.borrow().clone())
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// A slot set and retrieved within the same non-actor thread returns the
    /// same string.
    #[test]
    fn thread_fallback_set_get_clear() {
        set_parse_error("test error");
        assert_eq!(get_parse_error().as_deref(), Some("test error"));
        clear_parse_error();
        assert_eq!(get_parse_error(), None);
    }

    /// A slot written on thread A for actor-id X is visible on thread B when
    /// actor-id X is active — simulating an actor migrating across workers.
    ///
    /// This is the core cross-thread regression for issue #1420.
    ///
    /// Thread A sets the error while "running as actor 99".
    /// Thread B reads the error while "running as actor 99".
    /// Both threads reach their synchronisation points without any sleep or
    /// timing assumption; the barrier provides the only ordering guarantee.
    #[test]
    fn actor_keyed_error_survives_thread_migration() {
        // We cannot inject actor context via the scheduler in a unit test, so
        // we write directly to the actor map (same internal path that
        // `set_parse_error` takes when actor_id >= 0) and read it back from a
        // different OS thread.  This is not the same as exercising
        // hew_actor_current_id(), but it is the precise path the three
        // parsers travel when the actor has a non-negative id.
        const ACTOR_ID: u64 = 999_999;

        // Barrier to ensure thread B reads only after thread A writes.
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let barrier2 = barrier.clone();

        let handle = std::thread::spawn(move || {
            // Simulate: actor 999_999 runs on thread B, reads the error slot.
            barrier2.wait(); // wait until thread A has written
            actor_map_get(ACTOR_ID)
        });

        // Thread A: write the error as if running actor 999_999.
        actor_map_set(ACTOR_ID, "cross-thread error".to_owned());
        barrier.wait(); // release thread B

        let result = handle.join().expect("thread B panicked");
        assert_eq!(result.as_deref(), Some("cross-thread error"));

        // Cleanup to avoid polluting other tests.
        actor_map_clear(ACTOR_ID);
    }

    /// Two different actor IDs have independent error slots.
    #[test]
    fn separate_actor_ids_have_independent_slots() {
        const A: u64 = 1_000_001;
        const B: u64 = 1_000_002;

        actor_map_set(A, "error for A".to_owned());
        actor_map_set(B, "error for B".to_owned());

        assert_eq!(actor_map_get(A).as_deref(), Some("error for A"));
        assert_eq!(actor_map_get(B).as_deref(), Some("error for B"));

        actor_map_clear(A);
        actor_map_clear(B);
    }
}
