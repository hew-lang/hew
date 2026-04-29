//! Per-actor, per-error-kind slot.
//!
//! Hew actors run on a pooled, work-stealing cooperative scheduler.  An actor
//! that calls a parser and is then parked may be resumed on a different OS
//! thread.  A plain `thread_local!` error slot would therefore drift: the
//! `*_last_error` accessor on the new thread would read either an empty slot
//! or another actor's error.
//!
//! This module provides a slot keyed by **(logical actor identity, error kind)**,
//! not OS thread identity:
//!
//! - When a Hew actor is currently dispatched (`hew_actor_current_id() >= 0`),
//!   the slot is stored in a process-wide `Mutex<HashMap<(u64, ErrorSlotKind), String>>`
//!   keyed by the actor ID and error kind.  Each error type has its own per-actor
//!   slot, so a yaml error cannot clear a datetime error and vice-versa.
//! - When called from outside any actor (main, test, blocking-pool helper),
//!   `hew_actor_current_id()` returns -1 and the slot falls back to a
//!   `thread_local!` `HashMap<ErrorSlotKind, String>` so that non-actor callers
//!   remain isolated from each other and from other error types.
//!
//! # Slot lifecycle
//!
//! Entries in the actor map are removed:
//! - On each successful operation (the caller invokes [`clear_error`]).
//! - On actor death, via [`clear_all_for_actor`], which is called from
//!   `hew_actor_free` / `cleanup_all_actors` through
//!   `actor::prepare_quiescent_actor_for_cleanup`.
//!
//! # WHY this module exists (marking the approximation)
//!
//! SHIM: uses an actor-ID-keyed global map rather than attaching the error
//! directly to the call (out-param, Strategy A).  Strategy A would require
//! changing the FFI signatures and all `.hew` bindings; that refactor is
//! deferred.  This shim is correct for all single-threaded, actor, and
//! non-actor contexts.  It becomes obsolete if the FFI surface is ever
//! changed to accept an out-parameter for the error.

use std::collections::HashMap;

use crate::lifetime::PoisonSafe;

// ── Parser discriminant ──────────────────────────────────────────────────────

/// Identifies which error type owns an error slot entry.
///
/// Adding a new error kind requires: (1) adding a variant here, (2) passing it at
/// every `set_error` / `clear_error` / `get_error` call site,
/// and (3) calling `clear_all_for_actor` on actor death (already wired).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorSlotKind {
    Datetime,
    Yaml,
    Toml,
    Json,
    Cron,
    Compress,
    Msgpack,
    Xml,
    Quic,
    Tls,
    Smtp,
    Http,
    Jwt,
}

// ── Process-wide actor-keyed error map ──────────────────────────────────────

/// Global map from `(actor_id, ErrorSlotKind)` → last error message.
///
/// Only populated when `hew_actor_current_id()` returns a non-negative value.
static ACTOR_PARSE_ERRORS: PoisonSafe<Option<HashMap<(u64, ErrorSlotKind), String>>> =
    PoisonSafe::new(None);

fn actor_map_set(actor_id: u64, kind: ErrorSlotKind, msg: String) {
    ACTOR_PARSE_ERRORS.access(|guard| {
        guard
            .get_or_insert_with(HashMap::new)
            .insert((actor_id, kind), msg);
    });
}

fn actor_map_get(actor_id: u64, kind: ErrorSlotKind) -> Option<String> {
    ACTOR_PARSE_ERRORS.access(|guard| guard.as_ref()?.get(&(actor_id, kind)).cloned())
}

fn actor_map_clear(actor_id: u64, kind: ErrorSlotKind) {
    ACTOR_PARSE_ERRORS.access(|guard| {
        if let Some(map) = guard.as_mut() {
            map.remove(&(actor_id, kind));
        }
    });
}

fn actor_map_clear_all_for_actor(actor_id: u64) {
    ACTOR_PARSE_ERRORS.access(|guard| {
        if let Some(map) = guard.as_mut() {
            map.retain(|&(id, _), _| id != actor_id);
        }
    });
}

// ── Per-thread fallback (non-actor callers) ─────────────────────────────────

thread_local! {
    /// Per-error-kind slot for callers outside any Hew actor context.
    ///
    /// Keyed by [`ErrorSlotKind`] so that non-actor callers cannot alias each
    /// other's errors across error types, matching the per-actor-map invariant.
    static THREAD_PARSE_ERROR: std::cell::RefCell<HashMap<ErrorSlotKind, String>> =
        std::cell::RefCell::new(HashMap::new());
}

// ── Public interface ─────────────────────────────────────────────────────────

/// Record `msg` as the most recent error for `kind` in the current
/// logical context (actor or thread).
pub fn set_error(kind: ErrorSlotKind, msg: impl Into<String>) {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_set(id as u64, kind, msg.into());
    } else {
        THREAD_PARSE_ERROR.with(|slot| {
            slot.borrow_mut().insert(kind, msg.into());
        });
    }
}

/// Clear the error for `kind` in the current logical context.
pub fn clear_error(kind: ErrorSlotKind) {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_clear(id as u64, kind);
    } else {
        THREAD_PARSE_ERROR.with(|slot| {
            slot.borrow_mut().remove(&kind);
        });
    }
}

/// Return (and clone) the last error for `kind` in the current logical
/// context, or `None` if no error has been set.
#[must_use]
pub fn get_error(kind: ErrorSlotKind) -> Option<String> {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_get(id as u64, kind)
    } else {
        THREAD_PARSE_ERROR.with(|slot| slot.borrow().get(&kind).cloned())
    }
}

/// Remove all error entries for `actor_id` across every [`ErrorSlotKind`].
///
/// Called from `actor::prepare_quiescent_actor_for_cleanup` on actor death so
/// that long-running nodes do not accumulate entries for reaped actors.
pub fn clear_all_for_actor(actor_id: u64) {
    actor_map_clear_all_for_actor(actor_id);
}

// ── Test helpers (cross-crate, doc-hidden) ───────────────────────────────────
//
// These helpers let integration tests in other crates (e.g. hew-std-encoding-toml)
// inject and inspect actor-keyed errors without a running scheduler.  They are
// not part of the public API: they are `#[doc(hidden)]` and prefixed with `__`
// to signal test-only intent without gating behind `#[cfg(test)]` (which would
// make them invisible to the other crates' test compilations).

/// Write an error directly for a specific actor ID and error kind.
///
/// Intended for integration tests that cannot inject actor context via the
/// scheduler.  Callers must clean up with [`__clear_error_for_actor`]
/// to avoid polluting other tests.
#[doc(hidden)]
pub fn __set_error_for_actor(actor_id: u64, kind: ErrorSlotKind, msg: impl Into<String>) {
    actor_map_set(actor_id, kind, msg.into());
}

/// Read the error for a specific actor ID and error kind.
///
/// Intended for integration tests.
#[doc(hidden)]
#[must_use]
pub fn __get_error_for_actor(actor_id: u64, kind: ErrorSlotKind) -> Option<String> {
    actor_map_get(actor_id, kind)
}

/// Clear the error for a specific actor ID and error kind.
///
/// Intended for integration tests.
#[doc(hidden)]
pub fn __clear_error_for_actor(actor_id: u64, kind: ErrorSlotKind) {
    actor_map_clear(actor_id, kind);
}

/// Clear all errors for a specific actor ID (all error kinds).
///
/// Intended for integration tests.
#[doc(hidden)]
pub fn __clear_all_errors_for_actor(actor_id: u64) {
    actor_map_clear_all_for_actor(actor_id);
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// A slot set and retrieved within the same non-actor thread returns the
    /// same string for the same error kind.
    #[test]
    fn thread_fallback_set_get_clear() {
        set_error(ErrorSlotKind::Toml, "test error");
        assert_eq!(
            get_error(ErrorSlotKind::Toml).as_deref(),
            Some("test error")
        );
        clear_error(ErrorSlotKind::Toml);
        assert_eq!(get_error(ErrorSlotKind::Toml), None);
    }

    /// Different error kinds have independent TLS slots — a yaml error does not
    /// clear or overwrite a datetime error.
    #[test]
    fn thread_fallback_parsers_are_independent() {
        // Use large IDs unlikely to collide with other test state.
        set_error(ErrorSlotKind::Datetime, "datetime error");
        set_error(ErrorSlotKind::Yaml, "yaml error");

        assert_eq!(
            get_error(ErrorSlotKind::Datetime).as_deref(),
            Some("datetime error"),
            "datetime error should not be overwritten by yaml write"
        );
        assert_eq!(
            get_error(ErrorSlotKind::Yaml).as_deref(),
            Some("yaml error"),
        );

        // Clearing yaml must not touch datetime.
        clear_error(ErrorSlotKind::Yaml);
        assert_eq!(
            get_error(ErrorSlotKind::Datetime).as_deref(),
            Some("datetime error"),
            "datetime error must survive yaml clear"
        );

        clear_error(ErrorSlotKind::Datetime);
    }

    /// A slot written on thread A for actor-id X is visible on thread B when
    /// actor-id X is active — simulating an actor migrating across workers.
    ///
    /// This is the core cross-thread regression for issue #1420.
    ///
    /// Native-only: `wasm32-wasip1` has no preemptive thread support, so the
    /// migration this test simulates is meaningless on that target. The
    /// invariant the test guards still holds on WASM (the slot is keyed by
    /// `(actor_id, kind)`, not by thread identity); native coverage is enough.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn actor_keyed_error_survives_thread_migration() {
        const ACTOR_ID: u64 = 999_999;

        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let barrier2 = barrier.clone();

        let handle = std::thread::spawn(move || {
            barrier2.wait();
            actor_map_get(ACTOR_ID, ErrorSlotKind::Toml)
        });

        actor_map_set(
            ACTOR_ID,
            ErrorSlotKind::Toml,
            "cross-thread error".to_owned(),
        );
        barrier.wait();

        let result = handle.join().expect("thread B panicked");
        assert_eq!(result.as_deref(), Some("cross-thread error"));

        actor_map_clear(ACTOR_ID, ErrorSlotKind::Toml);
    }

    /// Two different actor IDs have independent error slots for the same parser.
    #[test]
    fn separate_actor_ids_have_independent_slots() {
        const A: u64 = 1_000_001;
        const B: u64 = 1_000_002;

        actor_map_set(A, ErrorSlotKind::Yaml, "error for A".to_owned());
        actor_map_set(B, ErrorSlotKind::Yaml, "error for B".to_owned());

        assert_eq!(
            actor_map_get(A, ErrorSlotKind::Yaml).as_deref(),
            Some("error for A")
        );
        assert_eq!(
            actor_map_get(B, ErrorSlotKind::Yaml).as_deref(),
            Some("error for B")
        );

        actor_map_clear(A, ErrorSlotKind::Yaml);
        actor_map_clear(B, ErrorSlotKind::Yaml);
    }

    /// Actor-map slots for different parsers on the same actor are independent.
    /// Writing a Json error must not clear or overwrite a Toml error for the
    /// same actor.
    #[test]
    fn actor_cross_parser_slots_are_independent() {
        const ACTOR_ID: u64 = 2_000_001;

        actor_map_set(ACTOR_ID, ErrorSlotKind::Toml, "toml error".to_owned());
        actor_map_set(ACTOR_ID, ErrorSlotKind::Json, "json error".to_owned());
        actor_map_set(
            ACTOR_ID,
            ErrorSlotKind::Yaml,
            "yaml success — cleared".to_owned(),
        );

        // Simulating yaml success path clearing its slot.
        actor_map_clear(ACTOR_ID, ErrorSlotKind::Yaml);

        // Toml and Json errors must survive the yaml clear.
        assert_eq!(
            actor_map_get(ACTOR_ID, ErrorSlotKind::Toml).as_deref(),
            Some("toml error"),
            "toml error must survive yaml clear"
        );
        assert_eq!(
            actor_map_get(ACTOR_ID, ErrorSlotKind::Json).as_deref(),
            Some("json error"),
            "json error must survive yaml clear"
        );
        assert_eq!(
            actor_map_get(ACTOR_ID, ErrorSlotKind::Yaml),
            None,
            "yaml slot must be cleared"
        );

        // clear_all_for_actor must remove all error kinds.
        actor_map_clear_all_for_actor(ACTOR_ID);
        assert_eq!(actor_map_get(ACTOR_ID, ErrorSlotKind::Toml), None);
        assert_eq!(actor_map_get(ACTOR_ID, ErrorSlotKind::Json), None);
    }

    /// `clear_all_for_actor` removes entries for the target actor without
    /// touching entries for other actors.
    #[test]
    fn clear_all_for_actor_does_not_touch_other_actors() {
        const TARGET: u64 = 3_000_001;
        const OTHER: u64 = 3_000_002;

        actor_map_set(
            TARGET,
            ErrorSlotKind::Datetime,
            "target datetime".to_owned(),
        );
        actor_map_set(TARGET, ErrorSlotKind::Json, "target json".to_owned());
        actor_map_set(OTHER, ErrorSlotKind::Toml, "other toml".to_owned());

        actor_map_clear_all_for_actor(TARGET);

        assert_eq!(actor_map_get(TARGET, ErrorSlotKind::Datetime), None);
        assert_eq!(actor_map_get(TARGET, ErrorSlotKind::Json), None);
        assert_eq!(
            actor_map_get(OTHER, ErrorSlotKind::Toml).as_deref(),
            Some("other toml"),
            "unrelated actor entry must survive"
        );

        actor_map_clear(OTHER, ErrorSlotKind::Toml);
    }
}
