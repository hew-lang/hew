//! Per-actor, per-parser parse-error slot.
//!
//! Hew actors run on a pooled, work-stealing cooperative scheduler.  An actor
//! that calls a parser and is then parked may be resumed on a different OS
//! thread.  A plain `thread_local!` error slot would therefore drift: the
//! `*_last_error` accessor on the new thread would read either an empty slot
//! or another actor's error.
//!
//! This module provides a slot keyed by **(logical actor identity, parser kind)**,
//! not OS thread identity:
//!
//! - When a Hew actor is currently dispatched (`hew_actor_current_id() >= 0`),
//!   the slot is stored in a process-wide `Mutex<HashMap<(u64, ParserKind), String>>`
//!   keyed by the actor ID and parser kind.  Each parser has its own per-actor
//!   slot, so a yaml success cannot clear a datetime error and vice-versa.
//! - When called from outside any actor (main, test, blocking-pool helper),
//!   `hew_actor_current_id()` returns -1 and the slot falls back to a
//!   `thread_local!` `HashMap<ParserKind, String>` so that non-actor callers
//!   remain isolated from each other and from other parsers.
//!
//! # Slot lifecycle
//!
//! Entries in the actor map are removed:
//! - On each successful parse (the caller invokes [`clear_parse_error`]).
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
//! changed to accept an out-parameter for the parse error.

use std::collections::HashMap;
use std::sync::Mutex;

use crate::util::MutexExt;

// ── Parser discriminant ──────────────────────────────────────────────────────

/// Identifies which parser owns a parse-error slot entry.
///
/// Adding a new parser requires: (1) adding a variant here, (2) passing it at
/// every `set_parse_error` / `clear_parse_error` / `get_parse_error` call site,
/// and (3) calling `clear_all_for_actor` on actor death (already wired).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParserKind {
    Datetime,
    Yaml,
    Toml,
    Json,
}

// ── Process-wide actor-keyed error map ──────────────────────────────────────

/// Global map from `(actor_id, ParserKind)` → last parse error message.
///
/// Only populated when `hew_actor_current_id()` returns a non-negative value.
static ACTOR_PARSE_ERRORS: Mutex<Option<HashMap<(u64, ParserKind), String>>> = Mutex::new(None);

fn actor_map_set(actor_id: u64, kind: ParserKind, msg: String) {
    let mut guard = ACTOR_PARSE_ERRORS.lock_or_recover();
    guard
        .get_or_insert_with(HashMap::new)
        .insert((actor_id, kind), msg);
}

fn actor_map_get(actor_id: u64, kind: ParserKind) -> Option<String> {
    let guard = ACTOR_PARSE_ERRORS.lock_or_recover();
    guard.as_ref()?.get(&(actor_id, kind)).cloned()
}

fn actor_map_clear(actor_id: u64, kind: ParserKind) {
    let mut guard = ACTOR_PARSE_ERRORS.lock_or_recover();
    if let Some(map) = guard.as_mut() {
        map.remove(&(actor_id, kind));
    }
}

fn actor_map_clear_all_for_actor(actor_id: u64) {
    let mut guard = ACTOR_PARSE_ERRORS.lock_or_recover();
    if let Some(map) = guard.as_mut() {
        map.retain(|&(id, _), _| id != actor_id);
    }
}

// ── Per-thread fallback (non-actor callers) ─────────────────────────────────

thread_local! {
    /// Per-parser error slot for callers outside any Hew actor context.
    ///
    /// Keyed by [`ParserKind`] so that non-actor callers cannot alias each
    /// other's errors across parsers, matching the per-actor-map invariant.
    static THREAD_PARSE_ERROR: std::cell::RefCell<HashMap<ParserKind, String>> =
        std::cell::RefCell::new(HashMap::new());
}

// ── Public interface ─────────────────────────────────────────────────────────

/// Record `msg` as the most recent parse error for `kind` in the current
/// logical context (actor or thread).
pub fn set_parse_error(kind: ParserKind, msg: impl Into<String>) {
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

/// Clear the parse error for `kind` in the current logical context.
pub fn clear_parse_error(kind: ParserKind) {
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

/// Return (and clone) the last parse error for `kind` in the current logical
/// context, or `None` if no error has been set.
#[must_use]
pub fn get_parse_error(kind: ParserKind) -> Option<String> {
    let id = crate::actor::hew_actor_current_id();
    if id >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "checked: id >= 0")]
        actor_map_get(id as u64, kind)
    } else {
        THREAD_PARSE_ERROR.with(|slot| slot.borrow().get(&kind).cloned())
    }
}

/// Remove all parse-error entries for `actor_id` across every [`ParserKind`].
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

/// Write a parse error directly for a specific actor ID and parser kind.
///
/// Intended for integration tests that cannot inject actor context via the
/// scheduler.  Callers must clean up with [`__clear_parse_error_for_actor`]
/// to avoid polluting other tests.
#[doc(hidden)]
pub fn __set_parse_error_for_actor(actor_id: u64, kind: ParserKind, msg: impl Into<String>) {
    actor_map_set(actor_id, kind, msg.into());
}

/// Read the parse error for a specific actor ID and parser kind.
///
/// Intended for integration tests.
#[doc(hidden)]
#[must_use]
pub fn __get_parse_error_for_actor(actor_id: u64, kind: ParserKind) -> Option<String> {
    actor_map_get(actor_id, kind)
}

/// Clear the parse error for a specific actor ID and parser kind.
///
/// Intended for integration tests.
#[doc(hidden)]
pub fn __clear_parse_error_for_actor(actor_id: u64, kind: ParserKind) {
    actor_map_clear(actor_id, kind);
}

/// Clear all parse errors for a specific actor ID (all parser kinds).
///
/// Intended for integration tests.
#[doc(hidden)]
pub fn __clear_all_parse_errors_for_actor(actor_id: u64) {
    actor_map_clear_all_for_actor(actor_id);
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// A slot set and retrieved within the same non-actor thread returns the
    /// same string for the same parser kind.
    #[test]
    fn thread_fallback_set_get_clear() {
        set_parse_error(ParserKind::Toml, "test error");
        assert_eq!(
            get_parse_error(ParserKind::Toml).as_deref(),
            Some("test error")
        );
        clear_parse_error(ParserKind::Toml);
        assert_eq!(get_parse_error(ParserKind::Toml), None);
    }

    /// Different parser kinds have independent TLS slots — a yaml error does not
    /// clear or overwrite a datetime error.
    #[test]
    fn thread_fallback_parsers_are_independent() {
        // Use large IDs unlikely to collide with other test state.
        set_parse_error(ParserKind::Datetime, "datetime error");
        set_parse_error(ParserKind::Yaml, "yaml error");

        assert_eq!(
            get_parse_error(ParserKind::Datetime).as_deref(),
            Some("datetime error"),
            "datetime error should not be overwritten by yaml write"
        );
        assert_eq!(
            get_parse_error(ParserKind::Yaml).as_deref(),
            Some("yaml error"),
        );

        // Clearing yaml must not touch datetime.
        clear_parse_error(ParserKind::Yaml);
        assert_eq!(
            get_parse_error(ParserKind::Datetime).as_deref(),
            Some("datetime error"),
            "datetime error must survive yaml clear"
        );

        clear_parse_error(ParserKind::Datetime);
    }

    /// A slot written on thread A for actor-id X is visible on thread B when
    /// actor-id X is active — simulating an actor migrating across workers.
    ///
    /// This is the core cross-thread regression for issue #1420.
    #[test]
    fn actor_keyed_error_survives_thread_migration() {
        const ACTOR_ID: u64 = 999_999;

        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let barrier2 = barrier.clone();

        let handle = std::thread::spawn(move || {
            barrier2.wait();
            actor_map_get(ACTOR_ID, ParserKind::Toml)
        });

        actor_map_set(ACTOR_ID, ParserKind::Toml, "cross-thread error".to_owned());
        barrier.wait();

        let result = handle.join().expect("thread B panicked");
        assert_eq!(result.as_deref(), Some("cross-thread error"));

        actor_map_clear(ACTOR_ID, ParserKind::Toml);
    }

    /// Two different actor IDs have independent error slots for the same parser.
    #[test]
    fn separate_actor_ids_have_independent_slots() {
        const A: u64 = 1_000_001;
        const B: u64 = 1_000_002;

        actor_map_set(A, ParserKind::Yaml, "error for A".to_owned());
        actor_map_set(B, ParserKind::Yaml, "error for B".to_owned());

        assert_eq!(
            actor_map_get(A, ParserKind::Yaml).as_deref(),
            Some("error for A")
        );
        assert_eq!(
            actor_map_get(B, ParserKind::Yaml).as_deref(),
            Some("error for B")
        );

        actor_map_clear(A, ParserKind::Yaml);
        actor_map_clear(B, ParserKind::Yaml);
    }

    /// Actor-map slots for different parsers on the same actor are independent.
    /// Writing a Json error must not clear or overwrite a Toml error for the
    /// same actor.
    #[test]
    fn actor_cross_parser_slots_are_independent() {
        const ACTOR_ID: u64 = 2_000_001;

        actor_map_set(ACTOR_ID, ParserKind::Toml, "toml error".to_owned());
        actor_map_set(ACTOR_ID, ParserKind::Json, "json error".to_owned());
        actor_map_set(
            ACTOR_ID,
            ParserKind::Yaml,
            "yaml success — cleared".to_owned(),
        );

        // Simulating yaml success path clearing its slot.
        actor_map_clear(ACTOR_ID, ParserKind::Yaml);

        // Toml and Json errors must survive the yaml clear.
        assert_eq!(
            actor_map_get(ACTOR_ID, ParserKind::Toml).as_deref(),
            Some("toml error"),
            "toml error must survive yaml clear"
        );
        assert_eq!(
            actor_map_get(ACTOR_ID, ParserKind::Json).as_deref(),
            Some("json error"),
            "json error must survive yaml clear"
        );
        assert_eq!(
            actor_map_get(ACTOR_ID, ParserKind::Yaml),
            None,
            "yaml slot must be cleared"
        );

        // clear_all_for_actor must remove all parsers.
        actor_map_clear_all_for_actor(ACTOR_ID);
        assert_eq!(actor_map_get(ACTOR_ID, ParserKind::Toml), None);
        assert_eq!(actor_map_get(ACTOR_ID, ParserKind::Json), None);
    }

    /// `clear_all_for_actor` removes entries for the target actor without
    /// touching entries for other actors.
    #[test]
    fn clear_all_for_actor_does_not_touch_other_actors() {
        const TARGET: u64 = 3_000_001;
        const OTHER: u64 = 3_000_002;

        actor_map_set(TARGET, ParserKind::Datetime, "target datetime".to_owned());
        actor_map_set(TARGET, ParserKind::Json, "target json".to_owned());
        actor_map_set(OTHER, ParserKind::Toml, "other toml".to_owned());

        actor_map_clear_all_for_actor(TARGET);

        assert_eq!(actor_map_get(TARGET, ParserKind::Datetime), None);
        assert_eq!(actor_map_get(TARGET, ParserKind::Json), None);
        assert_eq!(
            actor_map_get(OTHER, ParserKind::Toml).as_deref(),
            Some("other toml"),
            "unrelated actor entry must survive"
        );

        actor_map_clear(OTHER, ParserKind::Toml);
    }
}
