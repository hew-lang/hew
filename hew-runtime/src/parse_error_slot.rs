//! Per-actor, per-error-kind slot.
//!
//! Hew actors run on a pooled, work-stealing cooperative scheduler.  An actor
//! that calls a parser or network FFI entry point and is then parked may be
//! resumed on a different OS thread.  A plain `thread_local!` error slot would
//! therefore drift: the `*_last_error` accessor on the new thread would read
//! either an empty slot or another actor's error.
//!
//! This module stores error slots directly on the active actor when dispatched
//! and falls back to thread-local storage for non-actor threads:
//!
//! - When a Hew actor is currently dispatched
//!   (`!actor::current_actor_silent().is_null()`), the slot is stored in
//!   [`ActorParseErrors`] embedded directly on [`crate::actor::HewActor`].
//!   Each value keeps an errno and detail together so consumers cannot observe
//!   mismatched halves, and each error kind has its own entry so a YAML error
//!   cannot clear an XML error and vice versa.  The inner map is allocated
//!   lazily (`None` until an actor records an error), so actors that never hit
//!   an error pay zero heap overhead and no global lock is ever acquired.
//! - When called from outside any actor (main, test, blocking-pool helper),
//!   `current_actor_silent()` returns null and the slot falls back to a
//!   `thread_local!` map so non-actor callers remain isolated from each other
//!   and across error kinds.
//!
//! The routing decision uses the SILENT probe deliberately: falling back to
//! the thread-local slot is an expected, non-error condition, so it must not
//! write "execution context not installed" into the generic last-error slot
//! and destroy an unrelated real error (#2658).
//!
//! # Slot lifecycle
//!
//! Actor-owned entries are removed:
//! - On each successful operation (the caller invokes [`clear_error`]).
//! - On actor cleanup, via [`ActorParseErrors::clear_all`] from
//!   `actor::prepare_quiescent_actor_for_cleanup`, and automatically when
//!   `HewActor` is dropped.
//!
//! # FFI contract
//!
//! The `*_last_error` C ABI families keep out-of-band error retrieval rather
//! than changing existing FFI signatures to out-parameters; storing the slots
//! directly on `HewActor` gives lock-free cross-actor isolation and automatic
//! lifetime reclamation under that ABI.

use std::collections::HashMap;

use crate::lifetime::PoisonSafe;

// ── Error-kind discriminant ──────────────────────────────────────────────────

/// Identifies which error type owns an error slot entry.
///
/// Adding a new error kind requires: (1) adding a variant here, and (2) passing
/// it at every `set_error` / `clear_error` / `get_error` call site. Actor
/// teardown reclaims every kind automatically via [`ActorParseErrors::clear_all`]
/// and `HewActor`'s drop.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorSlotKind {
    Yaml,
    Toml,
    Json,
    Compress,
    Msgpack,
    Xml,
    Quic,
    Tls,
    Smtp,
    Http,
    Websocket,
    Jwt,
}

// ── Per-actor error storage ─────────────────────────────────────────────────

/// Last coherent error pair (`errno` + human-readable detail).
#[derive(Debug, Clone, PartialEq, Eq)]
struct ErrorSlotValue {
    errno: i64,
    detail: String,
}

/// Actor-owned per-error-kind slot table embedded on [`crate::actor::HewActor`].
///
/// Lazily initialized (`None` until an actor records its first error) so
/// error-free actors incur zero heap allocation. Wrapped in [`PoisonSafe`] so
/// `HewActor: Sync` remains sound under concurrent inspection or teardown.
#[derive(Debug)]
pub(crate) struct ActorParseErrors {
    slots: PoisonSafe<Option<HashMap<ErrorSlotKind, ErrorSlotValue>>>,
}

impl ActorParseErrors {
    /// Construct an empty, unallocated error slot table.
    pub(crate) const fn new() -> Self {
        Self {
            slots: PoisonSafe::new(None),
        }
    }

    fn set_with_errno(&self, kind: ErrorSlotKind, errno: i64, detail: String) {
        self.slots.access(|guard| {
            guard
                .get_or_insert_with(HashMap::new)
                .insert(kind, ErrorSlotValue { errno, detail });
        });
    }

    fn get(&self, kind: ErrorSlotKind) -> Option<String> {
        self.slots
            .access(|guard| guard.as_ref()?.get(&kind).map(|value| value.detail.clone()))
    }

    fn get_errno(&self, kind: ErrorSlotKind) -> i64 {
        self.slots.access(|guard| {
            guard
                .as_ref()
                .and_then(|map| map.get(&kind))
                .map_or(0, |value| value.errno)
        })
    }

    fn clear(&self, kind: ErrorSlotKind) {
        self.slots.access(|guard| {
            if let Some(map) = guard.as_mut() {
                map.remove(&kind);
            }
        });
    }

    /// Release all recorded error entries for this actor.
    pub(crate) fn clear_all(&self) {
        self.slots.access(|guard| {
            *guard = None;
        });
    }
}

// ── Per-thread fallback (non-actor callers) ─────────────────────────────────

thread_local! {
    /// Per-error-kind slot for callers outside any Hew actor context.
    ///
    /// Keyed by [`ErrorSlotKind`] so that non-actor callers cannot alias each
    /// other's errors across error types, matching the per-actor-slot invariant.
    static THREAD_PARSE_ERROR: std::cell::RefCell<HashMap<ErrorSlotKind, ErrorSlotValue>> =
        std::cell::RefCell::new(HashMap::new());
}

// ── Public interface ─────────────────────────────────────────────────────────

/// Record `msg` as the most recent error for `kind` in the current
/// logical context (actor or thread).
pub fn set_error(kind: ErrorSlotKind, msg: impl Into<String>) {
    set_error_with_errno(kind, 0, msg);
}

/// Record an errno and detail together in the current logical context.
///
/// Both values share one actor-aware authority so a scheduler migration cannot
/// pair an errno from one actor with the detail from another.
pub fn set_error_with_errno(kind: ErrorSlotKind, errno: i64, msg: impl Into<String>) {
    let actor = crate::actor::current_actor_silent();
    if actor.is_null() {
        THREAD_PARSE_ERROR.with(|slot| {
            slot.borrow_mut().insert(
                kind,
                ErrorSlotValue {
                    errno,
                    detail: msg.into(),
                },
            );
        });
    } else {
        // SAFETY: `current_actor_silent` reads the installed canonical context's
        // live actor pointer, valid for the duration of this dispatch boundary.
        unsafe { &*actor }
            .parse_errors
            .set_with_errno(kind, errno, msg.into());
    }
}

/// Clear the error for `kind` in the current logical context.
pub fn clear_error(kind: ErrorSlotKind) {
    let actor = crate::actor::current_actor_silent();
    if actor.is_null() {
        THREAD_PARSE_ERROR.with(|slot| {
            slot.borrow_mut().remove(&kind);
        });
    } else {
        // SAFETY: `current_actor_silent` reads the installed canonical context's
        // live actor pointer, valid for the duration of this dispatch boundary.
        unsafe { &*actor }.parse_errors.clear(kind);
    }
}

/// Return (and clone) the last error for `kind` in the current logical
/// context, or `None` if no error has been set.
#[must_use]
pub fn get_error(kind: ErrorSlotKind) -> Option<String> {
    let actor = crate::actor::current_actor_silent();
    if actor.is_null() {
        THREAD_PARSE_ERROR.with(|slot| slot.borrow().get(&kind).map(|value| value.detail.clone()))
    } else {
        // SAFETY: `current_actor_silent` reads the installed canonical context's
        // live actor pointer, valid for the duration of this dispatch boundary.
        unsafe { &*actor }.parse_errors.get(kind)
    }
}

/// Return the errno paired with the last error for `kind`, or 0 when absent.
#[must_use]
pub fn get_errno(kind: ErrorSlotKind) -> i64 {
    let actor = crate::actor::current_actor_silent();
    if actor.is_null() {
        THREAD_PARSE_ERROR.with(|slot| slot.borrow().get(&kind).map_or(0, |value| value.errno))
    } else {
        // SAFETY: `current_actor_silent` reads the installed canonical context's
        // live actor pointer, valid for the duration of this dispatch boundary.
        unsafe { &*actor }.parse_errors.get_errno(kind)
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Read the process-generic last-error slot as an owned string.
    fn generic_last_error() -> Option<String> {
        let ptr = crate::hew_last_error();
        if ptr.is_null() {
            return None;
        }
        // SAFETY: hew_last_error returned a non-null, NUL-terminated C string
        // owned by the thread-local slot; it stays valid until the next write.
        Some(
            unsafe { std::ffi::CStr::from_ptr(ptr) }
                .to_string_lossy()
                .into_owned(),
        )
    }

    /// Force a non-actor context for the duration of a closure, restoring the
    /// previous execution context afterwards.
    fn with_no_execution_context(f: impl FnOnce()) {
        let prev = crate::execution_context::set_current_context(std::ptr::null_mut());
        f();
        let _ = crate::execution_context::set_current_context(prev);
    }

    /// Direct #2658 regression: a non-actor `get_error` fallback lookup is an
    /// expected condition, so it must return `None` for an empty slot AND leave
    /// a prior real generic error intact — not clobber it with
    /// "execution context not installed".
    #[test]
    fn nonactor_get_error_preserves_generic_last_error() {
        with_no_execution_context(|| {
            crate::set_last_error("prior real error");
            assert_eq!(get_error(ErrorSlotKind::Json), None);
            assert_eq!(
                generic_last_error().as_deref(),
                Some("prior real error"),
                "non-actor get_error must not clobber the generic last-error slot"
            );
            crate::hew_clear_error();
        });
    }

    /// Same invariant for `set_error`: routing to the thread-local slot must
    /// not disturb the generic last-error slot.
    #[test]
    fn nonactor_set_error_preserves_generic_last_error() {
        with_no_execution_context(|| {
            crate::set_last_error("prior real error");
            set_error(ErrorSlotKind::Json, "json parse failed");
            assert_eq!(
                generic_last_error().as_deref(),
                Some("prior real error"),
                "non-actor set_error must not clobber the generic last-error slot"
            );
            clear_error(ErrorSlotKind::Json);
            crate::hew_clear_error();
        });
    }

    /// Same invariant for `clear_error`.
    #[test]
    fn nonactor_clear_error_preserves_generic_last_error() {
        with_no_execution_context(|| {
            crate::set_last_error("prior real error");
            clear_error(ErrorSlotKind::Json);
            assert_eq!(
                generic_last_error().as_deref(),
                Some("prior real error"),
                "non-actor clear_error must not clobber the generic last-error slot"
            );
            crate::hew_clear_error();
        });
    }

    /// From a clean generic slot, the routing probe inside set/clear/get must
    /// write nothing at all — the slot stays null, not merely unchanged.
    #[test]
    fn nonactor_probe_writes_no_generic_error_from_clean_slot() {
        with_no_execution_context(|| {
            crate::hew_clear_error();
            set_error(ErrorSlotKind::Yaml, "yaml parse failed");
            assert_eq!(
                generic_last_error(),
                None,
                "set_error routing probe wrote the generic slot"
            );
            assert_eq!(
                get_error(ErrorSlotKind::Yaml).as_deref(),
                Some("yaml parse failed")
            );
            assert_eq!(
                generic_last_error(),
                None,
                "get_error routing probe wrote the generic slot"
            );
            clear_error(ErrorSlotKind::Yaml);
            assert_eq!(
                generic_last_error(),
                None,
                "clear_error routing probe wrote the generic slot"
            );
            assert_eq!(get_error(ErrorSlotKind::Yaml), None);
        });
    }

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
    /// clear or overwrite an XML error.
    #[test]
    fn thread_fallback_parsers_are_independent() {
        // Use large IDs unlikely to collide with other test state.
        set_error(ErrorSlotKind::Xml, "XML error");
        set_error(ErrorSlotKind::Yaml, "yaml error");

        assert_eq!(
            get_error(ErrorSlotKind::Xml).as_deref(),
            Some("XML error"),
            "XML error should not be overwritten by yaml write"
        );
        assert_eq!(
            get_error(ErrorSlotKind::Yaml).as_deref(),
            Some("yaml error"),
        );

        // Clearing yaml must not touch XML.
        clear_error(ErrorSlotKind::Yaml);
        assert_eq!(
            get_error(ErrorSlotKind::Xml).as_deref(),
            Some("XML error"),
            "XML error must survive yaml clear"
        );

        clear_error(ErrorSlotKind::Xml);
    }

    /// A slot written on thread A while actor X is installed is visible on
    /// thread B when actor X resumes there — simulating an actor migrating
    /// across scheduler workers (#1420).
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn actor_slot_error_survives_thread_migration() {
        let mut actor = Box::new(crate::test_actor::stub_actor());
        let actor_addr = (&raw mut *actor) as usize;

        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let barrier2 = barrier.clone();

        let handle = std::thread::spawn(move || {
            barrier2.wait();
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: actor_addr as *mut crate::actor::HewActor,
                    ..Default::default()
                },
            );
            (
                get_error(ErrorSlotKind::Toml),
                get_errno(ErrorSlotKind::Toml),
            )
        });

        {
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: &raw mut *actor,
                    ..Default::default()
                },
            );
            set_error_with_errno(ErrorSlotKind::Toml, 17, "cross-thread error");
        }
        barrier.wait();

        let (detail, errno) = handle.join().expect("thread B panicked");
        assert_eq!(detail.as_deref(), Some("cross-thread error"));
        assert_eq!(errno, 17);
    }

    /// Two different actors have independent error slots for the same parser.
    #[test]
    fn separate_actors_have_independent_slots() {
        let mut actor_a = Box::new(crate::test_actor::stub_actor());
        let mut actor_b = Box::new(crate::test_actor::stub_actor());

        {
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: &raw mut *actor_a,
                    ..Default::default()
                },
            );
            set_error(ErrorSlotKind::Yaml, "error for A");
        }
        {
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: &raw mut *actor_b,
                    ..Default::default()
                },
            );
            set_error(ErrorSlotKind::Yaml, "error for B");
            assert_eq!(
                get_error(ErrorSlotKind::Yaml).as_deref(),
                Some("error for B")
            );
        }
        {
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: &raw mut *actor_a,
                    ..Default::default()
                },
            );
            assert_eq!(
                get_error(ErrorSlotKind::Yaml).as_deref(),
                Some("error for A")
            );
        }
    }

    /// Slots for different parsers on the same actor are independent.
    /// Writing or clearing a YAML error must not disturb TOML or JSON errors.
    #[test]
    fn actor_cross_parser_slots_are_independent() {
        let mut actor = Box::new(crate::test_actor::stub_actor());
        let _ctx = crate::execution_context::TestExecutionContext::install(
            crate::execution_context::HewExecutionContext {
                actor: &raw mut *actor,
                ..Default::default()
            },
        );

        set_error(ErrorSlotKind::Toml, "toml error");
        set_error(ErrorSlotKind::Json, "json error");
        set_error(ErrorSlotKind::Yaml, "yaml success — cleared");

        clear_error(ErrorSlotKind::Yaml);

        assert_eq!(
            get_error(ErrorSlotKind::Toml).as_deref(),
            Some("toml error"),
            "toml error must survive yaml clear"
        );
        assert_eq!(
            get_error(ErrorSlotKind::Json).as_deref(),
            Some("json error"),
            "json error must survive yaml clear"
        );
        assert_eq!(
            get_error(ErrorSlotKind::Yaml),
            None,
            "yaml slot must be cleared"
        );

        actor.parse_errors.clear_all();
        assert_eq!(get_error(ErrorSlotKind::Toml), None);
        assert_eq!(get_error(ErrorSlotKind::Json), None);
    }

    /// Clearing one actor's error slots does not touch another actor's slots.
    #[test]
    fn clear_all_does_not_touch_other_actors() {
        let mut target = Box::new(crate::test_actor::stub_actor());
        let mut other = Box::new(crate::test_actor::stub_actor());

        {
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: &raw mut *target,
                    ..Default::default()
                },
            );
            set_error(ErrorSlotKind::Xml, "target XML");
            set_error(ErrorSlotKind::Json, "target json");
        }
        {
            let _ctx = crate::execution_context::TestExecutionContext::install(
                crate::execution_context::HewExecutionContext {
                    actor: &raw mut *other,
                    ..Default::default()
                },
            );
            set_error(ErrorSlotKind::Toml, "other toml");
        }

        target.parse_errors.clear_all();

        assert_eq!(target.parse_errors.get(ErrorSlotKind::Xml), None);
        assert_eq!(target.parse_errors.get(ErrorSlotKind::Json), None);
        assert_eq!(
            other.parse_errors.get(ErrorSlotKind::Toml).as_deref(),
            Some("other toml"),
            "unrelated actor entry must survive"
        );
    }
}
