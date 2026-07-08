//! Global actor registry for profiler enumeration.
//!
//! Tracks all live actors so the profiler dashboard can list them with
//! their per-actor statistics (message count, processing time).
//!
//! Registration is O(1) and lock-free for the common path (spawn/free).
//! Enumeration (used by the HTTP API) takes a brief lock.
//!
//! ## Actor type names
//!
//! `HewActor` is `#[repr(C)]` and its layout is fixed by the codegen ABI, so
//! we cannot add a type-name field there.  Instead we maintain a side table
//! keyed by the dispatch function pointer (cast to `usize`).  Generated code
//! calls `hew_actor_register_type` once per actor type before spawning any
//! instance of that type; `snapshot_all` consults the table when building
//! snapshots.  Unregistered dispatch functions fall back to `"Actor"`.

use crate::lifetime::PoisonSafe;
use crate::send_ptr::SendPtr;
use std::collections::HashMap;
use std::sync::atomic::Ordering;

use crate::actor::HewActor;
use crate::internal::types::{HewActorState, HewDispatchFn};
use crate::mailbox::HewMailbox;

/// Global registry of live actors. Keyed by actor ID.
static REGISTRY: PoisonSafe<Option<HashMap<u64, SendPtr<HewActor>>>> = PoisonSafe::new(None);

/// Side table mapping dispatch function pointer (as `usize`) to Hew type name.
///
/// Populated by `hew_actor_register_type` before any instance of the type is
/// spawned.  A `&'static str` is safe to store here because all type name
/// strings originate from string literals baked into the binary.
///
/// # SHIM
///
/// WHY: `HewActor` is `#[repr(C)]` — adding a field would break the codegen
///      ABI.  A side table keyed by dispatch-fn pointer gives us the mapping
///      without touching the struct layout.
/// WHEN: Remove if we ever add an out-of-band actor metadata channel that
///       communicates type identity at spawn time without using the dispatch
///       pointer as a key.
/// REAL: Embed the type name directly in `HewActor` once the C ABI is
///       versioned and codegen is updated to fill the new field.
static DISPATCH_TYPE_REGISTRY: PoisonSafe<Option<HashMap<usize, &'static str>>> =
    PoisonSafe::new(None);

/// Side table mapping `(dispatch_fn_ptr_as_usize, msg_type)` to the fully-qualified
/// handler name `"ActorName::handler_name"`.
///
/// Populated by `hew_register_handler_name` (called at program startup by codegen).
/// Used at `drain_events_json` time on native profiler builds to resolve handler names
/// for trace events — keyed by `(dispatch_fn, msg_type)` to correctly disambiguate
/// between actor types that share the same `msg_type` integer.
///
/// # SHIM
///
/// WHY: WASM uses a flat `msg_type → name` map in bridge.rs (last-registered wins),
///      which is ambiguous for programs with multiple actor types sharing `msg_type` values.
///      This table uses a compound key to be unambiguous.
/// WHEN: Remove the WASM flat-map ambiguity when WASM codegen calls
///       `hew_register_handler_name` instead of (or alongside)
///       `hew_wasm_register_actor_meta`.  Tracked in #1259.
/// REAL: Unify WASM and native paths onto this compound-key registry.
static HANDLER_NAME_REGISTRY: PoisonSafe<Option<HashMap<(usize, i32), String>>> =
    PoisonSafe::new(None);

/// Register a Hew type name for a dispatch function.
///
/// First registration for a `dispatch_fn` key wins; later registrations for the
/// same key drop their `type_name` without leaking. The accepted name is
/// promoted to `&'static str` by leaking it exactly once — only when this call
/// actually inserts — so the table holds at most one leaked string per type,
/// never one per call. The allocation is owned by the table: it is reclaimed and
/// freed by `clear_dispatch_registry` at session reset (see its docs).
///
/// `type_name` is taken by value so the leak decision happens under the table
/// lock: a losing concurrent registration frees its `String` instead of
/// orphaning a `Box::leak`'d copy. Before this took ownership the caller leaked
/// on every call (one orphaned string per spawn of an already-registered type),
/// which a repeated-spawn corpus surfaced under `LeakSanitizer`.
pub fn register_dispatch_type(dispatch_fn: Option<HewDispatchFn>, type_name: String) {
    let key = dispatch_fn.map_or(0, |f| f as usize);
    if key == 0 {
        return;
    }
    DISPATCH_TYPE_REGISTRY.access(|guard| {
        guard
            .get_or_insert_with(HashMap::new)
            .entry(key)
            // Leak only on insert: the vacant arm promotes the owned name to
            // `&'static str`; the occupied arm drops `type_name` (no leak).
            .or_insert_with(|| Box::leak(type_name.into_boxed_str()));
    });
}

/// Look up the Hew type name for a dispatch function pointer, copied into an
/// owned `String` **under the registry lock**.
///
/// Returns `"Actor".to_owned()` if the dispatch fn is not registered.
///
/// [`lookup_dispatch_type`] is now a thin wrapper around this helper — it used
/// to return a bare `&'static str` that borrowed the registry's
/// `Box::leak`'d backing allocation and **released the lock before
/// returning** (see [`PoisonSafe::access`], which drops its guard when the
/// closure returns). Copying *after* that — e.g. `lookup_dispatch_type(d).to_owned()`
/// — raced a concurrent `clear_dispatch_registry`, which can `Box::from_raw`-free
/// that allocation in the gap between lock-release and the copy, leaving the
/// copy to read freed memory. This helper performs the `to_owned()` **inside**
/// the `access` closure, so the copy completes while the lock is still held
/// and `clear` cannot run.
#[must_use]
pub fn lookup_dispatch_type_owned(dispatch_fn: Option<HewDispatchFn>) -> String {
    let key = match dispatch_fn.map(|f| f as usize) {
        Some(k) if k != 0 => k,
        _ => return "Actor".to_owned(),
    };
    lookup_dispatch_type_by_ptr_owned(key)
}

/// Owned-`String` variant of [`lookup_dispatch_type_by_ptr`] that copies the
/// name **under the registry lock**.
///
/// Returns `"Actor".to_owned()` if the pointer is zero or unregistered. See
/// [`lookup_dispatch_type_owned`] for why the by-value copy must happen inside
/// the `access` closure rather than at the call site.
#[must_use]
pub fn lookup_dispatch_type_by_ptr_owned(dispatch_ptr: usize) -> String {
    if dispatch_ptr == 0 {
        return "Actor".to_owned();
    }
    DISPATCH_TYPE_REGISTRY.access(|guard| {
        // `to_owned()` runs while the lock is held, so it observes the leaked
        // backing allocation before any concurrent `clear_dispatch_registry`
        // (which acquires this same lock) can free it.
        guard
            .as_ref()
            .and_then(|m| m.get(&dispatch_ptr).copied())
            .unwrap_or("Actor")
            .to_owned()
    })
}

/// Look up the Hew type name for a dispatch function pointer, copied into an
/// owned `String` under the registry lock.
///
/// Returns `"Actor".to_owned()` if the dispatch fn is not registered.
///
/// This is now a thin wrapper over [`lookup_dispatch_type_owned`]. It used to
/// return a bare `&'static str` borrowed from the registry's `Box::leak`'d
/// backing allocation, handed back *after* the registry lock was released — a
/// caller that copied the name out after that point (`to_owned`, `format!`,
/// comparison-then-copy) raced `clear_dispatch_registry`, which frees that
/// exact allocation with `Box::from_raw`. Every real caller of the old
/// borrowing form only ever consumed it transiently (immediate `assert_eq!`
/// against a `&str` literal in this module's own tests; zero callers
/// elsewhere in the crate), so there was no use case the unsound signature
/// actually served — it only kept the footgun reachable from safe Rust with
/// nothing in the type system enforcing the "don't retain this" invariant the
/// docs relied on. Delegating to the owned helper closes the hole outright
/// instead of just documenting around it.
#[must_use]
pub fn lookup_dispatch_type(dispatch_fn: Option<HewDispatchFn>) -> String {
    lookup_dispatch_type_owned(dispatch_fn)
}

/// Look up the Hew type name for a dispatch function pointer stored as
/// `usize`, copied into an owned `String` under the registry lock.
///
/// Returns `"Actor".to_owned()` if the pointer is zero or has not been
/// registered. Thin wrapper over [`lookup_dispatch_type_by_ptr_owned`]; see
/// [`lookup_dispatch_type`] for why the borrowing `&'static str` form was
/// retired rather than kept as a documented-hazard fast path.
#[must_use]
pub fn lookup_dispatch_type_by_ptr(dispatch_ptr: usize) -> String {
    lookup_dispatch_type_by_ptr_owned(dispatch_ptr)
}

/// Look up the fully-qualified handler name by raw dispatch pointer and `msg_type`.
///
/// Returns `Some("ActorName::handler_name")` when registered, `None` otherwise.
/// This variant avoids a `transmute` by operating on the raw `usize` pointer.
pub fn handler_name_by_ptr(dispatch_ptr: usize, msg_type: i32) -> Option<String> {
    if dispatch_ptr == 0 {
        return None;
    }
    HANDLER_NAME_REGISTRY.access(|guard| {
        guard
            .as_ref()
            .and_then(|m| m.get(&(dispatch_ptr, msg_type)).cloned())
    })
}

/// Register a fully-qualified handler name for a `(dispatch_fn_ptr, msg_type)` pair.
///
/// Must be called once per `(actor_type, handler)` pair at program startup, before
/// any instances of that actor type are spawned.  Subsequent registrations for the
/// same key are silently ignored (first-registered wins, matching `register_dispatch_type`).
///
/// `handler_name` must be a `"ActorName::handler_name"` string.
pub fn register_handler_name(
    dispatch_fn: Option<HewDispatchFn>,
    msg_type: i32,
    handler_name: String,
) {
    let key = dispatch_fn.map_or(0, |f| f as usize);
    if key == 0 {
        return;
    }
    HANDLER_NAME_REGISTRY.access(|guard| {
        guard
            .get_or_insert_with(HashMap::new)
            .entry((key, msg_type))
            .or_insert(handler_name);
    });
}

/// Look up the dispatch function pointer (as `usize`) for a live actor by its ID.
///
/// Returns `Some(dispatch_fn_as_usize)` when the actor is currently registered in
/// the profiler registry, or `None` if the actor has already been freed or was
/// never registered.  Used at `drain_events_json` drain time to build the compound
/// `(dispatch_fn, msg_type)` key for handler-name resolution.
///
/// SHIM: WHY: `HewTraceEvent` cannot carry a dispatch-fn pointer without an ABI
///       change.  Drain-time lookup from the live actor registry is the
///       zero-ABI-impact alternative (Option B from #1260).
/// WHEN: Remove if `HewTraceEvent` is extended with `actor_type_id` in a future
///       ABI revision.
/// REAL: Embed the dispatch pointer (or a stable type ID) directly in
///       `HewTraceEvent` when the C ABI is versioned.
pub fn lookup_dispatch_for_actor_id(actor_id: u64) -> Option<usize> {
    REGISTRY.access(|guard| {
        let map = guard.as_ref()?;
        let ptr = map.get(&actor_id)?.as_ptr();
        // SAFETY: The actor pointer is valid while it is registered.  We take only
        // the dispatch fn value (a function pointer cast to usize) without
        // dereferencing any managed data.
        let dispatch = unsafe { (*ptr).dispatch };
        dispatch.map(|f| f as usize)
    })
}

/// Clear all registered dispatch-type mappings.
///
/// Called at JIT session reset (via the session hook registered in
/// `profiler::register_reset_hooks`) so that stale dispatch-pointer-to-type-name
/// mappings from a prior JIT load cycle cannot bleed into a fresh one.
///
/// Each value in `DISPATCH_TYPE_REGISTRY` is a `&'static str` produced by
/// `Box::leak(String::into_boxed_str(..))` in `register_dispatch_type`. A plain
/// `map.clear()` would drop the fat pointers but orphan the heap they point to,
/// so we reclaim each allocation with `Box::from_raw` (the exact inverse of the
/// `Box::leak` that created it) and let the reconstituted `Box<str>` drop.
/// Keeping the table's internal value type as `&'static str` (rather than an
/// owned `String` per entry) avoids a second per-registration allocation on
/// top of the leak the table already needs to hand a process-lifetime pointer
/// to lookup callers under the lock.
///
/// Reclaiming here is sound because no live borrow into these allocations races
/// the free. Every retaining consumer copies the name out **while holding this
/// same lock**, via the owned lookup helpers (`lookup_dispatch_type_owned` /
/// `lookup_dispatch_type_by_ptr_owned`, and the public [`lookup_dispatch_type`] /
/// [`lookup_dispatch_type_by_ptr`] wrappers around them): `snapshot_all` into an
/// owned `ActorSnapshot::actor_type: String`, and `signal`/`tracing` into their
/// `String`/`format!` outputs. Because the copy completes inside the
/// `DISPATCH_TYPE_REGISTRY.access` closure, this `clear` — which acquires the
/// same lock — cannot interleave between a consumer's lookup and its copy.
/// There is no longer a borrowing public accessor that hands back a
/// `&'static str` after releasing the lock: that lookup-to-copy TOCTOU
/// use-after-free window (a caller copying the name out *after* the lock
/// released, racing this reclaim) was closed by retiring the borrowing form
/// of [`lookup_dispatch_type`] / [`lookup_dispatch_type_by_ptr`] in favor of
/// copying under the lock unconditionally — see their docs. This lock-scope
/// coupling matters because the profiler server/sampler threads are NOT
/// joined on this path — they are joined only by `profiler::shutdown`
/// (reachable from `hew_node_stop`), whereas this runs from the
/// `session_reset` hook fired by `hew_sched_shutdown` after only the scheduler
/// workers are joined. A live profiler thread serving `/api/actors` either
/// holds an already-owned `ActorSnapshot` or is mid-copy under the lock —
/// never a bare borrow into what we free here.
///
/// After this call, `lookup_dispatch_type` returns `"Actor".to_owned()` for all
/// pointers until `register_dispatch_type` is called again.
pub(crate) fn clear_dispatch_registry() {
    DISPATCH_TYPE_REGISTRY.access(|guard| {
        if let Some(map) = guard.as_mut() {
            for leaked in map.values() {
                // SAFETY: every value was created by
                // `Box::leak(String::into_boxed_str(..))` in
                // `register_dispatch_type`, so the pointer owns a live
                // `Box<str>` allocation and reclaiming it via `Box::from_raw`
                // is the exact inverse of that leak. Each pointer is unique
                // (HashMap keys are distinct dispatch fns, each leaked exactly
                // once), so there is no double free, and the reconstituted
                // `Box<str>` drops before `map.clear()` invalidates the entry.
                //
                // No live borrow into these allocations races this free: every
                // retaining consumer copies the name out under *this same lock*
                // via the owned lookup helpers (see the doc above), so a copy
                // either completed before we acquired the lock or has not started
                // — it can never interleave with this reclaim. Even the profiler
                // threads — which this path does NOT join before clearing — hold
                // owned data, not a pointer into what we free here.
                let owned: Box<str> =
                    unsafe { Box::from_raw(std::ptr::from_ref(*leaked).cast_mut()) };
                drop(owned);
            }
            map.clear();
        }
    });
    HANDLER_NAME_REGISTRY.access(|guard| {
        if let Some(map) = guard.as_mut() {
            map.clear();
        }
    });
}

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
    REGISTRY.access(|guard| {
        // SAFETY: `HewActor` is `Send + Sync` (see actor.rs).  The registry only
        // holds pointers to live actors and `unregister` removes the entry
        // before the actor is freed, so the pointer is valid for the duration
        // of its registration.
        let entry = unsafe { SendPtr::new(actor) };
        guard.get_or_insert_with(HashMap::new).insert(id, entry);
    });
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
    REGISTRY.access(|guard| {
        if let Some(map) = guard.as_mut() {
            map.remove(&id);
        }
    });
}

/// Snapshot of a single actor's stats for the profiler API.
#[derive(Debug, Clone)]
pub struct ActorSnapshot {
    /// Actor ID.
    pub id: u64,
    /// Actor PID.
    pub pid: u64,
    /// Hew actor type name, e.g. `"Counter"`.  Defaults to `"Actor"` when the
    /// type has not been registered via `hew_actor_register_type`.
    ///
    /// Owned (`String`, not `&'static str`) so a snapshot never aliases the
    /// dispatch-type registry's leaked backing allocations. `clear_dispatch_registry`
    /// (the `session_reset` hook) reclaims those allocations with `Box::from_raw`,
    /// but it runs after only the *scheduler workers* are joined — NOT the
    /// profiler server/sampler threads, which are joined solely by
    /// `profiler::shutdown` on the `hew_node_stop` path. A `HEW_PPROF` program
    /// with no `HewNode` (the profiler is started via `maybe_start` in
    /// `hew_sched_init`) therefore has a live profiler thread serving
    /// `/api/actors` concurrently with `clear_dispatch_registry`. Were this a
    /// `&'static str` it would dangle in `serve_actors`/`pprof` after the clear;
    /// copying the name out in `snapshot_all` severs that coupling so the
    /// snapshot safely outlives the registry.
    pub actor_type: String,
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

#[derive(Debug, Clone, Copy, Default)]
pub struct MailboxAggregate {
    pub sum_depth: u64,
    pub max_depth: u64,
    pub p99_depth: u64,
}

/// Enumerate all live actors and return a snapshot of their stats.
pub fn snapshot_all() -> Vec<ActorSnapshot> {
    REGISTRY.access(|guard| {
        let Some(map) = guard.as_ref() else {
            return Vec::new();
        };

        let mut result = Vec::with_capacity(map.len());
        for entry in map.values() {
            let actor_ptr = entry.as_ptr();
            // SAFETY: Actor is registered and not yet freed. The pointer
            // is valid and the atomic fields can be read concurrently.
            let a = unsafe { &*actor_ptr };

            let state_int = a.actor_state.load(Ordering::Relaxed);
            let state_name = if state_int == HewActorState::Idle as i32 {
                "idle"
            } else if state_int == HewActorState::Runnable as i32 {
                "runnable"
            } else if state_int == HewActorState::Running as i32 {
                "running"
            } else if state_int == HewActorState::Suspended as i32 {
                "suspended"
            } else if state_int == HewActorState::Stopping as i32 {
                "stopping"
            } else if state_int == HewActorState::Crashing as i32 {
                "crashing"
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

            // lookup_dispatch_type_owned acquires DISPATCH_TYPE_REGISTRY — a
            // different mutex, so nesting here is safe. The owned variant copies
            // the name into a `String` *under that lock*, so the copy observes
            // the registry's leaked backing allocation before any concurrent
            // `clear_dispatch_registry` (which acquires the same lock) can
            // `Box::from_raw`-free it. Copying after the lock released — as a
            // plain `lookup_dispatch_type(..).to_owned()` would — is a
            // lookup-to-copy TOCTOU read of freed memory, because the profiler
            // thread consuming this snapshot is not joined before that reclaim
            // (see `ActorSnapshot::actor_type` and `lookup_dispatch_type_owned`).
            let actor_type = lookup_dispatch_type_owned(a.dispatch);

            result.push(ActorSnapshot {
                id: a.id,
                pid: a.id,
                actor_type,
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
    })
}

/// Count live actors currently in a specific scheduler state.
#[must_use]
pub fn state_count(state: i32) -> u64 {
    REGISTRY.access(|guard| {
        guard.as_ref().map_or(0, |map| {
            map.values()
                .filter(|entry| {
                    let actor_ptr = entry.as_ptr();
                    // SAFETY: Actor is registered and not yet freed.
                    unsafe { (*actor_ptr).actor_state.load(Ordering::Relaxed) == state }
                })
                .count() as u64
        })
    })
}

/// Count runnable actors that have a parked continuation ready to resume.
#[must_use]
pub fn runnable_coroutine_count() -> u64 {
    REGISTRY.access(|guard| {
        guard.as_ref().map_or(0, |map| {
            map.values()
                .filter(|entry| {
                    let actor_ptr = entry.as_ptr();
                    // SAFETY: Actor is registered and not yet freed.
                    unsafe {
                        (*actor_ptr).actor_state.load(Ordering::Relaxed)
                            == HewActorState::Runnable as i32
                            && !(*actor_ptr)
                                .suspended_cont
                                .load(Ordering::Relaxed)
                                .is_null()
                    }
                })
                .count() as u64
        })
    })
}

#[must_use]
pub fn mailbox_aggregate() -> MailboxAggregate {
    REGISTRY.access(|guard| {
        let Some(map) = guard.as_ref() else {
            return MailboxAggregate::default();
        };
        let mut depths = Vec::with_capacity(map.len());
        for entry in map.values() {
            let actor_ptr = entry.as_ptr();
            // SAFETY: Actor is registered and not yet freed.
            let actor = unsafe { &*actor_ptr };
            if actor.mailbox.is_null() {
                depths.push(0_u64);
                continue;
            }
            let mailbox = actor.mailbox.cast::<HewMailbox>();
            // SAFETY: Mailbox is valid while actor is registered.
            let depth = unsafe { (*mailbox).count.load(Ordering::Relaxed) }
                .max(0)
                .cast_unsigned();
            depths.push(depth);
        }
        if depths.is_empty() {
            return MailboxAggregate::default();
        }
        depths.sort_unstable();
        let sum_depth = depths.iter().copied().sum();
        let max_depth = *depths.last().unwrap_or(&0);
        let p99_index = ((depths.len() * 99).saturating_add(99) / 100).saturating_sub(1);
        MailboxAggregate {
            sum_depth,
            max_depth,
            p99_depth: depths[p99_index.min(depths.len() - 1)],
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    /// Serialise tests that modify shared registry state.
    // INTENTIONAL: TEST_LOCK uses raw Mutex; short-lived test serialisation
    // barrier, closure API adds no value here.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C-unwind" fn fake_dispatch_a(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn fake_dispatch_b(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    #[test]
    fn unregistered_dispatch_defaults_to_actor() {
        let _lock = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // A dispatch fn that was never registered should return the default.
        let name = lookup_dispatch_type(Some(fake_dispatch_a));
        // May be "Actor" (unregistered) or whatever a previous test left; the
        // point is the function does not panic and returns a &'static str.
        assert!(!name.is_empty());
    }

    #[test]
    fn registered_dispatch_returns_type_name() {
        let _lock = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        register_dispatch_type(Some(fake_dispatch_b), "MyCounter".to_owned());
        let name = lookup_dispatch_type(Some(fake_dispatch_b));
        assert_eq!(name, "MyCounter");
    }

    #[test]
    fn null_dispatch_defaults_to_actor() {
        let name = lookup_dispatch_type(None);
        assert_eq!(name, "Actor");
    }

    unsafe extern "C-unwind" fn fake_dispatch_c(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn fake_dispatch_d(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    #[test]
    fn second_registration_does_not_overwrite() {
        let _lock = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        register_dispatch_type(Some(fake_dispatch_c), "TypeA".to_owned());
        // Attempt to overwrite with "TypeB" — should be silently ignored and the
        // losing `String` dropped (freed), not leaked.
        register_dispatch_type(Some(fake_dispatch_c), "TypeB".to_owned());
        let name = lookup_dispatch_type(Some(fake_dispatch_c));
        assert_eq!(name, "TypeA");
    }

    /// After `clear_dispatch_registry`, any previously registered dispatch fn
    /// returns the default `"Actor"` fallback.
    #[test]
    fn dispatch_registry_cleared_after_clear() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Register a known type.
        register_dispatch_type(Some(fake_dispatch_d), "MyWidget".to_owned());
        assert_eq!(
            lookup_dispatch_type(Some(fake_dispatch_d)),
            "MyWidget",
            "type must be registered before clear"
        );

        // Clear the registry.
        clear_dispatch_registry();

        // After clear, the same dispatch fn falls back to the default.
        assert_eq!(
            lookup_dispatch_type(Some(fake_dispatch_d)),
            "Actor",
            "lookup must return default after clear_dispatch_registry"
        );
    }

    unsafe extern "C-unwind" fn fake_dispatch_reclaim(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    /// `clear_dispatch_registry` reclaims the `Box::leak`'d type-name strings.
    ///
    /// Repeatedly register-then-clear under the same dispatch key: each register
    /// leaks one `Box<str>` and each clear must reclaim it. Run under
    /// `LeakSanitizer` (`make asan`) this reports zero leaked bytes; a plain
    /// `map.clear()` would orphan one allocation per iteration. The behavioural
    /// assertion (fallback after clear) backstops the same invariant when
    /// `AddressSanitizer` is not active.
    #[test]
    fn clear_dispatch_registry_reclaims_leaked_strings() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        clear_dispatch_registry();

        for _ in 0..16 {
            register_dispatch_type(Some(fake_dispatch_reclaim), "ReclaimedActor".to_owned());
            assert_eq!(
                lookup_dispatch_type(Some(fake_dispatch_reclaim)),
                "ReclaimedActor",
                "type must be registered before clear"
            );
            clear_dispatch_registry();
            assert_eq!(
                lookup_dispatch_type(Some(fake_dispatch_reclaim)),
                "Actor",
                "lookup must fall back after clear reclaims the string"
            );
        }
    }

    unsafe extern "C-unwind" fn fake_dispatch_retain_across_clear(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    /// Regression for the lookup-to-copy TOCTOU use-after-free that used to be
    /// reachable through the public `lookup_dispatch_type`/`lookup_dispatch_type_by_ptr`
    /// API: retain a lookup result across `clear_dispatch_registry` and read it.
    ///
    /// `lookup_dispatch_type` used to return a bare `&'static str` borrowed from
    /// the registry's `Box::leak`'d backing allocation, handed back *after* the
    /// registry lock was released. `clear_dispatch_registry` reclaims that exact
    /// allocation with `Box::from_raw`. A caller that retained the borrow past a
    /// concurrent (or, as here, subsequent) clear read freed memory through a
    /// 100%-safe-Rust call path — no `unsafe` at the call site, nothing in the
    /// type system preventing it. `lookup_dispatch_type` is now a thin wrapper
    /// over the owned helper, so the retained value is a real `String` holding
    /// its own copy: it must survive the clear and still read the exact
    /// registered name, with no reliance on allocator-timing luck. Run under
    /// `AddressSanitizer` (`make asan`), the pre-fix borrowing form would report
    /// a heap-use-after-free at the final `assert_eq!`; the owned return type
    /// makes that class of bug unreachable through this function at all.
    #[test]
    fn lookup_dispatch_type_retained_across_clear_stays_valid() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        clear_dispatch_registry();
        register_dispatch_type(
            Some(fake_dispatch_retain_across_clear),
            "RetainedViaLookup".to_owned(),
        );

        // Retain the lookup result past the point where the lock is released
        // and past a subsequent clear — exactly the misuse pattern the retired
        // borrowing form of this function could not protect against.
        let retained = lookup_dispatch_type(Some(fake_dispatch_retain_across_clear));
        let retained_by_ptr =
            lookup_dispatch_type_by_ptr(fake_dispatch_retain_across_clear as *const () as usize);

        // This frees the backing allocation the old borrowing form would have
        // aliased. If `retained`/`retained_by_ptr` were still `&'static str`
        // borrows into that allocation, this is exactly where they would dangle.
        clear_dispatch_registry();

        assert_eq!(
            retained, "RetainedViaLookup",
            "retained owned String must survive clear_dispatch_registry intact"
        );
        assert_eq!(
            retained_by_ptr, "RetainedViaLookup",
            "retained owned String (by-ptr variant) must survive clear_dispatch_registry intact"
        );

        // Post-clear, a fresh lookup correctly falls back to the default —
        // confirms the clear itself still ran and reclaimed the entry; the
        // assertions above are about the *already-retained* copies, not this.
        assert_eq!(
            lookup_dispatch_type(Some(fake_dispatch_retain_across_clear)),
            "Actor",
            "fresh lookup after clear must fall back to the default"
        );
    }

    /// A snapshot taken before `clear_dispatch_registry` stays valid after it.
    ///
    /// Regression for a use-after-free: `clear_dispatch_registry` frees the
    /// `Box::leak`'d type-name allocations with `Box::from_raw`, but it runs on
    /// the `session_reset`/`hew_sched_shutdown` path that joins only the
    /// scheduler workers — NOT the profiler server/sampler threads (joined only
    /// by `profiler::shutdown` from `hew_node_stop`). A `HEW_PPROF` program with
    /// no `HewNode` therefore has a live profiler thread holding an
    /// `ActorSnapshot` while the registry is cleared. If `ActorSnapshot::actor_type`
    /// were a `&'static str` it would alias the freed allocation and dangle here;
    /// because the snapshot owns its copy, the name survives the clear intact.
    ///
    /// This models that retained-snapshot-across-clear window directly: snapshot,
    /// clear (frees the backing allocation), then read `actor_type`. Under
    /// `AddressSanitizer`/`LeakSanitizer` (`make asan`) a borrow would be reported
    /// as a use-after-free at the post-clear read; the owned `String` reads clean
    /// and still equals the registered name.
    #[test]
    fn snapshot_actor_type_survives_registry_clear() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        clear_dispatch_registry();
        REGISTRY.access(|reg| {
            if let Some(m) = reg.as_mut() {
                m.clear();
            }
        });

        register_dispatch_type(Some(fake_dispatch_reclaim), "RetainedActor".to_owned());

        // SAFETY: zero-initialising a #[repr(C)] HewActor is sound — every atomic
        // field has a valid zero value and every pointer field is null. `mailbox`
        // is null so snapshot_all skips the mailbox read. We unregister before the
        // Box drops so the registry never holds a dangling pointer.
        let mut actor: Box<HewActor> = unsafe { Box::new(std::mem::zeroed()) };
        actor.id = 0x0bad_f00d_dead_0001;
        actor.dispatch = Some(fake_dispatch_reclaim);
        let actor_ptr: *mut HewActor = &raw mut *actor;
        // SAFETY: actor_ptr is valid for the duration of this test.
        unsafe { register(actor_ptr) };

        // Take the snapshot the profiler server thread would hold across teardown.
        let snapshots = snapshot_all();
        let snap = snapshots
            .iter()
            .find(|s| s.id == actor.id)
            .cloned()
            .expect("registered actor must appear in snapshot");
        assert_eq!(
            snap.actor_type, "RetainedActor",
            "snapshot must carry the registered type name before clear"
        );

        // Now reclaim the leaked backing allocation — the exact byte the field
        // would have pointed at were it still a borrow.
        clear_dispatch_registry();

        // Reading the retained snapshot after the clear must not touch freed
        // memory; the owned copy still holds the name.
        assert_eq!(
            snap.actor_type, "RetainedActor",
            "snapshot actor_type must survive clear_dispatch_registry (owned, not borrowed)"
        );

        // SAFETY: actor_ptr is still valid; actor has not been freed.
        unsafe { unregister(actor_ptr) };
    }

    unsafe extern "C-unwind" fn fake_dispatch_e(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    /// Regression test for the `hew_sched_shutdown` ordering bug:
    ///
    /// `session_reset()` (which calls `clear_dispatch_registry()`) must fire
    /// AFTER the exit profile snapshot runs, not before.  If the reset fires
    /// first, `snapshot_all()` sees an empty dispatch-type registry and falls
    /// back to "Actor" for every actor — degrading exit profile labels.
    ///
    /// This test proves the invariant at the snapshot seam: `snapshot_all`
    /// returns the registered type name before clear and "Actor" after,
    /// confirming that calling clear before snapshot loses the type label.
    #[test]
    fn snapshot_preserves_type_name_before_session_reset() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Clear registries to a known state before the test.
        clear_dispatch_registry();
        REGISTRY.access(|reg| {
            if let Some(m) = reg.as_mut() {
                m.clear();
            }
        });

        // Register the dispatch type for the fake actor.
        register_dispatch_type(Some(fake_dispatch_e), "ProfiledActor".to_owned());

        // Build a minimal HewActor on the heap.  All fields are zero-initialized
        // (atomics initialised to 0, pointers to null).  `mailbox` is null so
        // snapshot_all skips the mailbox-depth read.  `dispatch` is set to
        // fake_dispatch_e so lookup_dispatch_type returns the registered name.
        //
        // SAFETY: zero-initialising a #[repr(C)] struct where every atomic field
        // has a valid zero value and every raw-pointer field is null is safe.
        // We unregister before the Box is dropped so the registry never holds a
        // dangling pointer.
        let mut actor: Box<HewActor> = unsafe { Box::new(std::mem::zeroed()) };
        actor.id = 0xdead_beef_cafe_1234;
        actor.dispatch = Some(fake_dispatch_e);

        let actor_ptr: *mut HewActor = &raw mut *actor;

        // Register the fake actor so snapshot_all can enumerate it.
        // SAFETY: actor_ptr is valid for the duration of this test.
        unsafe { register(actor_ptr) };

        // Profile snapshot runs against a live dispatch registry — type name
        // must appear in the output.  This is the state BEFORE session_reset()
        // fires (i.e. the correct ordering: profile first, then reset).
        let before_snapshots = snapshot_all();
        let before = before_snapshots
            .iter()
            .find(|s| s.id == actor.id)
            .expect("registered actor must appear in snapshot");
        assert_eq!(
            before.actor_type, "ProfiledActor",
            "snapshot before session_reset must preserve registered type name"
        );

        // Now simulate what session_reset() does: clear the dispatch registry.
        // This is what would happen if hew_sched_shutdown called session_reset()
        // BEFORE maybe_write_on_exit() — the bug being fixed.
        clear_dispatch_registry();

        // After session_reset the same actor snapshot falls back to "Actor".
        // This proves that the ordering matters: profile must run before reset.
        let after_snapshots = snapshot_all();
        let after = after_snapshots
            .iter()
            .find(|s| s.id == actor.id)
            .expect("registered actor must still appear in snapshot");
        assert_eq!(
            after.actor_type, "Actor",
            "snapshot after clear_dispatch_registry must fall back to default"
        );

        // Clean up: unregister before actor is dropped.
        // SAFETY: actor_ptr is still valid; actor has not been freed.
        unsafe { unregister(actor_ptr) };
    }

    /// Removal-of-Blocked invariant: cycling an actor through every valid
    /// state (and through the stale legacy discriminant `3`) must never
    /// surface `"blocked"` on a snapshot.  Discriminant `3` falls through to
    /// the catch-all `"unknown"` label — the variant has been excised from
    /// the runtime enum and the registry's stringifier no longer recognises
    /// it as a real state.
    #[test]
    fn snapshot_never_reports_blocked_state() {
        use std::sync::atomic::Ordering;

        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        clear_dispatch_registry();
        REGISTRY.access(|reg| {
            if let Some(m) = reg.as_mut() {
                m.clear();
            }
        });

        register_dispatch_type(Some(fake_dispatch_e), "CooperativeActor".to_owned());

        // SAFETY: zero-init of a #[repr(C)] HewActor is sound — every atomic
        // field has a valid zero pattern and every pointer field is null.
        let mut actor: Box<HewActor> = unsafe { Box::new(std::mem::zeroed()) };
        actor.id = 0x1234_5678_9abc_def0;
        actor.dispatch = Some(fake_dispatch_e);

        let actor_ptr: *mut HewActor = &raw mut *actor;
        // SAFETY: actor_ptr is valid for the duration of this test.
        unsafe { register(actor_ptr) };

        // Every state the runtime is allowed to transition into, including the
        // repurposed `3` discriminant (formerly the dead `Blocked` variant,
        // now `Suspended`).  The probe asserts that none of these states
        // stringify to "blocked".
        let states: &[(i32, &str)] = &[
            (HewActorState::Idle as i32, "idle"),
            (HewActorState::Runnable as i32, "runnable"),
            (HewActorState::Running as i32, "running"),
            (HewActorState::Suspended as i32, "suspended"),
            (HewActorState::Stopping as i32, "stopping"),
            (HewActorState::Crashing as i32, "crashing"),
            (HewActorState::Crashed as i32, "crashed"),
            (HewActorState::Stopped as i32, "stopped"),
            // Repurposed discriminant — `3` is now Suspended, never "blocked".
            (3, "suspended"),
            // Out-of-range value — catch-all.
            (999, "unknown"),
        ];

        for (raw, expected_label) in states {
            actor.actor_state.store(*raw, Ordering::Relaxed);

            let snaps = snapshot_all();
            let snap = snaps
                .iter()
                .find(|s| s.id == actor.id)
                .expect("registered actor must appear in snapshot");

            assert_ne!(
                snap.state, "blocked",
                "snapshot must never report \"blocked\" (raw state={raw})"
            );
            assert_eq!(
                snap.state, *expected_label,
                "snapshot label for raw state {raw} should be {expected_label}"
            );
        }

        // SAFETY: actor_ptr is still valid; actor has not been freed.
        unsafe { unregister(actor_ptr) };
    }

    unsafe extern "C-unwind" fn fake_dispatch_handler(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    /// Registering a handler name and looking it up by raw ptr returns the stored value.
    #[test]
    fn handler_name_round_trips() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        register_handler_name(
            Some(fake_dispatch_handler),
            7,
            "Counter::on_increment".to_owned(),
        );
        let ptr = fake_dispatch_handler as *const () as usize;
        let result = handler_name_by_ptr(ptr, 7);
        assert_eq!(
            result.as_deref(),
            Some("Counter::on_increment"),
            "registered handler name must round-trip"
        );
    }

    /// Unknown (`dispatch_ptr`, `msg_type`) pair returns None.
    #[test]
    fn handler_name_unknown_pair_returns_none() {
        let result = handler_name_by_ptr(0xdead_beef_cafe_babe, 42);
        assert!(result.is_none(), "unknown pair must return None");
    }

    /// Second registration for the same (`dispatch_ptr`, `msg_type`) is silently ignored.
    #[test]
    fn handler_name_second_registration_ignored() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        register_handler_name(Some(fake_dispatch_handler), 99, "Actor::first".to_owned());
        register_handler_name(Some(fake_dispatch_handler), 99, "Actor::second".to_owned());
        let ptr = fake_dispatch_handler as *const () as usize;
        let result = handler_name_by_ptr(ptr, 99);
        // First registration wins.
        assert_eq!(result.as_deref(), Some("Actor::first"));
    }

    /// `clear_dispatch_registry` also clears handler name registrations.
    #[test]
    fn handler_names_cleared_with_dispatch_registry() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        register_handler_name(Some(fake_dispatch_handler), 55, "MyType::on_msg".to_owned());
        let ptr = fake_dispatch_handler as *const () as usize;
        assert!(
            handler_name_by_ptr(ptr, 55).is_some(),
            "handler name must be present before clear"
        );
        clear_dispatch_registry();
        assert!(
            handler_name_by_ptr(ptr, 55).is_none(),
            "handler name must be absent after clear_dispatch_registry"
        );
    }

    unsafe extern "C-unwind" fn fake_dispatch_for_id_lookup(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    /// `lookup_dispatch_for_actor_id` returns the dispatch pointer while an actor is live.
    #[test]
    fn dispatch_for_actor_id_returns_ptr_while_live() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Clear registry to known state.
        REGISTRY.access(|reg| {
            if let Some(m) = reg.as_mut() {
                m.clear();
            }
        });

        let mut actor: Box<crate::actor::HewActor> =
            // SAFETY: zero-initialising is valid for all atomic and ptr fields.
            unsafe { Box::new(std::mem::zeroed()) };
        actor.id = 0x1234_5678_9abc_def0;
        actor.dispatch = Some(fake_dispatch_for_id_lookup);

        let actor_ptr: *mut crate::actor::HewActor = &raw mut *actor;
        // SAFETY: actor_ptr is valid for the duration of this test.
        unsafe { register(actor_ptr) };

        let found = lookup_dispatch_for_actor_id(actor.id);
        assert_eq!(
            found,
            Some(fake_dispatch_for_id_lookup as *const () as usize),
            "dispatch ptr must match while actor is registered"
        );

        // After unregister the lookup returns None.
        // SAFETY: actor_ptr is still valid.
        unsafe { unregister(actor_ptr) };
        let gone = lookup_dispatch_for_actor_id(actor.id);
        assert!(
            gone.is_none(),
            "dispatch ptr must be absent after unregister"
        );
    }

    unsafe extern "C-unwind" fn fake_dispatch_race(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut std::ffi::c_void {
        std::ptr::null_mut()
    }

    /// Regression for the lookup-to-copy TOCTOU use-after-free:
    /// `lookup_dispatch_type_by_ptr_owned` must copy the type name *under* the
    /// `DISPATCH_TYPE_REGISTRY` lock, so a concurrent `clear_dispatch_registry`
    /// (which `Box::from_raw`-frees each leaked `&'static str`) cannot free the
    /// backing allocation between the lookup and the copy.
    ///
    /// The original fix owned the name in `ActorSnapshot` (closing the
    /// *retained-snapshot-across-clear* dangle) but still copied via
    /// `lookup_dispatch_type(d).to_owned()` — the `&'static str` was handed back
    /// after `PoisonSafe::access` released the lock, so the `.to_owned()` raced
    /// the reclaim. The owned helper closes that window by copying inside the
    /// `access` closure.
    ///
    /// This hammers `lookup_dispatch_type_by_ptr_owned` from reader threads while
    /// a writer thread register/clears the same key in a tight loop. Under
    /// `AddressSanitizer` (`make asan`) a copy that read freed memory would be
    /// reported as a heap-use-after-free; the owned-under-lock helper reads clean.
    /// Every observed name is either the registered string or the `"Actor"`
    /// fallback — never a torn or freed value.
    #[test]
    fn lookup_owned_is_safe_against_concurrent_clear() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;

        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        clear_dispatch_registry();

        let key = fake_dispatch_race as *const () as usize;
        let stop = Arc::new(AtomicBool::new(false));

        // Writer: register then clear the same key in a tight loop. Each register
        // leaks one `Box<str>`; each clear reclaims it. The readers race exactly
        // the post-lookup copy against this reclaim.
        let writer_stop = Arc::clone(&stop);
        let writer = std::thread::spawn(move || {
            while !writer_stop.load(Ordering::Relaxed) {
                register_dispatch_type(Some(fake_dispatch_race), "RacedActor".to_owned());
                clear_dispatch_registry();
            }
        });

        // Readers: copy the name under the lock via the owned helper. Any value
        // returned must be a valid, fully-formed string — either the registered
        // name or the unregistered fallback, never freed bytes.
        let readers: Vec<_> = (0..4)
            .map(|_| {
                std::thread::spawn(move || {
                    for _ in 0..20_000 {
                        let name = lookup_dispatch_type_by_ptr_owned(key);
                        assert!(
                            name == "RacedActor" || name == "Actor",
                            "owned lookup returned an unexpected value: {name:?}"
                        );
                    }
                })
            })
            .collect();

        for r in readers {
            r.join().expect("reader thread panicked (likely a UAF)");
        }
        stop.store(true, Ordering::Relaxed);
        writer.join().expect("writer thread panicked");

        // Leave the registry empty for the next test.
        clear_dispatch_registry();
    }
}
