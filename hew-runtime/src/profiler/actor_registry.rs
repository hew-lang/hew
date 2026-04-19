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

use crate::util::MutexExt;
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
static DISPATCH_TYPE_REGISTRY: Mutex<Option<HashMap<usize, &'static str>>> = Mutex::new(None);

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
static HANDLER_NAME_REGISTRY: Mutex<Option<HashMap<(usize, i32), String>>> = Mutex::new(None);

/// Register a Hew type name for a dispatch function.
///
/// Must be called once per actor type, before spawning any instance of that
/// type.  Subsequent registrations for the same `dispatch_fn` key are ignored.
///
/// `type_name` must be a `'static` string (a literal baked into the binary).
pub fn register_dispatch_type(
    dispatch_fn: Option<
        unsafe extern "C" fn(*mut std::ffi::c_void, i32, *mut std::ffi::c_void, usize),
    >,
    type_name: &'static str,
) {
    let key = dispatch_fn.map_or(0, |f| f as usize);
    if key == 0 {
        return;
    }
    let mut guard = DISPATCH_TYPE_REGISTRY.lock_or_recover();
    guard
        .get_or_insert_with(HashMap::new)
        .entry(key)
        .or_insert(type_name);
}

/// Look up the Hew type name for a dispatch function pointer.
///
/// Returns `"Actor"` if the dispatch fn is not registered.
#[must_use]
pub fn lookup_dispatch_type(
    dispatch_fn: Option<
        unsafe extern "C" fn(*mut std::ffi::c_void, i32, *mut std::ffi::c_void, usize),
    >,
) -> &'static str {
    let key = match dispatch_fn.map(|f| f as usize) {
        Some(k) if k != 0 => k,
        _ => return "Actor",
    };
    lookup_dispatch_type_by_ptr(key)
}

/// Look up the Hew type name for a dispatch function pointer stored as `usize`.
///
/// Returns `"Actor"` if the pointer is zero or has not been registered.
pub fn lookup_dispatch_type_by_ptr(dispatch_ptr: usize) -> &'static str {
    if dispatch_ptr == 0 {
        return "Actor";
    }
    let guard = DISPATCH_TYPE_REGISTRY.lock_or_recover();
    guard
        .as_ref()
        .and_then(|m| m.get(&dispatch_ptr).copied())
        .unwrap_or("Actor")
}

/// Look up the fully-qualified handler name by raw dispatch pointer and `msg_type`.
///
/// Returns `Some("ActorName::handler_name")` when registered, `None` otherwise.
/// This variant avoids a `transmute` by operating on the raw `usize` pointer.
pub fn lookup_handler_name_by_ptr(dispatch_ptr: usize, msg_type: i32) -> Option<String> {
    if dispatch_ptr == 0 {
        return None;
    }
    let guard = HANDLER_NAME_REGISTRY.lock_or_recover();
    guard
        .as_ref()
        .and_then(|m| m.get(&(dispatch_ptr, msg_type)).cloned())
}

/// Register a fully-qualified handler name for a `(dispatch_fn_ptr, msg_type)` pair.
///
/// Must be called once per `(actor_type, handler)` pair at program startup, before
/// any instances of that actor type are spawned.  Subsequent registrations for the
/// same key are silently ignored (first-registered wins, matching `register_dispatch_type`).
///
/// `handler_name` must be a `"ActorName::handler_name"` string.
pub fn register_handler_name(
    dispatch_fn: Option<
        unsafe extern "C" fn(*mut std::ffi::c_void, i32, *mut std::ffi::c_void, usize),
    >,
    msg_type: i32,
    handler_name: String,
) {
    let key = dispatch_fn.map_or(0, |f| f as usize);
    if key == 0 {
        return;
    }
    let mut guard = HANDLER_NAME_REGISTRY.lock_or_recover();
    guard
        .get_or_insert_with(HashMap::new)
        .entry((key, msg_type))
        .or_insert(handler_name);
}

/// Look up the fully-qualified handler name for a `(dispatch_fn_ptr, msg_type)` pair.
///
/// Returns `Some("ActorName::handler_name")` when the pair was previously registered
/// via `register_handler_name`, or `None` if not found.
pub fn lookup_handler_name(
    dispatch_fn: Option<
        unsafe extern "C" fn(*mut std::ffi::c_void, i32, *mut std::ffi::c_void, usize),
    >,
    msg_type: i32,
) -> Option<String> {
    let key = dispatch_fn.map_or(0, |f| f as usize);
    if key == 0 {
        return None;
    }
    let guard = HANDLER_NAME_REGISTRY.lock_or_recover();
    guard
        .as_ref()
        .and_then(|m| m.get(&(key, msg_type)).cloned())
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
    let guard = REGISTRY.lock_or_recover();
    let map = guard.as_ref()?;
    let SendPtr(ptr) = map.get(&actor_id)?;
    // SAFETY: The actor pointer is valid while it is registered.  We take only
    // the dispatch fn value (a function pointer cast to usize) without
    // dereferencing any managed data.
    let dispatch = unsafe { (**ptr).dispatch };
    dispatch.map(|f| f as usize)
}

/// Clear all registered dispatch-type mappings.
///
/// Called at JIT session reset (via the session hook registered in
/// `profiler::register_reset_hooks`) so that stale dispatch-pointer-to-type-name
/// mappings from a prior JIT load cycle cannot bleed into a fresh one.
///
/// After this call, `lookup_dispatch_type` returns `"Actor"` for all pointers
/// until `register_dispatch_type` is called again.
pub(crate) fn clear_dispatch_registry() {
    let mut guard = DISPATCH_TYPE_REGISTRY.lock_or_recover();
    if let Some(map) = guard.as_mut() {
        map.clear();
    }
    let mut hguard = HANDLER_NAME_REGISTRY.lock_or_recover();
    if let Some(map) = hguard.as_mut() {
        map.clear();
    }
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
    let mut guard = REGISTRY.lock_or_recover();
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
    let mut guard = REGISTRY.lock_or_recover();
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
    /// Hew actor type name, e.g. `"Counter"`.  Defaults to `"Actor"` when the
    /// type has not been registered via `hew_actor_register_type`.
    pub actor_type: &'static str,
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
    let guard = REGISTRY.lock_or_recover();

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

        let actor_type = lookup_dispatch_type(a.dispatch);

        result.push(ActorSnapshot {
            id: a.id,
            pid: a.pid,
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
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Serialise tests that modify shared registry state.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C" fn fake_dispatch_a(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
    }

    unsafe extern "C" fn fake_dispatch_b(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
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
        register_dispatch_type(Some(fake_dispatch_b), "MyCounter");
        let name = lookup_dispatch_type(Some(fake_dispatch_b));
        assert_eq!(name, "MyCounter");
    }

    #[test]
    fn null_dispatch_defaults_to_actor() {
        let name = lookup_dispatch_type(None);
        assert_eq!(name, "Actor");
    }

    unsafe extern "C" fn fake_dispatch_c(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
    }

    unsafe extern "C" fn fake_dispatch_d(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
    }

    #[test]
    fn second_registration_does_not_overwrite() {
        let _lock = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        register_dispatch_type(Some(fake_dispatch_c), "TypeA");
        // Attempt to overwrite with "TypeB" — should be silently ignored.
        register_dispatch_type(Some(fake_dispatch_c), "TypeB");
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
        register_dispatch_type(Some(fake_dispatch_d), "MyWidget");
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

    unsafe extern "C" fn fake_dispatch_e(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
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
        let mut reg = REGISTRY.lock_or_recover();
        if let Some(m) = reg.as_mut() {
            m.clear();
        }
        drop(reg);

        // Register the dispatch type for the fake actor.
        register_dispatch_type(Some(fake_dispatch_e), "ProfiledActor");

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
        actor.pid = 0xdead_beef_cafe_1234;
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

    unsafe extern "C" fn fake_dispatch_handler(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
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
        let result = lookup_handler_name_by_ptr(ptr, 7);
        assert_eq!(
            result.as_deref(),
            Some("Counter::on_increment"),
            "registered handler name must round-trip"
        );
    }

    /// Unknown (`dispatch_ptr`, `msg_type`) pair returns None.
    #[test]
    fn handler_name_unknown_pair_returns_none() {
        let result = lookup_handler_name_by_ptr(0xdead_beef_cafe_babe, 42);
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
        let result = lookup_handler_name_by_ptr(ptr, 99);
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
            lookup_handler_name_by_ptr(ptr, 55).is_some(),
            "handler name must be present before clear"
        );
        clear_dispatch_registry();
        assert!(
            lookup_handler_name_by_ptr(ptr, 55).is_none(),
            "handler name must be absent after clear_dispatch_registry"
        );
    }

    unsafe extern "C" fn fake_dispatch_for_id_lookup(
        _s: *mut std::ffi::c_void,
        _m: i32,
        _p: *mut std::ffi::c_void,
        _n: usize,
    ) {
    }

    /// `lookup_dispatch_for_actor_id` returns the dispatch pointer while an actor is live.
    #[test]
    fn dispatch_for_actor_id_returns_ptr_while_live() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Clear registry to known state.
        let mut reg = REGISTRY.lock_or_recover();
        if let Some(m) = reg.as_mut() {
            m.clear();
        }
        drop(reg);

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
}
