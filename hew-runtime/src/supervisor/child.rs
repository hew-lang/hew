//! Child and child-supervisor registration, lookup and dynamic-child management.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

/// Register a child supervisor under a parent supervisor.
///
/// The parent will recursively stop the child supervisor when the parent is
/// stopped, and the child supervisor's crash (restart budget exhausted)
/// propagates to the parent.
///
/// # Safety
///
/// - `parent` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must not already be registered as a child of another supervisor
///   (no cycles).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_supervisor(
    parent: *mut HewSupervisor,
    child: *mut HewSupervisor,
) -> c_int {
    if parent.is_null() || child.is_null() || parent == child {
        return -1;
    }
    // SAFETY: caller guarantees `child` is live through this construction edge.
    let child_token = unsafe { (*child).local_pid_id };
    // SAFETY: caller keeps parent live through this registration.
    let mut guard = unsafe { &(*parent).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable parallel-roster access.
    let p = &mut *guard;
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_tokens.len());
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_specs.len());
    let idx = p.child_supervisors.len();
    p.child_supervisors.push(child);
    p.child_supervisor_tokens.push(child_token);
    p.child_supervisor_specs.push(None);
    // Set parent back-pointer on the child supervisor.
    // SAFETY: caller guarantees child is valid.
    unsafe {
        (*child).parent = parent;
        (*child).index_in_parent = idx;
        // Unregister from top-level list (was registered in
        // hew_supervisor_start when parent was still null).
        crate::shutdown::hew_shutdown_unregister_supervisor(child);
    };
    0
}

/// Register a child supervisor with an init function for restartability.
///
/// When the child supervisor's restart budget is exhausted and it escalates,
/// the parent can restart the entire subtree by calling `init_fn`.
///
/// # Safety
///
/// - `parent` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `init_fn` must be a valid function pointer that returns a new supervisor.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_supervisor_with_init(
    parent: *mut HewSupervisor,
    child: *mut HewSupervisor,
    init_fn: SupervisorInitFn,
) -> c_int {
    if parent.is_null() || child.is_null() || parent == child {
        return -1;
    }
    // SAFETY: caller guarantees `child` is live through this construction edge.
    let child_token = unsafe { (*child).local_pid_id };
    // SAFETY: caller keeps parent live through this registration.
    let mut guard = unsafe { &(*parent).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable parallel-roster access.
    let p = &mut *guard;
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_tokens.len());
    debug_assert_eq!(p.child_supervisors.len(), p.child_supervisor_specs.len());
    let identity = p.next_child_spec_identity;
    let Some(next_identity) = identity.checked_add(1) else {
        return -1;
    };
    p.next_child_spec_identity = next_identity;
    let idx = p.child_supervisors.len();
    p.child_supervisors.push(child);
    p.child_supervisor_tokens.push(child_token);
    p.child_supervisor_specs.push(Some(SupervisorChildSpec {
        spawn: SupervisorChildSpawn::Legacy(init_fn),
        identity,
        restart_policy: RESTART_PERMANENT,
        spent: false,
    }));
    // SAFETY: child and parent are valid pointers per caller contract.
    unsafe {
        (*child).parent = parent;
        (*child).index_in_parent = idx;
        // The child was auto-registered as a top-level supervisor in
        // hew_supervisor_start (parent was null at that point). Now that
        // it has a parent, unregister it so only the true root is stopped.
        crate::shutdown::hew_shutdown_unregister_supervisor(child);
    };
    0
}

/// Return the child supervisor pointer at `index`, or null when the slot is out
/// of range, empty, or holds an occupant its stable token no longer pins.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child_supervisor(
    sup: *mut HewSupervisor,
    index: c_int,
) -> *mut HewSupervisor {
    if sup.is_null() || index < 0 {
        return ptr::null_mut();
    }
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    let entry = {
        // SAFETY: caller keeps `sup` live through this nested-roster read.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard protects this scoped parallel-roster access.
        let s = &*guard;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        s.child_supervisors
            .get(i)
            .copied()
            .zip(s.child_supervisor_tokens.get(i).copied())
    };
    let Some((child_sup, token)) = entry else {
        return ptr::null_mut();
    };
    // The token, not the cached pointer, decides whether the slot still holds a
    // live nested supervisor.
    if crate::lifetime::local_handles::pin_current_supervisor(token)
        .is_some_and(|pin| pin.supervisor() == child_sup)
    {
        child_sup
    } else {
        ptr::null_mut()
    }
}

/// Return the child actor pointer at `index`, or null if out of range.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child(
    sup: *mut HewSupervisor,
    index: c_int,
) -> *mut HewActor {
    if sup.is_null() || index < 0 {
        return ptr::null_mut();
    }
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    load_child_slot(sup, i)
}

/// Return the child actor pointer at `index`, waiting up to `timeout_ms`
/// for the child to become available if it's currently being restarted.
///
/// Returns null if the child is still unavailable after the timeout, or if
/// the supervisor has been cancelled.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child_wait(
    sup: *mut HewSupervisor,
    index: c_int,
    timeout_ms: i32,
) -> *mut HewActor {
    if sup.is_null() || index < 0 {
        return ptr::null_mut();
    }
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0 check above")]
    let i = index as usize;
    // SAFETY: caller keeps `sup` live for the whole wait.
    let (epoch_lock, epoch_cv) = unsafe { &(*sup).restart_epoch };

    // Fast path: child is already available.
    let child = load_child_slot(sup, i);
    if !child.is_null() {
        return child;
    }

    // Slow path: child is being restarted. Wait on the restart condvar
    // instead of polling the slot without synchronization.
    #[expect(
        clippy::cast_sign_loss,
        reason = "timeout_ms is clamped to >= 0 by max(0)"
    )]
    let deadline =
        std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms.max(0) as u64);
    let mut guard = epoch_lock.lock_or_recover();
    loop {
        let child = load_child_slot(sup, i);
        if !child.is_null() {
            return child;
        }
        // If the supervisor was cancelled, don't wait forever.
        // SAFETY: caller keeps `sup` live for this atomic read.
        if unsafe { (*sup).cancelled.load(Ordering::Acquire) } {
            return ptr::null_mut();
        }
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return ptr::null_mut();
        }
        let (new_guard, wait_result) = epoch_cv.wait_timeout_or_recover(guard, remaining);
        guard = new_guard;
        if wait_result.timed_out() {
            return load_child_slot(sup, i);
        }
    }
}

/// Return the total number of children (actors + child supervisors).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "child counts fit in c_int for any reasonable supervisor"
)]
pub unsafe extern "C" fn hew_supervisor_child_count(sup: *mut HewSupervisor) -> c_int {
    if sup.is_null() {
        set_last_error("hew_supervisor_child_count: supervisor is null");
        return -1;
    }
    // SAFETY: caller keeps `sup` live through this roster-count read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped parallel-roster access.
    let s = &*guard;
    debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
    debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
    (s.child_count + s.child_supervisors.len()) as c_int
}

/// Look up a static child by its compile-time-assigned slot index.
///
/// Non-blocking. Acquires `roster` briefly to read the slot pointer
/// and discriminator fields atomically, then releases it and returns a
/// [`ChildLookupResult`] reflecting the slot state at observation time.
///
/// Discrimination logic (in priority order):
///
/// 1. Null or invalid `sup` → `Dead(SupervisorShutdown)`.
/// 2. `cancelled || running == 0` → `Dead(SupervisorShutdown)`.
/// 3. `key >= child_count` → `Dead(UnknownSlot)` (codegen bug; fail closed).
/// 4. Slot is non-null → `Live(handle)`.
/// 5. Slot is null, the spec is spent → `Dead(BudgetExhausted)`.
/// 6. Slot is null, `circuit_breaker.state == OPEN` → `Transient(CircuitOpen)`.
/// 7. Slot is null, `next_restart_time_ns > now` → `Transient(BackoffDelay)`.
/// 8. Slot is null, otherwise → `Transient(Restarting)`.
///
/// `BudgetExhausted` names every ruling in which this supervisor will not
/// restart the role while itself staying up: restart budget, a
/// `temporary`/`transient` policy decline, a tripped circuit breaker, or an
/// on-crash hook answering `Kill`. A ruling that also stops the supervisor
/// reaches `running == 0` first and reads `SupervisorShutdown`.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`] (or by a
/// nested-supervisor lookup). Behaviour is undefined if `sup` has been freed.
///
/// # C ABI
///
/// This function is part of the Hew v0.5 static-child lookup surface.
/// It is added to the MIR runtime-ABI allowlist in `hew-mir/src/runtime_symbols.rs`.
/// The MIR `CallRuntimeAbi` producer for dotted-access lowering is deferred
/// until the `Instr::CallRuntimeAbi` emitter shape is established.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_child_get(
    sup: *mut HewSupervisor,
    key: u32,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    child_get_from_supervisor(sup, key, ChildHandleKind::RawPointer)
}

#[derive(Clone, Copy, Debug)]
enum ChildHandleKind {
    RawPointer,
    StableLocalPid,
}

/// Resolve one static child while the supervisor allocation is known live.
///
/// Both public lookup ABIs share this discriminator authority. The stable form
/// copies the current incarnation's `LocalPid` token while `roster` is
/// held, so no caller can retain or dereference the child allocation after the
/// restart machinery replaces it.
fn child_get_from_supervisor(
    sup: *mut HewSupervisor,
    key: u32,
    handle_kind: ChildHandleKind,
) -> ChildLookupResult {
    // Fast-path: supervisor-level shutdown check (no lock required — atomics).
    // SAFETY: caller keeps `sup` live for these atomic reads.
    // SAFETY: caller keeps the allocation live through this atomic re-check.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    // Critical section: read the slot pointer AND the per-slot discriminator
    // fields under the same lock so the (pointer, CB-state, backoff-timer)
    // triple is consistent with one lifecycle state from the FSM in §2.2.
    //
    // The default scheduler runs one worker per core, so readers here race the
    // restart machinery (store_child_slot / restart_child_from_spec) on other
    // workers; `roster` is the exclusion that keeps the discriminator
    // triple coherent. Future optimization: migrate to AtomicPtr<HewActor> +
    // atomic discriminator fields so readers can avoid the mutex on the common
    // Live path.
    // SAFETY: caller keeps `sup` live through the classified roster lookup.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped child/spec access.
    let s = &*guard;

    // Re-check shutdown under the lock (the supervisor could have been
    // cancelled or run out of budget between the atomic check above and
    // acquiring the lock).
    // SAFETY: the stable-identity pin keeps the allocation live through this
    // atomic re-check.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = key as usize;
    if i >= s.child_count {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    let slot = s.children.get(i).copied().unwrap_or(ptr::null_mut());
    if !slot.is_null() {
        let handle = match handle_kind {
            ChildHandleKind::RawPointer => slot,
            ChildHandleKind::StableLocalPid => {
                // SAFETY: a non-null slot is owned by this supervisor and the
                // children lock prevents replacement/reclamation through this
                // read. Actor publication installs the token before publishing
                // the slot; an invalid token is an invariant failure and must
                // fail closed rather than exposing the allocation address.
                let token = unsafe { (*slot).local_pid_id };
                if token == crate::lifetime::local_handles::HewLocalPidId::INVALID {
                    return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
                }
                usize::from(token) as *mut HewActor
            }
        };
        return ChildLookupResult::live(handle);
    }

    // Slot is null — classify why using the per-child spec.
    classify_null_child_slot(s, i)
}

/// Classify a null child slot from its per-child spec (the FSM in §2.2):
/// circuit-breaker cooldown, backoff timer pending, or an active restart.
/// Shared by the classified lookups and the owner-scoped role ask so both
/// surfaces name the same slot state. Caller holds `roster`;
/// `child_specs` is parallel to `children` after `hew_supervisor_start`, so
/// the index is always valid here.
pub(crate) fn classify_null_child_slot(s: &SupervisorRoster, i: usize) -> ChildLookupResult {
    // ROSTER-GUARDED-HELPER: every caller holds this supervisor's
    // `roster` for the complete borrowed-spec classification.
    let spec = &s.child_specs[i];

    // The supervisor already ruled against restarting this role, so the slot
    // never refills. Dead, not Transient: a restart barrier must return rather
    // than wait for a restart that will not come.
    if spec.spent {
        return ChildLookupResult::dead(ChildSlotReason::BudgetExhausted);
    }

    // CB OPEN = circuit breaker is suppressing restarts during cooldown.
    // Value 1 = HEW_CIRCUIT_BREAKER_OPEN (from hew_supervisor_set_circuit_breaker).
    if spec.circuit_breaker.state == 1 {
        return ChildLookupResult::transient(ChildSlotReason::CircuitOpen);
    }

    // Backoff delay: next_restart_time_ns is a monotonic nanosecond deadline
    // set by restart_child_from_spec when exponential backoff is configured.
    // A non-zero value > now means the timer hasn't fired yet.
    let now_ns = monotonic_time_ns();
    if spec.next_restart_time_ns > 0 && spec.next_restart_time_ns > now_ns {
        return ChildLookupResult::transient(ChildSlotReason::BackoffDelay);
    }

    // Default transient: slot is null, no CB suppression, no pending backoff —
    // the restart machinery is actively spawning the replacement actor.
    ChildLookupResult::transient(ChildSlotReason::Restarting)
}

/// Look up a static child through a stable supervisor identity.
///
/// The returned `handle` word encodes the current child's stable `LocalPid`
/// token, never a child allocation pointer. Supervisor access is pinned for the
/// complete classified lookup; the child token is copied under `roster`.
/// A restart after return retires that exact token, so a subsequent token send
/// fails closed instead of retargeting reused storage.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_child_get(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
) -> ChildLookupResult {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    };
    let sup = pin.supervisor();
    let result = child_get_from_supervisor(sup, key, ChildHandleKind::StableLocalPid);
    drop(pin);
    result
}

/// Supervisors are unavailable on the wasm runtime; retain an exact symbol so
/// runtime-family parity stays exhaustive while codegen rejects the substrate.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_child_get(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
) -> ChildLookupResult {
    ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown)
}

/// Test-only hook fired inside [`role_resolve_current_child_id`] in the gap
/// between resolving the current child slot and returning its ID — with
/// `roster` HELD. The forced-interleaving regression installs a closure
/// that probes the lock from another thread at this exact point, proving the
/// restart machinery's slot writers (`store_child_slot` / `take_child_slot`)
/// cannot interpose inside the classified-resolution critical section.
#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) static ROLE_ASK_SUBMIT_GAP_HOOK: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> =
    Mutex::new(None);

/// Dynamically add a child by spec while the supervisor is running.
///
/// Unlike [`hew_supervisor_add_child_spec`], this function can be called
/// at any time — before or after [`hew_supervisor_start`].
///
/// Returns the child index (≥ 0) on success, -1 on error.
///
/// **State-drop registration**: this function does not accept a `state_drop_fn`
/// parameter. If the child actor type has owned heap fields, the caller must
/// invoke [`hew_supervisor_set_child_state_drop`] immediately after this
/// call returns — before any other thread can crash and restart the child:
///
/// ```text
/// let idx = hew_supervisor_add_child_dynamic(sup, spec);
/// if idx >= 0 {
///     hew_supervisor_set_child_state_drop(sup, idx, my_state_drop);
/// }
/// ```
///
/// If the supervisor is already running (`hew_supervisor_start` has been
/// called), the child is spawned immediately inside this call.  A crash
/// between the return of this function and the `set_child_state_drop` call
/// will restart the child without the drop callback, leaking any owned fields
/// in the original actor's state.  For most use-cases this window is
/// acceptable; the restart callback is wired before the child processes its
/// first message.  Callers that cannot tolerate any window should stop the
/// supervisor, add the child, register the drop, then restart.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `spec` must be a valid pointer to a [`HewChildSpec`].
/// - `spec.init_state` must be valid for `spec.init_state_size` bytes
///   (or null when `init_state_size` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_dynamic(
    sup: *mut HewSupervisor,
    spec: *const HewChildSpec,
) -> c_int {
    cabi_guard!(sup.is_null() || spec.is_null(), -1);
    // SAFETY: caller guarantees `spec` is valid.
    let sp = unsafe { &*spec };

    // The v0.6 init-closure restart model: a dynamic child carrying an init_fn
    // produces its state via the thunk; skip the byte-copy template (mirror
    // hew_supervisor_add_child_spec).
    let has_init_fn = sp.init_fn.is_some();

    // Deep-copy init state — only on the template (non-init_fn) path.
    let state_copy = if !has_init_fn && sp.init_state_size > 0 && !sp.init_state.is_null() {
        // SAFETY: init_state is valid for init_state_size bytes.
        let buf = crate::mem::buf_try_alloc(sp.init_state_size); // ALLOCATOR-PAIRING: GlobalAlloc
        if buf.is_null() {
            return -1;
        }
        // SAFETY: both pointers are valid.
        unsafe {
            ptr::copy_nonoverlapping(
                sp.init_state.cast::<u8>(),
                buf.cast::<u8>(),
                sp.init_state_size,
            );
        };
        buf
    } else {
        ptr::null_mut()
    };

    // Deep-copy name.
    let name_copy = if sp.name.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees name is a valid C string.
        // Portable strdup (libc::strdup unavailable on Windows-MSVC, #2505).
        unsafe { crate::cabi::cstr_strdup(sp.name) }
    };

    let mut internal_spec = InternalChildSpec {
        identity: 0,
        revision: 1,
        spent: false,
        name: name_copy,
        state_template: Arc::new(ChildStateTemplate {
            borrows_typed_fields: false,
            allocation: Arc::new(ChildStateTemplateAllocation {
                state: state_copy,
                size: if has_init_fn { 0 } else { sp.init_state_size },
                owns_typed_fields: false,
                state_drop: Arc::new(ChildStateDropDescriptor::new()),
            }),
            clone_fn: None,
        }),
        dispatch: sp.dispatch,
        restart_policy: sp.restart_policy,
        mailbox_capacity: sp.mailbox_capacity,
        overflow: sp.overflow,
        coalesce_key_fn: sp.coalesce_key_fn,
        coalesce_fallback: sp.coalesce_fallback,
        message_drop_fn: sp.message_drop_fn,
        sys_dispatch: sp.sys_dispatch,
        native_spawn: None,
        restart_delay_ms: 0,
        max_restart_delay_ms: DEFAULT_MAX_RESTART_DELAY_MS,
        next_restart_time_ns: 0,
        circuit_breaker: CircuitBreakerState::default(),
        arena_cap_bytes: sp.arena_cap_bytes,
        cycle_capable: sp.cycle_capable,
        on_crash: sp.on_crash,
        // Carried IN the spec literal (like on_crash) so the dynamic child's
        // initial spawn — which also routes through restart_child_from_spec —
        // fires the lifecycle wrapper.
        lifecycle_fn: sp.lifecycle_fn,
        // Carried IN the spec literal so the dynamic child's initial spawn uses
        // the thunk (the load-bearing first-spawn carrier).
        init_fn: sp.init_fn,
        // The exact adopted buffer is installed atomically with roster
        // reservation below. No supervisor borrow crosses this construction.
        config: ptr::null_mut(),
    };

    let (i, should_spawn) = {
        // SAFETY: caller keeps `sup` live; this guard is the sole authority for
        // config adoption and the complete child/spec placeholder reservation.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;

        // Adopt the supervisor config buffer (idempotent on the same pointer)
        // in the same transaction that publishes the spec borrowing it.
        if has_init_fn && !sp.config.is_null() {
            if s.config_buf.is_null() {
                s.config_buf = sp.config;
                s.config_size = sp.config_size;
            } else if s.config_buf != sp.config {
                set_last_error(
                    "hew_supervisor_add_child_dynamic: conflicting supervisor config buffer",
                );
                // SAFETY: `sp.config` is a sized-block-allocated orphan distinct
                // from the already-adopted buffer (ALLOCATOR-PAIRING: GlobalAlloc).
                unsafe { crate::mem::buf_free(sp.config) };
                return -1;
            }
            internal_spec.config = s.config_buf;
        }

        let i = s.child_count;
        internal_spec.identity = s.next_child_spec_identity;
        let Some(next_identity) = s.next_child_spec_identity.checked_add(1) else {
            set_last_error("hew_supervisor_add_child_dynamic: child-spec identity exhausted");
            return -1;
        };
        s.next_child_spec_identity = next_identity;
        s.child_specs.push(internal_spec);
        s.children.push(ptr::null_mut());
        s.child_count += 1;
        debug_assert_eq!(s.children.len(), s.child_specs.len());
        // SAFETY: caller keeps `sup` live; running is atomic and independent of
        // the mutex-protected roster transaction.
        (i, unsafe { (*sup).running.load(Ordering::Acquire) != 0 })
    };

    run_dynamic_child_reserved_hook_for_test();

    // Spawn the child if the supervisor is running.
    if should_spawn {
        // SAFETY: spec is valid.
        unsafe { restart_child_from_spec(sup, i) };
    }
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "child index fits in c_int for any reasonable supervisor"
    )]
    {
        i as c_int
    }
}

/// Remove a child from the supervisor by index.
///
/// Stops the child actor and removes it from the supervisor's child list.
/// Returns 0 on success, -1 on error.
///
/// Note: This performs a swap-remove. The child at `child_index` is swapped
/// with the last child, so the order of remaining children may change.
/// The removed child's actor is stopped and freed.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_remove_child(
    sup: *mut HewSupervisor,
    child_index: c_int,
) -> c_int {
    if sup.is_null() || child_index < 0 {
        return -1;
    }
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // Extract the complete ownership unit under the same lock used by restart
    // snapshots and setters. `InternalChildSpec` (including its Arc template)
    // is then dropped normally outside the lock; no manual wrapper-only free
    // can bypass typed template teardown.
    let (child, removed_spec) = {
        // SAFETY: caller guarantees `sup` live; roster references exist only
        // inside this lock scope.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        if idx >= s.child_count {
            return -1;
        }
        debug_assert_eq!(s.children.len(), s.child_specs.len());
        debug_assert_eq!(s.child_count, s.children.len());

        let previous_last = s.child_count - 1;
        let child = s.children.swap_remove(idx);
        let removed_spec = s.child_specs.swap_remove(idx);
        s.child_count -= 1;

        // Static-backed pool membership names child roster indices. Apply the
        // same swap-remove mapping atomically: remove the retired membership,
        // then retarget references to the former last child to its new slot.
        for pool_spec in &mut s.pool_specs {
            pool_spec.static_members.retain(|member| *member != idx);
            if idx != previous_last {
                for member in &mut pool_spec.static_members {
                    if *member == previous_last {
                        *member = idx;
                    }
                }
            }
        }

        if idx < s.child_count {
            let swapped = s.children[idx];
            if !swapped.is_null() {
                // SAFETY: the swapped child remains live and is now owned by
                // the roster slot at `idx`.
                #[expect(
                    clippy::cast_possible_truncation,
                    clippy::cast_possible_wrap,
                    reason = "child index fits in i32 for any reasonable child count"
                )]
                // SAFETY: `swapped` is the live actor retained in the roster.
                unsafe {
                    (*swapped).supervisor_child_index = idx as i32;
                }
            }
        }
        (child, removed_spec)
    };

    // Stop and free the extracted actor after releasing the roster lock.
    if !child.is_null() {
        // SAFETY: child pointer is valid.
        unsafe { actor::hew_actor_stop(child) };
        // SAFETY: child was stopped.
        unsafe { actor::hew_actor_free(child) };
    }
    drop(removed_spec);
    0
}

/// Declare that a shallow child's typed fields are externally borrowed.
///
/// Returns 0 on success or -1 for an invalid slot or an owning clone/init
/// source. This does not transfer ownership of any field. It permits replaying
/// the shallow template even when a type-level drop callback is registered;
/// neither the template nor its child incarnations may call that callback.
/// Ordinary owning state must use an init thunk or matching clone/drop pair.
///
/// The immutable template generation records this contract under the roster
/// lock. An in-flight restart using the previous generation cannot publish
/// across this change. A later clone registration cannot acquire the external
/// owner's fields by clearing borrowed provenance.
///
/// # Safety
///
/// `sup` must remain live for the call. Every pointer embedded in the initial
/// state must refer to storage that its external owner keeps alive until the
/// spec and ALL child incarnations have finished reclamation, including any
/// deferred teardown. Dispatch, lifecycle and crash callbacks must not release,
/// consume, or replace these borrowed fields with owning values. Shared data
/// may only be mutated through synchronization appropriate for every alias.
/// The initial wrapper itself is copied at registration and need not survive.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_state_borrowed(
    sup: *mut HewSupervisor,
    child_index: c_int,
) -> c_int {
    if sup.is_null() {
        return -1;
    }
    let Ok(index) = usize::try_from(child_index) else {
        return -1;
    };
    // SAFETY: the caller keeps sup live; the roster lock serializes generations.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    let Some(spec) = roster.child_specs.get_mut(index) else {
        return -1;
    };
    if spec.init_fn.is_some()
        || spec.state_template.clone_fn.is_some()
        || spec.state_template.allocation.owns_typed_fields
    {
        set_last_error(
            "hew_supervisor_set_child_state_borrowed: owning state source cannot become borrowed",
        );
        return -1;
    }
    let Some(revision) = spec.revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_state_borrowed: child-spec revision exhausted");
        return -1;
    };
    spec.state_template = Arc::new(ChildStateTemplate {
        allocation: Arc::clone(&spec.state_template.allocation),
        clone_fn: None,
        borrows_typed_fields: true,
    });
    spec.revision = revision;
    if let Some(&child) = roster.children.get(index) {
        if !child.is_null() {
            // SAFETY: the slot belongs to this shallow spec and the caller
            // explicitly preserves external field ownership on this incarnation.
            unsafe { actor::mark_state_drop_borrowed(child) };
        }
    }
    0
}

/// Register a state-drop callback for a child actor spec.
///
/// Called by codegen immediately after [`hew_supervisor_add_child_spec`] to
/// attach the actor-type's drop function to the internal spec. Every restart
/// path (initial spawn and all subsequent restarts) calls the registered
/// function on the newly spawned actor so that heap-allocated state fields
/// (e.g. `Vec`, `String`) are freed on teardown.
///
/// `child_index` is the zero-based index of the child whose spec should be
/// updated. Indices are stable until [`hew_supervisor_remove_child`] is called.
/// A shallow spec must separately establish the explicit contract through
/// [`hew_supervisor_set_child_state_borrowed`] to restart with a drop callback
/// and no clone/init source. A drop descriptor alone is not borrow authority.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `state_drop_fn` must be a valid function pointer with C ABI that accepts
///   a `*mut c_void` pointing to the actor's state struct and frees every
///   heap-allocated field inside it without freeing the struct itself.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_state_drop(
    sup: *mut HewSupervisor,
    child_index: c_int,
    state_drop_fn: unsafe extern "C" fn(*mut c_void),
) {
    if sup.is_null() || child_index < 0 {
        return;
    }
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // Publish the descriptor and back-fill the current incarnation in one
    // roster critical section. Every immutable template generation shares the
    // same atomic descriptor, so an outstanding restart lease also observes it.
    // SAFETY: caller guarantees `sup` live; mutation is scoped to this roster
    // critical section.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if idx >= s.child_count {
        return;
    }
    s.child_specs[idx]
        .state_template
        .allocation
        .state_drop
        .store(state_drop_fn);

    // Register on the already-spawned actor for its first run (the initial
    // spawn happens inside add_child_spec before this setter is called).
    let child = s.children[idx];
    if !child.is_null() {
        // SAFETY: child is a valid actor pointer; state_drop_fn has the
        // correct signature.
        unsafe { actor::hew_actor_set_state_drop(child, state_drop_fn) };
    }
}

/// Register the lifecycle wrapper for a child actor spec.
///
/// Codegen emits this call after [`hew_supervisor_add_child_spec`] for parity
/// with the state setters. It stores the wrapper pointer on the spec so it is
/// available to symmetry consumers and to any future code path that rebuilds a
/// spec without the literal carrier.
///
/// **It does NOT fire the wrapper on the already-spawned child.** UNLIKE
/// [`hew_supervisor_set_child_state_drop`] (which back-fills the running
/// actor), the initial supervised spawn's lifecycle fire already happened
/// inside `add_child_spec` → `restart_child_from_spec`, reading the
/// `lifecycle_fn` carried IN the `HewChildSpec` literal (copied at spec
/// registration). Firing here too would run `init()` / `#[on(start)]` a SECOND
/// time on the initial incarnation. The literal field is the load-bearing
/// carrier for the initial fire; this setter is back-fill symmetry only.
///
/// `child_index` is the zero-based index of the child whose spec should be
/// updated. Indices are stable until [`hew_supervisor_remove_child`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `lifecycle_fn` must be a valid C-ABI function pointer matching the
///   [`HewLifecycleFn`] contract (takes the actor pointer, runs `init` /
///   `on_start` under the actor state lock, registers the terminate hook).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_lifecycle(
    sup: *mut HewSupervisor,
    child_index: c_int,
    lifecycle_fn: HewLifecycleFn,
) {
    if sup.is_null() || child_index < 0 {
        return;
    }
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // SAFETY: caller guarantees `sup` live; lifecycle metadata is part of the
    // synchronized child spec roster.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if idx >= s.child_count {
        return;
    }

    // Store only. The initial-spawn fire already ran inside add_child_spec
    // (reading the literal-carried pointer); do NOT re-fire here.
    let Some(next_revision) = s.child_specs[idx].revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_lifecycle: child-spec revision exhausted");
        return;
    };
    s.child_specs[idx].lifecycle_fn = Some(lifecycle_fn);
    s.child_specs[idx].revision = next_revision;
}

/// Register a state-clone callback for a child actor spec, breaking the
/// initial-spawn byte-alias between the spec's `init_state` template and the
/// running actor's `state` allocation.
///
/// Called by codegen (Lane A2) immediately after [`hew_supervisor_add_child_spec`]
/// (or [`hew_supervisor_add_child_dynamic`]). Stores the clone fn on the spec so
/// future restart paths use it (see `restart_child_from_spec`), back-fills it
/// on the already-spawned child actor for symmetry, **and** — critically —
/// re-clones `spec.init_state` in place using the freshly-registered
/// `state_clone_fn`, replacing the byte-copy template that
/// `hew_supervisor_add_child_spec` installed.
///
/// **Why the in-place re-clone**: prior to this setter, the spec's
/// `init_state` is a `memcpy` of the user-supplied template, and the initial
/// actor's `state` is a `memcpy` of *that* — meaning all three wrappers
/// share identical byte patterns including embedded heap pointers
/// (`Vec.ptr`, `String.ptr`, IO handles). When the actor first mutates or
/// reallocates an owned field, the spec's wrapper carries a dangling pointer
/// (root cause of audit C1 UAF). Re-cloning the spec at registration time —
/// while the actor is still idle in its mailbox queue and has not yet
/// dispatched a message — converts `spec.init_state` into an independently-
/// owned deep clone. Subsequent restarts then deep-clone *that* clean
/// template via the same `state_clone_fn`.
///
/// **Race window**: codegen emits this setter call back-to-back with
/// `hew_supervisor_add_child_spec` in the same basic block; the spawned
/// actor's mailbox is empty at this point, so no dispatch can have run yet.
/// This matches the calling contract documented on
/// [`actor::hew_actor_set_state_drop`].
///
/// **OOM on re-clone**: if the in-place clone fails (`clone_fn` returns null),
/// the spec retains its byte-copy template and borrowed initial incarnation.
/// The registered clone callback is retried on each restart; failure refuses
/// that restart rather than falling back to an owning byte copy.
///
/// `child_index` is the zero-based index of the child whose spec should be
/// updated. Indices are stable until [`hew_supervisor_remove_child`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `state_clone_fn` must satisfy the [`actor::HewStateCloneFn`] contract
///   (deep-cloning, `malloc`-compatible output, null on OOM).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_state_clone(
    sup: *mut HewSupervisor,
    child_index: c_int,
    state_clone_fn: actor::HewStateCloneFn,
) {
    if sup.is_null() || child_index < 0 {
        return;
    }
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // Lease the current immutable generation under the roster lock, then run
    // the unsafe callback lock-free. The Arc keeps `template_ptr` alive across
    // concurrent replacement/removal and permits callback re-entrancy.
    let (spec_identity, old_template) = {
        // SAFETY: caller guarantees `sup` live; this roster reference is
        // released before invoking the callback below.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard protects this scoped immutable roster access.
        let s = &*guard;
        if idx >= s.child_count {
            return;
        }
        if s.child_specs[idx].state_template.borrows_typed_fields {
            set_last_error("hew_supervisor_set_child_state_clone: externally borrowed state cannot transfer ownership");
            return;
        }
        (
            s.child_specs[idx].identity,
            Arc::clone(&s.child_specs[idx].state_template),
        )
    };

    let template_ptr = old_template.allocation.state;
    let template_size = old_template.allocation.size;
    let mut transferred_initial_state_ownership = false;
    let new_allocation = if template_size > 0 && !template_ptr.is_null() {
        // SAFETY: template_ptr is a sized-block wrapper of template_size bytes
        // produced by hew_supervisor_add_child_spec's byte-copy; the
        // contract of state_clone_fn admits reading from such a wrapper as
        // long as it has not yet been mutated. The race-window analysis in
        // the doc comment justifies the no-mutation precondition.
        let fresh = unsafe { state_clone_fn(template_ptr.cast_const()) };
        if fresh.is_null() {
            Arc::clone(&old_template.allocation)
        } else {
            transferred_initial_state_ownership = true;
            Arc::new(ChildStateTemplateAllocation {
                state: fresh,
                size: template_size,
                owns_typed_fields: true,
                state_drop: Arc::clone(&old_template.allocation.state_drop),
            })
        }
    } else {
        Arc::clone(&old_template.allocation)
    };

    let new_template = Arc::new(ChildStateTemplate {
        borrows_typed_fields: false,
        allocation: new_allocation,
        clone_fn: Some(state_clone_fn),
    });

    // Install only if this exact spec still occupies the slot. A remove/swap
    // or competing setter may have advanced it while the callback ran.
    // SAFETY: callback returned and no supervisor reference crossed it;
    // reacquire before creating a scoped mutable roster reference.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if s.child_specs.get(idx).is_none_or(|spec| {
        spec.identity != spec_identity || !Arc::ptr_eq(&spec.state_template, &old_template)
    }) {
        return;
    }
    let Some(next_revision) = s.child_specs[idx].revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_state_clone: child-spec revision exhausted");
        return;
    };
    s.child_specs[idx].state_template = new_template;
    s.child_specs[idx].revision = next_revision;

    // Register on the already-spawned actor for its first run (the initial
    // spawn happens inside add_child_spec before this setter is called).
    let child = s.children[idx];
    if !child.is_null() {
        // Install the clone descriptor before publishing owned provenance. A
        // dispatch that observes owned provenance therefore cannot observe a
        // half-registered actor.
        // SAFETY: child is a valid actor pointer; state_clone_fn has the
        // correct signature.
        unsafe { actor::hew_actor_set_state_clone(child, state_clone_fn) };
        if transferred_initial_state_ownership {
            // The old shallow template wrapper is gone and its field owners
            // now belong solely to the initial actor. Clear only the borrowed
            // provenance bit; a racing crash-escrow consumption remains set in
            // its independent atomic authority.
            // SAFETY: child is the live initial incarnation whose alias was
            // broken by the successful clone immediately above.
            unsafe { actor::mark_state_drop_owned(child) };
        }
    }
}

/// Register the per-child init thunk for the v0.6 init-closure restart model.
///
/// The thunk PRODUCES a fresh, independently-owned actor state on the initial
/// spawn AND every restart by re-running the child's init-arg expressions
/// against the supervisor's construction-time config (the `config` buffer). It
/// REPLACES the byte-copy state template, making owned (`string`/`Vec`) init
/// args sound under restart — each incarnation gets unaliased owned values.
///
/// **The load-bearing carrier is the `HewChildSpec` literal, not this setter.**
/// Codegen rides `init_fn` + `config` + `config_size` IN the spec literal so the
/// INITIAL supervised spawn — which fires inside `hew_supervisor_add_child_spec`
/// before any post-hoc setter runs — already uses the thunk. This setter is
/// back-fill / symbol-stability symmetry (mirroring
/// `hew_supervisor_set_child_state_clone`), and is also the additive ABI entry
/// point out-of-tree C callers use to install a thunk after `add_child_spec`.
///
/// Config-buffer ownership: the supervisor adopts `config` ONCE (the first
/// non-null registration) and frees it EXACTLY ONCE at teardown
/// (`stop_supervisor_owned`). Subsequent registrations with the same pointer are
/// idempotent; a conflicting non-null pointer is a codegen ABI error (one config
/// buffer per supervisor). The thunk only ever READS `config`.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child_index` must be a valid index (0 ≤ index < `child_count`).
/// - `init_fn` must satisfy the [`HewChildInitFn`] contract (produces a fresh
///   owned state wrapper; `state == null` on OOM).
/// - `config` must be null, or a `malloc`-compatible heap allocation of
///   `config_size` bytes whose ownership transfers to the supervisor on the
///   first registration.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_child_init_fn(
    sup: *mut HewSupervisor,
    child_index: c_int,
    init_fn: HewChildInitFn,
    config: *mut c_void,
    config_size: usize,
) {
    if sup.is_null() || child_index < 0 {
        return;
    }
    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let idx = child_index as usize;

    // SAFETY: caller guarantees `sup` live; init/config publication is part of
    // the child spec roster and never calls `init_fn` under this lock.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if idx >= s.child_count {
        return;
    }

    let Some(next_revision) = s.child_specs[idx].revision.checked_add(1) else {
        set_last_error("hew_supervisor_set_child_init_fn: child-spec revision exhausted");
        return;
    };

    // Adopt the supervisor-owned config buffer once; idempotent on the same
    // pointer. (The literal carrier already adopted it inside add_child_spec for
    // the initial spawn; this keeps the setter self-consistent for out-of-tree
    // callers that install the thunk post-hoc.)
    if !config.is_null() {
        if s.config_buf.is_null() {
            s.config_buf = config;
            s.config_size = config_size;
        } else if s.config_buf != config {
            // Conflicting non-null config pointer — unreachable from correct
            // codegen (the setter runs after add_child_spec adopted the SAME
            // buffer). debug_assert in debug; in release free the rejected
            // duplicate (fail closed, no leak). It differs from the adopted
            // buffer, so this cannot double-free it.
            debug_assert!(
                false,
                "hew_supervisor_set_child_init_fn: child {idx} config buffer ({config:p}) \
                 differs from the supervisor's adopted buffer ({:p}); codegen must emit ONE \
                 config buffer per supervisor",
                s.config_buf
            );
            // SAFETY: config is a sized-block orphan distinct from the
            // adopted buffer (ALLOCATOR-PAIRING: GlobalAlloc).
            unsafe { crate::mem::buf_free(config) };
        }
    }

    s.child_specs[idx].init_fn = Some(init_fn);
    s.child_specs[idx].config = s.config_buf;
    s.child_specs[idx].revision = next_revision;
}

/// Register the config struct's drop-inplace glue so the supervisor releases the
/// config buffer's OWNED inner fields (`string`/`bytes`/…) at teardown, before
/// the flat `buf_free` of the buffer.
///
/// The config buffer is a flat snapshot of the moved-in config value and OWNS
/// its inner owned fields (the init thunks only CLONE from them). Without this
/// drop glue those fields leak at teardown. Codegen calls this once, after the
/// config buffer is materialised, when the config struct has any owned field.
/// An all-scalar config never calls it (`config_drop_fn` stays `None`).
///
/// Idempotent: re-registration with the same fn is a no-op; a CONFLICTING fn is
/// a codegen ABI error (one config struct type per supervisor) — `debug_assert`
/// in debug, last-writer-wins in release (both fns drop the same struct layout,
/// so neither leaks nor double-frees).
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `drop_fn` must be the `__hew_record_drop_inplace_<T>` for the config
///   struct type whose instance backs `config_buf`.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_config_drop_fn(
    sup: *mut HewSupervisor,
    drop_fn: unsafe extern "C" fn(*mut c_void),
) {
    if sup.is_null() {
        return;
    }
    // SAFETY: caller guarantees sup is valid; config ownership is serialized
    // with the child specs whose init thunks borrow it.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable metadata access.
    let s = &mut *guard;
    debug_assert!(
        s.config_drop_fn
            .is_none_or(|f| std::ptr::fn_addr_eq(f, drop_fn)),
        "hew_supervisor_set_config_drop_fn: a different config drop fn is already \
         registered; codegen must register ONE config drop fn per supervisor"
    );
    s.config_drop_fn = Some(drop_fn);
}

// ── Circuit breaker constants for C ABI ────────────────────────────────────────

/// Circuit breaker state: CLOSED (normal operation).
#[no_mangle]
pub static HEW_CIRCUIT_BREAKER_CLOSED: c_int = 0;

/// Circuit breaker state: OPEN (blocking restarts).
#[no_mangle]
pub static HEW_CIRCUIT_BREAKER_OPEN: c_int = 1;

/// Circuit breaker state: `HALF_OPEN` (probe restart).
#[no_mangle]
pub static HEW_CIRCUIT_BREAKER_HALF_OPEN: c_int = 2;

// ── Cooperative restart-await observer (`await_restart`) ─────────────────────

/// Codegen ABI: the `await_restart` parked the continuation; the runtime wakes
/// it via `enqueue_resume` when the restart cycle completes. The caller MUST
/// `coro.suspend`.
pub const RESTART_AWAIT_SUSPEND: i32 = 0;
/// Codegen ABI: the role is settled — it holds a running incarnation with no
/// fault pending under it, or it is permanently Dead (will never restart). The
/// caller MUST NOT suspend and resumes immediately on the bind edge —
/// re-resolving the slot, which is either Live (proceed) or fails closed at the
/// send re-resolve (never an infinite hang).
pub const RESTART_AWAIT_READY: i32 = 1;
