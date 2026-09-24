//! Supervisor creation, start/stop and crash-notification entry points.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

pub(crate) fn notify_child_supervisor_stopped(
    parent: crate::lifetime::local_handles::HewLocalPidId,
    slot: u32,
    child: crate::lifetime::local_handles::HewLocalPidId,
) {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(parent) else {
        return;
    };
    // SAFETY: the parent pin protects the self actor through mailbox admission.
    let self_actor = unsafe { (*pin.supervisor()).self_actor };
    if self_actor.is_null() {
        return;
    }
    let event = ChildSupervisorStopped {
        supervisor_index: slot,
        child_token: child,
    };
    // SAFETY: the parent pin retains the target; admission copies this event.
    unsafe {
        actor::send_system_message(
            self_actor,
            HewSysMsg::ChildSupervisorStopped,
            (&raw const event).cast_mut().cast(),
            std::mem::size_of::<ChildSupervisorStopped>(),
        );
    }
}

pub(crate) unsafe fn supervisor_sys_dispatch_impl(
    ctx: *mut crate::execution_context::HewExecutionContext,
    state: *mut c_void,
    sys_msg: i32,
    data: *mut c_void,
    data_size: usize,
) {
    if state.is_null() {
        return;
    }
    let sup = state.cast::<HewSupervisor>();

    // SAFETY: state points to the live supervisor backing this dispatch.
    if unsafe { (*sup).running.load(Ordering::Acquire) } == 0 {
        return;
    }

    // Fail-closed decode. The scheduler already validated this value against
    // the closed set; re-decoding here keeps the callee independent of that
    // guarantee rather than trusting a raw integer.
    let Some(kind) = HewSysMsg::from_raw(sys_msg) else {
        eprintln!("[supervisor] refusing system signal with unknown kind {sys_msg}");
        return;
    };

    match kind {
        HewSysMsg::ChildStopped | HewSysMsg::ChildCrashed => {
            // SAFETY: forwarded unchanged; `sup`/`ctx` are this dispatch's own.
            unsafe { dispatch_child_lifecycle_event(sup, ctx, data, data_size) };
        }
        HewSysMsg::ChildSupervisorEscalated => {
            if data.is_null() || data_size < std::mem::size_of::<ChildSupervisorEscalation>() {
                return;
            }
            // SAFETY: data is valid for at least sizeof(ChildSupervisorEscalation).
            let event = unsafe { &*data.cast::<ChildSupervisorEscalation>() };
            crate::tracing::ensure_supervisor_trace_root();
            let idx = event.supervisor_index as usize;
            let record = FaultRecord::from_raw(event.fault_record);
            // SAFETY: dispatch retains the parent while its slot identity is checked.
            let current = unsafe { &(*sup).roster }
                .lock_or_recover()
                .child_supervisor_tokens
                .get(idx)
                .copied()
                == Some(event.child_token);
            if !current {
                crate::exit_status::settle_supervised_fault(record, FaultRuling::Unrecovered);
                return;
            }
            // SAFETY: parent supervisor is valid for the lifetime of this dispatch.
            // This is the parent's RULING on the escalated record: it settles
            // the very record the child transferred, clearing it when the
            // subtree comes back.
            let ruling = unsafe { restart_child_supervisor_with_budget(sup, idx, record) };
            crate::exit_status::settle_supervised_fault(record, ruling);
            wake_restart_waiters(sup);
        }
        HewSysMsg::ChildSupervisorStopped => {
            if data.is_null() || data_size < std::mem::size_of::<ChildSupervisorStopped>() {
                return;
            }
            // SAFETY: the envelope contains this typed notification.
            let event = unsafe { &*data.cast::<ChildSupervisorStopped>() };
            // SAFETY: this dispatch retains the supervisor across its child transition.
            unsafe { dispatch_child_supervisor_stopped(sup, event) };
        }
        HewSysMsg::SupervisorStop => {
            // SAFETY: dispatch keeps the supervisor live.
            unsafe {
                publish_supervisor_cancellation(sup);
                (*sup).running.store(0, Ordering::Release);
            }
            let mut retained = Vec::new();
            for (child_sup, child_token, child_spec) in take_nested_supervisor_roster(sup) {
                if !child_sup.is_null() {
                    retain_nested_completion(sup, child_token);
                    // Admission failure means no teardown winner exists yet;
                    // retain the parent ownership edge for the later canonical
                    // parent stop. Once admission succeeds, the stable-token
                    // claim decides ownership: only its winner detaches and
                    // publishes, while a loser never touches the child pointer.
                    let Some(teardown) =
                        crate::lifetime::local_handles::begin_current_supervisor_teardown()
                    else {
                        retained.push((child_sup, child_token, child_spec));
                        continue;
                    };
                    // SAFETY: pointer/token are one extracted parent entry.
                    if unsafe { claim_nested_supervisor_for_detach(child_sup, child_token) } {
                        // SAFETY: the successful claim is passed explicitly;
                        // this stop path must not race the claim a second time.
                        unsafe {
                            stop_supervisor_with_teardown_authority(child_sup, teardown, true);
                        };
                    }
                }
            }
            if !retained.is_empty() {
                // SAFETY: dispatch keeps `sup` live through this re-publication.
                let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
                // SAFETY: the guard serializes this scoped mutable roster access.
                let s = &mut *guard;
                for (child_sup, child_token, child_spec) in retained {
                    s.child_supervisors.push(child_sup);
                    s.child_supervisor_tokens.push(child_token);
                    s.child_supervisor_specs.push(child_spec);
                }
            }
            // Stop each retained actor while the roster lock prevents dynamic
            // removal from reclaiming the pointer being stopped.
            // SAFETY: dispatch keeps `sup` live for the critical section.
            let guard = unsafe { &(*sup).roster }.lock_or_recover();
            // SAFETY: the guard protects this scoped roster traversal.
            let s = &*guard;
            for child in &s.children {
                if !child.is_null() {
                    // SAFETY: child pointer is valid.
                    unsafe { actor::hew_actor_stop(*child) };
                }
            }
        }
        HewSysMsg::DelayedRestart => {
            if data.is_null() || data_size < std::mem::size_of::<DelayedRestartEvent>() {
                return;
            }
            // SAFETY: data is valid for at least sizeof(DelayedRestartEvent).
            let event = unsafe { &*data.cast::<DelayedRestartEvent>() };
            // S3: establish a sampled root for the delayed-restart span too
            // (this dispatch was woken by a timer-thread sys-send, which may
            // carry a zero trace context).
            crate::tracing::ensure_supervisor_trace_root();
            let record = FaultRecord::from_raw(event.fault_record);
            // SAFETY: the stable identity is resolved under the roster lock;
            // retired dynamic children are ignored. The armed timer transferred
            // the record here; the restart's EFFECT is the ruling.
            let ruling =
                unsafe { restart_with_budget_and_strategy(sup, event.child_identity, record) };
            crate::exit_status::settle_supervised_fault(record, ruling);
            wake_restart_waiters(sup);
        }
        HewSysMsg::GroupRestart => {
            // SAFETY: forwards this dispatch's live supervisor.
            unsafe { dispatch_group_restart(sup) };
        }
        // A supervisor's own actor is never linked or monitored by the runtime.
        HewSysMsg::Exit | HewSysMsg::Down => {}
    }
}

// ---------------------------------------------------------------------------
// Public C ABI
// ---------------------------------------------------------------------------

/// Create a new supervisor.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_new(
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,
) -> *mut HewSupervisor {
    let runtime = crate::runtime::rt_current();
    let sup = Box::new(HewSupervisor {
        runtime: runtime as *const crate::runtime::RuntimeInner,
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        strategy,
        max_restarts,
        window_secs,
        parent: ptr::null_mut(),
        index_in_parent: 0,
        running: AtomicI32::new(0),
        cancelled: AtomicBool::new(false),
        restart_timers: Arc::new(RestartTimerControl::new()),
        self_actor: ptr::null_mut(),
        roster: Mutex::new(SupervisorRoster {
            children: Vec::with_capacity(SUP_INITIAL_CAPACITY),
            child_specs: Vec::with_capacity(SUP_INITIAL_CAPACITY),
            child_count: 0,
            next_child_spec_identity: 1,
            child_supervisors: Vec::new(),
            child_supervisor_tokens: Vec::new(),
            child_supervisor_specs: Vec::new(),
            retiring_children: Vec::new(),
            pending_group_restart: None,
            restart_times: [0u64; MAX_RESTARTS_TRACK],
            restart_count: 0,
            restart_head: 0,
            pool_slots: Vec::new(),
            pool_specs: Vec::new(),
            config_buf: ptr::null_mut(),
            config_size: 0,
            config_drop_fn: None,
        }),
        restart_epoch: (Mutex::new(0), Condvar::new()),
        restart_await_waiters: Mutex::new(Vec::new()),
    });
    let raw = Box::into_raw(sup); // ALLOCATOR-PAIRING: GlobalAlloc
    let publication = match crate::lifetime::local_handles::begin_supervisor_publication_in(
        &runtime.local_handles,
    ) {
        Ok(publication) => publication,
        Err(error) => {
            set_last_error(format!(
                "hew_supervisor_new: handle admission failed: {error:?}"
            ));
            // SAFETY: publication failed before any control or route was stored.
            drop(unsafe { Box::from_raw(raw) });
            return ptr::null_mut();
        }
    };
    match publication.register_supervisor(runtime.runtime_id(), raw) {
        Ok(token) => {
            // SAFETY: `raw` remains exclusively construction-owned here.
            unsafe { (*raw).local_pid_id = token };
            raw
        }
        Err(error) => {
            set_last_error(format!(
                "hew_supervisor_new: handle registration failed: {error:?}"
            ));
            // SAFETY: registration rolled back without publishing this pointer.
            drop(unsafe { Box::from_raw(raw) });
            ptr::null_mut()
        }
    }
}

/// Add a child via a child spec.
///
/// The supervisor deep-copies `init_state` and `name` from the spec.
/// The caller retains ownership of the original spec and its fields
/// (including `init_state`) and must free them independently.
/// The supervisor frees its internal copies when
/// [`hew_supervisor_stop`] is called.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `spec` must be a valid pointer to a [`HewChildSpec`].
/// - `spec.init_state` must be valid for `spec.init_state_size` bytes
///   (or null when `init_state_size` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_add_child_spec(
    sup: *mut HewSupervisor,
    spec: *const HewChildSpec,
) -> c_int {
    cabi_guard!(sup.is_null() || spec.is_null(), -1);
    // SAFETY: caller guarantees `spec` is a valid, aligned, initialized `HewChildSpec` pointer.
    let sp = unsafe { &*spec };

    // The v0.6 init-closure restart model: when the spec carries an `init_fn`,
    // the thunk is THE state source on the initial spawn and every restart.
    // Skip the byte-copy state template entirely — `restart_child_from_spec`
    // ignores `init_state` on the thunk path, and capturing a template here
    // would re-introduce the owned-field aliasing hazard the thunk model fixes.
    let has_init_fn = sp.init_fn.is_some();

    // Copy init wrapper bytes — only when there is no init_fn (the thunk path
    // produces state directly, leaving init_state null).
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
                // On the thunk path the state size is produced by the thunk
                // result, not the spec; keep it 0 so no template path can read
                // a stale size.
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
        // Carried IN the spec literal (like on_crash) so the initial supervised
        // spawn — which happens inside this call via restart_child_from_spec —
        // fires the lifecycle wrapper. A post-hoc setter would run too late to
        // cover the initial fire (see hew_supervisor_set_child_lifecycle).
        lifecycle_fn: sp.lifecycle_fn,
        // Carried IN the spec literal (like lifecycle_fn) so the INITIAL spawn
        // below uses the thunk — the load-bearing first-spawn carrier. The
        // post-hoc setter is back-fill/symmetry only.
        init_fn: sp.init_fn,
        // Installed from the exact adopted supervisor config in the reservation
        // transaction below.
        config: ptr::null_mut(),
    };

    // Reserve a complete null child/spec slot in one roster critical section.
    // No callback runs under the lock; restart below validates the reserved
    // identity and publishes into the placeholder.
    let i = {
        // SAFETY: caller keeps `sup` live; config adoption and complete roster
        // reservation are one lock-protected transaction.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;
        if has_init_fn && !sp.config.is_null() {
            if s.config_buf.is_null() {
                s.config_buf = sp.config;
                s.config_size = sp.config_size;
            } else if s.config_buf != sp.config {
                set_last_error("hew_supervisor_add_child_spec: conflicting config buffer");
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
            set_last_error("hew_supervisor_add_child_spec: child-spec identity exhausted");
            return -1;
        };
        s.next_child_spec_identity = next_identity;
        s.child_specs.push(internal_spec);
        s.children.push(ptr::null_mut());
        s.child_count += 1;
        debug_assert_eq!(s.children.len(), s.child_specs.len());
        i
    };

    // SAFETY: the exact identity-backed placeholder was reserved above.
    unsafe { restart_child_from_spec(sup, i) };
    0
}

/// Start the supervisor (create its own actor).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_start(sup: *mut HewSupervisor) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller guarantees sup is valid and start is a construction edge.
    unsafe { (*sup).running.store(1, Ordering::Release) };

    // Create the supervisor's own actor. We pass a dummy state (the sup
    // pointer itself) and override it after spawn.
    // SAFETY: spawning with the supervisor dispatch function.
    // The supervisor actor has NO application handlers: its entire protocol is
    // lifecycle signals, so it registers only the SYSTEM entry point. With
    // `dispatch` left `None`, any message a program sends to the supervisor's
    // actor handle is freed unread instead of reaching supervision logic.
    let self_actor = unsafe {
        actor::hew_actor_spawn(
            sup.cast::<HewSupervisor>().cast::<c_void>(),
            std::mem::size_of::<HewSupervisor>(),
            None,
        )
    };
    if self_actor.is_null() {
        // SAFETY: caller keeps `sup` live through start failure rollback.
        unsafe { (*sup).running.store(0, Ordering::Release) };
        return -1;
    }
    // SAFETY: `self_actor` is the freshly spawned supervisor actor; no other
    // thread can observe it before this call returns because the supervisor's
    // `self_actor` slot is still null.
    unsafe {
        actor::hew_actor_set_sys_dispatch(self_actor, Some(supervisor_sys_dispatch));
    }

    // Override the actor's state to point to our supervisor struct directly
    // (not a deep copy — we need the supervisor to receive updates).
    // SAFETY: self_actor is valid; free the deep copy.
    unsafe {
        if !(*self_actor).state.is_null() {
            crate::mem::buf_free((*self_actor).state); // ALLOCATOR-PAIRING: GlobalAlloc
        }
        (*self_actor).state = sup.cast::<c_void>();
        (*self_actor).state_size = 0; // mark as non-owned
    }

    // SAFETY: construction has exclusive authority over the self-actor slot.
    unsafe { (*sup).self_actor = self_actor };

    // Auto-register top-level supervisors for graceful shutdown so they
    // are cleaned up even if the generated code omits an explicit stop.
    // SAFETY: construction has exclusive authority over the parent edge.
    if unsafe { (*sup).parent.is_null() } {
        // SAFETY: sup is valid and will remain valid until shutdown.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    }

    0
}

/// Notify the supervisor that a supervised child ACTOR has stopped or crashed.
///
/// `child_index` is `u32`: the escalation case that formerly rode this same
/// symbol with `child_index = -1` is now
/// [`hew_supervisor_notify_child_supervisor_escalation`], so no caller —
/// external, JIT, or generated — can express the retagging value.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - The supervisor must have been started with [`hew_supervisor_start`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_notify_child_actor_event(
    sup: *mut HewSupervisor,
    child_index: u32,
    child_id: u64,
    exit_state: c_int,
    crash_code: c_int,
    fault_record: u64,
) -> bool {
    cabi_guard!(sup.is_null(), false);
    // SAFETY: caller keeps `sup` live through this notification.
    let self_actor = unsafe { (*sup).self_actor };
    if self_actor.is_null() {
        return false;
    }

    let event = ChildEvent {
        child_index,
        child_id,
        exit_state,
        crash_code,
        fault_record,
    };

    let kind = if exit_state == HewActorState::Crashed as c_int {
        HewSysMsg::ChildCrashed
    } else {
        HewSysMsg::ChildStopped
    };

    // SAFETY: self_actor is valid, mailbox is valid.
    unsafe {
        actor::send_system_message(
            self_actor,
            kind,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildEvent>(),
        )
    }
}

/// Notify the supervisor that a child SUPERVISOR exhausted its restart budget
/// and escalated.
///
/// The sibling of [`hew_supervisor_notify_child_actor_event`], split out so the
/// two events cannot be confused: `supervisor_index` indexes
/// `child_supervisors`, a different collection from the one an actor event
/// indexes. The old single symbol distinguished them by `child_index == -1`,
/// which silently retagged the meaning of the neighbouring id field.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - The supervisor must have been started with [`hew_supervisor_start`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_notify_child_supervisor_escalation(
    sup: *mut HewSupervisor,
    supervisor_index: u32,
    exit_state: c_int,
    crash_code: c_int,
    fault_record: u64,
) {
    cabi_guard!(sup.is_null());
    // SAFETY: caller keeps `sup` live through this notification.
    let self_actor = unsafe { (*sup).self_actor };
    if self_actor.is_null() {
        return;
    }

    // SAFETY: the caller retains the supervisor while its current slot is copied.
    let child_token = unsafe { &(*sup).roster }
        .lock_or_recover()
        .child_supervisor_tokens
        .get(supervisor_index as usize)
        .copied()
        .unwrap_or(crate::lifetime::local_handles::HewLocalPidId::INVALID);
    let event = ChildSupervisorEscalation {
        supervisor_index,
        child_token,
        exit_state,
        crash_code,
        fault_record,
    };

    // SAFETY: self_actor is valid, mailbox is valid.
    unsafe {
        let _ = actor::send_system_message(
            self_actor,
            HewSysMsg::ChildSupervisorEscalated,
            (&raw const event).cast::<c_void>().cast_mut(),
            std::mem::size_of::<ChildSupervisorEscalation>(),
        );
    }
}

/// Stop the supervisor and all its children.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`]. The
/// pointer must not be used after this call.
unsafe fn stop_claimed_supervisor(
    sup: *mut HewSupervisor,
    root_unregistered: bool,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
) -> bool {
    // SAFETY: forward the caller's unique teardown ownership.
    unsafe { finish_claimed_supervisor(sup, root_unregistered, teardown, false) }
}

pub(crate) unsafe fn finish_claimed_supervisor(
    sup: *mut HewSupervisor,
    root_unregistered: bool,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
    defer: bool,
) -> bool {
    if defer || current_thread_owns_supervisor_tree(sup) {
        if !spawn_owned_deferred_supervisor_stop(sup, teardown.clone()) {
            if root_unregistered {
                // SAFETY: the failed handoff leaves the top-level allocation
                // live and teardown ownership is released below.
                unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
            }
            release_supervisor_teardown(sup);
            set_last_error("hew_supervisor_stop: failed to spawn deferred stop thread");
            drop(teardown);
            return false;
        }
        if !root_unregistered {
            // Unregister once the deferred owner is guaranteed to finish.
            // SAFETY: `sup` is live and was registered when started.
            unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
        }
        drop(teardown);
        return true;
    }

    if !root_unregistered {
        // Unregister before consuming the raw pointer so shutdown cannot race
        // this exact-once teardown owner.
        // SAFETY: `sup` is live and was registered when started.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
    }
    run_supervisor_teardown_hook_for_test();
    // SAFETY: teardown ownership is uniquely claimed by the caller.
    unsafe { stop_supervisor_owned(sup, &teardown) };
    drop(teardown);
    true
}

pub(crate) unsafe fn stop_supervisor_with_teardown_authority(
    sup: *mut HewSupervisor,
    teardown: crate::lifetime::local_handles::SupervisorTeardownLease,
    preclaimed: bool,
) {
    request_supervisor_shutdown(sup);
    // SAFETY: the caller's teardown lease keeps runtime cleanup from reclaiming
    // the live allocation or its runtime authority during access closure.
    if unsafe { close_supervisor_access(sup, SUPERVISOR_PIN_DRAIN_TIMEOUT) }.is_none() {
        // A recursively detached child no longer has a parent-owned root. Hand
        // every still-live top-level allocation back to canonical cleanup before
        // this lease can release the cleanup barrier.
        // SAFETY: access closure failed closed, so `sup` remains allocated.
        if unsafe { (*sup).parent.is_null() } {
            // SAFETY: the still-live allocation has no parent root and remains
            // valid until canonical cleanup consumes the restored root.
            unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        }
        set_last_error("hew_supervisor_stop: handle pins did not drain");
        return;
    }
    if !preclaimed && !claim_supervisor_teardown(sup) {
        // Another owner holds teardown. One that handed the allocation back
        // to canonical cleanup keeps its claim, and the caller may have taken
        // it from the root set to get here: restore the root, or nothing
        // reclaims it and the final actor sweep frees the self actor's
        // borrowed state as its own.
        // SAFETY: the caller guarantees a live allocation.
        if unsafe { (*sup).parent.is_null() } {
            // SAFETY: the claimed allocation stays live until canonical
            // cleanup consumes the restored root.
            unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        }
        return;
    }
    // SAFETY: teardown ownership was claimed above and remains unique.
    unsafe { stop_claimed_supervisor(sup, false, teardown) };
}

/// Stop the supervisor and all its children.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`]. The
/// pointer must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_stop(sup: *mut HewSupervisor) {
    cabi_guard!(sup.is_null());

    // SAFETY: the public raw-pointer contract guarantees a live supervisor.
    let Some(teardown) = (unsafe { begin_supervisor_teardown(sup) }) else {
        return;
    };

    // SAFETY: the public raw-pointer contract guarantees a live supervisor;
    // the acquired lease remains visible to cleanup through final reclamation.
    unsafe { stop_supervisor_with_teardown_authority(sup, teardown, false) };
}

/// Return the stable direct identity for one supervisor allocation.
///
/// # Safety
///
/// `sup` must be a live pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_direct_id(
    sup: *mut HewSupervisor,
) -> crate::lifetime::local_handles::HewLocalPidId {
    if sup.is_null() {
        return crate::lifetime::local_handles::HewLocalPidId::INVALID;
    }
    // SAFETY: guaranteed by the caller.
    unsafe { (*sup).local_pid_id }
}

/// Query a supervisor through its stable local identity.
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_is_running(
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> c_int {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return 0;
    };
    run_supervisor_access_hook_for_test();
    let sup = pin.supervisor();
    // SAFETY: the pin prevents reclamation through these atomic loads.
    c_int::from(unsafe {
        (*sup).running.load(Ordering::Acquire) != 0 && !(*sup).cancelled.load(Ordering::Acquire)
    })
}

/// Stop a supervisor through its stable local identity.
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_stop(
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> c_int {
    stop_local_supervisor(token, false)
}

pub(crate) fn stop_local_supervisor(
    token: crate::lifetime::local_handles::HewLocalPidId,
    defer: bool,
) -> c_int {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return 1;
    };
    let Some(teardown) = crate::lifetime::local_handles::begin_current_supervisor_teardown() else {
        drop(pin);
        return 1;
    };
    let sup = pin.supervisor();
    // Publish shutdown and close the direct route while the allocation is
    // still protected by this operation's pin. Dropping the pin then permits
    // the raw destructor path to drain without self-deadlock.
    request_supervisor_shutdown(sup);
    let control = pin.control();
    let won_close = crate::lifetime::local_handles::close_current_supervisor(&control);
    if won_close {
        run_supervisor_close_hook_for_test();
    }
    if !claim_supervisor_teardown(sup) {
        drop(pin);
        return 1;
    }
    // SAFETY: the operation pin keeps the allocation live for this field read.
    let top_level = unsafe { (*sup).parent.is_null() };
    // Remove the root while the pin still protects this allocation. Runtime
    // cleanup can no longer select it for a competing canonical destructor.
    if top_level {
        // SAFETY: the pin proves that `sup` remains live through unregister.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };
    }
    drop(pin);
    if !defer && !control.wait_for_pins(SUPERVISOR_PIN_DRAIN_TIMEOUT) {
        // Restore canonical cleanup ownership for a top-level allocation whose
        // operation could not safely reach reclamation.
        if top_level {
            // SAFETY: timeout is fail-closed, so `sup` remains allocated.
            unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        }
        release_supervisor_teardown(sup);
        set_last_error("supervisor token stop: handle pins did not drain");
        return 2;
    }
    // Reclamation removes the registry's final control reference. Do not keep
    // the token-stop owner's local reference alive across the teardown lease:
    // runtime cleanup may proceed as soon as that lease is relinquished and
    // must observe the control registry fully drained.
    drop(control);
    // SAFETY: this token operation claimed teardown while pinned and already
    // removed the supervisor from the runtime cleanup root set.
    c_int::from(!unsafe { finish_claimed_supervisor(sup, top_level, teardown, defer) }) * 2
}

#[cfg(all(test, not(target_arch = "wasm32")))]
#[allow(
    unused_unsafe,
    reason = "test-owned raw supervisors often group several unsafe operations"
)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    /// Test-only shorthand that still obtains the typed roster guard. Tests
    /// own each supervisor for the complete guard lifetime.
    macro_rules! locked_roster {
        ($sup:expr) => {{
            // SAFETY: each use is scoped to a test-owned live supervisor.
            unsafe { &(*$sup).roster }.lock_or_recover()
        }};
    }

    unsafe fn teardown_is_claimed(sup: *mut HewSupervisor) -> bool {
        // SAFETY: callers pass a test-owned live supervisor.
        let token = unsafe { (*sup).local_pid_id };
        crate::lifetime::local_handles::current_supervisor_control_for_raw(token, sup)
            .is_some_and(|control| control.teardown_is_claimed())
    }

    struct OwnedDeferredSupervisorSpawnFailureGuard;

    impl Drop for OwnedDeferredSupervisorSpawnFailureGuard {
        fn drop(&mut self) {
            FAIL_OWNED_DEFERRED_SUPERVISOR_SPAWN.with(|slot| slot.set(false));
        }
    }

    fn fail_owned_deferred_supervisor_spawn() -> OwnedDeferredSupervisorSpawnFailureGuard {
        FAIL_OWNED_DEFERRED_SUPERVISOR_SPAWN.with(|slot| slot.set(true));
        OwnedDeferredSupervisorSpawnFailureGuard
    }

    fn wait_for_condition(
        timeout: std::time::Duration,
        mut condition: impl FnMut() -> bool,
    ) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        while std::time::Instant::now() < deadline {
            if condition() {
                return true;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        condition()
    }

    fn defer_state_transition(
        actor: *mut HewActor,
        target_state: HewActorState,
        delay: std::time::Duration,
    ) -> std::thread::JoinHandle<()> {
        let actor_addr = actor as usize;
        std::thread::spawn(move || {
            std::thread::sleep(delay);
            // SAFETY: the test keeps the actor allocation alive until this
            // state transition runs.
            unsafe {
                (*(actor_addr as *mut HewActor))
                    .actor_state
                    .store(target_state as i32, Ordering::Release);
            }
        })
    }

    unsafe extern "C-unwind" fn noop_child_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    static BORROWED_NORMAL_DISPATCH_COUNT: AtomicUsize = AtomicUsize::new(0);
    static BORROWED_NORMAL_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C-unwind" fn counted_borrowed_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        BORROWED_NORMAL_DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
        ptr::null_mut()
    }

    unsafe extern "C" fn count_borrowed_normal_drop(_state: *mut c_void) {
        BORROWED_NORMAL_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    unsafe fn make_supervisor_with_child() -> (*mut HewSupervisor, *mut HewActor, *mut HewActor) {
        // SAFETY: this helper creates a fresh supervisor tree for the test and
        // returns the owned raw pointers without publishing them elsewhere.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            assert_eq!(hew_supervisor_start(sup), 0);

            let child = locked_roster!(sup).children[0];
            let self_actor = (*sup).self_actor;
            (sup, child, self_actor)
        }
    }

    #[test]
    fn supervisor_teardown_quiescence_waits_expire_fail_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh tree and restores terminal states
        // before consuming it through the normal public stop path.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();

            // A worker can die with any one of these ownership edges still
            // active.  Each wait must return at its shared deadline rather than
            // spin forever; the caller then returns the tree to cleanup without
            // freeing a live actor or timer-borrowed supervisor.
            (*self_actor)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);
            assert!(!wait_for_supervisor_self_actor_quiescent(
                sup,
                Instant::now()
            ));

            let timer = (*sup)
                .restart_timers
                .begin(FaultRecord::NONE)
                .expect("fresh supervisor accepts a timer lease");
            assert!(!wait_for_pending_restart_timers(
                &(*sup).restart_timers,
                Instant::now()
            ));

            (*child)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);
            assert!(!wait_for_child_quiescent(child, Instant::now()));

            drop(timer);
            (*self_actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            (*child)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn runtime_cleanup_cancels_long_restart_timer_without_retry() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the production timer lease owns the raw supervisor borrow;
        // canonical cleanup cancels and drains it before reclaiming the tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let timers = Arc::clone(&(*sup).restart_timers);
            schedule_delayed_restart(sup, 0, Duration::from_secs(30), FaultRecord::NONE);
            assert_eq!(
                timers.pending_for_test(),
                1,
                "long-backoff timer must publish its raw borrow before spawn"
            );

            let started = Instant::now();
            crate::scheduler::hew_runtime_cleanup();

            assert!(
                started.elapsed() < Duration::from_secs(2),
                "cleanup must wake a 30-second restart backoff instead of leasing its full delay"
            );
            assert_eq!(timers.pending_for_test(), 0);
            assert!(
                crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "one cleanup call must reclaim the runtime after cancellable timers drain"
            );
        }
    }

    /// SHUTDOWN CANCELS AN ARMED TIMER. The timer thread breaks out of its wait
    /// without ever running the restart, so the record the arming TRANSFERRED to
    /// it must be settled at the cancel site.
    ///
    /// Leaving it Open still fails closed on the exit code, but the transition
    /// is incomplete and it is not free: `hew_shutdown_wait` holds the runtime
    /// open for its whole quiescence period (`drain_is_idle` is gated on
    /// `has_open_supervised_faults`) waiting for a ruling that can no longer
    /// arrive, because the only authority that could have made it was cancelled.
    #[test]
    fn cancelling_an_armed_restart_timer_settles_its_record() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the supervisor outlives the timer lease, which drains before
        // this body returns.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let timers = Arc::clone(&(*sup).restart_timers);
            let record = crate::exit_status::open_supervised_fault();
            assert!(
                crate::exit_status::supervised_fault_is_open(record),
                "a freshly opened record awaits a ruling"
            );

            assert!(
                schedule_delayed_restart(sup, 0, Duration::from_secs(30), record),
                "the timer is admitted and spawned, so the record is armed"
            );
            assert_eq!(timers.pending_for_test(), 1);
            assert!(
                crate::exit_status::supervised_fault_is_open(record),
                "arming TRANSFERS the record to the timer; it does not settle it"
            );

            timers.cancel();
            assert!(
                timers.wait_for_drain(Instant::now() + Duration::from_secs(5)),
                "cancellation must wake the 30-second backoff"
            );

            assert!(
                !crate::exit_status::supervised_fault_is_open(record),
                "a cancelled timer attempts no restart, so it settles the record \
                 it owned instead of stranding it Open for the shutdown quiesce"
            );
        }
    }

    #[test]
    fn cancelled_restart_timer_settles_record_before_publishing_drain() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the supervisor outlives the timer lease, which drains before
        // this body returns.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let timers = Arc::clone(&(*sup).restart_timers);
            let record = crate::exit_status::open_supervised_fault();
            let released = Arc::new(std::sync::Barrier::new(2));
            let continue_timer = Arc::new(std::sync::Barrier::new(2));
            let released_hook = Arc::clone(&released);
            let continue_timer_hook = Arc::clone(&continue_timer);
            let _hook_guard = install_restart_timer_released_hook_for_test(Arc::new(move || {
                released_hook.wait();
                continue_timer_hook.wait();
            }));

            assert!(schedule_delayed_restart(
                sup,
                0,
                Duration::from_secs(30),
                record
            ));
            timers.cancel();
            released.wait();
            assert!(timers.wait_for_drain(Instant::now() + Duration::from_secs(5)));

            let settled_before_drain = !crate::exit_status::supervised_fault_is_open(record);
            continue_timer.wait();
            let settle_deadline = Instant::now() + Duration::from_secs(5);
            while crate::exit_status::supervised_fault_is_open(record)
                && Instant::now() < settle_deadline
            {
                std::thread::yield_now();
            }
            assert!(
                settled_before_drain,
                "a timer must settle its record before releasing the drain lease"
            );
        }
    }

    #[test]
    fn public_stop_cancels_long_restart_timer_promptly() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: public stop owns and reclaims the fresh supervisor only after
        // the production timer lease has observed cancellation and drained.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let timers = Arc::clone(&(*sup).restart_timers);
            schedule_delayed_restart(sup, 0, Duration::from_secs(30), FaultRecord::NONE);
            assert_eq!(timers.pending_for_test(), 1);

            let started = Instant::now();
            hew_supervisor_stop(sup);

            assert!(
                started.elapsed() < Duration::from_secs(2),
                "public stop must interrupt a long restart backoff"
            );
            assert_eq!(timers.pending_for_test(), 0);
        }
    }

    #[test]
    fn runtime_cleanup_cancels_nested_supervisor_timer_in_one_pass() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test builds one parent-owned nested tree, then canonical
        // cleanup recursively cancels every timer before reclaiming either Box.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let timers = Arc::clone(&(*nested).restart_timers);
            schedule_delayed_restart(nested, 0, Duration::from_secs(30), FaultRecord::NONE);
            assert_eq!(timers.pending_for_test(), 1);

            let started = Instant::now();
            crate::scheduler::hew_runtime_cleanup();

            assert!(
                started.elapsed() < Duration::from_secs(2),
                "recursive cleanup must wake a nested supervisor's long timer"
            );
            assert_eq!(timers.pending_for_test(), 0);
            assert!(
                crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "nested timer cancellation must finish in the initial cleanup pass"
            );
        }
    }

    #[test]
    fn supervisor_stop_detaches_timed_out_child_before_cleanup_handoff() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns both fresh supervisors. A deliberately retained
        // production timer lease forces the nested stop through its bounded
        // fail-closed handoff; releasing it then lets canonical cleanup reclaim
        // both independent roots.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let timer = (*nested)
                .restart_timers
                .begin(FaultRecord::NONE)
                .expect("fresh nested supervisor accepts a timer lease");

            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert!(
                (*nested).parent.is_null(),
                "child must detach before a bounded stop can return it to cleanup"
            );
            assert!(
                crate::shutdown::is_supervisor_registered_for_test(nested),
                "timed-out detached child must become an independent cleanup root"
            );
            assert!(
                locked_roster!(parent).child_supervisors.is_empty(),
                "parent must not retain a second ownership edge"
            );

            drop(timer);
            crate::scheduler::hew_runtime_cleanup();
            assert!(crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null());
        }
    }

    #[test]
    fn supervisor_stop_retains_child_when_teardown_admission_is_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns both supervisors and closes admission only in
        // its isolated runtime immediately before canonical cleanup.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);

            crate::runtime::rt_current()
                .local_handles
                .close_supervisor_teardown_admission();
            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert_eq!((*nested).parent, parent);
            assert_eq!(locked_roster!(parent).child_supervisors, vec![nested]);
            assert!(
                !crate::shutdown::is_supervisor_registered_for_test(nested),
                "without teardown admission the original parent edge remains the sole root"
            );
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    #[test]
    fn nested_parent_claim_wins_before_token_stop_without_root_imbalance() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the parent dispatch owns roster extraction; the barrier holds
        // the claimed child live at the canonical teardown edge.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let token = (*nested).local_pid_id;

            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook_guard = install_supervisor_teardown_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));

            let parent_addr = parent as usize;
            let stop = std::thread::spawn(move || {
                supervisor_sys_dispatch_impl(
                    ptr::null_mut(),
                    (parent_addr as *mut HewSupervisor).cast::<c_void>(),
                    HewSysMsg::SupervisorStop as i32,
                    ptr::null_mut(),
                    0,
                );
            });
            entered.wait();

            assert_eq!(hew_local_pid_supervisor_stop(token), 1);
            assert!(locked_roster!(parent).child_supervisors.is_empty());
            assert!(!crate::shutdown::is_supervisor_registered_for_test(nested));
            assert_eq!(
                crate::shutdown::registered_supervisor_count_for_test(),
                1,
                "parent remains the only cleanup root after its child claim wins"
            );

            release.wait();
            stop.join().expect("parent-owned nested stop");
            drop(hook_guard);
            hew_supervisor_stop(parent);
            assert_eq!(crate::shutdown::registered_supervisor_count_for_test(), 0);
        }
    }

    #[test]
    fn nested_token_stop_wins_before_parent_claim_without_dangling_root() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the token pin is held across the barrier, proving the parent
        // can lose without dereferencing or publishing the nested pointer.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, _nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let token = (*nested).local_pid_id;

            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook_guard = install_supervisor_close_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));
            let token_stop = std::thread::spawn(move || hew_local_pid_supervisor_stop(token));
            entered.wait();

            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert!(locked_roster!(parent).child_supervisors.is_empty());
            assert!(!crate::shutdown::is_supervisor_registered_for_test(nested));
            assert_eq!(
                crate::shutdown::registered_supervisor_count_for_test(),
                1,
                "token-owned nested stop must not publish a second root while parent remains"
            );
            release.wait();
            assert_eq!(token_stop.join().expect("token-owned nested stop"), 0);
            drop(hook_guard);
            hew_supervisor_stop(parent);
            assert_eq!(crate::shutdown::registered_supervisor_count_for_test(), 0);
        }
    }

    #[test]
    fn supervisor_stop_detaches_child_before_deferred_spawn_failure() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test installs one nested child as current actor solely to
        // select the production deferred-stop branch.
        unsafe {
            let (parent, _parent_child, _parent_self) = make_supervisor_with_child();
            let (nested, nested_child, _nested_self) = make_supervisor_with_child();
            assert_eq!(hew_supervisor_add_child_supervisor(parent, nested), 0);
            let fail_guard = fail_owned_deferred_supervisor_spawn();
            let ctx = TestExecutionContext::install(HewExecutionContext {
                actor: nested_child,
                actor_id: (*nested_child).id,
                ..HewExecutionContext::default()
            });

            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                parent.cast::<c_void>(),
                HewSysMsg::SupervisorStop as i32,
                ptr::null_mut(),
                0,
            );

            assert!((*nested).parent.is_null());
            assert!(locked_roster!(parent).child_supervisors.is_empty());
            assert!(
                crate::shutdown::is_supervisor_registered_for_test(nested),
                "deferred spawn failure must leave the detached child rooted"
            );

            drop(ctx);
            drop(fail_guard);
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    #[test]
    fn runtime_cleanup_retains_tree_when_timer_thread_cannot_drain() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the fresh tree and deliberately holds a real
        // timer lease past the bounded cleanup wait to exercise the fail-closed
        // fallback, then releases it before retrying reclamation.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            crate::shutdown::hew_shutdown_register_supervisor(sup);
            let timer = (*sup)
                .restart_timers
                .begin(FaultRecord::NONE)
                .expect("fresh supervisor accepts a timer lease");

            crate::scheduler::hew_runtime_cleanup();

            assert!(
                crate::shutdown::is_supervisor_registered_for_test(sup),
                "canonical cleanup must retain a supervisor while a timer could dereference it"
            );
            assert!(
                actor::is_actor_live_with_id(child_id, child)
                    && actor::is_actor_live_with_id(self_id, self_actor),
                "incomplete cleanup must leave runtime-owned actors live with the retained tree"
            );
            assert!(
                !crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "incomplete cleanup must leave the runtime installed for retry"
            );

            drop(timer);
            crate::scheduler::hew_runtime_cleanup();
            assert!(
                crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
                "retry after timer drain must reclaim the retained tree and runtime"
            );
        }
    }

    #[test]
    fn supervisor_restart_completes_while_metrics_reset_runs_in_parallel() {
        let _rt = crate::runtime_test_guard();
        let _scheduler = RealSchedulerGuard::new();
        // SAFETY: this test owns the supervisor and joins the concurrent
        // metrics caller before stopping it.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            locked_roster!(sup).child_specs[0].restart_policy = RESTART_PERMANENT;

            let started = Arc::new(std::sync::Barrier::new(2));
            let reset_started = Arc::clone(&started);
            let resetter = std::thread::spawn(move || {
                reset_started.wait();
                for _ in 0..2_048 {
                    crate::scheduler::hew_sched_metrics_reset();
                }
            });

            started.wait();
            actor::hew_actor_trap(child, 1);
            assert!(
                test_wait_for_restart(sup, 1, 2_000) >= 1,
                "a supervisor restart must complete while live metrics reset runs"
            );
            resetter.join().expect("metrics resetter must not panic");
            hew_supervisor_stop(sup);
        }
    }

    /// Installs a worker-backed scheduler for tests that need real dispatch,
    /// and tears it down symmetrically. `runtime_test_guard` alone installs a
    /// worker-LESS placeholder, so nothing would ever run a handler.
    struct RealSchedulerGuard;

    impl RealSchedulerGuard {
        fn new() -> Self {
            crate::scheduler::init_real_scheduler_for_test();
            Self
        }
    }

    impl Drop for RealSchedulerGuard {
        fn drop(&mut self) {
            crate::scheduler::hew_sched_shutdown();
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    /// Byte-compatible replica of the internal supervision event payload, as
    /// an attacker holding a supervisor's actor handle would hand-build it.
    /// Declared independently of `ChildEvent` so the forgery test keeps its
    /// teeth even if the internal struct is retyped.
    #[repr(C)]
    struct ForgedChildEvent {
        child_index: c_int,
        child_id: u64,
        exit_state: c_int,
        crash_code: c_int,
    }

    /// A supervision event forged on the USER queue must never free a live
    /// child.
    ///
    /// `hew_actor_send` is public C ABI and routes to the USER queue. Before
    /// the sys/user channel split, `supervisor_dispatch_impl` matched on the
    /// raw `msg_type` VALUE with no provenance gate, so a forged `ChildEvent`
    /// delivered on the user queue drove `take_child_slot` +
    /// `hew_actor_free` on a live child — a use-after-free reachable with no
    /// hash collision. Supervision events now arrive only through the typed
    /// system dispatch entry point, which the user queue cannot reach.
    #[test]
    fn user_queue_supervision_value_does_not_free_a_live_child() {
        let _rt = crate::runtime_test_guard();
        let _sched = RealSchedulerGuard::new();
        // SAFETY: the test owns the supervisor tree for the whole body.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;
            assert!(
                actor::is_actor_live_with_id(child_id, child),
                "precondition: the child is live before the forged send"
            );

            // The forged payload an attacker holding the supervisor's actor
            // handle would build: index 0, the live child's id, Crashed.
            let forged = ForgedChildEvent {
                child_index: 0,
                child_id,
                exit_state: HewActorState::Crashed as c_int,
                crash_code: 0,
            };
            // Every value in the former reserved block, not just the
            // supervision one: none may reach the system handler.
            for forged_type in 100..=105_i32 {
                crate::actor::hew_actor_send(
                    self_actor,
                    forged_type,
                    (&raw const forged).cast::<c_void>().cast_mut(),
                    std::mem::size_of::<ForgedChildEvent>(),
                );
            }

            let freed = wait_for_condition(std::time::Duration::from_secs(2), || {
                !actor::is_actor_live_with_id(child_id, child)
            });
            assert!(
                !freed,
                "a user-queue send of a reserved system value freed a LIVE \
                 supervised child (use-after-free)"
            );
            assert_eq!(
                hew_supervisor_child_count(sup),
                1,
                "the forged user-queue send must not alter the child roster"
            );

            // NON-VACUITY: "the child survived" only means something if the
            // supervisor was actually running and WOULD have acted on a real
            // event. Deliver the same ChildCrashed by its legitimate route —
            // the privileged system send — and require that it does reclaim
            // the child. The forgery and the real thing carry identical bytes;
            // only the channel differs, which is the whole point.
            hew_supervisor_notify_child_actor_event(
                sup,
                0,
                child_id,
                HewActorState::Crashed as c_int,
                0,
                FaultRecord::NONE.as_raw(),
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "the supervision path must be live: an event delivered on the \
                 SYSTEM channel must reclaim the child, otherwise the forgery \
                 assertions above prove nothing"
            );

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn borrowed_legacy_actor_dispatches_normal_message_without_drop_authority() {
        let _rt = crate::runtime_test_guard();
        let _sched = RealSchedulerGuard::new();
        BORROWED_NORMAL_DISPATCH_COUNT.store(0, Ordering::SeqCst);
        BORROWED_NORMAL_DROP_COUNT.store(0, Ordering::SeqCst);

        // SAFETY: this test exclusively owns the supervisor through stop.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());
            let state = 9_u64;
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: std::ptr::from_ref(&state).cast_mut().cast(),
                init_state_size: std::mem::size_of::<u64>(),
                dispatch: Some(counted_borrowed_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            hew_supervisor_set_child_state_drop(sup, 0, count_borrowed_normal_drop);
            let child = locked_roster!(sup).children[0];
            assert!((*child).state_drop_borrowed.load(Ordering::Acquire));
            assert!((*child).state_drop_fn.is_some());
            assert!((*child).state_clone_fn.is_none());

            actor::hew_actor_send(child, 77, ptr::null_mut(), 0);
            assert!(
                wait_for_condition(Duration::from_secs(2), || {
                    BORROWED_NORMAL_DISPATCH_COUNT.load(Ordering::Acquire) == 1
                }),
                "legacy borrowed actor must complete an ordinary user dispatch"
            );
            assert!(
                !(*child).state_drop_consumed.load(Ordering::Acquire),
                "normal dispatch must not fabricate crash-escrow consumption"
            );

            hew_supervisor_stop(sup);
            assert_eq!(BORROWED_NORMAL_DROP_COUNT.load(Ordering::SeqCst), 0);
        }
    }

    #[test]
    fn reserved_two_child_roster_targets_setter_by_exact_index() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: construction and inspection are single-threaded; the test
        // owns the supervisor until stop.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            let states = [11_u64, 22_u64];
            for state in &states {
                let spec = HewChildSpec {
                    name: ptr::null(),
                    init_state: std::ptr::from_ref(state).cast_mut().cast(),
                    init_state_size: std::mem::size_of::<u64>(),
                    dispatch: Some(noop_child_dispatch),
                    sys_dispatch: None,
                    restart_policy: RESTART_TEMPORARY,
                    mailbox_capacity: -1,
                    overflow: OVERFLOW_DROP_NEW,
                    coalesce_key_fn: None,
                    coalesce_fallback: OVERFLOW_DROP_NEW,
                    message_drop_fn: None,
                    arena_cap_bytes: 0,
                    cycle_capable: 0,
                    on_crash: None,
                    lifecycle_fn: None,
                    init_fn: None,
                    config: ptr::null_mut(),
                    config_size: 0,
                };
                assert_eq!(
                    hew_supervisor_add_child_spec(sup, &raw const spec),
                    0,
                    "static add remains a status-only ABI"
                );
            }

            hew_supervisor_set_child_state_drop(sup, 1, count_borrowed_normal_drop);
            assert!(locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state_drop
                .load()
                .is_none());
            assert!(locked_roster!(sup).child_specs[1]
                .state_template
                .allocation
                .state_drop
                .load()
                .is_some());
            assert!(locked_roster!(sup).children[0]
                .as_ref()
                .unwrap()
                .state_drop_fn
                .is_none());
            assert!(locked_roster!(sup).children[1]
                .as_ref()
                .unwrap()
                .state_drop_fn
                .is_some());
            hew_supervisor_stop(sup);
        }
    }

    static TEARDOWN_RACE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn count_teardown_race_state_drop(_state: *mut c_void) {
        TEARDOWN_RACE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn init_counted_teardown_race_state(
        _config: *const c_void,
    ) -> HewChildInitResult {
        // SAFETY: the runtime owns and later libc-frees this wrapper.
        let state = crate::mem::buf_try_alloc(std::mem::size_of::<u64>()).cast::<u64>();
        if state.is_null() {
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        // SAFETY: state points to one freshly allocated u64.
        unsafe { *state = 17 };
        HewChildInitResult {
            state: state.cast::<c_void>(),
            size: std::mem::size_of::<u64>(),
        }
    }

    unsafe fn make_supervisor_with_counted_child() -> *mut HewSupervisor {
        // SAFETY: this helper owns the fresh supervisor; the init thunk gives
        // each child independently-owned state so the callback is an actor-
        // teardown counter, not a legacy shallow-template alias counter.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: Some(init_counted_teardown_race_state),
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            hew_supervisor_set_child_state_drop(sup, 0, count_teardown_race_state_drop);
            sup
        }
    }

    fn assert_cleanup_waits_for_synchronous_stop_owner(stop_by_token: bool) {
        let _rt = crate::runtime_test_guard();
        TEARDOWN_RACE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the helper returns one live test-owned supervisor.
        let sup = unsafe { make_supervisor_with_counted_child() };
        if stop_by_token {
            // Exercise recursive supervisor destruction after cleanup has closed
            // new teardown admission. The child must share the parent's lease.
            // SAFETY: both fresh supervisors are test-owned and unparented.
            let child = unsafe { make_supervisor_with_counted_child() };
            assert_eq!(
                // SAFETY: the pointers are live and distinct.
                unsafe { hew_supervisor_add_child_supervisor(sup, child) },
                0
            );
        }
        // SAFETY: this top-level root remains live until the stop owner or
        // canonical cleanup consumes it.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        // SAFETY: the supervisor remains live until one of the joined owners
        // reclaims it.
        let token = unsafe { hew_supervisor_direct_id(sup) };

        let teardown_entered = Arc::new(std::sync::Barrier::new(2));
        let teardown_release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = teardown_entered.clone();
        let release_hook = teardown_release.clone();
        let paused = Arc::new(AtomicBool::new(false));
        let paused_hook = paused.clone();
        let _hook = install_supervisor_teardown_hook_for_test(Arc::new(move || {
            if !paused_hook.swap(true, Ordering::AcqRel) {
                entered_hook.wait();
                release_hook.wait();
            }
        }));

        let sup_addr = sup as usize;
        let stop = std::thread::spawn(move || {
            if stop_by_token {
                hew_local_pid_supervisor_stop(token)
            } else {
                // SAFETY: the test keeps the registered allocation live until
                // this raw stop has acquired its teardown lease.
                unsafe { hew_supervisor_stop(sup_addr as *mut HewSupervisor) };
                0
            }
        });
        teardown_entered.wait();

        assert!(!crate::shutdown::is_supervisor_registered_for_test(sup));
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            if stop_by_token { (1, 2) } else { (0, 1) }
        );
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_teardown_state_for_test(),
            (true, 1)
        );

        let drain_entered = Arc::new(std::sync::Barrier::new(2));
        let drain_release = Arc::new(std::sync::Barrier::new(2));
        crate::lifetime::local_handles::install_current_supervisor_teardown_drain_hook_for_test(
            drain_entered.clone(),
            drain_release.clone(),
        );
        let (cleanup_done_tx, cleanup_done_rx) = std::sync::mpsc::channel();
        let cleanup = std::thread::spawn(move || {
            crate::scheduler::hew_runtime_cleanup();
            cleanup_done_tx
                .send(())
                .expect("cleanup completion receiver");
        });
        drain_entered.wait();
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_teardown_state_for_test(),
            (false, 1)
        );
        assert!(matches!(
            cleanup_done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));
        assert_eq!(TEARDOWN_RACE_DROP_COUNT.load(Ordering::SeqCst), 0);

        teardown_release.wait();
        assert_eq!(stop.join().expect("stop thread"), 0);
        drain_release.wait();
        cleanup.join().expect("cleanup thread");
        cleanup_done_rx.recv().expect("cleanup completion");
        assert_eq!(
            TEARDOWN_RACE_DROP_COUNT.load(Ordering::SeqCst),
            if stop_by_token { 2 } else { 1 }
        );
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn runtime_cleanup_waits_for_synchronous_token_stop_owner() {
        assert_cleanup_waits_for_synchronous_stop_owner(true);
    }

    #[test]
    fn runtime_cleanup_waits_for_synchronous_raw_stop_owner() {
        assert_cleanup_waits_for_synchronous_stop_owner(false);
    }

    #[test]
    fn direct_supervisor_pin_blocks_stop_reclamation_until_use_finishes() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: fresh allocation remains live until the stop thread completes.
        unsafe { (*sup).running.store(1, Ordering::Release) };
        // SAFETY: sup is a live supervisor created above.
        let token = unsafe { hew_supervisor_direct_id(sup) };

        let entered = Arc::new(std::sync::Barrier::new(2));
        let release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = entered.clone();
        let release_hook = release.clone();
        let _hook = install_supervisor_access_hook_for_test(Arc::new(move || {
            entered_hook.wait();
            release_hook.wait();
        }));
        let resolver = std::thread::spawn(move || hew_local_pid_supervisor_is_running(token));
        entered.wait();

        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let stop = std::thread::spawn(move || {
            done_tx
                .send(hew_local_pid_supervisor_stop(token))
                .expect("stop completion receiver");
        });
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
        while crate::lifetime::local_handles::current_supervisor_counts_for_test().0 != 0 {
            assert!(
                std::time::Instant::now() < deadline,
                "stop did not retire route"
            );
            std::thread::yield_now();
        }
        assert!(matches!(
            done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));

        release.wait();
        assert_eq!(resolver.join().expect("resolver thread"), 0);
        stop.join().expect("stop thread");
        assert_eq!(done_rx.recv().expect("stop result"), 0);
        assert_eq!(hew_local_pid_supervisor_stop(token), 1);
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn supervisor_close_wins_before_late_resolver_pin() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: sup is live.
        let token = unsafe { hew_supervisor_direct_id(sup) };

        let close_entered = Arc::new(std::sync::Barrier::new(2));
        let close_release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = close_entered.clone();
        let release_hook = close_release.clone();
        let _close_hook = install_supervisor_close_hook_for_test(Arc::new(move || {
            entered_hook.wait();
            release_hook.wait();
        }));
        let access_ran = Arc::new(AtomicBool::new(false));
        let access_ran_hook = access_ran.clone();
        let _access_hook = install_supervisor_access_hook_for_test(Arc::new(move || {
            access_ran_hook.store(true, Ordering::Release);
        }));

        let stop = std::thread::spawn(move || hew_local_pid_supervisor_stop(token));
        close_entered.wait();
        assert_eq!(hew_local_pid_supervisor_is_running(token), 0);
        assert!(!access_ran.load(Ordering::Acquire));
        close_release.wait();
        assert_eq!(stop.join().expect("stop thread"), 0);
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn runtime_cleanup_waits_for_supervisor_pin_then_empties_controls() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: sup is live and registered as a cleanup root below.
        let token = unsafe { hew_supervisor_direct_id(sup) };
        // SAFETY: this fresh supervisor remains live until cleanup consumes it.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };

        let entered = Arc::new(std::sync::Barrier::new(2));
        let release = Arc::new(std::sync::Barrier::new(2));
        let entered_hook = entered.clone();
        let release_hook = release.clone();
        let _hook = install_supervisor_access_hook_for_test(Arc::new(move || {
            entered_hook.wait();
            release_hook.wait();
        }));
        let resolver = std::thread::spawn(move || hew_local_pid_supervisor_is_running(token));
        entered.wait();

        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let cleanup = std::thread::spawn(move || {
            crate::scheduler::hew_runtime_cleanup();
            done_tx.send(()).expect("cleanup completion receiver");
        });
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
        while crate::lifetime::local_handles::current_supervisor_counts_for_test().0 != 0 {
            assert!(
                std::time::Instant::now() < deadline,
                "cleanup did not retire supervisor routes"
            );
            std::thread::yield_now();
        }
        assert!(matches!(
            done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));

        release.wait();
        assert_eq!(resolver.join().expect("resolver thread"), 0);
        cleanup.join().expect("cleanup thread");
        done_rx.recv().expect("cleanup completion");
    }

    #[test]
    fn supervisor_pin_timeout_leaks_fail_closed_until_pin_drops() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // SAFETY: sup is live.
        let token = unsafe { hew_supervisor_direct_id(sup) };
        let pin = crate::lifetime::local_handles::pin_current_supervisor(token)
            .expect("live supervisor pin");

        // SAFETY: sup remains allocated and test-owned. A zero timeout forces
        // the fail-closed branch while `pin` remains held.
        assert!(!unsafe { close_supervisor_access_with_timeout(sup, std::time::Duration::ZERO) });
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 1)
        );
        assert_eq!(hew_local_pid_supervisor_is_running(token), 0);

        drop(pin);
        // SAFETY: the failed close leaked the still-allocated supervisor; this
        // retry drains the now-empty pin set and owns destruction.
        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(
            crate::lifetime::local_handles::current_supervisor_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn owned_stop_returns_root_to_cleanup_when_access_close_fails() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh top-level supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // Mirror the deferred/token handoff: canonical cleanup ownership is
        // removed only after teardown admission and the exact owner claim.
        // SAFETY: `sup` remains live through this fail-closed handoff.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        // SAFETY: `sup` is a live supervisor in the current runtime.
        let teardown = unsafe { begin_supervisor_teardown(sup) }.expect("teardown lease");
        assert!(claim_supervisor_teardown(sup));
        // SAFETY: the teardown owner now holds the allocation exclusively.
        unsafe { crate::shutdown::hew_shutdown_unregister_supervisor(sup) };

        FAIL_NEXT_SUPERVISOR_ACCESS_CLOSE.with(|slot| slot.set(true));
        // SAFETY: the exact teardown owner retains the live allocation.
        unsafe { stop_supervisor_owned(sup, &teardown) };

        assert!(
            crate::shutdown::is_supervisor_registered_for_test(sup),
            "failed access close must return the top-level allocation to cleanup"
        );
        // The handoff remains claimed until canonical post-worker cleanup so a
        // racing worker cannot become a second destructor.
        // SAFETY: the restored root keeps `sup` live until cleanup below.
        assert!(unsafe { teardown_is_claimed(sup) });
        drop(teardown);
        crate::scheduler::hew_runtime_cleanup();
    }

    #[test]
    fn refused_stop_keeps_a_handed_back_root_for_cleanup() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the fresh top-level supervisor.
        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
        assert!(!sup.is_null());
        // A teardown owner that ran out of time hands the allocation back to
        // canonical cleanup as a root and keeps its claim.
        assert!(claim_supervisor_teardown(sup));
        // SAFETY: the claimed allocation stays live through this handback.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        // Shutdown takes each root before stopping it; the claim refuses.
        // SAFETY: as above.
        unsafe {
            crate::shutdown::hew_shutdown_unregister_supervisor(sup);
            hew_supervisor_stop(sup);
        }
        assert!(
            crate::shutdown::is_supervisor_registered_for_test(sup),
            "a refused stop must leave the handed-back root to canonical cleanup"
        );
        // Cleanup asserts that no supervisor control survives it.
        crate::scheduler::hew_runtime_cleanup();
        assert!(
            crate::runtime::default_runtime_ptr(Ordering::Acquire).is_null(),
            "cleanup must reclaim the handed-back supervisor and the runtime"
        );
    }

    #[test]
    fn stop_supervisor_from_child_dispatch_is_deferred() {
        // Install a runtime so the live-actor registry resolves; held for the
        // whole test (it serializes actor-freeing tests on the shared lock).
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree and only mutates the
        // current actor context within the test thread.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            // Probe liveness by (id, ptr): sibling tests in this process spawn
            // actors concurrently, and a recycled allocation address would make
            // a pointer-only probe report the freed actor as live again (ABA).
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            (*child)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: child_id,
                ..HewExecutionContext::default()
            });
            let unblock = defer_state_transition(
                child,
                HewActorState::Stopped,
                std::time::Duration::from_millis(200),
            );

            let start = std::time::Instant::now();
            hew_supervisor_stop(sup);
            let elapsed = start.elapsed();

            unblock.join().unwrap();

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "child actor should be freed asynchronously after deferred supervisor stop"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(self_id, self_actor)
                }),
                "supervisor self actor should be freed asynchronously after deferred stop"
            );

            assert!(
                elapsed < std::time::Duration::from_millis(100),
                "child-owned supervisor stop should return immediately instead of waiting for the current dispatch thread, took {elapsed:?}"
            );
        }
    }

    #[test]
    fn stop_supervisor_from_child_terminate_is_deferred() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree and simulates a reentrant
        // terminate callback by controlling the child actor state directly.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            // Probe liveness by (id, ptr) — see
            // stop_supervisor_from_child_dispatch_is_deferred for the ABA
            // rationale.
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: child_id,
                ..HewExecutionContext::default()
            });
            let start = std::time::Instant::now();
            hew_supervisor_stop(sup);
            let elapsed = start.elapsed();

            child_ref.terminate_finished.store(true, Ordering::Release);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "child should be released after deferred supervisor stop"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(self_id, self_actor)
                }),
                "supervisor self actor should be released after deferred stop"
            );

            assert!(
                elapsed < std::time::Duration::from_secs(1),
                "reentrant supervisor stop should defer instead of spinning inside terminate, took {elapsed:?}"
            );
        }
    }

    /// Serializes the deferred-teardown join-barrier tests. They share the
    /// process-global `DEFERRED_TEARDOWN_THREADS` registry and each holds its
    /// teardown open with a gated terminate; running two of them concurrently
    /// would let one test's `drain_deferred_teardown_threads` steal and join
    /// the other's still-gated handle, so the victim's own drain observes an
    /// empty registry and asserts before the stolen teardown frees its actor.
    /// Production only ever drains once, single-threaded, in
    /// `cleanup_all_actors`; this lock restores that precondition for the tests.
    static TEARDOWN_DRAIN_SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn deferred_stop_returns_root_to_cleanup_after_scheduler_shutdown() {
        let _rt = crate::runtime_test_guard();
        let _serial = TEARDOWN_DRAIN_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // SAFETY: this test owns the supervisor tree and installs the child as
        // the current actor only long enough to select the deferred stop path.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            crate::shutdown::hew_shutdown_register_supervisor(sup);
            (*child)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: (*child).id,
                ..HewExecutionContext::default()
            });
            hew_supervisor_stop(sup);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    crate::lifetime::live_actors::deferred_teardown_thread_count() == 1
                }),
                "deferred supervisor owner must be registered before shutdown"
            );

            crate::scheduler::hew_sched_shutdown();
            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            assert!(
                crate::shutdown::is_supervisor_registered_for_test(sup),
                "shutdown-aware deferred stop must restore the top-level root"
            );
            assert!(
                teardown_is_claimed(sup),
                "handoff must keep teardown claimed until canonical cleanup"
            );

            crate::scheduler::hew_runtime_cleanup();
        }
    }

    #[test]
    fn drain_deferred_teardown_joins_in_flight_supervisor_stop() {
        let _rt = crate::runtime_test_guard();
        let _serial = TEARDOWN_DRAIN_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // The deferred-sup-stop thread dereferences the supervisor's child and
        // self actors for the whole teardown. `cleanup_all_actors` joins the
        // registered teardown threads before sweeping `LIVE_ACTORS`; this test
        // exercises that join barrier directly. The teardown is held open
        // across the drain call by an unfinished child terminate, so a drain
        // that does not join the deferred-sup-stop thread returns while the
        // supervisor self actor is still tracked.
        // SAFETY: this test owns the supervisor tree and gates the teardown
        // through test-controlled atomics, mirroring the reentrant-terminate
        // test above.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: child_id,
                ..HewExecutionContext::default()
            });
            // Deferred path: the current thread owns the supervisor tree.
            hew_supervisor_stop(sup);

            // Release the gated terminate while the drain below is joining the
            // teardown thread. The store always happens-before the child's
            // allocation is freed: the teardown thread blocks on
            // `terminate_finished` before reclaiming the child.
            let child_addr = child as usize;
            let release = std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_millis(100));
                // SAFETY: the teardown thread cannot free the child before
                // observing this store (see comment above).
                (*(child_addr as *mut HewActor))
                    .terminate_finished
                    .store(true, Ordering::Release);
            });

            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            // The join barrier guarantees the teardown finished — no polling.
            assert!(
                !actor::is_actor_live_with_id(child_id, child),
                "joined teardown must have released the child actor"
            );
            assert!(
                !actor::is_actor_live_with_id(self_id, self_actor),
                "joined teardown must have released the supervisor self actor"
            );

            release.join().unwrap();
        }
    }

    #[test]
    fn drain_deferred_teardown_joins_in_flight_restart_free() {
        let _rt = crate::runtime_test_guard();
        let _serial = TEARDOWN_DRAIN_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // The ONE_FOR_ALL / REST_FOR_ONE restart arms free stopped siblings on
        // a background "deferred-free" thread that runs ordinary exact-
        // authority actor teardown on actors still tracked in
        // `LIVE_ACTORS`. `cleanup_all_actors` must join that teardown before
        // sweeping the registry, or the sweep races the in-flight free into a
        // use-after-free / double-free. This drives the production restart spawn
        // helper directly and holds the free open across the drain via a gated
        // terminate, so a drain that does NOT join the deferred-free thread
        // (the pre-fix detached spawn) returns while the sibling is still
        // tracked and the assertion fails.
        // SAFETY: this test owns the supervisor tree and gates the teardown
        // through test-controlled atomics, mirroring the supervisor-stop
        // variant above.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let child_id = (*child).id;

            // Detach the sibling from the supervisor exactly as the restart
            // arms do via `take_child_slot`, then drive it to a quiescent
            // terminal state and gate its terminate open.
            let taken = take_child_slot(&raw mut *sup, 0);
            assert_eq!(taken, child);
            let child_ref = &*child;
            child_ref
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            child_ref.terminate_called.store(true, Ordering::Release);
            child_ref.terminate_finished.store(false, Ordering::Release);

            // Production restart teardown spawn + registration.
            spawn_deferred_restart_free(vec![DeferredFree(child)]);

            // Release the gated terminate while the drain below is joining the
            // deferred-free thread. The teardown blocks in
            // actor resource teardown on `terminate_finished`
            // before reclaiming the sibling, so this store happens-before the
            // free.
            let child_addr = child as usize;
            let release = std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_millis(100));
                // SAFETY: the teardown thread cannot free the sibling before
                // observing this store (see comment above).
                (*(child_addr as *mut HewActor))
                    .terminate_finished
                    .store(true, Ordering::Release);
            });

            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            // The join barrier guarantees the teardown finished — no polling.
            assert!(
                !actor::is_actor_live_with_id(child_id, child),
                "joined restart teardown must have released the stopped sibling"
            );

            release.join().unwrap();
        }
    }

    #[test]
    fn concurrent_second_stop_returns_while_deferred_owner_waits() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree, injects a synthetic
        // current actor for the owner-thread path, and only mutates actor
        // states through test-controlled atomics.
        unsafe {
            let (sup, child, self_actor) = make_supervisor_with_child();
            // Probe liveness by (id, ptr) — see
            // stop_supervisor_from_child_dispatch_is_deferred for the ABA
            // rationale.
            let child_id = (*child).id;
            let self_id = (*self_actor).id;
            let child_sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!child_sup.is_null());
            assert_eq!(hew_supervisor_add_child_supervisor(sup, child_sup), 0);

            (*child)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);
            (*self_actor)
                .actor_state
                .store(HewActorState::Running as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: (*child).id,
                ..HewExecutionContext::default()
            });
            // Deferred-teardown windows widened 10x (200/250ms -> 2000/2500ms)
            // so a second stop returning under full-suite CI load still lands
            // well inside the window the deferred teardown owns the tree. This
            // asserts real thread scheduling, which the in-process simtime seam
            // can't fake; the deterministic fix (gate the transition on a
            // released signal, assert ordering not wall-clock) is the v0.5.5
            // de-flake (#39). The relative invariant below is unchanged.
            let self_unblock = defer_state_transition(
                self_actor,
                HewActorState::Stopped,
                std::time::Duration::from_secs(2),
            );
            let child_unblock = defer_state_transition(
                child,
                HewActorState::Stopped,
                std::time::Duration::from_millis(2_500),
            );

            hew_supervisor_stop(sup);

            let finished = std::sync::Arc::new(AtomicBool::new(false));
            let elapsed_ms = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0));
            let finished_clone = std::sync::Arc::clone(&finished);
            let elapsed_clone = std::sync::Arc::clone(&elapsed_ms);
            let sup_addr = sup as usize;
            let second = std::thread::spawn(move || {
                let start = std::time::Instant::now();
                hew_supervisor_stop(sup_addr as *mut HewSupervisor);
                let elapsed = u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX);
                elapsed_clone.store(elapsed, Ordering::Release);
                finished_clone.store(true, Ordering::Release);
            });

            assert!(
                wait_for_condition(std::time::Duration::from_secs(5), || {
                    finished.load(Ordering::Acquire)
                }),
                "second stop caller should return while deferred teardown owns the supervisor"
            );
            assert!(
                elapsed_ms.load(Ordering::Acquire) < 1_000,
                "second stop caller should not race into teardown ownership"
            );
            assert_eq!(
                locked_roster!(sup).child_supervisors.len(),
                1,
                "deferred teardown must not mutate child supervisor vectors before self actor quiesces"
            );

            second.join().unwrap();
            self_unblock.join().unwrap();
            child_unblock.join().unwrap();

            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(child_id, child)
                }),
                "child actor should still be released after the deferred winner completes"
            );
            assert!(
                wait_for_condition(std::time::Duration::from_secs(2), || {
                    !actor::is_actor_live_with_id(self_id, self_actor)
                }),
                "supervisor self actor should still be released after the deferred winner completes"
            );
        }
    }

    #[test]
    fn failed_deferred_spawn_keeps_supervisor_registered() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: this test owns the supervisor tree, injects the current actor
        // to exercise the owner-thread path, and then performs synchronous
        // cleanup once the injected spawn failure has been asserted.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let fail_guard = fail_owned_deferred_supervisor_spawn();

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor: child,
                actor_id: (*child).id,
                ..HewExecutionContext::default()
            });
            crate::hew_clear_error();
            hew_supervisor_stop(sup);

            assert!(
                crate::shutdown::is_supervisor_registered_for_test(sup),
                "failed deferred spawn must not orphan the top-level supervisor from shutdown tracking"
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null(), "spawn failure should surface an error");
            let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
            assert!(
                msg.contains("failed to spawn deferred stop thread"),
                "spawn failure should preserve the stop error, got: {msg}"
            );

            drop(fail_guard);
            hew_supervisor_stop(sup);
        }
    }

    // ---------------------------------------------------------------------------
    // Tests for hew_supervisor_child_get and hew_supervisor_nested_get
    // ---------------------------------------------------------------------------

    /// A running child returns Live with its actor pointer.
    #[test]
    fn child_get_live_returns_handle() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; cleans up after assertions.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 0, "expected Live (tag=0)");
            assert_eq!(result.reason, ChildSlotReason::Ok as u8);
            assert_eq!(result.handle, child);

            hew_supervisor_stop(sup);
        }
    }

    /// Stable role lookup returns the child's semantic identity, never its
    /// allocation address, and the supervisor token fails closed after stop.
    #[test]
    fn local_pid_child_get_returns_token_and_rejects_retired_supervisor() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the complete tree and retains only stable scalar
        // identities after stop reclaims the raw allocations.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            let child_token = (*child).local_pid_id;

            let live = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(live.tag, 0);
            assert_eq!(live.reason, ChildSlotReason::Ok as u8);
            assert_eq!(live.handle as usize, usize::from(child_token));
            assert_ne!(
                live.handle, child,
                "stable lookup exposed a raw child pointer"
            );

            let unknown = hew_local_pid_supervisor_child_get(supervisor_token, 1);
            assert_eq!(unknown.tag, 2);
            assert_eq!(unknown.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(unknown.handle.is_null());

            hew_supervisor_stop(sup);
            let retired = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(retired.tag, 2);
            assert_eq!(retired.reason, ChildSlotReason::SupervisorShutdown as u8);
            assert!(retired.handle.is_null());
        }
    }

    /// A transient restart slot remains classified and never leaks a stale
    /// incarnation token through the stable lookup ABI.
    #[test]
    fn local_pid_child_get_null_restart_slot_is_transient() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the tree and restores the child before teardown.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let transient = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(transient.tag, 1);
            assert_eq!(transient.reason, ChildSlotReason::Restarting as u8);
            assert!(transient.handle.is_null());

            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// The lookup-token-then-send shape loses the ask when the restart
    /// machinery advances the slot inside the unlocked gap. Both faces of the
    /// window, forced deterministically at the exact seam the two-call codegen
    /// sequence exposed:
    ///
    /// 1. Token resolved, replacement lands, OLD incarnation stopped, THEN the
    ///    send: refused (`ErrActorStopped`) even though the caller observed a
    ///    Live slot at resolve time.
    /// 2. Token resolved, ask ACCEPTED by the old incarnation, THEN the
    ///    replacement wave retires it: the accepted ask's reply resolves null
    ///    with only the orphaned marker — the input that surfaced as a silent
    ///    join-site trap with no diagnostic.
    #[test]
    fn stale_role_token_send_after_replacement_loses_the_ask() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree and sequences the
        // replacement wave by hand; no worker threads run.
        unsafe {
            let (sup, old_child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;

            // Face 1: resolve → replace+stop → send.
            let live = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(live.tag, 0, "pre-race lookup must observe a Live slot");
            let old_token = (*old_child).local_pid_id;
            assert_eq!(
                live.handle as usize,
                usize::from(old_token),
                "the lookup handle word is the incarnation's stable token"
            );

            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null(), "replacement spawn must succeed");
            actor::hew_actor_stop(old_child);

            let ch = crate::reply_channel::hew_reply_channel_new();
            let status =
                actor::hew_local_pid_ask_with_channel(old_token, 7, ptr::null_mut(), 0, ch.cast());
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32,
                "a stale role token resolved before the replacement wave must refuse the send"
            );
            crate::reply_channel::hew_reply_channel_free(ch);
            // The old incarnation stopped Idle → Stopped; reclaim it.
            assert_eq!(actor::hew_actor_free(old_child), 0);

            // Face 2: resolve → ask accepted → replacement wave retires the
            // target. The accepted ask's reply resolves null + orphaned.
            let live2 = hew_local_pid_supervisor_child_get(supervisor_token, 0);
            assert_eq!(live2.tag, 0, "replacement slot must be Live");
            let repl_token = (*replacement).local_pid_id;
            assert_eq!(live2.handle as usize, usize::from(repl_token));
            let ch2 = crate::reply_channel::hew_reply_channel_new();
            let status2 = actor::hew_local_pid_ask_with_channel(
                repl_token,
                7,
                ptr::null_mut(),
                0,
                ch2.cast(),
            );
            assert_eq!(
                status2,
                crate::internal::types::HewError::Ok as i32,
                "the ask must be accepted while the incarnation is current"
            );

            // The replacement wave retires the accepted-ask target: pull it
            // from the slot and tear it down (worker-less runtime: force the
            // terminal state the drain would have produced, then free — the
            // free path retires the queued ask's sender ref).
            let retired = take_child_slot(&raw mut *sup, 0);
            assert_eq!(retired, replacement);
            assert!(
                scheduler::discard_queued_actor_for_test(retired),
                "worker-less fixture must consume the accepted ask's wake entry"
            );
            (*retired)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            assert_eq!(actor::hew_actor_free(retired), 0);

            let reply = crate::reply_channel::hew_reply_wait(ch2);
            assert!(
                reply.is_null(),
                "an ask orphaned by the replacement wave must resolve a null reply"
            );
            assert_eq!(
                crate::reply_channel::hew_reply_channel_is_orphaned(ch2),
                1,
                "the orphaned marker is the only fact the null-only reply carries"
            );
            assert_eq!(
                crate::reply_channel::hew_reply_channel_failure_kind(ch2),
                crate::internal::types::HEW_REPLY_FAIL_ACTOR_STOPPED,
                "the status-bearing surface classifies the retirement as an actor stop"
            );
            crate::reply_channel::hew_reply_channel_free(ch2);

            hew_supervisor_stop(sup);
        }
    }

    /// The owner-scoped role ask resolves the slot and submits under ONE
    /// `roster` critical section: the slot writers cannot interpose
    /// (probed at the seam), and the ask lands in the incarnation that was
    /// current at resolve time — never a later one, never nowhere.
    #[test]
    fn role_ask_submits_to_resolved_incarnation_under_slot_lock() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree; the seam hook probes the
        // lock from a joined helper thread (synchronization only, no sleeps).
        unsafe {
            let (sup, old_child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            let sup_addr = sup as usize;

            let probed = Arc::new(AtomicBool::new(false));
            let probed_hook = Arc::clone(&probed);
            *ROLE_ASK_SUBMIT_GAP_HOOK.lock_or_recover() = Some(Arc::new(move || {
                let handle = std::thread::spawn(move || {
                    // The supervisor outlives the ask call that fires this
                    // hook; the probe only touches the lock word.
                    let sup = sup_addr as *mut HewSupervisor;
                    (*sup).roster.try_lock().is_err()
                });
                let writer_excluded = handle.join().expect("lock probe thread");
                assert!(
                    writer_excluded,
                    "roster must be held at the resolve→submit seam so \
                     store_child_slot/take_child_slot cannot interpose"
                );
                probed_hook.store(true, Ordering::Release);
            }));

            let ch = crate::reply_channel::hew_reply_channel_new();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                42,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            *ROLE_ASK_SUBMIT_GAP_HOOK.lock_or_recover() = None;
            assert_eq!(status, crate::internal::types::HewError::Ok as i32);
            assert!(
                probed.load(Ordering::Acquire),
                "the seam hook must have run inside the critical section"
            );

            // A replacement landing AFTER the owner-scoped submission cannot
            // repoint the already-enqueued ask.
            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null());

            let old_mb = (*old_child).mailbox.cast::<mailbox::HewMailbox>();
            let node = mailbox::hew_mailbox_try_recv(old_mb);
            assert!(
                !node.is_null(),
                "the ask must be enqueued in the incarnation resolved under the lock"
            );
            assert_eq!((*node).msg_type, 42);
            assert_eq!(
                (*node).reply_channel,
                ch.cast(),
                "the enqueued node must carry the caller's reply channel"
            );
            let repl_mb = (*replacement).mailbox.cast::<mailbox::HewMailbox>();
            assert!(
                mailbox::hew_mailbox_try_recv(repl_mb).is_null(),
                "the replacement incarnation must not receive the pre-swap ask"
            );

            // Node free retires the queued sender ref (orphan path) so the
            // creator-side wait resolves; then reclaim both incarnations.
            mailbox::hew_msg_node_free(node);
            assert!(crate::reply_channel::hew_reply_wait(ch).is_null());
            crate::reply_channel::hew_reply_channel_free(ch);

            assert!(
                scheduler::discard_queued_actor_for_test(old_child),
                "worker-less fixture must consume the accepted ask's wake entry"
            );
            (*old_child)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            assert_eq!(actor::hew_actor_free(old_child), 0);
            hew_supervisor_stop(sup);
        }
    }

    /// A mid-restart (null) slot refuses the owner-scoped ask closed: nothing
    /// is enqueued and the caller's channel reference survives.
    #[test]
    fn role_ask_mid_restart_slot_fails_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the tree; nulls the slot to model the restart
        // window, then restores it for teardown.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let before = crate::reply_channel::active_channel_count();
            let ch = crate::reply_channel::hew_reply_channel_new();
            crate::hew_clear_error();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32,
                "a mid-restart slot must refuse, never guess a future incarnation"
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null(), "the refusal must record a diagnostic");
            let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
            assert!(
                msg.contains("Restarting"),
                "the refusal must carry the classified slot state (tag semantics); got: {msg}"
            );
            assert_eq!(
                crate::reply_channel::active_channel_count(),
                before + 1,
                "the refused ask must preserve the caller-owned channel reference"
            );
            crate::reply_channel::hew_reply_channel_free(ch);

            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// Mechanism-2 regression (dogfood F1): a synchronous stable-role refusal
    /// must classify itself in the TLS ask-error slot, because the suspending
    /// with-channel caller binds its `Err` kind from
    /// `hew_actor_ask_take_last_error`. Before the fix the refusal returned
    /// its `HewError` code with the slot unwritten, so the failure surfaced
    /// as `Err(AskError::NoError)` — the enum's own "not an error" sentinel.
    #[test]
    fn role_ask_refusal_records_actor_stopped_ask_error() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the tree; nulls the slot to model the restart
        // window, then restores it for teardown.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            // Drain any stale value so the assertion reads THIS refusal's write.
            let _ = crate::actor::hew_actor_ask_take_last_error();
            let ch = crate::reply_channel::hew_reply_channel_new();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32,
                "a mid-restart slot must refuse closed"
            );
            assert_eq!(
                crate::actor::hew_actor_ask_take_last_error(),
                crate::internal::types::AskError::ActorStopped as i32,
                "the refusal must record a real AskError kind, never leave the \
                 slot at None (which misreports the failure as no-error)"
            );
            crate::reply_channel::hew_reply_channel_free(ch);

            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// A null reply channel is rejected before submission, but must still
    /// classify the failure for with-channel callers that read the TLS slot.
    #[test]
    fn role_ask_null_channel_records_ask_error() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree and passes a null channel to
        // exercise the submit guard directly.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;

            let _ = crate::actor::hew_actor_ask_take_last_error();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ptr::null_mut(),
            );
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrOom as i32,
                "a null reply channel must be rejected as OOM"
            );
            assert_eq!(
                crate::actor::hew_actor_ask_take_last_error(),
                crate::internal::types::AskError::ActorStopped as i32,
                "the null-channel refusal must report a real AskError, not NoError"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// Fixture for the lock-order test: a supervised child with a BOUNDED
    /// Block-policy mailbox (capacity 1), so a second enqueue WAITS for space.
    unsafe fn make_supervisor_with_block_mailbox_child(
    ) -> (*mut HewSupervisor, *mut HewActor, *mut HewActor) {
        // SAFETY: this helper creates a fresh supervisor tree for the test and
        // returns the owned raw pointers without publishing them elsewhere.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: 1,
                overflow: 0, // HewOverflowPolicy::Block
                coalesce_key_fn: None,
                coalesce_fallback: 0,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            assert_eq!(hew_supervisor_start(sup), 0);

            let child = locked_roster!(sup).children[0];
            let self_actor = (*sup).self_actor;
            (sup, child, self_actor)
        }
    }

    /// LOCK-ORDER INVARIANT: the role-ask enqueue — including a Block-policy
    /// capacity WAIT — never runs under `roster`. Holding the lock
    /// across the wait closes a cycle: the submitter waits for the child to
    /// drain its full mailbox, while the child's own handler can be blocked
    /// acquiring `roster` for a stable-role ask of its own. The pinned
    /// hook proves the lock is FREE at submission time, and a full Block
    /// mailbox is then drained by the test to complete the waiting enqueue.
    #[test]
    fn role_ask_block_mailbox_wait_runs_outside_roster() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree; thread coordination uses
        // barriers and joins only (no sleeps).
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_block_mailbox_child();
            let supervisor_token = (*sup).local_pid_id;
            let sup_addr = sup as usize;

            // Fill the capacity-1 mailbox so the role ask's enqueue must wait.
            actor::hew_actor_send(child, 1, ptr::null_mut(), 0);
            assert!(
                mailbox::hew_mailbox_len((*child).mailbox.cast()) >= 1,
                "the pre-fill send must occupy the capacity-1 mailbox"
            );

            let entered_submit = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered_submit);
            let lock_free_at_submit = Arc::new(AtomicBool::new(false));
            let lock_free_hook = Arc::clone(&lock_free_at_submit);
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = Some(Arc::new(move || {
                // The resolve phase released roster before this point;
                // probe from a joined helper thread (try_lock from the owning
                // thread is not the invariant under test).
                let probe = std::thread::spawn(move || {
                    let sup = sup_addr as *mut HewSupervisor;
                    // The supervisor outlives the ask that fires this hook;
                    // the probe only touches the lock word.
                    (*sup).roster.try_lock().is_ok()
                });
                lock_free_hook.store(probe.join().expect("lock probe"), Ordering::Release);
                entered_hook.wait();
            }));

            let ch = crate::reply_channel::hew_reply_channel_new();
            let ch_addr = ch as usize;
            let submitter = std::thread::spawn(move || {
                // The channel outlives the submission (the main thread joins
                // before freeing); the token is a Copy scalar identity.
                hew_supervisor_role_ask_with_channel(
                    supervisor_token,
                    0,
                    42,
                    ptr::null_mut(),
                    0,
                    ch_addr as *mut c_void,
                )
            });

            // The submitter is at (or past) the pinned-submit seam; the slot
            // lock must be free even though its enqueue may be waiting for
            // mailbox capacity.
            entered_submit.wait();
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
            assert!(
                lock_free_at_submit.load(Ordering::Acquire),
                "roster must be FREE during the role-ask submission \
                 (the Block-policy capacity wait must not run under the slot lock)"
            );
            assert!(
                (*sup).roster.try_lock().is_ok(),
                "slot writers must not be excluded while the enqueue waits"
            );

            // Drain the pre-fill message: capacity frees and the waiting
            // enqueue completes.
            let mb = (*child).mailbox.cast::<mailbox::HewMailbox>();
            let prefill = mailbox::hew_mailbox_try_recv(mb);
            assert!(!prefill.is_null());
            mailbox::hew_msg_node_free(prefill);

            let status = submitter.join().expect("submitter thread");
            assert_eq!(
                status,
                crate::internal::types::HewError::Ok as i32,
                "the waiting enqueue must complete once capacity frees"
            );

            // Drain the ask node (retires its queued sender ref), resolve the
            // creator-side wait, and tear down.
            let ask_node = mailbox::hew_mailbox_try_recv(mb);
            assert!(!ask_node.is_null());
            assert_eq!((*ask_node).msg_type, 42);
            mailbox::hew_msg_node_free(ask_node);
            assert!(crate::reply_channel::hew_reply_wait(ch).is_null());
            crate::reply_channel::hew_reply_channel_free(ch);

            // Worker-less runtime: the enqueue left the child Runnable with no
            // worker to drain it; restore Idle so the supervisor stop's
            // quiescence wait can finalize it.
            (*child)
                .actor_state
                .store(HewActorState::Idle as i32, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// A retirement landing between the classified resolve and the ID-pinned
    /// submission fails CLOSED with a named refusal — the interleaving that
    /// was a use-after-free in the raw lookup-then-ask shape (an unpinned
    /// child pointer dereferenced after the incarnation was freed). Covers
    /// both role-ask entry points; the channel twin also preserves the
    /// caller's creator reference.
    #[test]
    fn role_ask_retirement_between_resolve_and_submit_fails_closed() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the tree; the hook retires + frees the
        // resolved incarnation at the exact resolve→submit seam.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let supervisor_token = (*sup).local_pid_id;
            let sup_addr = sup as usize;

            // The hook retires the CURRENT incarnation: pull it from the
            // slot, force the terminal state the drain would have produced
            // (worker-less runtime), and free it — the exact former-UAF
            // interleaving, now required to fail closed.
            let retire_hook: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
                let sup = sup_addr as *mut HewSupervisor;
                // The supervisor outlives the ask firing this hook; the
                // retired incarnation is exclusively owned once pulled from
                // the slot.
                let retired = take_child_slot(&raw mut *sup, 0);
                assert!(!retired.is_null(), "hook must find the live incarnation");
                (*retired)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(retired), 0);
            });

            // Channel twin.
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = Some(Arc::clone(&retire_hook));
            let before = crate::reply_channel::active_channel_count();
            let ch = crate::reply_channel::hew_reply_channel_new();
            crate::hew_clear_error();
            let status = hew_supervisor_role_ask_with_channel(
                supervisor_token,
                0,
                7,
                ptr::null_mut(),
                0,
                ch.cast(),
            );
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
            assert_eq!(
                status,
                crate::internal::types::HewError::ErrActorStopped as i32
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null());
            let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
            assert!(
                msg.contains("retired during submission"),
                "the refusal must name the retirement interleaving; got: {msg}"
            );
            assert_eq!(
                crate::reply_channel::active_channel_count(),
                before + 1,
                "the refused ask must preserve the caller-owned channel reference"
            );
            crate::reply_channel::hew_reply_channel_free(ch);

            // Blocking twin: re-arm the slot with a fresh incarnation, retire
            // it at the same seam, and require the null + AskError refusal.
            let respawned = restart_child_from_spec(sup, 0);
            assert!(!respawned.is_null());
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = Some(retire_hook);
            let reply = hew_supervisor_role_ask(supervisor_token, 0, 7, ptr::null_mut(), 0);
            *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
            assert!(
                reply.is_null(),
                "the blocking role ask must refuse, never dereference the retired incarnation"
            );
            assert_eq!(
                actor::hew_actor_ask_take_last_error(),
                crate::internal::types::AskError::ActorStopped as i32,
                "the blocking refusal must bind AskError::ActorStopped"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// TOOTH for the masked-`id` reuse finding: after 2^48 allocations a fresh
    /// actor can carry a retired incarnation's packed `id` (the serial is masked
    /// to 48 bits). If phase two pinned by `id` alone it would submit to that
    /// DIFFERENT actor — the exact wrong-actor delivery the identity check
    /// exists to make impossible.
    ///
    /// This fabricates the alias at the resolve→submit seam: the hook retires
    /// the resolved incarnation A, then spawns a genuine actor B tracked under
    /// A's identical packed `id` but a DISTINCT full serial (a 2^48 wrap). A
    /// naive by-`id` pin would find B and enqueue to it; the identity-verified
    /// pin must instead refuse CLOSED (the pinned actor's full serial differs
    /// from the resolved one) and never enqueue. Covers both entry points; run
    /// 20× to shake out any ordering dependence.
    /// Build a resolve→submit-seam hook that fabricates the masked-`id` alias:
    /// it retires + frees the resolved incarnation A, then spawns a genuine
    /// actor B tracked under A's identical packed `id` but a DISTINCT full
    /// serial (a 2^48 wrap), publishing B's pointer through `alias_out`.
    ///
    /// # Safety
    ///
    /// `sup_addr` must be a live `*mut HewSupervisor` the caller owns for the
    /// hook's lifetime; the caller reclaims the actor stored in `alias_out`.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn make_role_ask_alias_hook(
        sup_addr: usize,
        alias_out: Arc<AtomicUsize>,
    ) -> Arc<dyn Fn() + Send + Sync> {
        Arc::new(move || {
            let sup = sup_addr as *mut HewSupervisor;
            // SAFETY: the caller owns `sup` for the hook's lifetime; the retired
            // incarnation is exclusively owned once pulled from the slot.
            unsafe {
                // Retire the resolved incarnation A and free it, so its masked
                // `id` is vacant in LIVE_ACTORS for reuse.
                let retired = take_child_slot(&raw mut *sup, 0);
                assert!(!retired.is_null(), "hook must find the live incarnation");
                let a_id = (*retired).id;
                let a_serial = (*retired).spawn_serial;
                (*retired)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(retired), 0);

                // Spawn a genuine actor B that ALIASES A's packed `id` (a 2^48
                // serial wrap) but carries a distinct full serial — precisely
                // the shape a real id-reuse produces.
                actor::override_next_spawn_actor_identity(a_id, a_serial.wrapping_add(1u64 << 48));
                let b = actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_child_dispatch));
                assert!(!b.is_null(), "alias actor B must spawn");
                assert_eq!((*b).id, a_id, "B must reuse A's masked packed id");
                assert_ne!(
                    (*b).spawn_serial,
                    a_serial,
                    "B must carry a distinct full serial"
                );
                alias_out.store(b as usize, Ordering::Release);
            }
        })
    }

    #[test]
    fn role_ask_masked_id_alias_refuses_closed_never_enqueues() {
        for _ in 0..20 {
            let _rt = crate::runtime_test_guard();
            // SAFETY: the test owns the tree; the hook retires the resolved
            // incarnation and installs a masked-`id`-aliasing replacement at the
            // exact resolve→submit seam, then the test reclaims both.
            unsafe {
                let (sup, _child, _self_actor) = make_supervisor_with_child();
                let supervisor_token = (*sup).local_pid_id;
                let sup_addr = sup as usize;
                // Carries the fabricated alias actor B out of the hook so the
                // test can assert it never received the ask and then free it.
                let alias_out = Arc::new(AtomicUsize::new(0));

                // ── Channel twin: refuse closed with the named diagnostic. ──
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() =
                    Some(make_role_ask_alias_hook(sup_addr, Arc::clone(&alias_out)));
                let before = crate::reply_channel::active_channel_count();
                let ch = crate::reply_channel::hew_reply_channel_new();
                crate::hew_clear_error();
                let status = hew_supervisor_role_ask_with_channel(
                    supervisor_token,
                    0,
                    7,
                    ptr::null_mut(),
                    0,
                    ch.cast(),
                );
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
                assert_eq!(
                    status,
                    crate::internal::types::HewError::ErrActorStopped as i32,
                    "an aliased id must refuse closed, never submit to the wrong actor"
                );
                let err = crate::hew_last_error();
                assert!(!err.is_null());
                let msg = std::ffi::CStr::from_ptr(err).to_string_lossy();
                assert!(
                    msg.contains("retired during submission"),
                    "the refusal must name the retirement; got: {msg}"
                );
                assert_eq!(
                    crate::reply_channel::active_channel_count(),
                    before + 1,
                    "the refused ask must preserve the caller-owned channel reference"
                );
                crate::reply_channel::hew_reply_channel_free(ch);

                // The aliasing actor B must NOT have received the ask.
                let b_channel = alias_out.swap(0, Ordering::AcqRel) as *mut HewActor;
                assert!(!b_channel.is_null(), "the channel-path hook must spawn B");
                assert_eq!(
                    crate::mailbox::hew_mailbox_len((*b_channel).mailbox.cast()),
                    0,
                    "the wrong-actor alias must never be enqueued (channel twin)"
                );
                (*b_channel)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(b_channel), 0);

                // ── Blocking twin: null reply + AskError::ActorStopped. ──
                let respawned = restart_child_from_spec(sup, 0);
                assert!(!respawned.is_null());
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() =
                    Some(make_role_ask_alias_hook(sup_addr, Arc::clone(&alias_out)));
                let reply = hew_supervisor_role_ask(supervisor_token, 0, 7, ptr::null_mut(), 0);
                *ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover() = None;
                assert!(
                    reply.is_null(),
                    "the blocking role ask must refuse, never deliver to the aliased actor"
                );
                assert_eq!(
                    actor::hew_actor_ask_take_last_error(),
                    crate::internal::types::AskError::ActorStopped as i32,
                    "the blocking refusal must bind AskError::ActorStopped"
                );

                let b_blocking = alias_out.swap(0, Ordering::AcqRel) as *mut HewActor;
                assert!(!b_blocking.is_null(), "the blocking-path hook must spawn B");
                assert_eq!(
                    crate::mailbox::hew_mailbox_len((*b_blocking).mailbox.cast()),
                    0,
                    "the wrong-actor alias must never be enqueued (blocking twin)"
                );
                (*b_blocking)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(actor::hew_actor_free(b_blocking), 0);

                hew_supervisor_stop(sup);
            }
        }
    }

    /// An out-of-range key returns Dead(UnknownSlot).
    #[test]
    fn child_get_unknown_key_returns_dead_unknown_slot() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();

            // Key 1 is out of range (only key 0 is declared).
            let result = hew_supervisor_child_get(sup, 1);
            assert_eq!(result.tag, 2, "expected Dead (tag=2)");
            assert_eq!(result.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(result.handle.is_null());

            hew_supervisor_stop(sup);
        }
    }

    /// A null supervisor pointer returns Dead(SupervisorShutdown).
    #[test]
    fn child_get_null_sup_returns_dead_supervisor_shutdown() {
        // SAFETY: null pointer is the input we are testing; the function must
        // handle it gracefully and return Dead(SupervisorShutdown) without UB.
        let result = unsafe { hew_supervisor_child_get(ptr::null_mut(), 0) };
        assert_eq!(result.tag, 2, "expected Dead (tag=2)");
        assert_eq!(result.reason, ChildSlotReason::SupervisorShutdown as u8);
        assert!(result.handle.is_null());
    }

    /// After `hew_supervisor_stop`, the supervisor has `running == 0` and
    /// subsequent lookups return Dead(SupervisorShutdown).
    #[test]
    fn child_get_stopped_supervisor_returns_dead_supervisor_shutdown() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; stop is called before the
        // pointer is last used.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();

            // Force running to 0 directly so we can query without spawning threads.
            (*sup).running.store(0, Ordering::Release);

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 2, "expected Dead (tag=2)");
            assert_eq!(result.reason, ChildSlotReason::SupervisorShutdown as u8);
            assert!(result.handle.is_null());

            // Restore to allow normal stop.
            (*sup).running.store(1, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// While the slot is null (simulating mid-restart), the lookup returns
    /// Transient(Restarting).
    #[test]
    fn child_get_null_slot_returns_transient_restarting() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; we manually null the slot to
        // simulate the restart-in-progress window, then restore it.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            // Simulate the restart-in-progress window: null the slot under lock.
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::Restarting as u8);
            assert!(result.handle.is_null());

            // Restore the slot so teardown can reach the actor.
            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    // ── await_restart cooperative observer ───────────────────────────────────

    /// Pre-park check (R4): a Live child returns READY — no park, the waiter
    /// list stays empty. The caller binds immediately instead of suspending.
    #[test]
    fn restart_await_suspend_live_child_returns_ready_no_park() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let slot = crate::read_slot::hew_read_slot_new();
            let actor = ptr::null_mut();

            let rc = hew_supervisor_restart_await_suspend(sup, 0, actor, slot);

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a Live child must return READY (no park)"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "READY path must not register a waiter"
            );

            // The caller still owns the creator ref on a READY return.
            crate::read_slot::hew_read_slot_free(slot);
            hew_supervisor_stop(sup);
        }
    }

    /// Pre-park check (R4 fail-closed): a permanently-Dead child (supervisor
    /// shut down) returns READY rather than parking forever. The resumed caller
    /// fails closed at the send re-resolve.
    #[test]
    fn restart_await_suspend_dead_child_returns_ready_never_hangs() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            // Force shutdown so child_get classifies the slot as Dead.
            (*sup).running.store(0, Ordering::Release);
            let slot = crate::read_slot::hew_read_slot_new();

            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a permanently-Dead child must return READY (fail closed, never hang)"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "the Dead fail-closed path must not register a waiter"
            );

            crate::read_slot::hew_read_slot_free(slot);
            // Restore so teardown can reach the actor.
            (*sup).running.store(1, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// A Transient slot (mid-restart) parks: SUSPEND is returned and exactly one
    /// waiter is registered. `notify_restart` (via wake) then drains the waiter
    /// list — the resume-contract anchor (`store_child_slot` before notify).
    #[test]
    fn restart_await_suspend_transient_parks_then_notify_drains() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; manually nulls the slot to
        // simulate the restart-in-progress window, then restores it.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            // Null the slot under lock → child_get returns Transient(Restarting).
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            // A null actor records `ActorIncarnation::NONE`, which never resolves,
            // so the drain runs with no wake. The incarnation controls for this
            // edge live in `restart_await_notify_*`.
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);

            assert_eq!(
                rc, RESTART_AWAIT_SUSPEND,
                "a Transient child must park (SUSPEND)"
            );
            assert_eq!(
                (*sup).restart_await_waiters.lock_or_recover().len(),
                1,
                "the park path must register exactly one waiter"
            );

            // Restore the slot (the restart completed) and fire the notify wake.
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);

            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "notify_restart must drain every parked waiter"
            );
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                1,
                "notify must release only the observer ref"
            );
            // Match the codegen bind edge: the caller releases the creator ref.
            crate::read_slot::hew_read_slot_free(slot);

            hew_supervisor_stop(sup);
        }
    }

    /// Incarnation control for the restart-await wake edge (#3069), notify arm.
    ///
    /// `RestartAwaitWaiter` captures the awaiting actor's incarnation at park
    /// time. `reincarnate` decides whether that actor dies and has its
    /// allocation handed to a fresh, unrelated incarnation before
    /// `notify_restart` drains the registry - the state a restart cycle
    /// produces when the awaiting actor itself is the one that went away.
    fn run_restart_await_notify_family(reincarnate: bool) {
        let sched = crate::scheduler::NoWorkerSchedulerForTest::install();
        let victim = crate::test_actor::TrackedTestActor::install_parked();
        // SAFETY: test owns the supervisor tree; nulling the slot under lock is
        // the documented way to present a Transient (mid-restart) child.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, victim.ptr(), slot);
            assert_eq!(
                rc, RESTART_AWAIT_SUSPEND,
                "a Transient child must park the awaiting actor"
            );

            if reincarnate {
                victim.reincarnate_parked();
            }

            // The restart landed: restore the slot, then fire the notify wake.
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);

            if reincarnate {
                crate::test_actor::assert_not_woken(&sched, &victim, "restart-await");
            } else {
                crate::test_actor::assert_woken(&sched, &victim, "restart-await");
            }

            // The caller releases the creator ref exactly as the bind edge does.
            crate::read_slot::hew_read_slot_free(slot);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn restart_await_notify_does_not_resume_a_reused_address() {
        run_restart_await_notify_family(true);
    }

    #[test]
    fn restart_await_notify_resumes_the_registering_incarnation() {
        run_restart_await_notify_family(false);
    }

    /// Incarnation control for the restart-await wake edge (#3069), TEARDOWN
    /// arm. Supervisor teardown drains the same registry through a separate
    /// call site, so it needs its own control: an awaiter that died before
    /// teardown must not hand its wake to whatever now occupies its address.
    fn run_restart_await_teardown_family(reincarnate: bool) {
        let sched = crate::scheduler::NoWorkerSchedulerForTest::install();
        let victim = crate::test_actor::TrackedTestActor::install_parked();
        // SAFETY: as above; the supervisor is consumed by the normal stop path.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, victim.ptr(), slot);
            assert_eq!(rc, RESTART_AWAIT_SUSPEND);

            if reincarnate {
                victim.reincarnate_parked();
            }

            // Teardown with the waiter still parked: the drain wakes it so the
            // resumed actor re-resolves a shut-down supervisor and fails closed.
            hew_supervisor_stop(sup);

            if reincarnate {
                crate::test_actor::assert_not_woken(&sched, &victim, "restart-await-teardown");
            } else {
                crate::test_actor::assert_woken(&sched, &victim, "restart-await-teardown");
            }

            crate::read_slot::hew_read_slot_free(slot);
        }
    }

    #[test]
    fn restart_await_teardown_does_not_resume_a_reused_address() {
        run_restart_await_teardown_family(true);
    }

    #[test]
    fn restart_await_teardown_resumes_the_registering_incarnation() {
        run_restart_await_teardown_family(false);
    }

    /// The abandon edge: detach removes the waiter and releases its ref, so a
    /// later `notify_restart` finds nothing to wake (no double-free, no leak).
    #[test]
    fn restart_await_detach_removes_waiter_before_notify() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);
            assert_eq!(rc, RESTART_AWAIT_SUSPEND);
            assert_eq!((*sup).restart_await_waiters.lock_or_recover().len(), 1);

            // Abandon: detach removes the waiter and releases the retained ref.
            hew_supervisor_restart_await_detach(sup, slot);
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "detach must remove the waiter"
            );
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                1,
                "detach must release only the observer ref"
            );
            // The direct caller still releases the creator ref after detach.
            crate::read_slot::hew_read_slot_free(slot);

            // A later notify has nothing to wake (the waiter is gone).
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);
            assert!((*sup).restart_await_waiters.lock_or_recover().is_empty());

            hew_supervisor_stop(sup);
        }
    }

    /// Guard that clears the park-gap hook on drop so a panicking test cannot
    /// leave the process-global hook installed for a sibling test.
    struct RestartAwaitParkGapHookGuard;

    impl Drop for RestartAwaitParkGapHookGuard {
        fn drop(&mut self) {
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = None;
        }
    }

    /// Lost-wakeup race regression (the B1-surface concurrency blocker): the
    /// default scheduler is multi-worker, so a restart cycle can complete
    /// (`store_child_slot` + `notify_restart`) in the gap between the pre-park
    /// `child_get` and the waiter push. Without the under-lock counter recheck the
    /// awaiting actor registers its waiter AFTER the drain already ran against an
    /// empty registry, so the wake is lost and the continuation parks forever.
    ///
    /// This drives the restart through the test-only park-gap hook so the racing
    /// `notify_restart` lands in exactly that window, deterministically. WITH the
    /// fix the awaiting call observes the advanced counter and resolves READY with
    /// no orphaned waiter; WITHOUT the fix it returns SUSPEND and leaves a waiter
    /// that nothing will ever drain (verified: removing the recheck makes the two
    /// assertions below fail — `rc` is SUSPEND and the registry holds one waiter).
    #[test]
    fn restart_await_suspend_notify_in_park_gap_does_not_lose_wakeup() {
        let _rt = crate::runtime_test_guard();
        let _hook_guard = RestartAwaitParkGapHookGuard;
        // SAFETY: the test owns the supervisor tree for its whole lifetime.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            // Null the slot so the pre-park `child_get` classifies it Transient
            // and the awaiting call proceeds toward parking.
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            // The racing restart, fired from the gap hook: restore the slot to
            // Live and complete the restart cycle (bump counter + drain waiters).
            // At this point the awaiting call has NOT yet pushed its waiter, so the
            // drain sees an empty registry — the exact lost-wakeup interleaving.
            let sup_addr = sup as usize;
            let child_addr = child as usize;
            let hook: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
                // SAFETY: the test keeps `sup`/`child` alive until after the
                // awaiting call returns; the hook runs synchronously within it.
                let sup = sup_addr as *mut HewSupervisor;
                store_child_slot(&raw mut *sup, 0, child_addr as *mut HewActor);
                notify_restart(sup);
            });
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = Some(hook);

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);

            // Disarm the hook before any further restart machinery runs.
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = None;

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a restart completing in the park gap must resolve READY, not park \
                 against a wake that already fired (lost-wakeup race)"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "the lost-wakeup recheck must NOT register an orphaned waiter that \
                 nothing will ever drain"
            );

            // READY keeps the creator ref with the caller; free it here.
            crate::read_slot::hew_read_slot_free(slot);
            hew_supervisor_stop(sup);
        }
    }

    /// The same lost-wakeup interleaving, but with `notify_restart` fired from a
    /// SEPARATE worker thread (the realistic multi-worker shape) while the
    /// awaiting actor is paused in the park gap. A bounded join backstops the
    /// teeth: WITHOUT the fix the awaiting call parks against an already-fired,
    /// drained-empty wake and the spawned awaiting thread never completes — the
    /// join times out (an observable hang). WITH the fix it resolves READY and
    /// the thread joins promptly.
    #[test]
    fn restart_await_suspend_concurrent_notify_in_gap_wakes_then_joins() {
        let _rt = crate::runtime_test_guard();
        let _hook_guard = RestartAwaitParkGapHookGuard;
        // SAFETY: the test owns the supervisor tree for its whole lifetime.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            // Two barriers coordinate the cross-thread interleaving precisely:
            //  - `in_gap` releases the notifier once the awaiting thread is in the
            //    park gap (post pre-park check, pre push);
            //  - `notified` blocks the awaiting thread until the notifier's restart
            //    cycle (bump + drain-empty) has fully completed.
            let in_gap = Arc::new(std::sync::Barrier::new(2));
            let notified = Arc::new(std::sync::Barrier::new(2));
            let in_gap_hook = Arc::clone(&in_gap);
            let notified_hook = Arc::clone(&notified);
            let hook: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
                // Signal the notifier that we are parked in the gap, then wait for
                // it to finish the racing restart cycle before we proceed to push.
                in_gap_hook.wait();
                notified_hook.wait();
            });
            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = Some(hook);

            // Awaiting actor: runs the suspend call on its own thread.
            let sup_addr = sup as usize;
            let awaiting = std::thread::spawn(move || {
                // SAFETY: the parent keeps `sup` alive until this thread joins; the
                // slot is created and freed within this thread.
                let sup = sup_addr as *mut HewSupervisor;
                let slot = crate::read_slot::hew_read_slot_new();
                let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);
                crate::read_slot::hew_read_slot_free(slot);
                rc
            });

            // Notifier: once the awaiting thread is in the gap, drive the racing
            // restart cycle (restore Live + bump counter + drain the still-empty
            // registry), then release the awaiting thread to proceed to its push.
            in_gap.wait();
            store_child_slot(&raw mut *sup, 0, child);
            notify_restart(sup);
            notified.wait();

            // Bounded teeth: poll for the awaiting thread to finish. WITHOUT the
            // recheck the awaiting call returns SUSPEND with an orphaned waiter and
            // (in a real run) the continuation never wakes; here the thread still
            // finishes (it returns SUSPEND rather than parking a real coroutine),
            // so the verdict is the rc + empty-registry assertion below, while this
            // bounded wait guarantees the test itself never hangs.
            let joined =
                wait_for_condition(std::time::Duration::from_secs(5), || awaiting.is_finished());
            assert!(
                joined,
                "awaiting thread must finish — a lost wakeup would hang it"
            );
            let rc = awaiting.join().expect("awaiting thread panicked");

            *RESTART_AWAIT_PARK_GAP_HOOK.lock_or_recover() = None;

            assert_eq!(
                rc, RESTART_AWAIT_READY,
                "a concurrent notify in the park gap must resolve the awaiting actor \
                 READY, not leave it parked against an already-fired wake"
            );
            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "no orphaned waiter may survive the racing restart"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// The contextless barrier resolves on fault-record settlement, not on a
    /// clock.
    ///
    /// Two answers from one rule. A healthy role with nothing pending returns
    /// AT ONCE — no grace window to sit out. A role with an open record BLOCKS,
    /// however long the ruling takes, which is what the deleted grace window got
    /// wrong under load. The ruling releases it.
    #[test]
    fn restart_await_blocking_resolves_on_fault_settlement() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();

            let start = std::time::Instant::now();
            hew_supervisor_restart_await_blocking(sup, 0);
            assert!(
                start.elapsed() < std::time::Duration::from_millis(100),
                "a live role with no fault pending under it must return at once"
            );

            // Open and attribute a record exactly as a supervised crash does,
            // without running one: the barrier's input is the record, so this
            // isolates it from restart timing.
            let record = crate::exit_status::open_supervised_fault();
            crate::exit_status::attribute_supervised_fault(record, child_role_chain(sup, 0));

            let sup_addr = sup as usize;
            let awaiting = std::thread::spawn(move || {
                // SAFETY: the test keeps `sup` alive until this thread joins.
                let sup = sup_addr as *mut HewSupervisor;
                // SAFETY: the supervisor outlives the wait.
                unsafe { hew_supervisor_restart_await_blocking(sup, 0) };
            });

            // The role still reads Live, so only the open record can be holding
            // the barrier. Without the record it would have returned by now, as
            // phase one just measured.
            assert!(
                !wait_for_condition(std::time::Duration::from_millis(300), || awaiting
                    .is_finished()),
                "an open record under a live role must hold the barrier"
            );

            crate::exit_status::settle_supervised_fault(record, FaultRuling::Handled);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(5), || awaiting.is_finished()),
                "the ruling must release the barrier"
            );
            awaiting.join().expect("awaiting thread panicked");

            hew_supervisor_stop(sup);
        }
    }

    /// The barrier holds until the child is back, not until a timer expires.
    ///
    /// A real crash opens the record before the terminal wake, so by the time
    /// `hew_actor_trap` returns the fault is pending under the role; the
    /// supervisor rules on its own dispatch. The barrier therefore observes the
    /// replacement incarnation, however long the ruling takes to land.
    #[test]
    fn restart_await_blocking_returns_with_the_restarted_incarnation() {
        let _rt = crate::runtime_test_guard();
        let _scheduler = RealSchedulerGuard::new();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            locked_roster!(sup).child_specs[0].restart_policy = RESTART_PERMANENT;

            actor::hew_actor_trap(child, 1);
            hew_supervisor_restart_await_blocking(sup, 0);

            assert!(
                *(*sup).restart_epoch.0.lock_or_recover() >= 1,
                "the barrier must hold until the restart cycle completes, not \
                 return on the pre-crash live window"
            );
            assert_eq!(
                hew_supervisor_child_get(sup, 0).tag,
                0,
                "the role must hold a live incarnation when the barrier returns"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// A `temporary` child that crashes is never restarted: the decline spends
    /// the spec, so the empty slot classifies `Dead(BudgetExhausted)` instead of
    /// sitting on `Transient(Restarting)` for good.
    #[test]
    fn spent_temporary_child_slot_classifies_dead_budget_exhausted() {
        let _rt = crate::runtime_test_guard();
        let _scheduler = RealSchedulerGuard::new();
        // SAFETY: the test owns the supervisor tree.
        unsafe {
            // `make_supervisor_with_child` declares the child `temporary`.
            let (sup, child, _self_actor) = make_supervisor_with_child();

            actor::hew_actor_trap(child, 1);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(10), || {
                    hew_supervisor_child_get(sup, 0).tag == 2
                }),
                "a declined restart must settle the slot Dead"
            );
            let settled = hew_supervisor_child_get(sup, 0);
            assert_eq!(
                settled.reason,
                ChildSlotReason::BudgetExhausted as u8,
                "a declined restart is BudgetExhausted, not a shutdown"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// Restores the delayed-restart arm on the way out, so one test's injected
    /// failure cannot leak into another.
    struct FailDelayedRestartArmGuard;

    impl FailDelayedRestartArmGuard {
        fn new() -> Self {
            FAIL_DELAYED_RESTART_ARM.store(true, Ordering::Release);
            Self
        }
    }

    impl Drop for FailDelayedRestartArmGuard {
        fn drop(&mut self) {
            FAIL_DELAYED_RESTART_ARM.store(false, Ordering::Release);
        }
    }

    /// A delayed restart whose timer could not be armed retires the role.
    ///
    /// `schedule_delayed_restart` returning false is the end of the line: no
    /// timer will fire, so nothing will ever refill the slot. Left unspent it
    /// classified `Transient(Restarting)` — a restart that is not coming —
    /// which every caller reads as "wait".
    ///
    /// The arm failure is injected at the seam because the real trigger is a
    /// thread spawn refused under memory or thread-limit pressure, which cannot
    /// be produced on demand.
    ///
    /// This asserts the CLASSIFICATION only. The matching "a blocked
    /// `await_restart` is released" assertion belongs with the flush barrier:
    /// on the current barrier the grace window releases the waiter regardless,
    /// so asserting it here would pass whether or not the role was retired.
    #[test]
    fn unarmable_delayed_restart_retires_the_role() {
        let _rt = crate::runtime_test_guard();
        let _scheduler = RealSchedulerGuard::new();
        let _arm = FailDelayedRestartArmGuard::new();
        // SAFETY: the test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            {
                let mut roster = locked_roster!(sup);
                let spec = &mut roster.child_specs[0];
                // Restartable by policy, and already backing off, so the crash
                // takes the DELAYED path rather than restarting in line.
                spec.restart_policy = RESTART_PERMANENT;
                spec.restart_delay_ms = 200;
            }

            actor::hew_actor_trap(child, 1);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(10), || {
                    hew_supervisor_child_get(sup, 0).tag == 2
                }),
                "a role whose restart could not be armed must settle Dead, not sit \
                 on Transient waiting for a timer that was never armed"
            );
            let settled = hew_supervisor_child_get(sup, 0);
            assert_eq!(
                settled.reason,
                ChildSlotReason::BudgetExhausted as u8,
                "an unarmable restart is a decline, not a shutdown"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// A tripped circuit breaker declines the restart the same way. Without the
    /// spent mark the slot would read `Transient(CircuitOpen)` for the whole
    /// cooldown even though nothing will refill it.
    #[test]
    fn breaker_declined_child_slot_classifies_dead_budget_exhausted() {
        let _rt = crate::runtime_test_guard();
        let _scheduler = RealSchedulerGuard::new();
        // SAFETY: the test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            {
                let mut roster = locked_roster!(sup);
                let spec = &mut roster.child_specs[0];
                // Restartable by policy, so the breaker is the only authority
                // that can decline.
                spec.restart_policy = RESTART_PERMANENT;
                spec.circuit_breaker.max_crashes = 1;
                spec.circuit_breaker.window_secs = 60;
                spec.circuit_breaker.cooldown_secs = 600;
            }

            actor::hew_actor_trap(child, 1);

            assert!(
                wait_for_condition(std::time::Duration::from_secs(10), || {
                    hew_supervisor_child_get(sup, 0).tag == 2
                }),
                "a breaker decline must settle the slot Dead"
            );
            let settled = hew_supervisor_child_get(sup, 0);
            assert_eq!(
                settled.reason,
                ChildSlotReason::BudgetExhausted as u8,
                "a declined restart is BudgetExhausted, not CircuitOpen"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// A terminal ruling drains the COOPERATIVE waiters, so a continuation
    /// parked on a Transient slot resumes into a Dead slot and fails closed at
    /// the bind. Before the spent wake nothing fired here and the continuation
    /// was parked for good.
    #[test]
    fn spent_spec_drains_parked_restart_await_waiters() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            let identity = locked_roster!(sup).child_specs[0].identity;
            // Null the slot so the pre-park lookup classifies it Transient.
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);
            assert_eq!(rc, RESTART_AWAIT_SUSPEND, "a Transient child must park");
            assert_eq!(
                (*sup).restart_await_waiters.lock_or_recover().len(),
                1,
                "the park path must register exactly one waiter"
            );

            mark_child_spec_spent(sup, identity);

            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "a spent spec must drain every parked waiter"
            );
            assert_eq!(
                hew_supervisor_child_get(sup, 0).tag,
                2,
                "the resumed continuation must re-resolve a Dead slot"
            );
            // Match the codegen bind edge: the caller releases the creator ref.
            crate::read_slot::hew_read_slot_free(slot);

            // Restore the slot so teardown can reach the actor.
            store_child_slot(&raw mut *sup, 0, child);
            locked_roster!(sup).child_specs[0].spent = false;
            hew_supervisor_stop(sup);
        }
    }

    /// Supervisor cancellation drains the COOPERATIVE waiters through the same
    /// wake point, so a parked continuation resumes into a shut-down slot
    /// instead of waiting on a supervisor that is going away.
    #[test]
    fn supervisor_cancellation_drains_parked_restart_await_waiters() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: the test owns the supervisor tree.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());

            let slot = crate::read_slot::hew_read_slot_new();
            let rc = hew_supervisor_restart_await_suspend(sup, 0, ptr::null_mut(), slot);
            assert_eq!(rc, RESTART_AWAIT_SUSPEND, "a Transient child must park");

            publish_supervisor_cancellation(sup);

            assert!(
                (*sup).restart_await_waiters.lock_or_recover().is_empty(),
                "cancellation must drain every parked waiter"
            );
            crate::read_slot::hew_read_slot_free(slot);

            // Restore so teardown can reach the actor.
            (*sup).cancelled.store(false, Ordering::Release);
            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// The contextless blocking helper returns immediately for a permanently
    /// Dead child (shut-down supervisor) — R4 fail-closed, no hang.
    #[test]
    fn restart_await_blocking_dead_child_returns_immediately() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            (*sup).running.store(0, Ordering::Release);
            let start = std::time::Instant::now();

            hew_supervisor_restart_await_blocking(sup, 0);

            assert!(
                start.elapsed() < std::time::Duration::from_millis(100),
                "a permanently-Dead child must return immediately, not block"
            );

            (*sup).running.store(1, Ordering::Release);
            hew_supervisor_stop(sup);
        }
    }

    /// When the circuit breaker is OPEN (state == 1), a null slot returns
    /// Transient(CircuitOpen).
    #[test]
    fn child_get_circuit_open_null_slot_returns_transient_circuit_open() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; we manually set state fields.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            // Null the slot and open the circuit breaker.
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());
            locked_roster!(sup).child_specs[0].circuit_breaker.state = 1; // HEW_CIRCUIT_BREAKER_OPEN

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::CircuitOpen as u8);
            assert!(result.handle.is_null());

            // Restore before teardown.
            locked_roster!(sup).child_specs[0].circuit_breaker.state = 0; // HEW_CIRCUIT_BREAKER_CLOSED
            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// When `next_restart_time_ns` is in the future (backoff window active),
    /// a null slot returns Transient(BackoffDelay).
    #[test]
    fn child_get_backoff_active_null_slot_returns_transient_backoff_delay() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree; we manually set next_restart_time_ns.
        unsafe {
            let (sup, child, _self_actor) = make_supervisor_with_child();

            // Null the slot and set the backoff deadline far in the future.
            store_child_slot(&raw mut *sup, 0, ptr::null_mut());
            // 1 hour from now in nanoseconds
            locked_roster!(sup).child_specs[0].next_restart_time_ns =
                monotonic_time_ns().saturating_add(3_600_000_000_000);

            let result = hew_supervisor_child_get(sup, 0);
            assert_eq!(result.tag, 1, "expected Transient (tag=1)");
            assert_eq!(result.reason, ChildSlotReason::BackoffDelay as u8);
            assert!(result.handle.is_null());

            // Restore before teardown.
            locked_roster!(sup).child_specs[0].next_restart_time_ns = 0;
            store_child_slot(&raw mut *sup, 0, child);
            hew_supervisor_stop(sup);
        }
    }

    /// Verify `ChildLookupResult` is 16 bytes and has the expected field layout.
    #[test]
    fn child_lookup_result_size_and_layout() {
        use std::mem;
        assert_eq!(
            mem::size_of::<ChildLookupResult>(),
            16,
            "ChildLookupResult must be 16 bytes for C ABI compatibility"
        );
        assert_eq!(
            mem::align_of::<ChildLookupResult>(),
            mem::align_of::<*mut HewActor>(),
            "ChildLookupResult must align to pointer size"
        );
    }

    /// A non-null child supervisor returns Live with the bit-cast pointer.
    #[test]
    fn nested_get_live_returns_handle() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns both supervisor trees; cleans up after assertions.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let child_sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!child_sup.is_null());
            assert_eq!(hew_supervisor_add_child_supervisor(sup, child_sup), 0);

            let result = hew_supervisor_nested_get(sup, 0);
            assert_eq!(result.tag, 0, "expected Live (tag=0)");
            assert_eq!(result.reason, ChildSlotReason::Ok as u8);
            // The handle carries the *mut HewSupervisor bit-pattern.
            assert_eq!(result.handle, child_sup.cast::<HewActor>());

            hew_supervisor_stop(sup);
        }
    }

    /// A key beyond `child_supervisors.len()` returns `Dead(UnknownSlot)`.
    #[test]
    fn nested_get_unknown_key_returns_dead_unknown_slot() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            // No nested supervisors added; key 0 is out of range.
            let result = hew_supervisor_nested_get(sup, 0);
            assert_eq!(result.tag, 2, "expected Dead (tag=2)");
            assert_eq!(result.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(result.handle.is_null());

            hew_supervisor_stop(sup);
        }
    }

    // ── state_clone_fn tests (Lane A1) ─────────────────────────────────────
    //
    // These tests exercise the supervisor-restart deep-clone path. The shape
    // mirrors the production C1 scenario: an actor holds a heap-allocated
    // owned field (here a sized-block byte buffer) and the supervisor must
    // produce an independently-owned restart-state, not a byte-alias.

    /// A miniature heap-bearing state struct used to validate clone/drop
    /// callbacks. Owns `payload` (from the sized-block allocator); the `sentinel` exists so the
    /// wrapper is non-trivially sized.
    #[repr(C)]
    struct HeapState {
        payload: *mut u8,
        payload_len: usize,
        sentinel: u32,
    }

    static CLONE_CALL_COUNT: AtomicUsize = AtomicUsize::new(0);
    static DROP_CALL_COUNT: AtomicUsize = AtomicUsize::new(0);
    static CLONE_FORCE_NULL: AtomicBool = AtomicBool::new(false);
    /// Serializes the `state_clone_fn_*` tests because they share the global
    /// `CLONE_*` / `DROP_CALL_COUNT` atomics above (test binary runs tests
    /// in parallel threads by default).
    static CLONE_TEST_SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn reset_clone_counters() {
        CLONE_CALL_COUNT.store(0, Ordering::SeqCst);
        DROP_CALL_COUNT.store(0, Ordering::SeqCst);
        CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
    }

    /// Deep-clone callback: allocates a fresh `HeapState` wrapper + fresh
    /// payload buffer, copies payload bytes. Returns null if
    /// `CLONE_FORCE_NULL` is set (used by the failure-blocks-restart test).
    unsafe extern "C-unwind" fn heap_state_clone(src: *const c_void) -> *mut c_void {
        CLONE_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
        if CLONE_FORCE_NULL.load(Ordering::SeqCst) {
            return ptr::null_mut();
        }
        // SAFETY: caller (runtime) guarantees src is a HeapState wrapper.
        let src = unsafe { &*src.cast::<HeapState>() };
        // SAFETY: allocated via the sized-block allocator to pair with buf_free in drop/teardown.
        let dst = crate::mem::buf_try_alloc(std::mem::size_of::<HeapState>()).cast::<HeapState>();
        if dst.is_null() {
            return ptr::null_mut();
        }
        let new_payload = if src.payload_len > 0 {
            // SAFETY: payload_len is in-bounds malloc size.
            let buf = crate::mem::buf_try_alloc(src.payload_len).cast::<u8>();
            if buf.is_null() {
                // SAFETY: dst was just allocated.
                unsafe { crate::mem::buf_free(dst.cast::<c_void>()) };
                return ptr::null_mut();
            }
            // SAFETY: src.payload is valid for src.payload_len bytes.
            unsafe { ptr::copy_nonoverlapping(src.payload, buf, src.payload_len) };
            buf
        } else {
            ptr::null_mut()
        };
        // SAFETY: dst was just allocated.
        unsafe {
            (*dst).payload = new_payload;
            (*dst).payload_len = src.payload_len;
            (*dst).sentinel = src.sentinel;
        }
        dst.cast::<c_void>()
    }

    /// Drop callback: frees the wrapper's payload buffer (NOT the wrapper).
    unsafe extern "C" fn heap_state_drop(state: *mut c_void) {
        DROP_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
        if state.is_null() {
            return;
        }
        // SAFETY: state is a HeapState wrapper.
        let s = unsafe { &mut *state.cast::<HeapState>() };
        if !s.payload.is_null() {
            // SAFETY: payload came from the clone callback's sized-block allocation.
            unsafe { crate::mem::buf_free(s.payload.cast::<c_void>()) };
            s.payload = ptr::null_mut();
        }
    }

    /// Build a heap-bearing initial-state template (caller owns the
    /// returned pointer; pass to `add_child_spec` which will byte-copy it).
    // Box return is intentional for clear ownership of the malloc-backed payload.
    #[allow(clippy::unnecessary_box_returns, reason = "explicit ownership in test")]
    fn make_heap_template() -> Box<HeapState> {
        // Use Box to keep ownership clear in the test; the runtime byte-copies
        // it into a sized-block buffer inside add_child_spec.
        let payload_bytes: &[u8] = b"original";
        // SAFETY: allocate the payload buffer via the sized-block allocator to match clone-fn's allocator.
        let payload = crate::mem::buf_try_alloc(payload_bytes.len()).cast::<u8>();
        // SAFETY: payload buffer came from the sized-block allocator.
        unsafe { ptr::copy_nonoverlapping(payload_bytes.as_ptr(), payload, payload_bytes.len()) };
        Box::new(HeapState {
            payload,
            payload_len: payload_bytes.len(),
            sentinel: 0xDEAD_BEEF,
        })
    }

    #[allow(
        clippy::unnecessary_box_returns,
        reason = "the returned template keeps explicit source ownership in these tests"
    )]
    unsafe fn add_heap_child(
        sup: *mut HewSupervisor,
        restart_policy: c_int,
        register_clone: bool,
    ) -> Box<HeapState> {
        let template = make_heap_template();
        let spec = HewChildSpec {
            name: ptr::null(),
            init_state: std::ptr::from_ref(&*template).cast_mut().cast::<c_void>(),
            init_state_size: std::mem::size_of::<HeapState>(),
            dispatch: Some(noop_child_dispatch),
            sys_dispatch: None,
            restart_policy,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: None,
            config: ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            // SAFETY: caller owns `sup`; `spec` and its template are live for the call.
            unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
            0
        );
        // SAFETY: successful registration added exactly one child to live `sup`.
        let index = c_int::try_from(unsafe { locked_roster!(sup).child_count - 1 })
            .expect("test child index fits c_int");
        // SAFETY: the child was just added at `index`.
        unsafe {
            hew_supervisor_set_child_state_drop(sup, index, heap_state_drop);
            if register_clone {
                hew_supervisor_set_child_state_clone(sup, index, heap_state_clone);
            }
        }
        template
    }

    unsafe fn make_supervisor_with_heap_child(
        register_clone: bool,
    ) -> (*mut HewSupervisor, Box<HeapState>) {
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 4, 1);
            assert!(!sup.is_null());
            let template = add_heap_child(sup, RESTART_PERMANENT, register_clone);
            (sup, template)
        }
    }

    unsafe fn dispatch_terminal_child_event(
        sup: *mut HewSupervisor,
        child_index: usize,
        kind: HewSysMsg,
        terminal_state: HewActorState,
    ) {
        // SAFETY: caller owns live `sup` and supplies a valid child index.
        let child = unsafe { locked_roster!(sup).children[child_index] };
        assert!(!child.is_null());
        // The production scheduler establishes the terminal state before it
        // sends this event to the supervisor system mailbox.
        // SAFETY: `sup`, `child`, and the stack event remain live for this
        // synchronous call; this is the same implementation reached by the
        // system mailbox.
        unsafe {
            (*child)
                .actor_state
                .store(terminal_state as i32, Ordering::Release);
            (*sup).running.store(1, Ordering::Release);
            let event = ChildEvent {
                child_index: u32::try_from(child_index).expect("test index fits u32"),
                child_id: (*child).id,
                exit_state: terminal_state as c_int,
                crash_code: 0,
                fault_record: 0,
            };
            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                sup.cast::<c_void>(),
                kind as i32,
                (&raw const event).cast_mut().cast::<c_void>(),
                std::mem::size_of::<ChildEvent>(),
            );
        }
    }

    #[test]
    fn production_child_events_apply_exact_state_drop_authority() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        for (terminal_state, consumed) in [
            (HewActorState::Crashed, false),
            (HewActorState::Crashed, true),
            (HewActorState::Stopped, false),
        ] {
            reset_clone_counters();
            // SAFETY: each loop iteration exclusively owns its supervisor.
            unsafe {
                let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 4, 1);
                assert!(!sup.is_null());
                let _template = add_heap_child(sup, RESTART_TEMPORARY, true);
                let child = locked_roster!(sup).children[0];
                assert!(
                    !(*child).state_drop_borrowed.load(Ordering::Acquire),
                    "clone-backed state must carry final-drop authority"
                );

                if consumed {
                    // Model the dispatch crash escrow consuming the typed
                    // state before it publishes ChildCrashed.
                    heap_state_drop((*child).state);
                    actor::record_dispatch_state_drop_consumed(child);
                }

                let kind = if terminal_state == HewActorState::Crashed {
                    HewSysMsg::ChildCrashed
                } else {
                    HewSysMsg::ChildStopped
                };
                dispatch_terminal_child_event(sup, 0, kind, terminal_state);

                assert_eq!(
                    DROP_CALL_COUNT.load(Ordering::SeqCst),
                    1,
                    "{terminal_state:?}, consumed={consumed}: production event must invoke exactly one typed drop"
                );
                assert!(locked_roster!(sup).children[0].is_null());
                hew_supervisor_stop(sup);
            }
        }
    }

    #[test]
    fn one_for_all_fresh_sibling_keeps_its_final_drop_authority() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: the test owns the complete supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ALL, 8, 60);
            assert!(!sup.is_null());
            let _template0 = add_heap_child(sup, RESTART_PERMANENT, true);
            let _template1 = add_heap_child(sup, RESTART_PERMANENT, true);
            let failed = locked_roster!(sup).children[0];
            let sibling = locked_roster!(sup).children[1];
            assert!(!(*failed).state_drop_borrowed.load(Ordering::Acquire));
            assert!(!(*sibling).state_drop_borrowed.load(Ordering::Acquire));

            // The failed actor's escrow consumes one authority. The normally
            // stopped sibling must independently consume its own authority in
            // deferred teardown; neither is a shallow-template alias.
            heap_state_drop((*failed).state);
            actor::record_dispatch_state_drop_consumed(failed);
            dispatch_terminal_child_event(sup, 0, HewSysMsg::ChildCrashed, HewActorState::Crashed);
            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "one consumed crash plus one fresh stopped sibling must drop exactly twice"
            );
            assert!(!locked_roster!(sup).children[0].is_null());
            assert!(!locked_roster!(sup).children[1].is_null());
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn rest_for_one_fresh_sibling_drops_once_and_fault_escrow_is_suppressed() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: the test owns the complete supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_REST_FOR_ONE, 8, 60);
            assert!(!sup.is_null());
            let _template0 = add_heap_child(sup, RESTART_PERMANENT, true);
            let _template1 = add_heap_child(sup, RESTART_PERMANENT, true);
            let failed = locked_roster!(sup).children[0];
            let fresh_sibling = locked_roster!(sup).children[1];

            // Crash escrow consumes the failed incarnation exactly once. The
            // later sibling stops normally and retains its independent final
            // drop authority through deferred REST_FOR_ONE teardown.
            heap_state_drop((*failed).state);
            actor::record_dispatch_state_drop_consumed(failed);
            dispatch_terminal_child_event(sup, 0, HewSysMsg::ChildCrashed, HewActorState::Crashed);
            crate::lifetime::live_actors::drain_deferred_teardown_threads();

            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "REST_FOR_ONE must suppress the consumed fault escrow and drop the fresh sibling once"
            );
            assert!(!locked_roster!(sup).children[0].is_null());
            assert!(!locked_roster!(sup).children[1].is_null());
            assert_ne!(locked_roster!(sup).children[1], fresh_sibling);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn state_clone_fn_basic_round_trip() {
        let _rt = crate::runtime_test_guard();
        // Registers a clone fn that deep-clones HeapState, drives a restart
        // via restart_child_from_spec, verifies clone_fn was invoked and the
        // new actor's state is a distinct allocation.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);

            // Registration of clone_fn re-clones spec.init_state in place to
            // break the initial byte-alias. Expect: 1 clone call so far.
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                1,
                "set_child_state_clone must re-clone the spec template once to break initial byte-alias"
            );

            let initial_child = locked_roster!(sup).children[0];
            assert!(!initial_child.is_null());
            let initial_state_ptr = (*initial_child).state;
            let spec_template_after_reg = locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state;
            assert_ne!(
                initial_state_ptr, spec_template_after_reg,
                "spec.init_state must be re-cloned to a distinct allocation; actor.state still byte-copied from original"
            );

            // Drive a restart. The supervisor sees state_clone_fn=Some and
            // routes through hew_actor_spawn_opts_adopt.
            let restarted = restart_child_from_spec(sup, 0);
            assert!(!restarted.is_null(), "restart must succeed");
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "clone_fn must be invoked once per restart (1 reg + 1 restart = 2 total)"
            );
            assert_ne!(
                (*restarted).state,
                spec_template_after_reg,
                "restarted actor.state must be a fresh clone, not aliasing the spec template"
            );
            assert!(
                (*restarted).init_state.is_null(),
                "adopt-spawn path must leave actor.init_state null (spec holds the template)"
            );
            assert!(
                (*restarted).state_drop_fn.is_some(),
                "the second actor incarnation must retain the state-drop descriptor"
            );
            assert!(
                !(*restarted).state_drop_consumed.load(Ordering::Acquire),
                "a fresh restarted incarnation begins with unconsumed final-drop authority"
            );

            // Sentinel survived the round-trip.
            let restarted_payload = &*(*restarted).state.cast::<HeapState>();
            assert_eq!(restarted_payload.sentinel, 0xDEAD_BEEF);
            assert_eq!(restarted_payload.payload_len, b"original".len());

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn restart_snapshot_cannot_publish_across_clone_generation_replacement() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: the test owns the supervisor and joins the only racing
        // restart before reclaiming either incarnation.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(false);
            let initial = locked_roster!(sup).children[0];
            assert!((*initial).state_drop_borrowed.load(Ordering::Acquire));

            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook = install_restart_spec_snapshot_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));

            let sup_addr = sup as usize;
            let stale_restart = std::thread::spawn(move || {
                // SAFETY: the parent thread keeps `sup` live through join.
                restart_child_from_spec(sup_addr as *mut HewSupervisor, 0) as usize
            });
            entered.wait();

            hew_supervisor_set_child_state_clone(sup, 0, heap_state_clone);
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), 1);
            let stored = (*initial)
                .state_clone_fn
                .expect("setter must install clone authority before ownership");
            assert!(std::ptr::fn_addr_eq(
                stored,
                heap_state_clone as actor::HewStateCloneFn
            ));
            assert!(
                !(*initial).state_drop_borrowed.load(Ordering::Acquire),
                "successful generation replacement transfers the then-current actor to owned"
            );

            release.wait();
            assert_eq!(stale_restart.join().expect("stale restart"), 0);
            assert_eq!(
                locked_roster!(sup).children[0],
                initial,
                "old-generation restart must not replace the back-filled incarnation"
            );
            drop(hook);

            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null());
            assert!(!(*replacement).state_drop_borrowed.load(Ordering::Acquire));
            assert!((*replacement).state_clone_fn.is_some());

            actor::hew_actor_stop(initial);
            assert_eq!(actor::hew_actor_free(initial), 0);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn remove_child_drops_clone_backed_actor_and_template_exactly_once_each() {
        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();

        // SAFETY: this test exclusively owns the supervisor and its sole slot.
        unsafe {
            let (sup, _source_template) = make_supervisor_with_heap_child(true);
            assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 0);
            assert_eq!(hew_supervisor_remove_child(sup, 0), 0);
            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                2,
                "remove must route actor state and independently-owned spec template through one typed drop each"
            );
            assert_eq!(hew_supervisor_remove_child(sup, 0), -1);
            hew_supervisor_stop(sup);
            assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 2);
        }
    }

    #[test]
    fn state_clone_fn_failure_blocks_restart() {
        let _rt = crate::runtime_test_guard();
        // clone_fn returns null. Verify: restart returns null, child slot
        // is null, circuit-breaker success counter is NOT advanced.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);
            // Put the breaker in HALF_OPEN: if the null-clone path
            // incorrectly called `circuit_breaker_record_success`, it would
            // transition the state back to CLOSED. Observing HALF_OPEN
            // unchanged is the strongest available signal that the success
            // path was NOT taken.
            locked_roster!(sup).child_specs[0].circuit_breaker.state = 2; // HEW_CIRCUIT_BREAKER_HALF_OPEN
            let baseline_clone_calls = CLONE_CALL_COUNT.load(Ordering::SeqCst);

            CLONE_FORCE_NULL.store(true, Ordering::SeqCst);
            let restarted = restart_child_from_spec(sup, 0);
            assert!(
                restarted.is_null(),
                "null-clone-return must propagate as a failed restart"
            );
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                baseline_clone_calls + 1,
                "clone_fn must be called exactly once before the null-return short-circuit"
            );
            assert_eq!(
                locked_roster!(sup).child_specs[0].circuit_breaker.state,
                2,
                "circuit-breaker must remain HALF_OPEN; null-clone must NOT call record_success"
            );
            assert!(
                locked_roster!(sup).children[0].is_null(),
                "child slot must be null after a blocked restart"
            );

            // Clear the flag so cleanup doesn't infinite-loop in any
            // subsequent restart attempt during stop.
            CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn borrowed_template_restart_keeps_external_payload_alive() {
        let runtime = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: source retains its payload through stop AND runtime cleanup;
        // no dispatch mutates or consumes the borrowed fields.
        let source = unsafe {
            let (sup, source) = make_supervisor_with_heap_child(false);
            assert_eq!(hew_supervisor_set_child_state_borrowed(sup, 0), 0);
            let initial = locked_roster!(sup).children[0];
            let initial_id = (*initial).id;
            (*(*initial).state.cast::<HeapState>()).sentinel = 0;

            // Registration must not turn the external owner into an actor owner.
            hew_supervisor_set_child_state_clone(sup, 0, heap_state_clone);
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), 0);
            assert!((*initial).state_drop_borrowed.load(Ordering::Acquire));
            assert!(locked_roster!(sup).child_specs[0]
                .state_template
                .clone_fn
                .is_none());

            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null());
            assert_ne!((*replacement).id, initial_id);
            assert!((*replacement).state_drop_borrowed.load(Ordering::Acquire));
            assert!((*replacement).state_drop_fn.is_some());
            let state = &*(*replacement).state.cast::<HeapState>();
            assert_eq!(
                state.sentinel, 0xDEAD_BEEF,
                "restart restores template scalar state"
            );
            assert_eq!(
                state.payload, source.payload,
                "payload remains an external borrow"
            );
            assert_eq!(
                std::slice::from_raw_parts(state.payload, state.payload_len),
                b"original"
            );

            // Direct restart replaced the slot; this test must retire its old
            // incarnation explicitly before stopping the current child/spec.
            actor::hew_actor_stop(initial);
            assert_eq!(actor::hew_actor_free(initial), 0);
            hew_supervisor_stop(sup);
            source
        };
        drop(runtime);
        assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 0);
        // SAFETY: external ownership lasted through all alias reclamation.
        unsafe {
            assert_eq!(
                std::slice::from_raw_parts(source.payload, source.payload_len),
                b"original"
            );
            crate::mem::buf_free(source.payload.cast());
        }
    }

    #[test]
    fn owned_clone_template_cannot_be_reclassified_as_borrowed() {
        let runtime = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: this test owns the clone-backed supervisor until stop.
        unsafe {
            let (sup, _source_wrapper) = make_supervisor_with_heap_child(true);
            assert_eq!(hew_supervisor_set_child_state_borrowed(sup, 0), -1);
            let child = locked_roster!(sup).children[0];
            assert!(!(*child).state_drop_borrowed.load(Ordering::Acquire));
            assert!(
                !locked_roster!(sup).child_specs[0]
                    .state_template
                    .borrows_typed_fields
            );
            hew_supervisor_stop(sup);
        }
        drop(runtime);
        assert_eq!(
            DROP_CALL_COUNT.load(Ordering::SeqCst),
            2,
            "initial actor and owned template each release their payload"
        );
    }

    #[test]
    fn borrowed_contract_wins_over_reentrant_clone_registration() {
        thread_local! {
            static TARGET: std::cell::Cell<*mut HewSupervisor> = const { std::cell::Cell::new(ptr::null_mut()) };
        }
        unsafe extern "C-unwind" fn clone_then_borrow(state: *const c_void) -> *mut c_void {
            // SAFETY: the caller leases a live HeapState template for this callback.
            let clone = unsafe { heap_state_clone(state) };
            TARGET.with(|target| {
                // SAFETY: the test keeps the target supervisor alive for the callback.
                let status = unsafe { hew_supervisor_set_child_state_borrowed(target.get(), 0) };
                assert_eq!(status, 0);
            });
            clone
        }
        let runtime = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: this thread owns the supervisor and keeps the external
        // payload alive through re-entry, stop, and complete reclamation.
        let source = unsafe {
            let (sup, source) = make_supervisor_with_heap_child(false);
            TARGET.with(|target| target.set(sup));
            hew_supervisor_set_child_state_clone(sup, 0, clone_then_borrow);
            TARGET.with(|target| target.set(ptr::null_mut()));
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), 1);
            assert_eq!(
                DROP_CALL_COUNT.load(Ordering::SeqCst),
                1,
                "only the unpublished clone is dropped"
            );
            let roster = locked_roster!(sup);
            assert!(roster.child_specs[0].state_template.borrows_typed_fields);
            assert!(roster.child_specs[0].state_template.clone_fn.is_none());
            assert!((*roster.children[0])
                .state_drop_borrowed
                .load(Ordering::Acquire));
            drop(roster);
            hew_supervisor_stop(sup);
            source
        };
        drop(runtime);
        assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 1);
        // SAFETY: the source is the sole remaining owner after all aliases retire.
        unsafe {
            assert_eq!(
                std::slice::from_raw_parts(source.payload, source.payload_len),
                b"original"
            );
            crate::mem::buf_free(source.payload.cast());
        }
    }

    #[test]
    fn borrowed_contract_invalidates_an_inflight_restart_snapshot() {
        let runtime = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: source outlives every incarnation; the only racing restart
        // is joined before any actor, spec, or external payload is reclaimed.
        let source = unsafe {
            let (sup, source) = make_supervisor_with_heap_child(false);
            let initial = locked_roster!(sup).children[0];
            let entered = Arc::new(std::sync::Barrier::new(2));
            let release = Arc::new(std::sync::Barrier::new(2));
            let entered_hook = Arc::clone(&entered);
            let release_hook = Arc::clone(&release);
            let hook = install_restart_spec_snapshot_hook_for_test(Arc::new(move || {
                entered_hook.wait();
                release_hook.wait();
            }));
            let address = sup as usize;
            let pending = std::thread::spawn(move || {
                restart_child_from_spec(address as *mut HewSupervisor, 0) as usize
            });
            entered.wait();
            assert_eq!(hew_supervisor_set_child_state_borrowed(sup, 0), 0);
            release.wait();
            assert_eq!(pending.join().unwrap(), 0);
            drop(hook);
            assert_eq!(locked_roster!(sup).children[0], initial);
            let replacement = restart_child_from_spec(sup, 0);
            assert!(!replacement.is_null());
            assert!((*replacement).state_drop_borrowed.load(Ordering::Acquire));
            actor::hew_actor_stop(initial);
            assert_eq!(actor::hew_actor_free(initial), 0);
            hew_supervisor_stop(sup);
            source
        };
        drop(runtime);
        assert_eq!(DROP_CALL_COUNT.load(Ordering::SeqCst), 0);
        // SAFETY: all aliases are reclaimed and the source still owns its payload.
        unsafe { crate::mem::buf_free(source.payload.cast()) };
    }

    #[test]
    fn state_drop_fn_without_state_clone_fn_refuses_restart() {
        let _rt = crate::runtime_test_guard();
        // A `state_drop_fn` registered without a `state_clone_fn` means the
        // actor's state owns heap fields but has no sound way to clone the
        // spec template for a restart. Byte-copying it would alias those
        // owned heap pointers between the template and the restarted actor
        // (double-free at teardown). The restart must refuse rather than
        // silently fall back to that unsound byte-copy (#1893).
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(false);
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                0,
                "no clone fn registered => no clone calls"
            );

            let restarted = restart_child_from_spec(sup, 0);
            assert!(
                restarted.is_null(),
                "a state_drop_fn registered without state_clone_fn must refuse the restart"
            );
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                0,
                "a refused restart must NOT invoke clone_fn either"
            );
            assert!(
                locked_roster!(sup).children[0].is_null(),
                "a refused restart must leave the child slot null"
            );

            hew_supervisor_stop(sup);
        }
    }

    /// Negative control for `state_drop_fn_without_state_clone_fn_refuses_restart`:
    /// a genuinely `BitCopy` actor (neither `state_drop_fn` nor `state_clone_fn`
    /// registered — no owned heap fields at all) must still restart via the
    /// legacy byte-copy path. The refusal above is specific to the dangerous
    /// drop-without-clone combination, not to "no clone fn" in general.
    #[test]
    fn no_state_ops_registered_still_uses_legacy_bytecopy_restart() {
        let _rt = crate::runtime_test_guard();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _child, _self_actor) = make_supervisor_with_child();
            let restarted = restart_child_from_spec(sup, 0);
            assert!(
                !restarted.is_null(),
                "a BitCopy actor with neither state_drop_fn nor state_clone_fn \
                 registered must still restart via the legacy byte-copy path"
            );
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn state_clone_fn_alias_freedom_under_mutation() {
        let _rt = crate::runtime_test_guard();
        // C1 regression: with clone_fn registered, mutating actor.state's
        // owned heap fields must NOT dangle spec.init_state's pointers.
        // Verifies that registration breaks the initial byte-alias and that
        // a subsequent restart deep-clones from the clean spec template.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);
            let child = locked_roster!(sup).children[0];
            assert!(!child.is_null());

            // Simulate the actor reallocating its `payload` (Vec growth):
            // free the old payload, malloc a fresh, larger one, splice into
            // actor.state. After this, if spec.init_state still aliased the
            // old payload pointer, a clone read would UAF.
            let actor_state = &mut *(*child).state.cast::<HeapState>();
            crate::mem::buf_free(actor_state.payload.cast::<c_void>());
            let new_payload = crate::mem::buf_try_alloc(64).cast::<u8>();
            assert!(!new_payload.is_null());
            libc::memset(new_payload.cast::<c_void>(), 0xAB, 64);
            actor_state.payload = new_payload;
            actor_state.payload_len = 64;

            // Critically, the spec template was re-cloned at registration
            // time; its `payload` points to an independent allocation that
            // is unaffected by the mutation above.
            let spec_template = &*locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state
                .cast::<HeapState>();
            assert_ne!(
                spec_template.payload, actor_state.payload,
                "post-registration: spec.init_state.payload must be independent from actor.state.payload"
            );
            assert_eq!(
                spec_template.payload_len,
                b"original".len(),
                "spec template payload length must reflect the clean clone, not the mutated actor"
            );

            // Restart: clone_fn reads spec.init_state (clean), not
            // actor.state (mutated). Must not UAF.
            let baseline_clones = CLONE_CALL_COUNT.load(Ordering::SeqCst);
            let restarted = restart_child_from_spec(sup, 0);
            assert!(!restarted.is_null());
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), baseline_clones + 1);

            let restarted_state = &*(*restarted).state.cast::<HeapState>();
            assert_eq!(
                restarted_state.payload_len,
                b"original".len(),
                "restart must reproduce the clean template, not the mutated actor's state"
            );
            assert_ne!(
                restarted_state.payload, spec_template.payload,
                "restart payload must be an independent clone, not aliasing the spec template"
            );

            hew_supervisor_stop(sup);
        }
    }

    const FORCED_BYTECOPY_FREED_SPEC_ENV: &str = "HEW_SUPERVISOR_FORCED_BYTECOPY_FREED_SPEC_PROBE";

    #[cfg(target_os = "macos")]
    const GUARD_MALLOC_DYLIB: &str = "/usr/lib/libgmalloc.dylib";

    unsafe fn free_spec_template_payload(sup: *mut HewSupervisor) -> *mut u8 {
        let spec_template = locked_roster!(sup).child_specs[0]
            .state_template
            .allocation
            .state
            .cast::<HeapState>();
        assert!(!spec_template.is_null());
        let payload = (*spec_template).payload;
        assert!(!payload.is_null());
        crate::mem::buf_free(payload.cast::<c_void>());
        payload
    }

    unsafe fn run_forced_bytecopy_freed_spec_payload_probe() -> ! {
        // Install a runtime so spawn/track resolve. Before #1893 this
        // subprocess faulted intentionally (GuardMalloc SIGSEGV) by forcing
        // the legacy byte-copy path to alias a freed spec payload. That
        // path is no longer reachable: a `state_drop_fn` registered without
        // a `state_clone_fn` now refuses the restart outright, so this
        // probe proves the refusal instead of the fault.
        let _rt = crate::runtime_test_guard();
        let (sup, _template) = make_supervisor_with_heap_child(false);
        let _dangling_payload = free_spec_template_payload(sup);

        let restarted = restart_child_from_spec(sup, 0);
        assert!(
            restarted.is_null(),
            "a state_drop_fn registered without state_clone_fn must refuse the restart, \
             not byte-copy a freed spec payload"
        );

        std::process::exit(0);
    }

    #[cfg(target_os = "macos")]
    fn assert_forced_bytecopy_freed_spec_faults_under_guard_malloc(test_name: &str) {
        if !std::path::Path::new(GUARD_MALLOC_DYLIB).exists() {
            eprintln!("skipping GuardMalloc alias-fault probe: {GUARD_MALLOC_DYLIB} not found");
            return;
        }

        let status = std::process::Command::new(std::env::current_exe().expect("current_exe"))
            .args(["--exact", test_name, "--nocapture"])
            .env("RUST_TEST_THREADS", "1")
            .env(FORCED_BYTECOPY_FREED_SPEC_ENV, "1")
            .env("DYLD_INSERT_LIBRARIES", GUARD_MALLOC_DYLIB)
            .env("MallocGuardEdges", "1")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .expect("spawn GuardMalloc alias-fault helper");
        assert!(
            status.success(),
            "the refused restart must exit cleanly under GuardMalloc, never fault; status={status:?}"
        );
    }

    #[cfg(not(target_os = "macos"))]
    fn assert_forced_bytecopy_freed_spec_faults_under_guard_malloc(_test_name: &str) {}

    #[test]
    fn null_clone_restart_blocks_freed_spec_payload_alias_probe() {
        if std::env::var_os(FORCED_BYTECOPY_FREED_SPEC_ENV).is_some() {
            // SAFETY: helper runs in a subprocess and intentionally faults
            // under GuardMalloc after constructing the legacy byte-copy alias.
            unsafe { run_forced_bytecopy_freed_spec_payload_probe() };
        }

        let _rt = crate::runtime_test_guard();
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree and mutates only its test state.
        unsafe {
            let (sup, _template) = make_supervisor_with_heap_child(true);
            let child = locked_roster!(sup).children[0];
            assert!(!child.is_null());

            // Poison the source every restart path actually reads:
            // spec.init_state, not the current child actor's state.
            let dangling_payload = free_spec_template_payload(sup);
            let baseline_clones = CLONE_CALL_COUNT.load(Ordering::SeqCst);

            CLONE_FORCE_NULL.store(true, Ordering::SeqCst);
            let restarted = restart_child_from_spec(sup, 0);
            assert!(
                restarted.is_null(),
                "null-clone policy must block the restart instead of byte-copying a freed spec payload"
            );
            assert_eq!(
                CLONE_CALL_COUNT.load(Ordering::SeqCst),
                baseline_clones + 1,
                "restart must call clone_fn once before the null-return short-circuit"
            );
            assert!(
                locked_roster!(sup).children[0].is_null(),
                "blocked restart must leave the child slot null"
            );
            assert_eq!(
                (&*locked_roster!(sup).child_specs[0]
                    .state_template
                    .allocation
                    .state
                    .cast::<HeapState>())
                    .payload,
                dangling_payload,
                "test setup must leave the freed spec payload in place as the byte-copy falsifier"
            );

            CLONE_FORCE_NULL.store(false, Ordering::SeqCst);
            // Null the already-freed spec payload so that InternalChildSpec::drop
            // (which now calls state_drop_fn before buf_free) does not double-free
            // the dangling pointer.  The falsifier assertion above already verified
            // it was in place; the test's correctness doesn't depend on it surviving
            // past that point.
            (&mut *locked_roster!(sup).child_specs[0]
                .state_template
                .allocation
                .state
                .cast::<HeapState>())
                .payload = ptr::null_mut();
            assert_eq!(actor::hew_actor_free(child), 0);
            hew_supervisor_stop(sup);
        }

        assert_forced_bytecopy_freed_spec_faults_under_guard_malloc(
            "supervisor::tests::null_clone_restart_blocks_freed_spec_payload_alias_probe",
        );
    }

    #[test]
    fn hew_supervisor_set_child_state_clone_back_fills() {
        let _rt = crate::runtime_test_guard();
        // Setting the clone fn after add_child_spec must back-fill it onto
        // the already-spawned actor so future direct-spawn restart consumers
        // see the same callback. Mirror of the state_drop_fn back-fill test.
        let _serial = CLONE_TEST_SERIAL
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_clone_counters();
        // SAFETY: test owns the supervisor tree.
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1);
            assert!(!sup.is_null());
            let template = make_heap_template();
            let spec = HewChildSpec {
                name: ptr::null(),
                init_state: std::ptr::from_ref(&*template).cast_mut().cast::<c_void>(),
                init_state_size: std::mem::size_of::<HeapState>(),
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_TEMPORARY,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            let child = locked_roster!(sup).children[0];
            assert!(!child.is_null());
            assert!(
                (*child).state_clone_fn.is_none(),
                "before set_child_state_clone, actor.state_clone_fn must be None"
            );

            hew_supervisor_set_child_state_clone(sup, 0, heap_state_clone);

            let stored = (*child)
                .state_clone_fn
                .expect("back-fill must populate actor.state_clone_fn");
            assert_eq!(
                stored as *const () as usize, heap_state_clone as *const () as usize,
                "back-filled fn pointer must match the registered fn"
            );
            // The spec template was re-cloned during registration.
            assert_eq!(CLONE_CALL_COUNT.load(Ordering::SeqCst), 1);

            // Stop without enabling clone-from-fail; cleans up the heap
            // allocations via state_drop_fn on actor.state and buf_free of
            // the cloned spec template.
            hew_supervisor_stop(sup);
        }
    }
}
