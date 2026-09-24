//! Supervisor teardown, quiescence and restart-snapshot helpers.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

pub(crate) fn claim_supervisor_teardown(sup: *mut HewSupervisor) -> bool {
    if sup.is_null() {
        return false;
    }
    // SAFETY: callers guarantee a live allocation; copy its stable authority
    // without creating a whole-supervisor reference.
    let (runtime, token) = unsafe { ((*sup).runtime, (*sup).local_pid_id) };
    if runtime.is_null() {
        return false;
    }
    // SAFETY: a live supervisor's runtime authority outlives its control.
    let Some(control) = (unsafe { &*runtime })
        .local_handles
        .supervisor_control_for_raw(token, sup)
    else {
        return false;
    };
    control.claim_teardown()
}

pub(crate) fn release_supervisor_teardown(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    // SAFETY: only used to roll back a failed deferred-spawn attempt while the
    // supervisor is still live and owned by the caller.
    let (runtime, token) = unsafe { ((*sup).runtime, (*sup).local_pid_id) };
    if runtime.is_null() {
        return;
    }
    // SAFETY: the live allocation keeps its runtime authority installed.
    if let Some(control) = (unsafe { &*runtime })
        .local_handles
        .supervisor_control_for_raw(token, sup)
    {
        control.release_teardown();
    }
}

pub(crate) fn request_supervisor_shutdown(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    publish_supervisor_cancellation(sup);
    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe { (*sup).running.store(0, Ordering::Release) };
}

pub(crate) fn publish_supervisor_cancellation(sup: *mut HewSupervisor) {
    // Publish the wait-free mirror first. Timer callbacks that already own the
    // control mutex observe this bit and skip their send; acquiring the control
    // next wakes sleepers and waits out any raw access already in progress.
    // SAFETY: callers keep `sup` live for these projected field operations.
    unsafe {
        (*sup).cancelled.store(true, Ordering::Release);
        (*sup).restart_timers.cancel();
    }
    // A pending group restart will never run. Its stopped incarnations stay
    // with the roster until teardown frees them; its records settle now.
    let records = {
        // SAFETY: callers keep `sup` live; no roster reference is held here.
        let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
        roster
            .pending_group_restart
            .as_mut()
            .map(|group| std::mem::take(&mut group.failures))
            .unwrap_or_default()
    };
    for (_, record) in records {
        crate::exit_status::settle_supervised_fault(record, FaultRuling::Unrecovered);
    }
    // Every slot now reads Dead. Release the restart barriers so a blocked
    // waiter drops its supervisor pin before teardown drains pins.
    wake_restart_waiters(sup);
}

/// Record that this supervisor will not restart `identity` and release every
/// restart barrier on it.
///
/// The decline exits of `decide_child_failure` land here: the roster slot stays
/// empty for good, so `classify_null_child_slot` must read it as Dead rather
/// than Transient and a barrier waiting on it must return instead of blocking
/// on a restart that will never come.
///
/// LOCK ORDER: the roster guard is released before the wake acquires the epoch.
pub(crate) fn mark_child_spec_spent(sup: *mut HewSupervisor, identity: u64) {
    {
        // SAFETY: callers keep `sup` live; the guard serializes this scoped
        // spec mutation with restart publication and dynamic removal.
        let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
        if let Some(spec) = roster
            .child_specs
            .iter_mut()
            .find(|candidate| candidate.identity == identity)
        {
            spec.spent = true;
        }
    }
    wake_restart_waiters(sup);
}

#[inline]
fn supervisor_quiescence_expired(deadline: Instant) -> bool {
    scheduler::shutdown_requested() || Instant::now() >= deadline
}

fn actor_is_supervisor_quiescent(actor: *mut HewActor) -> bool {
    // SAFETY: callers keep `actor` live throughout their wait.
    let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    state != HewActorState::Running as i32 && state != HewActorState::Runnable as i32
}

pub(crate) fn wait_for_supervisor_self_actor_quiescent(
    sup: *mut HewSupervisor,
    deadline: Instant,
) -> bool {
    if sup.is_null() {
        return true;
    }

    // SAFETY: caller guarantees `sup` is a valid live supervisor pointer.
    unsafe {
        let self_actor = (*sup).self_actor;
        if self_actor.is_null() {
            return true;
        }

        actor::hew_actor_stop(self_actor);
        loop {
            if actor_is_supervisor_quiescent(self_actor) {
                return true;
            }
            if supervisor_quiescence_expired(deadline) {
                return false;
            }
            std::thread::yield_now();
        }
    }
}

pub(crate) fn wait_for_pending_restart_timers(
    timers: &RestartTimerControl,
    deadline: Instant,
) -> bool {
    // The caller publishes cancellation before reaching this bounded wait.
    timers.wait_for_drain(deadline)
}

pub(crate) fn wait_for_child_quiescent(child: *mut HewActor, deadline: Instant) -> bool {
    // A native incarnation is done only once its terminal cleanup, stop hooks
    // included, has finished: a stop hook parked at a suspension point leaves
    // the actor Suspended, which is not Running yet is not finished either.
    // SAFETY: callers keep `child` live throughout their wait.
    let completion = unsafe { (*child).native_completion.clone() };
    let done = || {
        completion.as_ref().map_or_else(
            || actor_is_supervisor_quiescent(child),
            |completion| completion.is_finished(),
        )
    };
    while !done() {
        if supervisor_quiescence_expired(deadline) {
            return false;
        }
        std::thread::yield_now();
    }
    true
}

unsafe fn return_supervisor_to_runtime_cleanup(sup: *mut HewSupervisor) {
    // Top-level stops unregister before the deferred owner starts. If scheduler
    // shutdown prevents that owner from reaching a safe actor-quiescence point,
    // restore the root so canonical post-worker cleanup remains the sole
    // destructor. Keep teardown claimed so a still-running worker cannot start
    // a second stop before the root sweep. Nested supervisors stay owned by
    // their parent tree.
    // SAFETY: the deferred owner still holds the live supervisor allocation.
    if unsafe { (*sup).parent.is_null() } {
        // SAFETY: the deferred owner is returning the still-live allocation
        // without consuming it.
        // SAFETY: canonical cleanup still owns the live top-level allocation.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    }
}

/// Race for a nested supervisor's teardown authority while its parent still
/// owns the roster entry. Only the winner detaches and publishes a cleanup
/// root. The stable-token pin keeps the allocation live through the claim, so
/// a losing parent can discard its roster word without dereferencing a pointer
/// the token-based winner may immediately reclaim.
///
/// # Safety
///
/// `sup` and `token` must be the parallel values extracted from one parent
/// roster critical section.
pub(crate) unsafe fn claim_nested_supervisor_for_detach(
    sup: *mut HewSupervisor,
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> bool {
    if sup.is_null() {
        return false;
    }
    let pin = crate::lifetime::local_handles::pin_current_supervisor(token);
    let control = if let Some(pin) = pin.as_ref() {
        if pin.supervisor() != sup {
            return false;
        }
        pin.control()
    } else {
        let Some(control) =
            crate::lifetime::local_handles::current_supervisor_control_for_raw(token, sup)
        else {
            return false;
        };
        // Cleanup may close this nested route before the parent's already-
        // admitted teardown owner resumes. Only use the raw parent ownership
        // edge after closure has also drained every admitted dereference.
        if !control.is_closed_and_drained() {
            return false;
        }
        control
    };
    if !control.claim_teardown() {
        return false;
    }
    // SAFETY: the stable pin proves the allocation live and the successful
    // claim makes this path the only authority allowed to detach/publish it.
    // On the cleanup fallback, the exact control proves access is closed and
    // fully drained while the parent roster retains allocation ownership.
    unsafe { (*sup).parent = ptr::null_mut() };
    // SAFETY: the claimed detached allocation remains live through stop or a
    // fail-closed handoff back to canonical cleanup.
    unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
    drop(pin);
    true
}

pub(crate) fn take_nested_supervisor_roster(
    sup: *mut HewSupervisor,
) -> Vec<(
    *mut HewSupervisor,
    crate::lifetime::local_handles::HewLocalPidId,
    Option<SupervisorChildSpec>,
)> {
    // SAFETY: callers keep `sup` live and transfer the complete nested roster
    // as one lock-protected ownership unit.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    debug_assert_eq!(
        roster.child_supervisors.len(),
        roster.child_supervisor_tokens.len()
    );
    debug_assert_eq!(
        roster.child_supervisors.len(),
        roster.child_supervisor_specs.len()
    );
    let supervisors = std::mem::take(&mut roster.child_supervisors);
    let tokens = std::mem::take(&mut roster.child_supervisor_tokens);
    let specs = std::mem::take(&mut roster.child_supervisor_specs);
    supervisors
        .into_iter()
        .zip(tokens)
        .zip(specs)
        .map(|((supervisor, token), spec)| (supervisor, token, spec))
        .collect()
}

fn wait_for_retiring_children(supervisor: &HewSupervisor, deadline: Instant) -> bool {
    loop {
        if supervisor
            .roster
            .lock_or_recover()
            .retiring_children
            .iter()
            .all(|completion| completion.is_finished())
        {
            return true;
        }
        if supervisor_quiescence_expired(deadline) {
            return false;
        }
        std::thread::yield_now();
    }
}

pub(crate) unsafe fn stop_supervisor_owned(
    sup: *mut HewSupervisor,
    teardown: &crate::lifetime::local_handles::SupervisorTeardownLease,
) {
    // Cancel BEFORE closing access. A restart barrier blocked on a child slot
    // holds a supervisor pin; cancellation makes every slot read Dead and wakes
    // it, so it returns and drops that pin instead of stalling the pin drain
    // below for its full timeout. Re-published by `request_supervisor_shutdown`
    // after the close, which is idempotent.
    // SAFETY: the caller transferred a live allocation to this owner.
    publish_supervisor_cancellation(sup);
    // ROSTER-EXCLUSIVE: access admission is closed and all stable pins drain
    // before the Box-owned teardown traversal below begins.
    // Every raw destructor path closes handle admission and drains outstanding
    // dereferences before the allocation may reach `Box::from_raw`.
    // SAFETY: the caller transfers a live supervisor allocation to this owner.
    let Some(access) = (unsafe { close_supervisor_access(sup, SUPERVISOR_PIN_DRAIN_TIMEOUT) })
    else {
        set_last_error("supervisor handle pins did not drain before reclamation");
        // SAFETY: access closure failed closed, so this owner still holds a
        // live allocation. Restore top-level ownership before its teardown
        // lease can release the runtime-cleanup barrier.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    };
    request_supervisor_shutdown(sup);
    let quiescence_deadline = Instant::now() + SUPERVISOR_QUIESCENCE_TIMEOUT;
    if !wait_for_supervisor_self_actor_quiescent(sup, quiescence_deadline) {
        set_last_error("supervisor teardown timed out waiting for self actor quiescence");
        // SAFETY: teardown ownership is still held and no allocation was
        // consumed; canonical runtime cleanup takes ownership back.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    }

    // SAFETY: teardown ownership is held exclusively by this thread and the
    // supervisor memory remains live until `Box::from_raw` below.
    // SAFETY: teardown keeps the allocation and its Arc field live through the
    // bounded wait.
    if !wait_for_pending_restart_timers(unsafe { &(*sup).restart_timers }, quiescence_deadline) {
        set_last_error("supervisor teardown timed out waiting for restart timers");
        // SAFETY: no ownership was consumed; a timer may still hold a raw
        // borrow, so canonical cleanup must retain the allocation.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    }

    // SAFETY: teardown ownership was claimed once for this supervisor, the
    // self actor is no longer dispatching, and no other thread may consume the
    // raw pointer now.
    let mut s = unsafe { Box::from_raw(sup) }; // ALLOCATOR-PAIRING: GlobalAlloc

    // Recursively stop all child supervisors first.
    for (child_sup, child_token, _child_spec) in take_nested_supervisor_roster(&raw mut *s) {
        if !child_sup.is_null() {
            retain_nested_completion(&raw mut *s, child_token);
            // Claim while the parent-owned roster extraction and stable token
            // still jointly identify the allocation. A losing path does not
            // touch `child_sup`; the concurrent winner owns reclamation.
            // SAFETY: pointer/token are one extracted parallel entry.
            if unsafe { claim_nested_supervisor_for_detach(child_sup, child_token) } {
                // SAFETY: the claim above is the unique child teardown authority.
                unsafe {
                    stop_supervisor_with_teardown_authority(child_sup, teardown.clone(), true);
                };
            }
        }
    }
    // Stop all children and wait for each to reach a terminal state.
    let child_count = s.roster.lock_or_recover().child_count;
    for i in 0..child_count {
        let child = take_child_slot(&raw mut *s, i);
        if !child.is_null() {
            // SAFETY: child pointer is valid.
            unsafe { actor::hew_actor_stop(child) };
            if !wait_for_child_quiescent(child, quiescence_deadline) {
                set_last_error("supervisor teardown timed out waiting for child quiescence");
                // `take_child_slot` detached this still-live child. Restore it
                // before returning ownership, otherwise canonical cleanup could
                // free the supervisor while the child remains unowned/live.
                store_child_slot(&raw mut *s, i, child);
                let sup = Box::into_raw(s);
                // SAFETY: Box ownership is converted back to the raw pointer
                // expected by canonical runtime cleanup.
                unsafe { return_supervisor_to_runtime_cleanup(sup) };
                return;
            }
            // SAFETY: child has reached a wake-proof terminal state.
            unsafe { actor::hew_actor_free(child) };
        }
    }

    // Detached incarnations remain part of the parent's cleanup obligation.
    if !wait_for_retiring_children(&s, quiescence_deadline) {
        set_last_error("supervisor teardown retained an unfinished child subtree");
        let sup = Box::into_raw(s);
        // SAFETY: the parent stays live until its detached children finish.
        unsafe { return_supervisor_to_runtime_cleanup(sup) };
        return;
    }
    // Every incarnation a pending group restart stopped has finished above.
    let pending = s.roster.lock_or_recover().pending_group_restart.take();
    if let Some(group) = pending {
        group.abandon();
    }

    if !s.self_actor.is_null() {
        // SAFETY: self_actor was stopped and waited above; it is no longer
        // dispatching and now only needs final pointer cleanup and free.
        unsafe {
            (*s.self_actor).state = ptr::null_mut();
            (*s.self_actor).state_size = 0;
            actor::hew_actor_free(s.self_actor);
        }
        s.self_actor = ptr::null_mut();
    }

    // Drain any parked `await_restart` continuations on teardown: wake each so
    // the resumed actor re-resolves the (now shut-down) supervisor and fails
    // closed (`child_get` → Dead(SupervisorShutdown)) rather than hanging
    // forever, and release the observer's retained slot ref. Mirrors the
    // notify_restart wake discipline; teardown is the abandon-everything edge.
    let parked: Vec<RestartAwaitWaiter> =
        std::mem::take(&mut *s.restart_await_waiters.lock_or_recover());
    for waiter in parked {
        // SAFETY: the observer holds an in-flight ref; depositing readiness is
        // the reactor-deposit contract (no-op if the abandon edge cancelled it).
        let do_wake = unsafe {
            crate::read_slot::read_slot_deposit_status(
                waiter.slot,
                crate::read_slot::ReadStatus::Data,
            )
        };
        if do_wake {
            crate::scheduler::enqueue_resume_by_incarnation(waiter.actor);
        }
        // SAFETY: the observer owned this ref; nothing else releases it.
        unsafe { crate::read_slot::hew_read_slot_free(waiter.slot) };
    }

    let (pools, config_buf, config_drop_fn) = {
        let mut roster = s.roster.lock_or_recover();
        let pools = std::mem::take(&mut roster.pool_slots);
        let config_buf = std::mem::replace(&mut roster.config_buf, ptr::null_mut());
        let config_drop_fn = roster.config_drop_fn.take();
        roster.config_size = 0;
        (pools, config_buf, config_drop_fn)
    };

    // Free pool slots. Each pool was Box-allocated by hew_supervisor_pool_add_slot;
    // pool_specs Drop impl handles name deallocation.
    for pool in pools {
        if !pool.is_null() {
            // SAFETY: pool was created by Box::into_raw in hew_supervisor_pool_add_slot.
            unsafe { drop(Box::from_raw(pool)) }; // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    // Free the construction-time config buffer (the init-closure restart
    // model's dynamic-data source). Freed EXACTLY ONCE here: every child spec
    // holds only a BORROW of this pointer, and the thunks only ever read it.
    // After every child actor and spec is dropped above, no live thunk can run,
    // so the buffer has no remaining readers.
    if !config_buf.is_null() {
        // The config buffer is a flat snapshot of the moved-in config value, so
        // it OWNS the config struct's inner owned fields (`string`/`bytes`/…) —
        // the thunks only CLONE from them into actor state (the actors'
        // state_drop_fns release those clones). Run the config struct's
        // drop-inplace glue here to release the buffer's OWN inner owned fields
        // BEFORE the flat free; without it those fields leak (the flat free
        // reclaims only the wrapper). `None` for an all-scalar config (no inner
        // owned field to drop). No live thunk can run at this point (every child
        // actor and spec is dropped above), so this is the sole final reader.
        if let Some(drop_fn) = config_drop_fn {
            // SAFETY: drop_fn is the codegen-emitted
            // `__hew_record_drop_inplace_<ConfigTy>` for this buffer's config
            // struct; config_buf points at a fully-initialised instance of that
            // struct. Runs exactly once (config_buf is freed + nulled below).
            unsafe { drop_fn(config_buf) };
        }
        // SAFETY: config_buf was a sized-block allocation adopted (ownership
        // transferred) from codegen via hew_supervisor_add_child_spec /
        // hew_supervisor_set_child_init_fn. Inner owned fields were released by
        // config_drop_fn above; this free reclaims the config wrapper itself.
        unsafe { crate::mem::buf_free(config_buf) }; // ALLOCATOR-PAIRING: GlobalAlloc
    }
    drop(s);
    finish_supervisor_reclamation(&access);
}

/// Restart a child from its spec, returning the new actor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_count` (for
/// restarts) or equal to `child_count` (for initial spawns, where the
/// caller is responsible for pushing the result onto the `children` vec).
fn restart_snapshot_is_current(
    sup: *mut HewSupervisor,
    index: usize,
    spec_identity: u64,
    spec_revision: u64,
    template: &Arc<ChildStateTemplate>,
) -> bool {
    // SAFETY: caller keeps the supervisor allocation live for this operation.
    let roster = unsafe { &(*sup).roster }.lock_or_recover();
    roster.child_specs.get(index).is_some_and(|spec| {
        spec.identity == spec_identity
            && spec.revision == spec_revision
            && Arc::ptr_eq(&spec.state_template, template)
    })
}

fn fail_restart_snapshot(
    sup: *mut HewSupervisor,
    index: usize,
    spec_identity: u64,
    spec_revision: u64,
    template: &Arc<ChildStateTemplate>,
) {
    // SAFETY: caller keeps the allocation live; roster mutation is serialized.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    let child_specs = &mut roster.child_specs;
    let Some(spec) = child_specs.get_mut(index) else {
        return;
    };
    if spec.identity != spec_identity
        || spec.revision != spec_revision
        || !Arc::ptr_eq(&spec.state_template, template)
    {
        return;
    }
    apply_restart_backoff(spec);
    // SAFETY: the same roster guard protects child-slot mutation.
    if let Some(slot) = roster.children.get_mut(index) {
        *slot = ptr::null_mut();
    }
}

fn publish_restart_snapshot(
    sup: *mut HewSupervisor,
    index: usize,
    spec_identity: u64,
    spec_revision: u64,
    template: &Arc<ChildStateTemplate>,
    child: *mut HewActor,
) -> bool {
    // SAFETY: caller keeps the allocation live; roster mutation is serialized.
    let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: caller keeps the allocation live through this guarded publish.
    let sup_actor_id = supervisor_actor_id(sup);
    // SAFETY: the roster guard above provides exclusive spec access.
    let child_specs = &mut roster.child_specs;
    let Some(spec) = child_specs.get_mut(index) else {
        return false;
    };
    if spec.identity != spec_identity
        || spec.revision != spec_revision
        || !Arc::ptr_eq(&spec.state_template, template)
    {
        return false;
    }

    // Complete registration while the exact-generation check and slot store
    // are still indivisible. A state-drop setter may have published into the
    // shared descriptor after this restart's initial snapshot; reload it here
    // so no late actor can replace the back-filled predecessor with `None`.
    if !child.is_null() {
        if let Some(drop_fn) = template.allocation.state_drop.load() {
            // SAFETY: `child` is the unpublished actor built from this exact
            // spec generation and the descriptor matches its state layout.
            unsafe { actor::hew_actor_set_state_drop(child, drop_fn) };
        }
        if let Some(clone_fn) = template.clone_fn {
            // SAFETY: same exact-generation argument as the drop descriptor.
            unsafe { actor::hew_actor_set_state_clone(child, clone_fn) };
        }
    }
    circuit_breaker_record_success(spec, sup_actor_id);
    // SAFETY: the same roster guard protects child-slot mutation.
    if let Some(slot) = roster.children.get_mut(index) {
        *slot = child;
    }
    true
}

unsafe fn discard_unpublished_restart(child: *mut HewActor) {
    if child.is_null() {
        return;
    }
    // SAFETY: the child was freshly spawned but never published in a roster
    // slot, so this thread is its only lifecycle owner.
    unsafe {
        actor::hew_actor_stop(child);
        let _ = actor::hew_actor_free(child);
    }
}

pub(crate) unsafe fn restart_child_from_spec(
    sup: *mut HewSupervisor,
    index: usize,
) -> *mut HewActor {
    // SAFETY: forwards the caller's liveness contract; no identity constraint
    // is needed for construction and direct index-based administrative calls.
    unsafe { restart_child_from_spec_expected(sup, index, None) }
}

#[expect(
    clippy::too_many_lines,
    reason = "one linear restart transaction keeps snapshot, spawn, registration, lifecycle, and exact-generation publish visibly ordered"
)]
pub(crate) unsafe fn restart_child_from_spec_expected(
    sup: *mut HewSupervisor,
    mut index: usize,
    expected_identity: Option<u64>,
) -> *mut HewActor {
    // Snapshot every spec scalar and retain the immutable template generation
    // under the roster lock. The Arc is the template lifetime lease: setters
    // may publish a replacement generation and remove_child may extract/drop
    // the spec immediately after this section, but neither can reclaim the
    // bytes this restart will read. Clone/init callbacks deliberately run after
    // the lock is released so unsafe out-of-tree callbacks may re-enter a
    // supervisor API without self-deadlocking.
    let (
        spec_identity,
        spec_revision,
        template,
        child_sys_dispatch,
        opts,
        state_drop_fn,
        lifecycle_fn,
        init_fn,
        config,
        native_spawn,
    ) = {
        // SAFETY: caller guarantees `sup` is live; the guard serializes roster access.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        let child_specs = &roster.child_specs;
        if let Some(identity) = expected_identity {
            let Some(current_index) = child_specs
                .iter()
                .position(|candidate| candidate.identity == identity)
            else {
                return ptr::null_mut();
            };
            index = current_index;
        }
        let Some(spec) = child_specs.get(index) else {
            return ptr::null_mut();
        };
        let template = Arc::clone(&spec.state_template);
        let opts = HewActorOpts {
            init_state: template.allocation.state,
            state_size: template.allocation.size,
            dispatch: spec.dispatch,
            mailbox_capacity: spec.mailbox_capacity,
            overflow: spec.overflow,
            coalesce_key_fn: spec.coalesce_key_fn,
            coalesce_fallback: spec.coalesce_fallback,
            budget: 0,
            arena_cap_bytes: spec.arena_cap_bytes,
            cycle_capable: spec.cycle_capable,
            message_drop_fn: spec.message_drop_fn,
        };
        (
            spec.identity,
            spec.revision,
            template,
            spec.sys_dispatch,
            opts,
            spec.state_template.allocation.state_drop.load(),
            spec.lifecycle_fn,
            spec.init_fn,
            spec.config,
            spec.native_spawn,
        )
    };
    run_restart_spec_snapshot_hook_for_test();

    // ── Native declared child ────────────────────────────────────────────
    //
    // The adapter is the whole incarnation: it re-runs the declared init
    // arguments against the supervisor's config, runs `init()` / `#[on(start)]`
    // and publishes the actor with its own state, drop and terminate
    // callbacks. Nothing below may run for it — a second lifecycle firing or a
    // second state registration would double what the spawn already did. This
    // path only claims the slot: the supervisor back-edge that routes a crash
    // here, and the exact-generation publish.
    if let Some(spawn) = native_spawn {
        // A restart has no caller to fault: the refusal is the null slot the
        // budget already counted, so its diagnostic is released here.
        let mut refusal: *mut crate::fault::HewFault = ptr::null_mut();
        // SAFETY: the adapter is emitted with this exact ABI, `config` is the
        // supervisor-owned buffer, and `refusal` is a writable null slot.
        let token = unsafe { spawn(config.cast_const(), &raw mut refusal) };
        if !refusal.is_null() {
            // SAFETY: the adapter transferred this fault to us.
            unsafe { crate::fault::hew_fault_drop(refusal) };
        }
        let child = if token == crate::lifetime::local_handles::HewLocalPidId::INVALID {
            ptr::null_mut()
        } else {
            crate::lifetime::local_handles::resolve_current_actor(token)
                .and_then(crate::lifetime::live_actors::get_actor_ptr_by_id)
                .unwrap_or(ptr::null_mut())
        };
        if child.is_null() {
            fail_restart_snapshot(sup, index, spec_identity, spec_revision, &template);
            return ptr::null_mut();
        }
        // SAFETY: the adapter returned a live, published actor and this thread
        // is the only writer of its supervision edge.
        unsafe {
            (*child).supervisor = sup.cast::<c_void>();
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "declared child index fits i32 for any declarable supervisor"
            )]
            {
                (*child).supervisor_child_index = index as i32;
            }
        }
        if !publish_restart_snapshot(sup, index, spec_identity, spec_revision, &template, child) {
            // SAFETY: publication failed, so no supervisor slot owns this actor.
            unsafe { discard_unpublished_restart(child) };
            return ptr::null_mut();
        }
        return child;
    }
    let state_clone_fn = template.clone_fn;
    let borrows_shallow_template = init_fn.is_none()
        && state_clone_fn.is_none()
        && (template.borrows_typed_fields || (opts.state_size != 0 && !opts.init_state.is_null()));

    // ── v0.6 init-closure restart model — the leading branch ────────────
    //
    // When the spec carries an `init_fn`, the thunk PRODUCES the child's state
    // (initial spawn AND every restart) by re-running every init-arg
    // expression against the supervisor's config. This REPLACES the byte-copy
    // template / clone-fn template paths below: there is no captured template
    // to clone, so each incarnation gets fresh, unaliased owned values — the
    // structural fix for the byte-copy-template-replay aliasing hazard that
    // the retired init-arg bit-copy refusal used to wall off in the checker.
    //
    // Ownership/drop contract (the memory-safety crux):
    //  - The thunk returns a fresh, fully-owned state wrapper (`res.state`).
    //  - On thunk OOM (`res.state == null`): fail closed exactly like the
    //    clone-OOM path — apply backoff, leave the slot null, do NOT advance
    //    the circuit breaker. The crash was already counted by `record_restart`.
    //  - On success: ownership of `res.state` transfers to
    //    `hew_actor_spawn_opts_adopt` (no second deep-copy). The new actor's
    //    `state_drop_fn` (registered below) frees its owned fields on the NEXT
    //    crash/teardown. The config buffer is only READ; it is freed once at
    //    supervisor teardown.
    //  - Adopt-failure free-path: `hew_actor_spawn_opts_adopt` `buf_free`s the
    //    wrapper on failure (it cannot run `state_drop_fn`, so inner owned
    //    fields leak — OOM-only, identical to the existing clone path, tolerated
    //    because spawn-failure here implies system-wide OOM and the supervisor
    //    escalates). The restart still fails closed (null new_child below).
    //    Re-confirmed for the owned config-init thunk (the per-field deep-clone
    //    path): the thunk-produced wrapper now genuinely carries inner owned
    //    heap fields (a cloned `string`/`bytes`), so this leg leaks them on an
    //    adopt OOM — strictly OOM-only, the same bounded leak as the clone
    //    path. Do NOT add a speculative `state_drop_fn` call on this leg: the
    //    wrapper layout the drop fn expects is only guaranteed once adopt has
    //    fully initialised the actor, so dropping a half-adopted wrapper could
    //    double-free. The bounded OOM leak is the correct fail-closed posture.
    let new_child = if let Some(init_fn) = init_fn {
        // SAFETY: `init_fn` is a codegen-emitted thunk matching the
        // `HewChildInitFn` contract; `config` is either null or the
        // supervisor-owned config buffer (alive for the supervisor's lifetime).
        let res = unsafe { init_fn(config.cast_const()) };
        if res.state.is_null() {
            // Thunk OOM: fail closed (mirror the clone-OOM policy exactly).
            fail_restart_snapshot(sup, index, spec_identity, spec_revision, &template);
            return ptr::null_mut();
        }
        // Build opts around the thunk-produced state and adopt it (no second
        // copy). `init_state`/`state_size` come from the thunk result, NOT the
        // spec template (which is null/0 on the thunk path).
        let thunk_opts = HewActorOpts {
            init_state: res.state,
            state_size: res.size,
            dispatch: opts.dispatch,
            mailbox_capacity: opts.mailbox_capacity,
            overflow: opts.overflow,
            coalesce_key_fn: opts.coalesce_key_fn,
            coalesce_fallback: opts.coalesce_fallback,
            budget: 0,
            arena_cap_bytes: opts.arena_cap_bytes,
            cycle_capable: opts.cycle_capable,
            message_drop_fn: opts.message_drop_fn,
        };
        // SAFETY: thunk_opts is valid; ownership of `res.state` transfers.
        unsafe { actor::hew_actor_spawn_opts_adopt(&raw const thunk_opts, res.state) }
    } else {
        // ── Legacy template paths (no init_fn) ──────────────────────────
        //
        // Kept for the degenerate stateless/legacy case and out-of-tree C ABI
        // callers. Pick the spawn shape based on whether the actor has a
        // registered deep-clone function.
        //
        // **state_clone_fn registered**: call the codegen-emitted clone fn to
        // produce a fresh, independently-owned wrapper from the spec's template,
        // then hand ownership to `hew_actor_spawn_opts_adopt`. This bypasses the
        // legacy `deep_copy_state` byte-copy that aliased owned heap pointers
        // between `spec.init_state` and `actor.state` (audit C1 UAF).
        //
        // **Null-clone-return policy**: when `clone_fn` itself returns null
        // (OOM allocating the new wrapper), we return early **without** calling
        // `circuit_breaker_record_success`. This is critical: a successful
        // restart's clone has to land before the breaker counts the restart as
        // healed, otherwise repeated null-clones would silently close the
        // breaker and mask OOM. The outer `restart_with_budget_and_strategy`
        // already counted this attempt via `record_restart`, so max-restarts /
        // escalation still fire on persistent failure. Backoff is also applied
        // so the supervisor doesn't busy-loop retrying clone fns.
        //
        // **state_clone_fn NOT registered**: fall back to the legacy byte-copy
        // path via `hew_actor_spawn_opts`. The Q185(c) checker remains in
        // defence-in-depth so codegen-emitted actors that should have a clone
        // fn don't silently land on this path.
        if let Some(clone_fn) = state_clone_fn {
            if opts.state_size == 0 || opts.init_state.is_null() {
                // Zero-sized or null template: clone is a no-op; nothing to adopt.
                // Use the legacy path (which also produces a null state for the
                // zero-sized case).
                // SAFETY: opts is valid.
                unsafe { actor::hew_actor_spawn_opts(&raw const opts) }
            } else {
                // SAFETY: spec.init_state is a sized-block wrapper of `state_size`
                // bytes, replaced by the clone-aware template at registration
                // time. clone_fn matches the HewStateCloneFn contract.
                let cloned = unsafe { clone_fn(opts.init_state.cast_const()) };
                if cloned.is_null() {
                    // Clone OOM: apply backoff, leave slot null, do NOT advance
                    // circuit-breaker success. The crash that triggered this
                    // restart was already counted by `record_restart` at the
                    // outer level.
                    fail_restart_snapshot(sup, index, spec_identity, spec_revision, &template);
                    return ptr::null_mut();
                }
                // SAFETY: opts is valid; ownership of `cloned` is transferred.
                unsafe { actor::hew_actor_spawn_opts_adopt(&raw const opts, cloned) }
            }
        } else if state_drop_fn.is_some() && !template.borrows_typed_fields {
            // No `state_clone_fn` registered, but `state_drop_fn` IS: the
            // legacy byte-copy path below is only sound for BitCopy actor
            // state (plain-old-data fields with no owned heap pointers). A
            // registered `state_drop_fn` means the actor's state owns heap
            // fields, so byte-copying the template would alias those owned
            // pointers between the template and every spawned incarnation —
            // a double-free on teardown. Codegen-emitted actors never reach
            // this path: they carry an `init_fn` thunk that produces fresh
            // owned values per incarnation. A C-ABI caller that registers
            // `state_drop_fn` without `state_clone_fn` gets a refused restart
            // here instead of a silent alias (#1893).
            set_last_error(format!(
                "hew_supervisor_set_child_state_drop: child {index} registered a state-drop \
                 function without a matching state-clone function; restart refused rather than \
                 byte-copying owned heap state"
            ));
            fail_restart_snapshot(sup, index, spec_identity, spec_revision, &template);
            return ptr::null_mut();
        } else {
            // Either no typed drop exists (BitCopy), or the caller explicitly
            // keeps all pointees alive under the borrowed-state contract.
            // The lease pins wrapper bytes through spawn; external ownership
            // pins pointees through every incarnation's deferred reclamation.
            // Borrowed actors are marked before callbacks or publication below.
            //
            // SAFETY: opts is valid and the selected state contract admits a
            // wrapper byte copy without transferring typed-field ownership.
            unsafe { actor::hew_actor_spawn_opts(&raw const opts) }
        }
    };

    // Set supervisor back-pointer on the new child.
    if !new_child.is_null() {
        // SAFETY: new_child was just spawned and is valid.
        unsafe {
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "child index fits in i32 for any reasonable child count"
            )]
            {
                (*new_child).supervisor = sup.cast::<c_void>();
                (*new_child).supervisor_child_index = index as i32;
            }
            if borrows_shallow_template {
                // The legacy spawn copied only wrapper bytes from the
                // persistent spec. Its embedded fields alias the original
                // external owner, so this incarnation starts without typed-drop
                // authority. Fresh init/clone branches deliberately retain
                // the actor allocator's default owned provenance.
                actor::mark_state_drop_borrowed(new_child);
            }
        }

        // Register the child's SYSTEM entry point on every incarnation. It is
        // carried in the spec, not set post-hoc by the program, so a restarted
        // child receives `#[on(exit)]` / `#[on(down)]` signals exactly as its
        // predecessor did.
        // SAFETY: new_child was just spawned and is valid.
        unsafe { actor::hew_actor_set_sys_dispatch(new_child, child_sys_dispatch) };

        // Register the state-drop callback so restarted actors free their
        // heap-allocated fields (e.g. Vec, String) on teardown.
        if let Some(drop_fn) = state_drop_fn {
            // SAFETY: new_child is valid; drop_fn is a codegen-emitted
            // function with the correct signature.
            unsafe { actor::hew_actor_set_state_drop(new_child, drop_fn) };
        }

        // Register the state-clone callback on the actor itself for symmetry
        // and future direct-spawn restart consumers (the supervisor restart
        // path reads the clone fn from the spec, not the actor, but storing
        // it on the actor matches the state_drop_fn pattern).
        if let Some(clone_fn) = state_clone_fn {
            // SAFETY: new_child is valid; clone_fn is a codegen-emitted
            // function with the correct signature.
            unsafe { actor::hew_actor_set_state_clone(new_child, clone_fn) };
        }

        // Claim the lifecycle linearization point only while this exact spec
        // and template generation still occupy the slot. A remove that wins
        // first prevents the callback; a remove that starts after this check
        // linearizes after lifecycle began. The actor remains unpublished, so
        // a later identity mismatch simply tears it down below.
        if !restart_snapshot_is_current(sup, index, spec_identity, spec_revision, &template) {
            // SAFETY: the actor has not been stored in the supervisor roster.
            unsafe { discard_unpublished_restart(new_child) };
            return ptr::null_mut();
        }

        // Fire the actor's lifecycle wrapper (`init()` / `#[on(start)]`).
        //
        // THE single supervised-lifecycle firing site: both the initial
        // supervised spawn (entered here via `add_child_spec` →
        // `restart_child_from_spec(index == child_count)`) and every
        // supervisor-triggered restart (entered via
        // `restart_with_budget_and_strategy`) flow through this one call, so a
        // supervised actor runs its init/on_start exactly once per incarnation
        // — behaviourally identical to a directly-spawned actor at birth.
        //
        // Fired AFTER state_drop/clone registration and BEFORE
        // `store_child_slot`: the slot store is the visibility edge that makes
        // the child reachable via `sup.<name>`, so init/on_start complete
        // before any external code can message the child (matching the
        // direct-spawn invariant that lifecycle runs before the spawn
        // destination pointer is stored).
        //
        // The wrapper itself acquires the actor's state lock; no lock on
        // `new_child` is held here (the clone path operates on the spec
        // template, not the new actor's lock), so there is no re-entrancy.
        if let Some(lifecycle_fn) = lifecycle_fn {
            // SAFETY: new_child was just spawned and is valid; lifecycle_fn is a
            // codegen-emitted C-ABI wrapper matching the HewLifecycleFn contract.
            unsafe { lifecycle_fn(new_child) };
        }
    }

    // Publish and record circuit success only if both roster identity and the
    // immutable template generation still match after lifecycle. This closes
    // remove/swap and concurrent-setter races without holding the roster lock
    // across arbitrary lifecycle code. For initial spawns the child vec has no
    // slot yet; identity validation still succeeds and the caller pushes it.
    if !new_child.is_null()
        && !publish_restart_snapshot(
            sup,
            index,
            spec_identity,
            spec_revision,
            &template,
            new_child,
        )
    {
        // SAFETY: publication failed, so no supervisor slot owns this actor.
        unsafe { discard_unpublished_restart(new_child) };
        return ptr::null_mut();
    }
    new_child
}

/// Restart a child supervisor from its stored init fn, returning the new
/// supervisor pointer.
///
/// # Safety
///
/// `sup` must be valid and `index` must be within `child_supervisors`.
pub(crate) unsafe fn restart_child_supervisor_from_spec(
    sup: *mut HewSupervisor,
    index: usize,
) -> *mut HewSupervisor {
    let (spawn, old_child, old_token) = {
        // SAFETY: caller keeps `sup` live and the guard protects all three
        // parallel nested-supervisor vectors.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: `sup` remains live for this scoped, lock-protected read.
        let s = &*guard;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
        let Some(spec) = s.child_supervisor_specs.get(index).and_then(Option::as_ref) else {
            return ptr::null_mut();
        };
        if spec.spent {
            return ptr::null_mut();
        }
        (
            spec.spawn,
            s.child_supervisors[index],
            s.child_supervisor_tokens[index],
        )
    };

    let (new_child, _new_child_pin) = match spawn {
        // SAFETY: the constructor was registered alongside this child.
        SupervisorChildSpawn::Legacy(init) => (unsafe { init() }, None),
        SupervisorChildSpawn::Native { spawn, config } => {
            let mut fault = ptr::null_mut();
            // SAFETY: the parent owns the config throughout this restart.
            let token = unsafe { spawn(config, &raw mut fault) };
            if !fault.is_null() {
                // SAFETY: the adapter transferred the diagnostic here.
                unsafe { crate::fault::hew_fault_drop(fault) };
            }
            let pin = crate::lifetime::local_handles::pin_current_supervisor(token);
            let child = pin.as_ref().map_or(
                ptr::null_mut(),
                crate::lifetime::local_handles::SupervisorPin::supervisor,
            );
            (child, pin)
        }
    };
    if new_child.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `new_child` and `sup` are valid pointers.
    unsafe {
        (*new_child).parent = sup;
        (*new_child).index_in_parent = index;
        crate::shutdown::hew_shutdown_unregister_supervisor(new_child);
    }
    // SAFETY: `new_child` is the live allocation returned by `init_fn`.
    let new_token = unsafe { (*new_child).local_pid_id };
    {
        // Publish pointer+token as one exact replacement. If another teardown
        // already removed/replaced this entry while init ran, return the new
        // allocation to top-level cleanup instead of corrupting its roster.
        // SAFETY: caller keeps `sup` live through this publication attempt.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        if s.child_supervisors.get(index).copied() != Some(old_child)
            || s.child_supervisor_tokens.get(index).copied() != Some(old_token)
        {
            // SAFETY: publication failed, so restore top-level ownership of
            // the still-live, otherwise-unpublished allocation.
            unsafe {
                (*new_child).parent = ptr::null_mut();
                crate::shutdown::hew_shutdown_register_supervisor(new_child);
            }
            return ptr::null_mut();
        }
        s.child_supervisors[index] = new_child;
        s.child_supervisor_tokens[index] = new_token;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
    }

    // `old_token` and `new_token` are always distinct here: `local_pid_id` is
    // minted fresh per `hew_supervisor_new` and never reset or reused, so a
    // pointer comparison is the wrong identity check — an allocator (observed
    // on Windows and macOS) can hand the freed `old_child` block straight
    // back out to `new_child`, making `old_child == new_child` true for two
    // genuinely different incarnations. The only real question here is
    // whether there was a previous child to retire at all.
    if !old_child.is_null() {
        retain_nested_completion(sup, old_token);
        stop_local_supervisor(old_token, true);
    }

    new_child
}
