//! Restart-child role dispatch and nested-supervisor stop handling.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

/// One stable child identity in the parent's declaration order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RestartChildRole {
    Actor(u64),
    Supervisor { index: usize, identity: u64 },
}

impl RestartChildRole {
    fn identity(self) -> u64 {
        match self {
            Self::Actor(identity) | Self::Supervisor { identity, .. } => identity,
        }
    }
}

/// The roles one restart strategy restarts after `failed`, in declaration
/// order; empty when `failed` itself is no longer restartable.
///
/// # Safety
/// `sup` remains live throughout the roster snapshot.
unsafe fn restart_roles_for_strategy(
    sup: *mut HewSupervisor,
    strategy: c_int,
    failed: RestartChildRole,
) -> Vec<RestartChildRole> {
    let roles = {
        // SAFETY: the caller keeps the parent alive through the roster snapshot.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        // A spent spec is a role this supervisor already ruled against
        // restarting. A group restart triggered by a SIBLING must not resurrect
        // it: the same `!spent` filter the nested specs carry applies to actor
        // specs, or a `temporary` child declined moments ago comes back under
        // `one_for_all` / `rest_for_one`.
        let mut roles: Vec<_> =
            roster
                .child_specs
                .iter()
                .filter(|spec| !spec.spent)
                .map(|spec| RestartChildRole::Actor(spec.identity))
                .chain(roster.child_supervisor_specs.iter().enumerate().filter_map(
                    |(index, spec)| {
                        spec.as_ref().filter(|spec| !spec.spent).map(|spec| {
                            RestartChildRole::Supervisor {
                                index,
                                identity: spec.identity,
                            }
                        })
                    },
                ))
                .collect();
        roles.sort_unstable_by_key(|role| role.identity());
        roles
            .into_iter()
            .filter(|role| match strategy {
                STRATEGY_ONE_FOR_ONE | STRATEGY_SIMPLE_ONE_FOR_ONE => *role == failed,
                STRATEGY_ONE_FOR_ALL => true,
                STRATEGY_REST_FOR_ONE => role.identity() >= failed.identity(),
                _ => unreachable!("unknown supervisor restart strategy"),
            })
            .collect::<Vec<_>>()
    };
    if roles.contains(&failed) {
        roles
    } else {
        Vec::new()
    }
}

/// Apply one restart strategy across both child kinds in declaration order:
/// stop every role the strategy restarts, then restart the group. A stopped
/// incarnation finishes its stop hooks and cleanup on its own turn, and those
/// may suspend, so a group with an incarnation still stopping waits in the
/// roster and resumes through [`HewSysMsg::GroupRestart`]. A failure while a
/// group waits joins it, and the joined group rules on every record at once.
///
/// # Safety
/// `sup` remains live throughout the restart and its callbacks.
unsafe fn restart_children_for_strategy(
    sup: *mut HewSupervisor,
    strategy: c_int,
    failed: RestartChildRole,
    record: FaultRecord,
) -> FaultRuling {
    // SAFETY: the caller keeps `sup` live through this snapshot.
    if unsafe { restart_roles_for_strategy(sup, strategy, failed) }.is_empty() {
        // SAFETY: no roster reference crosses cancellation/escalation.
        return unsafe { group_restart_ruling(sup, false, record) };
    }
    // SAFETY: the caller keeps `sup` live; the guard ends with this statement.
    let pending = unsafe { &(*sup).roster }
        .lock_or_recover()
        .pending_group_restart
        .take();
    let mut group = pending.unwrap_or_else(|| PendingGroupRestart::new(sup));
    group.failures.push((failed, record));
    // SAFETY: the caller keeps `sup` live through the stops and spawns.
    let Some((restarted, mut failures)) = (unsafe { advance_group_restart(sup, strategy, group) })
    else {
        return FaultRuling::ArmedForRestart;
    };
    // This failure's record is the last; the caller settles it with the ruling.
    failures.pop();
    let joined = if restarted {
        FaultRuling::Handled
    } else {
        FaultRuling::Unrecovered
    };
    for (_, joined_record) in failures {
        crate::exit_status::settle_supervised_fault(joined_record, joined);
    }
    // SAFETY: no roster reference crosses cancellation/escalation.
    unsafe { group_restart_ruling(sup, restarted, record) }
}

/// Stop every occupied role the pending group restarts, then restart the group
/// once nothing it stopped is still finishing. Returns whether the group came
/// back together with the failures it answered, or `None` after returning the
/// group to the roster to wait.
///
/// # Safety
/// `sup` remains live throughout the stops and the spawns.
unsafe fn advance_group_restart(
    sup: *mut HewSupervisor,
    strategy: c_int,
    mut group: PendingGroupRestart,
) -> Option<(bool, Vec<(RestartChildRole, FaultRecord)>)> {
    let mut roles = Vec::new();
    let mut restartable = true;
    for &(failed, _) in &group.failures {
        // SAFETY: the caller keeps `sup` live through each snapshot.
        let failed_roles = unsafe { restart_roles_for_strategy(sup, strategy, failed) };
        restartable &= !failed_roles.is_empty();
        for role in failed_roles {
            if !roles.contains(&role) {
                roles.push(role);
            }
        }
    }
    if !restartable {
        // A failed role was spent or retired while the group waited.
        let failures = std::mem::take(&mut group.failures);
        group.abandon();
        return Some((false, failures));
    }
    roles.sort_unstable_by_key(|role| role.identity());
    let mut deferred = Vec::new();
    for &role in &roles {
        // SAFETY: the caller keeps `sup` live through each stop.
        unsafe { stop_group_role(sup, role, &mut group, &mut deferred) };
    }
    spawn_deferred_restart_free(deferred);
    group.release_finished();
    if group.stopping.is_empty() {
        // SAFETY: the caller keeps `sup` live through the spawns.
        let restarted = unsafe { restart_group(sup, &roles) };
        return Some((restarted, group.failures));
    }
    // Each completion still stopping was registered before its stop, so its
    // finish signals this supervisor after the group is back in the roster.
    // SAFETY: the caller keeps `sup` live for this roster update.
    unsafe { &(*sup).roster }
        .lock_or_recover()
        .pending_group_restart = Some(group);
    None
}

/// Stop the incarnation occupying one role of a group restart and hand it to
/// the group, so no replacement starts beside it.
///
/// # Safety
/// `sup` remains live throughout the stop.
unsafe fn stop_group_role(
    sup: *mut HewSupervisor,
    role: RestartChildRole,
    group: &mut PendingGroupRestart,
    deferred: &mut Vec<DeferredFree>,
) {
    match role {
        RestartChildRole::Actor(identity) => {
            let child = take_child_slot_by_identity(sup, identity);
            if child.is_null() {
                return;
            }
            // SAFETY: the extracted slot transfers this live incarnation here.
            match unsafe { (*child).native_completion.clone() } {
                Some(completion) => {
                    retain_child_completion(sup, Arc::clone(&completion));
                    completion.wake_on_finish(&group.waker);
                    group.stopping.push(StoppedIncarnation {
                        actor: child,
                        completion,
                    });
                }
                // Nothing reports when this actor finishes; free it off the
                // supervisor's turn.
                None => deferred.push(DeferredFree(child)),
            }
            // SAFETY: the extracted slot transferred this live incarnation.
            unsafe { actor::hew_actor_stop(child) };
        }
        RestartChildRole::Supervisor { index, identity } => {
            // The roster's nested pointer is a cached address, not a lifetime:
            // the stable token is. Detach the pair in one critical section.
            let entry = {
                // SAFETY: the caller keeps the parent alive for this update.
                let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
                let roster = &mut *guard;
                let current = roster
                    .child_supervisor_specs
                    .get(index)
                    .and_then(Option::as_ref)
                    .is_some_and(|spec| spec.identity == identity);
                match roster.child_supervisors.get(index).copied() {
                    Some(child) if current && !child.is_null() => {
                        let token = roster.child_supervisor_tokens[index];
                        roster.child_supervisors[index] = ptr::null_mut();
                        roster.child_supervisor_tokens[index] =
                            crate::lifetime::local_handles::HewLocalPidId::INVALID;
                        Some((child, token))
                    }
                    _ => None,
                }
            };
            let Some((child, token)) = entry else {
                return;
            };
            if let Some(completion) =
                crate::lifetime::local_handles::current_supervisor_completion(token)
            {
                retain_child_completion(sup, Arc::clone(&completion));
                completion.wake_on_finish(&group.waker);
                group.stopping.push(StoppedIncarnation {
                    actor: ptr::null_mut(),
                    completion,
                });
            }
            stop_detached_nested_supervisor(child, token);
        }
    }
}

/// Stop a nested supervisor that a group restart detached from its parent's
/// roster. The claim makes it an independent root, so a teardown that has to
/// hand it back still reaches canonical cleanup; the stop runs off the
/// parent's turn.
fn stop_detached_nested_supervisor(
    child: *mut HewSupervisor,
    token: crate::lifetime::local_handles::HewLocalPidId,
) {
    // Admission closes only once runtime cleanup has joined every worker, so
    // a supervisor turn that reaches here is always admitted.
    let Some(teardown) = crate::lifetime::local_handles::begin_current_supervisor_teardown() else {
        return;
    };
    // SAFETY: pointer and token are one entry detached under the roster lock.
    if unsafe { claim_nested_supervisor_for_detach(child, token) } {
        // The claim makes this path the unique owner, so the allocation stays
        // live until the deferred stop reclaims it.
        request_supervisor_shutdown(child);
        // SAFETY: the claim above is the unique teardown authority.
        unsafe { finish_claimed_supervisor(child, false, teardown, true) };
    }
}

/// Spawn every role of a group restart; whether all of them came back.
///
/// # Safety
/// `sup` remains live throughout the spawns.
unsafe fn restart_group(sup: *mut HewSupervisor, roles: &[RestartChildRole]) -> bool {
    let mut complete = true;
    for role in roles {
        complete &= match *role {
            RestartChildRole::Actor(identity) => {
                // SAFETY: the stable identity refuses any concurrent retirement.
                !unsafe { restart_child_from_spec_expected(sup, 0, Some(identity)) }.is_null()
            }
            RestartChildRole::Supervisor { index, .. } => {
                // SAFETY: the parent dispatch owns the nested roster throughout restart.
                !unsafe { restart_child_supervisor_from_spec(sup, index) }.is_null()
            }
        };
    }
    complete
}

/// The ruling a group restart reaches once its spawns ran: a restart that left
/// the failed role empty stops this supervisor and escalates.
///
/// # Safety
/// `sup` is live; no roster reference is held.
unsafe fn group_restart_ruling(
    sup: *mut HewSupervisor,
    restarted: bool,
    record: FaultRecord,
) -> FaultRuling {
    if !restarted {
        return stop_and_maybe_escalate(sup, record);
    }
    notify_restart(sup);
    FaultRuling::Handled
}

/// Restart children after checking the supervisor restart budget, and report
/// whether the failed child was actually recovered.
///
/// `restart_child_from_spec_expected` is nullable: it refuses when the spec was
/// retired, when the exact template generation it leased no longer publishes,
/// or when the spawn itself fails. A null there means the crashed child is
/// gone and nothing replaced it — the recovery this supervisor attempted did
/// not happen — so it takes the same `stop_and_maybe_escalate` route as the
/// nested-supervisor path's null restart, and reports `Unrecovered`. Ignoring
/// the result (and notifying restart waiters unconditionally) reported a
/// recovery that never occurred.
///
/// # Safety
///
/// `sup` must be valid.
pub(crate) unsafe fn restart_with_budget_and_strategy(
    sup: *mut HewSupervisor,
    failed_identity: u64,
    record: FaultRecord,
) -> FaultRuling {
    // SAFETY: caller keeps the allocation live; these scalar policy fields are
    // immutable after construction.
    let (strategy, max_restarts, window_secs) =
        unsafe { ((*sup).strategy, (*sup).max_restarts, (*sup).window_secs) };
    let sup_actor_id = supervisor_actor_id(sup);
    let (strategy, identities, failed_index, recent, max_restarts, sup_actor_id) = {
        // SAFETY: caller keeps `sup` live; budget bookkeeping and roster
        // snapshot are serialized with dynamic roster mutation.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;
        let Some(failed_index) = s
            .child_specs
            .iter()
            .position(|spec| spec.identity == failed_identity)
        else {
            // Dynamic removal retired the spec: no entry left to restart.
            return FaultRuling::Unrecovered;
        };
        let recent = restart_within_window(window_secs, s);
        if recent < max_restarts {
            record_restart(s);
        }
        (
            strategy,
            s.child_specs
                .iter()
                .map(|spec| spec.identity)
                .collect::<Vec<_>>(),
            failed_index,
            recent,
            max_restarts,
            sup_actor_id,
        )
    };

    if recent >= max_restarts {
        // Observability (AFTER the max-restart-intensity decision, BEFORE the
        // escalate): record that the budget was exhausted, carrying the
        // within-window restart count.
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_MAX_RESTARTS,
            recent,
        );
        // SAFETY: caller keeps `sup` live; no roster reference crosses this
        // cancellation/escalation operation.
        return stop_and_maybe_escalate(sup, record);
    }

    crate::observe::record_actor_restart();
    // Observability (AFTER the restart decision is taken): record the restart,
    // carrying the restart strategy in the discriminator. Read-only side
    // effect; never gates control flow.
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_RESTART,
        strategy,
    );

    // SAFETY: `failed_index` was resolved under the roster lock above and
    // `identities` is that same snapshot. A restart that produced no child
    // takes the nested supervisor path's route: stop this supervisor and
    // escalate, so restart waiters never see a failure as a recovery.
    unsafe {
        restart_children_for_strategy(
            sup,
            strategy,
            RestartChildRole::Actor(identities[failed_index]),
            record,
        )
    }
}

/// Restart an exhausted child supervisor subtree after checking the parent's
/// restart budget. Child-supervisor recovery is only available when the child
/// was registered with an init fn.
///
/// This is the PARENT'S RULING on a record its child escalated. A subtree this
/// parent successfully restarts CLEARS that record: nested budget exhaustion
/// followed by a recovery here is not a terminal fault, because the tree did
/// what a tree is for. Failing here escalates the same record further up, or
/// settles it when this is the root.
///
/// # Safety
///
/// `sup` must be valid and `failed_index` must be within `child_supervisors`.
pub(crate) unsafe fn restart_child_supervisor_with_budget(
    sup: *mut HewSupervisor,
    failed_index: usize,
    record: FaultRecord,
) -> FaultRuling {
    // SAFETY: these policy scalars are immutable after construction.
    let (max_restarts, window_secs, strategy) =
        unsafe { ((*sup).max_restarts, (*sup).window_secs, (*sup).strategy) };
    let sup_actor_id = supervisor_actor_id(sup);
    let (restartable, recent, max_restarts, sup_actor_id, strategy) = {
        // SAFETY: caller keeps `sup` live; nested-roster validation and budget
        // bookkeeping are one synchronized snapshot.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable supervisor access.
        let s = &mut *guard;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_specs.len());
        if s.child_supervisor_specs
            .get(failed_index)
            .and_then(Option::as_ref)
            .is_some_and(|spec| spec.restart_policy == RESTART_TEMPORARY)
        {
            return FaultRuling::Unrecovered;
        }
        let restartable = s
            .child_supervisor_specs
            .get(failed_index)
            .and_then(Option::as_ref)
            .is_some();
        let recent = if restartable {
            restart_within_window(window_secs, s)
        } else {
            0
        };
        if restartable && recent < max_restarts {
            record_restart(s);
        }
        (restartable, recent, max_restarts, sup_actor_id, strategy)
    };

    if !restartable {
        // SAFETY: no roster reference crosses cancellation/escalation.
        return stop_and_maybe_escalate(sup, record);
    }

    if recent >= max_restarts {
        // Observability (AFTER the max-restart-intensity decision): record
        // budget exhaustion on the child-supervisor recovery path too.
        crate::tracing::record_supervisor_event(
            sup_actor_id,
            crate::tracing::SPAN_SUPERVISOR_MAX_RESTARTS,
            recent,
        );
        // SAFETY: no roster reference crosses cancellation/escalation.
        return stop_and_maybe_escalate(sup, record);
    }

    crate::observe::record_actor_restart();
    // Observability (AFTER the restart decision): record the child-supervisor
    // subtree restart, carrying the strategy discriminator.
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_RESTART,
        strategy,
    );

    // SAFETY: `failed_index` is validated above and `sup` is the live parent
    // supervisor whose child-supervisor slot we are replacing.
    let failed = {
        // SAFETY: this dispatch retains the parent and the roster protects its spec.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        let Some(spec) = roster
            .child_supervisor_specs
            .get(failed_index)
            .and_then(Option::as_ref)
        else {
            drop(roster);
            return stop_and_maybe_escalate(sup, record);
        };
        RestartChildRole::Supervisor {
            index: failed_index,
            identity: spec.identity,
        }
    };
    // SAFETY: the parent dispatch retains the live supervisor across this
    // strategy. A restored subtree recovers the escalated record here.
    unsafe { restart_children_for_strategy(sup, strategy, failed, record) }
}

/// Invoke a child's `#[on(crash)]` handler (if installed) and return its
/// `CrashAction` decision as an i32 tag, or `None` when no handler is installed.
///
/// The handler receives the crash code (widened to i64), a trap-kind diagnostic
/// message (e.g. "HeapExceeded"/"Signal"), and the child's template seed-state
/// pointer.
///
/// String-ABI contract (M-5): `crash_message` is typed `string` on the Hew side
/// (`CrashInfo.message`), so it MUST be a Hew header-aware allocation. The codegen
/// prologue CLONES it (`hew_string_clone`, a refcount bump) into the owned
/// `CrashInfo.message` field; both `hew_string_clone` and the hook's `CrashInfo`
/// drop (`hew_string_drop`) read the 16-byte header at `data -
/// CSTRING_HEADER_SIZE`. A bare Rust `CString` carries no header, so those
/// primitives OOB-read and `abort()` — the reported M-5 critical bug. We therefore
/// allocate the message through the Hew string allocator (`str_to_malloc`, rc==1)
/// and the supervisor REMAINS the owner of that original: it frees it via
/// `free_cstring` after the call. The hook's clone is an independent `+1` owner
/// released by the hook's own `CrashInfo` drop, so the two releases balance to a
/// single free with no double-free — eliminating the abort/heap-corruption the
/// pre-fix headerless-`CString` + move-of-borrow produced on every real crash.
///
/// # Safety
///
/// `handler` (when `Some`) must be a valid `HewOnCrashFn` fn-pointer; `ctx` must
/// be the live execution context for the in-flight supervisor dispatch;
/// `state_ptr` must be the child's supervisor-owned template state.
unsafe fn invoke_on_crash_handler(
    handler: Option<HewOnCrashFn>,
    state_ptr: *mut c_void,
    crash_code: c_int,
    ctx: *mut crate::execution_context::HewExecutionContext,
) -> Option<i32> {
    let handler = handler?;
    // Allocate the trap-kind message as a Hew header-aware string (rc == 1) so
    // the handler's `hew_string_clone` ingress and `CrashInfo` drop operate on a
    // valid refcount header. `trap_kind_name` is a non-empty `&'static str`, so
    // `str_to_malloc` only returns null on allocation failure; pass null through
    // (the codegen clone/drop are null-safe).
    let crash_message: *mut c_char =
        crate::cabi::str_to_malloc(ExitReason::from_error_code(crash_code).trap_kind_name());
    // Widen crash_code from c_int to i64 at the call boundary. `HewOnCrashFn`
    // uses i64 to match `CrashInfo.code: i64` in std/failure.hew.
    #[allow(
        clippy::cast_lossless,
        reason = "c_int to i64: intentional widening to match HewOnCrashFn ABI"
    )]
    let crash_code_i64 = crash_code as i64;
    // SAFETY: `handler` is a valid `HewOnCrashFn`; `ctx` is the live execution
    // context; `state_ptr` is the child's supervisor-owned template state;
    // `crash_message` is a Hew header-aware allocation (or null), owned by this
    // frame and live across the call. The hook clones it into its own owner.
    let action = unsafe { handler(ctx, crash_code_i64, crash_message, state_ptr) };
    let tag = action.tag_i32();
    // Release the supervisor's original owner of the header-aware message. The
    // hook cloned (retained) it into `CrashInfo.message` and released that owner
    // on return, so this brings the refcount to zero and frees the buffer exactly
    // once. `free_cstring` is null-safe.
    if !crash_message.is_null() {
        // SAFETY: `crash_message` came from `str_to_malloc` (header-aware) and is
        // not null; the only other owner (the hook's clone) was already released.
        unsafe { crate::cabi::free_cstring(crash_message) };
    }
    Some(tag)
}

/// Apply the restart strategy after a child failure, then settle that
/// failure's process exit-status record.
///
/// This is the ONE place a supervised crash's disposition becomes final.
/// [`decide_child_failure`] holds the whole decision — every give-up path
/// returns [`FaultRuling::Unrecovered`] through it rather than
/// returning early past the accounting — so "did a supervisor handle this
/// fault?" is answered structurally, at a single point, instead of at each of
/// the eight places the old body could `return`.
///
/// The settle is paired with the `open_supervised_fault` the crash site
/// performed before notifying this supervisor, so it runs exactly when
/// `crashed` is true.
///
/// # Safety
///
/// `sup` must be valid.
unsafe fn apply_restart(
    sup: *mut HewSupervisor,
    failed_identity: u64,
    exit_state: c_int,
    crash_code: c_int,
    ctx: *mut crate::execution_context::HewExecutionContext,
    record: FaultRecord,
    native_action: Option<i32>,
) {
    // SAFETY: forwarded unchanged to the decision funnel.
    let ruling = unsafe {
        decide_child_failure(
            sup,
            failed_identity,
            exit_state,
            crash_code,
            ctx,
            record,
            native_action,
        )
    };
    if exit_state == HewActorState::Crashed as c_int {
        // ORDER: the ruling above has already published the slot state — a
        // restarted child, or a spent spec that classifies Dead. Settling
        // clears the role attribution, and only then are waiters released, so a
        // barrier can never be woken into a role whose ruling is still landing.
        crate::exit_status::settle_supervised_fault(record, ruling);
        wake_restart_waiters(sup);
    }
}

/// Arm a backoff restart for one child and rule on the attempt.
///
/// Arming TRANSFERS `record` to the timer: the restart's effect when it fires
/// is the ruling, so a successful arm reports [`FaultRuling::ArmedForRestart`].
/// A REFUSED arm is terminal. No timer exists to fire, so nothing will ever
/// refill the slot; left unspent it classifies `Transient(Restarting)` — a
/// restart that is not coming — which every caller reads as "wait". Retiring
/// the role is the fail-closed answer.
///
/// # Safety
///
/// `sup` must be valid and stay live across the arm.
unsafe fn arm_backoff_restart(
    sup: *mut HewSupervisor,
    spec_identity: u64,
    delay_ms: u64,
    record: FaultRecord,
    sup_actor_id: u64,
) -> FaultRuling {
    crate::tracing::record_supervisor_event(
        sup_actor_id,
        crate::tracing::SPAN_SUPERVISOR_BACKOFF,
        i32::try_from(delay_ms).unwrap_or(i32::MAX),
    );
    // Scheduling retains its own timer lease and carries the stable child
    // identity, never an index.
    if schedule_delayed_restart(
        sup,
        spec_identity,
        std::time::Duration::from_millis(delay_ms),
        record,
    ) {
        FaultRuling::ArmedForRestart
    } else {
        mark_child_spec_spent(sup, spec_identity);
        FaultRuling::Unrecovered
    }
}

/// Decide whether this supervisor recovers a child failure.
///
/// Returns [`FaultRuling::Handled`] only when this supervisor owns
/// the outcome — it restarted the child, scheduled a restart, or escalated to a
/// parent that now owns the decision. Every other exit is `Unrecovered`,
/// including the ones that look like ordinary policy: a `temporary` child, a
/// tripped circuit breaker, a hook answering `Kill`, and `Escalate` at a root
/// with no parent are all "the supervisor declined to recover this crash", and
/// declining is not recovering.
///
/// A non-crash exit (a graceful stop) is not a fault at all; the caller does not
/// settle anything for it, so this function's return value is ignored there.
///
/// # Safety
///
/// `sup` must be valid.
unsafe fn decide_child_failure(
    sup: *mut HewSupervisor,
    failed_identity: u64,
    exit_state: c_int,
    crash_code: c_int,
    ctx: *mut crate::execution_context::HewExecutionContext,
    record: FaultRecord,
    native_action: Option<i32>,
) -> FaultRuling {
    let crashed = exit_state == HewActorState::Crashed as c_int;
    let (spec_identity, template, on_crash, sup_actor_id) = {
        // SAFETY: caller keeps `sup` live; crash accounting and callback
        // snapshot are serialized with setters and dynamic removal.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        let sup_actor_id = supervisor_actor_id(sup);
        let Some(spec) = s
            .child_specs
            .iter_mut()
            .find(|candidate| candidate.identity == failed_identity)
        else {
            // The spec was retired (dynamic removal) before this event was
            // dispatched: no roster entry remains to restart, so nothing will
            // recover the crash.
            return FaultRuling::Unrecovered;
        };
        if crashed {
            circuit_breaker_record_crash(spec, crash_code, sup_actor_id);
        }
        (
            spec.identity,
            Arc::clone(&spec.state_template),
            spec.on_crash,
            sup_actor_id,
        )
    };

    // The arbitrary on-crash callback runs without `roster`. The Arc
    // lease keeps the exact template generation alive across a concurrent
    // clone setter or remove_child.
    let crash_action_tag = if native_action.is_some() {
        native_action
    } else if crashed {
        // SAFETY: `ctx` is the live supervisor dispatch context and `template`
        // leases the state allocation for the complete synchronous callback.
        unsafe { invoke_on_crash_handler(on_crash, template.allocation.state, crash_code, ctx) }
    } else {
        None
    };

    match crash_action_tag {
        // The hook chose termination over recovery. Deliberate, but not a
        // recovery: nothing restarts this child, so the slot is spent.
        Some(CRASH_ACTION_KILL) => {
            mark_child_spec_spent(sup, spec_identity);
            return FaultRuling::Unrecovered;
        }
        Some(CRASH_ACTION_ESCALATE) => {
            // SAFETY: the caller keeps `sup` live; only non-roster parent state
            // is inspected after the callback lease has been released.
            if unsafe { (*sup).parent.is_null() } {
                // Escalating past the root has nowhere to go: the fault reached
                // the top of the supervision tree with no authority left.
                mark_child_spec_spent(sup, spec_identity);
                return FaultRuling::Unrecovered;
            }
            // SAFETY: no roster reference crosses escalation. The record is
            // TRANSFERRED only if a live parent accepts it; a refused send left
            // it with no new authority. An ACCEPTED escalation may still see the
            // parent restart this subtree, so only a refusal spends the slot.
            return if escalate_to_parent(sup, record) {
                FaultRuling::Escalated
            } else {
                mark_child_spec_spent(sup, spec_identity);
                FaultRuling::Unrecovered
            };
        }
        _ => {}
    }

    let delay_ms = {
        // SAFETY: caller keeps `sup` live; find the stable identity again so a
        // concurrent swap-remove cannot apply policy to a sibling.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard serializes this scoped mutable roster access.
        let s = &mut *guard;
        let Some(spec) = s
            .child_specs
            .iter_mut()
            .find(|candidate| candidate.identity == spec_identity)
        else {
            // Retired between the callback and the policy read; same reasoning
            // as the first lookup above.
            return FaultRuling::Unrecovered;
        };

        if crashed && spec.restart_delay_ms > 0 {
            apply_restart_backoff(spec);
        }
        if spec.restart_policy == RESTART_TEMPORARY
            || (spec.restart_policy == RESTART_TRANSIENT
                && exit_state == HewActorState::Stopped as c_int)
            || !circuit_breaker_should_restart(spec, sup_actor_id)
        {
            // Policy declines the restart: a `temporary` child, or a circuit
            // breaker that has tripped. When the child STOPPED gracefully this
            // is not a fault and the caller settles nothing; when it CRASHED,
            // the policy chose to leave the crash unrecovered. Either way the
            // slot never refills, so spend it once the guard is released.
            drop(guard);
            mark_child_spec_spent(sup, spec_identity);
            return FaultRuling::Unrecovered;
        }
        if restart_delay_allows_restart(spec) {
            if crashed && spec.restart_delay_ms == 0 {
                spec.restart_delay_ms = INITIAL_RESTART_DELAY_MS;
            }
            None
        } else {
            let remaining = spec
                .next_restart_time_ns
                .saturating_sub(monotonic_time_ns());
            Some((remaining / 1_000_000).max(1))
        }
    };

    if let Some(delay_ms) = delay_ms {
        // SAFETY: the caller keeps `sup` live across the arm and its ruling.
        return unsafe { arm_backoff_restart(sup, spec_identity, delay_ms, record, sup_actor_id) };
    }

    // SAFETY: budget/strategy resolves the stable identity under the roster
    // lock and refuses if dynamic removal retired it.
    unsafe { restart_with_budget_and_strategy(sup, spec_identity, record) }
}

/// The supervisor's [`HewSysDispatchFn`] — its SYSTEM entry point.
///
/// Registered as `HewActor.sys_dispatch`, never as `dispatch`, so it is
/// reachable ONLY from nodes dequeued with `Origin::Sys`. A `hew_actor_send`
/// to the supervisor's actor handle lands on the user queue and can never
/// arrive here, which is what makes a forged supervision event — the
/// `take_child_slot` + `hew_actor_free` of a LIVE child — unrepresentable
/// rather than merely gated.
///
/// The dispatch logic lives in `supervisor_sys_dispatch_impl` (which keeps the
/// early-`return` control flow).
pub(crate) unsafe extern "C-unwind" fn supervisor_sys_dispatch(
    ctx: *mut crate::execution_context::HewExecutionContext,
    state: *mut c_void,
    sys_msg: i32,
    data: *mut c_void,
    data_size: usize,
) -> *mut c_void {
    // SAFETY: forwards the caller's invariants unchanged to the impl.
    unsafe { supervisor_sys_dispatch_impl(ctx, state, sys_msg, data, data_size) };
    // Supervision events run to completion.
    ptr::null_mut()
}

/// Handle one `ChildStopped` / `ChildCrashed` system event.
///
/// Split out of `supervisor_sys_dispatch_impl` so the dispatch reads as a
/// routing table and this arm — which owns the crash record's routing to a
/// ruling — reads as one decision.
///
/// # Safety
///
/// `sup` must be the live supervisor backing this dispatch, and `data` must
/// point to at least `data_size` readable bytes.
pub(crate) unsafe fn dispatch_child_lifecycle_event(
    sup: *mut HewSupervisor,
    ctx: *mut crate::execution_context::HewExecutionContext,
    data: *mut c_void,
    data_size: usize,
) {
    if data.is_null() || data_size < std::mem::size_of::<ChildEvent>() {
        return;
    }
    // SAFETY: data is valid for at least sizeof(ChildEvent).
    let event = unsafe { &*data.cast::<ChildEvent>() };

    // TRACE-CONTEXT COMPLETENESS (S3, crash-recovery seam): a crash is
    // reported from `hew_actor_trap` (signal-handler context), so the
    // sys-message that woke this dispatch may carry an all-zero trace
    // context. Establish a sampled root HERE — in normal
    // supervisor-dispatch context, never in the trap — so the restart /
    // escalate / circuit spans emitted below (S2) parent under a real,
    // sampled trace id instead of an unsampled zero-parent fallback.
    crate::tracing::ensure_supervisor_trace_root();

    let idx = event.child_index as usize;
    let Some((child, spec_identity)) = take_child_slot_for_event(sup, idx, event.child_id) else {
        // The slot no longer names this child (retired, or already
        // replaced): no ruling can be made, so the record it carried
        // reaches no authority.
        if event.exit_state == HewActorState::Crashed as c_int {
            crate::exit_status::settle_supervised_fault(
                FaultRecord::from_raw(event.fault_record),
                FaultRuling::Unrecovered,
            );
        }
        return;
    };

    // Retain the native hook decision before freeing its incarnation.
    let native_action = if child.is_null() {
        None
    } else {
        // SAFETY: the retired slot still pins this quiescent actor.
        unsafe { &*child }
            .native_completion
            .as_ref()
            .and_then(|c| c.crash_action())
    };

    // Free the old child.
    if !child.is_null() {
        // Explicit provenance plus the retiring incarnation's atomic
        // `state_drop_consumed` bit are the typed-drop authority.
        // Crash escrow sets consumed only after actually consuming
        // state; pre-dispatch crashes, normal stops, init-thunk state
        // and clone-produced state retain their final-drop authority,
        // while shallow-template borrowers never acquire it.
        // SAFETY: child is quiescent and no longer referenced by its
        // supervisor slot.
        unsafe { actor::hew_actor_free(child) };
    }

    // SAFETY: sup is valid; ctx is the supervisor's own dispatch
    // context, threaded through so a registered on_crash handler
    // receives the supervisor's ctx (preserves task-scope
    // cancellation propagation per f4df6354).
    unsafe {
        apply_restart(
            sup,
            spec_identity,
            event.exit_state,
            event.crash_code,
            ctx,
            FaultRecord::from_raw(event.fault_record),
            native_action,
        );
    };
}

/// Resume the pending group restart once every incarnation it stopped has
/// finished, and rule on every record it answers.
///
/// # Safety
///
/// `sup` must be the live supervisor backing this dispatch.
pub(crate) unsafe fn dispatch_group_restart(sup: *mut HewSupervisor) {
    let group = {
        // SAFETY: the dispatch keeps `sup` live; the guard ends in this block.
        let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
        if !roster
            .pending_group_restart
            .as_ref()
            .is_some_and(PendingGroupRestart::finished)
        {
            return;
        }
        roster.pending_group_restart.take()
    };
    let Some(group) = group else {
        return;
    };
    crate::tracing::ensure_supervisor_trace_root();
    // SAFETY: the dispatch keeps `sup` live through the stops and spawns; the
    // strategy is immutable after construction.
    let Some((restarted, failures)) =
        (unsafe { advance_group_restart(sup, (*sup).strategy, group) })
    else {
        return;
    };
    let mut failures = failures.into_iter();
    if let Some((_, record)) = failures.next() {
        // SAFETY: no roster reference crosses cancellation/escalation.
        let ruling = unsafe { group_restart_ruling(sup, restarted, record) };
        crate::exit_status::settle_supervised_fault(record, ruling);
    }
    let joined = if restarted {
        FaultRuling::Handled
    } else {
        FaultRuling::Unrecovered
    };
    for (_, record) in failures {
        crate::exit_status::settle_supervised_fault(record, joined);
    }
    wake_restart_waiters(sup);
}

/// Retire only the incarnation named by a normal-stop notification.
pub(crate) unsafe fn dispatch_child_supervisor_stopped(
    sup: *mut HewSupervisor,
    event: &ChildSupervisorStopped,
) {
    let index = event.supervisor_index as usize;
    let restart = {
        // SAFETY: dispatch retains the parent throughout this roster transition.
        let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
        if roster.child_supervisor_tokens.get(index).copied() != Some(event.child_token) {
            return;
        }
        let Some(spec) = roster
            .child_supervisor_specs
            .get_mut(index)
            .and_then(Option::as_mut)
        else {
            return;
        };
        let restart = spec.restart_policy == RESTART_PERMANENT;
        spec.spent = !restart;
        roster.child_supervisors[index] = ptr::null_mut();
        roster.child_supervisor_tokens[index] =
            crate::lifetime::local_handles::HewLocalPidId::INVALID;
        restart
    };
    retain_nested_completion(sup, event.child_token);
    if restart {
        // SAFETY: the notification named this parent's retained declaration.
        unsafe { restart_child_supervisor_with_budget(sup, index, FaultRecord::NONE) };
    } else {
        notify_restart(sup);
    }
}
