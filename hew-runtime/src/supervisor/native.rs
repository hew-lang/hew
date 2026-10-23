//! Resumable declared-child invocation and private initial construction.
#[allow(
    clippy::wildcard_imports,
    reason = "supervision shares one private contract"
)]
use super::*;
use crate::coro_state::{self, CoroStatus, HewCoroState};
use crate::fault::HewFault;
use crate::lifetime::local_handles::{self, HewLocalPidId};
use crate::wake::{HewWaker, OwnedWaker};

/// Restore the previous thread carrier even when a fixture callback unwinds.
struct RestoreExecutionContext(*mut crate::execution_context::HewExecutionContext);

impl Drop for RestoreExecutionContext {
    fn drop(&mut self) {
        let _ = crate::execution_context::set_current_context(self.0);
    }
}

/// Stable storage owned by the initial operation or pending restart transaction.
/// No callback or waker owns a pointer into this allocation.
#[derive(Debug)]
pub(super) struct NativeInvocation {
    callback: HewNativeChildSpawnFn,
    owner: HewLocalPidId,
    config: *const c_void,
    result: HewLocalPidId,
    fault: *mut HewFault,
    state: *mut HewCoroState,
    frame: *mut c_void,
    started: bool,
    completion: Arc<crate::actor_native::NativeActorCompletion>,
}

impl NativeInvocation {
    /// The supervisor retains config until this invocation has been drained.
    #[expect(
        clippy::unnecessary_box_returns,
        reason = "callback output pointers must retain this allocation's address across suspension"
    )]
    pub(super) unsafe fn new(
        sup: *mut HewSupervisor,
        callback: HewNativeChildSpawnFn,
        config: *const c_void,
        waker: &OwnedWaker,
    ) -> Box<Self> {
        // SAFETY: The caller retains the supervisor/config; this owner creates its independent state and stable output storage.
        unsafe {
            // SAFETY: the owner retains the supervisor throughout construction.
            let owner = (*sup).local_pid_id;
            let completion = local_handles::current_supervisor_completion(owner)
                .expect("a live supervisor has its completion");
            // SAFETY: the descriptor remains retained by the state.
            let state = coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            {
                let mut registration = completion.construction.lock_or_recover();
                assert!(
                    registration.is_none(),
                    "one child invocation per supervisor"
                );
                // SAFETY: this invocation owns the live state until final cleanup.
                let cancellation = coro_state::RetainedCancellation::new(&*state);
                // Cancellation publishes its bit before consulting this registration.
                // SAFETY: the owner keeps the supervisor live through registration.
                if (*sup).cancelled.load(Ordering::Acquire) {
                    cancellation.cancel(crate::fault::HEW_FAULT_CANCELLED);
                }
                *registration = Some(cancellation);
            }
            Box::new(Self {
                callback,
                owner,
                config,
                result: HewLocalPidId::INVALID,
                fault: ptr::null_mut(),
                state,
                frame: ptr::null_mut(),
                started: false,
                completion,
            })
        }
    }

    pub(super) fn cancel(&self, reason: i32) {
        self.completion.cancel_construction(reason);
    }

    /// Resume only after readiness; the caller serializes every poll and keeps
    /// this boxed allocation stable until final suspension and frame destruction.
    pub(super) unsafe fn poll(&mut self) -> Option<Result<HewLocalPidId, *mut HewFault>> {
        // SAFETY: This execution owner serializes the live invocation frame, state, result and fault slots through final cleanup.
        unsafe {
            // Each resume may run on another worker or the teardown owner.
            // The invocation facts stay explicit; no actor/arena pointer is
            // borrowed from the supervisor turn across suspension.
            let execution = (*self.state).execution();
            let previous = crate::execution_context::current_context();
            let mut context = crate::execution_context::HewExecutionContext {
                cancel_token: coro_state::hew_coro_state_token(self.state),
                caller: execution.actor_caller,
                request_deadline: execution.request_deadline,
                prev_context: previous,
                ..crate::execution_context::HewExecutionContext::default()
            };
            let owner = execution.actor_turn.actor_id();
            let _restore = RestoreExecutionContext(previous);
            let _ = crate::execution_context::set_current_context(&raw mut context);
            crate::fault::with_owning_actor(owner, || {
                if self.started {
                    crate::cont::hew_cont_resume(self.frame);
                } else {
                    self.started = true;
                    self.frame = (self.callback)(
                        self.owner,
                        self.config,
                        &raw mut self.result,
                        &raw mut self.fault,
                        self.state,
                    );
                }
                let status = coro_state::hew_coro_state_status(self.state);
                if status == CoroStatus::Pending as i32 {
                    if self.frame.is_null() || crate::cont::hew_cont_done(self.frame) {
                        std::process::abort();
                    }
                    return None;
                }
                if status == CoroStatus::Yielded as i32 {
                    std::process::abort();
                }
                if !self.frame.is_null() {
                    if !crate::cont::hew_cont_done(self.frame) {
                        std::process::abort();
                    }
                    crate::cont::hew_cont_destroy(self.frame);
                    self.frame = ptr::null_mut();
                }
                let private = coro_state::hew_coro_state_private_status(self.state);
                self.completion.construction.lock_or_recover().take();
                coro_state::hew_coro_state_free(self.state);
                self.state = ptr::null_mut();
                if private == 0 && self.result != HewLocalPidId::INVALID {
                    Some(Ok(self.result))
                } else {
                    if self.fault.is_null() {
                        self.fault = crate::fault::hew_fault_new(if private == 0 {
                            crate::internal::types::HewError::ErrOom as i32
                        } else {
                            private
                        });
                    }
                    Some(Err(std::mem::replace(&mut self.fault, ptr::null_mut())))
                }
            })
        }
    }
}

impl Drop for NativeInvocation {
    fn drop(&mut self) {
        // A live frame borrows the parent config: its owner must cancel and
        // drain it, or retain the entire parent on bounded shutdown failure.
        if !self.state.is_null() {
            std::process::abort();
        }
        if !self.fault.is_null() {
            // SAFETY: an untransferred diagnostic is exclusively owned here.
            unsafe { crate::fault::hew_fault_drop(self.fault) };
        }
    }
}

/// Discard a completed child whose publication lost to cancellation. Its
/// canonical terminal owner remains joined to the parent until cleanup ends.
pub(super) unsafe fn discard_native_result(
    sup: *mut HewSupervisor,
    kind: c_int,
    token: HewLocalPidId,
) {
    // SAFETY: The invocation transfers one unpublished result into the existing terminal owner joined to its retained parent.
    unsafe {
        if kind == ROLE_KIND_SUPERVISOR {
            retain_nested_completion(sup, token);
            stop_local_supervisor(token, true);
        } else if let Some(actor) = local_handles::resolve_current_actor(token)
            .and_then(crate::lifetime::live_actors::get_actor_ptr_by_id)
        {
            // SAFETY: the returned direct actor is newly transferred and unadopted.
            if let Some(completion) = (*actor).native_completion.clone() {
                retain_child_completion(sup, completion);
            }
            // SAFETY: this invocation owns the unpublished actor.
            actor::hew_actor_stop(actor);
            spawn_deferred_restart_free(vec![DeferredFree(actor)]);
        }
    }
}

/// Initial construction remains private until every declaration is installed.
/// Rollback transfers the parent to canonical teardown, then waits for its
/// completion before releasing this operation's fault/result storage.
#[derive(Debug)]
pub struct HewSupervisorConstruction {
    supervisor: *mut HewSupervisor,
    children: Vec<HewNativeChildSpec>,
    order: Vec<(usize, usize)>,
    next: usize,
    invocation: Option<Box<NativeInvocation>>,
    rollback: Option<Arc<crate::actor_native::NativeActorCompletion>>,
    waker: Arc<OwnedWaker>,
    result: HewLocalPidId,
    fault: *mut HewFault,
    final_state: bool,
    taken: bool,
}

impl HewSupervisorConstruction {
    unsafe fn rollback(&mut self) {
        // SAFETY: The operation uniquely owns the private parent and transfers it once to canonical teardown.
        unsafe {
            // SAFETY: this operation owns the still-private parent.
            let token = (*self.supervisor).local_pid_id;
            self.rollback = local_handles::current_supervisor_completion(token);
            if let Some(completion) = &self.rollback {
                completion.wake_on_finish(&self.waker);
            }
            // The stop request hands this allocation to the canonical teardown owner.
            hew_supervisor_stop_native(token);
            self.supervisor = ptr::null_mut();
        }
    }
}

/// Adopt config and copy descriptors without invoking child code.
///
/// # Safety
/// Config is uniquely transferred (including refusal), descriptors are readable
/// for `child_count`, and waker obeys its retained descriptor contract.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_spawn_new(
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,
    config: *mut c_void,
    config_drop: Option<unsafe extern "C" fn(*mut c_void)>,
    children: *const HewNativeChildSpec,
    child_count: usize,
    has_backoff: c_int,
    backoff_initial_ns: i64,
    backoff_maximum_ns: i64,
    pool_count: i64,
    waker: *const HewWaker,
) -> *mut HewSupervisorConstruction {
    // SAFETY: The caller transfers config and lends readable descriptors and a retained waker; this operation owns the private parent.
    unsafe {
        // SAFETY: the caller retains its descriptor and array during this call.
        let waker = Arc::new(OwnedWaker::retain(&*waker));
        let children = std::slice::from_raw_parts(children, child_count).to_vec();
        let mut op = Box::new(HewSupervisorConstruction {
            supervisor: ptr::null_mut(),
            children,
            order: Vec::new(),
            next: 0,
            invocation: None,
            rollback: None,
            waker,
            result: HewLocalPidId::INVALID,
            fault: ptr::null_mut(),
            final_state: false,
            taken: false,
        });
        let is_pool = strategy == STRATEGY_SIMPLE_ONE_FOR_ONE;
        let valid_count = if is_pool {
            (1..=i64::from(i32::MAX)).contains(&pool_count) && child_count == 1
        } else {
            pool_count == 0
        };
        let valid_backoff = has_backoff == 0
            || (has_backoff == 1
                && backoff_initial_ns > 0
                && backoff_maximum_ns >= backoff_initial_ns);
        if !valid_count || !valid_backoff {
            // SAFETY: the operation still owns the unadopted config.
            pool::release_unadopted_config(config, config_drop);
            op.fault = Box::into_raw(Box::new(HewFault::with_message(
                crate::internal::types::HEW_TRAP_USER_PANIC,
                if valid_backoff {
                    "supervisor pool count must be between 1 and 2147483647"
                } else {
                    "supervisor backoff requires positive initial and maximum >= initial"
                }
                .into(),
            )));
            op.final_state = true;
            return Box::into_raw(op);
        }
        // SAFETY: supervisor allocation has no preconditions.
        op.supervisor = hew_supervisor_new(strategy, max_restarts, window_secs);
        if op.supervisor.is_null() {
            pool::release_unadopted_config(config, config_drop);
            op.fault = crate::fault::hew_fault_new(crate::internal::types::HewError::ErrOom as i32);
            op.final_state = true;
            return Box::into_raw(op);
        }
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "validated positive pool count fits i32"
        )]
        let members = if is_pool { pool_count as usize } else { 1 };
        let mut declarations: Vec<_> = op.children.iter().enumerate().collect();
        declarations.sort_unstable_by_key(|(_, child)| child.startup_rank);
        let valid_order = declarations
            .iter()
            .enumerate()
            .all(|(rank, (_, child))| child.startup_rank as usize == rank);
        let slots = {
            // SAFETY: the operation exclusively owns this fresh parent.
            let mut roster = (*op.supervisor).roster.lock_or_recover();
            roster.backoff = (has_backoff == 1).then_some((
                backoff_initial_ns.cast_unsigned(),
                backoff_maximum_ns.cast_unsigned(),
            ));
            roster.config_buf = config;
            roster.config_drop_fn = config_drop;
            if valid_order {
                pool::register_native_roster(
                    &mut roster,
                    &op.children,
                    &declarations,
                    members,
                    is_pool,
                )
            } else {
                None
            }
        };
        if let Some(slots) = slots {
            op.order = declarations
                .iter()
                .flat_map(|(declaration, _)| {
                    slots[*declaration].iter().map(|slot| (*declaration, *slot))
                })
                .collect();
            // Private construction admits earlier sibling roles, while lifecycle
            // notifications wait for publication or rollback.
            (*op.supervisor).constructing.store(true, Ordering::Release);
        } else {
            op.fault = crate::fault::hew_fault_new(crate::internal::types::HewError::ErrOom as i32);
            op.rollback();
        }
        Box::into_raw(op)
    }
}

/// Advance initial construction; cancellation drains the active callback and
/// canonical rollback before final completion. `cancellation` is zero while
/// running, otherwise the caller's ordinary cancellation fault code.
///
/// # Safety
/// The caller exclusively owns a live operation until take/free.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_spawn_poll(
    op: *mut HewSupervisorConstruction,
    cancellation: c_int,
) -> c_int {
    // SAFETY: The caller exclusively owns this live operation, its retained parent and boxed invocation through final rollback.
    unsafe {
        let op = &mut *op;
        if op.final_state {
            return 1;
        }
        if let Some(completion) = &op.rollback {
            op.final_state = completion.is_finished();
            return c_int::from(op.final_state);
        }
        // SAFETY: the operation retains its private parent until rollback transfer.
        let cancellation = match cancellation {
            0 if (*op.supervisor).cancelled.load(Ordering::Acquire) => {
                crate::fault::HEW_FAULT_CANCELLED
            }
            code => code,
        };
        let cancelled = cancellation != 0;
        if cancelled && op.fault.is_null() {
            op.fault = crate::fault::hew_fault_new(cancellation);
        }
        loop {
            if op.invocation.is_none() {
                if cancelled || op.next == op.order.len() {
                    break;
                }
                let (declaration, _) = op.order[op.next];
                let config = (*op.supervisor).roster.lock_or_recover().config_buf;
                op.invocation = Some(NativeInvocation::new(
                    op.supervisor,
                    op.children[declaration].spawn,
                    config.cast_const(),
                    &op.waker,
                ));
            }
            let invocation = op
                .invocation
                .as_mut()
                .unwrap_or_else(|| std::process::abort());
            if cancelled {
                invocation.cancel(cancellation);
            }
            let Some(outcome) = invocation.poll() else {
                return 0;
            };
            op.invocation.take();
            let (declaration, slot) = op.order[op.next];
            let cancelled = cancelled || (*op.supervisor).cancelled.load(Ordering::Acquire);
            match outcome {
                Ok(token) => {
                    let kind = op.children[declaration].role_kind;
                    if cancelled || !pool::adopt_native_child(op.supervisor, slot, kind, token) {
                        discard_native_result(op.supervisor, kind, token);
                        if op.fault.is_null() {
                            op.fault = crate::fault::hew_fault_new(
                                if cancelled || (*op.supervisor).cancelled.load(Ordering::Acquire) {
                                    crate::fault::HEW_FAULT_CANCELLED
                                } else {
                                    crate::internal::types::HewError::ErrOom as i32
                                },
                            );
                        }
                        break;
                    }
                    op.next += 1;
                }
                Err(fault) => {
                    if op.fault.is_null() {
                        op.fault = fault;
                    } else {
                        crate::fault::hew_fault_drop(fault);
                    }
                    break;
                }
            }
        }
        if op.fault.is_null() && hew_supervisor_start(op.supervisor) == 0 {
            op.result = (*op.supervisor).local_pid_id;
            op.supervisor = ptr::null_mut();
            op.final_state = true;
            return 1;
        }
        if op.fault.is_null() {
            op.fault = crate::fault::hew_fault_new(
                if (*op.supervisor).cancelled.load(Ordering::Acquire) {
                    crate::fault::HEW_FAULT_CANCELLED
                } else {
                    crate::internal::types::HewError::ErrOom as i32
                },
            );
        }
        op.rollback();
        op.final_state = op
            .rollback
            .as_ref()
            .is_none_or(|completion| completion.is_finished());
        c_int::from(op.final_state)
    }
}

/// Transfer the final result or diagnostic exactly once.
///
/// # Panics
/// Panics if called before final completion or more than once.
///
/// # Safety
/// `op` is final, untaken, and fault points to a writable empty slot.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_spawn_take(
    op: *mut HewSupervisorConstruction,
    fault: *mut *mut HewFault,
) -> HewLocalPidId {
    // SAFETY: The caller owns the final operation and lends its empty output fault slot for this one transfer.
    unsafe {
        let op = &mut *op;
        assert!(op.final_state && !op.taken);
        op.taken = true;
        *fault = std::mem::replace(&mut op.fault, ptr::null_mut());
        op.result
    }
}

/// Release a final operation after result transfer.
///
/// # Panics
/// Panics if called before final completion and result transfer.
///
/// # Safety
/// `op` is final, taken, and exclusively owned.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_spawn_free(op: *mut HewSupervisorConstruction) {
    // SAFETY: The caller transfers the final, taken operation exactly once.
    unsafe {
        let op = Box::from_raw(op);
        assert!(op.final_state && op.taken);
    }
}

/// The exact declaration generation checked again when an invocation finishes.
enum NativePublication {
    Actor {
        index: usize,
        identity: u64,
        revision: u64,
        template: Arc<ChildStateTemplate>,
    },
    Supervisor {
        index: usize,
        identity: u64,
        previous: *mut HewSupervisor,
        previous_token: HewLocalPidId,
    },
}

impl NativePublication {
    fn kind(&self) -> c_int {
        match self {
            Self::Actor { .. } => ROLE_KIND_ACTOR,
            Self::Supervisor { .. } => ROLE_KIND_SUPERVISOR,
        }
    }

    /// Read the callback and exact-generation publication facts together.
    unsafe fn snapshot(
        sup: *mut HewSupervisor,
        role: RestartChildRole,
    ) -> Option<(Self, HewNativeChildSpawnFn, *const c_void)> {
        // SAFETY: The caller retains the parent; the roster lock protects each declaration and borrowed config snapshot.
        unsafe {
            let roster = (*sup).roster.lock_or_recover();
            match role {
                RestartChildRole::Actor(identity) => {
                    let (index, spec) = roster
                        .child_specs
                        .iter()
                        .enumerate()
                        .find(|(_, spec)| spec.identity == identity && !spec.spent)?;
                    Some((
                        Self::Actor {
                            index,
                            identity,
                            revision: spec.revision,
                            template: Arc::clone(&spec.state_template),
                        },
                        spec.native_spawn?,
                        spec.config.cast_const(),
                    ))
                }
                RestartChildRole::Supervisor { index, identity } => {
                    let spec = roster.child_supervisor_specs.get(index)?.as_ref()?;
                    if spec.identity != identity || spec.spent {
                        return None;
                    }
                    let SupervisorChildSpawn::Native { spawn, config } = spec.spawn else {
                        return None;
                    };
                    Some((
                        Self::Supervisor {
                            index,
                            identity,
                            previous: roster.child_supervisors[index],
                            previous_token: roster.child_supervisor_tokens[index],
                        },
                        spawn,
                        config,
                    ))
                }
            }
        }
    }

    unsafe fn publish(&self, sup: *mut HewSupervisor, token: HewLocalPidId) -> bool {
        // SAFETY: The invocation owns its fresh result, while the caller retains the parent through exact-generation publication.
        unsafe {
            match self {
                Self::Actor {
                    index,
                    identity,
                    revision,
                    template,
                } => {
                    let Some(child) = local_handles::resolve_current_actor(token)
                        .and_then(crate::lifetime::live_actors::get_actor_ptr_by_id)
                    else {
                        return false;
                    };
                    // The fresh actor has no supervision edge until this invocation
                    // transfers it; the generation-checked store is the publication.

                    (*child).supervisor = sup.cast();
                    #[expect(
                        clippy::cast_possible_truncation,
                        clippy::cast_possible_wrap,
                        reason = "declaration slots fit i32"
                    )]
                    {
                        (*child).supervisor_child_index = *index as i32;
                    }

                    teardown::publish_restart_snapshot(
                        sup, *index, *identity, *revision, template, child,
                    )
                }
                Self::Supervisor {
                    index,
                    identity,
                    previous,
                    previous_token,
                } => {
                    let Some(pin) = local_handles::pin_current_supervisor(token) else {
                        return false;
                    };
                    let child = pin.supervisor();
                    let mut roster = (*sup).roster.lock_or_recover();
                    if (*sup).cancelled.load(Ordering::Acquire)
                        || !roster
                            .child_supervisor_specs
                            .get(*index)
                            .and_then(Option::as_ref)
                            .is_some_and(|spec| spec.identity == *identity && !spec.spent)
                        || roster.child_supervisors.get(*index) != Some(previous)
                        || roster.child_supervisor_tokens.get(*index) != Some(previous_token)
                    {
                        return false;
                    }

                    (*child).parent = sup;
                    (*child).index_in_parent = *index;
                    roots::hew_shutdown_unregister_supervisor(child);

                    roster.child_supervisors[*index] = child;
                    roster.child_supervisor_tokens[*index] = token;
                    if let Some(spec) = roster.child_supervisor_specs[*index].as_mut() {
                        spec.backoff.published();
                    }
                    true
                }
            }
        }
    }

    fn failed(&self, sup: *mut HewSupervisor) {
        if let Self::Actor {
            index,
            identity,
            revision,
            template,
        } = self
        {
            teardown::fail_restart_snapshot(sup, *index, *identity, *revision, template);
        }
    }
}

/// The construction phase of the existing group restart transaction. The
/// stopped wave and fault records remain owned by `PendingGroupRestart`.
pub(super) struct NativeReconstruction {
    roles: Vec<RestartChildRole>,
    next: usize,
    invocation: Option<(NativePublication, Box<NativeInvocation>)>,
}

impl NativeReconstruction {
    pub(super) unsafe fn new(sup: *mut HewSupervisor, roles: &[RestartChildRole]) -> Self {
        // SAFETY: The caller retains the supervisor/config; this owner creates its independent state and stable output storage.
        unsafe {
            let roster = (*sup).roster.lock_or_recover();
            let mut roles = roles.to_vec();
            roles.sort_unstable_by_key(|role| {
                roster
                    .native_startup_order
                    .iter()
                    .position(|identity| *identity == role.identity())
                    .expect("native declaration rank")
            });
            // Keep lifecycle policy serialized throughout suspended construction,
            // as it was during a synchronous supervisor turn.
            (*sup).constructing.store(true, Ordering::Release);
            Self {
                roles,
                next: 0,
                invocation: None,
            }
        }
    }

    /// None retains this transaction until its ordinary readiness notification.
    pub(super) unsafe fn poll(
        &mut self,
        sup: *mut HewSupervisor,
        waker: &OwnedWaker,
    ) -> Option<bool> {
        // SAFETY: This execution owner serializes the live invocation frame, state, result and fault slots through final cleanup.
        unsafe {
            loop {
                if (*sup).cancelled.load(Ordering::Acquire) {
                    return self.drain_cancelled(sup).then_some(false);
                }
                if self.invocation.is_none() {
                    if self.next == self.roles.len() {
                        return Some(true);
                    }
                    let Some((publication, callback, config)) =
                        NativePublication::snapshot(sup, self.roles[self.next])
                    else {
                        return Some(false);
                    };
                    self.invocation = Some((
                        publication,
                        NativeInvocation::new(sup, callback, config, waker),
                    ));
                }
                let (publication, invocation) =
                    self.invocation.as_mut().expect("active restart child");
                let outcome = invocation.poll()?;
                let complete = match outcome {
                    Ok(token) => {
                        if publication.publish(sup, token) {
                            true
                        } else {
                            discard_native_result(sup, publication.kind(), token);
                            false
                        }
                    }
                    Err(fault) => {
                        // The pending group owns the original failure record. A
                        // constructor refusal cannot replace or clear its ruling.
                        crate::fault::hew_fault_drop(fault);
                        publication.failed(sup);
                        false
                    }
                };
                self.invocation.take();
                if !complete {
                    return Some(false);
                }
                self.next += 1;
            }
        }
    }

    /// Teardown has drained the self actor, so it now exclusively owns the
    /// suspended invocation. Cancellation must finish typed cleanup before
    /// config or any sibling can be reclaimed.
    pub(super) unsafe fn drain_cancelled(&mut self, sup: *mut HewSupervisor) -> bool {
        // SAFETY: Self-actor quiescence transfers this frame exclusively to teardown, which retains siblings and config until it finishes.
        unsafe {
            let Some((publication, invocation)) = self.invocation.as_mut() else {
                return true;
            };
            invocation.cancel(crate::fault::HEW_FAULT_CANCELLED);
            let Some(outcome) = invocation.poll() else {
                return false;
            };
            match outcome {
                Ok(token) => discard_native_result(sup, publication.kind(), token),
                Err(fault) => crate::fault::hew_fault_drop(fault),
            }
            self.invocation.take();
            true
        }
    }
}

/// Test-host driver for native ABI fixtures. Production construction is always
/// polled by the generated caller or existing pending restart transaction.
#[cfg(test)]
#[expect(
    clippy::too_many_arguments,
    reason = "test host mirrors the native initial-construction ABI"
)]
pub(crate) unsafe fn native_spawn_for_test(
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,
    config: *mut c_void,
    config_drop: Option<unsafe extern "C" fn(*mut c_void)>,
    children: *const HewNativeChildSpec,
    child_count: usize,
    pool_count: i64,
    fault: *mut *mut HewFault,
) -> HewLocalPidId {
    // SAFETY: The fixture transfers config and retains its descriptors; this test host serializes the complete operation.
    unsafe {
        let (ready, waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: fixtures transfer the config and retain their typed descriptors.

        let op = hew_supervisor_native_spawn_new(
            strategy,
            max_restarts,
            window_secs,
            config,
            config_drop,
            children,
            child_count,
            0,
            0,
            0,
            pool_count,
            waker.descriptor(),
        );
        while hew_supervisor_native_spawn_poll(op, 0) == 0 {
            ready.wait();
        }
        let token = hew_supervisor_native_spawn_take(op, fault);
        hew_supervisor_native_spawn_free(op);
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Config {
        destroyed: Arc<AtomicUsize>,
        dropped: Arc<AtomicUsize>,
        ignore_cancellation: bool,
        cancel_on_resume: bool,
        expected_cancellation: i32,
    }

    #[repr(C)]
    struct SuspendedChild {
        prefix: crate::cont::CoroFramePrefix,
        config: *const Config,
        owner: HewLocalPidId,
        wait: *mut role::HewNativeRestartWait,
        result: *mut HewLocalPidId,
        state: *mut HewCoroState,
    }

    unsafe fn assert_context(state: *mut HewCoroState) {
        // SAFETY: the callback invocation retains state and installs its carrier.
        unsafe {
            let context = crate::execution_context::current_context();
            assert!(!context.is_null());
            assert!((*context).actor.is_null());
            assert_eq!(
                (*context).cancel_token,
                coro_state::hew_coro_state_token(state)
            );
        }
    }

    unsafe extern "C-unwind" fn resume_child(frame: *mut c_void) {
        // SAFETY: The invocation exclusively resumes this fixture frame and keeps its config, state and output slot live.
        unsafe {
            // SAFETY: the invocation owns this suspended fixture frame and config.
            let frame = &mut *frame.cast::<SuspendedChild>();
            assert_context(frame.state);
            let config = &*frame.config;
            assert_eq!(config.dropped.load(Ordering::Acquire), 0);
            if config.cancel_on_resume {
                let pin = local_handles::pin_current_supervisor(frame.owner).unwrap();
                request_supervisor_shutdown(pin.supervisor());
            }
            // SAFETY: the invocation exclusively resumes its live state.
            let cancelled = coro_state::hew_coro_state_is_cancelled(frame.state) != 0;
            let code = if cancelled {
                coro_state::hew_coro_state_cancel_code(frame.state)
            } else {
                0
            };
            if cancelled {
                assert_eq!(code, config.expected_cancellation);
            }
            let status = if cancelled && !config.ignore_cancellation {
                code
            } else {
                // SAFETY: this fixture spawns an empty, independently owned actor.
                let actor = actor::hew_actor_spawn(ptr::null_mut(), 0, None);
                *frame.result = (*actor).local_pid_id;
                0
            };
            frame.prefix.resume = None;
            coro_state::hew_coro_state_finish(frame.state, status);
        }
    }

    unsafe extern "C-unwind" fn destroy_child(frame: *mut c_void) {
        // SAFETY: Final suspension transfers this unique fixture frame here; its config and role wait remain live until destruction.
        unsafe {
            // SAFETY: final suspension transferred the unique frame here.
            let frame = Box::from_raw(frame.cast::<SuspendedChild>());
            assert_context(frame.state);
            role::hew_supervisor_native_restart_wait_free(frame.wait);
            assert!(frame.prefix.resume.is_none());
            (*frame.config).destroyed.fetch_add(1, Ordering::AcqRel);
        }
    }

    unsafe extern "C-unwind" fn begin_child(
        owner: HewLocalPidId,
        config: *const c_void,
        result: *mut HewLocalPidId,
        _fault: *mut *mut HewFault,
        state: *mut HewCoroState,
    ) -> *mut c_void {
        // SAFETY: The invocation retains config/output/state; the returned fixture frame owns its ordinary restart-wait registration.
        unsafe {
            assert_context(state);
            Box::into_raw(Box::new(SuspendedChild {
                prefix: crate::cont::CoroFramePrefix {
                    resume: Some(resume_child),
                    destroy: Some(destroy_child),
                },
                config: config.cast(),
                owner,
                wait: role::hew_supervisor_native_restart_wait_new(
                    owner,
                    0,
                    ROLE_KIND_ACTOR,
                    coro_state::hew_coro_state_waker(state),
                ),
                result,
                state,
            }))
            .cast()
        }
    }

    unsafe extern "C" fn drop_config(config: *mut c_void) {
        // SAFETY: Canonical teardown uniquely owns the initialized config, after frame destruction released all of its readers.
        unsafe {
            // SAFETY: canonical teardown owns the initialized config wrapper.
            let config = config.cast::<Config>().read();
            assert_eq!(config.destroyed.load(Ordering::Acquire), 1);
            assert_eq!(config.dropped.fetch_add(1, Ordering::AcqRel), 0);
        }
    }

    #[test]
    fn supervisor_resumable_initial_construction_destroys_frame_before_config() {
        let _runtime = crate::runtime_test_guard();
        for (cancellation, ignore_cancellation, cancel_on_resume) in [
            (0, false, false),
            (crate::fault::HEW_FAULT_CANCELLED, false, false),
            (crate::fault::HEW_FAULT_CANCELLED, true, false),
            (crate::fault::HEW_FAULT_DEADLINE, false, false),
            (crate::fault::HEW_FAULT_DEADLINE, true, false),
            (0, true, true),
        ] {
            let expected_cancellation = if cancel_on_resume {
                crate::fault::HEW_FAULT_CANCELLED
            } else {
                cancellation
            };
            let destroyed = Arc::new(AtomicUsize::new(0));
            let dropped = Arc::new(AtomicUsize::new(0));
            let (ready, waker) = crate::wake::blocking::Readiness::new();
            let child = HewNativeChildSpec {
                restart_policy: RESTART_PERMANENT,
                role_kind: ROLE_KIND_ACTOR,
                spawn: begin_child,
                name: ptr::null(),
                stop_ns: 0,
                startup_rank: 0,
                dispatch: None,
            };
            // SAFETY: the operation adopts one initialized config and all
            // descriptors remain live through begin; this host serializes polls.
            unsafe {
                let config =
                    crate::mem::buf_try_alloc(std::mem::size_of::<Config>()).cast::<Config>();
                config.write(Config {
                    destroyed: Arc::clone(&destroyed),
                    dropped: Arc::clone(&dropped),
                    ignore_cancellation,
                    cancel_on_resume,
                    expected_cancellation,
                });
                let op = hew_supervisor_native_spawn_new(
                    STRATEGY_ONE_FOR_ONE,
                    3,
                    5,
                    config.cast(),
                    Some(drop_config),
                    &raw const child,
                    1,
                    0,
                    0,
                    0,
                    0,
                    waker.descriptor(),
                );
                assert_eq!(hew_supervisor_native_spawn_poll(op, 0), 0);
                assert_eq!(destroyed.load(Ordering::Acquire), 0);
                assert_eq!(dropped.load(Ordering::Acquire), 0);
                while hew_supervisor_native_spawn_poll(op, cancellation) == 0 {
                    ready.wait();
                }
                assert_eq!(destroyed.load(Ordering::Acquire), 1);
                let mut fault = ptr::null_mut();
                let token = hew_supervisor_native_spawn_take(op, &raw mut fault);
                hew_supervisor_native_spawn_free(op);
                if cancellation != 0 || cancel_on_resume {
                    assert_eq!(token, HewLocalPidId::INVALID);
                    assert!(!fault.is_null());
                    assert_eq!((*fault).code(), expected_cancellation);
                    crate::fault::hew_fault_drop(fault);
                } else {
                    assert!(fault.is_null());
                    let pin = local_handles::pin_current_supervisor(token).unwrap();
                    let parent = pin.supervisor();
                    drop(pin);
                    hew_supervisor_stop(parent);
                }
                assert_eq!(dropped.load(Ordering::Acquire), 1);
            }
        }
    }

    #[test]
    fn supervisor_pending_restart_cancellation_drains_frame_before_config() {
        struct Scheduler;
        impl Drop for Scheduler {
            fn drop(&mut self) {
                crate::scheduler::hew_sched_shutdown();
                crate::scheduler::hew_runtime_cleanup();
            }
        }
        let _runtime = crate::runtime_test_guard();
        crate::scheduler::init_real_scheduler_for_test();
        let _scheduler = Scheduler;
        for ignore_cancellation in [false, true] {
            let destroyed = Arc::new(AtomicUsize::new(0));
            let dropped = Arc::new(AtomicUsize::new(0));
            let (_, waker) = crate::wake::blocking::Readiness::new();
            let child = HewNativeChildSpec {
                restart_policy: RESTART_PERMANENT,
                role_kind: ROLE_KIND_ACTOR,
                spawn: begin_child,
                name: ptr::null(),
                stop_ns: 0,
                startup_rank: 0,
                dispatch: None,
            };
            // SAFETY: this test owns initial invocation, then transfers the
            // published parent to its ordinary restart and teardown owners.
            unsafe {
                let config =
                    crate::mem::buf_try_alloc(std::mem::size_of::<Config>()).cast::<Config>();
                config.write(Config {
                    destroyed: Arc::clone(&destroyed),
                    dropped: Arc::clone(&dropped),
                    ignore_cancellation,
                    cancel_on_resume: false,
                    expected_cancellation: crate::fault::HEW_FAULT_CANCELLED,
                });
                let op = hew_supervisor_native_spawn_new(
                    STRATEGY_ONE_FOR_ONE,
                    3,
                    5,
                    config.cast(),
                    Some(drop_config),
                    &raw const child,
                    1,
                    0,
                    0,
                    0,
                    0,
                    waker.descriptor(),
                );
                assert_eq!(hew_supervisor_native_spawn_poll(op, 0), 0);
                assert_eq!(hew_supervisor_native_spawn_poll(op, 0), 1);
                let mut fault = ptr::null_mut();
                let token = hew_supervisor_native_spawn_take(op, &raw mut fault);
                hew_supervisor_native_spawn_free(op);
                assert!(fault.is_null());
                let pin = local_handles::pin_current_supervisor(token).unwrap();
                let parent = pin.supervisor();
                let identity = (*parent).roster.lock_or_recover().child_specs[0].identity;
                destroyed.store(0, Ordering::Release);
                assert_eq!(
                    restart::restart_children_for_strategy(
                        parent,
                        STRATEGY_ONE_FOR_ONE,
                        RestartChildRole::Actor(identity),
                        FaultRecord::from_raw(0)
                    ),
                    FaultRuling::ArmedForRestart
                );
                assert!((*parent)
                    .roster
                    .lock_or_recover()
                    .pending_group_restart
                    .as_ref()
                    .is_some_and(|group| group.construction.is_some()));
                assert_eq!(destroyed.load(Ordering::Acquire), 0);
                drop(pin);
                hew_supervisor_stop(parent);
                assert_eq!(destroyed.load(Ordering::Acquire), 1);
                assert_eq!(dropped.load(Ordering::Acquire), 1);
            }
        }
    }
}
