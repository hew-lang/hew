use super::{
    checked, hew_cancel_token_cancel, hew_checked_scope_wait_cancel_losers,
    hew_checked_scope_wait_status, hew_checked_scope_wait_take_fault, hew_checked_task_wait_take,
    hew_fault_combine, HewCheckedScopeWait, HewCheckedTaskWait, HewCoroState, HewFault,
    HewReleaseCursor, HewTask, HewWaker, Mutex, MutexExt, ReleaseDriver, ScopeCancellation,
    ScopeResultClose, PENDING, READY,
};
use crate::coro_state::{
    hew_coro_state_cancel_code, hew_coro_state_finish, hew_coro_state_is_cancelled,
};
use std::ptr;

pub(super) struct RaceGroup {
    task: *mut HewTask,
    members: Vec<*mut HewTask>,
    member_slot_bytes: u64,
    value_owner: Option<crate::owned_value_budget::ValueOwner>,
    wait: Box<HewCheckedScopeWait>,
    selected: bool,
    winner: bool,
    cancellation: i32,
    fault: *mut HewFault,
    release: Option<Box<ReleaseDriver>>,
}

impl RaceGroup {
    pub(super) unsafe fn visit_managed(
        &mut self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use hew_cabi::managed::{DESCEND, HEAP_EXCEEDED, SKIP_DESCENDANTS};
        let Some(bytes) = self
            .member_slot_bytes
            .checked_mul(self.members.capacity() as u64)
        else {
            return HEAP_EXCEEDED;
        };
        // SAFETY: a frozen executor keeps the group's owning member vector stable.
        unsafe {
            match crate::owned_value_budget::visit::storage(
                visitor,
                self.members.as_ptr().cast(),
                bytes,
            ) {
                SKIP_DESCENDANTS => return 0,
                DESCEND => {}
                failure => return failure,
            }
            for member in &self.members {
                let status = super::hew_managed_visit_task(ptr::from_ref(member).cast(), visitor);
                if status != 0 {
                    return status;
                }
            }
            if (*visitor).kind == hew_cabi::managed::HewManagedWalkKind::CommitTransfer {
                self.value_owner = crate::owned_value_budget::transfer::destination_owner(visitor);
            }
        }
        0
    }

    pub(super) unsafe fn new(
        task: *mut HewTask,
        members: Vec<*mut HewTask>,
        state: *mut HewCoroState,
        waker: *const HewWaker,
        member_slot_bytes: u64,
    ) -> Self {
        // Keep the original owning member storage throughout the group. Each
        // readiness observation borrows it through a separate runtime reference.
        let tasks = members
            .iter()
            .map(|member| unsafe {
                super::retain(*member);
                HewCheckedTaskWait::new(*member, waker, false)
            })
            .collect();
        Self {
            task,
            members,
            member_slot_bytes,
            // SAFETY: the executor keeps the original invocation owner live.
            value_owner: unsafe { checked(task) }
                .lock_or_recover()
                .value_owner
                .clone(),
            wait: Box::new(HewCheckedScopeWait {
                // SAFETY: executor retains the group and its live parent scope.
                scope: unsafe { (*task).scope },
                tasks,
                cancellation: ScopeCancellation::Ordinary,
                subset: true,
                close: Mutex::new(ScopeResultClose {
                    // SAFETY: cleanup ignores cancellation and retains readiness.
                    state: unsafe { crate::coro_state::hew_coro_state_cleanup_child(state) },
                    driver: None,
                    collected: false,
                    complete: false,
                    fault: ptr::null_mut(),
                }),
            }),
            selected: false,
            winner: false,
            cancellation: 0,
            fault: ptr::null_mut(),
            release: None,
        }
    }

    pub(super) unsafe fn poll(&mut self, state: *mut HewCoroState, fault: *mut *mut HewFault) {
        // SAFETY: one executor poll exclusively owns the group and output slots.
        unsafe {
            if self.cancellation == 0 && hew_coro_state_is_cancelled(state) != 0 {
                self.cancellation = hew_coro_state_cancel_code(state);
                self.selected = true;
                for member in &self.wait.tasks {
                    if !checked(member.task).lock_or_recover().completed {
                        hew_cancel_token_cancel((*member.task).cancel_token, self.cancellation);
                    }
                }
            }
            if !self.selected {
                let first = self
                    .wait
                    .tasks
                    .iter()
                    .enumerate()
                    .filter_map(|(index, wait)| {
                        let member = checked(wait.task).lock_or_recover();
                        member
                            .completed
                            .then_some((member.order, index, member.outcome()))
                    })
                    .min_by_key(|(order, _, _)| *order);
                let Some((_, index, outcome)) = first else {
                    return;
                };
                self.selected = true;
                if outcome == READY {
                    let result = checked(self.task).lock_or_recover().result;
                    hew_checked_task_wait_take(
                        &raw const self.wait.tasks[index],
                        result,
                        &raw mut self.fault,
                        ptr::null_mut(),
                    );
                    self.winner = true;
                }
                hew_checked_scope_wait_cancel_losers(&raw mut *self.wait);
            }
            let cleanup = self.wait.close.lock_or_recover().state;
            crate::coro_state::abandon_cleanup_io(cleanup);
            if hew_checked_scope_wait_status(&raw const *self.wait) == PENDING {
                return;
            }
            if self.release.is_none() {
                let mut drained = ptr::null_mut();
                hew_checked_scope_wait_take_fault(&raw const *self.wait, &raw mut drained);
                self.fault = hew_fault_combine(self.fault, drained);
                if self.cancellation != 0 && self.fault.is_null() {
                    self.fault = crate::fault::hew_fault_new(self.cancellation);
                }
                if self.winner && !self.fault.is_null() {
                    let result = checked(self.task).lock_or_recover();
                    self.release = Some(ReleaseDriver::new(HewReleaseCursor::values([(
                        result.result,
                        *result.layout,
                    )])));
                }
            }
            if let Some(release) = &mut self.release {
                let cleanup = self.wait.close.lock_or_recover().state;
                if !release.poll(cleanup) {
                    return;
                }
                self.fault = hew_fault_combine(self.fault, release.take_fault());
            }
            let status = self.fault.as_ref().map_or(0, HewFault::code);
            fault.write(std::mem::replace(&mut self.fault, ptr::null_mut()));
            hew_coro_state_finish(state, status);
        }
    }
}

impl Drop for RaceGroup {
    fn drop(&mut self) {
        if let Some(owner) = &self.value_owner {
            owner.release(&[crate::owned_value_budget::ManagedStorage {
                identity: self.members.as_ptr() as usize,
                bytes: self
                    .member_slot_bytes
                    .checked_mul(self.members.capacity() as u64)
                    .unwrap_or_else(|| std::process::abort()),
            }]);
        }
        for member in self.members.drain(..) {
            // SAFETY: the invocation transferred one owning handle per member;
            // observer references remain until this group's fields are dropped.
            unsafe {
                super::release_account(member);
                super::release_task_ref(member);
            }
        }
    }
}
