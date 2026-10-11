use super::{
    checked, hew_cancel_token_cancel, hew_checked_scope_wait_cancel_losers,
    hew_checked_scope_wait_status, hew_checked_scope_wait_take_fault, hew_checked_task_wait_new,
    hew_checked_task_wait_take, hew_fault_combine, HewCheckedScopeWait, HewCoroState, HewFault,
    HewReleaseCursor, HewTask, HewWaker, Mutex, MutexExt, ReleaseDriver, ScopeCancellation,
    ScopeResultClose, PENDING, READY,
};
use crate::coro_state::{
    hew_coro_state_cancel_code, hew_coro_state_finish, hew_coro_state_is_cancelled,
};
use std::ptr;

pub(super) struct RaceGroup {
    task: *mut HewTask,
    wait: Box<HewCheckedScopeWait>,
    selected: bool,
    winner: bool,
    cancellation: i32,
    fault: *mut HewFault,
    release: Option<Box<ReleaseDriver>>,
}

impl RaceGroup {
    pub(super) unsafe fn new(
        task: *mut HewTask,
        members: Vec<*mut HewTask>,
        state: *mut HewCoroState,
        waker: *const HewWaker,
    ) -> Self {
        // SAFETY: invocation owns every member reference, transferred into waits.
        let tasks = members
            .into_iter()
            .map(|member| unsafe { *Box::from_raw(hew_checked_task_wait_new(member, waker)) })
            .collect();
        Self {
            task,
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
