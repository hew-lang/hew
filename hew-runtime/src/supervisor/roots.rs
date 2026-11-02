//! Top-level supervisor registration shared by lifecycle and process shutdown.

use super::HewSupervisor;

/// Wrapper for `*mut HewSupervisor` to impl `Send`.
///
/// # Safety
///
/// Supervisor pointers are only accessed during shutdown while the
/// runtime is in a controlled wind-down state.
pub(crate) struct SupervisorPtr(pub(crate) *mut HewSupervisor);

// SAFETY: Supervisor pointers are only accessed during shutdown while
// the runtime is in a controlled wind-down state.
unsafe impl Send for SupervisorPtr {}

/// Operate on the installed runtime's registered top-level supervisors.
///
/// The supervisor-roots list was the `TOP_LEVEL_SUPERVISORS` global; it now
/// lives in `RuntimeInner::supervisor_roots`. Fails closed via [`crate::runtime::rt_current`]
/// when no runtime is installed — registering a supervisor root before a
/// runtime exists is a lifecycle error, not a silent no-op.
#[inline]
pub(crate) fn with_supervisor_roots<R>(f: impl FnOnce(&mut Vec<SupervisorPtr>) -> R) -> R {
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::runtime::rt_current().supervisor_roots.access(f)
    }
    #[cfg(target_arch = "wasm32")]
    {
        static ROOTS: crate::lifetime::PoisonSafe<Vec<SupervisorPtr>> =
            crate::lifetime::PoisonSafe::new(Vec::new());
        ROOTS.access(f)
    }
}

/// Delayed restarts and retiring children remain admitted work even while the
/// supervisor's actor has no runnable message. Inspect the existing rooted tree
/// without consuming its registration or cancelling its restart policy.
#[cfg(target_arch = "wasm32")]
pub(crate) fn has_pending_guest_work() -> bool {
    use crate::util::MutexExt;

    unsafe fn pending(supervisor: *mut HewSupervisor) -> bool {
        // SAFETY: the caller retains either the roots lock or the parent's
        // roster. The guest runs no user code during this tree observation.
        let Some(supervisor) = (unsafe { supervisor.as_ref() }) else {
            return false;
        };
        if supervisor.restart_timers.state.lock_or_recover().pending != 0 {
            return true;
        }
        let roster = supervisor.roster.lock_or_recover();
        roster.pending_group_restart.is_some()
            || !roster.construction_events.is_empty()
            || roster
                .retiring_children
                .iter()
                .any(|child| !child.is_finished())
            || roster.child_supervisors.iter().any(|child| {
                // SAFETY: this roster retains the nested supervisor.
                unsafe { pending(*child) }
            })
    }

    with_supervisor_roots(|roots| {
        roots.iter().any(|root| {
            // SAFETY: the roots lock retains the registered supervisor.
            unsafe { pending(root.0) }
        })
    })
}

/// Register a top-level supervisor for ordered shutdown.
///
/// During graceful shutdown, registered supervisors are stopped
/// bottom-up (last registered = first stopped) before the scheduler
/// is terminated.
///
/// # Safety
///
/// `sup` must be a valid pointer to a [`HewSupervisor`] that remains
/// valid until shutdown or until it is unregistered.
#[no_mangle]
pub unsafe extern "C" fn hew_shutdown_register_supervisor(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    with_supervisor_roots(|sups| {
        if !sups.iter().any(|s| s.0 == sup) {
            sups.push(SupervisorPtr(sup));
        }
    });
}

/// Unregister a top-level supervisor from shutdown.
///
/// # Safety
///
/// `sup` must be a pointer previously passed to
/// [`hew_shutdown_register_supervisor`].
#[no_mangle]
pub unsafe extern "C" fn hew_shutdown_unregister_supervisor(sup: *mut HewSupervisor) {
    if sup.is_null() {
        return;
    }
    with_supervisor_roots(|sups| sups.retain(|s| s.0 != sup));
}

/// One root taken out of the cleanup set with the authority needed to stop it.
pub(crate) struct TakenSupervisorRoot {
    supervisor: *mut HewSupervisor,
    control: Option<std::sync::Arc<crate::lifetime::local_handles::SupervisorControl>>,
    teardown: Option<crate::lifetime::local_handles::SupervisorTeardownLease>,
}

impl TakenSupervisorRoot {
    /// Stop the taken root. Once the roots lock is released, a token stop
    /// that already claimed teardown may free the allocation at any moment,
    /// so the claim is taken through the control read under the lock and the
    /// allocation is touched only when that claim is won.
    pub(crate) fn stop(self) {
        let Some(teardown) = self.teardown else {
            return;
        };
        let claim = self.control.as_deref().map_or(
            crate::lifetime::local_handles::TeardownClaim::Held,
            crate::lifetime::local_handles::SupervisorControl::claim_teardown,
        );
        // Reclamation drops the registry's final control reference.
        drop(self.control);
        // SAFETY: the root was live when taken; the claim outcome decides
        // whether this stop may touch it.
        unsafe { super::stop_supervisor_with_teardown_authority(self.supervisor, teardown, claim) };
    }
}

/// Take every registered root for shutdown. Every destructor unregisters a
/// root before freeing it, so membership under the roots lock proves the
/// allocation live: read its stop authority here, while that proof holds.
pub(crate) fn take_supervisor_roots() -> Vec<TakenSupervisorRoot> {
    with_supervisor_roots(|roots| {
        std::mem::take(roots)
            .into_iter()
            .filter(|root| !root.0.is_null())
            .map(|root| {
                let supervisor = root.0;
                // SAFETY: the roots lock retains the registered allocation.
                let (handles, token) =
                    unsafe { ((*supervisor).handles, (*supervisor).local_pid_id) };
                // SAFETY: cleanup retains this authority until admitted
                // teardown leases drain.
                let handles = unsafe { handles.as_ref() };
                TakenSupervisorRoot {
                    supervisor,
                    control: handles
                        .and_then(|handles| handles.supervisor_control_for_raw(token, supervisor)),
                    teardown: handles.and_then(
                        crate::lifetime::local_handles::LocalHandles::begin_supervisor_teardown,
                    ),
                }
            })
            .collect()
    })
}

/// Stop roots in reverse registration order while the process driver can
/// still resume suspended child construction and terminal hooks.
#[cfg(target_arch = "wasm32")]
pub(crate) fn stop_registered_supervisors() -> bool {
    for root in take_supervisor_roots().into_iter().rev() {
        root.stop();
    }
    with_supervisor_roots(|roots| roots.is_empty())
}
