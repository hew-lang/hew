//! Top-level supervisor registration shared by lifecycle and process shutdown.

use super::HewSupervisor;

/// Wrapper for `*mut HewSupervisor` to impl `Send`.
///
/// # Safety
///
/// Supervisor pointers are only accessed during shutdown while the
/// runtime is in a controlled wind-down state.
pub(crate) struct SupervisorPtr(pub(crate) *mut crate::supervisor::HewSupervisor);

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
    crate::runtime::rt_current().supervisor_roots.access(f)
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
pub unsafe extern "C" fn hew_shutdown_register_supervisor(
    sup: *mut crate::supervisor::HewSupervisor,
) {
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
pub unsafe extern "C" fn hew_shutdown_unregister_supervisor(
    sup: *mut crate::supervisor::HewSupervisor,
) {
    if sup.is_null() {
        return;
    }
    with_supervisor_roots(|sups| sups.retain(|s| s.0 != sup));
}
