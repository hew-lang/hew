//! Checked observation ownership for local and admitted actor handles.

use crate::monitor::HEW_OBSERVATION_NO_CONTEXT;

/// One ordinary observation owner, shared by local and admitted actor values.
/// Setup acknowledgement, cancellation and its readiness registration retire together.
pub struct HewNativeActorObservation {
    pub(crate) transport: crate::platform::host_io::RemoteActorObservation,
    pub(crate) ref_id: u64,
    pub(crate) actor_id: u64,
    pub(crate) local_link: Option<u64>,
    pub(crate) kind: u8,
    pub(crate) status: Option<i32>,
    pub(crate) taken: bool,
}

impl std::fmt::Debug for HewNativeActorObservation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("HewNativeActorObservation")
            .field("ref_id", &self.ref_id)
            .field("status", &self.status)
            .finish_non_exhaustive()
    }
}

impl HewNativeActorObservation {
    fn poll(&mut self) -> bool {
        crate::platform::host_io::poll_imported_observation(self)
            .unwrap_or_else(|| self.status.is_some())
    }
}

impl Drop for HewNativeActorObservation {
    fn drop(&mut self) {
        if crate::platform::host_io::retire_imported_observation(self) {
            return;
        }
        if !self.taken && self.status == Some(0) {
            if self.kind == 0 {
                crate::monitor::hew_actor_demonitor(self.ref_id);
            } else if let Some(target) = self.local_link {
                crate::link::unlink_by_ids(self.actor_id, target);
            }
        }
    }
}

/// Begin a monitor (kind 0) or link (kind 1), capturing the current execution
/// context once. Local registration completes inline; imported setup suspends.
/// # Safety
/// The caller borrows the actor token and wake descriptor throughout creation.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_new(
    token: crate::lifetime::local_handles::HewLocalPidId,
    kind: u8,
    waker: *const crate::wake::HewWaker,
) -> *mut HewNativeActorObservation {
    let mut operation = HewNativeActorObservation {
        transport: crate::platform::host_io::new_remote_actor_observation(),
        ref_id: 0,
        actor_id: 0,
        local_link: None,
        kind,
        status: None,
        taken: false,
    };
    let actor = crate::actor::hew_actor_self();
    if actor.is_null() {
        operation.status = Some(HEW_OBSERVATION_NO_CONTEXT);
    } else if kind > 1 {
        operation.status = Some(-crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
    } else {
        // SAFETY: the current activation owns its actor allocation.
        operation.actor_id = unsafe { (*actor).id };
        // SAFETY: forwarded live wake descriptor contract.
        if unsafe {
            crate::platform::host_io::try_start_imported_observation(&mut operation, token, waker)
        } {
            return Box::into_raw(Box::new(operation));
        }
        operation.status = Some(if kind == 0 {
            // SAFETY: the local stack output is writable.
            unsafe { crate::monitor::hew_native_actor_monitor(token, &raw mut operation.ref_id) }
        } else {
            let (status, selected) = crate::link::link_actor_for_observation(token);
            operation.local_link = selected;
            status
        });
    }
    Box::into_raw(Box::new(operation))
}

/// Return -1 while setup is pending, or 0 once its result can be taken.
/// # Safety
/// `operation` is uniquely borrowed and live.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_poll(
    operation: *mut HewNativeActorObservation,
) -> i32 {
    // SAFETY: the caller owns this operation.
    if unsafe { operation.as_mut() }.is_none_or(HewNativeActorObservation::poll) {
        0
    } else {
        -1
    }
}

/// Transfer a completed setup result. Only successful monitor results initialize
/// `out_id`; links have no source-visible registration id.
/// # Safety
/// The operation is live, ready and uniquely borrowed; `out_id` is writable.
/// # Panics
/// Panics if the caller has not acquired readiness before taking the result.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_take(
    operation: *mut HewNativeActorObservation,
    out_id: *mut u64,
) -> i32 {
    // SAFETY: the caller owns the completed operation and output slot.
    let Some(operation) = (unsafe { operation.as_mut() }) else {
        return -crate::internal::types::HEW_TRAP_HEAP_EXCEEDED;
    };
    let status = operation
        .status
        .expect("observation setup ready before take");
    if status == 0 {
        if operation.kind == 0 {
            // SAFETY: the caller provides a writable monitor output slot.
            unsafe { out_id.write(operation.ref_id) };
        }
        operation.taken = true;
    }
    status
}

/// Cancel unfinished setup or release a taken operation.
/// # Safety
/// The caller transfers its unique operation owner; no concurrent poll remains.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_free(operation: *mut HewNativeActorObservation) {
    // SAFETY: the unique operation owner is transferred here.
    if !operation.is_null() {
        // SAFETY: the caller transfers the unique non-null operation owner.
        drop(unsafe { Box::from_raw(operation) });
    }
}
