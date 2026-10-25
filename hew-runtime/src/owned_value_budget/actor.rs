//! Construction adopts initialized state before any actor hook executes.

use super::transfer::ValueTransfer;
use super::{Acquisition, ManagedStorage};
use crate::coro_state::HewCoroState;
use crate::util::MutexExt;

#[derive(Debug)]
pub(crate) struct ConstructionBudget {
    transfer: ValueTransfer,
    backing: Option<Acquisition>,
}
// SAFETY: construction pins state; the completion mutex serializes preparation.
unsafe impl Send for ConstructionBudget {}

/// Poll the frozen initial-state handoff before init/start can execute.
/// Returns 0 pending, 1 ready, 2 refused with an owned checked fault.
/// # Safety
/// The constructor uniquely owns its invocation and keeps the actor reserved.
#[no_mangle]
/// # Panics
/// Panics if a reserved native actor lacks required construction metadata.
pub unsafe extern "C" fn hew_actor_prepare_managed_state(
    token: crate::lifetime::local_handles::HewLocalPidId,
    construction: *mut HewCoroState,
    fault: *mut *mut crate::fault::HewFault,
) -> i32 {
    let Some(id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return 2;
    };
    crate::lifetime::live_actors::with_actor_send_by_id(id, |raw| {
        // SAFETY: reservation and registry guard pin state and its descriptor.
        unsafe {
            let actor = &*raw;
            let completion = actor.native_completion.as_ref().expect("native completion");
            let cleanup = &completion.cleanup;
            if cleanup.state_source_owner.lock_or_recover().is_none() {
                return 1;
            }
            let mut pending = completion.construction_budget.lock_or_recover();
            if pending.is_none() {
                let layout_pointer = cleanup
                    .state_layout
                    .load(std::sync::atomic::Ordering::Acquire);
                let layout = &*layout_pointer;
                let bytes = (*layout.managed).slot_bytes;
                let backing = if let Some(owner) = &cleanup.value_owner {
                    if let Ok(guard) = owner.acquire(&[ManagedStorage {
                        identity: actor.state as usize,
                        bytes,
                    }]) {
                        Some(guard)
                    } else {
                        *fault = crate::fault::hew_fault_new(
                            crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
                        );
                        return 2;
                    }
                } else {
                    None
                };
                let source = cleanup
                    .state_source_owner
                    .lock_or_recover()
                    .as_ref()
                    .unwrap()
                    .clone();
                let mut transfer = ValueTransfer::begin(
                    source,
                    cleanup.value_owner.clone(),
                    layout_pointer,
                    actor.state,
                );
                transfer.set_destination_actor(raw);
                transfer.set_waker(crate::wake::OwnedWaker::retain(
                    &*crate::coro_state::hew_coro_state_waker(construction),
                ));
                *pending = Some(ConstructionBudget { transfer, backing });
            }
            match pending.as_mut().unwrap().transfer.poll_prepare() {
                Ok(false) => 0,
                Ok(true) => {
                    let prepared = pending.take().unwrap();
                    if prepared.transfer.commit().is_err() {
                        std::process::abort();
                    }
                    if let Some(backing) = prepared.backing {
                        backing.commit();
                        cleanup
                            .state_backing_charged
                            .store(true, std::sync::atomic::Ordering::Release);
                    }
                    cleanup.state_source_owner.lock_or_recover().take();
                    (*construction)
                        .execution_mut()
                        .value_owner
                        .clone_from(&cleanup.value_owner);
                    1
                }
                Err(_) => {
                    pending.take();
                    *fault =
                        crate::fault::hew_fault_new(crate::internal::types::HEW_TRAP_HEAP_EXCEEDED);
                    2
                }
            }
        }
    })
    .unwrap_or(2)
}
