//! Where a runnable actor goes, per target.
//!
//! Activation is the same code everywhere ([`crate::activation`]); publishing a
//! queue entry and waking whoever will run it is not. The native scheduler
//! pushes onto the work-stealing queue and wakes a parked worker; wasm32 has no
//! worker, so the entry goes on the driver's queue and the process itself runs
//! it at the next readiness step.

use crate::activation::SchedulerQueueEntry;
use crate::actor::HewActor;
use crate::lifetime::live_actors::ActorIncarnation;

/// Submit an actor to this target's run queue.
pub(crate) fn sched_enqueue(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: every caller holds a live actor while transferring that lifetime
    // to the queue entry.
    let entry = unsafe { SchedulerQueueEntry::retain(actor) };
    publish(entry);
}

/// Publish an entry whose queue reference the caller already owns.
pub(crate) fn sched_enqueue_owned(entry: SchedulerQueueEntry) {
    publish(entry);
}

/// Wake the parked activation of one live incarnation.
///
/// A wake recorded before the registrant died is dropped rather than delivered
/// to whatever later occupies the allocation (#3069); the incarnation check is
/// what makes that true, and it is the same check on every target.
pub(crate) fn enqueue_resume_by_incarnation(target: ActorIncarnation) {
    #[cfg(not(target_arch = "wasm32"))]
    crate::scheduler::enqueue_resume_by_incarnation(target);
    #[cfg(target_arch = "wasm32")]
    crate::wasm_driver::enqueue_resume_by_incarnation(target);
}

fn publish(entry: SchedulerQueueEntry) {
    #[cfg(not(target_arch = "wasm32"))]
    crate::scheduler::publish_queue_entry(entry);
    #[cfg(target_arch = "wasm32")]
    crate::wasm_driver::publish_queue_entry(entry);
}
