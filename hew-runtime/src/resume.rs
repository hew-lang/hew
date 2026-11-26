//! Where a runnable actor goes, per target.
//!
//! Activation is the same code everywhere ([`crate::activation`]); publishing a
//! queue entry and waking whoever will run it is not. The native scheduler
//! pushes onto the work-stealing queue and wakes a parked worker; on the
//! single-thread driver (always on wasm32) the entry joins the driver's ready
//! list and the process itself runs it at a later step.

use crate::activation::SchedulerQueueEntry;
use crate::actor::HewActor;

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

/// Requeue the actor whose activation is ending, behind the work it woke.
///
/// A budget-exhausted or yielding actor uses this instead of
/// [`sched_enqueue`]: a peer it woke during the turn runs before it does.
pub(crate) fn sched_requeue_self(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: the calling activation keeps the actor live while transferring
    // that lifetime to the queue entry.
    let entry = unsafe { SchedulerQueueEntry::retain(actor) };
    crate::platform::executor::publish_requeue_entry(entry);
}

/// Publish an entry whose queue reference the caller already owns.
pub(crate) fn sched_enqueue_owned(entry: SchedulerQueueEntry) {
    publish(entry);
}

pub(crate) use crate::activation::enqueue_resume_by_incarnation;

fn publish(entry: SchedulerQueueEntry) {
    crate::platform::executor::publish_queue_entry(entry);
}
