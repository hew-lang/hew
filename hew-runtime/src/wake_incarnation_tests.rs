//! Wake-edge incarnation tests: one per readiness family (#3069).
//!
//! Every family here registers a wake target while the target actor is parked
//! and fires it later. The registration is an ADDRESS, and addresses are
//! recycled: the allocator hands a freed actor box straight back to the next
//! spawn, so a wake recorded by an actor that has since died can resolve to a
//! live, unrelated incarnation. The liveness probe on the wake edge cannot tell
//! the two apart - both are tracked-live at the same address - so the stale
//! wake moves a stranger `Suspended -> Runnable` with no readiness behind it,
//! and a suspending `select` resumed that way fabricates a timeout.
//!
//! [`TrackedTestActor::reincarnate_parked`] forces that interleaving with no
//! allocator hook and no threads: it destroys the registered incarnation in
//! place and constructs a fresh, parked one at the same address under a new
//! identity.
//!
//! Each family gets a pair. The negative asserts the replacement is untouched
//! after a stale wake; the positive control asserts the same edge DOES wake the
//! incarnation that actually registered, so the negative cannot be satisfied by
//! a wake path that simply drops everything.

use std::ptr;

use crate::scheduler::NoWorkerSchedulerForTest;
use crate::test_actor::{assert_not_woken, assert_woken, TrackedTestActor};

// ── timer / deadline (await_cancel) ──────────────────────────────────────────

/// Register the shared deadline arbiter against a parked actor, then fire the
/// timeout arm.
///
/// `reincarnate` decides whether the registering incarnation dies (and has its
/// address reused) before the timer fires.
fn run_timer_family(reincarnate: bool) {
    use crate::await_cancel::{
        hew_await_cancel_cancel, hew_await_cancel_free, hew_await_cancel_new, AwaitCancelStatus,
    };

    let sched = NoWorkerSchedulerForTest::install();
    let victim = TrackedTestActor::install_parked();

    // SAFETY: `victim` is live and tracked; the registration takes no ownership
    // of it.
    let reg = unsafe { hew_await_cancel_new(victim.ptr(), None, ptr::null_mut()) };

    if reincarnate {
        victim.reincarnate_parked();
    }

    // The timer wheel's deadline arm: settle the wait as TimedOut and wake.
    // SAFETY: `reg` is the live registration created above.
    let ran = unsafe {
        hew_await_cancel_cancel(
            reg,
            AwaitCancelStatus::TimedOut as i32,
            /* wake_actor */ 1,
        )
    };
    assert_eq!(ran, 1, "the timeout arm must win the one-shot arbiter");
    // SAFETY: releases the creator reference taken by `hew_await_cancel_new`.
    unsafe { hew_await_cancel_free(reg) };

    if reincarnate {
        assert_not_woken(&sched, &victim, "timer");
    } else {
        assert_woken(&sched, &victim, "timer");
    }
}

#[test]
fn timer_wake_does_not_resume_a_reused_address() {
    run_timer_family(true);
}

#[test]
fn timer_wake_resumes_the_registering_incarnation() {
    run_timer_family(false);
}

// ── channel (typed stream pipe) ──────────────────────────────────────────────

fn run_channel_family(reincarnate: bool) {
    use crate::channel_core::{ChannelCore, STREAM_AWAIT_SUSPEND};
    use crate::read_slot::{hew_read_slot_free, hew_read_slot_new};

    let sched = NoWorkerSchedulerForTest::install();
    let victim = TrackedTestActor::install_parked();

    let core = ChannelCore::new(4);
    let slot = hew_read_slot_new();
    // SAFETY: `victim` is live and tracked; `slot` is freshly created and the
    // test holds its creator reference.
    let parked = unsafe { core.await_next(victim.ptr(), slot) };
    assert_eq!(
        parked, STREAM_AWAIT_SUSPEND,
        "an empty, open pipe must park the consumer"
    );

    if reincarnate {
        victim.reincarnate_parked();
    }

    // A producer send is the readiness edge: it queues the item and wakes the
    // registered consumer.
    core.blocking_send(vec![1, 2, 3, 4]);

    if reincarnate {
        assert_not_woken(&sched, &victim, "channel");
    } else {
        assert_woken(&sched, &victim, "channel");
    }

    // SAFETY: releases the test's creator reference on the slot.
    unsafe { hew_read_slot_free(slot) };
}

#[test]
fn channel_wake_does_not_resume_a_reused_address() {
    run_channel_family(true);
}

#[test]
fn channel_wake_resumes_the_registering_incarnation() {
    run_channel_family(false);
}

// ── reply channel (suspending ask) ───────────────────────────────────────────

fn run_reply_family(reincarnate: bool) {
    use crate::reply_channel::{
        hew_reply, hew_reply_channel_free, hew_reply_channel_new, hew_reply_channel_retain,
        hew_reply_channel_set_parked_waiter,
    };

    let sched = NoWorkerSchedulerForTest::install();
    let victim = TrackedTestActor::install_parked();

    let ch = hew_reply_channel_new();
    // SAFETY: `ch` is live; `victim` is the actor whose continuation parks on
    // this ask.
    unsafe { hew_reply_channel_set_parked_waiter(ch, victim.ptr()) };
    // The callee side holds its own sender reference, consumed by `hew_reply`.
    // SAFETY: `ch` is live and the test holds the waiter reference.
    unsafe { hew_reply_channel_retain(ch) };

    if reincarnate {
        victim.reincarnate_parked();
    }

    let payload: i32 = 7;
    // SAFETY: `ch` is live with a sender reference outstanding; `payload` is a
    // live bit-copy value of the stated size.
    let delivered =
        unsafe { hew_reply(ch, (&raw const payload).cast_mut().cast(), size_of::<i32>()) };
    assert!(delivered, "the reply must be delivered to the channel");

    if reincarnate {
        assert_not_woken(&sched, &victim, "reply");
    } else {
        assert_woken(&sched, &victim, "reply");
    }

    // SAFETY: releases the test's waiter reference (the last one).
    unsafe { hew_reply_channel_free(ch) };
}

#[test]
fn reply_wake_does_not_resume_a_reused_address() {
    run_reply_family(true);
}

#[test]
fn reply_wake_resumes_the_registering_incarnation() {
    run_reply_family(false);
}

// ── task scope (await over a scope-owned child task) ─────────────────────────

fn run_scope_family(reincarnate: bool) {
    use crate::read_slot::{hew_read_slot_free, hew_read_slot_new};
    use crate::task_scope::{
        hew_task_await_suspend, hew_task_new, hew_task_scope_complete_task, hew_task_scope_destroy,
        hew_task_scope_new, hew_task_scope_spawn, TASK_AWAIT_SUSPEND,
    };

    let sched = NoWorkerSchedulerForTest::install();
    let victim = TrackedTestActor::install_parked();

    // SAFETY: the test owns every scope/task/slot pointer exclusively.
    unsafe {
        let scope = hew_task_scope_new();
        let task = hew_task_new();
        hew_task_scope_spawn(scope, task);

        let slot = hew_read_slot_new();
        let parked = hew_task_await_suspend(scope, task, victim.ptr(), slot);
        assert_eq!(
            parked, TASK_AWAIT_SUSPEND,
            "an outstanding child task must park the awaiting actor"
        );

        if reincarnate {
            victim.reincarnate_parked();
        }

        // The child completes: the observer deposits readiness and wakes.
        hew_task_scope_complete_task(scope, task);

        if reincarnate {
            assert_not_woken(&sched, &victim, "scope");
        } else {
            assert_woken(&sched, &victim, "scope");
        }

        hew_read_slot_free(slot);
        hew_task_scope_destroy(scope);
    }
}

#[test]
fn scope_wake_does_not_resume_a_reused_address() {
    run_scope_family(true);
}

#[test]
fn scope_wake_resumes_the_registering_incarnation() {
    run_scope_family(false);
}
