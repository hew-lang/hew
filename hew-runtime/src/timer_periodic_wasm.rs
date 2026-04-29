//! Cooperative periodic timer support for WASM targets.
//!
//! Periodic receive handlers are lowered to runtime calls that schedule
//! zero-payload self-sends. On wasm32 there is no background ticker thread, so
//! the host (or [`crate::scheduler_wasm::hew_wasm_sched_tick`]) drives this
//! queue explicitly.

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;

use crate::actor::{self, HewActor};
use crate::internal::types::HewActorState;
use crate::lifetime::PoisonSafe;
use crate::mailbox_wasm::HewMailboxWasm;

#[derive(Debug)]
struct PeriodicHandle {
    actor: *mut HewActor,
    next_fire_ms: u64,
}

// SAFETY: The cooperative WASM timer queue is single-threaded; tests serialize
// access with runtime_test_guard.
unsafe impl Send for PeriodicHandle {}
// SAFETY: The handle contains only immutable scalar metadata plus an actor ptr.
unsafe impl Sync for PeriodicHandle {}

#[derive(Debug)]
struct PeriodicEntry {
    next_fire_ms: u64,
    interval_ms: u64,
    actor: *mut HewActor,
    msg_type: i32,
    handle: *mut PeriodicHandle,
}

// SAFETY: The queue is driven by a single thread / serialized test harness.
unsafe impl Send for PeriodicEntry {}
// SAFETY: Queue entries are only mutated while held under PERIODIC_QUEUE.
unsafe impl Sync for PeriodicEntry {}

// WASM-TODO(#1451): replace this cooperative Vec-backed timer queue with the shared
// timer wheel backend used natively; insert/drain/cancel are still O(n)/O(n²).
static PERIODIC_QUEUE: PoisonSafe<Vec<PeriodicEntry>> = PoisonSafe::new(Vec::new());
static PERIODIC_REGISTRY: PoisonSafe<Option<HashMap<usize, Vec<usize>>>> = PoisonSafe::new(None);

fn register_timer(actor: *mut HewActor, handle: *mut PeriodicHandle) {
    PERIODIC_REGISTRY.access(|lock| {
        lock.get_or_insert_with(HashMap::new)
            .entry(actor as usize)
            .or_default()
            .push(handle as usize);
    });
}

fn unregister_timer(actor: *mut HewActor, handle: *mut PeriodicHandle) {
    PERIODIC_REGISTRY.access(|lock| {
        if let Some(map) = lock.as_mut() {
            if let Some(handles) = map.get_mut(&(actor as usize)) {
                handles.retain(|addr| *addr != handle as usize);
                if handles.is_empty() {
                    map.remove(&(actor as usize));
                }
            }
        }
    });
}

fn insert_sorted(entry: PeriodicEntry) {
    PERIODIC_QUEUE.access(|queue| {
        let pos = queue.partition_point(|existing| existing.next_fire_ms <= entry.next_fire_ms);
        queue.insert(pos, entry);
    });
}

/// Remove and return all periodic entries whose fire time has passed.
fn drain_due_entries(now_ms: u64) -> Vec<PeriodicEntry> {
    PERIODIC_QUEUE.access(|queue| {
        let mut ready = Vec::new();
        while queue
            .first()
            .is_some_and(|entry| entry.next_fire_ms <= now_ms)
        {
            ready.push(queue.remove(0));
        }
        ready
    })
}

unsafe fn free_handle(handle: *mut PeriodicHandle) {
    // SAFETY: caller guarantees `handle` was allocated by Box::into_raw and is unique here.
    let _ = unsafe { Box::from_raw(handle) };
}

fn current_time_ms() -> u64 {
    #[cfg(target_arch = "wasm32")]
    // SAFETY: hew_now_ms has no preconditions on wasm32.
    unsafe {
        crate::wasm_stubs::hew_now_ms()
    }

    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: io_time::hew_now_ms has no preconditions in native test builds.
    unsafe {
        crate::io_time::hew_now_ms()
    }
}

unsafe fn send_periodic_message(actor: *mut HewActor, msg_type: i32) {
    // SAFETY: caller guarantees `actor` is a live HewActor pointer.
    let a = unsafe { &*actor };
    // SAFETY: the mailbox belongs to `actor`, and wake_wasm_actor shares the same contract.
    unsafe {
        crate::mailbox_wasm::hew_mailbox_send(a.mailbox.cast(), msg_type, ptr::null_mut(), 0);
        actor::wake_wasm_actor(actor);
    }
}

fn actor_has_live_mailbox(actor: *mut HewActor) -> bool {
    if actor.is_null() {
        return false;
    }

    // SAFETY: actor nullability was checked above; reading actor_state is side-effect free.
    let state = unsafe { (*actor).actor_state.load(Ordering::Relaxed) };
    if state == HewActorState::Stopping as i32
        || state == HewActorState::Stopped as i32
        || state == HewActorState::Crashed as i32
    {
        return false;
    }

    // SAFETY: actor nullability was checked above; mailbox is a plain field read.
    let mailbox = unsafe { (*actor).mailbox.cast::<HewMailboxWasm>() };
    if mailbox.is_null() {
        return false;
    }

    // SAFETY: mailbox was read from a live actor and is only queried here.
    unsafe { !crate::mailbox_wasm::mailbox_is_closed(mailbox) }
}

pub(crate) fn pending_periodic_count() -> usize {
    PERIODIC_QUEUE.access(|queue| queue.len())
}

/// Deliver any due periodic messages and re-arm surviving timers.
///
/// Re-arming is based on the timer's previous scheduled fire time rather than
/// `now_ms` to avoid accumulating drift when the host drives the queue late.
pub(crate) unsafe fn drain_ready_periodic(now_ms: u64) -> u32 {
    let ready = drain_due_entries(now_ms);
    let mut fired = 0_u32;

    for mut entry in ready {
        if !actor_has_live_mailbox(entry.actor) {
            unregister_timer(entry.actor, entry.handle);
            // SAFETY: the timer was removed from both queue and registry and owns its handle.
            unsafe { free_handle(entry.handle) };
            continue;
        }

        let mut fire_time = entry.next_fire_ms;
        loop {
            // SAFETY: actor_has_live_mailbox verified the actor/mailbox are still valid.
            unsafe { send_periodic_message(entry.actor, entry.msg_type) };
            fired = fired.saturating_add(1);

            let Some(next_fire) = fire_time.checked_add(entry.interval_ms) else {
                fire_time = u64::MAX;
                break;
            };
            fire_time = next_fire;
            if fire_time > now_ms {
                break;
            }
        }

        entry.next_fire_ms = fire_time;
        // SAFETY: handle remains owned by this active entry while it stays queued.
        unsafe {
            (*entry.handle).next_fire_ms = fire_time;
        }
        insert_sorted(entry);
    }

    fired
}

/// Cancel all periodic timers registered for `actor`.
///
/// # Safety
///
/// `actor` must be a live actor pointer or one that is about to be freed.
pub(crate) unsafe fn cancel_all_timers_for_actor(actor: *mut HewActor) {
    let handles = PERIODIC_REGISTRY.access(|lock| {
        lock.as_mut()
            .and_then(|map| map.remove(&(actor as usize)))
            .unwrap_or_default()
    });

    if handles.is_empty() {
        return;
    }

    PERIODIC_QUEUE.access(|queue| {
        queue.retain(|entry| !ptr::eq(entry.actor, actor));
    });

    for handle in handles {
        // SAFETY: handles were removed from registry/queue above and are uniquely owned here.
        unsafe { free_handle(handle as *mut PeriodicHandle) };
    }
}

/// Schedule a periodic self-send on the cooperative WASM timer queue.
///
/// # Safety
///
/// `actor` must point to a live Hew actor.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_actor_schedule_periodic(
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
) -> *mut c_void {
    if actor.is_null() || interval_ms == 0 {
        return ptr::null_mut();
    }

    let next_fire_ms = current_time_ms().saturating_add(interval_ms);
    let handle = Box::into_raw(Box::new(PeriodicHandle {
        actor,
        next_fire_ms,
    }));

    register_timer(actor, handle);
    insert_sorted(PeriodicEntry {
        next_fire_ms,
        interval_ms,
        actor,
        msg_type,
        handle,
    });

    handle.cast()
}

/// Cancel a previously scheduled periodic timer.
///
/// # Safety
///
/// `handle` must be a pointer returned by [`hew_actor_schedule_periodic`].
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_actor_cancel_periodic(handle: *mut c_void) {
    if handle.is_null() {
        return;
    }

    let handle = handle.cast::<PeriodicHandle>();
    // SAFETY: caller guarantees `handle` came from hew_actor_schedule_periodic.
    let actor = unsafe { (*handle).actor };
    unregister_timer(actor, handle);

    PERIODIC_QUEUE.access(|queue| {
        queue.retain(|entry| !ptr::eq(entry.handle, handle));
    });

    // SAFETY: the handle has been removed from queue/registry above.
    unsafe { free_handle(handle) };
}

/// Clear the cooperative periodic queue.
///
/// # Safety
///
/// Must not race with timer queue mutation; the WASM scheduler is single-threaded.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_periodic_shutdown() {
    let handles = PERIODIC_QUEUE.access(|queue| {
        let handles = queue
            .iter()
            .map(|entry| entry.handle as usize)
            .collect::<Vec<_>>();
        queue.clear();
        handles
    });

    PERIODIC_REGISTRY.access(|registry| {
        *registry = None;
    });

    for handle in handles {
        // SAFETY: shutdown drained the queue, so each handle is uniquely owned here.
        unsafe { free_handle(handle as *mut PeriodicHandle) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, AtomicU64};

    use crate::actor::{HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET, HEW_PRIORITY_NORMAL};

    static NEXT_TEST_ACTOR_ID: AtomicU64 = AtomicU64::new(1);

    unsafe extern "C" fn count_dispatch(
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // SAFETY: tests install an AtomicU32 state pointer for every TestActor.
        let counter = unsafe { &*(state.cast::<AtomicU32>()) };
        counter.fetch_add(1, Ordering::Relaxed);
    }

    struct TestActor {
        actor: *mut HewActor,
        counter: *mut AtomicU32,
    }

    impl TestActor {
        fn new() -> Self {
            let counter = Box::into_raw(Box::new(AtomicU32::new(0)));
            // SAFETY: test mailbox constructor has no preconditions.
            let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast::<c_void>();
            let arena = crate::arena::hew_arena_new();
            let id = NEXT_TEST_ACTOR_ID.fetch_add(1, Ordering::Relaxed);
            let actor = Box::into_raw(Box::new(HewActor {
                sched_link_next: AtomicPtr::new(ptr::null_mut()),
                id,
                pid: id,
                state: counter.cast(),
                state_size: size_of::<AtomicU32>(),
                dispatch: Some(count_dispatch),
                mailbox,
                actor_state: AtomicI32::new(HewActorState::Idle as i32),
                budget: AtomicI32::new(HEW_MSG_BUDGET),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                coalesce_key_fn: None,
                terminate_fn: None,
                terminate_called: AtomicBool::new(false),
                terminate_finished: AtomicBool::new(false),
                error_code: AtomicI32::new(0),
                supervisor: ptr::null_mut(),
                supervisor_child_index: -1,
                priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
                reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
                idle_count: AtomicI32::new(0),
                hibernation_threshold: AtomicI32::new(0),
                hibernating: AtomicI32::new(0),
                prof_messages_processed: AtomicU64::new(0),
                prof_processing_time_ns: AtomicU64::new(0),
                arena: arena.cast(),
            }));
            Self { actor, counter }
        }

        fn count(&self) -> u32 {
            // SAFETY: TestActor owns `counter` for its full lifetime.
            unsafe { (*self.counter).load(Ordering::Relaxed) }
        }
    }

    impl Drop for TestActor {
        fn drop(&mut self) {
            // SAFETY: TestActor exclusively owns its actor, mailbox, arena, and counter allocations.
            unsafe {
                cancel_all_timers_for_actor(self.actor);
                crate::mailbox_wasm::hew_mailbox_free((*self.actor).mailbox.cast());
                crate::arena::hew_arena_free_all((*self.actor).arena.cast());
                let _ = Box::from_raw(self.counter);
                let _ = Box::from_raw(self.actor);
            }
        }
    }

    fn drive_scheduler() {
        loop {
            // SAFETY: tests serialize scheduler access and call this only after init.
            let remaining = unsafe { crate::scheduler_wasm::hew_wasm_sched_tick(16) };
            if remaining == 0 {
                break;
            }
        }
    }

    fn reset_runtime() {
        crate::scheduler_wasm::hew_sched_shutdown();
        // SAFETY: tests serialize timer-queue access with runtime_test_guard.
        unsafe { hew_periodic_shutdown() };
        crate::scheduler_wasm::hew_sched_init();
    }

    #[test]
    fn periodic_fires_after_interval() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        assert!(!handle.is_null());

        // SAFETY: handle was returned by hew_actor_schedule_periodic and is still active.
        let first_fire = unsafe { (*handle.cast::<PeriodicHandle>()).next_fire_ms };
        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_sub(1));
        }
        drive_scheduler();
        assert_eq!(actor.count(), 0, "periodic message must not fire early");

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire);
        }
        drive_scheduler();
        assert_eq!(actor.count(), 1, "periodic message must fire at deadline");
    }

    #[test]
    fn cancel_before_fire() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        // SAFETY: handle was returned by hew_actor_schedule_periodic and is still active.
        let first_fire = unsafe { (*handle.cast::<PeriodicHandle>()).next_fire_ms };

        // SAFETY: handle is active and owned by this test.
        unsafe { hew_actor_cancel_periodic(handle) };
        assert_eq!(
            pending_periodic_count(),
            0,
            "cancel must remove queued timer"
        );

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_add(20));
        };
        drive_scheduler();
        assert_eq!(actor.count(), 0, "cancelled timer must not fire");
    }

    #[test]
    fn cancel_after_fire_does_not_double_send() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        // SAFETY: handle was returned by hew_actor_schedule_periodic and is still active.
        let first_fire = unsafe { (*handle.cast::<PeriodicHandle>()).next_fire_ms };

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire);
        };
        drive_scheduler();
        assert_eq!(actor.count(), 1);

        // SAFETY: handle is active and owned by this test.
        unsafe { hew_actor_cancel_periodic(handle) };
        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_add(50));
        };
        drive_scheduler();
        assert_eq!(actor.count(), 1, "cancelled timer must not re-arm");
    }

    #[test]
    fn multiple_actors_each_periodic() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor_a = TestActor::new();
        let actor_b = TestActor::new();
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_a = unsafe { hew_actor_schedule_periodic(actor_a.actor, 1, 20) };
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_b = unsafe { hew_actor_schedule_periodic(actor_b.actor, 2, 35) };
        // SAFETY: both handles are active and owned by this test.
        let fire_a = unsafe { (*handle_a.cast::<PeriodicHandle>()).next_fire_ms };
        // SAFETY: both handles are active and owned by this test.
        let fire_b = unsafe { (*handle_b.cast::<PeriodicHandle>()).next_fire_ms };

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(fire_a);
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 1);
        assert_eq!(actor_b.count(), 0);

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(fire_b);
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 1);
        assert_eq!(actor_b.count(), 1);

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(fire_a.saturating_add(20));
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 2);
        assert_eq!(actor_b.count(), 1);
    }

    #[test]
    fn shutdown_clears_all_entries() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor_a = TestActor::new();
        let actor_b = TestActor::new();
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_a = unsafe { hew_actor_schedule_periodic(actor_a.actor, 1, 10) };
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_b = unsafe { hew_actor_schedule_periodic(actor_b.actor, 2, 15) };
        // SAFETY: both handles are active and owned by this test.
        let next_fire = unsafe {
            (*handle_a.cast::<PeriodicHandle>())
                .next_fire_ms
                .max((*handle_b.cast::<PeriodicHandle>()).next_fire_ms)
        };

        assert_eq!(pending_periodic_count(), 2);
        assert_eq!(crate::scheduler_wasm::hew_wasm_sleeping_count(), 2);

        crate::scheduler_wasm::hew_sched_shutdown();
        assert_eq!(
            pending_periodic_count(),
            0,
            "shutdown must clear periodic queue"
        );
        assert_eq!(crate::scheduler_wasm::hew_wasm_sleeping_count(), 0);

        crate::scheduler_wasm::hew_sched_init();
        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(next_fire.saturating_add(100));
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 0);
        assert_eq!(actor_b.count(), 0);
    }

    #[test]
    fn cancel_all_timers_for_actor_prevents_future_fires() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        // SAFETY: handle was returned by hew_actor_schedule_periodic and is still active.
        let first_fire = unsafe { (*handle.cast::<PeriodicHandle>()).next_fire_ms };

        // SAFETY: actor is still owned by this test and not freed until drop.
        unsafe { cancel_all_timers_for_actor(actor.actor) };
        assert_eq!(pending_periodic_count(), 0);

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_add(50));
        };
        drive_scheduler();
        assert_eq!(
            actor.count(),
            0,
            "actor cleanup must cancel periodic timers"
        );
    }
}
