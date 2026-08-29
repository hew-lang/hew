//! Shared stub actors for the wake-edge tests.
//!
//! Every readiness source records its wake target while the target actor is
//! alive and fires it later, when the target may already be gone. The hazard
//! these helpers reproduce is ADDRESS reuse: an actor box freed and handed
//! straight back to the next spawn, so a wake recorded against the dead
//! incarnation resolves to a live, unrelated one.
//!
//! [`TrackedTestActor::reincarnate_parked`] makes that deterministic without an
//! allocator hook. It destroys the installed incarnation in place and
//! placement-constructs a fresh one, with a fresh identity, at the exact same
//! address, then re-tracks it. A wake recorded before the call and fired after
//! it names an address that is live again and belongs to somebody else - the
//! precise state a supervisor restart or a spawn-after-free produces in
//! production, minus the allocator's timing.

use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, AtomicU64, Ordering};

use crate::actor::{HewActor, HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET, HEW_PRIORITY_NORMAL};
use crate::internal::types::{ContTag, HewActorState};
use crate::lifetime::live_actors;
use crate::scheduler::NoWorkerSchedulerForTest;

/// Build a minimal `HewActor` with sensible defaults.
pub(crate) fn stub_actor() -> HewActor {
    HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: 1,
        state: ptr::null_mut(),
        state_size: 0,
        dispatch: None,
        mailbox: ptr::null_mut(),
        actor_state: AtomicI32::new(HewActorState::Runnable as i32),
        budget: AtomicI32::new(HEW_MSG_BUDGET),
        init_state: ptr::null_mut(),
        init_state_size: 0,
        coalesce_key_fn: None,
        terminate_fn: None,
        state_drop_fn: None,
        state_clone_fn: None,
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
        dispatch_active: AtomicBool::new(false),
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
        arena: ptr::null_mut(),
        suspended_cont: AtomicPtr::new(ptr::null_mut()),
        cont_tag: AtomicI32::new(ContTag::Empty as i32),
        pending_wake: AtomicBool::new(false),
        suspended_reply_channel: AtomicPtr::new(ptr::null_mut()),
        suspended_cancel_token: AtomicPtr::new(ptr::null_mut()),
        runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
        runtime: ptr::null(),
        send_pin_count: AtomicU32::new(0),
        gen_sink: AtomicPtr::new(ptr::null_mut()),
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        spawn_serial: 1,
        sys_dispatch: None,
        state_drop_consumed: AtomicBool::new(false),
        state_drop_borrowed: AtomicBool::new(false),
        parked_ask_channel: AtomicPtr::new(ptr::null_mut()),
    }
}

/// Distinct ids for stub actors so tests sharing a registry cannot collide.
fn next_stub_actor_id() -> u64 {
    static NEXT_ID: AtomicU64 = AtomicU64::new(1);
    NEXT_ID.fetch_add(1, Ordering::Relaxed)
}

/// A distinct `spawn_serial` per stub incarnation. Production allocates from
/// the same kind of monotonic counter and never reissues; the tests need the
/// same property so a reincarnation is provably a different incarnation.
fn next_stub_spawn_serial() -> u64 {
    static NEXT_SERIAL: AtomicU64 = AtomicU64::new(1);
    NEXT_SERIAL.fetch_add(1, Ordering::Relaxed)
}

/// A stub actor tracked in the current runtime's liveness registry for the
/// duration of a test, untracked and freed on drop.
///
/// The wake edges confirm liveness against the registry before waking, so a
/// wake-expecting test must present its stub as a LIVE actor.
pub(crate) struct TrackedTestActor {
    ptr: *mut HewActor,
}

impl TrackedTestActor {
    /// Track `actor` under a fresh identity.
    pub(crate) fn install(mut actor: HewActor) -> Self {
        actor.id = next_stub_actor_id();
        actor.spawn_serial = next_stub_spawn_serial();
        // Own the actor through a single raw pointer from `Box::into_raw`
        // rather than storing a `Box` alongside a derived raw pointer: under
        // Stacked Borrows, moving the `Box` (e.g. returning `Self`) retags and
        // invalidates the pointee, so the saved raw tag would be stale. The
        // `into_raw` pointer keeps valid provenance across the struct move and
        // `Drop` reconstitutes the `Box` to free it exactly once.
        let ptr: *mut HewActor = Box::into_raw(Box::new(actor));
        // SAFETY: `ptr` is a freshly-boxed, fully-initialised actor.
        assert!(unsafe { live_actors::track_actor(ptr) });
        Self { ptr }
    }

    /// Track a stub actor already parked: `Suspended`, with a non-null
    /// `suspended_cont` handle so a wake takes the direct-delivery arm rather
    /// than the mid-park pending-wake arm.
    pub(crate) fn install_parked() -> Self {
        let installed = Self::install(stub_actor());
        installed.park();
        installed
    }

    /// Put this actor into the parked shape a readiness source expects.
    fn park(&self) {
        self.actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        self.suspended_cont.store(
            ptr::null_mut::<u8>().wrapping_add(1).cast(),
            Ordering::Release,
        );
        self.cont_tag
            .store(ContTag::Parked as i32, Ordering::Release);
    }

    pub(crate) fn ptr(&self) -> *mut HewActor {
        self.ptr
    }

    /// Untrack the actor WITHOUT freeing the box, modelling a caller torn down
    /// before a late reply fires. After this returns, a wake for the actor must
    /// observe it as no longer live.
    pub(crate) fn untrack(&self) {
        live_actors::untrack_actor(self.ptr);
    }

    /// Destroy this incarnation and construct a fresh, parked one at the SAME
    /// address, tracked under a new identity.
    ///
    /// This is the deterministic form of the production race: a supervisor
    /// restart (or any spawn following a free) receiving the dead child's
    /// allocation back from the allocator.
    pub(crate) fn reincarnate_parked(&self) {
        // SAFETY: the guard owns this allocation exclusively for its lifetime.
        unsafe { reincarnate_parked_in_place(self.ptr) };
    }
}

/// Destroy the tracked incarnation at `actor` and construct a fresh, parked one
/// at the SAME address, tracked under a new identity.
///
/// # Safety
///
/// `actor` must be a tracked, fully initialised actor allocation the caller
/// owns exclusively (no other thread may be inside a pin on it).
pub(crate) unsafe fn reincarnate_parked_in_place(actor: *mut HewActor) {
    assert!(
        live_actors::untrack_actor(actor),
        "the incarnation being replaced must still be tracked"
    );

    let mut replacement = stub_actor();
    replacement.id = next_stub_actor_id();
    replacement.spawn_serial = next_stub_spawn_serial();
    replacement
        .actor_state
        .store(HewActorState::Suspended as i32, Ordering::Release);
    replacement.suspended_cont.store(
        ptr::null_mut::<u8>().wrapping_add(1).cast(),
        Ordering::Release,
    );
    replacement
        .cont_tag
        .store(ContTag::Parked as i32, Ordering::Release);

    // SAFETY: the old incarnation is untracked and exclusively owned by the
    // caller. Destroying it in place leaves the allocation owned and
    // uninitialised; the write immediately reinitialises it.
    unsafe {
        actor.drop_in_place();
        actor.write(replacement);
    }
    // SAFETY: the replacement is fully initialised at `actor` and has a fresh
    // identity distinct from the destroyed incarnation.
    assert!(unsafe { live_actors::track_actor(actor) });
}

impl std::ops::Deref for TrackedTestActor {
    type Target = HewActor;
    fn deref(&self) -> &HewActor {
        // SAFETY: `ptr` owns a live, boxed actor for the guard's lifetime.
        unsafe { &*self.ptr }
    }
}

impl Drop for TrackedTestActor {
    fn drop(&mut self) {
        // Idempotent: `untrack_actor` only removes a matching entry, so a
        // double-untrack (the test already called `untrack`) is a no-op.
        live_actors::untrack_actor(self.ptr);
        // SAFETY: `ptr` came from `Box::into_raw` in `install`; reclaim the box
        // so the actor is freed exactly once.
        unsafe { drop(Box::from_raw(self.ptr)) };
    }
}

/// Assert the actor at `victim`'s address was NOT resumed by a stale wake: it
/// stays parked, carries no pending-wake marker, and nothing reached the run
/// queue.
pub(crate) fn assert_not_woken(
    sched: &NoWorkerSchedulerForTest,
    victim: &TrackedTestActor,
    family: &str,
) {
    assert_eq!(
        victim.actor_state.load(Ordering::Acquire),
        HewActorState::Suspended as i32,
        "{family}: a wake registered by a dead incarnation must not resume the \
         actor that later occupies its address"
    );
    assert!(
        !crate::coro_exec::take_pending_wake(victim),
        "{family}: a stale wake must not leave a pending-wake marker on the \
         replacement either - the marker would fire on its next park"
    );
    assert_eq!(
        sched.pop_global(),
        None,
        "{family}: a stale wake must not enqueue the replacement"
    );
}

/// Assert the registering incarnation WAS resumed (the positive control).
pub(crate) fn assert_woken(
    sched: &NoWorkerSchedulerForTest,
    victim: &TrackedTestActor,
    family: &str,
) {
    assert_eq!(
        victim.actor_state.load(Ordering::Acquire),
        HewActorState::Runnable as i32,
        "{family}: the registering incarnation must be resumed by its own wake"
    );
    assert_eq!(
        sched.pop_global(),
        Some(victim.ptr()),
        "{family}: the woken actor must be enqueued exactly once"
    );
}
