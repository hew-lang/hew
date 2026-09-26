//! Actor unit tests (native target).
use super::*;
use crate::execution_context::TestExecutionContext;

struct SpawnPublicationHookGuard;

impl SpawnPublicationHookGuard {
    fn install(
        entered: std::sync::Arc<std::sync::Barrier>,
        release: std::sync::Arc<std::sync::Barrier>,
    ) -> Self {
        *SPAWN_PUBLICATION_HOOK
            .lock()
            .unwrap_or_else(PoisonError::into_inner) = Some((entered, release));
        Self
    }
}

impl Drop for SpawnPublicationHookGuard {
    fn drop(&mut self) {
        *SPAWN_PUBLICATION_HOOK
            .lock()
            .unwrap_or_else(PoisonError::into_inner) = None;
    }
}

static SEND_BY_ID_DISPATCH_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);
static ASK_SEND_BY_ID_DISPATCH_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);
static DRAIN_BUSY_LOOP_STARTED: AtomicBool = AtomicBool::new(false);
static DRAIN_BUSY_LOOP_RELEASE: AtomicBool = AtomicBool::new(false);
static DRAIN_TRAP_ON_STOP_STARTED: AtomicBool = AtomicBool::new(false);
/// Release flag for `drain_trap_on_stop_dispatch`: the dispatch holds
/// in `Running` state until the test sets this, guaranteeing that
/// `drain_actors` calls `hew_actor_stop` while the actor is still
/// `Running` (not yet `Idle`). Without this gate the dispatch could
/// finish before drain calls stop, causing the actor to transition
/// `Running → Idle → Stopped` instead of `Running → Crashed`, and drain
/// returns `Drained` instead of `Incomplete { crashed }`.
static DRAIN_TRAP_ON_STOP_RELEASE: AtomicBool = AtomicBool::new(false);

// Probes for `shutdown_sentinel_is_never_delivered_to_handler`.
static STOP_PROBE_STARTED: AtomicBool = AtomicBool::new(false);
static STOP_PROBE_RELEASE: AtomicBool = AtomicBool::new(false);
static STOP_PROBE_DISPATCHED_AFTER_STOP: AtomicBool = AtomicBool::new(false);

// Probe for `user_msg_type_minus_one_reaches_handler_and_does_not_terminate`.
static USER_MINUS_ONE_HANDLED: AtomicBool = AtomicBool::new(false);

// Probes for `user_queue_system_values_never_reach_the_system_dispatch`.
// Every former reserved value is sent on the USER queue; the user probe
// must see them all as ordinary application messages and the system probe
// must never fire.
static USER_PROBE_SEEN: std::sync::Mutex<Vec<i32>> = std::sync::Mutex::new(Vec::new());
static SYS_PROBE_SEEN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
static SYS_PROBE_LAST_KIND: AtomicI32 = AtomicI32::new(-1);

#[test]
fn panic_hook_filter_requires_typed_payload_and_live_boundary() {
    let typed = HewPanic { code: 101 };
    let ordinary = "ordinary Rust panic";

    assert!(
        !is_caught_hew_panic(&typed),
        "a typed payload without a catch boundary must remain visible"
    );
    let _ctx = TestExecutionContext::install(HewExecutionContext {
        flags: crate::execution_context::HEW_CTX_FLAG_UNWIND_BOUNDARY_INSTALLED,
        ..HewExecutionContext::default()
    });
    assert!(
        is_caught_hew_panic(&typed),
        "typed Hew control flow inside the scheduler boundary is silent"
    );
    assert!(
        !is_caught_hew_panic(&ordinary),
        "ordinary Rust panics must continue through the prior hook"
    );
}

unsafe extern "C-unwind" fn channel_split_user_probe(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    USER_PROBE_SEEN
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .push(msg_type);
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn channel_split_sys_probe(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    sys_msg: i32,
    _data: *mut c_void,
    _size: usize,
) -> *mut c_void {
    SYS_PROBE_LAST_KIND.store(sys_msg, Ordering::Release);
    SYS_PROBE_SEEN.fetch_add(1, Ordering::Release);
    ptr::null_mut()
}

/// NON-VACUITY companion to
/// `user_queue_system_values_never_reach_the_system_dispatch`: the system
/// entry point IS reachable, by its own route.
///
/// Without this, "no forged user-queue value reached system dispatch" would
/// pass just as well if system dispatch were unreachable altogether. The
/// same `Down` signal that a forged `hew_actor_send` cannot deliver arrives
/// here through the privileged system send — and stays on the system side.
#[test]
fn system_dispatch_is_reachable_only_by_the_system_channel() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    USER_PROBE_SEEN
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clear();
    SYS_PROBE_SEEN.store(0, Ordering::Release);
    SYS_PROBE_LAST_KIND.store(-1, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(channel_split_user_probe)) };
    assert!(!actor.is_null());
    // SAFETY: `actor` is the freshly spawned actor this test owns.
    unsafe { hew_actor_set_sys_dispatch(actor, Some(channel_split_sys_probe)) };

    let down = crate::monitor::HewDownMessage {
        monitor_id: 0,
        target_kind: 0,
        reason_kind: 0,
        node_hi: 0,
        node_lo: 0,
        slot: 0,
        session_incarnation: 0,
        crash_kind: 0,
    };
    // SAFETY: the actor is live, so its mailbox is valid; `down` outlives
    // the copying send.
    unsafe {
        let mb = (*actor).mailbox.cast::<mailbox::HewMailbox>();
        mailbox::mailbox_send_sys(
            mb,
            crate::mailbox_header::HewSysMsg::Down,
            (&raw const down).cast::<c_void>().cast_mut(),
            std::mem::size_of::<crate::monitor::HewDownMessage>(),
        );
        // Wake the actor so the queued system signal is drained. System
        // messages have dequeue priority, so this arrives after the Down.
        hew_actor_send(actor, 4242, ptr::null_mut(), 0);
    }

    wait_until(|| SYS_PROBE_SEEN.load(Ordering::Acquire) >= 1);
    assert_eq!(
        SYS_PROBE_SEEN.load(Ordering::Acquire),
        1,
        "a signal sent on the SYSTEM channel must reach the system dispatch entry point once"
    );
    assert_eq!(
        SYS_PROBE_LAST_KIND.load(Ordering::Acquire),
        crate::mailbox_header::HewSysMsg::Down.as_i32(),
        "the system handler must receive the typed discriminant it was sent"
    );
    wait_until(|| {
        !USER_PROBE_SEEN
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .is_empty()
    });
    assert_eq!(
        USER_PROBE_SEEN
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_slice(),
        [4242],
        "the user handler must see the application message and NOTHING else: \
         a SYSTEM-queue signal must never be downgraded onto it"
    );

    // SAFETY: actor is live and tracked; stop then free it exactly once.
    unsafe {
        hew_actor_stop(actor);
        let _ = hew_actor_free(actor);
    }
}

/// No value sent on the USER queue can reach the SYSTEM dispatch entry
/// point, and every such value is delivered to the user handler as an
/// ordinary application message.
///
/// This is the structural closure of the forged-EXIT defect. Before the
/// split there was ONE dispatch function and the scheduler handed it the
/// raw `msg_type` regardless of provenance, so `hew_actor_send(actor, 103,
/// null, 0)` — a legal public C-ABI call — arrived byte-for-byte as a
/// runtime-originated EXIT signal: the generated trampoline's EXIT arm
/// read a 16-byte `ExitMessage` out of the null payload (no `data_size`
/// guard existed) and called `hew_actor_exit_unhandled` with the loaded
/// reason. There is now no shared namespace to collide in: the system
/// entry point takes `HewSysMsg` discriminants and is reachable only from
/// the system queue.
#[test]
fn user_queue_system_values_never_reach_the_system_dispatch() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    USER_PROBE_SEEN
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clear();
    SYS_PROBE_SEEN.store(0, Ordering::Release);
    SYS_PROBE_LAST_KIND.store(-1, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(channel_split_user_probe)) };
    assert!(!actor.is_null());
    // SAFETY: `actor` is the freshly spawned actor this test owns.
    unsafe { hew_actor_set_sys_dispatch(actor, Some(channel_split_sys_probe)) };

    // Every `HewSysMsg` discriminant plus the whole former reserved block
    // (100..=105) and the former shutdown sentinel (-1), forged on the
    // public C ABI with a NULL payload and a zero size — the exact call
    // that produced the out-of-bounds read and the forged terminal Crashed.
    let forged: Vec<i32> = (0..=7)
        .chain(100..=105)
        .chain(std::iter::once(-1))
        .collect();
    for &msg_type in &forged {
        // SAFETY: actor is a valid live actor pointer returned by spawn.
        unsafe { hew_actor_send(actor, msg_type, ptr::null_mut(), 0) };
    }

    // Every user-queue send must reach the application handler, whatever
    // its value.
    wait_until(|| {
        USER_PROBE_SEEN
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .len()
            >= forged.len()
    });

    let mut seen = USER_PROBE_SEEN
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clone();
    seen.sort_unstable();
    let mut expected = forged.clone();
    expected.sort_unstable();
    assert_eq!(
        seen, expected,
        "the user handler must receive exactly the values sent, unfiltered"
    );

    assert_eq!(
        SYS_PROBE_SEEN.load(Ordering::Acquire),
        0,
        "a user-queue send reached the SYSTEM dispatch entry point (kind {})",
        SYS_PROBE_LAST_KIND.load(Ordering::Acquire)
    );

    // No forged send may terminate the actor.
    // SAFETY: actor remains tracked until the explicit free below.
    let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert!(
        state != HewActorState::Stopped as i32 && state != HewActorState::Crashed as i32,
        "a forged user-queue lifecycle value terminated the actor (state={state})"
    );

    // SAFETY: actor is live and tracked; stop then free it exactly once.
    unsafe {
        hew_actor_stop(actor);
        let _ = hew_actor_free(actor);
    }
}

/// The shutdown path still self-stops through the SYSTEM channel, and the
/// stop is observed structurally (`Origin::Sys(Shutdown)`) rather than by
/// comparing a value, so an actor that registers a system dispatch does not
/// see it either.
#[test]
fn shutdown_signal_stops_the_actor_and_bypasses_the_system_dispatch() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    SYS_PROBE_SEEN.store(0, Ordering::Release);
    SYS_PROBE_LAST_KIND.store(-1, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(channel_split_user_probe)) };
    assert!(!actor.is_null());
    // SAFETY: `actor` is the freshly spawned actor this test owns.
    unsafe { hew_actor_set_sys_dispatch(actor, Some(channel_split_sys_probe)) };

    // SAFETY: actor is live; stop enqueues the Shutdown signal.
    unsafe { hew_actor_stop(actor) };

    // The shutdown signal must drive the actor to a clean terminal Stopped.
    wait_until(|| {
        // SAFETY: actor remains tracked until the free below.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        state == HewActorState::Stopped as i32
    });
    assert_eq!(
        SYS_PROBE_SEEN.load(Ordering::Acquire),
        0,
        "the shutdown signal must be consumed by the scheduler, never handed \
             to a registered system dispatch"
    );

    // SAFETY: actor is tracked; free it exactly once.
    unsafe {
        let _ = hew_actor_free(actor);
    }
}

/// Handler that records receiving `msg_type == -1`. Used to prove that a
/// USER-queue message carrying the shutdown-sentinel VALUE is delivered
/// normally (the value is only reserved on the system queue).
unsafe extern "C-unwind" fn user_minus_one_probe_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type == -1 {
        USER_MINUS_ONE_HANDLED.store(true, Ordering::Release);
    }
    std::ptr::null_mut()
}

#[test]
fn user_msg_type_minus_one_reaches_handler_and_does_not_terminate() {
    // Regression guard for the provenance fix: `msg_type` is unrestricted in
    // the public C ABI and codegen tags are full-range hashes, so a USER send
    // of `-1` (WITHOUT a stop) is a real message. The scheduler's shutdown
    // interception gates on SYSTEM-queue provenance, so this must reach the
    // handler and must NOT terminate the actor. (Keying the interception on
    // the value alone silently dropped this message and stopped the actor.)
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    USER_MINUS_ONE_HANDLED.store(false, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(user_minus_one_probe_dispatch)) };
    assert!(!actor.is_null());

    // A USER-queue send (hew_actor_send routes to the user queue) with the
    // reserved sentinel value.
    // SAFETY: actor is a valid live actor pointer returned by spawn.
    unsafe { hew_actor_send(actor, -1, ptr::null_mut(), 0) };

    // A user-queue message with msg_type == -1 must reach the handler, not be intercepted as a
    // shutdown signal.
    wait_until(|| USER_MINUS_ONE_HANDLED.load(Ordering::Acquire));

    // The actor must still be alive — no spurious sentinel-driven self-stop.
    // SAFETY: actor remains tracked until the explicit free below.
    let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert!(
        state != HewActorState::Stopped as i32 && state != HewActorState::Crashed as i32,
        "delivering a user-queue msg_type == -1 must not terminate the actor (state={state})"
    );

    // SAFETY: actor is live and tracked; stop then free it exactly once.
    unsafe {
        hew_actor_stop(actor);
        let _ = hew_actor_free(actor);
    }
}

/// Handler that records every dispatch it receives. The FIRST message
/// parks it in `Running` so the stop lands on the Running branch; any
/// LATER dispatch means the scheduler kept feeding an actor that had
/// already been told to stop.
unsafe extern "C-unwind" fn stop_probe_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if STOP_PROBE_STARTED.swap(true, Ordering::AcqRel) {
        STOP_PROBE_DISPATCHED_AFTER_STOP.store(true, Ordering::Release);
        return std::ptr::null_mut();
    }
    // Hold in Running until the release thread observes the stop is
    // latched, so `hew_actor_stop` runs against a Running actor.
    while !STOP_PROBE_RELEASE.load(Ordering::Acquire) {
        std::hint::spin_loop();
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    std::ptr::null_mut()
}

/// (c) A stop requested while every `HewMsgNode` allocation FAILS is still
/// observed by the actor.
///
/// This replaces `shutdown_sentinel_is_never_delivered_to_handler`, whose
/// subject — a queued `msg_type == -1` node that must be intercepted before
/// it reaches the handler — no longer exists.
///
/// The defect it closes: `mailbox_send_stop_sys_once` allocated the sentinel
/// node BEFORE the `stop_signal_sent` CAS, so on allocation failure it
/// returned `false` with neither the node enqueued nor the flag set, and
/// `hew_actor_stop` discarded that `bool` (`let _ = ...`). Under memory
/// pressure a Running actor therefore never observed its own stop. Latching
/// an atomic bool has no such window, and this test proves it by poisoning
/// the mailbox allocator across the whole `hew_actor_stop` call.
#[test]
fn stop_of_running_actor_is_observed_even_when_node_allocation_fails() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    STOP_PROBE_STARTED.store(false, Ordering::Release);
    STOP_PROBE_RELEASE.store(false, Ordering::Release);
    STOP_PROBE_DISPATCHED_AFTER_STOP.store(false, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(stop_probe_dispatch)) };
    assert!(!actor.is_null());

    // Two messages: the first parks the handler in Running, the second sits
    // in the queue as evidence. If the stop were lost, the loop would come
    // back round and dispatch it.
    // SAFETY: actor is a valid live actor pointer returned by spawn.
    unsafe {
        hew_actor_send(actor, 1, ptr::null_mut(), 0);
        hew_actor_send(actor, 2, ptr::null_mut(), 0);
    }
    // Handler should begin running before the stop is issued.
    wait_until(|| STOP_PROBE_STARTED.load(Ordering::Acquire));

    // Release the dispatch spin only once the stop has actually been
    // latched, so the actor is stopped while Running.
    // SAFETY: the mailbox outlives the joined release thread.
    let mailbox_addr = unsafe { (*actor).mailbox } as usize;
    let release_handle = std::thread::spawn(move || {
        let mb = mailbox_addr as *mut HewMailbox;
        // SAFETY: `mb` stays valid until the test joins this thread.
        while !unsafe { mailbox::mailbox_stop_requested(mb) } {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        STOP_PROBE_RELEASE.store(true, Ordering::Release);
    });

    // Poison the allocator for the whole stop. `fail_mailbox_alloc_on_nth`
    // is thread-local and arms the NEXT allocation on THIS thread, which is
    // the thread `hew_actor_stop` runs on.
    let alloc_trap = mailbox::fail_mailbox_alloc_on_nth(0);
    // SAFETY: actor is live and Running.
    unsafe { hew_actor_stop(actor) };
    // Still armed => `hew_actor_stop` allocated nothing at all. The old
    // sentinel path would have consumed this and then dropped the request.
    assert!(
        mailbox::mailbox_alloc_failure_still_armed(),
        "hew_actor_stop must not allocate; the injected failure must survive it"
    );
    drop(alloc_trap);

    release_handle
        .join()
        .expect("release thread must not panic");

    // The stop was observed despite the poisoned allocator: the actor
    // reaches a clean terminal Stopped state...
    // A Running actor must observe its own stop even when node allocation fails.
    wait_until(|| {
        // SAFETY: actor remains tracked until the explicit free below.
        let s = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        s == HewActorState::Stopped as i32
    });
    // ...and the queued second message was never dispatched, because the
    // loop-top stop check ran before the receive.
    assert!(
        !STOP_PROBE_DISPATCHED_AFTER_STOP.load(Ordering::Acquire),
        "no message may be dispatched after the stop is latched"
    );

    // SAFETY: actor is terminal and still tracked; free it exactly once.
    unsafe {
        let _ = hew_actor_free(actor);
    }
}

#[test]
fn spawned_actor_direct_identity_retires_before_reclamation() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: runtime guard installs the owning liveness/handle authority;
    // null state with zero size and the test dispatch satisfy spawn.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn returned a live actor.
    let (actor_id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };
    assert_ne!(
        token,
        crate::lifetime::local_handles::HewLocalPidId::INVALID
    );
    assert_eq!(
        crate::lifetime::local_handles::resolve_current_actor(token),
        Some(actor_id)
    );
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (1, 1)
    );

    // SAFETY: actor is live, idle, and not used after successful free.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    assert_eq!(
        crate::lifetime::local_handles::resolve_current_actor(token),
        None
    );
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (0, 0)
    );
}

#[test]
fn duplicate_spawn_identity_preserves_original_liveness_and_route() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: runtime guard installs the owning authority; empty state is valid.
    let original = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!original.is_null());
    // SAFETY: original is live until the final free below.
    let (actor_id, token) = unsafe { ((*original).id, (*original).local_pid_id) };

    override_next_spawn_actor_id(actor_id);
    // SAFETY: the injected identity collision is handled before publication.
    let duplicate = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(duplicate.is_null());
    assert_eq!(live_actors::get_actor_ptr_by_id(actor_id), Some(original));
    assert_eq!(
        crate::lifetime::local_handles::resolve_current_actor(token),
        Some(actor_id)
    );
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (1, 1)
    );

    // SAFETY: original remains the tracked, idle allocation.
    assert_eq!(unsafe { hew_actor_free(original) }, 0);
}

#[test]
fn route_exhaustion_rolls_back_spawn_ownership_and_publication() {
    let _guard = crate::runtime_test_guard();
    crate::runtime::rt_current()
        .local_handles
        .fail_next_registration_for_test();
    let mut state = 37_u64;
    // SAFETY: state is readable for its exact size; injected exhaustion is
    // expected to release both copies, the mailbox, arena, and actor box.
    let actor = unsafe {
        hew_actor_spawn(
            (&raw mut state).cast(),
            std::mem::size_of::<u64>(),
            Some(noop_dispatch),
        )
    };
    assert!(actor.is_null());
    assert_eq!(live_actors::actor_count_for_test(), 0);
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (0, 0)
    );
}

#[test]
fn cleanup_waits_for_atomic_actor_publication() {
    let _guard = crate::runtime_test_guard();
    let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
    let release = std::sync::Arc::new(std::sync::Barrier::new(2));
    let _hook = SpawnPublicationHookGuard::install(
        std::sync::Arc::clone(&entered),
        std::sync::Arc::clone(&release),
    );

    let spawn = std::thread::spawn(|| {
        // SAFETY: the installed runtime is process-visible and empty state is valid.
        unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) as usize }
    });
    entered.wait();
    assert_eq!(live_actors::actor_count_for_test(), 0);
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (1, 1),
        "route reservation precedes liveness publication"
    );

    let cleanup_done = std::sync::Arc::new(AtomicBool::new(false));
    let cleanup_done_thread = std::sync::Arc::clone(&cleanup_done);
    let cleanup = std::thread::spawn(move || {
        // SAFETY: scheduler workers are absent under runtime_test_guard.
        unsafe { cleanup_all_actors() };
        cleanup_done_thread.store(true, Ordering::Release);
    });
    // Cleanup closes the publication gate and then waits for the held spawn.
    wait_until(|| !crate::lifetime::local_handles::current_publication_open_for_test());
    assert!(!cleanup_done.load(Ordering::Acquire));

    release.wait();
    assert_ne!(spawn.join().expect("spawn thread"), 0);
    cleanup.join().expect("cleanup thread");
    assert_eq!(live_actors::actor_count_for_test(), 0);
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (0, 0)
    );
}

#[test]
fn local_pid_operations_resolve_stable_actor_identity() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();
    // SAFETY: runtime guard installs the owning authority; empty state is valid.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn returned a live actor.
    let (expected_id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };

    let mut resolved_id = 0;
    assert_eq!(
        unsafe {
            // SAFETY: resolved_id is writable for the call.
            hew_local_pid_actor_id(token, &raw mut resolved_id)
        },
        0
    );
    assert_eq!(resolved_id, expected_id);
    // SAFETY: a null payload with size zero is readable by contract.
    assert_eq!(
        unsafe {
            // SAFETY: a null payload with size zero is readable by contract.
            hew_local_pid_send(token, 17, ptr::null_mut(), 0)
        },
        HewError::Ok as i32
    );

    wait_until(|| {
        // SAFETY: actor remains live until the free immediately below.
        (unsafe { (*actor).actor_state.load(Ordering::Acquire) }) == HewActorState::Idle as i32
    });
    // SAFETY: scheduler drained the message and actor is idle.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    assert_eq!(
        // SAFETY: resolved_id is writable for the call.
        unsafe { hew_local_pid_actor_id(token, &raw mut resolved_id) },
        HewError::ErrActorStopped as i32
    );
    // SAFETY: a null payload with size zero is readable by contract.
    assert_eq!(
        unsafe {
            // SAFETY: a null payload with size zero is readable by contract.
            hew_local_pid_send(token, 17, ptr::null_mut(), 0)
        },
        HewError::ErrActorStopped as i32
    );
    drop(runtime);
}

#[test]
fn local_pid_ask_uses_actor_identity_and_clears_error_slot() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();
    // SAFETY: null state and the reply dispatch form a valid actor spawn.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is live until teardown below.
    let token = unsafe { (*actor).local_pid_id };

    LAST_ACTOR_ASK_ERROR.with(|slot| slot.set(AskError::Timeout as i32));
    // SAFETY: a null payload with size zero is readable by contract.
    let reply = unsafe { hew_local_pid_ask(token, 1, ptr::null_mut(), 0) };
    assert!(!reply.is_null());
    // SAFETY: successful replies are malloc-allocated.
    unsafe { crate::mem::buf_free(reply) };
    assert_eq!(hew_actor_ask_take_last_error(), AskError::None as i32);

    // SAFETY: ask completed and actor is idle.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    drop(runtime);
}

#[test]
fn local_pid_ask_with_channel_failure_preserves_caller_reference() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null state and valid dispatch form a valid actor spawn.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is live until teardown below.
    let token = unsafe { (*actor).local_pid_id };
    // SAFETY: actor is live; close makes subsequent asks fail closed.
    unsafe { hew_actor_close(actor) };

    let before = reply_channel::active_channel_count();
    let ch = reply_channel::hew_reply_channel_new();
    assert_eq!(reply_channel::active_channel_count(), before + 1);
    // SAFETY: ch is caller-owned and payload is empty.
    let status = unsafe { hew_local_pid_ask_with_channel(token, 1, ptr::null_mut(), 0, ch.cast()) };
    assert_eq!(status, HewError::ErrActorStopped as i32);
    assert_eq!(
        reply_channel::active_channel_count(),
        before + 1,
        "failed token ask must preserve the caller-owned channel reference"
    );

    // SAFETY: release the preserved caller ref, then free the closed actor.
    unsafe {
        reply_channel::hew_reply_channel_free(ch);
        assert_eq!(hew_actor_free(actor), 0);
    }
    assert_eq!(reply_channel::active_channel_count(), before);
}

#[test]
fn actor_identity_pin_blocks_reclamation_until_guard_drop() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: empty state and no-op dispatch form a valid actor spawn.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor remains allocated while the pin is held.
    let actor_id = unsafe { (*actor).id };
    let pin = live_actors::pin_actor_by_id(actor_id).expect("live actor pin");

    // Free pauses after retiring the actor, immediately before its pin drain.
    let free_entered = std::sync::Arc::new(std::sync::Barrier::new(2));
    let free_release = std::sync::Arc::new(std::sync::Barrier::new(2));
    let _free_hook = install_free_post_retire_registration_hook_for_test(
        actor_id,
        std::sync::Arc::clone(&free_entered),
        std::sync::Arc::clone(&free_release),
    );
    let free_done = std::sync::Arc::new(AtomicBool::new(false));
    let free_done_thread = std::sync::Arc::clone(&free_done);
    let actor_addr = actor as usize;
    let free = std::thread::spawn(move || {
        // SAFETY: the actor remains pinned until the main thread releases it.
        let status = unsafe { hew_actor_free(actor_addr as *mut HewActor) };
        free_done_thread.store(true, Ordering::Release);
        status
    });

    free_entered.wait();
    free_release.wait();
    assert!(
        !free_done.load(Ordering::Acquire),
        "actor free must wait for the identity pin"
    );
    drop(pin);
    assert_eq!(free.join().expect("free thread"), 0);
    assert!(free_done.load(Ordering::Acquire));
}

/// `hew_actor_drain_set` resolves IDs before calling the raw-pointer stop
/// entry point. Prove that resolution takes an allocation pin under
/// `LIVE_ACTORS` and holds it until stop returns.
///
/// The free thread is paused after it has untracked the actor but before
/// its pin-drain loop. At that point the old implementation's unpinned raw
/// lookup left `send_pin_count == 0`: releasing free would reclaim the
/// allocation before drain dereferenced it. The production path must
/// instead expose exactly one drain pin. The free hook is also a safety
/// harness for that executable counterfactual: all observations are saved,
/// both threads are released and joined, and assertions run afterward, so
/// reverting only the pin produces a deterministic failure without
/// intentionally executing a use-after-free.
#[test]
fn drain_set_pins_target_across_lookup_stop_and_final_free() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state and no-op dispatch form a valid actor spawn.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: the actor is live and remains allocated until both test
    // rendezvous are released and the free thread is joined.
    let actor_id = unsafe { (*actor).id };

    let drain_entered = std::sync::Arc::new(std::sync::Barrier::new(2));
    let drain_release = std::sync::Arc::new(std::sync::Barrier::new(2));
    let _drain_hook = install_drain_post_pin_pre_stop_hook_for_test(
        actor_id,
        std::sync::Arc::clone(&drain_entered),
        std::sync::Arc::clone(&drain_release),
    );

    let free_entered = std::sync::Arc::new(std::sync::Barrier::new(2));
    let free_release = std::sync::Arc::new(std::sync::Barrier::new(2));
    let _free_hook = install_free_post_retire_registration_hook_for_test(
        actor_id,
        std::sync::Arc::clone(&free_entered),
        std::sync::Arc::clone(&free_release),
    );

    let (drain_done_tx, drain_done_rx) = std::sync::mpsc::channel();
    let drain = std::thread::spawn(move || {
        let ids = [actor_id];
        let mut outcome = DrainOutcomeRepr::default();
        // SAFETY: ids and outcome remain valid for this synchronous FFI
        // call; the timeout comfortably exceeds the test rendezvous.
        let status = unsafe {
            hew_actor_drain_set(ids.as_ptr(), ids.len(), 5_000_000_000, &raw mut outcome)
        };
        let observed = (status, outcome.still_live_len, outcome.crashed_len);
        // SAFETY: outcome was initialized by hew_actor_drain_set.
        unsafe { hew_actor_drain_outcome_free(&raw mut outcome) };
        drain_done_tx
            .send(observed)
            .expect("drain result receiver must remain live");
    });

    // Drain has resolved actor_id, incremented send_pin_count under
    // LIVE_ACTORS, and is paused immediately before hew_actor_stop.
    drain_entered.wait();

    let actor_addr = actor as usize;
    let (free_done_tx, free_done_rx) = std::sync::mpsc::channel();
    let free = std::thread::spawn(move || {
        // SAFETY: the drain pin keeps the allocation live until stop has
        // returned; the free path then owns final reclamation.
        let status = unsafe { hew_actor_free(actor_addr as *mut HewActor) };
        free_done_tx
            .send(status)
            .expect("free result receiver must remain live");
    });

    // Free has latched the actor terminal and removed it from LIVE_ACTORS,
    // but is paused before it can wait on or reclaim the drain pin.
    free_entered.wait();
    let retired_before_stop = !live_actors::is_actor_live_with_id(actor_id, actor);
    // SAFETY: free is blocked at the post-retire hook, so the allocation is
    // still live even in the counterfactual where the drain pin is absent.
    let pin_while_retired = unsafe { (*actor).send_pin_count.load(Ordering::Acquire) };
    let drain_blocked_before_release = matches!(
        drain_done_rx.try_recv(),
        Err(std::sync::mpsc::TryRecvError::Empty)
    );
    let free_blocked_before_release = matches!(
        free_done_rx.try_recv(),
        Err(std::sync::mpsc::TryRecvError::Empty)
    );

    // Let drain perform its sole raw-pointer dereference and release the
    // pin. Free remains paused, making the post-stop count safe to inspect.
    drain_release.wait();
    let drain_result = drain_done_rx
        .recv_timeout(std::time::Duration::from_secs(5))
        .expect("drain_set must finish after its stop pin is released");
    drain.join().expect("drain thread");
    // SAFETY: free is still blocked before its pin-drain/finalize sequence.
    let pin_after_stop = unsafe { (*actor).send_pin_count.load(Ordering::Acquire) };

    free_release.wait();
    let free_result = free_done_rx
        .recv_timeout(std::time::Duration::from_secs(5))
        .expect("free must finish after the drain pin reaches zero");
    free.join().expect("free thread");

    assert!(
        retired_before_stop,
        "free must reach the final untracked window while drain is paused"
    );
    assert_eq!(
        pin_while_retired, 1,
        "the drain target must own exactly one allocation pin across stop"
    );
    assert!(
        drain_blocked_before_release,
        "drain_set must remain paused before the raw-pointer stop"
    );
    assert!(
        free_blocked_before_release,
        "free must not complete while the drain pin is still owned"
    );
    assert_eq!(
        drain_result,
        (0, 0, 0),
        "retired actor must resolve to a successful drained outcome"
    );
    assert_eq!(
        pin_after_stop, 0,
        "drain must release its allocation pin exactly once after stop"
    );
    assert_eq!(free_result, 0, "final actor free must succeed");
}

static DRAIN_CLEANUP_FINALIZE_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

fn drain_one_actor_via_ffi(actor_id: ActorId) -> (i32, usize, usize) {
    let ids = [actor_id];
    let mut outcome = DrainOutcomeRepr::default();
    // SAFETY: ids and outcome remain valid for this synchronous call.
    let status =
        unsafe { hew_actor_drain_set(ids.as_ptr(), ids.len(), 5_000_000_000, &raw mut outcome) };
    let observed = (status, outcome.still_live_len, outcome.crashed_len);
    // SAFETY: outcome was initialized by hew_actor_drain_set.
    unsafe { hew_actor_drain_outcome_free(&raw mut outcome) };
    observed
}

fn spawn_stateful_noop_actor() -> *mut HewActor {
    let mut initial_state = 0_u8;
    // SAFETY: the one-byte source remains valid for this synchronous deep
    // copy, and no-op dispatch is a valid actor entry point.
    unsafe {
        hew_actor_spawn(
            (&raw mut initial_state).cast(),
            std::mem::size_of_val(&initial_state),
            Some(noop_dispatch),
        )
    }
}

fn count_drain_cleanup_finalize(_actor: *mut HewActor) {
    DRAIN_CLEANUP_FINALIZE_COUNT.fetch_add(1, Ordering::AcqRel);
}

/// Prove that a drain's quiescent state observation retains exact actor
/// lifetime through cleanup's first dereference and retirement claim.
///
/// The rendezvous is the old unlock-to-prepare gap: the drain has observed
/// `Stopped`, but has not entered `prepare_quiescent_actor_for_cleanup`.
/// A concurrent free then retires the actor. Without the carried pin, the
/// actor can be finalized while drain still holds the stale raw pointer.
/// The post-retire free hook keeps the counterfactual executable without
/// allowing that UAF; the test records all proof values, releases and joins
/// both threads, then asserts the carried pin, exact release, and one final
/// cleanup.
#[test]
fn drain_set_pins_quiescent_state_into_cleanup_claim() {
    let _guard = crate::runtime_test_guard();

    let actor = spawn_stateful_noop_actor();
    assert!(!actor.is_null());
    // SAFETY: the actor remains live through the coordinated teardown.
    let actor_id = unsafe { (*actor).id };
    TERMINATE_CALL_COUNT.store(0, Ordering::Release);
    DRAIN_CLEANUP_FINALIZE_COUNT.store(0, Ordering::Release);
    // SAFETY: the actor is live and solely controlled by this test.
    unsafe { hew_actor_set_terminate(actor, counting_terminate_callback) };
    set_pre_queue_destroy_hook_for_test(Some(count_drain_cleanup_finalize));

    let state_entered = std::sync::Arc::new(std::sync::Barrier::new(2));
    let state_release = std::sync::Arc::new(std::sync::Barrier::new(2));
    let _state_hook = install_drain_post_state_pre_cleanup_hook_for_test(
        actor_id,
        std::sync::Arc::clone(&state_entered),
        std::sync::Arc::clone(&state_release),
    );

    let free_entered = std::sync::Arc::new(std::sync::Barrier::new(2));
    let free_release = std::sync::Arc::new(std::sync::Barrier::new(2));
    let _free_hook = install_free_post_retire_registration_hook_for_test(
        actor_id,
        std::sync::Arc::clone(&free_entered),
        std::sync::Arc::clone(&free_release),
    );

    let (drain_done_tx, drain_done_rx) = std::sync::mpsc::channel();
    let drain = std::thread::spawn(move || {
        drain_done_tx
            .send(drain_one_actor_via_ffi(actor_id))
            .expect("drain result receiver must remain live");
    });

    // Initial stop has completed; drain now owns the state-to-cleanup pin.
    state_entered.wait();
    // SAFETY: the drain pin keeps this allocation live at the rendezvous.
    let pin_at_state_handoff = unsafe { (*actor).send_pin_count.load(Ordering::Acquire) };

    let actor_addr = actor as usize;
    let (free_done_tx, free_done_rx) = std::sync::mpsc::channel();
    let free = std::thread::spawn(move || {
        // SAFETY: drain's carried pin keeps the actor allocated until free
        // wins retirement and later observes that pin reach zero.
        let status = unsafe { hew_actor_free(actor_addr as *mut HewActor) };
        free_done_tx
            .send(status)
            .expect("free result receiver must remain live");
    });

    // Free owns final retirement but cannot reclaim across drain's first
    // cleanup dereference or retirement-claim attempt.
    free_entered.wait();
    let retired_before_cleanup = !live_actors::is_actor_live_with_id(actor_id, actor);
    // SAFETY: free is paused after retirement and before pin drain/finalize.
    let pin_while_retired = unsafe { (*actor).send_pin_count.load(Ordering::Acquire) };
    let drain_blocked_before_release = matches!(
        drain_done_rx.try_recv(),
        Err(std::sync::mpsc::TryRecvError::Empty)
    );
    let free_blocked_before_release = matches!(
        free_done_rx.try_recv(),
        Err(std::sync::mpsc::TryRecvError::Empty)
    );

    state_release.wait();
    let drain_result = drain_done_rx
        .recv_timeout(std::time::Duration::from_secs(5))
        .expect("drain must yield to the winning final freer");
    drain.join().expect("drain thread");
    // SAFETY: free remains blocked at its post-retirement proof hook.
    let pin_after_cleanup_handoff = unsafe { (*actor).send_pin_count.load(Ordering::Acquire) };

    free_release.wait();
    let free_result = free_done_rx
        .recv_timeout(std::time::Duration::from_secs(5))
        .expect("free must complete after the cleanup pin is released");
    free.join().expect("free thread");

    let terminate_count = TERMINATE_CALL_COUNT.load(Ordering::Acquire);
    let finalize_count = DRAIN_CLEANUP_FINALIZE_COUNT.load(Ordering::Acquire);
    set_pre_queue_destroy_hook_for_test(None);

    assert_eq!(
        pin_at_state_handoff, 1,
        "quiescent state must be carried by exactly one allocation pin"
    );
    assert!(
        retired_before_cleanup,
        "the final free must retire the actor in the old state-to-prepare gap"
    );
    assert_eq!(
        pin_while_retired, 1,
        "retirement must retain the drain's state-to-cleanup pin"
    );
    assert!(
        drain_blocked_before_release,
        "drain must remain paused before cleanup's first dereference"
    );
    assert!(
        free_blocked_before_release,
        "free must not reclaim while the cleanup handoff pin is owned"
    );
    assert_eq!(drain_result, (0, 0, 0));
    assert_eq!(
        pin_after_cleanup_handoff, 0,
        "the losing drain must release its caller pin exactly once"
    );
    assert_eq!(free_result, 0, "the retirement winner must finalize");
    assert_eq!(
        terminate_count, 1,
        "stop/free composition must invoke terminate exactly once"
    );
    assert_eq!(
        finalize_count, 1,
        "exactly one path may reach actor resource finalization"
    );
}

#[test]
fn actor_cleanup_drains_every_direct_identity() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: runtime guard installs the authority and both spawns use
    // empty test state.
    let first = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: same runtime and empty-state preconditions as the first spawn.
    let second = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!first.is_null() && !second.is_null());
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (2, 2)
    );

    // SAFETY: no scheduler workers or dispatches exist under the test guard.
    unsafe { cleanup_all_actors() };
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (0, 0)
    );
}

#[test]
fn take_by_actor_id_retires_exact_direct_identity() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: runtime guard installs the authority; empty state satisfies spawn.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn returned a live actor.
    let (actor_id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };
    // SAFETY: the fresh actor is idle and has no external registrations.
    unsafe { prepare_quiescent_actor_for_cleanup(actor) };
    // SAFETY: actor remains live through the latch decision.
    let finalize_state = match decide_finalize_by_latch(unsafe { &*actor }) {
        FinalizeDecision::Finalize(state) => state,
        FinalizeDecision::Skip => panic!("fresh idle actor must be finalizable"),
    };

    assert_eq!(live_actors::take_actor_by_id(actor_id, actor), Some(actor));
    assert_eq!(
        crate::lifetime::local_handles::resolve_current_actor(token),
        None
    );
    assert_eq!(
        crate::lifetime::local_handles::current_counts_for_test(),
        (0, 0)
    );
    // SAFETY: the actor is retired and has no pins.
    unsafe { scrub_actor_relationships_after_pin_drain(actor) };
    // SAFETY: actor is wake-proof, untracked, unpinned, and test-owned.
    unsafe { finalize_quiescent_actor_cleanup(actor, finalize_state) };
}

/// With no execution context installed, the diagnostic accessor
/// `hew_actor_current_id` writes `EXECUTION_CONTEXT_NOT_INSTALLED` into the
/// generic last-error slot (callers treating an absent context as a failure
/// depend on that), while `hew_actor_current_id_silent` returns the same -1
/// without touching the slot — it is a routing probe, not a diagnostic.
#[test]
fn silent_probe_diverges_from_diagnostic_on_missing_context() {
    let prev = crate::execution_context::set_current_context(ptr::null_mut());

    crate::hew_clear_error();
    assert_eq!(hew_actor_current_id_silent(), -1);
    assert!(
        crate::hew_last_error().is_null(),
        "silent probe must not write the generic last-error slot"
    );

    assert_eq!(hew_actor_current_id(), -1);
    let err = crate::hew_last_error();
    assert!(
        !err.is_null(),
        "diagnostic accessor must record the missing-context error"
    );
    // SAFETY: hew_last_error returned a non-null, NUL-terminated C string
    // owned by the thread-local slot; it stays valid until the next write.
    let msg = unsafe { std::ffi::CStr::from_ptr(err) }
        .to_str()
        .expect("last-error message is valid UTF-8");
    assert_eq!(
        msg,
        crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
    );

    crate::hew_clear_error();
    let _ = crate::execution_context::set_current_context(prev);
}

/// `Suspended` is non-quiescent: a suspended actor owns a live continuation
/// frame, so a `hew_actor_free` caller spinning on the state must block
/// through the `Suspended` window rather than freeing the box out from
/// under the parked continuation (R7 / `cleanup-all-exits`). The only
/// quiescent states are the truly idle/terminal ones.
#[test]
fn suspended_state_is_not_quiescent() {
    assert!(
        !actor_free_state_is_quiescent(HewActorState::Suspended as i32),
        "Suspended owns a live frame and must block actor_free, like Sleeping/Crashing"
    );
    // The quiescent set is exactly Idle/Stopped/Crashed; no live-frame or
    // in-flight state may leak into it.
    assert!(actor_free_state_is_quiescent(HewActorState::Idle as i32));
    assert!(actor_free_state_is_quiescent(HewActorState::Stopped as i32));
    assert!(actor_free_state_is_quiescent(HewActorState::Crashed as i32));
    assert!(!actor_free_state_is_quiescent(
        HewActorState::Running as i32
    ));
    assert!(!actor_free_state_is_quiescent(
        HewActorState::Runnable as i32
    ));
    assert!(!actor_free_state_is_quiescent(
        HewActorState::Sleeping as i32
    ));
    assert!(!actor_free_state_is_quiescent(
        HewActorState::Crashing as i32
    ));
    assert!(!actor_free_state_is_quiescent(
        HewActorState::Stopping as i32
    ));
}

struct NativeSchedulerGuard;

impl NativeSchedulerGuard {
    fn new() -> Self {
        // Retire any `runtime_test_guard()` worker-less placeholder, then
        // install a real worker-backed scheduler (see
        // `init_real_scheduler_for_test`). This guard's `Drop` tears the
        // real runtime down symmetrically (`hew_sched_shutdown` +
        // `hew_runtime_cleanup`), so its workers are joined before free.
        crate::scheduler::init_real_scheduler_for_test();
        Self
    }
}

impl Drop for NativeSchedulerGuard {
    fn drop(&mut self) {
        crate::scheduler::hew_sched_shutdown();
        crate::scheduler::hew_runtime_cleanup();
    }
}

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn count_send_by_id_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    SEND_BY_ID_DISPATCH_COUNT.fetch_add(1, Ordering::AcqRel);

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn count_ask_send_by_id_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    ASK_SEND_BY_ID_DISPATCH_COUNT.fetch_add(1, Ordering::AcqRel);
    let ch = crate::execution_context::hew_get_reply_channel();
    if ch.is_null() {
        return std::ptr::null_mut();
    }
    let mut value: i32 = 7;
    // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
    // and `value` lives for the duration of the call.
    unsafe {
        let _ = crate::reply_channel::hew_reply(
            ch.cast(),
            (&raw mut value).cast(),
            std::mem::size_of::<i32>(),
        );
    }

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn drain_busy_loop_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    DRAIN_BUSY_LOOP_STARTED.store(true, Ordering::Release);
    while !DRAIN_BUSY_LOOP_RELEASE.load(Ordering::Acquire) {
        std::hint::spin_loop();
        std::thread::sleep(std::time::Duration::from_millis(1));
    }

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn drain_trap_on_stop_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    DRAIN_TRAP_ON_STOP_STARTED.store(true, Ordering::Release);
    // Hold in Running until the test's release thread observes that
    // drain_actors has called hew_actor_stop (the shutdown system message is
    // queued). This prevents the dispatch from finishing before the stop is
    // requested, which would let the actor transition Running→Idle→Stopped
    // instead of crashing mid-drain and yield Drained rather than
    // Incomplete{crashed}.
    while !DRAIN_TRAP_ON_STOP_RELEASE.load(Ordering::Acquire) {
        std::hint::spin_loop();
        std::thread::sleep(std::time::Duration::from_millis(1));
    }

    // Crash from within this still-Running dispatch, modelling an actor that
    // faults as it is being drained. The crash trigger is a self-trap rather
    // than observing the stop: the stop is an out-of-band flag the scheduler
    // reads at loop top, so it never becomes a dispatch a handler could
    // react to.
    // SAFETY: this runs on the actor's own dispatch thread while its context is installed.
    unsafe { hew_actor_trap(hew_actor_self(), 77) };

    std::ptr::null_mut()
}

/// Poll `condition` until it holds. The pass condition is the event itself;
/// the test runner's timeout is the only hang guard.
fn wait_until(mut condition: impl FnMut() -> bool) {
    while !condition() {
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

/// A drain returns as soon as its actors settle, so these tests pass it a
/// deadline no passing run reaches; the test runner's timeout is the hang
/// guard.
fn unbounded_drain_deadline() -> std::time::Instant {
    std::time::Instant::now() + std::time::Duration::from_hours(24)
}

fn wait_for_actor_quiescent(actor: *mut HewActor) {
    wait_until(|| {
        // SAFETY: tests only call this while the actor allocation is still live.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        actor_free_state_is_quiescent(state)
    });
}

unsafe extern "C-unwind" fn native_self_stop_without_reply_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    hew_actor_self_stop();

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn native_reply_once_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    let ch = crate::execution_context::hew_get_reply_channel();
    if ch.is_null() {
        return std::ptr::null_mut();
    }
    let mut value: i32 = 21;
    // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
    // and `value` lives for the duration of the call.
    unsafe {
        let _ = crate::reply_channel::hew_reply(
            ch.cast(),
            (&raw mut value).cast(),
            std::mem::size_of::<i32>(),
        );
    }

    std::ptr::null_mut()
}

/// Holds `native_late_reply_dispatch` shut until the test has observed its
/// ask time out, so the reply is late by construction rather than by how
/// promptly the host schedules the handler thread.
static LATE_REPLY_GATE: (std::sync::Mutex<bool>, std::sync::Condvar) =
    (std::sync::Mutex::new(false), std::sync::Condvar::new());

fn close_late_reply_gate() {
    *LATE_REPLY_GATE
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = false;
}

fn open_late_reply_gate() {
    *LATE_REPLY_GATE
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = true;
    LATE_REPLY_GATE.1.notify_all();
}

unsafe extern "C-unwind" fn native_late_reply_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    let mut open = LATE_REPLY_GATE
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while !*open {
        open = LATE_REPLY_GATE
            .1
            .wait(open)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
    }
    drop(open);
    let ch = crate::execution_context::hew_get_reply_channel();
    if ch.is_null() {
        return std::ptr::null_mut();
    }
    let mut value: i32 = 99;
    // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
    // and `value` lives for the duration of the call.
    unsafe {
        let _ = crate::reply_channel::hew_reply(
            ch.cast(),
            (&raw mut value).cast(),
            std::mem::size_of::<i32>(),
        );
    }

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn native_reply_then_trap_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    let ch = crate::execution_context::hew_get_reply_channel();
    if ch.is_null() {
        return std::ptr::null_mut();
    }
    let mut value: i32 = 123;
    // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
    // and `value` lives for the duration of the call.
    unsafe {
        let _ = crate::reply_channel::hew_reply(
            ch.cast(),
            (&raw mut value).cast(),
            std::mem::size_of::<i32>(),
        );
    }
    hew_panic();

    std::ptr::null_mut()
}

fn make_stop_test_actor(initial_state: HewActorState) -> (*mut HewActor, *mut HewMailbox) {
    make_stop_test_actor_with_id(1, initial_state)
}

fn make_stop_test_actor_with_id(
    id: u64,
    initial_state: HewActorState,
) -> (*mut HewActor, *mut HewMailbox) {
    // SAFETY: test helper fully owns the returned actor/mailbox and never publishes them.
    unsafe {
        let mailbox = mailbox::hew_mailbox_new();
        assert!(!mailbox.is_null());
        let actor = Box::into_raw(Box::new(HewActor {
            dispatch_ownership: crate::actor::HewDispatchOwnership::CopiedPayload,
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: mailbox.cast(),
            actor_state: AtomicI32::new(initial_state as i32),
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
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
            send_pin_count: AtomicU32::new(0),
            gen_sink: AtomicPtr::new(ptr::null_mut()),
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
            spawn_serial: id,
            sys_dispatch: None,
            state_drop_consumed: AtomicBool::new(false),
            state_drop_borrowed: AtomicBool::new(false),
            parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
            checked_invocation: AtomicPtr::new(std::ptr::null_mut()),
            pending_external_trap_code: AtomicI32::new(0),
            native_completion: None,
        }));
        (actor, mailbox)
    }
}

/// Deterministic #2831 ownership witness for the idle-stop half.
///
/// The send rendezvous fires AFTER the ask node (and its retained
/// sender-side channel reference) transfers into the mailbox, but BEFORE
/// the sender attempts `Idle -> Runnable`. The test then lets stop win
/// `Idle -> Stopped`.
///
/// The first case is the exact pre-fix counterfactual: it executes the whole
/// direct-idle stop path while omitting only the new terminal mailbox
/// reclaim. The same node pointer remains registered, the same channel
/// remains not-ready with both refs, and a zero-deadline wait returns only as
/// a timeout. The second case executes production stop and proves that exact
/// node disappears, the queued sender ref is consumed, and the channel is
/// ready+orphaned before the paused sender even attempts its doomed wake CAS.
#[test]
#[expect(
    clippy::too_many_lines,
    clippy::undocumented_unsafe_blocks,
    reason = "the deterministic FFI ownership witness keeps each unsafe assertion beside the exact lifecycle phase it proves"
)]
fn idle_stop_retires_ask_enqueued_before_sender_wake_cas() {
    struct AskSubmission {
        actor: *mut HewActor,
        ch: *mut HewReplyChannel,
    }
    // SAFETY: each actor outlives its joined sender thread; the thread uses
    // both pointers only through the held-pointer ask ABI, and the test
    // retains the creator-side channel reference until after the join.
    unsafe impl Send for AskSubmission {}
    impl AskSubmission {
        unsafe fn submit(self) -> i32 {
            // SAFETY: upheld by the caller; this method exists so the
            // closure captures the Send wrapper as a whole, rather than
            // disjoint-capturing its raw-pointer fields.
            unsafe { ask_with_channel_pinned(self.actor, 1, ptr::null_mut(), 0, self.ch.cast()) }
        }
    }

    unsafe fn run_case(reclaim_queued: bool, close_instead_of_stop: bool) {
        static NEXT_ID: AtomicU64 = AtomicU64::new(28_310_000);

        let frame_baseline = crate::observe::coroutine_snapshot();

        let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
        let (actor, mb) = make_stop_test_actor_with_id(id, HewActorState::Idle);
        // SAFETY: the helper returned a fully initialized actor with a
        // unique id; tracking owns no allocation reference.
        assert!(unsafe { live_actors::track_actor(actor) });
        assert!(live_actors::is_actor_live_with_id(id, actor));

        let ch = reply_channel::hew_reply_channel_new();
        assert!(!ch.is_null());
        let (hook, entered, release) = SendPostEnqueueHookGuard::install(id);
        let submission = AskSubmission { actor, ch };
        let sender = std::thread::spawn(move || {
            // SAFETY: target stays live/tracked until this thread joins;
            // null payload of size zero is valid and ch remains creator-owned
            // by the test until after the join.
            unsafe { submission.submit() }
        });

        // Sender is now paused after enqueue and before wake.
        entered.wait();
        // SAFETY: the creator reference keeps ch live.
        assert_eq!(
            unsafe { reply_channel::ref_count_for_test(ch) },
            2,
            "creator + exact queued-node sender reference"
        );
        let exact_node = mailbox::ask_node_for_reply_channel_for_test(ch.cast::<c_void>());
        assert!(
            !exact_node.is_null(),
            "the queued ask node is identity-tracked"
        );
        // SAFETY: mailbox is live and the no-worker scheduler gives this
        // test exclusive consumer-side control.
        assert_eq!(unsafe { mailbox::hew_mailbox_len(mb) }, 1);

        if reclaim_queued {
            // Production edge: closes, wins Idle -> Stopped, then retires
            // the queued node before returning.
            if close_instead_of_stop {
                unsafe { hew_actor_close(actor) };
            } else {
                unsafe { hew_actor_stop(actor) };
            }
        } else {
            // Exact counterfactual: same close + terminal CAS + lifecycle
            // path, differing only by omission of the new reclaim call.
            unsafe { mailbox::mailbox_close(mb) };
            // SAFETY: actor/a/mb are the same live allocation; mailbox is closed.
            assert!(unsafe { try_terminalize_idle_actor(actor, &*actor, mb, false) });
        }
        // SAFETY: actor stays live through this test.
        assert_eq!(
            unsafe { (*actor).actor_state.load(Ordering::Acquire) },
            HewActorState::Stopped as i32
        );

        // A zero-deadline wait distinguishes "published" from "still
        // waiting" without using an elapsed-time threshold.
        // SAFETY: creator ref keeps ch live; one waiter, on this thread.
        let waited = unsafe { reply_channel::hew_reply_wait_timeout(ch, 0) };
        assert!(waited.is_null());

        if reclaim_queued {
            assert!(
                unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
                "terminal reclaim publishes the null sentinel"
            );
            assert_eq!(
                unsafe { reply_channel::hew_reply_channel_is_orphaned(ch) },
                1
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(ch) },
                1,
                "only the creator ref remains after exact-once node retirement"
            );
            assert!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null(),
                "the exact queued node was reclaimed before sender wake"
            );
            // SAFETY: mailbox remains live, now drained.
            assert_eq!(unsafe { mailbox::hew_mailbox_len(mb) }, 0);
        } else {
            assert!(
                !unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
                "without the reclaim edge the wait returns only because its deadline elapsed"
            );
            assert_eq!(
                unsafe { reply_channel::hew_reply_channel_is_orphaned(ch) },
                0
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(ch) },
                2,
                "the same stranded node still owns the sender ref"
            );
            assert_eq!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()),
                exact_node,
                "the exact sender-carrying node survives the pre-fix omission"
            );
            // SAFETY: mailbox remains live and solely consumed here.
            assert_eq!(unsafe { mailbox::hew_mailbox_len(mb) }, 1);
        }

        // Let the sender attempt Idle -> Runnable. It must lose to Stopped,
        // while the send itself reports success because ownership already
        // transferred into the mailbox before the rendezvous.
        release.wait();
        assert_eq!(sender.join().expect("sender thread panicked"), 0);
        drop(hook);

        if !reclaim_queued {
            // The counterfactual omitted the terminal publisher's drain,
            // so the sender-side helper must retire its own already-
            // published enqueue after its wake CAS observes Stopped. This
            // is the backstop for a producer that passed the open check but
            // publishes after the terminal owner's first drain.
            assert!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null(),
                "the losing sender helps retire a late terminal enqueue"
            );
        }
        assert_eq!(
            unsafe { reply_channel::ref_count_for_test(ch) },
            1,
            "terminal cleanup consumes the queued sender ref exactly once"
        );
        assert!(mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null());
        // SAFETY: release the test's creator reference.
        unsafe { reply_channel::hew_reply_channel_free(ch) };

        assert!(live_actors::untrack_actor(actor));
        assert!(!live_actors::is_actor_live_with_id(id, actor));
        // SAFETY: actor and mailbox came from the fixture, are untracked,
        // stopped, empty, and unused after this point.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mb);
        }
        let frame_after = crate::observe::coroutine_snapshot();
        assert_eq!(frame_after.live, frame_baseline.live);
        assert_eq!(
            frame_after.frame_bytes_live,
            frame_baseline.frame_bytes_live
        );
    }

    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
    // Counterfactual first, then the production edge.
    unsafe {
        run_case(false, false);
        run_case(true, false);
        run_case(true, true);
    }
}

/// The post-link handoff takes a scheduler lifetime pin only when it
/// actually observes `Idle` and may publish `Runnable`. An enqueue against
/// an already runnable/running actor belongs to that existing activation;
/// retaining and immediately releasing a speculative queue entry needlessly
/// touches the actor's shared lifetime counter. Terminal states still run
/// the reclaim handoff even though they likewise need no queue entry.
#[test]
fn post_enqueue_handoff_pins_only_an_observed_idle_actor() {
    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();

    for (id, state) in [
        (28_311_700, HewActorState::Runnable),
        (28_311_701, HewActorState::Running),
    ] {
        let (actor, mb) = make_stop_test_actor_with_id(id, state);
        // Saturation makes any attempted speculative retain fail instead
        // of allowing a retain/release pair to escape a final-value check.
        // SAFETY: this isolated fixture is not visible to a scheduler.
        unsafe {
            (*actor).send_pin_count.store(u32::MAX, Ordering::Release);
            finish_mailbox_enqueue_inner(actor, &*actor, true);
            assert_eq!((*actor).send_pin_count.load(Ordering::Acquire), u32::MAX);
            assert_eq!((*actor).actor_state.load(Ordering::Acquire), state as i32);
            (*actor).send_pin_count.store(0, Ordering::Release);
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mb);
        }
    }

    let (terminal_actor, terminal_mb) =
        make_stop_test_actor_with_id(28_311_702, HewActorState::Crashed);
    let ch = reply_channel::hew_reply_channel_new();
    assert!(!ch.is_null());
    // SAFETY: the fresh channel and isolated terminal fixture remain live
    // through the synchronous enqueue/reclaim handoff.
    unsafe {
        reply_channel::hew_reply_channel_retain(ch);
        assert_eq!(
            mailbox::hew_mailbox_send_with_reply(terminal_mb, 1, ptr::null_mut(), 0, ch.cast(),),
            0
        );
        (*terminal_actor)
            .send_pin_count
            .store(u32::MAX, Ordering::Release);
        finish_mailbox_enqueue_inner(terminal_actor, &*terminal_actor, true);
        assert_eq!(
            (*terminal_actor).send_pin_count.load(Ordering::Acquire),
            u32::MAX,
            "terminal reclaim must not acquire a scheduler queue pin"
        );
        assert!(
            mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null(),
            "non-Idle terminal handoff must still retire the queued node"
        );
        assert!(reply_channel::hew_reply_channel_is_ready_for_test(ch));
        assert_eq!(reply_channel::ref_count_for_test(ch), 1);

        (*terminal_actor).send_pin_count.store(0, Ordering::Release);
        reply_channel::hew_reply_channel_free(ch);
        drop(Box::from_raw(terminal_actor));
        mailbox::hew_mailbox_free(terminal_mb);
    }

    let (idle_actor, idle_mb) = make_stop_test_actor_with_id(28_311_703, HewActorState::Idle);
    // SAFETY: the worker-less scheduler owns the resulting queue entry
    // until this test explicitly discards it.
    unsafe { finish_mailbox_enqueue_inner(idle_actor, &*idle_actor, true) };
    // SAFETY: fixture remains live and exclusively test-owned.
    let idle = unsafe { &*idle_actor };
    assert_eq!(
        idle.actor_state.load(Ordering::Acquire),
        HewActorState::Runnable as i32
    );
    assert_eq!(
        idle.send_pin_count.load(Ordering::Acquire),
        1,
        "an observed Idle actor must be pinned before queue publication"
    );
    assert!(scheduler::discard_queued_actor_for_test(idle_actor));
    assert_eq!(idle.send_pin_count.load(Ordering::Acquire), 0);
    // SAFETY: the only queue entry was discarded and the fixture is unused.
    unsafe {
        drop(Box::from_raw(idle_actor));
        mailbox::hew_mailbox_free(idle_mb);
    }
}

/// A producer that completes its MPSC predecessor link after the terminal
/// owner's bounded empty observation must perform the common post-link
/// handoff. The exact omission strands the same ask node and sender ref;
/// production retires it once and wakes the waiter.
#[test]
#[expect(
    clippy::too_many_lines,
    clippy::undocumented_unsafe_blocks,
    reason = "the deterministic MPSC ownership witness keeps each unsafe assertion beside the lifecycle seam it proves"
)]
fn terminal_sender_rechecks_after_last_activation_drain() {
    struct SendSubmission {
        actor: *mut HewActor,
        mailbox: *mut HewMailbox,
        channel: *mut HewReplyChannel,
        close_terminal_handoff: bool,
    }

    // SAFETY: each pointer outlives the joined sender thread and the test
    // retains the channel's creator reference until after the join.
    unsafe impl Send for SendSubmission {}

    impl SendSubmission {
        unsafe fn submit(self) -> i32 {
            // SAFETY: the channel carries creator + queued-sender refs and
            // the mailbox remains live through the joined call.
            let result = unsafe {
                mailbox::hew_mailbox_send_with_reply(
                    self.mailbox,
                    1,
                    ptr::null_mut(),
                    0,
                    self.channel.cast(),
                )
            };
            if result != 0 {
                return result;
            }

            // Mirror the production post-enqueue wake. The actor is already
            // terminal at this point, so the CAS must lose before entering
            // the handoff helper.
            // SAFETY: actor stays live through the joined call.
            let a = unsafe { &*self.actor };
            assert!(a
                .actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_err());
            // SAFETY: exact production post-link handoff with a switch that
            // omits only terminal help for the counterfactual.
            unsafe {
                finish_mailbox_enqueue_inner(self.actor, a, self.close_terminal_handoff);
            }
            result
        }
    }

    unsafe fn run_case(close_terminal_handoff: bool) {
        static NEXT_ID: AtomicU64 = AtomicU64::new(28_312_000);

        let frame_baseline = crate::observe::coroutine_snapshot();
        let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
        let (actor, mb) = make_stop_test_actor_with_id(id, HewActorState::Crashed);
        // SAFETY: fully initialized unique actor, owned through cleanup.
        assert!(unsafe { live_actors::track_actor(actor) });
        assert!(live_actors::is_actor_live_with_id(id, actor));

        let ch = reply_channel::hew_reply_channel_new();
        assert!(!ch.is_null());
        // Mint the sender reference transferred into the delayed ask node.
        // SAFETY: fresh creator-owned channel.
        unsafe { reply_channel::hew_reply_channel_retain(ch) };

        let (link_hook, link_entered, link_release) =
            mailbox::MpscPostSwapPreLinkHookGuard::install(ch.cast());

        let submission = SendSubmission {
            actor,
            mailbox: mb,
            channel: ch,
            close_terminal_handoff,
        };
        let sender = std::thread::spawn(move || {
            // SAFETY: pointer and reference lifetimes are upheld by run_case.
            unsafe { submission.submit() }
        });

        // The producer has swapped the queue head but has not linked the
        // predecessor, so the activation's bounded dequeue sees only
        // `Inconsistent`.
        link_entered.wait();
        let exact_node = mailbox::ask_node_for_reply_channel_for_test(ch.cast());
        assert!(
            !exact_node.is_null(),
            "the delayed ask node is identity-tracked"
        );
        // SAFETY: creator reference keeps the exact channel live.
        assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 2);

        // The terminal owner exhausts its bounded pass while the new head
        // is unreachable from the old tail.
        // SAFETY: this thread is the sole terminal consumer.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
        assert_eq!(
            mailbox::ask_node_for_reply_channel_for_test(ch.cast()),
            exact_node
        );

        // Finish the predecessor link; the producer's canonical handoff is
        // now the only code guaranteed to run.
        link_release.wait();
        assert_eq!(sender.join().expect("sender thread panicked"), 0);

        if close_terminal_handoff {
            assert!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null(),
                "producer-side handoff retires the late-linked exact node"
            );
            assert!(
                unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
                "late terminal ask publishes its orphan sentinel"
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(ch) },
                1,
                "only the creator reference survives the exact-once retire"
            );
        } else {
            assert_eq!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()),
                exact_node,
                "omitting post-link terminal handoff strands the exact node"
            );
            assert!(
                !unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
                "the stranded ask remains unresolved after both owners return"
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(ch) },
                2,
                "the stranded node still owns its sender reference"
            );

            // Test cleanup after the omission proof.
            // SAFETY: both producer and activation owner have returned, so
            // this thread is the sole terminal consumer.
            unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
            assert!(mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null());
            assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 1);
        }

        drop(link_hook);
        // SAFETY: release the creator ref after the queued sender ref is gone.
        unsafe { reply_channel::hew_reply_channel_free(ch) };

        assert!(live_actors::untrack_actor(actor));
        assert!(!live_actors::is_actor_live_with_id(id, actor));
        // SAFETY: untracked terminal actor and drained mailbox are unused.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mb);
        }
        let frame_after = crate::observe::coroutine_snapshot();
        assert_eq!(frame_after.live, frame_baseline.live);
        assert_eq!(
            frame_after.frame_bytes_live,
            frame_baseline.frame_bytes_live
        );
    }

    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
    // Counterfactual first, then the repaired production handoff.
    unsafe {
        run_case(false);
        run_case(true);
    }
}

/// System producers use the same post-link terminal handoff as user sends
/// and asks. A delayed system predecessor link that lands after the
/// terminal drain is reclaimed by the producer; omitting only that handoff
/// leaves the system node observable.
#[test]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the deterministic delayed-link fixture keeps each raw actor/mailbox operation inside one unsafe case helper"
)]
fn delayed_system_link_uses_common_terminal_handoff() {
    unsafe fn run_case(close_terminal_handoff: bool) {
        let (actor, mb) = make_stop_test_actor_with_id(28_312_500, HewActorState::Crashed);
        assert!(unsafe { live_actors::track_actor(actor) });

        let (hook, entered, release) =
            mailbox::MpscPostSwapPreLinkHookGuard::install_system(mailbox::HewSysMsg::Down);
        let actor_addr = actor.addr();
        let sender = std::thread::spawn(move || {
            let actor = ptr::with_exposed_provenance_mut::<HewActor>(actor_addr);
            // SAFETY: fixture and mailbox outlive this joined producer.
            let a = unsafe { &*actor };
            let mailbox = a.mailbox.cast::<HewMailbox>();
            assert!(unsafe {
                mailbox::mailbox_send_sys_checked(
                    mailbox,
                    mailbox::HewSysMsg::Down,
                    ptr::null_mut(),
                    0,
                )
            });
            unsafe {
                finish_mailbox_enqueue_inner(actor, a, close_terminal_handoff);
            }
        });

        entered.wait();
        // SAFETY: the queue is intentionally inconsistent and this thread
        // owns the terminal consumer.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
        release.wait();
        sender.join().expect("system producer");

        // SAFETY: terminal fixture has no concurrent consumer.
        let remaining = unsafe { mailbox::hew_mailbox_try_recv_sys(mb) };
        if close_terminal_handoff {
            assert!(
                remaining.is_null(),
                "common handoff must retire the delayed system node"
            );
        } else {
            assert!(
                !remaining.is_null(),
                "omitting system post-link handoff must strand its node"
            );
            // SAFETY: dequeue transferred the stranded node to this test.
            unsafe { mailbox::hew_msg_node_free(remaining) };
        }

        drop(hook);
        assert!(live_actors::untrack_actor(actor));
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mb);
        }
    }

    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
    unsafe {
        run_case(false);
        run_case(true);
    }
}

/// A self-send runs inside the activation whose `dispatch_active` flag it
/// observes. Waiting for that same flag would deadlock the handler before
/// its ownership guard can perform the terminal drain. The helper instead
/// takes the terminal-reclaim lock, observes its own still-active frame,
/// and defers to that frame's final locked drain without waiting.
#[test]
fn terminal_self_sender_defers_to_own_activation_without_deadlock() {
    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
    let frame_baseline = crate::observe::coroutine_snapshot();
    let id = 28_313_000;
    let (actor, mb) = make_stop_test_actor_with_id(id, HewActorState::Running);
    // SAFETY: fully initialized unique actor, owned through cleanup.
    assert!(unsafe { live_actors::track_actor(actor) });
    assert!(live_actors::is_actor_live_with_id(id, actor));

    let ch = reply_channel::hew_reply_channel_new();
    assert!(!ch.is_null());
    // SAFETY: mint the sender ref and enqueue while the actor is live.
    unsafe {
        reply_channel::hew_reply_channel_retain(ch);
        assert_eq!(
            mailbox::hew_mailbox_send_with_reply(mb, 1, ptr::null_mut(), 0, ch.cast(),),
            0
        );
        (*actor)
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        (*actor).dispatch_active.store(true, Ordering::Release);
        mailbox::mailbox_close(mb);
    }
    let exact_node = mailbox::ask_node_for_reply_channel_for_test(ch.cast());
    assert!(!exact_node.is_null());

    {
        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor,
            actor_id: id,
            ..HewExecutionContext::default()
        });
        // SAFETY: actor stays live and this context proves the caller owns
        // the active dispatch it would otherwise wait on.
        unsafe { reclaim_terminal_enqueue_if_unowned(&*actor) };
    }
    assert_eq!(
        mailbox::ask_node_for_reply_channel_for_test(ch.cast()),
        exact_node,
        "self-owner leaves the fully-linked node for its own final drain"
    );
    // SAFETY: the creator reference keeps the exact channel live.
    assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 2);

    // Execute that exact final activation release.
    // SAFETY: isolated actor, no real scheduler activation.
    unsafe { crate::scheduler::release_terminal_activation_ownership_for_test(actor) };
    assert!(mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null());
    assert!(
        // SAFETY: the creator reference keeps the exact channel live.
        unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
        "the owning activation resolves its self-enqueued ask"
    );
    // SAFETY: the creator reference keeps the exact channel live.
    assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 1);
    // SAFETY: release the remaining creator ref.
    unsafe { reply_channel::hew_reply_channel_free(ch) };

    assert!(live_actors::untrack_actor(actor));
    assert!(!live_actors::is_actor_live_with_id(id, actor));
    // SAFETY: untracked terminal actor and drained mailbox are unused.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mb);
    }
    let frame_after = crate::observe::coroutine_snapshot();
    assert_eq!(frame_after.live, frame_baseline.live);
    assert_eq!(
        frame_after.frame_bytes_live,
        frame_baseline.frame_bytes_live
    );
}

/// An activation release must not snapshot a non-terminal state, lose to an
/// external trap that observes `dispatch_active == true`, and then clear
/// ownership without either side reclaiming the queued ask.
///
/// The rendezvous stops the activation after the counterfactual's state
/// snapshot but before ownership release. The external trap then publishes
/// `Crashed` and defers its locked drain to that active owner. Omitting only
/// the activation's locked terminal recheck strands the exact node and its
/// sender reference; production observes the trap publication under the
/// shared lock and retires both before clearing ownership.
#[test]
#[expect(
    clippy::too_many_lines,
    clippy::undocumented_unsafe_blocks,
    reason = "the trap/drop ownership witness keeps each unsafe assertion beside the lifecycle seam it proves"
)]
fn terminal_trap_and_activation_drop_share_reclaim_handoff() {
    struct OwnerRelease {
        actor: *mut HewActor,
        close_terminal_handoff: bool,
    }

    // SAFETY: the actor outlives the joined owner thread and remains
    // exclusively controlled by the test fixture.
    unsafe impl Send for OwnerRelease {}

    impl OwnerRelease {
        unsafe fn release(self) {
            if self.close_terminal_handoff {
                // SAFETY: actor remains live and no scheduler sees it.
                unsafe {
                    crate::scheduler::release_terminal_activation_ownership_for_test(self.actor);
                }
            } else {
                // SAFETY: same fixture contract; this executes only the
                // exact pre-fix omission counterfactual.
                unsafe {
                    crate::scheduler::release_activation_ownership_omitting_terminal_recheck_for_test(
                            self.actor,
                        );
                }
            }
        }
    }

    unsafe fn run_case(close_terminal_handoff: bool) {
        static NEXT_ID: AtomicU64 = AtomicU64::new(28_314_000);

        let frame_baseline = crate::observe::coroutine_snapshot();
        let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
        let (actor, mb) = make_stop_test_actor_with_id(id, HewActorState::Running);
        // SAFETY: fully initialized unique actor, owned through cleanup.
        assert!(unsafe { live_actors::track_actor(actor) });
        assert!(live_actors::is_actor_live_with_id(id, actor));

        let ch = reply_channel::hew_reply_channel_new();
        assert!(!ch.is_null());
        // Mint the sender reference transferred into the queued ask node.
        // SAFETY: fresh creator-owned channel and live mailbox.
        unsafe {
            reply_channel::hew_reply_channel_retain(ch);
            assert_eq!(
                mailbox::hew_mailbox_send_with_reply(mb, 1, ptr::null_mut(), 0, ch.cast(),),
                0
            );
        }
        let exact_node = mailbox::ask_node_for_reply_channel_for_test(ch.cast());
        assert!(!exact_node.is_null());
        // SAFETY: creator reference keeps the exact channel live.
        assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 2);

        let (hook, owner_entered, owner_release) =
            crate::scheduler::ActivationPreTerminalLockHookGuard::install(id);
        let release = OwnerRelease {
            actor,
            close_terminal_handoff,
        };
        let owner = std::thread::spawn(move || {
            // SAFETY: run_case joins before actor cleanup.
            unsafe { release.release() };
        });

        owner_entered.wait();
        // The synthetic activation has published ownership but has not
        // entered the terminal-reclaim critical section.
        // SAFETY: actor remains live through the joined owner.
        assert!(unsafe { (*actor).dispatch_active.load(Ordering::Acquire) });
        assert_eq!(
            unsafe { (*actor).actor_state.load(Ordering::Acquire) },
            HewActorState::Running as i32
        );

        // Publish terminal through the production external-trap path. Its
        // locked quiescence check sees the active owner and must defer.
        // SAFETY: actor is live and tracked.
        unsafe { hew_actor_trap(actor, 91) };
        assert_eq!(
            unsafe { (*actor).actor_state.load(Ordering::Acquire) },
            HewActorState::Crashed as i32
        );
        assert!(unsafe { (*actor).dispatch_active.load(Ordering::Acquire) });
        assert_eq!(
            mailbox::ask_node_for_reply_channel_for_test(ch.cast()),
            exact_node,
            "the trap correctly leaves the exact node to its active owner"
        );
        assert!(
            !unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
            "the deferred ask remains unresolved until ownership handoff"
        );
        assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 2);

        owner_release.wait();
        owner.join().expect("activation owner thread panicked");
        assert!(!unsafe { (*actor).dispatch_active.load(Ordering::Acquire) });

        if close_terminal_handoff {
            assert!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null(),
                "the locked terminal recheck retires the deferred node"
            );
            assert!(
                unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
                "the exact ask receives its orphan sentinel"
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(ch) },
                1,
                "only the creator reference survives the exact-once retire"
            );
        } else {
            assert_eq!(
                mailbox::ask_node_for_reply_channel_for_test(ch.cast()),
                exact_node,
                "the pre-fix state snapshot strands the deferred node"
            );
            assert!(
                !unsafe { reply_channel::hew_reply_channel_is_ready_for_test(ch) },
                "neither omitted handoff participant resolves the ask"
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(ch) },
                2,
                "the stranded node still owns its sender reference"
            );

            // Counterfactual cleanup after proving the omission.
            // SAFETY: trap and activation owner have both returned.
            unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
            assert!(mailbox::ask_node_for_reply_channel_for_test(ch.cast()).is_null());
            assert_eq!(unsafe { reply_channel::ref_count_for_test(ch) }, 1);
        }

        drop(hook);
        // SAFETY: release the creator ref after the queued sender ref is gone.
        unsafe { reply_channel::hew_reply_channel_free(ch) };

        assert!(live_actors::untrack_actor(actor));
        assert!(!live_actors::is_actor_live_with_id(id, actor));
        // SAFETY: untracked terminal actor and drained mailbox are unused.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mb);
        }
        let frame_after = crate::observe::coroutine_snapshot();
        assert_eq!(frame_after.live, frame_baseline.live);
        assert_eq!(
            frame_after.frame_bytes_live,
            frame_baseline.frame_bytes_live
        );
    }

    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
    // Counterfactual first, then the repaired production handoff.
    unsafe {
        run_case(false);
        run_case(true);
    }
}

/// Deterministic #2831 ownership witness for the crash half.
///
/// Two real ask nodes are queued. The scheduler ownership transfer is then
/// modeled exactly: dequeue the first node (it is now in-flight and solely
/// scheduler-owned), publish its crash fallback, and free it before entering
/// the trap publisher. The second node remains queued behind it.
///
/// The counterfactual executes all crash publication while omitting only the
/// new trap-side mailbox reclaim. It proves the in-flight ask is settled but
/// the exact queued node/ref remains unready through a zero-deadline wait.
/// Production settles both before notification can transfer the crashed
/// incarnation to a supervisor.
#[test]
#[expect(
    clippy::too_many_lines,
    clippy::undocumented_unsafe_blocks,
    reason = "the deterministic FFI ownership witness keeps each unsafe assertion beside the exact lifecycle phase it proves"
)]
fn crash_trap_retires_asks_queued_behind_inflight_ask() {
    unsafe fn run_case(reclaim_queued: bool) {
        static NEXT_ID: AtomicU64 = AtomicU64::new(28_311_000);

        let frame_baseline = crate::observe::coroutine_snapshot();

        let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
        let (actor, mb) = make_stop_test_actor_with_id(id, HewActorState::Crashing);
        // SAFETY: fully initialized unique actor.
        assert!(unsafe { live_actors::track_actor(actor) });
        assert!(live_actors::is_actor_live_with_id(id, actor));

        let inflight_ch = reply_channel::hew_reply_channel_new();
        let queued_ch = reply_channel::hew_reply_channel_new();
        assert!(!inflight_ch.is_null() && !queued_ch.is_null());
        // The ask submission mints one sender-side reference for each node.
        // SAFETY: both channels are fresh and creator-owned.
        unsafe {
            reply_channel::hew_reply_channel_retain(inflight_ch);
            reply_channel::hew_reply_channel_retain(queued_ch);
        }
        // SAFETY: live mailbox, empty payload, valid retained channels.
        assert_eq!(
            unsafe {
                mailbox::hew_mailbox_send_with_reply(mb, 1, ptr::null_mut(), 0, inflight_ch.cast())
            },
            0
        );
        assert_eq!(
            unsafe {
                mailbox::hew_mailbox_send_with_reply(mb, 2, ptr::null_mut(), 0, queued_ch.cast())
            },
            0
        );
        // Scheduler dequeues one ask and now owns it in-flight; the next ask
        // remains in the mailbox.
        // SAFETY: test is the sole mailbox consumer.
        let inflight_node = unsafe { mailbox::hew_mailbox_try_recv(mb) };
        assert!(!inflight_node.is_null());
        let exact_queued_node = mailbox::ask_node_for_reply_channel_for_test(queued_ch.cast());
        assert!(!exact_queued_node.is_null());
        // SAFETY: mailbox remains live.
        assert_eq!(unsafe { mailbox::hew_mailbox_len(mb) }, 1);

        // Mirror activate_actor's pre-publication in-flight cleanup:
        // publish crash failure, detach the consumed sender reference from
        // the node, then free that exclusively-owned node.
        // SAFETY: inflight_ch has a live sender ref and node is exclusive.
        unsafe {
            reply_channel::hew_reply_channel_publish_crash_fallback(inflight_ch);
            (*inflight_node).reply_channel = ptr::null_mut();
            mailbox::hew_msg_node_free(inflight_node);
        }
        assert!(
            unsafe { reply_channel::hew_reply_channel_is_ready_for_test(inflight_ch) },
            "in-flight crash fallback is published before terminal state"
        );
        assert_eq!(
            unsafe { reply_channel::ref_count_for_test(inflight_ch) },
            1,
            "in-flight sender ref was consumed exactly once"
        );
        assert!(
            mailbox::ask_node_for_reply_channel_for_test(inflight_ch.cast()).is_null(),
            "only the exact queued-behind ask node remains at the trap seam"
        );

        // Exact production/counterfactual split.
        // SAFETY: actor is in Crashing, live and tracked.
        let mailbox_reclaim = if reclaim_queued {
            TrapMailboxReclaim::OwnedActivation
        } else {
            TrapMailboxReclaim::OmitForTest
        };
        unsafe { hew_actor_trap_inner(actor, -1, mailbox_reclaim) };
        assert_eq!(
            unsafe { (*actor).actor_state.load(Ordering::Acquire) },
            HewActorState::Crashed as i32
        );

        // SAFETY: creator refs keep both channels live.
        assert!(unsafe { reply_channel::hew_reply_wait_timeout(inflight_ch, 0).is_null() });
        assert!(unsafe { reply_channel::hew_reply_wait_timeout(queued_ch, 0).is_null() });

        if reclaim_queued {
            assert!(
                unsafe { reply_channel::hew_reply_channel_is_ready_for_test(queued_ch) },
                "queued crash ask is published before supervisor notification"
            );
            assert_eq!(
                unsafe { reply_channel::hew_reply_channel_is_orphaned(queued_ch) },
                1
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(queued_ch) },
                1,
                "queued crash sender ref is consumed exactly once"
            );
            assert!(mailbox::ask_node_for_reply_channel_for_test(queued_ch.cast()).is_null());
        } else {
            assert!(
                !unsafe { reply_channel::hew_reply_channel_is_ready_for_test(queued_ch) },
                "without the trap reclaim edge the wait only returns at its deadline"
            );
            assert_eq!(
                unsafe { reply_channel::ref_count_for_test(queued_ch) },
                2,
                "the exact queued node still owns its sender ref"
            );
            assert_eq!(
                mailbox::ask_node_for_reply_channel_for_test(queued_ch.cast()),
                exact_queued_node
            );
            // SAFETY: crashed actor remains live and test is sole consumer.
            unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
        }

        assert_eq!(unsafe { reply_channel::ref_count_for_test(queued_ch) }, 1);
        // SAFETY: release both creator references.
        unsafe {
            reply_channel::hew_reply_channel_free(inflight_ch);
            reply_channel::hew_reply_channel_free(queued_ch);
        }

        assert!(live_actors::untrack_actor(actor));
        assert!(!live_actors::is_actor_live_with_id(id, actor));
        // SAFETY: untracked crashed actor and drained mailbox are unused.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mb);
        }
        let frame_after = crate::observe::coroutine_snapshot();
        assert_eq!(frame_after.live, frame_baseline.live);
        assert_eq!(
            frame_after.frame_bytes_live,
            frame_baseline.frame_bytes_live
        );
    }

    let _rt = crate::runtime_test_guard();
    let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
    unsafe {
        run_case(false);
        run_case(true);
    }
}

/// The shutdown-leak regression for the `ask`-race fixture (#2817).
///
/// An actor parked at a suspend point is NOT quiescent, so the shutdown
/// sweep's finalize decision can only leak it — box and frame both. That is
/// correct as a last resort but wrong as the outcome of a normal exit, and
/// it is exactly what `actor_ask_race.hew` produced: the actor that lost the
/// race was still parked on `sleep` when `main` returned.
///
/// This pins the two halves of the fix: the leak is real if nothing runs
/// abandonment first, and `abandon_parked_activation` — which
/// `retire_parked_activations` runs over every live actor at the head of
/// `hew_runtime_cleanup` — releases the frame and returns the actor to a
/// state the sweep reclaims.
#[test]
fn abandoning_a_parked_activation_makes_it_reclaimable() {
    let (actor, mailbox) = make_stop_test_actor(HewActorState::Suspended);
    // SAFETY: the helper hands over sole ownership; nothing else can see it.
    let a = unsafe { &*actor };

    let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
    let handle = frame.handle();
    assert!(crate::coro_exec::begin_park(a).is_ok());
    // SAFETY: `frame` outlives this test body.
    unsafe { crate::coro_exec::finish_park(a, handle) };
    assert!(crate::coro_exec::has_live_parked_cont(a));

    // Without abandonment the sweep has no choice but the fail-closed leak.
    assert!(
        matches!(decide_finalize_by_latch(a), FinalizeDecision::Skip),
        "a parked actor must not be finalizable while its frame is live"
    );

    abandon_parked_activation(a);

    assert_eq!(
        frame.destroyed.load(Ordering::Acquire),
        1,
        "abandonment must run the parked frame's destroy outline exactly once"
    );
    assert!(!crate::coro_exec::has_live_parked_cont(a));
    assert_eq!(
        a.actor_state.load(Ordering::Acquire),
        HewActorState::Stopped as i32,
        "abandonment must latch the actor out of the non-quiescent Suspended window"
    );
    assert!(
        matches!(decide_finalize_by_latch(a), FinalizeDecision::Finalize(_)),
        "after abandonment the shutdown sweep must reclaim the actor, not leak it"
    );

    // SAFETY: sole owner; the parked frame is already destroyed.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mailbox);
    }
}

// --- null-guard regression tests ---
//
// Each test passes a null pointer to an FFI setter/getter that previously
// dereferenced unconditionally.  The expected behaviour after this fix is:
//  - void functions: return without crashing (SIGSEGV before fix)
//  - value functions: return the documented zero sentinel
//
// These tests do NOT need a scheduler or a real actor allocation.

#[test]
fn null_actor_close_returns_without_crash() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    unsafe { hew_actor_close(ptr::null_mut()) };
}

#[test]
fn actor_self_without_execution_context_fails_closed() {
    let _guard = crate::runtime_test_guard();
    crate::hew_clear_error();
    assert!(hew_actor_self().is_null());
    let err = crate::hew_last_error();
    assert!(!err.is_null());
    // SAFETY: hew_last_error returned a non-null C string.
    let err = unsafe { std::ffi::CStr::from_ptr(err).to_str().unwrap() };
    assert_eq!(
        err,
        crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
    );
    crate::hew_clear_error();
}

#[test]
fn null_actor_stop_returns_without_crash() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    unsafe { hew_actor_stop(ptr::null_mut()) };
}

#[test]
fn null_actor_set_budget_returns_without_crash() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    unsafe { hew_actor_set_budget(ptr::null_mut(), 10) };
}

#[test]
fn null_actor_get_budget_returns_sentinel() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    let v = unsafe { hew_actor_get_budget(ptr::null()) };
    assert_eq!(v, 0, "expected zero sentinel for null actor");
}

unsafe extern "C-unwind" fn null_guard_dummy_terminate(_: *mut c_void) {}

#[test]
fn null_actor_set_terminate_returns_without_crash() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    unsafe { hew_actor_set_terminate(ptr::null_mut(), null_guard_dummy_terminate) };
}

#[test]
fn null_actor_set_reductions_returns_without_crash() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    unsafe { hew_actor_set_reductions(ptr::null_mut(), 5) };
}

#[test]
fn null_actor_get_reductions_returns_sentinel() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    let v = unsafe { hew_actor_get_reductions(ptr::null()) };
    assert_eq!(v, 0, "expected zero sentinel for null actor");
}

#[test]
fn null_actor_pid_returns_sentinel() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    let v = unsafe { hew_actor_pid(ptr::null_mut()) };
    assert_eq!(v, 0, "expected zero sentinel for null actor");
}

// --- null-guard regression tests for the high-frequency send/ask paths ---
//
// These cover the paths the prior batch missed: `hew_actor_send`,
// `hew_actor_try_send`, and the ask-family helper.  Each test passes a
// null actor pointer and expects the guard to fire without a SIGSEGV and
// to return `HewError::ErrActorStopped` for i32-returning variants.

#[test]
fn null_actor_send_returns_without_crash() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    unsafe { hew_actor_send(ptr::null_mut(), 0, ptr::null_mut(), 0) };
}

#[test]
fn null_actor_try_send_returns_err_actor_stopped() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    let result = unsafe { hew_actor_try_send(ptr::null_mut(), 0, ptr::null_mut(), 0) };
    assert_eq!(
        result,
        HewError::ErrActorStopped as i32,
        "expected ErrActorStopped for null actor"
    );
}

#[test]
fn null_actor_send_result_internal_reply_returns_err_actor_stopped() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null is the input we are testing the guard against.
    let result = unsafe {
        actor_send_result_internal_reply(ptr::null_mut(), 0, ptr::null_mut(), 0, ptr::null_mut())
    };
    assert_eq!(
        result,
        HewError::ErrActorStopped as i32,
        "expected ErrActorStopped for null actor"
    );
}

#[test]
fn send_by_id_concurrent_no_deadlock() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();
    SEND_BY_ID_DISPATCH_COUNT.store(0, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor =
        unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(count_send_by_id_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor is live for the duration of the test.
    let actor_id = unsafe { (*actor).id };
    let thread_count = 8usize;
    let sends_per_thread = 32usize;
    let start = std::sync::Arc::new(std::sync::Barrier::new(thread_count));
    let mut handles = Vec::with_capacity(thread_count);

    for _ in 0..thread_count {
        let start = start.clone();
        handles.push(std::thread::spawn(move || {
            start.wait();
            for _ in 0..sends_per_thread {
                // SAFETY: actor remains live until all sender threads join.
                let rc =
                    unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
                assert_eq!(rc, 0);
            }
        }));
    }

    for handle in handles {
        handle.join().expect("send thread must not panic");
    }

    let expected = thread_count * sends_per_thread;
    // Scheduler should drain all by-id sends without deadlocking.
    wait_until(|| SEND_BY_ID_DISPATCH_COUNT.load(Ordering::Acquire) >= expected);
    assert_eq!(SEND_BY_ID_DISPATCH_COUNT.load(Ordering::Acquire), expected);

    // SAFETY: actor remains live until teardown below.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(hew_actor_free(actor), 0);
    }
}

/// A fire-and-forget send-by-id to an actor ID that is no longer
/// tracked locally (freed, stopped, or never existed) is a genuine,
/// caller-visible failure. It must report a code DISTINCT from
/// `ErrMailboxFull` — the status that a declared bounded mailbox's
/// `DropNew`/`DropOld`/`Coalesce` policy-drop resolves to `Ok` and
/// returns (see `send_by_id_dropnew_policy_drop_is_silent` below).
///
/// Before this fix, `hew_actor_send_by_id` returned the same `-1` for
/// both cases, preventing codegen's `Terminator::Send` from
/// distinguishing genuine failure from policy-drop without swallowing
/// the genuine failure.
#[test]
fn send_by_id_after_free_returns_genuine_failure_not_mailbox_full() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor is valid until the free below.
    let actor_id = unsafe { (*actor).id };

    // SAFETY: actor is quiescent after close and fully owned by this test.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(hew_actor_free(actor), 0);
    }

    // SAFETY: caller only provides message bytes; the runtime should reject
    // the now-untracked actor ID instead of crashing.
    let rc = unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
    assert_eq!(
        rc,
        HewError::ErrActorStopped as i32,
        "send-by-id to a gone actor must report ErrActorStopped, not the \
             ErrMailboxFull code a declared overflow policy-drop also used to \
             report — the two must never collapse onto the same value"
    );
    assert_ne!(
        rc,
        HewError::ErrMailboxFull as i32,
        "a genuine send failure must never be confused with a policy-drop"
    );
}

/// A send-by-id into a full `DropNew` mailbox reports the dedicated
/// positive policy-loss status, distinct from a failed admission. Paired with
/// `send_by_id_after_free_returns_genuine_failure_not_mailbox_full`:
/// the two scenarios must produce DIFFERENT codes so checked sends can
/// materialize the right `SendError` without confusing loss and rejection.
#[test]
fn send_by_id_dropnew_policy_drop_reports_message_lost() {
    let _guard = crate::runtime_test_guard();

    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(noop_dispatch),
        mailbox_capacity: 1,
        overflow: HewOverflowPolicy::DropNew as i32,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        message_drop_fn: None,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };
    // SAFETY: opts is valid for the duration of the call.
    let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
    assert!(!actor.is_null(), "bounded DropNew spawn must succeed");
    // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
    let actor_id = unsafe { (*actor).id };
    // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
    let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };

    // Directly fill the one capacity slot, bypassing the scheduler so the
    // actor stays Idle and the slot stays occupied (same technique as
    // `native_ask_bounded_mailbox_full_sets_mailbox_full_error`).
    // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
    let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
    assert_eq!(pre_fill, HewError::Ok as i32, "pre-fill must succeed");

    // The mailbox is now at capacity; this send must overflow into the
    // DropNew policy and report visible loss.
    // SAFETY: actor remains live for this call.
    let rc = unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
    assert_eq!(
        rc, HEW_ACTOR_SEND_MESSAGE_LOST,
        "a DropNew policy-drop must report the dedicated message-loss status"
    );

    // Actor is still Idle (no state transition occurred: the DropNew
    // overflow never enqueues, so nothing wakes the scheduler).
    // hew_actor_stop CAS Idle → Stopped succeeds directly; no scheduler
    // needed — mirrors `native_ask_bounded_mailbox_full_sets_mailbox_full_error`.
    // SAFETY: actor is valid; stopping a live actor is safe.
    unsafe { hew_actor_stop(actor) };
    // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
    // pre-filled message during free_actor_resources.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

/// The `Fail` overflow policy is fail-closed for fire-and-forget sends.
/// Unlike lossy policies, the `Fail` policy is an explicit rejection. It
/// must report a
/// distinct non-zero code even on the no-reply-channel send path, so
/// `Terminator::Send` traps rather than silently dropping the message.
#[test]
fn send_by_id_fail_policy_overflow_is_genuine_failure() {
    let _guard = crate::runtime_test_guard();

    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(noop_dispatch),
        mailbox_capacity: 1,
        overflow: HewOverflowPolicy::Fail as i32,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        message_drop_fn: None,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };
    // SAFETY: opts is valid for the duration of the call.
    let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
    assert!(!actor.is_null(), "bounded Fail spawn must succeed");
    // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
    let actor_id = unsafe { (*actor).id };
    // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
    let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };

    // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
    let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
    assert_eq!(pre_fill, HewError::Ok as i32, "pre-fill must succeed");

    // SAFETY: actor remains live for this call.
    let rc = unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
    assert_ne!(
        rc,
        HewError::Ok as i32,
        "Fail-policy overflow must not silently succeed on a fire-and-forget send"
    );

    // SAFETY: actor is valid; stopping a live actor is safe.
    unsafe { hew_actor_stop(actor) };
    // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
    // pre-filled message during free_actor_resources.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

/// A held actor pointer stamped with a different `runtime_id` than the
/// runtime bound on this thread fails closed on every held-pointer send
/// path (`boundary-fail-closed`): the boundary refuses with
/// `ErrForeignRuntime` and never routes the foreign pointer. In a
/// single-runtime program this never fires, so a DEFAULT-stamped actor
/// (the control) still accepts the same sends.
///
/// This stamps the discriminant directly rather than constructing a second
/// worker-backed runtime: the check compares two `RuntimeId`s and the
/// second runtime is never dereferenced, so a foreign id is sufficient to
/// exercise the wall without standing up a second scheduler.
#[test]
fn cross_runtime_send_fails_closed() {
    let _guard = crate::runtime_test_guard();

    // The thread is bound to the default runtime (RuntimeId::DEFAULT) via
    // the test guard. Build a fully-formed test actor and re-stamp it as if
    // it were spawned by a different runtime.
    let (actor, mailbox) = make_stop_test_actor_with_id(0xBEEF, HewActorState::Idle);
    // SAFETY: the test exclusively owns `actor` and never publishes it.
    unsafe {
        (*actor).runtime_id = crate::runtime_id::RuntimeId(1);
    }

    // Every held-pointer send path refuses the foreign actor.
    // SAFETY: `actor` is valid and fully owned by this test; null payload.
    let try_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
    assert_eq!(
        try_rc,
        HewError::ErrForeignRuntime as i32,
        "try_send to a foreign-runtime actor must fail closed"
    );

    // The fire-and-forget result path (used by `hew_actor_send`) also
    // refuses; assert on the result-returning internal it delegates to.
    // SAFETY: as above.
    let send_rc = unsafe { actor_send_result_internal(actor, 1, ptr::null_mut(), 0) };
    assert_eq!(
        send_rc,
        HewError::ErrForeignRuntime as i32,
        "send to a foreign-runtime actor must fail closed"
    );

    // The refusal must NOT have enqueued anything: no message reached the
    // mailbox, so nothing was routed to the foreign actor.
    // SAFETY: `mailbox` is valid and owned by this test.
    let has_messages = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
    assert_eq!(
        has_messages, 0,
        "a refused cross-runtime send must not enqueue a message"
    );

    // Control: re-stamp as the default runtime and the SAME send now
    // succeeds (the check is invisible single-runtime).
    // SAFETY: the test owns `actor`.
    unsafe {
        (*actor).runtime_id = crate::runtime_id::RuntimeId::DEFAULT;
    }
    // SAFETY: as above.
    let ok_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
    assert_eq!(
        ok_rc,
        HewError::Ok as i32,
        "a same-runtime actor must accept the send"
    );

    // SAFETY: the test fully owns the actor and its mailbox.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mailbox);
    }
}

/// An injected drop fault discards the message before it reaches the
/// queue. That is loss, not delivery: a checked send reads the
/// declared-loss code and an ask refuses instead of waiting for a reply
/// that cannot arrive. The control is the same send with no fault armed.
#[test]
fn injected_drop_fault_reports_loss_not_delivery() {
    let _guard = crate::runtime_test_guard();
    let actor_id = 0xD40Bu64;
    let (actor, mailbox) = make_stop_test_actor_with_id(actor_id, HewActorState::Idle);

    // Two drops: one for the tell below, one for the ask.
    crate::deterministic::hew_fault_clear_all();
    crate::deterministic::hew_fault_inject_drop(actor_id, 2);

    // SAFETY: the test exclusively owns `actor`; null payload, zero size.
    let dropped = unsafe { actor_send_result_internal(actor, 1, ptr::null_mut(), 0) };
    assert_eq!(
        dropped, HEW_ACTOR_SEND_MESSAGE_LOST,
        "a dropped send must report declared loss, not delivery"
    );
    // SAFETY: `mailbox` is valid and owned by this test.
    let queued_after_drop = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
    assert_eq!(
        queued_after_drop, 0,
        "a dropped send must not enqueue a message"
    );

    // The ask half returns rather than blocking on a reply that the
    // discarded message can never produce.
    // SAFETY: as above.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(reply.is_null(), "a dropped ask produces no reply");
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::ActorStopped as i32,
        "a dropped ask reports the actor as unreachable"
    );

    // Control: the fault budget is spent, so the same send delivers.
    // SAFETY: as above.
    let delivered = unsafe { actor_send_result_internal(actor, 1, ptr::null_mut(), 0) };
    assert_eq!(
        delivered,
        HewError::Ok as i32,
        "an unfaulted send must still report delivery"
    );
    // SAFETY: `mailbox` is valid and owned by this test.
    let queued_after_delivery = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
    assert_eq!(
        queued_after_delivery, 1,
        "an unfaulted send must enqueue its message"
    );

    crate::deterministic::hew_deterministic_reset();
    // SAFETY: the test fully owns the actor and its mailbox.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mailbox);
    }
}

/// V2(a–c): the off-dispatch producer choke point `enter_actor_runtime`
/// binds the actor's OWNING runtime, not the process default. A second
/// worker-less runtime is minted carrying `RuntimeId(1)`, and an actor is
/// stamped to it (both `runtime` pointer and `runtime_id` from that runtime,
/// the spawn invariant). Then:
///   (a) WITHOUT `enter_actor_runtime` the thread is bound to the default
///       (`RuntimeId::DEFAULT`) via the test guard, so a held-pointer send
///       fails closed `ErrForeignRuntime` (the existing cross-runtime wall);
///   (b) WITH `enter_actor_runtime(actor)` the guard binds `RuntimeId(1)` —
///       `rt_current_id()` is asserted to OBSERVE it (anti-vacuous: the bind
///       is checked, not assumed, per `static-classification-vacuates-…`) —
///       and the SAME send now succeeds and enqueues; the guard restores the
///       previous (default) binding on drop (`lifecycle-symmetry`);
///   (c) the by-construction skew invariant holds: the actor's owning-runtime
///       stamp id equals its `runtime_id`.
#[test]
fn enter_actor_runtime_binds_owning_runtime_off_dispatch() {
    let _guard = crate::runtime_test_guard();

    // Mint a second, worker-less runtime carrying RuntimeId(1). It is a stack
    // local that outlives every guard/stamp derived from it below: the
    // actor's `runtime` field points at it and `enter_actor_runtime` borrows
    // it, and both are dropped before `rt_b` leaves scope.
    let rt_b = crate::runtime::RuntimeInner::new_with_id_for_test(
        crate::scheduler::worker_less_scheduler(),
        crate::runtime_id::RuntimeId(1),
    );

    // Build a test actor and re-stamp it as if spawned by rt_b: both the
    // `runtime` pointer and `runtime_id` come from the SAME runtime (the
    // spawn invariant). `Runnable` state so a successful send does not also
    // push it onto a scheduler queue — the mailbox enqueue is what (b)
    // asserts.
    let (actor, mailbox) = make_stop_test_actor_with_id(0xB2, HewActorState::Runnable);
    // SAFETY: the test exclusively owns `actor` and never publishes it.
    unsafe {
        (*actor).runtime_id = crate::runtime_id::RuntimeId(1);
        (*actor).runtime = &raw const rt_b;
    }

    // (c) Skew guard: the owning-runtime stamp's id equals the runtime_id.
    // SAFETY: the test owns `actor` and `rt_b`; the stamp is non-null here.
    unsafe {
        assert!(
            (*actor).runtime.is_null() || (*(*actor).runtime).runtime_id() == (*actor).runtime_id,
            "a spawned actor must carry an owning-runtime stamp whose id equals its runtime_id"
        );
    }

    // (a) WITHOUT enter_actor_runtime: the calling thread is bound to the
    // default runtime, so the foreign actor fails closed and enqueues
    // nothing.
    // SAFETY: `actor` is valid and fully owned by this test; null payload.
    let foreign_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
    assert_eq!(
        foreign_rc,
        HewError::ErrForeignRuntime as i32,
        "without entering the owner, an off-dispatch send is foreign and fails closed"
    );
    // SAFETY: `mailbox` is owned by the test.
    let refused_count = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
    assert_eq!(
        refused_count, 0,
        "a refused cross-runtime send must not enqueue a message"
    );

    // (b) WITH enter_actor_runtime: the choke point binds rt_b, observed via
    // rt_current_id; the same send is now in-runtime and reaches the mailbox.
    {
        // SAFETY: `actor` is live and owns a non-null `runtime` stamp to
        // `rt_b`, which outlives this guard.
        let _bind = unsafe { crate::runtime::enter_actor_runtime(actor) }
            .expect("entering the owning runtime yields a guard");
        assert_eq!(
            crate::runtime::rt_current_id(),
            Some(crate::runtime_id::RuntimeId(1)),
            "enter_actor_runtime must bind the actor's owning runtime, not the default"
        );

        // SAFETY: as above.
        let bound_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            bound_rc,
            HewError::Ok as i32,
            "with the owner bound, the off-dispatch send is in-runtime and succeeds"
        );
        // SAFETY: `mailbox` is owned by the test.
        let accepted_count = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
        assert_eq!(
            accepted_count, 1,
            "the accepted send must have enqueued exactly the one message"
        );
    }

    // The guard restored the previous (default) binding on drop.
    assert_eq!(
        crate::runtime::rt_current_id(),
        Some(crate::runtime_id::RuntimeId::DEFAULT),
        "dropping the enter_actor_runtime guard restores the previous (default) binding"
    );

    // SAFETY: the test fully owns the actor and its mailbox; drop the actor
    // before `rt_b` leaves scope so its `runtime` stamp is never read after.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mailbox);
    }
}

/// V2(d): `enter_actor_runtime` TRAPS — it does not silently default and does
/// not return `None` — when an actor carries a non-default `runtime_id` but a
/// NULL owning-runtime stamp. That pairing is a spawn-invariant contradiction
/// (spawn stamps `runtime` and `runtime_id` from the same runtime), so
/// silently binding the default would re-open the silent-default hazard the
/// stamp closes (`no-fail-open-fallback-after-authority`). The DEFAULT path
/// is unaffected: a default actor with a null stamp resolves the default
/// fallback exactly as in M2.
#[test]
fn enter_actor_runtime_traps_on_multi_runtime_null_stamp() {
    let _guard = crate::runtime_test_guard();

    // Control (the legitimate M2 path): a DEFAULT-stamped actor with a null
    // `runtime` resolves the default fallback — NO trap, returns Some.
    let (default_actor, default_mailbox) = make_stop_test_actor_with_id(0xD0, HewActorState::Idle);
    // SAFETY: the test owns `default_actor`; it carries runtime_id DEFAULT and
    // a null runtime stamp (the helper defaults).
    let default_bound = unsafe { crate::runtime::enter_actor_runtime(default_actor) };
    assert!(
        default_bound.is_some(),
        "a DEFAULT actor with a null stamp must resolve the default fallback (no trap)"
    );
    drop(default_bound);

    // The trap: a non-default runtime_id with a null stamp must panic rather
    // than silently default.
    let (foreign_actor, foreign_mailbox) = make_stop_test_actor_with_id(0xD1, HewActorState::Idle);
    // SAFETY: the test owns `foreign_actor`; stamp a non-default id but leave
    // `runtime` null — precisely the contradiction the trap exists to catch.
    unsafe {
        (*foreign_actor).runtime_id = crate::runtime_id::RuntimeId(1);
    }
    let trapped = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        // SAFETY: `foreign_actor` is live and owned by the test; the trap
        // fires before any runtime is entered, so no guard leaks.
        let _ = unsafe { crate::runtime::enter_actor_runtime(foreign_actor) };
    }));
    assert!(
        trapped.is_err(),
        "a non-default runtime_id with a null owning-runtime stamp must TRAP, not default"
    );

    // SAFETY: the test fully owns both actors and mailboxes.
    unsafe {
        drop(Box::from_raw(default_actor));
        mailbox::hew_mailbox_free(default_mailbox);
        drop(Box::from_raw(foreign_actor));
        mailbox::hew_mailbox_free(foreign_mailbox);
    }
}

static WAKE_SAW_RECORDED_FAULT: AtomicBool = AtomicBool::new(false);
static WAKE_HOOK_FIRED: AtomicBool = AtomicBool::new(false);

fn record_fault_visibility_at_first_wake(event: c_int) {
    if event == HEW_ACTOR_CRASH_TEARDOWN_BEFORE_FIRST_WAKE {
        WAKE_HOOK_FIRED.store(true, Ordering::SeqCst);
        WAKE_SAW_RECORDED_FAULT.store(
            crate::exit_status::unrecovered_actor_fault(),
            Ordering::SeqCst,
        );
    }
}

/// THE CRASH IS ACCOUNTED FOR BEFORE ANYONE CAN SEE IT.
///
/// The teardown below this point wakes other threads: the mailbox close
/// releases blocked senders and the queued-terminal reclaim retires pending
/// asks, completing a waiter's `await`. Whatever that waiter does next — an
/// `exit(0)` included — must not be able to read an exit status that has
/// not yet been told about this crash.
///
/// Recording the fault after those wake-ups made the exit status a race
/// between the crashing thread and the thread it woke. Linux and macOS won
/// it; Windows lost it, and `exit(0)` reported success over this very
/// crash. Asserting at the wake point makes the ordering a property instead
/// of a scheduler coincidence, so no platform can decide it.
#[test]
fn an_unsupervised_crash_is_recorded_before_the_teardown_wakes_anyone() {
    let _guard = crate::runtime_test_guard();
    crate::exit_status::reset_process_exit_status();
    WAKE_HOOK_FIRED.store(false, Ordering::SeqCst);
    WAKE_SAW_RECORDED_FAULT.store(false, Ordering::SeqCst);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    hew_actor_set_crash_teardown_order_hook(Some(record_fault_visibility_at_first_wake));
    // SAFETY: the actor is live and owned by this test.
    unsafe { hew_actor_trap(actor, 99) };
    hew_actor_set_crash_teardown_order_hook(None);

    assert!(
        WAKE_HOOK_FIRED.load(Ordering::SeqCst),
        "the crash teardown must reach its first wake point"
    );
    assert!(
        WAKE_SAW_RECORDED_FAULT.load(Ordering::SeqCst),
        "an unsupervised crash must already be on the exit-status authority \
             when the teardown starts releasing other threads"
    );

    // SAFETY: the actor is terminal and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    crate::exit_status::reset_process_exit_status();
}

#[test]
fn actor_crash_cancels_current_task_scope() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: test owns the scope pointer and restores the context before teardown.
    unsafe {
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());
        let scope = crate::task_scope::checked::hew_checked_scope_new(ptr::null_mut());
        let previous = crate::task_scope::hew_task_scope_set_current(scope);

        hew_actor_trap(actor, 99);

        assert_eq!(
            crate::cancel_token::hew_cancel_token_is_requested((*scope).cancel_token),
            1
        );
        let _ = crate::task_scope::hew_task_scope_set_current(previous);
        crate::task_scope::checked::hew_checked_scope_close(scope);
        assert_eq!(hew_actor_free(actor), 0);
    }
}

#[cfg_attr(
    not(unix),
    allow(
        dead_code,
        reason = "only consumed by the unix-gated free-during-reactor-detach race test"
    )
)]
static REACTOR_WAKE_HOOK_FIRED: AtomicBool = AtomicBool::new(false);

/// Pre-detach hook that models a reactor delivery waking the actor during
/// the `hew_actor_free` detach window. Runs after free observed the actor
/// quiescent (`Idle`) but before `reactor_detach_actor`, and performs the
/// exact wake side-effect a real delivery's `hew_actor_try_send` does:
/// `CAS Idle->Runnable` + `sched_enqueue`. This is the side effect detach
/// does not undo and that the buggy free path freed under. Self-contained
/// (uses only the `actor` argument) so it can be a plain `fn` pointer, and
/// it deliberately does NOT touch the process-global `DELIVERING_ACTOR`
/// guard, so it needs no cross-test serialization with the reactor tests.
#[cfg_attr(
    not(unix),
    allow(
        dead_code,
        reason = "only consumed by the unix-gated free-during-reactor-detach race test"
    )
)]
fn reactor_wake_during_detach_hook(actor: *mut HewActor) {
    // SAFETY: the free path holds the actor live across the hook; it is the
    // same pointer free is about to detach.
    let a = unsafe { &*actor };
    // The wake side-effect: a reactor `on_data` delivery's
    // `hew_actor_try_send` CASes Idle->Runnable and enqueues the actor.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        scheduler::sched_enqueue(actor);
    }
    REACTOR_WAKE_HOOK_FIRED.store(true, Ordering::Release);
}

/// A reactor delivery that wakes + enqueues an actor *during* the
/// `hew_actor_free` reactor-detach window must never let that actor be
/// freed while a live pointer to it remains in a scheduler queue
/// (use-after-free in `activate_actor`).
///
/// Forced ordering (deterministic, no timing luck): a worker-less scheduler
/// guarantees nothing drains the queue, and the pre-detach hook performs the
/// wake inline in the exact window between free's pre-detach quiescence read
/// and `reactor_detach_actor`. So every run reproduces the race. The hook
/// performs only the actor-local wake (no global `DELIVERING_ACTOR` write),
/// so the test is self-contained and does not race the reactor tests.
///
/// With the producer-side post-detach re-check, free observes the actor is
/// `Runnable` (woken during detach) and refuses to free it: it returns -2
/// ("still running") and the actor stays tracked + queued + intact.
///
/// WITHOUT the fix (free using only the pre-detach quiescence read), free
/// would untrack + free the actor and return 0, leaving a dangling pointer
/// in the global queue — the bug. The assertions below (`rc == -2`, actor
/// still live in `LIVE_ACTORS`, still Runnable, identity intact, pointer still
/// queued) all flip in that case: `rc` would be 0 and the queued pointer
/// would reference freed memory (a genuine UAF when later activated, caught
/// under a sanitizer). Verified: reverting the producer-side re-check makes
/// this test fail at the `rc == -2` assertion with the observed `rc == 0`.
#[test]
#[cfg(unix)]
fn free_refuses_actor_woken_by_reactor_during_detach() {
    let _guard = crate::runtime_test_guard();
    // Worker-less scheduler: sched_enqueue works, nothing drains the queue.
    // The guard holds SCHED_TEST_MUTEX, serializing against scheduler tests.
    let sched = scheduler::NoWorkerSchedulerForTest::install();
    // Also hold the tracing lock (consistent lock order: SCHED then tracing):
    // this test's `hew_actor_close`/free emits SPAN_STOP lifecycle events into
    // the process-global trace ring whenever tracing is enabled, which would
    // otherwise race a concurrent tracing/span test's ring assertions.
    let _tracing = crate::tracing::tracing_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is valid and owned by this test.
    let actor_id = unsafe { (*actor).id };

    // Freshly spawned actors are Idle (quiescent) — free's pre-detach check
    // will pass, then the hook wakes the actor during detach.
    // SAFETY: actor is valid (just spawned, owned by this test).
    let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(spawned_state, HewActorState::Idle as i32);

    REACTOR_WAKE_HOOK_FIRED.store(false, Ordering::Release);
    set_free_pre_detach_hook_for_test(Some(reactor_wake_during_detach_hook));

    // SAFETY: actor is valid; free is the operation under test.
    let rc = unsafe { hew_actor_free(actor) };

    // Always clear the hook so it cannot affect teardown or other tests.
    set_free_pre_detach_hook_for_test(None);

    assert!(
        REACTOR_WAKE_HOOK_FIRED.load(Ordering::Acquire),
        "pre-detach hook must have fired — the test did not exercise the race"
    );
    assert_eq!(
        rc, -2,
        "hew_actor_free must REFUSE to free an actor woken+enqueued during \
             reactor detach (got {rc}; rc==0 means the queued actor was freed — UAF)"
    );
    // The actor must still be tracked and intact (not freed).
    assert!(
        live_actors::is_actor_live(actor),
        "refused-free actor must remain tracked in LIVE_ACTORS"
    );
    // SAFETY: assertion above proves the actor is still live (not freed).
    let queued_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(
        queued_state,
        HewActorState::Runnable as i32,
        "refused-free actor must remain Runnable (woken by the delivery)"
    );
    // SAFETY: actor still live; reading its stable id is sound.
    assert_eq!(unsafe { (*actor).id }, actor_id, "actor identity intact");

    // The wake left a live pointer in the global queue. It must be the
    // (still-valid) actor — not a dangling pointer to freed memory.
    let queued = sched.pop_global();
    assert_eq!(
        queued,
        Some(actor),
        "the woken actor's pointer must still be queued and valid"
    );

    // Teardown: return the actor to Idle so the final free succeeds. The
    // mailbox is empty, so a real activation would simply CAS Runnable->Idle;
    // do that directly here to avoid emitting tracing span events into the
    // process-global trace ring (which a concurrent tracing test asserts on).
    // SAFETY: actor is still live; this test exclusively owns it now.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Release);
    }
    // Drain the stale pointer the wake left in the global queue before the
    // box is freed, so nothing dequeues it after free.
    assert_eq!(
        sched.pop_global(),
        None,
        "the single queued pointer was already consumed above"
    );
    // SAFETY: actor is valid and back to Idle.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(hew_actor_free(actor), 0);
    }
    drop(sched);
}

static POST_LATCH_WAKE_HOOK_FIRED: AtomicBool = AtomicBool::new(false);
static POST_LATCH_WAKE_SUCCEEDED: AtomicBool = AtomicBool::new(false);

/// Post-latch hook that models a non-reactor wake — the exact link/monitor
/// exit/down propagation side effect (`send_exit_signal` /
/// `send_down_notification`) — firing in the window between free latching the
/// actor out of `Idle` and `untrack_actor`. It routes through the *real*
/// `with_live_actor_by_id` guard (holding the `LIVE_ACTORS` lock, exactly as
/// the production link/monitor paths do) and performs the producer-side
/// `CAS Idle->Runnable` + `sched_enqueue`. Self-contained (uses only the
/// `actor` argument), so it can be a plain `fn` pointer. Whether that CAS
/// succeeds is the load-bearing observation:
///   - WITH the latch: free has already CAS'd the actor to `Stopped`, so this
///     CAS fails — no enqueue, no queued-after-free, no UAF.
///   - WITHOUT the latch (free breaking on the bare post-detach `Idle`): the
///     actor is still `Idle`, this CAS succeeds and enqueues a pointer that
///     free then untracks + frees → dangling queue entry (the verdict's UAF).
fn nonreactor_wake_post_latch_hook(actor: *mut HewActor) {
    // SAFETY: free holds the actor live across this hook; it is the same
    // pointer free is about to untrack.
    let id = unsafe { (*actor).id };
    let woke = with_live_actor_by_id(id, actor, |a_ref| {
        if a_ref
            .actor_state
            .compare_exchange(
                HewActorState::Idle as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            scheduler::sched_enqueue(actor);
            true
        } else {
            false
        }
    });
    if woke == Some(true) {
        POST_LATCH_WAKE_SUCCEEDED.store(true, Ordering::Release);
    }
    POST_LATCH_WAKE_HOOK_FIRED.store(true, Ordering::Release);
}

/// A non-reactor wake — in-flight link/monitor exit/down propagation (or a
/// direct actor-to-actor send) for a crashing peer — must never enqueue an
/// actor that `hew_actor_free` is about to untrack + free. This is the defect
/// the independent review reproduced and BLOCKED on: the reactor fix
/// closed only the reactor wake; a non-reactor waker could still
/// `CAS Idle->Runnable` + `sched_enqueue` in the window between free's
/// post-detach `Idle` observation and `untrack_actor`, after which free freed
/// a still-queued actor → UAF in `activate_actor`.
///
/// Forced ordering (deterministic, no timing luck): a worker-less scheduler
/// guarantees nothing drains the queue, and the post-latch hook performs the
/// real link/monitor wake inline in the exact window between free latching the
/// actor out of `Idle` and `untrack_actor`. The wake routes through the same
/// `with_live_actor_by_id` guard the production link/monitor paths use, so the
/// test exercises the production reachability, not a synthetic shortcut.
///
/// WITH the producer-side Idle->Stopped latch: by the time the hook runs the
/// actor is `Stopped`, so the waker's `CAS Idle->Runnable` FAILS — nothing is
/// enqueued, free completes cleanly (`rc == 0`), and the global queue is empty
/// (no queued-after-free). The assertions below encode exactly that.
///
/// WITHOUT the latch (revert step 3 to break on the bare post-detach `Idle`):
/// the hook's CAS succeeds, `sched_enqueue` leaves a live pointer in the
/// queue, and free untracks + frees it → `POST_LATCH_WAKE_SUCCEEDED == true`
/// and a dangling pointer is observable via `sched.pop_global()` after the box
/// is freed (the UAF; would trip ASAN on a later `activate_actor`). Verified:
/// reverting the latch flips this test to fail at the
/// `!POST_LATCH_WAKE_SUCCEEDED` assertion (observed `rc=0 queued_after_free=true`).
#[test]
fn free_latches_actor_against_nonreactor_wake_before_untrack() {
    let _guard = crate::runtime_test_guard();
    // Worker-less scheduler: sched_enqueue works, nothing drains the queue, so
    // any wake-enqueued pointer stays observable. The guard holds
    // SCHED_TEST_MUTEX, serializing against scheduler tests.
    let sched = scheduler::NoWorkerSchedulerForTest::install();
    // Hold the tracing lock too (consistent lock order: SCHED then tracing):
    // free's terminate/finalize path emits lifecycle events into the
    // process-global trace ring when tracing is enabled, which would otherwise
    // race a concurrent tracing/span test's ring assertions.
    let _tracing = crate::tracing::tracing_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // Freshly spawned actors are Idle (quiescent) — free's wait + post-detach
    // reload both observe Idle, then free latches Idle->Stopped before the
    // post-latch hook fires the non-reactor wake.
    // SAFETY: actor is valid (just spawned, owned by this test).
    let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(spawned_state, HewActorState::Idle as i32);

    POST_LATCH_WAKE_HOOK_FIRED.store(false, Ordering::Release);
    POST_LATCH_WAKE_SUCCEEDED.store(false, Ordering::Release);
    set_free_post_latch_hook_for_test(Some(nonreactor_wake_post_latch_hook));

    // SAFETY: actor is valid; free is the operation under test.
    let rc = unsafe { hew_actor_free(actor) };

    // Always clear the hook so it cannot affect teardown or other tests.
    set_free_post_latch_hook_for_test(None);

    assert!(
        POST_LATCH_WAKE_HOOK_FIRED.load(Ordering::Acquire),
        "post-latch hook must have fired — the test did not exercise the window"
    );
    // The load-bearing assertion: the non-reactor waker's CAS Idle->Runnable
    // must FAIL because free latched the actor to Stopped first. If it
    // succeeds, the producer-side latch did not close the window (the UAF).
    assert!(
        !POST_LATCH_WAKE_SUCCEEDED.load(Ordering::Acquire),
        "a non-reactor wake CAS'd Idle->Runnable in the free window — free \
             latched the actor too late (this is the use-after-free the latch must close)"
    );
    // With the wake blocked, free completes cleanly.
    assert_eq!(
        rc, 0,
        "hew_actor_free must succeed once the actor is wake-proof (got {rc})"
    );
    // The actor is freed and no longer tracked.
    assert!(
        !live_actors::is_actor_live(actor),
        "freed actor must no longer be tracked in LIVE_ACTORS"
    );
    // No pointer was left in the global queue — nothing dangles after free.
    let queued_after_free = sched.pop_global();
    assert_eq!(
        queued_after_free, None,
        "no actor pointer may remain queued after free (a queued pointer here \
             would dangle — the use-after-free)"
    );
    drop(sched);
}

/// A queue reference is acquired before publishing the raw actor pointer.
/// Forced trap/free cannot reclaim the box while the producer is paused in
/// that window, and remains blocked after publication until the entry is
/// removed. The exact no-reference counterfactual frees first and leaves
/// the same raw address queued.
#[test]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the red-first queue-publication fixture keeps raw actor lifetime operations adjacent to the two compared protocol branches"
)]
fn scheduler_enqueue_reference_closes_terminal_free_uaf() {
    unsafe fn run_case(sched: &scheduler::NoWorkerSchedulerForTest, own_queue_ref: bool) {
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        let id = unsafe { (*actor).id };
        if !own_queue_ref {
            unsafe {
                (*actor)
                    .actor_state
                    .store(HewActorState::Runnable as i32, Ordering::Release);
            }
        }

        let (hook, entered, release) =
            scheduler::SchedulerQueueHandoffHookGuard::install_enqueue_pre_publish(id);
        let actor_addr = actor.addr();
        let producer = std::thread::spawn(move || {
            let actor = ptr::with_exposed_provenance_mut::<HewActor>(actor_addr);
            if own_queue_ref {
                // SAFETY: the test keeps this actor live through the call.
                // The canonical producer takes queue ownership before its
                // Idle -> Runnable transition.
                unsafe { finish_mailbox_enqueue(actor, &*actor) };
            } else {
                // SAFETY: actor is live on hook entry; this is the exact
                // missing-retain counterfactual.
                unsafe { scheduler::sched_enqueue_omitting_queue_ref_for_test(actor) };
            }
        });
        entered.wait();

        // SAFETY: actor is live at the rendezvous.
        unsafe { hew_actor_trap(actor, 1) };
        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let free = std::thread::spawn(move || {
            // SAFETY: ownership is transferred to this free thread.
            let rc =
                unsafe { hew_actor_free(ptr::with_exposed_provenance_mut::<HewActor>(actor_addr)) };
            done_tx.send(rc).expect("free result receiver");
        });

        if own_queue_ref {
            assert!(
                matches!(
                    done_rx.try_recv(),
                    Err(std::sync::mpsc::TryRecvError::Empty)
                ),
                "queue reference must pin actor before raw-pointer publish"
            );
            release.wait();
            producer.join().expect("enqueue producer");
            assert!(
                matches!(
                    done_rx.try_recv(),
                    Err(std::sync::mpsc::TryRecvError::Empty)
                ),
                "published queue entry must retain actor after producer returns"
            );
            assert_eq!(
                sched.pop_global(),
                Some(actor),
                "removing the exact queue entry releases its lifetime ref"
            );
        } else {
            assert_eq!(
                done_rx
                    .recv_timeout(std::time::Duration::from_secs(2))
                    .expect("omission permits free before publish"),
                0,
                "without the queue reference terminal free wins the rendezvous"
            );
            release.wait();
            producer.join().expect("counterfactual producer");
            assert_eq!(
                sched.pop_global_without_queue_ref(),
                Some(ptr::with_exposed_provenance_mut::<HewActor>(actor_addr)),
                "omission leaves the freed raw address queued"
            );
        }

        drop(hook);
        if own_queue_ref {
            assert_eq!(done_rx.recv().expect("free result"), 0);
        }
        free.join().expect("free thread");
    }

    let _guard = crate::runtime_test_guard();
    let sched = scheduler::NoWorkerSchedulerForTest::install();
    // The no-reference counterfactual must demonstrate the stale pointer
    // first; production then proves both sides of the handoff pin.
    unsafe {
        run_case(&sched, false);
        run_case(&sched, true);
    }
    drop(sched);
}

/// A dequeued entry keeps its queue reference until `dispatch_active` is
/// successfully claimed. Trap/free is held out at the exact popped-before-
/// claim seam. Dropping that reference first lets free reclaim the actor
/// while the worker still holds its raw pointer.
#[test]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the red-first pop-to-claim fixture uses explicit raw pointers to witness the scheduler lifetime handoff"
)]
fn scheduler_pop_to_claim_reference_closes_terminal_free_uaf() {
    let _guard = crate::runtime_test_guard();
    let sched = scheduler::NoWorkerSchedulerForTest::install();

    // Counterfactual: pop, release the only queue ref before claim, then
    // terminal free can complete while the worker-local raw address remains.
    let omitted = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!omitted.is_null());
    unsafe {
        (*omitted)
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
    }
    scheduler::sched_enqueue(omitted);
    assert_eq!(sched.take_global_with_queue_ref(), Some(omitted));
    unsafe { scheduler::release_scheduler_queue_ref_for_test(omitted) };
    let omitted_addr = omitted.addr();
    unsafe {
        hew_actor_trap(omitted, 1);
        assert_eq!(hew_actor_free(omitted), 0);
    }
    assert_eq!(
        omitted_addr,
        omitted.addr(),
        "worker-local raw address survives only as a stale pointer"
    );

    // Production: the real activation pauses after pop while its queue ref
    // still owns the allocation.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    let id = unsafe { (*actor).id };
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
    }
    scheduler::sched_enqueue(actor);
    let (hook, entered, release) =
        scheduler::SchedulerQueueHandoffHookGuard::install_activate_pre_claim(id);
    let sched_addr = (&raw const sched).addr();
    let activation = std::thread::spawn(move || {
        // SAFETY: the guard outlives this joined activation.
        let sched = unsafe {
            &*ptr::with_exposed_provenance::<scheduler::NoWorkerSchedulerForTest>(sched_addr)
        };
        assert!(sched.activate_one_global());
    });
    entered.wait();
    unsafe { hew_actor_trap(actor, 1) };

    let actor_addr = actor.addr();
    let (done_tx, done_rx) = std::sync::mpsc::channel();
    let free = std::thread::spawn(move || {
        let rc =
            unsafe { hew_actor_free(ptr::with_exposed_provenance_mut::<HewActor>(actor_addr)) };
        done_tx.send(rc).expect("free result receiver");
    });
    assert!(
        matches!(
            done_rx.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ),
        "popped queue reference must block free before activation claim"
    );

    release.wait();
    activation.join().expect("activation thread");
    assert_eq!(done_rx.recv().expect("free result"), 0);
    free.join().expect("free thread");
    drop(hook);
    drop(sched);
}

// ── Forced-ordering test: free must drain send pins before finalizing ──

/// Set to `true` by the background thread spawned by
/// `post_latch_inject_send_pin` BEFORE it decrements `send_pin_count`.
/// On the fixed code the freer spins until the pin drops, so the bg
/// thread runs while the freer is blocked and `SEND_PIN_DRAIN_WAITED` is
/// `true` when the freer proceeds to finalize.  Without the post-untrack
/// pin-drain loop the freer proceeds to finalize immediately (no spin); the
/// bg thread is still sleeping so `SEND_PIN_DRAIN_WAITED` is `false` when
/// `hew_actor_free` returns → assertion fails → test fails.
static SEND_PIN_DRAIN_WAITED: AtomicBool = AtomicBool::new(false);

/// Set to `true` by the test thread immediately after `hew_actor_free`
/// returns.  The background thread checks this flag before decrementing
/// the pin: on the old (unfixed) code, free returned while the bg thread
/// was still sleeping; setting CANCEL prevents the bg thread from
/// performing a use-after-free write to the freed actor box.
static SEND_PIN_TEST_CANCEL: AtomicBool = AtomicBool::new(false);

/// Post-latch hook for `free_waits_for_send_pin_drain_before_finalize`.
///
/// Simulates a concurrent by-ID sender that managed to pin the actor
/// after the quiescence check but before `untrack_actor`.  Increments
/// `send_pin_count` directly (as `with_actor_send_by_id` would) and
/// spawns a background thread that will release the pin after a delay
/// long enough to distinguish "free waited" from "free raced ahead".
fn post_latch_inject_send_pin(actor: *mut HewActor) {
    // Simulate pin-increment (the step with_actor_send_by_id performs
    // under LIVE_ACTORS before releasing the lock).
    // SAFETY: the actor is still live; free holds it across this hook.
    unsafe { (*actor).send_pin_count.fetch_add(1, Ordering::AcqRel) };

    // Cast to usize so the closure captures a Send-safe integer.
    // (RFC 2229 field-level capture would capture `ap.0: *mut HewActor`
    // — not Send — if we used a newtype wrapper with a field access.)
    let actor_addr = actor as usize;
    std::thread::spawn(move || {
        // Sleep long enough that an unblocked freer returns before we run.
        std::thread::sleep(std::time::Duration::from_millis(60));

        // Check the cancel flag set by the test thread after free returns.
        // Without the post-untrack pin-drain spin, free returns immediately
        // and CANCEL is set before we wake — we must NOT do the fetch_sub on
        // the now-freed box.
        if SEND_PIN_TEST_CANCEL.load(Ordering::Acquire) {
            return;
        }

        // Record that we ran before decrementing — the freer must observe
        // this flag as `true` when it proceeds past the pin drain loop.
        SEND_PIN_DRAIN_WAITED.store(true, Ordering::Release);

        // Release the pin.  The freer's Acquire load of send_pin_count
        // pairs with this Release, so it sees all writes we made above.
        let actor_ptr = actor_addr as *mut HewActor;
        // SAFETY: CANCEL was false → free is still spinning (pin drain loop),
        // so the actor box is still live and the atomic write is valid.
        unsafe { (*actor_ptr).send_pin_count.fetch_sub(1, Ordering::Release) };
    });
}

/// Verify that `hew_actor_free` drains all send pins **before** finalizing
/// (calling terminate + freeing the box), not before the quiescence check.
///
/// **Why this test catches the post-untrack pin-drain TOCTOU bug:**
///
/// The pre-drain form checked `send_pin_count == 0` inside the quiescence
/// *wait loop* (before the latch), not after `untrack_actor`.  The
/// post-latch hook fires AFTER the latch succeeds but BEFORE `untrack_actor`.
/// In that window, `with_actor_send_by_id` can still find the actor in the
/// map and increment the pin — the quiescence check that passed was
/// already stale.  That form then proceeded to `untrack_actor` +
/// `finalize` without waiting → use-after-free.
///
/// **Pre-drain form (FAIL):** the hook increments `send_pin_count`; free
/// has no pin-drain loop after `untrack_actor` → proceeds to finalize
/// immediately → returns in < 5 ms.  The bg thread wakes at 60 ms, finds
/// `CANCEL == true` (set just after free returned), skips the `fetch_sub`
/// (no UAF).  `SEND_PIN_DRAIN_WAITED` is `false` → assertion fails.
///
/// **On fixed code (PASS):** free calls `untrack_actor`, then spins on
/// `send_pin_count`.  The bg thread wakes at 60 ms, sees `CANCEL ==
/// false` (free still spinning), stores `SEND_PIN_DRAIN_WAITED = true`,
/// decrements pin.  Free sees pin == 0, finalize, return.  Assertion
/// passes.
#[test]
fn free_waits_for_send_pin_drain_before_finalize() {
    let _guard = crate::runtime_test_guard();
    // Worker-less scheduler: same setup as the latch / reactor tests.
    let sched = scheduler::NoWorkerSchedulerForTest::install();
    let _tracing = crate::tracing::tracing_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // Freshly spawned actors are Idle — the quiescence wait passes
    // immediately so the post-latch hook fires before untrack.
    // SAFETY: actor is valid (just spawned); the state field is always initialized.
    let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(spawned_state, HewActorState::Idle as i32);

    SEND_PIN_DRAIN_WAITED.store(false, Ordering::Release);
    SEND_PIN_TEST_CANCEL.store(false, Ordering::Release);
    set_free_post_latch_hook_for_test(Some(post_latch_inject_send_pin));

    // SAFETY: actor is valid; free is the operation under test.
    let rc = unsafe { hew_actor_free(actor) };

    set_free_post_latch_hook_for_test(None);
    // Signal the bg thread: if free returned before the bg thread ran
    // (the pre-drain regression path), the bg thread must not touch the
    // freed box.
    SEND_PIN_TEST_CANCEL.store(true, Ordering::Release);

    // Fixed code: free spun until the bg thread decremented send_pin_count;
    // the bg thread stored WAITED = true before decrementing, so when free
    // proceeded to finalize it observed WAITED = true, meaning all pins
    // were drained before finalize ran.
    //
    // Pre-drain form: free returned before the bg thread ran → WAITED = false.
    assert!(
        SEND_PIN_DRAIN_WAITED.load(Ordering::Acquire),
        "hew_actor_free must drain all send pins before finalizing the actor \
             box; if this assertion fails, finalize ran while a send pin was held \
             (use-after-free window)"
    );
    assert_eq!(rc, 0, "hew_actor_free must succeed (got {rc})");
    assert!(
        !live_actors::is_actor_live(actor),
        "freed actor must no longer be tracked in LIVE_ACTORS"
    );
    drop(sched);
}

// ── System-channel invariants, as tests rather than as prose ────────
//
// Three of the justifications for reaching system-channel state from a
// reachable-but-defensible position were paragraphs. A paragraph does not
// fail when the code moves underneath it, so each one that can be checked
// mechanically is checked here instead.

/// JUSTIFICATION UNDER TEST: no user-declarable spawn entry point can
/// install a system dispatch pointer, because the slot has no parameter in
/// any spawn argument list.
///
/// All four entry points are exercised, including both `HewActorOpts`
/// forms — `HewActorOpts` is the only spawn argument that is a struct, so
/// it is the only one where a field could be added without changing a
/// function signature, and it is therefore the one worth pinning. Add a
/// system-dispatch parameter or field to any of them and wire it through,
/// and this test fails on that entry point.
#[test]
fn no_spawn_entry_point_installs_a_system_dispatch() {
    let _guard = crate::runtime_test_guard();

    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(noop_dispatch),
        mailbox_capacity: 0,
        overflow: HewOverflowPolicy::DropOld as i32,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        message_drop_fn: None,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };

    // SAFETY: null state with size 0 and a valid dispatch are valid spawn
    // arguments for every entry point; `opts` outlives both calls, and the
    // adopt form is documented to take ownership of the cloned-state
    // pointer, which is null here.
    let spawned: [(&str, *mut HewActor); 4] = unsafe {
        [
            (
                "hew_actor_spawn",
                hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)),
            ),
            (
                "hew_actor_spawn_bounded",
                hew_actor_spawn_bounded(ptr::null_mut(), 0, Some(noop_dispatch), 8),
            ),
            (
                "hew_actor_spawn_opts",
                hew_actor_spawn_opts(&raw const opts),
            ),
            (
                "hew_actor_spawn_opts_adopt",
                hew_actor_spawn_opts_adopt(&raw const opts, ptr::null_mut()),
            ),
        ]
    };

    for (name, actor) in spawned {
        assert!(!actor.is_null(), "{name} must spawn");
        // SAFETY: actor was just spawned and is not being dispatched.
        let installed = unsafe { (*actor).sys_dispatch };
        assert!(
            installed.is_none(),
            "{name} left a system dispatch installed; no spawn argument may reach that slot"
        );
        // SAFETY: actor is valid, Idle, and owned solely by this test.
        let rc = unsafe { hew_actor_free(actor) };
        assert_eq!(rc, 0, "{name}: teardown must succeed (got {rc})");
    }
}

static QUEUE_DESTROY_OBSERVED_STATE: AtomicI32 = AtomicI32::new(-1);
static QUEUE_DESTROY_OBSERVED_TRACKED: AtomicBool = AtomicBool::new(true);
static QUEUE_DESTROY_RAN: AtomicBool = AtomicBool::new(false);

fn observe_at_queue_destroy(actor: *mut HewActor) {
    // SAFETY: the hook fires inside teardown, before the box is reclaimed,
    // so `actor` is still a live allocation.
    let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    QUEUE_DESTROY_OBSERVED_STATE.store(state, Ordering::Release);
    QUEUE_DESTROY_OBSERVED_TRACKED.store(live_actors::is_actor_live(actor), Ordering::Release);
    QUEUE_DESTROY_RAN.store(true, Ordering::Release);
}

/// JUSTIFICATION UNDER TEST: teardown reaches destruction of the actor's
/// system queue only after the actor has been latched into a terminal
/// state and removed from live tracking.
///
/// That ordering is the whole defence for destroying a queue that
/// producers can otherwise push into: once the actor is terminal, every
/// producer's `CAS Idle->Runnable` fails, and once it is untracked no new
/// producer can find it by id at all. Prose cannot notice when the order
/// changes. The hook fires on the instruction before `hew_mailbox_free`,
/// so these two reads are taken AT destruction, not near it.
///
/// Counterfactual: move the mailbox free above the `Idle->Stopped` latch
/// and the state assertion trips; move it above `untrack_actor` and the
/// tracking assertion trips.
#[test]
fn teardown_reaches_queue_destruction_only_after_terminal_and_untracked() {
    let _guard = crate::runtime_test_guard();
    let sched = scheduler::NoWorkerSchedulerForTest::install();

    QUEUE_DESTROY_RAN.store(false, Ordering::Release);
    QUEUE_DESTROY_OBSERVED_STATE.store(-1, Ordering::Release);
    QUEUE_DESTROY_OBSERVED_TRACKED.store(true, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn arguments.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is valid and freshly spawned.
    let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(spawned_state, HewActorState::Idle as i32);

    set_pre_queue_destroy_hook_for_test(Some(observe_at_queue_destroy));
    // SAFETY: actor is valid and owned solely by this test.
    let rc = unsafe { hew_actor_free(actor) };
    set_pre_queue_destroy_hook_for_test(None);

    assert_eq!(rc, 0, "teardown must succeed (got {rc})");
    assert!(
        QUEUE_DESTROY_RAN.load(Ordering::Acquire),
        "teardown must actually reach the queue destruction it is being observed at"
    );

    let observed = QUEUE_DESTROY_OBSERVED_STATE.load(Ordering::Acquire);
    assert!(
        observed == HewActorState::Stopped as i32 || observed == HewActorState::Crashed as i32,
        "the system queue was destroyed while the actor was in state {observed}; \
             it must be latched terminal (Stopped or Crashed) first, or a producer \
             can still win CAS Idle->Runnable and push into a queue being freed"
    );
    assert!(
        !QUEUE_DESTROY_OBSERVED_TRACKED.load(Ordering::Acquire),
        "the system queue was destroyed while the actor was still tracked; \
             a by-id producer could still have found it"
    );

    drop(sched);
}

/// JUSTIFICATION UNDER TEST: a teardown cannot happen without leaving a
/// countable trace.
///
/// Destroying an actor's system queue destroys whatever lifecycle signals
/// were still undispatched in it. That is tolerable only because it is
/// accounted: every discarded signal is named and counted. The counter is
/// process-wide and other tests tear down mailboxes concurrently, so this
/// asserts a lower bound on the delta — restoring the unaccounted drain
/// moves it to exactly zero, which is what makes the bound non-vacuous.
///
/// This is the actor-level companion to
/// `mailbox_teardown_accounts_for_the_system_signals_it_discards`: that one
/// covers a bare mailbox, this one covers the full actor teardown path the
/// authenticated edge actually names.
#[test]
fn actor_teardown_of_a_pending_signal_moves_the_retirement_counter() {
    let _guard = crate::runtime_test_guard();
    let sched = scheduler::NoWorkerSchedulerForTest::install();

    // SAFETY: null state + valid dispatch are valid spawn arguments.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is valid; its mailbox is live for the actor's lifetime.
    let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };
    assert!(!mb.is_null());

    // SAFETY: the actor is Idle under a worker-less scheduler, so nothing
    // dispatches this signal before teardown observes it.
    let queued = unsafe {
        mailbox::mailbox_send_sys_checked(mb, mailbox::HewSysMsg::Exit, ptr::null_mut(), 0)
    };
    assert!(queued, "the test signal must be queued");
    // SAFETY: mailbox pointer is valid.
    assert_eq!(unsafe { mailbox::hew_mailbox_sys_len(mb) }, 1);

    let before = mailbox::sys_lane_signals_retired();
    // SAFETY: actor is valid and owned solely by this test.
    let rc = unsafe { hew_actor_free(actor) };
    assert_eq!(rc, 0, "teardown must succeed (got {rc})");

    assert!(
        mailbox::sys_lane_signals_retired() > before,
        "actor teardown discarded an undispatched lifecycle signal without counting it"
    );

    drop(sched);
}

/// Forced-ordering regression for the `cleanup_all_actors` re-enqueue UAF
/// in the post-prepare latch window.
///
/// A pinned by-ID sender that incremented `send_pin_count` before
/// `drain_all_for_cleanup` removed the map entry can still be running its
/// send closure, which may CAS `Idle→Runnable` to re-enqueue the actor into
/// the scheduler.  Without the `Idle→Stopped` latch, `cleanup_all_actors`
/// would call `finalize` on a still-queued actor — a UAF.
///
/// This test simulates that race with `CLEANUP_POST_PREPARE_HOOK`: the hook
/// fires after `prepare_quiescent_actor_for_cleanup` and before the latch,
/// performs `CAS Idle→Runnable` (as a concurrent sender would), and then
/// asserts that the latch-fail path skips finalize (the actor is leaked) and
/// does NOT free the allocation.
///
/// **Before the fix**: `cleanup_all_actors` proceeded to finalize regardless
/// of state → the hook would observe state=Runnable post-finalize (freed
/// memory read → UB), and in practice the scheduler would later dereference
/// the queued dangling pointer.
///
/// **After the fix**: `CAS Idle→Stopped` fails (state is already Runnable)
/// → the actor is logged + leaked.  Allocation still valid post-call.
static CLEANUP_REENQUEUE_CAS_SUCCEEDED: AtomicBool = AtomicBool::new(false);

fn reenqueue_for_cleanup_hook(actor: *mut HewActor) {
    // SAFETY: actor is valid (from cleanup_all_actors iteration).
    let a = unsafe { &*actor };
    let ok = a
        .actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok();
    CLEANUP_REENQUEUE_CAS_SUCCEEDED.store(ok, Ordering::Release);
}

#[test]
fn cleanup_skips_actor_reenqueued_during_latch_window() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = scheduler::NoWorkerSchedulerForTest::install();
    let _tracing = crate::tracing::tracing_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // Freshly spawned actors are Idle — the hook will win the
    // CAS Idle→Runnable race before the latch can run.
    // SAFETY: actor is valid (just spawned).
    let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(spawned_state, HewActorState::Idle as i32);

    CLEANUP_REENQUEUE_CAS_SUCCEEDED.store(false, Ordering::Release);

    set_cleanup_post_prepare_hook_for_test(Some(reenqueue_for_cleanup_hook));

    // SAFETY: scheduler is stopped (NoWorkerSchedulerForTest installed);
    // no dispatch is in progress.
    unsafe { cleanup_all_actors() };

    set_cleanup_post_prepare_hook_for_test(None);

    // 1. Hook must have fired and the CAS must have succeeded (state was Idle
    //    when the hook ran, before the latch attempt).
    assert!(
        CLEANUP_REENQUEUE_CAS_SUCCEEDED.load(Ordering::Acquire),
        "hook must fire and CAS Idle→Runnable must succeed in the latch window"
    );

    // 2. The latch-fail path must have SKIPPED finalize: the allocation is
    //    still valid and the state is Runnable (not freed/corrupted).
    // SAFETY: actor was not freed (latch-fail → continue; allocation is valid).
    let post_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(
        post_state,
        HewActorState::Runnable as i32,
        "actor must be Runnable (leaked, not freed) after cleanup latch-fail"
    );

    // 3. The actor must be untracked — drain_all_for_cleanup removed it.
    assert!(
        !live_actors::is_actor_live(actor),
        "actor must be untracked even when cleanup skips finalize"
    );

    // Manual cleanup: cleanup_all_actors deliberately leaked this actor to
    // avoid UAF.  Finalize it here to avoid a test memory leak.  The actor
    // has no terminate_fn (noop_dispatch), so call_terminate_fn is a no-op.
    // SAFETY: actor is valid, untracked, and no concurrent access is possible.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        finalize_quiescent_actor_cleanup(actor, HewActorState::Stopped as i32);
    }
}

/// Deterministic free-vs-leak probe for the `cleanup_all_actors`
/// *snapshot-already-non-Idle* re-enqueue UAF.
///
/// Set by [`cleanup_runnable_leak_state_drop_callback`] when (and only when)
/// `free_actor_resources` runs the codegen state-drop — i.e. when the actor
/// was *finalized* (freed).  A leaked (skipped) actor never reaches finalize,
/// so the counter stays 0.  Reading this global (not the actor box) keeps the
/// assertion well-defined on BOTH the buggy path (box freed) and the fixed
/// path (box leaked): no use-after-free read is needed to tell them apart.
static CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

unsafe extern "C" fn cleanup_runnable_leak_state_drop_callback(_state: *mut c_void) {
    CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

/// Forced-ordering regression for the `cleanup_all_actors`
/// *snapshot-already-non-Idle* re-enqueue UAF.
///
/// Companion to `cleanup_skips_actor_reenqueued_during_latch_window`, which
/// covers the window where the wake CAS lands AFTER cleanup loads the state.
/// This test covers the window a stale-snapshot decision MISSES: a pinned
/// by-ID sender wins `CAS Idle→Runnable` BEFORE cleanup reaches the actor, so
/// the actor is already `Runnable` (re-enqueued) when the finalize decision
/// runs.
///
/// We reproduce that window deterministically and end-to-end: store
/// `Runnable` AND `sched_enqueue` the actor into the scheduler's global
/// queue BEFORE `cleanup_all_actors` runs — exactly the end-state of a
/// pinned by-ID sender that won `CAS Idle→Runnable` + `sched_enqueue` before
/// the sweep reached the actor. `drain_all_for_cleanup` untracks it, and the
/// per-actor finalize decision then observes `Runnable`. After cleanup we
/// DRAIN the queue (`pop_global`) and deref the popped pointer — the
/// "anything ever drains `global_queue` post-cleanup" scenario, where a
/// later consumer pops the queue — proving the leaked pointer still resolves
/// to a live actor (no UAF). On the buggy path that deref would hit freed
/// memory, but the counter assertion fails first, so the deref only runs once
/// the fix has proven the box was leaked, not freed.
///
/// **Under a snapshot-gated decision (FAIL):** cleanup loads `state =
/// Runnable`, the `if state == Idle` short-circuits (no latch CAS, no
/// fail-closed `continue`), and `finalize_quiescent_actor_cleanup(actor,
/// Runnable)` frees the re-enqueued actor — state-drop runs → counter == 1 →
/// assertion fails. (A scheduler queue holding the now-dangling pointer is
/// the UAF.)
///
/// **Under the CAS-result decision (PASS):** the finalize decision attempts
/// `CAS Idle→Stopped`, observes `Err(Runnable)`, and SKIPs (leaks
/// fail-closed) — finalize never runs → counter == 0 → assertion passes.
/// The leaked actor is reclaimed manually at the end so the test does not
/// leak.
#[test]
fn cleanup_skips_actor_already_runnable_before_finalize_decision() {
    let _guard = crate::runtime_test_guard();
    let scheduler = scheduler::NoWorkerSchedulerForTest::install();
    let _tracing = crate::tracing::tracing_test_guard();

    CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

    // Spawn with a sized-block-allocated source so `state` is non-null: the
    // state-drop callback only fires when finalize runs over a non-null,
    // non-crashed state — that is the "was freed" signal.
    // SAFETY: buf_try_alloc returns a valid 8-byte allocation.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the bytes; src is released immediately after.
    let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn copied the bytes; release the source allocation.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid and not being dispatched.
    unsafe {
        hew_actor_set_state_drop(actor, cleanup_runnable_leak_state_drop_callback);
        assert!(
            !(*actor).state.is_null(),
            "spawn must produce a non-null state for the state-drop signal"
        );
        // Simulate the end-state of a pinned by-ID sender that already won
        // `CAS Idle→Runnable` (+ `sched_enqueue`) BEFORE the sweep loop
        // observes this actor — the exact snapshot-already-non-Idle window.
        (*actor)
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
    }

    // Genuinely enqueue the actor, so this is "Runnable AND queued" — the
    // real shape of a won wake CAS, not just a state store. `sched_enqueue`
    // pushes the raw pointer into the global queue and notifies a parker
    // (no worker exists to deref it under NoWorkerSchedulerForTest).
    scheduler::sched_enqueue(actor);

    // SAFETY: scheduler is stopped (NoWorkerSchedulerForTest installed); no
    // dispatch is in progress.
    unsafe { cleanup_all_actors() };

    // Load-bearing assertion: a `Runnable` (re-enqueued) actor must be
    // SKIPPED, not finalized.  state-drop running means finalize ran means
    // the queued actor was freed — the use-after-free.  Reads only the
    // global counter, never the (possibly-freed) actor box.
    assert_eq!(
        CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
        0,
        "cleanup_all_actors must SKIP (leak) an actor that is Runnable at the \
             finalize decision; a non-zero count means it ran finalize over a \
             re-enqueued actor (the snapshot-already-Runnable use-after-free)"
    );

    // Drain the scheduler queue exactly as a post-cleanup consumer would.
    // The pointer cleanup left enqueued must still be VALID. On the buggy
    // (snapshot-gated) path cleanup freed this box, so this pop would return
    // a dangling pointer and the deref below would read freed memory — but
    // the counter assertion above already failed before we reach here, so
    // the deref only ever runs on the fixed (leaked-not-freed) path. This is
    // the "anything ever drains global_queue post-cleanup" scenario, made
    // observable.
    let popped = scheduler.pop_global();
    assert_eq!(
        popped,
        Some(actor),
        "the enqueued actor must still be in the scheduler queue (cleanup \
             leaked it rather than freeing a queued pointer)"
    );
    // SAFETY: reached only because the counter assertion passed, i.e. cleanup
    // leaked (did not free) the actor — the queued pointer is still valid.
    let queued_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(
        queued_state,
        HewActorState::Runnable as i32,
        "the queued pointer must still resolve to the live Runnable actor (no UAF)"
    );

    // The actor must be untracked (drain_all_for_cleanup removed it) even
    // though cleanup skipped finalize.  Pointer-identity probe; no deref.
    assert!(
        !live_actors::is_actor_live(actor),
        "actor must be untracked even when cleanup skips finalize"
    );

    // Manual reclaim: cleanup deliberately leaked this actor to avoid the
    // UAF.  Finalize it here so the test itself does not leak.
    // SAFETY: actor is valid, untracked, pin-free, no concurrent access.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        finalize_quiescent_actor_cleanup(actor, HewActorState::Stopped as i32);
    }
    assert_eq!(
        CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
        1,
        "manual reclaim must run state-drop exactly once (no leak)"
    );
}

/// Deterministic free-vs-leak probe for the bonus `cleanup_all_actors`
/// *Suspended*-finalize fix.  Set by
/// [`cleanup_suspended_leak_state_drop_callback`] only when finalize runs
/// over the actor (i.e. it was freed); a leaked (skipped) actor never reaches
/// finalize, so the counter stays 0.
static CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

unsafe extern "C" fn cleanup_suspended_leak_state_drop_callback(_state: *mut c_void) {
    CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

/// Forced-ordering regression for the bonus `cleanup_all_actors`
/// *Suspended*-finalize leak that the quiescence gate also closes.
///
/// A `Suspended` actor is parked at a non-final `coro.suspend` with a live
/// continuation frame (`suspended_cont`).  `actor_free_state_is_quiescent`
/// excludes `Suspended`, so it is never safe to finalize on the shutdown
/// sweep: `hew_actor_free_inner` destroys the parked frame first, but
/// `cleanup_all_actors` (workers joined) cannot block to do so — it must
/// leak-not-free.
///
/// **Under a snapshot-gated decision (FAIL):** cleanup loads `state =
/// Suspended`, the `if state == Idle` short-circuits (no latch, no
/// fail-closed `continue`), and `finalize_quiescent_actor_cleanup(actor,
/// Suspended)` frees the parked actor — running its state-drop and leaking
/// the continuation frame → counter == 1 → assertion fails.
///
/// **Under the quiescence gate (PASS):** the finalize decision attempts `CAS
/// Idle→Stopped`, observes `Err(Suspended)`, and — because `Suspended` is
/// not `actor_free_state_is_quiescent` — SKIPs (leaks fail-closed) → finalize
/// never runs → counter == 0 → assertion passes.
///
/// The finalize decision reads only `actor_state`, so storing `Suspended` is
/// the faithful observable; no real parked frame is needed to exercise the
/// gate.
#[test]
fn cleanup_skips_suspended_actor_at_finalize_decision() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = scheduler::NoWorkerSchedulerForTest::install();
    let _tracing = crate::tracing::tracing_test_guard();

    CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

    // Non-null state so the state-drop callback is the "was finalized" signal.
    // SAFETY: malloc returns a valid 8-byte allocation or null.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the bytes; src is released immediately after.
    let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn copied the bytes; release the source allocation.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid and not being dispatched.
    unsafe {
        hew_actor_set_state_drop(actor, cleanup_suspended_leak_state_drop_callback);
        assert!(
            !(*actor).state.is_null(),
            "spawn must produce a non-null state for the state-drop signal"
        );
        // Park the actor at a suspend point (non-quiescent). The finalize
        // decision reads only `actor_state`, so this state store is the
        // faithful observable of a parked actor at shutdown.
        (*actor)
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
    }

    // SAFETY: scheduler is stopped (NoWorkerSchedulerForTest installed); no
    // dispatch is in progress.
    unsafe { cleanup_all_actors() };

    // A `Suspended` actor is non-quiescent: cleanup must SKIP (leak) it,
    // never finalize a parked actor.  counter != 0 means it freed one
    // (and leaked the continuation frame).  Reads only the global counter.
    assert_eq!(
        CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
        0,
        "cleanup_all_actors must SKIP (leak) a Suspended actor; a non-zero \
             count means it finalized a parked actor (freeing its box and leaking \
             the continuation frame)"
    );

    // Untracked even though cleanup skipped finalize. Pointer-identity; no deref.
    assert!(
        !live_actors::is_actor_live(actor),
        "actor must be untracked even when cleanup skips finalize"
    );

    // Manual reclaim so the test itself does not leak.  Drop to a terminal
    // state first (no real frame was parked, so no destroy_parked needed).
    // SAFETY: actor is valid, untracked, pin-free, no concurrent access.
    unsafe {
        (*actor)
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        finalize_quiescent_actor_cleanup(actor, HewActorState::Stopped as i32);
    }
    assert_eq!(
        CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
        1,
        "manual reclaim must run state-drop exactly once (no leak)"
    );
}

#[test]
fn ask_by_id_concurrent_with_sends_completes_without_leaking_channels() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);
    ASK_SEND_BY_ID_DISPATCH_COUNT.store(0, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor =
        unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(count_ask_send_by_id_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor is live for the duration of the test.
    let actor_id = unsafe { (*actor).id };
    let ask_threads = 6usize;
    let send_threads = 6usize;
    let asks_per_thread = 12usize;
    let sends_per_thread = 12usize;
    let start = std::sync::Arc::new(std::sync::Barrier::new(ask_threads + send_threads));
    let mut handles = Vec::with_capacity(ask_threads + send_threads);

    for _ in 0..ask_threads {
        let start = start.clone();
        handles.push(std::thread::spawn(move || {
            start.wait();
            for _ in 0..asks_per_thread {
                // SAFETY: actor remains live until all worker threads join.
                let reply = unsafe { hew_actor_ask_by_id(actor_id, 1, ptr::null_mut(), 0) };
                assert!(!reply.is_null(), "by-id ask should receive a reply");
                // SAFETY: successful ask replies are malloc-allocated.
                unsafe {
                    assert_eq!(*reply.cast::<i32>(), 7);
                    crate::mem::buf_free(reply);
                }
            }
        }));
    }

    for _ in 0..send_threads {
        let start = start.clone();
        handles.push(std::thread::spawn(move || {
            start.wait();
            for _ in 0..sends_per_thread {
                // SAFETY: actor remains live until all worker threads join.
                let rc =
                    unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
                assert_eq!(rc, 0);
            }
        }));
    }

    for handle in handles {
        handle.join().expect("mixed ask/send thread must not panic");
    }

    let expected = (ask_threads * asks_per_thread) + (send_threads * sends_per_thread);
    // Scheduler should drain mixed by-id ask/send traffic without deadlocking.
    wait_until(|| ASK_SEND_BY_ID_DISPATCH_COUNT.load(Ordering::Acquire) >= expected);
    assert_eq!(
        ASK_SEND_BY_ID_DISPATCH_COUNT.load(Ordering::Acquire),
        expected
    );
    // Concurrent by-id asks should release all reply channels.
    wait_until(|| reply_channel::active_channel_count() == 0);

    // SAFETY: actor remains live until teardown below.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(hew_actor_free(actor), 0);
    }

    drop(runtime);
    assert_eq!(reply_channel::active_channel_count(), 0);
}

#[test]
fn with_live_actor_by_id_requires_matching_id_and_pointer() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let other = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    assert!(!other.is_null());

    // SAFETY: both actors remain live until teardown below.
    let actor_id = unsafe { (*actor).id };
    // SAFETY: `other` remains live until teardown below.
    let other_id = unsafe { (*other).id };

    assert_eq!(
        with_live_actor_by_id(actor_id, actor, |actor_ref| actor_ref.id),
        Some(actor_id)
    );
    assert_eq!(with_live_actor_by_id(other_id, actor, |_| ()), None);
    assert_eq!(with_live_actor_by_id(actor_id, other, |_| ()), None);

    // SAFETY: both actors are quiescent after close and fully owned by this test.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(hew_actor_free(actor), 0);
        hew_actor_close(other);
        assert_eq!(hew_actor_free(other), 0);
    }
}

#[test]
fn ask_with_channel_send_failure_returns_error() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: Spawning with null state and a valid dispatch function.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor pointer is valid — returned by hew_actor_spawn above.
    unsafe {
        hew_actor_close(actor);
    }

    let ch = reply_channel::hew_reply_channel_new();
    // SAFETY: actor and ch are valid pointers from their respective constructors.
    let rc = unsafe { hew_actor_ask_with_channel(actor, 0, std::ptr::null_mut(), 0, ch) };
    assert_eq!(rc, HewError::ErrActorStopped as i32);

    // SAFETY: ch and actor are valid pointers; freeing resources after test.
    unsafe {
        reply_channel::hew_reply_channel_free(ch);
        assert_eq!(hew_actor_free(actor), 0);
    }
}

#[test]
fn ask_with_channel_send_oom_marks_allocation_failed() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    let ch = reply_channel::hew_reply_channel_new();
    let _alloc_guard = crate::mailbox::fail_mailbox_alloc_on_nth(0);

    // SAFETY: actor and ch are valid pointers from their respective constructors.
    let rc = unsafe { hew_actor_ask_with_channel(actor, 0, std::ptr::null_mut(), 0, ch) };
    assert_eq!(rc, HewError::ErrOom as i32);
    // SAFETY: the failed send preserves the caller-owned ref so tests can
    // inspect the channel before releasing it.
    unsafe {
        assert!(reply_channel::hew_reply_channel_allocation_failed_for_test(
            ch
        ));
        reply_channel::hew_reply_channel_free(ch);
        hew_actor_stop(actor);
        assert_eq!(hew_actor_free(actor), 0);
    }
    assert_eq!(reply_channel::active_channel_count(), 0);
}

#[test]
fn native_ask_self_stop_without_reply_returns_null_and_releases_channel() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);

    // SAFETY: null state and dispatch function are valid for actor spawn.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::null_mut(),
            0,
            Some(native_self_stop_without_reply_dispatch),
        )
    };
    assert!(!actor.is_null());

    let actor_addr = actor as usize;
    let (tx, rx) = std::sync::mpsc::channel();
    let ask_thread = std::thread::spawn(move || {
        let actor = actor_addr as *mut HewActor;
        // SAFETY: actor was spawned by this test and remains live until the thread joins.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        let reply_is_null = reply.is_null();
        if !reply.is_null() {
            // SAFETY: successful ask replies are malloc-allocated.
            unsafe { crate::mem::buf_free(reply) };
        }
        tx.send(reply_is_null)
            .expect("native ask waiter should report its result");
    });

    let reply_is_null = rx
        .recv()
        .expect("native ask waiter should report its result");

    ask_thread
        .join()
        .expect("native ask waiter thread should not panic");

    assert!(
        reply_is_null,
        "ask should resolve as null when the actor self-stops before replying"
    );
    // SAFETY: `actor` remains allocated and owned by this test while we
    // inspect its atomic state.
    let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert!(
        actor_state == HewActorState::Stopping as i32
            || actor_state == HewActorState::Stopped as i32,
        "self-stop ask should leave the actor in teardown, got state {actor_state}"
    );
    // Self-stop ask should eventually drive the actor to Stopped.
    wait_until(|| {
        // SAFETY: `actor` remains allocated and owned by this test while
        // we poll its atomic state.
        unsafe { (*actor).actor_state.load(Ordering::Acquire) == HewActorState::Stopped as i32 }
    });
    // Self-stop ask cleanup should release the native reply channel.
    wait_until(|| reply_channel::active_channel_count() == 0);

    // SAFETY: actor is stopped and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);

    drop(runtime);
    assert_eq!(reply_channel::active_channel_count(), 0);
}

#[test]
fn native_ask_successful_reply_returns_value_without_duplicate_cleanup() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);

    // SAFETY: null state and dispatch function are valid for actor spawn.
    let actor =
        unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor is valid for the duration of the ask.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(!reply.is_null(), "native ask should return the reply value");
    // SAFETY: non-null asks return a malloc-allocated i32 payload here.
    assert_eq!(unsafe { *reply.cast::<i32>() }, 21);
    // SAFETY: successful ask replies are malloc-allocated.
    unsafe { crate::mem::buf_free(reply) };

    // Successful native asks should leave no live reply channels.
    wait_until(|| reply_channel::active_channel_count() == 0);

    // SAFETY: actor is idle and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);

    drop(runtime);
    assert_eq!(reply_channel::active_channel_count(), 0);
}

#[test]
fn native_ask_timeout_rejects_late_reply_after_blocking_dispatch() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);

    // SAFETY: null state and dispatch function are valid for actor spawn.
    let actor =
        unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_late_reply_dispatch)) };
    assert!(!actor.is_null());

    close_late_reply_gate();
    // SAFETY: actor is valid for the duration of the timed ask.
    let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 1) };
    open_late_reply_gate();
    assert!(
        reply.is_null(),
        "timed native asks should reject replies that only arrive after the timeout"
    );
    // Timed-out native asks release late-reply channels after cancellation.
    wait_until(|| reply_channel::active_channel_count() == 0);
    // Late-reply dispatch should finish after the timeout path.
    wait_until(|| {
        // SAFETY: actor remains owned by this test while waiting for dispatch to finish.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        state == HewActorState::Idle as i32 || state == HewActorState::Stopped as i32
    });

    // SAFETY: actor is quiescent and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);

    drop(runtime);
    assert_eq!(reply_channel::active_channel_count(), 0);
}

#[test]
fn native_ask_reply_then_trap_returns_value_without_duplicate_crash_reply() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);

    // SAFETY: null state and dispatch function are valid for actor spawn.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::null_mut(),
            0,
            Some(native_reply_then_trap_dispatch),
        )
    };
    assert!(!actor.is_null());

    // SAFETY: actor is valid for the duration of the ask.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(
        !reply.is_null(),
        "asks should preserve the first reply even if dispatch traps afterwards"
    );
    // SAFETY: non-null asks return a malloc-allocated i32 payload here.
    assert_eq!(unsafe { *reply.cast::<i32>() }, 123);
    // SAFETY: successful ask replies are malloc-allocated.
    unsafe { crate::mem::buf_free(reply) };

    // Reply-then-trap dispatch should still transition the actor to Crashed.
    wait_until(|| {
        // SAFETY: actor remains owned by this test while we poll its state.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        state == HewActorState::Crashed as i32
    });
    // Trap-after-reply asks should not double-complete or leak reply channels.
    wait_until(|| reply_channel::active_channel_count() == 0);

    // SAFETY: actor is quiescent and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);

    drop(runtime);
    assert_eq!(reply_channel::active_channel_count(), 0);
}

unsafe extern "C-unwind" fn native_self_stop_then_trap_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    // The handler self-stops (transitions Running → Stopping) and then
    // panics.  Crash recovery must dominate the pending self-stop:
    // publish `Crashed` (not `Stopped`) and run the full
    // link/monitor/supervisor notification path.  Before the Stage 1
    // ASan-cleanup fix this path went through `handle_crash_recovery`
    // → `hew_actor_trap`'s CAS loop, which accepts any non-terminal
    // current state and writes `Crashed`.  The Crashing-intermediate
    // ordering must preserve the same dominance semantics: the worker
    // CAS-loops both `Running → Crashing` and `Stopping → Crashing` so
    // that a self-stopped-then-crashed actor still publishes `Crashed`
    // and notifies supervisors/links/monitors rather than stalling
    // permanently in `Stopping`.
    hew_actor_self_stop();
    hew_panic();

    std::ptr::null_mut()
}

/// Regression: self-stop followed by a panic in the same dispatch must
/// still publish `Crashed` (crash dominates the pending `Stopping`),
/// run the supervisor/link/monitor notification path, and allow
/// `hew_actor_free` to complete within bounded wait.  Without
/// `Stopping → Crashing` acceptance in the scheduler's crash branch,
/// the actor would be stranded in `Stopping` (non-quiescent), no
/// crash report would publish, and `hew_actor_free` would time out.
#[test]
fn native_self_stop_then_crash_publishes_crashed_and_notifies_supervisor() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::null_mut(),
            0,
            Some(native_self_stop_then_trap_dispatch),
        )
    };
    assert!(!actor.is_null());

    // Deliver a message to trigger the dispatch (no ask — the handler
    // self-stops then crashes; no reply is expected or possible).
    // SAFETY: actor is valid and tracked.
    unsafe { hew_actor_send(actor, 1, ptr::null_mut(), 0) };

    // (a) State reaches `Crashed` and the crash path has published its
    // error code.  Bounded by 2s — the worker runs arena_reset + msg-node
    // free + handle_crash_recovery synchronously and the test fails fast
    // rather than hanging.
    //
    // `hew_actor_trap` publishes the crash code before the terminal CAS,
    // so `Crashed` is never visible ahead of the code.  Waiting on the
    // state alone and then reading the error in a separate step is what
    // holds the runtime to that: if publication ever moved back after the
    // CAS, this read would see the `0` default rather than tolerate it.
    // Self-stop-then-crash must publish Crashed; the actor must not be
    // stranded in Stopping/Crashing.
    wait_until(|| {
        // SAFETY: actor remains owned by this test while we poll its state.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        state == HewActorState::Crashed as i32
    });
    assert_ne!(
        // SAFETY: actor is owned by this test.
        unsafe { hew_actor_get_error(actor) },
        0,
        "an actor observed as Crashed must already report its crash code",
    );

    // (b) `hew_actor_free` completes within its bounded wait
    // (`actor.rs::hew_actor_free_inner` has a 2s timeout on the
    // quiescence spin).  If `Crashing` had stalled the waiter, this
    // would return -2 instead of 0.
    // SAFETY: actor is quiescent and owned by this test.
    let free_rc = unsafe { hew_actor_free(actor) };
    assert_eq!(
        free_rc, 0,
        "hew_actor_free must complete bounded after Crashing → Crashed publication",
    );

    drop(runtime);
    assert_eq!(reply_channel::active_channel_count(), 0);
}

// ── ask error discrimination tests ───────────────────────────────────

/// Mechanism-2 regression (dogfood F1): the with-channel ask family
/// returns a `HewError` code instead of a reply pointer, but its callers
/// classify the failure through `hew_actor_ask_take_last_error`. A failed
/// synchronous submission must therefore record a real `AskError` kind —
/// before the fix the code was returned with the slot unwritten and the
/// failure surfaced as `Err(AskError::NoError)`.
#[test]
fn with_channel_ask_stopped_actor_records_actor_stopped_error() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is valid; stopping it forces the send to fail.
    unsafe { hew_actor_stop(actor) };

    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
    let ch = reply_channel::hew_reply_channel_new();
    // SAFETY: actor pointer remains valid after stop; ch is a live channel.
    let status = unsafe { hew_actor_ask_with_channel(actor, 1, ptr::null_mut(), 0, ch) };
    assert_ne!(
        status,
        HewError::Ok as i32,
        "ask submission against a stopped actor must fail"
    );
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::ActorStopped as i32,
        "a failed with-channel submission must record ActorStopped, never \
             leave the slot at None"
    );
    // Failure keeps the creator reference (KeepCreatorRef); release it.
    // SAFETY: ch was created by hew_reply_channel_new above.
    unsafe { reply_channel::hew_reply_channel_free(ch) };

    // SAFETY: actor is stopped and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

/// `hew_actor_ask` on a stopped actor sets `ActorStopped` in the error slot.
#[test]
fn native_ask_stopped_actor_sets_actor_stopped_error() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is valid.
    unsafe { hew_actor_stop(actor) };

    // Reset error slot, then attempt ask.
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
    // SAFETY: actor is stopped but pointer remains valid.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(reply.is_null(), "ask on stopped actor must return null");
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::ActorStopped as i32,
        "stopped actor must report ActorStopped error"
    );

    // SAFETY: actor is stopped and owned by this test.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

#[test]
fn native_ask_send_oom_releases_reply_channel() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    assert_eq!(reply_channel::active_channel_count(), 0);
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
    let _alloc_guard = crate::mailbox::fail_mailbox_alloc_on_nth(0);
    // SAFETY: actor is live and the forced mailbox allocation failure makes
    // the ask fail before any reply can be queued.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(reply.is_null(), "OOM ask send must return null");
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::ActorStopped as i32,
        "send-side OOM is reported through the ActorStopped ask bucket"
    );
    assert_eq!(
        reply_channel::active_channel_count(),
        0,
        "failed ask send must release both reply-channel references"
    );

    // SAFETY: the ask never enqueued work, so stopping/freely cleaning the
    // actor is valid once the reply-channel invariant above holds.
    unsafe {
        hew_actor_stop(actor);
        assert_eq!(hew_actor_free(actor), 0);
    }
    drop(runtime);
}

/// `hew_actor_ask_timeout` on a stopped actor sets `ActorStopped`.
#[test]
fn native_ask_timeout_stopped_actor_sets_actor_stopped_error() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor is live and single-owner; stopping it to force send failure.
    unsafe { hew_actor_stop(actor) };

    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
    // SAFETY: actor is stopped.
    let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 50) };
    assert!(reply.is_null());
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::ActorStopped as i32,
        "send failure on stopped actor must report ActorStopped"
    );
    // SAFETY: actor was stopped above; no asks are pending.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

/// `hew_actor_ask_timeout` fires `Timeout` when the handler does not reply in time.
#[test]
fn native_ask_timeout_sets_timeout_error() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    // SAFETY: null state + valid dispatch.
    let actor =
        unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_late_reply_dispatch)) };
    assert!(!actor.is_null());

    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
    close_late_reply_gate();
    // SAFETY: actor is valid; the handler cannot reply until the gate opens.
    let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 1) };
    open_late_reply_gate();
    assert!(reply.is_null(), "ask must time out");
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::Timeout as i32,
        "timed-out ask must report Timeout"
    );

    // Let the late-reply dispatch finish and free the actor cleanly: the
    // late-reply channel is released after cancellation.
    wait_until(|| reply_channel::active_channel_count() == 0);
    // SAFETY: actor was spawned above and all channels are drained.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    drop(runtime);
}

/// `hew_actor_ask` when the actor self-stops without replying sets `OrphanedAsk`.
#[test]
fn native_ask_orphaned_sets_orphaned_ask_error() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    // SAFETY: null state + valid dispatch.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::null_mut(),
            0,
            Some(native_self_stop_without_reply_dispatch),
        )
    };
    assert!(!actor.is_null());

    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));

    let actor_addr = actor as usize;
    let (tx, rx) = std::sync::mpsc::channel();
    let handle = std::thread::spawn(move || {
        let actor = actor_addr as *mut HewActor;
        // SAFETY: actor was spawned above and remains live until the thread joins.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        let is_null = reply.is_null();
        if !reply.is_null() {
            // SAFETY: reply was allocated by the runtime and ownership transfers to caller.
            unsafe { crate::mem::buf_free(reply) };
        }
        let err = hew_actor_ask_take_last_error();
        tx.send((is_null, err)).expect("sender should be live");
    });

    let (is_null, err) = rx.recv().expect("ask thread should report its result");
    handle.join().expect("ask thread must not panic");

    assert!(is_null, "orphaned ask must return null");
    assert_eq!(
        err,
        AskError::OrphanedAsk as i32,
        "orphaned ask must report OrphanedAsk"
    );

    // Orphaned ask must release its reply channel.
    wait_until(|| reply_channel::active_channel_count() == 0);
    // SAFETY: actor has self-stopped; all channels are released.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    drop(runtime);
}

/// Successful ask clears the error slot.
#[test]
fn native_ask_success_clears_error_slot() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    // SAFETY: null state + valid dispatch.
    let actor =
        unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
    assert!(!actor.is_null());

    // Poison slot, then succeed.
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
    // SAFETY: actor is valid.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(!reply.is_null(), "ask must succeed");
    // SAFETY: non-null reply is malloc-allocated.
    unsafe { crate::mem::buf_free(reply) };
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::None as i32,
        "successful ask must clear the error slot"
    );

    // SAFETY: actor is live; ask has returned and no pending channels remain.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    drop(runtime);
}

/// `hew_actor_ask_take_last_error` resets the slot to None after reading.
#[test]
fn actor_ask_take_last_error_resets_slot() {
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
    let first = hew_actor_ask_take_last_error();
    let second = hew_actor_ask_take_last_error();
    assert_eq!(
        first,
        AskError::Timeout as i32,
        "first take must return Timeout"
    );
    assert_eq!(
        second,
        AskError::None as i32,
        "second take must return None"
    );
}

// ── MailboxFull / NoRunnableWork discrimination (native) ─────────────

/// `hew_actor_ask` on a bounded mailbox that is at capacity returns `MailboxFull`.
///
/// The send inside the ask sees a full mailbox (capacity = 1, one pre-queued
/// message) and returns `ErrMailboxFull` before the ask-wait loop is entered.
///
/// The pre-fill is done by calling `hew_mailbox_send` directly on the mailbox
/// pointer.  This bypasses `actor_send_result_internal_reply` (and therefore
/// `sched_enqueue`) intentionally: we want the message to sit in the mailbox
/// without the actor being scheduled, so the slot is still occupied when the
/// ask executes.  The actor remains in the `Idle` state throughout, which lets
/// `hew_actor_stop` CAS it directly to `Stopped` for clean teardown — no
/// scheduler is required.
#[test]
fn native_ask_bounded_mailbox_full_sets_mailbox_full_error() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn_bounded(ptr::null_mut(), 0, Some(noop_dispatch), 1) };
    assert!(!actor.is_null());

    // Directly enqueue one message into the mailbox, bypassing the actor-state
    // transition and scheduler enqueue.  The actor stays Idle; the mailbox now
    // has count=1 == capacity=1.
    // SAFETY: actor is valid; mailbox pointer is valid for the actor's lifetime.
    let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };
    // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
    // The null data pointer is intentional — the message slot just needs to exist.
    let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
    assert_eq!(
        pre_fill,
        HewError::Ok as i32,
        "pre-fill into empty bounded mailbox must succeed"
    );

    // Reset the error slot, then ask. The send inside the ask hits the full
    // mailbox and returns ErrMailboxFull immediately — the ask-wait loop is
    // never entered.
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
    // SAFETY: actor is valid; the ask send will fail with MailboxFull.
    let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
    assert!(
        reply.is_null(),
        "ask into full bounded mailbox must return null"
    );
    assert_eq!(
        hew_actor_ask_take_last_error(),
        AskError::MailboxFull as i32,
        "full bounded mailbox must report MailboxFull"
    );

    // Actor is still Idle (no state transition occurred during pre-fill).
    // hew_actor_stop CAS Idle → Stopped succeeds; no scheduler needed.
    // SAFETY: actor is valid; closing a live actor's mailbox is safe.
    unsafe { hew_actor_stop(actor) };
    // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
    // pre-filled message during free_actor_resources.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

/// Bounded-mailbox actor that self-stops without replying sets `OrphanedAsk`,
/// not `MailboxFull`: the mailbox has room for the ask message, so the
/// discriminant is the orphaned reply channel, not a send failure.
#[test]
fn native_ask_bounded_actor_orphan_sets_orphaned_ask_error() {
    let _guard = crate::runtime_test_guard();
    let runtime = NativeSchedulerGuard::new();

    // capacity=8: plenty of room for the ask message, so the send succeeds
    // and the discriminant is the orphaned reply channel.
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe {
        hew_actor_spawn_bounded(
            ptr::null_mut(),
            0,
            Some(native_self_stop_without_reply_dispatch),
            8,
        )
    };
    assert!(!actor.is_null());

    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));

    let actor_addr = actor as usize;
    let (tx, rx) = std::sync::mpsc::channel();
    let handle = std::thread::spawn(move || {
        let actor = actor_addr as *mut HewActor;
        // SAFETY: actor was spawned above and remains live until the thread joins.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        let is_null = reply.is_null();
        if !reply.is_null() {
            // SAFETY: reply was allocated by the runtime and ownership transfers to caller.
            unsafe { crate::mem::buf_free(reply) };
        }
        let err = hew_actor_ask_take_last_error();
        tx.send((is_null, err)).expect("sender should be live");
    });

    let (is_null, err) = rx.recv().expect("ask thread should report its result");
    handle.join().expect("ask thread must not panic");

    assert!(is_null, "bounded-actor orphaned ask must return null");
    assert_eq!(
        err,
        AskError::OrphanedAsk as i32,
        "bounded-actor orphaned ask must report OrphanedAsk, not MailboxFull"
    );

    // Orphaned ask on bounded actor must release its reply channel.
    wait_until(|| reply_channel::active_channel_count() == 0);
    // SAFETY: actor has self-stopped; all channels are released.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    drop(runtime);
}

#[test]
fn stop_idle_actor_is_idempotent_and_requests_no_shutdown() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: Spawning with null state and a valid dispatch function.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor/mailbox pointers are valid for the duration of the test.
    unsafe {
        let mb = (*actor).mailbox.cast::<HewMailbox>();
        assert_eq!(mailbox::hew_mailbox_sys_len(mb), 0);

        hew_actor_stop(actor);
        assert_eq!(
            (*actor).actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32
        );
        assert!(
            !mailbox::mailbox_stop_requested(mb),
            "an idle actor stops synchronously; there is no dispatch loop \
                 left to observe a deferred stop request"
        );
        assert_eq!(
            mailbox::hew_mailbox_sys_len(mb),
            0,
            "stopping an idle actor must put nothing on the system queue"
        );

        hew_actor_stop(actor);
        hew_actor_stop(actor);
        assert_eq!(
            mailbox::hew_mailbox_sys_len(mb),
            0,
            "repeated stop calls on a stopped actor must not accumulate system messages"
        );

        assert_eq!(hew_actor_free(actor), 0);
    }
}

#[test]
fn stop_runnable_actor_does_not_request_shutdown() {
    let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);

    // SAFETY: actor/mailbox pointers are valid for the duration of the test.
    unsafe {
        hew_actor_stop(actor);
        hew_actor_stop(actor);
        assert!(
            mailbox::mailbox_is_closed(mailbox),
            "stop must close runnable actors before they drain their queued activation"
        );
        assert!(
            !mailbox::mailbox_stop_requested(mailbox),
            "runnable actors already have a queued activation that drains to \
                 Stopped on the closed mailbox; latching the stop flag would make \
                 them abandon that queued work instead"
        );
        assert_eq!(
            mailbox::hew_mailbox_sys_len(mailbox),
            0,
            "the stop is out of band — nothing is ever enqueued"
        );
        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

#[test]
fn logical_fault_allocation_does_not_publish_an_actor_crash() {
    let (actor, mailbox) = make_stop_test_actor_with_id(91_003, HewActorState::Running);
    let mut context = crate::execution_context::HewExecutionContext {
        actor,
        actor_id: 91_003,
        ..crate::execution_context::HewExecutionContext::default()
    };
    let previous = crate::execution_context::set_current_context(&raw mut context);
    let fault = crate::fault::hew_fault_new(crate::internal::types::HEW_TRAP_USER_PANIC);
    assert_eq!(crate::fault::crashing_owner(), None);
    // SAFETY: the fault and both fixture allocations remain uniquely owned.
    unsafe {
        let message = crate::fault::hew_fault_take_message(fault);
        hew_cabi::string::string_release(message);
        assert_eq!(crate::fault::crashing_owner(), None);
        crate::fault::note_actor_crash(91_003);
        assert_eq!(crate::fault::crashing_owner(), Some(91_003));
        crate::fault::release_actor_state(91_003, false, || {});
        let _ = crate::execution_context::set_current_context(previous);
        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

#[test]
fn stop_queued_continuation_latches_cancellation() {
    let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);
    // SAFETY: this fixture owns both allocations. The parked pointer is
    // only a presence marker; no scheduler runs or dereferences it.
    unsafe {
        (*actor)
            .suspended_cont
            .store(ptr::dangling_mut(), Ordering::Release);
        (*actor).cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        hew_actor_stop(actor);
        assert!(mailbox::mailbox_stop_requested(mailbox));
        assert_eq!(
            (*actor).actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32
        );
        assert_eq!(mailbox::hew_mailbox_sys_len(mailbox), 0);
        (*actor)
            .suspended_cont
            .store(ptr::null_mut(), Ordering::Release);
        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

/// Ids for the native-submission tests, kept apart from the fixed ids the
/// rest of the suite uses so a registered route is never shared.
static NATIVE_SUBMIT_NEXT_ID: AtomicU64 = AtomicU64::new(0x00A1_1A5E_0000_0000);

/// Publish a test actor as a routable destination so the native submission
/// path can resolve a token to it. Requires an installed runtime, so the
/// caller holds `runtime_test_guard()`.
fn track_native_submit_actor(
    initial_state: HewActorState,
) -> (
    *mut HewActor,
    *mut HewMailbox,
    crate::lifetime::local_handles::HewLocalPidId,
) {
    let id = NATIVE_SUBMIT_NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let (actor, mailbox) = make_stop_test_actor_with_id(id, initial_state);
    let runtime =
        crate::runtime::rt_current_opt().expect("the runtime test guard installs a runtime");
    let token = runtime
        .local_handles
        .register_actor(crate::runtime_id::RuntimeId::DEFAULT, id)
        .expect("a fresh id registers one direct route");
    // SAFETY: the helper returned a fully initialized actor this test owns;
    // tracking publishes it without taking an allocation reference. The
    // owner stamp is what `untrack_actor` retires the route through.
    unsafe {
        (*actor).runtime = std::ptr::from_ref(runtime);
        (*actor).local_pid_id = token;
        assert!(live_actors::track_actor(actor));
    }
    (actor, mailbox, token)
}

/// Untracking retires the direct route through the actor's owner stamp, so
/// this is the whole teardown of `track_native_submit_actor`.
fn retire_native_submit_actor(actor: *mut HewActor) {
    assert!(live_actors::untrack_actor(actor));
}

/// An admitted native submission delivers its payload by reference: the
/// queued node aliases the sender's envelope rather than copying its
/// buffer, and the node free is the one release of that refcount. The
/// actor starts `Running` so the wake CAS is a no-op and no scheduler is
/// needed.
/// An injected drop fault loses a native envelope submission exactly as it
/// loses a copy-mode one: nothing is enqueued, the message is released once
/// rather than leaked, and the caller is told it was lost rather than
/// delivered.
#[test]
fn an_injected_drop_fault_loses_a_native_submission_and_says_so() {
    static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    let _rt = crate::runtime_test_guard();
    DROP_COUNT.store(0, Ordering::SeqCst);
    crate::deterministic::hew_deterministic_reset();

    let (actor, mailbox, token) = track_native_submit_actor(HewActorState::Running);
    // SAFETY: actor/mailbox are valid for the test; each envelope carries
    // one refcount that transfers on admission.
    unsafe {
        let fresh = || {
            let size = 4usize;
            let payload = crate::mem::buf_try_alloc(size);
            assert!(!payload.is_null());
            libc::memcpy(payload, b"lost".as_ptr().cast(), size);
            crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue))
        };

        crate::deterministic::hew_fault_inject_drop((*actor).id, 1);

        assert!(
            matches!(
                try_submit_native_envelope(token, 4, fresh()),
                mailbox::SendOutcome::Dropped
            ),
            "a dropped submission is a loss, not an acceptance"
        );
        assert_eq!(
            mailbox::hew_mailbox_has_messages(mailbox),
            0,
            "a dropped submission never reaches the queue"
        );
        assert_eq!(
            DROP_COUNT.load(Ordering::SeqCst),
            1,
            "the lost message is released exactly once"
        );

        // The injection was for one message; the next submission lands.
        assert!(matches!(
            try_submit_native_envelope(token, 4, fresh()),
            mailbox::SendOutcome::Enqueued
        ));
        assert_eq!(mailbox::hew_mailbox_has_messages(mailbox), 1);
        let node = mailbox::hew_mailbox_try_recv(mailbox);
        assert!(!node.is_null());
        mailbox::hew_msg_node_free(node);
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 2);
    }

    crate::deterministic::hew_deterministic_reset();
    retire_native_submit_actor(actor);
}

#[test]
fn native_submission_delivers_by_reference_and_releases_once() {
    static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    let _rt = crate::runtime_test_guard();
    DROP_COUNT.store(0, Ordering::SeqCst);

    let (actor, mailbox, token) = track_native_submit_actor(HewActorState::Running);
    // SAFETY: actor/mailbox are valid for the test; the envelope carries
    // one refcount that transfers on admission.
    unsafe {
        let size = 5usize;
        let payload = crate::mem::buf_try_alloc(size);
        assert!(!payload.is_null());
        libc::memcpy(payload, b"alive".as_ptr().cast(), size);
        let env = crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
        assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

        assert!(matches!(
            try_submit_native_envelope(token, 4, env),
            mailbox::SendOutcome::Enqueued
        ));
        // Enqueued, not yet consumed.
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0);
        assert_eq!(mailbox::hew_mailbox_has_messages(mailbox), 1);

        // Drain (models dispatch); node free releases the envelope once.
        let node = mailbox::hew_mailbox_try_recv(mailbox);
        assert!(!node.is_null());
        assert_eq!((*node).msg_type, 4);
        let borrowed = crate::mailbox::hew_msg_envelope_payload_ptr((*node).envelope);
        assert_eq!(
            borrowed, payload,
            "payload delivered by reference, not copied"
        );
        mailbox::hew_msg_node_free(node);
        assert_eq!(
            DROP_COUNT.load(Ordering::SeqCst),
            1,
            "an admitted submission must release the envelope exactly once"
        );

        retire_native_submit_actor(actor);
        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

/// Terminal-state send gate, copy-mode paths. `hew_actor_trap` takes its
/// terminal CAS BEFORE closing the mailbox, leaving a window in which the
/// actor is terminal but the mailbox is still open. A send racing that
/// window must be rejected by the actor-level terminal gate so it never
/// enqueues into, or reports false success against, a terminal actor —
/// closing the window the lost-crash-notify reorder opened.
///
/// Drives the test actor directly into each terminal state (the trap's CAS
/// post-state, mailbox deliberately left OPEN to model the window) and
/// asserts every copy-mode held-pointer send path rejects with
/// `ErrActorStopped` and enqueues nothing.
#[test]
fn terminal_actor_copy_send_paths_reject_without_enqueue() {
    let _guard = crate::runtime_test_guard();

    for terminal in [HewActorState::Crashed, HewActorState::Stopped] {
        // `Running` initial state so a (hypothetically) accepted send would
        // not also try to push onto a scheduler queue; the mailbox is left
        // OPEN to model the trap's terminal-CAS-before-close window.
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);
        // SAFETY: the test exclusively owns `actor`; publish the terminal
        // state with a release store, exactly as the trap's CAS does.
        unsafe {
            (*actor)
                .actor_state
                .store(terminal as i32, Ordering::Release);
            assert!(
                !mailbox::mailbox_is_closed(mailbox),
                "mailbox must be OPEN to model the terminal-CAS-before-close window"
            );
        }

        // try_send rejects without enqueue. The non-blocking path returns
        // `ErrClosed` (mirroring `hew_mailbox_try_send`'s closed-mailbox
        // code), distinct from the blocking paths' `ErrActorStopped`.
        // SAFETY: `actor` valid and owned; null payload.
        let try_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            try_rc,
            HewError::ErrClosed as i32,
            "try_send into a {terminal:?} actor must be rejected by the terminal gate"
        );

        // The fire-and-forget result path (used by `hew_actor_send`).
        // SAFETY: as above.
        let send_rc = unsafe { actor_send_result_internal(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            send_rc,
            HewError::ErrActorStopped as i32,
            "send into a {terminal:?} actor must be rejected by the terminal gate"
        );

        // No path enqueued: nothing reached the (still-open) mailbox.
        // SAFETY: `mailbox` is valid and owned by this test.
        let has_messages = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
        assert_eq!(
            has_messages, 0,
            "a send rejected by the terminal gate must not enqueue into a {terminal:?} actor"
        );

        // SAFETY: the test fully owns the actor and its mailbox.
        unsafe {
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }
}

/// Terminal-state send gate, native submission path. The gate rejects
/// before the mailbox sees the message, so nothing undeliverable is
/// enqueued and the sender keeps its one envelope refcount to release or
/// readdress. Both terminal states, mailbox left OPEN to model the trap's
/// terminal-CAS-before-close window.
#[test]
fn terminal_actor_native_submission_refuses_without_enqueue() {
    static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    let _rt = crate::runtime_test_guard();

    for terminal in [HewActorState::Crashed, HewActorState::Stopped] {
        DROP_COUNT.store(0, Ordering::SeqCst);

        let (actor, mailbox, token) = track_native_submit_actor(HewActorState::Running);
        // SAFETY: the test exclusively owns `actor`; publish terminal state
        // with a release store as the trap's CAS does, mailbox left OPEN.
        unsafe {
            (*actor)
                .actor_state
                .store(terminal as i32, Ordering::Release);
            assert!(
                !mailbox::mailbox_is_closed(mailbox),
                "mailbox must be OPEN to model the terminal-CAS-before-close window"
            );

            let size = 5usize;
            let payload = crate::mem::buf_try_alloc(size);
            assert!(!payload.is_null());
            libc::memcpy(payload, b"alias".as_ptr().cast(), size);
            let env = crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

            assert!(
                matches!(
                    try_submit_native_envelope(token, 4, env),
                    mailbox::SendOutcome::Closed
                ),
                "the terminal gate must refuse a {terminal:?} destination"
            );

            // Refused: nothing enqueued, and the message is still the
            // sender's to release.
            assert_eq!(
                mailbox::hew_mailbox_has_messages(mailbox),
                0,
                "a refusal must not enqueue an undeliverable node ({terminal:?})"
            );
            assert_eq!(
                DROP_COUNT.load(Ordering::SeqCst),
                0,
                "a refusal preserves the message for its sender ({terminal:?})"
            );
            crate::mailbox::hew_msg_envelope_release(env);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1);

            retire_native_submit_actor(actor);
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }
}

/// Concurrent racing-sender window. A real native submission is issued
/// from a second thread CONCURRENTLY with `hew_actor_trap`'s terminal
/// transition, looped so it lands across the whole pre-CAS / in-window /
/// post-close spectrum. The invariants the gate must hold on EVERY
/// interleaving: (1) the envelope refcount is released exactly once per
/// submission — admitted, the drain releases it; refused, the sender does
/// (no leak, no double-free); (2) the crash notify is never lost — the
/// actor reaches the `Crashed` terminal state. Run under ASan/LSan this
/// also proves the refused message's buffer is freed exactly once.
#[test]
fn racing_native_submission_during_trap_releases_once_and_crash_notifies() {
    const ITERS: usize = 2_000;

    static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    // `hew_actor_trap` walks the runtime-owned monitor table. Participate
    // in the shared runtime-test serialization contract so another test
    // cannot tear down the installed `RuntimeInner` during that walk.
    let _runtime_guard = crate::runtime_test_guard();
    assert!(
        crate::scheduler::SchedTestLock::is_held(),
        "trap/monitor tests must hold the shared runtime test lock"
    );

    for _ in 0..ITERS {
        DROP_COUNT.store(0, Ordering::SeqCst);

        // `Running` so the sender's success path would not also enqueue on a
        // scheduler; the trap drives this actor terminal under the sender.
        let (actor, mailbox, token) = track_native_submit_actor(HewActorState::Running);

        let start = std::sync::Arc::new(std::sync::Barrier::new(2));
        let sender_start = start.clone();

        let sender = std::thread::spawn(move || {
            // SAFETY: the envelope carries one refcount that transfers only
            // on admission; a refusal leaves it with this thread.
            unsafe {
                let size = 5usize;
                let payload = crate::mem::buf_try_alloc(size);
                assert!(!payload.is_null());
                libc::memcpy(payload, b"alias".as_ptr().cast(), size);
                let env =
                    crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
                sender_start.wait();
                match try_submit_native_envelope(token, 4, env) {
                    mailbox::SendOutcome::Enqueued => {}
                    mailbox::SendOutcome::Closed => {
                        crate::mailbox::hew_msg_envelope_release(env);
                    }
                    _ => panic!("an unbounded destination either admits or reports closed"),
                }
            }
        });

        start.wait();
        // SAFETY: `actor` is valid; the trap drives it to the Crashed
        // terminal state concurrently with the racing sender.
        unsafe { hew_actor_trap(actor, 1) };

        sender.join().expect("sender thread must not panic");

        // SAFETY: both threads have joined; the actor and mailbox are now
        // exclusively owned by this thread.
        unsafe {
            // Crash notify never lost: the trap published the terminal state.
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Crashed as i32,
                "trap must win the terminal race and publish Crashed"
            );

            // Drain whatever the send enqueued before the gate/close (the
            // inherent "sent the instant before the crash" node, if any) so
            // its envelope release is accounted before the exactly-once check.
            let mut drained = 0usize;
            loop {
                let node = mailbox::hew_mailbox_try_recv(mailbox);
                if node.is_null() {
                    break;
                }
                mailbox::hew_msg_node_free(node);
                drained += 1;
            }
            assert!(
                drained <= 1,
                "at most one node can land in the pre-close window, drained {drained}"
            );

            retire_native_submit_actor(actor);
            mailbox::hew_mailbox_free(mailbox);

            // Exactly-once release of the single submission's envelope
            // across EVERY interleaving: refused-by-gate, refused-by-close,
            // or admitted-then-drained — each releases the refcount once.
            assert_eq!(
                DROP_COUNT.load(Ordering::SeqCst),
                1,
                "the racing submission's envelope must be released exactly once"
            );

            drop(Box::from_raw(actor));
        }
    }
}

/// PROBE (P5-RX Stage 2a, A625): models the codegen contract for an
/// escaping borrowed `String` view under both runtime receipt modes, and
/// asserts exactly-once release in each. This test was first reinstated in
/// its PRE-FIX shape — a naked handler drop of the borrowed handle followed
/// by the envelope release — which `ASan` flagged as a heap-use-after-free /
/// double-free (the borrowed buffer is owned by the envelope, so the
/// handler must NOT free it). The retain-on-escape mechanism flips it green:
///
///   - BORROW arm (`borrow_mode != 0`): at the owned sink the handler takes
///     its OWN retained owner via `hew_string_clone` (a refcount bump on the
///     shared buffer). The handler's owned-drop then releases that clone,
///     and `hew_msg_envelope_release` releases the envelope's original — two
///     decrements against a refcount that the clone raised to two, so the
///     backing buffer is freed exactly once.
///   - COPY arm (`borrow_mode == 0`): ownership of the payload transferred
///     to the handler outright; codegen emits a plain move (no clone), the
///     handler frees its private owner once, and nothing else aliases it.
///
/// Wrapped in a 20× loop so a residual double-free or leak is overwhelmingly
/// likely to trip `ASan` / the per-iteration single-release assertion.
#[test]
fn live_borrow_receive_retains_escaping_payload_releases_once() {
    static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn drop_string_payload(payload: *mut c_void) {
        // SAFETY: the envelope stores a `*mut c_char` string handle in the
        // first pointer-sized slot of `payload` (set by the test below);
        // load it and release one owner.
        let handle = unsafe { *payload.cast::<*mut hew_cabi::string::HewString>() };
        // SAFETY: `handle` is a live header-aware String produced by
        // `hew_string_from_char` (or a clone of it), released exactly once.
        unsafe { crate::string::hew_string_drop(handle) };
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    for _ in 0..20 {
        // ---- BORROW arm: borrow_mode != 0, retain-on-escape ----
        DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: a self-contained envelope lifecycle — allocate a one-slot
        // payload holding a fresh String handle, wrap it, model the handler
        // retain/drop, then release the envelope. Every pointer is live for
        // the block and freed exactly once.
        unsafe {
            let s = crate::string::hew_string_from_char(i32::from(b'x'));
            let slot = std::mem::size_of::<*mut hew_cabi::string::HewString>();
            let buf = crate::mem::buf_try_alloc(slot).cast::<*mut hew_cabi::string::HewString>();
            assert!(!buf.is_null());
            *buf = s;
            let env =
                crate::mailbox::hew_msg_envelope_new(buf.cast(), slot, Some(drop_string_payload));

            // Handler escapes the borrowed view into an owned sink. The
            // gated retain hands it a private owner (refcount bump).
            let borrowed = crate::mailbox::hew_msg_envelope_payload_ptr(env);
            let received_handle = *borrowed.cast::<*mut hew_cabi::string::HewString>();
            let retained = crate::string::hew_string_clone(received_handle);

            // Sink's owned-drop releases the handler's clone (1st decrement).
            crate::string::hew_string_drop(retained);
            // Envelope releases its original (2nd decrement -> frees once).
            crate::mailbox::hew_msg_envelope_release(env);

            assert_eq!(
                DROP_COUNT.load(Ordering::SeqCst),
                1,
                "borrow-mode escape must release the shared buffer exactly once"
            );
        }

        // ---- COPY arm: borrow_mode == 0, plain move, sole owner ----
        DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: same self-contained envelope lifecycle as the borrow arm;
        // copy mode emits no clone, so the envelope release is the sole free.
        unsafe {
            let s = crate::string::hew_string_from_char(i32::from(b'y'));
            let slot = std::mem::size_of::<*mut hew_cabi::string::HewString>();
            let buf = crate::mem::buf_try_alloc(slot).cast::<*mut hew_cabi::string::HewString>();
            assert!(!buf.is_null());
            *buf = s;
            let env =
                crate::mailbox::hew_msg_envelope_new(buf.cast(), slot, Some(drop_string_payload));

            // No clone in copy mode: the handler owns the payload outright;
            // the envelope release is its sole, single free.
            crate::mailbox::hew_msg_envelope_release(env);

            assert_eq!(
                DROP_COUNT.load(Ordering::SeqCst),
                1,
                "copy-mode receipt must free its owner exactly once"
            );
        }
    }
}

#[test]
fn close_then_stop_runnable_actor_requests_no_shutdown() {
    let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);

    // SAFETY: actor/mailbox pointers are valid for the duration of the test.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(
            (*actor).actor_state.load(Ordering::Acquire),
            HewActorState::Runnable as i32,
            "close should leave runnable actors runnable while only closing the mailbox"
        );
        assert!(
            mailbox::mailbox_is_closed(mailbox),
            "close must mark the mailbox closed before stop is requested"
        );

        hew_actor_stop(actor);
        assert!(
            !mailbox::mailbox_stop_requested(mailbox),
            "stop after close must not latch a stop request for runnable actors"
        );

        hew_actor_stop(actor);
        assert!(
            !mailbox::mailbox_stop_requested(mailbox),
            "repeated stop after close must leave runnable actors unlatched"
        );
        assert_eq!(
            mailbox::hew_mailbox_sys_len(mailbox),
            0,
            "the stop is out of band — nothing is ever enqueued"
        );

        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

#[test]
fn stop_running_actor_latches_the_stop_flag_without_enqueueing() {
    let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);

    // SAFETY: actor/mailbox pointers are valid for the duration of the test.
    unsafe {
        hew_actor_stop(actor);
        assert!(
            mailbox::mailbox_stop_requested(mailbox),
            "stopping a Running actor must latch the out-of-band stop flag"
        );
        hew_actor_stop(actor);
        assert!(
            mailbox::mailbox_stop_requested(mailbox),
            "the latch is idempotent — a repeated stop leaves it set"
        );
        assert_eq!(
            mailbox::hew_mailbox_sys_len(mailbox),
            0,
            "the stop must consume no queue slot and allocate no node"
        );
        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

#[test]
fn close_then_stop_running_actor_latches_the_stop_flag() {
    let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);

    // SAFETY: actor/mailbox pointers are valid for the duration of the test.
    unsafe {
        hew_actor_close(actor);
        assert_eq!(
            (*actor).actor_state.load(Ordering::Acquire),
            HewActorState::Running as i32,
            "close should leave running actors running while only closing the mailbox"
        );
        assert!(
            mailbox::mailbox_is_closed(mailbox),
            "close must mark the mailbox closed before stop is requested"
        );

        hew_actor_stop(actor);
        assert!(
            mailbox::mailbox_stop_requested(mailbox),
            "stop after close must still latch the stop for a running actor"
        );

        hew_actor_stop(actor);
        assert!(
            mailbox::mailbox_stop_requested(mailbox),
            "repeated stop after close is idempotent"
        );
        assert_eq!(
            mailbox::hew_mailbox_sys_len(mailbox),
            0,
            "the stop must consume no queue slot and allocate no node"
        );

        mailbox::hew_mailbox_free(mailbox);
        drop(Box::from_raw(actor));
    }
}

#[test]
fn free_actor_resources_completes_when_terminate_finishes_quickly() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: null state, valid dispatch.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor pointer is valid — returned by hew_actor_spawn.
    unsafe {
        hew_actor_close(actor);
    }

    TERMINATE_WAIT_POLL_TICKS.store(0, Ordering::Release);
    // SAFETY: actor is valid, closed, and in a terminal-safe state.
    let rc = unsafe { hew_actor_free(actor) };

    assert_eq!(rc, 0);
    assert_eq!(
        TERMINATE_WAIT_POLL_TICKS.load(Ordering::Acquire),
        0,
        "free must not wait on a terminate that already finished"
    );
}

#[test]
fn terminate_long_does_not_spin() {
    // The finisher releases terminate only after free has polled this
    // many times, so free demonstrably waits.
    const WAIT_TICKS: usize = 20;
    let _guard = crate::runtime_test_guard();
    // SAFETY: null state, valid dispatch.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor is valid for the duration of the wait below.
    let a = unsafe { &*actor };
    a.terminate_called.store(true, Ordering::Release);
    a.terminate_finished.store(false, Ordering::Release);
    a.actor_state
        .store(HewActorState::Stopped as i32, Ordering::Release);

    // Each poll sleeps at least one interval, so a sleeping wait records
    // at most one tick per interval elapsed; a busy spin records far more.
    // Neither bound depends on how promptly the host schedules a thread.
    TERMINATE_WAIT_POLL_TICKS.store(0, Ordering::Release);
    let actor_addr = actor as usize;
    let finisher = std::thread::spawn(move || {
        while TERMINATE_WAIT_POLL_TICKS.load(Ordering::Acquire) < WAIT_TICKS {
            std::thread::yield_now();
        }
        // SAFETY: free waits for this store before reclaiming the actor.
        unsafe {
            (*(actor_addr as *mut HewActor))
                .terminate_finished
                .store(true, Ordering::Release);
        }
    });

    let start = std::time::Instant::now();
    // SAFETY: actor is valid and waits for terminate_finished before free.
    let rc = unsafe { hew_actor_free(actor) };
    let elapsed = start.elapsed();
    finisher.join().unwrap();

    assert_eq!(rc, 0);
    let ticks = TERMINATE_WAIT_POLL_TICKS.load(Ordering::Acquire);
    assert!(
        ticks >= WAIT_TICKS,
        "free must wait for the long terminate path, polled {ticks} times"
    );
    let intervals = elapsed.as_nanos() / TERMINATE_WAIT_POLL_INTERVAL.as_nanos();
    assert!(
        ticks as u128 <= intervals + 1,
        "terminate wait must sleep between polls instead of busy-spinning: \
             {ticks} polls in {elapsed:?}"
    );
}

#[test]
fn free_current_actor_from_dispatch_is_deferred() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: this test fully owns the spawned actor and only mutates its
    // fields while no other runtime threads can access it.
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());
        (*actor)
            .actor_state
            .store(HewActorState::Stopping as i32, Ordering::Release);

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor,
            actor_id: (*actor).id,
            ..HewExecutionContext::default()
        });
        let rc = hew_actor_free(actor);
        // Logical proof that the current-thread free DEFERRED instead of
        // tearing the actor down synchronously: the actor must still be live
        // the instant free returns. The real teardown runs on a background
        // thread that waits for the actor to reach a terminal state, which
        // this test publishes only after reading liveness.
        let live_immediately_after = is_actor_live(actor);
        (*actor)
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        assert_eq!(
            rc, 0,
            "current-thread frees should defer instead of timing out"
        );
        assert!(
            live_immediately_after,
            "current-thread free should defer: the actor must still be live the instant free returns, with teardown deferred to a background thread"
        );
        // The actor is freed asynchronously after dispatch unwinds.
        wait_until(|| !is_actor_live(actor));
    }
}

#[test]
fn cleanup_all_actors_waits_for_deferred_free_threads() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    // SAFETY: this test owns the actor and coordinates all concurrent access.
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());

        (*actor)
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        (*actor).terminate_called.store(true, Ordering::Release);
        (*actor).terminate_finished.store(false, Ordering::Release);

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor,
            actor_id: (*actor).id,
            ..HewExecutionContext::default()
        });
        assert_eq!(hew_actor_free(actor), 0, "self-free should defer");

        let cleanup_started = std::sync::Arc::new(std::sync::Barrier::new(2));
        let cleanup_done = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let started = cleanup_started.clone();
        let done = cleanup_done.clone();

        let cleanup = std::thread::spawn(move || {
            started.wait();
            // SAFETY: the test synchronizes access and no scheduler work is active.
            cleanup_all_actors();
            done.store(true, Ordering::Release);
        });

        cleanup_started.wait();
        // Cleanup takes the deferred handle before joining it, and the join
        // cannot finish while terminate is still running.
        wait_until(|| live_actors::deferred_teardown_thread_count() == 0);
        assert!(
            !cleanup_done.load(Ordering::Acquire),
            "cleanup_all_actors must wait for deferred self-free threads"
        );

        (*actor).terminate_finished.store(true, Ordering::Release);
        cleanup.join().unwrap();
        assert!(
            !is_actor_live(actor),
            "deferred free should finish before cleanup returns"
        );
    }
}

#[test]
fn drain_actors_all_drain_cleans_registries() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();
    let _ticker_guard = crate::timer_periodic::TICKER_TEST_MUTEX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor_one = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor_two = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor_three = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor_one.is_null());
    assert!(!actor_two.is_null());
    assert!(!actor_three.is_null());

    // SAFETY: the spawned actors remain live until the assertions below finish.
    let actor_one_id = unsafe { (*actor_one).id };
    // SAFETY: the spawned actors remain live until the assertions below finish.
    let actor_two_id = unsafe { (*actor_two).id };
    // SAFETY: the spawned actors remain live until the assertions below finish.
    let actor_three_id = unsafe { (*actor_three).id };

    // SAFETY: actor_one is a valid live actor pointer returned by spawn.
    let timer = unsafe { crate::timer_periodic::hew_actor_schedule_periodic(actor_one, 7, 100) };
    assert!(
        !timer.is_null(),
        "periodic timer should register successfully"
    );
    // SAFETY: both actor pointers were returned by spawn and are still live.
    unsafe {
        crate::link::hew_actor_link(actor_one, actor_two);
    }
    // SAFETY: both actor pointers were returned by spawn and are still live.
    let monitor_ref = unsafe {
        crate::monitor::register_actor_monitor(actor_three, actor_one)
            .expect("monitor registration")
    };
    assert_ne!(monitor_ref, 0, "monitor registration should succeed");

    assert_eq!(crate::timer_periodic::timer_count_for_actor(actor_one), 1);
    assert!(crate::link::has_links_for_actor(actor_one_id, actor_one));
    assert!(crate::link::has_links_for_actor(actor_two_id, actor_two));
    assert!(crate::monitor::has_monitors_for_actor(
        actor_one_id,
        actor_one
    ));
    assert!(crate::monitor::has_monitors_for_actor(
        actor_three_id,
        actor_three
    ));

    let outcome = drain_actors(
        &[actor_one_id, actor_two_id, actor_three_id],
        unbounded_drain_deadline(),
    );
    assert_eq!(outcome, DrainOutcome::Drained);
    assert!(!is_actor_live(actor_one));
    assert!(!is_actor_live(actor_two));
    assert!(!is_actor_live(actor_three));
    assert_eq!(crate::timer_periodic::timer_count_for_actor(actor_one), 0);
    assert!(!crate::link::has_links_for_actor(actor_one_id, actor_one));
    assert!(!crate::link::has_links_for_actor(actor_two_id, actor_two));
    assert!(!crate::monitor::has_monitors_for_actor(
        actor_one_id,
        actor_one
    ));
    assert!(!crate::monitor::has_monitors_for_actor(
        actor_three_id,
        actor_three
    ));
}

#[test]
fn drain_actors_partial_drain_with_timeout() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    DRAIN_BUSY_LOOP_STARTED.store(false, Ordering::Release);
    DRAIN_BUSY_LOOP_RELEASE.store(false, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let stubborn_actor =
        unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(drain_busy_loop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let helper_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let spare_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!stubborn_actor.is_null());
    assert!(!helper_actor.is_null());
    assert!(!spare_actor.is_null());

    // SAFETY: the spawned actors remain live until the assertions below finish.
    let stubborn_actor_id = unsafe { (*stubborn_actor).id };
    // SAFETY: the spawned actors remain live until the assertions below finish.
    let helper_actor_id = unsafe { (*helper_actor).id };
    // SAFETY: the spawned actors remain live until the assertions below finish.
    let spare_actor_id = unsafe { (*spare_actor).id };

    // SAFETY: stubborn_actor is a valid live actor pointer returned by spawn.
    unsafe { hew_actor_send(stubborn_actor, 1, ptr::null_mut(), 0) };
    // Busy loop actor should begin running before drain starts.
    wait_until(|| DRAIN_BUSY_LOOP_STARTED.load(Ordering::Acquire));

    let outcome = drain_actors(
        &[stubborn_actor_id, helper_actor_id, spare_actor_id],
        std::time::Instant::now() + std::time::Duration::from_millis(100),
    );
    assert_eq!(
        outcome,
        DrainOutcome::Incomplete {
            still_live: vec![stubborn_actor_id],
            crashed: Vec::new(),
        }
    );
    assert!(
        is_actor_live(stubborn_actor),
        "busy actor must remain live at the deadline"
    );
    assert!(
        !is_actor_live(helper_actor),
        "cooperating actor should be drained"
    );
    assert!(
        !is_actor_live(spare_actor),
        "cooperating actor should be drained"
    );

    DRAIN_BUSY_LOOP_RELEASE.store(true, Ordering::Release);
    // The busy actor becomes quiescent after releasing the loop.
    wait_for_actor_quiescent(stubborn_actor);
    // SAFETY: stubborn_actor is quiescent after the wait above.
    let free_rc = unsafe { hew_actor_free(stubborn_actor) };
    assert_eq!(free_rc, 0);
}

#[test]
fn drain_actors_crashed_during_drain_reports_crashed() {
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    DRAIN_TRAP_ON_STOP_STARTED.store(false, Ordering::Release);
    DRAIN_TRAP_ON_STOP_RELEASE.store(false, Ordering::Release);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(drain_trap_on_stop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: actor remains live until the assertions below finish.
    let actor_id = unsafe { (*actor).id };

    // SAFETY: actor is a valid live actor pointer returned by spawn.
    unsafe { hew_actor_send(actor, 1, ptr::null_mut(), 0) };
    // Trap-on-stop actor should begin running before drain starts.
    wait_until(|| DRAIN_TRAP_ON_STOP_STARTED.load(Ordering::Acquire));

    // Release the dispatch spin only once drain_actors has actually called
    // hew_actor_stop AND the out-of-band stop has been latched on the
    // mailbox. `hew_actor_stop` stores that flag with Release ordering as
    // its last act on the Running branch, so observing it means the next
    // loop-top check will take the stop and the actor goes Running→Crashed
    // (the trap fires on stop) rather than Running→Idle→Stopped. Waiting on
    // this real condition removes the timing bet: under load a fixed sleep
    // could elapse before drain reached stop, releasing the dispatch while
    // the actor was still Idle-bound and yielding Drained.
    //
    // SAFETY: the actor and its mailbox outlive the joined release thread.
    let mailbox_addr = unsafe { (*actor).mailbox } as usize;
    let release_handle = std::thread::spawn(move || {
        let mb = mailbox_addr as *mut HewMailbox;
        // SAFETY: `mb` is the live actor's mailbox; it stays valid until
        // the test joins this thread and frees the actor below.
        while !unsafe { mailbox::mailbox_stop_requested(mb) } {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        DRAIN_TRAP_ON_STOP_RELEASE.store(true, Ordering::Release);
    });

    let outcome = drain_actors(&[actor_id], unbounded_drain_deadline());

    release_handle
        .join()
        .expect("release thread should not panic");

    assert_eq!(
        outcome,
        DrainOutcome::Incomplete {
            still_live: Vec::new(),
            crashed: vec![actor_id],
        }
    );
    assert!(
        is_actor_live(actor),
        "crashed actors should remain tracked for caller-directed cleanup"
    );
    // SAFETY: crashed actors remain tracked until the explicit free below.
    let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    assert_eq!(actor_state, HewActorState::Crashed as i32);
    // SAFETY: crashed actors are quiescent and can be explicitly freed.
    let free_rc = unsafe { hew_actor_free(actor) };
    assert_eq!(free_rc, 0);
}

#[test]
fn drain_actors_with_pending_timer_cancels_timer() {
    // Pin the canonical ordering: when an actor with a registered
    // periodic timer is drained, the timer must be cancelled before
    // the actor is freed.
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();
    let _ticker_guard = crate::timer_periodic::TICKER_TEST_MUTEX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: the spawned actor remains live until the assertions below finish.
    let actor_id = unsafe { (*actor).id };

    // SAFETY: actor is a valid live actor pointer returned by spawn.
    let timer = unsafe { crate::timer_periodic::hew_actor_schedule_periodic(actor, 7, 100) };
    assert!(
        !timer.is_null(),
        "periodic timer should register successfully"
    );
    assert_eq!(crate::timer_periodic::timer_count_for_actor(actor), 1);

    let outcome = drain_actors(&[actor_id], unbounded_drain_deadline());
    assert_eq!(outcome, DrainOutcome::Drained);
    assert!(
        !is_actor_live(actor),
        "drained actor should be removed from live tracking"
    );
    assert_eq!(
        crate::timer_periodic::timer_count_for_actor(actor),
        0,
        "drain must cancel pending periodic timers"
    );
}

#[test]
fn drain_actors_with_active_link_removes_link() {
    // Pin the canonical ordering: draining an actor with active link
    // entries must drop both sides of the link before the actor is
    // freed. This guards against teardown paths that skipped link
    // cleanup and left dangling references.
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor_one = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let actor_two = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor_one.is_null());
    assert!(!actor_two.is_null());

    // SAFETY: spawned actors remain live until the assertions below finish.
    let actor_one_id = unsafe { (*actor_one).id };
    // SAFETY: spawned actors remain live until the assertions below finish.
    let actor_two_id = unsafe { (*actor_two).id };

    // SAFETY: both actor pointers were returned by spawn and are still live.
    unsafe {
        crate::link::hew_actor_link(actor_one, actor_two);
    }
    assert!(crate::link::has_links_for_actor(actor_one_id, actor_one));
    assert!(crate::link::has_links_for_actor(actor_two_id, actor_two));

    // Drain only `actor_one`. The peer side of the link must be cleared
    // even though `actor_two` is being drained in the same batch.
    let outcome = drain_actors(&[actor_one_id, actor_two_id], unbounded_drain_deadline());
    assert_eq!(outcome, DrainOutcome::Drained);
    assert!(!is_actor_live(actor_one));
    assert!(!is_actor_live(actor_two));
    assert!(
        !crate::link::has_links_for_actor(actor_one_id, actor_one),
        "drain must remove links owned by drained actors"
    );
    assert!(
        !crate::link::has_links_for_actor(actor_two_id, actor_two),
        "drain must remove links owned by drained actors"
    );
}

#[test]
fn drain_actors_with_active_monitor_removes_monitor() {
    // Pin the canonical ordering: draining an actor that is being monitored
    // must remove both the monitored and the observer side of the monitor
    // entry before the actors are freed. This guards against teardown paths
    // that skipped monitor cleanup and left dangling references.
    let _guard = crate::runtime_test_guard();
    let _scheduler = NativeSchedulerGuard::new();

    // SAFETY: null state + valid dispatch are valid spawn args.
    let monitored = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    // SAFETY: null state + valid dispatch are valid spawn args.
    let observer = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!monitored.is_null());
    assert!(!observer.is_null());

    // SAFETY: spawned actors remain live until the assertions below finish.
    let monitored_id = unsafe { (*monitored).id };
    // SAFETY: spawned actors remain live until the assertions below finish.
    let observer_id = unsafe { (*observer).id };

    // Register `observer` as a monitor of `monitored`.
    // SAFETY: both actor pointers were returned by spawn and are still live.
    let monitor_ref = unsafe {
        crate::monitor::register_actor_monitor(observer, monitored).expect("monitor registration")
    };
    assert_ne!(monitor_ref, 0, "monitor registration should succeed");
    assert!(
        crate::monitor::has_monitors_for_actor(monitored_id, monitored),
        "monitored actor should have a monitor entry"
    );
    assert!(
        crate::monitor::has_monitors_for_actor(observer_id, observer),
        "observer actor should have a monitor entry"
    );

    let outcome = drain_actors(&[monitored_id, observer_id], unbounded_drain_deadline());
    assert_eq!(outcome, DrainOutcome::Drained);
    assert!(!is_actor_live(monitored));
    assert!(!is_actor_live(observer));
    assert!(
        !crate::monitor::has_monitors_for_actor(monitored_id, monitored),
        "drain must remove monitor entries owned by the monitored actor"
    );
    assert!(
        !crate::monitor::has_monitors_for_actor(observer_id, observer),
        "drain must remove monitor entries owned by the observer actor"
    );
}

#[test]
fn drain_actors_empty_set_returns_drained() {
    assert_eq!(
        drain_actors(&[], std::time::Instant::now()),
        DrainOutcome::Drained
    );
}

#[test]
fn deep_copy_state_copies_data_correctly() {
    let src: [u8; 4] = [0xDE, 0xAD, 0xBE, 0xEF];
    // SAFETY: src is a valid 4-byte buffer.
    let dst = unsafe { deep_copy_state(src.as_ptr().cast_mut().cast(), 4) };
    assert!(!dst.is_null());
    // SAFETY: dst is a freshly-allocated 4-byte buffer.
    let copied = unsafe { std::slice::from_raw_parts(dst.cast::<u8>(), 4) };
    assert_eq!(copied, &src);
    // SAFETY: dst came from deep_copy_state's sized-block allocation.
    unsafe { crate::mem::buf_free(dst) };
}

#[test]
fn deep_copy_state_null_source_returns_null() {
    // SAFETY: null source is explicitly handled.
    let dst = unsafe { deep_copy_state(ptr::null_mut(), 64) };
    assert!(dst.is_null());
    // No error should be set for a legitimate null/zero call.
    assert!(crate::hew_last_error().is_null());
}

#[test]
fn deep_copy_state_zero_size_returns_null() {
    let src: u8 = 42;
    // SAFETY: src is valid; size=0 triggers the early return.
    let dst = unsafe { deep_copy_state(std::ptr::from_ref(&src).cast_mut().cast(), 0) };
    assert!(dst.is_null());
}

#[test]
fn hew_actor_set_state_drop_records_callback_pointer() {
    // Verify the field roundtrip: setter stores the function pointer and
    // a subsequent read sees the same address. This is the four-touch
    // counterpart of `terminate_fn`'s setter and uses the same shape.
    unsafe extern "C" fn dummy_state_drop(_state: *mut c_void) {}

    let (actor, mailbox) = make_stop_test_actor(HewActorState::Idle);
    // SAFETY: actor is freshly built and not published; setter is the only
    // writer.
    unsafe {
        assert!(
            (*actor).state_drop_fn.is_none(),
            "state_drop_fn must default to None"
        );
        hew_actor_set_state_drop(actor, dummy_state_drop);
        let stored = (*actor).state_drop_fn.expect("setter must populate slot");
        assert_eq!(
            stored as *const () as usize, dummy_state_drop as *const () as usize,
            "stored callback pointer must match the one passed to the setter"
        );
    }
    // SAFETY: actor and mailbox were allocated above and never published.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mailbox);
    }
}

static STATE_DROP_AUTHORITY_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

unsafe extern "C" fn authority_state_drop_callback(_state: *mut c_void) {
    STATE_DROP_AUTHORITY_COUNT.fetch_add(1, Ordering::SeqCst);
}

#[test]
fn externally_crashed_actor_without_escrow_runs_state_drop_once() {
    // An actor may be marked Crashed while idle, before dispatch has opened
    // or consumed any state escrow. Lifecycle state cannot suppress its
    // still-live typed owner.
    let _guard = crate::runtime_test_guard();
    STATE_DROP_AUTHORITY_COUNT.store(0, Ordering::SeqCst);

    // Spawn with a sized-block-allocated source so the resulting actor has
    // a non-null `state` field (deep-copied). This ensures the
    // state-drop call is not hidden by the inner is_null guard.
    // SAFETY: buf_try_alloc returns a valid 8-byte allocation.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the bytes; src is freed below.
    let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn copied the bytes; release the source allocation.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid and not being dispatched.
    unsafe {
        hew_actor_set_state_drop(actor, authority_state_drop_callback);
        let a = &*actor;
        assert!(!a.state.is_null(), "spawn must produce a non-null state");
        a.actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);

        // Go through the public hew_actor_free entry point so the
        // LIVE_ACTORS untracking, timer cancellation, and link/monitor
        // teardown all fire in the order the runtime expects. The
        let rc = hew_actor_free(actor);
        assert_eq!(rc, 0);
    }

    assert_eq!(
        STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst),
        1,
        "an externally crashed actor retained final typed-drop authority"
    );
}

#[test]
fn crash_escrow_consumed_state_is_not_dropped_twice_at_free() {
    let _guard = crate::runtime_test_guard();
    STATE_DROP_AUTHORITY_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: malloc returns a valid 8-byte allocation or null.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies initialized bytes; src is released below.
    let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn completed its deep copy and retains no source pointer.
    unsafe { crate::mem::buf_free(src) };

    // Model the scheduler's post-drain authority transfer. The callback
    // count represents the escrow's exactly-once typed drop.
    // SAFETY: the test exclusively owns the live actor and intentionally
    // models the scheduler's ordered drop-then-authority transfer.
    unsafe {
        hew_actor_set_state_drop(actor, authority_state_drop_callback);
        authority_state_drop_callback((*actor).state);
        record_dispatch_state_drop_consumed(actor);
        (*actor)
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        assert_eq!(hew_actor_free(actor), 0);
    }
    assert_eq!(
        STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst),
        1,
        "final free must not retry state already consumed by crash escrow"
    );
}

#[test]
fn caught_unwind_after_state_clear_transfers_final_drop_authority() {
    let _guard = crate::runtime_test_guard();
    STATE_DROP_AUTHORITY_COUNT.store(0, Ordering::SeqCst);

    let src = 41_u64;
    // SAFETY: spawn copies the initialized scalar bytes into actor state.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::from_ref(&src).cast_mut().cast(),
            std::mem::size_of::<u64>(),
            Some(noop_dispatch),
        )
    };
    assert!(!actor.is_null());

    // Model the caught-Rust-unwind window after generated code neutralized
    // the escrow field but before it completed the live-field overwrite.
    // Recovery(false) must consume the now-authoritative snapshot and
    // transfer that fact to final actor teardown.
    // SAFETY: the test exclusively owns the actor and brackets one complete
    // dispatch escrow before terminal free.
    unsafe {
        hew_actor_set_state_drop(actor, authority_state_drop_callback);
        assert!(crate::cont::begin_dispatch_crash_cleanup(
            (*actor).state,
            (*actor).state_size,
            Some(authority_state_drop_callback),
        ));
        assert!(crate::cont::hew_dispatch_state_cleanup_clear(
            (*actor).state,
            std::mem::size_of::<u64>() as u64,
        ));
        let outcome = crate::cont::recover_dispatch_crash_cleanup_with_outcome(false);
        assert!(outcome.registry_found);
        assert!(
            outcome.state_authority_consumed,
            "a begun state mutation makes false-recovery one-way"
        );
        assert_eq!(STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst), 1);
        record_dispatch_state_drop_consumed(actor);
        (*actor)
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        assert_eq!(hew_actor_free(actor), 0);
    }
    assert_eq!(
        STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst),
        1,
        "final free must not retry live bytes after a cleared escrow consumed state authority"
    );
}

#[test]
fn caught_unwind_before_state_mutation_preserves_final_drop_authority() {
    let _guard = crate::runtime_test_guard();
    STATE_DROP_AUTHORITY_COUNT.store(0, Ordering::SeqCst);

    let src = 42_u64;
    // SAFETY: spawn copies the initialized scalar bytes into actor state.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::from_ref(&src).cast_mut().cast(),
            std::mem::size_of::<u64>(),
            Some(noop_dispatch),
        )
    };
    assert!(!actor.is_null());

    // Untouched false-recovery remains the control: discard only escrow
    // bytes and leave the original state callback for final actor free.
    // SAFETY: the test exclusively owns the actor and brackets one complete
    // dispatch escrow before terminal free.
    unsafe {
        hew_actor_set_state_drop(actor, authority_state_drop_callback);
        assert!(crate::cont::begin_dispatch_crash_cleanup(
            (*actor).state,
            (*actor).state_size,
            Some(authority_state_drop_callback),
        ));
        let outcome = crate::cont::recover_dispatch_crash_cleanup_with_outcome(false);
        assert!(outcome.registry_found);
        assert!(!outcome.state_authority_consumed);
        assert_eq!(STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst), 0);
        (*actor)
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        assert_eq!(hew_actor_free(actor), 0);
    }
    assert_eq!(
        STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst),
        1,
        "untouched false-recovery must preserve final live-state drop authority"
    );
}

#[test]
fn free_actor_resources_runs_state_drop_on_stopped_actor() {
    // Companion to the crash-authority tests above:
    // a non-Crashed actor MUST still see its state-drop callback fire.
    // Pins the negative case so the crash-skip guard cannot regress to
    // an unconditional skip.
    let _guard = crate::runtime_test_guard();
    STATE_DROP_AUTHORITY_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: malloc returns a valid 8-byte allocation or null.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the bytes; src is freed below.
    let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn copied the bytes; release the source allocation.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid and not being dispatched.
    unsafe {
        hew_actor_set_state_drop(actor, authority_state_drop_callback);
        let a = &*actor;
        assert!(!a.state.is_null(), "spawn must produce a non-null state");
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        let rc = hew_actor_free(actor);
        assert_eq!(rc, 0);
    }

    assert_eq!(
        STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst),
        1,
        "state-drop callback must fire exactly once on a Stopped actor"
    );
}

/// C1 leak probe (D-C1): freeing a never-woken `Suspended` actor destroys
/// its parked continuation exactly once on the free path — the frame-owned
/// heap value (`heap_guard`) does NOT leak. The scratch frame's destroy
/// outline frees `heap_guard` and bumps `destroyed`; asserting `destroyed ==
/// 1` proves the C1 teardown ran, and the freed `heap_guard` is what
/// `MallocScribble` / `leaks --atExit` accounts for in the exec probe.
///
/// Bite-proof: WITHOUT the free-path destroy the `destroyed` counter would
/// stay 0 (and `heap_guard` would leak) — so this assertion fails closed if
/// the C1 wiring regresses. `scratch_destroy` frees only `heap_guard`, not
/// the frame struct, so the test reclaims the frame box afterward (no test
/// leak).
#[test]
fn free_path_destroys_parked_continuation_c1() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: spawn a real actor (null state / size 0 is documented legal).
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // Park a scratch continuation, as a never-woken suspended dispatch
    // would: publish Parked + store the handle, then mark the actor
    // Suspended. The scratch frame owns a real heap_guard allocation the
    // destroy outline must free.
    let frame = crate::coro_exec::test_support::ScratchFrameOwner::new(4);
    let handle = frame.into_handle();
    // SAFETY: actor is live and owned by this test thread.
    unsafe {
        let a = &*actor;
        assert!(crate::coro_exec::begin_park(a).is_ok());
        crate::coro_exec::finish_park(a, handle);
        a.actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        assert!(
            crate::coro_exec::has_live_parked_cont(a),
            "the actor now owns a live parked continuation"
        );
    }

    // Free the actor WITHOUT ever waking the continuation. The C1 free-path
    // teardown must destroy the parked frame exactly once before reclaiming
    // the box (which frees heap_guard via the scratch destroy outline).
    // SAFETY: actor is valid and not being dispatched.
    let rc = unsafe { hew_actor_free(actor) };
    assert_eq!(rc, 0);

    // Reclaim the scratch frame struct (scratch_destroy freed only its
    // heap_guard, not the frame) and assert the destroy outline ran exactly
    // once on the free path.
    // SAFETY: `handle` came from ScratchFrameOwner::into_handle above; its
    // outer allocation remains live because scratch_destroy frees only the
    // heap guard.
    let frame = unsafe { crate::coro_exec::test_support::ScratchFrameOwner::from_handle(handle) };
    assert_eq!(
        frame.destroyed.load(Ordering::Acquire),
        1,
        "C1: the parked continuation is destroyed exactly once on the free path"
    );
    assert!(
        frame.heap_guard.load(Ordering::Acquire).is_null(),
        "the frame-owned heap value was freed by the destroy outline (no leak)"
    );
}

#[test]
fn borrowed_shallow_state_has_no_final_drop_authority() {
    // A legacy supervisor byte-copy incarnation explicitly records that
    // its embedded owners are borrowed from the persistent spec. The same
    // common free entry point used by fresh actors must therefore skip its
    // typed drop without relying on restart context or lifecycle state.
    let _guard = crate::runtime_test_guard();
    STATE_DROP_AUTHORITY_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: malloc returns a valid 8-byte allocation or null.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the bytes; src is freed below.
    let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!actor.is_null());
    // SAFETY: spawn copied the bytes; release the source allocation.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid and not being dispatched.
    unsafe {
        hew_actor_set_state_drop(actor, authority_state_drop_callback);
        let a = &*actor;
        assert!(!a.state.is_null(), "spawn must produce a non-null state");
        mark_state_drop_borrowed(actor);
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        let rc = hew_actor_free(actor);
        assert_eq!(rc, 0);
    }

    assert_eq!(
        STATE_DROP_AUTHORITY_COUNT.load(Ordering::SeqCst),
        0,
        "borrowed shallow state must not claim typed-drop authority"
    );
}

#[test]
fn hew_actor_set_state_drop_null_actor_is_noop() {
    // Spawn returns null on allocation failure; codegen unconditionally
    // calls this setter, so it must tolerate a null receiver without
    // dereferencing. Verifies the cabi_guard short-circuit.
    unsafe extern "C" fn dummy_state_drop(_state: *mut c_void) {}
    // SAFETY: passing null is exactly what we are guarding against; the
    // function must return without touching the pointer.
    unsafe { hew_actor_set_state_drop(std::ptr::null_mut(), dummy_state_drop) };
}

#[test]
fn deep_copy_state_alloc_failure_returns_null_and_sets_error() {
    let _guard = crate::runtime_test_guard();
    let src: u8 = 1;
    crate::hew_clear_error();
    let _guard = fail_actor_state_alloc_on_nth(0);
    // SAFETY: src is valid; allocation failure is injected by the test.
    let dst = unsafe { deep_copy_state(std::ptr::from_ref(&src).cast_mut().cast(), 1) };
    assert!(dst.is_null(), "should return null on allocation failure");
    let err = crate::hew_last_error();
    assert!(!err.is_null(), "hew_last_error should be set after OOM");
    // SAFETY: hew_last_error returned a non-null C string.
    let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
    assert!(
        msg.contains("OOM"),
        "error message should mention OOM, got: {msg}"
    );
}

static TERMINATE_CALL_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

unsafe extern "C-unwind" fn counting_terminate_callback(_state: *mut c_void) {
    TERMINATE_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
}

#[test]
fn terminate_fires_on_normal_stop_and_not_on_crash() {
    // LESSONS: cleanup-all-exits (P0) — on(stop) must run at normal actor
    // teardown (finalize_quiescent_actor_cleanup) but must NOT
    // run when the actor is in the Crashed state (same path guards
    // state_drop_fn). Pins the crash-skip invariant and the normal-stop
    // fire invariant with a minimal in-process test.
    //
    // Both actors spawn with a non-null state (8-byte malloc) so
    // call_terminate_fn does not bail out at the null-state early-return.
    let _guard = crate::runtime_test_guard();
    TERMINATE_CALL_COUNT.store(0, Ordering::SeqCst);

    // --- normal-stop path: terminate_fn must fire ---
    // SAFETY: malloc returns a valid 8-byte allocation or null; freed below.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the 8 bytes.
    let stopped_actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!stopped_actor.is_null());
    // SAFETY: spawn copied the bytes; release the source.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid; terminate not yet called.
    unsafe {
        hew_actor_set_terminate(stopped_actor, counting_terminate_callback);
        let a = &*stopped_actor;
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        let rc = hew_actor_free(stopped_actor);
        assert_eq!(rc, 0, "hew_actor_free on stopped actor must succeed");
    }
    assert_eq!(
        TERMINATE_CALL_COUNT.load(Ordering::SeqCst),
        1,
        "terminate callback must fire exactly once for a Stopped actor"
    );

    // --- crash path: terminate_fn must NOT fire ---
    TERMINATE_CALL_COUNT.store(0, Ordering::SeqCst);
    // SAFETY: malloc returns a valid 8-byte allocation or null; freed below.
    let src = crate::mem::buf_try_alloc(8);
    assert!(!src.is_null());
    // SAFETY: spawn deep-copies the bytes.
    let crashed_actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
    assert!(!crashed_actor.is_null());
    // SAFETY: spawn copied the bytes; release the source.
    unsafe { crate::mem::buf_free(src) };

    // SAFETY: actor is valid; terminate registered but must not run on crash.
    unsafe {
        hew_actor_set_terminate(crashed_actor, counting_terminate_callback);
        let a = &*crashed_actor;
        a.actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        let rc = hew_actor_free(crashed_actor);
        assert_eq!(rc, 0, "hew_actor_free on crashed actor must succeed");
    }
    assert_eq!(
        TERMINATE_CALL_COUNT.load(Ordering::SeqCst),
        0,
        "terminate callback must NOT fire for a Crashed actor"
    );
}

#[test]
fn free_actor_resources_times_out_on_hanging_terminate() {
    let _guard = crate::runtime_test_guard();
    // Simulate an actor whose terminate_called is true but
    // terminate_finished never becomes true. The bounded wait in
    // free_actor_resources should time out after ~5s and proceed.
    // SAFETY: null state, valid dispatch.
    let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null());

    // SAFETY: actor is valid.
    let a = unsafe { &*actor };
    // Simulate a hung terminate: called=true, finished=false.
    a.terminate_called.store(true, Ordering::Release);
    a.terminate_finished.store(false, Ordering::Release);
    // Put actor in Stopped state so hew_actor_free doesn't fail the
    // state check.
    a.actor_state
        .store(HewActorState::Stopped as i32, Ordering::Release);

    let start = std::time::Instant::now();
    // SAFETY: actor is valid and in Stopped state.
    let rc = unsafe { hew_actor_free(actor) };
    let elapsed = start.elapsed();

    assert_eq!(rc, 0);
    // Should take roughly 5 seconds (the timeout), not hang forever.
    assert!(
        elapsed >= std::time::Duration::from_secs(4),
        "should wait ~5s before timing out, took {elapsed:?}"
    );
    assert!(
        elapsed < std::time::Duration::from_secs(10),
        "should not hang much longer than the timeout, took {elapsed:?}"
    );
}

#[test]
fn free_current_actor_from_terminate_is_deferred() {
    let _guard = crate::runtime_test_guard();
    // SAFETY: this test fully owns the spawned actor and simulates the
    // terminate callback state on the current thread.
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());
        let a = &*actor;
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        a.terminate_called.store(true, Ordering::Release);
        a.terminate_finished.store(false, Ordering::Release);

        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor,
            actor_id: (*actor).id,
            ..HewExecutionContext::default()
        });

        // Terminate is still running on this thread, so a free that waited
        // for it here could never return; a deferred free returns with the
        // actor still live.
        let rc = hew_actor_free(actor);
        let live_immediately_after = is_actor_live(actor);

        a.terminate_finished.store(true, Ordering::Release);
        wait_until(|| !is_actor_live(actor));

        assert_eq!(rc, 0, "reentrant terminate frees should still succeed");
        assert!(
            live_immediately_after,
            "reentrant free should defer instead of spin-waiting in terminate"
        );
    }
}

#[test]
fn spawn_with_restart_state_alloc_failure_returns_null_and_sets_error() {
    let _guard = crate::runtime_test_guard();
    let src: u8 = 1;
    crate::hew_clear_error();
    let _guard = fail_actor_state_alloc_on_nth(1);
    // SAFETY: src is valid; allocation failure is injected into the restart-state copy.
    let actor = unsafe {
        hew_actor_spawn(
            std::ptr::from_ref(&src).cast_mut().cast(),
            1,
            Some(noop_dispatch),
        )
    };
    assert!(actor.is_null(), "spawn should return null on OOM");
    let err = crate::hew_last_error();
    assert!(!err.is_null(), "hew_last_error should be set after OOM");
    // SAFETY: hew_last_error returned a non-null C string.
    let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
    assert!(
        msg.contains("OOM"),
        "error message should mention OOM, got: {msg}"
    );
}

/// Freeing an actor via `hew_actor_free` must remove all parse-error slot
/// entries for that actor across every parser kind.
///
/// This guards against unbounded growth of the global parse-error map on
/// long-running nodes that spawn and reap many actors.
///
/// Run 3× to satisfy the flake gate.
#[test]
fn hew_actor_free_clears_parse_error_slots() {
    for _run in 0..3 {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state, valid dispatch.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is valid — returned by hew_actor_spawn.
        let actor_id = unsafe { (*actor).id };

        // Inject errors for all four error kinds.
        crate::parse_error_slot::__set_error_for_actor(
            actor_id,
            crate::parse_error_slot::ErrorSlotKind::Datetime,
            "datetime error",
        );
        crate::parse_error_slot::__set_error_for_actor(
            actor_id,
            crate::parse_error_slot::ErrorSlotKind::Yaml,
            "yaml error",
        );
        crate::parse_error_slot::__set_error_for_actor(
            actor_id,
            crate::parse_error_slot::ErrorSlotKind::Toml,
            "toml error",
        );
        crate::parse_error_slot::__set_error_for_actor(
            actor_id,
            crate::parse_error_slot::ErrorSlotKind::Json,
            "json error",
        );

        // Verify they are present before free.
        assert!(crate::parse_error_slot::__get_error_for_actor(
            actor_id,
            crate::parse_error_slot::ErrorSlotKind::Datetime
        )
        .is_some());

        // Free the actor — this calls prepare_quiescent_actor_for_cleanup
        // which calls parse_error_slot::clear_all_for_actor.
        // SAFETY: actor is valid and was spawned by hew_actor_spawn above.
        let rc = unsafe { hew_actor_free(actor) };
        assert_eq!(rc, 0, "hew_actor_free must succeed");

        // All four slots must now be empty.
        for kind in [
            crate::parse_error_slot::ErrorSlotKind::Datetime,
            crate::parse_error_slot::ErrorSlotKind::Yaml,
            crate::parse_error_slot::ErrorSlotKind::Toml,
            crate::parse_error_slot::ErrorSlotKind::Json,
        ] {
            assert_eq!(
                crate::parse_error_slot::__get_error_for_actor(actor_id, kind),
                None,
                "error slot for {kind:?} must be cleared after actor free"
            );
        }
    }
}

// ── arena_cap_bytes threading via hew_actor_spawn_opts ───────────────

/// `hew_actor_spawn_opts` with `arena_cap_bytes > 0` spawns an actor whose
/// arena enforces the cap: the first allocation over the cap raises the
/// typed `HeapExceeded` unwind caught by the scheduler.
#[test]
fn max_heap_spawn_opts_threads_cap_to_arena() {
    let _guard = crate::runtime_test_guard();

    // Cap: exactly 128 bytes.
    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(noop_dispatch),
        mailbox_capacity: 0,
        overflow: 0,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        message_drop_fn: None,
        budget: 0,
        arena_cap_bytes: 128,
        cycle_capable: 0,
    };

    // SAFETY: opts is valid for the duration of the call.
    let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
    assert!(
        !actor.is_null(),
        "spawn with arena_cap_bytes=128 must succeed"
    );

    // Verify the arena cap was set: install the actor's arena, attempt to
    // alloc 129 bytes (one over cap), and catch the scheduler-facing unwind.
    // SAFETY: actor is valid; arena pointer comes from the actor struct.
    let arena = unsafe { (*actor).arena };
    assert!(!arena.is_null(), "actor arena must be allocated");
    // SAFETY: actor is live for the duration of this test.
    let actor_id = unsafe { (*actor).id };
    let _ctx = TestExecutionContext::install(HewExecutionContext {
        actor,
        actor_id,
        // This unit fixture explicitly supplies the catch boundary that a
        // real scheduler dispatch installs around arena allocation.
        flags: crate::execution_context::HEW_CTX_FLAG_UNWIND_BOUNDARY_INSTALLED,
        ..HewExecutionContext::default()
    });

    // Install the arena lane so hew_arena_malloc routes through it.
    // SAFETY: arena is a valid pointer from hew_arena_new_with_cap.
    unsafe { crate::arena::hew_arena_set_current(arena) };

    // Allocate up to the cap: 128 bytes in a single call.
    // SAFETY: arena is installed and valid.
    let p = unsafe { crate::arena::hew_arena_malloc(128) };
    assert!(!p.is_null(), "128-byte alloc at cap must succeed");

    // Now exceed the cap: one more byte raises HeapExceeded.
    let exhausted = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        // SAFETY: arena is still installed.
        let _ = unsafe { crate::arena::hew_arena_malloc(1) };
    }))
    .expect_err("alloc over arena cap must unwind");
    assert_eq!(
        exhausted.downcast_ref::<HewPanic>().map(|panic| panic.code),
        Some(crate::supervisor::HEW_TRAP_HEAP_EXCEEDED)
    );

    // Restore no-arena state before teardown.
    // SAFETY: null restores no-arena state.
    unsafe { crate::arena::hew_arena_set_current(ptr::null_mut()) };

    // SAFETY: actor is valid and was spawned above.
    let rc = unsafe { hew_actor_free(actor) };
    assert_eq!(rc, 0, "hew_actor_free must succeed");
}

/// `hew_actor_spawn_opts` with `arena_cap_bytes = 0` spawns an actor with
/// an unbounded arena (same as legacy `hew_arena_new`).
#[test]
fn max_heap_spawn_opts_zero_cap_is_unbounded() {
    let _guard = crate::runtime_test_guard();

    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(noop_dispatch),
        mailbox_capacity: 0,
        overflow: 0,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        message_drop_fn: None,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };

    // SAFETY: opts is valid for the duration of the call.
    let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
    assert!(
        !actor.is_null(),
        "spawn with arena_cap_bytes=0 must succeed"
    );

    // SAFETY: actor is valid; arena pointer comes from the actor struct.
    let arena = unsafe { (*actor).arena };
    assert!(!arena.is_null(), "actor arena must be allocated");
    // SAFETY: actor is live for the duration of this test.
    let actor_id = unsafe { (*actor).id };
    let _ctx = TestExecutionContext::install(HewExecutionContext {
        actor,
        actor_id,
        ..HewExecutionContext::default()
    });

    // Install the arena and alloc a large block — must succeed (unbounded).
    // SAFETY: arena is a valid pointer from hew_arena_new.
    unsafe { crate::arena::hew_arena_set_current(arena) };

    // SAFETY: arena is installed.
    let p = unsafe { crate::arena::hew_arena_malloc(65536) };
    assert!(!p.is_null(), "64 KiB alloc in unbounded arena must succeed");

    // SAFETY: null restores no-arena state.
    unsafe { crate::arena::hew_arena_set_current(ptr::null_mut()) };

    // SAFETY: actor is valid.
    let rc = unsafe { hew_actor_free(actor) };
    assert_eq!(rc, 0, "hew_actor_free must succeed");
}

// ── null-arena guard: backport of wasm OOM behaviour to native path ───

/// `hew_actor_spawn` must return null and release all owned resources
/// (`state`, `init_state` copy, mailbox) when `hew_arena_new` fails (OOM).
///
/// Covers the native `spawn_actor_internal` null-arena guard introduced
/// to match the wasm twin's existing OOM handling.  The "no-leak" half is
/// enforced by ASAN in CI; the "no null-arena actor" half is enforced by
/// the null return asserted here (a non-null return from the broken pre-fix
/// code would carry `actor.arena = null` and crash on the first arena alloc).
#[test]
fn spawn_arena_alloc_failure_returns_null() {
    let _guard = crate::runtime_test_guard();

    // ── case 1: zero-size state (init_state is null on the spawn path) ──
    // Arena fails → cleanup_failed_spawn frees the (empty) state + mailbox.
    crate::hew_clear_error();
    let _arena_guard = fail_arena_alloc_next();
    // SAFETY: null state with size=0 is valid; dispatch is a valid fn ptr.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(
        actor.is_null(),
        "spawn must return null when arena allocation fails (zero-state path)"
    );
    // Guard is consumed; injection is disarmed.

    // ── case 2: non-null state (init_state is allocated then freed) ──
    // `deep_copy_state` succeeds twice (state copy + init_state copy);
    // arena fails → cleanup_failed_spawn frees both copies + mailbox.
    let src: [u8; 8] = [0xA1, 0xB2, 0xC3, 0xD4, 0xE5, 0xF6, 0x07, 0x18];
    crate::hew_clear_error();
    let _arena_guard = fail_arena_alloc_next();
    // SAFETY: src is a valid 8-byte readable buffer; dispatch is valid.
    let actor = unsafe {
        hew_actor_spawn(
            src.as_ptr().cast_mut().cast(),
            src.len(),
            Some(noop_dispatch),
        )
    };
    assert!(
        actor.is_null(),
        "spawn must return null when arena allocation fails (with-state path)"
    );

    // ── case 3: normal spawn succeeds immediately after ──
    // Verifies the injection is fully disarmed and the runtime is intact.
    // SAFETY: null state with size=0 is valid.
    let ok_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(
        !ok_actor.is_null(),
        "normal spawn after failed arena alloc must succeed"
    );
    // SAFETY: ok_actor is a valid pointer from hew_actor_spawn.
    let rc = unsafe { hew_actor_free(ok_actor) };
    assert_eq!(rc, 0, "hew_actor_free on the recovery actor must succeed");
}

// ── actor-serial exhaustion: the packed id must never alias ──────────
//
// `pid::hew_pid_make` masks the serial to 48 bits, so the allocation after
// `MAX_ACTOR_SERIAL` packs to PID 0 (the invalid-actor sentinel that
// `hew_node_api_register_by_pid` and the pool lookups read as "no actor")
// and every allocation after that re-issues an id already live. The
// allocator refuses instead.

#[test]
fn actor_serial_allocator_stops_at_the_representable_boundary() {
    let counter = AtomicU64::new(MAX_SPAWN_SERIAL);
    assert_eq!(
        take_actor_serial(&counter),
        Some(MAX_SPAWN_SERIAL),
        "the last representable serial must still be issued"
    );
    assert_eq!(
        take_actor_serial(&counter),
        None,
        "the allocation past the boundary must be refused"
    );
    assert_eq!(
        counter.load(Ordering::Relaxed),
        MAX_SPAWN_SERIAL + 1,
        "a refused allocation must not advance the counter — an unbounded \
             counter wraps back into the live id range"
    );
    // Refusal is sticky: it does not clear itself on the next call.
    assert_eq!(take_actor_serial(&counter), None);
}

#[test]
fn spawn_with_exhausted_serial_space_returns_null() {
    let _guard = crate::runtime_test_guard();

    crate::hew_clear_error();
    seed_next_actor_serial(MAX_SPAWN_SERIAL + 1);
    // SAFETY: null state with size=0 is valid; dispatch is a valid fn ptr.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(
        actor.is_null(),
        "spawn must refuse once the serial space is exhausted, never mint PID 0"
    );
    let err = crate::hew_last_error();
    assert!(!err.is_null(), "the refusal must record a diagnostic");
    // SAFETY: `hew_last_error` returned a non-null, NUL-terminated C string
    // owned by the thread-local slot; it stays valid until the next write.
    let msg = unsafe { std::ffi::CStr::from_ptr(err) }
        .to_str()
        .expect("last-error message is valid UTF-8");
    assert!(
        msg.contains("serial space exhausted"),
        "the refusal must name its cause, got: {msg}"
    );

    // The seed is one-shot: the very next spawn uses the real counter and
    // succeeds, so exhaustion cannot leak into sibling tests.
    // SAFETY: null state with size=0 is valid; dispatch is a valid fn ptr.
    let ok_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(
        !ok_actor.is_null(),
        "the next spawn must be unaffected by the one-shot seed"
    );
    // SAFETY: ok_actor is a valid pointer from hew_actor_spawn.
    assert_eq!(unsafe { hew_actor_free(ok_actor) }, 0);
}

#[test]
fn spawn_at_the_last_representable_serial_still_succeeds() {
    let _guard = crate::runtime_test_guard();

    seed_next_actor_serial(MAX_SPAWN_SERIAL);
    // SAFETY: null state with size=0 is valid; dispatch is a valid fn ptr.
    let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
    assert!(!actor.is_null(), "the boundary serial must still spawn");
    // SAFETY: actor is a live allocation from hew_actor_spawn.
    let (id, serial) = unsafe { ((*actor).id, (*actor).spawn_serial) };
    assert_eq!(serial, MAX_SPAWN_SERIAL);
    assert_ne!(
        id, 0,
        "a boundary spawn must not carry the invalid sentinel"
    );
    assert_eq!(crate::pid::hew_pid_serial(id), MAX_SPAWN_SERIAL);
    // SAFETY: actor is a valid pointer from hew_actor_spawn.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

// ── gen_sink CAS-race double-free regression (PR #2401 finding) ──────
//
// `hew_actor_gen_sink_complete` (the pump's clean-exit release) and
// `fault_close_registered_gen_sink` (the crash/teardown release) both
// race on the same `AtomicPtr` slot when a terminal teardown fires
// concurrently with the pump's own generator-exhausted exit. Before the
// fix, `hew_actor_gen_sink_complete` discarded its CAS result and
// unconditionally called `hew_sink_close(sink)` even when it LOST the
// race — double-freeing the same `Box<HewSink>` the fault path had
// already closed. These tests deterministically force each ordering
// (no real thread race needed) by calling the two release paths
// back-to-back on a single actor + sink pair.
