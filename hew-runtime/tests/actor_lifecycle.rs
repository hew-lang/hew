//! Actor lifecycle integration tests.
//!
//! Tests the critical actor lifecycle paths: spawn → dispatch → close → free,
//! mailbox FIFO ordering, and the ask/reply pattern.  These exercise the
//! lowest-coverage areas of the actor module.
//!
//! Every test uses condvar-based signalling instead of fixed sleeps so
//! that behaviour is deterministic under CI load.

// Many tests deliberately exercise raw FFI functions that are inherently unsafe.
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants are documented per-test"
)]
#![expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "FFI tests use deliberate casts between pointer and integer types"
)]

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_close, hew_actor_free, hew_actor_send, hew_actor_spawn};
use hew_runtime::internal::types::{HewActorState, HewError};
use hew_runtime::mailbox::{
    hew_mailbox_free, hew_mailbox_has_messages, hew_mailbox_len, hew_mailbox_new, hew_mailbox_send,
    hew_mailbox_try_recv, hew_msg_node_free,
};
use hew_runtime::reply_channel;

// ── Global scheduler init ───────────────────────────────────────────────

static SCHED_INIT: std::sync::Once = std::sync::Once::new();

fn ensure_scheduler() {
    SCHED_INIT.call_once(|| {
        hew_runtime::scheduler::hew_sched_init();
    });
}

// ── Condvar-based dispatch signalling ───────────────────────────────────

/// Per-test dispatch signal: records (count, vec-of-received-values).
/// Each test that needs its own signal defines one as a `static`.
struct DispatchLog {
    count: Mutex<i32>,
    cond: Condvar,
}

impl DispatchLog {
    const fn new() -> Self {
        Self {
            count: Mutex::new(0),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        *self.count.lock().unwrap() = 0;
    }

    fn wait_for(&self, expected: i32, timeout: Duration) -> bool {
        let mut count = self.count.lock().unwrap();
        let deadline = Instant::now() + timeout;
        while *count < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = self.cond.wait_timeout(count, remaining).unwrap();
            count = guard;
            if result.timed_out() && *count < expected {
                return false;
            }
        }
        true
    }

    fn current(&self) -> i32 {
        *self.count.lock().unwrap()
    }
}

unsafe extern "C" fn noop_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
}

// ═══════════════════════════════════════════════════════════════════════
// 2. Actor send and dispatch via scheduler
// ═══════════════════════════════════════════════════════════════════════

static SEND_RECV_SIGNAL: DispatchLog = DispatchLog::new();

/// Serialisation lock for tests that share `SEND_RECV_SIGNAL`.
static SEND_RECV_LOCK: Mutex<()> = Mutex::new(());

unsafe extern "C" fn send_recv_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    let mut count = SEND_RECV_SIGNAL.count.lock().unwrap();
    *count += 1;
    SEND_RECV_SIGNAL.cond.notify_all();
}

/// Send a single message to an actor and confirm the dispatch function runs.
#[test]
fn actor_send_triggers_dispatch() {
    let _guard = SEND_RECV_LOCK.lock().unwrap();
    ensure_scheduler();
    SEND_RECV_SIGNAL.reset();

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(send_recv_dispatch),
        );

        let val: i32 = 42;
        hew_actor_send(
            actor,
            1,
            (&raw const val).cast_mut().cast(),
            size_of::<i32>(),
        );

        assert!(
            SEND_RECV_SIGNAL.wait_for(1, Duration::from_secs(10)),
            "dispatch must be invoked after send (count={})",
            SEND_RECV_SIGNAL.current()
        );

        hew_actor_free(actor);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 3. Actor close / stop lifecycle
// ═══════════════════════════════════════════════════════════════════════

/// Closing an idle actor transitions it directly to Stopped.
#[test]
fn actor_close_idle_transitions_to_stopped() {
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());

        // Actor is idle (no messages enqueued).
        let state_before = (*actor).actor_state.load(Ordering::Acquire);
        assert_eq!(
            state_before,
            HewActorState::Idle as i32,
            "precondition: actor must be idle"
        );

        hew_actor_close(actor);

        let state_after = (*actor).actor_state.load(Ordering::Acquire);
        assert_eq!(
            state_after,
            HewActorState::Stopped as i32,
            "close on an idle actor should transition to Stopped"
        );

        hew_actor_free(actor);
    }
}

/// After closing an actor, sends should be rejected (mailbox closed).
#[test]
fn send_to_closed_actor_is_rejected() {
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());

        hew_actor_close(actor);

        // try_send should return ErrActorStopped or ErrClosed.
        let val: i32 = 7;
        let rc = hew_runtime::actor::hew_actor_try_send(
            actor,
            1,
            (&raw const val).cast_mut().cast(),
            size_of::<i32>(),
        );
        assert!(
            rc == HewError::ErrActorStopped as i32 || rc == HewError::ErrClosed as i32,
            "send to a closed actor should fail (got {rc})"
        );

        hew_actor_free(actor);
    }
}

/// Full lifecycle: spawn → send → close → free.
#[test]
fn actor_full_lifecycle_spawn_send_close_free() {
    static LIFECYCLE_SIGNAL: DispatchLog = DispatchLog::new();
    static LIFECYCLE_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C" fn lifecycle_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let mut count = LIFECYCLE_SIGNAL.count.lock().unwrap();
        *count += 1;
        LIFECYCLE_SIGNAL.cond.notify_all();
    }

    let _guard = LIFECYCLE_LOCK.lock().unwrap();
    ensure_scheduler();
    LIFECYCLE_SIGNAL.reset();

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(lifecycle_dispatch),
        );
        assert!(!actor.is_null());

        // Send a message and wait for dispatch.
        hew_actor_send(actor, 1, ptr::null_mut(), 0);
        assert!(
            LIFECYCLE_SIGNAL.wait_for(1, Duration::from_secs(10)),
            "dispatch not invoked"
        );

        // Close and free.
        hew_actor_close(actor);
        let rc = hew_actor_free(actor);
        assert!(rc == 0, "hew_actor_free should succeed (got {rc})");
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 4. Mailbox FIFO message ordering
// ═══════════════════════════════════════════════════════════════════════

/// Send multiple messages to a raw mailbox and verify FIFO dequeue order.
#[test]
fn mailbox_fifo_ordering() {
    unsafe {
        let mb = hew_mailbox_new();
        assert!(!mb.is_null());

        // Enqueue messages with distinct msg_type tags 0..5.
        for i in 0..5i32 {
            let mut val = i * 10;
            hew_mailbox_send(mb, i, (&raw mut val).cast(), size_of::<i32>());
        }

        assert_eq!(hew_mailbox_len(mb), 5, "mailbox should have 5 messages");

        // Dequeue and verify ordering.
        for expected_type in 0..5i32 {
            let node = hew_mailbox_try_recv(mb);
            assert!(
                !node.is_null(),
                "expected message {expected_type} but mailbox was empty"
            );
            assert_eq!(
                (*node).msg_type,
                expected_type,
                "messages must be dequeued in FIFO order"
            );
            let payload = *((*node).data.cast::<i32>());
            assert_eq!(
                payload,
                expected_type * 10,
                "payload must match the sent value"
            );
            hew_msg_node_free(node);
        }

        // Mailbox should be empty now.
        assert_eq!(
            hew_mailbox_has_messages(mb),
            0,
            "mailbox should be empty after draining"
        );

        hew_mailbox_free(mb);
    }
}

/// Interleave sends and receives to verify partial-drain FIFO behaviour.
#[test]
fn mailbox_interleaved_send_recv_preserves_order() {
    unsafe {
        let mb = hew_mailbox_new();

        // Send two messages.
        let mut v0: i32 = 100;
        let mut v1: i32 = 200;
        hew_mailbox_send(mb, 0, (&raw mut v0).cast(), size_of::<i32>());
        hew_mailbox_send(mb, 1, (&raw mut v1).cast(), size_of::<i32>());

        // Receive first — should be v0.
        let node0 = hew_mailbox_try_recv(mb);
        assert!(!node0.is_null());
        assert_eq!((*node0).msg_type, 0);
        assert_eq!(*((*node0).data.cast::<i32>()), 100);
        hew_msg_node_free(node0);

        // Send another before receiving the second.
        let mut v2: i32 = 300;
        hew_mailbox_send(mb, 2, (&raw mut v2).cast(), size_of::<i32>());

        // Receive second — should be v1 (FIFO).
        let node1 = hew_mailbox_try_recv(mb);
        assert!(!node1.is_null());
        assert_eq!((*node1).msg_type, 1);
        assert_eq!(*((*node1).data.cast::<i32>()), 200);
        hew_msg_node_free(node1);

        // Receive third — should be v2.
        let node2 = hew_mailbox_try_recv(mb);
        assert!(!node2.is_null());
        assert_eq!((*node2).msg_type, 2);
        assert_eq!(*((*node2).data.cast::<i32>()), 300);
        hew_msg_node_free(node2);

        hew_mailbox_free(mb);
    }
}

/// Verify that `try_recv` returns null on an empty mailbox.
#[test]
fn mailbox_try_recv_empty_returns_null() {
    unsafe {
        let mb = hew_mailbox_new();
        let node = hew_mailbox_try_recv(mb);
        assert!(node.is_null(), "try_recv on empty mailbox must return null");
        hew_mailbox_free(mb);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 5. Actor ask / reply pattern
// ═══════════════════════════════════════════════════════════════════════

/// Dispatch function that echoes the payload doubled via the reply channel.
///
/// The reply channel is retrieved from the scheduler's thread-local
/// (set from `HewMsgNode.reply_channel` before dispatch).
unsafe extern "C" fn echo_double_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    if data.is_null() || data_size < size_of::<i32>() {
        return;
    }

    unsafe {
        // Read the original i32 payload.
        let payload = *(data.cast::<i32>());

        // Get reply channel from scheduler thread-local.
        let ch = hew_runtime::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return;
        }

        // Reply with payload * 2.
        let mut doubled = payload * 2;
        reply_channel::hew_reply(ch.cast(), (&raw mut doubled).cast(), size_of::<i32>());
    }
}

/// Ask an actor to double a value and verify the reply.
#[test]
fn actor_ask_reply_roundtrip() {
    ensure_scheduler();

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(echo_double_dispatch),
        );
        assert!(!actor.is_null());

        let val: i32 = 21;
        let reply = hew_runtime::actor::hew_actor_ask(
            actor,
            1,
            (&raw const val).cast_mut().cast(),
            size_of::<i32>(),
        );

        assert!(!reply.is_null(), "ask should receive a non-null reply");
        let result = *(reply.cast::<i32>());
        assert_eq!(result, 42, "echo_double should return 21 * 2 = 42");

        // Free the reply payload (it was malloc'd by hew_reply).
        libc::free(reply);
        hew_actor_free(actor);
    }
}

/// Ask with a timeout — the echo actor should reply well within the limit.
#[test]
fn actor_ask_with_timeout_succeeds() {
    ensure_scheduler();

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(echo_double_dispatch),
        );
        assert!(!actor.is_null());

        let val: i32 = 5;
        let reply = hew_runtime::actor::hew_actor_ask_timeout(
            actor,
            1,
            (&raw const val).cast_mut().cast(),
            size_of::<i32>(),
            5000, // 5 second timeout — generous
        );

        assert!(
            !reply.is_null(),
            "ask_timeout should succeed within 5 seconds"
        );
        let result = *(reply.cast::<i32>());
        assert_eq!(result, 10, "echo_double should return 5 * 2 = 10");

        libc::free(reply);
        hew_actor_free(actor);
    }
}

/// Ask on a closed actor should return null (send failure).
#[test]
fn actor_ask_closed_returns_null() {
    ensure_scheduler();

    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());

        hew_actor_close(actor);

        let reply = hew_runtime::actor::hew_actor_ask(actor, 1, ptr::null_mut(), 0);
        assert!(reply.is_null(), "ask on a closed actor should return null");

        hew_actor_free(actor);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 6. Multi-message dispatch ordering
// ═══════════════════════════════════════════════════════════════════════

/// Record of (`msg_type`, payload) received by the dispatch function.
struct OrderLog {
    entries: Mutex<Vec<(i32, i32)>>,
    count: Mutex<i32>,
    cond: Condvar,
}

impl OrderLog {
    const fn new() -> Self {
        Self {
            entries: Mutex::new(Vec::new()),
            count: Mutex::new(0),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        self.entries.lock().unwrap().clear();
        *self.count.lock().unwrap() = 0;
    }

    fn wait_for(&self, expected: i32, timeout: Duration) -> bool {
        let mut count = self.count.lock().unwrap();
        let deadline = Instant::now() + timeout;
        while *count < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = self.cond.wait_timeout(count, remaining).unwrap();
            count = guard;
            if result.timed_out() && *count < expected {
                return false;
            }
        }
        true
    }
}

static ORDER_LOG: OrderLog = OrderLog::new();
static ORDER_LOCK: Mutex<()> = Mutex::new(());

unsafe extern "C" fn order_dispatch(
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    let payload = if !data.is_null() && data_size >= size_of::<i32>() {
        unsafe { *(data.cast::<i32>()) }
    } else {
        -1
    };

    ORDER_LOG.entries.lock().unwrap().push((msg_type, payload));
    let mut count = ORDER_LOG.count.lock().unwrap();
    *count += 1;
    ORDER_LOG.cond.notify_all();
}

/// Send N messages with sequential payloads and verify the actor receives
/// them in the same order (FIFO guarantee of the mailbox).
#[test]
fn actor_dispatch_preserves_message_order() {
    const N: i32 = 10;

    let _guard = ORDER_LOCK.lock().unwrap();
    ensure_scheduler();
    ORDER_LOG.reset();

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(order_dispatch),
        );

        for i in 0..N {
            let mut val = i;
            hew_actor_send(actor, i, (&raw mut val).cast(), size_of::<i32>());
        }

        assert!(
            ORDER_LOG.wait_for(N, Duration::from_secs(10)),
            "not all messages dispatched"
        );

        let entries = ORDER_LOG.entries.lock().unwrap();
        assert_eq!(
            entries.len(),
            N as usize,
            "should have received {N} messages"
        );

        for (idx, &(msg_type, payload)) in entries.iter().enumerate() {
            assert_eq!(msg_type, idx as i32, "msg_type mismatch at index {idx}");
            assert_eq!(payload, idx as i32, "payload mismatch at index {idx}");
        }

        hew_actor_free(actor);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 7. Actor state transitions under dispatch
// ═══════════════════════════════════════════════════════════════════════

static STATE_SIGNAL: DispatchLog = DispatchLog::new();
static STATE_LOCK: Mutex<()> = Mutex::new(());

/// Recorded actor state observed *during* dispatch (should be Running).
static OBSERVED_STATE: AtomicI32 = AtomicI32::new(-1);

unsafe extern "C" fn state_observing_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    // Read the current actor's state during dispatch.
    let me = hew_runtime::actor::hew_actor_self();
    if !me.is_null() {
        let s = unsafe { (*me).actor_state.load(Ordering::Acquire) };
        OBSERVED_STATE.store(s, Ordering::Release);
    }

    let mut count = STATE_SIGNAL.count.lock().unwrap();
    *count += 1;
    STATE_SIGNAL.cond.notify_all();
}

/// During dispatch, the actor should be in the Running state.
#[test]
fn actor_is_running_during_dispatch() {
    let _guard = STATE_LOCK.lock().unwrap();
    ensure_scheduler();
    STATE_SIGNAL.reset();
    OBSERVED_STATE.store(-1, Ordering::Release);

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(state_observing_dispatch),
        );

        hew_actor_send(actor, 1, ptr::null_mut(), 0);
        assert!(
            STATE_SIGNAL.wait_for(1, Duration::from_secs(10)),
            "dispatch never ran"
        );

        let observed = OBSERVED_STATE.load(Ordering::Acquire);
        assert_eq!(
            observed,
            HewActorState::Running as i32,
            "actor should be Running during dispatch (got {observed})"
        );

        hew_actor_free(actor);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 8. Mailbox deep-copy isolation
// ═══════════════════════════════════════════════════════════════════════

/// Verify that mailbox messages are deep-copied: mutating the original
/// buffer after send does not affect the enqueued message.
#[test]
fn mailbox_deep_copy_isolates_sender_and_receiver() {
    unsafe {
        let mb = hew_mailbox_new();
        let mut val: i32 = 42;

        hew_mailbox_send(mb, 0, (&raw mut val).cast(), size_of::<i32>());

        // Mutate the original after send.
        val = 999;

        let node = hew_mailbox_try_recv(mb);
        assert!(!node.is_null());
        let received = *((*node).data.cast::<i32>());
        assert_eq!(
            received, 42,
            "message should contain the value at send time, not the mutated value"
        );

        let _ = val; // suppress unused warning
        hew_msg_node_free(node);
        hew_mailbox_free(mb);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 9. Budget and priority configuration
// ═══════════════════════════════════════════════════════════════════════

/// Verify that budget and priority setters/getters round-trip correctly.
#[test]
fn actor_budget_and_priority_roundtrip() {
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());

        // Set custom budget.
        hew_runtime::actor::hew_actor_set_budget(actor, 128);
        assert_eq!(
            hew_runtime::actor::hew_actor_get_budget(actor),
            128,
            "budget should round-trip"
        );

        // Set priority.
        hew_runtime::actor::hew_actor_set_priority(actor, 2); // low
        assert_eq!(
            hew_runtime::actor::hew_actor_get_priority(actor),
            2,
            "priority should round-trip"
        );

        // Reset budget to default (pass 0).
        hew_runtime::actor::hew_actor_set_budget(actor, 0);
        assert_eq!(
            hew_runtime::actor::hew_actor_get_budget(actor),
            hew_runtime::actor::HEW_MSG_BUDGET as u32,
            "budget of 0 should reset to default"
        );

        hew_actor_free(actor);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 10. Actor error / trap
// ═══════════════════════════════════════════════════════════════════════

/// `hew_actor_trap` sets the error code and transitions to Crashed.
#[test]
fn actor_trap_sets_error_and_crashes() {
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!actor.is_null());

        // No error initially.
        assert_eq!(
            hew_runtime::actor::hew_actor_get_error(actor),
            0,
            "new actor should have error code 0"
        );

        // Trap with error code 42.
        hew_runtime::actor::hew_actor_trap(actor, 42);

        assert_eq!(
            hew_runtime::actor::hew_actor_get_error(actor),
            42,
            "trap should set the error code"
        );
        assert_eq!(
            (*actor).actor_state.load(Ordering::Acquire),
            HewActorState::Crashed as i32,
            "trap should transition actor to Crashed"
        );

        hew_actor_free(actor);
    }
}
