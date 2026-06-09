//! Actor lifecycle integration tests.
//!
//! Tests the critical actor lifecycle paths: spawn → dispatch → close → free,
//! mailbox FIFO ordering, and the ask/reply pattern.  These exercise the
//! lowest-coverage areas of the actor module.
//!
//! Every test uses condvar-based signalling instead of fixed sleeps so
//! that behaviour is deterministic under CI load.
//!
//! Handle lifecycle (spawn/close/free, mailbox new/free, msg-node free)
//! is funnelled through `hew_runtime_testkit::{TestActor, TestMailbox}`.
//! The remaining `unsafe { … }` blocks are inherent to the C ABI: dispatch
//! callbacks have `extern "C"` signatures, and reading `HewMsgNode` payload
//! fields requires pointer dereference.

// Dispatch callbacks and the few remaining FFI calls (reply channel,
// scheduler reply lookup) are inherently unsafe by C ABI contract — the
// safety invariants are documented at each surviving site.
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — dispatch and msg-node accesses use the runtime's documented invariants"
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

use hew_runtime::reply_channel;
use hew_runtime_testkit::{ensure_scheduler, HewActorState, HewError, TestActor, TestMailbox};

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

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) {
}

// ═══════════════════════════════════════════════════════════════════════
// 2. Actor send and dispatch via scheduler
// ═══════════════════════════════════════════════════════════════════════

static SEND_RECV_SIGNAL: DispatchLog = DispatchLog::new();

/// Serialisation lock for tests that share `SEND_RECV_SIGNAL`.
static SEND_RECV_LOCK: Mutex<()> = Mutex::new(());

unsafe extern "C-unwind" fn send_recv_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
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

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, send_recv_dispatch);

    let mut val: i32 = 42;
    actor.send(1, &mut val);

    assert!(
        SEND_RECV_SIGNAL.wait_for(1, Duration::from_secs(10)),
        "dispatch must be invoked after send (count={})",
        SEND_RECV_SIGNAL.current()
    );
}

// ═══════════════════════════════════════════════════════════════════════
// 3. Actor close / stop lifecycle
// ═══════════════════════════════════════════════════════════════════════

/// Closing an idle actor transitions it directly to Stopped.
#[test]
fn actor_close_idle_transitions_to_stopped() {
    let actor = TestActor::spawn(noop_dispatch);

    // Actor is idle (no messages enqueued).
    assert_eq!(
        actor.state_raw(),
        HewActorState::Idle as i32,
        "precondition: actor must be idle"
    );

    actor.close();

    assert_eq!(
        actor.state_raw(),
        HewActorState::Stopped as i32,
        "close on an idle actor should transition to Stopped"
    );
}

/// After closing an actor, sends should be rejected (mailbox closed).
///
/// `hew_actor_try_send` delegates to `hew_mailbox_try_send` on native targets,
/// which returns [`HewError::ErrClosed`] (`-4`) when the mailbox is closed.
/// Note that `hew_mailbox_send` (the blocking variant) returns
/// [`HewError::ErrActorStopped`] (`-2`) instead — see the native/WASM
/// divergence note on that function.
#[test]
fn send_to_closed_actor_is_rejected() {
    let actor = TestActor::spawn(noop_dispatch);
    actor.close();

    // hew_actor_try_send → hew_mailbox_try_send → ErrClosed (-4).
    let mut val: i32 = 7;
    let rc = actor.try_send(1, &mut val);
    assert_eq!(
        rc,
        HewError::ErrClosed as i32,
        "hew_actor_try_send to a closed actor must return ErrClosed (got {rc})"
    );
}

/// Full lifecycle: spawn → send → close → free.
#[test]
fn actor_full_lifecycle_spawn_send_close_free() {
    static LIFECYCLE_SIGNAL: DispatchLog = DispatchLog::new();
    static LIFECYCLE_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C-unwind" fn lifecycle_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) {
        let mut count = LIFECYCLE_SIGNAL.count.lock().unwrap();
        *count += 1;
        LIFECYCLE_SIGNAL.cond.notify_all();
    }

    let _guard = LIFECYCLE_LOCK.lock().unwrap();
    ensure_scheduler();
    LIFECYCLE_SIGNAL.reset();

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, lifecycle_dispatch);

    // Send a message and wait for dispatch.
    actor.send_empty(1);
    assert!(
        LIFECYCLE_SIGNAL.wait_for(1, Duration::from_secs(10)),
        "dispatch not invoked"
    );

    // Close explicitly; Drop will free.
    actor.close();
    // TestActor::Drop runs hew_actor_free; success is implied if the test
    // does not deadlock or abort.
}

// ═══════════════════════════════════════════════════════════════════════
// 4. Mailbox FIFO message ordering
// ═══════════════════════════════════════════════════════════════════════

/// Send multiple messages to a raw mailbox and verify FIFO dequeue order.
#[test]
fn mailbox_fifo_ordering() {
    let mb = TestMailbox::new();

    // Enqueue messages with distinct msg_type tags 0..5.
    for i in 0..5i32 {
        let mut val = i * 10;
        mb.send(i, &mut val);
    }

    assert_eq!(mb.len(), 5, "mailbox should have 5 messages");

    // Dequeue and verify ordering.
    for expected_type in 0..5i32 {
        let node = mb
            .try_recv()
            .unwrap_or_else(|| panic!("expected message {expected_type} but mailbox was empty"));
        assert_eq!(
            node.msg_type(),
            expected_type,
            "messages must be dequeued in FIFO order"
        );
        // SAFETY: payload was sent as `i32` of size 4; node.data is the
        // runtime-allocated deep-copy of the original send.
        let payload: i32 = unsafe { node.payload() };
        assert_eq!(
            payload,
            expected_type * 10,
            "payload must match the sent value"
        );
    }

    // Mailbox should be empty now.
    assert!(!mb.has_messages(), "mailbox should be empty after draining");
}

/// Interleave sends and receives to verify partial-drain FIFO behaviour.
#[test]
fn mailbox_interleaved_send_recv_preserves_order() {
    let mb = TestMailbox::new();

    // Send two messages.
    let mut v0: i32 = 100;
    let mut v1: i32 = 200;
    mb.send(0, &mut v0);
    mb.send(1, &mut v1);

    // Receive first — should be v0.
    let node0 = mb.try_recv().expect("first recv");
    assert_eq!(node0.msg_type(), 0);
    // SAFETY: payload is i32 by construction.
    assert_eq!(unsafe { node0.payload::<i32>() }, 100);
    drop(node0);

    // Send another before receiving the second.
    let mut v2: i32 = 300;
    mb.send(2, &mut v2);

    // Receive second — should be v1 (FIFO).
    let node1 = mb.try_recv().expect("second recv");
    assert_eq!(node1.msg_type(), 1);
    // SAFETY: payload is i32 by construction.
    assert_eq!(unsafe { node1.payload::<i32>() }, 200);
    drop(node1);

    // Receive third — should be v2.
    let node2 = mb.try_recv().expect("third recv");
    assert_eq!(node2.msg_type(), 2);
    // SAFETY: payload is i32 by construction.
    assert_eq!(unsafe { node2.payload::<i32>() }, 300);
}

/// Verify that `try_recv` returns null on an empty mailbox.
#[test]
fn mailbox_try_recv_empty_returns_null() {
    let mb = TestMailbox::new();
    assert!(
        mb.try_recv().is_none(),
        "try_recv on empty mailbox must return None"
    );
}

// ═══════════════════════════════════════════════════════════════════════
// 5. Actor ask / reply pattern
// ═══════════════════════════════════════════════════════════════════════

/// Dispatch function that echoes the payload doubled via the reply channel.
///
/// The reply channel is retrieved from the scheduler's thread-local
/// (set from `HewMsgNode.reply_channel` before dispatch).
unsafe extern "C-unwind" fn echo_double_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
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
        let _ = reply_channel::hew_reply(ch.cast(), (&raw mut doubled).cast(), size_of::<i32>());
    }
}

/// Ask an actor to double a value and verify the reply.
#[test]
fn actor_ask_reply_roundtrip() {
    ensure_scheduler();

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, echo_double_dispatch);

    let mut val: i32 = 21;
    let reply = actor.ask(1, &mut val);

    assert!(!reply.is_null(), "ask should receive a non-null reply");
    // SAFETY: hew_actor_ask returns libc::malloc'd i32 payload from echo_double_dispatch.
    let result: i32 = unsafe { *(reply.cast::<i32>()) };
    assert_eq!(result, 42, "echo_double should return 21 * 2 = 42");

    // SAFETY: reply was malloc'd by the runtime; libc::free is the documented destructor.
    unsafe { libc::free(reply) };
}

/// Ask with a timeout — the echo actor should reply well within the limit.
#[test]
fn actor_ask_with_timeout_succeeds() {
    ensure_scheduler();

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, echo_double_dispatch);

    let mut val: i32 = 5;
    let reply = actor.ask_timeout(1, &mut val, 5000); // 5s — generous

    assert!(
        !reply.is_null(),
        "ask_timeout should succeed within 5 seconds"
    );
    // SAFETY: payload is i32.
    let result: i32 = unsafe { *(reply.cast::<i32>()) };
    assert_eq!(result, 10, "echo_double should return 5 * 2 = 10");

    // SAFETY: reply was malloc'd by the runtime.
    unsafe { libc::free(reply) };
}

/// Ask on a closed actor should return null (send failure).
#[test]
fn actor_ask_closed_returns_null() {
    ensure_scheduler();

    let actor = TestActor::spawn(noop_dispatch);
    actor.close();

    let mut val: i32 = 0;
    let reply = actor.ask(1, &mut val);
    assert!(reply.is_null(), "ask on a closed actor should return null");
}

// ═══════════════════════════════════════════════════════════════════════
// 5b. Ask / reply edge cases
// ═══════════════════════════════════════════════════════════════════════

/// Ask on a stopped (not just closed) actor should return null without deadlocking.
#[test]
fn ask_stopped_actor_returns_null() {
    ensure_scheduler();

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, echo_double_dispatch);

    // Stop the actor (closes mailbox + enqueues sys message).
    actor.stop();

    assert!(
        actor.wait_for_state(HewActorState::Stopped, Duration::from_secs(5)),
        "actor should reach Stopped before ask"
    );

    // Ask should fail immediately (mailbox is closed) and return null.
    let mut val: i32 = 7;
    let reply = actor.ask(1, &mut val);
    assert!(
        reply.is_null(),
        "ask on a stopped actor must return null, not deadlock"
    );
}

/// Send an ask via `hew_actor_ask_with_channel`, then stop the actor before
/// waiting on the reply. The orphaned reply channel should unblock with null.
#[test]
fn ask_freed_queued_messages_unblock_caller() {
    ensure_scheduler();

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, echo_double_dispatch);

    // Create a reply channel and send an ask (but don't wait yet).
    let ch = hew_runtime::reply_channel::hew_reply_channel_new();
    assert!(!ch.is_null());

    let mut val: i32 = 99;
    let send_rc = actor.ask_with_channel(1, &mut val, ch);

    if send_rc == 0 {
        // Message was enqueued — now stop the actor so it won't
        // process the message. The queued node should be freed,
        // which should signal the reply channel.
        actor.stop();

        // Wait on the reply channel with a short timeout. If orphan
        // handling is broken, this will block for the full timeout.
        let start = std::time::Instant::now();
        // SAFETY: ch is a valid reply channel; hew_reply_wait_timeout is
        // documented to return null and unblock on orphaned channels.
        let reply = unsafe { hew_runtime::reply_channel::hew_reply_wait_timeout(ch, 200) };
        let elapsed = start.elapsed();

        // The reply may be null (orphaned — actor stopped before
        // dispatching) or non-null (actor dispatched before stopping).
        // Both are correct; the test verifies that the caller is
        // UNBLOCKED promptly, not that the reply is always null.
        assert!(
            elapsed.as_millis() < 200,
            "reply channel should be unblocked promptly, took {}ms",
            elapsed.as_millis()
        );
        if !reply.is_null() {
            // SAFETY: reply was malloc'd by hew_reply.
            unsafe { libc::free(reply) };
        }
    }
    // else: send failed — hew_actor_ask_with_channel retained then
    // released its reference, so refs is back to 1 (our initial
    // reference from hew_reply_channel_new).

    // SAFETY: Release our reference to the reply channel.
    unsafe { hew_runtime::reply_channel::hew_reply_channel_free(ch) };
}

/// Dispatch function that sleeps for 500ms before replying.
///
/// Used to test ask timeout behaviour — the caller should time out
/// well before this function finishes sleeping. The sleep must be
/// well under `hew_actor_free`'s 2s deadline so the actor can be
/// freed after the test.
unsafe extern "C-unwind" fn slow_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
) {
    std::thread::sleep(Duration::from_millis(500));

    // SAFETY: Read the reply channel from the scheduler thread-local and
    // reply so the channel's sender-side reference is properly released.
    unsafe {
        let ch = hew_runtime::scheduler::hew_get_reply_channel();
        if !ch.is_null() && !data.is_null() && data_size >= size_of::<i32>() {
            let payload = *(data.cast::<i32>());
            let mut result = payload;
            let _ = reply_channel::hew_reply(ch.cast(), (&raw mut result).cast(), size_of::<i32>());
        }
    }
}

/// Ask with a short timeout against a slow actor — should return null
/// when the timeout expires before the dispatch function replies.
#[test]
fn ask_timeout_returns_null() {
    ensure_scheduler();

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, slow_dispatch);

    let mut val: i32 = 42;
    let reply = actor.ask_timeout(1, &mut val, 100); // 100ms — well under 500ms sleep
    assert!(
        reply.is_null(),
        "ask_timeout should return null when the actor is too slow"
    );

    // The slow dispatch is still running on a scheduler thread.
    // Drop runs close + free; free waits for the actor to finish.
    actor.close();
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

unsafe extern "C-unwind" fn order_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
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

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, order_dispatch);

    for i in 0..N {
        let mut val = i;
        actor.send(i, &mut val);
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
}

// ═══════════════════════════════════════════════════════════════════════
// 7. Actor state transitions under dispatch
// ═══════════════════════════════════════════════════════════════════════

static STATE_SIGNAL: DispatchLog = DispatchLog::new();
static STATE_LOCK: Mutex<()> = Mutex::new(());

/// Recorded actor state observed *during* dispatch (should be Running).
static OBSERVED_STATE: AtomicI32 = AtomicI32::new(-1);

unsafe extern "C-unwind" fn state_observing_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
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

    let mut state: i32 = 0;
    let actor = TestActor::spawn_with_state(&mut state, state_observing_dispatch);

    actor.send_empty(1);
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
}

// ═══════════════════════════════════════════════════════════════════════
// 8. Mailbox deep-copy isolation
// ═══════════════════════════════════════════════════════════════════════

/// Verify that mailbox messages are deep-copied: mutating the original
/// buffer after send does not affect the enqueued message.
#[test]
fn mailbox_deep_copy_isolates_sender_and_receiver() {
    let mb = TestMailbox::new();
    let mut val: i32 = 42;

    mb.send(0, &mut val);

    // Mutate the original after send.
    val = 999;

    let node = mb.try_recv().expect("recv after send");
    // SAFETY: payload is i32 by construction.
    let received: i32 = unsafe { node.payload() };
    assert_eq!(
        received, 42,
        "message should contain the value at send time, not the mutated value"
    );

    let _ = val; // suppress unused warning
}

// ═══════════════════════════════════════════════════════════════════════
// 9. Budget and priority configuration
// ═══════════════════════════════════════════════════════════════════════

/// Verify that budget and priority setters/getters round-trip correctly.
#[test]
fn actor_budget_and_priority_roundtrip() {
    let actor = TestActor::spawn(noop_dispatch);

    // Set custom budget.
    actor.set_budget(128);
    assert_eq!(actor.budget(), 128, "budget should round-trip");

    // Set priority.
    actor.set_priority(2); // low
    assert_eq!(actor.priority(), 2, "priority should round-trip");

    // Reset budget to default (pass 0).
    actor.set_budget(0);
    assert_eq!(
        actor.budget(),
        hew_runtime::actor::HEW_MSG_BUDGET as u32,
        "budget of 0 should reset to default"
    );
}

// ═══════════════════════════════════════════════════════════════════════
// 10. Actor error / trap
// ═══════════════════════════════════════════════════════════════════════

/// `hew_actor_trap` sets the error code and transitions to Crashed.
#[test]
fn actor_trap_sets_error_and_crashes() {
    let actor = TestActor::spawn(noop_dispatch);

    // No error initially.
    assert_eq!(actor.error(), 0, "new actor should have error code 0");

    // Trap with error code 42.
    actor.trap(42);

    assert_eq!(actor.error(), 42, "trap should set the error code");
    assert_eq!(
        actor.state_raw(),
        HewActorState::Crashed as i32,
        "trap should transition actor to Crashed"
    );
}

// Suppress unused-import warnings: `ptr` and `c_void` remain referenced by
// dispatch fn signatures; HewError is used; ensure_scheduler is used; the
// rest are funnelled through the testkit wrappers.
#[allow(
    dead_code,
    reason = "ptr/c_void are used inside extern \"C\" dispatch fns"
)]
fn _unused_imports_keepalive() {
    let _ = ptr::null::<c_void>();
}
