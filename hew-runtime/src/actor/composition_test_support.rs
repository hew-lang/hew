//! Worker-free actor teardown substrate for cross-crate composition tests.
//!
//! This module is feature-gated because it constructs an idle actor without
//! publishing it to the live registry or scheduler. That gives codegen tests a
//! deterministic place to install an actual generated message destructor,
//! queue an ask, and let `Idle -> Stopped` retire it before dispatch.
use super::*;

const WAITER_TIMEOUT_MS: i32 = 5_000;

/// Observable results from one terminal ask teardown.
#[derive(Debug, PartialEq, Eq)]
pub struct TerminalAskPayloadReport {
    /// Mailbox submission result (`HewError::Ok` on success).
    pub send_result: i32,
    /// Actor state after the stop request.
    pub actor_state: i32,
    /// Whether the bounded wait returned the null failure sentinel.
    pub wait_returned_null: bool,
    /// Reply failure classification observed while the creator ref is live.
    pub failure_kind: i32,
    /// Payload-drop observations after queueing and before terminalization.
    pub payload_drops_before_stop: usize,
    /// User-message count before terminalization.
    pub queued_before_stop: usize,
    /// User-message count after terminal reclaim.
    pub queued_after_stop: usize,
}

unsafe extern "C-unwind" fn dispatch_must_not_run(
    _ctx: *mut HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    panic!("composition oracle dispatched a message before terminalization");
}

fn idle_actor(mailbox: *mut HewMailbox) -> *mut HewActor {
    Box::into_raw(Box::new(HewActor {
        dispatch_ownership: crate::actor::HewDispatchOwnership::CopiedPayload,
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: u64::MAX - 2_848,
        state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(dispatch_must_not_run),
        mailbox: mailbox.cast(),
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
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
        cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
        pending_wake: AtomicBool::new(false),
        suspended_reply_channel: AtomicPtr::new(ptr::null_mut()),
        suspended_cancel_token: AtomicPtr::new(ptr::null_mut()),
        runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
        runtime: ptr::null(),
        send_pin_count: AtomicU32::new(0),
        gen_sink: AtomicPtr::new(ptr::null_mut()),
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        spawn_serial: u64::MAX - 2_848,
        sys_dispatch: None,
        state_drop_consumed: AtomicBool::new(false),
        state_drop_borrowed: AtomicBool::new(false),
        parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
        checked_invocation: AtomicPtr::new(std::ptr::null_mut()),
        pending_external_trap_code: AtomicI32::new(0),
        native_completion: None,
    }))
}

/// Queue one ask carrying `payload`, stop its actor before dispatch, and
/// report the waiter and terminal-reclaim observations.
///
/// `message_drop_fn` is installed through the production actor API. On a
/// successful send, ownership of every heap owner embedded in `payload`
/// transfers to the queued message and is consumed by terminal reclaim.
///
/// # Safety
///
/// - `message_drop_fn` must match `msg_type` and the payload layout.
/// - `payload` must point to `payload_size` readable bytes, or be null when
///   `payload_size == 0`.
/// - On successful submission the caller must not separately destroy any
///   owners embedded in the payload bytes.
/// - `payload_drop_count` must report the callback's drop observations
///   without mutating the actor, mailbox, payload, or reply channel.
///
/// # Panics
///
/// Panics if the test-only mailbox or reply-channel allocation fails.
pub unsafe fn terminalize_queued_ask(
    message_drop_fn: mailbox::HewMessageDropFn,
    msg_type: i32,
    payload: *mut c_void,
    payload_size: usize,
    payload_drop_count: fn() -> usize,
) -> TerminalAskPayloadReport {
    // SAFETY: this feature-gated oracle creates and exclusively owns the
    // mailbox through the matching free below.
    let mailbox = unsafe { mailbox::hew_mailbox_new() };
    assert!(!mailbox.is_null(), "composition oracle mailbox allocation");
    let actor = idle_actor(mailbox);
    let channel = reply_channel::hew_reply_channel_new();
    assert!(
        !channel.is_null(),
        "composition oracle reply channel allocation"
    );

    // SAFETY: `actor` and `channel` are live and exclusively controlled by
    // this oracle; the retained sender reference is transferred below.
    unsafe {
        hew_actor_set_message_drop(actor, message_drop_fn);
        reply_channel::hew_reply_channel_retain(channel);
    }
    // SAFETY: the caller supplies a valid payload and matching destructor;
    // `mailbox` is live and the retained channel reference becomes the ask
    // node's sender reference on successful submission.
    let send_result = unsafe {
        mailbox::hew_mailbox_send_with_reply(
            mailbox,
            msg_type,
            payload,
            payload_size,
            channel.cast(),
        )
    };
    // SAFETY: `mailbox` remains exclusively owned and live here.
    let queued_before_stop = unsafe { mailbox::hew_mailbox_len(mailbox) };
    let payload_drops_before_stop = payload_drop_count();

    // SAFETY: `actor` is the live, exclusively owned test actor.
    unsafe { hew_actor_stop(actor) };

    // SAFETY: `actor` is still live until the teardown below.
    let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
    // SAFETY: the creator reference keeps `channel` live. A timeout fails
    // the oracle closed instead of hanging the entire test job.
    let wait_result = unsafe { reply_channel::hew_reply_wait_timeout(channel, WAITER_TIMEOUT_MS) };
    // SAFETY: the creator reference remains live until `free` below.
    let failure_kind = unsafe { reply_channel::hew_reply_channel_failure_kind(channel) };
    // SAFETY: `mailbox` remains live until the teardown below.
    let queued_after_stop = unsafe { mailbox::hew_mailbox_len(mailbox) };
    // SAFETY: release the creator reference after all observations.
    unsafe { reply_channel::hew_reply_channel_free(channel) };

    // SAFETY: the actor and mailbox were created above, never published,
    // and terminal reclaim has detached their queued node.
    unsafe {
        drop(Box::from_raw(actor));
        mailbox::hew_mailbox_free(mailbox);
    }

    TerminalAskPayloadReport {
        send_result,
        actor_state,
        wait_returned_null: wait_result.is_null(),
        failure_kind,
        payload_drops_before_stop,
        queued_before_stop,
        queued_after_stop,
    }
}
