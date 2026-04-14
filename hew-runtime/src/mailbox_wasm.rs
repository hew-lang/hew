//! WASM mailbox: single-threaded actor message passing using [`VecDeque`].
//!
//! This is the WASM counterpart of [`crate::mailbox`]. On native targets the
//! mailbox uses lock-free MPSC queues, atomics, and condvars for multi-threaded
//! actor scheduling. WASM is single-threaded, so we replace all of that with
//! plain `VecDeque` queues and scalar counters.
//!
//! The C ABI surface is identical to the native mailbox so that
//! codegen-emitted calls resolve transparently on both targets. The
//! `#[no_mangle]` attribute is only applied on `wasm32` to avoid symbol
//! conflicts with the native mailbox when running tests on the host.

#[cfg(test)]
use std::cell::Cell;
use std::collections::VecDeque;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::AtomicPtr;

use crate::internal::types::{HewError, HewOverflowPolicy};
use crate::set_last_error;

/// Key extractor used by coalescing mailboxes.
pub type HewCoalesceKeyFn = unsafe extern "C" fn(i32, *mut c_void, usize) -> u64;

// ── Conditional no_mangle ───────────────────────────────────────────────
//
// On wasm32 these functions replace the native mailbox symbols 1-for-1.
// On native builds (where tests run) we omit `#[no_mangle]` so they
// don't collide with the real mailbox symbols from `mailbox.rs`.

macro_rules! wasm_no_mangle {
    (
        $(#[$meta:meta])*
        pub unsafe extern "C" fn $name:ident( $($args:tt)* ) $( -> $ret:ty )? $body:block
    ) => {
        #[cfg_attr(target_arch = "wasm32", no_mangle)]
        $(#[$meta])*
        pub unsafe extern "C" fn $name( $($args)* ) $( -> $ret )? $body
    };
}

#[cfg(test)]
thread_local! {
    static FAIL_MAILBOX_ALLOC_ON_NTH: Cell<usize> = const { Cell::new(usize::MAX) };
}

#[cfg(test)]
struct MailboxAllocFailureGuard;

#[cfg(test)]
impl Drop for MailboxAllocFailureGuard {
    fn drop(&mut self) {
        FAIL_MAILBOX_ALLOC_ON_NTH.with(|slot| slot.set(usize::MAX));
    }
}

#[cfg(test)]
fn fail_mailbox_alloc_on_nth(n: usize) -> MailboxAllocFailureGuard {
    FAIL_MAILBOX_ALLOC_ON_NTH.with(|slot| slot.set(n));
    MailboxAllocFailureGuard
}

#[cfg(test)]
fn should_fail_mailbox_alloc() -> bool {
    FAIL_MAILBOX_ALLOC_ON_NTH.with(|slot| {
        let remaining = slot.get();
        if remaining == usize::MAX {
            return false;
        }
        if remaining == 0 {
            slot.set(usize::MAX);
            return true;
        }
        slot.set(remaining - 1);
        false
    })
}

fn mailbox_malloc(size: usize) -> *mut c_void {
    #[cfg(test)]
    {
        if should_fail_mailbox_alloc() {
            return ptr::null_mut();
        }
    }

    // SAFETY: `size` is forwarded to libc unchanged.
    unsafe { libc::malloc(size) }
}

fn reserve_queue_capacity<T>(queue: &mut VecDeque<T>, additional: usize) -> bool {
    if queue.capacity().saturating_sub(queue.len()) >= additional {
        return true;
    }

    #[cfg(test)]
    {
        if should_fail_mailbox_alloc() {
            return false;
        }
    }

    queue.try_reserve(additional).is_ok()
}

fn report_sys_enqueue_failure(msg_type: i32, size: usize) {
    let msg = format!(
        "hew_mailbox_send_sys: failed to deliver system message (msg_type={msg_type}, size={size})"
    );
    set_last_error(msg.clone());
    eprintln!("{msg}");
}

fn report_stop_enqueue_failure() {
    let msg = "hew_actor_stop: failed to enqueue shutdown system message";
    set_last_error(msg);
    eprintln!("{msg}");
}

// ── Message node ────────────────────────────────────────────────────────
//
// We duplicate the struct here rather than importing from `crate::mailbox`
// because that module is `#[cfg(not(target_arch = "wasm32"))]` and does
// not exist on WASM targets.
//
// The shared prefix fields (next … reply_channel) have identical offsets to
// the native struct for C ABI compat.  The native struct appends one extra
// tail field, `trace_context: HewTraceContext`, that WASM intentionally
// omits (tracing infrastructure is not used on the single-threaded WASM
// scheduler).  This struct is therefore a strict prefix of the native layout;
// the compile-time assertions below enforce that the prefix never drifts.

/// A single message in a mailbox queue.
///
/// Allocated with [`libc::malloc`] and freed by [`msg_node_free`].
/// The `next` field is unused on WASM (queues are `VecDeque`-backed)
/// but kept for struct layout compatibility with the native mailbox.
///
/// The native struct appends a `trace_context` tail field that WASM omits;
/// this struct is a strict prefix of the native layout.
#[repr(C)]
#[derive(Debug)]
pub struct HewMsgNode {
    /// Intrusive MPSC next-pointer (unused on WASM, kept for layout compat).
    pub next: AtomicPtr<HewMsgNode>,
    /// Application-defined message type tag.
    pub msg_type: i32,
    /// Pointer to deep-copied message payload (malloc'd).
    pub data: *mut c_void,
    /// Size of `data` in bytes.
    pub data_size: usize,
    /// Optional reply channel for the ask pattern (unused by mailbox).
    pub reply_channel: *mut c_void,
}

// Compile-time check: this WASM HewMsgNode must have identical alignment
// and field offsets (for the shared prefix fields) to the canonical native
// definition in `crate::mailbox`.
//
// The native struct appends `trace_context` after `reply_channel`; the WASM
// struct is intentionally a strict prefix, so we check per-field offsets and
// alignment rather than size equality.
//
// Gated to `not(target_arch = "wasm32")` because `crate::mailbox` is only
// compiled on native targets; this block therefore runs during `cargo test`
// where both modules exist simultaneously.
#[cfg(not(target_arch = "wasm32"))]
const _: () = {
    use std::mem::offset_of;
    type W = HewMsgNode;
    type N = crate::mailbox::HewMsgNode;

    assert!(
        align_of::<W>() == align_of::<N>(),
        "WASM HewMsgNode alignment diverged from native (mailbox_wasm)"
    );
    assert!(
        size_of::<W>() <= size_of::<N>(),
        "WASM HewMsgNode grew larger than native — layout diverged (mailbox_wasm)"
    );

    assert!(offset_of!(W, next) == offset_of!(N, next));
    assert!(offset_of!(W, msg_type) == offset_of!(N, msg_type));
    assert!(offset_of!(W, data) == offset_of!(N, data));
    assert!(offset_of!(W, data_size) == offset_of!(N, data_size));
    assert!(offset_of!(W, reply_channel) == offset_of!(N, reply_channel));
};

// ── Message node helpers ────────────────────────────────────────────────

/// Allocate a [`HewMsgNode`] via `libc::malloc`, deep-copying `data`.
///
/// # Safety
///
/// `data` must point to at least `data_size` readable bytes, or be null
/// when `data_size` is 0.
///
/// Returns null on OOM.
unsafe fn msg_node_alloc(msg_type: i32, data: *const c_void, data_size: usize) -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) -- POD-like struct, no drop glue.
    let node = mailbox_malloc(size_of::<HewMsgNode>()).cast::<HewMsgNode>();
    if node.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = msg_type;
        (*node).data_size = data_size;
        (*node).reply_channel = ptr::null_mut();

        // Deep-copy message data for actor isolation.
        if data_size > 0 && !data.is_null() {
            let buf = mailbox_malloc(data_size);
            if buf.is_null() {
                libc::free(node.cast());
                return ptr::null_mut();
            }
            libc::memcpy(buf, data, data_size);
            (*node).data = buf;
        } else {
            (*node).data = ptr::null_mut();
        }
    }

    node
}

/// Free a [`HewMsgNode`] and its payload.
///
/// If the node still owns an ask reply channel, it is completed with an
/// empty reply before the node is freed so the waiter is not left hanging.
///
/// # Safety
///
/// `node` must have been allocated by [`msg_node_alloc`] (or
/// [`libc::malloc`] with the same layout) and must not be used after
/// this call.
unsafe fn msg_node_free(node: *mut HewMsgNode) {
    if node.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `node` was malloc'd and is exclusively owned.
    unsafe {
        // If a reply channel was set (ask pattern) but the message was never
        // replied to, deposit an empty reply so the waiting side observes the
        // orphaned ask and releases the sender-side reference.
        if !(*node).reply_channel.is_null() {
            retire_reply_channel((*node).reply_channel);
            (*node).reply_channel = ptr::null_mut();
        }
        libc::free((*node).data);
        libc::free(node.cast());
    }
}

wasm_no_mangle! {
    /// C ABI entry point for freeing a message node on WASM.
    ///
    /// # Safety
    ///
    /// Same requirements as [`msg_node_free`].
    pub unsafe extern "C" fn hew_msg_node_free(node: *mut HewMsgNode) {
        // SAFETY: Caller guarantees `node` was malloc'd.
        unsafe { msg_node_free(node) };
    }
}

// ── WASM Mailbox ────────────────────────────────────────────────────────

/// Single-threaded actor mailbox for WASM targets.
///
/// Uses plain [`VecDeque`] queues and scalar counters instead of the
/// lock-free MPSC queues and atomics used by the native mailbox.
#[derive(Debug)]
pub struct HewMailboxWasm {
    /// User message queue.
    user_queue: VecDeque<*mut HewMsgNode>,
    /// System message queue (always unbounded, priority over user queue).
    sys_queue: VecDeque<*mut HewMsgNode>,
    /// Current number of user messages.
    count: i64,
    /// Maximum user-queue capacity (`-1` = unbounded).
    capacity: i64,
    /// Policy applied when user-queue is at capacity.
    overflow: HewOverflowPolicy,
    /// Optional key extractor used by [`HewOverflowPolicy::Coalesce`].
    coalesce_key_fn: Option<HewCoalesceKeyFn>,
    /// Fallback policy used when coalesce finds no matching key.
    coalesce_fallback: HewOverflowPolicy,
    /// High-water mark: maximum `count` value observed.
    high_water_mark: i64,
    /// Whether the mailbox has been closed.
    closed: bool,
    /// Whether a shutdown system message (`msg_type = -1`) has been enqueued.
    #[cfg_attr(
        not(target_arch = "wasm32"),
        allow(dead_code, reason = "field is only used by wasm-only stop semantics")
    )]
    stop_signal_sent: bool,
}

/// Update the high-water mark after incrementing `count`.
fn update_high_water_mark(mb: &mut HewMailboxWasm) {
    if mb.count > mb.high_water_mark {
        mb.high_water_mark = mb.count;
    }
}

fn normalize_coalesce_fallback(policy: HewOverflowPolicy) -> HewOverflowPolicy {
    match policy {
        HewOverflowPolicy::Coalesce => HewOverflowPolicy::DropOld,
        other => other,
    }
}

unsafe fn coalesce_message_key(
    key_fn: Option<HewCoalesceKeyFn>,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) -> u64 {
    if let Some(key_fn) = key_fn {
        // SAFETY: caller guarantees key function and payload pointers are valid.
        unsafe { key_fn(msg_type, data, data_size) }
    } else {
        #[expect(
            clippy::cast_sign_loss,
            reason = "bit-pattern-preserving cast is fine for fallback msg_type keying"
        )]
        {
            msg_type as u64
        }
    }
}

unsafe fn replace_node_payload(
    node: *mut HewMsgNode,
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
) -> bool {
    // SAFETY: `node` is a valid queued node exclusively owned by the mailbox.
    unsafe {
        let mut new_buf: *mut c_void = ptr::null_mut();
        if data_size > 0 && !data.is_null() {
            new_buf = mailbox_malloc(data_size);
            if new_buf.is_null() {
                return false;
            }
            libc::memcpy(new_buf, data, data_size);
        }

        libc::free((*node).data);
        (*node).data = new_buf;
        (*node).msg_type = msg_type;
        (*node).data_size = data_size;
    }
    true
}

unsafe fn retire_reply_channel(reply_channel: *mut c_void) {
    if reply_channel.is_null() {
        return;
    }
    // mailbox_wasm reply channels are always WASM-style channels — both in WASM
    // production builds and in non-WASM test builds that exercise this module.
    // SAFETY: `reply_channel` is a live WASM reply channel owned by the mailbox
    // and we are retiring it with an empty reply.
    //
    // Mark the channel as orphaned BEFORE depositing the null sentinel so the
    // ask waiter can distinguish mailbox-teardown null from a legitimate null
    // reply deposited by the handler.  The flag is read after `reply_ready`
    // becomes true, so the ordering is: set orphaned → call hew_reply (sets
    // replied=true) → waiter sees replied=true → waiter reads orphaned=true.
    unsafe {
        (*reply_channel.cast::<crate::reply_channel_wasm::WasmReplyChannel>()).orphaned = true;
        crate::reply_channel_wasm::hew_reply(reply_channel.cast(), ptr::null_mut(), 0);
    }
}

/// Outcome of an overflow-policy-aware send into the user queue.
///
/// Mirrors the native `SendOutcome` in `mailbox.rs`. FFI entry points map
/// these variants to their own return conventions.
#[derive(Clone, Copy)]
enum SendOutcome {
    /// Message was successfully enqueued.
    Enqueued,
    /// Mailbox is closed — message was not sent.
    Closed,
    /// Message intentionally dropped (`DropNew` policy).
    Dropped,
    /// Overflow policy is `Fail` — operation rejected.
    Failed,
    /// Oldest message was evicted to make room (`DropOld` policy).
    DroppedOld,
    /// Message payload was merged with an existing queued message
    /// (`Coalesce` policy).
    Coalesced,
    /// Memory allocation failed.
    Oom,
}

#[expect(
    clippy::too_many_lines,
    reason = "overflow-policy dispatch is inherently complex — splitting further would scatter the state machine"
)]
unsafe fn send_user_message_inner(
    mb: &mut HewMailboxWasm,
    msg_type: i32,
    data: *const c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> SendOutcome {
    if mb.closed {
        return SendOutcome::Closed;
    }

    if mb.capacity > 0 && mb.count >= mb.capacity {
        match mb.overflow {
            HewOverflowPolicy::Block | HewOverflowPolicy::DropNew => {
                return SendOutcome::Dropped;
            }
            HewOverflowPolicy::Fail => {
                return SendOutcome::Failed;
            }
            HewOverflowPolicy::DropOld => {
                // Allocate BEFORE evicting so OOM leaves the queue unchanged.
                // SAFETY: `data` validity guaranteed by caller.
                let node = unsafe { msg_node_alloc(msg_type, data, size) };
                if node.is_null() {
                    return SendOutcome::Oom;
                }
                if !reserve_queue_capacity(&mut mb.user_queue, 1) {
                    // SAFETY: `node` is still exclusively owned.
                    unsafe { msg_node_free(node) };
                    return SendOutcome::Oom;
                }
                // Allocation succeeded — now safe to evict the oldest.
                if let Some(old) = mb.user_queue.pop_front() {
                    // SAFETY: node was allocated by msg_node_alloc.
                    unsafe { msg_node_free(old) };
                    mb.count -= 1;
                }
                // SAFETY: node was just allocated and is exclusively owned.
                unsafe { (*node).reply_channel = reply_channel };
                mb.user_queue.push_back(node);
                mb.count += 1;
                update_high_water_mark(mb);
                return SendOutcome::DroppedOld;
            }
            HewOverflowPolicy::Coalesce => {
                // SAFETY: `data` validity guaranteed by caller.
                let incoming_key = unsafe {
                    coalesce_message_key(mb.coalesce_key_fn, msg_type, data.cast_mut(), size)
                };
                let found = mb
                    .user_queue
                    .iter()
                    .find(|&&node| {
                        // SAFETY: all queued nodes were allocated by msg_node_alloc.
                        unsafe {
                            coalesce_message_key(
                                mb.coalesce_key_fn,
                                (*node).msg_type,
                                (*node).data,
                                (*node).data_size,
                            ) == incoming_key
                        }
                    })
                    .copied();
                if let Some(existing) = found {
                    // SAFETY: `existing` points at a queued node still owned by
                    // `mb`, so reading its reply-channel field is valid.
                    let existing_reply_channel = unsafe { (*existing).reply_channel };
                    // SAFETY: `existing` remains owned by the mailbox queue.
                    let ok = unsafe { replace_node_payload(existing, msg_type, data, size) };
                    if !ok {
                        return SendOutcome::Oom;
                    }
                    // Preserve the queued node's reply_channel to mirror the
                    // native mailbox contract for ask-style coalescing, but
                    // retire any superseded incoming waiter immediately.
                    if !reply_channel.is_null() && reply_channel != existing_reply_channel {
                        // SAFETY: the incoming ask waiter is no longer queued.
                        unsafe { retire_reply_channel(reply_channel) };
                    }
                    return SendOutcome::Coalesced;
                }

                match normalize_coalesce_fallback(mb.coalesce_fallback) {
                    HewOverflowPolicy::Block | HewOverflowPolicy::DropNew => {
                        return SendOutcome::Dropped;
                    }
                    HewOverflowPolicy::Fail => {
                        return SendOutcome::Failed;
                    }
                    HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce => {
                        // Allocate BEFORE evicting so OOM leaves the queue unchanged.
                        // SAFETY: `data` validity guaranteed by caller.
                        let node = unsafe { msg_node_alloc(msg_type, data, size) };
                        if node.is_null() {
                            return SendOutcome::Oom;
                        }
                        if !reserve_queue_capacity(&mut mb.user_queue, 1) {
                            // SAFETY: `node` is still exclusively owned.
                            unsafe { msg_node_free(node) };
                            return SendOutcome::Oom;
                        }
                        // Allocation succeeded — now safe to evict the oldest.
                        if let Some(old) = mb.user_queue.pop_front() {
                            // SAFETY: node was allocated by msg_node_alloc.
                            unsafe { msg_node_free(old) };
                            mb.count -= 1;
                        }
                        // SAFETY: node was just allocated and is exclusively owned.
                        unsafe { (*node).reply_channel = reply_channel };
                        mb.user_queue.push_back(node);
                        mb.count += 1;
                        update_high_water_mark(mb);
                        return SendOutcome::DroppedOld;
                    }
                }
            }
        }
    }

    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, size) };
    if node.is_null() {
        return SendOutcome::Oom;
    }
    if !reserve_queue_capacity(&mut mb.user_queue, 1) {
        // SAFETY: `node` is still exclusively owned by this send path and has
        // no reply channel attached yet.
        unsafe { msg_node_free(node) };
        return SendOutcome::Oom;
    }
    // SAFETY: node was just allocated and is exclusively owned.
    unsafe { (*node).reply_channel = reply_channel };
    mb.user_queue.push_back(node);
    mb.count += 1;
    update_high_water_mark(mb);

    SendOutcome::Enqueued
}

/// Map [`SendOutcome`] to the `HewError`-based return convention used by
/// `hew_mailbox_send` / `hew_mailbox_try_send` / `hew_mailbox_send_with_reply`.
fn send_outcome_to_hew_error(outcome: SendOutcome) -> i32 {
    match outcome {
        SendOutcome::Enqueued | SendOutcome::Coalesced | SendOutcome::DroppedOld => {
            HewError::Ok as i32
        }
        SendOutcome::Closed => HewError::ErrClosed as i32,
        SendOutcome::Dropped | SendOutcome::Failed => HewError::ErrMailboxFull as i32,
        SendOutcome::Oom => HewError::ErrOom as i32,
    }
}

unsafe fn send_user_message(
    mb: &mut HewMailboxWasm,
    msg_type: i32,
    data: *const c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> i32 {
    // SAFETY: caller guarantees all pointer invariants.
    let outcome = unsafe { send_user_message_inner(mb, msg_type, data, size, reply_channel) };
    if matches!(outcome, SendOutcome::Enqueued | SendOutcome::DroppedOld) {
        crate::scheduler_wasm::record_message_sent();
    }
    send_outcome_to_hew_error(outcome)
}

// ── Constructors ────────────────────────────────────────────────────────

wasm_no_mangle! {
    /// Create an unbounded WASM mailbox.
    ///
    /// # Safety
    ///
    /// Returned pointer must be freed with [`hew_mailbox_free`].
    pub unsafe extern "C" fn hew_mailbox_new() -> *mut HewMailboxWasm {
        Box::into_raw(Box::new(HewMailboxWasm {
            user_queue: VecDeque::new(),
            sys_queue: VecDeque::new(),
            count: 0,
            capacity: -1,
            overflow: HewOverflowPolicy::DropNew,
            coalesce_key_fn: None,
            coalesce_fallback: HewOverflowPolicy::DropOld,
            high_water_mark: 0,
            closed: false,
            stop_signal_sent: false,
        }))
    }
}

wasm_no_mangle! {
    /// Create a bounded WASM mailbox with the given capacity.
    ///
    /// # Safety
    ///
    /// Returned pointer must be freed with [`hew_mailbox_free`].
    pub unsafe extern "C" fn hew_mailbox_new_bounded(capacity: i32) -> *mut HewMailboxWasm {
        Box::into_raw(Box::new(HewMailboxWasm {
            user_queue: VecDeque::new(),
            sys_queue: VecDeque::new(),
            count: 0,
            capacity: i64::from(capacity),
            overflow: HewOverflowPolicy::DropNew,
            coalesce_key_fn: None,
            coalesce_fallback: HewOverflowPolicy::DropOld,
            high_water_mark: 0,
            closed: false,
            stop_signal_sent: false,
        }))
    }
}

wasm_no_mangle! {
    /// Create a bounded WASM mailbox with the given capacity and overflow
    /// policy.
    ///
    /// A `capacity` of `0` creates an unbounded mailbox.
    ///
    /// # Safety
    ///
    /// Returned pointer must be freed with [`hew_mailbox_free`].
    pub unsafe extern "C" fn hew_mailbox_new_with_policy(
        capacity: usize,
        policy: HewOverflowPolicy,
    ) -> *mut HewMailboxWasm {
        let cap = if capacity == 0 {
            -1
        } else {
            i64::try_from(capacity).unwrap_or(i64::MAX)
        };
        Box::into_raw(Box::new(HewMailboxWasm {
            user_queue: VecDeque::new(),
            sys_queue: VecDeque::new(),
            count: 0,
            capacity: cap,
            overflow: policy,
            coalesce_key_fn: None,
            coalesce_fallback: HewOverflowPolicy::DropOld,
            high_water_mark: 0,
            closed: false,
            stop_signal_sent: false,
        }))
    }
}

wasm_no_mangle! {
    /// Create a bounded WASM mailbox using [`HewOverflowPolicy::Coalesce`].
    ///
    /// # Safety
    ///
    /// Returned pointer must be freed with [`hew_mailbox_free`].
    pub unsafe extern "C" fn hew_mailbox_new_coalesce(capacity: u32) -> *mut HewMailboxWasm {
        // SAFETY: constructor forwards trusted parameters to the mailbox factory.
        unsafe { hew_mailbox_new_with_policy(usize::try_from(capacity).unwrap_or(usize::MAX), HewOverflowPolicy::Coalesce) }
    }
}

wasm_no_mangle! {
    /// Configure coalescing behaviour for a mailbox.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_set_coalesce_config(
        mb: *mut HewMailboxWasm,
        key_fn: Option<HewCoalesceKeyFn>,
        fallback_policy: HewOverflowPolicy,
    ) {
        // SAFETY: caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };
        mb.coalesce_key_fn = key_fn;
        mb.coalesce_fallback = normalize_coalesce_fallback(fallback_policy);
    }
}

// ── Send (producer side) ────────────────────────────────────────────────

wasm_no_mangle! {
    /// Send a message to the user queue, deep-copying `data`.
    ///
    /// Returns `0` ([`HewError::Ok`]) on success, `-1`
    /// ([`HewError::ErrMailboxFull`]) if bounded and at capacity (for
    /// `DropNew`/`Fail`/`Block` policies), `-4` ([`HewError::ErrClosed`])
    /// if the mailbox is closed, or `-5` ([`HewError::ErrOom`]) if
    /// allocation fails.
    ///
    /// On WASM, `Block` degrades to `DropNew` because there is no way to
    /// block a single-threaded runtime. `Coalesce` still replaces matching
    /// queued messages and otherwise uses its configured fallback policy.
    ///
    /// # Native / WASM divergence
    ///
    /// On native targets `hew_mailbox_send` (the blocking variant) returns
    /// [`HewError::ErrActorStopped`] (`-2`) when the mailbox is closed,
    /// reflecting the actor-layer semantics.  On WASM this function returns
    /// [`HewError::ErrClosed`] (`-4`) instead, matching the native
    /// `hew_mailbox_try_send` behaviour.  This divergence is intentional:
    /// WASM has no blocking send, so both variants use `ErrClosed`.
    ///
    /// # Safety
    ///
    /// - `mb` must be a valid pointer returned by [`hew_mailbox_new`] or
    ///   [`hew_mailbox_new_bounded`].
    /// - `data` must point to at least `size` readable bytes, or be null
    ///   when `size` is 0.
    pub unsafe extern "C" fn hew_mailbox_send(
        mb: *mut HewMailboxWasm,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) -> i32 {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };
        // SAFETY: Caller guarantees payload validity.
        unsafe { send_user_message(mb, msg_type, data.cast_const(), size, ptr::null_mut()) }
    }
}

wasm_no_mangle! {
    /// Send a message with an associated reply channel.
    ///
    /// Identical to [`hew_mailbox_send`] but sets the `reply_channel`
    /// field on the message node for the ask pattern and returns the same
    /// status codes, including [`HewError::ErrOom`] on allocation failure.
    ///
    /// # Safety
    ///
    /// Same requirements as [`hew_mailbox_send`], plus `reply_channel`
    /// must be a valid reply channel pointer (or null).
    pub unsafe extern "C" fn hew_mailbox_send_with_reply(
        mb: *mut HewMailboxWasm,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
        reply_channel: *mut c_void,
    ) -> i32 {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };
        // SAFETY: Caller guarantees payload validity and reply-channel provenance.
        unsafe { send_user_message(mb, msg_type, data.cast_const(), size, reply_channel) }
    }
}

wasm_no_mangle! {
    /// Non-blocking send -- identical to [`hew_mailbox_send`] on WASM since
    /// there is no blocking distinction in a single-threaded runtime.
    ///
    /// # Safety
    ///
    /// Same requirements as [`hew_mailbox_send`].
    pub unsafe extern "C" fn hew_mailbox_try_send(
        mb: *mut HewMailboxWasm,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) -> i32 {
        // SAFETY: Caller guarantees all pointers are valid.
        unsafe { hew_mailbox_send(mb, msg_type, data, size) }
    }
}

wasm_no_mangle! {
    /// Policy-aware push into the user queue.
    ///
    /// Returns `0` on success, `1` if the message was dropped (`DropNew` policy),
    /// `2` if the oldest message was dropped (`DropOld` policy), `3` if coalesced,
    /// or `-1` on failure (including OOM and closed mailbox).
    ///
    /// This is the WASM counterpart of `mailbox::hew_mailbox_try_push`. It
    /// never carries a reply channel — use [`hew_mailbox_send_with_reply`] for
    /// the ask pattern.
    ///
    /// # Safety
    ///
    /// - `mb` must be a valid mailbox pointer.
    /// - `data` must point to at least `data_size` readable bytes, or be null
    ///   when `data_size` is 0.
    pub unsafe extern "C" fn hew_mailbox_try_push(
        mb: *mut HewMailboxWasm,
        msg_type: i32,
        data: *const c_void,
        data_size: usize,
    ) -> i32 {
        // SAFETY: Caller guarantees `mb` is valid.
        let mbr = unsafe { &mut *mb };
        // SAFETY: Caller guarantees `data` points to `data_size` readable bytes.
        let outcome = unsafe {
            send_user_message_inner(mbr, msg_type, data, data_size, ptr::null_mut())
        };
        if matches!(outcome, SendOutcome::Enqueued | SendOutcome::DroppedOld) {
            crate::scheduler_wasm::record_message_sent();
        }
        match outcome {
            SendOutcome::Enqueued => 0,
            SendOutcome::Dropped => 1,
            SendOutcome::DroppedOld => 2,
            SendOutcome::Coalesced => 3,
            SendOutcome::Closed | SendOutcome::Failed | SendOutcome::Oom => -1,
        }
    }
}

wasm_no_mangle! {
    /// Send a system message, bypassing capacity limits.
    ///
    /// System messages (actor stop / restart / supervisor lifecycle signals)
    /// are **always** accepted, even after the mailbox is closed. This matches
    /// the native `hew_mailbox_send_sys` semantics, which has no closed check
    /// and is effectively void-returning.
    ///
    /// On allocation failure, the message is dropped and the runtime records
    /// the failure in `hew_last_error` and logs it to stderr.
    ///
    /// # Safety
    ///
    /// Same requirements as [`hew_mailbox_send`].
    pub unsafe extern "C" fn hew_mailbox_send_sys(
        mb: *mut HewMailboxWasm,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };

        // SAFETY: `data` validity guaranteed by caller.
        let node = unsafe { msg_node_alloc(msg_type, data.cast_const(), size) };
        if node.is_null() {
            report_sys_enqueue_failure(msg_type, size);
            return;
        }
        if !reserve_queue_capacity(&mut mb.sys_queue, 1) {
            // SAFETY: `node` is still owned by this send path and has no reply channel.
            unsafe { msg_node_free(node) };
            report_sys_enqueue_failure(msg_type, size);
            return;
        }
        mb.sys_queue.push_back(node);
        crate::scheduler_wasm::record_message_sent();
    }
}

#[cfg_attr(
    not(target_arch = "wasm32"),
    allow(
        dead_code,
        reason = "helper is only referenced by wasm-only actor stop code"
    )
)]
pub(crate) unsafe fn mailbox_send_stop_sys_once(mb: *mut HewMailboxWasm) -> bool {
    if mb.is_null() {
        return false;
    }
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &mut *mb };

    // SAFETY: stop signals carry no payload.
    let node = unsafe { msg_node_alloc(-1, ptr::null(), 0) };
    if node.is_null() {
        report_stop_enqueue_failure();
        return false;
    }
    if !reserve_queue_capacity(&mut mb.sys_queue, 1) {
        // SAFETY: `node` is still owned by this helper and was never published.
        unsafe { msg_node_free(node) };
        report_stop_enqueue_failure();
        return false;
    }
    if mb.stop_signal_sent {
        // SAFETY: `node` was allocated above and was not published to the queue.
        unsafe { msg_node_free(node) };
        return false;
    }

    mb.stop_signal_sent = true;
    mb.sys_queue.push_back(node);
    crate::scheduler_wasm::record_message_sent();
    true
}

// ── Receive (consumer side) ─────────────────────────────────────────────

wasm_no_mangle! {
    /// Try to receive a message. System messages have priority.
    ///
    /// Returns a pointer to a [`HewMsgNode`] on success, or null if both
    /// queues are empty. The caller owns the returned node and must free it
    /// with [`hew_msg_node_free`].
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_try_recv(
        mb: *mut HewMailboxWasm,
    ) -> *mut HewMsgNode {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };

        // System messages have priority.
        if let Some(node) = mb.sys_queue.pop_front() {
            crate::scheduler_wasm::record_message_received();
            return node;
        }

        // User messages.
        if let Some(node) = mb.user_queue.pop_front() {
            mb.count -= 1;
            crate::scheduler_wasm::record_message_received();
            return node;
        }

        ptr::null_mut()
    }
}

wasm_no_mangle! {
    /// Try to receive a system message only.
    ///
    /// Returns a pointer to a [`HewMsgNode`] on success, or null if the
    /// system queue is empty.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_try_recv_sys(
        mb: *mut HewMailboxWasm,
    ) -> *mut HewMsgNode {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };

        if let Some(node) = mb.sys_queue.pop_front() {
            crate::scheduler_wasm::record_message_received();
            return node;
        }

        ptr::null_mut()
    }
}

// ── Queries ─────────────────────────────────────────────────────────────

wasm_no_mangle! {
    /// Returns `1` if either queue has messages, `0` otherwise.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_has_messages(
        mb: *mut HewMailboxWasm,
    ) -> i32 {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &*mb };
        i32::from(!mb.user_queue.is_empty() || !mb.sys_queue.is_empty())
    }
}

wasm_no_mangle! {
    /// Return the number of user messages in the mailbox.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_len(mb: *const HewMailboxWasm) -> usize {
        // SAFETY: Caller guarantees `mb` is valid.
        let count = unsafe { &*mb }.count;
        usize::try_from(count).unwrap_or(0)
    }
}

wasm_no_mangle! {
    /// Return the number of system messages in the mailbox.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_sys_len(mb: *const HewMailboxWasm) -> usize {
        // SAFETY: Caller guarantees `mb` is valid.
        unsafe { &*mb }.sys_queue.len()
    }
}

wasm_no_mangle! {
    /// Return the mailbox capacity. Returns `0` for unbounded mailboxes.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_capacity(mb: *const HewMailboxWasm) -> usize {
        // SAFETY: Caller guarantees `mb` is valid.
        let cap = unsafe { &*mb }.capacity;
        usize::try_from(cap).unwrap_or(0)
    }
}

// ── Close ───────────────────────────────────────────────────────────────

wasm_no_mangle! {
    /// Close the mailbox, rejecting future sends.
    ///
    /// # Safety
    ///
    /// `mb` must be a valid mailbox pointer.
    pub unsafe extern "C" fn hew_mailbox_close(mb: *mut HewMailboxWasm) {
        if !mb.is_null() {
            // WASM divergence (intentional): native writes `closed` as
            // `AtomicBool::store(true, Ordering::Release)` to make the flag
            // visible across threads.  WASM is single-threaded — there is no
            // concurrent reader — so a plain store is equivalent.
            // SAFETY: Caller guarantees `mb` is valid.
            unsafe { (*mb).closed = true };
        }
    }
}

/// Return `true` if the mailbox has been closed.
///
/// # WASM divergence
///
/// The native counterpart (`mailbox::mailbox_is_closed`) reads
/// `HewMailbox.closed: AtomicBool` with `Ordering::Acquire` to pair with the
/// `Ordering::Release` write in `hew_mailbox_close`, ensuring cross-thread
/// visibility.  `HewMailboxWasm.closed` is a plain `bool` (consistent with
/// every other scalar field in this struct, per the single-threaded design of
/// the WASM mailbox).  On a single-threaded WASM runtime all operations are
/// sequentially ordered on one thread, so the plain read is unconditionally
/// equivalent to `Acquire` — there is no other thread that could observe a
/// stale value.
///
/// # Safety
///
/// `mb` must be a valid, non-null pointer to a [`HewMailboxWasm`].
pub(crate) unsafe fn mailbox_is_closed(mb: *mut HewMailboxWasm) -> bool {
    // SAFETY: Caller guarantees `mb` is non-null and valid.
    // Single-threaded WASM: plain read is equivalent to Acquire (see doc above).
    unsafe { (*mb).closed }
}

// ── Cleanup ─────────────────────────────────────────────────────────────

wasm_no_mangle! {
    /// Free the mailbox, draining and freeing all remaining messages.
    ///
    /// # Safety
    ///
    /// `mb` must have been returned by [`hew_mailbox_new`],
    /// [`hew_mailbox_new_bounded`], [`hew_mailbox_new_with_policy`], or
    /// [`hew_mailbox_new_coalesce`]
    /// and must not be used after this call.
    pub unsafe extern "C" fn hew_mailbox_free(mb: *mut HewMailboxWasm) {
        if mb.is_null() {
            return;
        }

        // SAFETY: Caller guarantees `mb` was Box-allocated and is exclusively
        // owned.
        let mut mailbox = unsafe { Box::from_raw(mb) };

        // Drain user queue.
        while let Some(node) = mailbox.user_queue.pop_front() {
            // SAFETY: Each node was allocated by `msg_node_alloc`.
            unsafe { msg_node_free(node) };
        }

        // Drain system queue.
        while let Some(node) = mailbox.sys_queue.pop_front() {
            // SAFETY: Each node was allocated by `msg_node_alloc`.
            unsafe { msg_node_free(node) };
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct PriceUpdate {
        symbol: u32,
        price: i32,
    }

    unsafe extern "C" fn price_symbol_key(
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) -> u64 {
        assert_eq!(data_size, size_of::<PriceUpdate>());
        // SAFETY: tests only call this with valid `PriceUpdate` payloads.
        let update = unsafe { &*data.cast::<PriceUpdate>() };
        u64::from(update.symbol)
    }

    fn last_error_message() -> Option<String> {
        let err = crate::hew_last_error();
        if err.is_null() {
            return None;
        }
        // SAFETY: `hew_last_error` returned a non-null C string.
        Some(
            unsafe { CStr::from_ptr(err) }
                .to_string_lossy()
                .into_owned(),
        )
    }

    #[test]
    fn send_recv_roundtrip() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 42;
            let rc = hew_mailbox_send(mb, 7, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(rc, HewError::Ok as i32);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 7);
            assert_eq!((*node).data_size, size_of::<i32>());
            assert_eq!(*((*node).data.cast::<i32>()), 42);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_returns_err_oom_when_node_alloc_fails() {
        // SAFETY: test owns the mailbox exclusively; failure injection only
        // affects allocations performed by this thread.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 7;
            let _oom = fail_mailbox_alloc_on_nth(0);

            assert_eq!(
                hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>()),
                HewError::ErrOom as i32
            );
            assert_eq!(hew_mailbox_len(mb), 0);
            assert!(hew_mailbox_try_recv(mb).is_null());

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_returns_err_oom_when_payload_alloc_fails() {
        // SAFETY: test owns the mailbox exclusively; failure injection only
        // affects allocations performed by this thread.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 9;
            let _oom = fail_mailbox_alloc_on_nth(1);

            assert_eq!(
                hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>()),
                HewError::ErrOom as i32
            );
            assert_eq!(hew_mailbox_len(mb), 0);
            assert!(hew_mailbox_try_recv(mb).is_null());

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_with_reply_returns_err_oom_without_consuming_reply_channel() {
        // SAFETY: test owns the mailbox and reply channel exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            let reply = crate::reply_channel_wasm::hew_reply_channel_new();
            let _oom = fail_mailbox_alloc_on_nth(0);

            assert_eq!(
                hew_mailbox_send_with_reply(mb, 7, ptr::null_mut(), 0, reply.cast()),
                HewError::ErrOom as i32
            );
            assert_eq!(hew_mailbox_len(mb), 0);
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);
            assert!(!crate::reply_channel_wasm::test_replied(reply));
            assert!(crate::reply_channel_wasm::reply_take(reply).is_null());

            crate::reply_channel_wasm::hew_reply_channel_free(reply);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_returns_err_oom_when_user_queue_growth_fails() {
        // SAFETY: test owns the mailbox exclusively; size=0 isolates queue growth.
        unsafe {
            let mb = hew_mailbox_new();
            let _oom = fail_mailbox_alloc_on_nth(1);

            assert_eq!(
                hew_mailbox_send(mb, 11, ptr::null_mut(), 0),
                HewError::ErrOom as i32
            );
            assert_eq!(hew_mailbox_len(mb), 0);
            assert!(hew_mailbox_try_recv(mb).is_null());

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_with_reply_returns_err_oom_when_user_queue_growth_fails() {
        // SAFETY: test owns the mailbox and reply channel exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            let reply = crate::reply_channel_wasm::hew_reply_channel_new();
            let _oom = fail_mailbox_alloc_on_nth(1);

            assert_eq!(
                hew_mailbox_send_with_reply(mb, 12, ptr::null_mut(), 0, reply.cast()),
                HewError::ErrOom as i32
            );
            assert_eq!(hew_mailbox_len(mb), 0);
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);
            assert!(!crate::reply_channel_wasm::test_replied(reply));
            assert!(crate::reply_channel_wasm::reply_take(reply).is_null());

            crate::reply_channel_wasm::hew_reply_channel_free(reply);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_sys_queue_growth_failure_sets_last_error() {
        // SAFETY: test owns the mailbox exclusively; size=0 isolates queue growth.
        unsafe {
            crate::hew_clear_error();
            let mb = hew_mailbox_new();
            let _oom = fail_mailbox_alloc_on_nth(1);

            hew_mailbox_send_sys(mb, 99, ptr::null_mut(), 0);

            assert!(hew_mailbox_try_recv_sys(mb).is_null());
            let err = last_error_message().expect("sys OOM should set hew_last_error");
            assert!(
                err.contains("hew_mailbox_send_sys: failed to deliver system message"),
                "unexpected error message: {err}"
            );

            hew_mailbox_free(mb);
            crate::hew_clear_error();
        }
    }

    #[test]
    fn stop_sys_queue_growth_failure_sets_last_error_and_allows_retry() {
        // SAFETY: test owns the mailbox exclusively; size=0 isolates queue growth.
        unsafe {
            crate::hew_clear_error();
            let mb = hew_mailbox_new();
            let _oom = fail_mailbox_alloc_on_nth(1);

            assert!(!mailbox_send_stop_sys_once(mb));
            assert!(!(*mb).stop_signal_sent);
            assert!(hew_mailbox_try_recv_sys(mb).is_null());

            let err = last_error_message().expect("stop OOM should set hew_last_error");
            assert!(
                err.contains("hew_actor_stop: failed to enqueue shutdown system message"),
                "unexpected error message: {err}"
            );

            assert!(mailbox_send_stop_sys_once(mb));
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, -1);
            msg_node_free(node);

            hew_mailbox_free(mb);
            crate::hew_clear_error();
        }
    }

    #[test]
    fn closing_and_freeing_mailbox_completes_orphaned_ask_channel() {
        // SAFETY: test owns the mailbox and reply channel exclusively; the
        // queued ask simulates an actor being stopped/closed before dispatch.
        unsafe {
            let mb = hew_mailbox_new();
            let ch = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(ch);

            let val: i32 = 42;
            let rc = hew_mailbox_send_with_reply(
                mb,
                7,
                (&raw const val).cast_mut().cast(),
                size_of::<i32>(),
                ch.cast(),
            );
            assert_eq!(rc, HewError::Ok as i32);

            assert_eq!(crate::reply_channel_wasm::test_ref_count(ch), 2);
            assert!(!crate::reply_channel_wasm::test_replied(ch));

            hew_mailbox_close(mb);
            hew_mailbox_free(mb);

            assert!(
                crate::reply_channel_wasm::test_replied(ch),
                "draining a closed mailbox should signal orphaned ask waiters"
            );
            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "draining a closed mailbox should release the sender-side ask reference"
            );

            let reply = crate::reply_channel_wasm::reply_take(ch);
            assert!(
                reply.is_null(),
                "orphaned asks should resolve as null replies"
            );

            crate::reply_channel_wasm::hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn bounded_overflow_dropnew() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(2);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            // Third send should fail with ErrMailboxFull.
            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn has_messages_and_len() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_has_messages(mb), 0);
            assert_eq!(hew_mailbox_len(mb), 0);

            let val: i32 = 10;
            let p = (&raw const val).cast_mut().cast();

            hew_mailbox_send(mb, 1, p, size_of::<i32>());
            assert_eq!(hew_mailbox_has_messages(mb), 1);
            assert_eq!(hew_mailbox_len(mb), 1);

            hew_mailbox_send(mb, 2, p, size_of::<i32>());
            assert_eq!(hew_mailbox_len(mb), 2);

            // Receive one.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            msg_node_free(node);
            assert_eq!(hew_mailbox_len(mb), 1);
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            // Receive the last one.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            msg_node_free(node);
            assert_eq!(hew_mailbox_len(mb), 0);
            assert_eq!(hew_mailbox_has_messages(mb), 0);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_queue_separate() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let u: i32 = 10;
            let s: i32 = 99;

            // Send a user message and a system message.
            hew_mailbox_send(mb, 1, (&raw const u).cast_mut().cast(), size_of::<i32>());
            hew_mailbox_send_sys(mb, 2, (&raw const s).cast_mut().cast(), size_of::<i32>());

            // System messages should not appear in len (user count).
            assert_eq!(hew_mailbox_len(mb), 1);

            // try_recv_sys returns only system messages.
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            assert_eq!(*((*node).data.cast::<i32>()), 99);
            msg_node_free(node);

            // No more system messages.
            assert!(hew_mailbox_try_recv_sys(mb).is_null());

            // User message is still there.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!(*((*node).data.cast::<i32>()), 10);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn recv_empty_returns_null() {
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            assert!(hew_mailbox_try_recv(mb).is_null());
            assert!(hew_mailbox_try_recv_sys(mb).is_null());
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_messages_have_priority_in_try_recv() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let u: i32 = 10;
            let s: i32 = 99;

            // Send a user message first, then a system message.
            hew_mailbox_send(mb, 1, (&raw const u).cast_mut().cast(), size_of::<i32>());
            hew_mailbox_send_sys(mb, 2, (&raw const s).cast_mut().cast(), size_of::<i32>());

            // try_recv should return the system message first.
            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 2);
            msg_node_free(node);

            // Then the user message.
            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn freeing_mailbox_releases_queued_reply_channel() {
        // SAFETY: test owns the mailbox and reply channel exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            let ch = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(ch);

            assert_eq!(crate::reply_channel_wasm::test_ref_count(ch), 2);
            assert_eq!(
                hew_mailbox_send_with_reply(mb, 7, ptr::null_mut(), 0, ch.cast()),
                HewError::Ok as i32
            );

            hew_mailbox_free(mb);

            assert_eq!(
                crate::reply_channel_wasm::test_ref_count(ch),
                1,
                "draining queued messages must release the queued sender ref"
            );
            crate::reply_channel_wasm::hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn bounded_overflow_drop_old() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(2, HewOverflowPolicy::DropOld);
            let a: i32 = 1;
            let b: i32 = 2;
            let c: i32 = 3;

            hew_mailbox_send(mb, 0, (&raw const a).cast_mut().cast(), size_of::<i32>());
            hew_mailbox_send(mb, 0, (&raw const b).cast_mut().cast(), size_of::<i32>());
            // Third send should succeed (drops oldest).
            let rc = hew_mailbox_send(mb, 0, (&raw const c).cast_mut().cast(), size_of::<i32>());
            assert_eq!(rc, HewError::Ok as i32);
            assert_eq!(hew_mailbox_len(mb), 2);

            // Oldest (a=1) was dropped, so first recv should be b=2.
            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 2);
            msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 3);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_is_same_as_send() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            let val: i32 = 7;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(hew_mailbox_try_send(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn null_data_succeeds() {
        // SAFETY: test owns the mailbox exclusively; null data is a valid input.
        unsafe {
            let mb = hew_mailbox_new();
            let rc = hew_mailbox_send(mb, 0, ptr::null_mut(), 0);
            assert_eq!(rc, 0);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert!((*node).data.is_null());
            assert_eq!((*node).data_size, 0);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn deep_copy_isolation() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let mut val: i32 = 100;
            hew_mailbox_send(mb, 0, (&raw mut val).cast(), size_of::<i32>());

            // Mutate original after send.
            val = 999;

            let node = hew_mailbox_try_recv(mb);
            // Message should have the original value.
            assert_eq!(*((*node).data.cast::<i32>()), 100);
            msg_node_free(node);

            // Suppress unused-value warning.
            let _ = val;

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn closed_mailbox_rejects_sends() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            // Close the mailbox.
            (*mb).closed = true;

            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrClosed as i32
            );

            // sys messages must still be accepted even on a closed mailbox.
            hew_mailbox_send_sys(mb, 0, p, size_of::<i32>());
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_accepted_after_close() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 42;
            let p = (&raw const val).cast_mut().cast();

            // Close the mailbox first.
            hew_mailbox_close(mb);

            // send_sys must enqueue the node despite the closed flag; this is
            // the intended native behaviour for lifecycle/shutdown signals.
            hew_mailbox_send_sys(mb, -1, p, size_of::<i32>());
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_bypasses_capacity() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            // Fill user queue.
            hew_mailbox_send(mb, 0, p, size_of::<i32>());
            // User queue is full.
            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );
            // System message should still succeed.
            hew_mailbox_send_sys(mb, 99, p, size_of::<i32>());
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn capacity_query() {
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb_unbounded = hew_mailbox_new();
            assert_eq!(hew_mailbox_capacity(mb_unbounded), 0);
            hew_mailbox_free(mb_unbounded);

            let mb_bounded = hew_mailbox_new_bounded(10);
            assert_eq!(hew_mailbox_capacity(mb_bounded), 10);
            hew_mailbox_free(mb_bounded);
        }
    }

    #[test]
    fn block_policy_degrades_to_drop_new() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            // Block degrades to DropNew on WASM -- should return
            // ErrMailboxFull.
            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_uses_configured_key_fn() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(2);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let first = PriceUpdate {
                symbol: 7,
                price: 10,
            };
            let second = PriceUpdate {
                symbol: 9,
                price: 20,
            };
            let replacement = PriceUpdate {
                symbol: 7,
                price: 99,
            };

            assert_eq!(
                hew_mailbox_send(
                    mb,
                    100,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    200,
                    (&raw const second).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(hew_mailbox_len(mb), 2);

            assert_eq!(
                hew_mailbox_send(
                    mb,
                    300,
                    (&raw const replacement).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(hew_mailbox_len(mb), 2);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 300);
            let payload = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*payload).symbol, 7);
            assert_eq!((*payload).price, 99);
            msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 200);
            let payload = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*payload).symbol, 9);
            assert_eq!((*payload).price, 20);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn coalesce_send_with_reply_retires_superseded_incoming_waiter() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);

            let first: i32 = 10;
            let replacement: i32 = 77;
            let existing_reply = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(existing_reply);
            let incoming_reply = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(incoming_reply);

            let existing_reply_ptr = existing_reply.cast::<c_void>();
            let incoming_reply_ptr = incoming_reply.cast::<c_void>();

            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    7,
                    (&raw const first).cast_mut().cast(),
                    size_of::<i32>(),
                    existing_reply_ptr,
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    7,
                    (&raw const replacement).cast_mut().cast(),
                    size_of::<i32>(),
                    incoming_reply_ptr,
                ),
                HewError::Ok as i32
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            assert_eq!(crate::reply_channel_wasm::test_ref_count(incoming_reply), 1);
            assert!(crate::reply_channel_wasm::test_replied(incoming_reply));
            assert!(crate::reply_channel_wasm::reply_take(incoming_reply).is_null());

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 7);
            assert_eq!(*((*node).data.cast::<i32>()), 77);
            assert_eq!((*node).reply_channel, existing_reply_ptr);
            msg_node_free(node);

            crate::reply_channel_wasm::hew_reply_channel_free(existing_reply);
            crate::reply_channel_wasm::hew_reply_channel_free(incoming_reply);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn drop_old_retires_evicted_reply_waiter() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::DropOld);
            let first: i32 = 10;
            let second: i32 = 20;
            let reply = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(reply);

            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<i32>(),
                    reply.cast(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    2,
                    (&raw const second).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );

            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);
            assert!(crate::reply_channel_wasm::test_replied(reply));
            assert!(crate::reply_channel_wasm::reply_take(reply).is_null());

            crate::reply_channel_wasm::hew_reply_channel_free(reply);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn free_null_is_noop() {
        // SAFETY: null pointer is explicitly handled.
        unsafe {
            hew_mailbox_free(ptr::null_mut());
        }
    }

    // ── try_send parity tests (WASM side) ───────────────────────────────
    // These mirror the corresponding tests in mailbox.rs to make the
    // native/WASM parity contract explicit and catch future regressions.

    #[test]
    fn try_send_drop_old_policy() {
        // try_send with DropOld must evict the oldest message rather than fail.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(2, HewOverflowPolicy::DropOld);
            let a: i32 = 1;
            let b: i32 = 2;
            let c: i32 = 3;

            assert_eq!(
                hew_mailbox_try_send(mb, 0, (&raw const a).cast_mut().cast(), size_of::<i32>()),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_try_send(mb, 0, (&raw const b).cast_mut().cast(), size_of::<i32>()),
                HewError::Ok as i32
            );
            // Full — DropOld should evict a=1 and admit c=3.
            assert_eq!(
                hew_mailbox_try_send(mb, 0, (&raw const c).cast_mut().cast(), size_of::<i32>()),
                HewError::Ok as i32,
                "try_send with DropOld must succeed when full"
            );
            assert_eq!(hew_mailbox_len(mb), 2);

            let n1 = hew_mailbox_try_recv(mb);
            assert_eq!(*((*n1).data.cast::<i32>()), 2);
            msg_node_free(n1);
            let n2 = hew_mailbox_try_recv(mb);
            assert_eq!(*((*n2).data.cast::<i32>()), 3);
            msg_node_free(n2);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_coalesce_policy() {
        // try_send with Coalesce must replace a matching queued message.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let first = PriceUpdate {
                symbol: 42,
                price: 10,
            };
            let updated = PriceUpdate {
                symbol: 42,
                price: 99,
            };

            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            // Full — same key: should coalesce and return Ok.
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    1,
                    (&raw const updated).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32,
                "try_send with Coalesce must coalesce a matching queued message"
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            let got = *((*node).data.cast::<PriceUpdate>());
            assert_eq!(
                got.price, 99,
                "coalesced message must carry updated payload"
            );
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_closed_returns_err_closed() {
        // try_send on a closed mailbox must return ErrClosed (-4).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            hew_mailbox_close(mb);

            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrClosed as i32,
                "try_send on closed mailbox must return ErrClosed"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_block_policy_fails_immediately() {
        // Block policy must degrade to ErrMailboxFull on try_send (WASM has no
        // condvar, so Block always fails without waiting — parity mirror of the
        // native try_send_block_policy_fails_immediately test).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::Ok as i32
            );
            // Full with Block policy — must return ErrMailboxFull, not block.
            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32,
                "try_send with Block must fail immediately on WASM"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_coalesce_block_fallback_no_match_fails_immediately() {
        // Coalesce with a Block fallback and no matching key: try_send must
        // return ErrMailboxFull without blocking.  This exercises the second
        // non_blocking guard inside send_with_overflow's Coalesce fallback arm.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            // Configure Block as the fallback so a cache-miss at capacity
            // would normally wait on the condvar.
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::Block);

            let first = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            let different = PriceUpdate {
                symbol: 2,
                price: 20,
            }; // different key → no coalesce match

            // Fill the mailbox with symbol=1.
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            // Full, no key match, fallback is Block — must fail immediately.
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    1,
                    (&raw const different).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::ErrMailboxFull as i32,
                "Coalesce+Block fallback with no key match must fail immediately on try_send"
            );
            assert_eq!(hew_mailbox_len(mb), 1, "queue length must be unchanged");

            let node = hew_mailbox_try_recv(mb);
            msg_node_free(node);
            hew_mailbox_free(mb);
        }
    }

    // ── hew_mailbox_try_push parity tests ───────────────────────────────
    // These mirror the corresponding tests in mailbox.rs to certify that the
    // WASM try_push returns the same fine-grained status codes as native.

    #[test]
    fn try_push_enqueued() {
        // An unbounded mailbox must return 0 (Enqueued).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 42;
            assert_eq!(
                hew_mailbox_try_push(mb, 7, (&raw const val).cast(), size_of::<i32>()),
                0,
                "unbounded try_push must return 0 (Enqueued)"
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 7);
            assert_eq!(*((*node).data.cast::<i32>()), 42);
            msg_node_free(node);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_dropped() {
        // DropNew policy at capacity must return 1 (Dropped).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            let a: i32 = 1;
            let b: i32 = 2;
            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const a).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const b).cast(), size_of::<i32>()),
                1,
                "DropNew try_push at capacity must return 1 (Dropped)"
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 1);
            msg_node_free(node);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_dropped_old() {
        // DropOld policy at capacity must return 2 (DroppedOld).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::DropOld);
            let a: i32 = 10;
            let b: i32 = 20;

            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const a).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const b).cast(), size_of::<i32>()),
                2,
                "DropOld try_push at capacity must return 2 (DroppedOld)"
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 20, "oldest must be evicted");
            msg_node_free(node);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_coalesced() {
        // Coalesce policy with matching key must return 3 (Coalesced).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(2);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let a = PriceUpdate {
                symbol: 7,
                price: 10,
            };
            let b = PriceUpdate {
                symbol: 9,
                price: 20,
            };
            let c = PriceUpdate {
                symbol: 7,
                price: 99,
            };

            assert_eq!(
                hew_mailbox_try_push(mb, 100, (&raw const a).cast(), size_of::<PriceUpdate>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 200, (&raw const b).cast(), size_of::<PriceUpdate>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 300, (&raw const c).cast(), size_of::<PriceUpdate>()),
                3,
                "coalesce try_push with matching key must return 3 (Coalesced)"
            );
            assert_eq!(hew_mailbox_len(mb), 2);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 300);
            let payload = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*payload).symbol, 7);
            assert_eq!((*payload).price, 99);
            msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 200);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_coalesce_fallback_dropnew() {
        // Coalesce with DropNew fallback and no match must return 1 (Dropped).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::DropNew);

            let a: i32 = 10;
            let b: i32 = 20;
            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const a).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const b).cast(), size_of::<i32>()),
                1,
                "coalesce fallback DropNew must return 1 (Dropped)"
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_closed_returns_negative() {
        // Closed mailbox must return -1.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            hew_mailbox_close(mb);

            let val: i32 = 1;
            assert_eq!(
                hew_mailbox_try_push(mb, 0, (&raw const val).cast(), size_of::<i32>()),
                -1,
                "try_push on closed mailbox must return -1"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_null_data() {
        // Null data with size 0 must succeed.
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_try_push(mb, 42, ptr::null(), 0), 0);
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 42);
            assert!((*node).data.is_null());
            assert_eq!((*node).data_size, 0);
            assert!(
                (*node).reply_channel.is_null(),
                "try_push must never set a reply channel"
            );
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_fail_policy_returns_negative() {
        // Fail policy at capacity must return -1 (Failed), not 1 (Dropped).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Fail);
            let a: i32 = 1;
            let b: i32 = 2;

            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const a).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const b).cast(), size_of::<i32>()),
                -1,
                "Fail policy try_push at capacity must return -1 (Failed)"
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 1);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_coalesce_fallback_fail_returns_negative() {
        // Coalesce with Fail fallback and no matching key must return -1.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::Fail);

            let first = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            let different = PriceUpdate {
                symbol: 2,
                price: 20,
            };

            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const first).cast(), size_of::<PriceUpdate>(),),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(
                    mb,
                    2,
                    (&raw const different).cast(),
                    size_of::<PriceUpdate>(),
                ),
                -1,
                "Coalesce+Fail fallback with no key match must return -1"
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            let got = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*got).symbol, 1, "original message must be preserved");
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_drop_old_oom_preserves_queue() {
        // If allocation fails under DropOld, the old message must NOT be
        // evicted — OOM must leave the queue unchanged.
        // SAFETY: test owns the mailbox exclusively; failure injection only
        // affects allocations performed by this thread.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::DropOld);
            let original: i32 = 42;

            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const original).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            // Force OOM on the next node allocation.
            let _oom = fail_mailbox_alloc_on_nth(0);

            let replacement: i32 = 99;
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const replacement).cast(), size_of::<i32>()),
                -1,
                "DropOld try_push OOM must return -1"
            );
            assert_eq!(
                hew_mailbox_len(mb),
                1,
                "OOM under DropOld must not evict the existing message"
            );

            // The original message must still be there, intact.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!(*((*node).data.cast::<i32>()), 42);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_coalesce_fallback_drop_old_oom_preserves_queue() {
        // Same as above but through the coalesce → DropOld fallback path.
        // SAFETY: test owns the mailbox exclusively; failure injection only
        // affects allocations performed by this thread.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let original = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            assert_eq!(
                hew_mailbox_try_push(
                    mb,
                    1,
                    (&raw const original).cast(),
                    size_of::<PriceUpdate>(),
                ),
                0
            );

            // Force OOM — different key so coalesce won't match, falls to DropOld.
            let _oom = fail_mailbox_alloc_on_nth(0);
            let different = PriceUpdate {
                symbol: 2,
                price: 20,
            };
            assert_eq!(
                hew_mailbox_try_push(
                    mb,
                    2,
                    (&raw const different).cast(),
                    size_of::<PriceUpdate>(),
                ),
                -1,
                "Coalesce+DropOld fallback OOM must return -1"
            );
            assert_eq!(
                hew_mailbox_len(mb),
                1,
                "OOM under Coalesce+DropOld fallback must not evict"
            );

            let node = hew_mailbox_try_recv(mb);
            let got = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*got).symbol, 1, "original message must be preserved");
            assert_eq!((*got).price, 10);
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    // ── hew_mailbox_sys_len parity tests ────────────────────────────────

    #[test]
    fn sys_len_tracks_system_messages() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_sys_len(mb), 0);

            let val: i32 = 99;
            hew_mailbox_send_sys(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(hew_mailbox_sys_len(mb), 1);

            hew_mailbox_send_sys(mb, 2, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(hew_mailbox_sys_len(mb), 2);

            // User messages must not affect sys_len.
            hew_mailbox_send(mb, 3, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(hew_mailbox_sys_len(mb), 2);
            assert_eq!(hew_mailbox_len(mb), 1);

            // Drain system messages.
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(!node.is_null());
            msg_node_free(node);
            assert_eq!(hew_mailbox_sys_len(mb), 1);

            let node = hew_mailbox_try_recv_sys(mb);
            assert!(!node.is_null());
            msg_node_free(node);
            assert_eq!(hew_mailbox_sys_len(mb), 0);

            assert!(hew_mailbox_try_recv_sys(mb).is_null());

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_len_zero_on_fresh_mailbox() {
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_sys_len(mb), 0);
            hew_mailbox_free(mb);

            let mb = hew_mailbox_new_bounded(5);
            assert_eq!(hew_mailbox_sys_len(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_len_unaffected_by_close() {
        // System messages are accepted even after close; sys_len must track.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 1;
            hew_mailbox_close(mb);

            hew_mailbox_send_sys(mb, -1, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(hew_mailbox_sys_len(mb), 1);

            hew_mailbox_free(mb);
        }
    }

    // ── Coalesce + reply-channel contract certification ─────────────────
    // These tests prove the coalesce/reply-channel invariants that the
    // phase0/runtime-reliability concern flagged.

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn coalesce_reply_channel_preserved_on_merge() {
        // When a coalescing send replaces a queued message that already carries
        // a reply channel, the QUEUED channel must be preserved (it was
        // promised to the original sender) and the INCOMING channel must be
        // retired as orphaned.
        // SAFETY: test owns the mailbox and reply channels exclusively.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);

            let first: i32 = 10;
            let second: i32 = 20;
            let queued_ch = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(queued_ch);
            let incoming_ch = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(incoming_ch);

            // Enqueue first message with queued_ch.
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    7,
                    (&raw const first).cast_mut().cast(),
                    size_of::<i32>(),
                    queued_ch.cast(),
                ),
                HewError::Ok as i32
            );

            // Coalesce with same key (msg_type=7) — queued_ch must survive,
            // incoming_ch must be retired.
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    7,
                    (&raw const second).cast_mut().cast(),
                    size_of::<i32>(),
                    incoming_ch.cast(),
                ),
                HewError::Ok as i32
            );

            // Incoming channel must be retired (orphaned + replied).
            assert!(
                crate::reply_channel_wasm::test_replied(incoming_ch),
                "superseded incoming reply channel must be retired"
            );
            assert!(
                crate::reply_channel_wasm::reply_is_orphaned(incoming_ch),
                "superseded incoming reply channel must be marked orphaned"
            );
            assert_eq!(crate::reply_channel_wasm::test_ref_count(incoming_ch), 1);

            // Queued channel must NOT be retired — it belongs to the first
            // sender and the handler will eventually reply on it.
            assert!(
                !crate::reply_channel_wasm::test_replied(queued_ch),
                "queued reply channel must NOT be retired during coalesce"
            );

            // Dequeue and verify the reply_channel on the coalesced node
            // points to the original queued channel.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 7);
            assert_eq!(*((*node).data.cast::<i32>()), 20, "payload must be updated");
            assert_eq!(
                (*node).reply_channel,
                queued_ch.cast(),
                "coalesced node must preserve the original queued reply channel"
            );
            msg_node_free(node);

            crate::reply_channel_wasm::hew_reply_channel_free(queued_ch);
            crate::reply_channel_wasm::hew_reply_channel_free(incoming_ch);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn coalesce_no_reply_channel_does_not_retire() {
        // When neither the queued nor the incoming message carries a reply
        // channel (fire-and-forget), coalesce must not attempt retirement.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);

            let first: i32 = 10;
            let second: i32 = 77;

            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const first).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const second).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 77);
            assert!(
                (*node).reply_channel.is_null(),
                "fire-and-forget coalesce must not fabricate a reply channel"
            );
            msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn coalesce_same_reply_channel_not_retired() {
        // Edge case: if the same reply channel pointer is sent twice (e.g.
        // retained and resent), it must NOT be retired — the identity check
        // must prevent double-retirement.
        // SAFETY: test owns the mailbox and reply channel exclusively.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);

            let ch = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(ch);
            crate::reply_channel_wasm::hew_reply_channel_retain(ch);

            let first: i32 = 10;
            let second: i32 = 20;

            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    7,
                    (&raw const first).cast_mut().cast(),
                    size_of::<i32>(),
                    ch.cast(),
                ),
                HewError::Ok as i32
            );
            // Send again with the SAME channel pointer.
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    7,
                    (&raw const second).cast_mut().cast(),
                    size_of::<i32>(),
                    ch.cast(),
                ),
                HewError::Ok as i32
            );

            // The channel must NOT be retired — it's the same pointer.
            assert!(
                !crate::reply_channel_wasm::test_replied(ch),
                "same-pointer coalesce must not retire the reply channel"
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!(*((*node).data.cast::<i32>()), 20);
            msg_node_free(node);

            crate::reply_channel_wasm::hew_reply_channel_free(ch);
            crate::reply_channel_wasm::hew_reply_channel_free(ch);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn coalesce_drop_old_fallback_retires_evicted_reply_channel() {
        // When coalesce cannot find a matching key and falls back to DropOld,
        // evicting a message with a reply channel must retire that channel.
        // SAFETY: test owns the mailbox and reply channel exclusively.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let reply = crate::reply_channel_wasm::hew_reply_channel_new();
            crate::reply_channel_wasm::hew_reply_channel_retain(reply);

            let first = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            let different = PriceUpdate {
                symbol: 2,
                price: 20,
            };

            // Enqueue with reply channel.
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                    reply.cast(),
                ),
                HewError::Ok as i32
            );

            // Different key → no coalesce match → DropOld fallback evicts first.
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    2,
                    (&raw const different).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );

            // Evicted message's reply channel must be retired.
            assert!(
                crate::reply_channel_wasm::test_replied(reply),
                "DropOld eviction must retire the evicted message's reply channel"
            );
            assert!(
                crate::reply_channel_wasm::reply_is_orphaned(reply),
                "DropOld eviction must mark the reply channel as orphaned"
            );
            assert_eq!(crate::reply_channel_wasm::test_ref_count(reply), 1);

            let node = hew_mailbox_try_recv(mb);
            msg_node_free(node);

            crate::reply_channel_wasm::hew_reply_channel_free(reply);
            hew_mailbox_free(mb);
        }
    }
}
