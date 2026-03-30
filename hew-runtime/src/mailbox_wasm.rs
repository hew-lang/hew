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

use std::collections::VecDeque;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::AtomicPtr;

use crate::internal::types::{HewError, HewOverflowPolicy};

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

// ── Message node ────────────────────────────────────────────────────────
//
// We duplicate the struct here rather than importing from `crate::mailbox`
// because that module is `#[cfg(not(target_arch = "wasm32"))]` and does
// not exist on WASM targets. The layout is identical for C ABI compat.

/// A single message in a mailbox queue.
///
/// Allocated with [`libc::malloc`] and freed by [`msg_node_free`].
/// The `next` field is unused on WASM (queues are `VecDeque`-backed)
/// but kept for struct layout compatibility with the native mailbox.
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

// ── Message node helpers ────────────────────────────────────────────────

/// Allocate a [`HewMsgNode`] via `libc::malloc`, deep-copying `data`.
///
/// # Safety
///
/// `data` must point to at least `data_size` readable bytes, or be null
/// when `data_size` is 0.
unsafe fn msg_node_alloc(msg_type: i32, data: *const c_void, data_size: usize) -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) -- POD-like struct, no drop glue.
    let node = unsafe { libc::malloc(size_of::<HewMsgNode>()) }.cast::<HewMsgNode>();
    assert!(!node.is_null(), "OOM allocating message node");

    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = msg_type;
        (*node).data_size = data_size;
        (*node).reply_channel = ptr::null_mut();

        // Deep-copy message data for actor isolation.
        if data_size > 0 && !data.is_null() {
            let buf = libc::malloc(data_size);
            assert!(
                !buf.is_null(),
                "OOM allocating message data ({data_size} bytes)"
            );
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
            new_buf = libc::malloc(data_size);
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

    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: `reply_channel` is a live native reply channel owned by the
    // mailbox and we are retiring it with an empty reply.
    unsafe {
        crate::reply_channel::hew_reply(reply_channel.cast(), ptr::null_mut(), 0);
    }
    #[cfg(target_arch = "wasm32")]
    // SAFETY: `reply_channel` is a live WASM reply channel owned by the mailbox
    // and we are retiring it with an empty reply.
    unsafe {
        crate::reply_channel_wasm::hew_reply(reply_channel.cast(), ptr::null_mut(), 0);
    }
}

unsafe fn send_user_message(
    mb: &mut HewMailboxWasm,
    msg_type: i32,
    data: *const c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> i32 {
    if mb.closed {
        return HewError::ErrClosed as i32;
    }

    if mb.capacity > 0 && mb.count >= mb.capacity {
        match mb.overflow {
            HewOverflowPolicy::Block | HewOverflowPolicy::DropNew | HewOverflowPolicy::Fail => {
                return HewError::ErrMailboxFull as i32;
            }
            HewOverflowPolicy::DropOld => {
                if let Some(old) = mb.user_queue.pop_front() {
                    // SAFETY: node was allocated by msg_node_alloc.
                    unsafe { msg_node_free(old) };
                    mb.count -= 1;
                }
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
                        return HewError::ErrOom as i32;
                    }
                    // Preserve the queued node's reply_channel to mirror the
                    // native mailbox contract for ask-style coalescing, but
                    // retire any superseded incoming waiter immediately.
                    if !reply_channel.is_null() && reply_channel != existing_reply_channel {
                        // SAFETY: the incoming ask waiter is no longer queued.
                        unsafe { retire_reply_channel(reply_channel) };
                    }
                    return HewError::Ok as i32;
                }

                match normalize_coalesce_fallback(mb.coalesce_fallback) {
                    HewOverflowPolicy::Block
                    | HewOverflowPolicy::DropNew
                    | HewOverflowPolicy::Fail => {
                        return HewError::ErrMailboxFull as i32;
                    }
                    HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce => {
                        if let Some(old) = mb.user_queue.pop_front() {
                            // SAFETY: node was allocated by msg_node_alloc.
                            unsafe { msg_node_free(old) };
                            mb.count -= 1;
                        }
                    }
                }
            }
        }
    }

    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, size) };
    // SAFETY: node was just allocated and is exclusively owned.
    unsafe { (*node).reply_channel = reply_channel };
    mb.user_queue.push_back(node);
    mb.count += 1;
    update_high_water_mark(mb);

    HewError::Ok as i32
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
    /// `DropNew`/`Fail`/`Block` policies), or `-4` ([`HewError::ErrClosed`])
    /// if the mailbox is closed.
    ///
    /// On WASM, `Block` degrades to `DropNew` because there is no way to
    /// block a single-threaded runtime. `Coalesce` still replaces matching
    /// queued messages and otherwise uses its configured fallback policy.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation for the message node fails (OOM).
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
    /// field on the message node for the ask pattern.
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
    /// Send a system message, bypassing capacity limits.
    ///
    /// System messages are always accepted (the sys queue is unbounded).
    /// Returns `0` ([`HewError::Ok`]) on success or `-4`
    /// ([`HewError::ErrClosed`]) if the mailbox is closed.
    ///
    /// # Safety
    ///
    /// Same requirements as [`hew_mailbox_send`].
    pub unsafe extern "C" fn hew_mailbox_send_sys(
        mb: *mut HewMailboxWasm,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) -> i32 {
        // SAFETY: Caller guarantees `mb` is valid.
        let mb = unsafe { &mut *mb };

        if mb.closed {
            return HewError::ErrClosed as i32;
        }

        // SAFETY: `data` validity guaranteed by caller.
        let node = unsafe { msg_node_alloc(msg_type, data.cast_const(), size) };
        mb.sys_queue.push_back(node);

        HewError::Ok as i32
    }
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
            return node;
        }

        // User messages.
        if let Some(node) = mb.user_queue.pop_front() {
            mb.count -= 1;
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
            // SAFETY: Caller guarantees `mb` is valid.
            unsafe { (*mb).closed = true };
        }
    }
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
    #[cfg(not(target_arch = "wasm32"))]
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

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

    #[cfg(not(target_arch = "wasm32"))]
    #[repr(C)]
    struct ReplyChannelPrefix {
        refs: AtomicUsize,
        ready: AtomicBool,
        cancelled: AtomicBool,
        value: *mut c_void,
        value_size: usize,
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
            assert_eq!(
                hew_mailbox_send_sys(mb, 0, p, size_of::<i32>()),
                HewError::ErrClosed as i32
            );

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
            assert_eq!(
                hew_mailbox_send_sys(mb, 99, p, size_of::<i32>()),
                HewError::Ok as i32
            );
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
            let existing_reply = crate::reply_channel::hew_reply_channel_new();
            crate::reply_channel::hew_reply_channel_retain(existing_reply);
            let incoming_reply = crate::reply_channel::hew_reply_channel_new();
            crate::reply_channel::hew_reply_channel_retain(incoming_reply);

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

            let incoming_state = incoming_reply.cast::<ReplyChannelPrefix>();
            assert_eq!((*incoming_state).refs.load(Ordering::Acquire), 1);
            assert!((*incoming_state).ready.load(Ordering::Acquire));
            assert!((*incoming_state).value.is_null());

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 7);
            assert_eq!(*((*node).data.cast::<i32>()), 77);
            assert_eq!((*node).reply_channel, existing_reply_ptr);
            msg_node_free(node);

            crate::reply_channel::hew_reply_channel_free(existing_reply);
            crate::reply_channel::hew_reply_channel_free(incoming_reply);
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
            let reply = crate::reply_channel::hew_reply_channel_new();
            crate::reply_channel::hew_reply_channel_retain(reply);

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

            let reply_state = reply.cast::<ReplyChannelPrefix>();
            assert_eq!((*reply_state).refs.load(Ordering::Acquire), 1);
            assert!((*reply_state).ready.load(Ordering::Acquire));
            assert!((*reply_state).value.is_null());

            crate::reply_channel::hew_reply_channel_free(reply);
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
}
