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
            high_water_mark: 0,
            closed: false,
        }))
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
    /// block a single-threaded runtime. `Coalesce` falls back to `DropOld`.
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

        if mb.closed {
            return HewError::ErrClosed as i32;
        }

        // Bounded capacity check.
        if mb.capacity > 0 && mb.count >= mb.capacity {
            match mb.overflow {
                // Block degrades to DropNew on single-threaded WASM.
                HewOverflowPolicy::Block
                | HewOverflowPolicy::DropNew
                | HewOverflowPolicy::Fail => {
                    return HewError::ErrMailboxFull as i32;
                }
                HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce => {
                    // Pop the oldest user message, then push the new one.
                    if let Some(old) = mb.user_queue.pop_front() {
                        // SAFETY: node was allocated by msg_node_alloc.
                        unsafe { msg_node_free(old) };
                        mb.count -= 1;
                    }
                }
            }
        }

        // SAFETY: `data` validity guaranteed by caller.
        let node = unsafe { msg_node_alloc(msg_type, data.cast_const(), size) };
        mb.user_queue.push_back(node);
        mb.count += 1;
        update_high_water_mark(mb);

        HewError::Ok as i32
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
    /// [`hew_mailbox_new_bounded`], or [`hew_mailbox_new_with_policy`]
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
    fn free_null_is_noop() {
        // SAFETY: null pointer is explicitly handled.
        unsafe {
            hew_mailbox_free(ptr::null_mut());
        }
    }
}
