//! Hew runtime: `mailbox` module.
//!
//! Dual-queue message passing primitive used by actors. Each mailbox has:
//!
//! - A **user message queue** (MPSC) for application-level messages.
//! - A **system message queue** (MPSC) for lifecycle events — always unbounded.
//!
//! Messages are deep-copied on send to ensure actor isolation. Bounded
//! mailboxes apply an overflow policy when capacity is exceeded.
//!
//! The user queue uses a lock-free stable-stub MPSC algorithm for the fast
//! path (unbounded, `DropNew`, `Fail`). Complex overflow policies (`Block`,
//! `DropOld`, `Coalesce`) fall back to a `Mutex`-protected `VecDeque` since
//! they require queue traversal or blocking.
//!
//! System messages use a separate lock-free MPSC queue.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::util::{CondvarExt, MutexExt};
#[cfg(test)]
use std::cell::Cell;
use std::cell::UnsafeCell;
use std::collections::VecDeque;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI64, AtomicPtr, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};

use crate::internal::types::{HewError, HewOverflowPolicy};
use crate::scheduler::{MESSAGES_RECEIVED, MESSAGES_SENT};
use crate::set_last_error;
use crate::tracing::HewTraceContext;

/// Re-export of [`HewOverflowPolicy`] for the public mailbox API.
pub use crate::internal::types::HewOverflowPolicy as OverflowPolicy;

/// Key extractor used by coalescing mailboxes.
pub type HewCoalesceKeyFn = unsafe extern "C" fn(i32, *mut c_void, usize) -> u64;

const SYS_QUEUE_WARN_THRESHOLD: usize = 10_000;

#[cfg(test)]
thread_local! {
    static FAIL_MAILBOX_ALLOC_ON_NTH: Cell<usize> = const { Cell::new(usize::MAX) };
}

#[cfg(test)]
pub(crate) struct MailboxAllocFailureGuard;

#[cfg(test)]
impl Drop for MailboxAllocFailureGuard {
    fn drop(&mut self) {
        FAIL_MAILBOX_ALLOC_ON_NTH.with(|slot| slot.set(usize::MAX));
    }
}

#[cfg(test)]
pub(crate) fn fail_mailbox_alloc_on_nth(n: usize) -> MailboxAllocFailureGuard {
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

// ── Message node ────────────────────────────────────────────────────────

/// A single message in a mailbox queue.
///
/// Allocated with [`libc::malloc`] and freed by the caller (or by
/// [`hew_msg_node_free`]).
///
/// ## Envelope discriminator (Phase α COW)
///
/// `envelope` is `null` for legacy copy-mode nodes (the existing
/// `data` / `data_size` / `libc::memcpy` path). When `envelope` is
/// non-null the payload lives behind a refcounted [`HewMsgEnvelope`]
/// and `data` / `data_size` are unused — `hew_msg_node_free` releases
/// the envelope instead of `libc::free`-ing `data`.
///
/// The legacy and envelope paths coexist; codegen flips sites to the
/// envelope path in a later commit. This commit is strictly additive:
/// every existing allocator zero-initialises `envelope`, so all current
/// nodes take the legacy free path.
#[repr(C)]
#[derive(Debug)]
pub struct HewMsgNode {
    /// Intrusive MPSC next-pointer — must be the first field so that
    /// `*mut HewMsgNode` can be cast to/from `*mut MpscNode`.
    pub next: AtomicPtr<HewMsgNode>,
    /// Application-defined message type tag.
    pub msg_type: i32,
    /// Pointer to deep-copied message payload (malloc'd) on the legacy
    /// copy path. Unused (and may be null) when `envelope` is non-null.
    pub data: *mut c_void,
    /// Size of `data` in bytes on the legacy copy path. Unused on the
    /// envelope path.
    pub data_size: usize,
    /// Optional reply channel for the ask pattern (unused by mailbox).
    pub reply_channel: *mut c_void,
    /// Phase-α COW envelope discriminator. Null = legacy copy-mode
    /// (use `data` / `data_size`); non-null = refcounted envelope path
    /// (`hew_msg_envelope_release` on free).
    pub envelope: *mut HewMsgEnvelope,
    /// Trace context captured when the message was enqueued.
    pub trace_context: HewTraceContext,
}

// ── Phase-α COW message envelope ────────────────────────────────────────
//
// `HewMsgEnvelope` is a refcounted container for actor message payloads.
// Today, every actor send `libc::memcpy`s the payload bytes into a fresh
// buffer; under the COW envelope, the receiver borrows the sender's
// already-owned payload by bumping the envelope's refcount, and the
// sender's binding is invalidated by the move-checker so no observable
// alias remains. The fork-on-write path exists for completeness but is
// expected to be cold — the move-checker rejects observable writes after
// send for non-`Copy` types, so a runtime fork only fires for the
// narrow corner cases the static analysis cannot prove safe.
//
// This commit lands the envelope FFI surface and reserved header bits.
// No call site calls into the envelope path yet; codegen flips selected
// sites in a later commit.

/// Header bit: ≥2 observers hold this envelope (sender + receiver).
pub const HEW_MSG_ENVELOPE_ALIAS_ACTIVE: u32 = 1 << 0;
/// Header bit: payload is `Frozen`; never forks (γ uses).
pub const HEW_MSG_ENVELOPE_SHARED_FROZEN: u32 = 1 << 1;
/// Header bit: payload was bumped from a per-dispatch arena (δ uses).
pub const HEW_MSG_ENVELOPE_ARENA_BACKED: u32 = 1 << 2;
/// Header bit: a fork-on-write has fired (diagnostic / metric).
pub const HEW_MSG_ENVELOPE_FORKED: u32 = 1 << 3;
/// Header bit: payload is a §12 capability transfer; alias forbidden.
pub const HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER: u32 = 1 << 4;
/// Reserved for γ (captured by ≥2 `task_scope` children).
pub const HEW_MSG_ENVELOPE_RESERVED_GAMMA_A: u32 = 1 << 5;
/// Reserved for γ aux.
pub const HEW_MSG_ENVELOPE_RESERVED_GAMMA_B: u32 = 1 << 6;
/// Reserved for δ aux.
pub const HEW_MSG_ENVELOPE_RESERVED_DELTA_A: u32 = 1 << 7;
/// Reserved for δ aux.
pub const HEW_MSG_ENVELOPE_RESERVED_DELTA_B: u32 = 1 << 8;
/// All bits ≥ 9 must read zero on every envelope load (fail-closed).
pub const HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK: u32 = !((1u32 << 9) - 1);

/// Drop glue invoked when an envelope's refcount drops to zero.
///
/// Receives the payload pointer; runs destructors on the payload's
/// fields. The envelope's release path calls this BEFORE freeing the
/// payload buffer itself, mirroring how `hew_msg_node_free` walked
/// drop responsibilities for legacy nodes.
pub type HewMsgEnvelopeDropFn = unsafe extern "C" fn(*mut c_void);

/// Refcounted COW envelope for actor message payloads.
///
/// All envelope FFI functions take `*mut HewMsgEnvelope`. The struct
/// is `#[repr(C)]` so codegen can reference its layout; the fields
/// are `pub` for the same reason but should only be read/written
/// through the FFI surface.
#[repr(C)]
pub struct HewMsgEnvelope {
    /// Number of live observers. Released atomically; payload + drop
    /// glue fire on the transition to zero.
    pub refcount: AtomicUsize,
    /// Header bits — see `HEW_MSG_ENVELOPE_*` constants.
    pub header_bits: std::sync::atomic::AtomicU32,
    /// Pointer to the heap-allocated payload bytes (malloc'd).
    pub payload: *mut c_void,
    /// Size of `payload` in bytes.
    pub payload_size: usize,
    /// Optional drop glue invoked once the refcount drops to zero,
    /// before the envelope frees the payload buffer.
    pub drop_glue: Option<HewMsgEnvelopeDropFn>,
}

impl std::fmt::Debug for HewMsgEnvelope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewMsgEnvelope")
            .field("refcount", &self.refcount.load(Ordering::Relaxed))
            .field("header_bits", &self.header_bits.load(Ordering::Relaxed))
            .field("payload", &self.payload)
            .field("payload_size", &self.payload_size)
            .field("drop_glue_set", &self.drop_glue.is_some())
            .finish()
    }
}

// SAFETY: the envelope's mutable state is the atomics on refcount and
// header_bits; payload bytes are only read while ≥1 observer holds
// the envelope, and codegen guarantees the alias contract (read-only).
// The raw `*mut c_void` payload pointer is opaque to Rust here — the
// only reads/writes through it are by the receiver (read-only) and
// the drop_glue invoked under exclusive last-release ownership.
unsafe impl Send for HewMsgEnvelope {}
// SAFETY: see the `Send` impl above; concurrent reads are read-only,
// concurrent refcount/header mutations go through atomics.
unsafe impl Sync for HewMsgEnvelope {}

/// Validate that bits 9..31 of `bits` are zero; panic on mismatch.
///
/// This is the `serializer-fail-closed` invariant for the envelope
/// header: any forwards-incompatible bit set by a future runtime
/// would otherwise be interpreted as zero by today's release path,
/// silently dropping a contract. Loud panic instead.
#[inline]
fn header_validate(bits: u32) -> u32 {
    assert!(
        bits & HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK == 0,
        "hew_msg_envelope: reserved header bits set (bits = {bits:#x}); \
         this runtime does not understand the envelope's contract — \
         refusing to proceed (fail-closed)"
    );
    bits
}

/// Allocate a fresh envelope wrapping `payload`.
///
/// The envelope takes ownership of `payload` (which must be a pointer
/// returned by `libc::malloc` or compatible) and of running `drop_glue`
/// before freeing the buffer. Initial refcount is 1. Initial header
/// bits are 0 (no alias active until `clone_alias` bumps the refcount).
///
/// Returns null on OOM.
///
/// # Safety
///
/// `payload` must be a `libc::malloc`-allocated buffer of at least
/// `payload_size` bytes, or null when `payload_size` is 0. After this
/// call the envelope owns the buffer; the caller must not free it
/// directly.
///
/// `drop_glue`, when set, is called exactly once with the payload
/// pointer when the refcount drops to zero. It must run destructors
/// only — the envelope frees the buffer afterwards.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_new(
    payload: *mut c_void,
    payload_size: usize,
    drop_glue: Option<HewMsgEnvelopeDropFn>,
) -> *mut HewMsgEnvelope {
    let env = mailbox_malloc(std::mem::size_of::<HewMsgEnvelope>()).cast::<HewMsgEnvelope>();
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: `env` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*env).refcount, AtomicUsize::new(1));
        ptr::write(
            &raw mut (*env).header_bits,
            std::sync::atomic::AtomicU32::new(0),
        );
        (*env).payload = payload;
        (*env).payload_size = payload_size;
        (*env).drop_glue = drop_glue;
    }
    env
}

/// Bump the envelope's refcount and set the `ALIAS_ACTIVE` bit.
///
/// Used by the in-process COW send path: the sender's already-owned
/// payload is wrapped, then `clone_alias` bumps to refcount=2 so both
/// the sender's send-site bookkeeping and the receiver's mailbox node
/// hold one count. (Under symmetric-affine sends — see Q7 — the
/// sender's count is released at the send site immediately, leaving
/// the receiver as the sole observer.)
///
/// Returns the same pointer for caller convenience.
///
/// # Safety
///
/// `env` must be a live envelope obtained from `hew_msg_envelope_new`
/// (or already-cloned). The caller guarantees no concurrent
/// `hew_msg_envelope_release` on the last reference.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_clone_alias(
    env: *mut HewMsgEnvelope,
) -> *mut HewMsgEnvelope {
    cabi_guard!(env.is_null(), ptr::null_mut());
    // SAFETY: `env` is live; refcount + header_bits are atomics.
    unsafe {
        let prev = (*env).refcount.fetch_add(1, Ordering::Relaxed);
        debug_assert!(prev >= 1, "clone_alias on a released envelope");
        (*env)
            .header_bits
            .fetch_or(HEW_MSG_ENVELOPE_ALIAS_ACTIVE, Ordering::Relaxed);
    }
    env
}

/// Drop one observer's reference. Frees the envelope (and runs drop
/// glue + frees the payload) when the count reaches zero.
///
/// Idempotent against a null pointer; calling this twice on the same
/// reference is undefined (the standard refcount contract).
///
/// # Safety
///
/// `env` must be a live envelope; the caller is decrementing exactly
/// one reference they own.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_release(env: *mut HewMsgEnvelope) {
    cabi_guard!(env.is_null());
    // SAFETY: `env` is live; refcount is atomic.
    unsafe {
        let prev = (*env).refcount.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(prev >= 1, "release on a zero-count envelope");
        if prev == 1 {
            // Final release: validate header bits, run drop glue, free
            // the payload buffer, and free the envelope itself.
            let bits = (*env).header_bits.load(Ordering::Acquire);
            header_validate(bits);
            if let Some(drop_fn) = (*env).drop_glue {
                if !(*env).payload.is_null() {
                    drop_fn((*env).payload);
                }
            }
            if !(*env).payload.is_null() {
                libc::free((*env).payload);
            }
            libc::free(env.cast());
        }
    }
}

/// Borrow the payload pointer for read-only access.
///
/// The receiver uses this to read fields off an aliased payload. The
/// returned pointer is borrow-only — the receiver must not free it
/// (the envelope owns the allocation).
///
/// Returns null on a null envelope.
///
/// # Safety
///
/// `env` must be a live envelope. The returned pointer is valid for
/// reads only as long as the caller holds a refcount.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_payload_ptr(env: *mut HewMsgEnvelope) -> *mut c_void {
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: `env` is live; payload is a stable pointer for the life
    // of the envelope (forks allocate a fresh envelope, not a fresh
    // payload on the original).
    unsafe { (*env).payload }
}

/// Fork an envelope for write. Allocates a fresh, single-observer
/// envelope holding a private copy of the payload bytes; decrements
/// the original's refcount; sets `FORKED` on the new envelope.
///
/// Used as the fail-safe write path when codegen could not prove the
/// sender's binding was invalidated post-send. Expected to be cold —
/// the move-checker rejects observable post-send writes for non-`Copy`
/// types. The runtime fork is the safety net for cases the static
/// analysis cannot reason about.
///
/// Returns null on OOM (the original envelope's refcount is
/// unchanged in that case so the caller still owns it).
///
/// # Safety
///
/// `env` must be a live envelope the caller holds a reference on.
/// After a successful fork the caller's reference points to the new
/// envelope; the old reference has been released as part of the fork.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_fork_for_write(
    env: *mut HewMsgEnvelope,
) -> *mut HewMsgEnvelope {
    cabi_guard!(env.is_null(), ptr::null_mut());

    // SAFETY: `env` is live; we read payload size + bytes under the
    // alias contract (read-only) and write into a fresh buffer. We
    // also snapshot `header_bits` so the forked envelope inherits the
    // source's reserved bits (`SHARED_FROZEN`, `CAPABILITY_TRANSFER`,
    // γ/δ reserved bits). `ALIAS_ACTIVE` is intentionally cleared on
    // the fork — the new envelope starts as the sole observer.
    let (payload_size, drop_glue, src_payload, src_bits) = unsafe {
        (
            (*env).payload_size,
            (*env).drop_glue,
            (*env).payload,
            (*env).header_bits.load(Ordering::Relaxed),
        )
    };

    // Allocate fresh payload buffer + memcpy.
    let new_payload = if payload_size > 0 && !src_payload.is_null() {
        let buf = mailbox_malloc(payload_size);
        if buf.is_null() {
            return ptr::null_mut();
        }
        // SAFETY: `buf` is a fresh allocation of `payload_size` bytes;
        // `src_payload` is readable for `payload_size` bytes under
        // the alias contract.
        unsafe { libc::memcpy(buf, src_payload, payload_size) };
        buf
    } else {
        ptr::null_mut()
    };

    // SAFETY: standard envelope-new path. The new envelope inherits
    // payload_size + drop_glue; refcount=1; header_bits initialised
    // to 0 then we set FORKED. `SHARED_FROZEN` / `CAPABILITY_TRANSFER`
    // / γ-δ reserved bits from the source are OR'd in below so the
    // contract bits survive a fork; `ALIAS_ACTIVE` is masked out
    // because the new envelope has only one observer.
    let forked = unsafe { hew_msg_envelope_new(new_payload, payload_size, drop_glue) };
    if forked.is_null() {
        if !new_payload.is_null() {
            // SAFETY: we allocated this buffer above and never published it.
            unsafe { libc::free(new_payload) };
        }
        return ptr::null_mut();
    }
    // Preserve reserved/contract bits from the source (everything
    // except `ALIAS_ACTIVE`) and set `FORKED` on the new envelope.
    let inherited_bits = (src_bits & !HEW_MSG_ENVELOPE_ALIAS_ACTIVE) | HEW_MSG_ENVELOPE_FORKED;
    // SAFETY: `forked` is a live envelope we just created.
    unsafe {
        (*forked)
            .header_bits
            .fetch_or(inherited_bits, Ordering::Relaxed);
    }

    // Release the caller's old reference; the new envelope replaces it.
    // SAFETY: caller transferred their reference into this call.
    unsafe { hew_msg_envelope_release(env) };

    forked
}

/// Allocate a [`HewMsgNode`] via `libc::malloc`, deep-copying `data`.
///
/// # Safety
///
/// `data` must point to at least `data_size` readable bytes, or be null
/// when `data_size` is 0.
unsafe fn msg_node_alloc(
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
    reply_channel: *mut c_void,
) -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) — POD-like struct, no drop glue.
    let node = mailbox_malloc(std::mem::size_of::<HewMsgNode>()).cast::<HewMsgNode>();
    if node.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = msg_type;
        (*node).data_size = data_size;
        (*node).reply_channel = reply_channel;
        // Phase-α: legacy copy path nodes hold no envelope. Codegen
        // flips selected sites to `msg_node_alloc_aliased` later.
        (*node).envelope = ptr::null_mut();
        (*node).trace_context = crate::tracing::current_context();

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

/// Allocate a [`HewMsgNode`] that takes the Phase-α envelope path
/// instead of `libc::memcpy`'ing `data` into a fresh buffer.
///
/// The caller transfers one refcount on `envelope` to the new node;
/// on `hew_msg_node_free` the node releases that refcount via
/// [`hew_msg_envelope_release`].
///
/// # Safety
///
/// `envelope` must be a live envelope obtained from
/// [`hew_msg_envelope_new`] (or whose refcount the caller has bumped
/// via [`hew_msg_envelope_clone_alias`]). The caller must not release
/// its own reference if it intends the new node to own it; this
/// function consumes one refcount.
///
/// Phase α: unused in production paths because both
/// `hew_actor_send_aliased` and `hew_mailbox_send_aliased` are
/// fail-closed.  Preserved for Phase β re-enable and exercised by
/// the in-tree test `aliased_node_alloc_consumes_envelope_refcount`.
#[allow(
    dead_code,
    reason = "Phase α: alias send is fail-closed; preserved for Phase β re-enable"
)]
unsafe fn msg_node_alloc_aliased(
    msg_type: i32,
    envelope: *mut HewMsgEnvelope,
    reply_channel: *mut c_void,
) -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) — POD-like struct, no drop glue.
    let node = mailbox_malloc(std::mem::size_of::<HewMsgNode>()).cast::<HewMsgNode>();
    if node.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = msg_type;
        // Legacy fields unused on the envelope path; zero them so any
        // accidental read fails predictably.
        (*node).data = ptr::null_mut();
        (*node).data_size = 0;
        (*node).reply_channel = reply_channel;
        (*node).envelope = envelope;
        (*node).trace_context = crate::tracing::current_context();
    }

    node
}

unsafe fn retire_orphaned_ask_sender_ref(reply_channel: *mut c_void) {
    if reply_channel.is_null() {
        return;
    }

    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: native mailboxes own one sender-side reply reference per ask they still own.
    unsafe {
        crate::reply_channel::hew_reply_channel_retire_orphaned_ask_sender_ref(
            reply_channel.cast(),
        );
    }
    #[cfg(target_arch = "wasm32")]
    // SAFETY: WASM keeps the existing empty-reply teardown behaviour for parity.
    unsafe {
        let _ = crate::reply_channel_wasm::hew_reply(reply_channel.cast(), ptr::null_mut(), 0);
    }
}

unsafe fn retire_msg_node_ask_sender_ref(node: *mut HewMsgNode) {
    // SAFETY: caller guarantees exclusive ownership of `node`.
    let reply_channel = unsafe { (*node).reply_channel };
    // SAFETY: caller guarantees exclusive ownership of `node`.
    unsafe {
        (*node).reply_channel = ptr::null_mut();
    }
    // SAFETY: the detached reply channel (if any) belonged to this queued ask node.
    unsafe { retire_orphaned_ask_sender_ref(reply_channel) };
}

/// Free a [`HewMsgNode`] and its payload.
///
/// # Safety
///
/// `node` must have been allocated by [`msg_node_alloc`],
/// [`msg_node_alloc_aliased`], or [`libc::malloc`] with the same
/// layout and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_node_free(node: *mut HewMsgNode) {
    cabi_guard!(node.is_null());
    // SAFETY: Caller guarantees `node` was malloc'd and is exclusively owned.
    unsafe {
        // Explicit orphaned-ask teardown: queued ask nodes own a sender-side
        // reply reference that must be retired before the node memory is freed.
        retire_msg_node_ask_sender_ref(node);
        // Phase-α COW: branch on the envelope discriminator. Legacy
        // nodes hold a malloc'd payload buffer in `data`; envelope
        // nodes drop one refcount on the shared envelope and let the
        // envelope's release path run drop glue + free the payload.
        if (*node).envelope.is_null() {
            libc::free((*node).data);
        } else {
            hew_msg_envelope_release((*node).envelope);
            (*node).envelope = ptr::null_mut();
        }
        libc::free(node.cast());
    }
}

// ── Lock-free MPSC queue ────────────────────────────────────────────────

/// Allocate a sentinel (dummy) node for an intrusive MPSC queue.
///
/// The sentinel has `msg_type = -1` and null data. It is never returned
/// to consumers — it exists only to simplify empty/non-empty transitions.
fn alloc_sentinel() -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) — POD-like struct, no drop glue.
    let node = mailbox_malloc(std::mem::size_of::<HewMsgNode>()).cast::<HewMsgNode>();
    if node.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = -1;
        (*node).data = ptr::null_mut();
        (*node).data_size = 0;
        (*node).reply_channel = ptr::null_mut();
        // Sentinels never carry an envelope payload; zero so that
        // hew_msg_node_free routes through the legacy `libc::free` path.
        (*node).envelope = ptr::null_mut();
        (*node).trace_context = HewTraceContext::default();
    }
    node
}

/// Lock-free MPSC queue using a stable-stub Vyukov-style algorithm.
///
/// Multiple producers enqueue via an atomic swap on `head`. A single
/// consumer dequeues from `tail`. A heap-allocated stub sentinel remains
/// live for the queue's full lifetime so producers never race a freed
/// former sentinel.
struct MpscQueue {
    head: AtomicPtr<HewMsgNode>,
    tail: UnsafeCell<*mut HewMsgNode>,
    stub: *mut HewMsgNode,
}

impl std::fmt::Debug for MpscQueue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // SAFETY: Debug output only snapshots the consumer tail pointer; the
        // queue itself remains responsible for single-consumer access.
        let tail = unsafe { *self.tail.get() };
        f.debug_struct("MpscQueue")
            .field("head", &self.head.load(Ordering::Relaxed))
            .field("tail", &tail)
            .field("stub", &self.stub)
            .finish()
    }
}

// SAFETY: Producers only touch the atomic `head` plus their own node's `next`.
// The single consumer owns `tail` through the queue's API contract.
unsafe impl Sync for MpscQueue {}
// SAFETY: The queue may move between threads; ownership invariants are the same
// as for `Sync`.
unsafe impl Send for MpscQueue {}

#[derive(Debug, PartialEq, Eq)]
enum DequeueState {
    Success(*mut HewMsgNode),
    Empty,
    Inconsistent,
}

impl MpscQueue {
    fn new() -> Option<Self> {
        let stub = alloc_sentinel();
        if stub.is_null() {
            return None;
        }
        Some(Self {
            head: AtomicPtr::new(stub),
            tail: UnsafeCell::new(stub),
            stub,
        })
    }

    #[inline]
    fn stub_ptr(&self) -> *mut HewMsgNode {
        self.stub
    }

    /// Enqueue a node. Safe for concurrent producers.
    ///
    /// # Safety
    ///
    /// `node` must be a valid, exclusively-owned `HewMsgNode` with
    /// `node.next` set to null.
    unsafe fn enqueue(&self, node: *mut HewMsgNode) {
        // SAFETY: `node` is valid and exclusively owned. Set next to null
        // before publishing.
        unsafe { (*node).next.store(ptr::null_mut(), Ordering::Relaxed) };

        let prev = self.head.swap(node, Ordering::AcqRel);
        // SAFETY: `prev` is either the stable stub or a previously-enqueued
        // live node. Linking with Release publishes `node` to the consumer.
        unsafe {
            (*prev).next.store(node, Ordering::Release);
        }
    }

    /// Single-consumer dequeue step. Returns [`DequeueState::Inconsistent`]
    /// when a producer has exchanged `head` but not yet linked `prev.next`.
    unsafe fn try_dequeue_once(&self) -> DequeueState {
        // SAFETY: Single-consumer invariant grants exclusive access to `tail`.
        let tail = unsafe { *self.tail.get() };
        // SAFETY: `tail` is always the stub or a live queued node.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };
        let stub = self.stub_ptr();

        if tail == stub {
            if next.is_null() {
                return DequeueState::Empty;
            }
            // SAFETY: `next` is the first real node after the stub sentinel.
            unsafe { *self.tail.get() = next };
            // SAFETY: `next` became the consumer tail, so pop_inner sees a
            // valid live node under the same single-consumer invariant.
            return unsafe { self.pop_inner(next) };
        }

        if !next.is_null() {
            // SAFETY: advance consumer tail to the successor before returning
            // the current tail node to the caller for freeing.
            unsafe { *self.tail.get() = next };
            return DequeueState::Success(tail);
        }

        let head = self.head.load(Ordering::Acquire);
        if tail != head {
            return DequeueState::Inconsistent;
        }

        // Queue holds a single real node. Re-inject the stable stub so the
        // consumer can pop the last node without ever freeing the sentinel
        // seen by producers.
        // SAFETY: the stable stub stays live for the queue lifetime and may be
        // re-enqueued by the single consumer.
        unsafe { self.enqueue(stub) };
        // SAFETY: `tail` is still live here; either stub linking completed or
        // we observe an in-flight producer and retry.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };
        if !next.is_null() {
            // SAFETY: `next` is the successor just observed from the current
            // consumer tail, so updating the tail preserves the invariant.
            unsafe { *self.tail.get() = next };
            return DequeueState::Success(tail);
        }

        DequeueState::Inconsistent
    }

    /// Helper after advancing past the stub sentinel.
    unsafe fn pop_inner(&self, tail: *mut HewMsgNode) -> DequeueState {
        // SAFETY: `tail` is the first real node after the stub sentinel.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };

        if !next.is_null() {
            // SAFETY: `next` is the successor just observed from the current
            // consumer tail, so updating the tail preserves the invariant.
            unsafe { *self.tail.get() = next };
            return DequeueState::Success(tail);
        }

        let head = self.head.load(Ordering::Acquire);
        if tail != head {
            return DequeueState::Inconsistent;
        }

        let stub = self.stub_ptr();
        // SAFETY: the stable stub stays live for the queue lifetime and may be
        // re-enqueued by the single consumer.
        unsafe { self.enqueue(stub) };
        // SAFETY: `tail` remains live until the caller frees the returned node.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };
        if !next.is_null() {
            // SAFETY: `next` is the successor just observed from the current
            // consumer tail, so updating the tail preserves the invariant.
            unsafe { *self.tail.get() = next };
            return DequeueState::Success(tail);
        }

        DequeueState::Inconsistent
    }

    /// Try to dequeue a node. **Single-consumer only.**
    ///
    /// Returns a dequeued message node, or null if the queue appears empty.
    /// If a producer is briefly mid-enqueue, spin a few times to avoid
    /// reporting a false empty to the caller.
    ///
    /// # Safety
    ///
    /// Only one thread may call this at a time (single-consumer invariant).
    unsafe fn try_dequeue(&self) -> *mut HewMsgNode {
        const SPIN_LIMIT: usize = 64;
        for _ in 0..SPIN_LIMIT {
            // SAFETY: try_dequeue owns the single-consumer contract for the
            // duration of this call and delegates one dequeue step.
            match unsafe { self.try_dequeue_once() } {
                DequeueState::Success(node) => return node,
                DequeueState::Empty => return ptr::null_mut(),
                DequeueState::Inconsistent => std::hint::spin_loop(),
            }
        }
        ptr::null_mut()
    }

    /// Drain and free all remaining nodes (including the sentinel).
    ///
    /// # Safety
    ///
    /// No concurrent access may occur. All nodes must have been allocated
    /// by `msg_node_alloc` (or `alloc_sentinel`).
    unsafe fn drain_and_free(&self) {
        loop {
            // SAFETY: caller guarantees exclusive teardown access, so dequeue
            // may consume until the queue is empty.
            let node = unsafe { self.try_dequeue() };
            if node.is_null() {
                break;
            }
            // Route every node through hew_msg_node_free so that any queued
            // ask/reply channels are retired and their waiters unblocked with
            // an empty reply before the memory is freed.
            // SAFETY: dequeue transferred exclusive ownership of `node`.
            unsafe { hew_msg_node_free(node) };
        }
        // Free the stable stub sentinel last.
        // SAFETY: the stub was heap-allocated at queue creation and is still
        // exclusively owned during teardown.
        unsafe { hew_msg_node_free(self.stub_ptr()) };
    }
}

// ── Mailbox ─────────────────────────────────────────────────────────────

/// Mutex-protected queue used by complex overflow policies that need
/// queue traversal or blocking.
#[derive(Debug)]
struct SlowPathQueue {
    user_queue: VecDeque<*mut HewMsgNode>,
}

// SAFETY: The raw pointers in the queue are only accessed while holding
// the mutex, and each pointer is exclusively owned by the mailbox.
unsafe impl Send for SlowPathQueue {}

/// Returns `true` if the given overflow policy requires the mutex slow path.
const fn needs_slow_path(policy: HewOverflowPolicy) -> bool {
    matches!(
        policy,
        HewOverflowPolicy::Block | HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce
    )
}

/// Dual-queue actor mailbox.
///
/// Uses a lock-free MPSC queue for the fast path (unbounded, `DropNew`,
/// `Fail`) and a `Mutex`-protected `VecDeque` for complex policies
/// (`Block`, `DropOld`, `Coalesce`).
#[derive(Debug)]
pub struct HewMailbox {
    /// Lock-free user message queue (used when `!needs_slow_path`).
    user_fast: MpscQueue,
    /// Lock-free system message queue.
    sys_queue: MpscQueue,
    /// Mutex-protected user queue for Block/DropOld/Coalesce policies.
    slow_path: Mutex<SlowPathQueue>,
    /// Approximate message count for capacity checks.
    pub(crate) count: AtomicI64,
    /// Approximate system-queue message count for observability.
    sys_count: AtomicUsize,
    /// Maximum user-queue capacity (`-1` or `0` = unbounded).
    pub(crate) capacity: i64,
    /// Policy applied when user-queue is at capacity.
    overflow: HewOverflowPolicy,
    /// Optional key extractor used by [`HewOverflowPolicy::Coalesce`].
    coalesce_key_fn: Option<HewCoalesceKeyFn>,
    /// Fallback policy used when coalesce finds no matching key.
    coalesce_fallback: HewOverflowPolicy,
    /// Whether the mailbox has been closed.
    closed: std::sync::atomic::AtomicBool,
    /// Whether a shutdown system message (`msg_type = -1`) has been enqueued.
    stop_signal_sent: std::sync::atomic::AtomicBool,
    /// Condvar notified when a user message is consumed, waking blocked senders.
    not_full: Condvar,
    /// High-water mark: maximum `count` value observed.
    pub(crate) high_water_mark: AtomicI64,
    /// Whether this mailbox uses the slow (mutex) path for user messages.
    use_slow_path: bool,
}

impl HewMailbox {
    /// Read-only accessor: `true` when the mailbox uses the mutex
    /// slow-path queue for user messages (rather than the lock-free
    /// MPSC queue).  Used by the Phase α aliased-send gate (now
    /// fail-closed; preserved for Phase β re-enable).
    #[allow(
        dead_code,
        reason = "Phase α: alias send gate is fail-closed; preserved for Phase β"
    )]
    #[inline]
    pub(crate) fn use_slow_path(&self) -> bool {
        self.use_slow_path
    }
}

/// Update the high-water mark after incrementing `count`.
fn update_high_water_mark(mb: &HewMailbox) {
    let current = mb.count.load(Ordering::Relaxed);
    let mut hwm = mb.high_water_mark.load(Ordering::Relaxed);
    while current > hwm {
        match mb.high_water_mark.compare_exchange_weak(
            hwm,
            current,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => break,
            Err(actual) => hwm = actual,
        }
    }
}

// ── Constructors ────────────────────────────────────────────────────────

/// Create an unbounded mailbox.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new() -> *mut HewMailbox {
    let Some(user_fast) = MpscQueue::new() else {
        return ptr::null_mut();
    };
    let Some(sys_queue) = MpscQueue::new() else {
        // SAFETY: user_fast was just successfully created and has no enqueued nodes yet.
        unsafe { user_fast.drain_and_free() };
        return ptr::null_mut();
    };

    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: -1,
        overflow: HewOverflowPolicy::DropNew,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_signal_sent: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: false,
    }))
}

/// Create a bounded mailbox with the given capacity.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new_bounded(capacity: i32) -> *mut HewMailbox {
    let Some(user_fast) = MpscQueue::new() else {
        return ptr::null_mut();
    };
    let Some(sys_queue) = MpscQueue::new() else {
        // SAFETY: user_fast was just successfully created and has no enqueued nodes yet.
        unsafe { user_fast.drain_and_free() };
        return ptr::null_mut();
    };

    let policy = HewOverflowPolicy::DropNew;
    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: i64::from(capacity),
        overflow: policy,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_signal_sent: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: needs_slow_path(policy),
    }))
}

/// Create a bounded mailbox with the given capacity and overflow policy.
///
/// A `capacity` of `0` creates an unbounded mailbox.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new_with_policy(
    capacity: usize,
    policy: OverflowPolicy,
) -> *mut HewMailbox {
    let Some(user_fast) = MpscQueue::new() else {
        return ptr::null_mut();
    };
    let Some(sys_queue) = MpscQueue::new() else {
        // SAFETY: user_fast was just successfully created and has no enqueued nodes yet.
        unsafe { user_fast.drain_and_free() };
        return ptr::null_mut();
    };

    let cap = if capacity == 0 {
        -1
    } else {
        i64::try_from(capacity).unwrap_or(i64::MAX)
    };
    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: cap,
        overflow: policy,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_signal_sent: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: needs_slow_path(policy),
    }))
}

/// Create a bounded mailbox with the given capacity and the [`Coalesce`](HewOverflowPolicy::Coalesce)
/// overflow policy.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new_coalesce(capacity: u32) -> *mut HewMailbox {
    let Some(user_fast) = MpscQueue::new() else {
        return ptr::null_mut();
    };
    let Some(sys_queue) = MpscQueue::new() else {
        // SAFETY: user_fast was just successfully created and has no enqueued nodes yet.
        unsafe { user_fast.drain_and_free() };
        return ptr::null_mut();
    };

    let cap = i64::from(capacity);
    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: cap,
        overflow: HewOverflowPolicy::Coalesce,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_signal_sent: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: true,
    }))
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
    reply_channel: *mut c_void,
) -> bool {
    // SAFETY: `node` is a valid queue node owned while mailbox lock is held.
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
        if (*node).reply_channel != reply_channel {
            // Keep the queued node's reply channel stable, but retire the
            // superseded incoming waiter so ask callers never hang.
            retire_orphaned_ask_sender_ref(reply_channel);
        }
    }
    true
}

/// Configure coalescing behaviour for a mailbox.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_set_coalesce_config(
    mb: *mut HewMailbox,
    key_fn: Option<HewCoalesceKeyFn>,
    fallback_policy: OverflowPolicy,
) {
    // SAFETY: caller guarantees `mb` is valid.
    let mb = unsafe { &mut *mb };
    mb.coalesce_key_fn = key_fn;
    mb.coalesce_fallback = normalize_coalesce_fallback(fallback_policy);
}

// ── Send (producer side) ────────────────────────────────────────────────

/// Outcome of an overflow-policy-aware send into the user queue.
///
/// FFI entry points map these variants to their own return conventions.
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

/// Core overflow-policy-aware enqueue into the user message queue.
///
/// Handles bounded-capacity checks, all five overflow policies (`Block`,
/// `DropNew`, `Fail`, `DropOld`, `Coalesce` with fallback), and the
/// unbounded fast path. Returns a [`SendOutcome`] that the caller maps
/// to its FFI return convention.
///
/// `drop_old_alloc_under_lock` controls whether the top-level `DropOld`
/// path allocates the new node *after* acquiring the queue lock (`true`,
/// matching [`hew_mailbox_send`]) or *before* (`false`, matching
/// [`hew_mailbox_try_push`]).
///
/// `non_blocking` controls whether `Block` (at both the top-level and the
/// Coalesce fallback) waits on the condvar (`false`) or immediately returns
/// [`SendOutcome::Failed`] (`true`). Set to `true` for [`hew_mailbox_try_send`]
/// to preserve its non-blocking contract while still applying `DropOld` and
/// `Coalesce` policies.
///
/// # Safety
///
/// - `mb` must reference a valid, live [`HewMailbox`].
/// - `data` must point to at least `data_size` readable bytes, or be null
///   when `data_size` is 0.
#[expect(
    clippy::too_many_lines,
    reason = "overflow-policy dispatch is inherently complex — splitting further would scatter the state machine"
)]
unsafe fn send_with_overflow(
    mb: &HewMailbox,
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
    drop_old_alloc_under_lock: bool,
    non_blocking: bool,
    reply_channel: *mut c_void,
) -> SendOutcome {
    if mb.closed.load(Ordering::Acquire) {
        return SendOutcome::Closed;
    }

    // Bounded capacity check.
    if mb.capacity > 0 {
        let cur = mb.count.load(Ordering::Acquire);
        if cur >= mb.capacity {
            match mb.overflow {
                HewOverflowPolicy::DropNew => return SendOutcome::Dropped,
                HewOverflowPolicy::Fail => return SendOutcome::Failed,
                HewOverflowPolicy::Block => {
                    // Non-blocking callers (try_send) must not wait.
                    if non_blocking {
                        return SendOutcome::Failed;
                    }
                    // Wait on condvar until space is available.
                    let mut q = mb.slow_path.lock_or_recover();
                    loop {
                        if mb.closed.load(Ordering::Acquire) {
                            return SendOutcome::Closed;
                        }
                        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                        if len < mb.capacity {
                            break;
                        }
                        q = mb.not_full.wait_or_recover(q);
                    }
                    // SAFETY: `data` validity guaranteed by caller.
                    let node = unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                    if node.is_null() {
                        return SendOutcome::Oom;
                    }
                    q.user_queue.push_back(node);
                    drop(q);
                    mb.count.fetch_add(1, Ordering::Release);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return SendOutcome::Enqueued;
                }
                HewOverflowPolicy::Coalesce => {
                    let mut q = mb.slow_path.lock_or_recover();
                    // Scan for an existing message with the same coalesce key.
                    // SAFETY: `data` validity guaranteed by caller.
                    let incoming_key = unsafe {
                        coalesce_message_key(
                            mb.coalesce_key_fn,
                            msg_type,
                            data.cast_mut(),
                            data_size,
                        )
                    };
                    let found = q
                        .user_queue
                        .iter()
                        .find(|&&n| {
                            // SAFETY: all nodes in the queue were allocated by msg_node_alloc.
                            unsafe {
                                coalesce_message_key(
                                    mb.coalesce_key_fn,
                                    (*n).msg_type,
                                    (*n).data,
                                    (*n).data_size,
                                ) == incoming_key
                            }
                        })
                        .copied();
                    if let Some(existing) = found {
                        // SAFETY: `existing` is valid; replace its payload.
                        let ok = unsafe {
                            replace_node_payload(existing, msg_type, data, data_size, reply_channel)
                        };
                        if !ok {
                            return SendOutcome::Oom;
                        }
                        return SendOutcome::Coalesced;
                    }
                    // No matching key — use configured fallback policy.
                    match normalize_coalesce_fallback(mb.coalesce_fallback) {
                        HewOverflowPolicy::DropNew => return SendOutcome::Dropped,
                        HewOverflowPolicy::Fail => return SendOutcome::Failed,
                        HewOverflowPolicy::Block => {
                            // Non-blocking callers must not wait.
                            if non_blocking {
                                return SendOutcome::Failed;
                            }
                            loop {
                                if mb.closed.load(Ordering::Acquire) {
                                    return SendOutcome::Closed;
                                }
                                let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                                if len < mb.capacity {
                                    break;
                                }
                                q = mb.not_full.wait_or_recover(q);
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node =
                                unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                            if node.is_null() {
                                return SendOutcome::Oom;
                            }
                            q.user_queue.push_back(node);
                            drop(q);
                            mb.count.fetch_add(1, Ordering::Release);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return SendOutcome::Enqueued;
                        }
                        HewOverflowPolicy::DropOld => {
                            // Lock already held from Coalesce scan.
                            if let Some(old) = q.user_queue.pop_front() {
                                // SAFETY: node was allocated by msg_node_alloc.
                                unsafe { hew_msg_node_free(old) };
                                mb.count.fetch_sub(1, Ordering::Release);
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node =
                                unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                            if node.is_null() {
                                return SendOutcome::Oom;
                            }
                            q.user_queue.push_back(node);
                            mb.count.fetch_add(1, Ordering::Release);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return SendOutcome::DroppedOld;
                        }
                        HewOverflowPolicy::Coalesce => unreachable!(),
                    }
                }
                HewOverflowPolicy::DropOld => {
                    if drop_old_alloc_under_lock {
                        // hew_mailbox_send path: lock first, then allocate.
                        let mut q = mb.slow_path.lock_or_recover();
                        if let Some(old) = q.user_queue.pop_front() {
                            // SAFETY: node was allocated by msg_node_alloc.
                            unsafe { hew_msg_node_free(old) };
                            mb.count.fetch_sub(1, Ordering::Release);
                        }
                        // SAFETY: `data` validity guaranteed by caller.
                        let node =
                            unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                        if node.is_null() {
                            return SendOutcome::Oom;
                        }
                        q.user_queue.push_back(node);
                    } else {
                        // hew_mailbox_try_push path: allocate first, then lock.
                        // SAFETY: `data` validity guaranteed by caller.
                        let node =
                            unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                        if node.is_null() {
                            return SendOutcome::Oom;
                        }
                        let mut q = mb.slow_path.lock_or_recover();
                        if let Some(old) = q.user_queue.pop_front() {
                            // SAFETY: node was allocated by msg_node_alloc.
                            unsafe { hew_msg_node_free(old) };
                            mb.count.fetch_sub(1, Ordering::Release);
                        }
                        q.user_queue.push_back(node);
                    }
                    mb.count.fetch_add(1, Ordering::Release);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return SendOutcome::DroppedOld;
                }
            }
        }
    }

    // Fast path: no capacity issue (or unbounded).
    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
    if node.is_null() {
        return SendOutcome::Oom;
    }

    if mb.use_slow_path {
        let mut q = mb.slow_path.lock_or_recover();
        q.user_queue.push_back(node);
    } else {
        // SAFETY: `node` was just allocated with next == null.
        unsafe { mb.user_fast.enqueue(node) };
    }

    mb.count.fetch_add(1, Ordering::Release);
    update_high_water_mark(mb);
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);

    SendOutcome::Enqueued
}

/// Send an envelope-aliased message to the mailbox — **fail-closed in
/// Phase α**.
///
/// # Phase α status — DO NOT CALL
///
/// This entry point is **disabled** in Phase α and aborts via
/// [`hew_panic`] on every invocation (after releasing the
/// caller-transferred envelope refcount, if non-null, so the buffer
/// container does not leak).  The codegen alias lowering is gated off
/// in `ActorSendOpLowering` and the runtime entry point
/// `hew_actor_send_aliased` (the only in-tree caller) is also
/// fail-closed; this function exists solely as a `#[no_mangle]`
/// symbol so external FFI / dlopen consumers that link the alias
/// path get the same fail-closed treatment instead of reaching the
/// underlying ownership hole.
///
/// The JIT host symbol classification (`scripts/jit-symbol-classification.toml`)
/// also lists this symbol under `internal`, not `stable`, so JIT
/// session dylibs cannot link it; this panic is the second layer of
/// the same capability boundary for non-JIT FFI consumers.
///
/// ## Why fail-closed
///
/// `hew_msg_envelope_release` calls `drop_glue` on every final
/// release with no "consumed" flag, so post-dispatch release would
/// double-free fields the receiver moved out of the payload, while a
/// discard-path release with `drop_glue=null` leaks them.  See the
/// full rationale on [`crate::actor::hew_actor_send_aliased`].  A
/// correct Phase β fix introduces a CONSUMED bit on
/// `HewMsgEnvelope::header_bits` set by `hew_msg_node_free` after
/// dispatch, with release skipping `drop_glue` when CONSUMED is set.
///
/// # Safety
///
/// - All parameters may be any value.  The function never dereferences
///   `mb` or `msg_type`; it releases `envelope` (if non-null) via
///   [`hew_msg_envelope_release`] and panics.  The caller's refcount
///   transfer contract is honoured even on this fail-closed path.
///
/// Returns `i32` only for ABI compatibility with the (now disabled)
/// pre-Phase-β shape; the function never actually returns because
/// [`hew_panic`] aborts the calling actor (or exits the process if
/// called outside an actor).
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send_aliased(
    _mb: *mut HewMailbox,
    _msg_type: i32,
    envelope: *mut HewMsgEnvelope,
) -> i32 {
    if !envelope.is_null() {
        // SAFETY: caller transferred one refcount on `envelope` per
        // the alias-send contract.  Release it BEFORE the panic so
        // the buffer container does not leak even if `hew_panic`
        // recovers via longjmp into a supervisor.
        unsafe { hew_msg_envelope_release(envelope) };
    }
    // Phase α capability boundary: see the doc-comment above.  The
    // alias send path cannot deliver a message safely under the
    // current envelope/release contract, so we refuse to enqueue.
    crate::actor::hew_panic();
    // Unreachable — `hew_panic` either longjmps to the actor
    // supervisor or exits the process.
    HewError::ErrActorStopped as i32
}

/// Send a message to the mailbox (user queue), deep-copying `data`.
///
/// Returns `0` ([`HewError::Ok`]) on success, `-1`
/// ([`HewError::ErrMailboxFull`]) if bounded and at capacity,
/// `-2` ([`HewError::ErrActorStopped`]) if the mailbox is closed,
/// or `-5` ([`HewError::ErrOom`]) if allocation fails.
///
/// # Native / WASM divergence
///
/// On native targets this function returns [`HewError::ErrActorStopped`] (`-2`)
/// when the mailbox is closed, matching the actor-layer semantics (the
/// destination actor has stopped).  The non-blocking variant
/// [`hew_mailbox_try_send`] returns [`HewError::ErrClosed`] (`-4`) instead —
/// a deliberate difference that reflects the caller's intent (non-blocking
/// callers get the raw mailbox state, blocking callers get the actor-level
/// error).
///
/// The WASM counterpart (`mailbox_wasm::hew_mailbox_send`) returns
/// [`HewError::ErrClosed`] (`-4`) for both the blocking and non-blocking
/// variants because WASM has no blocking send; the two variants are identical
/// on that target.
///
/// # Safety
///
/// - `mb` must be a valid pointer returned by [`hew_mailbox_new`] or
///   [`hew_mailbox_new_bounded`].
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    // SAFETY: Caller guarantees `data` points to `size` readable bytes.
    match unsafe { send_with_overflow(mb, msg_type, data, size, true, false, ptr::null_mut()) } {
        SendOutcome::Enqueued | SendOutcome::Coalesced | SendOutcome::DroppedOld => {
            HewError::Ok as i32
        }
        SendOutcome::Closed => HewError::ErrActorStopped as i32,
        SendOutcome::Dropped | SendOutcome::Failed => HewError::ErrMailboxFull as i32,
        SendOutcome::Oom => HewError::ErrOom as i32,
    }
}

/// Send a message with an associated reply channel.
///
/// Identical to [`hew_mailbox_send`] but also sets the `reply_channel`
/// field on the allocated message node so the receiver can reply via
/// [`hew_get_reply_channel`](crate::scheduler::hew_get_reply_channel).
///
/// # Safety
///
/// - `mb` must be a valid mailbox pointer.
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
/// - `reply_channel` must be a valid reply channel pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send_with_reply(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    // SAFETY: Caller guarantees `data` points to `size` readable bytes.
    match unsafe { send_with_overflow(mb, msg_type, data, size, true, false, reply_channel) } {
        SendOutcome::Enqueued | SendOutcome::Coalesced | SendOutcome::DroppedOld => {
            HewError::Ok as i32
        }
        SendOutcome::Closed => HewError::ErrActorStopped as i32,
        SendOutcome::Dropped | SendOutcome::Failed => HewError::ErrMailboxFull as i32,
        SendOutcome::Oom => HewError::ErrOom as i32,
    }
}

/// Non-blocking send that applies overflow policies without ever blocking.
///
/// Behaves identically to [`hew_mailbox_send`] for `DropOld` and `Coalesce`
/// policies (the oldest message is evicted / a matching queued message is
/// replaced in-place). For `Block`, `DropNew`, and `Fail` policies the call
/// returns [`HewError::ErrMailboxFull`] immediately rather than waiting.
///
/// Returns `0` ([`HewError::Ok`]) on success (including eviction under
/// `DropOld`/`Coalesce`), `-1` ([`HewError::ErrMailboxFull`]) if the mailbox
/// is full and the policy does not permit eviction, `-4`
/// ([`HewError::ErrClosed`]) if the mailbox is closed, or `-5`
/// ([`HewError::ErrOom`]) if allocation fails.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_send(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    // SAFETY: Caller guarantees `data` points to `size` readable bytes.
    match unsafe { send_with_overflow(mb, msg_type, data, size, false, true, ptr::null_mut()) } {
        SendOutcome::Enqueued | SendOutcome::Coalesced | SendOutcome::DroppedOld => {
            HewError::Ok as i32
        }
        SendOutcome::Closed => HewError::ErrClosed as i32,
        SendOutcome::Dropped | SendOutcome::Failed => HewError::ErrMailboxFull as i32,
        SendOutcome::Oom => HewError::ErrOom as i32,
    }
}

/// Send a system message, bypassing capacity limits.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send_sys(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, size, ptr::null_mut()) };
    if node.is_null() {
        set_last_error(format!(
            "hew_mailbox_send_sys: failed to deliver system message (msg_type={msg_type}, size={size})"
        ));
        eprintln!(
            "hew_mailbox_send_sys: failed to deliver system message (msg_type={msg_type}, size={size})"
        );
        return;
    }

    // SAFETY: `node` is freshly allocated and owned by this mailbox send.
    unsafe { enqueue_sys_node(mb, node) };
}

unsafe fn enqueue_sys_node(mb: &HewMailbox, node: *mut HewMsgNode) {
    // SAFETY: `node` was just allocated with next == null.
    unsafe { mb.sys_queue.enqueue(node) };
    let sys_queue_len = mb.sys_count.fetch_add(1, Ordering::AcqRel) + 1;
    if sys_queue_len > SYS_QUEUE_WARN_THRESHOLD {
        eprintln!("[mailbox] warning: system queue has {sys_queue_len} messages (mailbox {mb:p})");
    }
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
}

pub(crate) unsafe fn mailbox_send_stop_sys_once(mb: *mut HewMailbox) -> bool {
    if mb.is_null() {
        return false;
    }
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    // SAFETY: stop signals carry no payload.
    let node = unsafe { msg_node_alloc(-1, ptr::null(), 0, ptr::null_mut()) };
    if node.is_null() {
        set_last_error("hew_actor_stop: failed to enqueue shutdown system message");
        eprintln!("hew_actor_stop: failed to enqueue shutdown system message");
        return false;
    }

    if mb
        .stop_signal_sent
        .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        // SAFETY: `node` was allocated above and was not published to the queue.
        unsafe { hew_msg_node_free(node) };
        return false;
    }

    // SAFETY: `node` is freshly allocated and now owned by the system queue.
    unsafe { enqueue_sys_node(mb, node) };
    true
}

/// Policy-aware push into the user queue.
///
/// Returns `0` on success, `1` if the message was dropped (`DropNew` policy),
/// `2` if the oldest message was dropped (`DropOld` policy), `3` if coalesced,
/// or `-1` on failure (including OOM).
///
/// # Safety
///
/// - `mb` must be a valid mailbox pointer.
/// - `data` must point to at least `data_size` readable bytes, or be null
///   when `data_size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_push(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mbr = unsafe { &*mb };
    // SAFETY: Caller guarantees `data` points to `data_size` readable bytes.
    match unsafe {
        send_with_overflow(
            mbr,
            msg_type,
            data,
            data_size,
            false,
            false,
            ptr::null_mut(),
        )
    } {
        SendOutcome::Enqueued => 0,
        SendOutcome::Dropped => 1,
        SendOutcome::DroppedOld => 2,
        SendOutcome::Coalesced => 3,
        SendOutcome::Closed | SendOutcome::Failed | SendOutcome::Oom => -1,
    }
}

// ── Close ───────────────────────────────────────────────────────────────

/// Close a mailbox so that future sends are rejected.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
pub(crate) unsafe fn mailbox_close(mb: *mut HewMailbox) {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    if !mb.closed.swap(true, Ordering::AcqRel) {
        // Wake any senders blocked on a full mailbox.
        mb.not_full.notify_all();
    }
}

/// Returns `true` if the mailbox has been closed.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
pub(crate) unsafe fn mailbox_is_closed(mb: *mut HewMailbox) -> bool {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    mb.closed.load(Ordering::Acquire)
}

// ── Receive (consumer side) ─────────────────────────────────────────────

/// Try to receive a message. System messages have priority.
///
/// Returns a pointer to a [`HewMsgNode`] on success, or null if both
/// queues are empty. The caller owns the returned node and must free it
/// with [`hew_msg_node_free`].
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer. Only one thread may call recv
/// functions at a time (single-consumer invariant).
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_recv(mb: *mut HewMailbox) -> *mut HewMsgNode {
    // SAFETY: Caller guarantees `mb` is valid and single-consumer.
    let mb = unsafe { &*mb };

    // System messages have priority (lock-free dequeue).
    // SAFETY: single-consumer invariant satisfied by caller.
    let sys_node = unsafe { mb.sys_queue.try_dequeue() };
    if !sys_node.is_null() {
        mb.sys_count.fetch_sub(1, Ordering::AcqRel);
        MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
        return sys_node;
    }

    // User messages: slow path uses mutex, fast path uses lock-free queue.
    if mb.use_slow_path {
        let mut q = mb.slow_path.lock_or_recover();
        if let Some(node) = q.user_queue.pop_front() {
            mb.count.fetch_sub(1, Ordering::Release);
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            drop(q);
            mb.not_full.notify_one();
            return node;
        }
    } else {
        // SAFETY: single-consumer invariant satisfied by caller.
        let node = unsafe { mb.user_fast.try_dequeue() };
        if !node.is_null() {
            mb.count.fetch_sub(1, Ordering::Release);
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            return node;
        }
    }

    ptr::null_mut()
}

/// Try to receive a system message only.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_try_recv`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_recv_sys(mb: *mut HewMailbox) -> *mut HewMsgNode {
    // SAFETY: Caller guarantees `mb` is valid and single-consumer.
    let mb = unsafe { &*mb };

    // SAFETY: single-consumer invariant satisfied by caller.
    let node = unsafe { mb.sys_queue.try_dequeue() };
    if !node.is_null() {
        mb.sys_count.fetch_sub(1, Ordering::AcqRel);
        MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
        return node;
    }

    ptr::null_mut()
}

// ── Queries ─────────────────────────────────────────────────────────────

/// Returns `1` if either queue has messages, `0` otherwise.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_has_messages(mb: *mut HewMailbox) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    if mb.sys_count.load(Ordering::Acquire) > 0 {
        return 1;
    }

    if mb.use_slow_path {
        let q = mb.slow_path.lock_or_recover();
        i32::from(!q.user_queue.is_empty())
    } else {
        i32::from(mb.count.load(Ordering::Acquire) > 0)
    }
}

/// Return the number of user messages in the mailbox.
/// Use [`hew_mailbox_sys_len`] to observe system-message backlog.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_len(mb: *const HewMailbox) -> usize {
    // SAFETY: Caller guarantees `mb` is valid.
    let count = unsafe { &*mb }.count.load(Ordering::Acquire);
    usize::try_from(count).unwrap_or(0)
}

/// Return the number of system messages in the mailbox.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_sys_len(mb: *const HewMailbox) -> usize {
    // SAFETY: Caller guarantees `mb` is valid.
    unsafe { &*mb }.sys_count.load(Ordering::Acquire)
}

/// Return the mailbox capacity. Returns `0` for unbounded mailboxes.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_capacity(mb: *const HewMailbox) -> usize {
    // SAFETY: Caller guarantees `mb` is valid.
    let cap = unsafe { &*mb }.capacity;
    usize::try_from(cap).unwrap_or(0)
}

// ── Cleanup ─────────────────────────────────────────────────────────────

/// Free the mailbox, draining and freeing all remaining messages.
///
/// # Safety
///
/// `mb` must have been returned by [`hew_mailbox_new`] or
/// [`hew_mailbox_new_bounded`] and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_free(mb: *mut HewMailbox) {
    cabi_guard!(mb.is_null());

    // SAFETY: Caller guarantees `mb` was Box-allocated and is exclusively owned.
    let mailbox = unsafe { Box::from_raw(mb) };

    // Drain slow-path user queue (if used).
    {
        let mut q = mailbox.slow_path.lock_or_recover();
        while let Some(node) = q.user_queue.pop_front() {
            // SAFETY: Each node was allocated by `msg_node_alloc`.
            unsafe { hew_msg_node_free(node) };
        }
    }

    // Drain lock-free user queue (sentinel + any remaining nodes).
    // SAFETY: No concurrent access — mailbox is exclusively owned.
    unsafe { mailbox.user_fast.drain_and_free() };

    // Drain lock-free system queue (sentinel + any remaining nodes).
    // SAFETY: No concurrent access — mailbox is exclusively owned.
    unsafe { mailbox.sys_queue.drain_and_free() };
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct PriceUpdate {
        symbol: u64,
        price: i32,
    }

    unsafe extern "C" fn price_symbol_key(
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) -> u64 {
        if data.is_null() || data_size < size_of::<PriceUpdate>() {
            return 0;
        }
        // SAFETY: caller passes a valid PriceUpdate payload.
        unsafe { (*data.cast::<PriceUpdate>()).symbol }
    }

    #[test]
    fn new_mailbox_is_empty() {
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_has_messages(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_and_recv_one() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 42;
            let rc = hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(rc, 0);
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!((*node).data_size, size_of::<i32>());
            assert_eq!(*((*node).data.cast::<i32>()), 42);
            hew_msg_node_free(node);

            assert_eq!(hew_mailbox_has_messages(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn bounded_rejects_overflow() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(2);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            // Third send should fail.
            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_bounded() {
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
    fn try_send_drop_old_policy() {
        // try_send with DropOld should evict the oldest message instead of
        // failing when the mailbox is full.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(2, HewOverflowPolicy::DropOld);
            let a: i32 = 1;
            let b: i32 = 2;
            let c: i32 = 3;

            // Fill the mailbox.
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
            assert_eq!(hew_mailbox_len(mb), 2, "queue length must stay at capacity");

            // Oldest message (a=1) was dropped; b=2 and c=3 remain in order.
            let n1 = hew_mailbox_try_recv(mb);
            assert_eq!(*((*n1).data.cast::<i32>()), 2);
            hew_msg_node_free(n1);

            let n2 = hew_mailbox_try_recv(mb);
            assert_eq!(*((*n2).data.cast::<i32>()), 3);
            hew_msg_node_free(n2);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_coalesce_policy() {
        // try_send with Coalesce should replace a matching queued message when
        // the mailbox is full, rather than failing.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            // Use the symbol field as the coalesce key.
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let first = PriceUpdate {
                symbol: 42,
                price: 10,
            };
            let updated = PriceUpdate {
                symbol: 42,
                price: 99,
            };

            // Enqueue the first message.
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            // Full — same key: should coalesce (replace payload) and return Ok.
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
            // Queue length unchanged — only one entry.
            assert_eq!(hew_mailbox_len(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            let got = *((*node).data.cast::<PriceUpdate>());
            assert_eq!(
                got.price, 99,
                "coalesced message must have the updated payload"
            );
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_retires_superseded_ask_without_stealing_existing_waiter() {
        use crate::reply_channel::{
            hew_reply_channel_free, hew_reply_channel_is_ready_for_test, hew_reply_channel_new,
            hew_reply_channel_retain, hew_reply_wait_timeout,
        };

        // SAFETY: test owns the mailbox and reply channels exclusively.
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

            let existing = hew_reply_channel_new();
            let incoming = hew_reply_channel_new();
            assert!(!existing.is_null());
            assert!(!incoming.is_null());

            hew_reply_channel_retain(existing);
            hew_reply_channel_retain(incoming);

            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                    existing.cast(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    2,
                    (&raw const updated).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                    incoming.cast(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_len(mb),
                1,
                "coalesce must keep queue length stable"
            );

            let incoming_reply = hew_reply_wait_timeout(incoming, 1_000);
            assert!(
                incoming_reply.is_null(),
                "superseded ask should observe an empty reply"
            );
            assert!(
                hew_reply_channel_is_ready_for_test(incoming),
                "superseded ask waiter must be retired promptly"
            );
            hew_reply_channel_free(incoming);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!(
                (*node).msg_type,
                2,
                "coalesced node should carry the updated message type"
            );
            let got = *((*node).data.cast::<PriceUpdate>());
            assert_eq!(
                got.price, 99,
                "coalesced node should carry the updated payload"
            );
            assert_eq!(
                (*node).reply_channel as usize,
                existing as usize,
                "coalesced node must keep the original queued ask waiter"
            );
            assert!(
                !hew_reply_channel_is_ready_for_test(existing),
                "original waiter must remain pending until the queued node retires"
            );

            hew_msg_node_free(node);

            let existing_reply = hew_reply_wait_timeout(existing, 1_000);
            assert!(
                existing_reply.is_null(),
                "retiring the queued node should unblock the original waiter with an empty reply"
            );
            assert!(
                hew_reply_channel_is_ready_for_test(existing),
                "original waiter must observe queued-node retirement"
            );
            hew_reply_channel_free(existing);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_block_policy_fails_immediately() {
        // Block policy must fail immediately on try_send (non-blocking contract).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::Ok as i32
            );
            // Full with Block policy — must return ErrMailboxFull immediately.
            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32,
                "try_send with Block must fail immediately, not block"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_closed_returns_err_closed() {
        // try_send on a closed mailbox must return ErrClosed (-4), matching the
        // WASM mailbox parity contract (not ErrActorStopped).
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            mailbox_close(mb);

            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrClosed as i32,
                "try_send on closed mailbox must return ErrClosed, not ErrActorStopped"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_closed_returns_err_actor_stopped() {
        // hew_mailbox_send (blocking variant) on a closed native mailbox must
        // return ErrActorStopped (-2), NOT ErrClosed (-4).
        //
        // Intentional native/WASM divergence:
        //   native hew_mailbox_send      → ErrActorStopped (-2)
        //   native hew_mailbox_try_send  → ErrClosed       (-4)
        //   WASM   hew_mailbox_send      → ErrClosed       (-4)
        //
        // The blocking send surfaces the actor-layer error; the non-blocking
        // variant surfaces the raw mailbox state.  WASM has no blocking send so
        // both variants use ErrClosed there.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            mailbox_close(mb);

            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrActorStopped as i32,
                "hew_mailbox_send on closed mailbox must return ErrActorStopped, not ErrClosed"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_coalesce_block_fallback_no_match_fails_immediately() {
        // Coalesce with a Block fallback and no matching key: try_send must
        // return ErrMailboxFull without blocking.  This exercises the second
        // `if non_blocking` guard inside send_with_overflow's Coalesce fallback
        // arm — the path that is dead for hew_mailbox_send (blocking allowed)
        // but live for hew_mailbox_try_send (non-blocking required).
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
            hew_msg_node_free(node);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_messages_have_priority() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let u: i32 = 10;
            let s: i32 = 99;

            // Send a user message first, then a system message.
            hew_mailbox_send(mb, 1, (&raw const u).cast_mut().cast(), size_of::<i32>());
            hew_mailbox_send_sys(mb, 2, (&raw const s).cast_mut().cast(), size_of::<i32>());

            // Recv should return the system message first.
            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn recv_sys_only() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 5;
            let p = (&raw const val).cast_mut().cast();

            hew_mailbox_send(mb, 1, p, size_of::<i32>());
            hew_mailbox_send_sys(mb, 2, p, size_of::<i32>());

            // try_recv_sys should only return system messages.
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);

            // No more system messages.
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(node.is_null());

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn recv_empty_returns_null() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            assert!(hew_mailbox_try_recv(mb).is_null());
            assert!(hew_mailbox_try_recv_sys(mb).is_null());
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
            hew_msg_node_free(node);

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
    fn coalesce_uses_configured_key_fn() {
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
                3
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 300);
            let payload = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*payload).symbol, 7);
            assert_eq!((*payload).price, 99);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_uses_configured_fallback_policy() {
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
                1
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            hew_msg_node_free(node);

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
            hew_msg_node_free(node);

            // Suppress unused-value warning.
            let _ = val;

            hew_mailbox_free(mb);
        }
    }

    unsafe fn assert_mailbox_free_unblocks_reply_waiter(mb: *mut HewMailbox) {
        use crate::reply_channel::{
            hew_reply_channel_free, hew_reply_channel_is_ready_for_test, hew_reply_channel_new,
            hew_reply_channel_retain, hew_reply_wait_timeout,
        };
        use std::sync::{Arc, Barrier};
        use std::thread;
        use std::time::Duration;

        // SAFETY: all raw pointers are valid; ownership is carefully tracked.
        unsafe {
            // Allocate a reply channel. refs=1 (owned by the waiter side).
            let ch = hew_reply_channel_new();
            assert!(!ch.is_null());

            // Retain a second reference for the message node's "sender" slot.
            // refs=2 after this call.
            hew_reply_channel_retain(ch);

            // Enqueue a message with the reply channel attached without
            // dispatching it, simulating an actor that is freed before it can
            // process the message.
            let rc = hew_mailbox_send_with_reply(mb, 1, ptr::null_mut(), 0, ch.cast());
            assert_eq!(rc, 0, "send_with_reply should succeed");

            // Barrier so the waiter thread is definitely blocking before we
            // free the mailbox.
            let barrier = Arc::new(Barrier::new(2));
            let barrier_clone = barrier.clone();

            // Encode the channel pointer as usize so the closure is Send
            // (usize: Send; *mut T: !Send). The pointer remains valid for the
            // life of the test because we hold the waiter-side reference.
            let ch_addr: usize = ch as usize;

            // Waiter thread: waits with a timeout so a regression fails fast
            // instead of hanging the test process until the job-level timeout.
            let waiter = thread::spawn(move || {
                barrier_clone.wait();
                // SAFETY: ch_addr encodes a valid HewReplyChannel pointer;
                // single-reader guarantee holds since only this thread calls
                // hew_reply_wait_timeout on this channel. Outer unsafe block covers
                // this closure.
                let ch_ptr = ch_addr as *mut crate::reply_channel::HewReplyChannel;
                let val = hew_reply_wait_timeout(ch_ptr, 1_000);
                let observed_reply = hew_reply_channel_is_ready_for_test(ch_ptr);
                // hew_msg_node_free sends an empty reply (null, 0), so val
                // must be null.
                let got_null = val.is_null();
                // Release the waiter's reference (refs: 1→0 → freed).
                hew_reply_channel_free(ch_ptr);
                (got_null, observed_reply)
            });

            // Let the waiter reach hew_reply_wait before we tear down.
            barrier.wait();
            // Small yield so the waiter thread has time to enter the condvar.
            thread::sleep(Duration::from_millis(5));

            // Free the mailbox. With the fix, drain_and_free() calls
            // hew_msg_node_free which calls hew_reply() to unblock the waiter.
            hew_mailbox_free(mb);

            let (got_null, observed_reply) = waiter.join().expect("waiter thread panicked");
            assert!(
                observed_reply,
                "reply waiter timed out instead of observing the mailbox teardown reply"
            );
            assert!(
                got_null,
                "reply waiter must receive a null/empty reply when mailbox is freed with a queued ask node"
            );
        }
    }

    // Regression test: fast-path mailbox teardown must retire queued
    // reply-bearing nodes via hew_msg_node_free so that ask waiters are
    // unblocked promptly rather than blocking until timeout.
    #[test]
    fn drain_and_free_unblocks_reply_waiter() {
        // SAFETY: helper fully owns the mailbox pointer for the duration.
        unsafe {
            let mb = hew_mailbox_new();
            assert!(
                !(*mb).use_slow_path,
                "unbounded mailbox should use fast path"
            );
            assert_mailbox_free_unblocks_reply_waiter(mb);
        }
    }

    #[test]
    fn slow_path_mailbox_free_unblocks_reply_waiter() {
        // SAFETY: helper fully owns the mailbox pointer for the duration.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::DropOld);
            assert!((*mb).use_slow_path, "DropOld mailbox should use slow path");
            assert_mailbox_free_unblocks_reply_waiter(mb);
        }
    }

    #[test]
    fn fast_path_queue_tolerates_concurrent_producers_and_immediate_frees() {
        use std::sync::{Arc, Barrier};
        use std::thread;

        const PRODUCERS: usize = 8;
        const PER_PRODUCER: usize = 4_000;
        const TOTAL: usize = PRODUCERS * PER_PRODUCER;

        for _round in 0..4 {
            let q = Arc::new(MpscQueue::new().expect("queue allocation should succeed"));
            let start = Arc::new(Barrier::new(PRODUCERS + 1));
            let mut handles = Vec::new();

            for producer_id in 0..PRODUCERS {
                let q = Arc::clone(&q);
                let start = Arc::clone(&start);
                handles.push(thread::spawn(move || {
                    start.wait();
                    for seq in 0..PER_PRODUCER {
                        let msg_type = i32::try_from(producer_id * PER_PRODUCER + seq)
                            .expect("test payload id fits in i32");
                        // SAFETY: null payload + zero size is a valid message.
                        let node =
                            unsafe { msg_node_alloc(msg_type, ptr::null(), 0, ptr::null_mut()) };
                        assert!(!node.is_null(), "message allocation must succeed");
                        // SAFETY: each node is freshly allocated and exclusively
                        // owned by this producer until publish.
                        unsafe { q.enqueue(node) };
                        if seq % 64 == 0 {
                            thread::yield_now();
                        }
                    }
                }));
            }

            start.wait();

            let mut consumed = 0;
            while consumed < TOTAL {
                // SAFETY: single-consumer loop owns dequeue access.
                let node = unsafe { q.try_dequeue() };
                if node.is_null() {
                    thread::yield_now();
                    continue;
                }
                consumed += 1;
                // SAFETY: the queue handed ownership of `node` to the consumer.
                unsafe { hew_msg_node_free(node) };
                if consumed % 1024 == 0 {
                    thread::yield_now();
                }
            }

            for handle in handles {
                handle.join().expect("producer thread panicked");
            }

            assert_eq!(
                consumed, TOTAL,
                "consumer must observe every published node"
            );

            let q = Arc::try_unwrap(q).expect("queue still shared after producers joined");
            // SAFETY: queue is exclusively owned and should contain only the stub.
            unsafe { q.drain_and_free() };
        }
    }

    #[test]
    fn trace_context_preserved_in_dequeue() {
        // Test that trace_context is properly copied during dequeue
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 42;

            // Create a custom trace context
            let custom_trace = HewTraceContext {
                trace_id_hi: 0xDEAD_BEEF_1234_5678,
                trace_id_lo: 0x9ABC_DEF0_1234_5678,
                span_id: 0x1122_3344_5566_7788,
                parent_span_id: 0xAABB_CCDD_EEFF_0011,
                flags: 0x01, // sampled
            };

            let _ctx = TestExecutionContext::install(HewExecutionContext::default());
            crate::tracing::set_context(custom_trace);

            // Send a message (which should capture the current trace context)
            let rc = hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(rc, 0);

            // Receive the message
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());

            // Verify the trace context was preserved
            assert_eq!((*node).trace_context.trace_id_hi, 0xDEAD_BEEF_1234_5678);
            assert_eq!((*node).trace_context.trace_id_lo, 0x9ABC_DEF0_1234_5678);
            assert_eq!((*node).trace_context.span_id, 0x1122_3344_5566_7788);
            assert_eq!((*node).trace_context.parent_span_id, 0xAABB_CCDD_EEFF_0011);
            assert_eq!((*node).trace_context.flags, 0x01);

            hew_msg_node_free(node);
            hew_mailbox_free(mb);
        }
    }

    // ── Phase-α envelope tests ──────────────────────────────────────

    /// Reserved-bit layout is documented and tested. Any reordering
    /// here will be caught at compile-time on the constants and at
    /// run-time on the assertion.
    #[test]
    fn envelope_header_bits_layout_locked() {
        assert_eq!(HEW_MSG_ENVELOPE_ALIAS_ACTIVE, 1u32 << 0);
        assert_eq!(HEW_MSG_ENVELOPE_SHARED_FROZEN, 1u32 << 1);
        assert_eq!(HEW_MSG_ENVELOPE_ARENA_BACKED, 1u32 << 2);
        assert_eq!(HEW_MSG_ENVELOPE_FORKED, 1u32 << 3);
        assert_eq!(HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER, 1u32 << 4);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_GAMMA_A, 1u32 << 5);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_GAMMA_B, 1u32 << 6);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_DELTA_A, 1u32 << 7);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_DELTA_B, 1u32 << 8);
        // Bits 9..31 are reserved-zero.
        assert_eq!(HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK, 0xFFFF_FE00);
    }

    /// Drop-glue probe used by envelope tests; bumps a counter so the
    /// test can assert that the final release fires `drop_glue` exactly
    /// once. Each test that touches this state takes
    /// `ENVELOPE_DROP_LOCK` to serialise — the counter is process-wide.
    static ENVELOPE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
    static ENVELOPE_DROP_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C" fn envelope_test_drop_glue(_payload: *mut c_void) {
        ENVELOPE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    fn alloc_test_payload(bytes: &[u8]) -> *mut c_void {
        // SAFETY: malloc + memcpy under the standard contract.
        unsafe {
            let buf = libc::malloc(bytes.len());
            assert!(!buf.is_null());
            libc::memcpy(buf, bytes.as_ptr().cast(), bytes.len());
            buf
        }
    }

    #[test]
    fn envelope_new_starts_at_refcount_one_no_alias() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope-new + release contract.
        unsafe {
            let payload = alloc_test_payload(b"hello envelope");
            let env = hew_msg_envelope_new(payload, 14, Some(envelope_test_drop_glue));
            assert!(!env.is_null());
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);
            assert_eq!((*env).header_bits.load(Ordering::SeqCst), 0);
            assert_eq!((*env).payload_size, 14);

            // Final release: drop glue fires once, payload + envelope freed.
            hew_msg_envelope_release(env);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn envelope_clone_alias_bumps_refcount_and_sets_alias_bit() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope contract.
        unsafe {
            let payload = alloc_test_payload(b"alias me");
            let env = hew_msg_envelope_new(payload, 8, Some(envelope_test_drop_glue));

            let same = hew_msg_envelope_clone_alias(env);
            assert_eq!(same, env, "clone_alias returns the same pointer");
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 2);
            assert!((*env).header_bits.load(Ordering::SeqCst) & HEW_MSG_ENVELOPE_ALIAS_ACTIVE != 0);

            // First release: refcount → 1; drop glue must NOT fire yet.
            hew_msg_envelope_release(env);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 0);

            // Final release: drop glue fires.
            hew_msg_envelope_release(env);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn envelope_payload_ptr_is_borrow_only() {
        // SAFETY: standard envelope contract.
        unsafe {
            let payload = alloc_test_payload(b"borrow");
            let env = hew_msg_envelope_new(payload, 6, None);
            let p = hew_msg_envelope_payload_ptr(env);
            assert_eq!(p, payload, "payload_ptr returns the original allocation");
            // Read-through borrow.
            let slice = std::slice::from_raw_parts(p.cast::<u8>(), 6);
            assert_eq!(slice, b"borrow");
            hew_msg_envelope_release(env);
        }
    }

    /// Phase α: `hew_actor_send_aliased` is now fail-closed and
    /// aborts via `hew_panic` on every invocation, after releasing
    /// the caller-transferred envelope refcount.  The pre-revision
    /// behaviour this test pinned (null-actor early release returns
    /// to the caller) no longer exists; the function cannot be
    /// invoked from a unit test without exiting the process.
    ///
    /// The underlying invariant — "the caller-transferred envelope
    /// refcount is released so the payload buffer is not leaked when
    /// the alias send cannot deliver" — is now covered by directly
    /// exercising [`hew_msg_envelope_release`] below; that is the
    /// release path the fail-closed FFI bodies invoke before
    /// `hew_panic`.  Re-enable / replace this test in Phase β when
    /// the alias send entry points come back online.
    #[test]
    #[ignore = "Phase α: hew_actor_send_aliased is fail-closed (panics)"]
    fn actor_send_aliased_null_actor_releases_envelope() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope-new contract; payload allocated
        // by `alloc_test_payload`; envelope ownership would transfer
        // into `hew_actor_send_aliased` if the alias path were live.
        unsafe {
            let payload = alloc_test_payload(b"null-actor");
            let env = hew_msg_envelope_new(payload, 10, Some(envelope_test_drop_glue));
            assert!(!env.is_null());
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);
            crate::actor::hew_actor_send_aliased(std::ptr::null_mut(), 0, env);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    /// Pins the envelope-release invariant the fail-closed alias-send
    /// FFI bodies rely on: releasing the caller's transferred refcount
    /// (the only refcount on a freshly-`new`'d envelope) drops the
    /// envelope, frees the buffer container, and fires `drop_glue`
    /// exactly once.  This is the payload-leak-prevention guarantee
    /// the Phase α `hew_actor_send_aliased` and `hew_mailbox_send_aliased`
    /// fail-closed paths invoke before calling `hew_panic`.
    #[test]
    fn envelope_release_after_new_drops_payload_exactly_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope-new contract; we hold the only
        // refcount, so a single release is the final release.
        unsafe {
            let payload = alloc_test_payload(b"release-once");
            let env = hew_msg_envelope_new(payload, 12, Some(envelope_test_drop_glue));
            assert!(!env.is_null());
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);
            hew_msg_envelope_release(env);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "single-refcount release must fire drop_glue exactly once"
            );
        }
    }

    #[test]
    fn envelope_fork_for_write_makes_distinct_payload_with_forked_bit() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope contract.
        unsafe {
            let payload = alloc_test_payload(b"original-bytes");
            let env = hew_msg_envelope_new(payload, 14, Some(envelope_test_drop_glue));
            // Two-observer state, like a real send.
            let _ = hew_msg_envelope_clone_alias(env);
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 2);

            // Fork transfers the caller's reference to the new envelope.
            let forked = hew_msg_envelope_fork_for_write(env);
            assert!(!forked.is_null());
            assert_ne!(forked, env, "fork returns a new envelope");
            assert_eq!((*forked).refcount.load(Ordering::SeqCst), 1);
            assert!((*forked).header_bits.load(Ordering::SeqCst) & HEW_MSG_ENVELOPE_FORKED != 0);
            // Forked payload is a distinct buffer with the same bytes.
            assert_ne!((*forked).payload, payload);
            let forked_slice = std::slice::from_raw_parts((*forked).payload.cast::<u8>(), 14);
            assert_eq!(forked_slice, b"original-bytes");
            // Original envelope's refcount dropped from 2 → 1.
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

            // Drain everything; drop_glue fires twice (once per envelope).
            hew_msg_envelope_release(forked);
            hew_msg_envelope_release(env);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 2);
        }
    }

    #[test]
    fn envelope_fork_for_write_preserves_reserved_header_bits() {
        // Regression: `fork_for_write` must inherit reserved/contract
        // bits from the source envelope (`SHARED_FROZEN`,
        // `CAPABILITY_TRANSFER`, γ/δ reserved). Only `ALIAS_ACTIVE` is
        // intentionally cleared on the fork (the new envelope has one
        // observer). `FORKED` is set unconditionally.
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope contract.
        unsafe {
            let payload = alloc_test_payload(b"capability-bytes");
            let env = hew_msg_envelope_new(payload, 16, Some(envelope_test_drop_glue));
            // Set the bits the fork must preserve.
            (*env).header_bits.fetch_or(
                HEW_MSG_ENVELOPE_SHARED_FROZEN
                    | HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER
                    | HEW_MSG_ENVELOPE_RESERVED_GAMMA_A
                    | HEW_MSG_ENVELOPE_RESERVED_DELTA_B,
                Ordering::SeqCst,
            );
            // Two-observer state.
            let _ = hew_msg_envelope_clone_alias(env);
            assert!(
                (*env).header_bits.load(Ordering::SeqCst) & HEW_MSG_ENVELOPE_ALIAS_ACTIVE != 0,
                "clone_alias should set ALIAS_ACTIVE on the source"
            );

            let forked = hew_msg_envelope_fork_for_write(env);
            assert!(!forked.is_null());
            let forked_bits = (*forked).header_bits.load(Ordering::SeqCst);
            assert!(
                forked_bits & HEW_MSG_ENVELOPE_FORKED != 0,
                "FORKED bit must be set on the forked envelope"
            );
            assert!(
                forked_bits & HEW_MSG_ENVELOPE_SHARED_FROZEN != 0,
                "SHARED_FROZEN must transfer to the forked envelope (got bits = {forked_bits:#x})"
            );
            assert!(
                forked_bits & HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER != 0,
                "CAPABILITY_TRANSFER must transfer to the forked envelope (got bits = {forked_bits:#x})"
            );
            assert!(
                forked_bits & HEW_MSG_ENVELOPE_RESERVED_GAMMA_A != 0,
                "RESERVED_GAMMA_A must transfer to the forked envelope (got bits = {forked_bits:#x})"
            );
            assert!(
                forked_bits & HEW_MSG_ENVELOPE_RESERVED_DELTA_B != 0,
                "RESERVED_DELTA_B must transfer to the forked envelope (got bits = {forked_bits:#x})"
            );
            assert!(
                forked_bits & HEW_MSG_ENVELOPE_ALIAS_ACTIVE == 0,
                "ALIAS_ACTIVE must NOT transfer (the forked envelope has one observer)"
            );

            hew_msg_envelope_release(forked);
            hew_msg_envelope_release(env);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 2);
        }
    }

    /// `MUST_BE_ZERO` mask covers exactly bits 9..31. The fail-closed
    /// panic itself cannot be exercised under `panic = "abort"` (the
    /// process would abort the test runner), so we assert the mask
    /// shape directly. The panic site is the `header_validate` call
    /// in `hew_msg_envelope_release`; reviewers should confirm that
    /// path is wired by inspection.
    #[test]
    fn envelope_must_be_zero_mask_covers_bits_nine_through_thirtyone() {
        for bit in 0..9 {
            assert_eq!(
                HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK & (1u32 << bit),
                0,
                "bit {bit} is in the live header range; mask must not cover it"
            );
        }
        for bit in 9..32 {
            assert_ne!(
                HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK & (1u32 << bit),
                0,
                "bit {bit} is reserved-zero; mask must cover it"
            );
        }
    }

    #[test]
    fn envelope_node_free_releases_envelope_branch() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: build a node on the envelope path and free it through
        // the same `hew_msg_node_free` entry point used by the runtime.
        unsafe {
            let payload = alloc_test_payload(b"node-envelope");
            let env = hew_msg_envelope_new(payload, 13, Some(envelope_test_drop_glue));
            let node = msg_node_alloc_aliased(7, env, ptr::null_mut());
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 7);
            assert!((*node).data.is_null(), "envelope nodes have null data");
            assert_eq!((*node).envelope, env);

            hew_msg_node_free(node);
            // node free released the envelope; drop glue fired once.
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    /// Cycle-break audit. The COW envelope is for *message payloads*,
    /// not for `ActorRef` itself; an `ActorRef` (or `Weak<ActorRef>`)
    /// inside a payload struct is reachable through the payload's
    /// drop glue, which the envelope runs exactly once on the
    /// transition to refcount==0. We model that with a plain
    /// `Arc<()>` here: dropping the envelope must release exactly one
    /// strong reference, never zero (leak) and never two (double-free).
    #[test]
    fn envelope_drop_glue_decrements_arc_exactly_once() {
        use std::sync::Arc;

        /// Drop glue for a payload buffer that holds an in-place
        /// `Arc<()>`. The envelope free's the buffer afterwards via
        /// `libc::free`; this glue only runs the destructor.
        unsafe extern "C" fn arc_in_buf_drop_glue(payload: *mut c_void) {
            // SAFETY: caller (envelope release) guarantees the buffer
            // holds an initialised `Arc<()>` constructed in-place via
            // `ptr::write` below.
            unsafe { std::ptr::drop_in_place(payload.cast::<Arc<()>>()) };
        }

        let observed: Arc<()> = Arc::new(());
        assert_eq!(Arc::strong_count(&observed), 1);

        // SAFETY: we libc::malloc a buffer the size of one `Arc<()>`,
        // ptr::write a clone into it, and hand ownership to the
        // envelope. The envelope releases the clone via drop_glue and
        // free's the buffer afterwards.
        unsafe {
            let arc_size = std::mem::size_of::<Arc<()>>();
            let buf = libc::malloc(arc_size).cast::<Arc<()>>();
            assert!(!buf.is_null());
            std::ptr::write(buf, Arc::clone(&observed));
            // The clone is now owned by `buf`; observed strong = 2.
            assert_eq!(Arc::strong_count(&observed), 2);

            let env = hew_msg_envelope_new(buf.cast(), arc_size, Some(arc_in_buf_drop_glue));
            assert!(!env.is_null());
            // Envelope construction does not touch strong count.
            assert_eq!(Arc::strong_count(&observed), 2);

            // Final release: drop_glue runs (strong → 1), then the
            // envelope free's the buffer + envelope itself.
            hew_msg_envelope_release(env);
            assert_eq!(Arc::strong_count(&observed), 1);
        }
    }
}
