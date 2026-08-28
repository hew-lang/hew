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
use crate::mailbox_header::{normalize_coalesce_fallback, Origin};
use crate::read_slot::{
    hew_read_slot_free, read_slot_deposit_status, read_slot_retain, HewReadSlot, ReadStatus,
};
use crate::scheduler::{MESSAGES_RECEIVED, MESSAGES_SENT};
use crate::set_last_error;
use crate::tracing::HewTraceContext;

// Exact ask-node identity ledger for ownership regressions. A queued ask moves
// one retained reply-channel reference into its `HewMsgNode`; recording that
// node-to-channel edge lets tests prove the SAME allocation stays live when a
// terminal reclaim edge is omitted, rather than inferring a leak from time.
// Test-only: no allocation, locking, or global state in production.
#[cfg(test)]
static ACTIVE_ASK_NODES: std::sync::LazyLock<
    crate::lifetime::PoisonSafe<std::collections::HashMap<usize, usize>>,
> = std::sync::LazyLock::new(|| crate::lifetime::PoisonSafe::new(std::collections::HashMap::new()));

#[cfg(test)]
fn track_ask_node_for_test(node: *mut HewMsgNode, reply_channel: *mut c_void) {
    if reply_channel.is_null() {
        return;
    }
    ACTIVE_ASK_NODES.access(|nodes| {
        assert!(
            nodes.insert(node.addr(), reply_channel.addr()).is_none(),
            "new ask node address was already tracked"
        );
    });
}

#[cfg(test)]
fn untrack_ask_node_for_test(node: *mut HewMsgNode) {
    ACTIVE_ASK_NODES.access(|nodes| {
        nodes.remove(&node.addr());
    });
}

#[cfg(test)]
pub(crate) fn ask_node_for_reply_channel_for_test(reply_channel: *mut c_void) -> *mut HewMsgNode {
    ACTIVE_ASK_NODES.access(|nodes| {
        nodes
            .iter()
            .find_map(|(&node, &channel)| (channel == reply_channel.addr()).then_some(node))
            .map_or(ptr::null_mut(), ptr::without_provenance_mut)
    })
}

/// Deterministic producer rendezvous after the MPSC head swap but before the
/// predecessor link is published.
///
/// The queue is deliberately inconsistent at this seam: a consumer observes
/// the new head but cannot yet reach the node from its tail. Targeting the hook
/// by reply-channel identity prevents unrelated concurrent mailbox tests from
/// participating.
#[cfg(test)]
type MpscPostSwapPreLinkHook = (
    usize,
    std::sync::Arc<std::sync::Barrier>,
    std::sync::Arc<std::sync::Barrier>,
);

#[cfg(test)]
static MPSC_POST_SWAP_PRE_LINK_HOOK: crate::lifetime::PoisonSafe<Option<MpscPostSwapPreLinkHook>> =
    crate::lifetime::PoisonSafe::new(None);

#[cfg(test)]
static MPSC_SYS_POST_SWAP_PRE_LINK_HOOK: crate::lifetime::PoisonSafe<
    Option<MpscPostSwapPreLinkHook>,
> = crate::lifetime::PoisonSafe::new(None);

#[cfg(test)]
static SYS_COUNT_PUBLICATION_HOOK: crate::lifetime::PoisonSafe<Option<MpscPostSwapPreLinkHook>> =
    crate::lifetime::PoisonSafe::new(None);

#[cfg(test)]
static USER_COUNT_PUBLICATION_HOOK: crate::lifetime::PoisonSafe<Option<MpscPostSwapPreLinkHook>> =
    crate::lifetime::PoisonSafe::new(None);

/// Deterministic rendezvous for a bounded `Block` sender after it has proved
/// the mailbox full while holding `slow_path`, joined `block_wait`, and
/// rechecked closure, but immediately before it releases the queue and
/// atomically releases the predicate mutex in `Condvar::wait`.
///
/// Targeting by mailbox identity keeps unrelated parallel tests out of the
/// rendezvous. The hook is consumed on first use so a spurious wake cannot
/// attempt the same two-party barrier a second time.
#[cfg(test)]
type BlockPreWaitHook = (
    usize,
    std::sync::Arc<std::sync::Barrier>,
    std::sync::Arc<std::sync::Barrier>,
);

#[cfg(test)]
static BLOCK_PRE_WAIT_HOOK: crate::lifetime::PoisonSafe<Option<BlockPreWaitHook>> =
    crate::lifetime::PoisonSafe::new(None);

#[cfg(test)]
struct BlockPreWaitHookGuard;

#[cfg(test)]
impl BlockPreWaitHookGuard {
    fn install(
        mailbox: *mut HewMailbox,
    ) -> (
        Self,
        std::sync::Arc<std::sync::Barrier>,
        std::sync::Arc<std::sync::Barrier>,
    ) {
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        BLOCK_PRE_WAIT_HOOK.access(|hook| {
            assert!(hook.is_none(), "block pre-wait hook already installed");
            *hook = Some((mailbox.addr(), entered.clone(), release.clone()));
        });
        (Self, entered, release)
    }
}

#[cfg(test)]
impl Drop for BlockPreWaitHookGuard {
    fn drop(&mut self) {
        BLOCK_PRE_WAIT_HOOK.access(|hook| *hook = None);
    }
}

#[cfg(test)]
fn run_block_pre_wait_hook(mailbox: &HewMailbox) {
    let mailbox_addr = std::ptr::from_ref(mailbox).addr();
    let rendezvous = BLOCK_PRE_WAIT_HOOK.access(|hook| {
        if hook
            .as_ref()
            .is_some_and(|(target, _, _)| *target == mailbox_addr)
        {
            hook.take().map(|(_, entered, release)| (entered, release))
        } else {
            None
        }
    });
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
}

#[cfg(test)]
pub(crate) struct SysCountPublicationHookGuard;

#[cfg(test)]
impl SysCountPublicationHookGuard {
    pub(crate) fn install(
        sys_msg: HewSysMsg,
    ) -> (
        Self,
        std::sync::Arc<std::sync::Barrier>,
        std::sync::Arc<std::sync::Barrier>,
    ) {
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        SYS_COUNT_PUBLICATION_HOOK.access(|hook| {
            assert!(hook.is_none(), "system-count hook already installed");
            *hook = Some((
                usize::try_from(sys_msg.as_i32()).expect("system kind is non-negative"),
                entered.clone(),
                release.clone(),
            ));
        });
        (Self, entered, release)
    }
}

#[cfg(test)]
impl Drop for SysCountPublicationHookGuard {
    fn drop(&mut self) {
        SYS_COUNT_PUBLICATION_HOOK.access(|hook| *hook = None);
    }
}

#[cfg(test)]
fn run_sys_count_publication_hook(node: *mut HewMsgNode) {
    // SAFETY: producer exclusively owns the initialized node at either seam.
    let msg_type = unsafe { (*node).msg_type };
    let rendezvous = SYS_COUNT_PUBLICATION_HOOK.access(|hook| {
        hook.as_ref().and_then(|(target, entered, release)| {
            (i32::try_from(*target).ok() == Some(msg_type))
                .then(|| (entered.clone(), release.clone()))
        })
    });
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
}

#[cfg(test)]
pub(crate) struct UserCountPublicationHookGuard;

#[cfg(test)]
impl UserCountPublicationHookGuard {
    pub(crate) fn install(
        node: *mut HewMsgNode,
    ) -> (
        Self,
        std::sync::Arc<std::sync::Barrier>,
        std::sync::Arc<std::sync::Barrier>,
    ) {
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        USER_COUNT_PUBLICATION_HOOK.access(|hook| {
            assert!(hook.is_none(), "user-count hook already installed");
            *hook = Some((node.addr(), entered.clone(), release.clone()));
        });
        (Self, entered, release)
    }
}

#[cfg(test)]
impl Drop for UserCountPublicationHookGuard {
    fn drop(&mut self) {
        USER_COUNT_PUBLICATION_HOOK.access(|hook| *hook = None);
    }
}

#[cfg(test)]
fn run_user_count_publication_hook(node: *mut HewMsgNode) {
    let rendezvous = USER_COUNT_PUBLICATION_HOOK.access(|hook| {
        hook.as_ref().and_then(|(target, entered, release)| {
            (*target == node.addr()).then(|| (entered.clone(), release.clone()))
        })
    });
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
}

#[cfg(test)]
pub(crate) struct MpscPostSwapPreLinkHookGuard {
    system: bool,
}

#[cfg(test)]
impl MpscPostSwapPreLinkHookGuard {
    pub(crate) fn install(
        reply_channel: *mut c_void,
    ) -> (
        Self,
        std::sync::Arc<std::sync::Barrier>,
        std::sync::Arc<std::sync::Barrier>,
    ) {
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        MPSC_POST_SWAP_PRE_LINK_HOOK.access(|hook| {
            assert!(hook.is_none(), "MPSC delayed-link hook already installed");
            *hook = Some((reply_channel.addr(), entered.clone(), release.clone()));
        });
        (Self { system: false }, entered, release)
    }

    pub(crate) fn install_system(
        sys_msg: HewSysMsg,
    ) -> (
        Self,
        std::sync::Arc<std::sync::Barrier>,
        std::sync::Arc<std::sync::Barrier>,
    ) {
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        MPSC_SYS_POST_SWAP_PRE_LINK_HOOK.access(|hook| {
            assert!(
                hook.is_none(),
                "system MPSC delayed-link hook already installed"
            );
            *hook = Some((
                usize::try_from(sys_msg.as_i32()).expect("system kind is non-negative"),
                entered.clone(),
                release.clone(),
            ));
        });
        (Self { system: true }, entered, release)
    }
}

#[cfg(test)]
impl Drop for MpscPostSwapPreLinkHookGuard {
    fn drop(&mut self) {
        if self.system {
            MPSC_SYS_POST_SWAP_PRE_LINK_HOOK.access(|hook| *hook = None);
        } else {
            MPSC_POST_SWAP_PRE_LINK_HOOK.access(|hook| *hook = None);
        }
    }
}

#[cfg(test)]
fn run_mpsc_post_swap_pre_link_hook(node: *mut HewMsgNode) {
    // SAFETY: enqueue owns `node` exclusively until it publishes the
    // predecessor link, and the hook only reads the initialized channel field.
    let reply_channel = unsafe { (*node).reply_channel };
    let rendezvous = MPSC_POST_SWAP_PRE_LINK_HOOK.access(|hook| {
        hook.as_ref().and_then(|(target, entered, release)| {
            (*target == reply_channel.addr()).then(|| (entered.clone(), release.clone()))
        })
    });
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
    // SAFETY: as above; msg_type is initialized before queue publication.
    let msg_type = unsafe { (*node).msg_type };
    let sys_rendezvous = MPSC_SYS_POST_SWAP_PRE_LINK_HOOK.access(|hook| {
        hook.as_ref().and_then(|(target, entered, release)| {
            (i32::try_from(*target).ok() == Some(msg_type))
                .then(|| (entered.clone(), release.clone()))
        })
    });
    if let Some((entered, release)) = sys_rendezvous {
        entered.wait();
        release.wait();
    }
}

pub use crate::cow_envelope::{HewMsgEnvelope, HewMsgEnvelopeDropFn};
pub use crate::mailbox_header::HewSysMsg;
pub use crate::mailbox_header::{
    HEW_MSG_ENVELOPE_ALIAS_ACTIVE, HEW_MSG_ENVELOPE_ARENA_BACKED,
    HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER, HEW_MSG_ENVELOPE_FORKED,
    HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK, HEW_MSG_ENVELOPE_RESERVED_DELTA_A,
    HEW_MSG_ENVELOPE_RESERVED_DELTA_B, HEW_MSG_ENVELOPE_RESERVED_GAMMA_A,
    HEW_MSG_ENVELOPE_RESERVED_GAMMA_B, HEW_MSG_ENVELOPE_SHARED_FROZEN,
};

/// Re-export of [`HewOverflowPolicy`] for the public mailbox API.
pub use crate::internal::types::HewOverflowPolicy as OverflowPolicy;

/// Key extractor used by coalescing mailboxes.
pub type HewCoalesceKeyFn = unsafe extern "C" fn(i32, *mut c_void, usize) -> u64;
/// Generated typed destructor for one actor message payload.
pub type HewMessageDropFn = unsafe extern "C" fn(i32, *mut c_void, usize);

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

/// Whether an injected allocation failure is still pending on this thread.
///
/// `should_fail_mailbox_alloc` disarms the trap only when it actually fires, so
/// a still-armed trap after a call is proof that the call allocated nothing.
#[cfg(test)]
pub(crate) fn mailbox_alloc_failure_still_armed() -> bool {
    FAIL_MAILBOX_ALLOC_ON_NTH.with(|slot| slot.get() != usize::MAX)
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
    /// Payload classification for mailbox envelope routing.
    /// `MailboxPayloadClass::Unit (0)` on all existing allocators (zero-init
    /// backward-compatible). See `crate::mailbox_envelope::MailboxPayloadClass`.
    pub payload_class: u8,
    /// Source actor PID for attribution and cross-node routing.
    /// `SOURCE_PID_UNKNOWN (0)` when the origin is not recorded.
    pub source_pid: u64,
    /// Opaque handle for an associated `HewCancellationToken`.
    /// `CANCEL_TOKEN_NONE (0)` when no token is attached.
    /// Cross-node serialisation of this field is deferred to a future lane.
    pub cancel_token_handle: u64,
}

// The envelope representation and lifecycle live in `crate::cow_envelope`.
// This module retains the native allocator and C-ABI null-error policy.

/// Allocate a fresh COW envelope using the native mailbox allocator.
///
/// # Safety
///
/// `payload` must be a malloc-compatible allocation of `payload_size` bytes
/// (or null for zero bytes). Ownership transfers to the returned envelope.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_new(
    payload: *mut c_void,
    payload_size: usize,
    drop_glue: Option<HewMsgEnvelopeDropFn>,
) -> *mut HewMsgEnvelope {
    // SAFETY: forwarded unchanged to the shared envelope lifecycle.
    unsafe { crate::cow_envelope::new(payload, payload_size, drop_glue, mailbox_malloc) }
}

/// Add an alias observer to a live COW envelope.
///
/// # Safety
///
/// `env` must be live and the caller must own one of its references.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_clone_alias(
    env: *mut HewMsgEnvelope,
) -> *mut HewMsgEnvelope {
    cabi_guard!(env.is_null(), ptr::null_mut());
    // SAFETY: the C ABI contract requires a live envelope.
    unsafe { crate::cow_envelope::clone_alias(env) }
}

/// Release one live COW envelope observer.
///
/// # Safety
///
/// `env` must be live and the caller must own exactly one reference.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_release(env: *mut HewMsgEnvelope) {
    cabi_guard!(env.is_null());
    // SAFETY: the C ABI contract requires a live envelope reference.
    unsafe { crate::cow_envelope::release(env) }
}

/// Borrow a COW envelope payload pointer for read-only access.
///
/// # Safety
///
/// `env` must be null or live. The result remains valid only while the caller
/// owns a reference and must not be used to mutate or free the payload.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_payload_ptr(env: *mut HewMsgEnvelope) -> *mut c_void {
    // SAFETY: a non-null C ABI argument must be a live envelope.
    unsafe { crate::cow_envelope::payload_ptr(env) }
}

/// Fork a COW envelope into a private writable payload copy.
///
/// # Safety
///
/// `env` must be live and the caller transfers one owned reference to this
/// call. On success, that reference is replaced by the returned envelope.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_fork_for_write(
    env: *mut HewMsgEnvelope,
) -> *mut HewMsgEnvelope {
    cabi_guard!(env.is_null(), ptr::null_mut());
    // SAFETY: the C ABI contract transfers one live envelope reference.
    unsafe { crate::cow_envelope::fork_for_write(env, mailbox_malloc) }
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
    // External / user-send seam: capture (and, when no execution context is
    // installed, MINT) a sampled trace root via `tracing::current_context` so
    // boundary-crossing sends carry a real causal context.
    // SAFETY: forwards the caller's `data`/`data_size`/`reply_channel` contract.
    unsafe {
        msg_node_alloc_with_trace(
            msg_type,
            data,
            data_size,
            reply_channel,
            crate::tracing::current_context(),
        )
    }
}

/// Allocate a [`HewMsgNode`] for a SYSTEM/control-plane send WITHOUT minting a
/// trace root.
///
/// System sends (supervisor child-event notifications, link/monitor signals)
/// must never mint a trace context. The crash notification reaches this path
/// from `hew_actor_trap`, which runs in signal-handler-adjacent context where
/// minting a root is forbidden. So this path captures only an already-installed
/// context (via `tracing::system_context`) and otherwise carries zero/absent
/// context, deferring the crash-recovery mint to
/// `tracing::ensure_supervisor_trace_root` on the supervisor-dispatch side.
///
/// # Safety
///
/// Same requirements as [`msg_node_alloc`].
unsafe fn msg_node_alloc_sys(
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
    reply_channel: *mut c_void,
) -> *mut HewMsgNode {
    // SAFETY: forwards the caller's `data`/`data_size`/`reply_channel` contract.
    unsafe {
        msg_node_alloc_with_trace(
            msg_type,
            data,
            data_size,
            reply_channel,
            crate::tracing::system_context(),
        )
    }
}

/// Allocate a [`HewMsgNode`], deep-copying `data` and stamping the provided
/// `trace_context` onto the node.
///
/// The trace-context capture policy is chosen by the caller: [`msg_node_alloc`]
/// passes the minting external-send seam, while [`msg_node_alloc_sys`] passes
/// the non-minting system-send seam.
///
/// # Safety
///
/// Same requirements as [`msg_node_alloc`].
unsafe fn msg_node_alloc_with_trace(
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
    reply_channel: *mut c_void,
    trace_context: HewTraceContext,
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
        (*node).trace_context = trace_context;
        // Explicit zero-init for the mailbox-envelope ABI fields.
        // These fields are NEW; mailbox_malloc uses libc::malloc (not calloc)
        // so they are NOT zero-initialized by the allocator. An uninitialized
        // payload_class byte that happened to equal SerializedCrossNode (3)
        // would silently pass the cross-node gate — defeating the fail-closed
        // invariant. Zero maps to the canonical sentinels:
        //   payload_class = 0  →  MailboxPayloadClass::Unit
        //   source_pid    = 0  →  SOURCE_PID_UNKNOWN
        //   cancel_token  = 0  →  CANCEL_TOKEN_NONE
        (*node).payload_class = 0;
        (*node).source_pid = 0;
        (*node).cancel_token_handle = 0;

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

    #[cfg(test)]
    track_ask_node_for_test(node, reply_channel);
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
/// Live alias-send path: reached from [`hew_mailbox_send_aliased`] /
/// [`send_aliased_with_overflow`] (and through them from
/// [`crate::actor::hew_actor_send_aliased`]). On `malloc` failure the
/// node is null and the envelope refcount is **not** consumed — the
/// caller releases it.
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
        // Explicit zero-init for the mailbox-envelope ABI fields.
        // See msg_node_alloc for the rationale; same invariant applies here.
        (*node).payload_class = 0;
        (*node).source_pid = 0;
        (*node).cancel_token_handle = 0;
    }

    #[cfg(test)]
    track_ask_node_for_test(node, reply_channel);
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

/// Nodes detached from a terminal mailbox under its single-consumer lock.
///
/// The intrusive list deliberately reuses each exclusively-owned node's
/// `next` field. This avoids allocating while the terminal lock is held and,
/// more importantly, lets the lock protect only queue detachment. Payload drop
/// glue, ask-reply retirement, resume enqueues, and their scheduler wakeups all
/// run later in [`DetachedTerminalNodes::retire`], after the lock is released.
struct DetachedTerminalNodes {
    head: *mut HewMsgNode,
    message_drop_fn: Option<HewMessageDropFn>,
    notify_not_full: bool,
}

impl DetachedTerminalNodes {
    fn new(message_drop_fn: Option<HewMessageDropFn>) -> Self {
        Self {
            head: ptr::null_mut(),
            message_drop_fn,
            notify_not_full: false,
        }
    }

    fn is_empty(&self) -> bool {
        self.head.is_null()
    }

    /// Add one exclusively-owned dequeued node to the detached list.
    ///
    /// # Safety
    ///
    /// `node` must be non-null, exclusively owned, and no longer reachable
    /// from either mailbox queue.
    unsafe fn push(&mut self, node: *mut HewMsgNode) {
        // SAFETY: caller owns `node`; its queue link is no longer observed.
        unsafe { (*node).next.store(self.head, Ordering::Relaxed) };
        self.head = node;
    }

    /// Retire every detached node after terminal serialization is released.
    ///
    /// `mailbox` is used only for the bounded-sender notification and must
    /// remain live for this call. Node destruction itself is independent of
    /// the mailbox because the typed drop callback was copied at detachment.
    ///
    /// # Safety
    ///
    /// Every node in this list must remain exclusively owned by the list.
    unsafe fn retire(mut self, mailbox: &HewMailbox) {
        if self.notify_not_full {
            mailbox.notify_not_full_all();
        }
        while !self.head.is_null() {
            let node = self.head;
            // SAFETY: this list exclusively owns `node`.
            self.head = unsafe { (*node).next.load(Ordering::Relaxed) };
            // SAFETY: `node` was detached from the mailbox and is exclusively
            // owned here. This is intentionally outside terminal_reclaiming:
            // ask retirement and generated drops may wake/schedule actors.
            unsafe { hew_msg_node_free_with_message_drop(node, self.message_drop_fn) };
        }
    }
}

/// Detach every currently reachable node while terminal serialization is held.
///
/// # Safety
///
/// `mb` must point to `mailbox`, which must remain live for the call. The
/// caller must hold `mailbox.terminal_reclaiming` and satisfy the terminal
/// single-consumer contract documented on [`mailbox_reclaim_queued_terminal`].
unsafe fn detach_queued_terminal_locked(mailbox: &HewMailbox) -> DetachedTerminalNodes {
    let mut detached = DetachedTerminalNodes::new(mailbox.message_drop_fn);

    // Do not route terminal detachment through mailbox_try_recv_with_origin:
    // its corruption fallback destroys an undecodable system node inline.
    // Terminal serialization must contain no destructor or callback edge.
    loop {
        // SAFETY: terminal serialization provides the single consumer.
        let node = unsafe { mailbox.sys_queue.try_dequeue() };
        if node.is_null() {
            break;
        }
        let previous = mailbox.sys_count.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(previous > 0, "system queue count underflow");
        MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
        // SAFETY: dequeue transferred exclusive ownership.
        unsafe { detached.push(node) };
    }

    if mailbox.use_slow_path {
        let mut queue = mailbox.slow_path.lock_or_recover();
        while let Some(node) = queue.user_queue.pop_front() {
            let previous = mailbox.count.fetch_sub(1, Ordering::Release);
            debug_assert!(previous > 0, "slow user queue count underflow");
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            detached.notify_not_full = true;
            // SAFETY: pop_front transferred exclusive ownership.
            unsafe { detached.push(node) };
        }
    } else {
        loop {
            // SAFETY: terminal serialization provides the single consumer.
            let node = unsafe { mailbox.user_fast.try_dequeue() };
            if node.is_null() {
                break;
            }
            let previous = mailbox.count.fetch_sub(1, Ordering::AcqRel);
            debug_assert!(previous > 0, "fast user queue count underflow");
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            detached.notify_not_full = true;
            // SAFETY: dequeue transferred exclusive ownership.
            unsafe { detached.push(node) };
        }
    }

    detached
}

/// Reclaim every message still queued when an actor becomes terminal.
///
/// There are three ways a live node can remain behind a terminal transition:
///
/// - a stop finalizes while user work remains queued;
/// - an idle sender enqueues, then loses its `Idle -> Runnable` wake CAS to the
///   stopper's `Idle -> Stopped` CAS;
/// - a crashing activation owns one dequeued message while later messages stay
///   queued behind it.
///
/// In every case a queued **ask** node owns a sender-side reply-channel
/// reference, and that reference is what its caller is blocked on. Until the
/// node is freed, nothing marks the channel orphaned and nothing publishes the
/// null reply that wakes the waiter. Leaving the node for `hew_mailbox_free`
/// (which may be deferred indefinitely, especially for a supervised crash)
/// converts the terminal event into a full ask-timeout stall.
///
/// [`hew_msg_node_free`] retires each node's ask sender reference
/// ([`retire_msg_node_ask_sender_ref`]), so draining here IS the wake: every
/// stranded waiter is unblocked with the orphaned classification
/// (`HEW_REPLY_FAIL_ACTOR_STOPPED`) it would have received during mailbox
/// teardown. Non-ask user nodes and leftover system nodes are reclaimed on the
/// same pass.
///
/// # Safety
///
/// `mb` must be null or a valid mailbox pointer. The caller must still own a
/// live actor/mailbox allocation and prove that no non-terminal activation can
/// consume concurrently. The mailbox's terminal-reclaim lock serialises these
/// eligible terminal consumers:
///
/// - the worker finalizing `Stopping -> Stopped`;
/// - the direct idle stopper immediately after winning `Idle -> Stopped`; or
/// - the crash/trap authority before link/monitor/supervisor notification can
///   transfer reclamation ownership;
/// - a producer whose enqueue completed but whose wake CAS observed terminal,
///   after proving `dispatch_active == false`.
pub(crate) unsafe fn mailbox_reclaim_queued_terminal(mb: *mut HewMailbox) {
    if mb.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mb` is valid.
    let mailbox = unsafe { &*mb };

    // A sender that passed the mailbox's open check before close can publish
    // after the first terminal drain. Its failed wake CAS then helps with a
    // second drain. Serialise those consumers so the lock-free MPSC queues
    // retain their single-consumer contract; waiting (rather than try-locking)
    // guarantees a producer whose enqueue already completed gets a pass after
    // an earlier drainer that may just have observed the queue empty.
    let detached = {
        let _reclaim_guard = mailbox.terminal_reclaiming.lock_or_recover();
        // SAFETY: the guard provides terminal single-consumer ownership.
        unsafe { detach_queued_terminal_locked(mailbox) }
    };
    // SAFETY: caller keeps the mailbox live; detachment owns every node.
    unsafe { detached.retire(mailbox) };
}

/// Conditionally reclaim queued terminal messages, then publish an ownership
/// release while still holding the terminal-reclaim lock.
///
/// Testing `eligible`, draining, and calling `release` in one critical section
/// closes both terminal handoffs:
///
/// - an external trap either observes the released activation and drains, or
///   the activation observes the terminal state and drains before releasing;
/// - a producer that finishes an MPSC predecessor link after the activation's
///   last drain either precedes a later activation drain or observes the
///   released owner and drains itself.
///
/// `release` is still called for a null mailbox; `eligible` is not, because
/// there is no queue to reclaim.
///
/// # Safety
///
/// `mb` must be null or a valid mailbox pointer. For a non-null pointer, the
/// caller must satisfy the safety contract of [`mailbox_reclaim_queued_terminal`]
/// whenever `eligible` returns true. Neither callback may recursively acquire
/// the mailbox's terminal-reclaim lock.
pub(crate) unsafe fn mailbox_reclaim_queued_terminal_if_then<F, R>(
    mb: *mut HewMailbox,
    mut eligible: F,
    release: R,
) where
    F: FnMut() -> bool,
    R: FnOnce(),
{
    if mb.is_null() {
        release();
        return;
    }
    // SAFETY: caller guarantees `mb` is valid.
    let mailbox = unsafe { &*mb };
    let mut release = Some(release);
    loop {
        let detached = {
            let _reclaim_guard = mailbox.terminal_reclaiming.lock_or_recover();
            if !eligible() {
                release.take().expect("release callback called once")();
                return;
            }
            // SAFETY: the guard provides terminal single-consumer ownership.
            let detached = unsafe { detach_queued_terminal_locked(mailbox) };
            if detached.is_empty() {
                // Publish dispatch release only after an empty observation made
                // under the same lock used by producer handoff. No callback or
                // wake runs in this critical section.
                release.take().expect("release callback called once")();
                return;
            }
            detached
        };
        // Keep dispatch ownership across retirement. Generated drops may
        // self-send; the next iteration catches those nodes before release.
        // SAFETY: caller keeps the mailbox live; detachment owns every node.
        unsafe { detached.retire(mailbox) };
    }
}

/// Reclaim queued terminal messages when `eligible` holds under the
/// terminal-reclaim lock.
///
/// Evaluating the ownership predicate under the same lock used by the final
/// activation drain makes the two outcomes exhaustive: either the producer
/// observes an owner that must drain afterward, or it observes the released
/// owner and performs the drain itself.
///
/// # Safety
///
/// `mb` must be null or a valid mailbox pointer. If `eligible` returns true,
/// the caller must satisfy the safety contract of
/// [`mailbox_reclaim_queued_terminal`]. `eligible` must not recursively acquire
/// the mailbox's terminal-reclaim lock.
pub(crate) unsafe fn mailbox_reclaim_queued_terminal_if<F>(mb: *mut HewMailbox, eligible: F)
where
    F: FnOnce() -> bool,
{
    if mb.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mb` is valid.
    let mailbox = unsafe { &*mb };
    let detached = {
        let _reclaim_guard = mailbox.terminal_reclaiming.lock_or_recover();
        if !eligible() {
            return;
        }
        // SAFETY: the guard provides terminal single-consumer ownership.
        unsafe { detach_queued_terminal_locked(mailbox) }
    };
    // SAFETY: caller keeps the mailbox live; detachment owns every node.
    unsafe { detached.retire(mailbox) };
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
    #[cfg(test)]
    untrack_ask_node_for_test(node);
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

/// `msg_type` stamped on the MPSC stable stub.
///
/// This value is ARBITRARY and carries no invariant. The whole `i32` space is
/// live and user-reachable, so no number here could be a sentinel: the public
/// mailbox contract leaves `msg_type` an unrestricted `i32` that
/// [`hew_mailbox_send`] forwards verbatim, [`msg_node_alloc`] copies the
/// caller's value unchanged, and protocol tags are the low 32 bits of a
/// `SipHash` reinterpreted bit-for-bit as `i32` — a hash ending in
/// `0x8000_0000` yields exactly `i32::MIN`. A C-ABI caller can enqueue a real
/// message stamped with this constant today, and that is fine.
///
/// The stub is told apart from a real message by POINTER IDENTITY
/// ([`MpscQueue::stub_ptr`]) alone. No code anywhere reads a stub's
/// `msg_type`, and correctness must never come to depend on it.
const MPSC_STUB_MSG_TYPE: i32 = i32::MIN;

/// Allocate the stable stub node for an intrusive MPSC queue.
///
/// The stub is a permanently-live placeholder that exists only to simplify the
/// empty/non-empty transitions of the Vyukov algorithm. It is never returned to
/// a consumer, so it carries no payload: null data and [`MPSC_STUB_MSG_TYPE`].
fn alloc_stub_node() -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) — POD-like struct, no drop glue.
    let node = mailbox_malloc(std::mem::size_of::<HewMsgNode>()).cast::<HewMsgNode>();
    if node.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = MPSC_STUB_MSG_TYPE;
        (*node).data = ptr::null_mut();
        (*node).data_size = 0;
        (*node).reply_channel = ptr::null_mut();
        // The stub never carries an envelope payload; zero so that
        // hew_msg_node_free routes through the legacy `libc::free` path.
        (*node).envelope = ptr::null_mut();
        (*node).trace_context = HewTraceContext::default();
        // Explicit zero-init for the mailbox-envelope ABI fields.
        // See msg_node_alloc for the rationale; same invariant applies here.
        (*node).payload_class = 0;
        (*node).source_pid = 0;
        (*node).cancel_token_handle = 0;
    }
    node
}

/// Lock-free MPSC queue using a stable-stub Vyukov-style algorithm.
///
/// Multiple producers enqueue via an atomic swap on `head`. A single
/// consumer dequeues from `tail`. A heap-allocated stub node remains live for
/// the queue's full lifetime so producers never race a freed former stub.
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
        let stub = alloc_stub_node();
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

    /// Build the [`DequeueState::Success`] handed back to the single consumer.
    ///
    /// Every success return routes through here so the stub-escape invariant
    /// has exactly one statement instead of one per exit. The consumer
    /// re-injects the stable stub on each drain-to-empty and must always step
    /// over it: the caller frees what it receives, while producers hold a live
    /// pointer to the stub for the queue's whole lifetime, so handing it out
    /// would leave them linking through freed memory.
    ///
    /// `debug_assert!` rather than a release check — this is the per-message
    /// dequeue hot path, and the algorithm already keeps the invariant by
    /// construction (a stub tail is stepped over before any pop, and the single
    /// consumer is the only party that re-enqueues it). The assertion exists to
    /// catch a future edit to that algorithm, not a condition reachable today.
    #[inline]
    fn consumer_success(&self, node: *mut HewMsgNode) -> DequeueState {
        debug_assert!(
            node != self.stub_ptr(),
            "MPSC stable stub escaped to the consumer; freeing it would leave producers linking through freed memory"
        );
        DequeueState::Success(node)
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
        #[cfg(test)]
        run_mpsc_post_swap_pre_link_hook(node);
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
            // SAFETY: `next` is the first real node after the stable stub.
            unsafe { *self.tail.get() = next };
            // SAFETY: `next` became the consumer tail, so pop_inner sees a
            // valid live node under the same single-consumer invariant.
            return unsafe { self.pop_inner(next) };
        }

        if !next.is_null() {
            // SAFETY: advance consumer tail to the successor before returning
            // the current tail node to the caller for freeing.
            unsafe { *self.tail.get() = next };
            return self.consumer_success(tail);
        }

        let head = self.head.load(Ordering::Acquire);
        if tail != head {
            return DequeueState::Inconsistent;
        }

        // Queue holds a single real node. Re-inject the stable stub so the
        // consumer can pop the last node without ever freeing the stub
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
            return self.consumer_success(tail);
        }

        DequeueState::Inconsistent
    }

    /// Helper after advancing past the stable stub.
    unsafe fn pop_inner(&self, tail: *mut HewMsgNode) -> DequeueState {
        // SAFETY: `tail` is the first real node after the stable stub.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };

        if !next.is_null() {
            // SAFETY: `next` is the successor just observed from the current
            // consumer tail, so updating the tail preserves the invariant.
            unsafe { *self.tail.get() = next };
            return self.consumer_success(tail);
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
            return self.consumer_success(tail);
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

    /// Drain and free all remaining nodes (including the stable stub).
    ///
    /// # Safety
    ///
    /// No concurrent access may occur. All nodes must have been allocated
    /// by `msg_node_alloc` (or `alloc_stub_node`).
    unsafe fn drain_and_free(&self, message_drop_fn: Option<HewMessageDropFn>) {
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
            unsafe { hew_msg_node_free_with_message_drop(node, message_drop_fn) };
        }
        // Free the stable stub last.
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

/// One producer continuation parked by a full `overflow block` mailbox. The
/// mailbox owns the copied node and one retained read-slot ref until admission,
/// cancellation, or close wins the registration race.
#[derive(Debug)]
struct BlockedSender {
    actor: *mut crate::actor::HewActor,
    slot: *mut HewReadSlot,
    node: *mut HewMsgNode,
}

// SAFETY: raw actor pointers are used only by the liveness-validating scheduler
// wake path; slots and nodes remain live under explicit retained ownership.
unsafe impl Send for BlockedSender {}

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
    /// FIFO producer continuations waiting for a bounded Block mailbox slot.
    /// Separate from `slow_path` because close may be invoked by a callback
    /// while that queue lock is already held.
    blocked_senders: Mutex<VecDeque<BlockedSender>>,
    /// Queued user messages plus bounded fast-path slots reserved by in-flight
    /// producers.
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
    /// Typed destructor for legacy copied message payloads evicted before
    /// dispatch can move their owned fields out.
    message_drop_fn: Option<HewMessageDropFn>,
    /// Whether the mailbox has been closed.
    closed: std::sync::atomic::AtomicBool,
    /// Whether a stop has been requested on this mailbox.
    ///
    /// This flag IS the stop signal — there is no queued node. See
    /// [`mailbox_request_stop`].
    stop_requested: std::sync::atomic::AtomicBool,
    /// Serialises terminal drains across the terminal publisher, an active
    /// scheduler owner, and a producer helping after its wake CAS loses.
    terminal_reclaiming: Mutex<()>,
    /// Predicate mutex for senders parked on `not_full`.
    ///
    /// This is deliberately separate from `slow_path`: coalescing invokes
    /// externally supplied key and payload-drop callbacks while the queue is
    /// protected, and those callbacks may close the actor. Close must
    /// serialize with the check-to-park seam without recursively acquiring
    /// the queue mutex.
    block_wait: Mutex<()>,
    /// Condvar notified when a user message is consumed, waking blocked senders.
    not_full: Condvar,
    /// High-water mark: maximum `count` value observed.
    pub(crate) high_water_mark: AtomicI64,
    /// Whether this mailbox uses the slow (mutex) path for user messages.
    use_slow_path: bool,
}

impl HewMailbox {
    /// Wake one bounded sender without permitting a notification to pass
    /// between that sender's final predicate check and its condvar wait.
    fn notify_not_full_one(&self) {
        let _wait = self.block_wait.lock_or_recover();
        self.not_full.notify_one();
    }

    /// Wake every bounded sender under the same check-to-park protocol as
    /// [`Self::notify_not_full_one`].
    fn notify_not_full_all(&self) {
        let _wait = self.block_wait.lock_or_recover();
        self.not_full.notify_all();
    }

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

    /// `true` when this mailbox's overflow policy can free an already-queued
    /// node's payload *without* the consumer handler ever running on it —
    /// i.e. `DropOld` (evicts the oldest queued node) or `Coalesce` (replaces
    /// a matching node's payload, or falls back to `DropOld`). Both retire the
    /// superseded node's `data` buffer via `libc::free` / `hew_msg_node_free`.
    ///
    /// The active-mode I/O reactor relies on this to fail closed at attach
    /// time: its `on_data` envelope is a raw (`envelope == null`) node whose
    /// embedded `BytesTriple` refcount is dropped *only* by the handler that
    /// consumes it. An eviction would free the triple container as plain bytes
    /// and leak the underlying refcounted buffer, so attaching to such a
    /// mailbox is refused rather than risk a leak (or a double-free, were the
    /// node given evict-time drop glue that the happy path would re-run).
    #[inline]
    pub(crate) fn overflow_evicts_queued_payload(&self) -> bool {
        matches!(
            self.overflow,
            HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce
        )
    }
}

/// Read the overflow-eviction classification for a mailbox behind a raw
/// pointer. Returns `false` for a null pointer (no mailbox = nothing to
/// evict). Used by the active-mode reactor's attach-time fail-closed guard.
///
/// # Safety
///
/// `mb`, when non-null, must be a valid pointer to a live [`HewMailbox`].
pub(crate) unsafe fn mailbox_overflow_evicts_queued_payload(mb: *const HewMailbox) -> bool {
    if mb.is_null() {
        return false;
    }
    // SAFETY: caller guarantees `mb` is a live mailbox when non-null.
    unsafe { (*mb).overflow_evicts_queued_payload() }
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

/// Atomically reserve one slot in a bounded lock-free mailbox.
///
/// Returns `false` without changing `count` when the mailbox is already at
/// capacity. The caller must either publish one node with
/// [`enqueue_reserved_fast_user_node`] or release the reservation if allocation
/// fails.
fn try_reserve_fast_path_capacity(mb: &HewMailbox) -> bool {
    debug_assert!(mb.capacity > 0);
    debug_assert!(!mb.use_slow_path);

    let mut current = mb.count.load(Ordering::Acquire);
    loop {
        if current >= mb.capacity {
            return false;
        }
        match mb.count.compare_exchange_weak(
            current,
            current + 1,
            Ordering::AcqRel,
            Ordering::Acquire,
        ) {
            Ok(_) => return true,
            Err(actual) => current = actual,
        }
    }
}

fn release_fast_path_capacity(mb: &HewMailbox) {
    let previous = mb.count.fetch_sub(1, Ordering::AcqRel);
    debug_assert!(previous > 0, "fast-path capacity reservation underflow");
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
        unsafe { user_fast.drain_and_free(None) };
        return ptr::null_mut();
    };

    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        blocked_senders: Mutex::new(VecDeque::new()),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: -1,
        overflow: HewOverflowPolicy::DropNew,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        message_drop_fn: None,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_requested: std::sync::atomic::AtomicBool::new(false),
        terminal_reclaiming: Mutex::new(()),
        block_wait: Mutex::new(()),
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
        unsafe { user_fast.drain_and_free(None) };
        return ptr::null_mut();
    };

    let policy = HewOverflowPolicy::DropNew;
    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        blocked_senders: Mutex::new(VecDeque::new()),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: i64::from(capacity),
        overflow: policy,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        message_drop_fn: None,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_requested: std::sync::atomic::AtomicBool::new(false),
        terminal_reclaiming: Mutex::new(()),
        block_wait: Mutex::new(()),
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
        unsafe { user_fast.drain_and_free(None) };
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
        blocked_senders: Mutex::new(VecDeque::new()),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: cap,
        overflow: policy,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        message_drop_fn: None,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_requested: std::sync::atomic::AtomicBool::new(false),
        terminal_reclaiming: Mutex::new(()),
        block_wait: Mutex::new(()),
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
        unsafe { user_fast.drain_and_free(None) };
        return ptr::null_mut();
    };

    let cap = i64::from(capacity);
    Box::into_raw(Box::new(HewMailbox {
        user_fast,
        sys_queue,
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        blocked_senders: Mutex::new(VecDeque::new()),
        count: AtomicI64::new(0),
        sys_count: AtomicUsize::new(0),
        capacity: cap,
        overflow: HewOverflowPolicy::Coalesce,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        message_drop_fn: None,
        closed: std::sync::atomic::AtomicBool::new(false),
        stop_requested: std::sync::atomic::AtomicBool::new(false),
        terminal_reclaiming: Mutex::new(()),
        block_wait: Mutex::new(()),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: true,
    }))
}

unsafe fn coalesce_message_key(
    key_fn: Option<HewCoalesceKeyFn>,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    envelope: *mut HewMsgEnvelope,
) -> u64 {
    if let Some(key_fn) = key_fn {
        let (payload, payload_size) = if data.is_null() && !envelope.is_null() {
            // SAFETY: the queued node owns a live envelope reference while the mailbox is locked.
            unsafe {
                (
                    hew_msg_envelope_payload_ptr(envelope),
                    (*envelope).payload_size,
                )
            }
        } else {
            (data, data_size)
        };
        // SAFETY: caller guarantees key function and payload pointers are valid.
        unsafe { key_fn(msg_type, payload, payload_size) }
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
    message_drop_fn: Option<HewMessageDropFn>,
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

        if (*node).envelope.is_null() {
            if let Some(drop_fn) = message_drop_fn {
                drop_fn((*node).msg_type, (*node).data, (*node).data_size);
            }
            libc::free((*node).data);
        } else {
            hew_msg_envelope_release((*node).envelope);
            (*node).envelope = ptr::null_mut();
        }
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

unsafe fn hew_msg_node_free_with_message_drop(
    node: *mut HewMsgNode,
    message_drop_fn: Option<HewMessageDropFn>,
) {
    if node.is_null() {
        return;
    }
    // SAFETY: caller transfers exclusive ownership of `node`.
    unsafe {
        if (*node).envelope.is_null() {
            if let Some(drop_fn) = message_drop_fn {
                drop_fn((*node).msg_type, (*node).data, (*node).data_size);
            }
        }
        hew_msg_node_free(node);
    }
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

/// Register the typed destructor used when queued legacy payloads are evicted.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer and `drop_fn`, when present, must match
/// every user-message payload shape routed to this actor.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_set_message_drop_fn(
    mb: *mut HewMailbox,
    drop_fn: Option<HewMessageDropFn>,
) {
    // SAFETY: caller guarantees `mb` is valid.
    unsafe { (*mb).message_drop_fn = drop_fn };
}

unsafe fn consume_dropped_incoming(
    mb: &HewMailbox,
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
) {
    if let Some(drop_fn) = mb.message_drop_fn {
        // SAFETY: send_with_overflow's caller guarantees the payload bytes are
        // valid for the duration of the call. DropNew reports success, so the
        // runtime must discharge the prepared owner before returning.
        unsafe { drop_fn(msg_type, data.cast_mut(), data_size) };
    }
}

// ── Send (producer side) ────────────────────────────────────────────────

/// Outcome of an overflow-policy-aware send into the user queue.
///
/// FFI entry points map these variants to their own return conventions.
/// `pub(crate)` (rather than private) because the fire-and-forget local
/// send seam (`actor::actor_send_result_internal_reply`) needs the raw
/// variant, not just a collapsed status code: whether the actor must be
/// woken/scheduled depends on whether a node actually reached the queue
/// (`Enqueued`/`Coalesced`/`DroppedOld`), which is a different question
/// from whether the call should report success to the caller (`Dropped`
/// also reports success, but nothing was queued, so nothing to schedule).
pub(crate) enum SendOutcome {
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

    // Lock-free bounded mailboxes must claim capacity before allocating or
    // publishing a node so concurrent producers cannot all pass the same
    // check. Slow-path policies retain their mutex-backed handling below.
    if mb.capacity > 0 && !mb.use_slow_path {
        if !try_reserve_fast_path_capacity(mb) {
            return match mb.overflow {
                HewOverflowPolicy::DropNew => {
                    // SAFETY: caller guarantees `data` is valid for this send.
                    unsafe { consume_dropped_incoming(mb, msg_type, data, data_size) };
                    SendOutcome::Dropped
                }
                HewOverflowPolicy::Fail => SendOutcome::Failed,
                HewOverflowPolicy::Block
                | HewOverflowPolicy::DropOld
                | HewOverflowPolicy::Coalesce => {
                    unreachable!("complex overflow policies use the slow path")
                }
            };
        }

        // SAFETY: `data` validity guaranteed by caller.
        let node = unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
        if node.is_null() {
            release_fast_path_capacity(mb);
            return SendOutcome::Oom;
        }

        // SAFETY: `node` was just allocated with next == null and is owned here;
        // the successful CAS above reserved its count slot.
        unsafe { enqueue_reserved_fast_user_node(mb, node) };
        return SendOutcome::Enqueued;
    }

    // Bounded mutex-backed mailboxes use the queue length as their sole
    // admission predicate.  Keep this guard through count publication and
    // enqueue: `count` can be published before a lock-free node is linked,
    // but treating it as capacity here lets another producer observe "full"
    // while the VecDeque is still empty and over-admit.
    if mb.capacity > 0 && mb.use_slow_path {
        let mut q = mb.slow_path.lock_or_recover();
        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
        if len >= mb.capacity {
            match mb.overflow {
                HewOverflowPolicy::DropNew => {
                    // SAFETY: caller guarantees `data` is valid for this send.
                    unsafe { consume_dropped_incoming(mb, msg_type, data, data_size) };
                    return SendOutcome::Dropped;
                }
                HewOverflowPolicy::Fail => return SendOutcome::Failed,
                HewOverflowPolicy::Block => {
                    // Non-blocking callers (try_send) must not wait.
                    if non_blocking {
                        return SendOutcome::Failed;
                    }
                    // Wait on condvar until space is available.
                    loop {
                        if mb.closed.load(Ordering::Acquire) {
                            return SendOutcome::Closed;
                        }
                        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                        if len < mb.capacity {
                            break;
                        }
                        // Join the condvar predicate mutex before releasing the
                        // queue. Close/capacity notifiers take `block_wait`, so
                        // they cannot pass this sender's final checks before
                        // it atomically parks.
                        let wait = mb.block_wait.lock_or_recover();
                        if mb.closed.load(Ordering::Acquire) {
                            return SendOutcome::Closed;
                        }
                        #[cfg(test)]
                        run_block_pre_wait_hook(mb);
                        drop(q);
                        let wait = mb.not_full.wait_or_recover(wait);
                        drop(wait);
                        q = mb.slow_path.lock_or_recover();
                    }
                    // SAFETY: `data` validity guaranteed by caller.
                    let node = unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                    if node.is_null() {
                        return SendOutcome::Oom;
                    }
                    enqueue_bounded_slow_path_node(mb, &mut q, node);
                    drop(q);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return SendOutcome::Enqueued;
                }
                HewOverflowPolicy::Coalesce => {
                    // Scan for an existing message with the same coalesce key.
                    // SAFETY: `data` validity guaranteed by caller.
                    let incoming_key = unsafe {
                        coalesce_message_key(
                            mb.coalesce_key_fn,
                            msg_type,
                            data.cast_mut(),
                            data_size,
                            ptr::null_mut(),
                        )
                    };
                    let found = q
                        .user_queue
                        .iter()
                        .find(|&&n| {
                            // SAFETY: all nodes in the queue were allocated by msg_node_alloc.
                            unsafe {
                                (*n).msg_type == msg_type
                                    && coalesce_message_key(
                                        mb.coalesce_key_fn,
                                        (*n).msg_type,
                                        (*n).data,
                                        (*n).data_size,
                                        (*n).envelope,
                                    ) == incoming_key
                            }
                        })
                        .copied();
                    if let Some(existing) = found {
                        // SAFETY: `existing` is valid; replace its payload.
                        let ok = unsafe {
                            replace_node_payload(
                                existing,
                                msg_type,
                                data,
                                data_size,
                                reply_channel,
                                mb.message_drop_fn,
                            )
                        };
                        if !ok {
                            return SendOutcome::Oom;
                        }
                        return SendOutcome::Coalesced;
                    }
                    // No matching key — use configured fallback policy.
                    match normalize_coalesce_fallback(mb.coalesce_fallback) {
                        HewOverflowPolicy::DropNew => {
                            // SAFETY: caller guarantees `data` is valid for this send.
                            unsafe { consume_dropped_incoming(mb, msg_type, data, data_size) };
                            return SendOutcome::Dropped;
                        }
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
                                let wait = mb.block_wait.lock_or_recover();
                                if mb.closed.load(Ordering::Acquire) {
                                    return SendOutcome::Closed;
                                }
                                #[cfg(test)]
                                run_block_pre_wait_hook(mb);
                                drop(q);
                                let wait = mb.not_full.wait_or_recover(wait);
                                drop(wait);
                                q = mb.slow_path.lock_or_recover();
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node =
                                unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                            if node.is_null() {
                                return SendOutcome::Oom;
                            }
                            enqueue_bounded_slow_path_node(mb, &mut q, node);
                            drop(q);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return SendOutcome::Enqueued;
                        }
                        HewOverflowPolicy::DropOld => {
                            // Lock already held from Coalesce scan.
                            if let Some(old) = q.user_queue.pop_front() {
                                // SAFETY: node was allocated by msg_node_alloc.
                                unsafe {
                                    hew_msg_node_free_with_message_drop(old, mb.message_drop_fn);
                                };
                                mb.count.fetch_sub(1, Ordering::Release);
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node =
                                unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                            if node.is_null() {
                                return SendOutcome::Oom;
                            }
                            enqueue_bounded_slow_path_node(mb, &mut q, node);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return SendOutcome::DroppedOld;
                        }
                        HewOverflowPolicy::Coalesce => unreachable!(),
                    }
                }
                HewOverflowPolicy::DropOld => {
                    if drop_old_alloc_under_lock {
                        // hew_mailbox_send path: the admission lock is already
                        // held, then allocate the replacement.
                        if let Some(old) = q.user_queue.pop_front() {
                            // SAFETY: node was allocated by msg_node_alloc.
                            unsafe { hew_msg_node_free_with_message_drop(old, mb.message_drop_fn) };
                            mb.count.fetch_sub(1, Ordering::Release);
                        }
                        // SAFETY: `data` validity guaranteed by caller.
                        let node =
                            unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                        if node.is_null() {
                            return SendOutcome::Oom;
                        }
                        enqueue_bounded_slow_path_node(mb, &mut q, node);
                    } else {
                        // The historical try_push path allocated first.  The
                        // node is now allocated while holding the admission
                        // lock so the below-capacity path has the same atomic
                        // decision-to-publication boundary.
                        // SAFETY: `data` validity guaranteed by caller.
                        let node =
                            unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
                        if node.is_null() {
                            return SendOutcome::Oom;
                        }
                        if let Some(old) = q.user_queue.pop_front() {
                            // SAFETY: node was allocated by msg_node_alloc.
                            unsafe { hew_msg_node_free_with_message_drop(old, mb.message_drop_fn) };
                            mb.count.fetch_sub(1, Ordering::Release);
                        }
                        enqueue_bounded_slow_path_node(mb, &mut q, node);
                    }
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return SendOutcome::DroppedOld;
                }
            }
        } else {
            // Below capacity is still a slow-path admission.  Do not delegate
            // to enqueue_user_node: its lock-free publication order would
            // expose `count` before this VecDeque node is protected.
            // SAFETY: `data` validity guaranteed by caller.
            let node = unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
            if node.is_null() {
                return SendOutcome::Oom;
            }
            enqueue_bounded_slow_path_node(mb, &mut q, node);
            drop(q);
            update_high_water_mark(mb);
            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
            return SendOutcome::Enqueued;
        }
    }

    // Unbounded fast path, or a slow-path mailbox currently below capacity.
    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, data_size, reply_channel) };
    if node.is_null() {
        return SendOutcome::Oom;
    }

    // SAFETY: `node` was just allocated with next == null and is owned here.
    unsafe { enqueue_user_node(mb, node) };
    SendOutcome::Enqueued
}

/// Enqueue an owned, fully-initialised user message node into the
/// mailbox's user queue and update the shared counters.
///
/// Routes to the slow-path mutex queue or the lock-free fast queue
/// depending on `mb.use_slow_path`. The shared `count` reservation is
/// published before the node becomes reachable, then the high-water mark and
/// global sent counter are updated. Used for unbounded sends and
/// mutex-backed sends below capacity; bounded lock-free sends use
/// [`enqueue_reserved_fast_user_node`] because their CAS reservation has
/// already incremented `count`.
///
/// # Safety
///
/// `node` must be a valid, exclusively-owned [`HewMsgNode`] with
/// `node.next == null`. Ownership of the node transfers into the queue.
unsafe fn enqueue_user_node(mb: &HewMailbox, node: *mut HewMsgNode) {
    #[cfg(test)]
    // SAFETY: production publishes the count reservation before reachability.
    unsafe {
        enqueue_user_node_inner(mb, node, true);
    }

    #[cfg(not(test))]
    {
        mb.count.fetch_add(1, Ordering::Release);
        // SAFETY: caller transfers an exclusively owned node.
        unsafe { link_user_node(mb, node) };
        update_high_water_mark(mb);
        MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
    }
}

/// Publish an already-allocated node to a bounded mutex-backed mailbox.
///
/// The caller must hold `slow_path` continuously from its capacity decision
/// through this publication.  Unlike the lock-free queue, `count` is only an
/// observability counter here: `user_queue.len()` is the sole admission
/// predicate, so a producer whose count update is visible can never leave an
/// apparently empty queue for another slow-path producer to over-admit.
fn enqueue_bounded_slow_path_node(mb: &HewMailbox, q: &mut SlowPathQueue, node: *mut HewMsgNode) {
    debug_assert!(mb.use_slow_path);
    debug_assert!(mb.capacity > 0);
    debug_assert!(i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX) < mb.capacity);

    mb.count.fetch_add(1, Ordering::Release);
    q.user_queue.push_back(node);
}

/// Link an exclusively owned node into the selected user queue.
///
/// # Safety
///
/// `node` must be a valid, exclusively-owned [`HewMsgNode`] with
/// `node.next == null`. Ownership transfers into the queue.
unsafe fn link_user_node(mb: &HewMailbox, node: *mut HewMsgNode) {
    if mb.use_slow_path {
        let mut q = mb.slow_path.lock_or_recover();
        q.user_queue.push_back(node);
    } else {
        // SAFETY: `node` was allocated with next == null.
        unsafe { mb.user_fast.enqueue(node) };
    }
}

#[cfg(test)]
unsafe fn enqueue_user_node_inner(
    mb: &HewMailbox,
    node: *mut HewMsgNode,
    publish_count_first: bool,
) {
    // SAFETY: production publishes the count reservation before reachability.
    if publish_count_first {
        mb.count.fetch_add(1, Ordering::Release);
        run_user_count_publication_hook(node);
    }
    // SAFETY: caller transfers an exclusively owned node.
    unsafe { link_user_node(mb, node) };
    if !publish_count_first {
        run_user_count_publication_hook(node);
        mb.count.fetch_add(1, Ordering::Release);
    }
    update_high_water_mark(mb);
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
}

/// Publish a node after [`try_reserve_fast_path_capacity`] has already
/// incremented `count`.
///
/// # Safety
///
/// `node` must be a valid, exclusively-owned [`HewMsgNode`] with
/// `node.next == null`, and the caller must own one capacity reservation.
unsafe fn enqueue_reserved_fast_user_node(mb: &HewMailbox, node: *mut HewMsgNode) {
    debug_assert!(mb.capacity > 0);
    debug_assert!(!mb.use_slow_path);

    // SAFETY: `node` was allocated with next == null.
    unsafe { mb.user_fast.enqueue(node) };
    update_high_water_mark(mb);
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
}

/// Overflow-policy-aware enqueue for the **envelope-mode alias path**.
///
/// The caller transfers exactly one refcount on `envelope`. This
/// function builds a single envelope-mode [`HewMsgNode`] up front and
/// from that point routes the envelope refcount through exactly one of
/// two release mechanisms, so the buffer is released **exactly once**
/// on every exit:
///
/// 1. **Node allocation failed** — the node never took ownership, so we
///    release the caller-transferred envelope refcount directly via
///    [`hew_msg_envelope_release`].
/// 2. **Node allocated** — the node owns the refcount. Every reject /
///    drop exit frees the node through [`hew_msg_node_free`] (which
///    calls [`hew_msg_envelope_release`] once); every enqueue exit
///    hands the node to the queue, where the eventual
///    [`hew_msg_node_free`] (dispatch, drain, close, supervisor-cancel,
///    session-reset, mailbox-free) performs the single release.
///
/// This is a deliberate parallel to [`send_with_overflow`] rather than a
/// shared body: copy-mode allocates lazily at each reject site (to skip
/// a `malloc`+`memcpy` that would be discarded) and byte-coalesces
/// matching payloads in place. Envelope payloads are pre-built, opaque,
/// refcounted buffers that cannot be lazily synthesised or byte-replaced,
/// so the alias path allocates once and applies the coalesce *fallback*
/// policy without the key-match/replace step.
///
/// # Safety
///
/// - `mb` must reference a valid, live [`HewMailbox`].
/// - `envelope` carries exactly one caller-transferred refcount (it may
///   be null, in which case the node delivers an empty payload and the
///   node free path is a no-op `libc::free(null)`).
#[cfg(not(target_arch = "wasm32"))]
#[expect(
    clippy::too_many_lines,
    reason = "mirrors send_with_overflow's overflow-policy dispatch; splitting would scatter the single-release exit enumeration"
)]
unsafe fn send_aliased_with_overflow(
    mb: &HewMailbox,
    msg_type: i32,
    envelope: *mut HewMsgEnvelope,
    non_blocking: bool,
) -> SendOutcome {
    // Build the envelope-mode node up front. On success the node owns
    // the single caller-transferred envelope refcount; every subsequent
    // exit releases it exactly once by routing the node through
    // `hew_msg_node_free`.
    // SAFETY: `envelope` carries one refcount per the alias-send contract.
    let node = unsafe { msg_node_alloc_aliased(msg_type, envelope, ptr::null_mut()) };
    if node.is_null() {
        // EXIT(alloc-failure): the node never took ownership of the
        // envelope, so release the caller-transferred refcount here —
        // exactly once.
        // SAFETY: we still own the single refcount transferred in.
        unsafe { hew_msg_envelope_release(envelope) };
        return SendOutcome::Oom;
    }

    // EXIT(closed): the destination mailbox is closed; free the node
    // (single envelope release) and report closed.
    if mb.closed.load(Ordering::Acquire) {
        // SAFETY: `node` is owned here and was allocated by msg_node_alloc_aliased.
        unsafe { hew_msg_node_free(node) };
        return SendOutcome::Closed;
    }

    // The node is already allocated on the alias path, but lock-free bounded
    // mailboxes still reserve capacity atomically before publishing it.
    if mb.capacity > 0 && !mb.use_slow_path {
        if !try_reserve_fast_path_capacity(mb) {
            // SAFETY: `node` is still exclusively owned here.
            unsafe { hew_msg_node_free(node) };
            return match mb.overflow {
                HewOverflowPolicy::DropNew => SendOutcome::Dropped,
                HewOverflowPolicy::Fail => SendOutcome::Failed,
                HewOverflowPolicy::Block
                | HewOverflowPolicy::DropOld
                | HewOverflowPolicy::Coalesce => {
                    unreachable!("complex overflow policies use the slow path")
                }
            };
        }

        // SAFETY: `node` is owned here with next == null; the successful CAS
        // above reserved its count slot.
        unsafe { enqueue_reserved_fast_user_node(mb, node) };
        return SendOutcome::Enqueued;
    }

    // Bounded slow-path policies admit from the mutex-protected queue length,
    // never from `count`.  Retain this guard from the decision until the node
    // and its observability count are published.
    if mb.capacity > 0 && mb.use_slow_path {
        let mut q = mb.slow_path.lock_or_recover();
        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
        if len >= mb.capacity {
            match mb.overflow {
                HewOverflowPolicy::DropNew => {
                    // EXIT(drop-new): reject the new message.
                    // SAFETY: `node` owned here.
                    unsafe { hew_msg_node_free(node) };
                    return SendOutcome::Dropped;
                }
                HewOverflowPolicy::Fail => {
                    // EXIT(fail): policy rejects on overflow.
                    // SAFETY: `node` owned here.
                    unsafe { hew_msg_node_free(node) };
                    return SendOutcome::Failed;
                }
                HewOverflowPolicy::Block => {
                    if non_blocking {
                        // EXIT(block-nonblocking): caller must not wait.
                        // SAFETY: `node` owned here.
                        unsafe { hew_msg_node_free(node) };
                        return SendOutcome::Failed;
                    }
                    loop {
                        if mb.closed.load(Ordering::Acquire) {
                            drop(q);
                            // EXIT(block-closed-while-waiting): mailbox
                            // closed under us.
                            // SAFETY: `node` owned here.
                            unsafe { hew_msg_node_free(node) };
                            return SendOutcome::Closed;
                        }
                        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                        if len < mb.capacity {
                            break;
                        }
                        let wait = mb.block_wait.lock_or_recover();
                        if mb.closed.load(Ordering::Acquire) {
                            drop(wait);
                            drop(q);
                            // SAFETY: `node` remains owned here.
                            unsafe { hew_msg_node_free(node) };
                            return SendOutcome::Closed;
                        }
                        #[cfg(test)]
                        run_block_pre_wait_hook(mb);
                        drop(q);
                        let wait = mb.not_full.wait_or_recover(wait);
                        drop(wait);
                        q = mb.slow_path.lock_or_recover();
                    }
                    // EXIT(block-enqueued): capacity freed; node enqueued.
                    enqueue_bounded_slow_path_node(mb, &mut q, node);
                    drop(q);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return SendOutcome::Enqueued;
                }
                HewOverflowPolicy::Coalesce => {
                    // Envelope payloads are opaque refcounted buffers and
                    // cannot be byte-coalesced in place, so apply the
                    // configured coalesce *fallback* policy directly.
                    match normalize_coalesce_fallback(mb.coalesce_fallback) {
                        HewOverflowPolicy::DropNew => {
                            drop(q);
                            // EXIT(coalesce-fallback-drop-new).
                            // SAFETY: `node` owned here.
                            unsafe { hew_msg_node_free(node) };
                            return SendOutcome::Dropped;
                        }
                        HewOverflowPolicy::Fail => {
                            drop(q);
                            // EXIT(coalesce-fallback-fail).
                            // SAFETY: `node` owned here.
                            unsafe { hew_msg_node_free(node) };
                            return SendOutcome::Failed;
                        }
                        HewOverflowPolicy::Block => {
                            if non_blocking {
                                drop(q);
                                // EXIT(coalesce-fallback-block-nonblocking).
                                // SAFETY: `node` owned here.
                                unsafe { hew_msg_node_free(node) };
                                return SendOutcome::Failed;
                            }
                            loop {
                                if mb.closed.load(Ordering::Acquire) {
                                    drop(q);
                                    // EXIT(coalesce-fallback-block-closed).
                                    // SAFETY: `node` owned here.
                                    unsafe { hew_msg_node_free(node) };
                                    return SendOutcome::Closed;
                                }
                                let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                                if len < mb.capacity {
                                    break;
                                }
                                let wait = mb.block_wait.lock_or_recover();
                                if mb.closed.load(Ordering::Acquire) {
                                    drop(wait);
                                    drop(q);
                                    // SAFETY: `node` remains owned here.
                                    unsafe { hew_msg_node_free(node) };
                                    return SendOutcome::Closed;
                                }
                                #[cfg(test)]
                                run_block_pre_wait_hook(mb);
                                drop(q);
                                let wait = mb.not_full.wait_or_recover(wait);
                                drop(wait);
                                q = mb.slow_path.lock_or_recover();
                            }
                            // EXIT(coalesce-fallback-block-enqueued).
                            enqueue_bounded_slow_path_node(mb, &mut q, node);
                            drop(q);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return SendOutcome::Enqueued;
                        }
                        HewOverflowPolicy::DropOld => {
                            if let Some(old) = q.user_queue.pop_front() {
                                // SAFETY: `old` was allocated by one of the
                                // msg_node_alloc family; its own payload /
                                // envelope is released exactly once here.
                                unsafe {
                                    hew_msg_node_free_with_message_drop(old, mb.message_drop_fn);
                                };
                                mb.count.fetch_sub(1, Ordering::Release);
                            }
                            // EXIT(coalesce-fallback-drop-old): old freed,
                            // new node enqueued.
                            enqueue_bounded_slow_path_node(mb, &mut q, node);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return SendOutcome::DroppedOld;
                        }
                        HewOverflowPolicy::Coalesce => {
                            unreachable!("normalize_coalesce_fallback never returns Coalesce")
                        }
                    }
                }
                HewOverflowPolicy::DropOld => {
                    if let Some(old) = q.user_queue.pop_front() {
                        // SAFETY: `old` was allocated by one of the
                        // msg_node_alloc family; released exactly once here.
                        unsafe {
                            hew_msg_node_free_with_message_drop(old, mb.message_drop_fn);
                        };
                        mb.count.fetch_sub(1, Ordering::Release);
                    }
                    // EXIT(drop-old): old freed, new node enqueued.
                    enqueue_bounded_slow_path_node(mb, &mut q, node);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return SendOutcome::DroppedOld;
                }
            }
        } else {
            enqueue_bounded_slow_path_node(mb, &mut q, node);
            drop(q);
            update_high_water_mark(mb);
            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
            return SendOutcome::Enqueued;
        }
    }

    // EXIT(fast-path): unbounded, or slow-path below capacity; node enqueued.
    // SAFETY: `node` owned here with next == null.
    unsafe { enqueue_user_node(mb, node) };
    SendOutcome::Enqueued
}

/// Send an envelope-aliased message to the mailbox.
///
/// The caller transfers exactly one refcount on `envelope`. Delivery
/// builds an envelope-mode [`HewMsgNode`] that owns that refcount and
/// enqueues it into the user queue, applying the mailbox's overflow
/// policy. The receiver borrows the payload read-only via
/// [`hew_msg_envelope_payload_ptr`]; the single refcount is released
/// exactly once when the node is freed via [`hew_msg_node_free`]
/// (on dispatch, drain, close, supervisor-cancel, session-reset, or
/// mailbox-free) — see [`send_aliased_with_overflow`] for the full
/// single-release exit enumeration.
///
/// Returns `0` ([`HewError::Ok`]) on success, `-2`
/// ([`HewError::ErrActorStopped`]) if the mailbox is null or closed,
/// `-1` ([`HewError::ErrMailboxFull`]) if bounded and the overflow
/// policy rejects, or `-5` ([`HewError::ErrOom`]) on allocation failure.
/// On every non-success outcome the envelope refcount is still released
/// exactly once, so the buffer never leaks and is never double-freed.
///
/// # Safety
///
/// - `mb` must be a valid mailbox pointer or null.
/// - `envelope` must carry exactly one caller-transferred refcount
///   obtained from [`hew_msg_envelope_new`] / [`hew_msg_envelope_clone_alias`],
///   or be null.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send_aliased(
    mb: *mut HewMailbox,
    msg_type: i32,
    envelope: *mut HewMsgEnvelope,
) -> i32 {
    if mb.is_null() {
        // EXIT(null-mailbox): no destination. Release the
        // caller-transferred refcount exactly once so the buffer does
        // not leak, then report the actor as stopped.
        if !envelope.is_null() {
            // SAFETY: we own the single refcount transferred in.
            unsafe { hew_msg_envelope_release(envelope) };
        }
        return HewError::ErrActorStopped as i32;
    }
    // SAFETY: Caller guarantees `mb` is valid (non-null checked above).
    let mb = unsafe { &*mb };
    // SAFETY: `envelope` carries one refcount per the alias-send contract;
    // `send_aliased_with_overflow` consumes it on every exit.
    match unsafe { send_aliased_with_overflow(mb, msg_type, envelope, false) } {
        SendOutcome::Enqueued | SendOutcome::Coalesced | SendOutcome::DroppedOld => {
            HewError::Ok as i32
        }
        SendOutcome::Closed => HewError::ErrActorStopped as i32,
        SendOutcome::Dropped | SendOutcome::Failed => HewError::ErrMailboxFull as i32,
        SendOutcome::Oom => HewError::ErrOom as i32,
    }
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

/// Send a fire-and-forget message to the mailbox (user queue), deep-copying
/// `data`. Returns the raw [`SendOutcome`] rather than a collapsed status
/// code: the caller (`actor::actor_send_result_internal_reply`) needs both
/// (a) the `HewError` status to report, and (b) whether a node actually
/// reached the queue, since only an actual enqueue needs to wake/schedule
/// the actor. A policy drop reports a distinct loss status to checked sends
/// but enqueued nothing.
///
/// A policy loss (`DropNew`, `DropOld`, or `Coalesce`) and a genuine failure
/// (`Fail`, closed mailbox, or allocation failure) must keep distinct statuses
/// so the language surface can report the exact outcome.
///
/// Not an FFI entry point — used only by the local no-reply-channel send
/// path ([`actor::actor_send_result_internal_reply`](crate::actor)) that
/// backs `Terminator::Send` (fire-and-forget `w.msg()` sends) and
/// `hew_actor_send`. The ask/reply-channel path keeps calling
/// [`hew_mailbox_send_with_reply`], whose contract is intentionally
/// untouched: an ask that silently dropped its message would leave the
/// caller's reply channel waiting forever for a reply that will never
/// arrive, so a full mailbox must stay caller-visible there regardless of
/// overflow policy.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
pub(crate) unsafe fn hew_mailbox_send_fire_and_forget(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> SendOutcome {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb_ref = unsafe { &*mb };
    // SAFETY: Caller guarantees `data` points to `size` readable bytes.
    unsafe { send_with_overflow(mb_ref, msg_type, data, size, true, false, ptr::null_mut()) }
}

/// The block-send registration owns the copied message and parked continuation.
pub(crate) const MAILBOX_AWAIT_SEND_SUSPEND: i32 = 0;
/// The message was admitted immediately; the caller must continue without parking.
pub(crate) const MAILBOX_AWAIT_SEND_READY: i32 = 1;

/// Register a cooperative producer wait for a bounded `Block` mailbox.
///
/// On a full mailbox this copies the message into a FIFO waiter, retains
/// `slot`, and returns [`MAILBOX_AWAIT_SEND_SUSPEND`]. The consumer admits and
/// wakes one waiter whenever it frees a queue slot. An immediately available
/// slot admits the copied node and returns [`MAILBOX_AWAIT_SEND_READY`].
///
/// # Safety
///
/// `mb`, `actor`, and `slot` must remain valid for registration; `data` must
/// cover `size` readable bytes (or be null for zero size).
pub(crate) unsafe fn mailbox_await_send(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    actor: *mut crate::actor::HewActor,
    slot: *mut HewReadSlot,
) -> i32 {
    if mb.is_null() || slot.is_null() {
        return HewError::ErrClosed as i32;
    }
    // SAFETY: caller guarantees a live mailbox.
    let mailbox = unsafe { &*mb };
    if mailbox.closed.load(Ordering::Acquire) {
        return HewError::ErrActorStopped as i32;
    }
    if mailbox.capacity <= 0 || mailbox.overflow != HewOverflowPolicy::Block {
        return HewError::ErrMailboxFull as i32;
    }

    let mut queue = mailbox.slow_path.lock_or_recover();
    if mailbox.closed.load(Ordering::Acquire) {
        return HewError::ErrActorStopped as i32;
    }
    // SAFETY: the caller guarantees the readable payload range.
    let node = unsafe { msg_node_alloc(msg_type, data, size, ptr::null_mut()) };
    if node.is_null() {
        return HewError::ErrOom as i32;
    }
    if i64::try_from(queue.user_queue.len()).unwrap_or(i64::MAX) < mailbox.capacity {
        enqueue_bounded_slow_path_node(mailbox, &mut queue, node);
        drop(queue);
        update_high_water_mark(mailbox);
        MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
        return MAILBOX_AWAIT_SEND_READY;
    }

    // Join the independent waiter lock while the queue is still held so a
    // consumer cannot free capacity between the final predicate check and the
    // registration. Close never takes the queue lock and therefore cannot
    // deadlock with callbacks that close from inside a coalesce traversal.
    let mut waiters = mailbox.blocked_senders.lock_or_recover();
    if mailbox.closed.load(Ordering::Acquire) {
        drop(waiters);
        drop(queue);
        // SAFETY: the node was never published and remains exclusively owned.
        unsafe { hew_msg_node_free_with_message_drop(node, mailbox.message_drop_fn) };
        return HewError::ErrActorStopped as i32;
    }
    // SAFETY: the caller owns the creator ref, so the slot is live to retain.
    unsafe { read_slot_retain(slot) };
    waiters.push_back(BlockedSender { actor, slot, node });
    MAILBOX_AWAIT_SEND_SUSPEND
}

/// Submit an ask without parking the scheduler worker when a bounded `Block`
/// mailbox is full.
///
/// A full mailbox takes ownership of the copied node and its reply-channel
/// sender reference in the same FIFO used by cooperative fire-and-forget
/// sends. Unlike a tell waiter, an ask has no capacity-wait read slot to wake:
/// its caller is already suspended on the reply channel, which is eventually
/// resolved by the admitted handler or orphaned when mailbox teardown frees
/// the pending node.
///
/// # Safety
///
/// `mb` must be live, `data` must cover `size` readable bytes (or be null for
/// zero size), and `reply_channel` must carry the sender reference transferred
/// to the mailbox by the ask submission path.
pub(crate) unsafe fn mailbox_send_with_reply_cooperative(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> i32 {
    if mb.is_null() || reply_channel.is_null() {
        return HewError::ErrClosed as i32;
    }
    // SAFETY: caller guarantees a live mailbox.
    let mailbox = unsafe { &*mb };
    if mailbox.closed.load(Ordering::Acquire) {
        return HewError::ErrActorStopped as i32;
    }
    if mailbox.capacity <= 0 || mailbox.overflow != HewOverflowPolicy::Block {
        // SAFETY: caller guarantees the ordinary send preconditions.
        return unsafe { hew_mailbox_send_with_reply(mb, msg_type, data, size, reply_channel) };
    }

    let mut queue = mailbox.slow_path.lock_or_recover();
    if mailbox.closed.load(Ordering::Acquire) {
        return HewError::ErrActorStopped as i32;
    }
    // SAFETY: the caller guarantees the readable payload and live channel.
    let node = unsafe { msg_node_alloc(msg_type, data, size, reply_channel) };
    if node.is_null() {
        return HewError::ErrOom as i32;
    }
    if i64::try_from(queue.user_queue.len()).unwrap_or(i64::MAX) < mailbox.capacity {
        enqueue_bounded_slow_path_node(mailbox, &mut queue, node);
        drop(queue);
        update_high_water_mark(mailbox);
        MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
        return HewError::Ok as i32;
    }

    let mut waiters = mailbox.blocked_senders.lock_or_recover();
    if mailbox.closed.load(Ordering::Acquire) {
        drop(waiters);
        drop(queue);
        // SAFETY: the node was never published and remains exclusively owned.
        unsafe { hew_msg_node_free_with_message_drop(node, mailbox.message_drop_fn) };
        return HewError::ErrActorStopped as i32;
    }
    waiters.push_back(BlockedSender {
        actor: ptr::null_mut(),
        slot: ptr::null_mut(),
        node,
    });
    HewError::Ok as i32
}

/// Remove an abandoned cooperative block-send registration. Idempotent when
/// admission or close already consumed the waiter.
///
/// # Safety
///
/// `mb` and `slot` must be live pointers supplied to [`mailbox_await_send`].
pub(crate) unsafe fn mailbox_detach_await_send(mb: *mut HewMailbox, slot: *mut HewReadSlot) {
    if mb.is_null() || slot.is_null() {
        return;
    }
    // SAFETY: caller guarantees a live mailbox.
    let mailbox = unsafe { &*mb };
    let removed = {
        let mut waiters = mailbox.blocked_senders.lock_or_recover();
        waiters
            .iter()
            .position(|waiter| waiter.slot == slot)
            .and_then(|position| waiters.remove(position))
    };
    if let Some(waiter) = removed {
        // SAFETY: removal transferred exclusive ownership of the unpublished node.
        unsafe { hew_msg_node_free_with_message_drop(waiter.node, mailbox.message_drop_fn) };
        // SAFETY: the waiter owned exactly one retained slot ref.
        unsafe { hew_read_slot_free(waiter.slot) };
    }
}

/// Resolve one removed waiter after releasing all mailbox locks.
///
/// # Safety
///
/// The caller must own the waiter's retained slot ref. `actor` may be stale;
/// `enqueue_resume` validates it against the live registry before use.
unsafe fn wake_blocked_sender(waiter: &BlockedSender, status: ReadStatus) {
    if waiter.slot.is_null() {
        return;
    }
    // SAFETY: the waiter owns a retained live slot ref.
    let should_wake = unsafe { read_slot_deposit_status(waiter.slot, status) };
    if should_wake {
        // SAFETY: enqueue_resume performs the liveness validation for this raw actor pointer.
        unsafe { crate::scheduler::enqueue_resume(waiter.actor, ptr::null_mut()) };
    }
    // SAFETY: the waiter owns exactly one retained slot ref.
    unsafe { hew_read_slot_free(waiter.slot) };
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

/// Non-blocking send that **bypasses the bounded-capacity overflow policy** and
/// appends to the tail of the *user* queue, so a terminal/out-of-band event is
/// delivered even when the mailbox is at capacity — without dropping, blocking,
/// or evicting any queued message.
///
/// This is the guaranteed-delivery channel for events that must never be lost
/// under data backpressure (the active-mode reactor's one-shot `on_close`). It
/// enqueues into the **user** queue (not the priority system queue) precisely so
/// per-mailbox FIFO is preserved: the terminal event is ordered *after* every
/// already-queued `on_data`, never ahead of it. At most one extra node is
/// admitted past capacity per call, and the connection it belongs to is
/// unregistered immediately afterward, so the overshoot is bounded by one slot
/// per terminating connection and the queue drains back under capacity as the
/// actor consumes it (the consumer's `not_full.notify_one` on each recv still
/// applies).
///
/// A *closed* mailbox is still refused: a closed mailbox means the actor is
/// already terminating, so the terminal event is moot and the caller may treat
/// `ErrClosed` as "no delivery needed" (no leak — the actor is going away).
///
/// Returns `0` ([`HewError::Ok`]) once the node is enqueued, `-4`
/// ([`HewError::ErrClosed`]) if the mailbox is closed, or `-5`
/// ([`HewError::ErrOom`]) if node allocation fails.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn hew_mailbox_send_guaranteed(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *const c_void,
    size: usize,
) -> i32 {
    if mb.is_null() {
        return HewError::ErrClosed as i32;
    }
    // SAFETY: Caller guarantees `mb` is valid when non-null.
    let mb = unsafe { &*mb };

    if mb.closed.load(Ordering::Acquire) {
        return HewError::ErrClosed as i32;
    }

    // SAFETY: `data` validity guaranteed by caller (or null when size == 0).
    let node = unsafe { msg_node_alloc(msg_type, data, size, ptr::null_mut()) };
    if node.is_null() {
        return HewError::ErrOom as i32;
    }

    // Append to the user queue unconditionally, skipping the bounded-capacity
    // check in `send_with_overflow`. `enqueue_user_node` routes to the slow or
    // fast queue exactly as the policy-aware path does, so the consumer dequeues
    // it in FIFO order behind any already-queued message.
    // SAFETY: `node` was just allocated with next == null and is owned here.
    unsafe { enqueue_user_node(mb, node) };
    HewError::Ok as i32
}

/// Send a runtime lifecycle signal, bypassing capacity limits.
///
/// `sys_msg` must be a [`HewSysMsg`] discriminant; any other value is refused
/// (fail-closed) rather than enqueued, so the system queue can only ever carry
/// members of the closed set. This is the privileged entry point — it is not
/// reachable from `hew_actor_send`, which routes to the USER queue.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send_sys(
    mb: *mut HewMailbox,
    sys_msg: i32,
    data: *mut c_void,
    size: usize,
) {
    let Some(kind) = HewSysMsg::from_raw(sys_msg) else {
        set_last_error(format!(
            "hew_mailbox_send_sys: refusing system message with unknown kind {sys_msg}"
        ));
        eprintln!(
            "hew_mailbox_send_sys: refusing system message with unknown kind {sys_msg} \
             (the system queue carries only the closed HewSysMsg set)"
        );
        return;
    };
    // SAFETY: forwarded caller contract.
    let _ = unsafe { mailbox_send_sys_checked(mb, kind, data, size) };
}

/// Internal checked system send. Returns false when the message node cannot be
/// allocated; callers that claim one-shot delivery can then take an explicit
/// controlled-failure path instead of silently losing the message.
///
/// Takes the typed kind: no caller inside the runtime can put a value on the
/// system queue that is not a lifecycle signal.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
pub(crate) unsafe fn mailbox_send_sys_checked(
    mb: *mut HewMailbox,
    sys_msg: HewSysMsg,
    data: *mut c_void,
    size: usize,
) -> bool {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    // SAFETY: `data` validity guaranteed by caller.
    // System sends use the NON-minting node allocator: the crash-recovery trace
    // root must be minted only on the supervisor-dispatch side (via
    // `ensure_supervisor_trace_root`), never in or under the `hew_actor_trap`
    // signal-handler context that originates child-crash notifications.
    let node = unsafe { msg_node_alloc_sys(sys_msg.as_i32(), data, size, ptr::null_mut()) };
    if node.is_null() {
        set_last_error(format!(
            "hew_mailbox_send_sys: failed to deliver system message ({sys_msg:?}, size={size})"
        ));
        eprintln!(
            "hew_mailbox_send_sys: failed to deliver system message ({sys_msg:?}, size={size})"
        );
        return false;
    }

    // SAFETY: `node` is freshly allocated and owned by this mailbox send.
    unsafe { enqueue_sys_node(mb, node) };
    true
}

/// Typed system send that ignores allocation failure.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[cfg(test)]
pub(crate) unsafe fn mailbox_send_sys(
    mb: *mut HewMailbox,
    sys_msg: HewSysMsg,
    data: *mut c_void,
    size: usize,
) {
    // SAFETY: forwarded caller contract.
    let _ = unsafe { mailbox_send_sys_checked(mb, sys_msg, data, size) };
}

unsafe fn enqueue_sys_node(mb: &HewMailbox, node: *mut HewMsgNode) {
    // SAFETY: production publishes the count reservation before reachability.
    unsafe { enqueue_sys_node_inner(mb, node, true) };
}

unsafe fn enqueue_sys_node_inner(
    mb: &HewMailbox,
    node: *mut HewMsgNode,
    publish_count_first: bool,
) {
    // Publish the reservation before the node can become reachable. A running
    // consumer may dequeue immediately after the predecessor link is stored;
    // incrementing afterward lets its fetch_sub observe zero and wrap usize.
    let sys_queue_len = if publish_count_first {
        let len = mb.sys_count.fetch_add(1, Ordering::AcqRel) + 1;
        #[cfg(test)]
        run_sys_count_publication_hook(node);
        len
    } else {
        0
    };
    // SAFETY: `node` was just allocated with next == null.
    unsafe { mb.sys_queue.enqueue(node) };
    #[cfg(test)]
    let sys_queue_len = if publish_count_first {
        sys_queue_len
    } else {
        run_sys_count_publication_hook(node);
        mb.sys_count.fetch_add(1, Ordering::AcqRel).wrapping_add(1)
    };
    #[cfg(not(test))]
    debug_assert!(publish_count_first);
    if sys_queue_len > SYS_QUEUE_WARN_THRESHOLD {
        eprintln!("[mailbox] warning: system queue has {sys_queue_len} messages (mailbox {mb:p})");
    }
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
}

/// Latch a stop request on this mailbox, OUT OF BAND.
///
/// The stop signal is this atomic bool, not a queued node. `hew_actor_stop`
/// calls this for a Running actor and the per-message loop in
/// `scheduler::activate_actor` reads it at loop top via
/// [`mailbox_stop_requested`].
///
/// This function CANNOT FAIL. That is its whole point. Its predecessor,
/// `mailbox_send_stop_sys_once`, allocated a sentinel `HewMsgNode` *before*
/// latching the flag, so on allocation failure it returned `false` having
/// neither enqueued the node nor set the flag — and both callers discarded the
/// result. Under memory pressure a Running actor therefore never observed its
/// own stop and ran until something else tore it down. There is no allocation
/// on this path, so there is no such window.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer or null.
pub(crate) unsafe fn mailbox_request_stop(mb: *mut HewMailbox) {
    if mb.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `mb` is valid when non-null.
    let mb = unsafe { &*mb };
    mb.stop_requested.store(true, Ordering::Release);
}

/// Whether a stop has been requested on this mailbox.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer or null. A null mailbox is NOT a stop
/// request (fail-closed against a spurious self-stop).
pub(crate) unsafe fn mailbox_stop_requested(mb: *mut HewMailbox) -> bool {
    if mb.is_null() {
        return false;
    }
    // SAFETY: Caller guarantees `mb` is valid when non-null.
    unsafe { &*mb }.stop_requested.load(Ordering::Acquire)
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
        let blocked: Vec<_> = {
            let mut waiters = mb.blocked_senders.lock_or_recover();
            waiters.drain(..).collect()
        };
        for waiter in blocked {
            // SAFETY: draining transferred exclusive ownership of the unpublished node.
            unsafe { hew_msg_node_free_with_message_drop(waiter.node, mb.message_drop_fn) };
            // SAFETY: the drained waiter owns its retained slot ref; Error wakes
            // the sender so it can continue across the now-stopped recipient.
            unsafe { wake_blocked_sender(&waiter, ReadStatus::Error) };
        }
        // Blocking senders join `block_wait` while still owning the queue,
        // recheck `closed`, release the queue, and atomically release
        // `block_wait` in `Condvar::wait`. Joining that dedicated predicate
        // mutex here prevents a one-shot close notification from passing the
        // check-to-park seam without recursively acquiring `slow_path` from a
        // coalesce key/drop callback. Always use the protocol: a sender may
        // already be parked even if coalesce configuration changed afterward.
        mb.notify_not_full_all();
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
    // SAFETY: caller upholds the `mb`-valid + single-consumer contract.
    unsafe { mailbox_try_recv_with_origin(mb) }.node
}

/// A received node plus the typed provenance of the queue it came from.
///
/// The origin is the discriminator, and it is a TYPE: a lifecycle signal is
/// `Origin::Sys(kind)` because of the queue it arrived on, never because its
/// `msg_type` equals some reserved integer. `msg_type` is unrestricted `i32` in
/// the public C ABI ([`hew_actor_send`] / `HewDispatchFn`) and codegen message
/// tags are `SipHash` values, so a user message may legitimately carry any value
/// including a `HewSysMsg` discriminant — it is still `Origin::User` and still
/// goes to the user trampoline.
///
/// A system-queue node whose stored kind does not decode is refused
/// (`Origin::User` is NOT the fallback; see `mailbox_try_recv_with_origin`).
pub(crate) struct RecvNode {
    pub node: *mut HewMsgNode,
    pub origin: Origin,
}

/// Single-consumer receive that preserves system-vs-user provenance.
///
/// System messages keep priority (dequeued first), exactly as
/// [`hew_mailbox_try_recv`] — this is its provenance-carrying core.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer. Only one thread may call recv
/// functions at a time (single-consumer invariant).
pub(crate) unsafe fn mailbox_try_recv_with_origin(mb: *mut HewMailbox) -> RecvNode {
    // SAFETY: Caller guarantees `mb` is valid and single-consumer.
    let mb = unsafe { &*mb };

    // System messages have priority (lock-free dequeue).
    // SAFETY: single-consumer invariant satisfied by caller.
    let sys_node = unsafe { mb.sys_queue.try_dequeue() };
    if !sys_node.is_null() {
        mb.sys_count.fetch_sub(1, Ordering::AcqRel);
        MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
        // SAFETY: `sys_node` is the non-null node just dequeued and is owned here.
        let raw = unsafe { (*sys_node).msg_type };
        let Some(kind) = HewSysMsg::from_raw(raw) else {
            // Fail-closed: a system-queue node whose kind does not decode is
            // NOT downgraded to a user message (that would hand it to the
            // application trampoline). Every runtime producer goes through the
            // typed `mailbox_send_sys*`, and the C entry point validates, so
            // reaching here means memory corruption. Drop it and report.
            eprintln!(
                "[mailbox] refusing system-queue node with undecodable kind {raw} \
                 (mailbox {mb:p}); dropping"
            );
            // SAFETY: `sys_node` is exclusively owned here and not published.
            unsafe { hew_msg_node_free(sys_node) };
            return RecvNode {
                node: ptr::null_mut(),
                origin: Origin::User,
            };
        };
        return RecvNode {
            node: sys_node,
            origin: Origin::Sys(kind),
        };
    }

    // User messages: slow path uses mutex, fast path uses lock-free queue.
    if mb.use_slow_path {
        let mut q = mb.slow_path.lock_or_recover();
        if let Some(node) = q.user_queue.pop_front() {
            mb.count.fetch_sub(1, Ordering::Release);
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            let blocked_sender = {
                let mut waiters = mb.blocked_senders.lock_or_recover();
                waiters.pop_front()
            };
            if let Some(waiter) = &blocked_sender {
                enqueue_bounded_slow_path_node(mb, &mut q, waiter.node);
                update_high_water_mark(mb);
                MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
            }
            drop(q);
            if let Some(waiter) = blocked_sender {
                // SAFETY: the waiter was removed under its lock and its node is
                // now admitted; this edge owns the retained slot ref.
                unsafe { wake_blocked_sender(&waiter, ReadStatus::Data) };
            } else {
                // Preserve the foreign-thread blocking sender path.
                mb.notify_not_full_one();
            }
            return RecvNode {
                node,
                origin: Origin::User,
            };
        }
    } else {
        // SAFETY: single-consumer invariant satisfied by caller.
        let node = unsafe { mb.user_fast.try_dequeue() };
        if !node.is_null() {
            mb.count.fetch_sub(1, Ordering::Release);
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            return RecvNode {
                node,
                origin: Origin::User,
            };
        }
    }

    RecvNode {
        node: ptr::null_mut(),
        origin: Origin::User,
    }
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

/// Returns `1` if the USER queue has messages, `0` otherwise.
///
/// This is the mailbox-holder's emptiness query: it answers "is there work for
/// me" without revealing anything about the runtime-private system lane. It is
/// the `stable` half of the split described on [`hew_mailbox_has_messages`].
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_has_user_messages(mb: *mut HewMailbox) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    if mb.use_slow_path {
        let q = mb.slow_path.lock_or_recover();
        i32::from(!q.user_queue.is_empty())
    } else {
        i32::from(mb.count.load(Ordering::Acquire) > 0)
    }
}

/// Returns `1` if EITHER queue has messages, `0` otherwise.
///
/// System-lane aware, therefore runtime-internal: a caller that can distinguish
/// an empty mailbox from one holding only a queued `Exit`/`Down`/supervisor
/// signal has observed privileged state, which is ingress in the same sense
/// that minting a system node is. Only the scheduler — the system queue's sole
/// legitimate consumer — has a reason to ask this question, so this half stays
/// `internal` and user code gets [`hew_mailbox_has_user_messages`] instead.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_has_messages(mb: *mut HewMailbox) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    if unsafe { &*mb }.sys_count.load(Ordering::Acquire) > 0 {
        return 1;
    }

    // SAFETY: Caller guarantees `mb` is valid.
    unsafe { hew_mailbox_has_user_messages(mb) }
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

/// Undispatched system-lane signals discarded by mailbox teardown, process-wide.
///
/// Mailbox teardown is the one place that destroys system-lane state, and it is
/// reachable from user-declarable actor teardown (`hew_actor_free` →
/// `free_actor_resources_*` → [`hew_mailbox_free`]). What made that reachability
/// a defect was that the discard was SILENT: a capability holder could tear an
/// actor down and every still-queued `Exit`/`Down`/supervisor signal simply
/// vanished with no record anywhere. Counting and naming each one is what turns
/// it into an accounted teardown — see [`retire_pending_sys_lane`].
static SYS_LANE_SIGNALS_RETIRED: AtomicUsize = AtomicUsize::new(0);

/// Read the process-wide count of system-lane signals discarded by teardown.
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "teardown accounting readout is asserted on by the mailbox regressions"
    )
)]
pub(crate) fn sys_lane_signals_retired() -> usize {
    SYS_LANE_SIGNALS_RETIRED.load(Ordering::Acquire)
}

/// Retire the undispatched system queue, then release the queue's sentinel.
///
/// The scheduler is the only legitimate consumer of the system queue. Once a
/// mailbox is being destroyed there is no consumer left, so anything queued is
/// lost by construction — but it must not be lost QUIETLY. Every node is
/// decoded through the closed [`HewSysMsg`] namespace, counted in
/// [`SYS_LANE_SIGNALS_RETIRED`], and named on stderr before it is freed, so the
/// loss is observable to whoever is looking at the run rather than invisible.
///
/// A raw value the closed namespace refuses is reported as `unknown`: teardown
/// is not a place to start trusting an undecodable discriminant, and a silent
/// skip would re-open the exact hole this exists to close.
///
/// # Safety
///
/// The caller must own `mailbox` exclusively (teardown), so the single-consumer
/// contract on `sys_queue` is satisfied.
unsafe fn retire_pending_sys_lane(mailbox: &HewMailbox) -> usize {
    let mut retired = 0usize;
    loop {
        // SAFETY: teardown owns the mailbox exclusively, so this frame is the
        // sole consumer for the duration of the drain.
        let node = unsafe { mailbox.sys_queue.try_dequeue() };
        if node.is_null() {
            break;
        }
        // SAFETY: `try_dequeue` transferred exclusive ownership of `node`.
        let raw = unsafe { (*node).msg_type };
        let name = HewSysMsg::from_raw(raw).map_or("unknown", HewSysMsg::name);
        eprintln!(
            "[mailbox] warning: discarding undispatched system signal {name} ({raw}) \
             at mailbox teardown (mailbox {mailbox:p})"
        );
        retired += 1;
        // SAFETY: exclusive ownership was transferred by the dequeue.
        unsafe { hew_msg_node_free(node) };
    }
    if retired > 0 {
        SYS_LANE_SIGNALS_RETIRED.fetch_add(retired, Ordering::AcqRel);
    }
    // Release the queue's stable stub sentinel (the drain above left the queue
    // empty, so this frees the sentinel and nothing else).
    // SAFETY: exclusive teardown ownership, as above.
    unsafe { mailbox.sys_queue.drain_and_free(None) };
    retired
}

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

    // Defensive teardown for callers that skipped `mailbox_close`: no parked
    // producer may retain a slot or copied payload beyond mailbox ownership.
    let blocked: Vec<_> = {
        let mut waiters = mailbox.blocked_senders.lock_or_recover();
        waiters.drain(..).collect()
    };
    for waiter in blocked {
        // SAFETY: draining transferred exclusive ownership of the unpublished node.
        unsafe { hew_msg_node_free_with_message_drop(waiter.node, mailbox.message_drop_fn) };
        // SAFETY: the drained waiter owns its retained slot ref.
        unsafe { wake_blocked_sender(&waiter, ReadStatus::Error) };
    }

    // Drain slow-path user queue (if used).
    {
        let mut q = mailbox.slow_path.lock_or_recover();
        while let Some(node) = q.user_queue.pop_front() {
            // SAFETY: Each node was allocated by `msg_node_alloc`.
            unsafe { hew_msg_node_free_with_message_drop(node, mailbox.message_drop_fn) };
        }
    }

    // Drain lock-free user queue (stable stub + any remaining nodes).
    // SAFETY: No concurrent access — mailbox is exclusively owned.
    unsafe { mailbox.user_fast.drain_and_free(mailbox.message_drop_fn) };

    // Retire the system queue (stable stub + any remaining nodes): accounted
    // and named, never a silent discard.
    // SAFETY: No concurrent access — mailbox is exclusively owned.
    let _ = unsafe { retire_pending_sys_lane(&mailbox) };
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    #[test]
    fn cooperative_block_sender_is_admitted_and_signalled_after_dequeue() {
        // SAFETY: this test owns the mailbox, nodes, and read slot exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            assert!(!mb.is_null());
            assert_eq!(hew_mailbox_send(mb, 1, ptr::null_mut(), 0), 0);
            let slot = crate::read_slot::hew_read_slot_new();
            assert_eq!(
                mailbox_await_send(mb, 2, ptr::null_mut(), 0, ptr::null_mut(), slot),
                MAILBOX_AWAIT_SEND_SUSPEND
            );
            assert_eq!(crate::read_slot::hew_read_slot_status(slot), 0);

            let first = hew_mailbox_try_recv(mb);
            assert!(!first.is_null());
            assert_eq!((*first).msg_type, 1);
            hew_msg_node_free(first);
            assert_eq!(
                crate::read_slot::hew_read_slot_status(slot),
                ReadStatus::Data as i32
            );

            let second = hew_mailbox_try_recv(mb);
            assert!(!second.is_null());
            assert_eq!((*second).msg_type, 2);
            hew_msg_node_free(second);
            crate::read_slot::hew_read_slot_free(slot);
            mailbox_close(mb);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn cooperative_block_sender_is_released_and_signalled_on_close() {
        // SAFETY: this test owns the mailbox, nodes, and read slot exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            assert_eq!(hew_mailbox_send(mb, 1, ptr::null_mut(), 0), 0);
            let slot = crate::read_slot::hew_read_slot_new();
            assert_eq!(
                mailbox_await_send(mb, 2, ptr::null_mut(), 0, ptr::null_mut(), slot),
                MAILBOX_AWAIT_SEND_SUSPEND
            );
            mailbox_close(mb);
            assert_eq!(
                crate::read_slot::hew_read_slot_status(slot),
                ReadStatus::Error as i32
            );
            crate::read_slot::hew_read_slot_free(slot);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn cooperative_block_ask_is_admitted_without_a_capacity_wait_slot() {
        // SAFETY: this test owns the mailbox, nodes, and reply-channel refs.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            assert_eq!(hew_mailbox_send(mb, 1, ptr::null_mut(), 0), 0);
            let channel = crate::reply_channel::hew_reply_channel_new();
            assert!(!channel.is_null());
            // Mirror ask submission's sender-side reference transferred into
            // the pending node; the test keeps the creator ref until teardown.
            crate::reply_channel::hew_reply_channel_retain(channel);
            assert_eq!(
                mailbox_send_with_reply_cooperative(mb, 2, ptr::null_mut(), 0, channel.cast(),),
                HewError::Ok as i32
            );

            let first = hew_mailbox_try_recv(mb);
            assert!(!first.is_null());
            assert_eq!((*first).msg_type, 1);
            hew_msg_node_free(first);

            let second = hew_mailbox_try_recv(mb);
            assert!(!second.is_null());
            assert_eq!((*second).msg_type, 2);
            assert_eq!((*second).reply_channel, channel.cast());
            hew_msg_node_free(second);
            crate::reply_channel::hew_reply_channel_free(channel);
            mailbox_close(mb);
            hew_mailbox_free(mb);
        }
    }

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
    fn bounded_fast_path_reserves_capacity_atomically() {
        use std::sync::atomic::AtomicBool;
        use std::sync::{Arc, Barrier};
        use std::thread;

        const CAPACITY: i32 = 1;
        const SENDERS: usize = 16;
        const ROUNDS: usize = 128;

        // SAFETY: the mailbox remains live until every producer has joined, and
        // the main test thread is the only consumer.
        unsafe {
            let mb = hew_mailbox_new_bounded(CAPACITY);
            assert!(!mb.is_null());
            assert!(!(*mb).use_slow_path, "DropNew should use the fast path");

            let shared_mb = Arc::new(AtomicPtr::new(mb));
            let start = Arc::new(Barrier::new(SENDERS + 1));
            let finish = Arc::new(Barrier::new(SENDERS + 1));
            let unexpected_status = Arc::new(AtomicBool::new(false));
            let mut producers = Vec::with_capacity(SENDERS);

            for producer_id in 0..SENDERS {
                let shared_mb = Arc::clone(&shared_mb);
                let start = Arc::clone(&start);
                let finish = Arc::clone(&finish);
                let unexpected_status = Arc::clone(&unexpected_status);
                producers.push(thread::spawn(move || {
                    let payload = i32::try_from(producer_id).expect("producer id fits in i32");
                    for _ in 0..ROUNDS {
                        start.wait();
                        // SAFETY: the mailbox outlives this producer; the payload
                        // is readable until the send finishes copying it.
                        let rc = hew_mailbox_try_send(
                            shared_mb.load(Ordering::Relaxed),
                            0,
                            (&raw const payload).cast_mut().cast(),
                            size_of::<i32>(),
                        );
                        if rc != HewError::Ok as i32 && rc != HewError::ErrMailboxFull as i32 {
                            unexpected_status.store(true, Ordering::Relaxed);
                        }
                        finish.wait();
                    }
                }));
            }

            let mut max_len = 0;
            let mut count_mismatch = false;
            for _ in 0..ROUNDS {
                start.wait();
                finish.wait();

                let len = hew_mailbox_len(mb);
                max_len = max_len.max(len);

                let mut drained = 0;
                loop {
                    let node = hew_mailbox_try_recv(mb);
                    if node.is_null() {
                        break;
                    }
                    drained += 1;
                    hew_msg_node_free(node);
                }
                count_mismatch |= drained != len;
            }

            for producer in producers {
                producer.join().expect("producer thread panicked");
            }

            let high_water_mark = (*mb).high_water_mark.load(Ordering::Relaxed);
            hew_mailbox_free(mb);

            assert!(
                !unexpected_status.load(Ordering::Relaxed),
                "send returned an unexpected status"
            );
            assert!(!count_mismatch, "mailbox count diverged from queued nodes");
            assert!(
                max_len <= usize::try_from(CAPACITY).expect("positive capacity fits in usize"),
                "bounded fast-path mailbox reached length {max_len} with capacity {CAPACITY}"
            );
            assert!(
                high_water_mark <= i64::from(CAPACITY),
                "bounded fast-path mailbox high-water mark {high_water_mark} exceeded capacity {CAPACITY}"
            );
        }
    }

    /// Every bounded slow-path policy must decide admission from the protected
    /// `VecDeque` length, not the independently published `count`.  Start all
    /// producers together with no consumer: capacity one makes every eviction,
    /// rejection, payload drop, and high-water result exact rather than a
    /// timing-sensitive approximation.
    #[test]
    fn bounded_slow_path_copy_admission_never_overshoots_capacity() {
        use std::sync::{Arc, Barrier};
        use std::thread;

        const SENDERS: usize = 16;

        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        for (policy, is_coalesce) in [
            (HewOverflowPolicy::Block, false),
            (HewOverflowPolicy::DropOld, false),
            (HewOverflowPolicy::Coalesce, true),
        ] {
            MESSAGE_DROP_COUNT.store(0, Ordering::SeqCst);
            // SAFETY: this test retains the mailbox until all producers join
            // and is its only consumer.
            unsafe {
                let mb = if is_coalesce {
                    let mb = hew_mailbox_new_coalesce(1);
                    hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::DropOld);
                    mb
                } else {
                    hew_mailbox_new_with_policy(1, policy)
                };
                hew_mailbox_set_message_drop_fn(mb, Some(message_test_drop_glue));

                let start = Arc::new(Barrier::new(SENDERS + 1));
                let successes = Arc::new(AtomicUsize::new(0));
                let rejects = Arc::new(AtomicUsize::new(0));
                let mut producers = Vec::with_capacity(SENDERS);
                for producer in 0..SENDERS {
                    let start = Arc::clone(&start);
                    let successes = Arc::clone(&successes);
                    let rejects = Arc::clone(&rejects);
                    let mb_addr = mb.addr();
                    producers.push(thread::spawn(move || {
                        let value = i32::try_from(producer).expect("producer fits i32");
                        start.wait();
                        // SAFETY: the owner joins every producer before free.
                        let rc = hew_mailbox_try_send(
                            ptr::without_provenance_mut(mb_addr),
                            i32::try_from(producer).expect("producer fits i32"),
                            (&raw const value).cast_mut().cast(),
                            size_of::<i32>(),
                        );
                        if rc == HewError::Ok as i32 {
                            successes.fetch_add(1, Ordering::SeqCst);
                        } else {
                            assert_eq!(rc, HewError::ErrMailboxFull as i32);
                            rejects.fetch_add(1, Ordering::SeqCst);
                        }
                    }));
                }

                start.wait();
                for producer in producers {
                    producer.join().expect("producer panicked");
                }

                assert_eq!(hew_mailbox_len(mb), 1, "{policy:?} queue length");
                assert_eq!(
                    (*mb).high_water_mark.load(Ordering::Acquire),
                    1,
                    "{policy:?} high-water mark"
                );
                match policy {
                    HewOverflowPolicy::Block => {
                        assert_eq!(successes.load(Ordering::SeqCst), 1);
                        assert_eq!(rejects.load(Ordering::SeqCst), SENDERS - 1);
                        assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 0);
                    }
                    HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce => {
                        assert_eq!(successes.load(Ordering::SeqCst), SENDERS);
                        assert_eq!(rejects.load(Ordering::SeqCst), 0);
                        assert_eq!(
                            MESSAGE_DROP_COUNT.load(Ordering::SeqCst),
                            SENDERS - 1,
                            "{policy:?} must release each evicted copy payload once"
                        );
                    }
                    HewOverflowPolicy::DropNew | HewOverflowPolicy::Fail => unreachable!(),
                }

                let node = hew_mailbox_try_recv(mb);
                assert!(!node.is_null());
                hew_msg_node_free_with_message_drop(node, (*mb).message_drop_fn);
                assert_eq!(
                    MESSAGE_DROP_COUNT.load(Ordering::SeqCst),
                    if matches!(policy, HewOverflowPolicy::Block) {
                        1
                    } else {
                        SENDERS
                    },
                    "{policy:?} must release the surviving payload once"
                );
                hew_mailbox_free(mb);
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn bounded_slow_path_alias_admission_never_overshoots_capacity() {
        use std::sync::{Arc, Barrier};
        use std::thread;

        const SENDERS: usize = 16;

        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        for (policy, is_coalesce) in [
            (HewOverflowPolicy::Block, false),
            (HewOverflowPolicy::DropOld, false),
            (HewOverflowPolicy::Coalesce, true),
        ] {
            ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
            // SAFETY: mailbox and every allocated envelope remain live until
            // the joined producers and final drain complete.
            unsafe {
                let mb = if is_coalesce {
                    let mb = hew_mailbox_new_coalesce(1);
                    hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::DropOld);
                    mb
                } else {
                    hew_mailbox_new_with_policy(1, policy)
                };
                let start = Arc::new(Barrier::new(SENDERS + 1));
                let successes = Arc::new(AtomicUsize::new(0));
                let rejects = Arc::new(AtomicUsize::new(0));
                let mut producers = Vec::with_capacity(SENDERS);
                for producer in 0..SENDERS {
                    let start = Arc::clone(&start);
                    let successes = Arc::clone(&successes);
                    let rejects = Arc::clone(&rejects);
                    let mb_addr = mb.addr();
                    producers.push(thread::spawn(move || {
                        let payload = alloc_test_payload(&[u8::try_from(producer).unwrap_or(0)]);
                        // SAFETY: this producer transfers its one refcount to
                        // the send state machine exactly once.
                        let env = hew_msg_envelope_new(payload, 1, Some(envelope_test_drop_glue));
                        assert!(!env.is_null());
                        start.wait();
                        // SAFETY: mailbox outlives the joined producer.
                        let outcome = send_aliased_with_overflow(
                            &*ptr::without_provenance_mut::<HewMailbox>(mb_addr),
                            i32::try_from(producer).expect("producer fits i32"),
                            env,
                            true,
                        );
                        if matches!(outcome, SendOutcome::Enqueued | SendOutcome::DroppedOld) {
                            successes.fetch_add(1, Ordering::SeqCst);
                        } else {
                            assert!(matches!(outcome, SendOutcome::Failed));
                            rejects.fetch_add(1, Ordering::SeqCst);
                        }
                    }));
                }

                start.wait();
                for producer in producers {
                    producer.join().expect("producer panicked");
                }

                assert_eq!(hew_mailbox_len(mb), 1, "{policy:?} queue length");
                assert_eq!(
                    (*mb).high_water_mark.load(Ordering::Acquire),
                    1,
                    "{policy:?} high-water mark"
                );
                match policy {
                    HewOverflowPolicy::Block => {
                        assert_eq!(successes.load(Ordering::SeqCst), 1);
                        assert_eq!(rejects.load(Ordering::SeqCst), SENDERS - 1);
                    }
                    HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce => {
                        assert_eq!(successes.load(Ordering::SeqCst), SENDERS);
                        assert_eq!(rejects.load(Ordering::SeqCst), 0);
                    }
                    HewOverflowPolicy::DropNew | HewOverflowPolicy::Fail => unreachable!(),
                }
                assert_eq!(
                    ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                    SENDERS - 1,
                    "{policy:?} must release every rejected or evicted alias once"
                );

                let node = hew_mailbox_try_recv(mb);
                assert!(!node.is_null());
                hew_msg_node_free(node);
                assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), SENDERS);
                hew_mailbox_free(mb);
            }
        }
    }

    #[test]
    fn bounded_fast_path_releases_capacity_reservation_on_oom() {
        // SAFETY: test owns the mailbox exclusively; null data with zero size is valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            assert!(!mb.is_null());

            let fail_guard = fail_mailbox_alloc_on_nth(0);
            assert_eq!(
                hew_mailbox_try_send(mb, 0, ptr::null_mut(), 0),
                HewError::ErrOom as i32
            );
            drop(fail_guard);

            assert_eq!(
                hew_mailbox_len(mb),
                0,
                "allocation failure must release the reserved capacity slot"
            );
            assert_eq!(
                hew_mailbox_try_send(mb, 0, ptr::null_mut(), 0),
                HewError::Ok as i32,
                "the rolled-back slot must be available to the next sender"
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
    fn coalesce_requires_matching_message_type() {
        // SAFETY: test owns the mailbox and every drained node.
        unsafe {
            let mb = hew_mailbox_new_coalesce(2);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropNew);
            let beta = PriceUpdate {
                symbol: 42,
                price: 20,
            };
            let alpha_old = PriceUpdate {
                symbol: 42,
                price: 1,
            };
            let alpha_new = PriceUpdate {
                symbol: 42,
                price: 2,
            };
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    8,
                    (&raw const beta).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    7,
                    (&raw const alpha_old).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_try_send(
                    mb,
                    7,
                    (&raw const alpha_new).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );

            let first = hew_mailbox_try_recv(mb);
            let second = hew_mailbox_try_recv(mb);
            assert_eq!((*first).msg_type, 8);
            assert_eq!((*second).msg_type, 7);
            assert_eq!((*(*second).data.cast::<PriceUpdate>()).price, 2);
            hew_msg_node_free(first);
            hew_msg_node_free(second);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_block_drop_callback_may_close_same_mailbox() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        // SAFETY: the mailbox stays allocated until the worker completes and
        // the process-wide callback target is cleared.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::Block);
            hew_mailbox_set_message_drop_fn(mb, Some(reentrant_close_message_drop_glue));
            REENTRANT_CLOSE_MAILBOX.store(mb.addr(), Ordering::Release);

            let first = PriceUpdate {
                symbol: 7,
                price: 10,
            };
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    1,
                    (&raw const first).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );

            let mb_addr = mb.addr();
            let (done_tx, done_rx) = std::sync::mpsc::channel();
            let worker = std::thread::spawn(move || {
                let replacement = PriceUpdate {
                    symbol: 7,
                    price: 20,
                };
                // A matching key replaces the queued payload and invokes its
                // typed destructor while coalescing owns `slow_path`.
                let result = hew_mailbox_send(
                    ptr::without_provenance_mut(mb_addr),
                    1,
                    (&raw const replacement).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                );
                let _ = done_tx.send(result);
            });

            let result = done_rx
                .recv_timeout(std::time::Duration::from_secs(5))
                .expect("re-entrant close from coalesce drop callback deadlocked");
            assert_eq!(result, HewError::Ok as i32);
            worker.join().expect("coalescing sender panicked");
            assert!((*mb).closed.load(Ordering::Acquire));

            REENTRANT_CLOSE_MAILBOX.store(0, Ordering::Release);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_block_key_callback_may_close_same_mailbox() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        // SAFETY: the mailbox stays allocated until the worker completes and
        // the process-wide callback target is cleared.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(
                mb,
                Some(reentrant_close_coalesce_key),
                HewOverflowPolicy::Block,
            );
            REENTRANT_CLOSE_MAILBOX.store(mb.addr(), Ordering::Release);

            let first: i32 = 10;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const first).cast_mut().cast(),
                    size_of::<i32>(),
                ),
                HewError::Ok as i32
            );

            let mb_addr = mb.addr();
            let (done_tx, done_rx) = std::sync::mpsc::channel();
            let worker = std::thread::spawn(move || {
                let replacement: i32 = 20;
                let result = hew_mailbox_send(
                    ptr::without_provenance_mut(mb_addr),
                    7,
                    (&raw const replacement).cast_mut().cast(),
                    size_of::<i32>(),
                );
                let _ = done_tx.send(result);
            });

            let result = done_rx
                .recv_timeout(std::time::Duration::from_secs(5))
                .expect("re-entrant close from coalesce key callback deadlocked");
            assert_eq!(result, HewError::Ok as i32);
            worker.join().expect("coalescing sender panicked");
            assert!((*mb).closed.load(Ordering::Acquire));

            REENTRANT_CLOSE_MAILBOX.store(0, Ordering::Release);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn replace_releases_envelope_exactly_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the envelope, node, and replacement payload.
        unsafe {
            let payload = alloc_test_payload(b"old");
            let envelope = hew_msg_envelope_new(payload, 3, Some(envelope_test_drop_glue));
            assert!(!envelope.is_null());
            let node = msg_node_alloc_aliased(7, envelope, ptr::null_mut());
            assert!(!node.is_null());
            let replacement: i32 = 42;
            assert!(replace_node_payload(
                node,
                7,
                (&raw const replacement).cast(),
                size_of::<i32>(),
                ptr::null_mut(),
                None,
            ));
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
            hew_msg_node_free(node);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn coalesce_key_reads_envelope_backed_payload() {
        // SAFETY: test owns the envelope-backed node and frees it after the key lookup.
        unsafe {
            let update = PriceUpdate {
                symbol: 42,
                price: 99,
            };
            let payload = libc::malloc(size_of::<PriceUpdate>());
            assert!(!payload.is_null());
            ptr::write(payload.cast::<PriceUpdate>(), update);
            let envelope = hew_msg_envelope_new(payload, size_of::<PriceUpdate>(), None);
            let node = msg_node_alloc_aliased(7, envelope, ptr::null_mut());
            assert!(!node.is_null());
            assert!((*node).data.is_null(), "envelope nodes have null data");
            assert_eq!((*node).data_size, 0, "envelope nodes have no data size");

            assert_eq!(
                coalesce_message_key(
                    Some(price_symbol_key),
                    (*node).msg_type,
                    (*node).data,
                    (*node).data_size,
                    (*node).envelope,
                ),
                update.symbol,
            );

            hew_msg_node_free(node);
        }
    }

    #[test]
    fn replace_runs_legacy_message_drop_exactly_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        MESSAGE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the legacy node and replacement payload.
        unsafe {
            let old: i32 = 1;
            let node = msg_node_alloc(
                7,
                (&raw const old).cast(),
                size_of::<i32>(),
                ptr::null_mut(),
            );
            let replacement: i32 = 2;
            assert!(replace_node_payload(
                node,
                7,
                (&raw const replacement).cast(),
                size_of::<i32>(),
                ptr::null_mut(),
                Some(message_test_drop_glue),
            ));
            assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 1);
            hew_msg_node_free(node);
            assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn message_drop_consumes_drop_new_payload_exactly_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        MESSAGE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox and both stack payloads for each
        // synchronous send call.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            assert!(!mb.is_null());
            hew_mailbox_set_message_drop_fn(mb, Some(message_test_drop_glue));
            let first: i32 = 1;
            let second: i32 = 2;
            assert!(matches!(
                send_with_overflow(
                    &*mb,
                    7,
                    (&raw const first).cast(),
                    size_of::<i32>(),
                    false,
                    false,
                    ptr::null_mut(),
                ),
                SendOutcome::Enqueued
            ));
            assert!(matches!(
                send_with_overflow(
                    &*mb,
                    7,
                    (&raw const second).cast(),
                    size_of::<i32>(),
                    false,
                    false,
                    ptr::null_mut(),
                ),
                SendOutcome::Dropped
            ));
            assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 1);
            hew_mailbox_free(mb);
            assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 2);
        }
    }

    #[test]
    fn coalesce_drop_old_fallback_runs_legacy_drop_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        MESSAGE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox and drained node.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);
            hew_mailbox_set_message_drop_fn(mb, Some(message_test_drop_glue));
            let old = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            let incoming = PriceUpdate {
                symbol: 2,
                price: 20,
            };
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const old).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const incoming).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
            assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 1);
            let node = hew_mailbox_try_recv(mb);
            hew_msg_node_free(node);
            hew_mailbox_free(mb);
            assert_eq!(MESSAGE_DROP_COUNT.load(Ordering::SeqCst), 1);
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
                    1,
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
                1,
                "coalesced node should retain the matched message type"
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
    fn guaranteed_send_admits_terminal_event_past_full_capacity() {
        // The dropped-close bug shape at the mailbox layer: a bounded mailbox at
        // capacity (Fail policy) rejects `try_send` — the path that lost the
        // active-mode `on_close`. `hew_mailbox_send_guaranteed` must admit the
        // terminal event anyway, appended AFTER the buffered message (FIFO), so
        // the actor still observes it.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            const DATA_TYPE: i32 = 7;
            const CLOSE_TYPE: i32 = 9;
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Fail);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            // Fill the single slot with a buffered "on_data".
            assert_eq!(
                hew_mailbox_try_send(mb, DATA_TYPE, p, size_of::<i32>()),
                HewError::Ok as i32
            );

            // Reproduce the bug: a plain try_send for the terminal event is
            // rejected on the full mailbox — this is the silent drop.
            assert_eq!(
                hew_mailbox_try_send(mb, CLOSE_TYPE, ptr::null_mut(), 0),
                HewError::ErrMailboxFull as i32,
                "try_send must drop the terminal event on a full mailbox (the bug)"
            );

            // The fix: guaranteed-send admits it past capacity.
            assert_eq!(
                hew_mailbox_send_guaranteed(mb, CLOSE_TYPE, ptr::null_mut(), 0),
                HewError::Ok as i32,
                "guaranteed-send must admit the terminal event past a full mailbox"
            );

            // FIFO: the buffered on_data drains FIRST, the terminal event SECOND.
            let first = hew_mailbox_try_recv(mb);
            assert!(!first.is_null());
            assert_eq!(
                (*first).msg_type,
                DATA_TYPE,
                "buffered on_data must drain before the terminal close (FIFO)"
            );
            hew_msg_node_free(first);

            let second = hew_mailbox_try_recv(mb);
            assert!(!second.is_null());
            assert_eq!(
                (*second).msg_type,
                CLOSE_TYPE,
                "the guaranteed terminal close must drain after the buffered data"
            );
            hew_msg_node_free(second);

            assert!(
                hew_mailbox_try_recv(mb).is_null(),
                "exactly one buffered + one terminal node; no extras"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn guaranteed_send_refused_on_closed_mailbox() {
        // A closed mailbox means the actor is already terminating; the terminal
        // event is moot, so guaranteed-send reports ErrClosed (no enqueue, no
        // leak) rather than admitting a node into a dead mailbox.
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Fail);
            mailbox_close(mb);
            assert_eq!(
                hew_mailbox_send_guaranteed(mb, 9, ptr::null_mut(), 0),
                HewError::ErrClosed as i32,
                "guaranteed-send must refuse a closed mailbox"
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

    /// A close published after the blocking copy-send path checks `closed`,
    /// but before it parks, must still wake that sender with
    /// `ErrActorStopped`. The rendezvous forces the exact lost-notification
    /// window; no timing delay or follow-up notification participates.
    #[test]
    fn block_close_in_check_to_park_gap_wakes_copy_sender() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        // SAFETY: the mailbox remains live until both spawned threads join.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>(),
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb.addr();
            let sender = std::thread::spawn(move || {
                let value: i32 = 2;
                // SAFETY: main keeps the mailbox live through this join and
                // `value` remains readable for the duration of the call.
                hew_mailbox_send(
                    ptr::without_provenance_mut(mb_addr),
                    1,
                    (&raw const value).cast_mut().cast(),
                    size_of::<i32>(),
                )
            });

            // Sender owns slow_path + block_wait and has rechecked closed=false.
            entered.wait();
            let closer = std::thread::spawn(move || {
                // SAFETY: main keeps the mailbox live through this join.
                mailbox_close(ptr::without_provenance_mut(mb_addr));
            });

            // Close publishes before it joins block_wait. Once observed here,
            // releasing the sender forces it to park before close can notify.
            let close_deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while !(*mb).closed.load(Ordering::Acquire) {
                assert!(
                    std::time::Instant::now() < close_deadline,
                    "closer did not publish the closed predicate"
                );
                std::thread::yield_now();
            }
            release.wait();

            closer.join().expect("closer thread panicked");
            assert_eq!(
                sender.join().expect("sender thread panicked"),
                HewError::ErrActorStopped as i32,
                "the one close notification must wake the pre-wait sender"
            );
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_block_close_in_check_to_park_gap_wakes_copy_sender() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        // SAFETY: the mailbox remains live until both spawned threads join.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::Block);
            let filler = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb.addr();
            let sender = std::thread::spawn(move || {
                let value = PriceUpdate {
                    symbol: 2,
                    price: 20,
                };
                // Different key: the full Coalesce mailbox must take its
                // blocking fallback and reach the forced pre-wait seam.
                hew_mailbox_send(
                    ptr::without_provenance_mut(mb_addr),
                    7,
                    (&raw const value).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                )
            });

            entered.wait();
            let closer = std::thread::spawn(move || {
                mailbox_close(ptr::without_provenance_mut(mb_addr));
            });
            let close_deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while !(*mb).closed.load(Ordering::Acquire) {
                assert!(
                    std::time::Instant::now() < close_deadline,
                    "closer did not publish the closed predicate"
                );
                std::thread::yield_now();
            }
            release.wait();

            closer.join().expect("closer thread panicked");
            assert_eq!(
                sender.join().expect("sender thread panicked"),
                HewError::ErrActorStopped as i32,
                "Coalesce→Block copy sender must wake on close"
            );
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_block_drain_in_check_to_park_gap_wakes_copy_sender() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        // SAFETY: the mailbox remains live until the worker joins.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::Block);
            let filler = PriceUpdate {
                symbol: 1,
                price: 10,
            };
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    7,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb.addr();
            let sender = std::thread::spawn(move || {
                let value = PriceUpdate {
                    symbol: 2,
                    price: 20,
                };
                hew_mailbox_send(
                    ptr::without_provenance_mut(mb_addr),
                    7,
                    (&raw const value).cast_mut().cast(),
                    size_of::<PriceUpdate>(),
                )
            });

            entered.wait();
            release.wait();
            let first = hew_mailbox_try_recv(mb);
            assert!(!first.is_null());
            hew_msg_node_free(first);

            assert_eq!(
                sender.join().expect("sender panicked"),
                HewError::Ok as i32,
                "capacity notification must wake Coalesce→Block sender"
            );
            let second = hew_mailbox_try_recv(mb);
            assert!(!second.is_null());
            assert_eq!((*(*second).data.cast::<PriceUpdate>()).symbol, 2);
            hew_msg_node_free(second);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn block_one_capacity_wake_then_close_wakes_remaining_sender() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        // SAFETY: the mailbox remains live until both senders join.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>(),
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb.addr();
            let (done_tx, done_rx) = std::sync::mpsc::channel();
            let mut workers = Vec::new();
            for value in [2_i32, 3_i32] {
                let done_tx = done_tx.clone();
                workers.push(std::thread::spawn(move || {
                    let result = hew_mailbox_send(
                        ptr::without_provenance_mut(mb_addr),
                        value,
                        (&raw const value).cast_mut().cast(),
                        size_of::<i32>(),
                    );
                    done_tx.send(result).expect("result receiver remains live");
                }));
            }
            drop(done_tx);

            entered.wait();
            release.wait();
            let first = hew_mailbox_try_recv(mb);
            assert!(!first.is_null());
            hew_msg_node_free(first);

            assert_eq!(
                done_rx
                    .recv_timeout(std::time::Duration::from_secs(5))
                    .expect("one sender must consume the capacity wake"),
                HewError::Ok as i32
            );
            mailbox_close(mb);
            assert_eq!(
                done_rx
                    .recv_timeout(std::time::Duration::from_secs(5))
                    .expect("close must wake the remaining sender"),
                HewError::ErrActorStopped as i32
            );
            for worker in workers {
                worker.join().expect("sender panicked");
            }
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

    /// The has-messages SPLIT: a queued system message is INVISIBLE to the
    /// user-lane query and visible to the system-aware one. This is the
    /// behavioural half of the classification move — `hew_mailbox_has_messages`
    /// is `internal` precisely because it answers `1` here, which lets a
    /// mailbox holder detect a pending supervisor/`Exit`/`Down` signal in an
    /// otherwise-empty mailbox.
    ///
    /// Counterfactual: implement `hew_mailbox_has_user_messages` by delegating
    /// to `hew_mailbox_has_messages` (the pre-split behaviour) and the first
    /// assertion fails.
    #[test]
    fn user_lane_query_does_not_observe_a_queued_system_message() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let s: i32 = 99;

            hew_mailbox_send_sys(mb, 2, (&raw const s).cast_mut().cast(), size_of::<i32>());

            assert_eq!(
                hew_mailbox_has_user_messages(mb),
                0,
                "the user-lane query must not reveal a queued system message"
            );
            assert_eq!(
                hew_mailbox_has_messages(mb),
                1,
                "the system-aware scheduler query still sees the system lane"
            );

            // A user message is visible to both.
            let u: i32 = 10;
            hew_mailbox_send(mb, 1, (&raw const u).cast_mut().cast(), size_of::<i32>());
            assert_eq!(hew_mailbox_has_user_messages(mb), 1);
            assert_eq!(hew_mailbox_has_messages(mb), 1);

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

    /// The system count reservation must be visible before a node becomes
    /// reachable. The counterfactual links first, lets the consumer dequeue
    /// from zero, and deterministically observes `usize::MAX`; production
    /// exposes the reservation first and never underflows.
    #[test]
    #[expect(
        clippy::undocumented_unsafe_blocks,
        reason = "the deterministic two-ordering fixture keeps raw mailbox/node operations compact inside its unsafe case helper"
    )]
    fn sys_count_is_published_before_consumer_reachability() {
        unsafe fn run_case(publish_count_first: bool) {
            let mb = unsafe { hew_mailbox_new() };
            assert!(!mb.is_null());
            let node = unsafe {
                msg_node_alloc_sys(
                    HewSysMsg::Exit.as_i32(),
                    ptr::null_mut(),
                    0,
                    ptr::null_mut(),
                )
            };
            assert!(!node.is_null());

            let (hook, entered, release) = SysCountPublicationHookGuard::install(HewSysMsg::Exit);
            let mb_addr = mb.addr();
            let node_addr = node.addr();
            let producer = std::thread::spawn(move || {
                let mb = ptr::with_exposed_provenance_mut::<HewMailbox>(mb_addr);
                let node = ptr::with_exposed_provenance_mut::<HewMsgNode>(node_addr);
                // SAFETY: mailbox and node outlive this joined producer.
                unsafe { enqueue_sys_node_inner(&*mb, node, publish_count_first) };
            });
            entered.wait();

            if publish_count_first {
                assert_eq!(
                    unsafe { (*mb).sys_count.load(Ordering::Acquire) },
                    1,
                    "reservation is visible before queue publication"
                );
                assert!(
                    unsafe { hew_mailbox_try_recv_sys(mb) }.is_null(),
                    "count publication alone does not fabricate a reachable node"
                );
                assert_eq!(
                    unsafe { (*mb).sys_count.load(Ordering::Acquire) },
                    1,
                    "an empty dequeue cannot consume the reservation"
                );
                release.wait();
                producer.join().expect("system producer");
                let received = unsafe { hew_mailbox_try_recv_sys(mb) };
                assert_eq!(received, node);
                assert_eq!(unsafe { (*mb).sys_count.load(Ordering::Acquire) }, 0);
                unsafe { hew_msg_node_free(received) };
            } else {
                assert_eq!(
                    unsafe { (*mb).sys_count.load(Ordering::Acquire) },
                    0,
                    "counterfactual pauses after link but before count"
                );
                let received = unsafe { hew_mailbox_try_recv_sys(mb) };
                assert_eq!(received, node, "consumer wins before omitted publication");
                assert_eq!(
                    unsafe { (*mb).sys_count.load(Ordering::Acquire) },
                    usize::MAX,
                    "dequeue-before-increment wraps the unsigned system count"
                );
                release.wait();
                producer.join().expect("counterfactual producer");
                assert_eq!(
                    unsafe { (*mb).sys_count.load(Ordering::Acquire) },
                    0,
                    "late increment merely wraps the corrupted count back"
                );
                unsafe { hew_msg_node_free(received) };
            }

            drop(hook);
            unsafe { hew_mailbox_free(mb) };
        }

        // Red-first omission, then production.
        unsafe {
            run_case(false);
            run_case(true);
        }
    }

    /// The user count reservation follows the same publication rule as the
    /// system queue: a reachable node must never race ahead of its count.
    /// Linking first lets a consumer deterministically decrement zero to
    /// `-1`; production reserves first and preserves the count.
    #[test]
    #[expect(
        clippy::undocumented_unsafe_blocks,
        reason = "the deterministic two-ordering fixture keeps raw mailbox/node operations compact inside its unsafe case helper"
    )]
    fn user_count_is_published_before_consumer_reachability() {
        unsafe fn run_case(publish_count_first: bool, slow_path: bool) {
            let mb = if slow_path {
                unsafe { hew_mailbox_new_with_policy(1, OverflowPolicy::DropOld) }
            } else {
                unsafe { hew_mailbox_new() }
            };
            assert!(!mb.is_null());
            let node = unsafe { msg_node_alloc(17, ptr::null_mut(), 0, ptr::null_mut()) };
            assert!(!node.is_null());

            let (hook, entered, release) = UserCountPublicationHookGuard::install(node);
            let mb_addr = mb.addr();
            let node_addr = node.addr();
            let producer = std::thread::spawn(move || {
                let mb = ptr::with_exposed_provenance_mut::<HewMailbox>(mb_addr);
                let node = ptr::with_exposed_provenance_mut::<HewMsgNode>(node_addr);
                // SAFETY: mailbox and node outlive this joined producer.
                unsafe { enqueue_user_node_inner(&*mb, node, publish_count_first) };
            });
            entered.wait();

            if publish_count_first {
                assert_eq!(
                    unsafe { (*mb).count.load(Ordering::Acquire) },
                    1,
                    "reservation is visible before queue publication"
                );
                assert!(
                    unsafe { hew_mailbox_try_recv(mb) }.is_null(),
                    "count publication alone does not fabricate a reachable node"
                );
                assert_eq!(
                    unsafe { (*mb).count.load(Ordering::Acquire) },
                    1,
                    "an empty dequeue cannot consume the reservation"
                );
                release.wait();
                producer.join().expect("user producer");
                let received = unsafe { hew_mailbox_try_recv(mb) };
                assert_eq!(received, node);
                assert_eq!(unsafe { (*mb).count.load(Ordering::Acquire) }, 0);
                drop(hook);
                unsafe { hew_msg_node_free(received) };
            } else {
                assert_eq!(
                    unsafe { (*mb).count.load(Ordering::Acquire) },
                    0,
                    "counterfactual pauses after link but before count"
                );
                let received = unsafe { hew_mailbox_try_recv(mb) };
                assert_eq!(received, node, "consumer wins before omitted publication");
                assert_eq!(
                    unsafe { (*mb).count.load(Ordering::Acquire) },
                    -1,
                    "dequeue-before-increment makes the user count negative"
                );
                release.wait();
                producer.join().expect("counterfactual producer");
                assert_eq!(
                    unsafe { (*mb).count.load(Ordering::Acquire) },
                    0,
                    "late increment merely hides the transiently corrupted count"
                );
                drop(hook);
                unsafe { hew_msg_node_free(received) };
            }

            unsafe { hew_mailbox_free(mb) };
        }

        // Red-first omission, then production, on both queue implementations.
        unsafe {
            for slow_path in [false, true] {
                run_case(false, slow_path);
                run_case(true, slow_path);
            }
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
            // A real lifecycle signal should still succeed. It must be a
            // member of the closed `HewSysMsg` set: this entry point refuses
            // anything else, so an arbitrary integer here would make the
            // assertion pass on the queued USER message alone.
            hew_mailbox_send_sys(mb, HewSysMsg::ChildStopped.as_i32(), p, size_of::<i32>());
            assert_eq!(hew_mailbox_sys_len(mb), 1);
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
                hew_mailbox_try_push(mb, 100, (&raw const c).cast(), size_of::<PriceUpdate>()),
                3
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 100);
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
    /// Actor teardown is user-declarable (`hew_actor_free` is `stable`) and it
    /// reaches system-lane DESTRUCTION through `hew_mailbox_free`. What made
    /// that a defect was the silence: an undispatched `Exit`/`Down`/supervisor
    /// signal vanished with no record. Teardown now counts and names every
    /// signal it discards, which is the property that makes the reachability an
    /// accounted destruction instead of a covert one.
    ///
    /// Counterfactual: restore the old body — `mailbox.sys_queue.drain_and_free(None)`
    /// in place of `retire_pending_sys_lane` — and the delta below is 0, so the
    /// assertion trips. The counter is process-wide and other tests tear down
    /// mailboxes concurrently, so the assertion is a lower bound on the delta;
    /// the counterfactual moves it to exactly zero, which is what makes the
    /// bound non-vacuous.
    #[test]
    fn mailbox_teardown_accounts_for_the_system_signals_it_discards() {
        let before = sys_lane_signals_retired();
        // SAFETY: the test owns the mailbox exclusively for its whole lifetime.
        unsafe {
            let mb = hew_mailbox_new();
            assert!(mailbox_send_sys_checked(
                mb,
                HewSysMsg::Exit,
                ptr::null_mut(),
                0
            ));
            assert_eq!(hew_mailbox_sys_len(mb), 1);
            hew_mailbox_free(mb);
        }
        assert!(
            sys_lane_signals_retired() > before,
            "mailbox teardown must account for the undispatched system signal it discarded"
        );
    }

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
            unsafe { q.drain_and_free(None) };
        }
    }

    /// The MPSC stable stub is disambiguated from a real message by POINTER
    /// IDENTITY, never by its `msg_type`. `MPSC_STUB_MSG_TYPE` is an arbitrary
    /// stamp with no reserved status: the whole `i32` space is live and
    /// user-reachable, so a real message may legitimately carry exactly that
    /// value. This pins the property that actually holds — a message stamped
    /// identically to the stub still round-trips as an ordinary node and is
    /// never mistaken for the stub, and the stub is never handed out.
    #[test]
    fn stub_stamp_value_is_not_reserved_against_real_messages() {
        let q = MpscQueue::new().expect("queue allocation should succeed");
        let stub = q.stub_ptr();
        // SAFETY: the stub stays live for the queue's lifetime and this thread
        // exclusively owns `q`.
        let stamp = unsafe { (*stub).msg_type };

        for _round in 0..3 {
            // A real message carrying the stub's own stamp: legal on the public
            // ABI, and the queue must treat it as any other message.
            // SAFETY: null payload + zero size is a valid message.
            let node = unsafe { msg_node_alloc(stamp, ptr::null(), 0, ptr::null_mut()) };
            assert!(!node.is_null(), "message allocation must succeed");
            assert_ne!(node, stub, "a real node is never the stub allocation");
            // SAFETY: freshly allocated and exclusively owned until publish.
            unsafe { q.enqueue(node) };

            // SAFETY: this thread is the sole consumer.
            let received = unsafe { q.try_dequeue() };
            assert_eq!(
                received, node,
                "a message stamped like the stub must still be delivered"
            );
            assert_ne!(
                received, stub,
                "disambiguation is by pointer identity, so the stub is not returned"
            );
            // SAFETY: the queue handed ownership of `received` to the consumer.
            unsafe { hew_msg_node_free(received) };

            // SAFETY: this thread is the sole consumer.
            let drained = unsafe { q.try_dequeue() };
            assert!(
                drained.is_null(),
                "queue must read empty once the message is consumed"
            );
        }

        // SAFETY: the queue is exclusively owned and holds only the stub.
        unsafe { q.drain_and_free(None) };
    }

    /// Release-visible counterpart of the `debug_assert!` in
    /// [`MpscQueue::consumer_success`]: the stable stub is re-injected by the
    /// consumer on every drain-to-empty, so every dequeue path must step over
    /// it. Handing it out would let the caller free a node the producers still
    /// hold a live pointer to.
    #[test]
    fn stable_stub_never_escapes_to_the_consumer() {
        // burst 1 exercises the single-real-node re-injection path; larger
        // bursts exercise the linked-successor path. Repeated rounds start the
        // second and third burst from a tail that IS the re-injected stub.
        for burst in [1usize, 2, 5] {
            let q = MpscQueue::new().expect("queue allocation should succeed");
            let stub = q.stub_ptr();
            for _round in 0..3 {
                for seq in 0..burst {
                    let msg_type = i32::try_from(seq).expect("test payload id fits in i32");
                    // SAFETY: null payload + zero size is a valid message.
                    let node = unsafe { msg_node_alloc(msg_type, ptr::null(), 0, ptr::null_mut()) };
                    assert!(!node.is_null(), "message allocation must succeed");
                    // SAFETY: freshly allocated and exclusively owned until publish.
                    unsafe { q.enqueue(node) };
                }
                for _ in 0..burst {
                    // SAFETY: this thread is the sole consumer.
                    let node = unsafe { q.try_dequeue() };
                    assert!(!node.is_null(), "published node must be observable");
                    assert_ne!(
                        node, stub,
                        "the stable stub must never be handed to a consumer"
                    );
                    // SAFETY: the queue handed ownership of `node` to the consumer.
                    unsafe { hew_msg_node_free(node) };
                }
                // SAFETY: this thread is the sole consumer.
                let drained = unsafe { q.try_dequeue() };
                assert!(
                    drained.is_null(),
                    "queue must read empty once the burst is consumed"
                );
            }
            // SAFETY: the queue is exclusively owned and holds only the stub.
            unsafe { q.drain_and_free(None) };
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

    /// Drop-glue probe used by envelope tests; bumps a counter so the
    /// test can assert that the final release fires `drop_glue` exactly
    /// once. Each test that touches this state takes
    /// `ENVELOPE_DROP_LOCK` to serialise — the counter is process-wide.
    static ENVELOPE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
    static ENVELOPE_DROP_LOCK: Mutex<()> = Mutex::new(());
    static MESSAGE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
    static REENTRANT_CLOSE_MAILBOX: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn envelope_test_drop_glue(_payload: *mut c_void) {
        ENVELOPE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn message_test_drop_glue(
        _msg_type: i32,
        _payload: *mut c_void,
        _payload_size: usize,
    ) {
        MESSAGE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn reentrant_close_message_drop_glue(
        _msg_type: i32,
        _payload: *mut c_void,
        _payload_size: usize,
    ) {
        let mailbox_addr = REENTRANT_CLOSE_MAILBOX.load(Ordering::Acquire);
        if mailbox_addr != 0 {
            // SAFETY: the owning test keeps the mailbox live until the
            // coalescing send and all resulting drop callbacks complete.
            unsafe { mailbox_close(ptr::without_provenance_mut(mailbox_addr)) };
        }
    }

    unsafe extern "C" fn reentrant_close_coalesce_key(
        msg_type: i32,
        _payload: *mut c_void,
        _payload_size: usize,
    ) -> u64 {
        let mailbox_addr = REENTRANT_CLOSE_MAILBOX.load(Ordering::Acquire);
        if mailbox_addr != 0 {
            // SAFETY: the owning test keeps the mailbox live through callback
            // completion.
            unsafe { mailbox_close(ptr::without_provenance_mut(mailbox_addr)) };
        }
        u64::try_from(msg_type).unwrap_or_default()
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

    /// Gate test: `hew_actor_send_aliased` now delivers via the live
    /// (non-panicking) envelope-mode enqueue. A null actor has no
    /// destination, so the function releases the caller-transferred
    /// envelope refcount **exactly once** (firing `drop_glue` once) and
    /// returns cleanly without panicking. This pins the null-actor exit
    /// of the single-release contract.
    #[test]
    fn actor_send_aliased_null_actor_releases_envelope() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard envelope-new contract; payload allocated by
        // `alloc_test_payload`; envelope ownership transfers into
        // `hew_actor_send_aliased`, which releases it on the null-actor
        // exit.
        unsafe {
            let payload = alloc_test_payload(b"null-actor");
            let env = hew_msg_envelope_new(payload, 10, Some(envelope_test_drop_glue));
            assert!(!env.is_null());
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);
            // Live path: returns without panicking; releases exactly once.
            crate::actor::hew_actor_send_aliased(std::ptr::null_mut(), 0, env);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "null-actor alias send must release the envelope exactly once"
            );
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

    /// Live alias-send delivery: drive an envelope through
    /// `hew_mailbox_send_aliased` to an unbounded mailbox, drain the
    /// node, confirm the receiver borrows the original payload by
    /// reference (no copy), then free the node and assert the envelope
    /// is released **exactly once** (`drop_glue` fires once — no leak, no
    /// double-free).
    #[test]
    fn envelope_alias_send_delivers_and_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively; standard envelope
        // contract; the single refcount transfers into the send.
        unsafe {
            let mb = hew_mailbox_new();
            let payload = alloc_test_payload(b"aliased-delivery");
            let env = hew_msg_envelope_new(payload, 16, Some(envelope_test_drop_glue));
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

            let rc = hew_mailbox_send_aliased(mb, 9, env);
            assert_eq!(rc, HewError::Ok as i32, "alias send must enqueue");
            assert_eq!(hew_mailbox_has_messages(mb), 1);
            // Still no release — the queued node holds the single refcount.
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 0);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 9);
            assert!(
                (*node).data.is_null(),
                "envelope nodes carry no copy buffer"
            );
            assert_eq!((*node).envelope, env, "node aliases the original envelope");
            // Receiver borrows the original payload by reference.
            let borrowed = hew_msg_envelope_payload_ptr((*node).envelope);
            assert_eq!(
                borrowed, payload,
                "payload delivered by reference, not copied"
            );
            let slice = std::slice::from_raw_parts(borrowed.cast::<u8>(), 16);
            assert_eq!(slice, b"aliased-delivery");

            // Dispatch/drain consumes the node → single envelope release.
            hew_msg_node_free(node);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "delivered alias send must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(closed): an alias send to a closed mailbox is rejected and
    /// the envelope is released exactly once (no delivery, no leak).
    #[test]
    fn envelope_alias_send_closed_mailbox_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            mailbox_close(mb);
            let payload = alloc_test_payload(b"closed");
            let env = hew_msg_envelope_new(payload, 6, Some(envelope_test_drop_glue));

            let rc = hew_mailbox_send_aliased(mb, 1, env);
            assert_eq!(rc, HewError::ErrActorStopped as i32);
            assert_eq!(
                hew_mailbox_has_messages(mb),
                0,
                "closed mailbox delivers nothing"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "closed-mailbox alias send must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(null-mailbox): an alias send with a null mailbox releases the
    /// envelope exactly once and reports the actor stopped.
    #[test]
    fn envelope_alias_send_null_mailbox_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: null mailbox is the input under test; envelope contract holds.
        unsafe {
            let payload = alloc_test_payload(b"no-mb");
            let env = hew_msg_envelope_new(payload, 5, Some(envelope_test_drop_glue));
            let rc = hew_mailbox_send_aliased(ptr::null_mut(), 1, env);
            assert_eq!(rc, HewError::ErrActorStopped as i32);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    /// EXIT(fail-overflow): a bounded mailbox at capacity with the `Fail`
    /// policy rejects the alias send and releases the envelope exactly
    /// once. Pins the bounded-reject exit of the single-release contract.
    #[test]
    fn envelope_alias_send_bounded_full_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Fail);
            // Fill capacity with a legacy copy-mode message.
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );

            let payload = alloc_test_payload(b"overflow");
            let env = hew_msg_envelope_new(payload, 8, Some(envelope_test_drop_glue));
            let rc = hew_mailbox_send_aliased(mb, 2, env);
            assert_eq!(
                rc,
                HewError::ErrMailboxFull as i32,
                "Fail policy rejects on overflow"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "bounded-reject alias send must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(drop-old): a bounded mailbox at capacity with the `DropOld`
    /// policy evicts the oldest queued message (freeing it once) and
    /// enqueues the alias node; draining then releases the alias
    /// envelope exactly once. Two distinct nodes, two distinct single
    /// releases.
    #[test]
    fn envelope_alias_send_drop_old_frees_old_and_delivers_new() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::DropOld);
            // Fill capacity with an aliased message so eviction must
            // release an envelope (exercises the old-node free path).
            let old_payload = alloc_test_payload(b"old");
            let old_env = hew_msg_envelope_new(old_payload, 3, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 1, old_env),
                HewError::Ok as i32
            );
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 0);

            // Overflow: DropOld evicts `old_env` (release #1) and enqueues new.
            let new_payload = alloc_test_payload(b"new");
            let new_env = hew_msg_envelope_new(new_payload, 3, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 2, new_env),
                HewError::Ok as i32
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "DropOld must release the evicted envelope exactly once"
            );
            assert_eq!(hew_mailbox_len(mb), 1, "queue stays at capacity");

            // Drain the surviving alias node (release #2).
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            assert_eq!((*node).envelope, new_env);
            hew_msg_node_free(node);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                2,
                "surviving alias envelope released exactly once on drain"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(mailbox-free / drain): an undelivered queued alias node is
    /// released exactly once when the mailbox is freed (shutdown drain),
    /// modelling the actor-stop / supervisor-cancel / session-reset
    /// teardown exits which all route queued nodes through
    /// `hew_msg_node_free`.
    #[test]
    fn envelope_alias_send_mailbox_free_drains_and_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively; the queued node is
        // never drained by the test — mailbox teardown must free it.
        unsafe {
            let mb = hew_mailbox_new();
            let payload = alloc_test_payload(b"undrained");
            let env = hew_msg_envelope_new(payload, 9, Some(envelope_test_drop_glue));
            assert_eq!(hew_mailbox_send_aliased(mb, 3, env), HewError::Ok as i32);
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 0);

            // Teardown drains the queue → single envelope release.
            hew_mailbox_free(mb);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "mailbox teardown must release the undelivered envelope exactly once"
            );
        }
    }
    /// not for the pid handle itself; a `LocalPid<A>`/`RemotePid<A>`
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

    // ── Per-exit single-release regression suite (P5.3) ─────────────────
    //
    // `send_aliased_with_overflow` is the single-release state machine for
    // the aliased send path: it allocates one envelope-mode node up front,
    // after which *every* exit routes the caller-transferred envelope
    // refcount through exactly one release. The tests below pin each exit
    // individually so a future edit that adds/changes an exit cannot
    // silently leak (drop count 0) or double-free (drop count 2). None of
    // these exits reach the scheduler's owned-value dispatch — they are
    // enqueue / overflow-discard / teardown paths whose release is the
    // node free, not a handler.

    /// EXIT(alloc-failure): when the up-front node allocation fails, the
    /// node never takes ownership, so the send must release the
    /// caller-transferred envelope refcount directly — exactly once — and
    /// report OOM. Uses the test-only allocation-failure seam.
    #[test]
    fn envelope_alias_send_node_alloc_oom_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            let payload = alloc_test_payload(b"oom");
            let env = hew_msg_envelope_new(payload, 3, Some(envelope_test_drop_glue));

            // Arm the allocation-failure seam so the *next* mailbox_malloc
            // (the node alloc inside the send) returns null. The mailbox is
            // already built, so no earlier allocation consumes the trigger.
            let _fail = fail_mailbox_alloc_on_nth(0);
            let outcome = send_aliased_with_overflow(&*mb, 7, env, false);
            assert!(
                matches!(outcome, SendOutcome::Oom),
                "node-alloc failure must report Oom"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "node-alloc-failure exit must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(drop-new): a bounded mailbox at capacity with the `DropNew`
    /// policy rejects the incoming alias node and releases its envelope
    /// exactly once.
    #[test]
    fn envelope_alias_send_drop_new_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::DropNew);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );

            let payload = alloc_test_payload(b"dropped");
            let env = hew_msg_envelope_new(payload, 7, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 2, env),
                HewError::ErrMailboxFull as i32
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "DropNew reject exit must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(block-nonblocking): a bounded `Block` mailbox at capacity with
    /// `non_blocking = true` must not wait — it rejects and releases the
    /// envelope exactly once. Exercised by calling
    /// `send_aliased_with_overflow` directly (the public alias FFI always
    /// passes `non_blocking = false`).
    #[test]
    fn envelope_alias_send_block_nonblocking_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );

            let payload = alloc_test_payload(b"wouldblock");
            let env = hew_msg_envelope_new(payload, 10, Some(envelope_test_drop_glue));
            let outcome = send_aliased_with_overflow(&*mb, 2, env, true);
            assert!(
                matches!(outcome, SendOutcome::Failed),
                "Block + non_blocking must fail rather than wait"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "block-nonblocking exit must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    /// EXIT(block-closed-while-waiting): close is published in the exact gap
    /// after a blocked aliased sender checks `closed` but before it enters
    /// `Condvar::wait`. The closer must join `block_wait`, so its one-shot
    /// notification happens only after the sender atomically releases that
    /// predicate mutex into the wait. This is the lost-wake counterexample:
    /// notifying without acquiring the predicate mutex strands the sender.
    #[test]
    fn envelope_alias_send_block_closed_while_waiting_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the mailbox outlives both worker threads (joined before free).
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb as usize;
            let worker = std::thread::spawn(move || {
                // SAFETY: main joins this thread before freeing `mb`
                // (covered by the test's outer `unsafe` scope).
                let mb = mb_addr as *mut HewMailbox;
                let payload = alloc_test_payload(b"closed-wait");
                let env = hew_msg_envelope_new(payload, 11, Some(envelope_test_drop_glue));
                matches!(
                    send_aliased_with_overflow(&*mb, 2, env, false),
                    SendOutcome::Closed
                )
            });

            // The sender owns slow_path + block_wait at the check-to-park seam.
            entered.wait();

            let closer = std::thread::spawn(move || {
                // SAFETY: main joins this thread before freeing `mb`.
                mailbox_close(mb_addr as *mut HewMailbox);
            });

            // `mailbox_close` publishes closed before joining block_wait.
            // Prove publication happened while the sender still owns that
            // predicate mutex; the closer cannot yet have notified.
            let close_deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while !(*mb).closed.load(Ordering::Acquire) {
                assert!(
                    std::time::Instant::now() < close_deadline,
                    "closer did not publish the closed predicate"
                );
                std::thread::yield_now();
            }
            release.wait();
            closer.join().expect("closer thread panicked");

            assert!(
                worker.join().unwrap(),
                "blocked sender must observe the close and report Closed"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "block-closed-while-waiting exit must release the envelope exactly once"
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn envelope_alias_send_coalesce_block_close_gap_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the mailbox outlives both worker threads.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::Block);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>(),
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb.addr();
            let worker = std::thread::spawn(move || {
                let mb = ptr::without_provenance_mut::<HewMailbox>(mb_addr);
                let payload = alloc_test_payload(b"coalesce-closed");
                let env = hew_msg_envelope_new(payload, 15, Some(envelope_test_drop_glue));
                matches!(
                    send_aliased_with_overflow(&*mb, 2, env, false),
                    SendOutcome::Closed
                )
            });

            entered.wait();
            let closer = std::thread::spawn(move || {
                mailbox_close(ptr::without_provenance_mut(mb_addr));
            });
            let close_deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while !(*mb).closed.load(Ordering::Acquire) {
                assert!(
                    std::time::Instant::now() < close_deadline,
                    "closer did not publish the closed predicate"
                );
                std::thread::yield_now();
            }
            release.wait();

            closer.join().expect("closer thread panicked");
            assert!(
                worker.join().expect("aliased sender panicked"),
                "Coalesce→Block aliased sender must wake closed"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "closed Coalesce→Block alias must release once"
            );
            hew_mailbox_free(mb);
        }
    }

    /// EXIT(block-enqueued): a blocked aliased sender that is woken by a
    /// concurrent drain (capacity freed) enqueues its node; the surviving
    /// node releases the envelope exactly once when later drained.
    #[test]
    fn envelope_alias_send_block_enqueued_after_wait_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the mailbox outlives the worker thread (joined before free).
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Block);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );

            let (_hook, entered, release) = BlockPreWaitHookGuard::install(mb);
            let mb_addr = mb as usize;
            let worker = std::thread::spawn(move || {
                // SAFETY: main joins this thread before freeing `mb`
                // (covered by the test's outer `unsafe` scope).
                let mb = mb_addr as *mut HewMailbox;
                let payload = alloc_test_payload(b"enqueue-wait");
                let env = hew_msg_envelope_new(payload, 12, Some(envelope_test_drop_glue));
                matches!(
                    send_aliased_with_overflow(&*mb, 2, env, false),
                    SendOutcome::Enqueued
                )
            });

            // Prove the sender reached the check-to-park seam while owning the
            // queue + predicate mutexes. Release it, then drain: receive first
            // waits for the queue and its notification then waits for
            // block_wait until the sender atomically enters Condvar::wait.
            entered.wait();
            release.wait();
            let filler_node = hew_mailbox_try_recv(mb);
            assert!(!filler_node.is_null());
            hew_msg_node_free(filler_node); // copy-mode: does not touch the counter

            assert!(
                worker.join().unwrap(),
                "woken sender must enqueue and report Enqueued"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                0,
                "alias node enqueued but not yet drained: no release yet"
            );

            // Drain the surviving alias node → single release.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "block-enqueued exit must release the envelope exactly once on drain"
            );

            hew_mailbox_free(mb);
        }
    }

    /// Build a bounded `Coalesce` mailbox (capacity 1) at capacity, with
    /// the given coalesce *fallback* policy, and return the live envelope
    /// pointer plus the mailbox so the caller can assert on the alias send
    /// outcome. The filler is a copy-mode node so it never touches the
    /// envelope drop counter.
    unsafe fn coalesce_fallback_mailbox_at_capacity(
        fallback: HewOverflowPolicy,
    ) -> *mut HewMailbox {
        // SAFETY: caller owns the returned mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Coalesce);
            hew_mailbox_set_coalesce_config(mb, None, fallback);
            let filler: i32 = 1;
            assert_eq!(
                hew_mailbox_send(
                    mb,
                    0,
                    (&raw const filler).cast_mut().cast(),
                    size_of::<i32>()
                ),
                HewError::Ok as i32
            );
            mb
        }
    }

    /// EXIT(coalesce-fallback-drop-new): opaque envelope payloads cannot be
    /// byte-coalesced, so a full `Coalesce` mailbox applies its fallback —
    /// here `DropNew`, which rejects and releases the envelope exactly once.
    #[test]
    fn envelope_alias_send_coalesce_fallback_drop_new_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = coalesce_fallback_mailbox_at_capacity(HewOverflowPolicy::DropNew);
            let payload = alloc_test_payload(b"c-drop-new");
            let env = hew_msg_envelope_new(payload, 10, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 2, env),
                HewError::ErrMailboxFull as i32
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "coalesce→DropNew exit must release the envelope exactly once"
            );
            hew_mailbox_free(mb);
        }
    }

    /// EXIT(coalesce-fallback-fail): the `Fail` fallback rejects on
    /// overflow and releases the envelope exactly once.
    #[test]
    fn envelope_alias_send_coalesce_fallback_fail_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = coalesce_fallback_mailbox_at_capacity(HewOverflowPolicy::Fail);
            let payload = alloc_test_payload(b"c-fail");
            let env = hew_msg_envelope_new(payload, 6, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 2, env),
                HewError::ErrMailboxFull as i32
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "coalesce→Fail exit must release the envelope exactly once"
            );
            hew_mailbox_free(mb);
        }
    }

    /// EXIT(coalesce-fallback-block-nonblocking): the `Block` fallback with
    /// `non_blocking = true` must not wait — it rejects and releases the
    /// envelope exactly once. Driven through `send_aliased_with_overflow`
    /// directly to set `non_blocking`.
    #[test]
    fn envelope_alias_send_coalesce_fallback_block_nonblocking_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = coalesce_fallback_mailbox_at_capacity(HewOverflowPolicy::Block);
            let payload = alloc_test_payload(b"c-block-nb");
            let env = hew_msg_envelope_new(payload, 10, Some(envelope_test_drop_glue));
            let outcome = send_aliased_with_overflow(&*mb, 2, env, true);
            assert!(
                matches!(outcome, SendOutcome::Failed),
                "coalesce→Block + non_blocking must fail rather than wait"
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "coalesce→Block-nonblocking exit must release the envelope exactly once"
            );
            hew_mailbox_free(mb);
        }
    }

    /// EXIT(coalesce-fallback-drop-old): the `DropOld` fallback evicts the
    /// oldest queued node and enqueues the alias node. Here the evicted
    /// node is itself an aliased envelope, so eviction is release #1 and
    /// draining the survivor is release #2 — two distinct single releases.
    #[test]
    fn envelope_alias_send_coalesce_fallback_drop_old_releases_once() {
        let _guard = ENVELOPE_DROP_LOCK.lock().unwrap();
        ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, HewOverflowPolicy::Coalesce);
            hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::DropOld);

            // Fill capacity with an aliased node so eviction must release
            // an envelope.
            let old_payload = alloc_test_payload(b"c-old");
            let old_env = hew_msg_envelope_new(old_payload, 5, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 1, old_env),
                HewError::Ok as i32
            );
            assert_eq!(ENVELOPE_DROP_COUNT.load(Ordering::SeqCst), 0);

            let new_payload = alloc_test_payload(b"c-new");
            let new_env = hew_msg_envelope_new(new_payload, 5, Some(envelope_test_drop_glue));
            assert_eq!(
                hew_mailbox_send_aliased(mb, 2, new_env),
                HewError::Ok as i32
            );
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                1,
                "coalesce→DropOld must release the evicted envelope exactly once"
            );

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);
            assert_eq!(
                ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
                2,
                "surviving alias envelope released exactly once on drain"
            );
            hew_mailbox_free(mb);
        }
    }
}
