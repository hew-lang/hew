//! Canonical per-dispatch execution context carrier.
//!
//! `HewExecutionContext` is the substrate for runtime readers that need
//! per-dispatch state. Reader sites fail closed when the scheduler has not
//! installed this carrier.
#![allow(
    clippy::module_name_repetitions,
    reason = "Runtime ABI substrate names intentionally carry the Hew prefix."
)]

use std::cell::Cell;
use std::cell::RefCell;
use std::ffi::c_void;
use std::mem::offset_of;
use std::ptr;

use crate::actor::HewActor;
pub use crate::arena::ActorArena as HewArena;
use crate::tracing::HewTraceContext;

#[cfg(not(target_arch = "wasm32"))]
pub use crate::task_scope::{HewCancellationToken as HewCancelToken, HewTaskScope};

#[cfg(target_arch = "wasm32")]
pub type HewCancelToken = c_void;
#[cfg(target_arch = "wasm32")]
pub type HewTaskScope = c_void;

pub const EXECUTION_CONTEXT_NOT_INSTALLED: &str = "execution context not installed";
pub const EXECUTION_CONTEXT_NOT_INSTALLED_AT_SPAWN: &str =
    "execution context not installed at spawn site";

/// Opaque actor-state-lock seat reserved for the D24-1 lock migration.
#[derive(Debug)]
pub enum HewActorStateLockState {}

/// Policy governing how a remote `send`/`ask` resolves when the route to the
/// peer is unavailable or the peer is declared dead by the cluster.
///
/// Mirrors the stdlib `PartitionPolicy` enum (`std/link_monitor.hew`) variant
/// order so a discriminant can round-trip between the two without a translation
/// table. The carrier is the per-dispatch
/// [`HewExecutionContext::partition_policy`] slot, encoded **as a small
/// repr-stable integer tag stored inside the pointer-width slot** — never a heap
/// pointer. A null slot reads as [`PartitionPolicy::FailFast`] (the default), so
/// the context-layout offset mirror is unchanged and no allocation
/// or drop obligation is attached to the hot dispatch path.
///
/// The send/ask failure path consumes this; `link_remote`
/// writes a non-default policy into the slot.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PartitionPolicy {
    /// Return `Partition` immediately when the route is unavailable or suspect.
    /// The fail-closed default for every remote ask that does not name a policy.
    #[default]
    FailFast = 0,
    /// Retry until the caller deadline, then return `Timeout` or `Partition`.
    Deadline = 1,
    /// Resolve monitor and supervision partitions as `MonitorLost`.
    MonitorLost = 2,
    /// Cross-node link/supervision fail-together behaviour. **Link-only**: not a
    /// valid `send`/`ask` policy — `link_remote` owns it. A send/ask
    /// failure path that observes this tag fails closed rather than silently
    /// degrading to `FailFast` (no-silent-no-op-stubs).
    CrashLinked = 3,
    /// Stop sending to a suspect or dead peer until a fresh incarnation appears.
    ///
    /// The send/ask path consults a per-runtime quarantine set keyed
    /// `node_id -> incarnation` (the incarnation a peer was quarantined at on its
    /// SWIM-DEAD verdict). A `Quarantine`-policy send/ask to a peer present in the
    /// set fails closed with `Partition` until the peer rejoins at a strictly
    /// higher incarnation, which evicts the entry. Incarnation is a property of
    /// peer identity at the membership layer, so the quarantine set is its home —
    /// NOT a reply-table column. The reply table correlates in-flight asks to a
    /// connection and needs no incarnation field for this behaviour.
    Quarantine = 4,
}

impl PartitionPolicy {
    /// Decode a partition policy from the raw pointer-width slot value.
    ///
    /// A null/zero slot is the absence of an explicit policy and reads as the
    /// `FailFast` default. An unrecognised non-zero tag fails closed to
    /// `FailFast` rather than fabricating a policy — but such a value can only
    /// arise from memory corruption, since every writer uses [`Self::to_slot`].
    #[must_use]
    pub fn from_slot(slot: *const c_void) -> Self {
        match slot as usize {
            1 => Self::Deadline,
            2 => Self::MonitorLost,
            3 => Self::CrashLinked,
            4 => Self::Quarantine,
            // Tag 0 is the absence of an explicit policy (the FailFast default);
            // any other value is unreachable via `to_slot` and fails closed to
            // the same default rather than fabricating a policy.
            _ => Self::FailFast,
        }
    }

    /// Encode this policy as the raw pointer-width slot value (an integer tag,
    /// not a heap pointer).
    #[must_use]
    pub fn to_slot(self) -> *mut c_void {
        (self as usize) as *mut c_void
    }

    /// Whether this policy is a valid choice for a remote `send`/`ask` call.
    ///
    /// `CrashLinked` is link-only; it is never a send/ask policy. A
    /// send/ask path that reads `CrashLinked` must fail closed.
    #[must_use]
    pub fn is_valid_for_send_ask(self) -> bool {
        !matches!(self, Self::CrashLinked)
    }
}

/// Read the partition policy in effect for the current dispatch.
///
/// Returns [`PartitionPolicy::FailFast`] when no execution context is installed
/// or the policy slot is null (the default). Used by the remote send/ask failure
/// path to select the partition outcome.
#[must_use]
pub fn current_partition_policy() -> PartitionPolicy {
    let ctx = current_context();
    if ctx.is_null() {
        return PartitionPolicy::FailFast;
    }
    // SAFETY: `current_context` returns either null (handled above) or a live
    // canonical context installed by the scheduler for this dispatch. The
    // `partition_policy` field is a plain pointer-width integer tag.
    let slot = unsafe { (*ctx).partition_policy };
    PartitionPolicy::from_slot(slot.cast_const())
}

/// Target-architecture-aware byte size of [`HewExecutionContext`].
///
/// Derived from `size_of` rather than a literal so the value is correct on
/// both 64-bit native targets (128 bytes) and 32-bit wasm32 targets (96 bytes,
/// because four-byte pointers eliminate the pointer-sized padding slots).
pub const HEW_CTX_SIZE: usize = std::mem::size_of::<HewExecutionContext>();

/// Byte offset of [`HewExecutionContext::actor`].
pub const HEW_CTX_OFFSET_ACTOR: usize = offset_of!(HewExecutionContext, actor);
/// Byte offset of [`HewExecutionContext::actor_id`].
pub const HEW_CTX_OFFSET_ACTOR_ID: usize = offset_of!(HewExecutionContext, actor_id);
/// Byte offset of [`HewExecutionContext::parent_supervisor`].
pub const HEW_CTX_OFFSET_PARENT_SUPERVISOR: usize =
    offset_of!(HewExecutionContext, parent_supervisor);
/// Byte offset of [`HewExecutionContext::supervisor_child_index`].
///
/// Pointer-width-aware: 24 on 64-bit targets, 20 on 32-bit targets (wasm32).
/// The previous hardcoded literal `24` caused compile-time assertion failures
/// when building for wasm32; `offset_of!` is self-maintaining across widths.
pub const HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX: usize =
    offset_of!(HewExecutionContext, supervisor_child_index);
/// Byte offset of [`HewExecutionContext::flags`].
pub const HEW_CTX_OFFSET_FLAGS: usize = offset_of!(HewExecutionContext, flags);
/// Byte offset of [`HewExecutionContext::cancel_token`].
pub const HEW_CTX_OFFSET_CANCEL_TOKEN: usize = offset_of!(HewExecutionContext, cancel_token);
/// Byte offset of [`HewExecutionContext::task_scope`].
pub const HEW_CTX_OFFSET_TASK_SCOPE: usize = offset_of!(HewExecutionContext, task_scope);
/// Byte offset of [`HewExecutionContext::arena`].
pub const HEW_CTX_OFFSET_ARENA: usize = offset_of!(HewExecutionContext, arena);
/// Byte offset of [`HewExecutionContext::trace`].
pub const HEW_CTX_OFFSET_TRACE: usize = offset_of!(HewExecutionContext, trace);
/// Byte offset of [`HewExecutionContext::trace.span_id`].
pub const HEW_CTX_OFFSET_TRACE_SPAN: usize =
    HEW_CTX_OFFSET_TRACE + offset_of!(HewTraceContext, span_id);
/// Byte offset of [`HewExecutionContext::partition_policy`].
pub const HEW_CTX_OFFSET_PARTITION_POLICY: usize =
    offset_of!(HewExecutionContext, partition_policy);
/// Byte offset of [`HewExecutionContext::prev_context`].
pub const HEW_CTX_OFFSET_PREV_CONTEXT: usize = offset_of!(HewExecutionContext, prev_context);
/// Byte offset of [`HewExecutionContext::lock_seat`].
pub const HEW_CTX_OFFSET_LOCK_SEAT: usize = offset_of!(HewExecutionContext, lock_seat);
/// Byte offset of [`HewExecutionContext::reply_channel`].
pub const HEW_CTX_OFFSET_REPLY_CHANNEL: usize = offset_of!(HewExecutionContext, reply_channel);

/// `flags` bit indicating the current dispatch consumed the reply-channel
/// sender-side reference (i.e. user code called `hew_reply`). Read by the
/// scheduler after dispatch to decide whether to publish a fallback reply.
pub const HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED: u32 = 1 << 0;

/// Canonical per-dispatch carrier installed in worker-local TLS.
///
/// [`HewTraceContext`] is embedded by value and is 40 bytes on all targets
/// (only `u64`/`u8` fields — no pointers). The pre-trace lanes that precede it
/// contain pointer-sized fields whose offsets are therefore target-dependent.
/// All layout values below are derived from `offset_of!` / `size_of!` and
/// reflect the compiler-computed layout for each target:
///
/// | Lane | Native 64-bit | wasm32 32-bit |
/// |------|--------------|---------------|
/// | `trace` start | 56 | 40 |
/// | post-trace start (`partition_policy`) | 96 | 80 |
/// | struct size | 128 | 96 |
///
/// The shift between targets originates in the pre-trace pointer fields:
/// `actor` (ptr) → `actor_id` (u64, align-padded to 8) → `parent_supervisor`
/// (ptr, 8 bytes on 64-bit / 4 bytes on wasm32) → `supervisor_child_index`
/// and every subsequent field moves 4 bytes earlier on wasm32.
/// Use [`HEW_CTX_OFFSET_TRACE`] and [`HEW_CTX_SIZE`] for portable access.
#[repr(C)]
#[derive(Debug, Default)]
pub struct HewExecutionContext {
    /// Actor currently being dispatched.
    pub actor: *mut HewActor,
    /// Snapshotted actor ID for readers that must avoid chasing `actor`.
    pub actor_id: u64,
    /// Snapshotted parent supervisor pointer, or null for unsupervised actors.
    pub parent_supervisor: *mut c_void,
    /// Snapshotted child index in the parent supervisor.
    pub supervisor_child_index: i32,
    /// Runtime context flags. Bit 0 ([`HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED`])
    /// is set when the current dispatch consumed the reply channel by calling
    /// `hew_reply`. Remaining bits are reserved until reader migration lands.
    pub flags: u32,
    /// Cooperative cancellation token for the dispatch, or null when absent.
    pub cancel_token: *mut HewCancelToken,
    /// Active structured-concurrency task scope, or null when absent.
    pub task_scope: *mut HewTaskScope,
    /// Actor-local arena active for this dispatch.
    pub arena: *mut HewArena,
    /// Trace context active for this dispatch.
    pub trace: HewTraceContext,
    /// Partition policy governing remote `send`/`ask` resolution for this
    /// dispatch, encoded as a small repr-stable integer tag in the
    /// pointer-width slot (see [`PartitionPolicy`]). Null reads as the
    /// `FailFast` default. The send/ask failure path reads it;
    /// `link_remote` writes a non-default policy here. Never a heap
    /// pointer — no allocation or drop on the hot dispatch path.
    pub partition_policy: *mut c_void,
    /// Previous canonical context for nested dispatch restoration.
    pub prev_context: *mut HewExecutionContext,
    /// Reserved D24-1 actor-state-lock seat.
    pub lock_seat: *mut HewActorStateLockState,
    /// Reply channel for the message currently being dispatched, or null for
    /// fire-and-forget sends. Owned by the per-dispatch carrier so nested
    /// dispatch (worker A mid-select → worker B inner ask) restores the
    /// outer arm's channel via the `prev_context` chain.
    pub reply_channel: *mut c_void,
}

thread_local! {
    static CURRENT_EXECUTION_CONTEXT: Cell<*mut HewExecutionContext> = const {
        Cell::new(ptr::null_mut())
    };
}

/// Install `ctx` as the current canonical execution context.
///
/// Returns the previously installed context so callers can restore it after the
/// dispatch boundary exits. Passing null clears the current context.
#[must_use]
pub fn set_current_context(ctx: *mut HewExecutionContext) -> *mut HewExecutionContext {
    CURRENT_EXECUTION_CONTEXT.with(|current| current.replace(ctx))
}

/// Return the current canonical execution context for this thread.
///
/// Returns null outside a scheduler dispatch boundary.
#[must_use]
pub fn current_context() -> *mut HewExecutionContext {
    CURRENT_EXECUTION_CONTEXT.with(Cell::get)
}

#[must_use]
pub(crate) fn require_current_context() -> *mut HewExecutionContext {
    let ctx = current_context();
    if ctx.is_null() {
        crate::set_last_error(EXECUTION_CONTEXT_NOT_INSTALLED);
    }
    ctx
}

// ── Reply-channel readers (ctx-backed, target-neutral) ─────────────────
//
// R17 sole-authority: the reply channel for the currently-dispatched message
// lives on `HewExecutionContext::reply_channel`, and the "consumed" bit lives
// in `HewExecutionContext::flags` under `HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED`.
// Nested dispatch (worker A asks worker B mid-select on native; nested
// ask/await on WASM) restores the outer arm's reply channel via the
// `prev_context` chain — no thread-local backing store survives on either
// target.

/// Get the reply channel for the currently-dispatched message.
///
/// Returns null if no reply channel is set (fire-and-forget send) or if no
/// execution context is installed; the latter records a fail-closed
/// diagnostic via [`require_current_context`]. Called from codegen-emitted
/// dispatch functions on both native and WASM targets.
#[no_mangle]
pub extern "C" fn hew_get_reply_channel() -> *mut c_void {
    let ctx = require_current_context();
    if ctx.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: scheduler-installed contexts remain valid for the lifetime of
    // the dispatch; `require_current_context` returned non-null.
    unsafe { (*ctx).reply_channel }
}

/// A single open reply-channel swap recorded on the per-worker swap stack.
///
/// The suspendable-callee driver (`Terminator::SuspendingCallClosure`) brackets
/// each SYNCHRONOUS child-advancing call (the closure-invoke ramp and every
/// `hew_cont_resume`) with a swap so the child closure coroutine's `Return` arm
/// (`hew_get_reply_channel` + `hew_reply`) deposits onto the driver-owned
/// channel rather than the ambient actor channel. The swap is a SCOPED TRANSFER
/// of BOTH the `reply_channel` pointer AND the consumed bit: the child's
/// `hew_reply` flips the consumed bit on the currently-installed context, so the
/// bracket must save the outer dispatch's consumed state on entry and restore it
/// on exit, otherwise the child's consumption corrupts the outer dispatch's
/// fallback/orphan-reply routing (the scheduler reads that bit to decide whether
/// to publish the outer ask's fallback reply).
struct ReplyChannelSwap {
    /// The context the swap mutated; restoration targets this exact context.
    ctx: *mut HewExecutionContext,
    /// The outer reply channel to restore on exit.
    saved_channel: *mut c_void,
    /// The outer consumed bit to restore on exit.
    saved_consumed: bool,
    /// The driver-owned channel installed for the bracket. Freed only on the
    /// trap/cancel/unwind edge (`hew_context_reply_channel_swap_unwind`); the
    /// normal-return pop leaves it live for the codegen finish/abandon teardown.
    #[cfg_attr(
        target_arch = "wasm32",
        allow(dead_code, reason = "read only by the native-only crash unwind path")
    )]
    driver_channel: *mut c_void,
}

thread_local! {
    /// Per-worker stack of open reply-channel swaps. Nesting arises when a child
    /// closure body itself drives another suspending closure synchronously before
    /// it parks; each bracket pushes on entry and pops on exit, so the stack is
    /// empty whenever a dispatch returns to the scheduler on the normal path.
    static REPLY_CHANNEL_SWAP_STACK: RefCell<Vec<ReplyChannelSwap>> =
        const { RefCell::new(Vec::new()) };
}

/// Restore the outer reply-channel pointer AND consumed bit recorded by `swap`.
fn restore_reply_channel_swap(swap: &ReplyChannelSwap) {
    if swap.ctx.is_null() {
        return;
    }
    // SAFETY: the swap recorded the exact context it mutated; that context is
    // still backed by live dispatch storage on this worker (the swap window
    // never spans a park, and the crash path runs on the same worker frame).
    unsafe {
        (*swap.ctx).reply_channel = swap.saved_channel;
        if swap.saved_consumed {
            (*swap.ctx).flags |= HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
        } else {
            (*swap.ctx).flags &= !HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
        }
    }
}

/// Open a scoped reply-channel swap: install `ch` as the current context's
/// reply channel, clear the consumed bit so the child's deposit starts from a
/// clean slate, and push the saved outer pointer + consumed bit onto the
/// per-worker swap stack for later restoration.
///
/// No-op if no execution context is installed.
#[no_mangle]
pub extern "C" fn hew_context_reply_channel_swap_push(ch: *mut c_void) {
    let ctx = current_context();
    if ctx.is_null() {
        return;
    }
    // SAFETY: scheduler-installed contexts remain valid for the lifetime of the
    // dispatch; the current-context pointer is non-null per the guard.
    let (saved_channel, saved_consumed) = unsafe {
        let saved_channel = (*ctx).reply_channel;
        let saved_consumed = ((*ctx).flags & HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED) != 0;
        (*ctx).reply_channel = ch;
        (*ctx).flags &= !HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
        (saved_channel, saved_consumed)
    };
    REPLY_CHANNEL_SWAP_STACK.with(|stack| {
        stack.borrow_mut().push(ReplyChannelSwap {
            ctx,
            saved_channel,
            saved_consumed,
            driver_channel: ch,
        });
    });
}

/// Close the innermost reply-channel swap on the NORMAL-return edge: pop the
/// stack and restore the outer reply-channel pointer + consumed bit. The
/// driver-owned channel is left live for the codegen finish/abandon teardown.
///
/// No-op if the stack is empty (defensive; the codegen brackets are balanced).
#[no_mangle]
pub extern "C" fn hew_context_reply_channel_swap_pop() {
    let swap = REPLY_CHANNEL_SWAP_STACK.with(|stack| stack.borrow_mut().pop());
    if let Some(swap) = swap {
        restore_reply_channel_swap(&swap);
    }
}

/// Unwind ALL open reply-channel swaps on the trap/cancel/unwind edge.
///
/// A child closure that traps/longjmps OR Rust-unwinds mid-call bypasses the
/// codegen swap-pop and the driver-channel teardown, leaving the outer context
/// pointing at the driver channel and the driver channel's refs live. Both
/// sibling scheduler exit edges call this BEFORE they read the dispatch reply
/// channel, so the outer reply routing is restored and the driver channels are
/// torn down exactly once:
/// * the native signal/`siglongjmp` crash-recovery frame (mirroring
///   `hew_actor_state_lock_release_after_panic`), and
/// * the `catch_unwind` `Err` edge when the generated handler Rust-unwound.
///
/// The child never deposited (it trapped/unwound), so both the retained sender
/// ref and the creator ref are released, matching the codegen abandon path.
///
/// No-double-free: the normal-return edge already popped its own frames via the
/// codegen swap-pop, so the swap stack is empty there and neither sibling edge
/// double-restores or double-frees a channel the normal teardown owns.
///
/// Native-only: the scheduler module (and the crash-reply machinery it pairs
/// with — `clear_reply_channel_on`, `current_reply_channel_consumed_on`) is
/// native-only; the WASM scheduler aborts on hard traps and does not drive the
/// suspending-closure swap.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn reply_channel_swap_unwind() {
    loop {
        let swap = REPLY_CHANNEL_SWAP_STACK.with(|stack| stack.borrow_mut().pop());
        let Some(swap) = swap else { break };
        restore_reply_channel_swap(&swap);
        if !swap.driver_channel.is_null() {
            // SAFETY: `driver_channel` is a live `HewReplyChannel` created by the
            // driver via `hew_reply_channel_new` and retained once; teardown
            // cancels and releases both refs, matching the abandon path.
            unsafe { teardown_driver_channel(swap.driver_channel) };
        }
    }
}

/// Test-only: current depth of the per-worker reply-channel swap stack. A
/// non-zero depth after a dispatch returns to the scheduler signals a swap that
/// leaked across an exit edge (the bug the trap/unwind unwinders close).
#[cfg(test)]
pub(crate) fn reply_channel_swap_stack_depth() -> usize {
    REPLY_CHANNEL_SWAP_STACK.with(|stack| stack.borrow().len())
}

/// Cancel + free both refs of a driver-owned reply channel on the crash edge.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn teardown_driver_channel(ch: *mut c_void) {
    let typed = ch.cast();
    // SAFETY: caller guarantees `ch` is a live driver-owned reply channel.
    unsafe {
        crate::reply_channel::hew_reply_channel_cancel(typed);
        crate::reply_channel::hew_reply_channel_free(typed);
        crate::reply_channel::hew_reply_channel_free(typed);
    }
}

/// Mark the current dispatch's reply channel as consumed if it matches `ch`.
///
/// An inner-ctx dispatch consuming its own channel cannot flip the outer
/// ctx's consumed bit because we only flip the bit on the currently-installed
/// ctx and only when the channels match.
pub(crate) fn mark_current_reply_channel_consumed(ch: *mut c_void) {
    if ch.is_null() {
        return;
    }
    let ctx = current_context();
    if ctx.is_null() {
        return;
    }
    // SAFETY: scheduler-installed contexts remain valid for the lifetime of
    // the dispatch; the current-context pointer is non-null per the guard.
    unsafe {
        if (*ctx).reply_channel == ch {
            (*ctx).flags |= HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
        }
    }
}

/// Return the current execution context pointer for use in codegen-emitted
/// terminate trampolines. Called from `__terminate_<Actor>` C-ABI functions
/// that bridge from the runtime's `fn(*mut c_void)` terminate-fn ABI to the
/// `fn(*mut HewExecutionContext)` `ActorHandler` ABI. The context is guaranteed
/// non-null when called from within `call_terminate_fn`, which installs it
/// before invoking the terminate callback.
///
/// # Safety
///
/// Must be called from a codegen-emitted terminate trampoline invoked by
/// `call_terminate_fn`. The returned pointer is valid for the duration of
/// the terminate callback and must not be stored beyond that scope.
#[no_mangle]
pub unsafe extern "C" fn hew_require_execution_context() -> *mut HewExecutionContext {
    require_current_context()
}

#[cfg(test)]
pub(crate) struct TestExecutionContext {
    ctx: *mut HewExecutionContext,
    prev: *mut HewExecutionContext,
}

#[cfg(test)]
impl TestExecutionContext {
    pub(crate) fn install(ctx: HewExecutionContext) -> Self {
        let boxed = Box::new(ctx);
        let raw = Box::into_raw(boxed);
        debug_assert_ne!(raw, ptr::null_mut());
        let prev = set_current_context(raw);
        // SAFETY: raw points to the boxed context owned by this guard.
        unsafe {
            (*raw).prev_context = prev;
        }
        Self { ctx: raw, prev }
    }
}

#[cfg(test)]
impl Drop for TestExecutionContext {
    fn drop(&mut self) {
        let restored = set_current_context(self.prev);
        debug_assert_eq!(restored, self.ctx);
        // SAFETY: self.ctx came from Box::into_raw in install and is dropped once here.
        unsafe {
            drop(Box::from_raw(self.ctx));
        }
    }
}

const _: () = {
    // All HEW_CTX_OFFSET_* constants are now defined via offset_of!, so the
    // offset equalities are trivially true. The non-trivial invariants that
    // must hold on every supported target are:
    //
    // 1. HEW_CTX_OFFSET_TRACE_SPAN must equal trace-base plus span_id offset
    //    inside HewTraceContext (both sides independently derived).
    // 2. struct alignment must be 8 (u64 fields in HewTraceContext impose this
    //    on both 64-bit native and wasm32 targets).
    assert!(
        offset_of!(HewExecutionContext, trace) + offset_of!(HewTraceContext, span_id)
            == HEW_CTX_OFFSET_TRACE_SPAN
    );
    assert!(std::mem::align_of::<HewExecutionContext>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    struct ContextResetGuard;

    impl ContextResetGuard {
        fn new() -> Self {
            let _ = set_current_context(ptr::null_mut());
            Self
        }
    }

    impl Drop for ContextResetGuard {
        fn drop(&mut self) {
            let _ = set_current_context(ptr::null_mut());
        }
    }

    #[test]
    fn install_restore_round_trip() {
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();
        let mut ctx_a = HewExecutionContext::default();
        let ctx_a_ptr = (&raw mut ctx_a);

        assert!(set_current_context(ctx_a_ptr).is_null());
        assert_eq!(current_context(), ctx_a_ptr);
        assert_eq!(set_current_context(ptr::null_mut()), ctx_a_ptr);
        assert!(current_context().is_null());
    }

    #[test]
    fn nested_install_restores_previous_context() {
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();
        let mut ctx_a = HewExecutionContext::default();
        let mut ctx_b = HewExecutionContext::default();
        let outer_context = &raw mut ctx_a;
        let nested_context = &raw mut ctx_b;

        let prev_a = set_current_context(outer_context);
        assert!(prev_a.is_null());

        let prev_b = set_current_context(nested_context);
        assert_eq!(prev_b, outer_context);
        assert_eq!(current_context(), nested_context);

        assert_eq!(set_current_context(prev_b), nested_context);
        assert_eq!(current_context(), outer_context);

        assert_eq!(set_current_context(prev_a), outer_context);
        assert!(current_context().is_null());
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn current_context_is_null_on_non_dispatch_thread() {
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();

        thread::spawn(|| {
            assert!(current_context().is_null());
        })
        .join()
        .expect("thread must not panic");
    }

    /// Regression test: each `HEW_CTX_OFFSET_*` constant must equal the
    /// `offset_of!` value for the corresponding field on every supported target.
    ///
    /// Previously the `HEW_CTX_OFFSET_*` constants were 64-bit literals; on
    /// wasm32 (4-byte pointers) several offsets were wrong, causing the
    /// compile-time assertions in the `const _: ()` block to fire. The fix
    /// replaces every literal with an `offset_of!` expression so the constants
    /// track the struct layout on any target automatically.
    ///
    /// This test asserts the same equality that previously lived only in the
    /// compile-time block, making the regression visible in test output even
    /// when building for a target that cannot run the compile-time assertions
    /// in cross-compiled mode.
    #[test]
    fn compile_time_offset_invariants_match_offset_of_macro() {
        assert_eq!(offset_of!(HewExecutionContext, actor), HEW_CTX_OFFSET_ACTOR);
        assert_eq!(
            offset_of!(HewExecutionContext, actor_id),
            HEW_CTX_OFFSET_ACTOR_ID
        );
        assert_eq!(
            offset_of!(HewExecutionContext, parent_supervisor),
            HEW_CTX_OFFSET_PARENT_SUPERVISOR
        );
        assert_eq!(
            offset_of!(HewExecutionContext, supervisor_child_index),
            HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX
        );
        assert_eq!(offset_of!(HewExecutionContext, flags), HEW_CTX_OFFSET_FLAGS);
        assert_eq!(
            offset_of!(HewExecutionContext, cancel_token),
            HEW_CTX_OFFSET_CANCEL_TOKEN
        );
        assert_eq!(
            offset_of!(HewExecutionContext, task_scope),
            HEW_CTX_OFFSET_TASK_SCOPE
        );
        assert_eq!(offset_of!(HewExecutionContext, arena), HEW_CTX_OFFSET_ARENA);
        assert_eq!(offset_of!(HewExecutionContext, trace), HEW_CTX_OFFSET_TRACE);
        assert_eq!(
            offset_of!(HewExecutionContext, trace) + offset_of!(HewTraceContext, span_id),
            HEW_CTX_OFFSET_TRACE_SPAN
        );
        assert_eq!(
            offset_of!(HewExecutionContext, partition_policy),
            HEW_CTX_OFFSET_PARTITION_POLICY
        );
        assert_eq!(
            offset_of!(HewExecutionContext, prev_context),
            HEW_CTX_OFFSET_PREV_CONTEXT
        );
        assert_eq!(
            offset_of!(HewExecutionContext, lock_seat),
            HEW_CTX_OFFSET_LOCK_SEAT
        );
        assert_eq!(
            offset_of!(HewExecutionContext, reply_channel),
            HEW_CTX_OFFSET_REPLY_CHANNEL
        );
    }

    #[test]
    fn nested_dispatch_restores_outer_reply_channel() {
        // R17 property: install ctx A (reply_channel=ch_A), then ctx B
        // (prev=A, reply_channel=ch_B). After clearing B, the current
        // context's reply_channel must be ch_A again — i.e. nested dispatch
        // cannot clobber the outer arm's reply channel.
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();

        let ch_a: *mut c_void = 0x0A0A_0A0A_usize as *mut c_void;
        let ch_b: *mut c_void = 0x0B0B_0B0B_usize as *mut c_void;

        let mut ctx_a = HewExecutionContext {
            reply_channel: ch_a,
            ..HewExecutionContext::default()
        };
        let outer = &raw mut ctx_a;
        let mut ctx_b = HewExecutionContext {
            reply_channel: ch_b,
            // Inner dispatch records its prev pointer, mirroring the scheduler.
            prev_context: outer,
            ..HewExecutionContext::default()
        };
        let inner = &raw mut ctx_b;

        let prev_a = set_current_context(outer);
        assert!(prev_a.is_null());
        // SAFETY: outer points to the live stack slot for ctx_a.
        assert_eq!(unsafe { (*current_context()).reply_channel }, ch_a);

        let prev_b = set_current_context(inner);
        assert_eq!(prev_b, outer);
        // SAFETY: inner points to the live stack slot for ctx_b.
        assert_eq!(unsafe { (*current_context()).reply_channel }, ch_b);

        // Clear B by restoring its prev — outer's reply_channel must survive.
        let restored = set_current_context(prev_b);
        assert_eq!(restored, inner);
        assert_eq!(current_context(), outer);
        // SAFETY: outer points to the live stack slot for ctx_a.
        assert_eq!(unsafe { (*current_context()).reply_channel }, ch_a);

        let _ = set_current_context(prev_a);
    }

    #[test]
    fn execution_context_size_matches_ctx_size_constant() {
        // HEW_CTX_SIZE is now derived from size_of rather than a literal, so
        // this assertion holds on all targets (128 on native 64-bit, 96 on
        // wasm32) without needing per-target conditional compilation.
        assert_eq!(std::mem::size_of::<HewExecutionContext>(), HEW_CTX_SIZE);
    }

    /// Every `PartitionPolicy` variant round-trips through the pointer-width
    /// slot, and a null slot reads as the `FailFast` default.
    #[test]
    fn partition_policy_round_trips_through_slot() {
        for policy in [
            PartitionPolicy::FailFast,
            PartitionPolicy::Deadline,
            PartitionPolicy::MonitorLost,
            PartitionPolicy::CrashLinked,
            PartitionPolicy::Quarantine,
        ] {
            let slot = policy.to_slot();
            assert_eq!(
                PartitionPolicy::from_slot(slot.cast_const()),
                policy,
                "policy {policy:?} did not round-trip through the slot"
            );
        }
        // A null slot is the absence of a policy → FailFast default.
        assert_eq!(
            PartitionPolicy::from_slot(ptr::null()),
            PartitionPolicy::FailFast,
            "null slot must read as the FailFast default"
        );
        // An out-of-range tag fails closed to FailFast (only reachable via
        // memory corruption; every writer uses to_slot).
        assert_eq!(
            PartitionPolicy::from_slot(99usize as *const c_void),
            PartitionPolicy::FailFast,
            "unrecognised tag must fail closed to FailFast"
        );
    }

    /// `CrashLinked` is rejected as a send/ask policy (link-only); every
    /// other variant admitted on send/ask is accepted.
    #[test]
    fn crash_linked_is_not_valid_for_send_ask() {
        assert!(!PartitionPolicy::CrashLinked.is_valid_for_send_ask());
        for policy in [
            PartitionPolicy::FailFast,
            PartitionPolicy::Deadline,
            PartitionPolicy::MonitorLost,
            PartitionPolicy::Quarantine,
        ] {
            assert!(
                policy.is_valid_for_send_ask(),
                "{policy:?} must be a valid send/ask policy"
            );
        }
    }

    /// With no execution context installed, the policy reader returns the
    /// `FailFast` default rather than dereferencing a null context.
    #[test]
    fn current_partition_policy_defaults_to_fail_fast_without_context() {
        let _context_guard = ContextResetGuard::new();
        let _ = set_current_context(ptr::null_mut());
        assert_eq!(current_partition_policy(), PartitionPolicy::FailFast);
    }

    /// The policy reader projects the installed context's slot tag.
    #[test]
    fn current_partition_policy_reads_installed_slot() {
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();

        let mut ctx = HewExecutionContext {
            partition_policy: PartitionPolicy::Deadline.to_slot(),
            ..HewExecutionContext::default()
        };
        let installed = &raw mut ctx;
        let prev = set_current_context(installed);
        assert_eq!(current_partition_policy(), PartitionPolicy::Deadline);
        let _ = set_current_context(prev);
    }

    /// SEC-1 regression: a scoped reply-channel swap must transfer BOTH the
    /// reply-channel pointer AND the consumed bit, so the child closure's
    /// `hew_reply` (which flips the consumed bit on the currently-installed
    /// context) cannot corrupt the OUTER dispatch's consumed state.
    ///
    /// Pre-fix the swap moved only the pointer: the child's deposit left the
    /// outer context flagged `HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED` for the child
    /// channel after the pointer was swapped back, which suppressed the real
    /// outer fallback/orphan reply. Post-fix the consumed bit is saved on push
    /// and restored on pop, so the outer dispatch is left exactly as it was.
    #[test]
    fn scoped_swap_isolates_child_consumed_bit_from_outer() {
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();

        let outer_ch: *mut c_void = 0x0A0A_0A0A_usize as *mut c_void;
        let driver_ch: *mut c_void = 0x0D0D_0D0D_usize as *mut c_void;

        let mut ctx = HewExecutionContext {
            reply_channel: outer_ch,
            ..HewExecutionContext::default()
        };
        let ctx_ptr = &raw mut ctx;
        let prev = set_current_context(ctx_ptr);
        assert!(prev.is_null());

        // The outer dispatch has NOT consumed its reply channel yet.
        // SAFETY: ctx_ptr is the live stack slot installed above.
        let initial_flags = unsafe { (*ctx_ptr).flags };
        assert_eq!(initial_flags & HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED, 0);

        hew_context_reply_channel_swap_push(driver_ch);
        // SAFETY: ctx_ptr is the live installed context.
        unsafe {
            assert_eq!(
                (*ctx_ptr).reply_channel,
                driver_ch,
                "push installs driver ch"
            );
            assert_eq!(
                (*ctx_ptr).flags & HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED,
                0,
                "push clears the consumed bit for a clean child deposit"
            );
        }

        // The child closure's `Return` arm deposits onto the driver channel,
        // flipping the consumed bit on the currently-installed (outer) context.
        mark_current_reply_channel_consumed(driver_ch);
        // SAFETY: ctx_ptr is the live installed context.
        let after_deposit = unsafe { (*ctx_ptr).flags };
        assert_ne!(
            after_deposit & HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED,
            0,
            "child deposit marks the installed context consumed"
        );

        hew_context_reply_channel_swap_pop();
        // SAFETY: ctx_ptr is the live installed context.
        unsafe {
            assert_eq!(
                (*ctx_ptr).reply_channel,
                outer_ch,
                "pop restores the outer reply channel"
            );
            assert_eq!(
                (*ctx_ptr).flags & HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED,
                0,
                "pop restores the outer consumed bit — the child's consumption is \
                 isolated from the outer dispatch (SEC-1)"
            );
        }
        assert_eq!(
            REPLY_CHANNEL_SWAP_STACK.with(|stack| stack.borrow().len()),
            0,
            "the swap stack is drained after a balanced push/pop"
        );

        let _ = set_current_context(prev);
    }

    /// SEC-2 regression: a child closure that traps/longjmps mid-call bypasses
    /// the codegen swap-pop and driver-channel teardown. The scheduler crash-
    /// recovery edge must unwind every open swap BEFORE it reads the dispatch
    /// reply channel, so the outer reply routing is restored AND the driver-
    /// owned channel is torn down exactly once (no leak, no reply to the wrong
    /// channel).
    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn crash_unwind_restores_outer_routing_and_frees_driver_channel() {
        let _runtime_guard = crate::runtime_test_guard();
        let _context_guard = ContextResetGuard::new();

        let baseline = crate::reply_channel::active_channel_count();

        // The driver creates the channel (creator ref) and retains one sender
        // ref, exactly as the SuspendingCallClosure driver does.
        let driver_ch = crate::reply_channel::hew_reply_channel_new();
        assert!(!driver_ch.is_null());
        // SAFETY: driver_ch was just created and is live.
        unsafe { crate::reply_channel::hew_reply_channel_retain(driver_ch) };
        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline + 1,
            "driver channel is live (new + retain = one active channel, two refs)"
        );

        let outer_ch: *mut c_void = 0x0A0A_0A0A_usize as *mut c_void;
        let mut ctx = HewExecutionContext {
            reply_channel: outer_ch,
            ..HewExecutionContext::default()
        };
        let ctx_ptr = &raw mut ctx;
        let prev = set_current_context(ctx_ptr);
        assert!(prev.is_null());

        hew_context_reply_channel_swap_push(driver_ch.cast());
        // SAFETY: ctx_ptr is the live installed context.
        let installed = unsafe { (*ctx_ptr).reply_channel };
        assert_eq!(installed, driver_ch.cast());

        // The child traps WITHOUT depositing (no `hew_reply`): the swap stays
        // open and both channel refs stay live. The crash-recovery edge unwinds.
        reply_channel_swap_unwind();

        // SAFETY: ctx_ptr is the live installed context.
        let restored = unsafe { (*ctx_ptr).reply_channel };
        assert_eq!(
            restored, outer_ch,
            "unwind restores the outer reply channel so the crash fallback routes \
             to the real outer ask, not the driver channel (SEC-2)"
        );
        assert_eq!(
            REPLY_CHANNEL_SWAP_STACK.with(|stack| stack.borrow().len()),
            0,
            "unwind drains every open swap"
        );
        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline,
            "unwind tears the driver channel down exactly once — no channel ref leak"
        );

        let _ = set_current_context(prev);
    }

    /// CODE-1 regression (ref-accounting contract): the driver UNCONDITIONALLY
    /// retains one sender ref. A unit/never-returning closure body deposits
    /// nothing (it skips `hew_reply`), so the unit finish path must release BOTH
    /// the retained sender ref and the creator ref. Releasing only one — the
    /// pre-fix behavior — leaks the channel. This pins the two-free rule the
    /// codegen unit finish path now emits.
    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn unit_suspending_closure_finish_releases_both_channel_refs() {
        let _runtime_guard = crate::runtime_test_guard();

        let baseline = crate::reply_channel::active_channel_count();

        // Driver setup for a unit closure: new (creator ref) + retain (sender ref).
        let ch = crate::reply_channel::hew_reply_channel_new();
        assert!(!ch.is_null());
        // SAFETY: ch was just created and is live.
        unsafe { crate::reply_channel::hew_reply_channel_retain(ch) };

        // The pre-fix unit finish path freed only ONE ref — demonstrate that this
        // leaves the channel live (the leak the fix removes).
        // SAFETY: ch is live; this releases the creator ref only.
        unsafe { crate::reply_channel::hew_reply_channel_free(ch) };
        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline + 1,
            "releasing only one ref leaks the channel (the pre-fix unit finish bug)"
        );

        // The fixed unit finish path releases the SECOND (retained sender) ref.
        // SAFETY: ch still holds one ref; this releases it, reclaiming the channel.
        unsafe { crate::reply_channel::hew_reply_channel_free(ch) };
        assert_eq!(
            crate::reply_channel::active_channel_count(),
            baseline,
            "the unit finish path's two frees reclaim the channel exactly once (CODE-1)"
        );
    }
}
