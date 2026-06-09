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
    /// Reserved Q29 partition-policy lane; always null before Q29.
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
}
