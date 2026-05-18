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

pub(crate) const EXECUTION_CONTEXT_NOT_INSTALLED: &str = "execution context not installed";

/// Opaque actor-state-lock seat reserved for the D24-1 lock migration.
#[derive(Debug)]
pub enum HewActorStateLockState {}

/// Stable byte size of [`HewExecutionContext`].
pub const HEW_CTX_SIZE: usize = 128;

/// Byte offset of [`HewExecutionContext::actor`].
pub const HEW_CTX_OFFSET_ACTOR: usize = 0;
/// Byte offset of [`HewExecutionContext::actor_id`].
pub const HEW_CTX_OFFSET_ACTOR_ID: usize = 8;
/// Byte offset of [`HewExecutionContext::parent_supervisor`].
pub const HEW_CTX_OFFSET_PARENT_SUPERVISOR: usize = 16;
/// Byte offset of [`HewExecutionContext::supervisor_child_index`].
pub const HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX: usize = 24;
/// Byte offset of [`HewExecutionContext::flags`].
pub const HEW_CTX_OFFSET_FLAGS: usize = 28;
/// Byte offset of [`HewExecutionContext::cancel_token`].
pub const HEW_CTX_OFFSET_CANCEL_TOKEN: usize = 32;
/// Byte offset of [`HewExecutionContext::task_scope`].
pub const HEW_CTX_OFFSET_TASK_SCOPE: usize = 40;
/// Byte offset of [`HewExecutionContext::arena`].
pub const HEW_CTX_OFFSET_ARENA: usize = 48;
/// Byte offset of [`HewExecutionContext::trace`].
pub const HEW_CTX_OFFSET_TRACE: usize = 56;
/// Byte offset of [`HewExecutionContext::partition_policy`].
pub const HEW_CTX_OFFSET_PARTITION_POLICY: usize = 96;
/// Byte offset of [`HewExecutionContext::prev_context`].
pub const HEW_CTX_OFFSET_PREV_CONTEXT: usize = 104;
/// Byte offset of [`HewExecutionContext::lock_seat`].
pub const HEW_CTX_OFFSET_LOCK_SEAT: usize = 112;
/// Byte offset of [`HewExecutionContext::_reserved`].
pub const HEW_CTX_OFFSET_RESERVED: usize = 120;

/// Canonical per-dispatch carrier installed in worker-local TLS.
///
/// The trace lane embeds the runtime's actual [`HewTraceContext`] by value. It
/// is 40 bytes in this runtime, so the post-trace lanes are pinned after offset
/// 96 while preserving the 128-byte carrier size.
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
    /// Runtime context flags; bits are reserved until reader migration lands.
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
    /// Remaining reserved lane for future dispatch substrate.
    #[allow(
        clippy::pub_underscore_fields,
        reason = "The public ABI carrier intentionally exposes the reserved lane as `_reserved`."
    )]
    pub _reserved: [u64; 1],
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
    assert!(offset_of!(HewExecutionContext, actor) == HEW_CTX_OFFSET_ACTOR);
    assert!(offset_of!(HewExecutionContext, actor_id) == HEW_CTX_OFFSET_ACTOR_ID);
    assert!(offset_of!(HewExecutionContext, parent_supervisor) == HEW_CTX_OFFSET_PARENT_SUPERVISOR);
    assert!(
        offset_of!(HewExecutionContext, supervisor_child_index)
            == HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX
    );
    assert!(offset_of!(HewExecutionContext, flags) == HEW_CTX_OFFSET_FLAGS);
    assert!(offset_of!(HewExecutionContext, cancel_token) == HEW_CTX_OFFSET_CANCEL_TOKEN);
    assert!(offset_of!(HewExecutionContext, task_scope) == HEW_CTX_OFFSET_TASK_SCOPE);
    assert!(offset_of!(HewExecutionContext, arena) == HEW_CTX_OFFSET_ARENA);
    assert!(offset_of!(HewExecutionContext, trace) == HEW_CTX_OFFSET_TRACE);
    assert!(offset_of!(HewExecutionContext, partition_policy) == HEW_CTX_OFFSET_PARTITION_POLICY);
    assert!(offset_of!(HewExecutionContext, prev_context) == HEW_CTX_OFFSET_PREV_CONTEXT);
    assert!(offset_of!(HewExecutionContext, lock_seat) == HEW_CTX_OFFSET_LOCK_SEAT);
    assert!(offset_of!(HewExecutionContext, _reserved) == HEW_CTX_OFFSET_RESERVED);
    assert!(std::mem::size_of::<HewExecutionContext>() == HEW_CTX_SIZE);
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

    #[test]
    fn field_offsets_match_pinned_constants() {
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
            offset_of!(HewExecutionContext, _reserved),
            HEW_CTX_OFFSET_RESERVED
        );
    }

    #[test]
    fn execution_context_size_is_pinned() {
        assert_eq!(std::mem::size_of::<HewExecutionContext>(), HEW_CTX_SIZE);
        assert_eq!(std::mem::size_of::<HewExecutionContext>(), 128);
    }
}
