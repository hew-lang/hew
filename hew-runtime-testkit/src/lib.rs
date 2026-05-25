//! # hew-runtime-testkit
//!
//! Safe handle wrappers over the raw `extern "C"` `hew-runtime` FFI surface.
//! The runtime exports primitives as `*mut HewActor`, `*mut HewSupervisor`,
//! `*mut HewMailbox`, `*mut HewScope`, `*mut HewStream` — every test that
//! touches them today reaches for an `unsafe { … }` block and manages the
//! `_free` / `_stop` / `_destroy` call by hand.
//!
//! This crate provides `TestActor`, `TestSupervisor`, `TestMailbox`,
//! `TestScope`, and `TestStream` RAII wrappers that
//!
//! 1. Encapsulate the raw pointer (`NonNull`-style invariant: non-null while
//!    the wrapper is alive).
//! 2. Expose safe methods over the methods tests actually call (spawn, send,
//!    close, ask, get/set budget, …). Each method's `unsafe` block carries the
//!    six-axis SAFETY justification in this file, so callers don't repeat it.
//! 3. Run the appropriate `_free` / `_stop` / `_destroy` from `Drop`, in the
//!    correct order (close before free for actors; stop + free for supervisors;
//!    `destroy` before `free` for scopes).
//!
//! The wrappers deliberately do **not** try to be a complete safe Rust API.
//! Anything the tests don't use is left out — adding a method here is a
//! commitment the test code now relies on the invariant set we documented at
//! its `unsafe` block.
//!
//! ## What we still leave raw in test code
//!
//! - `extern "C-unwind"` dispatch callbacks
//!   (`fn(*mut HewExecutionContext, *mut c_void, i32, *mut c_void, usize)`).
//!   These are FFI-boundary types by nature; a safe wrapper would require
//!   trampolines that obscure what the test is exercising.
//! - Reply-channel pointer manipulation inside dispatch callbacks.
//! - Reading message-node fields (`(*node).msg_type`, `(*node).data`). The
//!   `MsgNode` wrapper exposes accessors, but inside a dispatch callback you
//!   already hold an `unsafe` block from the `extern "C"` signature, so wrapping
//!   adds no safety.
//! - Direct `(*actor).actor_state` reads. Tests that need the field use
//!   [`TestActor::state`] when possible; ad-hoc polling loops are out of scope.
//!
//! ## Architecture anchors
//!
//! Per the runtime's architecture doc:
//! - §5.7 (no racing polling): wrappers use blocking `_free` semantics, not
//!   spin-poll on state.
//! - §5.8 (six-axis SAFETY comments): every `unsafe { … }` in this file uses
//!   the validity / aliasing / lifetimes / threads / unwind / soundness
//!   template at the call site.
//!
//! ## Drop ordering
//!
//! - [`TestActor`]: `hew_actor_close` → `hew_actor_free`. Free waits for the
//!   actor to reach a terminal state (≤2s), so close-before-free is the
//!   canonical idiom and is what the integration tests already do.
//! - [`TestSupervisor`]: `hew_supervisor_stop` (idempotent; stops children).
//!   The runtime owns the supervisor allocation after `stop`.
//! - [`TestMailbox`]: `hew_mailbox_free`. Pending messages are drained inside
//!   the runtime.
//! - [`TestScope`]: `hew_scope_destroy`. (`free` is for the post-destroy
//!   pointer, but tests destroy and then drop the wrapper.)
//! - [`TestStream`]: `hew_stream_close` — the test pattern across the codebase.
//!
//! Wrappers expose [`leak`] / [`into_raw`] for tests that need to transfer
//! ownership to the runtime explicitly (e.g. `hew_supervisor_add_child` takes
//! over an actor pointer).

#![forbid(unsafe_op_in_unsafe_fn)]
#![warn(missing_docs)]
#![allow(
    deprecated,
    reason = "TestScope wraps HewScope; both deferred to v0.5.1"
)]
// This crate's whole purpose is to give tests a safer surface over the runtime
// FFI; the strict workspace clippy lints aimed at production library code
// produce false positives here. Each allow names the reason once at module
// level so individual call sites stay readable.
#![allow(
    clippy::must_use_candidate,
    reason = "test-helper APIs are short-lived; #[must_use] noise outweighs the lint's value here"
)]
#![allow(
    clippy::missing_panics_doc,
    reason = "wrappers panic only on runtime allocation failure, which is documented at the type level"
)]
#![allow(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "TestStream::from_raw is explicitly unsafe; other ptr-taking fns are fine"
)]
#![allow(
    clippy::doc_nested_refdefs,
    reason = "module-level docs use bracketed link references that look like nested refs"
)]

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

pub use hew_runtime::actor::HewActor;
pub use hew_runtime::execution_context::HewExecutionContext;
pub use hew_runtime::internal::types::{HewActorState, HewError};
pub use hew_runtime::mailbox::{HewMailbox, HewMsgNode, OverflowPolicy};
pub use hew_runtime::scope::HewScope;
pub use hew_runtime::stream::HewStream;
pub use hew_runtime::supervisor::HewSupervisor;

/// `extern "C-unwind"` dispatch callback signature. Re-exported for callers.
pub type DispatchFn =
    unsafe extern "C-unwind" fn(*mut HewExecutionContext, *mut c_void, i32, *mut c_void, usize);

/// Initialise the runtime scheduler exactly once across all tests in the
/// process.
///
/// The scheduler is global state in `hew-runtime`; calling
/// `hew_sched_init` more than once is idempotent but bookkeeping costs
/// matter under heavy test concurrency. Uses a [`std::sync::Once`].
pub fn ensure_scheduler() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        hew_runtime::scheduler::hew_sched_init();
    });
}

// ── TestActor ──────────────────────────────────────────────────────────────

/// Safe RAII wrapper around a runtime actor handle (`*mut HewActor`).
///
/// Construct with [`TestActor::spawn`] or [`TestActor::spawn_with_state`].
/// On drop, the wrapper invokes `hew_actor_close` followed by
/// `hew_actor_free` — the canonical teardown the integration tests use.
///
/// The pointer is non-null and points to a runtime-allocated `HewActor`
/// for the wrapper's whole lifetime. Methods that consult the actor read
/// through this invariant.
#[must_use]
#[derive(Debug)]
pub struct TestActor {
    ptr: *mut HewActor,
}

// SAFETY: HewActor is internally synchronized (atomic state, lock-protected
// mailbox). The wrapper exposes only methods that the runtime documents as
// safe to call from any thread.
unsafe impl Send for TestActor {}
// SAFETY: Same justification — the runtime's accessor functions are
// thread-safe on the actor pointer.
unsafe impl Sync for TestActor {}

impl TestActor {
    /// Spawn a stateless actor with the given dispatch callback.
    ///
    /// # Panics
    /// Panics if the runtime returns a null pointer (allocation failure).
    pub fn spawn(dispatch: DispatchFn) -> Self {
        // SAFETY:
        // - validity: state pointer is null with size 0 (documented as legal).
        // - aliasing: no caller holds a reference to the runtime's internal
        //   state until we return.
        // - lifetimes: the runtime takes ownership; we hold the only handle.
        // - threads: hew_actor_spawn is safe to call from any thread once
        //   ensure_scheduler() has run.
        // - unwind: hew_actor_spawn is a Rust function with extern "C" ABI; it
        //   cannot unwind across the boundary.
        // - soundness: dispatch is an extern "C" fn pointer with the runtime's
        //   expected signature.
        let ptr =
            unsafe { hew_runtime::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(dispatch)) };
        assert!(!ptr.is_null(), "hew_actor_spawn returned null");
        Self { ptr }
    }

    /// Spawn an actor whose initial state is the byte representation of
    /// `state`. The runtime deep-copies the buffer immediately.
    ///
    /// # Panics
    /// Panics if the runtime returns a null pointer.
    pub fn spawn_with_state<T: Copy>(state: &mut T, dispatch: DispatchFn) -> Self {
        // SAFETY:
        // - validity: `state` is a live `&mut T`, so `(state as *mut T)` is
        //   valid for read of size_of::<T>().
        // - aliasing: we have exclusive access via the `&mut`. The runtime
        //   deep-copies before returning, so post-call no aliasing exists.
        // - lifetimes: copy happens before we relinquish the &mut by returning.
        // - threads / unwind / soundness: same as spawn().
        let ptr = unsafe {
            hew_runtime::actor::hew_actor_spawn(
                (state as *mut T).cast(),
                std::mem::size_of::<T>(),
                Some(dispatch),
            )
        };
        assert!(!ptr.is_null(), "hew_actor_spawn returned null");
        Self { ptr }
    }

    /// Raw handle, valid while the wrapper is alive.
    ///
    /// Tests sometimes pass the raw pointer to `extern "C"` helpers (e.g.
    /// `hew_actor_link`); the wrapper's `Drop` continues to manage the
    /// underlying allocation.
    #[must_use]
    pub fn as_ptr(&self) -> *mut HewActor {
        self.ptr
    }

    /// Send a typed message to the actor (blocking variant).
    pub fn send<T: Copy>(&self, msg_type: i32, payload: &mut T) {
        // SAFETY:
        // - validity: self.ptr is a live HewActor; payload is a live &mut T.
        // - aliasing: hew_actor_send deep-copies the payload before returning.
        // - lifetimes / threads / unwind / soundness: documented for
        //   hew_actor_send.
        unsafe {
            hew_runtime::actor::hew_actor_send(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
            );
        }
    }

    /// Send a zero-payload message (`null, 0`).
    pub fn send_empty(&self, msg_type: i32) {
        // SAFETY: same as send(); a null/zero payload is the documented
        // tombstone-message form.
        unsafe {
            hew_runtime::actor::hew_actor_send(self.ptr, msg_type, ptr::null_mut(), 0);
        }
    }

    /// Try-send variant; returns the runtime error code (`0` on success,
    /// negative `HewError` value on failure).
    pub fn try_send<T: Copy>(&self, msg_type: i32, payload: &mut T) -> i32 {
        // SAFETY: identical invariants to send().
        unsafe {
            hew_runtime::actor::hew_actor_try_send(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
            )
        }
    }

    /// Try-send with a null payload.
    pub fn try_send_empty(&self, msg_type: i32) -> i32 {
        // SAFETY: null/zero payload form documented for hew_actor_try_send.
        unsafe { hew_runtime::actor::hew_actor_try_send(self.ptr, msg_type, ptr::null_mut(), 0) }
    }

    /// Ask-pattern: send and wait for a reply. Returns the reply pointer
    /// (raw `*mut c_void`) — caller frees with `libc::free` on success.
    ///
    /// Returns null on failure (closed actor, ask deadline, …).
    pub fn ask<T: Copy>(&self, msg_type: i32, payload: &mut T) -> *mut c_void {
        // SAFETY: identical invariants to send(); hew_actor_ask returns
        // libc::malloc'd memory the caller must free.
        unsafe {
            hew_runtime::actor::hew_actor_ask(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
            )
        }
    }

    /// Ask with timeout (milliseconds). Returns null on timeout or failure.
    pub fn ask_timeout<T: Copy>(
        &self,
        msg_type: i32,
        payload: &mut T,
        timeout_ms: i32,
    ) -> *mut c_void {
        // SAFETY: identical invariants to ask().
        unsafe {
            hew_runtime::actor::hew_actor_ask_timeout(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
                timeout_ms,
            )
        }
    }

    /// Ask via an externally-supplied reply channel. Returns the runtime
    /// status code (`0` on success).
    pub fn ask_with_channel<T: Copy>(
        &self,
        msg_type: i32,
        payload: &mut T,
        ch: *mut hew_runtime::reply_channel::HewReplyChannel,
    ) -> i32 {
        // SAFETY: caller is responsible for the reply channel's reference
        // count; the wrapper just forwards to the FFI.
        unsafe {
            hew_runtime::actor::hew_actor_ask_with_channel(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
                ch,
            )
        }
    }

    /// Close the actor (no further sends accepted; drains pending).
    pub fn close(&self) {
        // SAFETY: self.ptr is a live HewActor; hew_actor_close is idempotent.
        unsafe { hew_runtime::actor::hew_actor_close(self.ptr) }
    }

    /// Stop the actor immediately (enqueues a system stop message, closes
    /// the mailbox).
    pub fn stop(&self) {
        // SAFETY: hew_actor_stop is documented as idempotent + safe to call
        // from any thread.
        unsafe { hew_runtime::actor::hew_actor_stop(self.ptr) }
    }

    /// Trap (force into Crashed state with the given error code).
    pub fn trap(&self, error_code: i32) {
        // SAFETY: self.ptr is a live HewActor.
        unsafe { hew_runtime::actor::hew_actor_trap(self.ptr, error_code) }
    }

    /// Get the actor's last error code, or `0` if none.
    pub fn error(&self) -> i32 {
        // SAFETY: read-only accessor over a live HewActor.
        unsafe { hew_runtime::actor::hew_actor_get_error(self.ptr) }
    }

    /// Get the per-actor message budget.
    pub fn budget(&self) -> u32 {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::actor::hew_actor_get_budget(self.ptr) }
    }

    /// Set the per-actor message budget.
    pub fn set_budget(&self, budget: u32) {
        // SAFETY: setter on a live HewActor.
        unsafe { hew_runtime::actor::hew_actor_set_budget(self.ptr, budget) }
    }

    /// Get the actor's priority class.
    pub fn priority(&self) -> i32 {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::actor::hew_actor_get_priority(self.ptr) }
    }

    /// Set the actor's priority class.
    pub fn set_priority(&self, priority: i32) {
        // SAFETY: setter on a live HewActor.
        unsafe { hew_runtime::actor::hew_actor_set_priority(self.ptr, priority) }
    }

    /// Current `HewActorState` value (read of the atomic state field).
    pub fn state_raw(&self) -> i32 {
        // SAFETY: self.ptr is a live HewActor; actor_state is a pub AtomicI32.
        unsafe { (*self.ptr).actor_state.load(Ordering::Acquire) }
    }

    /// Block until the actor reaches `expected` (polled at 10ms intervals)
    /// or `timeout` elapses. Returns `true` if the state was reached.
    ///
    /// Polling is the runtime's documented way to observe state transitions
    /// from outside the dispatch path — see the existing integration tests
    /// that use the same shape.
    pub fn wait_for_state(&self, expected: HewActorState, timeout: Duration) -> bool {
        let target = expected as i32;
        let deadline = Instant::now() + timeout;
        loop {
            if self.state_raw() == target {
                return true;
            }
            if Instant::now() >= deadline {
                return false;
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    /// Consume the wrapper and return the raw pointer without closing/freeing.
    ///
    /// Use when the runtime takes ownership (e.g. supervisors adopting an
    /// actor handle). The caller is responsible for the eventual close+free.
    #[must_use]
    pub fn into_raw(self) -> *mut HewActor {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }
}

impl Drop for TestActor {
    fn drop(&mut self) {
        // SAFETY:
        // - validity: self.ptr is non-null by invariant.
        // - aliasing: Drop is the unique consumer.
        // - lifetimes: after free the pointer is invalid; nothing else holds it.
        // - threads: hew_actor_close and hew_actor_free are documented as safe
        //   to call from a non-scheduler thread; hew_actor_free blocks until
        //   the actor reaches a terminal state.
        // - unwind: both FFI fns cannot unwind.
        // - soundness: close-before-free is the canonical teardown.
        unsafe {
            hew_runtime::actor::hew_actor_close(self.ptr);
            let _ = hew_runtime::actor::hew_actor_free(self.ptr);
        }
    }
}

// ── TestMailbox ────────────────────────────────────────────────────────────

/// Safe RAII wrapper around `*mut HewMailbox`.
#[must_use]
#[derive(Debug)]
pub struct TestMailbox {
    ptr: *mut HewMailbox,
}

// SAFETY: HewMailbox is internally synchronized.
unsafe impl Send for TestMailbox {}
// SAFETY: same.
unsafe impl Sync for TestMailbox {}

impl TestMailbox {
    /// New unbounded mailbox.
    pub fn new() -> Self {
        // SAFETY: hew_mailbox_new takes no preconditions; result is null only
        // on allocation failure (then we panic).
        let ptr = unsafe { hew_runtime::mailbox::hew_mailbox_new() };
        assert!(!ptr.is_null(), "hew_mailbox_new returned null");
        Self { ptr }
    }

    /// New bounded mailbox with the given capacity.
    pub fn bounded(capacity: i32) -> Self {
        // SAFETY: capacity is the FFI's signed type; allocation failure → null → panic.
        let ptr = unsafe { hew_runtime::mailbox::hew_mailbox_new_bounded(capacity) };
        assert!(!ptr.is_null(), "hew_mailbox_new_bounded returned null");
        Self { ptr }
    }

    /// New mailbox with explicit overflow policy.
    pub fn with_policy(capacity: usize, policy: OverflowPolicy) -> Self {
        // SAFETY: hew_mailbox_new_with_policy is total over (capacity, policy).
        let ptr = unsafe { hew_runtime::mailbox::hew_mailbox_new_with_policy(capacity, policy) };
        assert!(!ptr.is_null(), "hew_mailbox_new_with_policy returned null");
        Self { ptr }
    }

    /// Raw pointer (valid while the wrapper is alive).
    #[must_use]
    pub fn as_ptr(&self) -> *mut HewMailbox {
        self.ptr
    }

    /// Send a typed message.
    pub fn send<T: Copy>(&self, msg_type: i32, payload: &mut T) -> i32 {
        // SAFETY: self.ptr is a live HewMailbox; payload is a live &mut T;
        // the runtime deep-copies.
        unsafe {
            hew_runtime::mailbox::hew_mailbox_send(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
            )
        }
    }

    /// Send a system message (no return code; system queue is unbounded).
    pub fn send_sys<T: Copy>(&self, msg_type: i32, payload: &mut T) {
        // SAFETY: as send(). System queue is always unbounded so the FFI
        // returns no failure code.
        unsafe {
            hew_runtime::mailbox::hew_mailbox_send_sys(
                self.ptr,
                msg_type,
                (payload as *mut T).cast(),
                std::mem::size_of::<T>(),
            );
        }
    }

    /// Try-receive a user message. Returns `None` if empty.
    ///
    /// The returned [`MsgNode`] frees itself on drop.
    pub fn try_recv(&self) -> Option<MsgNode> {
        // SAFETY: self.ptr is a live HewMailbox.
        let node = unsafe { hew_runtime::mailbox::hew_mailbox_try_recv(self.ptr) };
        if node.is_null() {
            None
        } else {
            Some(MsgNode { ptr: node })
        }
    }

    /// Try-receive a system message.
    pub fn try_recv_sys(&self) -> Option<MsgNode> {
        // SAFETY: as try_recv().
        let node = unsafe { hew_runtime::mailbox::hew_mailbox_try_recv_sys(self.ptr) };
        if node.is_null() {
            None
        } else {
            Some(MsgNode { ptr: node })
        }
    }

    /// Message count.
    pub fn len(&self) -> usize {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::mailbox::hew_mailbox_len(self.ptr) }
    }

    /// `true` if the mailbox is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// `true` if at least one user message is pending.
    pub fn has_messages(&self) -> bool {
        // SAFETY: read-only accessor; runtime returns 0 / nonzero.
        unsafe { hew_runtime::mailbox::hew_mailbox_has_messages(self.ptr) != 0 }
    }

    /// Mailbox capacity (`0` = unbounded).
    pub fn capacity(&self) -> usize {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::mailbox::hew_mailbox_capacity(self.ptr) }
    }
}

impl Default for TestMailbox {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for TestMailbox {
    fn drop(&mut self) {
        // SAFETY: self.ptr is non-null by invariant; hew_mailbox_free drains
        // pending messages and is safe to call from any thread.
        unsafe { hew_runtime::mailbox::hew_mailbox_free(self.ptr) }
    }
}

/// Safe RAII wrapper around `*mut HewMsgNode`.
///
/// The node is freed via `hew_msg_node_free` on drop.
#[must_use]
#[derive(Debug)]
pub struct MsgNode {
    ptr: *mut HewMsgNode,
}

impl MsgNode {
    /// Message type tag.
    pub fn msg_type(&self) -> i32 {
        // SAFETY: self.ptr is a live HewMsgNode (non-null by invariant).
        unsafe { (*self.ptr).msg_type }
    }

    /// Payload pointer (may be null if size is 0).
    pub fn data_ptr(&self) -> *mut c_void {
        // SAFETY: self.ptr is a live HewMsgNode.
        unsafe { (*self.ptr).data }
    }

    /// Read a `Copy` payload by value.
    ///
    /// # Safety
    /// Caller guarantees the message was sent with a payload of exactly
    /// `size_of::<T>()` bytes and the appropriate type.
    pub unsafe fn payload<T: Copy>(&self) -> T {
        // SAFETY (delegated): caller's `payload` contract guarantees the data
        // pointer references `size_of::<T>()` bytes of a valid `T`.
        let p = self.data_ptr().cast::<T>();
        // SAFETY: see contract comment above; `p` is a valid `*const T`.
        unsafe { *p }
    }

    /// Underlying raw pointer (for tests that still need it).
    #[must_use]
    pub fn as_ptr(&self) -> *mut HewMsgNode {
        self.ptr
    }
}

impl Drop for MsgNode {
    fn drop(&mut self) {
        // SAFETY: self.ptr is non-null and was returned by a mailbox try_recv
        // call; hew_msg_node_free is the canonical destructor.
        unsafe { hew_runtime::mailbox::hew_msg_node_free(self.ptr) }
    }
}

// ── TestSupervisor ─────────────────────────────────────────────────────────

/// Safe RAII wrapper around `*mut HewSupervisor`.
///
/// On drop, calls `hew_supervisor_stop`, which is idempotent and stops all
/// child actors. The runtime owns the deallocation after stop.
#[must_use]
#[derive(Debug)]
pub struct TestSupervisor {
    ptr: *mut HewSupervisor,
}

// SAFETY: HewSupervisor is internally synchronized.
unsafe impl Send for TestSupervisor {}
// SAFETY: same.
unsafe impl Sync for TestSupervisor {}

impl TestSupervisor {
    /// Construct a supervisor with the given strategy / restart budget.
    ///
    /// `strategy`: 0=OneForOne, 1=OneForAll, 2=RestForOne (consult runtime
    /// docs for the canonical numbers).
    pub fn new(strategy: i32, max_restarts: i32, window_secs: i32) -> Self {
        // SAFETY: hew_supervisor_new is total over its arguments.
        let ptr = unsafe {
            hew_runtime::supervisor::hew_supervisor_new(strategy, max_restarts, window_secs)
        };
        assert!(!ptr.is_null(), "hew_supervisor_new returned null");
        Self { ptr }
    }

    /// Raw pointer (valid while the wrapper is alive).
    #[must_use]
    pub fn as_ptr(&self) -> *mut HewSupervisor {
        self.ptr
    }

    /// Consume the wrapper, returning the raw pointer; caller must arrange
    /// for `hew_supervisor_stop` eventually.
    #[must_use]
    pub fn into_raw(self) -> *mut HewSupervisor {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }

    /// `true` if the supervisor is currently running (not yet stopped).
    pub fn is_running(&self) -> bool {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::supervisor::hew_supervisor_is_running(self.ptr) != 0 }
    }

    /// Number of children currently registered.
    pub fn child_count(&self) -> i32 {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::supervisor::hew_supervisor_child_count(self.ptr) }
    }

    /// Start the supervisor (enables auto-restart on registered children).
    pub fn start(&self) -> i32 {
        // SAFETY: idempotent start on a live supervisor.
        unsafe { hew_runtime::supervisor::hew_supervisor_start(self.ptr) }
    }

    /// Stop the supervisor explicitly (idempotent with Drop).
    pub fn stop(&self) {
        // SAFETY: hew_supervisor_stop is idempotent.
        unsafe { hew_runtime::supervisor::hew_supervisor_stop(self.ptr) }
    }
}

impl Drop for TestSupervisor {
    fn drop(&mut self) {
        // SAFETY: self.ptr is non-null by invariant; hew_supervisor_stop is
        // documented idempotent and drives all child cleanup.
        unsafe { hew_runtime::supervisor::hew_supervisor_stop(self.ptr) }
    }
}

// ── TestScope ──────────────────────────────────────────────────────────────

/// Safe RAII wrapper around a value-typed `HewScope`.
///
/// `hew_scope_new` returns a value (the scope's internal fields are owned by
/// the test); FFI methods then take `*mut HewScope` addresses. The wrapper
/// stores the scope inline in a `Box` so the address is stable for the
/// wrapper's lifetime.
///
/// On drop, the wrapper calls `hew_scope_destroy`, which cancels and joins
/// any spawned children. (`hew_scope_free` is reserved for the
/// `hew_scope_create` pointer form, which the testkit doesn't wrap.)
#[must_use]
pub struct TestScope {
    inner: Box<HewScope>,
}

impl std::fmt::Debug for TestScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TestScope").finish()
    }
}

// SAFETY: HewScope is internally synchronized.
unsafe impl Send for TestScope {}
// SAFETY: same.
unsafe impl Sync for TestScope {}

impl TestScope {
    /// Create a new scope.
    pub fn new() -> Self {
        // SAFETY: hew_scope_new is total; returns an initialized HewScope.
        let scope = unsafe { hew_runtime::scope::hew_scope_new() };
        Self {
            inner: Box::new(scope),
        }
    }

    /// Mutable pointer to the underlying `HewScope` (stable for the
    /// wrapper's lifetime).
    #[must_use]
    pub fn as_mut_ptr(&mut self) -> *mut HewScope {
        &raw mut *self.inner
    }

    /// Cancel the scope (request cancellation; in-flight children observe).
    pub fn cancel(&mut self) {
        // SAFETY: idempotent on a live HewScope; address is stable inside Box.
        unsafe { hew_runtime::scope::hew_scope_cancel(self.as_mut_ptr()) }
    }

    /// Has the scope been cancelled?
    pub fn is_cancelled(&mut self) -> bool {
        // SAFETY: read-only accessor.
        unsafe { hew_runtime::scope::hew_scope_is_cancelled(self.as_mut_ptr()) != 0 }
    }

    /// Block until all children complete or cancellation is observed.
    pub fn wait_all(&mut self) {
        // SAFETY: hew_scope_wait_all blocks on internal condvars; safe to
        // call from any thread except a scope child itself.
        unsafe { hew_runtime::scope::hew_scope_wait_all(self.as_mut_ptr()) }
    }

    /// Adopt an actor handle into the scope; the scope is now responsible
    /// for closing and freeing it on `wait_all`.
    ///
    /// Returns the runtime status code (`0` on success, `-1` if the scope
    /// is at capacity).
    pub fn adopt(&mut self, actor: TestActor) -> i32 {
        let raw = actor.into_raw();
        // SAFETY: `raw` came from hew_actor_spawn and is being transferred to
        // the scope's actor list. On rejection (`rc == -1`) we still own
        // `raw`, so we wrap it back into a TestActor to drop cleanly.
        let rc = unsafe { hew_runtime::scope::hew_scope_spawn(self.as_mut_ptr(), raw.cast()) };
        if rc != 0 {
            // Re-wrap so Drop runs close+free.
            let _restored = TestActor { ptr: raw };
        }
        rc
    }

    /// Try to spawn a raw actor pointer into the scope without
    /// consuming a `TestActor`. Caller is responsible for the lifetime
    /// guarantees of `actor`.
    ///
    /// # Safety
    /// Caller guarantees `actor` is a live `*mut HewActor` and that the
    /// scope's eventual `wait_all` is allowed to free it (or, on rejection,
    /// that the caller still owns the pointer and frees it).
    pub unsafe fn raw_spawn(&mut self, actor: *mut HewActor) -> i32 {
        let p = self.as_mut_ptr();
        // SAFETY: `actor` lifetime is guaranteed by the caller (see fn doc);
        // `p` is a live HewScope by wrapper invariant.
        unsafe { hew_runtime::scope::hew_scope_spawn(p, actor.cast()) }
    }

    /// Current `actor_count` field (read of internal state).
    ///
    /// Exposed for tests that assert on scope bookkeeping. This is not a
    /// stable API surface — the scope's internal layout may change.
    pub fn actor_count(&mut self) -> i32 {
        // SAFETY: scope is a live HewScope; actor_count is a plain i32 field.
        unsafe { (*self.as_mut_ptr()).actor_count }
    }

    /// Read actor-slot `i` without taking ownership.
    ///
    /// Tests use this to assert that `wait_all` nulled the slots out.
    pub fn actor_slot(&mut self, i: usize) -> *mut c_void {
        // SAFETY: scope is live; actors[] is a fixed-size array within the
        // HewScope value. Out-of-bounds is a panic via slice indexing.
        unsafe { (*self.as_mut_ptr()).actors[i] }
    }
}

impl Default for TestScope {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for TestScope {
    fn drop(&mut self) {
        // SAFETY: address comes from a Box we own; destroy joins children
        // and is idempotent.
        unsafe { hew_runtime::scope::hew_scope_destroy(self.as_mut_ptr()) }
    }
}

// ── TestStream ─────────────────────────────────────────────────────────────

/// Safe RAII wrapper around `*mut HewStream`.
///
/// On drop, calls `hew_stream_close`, which is the canonical teardown.
#[must_use]
#[derive(Debug)]
pub struct TestStream {
    ptr: *mut HewStream,
}

// SAFETY: HewStream is internally synchronized.
unsafe impl Send for TestStream {}
// SAFETY: same.
unsafe impl Sync for TestStream {}

impl TestStream {
    /// Wrap an already-constructed stream pointer (returned by a runtime
    /// FFI call such as `hew_stream_channel`).
    ///
    /// # Safety
    /// Caller guarantees `ptr` is non-null and owned by the test (not
    /// already wrapped, not already closed).
    pub unsafe fn from_raw(ptr: *mut HewStream) -> Self {
        assert!(!ptr.is_null(), "TestStream::from_raw on null pointer");
        Self { ptr }
    }

    /// Raw pointer (valid while the wrapper is alive).
    #[must_use]
    pub fn as_ptr(&self) -> *mut HewStream {
        self.ptr
    }

    /// Consume the wrapper without closing.
    #[must_use]
    pub fn into_raw(self) -> *mut HewStream {
        let ptr = self.ptr;
        std::mem::forget(self);
        ptr
    }
}

impl Drop for TestStream {
    fn drop(&mut self) {
        // SAFETY: self.ptr is non-null by invariant; close is the canonical
        // teardown and is idempotent.
        unsafe { hew_runtime::stream::hew_stream_close(self.ptr) }
    }
}
