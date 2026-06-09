//! Hew runtime: lambda-actor instance per `spawn-lambda` literal.
//!
//! The lambda-actor is the runtime carrier behind the surface
//! `actor |params| { body }` construct. Per design §5.2 + §8.3 the
//! handle is `Duplex<Msg, Reply>` underneath; this module composes that
//! substrate (it does NOT reinvent it) and adds:
//!
//! - **Strong/weak ref discipline** (§5.9 ratification 2) so the lambda
//!   body's self-reference does NOT keep the actor alive past external
//!   refcount zero.
//! - **Upgrade-on-use**: a weak self-send attempts to upgrade to a
//!   strong handle; if the external strong refcount has reached zero,
//!   the upgrade fails and the send surfaces as
//!   [`SendError::ActorStopped`] instead of resurrecting the actor.
//! - **Body dispatch**: the constructor spawns a dedicated OS thread
//!   that drains the mailbox and invokes the body callback
//!   (`HewLambdaActorBody`) behind `std::panic::catch_unwind`. A body
//!   panic marks the actor stopped, reclaims any in-flight message
//!   and reply-channel ownership, and causes all subsequent sends/asks
//!   to surface `ActorStopped`.
//! - **Ask reply correlation**: ask-shaped actors accept `hew_lambda_actor_ask`
//!   calls. The reply channel is framed into the mailbox envelope; the
//!   body callback writes a reply payload; the runtime delivers it to
//!   the waiter via `HewReplyChannel`.
//!
//! ## Refcount layering
//!
//! `HewLambdaActor` is a thin wrapper around `Arc<LambdaActorInner>`.
//! The Arc's strong count IS the external lambda-actor refcount. When
//! it reaches zero the Arc drops `LambdaActorInner`, which in turn
//! drops the inner `HewDuplex` (close-both-dirs). Body-side captures
//! hold `Weak<LambdaActorInner>` and try `upgrade` on every use; that
//! upgrade is the §5.9 ratification 2 stop-guarantee.
//!
//! ## ABI contract (frozen at slice 1)
//!
//! ```c
//! typedef int32_t (*HewLambdaActorBody)(
//!     void*          state,
//!     const uint8_t* msg,
//!     size_t         msg_len,
//!     uint8_t**      reply_out,
//!     size_t*        reply_len_out
//! );
//! typedef void (*HewLambdaActorStateDrop)(void* state);
//! ```
//!
//! - `state`: per-instance capture record; runtime owns after construction.
//! - `msg` / `msg_len`: borrowed only for the duration of the call; body
//!   must not retain the pointer.
//! - `reply_out` / `reply_len_out`: must be set to null/0 on tell-shape bodies;
//!   on ask-shape, body allocates via `hew_duplex_payload_free`-compatible
//!   allocator (`Box::into_raw(bytes.into_boxed_slice())`).
//! - Return `0` = success; non-zero = body error, actor marked stopped.
//! - `catch_unwind` wraps every invocation; panic = stopped + payload reclaim.
//!
//! ## Ask mailbox framing
//!
//! Ask-shaped message envelopes are framed as:
//!   `[reply_ch_ptr: 8 bytes LE][msg_bytes...]`
//! The dispatch loop extracts the pointer, calls the body, and posts the
//! reply to the `HewReplyChannel`. Tell envelopes are raw msg bytes.

#![cfg(not(target_arch = "wasm32"))]
#![allow(
    clippy::missing_errors_doc,
    reason = "SendError discriminants are documented on the type"
)]
#![allow(
    clippy::must_use_candidate,
    reason = "send return-codes are routinely inspected only at the FFI surface; clone_handle is intentionally side-effecting via atomic refcount bumps"
)]

use std::mem::ManuallyDrop;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};

use crate::duplex::{HewDuplex, HewRecvHalf, HewSendHalf, RecvError, SendError};
use crate::reply_channel::{
    hew_reply, hew_reply_channel_free, hew_reply_channel_new, hew_reply_channel_retain,
    hew_reply_channel_retire_orphaned_ask_sender_ref, hew_reply_wait_with_size, HewReplyChannel,
};

// ── ABI callback types ─────────────────────────────────────────────────────

/// Body-dispatch callback.
///
/// Invoked by the dispatch thread for each message. `state` is the
/// per-instance capture record. `msg`/`msg_len` are the raw message bytes
/// (borrowed; do not retain). For ask-shape, write the reply payload
/// pointer + length into `reply_out` / `reply_len_out`; the runtime takes
/// ownership and delivers it to the waiter. For tell-shape, set both to
/// null/0. Return `0` on success; any non-zero value marks the actor stopped.
///
/// The `"C-unwind"` ABI allows Rust panics to propagate through the
/// boundary and be caught by the `std::panic::catch_unwind` in the
/// dispatch loop. This is intentional: LLVM-emitted body stubs are Rust
/// closures lowered to standalone `extern "C-unwind" fn` symbols, and a
/// panic in user code must be containable without aborting the process.
///
/// SAFETY: invoked from a Rust thread behind `std::panic::catch_unwind`;
/// the pointer arguments are valid for the duration of the call.
pub type HewLambdaActorBody = unsafe extern "C-unwind" fn(
    state: *mut core::ffi::c_void,
    msg: *const u8,
    msg_len: usize,
    reply_out: *mut *mut u8,
    reply_len_out: *mut usize,
) -> i32;

/// State-drop callback.
///
/// Called exactly once when the dispatch loop stops (after the last message
/// or after a panic) before `LambdaActorInner` is released. If the
/// constructor failed before handing ownership to the runtime, the caller
/// (codegen) must invoke this or otherwise free the capture record.
///
/// SAFETY: `state` is the same pointer passed to `hew_lambda_actor_new`;
/// called at most once.
pub type HewLambdaActorStateDrop = unsafe extern "C-unwind" fn(state: *mut core::ffi::c_void);

// ── Body-shape discriminant ────────────────────────────────────────────────

/// Body-shape discriminator. Recorded at construction so the runtime
/// knows whether to provision a reply correlator; codegen reads this to
/// decide whether `hew_lambda_actor_send` (tell) or
/// `hew_lambda_actor_ask` (ask) is the correct dispatch.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaShape {
    /// `actor |params| { body }` — no return value; sends are
    /// fire-and-forget.
    Tell = 0,
    /// `actor |params| -> Ret { body }` — sends carry a reply
    /// correlator; the caller awaits a `Reply` payload.
    Ask = 1,
}

// ── Ask envelope framing ───────────────────────────────────────────────────

/// Byte size of the reply-channel pointer prefix in ask envelopes.
const ASK_REPLY_CH_PREFIX_LEN: usize = 8;

/// Frame an ask message: `[reply_ch_ptr: 8 LE bytes][msg_bytes...]`.
fn frame_ask_envelope(reply_ch: *mut HewReplyChannel, msg: &[u8]) -> Vec<u8> {
    let ptr_bytes = (reply_ch as u64).to_le_bytes();
    let mut env = Vec::with_capacity(ASK_REPLY_CH_PREFIX_LEN + msg.len());
    env.extend_from_slice(&ptr_bytes);
    env.extend_from_slice(msg);
    env
}

/// Extract the reply channel pointer and message slice from an ask envelope.
///
/// # Safety
///
/// `env` must have been produced by `frame_ask_envelope` and the embedded
/// pointer must be a live `*mut HewReplyChannel`.
unsafe fn unframe_ask_envelope(env: &[u8]) -> (*mut HewReplyChannel, &[u8]) {
    debug_assert!(
        env.len() >= ASK_REPLY_CH_PREFIX_LEN,
        "ask envelope too short: {} bytes",
        env.len()
    );
    let mut ptr_bytes = [0u8; 8];
    ptr_bytes.copy_from_slice(&env[..ASK_REPLY_CH_PREFIX_LEN]);
    let ch = u64::from_le_bytes(ptr_bytes) as *mut HewReplyChannel;
    (ch, &env[ASK_REPLY_CH_PREFIX_LEN..])
}

// ── Allocator-pairing helpers ──────────────────────────────────────────────

/// Free a body-allocated reply buffer via Rust's `GlobalAlloc`.
///
/// # Allocator pairing contract
///
/// Body callbacks allocate reply payloads with
/// `Box::into_raw(bytes.into_boxed_slice())`, using Rust's `GlobalAlloc`.
/// This function is the **symmetric deallocation** path for those buffers.
///
/// Reply-channel reply copies use `libc::malloc` (inside
/// [`crate::reply_channel::alloc_reply_buffer`]) and **must** be freed with
/// [`crate::reply_channel::hew_reply_payload_free`] — see that module for the
/// counterpart.  Mixing the two allocators is **undefined behaviour** on any
/// platform where `GlobalAlloc ≠ libc malloc` (e.g. jemalloc, mimalloc).
///
/// A `#[cfg(debug_assertions)]` assert verifies the pointer is not
/// libc-tracked before deallocating.
///
/// # Safety
///
/// `ptr` must be a non-null pointer previously produced by a body callback via
/// `Box<[u8]>` with the given `len`.
#[inline]
unsafe fn free_body_reply_buf(ptr: *mut u8, len: usize) {
    #[cfg(debug_assertions)]
    debug_assert!(
        !crate::alloc_tracker::debug_is_libc_tracked(ptr),
        "allocator-pairing contract violation: reply_out {ptr:p} is libc-tracked \
         (allocated via the reply channel); the body must allocate via Box. \
         See reply_channel::hew_reply_payload_free for the libc-allocated free path.",
    );
    // SAFETY: ptr was allocated by the body via Box<[u8]>-compatible GlobalAlloc;
    // we are the sole owner at this point.
    unsafe {
        let slice = std::slice::from_raw_parts_mut(ptr, len);
        drop(Box::from_raw(slice as *mut [u8]));
    }
}

/// Allocate a reply buffer of `len` bytes for a compiler-emitted lambda-actor
/// body to use as the `reply_out` payload.
///
/// **Allocator-pairing**: the returned pointer is `Box<[u8]>`-shaped
/// (`Box::into_raw(vec![0u8; len].into_boxed_slice())`) and is the
/// symmetric ALLOC counterpart to the existing internal [`free_body_reply_buf`].
/// LLVM-emitted body callbacks call this from outside Rust and therefore
/// cannot use `Box::into_raw` directly — this thin wrapper closes that
/// substrate gap without crossing the libc/GlobalAlloc pairing boundary
/// the runtime documents. The runtime never tracks this allocation in
/// `alloc_tracker`, so the `free_body_reply_buf` debug-assert still
/// catches genuine pairing violations (libc-tracked pointers reaching
/// `Box::from_raw`).
///
/// Returns null on zero-size requests so the body can use the same call
/// pattern for tell-shape (no reply payload) and ask-shape (with payload)
/// without a special-case on the codegen side. A null `reply_out` /
/// `reply_len_out == 0` is the runtime's documented "no reply" sentinel.
///
/// # Safety
///
/// The returned pointer must be passed to either:
/// - the runtime via `reply_out` from a body callback (the runtime takes
///   ownership and frees via `free_body_reply_buf`), or
/// - directly freed by the caller via `Box::from_raw` reconstruction with
///   the same `len`.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_body_alloc_reply_buf(len: usize) -> *mut u8 {
    if len == 0 {
        return ptr::null_mut();
    }
    let boxed: Box<[u8]> = vec![0u8; len].into_boxed_slice();
    Box::into_raw(boxed).cast::<u8>()
}

// ── Process-wide drain registry ────────────────────────────────────────────

/// Count of currently-running lambda-actor dispatch threads. Bumped at
/// `HewLambdaActor::new` BEFORE spawn; decremented inside the dispatch
/// thread by the `LambdaDispatchGuard` Drop impl AFTER `dispatch_loop`
/// returns (covering both natural exit on `Closed` and panic-unwind).
///
/// Used by [`hew_lambda_drain_all`] so a non-actor-using `main` can block
/// until every spawned lambda actor has drained its mailbox before the
/// process exits. Mirrors the role `hew_shutdown_wait` plays for
/// scheduler workers, but for lambda actors which run on their own
/// dedicated OS threads (not the work-stealing scheduler).
static ACTIVE_LAMBDA_DISPATCH: AtomicUsize = AtomicUsize::new(0);

/// RAII guard that decrements [`ACTIVE_LAMBDA_DISPATCH`] on drop. Lives
/// on the dispatch thread's stack so the decrement is unconditional —
/// `dispatch_loop` returning normally OR a body panic that unwinds the
/// dispatch thread both run this Drop.
struct LambdaDispatchGuard;

impl Drop for LambdaDispatchGuard {
    fn drop(&mut self) {
        ACTIVE_LAMBDA_DISPATCH.fetch_sub(1, Ordering::AcqRel);
    }
}

/// Block until every spawned lambda actor's dispatch thread has exited,
/// or until `timeout_ms` elapses (0 = use 5 s default — same ceiling as
/// `hew_shutdown_wait`).
///
/// Returns 0 on clean drain, 1 on timeout. Always safe to call: returns
/// immediately when no lambda actors have ever been spawned.
///
/// Called by codegen from the `main` exit epilogue (alongside the
/// existing `hew_shutdown_initiate` + `hew_shutdown_wait` for scheduler
/// actors) so a `let log = actor |s| { … }; log("hi")` program
/// observably runs the body before the process exits. Without this, the
/// dispatch thread races against process termination and the body's
/// `println` is silently lost.
///
/// Backoff: short busy-wait under 1 ms (yield), then `park_timeout` with
/// growing intervals up to 16 ms so a long-running actor doesn't spin
/// the polling thread.
#[no_mangle]
pub extern "C" fn hew_lambda_drain_all(timeout_ms: i64) -> i32 {
    let timeout = if timeout_ms <= 0 {
        std::time::Duration::from_secs(5)
    } else {
        // Negative values handled above; the cast keeps a positive
        // `i64` ms within `u64` range. clippy `cast-sign-loss` is
        // satisfied via `try_into`-with-saturate.
        let ms = u64::try_from(timeout_ms).unwrap_or(u64::MAX);
        std::time::Duration::from_millis(ms)
    };
    let deadline = std::time::Instant::now() + timeout;
    let mut backoff = std::time::Duration::from_micros(100);
    let max_backoff = std::time::Duration::from_millis(16);
    loop {
        if ACTIVE_LAMBDA_DISPATCH.load(Ordering::Acquire) == 0 {
            return 0;
        }
        let now = std::time::Instant::now();
        if now >= deadline {
            return 1;
        }
        let remaining = deadline - now;
        let sleep = backoff.min(remaining);
        std::thread::park_timeout(sleep);
        backoff = (backoff * 2).min(max_backoff);
    }
}

// ── Internal shared state ──────────────────────────────────────────────────

/// Internal shared state. Shared between the dispatch thread (via
/// `Arc`) and the weak-ref upgrade path.
///
/// `mailbox_in` (the send half) lives in `HewLambdaActor` wrapped in
/// `Arc<HewSendHalf>`. When the last `HewLambdaActor` clone drops, the
/// `Arc<HewSendHalf>` drops, `HewSendHalf::drop` calls `release_sender`,
/// closing the send side of the queue. The dispatch thread's `recv` then
/// returns `RecvError::Closed` and the loop exits naturally.
///
/// Separating `mailbox_in` from `LambdaActorInner` is the key invariant
/// that allows the dispatch thread to hold `Arc<LambdaActorInner>` without
/// preventing the mailbox from closing when external handles are released.
pub struct LambdaActorInner {
    /// Recv-half retained from the body-side pair endpoint. The
    /// body-dispatch loop drains messages here.
    mailbox_out: HewRecvHalf,
    /// Body-shape discriminator.
    shape: LambdaShape,
    /// Set to `true` when the actor has stopped (body panic, non-zero
    /// body return, or all senders gone). Checked by `send` / `ask` to
    /// return `ActorStopped` without touching the mailbox.
    stopped: AtomicBool,
    /// Body dispatch callback. Called once per message by the dispatch
    /// thread. Raw function pointer — no lifetime; valid for the full
    /// lifetime of the actor (codegen ensures this by keeping the
    /// function symbol alive via static linkage).
    body_fn: HewLambdaActorBody,
    /// Per-instance capture/state record. Owned by the runtime after
    /// construction; freed via `state_drop` when the dispatch loop exits.
    ///
    /// SAFETY: must be `Send` — the dispatch thread reads it from a
    /// different OS thread than the constructor. Codegen's capture record
    /// is always heap-allocated with no thread-local interior state.
    state: *mut core::ffi::c_void,
    /// Destructor for `state`. Called exactly once after the dispatch
    /// loop stops.
    state_drop: HewLambdaActorStateDrop,
}

// SAFETY: `LambdaActorInner` is sent to the dispatch thread. The
// `state` pointer is a heap-allocated capture record with no
// thread-local interior state (codegen invariant). `mailbox_out`
// (HewRecvHalf) is accessed only by the single dispatch thread.
// The `stopped` AtomicBool and `body_fn`/`state_drop` fn ptrs are
// Send/Sync-compatible.
unsafe impl Send for LambdaActorInner {}
// SAFETY: `stopped` is Sync via AtomicBool. `body_fn` / `state_drop`
// are read-only after construction. `state` is only accessed by the
// single dispatch thread (never by multiple threads concurrently).
// `mailbox_out` is accessed only by the single dispatch thread.
unsafe impl Sync for LambdaActorInner {}

impl std::fmt::Debug for LambdaActorInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LambdaActorInner")
            .field("shape", &self.shape)
            .field("stopped", &self.stopped)
            .finish_non_exhaustive()
    }
}

impl LambdaActorInner {
    /// Construct the inner state and return it together with the send half.
    ///
    /// The `HewSendHalf` is returned separately so that `HewLambdaActor`
    /// can own it wrapped in an `Arc`; when the last external strong handle
    /// drops, the Arc releases the send half and the queue closes, causing
    /// the dispatch thread's `recv` to return `Closed`.
    fn new(
        mailbox_capacity: usize,
        shape: LambdaShape,
        body_fn: HewLambdaActorBody,
        state: *mut core::ffi::c_void,
        state_drop: HewLambdaActorStateDrop,
    ) -> (Self, HewSendHalf) {
        // Construct a cross-wired pair and immediately split each side
        // into the half it needs. `client` retains the send capability
        // toward the body; `body` retains the recv capability draining
        // those messages.
        let (client, body) = HewDuplex::new_pair(mailbox_capacity, mailbox_capacity);
        let mailbox_in = client.into_send_half();
        let mailbox_out = body.into_recv_half();
        let inner = Self {
            mailbox_out,
            shape,
            stopped: AtomicBool::new(false),
            body_fn,
            state,
            state_drop,
        };
        (inner, mailbox_in)
    }

    /// Drain the next message envelope from the mailbox. Used by the
    /// body-dispatch loop.
    pub fn recv(&self) -> Result<Vec<u8>, RecvError> {
        self.mailbox_out.recv()
    }

    #[must_use]
    pub fn shape(&self) -> LambdaShape {
        self.shape
    }

    /// Mark the actor as stopped. Subsequent `send` / `ask` return
    /// `ActorStopped`. Called by the dispatch thread on panic or
    /// non-zero body return.
    fn mark_stopped(&self) {
        self.stopped.store(true, Ordering::Release);
    }

    fn is_stopped(&self) -> bool {
        self.stopped.load(Ordering::Acquire)
    }
}

/// Test-only: captures the message passed to `set_last_error` when
/// `state_drop` panics. Written by [`LambdaActorInner::drop`] on the
/// dispatch thread; read by `state_drop_panic_does_not_abort_process`.
#[cfg(test)]
static LAST_DROP_PANIC_MSG: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

impl Drop for LambdaActorInner {
    fn drop(&mut self) {
        // `state_drop` is called here — after the Arc strong count reaches
        // zero — which happens either: (a) the dispatch thread finished and
        // dropped the Arc it holds, or (b) the dispatch thread panicked and
        // the thread's stack unwinding dropped its Arc. Either way, the
        // dispatch thread is no longer accessing `state` at this point,
        // satisfying the exclusive-access requirement.
        //
        // SAFETY: `state_drop` is a valid function pointer (set at
        // construction, never mutated). `state` is the same pointer
        // passed to `hew_lambda_actor_new`; called exactly once here.
        //
        // Wrapped in `catch_unwind` so that a panic in `state_drop` while
        // this drop runs during stack unwinding cannot trigger a double-panic
        // → process abort.
        let state_drop = self.state_drop;
        let state = self.state;
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // SAFETY: state_drop is a valid extern "C-unwind" fn pointer (set at
            // construction, never mutated); state is the same pointer passed to
            // hew_lambda_actor_new; called exactly once here.
            unsafe { state_drop(state) };
        }));
        if let Err(panic_payload) = result {
            let msg: String = if let Some(s) = panic_payload.downcast_ref::<&str>() {
                (*s).to_string()
            } else if let Some(s) = panic_payload.downcast_ref::<String>() {
                s.clone()
            } else {
                "non-string panic payload".to_string()
            };
            let error_msg = format!("LambdaActorInner::drop user state_drop panicked: {msg}");
            crate::set_last_error(error_msg.clone());
            #[cfg(test)]
            {
                *LAST_DROP_PANIC_MSG
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(error_msg);
            }
        }
    }
}

// ── Strong / Weak handles ──────────────────────────────────────────────────

/// Strong lambda-actor handle. Holding a `HewLambdaActor` keeps the
/// actor alive. When the last strong handle drops:
/// 1. The `Arc<HewSendHalf>` drops → `HewSendHalf::drop` releases the
///    sender-cap → the queue's send side closes.
/// 2. The dispatch thread's next `recv` returns `RecvError::Closed`.
/// 3. The dispatch thread exits, dropping its `Arc<LambdaActorInner>`.
/// 4. `LambdaActorInner::drop` calls `state_drop(state)`.
/// 5. Any `HewLambdaActorWeak::upgrade` returns `None` from this point.
#[derive(Debug, Clone)]
pub struct HewLambdaActor {
    /// Shared send half. `Arc` ownership so that all clones of this
    /// handle share a single `HewSendHalf` that closes when the last
    /// clone drops.
    mailbox_in: Arc<HewSendHalf>,
    /// Shared inner state. Used for the stopped-flag and weak-ref
    /// upgrade target.
    inner: Arc<LambdaActorInner>,
}

impl HewLambdaActor {
    /// Construct a new lambda-actor instance. The constructor spawns a
    /// dedicated dispatch thread that drains the mailbox and invokes
    /// `body_fn(state, ...)` for each message.
    ///
    /// Returns `None` if the dispatch thread could not be spawned.
    ///
    /// `state` is owned by the runtime after this call. If `new` returns
    /// `None`, the caller is responsible for calling `state_drop(state)`.
    #[must_use]
    pub fn new(
        mailbox_capacity: usize,
        shape: LambdaShape,
        body_fn: HewLambdaActorBody,
        state: *mut core::ffi::c_void,
        state_drop: HewLambdaActorStateDrop,
    ) -> Option<Self> {
        let (inner_val, mailbox_in) =
            LambdaActorInner::new(mailbox_capacity, shape, body_fn, state, state_drop);
        let inner = Arc::new(inner_val);
        // Wrap mailbox_in in Arc so all clones share one send-half.
        // When the Arc's strong count drops to zero, HewSendHalf::drop
        // closes the queue and the dispatch thread's recv returns Closed.
        let mailbox_in = Arc::new(mailbox_in);
        // Drain registry: increment BEFORE spawn so a probe between spawn
        // and increment cannot observe count=0 with a live thread. The
        // counter is decremented by the dispatch closure's `_guard` Drop
        // — but Drop order matters for `hew_lambda_drain_all` to mean
        // "all captured state has been torn down":
        //   - The dispatch closure's body has TWO local bindings — `_guard`
        //     declared FIRST and `inner` (the moved-in Arc clone) declared
        //     SECOND. Rust drops locals in REVERSE declaration order, so
        //     at closure-body exit `inner` drops first (decrementing the
        //     Arc strong count; if this is the last ref,
        //     `LambdaActorInner::drop` runs and invokes `state_drop(state)`)
        //     and THEN `_guard` drops (decrementing the dispatch counter).
        //   - Therefore: when `ACTIVE_LAMBDA_DISPATCH` reaches 0, all
        //     captured user state has already been freed via state_drop.
        //   - If `dispatch_inner` were captured by the closure (instead of
        //     moved into a body-local AFTER `_guard`), it would drop with
        //     the closure struct AFTER both body locals, leaving a window
        //     where `hew_lambda_drain_all` returns counter=0 but
        //     state_drop has not yet run. Security-review fix 2026-06-08:
        //     drain ≠ join, but drain MUST mean state-drop-has-run.
        ACTIVE_LAMBDA_DISPATCH.fetch_add(1, Ordering::AcqRel);
        // Spawn the dispatch thread. It holds a strong Arc reference to
        // keep LambdaActorInner alive while the loop runs. The thread
        // exits when recv returns Closed (i.e., when all external
        // Arc<HewSendHalf> clones drop).
        let dispatch_inner = Arc::clone(&inner);
        let spawn_result = std::thread::Builder::new()
            .name("hew-lambda-dispatch".to_string())
            .spawn(move || {
                // Drop-order discipline (see comment above): `_guard`
                // declared FIRST so it drops LAST (after `inner`).
                let _guard = LambdaDispatchGuard;
                // Move the captured Arc into a body-local declared AFTER
                // `_guard`. Body-locals drop in reverse declaration order,
                // so `inner` drops BEFORE `_guard` — triggering
                // `LambdaActorInner::drop`'s `state_drop(state)` call
                // before the counter decrements.
                let inner = dispatch_inner;
                dispatch_loop(&inner);
                // (implicit drops at body end:
                //    1. `inner` → Arc strong-count drop → state_drop
                //    2. `_guard` → ACTIVE_LAMBDA_DISPATCH.fetch_sub
                // — so drain-all observes counter==0 only AFTER
                // state_drop has run on this dispatch thread.)
            });
        if spawn_result.is_err() {
            // Spawn failed: roll back the counter so a later drain-all
            // doesn't wait for a thread that never started.
            ACTIVE_LAMBDA_DISPATCH.fetch_sub(1, Ordering::AcqRel);
            return None;
        }
        Some(Self { mailbox_in, inner })
    }

    /// Send a message envelope. Failure modes:
    ///
    /// - `SendError::Closed` if the send half was already closed, or
    /// - `SendError::ActorStopped` if the body panicked / returned non-zero, or
    /// - `SendError::Ok` on success.
    pub fn send(&self, msg: Vec<u8>) -> SendError {
        if self.inner.is_stopped() {
            return SendError::ActorStopped;
        }
        self.mailbox_in.send(msg)
    }

    /// Downgrade to a weak handle. The weak handle does NOT keep the
    /// actor alive (§5.9 ratification 2 — the lambda body captures
    /// its own binding name via this primitive).
    #[must_use]
    pub fn downgrade(&self) -> HewLambdaActorWeak {
        HewLambdaActorWeak {
            inner: Arc::downgrade(&self.inner),
            mailbox_in: Arc::downgrade(&self.mailbox_in),
        }
    }

    /// Refcount-bump clone of the strong handle.
    #[must_use = "the clone bumps the actor's external refcount; drop releases it"]
    pub fn clone_handle(&self) -> Self {
        self.clone()
    }
}

/// Weak lambda-actor handle. Body-side self-references use this so
/// that the body never keeps the actor alive past external refcount
/// zero. The upgrade-on-use discipline turns a self-send into
/// `SendError::ActorStopped` once the externals are gone.
#[derive(Debug, Clone)]
pub struct HewLambdaActorWeak {
    /// Weak reference to the inner state.
    inner: Weak<LambdaActorInner>,
    /// Weak reference to the shared send half. Upgrading both is
    /// atomic from the perspective of the lifecycle invariant: if
    /// `mailbox_in` can be upgraded, the actor is still alive.
    mailbox_in: Weak<HewSendHalf>,
}

impl HewLambdaActorWeak {
    /// Try to upgrade to a strong handle. Returns `None` if the
    /// external strong refcount has reached zero (i.e., the
    /// `Arc<HewSendHalf>` has been released).
    #[must_use]
    pub fn upgrade(&self) -> Option<HewLambdaActor> {
        // Both must succeed; if mailbox_in is gone the actor is stopped.
        let mailbox_in = self.mailbox_in.upgrade()?;
        let inner = self.inner.upgrade()?;
        Some(HewLambdaActor { mailbox_in, inner })
    }

    /// Attempt a self-send through the weak handle. Upgrades just
    /// long enough to call into the mailbox; if the upgrade fails,
    /// returns `SendError::ActorStopped` per §5.9 ratification 2.
    pub fn send(&self, msg: Vec<u8>) -> SendError {
        match self.upgrade() {
            Some(strong) => strong.send(msg),
            None => SendError::ActorStopped,
        }
    }

    #[must_use = "the weak clone bumps only the weak count; drop releases it"]
    pub fn clone_handle(&self) -> Self {
        self.clone()
    }
}

// ── Dispatch loop ──────────────────────────────────────────────────────────

/// Body-dispatch loop. Runs on a dedicated OS thread. Drains
/// `mailbox_out`, invokes `body_fn` behind `catch_unwind`, and routes
/// reply payloads (ask shape) to the caller's `HewReplyChannel`.
///
/// Loop exits when `recv` returns `Closed` (all external strong handles
/// dropped, mailbox drained). The Arc held by this loop is the last
/// reference; when this function returns, `LambdaActorInner::drop` runs
/// and calls `state_drop`.
fn dispatch_loop(inner: &Arc<LambdaActorInner>) {
    loop {
        let envelope = match inner.recv() {
            Ok(env) => env,
            Err(RecvError::Ok) => {
                // RecvError::Ok is the success discriminant (recv returned a
                // value in the Ok(_) arm above); this arm is unreachable via
                // recv() but must be listed for exhaustiveness.
                continue;
            }
            Err(RecvError::Closed) => break,
            Err(RecvError::Empty) => {
                // `try_recv` semantics surfaced — should not happen on
                // blocking `recv`, but handle defensively.
                continue;
            }
            Err(RecvError::PartitionDetected) => {
                // Partition in v0.5 single-node: treat as stopped.
                inner.mark_stopped();
                break;
            }
        };

        match inner.shape() {
            LambdaShape::Tell => {
                dispatch_tell(inner, &envelope);
            }
            LambdaShape::Ask => {
                dispatch_ask(inner, &envelope);
            }
        }

        if inner.is_stopped() {
            // Drain remaining ask messages, orphaning their reply channels.
            drain_stopped(inner);
            break;
        }
    }
}

/// Invoke the body callback for a tell-shaped message.
fn dispatch_tell(inner: &LambdaActorInner, msg: &[u8]) {
    let body_fn = inner.body_fn;
    let state = inner.state;
    let msg_ptr = if msg.is_empty() {
        ptr::null()
    } else {
        msg.as_ptr()
    };
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut reply_out: *mut u8 = ptr::null_mut();
        let mut reply_len_out: usize = 0;
        // SAFETY: body_fn is a valid extern "C-unwind" fn pointer; msg_ptr valid for
        // msg.len() bytes; reply_out/reply_len_out are local stack variables.
        // SAFETY: body_fn valid; reply_out/reply_len_out are local stack
        // variables; raw ptrs avoid implicit-borrow lint.
        let rc = unsafe {
            body_fn(
                state,
                msg_ptr,
                msg.len(),
                &raw mut reply_out,
                &raw mut reply_len_out,
            )
        };
        // Tell-shape contract: reply_out must be null. If the body sets
        // it (contract violation), free it and mark stopped.
        if !reply_out.is_null() {
            // SAFETY: body_fn allocated this via Box-compatible GlobalAlloc;
            // reclaim it before marking stopped.
            unsafe { free_body_reply_buf(reply_out, reply_len_out) };
            crate::set_last_error(
                "hew_lambda_actor: tell-shape body set reply_out (contract violation); actor stopped"
                    .to_string(),
            );
            return i32::MIN; // force stopped
        }
        rc
    }));
    match result {
        Ok(0) => {} // success
        Ok(_) => {
            // Non-zero return: mark stopped.
            inner.mark_stopped();
        }
        Err(_) => {
            // Panic: mark stopped.
            inner.mark_stopped();
        }
    }
}

/// Invoke the body callback for an ask-shaped message.
fn dispatch_ask(inner: &LambdaActorInner, envelope: &[u8]) {
    if envelope.len() < ASK_REPLY_CH_PREFIX_LEN {
        // Malformed envelope — should never happen from our own framing.
        // Mark stopped and let the reply channel dangle (it will be
        // orphaned when the waiter times out).
        crate::set_last_error(
            "hew_lambda_actor: ask envelope too short; actor stopped".to_string(),
        );
        inner.mark_stopped();
        return;
    }
    // SAFETY: envelope was produced by `frame_ask_envelope`; the
    // reply-channel pointer was valid at send time and the channel is
    // ref-counted so it stays valid until `hew_reply` or
    // `hew_reply_channel_free` releases it.
    let (reply_ch, msg) = unsafe { unframe_ask_envelope(envelope) };

    let body_fn = inner.body_fn;
    let state = inner.state;
    let msg_ptr = if msg.is_empty() {
        ptr::null()
    } else {
        msg.as_ptr()
    };

    // Hoist reply_out / reply_len_out OUTSIDE the catch_unwind closure so
    // that the Err (panic) branch can observe and reclaim any partial reply
    // the body allocated before panicking. Without this, a body that sets
    // *reply_out then panics would leak that allocation because catch_unwind
    // discards the closure locals.
    //
    // Contract for body implementations: bodies that panic forfeit ownership
    // of any reply allocation they wrote to *reply_out — the dispatch layer
    // reclaims it here. Bodies that return non-zero with a non-null *reply_out
    // also forfeit that allocation.
    let mut reply_out: *mut u8 = ptr::null_mut();
    let mut reply_len_out: usize = 0;
    // Take raw pointers before the AssertUnwindSafe closure captures the env.
    // SAFETY: reply_out/len_out are live for the duration of the closure call;
    // the closure returns before they are moved or dropped.
    let reply_out_p = &raw mut reply_out;
    let reply_len_out_p = &raw mut reply_len_out;

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        // SAFETY: body_fn/state/msg_ptr as in dispatch_tell; reply_out/len are
        // outer stack slots passed as out-params via raw pointers.
        unsafe { body_fn(state, msg_ptr, msg.len(), reply_out_p, reply_len_out_p) }
    }));

    // After catch_unwind returns, reply_out / reply_len_out reflect whatever
    // the body wrote (or null/0 if it wrote nothing / panicked early).
    let reply_ptr = reply_out;
    let reply_len = reply_len_out;

    match result {
        Ok(0) => {
            // Success: deliver reply to the waiter.
            // SAFETY: reply_ch is a live HewReplyChannel pointer (see above);
            // reply_ptr/reply_len from body callback — ownership transfers to
            // the reply channel which makes a copy; then we free the body's
            // allocation below.
            let delivered = unsafe { hew_reply(reply_ch, reply_ptr.cast(), reply_len) };
            // Free the body-allocated reply buffer now that hew_reply has
            // (optionally) copied it. If the channel was cancelled (`delivered
            // == false`), the waiter already abandoned this reply and the
            // runtime must reclaim the memory.
            if !reply_ptr.is_null() {
                // SAFETY: body allocated this via Box<[u8]>-compatible GlobalAlloc;
                // hew_reply took a copy, so original buffer is ours to free.
                unsafe { free_body_reply_buf(reply_ptr, reply_len) };
            }
            let _ = delivered;
        }
        Ok(_) => {
            // Non-zero return: actor stopped. Free any partial reply and
            // orphan the reply channel.
            if !reply_ptr.is_null() {
                // SAFETY: body allocated this via Box<[u8]>-compatible GlobalAlloc;
                // body returned non-zero so the channel is orphaned and we reclaim.
                unsafe { free_body_reply_buf(reply_ptr, reply_len) };
            }
            // Signal the waiter with a null reply (orphaned path).
            // SAFETY: reply_ch is alive; orphan_ask_sender_ref takes the
            // ref we're carrying.
            unsafe {
                hew_reply_channel_retire_orphaned_ask_sender_ref(reply_ch);
            }
            inner.mark_stopped();
        }
        Err(_) => {
            // Panic: actor stopped. Reclaim any partial reply the body wrote
            // before panicking (hoisted reply_out captures it), then orphan
            // the reply channel so the waiter unblocks.
            if !reply_ptr.is_null() {
                // SAFETY: body allocated this via Box<[u8]>-compatible GlobalAlloc
                // and then panicked; we are the sole owner.
                unsafe { free_body_reply_buf(reply_ptr, reply_len) };
            }
            // SAFETY: same as non-zero path above.
            unsafe {
                hew_reply_channel_retire_orphaned_ask_sender_ref(reply_ch);
            }
            inner.mark_stopped();
        }
    }
}

/// Drain residual messages from a stopped actor. Ask envelopes are
/// orphaned so waiters unblock; tell envelopes are discarded.
fn drain_stopped(inner: &LambdaActorInner) {
    while let Ok(envelope) = inner.recv() {
        if inner.shape() == LambdaShape::Ask {
            if envelope.len() >= ASK_REPLY_CH_PREFIX_LEN {
                // SAFETY: envelope from frame_ask_envelope; reply_ch pointer valid
                // (still ref-counted by the sending side).
                let (reply_ch, _msg) = unsafe { unframe_ask_envelope(&envelope) };
                // SAFETY: retire_orphaned_ask_sender_ref consumes the ref.
                unsafe {
                    hew_reply_channel_retire_orphaned_ask_sender_ref(reply_ch);
                }
            } else {
                // Malformed ask envelope: too short to hold a reply-channel pointer.
                // The reply-channel sender ref cannot be recovered; log diagnostics
                // for post-mortem. The waiter will see a timeout rather than a
                // deterministic orphan error — this is an unrecoverable corruption.
                let preview: Vec<u8> = envelope
                    .iter()
                    .copied()
                    .take(ASK_REPLY_CH_PREFIX_LEN)
                    .collect();
                crate::set_last_error(format!(
                    "hew_lambda_actor: drain_stopped: malformed ask envelope \
                     (len={}, prefix={preview:?}); reply-channel sender ref could not be recovered",
                    envelope.len(),
                ));
            }
        }
        // Tell envelopes are simply dropped.
    }
}

// ── Double-close guard wrappers ────────────────────────────────────────────
//
// Pattern: a `#[repr(C)]` outer wrapper carries the inner runtime object in
// `ManuallyDrop` plus an `AtomicBool released` flag (first field). The
// release entry (`hew_lambda_actor_release`, `hew_lambda_actor_weak_drop`)
// swaps `released` AcqRel and, on first transition, drops the inner
// object and frees the wrapper.
//
// Caller contract (CALLER UB if violated):
//   - Each wrapper pointer must have AT MOST ONE in-flight FFI call at
//     any time. The release entry consumes the wrapper allocation; any
//     concurrent or subsequent FFI call on the same pointer — including
//     a second release after the first release returned — is caller UB
//     (use-after-free on the wrapper's atomic field read).
//   - Compiler-emitted code is structured to honour this: each lambda-
//     actor handle is owned by exactly one function activation, the
//     per-handle LIFO drop spine schedules release exactly once on the
//     owning thread at scope exit, and no codegen path emits
//     send/ask/clone/downgrade/release after the scheduled release.
//     Strong-to-strong `hew_lambda_actor_clone` allocates a fresh
//     wrapper around the shared `Arc<LambdaActorInner>` — the original
//     wrapper and the clone wrapper each own their own release
//     lifecycle, so two threads holding separate clones can release
//     independently without sharing a wrapper.
//   - The `released` swap guard remains because send/ask/clone/upgrade/
//     downgrade still read the flag — within a single function
//     activation, this catches a tight serial release-then-send bug
//     before any subsequent FFI call escapes outside the activation.
//     After the release call returns, any further use of the pointer is
//     caller UB regardless of the flag check; the guard does not promise
//     post-return safety.
//
// Trade-off rationale: the previous design (allocate via `Box::into_raw`,
// never free) preserved post-release flag-read safety for the cost of a
// per-`hew_lambda_actor_new` 32-byte wrapper leak. For long-running
// processes that spawn many lambda actors, that leak is unacceptable
// (security blocker 2026-06-08 review). The freeing design matches the
// broader Hew C-ABI convention (every `Box::into_raw` allocation has a
// paired `Box::from_raw` release entry) and is sound under the
// single-in-flight contract the compiler already emits.

/// C-ABI wrapper around [`HewLambdaActor`] with a double-release guard.
///
/// `#[repr(C)]` layout: `released` (first field) is read via `addr_of!`
/// by every C-ABI entry to detect post-release access within a single
/// activation. The release entry consumes the wrapper allocation;
/// concurrent or post-return reuse of the same pointer is caller UB.
/// See the design comment above (`Double-close guard wrappers`).
#[repr(C)]
#[derive(Debug)]
pub struct HewLambdaActorHandle {
    released: AtomicBool,
    inner: ManuallyDrop<HewLambdaActor>,
}

impl HewLambdaActorHandle {
    fn new(actor: HewLambdaActor) -> Self {
        Self {
            released: AtomicBool::new(false),
            inner: ManuallyDrop::new(actor),
        }
    }
}

/// C-ABI wrapper around [`HewLambdaActorWeak`] with a double-drop guard.
///
/// Layout mirrors [`HewLambdaActorHandle`] (see its doc).
#[repr(C)]
#[derive(Debug)]
pub struct HewLambdaActorWeakHandle {
    released: AtomicBool,
    inner: ManuallyDrop<HewLambdaActorWeak>,
}

impl HewLambdaActorWeakHandle {
    fn new(weak: HewLambdaActorWeak) -> Self {
        Self {
            released: AtomicBool::new(false),
            inner: ManuallyDrop::new(weak),
        }
    }
}

// ── C-ABI entries ──────────────────────────────────────────────────────────
//
// Consumed by the codegen lowering for the `Place::LambdaActorHandle`
// shape and the `DropKind::LambdaActorRelease` drop-elaboration
// target.

/// Allocate a new lambda-actor instance with the given mailbox capacity,
/// shape discriminant, body callback, per-instance state, and state
/// destructor. The constructor spawns a dispatch thread immediately.
///
/// `shape` must be `0` (Tell) or `1` (Ask); any other value sets a
/// last-error and returns null without invoking `state_drop`.
///
/// On a null return due to invalid arguments, the caller remains
/// responsible for calling `state_drop(state)` if `state` is non-null.
///
/// # Safety
///
/// - `body_fn` must be a valid `extern "C-unwind" fn` pointer for the lifetime
///   of the returned handle.
/// - `state` is passed to `body_fn` on every dispatch; must be valid for
///   the lifetime of the actor. `state_drop(state)` is called once after
///   the dispatch loop stops.
/// - `state_drop` must be a valid `extern "C-unwind" fn` pointer.
/// - The returned pointer is owned by the runtime and must be released
///   with [`hew_lambda_actor_release`].
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_new(
    mailbox_capacity: usize,
    shape: i32,
    body_fn: Option<HewLambdaActorBody>,
    state: *mut core::ffi::c_void,
    state_drop: Option<HewLambdaActorStateDrop>,
) -> *mut HewLambdaActorHandle {
    if mailbox_capacity == 0 {
        crate::set_last_error("hew_lambda_actor_new: mailbox_capacity must be > 0".to_string());
        return ptr::null_mut();
    }
    let Some(body_fn) = body_fn else {
        crate::set_last_error("hew_lambda_actor_new: body_fn is null".to_string());
        return ptr::null_mut();
    };
    let Some(state_drop) = state_drop else {
        crate::set_last_error("hew_lambda_actor_new: state_drop is null".to_string());
        return ptr::null_mut();
    };
    let lambda_shape = match shape {
        0 => LambdaShape::Tell,
        1 => LambdaShape::Ask,
        other => {
            crate::set_last_error(format!(
                "hew_lambda_actor_new: invalid shape discriminant {other}"
            ));
            return ptr::null_mut();
        }
    };
    let Some(actor) =
        HewLambdaActor::new(mailbox_capacity, lambda_shape, body_fn, state, state_drop)
    else {
        crate::set_last_error("hew_lambda_actor_new: could not spawn dispatch thread".to_string());
        return ptr::null_mut();
    };
    let ptr = Box::into_raw(Box::new(HewLambdaActorHandle::new(actor)));
    crate::tracing::record_channel_event(ptr as u64, crate::tracing::SPAN_LAMBDA_SPAWNED);
    ptr
}

/// Refcount-bump clone of a strong lambda-actor handle.
///
/// Returns null if the handle has already been released.
///
/// # Safety
///
/// `actor` must point to a valid, open `HewLambdaActorHandle` returned by
/// [`hew_lambda_actor_new`] or this function.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_clone(
    actor: *mut HewLambdaActorHandle,
) -> *mut HewLambdaActorHandle {
    if actor.is_null() {
        crate::set_last_error("hew_lambda_actor_clone: null handle".to_string());
        return ptr::null_mut();
    }
    // Released-flag guard: cloning a released handle would bump the Arc
    // strong count on a ManuallyDrop-dropped inner — UB. Consult the flag
    // before touching inner. See hew_lambda_actor_send for addr_of! rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*actor).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_lambda_actor_clone: handle already released".to_string());
        return ptr::null_mut();
    }
    // SAFETY:
    // - Provenance: caller guarantees `actor` came from `hew_lambda_actor_new`
    //   or a prior `_clone` call.
    // - Type tag: `*mut HewLambdaActorHandle` matches the originating type.
    // - Lifetime owner: caller still owns the wrapper; we take a shared
    //   borrow of the inner actor only, then heap-allocate the clone.
    // - Aliasing concurrency: `HewLambdaActor` is `Clone`-via-Arc; cloning
    //   an Arc is atomic and Send/Sync-safe.
    // - Bounds: single non-null aligned pointer dereference.
    // - Failure mode: released flag guards against cloning a released handle
    //   (inner ManuallyDrop is invalid after release).
    // ManuallyDrop<HewLambdaActor> derefs to HewLambdaActor; calling
    // clone_handle() on the deref target returns HewLambdaActor (not
    // ManuallyDrop<HewLambdaActor>), which is what HewLambdaActorHandle::new
    // expects. Do NOT use `.clone()` here — ManuallyDrop's own Clone returns
    // ManuallyDrop<T>, not T.
    let cloned = unsafe { (*actor).inner.clone_handle() };
    Box::into_raw(Box::new(HewLambdaActorHandle::new(cloned)))
}

/// Send a message envelope (tell-shaped dispatch). Returns the
/// `SendError` discriminant as `i32`.
///
/// Returns `SendError::DoubleClose` (4) if the handle has already been released.
/// Returns `SendError::ActorStopped` (3) if the body has panicked or
/// returned a non-zero exit code.
///
/// # Safety
///
/// `actor` must be a valid, open `HewLambdaActorHandle` pointer. `msg` must be
/// valid for `len` bytes (may be null when `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_send(
    actor: *mut HewLambdaActorHandle,
    msg: *const u8,
    len: usize,
) -> i32 {
    if actor.is_null() {
        crate::set_last_error("hew_lambda_actor_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    // Released-flag guard: load with Acquire to synchronise with the AcqRel
    // store in hew_lambda_actor_release. Catches sequential release-then-send
    // from native FFI callers. Concurrent release-during-send is still a
    // caller contract violation — this guard does not serialise them.
    // SAFETY: `released` is the first field of #[repr(C)] HewLambdaActorHandle;
    // addr_of! projects without materialising &mut *actor. The outer wrapper is
    // never freed (intentional leak — see wrapper-struct design comment).
    let released = unsafe { &*ptr::addr_of!((*actor).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_lambda_actor_send: handle already released".to_string());
        return SendError::DoubleClose as i32;
    }
    let payload = if len == 0 {
        Vec::new()
    } else if msg.is_null() {
        crate::set_last_error("hew_lambda_actor_send: null msg with non-zero len".to_string());
        return SendError::Closed as i32;
    } else {
        // SAFETY:
        // - Provenance: caller-owned `msg` valid for `len` bytes per
        //   the docstring; we copy into an owned Vec.
        // - Type tag: opaque bytes; no reinterpretation.
        // - Lifetime owner: payload ownership transfers to the Vec
        //   before this frame returns.
        // - Aliasing concurrency: caller must not mutate `msg`
        //   concurrently (standard FFI immutable-borrow contract).
        // - Bounds: `len` bytes from a non-null pointer (null+nonzero
        //   already rejected).
        // - Failure mode: UB on caller contract violation; cannot
        //   detect at this layer.
        unsafe { std::slice::from_raw_parts(msg, len) }.to_vec()
    };
    // SAFETY: see hew_lambda_actor_clone for the dereference axes;
    // identical (caller-supplied valid pointer, shared borrow of inner only,
    // Arc-protected internal state is Sync).
    // `(*actor).inner` = ManuallyDrop<HewLambdaActor> (deref → HewLambdaActor);
    // `.inner` = Arc<LambdaActorInner>; `.mailbox_in` = Arc<HewSendHalf>.
    // SAFETY: (*actor).inner is ManuallyDrop<HewLambdaActor>; take an
    // explicit shared ref to avoid the implicit-autoref lint.
    let actor_ref = unsafe { &(*actor).inner };
    let stopped = actor_ref.inner.is_stopped();
    if stopped {
        return SendError::ActorStopped as i32;
    }
    let res = actor_ref.mailbox_in.send(payload);
    res as i32
}

/// Send a message envelope and await a reply (ask-shaped dispatch).
///
/// The reply payload is written to `*reply_out` / `*reply_len_out` on
/// success; the caller must free it with `hew_reply_payload_free`
/// (NOT `hew_duplex_payload_free` — reply payloads are malloc-allocated
/// while duplex payloads are Box-allocated; mixing them is UB).
/// On failure or actor stopped, `*reply_out` is set to null and
/// `*reply_len_out` to 0.
///
/// Returns `0` (`SendError::Ok`) on success.
/// Returns `SendError::ActorStopped` (3) if the actor was already stopped
/// before the send (pre-send check failed).
/// Returns `SendError::OrphanedAsk` (5) if the ask was sent but the actor's
/// body panicked or returned non-zero before delivering a reply; `*reply_out`
/// is null. Distinct from `ActorStopped` so callers know the message was
/// received but the actor died during handling.
/// Returns `SendError::Closed` (1) if the mailbox was closed.
/// Returns `SendError::DoubleClose` (4) if the handle has already been released.
///
/// # Safety
///
/// `actor` must be a valid, open `HewLambdaActorHandle` pointer.
/// `msg` must be valid for `len` bytes (may be null when `len == 0`).
/// `reply_out` and `reply_len_out` must be valid writable pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_ask(
    actor: *mut HewLambdaActorHandle,
    msg: *const u8,
    len: usize,
    reply_out: *mut *mut u8,
    reply_len_out: *mut usize,
) -> i32 {
    // Zero out-params so callers observe a safe default on any early return.
    if !reply_out.is_null() {
        // SAFETY: caller guarantees reply_out is a valid writable pointer.
        unsafe { ptr::write(reply_out, ptr::null_mut()) };
    }
    if !reply_len_out.is_null() {
        // SAFETY: caller guarantees reply_len_out is a valid writable pointer.
        unsafe { ptr::write(reply_len_out, 0usize) };
    }

    if actor.is_null() {
        crate::set_last_error("hew_lambda_actor_ask: null handle".to_string());
        return SendError::Closed as i32;
    }
    if reply_out.is_null() || reply_len_out.is_null() {
        crate::set_last_error("hew_lambda_actor_ask: null reply_out / reply_len_out".to_string());
        return SendError::Closed as i32;
    }

    // Released-flag guard.
    // SAFETY: addr_of! projection; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*actor).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_lambda_actor_ask: handle already released".to_string());
        return SendError::DoubleClose as i32;
    }

    // Allocate a one-shot reply channel. The channel carries one reference
    // for the waiter (this thread) and one for the dispatch thread (framed
    // in the envelope). `hew_reply` releases the dispatch-side ref;
    // `hew_reply_wait` returns the waiter's payload and the waiter must
    // then call `hew_reply_channel_free` to release the waiter ref.
    let reply_ch = hew_reply_channel_new();
    if reply_ch.is_null() {
        crate::set_last_error("hew_lambda_actor_ask: could not allocate reply channel".to_string());
        return SendError::Closed as i32;
    }
    // Retain one extra reference for the dispatch thread (total = 2: one for
    // the dispatch side framed in the envelope, one for the waiter below).
    // SAFETY: reply_ch is a fresh, live channel.
    unsafe { hew_reply_channel_retain(reply_ch) };

    let msg_bytes = if len == 0 {
        &[][..]
    } else if msg.is_null() {
        crate::set_last_error("hew_lambda_actor_ask: null msg with non-zero len".to_string());
        // Release both refs before returning.
        // SAFETY: two refs allocated above; free both.
        unsafe {
            hew_reply_channel_free(reply_ch);
            hew_reply_channel_free(reply_ch);
        }
        return SendError::Closed as i32;
    } else {
        // SAFETY: caller guarantees msg is valid for len bytes.
        unsafe { std::slice::from_raw_parts(msg, len) }
    };

    let envelope = frame_ask_envelope(reply_ch, msg_bytes);

    // SAFETY: (*actor).inner is ManuallyDrop<HewLambdaActor>; take an
    // explicit shared ref to avoid the implicit-autoref lint.
    let actor_ref = unsafe { &(*actor).inner };
    let stopped = actor_ref.inner.is_stopped();
    if stopped {
        // SAFETY: reply_ch is a live HewReplyChannel pointer; each call
        // releases one ref (one sender ref + one waiter ref allocated above).
        unsafe {
            hew_reply_channel_free(reply_ch);
            hew_reply_channel_free(reply_ch);
        }
        return SendError::ActorStopped as i32;
    }
    let send_res = actor_ref.mailbox_in.send(envelope);
    if send_res != SendError::Ok {
        // Send failed; release both refs.
        // SAFETY: envelope was not consumed (send failed); two refs remain.
        unsafe {
            hew_reply_channel_free(reply_ch);
            hew_reply_channel_free(reply_ch);
        }
        return send_res as i32;
    }

    // Wait for the reply. `hew_reply_wait_with_size` blocks until `hew_reply`
    // (or orphan/cancel) signals the condvar. It returns the payload pointer
    // (caller frees with `hew_reply_payload_free`) or null on orphan.
    // `hew_reply_wait_with_size` does NOT consume the waiter's ref — we must
    // call `hew_reply_channel_free` after.
    let mut out_size: usize = 0;
    // SAFETY: reply_ch is a live channel with one waiter ref; we hold it.
    let payload_ptr = unsafe { hew_reply_wait_with_size(reply_ch, &raw mut out_size) };

    // Check the orphaned flag before releasing the waiter ref.
    // The orphaned store (in hew_reply_channel_retire_orphaned_ask_sender_ref)
    // uses Release ordering; we load with Acquire to pair with it.
    // SAFETY: reply_ch is still live; we hold the waiter ref.
    let is_orphaned = unsafe {
        (*reply_ch)
            .orphaned
            .load(std::sync::atomic::Ordering::Acquire)
    };

    // Release the waiter ref.
    // SAFETY: reply_ch is still live; this is the last (waiter) ref.
    unsafe { hew_reply_channel_free(reply_ch) };

    if is_orphaned {
        // Body panicked or returned non-zero — reply is null, actor stopped.
        // Return OrphanedAsk (5) rather than Ok so callers can distinguish
        // "died during our ask" from "not stopped" or "stopped before send".
        return SendError::OrphanedAsk as i32;
    }

    // Write results to caller's out-params.
    // SAFETY: reply_out / reply_len_out are non-null (checked above).
    unsafe {
        ptr::write(reply_out, payload_ptr.cast());
        ptr::write(reply_len_out, out_size);
    }

    SendError::Ok as i32
}

/// Release a strong lambda-actor handle. If this is the last strong
/// handle, the inner state (including the embedded `HewDuplex`) is
/// freed and body-side weak captures will fail their next upgrade.
///
/// Returns `SendError::Ok` (0) on the first call.
///
/// The wrapper allocation is consumed by this call so long-running
/// processes do not accumulate per-actor wrappers. Any subsequent FFI
/// call on the same pointer — including a second release call after
/// this call returned, OR a concurrent release on a separate thread
/// — is caller UB (use-after-free on the wrapper's atomic field
/// read). The compiler's LIFO drop spine schedules release exactly
/// once on the owning thread.
///
/// # Safety
///
/// `actor` must have been returned by [`hew_lambda_actor_new`] or
/// [`hew_lambda_actor_clone`] AND must have at most one in-flight
/// FFI call on this pointer (this release call). Concurrent or
/// post-return reuse of the same pointer is caller UB.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_release(actor: *mut HewLambdaActorHandle) -> i32 {
    if actor.is_null() {
        return SendError::Ok as i32;
    }
    // SAFETY: (flag-read phase)
    // - Provenance: caller guarantees `actor` came from
    //   `Box::into_raw(Box::new(HewLambdaActorHandle::new(...)))` AND
    //   no prior release call on this pointer has already RETURNED
    //   (per the Safety contract; post-return reuse is UB).
    // - Type tag: `released` is the first field of `#[repr(C)]`
    //   HewLambdaActorHandle. `addr_of!((*actor).released)` projects
    //   the AtomicBool's address without materialising a `&mut
    //   HewLambdaActorHandle` (no aliasing UB even though no other
    //   in-flight caller is permitted).
    // - The swap is technically still useful as a tight-window guard
    //   against same-thread release-after-release within a single
    //   activation: returning `DoubleClose` (rather than re-entering
    //   the drop) prevents a UB ManuallyDrop::drop on already-dropped
    //   inner. The wrapper-free path still runs unconditionally
    //   below — see free-phase comment.
    let released = unsafe { &*ptr::addr_of!((*actor).released) };
    let was_released = released.swap(true, Ordering::AcqRel);
    if was_released {
        // Tight-window guard: a second release within a single window
        // hit the flag before this caller; the wrapper is still alive
        // (post-return reuse is UB so we trust that here) — return the
        // typed status. NOTE: we deliberately do NOT free the wrapper
        // here because a peer is concurrently responsible for it; but
        // per the contract there is no peer, so this branch only fires
        // on a tight serial bug. The free below would then race with
        // whichever path runs second — both branches free, so we get
        // a double-free instead of a leak. For now we accept that
        // tight-serial-bug double-release surfaces as a double-free in
        // tests, which is louder than a silent leak (the compiler's
        // drop-spine prevents this from ever firing in practice).
        return SendError::DoubleClose as i32;
    }
    // SAFETY: (drop phase) swap won; this caller has exclusive access to
    // the inner runtime object. Per the Safety contract above, no other
    // FFI caller is concurrently inside this function on this pointer.
    let h = unsafe { &mut *actor };
    // SAFETY: first release — inner HewLambdaActor is still valid.
    // ManuallyDrop::drop decrements the Arc strong count (and triggers
    // LambdaActorInner Drop if it reaches zero, which closes the
    // HewDuplex and joins the dispatch thread via the dispatch-loop's
    // observable end-of-state path).
    unsafe { ManuallyDrop::drop(&mut h.inner) };
    crate::tracing::record_channel_event(actor as u64, crate::tracing::SPAN_LAMBDA_RELEASED);
    // SAFETY: (wrapper-free phase) actor was allocated by `Box::into_raw`
    // in `hew_lambda_actor_new` / `_clone`. Per the Safety contract no
    // other FFI caller may concurrently touch this pointer. Reconstruct
    // the Box and drop it to free the wrapper. Any subsequent same-
    // pointer FFI call is caller UB.
    unsafe { drop(Box::from_raw(actor)) };
    SendError::Ok as i32
}

/// Downgrade a strong handle to a weak one. The returned pointer is
/// owned by the caller and must be released with
/// [`hew_lambda_actor_weak_drop`].
///
/// Returns null if the handle has already been released.
///
/// # Safety
///
/// `actor` must be a valid, open `HewLambdaActorHandle` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_downgrade(
    actor: *mut HewLambdaActorHandle,
) -> *mut HewLambdaActorWeakHandle {
    if actor.is_null() {
        crate::set_last_error("hew_lambda_actor_downgrade: null handle".to_string());
        return ptr::null_mut();
    }
    // Released-flag guard: downgrading a released handle would call downgrade
    // on a ManuallyDrop-dropped HewLambdaActor — UB. See
    // hew_lambda_actor_send for addr_of! rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*actor).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_lambda_actor_downgrade: handle already released".to_string());
        return ptr::null_mut();
    }
    // SAFETY: see hew_lambda_actor_clone — identical six-axis profile.
    let weak = unsafe { (*actor).inner.downgrade() };
    Box::into_raw(Box::new(HewLambdaActorWeakHandle::new(weak)))
}

/// Attempt a weak self-send. Upgrades the weak handle just long enough
/// to dispatch; if the upgrade fails (external strong refcount is zero)
/// returns `SendError::ActorStopped` discriminant instead of
/// resurrecting the actor — §5.9 ratification 2 enforcement.
///
/// Returns `SendError::DoubleClose` (4) if the weak handle has already been dropped.
///
/// # Safety
///
/// `weak` must be a valid, open `HewLambdaActorWeakHandle` pointer. `msg` must be
/// valid for `len` bytes (may be null when `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_weak_send(
    weak: *mut HewLambdaActorWeakHandle,
    msg: *const u8,
    len: usize,
) -> i32 {
    if weak.is_null() {
        crate::set_last_error("hew_lambda_actor_weak_send: null weak handle".to_string());
        return SendError::Closed as i32;
    }
    // Released-flag guard: see hew_lambda_actor_send for rationale; mirrored
    // here for the weak-handle wrapper.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*weak).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_lambda_actor_weak_send: handle already released".to_string());
        return SendError::DoubleClose as i32;
    }
    let payload = if len == 0 {
        Vec::new()
    } else if msg.is_null() {
        crate::set_last_error("hew_lambda_actor_weak_send: null msg with non-zero len".to_string());
        return SendError::Closed as i32;
    } else {
        // SAFETY: see hew_lambda_actor_send `msg` axis — identical
        // (caller-owned bytes, copied into owned Vec before return).
        unsafe { std::slice::from_raw_parts(msg, len) }.to_vec()
    };
    // SAFETY:
    // - Provenance: caller guarantees `weak` came from
    //   `hew_lambda_actor_downgrade` or `_weak_clone`.
    // - Type tag: `*mut HewLambdaActorWeakHandle` matches the originating type.
    // - Lifetime owner: caller still owns the wrapper; shared borrow of inner only.
    // - Aliasing concurrency: `Weak::upgrade` is atomic-Sync.
    // - Bounds: single non-null aligned pointer dereference.
    // - Failure mode: dangling pointer is the caller's contract
    //   violation; undetectable here.
    let res = unsafe { (*weak).inner.send(payload) };
    res as i32
}

/// Refcount-bump clone of a weak handle.
///
/// Returns null if the weak handle has already been dropped.
///
/// # Safety
///
/// `weak` must be a valid, open `HewLambdaActorWeakHandle` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_weak_clone(
    weak: *mut HewLambdaActorWeakHandle,
) -> *mut HewLambdaActorWeakHandle {
    if weak.is_null() {
        crate::set_last_error("hew_lambda_actor_weak_clone: null weak handle".to_string());
        return ptr::null_mut();
    }
    // Released-flag guard: cloning a dropped weak handle would call clone
    // on a ManuallyDrop-dropped HewLambdaActorWeak — UB. See
    // hew_lambda_actor_send for addr_of! rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*weak).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_lambda_actor_weak_clone: handle already released".to_string());
        return ptr::null_mut();
    }
    // SAFETY: see hew_lambda_actor_weak_send for dereference axes.
    // Use clone_handle() not clone() — ManuallyDrop's own Clone returns
    // ManuallyDrop<T>; clone_handle() returns HewLambdaActorWeak via Deref.
    let cloned = unsafe { (*weak).inner.clone_handle() };
    Box::into_raw(Box::new(HewLambdaActorWeakHandle::new(cloned)))
}

/// Release a weak handle. Does NOT affect the actor's external strong
/// refcount; weak handles only contribute to the weak count.
///
/// Returns `SendError::Ok` (0) on the first call.
///
/// The wrapper allocation is consumed by this call (see
/// `hew_lambda_actor_release` for the caller-UB contract). Same-pointer
/// post-return or concurrent FFI use is caller UB.
///
/// # Safety
///
/// `weak` must have been returned by [`hew_lambda_actor_downgrade`] or
/// [`hew_lambda_actor_weak_clone`] AND must have at most one in-flight
/// FFI call on this pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_weak_drop(weak: *mut HewLambdaActorWeakHandle) -> i32 {
    if weak.is_null() {
        return SendError::Ok as i32;
    }
    // SAFETY: see `hew_lambda_actor_release` — `released` is projected
    // via `addr_of!` so no `&mut` is materialised before the swap.
    let released = unsafe { &*ptr::addr_of!((*weak).released) };
    let was_released = released.swap(true, Ordering::AcqRel);
    if was_released {
        return SendError::DoubleClose as i32;
    }
    // SAFETY: swap winner — exclusive against any well-formed caller.
    let h = unsafe { &mut *weak };
    // SAFETY: first drop — inner HewLambdaActorWeak is still valid.
    // ManuallyDrop::drop decrements the Arc weak count.
    unsafe { ManuallyDrop::drop(&mut h.inner) };
    // SAFETY: weak was allocated by `Box::into_raw` in
    // `hew_lambda_actor_downgrade` / `hew_lambda_actor_weak_clone`.
    // Per the Safety contract no other FFI caller may concurrently
    // touch this pointer.
    unsafe { drop(Box::from_raw(weak)) };
    SendError::Ok as i32
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
#[allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Test module: every unsafe block calls the C-ABI we own; \
              invariants are documented by test names and helper fn doc-comments."
)]
mod tests {
    use std::sync::atomic::Ordering as Ord;

    use super::*;

    // ── Shared test helpers ────────────────────────────────────────────────

    /// No-op state-drop callback for tests that don't need cleanup.
    unsafe extern "C-unwind" fn noop_state_drop(_state: *mut core::ffi::c_void) {}

    /// Body for `tell_shape_dispatch_roundtrip`: appends received bytes to an
    /// `Arc<Mutex<Vec<Vec<u8>>>>` passed via state.
    unsafe extern "C-unwind" fn arc_mutex_body(
        state: *mut core::ffi::c_void,
        msg: *const u8,
        msg_len: usize,
        reply_out: *mut *mut u8,
        reply_len_out: *mut usize,
    ) -> i32 {
        use std::sync::Mutex;
        // SAFETY: state is `*mut Arc<Mutex<Vec<Vec<u8>>>>` allocated by the
        // test via Box::into_raw; valid for the actor lifetime.
        let arc = unsafe { &*(state as *const Arc<Mutex<Vec<Vec<u8>>>>) };
        let bytes = if msg_len == 0 {
            Vec::new()
        } else {
            // SAFETY: msg valid for msg_len bytes per body-fn contract.
            unsafe { std::slice::from_raw_parts(msg, msg_len) }.to_vec()
        };
        arc.lock().unwrap().push(bytes);
        // SAFETY: reply_out / reply_len_out are out-params supplied by the
        // dispatch layer; non-null per body-fn contract.
        unsafe {
            *reply_out = ptr::null_mut();
            *reply_len_out = 0;
        }
        0
    }

    /// State-drop for `tell_shape_dispatch_roundtrip`: frees the Box wrapping
    /// the `Arc<Mutex<...>>`.
    unsafe extern "C-unwind" fn arc_mutex_state_drop(state: *mut core::ffi::c_void) {
        use std::sync::Mutex;
        // SAFETY: state is a Box<Arc<Mutex<Vec<Vec<u8>>>>> pointer from
        // Box::into_raw; freed exactly once here.
        unsafe { drop(Box::from_raw(state.cast::<Arc<Mutex<Vec<Vec<u8>>>>>())) };
    }

    /// Body for `tell_body_receives_message_bytes`: same collect-to-Arc pattern
    /// but used via the C-ABI entry point rather than the Rust API.
    unsafe extern "C-unwind" fn collect_body(
        state: *mut core::ffi::c_void,
        msg: *const u8,
        msg_len: usize,
        reply_out: *mut *mut u8,
        reply_len_out: *mut usize,
    ) -> i32 {
        use std::sync::Mutex;
        // SAFETY: state is `*mut Arc<Mutex<Vec<Vec<u8>>>>` from Box::into_raw.
        let arc = unsafe { &*(state as *const Arc<Mutex<Vec<Vec<u8>>>>) };
        // SAFETY: msg valid for msg_len bytes; msg_len > 0 guaranteed by caller.
        let bytes = unsafe { std::slice::from_raw_parts(msg, msg_len) }.to_vec();
        arc.lock().unwrap().push(bytes);
        // SAFETY: reply_out/reply_len_out are dispatch-layer out-params.
        unsafe {
            *reply_out = ptr::null_mut();
            *reply_len_out = 0;
        }
        0
    }

    /// State-drop for `tell_body_receives_message_bytes`.
    unsafe extern "C-unwind" fn collect_state_drop(state: *mut core::ffi::c_void) {
        use std::sync::Mutex;
        // SAFETY: same Box<Arc<Mutex<...>>> provenance as arc_mutex_state_drop.
        unsafe { drop(Box::from_raw(state.cast::<Arc<Mutex<Vec<Vec<u8>>>>>())) };
    }

    /// Body callback that doubles an i64 message and writes it as reply bytes.
    ///
    /// `state` is ignored (null).
    unsafe extern "C-unwind" fn double_i64_body(
        _state: *mut core::ffi::c_void,
        msg: *const u8,
        msg_len: usize,
        reply_out: *mut *mut u8,
        reply_len_out: *mut usize,
    ) -> i32 {
        assert_eq!(msg_len, 8, "double_i64_body expects 8-byte i64 payload");
        let n = unsafe {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(std::slice::from_raw_parts(msg, 8));
            i64::from_le_bytes(buf)
        };
        let doubled = n * 2;
        let reply_bytes = doubled.to_le_bytes().to_vec().into_boxed_slice();
        let reply_len = reply_bytes.len();
        let reply_ptr = Box::into_raw(reply_bytes).cast::<u8>();
        unsafe {
            *reply_out = reply_ptr;
            *reply_len_out = reply_len;
        }
        0
    }

    /// Body callback that panics. Used to test panic-isolation.
    unsafe extern "C-unwind" fn panicking_body(
        _state: *mut core::ffi::c_void,
        _msg: *const u8,
        _msg_len: usize,
        reply_out: *mut *mut u8,
        reply_len_out: *mut usize,
    ) -> i32 {
        unsafe {
            *reply_out = ptr::null_mut();
            *reply_len_out = 0;
        }
        panic!("intentional test panic from body callback");
    }

    /// Body callback that returns a non-zero exit code.
    unsafe extern "C-unwind" fn failing_body(
        _state: *mut core::ffi::c_void,
        _msg: *const u8,
        _msg_len: usize,
        reply_out: *mut *mut u8,
        reply_len_out: *mut usize,
    ) -> i32 {
        unsafe {
            *reply_out = ptr::null_mut();
            *reply_len_out = 0;
        }
        42 // non-zero → actor stops
    }

    /// State-drop callback that increments a shared counter. Cast
    /// `Arc<AtomicUsize>` pointer to `*mut c_void` via `Arc::into_raw`.
    unsafe extern "C-unwind" fn counting_state_drop(state: *mut core::ffi::c_void) {
        // SAFETY: state is `*const AtomicUsize` from Arc::into_raw; we
        // reconstruct the Arc and drop it (decrement refcount).
        let arc = unsafe { Arc::from_raw(state as *const AtomicUsize) };
        arc.fetch_add(1, Ord::SeqCst);
        // Arc drops here, decrementing refcount.
    }

    // ── Existing Rust-API tests (updated for new constructor) ──────────────

    /// Tell-shape send → recv round-trip at the Rust API level.
    ///
    /// Uses a Mutex<Vec<u8>> as state to collect received messages;
    /// reads them back after a brief wait. This test doesn't interact
    /// with the C-ABI entry but verifies the Rust dispatch path.
    #[test]
    fn tell_shape_dispatch_roundtrip() {
        use std::sync::{Arc, Mutex};
        use std::time::Duration;

        let received: Arc<Mutex<Vec<Vec<u8>>>> = Arc::new(Mutex::new(Vec::new()));
        let received_clone = Arc::clone(&received);

        // Box the Arc<Mutex<...>> and leak it as a state pointer.
        let state_ptr = Box::into_raw(Box::new(received_clone)).cast::<core::ffi::c_void>();

        let actor = HewLambdaActor::new(
            4,
            LambdaShape::Tell,
            arc_mutex_body,
            state_ptr,
            arc_mutex_state_drop,
        )
        .expect("spawn dispatch thread");

        assert_eq!(actor.send(b"hello".to_vec()), SendError::Ok);
        assert_eq!(actor.send(b"world".to_vec()), SendError::Ok);
        drop(actor);

        // Give the dispatch thread time to process.
        std::thread::sleep(Duration::from_millis(50));

        let guard = received.lock().unwrap();
        assert_eq!(guard.len(), 2);
        assert_eq!(guard[0], b"hello");
        assert_eq!(guard[1], b"world");
    }

    #[test]
    fn weak_upgrade_succeeds_while_strong_alive() {
        let a = HewLambdaActor::new(
            2,
            LambdaShape::Tell,
            noop_tell_body,
            ptr::null_mut(),
            noop_state_drop,
        )
        .expect("spawn");
        let w = a.downgrade();
        let upgraded = w.upgrade().expect("upgrade while alive");
        assert_eq!(upgraded.send(b"via-weak".to_vec()), SendError::Ok);
        drop(upgraded);
        drop(a);
    }

    unsafe extern "C-unwind" fn noop_tell_body(
        _state: *mut core::ffi::c_void,
        _msg: *const u8,
        _msg_len: usize,
        reply_out: *mut *mut u8,
        reply_len_out: *mut usize,
    ) -> i32 {
        unsafe {
            *reply_out = ptr::null_mut();
            *reply_len_out = 0;
        }
        0
    }

    #[test]
    fn weak_upgrade_fails_after_strong_drops() {
        let a = HewLambdaActor::new(
            2,
            LambdaShape::Tell,
            noop_tell_body,
            ptr::null_mut(),
            noop_state_drop,
        )
        .expect("spawn");
        let w = a.downgrade();
        drop(a);
        // Give dispatch thread a moment to drain.
        std::thread::sleep(std::time::Duration::from_millis(20));
        assert!(w.upgrade().is_none());
        assert_eq!(w.send(b"stopped".to_vec()), SendError::ActorStopped);
    }

    #[test]
    fn weak_does_not_keep_actor_alive() {
        let a = HewLambdaActor::new(
            2,
            LambdaShape::Tell,
            noop_tell_body,
            ptr::null_mut(),
            noop_state_drop,
        )
        .expect("spawn");
        let w = a.downgrade();
        // `inner` Arc: external handle + dispatch thread = 2 strong refs.
        // `mailbox_in` Arc: only the external handle = 1 strong ref.
        // (The dispatch thread does NOT hold a mailbox_in reference —
        // that's the key invariant ensuring the queue closes when the
        // external handle drops.)
        assert_eq!(Arc::strong_count(&a.mailbox_in), 1);
        assert_eq!(Arc::strong_count(&a.inner), 2);
        drop(a);
        // When `a` drops: Arc<HewSendHalf> strong count → 0 → queue closes
        // → dispatch thread's recv returns Closed → thread exits.
        std::thread::sleep(std::time::Duration::from_millis(50));
        assert!(w.upgrade().is_none());
    }

    #[test]
    fn multiple_strong_handles_keep_actor_alive() {
        let a = HewLambdaActor::new(
            2,
            LambdaShape::Tell,
            noop_tell_body,
            ptr::null_mut(),
            noop_state_drop,
        )
        .expect("spawn");
        let b = a.clone_handle();
        let w = a.downgrade();
        drop(a);
        let up = w.upgrade().expect("upgrade while sibling strong alive");
        assert_eq!(up.send(b"x".to_vec()), SendError::Ok);
        drop(up);
        drop(b);
        std::thread::sleep(std::time::Duration::from_millis(50));
        assert!(w.upgrade().is_none());
    }

    // ── C-ABI tests ────────────────────────────────────────────────────────

    /// Helper: construct a noop-tell actor via C-ABI.
    fn new_tell_cabi(capacity: usize) -> *mut HewLambdaActorHandle {
        // SAFETY: all args are valid; noop_tell_body / noop_state_drop are
        // valid extern "C-unwind" function pointers.
        unsafe {
            hew_lambda_actor_new(
                capacity,
                LambdaShape::Tell as i32,
                Some(noop_tell_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        }
    }

    #[test]
    fn cabi_new_send_release_tell() {
        let a = new_tell_cabi(4);
        assert!(!a.is_null());
        let msg = b"abi";
        // SAFETY: a non-null per assertion; msg valid for its length.
        unsafe {
            let rc = hew_lambda_actor_send(a, msg.as_ptr(), msg.len());
            assert_eq!(rc, SendError::Ok as i32);
            assert_eq!(hew_lambda_actor_release(a), SendError::Ok as i32);
        }
    }

    #[test]
    fn cabi_new_rejects_invalid_shape() {
        // SAFETY: args valid except shape discriminant.
        let a = unsafe {
            hew_lambda_actor_new(
                4,
                42,
                Some(noop_tell_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(a.is_null());
    }

    #[test]
    fn cabi_new_rejects_zero_capacity() {
        // SAFETY: args valid except capacity.
        let a = unsafe {
            hew_lambda_actor_new(
                0,
                LambdaShape::Tell as i32,
                Some(noop_tell_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(a.is_null());
    }

    #[test]
    fn cabi_new_rejects_null_body_fn() {
        // SAFETY: body_fn is None (null).
        let a = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Tell as i32,
                None,
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(a.is_null());
    }

    #[test]
    fn cabi_weak_send_returns_actor_stopped_after_release() {
        let a = new_tell_cabi(4);
        // SAFETY: a non-null.
        let w = unsafe { hew_lambda_actor_downgrade(a) };
        assert!(!w.is_null());
        // SAFETY: a is a live strong handle; release it.
        unsafe { assert_eq!(hew_lambda_actor_release(a), SendError::Ok as i32) };
        // After release the weak's upgrade fails; send surfaces ActorStopped.
        let payload = b"recurse";
        // Wait for dispatch thread to drain.
        std::thread::sleep(std::time::Duration::from_millis(50));
        // SAFETY: w is still a valid weak-handle wrapper; payload valid.
        let rc = unsafe { hew_lambda_actor_weak_send(w, payload.as_ptr(), payload.len()) };
        assert_eq!(rc, SendError::ActorStopped as i32);
        // SAFETY: drop the weak.
        unsafe { assert_eq!(hew_lambda_actor_weak_drop(w), SendError::Ok as i32) };
    }

    #[test]
    fn cabi_weak_send_succeeds_while_strong_alive() {
        let a = new_tell_cabi(4);
        // SAFETY: a non-null.
        let w = unsafe { hew_lambda_actor_downgrade(a) };
        let payload = b"alive";
        // SAFETY: w and payload valid.
        let rc = unsafe { hew_lambda_actor_weak_send(w, payload.as_ptr(), payload.len()) };
        assert_eq!(rc, SendError::Ok as i32);
        // SAFETY: clean up both handles.
        unsafe {
            assert_eq!(hew_lambda_actor_weak_drop(w), SendError::Ok as i32);
            assert_eq!(hew_lambda_actor_release(a), SendError::Ok as i32);
        }
    }

    // ── Concurrent release stress (DELETED) ────────────────────────────────
    //
    // The prior `cabi_concurrent_release_lambda_actor_returns_single_winner`
    // and double-release / send-after-release / weak-send-after-drop tests
    // exercised post-release wrapper survival, which the legacy "leak
    // the wrapper" design supported. The current design consumes the
    // wrapper allocation on release (security blocker fix 2026-06-08)
    // and documents concurrent-or-post-return same-pointer FFI use as
    // caller UB. The compiler-emitted drop spine guarantees the
    // single-in-flight contract; verifying that property at this layer
    // would require a global EBR/quarantine that is out of scope for
    // this slice. The atomic `released` flag remains as a tight-window
    // guard within a single function activation.

    // ── Body dispatch tests ────────────────────────────────────────────────

    /// Tell-shape body dispatch: body receives correct message bytes.
    #[test]
    fn tell_body_receives_message_bytes() {
        use std::sync::{Arc, Mutex};
        use std::time::Duration;

        let received: Arc<Mutex<Vec<Vec<u8>>>> = Arc::new(Mutex::new(Vec::new()));
        let received_clone = Arc::clone(&received);
        let state_ptr = Box::into_raw(Box::new(received_clone)).cast::<core::ffi::c_void>();

        // SAFETY: hew_lambda_actor_new contract; state_ptr valid; body
        // collect_body and collect_state_drop defined at module level.
        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Tell as i32,
                Some(collect_body),
                state_ptr,
                Some(collect_state_drop),
            )
        };
        assert!(!actor.is_null());
        let msg = b"dispatch-test";
        unsafe { hew_lambda_actor_send(actor, msg.as_ptr(), msg.len()) };
        unsafe { hew_lambda_actor_release(actor) };

        std::thread::sleep(Duration::from_millis(100));
        let guard = received.lock().unwrap();
        assert_eq!(guard.len(), 1);
        assert_eq!(guard[0], b"dispatch-test");
    }

    /// Ask-shape body dispatch: body writes reply, caller observes it.
    #[test]
    fn ask_body_reply_roundtrip() {
        use std::time::Duration;

        // Serialise with `ask_reply_alloc_failure_returns_ok_with_null_reply`
        // — that test toggles the process-wide `FORCE_REPLY_ALLOC_FAILURE`
        // flag, which `hew_reply` consumes lazily. Without this guard, our
        // ask can land first and pick up its forced-failure, which would
        // surface as a spurious null-reply assertion miss here.
        let _guard = crate::runtime_test_guard();

        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Ask as i32,
                Some(double_i64_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(!actor.is_null());

        let n: i64 = 21;
        let msg_bytes = n.to_le_bytes();
        let mut reply_ptr: *mut u8 = ptr::null_mut();
        let mut reply_len: usize = 0;
        let rc = unsafe {
            hew_lambda_actor_ask(
                actor,
                msg_bytes.as_ptr(),
                msg_bytes.len(),
                &raw mut reply_ptr,
                &raw mut reply_len,
            )
        };
        assert_eq!(rc, SendError::Ok as i32, "ask should succeed");
        assert!(!reply_ptr.is_null(), "reply payload must be non-null");
        assert_eq!(reply_len, 8, "i64 reply is 8 bytes");
        let result = unsafe {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(std::slice::from_raw_parts(reply_ptr, reply_len));
            i64::from_le_bytes(buf)
        };
        assert_eq!(result, 42, "double(21) == 42");

        // Free reply payload via hew_reply_payload_free (libc::free) — NOT
        // hew_duplex_payload_free which uses Rust GlobalAlloc. Reply payloads
        // are malloc-allocated inside the reply channel.
        unsafe { crate::reply_channel::hew_reply_payload_free(reply_ptr, reply_len) };
        unsafe { hew_lambda_actor_release(actor) };
        std::thread::sleep(Duration::from_millis(50));
    }

    /// Body panic marks actor stopped; subsequent send returns `ActorStopped`.
    ///
    /// Containment is cross-platform. The body callback is `extern "C-unwind"`,
    /// so a panic raised inside it unwinds across the FFI boundary into the
    /// dispatch loop's `std::panic::catch_unwind`, which marks the actor
    /// stopped on every target. This holds on Windows/MSVC as well: the
    /// `C-unwind` ABI carries the unwind through SEH, so the outer
    /// `catch_unwind` intercepts it exactly as it does on the Itanium ABI
    /// targets. Verified on `x86_64-pc-windows-msvc`.
    #[test]
    fn body_panic_marks_actor_stopped() {
        use std::time::Duration;

        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Tell as i32,
                Some(panicking_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(!actor.is_null());
        let msg = b"trigger-panic";
        unsafe { hew_lambda_actor_send(actor, msg.as_ptr(), msg.len()) };
        // Give the dispatch thread time to catch the panic.
        std::thread::sleep(Duration::from_millis(100));
        // Subsequent send must return ActorStopped.
        let rc = unsafe { hew_lambda_actor_send(actor, msg.as_ptr(), msg.len()) };
        assert_eq!(rc, SendError::ActorStopped as i32, "panic must stop actor");
        unsafe { hew_lambda_actor_release(actor) };
    }

    /// Non-zero body return marks actor stopped.
    #[test]
    fn nonzero_body_return_marks_actor_stopped() {
        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Tell as i32,
                Some(failing_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(!actor.is_null());
        let msg = b"trigger-fail";
        unsafe { hew_lambda_actor_send(actor, msg.as_ptr(), msg.len()) };
        std::thread::sleep(std::time::Duration::from_millis(100));
        let rc = unsafe { hew_lambda_actor_send(actor, msg.as_ptr(), msg.len()) };
        assert_eq!(
            rc,
            SendError::ActorStopped as i32,
            "non-zero return must stop actor"
        );
        unsafe { hew_lambda_actor_release(actor) };
    }

    /// Ask-shape: body panic orphans the reply channel; ask returns `OrphanedAsk`.
    #[test]
    fn ask_body_panic_orphans_reply_channel() {
        use std::time::Duration;

        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Ask as i32,
                Some(panicking_body),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(!actor.is_null());

        let msg = b"trigger-ask-panic";
        let mut reply_ptr: *mut u8 = ptr::null_mut();
        let mut reply_len: usize = 0;
        // The ask must unblock (not deadlock) even though the body panics.
        let rc = unsafe {
            hew_lambda_actor_ask(
                actor,
                msg.as_ptr(),
                msg.len(),
                &raw mut reply_ptr,
                &raw mut reply_len,
            )
        };
        // Ask unblocked and returns OrphanedAsk (5), NOT Ok — the actor's body
        // panicked after receiving the message, so the reply was orphaned.
        // This distinguishes "died during handling" from a pre-send stop check.
        assert_eq!(
            rc,
            SendError::OrphanedAsk as i32,
            "body-panic ask must return OrphanedAsk; got rc={rc}"
        );
        // Orphaned ask delivers null reply; payload pointer must be null.
        assert!(reply_ptr.is_null(), "orphaned ask must deliver null reply");
        unsafe { hew_lambda_actor_release(actor) };
        std::thread::sleep(Duration::from_millis(50));
    }

    /// Regression test for B5 (security review 2026-06-08, final):
    /// `hew_lambda_actor_ask` MUST surface a status=Ok / `reply_ptr=null`
    /// outcome whenever the runtime's reply-buffer copy fails
    /// (`alloc_reply_buffer` returned null inside `hew_reply` →
    /// `publish_reply_from_sender_ref` with null/0). Pre-codegen-guard,
    /// the LLVM-emitted ask call-site unconditionally loaded the i64
    /// reply value from `*reply_out` on status=Ok — a null deref. The
    /// codegen fix (llvm.rs `hew_lambda_actor_ask` Ok branch) now
    /// guards with `reply_ptr != null && reply_len >= 8` and falls
    /// through to a `Result::Err(AskError::PayloadSizeMismatch)`
    /// branch on failure.
    ///
    /// This test pins the runtime side of the contract: when the
    /// next reply-buffer alloc is forced to fail, the dispatch thread
    /// runs the body to completion, publishes a null reply, and
    /// `hew_lambda_actor_ask` returns `SendError::Ok` with
    /// `reply_ptr == null` / `reply_len == 0`. The codegen guard
    /// then catches that and synthesises the Err.
    #[test]
    fn ask_reply_alloc_failure_returns_ok_with_null_reply() {
        use std::time::Duration;

        // ask-shape body that writes an i64 reply via the standard
        // body allocator. Mirrors `ask_body_reply_roundtrip`'s body.
        // Defined before the test-lock guard to satisfy
        // `clippy::items_after_statements` (the item is hoisted to the
        // top of the scope regardless, so ordering it textually first
        // avoids a pedantic-lint suppression).
        unsafe extern "C-unwind" fn ask_body_publishes_i64(
            _state: *mut core::ffi::c_void,
            _msg: *const u8,
            _msg_len: usize,
            reply_out: *mut *mut u8,
            reply_len_out: *mut usize,
        ) -> i32 {
            const WIDTH: usize = std::mem::size_of::<i64>();
            // SAFETY: GlobalAlloc-allocated Box, freed by the dispatch
            // layer via `free_body_reply_buf` after `hew_reply` copies
            // (or on the not-delivered path when copy fails).
            let buf: Box<[u8]> = vec![0u8; WIDTH].into_boxed_slice();
            let val: i64 = 99;
            let raw = Box::into_raw(buf).cast::<u8>();
            unsafe {
                core::ptr::copy_nonoverlapping((&raw const val).cast::<u8>(), raw, WIDTH);
                *reply_out = raw;
                *reply_len_out = WIDTH;
            }
            0
        }

        // Serialise with other tests that toggle FORCE_REPLY_ALLOC_FAILURE
        // (see `reply_channel::tests::hew_reply_reports_buffer_alloc_failure`)
        // and with parallel ask-body tests that observe alloc behaviour.
        // The forced-failure flag is a process-wide static; without this
        // guard our toggle can be consumed by an unrelated
        // `ask_body_reply_roundtrip` reply, manifesting as flaky failures.
        let _guard = crate::runtime_test_guard();

        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Ask as i32,
                Some(ask_body_publishes_i64),
                ptr::null_mut(),
                Some(noop_state_drop),
            )
        };
        assert!(!actor.is_null());

        // Force the NEXT `alloc_reply_buffer` call (inside `hew_reply`)
        // to return null, simulating reply-buffer OOM.
        crate::reply_channel::force_reply_alloc_failure_for_test();

        let msg = b"trigger-oom";
        let mut reply_ptr: *mut u8 = ptr::null_mut();
        let mut reply_len: usize = 0;
        let rc = unsafe {
            hew_lambda_actor_ask(
                actor,
                msg.as_ptr(),
                msg.len(),
                &raw mut reply_ptr,
                &raw mut reply_len,
            )
        };

        // Runtime contract: body ran successfully, but reply-buffer
        // alloc failed. The ask returns Ok (the SEND succeeded; the
        // body did not panic) with null reply payload. The codegen
        // ask-decode guard then materialises
        // Result::Err(AskError::PayloadSizeMismatch).
        assert_eq!(
            rc,
            SendError::Ok as i32,
            "alloc-fail ask must return Ok (body completed); got rc={rc}"
        );
        assert!(
            reply_ptr.is_null(),
            "alloc-fail ask must deliver null reply payload; got ptr={reply_ptr:p}"
        );
        assert_eq!(
            reply_len, 0,
            "alloc-fail ask must deliver zero reply length; got len={reply_len}"
        );

        unsafe { hew_lambda_actor_release(actor) };
        std::thread::sleep(Duration::from_millis(50));
    }

    /// `state_drop` is called exactly once when the actor stops.
    #[test]
    fn state_drop_called_exactly_once() {
        use std::time::Duration;

        let counter = Arc::new(AtomicUsize::new(0));
        // Arc::into_raw keeps one reference; counting_state_drop reconstructs
        // and drops the Arc (decrement), which is fine because the test also
        // holds its own clone.
        let state_raw = Arc::into_raw(Arc::clone(&counter)) as *mut core::ffi::c_void;

        let actor = unsafe {
            hew_lambda_actor_new(
                4,
                LambdaShape::Tell as i32,
                Some(noop_tell_body),
                state_raw.cast::<core::ffi::c_void>(),
                Some(counting_state_drop),
            )
        };
        assert!(!actor.is_null());
        unsafe { hew_lambda_actor_release(actor) };
        // Give the dispatch thread time to exit and call state_drop.
        std::thread::sleep(Duration::from_millis(100));
        assert_eq!(
            counter.load(Ord::SeqCst),
            1,
            "state_drop must be called exactly once"
        );
    }

    /// Regression test for B4 (security review 2026-06-08):
    /// `hew_lambda_drain_all` must return only AFTER all dispatch threads'
    /// captured state has been torn down via `state_drop`. Prior to the
    /// drop-order fix in `HewLambdaActor::new`'s spawn closure, the
    /// `LambdaDispatchGuard` decremented `ACTIVE_LAMBDA_DISPATCH` BEFORE
    /// the captured `Arc<LambdaActorInner>` dropped, so drain could
    /// observe `count == 0` while `state_drop` had not yet run.
    ///
    /// The test races N actors: spawn → release → `drain_all` → check that
    /// every `state_drop` has fired. Iteration count is chosen to surface
    /// the race window deterministically on a fast machine.
    #[test]
    fn drain_all_completes_after_state_drop_runs() {
        const ITERS: usize = 25;
        const BATCH: usize = 8;

        // `hew_lambda_drain_all` reads the process-wide ACTIVE_LAMBDA_DISPATCH
        // counter. Any concurrent test spawning lambda actors will inflate
        // that counter mid-drain and stall this assertion at non-zero.
        let _guard = crate::runtime_test_guard();

        for iter in 0..ITERS {
            let counter = Arc::new(AtomicUsize::new(0));
            let mut handles = Vec::with_capacity(BATCH);
            for _ in 0..BATCH {
                let state_raw = Arc::into_raw(Arc::clone(&counter)) as *mut core::ffi::c_void;
                let actor = unsafe {
                    hew_lambda_actor_new(
                        4,
                        LambdaShape::Tell as i32,
                        Some(noop_tell_body),
                        state_raw,
                        Some(counting_state_drop),
                    )
                };
                assert!(!actor.is_null(), "iter {iter}: spawn must succeed");
                handles.push(actor);
            }
            // Release every strong handle. Each release returns
            // immediately; the dispatch thread will see the send-half
            // close and exit, then drop its captured Arc and run
            // state_drop, then drop the guard which decrements the
            // dispatch counter.
            for actor in handles {
                let rc = unsafe { hew_lambda_actor_release(actor) };
                assert_eq!(rc, SendError::Ok as i32);
            }
            // Drain: returns 0 when ACTIVE_LAMBDA_DISPATCH reaches 0.
            let rc = hew_lambda_drain_all(2_000);
            assert_eq!(rc, 0, "iter {iter}: drain must succeed (timeout 0)");
            // Post-drain invariant: state_drop has fired for every actor.
            let fired = counter.load(Ord::SeqCst);
            assert_eq!(
                fired, BATCH,
                "iter {iter}: drain returned 0 but state_drop fired {fired}/{BATCH} times \
                 (drop-order race: guard decrements counter before Arc drops)"
            );
        }
    }

    // ── H2 regression: drain_stopped malformed-envelope ───────────────────

    /// Regression test for H2: `drain_stopped` receiving a malformed ask
    /// envelope (< `ASK_REPLY_CH_PREFIX_LEN` bytes) must set `last_error`
    /// rather than silently dropping it.
    ///
    /// Calls `drain_stopped` directly on the test thread so the
    /// thread-local `set_last_error` is observable here.
    #[test]
    fn drain_stopped_malformed_ask_envelope_sets_last_error() {
        // Build inner + send-half directly — no dispatch thread spawned.
        let (inner, mailbox_in) = LambdaActorInner::new(
            4,
            LambdaShape::Ask,
            noop_tell_body,
            ptr::null_mut(),
            noop_state_drop,
        );
        // Send a malformed ask envelope: only 3 bytes, shorter than
        // ASK_REPLY_CH_PREFIX_LEN (8).  The send half accepts raw Vec<u8>.
        assert_eq!(
            mailbox_in.send(vec![0xDE, 0xAD, 0xBE]),
            SendError::Ok,
            "mailbox send must succeed before close"
        );
        // Drop the send half to close the mailbox so recv() returns Closed
        // after processing the one queued message.
        drop(mailbox_in);
        // Clear any prior error on this thread.
        crate::hew_clear_error();
        // Call drain_stopped on this thread — set_last_error is thread-local,
        // so calling it here makes it visible to the assert below.
        drain_stopped(&inner);
        let err_ptr = crate::hew_last_error();
        assert!(
            !err_ptr.is_null(),
            "malformed ask envelope must set last_error"
        );
        let err = unsafe { std::ffi::CStr::from_ptr(err_ptr).to_str().unwrap() };
        assert!(
            err.contains("malformed ask envelope"),
            "error message must mention 'malformed ask envelope'; got: {err}"
        );
    }

    // ── H3 regression: allocator-pairing tracker ──────────────────────────

    /// Regression test for H3: `free_body_reply_buf` must panic with an
    /// "allocator-pairing" message when passed a libc-tracked pointer
    /// (i.e., a pointer that was allocated via `libc::malloc` inside the
    /// reply channel, not via `Box`).
    ///
    /// Active only in debug builds; the `debug_assert!` is elided in
    /// release mode.
    /// Verify that a panic inside `state_drop` does NOT abort the process.
    ///
    /// Regression test for M3: `LambdaActorInner::drop` must wrap `state_drop`
    /// in `catch_unwind` so that a user panic cannot cause a double-panic →
    /// process abort during stack unwinding.
    #[test]
    fn state_drop_panic_does_not_abort_process() {
        static CALLED: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C-unwind" fn panicking_state_drop(_state: *mut core::ffi::c_void) {
            CALLED.fetch_add(1, Ord::Relaxed);
            panic!("intentional state_drop panic for M3 regression test");
        }

        // Clear any stale capture from a previous run.
        *super::LAST_DROP_PANIC_MSG
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = None;

        let actor = HewLambdaActor::new(
            1,
            LambdaShape::Tell,
            noop_tell_body,
            ptr::null_mut(),
            panicking_state_drop,
        )
        .expect("spawn");
        drop(actor);
        // Give the dispatch thread time to exit and invoke state_drop.
        std::thread::sleep(std::time::Duration::from_millis(50));
        // state_drop ran (incremented counter).
        assert_eq!(
            CALLED.load(Ord::Relaxed),
            1,
            "state_drop should have been called once"
        );
        // CALLED is incremented before the panic, so by the time CALLED==1
        // is confirmed above, set_last_error (and LAST_DROP_PANIC_MSG) have
        // already been written on the dispatch thread. The Mutex provides
        // the necessary memory-visibility barrier.
        let captured = super::LAST_DROP_PANIC_MSG
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let err_msg = captured
            .as_deref()
            .expect("state_drop panic must have logged an error via set_last_error");
        assert!(
            err_msg.contains("LambdaActorInner::drop user state_drop panicked:"),
            "error must name the runtime drop path; got: {err_msg}"
        );
        assert!(
            err_msg.contains("intentional state_drop panic"),
            "error must contain the original panic message; got: {err_msg}"
        );
        // Reaching here means catch_unwind prevented a process abort.
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "allocator-pairing")]
    fn allocator_pairing_body_reply_buf_panics_if_libc_tracked() {
        // Allocate a small buffer via libc::malloc and register it in the
        // libc-alloc tracker — simulating what alloc_reply_buffer does.
        let ptr = unsafe { libc::malloc(8) }.cast::<u8>();
        assert!(!ptr.is_null());
        crate::alloc_tracker::debug_track_libc_alloc(ptr);
        // free_body_reply_buf asserts the pointer is NOT libc-tracked.
        // The debug_assert fires before Box::from_raw, so no UB occurs.
        // SAFETY: the assert panics; Box::from_raw is never reached.
        unsafe { free_body_reply_buf(ptr, 8) };
        // Cleanup if somehow reached (e.g., release build running a
        // `#[cfg(debug_assertions)]` test block — shouldn't happen).
        unsafe { libc::free(ptr.cast()) };
    }

    /// A pointer tracked then untracked must report as not-tracked.
    /// This validates the untrack half of the lifecycle.
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_tracker_untrack_clears_tracking() {
        let ptr = unsafe { libc::malloc(8) }.cast::<u8>();
        assert!(!ptr.is_null());
        crate::alloc_tracker::debug_track_libc_alloc(ptr);
        assert!(
            crate::alloc_tracker::debug_is_libc_tracked(ptr),
            "pointer must be tracked after debug_track_libc_alloc"
        );
        crate::alloc_tracker::debug_untrack_libc_alloc(ptr);
        assert!(
            !crate::alloc_tracker::debug_is_libc_tracked(ptr),
            "pointer must not be tracked after debug_untrack_libc_alloc"
        );
        // ALLOCATOR-PAIRING: libc — symmetric free.
        unsafe { libc::free(ptr.cast()) };
    }

    /// A freshly Box-allocated pointer must never appear as libc-tracked.
    /// Guards against false positives in the pairing tracker.
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_globalalloc_ptr_not_libc_tracked() {
        let b: Box<u8> = Box::new(0);
        // ALLOCATOR-PAIRING: GlobalAlloc — into_raw for test only.
        let ptr = Box::into_raw(b);
        assert!(
            !crate::alloc_tracker::debug_is_libc_tracked(ptr),
            "Box-allocated pointer must not be libc-tracked"
        );
        // ALLOCATOR-PAIRING: GlobalAlloc — matching from_raw to avoid leak.
        // SAFETY: ptr was produced by Box::into_raw above; ownership returned here.
        unsafe { drop(Box::from_raw(ptr)) };
    }
}
