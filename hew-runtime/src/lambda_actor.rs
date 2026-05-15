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
//! ## Body invocation
//!
//! This module carries the data structures + ABI entries. Body
//! dispatch (deserialising a `Msg` envelope, running the lambda's
//! statements, posting a Reply for ask-shape) is consumed by the
//! codegen lowering. Tests prove the upgrade discipline without
//! invoking a body.

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
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Weak};

use crate::duplex::{HewDuplex, HewRecvHalf, HewSendHalf, RecvError, SendError};

/// Body-shape discriminator. Recorded at construction so the runtime
/// knows whether to provision a reply correlator; codegen (slice 5)
/// will read this to decide whether `hew_lambda_actor_send` (tell) or
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

/// Internal shared state. The Arc's strong count is the external
/// lambda-actor refcount.
///
/// The mailbox is constructed by pairing two `HewDuplex` handles and
/// immediately splitting each — the lambda-actor is one end of the
/// pair (clients write into `mailbox_in`; the body dispatch loop drains
/// from `mailbox_out`). The opposite-direction queue of each pair-side
/// is auto-released when the half-conversions drop their unused caps,
/// so only the client-write-to-body-read direction stays open.
#[derive(Debug)]
pub struct LambdaActorInner {
    /// Send-half retained from the client-side pair endpoint. Sends
    /// from `HewLambdaActor::send` write here.
    mailbox_in: HewSendHalf,
    /// Recv-half retained from the body-side pair endpoint. The
    /// body-dispatch loop drains messages here.
    mailbox_out: HewRecvHalf,
    /// Body-shape discriminator.
    shape: LambdaShape,
}

impl LambdaActorInner {
    fn new(mailbox_capacity: usize, shape: LambdaShape) -> Self {
        // Construct a cross-wired pair and immediately split each side
        // into the half it needs. `client` retains the send capability
        // toward the body; `body` retains the recv capability draining
        // those messages. The reverse-direction queue (body → client)
        // has no use today (tell-shape and the not-yet-wired ask-shape
        // reply path will introduce it when the runtime carries
        // correlator state); each split releases its unused cap and
        // the unused queue is freed within the constructor.
        let (client, body) = HewDuplex::new_pair(mailbox_capacity, mailbox_capacity);
        let mailbox_in = client.into_send_half();
        let mailbox_out = body.into_recv_half();
        Self {
            mailbox_in,
            mailbox_out,
            shape,
        }
    }

    /// Send a message envelope on the actor's mailbox. Blocks if the
    /// mailbox is full and at least one receiver remains.
    pub fn send(&self, msg: Vec<u8>) -> SendError {
        self.mailbox_in.send(msg)
    }

    /// Drain the next message envelope from the mailbox. Used by the
    /// body-dispatch loop (not exposed via C-ABI yet).
    pub fn recv(&self) -> Result<Vec<u8>, RecvError> {
        self.mailbox_out.recv()
    }

    #[must_use]
    pub fn shape(&self) -> LambdaShape {
        self.shape
    }
}

// ── Strong / Weak handles ──────────────────────────────────────────────────

/// Strong lambda-actor handle. Holding a `HewLambdaActor` keeps the
/// actor alive. When the last strong handle drops, the inner state
/// (including the embedded `HewDuplex`) is freed and any body-side
/// weak handle's `upgrade` will fail thereafter.
#[derive(Debug, Clone)]
pub struct HewLambdaActor {
    inner: Arc<LambdaActorInner>,
}

impl HewLambdaActor {
    /// Construct a new lambda-actor instance with the given mailbox
    /// capacity and body shape (tell or ask).
    #[must_use]
    pub fn new(mailbox_capacity: usize, shape: LambdaShape) -> Self {
        Self {
            inner: Arc::new(LambdaActorInner::new(mailbox_capacity, shape)),
        }
    }

    /// Send a message envelope. Failure modes:
    ///
    /// - `SendError::Closed` if the inner Duplex was closed (the body
    ///   side dropped its receiver), or
    /// - `SendError::Ok` on success.
    pub fn send(&self, msg: Vec<u8>) -> SendError {
        self.inner.send(msg)
    }

    /// Downgrade to a weak handle. The weak handle does NOT keep the
    /// actor alive (§5.9 ratification 2 — the lambda body captures
    /// its own binding name via this primitive).
    #[must_use]
    pub fn downgrade(&self) -> HewLambdaActorWeak {
        HewLambdaActorWeak {
            inner: Arc::downgrade(&self.inner),
        }
    }

    /// Refcount-bump clone of the strong handle.
    #[must_use = "the clone bumps the actor's external refcount; drop releases it"]
    pub fn clone_handle(&self) -> Self {
        self.clone()
    }

    /// Borrow the inner state (used by the body-dispatch loop).
    #[must_use]
    pub fn inner(&self) -> &LambdaActorInner {
        &self.inner
    }
}

/// Weak lambda-actor handle. Body-side self-references use this so
/// that the body never keeps the actor alive past external refcount
/// zero. The upgrade-on-use discipline turns a self-send into
/// `SendError::ActorStopped` once the externals are gone.
#[derive(Debug, Clone)]
pub struct HewLambdaActorWeak {
    inner: Weak<LambdaActorInner>,
}

impl HewLambdaActorWeak {
    /// Try to upgrade to a strong handle. Returns `None` if the
    /// external strong refcount has reached zero.
    #[must_use]
    pub fn upgrade(&self) -> Option<HewLambdaActor> {
        self.inner.upgrade().map(|inner| HewLambdaActor { inner })
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

// ── Double-close guard wrappers ────────────────────────────────────────────
//
// See the parallel comment in duplex.rs for the full rationale. Short form:
// the outer allocation is intentionally never freed so the `released` flag
// remains readable on any subsequent call, converting a double-free UB into
// a typed `SendError::DoubleClose` (= 4) return.

/// C-ABI wrapper around [`HewLambdaActor`] with a double-release guard.
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

/// Allocate a new lambda-actor instance with the given mailbox capacity
/// and shape discriminant. `shape` must be `0` (Tell) or `1` (Ask);
/// any other value sets a last-error and returns null.
///
/// # Safety
///
/// The returned pointer is owned by the runtime and must be released
/// with [`hew_lambda_actor_release`].
#[no_mangle]
pub extern "C" fn hew_lambda_actor_new(
    mailbox_capacity: usize,
    shape: i32,
) -> *mut HewLambdaActorHandle {
    if mailbox_capacity == 0 {
        crate::set_last_error("hew_lambda_actor_new: mailbox_capacity must be > 0".to_string());
        return ptr::null_mut();
    }
    let shape = match shape {
        0 => LambdaShape::Tell,
        1 => LambdaShape::Ask,
        other => {
            crate::set_last_error(format!(
                "hew_lambda_actor_new: invalid shape discriminant {other}"
            ));
            return ptr::null_mut();
        }
    };
    Box::into_raw(Box::new(HewLambdaActorHandle::new(HewLambdaActor::new(
        mailbox_capacity,
        shape,
    ))))
}

/// Refcount-bump clone of a strong lambda-actor handle.
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
    // SAFETY:
    // - Provenance: caller guarantees `actor` came from `hew_lambda_actor_new`
    //   or a prior `_clone` call.
    // - Type tag: `*mut HewLambdaActorHandle` matches the originating type.
    // - Lifetime owner: caller still owns the wrapper; we take a shared
    //   borrow of the inner actor only, then heap-allocate the clone.
    // - Aliasing concurrency: `HewLambdaActor` is `Clone`-via-Arc; cloning
    //   an Arc is atomic and Send/Sync-safe.
    // - Bounds: single non-null aligned pointer dereference.
    // - Failure mode: caller-supplied dangling pointer is UB; not
    //   detectable at this layer.
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
    let res = unsafe { (*actor).inner.send(payload) };
    res as i32
}

/// Release a strong lambda-actor handle. If this is the last strong
/// handle, the inner state (including the embedded `HewDuplex`) is
/// freed and body-side weak captures will fail their next upgrade.
///
/// Returns `SendError::Ok` (0) on the first call.
/// Returns `SendError::DoubleClose` (4) on any subsequent call without
/// invoking undefined behaviour — per design principle D4.
///
/// The outer handle allocation intentionally persists after release so
/// that the double-release guard flag remains readable for subsequent calls.
///
/// # Safety
///
/// `actor` must have been returned by [`hew_lambda_actor_new`] or
/// [`hew_lambda_actor_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_release(actor: *mut HewLambdaActorHandle) -> i32 {
    if actor.is_null() {
        return SendError::Ok as i32;
    }
    // SAFETY: (flag-read phase)
    // - Provenance: caller guarantees `actor` came from
    //   `Box::into_raw(Box::new(HewLambdaActorHandle::new(...)))`.
    // - Type tag: `*mut HewLambdaActorHandle` matches the originating type.
    //   `released` is the first field of `#[repr(C)]` HewLambdaActorHandle;
    //   `addr_of!((*actor).released)` yields a valid `*const AtomicBool`
    //   without materialising a `&mut HewLambdaActorHandle` (which would
    //   alias with a concurrent release caller's `&mut` on the same
    //   pointer — formal aliasing UB even when `inner` is not touched twice).
    // - Lifetime owner: the outer wrapper is never freed (intentional leak
    //   — see wrapper-struct design comment in duplex.rs).
    // - Aliasing concurrency: AcqRel swap linearizes all release callers;
    //   the loser observes `true` and returns before constructing any
    //   reference into the wrapper.
    // - Bounds: single in-bounds field read on a non-null aligned
    //   `#[repr(C)]` pointer.
    // - Failure mode: double-release returns SendError::DoubleClose (4)
    //   via the atomic-flag guard; no UB on the second call.
    let released = unsafe { &*ptr::addr_of!((*actor).released) };
    if released.swap(true, Ordering::AcqRel) {
        return SendError::DoubleClose as i32;
    }
    // SAFETY: (drop phase) the swap won — exclusive against all other
    // release callers.
    let h = unsafe { &mut *actor };
    // SAFETY: first release — inner HewLambdaActor is still valid.
    // ManuallyDrop::drop decrements the Arc strong count (and triggers
    // LambdaActorInner Drop if it reaches zero, which closes the
    // HewDuplex) without freeing the outer wrapper.
    unsafe { ManuallyDrop::drop(&mut h.inner) };
    SendError::Ok as i32
}

/// Downgrade a strong handle to a weak one. The returned pointer is
/// owned by the caller and must be released with
/// [`hew_lambda_actor_weak_drop`].
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
    // SAFETY: see hew_lambda_actor_clone — identical six-axis profile.
    let weak = unsafe { (*actor).inner.downgrade() };
    Box::into_raw(Box::new(HewLambdaActorWeakHandle::new(weak)))
}

/// Attempt a weak self-send. Upgrades the weak handle just long enough
/// to dispatch; if the upgrade fails (external strong refcount is zero)
/// returns `SendError::ActorStopped` discriminant instead of
/// resurrecting the actor — §5.9 ratification 2 enforcement.
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
/// Returns `SendError::DoubleClose` (4) on any subsequent call without
/// invoking undefined behaviour — per design principle D4.
///
/// # Safety
///
/// `weak` must have been returned by [`hew_lambda_actor_downgrade`] or
/// [`hew_lambda_actor_weak_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_lambda_actor_weak_drop(weak: *mut HewLambdaActorWeakHandle) -> i32 {
    if weak.is_null() {
        return SendError::Ok as i32;
    }
    // SAFETY: (flag-read phase) see `hew_lambda_actor_release` — read
    // `released` via `addr_of!` so no `&mut HewLambdaActorWeakHandle` is
    // materialised before the swap; that avoids aliasing UB against a
    // concurrent weak-drop caller on the same pointer. `released` is the
    // first field of the `#[repr(C)]` wrapper.
    let released = unsafe { &*ptr::addr_of!((*weak).released) };
    if released.swap(true, Ordering::AcqRel) {
        return SendError::DoubleClose as i32;
    }
    // SAFETY: (drop phase) the swap won — exclusive against all other
    // weak-drop callers.
    let h = unsafe { &mut *weak };
    // SAFETY: first drop — inner HewLambdaActorWeak is still valid.
    // ManuallyDrop::drop decrements the Arc weak count without freeing
    // the outer wrapper.
    unsafe { ManuallyDrop::drop(&mut h.inner) };
    SendError::Ok as i32
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tell_shape_send_then_recv_roundtrips() {
        let a = HewLambdaActor::new(4, LambdaShape::Tell);
        assert_eq!(a.send(b"msg".to_vec()), SendError::Ok);
        let inner = a.inner();
        let got = inner.recv().expect("recv");
        assert_eq!(got, b"msg");
    }

    #[test]
    fn weak_upgrade_succeeds_while_strong_alive() {
        let a = HewLambdaActor::new(2, LambdaShape::Tell);
        let w = a.downgrade();
        // Strong is alive — upgrade gives Some.
        let upgraded = w.upgrade().expect("upgrade while alive");
        assert_eq!(upgraded.send(b"via-weak".to_vec()), SendError::Ok);
        drop(upgraded);
        drop(a);
    }

    #[test]
    fn weak_upgrade_fails_after_strong_drops() {
        let a = HewLambdaActor::new(2, LambdaShape::Tell);
        let w = a.downgrade();
        drop(a);
        assert!(w.upgrade().is_none());
        // Self-send through weak should surface ActorStopped, not block.
        assert_eq!(w.send(b"stopped".to_vec()), SendError::ActorStopped);
    }

    #[test]
    fn weak_does_not_keep_actor_alive() {
        // §5.9 ratification 2: body-side weak self-ref must NOT keep
        // the actor alive past external refcount zero. Demonstrate
        // by checking that strong_count drops to zero even with a
        // live weak handle.
        let a = HewLambdaActor::new(2, LambdaShape::Tell);
        let w = a.downgrade();
        assert_eq!(Arc::strong_count(&a.inner), 1);
        drop(a);
        // Weak still exists, but the inner was freed because no
        // strong remained.
        assert!(w.upgrade().is_none());
    }

    #[test]
    fn multiple_strong_handles_keep_actor_alive() {
        let a = HewLambdaActor::new(2, LambdaShape::Tell);
        let b = a.clone_handle();
        let w = a.downgrade();
        drop(a);
        // b still strong: upgrade should succeed.
        let up = w.upgrade().expect("upgrade while sibling strong alive");
        assert_eq!(up.send(b"x".to_vec()), SendError::Ok);
        drop(up);
        drop(b);
        // Now no strong remains.
        assert!(w.upgrade().is_none());
    }

    #[test]
    fn cabi_new_send_release_tell() {
        let a = hew_lambda_actor_new(4, LambdaShape::Tell as i32);
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
    fn cabi_weak_send_returns_actor_stopped_after_release() {
        let a = hew_lambda_actor_new(4, LambdaShape::Tell as i32);
        // SAFETY: a non-null.
        let w = unsafe { hew_lambda_actor_downgrade(a) };
        assert!(!w.is_null());
        // SAFETY: a is a live strong handle; release it.
        unsafe { assert_eq!(hew_lambda_actor_release(a), SendError::Ok as i32) };
        // After release the weak's upgrade fails; send surfaces ActorStopped.
        let payload = b"recurse";
        // SAFETY: w is still a valid weak-handle wrapper; payload valid.
        let rc = unsafe { hew_lambda_actor_weak_send(w, payload.as_ptr(), payload.len()) };
        assert_eq!(rc, SendError::ActorStopped as i32);
        // SAFETY: drop the weak.
        unsafe { assert_eq!(hew_lambda_actor_weak_drop(w), SendError::Ok as i32) };
    }

    #[test]
    fn cabi_weak_send_succeeds_while_strong_alive() {
        let a = hew_lambda_actor_new(4, LambdaShape::Tell as i32);
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

    #[test]
    fn cabi_new_rejects_invalid_shape() {
        let a = hew_lambda_actor_new(4, 42);
        assert!(a.is_null());
    }

    #[test]
    fn cabi_new_rejects_zero_capacity() {
        let a = hew_lambda_actor_new(0, LambdaShape::Tell as i32);
        assert!(a.is_null());
    }

    // ── Double-release guard test ──────────────────────────────────────────

    #[test]
    fn cabi_double_release_lambda_actor_returns_already_closed() {
        // Verify that calling hew_lambda_actor_release twice on the same
        // handle returns DoubleClose on the second call rather than UB.
        let a = hew_lambda_actor_new(4, LambdaShape::Tell as i32);
        assert!(!a.is_null());
        // SAFETY: `a` came from hew_lambda_actor_new (non-null per assertion).
        unsafe {
            // First release: normal.
            assert_eq!(hew_lambda_actor_release(a), SendError::Ok as i32);
            // Second release: double-release guard fires.
            assert_eq!(hew_lambda_actor_release(a), SendError::DoubleClose as i32);
        }
    }

    // ── Concurrent release stress ──────────────────────────────────────────
    //
    // Same shape as `cabi_concurrent_close_duplex_returns_single_winner`
    // in duplex.rs, applied to `hew_lambda_actor_release`. The
    // raw-pointer flag-read in that entry is what makes concurrent calls
    // sound — a regression to `&mut *actor` before the swap would alias
    // multiple winners' `&mut`s simultaneously (formal Rust UB).

    #[test]
    fn cabi_concurrent_release_lambda_actor_returns_single_winner() {
        use std::sync::Barrier;
        use std::thread;
        const THREADS: usize = 8;
        const ITERS: usize = 100;

        for iter in 0..ITERS {
            let actor = hew_lambda_actor_new(4, LambdaShape::Tell as i32);
            assert!(!actor.is_null());
            let actor_addr = actor as usize;
            let barrier = Arc::new(Barrier::new(THREADS));
            let mut handles = Vec::with_capacity(THREADS);
            for _ in 0..THREADS {
                let bar = Arc::clone(&barrier);
                handles.push(thread::spawn(move || {
                    bar.wait();
                    // SAFETY: same handle pointer shared across threads;
                    // implementation linearizes via AtomicBool::swap.
                    unsafe { hew_lambda_actor_release(actor_addr as *mut HewLambdaActorHandle) }
                }));
            }
            let mut winners = 0;
            let mut losers = 0;
            for h in handles {
                match h.join().unwrap() {
                    rc if rc == SendError::Ok as i32 => winners += 1,
                    rc if rc == SendError::DoubleClose as i32 => losers += 1,
                    rc => panic!(
                        "iter {iter}: unexpected return {rc} from concurrent hew_lambda_actor_release"
                    ),
                }
            }
            assert_eq!(
                winners, 1,
                "iter {iter}: expected one winner, got {winners}"
            );
            assert_eq!(
                losers,
                THREADS - 1,
                "iter {iter}: expected {} losers, got {losers}",
                THREADS - 1
            );
        }
    }
}
