//! Accept-fixture parity tests for the M2 unified-concurrency runtime
//! substrate.
//!
//! The plan's named accept fixtures (`lambda_self_send_fib.hew`,
//! `duplex_construct_pair.hew`, `duplex_split_halves.hew`,
//! `duplex_close_both_dirs.hew`) cannot run end-to-end without slice
//! 5 codegen (which emits the `hew_duplex_*` / `hew_lambda_actor_*`
//! ABI calls from compiled Hew programs). Until slice 5 lands, the
//! same behavioural envelope is exercised through the C-ABI surface
//! directly via these Rust integration tests.
//!
//! Each test names the corresponding Hew fixture in a doc comment so
//! slice 5 can graduate them to `.hew` form by lifting the body into
//! an `actor |...| { ... }` literal that lowers to the same C-ABI
//! calls.

#![cfg(not(target_arch = "wasm32"))]

use std::ptr;

use hew_runtime::duplex::{
    hew_duplex_clone, hew_duplex_close, hew_duplex_close_half, hew_duplex_pair,
    hew_duplex_payload_free, hew_duplex_recv, hew_duplex_recv_half, hew_duplex_send,
    hew_duplex_send_half, hew_recv_half_recv, hew_send_half_send, HewDuplexDirection,
    HewDuplexHandle, RecvError, SendError,
};
use hew_runtime::lambda_actor::{
    hew_lambda_actor_downgrade, hew_lambda_actor_new, hew_lambda_actor_release,
    hew_lambda_actor_weak_drop, hew_lambda_actor_weak_send, LambdaShape,
};

/// Helper: synchronous recv that returns the bytes (copying out of the
/// runtime-owned buffer before freeing it).
///
/// # Safety
///
/// `d` must point to a valid `HewDuplexHandle` returned by the runtime.
unsafe fn recv_bytes(d: *mut HewDuplexHandle) -> Result<Vec<u8>, i32> {
    let mut p: *mut u8 = ptr::null_mut();
    let mut n: usize = 0;
    // SAFETY:
    // - Provenance: `d` valid per caller contract; `p` / `n` are
    //   local stack slots writeable by the FFI call.
    // - Type tag: passed types match the FFI signature.
    // - Lifetime owner: the recv call transfers payload ownership
    //   into `p`; this helper holds the payload only for the copy.
    // - Aliasing concurrency: `p` / `n` are not shared with other
    //   threads; this call frame has exclusive access.
    // - Bounds: single FFI call; the runtime writes one pointer + one
    //   usize into the provided slots.
    // - Failure mode: dangling `d` is the caller's responsibility.
    let rc = unsafe { hew_duplex_recv(d, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n)) };
    if rc != RecvError::Ok as i32 {
        return Err(rc);
    }
    // SAFETY:
    // - Provenance: `p` came from a successful `hew_duplex_recv`.
    // - Lifetime owner: freed below after `to_vec` copy.
    let bytes = unsafe { std::slice::from_raw_parts(p, n) }.to_vec();
    // SAFETY: `p` is freed exactly once here; `n` is its length.
    unsafe { hew_duplex_payload_free(p, n) };
    Ok(bytes)
}

// ── duplex_construct_pair.hew ──────────────────────────────────────────────
//
// Demonstrates `duplex_pair::<Cmd, Reply>(N)` constructing two
// cross-wired Duplex handles where each side's send-direction is the
// other side's recv-direction.

#[test]
fn duplex_construct_pair() {
    let mut a: *mut HewDuplexHandle = ptr::null_mut();
    let mut b: *mut HewDuplexHandle = ptr::null_mut();
    // SAFETY: out_a / out_b are valid stack slots.
    let rc = unsafe { hew_duplex_pair(8, 8, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
    assert_eq!(rc, SendError::Ok as i32);
    assert!(!a.is_null() && !b.is_null());

    let cmd = b"do-the-thing";
    let reply = b"done";
    // SAFETY: a, b non-null; payloads valid for their lengths.
    unsafe {
        assert_eq!(
            hew_duplex_send(a, cmd.as_ptr(), cmd.len()),
            SendError::Ok as i32
        );
        assert_eq!(recv_bytes(b).unwrap(), cmd);
        assert_eq!(
            hew_duplex_send(b, reply.as_ptr(), reply.len()),
            SendError::Ok as i32
        );
        assert_eq!(recv_bytes(a).unwrap(), reply);
        assert_eq!(hew_duplex_close(a), SendError::Ok as i32);
        assert_eq!(hew_duplex_close(b), SendError::Ok as i32);
    }
}

// ── duplex_split_halves.hew ────────────────────────────────────────────────
//
// Demonstrates `.send_half()` / `.recv_half()` extraction: a unified
// handle is consumed and direction-only aliases remain, each carrying
// the correct refcount cap.

#[test]
fn duplex_split_halves() {
    let mut a: *mut HewDuplexHandle = ptr::null_mut();
    let mut b: *mut HewDuplexHandle = ptr::null_mut();
    // SAFETY: out-slots valid.
    unsafe {
        hew_duplex_pair(4, 4, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
        let a_send = hew_duplex_send_half(a);
        let b_recv = hew_duplex_recv_half(b);
        assert!(!a_send.is_null() && !b_recv.is_null());
        let payload = b"split-msg";
        assert_eq!(
            hew_send_half_send(a_send, payload.as_ptr(), payload.len()),
            SendError::Ok as i32
        );
        let mut p: *mut u8 = ptr::null_mut();
        let mut n: usize = 0;
        let rc = hew_recv_half_recv(b_recv, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n));
        assert_eq!(rc, RecvError::Ok as i32);
        let received = std::slice::from_raw_parts(p, n).to_vec();
        assert_eq!(received, payload);
        hew_duplex_payload_free(p, n);
        // Closing the send half should signal the receiver.
        assert_eq!(
            hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32),
            SendError::Ok as i32
        );
        let rc2 = hew_recv_half_recv(b_recv, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n));
        assert_eq!(rc2, RecvError::Closed as i32);
        assert_eq!(
            hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32),
            SendError::Ok as i32
        );
    }
}

// ── duplex_close_both_dirs.hew ─────────────────────────────────────────────
//
// Demonstrates close-both-directions on last-handle drop: dropping the
// unified Duplex on side `a` closes BOTH directions of the underlying
// dual queue from `a`'s perspective, so `b` sees Closed both when it
// tries to recv (no remaining sender on the a->b queue) and when it
// tries to send (no remaining receiver on the b->a queue).

#[test]
fn duplex_close_both_dirs() {
    let mut a: *mut HewDuplexHandle = ptr::null_mut();
    let mut b: *mut HewDuplexHandle = ptr::null_mut();
    // SAFETY: out-slots valid.
    unsafe {
        hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
        let payload = b"final";
        hew_duplex_send(a, payload.as_ptr(), payload.len());
        assert_eq!(hew_duplex_close(a), SendError::Ok as i32);
        // b drains the pending message.
        assert_eq!(recv_bytes(b).unwrap(), payload);
        // After drain, b's recv sees Closed (no remaining sender).
        let mut p: *mut u8 = ptr::null_mut();
        let mut n: usize = 0;
        assert_eq!(
            hew_duplex_recv(b, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n)),
            RecvError::Closed as i32
        );
        // And b's send fails closed (no remaining receiver on its outbound queue).
        let p2 = b"too-late";
        assert_eq!(
            hew_duplex_send(b, p2.as_ptr(), p2.len()),
            SendError::Closed as i32
        );
        assert_eq!(hew_duplex_close(b), SendError::Ok as i32);
    }
}

// ── lambda_self_send_fib.hew ───────────────────────────────────────────────
//
// Demonstrates the §5.9 ratification 2 stop-guarantee: a lambda body's
// self-binding-name reference is captured as a weak handle, so the
// body never keeps the actor alive past external strong-refcount
// zero. When external handles drop, an attempted self-send (which
// would be the recursive `fib(n-1)` body invocation in the surface
// program) surfaces as SendError::ActorStopped instead of
// resurrecting the actor.
//
// This test exercises weak-refcount discipline only; it does not
// exercise ask/reply semantics. The actor uses Tell shape with a
// noop body so the test is self-contained.

/// Noop tell-shape body callback for tests that only need refcount mechanics.
unsafe extern "C-unwind" fn noop_tell_body(
    _state: *mut std::ffi::c_void,
    _msg: *const u8,
    _msg_len: usize,
    reply_out: *mut *mut u8,
    reply_len_out: *mut usize,
) -> i32 {
    // SAFETY: reply_out / reply_len_out are non-null out-params supplied by
    // the dispatch layer; writing null/0 is valid for tell-shape noop bodies.
    unsafe {
        *reply_out = ptr::null_mut();
        *reply_len_out = 0;
    }
    0
}

/// Noop state-drop for tests that use a null state pointer.
unsafe extern "C" fn noop_state_drop(_state: *mut std::ffi::c_void) {}

#[test]
fn lambda_self_send_fib_stop_after_external_release() {
    // SAFETY: noop_tell_body / noop_state_drop are valid extern "C" fn ptrs;
    // state is null (noop body ignores it); capacity > 0.
    let actor = unsafe {
        hew_lambda_actor_new(
            8,
            LambdaShape::Tell as i32,
            Some(noop_tell_body),
            ptr::null_mut(),
            Some(noop_state_drop),
        )
    };
    assert!(!actor.is_null());
    // SAFETY: actor non-null.
    let weak = unsafe { hew_lambda_actor_downgrade(actor) };
    assert!(!weak.is_null());
    // Simulate the body running while external refcount > 0: weak
    // self-send succeeds.
    let arg = b"5";
    // SAFETY: handles valid; arg valid for its length.
    let rc1 = unsafe { hew_lambda_actor_weak_send(weak, arg.as_ptr(), arg.len()) };
    assert_eq!(rc1, SendError::Ok as i32);
    // Drop the external strong handle.
    // SAFETY: actor is a live strong handle.
    unsafe { assert_eq!(hew_lambda_actor_release(actor), SendError::Ok as i32) };
    // Now the body's self-send must surface ActorStopped — NOT block,
    // NOT enqueue silently, NOT resurrect.
    let arg2 = b"4";
    // SAFETY: weak still valid; arg valid.
    let rc2 = unsafe { hew_lambda_actor_weak_send(weak, arg2.as_ptr(), arg2.len()) };
    assert_eq!(rc2, SendError::ActorStopped as i32);
    // SAFETY: clean up weak.
    unsafe { assert_eq!(hew_lambda_actor_weak_drop(weak), SendError::Ok as i32) };
}

// ── Clone keeps directions open ────────────────────────────────────────────

#[test]
fn duplex_clone_keeps_directions_open_after_original_close() {
    let mut a: *mut HewDuplexHandle = ptr::null_mut();
    let mut b: *mut HewDuplexHandle = ptr::null_mut();
    // SAFETY: out-slots valid.
    unsafe {
        hew_duplex_pair(4, 4, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
        let a2 = hew_duplex_clone(a);
        assert!(!a2.is_null());
        assert_eq!(hew_duplex_close(a), SendError::Ok as i32);
        // a2 still alive — b should still see a live sender + receiver.
        let payload = b"via-clone";
        assert_eq!(
            hew_duplex_send(a2, payload.as_ptr(), payload.len()),
            SendError::Ok as i32
        );
        assert_eq!(recv_bytes(b).unwrap(), payload);
        assert_eq!(hew_duplex_close(a2), SendError::Ok as i32);
        assert_eq!(hew_duplex_close(b), SendError::Ok as i32);
    }
}
