//! Hew runtime: read slot for the non-blocking `await conn.read()` pattern.
//!
//! A read slot is the value-routing vehicle for a suspended fd-read. It is the
//! fd-readiness analogue of [`crate::reply_channel::HewReplyChannel`]: the
//! suspendable handler creates a slot, registers it with the reactor carrying
//! its parked continuation (`reactor_await_read`), and suspends — freeing the
//! worker. When the reactor reports the fd ready it reads the available bytes,
//! deposits the resulting `bytes` value (or an error/EOF status) into the slot,
//! and wakes the parked continuation via
//! `crate::scheduler::enqueue_resume(caller_actor, null)` (the same
//! source-agnostic waker the reply path uses). On the resume edge the handler
//! takes the deposited result out of the slot and binds it.
//!
//! Unlike the reply channel there is NO condvar: the only consumer is a parked
//! coroutine woken by `enqueue_resume`, never a blocked foreign thread. The slot
//! is a one-shot atomic cell plus a manual refcount that lets the abandon edge
//! (handler freed while suspended) free the slot without racing a reactor that
//! still holds a pointer to it on its `Registration`.
//!
//! Native-only: the reactor that fills the slot is `epoll`/`kqueue`-backed and
//! does not exist on `wasm32`.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at each fn signature."
)]

use std::sync::atomic::{AtomicI32, AtomicUsize, Ordering};

use crate::bytes::BytesTriple;

/// The deposit status the reactor records into a read slot before waking the
/// parked continuation. The resume edge reads it to decide whether to bind the
/// `Ok(bytes)` payload or an error/EOF.
///
/// The integer values are part of the codegen ABI: `emit_suspending_read_terminator`
/// branches on `hew_read_slot_status` and the discriminants must stay in
/// lock-step with the codegen literals.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadStatus {
    /// No deposit yet — the reactor has not fired (only observed transiently;
    /// the resume edge never sees this because it runs after `enqueue_resume`).
    Pending = 0,
    /// Bytes were read and deposited in `value`. The resume edge takes ownership.
    Data = 1,
    /// The peer closed cleanly (EOF) with no buffered bytes. The resume edge
    /// binds an empty `bytes` (matching the blocking `hew_tcp_read` EOF
    /// convention: an empty triple).
    Eof = 2,
    /// A hard read error / HUP occurred. The resume edge binds an empty `bytes`
    /// (the error variant is the empty-buffer signal the blocking path also
    /// uses; NEW-6 layers a typed-error surface on top of this).
    Error = 3,
}

impl ReadStatus {
    #[cfg(test)]
    fn from_i32(v: i32) -> Self {
        match v {
            1 => ReadStatus::Data,
            2 => ReadStatus::Eof,
            3 => ReadStatus::Error,
            _ => ReadStatus::Pending,
        }
    }
}

/// One-shot read slot. Created by the suspending-read codegen ramp, filled by
/// the reactor thread on fd readiness, drained by the resume edge.
///
/// Lifecycle of the manual refcount (`refs`):
/// - `hew_read_slot_new` → refs = 1 (the CREATOR ref, owned by the suspending
///   handler / its coro frame).
/// - `reactor_await_read` snapshots a pointer to the slot onto its
///   `Registration` and bumps refs to 2 (the REACTOR ref). The reactor releases
///   its ref after it deposits + wakes (or when the registration is torn down).
/// - The resume edge calls `hew_read_slot_take` (reads the deposit) then
///   `hew_read_slot_free` (drops the creator ref). The slot is freed when the
///   last ref drops.
/// - Abandon edge (handler freed while suspended): `hew_read_slot_cancel` marks
///   the slot cancelled (so a still-pending reactor deposit is dropped instead
///   of waking a freed actor) then `hew_read_slot_free` drops the creator ref;
///   the reactor's own ref is dropped by `reactor_detach_actor` scrubbing the
///   registration. The struct is freed only when BOTH refs are gone.
#[repr(C)]
#[derive(Debug)]
pub struct HewReadSlot {
    /// Manual reference count shared by the suspending handler and the reactor.
    refs: AtomicUsize,
    /// The deposit status ([`ReadStatus`] discriminant). Written by the reactor
    /// with `Release`, read by the resume edge with `Acquire`.
    status: AtomicI32,
    /// Set to 1 by [`hew_read_slot_cancel`] when the suspending handler is torn
    /// down before the deposit. The reactor checks this BEFORE depositing /
    /// waking and skips both — a cancelled slot never wakes a freed actor.
    cancelled: AtomicI32,
    /// The deposited `bytes` value (owned, refcount-1) when `status == Data`.
    /// An empty triple (`ptr == null`, `len == 0`) otherwise. The resume edge
    /// takes ownership; the abandon edge drops the buffer if a deposit landed
    /// before cancellation.
    value: BytesTriple,
}

// SAFETY: `HewReadSlot` is designed for cross-thread use. `status` is the
// release/acquire barrier between the reactor writer and the resume-edge reader;
// `value` is only read after the `status` acquire load observes a non-Pending
// deposit. The refcount serialises the free.
unsafe impl Send for HewReadSlot {}
// SAFETY: concurrent access is synchronised through the atomics + the refcount.
unsafe impl Sync for HewReadSlot {}

/// Allocate a new read slot. Returns a pointer with one creator ref.
///
/// # Safety
///
/// The returned pointer must eventually be released with [`hew_read_slot_free`].
#[no_mangle]
pub extern "C" fn hew_read_slot_new() -> *mut HewReadSlot {
    Box::into_raw(Box::new(HewReadSlot {
        refs: AtomicUsize::new(1),
        status: AtomicI32::new(ReadStatus::Pending as i32),
        cancelled: AtomicI32::new(0),
        value: BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        },
    }))
}

/// Read the current refcount (test-only). Lets a forced-ordering test assert the
/// in-flight ref the reactor takes keeps the slot alive across a registration
/// scrub.
///
/// # Safety
///
/// `slot` must be a valid live `HewReadSlot` the caller holds a ref to.
#[cfg(test)]
pub(crate) unsafe fn read_slot_refs_for_test(slot: *mut HewReadSlot) -> usize {
    // SAFETY: caller holds a ref, so the box is live.
    unsafe { (*slot).refs.load(Ordering::Acquire) }
}

/// Bump the refcount (the reactor's ref). Returns the new count.
///
/// # Safety
///
/// `slot` must be a valid live `HewReadSlot` (the caller holds a ref).
pub(crate) unsafe fn read_slot_retain(slot: *mut HewReadSlot) {
    if slot.is_null() {
        return;
    }
    // SAFETY: caller holds a ref, so the box is live.
    unsafe { (*slot).refs.fetch_add(1, Ordering::AcqRel) };
}

/// Drop one ref; free the box when the last ref drops. The runtime export the
/// suspending-read ramp calls on the resume / send-failure / abandon edges.
///
/// If a `Data` deposit is still present (an abandon edge that raced a reactor
/// deposit), the buffer is released on the final drop so the read bytes are not
/// leaked.
///
/// # Safety
///
/// `slot` may be null (no-op). Otherwise it must be a valid `HewReadSlot` the
/// caller holds a ref to; the caller must not use `slot` after this returns.
#[no_mangle]
pub unsafe extern "C" fn hew_read_slot_free(slot: *mut HewReadSlot) {
    if slot.is_null() {
        return;
    }
    // SAFETY: caller holds a ref, so the box is live until we drop it.
    let prev = unsafe { (*slot).refs.fetch_sub(1, Ordering::AcqRel) };
    if prev != 1 {
        return;
    }
    // Last ref — reclaim the box. SAFETY: refs reached 0, so no other thread
    // holds a live reference; we own the box exclusively.
    let boxed = unsafe { Box::from_raw(slot) };
    // Release a still-present Data deposit's buffer (abandon-after-deposit).
    if boxed.status.load(Ordering::Acquire) == ReadStatus::Data as i32 && !boxed.value.ptr.is_null()
    {
        // SAFETY: `value.ptr` is an owned refcount-1 bytes buffer from
        // `hew_bytes_from_static`; nothing else references it after the box drop.
        unsafe { crate::bytes::hew_bytes_drop(boxed.value.ptr) };
    }
    drop(boxed);
}

/// Mark the slot cancelled (the abandon edge). The reactor checks this before
/// depositing + waking; a cancelled slot never wakes a freed actor and never
/// keeps a deposited buffer alive past the handler's teardown.
///
/// # Safety
///
/// `slot` may be null (no-op). Otherwise it must be a valid `HewReadSlot` the
/// caller holds a ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_read_slot_cancel(slot: *mut HewReadSlot) {
    if slot.is_null() {
        return;
    }
    // SAFETY: caller holds a ref.
    unsafe { (*slot).cancelled.store(1, Ordering::Release) };
}

/// Read the deposit status without consuming it. The resume edge branches on
/// this to pick the `Ok(bytes)` / EOF / error binding.
///
/// # Safety
///
/// `slot` must be a valid `HewReadSlot` the caller holds a ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_read_slot_status(slot: *mut HewReadSlot) -> i32 {
    if slot.is_null() {
        return ReadStatus::Error as i32;
    }
    // SAFETY: caller holds a ref.
    unsafe { (*slot).status.load(Ordering::Acquire) }
}

/// Take the deposited `bytes` value out of the slot, transferring ownership of
/// the buffer to the caller. Returns an empty triple when the status is not
/// `Data`. Idempotent in the sense that the slot's stored pointer is nulled so a
/// later `hew_read_slot_free` does not double-drop.
///
/// # Safety
///
/// `slot` must be a valid `HewReadSlot` the caller holds a ref to; called on the
/// resume edge after `enqueue_resume`, so the reactor's `Release` deposit
/// happens-before this `Acquire` read.
#[no_mangle]
pub unsafe extern "C" fn hew_read_slot_take(slot: *mut HewReadSlot) -> BytesTriple {
    let empty = BytesTriple {
        ptr: std::ptr::null_mut(),
        offset: 0,
        len: 0,
    };
    if slot.is_null() {
        return empty;
    }
    // SAFETY: caller holds a ref; the reactor's Release deposit on `status`
    // happens-before this Acquire load, so `value` is fully published.
    let s = unsafe { &*slot };
    if s.status.load(Ordering::Acquire) != ReadStatus::Data as i32 {
        return empty;
    }
    // Transfer ownership: read the triple and null the slot's pointer so the
    // final `hew_read_slot_free` does not drop the buffer again.
    let triple = s.value;
    // SAFETY: exclusive on the resume edge (single consumer); the reactor wrote
    // the deposit before waking and does not touch `value` after.
    unsafe {
        let value_ptr = std::ptr::addr_of!(s.value).cast_mut();
        (*value_ptr).ptr = std::ptr::null_mut();
        (*value_ptr).len = 0;
    }
    triple
}

/// Deposit a successful read result into the slot and report whether the waker
/// should fire. Called on the REACTOR thread.
///
/// Returns `true` if the caller should wake the parked continuation
/// (`enqueue_resume`), `false` if the slot was cancelled (abandon edge won the
/// race) — in which case the deposited buffer is dropped here and NO wake is
/// issued.
///
/// # Safety
///
/// `slot` must be a valid `HewReadSlot` the reactor holds a ref to. `triple`
/// (if non-empty) must own one refcount the slot takes over.
pub(crate) unsafe fn read_slot_deposit_data(slot: *mut HewReadSlot, triple: BytesTriple) -> bool {
    if slot.is_null() {
        if !triple.ptr.is_null() {
            // SAFETY: triple owns a refcount nobody else will take.
            unsafe { crate::bytes::hew_bytes_drop(triple.ptr) };
        }
        return false;
    }
    // SAFETY: reactor holds a ref.
    let s = unsafe { &*slot };
    if s.cancelled.load(Ordering::Acquire) != 0 {
        // Abandon edge already cancelled: drop the buffer, do not wake.
        if !triple.ptr.is_null() {
            // SAFETY: triple owns a refcount nobody else will take.
            unsafe { crate::bytes::hew_bytes_drop(triple.ptr) };
        }
        return false;
    }
    // Write the value BEFORE publishing the Data status (Release) so the resume
    // edge's Acquire load of `status` observes a fully-written `value`.
    // SAFETY: the reactor is the sole writer; the resume edge reads only after
    // the status Release below.
    unsafe {
        let value_ptr = std::ptr::addr_of!(s.value).cast_mut();
        (*value_ptr) = triple;
    }
    s.status.store(ReadStatus::Data as i32, Ordering::Release);
    true
}

/// Deposit a terminal status (EOF / error) into the slot and report whether the
/// waker should fire. Called on the REACTOR thread. Same cancellation contract
/// as [`read_slot_deposit_data`]; carries no buffer.
///
/// # Safety
///
/// `slot` must be a valid `HewReadSlot` the reactor holds a ref to.
pub(crate) unsafe fn read_slot_deposit_status(slot: *mut HewReadSlot, status: ReadStatus) -> bool {
    if slot.is_null() {
        return false;
    }
    // SAFETY: reactor holds a ref.
    let s = unsafe { &*slot };
    if s.cancelled.load(Ordering::Acquire) != 0 {
        return false;
    }
    s.status.store(status as i32, Ordering::Release);
    true
}

#[cfg(test)]
#[allow(
    clippy::undocumented_unsafe_blocks,
    reason = "test-only FFI calls; the slot lifecycle each exercises is described \
              in the test body, and every pointer is a fresh local slot/buffer"
)]
mod tests {
    use super::*;

    fn make_triple(bytes: &[u8]) -> BytesTriple {
        let len = u32::try_from(bytes.len()).unwrap();
        // SAFETY: bytes is valid for len; hew_bytes_from_static copies it.
        unsafe { crate::bytes::hew_bytes_from_static(bytes.as_ptr(), len) }
    }

    #[test]
    fn new_slot_is_pending_and_frees_clean() {
        let slot = hew_read_slot_new();
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Pending as i32
        );
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn deposit_data_then_take_transfers_bytes() {
        let slot = hew_read_slot_new();
        let triple = make_triple(b"hello");
        let wake = unsafe { read_slot_deposit_data(slot, triple) };
        assert!(wake, "non-cancelled deposit must signal a wake");
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Data as i32
        );
        let taken = unsafe { hew_read_slot_take(slot) };
        assert_eq!(taken.len, 5);
        assert!(!taken.ptr.is_null());
        // Take transferred ownership; free the buffer + the slot.
        unsafe { crate::bytes::hew_bytes_drop(taken.ptr) };
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn cancelled_deposit_drops_buffer_and_suppresses_wake() {
        let slot = hew_read_slot_new();
        unsafe { hew_read_slot_cancel(slot) };
        let triple = make_triple(b"abandoned");
        let wake = unsafe { read_slot_deposit_data(slot, triple) };
        assert!(!wake, "cancelled slot must not wake");
        // The buffer was dropped inside deposit; status stays Pending.
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Pending as i32
        );
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn abandon_after_data_deposit_frees_buffer_on_last_ref() {
        // Models the reactor depositing Data, then the handler abandoning before
        // it takes: the two-ref free sequence must not double-free, and the
        // still-present Data buffer must be released by the final free (proven
        // leak-clean + double-free-clean under the sanitizer suite). The refcount
        // ordering is the deterministic property asserted here.
        let slot = hew_read_slot_new();
        // Reactor takes its ref (refs = 2).
        unsafe { read_slot_retain(slot) };
        let triple = make_triple(b"unconsumed");
        assert!(unsafe { read_slot_deposit_data(slot, triple) });
        // Reactor drops its ref (refs = 1) — the slot is NOT freed yet (the
        // creator still holds a ref), so the Data buffer survives.
        unsafe { hew_read_slot_free(slot) };
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Data as i32,
            "the slot survives while the creator ref is held"
        );
        // Handler abandons without taking — the creator-ref free is the last ref;
        // it reclaims the box and drops the still-present Data buffer (no leak,
        // no double-free).
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn eof_status_deposits_without_buffer() {
        let slot = hew_read_slot_new();
        let wake = unsafe { read_slot_deposit_status(slot, ReadStatus::Eof) };
        assert!(wake);
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Eof as i32
        );
        let taken = unsafe { hew_read_slot_take(slot) };
        assert!(taken.ptr.is_null(), "non-Data take returns empty");
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn from_i32_round_trips_known_statuses() {
        assert_eq!(ReadStatus::from_i32(1), ReadStatus::Data);
        assert_eq!(ReadStatus::from_i32(2), ReadStatus::Eof);
        assert_eq!(ReadStatus::from_i32(3), ReadStatus::Error);
        assert_eq!(ReadStatus::from_i32(99), ReadStatus::Pending);
    }
}
