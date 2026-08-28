//! Target-neutral COW mailbox envelope representation and lifecycle.
//!
//! Native and WASM mailboxes deliberately keep different queue and scheduling
//! implementations. The payload envelope, however, is ABI-identical and has
//! one lifecycle, so this module is its sole implementation authority.

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

use crate::mailbox_header::{
    header_validate, HEW_MSG_ENVELOPE_ALIAS_ACTIVE, HEW_MSG_ENVELOPE_FORKED,
};

/// Drop glue invoked once, before the envelope releases its payload allocation.
pub type HewMsgEnvelopeDropFn = unsafe extern "C" fn(*mut c_void);

/// ABI-stable, refcounted COW container for an actor message payload.
#[repr(C)]
pub struct HewMsgEnvelope {
    /// Number of live observers.
    pub refcount: AtomicUsize,
    /// Envelope contract bits; see [`crate::mailbox_header`].
    pub header_bits: AtomicU32,
    /// Heap-allocated payload bytes.
    pub payload: *mut c_void,
    /// Size of `payload` in bytes.
    pub payload_size: usize,
    /// Optional typed destructor run before the payload allocation is freed.
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

// SAFETY: only atomic fields mutate after construction; payload is read-only
// while an observer exists and is dropped only by the final releaser.
unsafe impl Send for HewMsgEnvelope {}
// SAFETY: see the `Send` implementation above.
unsafe impl Sync for HewMsgEnvelope {}

/// Target mailbox allocator, retained as an argument so target test hooks and
/// allocation policies remain owned by their respective mailbox modules.
pub type EnvelopeAlloc = fn(usize) -> *mut c_void;

/// Allocate an envelope with one observer.
///
/// # Safety
///
/// `payload` must be allocated by `libc::malloc` (or be null for a zero-sized
/// payload). Ownership transfers to the returned envelope.
pub unsafe fn new(
    payload: *mut c_void,
    payload_size: usize,
    drop_glue: Option<HewMsgEnvelopeDropFn>,
    allocate: EnvelopeAlloc,
) -> *mut HewMsgEnvelope {
    let env = allocate(std::mem::size_of::<HewMsgEnvelope>()).cast::<HewMsgEnvelope>();
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: `env` is a fresh allocation with suitable size and alignment.
    unsafe {
        ptr::write(&raw mut (*env).refcount, AtomicUsize::new(1));
        ptr::write(&raw mut (*env).header_bits, AtomicU32::new(0));
        (*env).payload = payload;
        (*env).payload_size = payload_size;
        (*env).drop_glue = drop_glue;
    }
    env
}

/// Add an alias observer and return the same envelope pointer.
///
/// # Safety
///
/// `env` must be live and the caller must own one reference.
pub unsafe fn clone_alias(env: *mut HewMsgEnvelope) -> *mut HewMsgEnvelope {
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the caller guarantees a live envelope.
    unsafe {
        let prev = (*env).refcount.fetch_add(1, Ordering::Relaxed);
        debug_assert!(prev >= 1, "clone_alias on a released envelope");
        (*env)
            .header_bits
            .fetch_or(HEW_MSG_ENVELOPE_ALIAS_ACTIVE, Ordering::Relaxed);
    }
    env
}

/// Release one observer, destroying the payload and envelope on the final one.
///
/// # Safety
///
/// `env` must be null or a live envelope for which the caller owns a reference.
pub unsafe fn release(env: *mut HewMsgEnvelope) {
    if env.is_null() {
        return;
    }
    // SAFETY: the caller guarantees a live envelope reference.
    unsafe {
        let prev = (*env).refcount.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(prev >= 1, "release on a zero-count envelope");
        if prev == 1 {
            header_validate((*env).header_bits.load(Ordering::Acquire));
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

/// Return a read-only borrowed payload pointer, or null for a null envelope.
///
/// # Safety
///
/// `env` must be null or live; the result remains valid while a reference exists.
pub unsafe fn payload_ptr(env: *mut HewMsgEnvelope) -> *mut c_void {
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the caller guarantees a live envelope.
    unsafe { (*env).payload }
}

/// Fork an envelope into a private single-observer payload copy.
///
/// # Safety
///
/// `env` must be live and the caller transfers one owned reference to this call.
pub unsafe fn fork_for_write(
    env: *mut HewMsgEnvelope,
    allocate: EnvelopeAlloc,
) -> *mut HewMsgEnvelope {
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the caller guarantees a live envelope and the payload alias
    // contract permits this read-only snapshot.
    let (payload_size, drop_glue, src_payload, src_bits) = unsafe {
        (
            (*env).payload_size,
            (*env).drop_glue,
            (*env).payload,
            (*env).header_bits.load(Ordering::Relaxed),
        )
    };
    let new_payload = if payload_size > 0 && !src_payload.is_null() {
        let buf = allocate(payload_size);
        if buf.is_null() {
            return ptr::null_mut();
        }
        // SAFETY: `buf` is fresh and `src_payload` is readable under the alias contract.
        unsafe { libc::memcpy(buf, src_payload, payload_size) };
        buf
    } else {
        ptr::null_mut()
    };
    // SAFETY: `new_payload` is a fresh compatible allocation.
    let forked = unsafe { new(new_payload, payload_size, drop_glue, allocate) };
    if forked.is_null() {
        if !new_payload.is_null() {
            // SAFETY: this unpublished allocation belongs to this function.
            unsafe { libc::free(new_payload) };
        }
        return ptr::null_mut();
    }
    let inherited_bits = (src_bits & !HEW_MSG_ENVELOPE_ALIAS_ACTIVE) | HEW_MSG_ENVELOPE_FORKED;
    // SAFETY: `forked` was created above and is live.
    unsafe {
        (*forked)
            .header_bits
            .fetch_or(inherited_bits, Ordering::Relaxed);
        release(env);
    }
    forked
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mailbox_header::{
        HEW_MSG_ENVELOPE_ARENA_BACKED, HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER,
        HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK, HEW_MSG_ENVELOPE_RESERVED_DELTA_A,
        HEW_MSG_ENVELOPE_RESERVED_GAMMA_B, HEW_MSG_ENVELOPE_SHARED_FROZEN,
    };
    use std::panic::{catch_unwind, AssertUnwindSafe};
    use std::sync::Mutex;

    static CORE_TEST_LOCK: Mutex<()> = Mutex::new(());
    static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
    static ALLOC_CALLS: AtomicUsize = AtomicUsize::new(0);
    static FAIL_ALLOC_ON_CALL: AtomicUsize = AtomicUsize::new(usize::MAX);

    unsafe extern "C" fn drop_probe(_payload: *mut c_void) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    fn alloc(size: usize) -> *mut c_void {
        // SAFETY: the shared lifecycle requires a malloc-compatible allocator.
        unsafe { libc::malloc(size) }
    }

    fn fail_on_selected_call(size: usize) -> *mut c_void {
        let call = ALLOC_CALLS.fetch_add(1, Ordering::SeqCst);
        if call == FAIL_ALLOC_ON_CALL.load(Ordering::SeqCst) {
            return ptr::null_mut();
        }
        // SAFETY: the shared lifecycle requires a malloc-compatible allocator.
        unsafe { libc::malloc(size) }
    }

    fn payload(bytes: &[u8]) -> *mut c_void {
        // SAFETY: malloc returns a buffer of `bytes.len()` bytes or null; tests
        // refuse OOM before copying the known-valid input bytes.
        unsafe {
            let allocation = libc::malloc(bytes.len());
            assert!(!allocation.is_null());
            ptr::copy_nonoverlapping(bytes.as_ptr(), allocation.cast::<u8>(), bytes.len());
            allocation
        }
    }

    #[test]
    fn layout_fingerprint_is_a_five_word_c_abi_record() {
        let word = std::mem::size_of::<usize>();
        assert_eq!(std::mem::align_of::<HewMsgEnvelope>(), word);
        assert_eq!(std::mem::size_of::<HewMsgEnvelope>(), 5 * word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, refcount), 0);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, header_bits), word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, payload), 2 * word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, payload_size), 3 * word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, drop_glue), 4 * word);
    }

    #[test]
    fn alias_refcount_starts_at_one_and_sets_the_alias_bit() {
        let _serial = CORE_TEST_LOCK.lock().unwrap();
        // SAFETY: the test owns the payload and every envelope reference.
        unsafe {
            let env = new(payload(b"alias"), 5, None, alloc);
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);
            assert_eq!((*env).header_bits.load(Ordering::SeqCst), 0);
            assert_eq!(clone_alias(env), env);
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 2);
            assert_ne!(
                (*env).header_bits.load(Ordering::SeqCst) & HEW_MSG_ENVELOPE_ALIAS_ACTIVE,
                0
            );
            release(env);
            release(env);
        }
    }

    #[test]
    fn null_payload_is_borrowable_and_never_calls_drop_glue() {
        let _serial = CORE_TEST_LOCK.lock().unwrap();
        DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the test owns the null-payload envelope reference.
        unsafe {
            let env = new(ptr::null_mut(), 0, Some(drop_probe), alloc);
            assert!(!env.is_null());
            assert!(payload_ptr(env).is_null());
            release(env);
        }
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn final_release_runs_drop_glue_exactly_once() {
        let _serial = CORE_TEST_LOCK.lock().unwrap();
        DROP_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: the test owns the payload and both alias references.
        unsafe {
            let env = new(payload(b"drop"), 4, Some(drop_probe), alloc);
            clone_alias(env);
            release(env);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0);
            release(env);
        }
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn fork_oom_rolls_back_without_consuming_the_source_reference() {
        let _serial = CORE_TEST_LOCK.lock().unwrap();
        DROP_COUNT.store(0, Ordering::SeqCst);
        ALLOC_CALLS.store(0, Ordering::SeqCst);
        FAIL_ALLOC_ON_CALL.store(usize::MAX, Ordering::SeqCst);
        // SAFETY: the test owns the payload and both alias references.
        unsafe {
            let env = new(
                payload(b"rollback"),
                8,
                Some(drop_probe),
                fail_on_selected_call,
            );
            assert!(!env.is_null());
            clone_alias(env);
            ALLOC_CALLS.store(0, Ordering::SeqCst);
            // The payload copy succeeds, then allocating its envelope fails.
            FAIL_ALLOC_ON_CALL.store(1, Ordering::SeqCst);
            assert!(fork_for_write(env, fail_on_selected_call).is_null());
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 2);
            assert_eq!(
                std::slice::from_raw_parts((*env).payload.cast::<u8>(), 8),
                b"rollback"
            );
            release(env);
            release(env);
        }
        FAIL_ALLOC_ON_CALL.store(usize::MAX, Ordering::SeqCst);
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn fork_copies_payload_and_inherits_only_non_alias_bits() {
        let _serial = CORE_TEST_LOCK.lock().unwrap();
        DROP_COUNT.store(0, Ordering::SeqCst);
        let inherited = HEW_MSG_ENVELOPE_SHARED_FROZEN
            | HEW_MSG_ENVELOPE_ARENA_BACKED
            | HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER
            | HEW_MSG_ENVELOPE_RESERVED_GAMMA_B
            | HEW_MSG_ENVELOPE_RESERVED_DELTA_A;
        // SAFETY: the test owns the payload and both resulting envelope references.
        unsafe {
            let env = new(payload(b"fork"), 4, Some(drop_probe), alloc);
            clone_alias(env);
            (*env).header_bits.fetch_or(inherited, Ordering::SeqCst);
            let forked = fork_for_write(env, alloc);
            assert!(!forked.is_null());
            assert_ne!((*forked).payload, (*env).payload);
            assert_eq!((*forked).refcount.load(Ordering::SeqCst), 1);
            assert_eq!(
                (*forked).header_bits.load(Ordering::SeqCst),
                inherited | HEW_MSG_ENVELOPE_FORKED
            );
            *(*forked).payload.cast::<u8>() = b'F';
            assert_eq!(
                std::slice::from_raw_parts((*forked).payload.cast::<u8>(), 4),
                b"Fork"
            );
            assert_eq!(
                std::slice::from_raw_parts((*env).payload.cast::<u8>(), 4),
                b"fork"
            );
            release(forked);
            release(env);
        }
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn reserved_bits_refuse_final_release() {
        let _serial = CORE_TEST_LOCK.lock().unwrap();
        // SAFETY: this test restores the live envelope after observing the
        // fail-closed panic so it can release the allocation normally.
        unsafe {
            let env = new(ptr::null_mut(), 0, None, alloc);
            (*env)
                .header_bits
                .store(HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK, Ordering::SeqCst);
            assert!(catch_unwind(AssertUnwindSafe(|| release(env))).is_err());
            (*env).refcount.store(1, Ordering::SeqCst);
            (*env).header_bits.store(0, Ordering::SeqCst);
            release(env);
        }
    }
}
