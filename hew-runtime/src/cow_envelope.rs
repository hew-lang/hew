//! Target-neutral owned mailbox envelope representation and lifecycle.
//!
//! Native and WASM mailboxes deliberately keep different queue and scheduling
//! implementations. The payload envelope, however, is ABI-identical and has
//! one lifecycle, so this module is its sole implementation authority.

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

use crate::mailbox_header::header_validate;

/// Drop glue invoked once, before the envelope releases its payload allocation.
pub type HewMsgEnvelopeDropFn = unsafe extern "C" fn(*mut c_void);

/// Typed cleanup supplied by a raw C message producer at admission.
#[derive(Clone, Copy)]
pub(crate) struct IngressDrop {
    pub message: i32,
    pub callback: unsafe extern "C" fn(i32, *mut c_void, usize),
}

/// Owned container for an actor message payload, with a stable payload prefix.
#[repr(C)]
pub struct HewMsgEnvelope {
    /// Live ownership references; admitted payloads are exclusive.
    pub refcount: AtomicUsize,
    /// Envelope contract bits; see [`crate::mailbox_header`].
    pub header_bits: AtomicU32,
    /// Heap-allocated payload bytes.
    pub payload: *mut c_void,
    /// Size of `payload` in bytes.
    pub payload_size: usize,
    /// Optional typed destructor run before the payload allocation is freed.
    pub drop_glue: Option<HewMsgEnvelopeDropFn>,
    /// Checked consuming destructor for native payloads that may run user code.
    pub release_start: Option<hew_cabi::value::HewValueReleaseStart>,
    /// Exact receiver owning an admitted native payload, zero before admission.
    pub release_actor: u64,
    pub release_serial: u64,
    /// Queued ask sender reference retired after its payload finishes cleanup.
    pub release_reply: *mut c_void,
    /// One-shot start/loss claim, attached only after the timer releases its payload.
    pub(crate) timer: Option<std::sync::Arc<crate::timer_actor::OneShot>>,
    /// Raw C adapter cleanup, consumed when its dispatch adapter claims fields.
    pub(crate) ingress_drop: Option<IngressDrop>,
}

impl std::fmt::Debug for HewMsgEnvelope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewMsgEnvelope")
            .field("refcount", &self.refcount.load(Ordering::Relaxed))
            .field("header_bits", &self.header_bits.load(Ordering::Relaxed))
            .field("payload", &self.payload)
            .field("payload_size", &self.payload_size)
            .field("drop_glue_set", &self.drop_glue.is_some())
            .finish_non_exhaustive()
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
/// `payload` must be a sized-block allocation from the sized-block allocator
/// (or null for a zero-sized payload). Ownership transfers to the returned
/// envelope.
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
        (*env).release_start = None;
        (*env).release_actor = 0;
        (*env).release_serial = 0;
        (*env).release_reply = ptr::null_mut();
        ptr::write(&raw mut (*env).timer, None);
        (*env).ingress_drop = None;
    }
    env
}

/// Release one observer, destroying the payload and envelope on the final one.
///
/// # Safety
///
/// `env` must be null or a live envelope for which the caller owns a reference.
/// # Panics
/// Panics if a native payload no longer has its registered receiver cleanup owner.
pub unsafe fn release(env: *mut HewMsgEnvelope) {
    if env.is_null() {
        return;
    }
    // SAFETY: the caller guarantees a live envelope reference.
    unsafe {
        let prev = (*env).refcount.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(prev >= 1, "release on a zero-count envelope");
        if prev == 1 {
            if let Some(timer) = (*env).timer.take() {
                timer.discarded();
            }
            header_validate((*env).header_bits.load(Ordering::Acquire));
            if let Some(start) = (*env).release_start {
                let cursor = crate::release_walker::HewReleaseCursor::payload(
                    (*env).payload,
                    (*env).payload_size,
                    start,
                    true,
                );
                if !(*env).release_reply.is_null() {
                    unsafe fn retire_reply(reply: *mut c_void) {
                        // SAFETY: the cursor owns the queued sender reference.
                        unsafe {
                            crate::reply_channel::hew_reply_channel_retire_orphaned_ask_sender_ref(
                                reply.cast(),
                            );
                        };
                    }
                    crate::release_walker::HewReleaseCursor::after(
                        cursor,
                        crate::release_walker::ReleaseItem::Storage {
                            owner: (*env).release_reply,
                            free: retire_reply,
                        },
                    );
                }
                let target = crate::lifetime::live_actors::ActorIncarnation::from_parts(
                    (*env).release_actor,
                    (*env).release_serial,
                );
                if target.is_none() {
                    std::process::abort();
                }
                let queued = crate::lifetime::live_actors::with_live_incarnation(target, |pin| {
                    let actor = pin.actor();
                    let completion = actor
                        .native_completion
                        .as_ref()
                        .expect("native payload owner");
                    completion.cleanup.enqueue(cursor);
                    crate::actor_native::cleanup::wake(actor);
                });
                if queued.is_none() {
                    std::process::abort();
                }
                crate::mem::buf_free(env.cast());
                return;
            }
            if let Some(drop) = (*env).ingress_drop.take() {
                (drop.callback)(drop.message, (*env).payload, (*env).payload_size);
            }
            if let Some(drop_fn) = (*env).drop_glue {
                if !(*env).payload.is_null() {
                    drop_fn((*env).payload);
                }
            }
            if !(*env).payload.is_null() {
                crate::mem::buf_free((*env).payload); // ALLOCATOR-PAIRING: GlobalAlloc
            }
            crate::mem::buf_free(env.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }
}

/// Detach an unadmitted native payload for its caller's consuming cleanup.
/// # Safety
/// The caller transfers its envelope reference; no admitted receiver owns it.
pub(crate) unsafe fn release_cursor(
    env: *mut HewMsgEnvelope,
) -> *mut crate::release_walker::HewReleaseCursor {
    if env.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the caller transfers one live reference and retains no payload loan.
    unsafe {
        assert_eq!(
            (*env).release_actor,
            0,
            "admitted payload belongs to its receiver"
        );
        if let Some(start) = (*env).release_start {
            let previous = (*env).refcount.fetch_sub(1, Ordering::AcqRel);
            assert!(previous > 0);
            if previous != 1 {
                return ptr::null_mut();
            }
            let cursor = crate::release_walker::HewReleaseCursor::payload(
                (*env).payload,
                (*env).payload_size,
                start,
                true,
            );
            crate::mem::buf_free(env.cast());
            cursor
        } else {
            release(env);
            ptr::null_mut()
        }
    }
}

/// Consume an unadmitted sealed request through its selected release recipe.
/// # Safety
/// The caller transfers one envelope reference and retains no payload loan.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_envelope_release_begin(
    env: *mut HewMsgEnvelope,
) -> *mut crate::release_walker::HewReleaseCursor {
    // SAFETY: the source owner transfers the unadmitted request.
    unsafe { release_cursor(env) }
}

/// Attach receiver lifetime accounting before publishing a native payload.
/// # Safety
/// The envelope is unpublished and the actor is pinned through admission.
pub(crate) unsafe fn bind_receiver(
    env: *mut HewMsgEnvelope,
    actor: &crate::actor::HewActor,
) -> bool {
    // SAFETY: admission owns this unpublished envelope through the attempt.
    unsafe {
        if (*env).release_start.is_none() {
            return true;
        }
        assert_eq!((*env).release_actor, 0);
        let Some(completion) = &actor.native_completion else {
            return false;
        };
        if !completion.cleanup.register() {
            return false;
        }
        (*env).release_actor = actor.id;
        (*env).release_serial = actor.spawn_serial;
        true
    }
}

/// Failed admission leaves the caller's payload independent of receiver close.
/// # Safety
/// Admission still owns the unpublished envelope and pins the same actor.
pub(crate) unsafe fn unbind_receiver(env: *mut HewMsgEnvelope, actor: &crate::actor::HewActor) {
    // SAFETY: failed admission has not published this envelope.
    unsafe {
        if (*env).release_actor == 0 {
            return;
        }
        assert_eq!((*env).release_actor, actor.id);
        (*env).release_actor = 0;
        (*env).release_serial = 0;
        actor
            .native_completion
            .as_ref()
            .expect("native receiver")
            .cleanup
            .retire_without_release();
        crate::actor_native::cleanup::wake(actor);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mailbox_header::HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK;
    use std::panic::{catch_unwind, AssertUnwindSafe};
    use std::sync::Mutex;

    static CORE_TEST_LOCK: Mutex<()> = Mutex::new(());
    static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn drop_probe(_payload: *mut c_void) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    fn alloc(size: usize) -> *mut c_void {
        crate::mem::buf_try_alloc(size) // ALLOCATOR-PAIRING: GlobalAlloc
    }

    fn payload(bytes: &[u8]) -> *mut c_void {
        // SAFETY: the block owns `bytes.len()` writable bytes; the input is a
        // valid slice of that length.
        unsafe {
            let allocation = crate::mem::buf_try_alloc(bytes.len()); // ALLOCATOR-PAIRING: GlobalAlloc
            assert!(!allocation.is_null());
            ptr::copy_nonoverlapping(bytes.as_ptr(), allocation.cast::<u8>(), bytes.len());
            allocation
        }
    }

    #[test]
    fn layout_fingerprint_preserves_the_payload_prefix() {
        let word = std::mem::size_of::<usize>();
        assert_eq!(
            std::mem::align_of::<HewMsgEnvelope>(),
            word.max(align_of::<u64>())
        );
        assert!(std::mem::size_of::<HewMsgEnvelope>() >= 7 * word + 2 * size_of::<u64>());
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, refcount), 0);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, header_bits), word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, payload), 2 * word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, payload_size), 3 * word);
        assert_eq!(std::mem::offset_of!(HewMsgEnvelope, drop_glue), 4 * word);
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
        // SAFETY: the test owns the payload and its envelope.
        unsafe {
            let env = new(payload(b"drop"), 4, Some(drop_probe), alloc);
            release(env);
        }
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1);
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
