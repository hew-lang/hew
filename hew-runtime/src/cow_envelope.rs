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

/// Typed cleanup supplied by a message producer at admission.
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
    pub(crate) timer: Option<crate::timer_actor::Delivery>,
    /// Undelivered payload or lifecycle cleanup, consumed at dispatch entry.
    pub(crate) ingress_drop: Option<IngressDrop>,
    /// The same immutable request state retained by its optional reply owner.
    pub(crate) request: Option<std::sync::Arc<crate::request_state::RequestState>>,
    /// Canonical typed wrapper metadata, when supplied by a checked producer.
    pub(crate) layout: *const hew_cabi::value::HewValueLayout,
    pub(crate) value_owner: Option<crate::owned_value_budget::ValueOwner>,
    pub(crate) transfer: Option<EnvelopeTransfer>,
    pub(crate) transfer_waker: Option<crate::wake::OwnedWaker>,
    pub(crate) admission_pending: bool,
    code: Option<crate::code_owner::CodeRoot>,
}

#[derive(Debug)]
pub(crate) struct EnvelopeTransfer {
    pub(crate) target: crate::lifetime::live_actors::ActorIncarnation,
    pub(crate) value: crate::owned_value_budget::transfer::ValueTransfer,
    pub(crate) pending: bool,
}

/// The actual typed payload allocation, excluding envelope and RC metadata.
/// # Safety
/// The caller pins this unpublished or exclusively retained envelope.
pub(crate) unsafe extern "C" fn visit_managed(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the synchronous walk retains this exact envelope and its descriptor.
    unsafe {
        let envelope = &*slot.cast::<HewMsgEnvelope>();
        if envelope.payload.is_null() || envelope.layout.is_null() {
            return 0;
        }
        let layout = &*envelope.layout;
        let bytes = match crate::owned_value_budget::visit::slot_bytes(layout) {
            Ok(bytes) => bytes,
            Err(status) => return status,
        };
        match crate::owned_value_budget::visit::storage(visitor, envelope.payload, bytes) {
            hew_cabi::managed::DESCEND => {
                hew_cabi::managed::visit_value(layout, envelope.payload, visitor)
            }
            hew_cabi::managed::SKIP_DESCENDANTS => 0,
            failure => failure,
        }
    }
}

static ENVELOPE_MANAGED: hew_cabi::managed::HewManagedValueDescriptor =
    hew_cabi::managed::HewManagedValueDescriptor {
        slot_bytes: 0,
        visit: Some(visit_managed),
    };
pub(crate) static ENVELOPE_LAYOUT: hew_cabi::value::HewValueLayout =
    hew_cabi::value::HewValueLayout {
        size: size_of::<HewMsgEnvelope>(),
        align: align_of::<HewMsgEnvelope>(),
        ownership_kind: hew_cabi::value::HewTypeOwnershipKind::Plain,
        clone_fn: None,
        drop_fn: None,
        release_start: None,
        managed: &raw const ENVELOPE_MANAGED,
        code_owner: ptr::null(),
    };

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
        ptr::write(&raw mut (*env).request, None);
        (*env).layout = ptr::null();
        ptr::write(&raw mut (*env).value_owner, None);
        ptr::write(&raw mut (*env).transfer, None);
        ptr::write(&raw mut (*env).transfer_waker, None);
        (*env).admission_pending = false;
        ptr::write(&raw mut (*env).code, None);
    }
    env
}

/// Reserve an actor-owned wrapper before source fields are consumed. Failure
/// leaves both the shallow wrapper allocation and its fields with the caller.
/// # Safety
/// Payload and descriptor are the checked compiler's exact initialized wrapper.
pub(crate) unsafe fn seal_checked(
    payload: *mut c_void,
    size: usize,
    drop_payload: HewMsgEnvelopeDropFn,
    release: Option<hew_cabi::value::HewValueReleaseStart>,
    layout: *const hew_cabi::value::HewValueLayout,
    waker: *const crate::wake::HewWaker,
) -> Result<*mut HewMsgEnvelope, i32> {
    // SAFETY: the source lends its initialized wrapper until sealing commits.
    unsafe {
        let owner = crate::owned_value_budget::current_owner();
        let width = if owner.is_some() && !payload.is_null() {
            let layout = layout.as_ref().ok_or(hew_cabi::managed::UNDESCRIBED)?;
            crate::owned_value_budget::visit::slot_bytes(layout)?
        } else {
            0
        };
        let reservation = owner
            .as_ref()
            .map(|owner| owner.reserve_new(width))
            .transpose()
            .map_err(|_| hew_cabi::managed::HEAP_EXCEEDED)?;
        let envelope = crate::mailbox::hew_msg_envelope_new(payload, size, Some(drop_payload));
        if envelope.is_null() {
            return Err(hew_cabi::managed::HEAP_EXCEEDED);
        }
        (*envelope).release_start = release;
        set_layout(envelope, layout);
        (*envelope).transfer_waker =
            (!waker.is_null()).then(|| crate::wake::OwnedWaker::retain(&*waker));
        if let Some(reservation) = reservation {
            if let Err(status) = reservation.commit_value(&ENVELOPE_LAYOUT, envelope.cast()) {
                free_allocation(envelope);
                return Err(status);
            }
            if !payload.is_null() {
                owner
                    .as_ref()
                    .unwrap()
                    .release_value(&*layout, payload)
                    .unwrap_or_else(|_| std::process::abort());
            }
        }
        (*envelope).value_owner = owner;
        Ok(envelope)
    }
}

/// Return the shallow wrapper's fields to the original source message. Their
/// descendants stay owned; only this now-empty envelope allocation is retired.
/// # Safety
/// No admission committed, and the caller resumes ownership of every field.
pub(crate) unsafe fn return_fields(env: *mut HewMsgEnvelope) {
    // SAFETY: no receiver observed this unique wrapper or its typed fields.
    unsafe {
        (*env).transfer.take();
        if let Some(owner) = &(*env).value_owner {
            let bytes = crate::owned_value_budget::visit::slot_bytes(&*(*env).layout)
                .unwrap_or_else(|_| std::process::abort());
            owner.release_backing((*env).payload, bytes);
        }
        crate::mem::buf_free((*env).payload);
        free_allocation(env);
    }
}

/// Preserve canonical payload capacity until its children finish authored close.
unsafe fn payload_cursor(env: *mut HewMsgEnvelope) -> *mut crate::release_walker::HewReleaseCursor {
    unsafe fn free_payload(payload: *mut c_void) {
        // SAFETY: the cursor owns this original sized-block allocation.
        unsafe {
            crate::mem::buf_free(payload);
        }
    }
    // SAFETY: the unique envelope transfers initialized payload storage to the cursor.
    unsafe {
        let envelope = &*env;
        if let Some(layout) = envelope.layout.as_ref() {
            let bytes = if envelope.value_owner.is_some() {
                crate::owned_value_budget::visit::slot_bytes(layout)
                    .unwrap_or_else(|_| std::process::abort())
            } else {
                0
            };
            let cursor = crate::release_walker::HewReleaseCursor::new(vec![
                crate::release_walker::ReleaseItem::ManagedBacking {
                    pointer: envelope.payload,
                    bytes,
                    free: free_payload,
                },
                crate::release_walker::ReleaseItem::Value {
                    slot: envelope.payload,
                    layout: *layout,
                },
            ]);
            (*cursor).set_value_owner(envelope.value_owner.clone());
            cursor
        } else {
            crate::release_walker::HewReleaseCursor::payload(
                envelope.payload,
                envelope.payload_size,
                envelope.release_start.expect("selected payload release"),
                true,
            )
        }
    }
}

/// Attach the checked producer's canonical metadata before publication.
/// # Safety
/// The envelope is unpublished and the layout remains live through acquisition.
pub(crate) unsafe fn set_layout(
    env: *mut HewMsgEnvelope,
    layout: *const hew_cabi::value::HewValueLayout,
) {
    // SAFETY: the fresh envelope is uniquely owned; null denotes a static foreign producer.
    unsafe {
        if let Some(layout_ref) = layout.as_ref() {
            assert_eq!((*env).payload_size, layout_ref.size);
            assert!((*env).layout.is_null());
            (*env).code = Some(crate::code_owner::CodeRoot::acquire(layout_ref.code_owner));
            (*env).layout = layout;
            (*env).release_start = layout_ref.release_start;
        }
    }
}

/// Transfer code ownership with detached cleanup, after all generated callbacks.
unsafe fn transfer_code(
    env: *mut HewMsgEnvelope,
    cursor: *mut crate::release_walker::HewReleaseCursor,
) {
    // SAFETY: the unique envelope transfers its root to its fresh consuming cursor.
    unsafe {
        if let Some(code) = (*env).code.take() {
            crate::release_walker::HewReleaseCursor::after(
                cursor,
                crate::release_walker::ReleaseItem::CodeRoot(code),
            );
        }
    }
}

/// Free envelope storage after its payload and timer owners have been settled.
/// This runs Rust field destruction, including the shared request-state Arc.
/// # Safety
/// The envelope is exclusive; its payload storage is independently retired and
/// its one-shot timer has already been taken or was never attached.
pub(crate) unsafe fn free_allocation(env: *mut HewMsgEnvelope) {
    // SAFETY: the caller owns this initialized envelope allocation exclusively.
    unsafe {
        debug_assert!(
            (*env).timer.is_none(),
            "timer claim must settle before envelope retirement"
        );
        if let Some(state) = &(*env).request {
            state.withdraw();
        }
        ptr::drop_in_place(env);
        crate::mem::buf_free(env.cast());
    }
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
            (*env).transfer.take();
            drop((*env).timer.take());
            header_validate((*env).header_bits.load(Ordering::Acquire));
            if let Some(start) = (*env).release_start {
                let _ = start;
                let cursor = payload_cursor(env);
                transfer_code(env, cursor);
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
                free_allocation(env);
                return;
            }
            if let Some(drop) = (*env).ingress_drop.take() {
                (drop.callback)(drop.message, (*env).payload, (*env).payload_size);
            }
            if !(*env).payload.is_null() {
                if let Some(owner) = &(*env).value_owner {
                    owner
                        .release_value(&ENVELOPE_LAYOUT, env.cast())
                        .unwrap_or_else(|_| std::process::abort());
                }
                if let Some(layout) = (*env).layout.as_ref() {
                    if let Some(drop) = layout.drop_fn {
                        drop((*env).payload);
                    }
                } else if let Some(drop) = (*env).drop_glue {
                    drop((*env).payload);
                }
            }
            if !(*env).payload.is_null() {
                crate::mem::buf_free((*env).payload); // ALLOCATOR-PAIRING: GlobalAlloc
            }
            free_allocation(env); // ALLOCATOR-PAIRING: GlobalAlloc
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
        (*env).transfer.take();
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
            let _ = start;
            let cursor = payload_cursor(env);
            transfer_code(env, cursor);
            free_allocation(env);
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
    fn checked_seal_refusal_preserves_source_fields_and_exact_payload_charge() {
        let layout = &crate::layout_intrinsics::hew_layout_val_string;
        // SAFETY: each branch owns one initialized string and its shallow wrapper.
        unsafe {
            for limit in [13, 14] {
                let owner = crate::owned_value_budget::ValueOwner::new(Some(limit));
                let text = hew_cabi::string::string_from_str("retain");
                let payload = alloc(size_of_val(&text));
                payload
                    .cast::<*mut hew_cabi::string::HewString>()
                    .write(text);
                owner.acquire_value(layout, payload).unwrap().commit();
                let sealed =
                    crate::owned_value_budget::with_value_owner(&Some(owner.clone()), || {
                        seal_checked(payload, layout.size, drop_probe, None, layout, ptr::null())
                    });
                if limit == 13 {
                    assert!(matches!(sealed, Err(hew_cabi::managed::HEAP_EXCEEDED)));
                    assert_eq!(
                        payload.cast::<*mut hew_cabi::string::HewString>().read(),
                        text
                    );
                    assert_eq!(owner.live_bytes(), 6);
                    owner.release_value(layout, payload).unwrap();
                    hew_cabi::string::string_release(text);
                    crate::mem::buf_free(payload);
                } else {
                    let env = sealed.unwrap();
                    assert_eq!(owner.live_bytes(), 14);
                    release(env);
                    assert_eq!(owner.live_bytes(), 0);
                }
            }
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
    fn cancelled_request_survives_reply_owner_until_envelope_retirement() {
        let _runtime = crate::runtime_test_guard();
        // SAFETY: the test owns both unpublished allocations, then retires the
        // reply owner before claiming and releasing the still-owned envelope.
        unsafe {
            let channel = crate::reply_channel::hew_reply_channel_new();
            crate::reply_channel::configure_native_request(
                channel,
                crate::request_deadline::RequestDeadline::default(),
                #[cfg(not(target_arch = "wasm32"))]
                None,
            );
            let env = new(ptr::null_mut(), 0, None, alloc);
            assert!(!env.is_null());
            crate::request_state::bind_reply(env, channel);
            let state = crate::reply_channel::native_request_state(channel).unwrap();
            let lifetime = std::sync::Arc::downgrade(&state);
            drop(state);
            crate::reply_channel::withdraw_native_request(channel);
            crate::reply_channel::hew_reply_channel_free(channel);
            assert!(lifetime.upgrade().is_some());
            assert!(!crate::request_state::claim_message(env, ptr::null_mut()));
            release(env);
            assert!(lifetime.upgrade().is_none());
        }
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
