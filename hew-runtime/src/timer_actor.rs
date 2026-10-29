//! Actor-owned one-shot requests on the shared timer wheel.

use std::ffi::c_void;
use std::ptr::{self, NonNull};
use std::sync::{Arc, Mutex};

use crate::lifetime::live_actors::ActorIncarnation;
use crate::mailbox::HewMsgEnvelope;
use crate::timer_deadline::Deadline;
use crate::timer_wheel::{HewTimerHandle, HewTimerWheel};

mod protocol;
use protocol::{Firing, Request as Protocol};

#[derive(Debug)]
struct Request {
    protocol: Protocol<NonNull<HewMsgEnvelope>>,
    pending: Option<HewTimerHandle>,
}

pub(crate) type Delivery = protocol::Delivery<Arc<OneShot>>;

// Keep the envelope's private timer seat one pointer wide on every target.
const _: () = assert!(size_of::<Option<Delivery>>() == size_of::<Option<Arc<OneShot>>>());

impl protocol::DeliveryOwner for Arc<OneShot> {
    fn start(&self) -> bool {
        self.started()
    }
    fn discarded(&self) {
        OneShot::discarded(self);
    }
}

/// One owner arbitrates firing, handler start, cancellation and overdue.
/// The registry owns the request independently of the source cancellation handle.
#[derive(Debug)]
pub struct OneShot {
    actor: ActorIncarnation,
    registry_key: usize,
    message: i32,
    wheel: *mut HewTimerWheel,
    overdue_ns: Option<u64>,
    request: Mutex<Request>,
}

// SAFETY: the request mutex owns its envelope and wheel handle. Actor access
// always acquires a live-incarnation pin; no callback retains an actor pointer.
unsafe impl Send for OneShot {}
// SAFETY: all mutable state is protected by the request mutex.
unsafe impl Sync for OneShot {}

impl OneShot {
    fn unregister(&self) {
        crate::timer_periodic::unregister_timer(
            self.registry_key as *mut crate::actor::HewActor,
            ptr::from_ref(self) as usize,
        );
    }

    fn remove_pending(&self, request: &mut Request) {
        if let Some(handle) = request.pending.take() {
            // SAFETY: the handle names this wheel's entry. Removal and firing
            // arbitrate ownership of the one raw Arc reference in the wheel.
            let data = unsafe {
                crate::timer_wheel::hew_timer_wheel_remove(
                    self.wheel,
                    handle.entry,
                    handle.generation,
                )
            };
            if !data.is_null() {
                // SAFETY: successful removal returned the wheel's owned Arc.
                drop(unsafe { Arc::from_raw(data.cast::<Self>()) });
            }
        }
    }

    fn arm(self: &Arc<Self>, request: &mut Request, deadline: Deadline) -> bool {
        let raw = Arc::into_raw(Arc::clone(self)).cast_mut().cast();
        // SAFETY: the wheel lives through timer quiescence; raw owns one Arc.
        let handle = unsafe {
            crate::timer_wheel::timer_wheel_schedule_precise_handle(
                self.wheel, deadline, fired, raw,
            )
        };
        if handle.entry.is_null() {
            // SAFETY: failed scheduling did not take the supplied reference.
            drop(unsafe { Arc::from_raw(raw.cast::<Self>()) });
            false
        } else {
            request.pending = Some(handle);
            true
        }
    }

    /// Before firing, withdraw delivery; afterwards, disarm only overdue.
    pub(crate) fn cancel(&self) {
        let envelope = {
            let mut request = self
                .request
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let envelope = request.protocol.cancel();
            self.remove_pending(&mut request);
            envelope.map_or(ptr::null_mut(), NonNull::as_ptr)
        };
        if !envelope.is_null() {
            // SAFETY: the timer owns this receiver-bound pending envelope.
            // Consuming user cleanup is queued for the actor, never run here.
            unsafe { crate::cow_envelope::release(envelope) };
        }
        self.unregister();
    }

    /// Called at the mailbox's handler-start claim, before invoking user code.
    fn started(&self) -> bool {
        let mut request = self
            .request
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let claim = request.protocol.start(crate::timer_wheel::now_for_wheel());
        self.remove_pending(&mut request);
        if claim.fault {
            // Keep the claim registered until cancellation can observe that
            // its winning fault has reached the actor's terminal-request latch.
            self.fault_owner();
        }
        drop(request);
        self.unregister();
        claim.allowed
    }

    /// Every envelope loss before handler start passes through this claim.
    fn discarded(&self) {
        self.overdue(true);
    }

    fn overdue(&self, discarded: bool) {
        let fault = {
            let mut request = self
                .request
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let Some(fault) = request.protocol.lost(self.overdue_ns.is_some(), discarded) else {
                return;
            };
            self.remove_pending(&mut request);
            // Cancellation uses this same mutex. It cannot finish terminal
            // cleanup before the winning fault has reached the actor owner.
            if fault {
                self.fault_owner();
            }
            fault
        };
        self.unregister();
        if discarded && !fault {
            crate::log_core::runtime_warning(
                "actor.timer_discarded: the one-shot handler did not start",
            );
        }
    }

    fn fault_owner(&self) {
        crate::lifetime::live_actors::with_live_incarnation(self.actor, |pin| {
            // SAFETY: this exact incarnation remains pinned through the
            // cooperative fault handoff; no callback touches its frame.
            unsafe {
                crate::actor::hew_actor_trap(
                    pin.as_ptr(),
                    crate::internal::types::HEW_TRAP_ACTOR_TIMER_OVERDUE,
                );
            }
        });
    }
}

unsafe extern "C" fn fired(data: *mut c_void) {
    // SAFETY: the wheel transfers the raw Arc for this fired entry exactly once.
    let timer = unsafe { Arc::from_raw(data.cast::<OneShot>()) };
    let envelope = {
        let mut request = timer
            .request
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        request.pending = None;
        match request.protocol.fire() {
            Firing::Deliver(envelope) => {
                let envelope = envelope.as_ptr();
                if let Some(overdue) = timer.overdue_ns {
                    let deadline = crate::timer_wheel::now_for_wheel()
                        .checked_add(std::time::Duration::from_nanos(overdue))
                        .expect("nanosecond durations fit the wheel");
                    request.protocol.arm_overdue(deadline);
                    if !timer.arm(&mut request, deadline) {
                        drop(request);
                        timer.overdue(false);
                        // SAFETY: firing took the receiver-bound envelope.
                        unsafe { crate::cow_envelope::release(envelope) };
                        return;
                    }
                }
                envelope
            }
            Firing::Deadline => {
                drop(request);
                timer.overdue(false);
                return;
            }
            Firing::Ignore => return,
        }
    };
    // The timer no longer owns the envelope, so this reference cannot form a
    // cycle. Its final release observes loss unless dispatch already claimed it.
    // SAFETY: firing exclusively owns the envelope until mailbox publication.
    unsafe { (*envelope).timer = Some(Delivery::new(Arc::clone(&timer))) };
    let outcome = crate::lifetime::live_actors::with_live_incarnation(timer.actor, |pin| {
        // SAFETY: the timer transfers its bound envelope only on admission.
        unsafe {
            crate::actor::submit_bound_native_envelope(
                pin.as_ptr(),
                timer.message,
                envelope,
                crate::actor_caller::HewCallerStamp::timer(),
            )
        }
    });
    if !matches!(
        outcome,
        Some(
            crate::mailbox::SendOutcome::Enqueued
                | crate::mailbox::SendOutcome::Dropped
                | crate::mailbox::SendOutcome::Coalesced
        )
    ) {
        // SAFETY: refusal retains the timer's bound payload ownership.
        unsafe { crate::cow_envelope::release(envelope) };
    }
}

/// Schedule a checked one-shot request. A non-null result owns one handle Arc.
/// On failure the caller retains the typed payload fields; this function frees
/// only the unpublished shallow wrapper and envelope.
/// # Safety
/// The wrapper and callbacks match the checked handler protocol. The target is
/// the current actor, and the caller retains all typed fields until success.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_timer_new(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    payload_release: Option<hew_cabi::value::HewValueReleaseStart>,
    after_ns: i64,
    overdue_ns: i64,
    has_overdue: bool,
    invocation: *const crate::coro_state::HewCoroState,
    payload_layout: *const hew_cabi::value::HewValueLayout,
) -> *const OneShot {
    // SAFETY: the caller supplies one unpublished wrapper and its exact destructor.
    let envelope =
        unsafe { crate::mailbox::hew_msg_envelope_new(payload, size, Some(drop_payload)) };
    if envelope.is_null() {
        // SAFETY: typed fields remain with the source on failure.
        unsafe { crate::mem::buf_free(payload) };
        return ptr::null();
    }
    // SAFETY: the unpublished envelope belongs to this call.
    unsafe {
        (*envelope).release_start = payload_release;
        crate::cow_envelope::set_layout(envelope, payload_layout);
    };
    let result = crate::timer_periodic::admit_timer(|| {
        let id = crate::lifetime::local_handles::resolve_current_actor(token)?;
        // SAFETY: generated code retains this checked invocation through scheduling.
        if unsafe { invocation.as_ref() }
            .is_none_or(|state| state.execution().actor_turn.actor_id() != id)
        {
            return None;
        }
        crate::lifetime::live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the send guard pins the actor through registration.
            let target = unsafe { &*actor };
            let wheel = crate::timer_periodic::global_wheel();
            if wheel.is_null() {
                return None;
            }
            let deadline = crate::timer_wheel::now_for_wheel().checked_add(
                std::time::Duration::from_nanos(u64::try_from(after_ns).unwrap_or(0)),
            )?;
            // SAFETY: the actor is pinned and this envelope is unpublished.
            if !unsafe { crate::cow_envelope::bind_receiver(envelope, target) } {
                return None;
            }
            let timer = Arc::new(OneShot {
                // SAFETY: the send guard retains this exact incarnation.
                actor: unsafe { ActorIncarnation::of(actor) },
                registry_key: actor as usize,
                message,
                wheel,
                overdue_ns: has_overdue.then(|| u64::try_from(overdue_ns).unwrap_or(0)),
                request: Mutex::new(Request {
                    protocol: Protocol::new(NonNull::new(envelope)?),
                    pending: None,
                }),
            });
            let mut request = timer
                .request
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);

            // SAFETY: target is pinned; publication is ordered against stop.
            let registered =
                unsafe { crate::timer_periodic::register_one_shot(actor, Arc::clone(&timer)) };
            if registered && timer.arm(&mut request, deadline) {
                drop(request);
                Some(Arc::into_raw(timer))
            } else {
                let _ = request.protocol.cancel();
                drop(request);
                timer.unregister();
                // SAFETY: no callback can have published this failed request.
                unsafe { crate::cow_envelope::unbind_receiver(envelope, target) };
                None
            }
        })
        .flatten()
    })
    .flatten();
    if let Some(timer) = result {
        timer
    } else {
        // SAFETY: failed registration retained the source's typed fields.
        unsafe {
            crate::mem::buf_free(payload);
            crate::cow_envelope::free_allocation(envelope);
        }
        ptr::null()
    }
}

/// Disarm a live cancellation handle without consuming it.
/// # Safety
/// `timer` is null or a borrowed handle returned by `hew_actor_timer_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_timer_cancel(timer: *const OneShot) {
    // SAFETY: the caller retains the optional handle throughout cancellation.
    if let Some(timer) = unsafe { timer.as_ref() } {
        timer.cancel();
    }
}

/// Release the cancellation handle; the actor still owns its scheduled timer.
/// # Safety
/// `timer` is null or one owned handle returned by `hew_actor_timer_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_timer_release(timer: *const OneShot) {
    if !timer.is_null() {
        // SAFETY: the source transfers precisely its handle reference.
        drop(unsafe { Arc::from_raw(timer) });
    }
}
