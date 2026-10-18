//! Delivery identity captured by the sending runtime.

use crate::execution_context::current_context;
use crate::node_identity::HewNodeId;

/// Plain identity carried from mailbox admission through a complete actor turn.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct HewCallerStamp {
    /// Task = 0, actor = 1, timer = 2, authenticated peer = 3.
    pub kind: u64,
    /// Peer identity, or the actor incarnation in `lo` when `kind` is 1.
    pub identity: HewNodeId,
}

impl HewCallerStamp {
    pub(crate) const fn actor(id: u64) -> Self {
        Self {
            kind: 1,
            identity: HewNodeId { hi: 0, lo: id },
        }
    }

    pub(crate) const fn timer() -> Self {
        Self {
            kind: 2,
            identity: HewNodeId { hi: 0, lo: 0 },
        }
    }

    #[cfg(any(test, not(target_arch = "wasm32")))]
    pub(crate) const fn peer(identity: HewNodeId) -> Self {
        Self { kind: 3, identity }
    }
}

/// Snapshot the local sender, independently of who called its current handler.
pub(crate) fn local_sender() -> HewCallerStamp {
    let context = current_context();
    // SAFETY: a non-null TLS context lives throughout the current execution.
    let Some(context) = (unsafe { context.as_ref() }) else {
        return HewCallerStamp::default();
    };
    if context.actor_id == 0 {
        HewCallerStamp::default()
    } else {
        HewCallerStamp::actor(context.actor_id)
    }
}

/// Snapshot the author of a checked call, including helper calls during construction.
/// # Safety
/// `state` is a live checked invocation and `out` is writable for one stamp.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_sender(
    state: *const crate::coro_state::HewCoroState,
    out: *mut HewCallerStamp,
) {
    // SAFETY: the caller retains its invocation throughout this snapshot.
    let id = unsafe { &*state }.execution().actor_turn.actor_id();
    let caller = if id == 0 {
        HewCallerStamp::default()
    } else {
        HewCallerStamp::actor(id)
    };
    // SAFETY: the caller supplies the complete output slot.
    unsafe { out.write(caller) };
}

/// Snapshot an unstructured host ingress sender through its existing runtime authority.
/// # Safety
/// `out` is writable for one stamp.
#[no_mangle]
pub unsafe extern "C" fn hew_local_sender(out: *mut HewCallerStamp) {
    // SAFETY: the caller supplies the complete output slot.
    unsafe { out.write(local_sender()) };
}

/// Read the delivery identity of the current actor turn.
///
/// # Safety
/// `out` must be null or writable for one caller stamp.
#[no_mangle]
pub unsafe extern "C" fn hew_call_caller(out: *mut HewCallerStamp) {
    if out.is_null() {
        return;
    }
    let context = current_context();
    // SAFETY: a non-null TLS context lives throughout the current execution.
    let caller =
        unsafe { context.as_ref() }.map_or(HewCallerStamp::default(), |context| context.caller);
    // SAFETY: the caller supplies a writable output slot.
    unsafe { out.write(caller) };
}

/// Record an accepted one-way delivery's refusal without exposing its reason
/// to a remote caller.
///
/// # Safety
/// `handler` and `reason` are null or live managed strings borrowed by the call.
#[no_mangle]
pub unsafe extern "C" fn hew_call_refusal_log(
    message: u32,
    handler: *const hew_cabi::string::HewString,
    reason: *const hew_cabi::string::HewString,
) {
    let context = current_context();
    // SAFETY: a non-null TLS context remains live for this activation.
    let Some(context) = (unsafe { context.as_ref() }) else {
        return;
    };
    // SAFETY: both managed strings are borrowed for this runtime call.
    let (handler, reason) = unsafe {
        (
            hew_cabi::string::string_as_str(handler),
            hew_cabi::string::string_as_str(reason),
        )
    };
    crate::tracing::record_delivery_refused(context, message.cast_signed(), reason);
    if context.reply_channel.is_null() {
        crate::log_core::runtime_warning(&format!(
            "actor.delivery_refused actor={} handler={handler} reason={reason:?}",
            context.actor_id
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    #[test]
    fn outgoing_calls_use_the_sending_actor_not_its_incoming_peer() {
        let peer = HewCallerStamp::peer(HewNodeId { hi: 17, lo: 23 });
        let _context = TestExecutionContext::install(HewExecutionContext {
            actor_id: 91,
            caller: peer,
            ..HewExecutionContext::default()
        });
        let mut incoming = HewCallerStamp::default();
        // SAFETY: incoming is a writable output slot for this call.
        unsafe { hew_call_caller(&raw mut incoming) };
        assert_eq!(incoming, peer);
        assert_eq!(local_sender(), HewCallerStamp::actor(91));
    }
}
