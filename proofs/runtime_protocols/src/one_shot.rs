//! Theorems over the production one-shot protocol and consuming delivery token.
use super::*;
use std::cell::{Cell, RefCell};

fn arbitrary_phase() -> Phase {
    match kani::any::<u8>() % 6 {
        0 => Phase::Scheduled,
        1 => Phase::Fired,
        2 => Phase::Disarmed,
        3 => Phase::Started,
        4 => Phase::Cancelled,
        _ => Phase::Overdue,
    }
}

fn arbitrary_deadline() -> Deadline {
    let fraction_ns: u32 = kani::any();
    kani::assume(fraction_ns < 1_000_000);
    Deadline {
        millis: kani::any(),
        fraction_ns,
    }
}

#[kani::proof]
fn one_shot_inductive_custody_and_terminal_sealing() {
    // Every state satisfying the custody invariant, not a bounded sample of
    // histories. The postcondition is the same invariant for induction.
    let phase = arbitrary_phase();
    let identity: u64 = kani::any();
    let envelope = (phase == Phase::Scheduled).then_some(identity);
    let mut request = Request {
        phase,
        envelope,
        deadline: kani::any::<bool>().then(arbitrary_deadline),
    };
    let now = arbitrary_deadline();
    let deadline = request.deadline;
    let obligated: bool = kani::any();
    let discarded: bool = kani::any();
    let action: u8 = kani::any::<u8>() % 5;
    let mut output = None;
    let mut starts = false;
    let mut fault = false;
    match action {
        0 => {
            if let Firing::Deliver(value) = request.fire() {
                output = Some(value);
            }
        }
        1 => output = request.cancel(),
        2 => {
            let start = request.start(now);
            starts = start.allowed;
            fault = start.fault;
        }
        3 => fault = request.lost(obligated, discarded).unwrap_or(false),
        _ if phase == Phase::Fired => request.arm_overdue(arbitrary_deadline()),
        _ => {} // Only firing has authority to arm the overdue obligation.
    }
    if action == 2 {
        let expired = phase == Phase::Fired && deadline.is_some_and(|limit| now >= limit);
        assert_eq!(
            starts,
            matches!(phase, Phase::Fired | Phase::Disarmed) && !expired,
            "one-shot: every eligible delivery must start, and expired obligations must refuse"
        );
        assert_eq!(
            fault, expired,
            "one-shot: deadline refusal must fault exactly once"
        );
    }
    assert_eq!(
        request.envelope.is_some(),
        request.phase == Phase::Scheduled,
        "one-shot: custody and phase must remain consistent after every transition"
    );
    assert_eq!(
        u8::from(request.envelope.is_some()) + u8::from(output.is_some()),
        u8::from(envelope.is_some()),
        "one-shot: a transition cannot duplicate or lose timer custody"
    );
    if let Some(value) = output {
        assert_eq!(
            value, identity,
            "one-shot: handoff preserves payload identity"
        );
    }
    if starts {
        assert!(matches!(phase, Phase::Fired | Phase::Disarmed));
        assert_eq!(request.phase, Phase::Started);
        assert!(
            !request.start(now).allowed,
            "one-shot: dispatch claim is single use"
        );
        request.cancel();
        assert!(
            !request.start(now).allowed,
            "one-shot: cancellation cannot reopen a started request"
        );
    }
    if fault {
        assert_eq!(request.phase, Phase::Overdue);
        assert!(!request.start(now).allowed);
        assert_eq!(
            request.lost(true, true),
            None,
            "one-shot: fault authority is claimed once"
        );
    }
    if matches!(phase, Phase::Started | Phase::Cancelled | Phase::Overdue) {
        assert_eq!(
            request.phase, phase,
            "one-shot: terminal states cannot reopen"
        );
    }
    kani::cover!(output.is_some() && action == 0, "firing transfers payload");
    kani::cover!(
        output.is_some() && action == 1,
        "cancellation returns payload for cleanup"
    );
    kani::cover!(
        starts && phase == Phase::Disarmed,
        "post-fire cancellation preserves one delivery"
    );
    kani::cover!(fault && discarded, "discarded obligated delivery faults");
}

struct Owner {
    request: RefCell<Request<u64>>,
    starts: Cell<u8>,
    faults: Cell<u8>,
    now: Cell<Deadline>,
}
impl DeliveryOwner for &Owner {
    fn start(&self) -> bool {
        let claim = self.request.borrow_mut().start(self.now.get());
        self.starts.set(self.starts.get() + u8::from(claim.allowed));
        self.faults.set(self.faults.get() + u8::from(claim.fault));
        claim.allowed
    }
    fn discarded(&self) {
        let fault = self.request.borrow_mut().lost(true, true).unwrap_or(false);
        self.faults.set(self.faults.get() + u8::from(fault));
    }
}

#[kani::proof]
#[kani::unwind(2)]
fn one_shot_firing_cancel_and_consuming_delivery_compose() {
    let identity: u64 = kani::any();
    let owner = Owner {
        request: RefCell::new(Request::new(identity)),
        starts: Cell::new(0),
        faults: Cell::new(0),
        now: Cell::new(Deadline::from_millis(0)),
    };
    let cancel_before_fire: bool = kani::any();
    let cancel_after_fire: bool = kani::any();
    let discard: bool = kani::any();
    let mut retired = 0;
    if cancel_before_fire {
        assert_eq!(owner.request.borrow_mut().cancel(), Some(identity));
        retired += 1;
    }
    // The list collector's callback/removal exclusion has its own theorem.
    // Here fire is permitted even after cancellation, as with a callback
    // already collected outside the wheel lock. No queue semantics are assumed.
    let firing = owner.request.borrow_mut().fire();
    if let Firing::Deliver(received) = firing {
        assert!(!cancel_before_fire);
        assert_eq!(received, identity);
        let delivery = Delivery::new(&owner);
        if cancel_after_fire {
            assert_eq!(owner.request.borrow_mut().cancel(), None);
        }
        if discard {
            drop(delivery);
        } else {
            assert!(delivery.start());
        }
        retired += 1;
    }
    assert_eq!(retired, 1, "one-shot: exactly one payload retirement path");
    assert_eq!(
        owner.starts.get(),
        u8::from(!cancel_before_fire && !discard),
        "one-shot: exactly the consumed dispatch token may start"
    );
    assert_eq!(
        owner.faults.get(),
        u8::from(!cancel_before_fire && !cancel_after_fire && discard),
        "one-shot: losing an obligated delivery must fault exactly once"
    );
    assert!(owner.request.borrow().envelope.is_none());
    assert!(
        !owner.request.borrow_mut().start(owner.now.get()).allowed,
        "one-shot: settlement must seal all subsequent starts"
    );
    kani::cover!(
        cancel_before_fire,
        "collected callback cannot redeliver cancelled custody"
    );
    kani::cover!(
        !cancel_before_fire && discard && !cancel_after_fire,
        "dropping obligated delivery faults once"
    );
    kani::cover!(
        !cancel_before_fire && !discard && cancel_after_fire,
        "disarmed delivery can start once"
    );
}
