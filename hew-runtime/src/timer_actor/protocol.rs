//! One-shot custody and settlement shared by firing, dispatch and envelope loss.
use crate::timer_deadline::Deadline;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Phase {
    Scheduled,
    Fired,
    Disarmed,
    Started,
    Cancelled,
    Overdue,
}

#[derive(Debug)]
pub(super) struct Request<E> {
    phase: Phase,
    envelope: Option<E>,
    deadline: Option<Deadline>,
}

pub(super) enum Firing<E> {
    Deliver(E),
    Deadline,
    Ignore,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Start {
    pub allowed: bool,
    pub fault: bool,
}

impl<E> Request<E> {
    pub(super) fn new(envelope: E) -> Self {
        Self {
            phase: Phase::Scheduled,
            envelope: Some(envelope),
            deadline: None,
        }
    }

    pub(super) fn fire(&mut self) -> Firing<E> {
        match self.phase {
            Phase::Scheduled => {
                self.phase = Phase::Fired;
                Firing::Deliver(
                    self.envelope
                        .take()
                        .expect("scheduled timer owns its envelope"),
                )
            }
            Phase::Fired => Firing::Deadline,
            Phase::Disarmed | Phase::Started | Phase::Cancelled | Phase::Overdue => Firing::Ignore,
        }
    }

    pub(super) fn arm_overdue(&mut self, deadline: Deadline) {
        assert_eq!(self.phase, Phase::Fired);
        self.deadline = Some(deadline);
    }

    pub(super) fn cancel(&mut self) -> Option<E> {
        self.phase = match self.phase {
            Phase::Scheduled => Phase::Cancelled,
            Phase::Fired => Phase::Disarmed,
            terminal => terminal,
        };
        self.deadline = None;
        self.envelope.take()
    }

    pub(super) fn start(&mut self, now: Deadline) -> Start {
        let allowed = match self.phase {
            Phase::Fired if self.deadline.is_some_and(|deadline| now >= deadline) => {
                self.phase = Phase::Overdue;
                return Start {
                    allowed: false,
                    fault: true,
                };
            }
            Phase::Fired | Phase::Disarmed => {
                self.phase = Phase::Started;
                true
            }
            Phase::Scheduled | Phase::Started | Phase::Cancelled | Phase::Overdue => false,
        };
        Start {
            allowed,
            fault: false,
        }
    }

    /// A collected deadline may lose to cancellation; an actual envelope loss
    /// also retires a disarmed delivery so that it can never subsequently start.
    pub(super) fn lost(&mut self, obligated: bool, discarded: bool) -> Option<bool> {
        match self.phase {
            Phase::Fired => {
                self.phase = if obligated {
                    Phase::Overdue
                } else {
                    Phase::Cancelled
                };
                Some(obligated)
            }
            Phase::Disarmed if discarded => {
                self.phase = Phase::Cancelled;
                None
            }
            _ => None,
        }
    }
}

/// The owner supplies the same settled-start and loss operations that its
/// producer uses. This token cannot be cloned, and start consumes it.
pub(crate) trait DeliveryOwner {
    fn start(&self) -> bool;
    fn discarded(&self);
}

#[repr(transparent)]
pub(crate) struct Delivery<O: DeliveryOwner>(O);

impl<O: DeliveryOwner> Delivery<O> {
    pub(super) fn new(owner: O) -> Self {
        Self(owner)
    }

    pub(crate) fn start(self) -> bool {
        self.0.start()
    }
}

impl<O: DeliveryOwner> Drop for Delivery<O> {
    fn drop(&mut self) {
        self.0.discarded();
    }
}

#[cfg(kani)]
#[path = "../../../proofs/runtime_protocols/src/one_shot.rs"]
mod formal;
