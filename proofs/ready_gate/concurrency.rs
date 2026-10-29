//! Model-check the exact production gate, including its CAS loops/orderings.
use crate::production::{AtomicByte, ReadyState};
use loom::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
use loom::sync::Arc;

impl AtomicByte for AtomicU8 {
    fn load(&self, order: Ordering) -> u8 {
        self.load(order)
    }
    fn fetch_or(&self, value: u8, order: Ordering) -> u8 {
        self.fetch_or(value, order)
    }
    fn fetch_and(&self, value: u8, order: Ordering) -> u8 {
        self.fetch_and(value, order)
    }
    fn compare_exchange(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8> {
        self.compare_exchange(old, new, success, failure)
    }
    fn compare_exchange_weak(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8> {
        self.compare_exchange_weak(old, new, success, failure)
    }
}

fn gate() -> Arc<ReadyState<AtomicU8>> {
    Arc::new(ReadyState::from_atomic(AtomicU8::new(1)))
}

fn repeated_poll(wakers: usize, bound: Option<usize>) {
    let mut model = loom::model::Builder::new();
    model.preemption_bound = bound;
    model.check(move || {
        let gate = gate();
        for _ in 0..2 {
            assert!(gate.try_begin());
            let mut notifications = Vec::new();
            for _ in 0..wakers {
                let g = gate.clone();
                notifications.push(loom::thread::spawn(move || g.notify()));
            }
            let requeue = gate.finish(false);
            let mut requests = usize::from(requeue);
            for notification in notifications {
                requests += usize::from(notification.join().unwrap());
            }
            assert_eq!(requests, 1, "lost or duplicated scheduling request");
        }
        // Consuming the last request without another notification must leave
        // no phantom work for a third poll cycle.
        assert!(gate.try_begin());
        assert!(!gate.finish(false));
        assert!(
            !gate.try_begin(),
            "stale notification leaked into another cycle"
        );
    });
}

#[test]
fn ready_gate_repeated_wake_finish_races() {
    repeated_poll(1, None);
}

#[test]
fn ready_gate_competing_notifications_two_preemptions() {
    repeated_poll(2, Some(2));
}

#[test]
fn ready_gate_exclusive_poll_acquisition() {
    loom::model(|| {
        let gate = gate();
        let active = Arc::new(AtomicUsize::new(0));
        let mut workers = Vec::new();
        for _ in 0..2 {
            let g = gate.clone();
            let a = active.clone();
            workers.push(loom::thread::spawn(move || {
                if g.try_begin() {
                    assert_eq!(
                        a.fetch_add(1, Ordering::SeqCst),
                        0,
                        "concurrent frame access"
                    );
                    loom::thread::yield_now();
                    assert_eq!(a.fetch_sub(1, Ordering::SeqCst), 1);
                    assert!(!g.finish(false));
                }
            }));
        }
        for worker in workers {
            worker.join().unwrap();
        }
        assert_eq!(active.load(Ordering::SeqCst), 0);
    });
}

#[test]
fn ready_gate_freeze_waits_for_poller_and_retains_notification() {
    loom::model(|| {
        let gate = gate();
        assert!(gate.try_begin());
        let g = gate.clone();
        let worker = loom::thread::spawn(move || {
            g.notify();
            g.finish(false)
        });
        gate.freeze();
        assert!(!gate.try_begin(), "poll started while frozen");
        worker.join().unwrap();
        assert!(gate.frozen_quiescent());
        assert!(!gate.try_begin());
        assert!(gate.unfreeze(), "frozen notification lost");
        assert!(gate.try_begin());
        assert!(!gate.finish(true));
        assert!(!gate.notify());
        assert!(!gate.try_begin(), "completed work restarted");
    });
}

#[test]
fn ready_gate_late_notifications_cannot_restart_completion() {
    loom::model(|| {
        let gate = gate();
        assert!(gate.try_begin());
        let g = gate.clone();
        let late = loom::thread::spawn(move || {
            g.notify();
            g.notify();
        });
        assert!(!gate.finish(true));
        late.join().unwrap();
        assert!(!gate.notify());
        assert!(!gate.try_begin());
    });
}
