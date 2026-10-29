use crate::timer_deadline::Deadline;
use crate::{timer_periodic, timer_wheel};
use loom::sync::Mutex;
use std::sync::atomic::{AtomicBool as AuditBool, AtomicUsize as AuditCounter, Ordering};
use std::sync::Arc;

// Loom 0.7.2 documents SeqCst loads/stores as AcqRel, admitting executions
// forbidden by the source protocol's total order. A mutex-backed single bool
// supplies one linearization point per SC operation, with no stale-read branch.
// This is an SC-only primitive adapter: weaker source orderings fail closed.
// std Arc refcounts remain the standard library boundary, not modeled atomics.
pub struct ScBool(Mutex<bool>);
impl ScBool {
    pub fn new(value: bool) -> Self {
        Self(Mutex::new(value))
    }
    pub fn load(&self, ordering: Ordering) -> bool {
        assert_eq!(
            ordering,
            Ordering::SeqCst,
            "SC flag adapter requires SeqCst source operations"
        );
        *self.0.lock().unwrap()
    }
    pub fn store(&self, value: bool, ordering: Ordering) {
        assert_eq!(
            ordering,
            Ordering::SeqCst,
            "SC flag adapter requires SeqCst source operations"
        );
        *self.0.lock().unwrap() = value;
    }
}

pub mod actor {
    use super::*;
    pub struct HewActor {
        pub id: usize,
        pub live: AuditBool,
        pub sends: AuditCounter,
    }
    pub unsafe fn actor_send_result_internal_reply(
        actor: *mut HewActor,
        _: i32,
        _: *const std::ffi::c_void,
        _: usize,
        _: *mut std::ffi::c_void,
        _: crate::actor_caller::HewCallerStamp,
    ) {
        let actor = unsafe { &*actor };
        assert!(
            actor.live.load(Ordering::SeqCst),
            "periodic: callback accessed retired actor"
        );
        loom::thread::yield_now();
        actor.sends.fetch_add(1, Ordering::SeqCst);
        assert!(
            actor.live.load(Ordering::SeqCst),
            "periodic: actor retired before send finished"
        );
    }
}
pub mod actor_caller {
    pub struct HewCallerStamp;
    impl HewCallerStamp {
        pub fn timer() -> Self {
            Self
        }
    }
}

struct Wheel(*mut timer_wheel::HewTimerWheel);
// SAFETY: wheel methods serialize list access; the model has one ticker and
// joins it and all registration/cancellation workers before dropping the wheel.
unsafe impl Send for Wheel {}
unsafe impl Sync for Wheel {}
impl Wheel {
    fn new() -> Self {
        Self(unsafe { timer_wheel::hew_timer_wheel_new() })
    }
    fn tick(&self, ns: u64) -> i32 {
        unsafe { timer_wheel::timer_wheel_tick_at(self.0, Deadline::from_nanos(ns)) }
    }
    fn empty(&self) -> bool {
        unsafe { timer_wheel::timer_wheel_next_deadline(self.0) }.is_none()
    }
}
impl Drop for Wheel {
    fn drop(&mut self) {
        unsafe { timer_wheel::hew_timer_wheel_free(self.0) };
    }
}

fn registration_vs_first_fire() {
    let actor = Arc::new(actor::HewActor {
        id: 1,
        live: AuditBool::new(true),
        sends: AuditCounter::new(0),
    });
    let wheel = Arc::new(Wheel::new());
    let a = actor.clone();
    let w = wheel.clone();
    let creator = loom::thread::spawn(move || unsafe {
        timer_periodic::schedule(Arc::as_ptr(&a).cast_mut(), w.0, 1)
    });
    let fired = wheel.tick(1);
    let handle = creator.join().unwrap();
    assert_ne!(handle, 0, "periodic: fixture registration must succeed");
    assert!(
        fired <= 1,
        "periodic: one wheel tick must collect at most one pending registration"
    );
    let probe = timer_periodic::probe(handle);
    timer_periodic::cancel(handle);
    assert!(
        wheel.empty(),
        "periodic: cancellation lost the rearmed timer handle"
    );
    assert!(
        timer_periodic::empty(),
        "periodic: cancellation retained registry ownership"
    );
    assert!(
        !probe.alive(),
        "periodic: cancellation leaked the wheel's context reference"
    );
    actor.live.store(false, Ordering::SeqCst);
    assert_eq!(
        wheel.tick(10),
        0,
        "periodic: cancelled registration remained eligible"
    );
}

fn cancel_vs_fire(double: bool) {
    let actor = Arc::new(actor::HewActor {
        id: 2,
        live: AuditBool::new(true),
        sends: AuditCounter::new(0),
    });
    let wheel = Arc::new(Wheel::new());
    let handle = unsafe { timer_periodic::schedule(Arc::as_ptr(&actor).cast_mut(), wheel.0, 1) };
    assert_ne!(handle, 0, "periodic: fixture registration must succeed");
    let probe = timer_periodic::probe(handle);
    let w = wheel.clone();
    let ticker = (!double).then(|| loom::thread::spawn(move || w.tick(1)));
    let other = double.then(|| loom::thread::spawn(move || timer_periodic::cancel(handle)));
    timer_periodic::cancel(handle);
    // An already admitted message is allowed. What must finish before actor
    // retirement is the callback's actor access, not the mailbox's old work.
    actor.live.store(false, Ordering::SeqCst);
    let sends_at_cancel = actor.sends.load(Ordering::SeqCst);
    if let Some(ticker) = ticker {
        ticker.join().unwrap();
    }
    if let Some(other) = other {
        other.join().unwrap();
    }
    assert_eq!(
        actor.sends.load(Ordering::SeqCst),
        sends_at_cancel,
        "periodic: send started after cancellation returned"
    );
    assert!(
        sends_at_cancel <= 1,
        "periodic: one due registration delivered twice"
    );
    assert!(
        wheel.empty(),
        "periodic: cancellation stranded a rearmed entry"
    );
    assert!(
        timer_periodic::empty(),
        "periodic: cancellation retained the registry reference"
    );
    assert!(
        !probe.alive(),
        "periodic: callback/removal failed to release a context reference"
    );
    assert_eq!(
        wheel.tick(10),
        0,
        "periodic: cancelled timer became live again"
    );
    timer_periodic::cancel(handle); // A stale handle is a non-dereferencing no-op.
}

fn late_phase() {
    let actor = Arc::new(actor::HewActor {
        id: 3,
        live: AuditBool::new(true),
        sends: AuditCounter::new(0),
    });
    let wheel = Wheel::new();
    let handle = unsafe { timer_periodic::schedule(Arc::as_ptr(&actor).cast_mut(), wheel.0, 7) };
    assert_ne!(handle, 0, "periodic: fixture registration must succeed");
    let probe = timer_periodic::probe(handle);
    assert_eq!(wheel.tick(6), 0, "periodic: exact deadline fired early");
    let now = 20_000_003;
    assert_eq!(
        wheel.tick(now),
        1,
        "periodic: late cursor must claim one due callback"
    );
    let next = unsafe { timer_wheel::timer_wheel_next_deadline(wheel.0) }
        .unwrap()
        .saturated_nanos();
    assert!(
        next > now,
        "periodic: late callback replayed missed cadence slots"
    );
    assert_eq!(
        (next - 7) % 7,
        0,
        "periodic: late callback drifted from its original phase"
    );
    assert_eq!(
        wheel.tick(now),
        0,
        "periodic: stalled cursor made more work eligible"
    );
    assert_eq!(
        wheel.tick(next),
        1,
        "periodic: rearmed callback was stranded"
    );
    assert_eq!(
        actor.sends.load(Ordering::SeqCst),
        2,
        "periodic: admitted callbacks delivered the wrong number of messages"
    );
    timer_periodic::cancel(handle);
    assert!(
        wheel.empty() && timer_periodic::empty(),
        "periodic: retirement left active ownership"
    );
    assert!(
        !probe.alive(),
        "periodic: retirement leaked the callback context"
    );
}

pub fn verify(selection: Option<&str>) {
    // A real insertion notifies the reactor after releasing the wheel lock.
    // Model that scheduling opportunity too, including before the registering
    // worker receives its first handle from the wheel.
    fn inserted(_: *mut timer_wheel::HewTimerWheel, _: u64) {
        loom::thread::yield_now();
    }
    timer_wheel::register_insert_hook(inserted);
    let checks: &[(&str, fn())] = &[
        ("initial_fire", registration_vs_first_fire),
        ("cancel_fire", || cancel_vs_fire(false)),
        ("double_cancel", || cancel_vs_fire(true)),
        ("late_phase", late_phase),
    ];
    assert!(
        selection.is_none_or(|name| checks.iter().any(|(candidate, _)| *candidate == name)),
        "periodic: unknown scenario"
    );
    for &(name, check) in checks {
        if selection.is_none() || selection == Some(name) {
            loom::model(check);
            println!("Verified periodic {name} (no preemption bound)");
        }
    }
}
