//! Concurrent compositions using production protocols and modeled locks/CAS.
use crate::wake::{scheduled::ReadyState, HewWaker};
use loom::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
use loom::thread;
use std::ffi::c_void;
use std::sync::Arc;

struct Trace {
    wakes: AtomicUsize,
    requests: AtomicUsize,
    cleanups: AtomicUsize,
    seen_status: AtomicUsize,
    reentrant_won: AtomicUsize,
    gate: ReadyState<AtomicU8>,
}
impl Trace {
    fn new(queued: bool) -> Arc<Self> {
        Arc::new(Self {
            wakes: AtomicUsize::new(0),
            requests: AtomicUsize::new(0),
            cleanups: AtomicUsize::new(0),
            seen_status: AtomicUsize::new(0),
            reentrant_won: AtomicUsize::new(0),
            gate: crate::wake::gate(queued),
        })
    }
}
unsafe extern "C" fn retain(data: *mut c_void) {
    Arc::increment_strong_count(data.cast::<Trace>());
}
unsafe extern "C" fn release(data: *mut c_void) {
    Arc::decrement_strong_count(data.cast::<Trace>());
}
unsafe extern "C" fn notify(data: *mut c_void) {
    let trace = &*data.cast::<Trace>();
    trace.wakes.fetch_add(1, Ordering::SeqCst);
    if trace.gate.notify() {
        trace.requests.fetch_add(1, Ordering::SeqCst);
    }
}
fn descriptor(trace: &Arc<Trace>) -> HewWaker {
    HewWaker {
        context: Arc::as_ptr(trace).cast_mut().cast(),
        wake: notify,
        retain,
        release,
    }
}

fn model(bound: Option<usize>) -> loom::model::Builder {
    let mut model = loom::model::Builder::new();
    model.preemption_bound = bound;
    // Environment settings must not silently turn a complete bounded run
    // into a sample, an elapsed-time cutoff, or a resumed checkpoint.
    model.max_permutations = None;
    model.max_duration = None;
    model.checkpoint_file = None;
    model.expect_explicit_explore = false;
    model
}

fn cancellation_delivery(detach: bool) {
    let model = model(Some(2));
    model.check(move || unsafe {
        use crate::cancel_token::*;
        let root = hew_cancel_token_new_child(std::ptr::null_mut());
        let leaf = hew_cancel_token_new_child(root);
        let trace = Trace::new(true);
        trace.gate.begin();
        let target = trace.clone();
        let leaf_address = leaf as usize;
        let observe = thread::spawn(move || {
            let observer = hew_cancel_observe(leaf_address as *mut _, &descriptor(&target));
            if detach {
                hew_cancel_unobserve(observer);
                0
            } else {
                observer as usize
            }
        });
        let address = root as usize;
        let first = thread::spawn(move || hew_cancel_token_cancel(address as *mut _, 11));
        let second = thread::spawn(move || hew_cancel_token_cancel(address as *mut _, 22));
        let observer = observe.join().unwrap();
        first.join().unwrap();
        second.join().unwrap();
        let wakes = trace.wakes.load(Ordering::SeqCst);
        if !detach {
            assert_eq!(
                wakes, 1,
                "protocol: cancellation registration lost or duplicated its notification"
            );
            hew_cancel_token_release(leaf);
            hew_cancel_token_release(root);
            assert_eq!(
                hew_cancel_token_is_requested(leaf),
                1,
                "protocol: observer failed to retain its token ancestry"
            );
            let reason = cancel_token_reason(leaf);
            assert!(
                reason == 11 || reason == 22,
                "protocol: published cancellation had no winning reason"
            );
            hew_cancel_unobserve(observer as *mut _);
        } else {
            assert!(wakes <= 1, "protocol: detachment duplicated cancellation");
            hew_cancel_token_release(leaf);
            hew_cancel_token_release(root);
        }
        assert_eq!(
            Arc::strong_count(&trace),
            1,
            "protocol: detached subscription retained its target"
        );
        assert_eq!(
            trace.requests.load(Ordering::SeqCst),
            0,
            "protocol: cancellation duplicated the active poll"
        );
        assert_eq!(
            trace.gate.finish(false),
            wakes != 0,
            "protocol: polling work lost its cancellation notification"
        );
        assert_eq!(trace.gate.try_begin(), wakes != 0);
        if wakes != 0 {
            assert!(!trace.gate.finish(true));
        }
        assert!(
            !trace.gate.try_begin(),
            "protocol: cancellation left a phantom poll"
        );
    });
}

struct Cleanup {
    registration: *mut crate::await_cancel::HewAwaitCancel,
    trace: Arc<Trace>,
}
unsafe extern "C" fn cleanup(data: *mut c_void, status: i32) {
    let source = &*data.cast::<Cleanup>();
    source.trace.cleanups.fetch_add(1, Ordering::SeqCst);
    source.trace.seen_status.store(
        crate::await_cancel::hew_await_cancel_status(source.registration) as usize,
        Ordering::SeqCst,
    );
    source.trace.reentrant_won.fetch_add(
        crate::await_cancel::hew_await_cancel_complete(source.registration) as usize,
        Ordering::SeqCst,
    );
    let _ = status;
}

fn await_race(prefix: u8) {
    let model = model(Some(2));
    model.check(move || unsafe {
        use crate::await_cancel::*;
        use crate::cancel_token::*;
        use crate::timer_wheel::*;
        crate::resume::WAKES.store(0, Ordering::Relaxed);
        let token = hew_cancel_token_new_child(std::ptr::null_mut());
        let mut context = crate::execution_context::Context {
            arena: std::ptr::null_mut(),
            cancel_token: token,
            request_deadline: crate::request_deadline::RequestDeadline::default(),
        };
        crate::execution_context::CURRENT.store(&mut context, Ordering::Relaxed);
        let trace = Trace::new(false);
        let mut source = Cleanup {
            registration: std::ptr::null_mut(),
            trace: trace.clone(),
        };
        let mut actor = crate::actor::HewActor { id: 1 };
        let reg = hew_await_cancel_new(
            &mut actor,
            Some(cleanup),
            std::ptr::from_mut(&mut source).cast(),
        );
        source.registration = reg;
        crate::execution_context::CURRENT.store(std::ptr::null_mut(), Ordering::Relaxed);
        let wheel = hew_timer_wheel_new();
        assert_eq!(hew_await_cancel_schedule_deadline_ms(reg, wheel, 1), 0);
        if prefix != 0 {
            if prefix == 1 {
                assert_eq!(hew_await_cancel_complete(reg), 1);
            } else {
                assert_eq!(hew_await_cancel_cancel(reg, 2, 0), 1);
            }
            assert!(
                hew_timer_wheel_next_deadline_ms(wheel) == -1,
                "protocol: settled await retained its timer registration"
            );
        }
        let address = reg as usize;
        let finish = thread::spawn(move || hew_await_cancel_complete(address as *mut _));
        let token_address = token as usize;
        let cancel = thread::spawn(move || {
            hew_cancel_token_cancel(token_address as *mut _, 17);
            hew_await_cancel_observe_token(address as *mut _)
        });
        let wheel_address = wheel as usize;
        let tick = thread::spawn(move || timer_wheel_tick_to(wheel_address as *mut _, 1));
        let finished = finish.join().unwrap();
        let cancelled = cancel.join().unwrap();
        tick.join().unwrap();
        let status = hew_await_cancel_status(reg);
        assert!(
            (1..=3).contains(&status),
            "protocol: racing await stayed pending"
        );
        assert_eq!(
            finished + cancelled + i32::from(status == 3) + i32::from(prefix != 0),
            1,
            "protocol: competing await outcomes admitted multiple owners"
        );
        let cleaned = trace.cleanups.load(Ordering::SeqCst);
        assert_eq!(
            cleaned,
            usize::from(status != 1),
            "protocol: competing outcomes duplicated or omitted cleanup"
        );
        if cleaned != 0 {
            assert_eq!(
                trace.seen_status.load(Ordering::SeqCst),
                status as usize,
                "protocol: cleanup ran before publishing its outcome"
            );
        }
        assert_eq!(
            trace.reentrant_won.load(Ordering::SeqCst),
            0,
            "protocol: reentrant cleanup settled the await twice"
        );
        assert_eq!(
            crate::resume::WAKES.load(Ordering::Relaxed),
            usize::from(status != 1 && prefix != 2),
            "protocol: competing outcomes duplicated or omitted the wake"
        );
        assert!(
            hew_timer_wheel_next_deadline_ms(wheel) == -1,
            "protocol: racing await kept an armed timer"
        );
        hew_await_cancel_free(reg);
        hew_cancel_token_release(token);
        hew_timer_wheel_free(wheel);
    });
}

struct CollectedDrop {
    sleep: *mut crate::coro_sleep::HewCoroSleep,
    trace: *const Arc<Trace>,
}
unsafe extern "C" fn drop_collected(data: *mut c_void) {
    let owner = &*data.cast::<CollectedDrop>();
    crate::coro_sleep::hew_coro_sleep_free(owner.sleep);
    (**owner.trace)
        .seen_status
        .store(Arc::strong_count(&*owner.trace), Ordering::SeqCst);
}

fn collected_sleep_teardown() {
    model(None).check(|| unsafe {
        use crate::coro_sleep::*;
        use crate::timer_wheel::*;
        let wheel = hew_timer_wheel_new();
        crate::timer_periodic::WHEEL.store(wheel, Ordering::Relaxed);
        let trace = Trace::new(false);
        let sleep = hew_coro_sleep_until_new(1_000_001, &descriptor(&trace));
        let mut owner = CollectedDrop {
            sleep,
            trace: &trace,
        };
        // Head insertion matches a native wheel slot: both entries are
        // collected before the first callback destroys the sleep creator.
        timer_wheel_schedule_precise_handle(
            wheel,
            crate::timer_deadline::Deadline::from_nanos(1_000_001),
            drop_collected,
            std::ptr::from_mut(&mut owner).cast(),
        );
        timer_wheel_tick_to(wheel, 2);
        assert_eq!(
            trace.seen_status.load(Ordering::SeqCst),
            2,
            "protocol: creator teardown consumed the collected callback reference"
        );
        assert_eq!(
            trace.wakes.load(Ordering::SeqCst),
            0,
            "protocol: cancelled sleep's collected callback woke the target"
        );
        assert_eq!(
            Arc::strong_count(&trace),
            1,
            "protocol: collected callback leaked its retained target"
        );
        hew_timer_wheel_free(wheel);
    });
}

fn sleep_teardown() {
    model(None).check(|| unsafe {
        use crate::coro_sleep::*;
        use crate::request_deadline::RequestDeadline;
        use crate::timer_wheel::*;
        let wheel = hew_timer_wheel_new();
        crate::timer_periodic::WHEEL.store(wheel, Ordering::Relaxed);
        let trace = Trace::new(false);
        let deadline =
            RequestDeadline::after(3_000_000).earlier(RequestDeadline::from_raw(1_000_001));
        let sleep = hew_coro_sleep_until_new(deadline.raw(), &descriptor(&trace));
        assert_eq!(
            timer_wheel_tick_to(wheel, 1),
            0,
            "protocol: sleep fired before its inherited deadline"
        );
        assert_eq!(hew_coro_sleep_status(sleep), 0);
        let address = sleep as usize;
        let dropper = thread::spawn(move || hew_coro_sleep_free(address as *mut _));
        let wheel_address = wheel as usize;
        let tick = thread::spawn(move || timer_wheel_tick_to(wheel_address as *mut _, 2));
        dropper.join().unwrap();
        tick.join().unwrap();
        let wakes = trace.wakes.load(Ordering::SeqCst);
        assert!(wakes <= 1, "protocol: a sleep callback notified twice");
        assert_eq!(
            Arc::strong_count(&trace),
            1,
            "protocol: sleep callback leaked or consumed a target reference twice"
        );
        assert_eq!(
            trace.gate.try_begin(),
            wakes != 0,
            "protocol: sleep notification disagrees with scheduler admission"
        );
        if wakes != 0 {
            assert!(!trace.gate.finish(true));
        }
        assert_eq!(
            timer_wheel_tick_to(wheel, 3),
            0,
            "protocol: dropped sleep retained a timer"
        );
        hew_timer_wheel_free(wheel);
    });
}

pub(crate) fn verify(selection: Option<&str>) {
    match selection {
        Some("cancellation_delivery") => {
            cancellation_delivery(false);
            cancellation_delivery(true);
            println!("Verified concurrent cancellation delivery and detachment (two preemptions)");
        }
        Some("await_race") => {
            await_race(0);
            await_race(1);
            await_race(2);
            println!("Verified completion/cancellation/timeout arbitration (two preemptions)");
        }
        Some("sleep_teardown") => {
            sleep_teardown();
            collected_sleep_teardown();
            println!("Verified sleep teardown versus collected callbacks (no preemption bound)");
        }
        None => {
            for name in ["cancellation_delivery", "await_race", "sleep_teardown"] {
                verify(Some(name));
            }
        }
        Some(other) => panic!("Unknown concurrent composition: {other}"),
    }
}
