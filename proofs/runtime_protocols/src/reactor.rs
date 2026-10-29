//! Runs the production reactor loop and wheel. Only kernel waiting and time
//! are supplied by this host. No elapsed-time or latency assertions.
use crate::reactor_wait::{self, Driver, AWAKE};
use crate::timer_deadline::Deadline;
use crate::timer_wheel::{self, HewTimerHandle, HewTimerWheel};
use loom::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use loom::sync::{Arc, Condvar, Mutex};
use std::io;
use std::time::Duration;

loom::thread_local! {
    static INSERT_TARGET: std::cell::RefCell<Option<Arc<Shared>>> = std::cell::RefCell::new(None);
}

fn inserted_hook(wheel: *mut HewTimerWheel, deadline_ns: u64) {
    INSERT_TARGET.with(|target| {
        if let Some(shared) = target.borrow().as_ref() {
            assert_eq!(
                wheel, shared.wheel.0,
                "reactor: hook must target the published wheel"
            );
            reactor_wait::inserted(&shared.sleep_until, deadline_ns, || shared.wake());
        }
    });
}

struct Wheel(*mut HewTimerWheel);
// SAFETY: the wheel serializes access internally. Arc retains it until all
// producers and the sole consumer have finished; Drop has exclusive access.
unsafe impl Send for Wheel {}
unsafe impl Sync for Wheel {}
impl Wheel {
    fn new() -> Self {
        Self(unsafe { timer_wheel::hew_timer_wheel_new() })
    }
    fn add(&self, deadline: Deadline) -> HewTimerHandle {
        unsafe extern "C" fn inert(_: *mut std::ffi::c_void) {}
        self.add_callback(deadline, inert, std::ptr::null_mut())
    }
    fn add_callback(
        &self,
        deadline: Deadline,
        callback: timer_wheel::HewTimerCb,
        data: *mut std::ffi::c_void,
    ) -> HewTimerHandle {
        let handle = unsafe {
            timer_wheel::timer_wheel_schedule_precise_handle(self.0, deadline, callback, data)
        };
        assert!(
            !handle.entry.is_null(),
            "reactor: fixture timer must be admitted"
        );
        handle
    }
    fn next(&self) -> Option<Deadline> {
        unsafe { timer_wheel::timer_wheel_next_deadline(self.0) }
    }
}
impl Drop for Wheel {
    fn drop(&mut self) {
        unsafe { timer_wheel::hew_timer_wheel_free(self.0) };
    }
}

#[derive(Default)]
struct Notification {
    pending: bool,
    producer_done: bool,
}
struct Shared {
    wheel: Wheel,
    sleep_until: AtomicU64,
    stop: AtomicBool,
    notification: Mutex<Notification>,
    changed: Condvar,
}
impl Shared {
    fn new() -> Self {
        Self {
            wheel: Wheel::new(),
            sleep_until: AtomicU64::new(AWAKE),
            stop: AtomicBool::new(false),
            notification: Mutex::new(Notification::default()),
            changed: Condvar::new(),
        }
    }
    fn wake(&self) {
        self.notification.lock().unwrap().pending = true;
        self.changed.notify_one();
    }
}

struct InsertionRace {
    shared: Arc<Shared>,
    inserted: Deadline,
    waits: usize,
}
impl Driver for InsertionRace {
    fn stopping(&self) -> bool {
        self.shared.stop.load(Ordering::Acquire)
    }
    fn next_deadline(&mut self) -> Option<Deadline> {
        self.shared.wheel.next()
    }
    fn now(&self) -> Deadline {
        Deadline::from_nanos(1_000_001)
    }
    fn wait(&mut self, timeout: Option<Duration>) -> io::Result<()> {
        self.waits += 1;
        assert_eq!(self.waits, 1, "reactor: race must enter exactly one wait");
        let mut notification = self.shared.notification.lock().unwrap();
        // This condition is test coordination, not a wake available to Hew.
        // It lets the producer finish even when a broken protocol strands the
        // fake kernel wait. The assertion below then reports that lost wake.
        while !notification.producer_done {
            notification = self.shared.changed.wait(notification).unwrap();
        }
        assert!(
            timeout.is_some_and(|t| t <= self.inserted.remaining(self.now())) || notification.pending,
            "reactor: earlier timer stranded behind the selected wait; timeout={timeout:?}, deadline={:?}",
            self.inserted,
        );
        notification.pending = false;
        self.shared.stop.store(true, Ordering::Release);
        Ok(())
    }
    fn dispatch(&mut self) {}
    fn tick(&mut self) {
        panic!("reactor: shutdown must not claim timers");
    }
}

fn insertion_race(initial: Option<Deadline>, inserted: Deadline) {
    timer_wheel::register_insert_hook(inserted_hook);
    let shared = Arc::new(Shared::new());
    if let Some(deadline) = initial {
        shared.wheel.add(deadline);
    }
    let producer = shared.clone();
    let thread = loom::thread::spawn(move || {
        // Substitute only the process-wheel/OS adapter. The production wheel
        // must actually invoke its hook after releasing its registration lock.
        INSERT_TARGET.with(|target| *target.borrow_mut() = Some(producer.clone()));
        producer.wheel.add(inserted);
        producer.notification.lock().unwrap().producer_done = true;
        producer.changed.notify_one();
    });
    let mut driver = InsertionRace {
        shared: shared.clone(),
        inserted,
        waits: 0,
    };
    reactor_wait::run(&shared.sleep_until, &mut driver).unwrap();
    thread.join().unwrap();
    assert_eq!(driver.waits, 1, "reactor: must actually enter the poller");
    assert_eq!(
        shared.sleep_until.load(Ordering::SeqCst),
        AWAKE,
        "reactor: returning from wait must relinquish the sleep limit"
    );
}

#[derive(Clone, Copy)]
enum Scenario {
    Drain,
    Interrupted,
    Stop,
    PollError,
    Exact,
    Cancel,
}
struct Scripted {
    shared: Arc<Shared>,
    scenario: Scenario,
    waits: usize,
    ticks: usize,
    dispatches: usize,
    blocked: bool,
    now_ns: u64,
    handle: Option<HewTimerHandle>,
    fired: Arc<AtomicUsize>,
}
impl Driver for Scripted {
    fn stopping(&self) -> bool {
        self.shared.stop.load(Ordering::Acquire)
    }
    fn next_deadline(&mut self) -> Option<Deadline> {
        self.shared.wheel.next()
    }
    fn now(&self) -> Deadline {
        Deadline::from_nanos(self.now_ns)
    }
    fn wait(&mut self, timeout: Option<Duration>) -> io::Result<()> {
        self.waits += 1;
        assert!(
            self.waits <= 3,
            "reactor: drained inputs must return to blocking"
        );
        if self.waits == 1 {
            match self.scenario {
                Scenario::Drain => {
                    assert!(
                        timeout.is_none(),
                        "reactor: empty wheel must wait indefinitely"
                    );
                    let mut notification = self.shared.notification.lock().unwrap();
                    assert!(
                        notification.pending,
                        "reactor: fixture must supply a notification"
                    );
                    notification.pending = false;
                    return Ok(()); // Consume the sole pending notification.
                }
                Scenario::Interrupted => return Err(io::ErrorKind::Interrupted.into()),
                Scenario::Stop => {
                    self.shared.stop.store(true, Ordering::Release);
                    return Ok(());
                }
                Scenario::PollError => return Err(io::ErrorKind::BrokenPipe.into()),
                Scenario::Exact | Scenario::Cancel => {
                    assert_eq!(
                        timeout,
                        Some(Duration::from_nanos(37)),
                        "reactor: wait must subtract the sampled clock without rounding"
                    );
                    if matches!(self.scenario, Scenario::Cancel) {
                        let handle = self.handle.take().unwrap();
                        let data = unsafe {
                            timer_wheel::hew_timer_wheel_remove(
                                self.shared.wheel.0,
                                handle.entry,
                                handle.generation,
                            )
                        };
                        assert_eq!(
                            data,
                            Arc::as_ptr(&self.fired).cast_mut().cast(),
                            "reactor: cancellation must recover its payload"
                        );
                        self.now_ns = 1_000_038;
                    } else {
                        self.now_ns = 1_000_037; // Spurious wake before eligibility.
                    }
                    return Ok(());
                }
            }
        }
        if matches!(self.scenario, Scenario::Exact) && self.waits == 2 {
            assert_eq!(
                timeout,
                Some(Duration::from_nanos(1)),
                "reactor: spurious wake must preserve the original deadline"
            );
            assert_eq!(
                self.fired.load(Ordering::SeqCst),
                0,
                "reactor: no early callback"
            );
            self.now_ns = 1_000_038;
            return Ok(());
        }
        assert!(
            timeout.is_none(),
            "reactor: no periodic poll when no timer is pending"
        );
        assert!(
            !self.shared.notification.lock().unwrap().pending,
            "reactor: idle check must happen after draining notifications"
        );
        self.blocked = true;
        // The fake kernel records an indefinite block. Stop the finite model
        // at that boundary; no test timer or wall-clock sleep releases it.
        self.shared.stop.store(true, Ordering::Release);
        Ok(())
    }
    fn dispatch(&mut self) {
        self.dispatches += 1;
    }
    fn tick(&mut self) {
        self.ticks += 1;
        unsafe { timer_wheel::timer_wheel_tick_at(self.shared.wheel.0, self.now()) };
    }
}

fn scripted(scenario: Scenario) {
    let shared = Arc::new(Shared::new());
    let fired = Arc::new(AtomicUsize::new(0));
    let handle = if matches!(
        scenario,
        Scenario::Exact | Scenario::Stop | Scenario::Cancel
    ) {
        unsafe extern "C" fn callback(data: *mut std::ffi::c_void) {
            unsafe { &*data.cast::<AtomicUsize>() }.fetch_add(1, Ordering::SeqCst);
        }
        Some(shared.wheel.add_callback(
            Deadline::from_nanos(1_000_038),
            callback,
            Arc::as_ptr(&fired).cast_mut().cast(),
        ))
    } else {
        None
    };
    if matches!(scenario, Scenario::Drain) {
        shared.wake();
    }
    let mut driver = Scripted {
        shared: shared.clone(),
        scenario,
        waits: 0,
        ticks: 0,
        dispatches: 0,
        blocked: false,
        now_ns: 1_000_001,
        handle,
        fired: fired.clone(),
    };
    let result = reactor_wait::run(&shared.sleep_until, &mut driver);
    let active_turns = match scenario {
        Scenario::Drain | Scenario::Interrupted | Scenario::Cancel => 1,
        Scenario::Exact => 2,
        Scenario::Stop | Scenario::PollError => 0,
    };
    assert_eq!(
        driver.waits,
        active_turns + 1,
        "reactor: expected wait path was not executed"
    );
    assert_eq!(
        driver.ticks, active_turns,
        "reactor: only active successful/interrupted turns may tick"
    );
    assert_eq!(
        driver.dispatches,
        if matches!(scenario, Scenario::PollError) {
            0
        } else {
            driver.waits
        },
        "reactor: poll failure must not dispatch stale events"
    );
    assert_eq!(
        driver.blocked,
        !matches!(scenario, Scenario::Stop | Scenario::PollError),
        "reactor: drained work must reach the blocking poller"
    );
    assert_eq!(
        result.is_err(),
        matches!(scenario, Scenario::PollError),
        "reactor: only interrupts are retryable"
    );
    assert_eq!(
        fired.load(Ordering::SeqCst),
        usize::from(matches!(scenario, Scenario::Exact)),
        "reactor: callbacks fire once at eligibility; cancellation/shutdown retains ownership"
    );
    assert_eq!(
        shared.sleep_until.load(Ordering::SeqCst),
        AWAKE,
        "reactor: poll return, including failure, must clear sleep publication"
    );
}

fn later_insert_is_quiet() {
    let sleep = AtomicU64::new(1_000_038);
    for deadline in [1_000_038, 1_000_039, u64::MAX - 1] {
        reactor_wait::inserted(&sleep, deadline, || {
            panic!("reactor: later insertion needlessly wakes the poller")
        });
    }
    sleep.store(AWAKE, Ordering::SeqCst);
    reactor_wait::inserted(&sleep, 1, || {
        panic!("reactor: active reactor needs no timer notification")
    });
}

pub fn verify(selection: Option<&str>) {
    let checks: &[(&str, fn())] = &[
        ("empty_insert", || {
            insertion_race(None, Deadline::from_nanos(1_000_038))
        }),
        ("earlier_insert", || {
            insertion_race(
                Some(Deadline::from_nanos(2_000_039)),
                Deadline::from_nanos(1_000_038),
            )
        }),
        ("saturated_insert", || {
            insertion_race(
                Some(Deadline::from_millis(u64::MAX - 1)),
                Deadline::from_millis(u64::MAX / 2),
            )
        }),
        ("idle_block", || scripted(Scenario::Drain)),
        ("interrupt", || scripted(Scenario::Interrupted)),
        ("stop", || scripted(Scenario::Stop)),
        ("poll_error", || scripted(Scenario::PollError)),
        ("exact_wait", || scripted(Scenario::Exact)),
        ("cancel_idle", || scripted(Scenario::Cancel)),
        ("later_quiet", later_insert_is_quiet),
    ];
    if let Some(name) = selection {
        assert!(
            checks.iter().any(|(candidate, _)| *candidate == name),
            "unknown reactor model: {name}"
        );
    }
    for &(name, check) in checks {
        if selection.is_none() || selection == Some(name) {
            loom::model(check);
            println!("Verified reactor {name} (no preemption bound)");
        }
    }
}
