//! Reactor parking protocol. The verification host replaces only the clock,
//! poller and atomic primitives; it executes this production loop unchanged.
use crate::runtime_sync::AtomicU64;
use crate::timer_deadline::Deadline;
use std::io;
use std::sync::atomic::Ordering;
use std::time::Duration;

pub(crate) const AWAKE: u64 = 0;

pub(crate) fn sleep_limit(deadline: Option<Deadline>) -> u64 {
    deadline.map_or(u64::MAX, |d| d.saturated_nanos().min(u64::MAX - 1))
}

pub(crate) trait Driver {
    fn stopping(&self) -> bool;
    fn next_deadline(&mut self) -> Option<Deadline>;
    fn now(&self) -> Deadline;
    fn wait(&mut self, timeout: Option<Duration>) -> io::Result<()>;
    fn dispatch(&mut self);
    fn tick(&mut self);
}

/// Called after publishing an insertion and releasing the wheel lock. A wake
/// source must retain a notification posted before the corresponding wait.
pub(crate) fn inserted(sleep_until: &AtomicU64, deadline_ns: u64, wake: impl FnOnce()) {
    let limit = sleep_until.load(Ordering::SeqCst);
    // Saturated limits represent many distinct deadlines. Their ordering is
    // unknown, so any insertion must force a fresh wheel lookup.
    if deadline_ns < limit || limit == u64::MAX - 1 {
        wake();
    }
}

fn next_timeout(sleep_until: &AtomicU64, driver: &mut impl Driver) -> Option<Duration> {
    let mut deadline = driver.next_deadline();
    loop {
        sleep_until.store(sleep_limit(deadline), Ordering::SeqCst);
        // An insert either observes the published sleep limit and wakes us,
        // or this re-read observes the insertion before we begin waiting.
        let again = driver.next_deadline();
        if again.is_some() && (deadline.is_none() || again < deadline) {
            deadline = again;
        } else {
            return deadline.map(|d| d.remaining(driver.now()));
        }
    }
}

pub(crate) fn run(sleep_until: &AtomicU64, driver: &mut impl Driver) -> io::Result<()> {
    while !driver.stopping() {
        let timeout = next_timeout(sleep_until, driver);
        let waited = driver.wait(timeout);
        sleep_until.store(AWAKE, Ordering::SeqCst);
        match waited {
            Ok(()) => {}
            Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
            Err(error) => return Err(error),
        }
        driver.dispatch();
        // Shutdown owns timers not yet claimed by a tick.
        if !driver.stopping() {
            driver.tick();
        }
    }
    Ok(())
}
