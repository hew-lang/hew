//! Bounded adaptive spinning before a thread parks.
//!
//! A worker that runs out of work, and a synchronous waiter whose reply is not
//! yet ready, usually see the next item within a few microseconds when actors
//! exchange requests. Parking at once costs a futex wait on the parker and a
//! futex wake on the notifier for every exchange. Spinning briefly first lets
//! the thread pick the item up without either syscall, and a notifier that sees
//! no parked thread skips its wake.
//!
//! Spinning is a latency heuristic only. Every probe made while spinning is
//! advisory; the parker's final probe under its mutex stays the single
//! authority that closes the lost-wakeup window.
//!
//! The budget adapts per thread. A spin that finds what it waited for grows
//! the budget to twice the iterations that spin needed (and at least doubles
//! it), up to the configured limit; a wasted spin halves it until it reaches
//! zero. A thread whose budget reached zero retries a quarter-limit spin every
//! [`RETRY_PERIOD`] waits, so a slow phase does not disable spinning for the
//! rest of the process, and an overloaded host pays little for the retries. A thread spins at most once per park, so an idle
//! runtime stays parked and costs no CPU.

#[cfg(test)]
use std::sync::atomic::{AtomicU64, Ordering};

/// Default upper bound on spin iterations per wait (`HEW_SPIN` unset).
///
/// One iteration is an advisory probe followed by [`PAUSES_PER_PROBE`]
/// [`std::hint::spin_loop`] hints, so the default spins for some tens of
/// microseconds: a few futex wake round trips.
pub(crate) const DEFAULT_SPIN_LIMIT: u32 = 512;

/// Spin-loop hints between probes. Probing less often keeps spinners from
/// pulling the queues' cache lines away from the threads pushing to them.
const PAUSES_PER_PROBE: u32 = 8;

/// The smallest non-zero budget; shrinking below it disables spinning.
pub(crate) const MIN_SPIN: u32 = 8;

/// A thread whose budget reached zero retries a quarter-limit spin on every
/// `RETRY_PERIOD`th wait.
pub(crate) const RETRY_PERIOD: u32 = 16;

/// Spin episodes started, for tests asserting that an idle runtime stops
/// spinning once its threads park.
#[cfg(test)]
static SPIN_EPISODES: AtomicU64 = AtomicU64::new(0);

/// Total spin episodes started by workers and synchronous waiters.
#[cfg(test)]
pub(crate) fn spin_episodes_total() -> u64 {
    SPIN_EPISODES.load(Ordering::Relaxed)
}

/// Resolve the configured spin limit for a scheduler with `worker_count`
/// workers on a host with `cores` available hardware threads.
///
/// `HEW_SPIN` takes `0` or `off` to disable spinning, or a positive iteration
/// limit. Spinning is also off when the host has one hardware thread or the
/// workers outnumber it (a spinner would then delay the very thread it waits
/// on), and on the single-thread driver, which has no workers.
pub(crate) fn configured_spin_limit(worker_count: usize, cores: usize) -> u32 {
    let requested = match std::env::var("HEW_SPIN") {
        Ok(value) => parse_spin_limit(&value).unwrap_or_else(|| {
            eprintln!("warning: HEW_SPIN={value} is invalid, using default");
            DEFAULT_SPIN_LIMIT
        }),
        Err(_) => DEFAULT_SPIN_LIMIT,
    };
    spin_limit_for(requested, worker_count, cores)
}

fn parse_spin_limit(value: &str) -> Option<u32> {
    let value = value.trim();
    if value.eq_ignore_ascii_case("off") {
        return Some(0);
    }
    value.parse::<u32>().ok()
}

fn spin_limit_for(requested: u32, worker_count: usize, cores: usize) -> u32 {
    if cores <= 1 || worker_count == 0 || worker_count > cores {
        0
    } else {
        requested
    }
}

/// Per-thread adaptive spin budget.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct SpinBudget {
    current: u32,
    waits_at_zero: u32,
}

impl SpinBudget {
    /// A fresh budget starts at `limit` so the first exchanges spin.
    pub(crate) const fn new(limit: u32) -> Self {
        Self {
            current: limit,
            waits_at_zero: 0,
        }
    }

    /// Iterations to spin for the next wait under `limit`, or zero to park at
    /// once. A zero budget spins a quarter of `limit` every [`RETRY_PERIOD`]th
    /// wait.
    pub(crate) fn rounds(&mut self, limit: u32) -> u32 {
        if limit == 0 {
            return 0;
        }
        if self.current > 0 {
            return self.current.min(limit);
        }
        self.waits_at_zero += 1;
        if self.waits_at_zero >= RETRY_PERIOD {
            self.waits_at_zero = 0;
            (limit / 4).max(MIN_SPIN).min(limit)
        } else {
            0
        }
    }

    /// The spin found what it waited for after `spent` iterations: grow to
    /// cover twice that wait, and at least double, up to `limit`.
    pub(crate) fn hit(&mut self, spent: u32, limit: u32) {
        self.current = self
            .current
            .saturating_mul(2)
            .max(spent.saturating_mul(2))
            .max(MIN_SPIN)
            .min(limit);
        self.waits_at_zero = 0;
    }

    /// The spin was wasted: shrink.
    pub(crate) fn miss(&mut self) {
        self.current /= 2;
        if self.current < MIN_SPIN {
            self.current = 0;
        }
    }
}

/// Spin up to `rounds` iterations until `ready` holds or `stop` does.
///
/// The caller records the outcome on its [`SpinBudget`]; a `stop` exit is
/// neither a hit nor a miss.
pub(crate) fn spin_until(
    rounds: u32,
    mut ready: impl FnMut() -> bool,
    mut stop: impl FnMut() -> bool,
) -> SpinOutcome {
    #[cfg(test)]
    SPIN_EPISODES.fetch_add(1, Ordering::Relaxed);
    for spent in 0..rounds {
        if ready() {
            return SpinOutcome::Ready { spent };
        }
        if stop() {
            return SpinOutcome::Stopped;
        }
        for _ in 0..PAUSES_PER_PROBE {
            std::hint::spin_loop();
        }
    }
    SpinOutcome::Exhausted
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SpinOutcome {
    /// `ready` held after `spent` iterations.
    Ready {
        spent: u32,
    },
    Exhausted,
    Stopped,
}

#[cfg(test)]
mod tests {
    use super::*;

    const LIMIT: u32 = 1024;

    #[test]
    fn budget_grows_on_hits_up_to_the_limit() {
        let mut budget = SpinBudget::new(0);
        budget.hit(0, LIMIT);
        assert_eq!(budget.rounds(LIMIT), MIN_SPIN);
        budget.hit(0, LIMIT);
        assert_eq!(
            budget.rounds(LIMIT),
            MIN_SPIN * 2,
            "a hit doubles the budget"
        );
        budget.hit(300, LIMIT);
        assert_eq!(
            budget.rounds(LIMIT),
            600,
            "a hit covers twice the wait it saw"
        );
        budget.hit(900, LIMIT);
        assert_eq!(budget.rounds(LIMIT), LIMIT);
        assert_eq!(
            budget.rounds(LIMIT / 2),
            LIMIT / 2,
            "a lowered limit caps it"
        );
    }

    #[test]
    fn wasted_spins_shrink_the_budget_to_zero() {
        let mut budget = SpinBudget::new(LIMIT);
        budget.miss();
        assert_eq!(budget.rounds(LIMIT), LIMIT / 2);
        for _ in 0..16 {
            budget.miss();
        }
        assert_eq!(budget.rounds(LIMIT), 0, "repeated misses stop spinning");
    }

    #[test]
    fn a_zero_budget_retries_periodically_and_recovers_on_a_hit() {
        let mut budget = SpinBudget::new(0);
        let retries: Vec<u32> = (0..RETRY_PERIOD * 2)
            .map(|_| budget.rounds(LIMIT))
            .collect();
        let probes = retries.iter().filter(|&&rounds| rounds != 0).count();
        assert_eq!(probes, 2, "one retry per period: {retries:?}");
        assert!(retries
            .iter()
            .all(|&rounds| rounds == 0 || rounds == LIMIT / 4));

        // The retry finds a reply after 100 iterations: the next wait covers it.
        budget.hit(100, LIMIT);
        assert_eq!(budget.rounds(LIMIT), 200);
    }

    #[test]
    fn a_zero_limit_never_spins() {
        let mut budget = SpinBudget::new(LIMIT);
        for _ in 0..RETRY_PERIOD * 2 {
            assert_eq!(budget.rounds(0), 0);
        }
    }

    #[test]
    fn spin_limit_respects_host_parallelism_and_overrides() {
        assert_eq!(parse_spin_limit("0"), Some(0));
        assert_eq!(parse_spin_limit("OFF"), Some(0));
        assert_eq!(parse_spin_limit(" 512 "), Some(512));
        assert_eq!(parse_spin_limit("lots"), None);
        assert_eq!(spin_limit_for(512, 4, 8), 512);
        assert_eq!(spin_limit_for(512, 8, 8), 512);
        assert_eq!(spin_limit_for(512, 9, 8), 0, "oversubscribed workers");
        assert_eq!(spin_limit_for(512, 1, 1), 0, "single hardware thread");
        assert_eq!(spin_limit_for(512, 0, 8), 0, "single-thread driver");
    }

    #[test]
    fn spin_until_reports_ready_stop_and_exhaustion() {
        let mut polls = 0;
        assert_eq!(
            spin_until(
                8,
                || {
                    polls += 1;
                    polls == 3
                },
                || false
            ),
            SpinOutcome::Ready { spent: 2 }
        );
        assert_eq!(spin_until(8, || false, || true), SpinOutcome::Stopped);
        assert_eq!(spin_until(8, || false, || false), SpinOutcome::Exhausted);
        assert_eq!(spin_until(0, || true, || false), SpinOutcome::Exhausted);
    }
}
