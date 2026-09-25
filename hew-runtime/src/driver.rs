//! The single-thread process driver.
//!
//! wasm32 always runs on it: the target has no worker thread, so the thread
//! that waits is the thread that makes progress. Native programs select it with
//! `HEW_DETERMINISTIC` (the test runner sets it for every test that is not
//! `#[real_time]`); otherwise they run on the work-stealing scheduler.
//!
//! On the driver every participant runs on the process thread:
//!
//! - an actor with work, for one turn (one message or one resumption);
//! - a task continuation, for one resumption;
//! - a parked root or hosting boundary, when its readiness latch is set.
//!
//! A participant that becomes ready joins the back of one ready list; the list
//! order is the ticket order. `fifo` always takes the front, `random` takes
//! `splitmix64(seed) % len` (the sandbox VM's pick, so a seed names the same
//! schedule on both engines). Under a deterministic configuration the clock is
//! virtual: it starts at 0 and moves only when nothing is ready, jumping to the
//! earliest pending timer. Nothing ready, no timer pending and no host
//! operation in flight is a deadlock, reported at once.

use std::collections::VecDeque;
use std::ptr;
use std::sync::atomic::{AtomicPtr, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::activation::{activate_queued_actor, SchedulerQueueEntry};
use crate::timer_wheel::{
    hew_timer_wheel_new, hew_timer_wheel_next_deadline_ms, timer_wheel_cursor_ms,
    timer_wheel_tick_to, HewTimerWheel,
};
use crate::util::MutexExt;
use crate::wake::blocking::Readiness;

/// How the driver picks the next ready participant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Schedule {
    /// The lowest ticket: the order participants became ready.
    Fifo,
    /// A seeded uniform pick over the ready list.
    Random,
}

/// The driver configuration a process runs under.
#[derive(Debug, Clone, Copy)]
struct Config {
    schedule: Schedule,
    seed: u64,
    /// Picks allowed before the run fails with a step-budget report.
    step_budget: u64,
    /// Whether time is the driver's virtual clock or the host clock.
    virtual_clock: bool,
}

/// Default step budget for a deterministic run (§5.9 rule 9).
const DEFAULT_STEP_BUDGET: u64 = 10_000_000;

/// Exit status of a run the driver stopped: a deadlock or a spent step budget.
pub const DRIVER_FAILURE_EXIT: i32 = 3;

fn parse_number(text: &str) -> Option<u64> {
    text.strip_prefix("0x")
        .or_else(|| text.strip_prefix("0X"))
        .map_or_else(
            || text.parse().ok(),
            |hex| u64::from_str_radix(hex, 16).ok(),
        )
}

/// Parse `HEW_DETERMINISTIC=schedule=random,seed=0x..,budget=N`. Every field is
/// optional; an empty value selects `fifo`, seed 0 and the default budget.
fn parse_config(text: &str) -> Result<Config, String> {
    let mut config = Config {
        schedule: Schedule::Fifo,
        seed: 0,
        step_budget: DEFAULT_STEP_BUDGET,
        virtual_clock: true,
    };
    for field in text.split(',').map(str::trim).filter(|f| !f.is_empty()) {
        let (key, value) = field
            .split_once('=')
            .ok_or_else(|| format!("`{field}` is not `key=value`"))?;
        match key {
            "schedule" => {
                config.schedule = match value {
                    "fifo" => Schedule::Fifo,
                    "random" => Schedule::Random,
                    _ => return Err(format!("unknown schedule `{value}`")),
                }
            }
            "seed" => {
                config.seed = parse_number(value).ok_or_else(|| format!("bad seed `{value}`"))?;
            }
            "budget" => {
                config.step_budget =
                    parse_number(value).ok_or_else(|| format!("bad budget `{value}`"))?;
            }
            _ => return Err(format!("unknown field `{key}`")),
        }
    }
    Ok(config)
}

static CONFIG: OnceLock<Option<Config>> = OnceLock::new();

fn config() -> Option<&'static Config> {
    CONFIG
        .get_or_init(|| {
            let requested = std::env::var("HEW_DETERMINISTIC").ok();
            let config = match requested.as_deref().map(parse_config) {
                Some(Ok(config)) => Some(config),
                Some(Err(reason)) => {
                    eprintln!("hew: HEW_DETERMINISTIC: {reason}");
                    std::process::exit(DRIVER_FAILURE_EXIT);
                }
                // wasm32 has no worker thread: the driver is the only runtime,
                // on the host clock unless a deterministic run was requested.
                None if cfg!(target_arch = "wasm32") => Some(Config {
                    schedule: Schedule::Fifo,
                    seed: 0,
                    step_budget: u64::MAX,
                    virtual_clock: false,
                }),
                None => None,
            };
            if config.is_some_and(|c| c.virtual_clock) {
                crate::deterministic::hew_simtime_enable(0);
            }
            config
        })
        .as_ref()
}

/// Whether this process runs on the single-thread driver.
pub(crate) fn active() -> bool {
    config().is_some()
}

fn active_config() -> &'static Config {
    config().expect("driver entry outside a driver run")
}

// ---------------------------------------------------------------------------
// Timer wheel
// ---------------------------------------------------------------------------

static WHEEL: AtomicPtr<HewTimerWheel> = AtomicPtr::new(ptr::null_mut());

/// The process-wide timer wheel, created on first use.
///
/// Nothing ticks it in the background: [`step`] is the only cursor advance, so
/// a timer fires exactly when the driver next looks.
pub(crate) fn global_wheel() -> *mut HewTimerWheel {
    let existing = WHEEL.load(Ordering::Acquire);
    if !existing.is_null() {
        return existing;
    }
    // Resolve the clock before the wheel reads it for its starting cursor.
    let _ = config();
    // SAFETY: allocation only; the wheel is never freed for the process life.
    let created = unsafe { hew_timer_wheel_new() };
    match WHEEL.compare_exchange(
        ptr::null_mut(),
        created,
        Ordering::AcqRel,
        Ordering::Acquire,
    ) {
        Ok(_) => created,
        Err(winner) => winner,
    }
}

/// Start the process runtime. The wasm32 driver has no scheduler to start, so
/// this only materializes the wheel the driver ticks.
///
/// # Safety
/// No preconditions.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_sched_init() -> i32 {
    let _ = global_wheel();
    0
}

// ---------------------------------------------------------------------------
// Ready list
// ---------------------------------------------------------------------------

/// One entry of the ready list.
enum Participant {
    /// An actor with work; the entry owns one scheduler queue reference.
    Actor(*mut crate::actor::HewActor),
    /// A task continuation to resume.
    #[cfg(not(target_arch = "wasm32"))]
    Task(Arc<crate::task_scope::checked::TaskExecution>),
    /// A parked root or hosting boundary whose readiness latch is set.
    Root(Arc<Readiness>),
}

// SAFETY: the ready list is the only holder of the actor queue reference while
// the entry is queued, and it hands it to exactly one activation.
unsafe impl Send for Participant {}

/// splitmix64, bit-for-bit the sandbox VM's `SeededPrng`.
struct SplitMix64(u64);

impl SplitMix64 {
    const GOLDEN: u64 = 0x9e37_79b9_7f4a_7c15;

    fn new(seed: u64) -> Self {
        Self(seed.wrapping_add(Self::GOLDEN))
    }

    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.wrapping_add(Self::GOLDEN);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^= z >> 31;
        #[expect(clippy::cast_possible_truncation, reason = "the high 32 bits")]
        let high = (z >> 32) as u32;
        high
    }

    fn next_index(&mut self, len: usize) -> usize {
        self.next_u32() as usize % len
    }
}

struct ReadyList {
    entries: VecDeque<Participant>,
    random: Option<SplitMix64>,
}

static READY: Mutex<ReadyList> = Mutex::new(ReadyList {
    entries: VecDeque::new(),
    random: None,
});

/// Signalled when a host thread (the reactor, a supervisor restart timer)
/// publishes while the driver waits for it.
#[cfg(not(target_arch = "wasm32"))]
static HOST_PUBLISHED: std::sync::Condvar = std::sync::Condvar::new();

/// Picks taken so far.
static STEPS: AtomicU64 = AtomicU64::new(0);

/// Host operations in flight that will publish to the ready list from another
/// thread: a supervisor restart timer. Reactor waits are counted by the reactor.
static HOST_WORK: AtomicUsize = AtomicUsize::new(0);

fn publish(participant: Participant) {
    READY.lock_or_recover().entries.push_back(participant);
    #[cfg(not(target_arch = "wasm32"))]
    HOST_PUBLISHED.notify_one();
}

/// Accept a runnable actor from [`crate::resume`].
pub(crate) fn publish_queue_entry(mut entry: SchedulerQueueEntry) {
    publish(Participant::Actor(entry.actor));
    // Disarm last: it clears the entry's pointer, which is what keeps `Drop`
    // from releasing the reference the ready list now owns.
    entry.disarm();
}

/// Accept a ready task continuation.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn publish_task(task: Arc<crate::task_scope::checked::TaskExecution>) {
    publish(Participant::Task(task));
}

/// Accept a set readiness latch; its waiter resumes when the entry is picked.
pub(crate) fn publish_root(readiness: Arc<Readiness>) {
    publish(Participant::Root(readiness));
}

/// Whether any participant is waiting to run.
pub(crate) fn has_queued_work() -> bool {
    !READY.lock_or_recover().entries.is_empty()
}

/// A host operation that will publish to the driver from another thread.
/// While one is held the driver waits for it instead of reporting a deadlock.
#[derive(Debug)]
pub(crate) struct HostWork(());

impl HostWork {
    pub(crate) fn begin() -> Self {
        HOST_WORK.fetch_add(1, Ordering::AcqRel);
        Self(())
    }
}

impl Drop for HostWork {
    fn drop(&mut self) {
        HOST_WORK.fetch_sub(1, Ordering::AcqRel);
        #[cfg(not(target_arch = "wasm32"))]
        HOST_PUBLISHED.notify_one();
    }
}

fn host_work_in_flight() -> bool {
    if HOST_WORK.load(Ordering::Acquire) != 0 {
        return true;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        !crate::reactor::drain_is_idle()
    }
    #[cfg(target_arch = "wasm32")]
    {
        false
    }
}

/// Take the next participant under the configured schedule.
fn pick() -> Option<Participant> {
    let config = active_config();
    let mut ready = READY.lock_or_recover();
    let len = ready.entries.len();
    if len == 0 {
        return None;
    }
    let index = match config.schedule {
        Schedule::Fifo => 0,
        Schedule::Random => ready
            .random
            .get_or_insert_with(|| SplitMix64::new(config.seed))
            .next_index(len),
    };
    let participant = ready.entries.remove(index);
    drop(ready);
    let steps = STEPS.fetch_add(1, Ordering::Relaxed) + 1;
    if steps > config.step_budget {
        fail(&format!("step budget of {} exhausted", config.step_budget));
    }
    participant
}

fn run(participant: Participant) {
    match participant {
        Participant::Actor(actor) => activate_queued_actor(actor),
        #[cfg(not(target_arch = "wasm32"))]
        Participant::Task(task) => task.poll(),
        Participant::Root(readiness) => readiness.picked(),
    }
}

fn now_ms() -> u64 {
    // SAFETY: no preconditions.
    unsafe { crate::clock::hew_now_ms() }
}

/// Fire every timer due at the current clock. Returns how many fired.
fn fire_due_timers() -> i32 {
    // SAFETY: `global_wheel` returns the live process-owned wheel.
    unsafe { timer_wheel_tick_to(global_wheel(), now_ms()) }
}

/// Move the clock to the earliest pending timer and fire what is due there.
/// Returns false when no timer is pending.
fn advance_to_next_timer() -> bool {
    let wheel = global_wheel();
    // SAFETY: the live process-owned wheel; the query takes its own lock.
    let remaining = unsafe { hew_timer_wheel_next_deadline_ms(wheel) };
    if remaining < 0 {
        return false;
    }
    if active_config().virtual_clock {
        // SAFETY: same live wheel.
        let cursor = unsafe { timer_wheel_cursor_ms(wheel) };
        let deadline = cursor.saturating_add(remaining.cast_unsigned());
        if deadline > now_ms() {
            let _ = crate::deterministic::hew_simtime_set_ms(
                i64::try_from(deadline).unwrap_or(i64::MAX),
            );
        }
    } else if remaining > 0 {
        std::thread::sleep(std::time::Duration::from_millis(remaining.cast_unsigned()));
    }
    fire_due_timers();
    true
}

/// Report a run the driver cannot continue and end the process.
fn fail(reason: &str) -> ! {
    let config = active_config();
    let schedule = match config.schedule {
        Schedule::Fifo => "fifo",
        Schedule::Random => "random",
    };
    eprintln!(
        "hew: {reason} after {} steps at virtual time {} ms (schedule {schedule}, seed {:#x})",
        STEPS.load(Ordering::Relaxed),
        now_ms(),
        config.seed,
    );
    std::process::exit(DRIVER_FAILURE_EXIT);
}

/// Advance the process one step while a participant waits.
///
/// Runs one ready participant; with none ready, fires the timers due now; with
/// none due, moves the clock to the next timer. A host operation in flight is
/// waited for. With nothing left that could make progress the waiter can never
/// resume: that is a deadlock, and the run fails closed at once.
pub(crate) fn step() {
    if let Some(participant) = pick() {
        run(participant);
        return;
    }
    if fire_due_timers() > 0 || advance_to_next_timer() {
        return;
    }
    if host_work_in_flight() {
        wait_for_host();
        return;
    }
    fail("deadlock: nothing is runnable and no timer is pending");
}

#[cfg(not(target_arch = "wasm32"))]
fn wait_for_host() {
    let ready = READY.lock_or_recover();
    if ready.entries.is_empty() {
        // Bounded so a host completion that publishes nothing (a timer lease
        // ending in cancellation) is re-examined promptly.
        let _ = HOST_PUBLISHED.wait_timeout(ready, std::time::Duration::from_millis(10));
    }
}

#[cfg(target_arch = "wasm32")]
fn wait_for_host() {}

/// Run every ready participant and every timer due now, without moving the
/// clock, until neither has anything left.
///
/// A periodic handler re-arms its timer, so the wheel is never permanently
/// empty while one is live; quiescence is therefore "nothing ready and no timer
/// due now".
pub(crate) fn run_to_quiescence() {
    loop {
        if let Some(participant) = pick() {
            run(participant);
            continue;
        }
        if fire_due_timers() == 0 && !has_queued_work() {
            return;
        }
    }
}

/// The shutdown drain's poll: where the threaded runtime sleeps while workers
/// finish, the driver runs the work itself, moving the clock to the next timer
/// once nothing is ready so cleanup that sleeps still completes.
pub(crate) fn drain_poll(interval: std::time::Duration) {
    if !active() {
        std::thread::sleep(interval);
        return;
    }
    run_to_quiescence();
    if !advance_to_next_timer() && host_work_in_flight() {
        #[cfg(not(target_arch = "wasm32"))]
        wait_for_host();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splitmix_matches_the_sandbox_vm_sequence() {
        // Reference values from `hew-sandbox-vm/src/scheduler/prng.ts` with
        // seed 42: `new SeededPrng(42).nextUint32()` three times.
        let mut prng = SplitMix64::new(42);
        let drawn: Vec<u32> = (0..3).map(|_| prng.next_u32()).collect();
        assert_eq!(drawn, [686_809_907, 1_196_582_743, 1_478_287_871]);
    }

    #[test]
    fn config_parses_every_field() {
        let config = parse_config("schedule=random,seed=0x2a,budget=7").expect("valid");
        assert_eq!(config.schedule, Schedule::Random);
        assert_eq!(config.seed, 42);
        assert_eq!(config.step_budget, 7);
        let default = parse_config("").expect("empty is valid");
        assert_eq!(default.schedule, Schedule::Fifo);
        assert!(parse_config("schedule=chaos").is_err());
        assert!(parse_config("seed").is_err());
    }
}
