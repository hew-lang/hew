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
//! virtual: it starts at 0, advances a millisecond per spent compute budget,
//! and jumps to the earliest pending timer when nothing is ready. Nothing ready,
//! no timer pending and no host operation in flight is a deadlock, reported at once.

use std::collections::VecDeque;
use std::ptr;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::activation::{activate_queued_actor, SchedulerQueueEntry};
use crate::timer_wheel::{hew_timer_wheel_new, HewTimerWheel};
#[cfg(target_arch = "wasm32")]
use crate::timer_wheel::{hew_timer_wheel_next_deadline_ms, timer_wheel_cursor_ms};
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

/// Snapshot the deterministic driver's replay coordinates at test exit.
pub(crate) fn report_state() -> Option<(&'static str, u64, u64, u64)> {
    let config = config().filter(|config| config.virtual_clock)?;
    let schedule = match config.schedule {
        Schedule::Fifo => "fifo",
        Schedule::Random => "random",
    };
    Some((
        schedule,
        config.seed,
        STEPS.load(Ordering::Relaxed),
        crate::deterministic::simtime_now().unwrap_or(0),
    ))
}

/// Initialize the clock mode before a program's first time read, which may
/// precede its first scheduler step.
pub fn initialize_clock() {
    let _configured = config();
}

/// Return whether this process has the deterministic test driver and virtual
/// clock. `std.testing` uses this to reject controls under `#[real_time]`.
#[no_mangle]
pub extern "C" fn hew_test_driver_active() -> bool {
    config().is_some_and(|config| config.virtual_clock)
}

/// Return the seed selected for the deterministic process entry.
/// `std.testing` checks the driver mode before calling this entry.
#[no_mangle]
pub extern "C" fn hew_test_seed() -> u64 {
    config().map_or(0, |config| config.seed)
}

/// Run ready work to quiescence without advancing the virtual clock.
/// `std.testing` checks the driver mode before calling this entry.
#[no_mangle]
pub extern "C" fn hew_test_settle() {
    if hew_test_driver_active() {
        run_to_quiescence();
    }
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
    /// A task continuation or inbound call ready to resume.
    Task(Arc<dyn crate::wake::scheduled::ReadyWork>),
    /// A parked root or hosting boundary whose readiness latch is set.
    Root(Arc<Readiness>),
    /// An admitted supervisor teardown. Teardown may step the driver while
    /// children finish, but its source-level requester has already returned.
    SupervisorStop(crate::supervisor::QueuedSupervisorStop),
    #[cfg(target_arch = "wasm32")]
    ActorFree(Vec<*mut crate::actor::HewActor>),
    #[cfg(not(target_arch = "wasm32"))]
    Shutdown,
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
        u32::try_from(z >> 32).expect("the high 32 bits fit")
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

/// Signalled when a host reactor thread publishes while the driver waits.
#[cfg(not(target_arch = "wasm32"))]
static HOST_PUBLISHED: std::sync::Condvar = std::sync::Condvar::new();

/// Picks taken so far.
static STEPS: AtomicU64 = AtomicU64::new(0);

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

/// Accept retained work whose readiness has been published.
pub(crate) fn publish_work(task: Arc<dyn crate::wake::scheduled::ReadyWork>) {
    publish(Participant::Task(task));
}

/// Accept a set readiness latch; its waiter resumes when the entry is picked.
pub(crate) fn publish_root(readiness: Arc<Readiness>) {
    publish(Participant::Root(readiness));
}

/// Enqueue a supervisor teardown after the source-level stop request returns.
pub(crate) fn publish_supervisor_stop(stop: crate::supervisor::QueuedSupervisorStop) {
    publish(Participant::SupervisorStop(stop));
}

/// Deliver a host termination request on the process thread.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn publish_shutdown() {
    publish(Participant::Shutdown);
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn discard_shutdown_requests() {
    READY
        .lock_or_recover()
        .entries
        .retain(|entry| !matches!(entry, Participant::Shutdown));
}

/// Whether any participant is waiting to run.
pub(crate) fn has_queued_work() -> bool {
    !READY.lock_or_recover().entries.is_empty()
}

fn host_work_in_flight() -> bool {
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

fn run_participant(participant: Participant) {
    match participant {
        Participant::Actor(actor) => activate_queued_actor(actor),
        Participant::Task(task) => task.poll(),
        Participant::Root(readiness) => readiness.picked(),
        Participant::SupervisorStop(stop) => stop.run(),
        #[cfg(target_arch = "wasm32")]
        Participant::ActorFree(actors) => {
            for actor in actors {
                // SAFETY: the queue entry owns these detached incarnations.
                unsafe { crate::actor::hew_actor_free(actor) };
            }
        }
        #[cfg(not(target_arch = "wasm32"))]
        Participant::Shutdown => crate::shutdown::hew_shutdown_initiate(0),
    }
}

fn now_ms() -> u64 {
    // SAFETY: no preconditions.
    unsafe { crate::clock::hew_now_ms() }
}

/// Fire every timer due at the current clock. Returns how many fired.
fn fire_due_timers() -> i32 {
    // SAFETY: `global_wheel` returns the live process-owned wheel.
    unsafe { crate::timer_wheel::hew_timer_wheel_tick(global_wheel()) }
}

/// A spent compute budget advances deterministic time and services due timers.
/// It never runs another participant inside the current activation.
pub(crate) fn safepoint_tick() {
    let Some(config) = config() else {
        return;
    };
    if config.virtual_clock {
        let next = now_ms().saturating_add(1);
        let _ = crate::deterministic::hew_simtime_set_ms(i64::try_from(next).unwrap_or(i64::MAX));
    }
    fire_due_timers();
}

/// Move the clock to the earliest pending timer and fire what is due there.
/// Returns false when no timer is pending, or when the clock cannot move: a
/// generator's clock reads zero throughout, so none of its timers can come due.
fn advance_to_next_timer() -> bool {
    if crate::generator::enabled() {
        return false;
    }
    let wheel = global_wheel();
    // SAFETY: the live process-owned wheel; the query takes its own lock.
    let Some(deadline) = (unsafe { crate::timer_wheel::timer_wheel_next_deadline(wheel) }) else {
        return false;
    };
    if active_config().virtual_clock {
        // The simulation/guest clock protocol deliberately uses milliseconds.
        // Round only at that host boundary, never inside the timer wheel.
        let deadline_ms = deadline.ceil_millis().unwrap_or(u64::MAX);
        if deadline_ms > now_ms() {
            let _ = crate::deterministic::hew_simtime_set_ms(
                i64::try_from(deadline_ms).unwrap_or(i64::MAX),
            );
        }
    } else {
        std::thread::sleep(deadline.remaining(crate::timer_wheel::now_for_wheel()));
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
        run_participant(participant);
        return;
    }
    if fire_due_timers() > 0 || advance_to_next_timer() {
        return;
    }
    if host_work_in_flight() {
        wait_for_host();
        return;
    }
    fail("deadlock: nothing is runnable and no timer can come due");
}

#[cfg(not(target_arch = "wasm32"))]
fn wait_for_host() {
    let ready = READY.lock_or_recover();
    if ready.entries.is_empty() {
        // Bounded so a reactor completion that publishes nothing is
        // re-examined promptly.
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
/// due now". Returns whether anything ran.
pub(crate) fn run_to_quiescence() -> bool {
    let mut progressed = false;
    loop {
        if let Some(participant) = pick() {
            run_participant(participant);
            progressed = true;
            continue;
        }
        if fire_due_timers() == 0 {
            if !has_queued_work() {
                return progressed;
            }
        } else {
            progressed = true;
        }
    }
}

/// Run at most one ready guest participant without blocking or advancing time.
/// Component callbacks use this same driver while external service calls wait.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_guest_step() -> bool {
    let fired = fire_due_timers();
    if let Some(participant) = pick() {
        run_participant(participant);
        true
    } else {
        fired != 0
    }
}

/// Delay until the existing guest driver's next timer, or -1 with none pending.
/// The embedding host waits for this delay or new ingress before driving again.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_guest_next_timer_ms() -> i64 {
    if has_queued_work() {
        return 0;
    }
    let wheel = global_wheel();
    // SAFETY: the process-owned wheel is live; both queries take its lock.
    let (remaining, cursor) = unsafe {
        (
            hew_timer_wheel_next_deadline_ms(wheel),
            timer_wheel_cursor_ms(wheel),
        )
    };
    if remaining < 0 {
        return -1;
    }
    let delay = cursor
        .saturating_add(remaining.cast_unsigned())
        .saturating_sub(now_ms());
    i64::try_from(delay).unwrap_or(i64::MAX)
}

#[cfg(target_arch = "wasm32")]
static GUEST_INGRESS_OPEN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);

#[cfg(target_arch = "wasm32")]
pub(crate) fn guest_ingress_open() -> bool {
    GUEST_INGRESS_OPEN.load(Ordering::Acquire)
}

/// Fence autonomous periodic admission without destroying the retained guest.
/// The host must first fence its own ingress and finish all admitted root calls,
/// including their release. Continue stepping until `hew_guest_is_drained`.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_guest_begin_drain() {
    GUEST_INGRESS_OPEN.store(false, Ordering::Release);
}

/// Whether all admitted guest work has finished, including suspended actor
/// turns, child tasks, delayed messages and queued cleanup. This does not run
/// user code or freeze a continuation. Root-call ownership remains with the
/// host, which must finish those calls before asking for this observation.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_guest_is_drained() -> bool {
    !guest_ingress_open()
        && !has_queued_work()
        && !crate::task_scope::checked::TaskExecution::has_live_tasks()
        && !crate::lifetime::live_actors::has_active_guest_actor()
        && !crate::timer_periodic::has_pending_guest_requests()
        && !crate::supervisor::roots::has_pending_guest_work()
}

/// Restore autonomous admission after a retained snapshot or aborted attempt.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_guest_resume() {
    GUEST_INGRESS_OPEN.store(true, Ordering::Release);
}

/// The shutdown drain's poll: where the threaded runtime sleeps while workers
/// finish, the driver runs the work itself, moving the clock to the next timer
/// once nothing is ready so cleanup that sleeps still completes.
///
/// Shutdown waits measure their windows on the runtime clock. While that clock
/// is simulated, the poll's wait passes in simulated time. On the driver the
/// clock moves by `interval` only when the drain is stuck: the caller's last
/// sample found work outstanding (`idle` is false), and this poll ran nothing,
/// found no timer to move to and no host work to wait for. A drain that cannot
/// converge then reaches its deadline instead of spinning on a frozen clock,
/// while one that is converging or confirming quiescence adds no virtual time
/// the program could observe. Without the driver, a simulated clock (a test
/// harness's) advances in place of sleeping unless the caller is idle.
pub(crate) fn drain_poll(interval: std::time::Duration, idle: bool) {
    if !active() {
        if crate::deterministic::simtime_now().is_some() {
            if !idle {
                advance_simulated_clock(interval);
            }
            std::thread::yield_now();
        } else {
            std::thread::sleep(interval);
        }
        return;
    }
    if run_to_quiescence() || advance_to_next_timer() {
        return;
    }
    if host_work_in_flight() {
        #[cfg(not(target_arch = "wasm32"))]
        wait_for_host();
    } else if !idle && active_config().virtual_clock {
        advance_simulated_clock(interval);
    }
}

fn advance_simulated_clock(interval: std::time::Duration) {
    let millis = i64::try_from(interval.as_nanos().div_ceil(1_000_000)).unwrap_or(i64::MAX);
    if millis > 0 {
        let _ = crate::deterministic::hew_simtime_advance_ms(millis);
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

#[cfg(target_arch = "wasm32")]
pub(crate) fn publish_actor_free(actors: Vec<*mut crate::actor::HewActor>) {
    publish(Participant::ActorFree(actors));
}
