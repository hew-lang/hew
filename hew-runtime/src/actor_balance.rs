//! Exact allocation-count balance for actor boxes, and the process-exit
//! verdict built on it.
//!
//! # Why this exists
//!
//! Exit status is not a leak oracle. An actor that is spawned and never
//! reclaimed still lets its program produce the right output and return the
//! right code, so a corpus that only builds a fixture, runs it and diffs
//! `exit status + stdout` is structurally blind to a leaked actor — which is
//! exactly how `examples/v05/checked-mir/actor_ask_race.hew` kept a leaked
//! loser through both the MIR-text gate and the execution gate.
//!
//! `leaks --atExit` is not the answer either: it deadlocks (0% CPU, tracee
//! wedged in `T`, still stuck after its target is killed), and it does not
//! exist on Linux, FreeBSD or under `wasmtime`.
//!
//! What does work is an exact allocation-count balance instrumented at the
//! REAL alloc/free sites. [`record_actor_box_alloc`] is called at the
//! `Box::into_raw` in `spawn_actor_internal` — the single site that mints an
//! actor box — and [`record_actor_box_free`] at the `Box::from_raw` in
//! `free_actor_resources` — the single site that reclaims one.
//! Nothing wraps or shims the allocator, so the counts are of the actual boxes
//! the runtime handed out, not of a wrapper's idea of them.
//!
//! # The verdict
//!
//! Armed by `HEW_ACTOR_LEAK_CHECK=1` and read once, at the end of
//! `hew_runtime_cleanup` (see [`verdict_after_runtime_cleanup`]). Unbalanced ⇒
//! a diagnostic naming both counts and [`HEW_EXIT_ACTOR_LEAK`] as the process
//! exit status, so the leak is visible to any exit-status oracle — including
//! the checked-MIR execution gate, which arms the check for every fixture it
//! runs.
//!
//! It is opt-in rather than always-on because a fail-closed leak at teardown is
//! an existing, deliberate runtime policy on some paths (an actor that cannot
//! be proven wake-proof is leaked rather than freed); turning every such case
//! into a non-zero exit for every program is a behaviour change well beyond a
//! test oracle.
//!
//! # Keeping the oracle honest
//!
//! Accounting that silently stops counting — a moved call site, a bump sunk
//! into a branch nothing takes — passes every assertion while proving nothing.
//! [`leak_selftest_skips_free`] is the counterfactual: with
//! `HEW_ACTOR_LEAK_SELFTEST=skip-free` the shutdown sweep omits the free of one
//! actor it would otherwise reclaim, and the same program must then exit
//! [`HEW_EXIT_ACTOR_LEAK`]. `scripts/checked-mir-corpus.sh run` runs that
//! counterfactual before it trusts the check on any fixture, and fails the gate
//! if the deliberately leaked actor still exits cleanly.

use std::sync::atomic::{AtomicU64, Ordering};

/// Process exit status published when the actor-box balance check finds
/// unreclaimed actors at runtime cleanup.
///
/// Distinct from every status the runtime already uses (0/1 for traps and
/// scheduler faults, 101 for the actor-context abort, 124/137 for the corpus
/// wall-clock cap, 128+signo for faults) so a counterfactual cannot pass by
/// failing for an unrelated reason.
pub const HEW_EXIT_ACTOR_LEAK: i32 = 93;

/// Environment variable that arms the balance verdict.
const LEAK_CHECK_VAR: &str = "HEW_ACTOR_LEAK_CHECK";

/// Environment variable that drives the oracle's own counterfactual.
const LEAK_SELFTEST_VAR: &str = "HEW_ACTOR_LEAK_SELFTEST";

/// Actor boxes minted by `spawn_actor_internal`.
static ACTOR_BOXES_ALLOCATED: AtomicU64 = AtomicU64::new(0);

/// Actor boxes reclaimed by `free_actor_resources`.
static ACTOR_BOXES_FREED: AtomicU64 = AtomicU64::new(0);

/// Test-only, process-local counterpart of `HEW_ACTOR_LEAK_SELFTEST=skip-free`.
///
/// WASI unit tests cannot rely on mutating their inherited environment after
/// startup, so the actual-target WASM oracle arms this one-shot and drives the
/// same omission branch at the real shutdown sweep.
#[cfg(all(test, target_arch = "wasm32"))]
static OMIT_NEXT_SHUTDOWN_FREE_FOR_TEST: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Count one actor box handed out. Called at the `Box::into_raw` itself.
pub(crate) fn record_actor_box_alloc() {
    ACTOR_BOXES_ALLOCATED.fetch_add(1, Ordering::Relaxed);
}

/// Count one actor box reclaimed. Called at the `Box::from_raw` itself.
pub(crate) fn record_actor_box_free() {
    ACTOR_BOXES_FREED.fetch_add(1, Ordering::Relaxed);
}

/// `(allocated, freed)` actor boxes for this process so far.
#[must_use]
pub fn actor_box_counts() -> (u64, u64) {
    (
        ACTOR_BOXES_ALLOCATED.load(Ordering::Relaxed),
        ACTOR_BOXES_FREED.load(Ordering::Relaxed),
    )
}

/// Is the balance verdict armed for this process?
#[must_use]
pub fn leak_check_armed() -> bool {
    std::env::var(LEAK_CHECK_VAR).is_ok_and(|v| v == "1")
}

/// Counterfactual knob: should the shutdown sweep omit one actor's free?
///
/// Only honoured when the check is armed, so it cannot change the behaviour of
/// a program that is not running the oracle against itself. The wasm32 unit
/// test below has a separate one-shot in-process override because its target
/// cannot set environment variables through the host runner.
#[must_use]
pub fn leak_selftest_skips_free() -> bool {
    #[cfg(all(test, target_arch = "wasm32"))]
    if OMIT_NEXT_SHUTDOWN_FREE_FOR_TEST.swap(false, Ordering::AcqRel) {
        return true;
    }
    leak_check_armed() && std::env::var(LEAK_SELFTEST_VAR).is_ok_and(|v| v == "skip-free")
}

/// Arm one real shutdown-sweep free omission for the allocation-oracle
/// self-test. The runtime test guard serialises its sole caller.
#[cfg(all(test, target_arch = "wasm32"))]
pub(crate) fn omit_next_shutdown_free_for_test() {
    OMIT_NEXT_SHUTDOWN_FREE_FOR_TEST.store(true, Ordering::Release);
}

/// Read the balance once runtime cleanup has finished and publish the verdict.
///
/// Returns normally when the check is disarmed or the counts balance. An
/// imbalance is terminal: the diagnostic goes to stderr and the process exits
/// [`HEW_EXIT_ACTOR_LEAK`] rather than returning, because the whole point is to
/// be visible to a caller that can only see exit status.
///
/// Called at the end of `hew_runtime_cleanup`, which is the point at which
/// every actor the runtime intends to reclaim has been reclaimed. A program
/// whose graceful drain does not converge never reaches runtime cleanup at all
/// (the runtime deliberately leaves workers live and lets process exit do the
/// teardown); the check cannot speak for that path and does not claim to.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn verdict_after_runtime_cleanup() {
    if !leak_check_armed() {
        return;
    }
    let (allocated, freed) = actor_box_counts();
    if allocated == freed {
        return;
    }
    eprintln!(
        "hew: actor leak: {allocated} actor(s) spawned, {freed} reclaimed, \
         {live} still allocated after runtime cleanup",
        live = allocated.saturating_sub(freed),
    );
    std::process::exit(HEW_EXIT_ACTOR_LEAK);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The counters are monotonic and independent: a recorded alloc moves only
    /// the alloc count, a recorded free only the free count. Guards the shape
    /// the verdict subtracts, without asserting an absolute value (other tests
    /// in this process spawn and free actors concurrently).
    #[test]
    fn actor_box_counters_move_independently() {
        let (a0, f0) = actor_box_counts();
        record_actor_box_alloc();
        let (a1, f1) = actor_box_counts();
        assert!(a1 > a0, "recording an alloc must raise the alloc count");
        assert!(f1 >= f0, "counts are monotonic");
        record_actor_box_free();
        let (a2, f2) = actor_box_counts();
        assert!(a2 >= a1, "counts are monotonic");
        assert!(f2 > f1, "recording a free must raise the free count");
    }

    /// The counterfactual knob cannot fire unless the check itself is armed —
    /// a stray `HEW_ACTOR_LEAK_SELFTEST` in an unrelated environment must not
    /// make an ordinary program skip an actor free.
    #[test]
    fn selftest_knob_requires_the_check_to_be_armed() {
        assert!(
            !leak_check_armed(),
            "the runtime test process must not run with the leak check armed",
        );
        assert!(
            !leak_selftest_skips_free(),
            "the selftest knob must be inert while the check is disarmed",
        );
    }
}
