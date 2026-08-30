//! Per-function, per-stage lowering timings behind `HEW_MEASURE_TIMINGS`.
//!
//! MIR lowering runs a long fixed sequence of ownership passes over every
//! function body, so a compile-time regression is only actionable when the
//! report names both the function and the pass. `hew check` never reaches the
//! driver's `measure_compile_phase` (that instruments the build path only), so
//! the accounting lives here, beside the passes it measures.
//!
//! Disabled by default: [`enabled`] reads the environment once and every
//! recording site is a predictable, always-false branch when the variable is
//! absent.

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::OnceLock;
use std::time::{Duration, Instant};

/// Whether `HEW_MEASURE_TIMINGS` was set when this process started.
#[must_use]
pub fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("HEW_MEASURE_TIMINGS").is_some())
}

#[derive(Default)]
struct Accumulator {
    /// Total time and call count per named stage, across every function.
    stages: HashMap<&'static str, (Duration, u64)>,
    /// Total lowering time per function symbol.
    functions: HashMap<String, Duration>,
    /// How many whole-function derivations each named analysis made.
    ///
    /// A stage total says a pass is expensive; it does not say whether the
    /// expense is one derivation per body or one per block. This total does,
    /// and `make bench-mir` compares it across two sizes of one program.
    totals: HashMap<&'static str, u64>,
    /// The same count broken down by call site, when the build knows it.
    derivations: HashMap<(&'static str, String), u64>,
    /// Function bodies whose lowering has been attributed.
    bodies: u64,
}

thread_local! {
    static ACCUMULATOR: RefCell<Accumulator> = RefCell::new(Accumulator::default());
}

/// A running stage measurement; the elapsed time is recorded when it drops.
#[derive(Debug)]
pub struct StageTimer {
    stage: &'static str,
    started: Instant,
}

impl Drop for StageTimer {
    fn drop(&mut self) {
        let elapsed = self.started.elapsed();
        let stage = self.stage;
        ACCUMULATOR.with(|accumulator| {
            let mut accumulator = accumulator.borrow_mut();
            let slot = accumulator
                .stages
                .entry(stage)
                .or_insert((Duration::ZERO, 0));
            slot.0 += elapsed;
            slot.1 += 1;
        });
    }
}

/// Start timing a named lowering stage, or return `None` when measurement is off.
#[must_use]
pub fn stage(stage: &'static str) -> Option<StageTimer> {
    enabled().then(|| StageTimer {
        stage,
        started: Instant::now(),
    })
}

/// Attribute the time since `started` to a named stage.
///
/// The RAII [`stage`] guard cannot measure a scope that ends by calling
/// [`report`] — the guard would still be alive — so whole-module accounting
/// records its own elapsed time.
pub fn record_stage(stage: &'static str, started: Option<Instant>) {
    let Some(started) = started else {
        return;
    };
    let elapsed = started.elapsed();
    ACCUMULATOR.with(|accumulator| {
        let mut accumulator = accumulator.borrow_mut();
        let slot = accumulator
            .stages
            .entry(stage)
            .or_insert((Duration::ZERO, 0));
        slot.0 += elapsed;
        slot.1 += 1;
    });
}

/// Count one whole-function analysis derivation.
///
/// The total is what says whether lowering cost tracks the pass sequence or the
/// block count, and `make bench-mir` gates on it. `caller` names the line that
/// asked, which is what says WHICH pass to look at; only a build that can pay
/// for `#[track_caller]` supplies it, so the site table is a debug-build
/// facility and the totals are always reported.
pub fn derivation(analysis: &'static str, caller: Option<&'static std::panic::Location<'static>>) {
    if !enabled() {
        return;
    }
    ACCUMULATOR.with(|accumulator| {
        let mut accumulator = accumulator.borrow_mut();
        *accumulator.totals.entry(analysis).or_insert(0) += 1;
        if let Some(caller) = caller {
            let site = format!("{}:{}", caller.file(), caller.line());
            *accumulator.derivations.entry((analysis, site)).or_insert(0) += 1;
        }
    });
}

/// Mark the start of one function's lowering, or `None` when measurement is off.
///
/// The symbol is not known until the body is sealed, so the start and the
/// attribution are two calls rather than an RAII guard.
#[must_use]
pub fn function_start() -> Option<Instant> {
    enabled().then(Instant::now)
}

/// Attribute the time since `started` to one function symbol.
pub fn function_end(symbol: &str, started: Option<Instant>) {
    let Some(started) = started else {
        return;
    };
    let elapsed = started.elapsed();
    ACCUMULATOR.with(|accumulator| {
        let mut accumulator = accumulator.borrow_mut();
        accumulator.bodies += 1;
        *accumulator
            .functions
            .entry(symbol.to_string())
            .or_insert(Duration::ZERO) += elapsed;
    });
}

/// Print the accumulated stage and function totals to stderr and reset them.
///
/// `limit` caps how many of the slowest functions are named; the stage table is
/// printed in full because it is bounded by the number of instrumented passes.
pub fn report(limit: usize) {
    if !enabled() {
        return;
    }
    let (stages, functions, derivations, totals, bodies) = ACCUMULATOR.with(|accumulator| {
        let mut accumulator = accumulator.borrow_mut();
        let bodies = std::mem::take(&mut accumulator.bodies);
        (
            std::mem::take(&mut accumulator.stages),
            std::mem::take(&mut accumulator.functions),
            std::mem::take(&mut accumulator.derivations),
            std::mem::take(&mut accumulator.totals),
            bodies,
        )
    });

    let mut stages: Vec<_> = stages.into_iter().collect();
    stages.sort_by_key(|(_, (elapsed, _))| std::cmp::Reverse(*elapsed));
    for (stage, (elapsed, calls)) in stages {
        eprintln!(
            "hew measure: mir stage {stage} {:.3} ms ({calls} calls)",
            elapsed.as_secs_f64() * 1_000.0
        );
    }

    let mut derivations: Vec<_> = derivations.into_iter().collect();
    derivations.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
    for ((analysis, site), count) in &derivations {
        eprintln!("hew measure: mir derivation {analysis} {site} {count}");
    }
    let mut totals: Vec<_> = totals.into_iter().collect();
    totals.sort_unstable();
    for (analysis, count) in totals {
        // Printed as two integers rather than a ratio: the complexity gate in
        // `scripts/bench-mir.sh` divides them, and an integer pair parses the
        // same on every locale and rounding mode.
        eprintln!("hew measure: mir derivations {analysis} {count} bodies {bodies}");
    }

    let mut functions: Vec<_> = functions.into_iter().collect();
    functions.sort_by_key(|(_, elapsed)| std::cmp::Reverse(*elapsed));
    for (symbol, elapsed) in functions.into_iter().take(limit) {
        eprintln!(
            "hew measure: mir fn {symbol} {:.3} ms",
            elapsed.as_secs_f64() * 1_000.0
        );
    }
}
