//! Faint-variable (strong-liveness) analysis + `clean_counter` MIR-lint coverage
//! (issue #2178).
//!
//! The load-bearing question for this lint is not "does it fire?" but "does it
//! discriminate?" — a shape-only matcher would flag every `c = c + k` in a loop.
//! So the positive cases are outnumbered here by negative probes that hold the
//! *shape* fixed and vary only whether the value reaches an observable:
//!
//!   * a counter whose value is returned / printed / branched on (strongly live);
//!   * a legitimate `for i in 0..n` index (feeds the header's branch condition);
//!   * an integer counter, whose checked-arith overflow flag feeds a trap branch
//!     and therefore decides whether the program traps;
//!   * two identical accumulators in one loop where exactly one is observed —
//!     the discriminating case a shape matcher cannot pass.

use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::faint::analyze_faintness;
use hew_mir::{lower_hir_module, IrPipeline, MirLint, RawMirFunction};
use hew_types::{module_registry::ModuleRegistry, Checker, LintId};

/// Full type-checked lowering — `clean_counter` is type-sensitive (it only
/// targets non-trapping float scalars), so locals must carry resolved types.
fn checked_pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "check: {:?}", tc_output.errors);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "hir: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify: {verify:?}");
    lower_hir_module(&output.module)
}

fn clean_counters(p: &IrPipeline) -> Vec<&MirLint> {
    p.lint_warnings
        .iter()
        .filter(|l| l.lint == LintId::CleanCounter)
        .collect()
}

fn dead_stores(p: &IrPipeline) -> Vec<&MirLint> {
    p.lint_warnings
        .iter()
        .filter(|l| l.lint == LintId::DeadStore)
        .collect()
}

fn func<'a>(p: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function `{name}` present in raw MIR"))
}

fn local_id(f: &RawMirFunction, name: &str) -> u32 {
    u32::try_from(
        f.local_names
            .iter()
            .position(|n| n.as_deref() == Some(name))
            .unwrap_or_else(|| panic!("local `{name}` present")),
    )
    .expect("local index fits u32")
}

// ── faint-variable query API ─────────────────────────────────────────

/// The core claim of the analysis: a purely self-referential accumulator is
/// *faint* (its value reaches no observable), while a counter whose value is
/// printed is *strongly live* — even though both are "live in the loop, dead at
/// the exit" under plain liveness. This is obstacle 2 of issue #2178.
#[test]
fn faintness_separates_dead_accumulator_from_observed_one() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 0.0;\n  var d = 0.0;\n  for i in 0..10 {\n    c = c + 1.0;\n    d = d + 2.0;\n  }\n  println(d.fmt());\n}\n",
    );
    let f = func(&p, "main");
    let faint = analyze_faintness(f);
    let c = local_id(f, "c");
    let d = local_id(f, "d");

    assert!(
        faint.is_faint(c),
        "`c` is only ever read to compute the next `c` — faint"
    );
    assert!(
        faint.is_strongly_live(d),
        "`d`'s value reaches `println` — strongly live"
    );
}

/// A `for i in 0..n` index is strongly live: its value feeds the header's
/// `icmp` into a `Branch` condition, which is observable control flow. This is
/// what stops the lint flagging every loop index.
#[test]
fn faintness_marks_for_range_index_strongly_live() {
    let p = checked_pipeline("fn main() {\n  for i in 0..10 {\n    println(i.fmt());\n  }\n}\n");
    let f = func(&p, "main");
    let faint = analyze_faintness(f);
    let i = local_id(f, "i");
    assert!(
        faint.is_strongly_live(i),
        "a for-range index feeds the loop's branch condition"
    );
}

/// An integer counter is strongly live under Hew's checked arithmetic: the
/// increment's `overflow_flag` is branched on into a `Trap`, so the counter's
/// value decides whether the program traps. Obstacle 3 of issue #2178.
#[test]
fn faintness_marks_integer_counter_strongly_live_via_overflow_trap() {
    let p =
        checked_pipeline("fn main() {\n  var c = 0;\n  for i in 0..10 {\n    c = c + 1;\n  }\n}\n");
    let f = func(&p, "main");
    let faint = analyze_faintness(f);
    let c = local_id(f, "c");
    assert!(
        faint.is_strongly_live(c),
        "checked-arith overflow flag feeds a trap branch, so `c` is strongly live"
    );
}

/// The overflow-flag chain is the *load-bearing* reason an integer counter is
/// excluded, independent of the type and shape guards. Pinned separately
/// because the three guards are redundant by design: mutation-testing the type
/// guard and the shape allowlist to admit integers leaves the lint silent
/// anyway, precisely because faintness already rules the counter strongly live.
/// If this assertion ever flips, the soundness argument has broken even if the
/// other guards still happen to mask it.
#[test]
fn integer_counter_overflow_flag_is_itself_strongly_live() {
    let p =
        checked_pipeline("fn main() {\n  var c = 0;\n  for i in 0..10 {\n    c = c + 1;\n  }\n}\n");
    let f = func(&p, "main");
    let faint = analyze_faintness(f);
    let c = local_id(f, "c");

    // `c` feeds `IntArithChecked`, which is not on the pure allowlist, so its
    // operands are seeded observable directly — no propagation edge required.
    assert!(
        faint.is_strongly_live(c),
        "the checked-add operand must be observable on its own"
    );
}

// ── clean_counter: positive ──────────────────────────────────────────

/// The canonical case: a float accumulator incremented every iteration and
/// never read. Float arithmetic has no trap edge (IEEE-754 saturates to
/// infinity), so deleting the counter is provably semantics-preserving.
#[test]
fn clean_counter_fires_on_unused_float_accumulator() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 0.0;\n  for i in 0..10 {\n    c = c + 1.0;\n  }\n}\n",
    );
    let findings = clean_counters(&p);
    assert_eq!(findings.len(), 1, "one clean_counter: {findings:?}");
    assert!(
        findings[0].message.contains("`c`") && findings[0].message.contains("dead work"),
        "message names the local and states the rule: {:?}",
        findings[0].message
    );
}

/// The shape is recovered through the temp + arith + writeback lowering for
/// multiplication too, not just `+` — obstacle 1 is about the *lowering shape*,
/// not one operator.
#[test]
fn clean_counter_fires_on_float_multiply_accumulator() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 1.0;\n  for i in 0..10 {\n    c = c * 2.0;\n  }\n}\n",
    );
    assert_eq!(
        clean_counters(&p).len(),
        1,
        "float multiply accumulator: {:?}",
        clean_counters(&p)
    );
}

/// THE discriminating test. Two accumulators of identical shape in one loop;
/// exactly one is observed afterwards. A shape-only matcher flags both, a
/// liveness-only matcher flags neither. Only a real faint-variable analysis
/// flags exactly `c`.
#[test]
fn clean_counter_fires_only_on_the_unobserved_of_two_identical_accumulators() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 0.0;\n  var d = 0.0;\n  for i in 0..10 {\n    c = c + 1.0;\n    d = d + 2.0;\n  }\n  println(d.fmt());\n}\n",
    );
    let findings = clean_counters(&p);
    assert_eq!(findings.len(), 1, "exactly one finding: {findings:?}");
    assert!(
        findings[0].message.contains("`c`"),
        "must name `c`, not `d`: {:?}",
        findings[0].message
    );
}

// ── clean_counter: precision guards (must stay silent) ───────────────

/// An integer counter must NEVER fire: removing it would delete a reachable
/// `IntegerOverflow` trap. Verified end-to-end in issue #2178 — the same program
/// starting at `i64::MAX` traps (exit 1) with the counter and prints/exits 0
/// without it.
#[test]
fn integer_counter_does_not_fire() {
    let p =
        checked_pipeline("fn main() {\n  var c = 0;\n  for i in 0..10 {\n    c = c + 1;\n  }\n}\n");
    assert!(
        clean_counters(&p).is_empty(),
        "checked integer arithmetic can trap — must not suggest removal: {:?}",
        clean_counters(&p)
    );
}

/// A counter whose value is returned is strongly live — no finding.
#[test]
fn returned_counter_does_not_fire() {
    let p = checked_pipeline(
        "fn f(n: i64) -> f64 {\n  var c = 0.0;\n  for i in 0..n {\n    c = c + 1.0;\n  }\n  c\n}\nfn main() {\n  let _ = f(3);\n}\n",
    );
    assert!(
        clean_counters(&p).is_empty(),
        "a returned counter must not fire: {:?}",
        clean_counters(&p)
    );
}

/// A counter whose value is printed after the loop is strongly live.
#[test]
fn printed_counter_does_not_fire() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 0.0;\n  for i in 0..10 {\n    c = c + 1.0;\n  }\n  println(c.fmt());\n}\n",
    );
    assert!(
        clean_counters(&p).is_empty(),
        "an observed counter must not fire: {:?}",
        clean_counters(&p)
    );
}

/// A counter read by a branch condition *inside* the loop influences control
/// flow — strongly live, no finding.
#[test]
fn counter_used_in_loop_condition_does_not_fire() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 0.0;\n  for i in 0..10 {\n    c = c + 1.0;\n    if c > 5.0 {\n      println(\"big\");\n    }\n  }\n}\n",
    );
    assert!(
        clean_counters(&p).is_empty(),
        "a counter feeding a branch must not fire: {:?}",
        clean_counters(&p)
    );
}

/// A plain `for i in 0..n` index with no accumulator at all must stay silent —
/// the lint must not treat ordinary loop machinery as dead work.
#[test]
fn plain_for_range_loop_does_not_fire() {
    let p = checked_pipeline("fn main() {\n  for i in 0..10 {\n    println(i.fmt());\n  }\n}\n");
    assert!(
        clean_counters(&p).is_empty(),
        "a plain for-range loop must not fire: {:?}",
        clean_counters(&p)
    );
}

// ── no double-firing with dead_store ─────────────────────────────────

/// `clean_counter` and `dead_store` must not both report the same code. On the
/// straight-line (non-looping) accumulate, `dead_store` owns the finding and
/// `clean_counter` stays silent — its writeback is not live out of the block, so
/// it is not a loop-carried counter.
#[test]
fn straight_line_accumulate_is_dead_store_only_not_clean_counter() {
    let p = checked_pipeline(
        "fn f() -> f64 {\n  var c = 0.0;\n  c = c + 1.0;\n  0.0\n}\nfn main() {\n  let _ = f();\n}\n",
    );
    assert!(
        clean_counters(&p).is_empty(),
        "straight-line case belongs to dead_store: {:?}",
        clean_counters(&p)
    );
}

/// The converse direction: on the loop case that `clean_counter` owns,
/// `dead_store` stays silent (the back-edge keeps the writeback live), so the
/// two lints partition the space rather than overlapping.
#[test]
fn loop_counter_is_clean_counter_only_not_dead_store() {
    let p = checked_pipeline(
        "fn main() {\n  var c = 0.0;\n  for i in 0..10 {\n    c = c + 1.0;\n  }\n}\n",
    );
    assert_eq!(clean_counters(&p).len(), 1, "clean_counter owns this case");
    assert!(
        dead_stores(&p).is_empty(),
        "dead_store must stay silent on the loop-carried counter: {:?}",
        dead_stores(&p)
    );
}
