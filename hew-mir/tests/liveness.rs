//! Backward liveness + `dead_store` MIR-lint coverage.
//!
//! Exercises the liveness query API directly (a dead store's destination is not
//! live afterwards, a surviving store's is) and the `dead_store` lint surfaced
//! on `IrPipeline::lint_warnings`: positive cases plus the precision guards that
//! must stay silent — a `for i in 0..n` counter, a loop-carried accumulator, a
//! heap-typed (drop-bearing) store, and a value read immediately after the
//! store.

use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::liveness::analyze_liveness;
use hew_mir::{lower_hir_module, Instr, IrPipeline, MirLint, Place, RawMirFunction};
use hew_types::{module_registry::ModuleRegistry, Checker, LintId};

/// Full type-checked lowering — `dead_store` is type-sensitive (it only targets
/// no-drop scalars), so the locals must carry resolved types.
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

// ── liveness query API ───────────────────────────────────────────────

/// In `var x = 5; x = 6; x`, the first store (`x = 5`) is dead — `x` is not live
/// immediately after it — while the second (`x = 6`) is live (its value is
/// returned). The per-point query must distinguish the two stores into the same
/// local.
#[test]
fn liveness_distinguishes_dead_and_live_store_into_same_local() {
    let p = checked_pipeline(
        "fn f() -> i64 {\n  var x = 5;\n  x = 6;\n  x\n}\nfn main() {\n  let _ = f();\n}\n",
    );
    let f = func(&p, "f");
    let liveness = analyze_liveness(f);
    let x = local_id(f, "x");

    let mut dead = 0u32;
    let mut live = 0u32;
    for block in &f.blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            if let Instr::Move {
                dest: Place::Local(n),
                ..
            } = instr
            {
                if *n == x {
                    if liveness.live_after(f, block.id, idx, x) {
                        live += 1;
                    } else {
                        dead += 1;
                    }
                }
            }
        }
    }
    assert_eq!(dead, 1, "exactly the `x = 5` store is dead");
    assert!(live >= 1, "the surviving `x = 6` store is live");
}

/// A returned local is live immediately after its defining store (the return
/// reads it). Block-boundary liveness would miss this when define + return live
/// in one block, so we assert at the program point.
#[test]
fn liveness_marks_returned_local_live_after_definition() {
    let p =
        checked_pipeline("fn f() -> i64 {\n  let x = 7;\n  x\n}\nfn main() {\n  let _ = f();\n}\n");
    let f = func(&p, "f");
    let liveness = analyze_liveness(f);
    let x = local_id(f, "x");
    let mut found = false;
    for block in &f.blocks {
        for (idx, instr) in block.instructions.iter().enumerate() {
            if let Instr::Move {
                dest: Place::Local(n),
                ..
            } = instr
            {
                if *n == x {
                    assert!(
                        liveness.live_after(f, block.id, idx, x),
                        "`x` must be live after its definition (it is returned)"
                    );
                    found = true;
                }
            }
        }
    }
    assert!(found, "the defining store of `x` is a Move");
}

// ── dead_store: positive ─────────────────────────────────────────────

/// A scalar overwritten before it is read fires exactly once, on the dead store.
#[test]
fn dead_store_fires_on_overwritten_scalar() {
    let p = checked_pipeline(
        "fn f() -> i64 {\n  var x = 5;\n  x = 6;\n  x\n}\nfn main() {\n  let _ = f();\n}\n",
    );
    let findings = dead_stores(&p);
    assert_eq!(findings.len(), 1, "one dead store: {findings:?}");
    assert!(
        findings[0].message.contains("`x`") && findings[0].message.contains("is never read"),
        "message names the local and states the rule: {:?}",
        findings[0].message
    );
}

/// A scalar binding whose value is never read at all fires once.
#[test]
fn dead_store_fires_on_unused_scalar() {
    let p = checked_pipeline(
        "fn f() -> i64 {\n  var x = 5;\n  let y = 10;\n  y\n}\nfn main() {\n  let _ = f();\n}\n",
    );
    let findings = dead_stores(&p);
    assert_eq!(
        findings.len(),
        1,
        "one dead store on the unused `x`: {findings:?}"
    );
    assert!(
        findings[0].message.contains("`x`"),
        "{:?}",
        findings[0].message
    );
}

// ── dead_store: precision guards (must stay silent) ──────────────────

/// `for i in 0..n` with the counter used normally and a loop-carried
/// accumulator read after the loop must NOT fire: the back-edge keeps both `i`
/// and `sum` live across the increment, so neither store is dead. This is the
/// load-bearing precision guard.
#[test]
fn for_range_counter_and_accumulator_do_not_fire() {
    let p = checked_pipeline("fn f(n: i64) -> i64 {\n  var sum = 0;\n  for i in 0..n {\n    sum = sum + i;\n  }\n  sum\n}\nfn main() {\n  let _ = f(3);\n}\n");
    assert!(
        dead_stores(&p).is_empty(),
        "for-range counter / accumulator must not be flagged: {:?}",
        dead_stores(&p)
    );
}

/// A heap-typed (`string`) store is excluded — `dead_store` only targets no-drop
/// scalars, so drop-safety is never in question.
#[test]
fn heap_typed_store_does_not_fire() {
    let p = checked_pipeline("fn f() -> string {\n  var s = \"a\";\n  s = \"b\";\n  s\n}\nfn main() {\n  let _ = f();\n}\n");
    assert!(
        dead_stores(&p).is_empty(),
        "string store must not be flagged: {:?}",
        dead_stores(&p)
    );
}

/// A value read immediately after assignment is live — no finding.
#[test]
fn live_store_does_not_fire() {
    let p =
        checked_pipeline("fn f() -> i64 {\n  let x = 5;\n  x\n}\nfn main() {\n  let _ = f();\n}\n");
    assert!(
        dead_stores(&p).is_empty(),
        "a read value must not be flagged: {:?}",
        dead_stores(&p)
    );
}

/// A clean program (only the stdlib prelude beyond `main`) produces no MIR lint
/// warnings — the pass must not spuriously fire inside the prelude.
#[test]
fn clean_program_has_no_findings() {
    let p = checked_pipeline("fn main() {\n  let _ = 1 + 2;\n}\n");
    assert!(
        p.lint_warnings.is_empty(),
        "clean program must have no MIR lint warnings: {:?}",
        p.lint_warnings
    );
}
