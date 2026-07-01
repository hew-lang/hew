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
use hew_mir::{
    lower_hir_module, BasicBlock, FunctionCallConv, Instr, IrPipeline, MirLint, Place,
    RawMirFunction, SelectArm, SelectArmKind, Terminator,
};
use hew_types::{module_registry::ModuleRegistry, Checker, LintId, ResolvedTy};
use std::collections::{BTreeMap, HashMap};

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

// ── Select-arm successor soundness ──────────────────────────────────
//
// These tests verify that `analyze_liveness` correctly includes `Select` arm
// body blocks as successors when computing `live_out`.  Before the canonical
// `BasicBlock::successors()` fix (NEW-A / vestigial-r2), `block.successors()`
// returned only `[next]` for a `Select` block, which made arm bodies invisible
// to every CFG pass.  The liveness module now routes through the fixed canonical
// API.  If arm bodies were again dropped from successors:
//
//   live_out(select_block) = live_in(join) only → join reads nothing → {}
//   → the store in the select block becomes live_after = {} → dead_store fires
//     on a value that IS actually read in an arm body (false positive / unsound).
//
// The two tests below pin that this cannot happen.

/// A local written before a `Select` and read in both arm bodies must be live
/// immediately after its defining store.  The synthetic CFG is:
///
/// ```text
///   Block 0: x = 42; Select { arms: [AfterTimer(dur), AfterTimer(dur)], next=3 }
///   Block 1: sink = x;  Goto { target: 3 }
///   Block 2: sink = x;  Goto { target: 3 }
///   Block 3: Return
/// ```
///
/// `Local(0) = x`, `Local(1) = dur` (timer duration — independent of x),
/// `Local(2) = sink`.  The `Select` terminator reads `dur`, NOT `x`, so x's
/// liveness at the store depends entirely on whether arm bodies 1 and 2 are
/// enumerated as CFG successors of block 0.  Before the `BasicBlock::successors()`
/// fix, only `[next=3]` was returned → arm bodies invisible → x not live at
/// the store → false-positive `dead_store`.  After the fix:
///
/// ```text
///   live_in(1) = {x}, live_in(2) = {x}
///   live_out(0) = live_in(1) ∪ live_in(2) ∪ live_in(3) ⊇ {x}
///   live_after(block0, instr0, x) = true  →  no dead_store
/// ```
#[test]
fn select_arm_read_keeps_predecessor_store_live() {
    // Local layout:
    //   Local(0) = x   (i64, user-named — the subject under test)
    //   Local(1) = dur (i64, anonymous  — consumed by AfterTimer arms, not x)
    //   Local(2) = sink(i64, anonymous  — written by arm bodies when they read x)
    let dur = Place::Local(1);
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::Local(0),
                value: 42,
            }],
            terminator: Terminator::Select {
                arms: vec![
                    SelectArm {
                        body_block: 1,
                        kind: SelectArmKind::AfterTimer { duration: dur },
                        binding: None,
                    },
                    SelectArm {
                        body_block: 2,
                        kind: SelectArmKind::AfterTimer { duration: dur },
                        binding: None,
                    },
                ],
                next: 3,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(0),
            }],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(0),
            }],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let func = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "select_arm_liveness_probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64, ResolvedTy::I64, ResolvedTy::I64],
        local_names: vec![Some("x".to_string()), None, None],
        local_scopes: vec![],
        local_decl_bytes: vec![],
        scope_table: vec![],
        blocks,
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: HashMap::new(),
        suspend_kinds: HashMap::new(),
        lambda_actor_user_param_locals: vec![],
        span: None,
        instr_spans: BTreeMap::new(),
    };

    let liveness = analyze_liveness(&func);
    let x = 0u32; // Local(0) = "x"

    // The store `x = 42` is instruction 0 of block 0.
    // Arm bodies 1 and 2 read x → they are visible as successors → x is live.
    assert!(
        liveness.live_after(&func, 0, 0, x),
        "x must be live after `x = 42` in block 0: arm bodies 1 and 2 both read it"
    );

    // Arm bodies must have x live on entry (they read it).
    assert!(
        liveness.is_live_in(1, x),
        "x must be live-in to arm body block 1"
    );
    assert!(
        liveness.is_live_in(2, x),
        "x must be live-in to arm body block 2"
    );

    // Join block does not read x — x must NOT be live-in there.
    assert!(
        !liveness.is_live_in(3, x),
        "x must not be live-in to the join block (no reads after the select)"
    );
}

/// Complement: a local written before a `Select` and NEVER read anywhere — not
/// in arm bodies, not after the join — must be dead-after-store.  This guards
/// the precision side: we must not suppress `dead_store` everywhere on Select
/// programs, only where an arm body actually reads the value.
///
/// `Local(0) = x` is the subject.  `Local(1) = dur` is an independent timer
/// duration local that the `AfterTimer` arms consume; using a separate slot
/// keeps the arm-kind reads from making `x` spuriously live.
#[test]
fn select_arm_no_read_leaves_store_dead() {
    // Block 0: x = 42; Select { arms: [AfterTimer(dur), AfterTimer(dur)], next=3 }
    //          (arm kinds read Local(1) = dur, NOT Local(0) = x)
    // Block 1: nop; Goto 3
    // Block 2: nop; Goto 3
    // Block 3: Return
    //
    // Nobody reads x anywhere — the store is provably dead.
    // Local(0) = x (i64, user-named), Local(1) = dur (i64, anonymous)
    let dur = Place::Local(1); // arm kind reads this, not x
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::Local(0),
                value: 42,
            }],
            terminator: Terminator::Select {
                arms: vec![
                    SelectArm {
                        body_block: 1,
                        kind: SelectArmKind::AfterTimer { duration: dur },
                        binding: None,
                    },
                    SelectArm {
                        body_block: 2,
                        kind: SelectArmKind::AfterTimer { duration: dur },
                        binding: None,
                    },
                ],
                next: 3,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    let func = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "select_arm_dead_probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64, ResolvedTy::I64],
        local_names: vec![Some("x".to_string()), None],
        local_scopes: vec![],
        local_decl_bytes: vec![],
        scope_table: vec![],
        blocks,
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: HashMap::new(),
        suspend_kinds: HashMap::new(),
        lambda_actor_user_param_locals: vec![],
        span: None,
        instr_spans: BTreeMap::new(),
    };

    let liveness = analyze_liveness(&func);
    let x = 0u32;

    // Nobody reads x anywhere — the store is provably dead.
    assert!(
        !liveness.live_after(&func, 0, 0, x),
        "x must be dead after `x = 42` when no arm body or successor reads it"
    );
}
