//! MIR-shape tests for B-4's wrapping operator lowering.
//!
//! `&+` / `&-` / `&*` must lower to plain `Instr::IntAdd` / `IntSub` /
//! `IntMul` — no overflow flag, no CFG split, no Trap block. These are
//! the first source-level producers of those three unchecked-arithmetic
//! variants.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the *absence* of a Trap block is the
//!   correctness invariant here. A wrapping op that accidentally emits
//!   `IntArithChecked` would silently change user-visible semantics.
//! - `exhaustive-coverage` (P0): one test per operator confirms the
//!   discriminator; a width-coverage test pins that the same lowering
//!   holds for non-default widths.

use hew_mir::{BasicBlock, Instr, IrPipeline, Terminator, TrapKind};

fn pipeline(source: &str) -> IrPipeline {
    use hew_hir::{lower_program, verify_hir, ResolutionCtx};
    use hew_types::TypeCheckOutput;

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    hew_mir::lower_hir_module(&output.module)
}

fn checked_pipeline(source: &str) -> IrPipeline {
    use hew_hir::{lower_program, verify_hir, ResolutionCtx};
    use hew_types::{module_registry::ModuleRegistry, Checker};

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    hew_mir::lower_hir_module(&output.module)
}

/// Assert the function's blocks contain exactly one `Instr::IntAdd` and
/// no `IntArithChecked` or `Terminator::Trap { IntegerOverflow }`.
fn assert_sole_int_add(blocks: &[BasicBlock]) {
    let add_count: usize = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntAdd { .. }))
        .count();
    assert_eq!(add_count, 1, "expected exactly one IntAdd; got {add_count}");

    let checked_count: usize = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntArithChecked { .. }))
        .count();
    assert_eq!(
        checked_count, 0,
        "`&+` must not emit IntArithChecked (would add unintended trap)"
    );

    assert_no_overflow_trap(blocks);
}

fn assert_sole_int_sub(blocks: &[BasicBlock]) {
    let sub_count: usize = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntSub { .. }))
        .count();
    assert_eq!(sub_count, 1, "expected exactly one IntSub; got {sub_count}");

    let checked_count: usize = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntArithChecked { .. }))
        .count();
    assert_eq!(
        checked_count, 0,
        "`&-` must not emit IntArithChecked (would add unintended trap)"
    );

    assert_no_overflow_trap(blocks);
}

fn assert_sole_int_mul(blocks: &[BasicBlock]) {
    let mul_count: usize = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntMul { .. }))
        .count();
    assert_eq!(mul_count, 1, "expected exactly one IntMul; got {mul_count}");

    let checked_count: usize = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntArithChecked { .. }))
        .count();
    assert_eq!(
        checked_count, 0,
        "`&*` must not emit IntArithChecked (would add unintended trap)"
    );

    assert_no_overflow_trap(blocks);
}

/// Confirm no block terminates with `Terminator::Trap { IntegerOverflow }`.
fn assert_no_overflow_trap(blocks: &[BasicBlock]) {
    for block in blocks {
        assert!(
            !matches!(
                block.terminator,
                Terminator::Trap {
                    kind: TrapKind::IntegerOverflow
                }
            ),
            "wrapping op must not emit an IntegerOverflow trap block; \
             terminator was {:?}",
            block.terminator
        );
    }
}

// ---------------------------------------------------------------------------
// Default width (i64): one test per operator.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_lowering_add_i64_emits_int_add_not_checked() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a &+ b }");
    assert!(
        p.diagnostics.is_empty(),
        "`&+` must lower cleanly: {:?}",
        p.diagnostics
    );
    assert_sole_int_add(&p.raw_mir[0].blocks);
}

#[test]
fn wrapping_method_add_i64_emits_int_add_not_checked_option() {
    let p =
        checked_pipeline("fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a.wrapping_add(b) }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_sole_int_add(&p.raw_mir[0].blocks);
    let checked_option_count = p.raw_mir[0]
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntArithCheckedOption { .. }))
        .count();
    assert_eq!(
        checked_option_count, 0,
        "wrapping method must not lower through checked-Option MIR"
    );
}

#[test]
fn wrapping_lowering_sub_i64_emits_int_sub_not_checked() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 5; let b: i64 = 3; a &- b }");
    assert!(
        p.diagnostics.is_empty(),
        "`&-` must lower cleanly: {:?}",
        p.diagnostics
    );
    assert_sole_int_sub(&p.raw_mir[0].blocks);
}

#[test]
fn wrapping_lowering_mul_i64_emits_int_mul_not_checked() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 7; let b: i64 = 6; a &* b }");
    assert!(
        p.diagnostics.is_empty(),
        "`&*` must lower cleanly: {:?}",
        p.diagnostics
    );
    assert_sole_int_mul(&p.raw_mir[0].blocks);
}

// ---------------------------------------------------------------------------
// Non-default width: i32. Confirms width-agnostic lowering.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_lowering_add_i32_emits_int_add() {
    let p = pipeline("fn main() -> i32 { let a: i32 = 1; let b: i32 = 2; a &+ b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_sole_int_add(&p.raw_mir[0].blocks);
}

#[test]
fn wrapping_lowering_sub_i32_emits_int_sub() {
    let p = pipeline("fn main() -> i32 { let a: i32 = 5; let b: i32 = 3; a &- b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_sole_int_sub(&p.raw_mir[0].blocks);
}

#[test]
fn wrapping_lowering_mul_i32_emits_int_mul() {
    let p = pipeline("fn main() -> i32 { let a: i32 = 5; let b: i32 = 3; a &* b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_sole_int_mul(&p.raw_mir[0].blocks);
}

// ---------------------------------------------------------------------------
// Unsigned width: u32. Confirms the lowering is signedness-agnostic
// (no IntSignedness needed for unchecked ops).
// ---------------------------------------------------------------------------

#[test]
fn wrapping_lowering_add_u32_emits_int_add() {
    let p = pipeline("fn main() -> u32 { let a: u32 = 1; let b: u32 = 2; a &+ b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_sole_int_add(&p.raw_mir[0].blocks);
}

#[test]
fn wrapping_lowering_mul_u32_emits_int_mul() {
    let p = pipeline("fn main() -> u32 { let a: u32 = 5; let b: u32 = 3; a &* b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_sole_int_mul(&p.raw_mir[0].blocks);
}

// ---------------------------------------------------------------------------
// CFG shape: a single wrapping op must produce a single-block function
// (no CFG split unlike IntArithChecked which always splits the block).
// ---------------------------------------------------------------------------

#[test]
fn wrapping_add_produces_no_cfg_split() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a &+ b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    // With IntArithChecked there are always ≥3 blocks (producing, trap,
    // continuation). A plain IntAdd in a simple return-value function
    // needs only 1 block (entry = return block).
    // We assert there is NO overflow-trap block and that the function
    // terminates with Return — both confirm no CFG split occurred.
    let blocks = &p.raw_mir[0].blocks;
    let has_return = blocks
        .iter()
        .any(|b| matches!(b.terminator, Terminator::Return));
    assert!(has_return, "function must have a Return terminator");
    // Trap count for IntegerOverflow must be zero.
    let overflow_traps: usize = blocks
        .iter()
        .filter(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: TrapKind::IntegerOverflow
                }
            )
        })
        .count();
    assert_eq!(
        overflow_traps, 0,
        "wrapping add must produce 0 IntegerOverflow trap blocks; got {overflow_traps}"
    );
}
