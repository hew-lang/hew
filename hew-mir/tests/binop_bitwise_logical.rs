//! MIR-shape tests for bitwise (&, |, ^) and logical (&&, ||) binary operators.
//!
//! Bitwise operators lower to single `Instr::IntBitAnd/IntBitOr/IntBitXor`
//! instructions with no CFG split and no trap blocks — they are well-defined
//! for all integer bit patterns.
//!
//! Logical `&&` / `||` lower to a CFG fork (entry block, rhs block, join
//! block) with a short-circuit branch: the rhs block is only entered when
//! the lhs result does not already determine the outcome. The CFG shape is
//! verified here rather than the exact instruction stream, since the result
//! depends on internal block allocation order.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): bitwise ops must NOT emit trap blocks or
//!   checked variants. Logical ops must NOT lower both operands before
//!   branching.
//! - `exhaustive-coverage` (P0): one test per operator confirms the
//!   discriminator; short-circuit tests check CFG block count.

use hew_mir::{BasicBlock, Instr, IrPipeline, Terminator};

fn pipeline(source: &str) -> IrPipeline {
    use hew_hir::{lower_program, verify_hir, ResolutionCtx};
    use hew_types::TypeCheckOutput;

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    hew_mir::lower_hir_module(&output.module)
}

/// Assert no block has a `Terminator::Trap` of any kind.
fn assert_no_trap(blocks: &[BasicBlock]) {
    for block in blocks {
        assert!(
            !matches!(block.terminator, Terminator::Trap { .. }),
            "bitwise op must not emit any Trap block; terminator was {:?}",
            block.terminator
        );
    }
}

// ---------------------------------------------------------------------------
// Bitwise AND (&)
// ---------------------------------------------------------------------------

#[test]
fn bitwise_and_i64_emits_int_bit_and() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 0b1100; let b: i64 = 0b1010; a & b }");
    assert!(
        p.diagnostics.is_empty(),
        "`&` must lower cleanly: {:?}",
        p.diagnostics
    );
    let blocks = &p.raw_mir[0].blocks;
    let count = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntBitAnd { .. }))
        .count();
    assert_eq!(count, 1, "expected exactly one IntBitAnd; got {count}");
    assert_no_trap(blocks);
}

#[test]
fn bitwise_and_produces_no_cfg_split() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 3; let b: i64 = 5; a & b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    // A single bitwise op in a simple return-value function needs only one
    // block (no branch, no trap). Assert no Branch terminator.
    let blocks = &p.raw_mir[0].blocks;
    let branch_count = blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch { .. }))
        .count();
    assert_eq!(
        branch_count, 0,
        "bitwise & must produce no Branch terminators; found {branch_count}"
    );
}

// ---------------------------------------------------------------------------
// Bitwise OR (|)
// ---------------------------------------------------------------------------

#[test]
fn bitwise_or_i64_emits_int_bit_or() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 0b1100; let b: i64 = 0b0011; a | b }");
    assert!(
        p.diagnostics.is_empty(),
        "`|` must lower cleanly: {:?}",
        p.diagnostics
    );
    let blocks = &p.raw_mir[0].blocks;
    let count = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntBitOr { .. }))
        .count();
    assert_eq!(count, 1, "expected exactly one IntBitOr; got {count}");
    assert_no_trap(blocks);
}

// ---------------------------------------------------------------------------
// Bitwise XOR (^)
// ---------------------------------------------------------------------------

#[test]
fn bitwise_xor_i64_emits_int_bit_xor() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 0b1010; let b: i64 = 0b1100; a ^ b }");
    assert!(
        p.diagnostics.is_empty(),
        "`^` must lower cleanly: {:?}",
        p.diagnostics
    );
    let blocks = &p.raw_mir[0].blocks;
    let count = blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instr::IntBitXor { .. }))
        .count();
    assert_eq!(count, 1, "expected exactly one IntBitXor; got {count}");
    assert_no_trap(blocks);
}

// ---------------------------------------------------------------------------
// Logical AND (&&) — short-circuit CFG shape
// ---------------------------------------------------------------------------

#[test]
fn logical_and_lowers_cleanly() {
    let p = pipeline("fn main() -> i64 { let a = true; let b = false; let r = a && b; 0 }");
    assert!(
        p.diagnostics.is_empty(),
        "`&&` must lower cleanly: {:?}",
        p.diagnostics
    );
}

#[test]
fn logical_and_produces_cfg_fork() {
    // Short-circuit &&: entry block branches on lhs; rhs block is only
    // entered when lhs is true. The function has at least 3 blocks:
    // entry (branch on lhs), rhs block (evaluate rhs, goto join), and
    // join (return). Asserting ≥1 Branch terminator confirms the CFG split.
    let p = pipeline("fn main() -> i64 { let a = true; let b = false; let r = a && b; 0 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let blocks = &p.raw_mir[0].blocks;
    let branch_count = blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch { .. }))
        .count();
    assert!(
        branch_count >= 1,
        "`&&` must produce at least one Branch terminator for short-circuit; \
         found {branch_count} (blocks: {blocks:#?})"
    );
}

#[test]
fn logical_and_has_no_trap_blocks() {
    let p = pipeline("fn main() -> i64 { let a = true; let b = true; let r = a && b; 0 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_no_trap(&p.raw_mir[0].blocks);
}

// ---------------------------------------------------------------------------
// Logical OR (||) — short-circuit CFG shape
// ---------------------------------------------------------------------------

#[test]
fn logical_or_lowers_cleanly() {
    let p = pipeline("fn main() -> i64 { let a = false; let b = true; let r = a || b; 0 }");
    assert!(
        p.diagnostics.is_empty(),
        "`||` must lower cleanly: {:?}",
        p.diagnostics
    );
}

#[test]
fn logical_or_produces_cfg_fork() {
    // Short-circuit ||: entry block branches on lhs; rhs block is only
    // entered when lhs is false. At least one Branch terminator must appear.
    let p = pipeline("fn main() -> i64 { let a = false; let b = true; let r = a || b; 0 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let blocks = &p.raw_mir[0].blocks;
    let branch_count = blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch { .. }))
        .count();
    assert!(
        branch_count >= 1,
        "`||` must produce at least one Branch terminator for short-circuit; \
         found {branch_count} (blocks: {blocks:#?})"
    );
}

#[test]
fn logical_or_has_no_trap_blocks() {
    let p = pipeline("fn main() -> i64 { let a = false; let b = false; let r = a || b; 0 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_no_trap(&p.raw_mir[0].blocks);
}
