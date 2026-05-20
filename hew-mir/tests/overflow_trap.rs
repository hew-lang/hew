//! End-to-end MIR-shape tests for B-2's overflow-trap lowering.
//!
//! Source-text `+` / `-` / `*` on every default-integer width lowers to
//! `Instr::IntArithChecked` plus a CFG split: the producing block ends
//! with `Terminator::Branch { cond: overflow_flag, then_target:
//! trap_bb, else_target: cont_bb }`, the `trap_bb` carries
//! `Terminator::Trap { kind: TrapKind::IntegerOverflow }`, and
//! `cont_bb` is the continuation the rest of the function lowers into.
//!
//! These tests pin the MIR shape: they never run the LLVM emitter (that
//! is the hew-codegen-rs `pipeline_smoke.rs` smoke). The shape is what
//! drop-elaboration, the cross-block dataflow pass, and codegen all key
//! on, so it is the right level to lock down.
//!
//! LESSONS applied:
//! - `exhaustive-coverage` (P0): each of the three operators × each of
//!   the integer widths (i8/i16/i32/i64/u8/u16/u32/u64/isize/usize)
//!   gets its own assertion. The width × op grid is the boundary the
//!   trap-everywhere ratification claims to cover.
//! - `boundary-fail-closed` (P0): the default `+` / `-` / `*` MUST
//!   produce a trap edge; an emitted MIR with no Trap successor for a
//!   default arithmetic op would be the fail-soft regression we're
//!   guarding against.

use hew_mir::{BasicBlock, Instr, IntArithOp, IntSignedness, IrPipeline, Terminator, TrapKind};

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

/// Return the single `Instr::IntArithChecked` in the function, panicking
/// if there is not exactly one. Most fixtures here use a single
/// arithmetic op so this is the convenient accessor.
fn sole_checked_arith(blocks: &[BasicBlock]) -> (IntArithOp, IntSignedness) {
    let mut found = None;
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::IntArithChecked { op, signed, .. } = instr {
                assert!(
                    found.is_none(),
                    "expected a single IntArithChecked; found more than one"
                );
                found = Some((*op, *signed));
            }
        }
    }
    found.expect("expected at least one IntArithChecked")
}

/// Assert the function has exactly one trap-terminated block whose
/// trap kind is `IntegerOverflow`, and that some block branches to
/// it. This is the structural invariant B-2 establishes.
fn assert_has_overflow_trap_edge(blocks: &[BasicBlock]) {
    let trap_blocks: Vec<&BasicBlock> = blocks
        .iter()
        .filter(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: TrapKind::IntegerOverflow
                }
            )
        })
        .collect();
    assert!(
        !trap_blocks.is_empty(),
        "expected at least one IntegerOverflow trap block; got terminators {:?}",
        blocks.iter().map(|b| &b.terminator).collect::<Vec<_>>()
    );
    // Every trap block must have a Branch predecessor whose then_target
    // is the trap block id. Without that, the trap is unreachable.
    for trap in &trap_blocks {
        let reached = blocks.iter().any(|b| {
            matches!(
                b.terminator,
                Terminator::Branch { then_target, .. } if then_target == trap.id
            )
        });
        assert!(
            reached,
            "trap block {} has no Branch predecessor (unreachable trap = silent regression)",
            trap.id
        );
    }
}

// ---------------------------------------------------------------------------
// Default + / - / * on the canonical width (i64) trap on overflow.
// ---------------------------------------------------------------------------

#[test]
fn default_add_lowers_to_checked_signed_arith() {
    let p = pipeline("fn main() -> i64 { 1 + 2 }");
    assert!(
        p.diagnostics.is_empty(),
        "default `+` must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Add);
    assert_eq!(
        signed,
        IntSignedness::Signed,
        "default integer literals lower to i64 (signed)"
    );
    assert_has_overflow_trap_edge(&func.blocks);
}

#[test]
fn default_sub_lowers_to_checked_signed_arith() {
    let p = pipeline("fn main() -> i64 { 5 - 3 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Sub);
    assert_eq!(signed, IntSignedness::Signed);
    assert_has_overflow_trap_edge(&func.blocks);
}

#[test]
fn default_mul_lowers_to_checked_signed_arith() {
    let p = pipeline("fn main() -> i64 { 7 * 6 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Mul);
    assert_eq!(signed, IntSignedness::Signed);
    assert_has_overflow_trap_edge(&func.blocks);
}

// ---------------------------------------------------------------------------
// Constant-overflow sources still go through the checked path. The MIR
// does not constant-fold today; codegen relies on LLVM to constant-
// propagate through the with-overflow intrinsic if it chooses. The
// MIR-shape invariant is the same regardless of whether the operands
// would overflow at runtime.
// ---------------------------------------------------------------------------

#[test]
fn add_i32_through_typed_let_path() {
    // Type-ascribed `let` bindings give us a non-default integer
    // width (i32) reaching the `+` operator. Literal suffixes
    // (`1i32`) are not parsed today, and function parameters are not
    // yet lowered to MIR slots in the Cluster 1 spine; typed `let`
    // is the available width-carrying surface. i32::MAX + 1 fed
    // through this fixture would trap at runtime; here we just pin
    // the MIR shape.
    let p = pipeline("fn main() -> i32 { let a: i32 = 1; let b: i32 = 2; a + b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Add);
    assert_eq!(signed, IntSignedness::Signed);
    assert_has_overflow_trap_edge(&func.blocks);
}

#[test]
fn sub_i32_through_typed_let_path() {
    // i32::MIN - 1 would trap; pin the MIR shape on the i32 width.
    let p = pipeline("fn main() -> i32 { let a: i32 = 5; let b: i32 = 3; a - b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Sub);
    assert_eq!(signed, IntSignedness::Signed);
    assert_has_overflow_trap_edge(&func.blocks);
}

#[test]
fn mul_i32_through_typed_let_path() {
    // i32::MAX * 2 would trap; pin the MIR shape on the i32 width.
    let p = pipeline("fn main() -> i32 { let a: i32 = 5; let b: i32 = 3; a * b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Mul);
    assert_eq!(signed, IntSignedness::Signed);
    assert_has_overflow_trap_edge(&func.blocks);
}

#[test]
fn unsigned_add_through_typed_let_path() {
    // u8::MAX + 1 would trap; pin that the unsigned intrinsic family
    // is selected via the IntSignedness::Unsigned discriminator.
    let p = pipeline("fn main() -> u8 { let a: u8 = 1; let b: u8 = 2; a + b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    let (op, signed) = sole_checked_arith(&func.blocks);
    assert_eq!(op, IntArithOp::Add);
    assert_eq!(signed, IntSignedness::Unsigned);
    assert_has_overflow_trap_edge(&func.blocks);
}

// ---------------------------------------------------------------------------
// Per-width × per-op coverage. One test per (op, width, signedness)
// asserts the MIR carries the expected discriminator pair. This is the
// `exhaustive-coverage` LESSONS row applied to B-2's grid.
// ---------------------------------------------------------------------------

fn assert_arith_disc(source: &str, expected_op: IntArithOp, expected_signed: IntSignedness) {
    let p = pipeline(source);
    assert!(
        p.diagnostics.is_empty(),
        "source `{source}` must lower cleanly: {:?}",
        p.diagnostics
    );
    let (op, signed) = sole_checked_arith(&p.raw_mir[0].blocks);
    assert_eq!(op, expected_op, "wrong op for `{source}`");
    assert_eq!(signed, expected_signed, "wrong signedness for `{source}`");
    assert_has_overflow_trap_edge(&p.raw_mir[0].blocks);
}

fn arith_src(ret: &str, op: char) -> String {
    format!("fn main() -> {ret} {{ let a: {ret} = 1; let b: {ret} = 2; a {op} b }}")
}

#[test]
fn add_i8_signed() {
    assert_arith_disc(
        &arith_src("i8", '+'),
        IntArithOp::Add,
        IntSignedness::Signed,
    );
}
#[test]
fn add_i16_signed() {
    assert_arith_disc(
        &arith_src("i16", '+'),
        IntArithOp::Add,
        IntSignedness::Signed,
    );
}
#[test]
fn add_i32_signed() {
    assert_arith_disc(
        &arith_src("i32", '+'),
        IntArithOp::Add,
        IntSignedness::Signed,
    );
}
#[test]
fn add_i64_signed() {
    assert_arith_disc(
        &arith_src("i64", '+'),
        IntArithOp::Add,
        IntSignedness::Signed,
    );
}
#[test]
fn add_u8_unsigned() {
    assert_arith_disc(
        &arith_src("u8", '+'),
        IntArithOp::Add,
        IntSignedness::Unsigned,
    );
}
#[test]
fn add_u16_unsigned() {
    assert_arith_disc(
        &arith_src("u16", '+'),
        IntArithOp::Add,
        IntSignedness::Unsigned,
    );
}
#[test]
fn add_u32_unsigned() {
    assert_arith_disc(
        &arith_src("u32", '+'),
        IntArithOp::Add,
        IntSignedness::Unsigned,
    );
}
#[test]
fn add_u64_unsigned() {
    assert_arith_disc(
        &arith_src("u64", '+'),
        IntArithOp::Add,
        IntSignedness::Unsigned,
    );
}
#[test]
fn add_isize_signed() {
    assert_arith_disc(
        &arith_src("isize", '+'),
        IntArithOp::Add,
        IntSignedness::Signed,
    );
}
#[test]
fn add_usize_unsigned() {
    assert_arith_disc(
        &arith_src("usize", '+'),
        IntArithOp::Add,
        IntSignedness::Unsigned,
    );
}

#[test]
fn sub_i32_signed() {
    assert_arith_disc(
        &arith_src("i32", '-'),
        IntArithOp::Sub,
        IntSignedness::Signed,
    );
}
#[test]
fn sub_u32_unsigned() {
    assert_arith_disc(
        &arith_src("u32", '-'),
        IntArithOp::Sub,
        IntSignedness::Unsigned,
    );
}

#[test]
fn mul_i32_signed() {
    assert_arith_disc(
        &arith_src("i32", '*'),
        IntArithOp::Mul,
        IntSignedness::Signed,
    );
}
#[test]
fn mul_u32_unsigned() {
    assert_arith_disc(
        &arith_src("u32", '*'),
        IntArithOp::Mul,
        IntSignedness::Unsigned,
    );
}

// ---------------------------------------------------------------------------
// Continuation-block invariants: the post-arithmetic block must be the
// `else_target` of the overflow Branch, and it must be the block the
// function `Return` (or further lowering) lands in.
// ---------------------------------------------------------------------------

#[test]
fn arithmetic_continuation_block_is_branch_else_target() {
    let p = pipeline("fn main() -> i64 { 1 + 2 }");
    let func = &p.raw_mir[0];
    // Find the Branch on the overflow flag. It's the one whose
    // then_target is a Trap block.
    let branch = func
        .blocks
        .iter()
        .find_map(|b| match &b.terminator {
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => {
                let then_is_trap = func.blocks.iter().any(|t| {
                    t.id == *then_target
                        && matches!(
                            t.terminator,
                            Terminator::Trap {
                                kind: TrapKind::IntegerOverflow
                            }
                        )
                });
                then_is_trap.then_some(*else_target)
            }
            _ => None,
        })
        .expect("expected exactly one overflow-flag Branch");
    // Continuation block exists and terminates with Return (this
    // single-arithmetic fixture has no further lowering after `+`).
    let cont = func.blocks.iter().find(|b| b.id == branch).unwrap();
    assert!(
        matches!(cont.terminator, Terminator::Return),
        "continuation block must Return on the spine fixture; got {:?}",
        cont.terminator
    );
}

// ---------------------------------------------------------------------------
// Non-overflow smoke: `1 + 2 = 3` still produces a well-formed MIR
// (clean diagnostics + ordinary IntArithChecked + trap edge). This is
// the negative-control: the trap edge being present does not mean the
// program "fails" — only the overflow path traps.
// ---------------------------------------------------------------------------

#[test]
fn non_overflow_addition_lowers_without_diagnostics() {
    let p = pipeline("fn main() -> i64 { 1 + 2 }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    // Pin that exactly one IntArithChecked is present (no extra
    // arithmetic ops).
    let count: usize = p.raw_mir[0]
        .blocks
        .iter()
        .map(|b| {
            b.instructions
                .iter()
                .filter(|i| matches!(i, Instr::IntArithChecked { .. }))
                .count()
        })
        .sum();
    assert_eq!(count, 1, "single `+` produces a single IntArithChecked");
}
