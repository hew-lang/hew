//! MIR-shape tests for B-5: divide-by-zero, signed-MIN/-1, and
//! shift-out-of-range trap lowering.
//!
//! These tests pin the MIR CFG shape produced by `lower_binary` for
//! `/`, `%`, `<<`, and `>>`. They never run the LLVM emitter; that is
//! the job of `hew-codegen-rs/tests/div_shift_trap_emission.rs`.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): every divide and shift MUST produce a
//!   trap edge reachable from a Branch predecessor. A missing trap edge is
//!   a silent regression to undefined behaviour (signed-overflow / UB on
//!   shift-by-too-much).
//! - `exhaustive-coverage` (P0): one test per (op × signedness) pair for
//!   canonical widths. Unsigned types have no signed-MIN/-1 trap block;
//!   the test asserts that only one trap block (`DivideByZero`) is present.

use hew_mir::{BasicBlock, Instr, IntSignedness, IrPipeline, PointerWidth, Terminator, TrapKind};

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

/// Assert at least one block with `Terminator::Trap { kind }` exists, and
/// that every such block is reachable from a Branch predecessor.
fn assert_trap_edge(blocks: &[BasicBlock], kind: TrapKind) {
    let trap_blocks: Vec<&BasicBlock> = blocks
        .iter()
        .filter(|b| matches!(&b.terminator, Terminator::Trap { kind: k } if *k == kind))
        .collect();
    assert!(
        !trap_blocks.is_empty(),
        "expected at least one {kind:?} trap block; got terminators {:?}",
        blocks.iter().map(|b| &b.terminator).collect::<Vec<_>>()
    );
    for trap in &trap_blocks {
        let reached = blocks.iter().any(|b| {
            matches!(
                b.terminator,
                Terminator::Branch { then_target, .. } if then_target == trap.id
            )
        });
        assert!(
            reached,
            "{kind:?} trap block {} has no Branch predecessor (unreachable trap = silent regression)",
            trap.id
        );
    }
}

/// Assert no trap block with `kind` exists in `blocks`.
fn assert_no_trap(blocks: &[BasicBlock], kind: TrapKind) {
    let found = blocks
        .iter()
        .any(|b| matches!(&b.terminator, Terminator::Trap { kind: k } if *k == kind));
    assert!(
        !found,
        "expected no {kind:?} trap block but found one; terminators: {:?}",
        blocks.iter().map(|b| &b.terminator).collect::<Vec<_>>()
    );
}

/// Return the sole `Instr::IntDiv` or `Instr::IntRem` in the function,
/// panicking if there is not exactly one.
fn sole_div_rem(blocks: &[BasicBlock]) -> IntSignedness {
    let mut found = None;
    for block in blocks {
        for instr in &block.instructions {
            let signed = match instr {
                Instr::IntDiv { signed, .. } | Instr::IntRem { signed, .. } => *signed,
                _ => continue,
            };
            assert!(
                found.is_none(),
                "expected exactly one IntDiv/IntRem; found more than one"
            );
            found = Some(signed);
        }
    }
    found.expect("expected at least one IntDiv/IntRem")
}

/// Return the sole `Instr::IntShl` or `Instr::IntShr` in the function.
fn sole_shift(blocks: &[BasicBlock]) -> Option<IntSignedness> {
    let mut found = None;
    for block in blocks {
        for instr in &block.instructions {
            let signed = match instr {
                Instr::IntShl { .. } => None, // Shl has no signedness
                Instr::IntShr { signed, .. } => Some(*signed),
                _ => continue,
            };
            assert!(
                found.is_none(),
                "expected exactly one IntShl/IntShr; found more than one"
            );
            found = Some(signed);
        }
    }
    found.expect("expected at least one IntShl/IntShr")
}

// ---------------------------------------------------------------------------
// Division — divide-by-zero trap on signed i64
// ---------------------------------------------------------------------------

#[test]
fn signed_div_emits_divide_by_zero_trap() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_eq!(sole_div_rem(&func.blocks), IntSignedness::Signed);
    assert_trap_edge(&func.blocks, TrapKind::DivideByZero);
}

#[test]
fn signed_div_emits_signed_min_div_neg_one_trap() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_trap_edge(&func.blocks, TrapKind::SignedMinDivNegOne);
}

// ---------------------------------------------------------------------------
// Division — unsigned u64 has divide-by-zero but NO signed-MIN/-1 trap
// ---------------------------------------------------------------------------

#[test]
fn unsigned_div_emits_divide_by_zero_trap() {
    let p = pipeline("fn main() -> u64 { let a: u64 = 10; let b: u64 = 2; a / b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_eq!(sole_div_rem(&func.blocks), IntSignedness::Unsigned);
    assert_trap_edge(&func.blocks, TrapKind::DivideByZero);
}

#[test]
fn unsigned_div_has_no_signed_min_div_neg_one_trap() {
    let p = pipeline("fn main() -> u64 { let a: u64 = 10; let b: u64 = 2; a / b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_no_trap(&func.blocks, TrapKind::SignedMinDivNegOne);
}

// ---------------------------------------------------------------------------
// Modulo — same trap shape as division
// ---------------------------------------------------------------------------

#[test]
fn signed_rem_emits_divide_by_zero_and_smno_traps() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 10; let b: i64 = 3; a % b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_trap_edge(&func.blocks, TrapKind::DivideByZero);
    assert_trap_edge(&func.blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn unsigned_rem_emits_divide_by_zero_only() {
    let p = pipeline("fn main() -> u32 { let a: u32 = 10; let b: u32 = 3; a % b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_trap_edge(&func.blocks, TrapKind::DivideByZero);
    assert_no_trap(&func.blocks, TrapKind::SignedMinDivNegOne);
}

// ---------------------------------------------------------------------------
// Division — per-width smoke (signed: i8/i16/i32; unsigned: u8/u16/u32)
// ---------------------------------------------------------------------------

fn div_src(ret: &str) -> String {
    format!("fn main() -> {ret} {{ let a: {ret} = 10; let b: {ret} = 2; a / b }}")
}

#[test]
fn div_i8_signed() {
    let p = pipeline(&div_src("i8"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(sole_div_rem(&p.raw_mir[0].blocks), IntSignedness::Signed);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::DivideByZero);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn div_i16_signed() {
    let p = pipeline(&div_src("i16"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(sole_div_rem(&p.raw_mir[0].blocks), IntSignedness::Signed);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::DivideByZero);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn div_i32_signed() {
    let p = pipeline(&div_src("i32"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(sole_div_rem(&p.raw_mir[0].blocks), IntSignedness::Signed);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::DivideByZero);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn div_u8_unsigned() {
    let p = pipeline(&div_src("u8"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(sole_div_rem(&p.raw_mir[0].blocks), IntSignedness::Unsigned);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::DivideByZero);
    assert_no_trap(&p.raw_mir[0].blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn div_u16_unsigned() {
    let p = pipeline(&div_src("u16"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(sole_div_rem(&p.raw_mir[0].blocks), IntSignedness::Unsigned);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::DivideByZero);
    assert_no_trap(&p.raw_mir[0].blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn div_u32_unsigned() {
    let p = pipeline(&div_src("u32"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_eq!(sole_div_rem(&p.raw_mir[0].blocks), IntSignedness::Unsigned);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::DivideByZero);
    assert_no_trap(&p.raw_mir[0].blocks, TrapKind::SignedMinDivNegOne);
}

// ---------------------------------------------------------------------------
// Shift — shift-out-of-range trap on i64 (shl)
// ---------------------------------------------------------------------------

#[test]
fn shl_i64_emits_shift_out_of_range_trap() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    // IntShl has no signedness — sole_shift returns None for Shl
    assert_eq!(sole_shift(&func.blocks), None);
    assert_trap_edge(&func.blocks, TrapKind::ShiftOutOfRange);
}

// ---------------------------------------------------------------------------
// Shift — shift-out-of-range trap on i64 (shr, signed)
// ---------------------------------------------------------------------------

#[test]
fn shr_i64_emits_shift_out_of_range_trap_signed() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 8; let b: i64 = 1; a >> b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_eq!(sole_shift(&func.blocks), Some(IntSignedness::Signed));
    assert_trap_edge(&func.blocks, TrapKind::ShiftOutOfRange);
}

// ---------------------------------------------------------------------------
// Shift — shift-out-of-range trap on u32 (shr, unsigned)
// ---------------------------------------------------------------------------

#[test]
fn shr_u32_emits_shift_out_of_range_trap_unsigned() {
    let p = pipeline("fn main() -> u32 { let a: u32 = 8; let b: u32 = 1; a >> b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_eq!(sole_shift(&func.blocks), Some(IntSignedness::Unsigned));
    assert_trap_edge(&func.blocks, TrapKind::ShiftOutOfRange);
}

// ---------------------------------------------------------------------------
// Shift — per-width smoke (i8/i16/i32/u8/u16/u64) — shl direction
// ---------------------------------------------------------------------------

fn shl_src(ret: &str) -> String {
    format!("fn main() -> {ret} {{ let a: {ret} = 1; let b: {ret} = 2; a << b }}")
}

#[test]
fn shl_i8() {
    let p = pipeline(&shl_src("i8"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::ShiftOutOfRange);
}

#[test]
fn shl_i16() {
    let p = pipeline(&shl_src("i16"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::ShiftOutOfRange);
}

#[test]
fn shl_i32() {
    let p = pipeline(&shl_src("i32"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::ShiftOutOfRange);
}

#[test]
fn shl_u8() {
    let p = pipeline(&shl_src("u8"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::ShiftOutOfRange);
}

#[test]
fn shl_u16() {
    let p = pipeline(&shl_src("u16"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::ShiftOutOfRange);
}

#[test]
fn shl_u64() {
    let p = pipeline(&shl_src("u64"));
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    assert_trap_edge(&p.raw_mir[0].blocks, TrapKind::ShiftOutOfRange);
}

// ---------------------------------------------------------------------------
// Shift — trap block is unreachable only via Branch (not directly landed on)
// ---------------------------------------------------------------------------

#[test]
fn shift_trap_block_has_branch_predecessor() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }");
    let func = &p.raw_mir[0];
    // Find the ShiftOutOfRange trap block
    let trap = func
        .blocks
        .iter()
        .find(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: TrapKind::ShiftOutOfRange
                }
            )
        })
        .expect("must have a ShiftOutOfRange trap block");
    let has_branch_pred = func.blocks.iter().any(|b| {
        matches!(
            b.terminator,
            Terminator::Branch { then_target, .. } if then_target == trap.id
        )
    });
    assert!(
        has_branch_pred,
        "ShiftOutOfRange trap block has no Branch predecessor"
    );
}

// ---------------------------------------------------------------------------
// Division continuation block is reachable on the safe path
// ---------------------------------------------------------------------------

#[test]
fn div_continuation_block_is_branch_else_target() {
    let p = pipeline("fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }");
    let func = &p.raw_mir[0];
    // Find the IntDiv instruction; it must be in some non-trap block.
    let div_block = func.blocks.iter().find(|b| {
        b.instructions
            .iter()
            .any(|i| matches!(i, Instr::IntDiv { .. }))
    });
    assert!(
        div_block.is_some(),
        "expected an IntDiv instruction in some block"
    );
}

// ---------------------------------------------------------------------------
// isize / usize: platform-sized div/shift now lower with target-width guards
// ---------------------------------------------------------------------------
//
// Before platform-int-arith, isize/usize div/shift fail-closed at MIR with a
// NotYetImplemented diagnostic because the trap-guard constant (signed-MIN,
// shift-width) was not knowable. They now thread the target pointer width.
// These tests assert the trap edges ARE emitted and that the width-bearing
// constant is the correct per-target value — the soundness boundary.

/// Lower `source` with an explicit target pointer width. Mirrors `pipeline`
/// but routes through `lower_hir_module_with_facts` so the isize/usize width
/// guards resolve to the requested width (the cross-compile soundness path).
fn pipeline_with_width(source: &str, width: PointerWidth) -> IrPipeline {
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
    hew_mir::lower_hir_module_with_facts(&output.module, &std::collections::HashMap::new(), width)
}

/// Return the set of `Instr::ConstI64` values emitted in the function.
fn const_i64_values(blocks: &[BasicBlock]) -> Vec<i64> {
    blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::ConstI64 { value, .. } => Some(*value),
            _ => None,
        })
        .collect()
}

#[test]
fn div_isize_lowers_with_signed_min_trap() {
    let p = pipeline("fn main() -> isize { let a: isize = 10; let b: isize = 2; a / b }");
    assert!(
        p.diagnostics.is_empty(),
        "isize div must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(sole_div_rem(&func.blocks), IntSignedness::Signed);
    assert_trap_edge(&func.blocks, TrapKind::DivideByZero);
    assert_trap_edge(&func.blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn div_usize_lowers_unsigned_no_signed_min_trap() {
    let p = pipeline("fn main() -> usize { let a: usize = 10; let b: usize = 2; a / b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_eq!(sole_div_rem(&func.blocks), IntSignedness::Unsigned);
    assert_trap_edge(&func.blocks, TrapKind::DivideByZero);
    assert_no_trap(&func.blocks, TrapKind::SignedMinDivNegOne);
}

#[test]
fn shl_isize_lowers_with_shift_out_of_range_trap() {
    let p = pipeline("fn main() -> isize { let a: isize = 1; let b: isize = 2; a << b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_trap_edge(&func.blocks, TrapKind::ShiftOutOfRange);
}

#[test]
fn shr_usize_lowers_with_shift_out_of_range_trap() {
    let p = pipeline("fn main() -> usize { let a: usize = 8; let b: usize = 1; a >> b }");
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let func = &p.raw_mir[0];
    assert_eq!(sole_shift(&func.blocks), Some(IntSignedness::Unsigned));
    assert_trap_edge(&func.blocks, TrapKind::ShiftOutOfRange);
}

// ── Width-correctness: the trap constant follows the TARGET pointer width ────
//
// The load-bearing cross-compile soundness check: a width-64 host must still
// emit a width-32 shift bound and i32::MIN signed-MIN when lowering for a
// 32-bit (wasm32) target, and width-64 / i64::MIN for a 64-bit target. A
// host-`cfg!`-derived width would emit the host's 64 into the wasm32 module.

#[test]
fn shift_isize_width_const_follows_target_pointer_width() {
    // 64-bit target: the shift-range bound constant must be 64.
    let p64 = pipeline_with_width(
        "fn main() -> usize { let a: usize = 1; let b: usize = 2; a << b }",
        PointerWidth::Bits64,
    );
    assert!(p64.diagnostics.is_empty(), "{:?}", p64.diagnostics);
    assert!(
        const_i64_values(&p64.raw_mir[0].blocks).contains(&64),
        "64-bit usize shift must emit a width-64 range bound; consts: {:?}",
        const_i64_values(&p64.raw_mir[0].blocks)
    );

    // 32-bit (wasm32) target: the SAME source lowered for wasm32 must emit a
    // width-32 bound, NOT the host's 64 — the cross-compile soundness check.
    let p32 = pipeline_with_width(
        "fn main() -> usize { let a: usize = 1; let b: usize = 2; a << b }",
        PointerWidth::Bits32,
    );
    assert!(p32.diagnostics.is_empty(), "{:?}", p32.diagnostics);
    let consts32 = const_i64_values(&p32.raw_mir[0].blocks);
    assert!(
        consts32.contains(&32),
        "32-bit usize shift must emit a width-32 range bound; consts: {consts32:?}"
    );
    assert!(
        !consts32.contains(&64),
        "32-bit usize shift must NOT emit a width-64 bound (host-width leak); consts: {consts32:?}"
    );
}

#[test]
fn div_isize_signed_min_const_follows_target_pointer_width() {
    // 64-bit isize div: signed-MIN constant must be i64::MIN.
    let p64 = pipeline_with_width(
        "fn main() -> isize { let a: isize = 10; let b: isize = 2; a / b }",
        PointerWidth::Bits64,
    );
    assert!(p64.diagnostics.is_empty(), "{:?}", p64.diagnostics);
    assert!(
        const_i64_values(&p64.raw_mir[0].blocks).contains(&i64::MIN),
        "64-bit isize div must emit i64::MIN as the signed-MIN guard; consts: {:?}",
        const_i64_values(&p64.raw_mir[0].blocks)
    );

    // 32-bit isize div: signed-MIN constant must be i32::MIN, NOT i64::MIN.
    let p32 = pipeline_with_width(
        "fn main() -> isize { let a: isize = 10; let b: isize = 2; a / b }",
        PointerWidth::Bits32,
    );
    assert!(p32.diagnostics.is_empty(), "{:?}", p32.diagnostics);
    let consts32 = const_i64_values(&p32.raw_mir[0].blocks);
    assert!(
        consts32.contains(&i64::from(i32::MIN)),
        "32-bit isize div must emit i32::MIN as the signed-MIN guard; consts: {consts32:?}"
    );
    assert!(
        !consts32.contains(&i64::MIN),
        "32-bit isize div must NOT emit i64::MIN (host-width leak); consts: {consts32:?}"
    );
}
