use crate::common;

use common::parse_and_typecheck_inline;
use hew_types::{NumericMethodFamily, NumericMethodOp, NumericSignedness, NumericWidth, Ty};

// `wrapping_add(i32)` moved off this side table to
// `RuntimeCallFamily::IntArith` (see the "Numeric opt-out arithmetic
// methods" arm in hew-types/src/check/methods.rs); its acceptance coverage
// is tests/core-acceptance/cases/int-checked-arithmetic.hew, which exercises
// it end to end through codegen rather than the checker-only fact this file
// tests. Wrapping and saturating add/sub at other widths (i8/i16/u8/u16/
// isize/usize) and saturating_mul still fall back to this side table —
// `NumericMethodFamily::{Wrapping,Saturating}` remain live for that reason.

#[test]
fn numeric_method_side_table_records_checked_unsigned_width() {
    // `checked_add` returns the compiler's canonical `Option`; a local enum
    // with the same leaf name is intentionally a distinct nominal type.
    let (_program, output) = parse_and_typecheck_inline(
        "fn add() -> Option<u8> { let a: u8 = 1; let b: u8 = 2; a.checked_add(b) }",
    );

    assert!(
        output.errors.is_empty(),
        "checked numeric method must typecheck: {:?}",
        output.errors
    );
    let lowering = output
        .numeric_method_lowerings
        .values()
        .next()
        .expect("side-table entry");
    assert_eq!(lowering.family, NumericMethodFamily::Checked);
    assert_eq!(lowering.op, NumericMethodOp::Add);
    assert_eq!(lowering.operand_ty, Ty::U8);
    assert_eq!(lowering.signedness, NumericSignedness::Unsigned);
    assert_eq!(lowering.width, NumericWidth::Bits(8));
}
