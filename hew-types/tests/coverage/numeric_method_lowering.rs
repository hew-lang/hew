use crate::common;

use common::parse_and_typecheck_inline;
use hew_types::{NumericMethodFamily, NumericMethodOp, NumericSignedness, NumericWidth, Ty};

#[test]
fn numeric_method_side_table_records_wrapping_add_i32() {
    let (_program, output) = parse_and_typecheck_inline(
        "fn main() -> i32 { let a: i32 = 1; let b: i32 = 2; a.wrapping_add(b) }",
    );

    assert!(
        output.errors.is_empty(),
        "numeric method must typecheck: {:?}",
        output.errors
    );
    assert_eq!(
        output.numeric_method_lowerings.len(),
        1,
        "accepted numeric method call must record exactly one checker-owned lowering entry"
    );
    let lowering = output
        .numeric_method_lowerings
        .values()
        .next()
        .expect("side-table entry");
    assert_eq!(lowering.family, NumericMethodFamily::Wrapping);
    assert_eq!(lowering.op, NumericMethodOp::Add);
    assert_eq!(lowering.result_ty, Ty::I32);
    assert_eq!(lowering.operand_ty, Ty::I32);
    assert_eq!(lowering.signedness, NumericSignedness::Signed);
    assert_eq!(lowering.width, NumericWidth::Bits(32));
}

#[test]
fn numeric_method_side_table_records_checked_unsigned_width() {
    let (_program, output) = parse_and_typecheck_inline(
        "enum Option<T> { Some(T); None; } fn main() -> Option<u8> { let a: u8 = 1; let b: u8 = 2; a.checked_add(b) }",
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
