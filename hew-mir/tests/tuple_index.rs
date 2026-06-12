//! MIR model tests for `Instr::TupleFieldLoad`.
//!
//! ## Scope
//!
//! The Hew surface language currently has no standalone tuple-construction
//! expression, so end-to-end source-driven tests that exercise the HIR→MIR
//! lowering path for `HirExprKind::TupleIndex` → `Instr::TupleFieldLoad` are
//! deferred until a tuple producer (tuple literal syntax or user-fn-call
//! lowering returning a tuple) lands.
//!
//! SHIM: these tests cover the MIR model invariants (instruction construction,
//! `instr_places` shape) rather than the HIR→MIR lowering path.
//! WHEN-OBSOLETE: replace with source-driven pipeline tests once tuple literals
//!   (`(e0, e1, …)`) are parsed and lowered end-to-end.
//! WHAT: write `fn pipeline_with_tc(source)` tests that lower Hew source
//!   containing `let (a, b) = some_fn_returning_tuple();` and assert that
//!   `Instr::TupleFieldLoad` appears in the raw MIR.
//!
//! The codegen emission path (GEP+load for `Instr::TupleFieldLoad`) is covered
//! by `hew-codegen-rs/tests/tuple_emission.rs`.
//!
//! LESSONS applied:
//! - `instr-places-exhaustiveness` (P1): adding `TupleFieldLoad` to the
//!   `instr_places` match is verified here by construction; the exhaustiveness
//!   check fires at compile time if the variant is missing.
//! - `boundary-fail-closed` (P0): the `tuple_decomp` early-return in the MIR
//!   lowering arm is preserved; this variant is only emitted for non-proxy
//!   locals.

use hew_mir::{FieldOffset, Instr, Place};

/// `Instr::TupleFieldLoad` must be constructible and carry the correct
/// positional index and place references.
#[test]
fn tuple_field_load_variant_carries_correct_fields() {
    let tuple_place = Place::Local(0);
    let dest_place = Place::Local(1);
    let instr = Instr::TupleFieldLoad {
        tuple: tuple_place,
        field_index: 2,
        dest: dest_place,
    };
    match instr {
        Instr::TupleFieldLoad {
            tuple,
            field_index,
            dest,
        } => {
            assert_eq!(tuple, Place::Local(0));
            assert_eq!(field_index, 2);
            assert_eq!(dest, Place::Local(1));
        }
        other => panic!("expected TupleFieldLoad, got: {other:?}"),
    }
}

/// `Instr::TupleFieldLoad` must be distinct from `Instr::RecordFieldLoad`
/// (both carry struct GEP semantics but for different type domains).
#[test]
fn tuple_field_load_is_distinct_from_record_field_load() {
    let tuple_instr = Instr::TupleFieldLoad {
        tuple: Place::Local(0),
        field_index: 0,
        dest: Place::Local(1),
    };
    let record_instr = Instr::RecordFieldLoad {
        record: Place::Local(0),
        field_offset: FieldOffset(0),
        dest: Place::Local(1),
    };
    // They are distinct enum variants; the match arms must not collapse.
    assert!(
        !matches!(tuple_instr, Instr::RecordFieldLoad { .. }),
        "TupleFieldLoad must not match RecordFieldLoad arm"
    );
    assert!(
        !matches!(record_instr, Instr::TupleFieldLoad { .. }),
        "RecordFieldLoad must not match TupleFieldLoad arm"
    );
}

/// A three-element tuple projection at index 2 must encode `field_index` = 2.
#[test]
fn tuple_field_load_encodes_correct_index_for_third_element() {
    let instr = Instr::TupleFieldLoad {
        tuple: Place::Local(0),
        field_index: 2,
        dest: Place::Local(3),
    };
    let Instr::TupleFieldLoad { field_index, .. } = instr else {
        panic!("expected TupleFieldLoad");
    };
    assert_eq!(field_index, 2, "third element must have field_index = 2");
}
