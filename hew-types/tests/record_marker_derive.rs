//! Tests for automatic marker derivation on `record` types (A-4).
//!
//! LESSON exhaustive-coverage: every marker derivation path has an explicit
//! test so a missing arm in `implements_marker` causes a failure.

use hew_types::traits::{MarkerTrait, TraitRegistry};
use hew_types::ty::Ty;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn named(name: &str) -> Ty {
    Ty::Named {
        builtin: None,
        name: name.to_string(),
        args: vec![],
    }
}

fn named_with(name: &str, args: Vec<Ty>) -> Ty {
    Ty::Named {
        builtin: None,
        name: name.to_string(),
        args,
    }
}

/// Register a named-field record and return a `Ty` for it.
fn register_named_record(reg: &mut TraitRegistry, name: &str, fields: Vec<Ty>) -> Ty {
    reg.register_type(name.to_string(), fields);
    reg.register_record_type(name.to_string());
    named(name)
}

/// Register a tuple-record and return a `Ty` for it.
fn register_tuple_record(reg: &mut TraitRegistry, name: &str, positional: Vec<Ty>) -> Ty {
    reg.register_type(name.to_string(), positional);
    reg.register_record_type(name.to_string());
    named(name)
}

// ---------------------------------------------------------------------------
// record Point { x: i64, y: i64 }
// All fields are i64 (i64). Derives Eq/Hash/Send/Frozen/Clone/Copy/Sync/Ord.
// ---------------------------------------------------------------------------

#[test]
fn record_point_derives_eq_and_hash() {
    let mut reg = TraitRegistry::new();
    let point = register_named_record(&mut reg, "Point", vec![Ty::I64, Ty::I64]);

    assert!(
        reg.implements_marker(&point, MarkerTrait::Eq),
        "Point {{ x: i64, y: i64 }} must derive Eq"
    );
    assert!(
        reg.implements_marker(&point, MarkerTrait::Hash),
        "Point {{ x: i64, y: i64 }} must derive Hash"
    );
}

#[test]
fn record_point_derives_send_frozen_clone_copy() {
    let mut reg = TraitRegistry::new();
    let point = register_named_record(&mut reg, "Point", vec![Ty::I64, Ty::I64]);

    assert!(
        reg.implements_marker(&point, MarkerTrait::Send),
        "Point must derive Send"
    );
    assert!(
        reg.implements_marker(&point, MarkerTrait::Frozen),
        "Point must derive Frozen"
    );
    assert!(
        reg.implements_marker(&point, MarkerTrait::Clone),
        "Point must derive Clone"
    );
    assert!(
        reg.implements_marker(&point, MarkerTrait::Copy),
        "Point must derive Copy when all fields are Copy"
    );
}

#[test]
fn record_point_not_resource() {
    let mut reg = TraitRegistry::new();
    let point = register_named_record(&mut reg, "Point", vec![Ty::I64, Ty::I64]);

    assert!(
        !reg.implements_marker(&point, MarkerTrait::Resource),
        "records are value types — never Resource"
    );
}

// ---------------------------------------------------------------------------
// record Measurement { v: float }
// f64 fields are NOT Eq/Hash/Ord. Derives Send/Frozen/Clone/Copy but not Eq/Hash.
// ---------------------------------------------------------------------------

#[test]
fn record_measurement_float_field_not_eq_not_hash() {
    let mut reg = TraitRegistry::new();
    let m = register_named_record(&mut reg, "Measurement", vec![Ty::F64]);

    assert!(
        !reg.implements_marker(&m, MarkerTrait::Eq),
        "Measurement {{ v: float }} must NOT derive Eq (float field)"
    );
    assert!(
        !reg.implements_marker(&m, MarkerTrait::Hash),
        "Measurement {{ v: float }} must NOT derive Hash (float field)"
    );
    assert!(
        !reg.implements_marker(&m, MarkerTrait::Ord),
        "Measurement {{ v: float }} must NOT derive Ord (float field)"
    );
}

#[test]
fn record_measurement_float_field_derives_send_clone_copy() {
    let mut reg = TraitRegistry::new();
    let m = register_named_record(&mut reg, "Measurement", vec![Ty::F64]);

    // float is Send/Frozen/Clone/Copy — record inherits these
    assert!(reg.implements_marker(&m, MarkerTrait::Send));
    assert!(reg.implements_marker(&m, MarkerTrait::Frozen));
    assert!(reg.implements_marker(&m, MarkerTrait::Clone));
    assert!(reg.implements_marker(&m, MarkerTrait::Copy));
}

// ---------------------------------------------------------------------------
// record Cb { f: () -> i64 }
// Function fields are Send/Copy/Clone but NOT Eq/Hash.
// ---------------------------------------------------------------------------

#[test]
fn record_cb_function_field_not_eq_not_hash() {
    let mut reg = TraitRegistry::new();
    let fn_ty = Ty::Function {
        params: vec![],
        ret: Box::new(Ty::I64),
    };
    let cb = register_named_record(&mut reg, "Cb", vec![fn_ty]);

    assert!(
        !reg.implements_marker(&cb, MarkerTrait::Eq),
        "Cb {{ f: () -> i64 }} must NOT derive Eq (function field)"
    );
    assert!(
        !reg.implements_marker(&cb, MarkerTrait::Hash),
        "Cb {{ f: () -> i64 }} must NOT derive Hash (function field)"
    );
}

#[test]
fn record_cb_function_field_derives_send_copy() {
    let mut reg = TraitRegistry::new();
    let fn_ty = Ty::Function {
        params: vec![],
        ret: Box::new(Ty::I64),
    };
    let cb = register_named_record(&mut reg, "Cb", vec![fn_ty]);

    // Function is Send/Copy/Clone
    assert!(reg.implements_marker(&cb, MarkerTrait::Send));
    assert!(reg.implements_marker(&cb, MarkerTrait::Copy));
    assert!(reg.implements_marker(&cb, MarkerTrait::Clone));
}

// ---------------------------------------------------------------------------
// record Owner { handle: Duplex<i64, i64> }
// Duplex is a resource: NOT Copy, NOT Clone. Owner must not be Copy or Clone.
// Resource marker must NOT propagate to the enclosing record.
// ---------------------------------------------------------------------------

#[test]
fn record_owner_duplex_field_not_copy_not_clone() {
    let mut reg = TraitRegistry::new();
    let duplex = named_with("Duplex", vec![Ty::I64, Ty::I64]);
    let owner = register_named_record(&mut reg, "Owner", vec![duplex]);

    assert!(
        !reg.implements_marker(&owner, MarkerTrait::Copy),
        "Owner containing Duplex must NOT derive Copy"
    );
    assert!(
        !reg.implements_marker(&owner, MarkerTrait::Clone),
        "Owner containing Duplex must NOT derive Clone"
    );
}

#[test]
fn record_owner_duplex_field_not_resource() {
    let mut reg = TraitRegistry::new();
    let duplex = named_with("Duplex", vec![Ty::I64, Ty::I64]);
    let owner = register_named_record(&mut reg, "Owner", vec![duplex]);

    assert!(
        !reg.implements_marker(&owner, MarkerTrait::Resource),
        "record Owner wrapping Duplex must NOT itself be Resource"
    );
}

// ---------------------------------------------------------------------------
// Tuple-record: same rules applied positionally.
// record PointT(i64, i64) — positional fields registered for derivation.
// ---------------------------------------------------------------------------

#[test]
fn tuple_record_int_fields_derives_eq_hash_copy() {
    let mut reg = TraitRegistry::new();
    let pt = register_tuple_record(&mut reg, "PointT", vec![Ty::I64, Ty::I64]);

    assert!(
        reg.implements_marker(&pt, MarkerTrait::Eq),
        "tuple-record PointT(i64, i64) must derive Eq"
    );
    assert!(
        reg.implements_marker(&pt, MarkerTrait::Hash),
        "tuple-record PointT(i64, i64) must derive Hash"
    );
    assert!(
        reg.implements_marker(&pt, MarkerTrait::Copy),
        "tuple-record PointT(i64, i64) must derive Copy"
    );
}

#[test]
fn tuple_record_float_field_not_eq_not_hash() {
    let mut reg = TraitRegistry::new();
    // record MeasureT(float)
    let m = register_tuple_record(&mut reg, "MeasureT", vec![Ty::F64]);

    assert!(
        !reg.implements_marker(&m, MarkerTrait::Eq),
        "tuple-record MeasureT(float) must NOT derive Eq"
    );
    assert!(
        !reg.implements_marker(&m, MarkerTrait::Hash),
        "tuple-record MeasureT(float) must NOT derive Hash"
    );
}

#[test]
fn tuple_record_duplex_field_not_copy_not_resource() {
    let mut reg = TraitRegistry::new();
    let duplex = named_with("Duplex", vec![Ty::I64, Ty::I64]);
    // record OwnerT(Duplex<i64, i64>)
    let owner_t = register_tuple_record(&mut reg, "OwnerT", vec![duplex]);

    assert!(
        !reg.implements_marker(&owner_t, MarkerTrait::Copy),
        "tuple-record OwnerT(Duplex<i64,i64>) must NOT derive Copy"
    );
    assert!(
        !reg.implements_marker(&owner_t, MarkerTrait::Resource),
        "tuple-record OwnerT wrapping Duplex must NOT be Resource"
    );
}

// ---------------------------------------------------------------------------
// Closure field: NOT Eq, NOT Hash, NOT Copy.
// record Listener { on_event: () -> () }
// ---------------------------------------------------------------------------

#[test]
fn record_closure_field_not_eq_not_hash_not_copy() {
    let mut reg = TraitRegistry::new();
    let closure = Ty::Closure {
        params: vec![],
        ret: Box::new(Ty::Unit),
        captures: vec![Ty::I64],
    };
    let listener = register_named_record(&mut reg, "Listener", vec![closure]);

    assert!(
        !reg.implements_marker(&listener, MarkerTrait::Eq),
        "Listener with closure field must NOT derive Eq"
    );
    assert!(
        !reg.implements_marker(&listener, MarkerTrait::Hash),
        "Listener with closure field must NOT derive Hash"
    );
    assert!(
        !reg.implements_marker(&listener, MarkerTrait::Copy),
        "Listener with closure field must NOT derive Copy"
    );
    // Closures ARE Clone
    assert!(
        reg.implements_marker(&listener, MarkerTrait::Clone),
        "Listener with closure field must derive Clone"
    );
}

// ---------------------------------------------------------------------------
// Negative impl overrides: register_negative_impl still applies for records.
// ---------------------------------------------------------------------------

#[test]
fn record_negative_impl_overrides_field_derivation() {
    let mut reg = TraitRegistry::new();
    let pt = register_named_record(&mut reg, "OpaquePoint", vec![Ty::I64, Ty::I64]);
    reg.register_negative_impl("OpaquePoint".to_string(), MarkerTrait::Eq);

    assert!(
        !reg.implements_marker(&pt, MarkerTrait::Eq),
        "negative impl must override field-derived Eq"
    );
    // Hash should still derive (no negative impl on Hash)
    assert!(
        reg.implements_marker(&pt, MarkerTrait::Hash),
        "Hash still derives from fields when only Eq is negated"
    );
}
