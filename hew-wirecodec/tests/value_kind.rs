//! Integration tests for [`WireValue::kind`] and [`WireValue::conforms_to_plan`].

use hew_wirecodec::{PrimitiveWireKind, VariantPlan, WireCodecPlan, WireShape, WireValue};

// ---------------------------------------------------------------------------
// kind() tests
// ---------------------------------------------------------------------------

#[test]
fn bool_value_returns_bool_kind() {
    assert_eq!(WireValue::Bool(true).kind(), PrimitiveWireKind::Bool);
}

#[test]
fn i32_value_returns_i32_kind() {
    assert_eq!(WireValue::I32(-42).kind(), PrimitiveWireKind::I32);
}

#[test]
fn char_value_returns_char_kind() {
    assert_eq!(WireValue::Char('é').kind(), PrimitiveWireKind::Char);
}

#[test]
fn struct_value_returns_nested_kind_with_type_name() {
    let v = WireValue::Struct {
        type_name: "Point".to_string(),
        fields: vec![],
    };
    assert_eq!(v.kind(), PrimitiveWireKind::Nested("Point".to_string()));
}

#[test]
fn enum_value_returns_nested_kind_with_type_name() {
    let v = WireValue::Enum {
        type_name: "Colour".to_string(),
        variant: "Red".to_string(),
    };
    assert_eq!(v.kind(), PrimitiveWireKind::Nested("Colour".to_string()));
}

// ---------------------------------------------------------------------------
// conforms_to_plan() tests
// ---------------------------------------------------------------------------

fn struct_plan(name: &str) -> WireCodecPlan {
    WireCodecPlan {
        name: name.to_string(),
        shape: WireShape::Struct { fields: vec![] },
        json_case: None,
        yaml_case: None,
    }
}

fn enum_plan(name: &str, variants: &[&str]) -> WireCodecPlan {
    WireCodecPlan {
        name: name.to_string(),
        shape: WireShape::Enum {
            variants: variants
                .iter()
                .map(|v| VariantPlan {
                    name: v.to_string(),
                })
                .collect(),
        },
        json_case: None,
        yaml_case: None,
    }
}

#[test]
fn struct_value_conforms_to_matching_plan() {
    let v = WireValue::Struct {
        type_name: "Point".to_string(),
        fields: vec![],
    };
    assert!(v.conforms_to_plan(&struct_plan("Point")));
}

#[test]
fn struct_value_does_not_conform_to_enum_plan() {
    let v = WireValue::Struct {
        type_name: "Point".to_string(),
        fields: vec![],
    };
    // Same name but wrong shape.
    assert!(!v.conforms_to_plan(&enum_plan("Point", &["A"])));
}

#[test]
fn enum_value_conforms_to_matching_plan() {
    let v = WireValue::Enum {
        type_name: "Colour".to_string(),
        variant: "Red".to_string(),
    };
    assert!(v.conforms_to_plan(&enum_plan("Colour", &["Red", "Green", "Blue"])));
}

#[test]
fn enum_value_does_not_conform_when_variant_missing() {
    let v = WireValue::Enum {
        type_name: "Colour".to_string(),
        variant: "Yellow".to_string(),
    };
    // Plan does not list Yellow as a valid variant.
    assert!(!v.conforms_to_plan(&enum_plan("Colour", &["Red", "Green", "Blue"])));
}

#[test]
fn enum_value_does_not_conform_to_name_mismatch_plan() {
    let v = WireValue::Enum {
        type_name: "Colour".to_string(),
        variant: "Red".to_string(),
    };
    // Plan name differs — same variant, different type name.
    assert!(!v.conforms_to_plan(&enum_plan("Shade", &["Red", "Green", "Blue"])));
}

#[test]
fn primitive_value_does_not_conform_to_enum_plan() {
    // Primitives always return false from conforms_to_plan; they are not plan
    // subjects. The enum arm is the focal path but the guard is unconditional.
    assert!(!WireValue::Bool(true).conforms_to_plan(&enum_plan("Colour", &["Red"])));
}

// ---------------------------------------------------------------------------
// Float non-Eq demonstration
// ---------------------------------------------------------------------------

/// `WireValue` does not implement `PartialEq` or `Eq`. This test demonstrates
/// the correct bit-pattern comparison pattern for F32 values.
#[test]
fn f32_not_eq_implementor() {
    let a = WireValue::F32(1.5_f32);
    let b = WireValue::F32(1.5_f32);
    // assert_eq!(a, b) would not compile — PartialEq is not derived.
    // Callers must destructure and compare via to_bits().
    match (&a, &b) {
        (WireValue::F32(x), WireValue::F32(y)) => {
            assert_eq!(x.to_bits(), y.to_bits());
        }
        _ => unreachable!(),
    }
}
