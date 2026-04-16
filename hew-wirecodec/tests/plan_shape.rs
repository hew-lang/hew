//! Shape tests for `WireCodecPlan::build` — verify the plan faithfully carries
//! field ordering, numbers, kinds, modifiers, and naming overrides.

use hew_parser::ast::{
    NamingCase, VariantDecl, VariantKind, Visibility, WireDecl, WireDeclKind, WireFieldDecl,
};
use hew_wirecodec::{
    FieldModifiers, IntegerBounds, PrimitiveWireKind, WireCodecError, WireCodecPlan,
};

fn field(name: &str, ty: &str, number: u32) -> WireFieldDecl {
    WireFieldDecl {
        name: name.to_string(),
        ty: ty.to_string(),
        field_number: number,
        is_optional: false,
        is_repeated: false,
        is_reserved: false,
        is_deprecated: false,
        json_name: None,
        yaml_name: None,
        since: None,
    }
}

fn point_decl() -> WireDecl {
    WireDecl {
        visibility: Visibility::Pub,
        kind: WireDeclKind::Struct,
        name: "Point".into(),
        fields: vec![field("x", "int", 1), field("y", "int", 2)],
        variants: vec![],
        json_case: None,
        yaml_case: None,
    }
}

#[test]
fn build_plan_for_basic_struct_carries_fields_in_declared_order() {
    let plan = WireCodecPlan::build(&point_decl()).expect("plan");
    assert_eq!(plan.name, "Point");
    let fields = plan.fields().expect("struct shape");
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "x");
    assert_eq!(fields[0].number, 1);
    assert_eq!(fields[0].kind, PrimitiveWireKind::I64);
    assert_eq!(fields[1].name, "y");
    assert_eq!(fields[1].number, 2);
    assert_eq!(fields[1].kind, PrimitiveWireKind::I64);
}

#[test]
fn build_plan_assigns_narrowing_bounds_on_integer_fields() {
    let plan = WireCodecPlan::build(&point_decl()).expect("plan");
    let fields = plan.fields().expect("struct shape");
    assert_eq!(
        fields[0].narrowing,
        IntegerBounds::for_kind(&PrimitiveWireKind::I64)
    );
}

#[test]
fn build_plan_propagates_field_modifiers_verbatim() {
    let mut decl = point_decl();
    decl.fields[0].is_optional = true;
    decl.fields[0].is_deprecated = true;
    decl.fields[1].is_repeated = true;
    let plan = WireCodecPlan::build(&decl).expect("plan");
    let fields = plan.fields().expect("struct shape");
    assert_eq!(
        fields[0].modifiers,
        FieldModifiers {
            is_optional: true,
            is_repeated: false,
            is_deprecated: true,
            is_reserved: false,
            since: None,
        }
    );
    assert!(fields[1].modifiers.is_repeated);
}

#[test]
fn build_plan_rejects_empty_field_type() {
    let mut decl = point_decl();
    decl.fields[0].ty = String::new();
    let err = WireCodecPlan::build(&decl).unwrap_err();
    assert!(
        matches!(err, WireCodecError::UnresolvedFieldType { ref field, .. } if field == "x"),
        "expected UnresolvedFieldType(x), got {err:?}"
    );
}

#[test]
fn build_plan_rejects_duplicate_field_numbers() {
    let mut decl = point_decl();
    decl.fields[1].field_number = 1;
    let err = WireCodecPlan::build(&decl).unwrap_err();
    assert!(
        matches!(err, WireCodecError::DuplicateFieldNumber { number: 1 }),
        "expected DuplicateFieldNumber(1), got {err:?}"
    );
}

#[test]
fn build_plan_applies_struct_level_json_case_to_field_keys() {
    let mut decl = WireDecl {
        visibility: Visibility::Pub,
        kind: WireDeclKind::Struct,
        name: "User".into(),
        fields: vec![field("first_name", "string", 1)],
        variants: vec![],
        json_case: Some(NamingCase::CamelCase),
        yaml_case: None,
    };
    decl.fields[0].ty = "string".to_string();
    let plan = WireCodecPlan::build(&decl).expect("plan");
    let fields = plan.fields().unwrap();
    assert_eq!(fields[0].json_name, "firstName");
    // YAML case was unset — field name passes through.
    assert_eq!(fields[0].yaml_name, "first_name");
}

#[test]
fn build_plan_per_field_json_override_beats_struct_level_case() {
    let mut decl = WireDecl {
        visibility: Visibility::Pub,
        kind: WireDeclKind::Struct,
        name: "User".into(),
        fields: vec![field("first_name", "string", 1)],
        variants: vec![],
        json_case: Some(NamingCase::ScreamingSnake),
        yaml_case: None,
    };
    decl.fields[0].json_name = Some("custom".into());
    let plan = WireCodecPlan::build(&decl).expect("plan");
    let fields = plan.fields().unwrap();
    assert_eq!(fields[0].json_name, "custom");
}

#[test]
fn build_plan_skips_reserved_fields_but_counts_their_numbers() {
    let mut decl = point_decl();
    let mut reserved = field("", "", 3);
    reserved.is_reserved = true;
    decl.fields.push(reserved);
    let plan = WireCodecPlan::build(&decl).expect("plan");
    let fields = plan.fields().unwrap();
    assert_eq!(fields.len(), 2);
    assert!(fields.iter().all(|f| f.number != 3));
}

#[test]
fn build_plan_for_enum_emits_variant_list() {
    let decl = WireDecl {
        visibility: Visibility::Pub,
        kind: WireDeclKind::Enum,
        name: "Color".into(),
        fields: vec![],
        variants: vec![
            VariantDecl {
                name: "Red".into(),
                kind: VariantKind::Unit,
            },
            VariantDecl {
                name: "Green".into(),
                kind: VariantKind::Unit,
            },
        ],
        json_case: None,
        yaml_case: None,
    };
    let plan = WireCodecPlan::build(&decl).expect("plan");
    let variants = plan.variants().expect("enum shape");
    assert_eq!(variants.len(), 2);
    assert_eq!(variants[0].name, "Red");
    assert_eq!(variants[1].name, "Green");
    assert!(plan.fields().is_none());
}
