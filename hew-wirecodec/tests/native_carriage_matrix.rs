//! Native carriage matrix — §6.1 from `lane-v05-type-wire-native-values.md`.
//!
//! Verifies that `TypeDescriptorWireExt::wire_kind()` produces the expected
//! `PrimitiveWireKind` for every `TypeDescriptor` shape, and that wire-rejected
//! types produce the appropriate `WireBoundaryError`.
//!
//! `ResolvedTy::Unit` is native wire and maps to an explicit unit/nil wire kind,
//! so callers can use `wire_kind()` as the single primitive dispatch authority.

use hew_types::resolved_ty::{ResolvedTraitBound, ResolvedTy};
use hew_wirecodec::{PrimitiveWireKind, TypeDescriptorWireExt, WireBoundaryError};

#[test]
fn wire_kind_primitive_scalars() {
    let cases: &[(ResolvedTy, PrimitiveWireKind)] = &[
        (ResolvedTy::Bool, PrimitiveWireKind::Bool),
        (ResolvedTy::I8, PrimitiveWireKind::I8),
        (ResolvedTy::I16, PrimitiveWireKind::I16),
        (ResolvedTy::I32, PrimitiveWireKind::I32),
        (ResolvedTy::I64, PrimitiveWireKind::I64),
        (ResolvedTy::U8, PrimitiveWireKind::U8),
        (ResolvedTy::U16, PrimitiveWireKind::U16),
        (ResolvedTy::U32, PrimitiveWireKind::U32),
        (ResolvedTy::U64, PrimitiveWireKind::U64),
        (ResolvedTy::F32, PrimitiveWireKind::F32),
        (ResolvedTy::F64, PrimitiveWireKind::F64),
        (ResolvedTy::Char, PrimitiveWireKind::Char),
        (ResolvedTy::String, PrimitiveWireKind::String),
        (ResolvedTy::Bytes, PrimitiveWireKind::Bytes),
        (ResolvedTy::Duration, PrimitiveWireKind::Duration),
    ];
    for (descriptor, expected_kind) in cases {
        let actual = descriptor
            .wire_kind()
            .unwrap_or_else(|e| panic!("{descriptor:?} should have wire kind, got {e:?}"));
        assert_eq!(
            &actual, expected_kind,
            "wire_kind mismatch for {descriptor:?}"
        );
    }
}

#[test]
fn wire_kind_bare_named_produces_nested() {
    let ty = ResolvedTy::Named {
        name: "Point".into(),
        args: vec![],
    };
    assert_eq!(
        ty.wire_kind(),
        Ok(PrimitiveWireKind::Nested("Point".into()))
    );
}

#[test]
fn wire_kind_generic_named_uses_canonical_string() {
    let ty = ResolvedTy::Named {
        name: "Pair".into(),
        args: vec![ResolvedTy::I64, ResolvedTy::String],
    };
    assert_eq!(
        ty.wire_kind(),
        Ok(PrimitiveWireKind::Nested("Pair<i64,string>".into()))
    );
}

#[test]
fn wire_kind_nested_generic_uses_canonical_string() {
    let inner = ResolvedTy::Named {
        name: "Option".into(),
        args: vec![ResolvedTy::String],
    };
    let outer = ResolvedTy::Named {
        name: "Vec".into(),
        args: vec![inner],
    };
    assert_eq!(
        outer.wire_kind(),
        Ok(PrimitiveWireKind::Nested("Vec<Option<string>>".into()))
    );
}

#[test]
fn wire_kind_trait_object_produces_nested() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".into(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    assert_eq!(
        ty.wire_kind(),
        Ok(PrimitiveWireKind::Nested("dyn(Display)".into()))
    );
}

#[test]
fn wire_kind_unit_maps_to_nil_primitive() {
    assert_eq!(ResolvedTy::Unit.wire_kind(), Ok(PrimitiveWireKind::Unit));
}

#[test]
fn wire_kind_never_is_rejected() {
    assert_eq!(
        ResolvedTy::Never.wire_kind(),
        Err(WireBoundaryError::NonWireType {
            canonical: "never".into()
        })
    );
}

#[test]
fn wire_kind_function_is_rejected() {
    let ty = ResolvedTy::Function {
        params: vec![ResolvedTy::I32],
        ret: Box::new(ResolvedTy::Unit),
    };
    let err = ty
        .wire_kind()
        .expect_err("Function should be wire-rejected");
    assert!(
        matches!(err, WireBoundaryError::NonWireType { .. }),
        "expected NonWireType, got {err:?}"
    );
}

#[test]
fn wire_kind_closure_is_rejected() {
    let ty = ResolvedTy::Closure {
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
        captures: vec![],
    };
    let err = ty.wire_kind().expect_err("Closure should be wire-rejected");
    assert!(matches!(err, WireBoundaryError::NonWireType { .. }));
}

#[test]
fn wire_kind_pointer_is_rejected() {
    let ty = ResolvedTy::Pointer {
        is_mutable: false,
        pointee: Box::new(ResolvedTy::I32),
    };
    let err = ty.wire_kind().expect_err("Pointer should be wire-rejected");
    assert!(matches!(err, WireBoundaryError::NonWireType { .. }));
}

#[test]
fn wire_kind_tuple_is_rejected() {
    let ty = ResolvedTy::Tuple(vec![ResolvedTy::I32]);
    let err = ty
        .wire_kind()
        .expect_err("Tuple should be rejected (no single wire kind)");
    assert!(matches!(err, WireBoundaryError::NonWireType { .. }));
}

#[test]
fn wire_kind_array_is_rejected() {
    let ty = ResolvedTy::Array(Box::new(ResolvedTy::I32), 4);
    let err = ty
        .wire_kind()
        .expect_err("Array should be rejected (no single wire kind)");
    assert!(matches!(err, WireBoundaryError::NonWireType { .. }));
}

#[test]
fn wire_kind_slice_is_rejected() {
    let ty = ResolvedTy::Slice(Box::new(ResolvedTy::I32));
    let err = ty
        .wire_kind()
        .expect_err("Slice should be rejected (no single wire kind)");
    assert!(matches!(err, WireBoundaryError::NonWireType { .. }));
}

#[test]
fn wire_kind_ok_for_native_primitives_and_named() {
    for (ty, expected_kind) in scalar_primitive_cases() {
        assert_eq!(
            ty.wire_kind(),
            Ok(expected_kind),
            "{ty:?}: scalar primitive should have a wire kind"
        );
    }
    let named = ResolvedTy::Named {
        name: "Foo".into(),
        args: vec![],
    };
    assert!(named.wire_kind().is_ok());
    assert!(!named.is_native_wire());
}

#[test]
fn native_wire_agrees_with_non_nested_wire_kind() {
    for (ty, expected_kind) in scalar_primitive_cases() {
        assert_eq!(
            ty.is_native_wire(),
            ty.wire_kind()
                .is_ok_and(|kind| !matches!(kind, PrimitiveWireKind::Nested(_))),
            "{ty:?}: native-wire predicate and primitive wire-kind dispatch diverged"
        );
        assert!(ty.is_native_wire(), "{ty:?} should be native wire");
        assert_eq!(ty.wire_kind(), Ok(expected_kind));
    }
}

#[test]
fn canonical_string_is_valid_wire_type_name_for_primitives() {
    for (ty, expected_kind) in scalar_primitive_cases() {
        assert_eq!(
            PrimitiveWireKind::from_type_name(&ty.canonical_string()),
            Ok(expected_kind),
            "{ty:?}: canonical_string should parse to its primitive wire kind"
        );
    }
}

#[test]
fn wire_boundary_error_display_covers_all_variants() {
    let cases: &[WireBoundaryError] = &[
        WireBoundaryError::OutOfRange {
            kind: PrimitiveWireKind::I8,
            value: 200,
        },
        WireBoundaryError::InvalidScalar { value: 0xD800 },
        WireBoundaryError::InvalidUtf8,
        WireBoundaryError::SignallingNaN,
        WireBoundaryError::NonWireType {
            canonical: "fn()->unit".into(),
        },
        WireBoundaryError::WireKindUnknown {
            name: "UnknownType".into(),
        },
    ];
    for err in cases {
        let s = err.to_string();
        assert!(
            !s.is_empty(),
            "error display should not be empty for {err:?}"
        );
    }
}

fn scalar_primitive_cases() -> Vec<(ResolvedTy, PrimitiveWireKind)> {
    vec![
        (ResolvedTy::Bool, PrimitiveWireKind::Bool),
        (ResolvedTy::I8, PrimitiveWireKind::I8),
        (ResolvedTy::I16, PrimitiveWireKind::I16),
        (ResolvedTy::I32, PrimitiveWireKind::I32),
        (ResolvedTy::I64, PrimitiveWireKind::I64),
        (ResolvedTy::U8, PrimitiveWireKind::U8),
        (ResolvedTy::U16, PrimitiveWireKind::U16),
        (ResolvedTy::U32, PrimitiveWireKind::U32),
        (ResolvedTy::U64, PrimitiveWireKind::U64),
        (ResolvedTy::F32, PrimitiveWireKind::F32),
        (ResolvedTy::F64, PrimitiveWireKind::F64),
        (ResolvedTy::Char, PrimitiveWireKind::Char),
        (ResolvedTy::String, PrimitiveWireKind::String),
        (ResolvedTy::Bytes, PrimitiveWireKind::Bytes),
        (ResolvedTy::Duration, PrimitiveWireKind::Duration),
        (ResolvedTy::Unit, PrimitiveWireKind::Unit),
    ]
}
