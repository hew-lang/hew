//! Descriptor authority matrix — §6.2 from `lane-v05-type-wire-native-values.md`.
//!
//! Verifies:
//! 1. `canonical_string()` produces the expected stable string for every
//!    `TypeDescriptor` shape.
//! 2. `is_native_wire()` classifies each shape correctly.
//! 3. `from_ty_strict_generic_args` rejects mismatched generic arities.
//! 4. The four `from_ty` reject paths still fire (regression guard).

use hew_types::{
    resolved_ty::{BoundaryError, ResolvedTraitBound, ResolvedTy},
    ty::{Ty, TypeVar},
};

#[test]
fn canonical_string_primitive_types() {
    let cases = [
        (ResolvedTy::Bool, "bool"),
        (ResolvedTy::I8, "i8"),
        (ResolvedTy::I16, "i16"),
        (ResolvedTy::I32, "i32"),
        (ResolvedTy::I64, "i64"),
        (ResolvedTy::U8, "u8"),
        (ResolvedTy::U16, "u16"),
        (ResolvedTy::U32, "u32"),
        (ResolvedTy::U64, "u64"),
        (ResolvedTy::F32, "f32"),
        (ResolvedTy::F64, "f64"),
        (ResolvedTy::Char, "char"),
        (ResolvedTy::String, "string"),
        (ResolvedTy::Bytes, "bytes"),
        (ResolvedTy::Duration, "duration"),
        (ResolvedTy::Unit, "unit"),
        (ResolvedTy::Never, "never"),
    ];
    for (ty, expected) in cases {
        assert_eq!(
            ty.canonical_string(),
            expected,
            "canonical mismatch for {ty:?}"
        );
    }
}

#[test]
fn canonical_string_tuple() {
    let ty = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::String]);
    assert_eq!(ty.canonical_string(), "(i64,string)");
}

#[test]
fn canonical_string_tuple_singleton() {
    let ty = ResolvedTy::Tuple(vec![ResolvedTy::Bool]);
    assert_eq!(ty.canonical_string(), "(bool)");
}

#[test]
fn canonical_string_array() {
    let ty = ResolvedTy::Array(Box::new(ResolvedTy::I32), 4);
    assert_eq!(ty.canonical_string(), "[i32;4]");
}

#[test]
fn canonical_string_slice() {
    let ty = ResolvedTy::Slice(Box::new(ResolvedTy::U8));
    assert_eq!(ty.canonical_string(), "[u8]");
}

#[test]
fn canonical_string_bare_named() {
    let ty = ResolvedTy::Named {
        name: "Point".into(),
        args: vec![],
    };
    assert_eq!(ty.canonical_string(), "Point");
}

#[test]
fn canonical_string_generic_named() {
    let ty = ResolvedTy::Named {
        name: "Pair".into(),
        args: vec![ResolvedTy::I64, ResolvedTy::String],
    };
    assert_eq!(ty.canonical_string(), "Pair<i64,string>");
}

#[test]
fn canonical_string_nested_generic() {
    let inner = ResolvedTy::Named {
        name: "Option".into(),
        args: vec![ResolvedTy::String],
    };
    let outer = ResolvedTy::Named {
        name: "Vec".into(),
        args: vec![inner],
    };
    assert_eq!(outer.canonical_string(), "Vec<Option<string>>");
}

#[test]
fn canonical_string_function() {
    let ty = ResolvedTy::Function {
        params: vec![ResolvedTy::I32, ResolvedTy::Bool],
        ret: Box::new(ResolvedTy::String),
    };
    assert_eq!(ty.canonical_string(), "fn(i32,bool)->string");
}

#[test]
fn canonical_string_function_no_params() {
    let ty = ResolvedTy::Function {
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
    };
    assert_eq!(ty.canonical_string(), "fn()->unit");
}

#[test]
fn canonical_string_closure_with_captures() {
    let ty = ResolvedTy::Closure {
        params: vec![ResolvedTy::I32],
        ret: Box::new(ResolvedTy::Bool),
        captures: vec![ResolvedTy::String, ResolvedTy::I64],
    };
    assert_eq!(ty.canonical_string(), "closure(i32)->bool{string,i64}");
}

#[test]
fn canonical_string_pointer_mutable() {
    let ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::I32),
    };
    assert_eq!(ty.canonical_string(), "*mut i32");
}

#[test]
fn canonical_string_pointer_const() {
    let ty = ResolvedTy::Pointer {
        is_mutable: false,
        pointee: Box::new(ResolvedTy::I32),
    };
    assert_eq!(ty.canonical_string(), "*const i32");
}

#[test]
fn canonical_string_trait_object_single_bound() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Iterator".into(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    assert_eq!(ty.canonical_string(), "dyn(Iterator)");
}

#[test]
fn canonical_string_trait_object_multiple_bounds() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![
            ResolvedTraitBound {
                trait_name: "Send".into(),
                args: vec![],
                assoc_bindings: vec![],
            },
            ResolvedTraitBound {
                trait_name: "Sync".into(),
                args: vec![],
                assoc_bindings: vec![],
            },
        ],
    };
    assert_eq!(ty.canonical_string(), "dyn(Send+Sync)");
}

#[test]
fn canonical_string_trait_object_with_type_arg() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Iterator".into(),
            args: vec![ResolvedTy::I32],
            assoc_bindings: vec![],
        }],
    };
    assert_eq!(ty.canonical_string(), "dyn(Iterator<i32>)");
}

#[test]
fn canonical_string_is_deterministic() {
    let ty = ResolvedTy::Named {
        name: "Pair".into(),
        args: vec![ResolvedTy::I64, ResolvedTy::String],
    };
    assert_eq!(ty.canonical_string(), ty.canonical_string());
}

#[test]
fn is_native_wire_true_for_primitive_scalars() {
    let native = [
        ResolvedTy::Bool,
        ResolvedTy::I8,
        ResolvedTy::I16,
        ResolvedTy::I32,
        ResolvedTy::I64,
        ResolvedTy::U8,
        ResolvedTy::U16,
        ResolvedTy::U32,
        ResolvedTy::U64,
        ResolvedTy::F32,
        ResolvedTy::F64,
        ResolvedTy::Char,
        ResolvedTy::String,
        ResolvedTy::Bytes,
        ResolvedTy::Duration,
        ResolvedTy::Unit,
    ];
    for ty in native {
        assert!(ty.is_native_wire(), "{ty:?} should be native wire");
    }
}

#[test]
fn is_native_wire_false_for_non_native_types() {
    let non_native = [
        ResolvedTy::Never,
        ResolvedTy::Tuple(vec![ResolvedTy::I32]),
        ResolvedTy::Array(Box::new(ResolvedTy::I32), 4),
        ResolvedTy::Slice(Box::new(ResolvedTy::I32)),
        ResolvedTy::Named {
            name: "Point".into(),
            args: vec![],
        },
        ResolvedTy::Function {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
        },
        ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![],
        },
        ResolvedTy::Pointer {
            is_mutable: false,
            pointee: Box::new(ResolvedTy::I32),
        },
        ResolvedTy::TraitObject { traits: vec![] },
    ];
    for ty in non_native {
        assert!(!ty.is_native_wire(), "{ty:?} should not be native wire");
    }
}

#[test]
fn strict_generic_args_accepts_matching_arity() {
    let ty = Ty::Named {
        name: "Pair".into(),
        args: vec![Ty::I64, Ty::String],
    };
    let result = ResolvedTy::from_ty_strict_generic_args(&ty, 2);
    assert_eq!(
        result,
        Ok(ResolvedTy::Named {
            name: "Pair".into(),
            args: vec![ResolvedTy::I64, ResolvedTy::String],
        })
    );
}

#[test]
fn strict_generic_args_rejects_too_few_args() {
    let ty = Ty::Named {
        name: "Pair".into(),
        args: vec![],
    };
    let result = ResolvedTy::from_ty_strict_generic_args(&ty, 2);
    assert_eq!(
        result,
        Err(BoundaryError::GenericArityMismatch {
            expected: 2,
            got: 0
        })
    );
}

#[test]
fn strict_generic_args_rejects_too_many_args() {
    let ty = Ty::Named {
        name: "Box".into(),
        args: vec![Ty::I32, Ty::Bool],
    };
    let result = ResolvedTy::from_ty_strict_generic_args(&ty, 1);
    assert_eq!(
        result,
        Err(BoundaryError::GenericArityMismatch {
            expected: 1,
            got: 2
        })
    );
}

#[test]
fn strict_generic_args_passes_through_non_named_types() {
    let ty = Ty::I32;
    assert_eq!(
        ResolvedTy::from_ty_strict_generic_args(&ty, 0),
        Ok(ResolvedTy::I32)
    );
    assert_eq!(
        ResolvedTy::from_ty_strict_generic_args(&ty, 999),
        Ok(ResolvedTy::I32)
    );
}

#[test]
fn strict_generic_args_propagates_boundary_errors_from_args() {
    let var = TypeVar::fresh();
    let ty = Ty::Named {
        name: "Box".into(),
        args: vec![Ty::Var(var)],
    };
    let result = ResolvedTy::from_ty_strict_generic_args(&ty, 1);
    assert_eq!(result, Err(BoundaryError::UnresolvedInference { var }));
}

#[test]
fn from_ty_rejects_unresolved_inference() {
    let var = TypeVar::fresh();
    assert_eq!(
        ResolvedTy::from_ty(&Ty::Var(var)),
        Err(BoundaryError::UnresolvedInference { var })
    );
}

#[test]
fn from_ty_rejects_error_placeholder() {
    assert_eq!(
        ResolvedTy::from_ty(&Ty::Error),
        Err(BoundaryError::TaintedError)
    );
}

#[test]
fn from_ty_rejects_int_literal() {
    assert_eq!(
        ResolvedTy::from_ty(&Ty::IntLiteral),
        Err(BoundaryError::UnmaterializedLiteral { is_integer: true })
    );
}

#[test]
fn from_ty_rejects_float_literal() {
    assert_eq!(
        ResolvedTy::from_ty(&Ty::FloatLiteral),
        Err(BoundaryError::UnmaterializedLiteral { is_integer: false })
    );
}

#[test]
fn generic_arity_mismatch_error_display_is_actionable() {
    let err = BoundaryError::GenericArityMismatch {
        expected: 2,
        got: 0,
    };
    let s = err.to_string();
    assert!(s.contains('2'), "should mention expected count: {s}");
    assert!(s.contains('0'), "should mention actual count: {s}");
}
