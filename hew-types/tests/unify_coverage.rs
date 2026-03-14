//! Integration tests for the type unification engine (`hew-types/src/unify.rs`).
//!
//! These tests target code paths not covered by the inline unit tests,
//! including slices, pointers, trait objects, machine types, qualified
//! name matching, `can_coerce` branches, error display, and `bind` edge cases.

use hew_types::ty::{Substitution, TraitObjectBound, Ty, TypeVar};
use hew_types::unify::{bind, can_coerce, unify, UnifyError};

// ---------------------------------------------------------------------------
// Helper: fresh substitution
// ---------------------------------------------------------------------------

fn fresh_subst() -> Substitution {
    Substitution::new()
}

// ===========================================================================
// bind() edge cases
// ===========================================================================

#[test]
fn bind_var_to_itself_is_noop() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    // Binding a variable to itself should succeed without inserting anything.
    assert!(bind(&mut subst, v, &Ty::Var(v)).is_ok());
    assert!(subst.lookup(v).is_none());
}

#[test]
fn bind_var_after_substitution_resolves_to_same_var() {
    // v1 -> v2, then bind(v2, Var(v1)) should resolve v1 to v2 and see
    // that v2 == v2, so it's a no-op.
    let mut subst = fresh_subst();
    let v1 = TypeVar::fresh();
    let v2 = TypeVar::fresh();
    subst.insert(v1, Ty::Var(v2));
    assert!(bind(&mut subst, v2, &Ty::Var(v1)).is_ok());
}

#[test]
fn bind_applies_substitution_before_occurs_check() {
    // v1 is already mapped to I32. Binding v2 to Tuple(Var(v1)) should
    // apply the substitution first, yielding Tuple(I32), which does NOT
    // contain v2 — so it must succeed.
    let mut subst = fresh_subst();
    let v1 = TypeVar::fresh();
    let v2 = TypeVar::fresh();
    subst.insert(v1, Ty::I32);
    let ty = Ty::Tuple(vec![Ty::Var(v1)]);
    assert!(bind(&mut subst, v2, &ty).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v2)), Ty::Tuple(vec![Ty::I32]));
}

#[test]
fn bind_occurs_check_through_named_type() {
    // Binding v to Named("Vec", [Var(v)]) should fail the occurs check.
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let ty = Ty::Named {
        name: "Vec".into(),
        args: vec![Ty::Var(v)],
    };
    let result = bind(&mut subst, v, &ty);
    assert!(matches!(result, Err(UnifyError::OccursCheck { .. })));
}

// ===========================================================================
// Slice unification
// ===========================================================================

#[test]
fn unify_identical_slices() {
    let mut subst = fresh_subst();
    let a = Ty::Slice(Box::new(Ty::I64));
    let b = Ty::Slice(Box::new(Ty::I64));
    assert!(unify(&mut subst, &a, &b).is_ok());
}

#[test]
fn unify_slice_with_type_var_element() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let a = Ty::Slice(Box::new(Ty::Var(v)));
    let b = Ty::Slice(Box::new(Ty::String));
    assert!(unify(&mut subst, &a, &b).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::String);
}

#[test]
fn unify_slice_element_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::Slice(Box::new(Ty::I32));
    let b = Ty::Slice(Box::new(Ty::Bool));
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_slice_with_non_slice_fails() {
    let mut subst = fresh_subst();
    let a = Ty::Slice(Box::new(Ty::I32));
    let b = Ty::Array(Box::new(Ty::I32), 5);
    assert!(unify(&mut subst, &a, &b).is_err());
}

// ===========================================================================
// Pointer unification
// ===========================================================================

#[test]
fn unify_const_pointers_same_pointee() {
    let mut subst = fresh_subst();
    let a = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    };
    let b = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
}

#[test]
fn unify_mut_pointers_with_var_pointee() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let a = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::Var(v)),
    };
    let b = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::F64),
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::F64);
}

#[test]
fn unify_pointers_mutability_mismatch() {
    // Mutable vs const pointer should fail unification.
    let mut subst = fresh_subst();
    let a = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::I32),
    };
    let b = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_pointer_pointee_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    };
    let b = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Bool),
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

// ===========================================================================
// TraitObject unification
// ===========================================================================

fn trait_bound(name: &str, args: Vec<Ty>) -> TraitObjectBound {
    TraitObjectBound {
        trait_name: name.to_string(),
        args,
    }
}

#[test]
fn unify_trait_objects_same_single_bound() {
    let mut subst = fresh_subst();
    let a = Ty::TraitObject {
        traits: vec![trait_bound("Display", vec![])],
    };
    let b = Ty::TraitObject {
        traits: vec![trait_bound("Display", vec![])],
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
}

#[test]
fn unify_trait_objects_different_order() {
    // Set-based comparison: order should not matter.
    let mut subst = fresh_subst();
    let a = Ty::TraitObject {
        traits: vec![trait_bound("Display", vec![]), trait_bound("Debug", vec![])],
    };
    let b = Ty::TraitObject {
        traits: vec![trait_bound("Debug", vec![]), trait_bound("Display", vec![])],
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
}

#[test]
fn unify_trait_objects_with_type_args() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let a = Ty::TraitObject {
        traits: vec![trait_bound("Iterator", vec![Ty::Var(v)])],
    };
    let b = Ty::TraitObject {
        traits: vec![trait_bound("Iterator", vec![Ty::I64])],
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I64);
}

#[test]
fn unify_trait_objects_different_trait_names() {
    let mut subst = fresh_subst();
    let a = Ty::TraitObject {
        traits: vec![trait_bound("Display", vec![])],
    };
    let b = Ty::TraitObject {
        traits: vec![trait_bound("Debug", vec![])],
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_trait_objects_different_count() {
    let mut subst = fresh_subst();
    let a = Ty::TraitObject {
        traits: vec![trait_bound("Display", vec![])],
    };
    let b = Ty::TraitObject {
        traits: vec![trait_bound("Display", vec![]), trait_bound("Debug", vec![])],
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_trait_objects_arg_arity_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::TraitObject {
        traits: vec![trait_bound("Iter", vec![Ty::I32])],
    };
    let b = Ty::TraitObject {
        traits: vec![trait_bound("Iter", vec![Ty::I32, Ty::Bool])],
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::ArityMismatch { .. })
    ));
}

// ===========================================================================
// Machine type unification
// ===========================================================================

#[test]
fn unify_machine_types_same_name() {
    let mut subst = fresh_subst();
    let a = Ty::Machine {
        name: "TrafficLight".into(),
    };
    let b = Ty::Machine {
        name: "TrafficLight".into(),
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
}

#[test]
fn unify_machine_types_different_names() {
    let mut subst = fresh_subst();
    let a = Ty::Machine {
        name: "TrafficLight".into(),
    };
    let b = Ty::Machine {
        name: "DoorLock".into(),
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_machine_with_named_same_name_no_args() {
    // Machine interops with bare Named of the same name (no type args).
    let mut subst = fresh_subst();
    let machine = Ty::Machine {
        name: "Sensor".into(),
    };
    let named = Ty::Named {
        name: "Sensor".into(),
        args: vec![],
    };
    assert!(unify(&mut subst, &machine, &named).is_ok());
    // Reverse direction too.
    assert!(unify(&mut subst, &named, &machine).is_ok());
}

#[test]
fn unify_machine_with_named_different_name() {
    let mut subst = fresh_subst();
    let machine = Ty::Machine {
        name: "Sensor".into(),
    };
    let named = Ty::Named {
        name: "Actuator".into(),
        args: vec![],
    };
    assert!(unify(&mut subst, &machine, &named).is_err());
}

#[test]
fn unify_machine_with_named_with_args_fails() {
    // Machine only matches Named with empty args.
    let mut subst = fresh_subst();
    let machine = Ty::Machine {
        name: "Sensor".into(),
    };
    let named = Ty::Named {
        name: "Sensor".into(),
        args: vec![Ty::I32],
    };
    assert!(unify(&mut subst, &machine, &named).is_err());
}

// ===========================================================================
// Qualified name matching
// ===========================================================================

#[test]
fn unify_qualified_and_bare_named() {
    // "json.Value" should unify with "Value"
    let mut subst = fresh_subst();
    let a = Ty::Named {
        name: "json.Value".into(),
        args: vec![],
    };
    let b = Ty::Named {
        name: "Value".into(),
        args: vec![],
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
}

#[test]
fn unify_qualified_different_modules_same_bare_name_fails() {
    // "auth.User" vs "billing.User" — both qualified, different modules.
    let mut subst = fresh_subst();
    let a = Ty::Named {
        name: "auth.User".into(),
        args: vec![],
    };
    let b = Ty::Named {
        name: "billing.User".into(),
        args: vec![],
    };
    assert!(unify(&mut subst, &a, &b).is_err());
}

#[test]
fn unify_qualified_with_type_args() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let a = Ty::Named {
        name: "collections.Map".into(),
        args: vec![Ty::String, Ty::Var(v)],
    };
    let b = Ty::Named {
        name: "Map".into(),
        args: vec![Ty::String, Ty::I64],
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I64);
}

// ===========================================================================
// Named type arity mismatch
// ===========================================================================

#[test]
fn unify_named_type_arity_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::Named {
        name: "Result".into(),
        args: vec![Ty::I32, Ty::String],
    };
    let b = Ty::Named {
        name: "Result".into(),
        args: vec![Ty::I32],
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::ArityMismatch { .. })
    ));
}

// ===========================================================================
// Function unification — additional paths
// ===========================================================================

#[test]
fn unify_functions_arity_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::Function {
        params: vec![Ty::I32, Ty::Bool],
        ret: Box::new(Ty::Unit),
    };
    let b = Ty::Function {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Unit),
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::ArityMismatch { .. })
    ));
}

#[test]
fn unify_functions_return_type_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::Function {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Bool),
    };
    let b = Ty::Function {
        params: vec![Ty::I32],
        ret: Box::new(Ty::String),
    };
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_function_with_closure_resolves_var() {
    // Function on the left, Closure on the right.
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let func = Ty::Function {
        params: vec![Ty::Var(v)],
        ret: Box::new(Ty::Bool),
    };
    let closure = Ty::Closure {
        params: vec![Ty::F32],
        ret: Box::new(Ty::Bool),
        captures: vec![Ty::String],
    };
    assert!(unify(&mut subst, &func, &closure).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::F32);
}

// ===========================================================================
// Error / Never — additional coverage
// ===========================================================================

#[test]
fn error_on_left_unifies_with_anything() {
    let mut subst = fresh_subst();
    assert!(unify(&mut subst, &Ty::Error, &Ty::F64).is_ok());
    assert!(unify(&mut subst, &Ty::Error, &Ty::String).is_ok());
    assert!(unify(&mut subst, &Ty::Error, &Ty::Tuple(vec![Ty::I32, Ty::Bool])).is_ok());
}

#[test]
fn never_on_right_unifies_with_anything() {
    let mut subst = fresh_subst();
    assert!(unify(&mut subst, &Ty::I32, &Ty::Never).is_ok());
    assert!(unify(&mut subst, &Ty::String, &Ty::Never).is_ok());
}

#[test]
fn error_on_right_unifies_with_anything() {
    let mut subst = fresh_subst();
    assert!(unify(&mut subst, &Ty::I32, &Ty::Error).is_ok());
}

// ===========================================================================
// can_coerce() — additional branches
// ===========================================================================

#[test]
fn can_coerce_array_to_slice() {
    let arr = Ty::Array(Box::new(Ty::I32), 10);
    let slc = Ty::Slice(Box::new(Ty::I32));
    assert!(can_coerce(&arr, &slc));
}

#[test]
fn can_coerce_array_to_slice_element_mismatch() {
    let arr = Ty::Array(Box::new(Ty::I32), 10);
    let slc = Ty::Slice(Box::new(Ty::Bool));
    assert!(!can_coerce(&arr, &slc));
}

#[test]
fn can_coerce_mut_pointer_to_const_pointer() {
    let mut_ptr = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::I32),
    };
    let const_ptr = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    };
    assert!(can_coerce(&mut_ptr, &const_ptr));
}

#[test]
fn can_coerce_const_pointer_to_mut_pointer_fails() {
    let const_ptr = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::I32),
    };
    let mut_ptr = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::I32),
    };
    assert!(!can_coerce(&const_ptr, &mut_ptr));
}

#[test]
fn can_coerce_mut_pointer_pointee_mismatch() {
    let mut_ptr = Ty::Pointer {
        is_mutable: true,
        pointee: Box::new(Ty::I32),
    };
    let const_ptr = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Bool),
    };
    assert!(!can_coerce(&mut_ptr, &const_ptr));
}

#[test]
fn can_coerce_error_on_right() {
    assert!(can_coerce(&Ty::I32, &Ty::Error));
    assert!(can_coerce(&Ty::String, &Ty::Error));
}

#[test]
fn can_coerce_error_on_left() {
    assert!(can_coerce(&Ty::Error, &Ty::I64));
}

#[test]
fn can_coerce_never_on_left() {
    assert!(can_coerce(&Ty::Never, &Ty::Bool));
    assert!(can_coerce(&Ty::Never, &Ty::String));
}

#[test]
fn can_coerce_same_types() {
    assert!(can_coerce(&Ty::Bool, &Ty::Bool));
    assert!(can_coerce(&Ty::String, &Ty::String));
    assert!(can_coerce(&Ty::F64, &Ty::F64));
}

#[test]
fn can_coerce_incompatible_types() {
    assert!(!can_coerce(&Ty::I32, &Ty::String));
    assert!(!can_coerce(&Ty::Bool, &Ty::F64));
}

// ===========================================================================
// UnifyError Display formatting
// ===========================================================================

#[test]
fn display_mismatch_error() {
    let err = UnifyError::Mismatch {
        expected: Ty::I32,
        actual: Ty::Bool,
    };
    let msg = format!("{err}");
    assert!(msg.contains("type mismatch"));
    assert!(msg.contains("i32"));
    assert!(msg.contains("bool"));
}

#[test]
fn display_occurs_check_error() {
    let v = TypeVar::fresh();
    let err = UnifyError::OccursCheck {
        var: v,
        ty: Ty::Tuple(vec![Ty::Var(v)]),
    };
    let msg = format!("{err}");
    assert!(msg.contains("infinite type"));
    assert!(msg.contains("occurs in"));
}

#[test]
fn display_arity_mismatch_error() {
    let err = UnifyError::ArityMismatch {
        expected: 3,
        actual: 1,
    };
    let msg = format!("{err}");
    assert!(msg.contains("arity mismatch"));
    assert!(msg.contains('3'));
    assert!(msg.contains('1'));
}

#[test]
fn unify_error_is_std_error() {
    // Verify the std::error::Error impl exists.
    let err: Box<dyn std::error::Error> = Box::new(UnifyError::Mismatch {
        expected: Ty::I32,
        actual: Ty::Bool,
    });
    assert!(err.to_string().contains("type mismatch"));
}

// ===========================================================================
// Substitution resolution — transitive chains
// ===========================================================================

#[test]
fn resolve_three_var_chain() {
    let mut subst = fresh_subst();
    let v1 = TypeVar::fresh();
    let v2 = TypeVar::fresh();
    let v3 = TypeVar::fresh();
    // v1 -> v2 -> v3 -> String
    subst.insert(v1, Ty::Var(v2));
    subst.insert(v2, Ty::Var(v3));
    subst.insert(v3, Ty::String);
    assert_eq!(subst.resolve(&Ty::Var(v1)), Ty::String);
    assert_eq!(subst.resolve(&Ty::Var(v2)), Ty::String);
}

#[test]
fn resolve_var_in_nested_structure() {
    // Resolving a non-Var type should apply substitution to children.
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    subst.insert(v, Ty::I64);
    let ty = Ty::Named {
        name: "Option".into(),
        args: vec![Ty::Var(v)],
    };
    let resolved = subst.resolve(&ty);
    assert_eq!(resolved, Ty::option(Ty::I64));
}

#[test]
fn resolve_unbound_var_stays_var() {
    let subst = fresh_subst();
    let v = TypeVar::fresh();
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::Var(v));
}

// ===========================================================================
// Unifying Option<T> with T as a type variable
// ===========================================================================

#[test]
fn unify_option_with_type_var_inner() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    let a = Ty::option(Ty::Var(v));
    let b = Ty::option(Ty::I32);
    assert!(unify(&mut subst, &a, &b).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I32);
}

// ===========================================================================
// Cross-variant mismatches (coverage of the catch-all arm)
// ===========================================================================

#[test]
fn unify_function_with_tuple_fails() {
    let mut subst = fresh_subst();
    let func = Ty::Function {
        params: vec![Ty::I32],
        ret: Box::new(Ty::Bool),
    };
    let tuple = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
    assert!(matches!(
        unify(&mut subst, &func, &tuple),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_named_with_primitive_fails() {
    let mut subst = fresh_subst();
    let named = Ty::Named {
        name: "Vec".into(),
        args: vec![Ty::I32],
    };
    assert!(matches!(
        unify(&mut subst, &named, &Ty::I32),
        Err(UnifyError::Mismatch { .. })
    ));
}

#[test]
fn unify_array_with_tuple_fails() {
    let mut subst = fresh_subst();
    let arr = Ty::Array(Box::new(Ty::I32), 3);
    let tup = Ty::Tuple(vec![Ty::I32, Ty::I32, Ty::I32]);
    assert!(unify(&mut subst, &arr, &tup).is_err());
}

// ===========================================================================
// Array element type mismatch (distinct from size mismatch)
// ===========================================================================

#[test]
fn unify_array_element_mismatch() {
    let mut subst = fresh_subst();
    let a = Ty::Array(Box::new(Ty::I32), 5);
    let b = Ty::Array(Box::new(Ty::Bool), 5);
    assert!(matches!(
        unify(&mut subst, &a, &b),
        Err(UnifyError::Mismatch { .. })
    ));
}

// ===========================================================================
// Multiple unifications building up the substitution
// ===========================================================================

#[test]
fn multiple_vars_resolved_by_successive_unifications() {
    let mut subst = fresh_subst();
    let v1 = TypeVar::fresh();
    let v2 = TypeVar::fresh();
    let v3 = TypeVar::fresh();

    // Unify a function with vars in params and return type.
    let a = Ty::Function {
        params: vec![Ty::Var(v1), Ty::Var(v2)],
        ret: Box::new(Ty::Var(v3)),
    };
    let b = Ty::Function {
        params: vec![Ty::I32, Ty::String],
        ret: Box::new(Ty::Bool),
    };
    assert!(unify(&mut subst, &a, &b).is_ok());
    assert_eq!(subst.resolve(&Ty::Var(v1)), Ty::I32);
    assert_eq!(subst.resolve(&Ty::Var(v2)), Ty::String);
    assert_eq!(subst.resolve(&Ty::Var(v3)), Ty::Bool);
}

// ===========================================================================
// Substitution snapshot and restore
// ===========================================================================

#[test]
fn substitution_snapshot_and_restore() {
    let mut subst = fresh_subst();
    let v = TypeVar::fresh();
    subst.insert(v, Ty::I32);

    let snap = subst.snapshot();
    // Mutate further.
    let v2 = TypeVar::fresh();
    subst.insert(v2, Ty::Bool);
    assert!(subst.lookup(v2).is_some());

    // Restore — v2 mapping should be gone.
    subst.restore(snap);
    assert!(subst.lookup(v2).is_none());
    assert_eq!(subst.resolve(&Ty::Var(v)), Ty::I32);
}
