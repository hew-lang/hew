//! `BitCopy` auto-inference guards.
//!
//! `lower_program` promotes an unmarked `type T { ... }` to `BitCopy` only
//! when every field already resolves to a `BitCopy` value-class. These tests
//! pin both halves of the contract:
//!
//! * a record of primitives (all-BitCopy fields) IS upgraded, and
//! * a record carrying a non-BitCopy field (e.g. `Vec<i64>`) is NOT.
//!
//! The negative case is the load-bearing one — it blocks the silent
//! over-promotion that would let `Strategy::UnknownBlocked` get bypassed
//! for genuinely non-BitCopy aggregates.

use hew_hir::{lower_program, ResolutionCtx, ResourceMarker, ValueClass};
use hew_types::{
    module_registry::ModuleRegistry, BuiltinType, Checker, ResolvedTy, TypeCheckOutput,
};

fn lower_checked(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn lower_no_tc(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

#[test]
fn struct_of_primitives_is_inferred_bitcopy() {
    let source = r"
        pub type Point {
            x: i64,
            y: i64,
        }
    ";
    let output = lower_checked(source);
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    let entry = output
        .module
        .type_classes
        .get("Point")
        .expect("Point should be registered in the type-class table");
    assert_eq!(
        entry.0,
        ResourceMarker::BitCopy,
        "Point's fields are all BitCopy primitives; auto-inference must promote it"
    );

    let ty = ResolvedTy::named_user("Point", Vec::new());
    assert_eq!(
        ValueClass::of_ty(&ty, &output.module.type_classes),
        ValueClass::BitCopy,
        "ValueClass::of_ty must agree with the table"
    );
}

#[test]
fn struct_with_non_bitcopy_field_is_not_inferred_bitcopy() {
    // `Vec<i64>` resolves to a generic CowValue collection — not BitCopy.
    // A user struct embedding it MUST NOT be promoted, otherwise downstream
    // MIR/codegen would treat a heap-owning aggregate as a trivially copyable
    // value and violate move-checker invariants.
    let source = r"
        pub type Sparse {
            tag: i64,
            payload: Vec<i64>,
        }
    ";
    let output = lower_checked(source);
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );

    let entry = output
        .module
        .type_classes
        .get("Sparse")
        .expect("Sparse should be registered in the type-class table");
    assert_ne!(
        entry.0,
        ResourceMarker::BitCopy,
        "Sparse carries a non-BitCopy `Vec<i64>` field; auto-inference must NOT promote it (got marker = {:?})",
        entry.0,
    );

    let ty = ResolvedTy::named_user("Sparse", Vec::new());
    assert_ne!(
        ValueClass::of_ty(&ty, &output.module.type_classes),
        ValueClass::BitCopy,
        "ValueClass::of_ty must agree: a Vec-bearing record is not BitCopy"
    );
}

#[test]
fn record_decl_of_primitives_is_inferred_bitcopy() {
    let output = lower_no_tc(
        r"
        record Point { x: i64, y: i64 }
    ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    assert_eq!(
        output.module.type_classes.get("Point").map(|entry| entry.0),
        Some(ResourceMarker::BitCopy)
    );
}

#[test]
fn concrete_generic_type_instantiation_is_inferred_bitcopy() {
    let output = lower_checked(
        r"
        pub type Wrapper<T> { inner: T }
        fn main() -> i64 {
            let w = Wrapper { inner: 7 };
            0
        }
    ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    let key = hew_hir::mangle("Wrapper", &[ResolvedTy::I64]);
    assert_eq!(
        output.module.type_classes.get(&key).map(|entry| entry.0),
        Some(ResourceMarker::BitCopy),
        "concrete Wrapper<i64> layout should be BitCopy under its mangled key"
    );
    let ty = ResolvedTy::named_user("Wrapper", vec![ResolvedTy::I64]);
    assert_eq!(
        ValueClass::of_ty(&ty, &output.module.type_classes),
        ValueClass::BitCopy
    );
}

#[test]
fn nested_generic_type_instantiations_converge_to_bitcopy() {
    let output = lower_checked(
        r"
        pub type Outer<T> { inner: Inner<T> }
        pub type Inner<T> { value: T }
        fn main() -> i64 {
            let o: Outer<i64> = Outer { inner: Inner { value: 7 } };
            0
        }
    ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    for key in [
        hew_hir::mangle("Inner", &[ResolvedTy::I64]),
        hew_hir::mangle("Outer", &[ResolvedTy::I64]),
    ] {
        assert_eq!(
            output.module.type_classes.get(&key).map(|entry| entry.0),
            Some(ResourceMarker::BitCopy),
            "{key} should be BitCopy after fixed-point inference"
        );
    }
}

#[test]
fn concrete_generic_type_with_string_field_is_not_inferred_bitcopy() {
    let output = lower_checked(
        r#"
        pub type Wrapper<T> { inner: T }
        fn main() -> i64 {
            let w = Wrapper { inner: "owned" };
            0
        }
    "#,
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    let key = hew_hir::mangle("Wrapper", &[ResolvedTy::String]);
    assert_ne!(
        output.module.type_classes.get(&key).map(|entry| entry.0),
        Some(ResourceMarker::BitCopy),
        "Wrapper<string> must not be promoted to BitCopy"
    );
}

#[test]
fn empty_field_user_type_remains_uninferred() {
    let output = lower_no_tc("pub type Empty { }");
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    assert_ne!(
        output.module.type_classes.get("Empty").map(|entry| entry.0),
        Some(ResourceMarker::BitCopy),
        "empty-field user types must not be inferred as BitCopy"
    );
}

#[test]
fn user_shadowed_builtin_name_does_not_take_builtin_value_class() {
    let output = lower_no_tc(
        r"
        pub type Duplex {
            payload: string,
        }
    ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    let user_ty = ResolvedTy::named_user("Duplex", Vec::new());
    assert_eq!(
        ValueClass::of_ty(&user_ty, &output.module.type_classes),
        ValueClass::Unknown,
        "user-defined Duplex with builtin: None must not inherit builtin Resource classification"
    );

    let builtin_ty = ResolvedTy::named_builtin(
        "Duplex",
        BuiltinType::Duplex,
        vec![ResolvedTy::I64, ResolvedTy::I64],
    );
    assert_eq!(
        ValueClass::of_ty(&builtin_ty, &output.module.type_classes),
        ValueClass::AffineResource,
        "builtin-discriminated Duplex still follows builtin type-class registration"
    );
}
