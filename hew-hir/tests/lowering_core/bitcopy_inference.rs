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

use hew_hir::{lookup_type_marker_for_ty, lower_program, ResolutionCtx, ResourceMarker};
use hew_types::{module_registry::ModuleRegistry, BuiltinType, Checker, ResolvedTy};

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

#[test]
fn struct_of_primitives_is_inferred_bitcopy() {
    let source = r"pub type Point {
    x: i64;
    y: i64;
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

    let ty = ResolvedTy::named_path(&output.module.defs, "Point", Vec::new());
    assert_eq!(
        lookup_type_marker_for_ty(&ty, &output.module.type_classes),
        Some(ResourceMarker::BitCopy),
        "The nominal marker lookup must agree with the table"
    );
}

#[test]
fn struct_with_non_bitcopy_field_is_not_inferred_bitcopy() {
    // `Vec<i64>` resolves to a generic CowValue collection — not BitCopy.
    // A user struct embedding it MUST NOT be promoted, otherwise downstream
    // MIR/codegen would treat a heap-owning aggregate as a trivially copyable
    // value and violate move-checker invariants.
    let source = r"pub type Sparse {
    tag: i64;
    payload: Vec<i64>;
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

    let ty = ResolvedTy::named_path(&output.module.defs, "Sparse", Vec::new());
    assert_ne!(
        lookup_type_marker_for_ty(&ty, &output.module.type_classes),
        Some(ResourceMarker::BitCopy),
        "The nominal marker lookup must agree: a Vec-bearing record is not BitCopy"
    );
}

#[test]
fn zero_field_extern_backed_record_is_bitcopy_handle_stand_in() {
    // The FFI handle stand-in shape: a zero-field record produced and
    // consumed only by `#[extern_symbol]` methods. Its values exist behind the C ABI, so the
    // record classifies as a pointer-width `BitCopy` stand-in — the same class
    // the `#[opaque]` rule assigns — instead of falling to
    // `Strategy::UnknownBlocked` at the MIR boundary.
    let output = lower_checked(
        r"
        pub type Handle {
        }

        impl Handle {
            #[extern_symbol(hew_test_handle_new)]
            pub fn new(capacity: i64) -> Handle {
                Handle {  }
            }

            #[extern_symbol(hew_test_handle_release)]
            pub fn release(self) -> i32 {
                0
            }
        }
    ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    assert_eq!(
        output
            .module
            .type_classes
            .get("Handle")
            .map(|entry| entry.0),
        Some(ResourceMarker::BitCopy),
        "a zero-field record named in #[extern_symbol] signatures is an FFI \
         handle stand-in and must classify BitCopy"
    );
}

#[test]
fn zero_field_record_beside_unrelated_extern_stays_uninferred() {
    // The gate is the record being NAMED in an extern signature, not the mere
    // presence of FFI in the program: an unrelated `#[extern_symbol]` method
    // must not promote a bystander empty record.
    let output = lower_checked(
        r"
        pub type Empty {
        }

        pub type Handle {
        }

        impl Handle {
            #[extern_symbol(hew_test_handle_new)]
            pub fn new(capacity: i64) -> Handle {
                Handle {  }
            }
        }
    ",
    );
    assert_ne!(
        output.module.type_classes.get("Empty").map(|entry| entry.0),
        Some(ResourceMarker::BitCopy),
        "an empty record not named in any extern signature must stay uninferred"
    );
}

#[test]
fn empty_field_user_type_remains_uninferred() {
    let output = lower_checked("pub type Empty { }");
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
    let output = lower_checked(
        r"pub type Stream {
    payload: string;
}
",
    );
    assert!(
        output.diagnostics.is_empty(),
        "no diagnostics expected; got: {:#?}",
        output.diagnostics
    );
    let user_ty = ResolvedTy::named_path(&output.module.defs, "Stream", Vec::new());
    assert_eq!(
        lookup_type_marker_for_ty(&user_ty, &output.module.type_classes),
        Some(ResourceMarker::None),
        "user-defined Stream with builtin: None must not inherit builtin Resource classification"
    );

    let builtin_ty = ResolvedTy::named_builtin(BuiltinType::Stream, vec![ResolvedTy::I64]);
    assert_eq!(
        lookup_type_marker_for_ty(&builtin_ty, &output.module.type_classes),
        Some(ResourceMarker::Resource),
        "builtin-discriminated Stream still follows builtin type-class registration"
    );
}

#[test]
fn nested_generic_records_use_exact_checker_value_classes() {
    let parsed = hew_parser::parse(
        "type Inner<T> { value: T; } type Outer<T> { inner: Inner<T>; } fn main() {}",
    );
    assert!(parsed.errors.is_empty());
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let defs = checked.defs.clone();
    let mut facts = hew_types::TypeFactService::new(checked.type_fact_context, checked.type_facts);
    for name in ["Inner", "Outer"] {
        let primitive = ResolvedTy::named_path(&defs, name, vec![ResolvedTy::I64]);
        let owned = ResolvedTy::named_path(&defs, name, vec![ResolvedTy::String]);
        assert_eq!(
            facts.require(&primitive).unwrap().class,
            hew_types::ValueClass::BitCopy
        );
        assert_ne!(
            facts.require(&owned).unwrap().class,
            hew_types::ValueClass::BitCopy
        );
    }
}
