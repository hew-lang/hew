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
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

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
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
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

    let ty = ResolvedTy::Named {
        name: "Point".to_string(),
        args: Vec::new(),
    };
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

    let ty = ResolvedTy::Named {
        name: "Sparse".to_string(),
        args: Vec::new(),
    };
    assert_ne!(
        ValueClass::of_ty(&ty, &output.module.type_classes),
        ValueClass::BitCopy,
        "ValueClass::of_ty must agree: a Vec-bearing record is not BitCopy"
    );
}
