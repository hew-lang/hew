//! Checker-side coverage for the `T → dyn Trait` coercion side-table.
//!
//! At every accepted coercion site the checker records a
//! [`hew_types::DynCoercion`] entry in
//! [`hew_types::TypeCheckOutput::dyn_trait_coercions`] naming the trait, the
//! resolved concrete `Self` type, and the per-method impl resolution that the
//! downstream LLVM vtable emitter will consume.  Object safety is enforced
//! here at the coercion site:
//!
//! * Generic methods on the trait → `TraitNotObjectSafe { reason: "generic method", .. }`.
//! * `Self`-returning methods on the trait → `TraitNotObjectSafe { reason: "Self-returning method", .. }`.
//!
//! These tests pin both the positive metadata shape and the negative
//! object-safety rejections.  They run against an isolated checker so the
//! stdlib `Display` blanket impls (`std/builtins.hew`) do not interact.

mod common;

use common::{typecheck, typecheck_isolated};
use hew_types::error::TypeErrorKind;
use hew_types::DynCoercion;
use hew_types::Ty;

/// `i64 → dyn Display` records a `DynCoercion` whose `concrete_type` is the
/// canonical integer kind (`i64` after defaulting from `i64`), whose
/// `trait_name` is `Display`, and whose `method_table` contains the trait's
/// single declared method (`fmt`) mapped to the primitive impl key.
#[test]
fn int_to_dyn_display_records_coercion_entry() {
    let output = typecheck(
        r"
        fn use_display(value: dyn Display) {}

        fn main() {
            use_display(42);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.dyn_trait_coercions.len(),
        1,
        "expected exactly one dyn-trait coercion entry, got: {:#?}",
        output.dyn_trait_coercions
    );
    let entry = output
        .dyn_trait_coercions
        .values()
        .next()
        .expect("entry exists");
    assert_eq!(entry.trait_name, "Display");
    assert_eq!(entry.concrete_type, Ty::I64);
    assert_eq!(
        entry.method_table,
        vec![("fmt".to_string(), "i64::fmt".to_string())],
        "method_table should map trait method `fmt` to the impl key for i64"
    );
}

/// Two distinct concrete types coerced to `dyn Display` produce two
/// independent side-table entries.  Each entry's `concrete_type` and
/// `method_table` reflect that site's receiver.
#[test]
fn two_concrete_types_to_dyn_display_produce_distinct_entries() {
    let output = typecheck(
        r"
        fn use_display(value: dyn Display) {}

        fn main() {
            use_display(42);
            use_display(true);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.dyn_trait_coercions.len(),
        2,
        "expected exactly two coercion entries, got: {:#?}",
        output.dyn_trait_coercions
    );

    let mut by_concrete: Vec<&DynCoercion> = output.dyn_trait_coercions.values().collect();
    by_concrete
        .sort_by(|a, b| format!("{:?}", a.concrete_type).cmp(&format!("{:?}", b.concrete_type)));

    let bool_entry = by_concrete
        .iter()
        .find(|e| e.concrete_type == Ty::Bool)
        .expect("bool entry");
    let int_entry = by_concrete
        .iter()
        .find(|e| e.concrete_type == Ty::I64)
        .expect("i64 entry");

    assert_eq!(bool_entry.trait_name, "Display");
    assert_eq!(
        bool_entry.method_table,
        vec![("fmt".to_string(), "bool::fmt".to_string())]
    );
    assert_eq!(int_entry.trait_name, "Display");
    assert_eq!(
        int_entry.method_table,
        vec![("fmt".to_string(), "i64::fmt".to_string())]
    );
}

/// A trait with a generic method, used in `dyn` position, is rejected with
/// `TraitNotObjectSafe` and produces no side-table entry.  The diagnostic
/// names the offending method.
#[test]
fn generic_method_breaks_object_safety_in_dyn_position() {
    let output = typecheck_isolated(
        r#"
        trait WithGenericMethod {
            fn foo<U>(val: Self, u: U) -> i64;
        }

        type Widget { name: string; }

        impl WithGenericMethod for Widget {
            fn foo<U>(val: Widget, u: U) -> i64 {
                0
            }
        }

        fn use_dyn(value: dyn WithGenericMethod) {}

        fn main() {
            let w = Widget { name: "x" };
            use_dyn(w);
        }
        "#,
    );
    let rejects: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::TraitNotObjectSafe {
                    reason: "generic method",
                    method_name,
                    ..
                } if method_name == "foo"
            )
        })
        .collect();
    assert!(
        !rejects.is_empty(),
        "expected TraitNotObjectSafe(generic method, foo); got: {:#?}",
        output.errors
    );
    assert!(
        output.dyn_trait_coercions.is_empty(),
        "no side-table entry should be recorded for an unsafe trait, got: {:#?}",
        output.dyn_trait_coercions
    );
}

/// A trait with a `Self`-returning method, used in `dyn` position, is
/// rejected with `TraitNotObjectSafe`.  The diagnostic names the offending
/// method and the reason field is `"Self-returning method"`.
#[test]
fn self_return_breaks_object_safety_in_dyn_position() {
    let output = typecheck_isolated(
        r#"
        trait Cloneable {
            fn cloned(val: Self) -> Self;
        }

        type Widget { name: string; }

        impl Cloneable for Widget {
            fn cloned(val: Widget) -> Widget {
                Widget { name: val.name }
            }
        }

        fn use_dyn(value: dyn Cloneable) {}

        fn main() {
            let w = Widget { name: "x" };
            use_dyn(w);
        }
        "#,
    );
    let rejects: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::TraitNotObjectSafe {
                    reason: "Self-returning method",
                    method_name,
                    ..
                } if method_name == "cloned"
            )
        })
        .collect();
    assert!(
        !rejects.is_empty(),
        "expected TraitNotObjectSafe(Self-returning method, cloned); got: {:#?}",
        output.errors
    );
    assert!(
        output.dyn_trait_coercions.is_empty(),
        "no side-table entry should be recorded for an unsafe trait, got: {:#?}",
        output.dyn_trait_coercions
    );
}

#[test]
fn dyn_iterator_with_item_binding_object_safe() {
    let output = typecheck_isolated(
        r"
        trait Iterator {
            type Item;
            fn next(iter: Self) -> Option<Self::Item>;
        }

        type Counter { value: i32; }

        impl Iterator for Counter {
            type Item = i32;
            fn next(iter: Counter) -> Option<i32> {
                Some(iter.value)
            }
        }

        fn use_iter(iter: dyn Iterator<Item = i32>) -> Option<i32> {
            iter.next()
        }

        fn main() {
            use_iter(Counter { value: 1 });
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected projected dyn Iterator to type-check, got: {:#?}",
        output.errors
    );
    let entry = output
        .dyn_trait_coercions
        .values()
        .next()
        .expect("coercion entry");
    assert_eq!(
        entry.assoc_bindings,
        vec![hew_types::DynAssocBinding {
            trait_name: "Iterator".to_string(),
            assoc_name: "Item".to_string(),
            ty: Ty::I32,
        }]
    );
    assert_eq!(entry.vtable_entries.len(), 1);
    assert_eq!(
        entry.vtable_entries[0].signature.return_type,
        Ty::option(Ty::I32)
    );
}

#[test]
fn dyn_iterator_without_binding_rejected() {
    let output = typecheck_isolated(
        r"
        trait Iterator {
            type Item;
            fn next(iter: Self) -> Option<Self::Item>;
        }

        type Counter { value: i32; }

        impl Iterator for Counter {
            type Item = i32;
            fn next(iter: Counter) -> Option<i32> {
                Some(iter.value)
            }
        }

        fn use_iter(iter: dyn Iterator) {}

        fn main() {
            use_iter(Counter { value: 1 });
        }
        ",
    );
    let missing_binding_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::MissingAssocTypeBinding { trait_name, missing }
                    if trait_name == "Iterator" && missing == &vec!["Item".to_string()]
            ) && e.message.contains("Item")
        })
        .collect();
    assert!(
        !missing_binding_errors.is_empty(),
        "expected missing Item binding diagnostic, got: {:#?}",
        output.errors
    );
    assert_eq!(
        missing_binding_errors.len(),
        1,
        "missing associated-type binding should be reported once, got: {:#?}",
        output.errors
    );
    assert!(output.dyn_trait_coercions.is_empty());
}

#[test]
fn dyn_iterator_failed_projection_is_diagnostic() {
    let output = typecheck_isolated(
        r"
        trait Iterator {
            type Item;
            fn next(iter: Self) -> Option<Self::Item>;
        }

        type Counter { value: i32; }

        impl Iterator for Counter {
            fn next(iter: Counter) -> Option<i32> {
                Some(iter.value)
            }
        }

        fn use_iter(iter: dyn Iterator<Item = i32>) {}

        fn main() {
            use_iter(Counter { value: 1 });
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::AssocTypeProjectionFailed {
                type_name,
                trait_name,
                assoc_name,
            } if type_name == "Counter" && trait_name == "Iterator" && assoc_name == "Item"
        ) && e.message.contains("<Counter as Iterator>::Item")),
        "expected failed projection diagnostic for Counter/Iterator/Item, got: {:#?}",
        output.errors
    );
    assert!(output.dyn_trait_coercions.is_empty());
}

#[test]
fn dyn_distinct_bindings_get_distinct_vtables() {
    let output = typecheck_isolated(
        r#"
        trait Iterator {
            type Item;
            fn next(iter: Self) -> Option<Self::Item>;
        }

        type IntCounter { value: i32; }
        type StringCounter { value: string; }

        impl Iterator for IntCounter {
            type Item = i32;
            fn next(iter: IntCounter) -> Option<i32> { Some(iter.value) }
        }

        impl Iterator for StringCounter {
            type Item = string;
            fn next(iter: StringCounter) -> Option<string> { Some(iter.value) }
        }

        fn use_int(iter: dyn Iterator<Item = i32>) {}
        fn use_string(iter: dyn Iterator<Item = string>) {}

        fn main() {
            use_int(IntCounter { value: 1 });
            use_string(StringCounter { value: "x" });
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected clean check, got: {:#?}",
        output.errors
    );
    let keys: Vec<_> = output
        .dyn_trait_coercions
        .values()
        .map(|entry| entry.vtable_key.clone())
        .collect();
    assert_eq!(keys.len(), 2, "expected two vtable keys: {keys:#?}");
    assert_ne!(keys[0], keys[1]);
    assert!(keys.iter().any(|key| key
        .assoc_bindings
        .iter()
        .any(|binding| binding.assoc_name == "Item" && binding.ty == Ty::I32)));
    assert!(keys.iter().any(|key| key
        .assoc_bindings
        .iter()
        .any(|binding| binding.assoc_name == "Item" && binding.ty == Ty::String)));
}

#[test]
fn dyn_trait_method_signature_substituted() {
    let output = typecheck_isolated(
        r"
        trait Iterator {
            type Item;
            fn next(iter: Self) -> Option<Self::Item>;
        }

        type Counter { value: i32; }

        impl Iterator for Counter {
            type Item = i32;
            fn next(iter: Counter) -> Option<i32> {
                Some(iter.value)
            }
        }

        fn use_iter(iter: dyn Iterator<Item = i32>) -> Option<i32> {
            iter.next()
        }

        fn main() {
            use_iter(Counter { value: 1 });
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected method dispatch to see Option<i32>, got: {:#?}",
        output.errors
    );
    let entry = output
        .dyn_trait_coercions
        .values()
        .next()
        .and_then(|coercion| coercion.vtable_entries.first())
        .expect("vtable entry");
    assert_eq!(entry.signature.return_type, Ty::option(Ty::I32));
    assert!(
        !matches!(entry.signature.return_type, Ty::AssocType { .. }),
        "signature leaked abstract associated type: {entry:#?}"
    );
}

/// A bare `impl T { fn name(self) -> string }` that structurally matches a
/// trait `Named { fn name(self) -> string }` (no explicit `impl Named for T`)
/// still populates the `method_table` for a `T → dyn Named` coercion.  This
/// is the structural-trait-impl path: codegen vtable emission cannot
/// distinguish nominal from structural matches when building the slot
/// resolution, so the checker must surface both.
#[test]
fn structural_impl_populates_method_table_for_dyn_named() {
    let output = typecheck_isolated(
        r#"
        trait Named {
            fn name(val: Self) -> string;
        }

        type Widget { label: string; }

        impl Widget {
            fn name(val: Widget) -> string {
                val.label
            }
        }

        fn use_named(value: dyn Named) {}

        fn main() {
            let w = Widget { label: "x" };
            use_named(w);
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected clean check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.dyn_trait_coercions.len(),
        1,
        "expected one coercion entry, got: {:#?}",
        output.dyn_trait_coercions
    );
    let entry = output
        .dyn_trait_coercions
        .values()
        .next()
        .expect("entry exists");
    assert_eq!(entry.trait_name, "Named");
    assert_eq!(
        entry.concrete_type,
        Ty::Named {
            name: "Widget".to_string(),
            args: vec![],
        }
    );
    assert_eq!(
        entry.method_table,
        vec![("name".to_string(), "Widget::name".to_string())],
        "structural-match method_table should map `name` to `Widget::name`"
    );
}
