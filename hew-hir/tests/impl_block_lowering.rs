//! V0b: user `impl [Trait for] UserNominal` HIR lowering beyond `Index`.
//!
//! These tests pin the V0b sufficient surface (per `vec-iterator-substrate-v05`
//! plan §3 V0b):
//!   - `impl<T> Trait for UserNominal<T> { type X = ...; fn m(...) { ... } }`
//!     lowers without falling through to the `unsupported "top-level-item"
//!     "slice-2"` catch-all.
//!   - Methods land as `HirItem::Function` entries named
//!     `<SelfType>::<method>` (matching the pre-V0b `impl Index` precedent).
//!   - A metadata-only `HirItem::Impl` is emitted alongside, carrying the
//!     trait/self pair, the lowered associated-type bindings, and the list
//!     of emitted method symbols.
//!   - Fail-closed boundary: shapes outside V0b emit
//!     `ImplBlockShapeNotLowered { shape }`, NOT a generic
//!     `NotYetImplemented { construct: "top-level-item" }`.

use hew_hir::{lower_program, HirDiagnosticKind, HirItem, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    // Type errors are not asserted here — some fixtures intentionally exercise
    // checker fall-through paths. Per-test asserts cover the actually-relevant
    // diagnostics in each scenario.
    let _ = &tc_output;
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

fn assert_no_top_level_item_unsupported(output: &hew_hir::LowerOutput) {
    let leaked: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct == "top-level-item"
            )
        })
        .collect();
    assert!(
        leaked.is_empty(),
        "V0b must not drop impl-blocks via the generic top-level-item \
         catch-all; got: {leaked:#?}"
    );
}

#[test]
fn iterator_impl_on_user_nominal_lowers_method_and_metadata() {
    let output = lower(
        r"
        pub type VecIter<T> {
            idx: i64;
        }

        impl<T> Iterator for VecIter<T> {
            type Item = T;

            fn next(it: VecIter<T>) -> Option<T> {
                None
            }
        }
        ",
    );

    assert_no_top_level_item_unsupported(&output);

    // The method body must land as a top-level HirItem::Function under the
    // qualified `<SelfType>::<method>` symbol.
    let method_fn = output.module.items.iter().find_map(|item| {
        if let HirItem::Function(f) = item {
            if f.name == "VecIter::next" {
                return Some(f);
            }
        }
        None
    });
    assert!(
        method_fn.is_some(),
        "impl<T> Iterator for VecIter<T>::next must lower as \
         HirItem::Function named `VecIter::next`; items: {:#?}",
        output
            .module
            .items
            .iter()
            .filter_map(|i| if let HirItem::Function(f) = i {
                Some(f.name.clone())
            } else {
                None
            })
            .collect::<Vec<_>>()
    );

    // The metadata-only HirItem::Impl must also be emitted, carrying the
    // trait/self pair, the lowered `type Item = T` alias, and the list of
    // emitted method symbols.
    let impl_block = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Impl(b) = item {
                Some(b)
            } else {
                None
            }
        })
        .expect("HirItem::Impl metadata anchor must be emitted");
    assert_eq!(impl_block.trait_name.as_deref(), Some("Iterator"));
    assert_eq!(impl_block.self_type_name, "VecIter");
    assert_eq!(impl_block.type_params, vec!["T".to_string()]);
    assert_eq!(
        impl_block.method_symbols,
        vec!["VecIter::next".to_string()],
        "method_symbols must mirror the flattened HirItem::Function names"
    );
    assert_eq!(
        impl_block.type_aliases.len(),
        1,
        "assoc-type `type Item = T;` must lower to one type_aliases entry"
    );
    assert_eq!(impl_block.type_aliases[0].0, "Item");
}

#[test]
fn inherent_impl_on_user_nominal_lowers_methods() {
    let output = lower(
        r"
        pub type Counter {
            n: i64;
        }

        impl Counter {
            fn get(c: Counter) -> i64 {
                c.n
            }
        }
        ",
    );

    assert_no_top_level_item_unsupported(&output);

    let has_method = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Counter::get"));
    assert!(
        has_method,
        "inherent impl Counter must lower the `get` method as `Counter::get`"
    );
    let block = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Impl(b) = item {
                Some(b)
            } else {
                None
            }
        })
        .expect("inherent impl must also emit a metadata HirItem::Impl");
    assert!(
        block.trait_name.is_none(),
        "inherent impl must have trait_name == None; got {:?}",
        block.trait_name
    );
}

#[test]
fn index_impl_still_lowers_unchanged() {
    // Regression guard: the pre-V0b `impl Index for ...` lowering precedent
    // continues to emit a `<Self>::at` HirItem::Function. V0b unifies the
    // arm; this test asserts the unification did not regress the Index case
    // that other fixtures (vec_index_lowering, index_trait_lowering) rely on
    // implicitly for the rewrite-table consumer.
    let output = lower(
        r"
        type Grid {
            bias: i32;
        }

        impl Index for Grid {
            type Output = i32;

            fn at(g: Grid, key: i32) -> i32 {
                g.bias + key
            }
        }
        ",
    );

    assert_no_top_level_item_unsupported(&output);
    let has_at = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Grid::at"));
    assert!(has_at, "Grid::at must still be emitted post-V0b");
}

#[test]
fn impl_block_with_single_bound_where_clause_lowers() {
    // V0b admits single-bound where-clauses of shape `where T: SingleTrait`
    // where `T` is one of the impl's own type parameters. The bound has no
    // runtime artefact (consumed by the checker's bound-enforcement plumbing
    // at the use site); HIR lowering simply admits the impl and threads its
    // methods through the regular flatten-to-`HirItem::Function` path.
    let output = lower(
        r"
        pub trait Eq {
            fn eq(a: Self, b: Self) -> bool;
        }

        pub type Wrap<T> {
            inner: T;
        }

        impl<T> Wrap<T> where T: Eq {
            fn first(w: Wrap<T>) -> Wrap<T> {
                w
            }
        }
        ",
    );

    assert_no_top_level_item_unsupported(&output);
    let shape_diag = output.diagnostics.iter().find_map(|d| {
        if let HirDiagnosticKind::ImplBlockShapeNotLowered { shape } = &d.kind {
            Some(shape.clone())
        } else {
            None
        }
    });
    assert!(
        shape_diag.is_none(),
        "single-bound `where T: Trait` impl must lower; got shape diagnostic: {shape_diag:?} \
         out of full diagnostics: {:#?}",
        output.diagnostics,
    );
    let has_first = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Wrap::first"));
    assert!(
        has_first,
        "Wrap::first must be emitted when the where-clause is admitted"
    );
}

#[test]
fn impl_block_with_multi_bound_where_clause_emits_fail_closed_shape_diagnostic() {
    // Fail-closed boundary: multi-bound `where T: A + B` predicates are
    // outside V0b's sufficient surface (γ1 lifts only single-bound
    // `where T: Trait`). The diagnostic must surface as
    // `ImplBlockShapeNotLowered { shape: "multi-bound where-clause on `T`" }`
    // rather than fall through to the generic `top-level-item` catch-all.
    let output = lower(
        r"
        pub trait Eq {
            fn eq(a: Self, b: Self) -> bool;
        }

        pub trait Ord {
            fn cmp(a: Self, b: Self) -> i64;
        }

        pub type Wrap<T> {
            inner: T;
        }

        impl<T> Wrap<T> where T: Eq + Ord {
            fn first(w: Wrap<T>) -> Wrap<T> {
                w
            }
        }
        ",
    );

    let shape_diag = output.diagnostics.iter().find_map(|d| {
        if let HirDiagnosticKind::ImplBlockShapeNotLowered { shape } = &d.kind {
            Some(shape.clone())
        } else {
            None
        }
    });
    assert_eq!(
        shape_diag.as_deref(),
        Some("multi-bound where-clause on `T`"),
        "V0b must surface a precise shape diagnostic for multi-bound \
         where-clauses; got diagnostics: {:#?}",
        output.diagnostics,
    );
}

#[test]
fn impl_block_with_where_clause_on_non_type_param_emits_fail_closed_shape_diagnostic() {
    // Fail-closed boundary: predicates on non-type-param types (e.g.
    // `where Wrap<T>: Eq`) are outside V0b's sufficient surface — γ1 only
    // admits predicates on the impl's own type parameters.
    let output = lower(
        r"
        pub trait Eq {
            fn eq(a: Self, b: Self) -> bool;
        }

        pub type Wrap<T> {
            inner: T;
        }

        impl<T> Wrap<T> where Wrap<T>: Eq {
            fn first(w: Wrap<T>) -> Wrap<T> {
                w
            }
        }
        ",
    );

    let shape_diag = output.diagnostics.iter().find_map(|d| {
        if let HirDiagnosticKind::ImplBlockShapeNotLowered { shape } = &d.kind {
            Some(shape.clone())
        } else {
            None
        }
    });
    assert!(
        matches!(
            shape_diag.as_deref(),
            Some(s) if s.starts_with("where-clause predicate on parameterised type")
        ),
        "V0b must surface a precise shape diagnostic for parameterised-type \
         where-clauses; got diagnostic: {shape_diag:?} out of full diagnostics: \
         {:#?}",
        output.diagnostics,
    );
}
