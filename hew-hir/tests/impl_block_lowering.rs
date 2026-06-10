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
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
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
fn impl_block_with_multi_bound_where_clause_lowers() {
    // Slice ε lift: multi-bound `where T: A + B` predicates are now admitted
    // (the checker already enforces them; HIR has no runtime artefact for bounds).
    // Both the single-bound and multi-bound forms must lower cleanly and emit
    // the impl's method as a top-level `HirItem::Function`.
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
        "multi-bound `where T: A + B` impl must lower in slice ε; \
         got shape diagnostic: {shape_diag:?} out of full diagnostics: {:#?}",
        output.diagnostics,
    );
    let has_first = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Wrap::first"));
    assert!(
        has_first,
        "Wrap::first must be emitted when the multi-bound where-clause is admitted"
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

#[test]
fn impl_block_blanket_impl_emits_fail_closed_shape_diagnostic() {
    // Fail-closed boundary: blanket impls (`impl<T> Trait for T`) require
    // specialisation infrastructure that is not in V0b. The diagnostic must
    // surface as a precise `ImplBlockShapeNotLowered { shape: "blanket impl
    // ..." }` rather than fall through to the generic `top-level-item`
    // catch-all.
    let output = lower(
        r"
        pub trait Eq {
            fn eq(a: Self, b: Self) -> bool;
        }

        impl<T> Eq for T {
            fn eq(a: T, b: T) -> bool {
                true
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
            Some(s) if s.starts_with("blanket impl")
        ),
        "V0b must surface a precise shape diagnostic for blanket impls; got \
         shape diagnostic: {shape_diag:?} out of full diagnostics: {:#?}",
        output.diagnostics,
    );
}

#[test]
fn impl_block_non_nominal_target_emits_fail_closed_shape_diagnostic() {
    // Fail-closed boundary: impl targets must be a named nominal type. Tuple,
    // array, function, and trait-object targets are not yet supported. The
    // diagnostic must surface as `ImplBlockShapeNotLowered { shape: "impl on
    // non-nominal target" }` rather than fall through to the generic
    // `top-level-item` catch-all.
    let output = lower(
        r"
        pub trait Show {
            fn show(self) -> bool;
        }

        impl<A, B> Show for (A, B) {
            fn show(t: (A, B)) -> bool {
                true
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
        Some("impl on non-nominal target"),
        "V0b must surface a precise shape diagnostic for non-nominal impl \
         targets; got diagnostics: {:#?}",
        output.diagnostics,
    );
}

#[test]
fn impl_block_inherent_on_builtin_nominal_emits_fail_closed_shape_diagnostic() {
    // Fail-closed boundary: bare inherent impls on builtin nominal types
    // (`impl Vec<T> { ... }`, `impl HashMap<K, V> { ... }`, etc.) collide
    // downstream with the stdlib-shipped inherent impls registered via the
    // checker's `register_builtins_hew_impls` path. V0b rejects them at the
    // lowering boundary so the failure site matches the cause site, rather
    // than surfacing later as a confusing duplicate-definition error.
    //
    // Trait impls on builtins (`impl MyTrait for Vec<T>`) are deliberately
    // not covered by this guard — orphan-rule policing is a separate concern.
    let output = lower(
        r"
        impl<T> Vec<T> {
            fn user_method(v: Vec<T>) -> i32 {
                0
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
        Some("inherent impl on builtin nominal `Vec`"),
        "V0b must surface a precise shape diagnostic for inherent impls on \
         builtin nominals; got diagnostics: {:#?}",
        output.diagnostics,
    );
}

#[test]
fn declarative_ffi_inherent_impl_on_builtin_nominal_is_skipped_without_diagnostic() {
    // Declarative receiver FFI: an inherent impl on a builtin nominal whose
    // methods ALL carry `#[extern_symbol(...)]` is the registration source the
    // checker consumes via `register_compiled_stdlib_receiver_impls` (the
    // `impl<T> Option<T>` / `impl<T, E> Result<T, E>` blocks in
    // `std/option.hew` / `std/result.hew`). HIR must treat the block as
    // already-consumed metadata: no `ImplBlockShapeNotLowered` rejection, and
    // no `<SelfType>::<method>` functions emitted from the panic-stub bodies
    // (call sites dispatch through checker-recorded extern-symbol rewrites).
    let output = lower(
        r#"
        impl<T> Option<T> {
            #[extern_symbol(hew_option_is_some)]
            fn is_some(opt: Option<T>) -> bool {
                panic("declarative FFI body reached without rewrite")
            }
            #[extern_symbol("hew_option_unwrap_{T}")]
            fn unwrap(opt: Option<T>) -> T {
                panic("declarative FFI body reached without rewrite")
            }
        }
        "#,
    );

    let shape_diag = output
        .diagnostics
        .iter()
        .find(|d| matches!(&d.kind, HirDiagnosticKind::ImplBlockShapeNotLowered { .. }));
    assert!(
        shape_diag.is_none(),
        "declarative-FFI inherent impl on a builtin nominal must be admitted \
         (skipped), not rejected; got diagnostics: {:#?}",
        output.diagnostics,
    );
    let emitted_methods: Vec<&str> = output
        .module
        .items
        .iter()
        .filter_map(|item| match item {
            HirItem::Function(f) if f.name.starts_with("Option::") => Some(f.name.as_str()),
            _ => None,
        })
        .collect();
    assert!(
        emitted_methods.is_empty(),
        "declarative-FFI stub bodies must not be emitted as callable \
         `Option::<method>` functions; got: {emitted_methods:?}",
    );
}

#[test]
fn mixed_ffi_and_real_body_inherent_impl_on_builtin_nominal_stays_fail_closed() {
    // A builtin-nominal inherent impl containing ANY method without
    // `#[extern_symbol]` is not declarative FFI — it is a user-authored
    // inherent impl and keeps the fail-closed rejection.
    let output = lower(
        r#"
        impl<T> Option<T> {
            #[extern_symbol(hew_option_is_some)]
            fn is_some(opt: Option<T>) -> bool {
                panic("declarative FFI body reached without rewrite")
            }
            fn user_method(opt: Option<T>) -> i32 {
                0
            }
        }
        "#,
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
        Some("inherent impl on builtin nominal `Option`"),
        "mixed FFI/real-body inherent impls on builtin nominals must stay \
         fail-closed; got diagnostics: {:#?}",
        output.diagnostics,
    );
}
