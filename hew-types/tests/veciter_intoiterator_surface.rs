//! Stage 3 of the iteration-foundation lane: pin the concrete `VecIter<T>`
//! record + `impl Iterator for VecIter<T>` + `impl IntoIterator for Vec<T>`
//! surfaces shipped in `std/builtins.hew`.
//!
//! Stage 1 locked the trait shapes; this stage anchors the first concrete
//! `IntoIterator` impl on a builtin nominal (`Vec`) and verifies that the
//! checker accepts the `IntoIter = VecIter<T>` projection on both element
//! types in the v0.5 stdlib catalog (`Vec<i32>` and `Vec<i64>`).
//!
//! Q004 caveat: the checker does not yet enforce that an `impl` method's
//! return type matches the trait's declared return — these tests therefore
//! anchor parse-time and trait-resolution invariants, not method-signature
//! shape checks. A separate lane owns Q004.

#![allow(
    clippy::missing_panics_doc,
    reason = "integration-test panics on assertion failure are intentional"
)]

mod common;

use common::{checker, parse_program};
use hew_parser::ast::Item;

fn read_stdlib_builtins() -> String {
    let mut path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop();
    path.push("std/builtins.hew");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read std/builtins.hew at {}: {e}", path.display()))
}

#[test]
fn stdlib_vec_declares_veciter_record() {
    let src = read_stdlib_builtins();
    let program = parse_program(&src);
    let record = program.items.iter().find_map(|(item, _)| match item {
        Item::TypeDecl(td) if td.name == "VecIter" => Some(td),
        _ => None,
    });
    assert!(
        record.is_some(),
        "std/builtins.hew must declare `VecIter` as a record (pub type) — got items: {:#?}",
        program
            .items
            .iter()
            .map(|(i, _)| match i {
                Item::TypeDecl(t) => format!("type {}", t.name),
                Item::Function(f) => format!("fn {}", f.name),
                Item::Impl(b) => format!("impl {:?}", b.trait_bound),
                _ => "<other>".to_string(),
            })
            .collect::<Vec<_>>()
    );
}

#[test]
fn stdlib_vec_declares_iterator_impl_for_veciter() {
    let src = read_stdlib_builtins();
    let program = parse_program(&src);
    let has_iter_impl = program.items.iter().any(|(item, _)| match item {
        Item::Impl(b) => {
            b.trait_bound
                .as_ref()
                .is_some_and(|tb| tb.name == "Iterator")
                && matches!(
                    &b.target_type.0,
                    hew_parser::ast::TypeExpr::Named { name, .. } if name == "VecIter"
                )
        }
        _ => false,
    });
    assert!(
        has_iter_impl,
        "std/builtins.hew must declare `impl<T> Iterator for VecIter<T>`"
    );
}

#[test]
fn stdlib_vec_declares_intoiterator_impl_for_vec() {
    let src = read_stdlib_builtins();
    let program = parse_program(&src);
    let has_into_impl = program.items.iter().any(|(item, _)| match item {
        Item::Impl(b) => {
            b.trait_bound
                .as_ref()
                .is_some_and(|tb| tb.name == "IntoIterator")
                && matches!(
                    &b.target_type.0,
                    hew_parser::ast::TypeExpr::Named { name, .. } if name == "Vec"
                )
        }
        _ => false,
    });
    assert!(
        has_into_impl,
        "std/builtins.hew must declare `impl<T> IntoIterator for Vec<T>`"
    );
}

#[test]
fn stdlib_vec_intoiterator_impl_binds_intoiter_assoc_to_veciter() {
    // Lock the `type IntoIter = VecIter<T>` projection — this is what callers
    // depending on `<Vec<T> as IntoIterator>::IntoIter::Item == T` rely on
    // (Stage 4 for-loop desugar; Stage 2 lazy combinators).
    let src = read_stdlib_builtins();
    let program = parse_program(&src);
    let impl_block = program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Impl(b)
                if b.trait_bound
                    .as_ref()
                    .is_some_and(|tb| tb.name == "IntoIterator")
                    && matches!(
                        &b.target_type.0,
                        hew_parser::ast::TypeExpr::Named { name, .. } if name == "Vec"
                    ) =>
            {
                Some(b)
            }
            _ => None,
        })
        .expect("impl IntoIterator for Vec<T> must exist");

    let mut saw_item = false;
    let mut saw_into_iter = false;
    for assoc in &impl_block.type_aliases {
        if assoc.name == "Item" {
            saw_item = true;
        }
        if assoc.name == "IntoIter" {
            saw_into_iter = true;
            // Confirm the binding is `VecIter<...>`.
            let is_veciter = matches!(
                &assoc.ty.0,
                hew_parser::ast::TypeExpr::Named { name, .. } if name == "VecIter"
            );
            assert!(
                is_veciter,
                "IntoIter assoc binding must be VecIter<T>, got {:#?}",
                assoc.ty
            );
        }
    }
    assert!(saw_item, "impl IntoIterator for Vec must bind `Item`");
    assert!(
        saw_into_iter,
        "impl IntoIterator for Vec must bind `IntoIter`"
    );
}

#[test]
fn stdlib_vec_typechecks_with_iterator_traits_registered() {
    // Whole-stdlib smoke through the live checker (with the std module path
    // available): confirms VecIter + impls register without disturbing other
    // stdlib surfaces. Pre-existing checker reds on unrelated builtins are
    // tolerated per the trait-surface lock-test precedent.
    let src = read_stdlib_builtins();
    let program = parse_program(&src);
    let mut checker = checker();
    let output = checker.check_program(&program);
    // We only assert that no diagnostic mentions VecIter or our new impls —
    // any other pre-existing red is out of scope for this stage.
    let veciter_red = output.errors.iter().find(|e| e.message.contains("VecIter"));
    assert!(
        veciter_red.is_none(),
        "VecIter surface must type-check cleanly; got: {veciter_red:#?}"
    );
}

#[test]
fn vec_i32_into_iter_resolves_through_trait_surface() {
    // Synthesise a minimal program that calls `.into_iter()` on `Vec<i32>` and
    // confirm the checker resolves it through the `IntoIterator` impl shipped
    // in `std/builtins.hew`. This is the concrete-instantiation arm of the Stage 3
    // contract ("works for at least Vec<i32> and Vec<i64>"; worker task).
    let src = r"
        fn main() {
            let v: Vec<i32> = Vec::new();
            let _it = v.into_iter();
        }
    ";
    let program = parse_program(src);
    let mut checker = checker();
    let output = checker.check_program(&program);
    let into_iter_red = output
        .errors
        .iter()
        .find(|e| e.message.contains("into_iter"));
    assert!(
        into_iter_red.is_none(),
        "Vec<i32>.into_iter() must resolve through IntoIterator; got: {into_iter_red:#?}\nall errors: {:#?}",
        output.errors
    );
}

#[test]
fn vec_i64_into_iter_resolves_through_trait_surface() {
    // Sibling of the i32 case — pins the other element type the v0.5 stdlib
    // catalog speaks (`hew_vec_get_i64`).
    let src = r"
        fn main() {
            let v: Vec<i64> = Vec::new();
            let _it = v.into_iter();
        }
    ";
    let program = parse_program(src);
    let mut checker = checker();
    let output = checker.check_program(&program);
    let into_iter_red = output
        .errors
        .iter()
        .find(|e| e.message.contains("into_iter"));
    assert!(
        into_iter_red.is_none(),
        "Vec<i64>.into_iter() must resolve through IntoIterator; got: {into_iter_red:#?}\nall errors: {:#?}",
        output.errors
    );
}
