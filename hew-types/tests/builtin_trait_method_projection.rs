//! Generic trait methods on builtin/primitive receivers must bind the impl's
//! type parameters from the concrete receiver's type arguments and project the
//! associated/return types eagerly — exactly as the user-record concrete
//! receiver path already does.
//!
//! Before the fix, `try_dispatch_primitive_trait_method` keyed on the
//! type-erased canonical key (`"Vec"`, element discarded) and returned the raw
//! signature's return type, leaving `Self::Output` / `Option<T>` returns as an
//! unresolved inference var that escaped to the checker output boundary
//! (`InferenceFailed`). The user-record (`Ty::Named`) path bound and projected
//! correctly, so the asymmetry only bit builtin receivers (Vec/HashMap/...).
//!
//! Each positive test co-asserts projection by feeding the projected element
//! into a type-constrained position (a `fn` that requires the concrete type) —
//! if projection failed, the value would be an unresolved var and the program
//! would not type-check. No external result-pinning (annotation / `unwrap_or`)
//! is used.

mod common;

use common::typecheck;
use hew_types::error::TypeErrorKind;

fn assert_clean(source: &str, label: &str) {
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "{label}: expected clean type-check, got errors: {:#?}",
        output.errors
    );
}

/// The exact reproducer: a generic trait impl on `Vec<T>` returning
/// `Option<Self::Output>`, dispatched on a concrete `Vec<i64>`, must project
/// `Option<i64>` with no external pinning. `takes_int(x)` pins the Some-arm
/// binder to `i64`, so a failed projection (unresolved var) would not check.
#[test]
fn vec_generic_trait_method_projects_output() {
    assert_clean(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        fn takes_int(x: i64) {}
        fn main() {
            let v: Vec<i64> = [10, 20, 30];
            match v.fetch(1) {
                Some(x) => takes_int(x),
                None => {}
            }
        }
        ",
        "vec<i64>.fetch projects Option<i64>",
    );
}

/// User-record control (`Box2<T>`) — the path that already worked — must keep
/// projecting, co-asserted in the same harness so a regression here is caught.
#[test]
fn user_record_control_still_projects() {
    assert_clean(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        type Box2<T> { inner: T; }
        impl<T> Acc for Box2<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { Some(self.inner) }
        }
        fn takes_int(x: i64) {}
        fn main() {
            let b = Box2 { inner: 42 };
            match b.fetch(0) {
                Some(x) => takes_int(x),
                None => {}
            }
        }
        ",
        "Box2<i64>.fetch projects Option<i64>",
    );
}

/// Broaden over builtin kind: a generic trait impl on `HashMap<K, V>` must bind
/// `V` from the concrete value type and project `Option<V>`.
#[test]
fn hashmap_generic_trait_method_projects_value() {
    assert_clean(
        r#"
        trait Lookup {
            type Output;
            fn grab(self, key: string) -> Option<Self::Output>;
        }
        impl<K, V> Lookup for HashMap<K, V> {
            type Output = V;
            fn grab(self, key: string) -> Option<V> { None }
        }
        fn takes_int(x: i64) {}
        fn main() {
            let m: HashMap<string, i64> = HashMap::new();
            match m.grab("a") {
                Some(x) => takes_int(x),
                None => {}
            }
        }
        "#,
        "HashMap<string,i64>.grab projects Option<i64>",
    );
}

/// Broaden over element class: string element. `Vec<string>.fetch` must project
/// `Option<string>`.
#[test]
fn vec_string_element_projects() {
    assert_clean(
        r#"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        fn takes_str(s: string) {}
        fn main() {
            let v: Vec<string> = ["a", "b"];
            match v.fetch(0) {
                Some(s) => takes_str(s),
                None => {}
            }
        }
        "#,
        "Vec<string>.fetch projects Option<string>",
    );
}

/// Broaden over element class: owned-record element. `Vec<Point>.fetch` must
/// project `Option<Point>` and the Some-arm binder must support field access.
#[test]
fn vec_owned_record_element_projects() {
    assert_clean(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        type Point { x: i64; y: i64; }
        fn takes_int(x: i64) {}
        fn main() {
            let pts: Vec<Point> = [Point { x: 1, y: 2 }];
            match pts.fetch(0) {
                Some(p) => takes_int(p.x),
                None => {}
            }
        }
        ",
        "Vec<Point>.fetch projects Option<Point>",
    );
}

/// Fail-closed: a genuinely-unresolvable element type (empty literal, no
/// annotation) must still emit a clean `InferenceFailed` diagnostic — the
/// projection must NOT fail open (accept a wrong/var shape) and must NOT leak a
/// `Ty::Var` past the checker boundary or panic.
#[test]
fn unresolvable_element_fails_closed() {
    let output = typecheck(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        fn main() {
            let v = [];
            let r = v.fetch(0);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::InferenceFailed),
        "expected a clean InferenceFailed diagnostic, got: {:#?}",
        output.errors
    );
}

/// Fail-closed (over-application): a concrete-`Self` builtin impl
/// (`impl Acc for Vec<i64>`) must NOT be selected for a non-matching receiver
/// (`Vec<string>`). Dispatch keys only on the canonical builtin (`Vec`), so the
/// instantiation must prove the impl's concrete `Self` arg (`i64`) matches the
/// receiver's element (`string`) — it does not, so this must fail closed with a
/// clean "no method" diagnostic, never project an authoritative `Option<i64>`.
#[test]
fn concrete_self_impl_not_overapplied_to_mismatched_element() {
    let output = typecheck(
        r#"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl Acc for Vec<i64> {
            type Output = i64;
            fn fetch(self, key: i64) -> Option<i64> { None }
        }
        fn main() {
            let v: Vec<string> = ["a"];
            let r = v.fetch(0);
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UndefinedMethod),
        "expected a no-method diagnostic (concrete impl must not over-apply), got: {:#?}",
        output.errors
    );
}

/// Fail-closed (constrained `Self`): `impl<V> Lookup for HashMap<string, V>`
/// must NOT be selected for `HashMap<i64, bool>` — the constrained key position
/// (`string`) does not match the receiver key (`i64`).
#[test]
fn constrained_key_impl_not_overapplied_to_mismatched_key() {
    let output = typecheck(
        r"
        trait Lookup {
            type Output;
            fn grab(self, key: i64) -> Option<Self::Output>;
        }
        impl<V> Lookup for HashMap<string, V> {
            type Output = V;
            fn grab(self, key: i64) -> Option<V> { None }
        }
        fn main() {
            let m: HashMap<i64, bool> = HashMap::new();
            let r = m.grab(0);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UndefinedMethod),
        "expected a no-method diagnostic (constrained key must not over-apply), got: {:#?}",
        output.errors
    );
}

/// Fail-closed (nested constructor mismatch): `impl<T> Acc for Vec<Vec<T>>` must
/// NOT bind `T` from `Vec<Option<i64>>` — the nested `Self` constructor (`Vec`)
/// differs from the receiver element constructor (`Option`), so the recursive
/// match must reject it rather than spuriously bind `T = i64`.
#[test]
fn nested_self_constructor_mismatch_does_not_bind() {
    let output = typecheck(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<Vec<T>> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        fn main() {
            let v: Vec<Option<i64>> = [Some(1)];
            let r = v.fetch(0);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UndefinedMethod),
        "expected a no-method diagnostic (nested ctor mismatch must not bind), got: {:#?}",
        output.errors
    );
}

/// Positive control for the shape-check: a concrete-`Self` builtin impl DOES
/// apply to its exact receiver (`impl Acc for Vec<i64>` on `Vec<i64>`), proving
/// the shape-check admits the matching case while rejecting the mismatched one.
#[test]
fn concrete_self_impl_projects_for_matching_receiver() {
    assert_clean(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl Acc for Vec<i64> {
            type Output = i64;
            fn fetch(self, key: i64) -> Option<i64> { None }
        }
        fn takes_int(x: i64) {}
        fn main() {
            let v: Vec<i64> = [1, 2];
            match v.fetch(0) {
                Some(x) => takes_int(x),
                None => {}
            }
        }
        ",
        "concrete impl Acc for Vec<i64> projects Option<i64> on Vec<i64>",
    );
}

/// Coherence (builtin): two impls of the same trait for the same builtin
/// constructor — a blanket `impl<T> Acc for Vec<T>` AND a concrete
/// `impl Acc for Vec<i64>` — must be rejected. Hew has no specialization or
/// overlapping impls (mission Q66.b; HEW-FUTURE §single-crate coherence), so a
/// trait may be implemented at most once per type constructor. Without this
/// rejection the dispatch side tables (self-args proof vs method signature)
/// could be populated from different impls and drift, re-opening the
/// over-application fail-open this fix closes.
#[test]
fn overlapping_builtin_impls_rejected() {
    let output = typecheck(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        impl Acc for Vec<i64> {
            type Output = i64;
            fn fetch(self, key: i64) -> Option<i64> { None }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::ConflictingTraitImpl { .. })),
        "expected a ConflictingTraitImpl coherence diagnostic for the overlapping \
         Vec impls, got: {:#?}",
        output.errors
    );
}

/// Coherence (builtin, same shape) — TRACKED KNOWN GAP.
///
/// Two genuinely-distinct `impl` blocks with the SAME `Self` shape — two
/// `impl<T> Acc for Vec<T>` — are a duplicate that SHOULD be rejected with
/// `ConflictingTraitImpl`, but currently are not: the coherence check compares
/// `Self` shape (shape-equal duplicates read as a re-presentation of one impl).
///
/// Closing this needs the impl's DEFINING identity as the coherence key, but the
/// only readily-available per-impl span cannot be used directly: the documented
/// user-redeclare path (a user `pub trait Display` shadowing the prelude
/// `Display`) makes the prelude `impl Display for i64` and the user
/// `impl Display for i64` — two DISTINCT traits — collapse to the same
/// `(canonical, "Display")` coherence key, so a defining-span key would falsely
/// reject that legal shadow. A correct fix must additionally key on the trait's
/// DEFINING identity (not its name), a larger cross-cutting change tracked as a
/// separate follow-up: "trait-impl coherence: reject duplicate same-(type,trait)
/// impls via defining-identity".
///
/// The projection fix this change delivers is unaffected by the gap: first-wins
/// keeps the dispatched method signature and the applicability proof from the
/// SAME (first) impl, so an accepted duplicate cannot cause the mis-projection
/// fail-open this fix closes. `#[ignore]` keeps the regression CI-visible
/// (`cargo test -- --ignored`) without failing the gate; remove `#[ignore]` when
/// the follow-up lands.
#[test]
#[ignore = "tracked: duplicate same-(type,trait) impl coherence needs defining-identity key (separate follow-up)"]
fn duplicate_same_shape_builtin_impls_rejected() {
    let output = typecheck(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        impl<T> Acc for Vec<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { None }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::ConflictingTraitImpl { .. })),
        "expected a ConflictingTraitImpl coherence diagnostic for the duplicate \
         same-shape Vec impls, got: {:#?}",
        output.errors
    );
}
/// user-defined record constructors — `impl<T> Acc for Box2<T>` AND
/// `impl Acc for Box2<i64>` must be rejected (fail-closed), never silently
/// accepted. User-record impls do NOT flow through the primitive/builtin side
/// table (where this change's drift fail-open lived), so they are rejected by the
/// pre-existing impl-coherence path rather than the new `ConflictingTraitImpl`
/// primitive diagnostic. Either way the overlap must not type-check cleanly.
#[test]
fn overlapping_user_record_impls_rejected() {
    let output = typecheck(
        r"
        trait Acc {
            type Output;
            fn fetch(self, key: i64) -> Option<Self::Output>;
        }
        type Box2<T> { inner: T; }
        impl<T> Acc for Box2<T> {
            type Output = T;
            fn fetch(self, key: i64) -> Option<T> { Some(self.inner) }
        }
        impl Acc for Box2<i64> {
            type Output = i64;
            fn fetch(self, key: i64) -> Option<i64> { Some(self.inner) }
        }
        fn main() {}
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "expected the overlapping Box2 impls to be rejected (fail-closed), \
         but type-check was clean",
    );
}
