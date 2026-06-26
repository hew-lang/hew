#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── Vec index auto-widening tests ────────────────────────────────────────────
//
// A372 / A373: signed integers narrower than i64 are accepted as Vec index
// arguments (`.get`, `.set`, `.remove`) and as `xs[i]` index expressions.
// The widening is operand-only: the Vec element return type is NOT changed.
// Non-integer and unsigned-integer indices remain rejected.

#[test]
fn vec_get_accepts_i32_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(10);
            let i: i32 = 0;
            let _v = xs.get(i);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Vec::get with i32 index should be accepted without error, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_set_accepts_i32_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(0);
            let i: i32 = 0;
            xs.set(i, 99);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Vec::set with i32 index should be accepted without error, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_index_accepts_i32_index() {
    let output = check_source(
        r"
        fn main() -> i64 {
            let xs: Vec<i64> = Vec::new();
            xs.push(42);
            let i: i32 = 0;
            xs[i]
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "xs[i32] index expression should be accepted without error, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_index_rejects_string_index() {
    let output = check_source(
        r#"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            let s = "hello";
            let _v = xs.get(s);
        }
        "#,
    );
    assert!(
        !output.errors.is_empty(),
        "Vec::get with a string index must be rejected"
    );
}

#[test]
fn vec_index_rejects_bool_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            let flag = true;
            let _v = xs.get(flag);
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "Vec::get with a bool index must be rejected"
    );
}

#[test]
fn vec_index_rejects_unsigned_i32_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            let u: u32 = 0;
            let _v = xs.get(u);
        }
        ",
    );
    // u32 is not a narrower *signed* integer — unsigned indices are not
    // auto-widened because the sign-extension semantics differ.
    assert!(
        !output.errors.is_empty(),
        "Vec::get with a u32 index must be rejected (unsigned is not auto-widened)"
    );
}

/// the boundary does: a concrete type lands in both maps; a non-concrete type
/// (a leaked inference var) lands only in `expr_types`, leaving the typed map
/// correctly absent (fail-closed omission, never a fabricated guess).
#[test]
fn insert_expr_type_mirrors_boundary_concreteness_split() {
    let mut out = TypeCheckOutput::default();
    let concrete = SpanKey {
        start: 0,
        end: 4,
        module_idx: 0,
    };
    let leaked = SpanKey {
        start: 4,
        end: 8,
        module_idx: 0,
    };

    out.insert_expr_type(concrete.clone(), Ty::I64);
    out.insert_expr_type(leaked.clone(), Ty::Var(TypeVar::fresh()));

    assert_eq!(out.expr_types.get(&concrete), Some(&Ty::I64));
    assert_eq!(
        out.resolved_expr_types.get(&concrete),
        Some(&ResolvedTy::from_ty(&Ty::I64).unwrap()),
        "concrete type must populate the typed handoff map"
    );

    assert!(
        out.expr_types.contains_key(&leaked),
        "leaked inference var still recorded in the Ty side-table"
    );
    assert!(
        !out.resolved_expr_types.contains_key(&leaked),
        "leaked inference var must be absent from the typed handoff map"
    );
}

// --- Generic swap regression (G4b-bug-1) ---
//
// A return type that PERMUTES the type params of a generic record triggered a
// false "type mismatch" because the sequential per-param substitution loop
// aliased the two params: applying A→B then B→A over the same accumulator
// turned both fields back to A. Parallel substitution fixes this.

#[test]
fn generic_swap_return_type_typechecks_without_false_mismatch() {
    // fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> { Pair { first: p.second, second: p.first } }
    // Before fix: "type mismatch: expected `B`, found `A`" on `p.first` (the
    // `second` field of the return, which expects type A after swapping).
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> {
    Pair<B, A> { first: p.second, second: p.first }
}

fn main() {
    let p = Pair { first: 1, second: 2 };
    let _s = swap(p);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "generic swap must type-check without errors; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_swap_heterogeneous_params_typechecks() {
    // Heterogeneous concrete args (i64 / bool) expose the aliasing bug most
    // clearly: before the fix the second field got expected type `A` instead of
    // `i64`, and the checker emitted "expected `A`, found `i64`".
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> {
    Pair<B, A> { first: p.second, second: p.first }
}

fn main() {
    let p = Pair { first: 1, second: true };
    let _s = swap(p);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "heterogeneous generic swap must type-check; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_identity_pair_homogeneous_still_typechecks() {
    // Confirm that the non-swapping (homogeneous) case was not broken by the
    // parallel-substitution change.
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn fst<A, B>(p: Pair<A, B>) -> A { p.first }

fn main() {
    let p = Pair { first: 10, second: 3 };
    let _x = fst(p);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "identity (non-swap) generic must still type-check; got: {:#?}",
        output.errors
    );
}

// --- expressions.rs field-access swap regression (G4b-bug-2) ---
//
// `expressions.rs` resolves `receiver.field` for a generic type by zipping
// `td.type_params` with the instantiation `args` and substituting the field's
// declared type.  When the args are themselves abstract named params in swapped
// order (e.g. `Pair<B, A>` inside a function generic over `<A, B>`), sequential
// substitution aliases both params: A→B then B→A maps `A` back to `A`, so
// `p.first` in `fn f<A,B>(p: Pair<B,A>) -> B` yielded type `A` instead of `B`.
// Parallel substitution reads the original field type once and maps all params
// simultaneously.

#[test]
fn generic_field_access_on_swapped_instantiation_typechecks() {
    // fn get_first_of_swapped<A, B>(p: Pair<B, A>) -> B { p.first }
    // Before fix: "type mismatch: expected `B`, found `A`" on `p.first`.
    // Field `first: A` in `Pair<A, B>`, instantiated with args [B, A]:
    // sequential A→B then B→A gives A again; parallel gives B.
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn get_first_of_swapped<A, B>(p: Pair<B, A>) -> B {
    p.first
}
",
    );
    assert!(
        output.errors.is_empty(),
        "field access on swapped generic instantiation must type-check; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_field_access_second_on_swapped_instantiation_typechecks() {
    // fn get_second_of_swapped<A, B>(p: Pair<B, A>) -> A { p.second }
    // Companion: field `second: B`, instantiated with args [B, A].
    // Sequential B→B (no-op for param A), then B→A gives A. Actually fine
    // in the single-param substitution — but the combined swap test below
    // exercises both fields in the same function to catch any residual alias.
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn get_second_of_swapped<A, B>(p: Pair<B, A>) -> A {
    p.second
}
",
    );
    assert!(
        output.errors.is_empty(),
        "second-field access on swapped generic instantiation must type-check; got: {:#?}",
        output.errors
    );
}

// --- generics.rs trait-object-bound swap regression (G4b-bug-3) ---
//
// `apply_trait_object_bound_substitutions` (generics.rs) substitutes the
// trait's declared type params into the method signature using the concrete
// args from the `dyn Trait<...>` bound.  When a generic function is
// parameterised `<A, B>` and accepts `dyn Mapper<B, A>` (swapped), the
// bound.args are [Named("B"), Named("A")] while type_params are ["A", "B"].
// Sequential substitution: A→B over the method sig, then B→A aliases the
// just-renamed B back to A — so `val: A` is expected for the arg instead
// of `val: B`.  Parallel substitution fixes the alias.

#[test]
fn dyn_trait_two_params_swapped_bound_typechecks() {
    // trait Mapper<A, B> { fn map(Self, A) -> B }
    // fn apply_swapped<A, B>(f: dyn Mapper<B, A>, val: B) -> A { f.map(val) }
    // Before fix: "type mismatch: expected `A`, found `B`" on `val`.
    let output = check_source(
        r"
trait Mapper<A, B> {
    fn map(val: A) -> B;
}

fn apply_swapped<A, B>(f: dyn Mapper<B, A>, val: B) -> A {
    f.map(val)
}
",
    );
    assert!(
        output.errors.is_empty(),
        "dyn Mapper<B,A> call must type-check under swapped bound; got: {:#?}",
        output.errors
    );
}

// --- registration.rs rename_method_type_params swap regression (G4b-bug-4) ---
//
// `rename_method_type_params` (registration.rs) renames the trait method's
// declared type params to match the impl method's names before comparing
// signatures.  When the impl reverses the trait's param order
// (`trait Discard<T,U>` / `impl fn discard<U,T>`), sequential application
// renames T→U then U→T — mapping both back to T.  The impl's first param
// `U` therefore fails to match the now-aliased expected `T` even though the
// signature is structurally equivalent.  Parallel rename resolves the alias.

#[test]
fn impl_method_swapped_type_param_names_accepted() {
    // trait Discard { fn discard<T, U>(a: T, b: U) -> i64; }
    // impl Discard for Discarter { fn discard<U, T>(a: U, b: T) -> i64 { 0 } }
    // Before fix: false "parameter `a` has type `U` but trait requires `T`".
    let output = check_source(
        r"
trait Discard {
    fn discard<T, U>(a: T, b: U) -> i64;
}

type Discarter { }

impl Discard for Discarter {
    fn discard<U, T>(a: U, b: T) -> i64 { 0 }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "impl with swapped method type-param names must satisfy trait; got: {:#?}",
        output.errors
    );
}

// ── lambda annotated-param vs expected-fn-type unification (L29) ─────────────
//
// When a lambda has an explicit param annotation (`|x: T|`) and is passed
// where a `fn(U) -> V` is expected, the checker must unify `T` against `U`.
// Previously the unification was only attempted when the annotation contained
// a `_` hole — fully-concrete annotations (no holes) were silently accepted
// regardless of the expected type. Ditto for annotated return types.

/// A fully-annotated param type that contradicts the expected fn type must be
/// rejected at the call-site with a Mismatch error.
#[test]
fn annotated_lambda_param_type_contradicts_expected_fn_type_is_rejected() {
    // `apply` expects `fn(bool) -> i64`; the lambda says `|x: i64|`.
    let output = check_source(
        r"
fn apply(f: fn(bool) -> i64) -> i64 { f(true) }

fn main() -> i64 {
    apply(|x: i64| x + 1)
}
",
    );
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                    if expected.contains("bool") && actual.contains("i64")
                        || expected.contains("i64") && actual.contains("bool")
            )
        }),
        "expected Mismatch error for bool/i64 param annotation conflict, got: {:#?}",
        output.errors
    );
}

/// A fully-annotated return type that contradicts the expected fn type must be
/// rejected.
#[test]
fn annotated_lambda_return_type_contradicts_expected_fn_type_is_rejected() {
    // `apply` expects `fn(i64) -> bool`; the lambda says `-> i64`.
    let output = check_source(
        r"
fn apply(f: fn(i64) -> bool) -> bool { f(1) }

fn main() -> bool {
    apply(|x: i64| -> i64 { x + 1 })
}
",
    );
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                    if expected.contains("bool") && actual.contains("i64")
                        || expected.contains("i64") && actual.contains("bool")
            )
        }),
        "expected Mismatch error for bool/i64 return annotation conflict, got: {:#?}",
        output.errors
    );
}

/// Arity mismatch: a lambda with more params than the expected fn type is
/// already rejected; verify this is still true (regression pin).
#[test]
fn lambda_arity_mismatch_against_expected_fn_type_is_rejected() {
    let output = check_source(
        r"
fn apply(f: fn(i64) -> i64) -> i64 { f(1) }

fn main() -> i64 {
    apply(|x: i64, y: i64| x + y)
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch),
        "expected ArityMismatch for lambda with extra param, got: {:#?}",
        output.errors
    );
}

/// Unannotated lambda (inference path) must still be accepted when the body
/// is compatible with the expected param type.
#[test]
fn unannotated_lambda_matching_expected_fn_type_is_accepted() {
    let output = check_source(
        r"
fn apply(f: fn(bool) -> i64) -> i64 { f(true) }

fn main() -> i64 {
    apply(|x| if x { 1 } else { 0 })
}
",
    );
    assert!(
        output.errors.is_empty(),
        "unannotated lambda compatible with expected fn type must pass; got: {:#?}",
        output.errors
    );
}

/// Correctly-annotated lambda (annotation matches expected type) is accepted.
#[test]
fn correctly_annotated_lambda_matching_expected_fn_type_is_accepted() {
    let output = check_source(
        r"
fn apply(f: fn(bool) -> i64) -> i64 { f(true) }

fn main() -> i64 {
    apply(|x: bool| if x { 1 } else { 0 })
}
",
    );
    assert!(
        output.errors.is_empty(),
        "correctly annotated lambda must be accepted; got: {:#?}",
        output.errors
    );
}

/// Nested fn type: param is itself a function type; verify annotation mismatch
/// is caught when the outer expected type and annotation disagree on the
/// inner function type.
#[test]
fn annotated_lambda_param_nested_fn_type_contradiction_is_rejected() {
    // Expected: `fn(fn(i64) -> i64) -> i64`
    // Lambda annotation: `fn(fn(bool) -> i64) -> i64` (inner param type wrong)
    let output = check_source(
        r"
fn apply(f: fn(fn(i64) -> i64) -> i64) -> i64 { f(|x| x + 1) }

fn main() -> i64 {
    apply(|g: fn(bool) -> i64| g(true))
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::Mismatch { .. })),
        "nested fn type annotation mismatch must be rejected; got: {:#?}",
        output.errors
    );
}
