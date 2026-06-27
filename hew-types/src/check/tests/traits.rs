#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn structural_satisfies_returns_false_for_unknown_trait() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    assert!(
        !checker.type_structurally_satisfies("MyType", "NoSuchTrait"),
        "unknown trait must not satisfy structural check"
    );
}

#[test]
fn structural_satisfies_e1_guard_associated_types() {
    let mut checker = make_checker_with_trait("Indexed", &["get"], true, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Indexed"),
        "E1 guard: traits with associated types must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_generic_methods() {
    let mut checker = make_checker_with_trait("Mapper", &["map"], false, true);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Mapper"),
        "E1 guard: traits with generic methods must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_method_only_trait_unknown_type_returns_false() {
    // In E2, the placeholder is replaced with real method-presence matching.
    // An unregistered type still returns false because no methods are found.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Greet"),
        "unregistered type must not satisfy structural check even after E2"
    );
}

#[test]
fn type_satisfies_trait_bound_nominal_path_unchanged() {
    // Verify that existing nominal conformance still works after the
    // structural fallback was wired into type_satisfies_trait_bound.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Greeter {}

        impl Greet for Greeter {
            fn hello(val: Greeter) {}
        }

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let g = Greeter {};
            use_greet(g);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nominal trait conformance must still succeed after E1 scaffold: {:?}",
        output.errors
    );
}

#[test]
fn type_satisfies_trait_bound_missing_impl_still_fails() {
    // A type that has no impl and no structural match must still fail the
    // bound — E1 must not silently accept it.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Stranger {}

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let s = Stranger {};
            use_greet(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "E1 must not accept a type with no impl and no structural match; expected errors"
    );
}

// -------------------------------------------------------------------------
// Primitive bound-satisfiability tests
//
// Guard the path where a primitive type (i64, f64, bool, string) satisfies a
// user-defined trait bound via an explicit `impl Trait for <primitive>`.
// Before the fix, the `_` arm in `type_satisfies_trait_bound` only consulted
// the `MarkerTrait` table and never checked `trait_impls_set`, causing all
// user-trait bounds on primitives to be falsely rejected.
// -------------------------------------------------------------------------

#[test]
fn primitive_i64_with_impl_satisfies_user_trait_bound() {
    // Positive: an explicit `impl Show for i64` must allow `i64` to satisfy
    // the `T: Show` bound on a generic function.
    let source = r#"
        trait Show {
            fn show(val: Self) -> string;
        }

        impl Show for i64 {
            fn show(val: i64) -> string { "i64" }
        }

        fn display<T: Show>(x: T) -> string {
            x.show()
        }

        fn main() -> i64 {
            let s = display(5);
            if s == "i64" { 0 } else { 1 }
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        bound_errors.is_empty(),
        "i64 with explicit impl must satisfy user-trait bound; got: {bound_errors:?}"
    );
}

#[test]
fn primitive_f64_bool_string_with_impl_satisfy_user_trait_bound() {
    // Positive: f64, bool, and string each satisfy the bound when an impl exists.
    let source = r#"
        trait Label {
            fn label(val: Self) -> string;
        }

        impl Label for f64    { fn label(val: f64)    -> string { "f64"    } }
        impl Label for bool   { fn label(val: bool)   -> string { "bool"   } }
        impl Label for string { fn label(val: string) -> string { "string" } }

        fn tag<T: Label>(x: T) -> string {
            x.label()
        }

        fn main() -> i64 {
            let a = tag(1.5);
            let b = tag(true);
            let c = tag("hi");
            if a == "f64" && b == "bool" && c == "string" { 0 } else { 1 }
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        bound_errors.is_empty(),
        "f64/bool/string with explicit impl must satisfy user-trait bound; got: {bound_errors:?}"
    );
}

#[test]
fn primitive_without_impl_still_rejected_for_user_trait_bound() {
    // Negative: a primitive with no `impl Trait for <primitive>` must still
    // produce a `BoundsNotSatisfied` error — the fix must not weaken the gate.
    let source = r"
        trait Show {
            fn show(val: Self) -> string;
        }

        fn display<T: Show>(x: T) -> string {
            x.show()
        }

        fn main() {
            let _ = display(5);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        !bound_errors.is_empty(),
        "primitive without a matching impl must still be rejected with BoundsNotSatisfied"
    );
}

#[test]
fn marker_trait_bounds_on_primitives_unaffected() {
    // Regression: built-in marker traits (e.g. `Ord`) on primitives must
    // still be satisfied via the `MarkerTrait` table, not just through
    // `trait_impls_set`.  The fix adds a pre-check for user impls but must
    // not remove or skip the `MarkerTrait` fallback.
    let source = r"
        fn max_val<T: Ord>(a: T, b: T) -> T {
            if a > b { a } else { b }
        }

        fn main() -> i64 {
            max_val(3, 7)
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        bound_errors.is_empty(),
        "built-in Ord marker on i64 must still satisfy bound after primitive-impl fix; got: {bound_errors:?}"
    );
}

#[test]
fn partial_ord_marker_bounds_accept_i64_and_f64() {
    let source = r"
        fn smaller<T: PartialOrd>(a: T, b: T) -> T {
            if a < b { a } else { b }
        }

        fn main() -> i64 {
            let i = smaller(3, 7);
            let f = smaller(3.5, 2.0);
            if i == 3 && f > 2.0 { 0 } else { 1 }
        }
    ";

    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "PartialOrd marker bound should accept i64 and f64 comparisons; got: {:?}",
        output.errors
    );
}

#[test]
fn unknown_trait_bound_reports_unknown_trait_not_missing_impl() {
    let source = r"
        fn passthrough<T: Mystery>(x: T) -> T { x }

        fn main() {
            let _ = passthrough(5);
        }
    ";

    let output = check_source(source);
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::UndefinedType && e.message.contains("unknown trait `Mystery`")
        }),
        "unknown trait bound should report an unknown trait diagnostic; got: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("does not implement trait `Mystery`")),
        "unknown trait bound must not be reported as a missing impl; got: {:?}",
        output.errors
    );
}

#[test]
fn unbounded_generic_ordering_requires_partial_ord_bound() {
    let source = r"
        fn smaller<T>(a: T, b: T) -> T {
            if a < b { a } else { b }
        }

        fn main() -> i64 {
            smaller(3, 7)
        }
    ";

    let output = check_source(source);
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("requires type parameter `T`")
                && e.message.contains("`PartialOrd`")
                && e.span.start < e.span.end
        }),
        "unbounded generic ordering should report a spanned PartialOrd-bound error; got: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("IntCmp lhs is not an integer")),
        "generic ordering must not leak the backend IntCmp diagnostic; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// E2 structural method-presence tests
//
// These exercises the live structural-satisfaction logic that replaced the
// E1 placeholder.  Programs use `impl Type { fn method }` (no trait) so
// the method is registered in `type_defs.methods` without a nominal impl.
// -------------------------------------------------------------------------

#[test]
fn structural_e2_single_method_match_satisfies_bound() {
    // Positive: a type that has the required method via a bare impl block
    // (no explicit `impl Trait for Type`) must satisfy the bound structurally.
    let source = r"
        trait Area {
            fn area(val: Self) -> i64;
        }

        type Square {}

        impl Square {
            fn area(s: Square) -> i64 { 1 }
        }

        fn measure<T: Area>(s: T) -> i64 {
            s.area()
        }

        fn main() {
            let sq = Square {};
            let _ = measure(sq);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "structural method match must satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_multi_method_trait_all_present_satisfies_bound() {
    // Positive: all required methods present → bound satisfied.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> i64;
        }

        type Widget {}

        impl Widget {
            fn label(w: Widget) -> string { "w" }
            fn code(w: Widget) -> i64 { 0 }
        }

        fn print_label<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let w = Widget {};
            let _ = print_label(w);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "all methods present → Named bound must be satisfied; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_method_with_non_self_param_satisfies_bound() {
    // Positive: trait method has a non-receiver parameter; the type's method
    // must have the same arity and parameter type.
    let source = r"
        trait Scalable {
            fn scale(val: Self, factor: i64) -> i64;
        }

        type Brick {}

        impl Brick {
            fn scale(b: Brick, factor: i64) -> i64 { factor }
        }

        fn resize<T: Scalable>(t: T) -> i64 {
            t.scale(2)
        }

        fn main() {
            let b = Brick {};
            let _ = resize(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "matching non-receiver param must satisfy Scalable bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_nominal_impl_still_preferred_over_structural() {
    // Positive: an explicit `impl Trait for Type` still works; E2 must not
    // break the nominal path.
    let source = r"
        trait Area {
            fn area(val: Self) -> i64;
        }

        type Circle {}

        impl Area for Circle {
            fn area(c: Circle) -> i64 { 3 }
        }

        fn measure<T: Area>(s: T) -> i64 {
            s.area()
        }

        fn main() {
            let c = Circle {};
            let _ = measure(c);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "explicit impl must still satisfy bound in E2; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_return_type_does_not_satisfy_bound() {
    // Negative: the type has a method with the right name but wrong return type;
    // the bound must not be satisfied.
    let source = r#"
        trait Area {
            fn area(val: Self) -> i64;
        }

        type Triangle {}

        impl Triangle {
            fn area(t: Triangle) -> string { "big" }
        }

        fn measure<T: Area>(s: T) -> i64 {
            s.area()
        }

        fn main() {
            let t = Triangle {};
            let _ = measure(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "wrong return type must not satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_arity_does_not_satisfy_bound() {
    // Negative: the type's method has one extra non-receiver parameter;
    // the arity mismatch must cause the bound to fail.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> i64;
        }

        type Server {}

        impl Server {
            fn ping(s: Server, timeout: i64) -> i64 { 1 }
        }

        fn use_ping<T: Ping>(t: T) -> i64 {
            t.ping()
        }

        fn main() {
            let s = Server {};
            let _ = use_ping(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "arity mismatch must not satisfy Ping bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_missing_one_of_two_methods_does_not_satisfy_bound() {
    // Negative: a multi-method trait where only one of two required methods is present.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> i64;
        }

        type Partial {}

        impl Partial {
            fn label(p: Partial) -> string { "p" }
            // `code` is intentionally missing
        }

        fn use_named<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let p = Partial {};
            let _ = use_named(p);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "partial method set must not satisfy Named bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_all_default_methods_still_requires_explicit_impl() {
    // Negative (conservative): a trait whose every method has a default body
    // has no required methods.  E2 returns false in that case — an explicit
    // `impl Trait for Type` is still needed, keeping explicit impls authoritative
    // for default-only and marker-like traits.
    let source = r#"
        trait WithDefault {
            fn greet(val: Self) -> string { "hello" }
        }

        type Thingy {}

        impl Thingy {
            fn greet(t: Thingy) -> string { "world" }
        }

        fn use_it<T: WithDefault>(t: T) {}

        fn main() {
            let t = Thingy {};
            use_it(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "all-default-method trait must require explicit impl; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Bound-diagnostic clarity tests (v0.3.0 slice: bound-diagnostic-clarity)
//
// These tests verify that BoundsNotSatisfied errors carry a diagnostic hint
// (in `suggestions`) that distinguishes the concrete failure mode:
//   • missing method(s)
//   • arity mismatch
//   • return-type / signature mismatch
//   • E1 guard requiring an explicit `impl` declaration
// -------------------------------------------------------------------------

#[test]
fn bound_diagnostic_missing_method_hint() {
    // A type that has no method at all should produce a hint naming the missing method.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> i64;
        }

        type Widget {}

        fn use_ping<T: Ping>(t: T) -> i64 { 0 }

        fn main() {
            let w = Widget {};
            let _ = use_ping(w);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("ping") && s.contains("missing")),
        "suggestion should mention missing method `ping`; got suggestions: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_arity_mismatch_hint() {
    // A type whose method has the right name but the wrong number of parameters
    // should produce a hint mentioning the arity mismatch.
    let source = r"
        trait Measure {
            fn measure(val: Self) -> i64;
        }

        type Ruler {}

        impl Ruler {
            fn measure(r: Ruler, scale: i64) -> i64 { scale }
        }

        fn use_measure<T: Measure>(t: T) -> i64 { 0 }

        fn main() {
            let r = Ruler {};
            let _ = use_measure(r);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions.iter().any(|s| s.contains("arity")),
        "suggestion should mention arity mismatch; got: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_return_type_mismatch_hint() {
    // A type whose method has the right name and arity but returns the wrong type
    // should produce a hint mentioning the return-type mismatch.
    let source = r#"
        trait Label {
            fn label(val: Self) -> string;
        }

        type Tag {}

        impl Tag {
            fn label(t: Tag) -> i64 { 0 }
        }

        fn use_label<T: Label>(t: T) -> string { "" }

        fn main() {
            let tag = Tag {};
            let _ = use_label(tag);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions.iter().any(|s| s.contains("return-type")),
        "suggestion should mention return-type mismatch; got: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_e1_associated_type_requires_explicit_impl_hint() {
    // A trait that declares an associated type triggers the E1 guard.
    // The diagnostic hint should tell the user an explicit impl is needed.
    let source = r"
        trait Container {
            type Item;
            fn get(val: Self) -> i64;
        }

        type Box {}

        fn use_container<T: Container>(t: T) -> i64 { 0 }

        fn main() {
            let b = Box {};
            let _ = use_container(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("explicit") && s.contains("impl")),
        "suggestion should mention explicit impl for E1 (associated type) guard; got: {:?}",
        err.suggestions
    );
}

#[test]
fn trait_method_where_clause_bound_enforced_negative() {
    let source = r#"
        trait Printable {
            fn print(val: Self) -> string;
        }

        trait Formatter {
            fn apply<U>(item: Self, value: U) -> string where U: Printable;
        }

        type Printer {}
        type Page {}
        type Rock {}

        impl Printable for Page {
            fn print(val: Page) -> string { "page" }
        }

        impl Formatter for Printer {
            fn apply<U>(item: Printer, value: U) -> string where U: Printable {
                "formatted"
            }
        }

        fn run<T: Formatter>(item: T) {
            let _ = item.apply(Rock {});
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.message.contains("Printable") && err.message.contains('U'),
        "expected bound error to mention the method-level bound, got {err:?}"
    );
}

#[test]
fn trait_method_where_clause_bound_enforced_positive() {
    let source = r#"
        trait Printable {
            fn print(val: Self) -> string;
        }

        trait Formatter {
            fn apply<U>(item: Self, value: U) -> string where U: Printable;
        }

        type Printer {}
        type Page {}

        impl Printable for Page {
            fn print(val: Page) -> string { "page" }
        }

        impl Formatter for Printer {
            fn apply<U>(item: Printer, value: U) -> string where U: Printable {
                "formatted"
            }
        }

        fn run<T: Formatter>(item: T) {
            let ok = item.apply(Page {});
            println(ok);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "expected method-level where-clause bound to be satisfied, got {:?}",
        output.errors
    );
    assert!(
        output.call_type_args.values().any(|args| args
            == &vec![crate::ty::Ty::Named {
                builtin: None,
                name: "Page".to_string(),
                args: vec![]
            }]),
        "expected method-level bound call to infer U=Page, got {:?}",
        output.call_type_args
    );
}

#[test]
fn named_method_lookup_prefers_type_defs_before_fn_sigs() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "hello".to_string(),
        FnSig {
            return_type: Ty::String,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Speaker::hello".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Speaker", &[], "hello")
        .expect("type_defs method should resolve");
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn named_type_with_get_method_rejects_bracket_index_via_type_def() {
    // m[k] on a named type that has a `.get()` method is no longer accepted;
    // the checker must emit a diagnostic pointing at `.get(k)`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Boxy".to_string(),
        make_test_type_def("Boxy", vec!["T".to_string()], methods),
    );
    checker.env.define(
        "boxy".to_string(),
        Ty::Named {
            builtin: None,
            name: "Boxy".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("boxy".to_string()), 0..4)),
        index: Box::new(make_int_literal(0, 5..6)),
    };

    let ty = checker.synthesize(&expr, &(0..6));
    assert_eq!(
        ty,
        Ty::Error,
        "bracket-index on named type must produce Ty::Error"
    );
    assert!(
        !checker.errors.is_empty(),
        "expected a diagnostic for bracket-index on named type with .get()"
    );
    let msg = &checker.errors[0].message;
    assert!(
        msg.contains(".get(k)"),
        "diagnostic should point at .get(k), got: {msg}"
    );
}

#[test]
fn named_type_with_get_method_rejects_bracket_index_via_fn_sig() {
    // Same as above but the `get` method is registered via fn_sigs rather than
    // inline on the type_def (the fn_sig-fallback path in lookup_named_method_sig).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.env.define(
        "wrapper".to_string(),
        Ty::Named {
            builtin: None,
            name: "Wrapper".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("wrapper".to_string()), 0..7)),
        index: Box::new(make_int_literal(0, 8..9)),
    };

    let ty = checker.synthesize(&expr, &(0..9));
    assert_eq!(
        ty,
        Ty::Error,
        "bracket-index on named type must produce Ty::Error"
    );
    assert!(
        !checker.errors.is_empty(),
        "expected a diagnostic for bracket-index on named type with .get() via fn_sigs"
    );
    let msg = &checker.errors[0].message;
    assert!(
        msg.contains(".get(k)"),
        "diagnostic should point at .get(k), got: {msg}"
    );
}

#[test]
fn hashmap_bracket_index_is_a_compile_error() {
    // m[k] on HashMap<string, i64> must be a compile error since the
    // named-type .get() fallback is removed. The explicit m.get(k) is the
    // correct form (returns Option<i64>).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Register HashMap with a string-keyed .get() method (as the stdlib defines it).
    // Return type is Option<V>, represented as the Named form.
    let option_v = Ty::Named {
        builtin: Some(crate::BuiltinType::Option),
        name: "Option".to_string(),
        args: vec![Ty::Named {
            builtin: None,
            name: "V".to_string(),
            args: vec![],
        }],
    };
    let mut methods = HashMap::new();
    methods.insert(
        "get".to_string(),
        FnSig {
            param_names: vec!["key".to_string()],
            params: vec![Ty::String],
            return_type: option_v,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "HashMap".to_string(),
        make_test_type_def("HashMap", vec!["K".to_string(), "V".to_string()], methods),
    );
    checker.env.define(
        "m".to_string(),
        Ty::Named {
            builtin: None,
            name: "HashMap".to_string(),
            args: vec![Ty::String, Ty::I64],
        },
        false,
    );

    // Use an i64 index so the only diagnostic comes from the named-type guard,
    // not from a type mismatch on the index expression itself.
    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("m".to_string()), 0..1)),
        index: Box::new(make_int_literal(0, 2..3)),
    };

    let ty = checker.synthesize(&expr, &(0..8));
    assert_eq!(ty, Ty::Error, "m[k] on HashMap must produce Ty::Error");
    assert!(
        !checker.errors.is_empty(),
        "m[k] on HashMap<string, i64> must produce a diagnostic"
    );
    let msg = &checker.errors[0].message;
    assert!(
        msg.contains(".get(k)"),
        "diagnostic should point at .get(k), got: {msg}"
    );
}

#[test]
fn index_trait_user_impl_runs() {
    let output = check_source(
        r"
        type Grid {
            bias: i32;
        }

        impl Index<i32> for Grid {
            type Output = i32;

            fn get(g: Grid, key: i32) -> Option<i32> {
                Some(g.bias + key)
            }

            fn at(g: Grid, key: i32) -> i32 {
                g.bias + key
            }
        }

        fn f() -> i32 {
            let g = Grid { bias: 40 };
            g[2]
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "user Index impl should type-check: {:?}",
        output.errors
    );
}

#[test]
fn index_dispatch_via_trait_for_vec() {
    let output = check_source("fn f(xs: Vec<i32>) -> i32 { xs[0] }");
    assert!(
        output.errors.is_empty(),
        "Vec indexing should continue to type-check through the auto-impl path: {:?}",
        output.errors
    );
}

#[test]
fn index_read_for_hashmap_string_key_yields_bare_value() {
    // `m["k"]` is the trapping `Index::at` accessor and yields the BARE value
    // `V` (a missing key aborts with IndexOutOfBounds — the map analogue of a
    // `v[i]` out-of-bounds trap). A function returning the bare value type
    // `i32` accepts the index result directly. The non-aborting outcome lives
    // on `m.get("k") -> Option<V>`.
    let output = check_source(r#"fn f(m: HashMap<string, i32>) -> i32 { m["k"] }"#);
    assert!(
        output.errors.is_empty(),
        "HashMap<string, _> read indexing should type-check as bare V: {:?}",
        output.errors
    );
}

#[test]
fn index_read_for_hashmap_string_key_is_not_option() {
    // The read result is the bare value `V` (trapping), so a function annotated
    // to return `Option<V>` must be rejected — proving the surface inverted from
    // the old `Option`-returning read. The non-aborting outcome is `m.get(k) ->
    // Option<V>`, not the `m[k]` trapping accessor.
    let output = check_source(r#"fn f(m: HashMap<string, i32>) -> Option<i32> { m["k"] }"#);
    assert!(
        !output.errors.is_empty(),
        "HashMap read index returns bare V, so an Option<i32> return must mismatch: {:?}",
        output.errors
    );
}

#[test]
fn dyn_index_with_output_binding() {
    let output = check_source("fn f(idx: dyn Index<Output = i32>) -> i32 { idx[2] }");
    assert!(
        output.errors.is_empty(),
        "dyn Index<Output = i32> indexing should type-check: {:?}",
        output.errors
    );
    assert!(
        output
            .dyn_trait_method_calls
            .values()
            .any(|call| call.trait_name == "Index" && call.method_name == "at"),
        "checker should record a dyn Index::at vtable dispatch for []"
    );
}

#[test]
fn named_method_lookup_substitutes_type_params_for_fn_sig_fallback() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::value".to_string(),
        FnSig {
            param_names: vec!["next".to_string()],
            params: vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
            return_type: Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Wrapper", &[Ty::String], "value")
        .expect("fn_sigs fallback should resolve");
    assert_eq!(sig.params, vec![Ty::String]);
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn module_qualified_named_type_method_rewrite_uses_unqualified_method_symbol() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Thing".to_string(),
        make_test_type_def("Thing", vec![], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Thing::label".to_string(),
        FnSig {
            return_type: Ty::String,
            ..FnSig::default()
        },
    );
    checker.env.define(
        "thing".to_string(),
        Ty::Named {
            builtin: None,
            name: "widgets.Thing".to_string(),
            args: vec![],
        },
        false,
    );

    let receiver = (Expr::Identifier("thing".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "label", &[], &(0..13));

    assert_eq!(ty, Ty::String);
    assert!(
        checker.errors.is_empty(),
        "expected clean module-qualified method dispatch, got: {:?}",
        checker.errors
    );
    assert!(
        checker
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(
                rewrite,
                MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "Thing::label"
            )),
        "module-qualified receiver must rewrite via unqualified method key, got: {:?}",
        checker.method_call_rewrites
    );
    assert!(
        !checker
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(
                rewrite,
                MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                    if c_symbol == "widgets.Thing::label"
            )),
        "qualified type prefix must not leak into method rewrite symbol: {:?}",
        checker.method_call_rewrites
    );
}

#[test]
fn generic_named_method_calls_record_method_type_args() {
    let source = r#"
        type Wrapper<T> { value: T }

        impl<T> Wrapper<T> {
            fn map<U>(wrapper: Wrapper<T>, mapper: fn(T) -> U) -> U {
                mapper(wrapper.value)
            }
        }

        fn to_len(value: string) -> i64 {
            value.len()
        }

        fn main() {
            let wrapper = Wrapper { value: "hew" };
            let len = wrapper.map(to_len);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "generic method call should type-check cleanly: {:?}",
        output.errors
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64]),
        "method call should record inferred method type args, got {:?}",
        output.call_type_args
    );
}

#[test]
fn impl_method_registration_keeps_inline_method_bounds_on_all_surfaces() {
    let source = r"
        trait Show {
            fn show(value: Self);
        }

        type Wrapper {}

        impl Wrapper {
            fn map<U: Show>(wrapper: Wrapper, value: U) -> U {
                value
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "inline-bounded impl method should register without checker errors: {:?}",
        output.errors
    );

    let fn_sig = output
        .fn_sigs
        .get("Wrapper::map")
        .expect("impl method must populate fn_sigs");
    let method_sig = output
        .type_defs
        .get("Wrapper")
        .and_then(|type_def| type_def.methods.get("map"))
        .expect("impl method must populate type_def.methods");

    assert_eq!(
        fn_sig.type_param_bounds.get("U"),
        Some(&vec!["Show".to_string()]),
        "fn_sigs surface must retain method-inline bounds"
    );
    assert_eq!(
        method_sig.type_param_bounds.get("U"),
        Some(&vec!["Show".to_string()]),
        "type_def.methods surface must retain method-inline bounds"
    );
}

// -------------------------------------------------------------------------
// Structural-hardening tests (qualified names + super-trait walk)
// -------------------------------------------------------------------------

#[test]
fn structural_hardening_uses_fn_sigs_named_method_fallback() {
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], HashMap::new()),
    );
    checker
        .fn_sigs
        .insert("Speaker::hello".to_string(), FnSig::default());

    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check should reuse named-method fn_sigs fallback"
    );
}

#[test]
fn structural_hardening_prefers_builtin_method_surface_for_imported_handle() {
    let mut checker = make_checker_with_trait("Closable", &["close"], false, false);

    let mut methods = HashMap::new();
    methods.insert(
        "close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Sender".to_string(),
        make_test_type_def("Sender", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Sender::close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );

    assert!(
        checker.type_structurally_satisfies("channel.Sender", "Closable"),
        "structural check should prefer builtin Sender::close over imported stubs"
    );
}

#[test]
fn structural_hardening_qualified_trait_name_matches() {
    // A type registered under "Speaker" must structurally satisfy a bound
    // expressed as "greet.Greet" once "greet" is a known module.
    // We build the checker state manually because check_program drains
    // type_defs/fn_sigs at the end of the pass.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("greet".to_string());

    // Register a TypeDef for Speaker.  The trait `hello(val: Self)` has its
    // receiver stripped by lookup_trait_method, so the effective trait_sig has
    // params=[].  The concrete method entry must match: receiver already stripped.
    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        bounds: HashMap::new(),
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default()); // params=[], return=Unit
            m
        },
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("Speaker", "greet.Greet"),
        "structural check with qualified trait name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check with unqualified trait name must still succeed"
    );
}

#[test]
fn structural_hardening_qualified_type_name_matches() {
    // A bound check with the type expressed as "mymod.Speaker" must succeed
    // when "mymod" is a known module and "Speaker" is registered in type_defs.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("mymod".to_string());

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        bounds: HashMap::new(),
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("mymod.Speaker", "Greet"),
        "structural check with qualified type name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "unqualified type name must still succeed"
    );
}

#[test]
fn structural_hardening_unknown_module_qualifier_is_rejected() {
    // If the prefix is not a known module, we must not strip it and must
    // not accidentally match a same-suffix type/trait.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    // "unknown" is NOT inserted into modules.

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        bounds: HashMap::new(),
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    // Trait "unknown.Greet" should not resolve to "Greet" because "unknown" is
    // not a registered module.
    assert!(
        !checker.type_structurally_satisfies("Speaker", "unknown.Greet"),
        "unrecognised module prefix must not be stripped"
    );
}

#[test]
fn structural_hardening_super_trait_methods_required() {
    // If trait B extends A, a type must provide A's required methods to
    // structurally satisfy B.  Before the fix, only B's own methods were
    // checked and A's were silently skipped.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
            // `print` (from super-trait Printable) is intentionally missing
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "missing super-trait method must fail structural check; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_methods_all_present_succeeds() {
    // When ALL required methods across the super-trait chain are present the
    // structural check must succeed without an explicit impl.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn print(d: Doc) {}
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "all super-trait methods present must pass structural check: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_child_default_overrides_super_required_method() {
    // If a child trait provides a default implementation for a super-trait
    // method, that inherited requirement is satisfied by the trait itself and
    // must not be re-required structurally from the concrete type.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn print(val: Self) {}
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "child default override should satisfy inherited structural requirement: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_diamond_sibling_shadowing_merges_across_supers() {
    // In a diamond, sibling default branches must cover inherited methods
    // collectively so only the root trait's still-required methods remain.
    let source = r"
        trait A {
            fn a(val: Self);
            fn b(val: Self);
        }

        trait B: A {
            fn a(val: Self) {}
        }

        trait C: A {
            fn b(val: Self) {}
        }

        trait D: B + C {
            fn d(val: Self);
        }

        type Doc {}

        impl Doc {
            fn d(d: Doc) {}
        }

        fn use_d<T: D>(t: T) {}

        fn main() {
            let d = Doc {};
            use_d(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "diamond sibling shadowing should merge across supers: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_e1_guard_propagates() {
    // If a super-trait has an associated type, the E1 guard must veto the
    // entire structural check — even if the immediate trait has no assoc types.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Build super-trait with an associated type.
    let assoc_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "AssocSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![
            TraitItem::AssociatedType {
                name: "Output".to_string(),
                default: None,
                bounds: vec![],
                span: 0..0,
            },
            TraitItem::Method(TraitMethod {
                name: "do_it".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            }),
        ],
        doc_comment: None,
        lang_item: None,
    };
    let info_super = Checker::trait_info_from_decl(&assoc_super);
    checker
        .trait_defs
        .insert("AssocSuper".to_string(), info_super);

    // Child trait with no assoc types of its own.
    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "AssocSuper".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["AssocSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "E1 guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn structural_hardening_super_trait_generic_method_guard_propagates() {
    // If a super-trait has a generic method, the E1 guard must veto the whole
    // structural check for the child trait too.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr, TypeParam};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let generic_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "GenericSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "map".to_string(),
            type_params: Some(vec![TypeParam {
                name: "U".to_string(),
                bounds: vec![],
            }]),
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let info_super = Checker::trait_info_from_decl(&generic_super);
    checker
        .trait_defs
        .insert("GenericSuper".to_string(), info_super);

    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "GenericSuper".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["GenericSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "generic-method guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn cyclic_trait_hierarchy_bound_check_surfaces_diagnostic() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .trait_super
        .insert("TraitA".to_string(), vec!["TraitB".to_string()]);
    checker
        .trait_super
        .insert("TraitB".to_string(), vec!["TraitA".to_string()]);
    checker
        .trait_impls_set
        .insert(("Thing".to_string(), "TraitA".to_string()));

    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MissingTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(
        &sig,
        &[Ty::Named {
            builtin: None,
            name: "Thing".to_string(),
            args: vec![],
        }],
        &(0..0),
    );

    assert!(
        checker
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedType
                && error.message.contains("unknown trait `MissingTrait`")),
        "expected cyclic trait hierarchy bound check to fail with a diagnostic; got {:?}",
        checker.errors
    );
}

// ── Non-root module inference-hole fail-closed regression tests ───────────────
//
// These cover the `report_unresolved_inference_holes` path for non-root module
// items in a `module_graph`.  Signature-level `_` holes are tracked by
// `collect_functions` (which already walks non-root modules via `topo_order`)
// and are detected by `report_unresolved_inference_in_items` via the
// `lookup_scoped_item` scoped-name fallback.
//
// Body-level holes (expressions containing `_`) require non-root module body
// checking from PR #756 to propagate into the inference state.  PR #756 added
// the infrastructure; the tests below prove the deferred-hole drain path works
// for non-root module bodies too:
//   - `body_cast_infer_hole_fails_closed`: unresolvable `as _` cast target
//   - `body_let_annotation_infer_resolves_cleanly`: resolvable `let y: _ = 42`
//   - `body_lambda_infer_param_hole_fails_closed`: unresolvable lambda `|x: _|`
