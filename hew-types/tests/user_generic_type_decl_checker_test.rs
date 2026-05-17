// Tests that the checker correctly registers `pub type Box<T>`, `pub type Pair<A, B>`,
// and `pub enum Result<T, E>` as generic type definitions, and that malformed cases
// (duplicate type params) produce clear errors.
mod common;

use hew_types::error::TypeErrorKind;

// ── Helpers ──────────────────────────────────────────────────────────────────

fn check(source: &str) -> hew_types::TypeCheckOutput {
    common::typecheck_isolated(source)
}

// ── Registration: type_params are populated on TypeDef ───────────────────────

#[test]
fn checker_registers_pub_type_box_t_with_one_type_param() {
    let tco = check("pub type Box<T> { value: T }");
    assert!(
        tco.errors.is_empty(),
        "expected no checker errors: {:?}",
        tco.errors
    );
}

#[test]
fn checker_registers_pub_type_pair_ab_with_two_type_params() {
    let tco = check("pub type Pair<A, B> { first: A; second: B }");
    assert!(
        tco.errors.is_empty(),
        "expected no checker errors: {:?}",
        tco.errors
    );
}

#[test]
fn checker_registers_pub_enum_result_te_with_two_type_params() {
    let tco = check("pub enum Result<T, E> { Ok(T); Err(E) }");
    assert!(
        tco.errors.is_empty(),
        "expected no checker errors: {:?}",
        tco.errors
    );
}

// ── Substitution: type params are usable at init sites ───────────────────────

#[test]
fn checker_accepts_explicit_type_arg_at_struct_init() {
    let tco = check(
        r"
        pub type Box<T> { value: T }
        fn main() {
            let b = Box<int> { value: 42 };
        }
    ",
    );
    assert!(
        tco.errors.is_empty(),
        "explicit type arg at init should check cleanly: {:?}",
        tco.errors
    );
}

#[test]
fn checker_accepts_inferred_type_arg_at_struct_init() {
    // Type body: fields separated by `;`. Struct literal expressions: fields separated by `,`.
    let tco = check(
        r#"
        pub type Box<T> { value: T }
        fn main() {
            let b: Box<string> = Box { value: "hello" };
        }
    "#,
    );
    assert!(
        tco.errors.is_empty(),
        "inferred type arg at init should check cleanly: {:?}",
        tco.errors
    );
}

#[test]
fn checker_accepts_two_param_type_at_init() {
    // Type body declarations use `;` to separate fields,
    // but struct literal expressions use `,` between fields.
    let tco = check(
        r#"
        pub type Pair<A, B> { first: A; second: B }
        fn main() {
            let p = Pair<int, string> { first: 1, second: "hello" };
        }
    "#,
    );
    assert!(
        tco.errors.is_empty(),
        "two-param type at explicit init should check cleanly: {:?}",
        tco.errors
    );
}

#[test]
fn checker_accepts_generic_enum_variant_construction() {
    let tco = check(
        r"
        pub enum Either<A, B> { Left(A); Right(B) }
        fn main() {
            let x: Either<int, string> = Either::Left(42);
        }
    ",
    );
    assert!(
        tco.errors.is_empty(),
        "generic enum variant construction should check cleanly: {:?}",
        tco.errors
    );
}

// ── Monomorphic type: type_params should be absent ───────────────────────────

#[test]
fn checker_accepts_monomorphic_type_without_type_params() {
    let tco = check("pub type Point { x: int; y: int }");
    assert!(
        tco.errors.is_empty(),
        "monomorphic type should check cleanly: {:?}",
        tco.errors
    );
}

// ── Error cases: malformed type param declarations ────────────────────────────

#[test]
fn checker_rejects_duplicate_type_param_names() {
    // `pub type Box<T, T> { value: T }` — two params named `T` is invalid
    let tco = check("pub type Box<T, T> { value: T }");
    assert!(
        !tco.errors.is_empty(),
        "duplicate type param names should produce a checker error"
    );
    assert!(
        tco.errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition
                && e.message.contains("type parameter")
                && e.message.contains("`T`")),
        "expected a DuplicateDefinition error mentioning 'type parameter' and 'T', got: {:?}",
        tco.errors
    );
}

#[test]
fn checker_rejects_duplicate_type_params_in_enum() {
    let tco = check("pub enum Either<T, T> { Left(T); Right(T) }");
    assert!(
        !tco.errors.is_empty(),
        "duplicate type param names in enum should produce a checker error"
    );
    assert!(
        tco.errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "expected a DuplicateDefinition error, got: {:?}",
        tco.errors
    );
}

#[test]
fn checker_rejects_wrong_type_arg_at_init_site() {
    // `Box<int> { value: "hello" }` — explicit `int` but field value is string
    let tco = check(
        r#"
        pub type Box<T> { value: T }
        fn main() {
            let b = Box<int> { value: "hello" };
        }
    "#,
    );
    assert!(
        !tco.errors.is_empty(),
        "type mismatch at init should produce a checker error"
    );
}
