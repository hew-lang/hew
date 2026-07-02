#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn typecheck_match_statement_exhaustive_enum_ok() {
    let (errors, _) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, Green => 2, } let _done = 0; }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn typecheck_match_statement_missing_variant_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, } let _done = 0; }\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive enum match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
}

#[test]
fn typecheck_guarded_wildcard_not_exhaustive() {
    // A guarded wildcard (`_ if false`) does not count as an exhaustive arm;
    // matching bool with only that arm leaves both `true` and `false` uncovered.
    // Bool is enum-like, so this is a hard error.
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x = true;\n",
        "    match x {\n",
        "        _ if false => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive bool match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch))
        .expect("expected NonExhaustiveMatch error");
    assert_eq!(err.severity, crate::error::Severity::Error);
    assert_eq!(err.message, "non-exhaustive match: missing true, false");
}

/// Literal-only i64 matches are fail-closed: without a catch-all there are
/// infinitely many missing values, so this is a hard non-exhaustive error.
#[test]
fn typecheck_i64_literal_missing_catchall_is_error() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x: i64 = 5;\n",
        "    match x {\n",
        "        1 => 10,\n",
        "        2 => 20,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive literal i64 match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "literal i64 missing catch-all must be an error: {errors:?}"
    );
}

/// Literal-only fixed-width and pointer-width integer matches are fail-closed:
/// without a catch-all there are infinitely many missing values, so these are
/// hard non-exhaustive errors for every integer width, not just i64.
#[test]
fn typecheck_integer_literal_missing_catchall_is_error_for_all_widths() {
    for ty in [
        "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "isize", "usize",
    ] {
        let (errors, warnings) = parse_and_check(&format!(
            "fn _f(x: {ty}) -> i64 {{\n\
                 match x {{\n\
                     0 => 10,\n\
                     1 => 20,\n\
                 }}\n\
                 0\n\
             }}\n"
        ));
        assert!(
            warnings
                .iter()
                .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
            "non-exhaustive literal {ty} match must not be a warning: {warnings:?}"
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
            "literal {ty} missing catch-all must be an error: {errors:?}"
        );
    }
}

#[test]
fn typecheck_integer_literal_with_catchall_is_exhaustive() {
    for ty in ["i32", "u8", "isize", "usize"] {
        let (errors, warnings) = parse_and_check(&format!(
            "fn main() {{\n\
                 let x: {ty} = 1;\n\
                 match x {{\n\
                     0 => 10,\n\
                     1 => 20,\n\
                     _ => 0,\n\
                 }}\n\
                 let _done = 0;\n\
             }}\n"
        ));
        assert!(
            errors.is_empty(),
            "catch-all {ty} match must not error: {errors:?}"
        );
        assert!(
            warnings
                .iter()
                .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
            "catch-all {ty} match must not warn as non-exhaustive: {warnings:?}"
        );
    }
}

#[test]
fn unsupported_payload_subpattern_suppresses_non_exhaustive_follow_on() {
    let (errors, warnings) = parse_and_check(
        r"
enum Color { Red; Blue }
enum Packet { Data { value: Color }; Empty }
fn f(p: Packet) -> i64 {
    match p {
        Packet::Data { value: Red } => 1,
    }
}",
    );

    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UnsupportedPayloadSubpattern { .. })),
        "expected unsupported payload subpattern root error, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "unsupported payload subpattern must suppress non-exhaustive cascade: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "unsupported payload subpattern must suppress non-exhaustive warnings: {warnings:?}"
    );
}

#[test]
fn typecheck_bool_true_false_match_remains_exhaustive() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x = true;\n",
        "    match x {\n",
        "        true => 1,\n",
        "        false => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "exhaustive bool match must not warn as non-exhaustive: {warnings:?}"
    );
}

/// String matches are over an open domain; literal arms need a catch-all.
#[test]
fn typecheck_string_literal_missing_catchall_is_error() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let s: string = \"maybe\";\n",
        "    match s {\n",
        "        \"yes\" => 1,\n",
        "        \"no\" => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive literal string match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "literal string missing catch-all must be an error: {errors:?}"
    );
}

#[test]
fn typecheck_string_literal_with_catchall_is_exhaustive() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let s: string = \"maybe\";\n",
        "    match s {\n",
        "        \"yes\" => 1,\n",
        "        \"no\" => 0,\n",
        "        _ => -1,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "catch-all string match must not warn as non-exhaustive: {warnings:?}"
    );
}

#[test]
fn typecheck_float_literal_pattern_is_accepted() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() -> i64 {\n",
        "    let x: f64 = -1.0;\n",
        "    match x {\n",
        "        -1.0 => 10,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::InvalidOperation)),
        "float literal pattern must not emit InvalidOperation, got: {errors:?}"
    );
}

#[test]
fn typecheck_struct_pattern_unknown_field_errors() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64, y: i64 }\n",
        "fn main() {\n",
        "    let p = Point { x: 1, y: 2 };\n",
        "    match p {\n",
        "        Point { x, z } => x,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedField)),
        "expected UndefinedField error for unknown field 'z', got: {errors:?}"
    );
}

#[test]
fn typecheck_match_wrong_enum_variant_errors() {
    // Matching a Colour scrutinee with a Shape variant should be an error.
    let (errors, _) = parse_and_check(concat!(
        "enum Colour { Red; Green; Blue; }\n",
        "enum Shape { Circle(i32); Rectangle(i32); }\n",
        "fn describe(c: Colour) -> i32 {\n",
        "    match c {\n",
        "        Circle(r) => r,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors.is_empty(),
        "expected type error for wrong-enum variant in match, got no errors"
    );
}

#[test]
fn typecheck_or_pattern_asymmetric_bindings_error() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let value = (1, 2);\n",
        "    match value {\n",
        "        (x, _) | (_, _) => 0,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("expected asymmetric or-pattern binding diagnostic");
    assert_eq!(err.message, "or-pattern branches must bind the same names");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `x`")),
        "expected left-branch binding note, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds no names")),
        "expected right-branch binding note, got: {err:?}"
    );
}

#[test]
fn typecheck_or_pattern_symmetric_bindings_ok() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() -> i64 {\n",
        "    let value = (1, 2);\n",
        "    match value {\n",
        "        (x, _) | (_, x) => x,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn typecheck_or_pattern_incompatible_binding_types_error() {
    let (errors, _) = parse_and_check(concat!(
        "fn unwrap(result: Result<i64, string>) -> i64 {\n",
        "    match result {\n",
        "        Ok(x) | Err(x) => x,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("expected incompatible or-pattern binding diagnostic");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `x` as `i64`")),
        "expected left-branch type note, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds `x` as `string`")),
        "expected right-branch type note, got: {err:?}"
    );
}

#[test]
fn typecheck_or_pattern_uppercase_binders_inconsistent_error() {
    // #2116 closing blocker: the or-pattern binding-consistency check must be
    // driven by the AUTHORITATIVE binding result (the env delta), not a casing
    // re-derivation.  `FOO` and `BAR` do not resolve as variants of `i64`, so
    // both are *binders*.  The two branches bind different names → inconsistent
    // → must raise OrPatternBindingMismatch.  The former casing heuristic dropped
    // uppercase binders, yielding two empty sets that compared equal and silently
    // accepted the mismatch (the left branch's binding won).
    let (errors, _) = parse_and_check(concat!(
        "fn f(x: i64) -> i64 {\n",
        "    match x {\n",
        "        FOO | BAR => FOO,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("FOO | BAR distinct uppercase binders must raise OrPatternBindingMismatch");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `FOO`")),
        "expected left-branch binding note for `FOO`, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds `BAR`")),
        "expected right-branch binding note for `BAR`, got: {err:?}"
    );
}

#[test]
fn typecheck_or_pattern_uppercase_binders_consistent_ok() {
    // The dual of the blocker: same uppercase binder on both sides is a
    // *consistent* binding and must be accepted (any casing).  This is the case
    // the old casing heuristic accepted only vacuously (by dropping both names);
    // the resolution/env-delta path accepts it because both branches genuinely
    // bind `FOO` at the same type.
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() -> i64 {\n",
        "    let x = 5;\n",
        "    match x {\n",
        "        FOO | FOO => FOO,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch)),
        "consistent uppercase binder `FOO | FOO` must not raise OrPatternBindingMismatch; got: {errors:?}"
    );
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn typecheck_or_pattern_unit_variants_bind_nothing_ok() {
    // Soundness guard against the false-hit the old `// JUSTIFIED` comment feared:
    // two unit-variant constructors bind NOTHING on both sides, so the branches
    // are consistent (empty == empty) and must be accepted.  Because constructor
    // identifiers are no longer over-defined as env bindings, the env delta for
    // each branch is empty and no spurious mismatch is raised.
    let (errors, _) = parse_and_check(concat!(
        "enum Color { Red; Green; Blue }\n",
        "fn f(c: Color) -> i64 {\n",
        "    match c {\n",
        "        Red | Green => 1,\n",
        "        Blue => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch)),
        "constructor or-pattern `Red | Green` (binds nothing) must not raise \
         OrPatternBindingMismatch; got: {errors:?}"
    );
}

#[test]
fn typecheck_or_pattern_constructor_vs_binder_inconsistent_error() {
    // Mixed branch: a unit variant (`Red`, binds nothing) or a binder (`x`).
    // The binder is present in only one branch → inconsistent → must error.
    // This proves the classification is resolution-driven on BOTH sides: `Red`
    // resolves as a variant (no binder) while `x` does not (a binder).
    let (errors, _) = parse_and_check(concat!(
        "enum Color { Red; Green; Blue }\n",
        "fn f(c: Color) -> i64 {\n",
        "    match c {\n",
        "        Red | x => 1,\n",
        "        Blue => 0,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("`Red | x` (constructor vs binder) must raise OrPatternBindingMismatch");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds no names")),
        "expected left-branch 'binds no names' note for unit variant `Red`, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds `x`")),
        "expected right-branch binding note for `x`, got: {err:?}"
    );
}

#[test]
fn typecheck_or_pattern_regex_capture_inconsistent_error() {
    // The env-delta consistency check sees regex capture binders, which the old
    // `collect_pattern_bound_names` dropped for ALL `Pattern::Regex` (returning an
    // empty set).  A capturing branch (`x`) versus a non-capturing branch is
    // inconsistent and must error — previously this was silently accepted.
    let (errors, _) = parse_and_check(concat!(
        "fn f(s: string) -> i64 {\n",
        "    match s {\n",
        "        re\"(?P<x>a+)\" | re\"b+\" => 1,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch)),
        "regex capture `x` present in only one or-branch must raise \
         OrPatternBindingMismatch; got: {errors:?}"
    );
}

#[test]
fn typecheck_or_pattern_unit_variant_vs_payload_binder_inconsistent_error() {
    // `Some(v) | None` over Option: the left binds the payload `v`, the right
    // (a unit variant) binds nothing → inconsistent.  Confirms payload binders
    // and unit-variant non-binders are reconciled by the same env-delta path.
    let (errors, _) = parse_and_check(concat!(
        "fn f(opt: Option<i64>) -> i64 {\n",
        "    match opt {\n",
        "        Some(v) | None => v,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch)),
        "`Some(v) | None` must raise OrPatternBindingMismatch (payload binder absent \
         from the unit-variant branch); got: {errors:?}"
    );
}

#[test]
fn typecheck_or_pattern_error_scrutinee_no_cascade() {
    // Error-recovery guard: when the scrutinee type is unresolvable (here an
    // undefined function makes it `Ty::Error`), a bare identifier cannot be
    // classified as constructor-or-binder, so the binding-consistency check is
    // suppressed rather than cascading off the already-broken scrutinee.  Only
    // the root-cause error (the undefined function) is reported — NO
    // OrPatternBindingMismatch is piled on, even though `Red` and `Green` would
    // otherwise look like two distinct binders against an unknown type.
    let (errors, _) = parse_and_check(concat!(
        "enum Colour { Red; Green }\n",
        "fn main() {\n",
        "    let _ = match missing() {\n",
        "        Red | Green => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch)),
        "an or-pattern over an error-typed scrutinee must not cascade an \
         OrPatternBindingMismatch; got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedFunction)),
        "the root-cause undefined-function error must still be reported; got: {errors:?}"
    );
}

// ── Borrowed-param escape: constructor classification (#2116 audit) ──────────
// `callee_is_aggregate_constructor` decides whether the borrowed-param escape
// analysis descends into a call's arguments.  It must classify by resolution,
// not by the callee's casing, or it both falsely rejects valid programs (an
// uppercase regular function) and falsely accepts memory-unsafe ones (a
// lowercase variant constructor that embeds a borrowed parameter).

#[test]
fn borrowed_param_escape_uppercase_function_call_not_flagged() {
    // Regression for the casing false-positive: a regular function that merely
    // *happens* to be uppercase-named is not a constructor, so passing a borrow
    // parameter to it is a safe borrow under call-boundary ownership — it must
    // NOT raise BorrowedParamReturn.
    let (errors, _) = parse_and_check(concat!(
        "fn Helper(x: &i64) -> i64 { 5 }\n",
        "fn f(r: &i64) -> i64 { Helper(r) }\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "passing a borrow param to the uppercase regular fn `Helper` is safe and must \
         not raise BorrowedParamReturn; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_lowercase_function_call_not_flagged() {
    // Control for the above: the lowercase spelling was already accepted; it must
    // stay accepted.
    let (errors, _) = parse_and_check(concat!(
        "fn helper(x: &i64) -> i64 { 5 }\n",
        "fn f(r: &i64) -> i64 { helper(r) }\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "passing a borrow param to a regular fn is safe; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_lowercase_variant_constructor_flagged() {
    // Regression for the casing false-NEGATIVE (a memory-safety hole): a
    // lowercase enum variant constructor that embeds a borrow parameter in the
    // returned aggregate must raise BorrowedParamReturn.  The old uppercase-first
    // heuristic dropped this, letting a returned reference outlive its owner.
    let (errors, _) = parse_and_check(concat!(
        "enum Holder { wrap(&i64); empty }\n",
        "fn f(r: &i64) -> Holder { wrap(r) }\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "embedding a borrow param in the lowercase variant `wrap` must raise \
         BorrowedParamReturn; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_uppercase_variant_constructor_flagged() {
    // The uppercase variant spelling was already caught and must stay caught —
    // proves the fix does not regress the originally-handled direction.
    let (errors, _) = parse_and_check(concat!(
        "enum Holder { Wrap(&i64); Empty }\n",
        "fn f(r: &i64) -> Holder { Wrap(r) }\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "embedding a borrow param in the variant `Wrap` must raise \
         BorrowedParamReturn; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_builtin_variant_constructor_flagged() {
    // Builtin Option/Result variant constructors must remain classified as
    // aggregate constructors so an embedded borrow param is still flagged.
    let (errors, _) = parse_and_check("fn f(r: &i64) -> Option<&i64> { Some(r) }\n");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "embedding a borrow param in `Some(_)` must raise BorrowedParamReturn; \
         got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_qualified_path_constructor_flagged() {
    // A `Type::Variant` qualified path constructor must remain classified as an
    // aggregate constructor (fail-closed for all `::` paths, including `Rc::new`).
    let (errors, _) = parse_and_check(concat!(
        "enum Holder { V(&i64); E }\n",
        "fn f(r: &i64) -> Holder { Holder::V(r) }\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "embedding a borrow param in the qualified variant `Holder::V` must raise \
         BorrowedParamReturn; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_match_arm_variant_collision_flagged() {
    // Residual fix (#2116): a match-arm pattern that is a *unit-variant
    // constructor* whose name collides with a borrow parameter must NOT shadow
    // that parameter in the escape scanner. The arm body `red` therefore
    // resolves to the borrowed param (the constructor `red` bound nothing), and
    // returning it is a genuine escape that must raise BorrowedParamReturn.
    //
    // Before the fix `shadow_pattern_bindings` blindly treated every
    // `Pattern::Identifier` as a binder, masking the `red => red` arm; the
    // escape only surfaced (confusingly, via a sibling HIR/MIR pass) when a
    // second non-colliding arm also returned the param. This fixture would
    // regress to a missed escape on the colliding arm if the binder-vs-
    // constructor decision were re-derived locally again.
    let (errors, _) = parse_and_check(concat!(
        "enum Color { red; green; }\n",
        "fn leak(red: &i64, color: Color) -> &i64 {\n",
        "    match color { red => red, green => red }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "a unit-variant arm `red` colliding with borrow param `red` must not \
         shadow it — returning the param must raise BorrowedParamReturn; \
         got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_or_pattern_variant_collision_flagged() {
    // The reviewer's exact repro shape, with a borrow param so the escape
    // scanner actually runs: an or-pattern of unit-variant constructors that
    // collide with the borrow param name. Both `red` and `green` are
    // constructors (bind nothing), so the arm body `red` is the borrowed param
    // and returning it must raise BorrowedParamReturn — not a confusing
    // InitialisedBeforeUse / MIR-decision-map diagnostic from a sibling pass.
    let (errors, _) = parse_and_check(concat!(
        "enum Color { red; green; }\n",
        "fn leak(red: &i64, color: Color) -> &i64 {\n",
        "    match color { red | green => red }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "an or-pattern of constructors `red | green` must not shadow the \
         borrow param `red`; returning it must raise BorrowedParamReturn; \
         got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_match_payload_binder_not_overflagged() {
    // Guard against over-flagging: a genuine payload *binder* (`Some(inner)`)
    // must shadow the dangerous param so returning the bound payload is NOT an
    // escape, while a sibling arm that returns the borrow param directly still
    // is. Exactly one BorrowedParamReturn — the `None => red` arm — must fire;
    // the `Some(inner) => inner` arm must stay clean.
    let (errors, _) = parse_and_check(concat!(
        "fn leak(red: &i64, opt: Option<&i64>) -> &i64 {\n",
        "    match opt { Some(inner) => inner, None => red }\n",
        "}\n",
    ));
    let escapes = errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .count();
    assert_eq!(
        escapes, 1,
        "only the `None => red` arm escapes; the `Some(inner) => inner` binder \
         arm must not be over-flagged; got {escapes} BorrowedParamReturn in: \
         {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_consistent_or_binder_not_overflagged() {
    // End-to-end of the or-pattern binder path: `Ok(x) | Err(x) => x` binds a
    // consistent `x` in both alternatives (recorded as the env delta of
    // `bind_pattern`), which `shadow_pattern_bindings` then shadows. Returning
    // the bound payload `x` (not the borrow param `p`) is safe and must raise
    // NO BorrowedParamReturn — a regression here would mean the or-pattern
    // binder set was not threaded through the single authority.
    let (errors, _) = parse_and_check(concat!(
        "fn leak(p: &i64, r: Result<&i64, &i64>) -> &i64 {\n",
        "    match r { Ok(x) | Err(x) => x }\n",
        "}\n",
    ));
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "returning a consistent or-pattern binder `x` (not the borrow param) \
         must not raise BorrowedParamReturn; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_let_else_unit_variant_collision_flagged() {
    // #2116 residual (6th finding): a `let`-else whose pattern is a unit-variant
    // identifier colliding with a borrow param binds NOTHING — the checker
    // classifies `red` as a refutable tag-test and skips `bind_pattern` for it
    // (statements.rs). The borrow-escape scanner must consult that SAME
    // authority (`let_identifier_is_unit_variant`) and NOT invent a shadow, so
    // the trailing `red` resolves to the borrow param and returning it raises
    // BorrowedParamReturn.
    //
    // Before the fix the scanner's `Stmt::Let` arm blindly treated the
    // identifier as a binder and shadowed the dangerous param, masking the
    // escape; it surfaced only later (confusingly) as a sibling MIR
    // `DecisionMapTotal` / HIR no-binding error.
    let (errors, _) = parse_and_check(concat!(
        "enum Color { red; green; }\n",
        "fn leak(red: &i64, color: Color) -> &i64 {\n",
        "    let red = color else { panic(\"no\") };\n",
        "    red\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "let-else unit-variant `let red = color else …` binds nothing — the \
        trailing `red` is the borrow param and must raise BorrowedParamReturn; \
        got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_let_else_intermediate_binding_flagged() {
    // The masked param flows through a genuine intermediate binder. After the
    // unit-variant `let red = color else …` (which binds nothing), `let x = red`
    // copies the still-dangerous borrow param into `x`; returning `x` must be
    // flagged. Exercises both the unit-variant skip AND the genuine-binder
    // danger-propagation path in the same function.
    let (errors, _) = parse_and_check(concat!(
        "enum Color { red; green; }\n",
        "fn leak(red: &i64, color: Color) -> &i64 {\n",
        "    let red = color else { panic(\"no\") };\n",
        "    let x = red;\n",
        "    x\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "the borrow param survives the unit-variant let-else and is copied into \
        `x`; returning `x` must raise BorrowedParamReturn; got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_while_let_unit_variant_collision_flagged() {
    // The `while let` binding form already routes through the shared authority
    // (`shadow_pattern_bindings` → `pattern_bound_names`); a unit-variant pattern
    // records no binder, so `return red` inside the loop resolves to the borrow
    // param and is flagged. Locks that consistency in alongside the let-else fix.
    let (errors, _) = parse_and_check(concat!(
        "enum Color { red; green; }\n",
        "fn leak(red: &i64, color: Color) -> &i64 {\n",
        "    while let red = color {\n",
        "        return red;\n",
        "    }\n",
        "    red\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "while-let unit-variant `while let red = color` binds nothing — \
        `return red` is the borrow param and must raise BorrowedParamReturn; \
        got: {errors:?}"
    );
}

#[test]
fn borrowed_param_escape_simple_let_binder_propagation_preserved() {
    // Guard against the fix over-reaching: a genuine simple `let` binder (`let y
    // = red`, where `y` is NOT a unit variant) must still copy the borrow
    // param's danger forward, so returning `y` is flagged. Proves the
    // unit-variant guard did not disable danger-propagation for real binders.
    let (errors, _) = parse_and_check(concat!(
        "fn leak(red: &i64) -> &i64 {\n",
        "    let y = red;\n",
        "    y\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BorrowedParamReturn)),
        "a genuine binder `let y = red` must still carry the borrow param's \
        danger forward; returning `y` must raise BorrowedParamReturn; \
        got: {errors:?}"
    );
}

#[test]
fn var_named_like_unit_variant_still_binds() {
    // Inertness proof for the escape scanner's `Stmt::Var` arm (which records
    // the var name as a dangerous binding unconditionally): a `var` has no
    // pattern and no refutability, so its name is ALWAYS a fresh binder — even
    // when it spells a unit variant. If `var a` were ever classified as a
    // constructor (non-binding), `a = a + 1; a` would fail to resolve. It does
    // not, so treating the var name as a binder can never disagree with the
    // checker.
    let (errors, _) = parse_and_check(concat!(
        "enum E { a; b; }\n",
        "fn f() -> i64 {\n",
        "    var a = 0;\n",
        "    a = a + 1;\n",
        "    a\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "`var a` must bind a fresh i64 even though `a` is a unit variant of E; \
        got: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_constructor_pattern_stays_fail_closed() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Some(x) => x,\n",
        "        None => panic(\"boom\"),\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::InferenceFailed)),
        "errored scrutinees must not seed constructor-pattern inference holes: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_pattern_no_undefined_variable_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64, y: i64 }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Point { x, y } => x + y,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_variant_pattern_no_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "enum Shape { Move { x: i64 } }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Shape::Move { x } => x,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_pattern_with_subpattern_no_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64 }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Point { x: inner_x } => inner_x,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_bool_scrutinee_constructor_pattern_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match true {\n",
        "        Some(v) => 1,\n",
        "        None => 0,\n",
        "        _ => 2,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "Some"
        )),
        "expected constructor-pattern mismatch on bool scrutinee, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("constructor pattern `Some`")),
        "expected fail-closed constructor-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "wildcard arm should suppress exhaustiveness follow-ons: {warnings:?}"
    );
}

#[test]
fn typecheck_int_scrutinee_struct_pattern_errors_without_binding_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64 }\n",
        "fn main() {\n",
        "    let _ = match 42 {\n",
        "        Point { x } => {\n",
        "            let _ = x;\n",
        "            0\n",
        "        },\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors.len(),
        1,
        "expected only the type-pattern mismatch, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "i64" && actual == "Point"
        )),
        "expected type-pattern mismatch on i64 scrutinee, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("type pattern `Point` cannot match value of type `i64`")),
        "expected fail-closed type-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "struct-pattern mismatch must not cascade into undefined-variable errors: {errors:?}"
    );
}

#[test]
fn typecheck_bool_scrutinee_tuple_pattern_errors_without_binding_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _ = match true {\n",
        "        (left, right) => {\n",
        "            let _ = left;\n",
        "            let _ = right;\n",
        "            0\n",
        "        },\n",
        "        _ => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors.len(),
        1,
        "expected only the tuple-pattern mismatch, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "tuple"
        )),
        "expected tuple-pattern mismatch on bool scrutinee, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("tuple pattern cannot match non-tuple type `bool`")),
        "expected fail-closed tuple-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "tuple-pattern mismatch must not cascade into undefined-variable errors: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_skips_exhaustiveness_follow_on() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match missing {\n",
        "        true => 1,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "expected primary undefined variable error, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive errors: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive warnings: {warnings:?}"
    );
}

#[test]
fn typecheck_generic_enum_constructor_infers_type_args() {
    let (errors, _) = parse_and_check(concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<i64>) -> Option<i64> { x }\n",
        "fn take_string(x: Option<string>) -> Option<string> { x }\n",
        "fn main() { take_int(Some(42)); take_string(Some(\"hello\")); }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn generic_enum_constructor_expected_context_coerces_payload_literal() {
    let source = concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<i64>) -> Option<i64> { x }\n",
        "fn main() { take_int(Some(42)); }\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((outer_call, _)) = &main_fn.body.stmts[0].0 else {
        panic!("expected outer call statement");
    };
    let Expr::Call {
        args: outer_args, ..
    } = outer_call
    else {
        panic!("expected outer call expression");
    };
    let (inner_call, inner_call_span) = outer_args[0].expr();
    let Expr::Call {
        args: inner_args, ..
    } = inner_call
    else {
        panic!("expected inner constructor call");
    };
    let (_, literal_span) = inner_args[0].expr();

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(literal_span)),
        Some(&Ty::I64),
        "constructor payload literal should coerce to `i64`: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(inner_call_span)),
        Some(&Ty::option(Ty::I64)),
        "constructor call should resolve to `Option<i64>`: {:?}",
        output.expr_types
    );
}

#[test]
fn builtin_result_constructors_materialize_output_types_without_call_type_args() {
    let source = concat!(
        "fn main() -> i64 {\n",
        "    Ok(7);\n",
        "    Err(9);\n",
        "    0\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, ok_call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected first statement to be `Ok(...)`");
    };
    let Stmt::Expression((Expr::Call { .. }, err_call_span)) = &main_fn.body.stmts[1].0 else {
        panic!("expected second statement to be `Err(...)`");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(ok_call_span)),
        "builtin `Ok(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(err_call_span)),
        "builtin `Err(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(ok_call_span)),
        Some(&Ty::result(Ty::I64, Ty::I64)),
        "expected `Ok(7)` output type to materialize fully before serialization: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(err_call_span)),
        Some(&Ty::result(Ty::I64, Ty::I64)),
        "expected `Err(9)` output type to materialize fully before serialization: {:?}",
        output.expr_types
    );
}

#[test]
fn result_constructors_accept_unit_payloads() {
    let source = concat!(
        "fn ok_unit() -> Result<(), i64> { Ok(()) }\n",
        "fn err_unit() -> Result<i64, ()> { Err(()) }\n",
    );
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
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn ok_unit_match_pattern_accepted() {
    // `Ok(())` as a match arm pattern against `Result<(), E>` must not error.
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let r: Result<(), string> = Ok(());\n",
        "    let _ = match r {\n",
        "        Ok(()) => 0,\n",
        "        Err(_) => 1,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "Ok(()) pattern in match arm against Result<(), E> must type-check: {errors:?}"
    );
}

#[test]
fn err_unit_match_pattern_accepted() {
    // `Err(())` as a match arm pattern against `Result<T, ()>` must not error.
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let r: Result<i64, ()> = Err(());\n",
        "    let _ = match r {\n",
        "        Ok(n) => n,\n",
        "        Err(()) => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "Err(()) pattern in match arm against Result<T, ()> must type-check: {errors:?}"
    );
}

#[test]
fn unit_match_pattern_accepted_on_unit_scrutinee() {
    // `()` as a top-level match pattern on a unit scrutinee must not error.
    let (errors, _) = parse_and_check(concat!(
        "fn unit_val() -> () { () }\n",
        "fn main() {\n",
        "    let _ = match unit_val() {\n",
        "        () => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "() pattern against unit scrutinee must type-check: {errors:?}"
    );
}

#[test]
fn ok_unit_pattern_rejected_against_non_unit_ok_payload() {
    // `Ok(())` against `Result<i64, E>` must still be an error — the payload
    // type is `i64`, not unit, so the empty-tuple pattern is a mismatch.
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let r: Result<i64, string> = Ok(1);\n",
        "    let _ = match r {\n",
        "        Ok(()) => 0,\n",
        "        Err(_) => 1,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        !errors.is_empty(),
        "Ok(()) against Result<i64, E> must produce a type error"
    );
}

#[test]
fn builtin_result_constructor_composite_output_type_fallbacks_materialize() {
    let source = concat!(
        "fn main() -> i64 {\n",
        "    Ok(Some(7));\n",
        "    Err(Some(9));\n",
        "    0\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, ok_call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected first statement to be `Ok(...)`");
    };
    let Stmt::Expression((Expr::Call { .. }, err_call_span)) = &main_fn.body.stmts[1].0 else {
        panic!("expected second statement to be `Err(...)`");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(ok_call_span)),
        "builtin `Ok(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(err_call_span)),
        "builtin `Err(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    let expected = Ty::result(Ty::option(Ty::I64), Ty::option(Ty::I64));
    assert_eq!(
        output.expr_types.get(&SpanKey::from(ok_call_span)),
        Some(&expected),
        "expected `Ok(Some(7))` output type to preserve composite fallback: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(err_call_span)),
        Some(&expected),
        "expected `Err(Some(9))` output type to preserve composite fallback: {:?}",
        output.expr_types
    );
}

#[test]
fn explicit_cooperate_expression_is_parse_error() {
    let source = "fn main() { cooperate(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|error| error.message.contains(
            "'cooperate' is compiler-internal; explicit cooperate expressions are not supported"
        )),
        "expected explicit cooperate parse rejection, got: {:?}",
        result.errors
    );
}
