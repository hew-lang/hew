#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

mod pattern_resolution {
    use super::*;

    /// Helper: type-check `source` and return the `pattern_resolutions` map.
    fn pattern_resolutions(source: &str) -> HashMap<SpanKey, ArmResolution> {
        let output = check_source(source);
        assert!(
            output.errors.is_empty(),
            "pattern_resolutions test must parse and check cleanly; errors: {:#?}",
            output.errors
        );
        output.pattern_resolutions
    }

    // ── wildcard arm ─────────────────────────────────────────────────────────

    #[test]
    fn wildcard_arm_records_wildcard_kind() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) {
    match x {
        _ => {}
    }
}",
        );
        assert_eq!(resolutions.len(), 1, "expected one resolution");
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::Wildcard);
        assert!(arm.variant_match.is_none());
        assert!(arm.payload_bindings.is_empty());
    }

    // ── literal arm ──────────────────────────────────────────────────────────

    #[test]
    fn literal_arm_records_literal_kind() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) -> i64 {
    match x {
        1 => 10,
        _ => 0,
    }
}",
        );
        // Two arms: literal + wildcard
        assert_eq!(resolutions.len(), 2, "expected two resolutions");
        let literal_arm = resolutions
            .values()
            .find(|r| r.pattern_kind == PatternKind::Literal)
            .expect("expected a Literal arm");
        assert!(literal_arm.variant_match.is_none());
        assert!(literal_arm.payload_bindings.is_empty());
    }

    // ── plain binding arm ─────────────────────────────────────────────────────

    #[test]
    fn binding_arm_records_binding_kind_no_variant_match() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) -> i64 {
    match x {
        n => n,
    }
}",
        );
        assert_eq!(resolutions.len(), 1);
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::Binding);
        assert!(arm.variant_match.is_none());
        assert!(arm.payload_bindings.is_empty());
    }

    // ── Option<T> arms ───────────────────────────────────────────────────────

    #[test]
    fn option_some_arm_records_variant_ctor_and_payload() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    match opt {
        Some(v) => v,
        None => 0,
    }
}",
        );
        assert_eq!(resolutions.len(), 2, "expected two arms");

        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("expected a Some arm");
        assert_eq!(some_arm.pattern_kind, PatternKind::VariantCtor);
        let vm = some_arm.variant_match.as_ref().unwrap();
        assert_eq!(vm.type_name, "Option");
        assert_eq!(vm.variant_name, "Some");
        assert_eq!(some_arm.payload_bindings.len(), 1);
        assert_eq!(some_arm.payload_bindings[0].binding_name, "v");
        assert_eq!(some_arm.payload_bindings[0].field_idx, 0);
        assert_eq!(some_arm.payload_bindings[0].ty, Ty::I64);

        let none_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "None")
            })
            .expect("expected a None arm");
        assert_eq!(none_arm.pattern_kind, PatternKind::VariantCtor);
        assert!(none_arm.payload_bindings.is_empty());
    }

    #[test]
    fn option_some_wildcard_payload_emits_no_binding() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) {
    match opt {
        Some(_) => {},
        None => {},
    }
}",
        );
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("expected a Some arm");
        // Wildcard sub-pattern emits no PayloadBinding
        assert!(
            some_arm.payload_bindings.is_empty(),
            "wildcard payload should not emit a PayloadBinding"
        );
    }

    // ── Result<T, E> arms ────────────────────────────────────────────────────

    #[test]
    fn result_ok_err_arms_record_variant_match() {
        let resolutions = pattern_resolutions(
            r"
fn foo(r: Result<i64, string>) -> i64 {
    match r {
        Ok(v) => v,
        Err(_) => -1,
    }
}",
        );
        assert_eq!(resolutions.len(), 2);

        let ok_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Ok")
            })
            .expect("expected an Ok arm");
        let vm = ok_arm.variant_match.as_ref().unwrap();
        assert_eq!(vm.type_name, "Result");
        assert_eq!(vm.variant_name, "Ok");
        assert_eq!(ok_arm.payload_bindings.len(), 1);
        assert_eq!(ok_arm.payload_bindings[0].binding_name, "v");
        assert_eq!(ok_arm.payload_bindings[0].ty, Ty::I64);

        let err_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Err")
            })
            .expect("expected an Err arm");
        // Wildcard payload — no binding recorded
        assert!(err_arm.payload_bindings.is_empty());
    }

    // ── user enum arms ───────────────────────────────────────────────────────

    #[test]
    fn user_enum_unit_variant_records_variant_ctor_no_payload() {
        let resolutions = pattern_resolutions(
            r"
enum Color { Red; Green; Blue }
fn foo(c: Color) {
    match c {
        Red => {},
        Green => {},
        Blue => {},
    }
}",
        );
        assert_eq!(resolutions.len(), 3);
        for arm in resolutions.values() {
            assert_eq!(arm.pattern_kind, PatternKind::VariantCtor);
            assert!(arm.variant_match.is_some());
            assert_eq!(arm.variant_match.as_ref().unwrap().type_name, "Color");
            assert!(arm.payload_bindings.is_empty());
        }
    }

    #[test]
    fn user_enum_tuple_variant_records_payload_bindings() {
        let resolutions = pattern_resolutions(
            r"
enum Shape { Circle(i64); Square(i64) }
fn foo(s: Shape) -> i64 {
    match s {
        Circle(r) => r,
        Square(side) => side,
    }
}",
        );
        assert_eq!(resolutions.len(), 2);

        let circle_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Circle")
            })
            .expect("expected Circle arm");
        assert_eq!(circle_arm.payload_bindings.len(), 1);
        assert_eq!(circle_arm.payload_bindings[0].binding_name, "r");
        assert_eq!(circle_arm.payload_bindings[0].field_idx, 0);
        assert_eq!(circle_arm.payload_bindings[0].ty, Ty::I64);
    }

    // ── match expression (not statement) ────────────────────────────────────

    #[test]
    fn match_expression_arm_records_resolution() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    let v = match opt {
        Some(x) => x,
        None => 0,
    };
    v
}",
        );
        assert_eq!(resolutions.len(), 2);
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("match expression must also record Some arm");
        assert_eq!(some_arm.payload_bindings.len(), 1);
        assert_eq!(some_arm.payload_bindings[0].binding_name, "x");
    }

    // ── tuple pattern ────────────────────────────────────────────────────────

    #[test]
    fn tuple_pattern_records_tuple_kind_and_bindings() {
        let resolutions = pattern_resolutions(
            r"
fn foo(pair: (i64, i64)) -> i64 {
    match pair {
        (a, b) => a + b,
    }
}",
        );
        assert_eq!(resolutions.len(), 1);
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::TuplePattern);
        assert!(arm.variant_match.is_none());
        assert_eq!(arm.payload_bindings.len(), 2);
        assert_eq!(arm.payload_bindings[0].binding_name, "a");
        assert_eq!(arm.payload_bindings[0].field_idx, 0);
        assert_eq!(arm.payload_bindings[1].binding_name, "b");
        assert_eq!(arm.payload_bindings[1].field_idx, 1);
    }

    #[test]
    fn record_pattern_uses_declaration_order_for_field_indices() {
        let resolutions = pattern_resolutions(
            r"
type Weird {
    z: i64,
    a: i64,
}

fn foo(w: Weird) -> i64 {
    match w {
        Weird { z, a } => z - a,
    }
}",
        );
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::StructPattern);
        assert_eq!(arm.payload_bindings.len(), 2);
        assert_eq!(arm.payload_bindings[0].binding_name, "z");
        assert_eq!(arm.payload_bindings[0].field_idx, 0);
        assert_eq!(arm.payload_bindings[1].binding_name, "a");
        assert_eq!(arm.payload_bindings[1].field_idx, 1);
    }

    #[test]
    fn record_match_omitted_field_fails_closed_until_rest_patterns() {
        let output = check_source(
            r"
type Point {
    x: i64,
    y: i64,
}

fn foo(p: Point) -> i64 {
    match p {
        Point { x } => x,
    }
}",
        );
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::InvalidOperation
                    && error.message.contains("omits field(s) y")
                    && error.message.contains("rest patterns")
            }),
            "expected omitted-field rest-pattern diagnostic, got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_match_literal_subpattern_is_supported() {
        let output = check_source(
            r"
fn foo(pair: (i64, i64)) -> i64 {
    match pair {
        (0, y) => y,
        _ => 0,
    }
}",
        );
        assert!(
            output.errors.is_empty(),
            "tuple literal subpattern must be accepted, got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn record_match_literal_subpattern_is_supported() {
        let output = check_source(
            r"
type Point {
    x: i64,
    y: i64,
}

fn foo(p: Point) -> i64 {
    match p {
        Point { x: 0, y } => y,
        _ => 0,
    }
}",
        );
        assert!(
            output.errors.is_empty(),
            "record literal subpattern must be accepted, got: {:#?}",
            output.errors
        );
    }

    // ── or-pattern is absent ─────────────────────────────────────────────────

    #[test]
    fn or_pattern_arm_absent_from_side_table() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) {
    match x {
        1 | 2 => {},
        _ => {},
    }
}",
        );
        // The or-pattern arm must be absent; only the wildcard arm is recorded.
        assert_eq!(
            resolutions.len(),
            1,
            "or-pattern arm must not appear in pattern_resolutions"
        );
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::Wildcard);
    }

    // ── payload types resolved at output boundary ────────────────────────────

    #[test]
    fn payload_binding_type_is_concrete_not_inferred() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    match opt {
        Some(v) => v,
        None => 0,
    }
}",
        );
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .unwrap();
        // Ty::Var must not survive the output boundary
        assert!(
            !matches!(some_arm.payload_bindings[0].ty, Ty::Var(_)),
            "payload binding type must be concrete, not Ty::Var"
        );
        assert_eq!(some_arm.payload_bindings[0].ty, Ty::I64);
    }

    #[test]
    fn struct_variant_records_source_order_field_idx() {
        // Variant declared with fields in order: c, a, b.
        // A pattern matching only `a` must record field_idx == 1
        // (the declaration position), not field_idx == 0 (alphabetical
        // position of "a" among ["a","b","c"]).
        let resolutions = pattern_resolutions(
            r"
enum Tri { Bar { c: i64; a: i64; b: i64 } }
fn foo(t: Tri) -> i64 {
    match t {
        Tri::Bar { a } => a,
    }
}",
        );
        let bar_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Bar")
            })
            .expect("Bar arm must be recorded");
        assert_eq!(bar_arm.payload_bindings.len(), 1);
        assert_eq!(bar_arm.payload_bindings[0].binding_name, "a");
        // Declaration order: c=0, a=1, b=2 — so field_idx for 'a' must be 1.
        assert_eq!(
            bar_arm.payload_bindings[0].field_idx, 1,
            "field_idx must reflect declaration order, not alphabetical order"
        );
    }

    // ── Issue #2116: resolution-based constructor vs binder detection ─────────

    #[test]
    fn uppercase_binder_not_constructor_records_binding_kind() {
        // An uppercase plain identifier that does NOT resolve to a unit variant
        // of the scrutinee type must be classified as `PatternKind::Binding`,
        // not as `PatternKind::VariantCtor`. The old case heuristic mis-classified
        // `INF` as a constructor and then failed to resolve it.
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) -> i64 {
    match x {
        INF => INF,
    }
}",
        );
        assert_eq!(resolutions.len(), 1);
        let arm = resolutions.values().next().unwrap();
        assert_eq!(
            arm.pattern_kind,
            PatternKind::Binding,
            "uppercase binder `INF` against non-enum type must record Binding, not VariantCtor"
        );
        assert!(
            arm.variant_match.is_none(),
            "uppercase binder must not have a variant_match"
        );
    }

    #[test]
    fn known_unit_variant_still_records_ctor_kind() {
        // Ensure the resolution-based fix does NOT accidentally reclassify a
        // genuine unit-variant constructor (`None`) as a binder when it DOES
        // resolve against the scrutinee type.
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    match opt {
        None => 0,
        Some(v) => v,
    }
}",
        );
        let none_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "None")
            })
            .expect("None arm must be recorded with a variant_match");
        assert_eq!(
            none_arm.pattern_kind,
            PatternKind::VariantCtor,
            "`None` against Option<i64> must record VariantCtor"
        );
    }

    #[test]
    fn uppercase_binder_in_payload_position_records_as_binding() {
        // An uppercase name in a constructor-payload slot that does NOT resolve
        // as a variant of the payload type must become a plain PayloadBinding
        // rather than a failed nested-constructor attempt.
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    match opt {
        Some(MAX) => MAX,
        None => 0,
    }
}",
        );
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("Some arm must be recorded");
        assert_eq!(some_arm.payload_bindings.len(), 1);
        assert_eq!(
            some_arm.payload_bindings[0].binding_name, "MAX",
            "uppercase payload binder MAX must appear in payload_bindings"
        );
        assert!(
            some_arm.payload_variant_patterns.is_empty(),
            "uppercase binder MAX must not be recorded as a nested constructor"
        );
    }

    #[test]
    fn plain_project_literal_subpatterns_are_admitted() {
        let output = check_source(
            r"
type Point { x: i64, y: i64 }
fn tuple_case(t: (i64, i64)) -> i64 {
    match t {
        (0, y) => y,
        (x, y) => x + y,
    }
}
fn record_case(p: Point) -> i64 {
    match p {
        Point { x: 0, y } => y,
        Point { x, y } => x + y,
    }
}",
        );
        assert!(
            output.errors.is_empty(),
            "literal tuple/record project predicates must type-check: {:#?}",
            output.errors
        );
    }

    #[test]
    fn top_level_uppercase_binder_over_option_compiles() {
        // Concrete repro for issue #2116 follow-up: a bare uppercase identifier
        // that does NOT resolve as a variant of the scrutinee type (here
        // `Option<i64>` has variants `Some` / `None`, neither of which is `INF`)
        // must be treated as a binding catch-all, making the match exhaustive.
        // The old code used a casing heuristic which caused a false
        // NonExhaustiveMatch error for this shape.
        let output = check_source(
            r"fn foo(opt: Option<i64>) -> i64 {
    match opt { INF => 0 }
}",
        );
        assert!(
            output.errors.is_empty(),
            "match opt {{ INF => 0 }} with uppercase binder must compile without errors; got: {:#?}",
            output.errors
        );
    }

    // ── Struct-variant field position binders (#2116 closing sweep) ──────────

    #[test]
    fn struct_variant_field_uppercase_binder_compiles() {
        // The concrete repro from issue #2116:
        //   enum Packet { Data { value: i64 }; Empty }
        //   match p { Packet::Data { value: MAX } => MAX, Empty => 0 }
        //
        // `MAX` does not resolve as a variant of `i64`, so it must be classified
        // as a binder rather than a "nested constructor".  The old casing heuristic
        // in `unsupported_payload_subpattern_label` caused a false
        // `UnsupportedPayloadSubpattern` error for this shape.
        let output = check_source(
            r"
enum Packet { Data { value: i64 }; Empty }
fn f(p: Packet) -> i64 {
    match p {
        Packet::Data { value: MAX } => MAX,
        Empty => 0,
    }
}",
        );
        assert!(
            output.errors.is_empty(),
            "struct-variant field uppercase binder `MAX` must compile without errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn struct_variant_field_real_constructor_still_rejected() {
        // Soundness guard: a bare identifier that DOES resolve as a variant of the
        // field type is still a nested constructor and must be rejected.
        // `Packet::Data { value: Red }` where `Red` is a unit variant of `Color`
        // must remain an `UnsupportedPayloadSubpattern` error.
        let output = check_source(
            r"
enum Color { Red; Blue }
enum Packet { Data { value: Color }; Empty }
fn f(p: Packet) -> i64 {
    match p {
        Packet::Data { value: Red } => 1,
        Packet::Data { value: _ } => 2,
        Empty => 0,
    }
}",
        );
        let has_unsupported = output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                    kind_label,
                    ..
                } if kind_label == "nested constructor"
            )
        });
        assert!(
            has_unsupported,
            "enum variant `Red` in struct field position must remain UnsupportedPayloadSubpattern; \
             got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn struct_variant_field_qualified_binder_rejected() {
        // A path-qualified identifier like `Packet::Empty` in struct field position
        // is always a constructor path regardless of resolution.
        let output = check_source(
            r"
enum Packet { Data { value: i64 }; Empty }
fn f(p: Packet) -> i64 {
    match p {
        Packet::Data { value: Packet::Empty } => 0,
        _ => 1,
    }
}",
        );
        let has_unsupported = output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                crate::error::TypeErrorKind::UnsupportedPayloadSubpattern { .. }
            )
        });
        assert!(
            has_unsupported,
            "qualified identifier in struct-variant field position must be UnsupportedPayloadSubpattern; \
             got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn record_field_uppercase_binder_compiles() {
        // A plain record (not an enum struct-variant) with an uppercase binder in
        // a field subpattern position must also be accepted after the casing fix.
        let output = check_source(
            r"
type Point { x: i64; y: i64 }
fn f(p: Point) -> i64 {
    match p {
        Point { x: MAX, y: _ } => MAX,
    }
}",
        );
        assert!(
            output.errors.is_empty(),
            "plain record field uppercase binder `MAX` must compile without errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_pattern_uppercase_binder_compiles() {
        // An uppercase binder in a tuple destructure element position must be
        // accepted after the casing fix.
        let output = check_source(
            r"
fn f(pair: (i64, i64)) -> i64 {
    match pair {
        (A, B) => A,
    }
}",
        );
        assert!(
            output.errors.is_empty(),
            "tuple pattern uppercase binders `A`, `B` must compile without errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn struct_variant_non_exhaustive_still_errors() {
        // Soundness guard: dropping a real variant arm must still produce a
        // NonExhaustiveMatch error even after the casing-to-resolution migration.
        // An uppercase binder must NOT make an otherwise non-exhaustive match pass.
        let output = check_source(
            r"
enum Packet { Data { value: i64 }; Empty }
fn f(p: Packet) -> i64 {
    match p {
        Packet::Data { value: MAX } => MAX,
        // Empty arm intentionally omitted
    }
}",
        );
        let has_non_exhaustive = output
            .errors
            .iter()
            .any(|e| matches!(e.kind, crate::error::TypeErrorKind::NonExhaustiveMatch));
        assert!(
            has_non_exhaustive,
            "match missing `Empty` arm must still emit NonExhaustiveMatch; got: {:#?}",
            output.errors
        );
    }
}

// ── Unsupported payload subpatterns (fail-closed gate) ──────────────────────
//
// These tests verify that the checker emits `UnsupportedPayloadSubpattern`
// rather than silently lowering unsupported payload subpatterns as wildcards.

/// Literal in tuple-variant payload position is accepted by the checker and
/// carried forward for HIR/MIR predicate handling rather than being rejected at
/// the parser/checker boundary.
#[test]
fn constructor_payload_literal_is_accepted() {
    let output = check_source(
        r"
enum Shape { Line(i64); Square(i64) }
fn main() -> i64 {
    let s = Shape::Line(2);
    match s {
        Shape::Line(1) => 999,
        Shape::Line(x) => x,
        Shape::Square(_) => 0,
    }
}",
    );
    assert!(
        !output.errors.iter().any(|e| matches!(
            &e.kind,
            crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                kind_label,
                ..
            } if kind_label == "literal"
        )),
        "literal payload subpatterns must not emit UnsupportedPayloadSubpattern; got errors: {:#?}",
        output.errors
    );
}

/// Nested constructor in tuple-variant payload is accepted and recorded as a
/// `PayloadVariantPattern` in the arm's resolution side-table entry.
#[test]
fn constructor_payload_nested_ctor_is_accepted_and_recorded() {
    let output = check_source(
        r"
enum Color { Red; Green }
enum Shape { Line(Color); Square(i64) }
fn main() -> i64 {
    let s = Shape::Line(Color::Red);
    match s {
        Shape::Line(Color::Red) => 1,
        Shape::Line(_) => 0,
        Shape::Square(_) => 0,
    }
}",
    );
    assert!(
        output.errors.is_empty(),
        "nested constructor payload subpattern must be accepted; got errors: {:#?}",
        output.errors
    );
    let nested: Vec<_> = output
        .pattern_resolutions
        .values()
        .flat_map(|resolution| resolution.payload_variant_patterns.iter())
        .collect();
    assert_eq!(
        nested.len(),
        1,
        "exactly one arm carries a nested constructor subpattern"
    );
    let pvp = nested[0];
    assert_eq!(pvp.field_idx, 0);
    assert_eq!(pvp.variant_match.type_name, "Color");
    assert_eq!(pvp.variant_match.variant_name, "Red");
    assert!(pvp.bindings.is_empty());
    assert!(pvp.nested.is_empty());
}

/// A nested constructor that binds from the inner payload (`Ok(Ok(v))`)
/// records the inner binding on the nested pattern, not on the arm.
#[test]
fn constructor_payload_nested_ctor_inner_binding_recorded() {
    let output = check_source(
        r"
fn doubly() -> Result<Result<i64, string>, string> {
    Ok(Ok(42))
}
fn main() -> i64 {
    match doubly() {
        Ok(Ok(v)) => v,
        Ok(Err(e)) => 0 - 1,
        Err(e) => 0 - 2,
    }
}",
    );
    assert!(
        output.errors.is_empty(),
        "nested Result patterns must be accepted; got errors: {:#?}",
        output.errors
    );
    let inner_ok = output
        .pattern_resolutions
        .values()
        .flat_map(|resolution| resolution.payload_variant_patterns.iter())
        .find(|pvp| pvp.variant_match.variant_name == "Ok")
        .expect("Ok(Ok(v)) arm records a nested Ok pattern");
    assert_eq!(inner_ok.bindings.len(), 1);
    assert_eq!(inner_ok.bindings[0].binding_name, "v");
    assert_eq!(inner_ok.bindings[0].field_idx, 0);
}

/// Tuple destructure inside tuple-variant payload position is admitted for HIR lowering.
#[test]
fn constructor_payload_tuple_destructure_is_accepted() {
    let output = check_source(
        r"
enum Pair { Both((i64, i64)); None }
fn main() -> i64 {
    let p = Pair::Both((1, 2));
    match p {
        Pair::Both((a, b)) => a,
        Pair::None => 0,
    }
}",
    );
    assert!(
        output.errors.is_empty(),
        "tuple-in-payload destructure must be fully accepted (both HIR-supported \
         and exhaustiveness-credited) without an extra catch-all arm; got errors: {:#?}",
        output.errors
    );
}

/// Binding and wildcard payload subpatterns must remain accepted.
/// Guards against the rejection being too broad.
#[test]
fn constructor_payload_binding_and_wildcard_are_accepted() {
    let output = check_source(
        r"
enum Shape { Line(i64); Square(i64) }
fn foo(s: Shape) -> i64 {
    match s {
        Shape::Line(x) => x,
        Shape::Square(_) => 0,
    }
}",
    );
    assert!(
        !output.errors.iter().any(|e| matches!(
            &e.kind,
            crate::error::TypeErrorKind::UnsupportedPayloadSubpattern { .. }
        )),
        "binding and wildcard payload subpatterns must not emit UnsupportedPayloadSubpattern; \
         got errors: {:#?}",
        output.errors
    );
}

// ── Generic machine transition-body inference (Lane B S8 prerequisite) ────

/// Bare struct-state constructor in a generic machine transition body must
/// type-check when the machine has type params and the state has a generic
/// field.  `Faulted { error: event.error }` must resolve without errors.
#[test]
fn generic_machine_struct_state_bare_constructor_infers() {
    let output = check_source(
        r"
        machine Work<T> {
            events {
                Crash { code: i64; }
            }

            state Running { handle: T; }
            state Faulted { code: i64; }


            on Crash: Running => Faulted {
                Faulted { code: event.code }
            }
            on Crash: Faulted => Faulted {
                state
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "bare struct-state constructor in generic machine transition must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

/// Qualified struct-state constructor `Machine::State { … }` inside a
/// generic machine transition body must also type-check.
#[test]
fn generic_machine_struct_state_qualified_constructor_infers() {
    let output = check_source(
        r"
        machine Work<T> {
            events {
                Crash { code: i64; }
            }

            state Running { handle: T; }
            state Faulted { code: i64; }


            on Crash: Running => Faulted {
                Work::Faulted { code: event.code }
            }
            on Crash: Faulted => Faulted {
                state
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "qualified struct-state constructor in generic machine transition must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

/// Non-generic machine struct-state constructors must continue to work
/// (regression guard for the `synthesize`→`check_against` change).
#[test]
fn non_generic_machine_struct_state_constructor_regression_free() {
    let output = check_source(
        r"
        machine Door {
            events {
                OpenDoor { id: i64; }
                CloseDoor;
            }

            state Closed;
            state Opened { handle: i64; }


            on OpenDoor: Closed => Opened {
                Door::Opened { handle: event.id }
            }
            on CloseDoor: Opened => Closed {
                Closed
            }
            on OpenDoor: Opened => Opened {
                state
            }
            on CloseDoor: Closed => Closed {
                state
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "non-generic machine struct-state constructor must stay green (regression); \
         got errors: {:#?}",
        output.errors
    );
}

#[test]
fn machine_transition_self_field_reads_source_payload() {
    let output = check_source(
        r"
        machine Counter {
            events {
                Inc;
                Reset;
            }

            state Zero;
            state NonZero { value: i64; }


            on Inc: Zero => NonZero {
                NonZero { value: 1 }
            }
            on Inc: NonZero => NonZero reenter {
                NonZero { value: self.value + 1 }
            }
            on Reset: NonZero => Zero {
                Zero
            }
            on Reset: Zero => Zero reenter {
                Zero
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`self.field` inside a concrete machine transition must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

#[test]
fn machine_transition_bare_self_remains_rejected() {
    let output = check_source(
        r"
        machine Counter {
            events {
                Reset;
            }

            state Zero;
            state NonZero { value: i64; }

            on Reset: NonZero => Zero {
                self
            }
            on Reset: Zero => Zero reenter {
                Zero
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedVariable
                && error.message.contains("`self` is not a valid identifier")),
        "bare `self` must remain rejected outside `self.field`; got errors: {:#?}",
        output.errors
    );
}

/// `step()` on a concretely-typed generic machine instance must accept a
/// bare event name — the receiver's generic args must substitute through the
/// registered event param.
#[test]
fn generic_machine_step_bare_event_propagates_receiver_args() {
    let output = check_source(
        r"
        machine Work<T> {
            events {
                Initialise;
                Started { handle: T; }
            }

            state Created;
            state Running { handle: T; }


            on Initialise: Created => Created {
                Created
            }
            on Initialise: Running => Running {
                state
            }
            on Started: Created => Running {
                Running { handle: event.handle }
            }
            on Started: Running => Running {
                state
            }
        }
        fn main() {
            var w: Work<i64> = Created;
            w.step(Initialise);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "step() with bare unit event on generic machine must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

// ── C1 UAF guard: E_SUPERVISOR_PERMANENT_OWNED_HEAP ──────────────────────────
// Negative tests — the diagnostic MUST fire.
