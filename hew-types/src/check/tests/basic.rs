#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn test_arity_mismatch_too_many_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 2
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 1,
                        radix: IntRadix::Decimal,
                    }),
                    12..13,
                )),
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 2,
                        radix: IntRadix::Decimal,
                    }),
                    15..16,
                )),
            ],
            is_tail_call: false,
        },
        0..17,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn test_arity_mismatch_too_few_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 0
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![],
            is_tail_call: false,
        },
        0..13,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn typecheck_error_undefined_var() {
    let result = hew_parser::parse("fn main() -> i32 {\n    let x = undefined_var;\n    x\n}");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for undefined variable"
    );
}

#[test]
fn removed_alias_int_emits_suggestion_for_i64_or_isize() {
    let result = hew_parser::parse("fn main() { let x: int = 5; }");
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
        .find(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("int"))
        .expect("expected UndefinedType error for removed alias `int`");
    assert!(
        err.message.contains("i64") || err.message.contains("isize"),
        "diagnostic should suggest i64 or isize; got: {}",
        err.message
    );
}

#[test]
fn removed_alias_uint_emits_suggestion_for_u64_or_usize() {
    let result = hew_parser::parse("fn main() { let x: uint = 5; }");
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
        .find(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("uint"))
        .expect("expected UndefinedType error for removed alias `uint`");
    assert!(
        err.message.contains("u64") || err.message.contains("usize"),
        "diagnostic should suggest u64 or usize; got: {}",
        err.message
    );
}

#[test]
fn removed_alias_int_capital_is_hard_error_with_i64_suggestion() {
    let result = hew_parser::parse("fn main() { let x: Int = 5; }");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // `Int` is no longer accepted; it must produce a hard type error.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("Int")),
        "expected UndefinedType error for removed alias `Int`; got errors: {:?}",
        output.errors
    );
    // No warning about Int should be emitted — this is a hard error, not a
    // deprecation.  Other warnings (e.g. UnusedVariable for `x`) are fine.
    assert!(
        output
            .warnings
            .iter()
            .all(|w| !w.message.contains("Int") && !w.message.contains("deprecated")),
        "unexpected Int-related warning; got: {:?}",
        output.warnings
    );
    // The error message should suggest i64.
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("Int"))
        .unwrap();
    assert!(
        err.message.contains("i64") || err.message.contains("isize"),
        "diagnostic should suggest i64 or isize; got: {}",
        err.message
    );
}

#[test]
fn typecheck_error_type_mismatch() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 {\n",
        "    a + b\n",
        "}\n\n",
        "fn main() {\n",
        "    let result = add(\"hello\", \"world\");\n",
        "}"
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
        !output.errors.is_empty(),
        "expected type errors for mismatched argument types"
    );
}

// -----------------------------------------------------------------------
// Additional edge-case tests
// -----------------------------------------------------------------------

#[test]
fn test_string_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::String("hello".to_string())), 0..5);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::String);
}

#[test]
#[expect(
    clippy::approx_constant,
    reason = "testing that 3.14 parses as Float, not using it as PI"
)]
fn test_float_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Float(3.14)), 0..4);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::FloatLiteral);
}

#[test]
fn test_char_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Char('a')), 0..3);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::Char);
}

#[test]
fn typecheck_binary_op_type_mismatch() {
    let source = "fn main() -> i32 {\n    let x: i32 = 1;\n    let y: bool = true;\n    x + y\n}";
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
        "expected type error for i32 + bool"
    );
}

#[test]
fn record_equality_comparison_typechecks_when_structurally_eligible() {
    let source = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 2 };\n    if a == b {\n        println(\"equal\");\n    }\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "eligible record `==` should typecheck: {:#?}",
        output.errors
    );
}

/// `!=` is admitted for eligible records; ordering remains rejected.
#[test]
fn record_inequality_typechecks_and_ordering_is_rejected() {
    let source = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 2 };\n    let ne = a != b;\n    let lt = a < b;\n    let _ = ne;\n    let _ = lt;\n}";
    let output = check_source(source);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("`!=` on record type `Pt`")),
        "eligible record `!=` should typecheck: {:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message
                    .contains("`<` is not supported for record type `Pt`")),
        "expected refusal for record `<`: {:#?}",
        output.errors
    );
}

/// Fieldless enums are tag values; their `==` remains admitted so MIR/codegen
/// can lower the comparison to tag equality instead of tripping the aggregate
/// structural-equality gate.
#[test]
fn enum_equality_not_gated_by_record_comparison_refusal() {
    let source = "enum Colour {\n    Red;\n    Green;\n}\n\nfn main() -> bool {\n    let a = Colour::Red;\n    let b = Colour::Green;\n    a == b\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "enum `==` must not trip the record-comparison gate: {:#?}",
        output.errors
    );
}

#[test]
fn enum_ordering_reports_checker_diagnostic() {
    let source = "enum Colour {\n    Red;\n    Green;\n}\n\nfn main() {\n    let a = Colour::Red;\n    let b = Colour::Green;\n    let _ = a < b;\n}";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("`<` is not supported for enum `Colour`")),
        "expected checker refusal for enum ordering: {:#?}",
        output.errors
    );
}

#[test]
fn payload_enum_equality_typechecks_when_structurally_eligible() {
    let source = "enum Shape {\n    Circle(i64);\n    Empty;\n}\n\nfn main() {\n    let a = Circle(1);\n    let b = Circle(1);\n    let _ = a == b;\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "eligible payload enum `==` should typecheck: {:#?}",
        output.errors
    );
}

#[test]
fn builtin_payload_enum_comparison_typechecks_when_structurally_eligible() {
    let source = "fn main() {\n    let a: Option<i64> = Some(1);\n    let b: Option<i64> = Some(2);\n    let _ = a == b;\n    let ok: Result<i64, i64> = Ok(1);\n    let err: Result<i64, i64> = Err(2);\n    let _ = ok != err;\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "eligible builtin payload enum comparisons should typecheck: {:#?}",
        output.errors
    );
}

#[test]
fn record_with_bytes_field_eq_rejects_with_named_diagnostic() {
    let output = check_source(
        r"
        type Packet { data: bytes }

        fn same(a: Packet, b: Packet) -> bool {
            a == b
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("`==` on record type `Packet`")
                && e.message.contains("field or payload")
                && e.message.contains("layout-managed/non-Copy")
                && e.message.contains("bytes")
                && !e.message.contains("IntCmp")
        }),
        "managed record eq should fail closed with a named checker diagnostic: {:#?}",
        output.errors
    );
}

#[test]
fn record_with_string_field_eq_is_accepted() {
    let output = check_source(
        r"
        type Person { name: string }

        fn same(a: Person, b: Person) -> bool {
            a == b
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "string-backed record equality should be admitted for structural codegen: {:#?}",
        output.errors
    );
}

#[test]
fn managed_payload_enum_eq_rejects_with_named_diagnostic() {
    let output = check_source(
        r"
        fn same(a: Option<bytes>, b: Option<bytes>) -> bool {
            a == b
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::InvalidOperation
                && e.message
                    .contains("`==` on enum `Option<bytes>` with payload variants")
                && e.message.contains("field or payload")
                && e.message.contains("layout-managed/non-Copy")
                && e.message.contains("bytes")
                && !e.message.contains("IntCmp")
        }),
        "managed payload enum eq should fail closed with a named checker diagnostic: {:#?}",
        output.errors
    );
}

/// When the operand types disagree, the plain mismatch diagnostic wins;
/// the record gate must not double-report.
#[test]
fn record_comparison_type_mismatch_reports_mismatch_not_refusal() {
    let source = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() -> bool {\n    let a = Pt { x: 1, y: 2 };\n    a == 5\n}";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected a type mismatch error: {:#?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("not yet implemented")),
        "record gate must not fire on mismatched operands: {:#?}",
        output.errors
    );
}

#[test]
fn int_literal_locals_unify_to_concrete_integer_binary_width() {
    let source = r"
        fn step() -> i32 {
            return 10;
        }

        fn main() -> i32 {
            let target_value = 7;
            var total = 0;
            if step() == target_value {
                total = total + step();
            }
            return total;
        }
        ";
    let output = check_source(source);

    assert!(
        output.errors.is_empty(),
        "literal-seeded operands should infer the i32 binary width: {:#?}",
        output.errors
    );
    let literal_key = span_key_for(source, "7");
    assert_eq!(
        output.expr_types.get(&literal_key),
        Some(&Ty::I32),
        "literal-backed binding should resolve to the concrete i32 width"
    );
}

#[test]
fn concrete_integer_float_comparison_stays_rejected() {
    let output = check_source(
        r"
        fn bad(x: i32, y: f64) -> bool {
            return x == y;
        }
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("explicit conversion")),
        "expected i32 vs f64 comparison to require an explicit conversion: {:#?}",
        output.errors
    );
}

#[test]
fn integer_string_comparison_stays_rejected() {
    let output = check_source(
        r#"
        fn bad(x: i32) -> bool {
            return x == "7";
        }
        "#,
    );

    assert!(
        !output.errors.is_empty(),
        "expected i32 vs string comparison to be rejected"
    );
}

// ── Regression: literal-bound-to-local width propagation ──────────────────
//
// These tests guard the path where a literal is bound to a local variable via
// `let` or `var`, then compared/computed with a concrete-width integer.  The
// checker must propagate the concrete width back to the literal local so the
// HIR and MIR see matching widths at binary operation sites.
//
// Background: commit 53aa2a06 fixed the case where a literal is a direct
// binary operand; the `let target = 7` path works through the `const_values`
// table + `expect_inferable_literal_binding` machinery.  Without this path
// the literal materialises to I64 at binding time, causing
// `IntCmp{I32, I64}` or `IntArithChecked{I32, I64}` in MIR that the
// fail-closed codegen correctly rejects.
//
// This cluster also tests that the for-range loop variable adopts the correct
// element width (the checker infers the element type from the range bounds;
// hew-hir threads this through to the HIR binding so MIR locals match).

/// The let-bound literal path (`let target = 7; fn_returning_i32() == target`)
/// must produce no type errors and the literal must be recorded as I32.
/// Regression: if `infer_integer_literal_binding_type` stops creating a Var
/// for the literal, the 7 stays `IntLiteral` → `I64` at output and downstream
/// MIR sees `IntCmp{I32, I64}`.
#[test]
fn let_bound_literal_unifies_to_i32_width_when_compared_against_i32_fn() {
    let source = r"
        fn pick(x: i32) -> i32 { x }

        fn main() -> i32 {
            let target = 7;
            if pick(7) == target { 1 } else { 0 }
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "let-bound literal compared against i32 fn result must not error: {:#?}",
        output.errors
    );
    let literal_key = span_key_for(source, "7");
    // The first `7` in the source is `pick(7)` — skip to the `let target = 7`
    // literal which is the second occurrence.
    let second_7_pos = source[literal_key.end..]
        .find('7')
        .map(|p| p + literal_key.end);
    if let Some(pos) = second_7_pos {
        let key = SpanKey {
            start: pos,
            end: pos + 1,
            module_idx: 0,
        };
        let recorded = output.expr_types.get(&key);
        assert_eq!(
            recorded,
            Some(&Ty::I32),
            "let-bound literal should resolve to I32 via use-site context, got {recorded:?}"
        );
    }
}

/// `var` bindings with an untyped integer literal remain inferable (not
/// immediately materialised to I64) so that use-site context can narrow them.
/// Regression: if `var passed = 0` materialises to I64 before `passed +
/// fn_returning_i32()` constrains it, the arithmetic site gets
/// IntArithChecked{I64, I32} in MIR.
#[test]
fn var_bound_literal_unifies_to_i32_when_added_to_i32_result() {
    let source = r"
        fn count() -> i32 { 1 }
        fn main() -> i32 {
            var passed = 0;
            passed = passed + count();
            passed
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "var-bound literal should infer I32 via arithmetic context: {:#?}",
        output.errors
    );
}

#[test]
fn integer_literal_match_pattern_must_fit_scrutinee_width() {
    let output = check_source(
        r"
        fn classify(x: i8) -> i64 {
            match x {
                128 => 1,
                _ => 0,
            }
        }
    ",
    );
    assert!(
        output.errors.iter().any(|err| err
            .message
            .contains("does not fit in match scrutinee type `i8`")),
        "expected i8 match literal range error, got: {:#?}",
        output.errors
    );
}

/// `for i in 2 .. n + 1` with `n: i32` — the checker must infer the range
/// element type as I32 so that uses of `i` as a narrower operand don't get
/// widened to I64.  Regression: if the range element type defaults to I64
/// and the literal operands in the loop body are recorded as I32 via the
/// `n: i32` context, MIR sees IntArithChecked{I64, I32}.
///
/// This test validates the checker-level half of the invariant: passing `i`
/// to a function that accepts `i32` must not require explicit coercion.
/// The HIR/MIR threading of this type is tested end-to-end by the
/// `climbing_stairs` and `matrix_multiply` corpus files once the Vec ABI gap
/// is fixed.
#[test]
fn for_range_loop_var_infers_i32_from_i32_bound() {
    let source = r"
        fn take_i32(x: i32) -> i32 { x }
        fn main() -> i32 {
            let n: i32 = 5;
            var acc: i32 = 0;
            for i in 0 .. n {
                acc = acc + take_i32(i);
            }
            acc
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "for-range loop variable should infer i32 when bound is i32: {:#?}",
        output.errors
    );
}

/// Checker accepts mixed-width range bounds (i32..i64) and resolves the
/// loop variable to the WIDER type (i64), not the narrower start bound.
///
/// The checker's `common_integer_type(i32, i64)` chooses `i64`.  The range
/// type must be `Range<i64>` so HIR reads the correct element type.
#[test]
fn for_range_mixed_width_bounds_resolves_to_wider_type() {
    let source = r"
        fn id_i64(x: i64) -> i64 { x }
        fn main() -> i64 {
            let a: i32 = 2;
            let b: i64 = 6;
            var sum: i64 = 0;
            for i in a..b {
                sum = sum + id_i64(i);
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "mixed-width range bounds should resolve to the wider type: {:#?}",
        output.errors
    );
}

/// Checker accepts negative literal range bounds (`-5..5`) when the loop
/// variable is narrowed to i32 via context.
///
/// The deferred `TypeVar` for the range element type must be re-recorded for
/// both the outer (`-5`) span AND the inner literal (`5`) span so HIR
/// unary lowering sees matching operand/result widths.
#[test]
fn for_range_negative_literal_bound_accepted_at_i32() {
    let source = r"
        fn id_i32(x: i32) -> i32 { x }
        fn main() -> i32 {
            var sum: i32 = 0;
            for i in -5..5 {
                sum = sum + id_i32(i);
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "negative literal range bounds should be accepted at i32: {:#?}",
        output.errors
    );
}

/// Range `.rev()` is a `Range<T>` method: the checker accepts `(0..5).rev()`
/// as a for-loop iterable, deriving an `i64` element type from the literal
/// bounds exactly as a bare range does.
#[test]
fn for_range_rev_adapter_accepted() {
    let source = r"
        fn id_i64(x: i64) -> i64 { x }
        fn main() -> i64 {
            var sum: i64 = 0;
            for i in (0..5).rev() {
                sum = sum + id_i64(i);
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "(0..5).rev() should be accepted as a for-loop iterable: {:#?}",
        output.errors
    );
}

/// Range `.step_by(k)` is a `Range<T>` method returning `Range<T>`, so it
/// composes with `.rev()`: `(0..=10).rev().step_by(3)` checks clean.
#[test]
fn for_range_step_by_and_compose_accepted() {
    let source = r"
        fn id_i64(x: i64) -> i64 { x }
        fn main() -> i64 {
            var sum: i64 = 0;
            for i in (0..10).step_by(2) {
                sum = sum + id_i64(i);
            }
            for j in (0..=10).rev().step_by(3) {
                sum = sum + id_i64(j);
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        ".step_by and .rev().step_by composition should check clean: {:#?}",
        output.errors
    );
}

/// #1857 interaction guard: `.rev()` on a range whose bound element type is
/// still an unconstrained inference variable at method dispatch must default
/// through `deferred_range_bounds` to `i64`, never leave a `Ty::Var` hole that
/// breaks the loop-body arithmetic.  Mirrors
/// `for_range_loop_var_infers_i32_from_i32_bound` for the descending adapter.
#[test]
fn for_range_rev_defaults_unconstrained_bound_to_i64() {
    let source = r"
        fn main() -> i64 {
            var acc: i64 = 0;
            for i in (0..5).rev() {
                acc = acc + i;
            }
            acc
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "an unconstrained (0..5).rev() must default to i64 like a bare range: {:#?}",
        output.errors
    );
}

/// Fail-closed: a statically-zero `step_by(0)` is rejected at type-check time
/// (a zero stride would never advance the counter).
#[test]
fn for_range_step_by_zero_rejected() {
    let source = r"
        fn main() -> i64 {
            for i in (0..5).step_by(0) {
                let _ = i;
            }
            0
        }
    ";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("step_by") && e.message.contains("positive")),
        "step_by(0) must be rejected fail-closed: {:#?}",
        output.errors
    );
}

/// Fail-closed: a statically-negative `step_by(-2)` is rejected (a negative
/// stride is meaningless; `.rev()` is the descending form).
#[test]
fn for_range_step_by_negative_rejected() {
    let source = r"
        fn main() -> i64 {
            for i in (0..5).step_by(-2) {
                let _ = i;
            }
            0
        }
    ";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("step_by") && e.message.contains("positive")),
        "step_by(-2) must be rejected fail-closed: {:#?}",
        output.errors
    );
}

/// Range loop variables resolve before receiver-only numeric methods. Both bounds are
/// integer literals so the range element type starts as a fresh inference
/// variable; method dispatch must see a concrete i64.
#[test]
fn for_range_literal_bounds_loop_var_resolves_before_method_lookup() {
    let source = r"
        fn main() -> f64 {
            var acc: f64 = 0.0;
            for i in 0..8 {
                let _: Option<f64> = i.try_to_f64();
                acc = acc + 1.0;
            }
            acc
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "literal-bound range loop variable must resolve before method lookup: {:#?}",
        output.errors
    );
}

/// Const-bound range loop variables resolve before receiver-only numeric methods. A
/// const-integer bound is coercible, so the range element type is also a fresh
/// inference variable. The loop variable must resolve to i64 at method dispatch.
#[test]
fn for_range_const_bound_loop_var_resolves_before_method_lookup() {
    let source = r"
        const N: i64 = 8;
        fn main() -> f64 {
            var acc: f64 = 0.0;
            for i in 0..N {
                let _: Option<f64> = i.try_to_f64();
                acc = acc + 1.0;
            }
            acc
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "const-bound range loop variable must resolve before method lookup: {:#?}",
        output.errors
    );
}

/// Fail-closed: when both range bounds are integer literals and the loop
/// variable is only consumed via method calls (never passed to a function that
/// would narrow the width), the checker must resolve the variable to a
/// concrete integer type (i64) and never leave `Ty::Var` visible to the
/// codegen boundary.
#[test]
fn for_range_literal_bounds_method_only_body_resolves_to_i64() {
    // Both float widths are targeted through receiver-only methods; no
    // function-call use-site narrows the width. The loop variable must default
    // to i64.
    let source = r"
        fn main() -> f64 {
            var sum: f64 = 0.0;
            for i in 0..4 {
                let _: Option<f64> = i.try_to_f64();
                let _: Option<f32> = i.try_to_f32();
                sum = sum + 1.0;
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "literal-bound range: multiple method calls must all resolve to i64: {:#?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_signedness_change_in_call() {
    let source = concat!(
        "fn takes_u32(x: u32) -> u32 { x }\n",
        "fn main() -> u32 {\n",
        "    let n: i64 = 42;\n",
        "    takes_u32(n)\n",
        "}\n"
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
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot implicitly convert")
                && e.message.contains("i64")
                && e.message.contains("u32")),
        "expected integer-mismatch rejection diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_integer_to_float_in_call() {
    let source = concat!(
        "fn takes_f64(x: f64) -> f64 { x }\n",
        "fn main() -> f64 {\n",
        "    let n: i64 = 42;\n",
        "    takes_f64(n)\n",
        "}\n"
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
        output
            .errors
            .iter()
            .any(|e| e.message.contains("implicit numeric coercion")),
        "expected explicit coercion diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_integer_widening_in_call() {
    // Passing i32 where i64 is expected is an error; the caller must write
    // `takes_i64(n as i64)`.  Silent widening was removed because LLVM's
    // IR verifier rejects the resulting mistyped call instruction.
    let source = concat!(
        "fn takes_i64(x: i64) -> i64 { x }\n",
        "fn main() -> i64 {\n",
        "    let n: i32 = 42;\n",
        "    takes_i64(n)\n",
        "}\n"
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
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot implicitly convert")
                && e.message.contains("i32")
                && e.message.contains("i64")),
        "expected integer-widening rejection diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_return_type_mismatch() {
    // The type checker may not flag all return-type mismatches at the
    // trailing-expression level; verify the function signature is recorded.
    let source = "fn foo() -> i32 {\n    true\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // The function signature should still reflect i32 return type
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::I32);
}

#[test]
fn typecheck_trailing_return_stmt_matches_declared_type() {
    let source = "fn foo() -> i32 {\n    return 42;\n}";
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
fn typecheck_trailing_return_stmt_matches_declared_bool_type() {
    let source = "fn foo() -> bool {\n    return true;\n}";
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
fn typecheck_nested_function_calls() {
    let source = concat!(
        "fn double(x: i32) -> i32 { let two: i32 = 2; x * two }\n",
        "fn main() -> i32 { let x: i32 = 5; double(double(x)) }\n"
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
fn typecheck_let_with_explicit_type() {
    let source = "fn main() { let x: i32 = 42; }";
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
fn typecheck_let_type_annotation_mismatch() {
    let source = "fn main() { let x: i64 = \"hello\"; }";
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
        "expected type error for string assigned to i64 variable"
    );
    assert!(output
        .errors
        .iter()
        .any(|e| { e.message.contains("expected `i64`") && e.message.contains("found `string`") }));
}

#[test]
fn typecheck_if_branch_type_consistency() {
    let source =
        "fn main() -> i32 {\n    if true { let x: i32 = 1; x } else { let y: i32 = 2; y }\n}";
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
fn typecheck_vec_type_annotation() {
    let source = "fn main() { let v: Vec<i32> = Vec::new(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Vec::new() may or may not resolve depending on builtins, but should not panic
    assert!(output.errors.len() <= 2);
}

#[test]
fn unresolved_vec_new_method_chain_fails_closed() {
    let source = "fn main() { Vec::new().clear(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::InferenceFailed
                && err.message.contains("Vec element type")
                && err.message.contains("Vec<")
        }),
        "expected fail-closed Vec inference diagnostic, got errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_multiple_functions_cross_call() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 { a + b }\n",
        "fn mul(a: i32, b: i32) -> i32 { a * b }\n",
        "fn main() -> i32 { add(mul(2, 3), 4) }\n"
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
fn typecheck_actor_receive_fn_registered() {
    use hew_parser::ast::{ActorDecl, Block, Item, Param, ReceiveFnDecl, TypeExpr};

    let recv = ReceiveFnDecl {
        is_generator: false,
        name: "greet".to_string(),
        params: vec![Param {
            name: "name".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "string".into(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
            is_consume: false,
        }],
        return_type: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        type_params: None,
        where_clause: None,
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    };
    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "Greeter".to_string(),
        type_params: vec![],
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns: vec![recv],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
        max_heap_bytes: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.fn_sigs.contains_key("Greeter::greet"));
}

fn span_key_for(source: &str, needle: &str) -> SpanKey {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("missing `{needle}` in source"));
    SpanKey {
        start,
        end: start + needle.len(),
        module_idx: 0,
    }
}

#[test]
fn context_readers_typecheck_inside_receive_handler() {
    let source = "\
        actor Worker {
            receive fn ping() {
                let actor_value = @actor_id;
                let supervisor_value = @supervisor;
                let span_value = @trace_span;
            }
        }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output.expr_types.get(&span_key_for(source, "@actor_id")),
        Some(&Ty::U64)
    );
    assert_eq!(
        output.expr_types.get(&span_key_for(source, "@trace_span")),
        Some(&Ty::U64)
    );
    assert_eq!(
        output.expr_types.get(&span_key_for(source, "@supervisor")),
        Some(&Ty::Pointer {
            is_mutable: true,
            pointee: Box::new(Ty::Unit),
        })
    );
}

#[test]
fn context_reader_outside_handler_is_typed_diagnostic() {
    let output = check_source("fn main() -> u64 { @actor_id }");
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ContextReaderOutsideHandler),
        "{:?}",
        output.errors
    );
}

#[test]
fn context_reader_in_non_actor_lambda_is_typed_diagnostic() {
    let source = "\
        actor Worker {
            receive fn ping() {
                let f = || @actor_id;
            }
        }";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ContextReaderOutsideHandler),
        "{:?}",
        output.errors
    );
}

/// `#[max_heap(N)]` on an actor → `actor_max_heap` side-table entry for that actor.
#[test]
fn max_heap_attribute_populates_side_table() {
    let source = "#[max_heap(4096)] actor Cache { receive fn get() {} }";
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
        "unexpected type errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.actor_max_heap.get("Cache"),
        Some(&4096u64),
        "actor_max_heap must record the parsed cap for Cache"
    );
}

/// Actor without `#[max_heap]` must not appear in the side-table.
#[test]
fn max_heap_absent_actor_not_in_side_table() {
    let source = "actor Plain { receive fn tick() {} }";
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
        "unexpected type errors: {:?}",
        output.errors
    );
    assert!(
        !output.actor_max_heap.contains_key("Plain"),
        "actor without #[max_heap] must not appear in actor_max_heap"
    );
}

/// `#[max_heap(2 mb)]` — suffix conversion done by the parser, checker sees bytes.
#[test]
fn max_heap_mb_suffix_populates_side_table_as_bytes() {
    let source = "#[max_heap(2 mb)] actor Big { receive fn work() {} }";
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
        "unexpected type errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.actor_max_heap.get("Big"),
        Some(&(2u64 * 1024 * 1024)),
        "2 mb must be recorded as 2_097_152 bytes"
    );
}

#[test]
fn typecheck_empty_function_no_error() {
    let source = "fn noop() {}";
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty());
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_recursive_function() {
    let source = concat!(
        "fn factorial(n: i32) -> i32 {\n",
        "    let one: i32 = 1; if n <= one { one } else { n * factorial(n - one) }\n",
        "}\n"
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
fn typecheck_local_result_enum_not_qualified_to_sqlite() {
    let source = concat!(
        "import ecosystem::db::sqlite;\n",
        "enum Result {\n",
        "    Ok(i64);\n",
        "    Err(i64)\n",
        "}\n",
        "fn unwrap_or(r: Result, fallback: i64) -> i64 {\n",
        "    match r {\n",
        "        Ok(v) => v,\n",
        "        Err(_) => fallback,\n",
        "    }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Filter out the expected UnresolvedImport for the dummy stdlib import — the
    // test is about local type naming, not module resolution.
    let non_import_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind != TypeErrorKind::UnresolvedImport)
        .collect();
    assert!(
        non_import_errors.is_empty(),
        "unexpected errors: {non_import_errors:?}"
    );
    let sig = output
        .fn_sigs
        .get("unwrap_or")
        .expect("unwrap_or signature should be registered");
    assert_eq!(
        sig.params[0],
        Ty::Named {
            builtin: None,
            name: "Result".to_string(),
            args: vec![],
        }
    );
}

// --- #2520: reserved type names (primitives + structural encoder heads) ---

#[test]
fn reserved_primitive_type_name_i64_rejected() {
    // `type i64 { ... }` collides with the primitive spelling. The
    // type-fragment encoder leaves primitive fragments unchanged, so the
    // checker must reject the declaration rather than treat it as a mangling
    // concern.
    let output = check_source("type i64 { value: i64; }\nfn main() -> i64 { return 0; }");
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ReservedTypeName && e.message.contains("i64")),
        "expected ReservedTypeName citing `i64`; got: {:?}",
        output.errors
    );
}

#[test]
fn reserved_structural_head_tuple_rejected() {
    // `type tuple<T> { ... }` collides with the structural encoder head
    // `tuple$x...$g`.
    let output = check_source("type tuple<T> { value: T; }\nfn main() -> i64 { return 0; }");
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ReservedTypeName && e.message.contains("tuple")),
        "expected ReservedTypeName citing `tuple`; got: {:?}",
        output.errors
    );
}

#[test]
fn reserved_type_name_rejected_across_declaration_kinds() {
    // The gate lives in the shared namespace-registration chokepoint, so
    // `record`/`actor` declarations are rejected the same way `type` is.
    for (src, needle) in [
        (
            "record string { a: i64 }\nfn main() -> i64 { return 0; }",
            "string",
        ),
        (
            "actor bytes {\n  receive fn ping() {}\n}\nfn main() -> i64 { return 0; }",
            "bytes",
        ),
    ] {
        let output = check_source(src);
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::ReservedTypeName && e.message.contains(needle)),
            "expected ReservedTypeName citing `{needle}` for source `{src}`; got: {:?}",
            output.errors
        );
    }
}

#[test]
fn non_reserved_type_names_still_accepted() {
    // Names that merely resemble but do not equal a reserved spelling must
    // still be accepted (no over-fire).
    let output = check_source(
        "type Point { x: i64; y: i64; }\n\
         type Tuple { a: i64; }\n\
         type MyString { s: i64; }\n\
         enum Colour { Red; Green; Blue; }\n\
         fn main() -> i64 { return 0; }",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::ReservedTypeName),
        "no ReservedTypeName expected for non-reserved names; got: {:?}",
        output.errors
    );
}
