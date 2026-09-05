#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn contextual_variants_resolve_only_from_the_expected_type() {
    let output = check_source(
        r"
enum Choice { Some(i64), None }

fn choose(flag: bool) -> Choice {
    if flag { .Some(7) } else { .None }
}

fn read(value: Choice) -> i64 {
    match value {
        .Some(number) => number,
        .None => 0,
    }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "context-selected expression and pattern variants must check: {:#?}",
        output.errors
    );
}

#[test]
fn contextual_variant_without_expected_enum_is_rejected() {
    let output = check_source("fn main() { let value = .None; }");
    assert!(output.errors.iter().any(|error| {
        error.kind == TypeErrorKind::ContextVariantNoType
            && error.message.contains("E_CONTEXT_VARIANT_NO_TYPE")
    }));
}

#[test]
fn contextual_variant_missing_from_expected_enum_is_rejected() {
    let output = check_source("enum Choice { Ready } fn make() -> Choice { .Missing }");
    assert!(output.errors.iter().any(|error| {
        error.kind == TypeErrorKind::PathMemberNotFound
            && error.message.contains("E_PATH_MEMBER_NOT_FOUND")
    }));
}

#[test]
fn contextual_variant_reports_ambiguous_expected_owner() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .published_bare_type_owners
        .entry((None, 0, "State".to_string()))
        .or_default()
        .extend(["left.State".to_string(), "right.State".to_string()]);

    assert!(checker
        .context_variant_expected_owner(&Ty::named("State", vec![]), &(0..6))
        .is_none());
    assert!(checker.errors.iter().any(|error| {
        error.kind == TypeErrorKind::ContextVariantAmbiguous
            && error.message.contains("E_CONTEXT_VARIANT_AMBIGUOUS")
    }));
}

/// Both spellings are refused since v0.6.0. The pattern form covers every
/// place a variant can be matched: a unit arm, a tuple arm, a struct-variant
/// arm, and a `let`-position tag test.
#[test]
fn bare_variant_patterns_error_in_every_pattern_position() {
    let output = check_source(
        r"
enum Choice { Present(i64), Absent, Named { value: i64 } }
fn make() -> Choice { Present(7) }
fn read(value: Choice) -> i64 {
    match value { Present(number) => number, Named { value } => value, Absent => 0 }
}
fn tag_test(value: Choice) -> i64 {
    let Absent = value else { return 1 };
    0
}
",
    );
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::BareVariantExpr));
    let pattern_names = output
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BareVariantPattern)
        .flat_map(|error| error.suggestions.iter())
        .cloned()
        .collect::<Vec<_>>();
    for expected in [
        "replace `Present` with `.Present`",
        "replace `Named` with `.Named`",
        "replace `Absent` with `.Absent`",
    ] {
        assert!(
            pattern_names.iter().any(|s| s == expected),
            "missing fix-it `{expected}`, got: {pattern_names:#?}"
        );
    }
    // Negative control: the deprecation path is gone, so nothing may report
    // either bare-variant spelling at warning severity outside migration mode.
    assert!(output.warnings.iter().all(|warning| !matches!(
        warning.kind,
        TypeErrorKind::BareVariantPattern | TypeErrorKind::BareVariantExpr
    )));
}

/// A dotted pattern in the same positions is the language and must check
/// clean — the positive control for
/// `bare_variant_patterns_error_in_every_pattern_position`.
#[test]
fn dotted_variant_patterns_check_in_every_pattern_position() {
    let output = check_source(
        r"
enum Choice { Present(i64), Absent, Named { value: i64 } }
fn read(value: Choice) -> i64 {
    match value { .Present(number) => number, .Named { value } => value, .Absent => 0 }
}
fn tag_test(value: Choice) -> i64 {
    let .Absent = value else { return 1 };
    0
}
",
    );
    assert!(
        output.errors.is_empty(),
        "dotted variant patterns must check: {:#?}",
        output.errors
    );
}

/// A record destructure shares `Pattern::Struct` with a struct-variant
/// pattern. Its name is a type, not a variant, so it must not be refused.
#[test]
fn record_destructure_is_not_a_bare_variant_pattern() {
    let output = check_source(
        r"
type Point { x: i64, y: i64 }
fn sum(p: Point) -> i64 {
    match p { Point { x, y } => x + y }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "record destructure must check: {:#?}",
        output.errors
    );
}

/// The migrator resolves its rewrites from the checker's own output, so the
/// downgraded severity must still carry the same kind and suggestions — for
/// the pattern form as much as the expression form.
#[test]
fn migration_mode_downgrades_both_bare_variant_spellings_to_warnings() {
    let parse_result = hew_parser::parse(
        r"
enum Choice { Present(i64), Absent }
fn contextual() -> Choice { Present(7) }
fn read(value: Choice) -> i64 {
    match value { Present(number) => number, Absent => 0 }
}
",
    );
    assert!(
        parse_result.errors.is_empty(),
        "migration fixture should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.set_migration_mode();
    let output = checker.check_program(&parse_result.program);
    assert!(
        output.errors.is_empty(),
        "migration mode must still type-check a legacy source: {:#?}",
        output.errors
    );
    for kind in [
        TypeErrorKind::BareVariantExpr,
        TypeErrorKind::BareVariantPattern,
    ] {
        assert!(
            output.warnings.iter().any(|warning| warning.kind == kind),
            "migration mode must report {kind:?} as a warning: {:#?}",
            output.warnings
        );
    }
}

#[test]
fn bare_variant_expression_suggestions_preserve_expected_type_context() {
    let output = check_source(
        r"
enum Choice { Present(i64) }
fn contextual() -> Choice { Present(7) }
fn inferred() { let value = Present(9); }
",
    );
    let suggestions = output
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BareVariantExpr)
        .flat_map(|error| error.suggestions.iter())
        .collect::<Vec<_>>();
    assert!(suggestions
        .iter()
        .any(|suggestion| suggestion.contains("with `.Present`")));
    assert!(suggestions
        .iter()
        .any(|suggestion| suggestion.contains("with `Choice.Present`")));
}

#[test]
fn dotted_owner_variants_typecheck_in_expression_position() {
    let output = check_source(
        r"
enum Choice { Present(i64), Absent }
fn tuple() -> Choice { Choice.Present(7) }
fn unit() -> Choice { Choice.Absent }
",
    );
    assert!(
        output.errors.is_empty(),
        "dotted owner variants must typecheck: {:#?}",
        output.errors
    );
}

#[test]
fn dotted_struct_variant_typechecks_in_expression_position() {
    let output = check_source(
        r"
enum Choice { Named { value: i64 } }
fn make() -> Choice { Choice.Named { value: 7 } }
",
    );
    assert!(
        output.errors.is_empty(),
        "dotted struct variant must typecheck: {:#?}",
        output.errors
    );
}

#[test]
fn dotted_associated_calls_resolve_without_using_the_head_as_a_value() {
    let output = check_source(
        r#"
fn main() {
    let values: Vec<i64> = Vec.new();
    Node.start("127.0.0.1:0");
}
"#,
    );
    assert!(
        output.errors.is_empty(),
        "dotted associated and namespace calls must check: {:#?}",
        output.errors
    );
}

#[test]
fn uppercase_pattern_binding_is_not_classified_as_a_variant() {
    let output = check_source(
        r"
fn read(value: i64) -> i64 {
    match value { Value => Value }
}
",
    );
    assert!(output.errors.is_empty());
    assert!(output
        .errors
        .iter()
        .chain(output.warnings.iter())
        .all(|diagnostic| diagnostic.kind != TypeErrorKind::BareVariantPattern));
}

#[test]
fn prelude_declarations_are_protected_before_source_registration() {
    let output = check_source("type Iterator { value: i64, }");
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::PreludeDeclCollision));
}

#[test]
fn non_root_prelude_declarations_are_protected_by_their_owner() {
    let output = check_source_in_module(
        "type Result { value: i64, }",
        vec!["hew".to_string(), "fixture".to_string()],
    );
    let collisions = output
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::PreludeDeclCollision)
        .count();
    assert_eq!(
        collisions, 1,
        "one package declaration must produce one protected-prelude error: {:#?}",
        output.errors
    );
}

#[test]
fn ordinary_builtin_declarations_remain_shadowable() {
    let output = check_source("type HashMapIter { value: i64, }");
    assert!(
        output.errors.is_empty(),
        "an ordinary builtin must remain shadowable: {:#?}",
        output.errors
    );
}

#[test]
fn modules_and_types_are_rejected_in_value_position() {
    let output = check_source("fn main() { let module_value = math; let type_value = Vec; }");
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::ModuleUsedAsValue));
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::TypeUsedAsValue));
}

#[test]
fn bare_type_remains_rejected_as_a_value_after_dotted_path_dispatch() {
    let output = check_source("enum Choice { Present(i64) } fn main() { let value = Choice; }");
    let type_as_value_errors = output
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::TypeUsedAsValue)
        .count();
    assert_eq!(
        type_as_value_errors, 1,
        "the bare enum must produce exactly the type-as-value diagnostic: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|error| error.kind != TypeErrorKind::UndefinedVariable),
        "the nominal head must not fall through to variable lookup: {:?}",
        output.errors
    );
}

#[test]
fn module_member_lookup_uses_path_diagnostics() {
    let output = check_source("fn main() { math.missing(); }");
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::PathMemberNotFound));
}

#[test]
fn contextual_variant_constructor_kind_must_match() {
    let output = check_source("enum State { Ready } fn make() -> State { .Ready(1) }");
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::PathKindMismatch));
}

#[test]
fn qualified_associated_item_rejects_multiple_trait_owners() {
    let parsed_trait = hew_parser::parse("pub trait Shared { type Item; }");
    assert!(parsed_trait.errors.is_empty());
    let trait_decl = parsed_trait
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Trait(trait_decl) => Some(trait_decl),
            _ => None,
        })
        .expect("trait fixture");
    let info = Checker::trait_info_from_decl(trait_decl);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .trait_defs
        .insert("left.Shared".to_string(), info.clone());
    checker.trait_defs.insert("right.Shared".to_string(), info);
    checker.published_bare_trait_owners.insert(
        (None, 0, "Shared".to_string()),
        ["left.Shared".to_string(), "right.Shared".to_string()]
            .into_iter()
            .collect(),
    );
    let program =
        hew_parser::parse("fn main() { let item = <i64 as Shared>.Item; println(item); }");
    assert!(
        program.errors.is_empty(),
        "fixture parse: {:?}",
        program.errors
    );
    let output = checker.check_program(&program.program);
    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::AssocItemAmbiguous));
}

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
    let source = "type Pt {\n    x: i64,\n    y: i64,\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 2 };\n    if a == b {\n        println(\"equal\");\n    }\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "eligible record `==` should typecheck: {:#?}",
        output.errors
    );
}

/// `!=` is admitted for eligible records; ordering with no user `impl Ord`
/// is a Limitation-channel `E_LIMIT_DERIVED_ORD` (D26 as amended by D340),
/// not a plain `InvalidOperation` — no structural-ordering codegen thunk
/// exists for aggregates, so this is a compiler gap, not a program error.
#[test]
fn record_inequality_typechecks_and_ordering_is_rejected() {
    let source = "type Pt {\n    x: i64,\n    y: i64,\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 2 };\n    let ne = a != b;\n    let lt = a < b;\n    let _ = ne;\n    let _ = lt;\n}";
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
        output.errors.iter().any(|e| e.kind
            == (TypeErrorKind::DerivedOrdUnavailable {
                type_name: "Pt".to_string()
            })
            && e.message.contains("E_LIMIT_DERIVED_ORD")
            && e.kind.channel() == crate::error::DiagChannel::Limitation),
        "expected E_LIMIT_DERIVED_ORD refusal for record `<`: {:#?}",
        output.errors
    );
}

/// Fieldless enums are tag values; their `==` remains admitted so MIR/codegen
/// can lower the comparison to tag equality instead of tripping the aggregate
/// structural-equality gate.
#[test]
fn enum_equality_not_gated_by_record_comparison_refusal() {
    let source = "enum Colour {\n    Red,\n    Green,\n}\n\nfn compare() -> bool {\n    let a = Colour.Red;\n    let b = Colour.Green;\n    a == b\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "enum `==` must not trip the record-comparison gate: {:#?}",
        output.errors
    );
}

/// Enum ordering with no user `impl Ord` is the same Limitation-channel
/// `E_LIMIT_DERIVED_ORD` refusal as the record case above, not a plain
/// `InvalidOperation`.
#[test]
fn enum_ordering_reports_checker_diagnostic() {
    let source = "enum Colour {\n    Red,\n    Green,\n}\n\nfn main() {\n    let a = Colour.Red;\n    let b = Colour.Green;\n    let _ = a < b;\n}";
    let output = check_source(source);
    assert!(
        output.errors.iter().any(|e| e.kind
            == (TypeErrorKind::DerivedOrdUnavailable {
                type_name: "Colour".to_string()
            })
            && e.message.contains("E_LIMIT_DERIVED_ORD")
            && e.kind.channel() == crate::error::DiagChannel::Limitation),
        "expected E_LIMIT_DERIVED_ORD refusal for enum ordering: {:#?}",
        output.errors
    );
}

#[test]
fn payload_enum_equality_typechecks_when_structurally_eligible() {
    let source = "enum Shape {\n    Circle(i64),\n    Empty,\n}\n\nfn main() {\n    let a = Shape.Circle(1);\n    let b = Shape.Circle(1);\n    let _ = a == b;\n}";
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
                && e.message.contains("member `data`")
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
                && e.message.contains("member `Some`")
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
    let source = "type Pt {\n    x: i64,\n    y: i64,\n}\n\nfn main() -> bool {\n    let a = Pt { x: 1, y: 2 };\n    a == 5\n}";
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

#[test]
fn literal_backed_binding_keeps_numeric_mismatch_diagnostic() {
    let output = check_source(
        r"
        fn take_i32(value: i32) {}

        fn main() {
            let decimal = 1.5;
            take_i32(decimal);
        }
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "a literal-backed float local must not bypass the i32 argument boundary: {:#?}",
        output.errors
    );
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

/// `for i in 0 .. n` where BOTH bounds are unannotated literal-coercible
/// (the start literal `0` and an unannotated `let n = 6;` local) — and the
/// loop variable `i` is later forced to `i32` by a use-site constraint
/// (`Vec<i32>::push(i)`). Regression: `check_binary_op`'s Range arm created a
/// fresh `TypeVar` for the range's element type but never unified it with
/// `n`'s own binding var. The loop-var constraint narrowed only the fresh
/// var to `i32`; `n`'s independent var still defaulted to `i64`, producing a
/// `Range<i32>` whose own end-bound expression resolved to `i64` — a
/// self-inconsistent range MIR correctly fail-closed on (`E_MIR`, corpus class
/// D). Unifying the fresh var with both bounds' own types at creation ties
/// the identity together so the loop-var constraint propagates to `n` too.
#[test]
fn for_range_start_literal_and_unannotated_end_bound_narrow_together() {
    let source = r"
        fn main() {
            let n = 6;
            var xs: Vec<i32> = Vec.new();
            for i in 0 .. n {
                xs.push(i);
            }
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "for-range with an unannotated end-bound local narrowed by loop-var \
         use must not error: {:#?}",
        output.errors
    );
}

/// Same shape as above but the range appears AFTER an unrelated for-range
/// over a genuinely concrete `i64` bound (`Vec::len()`). Regression: the
/// unification fix must not leak the narrowed width across sibling
/// for-range statements — each range's fresh `TypeVar` is independent.
#[test]
fn for_range_narrowing_does_not_leak_to_sibling_range_over_concrete_i64_bound() {
    let source = r"
        fn main() {
            let n = 6;
            var xs: Vec<i32> = Vec.new();
            for i in 0 .. n {
                xs.push(i);
            }
            var ys: Vec<i32> = Vec.new();
            ys.push(1);
            let len = ys.len();
            for e in 0 .. len {
                println(e);
            }
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "a sibling for-range over a concrete i64 `.len()` bound must stay \
         i64 regardless of an unrelated i32-narrowed range earlier in the \
         same function: {:#?}",
        output.errors
    );
}

/// Mixed signedness is not an implicit range conversion. Keeping this rejected
/// is the counterfactual for MIR's signedness-aware widening: every accepted
/// mixed-width range has one unambiguous extension mode.
#[test]
fn for_range_mixed_signedness_bounds_are_rejected() {
    let source = r"
        fn main() {
            let start: i32 = -2;
            let end: u64 = 6;
            for value in start .. end {
                println(value);
            }
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("range bounds require compatible integer types")),
        "mixed-signedness range must be rejected before MIR: {:#?}",
        output.errors
    );
}

#[test]
fn numeric_branch_joins_accept_checker_selected_common_types() {
    let source = r"
        fn main() {
            let flag = true;
            let narrow_signed: i32 = -2;
            let wide_signed: i64 = 4;
            let signed = if flag { narrow_signed } else { wide_signed };
            let narrow_unsigned: u16 = 65534;
            let wide_unsigned: u64 = 65537;
            let unsigned = if flag { narrow_unsigned } else { wide_unsigned };
            let float = if flag { narrow_signed } else { 4.5 };
            let present: Option<i64> = Some(1);
            let signed_if_let = if let .Some(_) = present {
                narrow_signed
            } else {
                wide_signed
            };
            println(signed);
            println(unsigned);
            println(float);
            println(signed_if_let);
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "checker-selected branch normalizations must remain explicit downstream, not be rejected by the ownership graph: {:#?}",
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
        fn sum_range() -> f64 {
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
        fn sum_range() -> f64 {
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
        fn sum_range() -> f64 {
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
    let source = "fn main() { let v: Vec<i32> = Vec.new(); }";
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
    let source = "fn main() { Vec.new().clear(); }";
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
        "import ecosystem.db.sqlite;\n",
        "enum Result {\n",
        "    Ok(i64),\n",
        "    Err(i64)\n",
        "}\n",
        "fn unwrap_or(r: Result, fallback: i64) -> i64 {\n",
        "    match r {\n",
        "        .Ok(v) => v,\n",
        "        .Err(_) => fallback,\n",
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
    checker.checking_embedded_builtins = true;
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

#[test]
fn checker_reuse_does_not_leak_result_shadowing_into_stdlib() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-types lives under the repo root")
        .to_path_buf();
    let string_path = repo_root.join("std/string.hew");
    let string_source = std::fs::read_to_string(&string_path).expect("read std/string.hew");

    let mut checker = Checker::new(ModuleRegistry::new(vec![repo_root.clone()]));

    let first = hew_parser::parse("pub type Result { handle: i64 }\n");
    assert!(
        first.errors.is_empty(),
        "first parse errors: {:?}",
        first.errors
    );
    let first_output = checker.check_program(&first.program);
    assert!(
        first_output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::PreludeDeclCollision),
        "first compile should reject the protected `Result` declaration, got: {:?}",
        first_output.errors
    );

    let parsed = hew_parser::parse(&string_source);
    assert!(
        parsed.errors.is_empty(),
        "std/string.hew parse errors: {:?}",
        parsed.errors
    );
    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(vec!["std".to_string(), "string".to_string()]);
    let module = Module {
        id: mod_id.clone(),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![string_path],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).unwrap();
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let second_output = checker.check_program(&program);
    assert!(
        second_output.errors.is_empty(),
        "checker reuse leaked `Result` shadowing into std::string: {:?}",
        second_output.errors
    );
}

#[test]
fn checker_reuse_does_not_leak_type_definitions() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let first = hew_parser::parse("pub type Stale { value: i64 }\n");
    assert!(
        first.errors.is_empty(),
        "first parse errors: {:?}",
        first.errors
    );
    let first_output = checker.check_program(&first.program);
    assert!(
        first_output.errors.is_empty(),
        "first compile should be clean, got: {:?}",
        first_output.errors
    );

    let second = hew_parser::parse("fn main(value: Stale) {}\n");
    assert!(
        second.errors.is_empty(),
        "second parse errors: {:?}",
        second.errors
    );
    let second_output = checker.check_program(&second.program);
    assert!(
        second_output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType && error.message.contains("Stale")
        }),
        "second compile accepted Stale leaked from the prior program: {:?}",
        second_output.errors
    );
}

#[test]
fn checker_reuse_does_not_leak_loaded_handle_methods_into_user_module() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-types lives under the repo root")
        .to_path_buf();
    let mut checker = Checker::new(ModuleRegistry::new(vec![repo_root]));

    let first = hew_parser::parse("import std.net;\n");
    assert!(
        first.errors.is_empty(),
        "first parse errors: {:?}",
        first.errors
    );
    let first_output = checker.check_program(&first.program);
    assert!(
        first_output.errors.is_empty(),
        "first compile should load std.net cleanly, got: {:?}",
        first_output.errors
    );
    assert_eq!(
        checker
            .module_registry()
            .resolve_handle_method("net.Listener", "accept")
            .as_deref(),
        Some("hew_tcp_accept"),
        "first compile should seed the legacy extracted receiver spelling"
    );

    let second = hew_parser::parse(
        r"
        pub type Listener { value: i64, }
        impl Listener {
            fn accept(self) -> i64 { self.value }
        }
        fn call(listener: Listener) -> i64 { listener.accept() }
        ",
    );
    assert!(
        second.errors.is_empty(),
        "second parse errors: {:?}",
        second.errors
    );
    let root_id = ModuleId::root();
    let module_id = ModuleId::new(vec!["net".to_string()]);
    let module = Module {
        id: module_id.clone(),
        items: second.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut module_graph = ModuleGraph::new(root_id.clone());
    module_graph
        .add_module(module)
        .expect("add user net module");
    module_graph.topo_order = vec![module_id, root_id];
    let second_program = Program {
        module_graph: Some(module_graph),
        items: vec![],
        module_doc: None,
    };

    let second_output = checker.check_program(&second_program);
    assert!(
        second_output.errors.is_empty(),
        "second compile should resolve its own net.Listener::accept: {:?}",
        second_output.errors
    );
    let listener = second_output
        .type_defs
        .get("net.Listener")
        .expect("second compile should publish its own net.Listener declaration");
    assert!(
        listener.fields.contains_key("value"),
        "second compile should retain its own net.Listener fields: {listener:?}"
    );
    assert!(
        !second_output
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(
                rewrite,
                MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                    if c_symbol == "hew_tcp_accept"
            )),
        "user net.Listener::accept acquired the cached std.net rewrite: {:?}",
        second_output.method_call_rewrites
    );
}

// --- Reserved compiler type fragments ---

#[test]
fn reserved_type_names_fail_closed_across_declaration_kinds() {
    for (source, name) in [
        (
            "type i64 { value: i64, }\nfn main() -> i64 { return 0; }",
            "i64",
        ),
        (
            "type CancellationToken { value: i64, }\nfn main() -> i64 { return 0; }",
            "CancellationToken",
        ),
        (
            "type tuple<T> { value: T, }\nfn main() -> i64 { return 0; }",
            "tuple",
        ),
        (
            "type typeparam<T> { value: T, }\nfn main() -> i64 { return 0; }",
            "typeparam",
        ),
        (
            "type string { value: i64, }\nfn main() -> i64 { return 0; }",
            "string",
        ),
        (
            "actor bytes { receive fn ping() {} }\nfn main() -> i64 { return 0; }",
            "bytes",
        ),
        ("type char = i64;", "char"),
        ("trait f32 {}", "f32"),
        (
            r"
            machine tuple {
                events { Toggle, }
                state Closed,
                state Open,
                on Toggle: Closed => .Open { Open }
                on Toggle: Open => .Closed { Closed }
            }
            fn main() {}
            ",
            "tuple",
        ),
    ] {
        let output = check_source(source);
        assert_eq!(
            output.errors.len(),
            1,
            "expected exactly one error for `{name}`; got: {:?}",
            output.errors
        );
        assert_eq!(
            output.errors[0].kind,
            TypeErrorKind::ReservedTypeName,
            "expected ReservedTypeName for `{name}`"
        );
        assert_eq!(
            output.errors[0].message,
            format!(
                "E_RESERVED_TYPE_NAME: `{name}` is reserved and cannot be used for a type declaration"
            )
        );
    }
}

#[test]
fn non_reserved_type_names_remain_accepted() {
    let output = check_source(
        "type Point { x: i64, y: i64, }\n\
         type Tuple { a: i64, }\n\
         type MyString { s: i64, }\n\
         type CancellationTokens { count: i64, }\n\
         enum Colour { Red, Green, Blue, }\n\
         fn main() -> i64 { return 0; }",
    );
    assert!(
        output.errors.is_empty(),
        "non-reserved type names should be accepted; got: {:?}",
        output.errors
    );
}
