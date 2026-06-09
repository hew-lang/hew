/// HIR lowering tests for regex literal match arm patterns.
///
/// Exercises the `HirMatchArmPredicate::Regex` variant produced by
/// `lower_match_expr` when a `PatternKind::Regex { captures }` arm is
/// resolved by the type checker and presented to HIR lowering.
///
/// Gate: slice 3 — verifies `HirMatchArmPredicate`, `HirRegexLiteral` table,
/// and `HirExprKind::RegexLiteralRef`. Slice 4 (MIR lowering) is NOT wired;
/// any attempt to compile these programs through MIR will `todo!()`.
use hew_hir::{
    lower_program, HirExprKind, HirItem, HirMatchArmPredicate, HirStmtKind, ResolutionCtx,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Lower a Hew source string through the type checker and into HIR.
/// Asserts no parse errors and no type errors before returning the output.
fn lower_checked(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

/// Find the first `HirExprKind::Match` expression in the given function body.
fn find_match_in_fn<'a>(output: &'a hew_hir::LowerOutput, fn_name: &str) -> &'a HirExprKind {
    let fn_item = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(f) = item {
                if f.name == fn_name {
                    return Some(f);
                }
            }
            None
        })
        .unwrap_or_else(|| panic!("{fn_name} function not found in HIR"));

    fn_item
        .body
        .statements
        .iter()
        .filter_map(|s| {
            if let HirStmtKind::Expr(e) = &s.kind {
                Some(&e.kind)
            } else {
                None
            }
        })
        .chain(fn_item.body.tail.iter().map(|e| &e.kind))
        .find(|k| matches!(k, HirExprKind::Match { .. }))
        .unwrap_or_else(|| panic!("no Match expr in {fn_name} body"))
}

/// A regex arm with no named captures lowers to `HirMatchArmPredicate::Regex`
/// with an empty captures list and a `literal_id` pointing into the module table.
#[test]
fn regex_arm_no_captures_lowers_to_predicate_regex() {
    let src = r#"
        fn classify(s: string) -> string {
            match s {
                re"^hello" => "greeting",
                _ => "other",
            }
        }
    "#;
    let output = lower_checked(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "classify") else {
        panic!("expected Match");
    };

    // Two arms: regex arm + wildcard arm.
    assert_eq!(arms.len(), 2, "expected 2 arms; got {}", arms.len());

    match &arms[0].predicate {
        HirMatchArmPredicate::Regex {
            literal_id,
            pattern,
            captures,
        } => {
            assert_eq!(pattern, "^hello", "pattern mismatch");
            assert!(
                captures.is_empty(),
                "expected no captures; got {captures:?}"
            );
            // The literal_id must index into the module's regex_literals table.
            let entry = output
                .module
                .regex_literals
                .get(*literal_id as usize)
                .expect("literal_id out of bounds in regex_literals table");
            assert_eq!(entry.pattern, "^hello");
            assert_eq!(entry.literal_id, *literal_id);
        }
        other => panic!("expected HirMatchArmPredicate::Regex; got {other:?}"),
    }

    // The second arm is the wildcard.
    assert!(
        matches!(arms[1].predicate, HirMatchArmPredicate::Wildcard),
        "second arm should be Wildcard; got {:?}",
        arms[1].predicate
    );
}

/// Two distinct regex patterns produce two distinct entries in the module
/// literal table. A repeated pattern (same string) deduplicates to one entry.
#[test]
fn regex_literals_table_deduplicates_identical_patterns() {
    let src = r#"
        fn check(s: string) -> i64 {
            let a = match s {
                re"foo" => 1,
                _ => 0,
            };
            let b = match s {
                re"bar" => 2,
                _ => 0,
            };
            let c = match s {
                re"foo" => 3,
                _ => 0,
            };
            a
        }
    "#;
    let output = lower_checked(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    // Only two distinct patterns: "foo" and "bar". "foo" appears twice but
    // must be deduplicated to one table entry.
    let table = &output.module.regex_literals;
    assert_eq!(
        table.len(),
        2,
        "expected 2 entries in regex_literals table (foo + bar); got {}: {table:?}",
        table.len()
    );

    let patterns: std::collections::HashSet<&str> =
        table.iter().map(|e| e.pattern.as_str()).collect();
    assert!(patterns.contains("foo"), "foo missing from table");
    assert!(patterns.contains("bar"), "bar missing from table");

    // Each entry's literal_id matches its position.
    for entry in table {
        assert_eq!(
            entry.literal_id as usize,
            table
                .iter()
                .position(|e| e.literal_id == entry.literal_id)
                .unwrap(),
            "literal_id {} does not match position in Vec",
            entry.literal_id
        );
    }
}

/// Multiple regex arms in a single match produce multiple `Regex` predicates,
/// each referencing a distinct `literal_id` for distinct patterns.
#[test]
fn multiple_regex_arms_produce_distinct_predicate_per_arm() {
    let src = r#"
        fn route(s: string) -> string {
            match s {
                re"^GET " => "get",
                re"^POST " => "post",
                _ => "other",
            }
        }
    "#;
    let output = lower_checked(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "route") else {
        panic!("expected Match");
    };

    // Three arms: two Regex arms + one Wildcard.
    assert_eq!(arms.len(), 3);
    assert!(
        matches!(arms[0].predicate, HirMatchArmPredicate::Regex { .. }),
        "arm 0 should be Regex"
    );
    assert!(
        matches!(arms[1].predicate, HirMatchArmPredicate::Regex { .. }),
        "arm 1 should be Regex"
    );
    assert!(
        matches!(arms[2].predicate, HirMatchArmPredicate::Wildcard),
        "arm 2 should be Wildcard"
    );

    // Both regex arms reference distinct literal_ids.
    let HirMatchArmPredicate::Regex {
        literal_id: id0, ..
    } = arms[0].predicate
    else {
        unreachable!()
    };
    let HirMatchArmPredicate::Regex {
        literal_id: id1, ..
    } = arms[1].predicate
    else {
        unreachable!()
    };
    assert_ne!(id0, id1, "distinct patterns must have distinct literal_ids");

    // Table has exactly 2 entries (one per distinct pattern).
    assert_eq!(output.module.regex_literals.len(), 2);
}
