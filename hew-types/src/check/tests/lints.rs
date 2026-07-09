#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn warn_unused_variable() {
    let source = "fn main() { let unused_var = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_var`")),
        "expected unused variable warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn raii_handle_bindings_suppress_unused_lint_but_plain_still_warns() {
    // A `#[resource]` and a `#[linear]` handle exist to be dropped/consumed:
    // binding one for its scope-exit drop is the use, so the unused-binding lint
    // must stay silent. A plain scalar in the same scope must still warn — the
    // suppression is type-targeted, not a blanket disable.
    let source = "\
#[resource]\n\
type Guard { id: i64; }\n\
impl Guard { fn close(g: Guard) { } }\n\
fn open() -> Guard { Guard { id: 1 } }\n\
#[linear]\n\
type Txn { id: i64; }\n\
impl Txn { fn commit(consuming self) -> i64 { 0 } }\n\
fn mk() -> Txn { Txn { id: 0 } }\n\
fn main() { let g = open(); let t = mk(); let plain = 42; }\n";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    let unused = |n: &str| {
        output
            .warnings
            .iter()
            .any(|w| w.message.contains(&format!("unused variable `{n}`")))
    };
    assert!(
        !unused("g"),
        "resource handle must not warn unused: {:?}",
        output.warnings
    );
    assert!(
        !unused("t"),
        "linear handle must not warn unused: {:?}",
        output.warnings
    );
    assert!(
        unused("plain"),
        "plain scalar must still warn unused: {:?}",
        output.warnings
    );
}

#[test]
fn warn_var_never_mutated() {
    let source = "fn main() { var x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "expected unmutated var warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_underscore_prefix() {
    let source = "fn main() { let _unused = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.message.contains("_unused")),
        "should not warn on _ prefix, got: {:?}",
        output.warnings
    );
}

// -----------------------------------------------------------------------
// Lint / warning regression tests
// -----------------------------------------------------------------------

/// `set_repl_fragment` suppresses the four whole-program completeness lints
/// (`UnusedImport`, `DeadCode`, `UnusedVariable`, `UnusedMut`) that misfire on
/// a synthetic `hew eval` fragment, while the default checker still emits every
/// one. This is the contract the eval REPL relies on; `hew check`/`hew build`
/// leave the flag at its `false` default and must keep emitting the lints.
#[test]
fn repl_fragment_suppresses_completeness_lints_default_keeps_them() {
    const SOURCE: &str = r"
import std::math;

fn dead_helper() -> i64 { 5 }

fn main() {
    let unused_binding = 41;
    var never_reassigned = 7;
    let _kept = never_reassigned + 1;
}
";
    let parsed = hew_parser::parse(SOURCE);
    assert!(
        parsed.errors.is_empty(),
        "fixture should parse cleanly, got: {:?}",
        parsed.errors
    );

    let has = |warnings: &[TypeError], kind: &TypeErrorKind, needle: &str| {
        warnings
            .iter()
            .any(|w| &w.kind == kind && w.message.contains(needle))
    };

    // Default mode: every completeness lint fires.
    let mut default_checker = Checker::new(test_registry());
    let default_out = default_checker.check_program(&parsed.program);
    assert!(
        has(&default_out.warnings, &TypeErrorKind::UnusedImport, "math"),
        "default should warn unused import, got: {:?}",
        default_out.warnings
    );
    assert!(
        has(
            &default_out.warnings,
            &TypeErrorKind::Lint(LintId::DeadCode),
            "dead_helper"
        ),
        "default should warn dead code, got: {:?}",
        default_out.warnings
    );
    assert!(
        has(
            &default_out.warnings,
            &TypeErrorKind::UnusedVariable,
            "unused_binding"
        ),
        "default should warn unused variable, got: {:?}",
        default_out.warnings
    );
    assert!(
        has(
            &default_out.warnings,
            &TypeErrorKind::UnusedMut,
            "never_reassigned"
        ),
        "default should warn unused mut, got: {:?}",
        default_out.warnings
    );

    // REPL-fragment mode: none of the four fire, and suppression does not
    // manufacture an error.
    let mut repl_checker = Checker::new(test_registry());
    repl_checker.set_repl_fragment();
    let repl_out = repl_checker.check_program(&parsed.program);
    let suppressed = [
        TypeErrorKind::UnusedImport,
        TypeErrorKind::Lint(LintId::DeadCode),
        TypeErrorKind::UnusedVariable,
        TypeErrorKind::UnusedMut,
    ];
    assert!(
        repl_out
            .warnings
            .iter()
            .all(|w| !suppressed.contains(&w.kind)),
        "repl_fragment should suppress all four completeness lints, got: {:?}",
        repl_out.warnings
    );
    assert!(
        repl_out.errors.is_empty(),
        "repl_fragment fixture should have no errors, got: {:?}",
        repl_out.errors
    );
}

#[test]
fn where_clause_assoc_binding_projects_iterator_item_in_generic_body() {
    let result = hew_parser::parse(
        r"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }

        fn main() -> i64 {
            let v: Vec<i64> = Vec::new();
            v.push(42);
            first_or_zero(v.into_iter())
        }
        ",
    );
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);

    assert!(
        output.warnings.is_empty(),
        "unexpected warnings: {:?}",
        output.warnings
    );
    assert!(
        output.errors.is_empty(),
        "Iterator<Item = i64> should project I::Item to i64 in a generic body: {:?}",
        output.errors
    );
    assert!(
        output
            .pattern_resolutions
            .values()
            .flat_map(|arm| arm.payload_bindings.iter())
            .any(|payload| payload.binding_name == "x" && payload.ty == Ty::I64),
        "match payload binding from Option<I::Item> should be published as i64: {:?}",
        output.pattern_resolutions
    );
}

#[test]
fn where_clause_assoc_binding_projects_non_iterator_assoc_type() {
    let (errors, warnings) = parse_and_check(
        r"
        trait Projector {
            type Output;
            fn get(self) -> Self::Output;
        }

        type Meter {
            value: i64;
        }

        impl Projector for Meter {
            type Output = i64;

            fn get(self) -> i64 {
                self.value
            }
        }

        fn read<P>(p: P) -> i64
        where
            P: Projector<Output = i64>,
        {
            p.get()
        }

        fn main() -> i64 {
            read(Meter { value: 42 })
        }
        ",
    );

    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
    assert!(
        errors.is_empty(),
        "where-clause associated-type projection should not be Iterator-specific: {errors:?}"
    );
}

#[test]
fn unbound_where_clause_assoc_projection_remains_type_error() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        r"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }
        ",
    );

    assert!(
        errors.iter().any(|error| error.message.contains("I::Item")),
        "unbound I::Item must remain unresolved/fail-closed; got {errors:?}"
    );
}

#[test]
fn call_site_rejects_assoc_binding_mismatch() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        r#"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }

        fn main() -> i64 {
            let v: Vec<string> = Vec::new();
            v.push("not an integer");
            first_or_zero(v.into_iter())
        }
        "#,
    );

    assert!(
        errors.iter().any(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied
                && error.message.contains("Iterator<Item = i64>")
                && error.message.contains("string")
        }),
        "Vec<string> must not satisfy Iterator<Item = i64>; got {errors:?}"
    );
}

#[test]
fn scope_error_type_constructs_and_field_accesses() {
    let source = concat!(
        "import std::concurrency;\n",
        "fn read_primary(err: concurrency.ScopeError<i64>) -> i64 {\n",
        "    let primary: i64 = err.primary;\n",
        "    primary\n",
        "}\n",
        "fn read_others(err: concurrency.ScopeError<i64>) -> Vec<i64> {\n",
        "    let others: Vec<i64> = err.also_failed;\n",
        "    others\n",
        "}\n",
        "fn read_cancelled(err: concurrency.ScopeError<i64>) -> i64 {\n",
        "    let cancelled: i64 = err.cancelled_count;\n",
        "    cancelled\n",
        "}\n",
        "fn pass_through(err: concurrency.ScopeError<i64>) -> concurrency.ScopeError<i64> {\n",
        "    err\n",
        "}\n",
        "fn main() {\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected type errors: {:?}",
        output.errors
    );
}

#[test]
fn builtin_print_registration_keeps_display_bounds_on_bare_names() {
    let mut checker = Checker::new(test_registry());
    checker.register_builtins();

    for name in ["print", "println", "to_string", "assert_eq", "assert_ne"] {
        let sig = checker
            .fn_sigs
            .get(name)
            .unwrap_or_else(|| panic!("missing builtin signature for {name}"));
        assert_eq!(
            sig.type_params,
            vec!["T".to_string()],
            "{name} should expose a single generic parameter"
        );
        assert_eq!(
            sig.type_param_bounds.get("T"),
            Some(&vec!["Display".to_string()]),
            "{name} should keep a Display bound on its bare-name registration"
        );
    }

    let len_sig = checker.fn_sigs.get("len").expect("missing len builtin");
    assert!(
        len_sig.type_param_bounds.is_empty(),
        "len must stay out of the Display migration"
    );
}

#[test]
fn print_and_println_reject_struct_without_display_impl() {
    let (errors, warnings) = parse_and_check_with_stdlib(
        r"
        type Hidden {
            value: i64;
        }

        fn main() {
            let hidden = Hidden { value: 1 };
            print(hidden);
            println(hidden);
        }
        ",
    );

    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
    let bounds_errors: Vec<_> = errors
        .iter()
        .filter(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied && error.message.contains("Display")
        })
        .collect();
    assert_eq!(
        bounds_errors.len(),
        2,
        "print/println should reject values without Display: {errors:?}"
    );
}

#[test]
fn display_impl_satisfies_bounded_magic_builtins() {
    let (errors, warnings) = parse_and_check_with_stdlib(
        r#"
        type Widget {
            value: i64;
        }

        impl Display for Widget {
            fn fmt(widget: Widget) -> string {
                "widget"
            }
        }

        fn main() {
            let widget = Widget { value: 1 };
            print(widget);
            println(widget);
            let text = to_string(widget);
            assert_eq(widget, widget);
            assert_ne(widget, widget);
            println(text);
        }
        "#,
    );

    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn deferred_bound_check_drains_after_defaulting() {
    let mut checker = make_checker_with_trait("MyTrait", &[], false, false);
    let span = 0..0;
    let var = TypeVar::fresh();
    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MyTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(&sig, &[Ty::Var(var)], &span);
    assert!(
        checker
            .errors
            .iter()
            .all(|error| error.kind != TypeErrorKind::BoundsNotSatisfied),
        "unresolved arg should defer bound enforcement: {:?}",
        checker.errors
    );
    assert_eq!(checker.deferred_bound_checks.len(), 1);

    checker.subst.insert(var, &Ty::I64).unwrap();
    checker.drain_deferred_bound_checks();

    let bounds_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bounds_errors.len(),
        1,
        "expected exactly one deferred bound failure after resolution: {:?}",
        checker.errors
    );
    assert!(
        bounds_errors[0].message.contains("MyTrait") && bounds_errors[0].message.contains("i64"),
        "expected deferred diagnostic to mention MyTrait and i64: {:?}",
        bounds_errors[0]
    );
}

#[test]
fn deferred_bound_check_skips_when_var_remains_unresolved() {
    let mut checker = make_checker_with_trait("MyTrait", &[], false, false);
    let span = 0..0;
    let var = TypeVar::fresh();
    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MyTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(&sig, &[Ty::Var(var)], &span);
    checker
        .deferred_inference_holes
        .push(DeferredInferenceHole {
            span: span.clone(),
            context: "test deferred bound hole".to_string(),
            hole_vars: vec![var],
            source_module: None,
        });

    checker.drain_deferred_bound_checks();
    let program = hew_parser::parse("").program;
    checker.report_unresolved_inference_holes(&program);

    let bounds_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert!(
        bounds_errors.is_empty(),
        "unresolved hole should not also emit bound noise: {:?}",
        checker.errors
    );
    let inference_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_errors.len(),
        1,
        "expected unresolved-hole reporter to stay authoritative: {:?}",
        checker.errors
    );
}

#[test]
fn deferred_bound_check_drains_when_var_resolves_to_satisfying_type() {
    let mut checker = Checker::new(test_registry());
    checker.register_builtins();
    let span = 0..0;
    let var = TypeVar::fresh();
    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(&sig, &[Ty::Var(var)], &span);
    checker.subst.insert(var, &Ty::I64).unwrap();
    checker.drain_deferred_bound_checks();

    assert!(
        checker.errors.is_empty(),
        "resolved Display-bound arg should pass deferred drain: {:?}",
        checker.errors
    );
}

// ---- unused variable ----

#[test]
fn warn_unused_in_nested_scope() {
    let (errors, warnings) = parse_and_check("fn main() { if true { let nested = 1; } }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `nested`")),
        "expected unused variable warning for nested, got: {warnings:?}"
    );
}

#[test]
fn no_warn_used_variable() {
    let (errors, warnings) = parse_and_check("fn main() { let x = 42; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "should not warn on used variable, got: {warnings:?}"
    );
}

#[test]
fn no_warn_underscore_alone() {
    let (_, warnings) = parse_and_check("fn main() { let _ = 42; }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "bare _ should never warn, got: {warnings:?}"
    );
}

// ---- var never mutated ----

#[test]
fn no_warn_var_actually_mutated() {
    let (errors, warnings) = parse_and_check("fn main() { var x = 10; x = 20; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "should not warn when var is actually reassigned, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_can_be_reassigned() {
    let (errors, warnings) = parse_and_check("fn bump(var x: i64) -> i64 { x = x + 1; x }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "mutable param reassignment should suppress unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_cannot_be_reassigned() {
    let (errors, warnings) = parse_and_check("fn bump(x: i64) -> i64 { x = x + 1; x }");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `x`")),
        "expected immutable parameter assignment error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable param should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn main() { let p = Point { x: 1 }; p.x = 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn bump(p: Point) { p.x = 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable parameter field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable parameter field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_compound_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn main() { let p = Point { x: 1 }; p.x += 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable compound field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable compound field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn mutable_field_assignment_root_counts_as_mutation() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn main() { var p = Point { x: 1 }; p.x = 2; println(p.x); }\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "field assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_field_assignment_root_counts_as_mutation() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn bump(var p: Point) { p.x = 2; println(p.x); }\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "parameter field assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn immutable_index_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check("fn main() { let xs = [1, 2]; xs[0] = 3; }\n");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `xs`")),
        "expected immutable index-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable index assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_index_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check("fn bump(xs: Vec<i64>) { xs[0] = 2; }\n");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `xs`")),
        "expected immutable parameter index-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable parameter index assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn mutable_index_assignment_root_counts_as_mutation() {
    let (errors, warnings) =
        parse_and_check("fn main() { var xs = [1, 2]; xs[0] = 3; println(xs[0]); }\n");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "index assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_index_assignment_root_counts_as_mutation() {
    let (errors, warnings) =
        parse_and_check("fn bump(var xs: Vec<i64>) { xs[0] = 2; println(xs[0]); }\n");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "parameter index assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn warn_var_never_mutated_suggestion() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .expect("expected never-reassigned warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("let")),
        "should suggest using `let`, got: {:?}",
        w.suggestions
    );
}

// ---- assignment LHS not false-positive as "used" ----

#[test]
fn warn_write_only_variable() {
    // `x` is only written to, never read — should get unused warning
    let (_, warnings) = parse_and_check("fn main() { var x = 0; x = 1; }");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "write-only variable should be warned as unused, got: {warnings:?}"
    );
}

#[test]
fn no_warn_variable_used_then_assigned() {
    // `x` is read (println) AND then written — it's genuinely used
    let (_, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; println(x); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "variable that is read should not get unused warning, got: {warnings:?}"
    );
}

// ---- while true → loop ----

#[test]
fn warn_while_true() {
    let (_, warnings) = parse_and_check("fn main() { while true { break; } }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("while true"))
        .expect("expected while-true style warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("loop")),
        "should suggest loop, got: {:?}",
        w.suggestions
    );
    assert!(
        matches!(w.kind, TypeErrorKind::StyleSuggestion),
        "should be StyleSuggestion kind"
    );
}

#[test]
fn no_warn_while_condition() {
    let (_, warnings) = parse_and_check("fn main() { let x = true; while x { break; } }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("while true")),
        "should not warn on while <variable>, got: {warnings:?}"
    );
}

// ---- unused return value ----

#[test]
fn no_warn_discarded_return_value() {
    // Calling a function that returns a value without binding the result
    // should NOT warn — users can discard results freely
    let (_, warnings) = parse_and_check(concat!(
        "fn compute() -> i32 { 42 }\n",
        "fn main() { compute(); }\n",
    ));
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "discarded return value should not warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_println() {
    // println returns unit, and is a known side-effect function
    let (_, warnings) = parse_and_check("fn main() { println(42); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "println() should not produce unused value warning, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_spawn() {
    // spawn is a side-effect expression — don't warn about discarded return
    let (_, warnings) = parse_and_check(concat!(
        "actor Worker { count: i32;\n",
        "    receive fn work() {} }\n",
        "fn main() { let _w = spawn Worker(count: 0); }\n",
    ));
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused") && !w.message.contains("unused variable")),
        "spawn() should not produce unused value warning, got: {warnings:?}"
    );
}

// ---- unit binding ----

#[test]
fn warn_unit_binding() {
    let (_, warnings) = parse_and_check("fn main() { let x = println(42); }");
    assert!(
        warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to unit type should warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_non_unit_binding() {
    let (_, warnings) =
        parse_and_check("fn compute() -> i32 { 42 }\nfn main() { let x = compute(); println(x); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to non-unit type should not warn, got: {warnings:?}"
    );
}

// ---- Levenshtein "did you mean?" suggestions ----

#[test]
fn suggest_similar_variable() {
    let (errors, _) = parse_and_check("fn main() { let counter = 42; println(conter); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("conter"))
        .expect("expected error for misspelled variable");
    assert!(
        err.suggestions.iter().any(|s| s.contains("counter")),
        "should suggest 'counter', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_function() {
    let (errors, _) = parse_and_check(concat!(
        "fn calculate_sum(a: i32, b: i32) -> i32 { a + b }\n",
        "fn main() { calculate_sun(1, 2); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("calculate_sun"))
        .expect("expected error for misspelled function");
    assert!(
        err.suggestions.iter().any(|s| s.contains("calculate_sum")),
        "should suggest 'calculate_sum', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_type() {
    // Use a misspelled type in a constructor position, which triggers undefined type lookup
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn make() { let p = Pont { x: 0, y: 0 }; println(p.x); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("Pont"))
        .expect("expected error for misspelled type");
    assert!(
        err.suggestions.iter().any(|s| s.contains("Point")),
        "should suggest 'Point', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_field() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn get_z(p: Point) -> i32 { p.z }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains('z'))
        .expect("expected error for undefined field");
    assert!(
        !err.suggestions.is_empty(),
        "should suggest similar fields, got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_method() {
    let (errors, _) = parse_and_check(concat!(
        "type Counter {}\n",
        "impl Counter { fn length(c: Counter) -> i64 { 0 } }\n",
        "fn main() { let c = Counter {}; c.lenght(); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("lenght"))
        .expect("expected error for misspelled method");
    assert!(
        err.suggestions.iter().any(|s| s.contains("length")),
        "should suggest 'length', got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_method_when_too_different() {
    let (errors, _) = parse_and_check(concat!(
        "type Counter {}\n",
        "impl Counter { fn length(c: Counter) -> i64 { 0 } }\n",
        "fn main() { let c = Counter {}; c.zzzzz(); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined method");
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("length")),
        "should not suggest distant method names, got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_when_too_different() {
    let (errors, _) = parse_and_check("fn main() { let alpha = 1; println(zzzzz); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined variable");
    // "zzzzz" is too far from "alpha" — no suggestion
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("alpha")),
        "should not suggest distant names, got: {:?}",
        err.suggestions
    );
}

// ---- warning severity and kind ----

#[test]
fn lint_warnings_have_warning_severity() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    for w in &warnings {
        assert_eq!(
            w.severity,
            crate::error::Severity::Warning,
            "lint warnings must have Warning severity, got: {:?} for {}",
            w.severity,
            w.message
        );
    }
}

// ---- needless_range_loop lint (M1 core) ----

/// Count `needless_range_loop` findings in one diagnostic channel.
fn count_needless_range_loop(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::NeedlessRangeLoop))
        .count()
}

#[test]
fn needless_range_loop_flags_index_access() {
    let (errors, warnings) =
        parse_and_check("fn scan(xs: Vec<i64>) { for i in 0..xs.len() { let _ = xs[i]; } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::NeedlessRangeLoop))
        .expect("expected a needless_range_loop warning");
    assert_eq!(hit.severity, crate::error::Severity::Warning);
    assert_eq!(hit.kind.as_kind_str(), "needless_range_loop");
    assert!(
        hit.message.contains("only used to index"),
        "message: {}",
        hit.message
    );
    assert!(
        hit.suggestions
            .iter()
            .any(|s| s.contains("iterate the collection directly")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn needless_range_loop_flags_get_access() {
    let (errors, warnings) =
        parse_and_check("fn scan(xs: Vec<i64>) { for i in 0..xs.len() { let _ = xs.get(i); } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        1,
        "warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_not_flagged_when_index_used_beyond_indexing() {
    let (errors, warnings) = parse_and_check(
        "fn scan(xs: Vec<i64>) { for i in 0..xs.len() { let _ = xs[i]; let _ = i + 1; } }",
    );
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "a bare use of the index must suppress the lint, warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_not_flagged_when_collection_mutated() {
    let (_, warnings) = parse_and_check(
        "fn scan(xs: Vec<i64>) { for i in 0..xs.len() { xs.push(i); let _ = xs[i]; } }",
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "mutating the collection must suppress the lint, warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_not_flagged_when_index_reassigned() {
    let (_, warnings) =
        parse_and_check("fn scan(xs: Vec<i64>) { for i in 0..xs.len() { i = 0; let _ = xs[i]; } }");
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "reassigning the index must suppress the lint, warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_not_flagged_for_nested_loop_reusing_index() {
    // The inner loop rebinds `i`, shadowing the outer index, so the outer loop
    // body no longer provably indexes `xs` with the outer `i`.
    let (_, warnings) = parse_and_check(
        "fn scan(xs: Vec<i64>, ys: Vec<i64>) { \
         for i in 0..xs.len() { for i in 0..ys.len() { let _ = xs[i]; } } }",
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "a shadowing nested loop must suppress the lint, warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_deny_level_routes_to_errors() {
    const SOURCE: &str = "fn scan(xs: Vec<i64>) { for i in 0..xs.len() { let _ = xs[i]; } }";
    let parsed = hew_parser::parse(SOURCE);
    assert!(parsed.errors.is_empty(), "fixture should parse cleanly");
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut levels = LintLevels::from_defaults();
    levels.set(LintId::NeedlessRangeLoop, LintLevel::Deny);
    checker.set_lint_levels(levels);
    let out = checker.check_program(&parsed.program);
    let hit = out
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::Lint(LintId::NeedlessRangeLoop))
        .expect("Deny should route the lint into errors");
    assert_eq!(hit.severity, crate::error::Severity::Error);
    // Severity partition: a Deny lint must NOT also leak into the warning
    // channel (which the `lint_warnings_have_warning_severity` invariant
    // requires to stay warning-only).
    assert_eq!(count_needless_range_loop(&out.warnings), 0);
}

#[test]
fn needless_range_loop_allow_level_suppresses() {
    const SOURCE: &str = "fn scan(xs: Vec<i64>) { for i in 0..xs.len() { let _ = xs[i]; } }";
    let parsed = hew_parser::parse(SOURCE);
    assert!(parsed.errors.is_empty(), "fixture should parse cleanly");
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut levels = LintLevels::from_defaults();
    levels.set(LintId::NeedlessRangeLoop, LintLevel::Allow);
    checker.set_lint_levels(levels);
    let out = checker.check_program(&parsed.program);
    assert_eq!(count_needless_range_loop(&out.warnings), 0);
    assert_eq!(count_needless_range_loop(&out.errors), 0);
}

/// Parse `source`, install it as the root lint source (so `// hew:allow(...)`
/// directives resolve), and type-check at the given level for
/// `needless_range_loop`.
fn check_with_lint_source(source: &str, level: LintLevel) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "fixture should parse cleanly: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut levels = LintLevels::from_defaults();
    levels.set(LintId::NeedlessRangeLoop, level);
    checker.set_lint_levels(levels);
    let mut sources = LintSources::new();
    sources.set_root(source.to_string());
    checker.set_lint_sources(sources);
    checker.check_program(&parsed.program)
}

#[test]
fn needless_range_loop_suppressed_by_directive_above() {
    const SOURCE: &str = "fn scan(xs: Vec<i64>) {\n\
         // hew:allow(needless_range_loop)\n\
         for i in 0..xs.len() { let _ = xs[i]; }\n\
         }";
    let out = check_with_lint_source(SOURCE, LintLevel::Warn);
    assert_eq!(
        count_needless_range_loop(&out.warnings),
        0,
        "a directive on the line above must suppress, warnings: {:?}",
        out.warnings
    );
    assert_eq!(count_needless_range_loop(&out.errors), 0);
}

#[test]
fn needless_range_loop_suppressed_by_trailing_directive() {
    const SOURCE: &str = "fn scan(xs: Vec<i64>) {\n\
         for i in 0..xs.len() { let _ = xs[i]; } // hew:allow(needless_range_loop)\n\
         }";
    let out = check_with_lint_source(SOURCE, LintLevel::Warn);
    assert_eq!(
        count_needless_range_loop(&out.warnings),
        0,
        "a trailing directive on the loop line must suppress, warnings: {:?}",
        out.warnings
    );
}

#[test]
fn needless_range_loop_suppressed_by_allow_all_directive() {
    const SOURCE: &str = "fn scan(xs: Vec<i64>) {\n\
         // hew:allow(all)\n\
         for i in 0..xs.len() { let _ = xs[i]; }\n\
         }";
    let out = check_with_lint_source(SOURCE, LintLevel::Warn);
    assert_eq!(
        count_needless_range_loop(&out.warnings),
        0,
        "`hew:allow(all)` must suppress every lint, warnings: {:?}",
        out.warnings
    );
}

#[test]
fn needless_range_loop_directive_for_other_lint_does_not_suppress() {
    // A directive naming a *different* lint must leave this one intact.
    const SOURCE: &str = "fn scan(xs: Vec<i64>) {\n\
         // hew:allow(some_other_lint)\n\
         for i in 0..xs.len() { let _ = xs[i]; }\n\
         }";
    let out = check_with_lint_source(SOURCE, LintLevel::Warn);
    assert_eq!(
        count_needless_range_loop(&out.warnings),
        1,
        "a non-matching directive must not suppress, warnings: {:?}",
        out.warnings
    );
}

#[test]
fn needless_range_loop_directive_overrides_deny() {
    // A local `allow` wins over a command-line `--deny` (rustc/Clippy rule):
    // the finding is dropped entirely rather than promoted to an error.
    const SOURCE: &str = "fn scan(xs: Vec<i64>) {\n\
         // hew:allow(needless_range_loop)\n\
         for i in 0..xs.len() { let _ = xs[i]; }\n\
         }";
    let out = check_with_lint_source(SOURCE, LintLevel::Deny);
    assert_eq!(count_needless_range_loop(&out.errors), 0);
    assert_eq!(count_needless_range_loop(&out.warnings), 0);
}

#[test]
fn needless_range_loop_directive_above_code_does_not_suppress() {
    // The directive is separated from the loop by a real statement, so the
    // upward scan stops at the code line and the lint still fires.
    const SOURCE: &str = "fn scan(xs: Vec<i64>) {\n\
         // hew:allow(needless_range_loop)\n\
         let n = xs.len();\n\
         for i in 0..xs.len() { let _ = xs[i]; }\n\
         }";
    let out = check_with_lint_source(SOURCE, LintLevel::Warn);
    assert_eq!(
        count_needless_range_loop(&out.warnings),
        1,
        "a directive shadowed by an intervening statement must not suppress, warnings: {:?}",
        out.warnings
    );
}

#[test]
fn needless_range_loop_not_flagged_for_offset_indexing() {
    // `xs[i+1]` uses the index in an arithmetic expression rather than as a
    // bare subscript, so the element-iteration rewrite is semantically unsafe.
    // The lint must NOT fire — flagging this would silently change behaviour
    // (the rewrite would lose the +1 offset).
    let (errors, warnings) =
        parse_and_check("fn f(xs: Vec<i64>) { for i in 0..xs.len() { let _ = xs[i+1]; } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "offset index xs[i+1] must suppress the lint, warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_not_flagged_for_nonzero_range_start() {
    // The range starts at 1, not 0, so `for x in xs` would miss the first
    // element — the rewrite is not valid and the lint must NOT fire.
    let (errors, warnings) =
        parse_and_check("fn f(xs: Vec<i64>) { for i in 1..xs.len() { let _ = xs[i]; } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "non-zero range start must suppress the lint, warnings: {warnings:?}"
    );
}

#[test]
fn needless_range_loop_not_flagged_for_non_vec_collection() {
    // `is_lintable_collection` only admits `Vec<_>`; a `HashMap<i64, i64>` in
    // an otherwise canonical 0..xs.len() / xs[i] shape must NOT be flagged
    // because `for x in xs` does not exist for maps.
    let (errors, warnings) =
        parse_and_check("fn f(xs: HashMap<i64, i64>) { for i in 0..xs.len() { let _ = xs[i]; } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_needless_range_loop(&warnings),
        0,
        "a HashMap collection must not be flagged, warnings: {warnings:?}"
    );
}

// ---- M2 checker-stage lints ----

/// Parse `source`, install it as the root lint source (so `// hew:allow(...)`
/// resolves) and as the level for `id`, then type-check. A single helper serves
/// every M2 lint because they all route through the shared registry.
fn check_with_lint_level(source: &str, id: LintId, level: LintLevel) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "fixture should parse cleanly: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut levels = LintLevels::from_defaults();
    levels.set(id, level);
    checker.set_lint_levels(levels);
    let mut sources = LintSources::new();
    sources.set_root(source.to_string());
    checker.set_lint_sources(sources);
    checker.check_program(&parsed.program)
}

fn check_with_lint_defaults(source: &str) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "fixture should parse cleanly: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut sources = LintSources::new();
    sources.set_root(source.to_string());
    checker.set_lint_sources(sources);
    checker.check_program(&parsed.program)
}

// ---- redundant_else_after_return ----

fn count_redundant_else(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::RedundantElseAfterReturn))
        .count()
}

#[test]
fn redundant_else_flags_return_then_else() {
    let (errors, warnings) =
        parse_and_check("fn f(c: bool) { if c { return; } else { let _ = 1; } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::RedundantElseAfterReturn))
        .expect("expected a redundant_else_after_return warning");
    assert_eq!(hit.kind.as_kind_str(), "redundant_else_after_return");
    assert_eq!(hit.severity, crate::error::Severity::Warning);
    assert!(
        hit.suggestions.iter().any(|s| s.contains("de-indent")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn redundant_else_flags_break_in_loop() {
    let (_, warnings) =
        parse_and_check("fn f(c: bool) { loop { if c { break; } else { let _ = 1; } } }");
    assert_eq!(
        count_redundant_else(&warnings),
        1,
        "a diverging `break` then-branch with an else must fire, warnings: {warnings:?}"
    );
}

#[test]
fn redundant_else_not_flagged_without_else() {
    let (_, warnings) = parse_and_check("fn f(c: bool) { if c { return; } let _ = 1; }");
    assert_eq!(
        count_redundant_else(&warnings),
        0,
        "no else means nothing to de-indent, warnings: {warnings:?}"
    );
}

#[test]
fn redundant_else_not_flagged_when_then_falls_through() {
    let (_, warnings) =
        parse_and_check("fn f(c: bool) { if c { let _ = 1; } else { let _ = 2; } }");
    assert_eq!(
        count_redundant_else(&warnings),
        0,
        "a then-branch that falls through keeps the else meaningful, warnings: {warnings:?}"
    );
}

#[test]
fn redundant_else_not_flagged_for_else_if_chain() {
    let (_, warnings) = parse_and_check(
        "fn f(c: bool, d: bool) { if c { return; } else if d { let _ = 1; } else { let _ = 2; } }",
    );
    assert_eq!(
        count_redundant_else(&warnings),
        0,
        "an else-if chain is not a clean de-indent, warnings: {warnings:?}"
    );
}

#[test]
fn redundant_else_not_flagged_when_else_introduces_binding() {
    // FP regression: the `else` block introduces `let y`, so de-indenting it
    // would move `y` into the enclosing scope.  Any same-name `let y` that
    // follows (as shown here) would then become a same-scope rebinding error.
    // The lint must stay silent even though the then-branch always diverges.
    let (errors, warnings) = parse_and_check(
        "fn f(c: bool) -> i64 { if c { return 0; } else { let _y = 1; } let _y = 2; _y }",
    );
    assert!(
        errors.is_empty(),
        "fixture should type-check cleanly, got: {errors:?}"
    );
    assert_eq!(
        count_redundant_else(&warnings),
        0,
        "else body introduces `let _y` — de-indent would change scope, must not lint, warnings: {warnings:?}"
    );
}

#[test]
fn redundant_else_deny_routes_to_errors() {
    let out = check_with_lint_level(
        "fn f(c: bool) { if c { return; } else { let _ = 1; } }",
        LintId::RedundantElseAfterReturn,
        LintLevel::Deny,
    );
    assert_eq!(
        count_redundant_else(&out.errors),
        1,
        "errors: {:?}",
        out.errors
    );
    assert_eq!(count_redundant_else(&out.warnings), 0);
}

#[test]
fn redundant_else_suppressed_by_directive() {
    const SOURCE: &str = "fn f(c: bool) {\n    // hew:allow(redundant_else_after_return)\n    if c { return; } else { let _ = 1; }\n}";
    let out = check_with_lint_level(SOURCE, LintId::RedundantElseAfterReturn, LintLevel::Warn);
    assert_eq!(
        count_redundant_else(&out.warnings),
        0,
        "a directive above the `if` must suppress, warnings: {:?}",
        out.warnings
    );
}

// ---- needless_match_to_if_let ----

fn count_needless_match(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::NeedlessMatchToIfLet))
        .count()
}

#[test]
fn needless_match_flags_some_none_empty() {
    let (errors, warnings) = parse_and_check(
        "fn f(o: Option<i64>) { match o { Some(x) => { let _ = x; } None => {} } }",
    );
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::NeedlessMatchToIfLet))
        .expect("expected a needless_match_to_if_let warning");
    assert_eq!(hit.kind.as_kind_str(), "needless_match_to_if_let");
    assert!(
        hit.suggestions.iter().any(|s| s.contains("if let Some(x)")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn needless_match_flags_none_first() {
    let (_, warnings) = parse_and_check(
        "fn f(o: Option<i64>) { match o { None => {} Some(x) => { let _ = x; } } }",
    );
    assert_eq!(
        count_needless_match(&warnings),
        1,
        "arm order must not matter, warnings: {warnings:?}"
    );
}

#[test]
fn needless_match_not_flagged_when_none_does_work() {
    let (_, warnings) = parse_and_check(
        "fn f(o: Option<i64>) { match o { Some(x) => { let _ = x; } None => { let _ = 0; } } }",
    );
    assert_eq!(
        count_needless_match(&warnings),
        0,
        "a None arm that does work is not an `if let`, warnings: {warnings:?}"
    );
}

#[test]
fn needless_match_not_flagged_with_guard() {
    let (_, warnings) = parse_and_check(
        "fn f(o: Option<i64>) { match o { Some(x) if x > 0 => { let _ = x; } None => {} _ => {} } }",
    );
    assert_eq!(
        count_needless_match(&warnings),
        0,
        "a guarded arm disqualifies the rewrite, warnings: {warnings:?}"
    );
}

#[test]
fn needless_match_not_flagged_in_value_position() {
    let (_, warnings) = parse_and_check(
        "fn f(o: Option<i64>) -> i64 { let y = match o { Some(x) => x, None => 0 }; y }",
    );
    assert_eq!(
        count_needless_match(&warnings),
        0,
        "a match feeding a `let` is in value position, warnings: {warnings:?}"
    );
}

#[test]
fn needless_match_deny_routes_to_errors() {
    let out = check_with_lint_level(
        "fn f(o: Option<i64>) { match o { Some(x) => { let _ = x; } None => {} } }",
        LintId::NeedlessMatchToIfLet,
        LintLevel::Deny,
    );
    assert_eq!(
        count_needless_match(&out.errors),
        1,
        "errors: {:?}",
        out.errors
    );
    assert_eq!(count_needless_match(&out.warnings), 0);
}

#[test]
fn needless_match_suppressed_by_directive() {
    const SOURCE: &str = "fn f(o: Option<i64>) {\n    // hew:allow(needless_match_to_if_let)\n    match o { Some(x) => { let _ = x; } None => {} }\n}";
    let out = check_with_lint_level(SOURCE, LintId::NeedlessMatchToIfLet, LintLevel::Warn);
    assert_eq!(
        count_needless_match(&out.warnings),
        0,
        "a directive above the match must suppress, warnings: {:?}",
        out.warnings
    );
}

// ---- len_zero_comparison ----

fn count_len_zero(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::LenZeroComparison))
        .count()
}

#[test]
fn len_zero_flags_eq_zero_as_is_empty() {
    let (errors, warnings) = parse_and_check("fn f(xs: Vec<i64>) -> bool { xs.len() == 0 }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::LenZeroComparison))
        .expect("expected a len_zero_comparison warning");
    assert_eq!(hit.kind.as_kind_str(), "len_zero_comparison");
    assert!(
        hit.suggestions.iter().any(|s| s.contains("xs.is_empty()")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn len_zero_flags_gt_zero_as_not_empty() {
    let (_, warnings) = parse_and_check("fn f(xs: Vec<i64>) -> bool { xs.len() > 0 }");
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::LenZeroComparison))
        .expect("len() > 0 should fire");
    assert!(
        hit.suggestions.iter().any(|s| s.contains("!xs.is_empty()")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn len_zero_flags_literal_on_left() {
    let (_, warnings) = parse_and_check("fn f(xs: Vec<i64>) -> bool { 0 == xs.len() }");
    assert_eq!(
        count_len_zero(&warnings),
        1,
        "operand order must not matter, warnings: {warnings:?}"
    );
}

#[test]
fn len_zero_flags_ge_one_as_not_empty() {
    let (_, warnings) = parse_and_check("fn f(xs: Vec<i64>) -> bool { xs.len() >= 1 }");
    assert_eq!(
        count_len_zero(&warnings),
        1,
        "len() >= 1 is a non-emptiness test, warnings: {warnings:?}"
    );
}

#[test]
fn len_zero_not_flagged_for_eq_one() {
    let (_, warnings) = parse_and_check("fn f(xs: Vec<i64>) -> bool { xs.len() == 1 }");
    assert_eq!(
        count_len_zero(&warnings),
        0,
        "len() == 1 is not an emptiness test, warnings: {warnings:?}"
    );
}

#[test]
fn len_zero_not_flagged_for_gt_one() {
    let (_, warnings) = parse_and_check("fn f(xs: Vec<i64>) -> bool { xs.len() > 1 }");
    assert_eq!(
        count_len_zero(&warnings),
        0,
        "len() > 1 is not an emptiness test, warnings: {warnings:?}"
    );
}

#[test]
fn len_zero_not_flagged_for_len_field() {
    // `len` is a record field here, not the collection method — the rewrite to
    // `is_empty()` does not apply.
    let (errors, warnings) =
        parse_and_check("type Buf { len: i64 }\nfn f(b: Buf) -> bool { b.len == 0 }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_len_zero(&warnings),
        0,
        "a `len` field access must not fire, warnings: {warnings:?}"
    );
}

#[test]
fn len_zero_flags_string_with_stdlib() {
    let (errors, warnings) =
        parse_and_check_with_stdlib("fn f(s: string) -> bool { s.len() == 0 }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    assert_eq!(
        count_len_zero(&warnings),
        1,
        "string exposes is_empty, warnings: {warnings:?}"
    );
}

#[test]
fn len_zero_deny_routes_to_errors() {
    let out = check_with_lint_level(
        "fn f(xs: Vec<i64>) -> bool { xs.len() == 0 }",
        LintId::LenZeroComparison,
        LintLevel::Deny,
    );
    assert_eq!(count_len_zero(&out.errors), 1, "errors: {:?}", out.errors);
    assert_eq!(count_len_zero(&out.warnings), 0);
}

#[test]
fn len_zero_suppressed_by_directive() {
    const SOURCE: &str =
        "fn f(xs: Vec<i64>) -> bool {\n    // hew:allow(len_zero_comparison)\n    xs.len() == 0\n}";
    let out = check_with_lint_level(SOURCE, LintId::LenZeroComparison, LintLevel::Warn);
    assert_eq!(
        count_len_zero(&out.warnings),
        0,
        "a directive above the comparison must suppress, warnings: {:?}",
        out.warnings
    );
}

#[test]
fn len_zero_hashmap_len_not_flagged() {
    // FP regression: HashMap has no `is_empty()` method in the dispatch
    // registry (HashMapMethod has no IsEmpty variant), so the suggested
    // `m.is_empty()` rewrite would fail at MIR lowering.  The lint must
    // not fire on HashMap receivers.
    let (errors, warnings) =
        parse_and_check("fn f(m: HashMap<string, i64>) -> bool { m.len() == 0 }");
    assert!(
        errors.is_empty(),
        "fixture should type-check cleanly, got: {errors:?}"
    );
    assert_eq!(
        count_len_zero(&warnings),
        0,
        "HashMap has no is_empty() — lint must not fire, warnings: {warnings:?}"
    );
}

// ---- needless_bool ----

fn count_needless_bool(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::NeedlessBool))
        .count()
}

#[test]
fn needless_bool_flags_true_false() {
    let (errors, warnings) =
        parse_and_check("fn f(c: bool) -> bool { if c { true } else { false } }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::NeedlessBool))
        .expect("expected a needless_bool warning");
    assert_eq!(hit.kind.as_kind_str(), "needless_bool");
    assert!(
        hit.suggestions.iter().any(|s| s.contains("condition")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn needless_bool_flags_false_true() {
    let (_, warnings) = parse_and_check("fn f(c: bool) -> bool { if c { false } else { true } }");
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::NeedlessBool))
        .expect("inverted polarity should fire");
    assert!(
        hit.suggestions.iter().any(|s| s.contains("negated")),
        "suggestions: {:?}",
        hit.suggestions
    );
}

#[test]
fn needless_bool_not_flagged_for_same_polarity() {
    let (_, warnings) = parse_and_check("fn f(c: bool) -> bool { if c { true } else { true } }");
    assert_eq!(
        count_needless_bool(&warnings),
        0,
        "matching branches are a constant, not needless_bool, warnings: {warnings:?}"
    );
}

#[test]
fn needless_bool_not_flagged_with_extra_statement() {
    let (_, warnings) =
        parse_and_check("fn f(c: bool) -> bool { if c { let _ = 1; true } else { false } }");
    assert_eq!(
        count_needless_bool(&warnings),
        0,
        "an extra statement in a branch disqualifies it, warnings: {warnings:?}"
    );
}

#[test]
fn needless_bool_not_flagged_for_non_bool_branches() {
    let (_, warnings) = parse_and_check("fn f(c: bool) -> i64 { if c { 1 } else { 0 } }");
    assert_eq!(
        count_needless_bool(&warnings),
        0,
        "integer branches are not boolean literals, warnings: {warnings:?}"
    );
}

#[test]
fn needless_bool_not_flagged_for_else_if() {
    // The outer `if`'s else is an `else if`, so the outer is never collapsed;
    // the inner `if d { false } else { false }` is the same polarity, so it is
    // not collapsed either. Net: an else-if chain produces no suggestion.
    let (_, warnings) = parse_and_check(
        "fn f(c: bool, d: bool) -> bool { if c { true } else if d { false } else { false } }",
    );
    assert_eq!(
        count_needless_bool(&warnings),
        0,
        "an else-if chain is not the two-branch shape, warnings: {warnings:?}"
    );
}

#[test]
fn needless_bool_deny_routes_to_errors() {
    let out = check_with_lint_level(
        "fn f(c: bool) -> bool { if c { true } else { false } }",
        LintId::NeedlessBool,
        LintLevel::Deny,
    );
    assert_eq!(
        count_needless_bool(&out.errors),
        1,
        "errors: {:?}",
        out.errors
    );
    assert_eq!(count_needless_bool(&out.warnings), 0);
}

#[test]
fn needless_bool_suppressed_by_directive() {
    const SOURCE: &str =
        "fn f(c: bool) -> bool {\n    // hew:allow(needless_bool)\n    if c { true } else { false }\n}";
    let out = check_with_lint_level(SOURCE, LintId::NeedlessBool, LintLevel::Warn);
    assert_eq!(
        count_needless_bool(&out.warnings),
        0,
        "a directive above the `if` must suppress, warnings: {:?}",
        out.warnings
    );
}

// ---- migrated warnings: clone_on_copy ----

fn count_clone_on_copy(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::CloneOnCopy))
        .count()
}

#[test]
fn clone_on_copy_warns_by_default() {
    let (errors, warnings) = parse_and_check("fn f(x: i64) { let _ = x.clone(); }");
    assert!(
        errors.is_empty(),
        "fixture should type-check, got: {errors:?}"
    );
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::CloneOnCopy))
        .expect("clone on a Copy type should warn by default");
    assert_eq!(hit.kind.as_kind_str(), "clone_on_copy");
    assert_eq!(hit.severity, crate::error::Severity::Warning);
}

#[test]
fn clone_on_copy_suppressed_by_allow() {
    let out = check_with_lint_level(
        "fn f(x: i64) { let _ = x.clone(); }",
        LintId::CloneOnCopy,
        LintLevel::Allow,
    );
    assert_eq!(count_clone_on_copy(&out.warnings), 0);
    assert_eq!(count_clone_on_copy(&out.errors), 0);
}

#[test]
fn clone_on_copy_deny_routes_to_errors() {
    let out = check_with_lint_level(
        "fn f(x: i64) { let _ = x.clone(); }",
        LintId::CloneOnCopy,
        LintLevel::Deny,
    );
    assert_eq!(
        count_clone_on_copy(&out.errors),
        1,
        "errors: {:?}",
        out.errors
    );
    assert_eq!(count_clone_on_copy(&out.warnings), 0);
}

#[test]
fn clone_on_copy_suppressed_by_directive() {
    const SOURCE: &str =
        "fn f(x: i64) {\n    // hew:allow(clone_on_copy)\n    let _ = x.clone();\n}";
    let out = check_with_lint_level(SOURCE, LintId::CloneOnCopy, LintLevel::Warn);
    assert_eq!(
        count_clone_on_copy(&out.warnings),
        0,
        "a directive above the clone must suppress, warnings: {:?}",
        out.warnings
    );
}

// ---- migrated warnings: dead_code ----

fn count_dead_code(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::DeadCode))
        .count()
}

#[test]
fn dead_code_deny_routes_to_errors() {
    let out = check_with_lint_level(
        "fn helper() {}\nfn main() {}",
        LintId::DeadCode,
        LintLevel::Deny,
    );
    assert_eq!(count_dead_code(&out.errors), 1, "errors: {:?}", out.errors);
    assert_eq!(count_dead_code(&out.warnings), 0);
}

#[test]
fn dead_code_suppressed_by_allow() {
    let out = check_with_lint_level(
        "fn helper() {}\nfn main() {}",
        LintId::DeadCode,
        LintLevel::Allow,
    );
    assert_eq!(count_dead_code(&out.warnings), 0);
    assert_eq!(count_dead_code(&out.errors), 0);
}

#[test]
fn dead_code_suppressed_by_directive() {
    const SOURCE: &str = "// hew:allow(dead_code)\nfn helper() {}\nfn main() {}";
    let out = check_with_lint_level(SOURCE, LintId::DeadCode, LintLevel::Warn);
    assert_eq!(
        count_dead_code(&out.warnings),
        0,
        "a directive above the dead function must suppress, warnings: {:?}",
        out.warnings
    );
}

#[test]
fn unused_variable_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("unused"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedVariable),
        "expected UnusedVariable kind, got: {:?}",
        w.kind
    );
}

#[test]
fn never_mutated_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedMut),
        "expected UnusedMut kind, got: {:?}",
        w.kind
    );
}

// ---- multiple warnings in one function ----

#[test]
fn multiple_warnings_in_one_fn() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let unused_a = 1;\n",
        "    var never_written = 2;\n",
        "    println(never_written);\n",
        "    while true { break; }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_a`")),
        "missing unused_a warning: {warnings:?}"
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "missing never-mutated warning: {warnings:?}"
    );
    assert!(
        warnings.iter().any(|w| w.message.contains("while true")),
        "missing while-true warning: {warnings:?}"
    );
}

// ── Unreachable Code Tests ──────────────────────────────────────────

#[test]
fn warn_unreachable_after_return() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_unreachable_when_return_is_last() {
    let source = "fn foo() -> i32 { let x = 1; return x; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "should not warn when return is last statement: {:?}",
        output.warnings
    );
}

// ── Shadowing Tests ─────────────────────────────────────────────────

#[test]
fn error_same_scope_shadowing() {
    let source = "fn main() { let x = 5; let x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing
                && e.message.contains("already defined in this scope")),
        "expected same-scope shadowing error, got: {:?}",
        output.errors
    );
}

#[test]
fn warn_nested_scope_shadowing() {
    // Nested/child scope shadowing is a warning, not an error.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing
                && w.message.contains("shadows a binding in an outer scope")),
        "expected outer-scope shadowing warning, got warnings: {:?}, errors: {:?}",
        output.warnings,
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {:?}",
        output.errors
    );
}

#[test]
fn no_shadowing_diagnostic_underscore_prefix() {
    // Underscore-prefixed bindings are fully exempt from shadowing diagnostics.
    let source = "fn main() { let _x = 1; if true { let _x = 2; println(_x); } println(_x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for _ prefixed vars: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for _ prefixed vars: {:?}",
        output.warnings
    );
}

#[test]
fn no_shadowing_diagnostic_for_loop_var() {
    // For-loop induction variables are exempt from shadowing diagnostics.
    let source = "fn main() { let i = 0; for i in 0..10 { println(i); } println(i); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for for-loop variable shadowing: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for for-loop variable shadowing: {:?}",
        output.warnings
    );
}

#[test]
fn warn_deeply_nested_scope_shadowing() {
    // Shadowing across multiple scope levels is still a warning.
    let source = r"
        fn main() {
            let val = 1;
            if true {
                if true {
                    let val = 99;
                    println(val);
                }
            }
            println(val);
        }
    ";
    let (errors, warnings) = parse_and_check(source);
    assert!(
        warnings.iter().any(|w| w.kind == TypeErrorKind::Shadowing
            && w.message.contains("shadows a binding in an outer scope")),
        "expected nested shadowing warning, got warnings: {warnings:?}, errors: {errors:?}"
    );
    assert!(
        !errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {errors:?}"
    );
}

#[test]
fn test_actor_field_shadowing_is_error() {
    // Shadowing an actor field is a hard error — bare field access requires
    // unambiguous names.
    let source = r"
        actor Counter {
            var count: i64 = 0;
            receive fn update(count: i64) {
                println(count);
            }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on param shadowing actor field, got: {errors:?}",
    );
}

#[test]
fn test_actor_fn_method_field_shadowing_is_error() {
    // Shadowing an actor field via an fn helper method is also a hard error.
    let source = r"
        actor Counter {
            var count: i64 = 0;
            fn helper(count: i64) -> i64 { count }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on fn param shadowing actor field, got: {errors:?}",
    );
}

#[test]
fn warn_nested_scope_shadowing_of_free_fn_param() {
    // Regression for PR #240 review finding: a nested local shadowing a
    // *user-visible parameter* must be downgraded to a warning, the same
    // treatment given to shadowing a `let`-declared local (see
    // `warn_nested_scope_shadowing`). Before the fix, function/receive-fn/
    // init/hook parameters were bound via `env.define` (no source span),
    // which is indistinguishable from the actor-field synthetic-binding case
    // that intentionally stays a hard error, so this shadowing was wrongly
    // rejected.
    let source = "fn f(x: i64) -> i64 { if true { let x = 2; return x; } return x; }";
    let (errors, warnings) = parse_and_check(source);
    assert!(
        !errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing),
        "shadowing a free-fn parameter should not be a hard error, got errors: {errors:?}",
    );
    assert!(
        warnings.iter().any(|w| w.kind == TypeErrorKind::Shadowing
            && w.message.contains("shadows a binding in an outer scope")),
        "expected outer-scope shadowing warning for parameter `x`, got warnings: {warnings:?}",
    );
}

#[test]
fn warn_nested_scope_shadowing_of_receive_fn_param() {
    // Same regression as `warn_nested_scope_shadowing_of_free_fn_param`, for
    // the actor `receive fn` parameter surface the review finding also named
    // explicitly (hew-types/src/check/items.rs, receive-fn param binding).
    let source = r#"
        actor Greeter {
            receive fn greet(name: string) {
                if true {
                    let name = "nested";
                    println(name);
                }
                println(name);
            }
        }
    "#;
    let (errors, warnings) = parse_and_check(source);
    assert!(
        !errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing),
        "shadowing a receive-fn parameter should not be a hard error, got errors: {errors:?}",
    );
    assert!(
        warnings.iter().any(|w| w.kind == TypeErrorKind::Shadowing
            && w.message.contains("shadows a binding in an outer scope")),
        "expected outer-scope shadowing warning for parameter `name`, got warnings: {warnings:?}",
    );
}

#[test]
fn unused_free_fn_param_is_not_warned() {
    // Parameters keep `def_span: None` (see `define_param_with_span`) even
    // though they now carry a `shadow_span` for the shadowing fix above, so
    // they must stay exempt from the `UnusedVariable` scope-exit lint exactly
    // as before this change — only the shadowing *classification* changed.
    let source = "fn f(unused: i64) -> i64 { return 5; }";
    let (errors, warnings) = parse_and_check(source);
    assert!(
        !warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedVariable),
        "unused parameters must not trigger UnusedVariable, got warnings: {warnings:?}",
    );
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn actor_this_field_points_to_bare_state_field() {
    let source = r"
        actor Counter {
            let count: i64;
            receive fn get() -> i64 {
                this.count
            }
        }
    ";
    let (errors, _) = parse_and_check(source);
    assert!(
        errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedField
                && error.message.contains("`this` is the actor handle")
                && error.message.contains("not `this.count`")
                && error
                    .suggestions
                    .iter()
                    .any(|suggestion| suggestion == "count")
        }),
        "`this.field` in actor body should suggest bare field access; got: {errors:?}",
    );
}

// ── Dead Code (Unused Function) Tests ───────────────────────────────

#[test]
fn warn_dead_code_unused_function() {
    let source = "fn unused_helper() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Lint(LintId::DeadCode)
                && w.message.contains("unused_helper")),
        "expected dead code warning for unused_helper, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_main() {
    let source = "fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Lint(LintId::DeadCode)),
        "should not warn about main: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_function() {
    let source = "fn helper() -> i32 { 42 } fn main() { let x = helper(); println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.warnings.iter().any(
            |w| w.kind == TypeErrorKind::Lint(LintId::DeadCode) && w.message.contains("helper")
        ),
        "should not warn about called function: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_underscore_prefix() {
    let source = "fn _unused() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Lint(LintId::DeadCode)),
        "should not warn for _ prefixed functions: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_from_actor_receive() {
    let source = r"
fn fib(n: i32) -> i32 {
if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
}
actor Worker {
receive fn compute(n: i32) {
    let r = fib(n);
    println(r);
}
}
fn main() {
let w = spawn Worker();
w.compute(10);
}
";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Lint(LintId::DeadCode) && w.message.contains("fib")),
        "should not warn about function called from actor receive fn: {:?}",
        output.warnings
    );
}

// ── Unused Import Tests ─────────────────────────────────────────────

#[test]
fn warn_unused_import() {
    let source = "import std::encoding::json;\nfn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "expected unused import warning for json, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_used_import() {
    let source =
        "import std::encoding::json;\nfn main() { let v = json.parse(\"[]\"); println(v); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "should not warn about used import: {:?}",
        output.warnings
    );
}

#[test]
fn stdlib_json_supertrait_import_does_not_warn_value_trait_unused() {
    let source =
        "import std::encoding::json;\nfn main() { let v = json.parse(\"[]\"); println(v); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        !output.warnings.iter().any(|w| {
            w.kind == TypeErrorKind::UnusedImport && w.message.contains("value_trait")
        }),
        "json's CanonicalValueMethods supertrait use must consume its value_trait import: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_named_import_type_used_bare() {
    // A named import (`::{ T }`) of a type used only as a bare type reference
    // must mark the module used — qualified-by-default routes bare references
    // through the published binding, which must still consume the import so the
    // unused-import lint does not false-positive.
    let source = "import std::io::closable::{ CloseError };\n\
                  fn handle(e: CloseError) -> i64 { 0 }\n\
                  fn main() { let _ = handle; println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("closable")),
        "named import used via a bare type reference must not warn unused: {:?}",
        output.warnings
    );
}

#[test]
fn warn_named_import_type_unused() {
    // The complement: a named import whose type is never referenced still warns.
    let source = "import std::io::closable::{ CloseError };\nfn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("closable")),
        "an unused named import must still warn: {:?}",
        output.warnings
    );
}

#[test]
fn stdlib_import_registers_trait_impls_for_generic_bounds() {
    let root_source = r"
        import std::string;

        fn main() -> string {
            string.describe(string.make_label())
        }
    ";
    let module_source = r#"
        pub trait Describable {
            fn describe(val: Self) -> string;
        }

        pub type Label {
            text: string;
        }

        pub fn make_label() -> Label {
            Label { text: "hello" }
        }

        impl Describable for Label {
            fn describe(label: Label) -> string {
                label.text
            }
        }

        pub fn describe<T: Describable>(item: T) -> string {
            item.describe()
        }

    "#;

    let mut root = hew_parser::parse(root_source);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );
    let call_span = root
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.trailing_expr.as_ref().map(|expr| expr.1.clone())
            }
            _ => None,
        })
        .expect("main trailing call should exist");
    let module = hew_parser::parse(module_source);
    assert!(
        module.errors.is_empty(),
        "module parse errors: {:?}",
        module.errors
    );

    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root import should exist");
    import_decl.resolved_items = Some(module.program.items.clone());

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("string"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.errors.is_empty(),
        "stdlib imported Hew impl should satisfy imported generic bounds: {:?}",
        output.errors
    );
    let inferred = output
        .call_type_args
        .get(&SpanKey::from(&call_span))
        .expect("stdlib imported generic call should record inferred type args");
    assert_eq!(
        inferred,
        &vec![Ty::Named {
            builtin: None,
            name: "Label".to_string(),
            args: vec![],
        }]
    );
    assert!(
        checker
            .trait_impls_set
            .contains(&("Label".to_string(), "Describable".to_string())),
        "stdlib Hew items should register trait impls for downstream generic bound checks"
    );
}

#[test]
fn impl_for_primitive_int_populates_primitive_trait_impl_table() {
    // Stage A1: `impl Display for i64` registers under the canonical `i64`
    // key (the lowering name for `Ty::I64`) so receiver-keyed dispatch can
    // find it later.  The literal AST string `i64` must round-trip through
    // `Ty::from_name` → `canonical_lowering_name` to agree with the
    // dispatch site, which only ever sees a resolved `Ty`.
    let source = r#"
        pub trait Display {
            fn fmt(val: Self) -> string;
        }

        impl Display for i64 {
            fn fmt(n: i64) -> string {
                ""
            }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(test_registry());
    let _output = checker.check_program(&parsed.program);

    let methods = checker
        .primitive_trait_impls
        .get(&("i64".to_string(), "Display".to_string()))
        .expect("primitive trait impl table should have entry for (i64, Display)");
    let fmt_sig = methods
        .get("fmt")
        .expect("fmt method should be recorded for impl Display for i64");
    assert!(
        fmt_sig.params.is_empty(),
        "receiver should be filtered: {:?}",
        fmt_sig.params
    );
    assert_eq!(fmt_sig.return_type, Ty::String);
}

#[test]
fn impl_for_builtin_vec_populates_primitive_trait_impl_table() {
    // Vec is a compiler-builtin generic with no `type_defs` entry; user
    // impls on Vec must reach the side table the same way primitives do.
    let source = r#"
        pub trait Display {
            fn fmt(val: Self) -> string;
        }

        impl Display for Vec<i32> {
            fn fmt(v: Vec<i32>) -> string {
                ""
            }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(test_registry());
    let _output = checker.check_program(&parsed.program);

    assert!(
        checker
            .primitive_trait_impls
            .contains_key(&("Vec".to_string(), "Display".to_string())),
        "primitive trait impl table should record impls keyed on the bare \
         builtin generic name (Vec) regardless of element type"
    );
}

#[test]
fn impl_for_user_struct_does_not_pollute_primitive_trait_impl_table() {
    // The side table must stay empty for user-defined struct receivers —
    // those flow through `type_defs` and would create duplicate dispatch
    // paths if the helper accepted them.
    let source = r#"
        pub trait Display {
            fn fmt(val: Self) -> string;
        }

        pub type MyType {
            value: i64;
        }

        impl Display for MyType {
            fn fmt(m: MyType) -> string {
                ""
            }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(test_registry());
    let _output = checker.check_program(&parsed.program);

    // The side table is non-empty after `register_builtins` because the ten
    // `std/builtins.hew` Display blanket impls (i8/i16/i32/i64/u8/u16/u32/u64/
    // bool/char) land here as the source-of-truth dispatch entries (see
    // `register_builtins_hew_impls`).  The invariant this test enforces is
    // narrower: a user `impl Display for MyType` (where `MyType` is a
    // user-declared struct) must NOT leak into this primitive-keyed table —
    // user-struct method dispatch goes through `type_defs.methods` instead.
    let leaked: Vec<_> = checker
        .primitive_trait_impls
        .keys()
        .filter(|(rx_key, _)| rx_key == "MyType")
        .collect();
    assert!(
        leaked.is_empty(),
        "user struct impls must not leak into the primitive trait table: {leaked:?}"
    );
}

/// Stage A2: receiver-kind dispatch matrix.
///
/// Each row exercises one canonical receiver kind (primitive or
/// compiler-builtin generic) plus a user `impl Display for <kind>` and
/// asserts that `x.fmt()` typechecks cleanly and records a
/// `MethodCallReceiverKind::PrimitiveTraitImpl` metadata entry.  The
/// metadata is the checker→codegen output-boundary contract; if a
/// receiver kind dispatches via the new path but the metadata is not
/// recorded, downstream codegen has no way to find the resolved impl
/// (per the `checker-output-boundary` P0 lesson).
fn assert_primitive_trait_dispatch_records_metadata(
    source: &str,
    expected_canonical: &str,
    expected_trait: &str,
) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors for source `{source}`: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean type check for source:\n{source}\n\nbut got: {:?}",
        output.errors
    );
    let any = output
        .method_call_receiver_kinds
        .values()
        .any(|kind| match kind {
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver,
            } => trait_name == expected_trait && canonical_receiver == expected_canonical,
            _ => false,
        });
    assert!(
        any,
        "expected PrimitiveTraitImpl{{trait_name={expected_trait}, canonical_receiver={expected_canonical}}} \
         in method_call_receiver_kinds, found: {:?}",
        output.method_call_receiver_kinds.values().collect::<Vec<_>>()
    );
}

#[test]
fn primitive_impl_dispatch_resolves_int_receiver() {
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let x: i64 = 42;
                let _ = x.fmt();
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_bool_receiver() {
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for bool {
                fn fmt(b: bool) -> string { "" }
            }
            fn main() {
                let b: bool = true;
                let _ = b.fmt();
            }
        "#,
        "bool",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_char_receiver() {
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for char {
                fn fmt(c: char) -> string { "" }
            }
            fn main() {
                let c: char = 'a';
                let _ = c.fmt();
            }
        "#,
        "char",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_i32_receiver() {
    // Width-aliased integer kinds dispatch through the same canonical key
    // as their `Ty::I32` variant — `i32` here, not `i64`.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i32 {
                fn fmt(n: i32) -> string { "" }
            }
            fn main() {
                let n: i32 = 7;
                let _ = n.fmt();
            }
        "#,
        "i32",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_string_receiver_via_method_using_trait_method() {
    // string routes through the declarative receiver dispatch fallback, not
    // the wildcard.  This sentinel guarantees that path consults the side
    // table for a method name that does NOT collide with a builtin string
    // method (so the not-found branch is the one that runs).  We use a Display
    // method named `to_display_string` to avoid the impl-on-string body
    // type-check issue tracked separately (see issue #1565 follow-ups).
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait MyShow { fn show(val: Self) -> i64; }
            impl MyShow for string {
                fn show(s: string) -> i64 { 0 }
            }
            fn main() {
                let s: string = "hi";
                let _: i64 = s.show();
            }
        "#,
        "string",
        "MyShow",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_vec_receiver() {
    // Vec routes through `check_vec_method`'s not-found arm.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for Vec<i32> {
                fn fmt(v: Vec<i32>) -> string { "" }
            }
            fn main() {
                let v: Vec<i32> = Vec::new();
                let _ = v.fmt();
            }
        "#,
        "Vec",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_preserves_builtin_numeric_conversion() {
    // R5 mitigation: the side table is consulted only when the compiler
    // builtin returns "no match". Numeric exact-conversion methods flow
    // through their checker-owned dispatch arm. With a user `Display for i32`
    // in scope, calling the builtin must still resolve to `Option<i64>`, not
    // be hijacked into the user trait's `fmt` method.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        impl Display for i32 {
            fn fmt(n: i32) -> string { "" }
        }
        fn main() {
            let n: i32 = 7;
            let _: Option<i64> = n.try_to_i64();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "builtin i32.try_to_i64 regressed under Stage A2 dispatch: {:?}",
        output.errors
    );
}

#[test]
fn primitive_impl_dispatch_resolves_ufcs_form_for_int_receiver() {
    // Stage A3: `Display::fmt(x)` on a primitive receiver must resolve
    // identically to `x.fmt()`.  Today the trait method registers in
    // `fn_sigs` with the receiver param stripped, so the existing
    // `fn_sigs` lookup mis-arities the call (0 expected vs 1 supplied).
    // The UFCS dispatcher intercepts before that lookup.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let x: i64 = 42;
                let _ = Display::fmt(x);
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_ufcs_form_with_extra_args() {
    // UFCS receiver-and-trailing-args case: `Trait::method(receiver,
    // arg1, arg2)`.  The receiver-stripped sig has params=[string], so
    // the call takes 2 args total (receiver + arg1) and the sig is
    // applied to the trailing args after the receiver is consumed.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Show { fn show(val: Self, suffix: string) -> string; }
            impl Show for i64 {
                fn show(n: i64, suffix: string) -> string { suffix }
            }
            fn main() {
                let x: i64 = 42;
                let _: string = Show::show(x, "!");
            }
        "#,
        "i64",
        "Show",
    );
}

#[test]
fn pub_type_receiver_with_user_trait_impl_still_dispatches_via_existing_path() {
    // Stage A4 regression sentinel: `pub type Foo` with `impl Display for
    // Foo` must continue to dispatch through the existing `type_defs`
    // method registry — it must NOT be hijacked into the primitive
    // side table (the primitive table only houses receivers that
    // `type_defs` cannot reach).  The receiver-kind metadata for the
    // call must be `NamedTypeInstance`, not `PrimitiveTraitImpl`.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        pub type Foo {
            value: i64;
        }
        impl Display for Foo {
            fn fmt(f: Foo) -> string { "" }
        }
        fn main() {
            let f: Foo = Foo { value: 1 };
            let _: string = f.fmt();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "user `pub type` + impl Display dispatch regressed: {:?}",
        output.errors
    );
    let dispatched_via_named = output.method_call_receiver_kinds.values().any(|kind| {
        matches!(
            kind,
            MethodCallReceiverKind::NamedTypeInstance { type_name } if type_name == "Foo"
        )
    });
    let leaked_into_primitive_table = output
        .method_call_receiver_kinds
        .values()
        .any(|kind| matches!(kind, MethodCallReceiverKind::PrimitiveTraitImpl { .. }));
    assert!(
        dispatched_via_named,
        "expected NamedTypeInstance{{type_name=Foo}} metadata for f.fmt(); got: {:?}",
        output
            .method_call_receiver_kinds
            .values()
            .collect::<Vec<_>>()
    );
    assert!(
        !leaked_into_primitive_table,
        "user struct dispatch leaked into primitive trait table: {:?}",
        output
            .method_call_receiver_kinds
            .values()
            .collect::<Vec<_>>()
    );
}

#[test]
fn ufcs_on_pub_type_receiver_does_not_record_primitive_trait_impl_metadata() {
    // Regression for the synthesize-before-short-circuit ordering bug.
    //
    // When `Display::fmt(f)` is called and `f` is a `pub type` (non-primitive)
    // receiver, the UFCS helper must return `None` without recording
    // `PrimitiveTraitImpl` metadata.  Before the fix, the helper could
    // still synthesise the first arg and — on a route that should belong
    // entirely to the existing type-def path — leave stale side-effects.
    //
    // The short-circuit added by this fix ensures that when `Display` has
    // no primitive impls registered (the trait is purely user-defined here),
    // the helper exits before synthesising `first_arg`, preventing any
    // double-processing of the receiver expression.
    //
    // `UserDisplay` is declared without any primitive blanket impls, so
    // the helper returns `None` immediately.  The test asserts no
    // `PrimitiveTraitImpl` metadata is recorded — the call is handled
    // entirely by the receiver-form dispatch path.
    let source = r#"
        pub trait UserDisplay { fn show(val: Self) -> string; }
        pub type Widget { value: i64; }
        impl UserDisplay for Widget {
            fn show(w: Widget) -> string { "" }
        }
        fn main() {
            let w: Widget = Widget { value: 1 };
            let _: string = w.show();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors for UserDisplay on pub type: {:?}",
        output.errors
    );
    let leaked_primitive_meta = output
        .method_call_receiver_kinds
        .values()
        .any(|kind| matches!(kind, MethodCallReceiverKind::PrimitiveTraitImpl { .. }));
    assert!(
        !leaked_primitive_meta,
        "UserDisplay (no primitive impls) must not produce PrimitiveTraitImpl metadata: {:?}",
        output
            .method_call_receiver_kinds
            .values()
            .collect::<Vec<_>>()
    );
}

#[test]
fn ufcs_over_applied_call_emits_exactly_one_arity_diagnostic() {
    // Regression for the double-arity-diagnostic bug.
    //
    // `Display::fmt(x, extra)` should produce exactly one arity error.
    // The old code ran both an explicit outer `check_arity(args, params+1)`
    // and then `apply_instantiated_call_signature` which internally calls
    // `check_arity(trailing_args, params)` via PositionalOnly — two
    // different checks for the same call, two different messages.
    //
    // The fix removes the redundant outer check_arity, leaving only the
    // inner one, matching the receiver-form path's behaviour.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        impl Display for i64 {
            fn fmt(n: i64) -> string { "" }
        }
        fn main() {
            let x: i64 = 42;
            let _ = Display::fmt(x, "extra");
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    let arity_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("argument"))
        .collect();
    assert_eq!(
        arity_errors.len(),
        1,
        "expected exactly 1 arity diagnostic for Display::fmt(x, extra), got {}: {:?}",
        arity_errors.len(),
        arity_errors
    );
}

#[test]
fn primitive_impl_dispatch_unknown_method_still_emits_error() {
    // When no impl matches and no builtin method exists, the existing
    // "no method `<name>` on <kind>" diagnostic must still fire — the
    // helper returns None so the existing reporter runs.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        impl Display for i64 {
            fn fmt(n: i64) -> string { "" }
        }
        fn main() {
            let x: i64 = 42;
            let _ = x.no_such_method();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `no_such_method`")),
        "expected `no method` diagnostic, got: {:?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Slice 1 (#1668, #1669): literal-form receivers and builtins blanket impls
// ---------------------------------------------------------------------------

#[test]
fn primitive_trait_dispatch_int_literal_receiver() {
    // #1668 reproducer: an `IntLiteral` receiver (no enclosing typed binding)
    // must default to `i64` before the side-table lookup so `(42).fmt()`
    // resolves the same way `let x: i64 = 42; x.fmt()` does.  Pre-fix this
    // emitted `no method `fmt` on i64` because
    // `canonical_primitive_or_builtin_key` short-circuited on the literal.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let _ = (42).fmt();
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_float_literal_receiver() {
    // Mirror of the i64-literal case for `FloatLiteral`.  Defaulting must
    // collapse the literal to `f64` (via `materialize_literal_defaults`)
    // before canonical-key lookup.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for f64 {
                fn fmt(x: f64) -> string { "" }
            }
            fn main() {
                let _ = (3.14).fmt();
            }
        "#,
        "f64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_ufcs_int_literal() {
    // Stage A3 (UFCS) sentinel: `Display::fmt(42)` must default the
    // synthesized receiver `IntLiteral` the same way Stage A2 does for
    // method-form.  Without UFCS-side defaulting the helper returns None
    // and the trait-qualified path mis-arities (sig.params=[] vs args=[42]).
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let _ = Display::fmt(42);
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_no_redeclare() {
    // #1669 reproducer: with no in-file `trait Display` declaration, the
    // ten `std/builtins.hew` blanket impls must already be reachable so
    // `let x: i64 = 42; x.fmt()` typechecks.  This is the "registration
    // boundary" fix — `register_builtins_hew_impls` populates
    // `primitive_trait_impls` from the compiled-in stdlib source.
    assert_primitive_trait_dispatch_records_metadata(
        r"
            fn main() {
                let x: i64 = 42;
                let _ = x.fmt();
            }
        ",
        "i64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_records_rewrite_for_builtin_fmt() {
    let parsed = hew_parser::parse(
        r"
            fn main() {
                let x: i64 = 42;
                let _ = x.fmt();
            }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "i64::fmt"
        )),
        "primitive-trait fmt dispatch must record a function rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_each_kind() {
    // One receiver per kind covered by std/builtins.hew lines 29–87.
    // Each row asserts `x.fmt()` resolves with NO in-file `trait Display`
    // declaration, exercising the builtins-blanket registration end to end.
    let cases: &[(&str, &str, &str)] = &[
        ("i8", "1 as i8", "i8"),
        ("i16", "1 as i16", "i16"),
        ("i32", "1 as i32", "i32"),
        ("i64", "1 as i64", "i64"),
        ("u8", "1 as u8", "u8"),
        ("u16", "1 as u16", "u16"),
        ("u32", "1 as u32", "u32"),
        ("u64", "1 as u64", "u64"),
        ("bool", "true", "bool"),
        ("char", "'a'", "char"),
    ];
    for (annotation, init, canonical) in cases {
        let source = format!(
            r"
                fn main() {{
                    let x: {annotation} = {init};
                    let _ = x.fmt();
                }}
            "
        );
        assert_primitive_trait_dispatch_records_metadata(&source, canonical, "Display");
    }
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_does_not_shadow_user_redeclare() {
    // Builtin-precedence sentinel: the audit's invariant is that a user's
    // in-file `trait Display { ... }` declaration continues to take
    // precedence over the builtins-blanket impls.  Concretely, if a user
    // redeclares Display with a *different* method name, the builtins
    // `fmt` lookup must NOT silently satisfy a call to that user method
    // name on a primitive receiver — it should still diagnose "no method
    // on i64".  This guards the registration ordering risk called out in
    // the design notes: builtins.hew impls land in the side table only; they
    // do not pollute `trait_defs` or hijack user names.
    let source = r"
        pub trait Display {
            fn render(val: Self) -> string;
        }
        fn main() {
            let x: i64 = 42;
            // `render` is the user trait's method name; no impl exists for i64.
            let _ = x.render();
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `render`")),
        "expected `no method `render`` diagnostic, got: {:?}",
        output.errors
    );
    // And the user's own trait redeclaration must still be present in
    // trait_defs (i.e. our builtins-blanket loader did NOT register
    // Display first and force the user declaration to be skipped).
    assert!(
        checker.trait_defs.contains_key("Display"),
        "user trait Display must remain registered; trait_defs keys: {:?}",
        checker.trait_defs.keys().collect::<Vec<_>>()
    );
}

#[test]
fn primitive_trait_dispatch_negative_non_display_method_still_diagnoses() {
    // Negative sentinel: a method name that no Display impl provides on
    // a primitive receiver must continue to emit the existing
    // `no method `<name>` on i64` diagnostic.  Defaulting the literal must
    // not silently swallow the not-found path.
    let source = r"
        fn main() {
            let _ = (42).no_such_method();
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `no_such_method`")),
        "expected `no method `no_such_method`` diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn print_user_struct_without_display_impl_is_rejected_by_checker() {
    // #1670 sentinel: user structs must not reach codegen's print lowering
    // unless the checker has resolved a Display impl for the printed type.
    // Without that bound, `print(Foo { ... })` should fail here rather than
    // relying on PrintOpLowering's unsupported-aggregate terminal.
    let source = r#"
        pub type Foo {
            label: string;
        }

        fn main() {
            print(Foo { label: "no display" });
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not implement trait `Display`")),
        "expected missing Display diagnostic for print(Foo {{ ... }}), got: {:?}",
        output.errors
    );
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_populates_side_table_at_register_builtins() {
    // White-box sentinel: confirm `register_builtins` alone (no
    // user source, no full `check_program`) seeds the
    // `primitive_trait_impls` side table with the ten builtins blanket
    // impls.  This guards against the registration boundary regressing
    // — if `register_builtins_hew_impls` ever stops being called, the
    // method-form Display dispatch silently regresses without any test
    // source needing to fail, so we pin the table contents here.
    let mut checker = Checker::new(test_registry());
    checker.register_builtins();
    let expected_canonical_keys = [
        "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "bool", "char",
    ];
    for key in expected_canonical_keys {
        let entry = checker
            .primitive_trait_impls
            .get(&(key.to_string(), "Display".to_string()))
            .unwrap_or_else(|| {
                panic!(
                    "missing builtins-blanket Display impl for primitive `{key}`; \
                     primitive_trait_impls keys: {:?}",
                    checker.primitive_trait_impls.keys().collect::<Vec<_>>()
                )
            });
        let fmt_sig = entry
            .get("fmt")
            .unwrap_or_else(|| panic!("missing fmt method for ({key}, Display) entry: {entry:?}"));
        assert!(
            fmt_sig.params.is_empty(),
            "receiver should be filtered from fmt sig for `{key}`: {fmt_sig:?}"
        );
        assert_eq!(
            fmt_sig.return_type,
            Ty::String,
            "fmt for `{key}` must return string, got: {:?}",
            fmt_sig.return_type
        );
    }
}

#[test]
fn duplicate_stdlib_import_with_same_resolved_source_does_not_reregister_items() {
    let mut root = hew_parser::parse(
        r"
            import std::bench;
            import std::bench;
        ",
    );
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let bench_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/bench/bench.hew")
        .canonicalize()
        .expect("stdlib bench module should exist");
    let bench_source =
        std::fs::read_to_string(&bench_path).expect("should read stdlib bench Hew source");
    let bench_module = hew_parser::parse(&bench_source);
    assert!(
        bench_module.errors.is_empty(),
        "bench parse errors: {:?}",
        bench_module.errors
    );

    for import_decl in root
        .program
        .items
        .iter_mut()
        .filter_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
    {
        import_decl.resolved_items = Some(bench_module.program.items.clone());
        import_decl.resolved_source_paths = vec![bench_path.clone()];
    }

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("bench"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.type_defs.contains_key("Suite"),
        "stdlib Hew items should still register public types"
    );
    assert!(
        output.fn_sigs.contains_key("bench.suite"),
        "stdlib Hew items should still register qualified functions"
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "duplicate stdlib imports should dedupe Hew item registration: {:?}",
        output.errors
    );
}

// ── Warning severity tests ──────────────────────────────────────────

#[test]
fn unreachable_code_has_warning_severity() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::UnreachableCode);
    assert!(w.is_some(), "expected unreachable warning");
    assert_eq!(
        w.unwrap().severity,
        crate::error::Severity::Warning,
        "unreachable code should have Warning severity"
    );
}

#[test]
fn shadowing_warning_has_note_for_original_definition() {
    // The warning for outer-scope shadowing must include a note pointing back
    // to where the original binding was defined.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Shadowing);
    assert!(w.is_some(), "expected shadowing warning");
    assert!(
        !w.unwrap().notes.is_empty(),
        "shadowing warning should have a note pointing to the original definition"
    );
}

// ── Bug fix regression tests ────────────────────────────────────────

#[test]
fn no_warn_unused_read_then_assign() {
    // Bug 1: var x = 0; println(x); x = 1; should NOT warn about unused x
    let (errors, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "read-then-assign should not produce unused warning, got: {warnings:?}"
    );
}

#[test]
fn warn_unreachable_after_if_all_branches_return() {
    // Bug 2: if where all branches return should mark subsequent code unreachable
    let (_, warnings) = parse_and_check(
        "fn foo() -> i32 { if true { return 1; } else { return 2; } let y = 3; y }",
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning after if with all-returning branches, got: {warnings:?}"
    );
}

#[test]
fn no_warn_dead_code_function_referenced_as_value() {
    // Bug 3: let f = helper; f(); should mark helper as called
    let (_, warnings) =
        parse_and_check("fn helper() -> i32 { 42 } fn main() { let f = helper; println(f); }");
    assert!(
        !warnings.iter().any(
            |w| w.kind == TypeErrorKind::Lint(LintId::DeadCode) && w.message.contains("helper")
        ),
        "function referenced as value should not get dead code warning, got: {warnings:?}"
    );
}

#[test]
fn warn_dead_code_self_recursive_function() {
    // Bug 4: fn rec() { rec(); } fn main() {} — rec only calls itself, dead
    let (_, warnings) = parse_and_check("fn rec() { rec(); } fn main() { println(1); }");
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Lint(LintId::DeadCode) && w.message.contains("rec")),
        "self-recursive unreachable function should get dead code warning, got: {warnings:?}"
    );
}

// -----------------------------------------------------------------------
// must_use lint
// -----------------------------------------------------------------------

fn count_must_use(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::MustUse))
        .count()
}

/// A local `WriteError`/`SendError` plus a fn returning each as a `Result` error
/// arm — enough to exercise the lint without loading the stdlib (the lint keys
/// on the canonical type name, which matches both the stdlib and these locals).
const MUST_USE_PRELUDE: &str = "enum WriteError { Disconnected(i64); }\n\
     enum SendError { Closed; }\n\
     fn w() -> Result<(), WriteError> { Ok(()) }\n\
     fn s() -> Result<(), SendError> { Ok(()) }\n";

#[test]
fn must_use_flags_discarded_write_result() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() {{ w(); }}");
    let (errors, warnings) = parse_and_check(&src);
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::MustUse))
        .expect("a discarded Result<(), WriteError> must fire must_use");
    assert!(hit.message.contains("WriteError"), "msg: {}", hit.message);
}

#[test]
fn must_use_flags_discarded_send_result() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() {{ s(); }}");
    let (_, warnings) = parse_and_check(&src);
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::MustUse))
        .expect("a discarded Result<(), SendError> must fire must_use");
    assert!(hit.message.contains("SendError"), "msg: {}", hit.message);
}

#[test]
fn must_use_flags_bare_error_value() {
    let src = "enum WriteError { Disconnected(i64); }\n\
         fn make() -> WriteError { WriteError::Disconnected(1) }\n\
         fn caller() { make(); }";
    let (_, warnings) = parse_and_check(src);
    assert_eq!(
        count_must_use(&warnings),
        1,
        "a bare discarded WriteError must fire, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_when_handled_by_question() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() -> Result<(), WriteError> {{ w()?; Ok(()) }}");
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "`?` consumes the Result, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_when_explicitly_bound() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() {{ let _ = w(); }}");
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "`let _ =` is the documented opt-out, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_in_tail_position() {
    // The trailing expression is the block's value (used), not a discard.
    let src = format!("{MUST_USE_PRELUDE}fn caller() -> Result<(), WriteError> {{ w() }}");
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "a tail Result is the function's value, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_when_matched() {
    let src =
        format!("{MUST_USE_PRELUDE}fn caller() {{ match w() {{ Ok(_) => {{}} Err(_) => {{}} }} }}");
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "a matched Result is handled, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_for_ordinary_result() {
    // Only WriteError/SendError are must-use; an unrelated error is left alone.
    let src = "fn g() -> Result<(), i64> { Ok(()) }\nfn caller() { g(); }";
    let (_, warnings) = parse_and_check(src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "a non-must-use error must not fire, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_deny_routes_to_errors() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() {{ w(); }}");
    let out = check_with_lint_level(&src, LintId::MustUse, LintLevel::Deny);
    assert_eq!(count_must_use(&out.errors), 1, "errors: {:?}", out.errors);
    assert_eq!(count_must_use(&out.warnings), 0);
}

#[test]
fn must_use_allow_suppresses() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() {{ w(); }}");
    let out = check_with_lint_level(&src, LintId::MustUse, LintLevel::Allow);
    assert_eq!(count_must_use(&out.warnings), 0);
    assert_eq!(count_must_use(&out.errors), 0);
}

#[test]
fn must_use_suppressed_by_directive() {
    let src = format!("{MUST_USE_PRELUDE}fn caller() {{\n    // hew:allow(must_use)\n    w();\n}}");
    let out = check_with_lint_level(&src, LintId::MustUse, LintLevel::Warn);
    assert_eq!(
        count_must_use(&out.warnings),
        0,
        "a directive above the discard must suppress, warnings: {:?}",
        out.warnings
    );
}

/// A fieldless actor whose `process` reply makes `await d.process(_)` resolve to
/// `Result<i64, AskError>` — the ask-shaped must-use case. `AskError` is a
/// builtin error type, so no prelude enum is needed (unlike WriteError/SendError).
const ASK_ACTOR: &str = "actor Doubler { receive fn process(n: i64) -> i64 { n * 2 } }\n";

#[test]
fn must_use_flags_discarded_await_ask() {
    // A bare `await actor.msg()` in statement position drops a
    // `Result<_, AskError>` — a lost timeout/full-mailbox/stopped-actor signal.
    let src = format!("{ASK_ACTOR}fn main() {{ let d = spawn Doubler; await d.process(5); }}");
    let (errors, warnings) = parse_and_check(&src);
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::MustUse))
        .expect("a discarded `await actor.msg()` (Result<_, AskError>) must fire must_use");
    assert!(
        hit.message.contains("AskError") && hit.message.contains("ask error"),
        "primary message should name AskError and the ask class: {}",
        hit.message
    );
    assert!(
        hit.suggestions.iter().any(|s| {
            s.contains("timeout") && s.contains("mailbox") && s.contains("stopped actor")
        }),
        "suggestion should name the concrete ask failures: {:?}",
        hit.suggestions
    );
}

#[test]
fn must_use_not_flagged_when_await_matched() {
    let src = format!(
        "{ASK_ACTOR}fn main() {{ let d = spawn Doubler; \
         match await d.process(5) {{ Ok(_) => {{}} Err(_) => {{}} }} }}"
    );
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "a matched await ask is handled, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_when_await_explicitly_bound() {
    let src =
        format!("{ASK_ACTOR}fn main() {{ let d = spawn Doubler; let _ = await d.process(5); }}");
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "`let _ = await …` is an explicit discard, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_not_flagged_when_await_in_tail_position() {
    let src = format!(
        "{ASK_ACTOR}fn caller() -> Result<i64, AskError> \
         {{ let d = spawn Doubler; await d.process(5) }}"
    );
    let (_, warnings) = parse_and_check(&src);
    assert_eq!(
        count_must_use(&warnings),
        0,
        "a tail await is the function's value, warnings: {warnings:?}"
    );
}

#[test]
fn must_use_await_suppressed_by_directive() {
    let src = format!(
        "{ASK_ACTOR}fn main() {{\n    let d = spawn Doubler;\n    \
         // hew:allow(must_use)\n    await d.process(5);\n}}"
    );
    let out = check_with_lint_level(&src, LintId::MustUse, LintLevel::Warn);
    assert_eq!(
        count_must_use(&out.warnings),
        0,
        "a directive above the discarded await must suppress, warnings: {:?}",
        out.warnings
    );
}

// -----------------------------------------------------------------------
// sleep_loop_blocks_mailbox lint
// -----------------------------------------------------------------------

fn count_sleep_loop_blocks_mailbox(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::SleepLoopBlocksMailbox))
        .count()
}

const SLEEP_LOOP_REPRO: &str = "actor Worker {\n\
     var running: bool = true;\n\
     receive fn run() { while running { sleep(10ms); } }\n\
     receive fn stop() { running = false; }\n\
     }\n";

#[test]
fn sleep_loop_blocks_mailbox_flags_sibling_stopped_loop() {
    let (errors, warnings) = parse_and_check(SLEEP_LOOP_REPRO);
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    let hit = warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Lint(LintId::SleepLoopBlocksMailbox))
        .expect("a sleep loop stopped only by a sibling message must warn");
    assert_eq!(hit.severity, crate::error::Severity::Warning);
    assert!(
        hit.message.contains("mailbox") && hit.message.contains("cannot be dispatched"),
        "message should explain mailbox starvation: {}",
        hit.message
    );
    assert!(
        hit.suggestions.iter().any(|s| s.contains("#[every")),
        "suggestion should name the periodic receive alternative: {:?}",
        hit.suggestions
    );
}

#[test]
fn sleep_loop_blocks_mailbox_flags_unconditional_loop() {
    let (errors, warnings) = parse_and_check(
        "actor Worker { receive fn run() { let t = instant::now(); loop { sleep_until(t); } } }\n",
    );
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        1,
        "unconditional sleep_until loop should warn: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_ignores_bounded_counter_loop() {
    let (errors, warnings) = parse_and_check(
        "actor Ticker {\n\
         receive fn tick(count: i64) {\n\
             var i: i64 = 0;\n\
             while i < count {\n\
                 sleep(5ms);\n\
                 i = i + 1;\n\
             }\n\
         }\n\
         }\n",
    );
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        0,
        "derived-progress loop conditions are not candidates: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_ignores_non_actor_function() {
    let (errors, warnings) =
        parse_and_check("fn main() { var running: bool = true; while running { sleep(10ms); } }\n");
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        0,
        "sleep loops outside receive handlers must be silent: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_ignores_sleep_loop_inside_lambda() {
    let (errors, warnings) = parse_and_check(
        "actor Worker {\n\
         var running: bool = true;\n\
         receive fn run() { let f = || { while running { sleep(10ms); } }; let _ = f; }\n\
         receive fn stop() { running = false; }\n\
         }\n",
    );
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        0,
        "sleep loops inside closures do not block the enclosing handler's mailbox: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_ignores_loop_with_reachable_break() {
    let (errors, warnings) = parse_and_check(
        "actor Worker {\n\
         var flag: bool = false;\n\
         receive fn run() { while true { sleep(1s); if flag { break; } } }\n\
         }\n",
    );
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        0,
        "a reachable break is an in-handler exit path: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_ignores_bare_sleep_without_loop() {
    let (errors, warnings) =
        parse_and_check("actor Worker { receive fn run() { sleep(100ms); } }\n");
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        0,
        "a one-shot sleep does not starve the mailbox forever: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_ignores_loop_assigning_its_guard() {
    let (errors, warnings) = parse_and_check(
        "actor Worker {\n\
         var running: bool = true;\n\
         receive fn run() { while running { sleep(10ms); running = false; } }\n\
         }\n",
    );
    assert!(errors.is_empty(), "fixture should type-check: {errors:?}");
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&warnings),
        0,
        "an assignment to the guard inside the loop is an in-handler exit path: {warnings:?}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_suppressed_by_directive() {
    const SOURCE: &str = "actor Worker {\n\
         var running: bool = true;\n\
         receive fn run() {\n\
             // hew:allow(sleep_loop_blocks_mailbox)\n\
             while running { sleep(10ms); }\n\
         }\n\
         receive fn stop() { running = false; }\n\
         }\n";
    let out = check_with_lint_level(SOURCE, LintId::SleepLoopBlocksMailbox, LintLevel::Warn);
    assert!(
        out.errors.is_empty(),
        "fixture should type-check: {:?}",
        out.errors
    );
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&out.warnings),
        0,
        "a directive above the loop must suppress the lint: {:?}",
        out.warnings
    );
}

#[test]
fn sleep_loop_blocks_mailbox_deny_routes_to_errors() {
    let out = check_with_lint_level(
        SLEEP_LOOP_REPRO,
        LintId::SleepLoopBlocksMailbox,
        LintLevel::Deny,
    );
    assert_eq!(
        count_sleep_loop_blocks_mailbox(&out.errors),
        1,
        "deny should route the lint into errors: {:?}",
        out.errors
    );
    assert_eq!(count_sleep_loop_blocks_mailbox(&out.warnings), 0);
}

// -----------------------------------------------------------------------
// comment-side Trojan-Source lints
// -----------------------------------------------------------------------

fn count_text_direction_codepoint(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::TextDirectionCodepointInComment))
        .count()
}

fn count_invisible_codepoint(diags: &[TypeError]) -> usize {
    diags
        .iter()
        .filter(|d| d.kind == TypeErrorKind::Lint(LintId::InvisibleCodepointInComment))
        .count()
}

#[test]
fn text_direction_codepoint_flags_line_comment_by_default_as_error() {
    let out = check_with_lint_defaults("// \u{202E}\nfn main() {}\n");
    let hit = out
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::Lint(LintId::TextDirectionCodepointInComment))
        .expect("RLO in a line comment must be denied by default");
    assert_eq!(hit.severity, crate::error::Severity::Error);
    assert_eq!(count_text_direction_codepoint(&out.warnings), 0);
}

#[test]
fn text_direction_codepoint_flags_outer_doc_comment() {
    let out = check_with_lint_defaults("/// \u{202E}\nfn main() {}\n");
    assert_eq!(
        count_text_direction_codepoint(&out.errors),
        1,
        "outer doc comments are source comments for this lint: {:?}",
        out.errors
    );
}

#[test]
fn text_direction_codepoint_flags_inner_doc_comment() {
    let out = check_with_lint_defaults("//! \u{202E}\nfn main() {}\n");
    assert_eq!(
        count_text_direction_codepoint(&out.errors),
        1,
        "inner doc comments are source comments for this lint: {:?}",
        out.errors
    );
}

#[test]
fn text_direction_codepoint_flags_nested_block_comment() {
    let out = check_with_lint_defaults("/* outer /* inner \u{2066} */ done */\nfn main() {}\n");
    assert_eq!(
        count_text_direction_codepoint(&out.errors),
        1,
        "nested block comments must be scanned once: {:?}",
        out.errors
    );
}

#[test]
fn invisible_codepoint_flags_zero_width_space_without_bidi_duplication() {
    let out = check_with_lint_defaults("// hidden\u{200B}gap\nfn main() {}\n");
    assert_eq!(
        count_invisible_codepoint(&out.warnings),
        1,
        "zero-width space should warn: {:?}",
        out.warnings
    );
    assert_eq!(count_invisible_codepoint(&out.errors), 0);
    assert_eq!(count_text_direction_codepoint(&out.errors), 0);
    assert_eq!(count_text_direction_codepoint(&out.warnings), 0);
}

#[test]
fn comment_lints_allow_readable_non_ascii_prose() {
    let out = check_with_lint_defaults(
        "// café — déjà vu, naïve façade\n// 世界 😀 → value\nfn main() {}\n",
    );
    assert_eq!(count_text_direction_codepoint(&out.errors), 0);
    assert_eq!(count_text_direction_codepoint(&out.warnings), 0);
    assert_eq!(count_invisible_codepoint(&out.errors), 0);
    assert_eq!(count_invisible_codepoint(&out.warnings), 0);
}

#[test]
fn comment_lints_do_not_scan_literal_content() {
    // Issue #2109 scopes this lint to comments; literal output escaping is owned
    // by the formatter, so raw literal content is a boundary case here.
    let out = check_with_lint_defaults(
        "fn main() {\n    let _url = \"http://example.com\";\n    let _s = \"\u{202E}\";\n}\n",
    );
    assert_eq!(count_text_direction_codepoint(&out.errors), 0);
    assert_eq!(count_text_direction_codepoint(&out.warnings), 0);
    assert_eq!(count_invisible_codepoint(&out.errors), 0);
    assert_eq!(count_invisible_codepoint(&out.warnings), 0);
}

#[test]
fn source_comment_scan_runs_once_per_module() {
    let out = check_with_lint_defaults(
        "// top \u{202E}\nfn a() {}\nfn b() {}\nfn main() { a(); b(); }\n",
    );
    assert_eq!(
        count_text_direction_codepoint(&out.errors),
        1,
        "a top-of-file comment must not be re-scanned for each item: {:?}",
        out.errors
    );
}

#[test]
fn text_direction_codepoint_suppressed_by_directive_above_comment() {
    let out = check_with_lint_defaults(
        "// hew:allow(text_direction_codepoint_in_comment)\n// \u{202E}\nfn main() {}\n",
    );
    assert_eq!(count_text_direction_codepoint(&out.errors), 0);
    assert_eq!(count_text_direction_codepoint(&out.warnings), 0);
}

#[test]
fn text_direction_codepoint_warn_level_routes_to_warnings() {
    let out = check_with_lint_level(
        "// \u{202E}\nfn main() {}\n",
        LintId::TextDirectionCodepointInComment,
        LintLevel::Warn,
    );
    assert_eq!(count_text_direction_codepoint(&out.errors), 0);
    assert_eq!(
        count_text_direction_codepoint(&out.warnings),
        1,
        "explicit warn should downgrade the deny-default lint: {:?}",
        out.warnings
    );
}

#[test]
fn text_direction_codepoint_allow_level_suppresses() {
    let out = check_with_lint_level(
        "// \u{202E}\nfn main() {}\n",
        LintId::TextDirectionCodepointInComment,
        LintLevel::Allow,
    );
    assert_eq!(count_text_direction_codepoint(&out.errors), 0);
    assert_eq!(count_text_direction_codepoint(&out.warnings), 0);
}

#[test]
fn invisible_codepoint_deny_level_routes_to_errors() {
    let out = check_with_lint_level(
        "// hidden\u{FE0F}\nfn main() {}\n",
        LintId::InvisibleCodepointInComment,
        LintLevel::Deny,
    );
    assert_eq!(
        count_invisible_codepoint(&out.errors),
        1,
        "explicit deny should promote the invisible-codepoint lint: {:?}",
        out.errors
    );
    assert_eq!(count_invisible_codepoint(&out.warnings), 0);
}

#[test]
fn invisible_codepoint_allow_level_suppresses() {
    let out = check_with_lint_level(
        "// hidden\u{FE0F}\nfn main() {}\n",
        LintId::InvisibleCodepointInComment,
        LintLevel::Allow,
    );
    assert_eq!(count_invisible_codepoint(&out.errors), 0);
    assert_eq!(count_invisible_codepoint(&out.warnings), 0);
}

// -----------------------------------------------------------------------
// Module namespacing tests
// -----------------------------------------------------------------------

// -- should_import_name --
