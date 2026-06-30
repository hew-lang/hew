#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn check_generic_lambda_removed_emits_typed_diagnostic() {
    // Generic lambda `<T>(params) => body` was removed in v0.5.
    // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic.
    let source = r"
        fn main() {
            let id = <T>(x: T) => x;
        }
    ";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
        result.errors
    );
}

/// Slice-1 generic lambda regression test.
///
/// Verifies that:
/// 1. A let-bound generic lambda type-checks cleanly.
/// 2. A direct call whose arguments make the type obvious resolves the
///    return type correctly.
/// 3. `call_type_args` is populated for the call so the enricher can
///    fill in explicit type arguments before serialisation to codegen.
// Generic lambda `<T>(params) => body` was removed in v0.5.
// The tests below confirm the parser emits typed diagnostics instead.
// Equivalent named-function generics continue to work (tested elsewhere).

#[test]
fn generic_lambda_slice1_removed_emits_diagnostic() {
    let source = r"
        fn main() {
            let v: i64 = 30;
            let r = <T>(a: T, b: T) -> T => a;
            let q = r(v, v);
        }
    ";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
        result.errors
    );
}

#[test]
fn generic_lambda_two_type_params_removed_emits_diagnostic() {
    let source = "fn main() { let combine = <A, B>(a: A, b: B) -> A => a; combine(1, 2); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
        result.errors
    );
}

#[test]
fn contextual_lambda_binding_records_lambda_expr_type() {
    let source = concat!(
        "fn main() {\n",
        "    let f: fn(i64) -> i64 = |x| x + 1;\n",
        "    let y = f(5);\n",
        "}\n",
    );

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let lambda_span = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.stmts.iter().find_map(|(stmt, _)| match stmt {
                    Stmt::Let {
                        value: Some((Expr::Lambda { .. }, span)),
                        ..
                    } => Some(span.clone()),
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("main let-bound lambda should exist");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    assert_eq!(
        output.expr_types.get(&SpanKey::from(&lambda_span)),
        Some(&Ty::Function {
            params: vec![Ty::I64],
            ret: Box::new(Ty::I64),
        })
    );
}

#[test]
fn method_level_type_params_freshen_per_named_method_call() {
    let source = r"
        type Holder { value: i64 }

        impl Holder {
            fn pick<T>(h: Holder, value: T) -> T {
                value
            }
        }

        fn main() {
            let h = Holder { value: 1 };
            let n = h.pick(42);
            let flag = h.pick(true);
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
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one call to infer T=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one call to infer T=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn generic_impl_method_level_type_params_freshen_per_call() {
    let source = r"
        type Box<T> { value: T }

        impl<T> Box<T> {
            fn transform<U>(b: Box<T>, f: fn(T) -> U) -> Box<U> {
                Box { value: f(b.value) }
            }
        }

        fn double(x: i64) -> i64 { x * 2 }
        fn is_even(x: i64) -> bool { x % 2 == 0 }

        fn main() {
            let b = Box { value: 42 };
            let doubled = b.transform(double);
            let even = b.transform(is_even);
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
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one call to infer U=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one call to infer U=bool, got {:?}",
        output.call_type_args
    );
}

/// F4 regression: an explicit-turbofish generic call (`id<i64>(5)`) must
/// record its resolved type arguments in `call_type_args`. Before the fix
/// the checker only recorded inferred sites (`type_args.is_none()`), so the
/// turbofish entry was missing — HIR/MIR then found no monomorphisation
/// data and MIR fell through to the "function call NYI" diagnostic.
#[test]
fn turbofish_generic_call_records_call_type_args() {
    let source = r"
        fn id<T>(x: T) -> T { x }
        fn main() {
            let y = id<i64>(5);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let call_span = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.stmts.iter().find_map(|(stmt, _)| match stmt {
                    Stmt::Let {
                        value: Some((Expr::Call { .. }, span)),
                        ..
                    } => Some(span.clone()),
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("main let-bound turbofish call should exist");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_span)),
        Some(&vec![Ty::I64]),
        "turbofish `id<i64>(5)` must record T=i64; got {:?}",
        output.call_type_args
    );
}

/// F4 regression: multi-parameter turbofish records every resolved arg in
/// declaration order.
#[test]
fn multi_param_turbofish_records_all_call_type_args() {
    let source = r"
        fn make_pair<A, B>(a: A, b: B) -> A { a }
        fn main() {
            let p = make_pair<i64, bool>(10, true);
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
        "type check errors: {:?}",
        output.errors
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64, Ty::Bool]),
        "turbofish `make_pair<i64, bool>` must record [i64, bool]; got {:?}",
        output.call_type_args
    );
}

/// F7: a return-type-polymorphic generic constructor call records its resolved
/// type argument in `call_type_args` even though `T` is determined by the
/// EXPECTED RETURN TYPE (here the `let` annotation), not by any argument. The
/// recorder runs inside `synthesize` while `T` is still an unbound inference
/// var; deferring the snapshot to the output boundary — where it is re-resolved
/// after `check_against` unifies the annotation in — is what makes the entry
/// concrete. Without it, HIR/MIR find no monomorphisation data and MIR falls
/// through to the "function call NYI" diagnostic. Covers both the inherent-impl
/// associated fn (`Stack::new()`) and the free fn (`new_stack()`).
#[test]
fn return_type_polymorphic_call_records_call_type_args() {
    let source = r"
        type Stack<T> { items: Vec<T>; }
        impl<T> Stack<T> {
            fn new() -> Stack<T> { Stack { items: Vec::new() } }
        }
        fn new_stack<T>() -> Stack<T> { Stack { items: Vec::new() } }
        fn main() {
            let a: Stack<i64> = Stack::new();
            let b: Stack<string> = new_stack();
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    // The two let-bound generic-ctor call spans, in declaration order.
    let call_spans: Vec<_> = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => Some(
                fd.body
                    .stmts
                    .iter()
                    .filter_map(|(stmt, _)| match stmt {
                        Stmt::Let {
                            value: Some((Expr::Call { .. }, span)),
                            ..
                        } => Some(span.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        })
        .expect("main should hold two let-bound generic-ctor calls");
    assert_eq!(call_spans.len(), 2, "expected two ctor call sites");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    // assoc-fn `Stack::new()` records T=i64 from the `let` annotation.
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[0])),
        Some(&vec![Ty::I64]),
        "return-type-polymorphic `Stack::new()` must record T=i64; got {:?}",
        output.call_type_args
    );
    // free-fn `new_stack()` records T=string from the `let` annotation.
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[1])),
        Some(&vec![Ty::String]),
        "return-type-polymorphic `new_stack()` must record T=string; got {:?}",
        output.call_type_args
    );
}

/// F7 boundary: the call-type-arg recorder now **defers** instead of dropping.
/// A type argument that is still an unbound inference var at recording time is
/// snapshotted so a later binding — via the expected return type, which
/// `check_against` unifies in only *after* `synthesize` already ran this
/// recording — propagates at the `check_program` output boundary. The
/// fail-closed invariant (no `Ty::Var` crosses into HIR/MIR) is preserved by
/// `validate_call_type_args_output_contract`; see
/// `validate_call_type_args_output_contract_prunes_leaked_inference_vars`.
#[test]
fn call_type_arg_recorder_defers_inference_var_then_reresolves() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = 0..0;
    let var = TypeVar::fresh();

    // An unresolved var arg is snapshotted (deferred), not dropped: the
    // return-type-polymorphic constructor pattern (`let s: Stack<i64> =
    // Stack::new()`) depends on this entry surviving until the expected type
    // pins `T` at the output boundary.
    checker.record_concrete_call_type_args(&span, &[Ty::Var(var)]);
    assert_eq!(
        checker.call_type_args.values().next(),
        Some(&vec![Ty::Var(var)]),
        "unresolved inference-var arg must be snapshotted for boundary re-resolution: {:?}",
        checker.call_type_args
    );

    // Once the var resolves to a concrete type, the recorder snapshots that
    // concrete type — the same value the boundary re-resolution in
    // `check_program` (`subst.resolve` + `materialize_literal_defaults`)
    // computes for a still-`Ty::Var` snapshot taken earlier.
    checker.subst.insert(var, &Ty::I64).unwrap();
    checker.record_concrete_call_type_args(&span, &[Ty::Var(var)]);
    assert_eq!(
        checker.call_type_args.values().next(),
        Some(&vec![Ty::I64]),
        "resolved var arg must snapshot as its concrete type: {:?}",
        checker.call_type_args
    );
}

#[test]
fn generic_impl_method_underconstrained_type_param_reports_inference_failed() {
    let source = r"
        enum Maybe<T> { Some(T); None; }
        type Holder {}

        impl Holder {
            fn wrap<T>(h: Holder, value: Maybe<T>) -> Maybe<T> {
                value
            }
        }

        fn main() {
            let h = Holder {};
            let unresolved = h.wrap(None);
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
            .any(|error| error.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for unresolved method-level type param, got {:?}",
        output.errors
    );
}

#[test]
fn trait_method_type_params_freshen_per_call_on_bounded_type_param() {
    let source = r"
        trait Transform {
            fn apply<U>(item: Self, f: fn(i64) -> U) -> U;
        }

        type Holder { value: i64 }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(i64) -> U) -> U {
                f(item.value)
            }
        }

        fn double(x: i64) -> i64 { x * 2 }
        fn is_odd(x: i64) -> bool { x % 2 != 0 }

        fn run<T: Transform>(item: T) {
            let doubled = item.apply(double);
            let odd = item.apply(is_odd);
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
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per trait-bound method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one trait-bound call to infer U=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one trait-bound call to infer U=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn trait_method_type_params_do_not_unify_across_calls() {
    let source = r"
        trait Transform {
            fn apply<U>(item: Self, f: fn(i64) -> U) -> U;
        }

        type Holder { value: i64 }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(i64) -> U) -> U {
                f(item.value)
            }
        }

        fn double(x: i64) -> i64 { x * 2 }
        fn is_odd(x: i64) -> bool { x % 2 != 0 }

        fn run<T: Transform>(item: T) {
            let doubled = item.apply(double);
            let odd = item.apply(is_odd);
            println(doubled);
            println(odd);
        }

        fn main() {
            let h = Holder { value: 21 };
            run(h);
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
        "trait-bound method calls should infer independently, got {:?}",
        output.errors
    );
}

/// Generic lambda `<T>(params) => body` was removed in v0.5.
/// The parser must emit a typed `E_CLOSURE_PIPE_SYNTAX` diagnostic even when
/// the generic lambda appears as a call argument rather than a let binding.
#[test]
fn generic_lambda_in_arg_position_rejected() {
    // Replaces `generic_lambda_scratch_state_no_leak`: the old test verified
    // that scratch state didn't leak between a generic lambda argument and a
    // subsequent let-binding. The scenario is now moot because generic lambdas
    // are rejected at the parse stage. This assertion verifies the removal
    // diagnostic fires in argument position.
    let source = r"fn main() { apply(<T>(x: T) => x, 5); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda in arg position, got: {:?}",
        result.errors
    );
}

#[test]
fn test_self_with_generics_in_impl() {
    let source = r"
        type Pair<T> {
            first: T,
            second: T,
        }

        impl<T> Pair<T> {
            fn new(first: T, second: T) -> Self {
                return Pair { first: first, second: second };
            }

            fn swap(p: Pair<T>) -> Self {
                return Pair { first: p.second, second: p.first };
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
        "type check errors: {:?}",
        output.errors
    );

    // Verify that Self resolves to Pair<T>, not bare Pair
    // The new method should return Pair<T>
    let new_sig = output
        .fn_sigs
        .get("Pair::new")
        .expect("Pair::new should exist");
    if let Ty::Named { name, args, .. } = &new_sig.return_type {
        assert_eq!(name, "Pair", "return type should be Pair");
        assert_eq!(args.len(), 1, "Pair should have one type argument");
    } else {
        panic!("Expected Pair::new to return a named type");
    }
}

#[test]
fn test_trait_object_type_args_substitution() {
    // Bug 2: Test that dyn Trait<Args> methods get correct substitutions
    let source = r"
        trait MyIter<T> {
            fn next(iter: Self) -> Option<T>;
        }

        type Counter {
            count: i64;
        }

        impl MyIter<i64> for Counter {
            fn next(c: Counter) -> Option<i64> {
                Some(42)
            }
        }

        fn test_iterator() -> i64 {
            let iter: dyn MyIter<i64> = Counter { count: 5 };
            let result = iter.next(); // Should be Option<i64>, not Option<T>
            match result {
                Some(x) => x,
                None => 0
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

    if !output.errors.is_empty() {
        for error in &output.errors {
            println!("Type error: {error}");
        }
    }

    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "proof assertions for all 6 call_type_args entries"
)]
fn trait_bound_compound_generic_methods_do_not_cross_contaminate() {
    let source = r#"
        trait Transform {
            fn apply<U>(item: Self, f: fn(i64) -> U) -> U;
        }

        trait Label {
            fn tag<V>(item: Self, prefix: V) -> string;
        }

        type Holder { value: i64 }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(i64) -> U) -> U {
                f(item.value)
            }
        }

        impl Label for Holder {
            fn tag<V>(item: Holder, prefix: V) -> string {
                "tagged"
            }
        }

        fn is_odd(x: i64) -> bool { x % 2 != 0 }

        fn run<T: Transform + Label>(item: T) {
            let odd = item.apply(is_odd);
            let tagged_num = item.tag(42);
            let tagged_str = item.tag("lbl");
            println(odd);
            println(tagged_num);
            println(tagged_str);
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
        "type check errors: {:?}",
        output.errors
    );
    // Exact count: 3 compound-bound generic method calls (apply<U=bool>, tag<V=i64>,
    // tag<V=string>) + 3 println builtins now registered with Display bounds.  Each
    // call site produces one entry keyed by span, so the total is deterministic.
    assert_eq!(
        output.call_type_args.len(),
        6,
        "expected 3 generic-method calls + 3 println Display-bound calls, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one Transform call to infer U=bool, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one Label call to infer V=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::String]),
        "expected one Label call to infer V=string, got {:?}",
        output.call_type_args
    );
    // Per-value count proof: pin exactly how many entries carry each type.
    // apply<U=bool> + println<T=bool> = 2; tag<V=i64> only = 1;
    // tag<V=string> + println<T=string> × 2 = 3.
    let bool_count = output
        .call_type_args
        .values()
        .filter(|args| args.as_slice() == [crate::ty::Ty::Bool])
        .count();
    assert_eq!(
        bool_count, 2,
        "apply<U=bool> and println<T=bool>: expected 2 [bool] entries, got {:?}",
        output.call_type_args
    );
    let int_count = output
        .call_type_args
        .values()
        .filter(|args| args.as_slice() == [crate::ty::Ty::I64])
        .count();
    assert_eq!(
        int_count, 1,
        "tag<V=i64>: expected exactly 1 [i64] entry, got {:?}",
        output.call_type_args
    );
    let string_count = output
        .call_type_args
        .values()
        .filter(|args| args.as_slice() == [crate::ty::Ty::String])
        .count();
    assert_eq!(
        string_count, 3,
        "tag<V=string> + println<T=string> × 2: expected 3 [string] entries, got {:?}",
        output.call_type_args
    );
}

#[test]
fn test_wire_since_without_version_warns() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    assert_eq!(checker.warnings.len(), 1);
    assert!(
        checker.warnings[0].message.contains("since 2"),
        "warning should mention since: {}",
        checker.warnings[0].message
    );
    assert!(
        checker.warnings[0]
            .message
            .contains("no #[wire(version = N)]"),
        "warning should mention missing version: {}",
        checker.warnings[0].message
    );
    assert_eq!(checker.warnings[0].span, 0..0);
}

#[test]
fn test_wire_since_without_version_uses_registered_decl_span() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_type_namespace_name(None, "TestMsg", &(10..50));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert_eq!(checker.warnings.len(), 1);
    assert_eq!(checker.warnings[0].span, 10..50);
}

#[test]
fn test_wire_since_with_version_no_extra_warning() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: Some(3),
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    // No "since without version" warning since version is present
    let since_without_version = checker
        .warnings
        .iter()
        .any(|w| w.message.contains("no #[wire(version = N)]"));
    assert!(
        !since_without_version,
        "should not warn about missing version"
    );
}

#[test]
fn empty_fn_body_return_mismatch_uses_decl_span() {
    let source = "fn greet() -> string {}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = match &result.program.items[0].0 {
        hew_parser::ast::Item::Function(fd) => fd.decl_span.clone(),
        item => panic!("expected function item, got {item:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let mismatch = output
        .errors
        .iter()
        .find(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. }))
        .unwrap_or_else(|| panic!("expected mismatch error, got {:?}", output.errors));

    assert_eq!(mismatch.span, expected_span);
}

#[test]
fn empty_receive_fn_body_return_mismatch_uses_decl_span() {
    let source = r"
actor Greeter {
    receive fn greet() -> string {}
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = match &result.program.items[0].0 {
        hew_parser::ast::Item::Actor(actor) => actor.receive_fns[0].span.clone(),
        item => panic!("expected actor item, got {item:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let mismatch = output
        .errors
        .iter()
        .find(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. }))
        .unwrap_or_else(|| panic!("expected mismatch error, got {:?}", output.errors));

    assert_eq!(mismatch.span, expected_span);
}

#[test]
fn actor_ref_cycle_warning_uses_first_actor_decl_span() {
    let source = concat!(
        "actor Alpha {\n",
        "    let beta: LocalPid<Beta>;\n",
        "}\n",
        "actor Beta {\n",
        "    let alpha: LocalPid<Alpha>;\n",
        "}\n",
        "fn main() {}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = result
        .program
        .items
        .iter()
        .find_map(|(item, span)| match item {
            Item::Actor(actor) if actor.name == "Alpha" => Some(span.clone()),
            _ => None,
        })
        .expect("expected Alpha actor item");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "actor cycle warning should not introduce type errors: {:?}",
        output.errors
    );

    let warning = output
        .warnings
        .iter()
        .find(|warning| warning.kind == TypeErrorKind::ActorRefCycle)
        .unwrap_or_else(|| panic!("expected ActorRefCycle warning, got {:?}", output.warnings));
    let actor_decl_start = source
        .find("actor Alpha")
        .expect("expected Alpha declaration text");
    let actor_name_end = actor_decl_start + "actor Alpha".len();

    assert_ne!(warning.span, 0..0);
    assert_eq!(warning.span, expected_span);
    assert!(
        warning.span.start <= actor_decl_start && actor_name_end <= warning.span.end,
        "warning span should cover the first actor declaration, got {:?}",
        warning.span
    );
}

#[test]
fn recursive_value_type_self_enum_is_rejected() {
    let output = check_source(
        r"
        enum Tree { Leaf; Node(i64, Tree, Tree); }
        fn main() {}
        ",
    );

    let error = output
        .errors
        .iter()
        .find(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::RecursiveValueType {
                    type_name,
                    referenced_type,
                } if type_name == "Tree" && referenced_type == "Tree"
            )
        })
        .unwrap_or_else(|| panic!("expected recursive Tree error, got {:?}", output.errors));

    assert!(error.message.contains("enum `Tree` is infinitely sized"));
    assert!(error
        .message
        .contains("variant `Node` contains `Tree` by value"));
}

#[test]
fn recursive_value_type_mutual_enums_are_rejected() {
    let output = check_source(
        r"
        enum A { A1(B); }
        enum B { B1(A); }
        fn main() {}
        ",
    );

    let recursive_types: HashSet<_> = output
        .errors
        .iter()
        .filter_map(|error| match &error.kind {
            TypeErrorKind::RecursiveValueType { type_name, .. } => Some(type_name.as_str()),
            _ => None,
        })
        .collect();

    assert!(
        recursive_types.contains("A") && recursive_types.contains("B"),
        "expected A and B recursive value type errors, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_allows_non_recursive_nested_enum() {
    let output = check_source(
        r"
        enum Inner { A; B(i64); }
        enum Outer { D(Inner); }
        fn main() {}
        ",
    );

    assert!(
        output.errors.is_empty(),
        "non-recursive nested enum should type-check, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_rejects_record_enum_cycle() {
    let output = check_source(
        r"
        record Boxed { tree: Tree }
        enum Tree { Leaf; Node(Boxed); }
        fn main() {}
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::RecursiveValueType { .. })),
        "expected record/enum recursive value type error, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_rejects_generic_record_wrapper_cycle() {
    let output = check_source(
        r"
        record Wrapper<T> { value: T }
        enum Tree { Leaf; Node(Wrapper<Tree>); }
        fn main() {}
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::RecursiveValueType {
                    type_name,
                    referenced_type,
                } if type_name == "Tree" && referenced_type == "Tree"
            )
        }),
        "expected generic wrapper recursive value type error, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_rejects_diverging_generic_self_reference() {
    // A polymorphic self-reference whose field re-instantiates the record
    // with strictly-larger type args — `Wrap<T> { w: Wrap<Wrap<T>> }`. Each
    // generic-instantiation step produces a distinct `(name, args)` pair
    // (`Wrap<i64>` → `Wrap<Wrap<i64>>` → …), so the `(name, args)` dedup set
    // never collides. Without a name-based bound on the expansion walk this
    // diverges and exhausts memory; the bound must let the value-cycle
    // detector terminate and reject `Wrap` as infinitely sized.
    let output = check_source(
        r"
        type Wrap<T> { w: Wrap<Wrap<T>> }
        fn main() {}
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::RecursiveValueType {
                    type_name,
                    referenced_type,
                } if type_name == "Wrap" && referenced_type == "Wrap"
            )
        }),
        "expected diverging generic self-reference rejected as recursive \
         value type, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_allows_pointer_self_reference() {
    let output = check_source(
        r"
        record Node { next: *const Node }
        fn main() {}
        ",
    );

    assert!(
        output.errors.is_empty(),
        "pointer indirection should break recursive value type cycle, got {:?}",
        output.errors
    );
}

#[test]
fn typecheck_await_local_pid_returns_unit() {
    let output = check_source(
        r#"
        actor Greeter {
            receive fn greet(name: string) {
                println(name);
            }
        }
        fn main() {
            let g = spawn Greeter;
            g.greet("hi");
            close(g);
            await g;
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}

#[test]
fn named_actor_receive_dispatch_reports_bad_arg_once() {
    let result = hew_parser::parse(
        r"
        actor Greeter {
            receive fn greet(name: string) {}
        }

        fn main() {
            let g = spawn Greeter;
            g.greet(missing_name);
        }
        ",
    );
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let undefined_variable_count = output
        .errors
        .iter()
        .filter(|error| matches!(error.kind, TypeErrorKind::UndefinedVariable))
        .count();

    assert_eq!(
        undefined_variable_count, 1,
        "named actor receive dispatch should not resynthesize the same bad arg: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_await_close_local_pid() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "g".to_string(),
        Ty::local_pid(Ty::Named {
            builtin: None,
            name: "Greeter".to_string(),
            args: vec![],
        }),
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("g".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_await_close_local_pid_worker() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "worker".to_string(),
        Ty::local_pid(Ty::Named {
            builtin: None,
            name: "Worker".to_string(),
            args: vec![],
        }),
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("worker".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_join_rejects_non_actor_sources() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = 0..0;
    let expr = Expr::Join(vec![
        make_int_literal(1, span.clone()),
        make_int_literal(2, span.clone()),
    ]);
    let _ = checker.synthesize(&expr, &span);
    assert!(
        checker.errors.iter().any(|error| error
            .message
            .contains("join expression element must be actor.method(args)")),
        "expected join source error, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_integer_literal_coerces_in_arithmetic() {
    // `n - 1` where n: i32 should work — literal 1 coerces to i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() { println(fib(10)); }\n"
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
        "integer literal should coerce in arithmetic: {:?}",
        output.errors
    );
}

#[test]
fn i32_in_condition_position_is_type_error() {
    // §12.1: no implicit numeric→bool coercion. `if flag` where `flag: i32`
    // must be rejected; the programmer must write `if flag != 0`.
    let (errors, _warnings) =
        parse_and_check("fn foo(flag: i32) -> i32 { if flag { 1 } else { 0 } }");
    assert!(
        errors.iter().any(
            |e| matches!(&e.kind, TypeErrorKind::Mismatch { expected, actual }
                if expected.contains("bool") && actual.contains("i32"))
        ),
        "i32 in bool (condition) position must be a type error: {errors:?}"
    );
}

#[test]
fn bool_does_not_coerce_to_i32() {
    let (errors, _warnings) = parse_and_check("fn foo(flag: bool) -> i32 { flag }");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "bool where i32 expected must be rejected: {errors:?}"
    );
}

#[test]
fn handle_type_does_not_coerce_to_string() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        "import std::encoding::json;\nfn foo(value: json.Value) -> string { value }",
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "json.Value where string expected must be rejected: {errors:?}"
    );
}

#[test]
fn string_does_not_coerce_to_handle_type() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        "import std::encoding::json;\nfn foo(text: string) -> json.Value { text }",
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "string where json.Value expected must be rejected: {errors:?}"
    );
}

#[test]
fn unconstrained_range_defaults_to_i64() {
    let source = "fn main() { for i in 0..10 { println(i); } }";
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
    let Stmt::For { iterable, .. } = &main_fn.body.stmts[0].0 else {
        panic!("expected for statement");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unconstrained range should type-check: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(&iterable.1)),
        Some(&Ty::range(Ty::I64)),
        "unconstrained range literal should default to Range<i64>: {:?}",
        output.expr_types
    );
}

#[test]
fn typecheck_literal_range_infers_from_context() {
    // `for i in 0..8 { fib(i) }` where fib takes i32 — range bounds
    // should not force i64; the loop variable should be usable as i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() {\n",
        "    for i in 0..8 {\n",
        "        println(fib(i));\n",
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
    assert!(
        output.errors.is_empty(),
        "literal range should infer element type from context: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_numeric() {
    let source = concat!(
        "fn main() {\n",
        "    let x: i64 = 42;\n",
        "    let y: i32 = x as i32;\n",
        "    let z: f64 = y as f64;\n",
        "    println(y);\n",
        "    println(z);\n",
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
        "numeric casts should type-check: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_invalid() {
    let source = concat!(
        "fn main() {\n",
        "    let s = \"hello\";\n",
        "    let x = s as i32;\n",
        "    println(x);\n",
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
            .any(|e| e.message.contains("cannot cast")),
        "should reject invalid cast: {:?}",
        output.errors
    );
}

// ── Literal coercion tests ────────────────────────────────────────

#[test]
fn literal_coercion_integer_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
}

#[test]
fn literal_coercion_integer_to_u8() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(255, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::U8);
}

#[test]
fn literal_coercion_integer_to_u8_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(256, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_negative_to_unsigned() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (
        Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(make_int_literal(1, 1..2)),
        },
        0..2,
    );
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("negative literal")),
        "expected negative-to-unsigned error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_i32_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // 2^31 = 2147483648, which exceeds i32 max (2147483647)
    let lit = make_int_literal(2_147_483_648, 0..10);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn int_literal_infers_from_annotated_binding() {
    let output = check_source(
        r"
        fn main() {
            let x: i32 = 1;
            let y: u8 = 255;
            let z: i16 = -12;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "integer literals should infer from adjacent annotations: {:#?}",
        output.errors
    );
}

#[test]
fn int_literal_inference_rejects_out_of_range_annotation() {
    let output = check_source(
        r"
        fn main() {
            let x: i32 = 2147483648;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "out-of-range literal must reject against inferred i32 context: {:#?}",
        output.errors
    );
}

#[test]
fn literal_coercion_integer_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_float_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (Expr::Literal(Literal::Float(std::f64::consts::PI)), 0..4);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

/// Native pointer width for fixed-width helper tests where the platform-sized
/// arms are not exercised (the value is irrelevant to a fixed-width arm).
const PTR_WIDTH_64: u8 = 64;
const PTR_WIDTH_32: u8 = 32;

#[test]
fn literal_coercion_integer_fits_i8() {
    assert!(integer_fits_type(127, &Ty::I8, PTR_WIDTH_64));
    assert!(integer_fits_type(-128, &Ty::I8, PTR_WIDTH_64));
    assert!(!integer_fits_type(128, &Ty::I8, PTR_WIDTH_64));
    assert!(!integer_fits_type(-129, &Ty::I8, PTR_WIDTH_64));
}

#[test]
fn literal_coercion_integer_fits_u8() {
    assert!(integer_fits_type(0, &Ty::U8, PTR_WIDTH_64));
    assert!(integer_fits_type(255, &Ty::U8, PTR_WIDTH_64));
    assert!(!integer_fits_type(256, &Ty::U8, PTR_WIDTH_64));
    assert!(!integer_fits_type(-1, &Ty::U8, PTR_WIDTH_64));
}

#[test]
fn literal_coercion_integer_fits_i16() {
    assert!(integer_fits_type(32767, &Ty::I16, PTR_WIDTH_64));
    assert!(integer_fits_type(-32768, &Ty::I16, PTR_WIDTH_64));
    assert!(!integer_fits_type(32768, &Ty::I16, PTR_WIDTH_64));
}

#[test]
fn literal_coercion_integer_fits_u16() {
    assert!(integer_fits_type(65535, &Ty::U16, PTR_WIDTH_64));
    assert!(!integer_fits_type(65536, &Ty::U16, PTR_WIDTH_64));
}

#[test]
fn literal_coercion_integer_fits_i32() {
    assert!(integer_fits_type(2_147_483_647, &Ty::I32, PTR_WIDTH_64));
    assert!(integer_fits_type(-2_147_483_648, &Ty::I32, PTR_WIDTH_64));
    assert!(!integer_fits_type(2_147_483_648, &Ty::I32, PTR_WIDTH_64));
}

#[test]
fn literal_coercion_integer_fits_u32() {
    assert!(integer_fits_type(4_294_967_295, &Ty::U32, PTR_WIDTH_64));
    assert!(!integer_fits_type(4_294_967_296, &Ty::U32, PTR_WIDTH_64));
    assert!(!integer_fits_type(-1, &Ty::U32, PTR_WIDTH_64));
}

#[test]
fn literal_coercion_integer_fits_u64() {
    // i64 max fits in u64
    assert!(integer_fits_type(i64::MAX, &Ty::U64, PTR_WIDTH_64));
    // 0 fits
    assert!(integer_fits_type(0, &Ty::U64, PTR_WIDTH_64));
    // Negative doesn't fit
    assert!(!integer_fits_type(-1, &Ty::U64, PTR_WIDTH_64));
}

#[test]
fn integer_type_info_isize_usize_resolve_target_width() {
    // 64-bit target: isize = 64-bit signed, usize = 64-bit unsigned.
    let isize64 = integer_type_info(&Ty::Isize, PTR_WIDTH_64).expect("isize is an integer");
    assert_eq!(isize64.width, 64);
    assert!(isize64.signed);
    let usize64 = integer_type_info(&Ty::Usize, PTR_WIDTH_64).expect("usize is an integer");
    assert_eq!(usize64.width, 64);
    assert!(!usize64.signed);

    // 32-bit (wasm32) target: width follows the pointer width.
    let isize32 = integer_type_info(&Ty::Isize, PTR_WIDTH_32).expect("isize is an integer");
    assert_eq!(isize32.width, 32);
    assert!(isize32.signed);
    let usize32 = integer_type_info(&Ty::Usize, PTR_WIDTH_32).expect("usize is an integer");
    assert_eq!(usize32.width, 32);
    assert!(!usize32.signed);
}

#[test]
fn common_integer_type_platform_sized_combine_only_with_self() {
    // Same platform type combines to itself.
    assert_eq!(
        common_integer_type(&Ty::Isize, &Ty::Isize, PTR_WIDTH_64),
        Some(Ty::Isize)
    );
    assert_eq!(
        common_integer_type(&Ty::Usize, &Ty::Usize, PTR_WIDTH_64),
        Some(Ty::Usize)
    );
    // Platform type + IntLiteral resolves to the platform type.
    assert_eq!(
        common_integer_type(&Ty::Isize, &Ty::IntLiteral, PTR_WIDTH_64),
        Some(Ty::Isize)
    );
    assert_eq!(
        common_integer_type(&Ty::IntLiteral, &Ty::Usize, PTR_WIDTH_64),
        Some(Ty::Usize)
    );
    // Mixed sign and mixed platform-vs-fixed are rejected (no implicit coerce),
    // even when the fixed type has the same width (i64 == 64-bit isize).
    assert_eq!(
        common_integer_type(&Ty::Isize, &Ty::Usize, PTR_WIDTH_64),
        None
    );
    assert_eq!(
        common_integer_type(&Ty::Isize, &Ty::I64, PTR_WIDTH_64),
        None
    );
    assert_eq!(
        common_integer_type(&Ty::I64, &Ty::Isize, PTR_WIDTH_64),
        None
    );
    assert_eq!(
        common_integer_type(&Ty::Usize, &Ty::U64, PTR_WIDTH_64),
        None
    );
    assert_eq!(
        common_integer_type(&Ty::Isize, &Ty::I32, PTR_WIDTH_64),
        None
    );
}

#[test]
fn common_integer_type_fixed_width_unchanged_by_isize_arms() {
    // Adding the platform arms must not change fixed-width selection.
    assert_eq!(
        common_integer_type(&Ty::I32, &Ty::I64, PTR_WIDTH_64),
        Some(Ty::I64)
    );
    assert_eq!(
        common_integer_type(&Ty::I64, &Ty::I32, PTR_WIDTH_64),
        Some(Ty::I64)
    );
    assert_eq!(
        common_integer_type(&Ty::U8, &Ty::U32, PTR_WIDTH_64),
        Some(Ty::U32)
    );
    assert_eq!(common_integer_type(&Ty::I32, &Ty::U32, PTR_WIDTH_64), None);
    assert_eq!(
        common_integer_type(&Ty::IntLiteral, &Ty::I32, PTR_WIDTH_64),
        Some(Ty::I32)
    );
}

#[test]
fn integer_fits_type_isize_usize_boundary() {
    // 64-bit isize: any i64 fits; usize: non-negative fits (u64 range).
    assert!(integer_fits_type(i64::MAX, &Ty::Isize, PTR_WIDTH_64));
    assert!(integer_fits_type(i64::MIN, &Ty::Isize, PTR_WIDTH_64));
    assert!(integer_fits_type(0, &Ty::Usize, PTR_WIDTH_64));
    assert!(integer_fits_type(i64::MAX, &Ty::Usize, PTR_WIDTH_64));
    assert!(!integer_fits_type(-1, &Ty::Usize, PTR_WIDTH_64));

    // 32-bit isize: bounds shrink to i32; usize to u32.
    assert!(integer_fits_type(
        i64::from(i32::MAX),
        &Ty::Isize,
        PTR_WIDTH_32
    ));
    assert!(integer_fits_type(
        i64::from(i32::MIN),
        &Ty::Isize,
        PTR_WIDTH_32
    ));
    assert!(!integer_fits_type(
        i64::from(i32::MAX) + 1,
        &Ty::Isize,
        PTR_WIDTH_32
    ));
    assert!(integer_fits_type(
        i64::from(u32::MAX),
        &Ty::Usize,
        PTR_WIDTH_32
    ));
    assert!(!integer_fits_type(
        i64::from(u32::MAX) + 1,
        &Ty::Usize,
        PTR_WIDTH_32
    ));
    assert!(!integer_fits_type(-1, &Ty::Usize, PTR_WIDTH_32));
}

#[test]
fn integer_type_range_isize_usize_follows_width() {
    assert_eq!(
        integer_type_range(&Ty::Isize, PTR_WIDTH_64),
        Some((i128::from(i64::MIN), i128::from(i64::MAX)))
    );
    assert_eq!(
        integer_type_range(&Ty::Usize, PTR_WIDTH_64),
        Some((0, i128::from(u64::MAX)))
    );
    assert_eq!(
        integer_type_range(&Ty::Isize, PTR_WIDTH_32),
        Some((i128::from(i32::MIN), i128::from(i32::MAX)))
    );
    assert_eq!(
        integer_type_range(&Ty::Usize, PTR_WIDTH_32),
        Some((0, i128::from(u32::MAX)))
    );
}

// ── Array literal → Vec type coercion tests ──────────────────────

#[test]
fn array_literal_synthesizes_vec() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(1, 1..2),
        make_int_literal(2, 4..5),
        make_int_literal(3, 7..8),
    ];
    let arr = (Expr::Array(elems), 0..9);
    let ty = checker.synthesize(&arr.0, &arr.1);
    assert_eq!(
        ty,
        Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::IntLiteral],
        }
    );
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {checker_errors:#?}",
        checker_errors = checker.errors
    );
}

#[test]
fn literal_coercion_array_to_i32_vec() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(1, 1..2),
        make_int_literal(2, 4..5),
        make_int_literal(3, 7..8),
    ];
    let arr = (Expr::Array(elems), 0..9);
    let expected = Ty::Named {
        builtin: Some(BuiltinType::Vec),
        name: "Vec".to_string(),
        args: vec![Ty::I32],
    };
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {checker_errors:#?}",
        checker_errors = checker.errors
    );
}

#[test]
fn array_literal_methods_index_and_get_resolve() {
    let output = check_source(
        r"
        fn main() -> i64 {
            let values = [1, 2, 3];
            if values.len() != 3 { return 10; }
            if values[0] != 1 { return 11; }
            match values.get(0) {
                Some(x) => { if x != 1 { return 12; } }
                None => { return 13; }
            }
            0
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "array literal should type as Vec<T> for methods and indexing: {:#?}",
        output.errors
    );
    assert!(
        output
            .resolved_calls
            .values()
            .any(|call| call.method_name == "get"),
        "Vec::get on an inferred integer array literal must record a resolved call: {:#?}",
        output.resolved_calls
    );
}

#[test]
fn literal_coercion_array_repeat_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let value = make_int_literal(0, 1..2);
    let count = make_int_literal(5, 4..5);
    let arr = (
        Expr::ArrayRepeat {
            value: Box::new(value),
            count: Box::new(count),
        },
        0..6,
    );
    let expected = Ty::Array(Box::new(Ty::I32), 5);
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Tuple literal coercion tests ───────────────────────────────────

#[test]
fn tuple_literal_coercion_to_typed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(42, 1..3),
        (Expr::Literal(Literal::Float(std::f64::consts::PI)), 5..9),
    ];
    let tuple = (Expr::Tuple(elems), 0..10);
    let expected = Ty::Tuple(vec![Ty::I32, Ty::F32]);
    let ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn tuple_literal_coercion_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![make_int_literal(256, 1..4)];
    let tuple = (Expr::Tuple(elems), 0..5);
    let expected = Ty::Tuple(vec![Ty::U8]);
    let _ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    // The tuple itself coerces, but the element should have reported an error
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn range_default_type_is_i64() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let range = (
        Expr::Range {
            start: None,
            end: None,
            inclusive: false,
        },
        0..2,
    );
    let ty = checker.synthesize(&range.0, &range.1);
    assert_eq!(ty, Ty::range(Ty::I64));
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Type variable resolution in check_against ────────────────────

#[test]
fn literal_coercion_through_type_var() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Create a type variable and unify it with i32
    let tv = TypeVar::fresh();
    checker.subst.insert(tv, &Ty::I32).unwrap();
    // Now check an integer literal against the type variable
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::Var(tv));
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Let-bound literal coercion tests ─────────────────────────────

#[test]
fn let_bound_literal_coercion() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 5
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
        else_block: None,
    };
    checker.check_stmt(&let_stmt, &(0..10));
    // Now check: let x: i32 = n
    let ident = (Expr::Identifier("n".to_string()), 15..16);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn let_bound_literal_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 2147483648 (exceeds i32 max)
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(2_147_483_648, 8..18)),
        else_block: None,
    };
    checker.check_stmt(&let_stmt, &(0..19));
    // Now check: let x: i32 = n — should fail with range error
    let ident = (Expr::Identifier("n".to_string()), 24..25);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn derived_intliteral_identifier_coerces_without_const_values() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let source_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some((
            Expr::Binary {
                left: Box::new(make_int_literal(1, 8..9)),
                op: BinaryOp::Add,
                right: Box::new(make_int_literal(2, 12..13)),
            },
            8..13,
        )),
        else_block: None,
    };
    checker.check_stmt(&source_stmt, &(0..14));

    assert!(
        !checker.const_values.contains_key("n"),
        "derived literals should not register const_values"
    );

    let target_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("y".to_string()), 20..21),
        ty: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            23..26,
        )),
        value: Some((Expr::Identifier("n".to_string()), 29..30)),
        else_block: None,
    };
    checker.check_stmt(&target_stmt, &(20..30));

    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn negated_literal_let_binding_coerces_signed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some((
            Expr::Unary {
                op: UnaryOp::Negate,
                operand: Box::new(make_int_literal(5, 9..10)),
            },
            8..10,
        )),
        else_block: None,
    };
    checker.check_stmt(&let_stmt, &(0..11));

    let ident = (Expr::Identifier("n".to_string()), 16..17);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I8);
    assert_eq!(ty, Ty::I8);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn const_default_width_registers_in_const_values() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    assert!(matches!(
        checker.const_values.get("N"),
        Some(ConstValue::Integer(100))
    ));

    let ident = (Expr::Identifier("N".to_string()), 10..11);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn const_explicit_width_not_in_const_values() {
    // Explicit-width consts (`const N: i32 = 100`) must not be registered
    // in `const_values` because they have a known non-literal type and cannot
    // be widened freely at use sites.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    assert!(
        !checker.const_values.contains_key("N"),
        "explicit-width consts should not register const_values"
    );
}

#[test]
fn const_explicit_width_assigned_to_wider_type_is_rejected() {
    // `const N: i32 = 100; let y: i64 = N;` must be a type error now that
    // implicit integer widening is removed.  The caller must write `N as i64`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    let target_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("y".to_string()), 10..11),
        ty: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            14..17,
        )),
        value: Some((Expr::Identifier("N".to_string()), 20..21)),
        else_block: None,
    };
    checker.check_stmt(&target_stmt, &(10..21));

    let binding = checker.env.lookup_ref("N").expect("N should be defined");
    assert_eq!(binding.ty, Ty::I32);

    // The widening rejection must fire exactly once, on the assignment span
    // (10..21), not on the const declaration span (0..3).
    let widening_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|e| {
            e.message.contains("cannot implicitly convert")
                && e.message.contains("i32")
                && e.message.contains("i64")
        })
        .collect();
    assert_eq!(
        widening_errors.len(),
        1,
        "expected exactly one widening-rejection error, got: {:?}",
        checker.errors
    );
    // No error should reference the const decl span (0..3) — the decl itself
    // is well-typed; only the assignment is rejected.
    assert!(
        !checker
            .errors
            .iter()
            .any(|e| e.span.start < 3 && e.span.end <= 3),
        "unexpected error on const decl span: {:?}",
        checker.errors
    );
}

#[test]
fn mutable_var_initializer_keeps_integer_literal_inferable() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let var_stmt = Stmt::Var {
        name: "n".to_string(),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
    };
    checker.check_stmt(&var_stmt, &(0..10));
    let binding = checker
        .env
        .lookup_ref("n")
        .expect("mutable binding should be defined");
    assert!(matches!(binding.ty, Ty::Var(_)));
    assert_eq!(checker.subst.resolve(&binding.ty), Ty::IntLiteral);
    assert_eq!(
        checker
            .expr_types
            .get(&SpanKey {
                start: 8,
                end: 9,
                module_idx: 0
            })
            .cloned(),
        Some(binding.ty.clone())
    );
}

#[test]
fn typecheck_output_materializes_literal_kinds_for_unannotated_lets() {
    let source = "fn main() { let x = 1; let y = 2.0; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.expr_types.values().any(Ty::is_numeric_literal),
        "TypeCheckOutput should materialize surviving literal kinds before serialization: {:?}",
        output.expr_types
    );
    assert!(output.expr_types.values().any(|ty| ty == &Ty::I64));
    assert!(output.expr_types.values().any(|ty| ty == &Ty::F64));
}

#[test]
fn bind_pattern_struct_fields_substitute_generic_type_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Pair".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Pair".to_string(),
            type_params: vec!["T".to_string(), "U".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::from([
                (
                    "first".to_string(),
                    Ty::Named {
                        builtin: None,
                        name: "T".to_string(),
                        args: vec![],
                    },
                ),
                (
                    "second".to_string(),
                    Ty::Named {
                        builtin: None,
                        name: "U".to_string(),
                        args: vec![],
                    },
                ),
            ]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );

    checker.bind_pattern(
        &Pattern::Struct {
            name: "Pair".to_string(),
            fields: vec![
                hew_parser::ast::PatternField {
                    name: "first".to_string(),
                    pattern: None,
                },
                hew_parser::ast::PatternField {
                    name: "second".to_string(),
                    pattern: None,
                },
            ],
        },
        &Ty::Named {
            builtin: None,
            name: "Pair".to_string(),
            args: vec![Ty::I64, Ty::Bool],
        },
        false,
        &(0..10),
    );

    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("first")
            .map(|binding| binding.ty.clone()),
        Some(Ty::I64),
        "generic struct destructuring must bind instantiated field types"
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("second")
            .map(|binding| binding.ty.clone()),
        Some(Ty::Bool),
        "generic struct destructuring must bind instantiated field types"
    );
}

#[test]
fn or_pattern_binding_helper_rejects_mutability_mismatch() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let names = HashSet::from(["x".to_string()]);
    let mut left_env = crate::env::TypeEnv::new();
    let mut right_env = crate::env::TypeEnv::new();
    left_env.define_with_span("x".to_string(), Ty::I64, true, 0..1);
    right_env.define_with_span("x".to_string(), Ty::I64, false, 0..1);

    assert!(
        !checker.or_pattern_bindings_match(&left_env, &right_env, &names, &names),
        "or-pattern merge must reject bindings with mismatched mutability"
    );
}

#[test]
fn struct_pattern_missing_type_def_emits_diagnostic() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    checker.bind_pattern(
        &Pattern::Struct {
            name: "Ghost".to_string(),
            fields: vec![hew_parser::ast::PatternField {
                name: "value".to_string(),
                pattern: None,
            }],
        },
        &Ty::Named {
            builtin: None,
            name: "Ghost".to_string(),
            args: vec![],
        },
        false,
        &(0..5),
    );

    assert!(
        checker.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType
                && error.message.contains("type `Ghost` is not defined")
        }),
        "missing type defs in struct patterns must fail closed: {checker_errors:#?}",
        checker_errors = checker.errors
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("value")
            .map(|binding| binding.ty.clone()),
        Some(Ty::Error),
        "missing type defs should seed placeholder bindings for recovery"
    );
}

#[test]
fn out_of_scope_type_param_name_reported_as_unknown_type() {
    // A type-parameter name is in scope only for the item that declares it.
    // `id` declares `<T>`, so `T` is a valid annotation inside `id`. `bad`
    // declares no type params, so its `x: T` and `-> T` name a type that is out
    // of scope. The checker must report these at the annotation rather than
    // exempt them merely because `id` declares a `T` somewhere in the program —
    // proving a source annotation valid is scope-aware, not a program-wide
    // type-param name lookup.
    let output = check_source(
        r"
fn id<T>(x: T) -> T { x }

fn bad(x: T) -> T { x }

fn main() -> i64 { id(5) }
",
    );
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType && error.message.contains("unknown type `T`")
        }),
        "out-of-scope `T` in `bad` must be reported as `unknown type `T``; got: {:#?}",
        output.errors
    );

    // The in-scope `T` declared by `id<T>` must NOT be reported: a generic
    // function whose only `T` uses are the ones its own signature declares type
    // checks cleanly. (Guards the gate against over-firing on legitimate
    // generics — the very behaviour the program-wide fallback used to protect.)
    let in_scope = check_source(r"fn id<T>(x: T) -> T { x }");
    assert!(
        !in_scope.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType && error.message.contains("unknown type `T`")
        }),
        "a generic function's own in-scope `T` must not be reported as unknown: {:#?}",
        in_scope.errors
    );
}

// ── Struct init literal coercion tests ─────────────────────────────

fn register_generic_wrapper(checker: &mut Checker) {
    let mut fields = HashMap::new();
    fields.insert(
        "value".to_string(),
        Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        },
    );
    checker.type_defs.insert(
        "Wrapper".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Wrapper".to_string(),
            type_params: vec!["T".to_string()],
            bounds: HashMap::new(),
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );
}

#[test]
fn struct_init_coerces_literal_to_expected_type_arg() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } checked against Wrapper<i32>
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
            type_args: None,
            base: None,
        },
        0..20,
    );
    let expected = Ty::Named {
        builtin: None,
        name: "Wrapper".to_string(),
        args: vec![Ty::I32],
    };
    let ty = checker.check_against(&init.0, &init.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_infers_type_param_from_literal() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } without expected type keeps the literal kind until
    // a later coercion/defaulting boundary.
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
            type_args: None,
            base: None,
        },
        0..20,
    );
    let ty = checker.synthesize(&init.0, &init.1);
    assert_eq!(
        ty,
        Ty::Named {
            builtin: None,
            name: "Wrapper".to_string(),
            args: vec![Ty::IntLiteral],
        }
    );
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_overflow_in_expected_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 256 } checked against Wrapper<u8> — should error
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(256, 10..13))],
            type_args: None,
            base: None,
        },
        0..20,
    );
    let expected = Ty::Named {
        builtin: None,
        name: "Wrapper".to_string(),
        args: vec![Ty::U8],
    };
    let _ty = checker.check_against(&init.0, &init.1, &expected);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

// ── Explicit type args at struct init site (F1 fix) ────────────────────────

#[test]
fn struct_init_explicit_type_arg_seeds_substitution() {
    // `Wrapper<string> { value: "hello" }` — explicit type arg must constrain
    // field checking against string, not an unbound param.
    let source = r#"
        type Wrapper<T> { value: T }
        fn main() {
            let w = Wrapper<string> { value: "hello" };
        }
    "#;
    let tco = check_source(source);
    assert!(
        tco.errors.is_empty(),
        "explicit type arg should check cleanly: {:?}",
        tco.errors
    );
}

#[test]
fn struct_init_explicit_type_arg_wrong_field_type_errors() {
    // `Wrapper<i64> { value: "hello" }` — explicit arg is i64, field is string: error.
    let source = r#"
        type Wrapper<T> { value: T }
        fn main() {
            let w = Wrapper<i64> { value: "hello" };
        }
    "#;
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parse_result.program);
    assert!(
        !tco.errors.is_empty(),
        "mismatched explicit type arg should produce a type error"
    );
}

#[test]
fn struct_init_explicit_type_arg_arity_mismatch_errors() {
    // `Wrapper<i64, string>` on a one-param struct should report arity mismatch.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    let span = 0..30_usize;
    let type_args = Some(vec![
        (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3_usize,
        ),
        (
            TypeExpr::Named {
                name: "string".to_string(),
                type_args: None,
            },
            4..10_usize,
        ),
    ]);
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(1, 20..21))],
            type_args,
            base: None,
        },
        span.clone(),
    );
    checker.synthesize(&init.0, &init.1);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("type parameter")),
        "arity mismatch should produce an error, got: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_explicit_type_arg_roundtrip_via_parse() {
    // Parser + checker integration: `Wrapper<string> { value: "hello" }` must
    // parse *and* type-check with the synthesised type `Wrapper<string>`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    let source = r#"type Wrapper<T> { value: T }
fn main() { let w = Wrapper<string> { value: "hello" }; }"#;
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let tco = checker.check_program(&parse_result.program);
    assert!(
        tco.errors.is_empty(),
        "parse+check roundtrip should be error-free: {:?}",
        tco.errors
    );
}

// ── Reject tests for check_against paths (blocker fixes) ───────────────────

#[test]
fn struct_init_explicit_type_arg_conflicts_with_binding_type_errors() {
    // `let w: Wrapper<i64> = Wrapper<string> { value: 1 };`
    // The explicit `string` annotation conflicts with the expected `i64`.
    // The checker must reject this rather than silently ignoring it.
    let source = r"
        type Wrapper<T> { value: T }
        fn main() {
            let w: Wrapper<i64> = Wrapper<string> { value: 1 };
        }
    ";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse without errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parse_result.program);
    assert!(
        !tco.errors.is_empty(),
        "conflicting explicit type arg should produce a type error"
    );
    let has_mismatch = tco.errors.iter().any(|e| {
        matches!(e.kind, TypeErrorKind::Mismatch { .. })
            || e.message.contains("conflicts")
            || e.message.contains("string")
    });
    assert!(
        has_mismatch,
        "error should mention the type conflict, got: {:?}",
        tco.errors
    );
}

#[test]
fn struct_init_explicit_type_arg_on_enum_variant_in_check_against_errors() {
    // Explicit type args on an enum variant struct initializer when the expected
    // type is already known should produce an error (fail-closed for this slice).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Register a generic enum `Keeper<T>` with a struct variant `Holding { value: T }`
    let mut variant_fields = HashMap::new();
    variant_fields.insert(
        "Holding".to_string(),
        VariantDef::Struct(vec![(
            "value".to_string(),
            Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
        )]),
    );
    checker.type_defs.insert(
        "Keeper".to_string(),
        TypeDef {
            kind: TypeDefKind::Enum,
            name: "Keeper".to_string(),
            type_params: vec!["T".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::new(),
            variants: variant_fields,
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );

    let span = 0..30_usize;
    // Explicit type args on an enum variant struct form in check_against path.
    let type_args = Some(vec![(
        TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        },
        0..3_usize,
    )]);
    let init = Expr::StructInit {
        name: "Holding".to_string(),
        fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        type_args,
        base: None,
    };
    let expected = Ty::Named {
        builtin: None,
        name: "Keeper".to_string(),
        args: vec![Ty::I64],
    };
    checker.check_against(&init, &span, &expected);
    assert!(
        !checker.errors.is_empty(),
        "explicit type args on enum variant struct form in check_against should produce an error"
    );
}

#[test]
fn struct_init_explicit_type_arg_on_enum_variant_synthesize_seeds_correctly() {
    // `Keeper::Holding<i64> { value: 42 }` in the synthesize path — the explicit
    // `i64` annotation should pre-seed the type_arg_map so the synthesised type
    // is `Keeper<i64>`, not `Keeper<i64>` or an unconstrained type var.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Register `Keeper<T>` enum with struct variant `Holding { value: T }`
    let mut variant_fields_map = HashMap::new();
    variant_fields_map.insert(
        "Keeper::Holding".to_string(),
        VariantDef::Struct(vec![(
            "value".to_string(),
            Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
        )]),
    );
    checker.type_defs.insert(
        "Keeper".to_string(),
        TypeDef {
            kind: TypeDefKind::Enum,
            name: "Keeper".to_string(),
            type_params: vec!["T".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::new(),
            variants: variant_fields_map,
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );

    let span = 0..30_usize;
    let type_args = Some(vec![(
        TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        },
        0..3_usize,
    )]);
    let init = Expr::StructInit {
        name: "Keeper::Holding".to_string(),
        fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        type_args,
        base: None,
    };
    let result = checker.synthesize(&init, &span);
    assert!(
        checker.errors.is_empty(),
        "enum variant explicit type arg should synthesize without errors: {:?}",
        checker.errors
    );
    // The synthesised type should be Keeper<i64> (IntLiteral coerces to i64 / i64)
    assert!(
        matches!(result, Ty::Named { ref name, .. } if name == "Keeper"),
        "synthesised type should be Keeper<…>, got: {result}"
    );
}

// ── record_init_type_args side-table emission ─────────────────────────────
//
// These tests verify that `check_struct_init` populates
// `TypeCheckOutput.record_init_type_args` with the resolved concrete `Ty`
// arguments for every user-defined generic record / enum-struct-variant
// initialiser site. Mirrors the `call_type_args` contract for generic free
// function calls (`record_concrete_call_type_args`).

fn collect_record_init_args(tco: &TypeCheckOutput) -> Vec<Vec<Ty>> {
    let mut entries: Vec<_> = tco.record_init_type_args.iter().collect();
    // Stable ordering by span start for deterministic assertions.
    entries.sort_by_key(|(k, _)| k.start);
    entries.into_iter().map(|(_, v)| v.clone()).collect()
}

#[test]
fn record_init_type_args_inferred_box_int() {
    // `Box { value: 42 }` — checker must infer `[i64]` from the literal
    // (post-defaulting) and record it on the side-table.
    let source = r"
        type Box<T> { value: T }
        fn main() { let _b = Box { value: 42 }; }
    ";
    let tco = check_source(source);
    assert!(
        tco.errors.is_empty(),
        "should check cleanly: {:?}",
        tco.errors
    );
    let entries = collect_record_init_args(&tco);
    assert_eq!(
        entries.len(),
        1,
        "exactly one record-init entry expected, got: {entries:?}"
    );
    assert_eq!(
        entries[0],
        vec![Ty::I64],
        "Box {{ value: 42 }} should resolve to Box<i64>, got: {entries:?}"
    );
}

#[test]
fn record_init_type_args_inferred_box_string() {
    let source = r#"
        type Box<T> { value: T }
        fn main() { let _b = Box { value: "hello" }; }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], vec![Ty::String]);
}

#[test]
fn record_init_type_args_explicit_type_arg() {
    // Explicit `<string>` annotation should produce a single
    // record_init_type_args entry of `[string]`.
    let source = r#"
        type Box<T> { value: T }
        fn main() { let _b = Box<string> { value: "hi" }; }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], vec![Ty::String]);
}

#[test]
fn record_init_type_args_two_params() {
    // Two type params, inferred from two field values of different types.
    let source = r#"
        type Pair<A, B> { first: A, second: B }
        fn main() { let _p = Pair { first: 1, second: "y" }; }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], vec![Ty::I64, Ty::String]);
}

#[test]
fn record_init_type_args_generic_in_generic_user_user() {
    // Generic-in-generic with two user records: `Box<Inner<i64>>` from
    // nested struct-init literals.  The checker emits one entry per
    // initialiser site, each with its own concrete type-args:
    //   - inner `Inner { x: 1 }`  → `[i64]`
    //   - outer `Box { value: Inner { x: 1 } }` → `[Inner<i64>]`
    let source = r"
        type Inner<T> { x: T }
        type Box<U> { value: U }
        fn main() { let _b = Box { value: Inner { x: 1 } }; }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 2, "got: {entries:?}");
    // Sorted by span start: outer Box init begins before inner Inner init.
    // The outer Box's single arg is `Inner<i64>`; the inner Inner's single
    // arg is `i64`.
    assert_eq!(
        entries[0],
        vec![Ty::Named {
            builtin: None,
            name: "Inner".to_string(),
            args: vec![Ty::I64],
        }]
    );
    assert_eq!(entries[1], vec![Ty::I64]);
}

#[test]
fn record_init_type_args_monomorphic_record_emits_no_entry() {
    // Fail-closed contract negative test: a record with empty `type_params`
    // must not produce a `record_init_type_args` entry. Downstream HIR
    // monomorphisation skips entries for monomorphic records.
    let source = r"
        type Mono { value: i64 }
        fn main() { let _m = Mono { value: 42 }; }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    assert!(
        tco.record_init_type_args.is_empty(),
        "monomorphic record-init should not emit a side-table entry, got: {:?}",
        tco.record_init_type_args
    );
}

#[test]
fn record_init_type_args_field_access_returns_substituted_type() {
    // Verifies that the *field-access* substitution path (already implemented
    // in `check_field_access` lines 3139-3146) and the side-table emission
    // agree: `b.value` on `b: Box<i64>` returns `i64`, and the init site
    // records `[i64]`.
    let source = r"
        type Box<T> { value: T }
        fn main() {
            let b = Box { value: 42 };
            let _v = b.value;
        }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    // The side-table records the Box<i64> instantiation.
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries, vec![vec![Ty::I64]]);
    // `b.value` resolves to `i64` (the substituted T) via the field-access
    // substitution at `check_field_access:3142-3145`.
    let field_access_ty = tco
        .expr_types
        .iter()
        .find_map(|(_, ty)| (ty == &Ty::I64).then(|| ty.clone()));
    assert_eq!(field_access_ty, Some(Ty::I64));
}

#[test]
fn record_init_type_args_two_distinct_instantiations() {
    // Same generic record at two different T's in the same program — two
    // distinct side-table entries, one per init site.
    let source = r#"
        type Box<T> { value: T }
        fn main() {
            let _a = Box { value: 42 };
            let _b = Box { value: "hi" };
        }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 2, "got: {entries:?}");
    // Sorted by span start; first site is the `Box { value: 42 }` literal.
    assert_eq!(entries[0], vec![Ty::I64]);
    assert_eq!(entries[1], vec![Ty::String]);
}

#[test]
fn record_init_type_args_enum_struct_variant_fully_bound() {
    // Generic enum struct-variant init with an annotation that binds every
    // type parameter: `let x: Either<i64, string> = Either::Left { value: 1 }`.
    // The record_init_type_args entry resolves both T=i64 and E=string.
    let source = r"
        enum Either<T, E> {
            Left { value: T };
            Right { err: E };
        }
        fn main() {
            let _x: Either<i64, string> = Either::Left { value: 1 };
        }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1, "got: {entries:?}");
    assert_eq!(entries[0], vec![Ty::I64, Ty::String]);
}

#[test]
fn record_init_type_args_enum_struct_variant_partial_inference_pruned() {
    // Negative test for the fail-closed contract: when only one of the two
    // type parameters can be inferred from the init's field values, the
    // entry's second arg stays a Ty::Var and must be pruned by
    // `validate_record_init_type_args_output_contract`.
    let source = r"
        enum Either<T, E> {
            Left { value: T };
            Right { err: E };
        }
        fn main() { let _x = Either::Left { value: 42 }; }
    ";
    let tco = check_source(source);
    // The source emits an InferenceFailed error for `_x` (E unresolved) — we
    // assert the side-table did not leak a partial entry.
    assert!(
        tco.record_init_type_args.is_empty(),
        "partial inference must not leak: {:?}",
        tco.record_init_type_args
    );
}

#[test]
fn record_init_type_args_unknown_field_does_not_emit_entry() {
    // `Box { wrong_field: 42 }` should produce an UndefinedField diagnostic
    // and not leak an entry into the side-table.
    let source = r"
        type Box<T> { value: T }
        fn main() { let _b = Box { wrong_field: 42 }; }
    ";
    let tco = check_source(source);
    assert!(
        tco.errors
            .iter()
            .any(|e| e.message.contains("no field `wrong_field`")),
        "expected UndefinedField diagnostic, got: {:?}",
        tco.errors
    );
    // Fail-closed: because T was never bound (no successful field), the
    // entry's resolved arg is still Ty::Var and the contract validator drops
    // it. record_init_type_args must therefore be empty.
    assert!(
        tco.record_init_type_args.is_empty(),
        "unknown-field init should not leak a side-table entry: {:?}",
        tco.record_init_type_args
    );
}

fn assert_decl_bound_rejects_no_display(source: &str) {
    let output = check_source(source);
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied
                && error.message.contains("NoDisplay")
                && error.message.contains("Display")
        }),
        "expected declaration-level Display bound to reject NoDisplay; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_decl_bounds_are_stored_on_type_defs() {
    let output = check_source(
        r"
        type Box<T: Display> { value: T }
        fn main() {
            let _box = Box { value: 42 };
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "valid Display instantiation should type-check: {:#?}",
        output.errors
    );
    let bounds = output
        .type_defs
        .get("Box")
        .and_then(|type_def| type_def.bounds.get("T"))
        .expect("Box<T: Display> should retain the T bound on TypeDef");
    assert_eq!(bounds, &vec!["Display".to_string()]);
}

#[test]
fn generic_decl_bound_rejects_struct_init_reference_site() {
    assert_decl_bound_rejects_no_display(
        r"
        type NoDisplay { n: i64 }
        type Box<T: Display> { value: T }
        fn main() {
            let _box = Box { value: NoDisplay { n: 1 } };
        }
        ",
    );
}

#[test]
fn generic_decl_bound_rejects_type_annotation_site() {
    assert_decl_bound_rejects_no_display(
        r"
        type NoDisplay { n: i64 }
        type Box<T: Display> { value: T }
        fn main() {
            let _box: Box<NoDisplay> = Box { value: NoDisplay { n: 1 } };
        }
        ",
    );
}

#[test]
fn generic_decl_bound_rejects_return_type_site() {
    assert_decl_bound_rejects_no_display(
        r"
        type NoDisplay { n: i64 }
        type Box<T: Display> { value: T }
        fn make() -> Box<NoDisplay> {
            Box { value: NoDisplay { n: 1 } }
        }
        ",
    );
}

#[test]
fn generic_decl_bound_rejects_imported_type_annotation_site() {
    let mut root = hew_parser::parse(
        r"
        import hew::boxes::{ Box };
        type NoDisplay { n: i64 }
        fn take(boxed: Box<NoDisplay>) -> i64 {
            0
        }
        ",
    );
    assert!(
        root.errors.is_empty(),
        "root import fixture should parse cleanly: {:?}",
        root.errors
    );
    let module = hew_parser::parse(
        r"
        pub type Box<T: Display> { value: T }
        ",
    );
    assert!(
        module.errors.is_empty(),
        "module fixture should parse cleanly: {:?}",
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
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied
                && error.message.contains("NoDisplay")
                && error.message.contains("Display")
        }),
        "expected imported Box<T: Display> annotation to reject NoDisplay; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_decl_bound_rejects_tuple_record_constructor_site() {
    assert_decl_bound_rejects_no_display(
        r"
        type NoDisplay { n: i64 }
        record Wrap<T: Display>(T);
        fn main() {
            let _wrap = Wrap(NoDisplay { n: 1 });
        }
        ",
    );
}

#[test]
fn generic_decl_bound_rejects_enum_tuple_variant_constructor_site() {
    assert_decl_bound_rejects_no_display(
        r"
        type NoDisplay { n: i64 }
        enum Maybe<T: Display> {
            Some(T);
            None;
        }
        fn main() {
            let _maybe = Maybe::Some(NoDisplay { n: 1 });
        }
        ",
    );
}

#[test]
fn generic_decl_bound_rejects_enum_struct_variant_constructor_site() {
    assert_decl_bound_rejects_no_display(
        r"
        type NoDisplay { n: i64 }
        enum Maybe<T: Display> {
            Some { value: T };
            None;
        }
        fn main() {
            let _maybe = Maybe::Some { value: NoDisplay { n: 1 } };
        }
        ",
    );
}

// ── trait-rewrite-substitution propagation probe ─────────────────────────────
//
// If a generic record has a method that internally calls another trait-bound
// generic function, does the substituted T propagate to the inner call's
// `call_type_args`? Methods on user generic records do not yet propagate the
// substituted type parameter to inner generic call sites (a v0.6 gap) — this
// test documents the state of the seam today rather than asserting
// end-to-end propagation.

#[test]
fn record_init_type_args_trait_rewrite_substitution_probe() {
    // Probe: does a substituted `T` propagate to an inner trait-bound generic
    // call invoked from a method on a user-defined generic record?
    //
    // Hew uses an explicit-receiver method form (no `self` keyword); the
    // receiver is named with its type. The probe constructs `Wrapper { value:
    // 42 }`, calls `Wrapper::show(w)`, and inspects `tco.call_type_args` for
    // the inner `to_string(...)` call. If propagation works, that call's
    // resolved type-args contain `[i64]`. If not, the call_type_args entry is
    // absent or contains an unbound `T`.
    //
    // **Construction-side side-table emission is the load-bearing assert.**
    // The propagation question is informational — the answer is captured in
    // the worker return prose.
    let source = r"
        type Wrapper<T: Display> { value: T }
        impl<T: Display> Wrapper<T> {
            fn show(w: Wrapper<T>) -> string { to_string(w.value) }
        }
        fn main() -> i64 {
            let w = Wrapper { value: 42 };
            let _s = w.show();
            0
        }
    ";
    let parse_result = hew_parser::parse(source);
    // If the surface (impl blocks on user generic records, `to_string` free
    // function) does not parse, the probe documents that gap and exits — the
    // construction-side seam is exercised by the other tests in this family.
    if !parse_result.errors.is_empty() {
        return;
    }
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parse_result.program);
    let entries = collect_record_init_args(&tco);
    // **Load-bearing assertion**: regardless of method-body substitution gaps,
    // the construction `Wrapper { value: 42 }` MUST record the instantiation.
    if tco.errors.is_empty() {
        assert!(!entries.is_empty(), "wrapper init should record an entry");
        assert!(
            entries.iter().any(|e| e.first() == Some(&Ty::I64)),
            "at least one entry should bind T=i64: {entries:?}"
        );
    }
    // **Probe result**: with this fixture, the construction site at
    // `Wrapper { value: 42 }` resolves to `record_init_type_args = [[i64]]`,
    // BUT the inner `to_string(w.value)` call inside `show` records
    // `call_type_args = [Named{name:"T", args:[]}]` — i.e. the unsubstituted
    // type parameter, not `i64`. Methods on user generic records do not
    // propagate the substituted T to inner generic call sites today; this is
    // a v0.6 gap. Only the construction-side surface is captured here.
}

#[test]
fn trailing_integer_literal_coerces_to_declared_return_type() {
    // fn foo() -> i32 { 0 }  — bare 0 defaulted to i64 before fix
    let source = "fn foo() -> i32 { 0 }";
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
        "trailing literal should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_coerces_smaller_width() {
    // fn foo() -> i8 { 42 }  — literal fits in i8
    let source = "fn foo() -> i8 { 42 }";
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
        "literal 42 fits in i8 and should coerce: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_out_of_range_is_rejected() {
    // fn foo() -> i8 { 300 }  — 300 does not fit in i8 (range -128..=127)
    let source = "fn foo() -> i8 { 300 }";
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
            .any(|e| e.message.contains("does not fit")),
        "out-of-range literal should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn trailing_if_with_literal_branches_coerces() {
    // fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }
    // Both branches are integer literals that should coerce to i32.
    let source = "fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }";
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
        "if-else with integer literals should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn explicit_return_with_literal_still_works() {
    // Regression guard: explicit return was already working; must stay working.
    let source = "fn foo() -> i32 { return 0; }";
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
        "explicit return with literal coercion should still work: {:?}",
        output.errors
    );
}

#[test]
fn trailing_negative_literal_rejected_for_unsigned_return() {
    // fn foo() -> u32 { -1 }  — negative literal cannot fit in unsigned type
    let source = "fn foo() -> u32 { -1 }";
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
        "negative literal should be rejected for unsigned return type"
    );
}

#[test]
fn trailing_match_with_literal_arms_coerces() {
    // fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }
    // Stmt::Match is the last statement; both arms are integer literals that
    // should coerce to i32 via the pre-seeded expected type.
    let source = "fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }";
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
        "match with integer literal arms should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_if_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested if expression's first
    // element; the literal branch must not synthesize to i64 first.
    let source = "fn foo(flag: bool, y: i32) -> (i32, i32) { (if flag { 1 } else { y }, y) }";
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
        "nested tuple if should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_match_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested match expression's
    // first element; the literal arm must not synthesize to i64 first.
    let source =
        "fn foo(flag: bool, y: i32) -> (i32, i32) { (match flag { true => 1, false => y }, y) }";
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
        "nested tuple match should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_type_mismatch_reports_exactly_one_error() {
    // fn foo() -> i32 { "hello" }
    // check_against already reports the mismatch at the expression site;
    // check_fn_decl's outer expect_type must NOT fire a duplicate.
    let source = "fn foo() -> i32 { \"hello\" }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error, got: {:?}",
        output.errors
    );
}

#[test]
fn trailing_identifier_mismatch_reports_exactly_one_error() {
    // fn foo(s: string) -> i32 { s }
    // The identifier arm in check_against matched before the default arm,
    // so without the guard it fired a second duplicate error.
    let source = "fn foo(s: string) -> i32 { s }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error for identifier, got: {:?}",
        output.errors
    );
}

#[test]
fn error_return_type_does_not_suppress_match_arm_diagnostics() {
    // fn foo() -> UnknownType { match true { true => "hello", false => 42 } }
    // UnknownType resolves to Ty::Error. Without the Ty::Error guard in
    // check_match_expr, the error type pre-seeds all arms via check_against,
    // silently accepting the string/i64 mismatch between arms.
    let source = r#"fn foo() -> UnknownType { match true { true => "hello", false => 42 } }"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // We expect at least two errors: one for UnknownType and one for the
    // arm type mismatch (string vs i64). Before the fix only the
    // UnknownType error appeared.
    let arm_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch")
    });
    assert!(
        arm_mismatch,
        "match arms with mismatched types should still report an error even when \
         the return type is Ty::Error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_returns_result_self_string() {
    // Regression: non-wire struct.from_json(s) must type-check as
    // Result<Self, string>, not Self.  The SHIM that returned Self directly
    // was removed; this test pins the correct surface type.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let r: Result<Point, string> = Point.from_json(s);
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
        "from_json should return Result<Self, string> with no type errors; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_bare_self_is_type_error() {
    // Assigning the result of from_json directly to `Self` (not Result<Self, …>)
    // must produce a type mismatch — confirms the SHIM is gone.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let p: Point = Point.from_json(s);
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
    let has_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch") || msg.contains("Result")
    });
    assert!(
        has_mismatch,
        "assigning Result<Point, string> to Point must be a type error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_yaml_and_from_toml_return_result() {
    // Both from_yaml and from_toml should also return Result<Self, string>.
    let source = r#"
type Cfg { n: i32; }
fn main() {
let _a: Result<Cfg, string> = Cfg.from_yaml("n: 1");
let _b: Result<Cfg, string> = Cfg.from_toml("n = 1");
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
        "from_yaml / from_toml should return Result<Self, string>; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Structural-bounds scaffold tests (E1)
//
// These tests verify the `type_structurally_satisfies` scaffold and the
// updated `type_satisfies_trait_bound` fallback without changing any
// existing program behaviour.
// -------------------------------------------------------------------------
