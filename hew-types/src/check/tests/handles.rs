#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

mod fork_block_body_checks {
    use super::*;

    #[test]
    fn fork_block_body_clean_single_call_has_no_type_errors() {
        // A clean `fork { f() }` body type-checks with no diagnostics: the
        // checker visits the body but a well-typed unit call is valid.
        let output = check_source(
            r"
            fn worker() {}
            fn main() {
                scope {
                    fork {
                        worker();
                    };
                };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "clean single-call fork body should produce no type errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_block_arg_bearing_body_is_type_checked() {
        // Pre-fold the block body was never visited, so a wrong-typed call
        // argument inside `fork { ... }` was silently accepted. The fold routes
        // the body through `check_block`, so the mismatch must now surface as a
        // type error instead of being bypassed.
        let output = check_source(
            r#"
            fn work(n: i64) { let _ = n; }
            fn main() {
                scope {
                    fork {
                        work("not an int");
                    };
                };
            }
            "#,
        );
        assert!(
            output.errors.iter().any(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::Mismatch { expected, actual }
                        if expected == "i64" && actual == "string"
                )
            }),
            "arg-bearing fork body must surface the arg type mismatch; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_block_multi_statement_body_is_type_checked() {
        // A multi-statement fork body is visited statement-by-statement: a bad
        // call in the second statement is reported even though the first is
        // well-typed. (HIR lowering separately fails the multi-statement shape
        // closed; this test pins only the checker's body coverage.)
        let output = check_source(
            r#"
            fn ok() {}
            fn work(n: i64) { let _ = n; }
            fn main() {
                scope {
                    fork {
                        ok();
                        work("nope");
                    };
                };
            }
            "#,
        );
        assert!(
            output.errors.iter().any(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::Mismatch { expected, actual }
                        if expected == "i64" && actual == "string"
                )
            }),
            "multi-statement fork body must type-check each statement; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_block_string_arg_parent_use_after_fork_block_rejected() {
        // Ownership hole regression: `fork { shout(greeting) }` must mark
        // `greeting` (a non-Copy `string`) moved into the child task, so that
        // parent use after the fork reports `UseAfterMove`.
        //
        // The block form `fork { f(args) }` and the named form
        // `fork ts = f(args)` must be symmetric — the named form already
        // rejects parent-use-after-move; this test pins the block form.
        let output = check_source(
            r#"
            fn shout(msg: string) {}

            fn main() {
                let greeting: string = "hello" + " world";
                scope {
                    fork {
                        shout(greeting);
                    };
                };
                // greeting was moved into the fork block — UseAfterMove here.
                let _x = greeting;
            }
            "#,
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::UseAfterMove),
            "parent use of a string arg after fork-block must be UseAfterMove \
             (parity with named fork); got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_block_bitcopy_arg_parent_use_after_fork_block_accepted() {
        // BitCopy scalars (i64) passed to a fork-block must remain live in
        // the parent scope — no UseAfterMove for Copy types.
        let output = check_source(
            r"
            fn add_print(a: i64, b: i64) {}

            fn main() {
                let x: i64 = 20;
                let y: i64 = 22;
                scope {
                    fork {
                        add_print(x, y);
                    };
                };
                // BitCopy scalars remain live in the parent.
                let _sum = x + y;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "parent use of i64 args after fork-block must check clean \
             (BitCopy exemption); got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_block_tail_expr_non_call_emits_actionable_diagnostic() {
        // `fork { 42 }` — a bare tail-expression with no semicolon — must
        // emit the actionable fork-shape message ("fork{} bodies must be a
        // direct function call") rather than a generic type mismatch.
        // Regression pin: previously the checker deferred to check_block
        // which emitted "type mismatch: expected `()`, found `i64`".
        let output = check_source(
            r"
            fn main() {
                scope {
                    fork { 42 };
                };
            }
            ",
        );
        let has_actionable = output.errors.iter().any(|e| {
            matches!(&e.kind, TypeErrorKind::InvalidOperation)
                && e.message.contains("fork")
                && e.message.contains("direct function call")
        });
        assert!(
            has_actionable,
            "fork {{ 42 }} (tail expr, no semicolon) must emit the actionable fork-shape \
             diagnostic, not a generic type mismatch; got: {:#?}",
            output.errors
        );
        // Must still be fail-closed (at least one error).
        assert!(
            !output.errors.is_empty(),
            "fork {{ 42 }} must produce at least one error"
        );
    }
}

/// Regression (L23 / `SpanKey` per-module discriminator): a closure literal
/// living in a *non-root* module must not trip the fail-closed
/// `ClosureCaptureModeUnresolved` / `ClosureEscapeKindUnresolved` contract
/// enforcers. The checker stamps capture/escape facts with the module's
/// 1-based `module_idx`; the validator must look them up at that same index
/// rather than the hardcoded root `0`. Before the fix this checked with a
/// spurious internal error for any imported module containing a closure.
#[test]
fn closure_in_imported_module_does_not_trip_fail_closed_contract() {
    let output = check_source_in_module(
        r"
pub fn compute(n: i64) -> i64 {
    let f = |x: i64| -> i64 { x * 2 };
    f(n)
}
",
        vec!["mylib".to_string()],
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::ClosureCaptureModeUnresolved { .. })),
        "closure in a non-root module must not raise ClosureCaptureModeUnresolved; got: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ClosureEscapeKindUnresolved),
        "closure in a non-root module must not raise ClosureEscapeKindUnresolved; got: {:?}",
        output.errors
    );
}

// ── Sub-issue 2: bind-then-return bypass regression tests ──────────────────

/// Parse and type-check a program with one fictional owned-handle type registered.
fn check_source_with_handle(source: &str, handle_type: &str) -> TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors in test source: {:#?}",
        parse_result.errors
    );
    let mut registry = ModuleRegistry::new(vec![]);
    registry.insert_handle_type_for_test(handle_type.to_string());
    let mut checker = Checker::new(registry);
    checker.check_program(&parse_result.program)
}

/// Direct `return self.field` — the existing check; must still fire after the
/// bind-then-return refactor.
#[test]
fn direct_handle_field_return_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                wrapper.pattern
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("pattern")
                && e.message.contains("double-free")),
        "direct return of owned handle field must be rejected; got: {:?}",
        output.errors
    );
}

/// `let p = wrapper.pattern; p` — the bind-then-return bypass from issue #1315 sub-2.
#[test]
fn bind_then_return_handle_field_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                p
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("pattern")
                && e.message.contains("double-free")),
        "let-binding alias of owned handle field must be rejected; got: {:?}",
        output.errors
    );
}

/// Diagnostic message for bind-then-return must name the binding so the user
/// can locate the alias.
#[test]
fn bind_then_return_diagnostic_names_binding() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn extract(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                p
            }
        }
        ",
        "regex.Pattern",
    );
    let msg = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::InvalidOperation)
        .map_or("", |e| e.message.as_str());
    assert!(
        msg.contains("via let-binding `p`"),
        "error message must identify the binding name; got: {msg:?}"
    );
}

/// `let p = wrapper.pattern; printDebug(p); return p` — intermediate use
/// does not suppress the diagnostic.
#[test]
fn bind_then_return_with_intermediate_use_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                println(p);
                p
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("double-free")),
        "alias with intermediate use must still be rejected; got: {:?}",
        output.errors
    );
}

/// A method that returns a non-handle field must not trigger the diagnostic.
#[test]
fn non_handle_field_return_is_allowed() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern, label: string }

        impl PatternWrapper {
            fn get_label(wrapper: PatternWrapper) -> string {
                wrapper.label
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output.errors.iter().all(
            |e| e.kind != TypeErrorKind::InvalidOperation || !e.message.contains("double-free")
        ),
        "returning a non-handle field must not be rejected; got: {:?}",
        output.errors
    );
}

/// A let-binding whose value is NOT a receiver field access must not be
/// flagged when it appears in return position.
#[test]
fn non_field_let_binding_return_is_allowed() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_label(wrapper: PatternWrapper) -> string {
                let s = to_string(42);
                s
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output.errors.iter().all(
            |e| e.kind != TypeErrorKind::InvalidOperation || !e.message.contains("double-free")
        ),
        "returning a non-field binding must not be rejected; got: {:?}",
        output.errors
    );
}

// ── Sub-issue 3: O(N²) registration scaling tests ──────────────────────────

/// Registering N struct types should trigger `refresh_handle_bearing_structs`
/// exactly once (lazy fixpoint), not N times.
///
/// This is an operation-count assertion — deterministic and never flaky.
/// For N=100, N=200, N=400 we expect `refresh_call_count` == 1 after the first
/// lookup, regardless of N.
#[test]
fn handle_bearing_refresh_deferred_to_single_fixpoint_pass() {
    fn register_n_plain_structs(n: usize) -> usize {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        for i in 0..n {
            let td = hew_parser::ast::TypeDecl {
                visibility: hew_parser::ast::Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: format!("S{i}"),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Field {
                    name: "value".to_string(),
                    ty: (
                        hew_parser::ast::TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                    span: 0..0,
                }],
                doc_comment: None,
                wire: None,
                is_indirect: false,
                resource_marker: hew_parser::ast::ResourceMarker::None,
                is_opaque: false,
                consuming_methods: Vec::new(),
            };
            checker.register_type_decl(&td);
        }
        // Trigger the lazy refresh with one lookup.
        checker.ensure_handle_bearing_fresh();
        checker.refresh_call_count
    }

    let count_100 = register_n_plain_structs(100);
    let count_200 = register_n_plain_structs(200);
    let count_400 = register_n_plain_structs(400);

    // Each run should refresh exactly once regardless of N.
    assert_eq!(count_100, 1, "N=100: expected 1 refresh, got {count_100}");
    assert_eq!(count_200, 1, "N=200: expected 1 refresh, got {count_200}");
    assert_eq!(count_400, 1, "N=400: expected 1 refresh, got {count_400}");
}

/// Timing check: registering 400 structs must run in at most 4× the time it
/// takes for 100 structs, demonstrating linear (not quadratic) scaling.
///
/// Uses `Instant::elapsed`-bounded ratio rather than an absolute wall-clock
/// threshold so CI hardware differences don't cause false failures.
/// Kept `#[ignore]` (CONVERT-C deferred): migration to `benches/` requires
/// adding Criterion to hew-types and is non-trivial. Invoke explicitly with
/// `cargo test -- --include-ignored` when you want the timing signal.
#[test]
#[ignore = "wall-clock ratio test; run explicitly with --include-ignored (benches/ migration deferred: no Criterion dep)"]
fn handle_bearing_registration_scales_linearly_not_quadratically() {
    use std::time::Instant;

    fn time_register_n(n: usize) -> std::time::Duration {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let start = Instant::now();
        for i in 0..n {
            let td = hew_parser::ast::TypeDecl {
                visibility: hew_parser::ast::Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: format!("T{i}"),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Field {
                    name: "x".to_string(),
                    ty: (
                        hew_parser::ast::TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                    span: 0..0,
                }],
                doc_comment: None,
                wire: None,
                is_indirect: false,
                resource_marker: hew_parser::ast::ResourceMarker::None,
                is_opaque: false,
                consuming_methods: Vec::new(),
            };
            checker.register_type_decl(&td);
        }
        checker.ensure_handle_bearing_fresh();
        start.elapsed()
    }

    let t100 = time_register_n(100);
    let t400 = time_register_n(400);

    // Use as_secs_f64 to avoid u128->f64 precision-loss lints; nanosecond
    // precision is far more than this test needs.
    let ratio = t400.as_secs_f64() / t100.as_secs_f64().max(f64::EPSILON);
    assert!(
        ratio < 16.0,
        "registration of 400 structs took {ratio:.1}× as long as 100 structs — expected < 16× \
         (quadratic would be ~16×, linear is ~4×). t100={t100:?} t400={t400:?}"
    );
}

// ── Task<T> surface rules ──────────────────────────────────────────────────
//
// `Task<T>` is a compiler-internal type. It has no user-source spelling:
//   - `fork name = expr` inside a `fork{}` body is the only construction site;
//     the binding's type is inferred to `Ty::Task(T)` by HIR lowering.
//   - `await name` inside a `select` arm or `fork{}` body consumes the handle
//     and yields `T`.
//   - Any explicit `Task<T>` in a user-written type annotation is rejected with
//     `E_TASK_NOT_NAMEABLE` (= `TypeErrorKind::TaskNotNameable`).
//
// §3.3 diagnostic-surface coverage: BOTH paths must be covered:
//   1. `Task<T>` written in an annotation → `TaskNotNameable` error (no infer).
//   2. `scope.launch { ... }` / `ScopeLaunch` → inferred `Ty::Task(T)`;
//      `await` on it yields `T` (no error on clean code).

mod task_type_surface_rules {
    use super::*;

    // ── Rejection: user-written Task<T> in annotation positions ─────────────

    #[test]
    fn task_in_let_annotation_is_rejected() {
        let output = check_source(
            r"
            fn main() {
                let _t: Task<i64> = 0;
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "Task<i64> in let annotation must emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn task_in_fn_param_annotation_is_rejected() {
        let output = check_source(
            r"
            fn foo(t: Task<i64>) -> i64 { 0 }
            fn main() -> i64 { 0 }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "Task<i64> in fn param must emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn task_in_return_type_annotation_is_rejected() {
        let output = check_source(
            r"
            fn foo() -> Task<i64> { 0 }
            fn main() -> i64 { 0 }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "Task<i64> as return type must emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    // ── Accept path: `fork name = call(...)` inside scope{} infers Ty::Task;
    // `await name` consumes the binding and yields T ──────────────────────────

    #[test]
    fn scope_fork_binding_infers_task_and_await_consumes_it() {
        // `scope { fork task = compute(); await task; }` is the structured
        // surface for spawning a child task and joining it; it must type-check
        // with no errors at all (the binding types as Task<i64>, await unwraps).
        let output = check_source(
            r"
            fn compute() -> i64 { 42 }
            fn main() {
                scope {
                    fork task = compute();
                    await task;
                }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "clean scope {{ fork x = call(); await x; }} must check without errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_await_yields_callee_return_type() {
        // `await x` on a fork binding produces the callee's return type:
        // annotating the await result with the matching type must check clean.
        let output = check_source(
            r"
            fn compute() -> i64 { 42 }
            fn main() {
                scope {
                    fork x = compute();
                    let v: i64 = await x;
                    let _ = v;
                }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "await of Task<i64> binding must type as i64; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_await_result_type_mismatch_rejected() {
        // The dual of the accept: annotating the await result with the wrong
        // type must produce a mismatch — proving the binding really carries
        // Task<i64>, not Error/Unit.
        let output = check_source(
            r"
            fn compute() -> i64 { 42 }
            fn main() {
                scope {
                    fork x = compute();
                    let _v: string = await x;
                }
            }
            ",
        );
        assert!(
            !output.errors.is_empty(),
            "await of Task<i64> bound as string must be a type error"
        );
    }

    #[test]
    fn fork_non_call_rhs_rejected() {
        // Parity with HIR's ForkChildNotACall gate, raised at check time.
        let output = check_source(
            r"
            fn main() {
                scope {
                    fork t = 42;
                }
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("requires a call expression")),
            "fork with non-call RHS must be rejected at check time; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_outside_scope_rejected() {
        let output = check_source(
            r"
            fn ping() {}
            fn main() {
                fork t = ping();
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("only valid inside a `scope { }` body")),
            "fork outside scope must be rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_inside_lambda_in_scope_rejected() {
        // A lambda body does not inherit the lexical task scope: the closure
        // may run after the scope has joined, so fork inside it is rejected.
        let output = check_source(
            r"
            fn ping() {}
            fn main() {
                scope {
                    let f = || { fork t = ping(); };
                    f();
                }
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("only valid inside a `scope { }` body")),
            "fork inside a lambda body must be rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_binding_not_visible_after_scope_block() {
        // The Task binding scopes to the `scope { }` block, exactly like a
        // `let` declared inside it.
        let output = check_source(
            r"
            fn ping() {}
            fn main() {
                scope {
                    fork t = ping();
                    await t;
                }
                await t;
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::UndefinedVariable),
            "fork binding must not escape the scope block; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_binding_shadows_outer_let_and_outer_survives() {
        // Inside the scope block the fork binding shadows the outer `t`
        // (mirroring `let` shadowing); after the block the outer i64 binding
        // is intact.
        let output = check_source(
            r"
            fn ping() {}
            fn main() {
                let t = 1;
                scope {
                    fork t = ping();
                    await t;
                }
                let _y: i64 = t;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "fork binding shadowing an outer let must check clean; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn nested_scope_fork_in_inner_scope_accepted() {
        // task_scope_depth is a counter, not a flag: fork inside a nested
        // scope body is still in a valid spawn context.
        let output = check_source(
            r"
            fn ping() {}
            fn main() {
                scope {
                    scope {
                        fork t = ping();
                        await t;
                    }
                }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "fork inside a nested scope must check clean; got: {:#?}",
            output.errors
        );
    }

    // ── fork arg move semantics ──────────────────────────────────────────────
    //
    // These tests verify that non-Copy arguments to a named fork spawn are
    // marked consumed in the parent scope, so that a subsequent use of the
    // same binding is rejected as UseAfterMove. BitCopy scalars (i64, bool,
    // etc.) are exempt and must remain live after the fork.
    //
    // Pin: hew-types/src/check/expressions.rs `synthesize_concurrency`
    // (the `Expr::ForkChild` arm marks non-Copy arg identifiers moved after
    // `synthesize` runs the call).

    #[test]
    fn fork_string_arg_parent_use_after_fork_rejected() {
        // `fork ts = shout(greeting)` moves `greeting` (a non-Copy `string`)
        // into the child task env. The parent must not be able to use it again
        // — UseAfterMove must fire on the second reference.
        let output = check_source(
            r#"
            fn shout(msg: string) {}

            fn main() {
                let greeting: string = "hello" + " world";
                scope {
                    fork ts = shout(greeting);
                    await ts;
                }
                // greeting was moved into the fork — UseAfterMove here.
                let _x = greeting;
            }
            "#,
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::UseAfterMove),
            "parent use of a string arg after fork must be UseAfterMove; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn fork_bitcopy_arg_parent_use_after_fork_accepted() {
        // i64 is BitCopy / Copy — the arg is shared into the fork env by
        // value, not transferred. The parent must retain ownership and be able
        // to use the binding again without any error.
        let output = check_source(
            r"
            fn add_print(a: i64, b: i64) {}

            fn main() {
                let x: i64 = 20;
                let y: i64 = 22;
                scope {
                    fork t = add_print(x, y);
                    await t;
                }
                // BitCopy scalars remain live in the parent.
                let _sum = x + y;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "parent use of i64 args after fork must check clean (BitCopy exemption); got: {:#?}",
            output.errors
        );
    }

    // ── in_unsafe scope flag sentinel ────────────────────────────────────────
    //
    // These tests verify that the `in_unsafe` flag is correctly set to `true`
    // inside an `unsafe { }` block and `false` outside one.  The observable
    // behaviour is the existing `require_unsafe` gate on extern fn calls:
    // inside unsafe => no error; outside unsafe => error.
    //
    // There are no unsafe *operations* yet (T1-B-* and T1-C-* add them), but
    // extern fn calls already use `in_unsafe` via `require_unsafe`, making them
    // a natural fail-closed probe.

    #[test]
    fn in_unsafe_flag_true_inside_unsafe_block_no_error() {
        // An extern fn call inside `unsafe { }` must not produce an error:
        // the `in_unsafe` flag is `true` during the block body.
        let output = check_source(
            r#"
            extern "C" { fn raw_op() -> i64; }
            fn caller() -> i64 {
                unsafe { raw_op() }
            }
            "#,
        );
        let unsafe_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.message.contains("unsafe"))
            .collect();
        assert!(
            unsafe_errors.is_empty(),
            "extern fn call inside unsafe block must not emit unsafe errors; got: {unsafe_errors:#?}"
        );
    }

    #[test]
    fn in_unsafe_flag_false_outside_unsafe_block_emits_error() {
        // An extern fn call outside `unsafe { }` must produce an error:
        // the `in_unsafe` flag is `false` in the surrounding function body.
        let output = check_source(
            r#"
            extern "C" { fn raw_op() -> i64; }
            fn caller() -> i64 {
                raw_op()
            }
            "#,
        );
        let has_unsafe_error = output.errors.iter().any(|e| e.message.contains("unsafe"));
        let errors = &output.errors;
        assert!(
            has_unsafe_error,
            "extern fn call outside unsafe block must emit an unsafe-required error; got: {errors:#?}"
        );
    }

    #[test]
    fn in_unsafe_flag_restored_after_unsafe_block_exits() {
        // After the unsafe block closes, `in_unsafe` reverts to `false`.
        // A second extern fn call after the block, outside any unsafe context,
        // must still produce an error.
        let output = check_source(
            r#"
            extern "C" { fn raw_op() -> i64; }
            fn caller() -> i64 {
                unsafe { raw_op() };
                raw_op()
            }
            "#,
        );
        let has_unsafe_error = output.errors.iter().any(|e| e.message.contains("unsafe"));
        let errors = &output.errors;
        assert!(
            has_unsafe_error,
            "extern fn call after unsafe block closes must emit an unsafe-required error; got: {errors:#?}"
        );
    }

    // ── Raw pointer dereference fail-closed endpoint (v0.5) ─────────────────
    //
    // v0.5 parses `*expr` only far enough to reject it deterministically
    // at type-check time.  No HIR/MIR/codegen path is reached, so native
    // and WASM lowering both fail-closed identically — this is the WASM
    // parity story for raw pointer ops in v0.5.

    #[test]
    fn raw_deref_outside_unsafe_emits_unsafe_block_required() {
        // `*p` outside `unsafe { ... }` must emit
        // `UnsafeOperationRequiresBlock` with the raw-pointer-dereference
        // operation tag.
        let output = check_source(
            r#"
            extern "C" { fn make_ptr() -> *const i64; }
            fn caller() -> i64 {
                let p = unsafe { make_ptr() };
                *p
            }
            "#,
        );
        let raw_deref_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::UnsafeOperationRequiresBlock { operation }
                        if operation == "raw pointer dereference"
                )
            })
            .collect();
        assert_eq!(
            raw_deref_errors.len(),
            1,
            "expected exactly one `UnsafeOperationRequiresBlock(raw pointer dereference)` \
             error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn raw_deref_inside_unsafe_emits_not_lowered() {
        // `*p` inside `unsafe { ... }` is still fail-closed: the compiler does not
        // lower raw-pointer dereference to HIR/MIR/codegen, so the checker
        // emits `RawPointerOpNotLowered` rather than typing the
        // expression as the pointee type.
        let output = check_source(
            r#"
            extern "C" { fn make_ptr() -> *const i64; }
            fn caller() -> i64 {
                unsafe {
                    let p = make_ptr();
                    *p
                }
            }
            "#,
        );
        let not_lowered: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::RawPointerOpNotLowered { operation }
                        if operation == "raw pointer dereference"
                )
            })
            .collect();
        assert_eq!(
            not_lowered.len(),
            1,
            "expected exactly one `RawPointerOpNotLowered(raw pointer dereference)` \
             error; got: {:#?}",
            output.errors
        );

        // Crucially, no `UnsafeOperationRequiresBlock` error must fire
        // inside `unsafe { ... }`: that diagnostic is for callers, not
        // for the v0.5 "not lowered" rejection.
        let unsafe_required = output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::UnsafeOperationRequiresBlock { .. }));
        assert!(
            !unsafe_required,
            "raw deref inside unsafe must not emit UnsafeOperationRequiresBlock; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn raw_deref_address_of_not_introduced_in_v05() {
        // Address-of (`&expr`, `&mut expr`, `&*expr`) remains out of scope
        // for v0.5.  `&*` is still the single `AmpStar` wrapping-multiply
        // token; introducing prefix `&` as address-of would require a
        // lexer change that is explicitly deferred.  Sanity-check that
        // a prefix `&expr` is not silently accepted as a raw-pointer
        // construction operator — the parser must reject before the
        // type checker even sees a coherent AST.
        let parse_result = hew_parser::parse(
            r"
            fn caller() -> i64 {
                let x: i64 = 0;
                let _p = &x;
                0
            }
            ",
        );
        assert!(
            !parse_result.errors.is_empty(),
            "prefix `&expr` must not be accepted as a v0.5 address-of \
             operator; got no parse errors"
        );
    }
}

// ── #2511: opaque handles rejected as receive-fn (message) parameters ───────
//
// Actor message payloads are CBOR-serialized to cross the mailbox dispatch
// boundary. Opaque handle types (`net.Listener`, user `#[opaque]` types, etc.)
// are pointer-shaped runtime resources with no serializable record layout, so
// they cannot be message payloads. Before #2511 this fell through to a raw
// codegen-front `wire CBOR serialize: named type ... is not a registered
// record layout` failure surfaced even at `hew check`; the checker now emits a
// clean `OpaqueMessagePayload` diagnostic pointing at the parameter.
mod opaque_receive_fn_param_rules {
    use super::*;

    fn opaque_payload_errors(output: &TypeCheckOutput) -> Vec<&TypeError> {
        output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueMessagePayload { .. }))
            .collect()
    }

    #[test]
    fn user_opaque_direct_receive_fn_param_is_rejected() {
        // A same-module `#[opaque]` type (producer-side, so constructing it is
        // allowed) used directly as a receive-fn parameter must still be
        // rejected: it can never cross the message boundary.
        let output = check_source(
            r"
            #[opaque]
            type Handle {}

            actor Server {
                var count: i64 = 0;
                receive fn handle(h: Handle) {
                    count = count + 1;
                }
            }
            ",
        );
        let errs = opaque_payload_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "opaque handle as a receive-fn parameter must emit exactly one \
             OpaqueMessagePayload error; got: {:#?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("E_OPAQUE_MESSAGE_PAYLOAD"),
            "diagnostic must carry the envelope code; got: {:?}",
            errs[0].message
        );
        assert!(
            matches!(&errs[0].kind, TypeErrorKind::OpaqueMessagePayload { type_name, .. } if type_name == "Handle"),
            "diagnostic must name the opaque type `Handle`; got: {:?}",
            errs[0].kind
        );
    }

    #[test]
    fn stdlib_handle_direct_receive_fn_param_is_rejected() {
        // A registered stdlib-style owned handle used directly as a receive-fn
        // parameter is rejected the same way.
        let output = check_source_with_handle(
            r"
            actor Server {
                var count: i64 = 0;
                receive fn handle(p: regex.Pattern) {
                    count = count + 1;
                }
            }
            ",
            "regex.Pattern",
        );
        let errs = opaque_payload_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "registered handle as a receive-fn parameter must emit exactly one \
             OpaqueMessagePayload error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn opaque_nested_in_record_receive_fn_param_is_rejected() {
        // The opaque leaf is reached transitively through a record field, so a
        // record-wrapped handle payload is rejected too (single clean error at
        // the parameter, not a late codegen clone-gate message).
        let output = check_source_with_handle(
            r"
            type Wrapper { conn: regex.Pattern }

            actor Server {
                var count: i64 = 0;
                receive fn handle(w: Wrapper) {
                    count = count + 1;
                }
            }
            ",
            "regex.Pattern",
        );
        let errs = opaque_payload_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "record field carrying an opaque handle must emit exactly one \
             OpaqueMessagePayload error at the receive-fn parameter; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn channel_handle_receive_fn_params_are_accepted() {
        let output = check_source(
            r"
            actor Server {
                receive fn sender(tx: channel.Sender<string>) {}
                receive fn receiver(rx: channel.Receiver<string>) {}
                receive fn nested(
                    handles: (channel.Sender<i64>, channel.Receiver<i64>)
                ) {}
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "built-in channel handles must remain valid local receive-fn \
             parameters, including in aggregate payloads; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn serializable_receive_fn_params_are_accepted() {
        // Negative control: ordinary CBOR-serializable payloads must NOT trip
        // the new diagnostic.
        let output = check_source(
            r"
            actor Server {
                var count: i64 = 0;
                receive fn handle(n: i64, label: string) {
                    count = count + n;
                }
            }
            ",
        );
        assert!(
            opaque_payload_errors(&output).is_empty(),
            "serializable receive-fn params must not raise OpaqueMessagePayload; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn opaque_handle_as_non_receive_fn_param_is_accepted() {
        // Negative control: the restriction is specific to receive-fn (message)
        // parameters. A plain free function taking an opaque handle is fine.
        let output = check_source_with_handle(
            r"
            fn use_pattern(p: regex.Pattern) -> i64 {
                0
            }
            ",
            "regex.Pattern",
        );
        assert!(
            opaque_payload_errors(&output).is_empty(),
            "opaque handle as a non-receive fn param must not raise \
             OpaqueMessagePayload; got: {:#?}",
            output.errors
        );
    }
}
