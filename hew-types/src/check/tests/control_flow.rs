#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── Supervisor child slot index tests ────────────────────────────────────
//
// These tests verify that the checker assigns correct slot indices to
// supervisor children (static and pool), populates the side-table at
// field-access sites, and rejects unknown child names.
#[cfg(test)]
mod supervisor_child_slot_tests {
    use super::*;

    fn parse_and_check(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in supervisor_child_slot test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program)
    }

    /// A static child `child cache: Cache` must resolve to slot index 0 in
    /// the static space and produce a `ChildSlot { kind: Static, index: 0 }` entry
    /// in the output's `supervisor_child_slots` map at the field-access span.
    #[test]
    fn static_child_resolves_with_correct_slot_index() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            supervisor App {
                child cache: Cache
            }

            fn main() {
                let app = spawn App;
                let c = app.cache;
                supervisor_stop(app);
            }
        ";
        let output = parse_and_check(source);
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
        // The `supervisor_child_slots` map must contain exactly one entry for
        // the `app.cache` access. We verify kind and index without pinning the
        // exact byte offset (which would be brittle).
        let slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Cache");
        let slot = slot.expect("expected a ChildSlot for Cache child in supervisor_child_slots");
        assert_eq!(
            slot.kind,
            ChildKind::Static,
            "cache child should be in the static slot space"
        );
        assert_eq!(slot.index, 0, "first static child gets slot index 0");
    }

    /// A second static child `child log: Log` declared after `cache` must receive
    /// slot index 1 in the static space, distinct from the first child.
    #[test]
    fn second_static_child_gets_sequential_slot_index() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            actor Log {
                receive fn write() {}
            }

            supervisor App {
                child cache: Cache
                child log: Log
            }

            fn main() {
                let app = spawn App;
                let c = app.cache;
                let l = app.log;
                supervisor_stop(app);
            }
        ";
        let output = parse_and_check(source);
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
        let cache_slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Cache")
            .expect("expected ChildSlot for Cache");
        let log_slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Log")
            .expect("expected ChildSlot for Log");
        assert_eq!(cache_slot.kind, ChildKind::Static);
        assert_eq!(cache_slot.index, 0);
        assert_eq!(log_slot.kind, ChildKind::Static);
        assert_eq!(log_slot.index, 1);
    }

    /// A pool child declared with `pool worker: Worker` must resolve to
    /// slot index 0 in the *pool* space. It must not collide with a static
    /// child that also has index 0 — the two spaces are disjoint.
    #[test]
    fn pool_child_resolves_with_pool_space_slot_index() {
        let source = r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor Pool {
                strategy: simple_one_for_one,
                pool worker: Worker(count: 2)
            }

            fn main() {
                let p = spawn Pool;
                let w = p.worker;
                supervisor_stop(p);
            }
        ";
        let output = parse_and_check(source);
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
        let slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Worker")
            .expect("expected ChildSlot for Worker pool child");
        assert_eq!(
            slot.kind,
            ChildKind::Pool,
            "pool child should be in the pool slot space"
        );
        assert_eq!(slot.index, 0, "first pool child gets pool slot index 0");
    }

    /// In a supervisor with both static and pool children, their slot indices
    /// must be disjoint: the static child has `(Static, 0)` and the pool child
    /// has `(Pool, 0)`. Neither borrows an index from the other space.
    #[test]
    fn static_and_pool_indices_are_disjoint() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            actor Worker {
                receive fn ping() {}
            }

            supervisor App {
                child cache: Cache
                pool worker: Worker
            }

            fn main() {
                let app = spawn App;
                let c = app.cache;
                supervisor_stop(app);
            }
        ";
        // NOTE: A supervisor with mixed static+pool children may not pass all
        // strategy consistency checks (that's S-B). We only check that the
        // checker computes correct slot indices for the declared children.
        // Strategy-level errors are acceptable; slot-index population is not gated on them.
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let cache_slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Cache");
        let cache_slot = cache_slot.expect("expected ChildSlot for Cache static child");
        assert_eq!(cache_slot.kind, ChildKind::Static);
        assert_eq!(cache_slot.index, 0, "static child index starts at 0");
        // Pool child not accessed in this program body; its slot is not in the side-table.
        // That is correct — the side-table is keyed by access-expression span.
    }

    /// Accessing an unknown child name on a supervisor-typed value must produce
    /// a type error (`UndefinedField`). The side-table must NOT contain an entry
    /// for this access.
    #[test]
    fn unknown_child_name_produces_type_error() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            supervisor App {
                child cache: Cache
            }

            fn main() {
                let app = spawn App;
                let x = app.unknown;
                supervisor_stop(app);
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
            "expected a type error for unknown supervisor child `unknown`"
        );
        // No slot must be recorded for the bad access.
        assert!(
            output.supervisor_child_slots.is_empty(),
            "supervisor_child_slots must be empty when the child name is unknown; got: {:?}",
            output.supervisor_child_slots
        );
    }
}

// ── if-let / while-let pattern contract ──────────────────────────────────
//
// These tests verify that the checker admits the top-level `if let` /
// `while let` patterns HIR lowering supports and keeps unsupported forms
// fail-closed.
#[cfg(test)]
mod iflet_whilelet_pattern_contract {
    use super::*;

    fn check_iflet_whilelet(source: &str) -> Vec<crate::error::TypeError> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program).errors
    }

    #[test]
    fn iflet_stmt_literal_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { if let 1 = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "literal if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_struct_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"type Point { x: i64; y: i64; } fn foo(p: Point) { if let Point { x, y } = p { x + y } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "struct if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_tuple_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: (i64, i64)) { if let (a, b) = x { a + b } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "tuple if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_or_pattern_is_accepted() {
        let errors =
            check_iflet_whilelet(r"enum E { A; B; } fn foo(x: E) { if let A | B = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "or if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_wildcard_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { if let _ = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "wildcard if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_identifier_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { if let y = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "identifier if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_literal_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let 1 = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "literal while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_struct_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"enum Msg { Data { value: i64 }; Done; } fn foo(x: Msg) { while let Data { value } = x { break; } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "struct while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_tuple_pattern_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: (i64, i64)) { while let (a, b) = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "tuple while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_or_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"enum E { A; B; } fn foo(x: E) { while let A | B = x { break; } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "or while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_wildcard_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let _ = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "wildcard while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_identifier_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let y = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "identifier while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_labeled_break_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: i64) { @scan: while let y = x { break @scan; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "labeled while-let break must not emit unknown loop label; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_labeled_continue_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: i64) { @scan: while let y = x { continue @scan; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "labeled while-let continue must not emit unknown loop label; got: {errors:?}",
        );
    }

    #[test]
    fn nested_loop_can_target_outer_whilelet_label() {
        let errors = check_iflet_whilelet(
            r"fn foo(x: i64) { @scan: while let y = x { loop { break @scan; } } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "nested loops must resolve outer while-let labels; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_unknown_label_still_errors() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let y = x { break @scan; } }");
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation
                    && e.message.contains("unknown loop label `@scan`")),
            "unknown while-let labels must still error; got: {errors:?}",
        );
    }
}

// ── for-loop iterable fail-closed regressions ──────────────────────────────

mod for_loop_iterable_fail_closed {
    use super::*;

    fn check_for_over(iter_ty: Ty) -> Vec<TypeError> {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.env.define("it".to_string(), iter_ty, false);
        let for_stmt = Stmt::For {
            label: None,
            is_await: false,
            pattern: (Pattern::Identifier("x".to_string()), 0..1),
            iterable: (Expr::Identifier("it".to_string()), 7..9),
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
        };
        checker.check_stmt(&for_stmt, &(0..20));
        checker.errors
    }

    // ── catch-all: unsupported iterable type ───────────────────────────────

    #[test]
    fn unsupported_iterable_bool_emits_not_iterable_diagnostic() {
        let result = hew_parser::parse("fn main() { for x in true { } }");
        assert!(
            result.errors.is_empty(),
            "parse errors: {result_errors:?}",
            result_errors = result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::InvalidOperation && e.message.contains("not iterable")
            }),
            "expected 'not iterable' diagnostic for bool iterable; got: {errs:?}",
            errs = output.errors
        );
    }

    #[test]
    fn unsupported_iterable_does_not_produce_fresh_typevar_elem() {
        // Direct AST: `for x in it` where `it: bool`. The elem type must be
        // Ty::Error, not Ty::Var, ensuring no inference holes leak downstream.
        let errors = check_for_over(Ty::Bool);
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "expected InvalidOperation diagnostic for non-iterable; got: {errors:?}",
        );
    }

    // ── Vec with empty type args ───────────────────────────────────────────

    #[test]
    fn vec_with_empty_type_args_emits_diagnostic_not_fresh_var() {
        let errors = check_for_over(Ty::Named {
            builtin: Some(crate::BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![],
        });
        assert!(
            errors.iter().any(|e| {
                e.kind == TypeErrorKind::InvalidOperation && e.message.contains("Vec")
            }),
            "expected InvalidOperation for Vec with no type args; got: {errors:?}",
        );
    }

    // ── Stream with empty type args (plain `for`, not `for await`) ────────

    #[test]
    fn stream_with_empty_type_args_emits_diagnostic_not_fresh_var() {
        let errors = check_for_over(Ty::Named {
            builtin: None,
            name: "Stream".to_string(),
            args: vec![],
        });
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "expected InvalidOperation for Stream with no type args; got: {errors:?}",
        );
    }

    // ── valid iterables must not regress ──────────────────────────────────

    #[test]
    fn vec_with_type_arg_is_valid() {
        let errors = check_for_over(Ty::Named {
            builtin: Some(crate::BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::I64],
        });
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Vec<i64> iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn array_iterable_is_valid() {
        let errors = check_for_over(Ty::Array(Box::new(Ty::I32), 4));
        assert!(
            errors.is_empty(),
            "Array iterable must not emit errors; got: {errors:?}",
        );
    }

    #[test]
    fn range_iterable_is_valid() {
        let errors = check_for_over(Ty::Named {
            builtin: Some(crate::BuiltinType::Range),
            name: "Range".to_string(),
            args: vec![Ty::I64],
        });
        assert!(
            errors.is_empty(),
            "Range<i64> iterable must not emit errors; got: {errors:?}",
        );
    }

    #[test]
    fn user_iterator_impl_is_valid_for_loop_iterable() {
        let output = check_source(
            r"
            type Counter {
                val: i32;
            }

            impl Iterator for Counter {
                type Item = i32;
                fn next(var self) -> Option<i32> {
                    Some(self.val)
                }
            }

            fn takes_i32(x: i32) {}

            fn main() {
                for x in Counter { val: 0 } {
                    takes_i32(x);
                }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "user Iterator impl should type-check as a for-loop iterable: {:?}",
            output.errors
        );
    }

    #[test]
    fn builtin_dyn_iterator_item_binding_smoke() {
        // W3.042 S2-S4: the dyn-trait dispatch gate enforces that
        // `Iterator::next` (declared `var self` in `std/builtins.hew`)
        // is called only on a `var`-bound receiver. The parameter is
        // therefore declared `var iter` so the method dispatch picks the
        // mutable-receiver path; without `var` here the call is correctly
        // rejected with a MutabilityError naming `dyn Iterator`.
        let output = check_source(
            r"
            type Counter {
                val: i32;
            }

            impl Iterator for Counter {
                type Item = i32;
                fn next(var self) -> Option<i32> {
                    Some(self.val)
                }
            }

            fn use_iter(var iter: dyn Iterator<Item = i32>) -> Option<i32> {
                iter.next()
            }

            fn main() {
                use_iter(Counter { val: 1 });
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "builtin dyn Iterator<Item = i32> should accept Counter: {:?}",
            output.errors
        );
    }

    #[test]
    fn generator_blocks_are_deferred_to_generator_surface_slice() {
        // `gen { yield 1; yield 2; }` type-checks as Generator<i32, Unit>.
        // The checker synthesizes the yield type from yield expressions; HIR/MIR
        // lowering is still fail-closed on GenBlock but that is a compile-phase
        // boundary, not a type-check boundary.
        let result = hew_parser::parse("fn main() { let g = gen { yield 1; yield 2; }; }");
        assert!(
            result.errors.is_empty(),
            "gen {{ ... }} expression blocks should parse cleanly: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "gen block with yield expressions must type-check cleanly: {:?}",
            output.errors
        );
    }

    #[test]
    fn closure_capture_facts_are_binding_accurate_and_deduplicated() {
        let output = check_source(
            r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k + k;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "copy capture should type-check cleanly: {:?}",
            output.errors
        );

        let facts: Vec<_> = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .filter(|fact| fact.name == "k")
            .collect();
        assert_eq!(
            facts.len(),
            1,
            "capture facts should deduplicate by binding id"
        );
        assert_eq!(facts[0].ty, Ty::I32);
        assert_eq!(facts[0].mode, ClosureCaptureMode::Copy);
        assert_eq!(facts[0].mode_origin, CaptureModeOrigin::ImplicitCopy);
    }

    #[test]
    fn closure_non_copy_capture_inferred_as_borrow_accepted() {
        // A non-Copy, read-only capture without `move` type-checks
        // cleanly and records `Borrow` / `InferredBorrow` on the
        // capture fact.
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hew";
                let f = || s;
                let _ = f;
            }
            "#,
        );
        assert!(
            output.errors.is_empty(),
            "non-Copy read-only capture should type-check via inferred Borrow: {:?}",
            output.errors
        );
        let s_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "s")
            .expect("capture fact for `s` must exist");
        assert_eq!(
            s_fact.mode,
            ClosureCaptureMode::Borrow,
            "read-only non-Copy capture should infer Borrow",
        );
        assert_eq!(s_fact.mode_origin, CaptureModeOrigin::InferredBorrow);
    }

    #[test]
    fn move_closure_capture_consumes_non_copy_binding() {
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hew";
                let f = move || s;
                let again = s;
            }
            "#,
        );
        assert!(
            output
                .errors
                .iter()
                .any(|err| err.kind == TypeErrorKind::UseAfterMove),
            "use after move capture should be rejected: {:?}",
            output.errors
        );
    }

    // ── inferred Borrow / BorrowMut capture modes ────────────────────────

    #[test]
    fn closure_capture_inferred_borrow_for_println_use() {
        // Acceptance witness: `let s = "hello"; let f = |x| s; f(0)`.
        // The capture is non-Copy, read-only, no `move` keyword. The
        // checker must record `Borrow` / `InferredBorrow` AND accept
        // the program.
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hello";
                let f = |x: i32| {
                    let _ = s;
                    x
                };
                let _ = f;
            }
            "#,
        );
        assert!(
            output.errors.is_empty(),
            "inferred-borrow capture must be accepted: {:?}",
            output.errors
        );
        let s_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "s")
            .expect("expected a capture fact for `s`");
        assert_eq!(s_fact.mode, ClosureCaptureMode::Borrow);
        assert_eq!(s_fact.mode_origin, CaptureModeOrigin::InferredBorrow);
    }

    #[test]
    fn closure_capture_inferred_borrowmut_via_mutating_method_call() {
        // Mutating method (`push`) on a captured binding promotes the
        // inferred mode to `BorrowMut` / `InferredBorrowMut`. The
        // assertion REQUIRES the fact to exist — a missing fact fails
        // the test (previous version used `if let Some(..)` which
        // passed silently when the fact was absent).
        let output = check_source(
            r"
            fn main() {
                var xs: Vec<i32> = Vec::new();
                let f = || xs.push(1);
                let _ = f;
            }
            ",
        );
        let xs_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "xs")
            .expect("capture fact for `xs` must exist (mutating method call)");
        assert_eq!(
            xs_fact.mode,
            ClosureCaptureMode::BorrowMut,
            "mutating method call should infer BorrowMut: {xs_fact:?}",
        );
        assert_eq!(xs_fact.mode_origin, CaptureModeOrigin::InferredBorrowMut);
    }

    #[test]
    fn closure_capture_inferred_borrowmut_via_assignment_projection() {
        // Assignment-projection path: `xs[0] = 1` mutates the
        // root binding `xs`, so the capture promotes to BorrowMut.
        let output = check_source(
            r"
            fn main() {
                var xs: Vec<i32> = Vec::new();
                let f = || { xs[0] = 1; };
                let _ = f;
            }
            ",
        );
        let xs_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "xs")
            .expect("capture fact for `xs` must exist (assignment projection)");
        assert_eq!(xs_fact.mode, ClosureCaptureMode::BorrowMut);
        assert_eq!(xs_fact.mode_origin, CaptureModeOrigin::InferredBorrowMut);
    }

    #[test]
    fn closure_capture_explicit_move_records_move_origin() {
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hew";
                let f = move || s;
                let _ = f;
            }
            "#,
        );
        let s_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "s")
            .expect("capture fact for `s` must exist (explicit move)");
        assert_eq!(s_fact.mode, ClosureCaptureMode::Move);
        assert_eq!(s_fact.mode_origin, CaptureModeOrigin::ExplicitMove);
    }

    // ── ClosureCapturesDuplexHandle gate tests ───────────────────────────
    //
    // Verifies that the deliberate checker wall (`TypeErrorKind::ClosureCapturesDuplexHandle`)
    // fires for every known source shape that attempts to capture a lambda-actor
    // Duplex handle in a regular fn-closure. The authoritative site is
    // `check_call` in `calls.rs`; HIR's `CheckerBoundaryViolation` used to be
    // the incidental (and unintentional) rejection wall — these tests pin the
    // deliberate error.

    #[test]
    fn closure_captures_duplex_handle_direct_call_rejected() {
        // The canonical shape: `let relay = |n| { log(n); };` where `log` is a
        // lambda-actor handle. Must emit `ClosureCapturesDuplexHandle`, not
        // `CheckerBoundaryViolation`.
        let output = check_source(
            r"
            fn main() {
                let log = actor |n: i64| { println(n); };
                let relay = |n: i64| { log(n); };
                relay(1);
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::ClosureCapturesDuplexHandle { name } if name == "log")),
            "direct call-syntax Duplex capture must emit ClosureCapturesDuplexHandle; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn closure_captures_duplex_handle_rebind_rejected() {
        // Evasion shape: rebind the handle before capturing.
        let output = check_source(
            r"
            fn main() {
                let log = actor |n: i64| { println(n); };
                let log2 = log;
                let relay = |n: i64| { log2(n); };
                relay(1);
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::ClosureCapturesDuplexHandle { name } if name == "log2")),
            "rebind evasion: ClosureCapturesDuplexHandle must fire on the rebind name; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn closure_captures_duplex_handle_move_closure_rejected() {
        // Evasion shape: explicit `move` closure.
        let output = check_source(
            r"
            fn main() {
                let log = actor |n: i64| { println(n); };
                let relay = move |n: i64| { log(n); };
                relay(1);
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::ClosureCapturesDuplexHandle { name } if name == "log")),
            "move-closure evasion: ClosureCapturesDuplexHandle must fire; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn closure_captures_duplex_handle_function_param_rejected() {
        // Evasion shape: LambdaPid parameter captured in a nested closure.
        // (Lambda-actor handles are typed `LambdaPid<M, R>`; call-syntax
        // `h(n)` is the actor surface, so this routes through the capture gate.)
        let output = check_source(
            r"
            fn use_handle(h: LambdaPid<i64, ()>) {
                let relay = |n: i64| { h(n); };
                relay(1);
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::ClosureCapturesDuplexHandle { name } if name == "h")),
            "function-param LambdaPid capture: ClosureCapturesDuplexHandle must fire; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn closure_captures_duplex_handle_does_not_affect_pid_captures() {
        // Regression guard: the new Duplex gate must NOT fire for ActorRef/LocalPid
        // captures. A closure capturing a declared-actor's pid is accepted (the
        // main feature of this PR). Only Duplex (lambda-actor handles) is refused.
        let output = check_source(
            r"
            actor Counter {
                var count: i64;
                receive fn increment(n: i64) { count = count + n; }
            }
            fn main() {
                let counter = spawn Counter(count: 0);
                let relay = actor |n: i64| { counter.increment(n); };
                relay(1);
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::ClosureCapturesDuplexHandle { .. })),
            "pid (ActorRef) captures must NOT trip ClosureCapturesDuplexHandle; got: {:#?}",
            output.errors
        );
    }

    // ── NonSyncMutCaptureCrossesSuspend gate — direct body-scanner tests ─
    //
    // The end-to-end diagnostic depends on three independent conditions:
    //   1. The capture mode resolves to `BorrowMut` (already covered by
    //      the borrow/borrow-mut tests above).
    //   2. The capture's resolved type is non-`Sync` (a separate
    //      `TraitRegistry::is_sync` query — covered by the marker-trait
    //      tests in `crate::traits`).
    //   3. The closure body contains a suspend point (`await`, channel
    //      `recv`, `yield`), as observed by `scan_lambda_body`.
    //
    // The three tests below are focused tests of condition (3) for each
    // suspend source, including the new `Yield = suspend` classification.
    // They build synthetic lambda bodies directly so the body-scanner
    // is exercised without depending on user-constructible non-`Sync`
    // types.
    //
    // The end-to-end diagnostic emission path (conditions 1+2+3 plus
    // the BorrowMut mode-origin message threading) is covered by
    // `non_sync_mut_capture_crosses_suspend_end_to_end` below.

    fn lit(value: i64) -> Spanned<Expr> {
        (
            Expr::Literal(Literal::Integer {
                value,
                radix: IntRadix::Decimal,
            }),
            0..0,
        )
    }

    #[test]
    fn scan_lambda_body_marks_await_as_suspend() {
        let body: Spanned<Expr> = (Expr::Await(Box::new(lit(0))), 0..0);
        let facts = super::closure_inference::scan_lambda_body(&body);
        assert!(facts.has_suspend, "await must mark has_suspend=true");
        assert_eq!(facts.suspend_kind, "await");
    }

    #[test]
    fn scan_lambda_body_marks_channel_recv_as_suspend() {
        use hew_parser::ast::SelectArm;
        let arm = SelectArm {
            binding: (hew_parser::ast::Pattern::Identifier("v".to_string()), 0..0),
            source: lit(0),
            body: lit(0),
        };
        let body: Spanned<Expr> = (
            Expr::Select {
                arms: vec![arm],
                timeout: None,
            },
            0..0,
        );
        let facts = super::closure_inference::scan_lambda_body(&body);
        assert!(facts.has_suspend, "select recv must mark has_suspend=true");
        assert_eq!(facts.suspend_kind, "channel recv");
    }

    #[test]
    fn scan_lambda_body_marks_yield_as_suspend() {
        let body: Spanned<Expr> = (Expr::Yield(Some(Box::new(lit(1)))), 0..0);
        let facts = super::closure_inference::scan_lambda_body(&body);
        assert!(facts.has_suspend, "yield must mark has_suspend=true");
        assert_eq!(facts.suspend_kind, "yield");
    }

    #[test]
    fn non_sync_mut_capture_crosses_suspend_end_to_end() {
        // End-to-end witness: a closure that mutates a captured
        // non-`Sync` non-`Copy` binding across a suspend point must
        // emit `NonSyncMutCaptureCrossesSuspend` and surface the
        // BorrowMut mode-origin label in the human-readable message.
        //
        // The capture type is a user-defined struct wrapping a raw
        // pointer: raw pointers are the canonical non-Sync primitive,
        // and a struct wrapping one is non-Copy (default for user
        // records) and non-Sync (inherits from its non-Send field).
        // The suspend point is `yield`, which the body scanner
        // accepts uniformly with `await` and channel `recv`.
        let output = check_source(
            r#"
            extern "C" { fn make_ptr() -> *const i64; }
            type Wrap { p: *const i64, xs: Vec<i64> }
            fn host() {
                var w: Wrap = Wrap { p: unsafe { make_ptr() }, xs: Vec::new() };
                let f = || { w = w; let _g = gen { yield 0; }; };
                let _ = f;
            }
            "#,
        );
        let found = output.errors.iter().any(|err| {
            matches!(
                &err.kind,
                TypeErrorKind::NonSyncMutCaptureCrossesSuspend { capture_name, .. }
                    if capture_name == "w"
            ) && err.message.contains("inferred from a mutating use")
        });
        assert!(
            found,
            "expected NonSyncMutCaptureCrossesSuspend(w) with mode-origin label in message; got: {:#?}",
            output.errors
        );
    }

    // ── escape classification facts populated for every closure ──────────

    #[test]
    fn closure_escape_facts_populated_for_local_call() {
        let output = check_source(
            r"
            fn main() {
                let k: i32 = 1;
                let f = || k + 1;
                let _ = f();
            }
            ",
        );
        assert!(
            !output.closure_escape_facts.is_empty(),
            "every closure literal should produce one escape fact"
        );
        let any_local = output
            .closure_escape_facts
            .values()
            .any(|f| matches!(f.kind, ClosureEscapeKind::Local));
        assert!(
            any_local,
            "direct-call-only closure must classify as Local: {:?}",
            output.closure_escape_facts
        );
    }

    #[test]
    fn closure_escape_facts_default_to_escapes_for_returned_lambda() {
        let output = check_source(
            r"
            fn make() -> fn(i32) -> i32 {
                |n: i32| n + 1
            }
            ",
        );
        let any_escapes = output
            .closure_escape_facts
            .values()
            .any(|f| matches!(f.kind, ClosureEscapeKind::Escapes));
        assert!(
            any_escapes,
            "returned anonymous lambda should classify as Escapes: {:?}",
            output.closure_escape_facts
        );
    }

    // ── escape classification matrix — the six additional cases ──────────

    fn assert_escape_kind(output: &TypeCheckOutput, expected: ClosureEscapeKind, label: &str) {
        let found = output
            .closure_escape_facts
            .values()
            .any(|f| f.kind == expected);
        assert!(
            found,
            "{label}: expected at least one closure classified {expected:?}; got: {:?}",
            output.closure_escape_facts
        );
    }

    #[test]
    fn closure_escape_anonymous_fork_body_is_forked() {
        // Case: `scope { fork { || compute() }; }`.
        let output = check_source(
            r"
            fn compute() -> i32 { 1 }
            fn main() {
                scope {
                    fork {
                        || compute()
                    };
                };
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Forked,
            "anonymous fork-body lambda",
        );
    }

    #[test]
    fn closure_escape_named_then_forked_is_forked() {
        // Case: `scope { let f = ...; fork { f() } }`.
        let output = check_source(
            r"
            fn main() {
                scope {
                    let f = || 1;
                    fork {
                        f();
                    };
                };
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Forked,
            "named-then-forked lambda",
        );
    }

    #[test]
    fn closure_escape_channel_send_is_escapes() {
        // Case: closure stored-in / sent through a channel.
        let output = check_source(
            r"
            fn main() {
                let (tx, _rx) = channel::<fn() -> i32>();
                let f = || 1;
                tx.send(f);
            }
            ",
        );
        assert_escape_kind(&output, ClosureEscapeKind::Escapes, "channel-send escape");
    }

    #[test]
    fn closure_escape_higher_order_arg_is_escapes() {
        // Case: `g(|| captured)` — anonymous lambda as
        // argument to a free function.
        let output = check_source(
            r"
            fn run(g: fn() -> i32) -> i32 { g() }
            fn main() {
                let _ = run(|| 1);
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Escapes,
            "higher-order-arg lambda",
        );
    }

    #[test]
    fn closure_escape_block_tail_value_is_escapes() {
        // Case: `{ let f = ...; f }` — block-tail value.
        let output = check_source(
            r"
            fn make() -> fn() -> i32 {
                let f = || 1;
                f
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Escapes,
            "block-tail value closure",
        );
    }

    #[test]
    fn closure_escape_nested_inner_escapes_via_send() {
        // Case: inner closure escapes inside an outer
        // closure's body (the inner is sent through a channel).
        // Both inner and outer must classify Escapes — inner because
        // the channel send is a non-local use, outer because it has
        // no enumerable local-only use-site set.
        let output = check_source(
            r"
            fn main() {
                let (tx, _rx) = channel::<fn() -> i32>();
                let outer = || {
                    let inner = || 1;
                    tx.send(inner);
                };
                let _ = outer;
            }
            ",
        );
        // At least two closure literals → two facts; ensure both
        // resolve to Escapes (no Local survives).
        assert!(
            output.closure_escape_facts.len() >= 2,
            "expected facts for both outer and inner closures: {:?}",
            output.closure_escape_facts
        );
        let any_local = output
            .closure_escape_facts
            .values()
            .any(|f| matches!(f.kind, ClosureEscapeKind::Local));
        assert!(
            !any_local,
            "nested transitive escape: no closure should classify Local; got: {:?}",
            output.closure_escape_facts
        );
    }

    // ── ClosureEscapeAdvisory narrowing: PassedToHigherOrder ─────────────
    //
    // Inlining a let-bound closure passed to a higher-order function does not
    // relieve the escape — an anonymous closure in argument position still
    // hits AnonContext::PassedToHigherOrder, so the advisory would fire again.
    // The admit_local rule set therefore excludes PassedToHigherOrder.

    #[test]
    fn escape_advisory_not_emitted_for_let_bound_closure_passed_to_higher_order() {
        // `let f = |x: i64| x * 2; apply(f, 5)` — the closure escapes via
        // PassedToHigherOrder but NO advisory should fire because inlining
        // the closure at the call site provides no relief.
        let output = check_source(
            r"
            fn apply(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }
            fn main() {
                let double = |x: i64| x * 2;
                let _ = apply(double, 5);
            }
            ",
        );
        let advisory_for_higher_order = output.warnings.iter().any(|w| {
            matches!(
                &w.kind,
                TypeErrorKind::ClosureEscapeAdvisory { rule }
                    if rule.contains("PassedToHigherOrder")
            )
        });
        assert!(
            !advisory_for_higher_order,
            "no ClosureEscapeAdvisory should fire for a let-bound closure passed to a \
             higher-order function; got warnings: {:?}",
            output.warnings
        );
    }

    #[test]
    fn escape_advisory_still_emitted_for_let_bound_closure_escaped_as_block_tail() {
        // Precision check: narrowing PassedToHigherOrder must not disable the
        // advisory for other genuinely-advisable rules.  A let-bound closure
        // returned as a block tail value (EscapesViaBlockValue) still produces
        // an advisory because making it anonymous at the return site would
        // resolve to AnonContext::Returned → Escapes::Returned, not Local.
        // The suggestion ("inline at call site") IS actionable in some
        // refactors even for EscapesViaBlockValue, so that rule stays.
        let output = check_source(
            r"
            fn make() -> fn() -> i32 {
                let f = || 1;
                f
            }
            ",
        );
        let has_advisory = output
            .warnings
            .iter()
            .any(|w| matches!(&w.kind, TypeErrorKind::ClosureEscapeAdvisory { .. }));
        assert!(
            has_advisory,
            "ClosureEscapeAdvisory must still fire for a let-bound closure returned as a \
             block-tail value; got warnings: {:?}",
            output.warnings
        );
    }

    // ── Fail-closed defenses for missing facts ───────────────────────────
    //
    // These tests drive `emit_unresolved_closure_diagnostics` directly
    // with synthetic span lists and (deliberately empty) fact maps so
    // that the contract enforcer's emission paths are exercised even
    // when the checker proper always populates both maps.

    #[test]
    fn unresolved_closure_capture_mode_diagnostic_fires_on_missing_fact() {
        let span: Span = 10..20;
        let sites = vec![(span.clone(), Some("f".to_string()), 0u32)];
        let capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>> = HashMap::new();
        let mut escape_facts: HashMap<SpanKey, ClosureEscapeFact> = HashMap::new();
        // Populate escape so ONLY the capture-mode diagnostic fires.
        escape_facts.insert(
            SpanKey::from(&span),
            ClosureEscapeFact {
                kind: ClosureEscapeKind::Local,
                rule: ClosureEscapeRule::DirectCallOnly,
            },
        );
        let mut diagnostics = Vec::new();
        super::emit_unresolved_closure_diagnostics(
            &sites,
            &capture_facts,
            &escape_facts,
            &mut diagnostics,
        );
        assert!(
            diagnostics
                .iter()
                .any(|err| matches!(err.kind, TypeErrorKind::ClosureCaptureModeUnresolved { .. })),
            "expected ClosureCaptureModeUnresolved; got: {diagnostics:?}"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|err| err.kind == TypeErrorKind::ClosureEscapeKindUnresolved),
            "ClosureEscapeKindUnresolved must NOT fire when escape fact is present",
        );
    }

    #[test]
    fn unresolved_closure_escape_kind_diagnostic_fires_on_missing_fact() {
        let span: Span = 30..40;
        let sites = vec![(span.clone(), None, 0u32)];
        let mut capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>> = HashMap::new();
        // Populate capture so ONLY the escape diagnostic fires.
        capture_facts.insert(SpanKey::from(&span), Vec::new());
        let escape_facts: HashMap<SpanKey, ClosureEscapeFact> = HashMap::new();
        let mut diagnostics = Vec::new();
        super::emit_unresolved_closure_diagnostics(
            &sites,
            &capture_facts,
            &escape_facts,
            &mut diagnostics,
        );
        assert!(
            diagnostics
                .iter()
                .any(|err| err.kind == TypeErrorKind::ClosureEscapeKindUnresolved),
            "expected ClosureEscapeKindUnresolved; got: {diagnostics:?}"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|err| matches!(err.kind, TypeErrorKind::ClosureCaptureModeUnresolved { .. })),
            "ClosureCaptureModeUnresolved must NOT fire when capture fact is present",
        );
    }

    #[test]
    fn unresolved_closure_diagnostics_silent_when_contract_holds() {
        let span: Span = 50..60;
        let sites = vec![(span.clone(), Some("f".to_string()), 0u32)];
        let mut capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>> = HashMap::new();
        capture_facts.insert(SpanKey::from(&span), Vec::new());
        let mut escape_facts: HashMap<SpanKey, ClosureEscapeFact> = HashMap::new();
        escape_facts.insert(
            SpanKey::from(&span),
            ClosureEscapeFact {
                kind: ClosureEscapeKind::Local,
                rule: ClosureEscapeRule::DirectCallOnly,
            },
        );
        let mut diagnostics = Vec::new();
        super::emit_unresolved_closure_diagnostics(
            &sites,
            &capture_facts,
            &escape_facts,
            &mut diagnostics,
        );
        assert!(
            diagnostics.is_empty(),
            "no diagnostic expected when both facts are present; got: {diagnostics:?}"
        );
    }

    // ── already-errored / divergent iterables must not get extra diagnostics ─

    #[test]
    fn error_typed_iterable_does_not_emit_extra_not_iterable_diagnostic() {
        // Ty::Error propagates silently; no spurious "type is not iterable".
        let errors = check_for_over(Ty::Error);
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Ty::Error iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn never_typed_iterable_does_not_emit_not_iterable_diagnostic() {
        // Ty::Never is divergent; no spurious "type is not iterable".
        let errors = check_for_over(Ty::Never);
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Ty::Never iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }
}
