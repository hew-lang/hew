#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── Actor field mutability enforcement ──────────────────────────────────
//
// `var` fields are writable everywhere; `let` and bare fields may only be
// assigned inside `init { }` (the constructor). Handlers, methods, and
// lifecycle hooks reject the write with a field-shaped MutabilityError that
// names the declaration site and the `var` fix.

fn immutable_field_errors(output: &TypeCheckOutput, field: &str) -> Vec<String> {
    output
        .errors
        .iter()
        .filter(|e| {
            matches!(e.kind, TypeErrorKind::MutabilityError)
                && e.message
                    .contains(&format!("cannot assign to immutable field `{field}`"))
        })
        .map(|e| e.message.clone())
        .collect()
}

#[test]
fn let_field_assignment_in_receive_fn_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn bump() {
        count = count + 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "let field write in a receive fn must be rejected; got: {:#?}",
        output.errors
    );
    let err = output
        .errors
        .iter()
        .find(|e| {
            e.message
                .contains("cannot assign to immutable field `count`")
        })
        .expect("immutable field error");
    assert!(
        !err.notes.is_empty(),
        "diagnostic must carry a declaration-site note; got: {err:#?}"
    );
    assert!(
        err.suggestions.iter().any(|s| s.contains("var")),
        "diagnostic must suggest declaring the field with `var`; got: {err:#?}"
    );
}

#[test]
fn bare_field_assignment_in_receive_fn_is_rejected() {
    // A bare field declaration defaults to immutable (parser default),
    // so it follows the same rule as `let`.
    let output = check_source(
        r"
actor Counter {
    count: i64;
    receive fn bump() {
        count = count + 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "bare field write in a receive fn must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn compound_assignment_to_let_field_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn bump() {
        count += 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "compound assignment to a let field must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_assignment_in_actor_method_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn poke() {
        bump();
    }
    fn bump() {
        count = count + 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "let field write in a plain actor method must be rejected; got: {:#?}",
        output.errors
    );
}

// ── LocalPid<T> dispatch rejects plain (non-receive) actor methods ─────────
//
// `pid.method(...)` may only reach a `receive fn` handler — MIR's actor
// handler layout is built from `receive_fns` only, so a plain `fn` has no
// dispatchable shape through a pid. The checker rejects this before HIR/MIR
// ever sees the call (#2366).

#[test]
fn pid_call_to_plain_actor_method_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    var n: i64 = 0;
    fn bump() { n = n + 1; }
    receive fn get() -> i64 { n }
}
fn main() {
    let c = spawn Counter(n: 0);
    c.bump();
}
",
    );
    let undefined_method: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod))
        .collect();
    assert_eq!(
        undefined_method.len(),
        1,
        "a pid call to a plain actor method must emit exactly one UndefinedMethod; got: {:#?}",
        output.errors
    );
    let msg = &undefined_method[0].message;
    assert!(
        msg.contains("bump"),
        "message must name the method; got: {msg}"
    );
    assert!(
        msg.contains("internal actor method"),
        "message must explain the method is internal; got: {msg}"
    );
    assert!(
        msg.contains("receive fn"),
        "message must suggest `receive fn`; got: {msg}"
    );
}

#[test]
fn pid_call_to_plain_send_method_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    var n: i64 = 0;
    fn send() { n = n + 1; }
}
fn main() {
    let c = spawn Counter(n: 0);
    c.send();
}
",
    );
    let undefined_method: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod))
        .collect();
    assert_eq!(
        undefined_method.len(),
        1,
        "a plain `fn send` must not satisfy LocalPid's own `send`; got: {:#?}",
        output.errors
    );
    let msg = &undefined_method[0].message;
    assert!(
        msg.contains("no `send` handler on `Counter`"),
        "must route through the existing envelope-check diagnostic; got: {msg}"
    );
    assert!(
        msg.contains("receive fn send"),
        "message must suggest `receive fn send`; got: {msg}"
    );
}

#[test]
fn pid_call_to_receive_fn_still_dispatches() {
    let output = check_source(
        "actor Doubler { receive fn process(n: i64) -> i64 { n * 2 } }\n\
         fn main() { let d = spawn Doubler; await d.process(5); }",
    );
    assert!(
        output.errors.is_empty(),
        "pid dispatch to a receive fn must remain unaffected; got: {:#?}",
        output.errors
    );
}

#[test]
fn plain_method_self_call_from_receive_fn_does_not_gain_undefined_method() {
    // Bare-name self-dispatch (`bump()` with no receiver) never reaches the
    // `LocalPid<T>` match arm — it resolves through call-expression lookup
    // (`hew-types/src/check/calls.rs`), a distinct path. That path
    // independently emits `UndefinedFunction` for this fixture already
    // (pre-existing, unrelated to #2366 — bare self-calls to plain actor
    // methods are not yet wired to resolve as calls). This test pins the
    // regression boundary for the new pid-dispatch guard: the guard in the
    // `LocalPid<T>` arm must not additionally fire `UndefinedMethod` on a
    // fixture it was never meant to see.
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn poke() {
        bump();
    }
    fn bump() {
        count = count + 1;
    }
}
",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod)),
        "bare self-dispatch never reaches the LocalPid<T> arm, so the new \
         pid-dispatch guard must not fire UndefinedMethod here; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_assignment_in_on_stop_hook_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn poke() {}
    #[on(stop)]
    fn drain() {
        count = 0;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "let field write in a lifecycle hook must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_assignment_in_init_is_accepted() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    init(initial: i64) {
        count = initial;
    }
    receive fn total() -> i64 {
        count
    }
}
",
    );
    assert!(
        immutable_field_errors(&output, "count").is_empty(),
        "init must be allowed to assign let fields; got: {:#?}",
        output.errors
    );
}

#[test]
fn var_field_assignment_in_receive_fn_is_accepted() {
    let output = check_source(
        r"
actor Counter {
    var count: i64;
    receive fn bump() {
        count = count + 1;
    }
}
",
    );
    assert!(
        immutable_field_errors(&output, "count").is_empty(),
        "var field write in a receive fn must be accepted; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_read_in_receive_fn_is_accepted() {
    let output = check_source(
        r"
actor Counter {
    let step: i64;
    var count: i64;
    receive fn bump() {
        count = count + step;
    }
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::MutabilityError)),
        "reading a let field in a receive fn must be accepted; got: {:#?}",
        output.errors
    );
}

// ── #[every(duration)] periodic receive handlers ─────────────────────────────
//
// Validation behaviours for the periodic-handler attribute: interval floor,
// handler shape (params / return / generator), attribute arity, and the
// supervisor-child rejection (periodic timers are armed by spawn-site
// codegen, which the supervisor child-spec spawn path never reaches).
mod every_attribute {
    use super::*;

    fn invalid_op_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output.errors.iter().any(|e| {
            // Most `#[every]` diagnostics use the generic `InvalidOperation`
            // kind; the two supervisor-periodic-child cases now carry the
            // distinct `SupervisorError { PeriodicChild }` kind (#2377). Accept
            // either so this fragment helper keeps checking the diagnostic body
            // regardless of which of the two related kinds emitted it.
            matches!(
                e.kind,
                TypeErrorKind::InvalidOperation
                    | TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::PeriodicChild,
                    }
            ) && e.message.contains(fragment)
        })
    }

    #[test]
    fn valid_millisecond_interval_accepted() {
        let output = check_source(
            "actor Ticker { var count: i64 = 0; #[every(50ms)] receive fn tick() { count += 1; } } fn main() {}",
        );
        assert!(
            output.errors.is_empty(),
            "a 50ms periodic handler is the spec'd happy path; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn sub_millisecond_interval_rejected() {
        let output =
            check_source("actor Ticker { #[every(500us)] receive fn tick() {} } fn main() {}");
        assert!(
            invalid_op_contains(&output, "minimum periodic interval is 1ms"),
            "500us floors to a 0ms timer interval and must be rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn exactly_one_millisecond_accepted() {
        let output =
            check_source("actor Ticker { #[every(1ms)] receive fn tick() {} } fn main() {}");
        assert!(
            output.errors.is_empty(),
            "1ms is the minimum valid interval and must be accepted; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn handler_with_params_rejected() {
        let output =
            check_source("actor Ticker { #[every(10ms)] receive fn tick(n: i64) {} } fn main() {}");
        assert!(
            invalid_op_contains(&output, "must not have parameters"),
            "periodic handlers receive no payload; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn handler_with_return_type_rejected() {
        let output = check_source(
            "actor Ticker { #[every(10ms)] receive fn tick() -> i64 { 1 } } fn main() {}",
        );
        assert!(
            invalid_op_contains(&output, "must not have a return type"),
            "periodic handlers are fire-and-forget; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn generator_handler_rejected() {
        let output = check_source(
            "actor Ticker { #[every(10ms)] receive gen fn ticks() -> i64 { yield 1; } } fn main() {}",
        );
        assert!(
            invalid_op_contains(&output, "must not be a generator"),
            "generator receive fns have no dispatchable body for a tick; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn multiple_every_attributes_rejected() {
        let output = check_source(
            "actor Ticker { #[every(10ms)] #[every(20ms)] receive fn tick() {} } fn main() {}",
        );
        assert!(
            invalid_op_contains(&output, "multiple #[every] attributes"),
            "only one #[every] per handler; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn non_duration_argument_rejected() {
        let output = check_source("actor Ticker { #[every(5)] receive fn tick() {} } fn main() {}");
        assert!(
            invalid_op_contains(&output, "must be a duration literal"),
            "a bare integer is not a duration; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn supervisor_child_with_periodic_handler_rejected() {
        let output = check_source(
            r"
            actor Heartbeat {
                #[every(100ms)]
                receive fn beat() {}
            }

            supervisor App {
                child hb: Heartbeat;
            }

            fn main() {}
            ",
        );
        assert!(
            invalid_op_contains(&output, "E_SUPERVISOR_PERIODIC_CHILD"),
            "supervisor child spawns never reach spawn-site timer arming; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn supervisor_child_without_periodic_handler_accepted() {
        let output = check_source(
            r"
            actor Worker {
                receive fn work() {}
            }

            supervisor App {
                child w: Worker;
            }

            fn main() {}
            ",
        );
        assert!(
            !invalid_op_contains(&output, "E_SUPERVISOR_PERIODIC_CHILD"),
            "the accept twin: a message-driven child must not trip the periodic check; got: {:#?}",
            output.errors
        );
    }

    // -----------------------------------------------------------------------
    // t2-let-destructure Stage 1: refutable-let gate + irrefutable acceptance
    // -----------------------------------------------------------------------

    #[test]
    fn let_refutable_enum_constructor_emits_exactly_one_error() {
        // `let Some(x) = opt;` — refutable enum variant in let position.
        // Must emit exactly one RefutableLetPattern error; binders must still exist
        // for error-recovery (no UnresolvedSymbol cascade on subsequent uses of `x`).
        let output = check_source("fn main() -> i64 { let opt = Some(5); let Some(x) = opt; x }");
        let refutable_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert_eq!(
            refutable_errors.len(),
            1,
            "expected exactly one RefutableLetPattern error, got: {:#?}",
            output.errors
        );
        // No cascade: must not also emit UnresolvedVariable for `x`.
        let unresolved: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .collect();
        assert!(
            unresolved.is_empty(),
            "refutable let gate must not cascade into UndefinedVariable; got: {unresolved:#?}"
        );
        // The error message must mention `if let` or `match`.
        let err_msg = &refutable_errors[0].message;
        assert!(
            err_msg.contains("if let") || err_msg.contains("match"),
            "refutable-let error message must mention `if let` or `match`; got: {err_msg}"
        );
    }

    #[test]
    fn let_refutable_literal_emits_exactly_one_error() {
        // `let 5 = n;` — literal pattern in let position.
        let output = check_source("fn main() -> i64 { let n = 5; let 5 = n; n }");
        let refutable_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert_eq!(
            refutable_errors.len(),
            1,
            "literal let pattern must emit exactly one RefutableLetPattern error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn let_irrefutable_record_no_error() {
        // `let Point { x, y } = p;` — irrefutable product type in let position.
        // Must emit zero checker errors; binders x and y must resolve.
        let output = check_source(
            "type Point { x: i64; y: i64; }
             fn main() -> i64 { let p = Point { x: 1, y: 2 }; let Point { x, y } = p; x + y }",
        );
        assert!(
            output.errors.is_empty(),
            "irrefutable record let must emit zero checker errors; got: {:#?}",
            output.errors
        );
    }

    // -----------------------------------------------------------------------
    // let-else: refutable pattern admitted with a divergent `else` clause
    // -----------------------------------------------------------------------

    #[test]
    fn let_else_with_diverging_else_no_refutable_error() {
        // `let Ok(n) = r else { return … };` — a refutable Ok-pattern is
        // admitted when an else clause is present and the else diverges.
        let output = check_source(
            "fn f(r: Result<i64, string>) -> Result<i64, string> {              let Ok(n) = r else { return Err(\"bad\") }; Ok(n) }",
        );
        let refutable: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert!(
            refutable.is_empty(),
            "let-else must NOT emit RefutableLetPattern; got: {:#?}",
            output.errors
        );
        let diverge: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::LetElseDoesNotDiverge))
            .collect();
        assert!(
            diverge.is_empty(),
            "a diverging else must not trip LetElseDoesNotDiverge; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn let_else_ok_binding_escapes_to_enclosing_scope() {
        // The Ok-path binder `n` must be visible in the rest of the enclosing
        // block AFTER the let-else statement. If it did not escape, the use of
        // `n` would cascade into UndefinedVariable.
        let output = check_source(
            "fn f(r: Result<i64, string>) -> Result<i64, string> {              let Ok(n) = r else { return Err(\"bad\") }; Ok(n + 1) }",
        );
        let unresolved: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .collect();
        assert!(
            unresolved.is_empty(),
            "let-else Ok binder must escape to the enclosing scope (no UndefinedVariable); \
             got: {unresolved:#?}"
        );
        assert!(
            output.errors.is_empty(),
            "a well-formed let-else must type-check cleanly; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn let_else_non_diverging_else_emits_error() {
        // `let Ok(n) = r else { 0 };` — the else block produces a value instead
        // of diverging. It must emit exactly one LetElseDoesNotDiverge error.
        let output =
            check_source("fn f(r: Result<i64, string>) -> i64 { let Ok(n) = r else { 0 }; n }");
        let diverge: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::LetElseDoesNotDiverge))
            .collect();
        assert_eq!(
            diverge.len(),
            1,
            "a non-diverging let-else else must emit exactly one LetElseDoesNotDiverge; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn let_else_else_does_not_see_ok_binder() {
        // The else block runs on the FAILURE path, where the Ok binder is not
        // bound. Referencing `n` inside the else must be an UndefinedVariable.
        let output = check_source(
            "fn f(r: Result<i64, string>) -> i64 { let Ok(n) = r else { return n }; n }",
        );
        let unresolved: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .collect();
        assert!(
            !unresolved.is_empty(),
            "the else block must not see the Ok binder `n`; expected UndefinedVariable, \
             got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn refutable_let_no_else_message_suggests_else_clause() {
        // The reworded no-else diagnostic must point users at the `else` clause
        // (now that let-else exists), while still mentioning if-let / match.
        let output = check_source("fn main() -> i64 { let opt = Some(5); let Some(x) = opt; x }");
        let refutable: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert_eq!(
            refutable.len(),
            1,
            "expected one RefutableLetPattern; got: {:#?}",
            output.errors
        );
        let msg = &refutable[0].message;
        assert!(
            msg.contains("else"),
            "the reworded refutable-let message must suggest an `else` clause; got: {msg}"
        );
    }

    #[test]
    fn let_else_unit_variant_admitted_and_records_resolution() {
        // `let E::A = e else { return … };` over a UNIT variant (no payload) is
        // a valid refutable pattern. It must type-check cleanly: no
        // RefutableLetPattern, no LetElseDoesNotDiverge, and — critically — the
        // pattern resolution must be recorded so HIR lowering does not cascade
        // into "pattern has no resolution" / verifier leakage.
        let output = check_source(
            "enum E { A; B(i64); }
             fn make_e(g: bool) -> E { if g { E::A } else { E::B(3) } }
             fn f(g: bool) -> Result<i64, string> { let E::A = make_e(g) else { return Err(\"x\") }; Ok(1) }",
        );
        assert!(
            output.errors.is_empty(),
            "a unit-variant let-else with a diverging else must type-check cleanly; got: {:#?}",
            output.errors
        );
        // A unit variant binds nothing — it must NOT introduce a phantom binding
        // that warns "unused variable `E::A`".
        let unused_binding_warns: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.message.contains("E::A"))
            .collect();
        assert!(
            unused_binding_warns.is_empty(),
            "a unit-variant pattern binds nothing and must not warn about `E::A`; got: {unused_binding_warns:#?}"
        );
    }

    #[test]
    fn let_else_builtin_none_unit_variant_admitted() {
        // The common `let None = opt else { … };` idiom (built-in Option unit
        // variant) rides the same refutable-unit-variant path and type-checks
        // cleanly with a diverging else.
        let output = check_source(
            "fn f(r: Option<i64>) -> Result<i64, string> { let None = r else { return Err(\"some\") }; Ok(0) }",
        );
        assert!(
            output.errors.is_empty(),
            "a built-in None unit-variant let-else must type-check cleanly; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn refutable_unit_variant_let_no_else_rejected() {
        // A unit variant in a PLAIN `let` (no else) is refutable and must be
        // rejected with RefutableLetPattern — the same gate as `let Some(x) =`.
        let output = check_source("fn f(r: Option<i64>) -> i64 { let None = r; 0 }");
        let refutable: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert_eq!(
            refutable.len(),
            1,
            "a unit-variant plain let must emit exactly one RefutableLetPattern; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn uppercase_plain_binding_is_not_a_unit_variant() {
        // Regression: an uppercase plain identifier that does NOT resolve to a
        // unit variant of the value type (e.g. `let INF = 999999;`) is a fresh
        // binding, not a refutable pattern. The detection is by RESOLUTION, not
        // by case — so it must bind cleanly (no RefutableLetPattern, no
        // "undefined variable") and be usable afterwards.
        let output = check_source("fn f() -> i64 { let INF = 999999; let Foo = 5; INF + Foo }");
        assert!(
            output.errors.is_empty(),
            "an uppercase plain binding that resolves to no variant must bind cleanly; got: {:#?}",
            output.errors
        );
        let refutable: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert!(
            refutable.is_empty(),
            "uppercase plain bindings must not be treated as refutable patterns; got: {refutable:#?}"
        );
    }

    #[test]
    fn bare_none_over_non_option_is_variant_pattern_not_binding() {
        // Regression for the let-side/use-side split: a bare `None` is ALWAYS a
        // variant pattern, independent of the value type, because the use-side
        // resolves a bare `None` to `Option::None` unconditionally. With the old
        // value-type-based detection, `let None = 5;` bound `None` as a local
        // (None is not a variant of `i64`), then any use resolved to the builtin
        // — a half-built binding that surfaced as "unused variable `None`" plus a
        // "cannot infer type" at the use. The aligned detection treats `None` as
        // a refutable variant pattern, so this is a SINGLE clean diagnostic: a
        // refutable-pattern rejection, with NO `None` binding and NO unused-var.
        let output = check_source("fn f() { let None = 5; }");
        let refutable: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert_eq!(
            refutable.len(),
            1,
            "`let None = 5;` must be a single refutable-variant rejection; got errors: {:#?}",
            output.errors
        );
        // The split symptom: `None` must NOT be bound as a local, so no
        // unused-variable warning for `None` may appear.
        let unused_none = output.warnings.iter().any(|w| {
            matches!(w.kind, TypeErrorKind::UnusedVariable) && w.message.contains("`None`")
        });
        assert!(
            !unused_none,
            "`None` must not be a half-built binding (no unused-variable warning); warnings: {:#?}",
            output.warnings
        );
    }

    #[test]
    fn bare_none_over_generic_is_variant_pattern_not_binding() {
        // The same alignment must hold over a generic type parameter: `let None
        // = x;` where `x: T` is a variant pattern, not a binding. The old
        // detection bound `None` (T is not `Option`), leaving an unused-variable
        // warning while the use-side would resolve `None` to the builtin —
        // inconsistent. The aligned detection routes it to the refutability gate.
        let output = check_source("fn g<T>(x: T) { let None = x; }");
        let refutable: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::RefutableLetPattern { .. }))
            .collect();
        assert_eq!(
            refutable.len(),
            1,
            "`let None = x;` in a generic fn must be a single refutable-variant rejection; \
             got errors: {:#?}",
            output.errors
        );
        let unused_none = output.warnings.iter().any(|w| {
            matches!(w.kind, TypeErrorKind::UnusedVariable) && w.message.contains("`None`")
        });
        assert!(
            !unused_none,
            "`None` over a generic must not be a half-built binding; warnings: {:#?}",
            output.warnings
        );
    }

    #[test]
    fn let_irrefutable_struct_record_keyword_no_error() {
        // `record`-keyword product type is also irrefutable.
        let output = check_source(
            "record Pair { a: i64, b: i64 }
             fn main() -> i64 { let p = Pair { a: 3, b: 4 }; let Pair { a, b } = p; a + b }",
        );
        assert!(
            output.errors.is_empty(),
            "irrefutable `record`-keyword destructure must emit zero checker errors; got: {:#?}",
            output.errors
        );
    }

    // -----------------------------------------------------------------------
    // Record shorthand pattern: `let { a, b } = rec` (no type name)
    // -----------------------------------------------------------------------

    #[test]
    fn let_record_shorthand_binds_fields_no_error() {
        // `let { x, y } = p` — shorthand with no type name must bind both
        // fields with zero checker errors.
        let output = check_source(
            "type Point { x: i64; y: i64; }
             fn main() -> i64 { let p = Point { x: 1, y: 2 }; let { x, y } = p; x + y }",
        );
        assert!(
            output.errors.is_empty(),
            "shorthand record let must emit zero checker errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn let_record_shorthand_unknown_field_emits_undefined_field_error() {
        // `let { x, z } = p` where `z` does not exist on `Point` must emit
        // exactly one UndefinedField error (not cascade into UnresolvedSymbol).
        let output = check_source(
            "type Point { x: i64; y: i64; }
             fn main() -> i64 { let p = Point { x: 1, y: 2 }; let { x, z } = p; x }",
        );
        let undef_field: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedField))
            .collect();
        assert_eq!(
            undef_field.len(),
            1,
            "unknown field in shorthand must emit exactly one UndefinedField; got: {:#?}",
            output.errors
        );
    }

    // ── Opaque handle direct-construction guard ──────────────────────────────
    //
    // An `#[opaque]` type is a pointer-shaped runtime handle with no user-
    // visible fields. Direct construction with `Handle {}` is allowed ONLY
    // within the DECLARING module (the producer). Cross-module construction
    // must be rejected with OpaqueDirectConstruct before reaching MIR, where
    // it would otherwise trip a misleading E_NOT_YET_IMPLEMENTED note.

    #[test]
    fn opaque_handle_same_module_construct_is_allowed() {
        // The module that DECLARES an `#[opaque]` type is the producer.
        // Its `#[extern_symbol]` impl stubs must be able to write `Handle {}`
        // as the stub body — the checker allows this. Only importers are
        // barred from direct construction.
        let output = check_source(
            r"
            #[opaque]
            type Handle {}

            fn make_handle() -> Handle {
                Handle {}
            }

            fn main() -> i64 {
                0
            }
            ",
        );
        let opaque_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueDirectConstruct { .. }))
            .collect();
        assert!(
            opaque_errors.is_empty(),
            "same-module construction of an #[opaque] type must be allowed (producer \
             semantics); got unexpected errors: {:#?}",
            output.errors
        );
    }

    #[test]
    fn opaque_handle_cross_module_construct_emits_opaque_direct_construct_error() {
        // Cross-module construction of an `#[opaque]` type must produce exactly
        // one `OpaqueDirectConstruct` error with the correct (qualified) type name.
        // The opaque type is declared in module `handles`; the root imports it
        // and tries to construct it directly — this must be rejected.
        //
        // Pattern: parse both sources, patch the root's ImportDecl's
        // `resolved_items` to the handles module's items, then check_program.
        // This mirrors the real check pipeline without needing disk module resolution.
        let handles_src = hew_parser::parse(
            r"
            #[opaque]
            pub type Handle {}
            ",
        );
        assert!(handles_src.errors.is_empty());
        let mut root = hew_parser::parse(
            r"
            import handles::{ Handle };
            fn main() -> i64 {
                let _h: Handle = Handle {};
                0
            }
            ",
        );
        assert!(root.errors.is_empty());

        // Patch the import to have its module items pre-resolved (no disk lookup).
        let import_decl = root
            .program
            .items
            .iter_mut()
            .find_map(|(item, _)| match item {
                Item::Import(imp) => Some(imp),
                _ => None,
            })
            .expect("root should have an import");
        import_decl.resolved_items = Some(handles_src.program.items.clone());

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&root.program);

        let opaque_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueDirectConstruct { .. }))
            .collect();
        assert_eq!(
            opaque_errors.len(),
            1,
            "cross-module construction of an opaque type must emit exactly one \
             OpaqueDirectConstruct error; got: {:#?}",
            output.errors
        );
        // The error message must carry the E_OPAQUE_CONSTRUCT envelope code.
        assert!(
            opaque_errors[0].message.contains("E_OPAQUE_CONSTRUCT"),
            "OpaqueDirectConstruct error message must contain E_OPAQUE_CONSTRUCT; \
             got: {:?}",
            opaque_errors[0].message
        );
    }

    #[test]
    fn opaque_handle_cross_module_construct_no_downstream_mir_errors() {
        // The checker must short-circuit cleanly on cross-module opaque construction
        // — no UndefinedVariable or UndefinedType cascade from the rejected construct.
        let handles_src = hew_parser::parse(
            r"
            #[opaque]
            pub type Handle {}
            ",
        );
        assert!(handles_src.errors.is_empty());
        let mut root = hew_parser::parse(
            r"
            import handles::{ Handle };
            fn main() -> i64 {
                let _h: Handle = Handle {};
                0
            }
            ",
        );
        assert!(root.errors.is_empty());
        let import_decl = root
            .program
            .items
            .iter_mut()
            .find_map(|(item, _)| match item {
                Item::Import(imp) => Some(imp),
                _ => None,
            })
            .expect("root should have an import");
        import_decl.resolved_items = Some(handles_src.program.items.clone());

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&root.program);

        // Must have the OpaqueDirectConstruct error.
        let has_opaque_error = output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::OpaqueDirectConstruct { .. }));
        assert!(
            has_opaque_error,
            "expected OpaqueDirectConstruct on cross-module construction; got: {:#?}",
            output.errors
        );
        // Must not cascade into UndefinedVariable or UndefinedType.
        let cascade_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(
                    e.kind,
                    TypeErrorKind::UndefinedVariable | TypeErrorKind::UndefinedType
                )
            })
            .collect();
        assert!(
            cascade_errors.is_empty(),
            "OpaqueDirectConstruct must not cascade into UndefinedVariable/UndefinedType; \
             got cascade: {cascade_errors:#?}",
        );
    }

    #[test]
    fn non_opaque_struct_construct_is_not_affected() {
        // A plain struct (not `#[opaque]`) must still construct without error.
        let output = check_source(
            r"
            type Point { x: i64; y: i64; }

            fn main() -> i64 {
                let p = Point { x: 1, y: 2 };
                p.x
            }
            ",
        );
        let opaque_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueDirectConstruct { .. }))
            .collect();
        assert!(
            opaque_errors.is_empty(),
            "non-opaque struct construction must not produce OpaqueDirectConstruct; \
             got: {opaque_errors:#?}",
        );
    }

    // ── Opaque handle as a receive-fn message parameter (#2511) ──────────────
    //
    // Actor message payloads are CBOR-serialized for mailbox dispatch. An
    // `#[opaque]` handle has no record layout, so it cannot cross the actor
    // message boundary. The checker must reject an opaque-typed receive-fn
    // parameter with a clean `OpaqueMessagePayload` diagnostic rather than
    // letting it reach codegen and surface as a raw wire-serialize failure.

    #[test]
    fn opaque_receive_fn_param_emits_opaque_message_payload_error() {
        let output = check_source(
            r"
            #[opaque]
            type Handle {}

            actor Server {
                receive fn handle(h: Handle) {}
            }

            fn main() {}
            ",
        );
        let payload_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueMessagePayload { .. }))
            .collect();
        assert_eq!(
            payload_errors.len(),
            1,
            "an opaque-typed receive-fn parameter must emit exactly one \
             OpaqueMessagePayload error; got: {:#?}",
            output.errors
        );
        match &payload_errors[0].kind {
            TypeErrorKind::OpaqueMessagePayload {
                param_name,
                opaque_name,
            } => {
                assert_eq!(param_name, "h");
                assert_eq!(opaque_name, "Handle");
            }
            other => panic!("unexpected error kind: {other:?}"),
        }
        assert!(
            payload_errors[0]
                .message
                .contains("E_OPAQUE_MESSAGE_PAYLOAD"),
            "OpaqueMessagePayload message must carry the envelope code; got: {:?}",
            payload_errors[0].message
        );
    }

    #[test]
    fn opaque_receive_fn_param_nested_in_vec_is_rejected() {
        // The restriction is transitive: a `Vec<Handle>` payload also can't be
        // serialized, so the opaque leaf must still be detected.
        let output = check_source(
            r"
            #[opaque]
            type Handle {}

            actor Server {
                receive fn handle(hs: Vec<Handle>) {}
            }

            fn main() {}
            ",
        );
        let payload_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueMessagePayload { .. }))
            .collect();
        assert_eq!(
            payload_errors.len(),
            1,
            "an opaque handle nested in a Vec receive-fn parameter must be \
             rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn non_opaque_receive_fn_params_are_not_affected() {
        // Plain serializable payloads (primitives, records) must still pass.
        let output = check_source(
            r"
            type Point { x: i64; y: i64; }

            actor Server {
                receive fn handle(n: i64, s: string, p: Point) {}
            }

            fn main() {}
            ",
        );
        let payload_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(&e.kind, TypeErrorKind::OpaqueMessagePayload { .. }))
            .collect();
        assert!(
            payload_errors.is_empty(),
            "serializable receive-fn parameters must not produce \
             OpaqueMessagePayload; got: {payload_errors:#?}",
        );
    }

    #[test]
    fn cross_actor_msg_id_collision_is_rejected() {
        // `Alpha::h69862` and `Beta::h103299` are a real SipHash-1-3 low-32-bit
        // collision (pinned in `actor_protocol::tests`). Two DISTINCT actors
        // sharing the 32-bit cross-node wire discriminant must be refused at
        // compile time (defense-in-depth: the runtime keys its codec registry by
        // (actor-type, msg_id) and routes correctly in-process, but the wire
        // discriminant itself is ambiguous for relays / mixed-binary peers).
        let output = check_source(
            r"
            actor Alpha {
                receive fn h69862() {}
            }
            actor Beta {
                receive fn h103299() {}
            }

            fn main() {}
            ",
        );

        let collision = output.errors.iter().find(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::CrossActorProtocolCollision { msg_id, .. } if *msg_id == 0xc0f6_cc98
            )
        });
        assert!(
            collision.is_some(),
            "two distinct actors with msg_id-colliding handlers must be rejected \
             with CrossActorProtocolCollision; got: {:#?}",
            output.errors
        );
        let err = collision.unwrap();
        if let TypeErrorKind::CrossActorProtocolCollision {
            actor_a,
            handler_a,
            actor_b,
            handler_b,
            ..
        } = &err.kind
        {
            // Both actors and both handlers are named (order: first-seen, then
            // the later offender).
            let actors = [actor_a.as_str(), actor_b.as_str()];
            assert!(actors.contains(&"Alpha") && actors.contains(&"Beta"));
            let handlers = [handler_a.as_str(), handler_b.as_str()];
            assert!(handlers.contains(&"h69862") && handlers.contains(&"h103299"));
        }
    }

    #[test]
    fn distinct_actor_non_colliding_handlers_are_clean() {
        // Negative control: two actors whose handler names do NOT collide on the
        // 32-bit msg_id must produce no cross-actor collision diagnostic.
        let output = check_source(
            r"
            actor Alpha {
                receive fn ping() {}
            }
            actor Beta {
                receive fn pong() {}
            }

            fn main() {}
            ",
        );

        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::CrossActorProtocolCollision { .. })),
            "non-colliding actor handlers must not produce a cross-actor collision; \
             got: {:#?}",
            output.errors
        );
    }

    /// Build a program with two distinct non-root modules, each containing one
    /// actor, and a root with just `fn main()`. Used to prove the cross-actor
    /// collision checker covers actors in separate modules (not just root actors).
    fn check_two_module_actors(
        alpha_src: &str,
        alpha_path: Vec<String>,
        beta_src: &str,
        beta_path: Vec<String>,
    ) -> TypeCheckOutput {
        let alpha_parsed = hew_parser::parse(alpha_src);
        assert!(
            alpha_parsed.errors.is_empty(),
            "alpha module must parse cleanly, got: {:#?}",
            alpha_parsed.errors
        );
        let beta_parsed = hew_parser::parse(beta_src);
        assert!(
            beta_parsed.errors.is_empty(),
            "beta module must parse cleanly, got: {:#?}",
            beta_parsed.errors
        );
        let root_parsed = hew_parser::parse("fn main() {}");
        assert!(root_parsed.errors.is_empty());

        let root_id = ModuleId::root();
        let alpha_id = ModuleId::new(alpha_path);
        let beta_id = ModuleId::new(beta_path);

        let alpha_module = Module {
            id: alpha_id.clone(),
            items: alpha_parsed.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let beta_module = Module {
            id: beta_id.clone(),
            items: beta_parsed.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(alpha_module).unwrap();
        mg.add_module(beta_module).unwrap();
        // topo: both non-root modules before root
        mg.topo_order = vec![alpha_id, beta_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: root_parsed.program.items,
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program)
    }

    #[test]
    fn single_nested_module_actor_descriptor_is_published() {
        // Regression probe: an actor in a nested module (path ["a","b"]) must
        // have its protocol descriptor published. The descriptor is keyed by
        // the module-short form "b.Alpha" (matching `current_module_short()` =
        // leaf segment used during fn_sigs registration). If collect_program_actors
        // or build_actor_protocol_descriptors fails to resolve the fn_sigs key,
        // the descriptor is silently absent.
        let parsed = hew_parser::parse("actor Alpha { receive fn increment() {} }");
        assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
        let root_id = ModuleId::root();
        let mod_id = ModuleId::new(vec!["a".to_string(), "b".to_string()]);
        let module = Module {
            id: mod_id.clone(),
            items: parsed.program.items,
            imports: vec![],
            source_paths: vec![],
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
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "single module actor must typecheck cleanly; got: {:#?}",
            output.errors
        );
        // The actor_protocol_descriptors must contain the module-short key "b.Alpha".
        // (Full-path identity "a.b.Alpha" is used only for the cross-actor collision
        // check; the descriptor map stays keyed by the leaf-qualified form for
        // downstream compatibility with HIR lowering and coercion checking.)
        let descriptor = output.actor_protocol_descriptors.get("b.Alpha");
        assert!(
            descriptor.is_some(),
            "actor in module [\"a\",\"b\"] must be published under identity \"b.Alpha\"; \
             known keys: {:?}",
            output.actor_protocol_descriptors.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn two_module_actors_both_get_descriptors() {
        // When two separate modules each contain an actor, BOTH descriptors
        // must be published. The descriptor map uses the module-short-qualified
        // key (leaf segment) for downstream compatibility. This is a structural
        // prerequisite for cross-actor collision detection — if either actor's
        // descriptor is skipped, the collision is missed.
        let output = check_two_module_actors(
            "actor Alpha { receive fn ping() {} }",
            vec!["a".to_string(), "b".to_string()],
            "actor Beta { receive fn pong() {} }",
            vec!["c".to_string(), "d".to_string()],
        );
        assert!(
            output.errors.is_empty(),
            "two non-colliding module actors must typecheck cleanly; got: {:#?}",
            output.errors
        );
        let all_keys: Vec<_> = output.actor_protocol_descriptors.keys().collect();
        assert!(
            output.actor_protocol_descriptors.contains_key("b.Alpha"),
            "b.Alpha descriptor must be present; known keys: {all_keys:?}"
        );
        assert!(
            output.actor_protocol_descriptors.contains_key("d.Beta"),
            "d.Beta descriptor must be present; known keys: {all_keys:?}"
        );
    }

    #[test]
    fn nested_module_cross_actor_collision_is_caught() {
        // `b.Alpha::h804959` and `d.Beta::h3600` are a real SipHash-1-3 low-32-bit
        // collision under the module-short-qualified identity form (module path
        // `["a","b"]` → short `"b"`, path `["c","d"]` → short `"d"`).
        // Pair is pinned in `actor_protocol::tests::module_qualified_cross_actor_handler_names_share_msg_id`.
        // The whole-program collision checker must detect this even though each
        // actor lives in a separate nested module — it must not silently skip
        // actors from the module graph.
        let output = check_two_module_actors(
            "actor Alpha { receive fn h804959() {} }",
            vec!["a".to_string(), "b".to_string()],
            "actor Beta { receive fn h3600() {} }",
            vec!["c".to_string(), "d".to_string()],
        );

        let collision = output.errors.iter().find(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::CrossActorProtocolCollision { msg_id, .. } if *msg_id == 0x3e59_ee08
            )
        });
        assert!(
            collision.is_some(),
            "two distinct module-nested actors with msg_id-colliding handlers \
             must be rejected with CrossActorProtocolCollision (0x3e59_ee08); \
             got: {:#?}",
            output.errors
        );
        if let TypeErrorKind::CrossActorProtocolCollision {
            actor_a,
            handler_a,
            actor_b,
            handler_b,
            ..
        } = &collision.unwrap().kind
        {
            let actors = [actor_a.as_str(), actor_b.as_str()];
            // The identity in the diagnostic is the full-path form ("a.b.Alpha" /
            // "c.d.Beta") because cross_actor_seen uses collision_identity.
            assert!(
                actors.iter().any(|a| a.ends_with("Alpha"))
                    && actors.iter().any(|a| a.ends_with("Beta")),
                "collision diagnostic must name both actors (qualified); got: {actor_a}, {actor_b}"
            );
            let handlers = [handler_a.as_str(), handler_b.as_str()];
            assert!(
                handlers.contains(&"h804959") && handlers.contains(&"h3600"),
                "collision diagnostic must name both handlers; got: {handler_a}, {handler_b}"
            );
        }
    }
}

/// Tests for local-shadows-global reserved-name resolution.
///
/// Four invariants:
///   1. A user payload variant shadows a builtin unit variant of the same name.
///   2. A user unit variant deterministically shadows a builtin unit variant.
///   3. Qualified builtin variant still resolves after the fix (no regression).
///   4. Two user enums with the same variant name remain ambiguous (user-vs-user
///      behaviour is unchanged by this fix).
#[cfg(test)]
mod reserved_names {
    use super::*;

    /// User `enum AppError { NotFound(string); }` — bare `NotFound("msg")`
    /// must resolve to `AppError::NotFound`, NOT to the builtin
    /// `LookupError::NotFound` unit variant.
    ///
    /// In the no-search-paths test context, `register_builtins` runs before
    /// `collect_types`, so the user's registration overwrites the builtin's
    /// `fn_sigs` slot (user already wins).  This test pins the expected
    /// behaviour as a regression guard, and also validates the
    /// `resolve_identifier_variant` two-pass fix does not break this.
    #[test]
    fn user_payload_variant_shadows_builtin_unit() {
        let output = check_source(
            r#"
            enum AppError {
                NotFound(string);
                Timeout;
                Forbidden;
            }

            fn get_error() -> AppError {
                NotFound("resource missing")
            }

            fn main() {
                let _e = get_error();
            }
            "#,
        );
        assert!(
            output.errors.is_empty(),
            "user payload variant NotFound should shadow builtin LookupError::NotFound; \
             got errors: {:#?}",
            output.errors
        );
    }

    /// User `enum Status { Timeout; }` — bare `Timeout` must deterministically
    /// resolve to `Status::Timeout`, not `LookupError::Timeout` (builtin unit).
    ///
    /// The two-pass scan in `resolve_identifier_variant` ensures user-declared
    /// types always win regardless of `HashMap` iteration order.
    #[test]
    fn user_unit_variant_shadows_builtin_unit() {
        let output = check_source(
            r"
            enum Status {
                Timeout;
                Ready;
            }

            fn check_timeout(s: Status) -> bool {
                s == Timeout
            }

            fn main() {
                let s: Status = Timeout;
                let _b = check_timeout(s);
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "user unit variant Timeout should shadow builtin; got errors: {:#?}",
            output.errors
        );
    }

    /// Qualified form `LookupError::NotFound` must still resolve correctly
    /// even when a user enum declares `NotFound`.  Qualified resolution
    /// bypasses the bare-name collision entirely.
    #[test]
    fn qualified_builtin_variant_still_resolves() {
        let output = check_source(
            r"
            fn main() {
                let _e: LookupError = LookupError::NotFound;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "qualified LookupError::NotFound must still resolve; got errors: {:#?}",
            output.errors
        );
    }

    /// Two user enums that both declare the same bare variant name: the TYPE
    /// CHECKER resolves it to whichever registration ran last (last-write-wins
    /// `fn_sigs` slot for user enums) — no type error at the checker layer.
    ///
    /// User-vs-user ambiguity is enforced at the HIR lowering layer via
    /// `bare_counts` (not the type checker), which is unchanged by the
    /// local-shadows-global fix.  This test pins the checker-layer behaviour:
    /// bare `Conflict` is NOT a type error even with two same-named variants,
    /// and this is unchanged by the shadowing fix (which only demotes BUILTIN
    /// variants relative to user-declared ones).
    #[test]
    fn user_vs_user_same_variant_checker_resolves_last_writer() {
        // At the checker layer, fn_sigs["Conflict"] is last-write-wins: A
        // registers first, B overwrites.  The expression `Conflict` resolves
        // to whichever won the slot — no type error.
        let output = check_source(
            r"
            enum A { Conflict; }
            enum B { Conflict; }

            fn main() {
                let _x = Conflict;
            }
            ",
        );
        // Type checker does not enforce uniqueness for bare variants when there
        // are multiple user-declared types with the same variant name — that
        // guard lives in HIR lowering (bare_counts).  No type errors expected.
        assert!(
            output.errors.is_empty(),
            "bare Conflict with two user enums should not produce type errors \
             (HIR lowering enforces uniqueness, not the checker); got: {:#?}",
            output.errors
        );
    }

    /// A user-declared `Task` enum must not trigger `TaskNotNameable`.
    ///
    /// `Task<T>` is normally a compiler-internal name.  When the user declares
    /// their own `Task` enum, the local-shadows-global rule must bypass the
    /// reservation guard so `Task` in type-annotation positions resolves to the
    /// user's enum, not the compiler-internal type.
    #[test]
    fn user_task_enum_shadows_reserved_name() {
        let output = check_source(
            r#"
            enum Task {
                Pending;
                Done;
            }

            fn describe(t: Task) -> string {
                match t {
                    Pending => { return "pending"; }
                    Done    => { return "done"; }
                }
            }

            fn main() {
                let t: Task = Pending;
                let _s = describe(t);
            }
            "#,
        );
        assert!(
            output.errors.is_empty(),
            "user-declared Task enum must not raise TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    /// `Task<T>` without a local declaration must still raise `TaskNotNameable`.
    ///
    /// This is the negative companion to `user_task_enum_shadows_reserved_name`:
    /// the reservation guard must fire when the user has NOT declared a `Task`
    /// type of their own.
    #[test]
    fn bare_task_without_declaration_still_reserved() {
        let output = check_source(
            r"
            fn main() {
                let _t: Task = 0;
            }
            ",
        );
        let has_not_nameable = output
            .errors
            .iter()
            .any(|e| e.kind == crate::error::TypeErrorKind::TaskNotNameable);
        assert!(
            has_not_nameable,
            "Task without local declaration must raise TaskNotNameable; got: {:#?}",
            output.errors
        );
    }
}
