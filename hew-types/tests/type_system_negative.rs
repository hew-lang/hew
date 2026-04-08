use hew_types::error::TypeErrorKind;
use hew_types::Checker;
use hew_types::Ty;

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    checker.check_program(&parsed.program)
}

fn generic_param(name: &str) -> Ty {
    Ty::Named {
        name: name.to_string(),
        args: vec![],
    }
}

fn assert_resolved_return_hole(source: &str, sig_name: &str, expected_return_type: &Ty) {
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "Expected return hole to resolve cleanly for {}, got errors: {:?}",
        sig_name,
        output.errors
    );

    let sig = output.fn_sigs.get(sig_name).unwrap_or_else(|| {
        panic!(
            "Expected signature for {}, got {:?}",
            sig_name, output.fn_sigs
        )
    });
    assert_eq!(
        &sig.return_type, expected_return_type,
        "Unexpected return type for {sig_name}: {sig:?}"
    );
}

fn ty_contains_unresolved_var(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::Tuple(elems) => elems.iter().any(ty_contains_unresolved_var),
        Ty::Array(elem, _) | Ty::Slice(elem) => ty_contains_unresolved_var(elem),
        Ty::Named { args, .. } => args.iter().any(ty_contains_unresolved_var),
        Ty::Function { params, ret } => {
            params.iter().any(ty_contains_unresolved_var) || ty_contains_unresolved_var(ret)
        }
        Ty::Closure {
            params,
            ret,
            captures,
        } => {
            params.iter().any(ty_contains_unresolved_var)
                || ty_contains_unresolved_var(ret)
                || captures.iter().any(ty_contains_unresolved_var)
        }
        Ty::Pointer { pointee, .. } => ty_contains_unresolved_var(pointee),
        Ty::TraitObject { traits } => traits
            .iter()
            .flat_map(|bound| bound.args.iter())
            .any(ty_contains_unresolved_var),
        Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::I64
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::U64
        | Ty::F32
        | Ty::F64
        | Ty::IntLiteral
        | Ty::FloatLiteral
        | Ty::Bool
        | Ty::Char
        | Ty::String
        | Ty::Bytes
        | Ty::Duration
        | Ty::Unit
        | Ty::Never
        | Ty::Machine { .. }
        | Ty::Error => false,
    }
}

fn assert_expr_type_output_has_no_unresolved_ty_vars(output: &hew_types::TypeCheckOutput) {
    assert!(
        output
            .expr_types
            .values()
            .all(|ty| !ty_contains_unresolved_var(ty)),
        "expr_types leaked unresolved Ty::Var: {:?}",
        output.expr_types
    );
}

// ── 1. MutabilityError — assign to a `let` binding ──────────────────

#[test]
fn mutability_error_assign_to_let_binding() {
    let output = typecheck(
        r"
        fn main() {
            let x = 5;
            x = 10;
        }
    ",
    );
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::MutabilityError)
        .unwrap_or_else(|| panic!("Expected MutabilityError, got errors: {:?}", output.errors));
    assert!(
        err.suggestions.iter().any(|s| s.contains("var x")),
        "Expected `var` suggestion in MutabilityError, got: {:?}",
        err.suggestions
    );
}

// ── 2. ArityMismatch — wrong number of arguments ────────────────────

#[test]
fn arity_mismatch_too_few_arguments() {
    let output = typecheck(
        r"
        fn add(a: int, b: int) -> int { a + b }
        fn main() { add(1); }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch),
        "Expected ArityMismatch, got errors: {:?}",
        output.errors
    );
}

// ── 3. ReturnTypeMismatch — explicit empty return in non-unit fn ─────
// The checker emits ReturnTypeMismatch only for explicit `return;` when
// the function signature requires a non-unit return type. Implicit
// trailing-expression mismatches produce Mismatch instead.

#[test]
fn return_type_mismatch_empty_return_in_non_unit_fn() {
    let output = typecheck(
        r"
        fn get_name() -> string { return; }
        fn main() { get_name(); }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ReturnTypeMismatch),
        "Expected ReturnTypeMismatch, got errors: {:?}",
        output.errors
    );
}

// ── 4. UndefinedField — access nonexistent struct field ──────────────

#[test]
fn undefined_field_on_struct() {
    let output = typecheck(
        r"
        type Point { x: int; y: int; }
        fn main() {
            let p = Point { x: 1, y: 2 };
            let z = p.z;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedField),
        "Expected UndefinedField, got errors: {:?}",
        output.errors
    );
}

// ── 5. UndefinedMethod — call method that doesn't exist ─────────────

#[test]
fn undefined_method_on_struct() {
    let output = typecheck(
        r"
        type Foo { x: int; }
        fn main() {
            let f = Foo { x: 1 };
            f.bar();
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedMethod),
        "Expected UndefinedMethod, got errors: {:?}",
        output.errors
    );
}

// ── 6. DuplicateDefinition — define same function twice ─────────────

#[test]
fn duplicate_definition_same_function() {
    let output = typecheck(
        r"
        fn foo() {}
        fn foo() {}
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6b. DuplicateDefinition — define same struct twice ──────────────

#[test]
fn duplicate_definition_same_struct() {
    let output = typecheck(
        r"
        type Foo { x: int; }
        type Foo { y: int; }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6c. DuplicateDefinition — define same enum twice ────────────────

#[test]
fn duplicate_definition_same_enum() {
    let output = typecheck(
        r"
        enum Colour { Red; }
        enum Colour { Green; }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6d. DuplicateDefinition — define same trait twice ───────────────

#[test]
fn duplicate_definition_same_trait() {
    let output = typecheck(
        r"
        trait Printable { fn render(val: Self) -> int; }
        trait Printable { fn print(val: Self) -> int; }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6e. DuplicateDefinition — define same actor twice ───────────────

#[test]
fn duplicate_definition_same_actor() {
    let output = typecheck(
        r"
        actor Worker { let id: int; }
        actor Worker { let count: int; }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6f. DuplicateDefinition — define same machine twice ─────────────

#[test]
fn duplicate_definition_same_machine() {
    let output = typecheck(
        r"
        machine Traffic {
            state Red;
            state Green;
            event Tick;
            on Tick: Red -> Green;
            on Tick: Green -> Red;
        }
        machine Traffic {
            state Idle;
            state Busy;
            event Tick;
            on Tick: Idle -> Busy;
            on Tick: Busy -> Idle;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6g. DuplicateDefinition — machine companion event collides with type ──

#[test]
fn duplicate_definition_machine_companion_event_same_type() {
    let output = typecheck(
        r"
        machine Light {
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
        }
        type LightEvent { code: int; }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

#[test]
fn duplicate_definition_machine_companion_event_type_before_machine() {
    let output = typecheck(
        r"
        type LightEvent { code: int; }
        machine Light {
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6h. DuplicateDefinition — machine companion event collides with trait ──

#[test]
fn duplicate_definition_machine_companion_event_same_trait() {
    let output = typecheck(
        r"
        machine Light {
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
        }
        trait LightEvent { fn render(val: Self) -> int; }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

#[test]
fn duplicate_definition_machine_companion_event_trait_before_machine() {
    let output = typecheck(
        r"
        trait LightEvent { fn render(val: Self) -> int; }
        machine Light {
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6i. DuplicateDefinition — define same wire type twice ────────────

#[test]
fn duplicate_definition_same_wire_type() {
    let output = typecheck(
        r"
        wire type Packet {
            id: i32 @1;
        }
        wire type Packet {
            name: String @1;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6j. DuplicateDefinition — define same type alias twice ────────────

#[test]
fn duplicate_definition_same_type_alias() {
    let output = typecheck(
        r"
        type Foo = int;
        type Foo = int;
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6k. DuplicateDefinition — type alias collides with struct ─────────

#[test]
fn duplicate_definition_type_alias_collides_with_struct() {
    let output = typecheck(
        r"
        type Foo { x: int; }
        type Foo = int;
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6l. DuplicateDefinition — type alias collides with trait ──────────

#[test]
fn duplicate_definition_type_alias_collides_with_trait() {
    let output = typecheck(
        r"
        trait Foo { fn render(val: Self) -> int; }
        type Foo = int;
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "Expected DuplicateDefinition, got errors: {:?}",
        output.errors
    );
}

// ── 6m. InferenceFailed — unresolved top-level type alias hole ───────

#[test]
fn inference_failed_unresolved_type_alias_hole() {
    let output = typecheck(
        r"
        type Foo = _;
        fn main() {}
    ",
    );
    let inference_failed_count = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .count();
    assert_eq!(
        inference_failed_count, 1,
        "Expected exactly one InferenceFailed, got errors: {:?}",
        output.errors
    );
}

// ── 6n. Narrow fix — nested type alias hole does not fail closed ─────

#[test]
fn nested_type_alias_hole_does_not_fail_closed() {
    let output = typecheck(
        r"
        type Pair = (int, _);
        fn main() {}
    ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Nested type alias holes should not fail closed here, got errors: {:?}",
        output.errors
    );
}

// ── 7. Shadowing — inner scope binding shadows outer scope ───────────
// Nested/child scope shadowing emits a warning (not an error); only same-scope
// rebinding and actor field shadowing are hard errors.

#[test]
fn shadowing_inner_scope_shadows_outer_binding() {
    let output = typecheck(
        r"
        fn main() {
            let x = 5;
            if true {
                let x = 10;
                println(x);
            }
            println(x);
        }
    ",
    );
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "Expected Shadowing warning, got warnings: {:?}, errors: {:?}",
        output.warnings,
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "Nested scope shadowing should be a warning, not an error, got: {:?}",
        output.errors
    );
}

// ── 8. InvalidOperation — binary op on incompatible types ───────────

#[test]
fn invalid_operation_string_plus_int() {
    let output = typecheck(
        r#"
        fn main() { let x = "hello" + 5; }
    "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation),
        "Expected InvalidOperation, got errors: {:?}",
        output.errors
    );
}

// ── 9. PurityViolation — call impure fn from pure fn ────────────────
// PurityViolation requires the `pure fn` keyword. Regular non-receive
// actor functions are NOT automatically pure.

#[test]
fn purity_violation_call_impure_from_pure() {
    let output = typecheck(
        r"
        fn side_effect() {}
        pure fn must_be_pure() {
            side_effect();
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PurityViolation),
        "Expected PurityViolation, got errors: {:?}",
        output.errors
    );
}

// ── 10. UseAfterMove — send non-Copy value to actor twice ───────────
// The checker marks non-Copy values as moved at actor message boundaries.
// A struct with a `string` field is non-Copy, triggering move semantics.

#[test]
fn use_after_move_send_to_actor_twice() {
    let output = typecheck(
        r#"
        type Payload { data: string; }
        actor Sink {
            let val: int;
            receive fn consume(h: Payload) {}
        }
        fn main() {
            let s = spawn Sink(val: 0);
            let h = Payload { data: "hello" };
            s.consume(h);
            s.consume(h);
        }
    "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UseAfterMove),
        "Expected UseAfterMove, got errors: {:?}",
        output.errors
    );
}

// ── 11. YieldOutsideGenerator — yield in a regular function ─────────

#[test]
fn yield_outside_generator_in_regular_fn() {
    let output = typecheck(
        r"
        fn main() { yield 42; }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::YieldOutsideGenerator),
        "Expected YieldOutsideGenerator, got errors: {:?}",
        output.errors
    );
}

// ── 12. UnusedVariable — declared but never used (warning) ──────────

#[test]
fn unused_variable_warning_for_unread_binding() {
    let output = typecheck(
        r#"
        fn main() {
            let _used = 1;
            let unused_val = 2;
            println(f"{_used}");
        }
    "#,
    );
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedVariable),
        "Expected UnusedVariable warning, got warnings: {:?}",
        output.warnings
    );
}

// ── 13. NonExhaustiveMatch — Option missing None arm (error) ──────

#[test]
fn nonexhaustive_match_option_missing_none() {
    let output = typecheck(
        r"
        fn check(x: Option<int>) -> int {
            match x {
                Some(v) => v,
            }
        }
        fn main() {
            check(Some(1));
        }
    ",
    );
    // Option is enum-like → non-exhaustive is a hard error.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "Expected NonExhaustiveMatch error for Option, got errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "NonExhaustiveMatch for Option must not appear as warning"
    );
}

// ── 14. NonExhaustiveMatch — Result missing Err arm (error) ───────

#[test]
fn nonexhaustive_match_result_missing_err() {
    let output = typecheck(
        r"
        fn check(r: Result<int, string>) -> int {
            match r {
                Ok(v) => v,
            }
        }
        fn main() {
            check(Ok(1));
        }
    ",
    );
    // Result is enum-like → non-exhaustive is a hard error.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "Expected NonExhaustiveMatch error for Result, got errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "NonExhaustiveMatch for Result must not appear as warning"
    );
}

// ── 15. NonExhaustiveMatch — enum missing variant (error) ─────────

#[test]
fn nonexhaustive_match_enum_missing_variant() {
    let output = typecheck(
        r#"
        enum Colour { Red; Green; Blue; }
        fn label(c: Colour) -> string {
            match c {
                Red => "red",
                Green => "green",
            }
        }
        fn main() {
            println(label(Red));
        }
    "#,
    );
    // User enum is enum-like → non-exhaustive is a hard error.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "Expected NonExhaustiveMatch error for missing Blue, got errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "NonExhaustiveMatch for enum must not appear as warning"
    );
}

// ── 16. MachineExhaustivenessError — fewer than 2 states ────────────

#[test]
fn machine_exhaustiveness_too_few_states() {
    let output = typecheck(
        r"
        machine Broken {
            state Only;
            event Ping;
            on Ping: Only -> Only;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError),
        "Expected MachineExhaustivenessError for < 2 states, got errors: {:?}",
        output.errors
    );
}

// ── 17. MachineExhaustivenessError — no events declared ─────────────

#[test]
fn machine_exhaustiveness_no_events() {
    let output = typecheck(
        r"
        machine Broken {
            state A;
            state B;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError),
        "Expected MachineExhaustivenessError for 0 events, got errors: {:?}",
        output.errors
    );
}

// ── 18. MachineExhaustivenessError — unknown event in transition ────

#[test]
fn machine_exhaustiveness_unknown_event() {
    let output = typecheck(
        r"
        machine Broken {
            state A;
            state B;
            event X;
            on X: A -> B;
            on X: B -> A;
            on Ghost: A -> B;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError
                && e.message.contains("unknown event")),
        "Expected MachineExhaustivenessError for unknown event, got errors: {:?}",
        output.errors
    );
}

// ── 19. MachineExhaustivenessError — unknown state in transition ────

#[test]
fn machine_exhaustiveness_unknown_state() {
    let output = typecheck(
        r"
        machine Broken {
            state A;
            state B;
            event X;
            on X: A -> B;
            on X: B -> Phantom;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError
                && e.message.contains("unknown state")),
        "Expected MachineExhaustivenessError for unknown state, got errors: {:?}",
        output.errors
    );
}

// ── 20. MachineExhaustivenessError — duplicate wildcard ─────────────

#[test]
fn machine_exhaustiveness_duplicate_wildcard() {
    let output = typecheck(
        r"
        machine Broken {
            state A;
            state B;
            event X;
            on X: _ -> _ { state }
            on X: _ -> _ { state }
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError
                && e.message.contains("duplicate wildcard")),
        "Expected MachineExhaustivenessError for duplicate wildcard, got errors: {:?}",
        output.errors
    );
}

#[test]
fn inference_hole_function_parameter_signature_is_rejected() {
    let output = typecheck(
        r"
        fn f(x: _) {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn checker_output_does_not_expose_unresolved_ty_var_survivors() {
    let output = typecheck(
        r"
        fn helper() {
            let x: _ = None;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
    assert_expr_type_output_has_no_unresolved_ty_vars(&output);
}

#[test]
fn checker_output_success_path_contains_no_unresolved_ty_var() {
    let output = typecheck(
        r"
        type Box<T> { value: T; }

        fn id<T>(x: T) -> _ { x }

        fn main() {
            let v = id(1);
            let _w = v;
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "Expected successful typecheck, got errors: {:?}",
        output.errors
    );
    assert_expr_type_output_has_no_unresolved_ty_vars(&output);
    let sig = output
        .fn_sigs
        .get("id")
        .unwrap_or_else(|| panic!("expected function signature for id"));
    assert!(
        sig.params.iter().all(|ty| !ty_contains_unresolved_var(ty))
            && !ty_contains_unresolved_var(&sig.return_type),
        "signature id leaked unresolved Ty::Var: {sig:?}"
    );
    let ty_def = output
        .type_defs
        .get("Box")
        .unwrap_or_else(|| panic!("expected type def for Box"));
    assert!(
        ty_def
            .fields
            .values()
            .all(|ty| !ty_contains_unresolved_var(ty)),
        "Box fields leaked unresolved Ty::Var: {:?}",
        ty_def.fields
    );
}

#[test]
fn inference_hole_function_return_signature_is_resolved() {
    let output = typecheck(
        r"
        fn f() -> _ {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed, got errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.fn_sigs.get("f").map(|sig| &sig.return_type),
        Some(&hew_types::Ty::Unit)
    );
}

#[test]
fn inference_hole_generic_free_function_return_signature_is_resolved() {
    assert_resolved_return_hole(
        r"
        fn f<T>(x: T) -> _ { x }
        fn main() {}
    ",
        "f",
        &generic_param("T"),
    );
}

#[test]
fn inference_hole_generic_impl_method_return_signature_is_resolved() {
    assert_resolved_return_hole(
        r"
        type Box<T> { value: T; }
        impl<T> Box<T> {
            fn get(boxed: Box<T>, x: T) -> _ { x }
        }
        fn main() {}
    ",
        "Box::get",
        &generic_param("T"),
    );
}

#[test]
fn inference_hole_generic_actor_receive_return_signature_is_resolved() {
    assert_resolved_return_hole(
        r"
        actor Foo {
            receive fn bar<T>(x: T) -> _ { x }
        }
        fn main() {}
    ",
        "Foo::bar",
        &generic_param("T"),
    );
}

#[test]
fn inference_hole_generic_actor_method_return_signature_is_resolved() {
    assert_resolved_return_hole(
        r"
        actor Foo {
            fn bar<T>(x: T) -> _ { x }
        }
        fn main() {}
    ",
        "Foo::bar",
        &generic_param("T"),
    );
}

#[test]
fn inference_hole_type_field_is_rejected() {
    let output = typecheck(
        r"
        type Box { value: _; }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_local_var_annotation_is_rejected() {
    let output = typecheck(
        r"
        fn main() {
            var value: _ = None;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_local_let_annotation_is_resolved_from_later_use() {
    let output = typecheck(
        r"
        fn takes(value: Option<int>) {}

        fn main() {
            let value: _ = None;
            takes(value);
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_const_annotation_is_rejected() {
    let output = typecheck(
        r"
        const MAYBE: _ = None;
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_const_annotation_is_resolved_from_later_use() {
    let output = typecheck(
        r"
        const MAYBE: _ = None;

        fn takes(value: Option<int>) {}

        fn main() {
            takes(MAYBE);
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_lambda_param_annotation_is_rejected() {
    let output = typecheck(
        r"
        fn main() {
            let _f = (x: _) => 1;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_lambda_param_annotation_is_inferred_from_expected_type() {
    let output = typecheck(
        r"
        fn main() {
            let _f: fn(int) -> int = (x: _) => 1;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_lambda_return_annotation_is_rejected() {
    let output = typecheck(
        r"
        fn main() {
            let _f = () -> _ => None;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_lambda_return_annotation_is_resolved_from_body() {
    let output = typecheck(
        r"
        fn main() {
            let _f = () -> _ => 1;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_actor_init_param_annotation_is_rejected() {
    let output = typecheck(
        r"
        actor Greeter {
            let name: string;
            init(prefix: _) {
                println(name);
            }
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_actor_init_param_annotation_is_resolved_from_body() {
    let output = typecheck(
        r"
        actor Greeter {
            let name: string;
            init(prefix: _) {
                name = prefix;
            }
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_cast_target_annotation_is_rejected() {
    let output = typecheck(
        r"
        fn main() {
            let x = 42;
            let y = x as _;
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "Expected InferenceFailed, got errors: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "Did not expect cast mismatch before inference failure, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_cast_target_annotation_is_resolved_from_later_use() {
    let output = typecheck(
        r"
        fn takes_i32(value: i32) {}

        fn main() {
            let x = 42;
            let y = x as _;
            takes_i32(y);
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "Expected cast target hole to resolve from later use, got errors: {:?}",
        output.errors
    );
}

#[test]
fn explicit_hole_nonitem_cast_target_annotation_still_rejects_invalid_inferred_cast() {
    let output = typecheck(
        r"
        fn takes_string(value: string) {}

        fn main() {
            let x = 42;
            let y = x as _;
            takes_string(y);
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })
                && e.message.contains("cannot cast")),
        "Expected cast mismatch, got errors: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::InferenceFailed),
        "Did not expect InferenceFailed once cast target was inferred, got errors: {:?}",
        output.errors
    );
}

// ── 21. MachineExhaustivenessError — duplicate explicit transition ──

#[test]
fn machine_exhaustiveness_duplicate_explicit() {
    let output = typecheck(
        r"
        machine Broken {
            state A;
            state B;
            event X;
            on X: A -> B;
            on X: A -> A;
            on X: B -> A;
        }
        fn main() {}
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError
                && e.message.contains("duplicate transition")),
        "Expected MachineExhaustivenessError for duplicate transition, got errors: {:?}",
        output.errors
    );
}

// ── 22. InvalidOperation — `?` in non-Option/Result function ────────

#[test]
fn postfix_try_in_non_option_result_function() {
    // Regression: `?` on an Option inside a function that returns a plain
    // type must be rejected at typecheck time, not silently emitted as bad IR.
    let output = typecheck(
        r"
        fn maybe(x: i32) -> Option<i32> {
            if x > 0 { Some(x) } else { None }
        }
        fn plain(x: i32) -> i32 {
            let v = maybe(x)?;
            v * 2
        }
        fn main() { plain(5); }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("enclosing function must return")),
        "Expected InvalidOperation for `?` in non-Option/Result function, got errors: {:?}",
        output.errors
    );
}

#[test]
fn postfix_try_valid_in_option_function() {
    // Sanity: `?` on an Option inside a function that also returns Option is fine.
    let output = typecheck(
        r"
        fn maybe(x: i32) -> Option<i32> {
            if x > 0 { Some(x) } else { None }
        }
        fn double_maybe(x: i32) -> Option<i32> {
            let v = maybe(x)?;
            Some(v * 2)
        }
        fn main() { double_maybe(5); }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "Expected no errors for valid `?` in Option-returning function, got: {:?}",
        output.errors
    );
}

#[test]
fn postfix_try_in_option_lambda_inside_plain_fn_is_valid() {
    // False-rejection regression: `?` inside a lambda with an annotated
    // `-> Option<i32>` return must not be rejected just because the *outer*
    // function returns `i32`.
    let output = typecheck(
        r"
        fn maybe(x: i32) -> Option<i32> {
            if x > 0 { Some(x) } else { None }
        }
        fn outer(x: i32) {
            let _f = (v: i32) -> Option<i32> => {
                let w = maybe(v)?;
                Some(w)
            };
        }
        fn main() { outer(3); }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "Expected no errors for `?` inside Option-returning lambda in plain fn, got: {:?}",
        output.errors
    );
}

#[test]
fn postfix_try_in_plain_lambda_inside_option_fn_is_invalid() {
    // False-acceptance regression: `?` inside a lambda annotated `-> i32`
    // must still be rejected even though the *outer* function returns Option.
    let output = typecheck(
        r"
        fn maybe(x: i32) -> Option<i32> {
            if x > 0 { Some(x) } else { None }
        }
        fn outer(x: i32) -> Option<i32> {
            let _f = (v: i32) -> i32 => {
                let w = maybe(v)?;
                w
            };
            None
        }
        fn main() { outer(3); }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("enclosing function must return")),
        "Expected InvalidOperation for `?` in i32-returning lambda, got: {:?}",
        output.errors
    );
}

// ── 23. BoundsNotSatisfied — type missing required trait ────────────

#[test]
fn bounds_not_satisfied_missing_trait_impl() {
    let output = typecheck(
        r"
        trait Printable {
            fn describe(val: Self) -> string;
        }
        type Dog { name: string; }
        impl Printable for Dog {
            fn describe(d: Dog) -> string { d.name }
        }
        type Rock { weight: int; }
        fn show<T: Printable>(val: T) -> string {
            val.describe()
        }
        fn main() {
            let r = Rock { weight: 42 };
            println(show<Rock>(r));
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::BoundsNotSatisfied),
        "Expected BoundsNotSatisfied, got errors: {:?}",
        output.errors
    );
}

// ── String::chars() typechecks and arity-guards ─────────────────────────────

#[test]
fn string_chars_returns_vec_char() {
    // chars() must typecheck cleanly and produce Vec<char> with zero args.
    let output = typecheck(
        r#"
        fn main() {
            let s = "hello";
            let cs: Vec<char> = s.chars();
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "String::chars() should typecheck without errors; got: {:?}",
        output.errors
    );
}

#[test]
fn string_chars_rejects_extra_args() {
    let output = typecheck(
        r#"
        fn main() {
            let s = "hello";
            let cs = s.chars(1);
        }
    "#,
    );
    assert!(
        !output.errors.is_empty(),
        "String::chars(arg) should produce a typecheck error"
    );
}
