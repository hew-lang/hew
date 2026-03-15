use hew_types::error::TypeErrorKind;
use hew_types::Checker;

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
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MutabilityError),
        "Expected MutabilityError, got errors: {:?}",
        output.errors
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

// ── 13. NonExhaustiveMatch — Option missing None arm (warning) ──────

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
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "Expected NonExhaustiveMatch warning for Option, got warnings: {:?}",
        output.warnings
    );
}

// ── 14. NonExhaustiveMatch — Result missing Err arm (warning) ───────

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
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "Expected NonExhaustiveMatch warning for Result, got warnings: {:?}",
        output.warnings
    );
}

// ── 15. NonExhaustiveMatch — enum missing variant (warning) ─────────

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
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "Expected NonExhaustiveMatch warning for missing Blue, got warnings: {:?}",
        output.warnings
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

// ── 22. BoundsNotSatisfied — type missing required trait ────────────

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
