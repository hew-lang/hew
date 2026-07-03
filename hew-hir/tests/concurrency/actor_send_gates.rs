//! Tests for actor `.send(msg)` dispatch semantics.
//!
//! The FC-P1-A2 HIR pre-pass gate (`check_actor_send_gates`) was retired when
//! anonymous-payload `.send(payload)` on named actors without a `receive fn
//! send` handler was formally rejected at the type-checker layer (issue #2122).
//! The type-checker now owns the single authority for this shape:
//! - No `receive fn send` handler → `UndefinedMethod` at check time.
//! - Non-unit `receive fn send` without `await` → `InvalidOperation` ("ask
//!   requires await") at check time.
//!
//! Both cases abort compilation before HIR runs, so the HIR gate is no longer
//! reachable for those shapes and has been removed.
//!
//! What remains here:
//! - Type-checker rejects anonymous `.send()` on actors with no `send` handler.
//! - Type-checker rejects anonymous `.send()` on actors with only a non-send handler.
//! - `await actor.compute(msg)` (ask form, user-named handler) compiles clean.
//! - `await actor.send(msg)` (user `receive fn send` → ask under `await`) compiles clean.
//! - Bare `actor.send(msg)` for user `receive fn send` returning non-unit is
//!   rejected at type-check (ask requires `await`).
//! - Lambda-actor `.send()` (`Duplex<Msg, Reply>`) compiles clean.

use hew_types::{module_registry::ModuleRegistry, Checker};

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parsed = hew_parser::parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&parsed.program)
}

fn lower_clean(source: &str) -> hew_hir::LowerOutput {
    use hew_hir::{lower_program, ResolutionCtx, TargetArch};
    let parsed = hew_parser::parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(
        tco.errors.is_empty(),
        "unexpected type errors: {:?}",
        tco.errors
    );
    lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host())
}

// ── Type-checker rejects anonymous send (no handler) ─────────────────────────

#[test]
fn actor_bare_send_no_handler_rejected_at_typecheck() {
    // The type-checker emits `UndefinedMethod` when `w.send(msg)` has no
    // matching `receive fn send` handler.  The rejection is at check time;
    // HIR is never reached.  This replaces the old FC-P1-A2 HIR gate test
    // `actor_bare_send_unit_handler_accepted` (deleted: anonymous send is
    // now a type error even when the actor has a unit-returning handler,
    // because that handler is named `handle`, not `send`).
    let tco = typecheck(
        r"
        actor Worker {
            let count: i64;
            receive fn handle(n: i64) {
            }
        }

        fn main() {
            let w = spawn Worker(count: 0);
            w.send(7);
        }
        ",
    );
    assert!(
        tco.errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::UndefinedMethod),
        "anonymous `.send()` with no `receive fn send` handler must produce \
         UndefinedMethod at type-check; got: {:#?}",
        tco.errors
    );
}

#[test]
fn actor_bare_send_non_unit_named_handler_rejected_at_typecheck() {
    // A bare `.send(msg)` on an actor whose only matching handler is non-unit
    // (e.g. `compute`) is rejected at type-check with `UndefinedMethod`
    // (no `receive fn send` handler).  The old HIR gate
    // (`ActorSendRequiresUnitHandler`) is no longer reachable for this shape.
    let tco = typecheck(
        r"
        actor Calculator {
            let total: i64;

            receive fn compute(x: i64) -> i64 {
                return x + 1;
            }
        }

        fn main() {
            let c = spawn Calculator(total: 0);
            c.send(3);
        }
        ",
    );
    assert!(
        tco.errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::UndefinedMethod),
        "anonymous `.send()` on an actor with only a non-send handler must \
         produce UndefinedMethod at type-check; got: {:#?}",
        tco.errors
    );
}

#[test]
fn actor_bare_send_no_handler_in_machine_transition_rejected_at_typecheck() {
    // Same rejection when the anonymous send is nested inside a machine
    // transition body.  The type-checker runs before HIR, so the rejection
    // is at check time regardless of the enclosing construct.
    let tco = typecheck(
        r"
        actor Calculator {
            let total: i64;

            receive fn compute(x: i64) -> i64 {
                return x + 1;
            }
        }

        machine M {
            events {
                Tick;
            }

            state Active;
            state Idle;

            on Tick: Active => Active reenter {
                let c = spawn Calculator(total: 0);
                c.send(3);
                Active
            }
            on Tick: Idle => Active {
                Active
            }
        }
        ",
    );
    assert!(
        tco.errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::UndefinedMethod),
        "anonymous `.send()` inside a machine transition must produce \
         UndefinedMethod at type-check; got: {:#?}",
        tco.errors
    );
}

// ── Type-checker admits well-formed actor dispatch ────────────────────────────

#[test]
fn actor_ask_non_unit_handler_accepted() {
    // Named handler invoked via ask form (`await`) — correct shape.
    let output = lower_clean(
        r"
        actor Calculator {
            let total: i64;

            receive fn compute(x: i64) -> i64 {
                return x + 1;
            }
        }

        fn main() {
            let c = spawn Calculator(total: 0);
            let _ = await c.compute(3);
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "ask-form (`await c.compute(...)`) must lower cleanly; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn awaited_send_non_unit_handler_accepted() {
    // A user `receive fn send(...) -> T` invoked as an ask via
    // `await ref.send(...)` — correct shape.
    let output = lower_clean(
        r"
        actor Doubler {
            receive fn send(n: i64) -> i64 {
                n * 2
            }
        }

        fn main() -> i64 {
            let d = spawn Doubler;
            match await d.send(21) {
                Ok(v) => v,
                Err(_) => 0,
            }
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "awaited `.send()` ask to a user `receive fn send` must lower cleanly; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn unawaited_send_non_unit_handler_rejected() {
    // A bare, unawaited `ref.send(...)` to a value-returning `receive fn send`
    // discards the reply — the type-checker rejects this ("ask requires await").
    let tco = typecheck(
        r"
        actor Doubler {
            receive fn send(n: i64) -> i64 {
                n * 2
            }
        }

        fn main() -> i64 {
            let d = spawn Doubler;
            d.send(21);
            0
        }
        ",
    );
    assert!(
        !tco.errors.is_empty(),
        "bare `.send()` to a non-unit `receive fn send` must be rejected \
         (reply discarded); got clean check"
    );
}

#[test]
fn actor_send_unknown_handler_skipped() {
    // Lambda-actor handles surface as `Duplex<Msg, Reply>` — not a named actor
    // handle.  The type-checker accepts lambda-actor `.send()` cleanly.
    let output = lower_clean(
        r"
        fn main() {
            let printer = actor |x: i64| {
                println(x);
            };
            printer.send(42);
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "lambda-actor `.send()` must lower cleanly; got: {:#?}",
        output.diagnostics
    );
}
