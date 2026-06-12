//! Tests for FC-P1-A2: Actor `.send(msg)` unit-handler HIR pre-pass gate.
//!
//! The MIR layer carries a defense-in-depth diagnostic at
//! `hew-mir/src/lower.rs:8477` (`ActorSendRequiresUnitHandler`). Per slepp
//! A222 the failure belongs at compile-time; this gate surfaces the same
//! constraint as a HIR fatal diagnostic before MIR ever runs.

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx, TargetArch};
use hew_parser::parser;
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = parser::parse(source);
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

fn has_actor_send_diag(output: &hew_hir::LowerOutput) -> bool {
    output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::ActorSendRequiresUnitHandler { .. }
        )
    })
}

#[test]
fn actor_bare_send_unit_handler_accepted() {
    // Unit-returning handler — `.send(msg)` is the canonical fire-and-forget
    // surface and must not be rejected.
    let output = lower(
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
        !has_actor_send_diag(&output),
        "unit-handler `.send()` must not trigger ActorSendRequiresUnitHandler; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn actor_bare_send_non_unit_handler_rejected() {
    // Non-unit handler reached via fire-and-forget `.send(msg)` — the reply
    // would be discarded. This is the FC-P1-A2 fail-closed case.
    let output = lower(
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
        has_actor_send_diag(&output),
        "non-unit-handler `.send()` must trigger ActorSendRequiresUnitHandler; got: {:#?}",
        output.diagnostics
    );

    // The diagnostic must name the handler and return type so the user can
    // resolve the failure without re-deriving it from the source span.
    let diag = output
        .diagnostics
        .iter()
        .find(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ActorSendRequiresUnitHandler { .. }
            )
        })
        .expect("diagnostic present");
    if let HirDiagnosticKind::ActorSendRequiresUnitHandler {
        actor_name,
        method_name,
        return_ty,
    } = &diag.kind
    {
        assert_eq!(actor_name, "Calculator");
        assert_eq!(method_name, "compute");
        assert!(
            return_ty.contains("i64"),
            "return_ty should render the handler's declared return type, got: {return_ty}"
        );
    } else {
        unreachable!("matched above");
    }

    // Fatal: into_result must reject.
    assert!(
        output.into_result().is_err(),
        "ActorSendRequiresUnitHandler is in the fatal set; into_result() must Err"
    );
}

#[test]
fn actor_ask_non_unit_handler_accepted() {
    // Same non-unit handler, but invoked via call-syntax with `await` (the
    // checker classifies this as `ActorMethodKind::Ask`). The gate is
    // specific to bare `.send(msg)` and must not fire on the ask form.
    let output = lower(
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
        !has_actor_send_diag(&output),
        "ask-form (`await c.compute(...)`) must not trigger ActorSendRequiresUnitHandler; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn actor_send_unknown_handler_skipped() {
    // Defense-in-depth: lambda-actor handles surface as `Duplex<Msg, Reply>`
    // (per `tests/vertical-slice/accept/lambda_method_send.hew`), not as
    // `LocalPid<Actor>`. The gate must skip cleanly when the receiver type
    // is not a recognised actor-handle shape — the lambda-actor protocol
    // is owned by a different surface. False-rejecting here would break
    // the allowed-secondary `.send()` form on lambda-actor handles.
    let output = lower(
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
        !has_actor_send_diag(&output),
        "lambda-actor `.send()` (Duplex<Msg, Reply>) must fall through silently; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn actor_send_in_machine_transition_body_rejected() {
    // Regression for the walker miss: prior to this test `check_actor_send_gates`
    // skipped `Item::Machine` on the (wrong) assumption that machines do not
    // carry user expression bodies. They do — transition bodies, transition
    // guards, and per-state `entry` / `exit` blocks all lower user expressions
    // via `hew-hir/src/lower.rs::lower_machine_expr_filtered` (~L4194) and
    // `lower_machine_block_filtered` (~L4129). A non-unit `.send()` hidden
    // inside any of these forms must trip the same HIR pre-pass gate as one
    // inside a free function.
    let output = lower(
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
        has_actor_send_diag(&output),
        "non-unit `.send()` inside a machine transition body must trigger \
         ActorSendRequiresUnitHandler; got: {:#?}",
        output.diagnostics
    );
}
