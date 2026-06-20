//! Type-checker fixtures for actor lifecycle hooks
//! (`#[on(start)]` / `#[on(stop)]` / `#[on(crash)]` / `#[on(upgrade)]`).
//!
//! These exercise the §9.1.2 surface defined in
//! `docs/specs/HEW-SPEC-2026.md`. The accept fixtures pin the
//! type-check-clean shape; the reject fixtures pin diagnostic
//! coverage (§3.3 both-path: every diagnostic has an accept twin and
//! a reject twin so the rule is observable from both sides).

mod common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;

// ── Accept fixtures ──────────────────────────────────────────────────

#[test]
fn accept_on_start_only() {
    let output = typecheck(
        r"
        actor Cache {
            let entries: i32;

            #[on(start)]
            fn warm() {
                entries
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "actor with only `#[on(start)]` should type-check: {:?}",
        output.errors
    );
}

#[test]
fn accept_on_stop_only() {
    let output = typecheck(
        r"
        actor Cache {
            let entries: i32;

            #[on(stop)]
            fn flush() {
                entries
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "actor with only `#[on(stop)]` should type-check: {:?}",
        output.errors
    );
}

#[test]
fn accept_multiple_on_stop_in_declared_order() {
    // Multiple `#[on(stop)]` hooks are legal; lexical order is the run
    // order (HEW-SPEC-2026 §9.1.2 rule 6). The type-checker accepts
    // them without complaint.
    let output = typecheck(
        r"
        actor Cache {
            let entries: i32;
            let socket: i32;

            #[on(stop)]
            fn flush_cache() {
                entries
            }

            #[on(stop)]
            fn close_socket() {
                socket
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "multiple `#[on(stop)]` hooks should be accepted: {:?}",
        output.errors
    );
}

// ── Reject fixtures ──────────────────────────────────────────────────

#[test]
fn reject_hook_with_parameters() {
    // Hooks must take no parameters; actor fields are in scope by bare
    // name. A parameter list (e.g. attempting `self`-style receivers,
    // imported from other ecosystems) is rejected.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(stop)]
            fn shutdown(unused: i32) {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.message.contains("must take")
            && e.message.contains("no parameters")
            && e.message.contains("on(stop)")),
        "hook with parameters should be rejected with a hook-specific \
         diagnostic: {:?}",
        output.errors
    );
}

#[test]
fn reject_duplicate_on_start() {
    // `#[on(start)]` is at-most-once per actor (rule 6). Declaring two
    // is a structural error.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(start)]
            fn first() {
                count
            }

            #[on(start)]
            fn second() {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("more than one `#[on(start)]`")),
        "duplicate `#[on(start)]` should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_unknown_hook_kind() {
    // `#[on(restart)]` is not a recognised lifecycle hook in this edition;
    // the checker emits a diagnostic listing the valid kinds (start, stop).
    // Uses a plain identifier that is not a reserved keyword.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(restart)]
            fn setup() {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(restart)")
                && e.message.contains("start")
                && e.message.contains("stop")
                && e.message.contains("crash")
                && e.message.contains("upgrade")),
        "`#[on(restart)]` should be rejected with valid-kinds list \
         (start, stop, crash, upgrade): {:?}",
        output.errors
    );
}

// ── E1: `#[on(crash)]` recognition / `#[on(upgrade)]` fail-closed ─────
//
// `#[on(crash)]` remains a live lifecycle hook. `#[on(upgrade)]` remains
// parser-recognised but must fail closed: it is reserved and not supported,
// because accepting it would create code that never runs.

#[test]
fn on_crash_still_works() {
    // `#[on(crash)]` with a diverging body (`panic(...)`) must type-check
    // cleanly.  The hook itself is live — only the non-diverging
    // `CrashAction`-return path is fail-closed (see
    // `reject_crash_action_return_not_yet_wired`).
    let output = typecheck(
        r#"
        actor Worker {
            let count: i32;

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                panic("handled")
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "well-formed `#[on(crash)]` with diverging body should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn on_upgrade_attribute_compile_errors() {
    let source = r"
        actor Worker {
            let count: i32;

            #[on(upgrade)]
            fn on_upgrade() {
                count
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    let error = output
        .errors
        .iter()
        .find(|e| matches!(&e.kind, TypeErrorKind::OnUpgradeNotYetWired))
        .expect("`#[on(upgrade)]` should produce OnUpgradeNotYetWired");
    let attr_start = source
        .find("#[on(upgrade)]")
        .expect("fixture should contain upgrade attribute");
    let attr_span = attr_start..attr_start + "#[on(upgrade)]".len();
    assert_eq!(
        error.span, attr_span,
        "diagnostic should point at the `#[on(upgrade)]` attribute"
    );
    assert!(
        error.message.contains("reserved")
            && error.message.contains("not supported")
            && error.message.contains("remove the attribute"),
        "diagnostic should explain that the hook is reserved/unsupported and advise removal: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_with_extra_args() {
    // `#[on(crash, foo)]` is malformed — the event slot takes exactly
    // one identifier. start/stop reach this through `check_lifecycle_hook`,
    // but crash has event-specific signature checking, so the attribute-shape
    // check lives in the event dispatch itself.
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash, foo)]
            fn on_crash() {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.message.contains("on(crash)")
            && e.message.contains("does not accept extra arguments")),
        "`#[on(crash, …)]` with extra args should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_upgrade_with_extra_args() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(upgrade, v2)]
            fn on_upgrade() {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::OnUpgradeNotYetWired)),
        "`#[on(upgrade, …)]` should fail closed before runtime wiring lands: {:?}",
        output.errors
    );
}

// ── E1b: `#[on(crash)]` fail-closed for non-diverging body ───────────
//
// `CrashAction` enum-variant return is not yet wired through the compiler
// (the MIR lowering coerces the handler return type to `i32` and a
// `CrashAction` value causes a codegen Move type mismatch).  Surfaced here
// at check-time so the user sees a clear compile error.
//
// WHEN obsolete: when v0.6 wires the full CrashAction return path.

#[test]
fn reject_crash_action_return_not_yet_wired() {
    // Reject twin: `CrashAction::Restart` in the hook body must fail closed
    // with `CrashActionReturnNotYetWired`, not produce a runtime
    // `E_NOT_YET_IMPLEMENTED` codegen panic.
    let source = r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                CrashAction::Restart
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    let error = output
        .errors
        .iter()
        .find(|e| matches!(&e.kind, TypeErrorKind::CrashActionReturnNotYetWired))
        .expect("`CrashAction::Restart` body should produce CrashActionReturnNotYetWired");
    assert!(
        error.message.contains("not yet wired") && error.message.contains("panic"),
        "diagnostic should mention 'not yet wired' and suggest panic: {:?}",
        error.message
    );
}

#[test]
fn reject_crash_action_return_stmt_not_yet_wired() {
    // Reject twin (explicit `return` form): `return CrashAction::Restart;`
    // inside a `#[on(crash)]` hook body must also fail closed with
    // `CrashActionReturnNotYetWired` — same lowering gap, same diagnostic.
    // The cross-eco review named this the `CrashActionReturnNotYetWired`
    // case for explicit-return; it was missing from the original gate which
    // only checked the block tail-expression type.
    let source = r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                return CrashAction::Restart;
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    let error = output
        .errors
        .iter()
        .find(|e| matches!(&e.kind, TypeErrorKind::CrashActionReturnNotYetWired))
        .expect("`return CrashAction::Restart;` should produce CrashActionReturnNotYetWired");
    assert!(
        error.message.contains("not yet wired") && error.message.contains("panic"),
        "diagnostic should mention 'not yet wired' and suggest panic: {:?}",
        error.message
    );
}

// ── E1c: non-final `return CrashAction` inside control flow ──────────
//
// A `return CrashAction::X;` that is NOT the last statement in the hook body
// (e.g. inside an `if` branch, followed by more code) was previously missed by
// the fail-closed gate (which only covered tail-position and final-return paths).
// These fixtures pin the completed coverage.

#[test]
fn reject_crash_action_nonfinal_return_before_more_stmts() {
    // A `return CrashAction::Restart;` that is NOT the last statement in the
    // hook body (followed by unreachable code) was the escape that the previous
    // gates missed.  `check_stmt` processes non-last statements; the tail-position
    // gate in `check_stmt_as_expr` was never reached for this path.
    //
    // The statement is processed by `check_stmt`'s `Stmt::Return` arm, which now
    // carries the same `in_crash_hook` gate as `check_stmt_as_expr`.
    let source = r#"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                return CrashAction::Restart;
                panic("dead code — makes the return non-final in its block")
            }
        }

        fn main() {}
        "#;
    let output = typecheck(source);
    let error = output
        .errors
        .iter()
        .find(|e| matches!(&e.kind, TypeErrorKind::CrashActionReturnNotYetWired))
        .expect(
            "non-final `return CrashAction::Restart;` (followed by more stmts) \
             should produce CrashActionReturnNotYetWired",
        );
    assert!(
        error.message.contains("not yet wired") && error.message.contains("panic"),
        "diagnostic should mention 'not yet wired' and suggest panic: {:?}",
        error.message
    );
}

#[test]
fn reject_crash_action_return_inside_if_then_more_code() {
    // A `return CrashAction::Restart;` inside an `if` body where the `if` itself
    // is NOT the last statement in the hook body: the `if` is routed through
    // `check_stmt_as_expr`, which calls `check_block` on the then-block.  Inside
    // that inner block, the `return` IS the last statement, so it goes through
    // `check_stmt_as_expr` — which the existing gate already covers.
    //
    // This test confirms the accept/reject boundary is consistent: the if-branch
    // `return` is caught by the pre-existing gate, pinning that neither the old
    // nor the new path leaks.
    let source = r#"
        actor Worker {
            let flag: i32;

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                if flag == 1 {
                    return CrashAction::Escalate;
                }
                panic("fallthrough")
            }
        }

        fn main() {}
        "#;
    let output = typecheck(source);
    // The `return CrashAction::Escalate;` is the last stmt in the if-body, so
    // `check_stmt_as_expr`'s gate fires — same `CrashActionReturnNotYetWired`.
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::CrashActionReturnNotYetWired)),
        "`return CrashAction::Escalate` inside an `if` branch should produce \
         CrashActionReturnNotYetWired: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_hook_with_if_and_diverging_body() {
    // Accept twin: an `#[on(crash)]` hook that uses an `if` branch with a
    // diverging expression (`panic(...)`) in each arm still type-checks cleanly.
    let output = typecheck(
        r#"
        actor Worker {
            let flag: i32;

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                if flag == 1 {
                    panic("restart path")
                } else {
                    panic("kill path")
                }
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "`#[on(crash)]` hook with diverging `if` branches should type-check cleanly: {:?}",
        output.errors
    );
}

// ── E2: `#[on(crash)]` signature pinning ─────────────────────────────
//
// Failure-philosophy slice E2 pins the crash hook signature shape:
// `fn on_crash(info: CrashInfo) -> CrashAction`.  `CrashInfo` and
// `CrashAction` come from `std/failure.hew` and are pre-registered for
// inline tests via `register_builtin_failure_surface`.  Each rejection
// has an accept twin and a reject twin (HEW-SPEC-2026 §3.3 both-path).

#[test]
fn on_crash_signature_pinned() {
    // Accept twin: the canonical shape with a diverging body.
    // The `CrashAction` return type is correctly validated; the body uses
    // `panic(...)` to avoid `CrashActionReturnNotYetWired` (see the
    // `crash_action_variants_recognised_by_type_checker` test for the
    // non-diverging case).
    let output = typecheck(
        r#"
        actor Worker {
            let count: i32;

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                panic("crash")
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "canonical `#[on(crash)]` shape with diverging body should type-check: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_missing_param() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash() -> CrashAction {
                CrashAction::Restart
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.message.contains("on(crash)")
            && e.message.contains("exactly one parameter")),
        "`#[on(crash)]` with no params should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_wrong_param_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: i32) -> CrashAction {
                CrashAction::Restart
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(crash)")
                && e.message.contains("must have type `CrashInfo`")),
        "`#[on(crash)]` with non-CrashInfo param should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_missing_return_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(crash)")
                && e.message.contains("must declare a return type")),
        "`#[on(crash)]` without an explicit return type should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_wrong_return_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> i32 {
                0
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(crash)")
                && e.message.contains("must return `CrashAction`")),
        "`#[on(crash)]` returning non-CrashAction should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn crash_action_variants_recognised_by_type_checker() {
    // The `CrashAction` enum carries three variants per Q46/A23:
    // `Restart | Escalate | Kill`. The type-checker must recognise each
    // variant as a valid `CrashAction` expression so the signature check
    // doesn't fire a "wrong type" error — the fail-closed gate
    // (`CrashActionReturnNotYetWired`) fires AFTER the type check succeeds,
    // confirming the checker sees the correct type.
    //
    // Each variant produces `CrashActionReturnNotYetWired`, not any
    // signature/type-mismatch error, pinning that the checker knows the
    // variant set even while the return path is wired fail-closed.
    for variant in ["Restart", "Escalate", "Kill"] {
        let src = format!(
            "
            actor Worker {{
                #[on(crash)]
                fn on_crash(info: CrashInfo) -> CrashAction {{
                    CrashAction::{variant}
                }}
            }}

            fn main() {{}}
            "
        );
        let output = typecheck(&src);
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::CrashActionReturnNotYetWired)),
            "`CrashAction::{variant}` should produce CrashActionReturnNotYetWired: {:?}",
            output.errors
        );
        // No signature or type-mismatch error — the checker accepts the type,
        // only the lowering gate fires.
        assert!(
            !output.errors.iter().any(|e| {
                e.message.contains("must return `CrashAction`")
                    || e.message.contains("undefined variable")
            }),
            "`CrashAction::{variant}` should not produce a type-mismatch error: {:?}",
            output.errors
        );
    }
}
