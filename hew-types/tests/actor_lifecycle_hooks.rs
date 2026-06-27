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

// ── E1b: `#[on(crash)]` returns a CrashAction value (M-4) ────────────
//
// The `CrashAction` enum-variant return is now wired end-to-end: the checker
// accepts a CrashAction-returning hook body (the former
// `CrashActionReturnNotYetWired` fail-closed gate is removed), MIR returns the
// CrashAction value by its natural enum-return ABI, and the supervisor honours
// the returned variant. Every return position — tail expression, explicit
// `return`, non-final return, in-branch return — type-checks identically.

#[test]
fn accept_crash_action_tail_return() {
    // A `CrashAction::Restart` tail expression now type-checks cleanly.
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
    assert!(
        output.errors.is_empty(),
        "`CrashAction::Restart` tail body should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_action_explicit_return_stmt() {
    // The explicit `return CrashAction::Restart;` form also type-checks.
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
    assert!(
        output.errors.is_empty(),
        "`return CrashAction::Restart;` should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_action_nonfinal_return_before_more_stmts() {
    // A non-final `return CrashAction::Restart;` (followed by more code)
    // type-checks: the explicit return is a valid CrashAction return.
    let source = r#"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                return CrashAction::Restart;
                panic("dead code after the early return")
            }
        }

        fn main() {}
        "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "non-final `return CrashAction::Restart;` should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_action_return_inside_if_then_more_code() {
    // A `return CrashAction::Escalate;` inside an `if` branch, with a diverging
    // fallthrough, type-checks: every return position is a valid CrashAction
    // return now that the fail-closed gate is removed.
    let source = r"
        actor Worker {
            let flag: i32;

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                if flag == 1 {
                    return CrashAction::Escalate;
                }
                CrashAction::Kill
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "`return CrashAction::Escalate` inside an `if` branch should type-check cleanly: {:?}",
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

// ── E1d: closure nested inside `#[on(crash)]` must not inherit flag ──
//
// A closure defined inside an `#[on(crash)]` hook body is NOT the hook itself.
// A `return CrashAction::X;` inside such a nested closure is a valid closure
// return statement, independent of the hook's own return path.

#[test]
fn accept_closure_inside_crash_hook_returning_crash_action() {
    // The closure captures context from the hook but has its OWN return type
    // annotation of `CrashAction`. The `return CrashAction::Restart;` inside the
    // closure is a valid closure return statement; the hook body itself diverges
    // via `panic(...)`. Both type-check cleanly.
    let output = typecheck(
        r#"
        actor Worker {
            let flag: i32;

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                let handler = || -> CrashAction {
                    return CrashAction::Restart;
                };
                panic("diverging hook body")
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "a closure inside `#[on(crash)]` that returns CrashAction should type-check cleanly: {:?}",
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
    // `Restart | Escalate | Kill`. The type-checker recognises each variant as
    // a valid `CrashAction` expression and (M-4) accepts it as the hook return
    // — no signature mismatch, no fail-closed gate, no error at all.
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
            output.errors.is_empty(),
            "`CrashAction::{variant}` should type-check cleanly as a hook return: {:?}",
            output.errors
        );
    }
}
