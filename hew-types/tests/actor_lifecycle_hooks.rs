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
fn reject_pure_hook() {
    // Hooks may send messages and call I/O; marking one `pure` is a
    // contradiction the checker rejects.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(stop)]
            pure fn flush() {
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
            .any(|e| e.message.contains("on(stop)") && e.message.contains("`pure`")),
        "`pure` lifecycle hook should be rejected: {:?}",
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

// ── E1: `#[on(crash)]` / `#[on(upgrade)]` recognition ───────────────
//
// Phase 1 slice E1 of the failure-philosophy plan adds two new event
// names to the `#[on(<event>)]` family. v0.5 only needs the parser/
// checker to *recognise* them; signature shape (params, return type,
// body) and the reserved-marker emission for `upgrade` are owned by
// E2. These fixtures pin recognition: well-formed declarations are
// accepted, malformed ones produce a hook-specific diagnostic.

#[test]
fn accept_on_crash_recognised_as_hook() {
    // E1: `#[on(crash)]` is a recognised hook event.  E2 pins the
    // signature: `(info: PanicInfo) -> CrashAction`.  This fixture asserts
    // the recognition path AND the E2 conforming shape: a well-formed
    // hook produces no errors.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(crash)]
            fn on_crash(info: PanicInfo) -> CrashAction {
                CrashAction::Restart
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "well-formed `#[on(crash)]` should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_on_upgrade_recognised_as_hook() {
    // E1: `#[on(upgrade)]` is a recognised hook event.  E2 keeps the
    // reserved-marker shape: no parameters, unit return type — matches
    // `#[on(stop)]`. Runtime invocation is deferred (see #1817).
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(upgrade)]
            fn on_upgrade() {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "well-formed reserved-marker `#[on(upgrade)]` should type-check: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_with_extra_args() {
    // `#[on(crash, foo)]` is malformed — the event slot takes exactly
    // one identifier. start/stop reach this through `check_lifecycle_hook`,
    // but crash/upgrade bypass that signature checker in E1, so the
    // attribute-shape check lives in the event dispatch itself.
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
            .any(|e| e.message.contains("on(upgrade)")
                && e.message.contains("does not accept extra arguments")),
        "`#[on(upgrade, …)]` with extra args should be rejected: {:?}",
        output.errors
    );
}

// ── E2: `#[on(crash)]` signature pinning ─────────────────────────────
//
// Failure-philosophy slice E2 pins the crash hook signature shape:
// `fn on_crash(info: PanicInfo) -> CrashAction`.  `PanicInfo` and
// `CrashAction` come from `std/failure.hew` and are pre-registered for
// inline tests via `register_builtin_failure_surface`.  Each rejection
// has an accept twin and a reject twin (HEW-SPEC-2026 §3.3 both-path).

#[test]
fn on_crash_signature_pinned() {
    // Accept twin: the canonical shape.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(crash)]
            fn on_crash(info: PanicInfo) -> CrashAction {
                CrashAction::Escalate
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "canonical `#[on(crash)]` shape should type-check: {:?}",
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
                && e.message.contains("must have type `PanicInfo`")),
        "`#[on(crash)]` with non-PanicInfo param should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_missing_return_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: PanicInfo) {
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
            fn on_crash(info: PanicInfo) -> i32 {
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
fn accept_on_crash_all_three_variants_assignable() {
    // The `CrashAction` enum carries three variants per Q46/A23:
    // `Restart | Escalate | Kill`. Pin them here so the variant set
    // can't silently regress (e.g. someone deleting `Kill` because
    // E3 supervisor logic only consults `Restart`/`Escalate`).
    for variant in ["Restart", "Escalate", "Kill"] {
        let src = format!(
            "
            actor Worker {{
                #[on(crash)]
                fn on_crash(info: PanicInfo) -> CrashAction {{
                    CrashAction::{variant}
                }}
            }}

            fn main() {{}}
            "
        );
        let output = typecheck(&src);
        assert!(
            output.errors.is_empty(),
            "`CrashAction::{variant}` should be a valid return value: {:?}",
            output.errors
        );
    }
}

// ── E2: `#[on(upgrade)]` signature pinning ───────────────────────────
//
// v0.5 reserves the `#[on(upgrade)]` surface but defers runtime
// invocation (tracked in issue #1817 — the deferred-WASM anchor). The hook
// signature is therefore the same shape as `#[on(stop)]`: no
// parameters, unit return. Adding a `PrevVersion` parameter or any
// non-unit return must wait for the runtime invocation lane.

#[test]
fn on_upgrade_signature_pinned() {
    let output = typecheck(
        r"
        actor Worker {
            let count: i32;

            #[on(upgrade)]
            fn on_upgrade() {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "reserved-marker `#[on(upgrade)]` shape should type-check: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_upgrade_with_param() {
    // v0.5 has no `PrevVersion` type — parameters are rejected so the
    // surface stays free for the runtime invocation lane (#1817) to
    // ratify the eventual shape without breaking existing code.
    let output = typecheck(
        r"
        actor Worker {
            #[on(upgrade)]
            fn on_upgrade(prev: i32) {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(upgrade)")
                && e.message.contains("must take")
                && e.message.contains("no parameters")),
        "`#[on(upgrade)]` with parameters should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_upgrade_non_unit_return() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(upgrade)]
            fn on_upgrade() -> i32 {
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
            .any(|e| e.message.contains("on(upgrade)")
                && e.message.contains("must")
                && e.message.contains("return")),
        "`#[on(upgrade)]` with non-unit return should be rejected: {:?}",
        output.errors
    );
}
