//! Alias-vs-copy metadata plumbing for the COW envelope lane (Phase α).
//!
//! These tests exercise the `actor_send_aliasing` seam introduced alongside
//! the `HewMsgEnvelope` runtime work: a per-send-site enum that, when
//! consumed by codegen, picks between the legacy deep-copy mailbox path and
//! the refcounted alias path.
//!
//! This commit ships only the producer side of the contract — every
//! send site is recorded as [`ActorSendAliasing::Copy`]. The decision
//! rule that flips eligible non-`Copy` sends to `Alias` lands in a
//! later commit on the same lane, after the codegen consumer is wired.
//! Until then, the assertions here only check that the map is populated
//! at every accepted send site so the codegen-side fail-closed contract
//! has something to read.

mod common;

use hew_types::check::ActorSendAliasing;

use common::typecheck_isolated;

const ACTOR_WITH_NON_COPY_SEND: &str = r#"
    actor Sink {
        let _unused: i64;
        receive fn consume(val: string) {}
    }

    fn main() {
        let a = spawn Sink(_unused: 0);
        let s: string = "payload";
        a.consume(s);
    }
"#;

const ACTOR_WITH_COPY_SEND: &str = r"
    actor Counter {
        let _unused: i64;
        receive fn bump(n: i64) {}
    }

    fn main() {
        let c = spawn Counter(_unused: 0);
        c.bump(42);
    }
";

/// The producer must insert an entry at every accepted send site so
/// codegen can read the side table fail-closed without re-deriving the
/// choice from the AST.
#[test]
fn actor_send_aliasing_map_populated_for_non_copy_send() {
    let output = typecheck_isolated(ACTOR_WITH_NON_COPY_SEND);
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    assert!(
        !output.actor_send_aliasing.is_empty(),
        "actor_send_aliasing must contain an entry for the non-Copy send"
    );
}

/// `Copy`-typed payloads (`i64`) also land in the side table as `Copy`.
/// The decision rule never picks `Alias` for `Copy` types because the
/// alias bit would be ambiguous and the value already fits inline at the
/// call boundary.
#[test]
fn actor_send_aliasing_map_populated_for_copy_send() {
    let output = typecheck_isolated(ACTOR_WITH_COPY_SEND);
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    assert!(
        !output.actor_send_aliasing.is_empty(),
        "actor_send_aliasing must contain an entry for the Copy send"
    );
    for decision in output.actor_send_aliasing.values() {
        assert!(
            matches!(decision, ActorSendAliasing::Copy(_)),
            "expected Copy, got {decision:?}"
        );
    }
}

/// Receive-method dispatch through a *bare actor field* (typed as the
/// actor name, e.g. `let target: Printer`, not `ActorRef<Printer>`) routes
/// through `check_named_method_fallback` rather than the `ActorRef` path.
/// That fallback used to skip `enforce_actor_boundary_send`, leaving the
/// `actor_send_aliasing` map empty for those sites — a producer gap that
/// blocks the codegen-side fail-closed lookup.
///
/// This test pins the fix: every arg of a receive-method dispatch on a
/// named-actor field must produce an entry in `actor_send_aliasing`.
#[test]
fn actor_send_aliasing_records_named_actor_field_receive_dispatch() {
    let output = typecheck_isolated(
        r"
        actor Printer {
            receive fn print_result(n: i64) {}
        }

        actor Adder {
            let amount: i64;
            let target: Printer;
            receive fn add(n: i64) {
                let result = n + amount;
                target.print_result(result);
            }
        }

        fn main() {
            let printer = spawn Printer;
            let adder = spawn Adder(amount: 10, target: printer);
            adder.add(5);
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    // Four accepted send sites in this program:
    //   1. `amount: 10` arg in `spawn Adder(...)` (spawn args)
    //   2. `target: printer` arg in `spawn Adder(...)` (spawn args)
    //   3. `adder.add(5)` from main (ActorRef receive dispatch)
    //   4. `target.print_result(result)` from inside Adder::add (the
    //      named-actor field receive dispatch — the gap this test pins).
    //
    // Without the producer fix at the named-actor field dispatch path,
    // entry (4) is missing and the count is 3. Asserting `>= 4` ensures
    // a regression here would make the test fail rather than slide past.
    assert!(
        output.actor_send_aliasing.len() >= 4,
        "expected actor_send_aliasing to record an entry for every accepted \
         send site including the named-actor field receive dispatch, got {} entries: {:#?}",
        output.actor_send_aliasing.len(),
        output.actor_send_aliasing
    );
}

// ── Alias-rule decision tests (commit 7b) ───────────────────────────────────

/// Bare-identifier non-`Copy` send → `Alias`.
#[test]
fn actor_send_aliasing_bare_identifier_non_copy_picks_alias() {
    let output = typecheck_isolated(
        r#"
        actor Sink {
            let _unused: i64;
            receive fn consume(val: string) {}
        }

        fn main() {
            let a = spawn Sink(_unused: 0);
            let s: string = "payload";
            a.consume(s);
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    let alias_count = output
        .actor_send_aliasing
        .values()
        .filter(|d| **d == ActorSendAliasing::Alias)
        .count();
    assert!(
        alias_count >= 1,
        "expected at least one Alias entry for bare-identifier non-Copy send, \
         got {} aliases out of {} total entries: {:#?}",
        alias_count,
        output.actor_send_aliasing.len(),
        output.actor_send_aliasing,
    );
}

/// Field-access non-`Copy` send → `Copy`.
#[test]
fn actor_send_aliasing_field_access_non_copy_stays_copy() {
    let output = typecheck_isolated(
        r#"
        type Bag {
            val: string
        }

        actor Sink {
            let _unused: i64;
            receive fn consume(val: string) {}
        }

        fn main() {
            let a = spawn Sink(_unused: 0);
            let b = Bag { val: "payload" };
            a.consume(b.val);
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    let alias_count = output
        .actor_send_aliasing
        .values()
        .filter(|d| **d == ActorSendAliasing::Alias)
        .count();
    assert_eq!(
        alias_count, 0,
        "field-access send must stay Copy; got {alias_count} Alias entries: {:#?}",
        output.actor_send_aliasing,
    );
}

/// `Copy`-typed payload (`i64`) → `Copy`.
#[test]
fn actor_send_aliasing_copy_typed_bare_identifier_stays_copy() {
    let output = typecheck_isolated(
        r"
        actor Counter {
            let _unused: i64;
            receive fn bump(n: i64) {}
        }

        fn main() {
            let c = spawn Counter(_unused: 0);
            let n: i64 = 42;
            c.bump(n);
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    for decision in output.actor_send_aliasing.values() {
        assert!(
            matches!(decision, ActorSendAliasing::Copy(_)),
            "Copy-typed sends must stay Copy, got {decision:?}"
        );
    }
}

/// User `impl Drop` is rejected fail-closed even when the type is only used
/// as an actor-send payload: the rejection fires at registration, so the
/// send-aliasing classifier never has to reason about an unsupported
/// destructor type.
#[test]
fn actor_send_user_impl_drop_payload_rejected() {
    let output = typecheck_isolated(
        r#"
        type Resource {
            name: string
        }

        impl Drop for Resource {
            fn drop(r: Resource) {}
        }

        actor Sink {
            let _unused: i64;
            receive fn consume(val: Resource) {}
        }

        fn main() {
            let a = spawn Sink(_unused: 0);
            let r = Resource { name: "rsrc" };
            a.consume(r);
        }
    "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.message
                .contains("`impl Drop` is not supported (its `drop` method would not run)")
        }),
        "user impl Drop must be rejected fail-closed, got: {:#?}",
        output.errors
    );
}

/// Programs with no actor sends must produce an empty map — the producer
/// only inserts at the boundary, so an empty TCO field on actor-free
/// programs confirms the population is scoped, not a default.
#[test]
fn actor_send_aliasing_map_empty_when_no_sends() {
    let output = typecheck_isolated(
        r"
        fn main() {
            let x: i64 = 1;
            let _ = x + 1;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "fixture should type-check cleanly, got: {:#?}",
        output.errors
    );
    assert!(
        output.actor_send_aliasing.is_empty(),
        "no actor sends in source means no entries"
    );
}
