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
        let _unused: int;
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
        let _unused: int;
        receive fn bump(n: int) {}
    }

    fn main() {
        let c = spawn Counter(_unused: 0);
        c.bump(42);
    }
";

/// Phase α records `Copy` at every accepted send site so codegen can read
/// the side table fail-closed without re-deriving the choice from the AST.
/// The map must contain at least one entry for a program with a single
/// non-`Copy` actor send.
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
    for (span, decision) in &output.actor_send_aliasing {
        assert_eq!(
            *decision,
            ActorSendAliasing::Copy,
            "Phase α must record Copy at every send site (span: {span:?})"
        );
    }
}

/// `Copy`-typed payloads (`int`) also land in the side table as `Copy`.
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
        assert_eq!(*decision, ActorSendAliasing::Copy);
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
            receive fn print_result(n: int) {}
        }

        actor Adder {
            let amount: int;
            let target: Printer;
            receive fn add(n: int) {
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
    // Three accepted send sites in this program:
    //   1. `printer` arg in `spawn Adder(... target: printer)` (spawn args)
    //   2. `adder.add(5)` from main (ActorRef receive dispatch)
    //   3. `target.print_result(result)` from inside Adder::add (the
    //      named-actor field receive dispatch — the gap this test pins).
    // We check (3) explicitly: the side table must have an entry whose
    // span covers a non-empty range inside the actor body. The simplest
    // robust assertion is on the count — at least 3 entries exist.
    assert!(
        output.actor_send_aliasing.len() >= 3,
        "expected actor_send_aliasing to record an entry for every accepted \
         send site (including the named-actor field dispatch), got {} entries: {:#?}",
        output.actor_send_aliasing.len(),
        output.actor_send_aliasing
    );
    // Every entry remains Copy in Phase α; the alias-enable rule lands in
    // the next commit on this lane.
    for decision in output.actor_send_aliasing.values() {
        assert_eq!(*decision, ActorSendAliasing::Copy);
    }
}

/// Programs with no actor sends must produce an empty map — the producer
/// only inserts at the boundary, so an empty TCO field on actor-free
/// programs confirms the population is scoped, not a default.
#[test]
fn actor_send_aliasing_map_empty_when_no_sends() {
    let output = typecheck_isolated(
        r"
        fn main() {
            let x: int = 1;
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
