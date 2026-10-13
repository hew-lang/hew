//! Checked actor-local indices and portable declaration identities.

use crate::common;

use common::typecheck_isolated as typecheck;

#[test]
fn actor_protocol_descriptor_populates_for_each_actor() {
    let output = typecheck(
        r"actor Counter {
    let count: i64;

    receive fn increment(n: i64) {}

    receive fn total() -> i64 {
        return count;
    }
}

fn main() -> i64 {
    return 0;
}
",
    );

    assert!(
        output.errors.is_empty(),
        "counter actor should typecheck: {:?}",
        output.errors
    );
    let descriptor = output
        .actor_protocol_descriptors
        .get("Counter")
        .expect("Counter must have a published protocol descriptor");
    assert_eq!(descriptor.actor_name, "Counter");
    assert_eq!(descriptor.handlers.len(), 2);

    let inc_id = descriptor
        .msg_id_for("increment")
        .expect("`increment` must be in the descriptor");
    let total_id = descriptor
        .msg_id_for("total")
        .expect("`total` must be in the descriptor");
    assert_ne!(
        inc_id, total_id,
        "distinct handler names must map to distinct msg_ids"
    );

    // External identities are independent of local dispatch indices.
    assert_eq!(descriptor.handlers[0].wire_identity, "Counter.increment");
    assert_eq!(descriptor.handlers[1].wire_identity, "Counter.total");
}

#[test]
fn actor_protocol_wire_identity_is_stable_across_handler_reorder() {
    let original = typecheck(
        r"actor Counter {
    let count: i64;

    receive fn increment(n: i64) {}

    receive fn total() -> i64 {
        return count;
    }
}
",
    );
    let reordered = typecheck(
        r"actor Counter {
    let count: i64;

    receive fn total() -> i64 {
        return count;
    }

    receive fn increment(n: i64) {}
}
",
    );

    assert!(original.errors.is_empty(), "{:?}", original.errors);
    assert!(reordered.errors.is_empty(), "{:?}", reordered.errors);

    let orig = original.actor_protocol_descriptors.get("Counter").unwrap();
    let reord = reordered.actor_protocol_descriptors.get("Counter").unwrap();

    assert_eq!(
        orig.handlers[0].wire_identity, reord.handlers[1].wire_identity,
        "`increment` wire identity must be stable across handler reorder",
    );
    assert_eq!(
        orig.handlers[1].wire_identity, reord.handlers[0].wire_identity,
        "`total` wire identity must be stable across handler reorder",
    );
}

#[test]
fn actor_protocol_descriptor_qualifies_by_actor_name() {
    // The actor owner is part of the portable declaration identity.
    let output = typecheck(
        r"actor Counter {
    let count: i64;

    receive fn tick() {}
}

actor Gauge {
    let value: i64;

    receive fn tick() {}
}

fn main() -> i64 {
    return 0;
}
",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let counter_tick = output
        .actor_protocol_descriptors
        .get("Counter")
        .map(|d| d.handlers[0].wire_identity.as_str())
        .expect("Counter::tick descriptor missing");
    let gauge_tick = output
        .actor_protocol_descriptors
        .get("Gauge")
        .map(|d| d.handlers[0].wire_identity.as_str())
        .expect("Gauge::tick descriptor missing");
    assert_ne!(
        counter_tick, gauge_tick,
        "declaration ownership separates same-named handlers across actors",
    );
}
