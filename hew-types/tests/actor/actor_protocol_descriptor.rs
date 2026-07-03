//! End-to-end checker tests for Q87 slice 1 actor protocol descriptors.
//!
//! Asserts the producer side-table (`TypeCheckOutput.actor_protocol_descriptors`)
//! is populated for source-defined actors and that handler reorder does not
//! perturb the resulting `msg_id`s. Collision-path tests live in
//! `hew-types/src/actor_protocol.rs` (unit) because synthesising a real
//! SipHash-1-3 32-bit preimage collision from source-level handler names is
//! impractical; the unit test uses the explicit-id constructor seam.

use crate::common;

use common::typecheck_isolated as typecheck;
use hew_types::compute_default_msg_id;

#[test]
fn actor_protocol_descriptor_populates_for_each_actor() {
    let output = typecheck(
        r"
        actor Counter {
            let count: i64;

            receive fn increment(n: i64) {
            }

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

    // Determinism: the published msg_ids must match the documented hash
    // contract verbatim. External tooling that reproduces the hash must see
    // the same values.
    assert_eq!(inc_id, compute_default_msg_id("Counter::increment"));
    assert_eq!(total_id, compute_default_msg_id("Counter::total"));
}

#[test]
fn actor_protocol_descriptor_msg_ids_are_stable_across_handler_reorder() {
    // Source-order swap of the two `receive fn`s in `Counter`. Pre-Q87 this
    // would have flipped the `.enumerate()` index for every handler. After
    // slice 1 the msg_ids are derived from the fully-qualified handler name
    // and must therefore be unchanged under reorder.
    let original = typecheck(
        r"
        actor Counter {
            let count: i64;

            receive fn increment(n: i64) {
            }

            receive fn total() -> i64 {
                return count;
            }
        }
        ",
    );
    let reordered = typecheck(
        r"
        actor Counter {
            let count: i64;

            receive fn total() -> i64 {
                return count;
            }

            receive fn increment(n: i64) {
            }
        }
        ",
    );

    assert!(original.errors.is_empty(), "{:?}", original.errors);
    assert!(reordered.errors.is_empty(), "{:?}", reordered.errors);

    let orig = original.actor_protocol_descriptors.get("Counter").unwrap();
    let reord = reordered.actor_protocol_descriptors.get("Counter").unwrap();

    assert_eq!(
        orig.msg_id_for("increment"),
        reord.msg_id_for("increment"),
        "`increment` msg_id must be stable across handler reorder",
    );
    assert_eq!(
        orig.msg_id_for("total"),
        reord.msg_id_for("total"),
        "`total` msg_id must be stable across handler reorder",
    );
}

#[test]
fn actor_protocol_descriptor_qualifies_by_actor_name() {
    // The same handler name in two different actors must not collide,
    // because the hash input is the fully-qualified name.
    let output = typecheck(
        r"
        actor Counter {
            let count: i64;

            receive fn tick() {
            }
        }

        actor Gauge {
            let value: i64;

            receive fn tick() {
            }
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
        .and_then(|d| d.msg_id_for("tick"))
        .expect("Counter::tick descriptor missing");
    let gauge_tick = output
        .actor_protocol_descriptors
        .get("Gauge")
        .and_then(|d| d.msg_id_for("tick"))
        .expect("Gauge::tick descriptor missing");
    assert_ne!(
        counter_tick, gauge_tick,
        "the actor-qualified hash input must separate same-named handlers across actors",
    );
}
