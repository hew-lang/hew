mod common;

use common::typecheck_isolated as typecheck;
use hew_types::{ActorMethodKind, Ty};

#[test]
fn actor_method_dispatch_classifies_fire_and_ask_sites() {
    let output = typecheck(
        r"
        actor Counter {
            let count: i32;

            receive fn increment(n: i32) {
            }

            receive fn print_total() -> i32 {
                return count;
            }
        }

        fn main() -> i32 {
            let c = spawn Counter(count: 0);
            c.increment(10);
            await c.print_total();
            return 0;
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "counter actor should typecheck: {:?}",
        output.errors
    );
    assert!(
        output.actor_method_dispatch.values().any(|kind| {
            matches!(kind, ActorMethodKind::Fire(method_id) if method_id == "Counter::increment")
        }),
        "increment call should be recorded as actor fire dispatch: {:?}",
        output.actor_method_dispatch
    );
    assert!(
        output.actor_method_dispatch.values().any(|kind| {
            matches!(
                kind,
                ActorMethodKind::Ask(method_id, Ty::I32) if method_id == "Counter::print_total"
            )
        }),
        "print_total call should be recorded as actor ask dispatch: {:?}",
        output.actor_method_dispatch
    );
}
