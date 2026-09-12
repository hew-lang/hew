use crate::common;

use common::typecheck_isolated as typecheck;
use hew_types::{ActorMethodKind, Ty};

#[test]
fn actor_method_dispatch_classifies_message_and_ask_sites() {
    let output = typecheck(
        r"
        actor Counter {
            let count: i32,

            receive fn increment(n: i32) {
            }

            receive fn print_total() -> i32 {
                return count;
            }
        }

        fn main() -> i32 {
            let c = spawn Counter(count: 0);
            let _ = c.increment(10);
            _ = c.print_total();
            return 0;
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "counter actor should typecheck: {:?}",
        output.errors
    );
    // The call is the send (no `send` keyword): a plain call on an actor
    // handle waits for completion like any other call, so `increment` is
    // dispatched as an ask with a `Unit` reply, not a fire-and-forget
    // `Message`. The one-way view lives in `mailbox(target, on_full: ..)`.
    assert!(
        output.actor_method_dispatch.values().any(|kind| {
            matches!(
                kind,
                ActorMethodKind::Ask { method_id, reply_ty: Ty::Unit, .. } if method_id == "Counter::increment"
            )
        }),
        "increment call should be recorded as an actor ask with a Unit reply: {:?}",
        output.actor_method_dispatch
    );
    assert!(
        output.actor_method_dispatch.values().any(|kind| {
            matches!(
                kind,
                ActorMethodKind::Ask { method_id, reply_ty: Ty::I32, .. } if method_id == "Counter::print_total"
            )
        }),
        "print_total call should be recorded as actor ask dispatch: {:?}",
        output.actor_method_dispatch
    );
}
