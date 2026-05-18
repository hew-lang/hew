mod common;

use common::{parse_and_typecheck_isolated, typecheck_isolated};
use hew_types::{ActorStateGuard, SpanKey};

#[test]
fn receive_handler_emits_guard_fact() {
    let (program, output) = parse_and_typecheck_isolated(
        r"
        actor Counter {
            let count: i32;
            receive fn inc(n: i32) {
                count = count + n;
            }
        }
        fn main() {}
        ",
    );

    assert!(
        output.errors.is_empty(),
        "unexpected type errors: {:?}",
        output.errors
    );
    let actor = program.items.iter().find_map(|(item, _)| match item {
        hew_parser::ast::Item::Actor(actor) if actor.name == "Counter" => Some(actor),
        _ => None,
    });
    let receive = &actor.expect("Counter actor").receive_fns[0];
    assert_eq!(
        output
            .actor_handler_state_guards
            .get(&SpanKey::from(&receive.span)),
        Some(&ActorStateGuard::Exclusive),
        "checker must produce the exclusive state-lock fact for receive handlers"
    );
}

#[test]
fn pure_receive_rejects_state_assignment_and_records_exclusive_guard_policy() {
    let (program, output) = parse_and_typecheck_isolated(
        r"
        actor Counter {
            let count: i32;
            pure receive fn inspect() -> i32 {
                count = count + 1;
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
            .any(|error| error.message.contains("pure")),
        "pure receive must still reject actor-state assignment: {:?}",
        output.errors
    );
    let actor = program.items.iter().find_map(|(item, _)| match item {
        hew_parser::ast::Item::Actor(actor) if actor.name == "Counter" => Some(actor),
        _ => None,
    });
    let receive = &actor.expect("Counter actor").receive_fns[0];
    assert_eq!(
        output
            .actor_handler_state_guards
            .get(&SpanKey::from(&receive.span)),
        Some(&ActorStateGuard::Exclusive),
        "pure receive handlers keep the uniform exclusive dispatch boundary"
    );
}

#[test]
fn actor_without_receive_has_no_guard_facts() {
    let output = typecheck_isolated(
        r"
        actor Empty {
            let count: i32;
        }
        fn main() {}
        ",
    );

    assert!(
        output.actor_handler_state_guards.is_empty(),
        "only receive handlers produce state-lock facts"
    );
}
