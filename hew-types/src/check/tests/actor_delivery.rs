use super::check_source;
use crate::actor_delivery::{ActorDeliveryCall, SendPolicy};
use crate::check::effects::SuspensionEffect;

#[test]
fn actor_delivery_constructs_and_retries_owned_messages() {
    let source = r#"actor Worker { receive fn process(value: string) {} }
        fn main() {
            let worker = spawn Worker();
            let backup = spawn Worker();
            let message = worker.process("work");
            match send message {
                .Ok(delivery) => {},
                .Err(rejected) => { let _ = send rejected.message.to(backup); }
            }
        }"#;
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output
            .actor_delivery_calls
            .values()
            .filter(|call| matches!(
                call,
                ActorDeliveryCall::Submit {
                    policy: SendPolicy::Reject
                }
            ))
            .count(),
        2
    );
    assert!(output
        .suspension_effects
        .calls
        .values()
        .all(|effect| *effect == SuspensionEffect::Never));
}

#[test]
fn actor_delivery_wait_policy_only_suspends_submission() {
    let source = r"actor Worker { receive fn process(value: i64) {} }
        fn main() {
            let worker = spawn Worker();
            let sender = policy(worker, on_full: .Wait);
            let message = sender.process(42);
            let _ = send message;
        }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let submissions: Vec<_> = output
        .suspension_effects
        .calls
        .iter()
        .filter(|(_, effect)| **effect == SuspensionEffect::MaySuspend)
        .collect();
    assert_eq!(submissions.len(), 1, "{:?}", output.suspension_effects);
    assert_eq!(
        &source[submissions[0].0.start..submissions[0].0.end],
        "send message"
    );
}

#[test]
fn actor_delivery_rejects_reuse_and_incompatible_destination() {
    for (body, diagnostic) in [
        ("let _ = send message; let _ = send message;", "moved"),
        (
            "let other = spawn Other(); let _ = message.to(other);",
            "type",
        ),
        ("let x = message.payload;", "sealed"),
        ("send message;", "result must be handled"),
    ] {
        let source = format!("actor Worker {{ receive fn process(value: i64) {{}} }} actor Other {{ receive fn process(value: i64) {{}} }} fn main() {{ let worker = spawn Worker(); let message = worker.process(1); {body} }}");
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.to_lowercase().contains(diagnostic)),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn actor_delivery_sealing_does_not_capture_user_record_names() {
    let output = check_source("type Message { payload: i64 } type ActorSender { target: i64 } fn main() { let message = Message { payload: 7 }; let sender = ActorSender { target: message.payload }; let x = sender.target; }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn actor_delivery_seals_message_destructuring_and_construction() {
    for body in [
        "let { payload, .. } = message;",
        "let forged = Message { target: worker, message_id: 999, payload: (1,) };",
    ] {
        let source = format!("actor Worker {{ receive fn process(value: i64) {{}} }} fn main() {{ let worker = spawn Worker(); let message = worker.process(1); {body} }}");
        let output = check_source(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains("sealed")),
            "{body}: {:?}",
            output.errors
        );
    }
}

#[test]
fn actor_delivery_named_arguments_preserve_protocol_order() {
    let output = check_source(
        r#"actor Worker { receive fn process(number: i64, text: string) {} }
        fn main() { let worker = spawn Worker(); let _ = send worker.process(text: "work", number: 7); }"#,
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let dispatch = output
        .actor_method_dispatch
        .values()
        .next()
        .expect("message constructor");
    assert!(
        matches!(dispatch, crate::ActorMethodKind::Message { argument_order, .. } if argument_order == &[1, 0])
    );
    let output = check_source("actor Worker { receive fn process(first: i64, second: i64) {} } fn main() { let worker = spawn Worker(); let message = worker.process(first: 1, first: 2); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("exactly once")),
        "{:?}",
        output.errors
    );
}

#[test]
fn actor_delivery_failure_reason_matches_annotated_error_values() {
    let output = check_source("actor Worker { receive fn process() {} } fn reason(error: SendError) -> SendError { error } fn main() { let worker = spawn Worker(); match send worker.process() { .Ok(_) => {}, .Err(failure) => { let same = reason(failure.reason); } } }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output.type_defs["std.builtins.SendFailure"].fields["reason"],
        crate::Ty::send_error()
    );
}
