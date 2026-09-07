use super::check_source;
use crate::actor_delivery::{ActorDeliveryCall, SendPolicy};
use crate::check::effects::SuspensionEffect;

/// The call is the send. A rejected submission hands the whole message back,
/// and the two moves left on it — `.to(other)` and `.retry()` — resubmit it.
#[test]
fn actor_calls_submit_and_rejections_can_be_resubmitted() {
    let source = r#"actor Worker { receive fn process(value: string) {} }
        fn main() {
            let worker = spawn Worker();
            let backup = spawn Worker();
            match worker.process("work") {
                .Ok(delivery) => {},
                .Err(rejected) => { let _ = rejected.message.to(backup); }
            }
            match worker.process("again") {
                .Ok(delivery) => {},
                .Err(rejected) => { let _ = rejected.message.retry(); }
            }
        }"#;
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    // Two calls submit; `.retry()` submits a third time; `.to(backup)`
    // readdresses and submits in one step.
    assert_eq!(
        output
            .actor_delivery_calls
            .values()
            .filter(|call| matches!(
                call,
                ActorDeliveryCall::Submit {
                    policy: SendPolicy::Reject
                } | ActorDeliveryCall::Readdress {
                    policy: SendPolicy::Reject,
                    ..
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

/// `.Wait` is the only suspending policy, and the effect comes from the view's
/// type, not from the handler being called (A399).
#[test]
fn actor_delivery_wait_policy_only_suspends_submission() {
    let source = r"actor Worker { receive fn process(value: i64) {} }
        fn main() {
            let worker = spawn Worker();
            let sender = policy(worker, on_full: .Wait);
            let _ = worker.process(1);
            let _ = sender.process(42);
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
        "sender.process(42)"
    );
}

/// A returned message is affine, addressed to one protocol, and opaque.
#[test]
fn actor_delivery_rejects_reuse_and_incompatible_destination() {
    for (body, diagnostic) in [
        ("let _ = m.retry(); let _ = m.retry();", "moved"),
        ("let other = spawn Other(); let _ = m.to(other);", "type"),
        ("let x = m.payload;", "sealed"),
        ("m.retry();", "e_send_result_dropped"),
    ] {
        let source = format!(
            "actor Worker {{ receive fn process(value: i64) {{}} }} \
             actor Other {{ receive fn process(value: i64) {{}} }} \
             fn main() {{ let worker = spawn Worker(); \
             match worker.process(1) {{ \
                 .Ok(_) => {{}}, \
                 .Err(rejected) => {{ let m = rejected.message; {body} }} \
             }} }}"
        );
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

/// A statement-position send or ask drops its typed delivery outcome, which is
/// how a delivery failure gets lost by accident (HEW-SPEC-2026 §2.1.1, §5.6).
/// Every way of using the outcome keeps the program legal; only the bare
/// statement is refused.
#[test]
fn statement_position_delivery_outcomes_are_refused() {
    const ACTOR: &str = "actor Doubler { receive fn tell(n: i64) {} \
         receive fn process(n: i64) -> i64 { n * 2 } }";

    for (body, error) in [
        ("d.tell(1);", "SendFailure"),
        ("await d.process(5);", "AskError"),
        (
            "let log = actor |n: i64| { let _ = n; }; log(5);",
            "SendError",
        ),
    ] {
        let source = format!("{ACTOR} fn main() {{ let d = spawn Doubler; {body} }}");
        let output = check_source(&source);
        let hit = output
            .errors
            .iter()
            .find(|e| e.kind == crate::error::TypeErrorKind::SendResultDropped)
            .unwrap_or_else(|| panic!("{body} must be E_SEND_RESULT_DROPPED: {:?}", output.errors));
        assert!(
            hit.message.contains("E_SEND_RESULT_DROPPED") && hit.message.contains(error),
            "{body}: {}",
            hit.message
        );
        assert!(
            hit.suggestions
                .iter()
                .any(|s| s.contains("let _ = <expr>;")),
            "{body} needs the explicit-discard fix-it: {:?}",
            hit.suggestions
        );
    }
}

#[test]
fn handled_delivery_outcomes_are_accepted() {
    const ACTOR: &str = "actor Doubler { receive fn tell(n: i64) {} \
         receive fn process(n: i64) -> i64 { n * 2 } }";

    for body in [
        "_ = d.tell(1);",
        "let _ = d.tell(1);",
        "let r = d.tell(1); let _ = r;",
        "d.tell(1) handle failure { Delivery.Accepted };",
        "match d.tell(1) { .Ok(_) => {}, .Err(_) => {} }",
        "_ = await d.process(5);",
        "match await d.process(5) { .Ok(_) => {}, .Err(_) => {} }",
    ] {
        let source = format!("{ACTOR} fn main() {{ let d = spawn Doubler; {body} }}");
        let output = check_source(&source);
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == crate::error::TypeErrorKind::SendResultDropped),
            "{body} uses its outcome and must be accepted: {:?}",
            output.errors
        );
    }
}

/// A tail delivery outcome is the block's value, and `?` propagates it; neither
/// is a discard.
#[test]
fn used_delivery_outcomes_outside_statement_position_are_accepted() {
    const ACTOR: &str = "actor Doubler { receive fn tell(n: i64) {} \
         receive fn process(n: i64) -> i64 { n * 2 } }";

    for signature_and_body in [
        "fn main() -> Result<i64, AskError> { let d = spawn Doubler; await d.process(5) }",
        "fn main() -> Result<(), AskError> { let d = spawn Doubler; \
         let _ = await d.process(5)?; Ok(()) }",
    ] {
        let source = format!("{ACTOR} {signature_and_body}");
        let output = check_source(&source);
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == crate::error::TypeErrorKind::SendResultDropped),
            "{signature_and_body}: {:?}",
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
        "let { payload, .. } = m;",
        "let forged = Message { target: worker, message_id: 999, payload: (1,) };",
    ] {
        let source = format!(
            "actor Worker {{ receive fn process(value: i64) {{}} }} \
             fn main() {{ let worker = spawn Worker(); \
             match worker.process(1) {{ \
                 .Ok(_) => {{}}, \
                 .Err(rejected) => {{ let m = rejected.message; {body} }} \
             }} }}"
        );
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
        fn main() { let worker = spawn Worker(); let _ = worker.process(text: "work", number: 7); }"#,
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
    let output = check_source("actor Worker { receive fn process(first: i64, second: i64) {} } fn main() { let worker = spawn Worker(); let _ = worker.process(first: 1, first: 2); }");
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
    let output = check_source("actor Worker { receive fn process() {} } fn reason(error: SendError) -> SendError { error } fn main() { let worker = spawn Worker(); match worker.process() { .Ok(_) => {}, .Err(failure) => { let same = reason(failure.reason); } } }");
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output.type_defs["std.builtins.SendFailure"].fields["reason"],
        crate::Ty::send_error()
    );
}
