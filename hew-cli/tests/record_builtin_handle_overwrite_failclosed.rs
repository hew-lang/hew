//! Overwriting an ordinary mutable record field holding an owned handle is
//! admitted: `main`-unreachable helpers made the old refusal here vacuous
//! either way (reachability-gated emission never lowered them), and the
//! reachable path for an owned handle is a real close-and-replace, not a
//! byte-copy that strands the source as a second owner. A builtin runtime
//! handle (`Sink`, `Stream`, a channel end, `MonitorRef`) has no
//! program-visible close signal, so a user `#[resource]` shadow stands in:
//! its `close` prints, making "the old value closed exactly once before the
//! new value lands" directly observable.

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, tempdir};

const RESOURCE_FIELD_OVERWRITE: &str = r#"
#[resource]
type Handle { id: i64 }

impl Handle {
    fn close(consume self) {
        println(f"closed {self.id}");
    }
}

type Holder { value: Handle }

fn main() {
    var holder = Holder { value: Handle { id: 1 } };
    holder.value = Handle { id: 2 };
    holder.value.close();
}
"#;

fn check_source(name: &str, source: &str) -> std::process::Output {
    let dir = tempdir();
    let path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write Hew source");
    Command::new(hew_binary())
        .args(["check", path.to_str().expect("utf-8 source path")])
        .current_dir(repo_root())
        .output()
        .expect("run hew check")
}

#[test]
fn ordinary_record_owned_handle_overwrite_closes_the_old_value_exactly_once() {
    support::require_codegen();
    let dir = tempdir();
    let path = dir.path().join("resource_field_overwrite.hew");
    std::fs::write(&path, RESOURCE_FIELD_OVERWRITE).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["run", path.to_str().expect("utf-8 source path")])
        .current_dir(repo_root())
        .output()
        .expect("run resource field overwrite fixture");
    assert!(
        output.status.success(),
        "an owned-handle record-field overwrite must be admitted and run clean:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "closed 1\nclosed 2\n",
        "the overwritten value (1) must close exactly once before the store, \
         and the replacement (2) exactly once at its explicit close — never \
         twice, never neither:\n{}",
        describe_output(&output)
    );
}

#[test]
fn ordinary_string_record_overwrite_remains_admitted() {
    let output = check_source(
        "string_overwrite",
        r#"
type Holder { value: string }
fn main() {
    var holder = Holder { value: "old" };
    holder.value = "new";
    println(holder.value);
}
"#,
    );
    assert!(
        output.status.success(),
        "the handle gate must not widen over the existing string overwrite protocol:\n{}",
        describe_output(&output)
    );
}

#[test]
fn ordinary_user_sender_receiver_shadows_remain_admitted() {
    let output = check_source(
        "user_channel_name_shadows",
        r"
type UserSender { value: i64 }
type UserReceiver { value: i64 }
type SenderHolder { value: UserSender }
type ReceiverHolder { value: UserReceiver }

fn overwrite_sender(a: UserSender, b: UserSender) -> i64 {
    var holder = SenderHolder { value: a };
    holder.value = b;
    holder.value.value
}

fn overwrite_receiver(a: UserReceiver, b: UserReceiver) -> i64 {
    var holder = ReceiverHolder { value: a };
    holder.value = b;
    holder.value.value
}

fn main() -> i64 {
    overwrite_sender(UserSender { value: 1 }, UserSender { value: 2 })
        + overwrite_receiver(UserReceiver { value: 3 }, UserReceiver { value: 4 })
}
",
    );
    assert!(
        output.status.success(),
        "source-qualified runtime diagnostics must not widen the gate over user \
         UserSender/UserReceiver records:\n{}",
        describe_output(&output)
    );
}
