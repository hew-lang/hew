//! Ordinary mutable record fields holding un-clonable builtin handles cannot be
//! reassigned until `RecordFieldStore` carries a source-slot move/neutralisation
//! protocol. Closing the old value alone is insufficient: the store byte-copies
//! the replacement and would leave its source as a second owner.

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, tempdir};

const BUILTIN_HANDLE_OVERWRITES: &str = r"
import std::channel;
import std::link_monitor;

type SinkHolder { value: Sink<string> }
type StreamHolder { value: Stream<string> }
type GeneratorHolder { value: Generator<i64, ()> }
type SenderHolder { value: channel.Sender<string> }
type ReceiverHolder { value: channel.Receiver<string> }
type MonitorHolder { value: link_monitor.MonitorRef }

fn overwrite_sink(a: Sink<string>, b: Sink<string>) {
    var holder = SinkHolder { value: a };
    holder.value = b;
}

fn overwrite_stream(a: Stream<string>, b: Stream<string>) {
    var holder = StreamHolder { value: a };
    holder.value = b;
}

fn overwrite_generator(
    a: Generator<i64, ()>,
    b: Generator<i64, ()>,
) {
    var holder = GeneratorHolder { value: a };
    holder.value = b;
}

fn overwrite_receiver(
    a: channel.Receiver<string>,
    b: channel.Receiver<string>,
) {
    var holder = ReceiverHolder { value: a };
    holder.value = b;
}

fn overwrite_sender(
    a: channel.Sender<string>,
    b: channel.Sender<string>,
) {
    var holder = SenderHolder { value: a };
    holder.value = b;
}

fn overwrite_monitor(
    a: link_monitor.MonitorRef,
    b: link_monitor.MonitorRef,
) {
    var holder = MonitorHolder { value: a };
    holder.value = b;
}

fn main() {}
";

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
fn ordinary_record_builtin_handle_overwrites_are_refused() {
    let output = check_source("builtin_handle_overwrites", BUILTIN_HANDLE_OVERWRITES);
    assert!(
        !output.status.success(),
        "close-bearing builtin record overwrites must fail closed, but check succeeded:\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        combined
            .matches("overwriting an owned handle field")
            .count(),
        6,
        "each reachable close-bearing builtin store must be rejected independently:\n{combined}"
    );
    for handle in [
        "Sink<String>",
        "Stream<String>",
        "Generator<i64, ()>",
        "channel.Sender<String>",
        "channel.Receiver<String>",
        "MonitorRef",
    ] {
        assert!(
            combined.contains(handle),
            "refusal must name destination handle type `{handle}`:\n{combined}"
        );
    }
    assert!(
        combined.contains("rebuild the whole record"),
        "diagnostic must give the sound record-level remediation:\n{combined}"
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
type Sender { value: i64 }
type Receiver { value: i64 }
type SenderHolder { value: Sender }
type ReceiverHolder { value: Receiver }

fn overwrite_sender(a: Sender, b: Sender) -> i64 {
    var holder = SenderHolder { value: a };
    holder.value = b;
    holder.value.value
}

fn overwrite_receiver(a: Receiver, b: Receiver) -> i64 {
    var holder = ReceiverHolder { value: a };
    holder.value = b;
    holder.value.value
}

fn main() -> i64 {
    overwrite_sender(Sender { value: 1 }, Sender { value: 2 })
        + overwrite_receiver(Receiver { value: 3 }, Receiver { value: 4 })
}
",
    );
    assert!(
        output.status.success(),
        "source-qualified runtime diagnostics must not widen the gate over user \
         Sender/Receiver records:\n{}",
        describe_output(&output)
    );
}
