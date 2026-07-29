//! Ordinary mutable record fields holding un-clonable builtin handles cannot be
//! reassigned until `RecordFieldStore` carries a source-slot move/neutralisation
//! protocol. Closing the old value alone is insufficient: the store byte-copies
//! the replacement and would leave its source as a second owner.

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, tempdir};

const BUILTIN_HANDLE_OVERWRITES: &str = r"
type SinkHolder { value: Sink<string> }
type StreamHolder { value: Stream<string> }
type GeneratorHolder { value: Generator<i64, ()> }

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
        3,
        "each Sink/Stream/Generator store must be rejected independently:\n{combined}"
    );
    for handle in ["Sink<string>", "Stream<string>", "Generator<i64, ()>"] {
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
