mod support;

use std::process::Command;
use support::{describe_output, hew_binary, run_bounded_command, tempdir};

#[test]
fn host_exports_reject_unselected_or_unsupported_source_contracts() {
    let directory = tempdir();
    for (name, source, selection, expected) in [
        (
            "private",
            "fn label(value: string) -> string { value }",
            "label=config_label",
            "public monomorphic root-source function",
        ),
        (
            "missing",
            "pub fn label(value: string) -> string { value }",
            "absent=config_label",
            "public monomorphic root-source function",
        ),
        (
            "generic",
            "pub fn label<T>(value: T) -> T { value }",
            "label=config_label",
            "public monomorphic root-source function",
        ),
        (
            "entry",
            "pub fn label(value: string) -> string { value } fn main() {}",
            "label=config_label",
            "library without a process entry",
        ),
        (
            "reserved",
            "pub fn label(value: string) -> string { value }",
            "label=__hew_fn_label",
            "non-reserved ASCII C identifier",
        ),
        (
            "sdk_type",
            "pub fn label(value: string) -> string { value }",
            "label=HewRuntime",
            "non-reserved ASCII C identifier",
        ),
        (
            "sdk_status",
            "pub fn label(value: string) -> string { value }",
            "label=HEW_OK",
            "non-reserved ASCII C identifier",
        ),
        (
            "keyword",
            "pub fn label(value: string) -> string { value }",
            "label=class",
            "non-reserved ASCII C identifier",
        ),
        (
            "punctuation",
            "pub fn label(value: string) -> string { value }",
            "label=bad-name",
            "non-reserved ASCII C identifier",
        ),
    ] {
        let input = directory.path().join(format!("{name}.hew"));
        let object = directory.path().join(format!("{name}.o"));
        std::fs::write(&input, source).unwrap();
        let mut command = Command::new(hew_binary());
        command
            .args(["build", "--emit-obj", "--export-c", selection])
            .arg(&input)
            .arg("-o")
            .arg(&object)
            .current_dir(directory.path());
        let output = run_bounded_command(command, format!("reject host contract {name}"));
        assert!(
            !output.status.success(),
            "{name}: {}",
            describe_output(&output)
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("E_HOST_EXPORT") && stderr.contains(expected),
            "{name}: {stderr}"
        );
        assert!(
            !object.exists() && !object.with_extension("h").exists(),
            "refused export emitted artefacts"
        );
    }
}

#[test]
fn public_host_sdk_does_not_grant_source_runtime_call_permission() {
    let directory = tempdir();
    for symbol in [
        "hew_host_text_release",
        "hew_host_json_release",
        "hew_host_value_forget",
        "hew_host_value_release",
        "hew_host_call_new",
        "hew_host_call_poll",
        "hew_host_call_cancel",
        "hew_host_call_release",
    ] {
        let input = directory.path().join(format!("{symbol}.hew"));
        std::fs::write(
            &input,
            format!("extern \"rt\" {{ fn {symbol}(value: i64); }} fn main() {{}}"),
        )
        .unwrap();
        let mut command = Command::new(hew_binary());
        command
            .arg("check")
            .arg(&input)
            .current_dir(directory.path());
        let output =
            run_bounded_command(command, format!("reject source SDK declaration {symbol}"));
        assert!(!output.status.success(), "{}", describe_output(&output));
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains(symbol) && stderr.contains("extern"),
            "{stderr}"
        );
        assert!(
            !stderr.contains("expected"),
            "unexpected parser or signature failure: {stderr}"
        );
    }
}

#[test]
fn asynchronous_exports_refuse_colliding_symbols_before_emitting_artefacts() {
    let directory = tempdir();
    let input = directory.path().join("asynchronous.hew");
    let object = directory.path().join("asynchronous.o");
    std::fs::write(&input, "pub fn label(value: string) -> string { value }").unwrap();
    for (options, expected) in [
        (vec!["--export-c-async", "label=call"], "--emit-obj"),
        (
            vec!["--emit-obj", "--export-c-async", "label=HewWaker"],
            "non-reserved ASCII C identifier",
        ),
        (
            vec![
                "--emit-obj",
                "--export-c-async",
                "label=call",
                "--export-c",
                "label=call_poll",
            ],
            "generated C export symbols collide",
        ),
    ] {
        let mut command = Command::new(hew_binary());
        command
            .arg("build")
            .args(options)
            .arg(&input)
            .arg("-o")
            .arg(&object)
            .current_dir(directory.path());
        let output = run_bounded_command(command, "reject asynchronous host contract");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !output.status.success() && stderr.contains(expected),
            "{stderr}"
        );
        assert!(!object.exists() && !object.with_extension("h").exists());
    }
}

#[test]
fn confined_host_constructor_checks_its_authored_initializer() {
    let directory = tempdir();
    let input = directory.path().join("constructor.hew");
    std::fs::write(
        &input,
        r#"import std.fs;
fn ambient() { let file = fs.open_read("orders.csv").expect("open"); }
#[confined] actor Bad { init() { ambient(); } }
#[confined] pub fn create() {
    let handle = spawn Bad;
    stop(handle);
    stopped(handle);
}"#,
    )
    .unwrap();
    let mut command = Command::new(hew_binary());
    command
        .arg("check")
        .arg(&input)
        .current_dir(directory.path());
    let output = run_bounded_command(command, "reject confined host initializer");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success() && stderr.contains("E_CONFINED_AUTHORITY"),
        "{stderr}"
    );
    assert!(
        !stderr.contains("without a checked body"),
        "initializer proof was lost: {stderr}"
    );
}
