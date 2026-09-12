mod support;

use std::process::Command;
use support::{describe_output, hew_binary, run_bounded_command, tempdir};

#[test]
fn host_exports_reject_unselected_or_unsupported_source_contracts() {
    let directory = tempdir();
    for (name, source, selection, expected) in [
        ("private", "fn label(value: string) -> string { value }", "label=config_label",
            "public monomorphic root-source function"),
        ("missing", "pub fn label(value: string) -> string { value }", "absent=config_label",
            "public monomorphic root-source function"),
        ("generic", "pub fn label<T>(value: T) -> T { value }", "label=config_label",
            "public monomorphic root-source function"),
        ("entry", "pub fn label(value: string) -> string { value } fn main() {}", "label=config_label",
            "library without a process entry"),
        ("scalar", "pub fn label(value: i64) -> i64 { value }", "label=config_label",
            "one shared or consumed string parameter and a string result"),
        ("multiple", "pub fn label(a: string, b: string) -> string { a + b }", "label=config_label",
            "one shared or consumed string parameter and a string result"),
        ("io", "fn helper(value: string) { println(value); } pub fn label(value: string) -> string { helper(value); value }",
            "label=config_label", "unsupported runtime operation"),
        ("reserved", "pub fn label(value: string) -> string { value }", "label=__hew_fn_label",
            "non-reserved ASCII C identifier"),
        ("keyword", "pub fn label(value: string) -> string { value }", "label=class",
            "non-reserved ASCII C identifier"),
        ("punctuation", "pub fn label(value: string) -> string { value }", "label=bad-name",
            "non-reserved ASCII C identifier"),
    ] {
        let input = directory.path().join(format!("{name}.hew"));
        let object = directory.path().join(format!("{name}.o"));
        std::fs::write(&input, source).unwrap();
        let mut command = Command::new(hew_binary());
        command.args(["build", "--emit-obj", "--export-c", selection])
            .arg(&input).arg("-o").arg(&object).current_dir(directory.path());
        let output = run_bounded_command(command, format!("reject host contract {name}"));
        assert!(!output.status.success(), "{name}: {}", describe_output(&output));
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("E_HOST_EXPORT") && stderr.contains(expected), "{name}: {stderr}");
        assert!(!object.exists() && !object.with_extension("h").exists(), "refused export emitted artefacts");
    }
}

#[test]
fn public_host_sdk_does_not_grant_source_runtime_call_permission() {
    let directory = tempdir();
    for symbol in ["hew_host_text_release", "hew_host_json_release"] {
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
