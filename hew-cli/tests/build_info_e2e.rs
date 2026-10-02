mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

const PREFIX: &[u8] = b"HEW-BUILD-INFO: ";

/// The NUL-terminated provenance record embedded in `bytes`, if any.
fn build_info(bytes: &[u8]) -> Option<String> {
    let start = bytes
        .windows(PREFIX.len())
        .position(|window| window == PREFIX)?;
    let end = bytes[start..].iter().position(|byte| *byte == 0)?;
    Some(String::from_utf8_lossy(&bytes[start..start + end]).into_owned())
}

#[test]
fn built_executables_carry_the_compiler_version_at_o0_and_o2() {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("hello.hew");
    std::fs::write(&input, "fn main() { println(\"hello\"); }\n").unwrap();

    let mut version = Command::new(hew_binary());
    version.arg("--version");
    let version = run_bounded_command(version, "hew --version");
    assert!(version.status.success(), "{}", describe_output(&version));
    let version = String::from_utf8_lossy(&version.stdout).trim().to_owned();
    assert!(version.starts_with("hew "), "unexpected version: {version}");

    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("hello-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .arg("--opt-level")
            .arg(opt)
            .arg("-o")
            .arg(&binary);
        let output = run_bounded_command(build, format!("build hello O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let record = build_info(&std::fs::read(&binary).unwrap())
            .unwrap_or_else(|| panic!("O{opt} binary has no HEW-BUILD-INFO record"));
        let triple = record
            .strip_prefix(&format!("HEW-BUILD-INFO: {version} "))
            .unwrap_or_else(|| panic!("record `{record}` does not match `{version}`"));
        assert!(!triple.is_empty() && !triple.contains(' '), "{record}");
    }
}
