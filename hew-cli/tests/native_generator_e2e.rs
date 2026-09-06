//! Generator owners drain recursively before ordinary storage release.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

fn run_generator(source: &str, expected: &str, status: i32, diagnostic: &str) {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("generators.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("generators-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build native generator O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output =
            run_bounded_command(Command::new(binary), format!("run native generator O{opt}"));
        assert_eq!(
            output.status.code(),
            Some(status),
            "{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            expected,
            "{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            diagnostic,
            "{}",
            describe_output(&output)
        );
    }
}

#[test]
fn nested_owners_and_partial_records_close_started_generators() {
    run_generator(include_str!("../../tests/core-acceptance/cases/generator-nested-values.hew"), "record\nrecord cleaned\nenum\nenum cleaned\nclosure\nclosure cleaned\npartial\nEXTRACTED\npartial cleaned\nlazy\nlazy cleaned\ndone\n", 0, "");
}

#[test]
fn yielded_and_unobserved_returned_owners_close_recursively() {
    run_generator(
        include_str!("../../tests/core-acceptance/cases/generator-nested-outputs.hew"),
        "yielded\nreceived\nyielded cleaned\nreturned\nfinished\nreturned cleaned\ndone\n",
        0,
        "",
    );
}
