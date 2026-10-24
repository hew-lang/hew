mod support;

use std::process::Command;

use support::{describe_output, hew_binary, run_bounded_command};

#[test]
fn completed_source_forms_execute_at_both_optimization_levels() {
    support::require_codegen();
    let dir = support::tempdir();
    for (name, expected) in [
        ("associated-record-constructor", "ready 42\n"),
        (
            "guarded-aggregate-match",
            "equal kept\nother fallback 2 4\nlarge selected 8\nempty\n",
        ),
        (
            "range-adapter-composition",
            "first\nsecond\n8\n4\n0\n-7\n-1\n5\n9223372036854775806\n-1\n-9223372036854775808\n0\n",
        ),
    ] {
        let source = support::repo_root().join(format!("tests/core-acceptance/cases/{name}.hew"));
        for level in ["0", "2"] {
            let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("{name}-{level}"));
            let mut build = Command::new(hew_binary());
            build
                .arg("build")
                .arg(&source)
                .args(["--opt-level", level, "-o"])
                .arg(&binary);
            let output = run_bounded_command(build, format!("build {name} O{level}"));
            assert!(output.status.success(), "{}", describe_output(&output));
            let output = run_bounded_command(Command::new(binary), format!("run {name} O{level}"));
            assert!(output.status.success(), "{}", describe_output(&output));
            assert_eq!(
                String::from_utf8_lossy(&output.stdout),
                expected,
                "{name} O{level}"
            );
        }
    }
}

#[test]
fn associated_record_constructors_validate_the_selected_record_fields() {
    let dir = support::tempdir();
    let source = dir.path().join("main.hew");
    std::fs::write(&source, "type Packet { value: i64; }\ntrait Schema { type Item; }\ntype Provider {}\nimpl Schema for Provider { type Item = Packet; }\nfn main() { <Provider as Schema>.Item { value: \"wrong\" }; }\n")
        .expect("write invalid associated constructor");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(source)
        .args(["--format", "json"])
        .output()
        .expect("check associated record field");
    assert!(!output.status.success(), "{}", describe_output(&output));
    let diagnostics = String::from_utf8_lossy(&output.stdout);
    assert!(
        diagnostics.contains("expected `i64`, found `string`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("E_NOT_YET_IMPLEMENTED"),
        "{diagnostics}"
    );
}
