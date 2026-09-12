mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn empty_enum_reports_copy_iterate_and_drop_at_o0_o2() {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("empty-enum.hew");
    std::fs::write(
        &input,
        include_str!("../../tests/core-acceptance/cases/empty-enum-values.hew"),
    )
    .unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("empty-enum-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .arg("--opt-level")
            .arg(opt)
            .arg("-o")
            .arg(&binary);
        let output = run_bounded_command(build, format!("build empty enum O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output = run_bounded_command(Command::new(&binary), format!("run empty enum O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "ORIGINAL\nchanged\n"
        );
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
    }
}
