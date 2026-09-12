//! Source string parsing composes with automatic temporary cleanup.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn float_parsing_reuses_inputs_and_closes_early_return_temporaries() {
    require_codegen();
    let source = r#"
import std.string;

fn valid(text: string, expected: f64) {
    match string.to_float(text) {
        .Ok(value) => { if value != expected { panic("wrong float value"); } }
        .Err(_) => panic("valid float refused"),
    }
}
fn invalid(text: string) {
    match string.to_float(text) {
        .Ok(_) => panic("invalid float accepted"),
        .Err(message) => {
            if message != "string.to_float: invalid float literal" { panic("wrong float error"); }
        }
    }
}
fn main() {
    let text = "-1.25" + "e2";
    for _ in 0..20 {
        valid(text, -125.0);
        valid("+.75", 0.75);
        valid("1.", 1.0);
        valid("2E-2", 0.02);
        valid(".5", 0.5);
        invalid("");
        invalid("+");
        invalid("1.2.3");
        invalid("1e");
        invalid("1e+");
        invalid("1e2e3");
        invalid("1e2x");
        invalid("1雪");
    }
    println(text);
    println("parsed");
}
"#;
    let directory = tempdir();
    let input = directory.path().join("parse.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(directory.path(), &format!("parse-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build string parsing O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output =
            run_bounded_command(Command::new(binary), format!("run string parsing O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
        assert_eq!(output.stdout, b"-1.25e2\nparsed\n");
    }
}
