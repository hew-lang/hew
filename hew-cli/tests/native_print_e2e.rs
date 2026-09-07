//! Primitive print calls retain their type, newline choice and string loan.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn primitive_prints_preserve_bits_order_and_embedded_nul() {
    let source = r#"
fn main() {
    let signed32: i32 = -2147483648;
    let signed64: i64 = -9223372036854775808;
    let byte: u8 = 255;
    let unsigned32: u32 = 4294967295;
    let zero: u64 = 0;
    let unsigned64 = ~zero;
    let fraction: f64 = 1.25;
    print(signed32); print("|"); println(signed32);
    print(signed64); print("|"); println(signed64);
    print(byte); print("|"); println(byte);
    print(unsigned32); print("|"); println(unsigned32);
    print(unsigned64); print("|"); println(unsigned64);
    print(fraction); print("|"); println(fraction);
    print(false); print("|"); println(true);
    let message = "é\0fin";
    print(message); print("|"); println(message);
    println(message.len());
    print_str("direct|"); println_str(message);
}
"#;
    let expected = concat!(
        "-2147483648|-2147483648\n",
        "-9223372036854775808|-9223372036854775808\n",
        "255|255\n",
        "4294967295|4294967295\n",
        "18446744073709551615|18446744073709551615\n",
        "1.25|1.25\n",
        "false|true\n",
        "é\0fin|é\0fin\n",
        "5\n",
        "direct|é\0fin\n",
    );
    check_print_output(source, expected);
}

#[test]
fn a_source_function_keeps_its_identity_when_named_like_a_print_builtin() {
    check_print_output(
        r#"
fn print_str(value: string) { println("user: " + value); }
fn main() { print_str("kept"); }
"#,
        "user: kept\n",
    );
}

fn check_print_output(source: &str, expected: &str) {
    require_codegen();
    let directory = tempdir();
    let input = directory.path().join("print.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(directory.path(), &format!("print-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build primitive print O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output =
            run_bounded_command(Command::new(binary), format!("run primitive print O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
        assert_eq!(output.stdout, expected.as_bytes());
    }
}
