mod support;

use std::process::Command;
use support::{describe_output, hew_binary, run_bounded_command, tempdir};

#[test]
fn deferred_bodies_reject_escaping_exits_before_codegen() {
    let dir = tempdir();
    let cases = [
        ("return", "fn main() { defer { return; } }"),
        ("break", "fn main() { loop { defer { break; } break; } }"),
        ("continue", "fn main() { for i in 0..2 { defer { continue; } } }"),
        ("try", "fn bad() -> Result<i64, string> { defer { let n = Ok(4)?; } Ok(1) } fn main() { let result = bad(); }"),
        ("scope", "fn main() { defer { scope {} } }"),
    ];
    for (name, source) in cases {
        let input = dir.path().join(format!("{name}.hew"));
        std::fs::write(&input, source).unwrap();
        let mut check = Command::new(hew_binary());
        check.arg("check").arg(input);
        let output = run_bounded_command(check, format!("reject deferred {name}"));
        assert!(!output.status.success(), "{}", describe_output(&output));
        let diagnostic = String::from_utf8_lossy(&output.stderr);
        assert!(
            diagnostic.contains("deferred body"),
            "{}",
            describe_output(&output)
        );
        assert!(
            !diagnostic.contains("internal compiler error"),
            "{}",
            describe_output(&output)
        );
    }
}
