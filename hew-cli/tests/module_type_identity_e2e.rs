//! Source aliases keep their target semantics and diagnostics name visible declarations.

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, run_bounded_command};

#[test]
fn imported_aliases_execute_arithmetic_and_cross_actor_boundaries() {
    support::require_codegen();
    let dir = support::tempdir();
    std::fs::write(
        dir.path().join("counts.hew"),
        r"pub type Count = i64;
type Total = Count;
actor Counter {
    let value: Total;
    receive fn add(amount: Count) -> Count { value + amount }
}
pub fn total(value: Count) -> Total {
    let counter = spawn Counter { value: value };
    let result = match counter.add(1) { .Ok(answer) => answer, .Err(_) => 0 };
    let negated: Count = -result;
    (-negated as Count) + 1
}
",
    )
    .expect("write alias module");
    std::fs::write(
        dir.path().join("main.hew"),
        "import counts;\nfn main() { println(counts.total(40)); }\n",
    )
    .expect("write importer");
    for level in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("aliases-{level}"));
        let mut build = Command::new(hew_binary());
        build
            .current_dir(dir.path())
            .args(["build", "main.hew", "--opt-level", level, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build imported aliases O{level}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output = run_bounded_command(Command::new(binary), "run imported aliases");
        assert!(output.status.success(), "{}", describe_output(&output));
        assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
    }
}

#[test]
fn invisible_root_types_are_undefined_in_an_imported_module() {
    let dir = support::tempdir();
    std::fs::write(
        dir.path().join("hidden.hew"),
        "pub fn secret() { Secret.A; }\n",
    )
    .expect("write isolated module");
    std::fs::write(
        dir.path().join("main.hew"),
        "import hidden;\nenum Secret { A; }\nfn main() { hidden.secret(); }\n",
    )
    .expect("write root declaration");
    let output = Command::new(hew_binary())
        .current_dir(dir.path())
        .args(["check", "main.hew", "--format", "json"])
        .output()
        .expect("check isolated module");
    assert!(!output.status.success(), "{}", describe_output(&output));
    let diagnostics = String::from_utf8_lossy(&output.stdout);
    assert!(
        diagnostics.contains("undefined variable `Secret`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("cannot be used as a value"),
        "{diagnostics}"
    );
}

#[test]
fn ambiguous_same_named_traits_keep_their_module_names() {
    let dir = support::tempdir();
    for module in ["a", "b"] {
        std::fs::write(
            dir.path().join(format!("{module}.hew")),
            "pub trait Speak { fn speak(self) -> i64; }\n",
        )
        .expect("write trait module");
    }
    std::fs::write(
        dir.path().join("main.hew"),
        r"import a;
import b;
type Voice {}
impl a.Speak for Voice { fn speak(self) -> i64 { 1 } }
impl b.Speak for Voice { fn speak(self) -> i64 { 2 } }
fn main() { Voice {}.speak(); }
",
    )
    .expect("write ambiguous call");
    let output = Command::new(hew_binary())
        .current_dir(dir.path())
        .args(["check", "main.hew", "--format", "json"])
        .output()
        .expect("check ambiguous call");
    assert!(!output.status.success(), "{}", describe_output(&output));
    let diagnostics = String::from_utf8_lossy(&output.stdout);
    assert!(
        diagnostics.contains("`a.Speak`") && diagnostics.contains("`b.Speak`"),
        "{diagnostics}"
    );
}
