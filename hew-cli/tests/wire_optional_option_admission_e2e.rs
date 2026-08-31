mod support;

use std::process::Command;

use support::{hew_binary, strip_ansi};

#[test]
fn wire_optional_field_without_option_fails_before_lowering() {
    let fixture = support::tempdir();
    let source = fixture.path().join("wire_optional_scalar.hew");
    std::fs::write(
        &source,
        "#[wire]\ntype Message { body: string @1 optional }\n",
    )
    .expect("write wire fixture");

    let output = Command::new(hew_binary())
        .args(["check", source.to_str().expect("fixture path is UTF-8")])
        .current_dir(fixture.path())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "a bare optional field must stop before lowering; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("E_WIRE_OPTIONAL_REQUIRES_OPTION")
            && stderr
                .contains("wire field `body` is marked `optional` but must have type `Option<T>`")
            && stderr.contains("wire_optional_scalar.hew:2:22"),
        "CLI must render the semantic wire admission error at the field type:\n{stderr}"
    );
}

#[test]
fn wire_optional_field_without_option_leaves_no_codegen_artifact() {
    let fixture = support::tempdir();
    let source = fixture.path().join("wire_optional_scalar.hew");
    let emit_dir = fixture.path().join("emit");
    std::fs::write(
        &source,
        "#[wire]\ntype Message { body: string @1 optional }\n",
    )
    .expect("write wire fixture");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.to_str().expect("emit path is UTF-8"),
            source.to_str().expect("fixture path is UTF-8"),
        ])
        .current_dir(fixture.path())
        .output()
        .expect("invoke hew compile");

    assert!(
        !output.status.success(),
        "invalid presence must reject compile"
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("E_WIRE_OPTIONAL_REQUIRES_OPTION"),
        "compile must retain the admission diagnostic:\n{stderr}"
    );
    assert!(
        !emit_dir.exists()
            || std::fs::read_dir(&emit_dir)
                .expect("read emit directory")
                .next()
                .is_none(),
        "checker rejection must leave no MIR, LLVM, object, or native artifact"
    );
}

#[test]
fn wire_optional_option_counterfactual_controls_check_cleanly() {
    let fixture = support::tempdir();
    let source = fixture.path().join("wire_optional_controls.hew");
    std::fs::write(
        &source,
        "#[wire]\ntype RequiredValue { body: string @1 }\n\
         #[wire]\ntype RequiredOption { body: Option<string> @1 }\n\
         #[wire]\ntype OptionalOption { body: Option<string> @1 optional }\n\
         fn main() -> i64 {\n\
             let _value = RequiredValue { body: \"required\" };\n\
             let _required_option = RequiredOption { body: Some(\"present\") };\n\
             let _optional_option = OptionalOption { body: None };\n\
             0\n\
         }\n",
    )
    .expect("write wire control fixture");

    let output = Command::new(hew_binary())
        .args(["check", source.to_str().expect("fixture path is UTF-8")])
        .current_dir(fixture.path())
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "required T, required Option<T>, and optional Option<T> must remain valid; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn wire_optional_option_declaration_checks_without_value_use() {
    let fixture = support::tempdir();
    let source = fixture.path().join("wire_optional_declaration.hew");
    std::fs::write(
        &source,
        "#[wire]\ntype Message { body: Option<string> @1 optional }\n",
    )
    .expect("write declaration-only wire fixture");

    let output = Command::new(hew_binary())
        .args(["check", source.to_str().expect("fixture path is UTF-8")])
        .current_dir(fixture.path())
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "a declaration-only optional Option<T> must register its value layout; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}
