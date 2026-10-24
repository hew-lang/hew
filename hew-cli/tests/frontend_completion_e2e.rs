mod support;

use std::process::Command;

use support::{describe_output, hew_binary, run_bounded_command};

#[test]
fn completed_source_forms_execute_at_both_optimization_levels() {
    support::require_codegen();
    let dir = support::tempdir();
    for (name, expected) in [
        ("qualified-record-bindings", "let 7\nfor 8\nselect 9\n"),
        (
            "irrefutable-iteration-bindings",
            "tuple 7\nrecord 8\nunit\nunit\nrange\nrange\n1\n3\n",
        ),
        (
            "selected-product-patterns",
            "tuple 7\nrecord 8\ndiscarded\nreleased 9\nprepared unit\nunit\nprepared unit\n",
        ),
        ("whole-owner-index", "1\n2\n2\n"),
        ("lifecycle-function-values", "stopped\nterminated\nother\n"),
        (
            "scalar-constant-composition",
            "131\n124\n1\n1\n6.5\n0\ntrue\ntrue\n42\nconstant\n",
        ),
        ("associated-record-constructor", "ready 42\n"),
        ("execution-context-readers", "true\n"),
        (
            "intrinsic-function-values",
            "stored\npassed\n42\nowned\ninferred\nresumed\n",
        ),
        (
            "scalar-pattern-composition",
            "five\nother\nnegative\ntimed five\ntimed other\nunit\neffect\ntrue\n",
        ),
        (
            "generic-numeric-bounds",
            "9 24\n9 24\n9 24\n0 1\n43\nfirst\nsecond\n",
        ),
        (
            "guarded-aggregate-match",
            "equal kept\nother fallback 2 4\nlarge selected 8\nempty\nunpacked 7\nempty unpack\n",
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

#[test]
fn generic_numeric_arithmetic_keeps_concrete_overflow_semantics() {
    support::require_codegen();
    let dir = support::tempdir();
    let source =
        support::repo_root().join("tests/core-acceptance/cases/generic-numeric-overflow.hew");
    for level in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("overflow-{level}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&source)
            .args(["--opt-level", level, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build generic overflow O{level}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output = run_bounded_command(
            Command::new(binary),
            format!("run generic overflow O{level}"),
        );
        assert_eq!(
            output.status.code(),
            Some(1),
            "{}",
            describe_output(&output)
        );
        assert!(output.stdout.is_empty(), "{}", describe_output(&output));
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            "hew: failure: IntegerOverflow (201)\n"
        );
    }
}

#[test]
fn checked_extern_function_values_execute_through_ordinary_callables() {
    support::require_codegen();
    let dir = support::tempdir();
    let source = dir.path().join("main.hew");
    std::fs::write(&source, "extern \"C\" { fn abs(value: i32) -> i32; } fn main() { let absolute: fn(i32) -> i32 = unsafe { abs }; println(absolute(-7)); }").expect("write extern callback source");
    for level in ["0", "2"] {
        let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("extern-{level}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&source)
            .args(["--opt-level", level, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build extern callback O{level}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output = run_bounded_command(
            Command::new(binary),
            format!("run extern callback O{level}"),
        );
        assert!(output.status.success(), "{}", describe_output(&output));
        assert_eq!(String::from_utf8_lossy(&output.stdout), "7\n");
    }
}

#[test]
fn unavailable_constant_and_structural_forms_fail_before_artifact_emission() {
    let dir = support::tempdir();
    for (name, source, detail) in [
        ("open-range", "fn main() { let values = ..4; }", "an open range requires an indexed receiver"),
        ("enum-update", "enum Choice { Data { value: i64; } Empty; } fn main() { let base: Choice = .Data { value: 1 }; let value: Choice = .Data { value: 2, ..base }; }", "enum variant constructors do not support record update syntax"),
        ("runtime-constant", "fn number() -> i64 { 3 } const VALUE: i64 = number(); fn main() {}", "constant initializer contains an ordinary function call"),
        ("legacy-supervisor", "fn main() { supervisor_stop(0); }", "supervisor_stop` was replaced"),
        ("legacy-child", "fn main() { let get: fn(i64, i64) -> i64 = supervisor_child; }", "supervisor_child` was replaced"),
        ("legacy-close", "fn main() { let release: fn(i64) -> () = close; }", "close` was replaced"),
        ("affine-index-writeback", "#[resource] type Token { id: i64; } impl Token { fn close(consume self) {} } #[resource] type Bag { rows: Vec<Vec<Token>>; } impl Bag { fn close(consume self) {} } fn replace(var bag: Bag) { bag.rows[0][0] = Token { id: 2 }; } fn main() {}", "cannot update through a borrowed affine collection element"),
    ] {
        let input = dir.path().join(format!("{name}.hew"));
        std::fs::write(&input, source).expect("write source boundary fixture");
        let artifact = hew_testutil::compiled_binary_path(dir.path(), name);
        let mut build = Command::new(hew_binary());
        build.arg("build").arg(input).arg("-o").arg(&artifact).args(["--format", "json"]);
        let output = run_bounded_command(build, format!("reject {name}"));
        assert!(!output.status.success(), "{}", describe_output(&output));
        assert!(!artifact.exists(), "rejected source emitted an artifact: {name}");
        let diagnostics = String::from_utf8_lossy(&output.stdout);
        assert!(diagnostics.contains(detail), "{diagnostics}");
        let structured: Vec<serde_json::Value> = serde_json::from_slice(&output.stdout).expect("valid JSON diagnostics");
        assert!(structured.iter().all(|diagnostic| diagnostic["channel"] == "user"), "{diagnostics}");
    }
}

#[test]
fn refutable_iteration_and_selection_bindings_fail_before_artifact_emission() {
    let dir = support::tempdir();
    for (name, source, context) in [
        ("literal-for", "fn main() { for 1 in 0..3 {} }", "for"),
        ("variant-for", "fn main() { let values: Vec<Option<i64>> = [.Some(1), .None]; for .Some(value) in (values) {} }", "for"),
        ("nested-for", "type Entry { value: Option<i64>; } fn main() { for Entry { value: .Some(value) } in [Entry { value: .None }] {} }", "for"),
        ("literal-select", "fn number() -> i64 { 1 } fn main() { select { 1 from number() => {} }; }", "select"),
        ("variant-select", "fn number() -> Option<i64> { .Some(1) } fn main() { select { .Some(value) from number() => {} }; }", "select"),
        ("nested-select", "fn number() -> (Option<i64>, i64) { (.None, 2) } fn main() { select { (.Some(value), other) from number() => {} }; }", "select"),
    ] {
        let path = dir.path().join(format!("{name}.hew"));
        std::fs::write(&path, source).unwrap();
        let binary = hew_testutil::compiled_binary_path(dir.path(), name);
        let output = Command::new(hew_binary()).arg("build").arg(&path).args(["--format", "json", "-o"]).arg(&binary).output().unwrap();
        assert!(!output.status.success(), "{}", describe_output(&output));
        assert!(!binary.exists(), "invalid binding emitted an artifact");
        let diagnostics = String::from_utf8_lossy(&output.stdout);
        assert!(diagnostics.contains(&format!("`{context}` requires an irrefutable binding")), "{diagnostics}");
        assert!(diagnostics.contains("if let") && diagnostics.contains("match"), "{diagnostics}");
        assert!(!diagnostics.contains("internal"), "{diagnostics}");
    }
}

#[test]
fn invalid_typed_embeds_keep_their_primary_diagnostic_without_constant_cascades() {
    let dir = support::tempdir();
    for name in [
        "typed-embed-custom-eq",
        "typed-embed-duplicate",
        "typed-embed-mismatch",
        "typed-embed-syntax",
    ] {
        let input = support::repo_root().join(format!("tests/core-acceptance/cases/{name}.hew"));
        let artifact = hew_testutil::compiled_binary_path(dir.path(), name);
        let output = Command::new(hew_binary())
            .arg("build")
            .arg(input)
            .arg("-o")
            .arg(&artifact)
            .args(["--format", "json"])
            .output()
            .unwrap();
        assert!(!output.status.success(), "{}", describe_output(&output));
        assert!(!artifact.exists(), "invalid embed emitted an artifact");
        let diagnostics: Vec<serde_json::Value> = serde_json::from_slice(&output.stdout).unwrap();
        assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
        assert!(
            diagnostics[0]["message"]
                .as_str()
                .unwrap()
                .contains("E_EMBED_"),
            "{diagnostics:#?}"
        );
        assert_eq!(diagnostics[0]["channel"], "user");
    }
}

#[test]
fn qualified_record_bindings_reject_a_different_declaration_with_the_same_fields() {
    let dir = support::tempdir();
    for module in ["left", "right"] {
        std::fs::write(
            dir.path().join(format!("{module}.hew")),
            "pub type Entry { value: i64; }",
        )
        .unwrap();
    }
    for (name, body) in [
        (
            "let",
            "let right.Entry { value } = left.Entry { value: 7 };",
        ),
        (
            "for",
            "for right.Entry { value } in [left.Entry { value: 7 }] {}",
        ),
        (
            "select",
            "select { right.Entry { value } from make() => println(value), };",
        ),
    ] {
        let source = format!("import left; import right; fn make() -> left.Entry {{ left.Entry {{ value: 7 }} }} fn main() {{ {body} }}");
        let input = dir.path().join(format!("{name}.hew"));
        std::fs::write(&input, source).unwrap();
        let artifact = hew_testutil::compiled_binary_path(dir.path(), name);
        let output = Command::new(hew_binary())
            .arg("build")
            .arg(input)
            .args(["-o"])
            .arg(&artifact)
            .args(["--format", "json"])
            .output()
            .unwrap();
        assert!(!output.status.success(), "{}", describe_output(&output));
        assert!(!artifact.exists());
        let diagnostics: Vec<serde_json::Value> = serde_json::from_slice(&output.stdout).unwrap();
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic["channel"] == "user"),
            "{diagnostics:#?}"
        );
        assert!(
            diagnostics
                .iter()
                .any(
                    |diagnostic| diagnostic["message"]
                        .as_str()
                        .is_some_and(|message| message.contains("right.Entry")
                            && message.contains("left.Entry"))
                ),
            "{diagnostics:#?}"
        );
    }
}
