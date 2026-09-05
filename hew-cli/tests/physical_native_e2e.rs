mod support;

use std::process::Command;

use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn owned_string_and_bytes_calls_exit_through_physical_native_entry() {
    require_codegen();

    let dir = tempdir();
    let source = dir.path().join("owned_calls.hew");
    std::fs::write(
        &source,
        r#"fn copy_string(value: string) -> string {
    value
}

fn copy_bytes(value: bytes) -> bytes {
    value
}

fn main() -> i64 {
    copy_string("hello");
    copy_bytes(b"ok");
    23
}
"#,
    )
    .expect("write physical native source");

    let build = run_bounded_command(
        {
            let mut command = Command::new(hew_binary());
            command.arg("build").arg(&source).current_dir(dir.path());
            command
        },
        format!("hew build {}", source.display()),
    );

    assert!(
        build.status.success(),
        "physical native build failed\n{}",
        describe_output(&build),
    );
    assert!(
        !dir.path().join("owned_calls.ll").exists(),
        "a normal native build must not leave diagnostic LLVM IR"
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "owned_calls");
    let output = run_bounded_command(Command::new(&binary), format!("run {}", binary.display()));

    assert_eq!(
        output.status.code(),
        Some(23),
        "physical native program did not preserve the authored entry result\n{}",
        describe_output(&output),
    );
    assert!(output.stdout.is_empty(), "unexpected program stdout");
    assert!(
        output.stderr.is_empty(),
        "unexpected compiler/runtime stderr"
    );
}

#[test]
fn owned_program_preserves_exit_across_compile_build_and_run() {
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("entry.hew");
    std::fs::write(
        &source,
        "fn copy(value: string) -> string { value }\nfn main() -> i64 { copy(\"hello\"); 23 }\n",
    )
    .unwrap();
    for release in [false, true] {
        let output = dir.path().join(if release { "release" } else { "debug" });
        let mut command = Command::new(hew_binary());
        command.arg("build").arg(&source).arg("-o").arg(&output);
        if release {
            command.arg("--release");
        }
        let build = run_bounded_command(command, "build owned entry");
        assert!(build.status.success(), "{}", describe_output(&build));
        let result = run_bounded_command(Command::new(&output), "execute owned entry");
        assert_eq!(
            result.status.code(),
            Some(23),
            "{}",
            describe_output(&result)
        );
        assert!(result.stdout.is_empty() && result.stderr.is_empty());
    }
    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg(&source)
        .arg("--emit-dir")
        .arg(dir.path());
    let build = run_bounded_command(compile, "compile owned entry");
    assert!(build.status.success(), "{}", describe_output(&build));
    let result = run_bounded_command(
        Command::new(hew_testutil::compiled_binary_path(dir.path(), "entry")),
        "execute compiled entry",
    );
    assert_eq!(
        result.status.code(),
        Some(23),
        "{}",
        describe_output(&result)
    );
    let mut run = Command::new(hew_binary());
    run.arg("run").arg(&source);
    let result = run_bounded_command(run, "run owned entry");
    assert_eq!(
        result.status.code(),
        Some(23),
        "{}",
        describe_output(&result)
    );
    assert!(result.stdout.is_empty());
}

#[test]
fn object_emission_skips_linking_and_selected_test_ignores_main() {
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("selected_test.hew");
    std::fs::write(&source, "fn copy(value: string) -> string { value }\nfn main() -> i64 { 99 }\n#[test]\nfn selected() { copy(\"hello\"); }\n").unwrap();
    let object = dir.path().join("entry.o");
    let mut command = Command::new(hew_binary());
    command
        .arg("build")
        .arg(&source)
        .arg("--emit-obj")
        .arg("-o")
        .arg(&object);
    let result = run_bounded_command(command, "emit owned object");
    assert!(result.status.success(), "{}", describe_output(&result));
    assert!(std::fs::metadata(object).unwrap().len() > 0);
    assert!(!hew_testutil::compiled_binary_path(dir.path(), "selected_test").exists());
    let mut command = Command::new(hew_binary());
    command.arg("test").arg(&source).arg("--no-color");
    let result = run_bounded_command(command, "run selected owned test");
    assert!(result.status.success(), "{}", describe_output(&result));
    assert!(String::from_utf8_lossy(&result.stdout).contains("test selected ... ok"));
}

#[test]
fn eval_owned_fragment_uses_the_shared_native_pipeline() {
    require_codegen();
    let mut command = Command::new(hew_binary());
    command.args(["eval", "let value = \"hello\"; let copied = value;"]);
    let result = run_bounded_command(command, "eval owned fragment");
    assert!(result.status.success(), "{}", describe_output(&result));
    assert!(result.stdout.is_empty(), "{}", describe_output(&result));
}

#[test]
fn retired_lowering_switch_is_rejected() {
    let mut command = Command::new(hew_binary());
    command.args(["build", "unused.hew", "--sir-lower"]);
    let result = run_bounded_command(command, "reject retired route flag");
    assert_eq!(
        result.status.code(),
        Some(2),
        "{}",
        describe_output(&result)
    );
    assert!(String::from_utf8_lossy(&result.stderr).contains("--sir-lower"));
}

#[test]
fn requested_sanitizer_instruments_the_emitted_object() {
    use object::{Object, ObjectSymbol};
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("sanitized.hew");
    std::fs::write(
        &source,
        "fn copy(value: string) -> string { value }\nfn main() -> i64 { copy(\"hello\"); 23 }\n",
    )
    .unwrap();
    for instrumented in [false, true] {
        let output = dir
            .path()
            .join(if instrumented { "asan.o" } else { "plain.o" });
        let mut command = Command::new(hew_binary());
        command
            .arg("build")
            .arg(&source)
            .arg("--emit-obj")
            .arg("-o")
            .arg(&output)
            .env("HEW_SANITIZE_ADDRESS", if instrumented { "1" } else { "0" });
        let result = run_bounded_command(command, "emit sanitizer object");
        assert!(result.status.success(), "{}", describe_output(&result));
        let bytes = std::fs::read(output).unwrap();
        let object = object::File::parse(bytes.as_slice()).unwrap();
        let has_asan = object.symbols().any(|symbol| {
            symbol.is_undefined() && symbol.name().is_ok_and(|name| name.contains("__asan_init"))
        });
        assert_eq!(
            has_asan, instrumented,
            "generated object must reflect the requested instrumentation"
        );
    }
}

#[test]
fn no_entry_object_contains_its_public_function() {
    use object::{Object, ObjectSymbol};
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("exports.hew");
    std::fs::write(&source, "pub fn answer() -> i64 { 42 }").unwrap();
    for release in [false, true] {
        let output = dir
            .path()
            .join(if release { "release.o" } else { "debug.o" });
        let mut command = Command::new(hew_binary());
        command
            .arg("build")
            .arg(&source)
            .arg("--emit-obj")
            .arg("-o")
            .arg(&output);
        if release {
            command.arg("--release");
        }
        let result = run_bounded_command(command, "emit no-entry public function");
        assert!(result.status.success(), "{}", describe_output(&result));
        let bytes = std::fs::read(output).unwrap();
        let object = object::File::parse(bytes.as_slice()).unwrap();
        assert!(
            object.symbols().any(|symbol| symbol.is_definition()
                && symbol
                    .name()
                    .is_ok_and(|name| name.trim_start_matches('_') == "answer")),
            "the object must define its selected public function"
        );
    }
}

#[test]
fn rootless_object_is_refused_without_creating_an_artifact() {
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("private.hew");
    let object = dir.path().join("private.o");
    std::fs::write(&source, "fn private_value() -> i64 { 42 }").unwrap();
    let mut command = Command::new(hew_binary());
    command
        .arg("build")
        .arg(&source)
        .arg("--emit-obj")
        .arg("-o")
        .arg(&object);
    let result = run_bounded_command(command, "refuse rootless object");
    assert_eq!(
        result.status.code(),
        Some(1),
        "{}",
        describe_output(&result)
    );
    assert!(String::from_utf8_lossy(&result.stderr).contains("E_NO_COMPILATION_ROOTS"));
    assert!(!object.exists());
}

#[test]
fn bytes_copy_mutation_preserves_the_original_at_both_optimization_levels() {
    assert_native_case(
        include_str!("../../tests/core-acceptance/cases/bytes-copy-mutate.hew"),
        0,
        b"original=65;copy=65,66\n",
        b"",
    );
}

#[test]
fn negative_bytes_index_takes_the_owned_cleanup_failure_edge() {
    assert_native_case(
        r#"fn main() -> i64 {
    let live_owner = "negative-cleanup".to_upper();
    let value = "A".to_bytes();
    value[-1];
    println(live_owner);
    0
}"#,
        205,
        b"",
        b"hew: failure: IndexOutOfBounds (205)\n",
    );
}

#[test]
fn past_end_bytes_index_takes_the_owned_cleanup_failure_edge() {
    assert_native_case(
        r#"fn main() -> i64 {
    let live_owner = "upper-cleanup".to_upper();
    let value = "A".to_bytes();
    value[1];
    println(live_owner);
    0
}"#,
        205,
        b"",
        b"hew: failure: IndexOutOfBounds (205)\n",
    );
}

fn assert_native_case(source: &str, exit: i32, stdout: &[u8], stderr: &[u8]) {
    require_codegen();
    let dir = tempdir();
    let source_path = dir.path().join("case.hew");
    std::fs::write(&source_path, source).unwrap();
    for release in [false, true] {
        let binary = hew_testutil::compiled_binary_path(
            dir.path(),
            if release { "release" } else { "debug" },
        );
        let mut command = Command::new(hew_binary());
        command
            .arg("build")
            .arg(&source_path)
            .arg("-o")
            .arg(&binary);
        if release {
            command.arg("--release");
        }
        let result = run_bounded_command(command, "build semantic behaviour case");
        assert!(result.status.success(), "{}", describe_output(&result));
        let result = run_bounded_command(Command::new(&binary), "execute semantic behaviour case");
        assert_eq!(
            result.status.code(),
            Some(exit),
            "{}",
            describe_output(&result)
        );
        assert_eq!(result.stdout, stdout, "{}", describe_output(&result));
        assert_eq!(result.stderr, stderr, "{}", describe_output(&result));
    }
}
