mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

#[test]
fn private_source_symbols_do_not_interpose_native_file_io() {
    require_codegen();
    let dir = tempdir();
    let input = dir.path().join("private_names.hew");
    std::fs::write(dir.path().join("input.bin"), b"native file contents\n").unwrap();
    std::fs::write(
        &input,
        r#"import std.fs;
            pub fn open(value: i64) -> i64 { value + 1 }
            pub fn read(value: i64) -> i64 { value + 2 }
            fn main_body(value: i64) -> i64 { value + 3 }
            fn identity<T>(value: T) -> T { value }
            fn main() -> i64 {
                let named: fn(i64) -> i64 = read;
                let anonymous = |value: i64| value + 4;
                if open(5) != 6 { return 1; }
                if named(5) != 7 { return 2; }
                if identity(main_body(5)) != 8 { return 3; }
                if anonymous(5) != 9 { return 4; }
                match fs.read("input.bin") {
                    .Ok(text) => { if text != "native file contents\n" { return 5; } },
                    .Err(_) => return 6,
                }
                println("private source calls and native file I/O");
                0
            }
        "#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let output_dir = dir.path().join(format!("o{level}"));
        std::fs::create_dir(&output_dir).unwrap();
        let binary = hew_testutil::compiled_binary_path(&output_dir, "private_names");
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .arg("--opt-level")
            .arg(level)
            .arg("--emit-llvm")
            .arg("-o")
            .arg(&binary)
            .current_dir(dir.path());
        let output = run_bounded_command(build, format!("compile private names O{level}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let llvm = std::fs::read_to_string(output_dir.join("private_names.ll")).unwrap();
        for native in ["open", "read", "main_body"] {
            assert!(
                !llvm.lines().any(
                    |line| line.starts_with("define ") && line.contains(&format!("@{native}("))
                ),
                "source body exposed the native symbol {native}"
            );
        }
        let mut run = Command::new(&binary);
        run.current_dir(dir.path());
        let output = run_bounded_command(run, format!("execute private names O{level}"));
        assert_eq!(
            output.status.code(),
            Some(0),
            "{}",
            describe_output(&output)
        );
        assert_eq!(output.stdout, b"private source calls and native file I/O\n");
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
    }
}

#[test]
fn file_resources_refuse_copy_and_borrowed_or_repeated_consumption() {
    require_codegen();
    let dir = tempdir();
    for (name, body, expected_exit, expected_diagnostic) in [
        ("consumed", "let input = open(); input.close(); input.collect();", 1,
            "use of moved value `input`"),
        ("cloned", "let input = open(); let _copy = clone input;", 1,
            "no method `clone` on `Stream<string>`"),
        ("record", "let reader = Reader { input: open() }; let _copy = clone reader;", 1,
            "type `Reader` cannot be cloned because member `input` of type `Stream<string>` has no Clone capability"),
        ("capture", "let input = open(); let cb = move || input.collect(); let _copy = clone cb;", 1,
            "type `fn[once]() -> string` cannot be cloned"),
        ("borrowed", "read_borrowed(open());", 3,
            "E_OWN_CONSUME_BORROWED: a consuming argument requires an owned value"),
        ("declared", "let _custom = Custom { value: 1 };", 3,
            "nested type `Custom` has no semantic value contract"),
    ] {
        let extra_declaration = match name {
            "borrowed" => "fn read_borrowed(input: Stream<string>) { input.close(); }",
            "declared" => "#[resource] type Custom { value: i64 } impl Custom { fn close(consume self) { println(self.value); } }",
            _ => "",
        };
        let source = format!(r#"import std.stream;
            type Reader {{ input: Stream<string> }}
            fn open() -> Stream<string> {{ match stream.from_file("resource.txt") {{
                .Ok(input) => input, .Err(_) => panic("open failed"),
            }} }}
            {extra_declaration}
            fn main() {{ {body} }}
        "#);
        let input = dir.path().join(format!("{name}.hew"));
        let binary = hew_testutil::compiled_binary_path(dir.path(), name);
        std::fs::write(&input, source).unwrap();
        let mut build = Command::new(hew_binary());
        build.arg("build").arg(&input).arg("-o").arg(&binary).current_dir(dir.path());
        let output = run_bounded_command(build, format!("reject resource {name}"));
        assert_eq!(output.status.code(), Some(expected_exit), "{name}: {}", describe_output(&output));
        assert!(!binary.exists(), "emitted refused {name}");
        let diagnostic = String::from_utf8_lossy(&output.stderr);
        assert!(diagnostic.contains(expected_diagnostic), "{name}: expected {expected_diagnostic:?}, got {diagnostic}");
    }
}
