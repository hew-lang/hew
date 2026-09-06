mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

const CONTENTS: &str = "known file: héllo 🌲";

#[test]
fn file_resource_lifetimes_and_fault_cleanup_execute_at_both_optimization_levels() {
    require_codegen();
    let dir = tempdir();
    for name in [
        "read",
        "collect",
        "close",
        "scope",
        "return",
        "record",
        "capture-drop",
        "capture-call",
        "fault-record",
        "fault-capture",
        "fault-local",
        "fault-caller",
    ] {
        std::fs::write(dir.path().join(format!("resource-{name}.txt")), CONTENTS).unwrap();
    }
    std::fs::write(dir.path().join("resource-invalid.txt"), [b'o', b'k', 0xff]).unwrap();
    std::fs::write(dir.path().join("resource-empty.txt"), []).unwrap();
    std::fs::write(dir.path().join("resource-nul.txt"), b"a\0b").unwrap();
    let cases = [
        (
            "errors",
            include_str!("../../tests/core-acceptance/cases/file-resource-read-errors.hew"),
            0,
            "read results\n".to_string(),
            "",
        ),
        (
            "lifetimes",
            include_str!("../../tests/core-acceptance/cases/file-resource-lifetimes.hew"),
            0,
            format!("{CONTENTS}\n{CONTENTS}\n7\n{CONTENTS}\nreleased\n"),
            "",
        ),
        (
            "fault",
            include_str!("../../tests/core-acceptance/cases/file-resource-fault.hew"),
            202,
            String::new(),
            "hew: failure: DivideByZero (202)\n",
        ),
    ];
    for (name, source, exit, stdout, stderr) in cases {
        let input = dir.path().join(format!("{name}.hew"));
        std::fs::write(&input, source).unwrap();
        for opt in ["0", "2"] {
            let binary = hew_testutil::compiled_binary_path(dir.path(), &format!("{name}-{opt}"));
            let mut build = Command::new(hew_binary());
            build
                .arg("build")
                .arg(&input)
                .arg("--opt-level")
                .arg(opt)
                .arg("-o")
                .arg(&binary)
                .current_dir(dir.path());
            let compilation = run_bounded_command(build, format!("build resource {name} O{opt}"));
            assert!(
                compilation.status.success(),
                "{}",
                describe_output(&compilation)
            );
            let mut run = Command::new(&binary);
            run.current_dir(dir.path());
            let output = run_bounded_command(run, format!("execute resource {name} O{opt}"));
            assert_eq!(
                output.status.code(),
                Some(exit),
                "{}",
                describe_output(&output)
            );
            assert_eq!(String::from_utf8_lossy(&output.stdout), stdout);
            assert_eq!(String::from_utf8_lossy(&output.stderr), stderr);
        }
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
            "declared" => "#[resource] type Custom { value: i64 } impl Custom { fn close(consuming self) { println(self.value); } }",
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
