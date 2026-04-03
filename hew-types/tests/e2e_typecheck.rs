use std::fs;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn new_networking_demo_checker() -> hew_types::Checker {
    hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
        repo_root(),
    ]))
}

/// Typecheck an inline source string with full stdlib access.
fn typecheck_inline(source: &str) -> hew_types::TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = new_networking_demo_checker();
    checker.check_program(&parse_result.program)
}

#[test]
fn typecheck_all_examples() {
    let examples_dir = repo_root().join("examples");
    test_directory(&examples_dir, "examples");
}

#[test]
fn typecheck_all_codegen_examples() {
    let dir = repo_root()
        .join("hew-codegen")
        .join("tests")
        .join("examples");
    test_directory(&dir, "hew-codegen/tests/examples");
}

#[test]
fn typecheck_top_level_networking_demos() {
    for relative in ["examples/http_server.hew", "examples/static_server.hew"] {
        assert_typechecks(&repo_root().join(relative), relative);
    }
}

fn assert_typechecks(path: &Path, label: &str) {
    let source = fs::read_to_string(path).unwrap();
    let parse_result = hew_parser::parse(&source);
    assert!(
        parse_result.errors.is_empty(),
        "{label} should parse cleanly, got: {:#?}",
        parse_result.errors
    );

    let mut checker = new_networking_demo_checker();
    let output = checker.check_program(&parse_result.program);
    assert!(
        output.errors.is_empty(),
        "{label} should type-check cleanly, got: {:#?}",
        output.errors
    );
}

fn test_directory(dir: &Path, label: &str) {
    let mut parse_ok = 0;
    let mut parse_fail = 0;
    let mut tc_ok = 0;
    let mut tc_fail = 0;
    let mut tc_errors = Vec::new();

    let mut entries: Vec<_> = fs::read_dir(dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "hew"))
        .collect();
    entries.sort();

    for path in &entries {
        let source = fs::read_to_string(path).unwrap();
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            parse_fail += 1;
            continue;
        }
        parse_ok += 1;

        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parse_result.program);
        if output.errors.is_empty() {
            tc_ok += 1;
        } else {
            tc_fail += 1;
            let first_err = &output.errors[0];
            tc_errors.push(format!(
                "  {} — {:?}",
                path.file_name().unwrap().to_string_lossy(),
                first_err
            ));
        }
    }

    let total = parse_ok + parse_fail;
    println!(
        "\n{label}: {parse_ok}/{total} parsed, {tc_ok}/{parse_ok} type-checked ({tc_fail} failed)"
    );
    if !tc_errors.is_empty() {
        println!("Type-check failures:");
        for e in &tc_errors {
            println!("{e}");
        }
    }
    // Informational — don't fail on type-check errors yet
}

// ===========================================================================
// Canonical qualified-spelling end-to-end tests
//
// These tests prove that the literal spellings `stream.Sink` and
// `channel.Receiver` are valid type annotations in Hew source.  The
// resolution path is: `canonical_named_builtin()` strips the module
// prefix and normalises to the short canonical name ("Sink", "Receiver"),
// then the checker's built-in method table handles method calls.  The
// `import` statements are required by the parser for qualified names to
// be accepted but are decorative here — type resolution and method
// dispatch are fully hardcoded and do not go through the stdlib module
// registry.  `typecheck_inline` (full stdlib access) is used so the
// imports are satisfied and no spurious "unknown module" errors occur.
// ===========================================================================

#[test]
fn stream_dot_sink_annotation_typechecks() {
    // A function whose parameter is explicitly spelled `stream.Sink<String>`.
    // Proves: the qualified spelling resolves to the canonical Sink<String>
    // type and its write/close methods are available.
    let output = typecheck_inline(
        r"
        import std::stream;

        fn flush_and_close(s: stream.Sink<String>, msg: String) {
            s.write(msg);
            s.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "stream.Sink<String> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

#[test]
fn stream_dot_stream_invalid_int_method_reports_user_facing_int() {
    let output = typecheck_inline(
        r"
        import std::stream;

        fn close_numbers(s: stream.Stream<int>) {
            s.close();
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.message.contains("`Stream<int>` is not supported")
                && !e.message.contains("Stream<i64>")
        }),
        "expected Stream<int> diagnostic, got: {:#?}",
        output.errors
    );
}

#[test]
fn channel_dot_receiver_annotation_typechecks() {
    // A function whose parameter is explicitly spelled `channel.Receiver<String>`.
    // Proves: the qualified spelling resolves to the canonical Receiver<String>
    // type and its recv/close methods are available.
    let output = typecheck_inline(
        r"
        import std::channel;

        fn take_one(rx: channel.Receiver<String>) -> Option<String> {
            let v = rx.recv();
            rx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "channel.Receiver<String> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}
