//! End-to-end tests for `--format=json` structured diagnostics.
//!
//! These exercise the CLI from a consumer's perspective: an agent runs
//! `hew check --format=json` and parses the JSON array of diagnostics, reads
//! the stable `code`/`severity`/`span`/`message`, and applies the
//! producer-owned `fixes`. They also guard the exit-code contract and the
//! Debug-payload leak fix.

mod support;

use std::fs;
use std::process::Command;

use serde_json::Value;
use support::{describe_output, hew_binary, strip_ansi};

fn write_fixture(source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("main.hew");
    fs::write(&path, source).expect("write fixture");
    (dir, path)
}

fn run(args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .output()
        .expect("hew binary must run")
}

/// Parse stdout as a JSON array of diagnostics, panicking with context on
/// failure so a malformed payload surfaces the actual bytes.
fn parse_json_array(output: &std::process::Output) -> Vec<Value> {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: Value = serde_json::from_str(&stdout).unwrap_or_else(|error| {
        panic!(
            "stdout must be a parseable JSON array; parse error: {error}\nstdout:\n{stdout}\n{}",
            describe_output(output)
        )
    });
    value
        .as_array()
        .unwrap_or_else(|| panic!("top-level JSON must be an array; got:\n{stdout}"))
        .clone()
}

#[test]
fn json_help_preserves_the_type_checkers_guidance() {
    let (_dir, path) = write_fixture("fn main() { let food = 42; println(foob); }");
    let json = run(&["check", "--format=json", path.to_str().unwrap()]);
    let text = run(&["check", path.to_str().unwrap()]);
    assert_eq!(json.status.code(), Some(1));
    assert_eq!(text.status.code(), Some(1));
    let diagnostics = parse_json_array(&json);
    let undefined = diagnostics
        .iter()
        .find(|diagnostic| diagnostic["code"] == "UndefinedVariable")
        .expect("undefined variable diagnostic");
    let help = undefined["help"].as_array().expect("structured help");
    assert!(!help.is_empty(), "the typo should offer guidance");
    let rendered = strip_ansi(&String::from_utf8_lossy(&text.stderr));
    for suggestion in help {
        assert!(rendered.contains(suggestion.as_str().unwrap()));
    }
}

#[test]
fn json_import_cycle_notes_locate_their_own_source_files() {
    let dir = support::tempdir();
    let first = dir.path().join("a.hew");
    let second = dir.path().join("b.hew");
    let second_source = "// 🦉\n\nimport \"a.hew\";\npub fn noop_b() {}\n";
    fs::write(&first, "import \"b.hew\";\npub fn noop_a() {}\n").unwrap();
    fs::write(&second, second_source).unwrap();
    let json = run(&["check", "--format=json", first.to_str().unwrap()]);
    assert_eq!(json.status.code(), Some(1));
    let diagnostics = parse_json_array(&json);
    let cycle = diagnostics
        .iter()
        .find(|diagnostic| diagnostic["code"] == "E_IMPORT_CYCLE")
        .expect("import cycle diagnostic");
    let note = cycle["notes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|note| {
            note["file"]
                .as_str()
                .is_some_and(|file| file.ends_with("b.hew"))
        })
        .expect("the other import edge has its own file");
    let span = &note["span"];
    assert_eq!(span["start_line"], 3);
    let start = usize::try_from(span["start_byte"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(span["end_byte"].as_u64().unwrap()).unwrap();
    assert!(second_source[start..end].contains("\"a.hew\""));
    assert!(!cycle["help"].as_array().unwrap().is_empty());
}

/// A type error emits a parseable JSON array carrying at least
/// `{code, severity, file, span, message}` and exits 1.
#[test]
fn check_and_compile_type_errors_emit_structured_json_and_exit_1() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"not a number\";\n}\n");
    for verb in ["check", "compile"] {
        let output = run(&[verb, "--format=json", path.to_str().unwrap()]);

        assert_eq!(
            output.status.code(),
            Some(1),
            "type error must exit 1\n{}",
            describe_output(&output),
        );

        let diagnostics = parse_json_array(&output);
        let mismatch = diagnostics
            .iter()
            .find(|d| d["code"] == "Mismatch")
            .expect("expected a Mismatch diagnostic");

        assert_eq!(mismatch["severity"], "error");
        assert!(
            mismatch["file"].as_str().unwrap().ends_with("main.hew"),
            "file field must name the source: {mismatch}",
        );
        let span = &mismatch["span"];
        assert_eq!(span["start_line"], 2, "span carries 1-based line: {span}");
        assert!(span["start_byte"].is_number(), "span carries byte offsets");
        assert!(
            mismatch["message"]
                .as_str()
                .unwrap()
                .contains("type mismatch"),
            "message carries human-readable prose: {mismatch}",
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        for forbidden in [
            "MirDiagnostic",
            "HirDiagnostic",
            "SiteId(",
            "owning_pass",
            "construct:",
        ] {
            assert!(
                !stdout.contains(forbidden),
                "{verb} JSON contains compiler internals `{forbidden}`: {stdout}",
            );
        }
    }
}

/// A clean program emits an empty JSON array and exits 0.
#[test]
fn check_clean_program_emits_empty_array_and_exits_0() {
    let (_dir, path) = write_fixture("fn main() {\n    println(\"hello\")\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);

    assert_eq!(
        output.status.code(),
        Some(0),
        "clean program must exit 0\n{}",
        describe_output(&output),
    );
    let diagnostics = parse_json_array(&output);
    assert!(
        diagnostics.is_empty(),
        "clean program must emit an empty diagnostic array; got: {diagnostics:?}",
    );
}

/// The code-action engine must surface a machine-checked fix in the JSON
/// `fixes` array — the core value for agentic consumers.
#[test]
fn check_json_carries_explicit_unused_binding_fix() {
    // An unused variable yields a "Prefix with `_`" quick-fix.
    let (_dir, path) = write_fixture("fn main() {\n    let unused = 1;\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);

    let diagnostics = parse_json_array(&output);
    let unused = diagnostics
        .iter()
        .find(|d| d["code"] == "UnusedVariable")
        .expect("expected an UnusedVariable diagnostic");

    let fixes = unused["fixes"].as_array().expect("fixes is an array");
    assert!(
        !fixes.is_empty(),
        "UnusedVariable must carry a producer-owned fix: {unused}",
    );
    assert_eq!(fixes[0]["applicability"], "maybe_incorrect");
    let edit = &fixes[0]["edits"][0];
    assert_eq!(
        edit["new_text"], "_unused",
        "fix edit must prefix the binding with `_`: {edit}",
    );
    assert!(
        edit["span"]["start_byte"].is_number(),
        "fix edit span must carry byte offsets so it can be applied: {edit}",
    );
}

/// `--format json` (space-separated) must be equivalent to `--format=json`.
#[test]
fn check_accepts_space_separated_format_value() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"oops\";\n}\n");
    let output = run(&["check", "--format", "json", path.to_str().unwrap()]);
    assert_eq!(output.status.code(), Some(1));
    let diagnostics = parse_json_array(&output);
    assert!(
        diagnostics.iter().any(|d| d["code"] == "Mismatch"),
        "space-separated `--format json` must produce the same JSON",
    );
}

/// The text path must remain unchanged (no Debug payloads) and must not emit
/// JSON — guards that the default format is still human text.
#[test]
fn check_default_text_format_is_unchanged() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"oops\";\n}\n");
    let output = run(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(output.status.code(), Some(1));
    assert!(
        stderr.contains("type mismatch"),
        "text diagnostics still render on stderr; got:\n{stderr}",
    );
    assert!(
        stdout.trim().is_empty(),
        "default text mode must not emit JSON on stdout; got:\n{stdout}",
    );
}

/// `E_MODULE_NOT_FOUND` locates itself at the offending `import` (not a zero
/// span) and, for a std-module typo close to a real module, names the fix.
#[test]
fn module_not_found_has_span_and_suggestion() {
    let (_dir, path) = write_fixture("import std.htp;\nfn main() { }\n");
    let output = run(&["check", "--format", "json", path.to_str().unwrap()]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "module-not-found must exit 1\n{}",
        describe_output(&output),
    );

    let diagnostics = parse_json_array(&output);
    let not_found = diagnostics
        .iter()
        .find(|d| d["code"] == "E_MODULE_NOT_FOUND")
        .unwrap_or_else(|| panic!("expected an E_MODULE_NOT_FOUND diagnostic: {diagnostics:?}"));

    let span = &not_found["span"];
    assert!(
        span["start_line"].as_u64().unwrap_or(0) > 0,
        "module-not-found must carry the import's own span, not a zero span: {span}",
    );
    assert_eq!(
        span["start_line"], 1,
        "the bad `import` is on line 1 of the fixture: {span}",
    );
    assert!(
        not_found["message"]
            .as_str()
            .unwrap()
            .contains("std.net.http"),
        "a std-module typo close to `std.net.http` must suggest it: {not_found}",
    );
    assert!(
        not_found["file"].as_str().unwrap().ends_with("main.hew"),
        "file field must name the source: {not_found}",
    );
}

/// The right module name at the wrong nesting (`std.http` for `std.net.http`)
/// is the single likeliest real mistake — not a typo `find_similar` would
/// catch (the leaf spelling is exactly right), so it needs its own exact-leaf
/// match ahead of the fuzzy fallback.
#[test]
fn module_not_found_suggests_an_exact_leaf_match_at_the_wrong_nesting() {
    let (_dir, path) = write_fixture("import std.http;\nfn main() { }\n");
    let output = run(&["check", "--format", "json", path.to_str().unwrap()]);

    let diagnostics = parse_json_array(&output);
    let not_found = diagnostics
        .iter()
        .find(|d| d["code"] == "E_MODULE_NOT_FOUND")
        .unwrap_or_else(|| panic!("expected an E_MODULE_NOT_FOUND diagnostic: {diagnostics:?}"));
    assert!(
        not_found["message"]
            .as_str()
            .unwrap()
            .contains("std.net.http"),
        "the right leaf name at the wrong nesting must still suggest `std.net.http`: {not_found}",
    );
}

#[test]
fn check_fix_dry_run_then_apply_preserves_effects_and_runs_both_profiles() {
    support::require_codegen();
    let source = "// é😀\nfn effect() -> i64 { println(7); 3 } fn main() { let held = effect(); var n = 4; println(n); }";
    let (dir, path) = write_fixture(source);
    let dry = run(&[
        "check",
        "--fix",
        "--dry-run",
        "--format=json",
        path.to_str().unwrap(),
    ]);
    assert!(dry.status.success(), "{}", describe_output(&dry));
    let diagnostics = parse_json_array(&dry);
    let unused_mut = diagnostics
        .iter()
        .find(|diagnostic| diagnostic["code"] == "UnusedMut")
        .unwrap();
    let edit = &unused_mut["fixes"][0]["edits"][0];
    assert_eq!(
        unused_mut["fixes"][0]["applicability"],
        "machine_applicable"
    );
    let start = usize::try_from(edit["span"]["start_byte"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(edit["span"]["end_byte"].as_u64().unwrap()).unwrap();
    let expected = format!(
        "{}{}{}",
        &source[..start],
        edit["new_text"].as_str().unwrap(),
        &source[end..]
    );
    assert_eq!(fs::read(&path).unwrap(), source.as_bytes());
    let applied = run(&["check", "--fix", "--format=json", path.to_str().unwrap()]);
    assert!(applied.status.success(), "{}", describe_output(&applied));
    assert!(!parse_json_array(&applied)
        .iter()
        .any(|diagnostic| diagnostic["code"] == "UnusedMut"));
    assert_eq!(fs::read_to_string(&path).unwrap(), expected);
    assert!(
        expected.contains("let held = effect()"),
        "unused initializer and binding stay in scope"
    );
    for level in ["0", "2"] {
        let binary = dir
            .path()
            .join(format!("fixed-o{level}{}", std::env::consts::EXE_SUFFIX));
        let mut command = Command::new(hew_binary());
        command
            .current_dir(dir.path())
            .arg("build")
            .arg(&path)
            .args(["--opt-level", level, "-o"])
            .arg(&binary);
        let built = support::run_bounded_command(command, "build applied diagnostic fixes");
        assert!(built.status.success(), "{}", describe_output(&built));
        let output =
            support::run_bounded_command(Command::new(&binary), "run applied diagnostic fixes");
        assert!(output.status.success(), "{}", describe_output(&output));
        assert_eq!(String::from_utf8_lossy(&output.stdout), "7\n4\n");
    }
    let mut command = Command::new(hew_binary());
    command.current_dir(dir.path()).arg("run").arg(&path);
    let output = support::run_bounded_command(command, "hew run after applying fixes");
    assert!(output.status.success(), "{}", describe_output(&output));
    assert_eq!(String::from_utf8_lossy(&output.stdout), "7\n4\n");
}

#[test]
fn check_fix_routes_imported_owned_source_without_touching_root() {
    let dir = support::tempdir();
    fs::write(
        dir.path().join("hew.toml"),
        "[package]\nname = \"probe\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
    )
    .unwrap();
    let root = "import probe.helper; fn main() { println(helper.value()); }";
    let helper = "// imported é😀\npub fn value() -> i64 { var n = 4; n }";
    let main = dir.path().join("main.hew");
    let imported = dir.path().join("helper.hew");
    fs::write(&main, root).unwrap();
    fs::write(&imported, helper).unwrap();
    let dry = support::run_hew_in(
        dir.path(),
        &["check", "--fix", "--dry-run", "--format=json", "main.hew"],
    );
    assert!(dry.status.success(), "{}", describe_output(&dry));
    let diagnostics = parse_json_array(&dry);
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic["code"] == "UnusedMut")
        .unwrap();
    assert!(diagnostic["file"].as_str().unwrap().ends_with("helper.hew"));
    assert_eq!(
        diagnostic["source_hash"],
        hew_analysis::code_actions::source_hash(helper)
    );
    assert_eq!(fs::read(&imported).unwrap(), helper.as_bytes());
    let applied = support::run_hew_in(dir.path(), &["check", "--fix", "--format=json", "main.hew"]);
    assert!(applied.status.success(), "{}", describe_output(&applied));
    assert_eq!(fs::read(&main).unwrap(), root.as_bytes());
    assert_eq!(
        fs::read_to_string(imported).unwrap(),
        helper.replace("var n", "let n")
    );
}

#[test]
fn check_fix_keeps_path_dependency_diagnostics_informative() {
    let dir = support::tempdir();
    let app = dir.path().join("app");
    let dependency = dir.path().join("library");
    fs::create_dir_all(&app).unwrap();
    fs::create_dir_all(&dependency).unwrap();
    fs::write(app.join("hew.toml"), "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n[dependencies]\nfoo = { path = \"../library\" }\n").unwrap();
    fs::write(
        dependency.join("hew.toml"),
        "[package]\nname = \"foo\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
    )
    .unwrap();
    let source = "pub fn answer() -> i64 { var n = 40; n }";
    let external = dependency.join("foo.hew");
    fs::write(&external, source).unwrap();
    fs::write(
        app.join("main.hew"),
        "import foo; fn main() { var n = 2; println(n + foo.answer()); }",
    )
    .unwrap();
    let run_selected = |args: &[&str]| {
        let mut command = Command::new(hew_binary());
        command
            .current_dir(&app)
            .env("HEW_HOME", dir.path().join("hew-home"))
            .args(args);
        support::run_bounded_command(command, "check selected source ownership")
    };
    let installed = run_selected(&["install"]);
    assert!(
        installed.status.success(),
        "{}",
        describe_output(&installed)
    );
    let fixed = run_selected(&["check", "--fix", "--format=json", "main.hew"]);
    assert!(fixed.status.success(), "{}", describe_output(&fixed));
    let diagnostics = parse_json_array(&fixed);
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic["code"] == "UnusedMut"
                && diagnostic["source_hash"] == hew_analysis::code_actions::source_hash(source)),
        "{diagnostics:?}"
    );
    assert_eq!(fs::read(&external).unwrap(), source.as_bytes());
    assert!(fs::read_to_string(app.join("main.hew"))
        .unwrap()
        .contains("let n = 2"));
}

#[test]
fn check_fix_refuses_overlapping_producer_edits_without_writing() {
    let source = "fn main() { return; var n = 3; println(n); }";
    let (_dir, path) = write_fixture(source);
    let output = run(&["check", "--fix", "--format=json", path.to_str().unwrap()]);
    assert_eq!(
        output.status.code(),
        Some(1),
        "{}",
        describe_output(&output)
    );
    assert!(parse_json_array(&output)
        .iter()
        .any(|diagnostic| diagnostic["code"] == "E_FIX_REFUSED"
            && diagnostic["message"].as_str().unwrap().contains("overlap")));
    assert_eq!(fs::read(path).unwrap(), source.as_bytes());
}
