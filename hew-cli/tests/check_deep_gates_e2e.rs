mod support;

use std::fs;
use std::process::Command;

use serde_json::Value;
use support::{describe_output, hew_binary, require_codegen, strip_ansi};

fn write_fixture(source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("main.hew");
    fs::write(&path, source).expect("write fixture");
    (dir, path)
}

fn run_check(args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .output()
        .expect("hew binary must run")
}

fn sorted_dir_entries(dir: &std::path::Path) -> Vec<String> {
    let mut entries = fs::read_dir(dir)
        .expect("read temp dir")
        .map(|entry| {
            entry
                .expect("read temp dir entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect::<Vec<_>>();
    entries.sort();
    entries
}

fn assert_only_source_artifact(dir: &std::path::Path) {
    assert_eq!(
        sorted_dir_entries(dir),
        vec!["main.hew".to_string()],
        "hew check must not write codegen artifacts"
    );
}

fn assert_duplicate_impl_diagnostic(
    source: &str,
    code: &str,
    primary: (u64, u64),
    previous: (u64, u64),
) {
    for action in ["check", "build"] {
        for format in ["text", "json"] {
            let (dir, path) = write_fixture(source);
            let mut command = Command::new(hew_binary());
            command.arg(action).arg(&path).current_dir(dir.path());
            if action == "build" {
                command
                    .args(["--emit-obj", "-o"])
                    .arg(dir.path().join("result.o"));
            }
            command.args(["--format", format]);
            let output =
                support::run_bounded_command(command, format!("duplicate impl {action} {format}"));
            assert_eq!(
                output.status.code(),
                Some(1),
                "{}",
                describe_output(&output)
            );
            let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
            let stdout = String::from_utf8_lossy(&output.stdout);
            for forbidden in [
                "E_HIR",
                "CheckerBoundaryViolation",
                "internal compiler error",
            ] {
                assert!(
                    !stderr.contains(forbidden) && !stdout.contains(forbidden),
                    "{}",
                    describe_output(&output)
                );
            }
            if format == "json" {
                let diagnostics: Vec<Value> =
                    serde_json::from_slice(&output.stdout).expect("JSON diagnostics");
                let errors: Vec<_> = diagnostics
                    .iter()
                    .filter(|diagnostic| diagnostic["severity"] == "error")
                    .collect();
                assert_eq!(errors.len(), 1, "{diagnostics:#?}");
                let error = errors[0];
                assert_eq!(error["code"], code);
                assert_eq!(error["channel"], "user");
                assert_eq!(error["source"], "hew-types");
                assert_eq!(error["file"], path.to_str().expect("fixture path"));
                assert_eq!(error["span"]["start_line"], primary.0);
                assert_eq!(error["span"]["start_col"], primary.1);
                let notes = error["notes"].as_array().expect("diagnostic notes");
                assert_eq!(notes.len(), 1, "{error:#?}");
                assert_eq!(notes[0]["span"]["start_line"], previous.0);
                assert_eq!(notes[0]["span"]["start_col"], previous.1);
                assert!(notes[0]["message"]
                    .as_str()
                    .expect("note message")
                    .starts_with("previous "));
            } else {
                assert!(
                    stderr.contains(&format!(
                        "{}:{}:{}: error:",
                        path.display(),
                        primary.0,
                        primary.1
                    )),
                    "{stderr}"
                );
                assert!(
                    stderr.contains(&format!(
                        "{}:{}:{}: note: previous ",
                        path.display(),
                        previous.0,
                        previous.1
                    )),
                    "{stderr}"
                );
            }
            assert_only_source_artifact(dir.path());
        }
    }
}

#[test]
fn duplicate_impl_heads_report_user_errors_without_check_or_build_artifacts() {
    for second_body in ["\"r\"", "\"different\""] {
        assert_duplicate_impl_diagnostic(
            &format!(
                "type R {{}}\n\
                 impl Display for R {{ fn fmt(self) -> string {{ \"r\" }} }}\n\
                 impl Display for R {{ fn fmt(self) -> string {{ {second_body} }} }}\n\
                 fn main() {{}}"
            ),
            "ConflictingTraitImpl",
            (3, 1),
            (2, 1),
        );
    }
    assert_duplicate_impl_diagnostic(
        "trait Marker {}\n\
         type R {}\n\
         impl Marker for R {}\n\
         impl Marker for R {}\n\
         fn main() {}",
        "ConflictingTraitImpl",
        (4, 1),
        (3, 1),
    );
    assert_duplicate_impl_diagnostic(
        "trait Label { fn label(self) -> i64; }\n\
         type Box<T> { value: T; }\n\
         impl<T> Label for Box<T> { fn label(self) -> i64 { 1 } }\n\
         impl<U> Label for Box<U> { fn label(self) -> i64 { 2 } }\n\
         fn main() {}",
        "ConflictingTraitImpl",
        (4, 1),
        (3, 1),
    );
    for receiver in ["string", "NodeId", "Location"] {
        assert_duplicate_impl_diagnostic(
            &format!(
                "impl Display for {receiver} {{ fn fmt(self) -> string {{ \"first\" }} }}\n\
                 impl Display for {receiver} {{ fn fmt(self) -> string {{ \"second\" }} }}\n\
                 fn main() {{}}"
            ),
            "ConflictingTraitImpl",
            (2, 1),
            (1, 1),
        );
    }
}

#[test]
fn duplicate_impl_admission_preserves_explicit_prelude_overrides() {
    require_codegen();
    for receiver in ["string", "NodeId", "Location"] {
        for action in ["check", "build"] {
            let (dir, path) = write_fixture(&format!(
                "impl Display for {receiver} {{ fn fmt(self) -> string {{ \"override\" }} }}\n\
                 fn render(value: {receiver}) -> string {{ f\"{{value}}\" }}\n\
                 fn main() {{}}"
            ));
            let mut command = Command::new(hew_binary());
            command.arg(action).arg(&path).current_dir(dir.path());
            let artifact = dir.path().join("result.o");
            if action == "build" {
                command.args(["--emit-obj", "-o"]).arg(&artifact);
            }
            let output = support::run_bounded_command(
                command,
                format!("prelude override {receiver} {action}"),
            );
            assert!(output.status.success(), "{}", describe_output(&output));
            if action == "check" {
                assert_only_source_artifact(dir.path());
            } else {
                assert!(artifact.is_file(), "successful build must emit its object");
            }
        }
    }
}

#[test]
fn duplicate_impl_methods_report_definition_errors_without_artifacts() {
    for implementation in [
        "impl R {\n    fn f(self) -> i64 { 1 }\n    fn f(self) -> i64 { 2 }\n}",
        "impl Display for R {\n    fn fmt(self) -> string { \"a\" }\n    fn fmt(self) -> string { \"b\" }\n}",
    ] {
        assert_duplicate_impl_diagnostic(
            &format!("type R {{}}\n{implementation}\nfn main() {{}}"),
            "DuplicateDefinition",
            (4, 5),
            (3, 5),
        );
    }
    assert_duplicate_impl_diagnostic(
        "type R {}\n\
         impl R {\n    fn f(self) -> i64 { 1 }\n}\n\
         impl R {\n    fn f(self) -> i64 { 2 }\n}\n\
         fn main() {}",
        "DuplicateDefinition",
        (6, 5),
        (3, 5),
    );
}

#[test]
fn duplicate_impl_cross_module_error_names_previous_source_without_artifacts() {
    for action in ["check", "build"] {
        let dir = support::tempdir();
        for (name, source) in [
            ("common.hew", "pub type R {}"),
            (
                "left.hew",
                "import common;\nimpl Display for common.R { fn fmt(self) -> string { \"r\" } }",
            ),
            (
                "right.hew",
                "import common;\nimpl Display for common.R { fn fmt(self) -> string { \"r\" } }",
            ),
            ("main.hew", "import left; import right; fn main() {}"),
        ] {
            fs::write(dir.path().join(name), source).expect("module fixture");
        }
        let before = sorted_dir_entries(dir.path());
        let mut command = Command::new(hew_binary());
        command
            .arg(action)
            .arg(dir.path().join("main.hew"))
            .current_dir(dir.path());
        if action == "build" {
            command
                .args(["--emit-obj", "-o"])
                .arg(dir.path().join("result.o"));
        }
        let output =
            support::run_bounded_command(command, format!("cross-module duplicate impl {action}"));
        assert_eq!(
            output.status.code(),
            Some(1),
            "{}",
            describe_output(&output)
        );
        let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
        assert!(
            stderr.contains(&format!(
                "{}:2:1: error: conflicting implementation",
                dir.path().join("right.hew").display()
            )),
            "{stderr}"
        );
        assert!(
            stderr.contains(&format!(
                "{}:2:1: note: previous implementation here",
                dir.path().join("left.hew").display()
            )),
            "{stderr}"
        );
        assert!(
            !stderr.contains("E_HIR") && !stderr.contains("internal compiler error"),
            "{stderr}"
        );
        assert_eq!(sorted_dir_entries(dir.path()), before);
    }
}

const CODEGEN_FRONT_ACCEPTED_FIXTURE: &str = "fn main() {\n    println(1 + 2);\n}\n";

fn run_check_in_fixture_dir(dir: &std::path::Path) -> std::process::Output {
    Command::new(hew_binary())
        .args(["check", "main.hew"])
        .current_dir(dir)
        .output()
        .expect("hew binary must run")
}

#[test]
fn check_http_peer_fields_match_extern_signatures_through_both_import_routes() {
    // Import checking visits the peer's Response methods even when the entry
    // does not call HTTP. Both routes must agree on the handle's source type.
    for module in ["std.net.http", "std.net.http.http_client"] {
        let (_dir, path) = write_fixture(&format!("import {module}; fn main() {{}}"));
        let output = run_check(&["check", path.to_str().unwrap()]);
        assert!(
            output.status.success(),
            "HTTP module {module} must check its peer record fields against extern parameters\n{}",
            describe_output(&output),
        );
    }
}

#[test]
fn check_runs_codegen_front_on_success_without_artifacts() {
    let (dir, _path) = write_fixture(CODEGEN_FRONT_ACCEPTED_FIXTURE);

    let output = run_check_in_fixture_dir(dir.path());
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "accepted fixture should pass through frontend, HIR, MIR, and codegen-front gates\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains(": OK"),
        "successful check should print OK after codegen-front validation; got:\n{stderr}",
    );
    assert_only_source_artifact(dir.path());
}

#[test]
fn run_behavior_is_unchanged_for_codegen_front_accepted_fixture() {
    require_codegen();
    let (_dir, path) = write_fixture(CODEGEN_FRONT_ACCEPTED_FIXTURE);

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .output()
        .expect("hew run must run");

    assert!(
        output.status.success(),
        "hew run should still compile, link, and execute the accepted fixture\n{}",
        describe_output(&output),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn check_werror_still_promotes_frontend_warnings() {
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20return 1;\n\
         \x20\x20\x20\x202\n\
         }\n",
    );

    let without_werror = run_check(&["check", path.to_str().unwrap()]);
    assert!(
        without_werror.status.success(),
        "baseline warning fixture should pass without --Werror\n{}",
        describe_output(&without_werror),
    );

    let with_werror = run_check(&["check", "--Werror", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&with_werror.stderr));
    assert!(
        !with_werror.status.success(),
        "--Werror should promote frontend warnings\n{}",
        describe_output(&with_werror),
    );
    assert!(
        stderr.contains("warning: unreachable code"),
        "warning diagnostic should still render under --Werror; got:\n{stderr}",
    );
    assert!(
        stderr
            .lines()
            .any(|line| line.contains('|') && line.trim_end().ends_with('2')),
        "warning diagnostic should preserve the unreachable source line; got:\n{stderr}",
    );
    assert!(
        stderr.lines().any(|line| line
            .split_once('|')
            .is_some_and(|(_, marker)| marker.trim_start().starts_with('^'))),
        "warning diagnostic should preserve its source caret; got:\n{stderr}",
    );
}

#[test]
fn check_no_typecheck_skips_hir_mir_gates() {
    // The fixture fails a full `hew check` (a mutable-receiver requirement,
    // checker-level); under `--no-typecheck` that gate is skipped, so the
    // same source passes without reaching any deep-gate diagnostic.
    let (_dir, path) = write_fixture(
        "type VHolder {\n    items: Vec<i64>;\n    tag: string;\n}\n\nfn main() {\n    let init: Vec<i64> = Vec.new();\n    init.push(7);\n    let s = VHolder { items: init, tag: \"base\" };\n    let s2 = VHolder { ..s, items: s.items };\n    println(s2.items.len());\n}\n",
    );

    let output = run_check(&["check", "--no-typecheck", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "--no-typecheck should not attempt HIR/MIR gates that require checker output\n{}",
        describe_output(&output),
    );
    assert!(
        !stderr.contains("E_MIR") && !stderr.contains("E_HIR"),
        "--no-typecheck should skip deep gate diagnostics; got:\n{stderr}",
    );
    assert!(
        stderr.contains(": OK"),
        "--no-typecheck success should still print OK; got:\n{stderr}",
    );
}

#[test]
fn check_threads_pkg_path_and_project_dir_options() {
    let project = support::tempdir();
    let external = support::tempdir();
    let pkg = support::tempdir();

    fs::create_dir_all(pkg.path().join("foo")).expect("create package dir");
    fs::write(
        pkg.path().join("foo/foo.hew"),
        "pub fn answer() -> i64 { 42 }\n",
    )
    .expect("write package");
    let pkg_main = project.path().join("pkg_main.hew");
    fs::write(
        &pkg_main,
        "import foo;\nfn main() -> i64 { foo.answer() }\n",
    )
    .expect("write pkg main");

    let pkg_out = Command::new(hew_binary())
        .args([
            "check",
            "--pkg-path",
            pkg.path().to_str().unwrap(),
            pkg_main.to_str().unwrap(),
        ])
        .current_dir(std::env::temp_dir())
        .output()
        .expect("hew check --pkg-path must run");
    assert!(
        pkg_out.status.success(),
        "--pkg-path should be forwarded into check's frontend/deep pipeline\n{}",
        describe_output(&pkg_out),
    );

    fs::write(
        project.path().join("hew.toml"),
        "[package]\nname = \"app\"\n",
    )
    .expect("write manifest");
    fs::create_dir(project.path().join("src")).expect("create src");
    fs::write(
        project.path().join("src/helper.hew"),
        "pub fn answer() -> i64 { 42 }\n",
    )
    .expect("write helper");
    let project_main = external.path().join("project_main.hew");
    fs::write(
        &project_main,
        "import app.helper;\nfn main() -> i64 { helper.answer() }\n",
    )
    .expect("write project main");

    let project_out = run_check(&[
        "check",
        "--project-dir",
        project.path().to_str().unwrap(),
        project_main.to_str().unwrap(),
    ]);
    assert!(
        project_out.status.success(),
        "--project-dir should anchor manifest/local import resolution\n{}",
        describe_output(&project_out),
    );
}

#[test]
fn check_accepts_requested_target_for_deep_gates() {
    let (_dir, path) = write_fixture("fn main() -> i64 { return 0; }\n");

    let output = run_check(&[
        "check",
        "--target",
        "wasm32-unknown-unknown",
        path.to_str().unwrap(),
    ]);

    assert!(
        output.status.success(),
        "hew check should parse and thread a requested target into deep gates\n{}",
        describe_output(&output),
    );
}

// ── integer widening ─────────────────────────────────────────────────────────

/// Assigning an i32 variable where an i64 is expected must be a type error
/// with a diagnostic naming both types and suggesting `as i64`.
/// (Implicit same-sign widening was removed because it generates invalid LLVM IR.)
#[test]
fn check_rejects_implicit_integer_widening_i32_to_i64() {
    let (_dir, path) =
        write_fixture("fn main() -> i64 {\n    let x: i32 = 5;\n    let y: i64 = x;\n    y\n}\n");

    let output = run_check(&["check", path.to_str().unwrap()]);

    assert!(
        !output.status.success(),
        "hew check must reject implicit i32 → i64 widening\n{}",
        describe_output(&output),
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("cannot implicitly convert"),
        "diagnostic must mention 'cannot implicitly convert'; got:\n{stderr}",
    );
    assert!(
        stderr.contains("i32") && stderr.contains("i64"),
        "diagnostic must name both types; got:\n{stderr}",
    );
    assert!(
        stderr.contains("as i64"),
        "diagnostic must suggest `as i64`; got:\n{stderr}",
    );
}

/// After an explicit `as i64` cast, the widening is accepted.
#[test]
fn check_accepts_explicit_integer_widening_cast_i32_to_i64() {
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n    let x: i32 = 5;\n    let y: i64 = x as i64;\n    y\n}\n",
    );

    let output = run_check(&["check", path.to_str().unwrap()]);

    assert!(
        output.status.success(),
        "hew check must accept explicit `x as i64` cast\n{}",
        describe_output(&output),
    );
}

#[test]
fn check_rejects_invalid_module_const_arithmetic_without_nyi_or_artifact() {
    let (dir, path) = write_fixture("const BAD: u8 = 0 - 1;\nfn main() {}\n");
    let output = run_check(&["check", path.to_str().expect("utf8 fixture path")]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(!output.status.success(), "{}", describe_output(&output));
    assert!(
        stderr.contains("constant initializer arithmetic overflows declared type `u8`"),
        "expected target-range diagnostic, got:\n{stderr}"
    );
    assert!(
        !stderr.contains("E_NOT_YET_IMPLEMENTED"),
        "supported const arithmetic must not leak NYI:\n{stderr}"
    );
    assert!(
        !dir.path().join("main").exists(),
        "failed `hew check` must not emit a native artifact"
    );
}

// ── E_SIR_UNSUPPORTED source-span rendering (#3384) ─────────────────────────

/// A limitation the SIR ownership pass refuses (`E_SIR_UNSUPPORTED`) must
/// render with the same `file:line:col` and source excerpt as any other
/// diagnostic, not a bare internal-symbol line.
///
/// The fixture inserts into a map of resources held by a `#[resource]`. The
/// insert can release the map when it fails, so the resource could not be
/// rebuilt whole around it, and the map has no copy to work on instead.
#[test]
fn check_sir_unsupported_renders_with_source_span() {
    let (_dir, path) = write_fixture(
        "#[resource]\ntype Conn {\n    fd: i64;\n}\n\nimpl Conn {\n    fn close(consume self) {\n        println(f\"close {self.fd}\");\n    }\n}\n\n#[resource]\ntype Registry {\n    conns: HashMap<string, Conn>;\n}\n\nimpl Registry {\n    fn close(consume self) {\n        println(\"close registry\");\n    }\n}\n\nfn main() {\n    var registry = Registry { conns: HashMap.new() };\n    let conn = Conn { fd: 1 };\n    registry.conns.insert(\"a\", conn);\n}\n",
    );

    let output = run_check(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(!output.status.success(), "{}", describe_output(&output));
    assert!(
        stderr.contains("E_SIR_UNSUPPORTED"),
        "expected the live map-mutation limitation to report E_SIR_UNSUPPORTED; got:\n{stderr}"
    );
    assert!(
        stderr.contains("compiler limitation:"),
        "E_SIR_UNSUPPORTED must render on the Limitation channel; got:\n{stderr}"
    );
    // The header must name the fixture file and a real line:col, not a bare
    // internal line with no location.
    let file_name = path.file_name().unwrap().to_str().unwrap();
    assert!(
        stderr.lines().any(|line| {
            line.contains(file_name) && line.contains(':') && line.contains("E_SIR_UNSUPPORTED")
        }),
        "expected a `{file_name}:<line>:<col>: ... E_SIR_UNSUPPORTED` header; got:\n{stderr}"
    );
    // A source excerpt with a caret underline, the same shape every other
    // diagnostic renders — this is the fixture's own declaring line, not
    // a Rust `{:?}` payload.
    assert!(
        stderr.contains("fn main()"),
        "expected the declaring function's source line in the excerpt; got:\n{stderr}"
    );
    assert!(
        stderr.lines().any(|line| line
            .split_once('|')
            .is_some_and(|(_, marker)| marker.trim_start().starts_with('^'))),
        "expected a caret underline against the fixture source; got:\n{stderr}"
    );
}

/// Same limitation as [`check_sir_unsupported_renders_with_source_span`], but
/// raised inside a root-file actor's `receive fn` body instead of a free
/// `fn main`.
///
/// Actor handler bodies never populate HIR's `functions_by_item` table (they
/// live inside `HirActorDecl`, not the free-function item list), so a naive
/// re-derivation of source origin through that table always misses them.
/// `SemCallable::source_origin` is set once, correctly, at construction for
/// every callable shape including actor members; this pins that the span
/// path reads that stored fact rather than reconstructing it.
#[test]
fn check_sir_unsupported_renders_with_source_span_for_actor_handler() {
    let (_dir, path) = write_fixture(
        "#[resource]\ntype Conn {\n    fd: i64;\n}\n\nimpl Conn {\n    fn close(consume self) {\n        println(f\"close {self.fd}\");\n    }\n}\n\n#[resource]\ntype Registry {\n    conns: HashMap<string, Conn>;\n}\n\nimpl Registry {\n    fn close(consume self) {\n        println(\"close registry\");\n    }\n}\n\nactor Holder {\n    var registry: Registry;\n\n    receive fn poke() {\n        let conn = Conn { fd: 1 };\n        registry.conns.insert(\"a\", conn);\n    }\n}\n\nfn main() {\n    let h = spawn Holder { registry: Registry { conns: HashMap.new() } };\n    let _ = h.poke();\n}\n",
    );

    let output = run_check(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(!output.status.success(), "{}", describe_output(&output));
    assert!(
        stderr.contains("E_SIR_UNSUPPORTED"),
        "expected the actor handler's map-mutation limitation to report E_SIR_UNSUPPORTED; got:\n{stderr}"
    );
    let file_name = path.file_name().unwrap().to_str().unwrap();
    assert!(
        stderr.lines().any(|line| {
            line.contains(file_name) && line.contains(':') && line.contains("E_SIR_UNSUPPORTED")
        }),
        "expected a `{file_name}:<line>:<col>: ... E_SIR_UNSUPPORTED` header for the actor handler; got:\n{stderr}"
    );
    assert!(
        stderr.contains("let conn = Conn { fd: 1 };"),
        "expected the handler body's own source line in the excerpt, not a bare line; got:\n{stderr}"
    );
    assert!(
        stderr.lines().any(|line| line
            .split_once('|')
            .is_some_and(|(_, marker)| marker.trim_start().starts_with('^'))),
        "expected a caret underline against the fixture source; got:\n{stderr}"
    );
}

/// Same limitation as [`check_sir_unsupported_renders_with_source_span`],
/// through `--format json`: the JSON diagnostic must carry the fixture's
/// real file and a non-zero span, not the zero span a spanless limitation
/// falls back to.
#[test]
fn check_sir_unsupported_renders_with_source_span_json() {
    let (_dir, path) = write_fixture(
        "#[resource]\ntype Conn {\n    fd: i64;\n}\n\nimpl Conn {\n    fn close(consume self) {\n        println(f\"close {self.fd}\");\n    }\n}\n\n#[resource]\ntype Registry {\n    conns: HashMap<string, Conn>;\n}\n\nimpl Registry {\n    fn close(consume self) {\n        println(\"close registry\");\n    }\n}\n\nfn main() {\n    var registry = Registry { conns: HashMap.new() };\n    let conn = Conn { fd: 1 };\n    registry.conns.insert(\"a\", conn);\n}\n",
    );

    let output = Command::new(hew_binary())
        .args(["check", "--format", "json", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");
    assert!(!output.status.success(), "{}", describe_output(&output));

    let stdout = String::from_utf8_lossy(&output.stdout);
    let diagnostics: Vec<Value> = serde_json::from_str(&stdout).unwrap_or_else(|error| {
        panic!("stdout must be a parseable JSON array; parse error: {error}\nstdout:\n{stdout}")
    });
    let diagnostic = diagnostics
        .iter()
        .find(|d| d["code"] == "E_SIR_UNSUPPORTED")
        .unwrap_or_else(|| panic!("expected an E_SIR_UNSUPPORTED diagnostic; got:\n{stdout}"));

    assert_eq!(diagnostic["channel"], "limitation");
    assert_eq!(diagnostic["file"], path.to_str().unwrap());
    assert!(
        diagnostic["span"]["start_line"].as_u64().unwrap() > 0,
        "expected a real start_line, not the zero-span fallback; got:\n{stdout}"
    );
    assert!(
        diagnostic["span"]["start_col"].as_u64().unwrap() > 0,
        "expected a real start_col, not the zero-span fallback; got:\n{stdout}"
    );
}

/// Negative control for #3384: the or-pattern, fixed-size array parameter,
/// and functional-record-update-spread constructs the issue names as
/// formerly triggering `E_SIR_UNSUPPORTED` all compile cleanly today,
/// confirming the positive test above targets a genuinely live limitation
/// rather than a stale trigger.
#[test]
fn check_former_sir_unsupported_triggers_now_compile() {
    let (_dir, path) = write_fixture(
        "fn classify(x: i64) -> string {\n    match x {\n        1 | 2 | 3 => \"small\",\n        _ => \"large\",\n    }\n}\n\nfn sum_first_three(values: [i64; 3]) -> i64 {\n    values[0] + values[1] + values[2]\n}\n\ntype Point {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    println(classify(2));\n    println(sum_first_three([1, 2, 3]));\n    let p = Point { x: 1, y: 2 };\n    let q = Point { ..p, x: 3 };\n    println(q.x + q.y);\n}\n",
    );

    let output = run_check(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "or-pattern, fixed-size array parameter and record-update spread must all check cleanly now\n{}",
        describe_output(&output),
    );
    assert!(
        !stderr.contains("E_SIR_UNSUPPORTED"),
        "none of these constructs should trigger the SIR consume-wall limitation; got:\n{stderr}"
    );
}
