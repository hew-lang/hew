mod support;

use std::fs;
use std::process::Command;

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

const CODEGEN_FRONT_ACCEPTED_FIXTURE: &str = "fn main() {\n    println(1 + 2);\n}\n";

fn run_check_in_fixture_dir(dir: &std::path::Path) -> std::process::Output {
    Command::new(hew_binary())
        .args(["check", "main.hew"])
        .current_dir(dir)
        .output()
        .expect("hew binary must run")
}

#[test]
fn check_fails_on_hir_gate_before_ok() {
    // `Some(0) | None` in an or-pattern is a HIR-rejected shape (constructor
    // with literal payload in an or-branch is not yet classified). This is a
    // stable HIR gate trigger; the old `y => y` fixture now lowers cleanly.
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20let x: Option<i64> = Some(0);\n\
         \x20\x20\x20\x20match x { Some(0) | None => 0, Some(n) => n }\n\
         }\n",
    );

    let output = run_check(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "hew check must fail when a later HIR gate rejects the file\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("E_NOT_YET_IMPLEMENTED") || stderr.contains("E_HIR"),
        "HIR gate diagnostic should be rendered; got:\n{stderr}",
    );
    assert!(
        !stderr.contains(": OK"),
        "hew check must not print OK after a HIR gate failure; got:\n{stderr}",
    );
}

#[test]
fn check_fails_on_mir_gate_before_ok() {
    // A functional-update override that aliases the consumed base
    // (`VHolder { items: s.items, ..s }`) is a fail-closed MIR gate: the base's
    // overridden owned field is released at the construction site, so the new
    // record would alias freed memory. This is an intentional, durable MIR
    // `NotYetImplemented` (the COW value model that would keep the base live is
    // not built), so it is a stable trigger for "hew check fails at the MIR
    // gate with a clean, source-attributed diagnostic".
    let (_dir, path) = write_fixture(
        "record VHolder { items: Vec<i64>, tag: string }\n\
         fn main() {\n\
         \x20\x20\x20\x20let init: Vec<i64> = Vec::new();\n\
         \x20\x20\x20\x20init.push(7);\n\
         \x20\x20\x20\x20let s = VHolder { items: init, tag: \"base\" };\n\
         \x20\x20\x20\x20let s2 = VHolder { items: s.items, ..s };\n\
         \x20\x20\x20\x20println(s2.items.len());\n\
         }\n",
    );

    let output = run_check(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "hew check must fail when a later MIR gate rejects the file\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("E_MIR") || stderr.contains("E_NOT_YET_IMPLEMENTED"),
        "MIR gate diagnostic should be rendered; got:\n{stderr}",
    );
    assert!(
        stderr.contains("main.hew:6:"),
        "MIR gate diagnostic should be source-attributed; got:\n{stderr}",
    );
    assert!(
        stderr.contains(
            "MIR lowering for functional-update override aliasing the consumed base \
             is not implemented yet"
        ),
        "MIR diagnostic should use a user-readable message; got:\n{stderr}",
    );
    assert!(
        !stderr.contains("MirDiagnostic")
            && !stderr.contains("NotYetImplemented {")
            && !stderr.contains("SiteId("),
        "hew check must not emit raw MIR debug payloads; got:\n{stderr}",
    );
    assert!(
        !stderr.contains(": OK"),
        "hew check must not print OK after a MIR gate failure; got:\n{stderr}",
    );
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
fn check_fails_on_codegen_front_gate_before_ok_without_artifacts() {
    let (dir, _path) = write_fixture(
        "fn unsupported(x: [i64; 2]) -> [i64; 2] {\n    return x;\n}\nfn main() {\n}\n",
    );

    let output = run_check_in_fixture_dir(dir.path());
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "hew check must fail when codegen-front validation rejects the MIR\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("E_CODEGEN_FRONT"),
        "codegen-front failure should use a stable diagnostic family; got:\n{stderr}",
    );
    assert!(
        stderr.contains("unsupported construct"),
        "codegen-front failure should render CodegenError Display, not Debug; got:\n{stderr}",
    );
    assert!(
        !stderr.contains("CodegenError")
            && !stderr.contains("Unsupported(")
            && !stderr.contains(": OK"),
        "hew check must not emit raw CodegenError debug payloads or OK after failure; got:\n{stderr}",
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
        stderr.contains("warnings treated as errors"),
        "--Werror failure summary should be preserved; got:\n{stderr}",
    );
}

#[test]
fn check_no_typecheck_skips_hir_mir_gates() {
    // The self-aliasing functional-update is a MIR gate failure under a full
    // check (see `check_fails_on_mir_gate_before_ok`); under `--no-typecheck`
    // the gate is skipped, so the same fixture passes.
    let (_dir, path) = write_fixture(
        "record VHolder { items: Vec<i64>, tag: string }\n\
         fn main() {\n\
         \x20\x20\x20\x20let init: Vec<i64> = Vec::new();\n\
         \x20\x20\x20\x20init.push(7);\n\
         \x20\x20\x20\x20let s = VHolder { items: init, tag: \"base\" };\n\
         \x20\x20\x20\x20let s2 = VHolder { items: s.items, ..s };\n\
         \x20\x20\x20\x20println(s2.items.len());\n\
         }\n",
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
fn show_stack_hints_render_before_later_hir_failure() {
    // The closure `|x| x+1` triggers HEW-PERF-001 (ClosureEnv stack hint).
    // The `Some(0) | None` or-pattern triggers E_NOT_YET_IMPLEMENTED at HIR.
    // The test asserts the stack hint is rendered BEFORE the gate diagnostic.
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20let f = |x: i64| -> i64 { x + 1 };\n\
         \x20\x20\x20\x20let _r = f(1);\n\
         \x20\x20\x20\x20let x: Option<i64> = Some(0);\n\
         \x20\x20\x20\x20match x { Some(0) | None => 0, Some(n) => n }\n\
         }\n",
    );

    let output = run_check(&["check", "--show-stack-hints", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "fixture should fail at HIR after stack hints render\n{}",
        describe_output(&output),
    );
    let hint_pos = stderr
        .find("HEW-PERF-001")
        .expect("expected stack hint before HIR failure");
    let gate_pos = stderr
        .find("E_NOT_YET_IMPLEMENTED")
        .or_else(|| stderr.find("E_HIR"))
        .expect("expected HIR gate diagnostic");
    assert!(
        hint_pos < gate_pos,
        "stack hints should render before later HIR/MIR gate diagnostics; got:\n{stderr}",
    );
    assert!(
        !stderr.contains(": OK"),
        "hew check must not print OK after a later gate failure; got:\n{stderr}",
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
        "import app::helper;\nfn main() -> i64 { helper.answer() }\n",
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
