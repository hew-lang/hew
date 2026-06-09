mod support;

use std::fs;
use std::process::Command;

use support::{describe_output, hew_binary, strip_ansi};

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

#[test]
fn check_fails_on_hir_gate_before_ok() {
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20let x = 1;\n\
         \x20\x20\x20\x20return match x {\n\
         \x20\x20\x20\x20\x20\x20\x20\x201 => 10,\n\
         \x20\x20\x20\x20\x20\x20\x20\x20_ => 0,\n\
         \x20\x20\x20\x20};\n\
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
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20let _r = re\"hello\";\n\
         \x20\x20\x20\x20return 0;\n\
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
        stderr.contains("main.hew:2:"),
        "MIR gate diagnostic should be source-attributed; got:\n{stderr}",
    );
    assert!(
        stderr.contains("MIR lowering for HirExprKind::RegexLiteralRef is not implemented yet"),
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
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20let _r = re\"hello\";\n\
         \x20\x20\x20\x20return 0;\n\
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
    let (_dir, path) = write_fixture(
        "fn main() -> i64 {\n\
         \x20\x20\x20\x20let f = |x: i64| -> i64 { x + 1 };\n\
         \x20\x20\x20\x20let _r = f(1);\n\
         \x20\x20\x20\x20let x = 1;\n\
         \x20\x20\x20\x20return match x {\n\
         \x20\x20\x20\x20\x20\x20\x20\x201 => 10,\n\
         \x20\x20\x20\x20\x20\x20\x20\x20_ => 0,\n\
         \x20\x20\x20\x20};\n\
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
