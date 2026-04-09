mod support;

use std::fs;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen, strip_ansi};

fn describe_output(output: &std::process::Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    )
}

#[test]
fn extern_type_codegen_errors_report_the_type_span() {
    require_codegen();

    let fixture = repo_root()
        .join("hew-codegen/tests/examples/e2e_negative/extern_infer_param.hew")
        .canonicalize()
        .unwrap();

    let output = Command::new(hew_binary())
        .args(["build", "--emit-mlir", "-g", fixture.to_str().unwrap()])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let location = format!("{}:2:21: error:", fixture.display());
    assert!(
        stderr.contains(&format!(
            "{location} cannot infer type for signature of extern function `bad_param`"
        )),
        "{stderr}",
    );
    assert!(
        stderr.contains("2 |     fn bad_param(x: _);")
            && stderr.contains("|                     ^"),
        "{stderr}",
    );
}

#[test]
fn module_qualified_generic_call_builds_with_inferred_type_args() {
    require_codegen();

    let workspace = tempfile::Builder::new()
        .prefix("module-qualified-generic-call-")
        .tempdir_in(repo_root())
        .expect("create module-qualified generic call workspace");
    fs::write(
        workspace.path().join("hew.toml"),
        "[package]\nname = \"myapp\"\n",
    )
    .expect("write hew.toml");
    fs::create_dir(workspace.path().join("src")).expect("create src directory");
    fs::write(
        workspace.path().join("main.hew"),
        "import myapp::widgets;\n\nfn main() -> i64 {\n    widgets.id(1)\n}\n",
    )
    .expect("write main.hew");
    fs::write(
        workspace.path().join("src/widgets.hew"),
        "pub fn id<T>(value: T) -> T {\n    value\n}\n",
    )
    .expect("write widgets.hew");

    let main_path = workspace.path().join("main.hew");
    let source_arg = main_path.to_str().expect("main path should be valid UTF-8");

    let check_output = Command::new(hew_binary())
        .args(["check", source_arg])
        .current_dir(workspace.path())
        .output()
        .expect("run hew check");
    assert!(
        check_output.status.success(),
        "hew check failed\n{}",
        describe_output(&check_output),
    );

    let build_output = Command::new(hew_binary())
        .args(["build", "--emit-mlir", "-g", source_arg])
        .current_dir(workspace.path())
        .output()
        .expect("run hew build --emit-mlir");
    assert!(
        build_output.status.success(),
        "hew build --emit-mlir failed\n{}",
        describe_output(&build_output),
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&build_output.stderr));
    assert!(
        !stderr.contains("undefined function 'id'"),
        "generic module call should specialize instead of failing with undefined function\n{stderr}"
    );
}
