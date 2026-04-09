mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen, strip_ansi};

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
