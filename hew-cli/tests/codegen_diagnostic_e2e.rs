use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn require_codegen() -> bool {
    static BUILD_OK: OnceLock<bool> = OnceLock::new();
    *BUILD_OK.get_or_init(|| {
        Command::new("make")
            .args(["runtime", "stdlib"])
            .current_dir(repo_root())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

#[test]
fn extern_type_codegen_errors_report_the_type_span() {
    if !require_codegen() {
        return;
    }

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

    let stderr = String::from_utf8_lossy(&output.stderr);
    let location = format!("loc(\"{}\":2:21): error:", fixture.display());

    assert!(
        stderr.contains(&format!(
            "{location} unsupported type expression in MLIR codegen"
        )),
        "{stderr}",
    );
    assert!(
        stderr.contains(&format!(
            "{location} cannot resolve parameter type in extern function"
        )),
        "{stderr}",
    );
}
